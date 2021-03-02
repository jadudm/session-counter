#lang racket

(require "constants.rkt")

(require sql db gregor)
(provide the-db
         initialize-db
         log-mac
         mfg-exists?
         get-mfg
         consolidate)

(struct report (vendor-bytes vendor-oui count))

;; We'll use an in-memory SQLite table.
;; Nothing is ever written to disk for privacy.

;; A parameter to hold the DB connection.
;; This is a box, essentially. 
(define the-db (make-parameter false))

;; initialize-db : none -> void
;; Creates a new, empty database in memory.
;; Initializes a table for temporary data storage.
(define (initialize-db)
  (cond
    [(connection? (the-db))
     (query-exec (the-db) "DELETE FROM wifi_nearby")
     (query-exec (the-db) "VACUUM")]
    [else (the-db (sqlite3-connect #:database 'memory))
          (query-exec (the-db)
                      (create-table wifi_nearby
                                    #:columns
                                    [mac text]
                                    [timestamp date]))]))

(define (log-mac mac #:timestamp [timestamp (now)])
  (unless (connection? (the-db))
    (initialize-db))
  (query-exec (the-db)
              (insert #:into wifi_nearby
                      #:set
                      [mac ,mac]
                      [timestamp ,(datetime->iso8601 timestamp)])))

  

(define (consolidate #:timestamp [timestamp (current-seconds)])
  (define consolidated (make-hash))
  
  (define mac-addrs
    (query-list (the-db)
                (select #:distinct mac
                        #:from wifi_nearby)))

  ;; For each MAC address, lets find out how many times it appeared.
  (for ([mac mac-addrs])
    (define count
      (query-value (the-db)
                   (select (count-all)
                           #:from wifi_nearby
                           #:where (= mac ,mac))))
    (when (<= APPEARANCE-THRESHOLD count)
      (hash-set! consolidated mac count)))

  consolidated
  )


(define mfg-exists?
  (let ([memo (make-hash)]
        [conn (make-parameter false)])
    (λ (mac #:mfg mfg)
      (when (not (conn))
        (conn (sqlite3-connect #:database mfg)))
      (cond
        [(hash-has-key? memo mac)
         true]
        [else
         (define row
           (with-handlers ([exn? (λ (e) false)])
             (query-row conn (select #:from oui
                                     #:where (= mac ,mac)))))
         (cond
           [row
            (hash-set! memo mac true)
            true]
           [else
            (hash-set! memo mac false)
            false])
         ]))))
           

(define get-mfg
  (let ([memo (make-hash)]
        [conn (make-parameter false)])
    (λ (mac #:length [length 'short] #:mfg mfg)
      (when (not (conn))
        (conn (sqlite3-connect #:database mfg)))
      (cond
        [(hash-has-key? memo mac)
         (values (first (hash-ref memo mac))
                 (second (hash-ref memo mac))
                 length)]
        [else
         (define row
           (with-handlers ([exn? (λ (e) false)])
             (query-row conn (select #:from oui
                                     #:where (= mac ,mac)))))
         (cond
           [row
            (hash-set! memo mac (list (vector-ref row 1) (vector-ref row 2)))
            (values (vector-ref row 1) (vector-ref row 2) length)]
           [else
            (hash-set! memo mac (list "uknown" "unknown"))
            (values "unknown" "unknown" length)])
         ]))))
           
      