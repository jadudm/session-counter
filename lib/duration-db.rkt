#lang racket

(require "constants.rkt")

(require sql db (prefix-in d: deta) gregor threading)
(provide the-db
         initialize-db
         log-mac
         mfg-exists?
         get-mfg
         consolidate)

(struct report (vendor-bytes vendor-oui count))

;; deta modifies the table name
;; TMA -> TMAs
(d:define-schema TMA
                 ([id d:id/f #:primary-key #:auto-increment]
                  [mac d:string/f #:contract non-empty-string?]
                  [timestamp d:datetime-tz/f]
                  ))

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
     (query-exec (the-db) (d:delete (d:from TMA #:as tma)))
     (query-exec (the-db) "VACUUM")
     (disconnect (the-db))
     (the-db (sqlite3-connect #:database 'memory))
     (d:create-table! (the-db) 'TMA)]
    [else (the-db (sqlite3-connect #:database 'memory))
          (d:create-table! (the-db) 'TMA)]
    ))

(define (log-mac mac #:timestamp [timestamp (now)])
  (unless (connection? (the-db))
    (initialize-db))
  (d:insert-one! (the-db)
                 (make-TMA
                  #:mac mac
                  #:timestamp (now/moment/utc))))
  

(define (consolidate #:timestamp [timestamp (current-seconds)])
  (define consolidated (make-hash))

  ;; FIXME deta does not have DISTINCT.
  (define sq (select #:distinct mac #:from TMAs))
  (define mac-addrs
    (query-list (the-db) sq))
    
    ;; For each MAC address, lets find out how many times it appeared.
    (for ([mac mac-addrs])
      (define q (~> (d:from TMA #:as tma)
                    (d:select (count tma.mac))
                    (d:where (= tma.mac ,mac))))
      (define count (query-value (the-db) q))
      (when (<= APPEARANCE-THRESHOLD count)
        (hash-set! consolidated mac count)))

    consolidated
    )

  (define mfg-conn (make-parameter false))

  (define (mfg-exists? mac #:mfg mfg)
    (let ([memo (make-hash)])
      (when (not (mfg-conn))
        (mfg-conn (sqlite3-connect #:database mfg)))
      (cond
        [(hash-has-key? memo mac)
         true]
        [else
         (define rows
           (with-handlers ([exn? (λ (e) false)])
             ;; FIXME The OUI DB is uppercase, but tshark reports lowercase.
             (query-rows (mfg-conn)
                         (select id
                                 #:from oui
                                 #:where (like mac ,(format "~a%"
                                                            (string-upcase mac)))))))
         (cond
           [(not (empty? rows))
            (hash-set! memo mac true)
            true]
           [else
            (hash-set! memo mac false)
            false])
         ])))

  (define get-mfg
    (let ([memo (make-hash)])
      (λ (mac #:mfg mfg)
        (when (not (mfg-conn))
          (mfg-conn (sqlite3-connect #:database mfg)))
        (cond
          [(hash-has-key? memo mac)
           (values mac
                   (first (hash-ref memo mac))
                   (second (hash-ref memo mac))
                   )]
          [else
           (define rows
             (with-handlers ([exn? (λ (e)
                                     ;;(fprintf (current-error-port) "e: ~a~n" e)
                                     false)])
               (query-rows (mfg-conn)
                           (select id manufacturer
                                   #:from oui
                                   #:where (like mac ,(format "~a%"
                                                              (string-upcase mac)))))
               ))
           (cond
             [(not (empty? rows))
              (hash-set! memo mac (list (vector-ref (first rows) 0)
                                        (vector-ref (first rows) 1)))
              (values mac
                      (vector-ref (first rows) 0)
                      (vector-ref (first rows) 1))]
             [else
              (hash-set! memo mac (list "unknown" "unknown"))
              (values mac
                      "unknown"
                      "unknown")])
           ]))))
           
  