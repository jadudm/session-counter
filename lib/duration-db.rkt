#lang racket

(require "constants.rkt")

(require sql db gregor)
(provide initialize-db
         log-mac
         mfg-exists?
         get-mfg
         consolidate)

;; We'll use an in-memory SQLite table.
;; Nothing is ever written to disk for privacy.

;; A parameter to hold the DB connection.
;; This is a box, essentially. 
(define conn false)

;; initialize-db : none -> void
;; Creates a new, empty database in memory.
;; Initializes a table for temporary data storage.
(define (initialize-db)
  (cond
    [(connection? conn)
     (query-exec conn (delete #:from TMA))
     (query-exec conn "VACUUM")
     (printf "VACUUM~n")
     ]
    [else
     ;; Makikng this virtual means it will be constantly
     ;; recreated... therefore useless for temporary storage.
     (set! conn (sqlite3-connect #:database 'memory
                                 #:mode 'create))
     (query-exec
      conn
      (create-table TMA
                    #:columns
                    [mac text]
                    [timestamp datetime]))
     ]))

(define (show-tables)
  (printf "~a~n"
          (query-rows 
           conn
           (select name
                   #:from sqlite_master
                   #:where (= type "table")))))

(define (log-mac mac #:timestamp [timestamp (now)])
  (when (not (connection? conn))
    (printf "WHAT IS THIS: ~a~n" conn)
    (initialize-db))
  (query-exec
   conn
   (insert #:into TMA
           #:set
           [mac ,mac]
           [timestamp ,(moment->iso8601 (now/moment/utc))]
           ))
  )

(define (consolidate #:timestamp [timestamp (current-seconds)])
  (define consolidated (make-hash))

  ;; FIXME deta does not have DISTINCT.
  (define sq (select #:distinct mac #:from TMA))
  (define mac-addrs
    (query-list conn sq))
    
  ;; For each MAC address, lets find out how many times it appeared.
  (for ([mac mac-addrs])
    (define q (select (count-all)
                      #:from TMA
                      #:where (= mac ,mac)))
    (define count (query-value conn q))
    (when (<= APPEARANCE-THRESHOLD count)
      (hash-set! consolidated mac count)))

  consolidated
  )

(define mfg-conn false)
(define (check-mfg-db-conn mfg)
  (when (not (connection? mfg-conn))
      (set! mfg-conn
            (virtual-connection
             (connection-pool
              (thunk (sqlite3-connect #:database mfg)))))))

(define (mfg-exists? mac #:mfg mfg)
  (let ([memo (make-hash)])
    (check-mfg-db-conn mfg)
    (cond
      [(hash-has-key? memo mac)
       true]
      [else
       (define rows
         (with-handlers ([exn? (λ (e) false)])
           ;; FIXME The OUI DB is uppercase, but tshark reports lowercase.
           (query-rows mfg-conn
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
      (check-mfg-db-conn mfg)
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
             (query-rows mfg-conn
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
           
