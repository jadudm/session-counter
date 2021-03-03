#lang racket

(provide get-token insert-wifi-device report-ram)

(require "constants.rkt"
         "logging.rkt")
(require  net/http-easy json gregor)

(define (make-uri path)
  (format "~a://~a~a" SCHEME HOST path))

;; FIXME
;; Need error logging
(define (backoff-retry id fun #:count [count 0])
  (define (timeout-handler e)
    (cond
      [(< count HTTP-RETRY-THRESHOLD)
       (define sleep-duration (* 3 count))
       (log-wifi-warning "~a: backing off for ~a seconds before retry" id sleep-duration)
       (sleep sleep-duration)
       (backoff-retry id fun #:count (add1 count))
       ]
      [else
       (log-wifi-fatal "~a: reached retry threshold. restart." id)]))
  (with-handlers ([exn:fail:http-easy:timeout? timeout-handler])
    (fun))
  )
                  
(define (get-token #:username user #:password pass)
  (backoff-retry
   "get-token"
   (thunk
    (define resp
      (post (make-uri "/auth/login")
            #:json (hasheq 'email USERNAME
                           'password PASSWORD)))
    (define json (response-json resp))
    (cond
      [(and (hash? json) (hash-has-key? json 'data))
       (hash-ref (hash-ref json 'data) 'access_token)]
      [else
       false]))))
   

(define (insert-wifi-device collection mac count
                            #:mfg-short [mfg-short "unknown"]
                            #:mfg-long [mfg-long "unknown"]
                            #:token token)
  (backoff-retry
   "insert-wifi-device"
   (thunk
    (define resp
      (post (make-uri (format "/items/~a" collection))
            #:headers (hasheq 'Authorization
                              (format "Bearer ~a" token))
            #:json (hasheq 'libid "ME-LEW"
                           'local_date_created (datetime->iso8601 (now))
                           'mac mac
                           'mfgs mfg-short
                           'mfgl mfg-long
                           'count count)))
    (define json (response-json resp))
    json)))

(define (report-ram bytes #:token token)
  (backoff-retry
   "report-ram"
   (thunk
    (define resp
      (post (make-uri "/items/memory_usage")
            #:headers (hasheq 'Authorization
                              (format "Bearer ~a" token))
            #:json (hasheq 'bytes bytes)))
    (define json (response-json resp))
    json)))
