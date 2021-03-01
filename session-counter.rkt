#lang racket

(require sql db gregor)
(require
  "lib/constants.rkt"
  "lib/tshark.rkt"
  "lib/duration-db.rkt"
  "lib/api.rkt")

(define logger-ch (make-channel))
(define (proc:logger msg?)
  (let loop ()
    (define msg (channel-get msg?))
    (printf "~a: ~a~n"
            (datetime->iso8601 (now))
            msg)
    (loop)))

(define (proc:minute-tick ch!)
  (define prev-minute (date-minute (seconds->date (current-seconds))))
  (let loop ()
    (sleep 3)
    (define current-minute (date-minute (seconds->date (current-seconds))))
    (when (not (equal? prev-minute current-minute))
      (channel-put ch! 'tick)
      (set! prev-minute current-minute))
    (loop)
    ))

(define (proc:every-nth-tick n-ticks tick-ch? nth-tick! passthrough!)
  (define counter 0)
  (let loop ()
    (define tick (channel-get tick-ch?))
    (set! counter (modulo (add1 counter) n-ticks))
    (when (zero? counter)
      (channel-put logger-ch
                   (format "every ~a ticks!"
                           n-ticks))
      (channel-put nth-tick! tick))
    (channel-put passthrough! tick)
    (loop)
    ))

(define (proc:tshark go? #:seconds seconds #:adapter adapter)
  (let loop ()
    (channel-get go?)
    (channel-put logger-ch
                 (format "running for ~a" seconds))
    (define pkts (run-tshark #:seconds seconds #:adapter adapter))
    (channel-put logger-ch
                 (format "found ~a MAC addresses" (hash-count pkts)))
    (for ([(k v) pkts])
        (log-mac k #:timestamp (now)))
    (loop)))

(define (proc:consolidate go? results!)
  (let loop ()
    (channel-get go?)
    (channel-put logger-ch
                 (format "consolidating."))
    (define consolidated-results (consolidate))
    ;; Flush the DB after consolidating.
    (initialize-db)
    (channel-put results! consolidated-results)
    (collect-garbage)
    (channel-put logger-ch
                 (format "memory ~a~n"
                         (current-memory-use)))
    (loop)
    ))

(define (proc:report-results results?)
  (let loop ()
    (define consolidated (channel-get results?))
    (channel-put logger-ch
                 (format "reporting ~a addresses" (hash-count consolidated)))
    (define token (get-token #:username USERNAME #:password PASSWORD))
    ;; FIXME
    ;; There's no reason to hammer the API endpoint.
    ;; We should have a way to bulk insert?
    (for ([(mac count) consolidated])
      (define sub-mac (substring mac 0 8))
      (channel-put logger-ch
                   (format "reporting ~a ~a" sub-mac count))
      (insert-into-collection COLLECTION
                              sub-mac
                              count
                              #:token token)
      (sleep 0.1))
    (loop)))
                              

(define (main)
  (initialize-db)
  
  (define tick-cap1 (make-channel))
  (define tick-cap2 (make-channel))
  (define tick-rept (make-channel))
  (define results (make-channel))
  
  (thread (thunk (proc:logger logger-ch)))
  (thread (thunk (proc:minute-tick tick-cap1)))
  (thread (thunk (proc:every-nth-tick REPORT-MINUTES tick-cap1 tick-rept tick-cap2)))
  (thread (thunk (proc:tshark tick-cap2 #:seconds OBSERVE-SECONDS #:adapter "wlan1")))
  (thread (thunk (proc:consolidate tick-rept results)))
  (thread (thunk (proc:report-results results)))
  )
