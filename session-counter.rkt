#lang racket

(require sql db gregor)
(require
  "lib/constants.rkt"
  "lib/tshark.rkt"
  "lib/duration-db.rkt"
  "lib/api.rkt"
  "lib/logging.rkt")

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

(define (proc:every-nth-tick n-ticks tick-ch? nth-tick!)
  (define counter 0)
  (let loop ()
    (define tick (channel-get tick-ch?))
    (set! counter (modulo (add1 counter) n-ticks))
    (when (zero? counter)
      (log-wifi-info "every ~a ticks!" n-ticks)
      (channel-put nth-tick! tick))
    (loop)
    ))

(define (proc:tshark go? #:seconds seconds #:adapter adapter)
  (let loop ()
    (channel-get go?)
    (log-wifi-info "running tshark for ~a" seconds)
    (define pkts (run-tshark #:seconds seconds #:adapter adapter))
    (log-wifi-info "found ~a MAC addresses" (hash-count pkts))
    (for ([(k v) pkts])
      (log-mac k #:timestamp (now)))
    (loop)))

(define (proc:consolidate go? results!)
  (let loop ()
    (channel-get go?)
    (log-wifi-info  "consolidating")
    (define consolidated-results (consolidate))
    ;; Flush the DB after consolidating.
    (initialize-db)
    (channel-put results! consolidated-results)
    (collect-garbage)
    (define ram-use (current-memory-use))
    (log-wifi-info "current-memory-use ~a" ram-use)
    (define token (get-token #:username USERNAME #:password PASSWORD))
    (when token
      (report-ram ram-use #:token token))
 
    (loop)
    ))

(define (proc:report-results results? #:oui-db [oui-db false])
  (let loop ()
    (define consolidated (channel-get results?))
    (when (> (hash-count consolidated) 0)
      (log-wifi-info "reporting ~a addresses" (hash-count consolidated))
      (define token (get-token #:username USERNAME #:password PASSWORD))
      ;; FIXME
      ;; There's no reason to hammer the API endpoint.
      ;; We should have a way to bulk insert?
      (when token
        (for ([(mac count) consolidated])
          (define-values (rept-mac short-mfg long-mfg)
            (cond
              [(mfg-exists?  (substring mac 0 14) #:mfg oui-db)
               (get-mfg (substring mac 0 14) #:mfg oui-db)]
              [(mfg-exists? (substring mac 0 11) #:mfg oui-db)
               (get-mfg (substring mac 0 11) #:mfg oui-db)]
              [(mfg-exists? (substring mac 0 8) #:mfg oui-db)
               (get-mfg (substring mac 0 8) #:mfg oui-db)]
              [else
               (values (substring mac 0 8) "unknown" "unknown")]))
          (log-wifi-info "reporting ~a ~a ~a"  rept-mac short-mfg count)
          (insert-wifi-device COLLECTION
                              rept-mac
                              count
                              #:mfg-short short-mfg
                              #:mfg-long long-mfg
                              #:token token)
          (sleep 0.1))))
    (loop)))

(define (proc:delta in? out1! out2!)
  (let loop ()
    (define v (channel-get in?))
    ;; This is a SEQ delta
    (channel-put out1! v)
    (channel-put out2! v)
    (loop)
    ))

(provide main)
(define (main #:oui-db [oui-db false])
  
  (initialize-db)
  
  (define tick (make-channel))
  (define t1 (make-channel))
  (define t2 (make-channel))
  (define tick-rept (make-channel))
  (define results (make-channel))
  
  ;;(define logger-id (thread (thunk (proc:simple-logger logger-ch))))
  (define logger-id (thread (thunk (proc:logger))))
  
  (thread (thunk (proc:minute-tick tick)))
  (thread (thunk (proc:delta tick t1 t2)))
  
  (thread (thunk (proc:tshark t1 #:seconds OBSERVE-SECONDS #:adapter "wlan1")))
  (thread (thunk (proc:every-nth-tick REPORT-MINUTES t2 tick-rept)))
  (thread (thunk (proc:consolidate tick-rept results)))
  (thread (thunk (proc:report-results results #:oui-db oui-db)))

  ;; Waits forever. Otherwise, Racket exits.
  (log-wifi-info "starting up")
  (thread-wait logger-id)
  )
