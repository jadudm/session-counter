#lang racket
;; sudo ip link set wlan0 down
;; sudo iw wlan0 set monitor control
;; sudo ip link set wlan0 up
;; sudo lshw -class network

;; MAC addresses are 17 characters long.
(define MAC-ADDRESS-LENGTH 17)

;;  tshark -a duration:16 -I -i wlan1 -Tfields -e wlan.sa 2>/dev/null | sort -u
(define (count-packets #:adapter [adapter "wlan1"]
                       #:seconds [seconds 15])
  (define addresses (make-hash))
  (define-values (pid inp outp errp)
    (with-handlers ([exn? (λ (err)
                            (fprintf (current-error-port)
                                     "Something went wrong with packet counting.~n")
                            (close-input-port inp)
                            (close-output-port outp)
                            (close-output-port errp))])
      (subprocess false false false
                  "/usr/bin/tshark"
                  "-a" (format "duration:~a" seconds)
                  "-I"
                  "-i" adapter
                  "-Tfields" "-e" "wlan.sa")))
  (define read-thread-id
    (thread (λ ()
              (let loop ([line (read-line inp)])
                (unless (eof-object? line)
                  (when (= MAC-ADDRESS-LENGTH (string-length line))
                    (hash-set! addresses line
                               (add1 (hash-ref addresses line 0))))
                  (loop (read-line inp))))
              
              ;; Before we quit, we need to close the subprocess ports.
              (close-input-port inp)
              (close-output-port outp)
              (close-input-port errp)
              )))
  ;; Monitor the thread. Kill it after seconds + 1
  (define watcher-id
    (thread (λ ()
              (sleep (add1 seconds))
              (fprintf (current-error-port)
                       "Packet counting timed out.~n")
              ;; If we get here, that means the process ran away, or
              ;; otherwise bad things happened.
              (subprocess-kill pid true)
              (close-input-port inp)
              (close-output-port outp)
              (close-input-port errp)
              (when (thread? read-thread-id)
                (kill-thread read-thread-id)))))
  ;; Run the monitoring process.
  (thread-wait read-thread-id)
  ;; If it terminates normally, stop the watcher.
  (kill-thread watcher-id)
  ;; Return the found addresses. Returns an empty hash
  ;; if things went wrong. (This adheres to contract.)
  addresses)
  

(define mem-use '())
(let loop ([count 0])
  (unless (> count 100)
    (define pkts (count-packets #:adapter "wlx9cefd5fc98b7" #:seconds 3))
    (define cmu (current-memory-use))
    (printf "~a: ~a ~a~n" count cmu (hash-count pkts))
    (set mem-use (cons cmu mem-use))
    (collect-garbage)
    (loop (add1 count))))


