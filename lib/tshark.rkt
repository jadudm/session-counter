#lang racket
(require "constants.rkt")

(provide
 (contract-out
  ;; No required arguments, optional keyword args, return contract
  [run-tshark (->* ()
                   (
                    #:tshark-path string?
                    #:seconds integer?
                    #:adapter string?
                    #:timeout integer?
                    )
                  hash?)]
  ))
      

#|
export WLAN=wlan1
sudo ip link set ${WLAN} down
sudo iw ${WLAN} set monitor control
sudo ip link set ${WLAN} up
|#
;; sudo lshw -class network
;;  tshark -a duration:16 -I -i ${WLAN} -Tfields -e wlan.sa 2>/dev/null | sort -u

;; #:seconds - how long to run tshark
;; #:timeout - how long past the runtime to wait before killing the tshark thread
(define (run-tshark #:tshark-path [tshark-path "/usr/bin/tshark"]
                    #:seconds [seconds 30]
                    #:adapter [adapter "wlan1"]
                    #:timeout [timeout 10]
                    )
  (define addresses (make-hash))
  (define-values (pid inp outp errp)
    (with-handlers ([exn? (λ (err)
                            (fprintf (current-error-port)
                                     "Something went wrong with packet counting.~n")
                            (close-input-port inp)
                            (close-output-port outp)
                            (close-output-port errp))])
      (subprocess false false false
                  tshark-path
                  "-a" (format "duration:~a" seconds)
                  "-I"
                  "-i" adapter
                  "-Tfields" "-e" "wlan.sa")))
  (define read-thread-id
    (thread (λ ()
              ;; MCJ 20210228
              ;; There may be some kind of buffering in the ports that
              ;; behaves differently on the RPi/Raspbian Buster than on a
              ;; Linux machine running Ubuntu 20.04. read-line seems to fail after
              ;; a single read. read-char works. So, we'll build up a list of
              ;; characters, and "flush" them to the address hash when a newline is
              ;; hit. 
              (define loc '())
              (let loop ([ch (read-char inp)])
                (unless (eof-object? ch)
                  (cond
                    [(and (member ch '(#\newline #\linefeed))
                          (not (empty? loc)))
                     (define addr (apply string-append (reverse (map ~a loc))))
                     ;; If something went wrong, don't record random
                     ;; garbage. Record strings of the correct length only.
                     (when (= MAC-ADDRESS-LENGTH (string-length addr))
                       (hash-set! addresses
                                  addr
                                  (add1 (hash-ref addresses addr 0))))
                     (set! loc empty)]
                    ;; Store anything that isn't a newline/linefeed.
                    [(member ch VALID-MAC-ADDR-CHARS)
                     (set! loc (cons ch loc))])
                  (loop (read-char inp))))

              ;; Before we quit, we need to close the subprocess ports.
              (close-input-port inp)
              (close-output-port outp)
              (close-input-port errp)
              )))
  ;; Monitor the thread. Kill it after seconds + 1
  (define watcher-id
    (thread (λ ()
              (sleep (+ timeout seconds))
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