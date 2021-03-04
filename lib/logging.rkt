#lang racket

(provide log-wifi-debug
         log-wifi-info
         log-wifi-info
         log-wifi-warning
         log-wifi-error
         log-wifi-fatal
         proc:logger
         )

(require gregor)

(define-logger wifi)
(define log-wifi-receiver
  (make-log-receiver wifi-logger 'debug))

;; Racket loggers wait for synchronizable events
;; from log messages. In this case, the forms
;;   log-wifi-debug
;;   log-wifi-info
;;   log-wifi-warning
;;   log-wifi-error
;;   log-wifi-fatal

(define (proc:logger)
  (let loop ()
    ;; lvl msg value topic
    (define v (sync log-wifi-receiver))
    (define LOGFILE
      (format "/home/pi/wifi-nearby-~a.log"
              (date->iso8601 (today))))
    (with-output-to-file LOGFILE
      #:mode 'text
      #:exists 'append
      (thunk
       (printf "[ ~a ] ~a~n"
               (vector-ref v 0)
               (vector-ref v 1))))
    (loop)
    ))