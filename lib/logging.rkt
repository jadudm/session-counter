#lang racket

(provide log-wifi-debug
         log-wifi-info
         log-wifi-info
         log-wifi-warning
         log-wifi-error
         log-wifi-fatal
         proc:logger
         )

(require sql db
         (prefix-in deta: deta)
         gregor)

(deta:define-schema log-entry
  ([id deta:id/f #:primary-key #:auto-increment]
   [timestamp deta:datetime-tz/f]
   [level deta:string/f #:contract non-empty-string?]
   [message deta:string/f #:contract non-empty-string?]))

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
(define LOGFILE "/home/pi/wifi-nearby-log.sqlite")
(when (not (file-exists? LOGFILE))
  (close-output-port (open-output-file LOGFILE)))
(define logdb
  (make-parameter (sqlite3-connect #:database LOGFILE)))

(define (proc:logger)
  ;; Quietly does nothing if it exists.
  (deta:create-table! (logdb) 'log-entry)
  
  (thread
   (thunk
    (let loop ()
      ;; lvl msg value topic
      (define v (sync log-wifi-receiver))
      (fprintf (current-error-port)
               "[ ~a ] ~a~n"
               (vector-ref v 0)
               (vector-ref v 1))
      (define entry
        (make-log-entry
         #:timestamp (now/moment/utc)
         #:level (~a (vector-ref v 0))
         #:message (vector-ref v 1)))
      (deta:insert-one! (logdb) entry)
      (loop)))))