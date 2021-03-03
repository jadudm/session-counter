#lang racket

(provide (all-defined-out))
            
;; MAC addresses are 17 characters long.
(define MAC-ADDRESS-LENGTH 17)
(define VALID-MAC-ADDR-CHARS (string->list "abcdef0123456789:"))

;; We're going to observe for 45 seconds every minute
(define OBSERVE-SECONDS 45)

;; Every 10 minutes we will report our findings.
;; We'll do this by asking if the number of minutes in a datetime stamp
;; is (zero? (modulo minutes 10)))
(define REPORT-MINUTES 5)

;; How many times do we need to see a device in this
;; time period in order to log it to the server?
(define APPEARANCE-THRESHOLD 3)


(define SCHEME "https")
(define HOST "directus-demo.app.cloud.gov")

(define COLLECTION "people2")

(define USERNAME (getenv "SESSIONCOUNTUSERNAME"))
(define PASSWORD (getenv "SESSIONCOUNTPASSWORD"))

(when (or (not USERNAME)
          (not PASSWORD))
  (fprintf (current-error-port)
           "SESSIONCOUNTUSERNAME|PASSWORD must be in the env. Exiting.~n")
  (exit))

(define HTTP-RETRY-THRESHOLD 10)