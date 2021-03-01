#lang racket

(provide get-token insert-into-collection)

(require "constants.rkt")
(require  net/http-easy json gregor)

(define (make-uri path)
  (format "~a://~a~a" SCHEME HOST path))

(define (get-token #:username user #:password pass)
  (define resp
    (post (make-uri "/auth/login")
          #:json (hasheq 'email USERNAME
                         'password PASSWORD)))
  (define json (response-json resp))
  (hash-ref (hash-ref json 'data) 'access_token))

(define (insert-into-collection collection mac count #:token token)
  (define resp
    (post (make-uri (format "/items/~a" collection))
          #:headers (hasheq 'Authorization
                            (format "Bearer ~a" token))
          #:json (hasheq 'libid "ME-LEW"
                         'local_date_created (datetime->iso8601 (now))
                         'mac mac
                         'oui "SOON"
                         'count count)))
  (define json (response-json resp))
  json)