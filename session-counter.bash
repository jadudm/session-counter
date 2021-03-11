#!/usr/bin/env racket
#lang racket

(require "session-counter.rkt")

(define oui-db (make-parameter false))

(define shell
  (command-line
   #:program "wifi-nearby"
   #:once-each
   [("-o" "--oui") path
                   "Path to the OUI SQLite database."
                   (oui-db path)]
   #:args ()
   (main #:oui-db (oui-db))
   ))