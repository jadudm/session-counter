#lang racket

(require sql db)

(define in-filename (make-parameter false))
(define out-filename (make-parameter false))

(define (parse)
  (command-line
   #:program "oui-to-sqlite"
   #:once-each
   ["--in" in-file
           "OUI file to process."
           (in-filename in-file)]
   ["--out" out-file
            "SQLite DB to write."
            (out-filename out-file)]
   #:args ()
   (process)))

(define (process)
  (cond
    [(not (file-exists? (out-filename)))
     (close-output-port (open-output-file (out-filename)))
     (define conn (sqlite3-connect #:database (out-filename)))
     (query-exec
      conn
      (create-table oui
                    #:columns
                    [mac text]
                    [id text]
                    [manufacturer text]))
     (disconnect conn)]
    [else
     (fprintf (current-error-port)
              "SQLite DB file already exists. Existing.~n")
     (exit)])

  (define lines (file->lines (in-filename)))
  (define all-parts
    (for/list ([line lines])
      (define clean (regexp-replace #px"\\#.*" (string-trim line) ""))
      (define parts (string-split clean "\t"))
      parts))

  (when (file-exists? (out-filename))
    (define conn (sqlite3-connect #:database (out-filename)))
    (for ([parts all-parts])
      (when (= (length parts) 3)
        (query-exec
         conn
         (insert #:into oui
                 #:set
                 [mac ,(first parts)]
                 [id ,(second parts)]
                 [manufacturer ,(third parts)])))
      )
    (disconnect conn)
    ))
    
(parse)