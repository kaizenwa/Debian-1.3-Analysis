(message 1 "Loading literate")
(set! *load-path* (append *typeset-lib* *load-path*))
(set! sgml-opts (cons "-i Literat " sgml-opts))
(set! sgml-subdir "sgml")

(load "include/normal.scm")

(define literate-pass
  (rdp-cond*
   `((#(DATA) ,(rdp-hmap (lambda (t) (plain-tr-string (data-token-data t)))))
     (#(OUTPUT PI) ,pass-token-action)
     (#(ENDTAG LITERATE))
     (#() ,(rdp-skip 1)))))

(define literate-g
  (rdp-cond*
   `((#(STARTTAG LITERATE)
      ,(rdp-repll
	(let ((last #f))
	  (lambda (t)
	    (let ((file (xat t 'FILE)))
	      (if (or (eq? (arg-type file) 'IMPLIED)
		      (equal? (xatv t 'FILE) ""))
		  (if last
		      `(#(DIVERT ,last) #(DIVERT POP))
		      '(#f #f))
		  (begin
		    (set! last (arg-val file))
		    `(#(DIVERT ,last) #(DIVERT POP)))))))
	  literate-pass))
     (#(ENDTAG (DOCUMENT REPORT BOOK MANPAGE)))
     (#() ,(rdp-skip 1)))))

(define (literate-prepare o d e)
  (doc-preprocess-hook 'run)
  ((stream-display-diverted)
   (stream-through
    (token-stream o)
    (lambda (s)
      (rdp-parse literate-g s)))))

(define (literate-compile-function o d e)
  (if (equal? d "-")
      (literate-prepare o d e)
      (with-output-to-file
	  (string-append d e)
	(lambda ()
	  (literate-prepare o d e)))))

(set! compile-function literate-compile-function)
