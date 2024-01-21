(define (make-biblio . nun)
  (if (pair? bib-list)
      (begin
	(for-each write-out 
		  (list do-sect (sc '++)  (lw 'bibl) "\\par "))
	(inc-indent 1)
	(format-all-bib)
	(dec-indent)
	(write-out #\newline))))

(define (format-all-bib)
  (let loop ((num 1)
	     (tag (reverse bib-list)))
    (if (pair? tag)
	(let ((item (assoc (car tag) bib-db)))
	  (if (pair? item)
	      (format-bib-item (cdr item) num)
	      (warn 0 `("Bibliographie item " ,(car tag)
					      " not in database! Ignored.")))
	  (loop (+ num 1) (cdr tag))))))

(define (format-bib-item data num)
  (let ((auth (cdr (assoc 'AUTHOR data)))
	(ttl  (cdr (assoc 'TITLE data)))
	(publ (cdr (assoc 'PUBL data)))
	(year (cdr (assoc 'YEAR data)))
	(isbn (cdr (assoc 'ISBN data))))
    (for-each write-out
	      (list para-start
	            "\\fi-357 [" num "]\\tab\\i1 " auth 
		    "\\i0: " ttl
		    "; " year " \\i1 " publ "\\i0\\par " #\newline))))
