(define (html3-write-tbl t)
  (letrec
      ((adjustcell
	(lambda (str where)
	  (let ((pos (string-find-laststring str where))
		(dl (string-length where)))
	    (if pos
		str
		str))))
       (cells
	(lambda (pats cls)
	  (if (eq? (vector-ref (car pats) 0) 'SEP)
	      (set! pats (cdr pats)))
	  (cons
	   "<TC ALIGN="
	   (cons
	    (cond
	     ((eq? 'DECIMAL (vector-ref (car pats) 0))
	      (string-append "DECIMAL DP=\"" (vector-ref (car pats) 2) "\""))
	     (else
	      (case (vector-ref (car pats) 2)
		((RIGHT) "RIGHT")
		((CENTER) "CENTER")
		(else "LEFT"))))
	    (append
	     `( ,@(if (> (vector-ref (car pats) 1) 1)
		      (list " COLSPAWN="
			    (number->string (vector-ref (car pats) 1))
			    ">")
		      '(">"))
		,(if (eq? 'DECIMAL (vector-ref (car pats) 0))
		     (adjustcell (car cls) (vector-ref (car pats) 2))
		     (car cls)))
	     (if (pair? (cdr pats))
		 (cells (cdr pats) (cdr cls))
		 '()))))))
       (row
	(lambda (r)
	  (cons
	   #"\n<TR>" 
	   (cells (reverse (cdr (vector-ref r 0)))
		  (reverse (vector-ref r 1))))))
       (sep
	(lambda (rs)
	  (if (null? rs) '()
	      (if (eq? (vector-ref (car rs) 0) 'SEP)
		   (cons (row (cadr rs)) (sep (cddr rs)))
		   (cons (row (car rs)) (sep (cdr rs))))))))

    (apply string-append
	   (cons
	    #"\n<TABLE>"
	    (append
	     (apply append
		    (reverse! (sep (vector-ref t 1))))
	     '(#"\n</TABLE>"))))))

; If there is no HTML-3 we use prformatting as for e.g., man and info

(define html-preformatted-write-tbl
  (let ((fn (delay (begin
		     (v-load *typeset-lib* "target/man" "tbl" )
		     (make-subfile-name)))))
    (lambda (tbl)
      (let* ((fni (string-append (force fn) ".tbl"))
	     (fno (string-append (force fn) ".asc"))
	     (file (open-output-file fni))
	     (str (open-output-string)))
	(display #"\n<PRE>" file)
	(display (tbl-write-tbl tbl) file)
	(display #"\n</PRE>" file)
	(close-output-port file)
	(system (string-append "gtbl " fni
			       " |groff -Tascii | col -b -f | sed -e '/^$/d' >"
			       fno))
	(file-cat fno str)
	(for-each delete-file (list fni fno))
	(close-output-port str)))))

