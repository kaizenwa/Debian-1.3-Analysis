(define (tbl-write-tbl t)
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
	   (cond
	    ((eq? 'DECIMAL (vector-ref (car pats) 0))
	     (adjustcell (car cls) (vector-ref (car pats) 2)))
	    (else (car cls)))
	   (if (pair? (cdr pats))
	       (cons #"\t"(cells (cdr pats) (cdr cls)))
	       '()))))
       (pats
	(lambda (pat)
	  (if (null? pat)
	      '(".")
	      (let* ((s (vector-ref (car pat) 2))
		     (c (cadr pat))
		     (a (vector-ref c 2))
		     (spawn (vector-ref c 1)))
		(cons
		 (cond
		  ((equal? s "YES") " | ")
		  ((equal? s "DOUBLE") " || ")
		  (else " "))
		 (cons
		  (cond
		   ((eq? a 'RIGHT)  "r")
		   ((eq? a 'CENTER) "c")
		   (else "l"))
		  (do ((s spawn (- s 1))
		       (sc '() (cons " s " sc)))
		      ((eqv? s 1) (append sc (pats (cddr pat)))))))))))
       (row
	(lambda (r)
	  (append
	   (cons #"\n" (pats (cons '#(SEP 0 "NO" 0)
				   (reverse (cdr (vector-ref r 0))))))
	   (cons #"\n" (cells (reverse (cdr (vector-ref r 0)))
			      (reverse (vector-ref r 1)))))))
       (sep
	(lambda (rs)
	  (if (null? rs) '()
	      (if (eq? (vector-ref (car rs) 0) 'SEP)
		   (cons (cons #"" (row (cadr rs))) (sep (cddr rs)))
		   ; separator einfuegen
		   (cons (row (car rs)) (sep (cdr rs))))))))

    (apply string-append
	   (cons
	    #"\n.TS"
	    (append
	     (apply append
		    (list-join '(#"\n.T&"') (sep (reverse (vector-ref t 1)))))
	     '(#"\n.TE"))))))

(define (tbl-preprocess-tbl t)
  (let ((tbl-file (make-tmp-file-name)))
  (with-output-to-file tbl-file
    (lambda () (display (tbl-write-tbl t))))
  (let ((o-file (make-tmp-file-name)))
    (system (string-append tbl-prog " " tbl-file ">" o-file))
    (let* ((port (open-output-string)))
      (file-cat o-file port)
      (delete-file tbl-file)
      (delete-file o-file)
      (close-output-port port)))))

(define (tbl-nroff-tbl t)
  (let ((tbl-file (make-tmp-file-name)))
  (with-output-to-file tbl-file
    (lambda () (display (tbl-write-tbl t))))
  (let ((o-file (make-tmp-file-name)))
    (system (string-append tbl-prog " " tbl-file
			   "|" nroff-prog
			   "| col -b -f | sed -e '/^$/d' > "
			   o-file))
    (let* ((port (open-output-string)))
      (file-cat o-file port)
      (delete-file tbl-file)
      (delete-file o-file)
      (close-output-port port)))))
