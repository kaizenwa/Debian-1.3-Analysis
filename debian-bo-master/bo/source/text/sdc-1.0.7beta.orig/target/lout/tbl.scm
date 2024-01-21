; OK, we keep this code. It happens to work for equal width
; columns. Spawning is simply ignored.

(define (lout-write-tbl t)
  (letrec
      ((CIDS '#("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L"
	       "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
       (RIDS '#("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l"
	       "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
       (pats (vector-ref t 0))
       (over ; refuses to work.
	(lambda (from delta)
	  (let loop ((res '())
		     (to (+ from delta)))
	    (if (eqv? from to)
		;;;(string-append " @Over " (strings-join res ","))
		""
		(loop (cons (vector-ref CIDS to) res) (- to 1))))))
       (cellpat
	(lambda (pat no)
	  (let* ((c (car pat))
		 (t (vector-ref c 0)))
	    (cons
	     "@Col "
	     (cons
	      (let ((width (vector-ref c 3)))
		(if (eq? width 'AUTO)
		    ""
		    (string-append (number->string (/ width 10)) "c @Wide ")))
	      (cons
	       (let ((style (vector-ref c 2)))
		 (cond
		  ((eq? (vector-ref c 0) 'DECIMAL) "")
		  ((eq? style 'LEFT) "")
		  ((eq? style 'RIGHT) "@RR ")
		  ((eq? style 'CENTER) "@CC ")
		  (else "")))
	       (cons
		(vector-ref CIDS no)
		(cons
		 (if (> (vector-ref c 1) 1)
		     (over (- no 1) (vector-ref c 1))
		     "")
		 (if (null? (cdr pat))
		     '("}")
		     (seppat (cdr pat) (+ no 1)))))))))))
       (seppat
	(lambda (pat no)
	  (if (eq? 'SEP (vector-ref (car pat) 0))
	      (cons
	       (let ((sep (vector-ref (car pat) 2)))
		 (cond
		  ((equal? sep "NO") " ! ")
		  ((equal? sep "YES") " !between{single} ")
		  ((equal? sep "DOUBLE") " !between{double} ")))
	       (cellpat (cdr pat) no))
	      (cons " ! " (cellpat pat no)))))

       (fmtpats
	(lambda (pats)
	  (let loop ((pats pats)
		     (no (- (length pats) 1))
		     (res '()))
	    (if (null? pats)
		(cons #"\n@HContract @VContract @Tab" res)
		(loop (cdr pats) (- no 1)
		      (append
		       (list #"\n@Fmt" (vector-ref RIDS no) " {")
		       (cellpat (reverse (cdar pats)) 0) res))))))
       (adjustcell
	(lambda (str where)
	  (let ((pos (string-find-laststring str where))
		(dl (string-length where)))
	    (if pos
		(string-append (substring str 0 (+ pos dl))
			       "^| "
			       (substring str (+ pos dl) (string-length str)))
		(string-append str "^| ")))))
       (cells
	(lambda (pats cls no)
	  (if (eq? (vector-ref (car pats) 0) 'SEP)
	      (set! pats (cdr pats)))
	  (cons
	    "}"
	    (cons
	     (cond
	      ((eq? 'DECIMAL (vector-ref (car pats) 0))
	       (adjustcell (car cls) (vector-ref (car pats) 2)))
	      (else (car cls)))
	     (cons
	      (string-append (vector-ref CIDS no) " {")
	      (if (pair? (cdr pats))
		  (cells (cdr pats) (cdr cls) (- no 1))
		  '()))))))
       (rows
	(lambda (rs sep)
	  (if (null? rs) '()
	      (let ((r (car rs)))
		(if (eq? (vector-ref r 0) 'SEP)
		    (rows (cdr rs) (vector-ref r 1))
		    (append
		     (append
		      (list #"\n@Row"
			    (vector-ref
			     RIDS
			     (- (length (memq (vector-ref r 0) pats)) 1))
			    (cond
			     ((equal? sep "NO") " ")
			     ((equal? sep "YES") " above{ single } ")
			     ((equal? sep "DOUBLE") " above{ double } ")))
		      (reverse! (cells (cdr (vector-ref r 0))
				       (vector-ref r 1)
				       (- (length (vector-ref r 1)) 1))))
		     (rows (cdr rs) "NO"))))))))
    (apply string-append
	   (append
	    (fmtpats (vector-ref t 0))
	    '(#"\n{")
	    (rows (reverse (vector-ref t 1)) "NO")
	    '(#"\n}//\n")))))
