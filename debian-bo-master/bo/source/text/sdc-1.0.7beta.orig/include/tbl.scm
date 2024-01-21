; Read in all the contents of a <tbl> element
(define (make-tbl-reader)
  (letrec
    ((patterns '())			; All the lists are reverse ordered.
     (rows '())				
     (cr #f)				; during the read process we use 
     (cp #f)				; these as short cut to the current
					; row and pattern
     (highpat 0)			; uniq name for unnamed pattern
     (sepf rowsep)			; current (row/col)
     (sep (lambda (args)
	    (sepf)))
     (atvn (lambda (att) (string->number (atv att))))

       ; normalizers. The final pattern list is always: <pat> (<sep><pat>)*
     (normal-pat (lambda (pat)
		   (if (null? (cdr pat))
		       pat
		       (cons (car pat) (normal-sep (cdr pat))))))
     (normal-sep (lambda (pat)
		   (if (eq? (vector-ref (car pat) 0) 'SEP)
		       (cons (car pat) (normal-pat (cdr pat)))
		       (cons '#(SEP 0 "NO" 0) (normal-pat pat)))))

     (pat (lambda (args)
	    (set! sepf colsep)		       ; only here we expect them
	    (let ((p (atv 'ID)))	       ; determine a name
	      (set! cp (if (equal? p "")
			   (begin (set! highpat (+ highpat 1)) highpat)
			   p)))
	    (set! cr (cons cp '()))	       ; open new row (shortcut)
	    (set! patterns (cons cr patterns)) ; offiziel
	    (compile-structure)
	    (set! cp cr)		       ; for following rows
	    (set-cdr! (car patterns) (normal-pat (cdar patterns)))
	    (set! sepf rowsep)))	       ; back to rows

     (patcol (lambda (type spawn align width)
	       (set-cdr! cr (cons 
			     (vector type spawn align width)
			     (cdr cr)))))
     (colsep (lambda () (patcol 'SEP 0 (atv 'SEP) 0)))
     (lftcol (lambda (args) (patcol 'CELL (atvn 'SPAWN) 'LEFT 'AUTO)))
     (rgtcol (lambda (args) (patcol 'CELL (atvn 'SPAWN) 'RIGHT 'AUTO)))
     (ctrcol (lambda (args) (patcol 'CELL (atvn 'SPAWN) 'CENTER 'AUTO)))
     (mltcol (lambda (args) (patcol 'CELL (atvn 'SPAWN)
				    (string->symbol (atv 'ALIGN))
				    (atvn 'WIDTH))))
     (dcmcol (lambda (args) (patcol 'DECIMAL (atvn 'SPAWN)
				    (atv 'ALIGN) 'AUTO)))

      ; Separators between rows are a row by themself
     (rowsep (lambda ()
	       (set! rows (cons (vector 'SEP (atv 'SEP)) rows))))

       ; Read in one row finally fill it with empty cells up to the
       ; length determined by the pattern
     (row (lambda (args)
	    (set! cp
		  (let* ((ap (atv 'PATTERN))
			 (np (assoc ap patterns))) ; find a pattern
		    (if np np cp)))	           ; use it or the default
	    (set! cr (vector cp '()))	           ; open new
	    (set! rows (cons cr rows))
	    (compile-structure)
	    (let loop ((nop (- (quotient (length (cdr cp)) 2)
			       (length (vector-ref cr 1)))))
	      (if (>= nop 0)		           ; have to fill?
		  (begin
		    (vector-set! cr 1 (cons "" (vector-ref cr 1)))
		    (loop (- nop 1)))))))

     (col (lambda (args)
	    (vector-set! cr 1 (cons (compile-structure-to-string)
				    (vector-ref cr 1))))))

      ; And eventually the make calculus.

    (lambda (cmd)
      (if (eq? cmd 'get)
	  (vector patterns rows)
	  (list `(C        . ,col)
		`(R        . ,row)
		`(SEP      . #( ,sep ""))
		`(LEFT     . #( ,lftcol ""))
		`(RIGHT    . #( ,rgtcol ""))
		`(CENTER   . #( ,ctrcol ""))
		`(DECIMAL  . #( ,dcmcol ""))
		`(BLOCK    . #( ,mltcol ""))
		`(PATTERN  . ,pat))))))

(define-macro (ttail s) `(tail (tail ,s)))

(define matching-tag
  (letrec
      ((inner-match (lambda (stream)
		      (cond
		       ((eq? (token-type (head stream)) 'ENDTAG) stream)
		       ((eq? (token-type (head stream)) 'STARTTAG)
			(inner-match (tail (inner-match (tail stream)))))
		       (else (inner-match (tail stream)))))))
    (lambda (stream)
      (if (not (eq? (token-type (head stream)) 'STARTTAG))
	  #f
	  (inner-match (tail stream))))))

(define (read-tbl data-converter)
  (lambda (c a s)
    (letrec
	((tbl-end (matching-tag s))
	 (atvn (lambda (t n) (string->number (car (xatv t n)))))
	 (definition-tag
	   (lambda (token)
	     (case (token-gi token)
	       ((LEFT)    `#(CELL ,(atvn token 'SPAWN) LEFT AUTO))
	       ((RIGHT)   `#(CELL ,(atvn token 'SPAWN) RIGHT AUTO))
	       ((CENTER)  `#(CELL ,(atvn token 'SPAWN) CENTER AUTO))
	       ((BLOCK)   `#(CELL ,(atvn token 'SPAWN)
				  ,(STRING->symbol (xatv token 'ALIGN))
				  ,(atvn token 'WIDTH)))
	       ((DECIMAL) `#(DECIMAL ,(atvn token 'SPAWN)
				     ,(xatv token 'ALIGN) AUTO))
	       ((SEP)     `#(SEP 0 ,(car (xatv token 'SEP)) 0)))))
	 (row-sep (lambda (token rows)
		    (cons (vector 'SEP (car (xatv token 'SEP))) rows)))
	 (pat-cell (lambda (cdef stream)
		     (pat-sep (cons (definition-tag (head stream)) cdef)
			      (ttail stream))))
	 (pat-sep (lambda (cdef stream)
		    (cond
		     ((stream-empty? stream) cdef)
		     ((eq? (token-gi (head stream)) 'SEP)
		      (pat-cell (cons (definition-tag (head stream)) cdef)
				(ttail stream)))
		     (else (pat-cell (cons '#(SEP 0 "NO" 0) cdef) stream)))))
	 (pattern (lambda (pats rows stream)
		    (let* ((end (matching-tag stream)) ; matching end
			   (p (xat (head stream) 'ID)) ; name of the pattern
			   (this
			    (cons
			     (if (eq? (arg-type p) 'IMPLIED)
				 (length pats)
				 (arg-val p))
			     (pat-cell '() (substream (tail stream) end)))))
		      (p/r this (cons this pats) rows (tail end)))))
	 (col (lambda (stream)		; HACK convert into a string
		(apply string-append	; Used because of old code!
		       (map (lambda (i) (data-token-data i))
			    (accumulate
			     cons '()
			     (stream-map data-converter
					 (walk-elements
					  (lambda (na s)
					    (set! a na) ; !! HACK !!
					    empty-stream)
					  a stream)))))))
	 (cols (lambda (pat init stream)
		 (cond
		  ((stream-empty? stream)
		   (if (null? pat)
		       init
		       (cols (cddr pat) (cons "" init) stream)))
		  ((and (eq? (token-type (head stream)) 'STARTTAG)
			(eq? (token-gi (head stream)) 'C))
		   (let ((end (matching-tag stream)))
		     (cols (cddr pat)
			   (cons (col (substream (tail stream) end)) init)
			   (tail end))))
		  (else (cols (tail stream))))))
	 (row (lambda (cp pats rows stream)
		(let* ((end (matching-tag stream))
		       (p (xatv (head stream) 'PATTERN))
		       (np (assoc p pats))
		       (this-p (if np np cp)))
		  (p/r this-p pats
		       (cons (vector this-p
				     (cols this-p '()
					   (substream (tail stream) end)))
			     rows)
		       (tail end)))))
	 (p/r (lambda (cp pats rows stream)
		(case (token-gi (head stream))
		  ((PATTERN) (pattern pats rows stream))
		  ((SEP)
		   (p/r cp pats (row-sep (head stream) rows) (ttail stream)))
		  ((R) (row cp pats rows stream))
		  ((NEWPAGE) (p/r cp pats rows (ttail stream)))
		  ((TBL) (terminate pats rows stream))
		  (else (p/r cp pats rows (ttail stream))))))
	 (terminate (lambda (pats rows stream)
		      (c a (cons (vector pats rows) (tail stream))))))
      (p/r #f '() '() (tail s)))))
