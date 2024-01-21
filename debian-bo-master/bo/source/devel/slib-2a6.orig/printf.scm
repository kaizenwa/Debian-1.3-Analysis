;;;; "printf.scm" Implementation of standard C functions for Scheme
;;; Copyright (C) 1991-1993, 1996 Aubrey Jaffer.
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(require 'string-case)

;;; Floating point is not handled yet.

(define (stdio:iprintf out-proc format-string . args)
  (define char-count 0)
  (define (out c)
    (cond ((char? c) (set! char-count (+ 1 char-count)))
	  (else (set! char-count (+ (string-length c) char-count))))
    (out-proc c) #t)
  (cond
   ((not (equal? "" format-string))
    (let ((pos -1)
	  (fl (string-length format-string))
	  (fc (string-ref format-string 0)))

      (define (advance)
	(set! pos (+ 1 pos))
	(cond ((>= pos fl) (set! fc #f))
	      (else (set! fc (string-ref format-string pos)))))
      (define (must-advance)
	(set! pos (+ 1 pos))
	(cond ((>= pos fl) (incomplete))
	      (else (set! fc (string-ref format-string pos)))))
      (define (end-of-format?)
	(>= pos fl))
      (define (incomplete)
	(slib:error 'printf "conversion specification incomplete"
		    format-string))

      (let loop ((args args))
	(advance)
	(cond
	 ((end-of-format?))
	 ((eqv? #\\ fc);;Emulating C strings may not be a good idea.
	  (must-advance)
	  (case fc
	    ((#\n #\N) (out #\newline))
	    ((#\t #\T) (out slib:tab))
	    ((#\r #\R) (out #\return))
	    ((#\f #\F) (out slib:form-feed))
	    ((#\newline) #f)
	    (else (out fc)))
	  (loop args))
	 ((eqv? #\% fc)
	  (must-advance)
	  (let ((left-adjust #f)	;-
		(signed #f)		;+
		(blank #f)
		(alternate-form #f)	;#
		(leading-0s #f)		;0
		(width 0)
		(precision -1)
		(type-modifier #f)
		(read-format-number
		 (lambda ()
		   (cond
		    ((eqv? #\* fc)	; GNU extension
		     (must-advance)
		     (let ((ans (car args)))
		       (set! args (cdr args))
		       ans))
		    (else
		     (do ((c fc fc)
			  (accum 0 (+ (* accum 10)
				      (string->number (string c)))))
			 ((not (char-numeric? fc)) accum)
		       (must-advance)))))))
	    (define integer-pad
	      (lambda (s radix)
		(cond ((not (negative? precision))
		       (set! leading-0s #f)))
		(let* ((pre
			(cond ((equal? "" s) "")
			      ((eqv? #\- (string-ref s 0))
			       (set! s (substring s 1 (string-length s)))
			       "-")
			      (signed "+")
			      (blank " ")
			      ((equal? "" s) "")
			      (alternate-form
			       (case radix
				 ((8) "0")
				 ((16) "0x")
				 (else "")))
			      (else "")))
		       (length-so-far (+ (string-length pre)
					 (string-length s))))
		  (cond ((<= width length-so-far)
			 (string-append pre s))
			(left-adjust
			 (string-append
			  pre s
			  (make-string (- width length-so-far) #\ )))
			(leading-0s
			 (string-append
			  pre (make-string (- width length-so-far) #\0)
			  s))
			(else
			 (string-append
			  (make-string (- width length-so-far) #\ )
			  pre s))))))

	    (do ()
		((case fc
		   ((#\-) (set! left-adjust #t) #f)
		   ((#\+) (set! signed #t) #f)
		   ((#\ ) (set! blank #t) #f)
		   ((#\#) (set! alternate-form #t) #f)
		   ((#\0) (set! leading-0s #t) #f)
		   (else #t)))
	      (must-advance))
	    (cond (left-adjust (set! leading-0s #f)))
	    (cond (signed (set! blank #f)))

	    (set! width (read-format-number))
	    (cond ((negative? width)
		   (set! left-adjust #t)
		   (set! width (- width))))
	    (cond ((eqv? #\. fc)
		   (must-advance)
		   (set! precision (read-format-number))))
	    (case fc			;Ignore these specifiers
	      ((#\l #\L #\h)
	       (set! type-modifier fc)
	       (must-advance)))

	    (case fc
	      ;; only - is allowed between % and c
	      ((#\c #\C)		; C is enhancement
	       (out (string (car args)))
	       (loop (cdr args)))

	      ;; only - flag, no type-modifiers
	      ((#\s #\S)		; S is enhancement
	       (let ((s (cond
			 ((symbol? (car args)) (symbol->string (car args)))
			 ((not (car args)) "(NULL)")
			 (else (car args)))))
		 (cond ((not (or (negative? precision)
				 (>= precision (string-length s))))
			(set! s (substring s 0 precision))))
		 (out
		  (cond
		   ((<= width (string-length s)) s)
		   (left-adjust
		    (string-append
		     s (make-string (- width (string-length s)) #\ )))
		   (else
		    (string-append (make-string (- width (string-length s))
						(if leading-0s #\0 #\ )) s))))
		 (loop (cdr args))))

	      ;; SLIB extension
	      ((#\a #\A)		;#\y #\Y are pretty-print
	       (require 'generic-write)
	       (let ((os "") (pr precision))
		 (generic-write
		  (car args) (not alternate-form) #f
		  (cond ((and left-adjust (negative? pr))
			 out)
			(left-adjust
			 (lambda (s)
			   (define sl (- pr (string-length s)))
			   (set! pr (cond ((negative? sl)
					   (out (substring s 0 pr)) 0)
					  (else (out s) sl)))
			   (positive? sl)))
			((negative? pr)
			 (set! pr width)
			 (lambda (s)
			   (set! pr (- pr (string-length s)))
			   (cond ((not os) (out s))
				 ((negative? pr)
				  (out os)
				  (set! os #f)
				  (out s))
				 (else (set! os (string-append os s))))
			   #t))
			(else
			 (lambda (s)
			   (define sl (- pr (string-length s)))
			   (cond ((negative? sl)
				  (set! os (string-append
					    os (substring s 0 pr))))
				 (else (set! os (string-append os s))))
			   (set! pr sl)
			   (positive? sl)))))
		 (cond (left-adjust
			(cond
			 ((> width (- precision pr))
			  (out (make-string (- width (- precision pr))
					    #\ )))))
		       ((not os))
		       ((<= width (string-length os)) (out os))
		       (else
			(out (make-string (- width (string-length os)) #\ ))
			(out os))))
	       (loop (cdr args)))

	      ((#\d #\D #\i #\I #\u #\U)
	       (out (integer-pad
		     (cond ((symbol? (car args))
			    (symbol->string (car args)))
			   ((number? (car args))
			    (number->string (car args)))
			   ((not (car args)) "0")
			   (else "1"))
		     10))
	       (loop (cdr args)))
	      ((#\o #\O)
	       (out (integer-pad (number->string (car args) 8) 8))
	       (loop (cdr args)))
	      ((#\x #\X)
	       (out
		((if (char-upper-case? fc) string-upcase string-downcase)
		 (integer-pad (number->string (car args) 16) 16)))
	       (loop (cdr args)))
	      ((#\%) (out #\%)
		     (loop args))
	      (else
	       (cond ((end-of-format?) (incomplete))
		     (else (out #\%) (out fc) (out #\?)
			   (loop args)))))))
	 (else (out fc)
	       (loop args)))))))
  char-count)				; return number of characters output.

(define (stdio:printf format . args)
  (apply stdio:iprintf display format args))

(define (stdio:fprintf port format . args)
  (if (equal? port (current-output-port))
      (apply stdio:iprintf display format args)
      (apply stdio:iprintf (lambda (x) (display x port)) format args)))

(define (stdio:sprintf s format . args)
  (let ((p 0) (end (string-length s)))
    (apply stdio:iprintf
	   (lambda (x)
	     (cond ((string? x)
		    (do ((i 0 (+ i 1)))
			((>= i (min (string-length x) end)))
		      (string-set! s p (string-ref x i))
		      (set! p (+ p 1))))
		   ((>= p end))
		   ((char? x)
		    (string-set! s p x)
		    (set! p (+ p 1)))
		   (else
		    (string-set! s p #\?)
		    (set! p (+ p 1)))))
	   format
	   args)
    p))

(define printf stdio:printf)
(define fprintf stdio:fprintf)
(define sprintf stdio:sprintf)
