;;; Module RDP - recursive descendant parser
;{{{ module declaration

(module rdp
	(include "stream.sch")
	(include "rdp.sch")
	(export
	 (rdp-parse grammar stream . variables)
	 (rdp-shift c h s . f)
	 (rdp-begin f . fx)
	 (rdp-accumulate combiner null f . fx)
	 (rdp-accu-list f . fx)
	 (rdp-let bindings f . fx)
	 (rdp-let-fetch bindings f . fx)
	 (rdp-set-fetch bindings f . fx)
	 (rdp-loop f . fx)
	 (rdp-iter* f delayed-next)
	 (rdp-cond alist)
	 (rdp-cond* alist)
	 (rdp-env-cond alist)
	 (rdp-continue what-to-continue)
	 (rdp-leave c h s)
	 (rdp-map function . parsers)
	 (rdp-map1 function f . fx)
	 (rdp-env-map function f . fx)
	 (rdp-hmap function)
	 (rdp-pretend . tokens)
	 (rdp-insert . tokens)
	 (rdp-skip n)
	 (rdp-wrap before after f . fx)
	 (rdp-process grammar . variables)
	 (rdp-sgram generator)
	 (rdp-repll replacement f . fx)
	 (make-state-env)		; move it somewhere else
	 )
	(import
	 (strings "strings.scm")
	 (stream "stream.scm")
	 (token-type compile "compile.scm")
	 (data-token-data compile "compile.scm")
	 (token-gi compile "compile.scm")
	 (token-args compile "compile.scm")
	 (control "control.scm")
	 ))

;}}}

;{{{ (make-state-env)

(define (state-env states)
  (letrec
      ((del (lambda (old what)
	      ; what ist a list of names, old a list of states
	      (if (null? old)
		  '()
		  (let ((p (memq (caar old) what)))
		    (if p
			(del (cdr old) (rm1 (car p) what))
			`(,(car old) . ,(del (cdr old) what)))))))
       (edel (lambda what (state-env (del states what))))
       (set (lambda (old pairs)
	      (cond
	       ((null? pairs) old)
	       ((null? old)
		(error "parse-state" "set: no variable " (caar pairs)))
	       (else
		(let ((p (assq (caar old) pairs)))
		  (if p
		      `((,(caar old) . ,(cadr p))
			. ,(set (cdr old) (rm1 p pairs)))
		      `(,(car old) . ,(set (cdr old) pairs))))))))
       (eset (lambda pairs (state-env (set states pairs))))
       (setp (lambda (old pairs)
	      (cond
	       ((null? pairs) old)
	       ((null? old)
		(error "parse-state" "setp: no variable " (caar pairs)))
	       (else
		(let ((p (assq (caar old) pairs)))
		  (if p
		      `((,(caar old) . ,(cdr p))
			. ,(setp (cdr old) (rm1 p pairs)))
		      `(,(car old) . ,(setp (cdr old) pairs))))))))
       (esetp (lambda pairs (state-env (setp states pairs))))
       (mod (lambda (token old pairs)
	      (cond
	       ((null? pairs) old)
	       ((null? old)
		(error "parse-state" "mod: no variable " (caar pairs)))
	       (else
		(let ((p (assq (caar old) pairs)))
		  (if p
		      `((,(caar old)
			 . ,((cadr p) token (cdar old)))
			 . ,(mod token (cdr old) (rm1 p pairs)))
		       `(,(car old) . ,(mod token (cdr old) pairs))))))))
       (emod (lambda (token . pairs) (state-env (mod token states pairs))))
       (wrp2
	(lambda (token . triples)
	      (do ((inside (make-queue))
		   (past (make-queue))
		   (todo triples)
		   (states states (cdr states)))
		  ((null? states)
		   (if (not (null? todo))
		       (error "state-env wrp" "no variable" (caar todo)))
		   (cons (state-env (queue-value inside))
			 (queue-value past)))
		(if (null? todo)
		    (begin
		      (apply enqueue! inside states)
		      (set! states '(dummy)))
		    (let ((p (assq (caar states) todo)))
		      (if (not p)
			  (enqueue inside (car states))
			  (begin
			    (enqueue
			     inside
			     `(,(caar states)
				. ,((cadr p) token (cdar states))))
			    (enqueue
			     past
			     `(,(caar states)
			       . ,((caddr p) token (cdar states))))
			    (set! todo (rm1 p todo)))))))))
       (wrp (lambda triples
	      (do ((inside (make-queue))
		   (past (make-queue))
		   (todo triples)
		   (states states (cdr states)))
		  ((null? states)
		   (if (not (null? todo))
		       (error "state-env wrp" "no variable" (caar todo)))
		   (cons (state-env (queue-value inside))
			 (queue-value past)))
		(if (null? todo)
		    (begin
		      (apply enqueue! inside states)
		      (set! states '(dummy)))
		    (let ((p (assq (caar states) todo)))
		      (if (not p)
			  (enqueue inside (car states))
			  (begin
			    (enqueue
			     inside
			     `(,(caar states)
				. ,((cadr p) (cdar states))))
			    (enqueue
			     past
			     `(,(caar states)
			       . ,((caddr p) (cdar states))))
			    (set! todo (rm1 p todo)))))))))
       (get (lambda (what)
	      (let ((p (assq what states)))
		(if p
		    (cdr p)
		    (error "parse-state" "get: no variable" what)))))
       (def (lambda (old)
	      (lambda pairs
		(if (null? pairs)
		    (state-env old)
		    (apply (def `((,(caar pairs) . ,(cadar pairs)) . ,old))
			   (cdr pairs)))))))
    (lambda (cmd)
      (case cmd
	((get) get)
	((setp) esetp)
	((wrp) wrp)
	((wrpe) wrp2)
	((set) eset)
	((mod) emod)
	((def) (def states))
	((del) edel)
	(else (error "state-env-dispatch" "unknown cmd" cmd))))))

(define (make-state-env) (state-env '()))

;}}}

;{{{ (rdp-parse grammar stream . variables)

(define (rdp-parse grammar stream . variables)
  (if
   (stream-empty? stream)
   stream
   (grammar
    `(,(lambda (c h s) s))
    (apply ((make-state-env) 'def) variables)
    stream)))

;}}}
;{{{ (rdp-shift c h s . f)

(define (rdp-shift c h s . f)
  (cond
   ((null? f) ((car c) (cdr c) h s))
   ((null? (cdr f)) ((car f) c h s))
   (else ((car f) (append (cdr f) c) h s))))

;}}}
;{{{ (rdp-begin f . fx)

(define (rdp-begin f . fx)
  (if (null? fx)
      f
      (lambda (c h s)
	(f (append fx c) h s))))

;}}}
;{{{ (rdp-accumulate combiner null f . fx)

(define (rdp-accumulate combiner null f . fx)
  (lambda (c h s)
    (if
     (stream-empty? s)
     null
     (let ((pending 'undefined) (pending-h 'undefined))
       (let
	   ((part (accumulate
		   combiner null
		   (apply
		    rdp-shift
		    `(,(lambda (c h s)
			 (set! pending s)
			 (set! pending-h h)
			 empty-stream))
		    h s f fx))))
	 (cons-stream
	  part
	  (rdp-reduce c pending-h pending)))))))

;}}}
;{{{ (rdp-accu-list f . fx)

(define (rdp-accu-list f . fx)
  (lambda (c h s)
    (if
     (stream-empty? s)
     '()
     (let ((pending 'undefined) (pending-h 'undefined))
       (let ((part (stream->list
		    (apply
		     rdp-shift
		     `(,(lambda (c h s)
			  (set! pending s)
			  (set! pending-h h)
			  empty-stream))
		     h s f fx))))
	 (cons-stream
	  part
	  (rdp-reduce c pending-h pending)))))))

;}}}
;{{{ (rdp-let bindings f . fx)

(define (rdp-let bindings f . fx)
  (lambda (c h s)
    (let* ((nh (apply (h 'wrp) bindings))
	   (trailer (lambda (c h s)
		      (rdp-reduce c (apply (h 'setp) (cdr nh)) s))))
      (f (if (null? fx)
	     (rdp-push trailer c)
	     (append fx (rdp-push trailer c)))
	 (car nh) s))))

;}}}
;{{{ (rdp-let-fetch bindings f . fx)

(define (rdp-let-fetch bindings f . fx)
  (lambda (c h s)
    (let* ((nh (apply (h 'wrpe) (head s) bindings))
	   (trailer (lambda (c h s)
		      (rdp-reduce c (apply (h 'setp) (cdr nh)) s))))
      (f (if (null? fx)
	     (rdp-push trailer c)
	     (append fx (rdp-push trailer c)))
	 (car nh) s))))

;}}}
;{{{ (rdp-set-fetch bindings f . fx)

(define (rdp-set-fetch bindings f . fx)
  (lambda (c h s)
    (f
     (if (null? fx) c (append fx c))
     (apply (h 'mod) (head s) bindings)
     s)))

;}}}
;{{{ (rdp-loop f . fx)

(define (rdp-loop f . fx)
  (letrec
      ((loop (lambda (c h s)
	       (f
		(append fx
			(rdp-push
			 (lambda (c h ns)
			   (if (eq? ns s)
			       (rdp-reduce c h ns)
			       (loop c h ns)))
			 c))
		h
		s))))
    loop))

;}}}
;{{{ (rdp-iter* f delayed-next)

(define (rdp-iter* f delayed-n)
  (lambda (c h s)
    (f
     (rdp-push
      (lambda (c h ns)
	(if (eq? ns s)
	    (rdp-reduce c h ns)
	    ((force delayed-n) c h ns)))
      c)
     h s)))

;}}}
;{{{ (rdp-cond a)

(define (rdp-cond a)
  (lambda (c h s)
    (let ((t (head s)))
      (let loop ((x a))
	(cond
	 ((null? x) (rdp-reduce c h s))
	 ((vector? (caar x))
	  (if (token-in? t (caar x))
	      (apply rdp-shift c h s (cdar x))
	      (loop (cdr x))))
	 ((procedure? (car x)) ((car x) (lambda () (loop (cdr x))) c h s))
	 (((caar x) t) (apply rdp-shift c h s (cdar x)))
	 (else (loop (cdr x))))))))

;}}}
;{{{ (rdp-cond* a)

; This code is equvivalent to (rdp-loop (rdp-cond a))

(define (rdp-cond* a)
  (lambda (c h s)
    (let ((t (head s))
	  (repeat (rdp-push
		   (lambda (c h ns)
		     (if (eq? ns s)
			 (rdp-reduce c h ns)
			 ((rdp-cond* a) c h ns)))
		   c)))
      (let loop ((x a))
	(cond
	 ((null? x) (rdp-reduce c h s))
	 ((vector? (caar x))
	  (if (token-in? t (caar x))
	      (apply rdp-shift repeat h s (cdar x))
	      (loop (cdr x))))
	 ((procedure? (car x)) ((car x) (lambda () (loop (cdr x)))
					repeat h s))
	 (((caar x) t) (apply rdp-shift repeat h s (cdar x)))
	 (else (loop (cdr x))))))))

;}}}
;{{{ (rdp-env-cond a)

(define (rdp-env-cond a)
  (lambda (c h s)
    (if (stream-empty? s)
	s
	(let ((t (head s))
	      (get (h 'get)))
	  (let loop ((x a))
	    (if (apply (caaar x) t (map get (cdaar x)))
		(apply rdp-shift c h s (cdar x))
		(loop (cdr x))))))))

;}}}
;{{{ (rdp-continue what-to-continue)

(define (rdp-continue what-to-continue)
  (lambda (ignored-c h s) (rdp-reduce what-to-continue h s)))

;}}}
;{{{ (rdp-leave) to leave endless loops (one level)

(define (rdp-leave c h s)  (rdp-reduce c h s))

;}}}
;{{{ (rdp-map function . parsers)

(define (rdp-map1 function f . fx)
  (lambda (c h s)
    (let ((pending-h 'undefined) (pending-s 'undefined))
      (let ((internal-stack `(,(lambda (c h s)
				 (set! pending-s s)
				 (set! pending-h h)
				 empty-stream))))
	(let* ((parsed (apply rdp-shift internal-stack h s f fx))
	       (result (function parsed)))
	  (let loop ((s parsed))
	    (if (eq? pending-s 'undefined)
		(loop (tail s))
		#t))
	  (stream2-append
	   result (delay (rdp-reduce c pending-h pending-s))))))))

(define (rdp-map-collect parsers)
  (if
   (null? parsers)
   rdp-leave				; yields '() from map/internal-stack
   (lambda (c h s)
     (let ((ph 'undefined) (ps 'undefined))
       (let ((internal-stack `(,(lambda (c h s)
				  (set! ph h)
				  (set! ps s)
				  empty-stream))))
	 (let (;(next (rdp-shift internal-stack h s (car parsers)))
	       (next ((car parsers) internal-stack h s)))
	   ; Ensure, that the stream has been read until the end is known.
	   (let loop ((s next))
	     (if (eq? ps 'undefined)
		 (loop (tail s))
		 #t))
	   (cons next (rdp-shift c ph ps
				 (rdp-map-collect (cdr parsers))))
	   ))))))

(define (rdp-map function . parsers)
  (lambda (c h s)
    (let ((pending-h 'undefined) (pending-s 'undefined))
      (let ((internal-stack `(,(lambda (c h s)
				 (set! pending-s s)
				 (set! pending-h h)
				 '()))))
	(let* ((parsed (rdp-shift internal-stack h s
				  (rdp-map-collect parsers)))
	       (result (apply function parsed)))
	  (stream-append result (rdp-reduce c pending-h pending-s)))))))

;}}}
;{{{ (rdp-env-map function f . fx)

;;; strange error: rename it as rdp-map-env and bigloo 1.8 will break!

(define (rdp-env-map function f . fx)
  (lambda (c h s)
    (let ((pending-h 'undefined) (pending-s 'undefined))
      (let ((internal-stack `(,(lambda (c h s)
				 (set! pending-s s)
				 (set! pending-h h)
				 empty-stream))))
	(let* ((parsed (apply rdp-shift internal-stack h s f fx))
	       (result (function parsed h)))
	  (let loop ((s parsed))
	    (if (eq? pending-s 'undefined)
		(loop (tail s))
		#t))
	  (stream2-append
	   result (delay (rdp-reduce c pending-h pending-s))))))))

;}}}
;{{{ (rdp-hmap function)

(define (rdp-hmap function)
  (lambda (c h s)
    (cons-stream
     (function (head s))
     (rdp-reduce c h (tail s)))))

;}}}
;{{{ (rdp-pretend . tokens)

(define (rdp-pretend . tokens)
  (cond
   ((null? tokens) rdp-leave)
   ((null? (cdr tokens)) (lambda (c h s)
			   (rdp-reduce c h (cons-stream (car tokens) s))))
   (else (lambda (c h s)
	   (rdp-reduce c h (apply stream-insert s tokens))))))

;}}}
;{{{ (rdp-insert . tokens)

(define (rdp-insert . tokens)
  (lambda (c h s)
    (cond
     ((null? tokens) (rdp-reduce c h s))
     ((null? (cdr tokens)) (cons-stream (car tokens) (rdp-reduce c h s)))
     (else   (apply stream-insert (rdp-reduce c h s) tokens)))))

;}}}
;{{{ (rdp-skip n)

(define (rdp-skip n)
  (lambda (c h s)
; bigloo 1.8 bug: application of inlined procedure seems to be broken
;    (rdp-reduce c h (stream-tail s n))))
    (rdp-reduce c h (let loop ((s s) (n n))
		      (if (eqv? n 0)
			  s
			  (loop (tail s) (- n 1)))))))

;}}}
;{{{ (rdp-wrap before after f . fx)

(define (rdp-wrap before after f . fx)
  (lambda (c h s)
    (cons-stream
     before
     (f
      (append fx (rdp-push
		  (lambda (c h s)
		    (cons-stream after (rdp-reduce c h s)))
		  c))
      h s))))

;}}}
;{{{ (rdp-process grammar . variables)

(define (rdp-process grammar . variables)
  (lambda (c h s)
    (rdp-reduce c h (apply rdp-parse grammar s variables))))

;}}}
;{{{ (rdp-sgram generator)

(define (rdp-sgram generator)
  (lambda (c h s)
    (rdp-shift c h s (generator s))))


;}}}
;{{{ (rdp-repll repl f . fx)

(define (rdp-replace-tag token h repl cont)
  (cond
   ((pair? repl)
    (let ((what (car repl)))
      (cond
       ((procedure? what)
	(rdp-replace-tag
	 token h
	 (apply what `(,token . ,(map (lambda (sym) ((h 'get) sym))
				      (cdr repl))))
	 cont))
       ((eq? what 'quote)
	(let loop ((quoted (cdr repl)))
	  (if (null? quoted)
	      cont
	      (cons-stream (car quoted) (loop (cdr quoted))))))
       (else
	(let loop ((todo repl))
	 (if (null? todo)
	     cont
	     (rdp-replace-tag
	      token h (car todo) (loop (cdr todo)))))))))
   ((procedure? repl)			; (lambda (token))
    (rdp-replace-tag token h (repl token) cont))
   ((not repl) cont)
   (else (cons-stream repl cont))))

(define (rdp-repll repl f . fx)
  (lambda (c h s)
    (let* ((repl (if (procedure? repl) (repl (head s)) repl))
	   (trailer
	    (lambda (c h s)
	      (rdp-replace-tag
	       (head s) h (if (or (null? repl)  (null? (cdr repl)))
			      #f
			      (cadr repl))
	       (rdp-reduce c h (tail s))))))
      (rdp-replace-tag
       (head s) h (if (null? repl) #f (car repl))
       (f
	(if (null? fx)
	    (rdp-push trailer c)
	    (append fx (rdp-push trailer c)))
	h (tail s))))))

;}}}
