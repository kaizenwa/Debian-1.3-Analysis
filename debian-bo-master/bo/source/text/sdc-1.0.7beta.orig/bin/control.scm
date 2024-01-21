;{{{ module CONTROL

(module control
	(export
	 null-proc
	 (inline identity x)
	 (make-hook)
	 (hook name cmd . rest)
	 (memoize f . ac)
	 (make-queue . initial)
	 (queue? queue)
	 (enqueue queue val . other)
	 (queue-value queue)
	 (queue-head queue)
	 (enqueue! queue . values)
	 (make-stack . args)
	 (stack-empty? stack)
	 (stack-set! stack value)
	 (stack->list stack)
	 (push stack val)
	 (pop stack)
	 (tos stack)
	 (append-to list stop rest)
	 (remove-all objs list)
	 (rm1 obj list)
	 (filter predicate list)
	 (list-prepend gap lst . other )
	 (list-join gap lst . other)
	 (list-flat-once! lst)
	 (make-counter . args)
	 (make-index . from)
	 (number->roman i)
	 )
	(import
	 (qsort "qsort.scm")))

;}}}

;{{{ Identity null-proc

(define null-proc list)

(define-inline (identity x) x)

;}}}

;{{{ (make-hook)

(define (make-hook)
  (letrec
      ((hook '())
       (doc #f)
       (run (lambda (hook . args)
	      (cond
	       ((procedure? hook) (apply hook args))
	       ((pair? hook)
		(let ((new-args (apply run (car hook) args)))
		  (apply run (cdr hook) new-args)))
	       (else args)))))
    (lambda (cmd . rest)
      (case cmd
       ((run) (apply run hook rest))
       ((add) (set! hook (cons (car rest) hook)))
       ((rm) (set! hook (rm1 (car rest) hook)))
       ((append) (set! hook (append hook (list (car rest)))))
       ((doc) doc)
       ((set-doc!) (if (and (not doc) (pair? rest))
		       (set! doc (if (null? (cdr rest)) (car rest) rest))))
       ))))
		       
(define named-hooks
  (make-hash-table 256 obj->0..255 car eq?))

(define (hook name cmd . rest)
  (let* ((entry (get-hash name named-hooks))
	 (proc (if entry (cdr entry) (make-hook))))
    (if (not entry)
	(put-hash! `(,name . ,proc) named-hooks))
    (apply proc cmd rest)))

;}}}
;{{{ (memoize f . ac)

(define (memoize f . ac)
  (define (lookup pred arg where)
    (if (null? where)
	#f
	(if (pred arg (caar where))
	    (car where)
	    (lookup pred arg (cdr where)))))
  (define (insert where ac args val)
    (if (null? args)
	val
	(let ((sub (lookup (car ac) (car args) where)))
	  (if sub
	      (begin
		(set-cdr! sub (insert (cdr sub) (cdr ac) (cdr args) val))
		where)
	      `((,(car args) . ,(insert '() (cdr ac) (cdr args) val))
		. ,where)))))
  (let ((res (if (null? ac)
		 (list (f))
		 '())))
    (lambda args
      (let loop ((run-args args) (run-ac ac) (prev res))
	(if
	 (null? run-args)
	 (if (null? run-ac)
	     prev
	     (error f "(memoized) too few arguments for" run-ac))
	 (if
	  (null? run-ac)
	  (error f "(memoized) too much arguments" run-args)
	  (let ((possible-result (lookup (car run-ac) (car run-args) prev)))
	    (if possible-result
		(loop (cdr run-args) (cdr run-ac) (cdr possible-result))
		(let ((computed-result (apply f args)))
		  (set! res (insert res ac args computed-result))
		  computed-result)))))))))


;}}}
;{{{ Queue's

(define (make-queue . initial)
  (let ((front `(QUEUE . ,(apply list initial))))
    (do ((rear front (cdr rear)))
	((null? (cdr rear))
	 `#(QUEUE ,front ,rear)))))

(define (queue? queue)
  (and (vector? queue) (eq? (vector-ref queue 0) 'QUEUE)))

(define (enqueue queue val . other)
  (let ((p (list val)))
    (set-cdr! (vector-ref queue 2) p)
    (vector-set! queue 2 p))
  (if (null? other)
      #t
      (apply enqueue queue other)))

(define (enqueue! queue . what)
  (set-cdr! (vector-ref queue 2) what)
  (let loop ((rear (vector-ref queue 2)))
    (if (null? (cdr rear))
	(vector-set! queue 2 rear)
	(loop (cdr rear)))))

(define (queue-value queue)
  (if (queue? queue)
      (cdr (vector-ref queue 1))
      (error "queue-value" "not a queue" queue)))

(define (queue-head queue)
  (car (queue-value queue)))

;}}}
;{{{ Stack support

(define (make-stack . args)
   (vector args))

(define (stack-empty? stack)
  (not (pair? (vector-ref stack 0))))

(define (stack->list stack) (vector-ref stack 0))

(define (stack-set! stack lst) (vector-set! stack 0 lst))

(define (push stack value)
   (let ((ns (cons value (vector-ref stack 0))))
     (vector-set! stack 0 ns)
     ns))

(define (pop stack)
  (let* ((os (vector-ref stack 0))
	 (top (car os)))
    (vector-set! stack 0 (cdr os))
    top))

(define (tos stack)
  (if (stack-empty? stack)
      #f
      (car (vector-ref stack 0))))

;}}}
;{{{ ********** List operations **********

;;; prepend gap to each element of list and each of rest
(define (list-prepend gap list . rest)
  (if (pair? list)
      (cons gap
	    (cons (car list)
		  (append (map car rest)
			  (apply list-prepend gap (cdr list) (map cdr rest)))))
      '()))

;;; Join lst by gap. Rest is as each of lst is list of same pos. of rest
(define (list-join gap lst . rest)
  (if (null? lst)
      '()
      (append (cons (car lst) (map car rest))
	      (apply list-prepend gap (cdr lst) (map cdr rest)))))
  

;;; If list has sublists put them in flat, but only for a nesting of 1
(define (list-flat-once! l)
  (cond
   ((null? l) l)
   ((list? (car l)) (append (car l) (list-flat-once! (cdr l))))
   (else   (cons (car l) (list-flat-once! (cdr l))))))

(define (append-to list stop rest)
  (cond
   ((null? list) '())
   ((eq? list stop) rest)
   (else (cons (car list) (append-to (cdr list) stop rest)))))

(define (remove-all objs list)
  (if (null? objs)
      list
      (let ((split (let loop ((l list))
		     (cond
		      ((null? l) #f)
		      ((memq (car l) objs) l)
		      (else (loop (cdr l)))))))
	(if split
	    (append-to list split (remove-all objs (cdr split)))
	    list))))

(define (rm1 obj list)
  (cond
   ((null? list) '())
   ((eq? obj (car list)) (cdr list))
   (else (let ((rest (rm1 obj (cdr list))))
	   (if (eq? rest (cdr list))
	       list
	       (cons (car list) rest))))))

(define (filter predicate list)
  (cond
   ((null? list) '())
   ((predicate (car list)) (cons (car list) (filter predicate (cdr list))))
   (else (filter predicate (cdr list)))))
;}}}
;{{{ (number->roman no)

(define (number->roman no)
  (apply
   string 
   (reverse!
    (let loop ((n no)
	       (r '()))
      (cond
       ((< n 0) (loop (- n) (cons #\- r)))
       ((>= n 1000) (loop (- n 1000) (cons #\M r)))
       ((>= n 900) (loop (- n 900) (cons #\M (cons #\C r))))
       ((>= n 500) (loop (- n 500) (cons #\D r)))
       ((>= n 400) (loop (- n 400) (cons #\D (cons #\C r))))
       ((>= n 100) (loop (- n 100) (cons #\C r)))
       ((>= n 90) (loop (- n 90) (cons #\C (cons #\X r))))
       ((>= n 50) (loop (- n 50) (cons #\L r)))
       ((>= n 40) (loop (- n 40) (cons #\L (cons #\X r))))
       ((>= n 10) (loop (- n 10) (cons #\X r)))
       ((>= n 9) (loop (- n 9) (cons #\X (cons #\I r))))
       ((>= n 5) (loop (- n 5) (cons #\V r)))
       ((= n 4) (cons #\V (cons #\I r)))
       ((>= n 1) (loop (- n 1) (cons #\I r)))
       (else r))))))

;}}}
;{{{ (make-counter . args)

(define (make-counter . args)
  (let ((obj
	 (let ((val 0)
	       (type 'decimal))
	   (lambda ( . args)
	     (let ((rt 'string))
	       (for-each 
		(lambda (a)
		  (cond
		   ((number? a) (set! val a))
		   ((eq? a 'number) (set! rt 'number))
		   ((eq? a '++) (set! val (+ val 1)))
		   ((eq? a '--) (set! val (- val 1)))
		   ((eq? a 'roman) (set! type 'roman))
		   ((eq? a 'decimal) (set! type 'decimal))
		   ((eq? a 'lchar) (set! type 'lchar))
		   ((eq? a 'uchar) (set! type 'uchar))))
		args)
	       (cond
		((eq? rt 'string) 
		 (cond
		  ((eq? type 'lchar) 
		   (string (integer->char (+ val (char->integer #\`)))))
		  ((eq? type 'uchar) 
		   (string (integer->char (+ val (char->integer #\@)))))
		  ((eq? type 'roman) (number->roman val))
		  (else (number->string val))))
		(else val)))))))
    (apply obj args)
    obj))

;}}}
;{{{ (make-index . from)

(define (make-index . from)

; structure of one index entry:
; ( id . data )
; data: #[ reflist  subidxlist ]

  (letrec
      ((idx (if (pair? from) (car from) '()))
       (add (lambda (idx ref id . subid)
	      (let ((el (assoc id idx)))
		    
		(if (not el)
		    (begin
		      (set! el (cons id  (vector '() '())))
		      (set! idx  (cons el idx))))

		(if (pair? subid)
		    (vector-set! (cdr el) 1 
				 (apply add (vector-ref (cdr el) 1) ref subid))
		    (vector-set! (cdr el) 0
				 (cons ref (vector-ref (cdr el) 0)))))
	      idx))
       (add! (lambda (ref id . subid)
	       (set! idx (apply add idx ref id subid))
	       idx))

       (srt (lambda (fn)
	      (letrec ((isrt (lambda (idx fn)
			       (set! idx (sort idx fn))
			       (for-each
				(lambda (i)
				  (vector-set!
				   (cdr i) 1
				   (isrt (vector-ref (cdr i) 1) fn)))
				idx)
			       idx))
		       (f (lambda (f)
			    (lambda (a b) (f (car a) (car b))))))
		(set! idx (isrt idx (f fn)))
		idx)))
       (walk (lambda (fn . fn-node)
	       (letrec ((wi (lambda (idx ids)
			      (for-each
			       (lambda (i)
				 (letrec ((idn (cons (car i) ids))
					  (cont
					   (lambda ()
					     (wi (vector-ref (cdr i) 1) idn))))
				   (fn idn (vector-ref (cdr i) 0))
				   (if (and (pair? fn-node)
					    (pair? (vector-ref (cdr i) 1)))
				       ((car fn-node) idn cont)
				       (cont))))
			       idx))))
		 (wi idx '()))
	       idx)))
    (lambda (cmd)
      (cond
       ((eq? cmd 'add) add!)
       ((eq? cmd 'value) idx)
       ((eq? cmd 'sort) srt)
       ((eq? cmd 'walk) walk)
       (else (display "index unknown call" (current-error-port)))))))

;}}}
