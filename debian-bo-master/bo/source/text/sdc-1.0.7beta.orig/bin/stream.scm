;{{{ module stream

(module stream
	(include "stream.sch")
	(export
	 ; (cons-stream hd tl)
	 (inline stream-empty? stream)
	 empty-stream
	 (stream . elements)
	 (inline head stream)
	 (inline tail stream)
	 (inline stream-tail stream n)
	 (accumulate combiner null stream)
	 (stream->list s)
	 (stream-map proc stream) 
	 (stream-for-each proc stream)
	 (stream-through stream . procs)
	 (stream-filter predicate stream)
	 (stream-append s1 . so)
	 (stream2-append s1 . so)
	 (stream-memq element stream)
	 (substream stream after)
	 (stream-insert s . elements)
	 (stream-cute pred action stream)
	 (stream-finde pred action stream)

	 (stream-dbg-watch msg stream)
	 )
)

;}}}
(define-inline (stream-empty? s) (null? s))

(define empty-stream '())

(define (stream . elements)
  (if (null? elements)
      empty-stream
      (cons-stream (car elements) (apply stream (cdr elements)))))

(define-inline (head stream) (car stream))


(define-inline (tail stream)
  (let ((v (cdr	stream)))
     (if (procedure? v)
	 (let ((nv (v)))
	   (set-cdr! stream nv)
	   nv)
	 v)))


(define-inline (stream-tail stream n)
  (if (eqv? n 0) stream (stream-tail (tail stream) (- n 1))))

(define (accumulate combiner null stream)
  (if (stream-empty? stream)
      null
      (combiner (head stream) (accumulate combiner null (tail stream)))))


(define (stream->list s)
  (let loop ((r s))
    (if (stream-empty? r)
	s
	(loop (tail r)))))


(define (stream-map proc stream)
  (if (stream-empty? stream)
      stream
      (cons-stream (proc (head stream)) (stream-map proc (tail stream)))))

(define (stream-for-each proc stream)
  (if (stream-empty? stream)
      empty-stream
      (begin
	(proc (head stream))
	(stream-for-each proc (tail stream)))))

(define (stream-through stream . procs)
  (if (null? procs)
      stream
      ((car procs) (apply stream-through stream (cdr procs)))))

(define (stream-cond-map start? end? proc stream)
  (letrec
      ((mi
	(lambda (inside start? end? proc stream)
	  (if stream
	      (let ((rs ((if inside end? start?) stream)))
		(if rs
		    (mi (not inside) start? end? proc rs)
		    (cons-stream
		     (if inside (proc (head stream)) (head stream))
		     (mi inside start? end? proc (tail stream)))))))))
    (mi #f start? end? proc stream)))

(define (stream-filter predicate stream)
  (cond
   ((stream-empty? stream) stream)
   ((predicate (head stream))
    (cons-stream (head stream) (stream-filter predicate (tail stream))))
   (else (stream-filter predicate (tail stream)))))

(define (stream-append s1 . so)
  (if (stream-empty? s1)
      (if (null? so) empty-stream (apply stream-append so))
      (cons-stream (head s1) (apply stream-append (tail s1) so))))

(define (stream2-append s1 . so)
  (if (stream-empty? s1)
      (if (null? so)
	  empty-stream
	  (apply stream2-append (force (car so)) (cdr so)))
      (cons-stream (head s1) (apply stream2-append (tail s1) so))))

(define (substream stream after)
  (cond
   ((not stream) #f)
   ((eq? stream after) #f)
   (else (cons-stream (head stream) (substream (tail stream) after)))))

(define (stream-insert stream . elements)
  (if (null? elements)
      stream
      (cons-stream (car elements)
		   (apply stream-insert stream (cdr elements)))))

(define (stream-finde pred action stream)
  (cond
   ((stream-empty? stream) empty-stream)
   ((pred (head stream)) (action stream))
   (else (stream-finde pred action (tail stream)))))

(define (stream-cute pred action stream) ; the name: pun intended
  (cond
   ((stream-empty? stream) empty-stream)
   ((pred (head stream)) (action stream))
   (else (cons-stream (head stream)
		      (stream-cute pred action (tail stream))))))

(define (stream-memq element stream)
  (cond
   ((not stream) #f)
   ((eq? element (head stream)) stream)
   (else (stream-memq element (tail stream)))))

;{{{ Debugging Aid

(define (stream-dbg-watch msg stream)
  (display msg (current-error-port))
  (if (stream-empty? stream)
      (begin
	(display #" it's EMPTY now." (current-error-port))
	empty-stream)
   (begin
     (write (head stream) (current-error-port))
     (cons-stream (head stream) (stream-dbg-watch msg (tail stream))))))

;}}}
