
(define-macro (cons-stream hd tl)
  `(cons ,hd (lambda () ,tl)))

(define-macro (tail-macro stream)
  `(let ((v (cdr ,stream)))
     (if (procedure? v)
	 (let ((nv (v)))
	   (set-cdr! ,stream nv)
	   nv)
	 v)))

(define-macro (stream->list-macro s)
  `(let loop ((r ,s))
     (if (stream-empty? r)
	 ,s
	 (loop (tail r)))))
