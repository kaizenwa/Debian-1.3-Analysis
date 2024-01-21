;;; -*-Scheme-*-
;;;
;;; Quicksort (straight from Wirth, Algorithmen & Datenstrukturen, p. 117)

(module qsort
	(export
	 (sort obj pred)))

(define (sort obj pred)
  (if (vector? obj)
      (sort! obj pred)
      (vector->list (sort! (list->vector obj) pred))))

(define (sort! v pred)
  (define (internal-sort l r)
    (let ((i l) (j r) (x (vector-ref v (quotient (- (+ l r) 1) 2))))
      (let loop ()
	(do () ((not (pred (vector-ref v i) x))) (set! i (+ i 1)))
	(do () ((not (pred x (vector-ref v j)))) (set! j (- j 1)))
	(if (<= i j)
	    (let ((h (vector-ref v j)))
	      (vector-set! v j (vector-ref v i))
	      (vector-set! v i h)
	      (set! i (+ i 1))
	      (set! j (- j 1))))
	(if (< i j)
	    (loop)))
      (if (< l j)
	  (internal-sort l j))
      (if (< i r)
	  (internal-sort i r))))
  (let ((len (vector-length v)))
    (if (> len 1)
	(internal-sort 0 (- len 1)))
    v))
