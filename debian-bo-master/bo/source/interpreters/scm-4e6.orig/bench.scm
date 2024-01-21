
(require (in-vicinity (implementation-vicinity) "pi.scm"))
(require 'transcript)
(define isqrt
  (cond ((provided? 'inexact) sqrt)
	(else (require 'root) integer-sqrt)))
(define i/
  (cond ((provided? 'inexact) /)
	(else quotient)))
(define around
  (cond ((provided? 'inexact)
	 (lambda (x)
	   (cond ((>= 3000 (abs x) 3) (inexact->exact (round x)))
		 (else x))))
	(else identity)))

(define (time-pi digits)
  (let ((start-time (get-internal-run-time)))
    (pi digits 4)
    (i/ (* 1000 (- (get-internal-run-time) start-time))
       internal-time-units-per-second)))

(define (benchmark . arg)
  (define file
    (cond ((null? arg) "bench.log")
	  (else (car arg))))
  (do ((digits 50 (+ digits digits))
       (t 0 (time-pi (+ digits digits))))
      ((> t 3000)
       (do ((tl '() (cons (time-pi digits) tl))
	    (j 12 (+ -1 j)))
	   ((zero? j)
	    (let* ((avg (i/ (apply + tl) (length tl)))
		   (dev (isqrt (i/ (apply
				    + (map (lambda (x) (* (- x avg) (- x avg)))
						 tl))
				   (length tl)))))
	      (and file (transcript-on file))
	      (for-each display
			(list digits " digits took " (around avg) " mSec +/- "
			      (around dev) " mSec."))
	      (newline)
	      (let ((scaled-avg (i/ (* (i/ (* avg 1000) digits) 1000) digits)))
		(for-each display
			  (list " That is about " scaled-avg
				" mSec/k-digit^2 +/- "
				(around
				 (i/ (* 100 (i/ (* (i/ (* dev 1000) digits)
						   1000) digits))
				     scaled-avg))
				"%."))
		(newline)
		(and file (transcript-off)))
	      ))))))
(benchmark)
