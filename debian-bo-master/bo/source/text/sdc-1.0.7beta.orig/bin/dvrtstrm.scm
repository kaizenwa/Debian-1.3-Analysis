;{{{ module declaration

(module divert-stream
	(include "stream.sch")
	(export
	 (stream-display-diverted)
	 )
	(import
	 (compile "compile.scm")
	 (stream "stream.scm")
	 ))

;}}}

;{{{ new output scheme

(define (display-on-port port) (lambda (what) (display what port)))

;;; display-token: spit the final stream out
(define-macro (display-token t port)
  `(if (memq (token-type ,t) '(OUTPUT PI))
       (let ((data (data-token-data ,t)))
	 (cond
	  ((pair? data) (for-each-and-every (display-on-port ,port) data))
	  ((procedure? data) (display (force data) ,port))
	  (else (display data ,port))))))

(define (for-each-and-every fn l)
  (cond
   ((pair? l) (for-each-and-every fn (car l))
	      (for-each-and-every fn (cdr l)))
   ((null? l) '())
   (else (fn l))))

(define (stream-display-diverted)
  (let* ((files '())
	 (current-port (current-output-port))
	 (file-stack `(,current-port)))
    (letrec
	((action
	  (lambda (s)
	    (if (stream-empty? s)
		(for-each (lambda (i)
			    (close-output-port (cdr i)))
			  files)
		(let ((t (head s)))
		  (if (eq? (token-type t) 'DIVERT)
		      (if (eq? 'POP (data-token-data t))
			  (if (not (null? (cdr file-stack)))
			      (begin
				(set! file-stack (cdr file-stack))
				(set! current-port (car file-stack))))
			  (let ((where (assoc (data-token-data t) files)))
			    (if (not where)
				(begin
				  (set! where `(,(data-token-data t)
						.  ,(open-output-file
						     (data-token-data t))))
				  (set! files `(,where . ,files))
				  ))
			    (set! current-port (cdr where))
			    (set! file-stack `(,current-port . ,file-stack))
			    ))
		      (display-token t current-port)
		      )
		  (action (tail s)))))))
      action)))

;}}}
