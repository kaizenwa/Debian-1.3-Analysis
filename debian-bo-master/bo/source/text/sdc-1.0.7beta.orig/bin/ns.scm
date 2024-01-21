(module namespace
	(export
	 (rassq el lst)
	 (ass cmp el lst)
	 (gen-ns id . next)
	 (ns-ensure-name name namespace)))

(define (rassq el lst)
  (cond
   ((null? lst) #f)
   ((eq? el (cdar lst)) (car lst))
   (else (rassq el (cdr lst)))))

(define (ass cmp el lst)
  (cond
   ((null? lst) #f)
   ((cmp el (car lst)) (car lst))
   (else (ass cmp el (cdr lst)))))

(define (gen-ns id . next)
  (letrec ((objs '())
	   (last #f)
	   (link-in (lambda (entry)
		      (if last
			  (let ((new (list entry)))
			    (set-cdr! last new)
			    (set! last new))
			  (let ((new (list entry)))
			    (set! objs new)
			    (set! last new)))
		      entry))
	   (names-of (lambda (o objs)
		       (if (null? objs)
			   (if next
			       (next 'names-of o)
			       '())
			   (if (eq? o (cdar objs))
			       (cons (caar objs) (names-of o (cdr objs)))
			       (names-of o (cdr objs)))))))
    (set! next (if (null? next) #f (car next)))
    (lambda (cmd . rest)
      (case cmd
	((resolve) (apply (lambda (name)
			    (let ((entry (assq name objs)))
			      (if entry entry
				  (if next (next 'resolve name)
				      #f)))) rest))
	((lookup)  (apply (lambda (name)
			    (let ((entry (assoc name objs)))
			      (if entry entry
				  (if next (next 'lookup name)
				      #f)))) rest))
	((bound)   (apply (lambda (name)
			    (let ((entry (assoc name objs)))
			      (if entry (car entry)
				  (if next (next 'bound name)
				      #f)))) rest))
	((bind)  (apply (lambda (obj name)
			  (let ((entry (assoc name objs)))
			    (if entry
				(error id "bind: name already used" name)
				(link-in (cons name obj))))) rest))
	((bind!) (apply (lambda (obj name)
			  (let ((entry (assoc name objs)))
			    (if entry
				(begin
				  (set-cdr! entry obj)
				  entry)
				(link-in (cons name obj))))) rest))
	((list) objs)
	((names-of) (names-of (car rest) objs))
	((ass) (apply (lambda (cmp obj)
			(let ((entry (ass cmp obj objs)))
			  (if entry entry
			      (if next (next 'ass cmp obj)
				  #f)))) rest))
	(else (error id "ns: unknown request" cmd))))))

(define (ns-ensure-name name ns)
  (let ((nn (ns 'bound name)))
    (if (not nn)
	(error "ns-ensure-name" "name not bound" name)
	nn)))

