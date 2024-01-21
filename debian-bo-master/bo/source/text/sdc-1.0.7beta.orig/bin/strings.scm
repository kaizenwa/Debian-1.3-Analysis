(module strings
	(export
	 (string-find-string str substr . optional-start)
	 (string-find-laststring str substr . optional-start)
	 (string-find-char str chr . optional-start)
	 (string-find-lastchar str chr . optional-start)
	 (string-replw str width)
	 (string-left s1 width . s2)
	 (string-tr-all str from to)
	 (string-split-string str sub . start)
	 (string-split-whitespace str)
	 (strings-join strings gap)
	 (string-split-lines str width)
	 fill-column
	 fill-prefix
	 (fill-split-lines str)
	 ))

; String Handling

(define (string-find-string str substr . optional-start)
  (let* ((start (if (pair? optional-start) (car optional-start) 0))
	 (len-substr (string-length substr))
	 (len-str (string-length str))
	 (max (- len-str len-substr)))
    (let loop ((left start))
      (cond ((> left max) #f)
	    ((string=? (substring str left (+ left len-substr)) substr) left)
	    (else (loop (+ 1 left)))))))

(define (string-find-laststring str substr . optional-start)
  (let* ((len-substr (string-length substr))
	 (len-str (string-length str))
	 (start (- len-str
		   (if (pair? optional-start)
		       (car optional-start) 0)
		   len-substr)))
    (let loop ((left start))
      (cond ((< left 0) #f)
	    ((string=? (substring str left (+ left len-substr)) substr) left)
	    (else (loop (- left 1)))))))

(define (string-find-char str chr . start-pos)
  (let ((len (string-length str)))
    (let find ((pos (if (pair? start-pos) (car start-pos) 0)))
      (cond ((>= pos len) #f)
	    ((char=? (string-ref str pos) chr) pos)
	    (else (find (+ 1 pos)))))))

(define (string-find-lastchar str chr . start-pos)
  (let find ((pos (- (if (pair? start-pos) (car start-pos) 
			 (string-length str)) 1)))
      (cond ((< pos 0) #f)
	    ((char=? (string-ref str pos) chr) pos)
	    (else (find (- pos 1))))))

;;;+f
;;; Geneate a string which is `width' characters long consisting on the
;;; given string `str'.  For example :-
;;;   (string-replw "abc" 10) == "abcabcabca"
;;;   (string-replw "abc" 1)  == "a"
;;;   (string-replw "abc" 0)  == ""
;;;   (string-replw ""    1)  == ""
;;;-f
(define (string-replw str width)
  (if (string=? str "")
      ""
      (let ((str-len (string-length str)))
	(let loop ((result "") (size 0))
	  (cond ((= size width) result)
		((> size width) (substring result 0 width))
		(else (loop (string-append result str) (+ size str-len))))))))

;;;+f
;;; Produce a string of size `width' in which the string `s1' is positioned
;;; at the left and `s2' is used to pad out the remaining characters to
;;; the right.  For example :-
;;;   (string-left "Detroit" 10 "+") == "Detroit+++"
;;;   (string-left "Detroit" 6)      == "Detroi"
;;; Based on the Icon function left(s1, i, s2)
;;;-f
(define (string-left s1 width . s2)
  (let ((padding (if (pair? s2) (car s2) " "))
	(str-len (string-length s1)))
    (cond ((> width str-len)
	   (string-append s1 (string-replw padding (- width str-len))))
	  ((< width str-len) (substring s1 0 width))
	  (else s1))))
;;;
;;; Translate all characters in string `str' which appear in string
;;; `from' into the character at the same position within string `to'.
;;; This is simmilar to what the `tr' program does.
;;; `from' and `to' are required to have equal length, tough
;;; resricting what `tr' does.
;;;
(define (string-tr-all str from to)
  (let ((from-len (string-length from))
	(len (string-length str)))
    (if (not (eqv? from-len (string-length to)))
	(error 'string-tr-all
	       "Strings `from' and `to' have different length."
	       (cons from to)))
    (do ((res (make-string len))
	 (i 0 (+ i 1)))
	((= i len) res)
      (string-set! res i
		   (let* ((c (string-ref str i))
			  (tr (string-find-char from c)))
		     (if tr (string-ref to tr) c))))))

(define (string-split-string str sub . start)
  (letrec ((stl (string-length str))
	   (sul (string-length sub))
	   (splsu (lambda (s) 
		    (let ((end (string-find-string str sub s)))
		      (cond
		       ((not end) (list (substring str s stl)))
		       ((= s end) (splsu (+ end sul)))
		       ((= stl (+ end sul)) (list (substring str s end)))
		       (else (cons (substring str s end) 
				   (splsu (+ end sul)))))))))
    (splsu (if (pair? start) (car start) 0))))


;;; The Scheme below is a loose translation of the following Python code
;;; by Guido van Rossum, CWI Amsterdam <guido@cwi.nl>
;;;
;;; # Split a string into a list of space/tab-separated words
;;; # NB: split(s) is NOT the same as splitfields(s, ' ')!
;;; def split(s):
;;;	res = []
;;;	i, n = 0, len(s)
;;;	while i < n:
;;;		while i < n and s[i] in whitespace: i = i+1
;;;		if i = n: break
;;;		j = i
;;;		while j < n and s[j] not in whitespace: j = j+1
;;;		res.append(s[i:j])
;;;		i = j
;;;	return res
;;;+f
;;; Returns a list of whitespace delimited words in the string `str'.
;;; If the string is empty or contains only whitespace, then
;;; it returns the empty list.
;;;-f
(define (string-split-whitespace str)
  (define (skip-whitespace str pos)
    (cond ((zero? pos) pos)
	  ((char-whitespace? (string-ref str pos))
	   (skip-whitespace str (- pos 1)))
	  (else pos)))
  (define (skip-non-whitespace str pos)
    (cond ((zero? pos)
	   (if (char-whitespace? (string-ref str pos))
	       (+ 1 pos)
	       pos))
	  ((char-whitespace? (string-ref str pos)) (+ 1 pos))
	  (else (skip-non-whitespace str (- pos 1)))))
      (define (string-split-tr str pos result)
    (let ((end (skip-whitespace str pos)))
      (if (zero? end)
	  result
	  (let* ((start (skip-non-whitespace str end))
		 (new-result (cons (substring str start (+ 1 end)) result)))
	    (if (zero? start)
		new-result
		(string-split-tr str (- start 1) new-result))))))
  (let ((result '())
    	(strlen (string-length str)))
    (if (zero? strlen)
	result
	(string-split-tr str (- strlen 1) result))))

; join list of strings by gap

(define (strings-join strings gap)
  (letrec ((ins (lambda (stuff)
		  (if (null? stuff)
		      '()
		      (cons gap (cons (car stuff) (ins (cdr stuff))))))))
    (if (pair? strings)
	(apply string-append (cdr (ins strings)))
	"")))

(define (string-split-lines str max)
  (let ((lines '())
	(end (string-length str)))
    (let loop ((start 0))
      (if (>= max (- end start))
	  (reverse! (cons (substring str start end) lines))
	  (let ((line-end (string-find-lastchar 
			   str #\space (+ start (min max (- end start))))))
	    (if (or (not line-end) (<= line-end start))
		(set! line-end (string-find-char str #\space start)))
	    (if (not line-end) (set! line-end end))
	    (set! lines (cons (substring str start line-end) lines))
	    (if (>= line-end end)
		(reverse! lines)
		(loop (+ line-end 1))))))))

; paragraph handling

(define fill-column 76)
(define fill-prefix "")

(define (fill-split-lines str)
  (string-split-lines str (- fill-column (string-length fill-prefix))))
