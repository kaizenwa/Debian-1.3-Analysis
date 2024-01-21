(set! sgml-opts (cons "-i Info " sgml-opts))

;===========================================================================

(message 1 "Loading info")
(set! *load-path* (append *typeset-lib* *load-path*))
(set! sgml-opts (cons "-i Info " sgml-opts))
(set! sgml-subdir "sgml")

;{{{ load "normal.scm"

(hook 'rdpl 'add (lambda f (list rdpl-accu)))

(load "include/normal.scm")

(set! normalize-make-index-section #t)
(set! normalize-make-bib-section #t)

;}}}

(load "include/layout.scm")

;{{{ SETM

(define info-space-string
  (memoize
   (lambda (length) (make-string length #\space))
   eqv?))

(define (info-break-before n str)
  (define (next-break p max)
    (if (or (eqv? p max)
	    (eqv? (string-ref str p) #\space)
	    (eqv? (string-ref str p) #\newline))
	p
	(next-break (+ p 1) max)))
  (let* ((max (string-length str))
	 (fb (next-break 0 max)))
    (let loop ((lb fb) (b fb))
      (cond
       ((> b n) lb)
       ((= b n) b)
       ((= b max) b)
       ((eqv? (string-ref str b) #\newline) b)
       (else (loop b (next-break (+ b 1) max)))))))
	  
(define (info-i-setm left right cx cy s)
  (if
   (stream-empty? s)
   s
   (let ((t (head s)))
     (case (token-type t)
       ((SETM)
	(case (token-gi t)
	  ((IH)  (info-i-setm `(,(+ (token-args t) (car left)) . ,left) right
			    cx cy
			    (tail s)))
	  ((/IL) (info-i-setm (cdr left) right
			    cx cy
			    (tail s)))
	  ((IR)  (info-i-setm left `(,(- (car right) (token-args t)) . ,right)
			    cx cy
			    (tail s)))
	  ((/IR) (info-i-setm left (cdr right)
			    cx cy
			    (tail s)))
	  ((tab-to)			; move to pos, maybe break line
	   (let ((pos (token-args t)))
	     (cons-stream
	      (if (> cx pos)
		  (string-left #"\n" (+ pos 1) " ")
		  (make-string pos #\space))
	      (info-i-setm left right pos cy (tail s)))))
	  (else (cons-stream
		 #\newline
		 (cons-stream
		  (info-space-string (car left))
		  (info-i-setm left right (car left) cy (tail s)))))))
       ((OUTPUT)
	(let loop ((x (let ((x (data-token-data t)))
			(cond
			 ((string? x) x)
			 ((char? x) (string x))
			 ((number? x) (number->string x))
			 (else x))))
		   (cx cx))
	  (let* ((xl (string-length x))
		 (ncx (+ cx xl)))
	    (if (< ncx (car right))
		(cons-stream x (info-i-setm left right
					  ncx cy (tail s)))
		(let* ((bp (info-break-before (- (car right) cx) x))
		       (ncx (+ cx bp)))
		  (if
		   (> ncx (car right))	; no fit?
		   (stream-insert
		    (if (= bp xl)
			(info-i-setm left right (+ (car left) bp) cy (tail s))
			(loop (substring x (+ bp 1) xl) (+ (car left) bp)))
		    #\newline (info-space-string (car left))
		    (substring x 0 bp))
		  (stream-insert
		   (if (= bp xl)
		       (info-i-setm left right (car left) cy (tail s))
		       (loop (substring x (+ bp 1) xl) (car left)))
		   (substring x 0 bp)
		   #\newline (info-space-string (car left)))
		  ))))))
       (else (cons-stream t (info-i-setm left right cx cy (tail s))))))))


(define (info-setm s)
  (info-i-setm '(1) '(76) 0 0 s))

;}}}

(define info-p-data
  `(#(DATA) ,(rdp-hmap (lambda (t) (info-tr-string (data-token-data t))))))

(define (info-break-verb t)
  (let loop 
      ((str (string-split-string
	     (plain-tr-string (data-token-data (head t)))
	     #"\n")))
    (if (null? (cdr str))
	(stream (car str))
	(cons-stream
	 (car str)
	 (cons-stream
	  '#(SETM NEWLINE)
	  (loop (cdr str)))))))

(define info-g-data-verb
  `(#(DATA) ,(rdp-map1 info-break-verb pass-token-action)))

;{{{ conv-for-repll

(define (conv-for-repll xxx)
  (letrec
      ((c1 (lambda (token x)
	     (if (symbol? x)
		 (xatv token x)
		 x)))
       (ce (lambda (token x)
	     (cond
	      ((symbol? x)
	       (let* ((attrib (xat token x))
		      (t (if attrib
			     (arg-type attrib)
			     (begin
			       (message 0 "Argument " x
					" not defined in " token)
			       #f))))
		 (cond
		  ((eq? t 'CDATA) (info-tr-string (arg-val attrib)))
		  ((not t) #f)
		  (else (arg-val attrib)))))
	      ((pair? x) (map (lambda (x) (ce token x)) x))
	      (else x)))))
    (cond
     ((and (pair? xxx) (not (procedure? (car xxx))))
      (lambda (token)
	 (ce token xxx)))
     (else xxx))))

(define (def-repl content lists trailer)
  (if (null? lists)
      trailer
      `(,@(map (lambda (i)
		 `(#(STARTTAG ,(car i))
		   ,(rdp-repll (conv-for-repll (cdr i)) content)))
	       (car lists))
	. ,(def-repl content (cdr lists) trailer))))

;}}}
;{{{ general

(define (f-sect-no token)
  (let ((no (reverse (xatv token 'NO))))
    ((case (car no)
       ((S C F) ordinary-seq-format)
       ((A) appdx-seq-format))
     (cdr no))))

(define (ref-mark token)
  (let* ((mark (xat token 'MARK))
	 (val (if (eq? (arg-type mark) 'PROMISE)
		  (force (arg-val mark))
		  (arg-val mark))))
    (if val
	(let ((no (reverse (cdr val))))
	  ((case (car no)
	     ((S F B C P) ordinary-seq-format)
	     ((A) appdx-seq-format)
	     (else (message
		    0 (car no) " doesn't serve to identify a marking type.")
		   ordinary-seq-format))
	   (cdr no)))
	"??")))

;}}}

(define info-general-header
"<!--
Warning: don't edit this file. It has been generated by typeset
The next compilation will silently overwrite all changes.
-->
")

(define info-nn-str  "

")

(define (info-write-node file up prev next what)
  (stream
   info-nn-str
   "File: " file
   " Node: " what
   ", Up: " up
   ", Previous: " prev
   ", Next: " next #"\n\n"))

;{{{ (info-tbl l-contents)

(define info-tbl-stuff (delay (begin
			       (load-silent "include/tbl.scm")
			       (load-silent "target/man/tbl.scm")
			       (load-silent "target/html/tbl.scm"))))

; somewhat hacky to resemble the old behavior

(define (old-info-tbl l-contents)
  (define (empty-cells n)
    (if (<= n 0) '() (cons "" (empty-cells (- n 1)))))
  (let*
      ((tr man-tr-string)
       (patterns '())
       (rows '())
       (cp #f)
       (highpat (let ((x (ints 0))) (lambda () (set! x (tail x)) (head x))))
       (atvn (lambda (t a) (string->number (car (xatv t a)))))
       (rdpl (lambda (f . fx)  (apply rdp-accu-list f fx)))
       (pattern-length (lambda (pat) (length (cdr pat))))
       (definition-tag
	 (lambda (token)
	   (case (token-gi token)
	     ((LEFT)    `#(CELL ,(atvn token 'SPAWN) LEFT AUTO))
	     ((RIGHT)   `#(CELL ,(atvn token 'SPAWN) RIGHT AUTO))
	     ((CENTER)  `#(CELL ,(atvn token 'SPAWN) CENTER AUTO))
	     ((BLOCK)   `#(CELL ,(atvn token 'SPAWN)
				,(STRING->symbol (car (xatv token 'ALIGN)))
				,(atvn token 'WIDTH)))
	     ((DECIMAL) `#(DECIMAL ,(atvn token 'SPAWN)
				   ,(xatv token 'ALIGN) AUTO))
	     ((SEP)     `#(SEP 0 ,(car (xatv token 'SEP)) 0))))))
    (letrec
	((call-contents
	  (rdp-call
	   (rdp-cond*
	    (if #f			; don't use markup for nroff
		l-contents
		`((#(STARTTAG)
		   ,(rdp-repll '() call-contents))
		  (#(DATA) ,pass-token-action)
		  (#(ENDTAG))
		  (#((PI OUTPUT)) ,pass-token-action))
		))))
	 (pattern
	  (rdp-map1
	   (lambda (pat)
	     (let ((x (accumulate cons '() pat))
		   (id (xat (head pat) 'ID)))
	       (set! cp (cons (if (eq? (arg-type id) 'IMPLIED)
				  (highpat)
				  (arg-val id))
			      (reverse! (cdr x))))
	       (set! patterns (cons cp patterns))
	       empty-stream))
	   (rdp-repll
	    `(,identity)		; keep the <pattern> tag
	    (rdp-cond*
	     `((#(STARTTAG (LEFT RIGHT CENTER BLOCK DECIMAL SEP))
		,(rdp-repll (list definition-tag #f) rdp-leave)
		,(rdp-cond
		  `((#(STARTTAG SEP)
		     ,(rdp-repll (list definition-tag #f) rdp-leave))
		    (#(ENDTAG PATTERN) ,rdp-leave)
		    (#() ,(rdp-insert '#(SEP 0 "NO" 0))))))
	       )))))
	 (row
	  (rdp-map1
	   (lambda (r)
	     (let* ((x (accumulate cons '() r))
		    (pat (xat (car x) 'PATTERN))
		    (pn (if (not (eq? (arg-type pat) 'IMPLIED))
			    (assoc (arg-val pat) patterns)
			    #f))
		    (new-cp (if pn pn cp))
		    (pl (+ 1 (quotient (pattern-length new-cp) 2))))
	       (set! cp new-cp)
	       (stream
		(vector cp
			`(,@(reverse! (map conv (cdr x)))
			  . ,(empty-cells (- pl (length (cdr x)))))))))
	   (rdp-repll
	    `(,identity)
	    (rdp-cond* `((#(STARTTAG C)
			  ,(rdpl (rdp-repll '() call-contents))))))))
	 (row-sep (lambda (token) (vector 'SEP (car (xatv token 'SEP)))))
	 (r/p (rdp-cond*
	       `((#(STARTTAG PATTERN) ,(rdp-call pattern))
		 (#(STARTTAG SEP) ,(rdp-repll `(,row-sep) rdp-leave))
		 (#(STARTTAG R) ,(rdp-call row)))))
	 (conv-cell (lambda (i)
		      (if (data-token? i)
			  (tr (data-token-data i))
			  i)))
	 (conv (lambda (cell) (apply string-append (map conv-cell cell))))
	 )
      `((#(STARTTAG TBL)
	 ,(rdp-map1
	   (lambda (tbl)
	    (force info-tbl-stuff)
	    (let ((x (accumulate cons '() tbl)))
	      (info-break-verb
	       (stream (tbl-nroff-tbl
			(vector
			 patterns
			 (reverse! (cdr x))))))))
	   (rdp-repll `(,identity) r/p)))
	. ,l-contents))))

;}}}
;{{{ text

(define info-g-text
  `((#(ENDTAG))
    ,info-p-data
    (#((PI OUTPUT)) ,pass-token-action)
    (#(STARTTAG FOOTNOTE) ,(rdp-repll '() (rdp-call info-p-body)))
    (#(STARTTAG SQ)
     ;{{{ depends on the current language

     ,(rdp-replc (lambda (token lang)
		   (if (member lang english-quotation-style)
		   '("``" "''")
		   '(",," "''")))
		 '(LANGUAGE)
		 (rdp-call info-p-text)))

    ;}}}
    .
    ,(def-repl
       (rdp-call info-p-text)
       `((
	  (INDEX)
	  (XREF      . ,(lambda (t)		; closure!!!
			  `(#f ,(ref-mark t))))
	  (BREF      . ,(lambda (t)
			  `(#f
			    ("[" ,(ref-mark t) "]"))))
	  ((MREF UREF)      ( " " ID " "))
	  (LABEL . ,(lambda (t)
		      (message 0
			       "Warning: <label> not yet implemented for info."
			       #"\ndropping `<label id=\""
			       (xatv t 'ID) "\">'.")
		      '(#f #f)))
	  
	  (STRONG  "*" "*")
	  (SQ      ",," "''" )
	  (SUB "_")
	  (SUP "^")
	  (SET "_" "_")
	  ((BF IT TT EM CODE VAR META MATH LANG))
	  (NEWPAGE "")

	  ))
       `()
       )))

(define info-p-text
  (rdp-cond* `(,@info-g-text
	       (#() ,(lambda (c h s)
		       (message 0 #"\nInfoText unhandled: " (head s))
		       (rdp-reduce c h (tail s)))))))

;}}}
;{{{ bib

(define info-bib
  (letrec
      ((tr info-tr-string)
       (fmt1
	(lambda (data)
	  (let* ((no (vector-ref data 0))
		 (tag (vector-ref data 1))
		 (entry (vector-ref data 2)))
	    (if
	     (not (start-tag? entry))
	     `(#(SETM NEWLINE) ,no #"	" ,tag #"\n")
	     (let ((auth (tr (xatv entry 'AUTHOR)))
		   (title (tr (xatv entry 'TITLE)))
		   (publ (tr (xatv entry 'PUBL)))
		   (year (car (xatv entry 'YEAR))))
	       `(#(SETM NEWLINE) #(SETM IH 3)
		 ,no #"	" ,auth " "
		 ,title "; " ,year " " ,publ ""
		 #(SETM /IL)))))))
       (fmt-seq (lambda (dl)
		  `(,@(fmt1 (head dl))
		    . ,(if (null? (cdr dl))
			   '()
			   (fmt-seq (cdr dl)))))))
    `(#(PLACE BIB)
      ,(rdp-map1
	(lambda (s)
	  (let ((token (head s)))
	    (if (null? (token-args token))
		empty-stream
		(apply
		 stream
		 `(#"\n" ,@(fmt-seq (token-args token)) #\newline)))
	    ))
	pass-token-action))))

;}}}
;{{{ index

(define info-index
  `(#(PLACE INDEX) ,(rdp-skip 1)))

;}}}
;{{{ body

(define info-p-body
  (rdp-repll `() (rdp-call (rdp-cond* info-g-body))))

(define info-g-body
  (old-info-tbl
   `(,info-p-data
     (#((PI OUTPUT)) ,pass-token-action)
     (#(ENDTAG))
     ,@info-g-text
     (#(STARTTAG VERB) ,(rdp-repll '() (rdp-cond `(,info-g-data-verb))))
     (#(STARTTAG BODY) ,info-p-body)
     . ,
     (def-repl
       (rdp-call (rdp-cond* info-g-body))
       `(((SPLIT (#"\n" #(SETM NEWLINE)))
	  (PAR)
	  (LISTITEM ,(let ((mark (lambda (token)
				   (vector-ref '#("o  " "-- " "*  ")
					       (min (xatv token 'LEVEL) 2)))))
		       `(#(SETM NEWLINE) ,mark #(SETM IH 3)))
		    #(SETM /IL))
	  (ENUMITEM
	   ,(let* ((LA-1 (- (char->integer #\a) 1))
		   (UA-1 (- (char->integer #\A) 1))
		   (conv (vector identity
				 (lambda (i) (integer->char (+ LA-1 i)))
				 (lambda (i) (number->roman i))
				 (lambda (i) (integer->char  (+ UA-1 i)))))
		   (mark (lambda (t)
			  ((vector-ref conv (xatv t 'LEVEL)) (xatv t 'NO)))))
	      `(#(SETM NEWLINE) ,mark "  " #(SETM IH 3)))
	   #(SETM /IL))
	  (DT #f #(SETM IH 3))
	  (DD #(SETM NEWLINE) (#(SETM /IL) #(SETM NEWLINE)))
	  (TABLE)
	  ((SLIDE INLINE))
	  (QUOTE (#(SETM IH 4) #(SETM IR 4) #(SETM NEWLINE))
		 (#(SETM /IL) #(SETM /IR)))
	  ((LIST ENUM DESC LABEL))
	  (NOTE)
	  (FIGURE)
	  (CAPTION  #(SETM NEWLINE) #(SETM NEWLINE))
	  (LITERATE)

	  ))
       `(,info-bib
	 ,info-index
	 (#(STARTSUBDOC) ,(rdp-skip 1))
	 (#() ,(lambda (c h s)
		 (message 0 #"\nInfoBody Unhandled: " (head s))
		 (rdp-reduce c h (tail s)))))))))

;}}}

(define (info-id-of-token t)
  (let ((id (xat t 'ID))
	(top "Top"))
    (cond
     ((not id) top)
     ((eq? (arg-type id) 'IMPLIED) (f-sect-no t))
     (else (arg-val id)))))

(define except-last
  (define (cutit s)
    (if (null? (cddr s))
	(set-cdr! s '())
	(cutit (cdr s))))
  (define (compute s)
    (if (or (null? s) (null? (cdr s)))
	s
	(begin
	  (cutit s)
	  s)))
  (memoize compute eq?))

(define (info-subsections section . appdx-f)
  (define (extract-them l)
    (if (null? l)
	l
	(except-last (cdr l))))
  (define (report s hd abstract body sections appdx . forget)
    (extract-them (if (null? appdx-f) sections appdx)))
  (define (document s hd body sections appdx se)
    (extract-them (if (null? appdx-f) sections appdx)))
  (define (book s hd pre intro sections appdx . se)
    (extract-them (if (null? appdx-f) sections appdx)))
  (define (division s hd body sections se)
    (extract-them sections))
  (apply
   (case (token-gi (car section))
     ((REPORT) report)
     ((DOCUMENT) document)
     ((BOOK) book)
     (else division))
   section))

(define (info-write-heading section)
  (define (doit s heading body . rest)
    (case (token-gi (car heading))
      (else (apply stream (except-last (cdr heading))))))
  (apply doit section))

(define (info-write-body section)
  (define (document s hd body . rest)
    (apply stream body))
  (define (report s hd abstract body . rest)
    (apply stream body))
  (info-setm
   (stream-insert
    (rdp-parse
     info-p-body
     (apply (case (token-gi (car section))
	      ((REPORT) report)
	      (else document))
	    section)
     `(LANGUAGE ,(xatv (car section) 'LANG)))
    #"\n" '#(SETM NEWLINE))))

;{{{ info-write-subnodes

(define info-top-node-id "Top")

; Within the following, the basic code idea to split into files
; is commented out. I'm not sure, whether this is still useful
; and it's going to complicate the whole thing.

(define (info-write-subnodes files up this prev next subsections)
  (define (next-id default lst)
    (if (null? lst)
	default
	(if (not (pair? (car lst)))
	    (next-id default (cdr lst))
	    (info-id-of-token (caar lst)))))
  (if (null? subsections)
      empty-stream
      (stream-append
       (stream #"\n\n* Menu:\n")
       (apply
	stream-append
	(map (lambda (i)
	       (if (not (pair? i))
		   empty-stream
		   (stream-append 
		    (stream
		     #"\n* "
		     (string-left
		      (info-id-of-token (car i)) 30
		      "::                            "))
		    (info-write-heading i))))
	     subsections))
       (let loop
	   ((files files)
	    (ss subsections)
	    (p prev))
	 (cond
	  ((null? ss) empty-stream)
	  ((not (pair? (car ss))) (loop files (cdr ss) p))
	  (else
	   (let*
	       ((sub (info-subsections (car ss)))
		(total-next (next-id next (cdr ss)))
		(n  (if (null? sub)
			total-next
			(info-id-of-token (caar sub))))
		(me (info-id-of-token (caar ss)))
		(files ;(if (eq? (token-gi (caar ss)) 'CHAPT)
		       ;  (tail files)
		       ;  files))
		 files)
		)
	   (stream2-append
	    ;(cons-stream
	    ; `#(DIVERT ,(head files))
	     (info-write-node (head files) this p n me)
	    ; )
	    (delay (info-write-heading (car ss)))
	    (delay (info-write-body (car ss)))
	    (delay (info-write-subnodes
		    files this me me total-next sub))
	    ;(delay (stream `#(DIVERT POP)))
	    (delay (loop files (cdr ss) me))))))))))

;}}}
;{{{ make a toplevel node

(define (info-switch filenames)
  (lambda (s)
    (stream2-append
     (stream info-general-header)
     (delay
       (info-write-node 
	(head filenames)
	"(DIR)Top" "(DIR)Top"
	(let ((c (info-subsections (head s))))
	  (if (null? c)
	      "(DIR)Top"
	      (info-id-of-token (caar c))))
	(info-id-of-token (car (head s)))))
     (delay (info-write-heading (head s)))
     (delay (info-write-body (head s)))
     (delay (info-write-subnodes
	     filenames
	     "(DIR)Top" info-top-node-id info-top-node-id "(DIR)Top"
	     `(,@(info-subsections (head s))
	       . ,(info-subsections (head s) 'get-appendix)))))))

;}}}

(define (info-prepare o d e)
  (doc-preprocess-hook 'run)
  ((stream-display-diverted)
   (stream-through
    (token-stream o)
    (lambda (s) (stream-map (lambda (t) (if (data-token? t)
					    (info-tr-string (data-token-data t))
					    t))
			    s))
    ;(info-switch (tail (subfiles-of (string-append d "-c") ".info")))
    ; ok, use a dummy for spliting
    (info-switch (letrec ((x (lambda (s) (cons-stream s (x s)))))
		   (x (string-append d ".info"))))
    all-doctypes
    )))

(define (info-compile-function o d e)
  (if (equal? d "-")
      (info-prepare o d e)
      (with-output-to-file
	  (string-append d e)
	(lambda ()
	  (info-prepare o d e)))))

(set! compile-function info-compile-function)
