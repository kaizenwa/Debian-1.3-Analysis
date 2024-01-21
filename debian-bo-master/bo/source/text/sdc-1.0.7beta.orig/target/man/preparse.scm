(message 1 "Loading man")
(set! *load-path* (append *typeset-lib* *load-path*))
(set! sgml-opts (cons "-i Man " sgml-opts))
(set! sgml-subdir "sgml")

(load "include/normal.scm")
(load "include/manpage.scm")

(set! normalize-make-index-section #f)
(set! normalize-make-bib-section #t)

(define man-p-data
  `(#(DATA) ,(rdp-hmap
	      (lambda (t)
		(if (eq? (token-type t) 'DATA)
		    (let* ((s (data-token-data t))
			   (l (string-length s)))
		      (man-tr-string
		       (if (and (> l 2)
				(equal?
				 (substring s (- l 2) l)
				 "\\n"))
			   (substring s 0 (- l 2))
			   s)))
		    t)))))

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
		  ((eq? t 'CDATA) (man-tr-string (arg-val attrib)))
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

(define (f-sect-no token)
  (let ((no (reverse (xatv token 'NO))))
    ((case (car no)
       ((S F C P) ordinary-seq-format)
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
	     ((S F B C) ordinary-seq-format)
	     ((A) appdx-seq-format)
	     (else (message
		    0 (car no) " doesn't serve to identify a marking type.")
		   ordinary-seq-format))
	   (cdr no)))
	"??")))



;{{{ (man-tbl l-contents)

(define man-tbl-stuff (delay (begin
			       (load-silent "include/layout.scm")
			       (load-silent "include/tbl.scm")
			       (load-silent "target/man/tbl.scm"))))

; somewhat hacky to resemble the old behavior

(define (old-man-tbl l-contents)
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
	     ((SEP)     `#(SEP 0 ,(car (xatv token 'SEP)) 0)))))
       )
    (letrec
	((call-contents (rdp-cond* l-contents))
	 (pattern
	  (rdp-map1
	   (lambda (pat)
	     (let ((x (stream->list pat))
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
	     (let* ((x (stream->list r))
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
	     (force man-tbl-stuff)
	     (let ((x (stream->list tbl)))
	       (stream
		(tbl-preprocess-tbl
		 (vector
		  patterns
		  (reverse (cdr x)))))))
	   (rdp-repll `(,identity) r/p)))
	. ,l-contents))))

;}}}

(define man-repls1
  `((INDEX)
    (XREF      . ,(lambda (t)		; closure!!!
		    `(#f ,(ref-mark t))))
    (BREF      . ,(lambda (t)
		    `(#f
		      ("[" ,(ref-mark t) "]"))))
    ((MREF UREF)      ( " " ID " "))
    ;    (LABEL . #("<label>" "<label>"))

    (STRONG  "\\fB" "\\fP")
    (SQ      ",," "''" )
    (BF      "\\fB" "\\fP")
    (IT      "\\fI" "\\fP")
    (TT      "\\fB" "\\fP")
    (EM      ,(lambda(t) (if (odd? (xatv t 'LEVEL))
			     "\\fI"
			     "\\fB"))
	     "\\fP")
    (CODE    "\\fB" "\\fP")
    (VAR     "\\fI" "\\fP")
    (META    )
    (MATH    )
    (SET     )
    (SUB     )
    (SUP     )
    (LANG    )
    (NEWPAGE #"\n.bp")
    (NL      #"\n.br")
    (LABEL)
))

(define man-p-text
  (rdp-cond*
   `(,man-p-data
     (#((PI OUTPUT)) ,pass-token-action)
     (#(STARTTAG FOOTNOTE) ,(rdp-repll `(#"\n.RS\nFootnote: " #"\n.RE\n")
				       (rdp-call man-p-body)))
     . ,(def-repl
	  (rdp-call man-p-text)
	  `(,man-repls1)
	  `((#(ENDTAG) ,rdp-leave)
	    (#() ,(lambda (c h s)
		    (message 0 #"\nUnhandled: " (head s))
		    (rdp-reduce c h (tail s)))))))))

(define man-bib
  (letrec
      ((tr man-tr-string)
       (fmt1
	(lambda (data)
	  (let* ((no (vector-ref data 0))
		 (tag (vector-ref data 1))
		 (entry (vector-ref data 2)))
	    (if
	     (not (start-tag? entry))
	     `(#"\n.IP \"\\fB" ,no #"\"\n" ,tag #"\\fP")
	     (let ((auth (tr (xatv entry 'AUTHOR)))
		   (title (tr (xatv entry 'TITLE)))
		   (publ (tr (xatv entry 'PUBL)))
		   (year (xatv entry 'YEAR)))
	       `(#"\n.IP \"" ,no #"\"\n\\fI" ,auth "\\fP: "
		 ,title "; " ,year " \\fI" ,publ "\\fP"))))))
       (fmt-seq (lambda (dl)
		  `(,@(fmt1 (head dl))
		    . ,(if (null? (cdr dl))
			   '(#\newline)
			   (fmt-seq (cdr dl)))))))
    `(#(PLACE BIB)
      ,(lambda (c h s)
	 (let ((token (head s)))
	   (if (not (null? (token-args token)))
	       (cons-stream
		(fmt-seq (token-args token))
		(rdp-reduce c h (tail s)))
	       (rdp-reduce c h (tail s))))))))

(define man-g-body-one
  (old-man-tbl
   `(,man-p-data
     (#((PI OUTPUT)) ,pass-token-action)
     (#(STARTTAG PAR) ,(rdp-repll `((,(lambda (t todo) todo) PARSTART))
				  man-p-text))
     . ,
     (def-repl
       (rdp-call man-p-body)
       `(,man-repls1
	 ((SPLIT)
	  (LISTITEM ,(let ((mark (lambda (token)
				   (vector-ref '#("\\(bu" "--" "*")
					       (min (xatv token 'LEVEL) 2)))))
		       `(#"\n.IP \"" ,mark #"\"")))
	  (ENUMITEM
	   ,(let* ((LA-1 (- (char->integer #\a) 1))
		   (UA-1 (- (char->integer #\A) 1))
		   (conv (vector identity
				 (lambda (i) (integer->char (+ LA-1 i)))
				 (lambda (i) (number->roman i))
				 (lambda (i) (integer->char  (+ UA-1 i)))))
		   (mark (lambda (t)
			  ((vector-ref conv (xatv t 'LEVEL)) (xatv t 'NO)))))
	      `(#"\n.IP \"" ,mark #"\"")))
	  (DT #"\n.IP \"" #"\"")
	  (DD)
	  
	  (TABLE)
	  ((SLIDE INLINE))
	  (QUOTE    #"\n.RS\n.sp" #"\n.RE\n.sp")
	  (VERB     #"\n.nf\n\\fB" #"\\fP\n.fi")
	  (NOTE)
	  (FIGURE   #"\n.RS\n" #"\n.RE")
	  (CAPTION  #"\n" #"\n.br")
	  
	  ))
       `((#(ENDTAG))
	 ,man-bib
	 (#() ,(lambda (c h s)
		 (message 0 #"\nManBody Unhandled: " (head s))
		 (rdp-reduce c h (tail s)))))))))

(define man-g-body
  (let ((empty (lambda (x) #"\n"))
	(indented-par (lambda (x) #"\n.IP\n"))
	(s-indent `(,(lambda (t l)
		       (if (eqv? (head l) 1)
			 #f
			 #"\n.RS"))
		    INDENTLEVEL))
	(e-indent `(,(lambda (t l)
		       (if (eqv? (head l) 1)
			   #f
			   #"\n.RE"))
		    INDENTLEVEL)))
    `((#(STARTTAG BODY)
       ,(rdp-repll '()
		   (rdp-let
		    `((PARSTART ,empty ,identity))
		    (rdp-call (rdp-cond `(,@man-g-body ,@man-g-body-one))))
		   (rdp-call man-p-body)))
      (#(STARTTAG (LIST ENUM DESC))
       ,(rdp-let
	 `((INDENTLEVEL ,tail ,identity)
	   (PARSTART ,indented-par ,identity))
	 (rdp-repll `(,s-indent ,e-indent)
		   (rdp-call man-p-body)))))))

(define man-p-body
  (rdp-cond* `(,@man-g-body ,@man-g-body-one)))

(define man-divs
  `(((SECT2 SECTN SECT1 SECT APPDX APPDX1 APPDX2 APPDXN))
    ((H2 H3 AH2 AH3 H4 AH4) #"\n.SS \"" #"\"")
    ((H1 H0 AH1)            #"\n.SH \"" #"\"")
    ((SECTS SECT1S SECT2S SECTNS CHAPTS APPDX1S APPDX2S APPDXS APPDXNS))
    (CHAPT #"\n.bp")
    (DTTL)
    (RTTL)
    (BTTL)
    (ABSTRACT)
    (INTRO)
    (PREFACE)
    ))

(define man-p-sections
  (rdp-cond*
   `(,man-p-data
     (#((PI OUTPUT)) ,pass-token-action)
     ,@man-g-body
     . ,(def-repl (rdp-call man-p-sections)
	  `(,man-divs ,man-repls1)
	  `((#(ENDTAG))
	    (#((INDEXDB BIBDB)) ,(rdp-skip 1))
	    (#() ,(lambda (c h s)
		    (message 0 #"\nManSect unhandled: " (head s)))))))))

(define man-general-header
".\\\"Warning: don't edit this file. It has been generated by typeset
.\\\" The next compilation will silently overwrite all changes.")

(define (man-switch s)
  (let ((section (man-section 'unknown))
	(date "unknown")
	(inst "unknown"))
    (rdp-parse
     (rdp-cond
      `((#(STARTTAG MANPAGE)
	 ,(rdp-repll
	   (lambda (t)			; funny: enforce eval order
					; with knoledge of rdp internals
	     (set! section (man-section (xatv t 'sect)))
	     (set! date (xatv t 'date))
	     (set! inst (xatv t 'inst))
	     `((quote ,man-general-header #"\n.TH \"") #"\n"))
	   (rdp-cond
	    `((#(STARTTAG TITLE)
	       ,(rdp-map
		 (lambda (title short)
		  (let* ((name (stream-map (lambda (s)
					     (if (eq? (token-type s) 'DATA)
						 (man-tr-string
						  (string-upcase
						   (data-token-data s)))
						 s))
					   title)))
		    (stream-append
		     name
		     (stream "\" " (car section) " \"" date "\" \"" inst
			     "\" \"" (cadr section) #"\"\n.SH NAME\n ")
		     name short)))
		 (rdp-repll '() man-p-text)
		 (rdp-cond
		  `((#(STARTTAG SHORT)
		     ,(rdp-repll '(" \\- " #\newline) man-p-text))))))
	      (#() ,(rdp-map (lambda ()
			       (error "man-process" "title required" ""))))
	      ))
	   man-p-sections))
	(#(STARTTAG (DOCUMENT REPORT BOOK))
	 ,(rdp-repll
	   `(,man-general-header)
	   (rdp-cond
	    `((#(STARTTAG (DTTL RTTL BTTL))
	       ,(rdp-repll '(#"\n.TH \"" #"\"\n.PP") man-p-text))))
	   man-p-sections))))
     s
     `(PARSTART #"\n.PP\n")
     `(INDENTLEVEL ,(ints 0)))))

(set! compile-function
      (lambda (o d e)
	(doc-preprocess-hook 'run)
	((stream-display-diverted)
	 (cons-stream
	  `#(DIVERT
	     ,(if (equal? d "-")
		  'POP
		  (string-append d e)))
	  (stream-through
	   (token-stream o)
	   man-switch
	   (lambda (s)
	     (if (and (eq? (token-type (head s)) 'STARTTAG)
		      (eq? (token-gi (head s)) 'MANPAGE))
		 s
		 (stream->list s)))
	   all-doctypes)))))
