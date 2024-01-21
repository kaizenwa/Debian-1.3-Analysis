(set! sgml-opts (cons "-i HTML " sgml-opts))

;{{{ old scheme stuff -- to be removed
(set! process-cdata html-tr-string)
(set! process-text html-process-text)
;;;-----

(define current-label "")
(define bib-label "")
(define current-o-file "")
(define bib-file "")
(define index-file "")

;===========================================================================

;}}}

(message 1 "Loading html")
(set! *load-path* (append *typeset-lib* *load-path*))
(set! sgml-opts (cons "-i HTML " sgml-opts))
(set! sgml-subdir "sgml")

(load "include/normal.scm")
(load "include/file-split.scm")
(load "include/manpage.scm")
(load "include/layout.scm")

(set! normalize-make-index-section #t)
(set! normalize-make-bib-section #t)

(define html-p-data
  `(#(DATA) ,(rdp-hmap (lambda (t) (html-tr-string (data-token-data t))))))

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
		  ((eq? t 'CDATA) (html-tr-string (arg-val attrib)))
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
;{{{ section numbering

(define (sect-no-from-id id)
  (let ((no (reverse id)))
    ((case (car no)
       ((S F C B F) ordinary-seq-format)
       ((A) appdx-seq-format)
       (else (message 0 'sect-no-from-id " "(car no)
		      " is not a valid format definition.")
	     ordinary-seq-format))
     (cdr no))))

(define (typed-sect-no-from-id id)
  (if
   (and (pair? id))
   (let ((no (reverse id)))
     (if
      (symbol? (car no))
      (string-append
       (symbol->string (car no))
       ((case (car no)
	  ((S F C B F) ordinary-seq-format)
	  ((A) appdx-seq-format)
	  (else (message 0 'typed-sect-no-from-id (car no)
			 " is not a valid format definition.")
		ordinary-seq-format))
	(cdr no)))
      (begin
	(message 0 'typed-sect-no-from-id " can't work with id " id)
	"ERROR")))
   (message 0 'typed-sect-no-from-id " can't work with id " id)))

(define (f-sect-no token) (sect-no-from-id (xatv token 'NO)))

(define (f-t-sect-no token) (typed-sect-no-from-id (xatv token 'NO)))

;}}}
;{{{ referencing functions

(define (ref-mark token)
  (let* ((mark (xat token 'MARK))
	 (val (if (eq? (arg-type mark) 'PROMISE)
		  (force (arg-val mark))
		  (arg-val mark))))
    (if val
	(sect-no-from-id (cdr val))
	"??")))

(define (href-mark token)
  (let* ((mark (xat token 'MARK))
	 (val (if (eq? (arg-type mark) 'PROMISE)
		  (force (arg-val mark))
		  (arg-val mark))))
    (if val
	`(("<A HREF=\"" ,(car val) #\#
	   ,(typed-sect-no-from-id (cdr val)) "\">")
	  (,(sect-no-from-id (cdr val)) "</A>"))
	'())))

;}}}
;{{{ (html-tbl l-contents)

(define html-tbl-stuff (delay (begin
			       (load-silent "include/tbl.scm")
			       (load-silent "target/man/tbl.scm")
			       (load-silent "target/html/tbl.scm"))))

; somewhat hacky to resemble the old behavior

(define (old-html-tbl l-contents)
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
	    (force html-tbl-stuff)
	    (let ((x (stream->list tbl)))
	      (stream
	       `#(OUTPUT
		  (#"\n<PRE>\n"
		   ,(tbl-nroff-tbl
		     (vector
		      patterns
		      (reverse! (cdr x))))
		   #"\n</PRE>")))))
	   (rdp-repll `(,identity) r/p)))
	. ,l-contents))))

;}}}
;{{{ Footnotes

(define html-footnote-list
  `(,(lambda (t fnl language)
       (if (not (null? (cdr fnl)))
	   (let ((footnote/s ((phrase language)
		   (if (null? (cddr fnl)) 'footnote 'footnotes))))
	     `(#"\n<HR>\n" ,footnote/s #"<DL>\n"
	       .
	       ,(let loop
		    ((fns (reverse!
			   (map (lambda (i)
				  (cons (car i)
					(accumulate cons '() (cdr i))))
				(cdr fnl))))
		     (n 1))
		  `(#"\n<DT><A NAME=\"FN"
		    ,(caar fns) "\">"
		    ,n ")</A></DT><DD>" ,(cdar fns) "</DD>"
		    . ,(if (null? (cdr fns))
			   (begin
			     (set-cdr! fnl '()) ; side effect
			     '(#"\n</DL><HR>\n"))
			   (loop (cdr fns) (+ n 1)))))))
	     #f))
       FOOTNOTES LANGUAGE))

;}}}
;{{{ text

(define html-g-text
  `((#(ENDTAG))
    ,html-p-data
    (#((PI OUTPUT)) ,pass-token-action)
    (#(STARTTAG FOOTNOTE)
     ;{{{ snarf and put it into the env.

     ,(rdp-repll
       '()
       (rdp-let `((FOOTNOTEID ,identity ,tail))
		(rdp-env-map
		 (lambda (c h)
		   (let* ((get (h 'get))
			  (fnl (get 'FOOTNOTES))
			  (id (head (get 'FOOTNOTEID))))
		     (set-cdr! fnl `((,id . ,c) . ,(cdr fnl)))
		     (stream "<A HREF=\"#FN" id
			     "\">(" (length (cdr fnl)) ")</A>")))
		 (rdp-call html-p-body)))))

    ;}}}
    (#(STARTTAG SQ)
     ;{{{ depends on the current language

     ,(rdp-repll (lambda (token)
		   (if (member (xatv token 'LANG) english-quotation-style)
		       '("``" "''")
		       '(",," "''")))
		 (rdp-call html-p-text)))

    ;}}}
    (#(STARTTAG UREF)
     ,(rdp-env-map
       (lambda (c env)
	 (stream-append
	  (stream "<A HREF=\"" (xatv (head c) 'ID) "\">")
	  (if (stream-empty? (tail c))
	      (stream ((phrase ((env 'get) 'LANGUAGE)) 'here))
	      (tail c))
	  (stream "</A>")))
       (rdp-repll `(,identity) (rdp-call html-p-text))))
    ; a somewhat crasy way because we have a different appearence
    ; if we have something in the xref or not
    (#(STARTTAG XREF)
     ,(rdp-map
       (lambda (c)
	 (let ((repl (href-mark (head c))))
	   (if (stream-empty? c)
	       (stream-append		; spit it out as is
		(apply stream (car repl))
		(apply stream (cdr repl)))
	       (stream-append
		(apply stream (car repl))
		c			; have the contents dumped
		(stream " (")		; and splice the number in ()'s
		(apply stream  `(,(caadr repl) ")" . ,(cdadr repl)))))))
       (rdp-repll `(,identity) (rdp-call html-p-text))))
    .
    ,(def-repl
       (rdp-call html-p-text)
       `((
	  (INDEX)
	  ;	  (XREF      . ,href-mark)
	  (BREF      . ,(lambda (t)
			  `(#f
			    ("[" ,(ref-mark t) "]"))))
	  (MREF      ( " " ID " ") )
	  ;    (LABEL . #("<label>" "<label>"))
	  
	  (STRONG  "<B><U>" "</U></B>")
;	  (SQ      ,@(if (member lang english-quotation-style) ; change style
;		   '("``" "''")
;		   '(",," "''")))
	  (BF      "<B>" "</B>")
	  (IT      "<I>" "</I>")
	  (TT      "<TT>" "</TT>")
	  (EM      . ,(lambda(t) (if (odd? (xatv t 'LEVEL))
				     '("<I>" "</I>")
				     '("<B>" "</B>"))))
	  (CODE    "<TT>" "</TT>")
	  (VAR     "<I>" "</I>")
	  (META    )
	  (MATH    )
	  (SET     "<U>" "</U>")
	  (SUB     "_")
	  (SUP     "^")
	  (LANG    )
	  (NEWPAGE "<HR>")
	  (NL      "<BR>")
	  (LABEL)
	  ))
       `()
       )))

(define html-p-text
  (rdp-cond* `(,@html-g-text
	       (#() ,(lambda (c h s)
		       (message 0 #"\nHtmlText unhandled: " (head s))
		       (rdp-reduce c h (tail s)))))))

;}}}
;{{{ INDEX

(define (html-make-index idx where)
  (if
   (> (length idx) 0)
   `(#"\n<UL>"
     ,@(map
	(lambda (i)
	  `(#"\n<LI>"
	    ,(html-tr-string (car i))	; the indexed text
	    ,@(list-join
	       "," (reverse! (map (lambda (i)
				    (let ((pos (cdr (where 'lookup i))))
				      `(" <A HREF=\""
					,(car pos)
					,@(let ((no (cdr pos)))
					    (if
					     (equal? no "")
					     '()
					     `("#"
					       ,(typed-sect-no-from-id
						 (cdr pos)))))
					"\">(o)</A>")))
				  (vector-ref (cdr i) 0))))
	    . ,(html-make-index (vector-ref (cdr i) 1) where)))
	idx)
     #"\n</UL>")
   ""))

(define html-index
  `(#(PLACE INDEX)
    ,(rdp-map1
      (lambda (s)
	(if (null? (car (token-args (head s))))
	    empty-stream
	    (apply
	     stream #"\n<P>"
	     (apply html-make-index (token-args (head s))))))
      pass-token-action)))

;}}}
;{{{ html-bib

(define html-bib
  (letrec
      ((tr html-tr-string)
       (fmt1
	(lambda (data)
	  (let* ((no (vector-ref data 0))
		 (tag (vector-ref data 1))
		 (entry (vector-ref data 2)))
	    (if
	     (not (start-tag? entry))
	     `(#"\n<DT><B>" ,no #"</B></DT>\n<DD>" ,tag #"\n</DD>\n")
	     (let ((auth (tr (xatv entry 'AUTHOR)))
		   (title (tr (xatv entry 'TITLE)))
		   (publ (tr (xatv entry 'PUBL)))
		   (year (xatv entry 'YEAR)))
	       `(#"\n<DT>" ,no #"<DT>\n<DD>\n<I>" ,auth "</I>"
		 ,title "; " ,year " <I>" ,publ "</I>\n</DD>"))))))
       (fmt-seq (lambda (dl)
		  `(,@(fmt1 (head dl))
		    . ,(if (null? (cdr dl))
			   '()
			   (fmt-seq (cdr dl)))))))
    `(#(PLACE BIB)
      ,(rdp-map1
	(lambda (s)
	  (let ((token (head s)))
	    (if (not (null? (token-args token)))
		(stream
		 `#(OUTPUT (#"\n<DL>"
			    ,@(fmt-seq (token-args token)) #"\n</DL>")))
		empty-stream)))
	pass-token-action))))

;}}}
;{{{ body

(define html-p-body
  (rdp-repll `() (rdp-call (rdp-cond* html-g-body))))

(define html-g-body
  (old-html-tbl
   `(,html-p-data
     (#((PI OUTPUT)) ,pass-token-action)
     (#(STARTTAG BODY) ,html-p-body)
     (#(ENDTAG))
     ,@html-g-text
     . ,
     (def-repl
       (rdp-call (rdp-cond* html-g-body))
       `(((SPLIT #"\n<P>\n")
	  (PAR)
	  ((LISTITEM ENUMITEM)#"\n<LI>" #"\n</LI>")
	  (DT #"\n<DT>" "</DT>")
	  (DD #"\n<DD>" #"\n</DD>")
	  (TABLE)
	  ((SLIDE INLINE))
	  (QUOTE    #"\n<BLOCKQUOTE>\n" #"\n</BLOCKQUOTE>\n")
	  (LIST #"<UL>" #"</UL>\n")
	  (ENUM #"<OL>" #"</OL>\n")
	  (DESC #"<DL>" #"</DL>\n")
	  (NOTE #"<H5>\n" #"\n</H5>\n")
	  (VERB #"<PRE>" #"\n</PRE>")
	  (FIGURE   (#"\n<A NAME=\"" ,f-t-sect-no "\">") "</A>")
	  (CAPTION  #f "<P>")
	  
	  ))
       `(,html-bib
	 ,html-index
	 (#() ,(rdp-map1
		(lambda (s)
		 (message 0 #"\nHtmlBody Unhandled: " (head s))
		 empty-stream)
		pass-token-action)))))))

;}}}
;{{{ divs

;;; the following is the simplified definition
;;; might get overridden for some document types (book)

(define html-divs
  `(((SECT2 APPDX2 SECTN SECT1 APPDX1 SECT APPDX APPDXN)
     (#"\n<A NAME=\"" ,f-t-sect-no "\">"))
    (CHAPT (#"\n<HR>\n<A NAME=\"" ,f-t-sect-no "\">"))
    ((H4 AH4) ("<H5>" ,f-sect-no " ") #"</H5></A>\n")
    ((H3 AH3) ("<H4>" ,f-sect-no " ") #"</H4></A>\n")
    ((H2 AH2) ("<H3>" ,f-sect-no " ") #"</H3></A>\n")
    ((H1 AH1) ("<H2>" ,f-sect-no " ") #"</H2></A>\n")
    (H0       ("<H1>" ,f-sect-no " ") #"</H1></A>\n")
    ((SECTS SECT1S SECT2S SECTNS APPDX1S APPDX2S APPDXS APPDXNS))
    (ABSTRACT)
    (INTRO)
    (PREFACE)
    ))

(define html-p-sections (rdp-call (rdp-cond* html-g-sections)))

(define html-g-sections
  `(,@html-g-text
    (#(STARTTAG BODY) ,html-p-body)
    ; some effort for the footnote shift to the end of:
    (#(STARTTAG (SECT APPDX))
     ,(rdp-repll
       (lambda (t)
	 `((#"\n<A NAME=\"" ,f-t-sect-no "\">")
	   ,html-footnote-list))
       html-p-sections))
    (#(STARTTAG SECTS)
     ,(rdp-repll
       `(,html-footnote-list)
       html-p-sections))
    ; H0 is what chapters follows, though we need a file header
    (#(STARTTAG H0)
     ,(rdp-map (lambda (s)
		 (stream-append
		  (stream #"<html>\n<head>\n<title>")
		  (tail s)
		  (stream #"</title>\n</head>\n<body>\n<a name=\""
			  (f-t-sect-no (head s))
			  "\"><h1>" (f-sect-no (head s)) " ")
		  (tail s)
		  (stream #"</h1></a>\n")))
	       (rdp-repll `(,identity) (rdp-call html-p-text))))
    . ,(def-repl (rdp-call html-p-sections)
	 `(,html-divs)
	 `((#((INDEXDB BIBDB)) ,(rdp-skip 1))
	   (#() ,(rdp-map1
		  (lambda (s)
		    (message 0 #"\nHtmlSect unhandled: " (head s))
		    empty-stream)
		  pass-token-action))))))

(define html-p-chapters
  (rdp-cond*
   `((#(STARTTAG CHAPT)
      ,(rdp-repll `(,(lambda (t) `#(DIVERT ,(xatv t 'FILE)))
		    #(DIVERT POP))
		  html-p-sections))
     (#(STARTTAG CHAPTS) ,(rdp-repll '() (rdp-call html-p-chapters)))
     (#(STARTTAG APPDXS)
      ,(rdp-repll `(,(lambda (t)
		       `(#(DIVERT ,(xatv t 'FILE))
			 #"<html>\n<head>\n<title>Appendix</title>"))
		    #(DIVERT POP))
		  html-p-sections))
     . ,html-g-sections)))

;}}}
;{{{ contents

(define html-skip-element
  (rdp-repll
   `()
   (rdp-cond* `((#(STARTTAG) ,(rdp-call html-skip-element))
		(#(ENDTAG))
		(#() ,(rdp-skip 1))))))

(define html-contents-division
  (rdp-repll `(,(lambda (t)
		  `((#"\n<LI><A HREF=\""
		     ,(xatv t 'FILE) "#" ,(f-t-sect-no t))
		    )))
	     (rdp-call html-contents)))

(define html-contents
  (rdp-cond*
   `((#(STARTTAG BODY) ,html-skip-element)
     ,@html-g-text
     (#(STARTTAG (SECT1 SECT APPDX CHAPT)) ,html-contents-division)
     . ,(def-repl
	  (rdp-call html-contents)
	  `((
	     ((SECT1S SECTS CHAPTS APPDXS)
	      #"\n<UL>" #"\n</UL>")
	     ((SECT1 SECT APPDX CHAPT)
	      (#"\n<LI><A HREF=\"#" ,f-t-sect-no))
	     ((H4 H3 H2 H1 H0 AH1 AH2 AH3)
	      ("\">" ,f-sect-no " ") "</A>")
	     ))
	  `((#(STARTTAG) ,html-skip-element)
	    (#() ,(rdp-skip 1)))))))

;}}}
;{{{ global

(define html-general-header
"<!--
Warning: don't edit this file. It has been generated by typeset
The next compilation will silently overwrite all changes.
-->
")

(define html-xttl
  (rdp-map
   (lambda (title)
     (stream-append
      (stream #"<HTML>\n<HEAD>\n<TITLE>\n") 
      title
     (stream #"\n</TITLE>\n</HEAD>\n<BODY>\n<A NAME=\"\"><H1>")
     title
     (stream #"</H1></A>\n")))
   (rdp-repll '() html-p-text)))

(define (html-report wrap)
  (rdp-repll
   wrap
   (rdp-cond `((#(STARTTAG RTTL) ,html-xttl)))
   (rdp-cond `((#(STARTTAG (ABSTRACT PREFACE))
		,(rdp-repll `(#"\n<P>" #"\n<HR>\n") html-p-body))))
   (lambda (c h s)
     (stream-append
      (html-contents `(,(lambda (c h s) empty-stream)) h s)
      (rdp-reduce c h s)))
   html-p-sections))

(define (html-switch s)
  (letrec
      ((wrap
	(lambda (t)
	  `(,html-general-header
	    (#"\n<HR>\n<ADDRESS>" ,(html-tr-string (xatv t 'AUTHOR))
	     #"</ADDRESS>\n</BODY>\n</HTML>\n")))))
    (rdp-parse
     (rdp-cond
      `((#(STARTTAG DOCUMENT)
	 ,(rdp-repll
	   wrap
	   (rdp-cond `((#(STARTTAG DTTL) ,html-xttl)))
	   html-p-sections))
	(#(STARTTAG REPORT) ,(html-report wrap))
	(#(STARTTAG BOOK)
	 ,(rdp-repll
	   wrap
	   (rdp-cond `((#(STARTTAG BTTL) ,html-xttl)))
	   (rdp-cond `((#(STARTTAG PREFACE)
			,(rdp-repll `(#"\n<P>" #"\n<HR>\n")
				    html-p-body))))
	   (lambda (c h s)
	     (stream-append
	      (html-contents `(,(lambda (c h s) empty-stream)) h s)
	      (rdp-reduce c h s)))
	   html-p-chapters))
	))
     s
     `(LANGUAGE ,(let* ((lang (xat (head s) 'LANG)))
		   (if lang
		       (car (arg-val lang))
		       "EN")))
     '(FOOTNOTES (FNL))
     `(FOOTNOTEID ,(ints 1)))))

;}}}
;{{{ glue code

(define (html-prepare o d e)
  (doc-preprocess-hook 'run)
  ((stream-display-diverted)
   (stream-through
    (token-stream o)
    html-switch
    stream->list
    (file-split-info-c/as (tail (subfiles-of (string-append d "-c") ".html")))
    ;(watch #"\nxvz: ")
    all-doctypes)))

(define (html-compile-function o d e)
  (if (equal? d "-")
      (html-prepare o d e)
      (with-output-to-file
	  (string-append d e)
	(lambda ()
	  (html-prepare o d e)))))

(set! compile-function html-compile-function)

;}}}
