(set! sgml-opts (cons "-i LaTeX " sgml-opts))
;(v-load *typeset-lib* "target/latex" "runlatex")

;===========================================================================

(message 1 "Loading LaTeX")
(set! *load-path* (append *typeset-lib* *load-path*))
(set! sgml-subdir "sgml")

(load "include/normal.scm")

(set! normalize-make-index-section #f)
(set! normalize-make-bib-section #f)

(load "include/manpage.scm")
(load "include/layout.scm")

(define latex-p-data
  `(#(DATA) ,(rdp-hmap (lambda (t) (latex-tr-string (data-token-data t))))))

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
		  ((eq? t 'CDATA) (latex-tr-string (arg-val attrib)))
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
       (else (message 0 'sect-no-from-id (car no)
		      " is not a valid format definition.")
	     ordinary-seq-format))
     (cdr no))))

(define (typed-sect-no-from-id id)
  (let ((no (reverse id)))
    (string-append
     (symbol->string (car no))
     ((case (car no)
	((S F C B F) ordinary-seq-format)
	((A) appdx-seq-format)
	(else (message 0 'sect-no-from-id (car no)
		       " is not a valid format definition.")
	      ordinary-seq-format))
      (cdr no)))))

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

;}}}
;{{{ (latex-tbl l-contents)

(define latex-tbl-stuff (delay (begin
			       (load-silent "include/tbl.scm")
			       (load-silent "target/latex/tbl.scm"))))

; somewhat hacky to resemble the old behavior

(define (old-latex-tbl l-contents)
  (define (empty-cells n)
    (if (<= n 0) '() (cons "" (empty-cells (- n 1)))))
  (let*
      ((tr latex-tr-string)
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
	    (force latex-tbl-stuff)
	    (let ((x (stream->list tbl)))
	      (stream
	       `#(OUTPUT
		  ,(latex-write-tbl
		    (vector
		     patterns
		     (reverse! (cdr x))))))))
	   (rdp-repll `(,identity) r/p)))
	. ,l-contents))))

;}}}
;{{{ latex-math

(define latex-p-math
  (rdp-cond*
   `((#(DATA) ,(rdp-hmap
		(lambda (t) (latex-math-tr-string (data-token-data t)))))
     .
     ,(def-repl
	(rdp-call latex-p-math)
	`((
	   (SET     "\\underline{" "}")
	   (SUB     "_{" "}")
	   (SUP     "^{" "}")
	   ))
	`((#((PI OUTPUT)) ,pass-token-action))))))

(define latex-math (rdp-repll `( "${" "}$") latex-p-math))

;}}}
;{{{ text

(define latex-g-text
  `((#(ENDTAG))
    ,latex-p-data
    (#((PI OUTPUT)) ,pass-token-action)
    ; a somewhat crasy way because we have a different appearence
    ; if we have something in the xref or not
    (#(STARTTAG XREF)
     ,(rdp-map
       (lambda (c)
	 (let ((repl (ref-mark (head c))))
	   (if (stream-empty? (tail c))
	       (stream-append		; spit it out as is
		(stream repl))
	       (stream-append
		(tail c)		; have the contents dumped
		         		; and splice the number in ()'s
		(apply stream  `(" (" ,repl ")"))))))
       (rdp-repll `(,identity) (rdp-call latex-p-text))))
    (#(STARTTAG MATH) ,latex-math)
    (#(STARTTAG FOOTNOTE) ,(rdp-repll `("\\footnote{" "}")
				      (rdp-call latex-p-body)))
    .
    ,(def-repl
       (rdp-call latex-p-text)
       `((
	  (INDEX ,(lambda (t) `("\\label{IDX-" 
				,(number->roman (xatv t 'MARK)) #"}\n")))
	  ;	  (XREF      . ,ref-mark)
	  (BREF      . ,(lambda (t)
			  `(#f
			    ("[" ,(ref-mark t) "]"))))
	  (MREF      ( " " ID " ") )
	  ;    (LABEL . #("<label>" "<label>"))
	  
	  (UREF ID)
	  (STRONG  "\\underline{\\bf " "}")
	  (SQ      . ,(lambda (token)
			(if (member (xatv token 'LANG)
				    english-quotation-style) ; change style
		   '("``" "''")
		   '("\"`" "\"'"))))
	  (BF      "{\\bf " "}")
	  (IT      "{\\it " "}")
	  (TT      "{\\tt " "}")
	  (EM      "{\\em " "}")
	  (CODE    "{\\tt " "}")
	  (VAR     "{\\it " "}")
	  (META    )
	  (LANG    )
	  (NEWPAGE #"\n\\newpage\n")
	  (NL      "~\\\\\n")
	  (LABEL)
	  ))
       `()
       )))

(define latex-p-text
  (rdp-cond* `(,@latex-g-text
	       (#() ,(lambda (c h s)
		       (message 0 #"\nLatexText unhandled: " (head s))
		       (rdp-reduce c h (tail s)))))))

;}}}
;{{{ INDEX

(define (latex-make-index level idx)
  (if
   (> (length idx) 0)
   `(#\newline
     ,@(map
	(lambda (i)
	  `(,(vector-ref '#(#"\n\\item " #"\n\\subitem ")
			 (min level 1))
	    ,(latex-tr-string (car i))	; the indexed text
	    " "
	    ,@(list-join
	       ", " (reverse! (map (lambda (i)
				    `(#"\\pageref{IDX-" ,(number->roman i) "}"))
				  (vector-ref (cdr i) 0))))
	    . ,(latex-make-index (+ level 1) (vector-ref (cdr i) 1))))
	idx)
     ,(if (eqv? level 0) #"\n\\indexspace" #\newline))
   '()))

(define latex-index
  `(#(PLACE INDEX)
    ,(rdp-map1
      (lambda (s)
	(if (null? (car (token-args (head s))))
	    empty-stream
	    (stream-append
	     (apply
	      stream
	      #"\n\\begin{theindex}"
	      (latex-make-index 0 (token-args (head s))))
	     (stream #"\n\\end{theindex}"))))
      pass-token-action)))

;}}}
;{{{ latex-bib

(define (latex-format-bib dl)
  (letrec
      ((tr latex-tr-string)
       (fmt1
	(lambda (data)
	  (let* ((no (vector-ref data 0))
		 (tag (vector-ref data 1))
		 (entry (vector-ref data 2)))
	    (if
	     (not (start-tag? entry))
	     `(#"\n\\bibitem{\bf " ,no #"}\n" ,tag)
	     (let ((auth (tr (xatv entry 'AUTHOR)))
		   (title (tr (xatv entry 'TITLE)))
		   (publ (tr (xatv entry 'PUBL)))
		   (year (xatv entry 'YEAR)))
	       `(#"\n\\bibitem{" ,no #"}\n{\\it " ,auth "\\/}: "
		 ,title "; " ,year " {\\it " ,publ "\\/}"))))))
       (fmt-seq (lambda (dl)
		  `(,@(fmt1 (head dl))
		    . ,(if (null? (cdr dl))
			   '()
			   (fmt-seq (cdr dl)))))))
    `(#"\n\\begin{thebibliography}{999}"
      ,@(fmt-seq dl)
      #"\n\\end{thebibliography}\n")))

(define latex-bib
  `(#(PLACE BIB)
    ,(rdp-map1
      (lambda (s)
	(let ((token (head s)))
	  (if (not (null? (token-args token)))
	      (stream
	       `#(OUTPUT ,(latex-format-bib (token-args token))))
	      empty-stream)))
      pass-token-action)))

;}}}
;{{{ body

(define latex-pp-body (rdp-call (rdp-cond* latex-g-body)))

(define latex-p-body
  (rdp-repll `() latex-pp-body))

(define latex-g-body
  (old-latex-tbl
   `(,latex-p-data
     (#(ENDTAG))
     (#((PI OUTPUT)) ,pass-token-action)
     (#(STARTTAG BODY) ,latex-p-body)
     (#(STARTTAG FIGURE)
      ,(rdp-repll
	'()
	(rdp-map (lambda (fig hd)
		   (stream-append
		    (stream #"\n\\begin{figure}[t]\n\\hfil{")
		    fig
		    (stream #"}\\hfil\n\\caption{")
		    hd
		    (stream #"}\n\\end{figure}\n")))
		 (rdp-call
		  (rdp-cond*
		   `((#(STARTTAG CAPTION)) . ,latex-g-body)))
		 (rdp-repll '() latex-p-text))))
     ,@latex-g-text
     . ,
     (def-repl
       (rdp-call (rdp-cond* latex-g-body))
       `(((SPLIT)
	  (PAR #f #"\\par\n")
	  ((LISTITEM ENUMITEM)#"\n\\item " #"\n")
	  (DT #"\n\\item[ " " ] ")
	  (DD #f #"\n")
	  (TABLE)
	  ((SLIDE INLINE))
	  (QUOTE  #"\n\\begin{quotation}\n" #"\n\\end{quotation}\n" )
	  (LIST   #"\n\\begin{itemize}\n" #"\n\\end{itemize}\n" )
	  (ENUM   #"\n\\begin{enumerate}\n" #"\n\\end{enumerate}\n" )
	  (DESC   #"\n\\begin{description}\n" #"\n\\end{description}\n" )
	  (NOTE   "{\\small{" "}}" )
	  (VERB   #"\n{\\pseudoverb%\n" #"\n\\endpseudoverb}\\par\n")
	  (LITERATE)
	  ))
       `(,latex-bib
	 ,latex-index
	 (#() ,(rdp-map1
		(lambda (s)
		 (message 0 #"\nLatexBody Unhandled: " (head s))
		 empty-stream)
		pass-token-action)))))))

;}}}
;{{{ divs

;;; the following is the simplified definition
;;; might get overridden for some document types (book)

(define latex-divs
  `(((SECT APPDX) #"\n\\section")
    ((SECT1 APPDX1) #"\n\\subsection")
    ((SECT2 SECTN APPDX2 APPDXN) #"\n\\subsubsection")
    ((H1 AH1 H2 AH2 H3 AH3 H4 AH4 H0) "{" #"}\n")
    ((SECTS SECT1S SECT2S SECTNS APPDXS APPDX1S APPDX2S APPDXNS))
    (INTRO)
    (PREFACE)
    ))

(define latex-p-sections (rdp-call (rdp-cond* latex-g-sections)))

(define latex-g-sections
  `(,@latex-g-text
    (#(STARTTAG BODY) ,latex-p-body)
    . ,(def-repl latex-p-sections
	 `(,latex-divs)
	 `((#((INDEXDB BIBDB)) ,(rdp-skip 1))
	   (#() ,(rdp-map1
		  (lambda (s)
		    (message 0 #"\nLatexSect unhandled: " (head s))
		    empty-stream)
		  pass-token-action))))))


(define latex-book-divs
  `(((CHAPT APPDX) #"\n\\chapter")
    (APPDX1 #"\n\\section")
    ((APPDX2 APPDXN) #"\n\\subsection")
    .
    , latex-divs))

(define latex-book-p-sections
  (rdp-call
   (rdp-cond*
    `(,@latex-g-text
      (#(STARTTAG BODY) ,latex-p-body)
      . ,(def-repl latex-book-p-sections
	   `(,latex-book-divs)
	   `((#((INDEXDB BIBDB)) ,(rdp-skip 1))
	     (#() ,(rdp-map1
		    (lambda (s)
		      (message 0 #"\nLatexSect unhandled: " (head s))
		      empty-stream)
		    pass-token-action))))))))

(define latex-p-chapters
  (rdp-cond*
   `((#(STARTTAG CHAPT)
      ,(rdp-repll `(#"\n\\chapter") latex-p-sections))
     (#(STARTTAG CHAPTS) ,(rdp-repll '() (rdp-call latex-p-chapters)))
     (#(STARTTAG APPDXS) ,(rdp-repll '(#"\n\\appendix") latex-book-p-sections))
     . ,latex-g-sections)))

;}}}
;{{{ contents

(define latex-skip-element
  (rdp-repll
   `()
   (rdp-cond* `((#(STARTTAG) ,(rdp-call latex-skip-element))
		(#(ENDTAG))
		(#() ,(rdp-skip 1))))))

(define latex-contents-division
  (rdp-repll `((#"\n<LI><A HREF=\"" ,f-t-sect-no))
	     (rdp-call latex-contents)))

(define latex-contents
  (rdp-cond*
   `((#(STARTTAG BODY) ,latex-skip-element)
     ,@latex-g-text
     (#(STARTTAG (SECT1 SECT APPDX CHAPT)) ,latex-contents-division)
     . ,(def-repl
	  (rdp-call latex-contents)
	  `((
	     ((SECT1S SECTS CHAPTS APPDXS)
	      #"\n<UL>" #"\n</UL>\n")
	     ((SECT1 SECT APPDX CHAPT)
	      (#"\n<LI><A HREF=\"#" ,f-t-sect-no))
	     ((H4 H3 H2 H1 H0 AH1 AH2 AH3)
	      ("\">" ,f-sect-no " ") "</A>")
	     ))
	  `((#(STARTTAG) ,latex-skip-element)
	    (#() ,(rdp-skip 1)))))))

;}}}
;{{{ global

(define latex-general-header
"% Warning: don't edit this file. It has been generated by typeset
% The next compilation will silently overwrite all changes.
")

(define latex-general-preample "
\\sloppy
% ripped off Rainer Sch\"opf's verbatim.dtx
\\makeatletter
\\begingroup
  \\catcode`\\ =\\active%
  \\def\\x{\\def\\typeset@vobeyspaces{\\catcode`\\ \\active\\let \\typeset@psspace}}
  \\expandafter\\endgroup\\x
\\def\\typeset@psspace{\\leavevmode\\penalty\\@M\\ }
\\def\\pseudoverb{%
  \\vspace{1.2ex}
  \\@beginparpenalty \\predisplaypenalty
  \\parindent\\z@\\parfillskip\\@flushglue\\parskip\\z@
  \\leftskip\\@totalleftmargin\\rightskip\\z@
  \\@@par
  \\def\\par{%
    \\if@tempswa
      \\null\\@@par\\penalty\\interlinepenalty
    \\else
      \\@tempswatrue
      \\ifhmode\\@@par\\penalty\\interlinepenalty\\fi
    \\fi}%
 \\tt
 \\frenchspacing\\typeset@vobeyspaces\\obeylines}
\\def\\endpseudoverb{%
  \\leavevmode\\vspace{-1.2ex}}
\\makeatother
")

(define latex-xttl
  (rdp-repll '(#"\n\\title{" "}" ) latex-p-text)))

(define (latex-report wrap)
  (rdp-repll
   wrap
   (rdp-cond `((#(STARTTAG RTTL) ,latex-xttl)))
   (rdp-insert #"\n\\maketitle\n")
   (rdp-cond `((#(STARTTAG (ABSTRACT PREFACE))
		,(rdp-repll `(#"\n\\begin{abstract}\n"
			      #"\n\\end{abstract}")
			    latex-pp-body))))
   (rdp-cond `((#(STARTTAG SECTS)) ,(rdp-repll '() latex-p-sections)))
   latex-p-sections))

(define (latex-switch s)
  (letrec ((the-index empty-stream)
	   (the-bib #f)
	   (doc-token (head s))
	   (face (apply hook 'face 'run (xatv (head s) 'FACE))))
    (set! the-index (stream-filter (lambda (t)
				     (if (eq? (token-type t) 'BIBDB)
					 (set! the-bib t))
				     (eq? (token-type t) 'INDEXDB))
				   s))
    (letrec
	((wrap
	  (lambda (t)
	    `((,latex-general-header
	       ,(case latex-latex-type
		  ((LATEX209) #"\n\\documentstyle")
		  (else #"\n\\documentclass"))
	       "[" ,(strings-join
		     `(,@(case (token-gi t)
			   ((REPORT) '("titlepage"))
			   ((BOOK) '("titlepage"))
			   (else '()))
		       ,@(let ((lang (xatv t 'LANG)))
			   (cond
			    ((and (eq? latex-latex-type 'LATEX209)
				  (equal? lang "DE"))
			     '("german"))
			    (else '())))
		       ,@(if (member "2S" face)
			     '("twoside") '())
		       ,@(if (member "2C" face)
			     '("twocolumn") '())
		       ,@latex-styleoptions)
		     ",")
	       "]"
	       ,(case (token-gi t)
		  ((DOCUMENT REPORT) "{article}")
		  ((BOOK) "{book}")
		  (else "{}"))
	       #\newline
	       ,@(if (not (eq? latex-latex-type 'LATEX209))
		     (apply append
			    (map (lambda (i) `(#"\n\\usepackage" ,i))
				 `(,@(let ((lang (car (xatv t 'LANG))))
				       (cond
					((equal? lang "DE") '("{german}"))
					(else '())))
				   ,@latex-packages)))
		     '())
	       ,latex-general-preample
	       ,@latex-preamble
	       #"\n\\date{" ,(latex-tr-string (xatv t 'DATE)) "}"
	       #"\n\\author{" ,(latex-tr-string (xatv t 'AUTHOR)) "}"
	       #"\n\\begin{document}\n")
	      (#f ; that's not such a fancy proc
	       ,(lambda (dummy)
		  `(,(if (or (not the-bib)
			     (null? (data-token-data the-bib)))
			 #f
			 (latex-format-bib (data-token-data the-bib)))
		    ,(if (or (stream-empty? the-index)
			     (member 'NIDX face)
			     (null? (data-token-data (head the-index))))
			 #f
			 `(#"\n\\begin{theindex}"
			   ,@(latex-make-index
			      0 (data-token-data (head the-index)))
			   #"\n\\end{theindex}\n"))
		    #"\n\\end{document}\n"))))))
	 (lang (let* ((lang (xat (head s) 'LANG)))
		 (if lang
		     (car (arg-val lang))
		     "EN")))
	 )
      (rdp-parse
       (rdp-cond
	`((#(STARTTAG DOCUMENT)
	   ,(rdp-repll
	     wrap
	     (rdp-cond `((#(STARTTAG DTTL) ,latex-xttl)))
	     (rdp-insert #"\n\\maketitle\n")
	     latex-p-sections))
	  (#(STARTTAG REPORT) ,(latex-report wrap))
	  (#(STARTTAG BOOK)
	   ,(rdp-repll
	     wrap
	     (rdp-cond `((#(STARTTAG BTTL) ,latex-xttl)))
	     (rdp-insert #"\n\\maketitle")
	     (rdp-cond `((#(STARTTAG PREFACE)
			  ,(rdp-repll `((#"\n\\chapter*{"
					,((phrase lang) 'preface) #"}\n"))
				      latex-p-body))))
	     (rdp-insert #"\n\\tableofcontents")
	     (rdp-cond `((#(STARTTAG INTRO)
			  ,(rdp-repll `((#"\n\\chapter*{"
					 ,((phrase lang) 'intro) #"}\n"))
				      latex-p-body))))
	     latex-p-chapters))
	  ))
       s
       `(LANGUAGE ,lang)))))

;}}}
;{{{ glue code

(define (latex-prepare o d e)
  (doc-preprocess-hook 'run)
  ((stream-display-diverted)
   (stream-through
    (token-stream o)
    latex-switch
    all-doctypes)))

(define (latex-compile-function o d e)
  (if (equal? d "-")
      (latex-prepare o d e)
      (with-output-to-file
	  (string-append d e)
	(lambda ()
	  (latex-prepare o d e)))))

(set! compile-function latex-compile-function)

;}}}
