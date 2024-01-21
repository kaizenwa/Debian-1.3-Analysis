(v-load *typeset-lib* "include" "language")
(v-load *typeset-lib* "include" "biblio")

(define (capture-on args) (capture-out #t))

(v-load *typeset-lib* "target/rtf" "fmts" )
(v-load *typeset-lib* "target/rtf" "bibfmt")

(define (rtf-process-char c)
  (let ((cv (process-escape-char c)))
    (if cv
	(cond
	 ((char=? cv #\{ ) "\\{")
	 ((char=? cv #\} ) "\\}")
	 ((char=? cv #\\ ) "\\\\" )
	 ((char=? cv #\newline ) #\space)
	 (else cv)))))

(set! process-cdata plain-tr-string)
(set! process-char rtf-process-char)

(define (rtf-process-char-verb c)
  (let ((cv (process-escape-char c)))
    (if cv
	(cond
	 ((char=? cv #\newline) "\\line ")
	 (else (rtf-process-char cv))))))

(define para-start-stack (make-stack '(0 . "\\pard \\li0 \\sb120\\sl240 ")))

(define (inc-indent a)
  (let ((nd (+ (caar (stack->list para-start-stack)) a)))
    (push para-start-stack
	  (cons nd
		(string-append "\\pard \\li" 
			       (number->string (* nd 357)) 
			       "\\sb120\\sl240 " ))))
  (set! para-start (cdar (stack->list para-start-stack))))

(define (dec-indent) 
  (pop para-start-stack)
  (set! para-start (cdar (stack->list para-start-stack))))

(define para-start (cdar (stack->list  para-start-stack)))
(define para-end #"\\par}\n" )

(define (c-para args)
  (let ((str (capture-out #f)))
    (if (not (equal? str ""))
	(begin
	  (for-each write-out
		    (list para-start "{\\plain " str para-end))
	  (set! para-start (cdar (stack->list para-start-stack)))))))

(define (rtf-lang lang)
  (cond 
   ((equal? lang "DE") "\\lang0x0407 ")
   ((equal? lang "EN") "\\lang0x0409 ")
   (else "\\lang0x0400 ")))

(define doc-institution #f)

(define (o-doc args)
  (set-lang! args)
  (set-date! args)
  (for-each write-out 
	    (list
	     "{\\rtf1\\ansi \\deff0\\deflang" (rtf-lang doc-lang)
	    "{\\fonttbl"
	    "{\\f0\\froman Times New Roman;}"
	    "{\\f1\\froman Symbol;}"
	    "{\\f2\\fswiss Arial;}"
	    "{\\f3\\fmodern Courier New;}"
	    "{\\f4\\fmodern Courier;}"
	    "}" #\newline
	    "{\\stylesheet"
	    "{\\s252\\li354\\sl240 \\b\\lang0 "
	    "\\sbasedon0\\snext255 heading 3;}"
	    "{\\s253\\sb120\\sl240 \\b\\f2" ;(rtf-lang doc-lang)
	    "\\sbasedon0\\snext0 heading 2;}"
	    "{\\s254\\sb240\\sl240 \\b\\f2\\ul " ;(rtf-lang doc-lang)
	    "\\sbasedon0\\snext0 heading 1;}"
	    "{\\s255\\li708\\sl240 Normal Indent;}"
            "{\\s244\\sl240 \\fs16\\up6"
	    "\\sbasedon0\\snext0 footnote reference;}"
	    "{\\s245\\sl240 \\fs20"
	    "\\sbasedon0\\snext245 footnote text;}"
	    "{\\sb120\\sl240 \\snext0 Normal;}"
	    "}" #\newline
	    "\\paperw11907\\paperh16834"
	    "\\margl1701\\margr567\\margt0\\margb1134\\gutter0 "
	    "\\ftnbj "
	    #\newline
	    "\\sectd \\linex0\\headery709\\footery709\\colsx709\\endhere "
	    "\\pard\\plain \\s254\\sb240\\sl240 "
	    (rtf-lang doc-lang)
	    #\newline
	    do-sect))
  (doc-postface-hook 'add (lambda () (write-out "}"))))

(define do-sect "\\plain \\s254\\sb240\\sl240 \\b\\f2\\ul ")
(define do-sect1 "\\plain \\s253\\sb120\\sl240 \\b\\f2 ")
(define do-sect2 "\\plain \\s252\\sli354\\sl240 \\b ")

(define sc (make-counter))
(define s1c (make-counter))
(define (reset)
  (sc 0 'decimal)(s1c 0 'decimal))

(define (o-appendix args)
  (sc 'uchar))

(define (o-sect args)
  (let* ((id (atv 'ID))
	 (no (sc '++)))
    (if (equal? id "") (set! id no ))
    (for-each write-out
	      (list do-sect (rtf-lang (atv 'LANG)) no " "))
    (s1c 0)))

(define (o-sect1 args)
  (let* ((id (atv 'ID))
	 (no (string-append (sc) "." (s1c '++))))
    (if (equal? id "") (set! id  no ))
    (for-each write-out
	      (list do-sect1 (rtf-lang (atv 'LANG)) no " "))))

(define (o-sect2 args)
  (for-each write-out
	    (list do-sect2 (rtf-lang (atv 'LANG)))))

(define (o-figure args)
  (write-out (string-append 
	      "\\par here goes figure" (atv 'ID) "\\par ")))

(define (o-label args)
  (write-out (string-append 
	      "<A NAME=\"" (atv 'ID) "\"")))

(define (c-xref args)
  (display #"\nWarning <xref> Tag used. Please change to <ref> Tag. RTF dosn't support <xref>." (current-error-port)))

(define (c-ref args)
  (let* ((str (capture-out #f))
	 (id  (atv 'ID))
	 (type (atv 'T)))

    (for-each write-out
	      (cond
	       ((equal? type "B")
		(list "[" (refer-bib id)"]"))
	       ((equal? type "U")
		(list
		 #"{\\i1 " str " (" id ")}"))
	       ((equal? type "M")
		(let ((par (string-find-char id #\( )))
		  (list str "{\\i1 " 
			(if par (substring id 0 par) id ) "\\i0}"
			(if par (substring id par (string-length id)) "") )))
	       ((equal? type "X")
		(list
		 #"<A HREF=\"#" id "\">"
		 (if (> (string-length str) 0)
		     str (lw 'here))
		 "</A>"))))))

(define d-footnote 0)

(define (o-footnote args)
  (set! d-footnote (+ d-footnote 1))
  (for-each write-out 
	    (list "{\\fs16\\up6 " d-footnote 
		  "{\\footnote \\pard\\plain \\s245\\sl240 \\fs20 "
		  "{\\fs16\\up6 " d-footnote "} " )))

(define (o-index args)
  (display #"\n Warning: <index> currently not supported for RTF output"
	   (current-error-port)))

(define (make-content args)
  (write-out (string-append do-sect (lw 'content) "}" ))
  (let loop ((entry (reverse content-list)))
    (if (pair? entry)
	(begin
	  (write-out (string-append "<LI>" (car entry)))
	  (loop (cdr entry)))
	(write-out "</UL><HR>"))))

(define final-replacement-alist 
  (append rtf-fmts-alist
`(

  (REPORT   . #( ,o-doc ,make-biblio ) )
  (DOCUMENT . #( ,o-doc ,make-biblio))

  (DTTL     . #( ""  "\\par" ))

  (BODY      . #("\\pard\\plain \\sb120\\sl240 \\lang0 {" "}"))

;  (ABSTRACT . #( "" ""))
;  (RBODY    . #( "" ""))

;  (CHAPT    . #( "<HR><A NAME=[id]"  #\newline))
;  (H1       . #( "<H1>" "</H1></A>"))

  (SECT    . #( , o-sect ""))
  (H2       . #( "" "\\par " ))

  (SECT1    . #( , o-sect1 ""))
  (H3       . #( "" "\\par " ))

  (SECT2    . #( , o-sect2 "" ))
  (H4       . #( "" "\\par " ))

  (P        . #( ,capture-on ,c-para ))
  (FOOTPAR  . #( "" "\\par" ))

  (XREF     . #( "" ,c-xref))
  (REF      . #( ,capture-on ,c-ref))
  (LABEL    . #( ,o-label  "</A>"))
  (INDEX    . #( ,o-index ""))

  (FOOTNOTE . #( ,o-footnote #"}}\n" ))

  (TT       . #( "{\\f4\\fs20 " "}"))
  (BF       . #( "{\\b1 " "\\b0}"))
  (IT       . #( "{\\i1 " "\\i0}" ))
  (EXAMPLE  . #( "{\\plain \\s255\\li708\\sb120\\sl240\\f4 "  "\\par}" ))
  (NOTE     . #( "{\\fs20 " "\\par}"))
  (NL       . #( "\\line " ""))
  (BREAK    . #( "\\line " ""))

  (SQ       . #( ",," "''"))
  (EM       . #( "{\\b1 " "\\b0}"))

  (MATH     . #( "" ""))
  (SET      . #( "{\\ul1 " "\\ul0}"))

  (APPENDIX . #( ,o-appendix ""))
  (BIBL     . #( ,insert-bib-item  ""))

)))

; prepare-run

(define content-list '())

(define (po-doc args)
  (set-lang! args))

(define (pc-doc args)
  (if has-bib
      (set! content-list 
	    (cons (string-append 
		   "<A href=#biblio>" (sc '++) " "
		   (lw 'bibl)"</a>")
		  content-list))))

(define (po-sect args)
  (let* ((id (atv 'ID))
	 (no (sc '++)))
    (if (equal? id "") (set! id no ))
    (capture-on '())
    (write-out (string-append 
		"<A href=#" id  "> " no " "))
    (s1c 0)))

(define (po-sect1 args)
  (let* ((id (atv 'ID))
	 (no (string-append (sc) "." (s1c '++))))
    (if (equal? id "") (set! id no ))
    (capture-on '())
    (write-out (string-append 
		"<A href=#" id  "> " no " "))))

(define has-bib #f)

(define (po-xref args)
  (if (equal? "Y" (atv 'BIB))
      (set! has-bib #t)))

(define (po-ref args)
  (if (equal? "B" (atv 'T))
      (set! has-bib #t)))

(define (make-content-entry args)
  (let ((str (string-append (capture-out #f) "</A>")))
    (set! content-list (cons str content-list))))

(define prepare-replacement-alist 
`(

  (DOCUMENT . #(,po-doc ,pc-doc))
  (REPORT   . #(,po-doc ,pc-doc))

  (SECT     . #( , po-sect  #\newline))
  (H2       . #( "" ,make-content-entry))

  (SECT1    . #( , po-sect1  #\newline))
  (H3       . #( "" ,make-content-entry))
  (XREF     . #( , po-xref  ""))
  (REF      . #( , po-ref  ""))

  (APPENDIX . #( ,o-appendix ""))

;  (SECT2    . #( , po-sect2  #\newline))
;  (H4       . #( "<H4>" "</H4></A>"))

))

(define (prepare-run)
  (if (equal? doc-type "report")
    (begin
      (set! replacement-alist prepare-replacement-alist)
      ((make-compiler sgmls-output))
      (reset)
  (set! replacement-alist final-replacement-alist))))

(doc-preprocess-hook 'run prepare-run)
