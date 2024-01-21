(message 1 "Loading Lout")
(set! *load-path* (append *typeset-lib* *load-path*))
(set! sgml-opts (cons "-i Lout " sgml-opts))

(load "include/normal.scm")
(set! normalize-make-index-section #f)
(set! normalize-make-bib-section #t)
(set! normalize-report normalize-report-intro)

(load "include/layout.scm" )

(define lout-p-data
  `(#(DATA) ,(rdp-hmap (lambda (t) (lout-tr-string (data-token-data t))))))

;{{{ (conv-for-repll)

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
		  ((eq? t 'CDATA) (lout-tr-string (arg-val attrib)))
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
;{{{ general stuff
(define (lout-division-lang token)
  (cdr (assoc (car (xatv token 'LANG))
	      '(("DE" . "German") ("EN" . "English")))))

(define (f-sect-no token)
  (let ((no (reverse (xatv token 'NO))))
    ((case (car no)
       ((S C F) ordinary-seq-format)
       ((A) appdx-seq-format)
       (else (error 'f-sect-no "Number not good in" token)))
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

;{{{ lout-desc

(define (lout-desc l-contents)
  (let*
      ((result `((#(STARTTAG DESC) . #f) . ,l-contents))
       (call-contents (rdp-cond* result))
       (dt1o #"\n{}// outdent @Break{ Bold @Font {")
       (dtno #"\n@LP outdent @Break{ Bold @Font {")
       (dtc "} &2f ")
;       (indent #"}\n@LP ||2f {")
       (indent #"}\n@LP @DescFollow {")
       (p-ddb (rdp-repll		; overwrite split
	       '()
	       (rdp-cond
		`((#(STARTTAG (LIST ENUM)) ; if the first is...
		   ,(rdp-insert indent)	; special handling
		   ,call-contents)	; continue like standard
		  (#()			; ELSE
		   ,(rdp-cond*
		     `((#(STARTTAG SPLIT)	; indent on spilt
			,(rdp-repll (list indent) call-contents)
			,call-contents) ; but use it just once
		       . ,result)))))))
       (p-dd1 (rdp-repll '(#f "}// ") (rdp-cond `((#(STARTTAG BODY) ,p-ddb)))))
       (p-dt1 (rdp-cond
	       `((#(STARTTAG DT) ,(rdp-repll `(,dt1o ,dtc) call-contents))
		 (#(STARTTAG DD) ,p-dd1))))
       (p-dtn (rdp-cond*
	       `((#(STARTTAG DT) ,(rdp-repll `(,dtno ,dtc) call-contents))
		 (#(STARTTAG DD) ,p-dd1))))
       (p-dt (rdp-begin p-dt1 p-dtn)))
    (set-cdr! (car result) (list (rdp-repll '() p-dt)))
    result))

;}}}
;{{{ lout-figure

; These "lout-figure" definitions do the same.
; the first by modifying the result stream of a straight translation
; the second by building a new after "deep thought" parsing
;
; This definition needs  >> (CAPTION  #"\n@Caption{" "}") <<
; within the replacements "contents"
;
;(define (lout-figure contents)
;  `((#(STARTTAG FIGURE)
;     ,(lambda (c h s)
;	(rdp-reduce
;	 c h
;	 (cons-stream
;	  #"\n@Figure"
;	  (stream-finde
;	   (start-gi? '(CAPTION))
;	   (lambda (sc)
;	     (stream-cute
;	      (end-gi? '(FIGURE))
;	      (lambda (se)
;		(stream-append
;		 (cons-stream #"\n{"
;			      (substream (tail s) sc))
;		 (stream #"}//0co\n")
;		 (tail se)))
;	      sc))
;	   s)))))))

(define (lout-figure contents)
  `((#(STARTTAG FIGURE)
     ,(rdp-repll
       '()
       (rdp-map
	(lambda (fc caption)
	  (stream-append
	   (stream #"\n@Figure")
	   caption
	   (stream #"\n{")
	   fc
	   (stream #"}//0co\n")))
	contents
	(rdp-cond
	 `((#(STARTTAG CAPTION) ,(rdp-repll '(#"\n@Caption{" "}")
					    contents))
	   (#() ,(rdp-map1
		  (lambda (s)
		    (error "lout-figure" "Expected CAPTION, got: " (head s))
		    empty-stream)
		  pass-token-action)))))))
    (#(STARTTAG CAPTION))))

;}}}
;{{{ (lout-tbl l-contents)

(define lout-tbl-stuff (delay (begin
				(v-load *typeset-lib* "include" "tbl")
				(v-load *typeset-lib* "target/lout" "tbl"))))

; somewhat hacky to resemble the old behavior

(define (old-lout-tbl l-contents)
  (define (empty-cells n)
    (if (<= n 0) '() (cons "" (empty-cells (- n 1)))))
  (let*
      ((tr lout-tr-string)
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
	((call-contents (rdp-cond* l-contents))
	 (pattern
	  (rdp-map
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
	  (rdp-map
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
	     (force lout-tbl-stuff)
	     (let ((x (stream->list tbl)))
	       (stream
		(lout-write-tbl
		 (vector
		  patterns
		  (reverse! (cdr x)))))))
	   (rdp-repll `(,identity) r/p)))
      . ,l-contents))))

;}}}

(define lout-repls1
  `((PAR)
    (SPLIT #"\n@LP\n")
    (INDEX     ("{@PageMark I." MARK "}"))
    (XREF      . ,(lambda (t)		; closure!!!
		    `(#f ,(ref-mark t))))
    (BREF      . ,(lambda (t)
		    `(#f
		      ("[" ,(ref-mark t) "]"))))
    ((MREF UREF)      ( " " ID " "))
    (FOOTNOTE "{@FootNote{" "}}")

    (STRONG  "{ Bold @Font @Underlined {" "}}" )
    (SQ      . ,(lambda (t) 
		  (if (member (xatv t 'LANG)
			      english-quotation-style) ; change style
		      '("``" "''")
		      '(",," "''"))))
    (BF      "{ Bold @Font {" "}}" )
    (IT      "{ Slope @Font{" "}}" )
    (TT      "{@TSCode{" "}}")
    (EM      ,(lambda(t) (if (odd? (xatv t 'LEVEL))
			     "{Slope} @Font{"
			     "{Bold} @Font{"))
	     "}")
    (CODE    "{@TSCode{" "}}")
    (VAR     "{ Slope @Font{" "}}" )
    (META    )
    (MATH    "{@Eq{" "}}")
    (SET     "{@Underlined{" "}}")
    (SUB     "{@Sub{" "}}")
    (SUP     "{@Sup{" "}}")
    (LANG    ("{" ,lout-division-lang " @Language{") "}}")
    (NEWPAGE #"\n@NP\n")
    (NL      #"\n@LLP\n")
    ;    (LABEL . #("<label>" "<label>"))
    ((LITERATE LABEL))
))

(define lout-body
  `((BODY)
    ; Don'T use Lout lists! The last change made it even worse!
    ; You can't put <newpage> inside of them!
    (LISTITEM ,(let ((mark (lambda (token)
			     (vector-ref '#("@Bullet" "--" "@Star")
					 (min (xatv token 'LEVEL) 2))))
		     (gap #"\n//@ListGap "))
		 `(#f			; can't be lambda because of rpll def
		   ,(lambda (t) (if (eqv? (xatv t 'NO) 1) #f gap))
		   "{1v @High {} //0co " ,mark #"}//0co @DescFollow {\n"
		   ))
	      #"}\n")
    (ENUMITEM
     ,(let* ((LA-1 (- (char->integer #\a) 1))
	     (UA-1 (- (char->integer #\A) 1))
	     (conv (vector identity
			   (lambda (i) (integer->char (+ LA-1 i)))
			   (lambda (i) (number->roman i))
			   (lambda (i) (integer->char  (+ UA-1 i)))))
	     (gap #"\n//@ListGap ")
	     (mark (lambda (t)
		     ((vector-ref conv (xatv t 'LEVEL)) (xatv t 'NO)))))
	`(#f
	  ,(lambda (t) (if (eqv? (xatv t 'NO) 1) #f gap))
	  ,mark #" //0co @DescFollow {\n"
	  ))
     #"}\n")
    ((LIST ENUM))

    (TABLE)
    ((SLIDE INLINE))
    (QUOTE    . ,(lambda (t)
		   (if (equal? (car (xatv t 'STYLE)) "CENTER")
		       `(#"\n@QuotedDisplay @HExpand { @Center {" #"}}\n")
		       `(#"\n@QuotedDisplay{" #"}\n"))))
    (VERB     #"{@TSCode {lines @Break {{}" #"}}}\n")
    (NOTE     #"\n{0.8f}@Font{\n" #"}\n")

))

(define lout-divs
  `(((SECT2 SECTN APPDX2 APPDXN)
     (#"\n// //@SubSectionGap @CNP " ,lout-division-lang
				  " @Language{")
     #"}")
    ((H3 AH3 H4 AH4)
     . ,(lambda (token)
	  (let ((no (f-sect-no token)))
	    `((quote #"\n@Heading{" ,no " ")
	      (quote "{@PageMark S." ,no #"}}\n@LP\n")))))
    (SECT1
     (#"\n@SubSection\n@InitialLanguage{"
      ,lout-division-lang #"}")
     #"\n@End @SubSection")
    (APPDX1
     (#"\n@SubAppendix\n@InitialLanguage{"
      ,lout-division-lang #"}")
     #"\n@End @SubAppendix")
    ((H2 H1 H0 AH2 AH1)
     . ,(lambda (token)
	  (let ((no (f-sect-no token)))
	    `(#"\n@Title{"
	      (quote #"}\n@Begin\n{@PageMark S." ,no #"}@LP\n")))))
    (SECT1S  #"\n@BeginSubSections" #"\n@EndSubSections")

;    (SECT (#"\n@Section\n@InitialLanguage{" ,lout-division-lang #"}"
;	    ,(lambda (t) (message 1 "Section " (f-sect-no t)) #f)
;	    )
;	  #"\n@End @Section")
    (SECTS #"\n@BeginSections"  #"\n@EndSections")
    ((SECT2S APPDX2S SECTNS APPDXNS))
    (CHAPTS)
    (CHAPT (#"\n@Chapter\n@InitialLanguage{" ,lout-division-lang #"}"
	    ,(lambda (t) (message 1 "Chapter " (f-sect-no t)) #f)
	    )
	   #"\n@End @Chapter")

    (APPDX1S #"\n@BeginSubAppendices" #"\n@EndSubAppendices")
;    (APPDX   (#"\n@Appendix\n@InitialLanguage{" ,lout-division-lang #"}")
;	     #"\n@End @Appendix")
    (APPDXS  #"\n@BeginAppendices" #"\n@EndAppendices")
    (DTTL "@CentredDisplay @Heading{" #"}\n@LP\n")
    (RTTL #"\n@Title{" "}//")
    (BTTL #"\n@Title{" "}//")
    (ABSTRACT  #"\n@Abstract{@LP\n" "}")
    (INTRO     #"\n@Introduction{" #"}\n")
    (PREFACE   #"\n@Preface{" #"}\n")
    ))

(define lout-unhacked-divs
  `((SECT (#"\n@Section\n@InitialLanguage{" ,lout-division-lang #"}"
	   ,(lambda (t) (message 1 "Section " (f-sect-no t)) #f)
	   )
	  #"\n@End @Section")
    (APPDX   (#"\n@Appendix\n@InitialLanguage{" ,lout-division-lang #"}")
	     #"\n@End @Appendix")))


; ok, parsing the whole stream just to insert the #(PLACE INDEX)
; seems to much effort, thus we dive deeper into the parser

(define (lout-rdp-insert-index-hack-before-last-sect/appdx idx)
  (lambda (c h s)
    (if (memq (token-gi (head (tail (tail s)))) '(REPORT DOCUMENT))
	(cons-stream
	 (lout-index-hack idx)
	 (rdp-reduce c h s))
	(rdp-reduce c h s))))
  
(define (lout-hack-idx-divs contents idx)
  `((#(STARTTAG SECT)
     ,(rdp-repll
       `((#"\n@Section\n@InitialLanguage{"
	  ,lout-division-lang #"}"
	  ,(lambda (t) (message 1 "Section " (f-sect-no t)) #f)
	  )
	 #"\n@End @Section")
       contents
       (lout-rdp-insert-index-hack-before-last-sect/appdx idx)
       ))
    (#(STARTTAG APPDX)
     ,(rdp-repll
       `((#"\n@Appendix\n@InitialLanguage{" ,lout-division-lang #"}")
	 #"\n@End @Appendix")
       contents
       (lout-rdp-insert-index-hack-before-last-sect/appdx idx)
       ))))

(define lout-contents
  `(((H4 AH4)
     ,(lambda (t)
	(let ((no (f-sect-no t)))
	  `(quote #"\n@BypassContentsEntry indent{2f} number{"
		  ,no "} pagenum{@PageOf S." ,no "} title{")))
     "}")
    ((H3 H2 H1 AH3 AH2)
     ,(lambda (t)
	(let ((no (f-sect-no t)))
	  `(quote #"\n@BypassContentsEntry indent{1f} number{"
		  ,no "} pagenum{@PageOf S." ,no "} title{")))
     "}")
    ((H0 AH1 HP)
     ,(lambda (t)
	(let ((no (f-sect-no t)))
	  `(quote #"\n@BypassMajorContentsEntry indent{0f} number{"
		  ,no "} pagenum{@PageOf S." ,no "} title{")))
     "}")
    ))

;{{{ Index

(define (string-right! s1 s2)
  (let ((l1 (string-length s1))
	(l2 (string-length s2)))
    (blit-string! s2 0 s1 (- l1 l2) l2)
    s1))

(define (lout-index-hack idx)
  (if (> (length idx) 0)
      (letrec
	  ((fm1
	    (lambda (type)
	      (lambda (i)
		`((,type ,(car i)
		   ,@(list-join "," (reverse! (map (lambda (i)
						     `(" @PageOf{I." ,i "}"))
						   (vector-ref (cdr i) 0))))
		   "}")
		  . ,(apply append (map (fm1 " @RawSubIndex{")
			 (vector-ref (cdr i) 1)))))))
	   (enum (lambda (n el)
		   (If (null? el)
		       el
		       `(#"\n"
			 ,(string-right! (make-string 6 #\0)
					 (number->string n))
			 ,@(car el)
			 . ,(enum (+ n 1) (cdr el)))))))
	(enum 0 (apply append (map (fm1 " @RawIndex{") idx))))))

(define lout-ins-index-hack
  `(#(PLACE INDEX)
    ,(rdp-hmap
    (lambda (t)
      (lout-index-hack (token-args t))))))

(define (lout-make-bypass-index idx)
  (if (> (length idx) 0)
      (letrec
	  ((fm1
	    (lambda (type)
	      (lambda (i)
		`((,type ,(car i)
		   ,@(list-join "," (reverse! (map (lambda (i)
						     `(" @PageOf{I." ,i "}"))
						   (vector-ref (cdr i) 0))))
		   #"}\n")
		  . ,(apply append (map (fm1 "@BypassRawIndex indent{1f} {")
			 (vector-ref (cdr i) 1))))))))
	`(#"\n@BypassBeginIndex\n"
	  ,(apply append (map (fm1 "@BypassRawIndex{") idx))
	  #"@BypassEndIndex\n"))
      #\newline))

;}}}
;{{{ lout-bib

(define lout-bib
  (letrec
      ((tr lout-tr-string)
       (fmt1
	(lambda (data)
	  (let* ((no (vector-ref data 0))
		 (tag (vector-ref data 1))
		 (entry (vector-ref data 2)))
	    (if
	     (not (start-tag? entry))
	     `(#"\n@LP Bold @Font{ 1.5v @Wide{["
	       ,no "]} |1v " ,tag "}")
	     (let ((auth (tr (xatv entry 'AUTHOR)))
		   (title (tr (xatv entry 'TITLE)))
		   (publ (tr (xatv entry 'PUBL)))
		   (year (xatv entry 'YEAR)))
	       `(#"\n@LP 1.5v @Wide {[" ,no
		 "]}|1v Slope @Font {" ,auth
		 "}: " ,title "; " ,year " Slope @Font {"
		 ,publ "}"))))))
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
		(stream
		 (fmt-seq (token-args token))))))
	pass-token-action))))

;}}}

(set! sgml-subdir "sgml")
;(set! sgml-opts '("-deru "))
;{{{ lout-brief

(define lout-brief
  (letrec
      ((g 
	(rdp-cond*
	 (lout-desc
	  (old-lout-tbl
	   `(,lout-p-data
	     (#(ENDTAG))
	     (#(OUTPUT) ,pass-token-action)
	     . ,(def-repl
		  (rdp-call g)
		  `(((EM   "{@em{"         "}}" )
		     (SQ   "{@sql}"        "{@sqr}" )
		     ((LIST ENUM) #"\n@List\n"    #"\n@EndUp\n")
		     ((LISTITEM ENUMITEM) #"\n@Item{"     #"}\n" )
		     (QUOTE #"\n@CenteredDisplay{\n" #"}\n")
		     )
		    ,lout-repls1
		    ,lout-body)
		  `((#() ,(rdp-map
			   (lambda (s)
			     (message 0 #"\nUnhandled: " (head s))
			     empty-stream)
			   pass-token-action))
		    ))))))))
    (rdp-repll '(#f #f) g)))

;}}}
;{{{ lout-document

(define (lout-document c h basic)
  ; first enforce read of the whole stream, thereby filling
  ; the cross ref data bases
  (let* ((idxs (stream-filter
		(lambda (t) (eq? (token-type t) 'INDEXDB))
		basic))
	 (idx (if (stream-empty? idxs)
		  '()
		  (data-token-data (head idxs))))
	 (lang (car (xatv (head basic) 'LANG))))
    (message 1 "collected cross refs")
    (letrec
	(
	 (call-contents (rdp-call contents))
	 (contents (rdp-cond*
		    (lout-desc
		     (old-lout-tbl
		      `(,lout-p-data
			(#(ENDTAG) ,rdp-leave)
			. ,(def-repl
			     (rdp-call contents)
			     ;(rdp-call contents)
			     `(,lout-repls1
			       ,lout-body ,lout-divs)
			     `(,@(lout-figure (rdp-call contents))
			       ,@(lout-hack-idx-divs
				  (rdp-call contents) idx)
			       ;,lout-ins-index-hack
			       ,lout-bib
			       (#((PI OUTPUT)) ,pass-token-action)
			       (#((INDEXDB BIBDB)) ,(rdp-skip 1))
			       (#() ,(rdp-map
				      (lambda (s)
				       (message 0 #"\nUnhandled: " (head s))
				       empty-stream)
				      pass-token-action)))
			     ))))))
	 )
      ((rdp-cond
	(def-repl contents
	  `(((DOCUMENT
	      (#"\n@SysInclude{docf}\n"
		      ,(lambda (t) (lout-dl-use 'document (xat t 'FACE) 
						(if (null? idx) 'NIDX 'IDX)))
	       #"\n@Use { @TypesetLayout }\n@Use { @OrdinaryLayout }\n"
	       #"\n@Document\n@InitialLanguage{"
	       ,lout-division-lang
	       #"}//\n@Text @Begin\n")
	      #"\n@End @Text\n")))
	  '()))
       c h basic))))

;}}}
;{{{ lout-report

(define (lout-report c h basic)
  (let* ((idxs (stream-filter
		(lambda (t) (eq? (token-type t) 'INDEXDB))
		basic))
	 (idx (if (stream-empty? idxs)
		  '()
		  (data-token-data (head idxs))))
	 (lang (car (xatv (head basic) 'LANG))))
    (message 1 "collected cross refs")
    (letrec
	((call-contents (rdp-call contents))
	 (contents
	  (rdp-cond*
	   (lout-desc
	    (old-lout-tbl
	     `(,lout-p-data
	       (#(ENDTAG))		; leave
	       . ,(def-repl
		    (rdp-call contents)
		    `(,lout-repls1
		      ,lout-body
		      ((SECTS) (APPDXS) . ,lout-divs))
		    `(,@(lout-figure (rdp-call contents))
			       ,@(lout-hack-idx-divs
				  (rdp-call contents) idx)
		      ,lout-bib
		      (#((PI OUTPUT)) ,pass-token-action)
		      (#((INDEXDB BIBDB)) ,(rdp-skip 1))
		      (#() ,(rdp-map1
			     (lambda (s)
			      (message 0 #"\nUnhandled: " (head s))
			      empty-stream)
			     pass-token-action)))
		    )))))))
      ((rdp-cond
	(def-repl contents
	  `(((REPORT (#"\n@SysInclude{reportf}\n"
		      ,(lambda (t) (lout-dl-use 'report (xat t 'FACE) 
						(if (null? idx) 'NIDX 'IDX)))
		      #"\n@Use {@TypesetLayout }\n@Use { @ReportLayout }\n"
		      #"\n@Report\n@InitialLanguage{"
		      ,lout-division-lang
		      #"}\n@Author{" AUTHOR
		      #"}\n@DateLine{" DATE
		      #"}\n@ColumnNumber{"
		      ,(lambda (t) (if (member "1C" (xatv t 'FACE))
				       "1}" "2}")))
		     #\newline)))
	  '()))
       c h basic))))

;}}}
;{{{ lout-book

(define (lout-book c h basic)
  ; first enforce read of the whole stream, thereby filling
  ; the cross ref data bases
  (let* ((idx (let ((idx (stream-filter
			  (lambda (t) (eq? (token-type t) 'INDEXDB))
			  basic)))
		(if idx (head idx) '#(INDEXDB ()))))
	 (face (let ((face (xat (head basic) 'FACE)))
		 (if face
		     (arg-val face)
		     '("NIDX" "1C" "2S"))))

	 (lang (car (xatv (head basic) 'LANG)))
	 (cl
	  (letrec
	      ((cl (rdp-cond* (def-repl (rdp-call cl) `(,lout-contents
							,lout-repls1)
				`(,lout-p-data
				  (#((PI OUTPUT)) ,pass-token-action)
				  (#(ENDTAG) ,rdp-leave))))))
	    (rdp-parse (rdp-cond `((#(STARTTAG CONTENTS) ,(rdp-repll '() cl))))
		       ((normalize-contents
			 2 `(LANGUAGE ,(xatv (head basic) 'LANG))) basic)))))
    (letrec
	(
	 (call-contents (rdp-call contents))
	 (contents (rdp-cond*
		    (lout-desc
		     (old-lout-tbl
		      `(,lout-p-data
			(#(ENDTAG))	; same as: ,rdp-leave
			. ,(def-repl
			     (rdp-call contents)
			     `(,lout-repls1
			       ,lout-body
			       ((APPDXS) . ,lout-divs) ,lout-unhacked-divs)
			     `(,@(lout-figure (rdp-call contents))
			       (#(PLACE CONTENTS)
				,(rdp-map
				  (lambda (t) cl) pass-token-action))
			       ,lout-bib
			       (#((PI OUTPUT)) ,pass-token-action)
			       (#((INDEXDB BIBDB)) ,(rdp-skip 1))
			       (#() ,(lambda (c h s)
				       (message 0 #"\nUnhandled: " (head s))
				       (rdp-reduce c h (tail s)))))
			     ))))))
		 
	 )
      ((rdp-cond
	(def-repl contents
	  `(((BOOK
	      (#"\n@SysInclude{bookf}\n"
	       ,(lambda (x) (lout-dl-use 'book (xat x 'FACE)))
	       #"\n@Use { @TypesetLayout }\n@Use { @BookLayout }\n"
	       #"\n@Book\n@InitialLanguage{"
	       ,lout-division-lang
	       #"}\n@Author{" AUTHOR #"}\n@Publisher{" INST
	       #"}\n @AfterTitlePage { //1.1b {}}\n")
	      ,(if
		(member "NIDX" face)
		#\newline
		(lout-make-bypass-index (data-token-data idx))))))
	  '()))
       c h
       (stream-cute
	(start-gi? '(INTRO PART CHAPT))
	(lambda (s) (cons-stream  '#(PLACE CONTENTS) s))
	basic)))))

;}}}

;{{{ lout-general-header (lout-dl-use . opts)

(define lout-general-header

"@SysInclude{fontdefs}
@SysInclude{langdefs}
@SysInclude{eq}
@SysInclude{dl}

#---------------
@SysInclude{fig}
@SysInclude{tab}
#---------------

extend @DocumentLayout
export 
@HorizontalLine @Leaders
@Underlined
\",,\" @Symbol
@ListGapAdd
@ListGap
@SubSectionGap
@TSListLabelWidth
@DescFollow
@TSCode
def @TypesetLayout

@Begin

def @ListGapAdd { 0.3v }
def @ListGap{ 0.8v }
def @SubSectionGap { 1.5v }
def @TSListLabelWidth { @ListLabelWidth }

# What the hell is going on?
# The commented out part is close to the native lout definitions.
# But refuses to work.
# Not sure that Jeff will answer some day...
#def @TSListMarkPlace { @Galley }
#def @TSListPlace { @Galley }
#
#def @TSListRawDef
#    named label {}
#{
#  |0c
#  @TSListLabelWidth @Wide { label &0io }
#  |0c @TSListPlace
#}
#
#def @TSListItemInc into { @TSListPlace&&preceding }
#    named mark {}
#    right text
#{
#  def sendtag into { @TSListMarkPlace&&preceding } { mark }
#  sendtag | text
#}
#
#macro @TSListDef { @TSListRawDef label { @TSListMarkPlace }  }
#macro @TSListItem { @TSListDef @TSListItemInc }

def @DescDDParPlace { @Galley }

def @DescFollowI into { @DescDDParPlace&&preceding }
    right x
{
   x
}

macro @DescFollow { |2f @DescDDParPlace | @DescFollowI }

macro @HorizontalLine { @HLine }

def @Underlined precedence 54 right x
{ @OneRow @HContract @VContract { x //0.2f @HLine } }

def @Symbol right x { {Symbol Base}@Font @Char x }
def \",,\" { @OneRow{{}/0.6fk @Char quotedblright} }

export \"--\"
def @TSCode body x 
{
 def \"--\" { \"--\" }
 {Courier Base }@Font x 
}

def @Leaders {.{@Leaders}}

@End @TypesetLayout
")

(define (lout-dl-use . opts)
  (let* ((face (let loop ((o opts))
		 (if (and (vector? (car o))
			  (eq? (arg-name (car o)) 'FACE))
		     (arg-val (car o))
		     (loop (if (null? (cdr o))
			       '(#(FACE TOKEN ("NIDX" "1C" "2S")))
			       (cdr o))))))
	 (make-index (and (not (member "NIDX" face))
			  (not (member 'NIDX opts))))
	 (twoside (member "2S" face))
	 (columns (if (member "1C" face) "1" "2"))
	 (is-book (memq 'book opts)))
    `(quote "
@Use{ @DocumentLayout
        @InitialFont{ " ,lout-initial-font " }
        @InitialBreak{ " ,lout-initial-break " }
        @ColumnNumber{ " ,columns " }
        @PageHeaders{ " ,(if is-book "Titles" "Simple") " }
        @MakeContents{ " ,(if is-book "Bypass" "No" ) " }
        @MakeReferences{ No  }
        @MakeIndex{ " ,(if make-index
			   (if is-book
			       "Bypass"
			       "Yes")
			   "No") " }
        @PageType{ " ,lout-pagetype " }
        @OddLeftMargin{ " ,(if twoside lout-inner-margin 
			       lout-both-margin) " }
        @OddRightMargin{ " ,(if twoside lout-outer-margin
				lout-both-margin) " }
        @EvenLeftMargin{ " ,(if twoside lout-outer-margin
				lout-both-margin) " }
        @EvenRightMargin{ " ,(if twoside lout-inner-margin
				 lout-both-margin) " }
        @ListGap{ 0.6v }
#        @ParaGap		{  1.30vx	}
# The obove looks ugly, especially within lists. "// @LP " for
# paragraph splitting seems to does the trick a little better.
# But even not good enough... :-(
  } ")))

;}}}

(define (lout-switch s)
  (rdp-parse
   (rdp-cond
    `((#(STARTTAG REPORT) ,(rdp-insert lout-general-header) ,lout-report)
      (#(STARTTAG DOCUMENT) ,(rdp-insert lout-general-header) ,lout-document)
      (#(STARTTAG BOOK) ,(rdp-insert lout-general-header) ,lout-book)
      (#(STARTTAG BRIEF) ,lout-brief)
      ))
   s))

;{{{ (lout-run file-name doc-basename doc-ext)

(define lout-cmd "lout" )
(define lout-postfilter "")

(define (lout-run file-name doc-basename doc-ext)
  (let* ((cmda (list lout-cmd " -I " (car *typeset-lib*) "/target/lout "))
	 (cmdn (list " -o /dev/null -e /dev/null "))
	 (cmdw (if (equal? doc-basename "-")
		   '("") (list " > " doc-basename doc-ext)))
	 (cmdi (list file-name)))
    (if (> *verbosely* 1)
	(begin
	  (for-each
	   (lambda (i) (display i (current-error-port)))
	   (append cmda cmdi (list lout-postfilter) cmdw))
	  (if (not (eqv? 0 (system
			    (apply
			     string-append
			     (append cmda cmdi (list lout-postfilter) cmdw)))))
	      (exit 1)))
	(begin
	  (message 1 "Launching Lout")
	  (if (not (eqv? 0 (system (apply string-append
					  (append cmda cmdn cmdi)))))
	      (begin (message 0 "Backend (lout) failed") (exit 1)))))
    (message 1 "Launching Lout second time")
    (system (apply string-append
		   (append cmda cmdi (list lout-postfilter) cmdw)))
    (if (< *verbosely* 2)
	(for-each
	 delete-file
	 (list file-name "lout.li" (string-append file-name ".ld")
	       (string-append file-name ".ld.ld"))))))

;}}}

(define (lout-prepare o d e)
  (doc-preprocess-hook 'run)
  ((stream-display-diverted)
   (stream-through
    (token-stream o)
    lout-switch
    all-doctypes
    ;(watch #"\nx:")
    )))

(define (lout-prepare-to o d e)
  (if (equal? d "-")
      (lout-prepare o d e)
      (with-output-to-file
	  (string-append d e)
	(lambda ()
	  (lout-prepare o d e)))))

(define (lout-run-over f)
  (lambda (o d e)
    (let ((fn (make-tmp-file-name)))
      (f o fn "")
      (lout-run fn d e))))

(set! compile-function
      (case 1
	((1) lout-prepare-to)
        ((2)
      (lambda (o d e)
	(doc-preprocess-hook 'run)
	((stream-display-diverted)
	 (stream-through
	  (token-stream o)
	  ;lout-switch
	  all-doctypes))))
	((3) compile-function)
))
