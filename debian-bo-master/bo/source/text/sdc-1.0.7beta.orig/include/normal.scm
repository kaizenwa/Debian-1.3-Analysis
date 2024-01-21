(load "bin/compile.sch")
(load "bin/stream.sch")
(load "bin/rdp.sch")
(load "include/language.scm")

;{{{ debugging aid

(define (tell . stuff)
  (newline (current-error-port))
  (apply fprint (current-error-port) stuff))

(define (watch msg)
  (lambda (s)
    (stream-dbg-watch msg s)))

;}}}

;{{{ Configuration variables

(define normalize-make-index-section #f)
(define normalize-make-bib-section #t)

;}}}

(define (ints n) (cons-stream n (ints (+ n 1))))

;{{{ subfiles

(define (subfiles-of prefix . postfix)
  (stream-map
   (lambda (n) (apply string-append prefix (number->string n) postfix))
   (ints 0)))

(define (subfiles) (subfiles-of (string-append doc-basename "-")))

;}}}
;{{{ section numbering

;;; format is #( (converters ...) gap header trailer)
;;;   converter yield strings
;;; the id is the reverse list of the actual number

(define (section-num format id)
  (define (coerce-to-string x)
    (if (string? x)
	x
	"/???/"))
  (if
   (not (pair? id))
   #f
   (do ((conv (vector-ref format 0)
	      (if (null? (cdr conv)) (vector-ref format 0) (cdr conv)))
	(id id (cdr id))
	(res (list (vector-ref format 3))))
       ((null? id) (apply string-append
			  (reverse!
			   (cons (vector-ref format 2) res))))
     (set! res (if (pair? (cdr id))
		   (cons (vector-ref format 1)
			 (cons (coerce-to-string ((car conv) (car id)))
			       res))
		   (cons (coerce-to-string ((car conv) (car id))) res))))))

(define (ordinary-seq-format num)
  (section-num `#((,number->string) "." "" "") num))

(define (appdx-seq-format num)
  (let ((A-1 (- (char->integer #\A) 1)))
    (section-num
     `#((,(lambda (i) (string (integer->char (+ A-1 i))))
	 ,number->string
	 ,number->string) "." "" "")
     num)))

;}}}
;{{{ attribute service

(define (is-start-tag? t) (start-tag? t))
(define (is-end-tag? t) (end-tag? t))
(define (is-data-token? t) (data-token? t))
(define (is-pi-token? t) (eq? (token-type t) 'PI))
(define (is-external-token? t)
  (case (token-type t)
    ((External-Reference NOTATION External-Definition
      STARTSUBDOC ENDSUBDOC) #t)
    (else #f)))

(define (cut-matching-tag action s)
  (letrec ((lvl-cut
	    (lambda (level s)
	      (if
	       (stream-empty? s)
	       s
	       (let ((t (head s)))
		 (cond
		  ((start-tag? t)
		   (cons-stream t (lvl-cut (+ level 1) (tail s))))
		  ((end-tag? t)
		   (if (eqv? level 0)
		       (action s)
		       (cons-stream t (lvl-cut (- level 1) (tail s)))))
		  (else (cons-stream t (lvl-cut level (tail s))))))))))
    (if (start-tag? (head s))
	(cons-stream (head s) (lvl-cut 0 (tail s)))
	(lvl-cut 0 s))))

(define (add-no-arg no)
  (lambda (token)
    `#(,(token-type token)
       ,(token-gi token)
       (#(NO TOKEN ,no) . ,(token-args token)))))

;}}}
;{{{ General GI replacers

;{{{ renaming

(define (rename-tag new-name)
  (lambda (t)
    (let* ((size (vector-length t))
	   (r (make-vector size)))
      (do ((i 0 (+ i 1)))
	  ((eqv? i size) (vector-set! r 1 new-name) r)
	(vector-set! r i (vector-ref t i))))))

;}}}

;}}}
;{{{ General RDP

;{{{ (rdp-replc function variables parser)

(define (rdp-replc function variables parser)
  (lambda (c h s)
    ((rdp-repll
      (apply function (head s) (map (h 'get) variables))
      parser)
     c h s)))

;}}}
;{{{ rdp-watch, rdp-watch-rest

(define (rdp-watch-rest msg)
  (lambda (c h s)
    (rdp-reduce c h (stream-dbg-watch msg s))))

(define (rdp-watch . msg)
  (lambda (c h s)
    (rdp-reduce c h
		(begin
		  (apply fprint (current-error-port)
			 `(,@msg "depth " ,(length c) " " ,(head s)))
		  s))))

;}}}
;{{{ (rdpl f . fx)

(define (rdpl-accu f . fx)
  (apply rdp-accu-list f fx))

(define (rdpl-stream f . fx)
  (apply rdp-begin f fx))

(define rdpl (car (hook 'rdpl 'run rdpl-stream)))

;}}}

;{{{ macro: (matching-tail token stream)

(define-macro (matching-tail token stream)
  `(if (and (end-tag? (head ,stream))
	    (eq? (token-gi (head ,stream))
		 (token-gi ,token)))
       (tail ,stream)
       ,stream))

;}}}
;{{{ (mkendtag sym)

(define mkendtag
  (memoize (lambda (sym) `#(ENDTAG ,sym))
	   eq?))

;}}}
;{{{ (rdpp-keep content)

(define (rdpp-keep content)
  (lambda (c h s)
     (let ((token (head s)))
       (cons-stream
	token
	(content
	 (rdp-push
	  (lambda (c h s)
	    (cons-stream
	     (mkendtag (token-gi token))
	     (rdp-reduce c h (matching-tail token s))))
	  c)
	 h (tail s))))))

;}}}
;{{{ (rdpp-rename new-gi content)

(define (rdpp-rename new-gi content)
  (let ((e-tag (mkendtag new-gi)))
    (lambda (c h s)
      (let ((token (head s)))
	(cons-stream
	 (vector 'STARTTAG new-gi (token-args token))
	 (content
	  (rdp-push
	   (lambda (c h s)
	     (cons-stream
	      e-tag
	      (rdp-reduce c h (matching-tail token s))))
	   c)
	  h (tail s)))))))

;}}}

(define (rdp-accept-match token)
  (lambda (c h s) (rdp-reduce c h (matching-tail token s))))

(define (rdp-otherwise-continue c) (cons '#() (rdp-continue c)))

;{{{ pass-xxx

(define (pass-token-action c h s) (cons-stream (head s)
					       (rdp-reduce c h (tail s))))

(define (pass-data-token) (list '#(DATA) pass-token-action))

(define (pass-pi-token)
  `(#(PI) ,(rdp-hmap (lambda(t) (plain-tr-string (data-token-data t))))))

(define (pass-external-token) (list is-external-token? pass-token-action))

(define (pass-empty-element tags)
  (list (start-gi? tags)
	(lambda (c h s)
	  (cons-stream (head s)
		       (cons-stream (head (tail s))
				    (rdp-reduce c h (tail (tail s))))))))

(define (pass-otherwise) (list '#() pass-token-action))

;}}}

;}}}

;{{{ Biblio Data Base

;;; (normal-bib)   -> lambda (cmd$symbol)
;;;   cmd: 'insert -> lambda (token)
;;;   cmd: 'ref    -> lambda (key$string)
;;;   cmd: 'get    -> lambda ()
;;;                   -> (#( no$number tag$string entry$token) ...)

;{{{ Code

(define (normal-bib-db)
  (letrec
      ((bib-db (gen-ns "Biblography data base"))
       (bib-list '())
       (insert (lambda (token)
		 (let ((tag (xatv token 'TAG)))
		   (bib-db 'bind token tag)
		   token)))
       (ref (lambda (key)
	      (let ((pos (member key bib-list)))
		(if pos (list (car pos) (length pos) 'B)
		    (begin
		      (set! bib-list (cons key bib-list))
		      (list (car bib-list) (length bib-list) 'B))))))
       (get (lambda ()
	      (do ((res '())
		   (missed bib-list (cdr missed))
		   (no (length bib-list) (- no 1)))
		  ((null? missed) res)
		(let ((val (bib-db 'lookup (car missed))))
		  (if val
		      (set! res (cons (vector no (car missed) (cdr val))
				      res))
		      (set! res (cons (vector no (car missed)
					      "NOT in Database")
				      res))))))))
    (lambda (cmd)
      (case cmd
	((insert) insert)
	((ref) ref)
	((get) get)
	(else (error "normal-bib-db" "unknown cmd" cmd))))))

;}}}

(define p-bibdata
  (let ((do-bibl
	 `((,(lambda (token bibdb) ((bibdb 'insert) token) #f) BIBDB))))
    (rdp-repll
     '()
     (rdp-cond*
      `((#(STARTTAG BIBL) ,(rdp-repll do-bibl rdp-leave))
	(#(ENDTAG BIBL) ,(rdp-skip 1))
	(#(DATA) ,(rdp-skip 1)))))))

;}}}
;{{{ external

(define promise-manpage-subdoc (delay (load-silent "include/nman.scm")))

(define normalize-as-division
  (define end-div '#(ENDTAG DIVISION))
  (define (s-div t) `#(STARTTAG DIVISION ,(token-args t)))
  (define (s-top t) `#(STARTTAG DIVISION (#(ID IMPLIED #f) . ,(token-args t))))
  (rdp-repll
   `(,s-top ,end-div)
   (letrec
       ((g (rdp-cond*
	    `((#(ENDTAG))
	      (#(STARTTAG (SECT SECT1 SECT2 CHAPT))
	       ,(rdp-repll `(,s-div ,end-div) (rdp-call g)))
	      (#(STARTTAG ABSTRACT)
	       ,(rdp-repll '(#(STARTTAG QUOTE (#(STYLE TOKEN ("DEFAULT"))))
			     #(ENDTAG QUOTE))
			   (rdp-call g)))
	      (#(STARTTAG APPENDIX) ,(rdp-repll '(#f #f) (rdp-call g)))
	      (#(STARTTAG) ,(rdpp-keep (rdp-call g)))
	      ,(pass-otherwise)))))
     g)))

(define (process-external-token content next)
  `((#((NOTATION External-Definition))
     ,(rdp-set-fetch `((EXTERNALS ,(lambda (t e) (e t))))
		      (rdp-skip 1))
     , next)
    (#(External-Reference)
     ,(rdp-repll `((,(lambda (t e) (e t))
		    EXTERNALS))
		 (rdp-pretend 'dummy))
     , next)
    (#(STARTSUBDOC)
     ,(rdp-sgram
       (lambda (s)
	 (cond
	  ((token-in? (head (tail s))
		      '#(STARTTAG (MANPAGE DOCUMENT REPORT)))
	   rdp-leave)
	  ((token-in? (head (tail s)) '#(STARTTAG BIBDATA))
	   (rdp-repll '() p-bibdata))
	  ((token-in? (head (tail s)) '#(ENDSUBDOC))
	   (message 1 "Warning: emtpy subdoc " (token-gi (head s)))
	   (rdp-skip 2))
	  (else
	   (message 0 #"\ntook wrong turn on subdoc " (token-gi (head s))
		    " can't process token " (head (tail s)))
	   (rdp-skip 1))))))
    ))


(define do-external-token
  (lambda (content next)
    (list (pass-external-token))))

(set! do-external-token process-external-token)

;}}}

(define p-data* (rdp-cond* `(,(pass-data-token)
			     ,@(do-external-token (rdp-call p-data*)
						  rdp-leave))))

;{{{ inline

(hook 'external-messages
      'add
      (lambda s
	(if (eq? (vector-ref (car s) 0) 'ERROR)
	    (apply message 0 (vector-ref (car s) 2)))
	s))
	
(define do-inline
  (rdp-sgram
   (lambda (s)
     (letrec ((file #f)
	      (ext-handler #f)
	      (getit (lambda (x)
		       (set! file (head x))
		       x))
	      (get-eh (lambda (x) (set! ext-handler x))))
       (rdp-let
	`((subfiles ,getit ,tail)
	  (EXTERNALS ,get-eh ,identity))
	(rdp-map1
	 (lambda (s)
	   (let* ((sym (string->symbol (string-append "<inline>" file)))
		  (notation (xatv (head s) 'N)))
	     (call-with-output-file
		 file
	       (lambda (port)
		 (stream-for-each
		  (lambda (i)
		    (display (plain-tr-string (data-token-data i)) port))
		  (tail s))))
	     (stream ((ext-handler `#(External-Definition
				      ,sym
				      #(NDATA ,notation ,file ,file)))
		      `#(External-Reference ,sym)))))
	 (rdp-repll `(,identity) p-data*)))))))

(define pl-inline `((#(STARTTAG INLINE) ,do-inline)))

;}}}
;{{{ p-any-element

(define p-any-element
  (let ((parse-any (rdp-call p-any-element)))
    (letrec
      ((cases
	(rdp-cond*
	 `((#(ENDTAG) ,rdp-leave)
	   (#(STARTTAG) ,parse-any)
	   ,@(do-external-token parse-any rdp-leave)
	   ,(pass-otherwise)))))
    (rdpp-keep cases))))

;}}}

;{{{ ENTITIES

;{{{ p-math

(define p-math
  (letrec ((parse (rdp-call doit))
	   (doit (rdp-cond* `((#(STARTTAG (SUB SUP SET))
			       ,(rdpp-keep parse))
			      (#(ENDTAG) ,rdp-leave)
			      ,@(do-external-token parse rdp-leave)
			      ,(pass-otherwise)))))
    (rdpp-keep parse)))

;}}}
;{{{ Refs: pl-refs

(define (pl-refs)
  (define (id-lookup ids id)
    (let ((x (ids 'lookup id)))
      (if x
	  x
	  (begin
	    (message 0 "normalize:ref lookup failed for " id)
	    `(id "???")))))
  (define (do-ref token)
    (let ((tt (case (string->symbol (car (xatv token 't)))
		((X) 'XREF) ((B) 'BREF)
		((M) 'MREF) ((U) 'UREF))))
      `((,(lambda (token ids bibdb)
	    (let* ((id (xatv token 'id))
		   (m (case tt
			((XREF) (ids 'lookup id))
			((MREF UREF) id)
			((BREF) ((bibdb 'ref) id))
			(else #f))))
	      `#(STARTTAG
		 ,tt
		 (#(MARK ,@(if m
			       `(TOKEN ,m)
			       `(PROMISE ,(case tt
					    ((XREF) (delay (id-lookup ids id)))
					    ((BREF) (delay ((bibdb 'ref) id)))
					    (else (delay #f)))))
			 )
		. ,(token-args token)))))
	 IDS BIBDB)
	,(lambda (token) `#(ENDTAG ,tt)))))

  `((#(STARTTAG LABEL)
     ,(rdp-repll `((,(lambda (t id ids)
		       (ids 'bind id (xatv t 'ID))
		       `#(STARTTAG
			   LABEL
			   (#(DIVISION TOKEN ,id)
			    . ,(token-args t))))
		    DIVISION-ID IDS)
		   #(ENDTAG LABEL))
		 (rdp-call p-text)))
    (#(STARTTAG FOOTNOTE) ,(rdpp-keep (rdp-call p-body*)))
    (#(STARTTAG REF) ,(rdp-repll do-ref (rdp-call p-text)))))

;}}}
;{{{ (pl-index . next)

(define pl-index
  (define do-index
    `(,(lambda (token no i)
	 (let ((id (xatv token 'ID))
	       (sub (xat token 'SUB)))
	   (apply (i 'add)
		  (head no)
		  id
		  (if (eq? (arg-type sub) 'IMPLIED)
		      '() (list (arg-val sub)))))
	 `#(STARTTAG INDEX (#(MARK TOKEN ,(head no))
			    . ,(token-args token))))
      INDEX-MARK INDEX))
  (lambda next
    `((#(STARTTAG INDEX) ,(rdp-let `((INDEX-MARK ,identity ,tail))
				   (rdp-repll (list do-index identity)
					      rdp-leave))
			 . ,next))))

;}}}

(define in-text '(STRONG BF IT TT META CODE VAR LANG))

;{{{ p-text

(define (do-foreign token)
  `#(External-Reference
     ,(string->symbol (car (xatv token 'FILE)))))

(define do-ems
  (let ((level 0))
    `(,(lambda (token)
	 (set! level (+ level 1))
	 `#(STARTTAG EM (#(LEVEL TOKEN ,level))))
      ,(lambda (token)
	 (set! level (- level 1))
	 token))))

(define p-text-do-sq
  `((,(lambda (t lang)
	`#(STARTTAG
	   ,(token-gi t)
	   (#(LANG TOKEN ,lang) . ,(token-args t))))
     LANGUAGE)
    , identity))

(define p-text
  (rdp-cond*
   `(,(pass-data-token)
     (#(STARTTAG EM) ,(rdp-repll do-ems (rdp-call p-text)))
     (#(STARTTAG SQ) ,(rdp-repll p-text-do-sq (rdp-call p-text)))
     (#(STARTTAG ,in-text) ,(rdpp-keep (rdp-call p-text)))
     ,@(pl-refs)
     ,@(pl-index)
     (#(STARTTAG MATH) ,p-math)
     ,(pass-empty-element '(NL))
     (#(STARTTAG FOREIGN) ,(rdp-repll (list do-foreign) rdp-leave))
     ,@(do-external-token (rdp-call p-text) rdp-leave)
     ,(pass-pi-token)
     ,@pl-inline
     )))

;}}}

;{{{ p-p

(define (p-try-p-s c h s)
  (let ((st '#(STARTTAG PAR ()))
	(et '#(ENDTAG PAR)))
    (if (stream-empty? s)
	(rdp-reduce c h s)
	(let* ((parsed (p-text
			(rdp-push (lambda (c h s)
				    (cons-stream et (rdp-reduce c h s))) c)
			h
			s)))
	  (cond
	   ((stream-empty? parsed) (rdp-reduce c h s))
	   (else
	    (if (eq? (head parsed) et)
		(rdp-reduce c h s)
		(cons-stream st parsed))))))))

(define (p-try-p-l c h s)
  (let ((st '#(STARTTAG XYL)))
    (if (stream-empty? s)
	(rdp-reduce c h s)
	(let* ((parsed ((rdpl p-text) c h s)))
	  (cond
	   ((stream-empty? parsed) (rdp-reduce c h s))
	   (else
	    (if (null? (head parsed))
		(rdp-reduce c h s)
		(cons-stream (cons st (head parsed)) (tail parsed)))))))))

(define p-try-p p-try-p-s)

(define (p-p-skip-empty-lines s)
  (stream-finde
   (lambda (t)
     (not (and (data-token? t)
	       (equal? (data-token-data t) "\\n"))))
   identity
   s))

(define (p-p c h s)
  (p-body c h (p-p-skip-empty-lines (tail (tail s)))))

;}}}

;{{{ gen-p-list

(define (gen-p-list list-type item-type level-counter)
  (letrec
      ((repls (lambda (n)
		`((,(lambda (t lvl)
		      `#(STARTTAG ,item-type
			 (#(NO TOKEN ,n)
			  #(LEVEL TOKEN ,(head lvl))
			  . ,(token-args t))))
		    ,level-counter)
		  #(ENDTAG ,item-type))))
       (item (lambda (n) (rdp-repll (repls n) (rdp-call p-body*))))
       (items (lambda (n)
		(rdp-iter*
		 (rdp-cond `((#(STARTTAG (ITEM O)) ,(item n))
			     (#(STARTTAG NEWPAGE)
			      ,(rdpp-keep rdp-leave)
			      ,(rdp-call (items n)))
			     ))
		 (delay (items (+ n 1)))))))
    (rdp-let
     `((,level-counter ,tail ,identity))
     (rdpp-rename list-type (items 1)))))

;}}}
;{{{ p-desc

(define p-desc
  (let ((p-body* (rdp-call p-body*)))
    (letrec
	((sequence
	   (rdp-iter*
	    (rdp-cond
	     `((#(STARTTAG DT)
		,(rdpp-keep p-text)
		; The following is a little HACKy: throw away data items
		; and keep going. Normaly the second #(DATA) line should
		; be valid. But then the DD-handling must handle DD tags
		; appearing too late.
		,(rdp-cond*
		  `((#(DATA) ,(rdp-skip 1))
		    (#(DATA) ,(rdp-wrap '#(STARTTAG DD ())
					'#(ENDTAG DD) p-body*))
		    (#(STARTTAG DD) ,(rdpp-keep p-body*))))
		)))
	    (delay sequence))))
      (rdpp-keep sequence))))

;}}}
;{{{ p-figure

(define p-figure
  (let* ((rep (lambda (t no)
		`#(STARTTAG ,(token-gi t)
			    (#(NO TOKEN ,no)
			     . ,(token-args t)))))
	 (repp `(,rep FIGURE-COUNTER)))
    (letrec
	((cc
	  (rdp-call
	   (rdp-cond*
	    `((#(STARTTAG (TABLE GRAPHIC))
	       ,p-any-element)
	      ,@pl-inline
	      (#(STARTTAG FOREIGN)
	       ,(rdp-process (rdp-repll `(,do-foreign) rdp-leave)))
	      . ,(do-external-token cc cc))))))
      (rdp-let
       `((FIGURE-COUNTER ,identity ,tail))
       (rdp-repll
	`((,(lambda (t o-no ids)
	      (let ((no (head o-no)))
		(ids 'bind no (xatv t 'ID))
		(rep t no)))
	   FIGURE-COUNTER IDS)
	  #(ENDTAG FIGURE))
	cc
	(rdp-cond
	 `((#(STARTTAG CAPTION) ,(rdp-repll (list repp identity) p-text))))
	)))))

;}}}

;{{{ p-body*

(define p-body
  (let ((p-body* (rdp-call p-body*)))
    (rdp-cond
     `((#(STARTTAG P) ,p-p)
       (#(STARTTAG (QUOTE NOTE SLIDE LITERATE))
	,(rdpp-keep p-body*))
       (#(STARTTAG (TABLE)) ,p-any-element)
       (#(STARTTAG LIST)  ,(gen-p-list 'LIST 'LISTITEM 'LIST-LEVEL))
       (#(STARTTAG ENUM)  ,(gen-p-list 'ENUM 'ENUMITEM 'ENUM-LEVEL))
       (#(STARTTAG DESC)  ,p-desc)
       (#(STARTTAG (VERB RVERB)) ,(rdpp-rename 'VERB p-data*))
       (#(STARTTAG FIGURE) ,p-figure)
       ,@(pl-index (rdp-call p-body))
       (,@(pass-empty-element '(NL NEWPAGE)) ,(rdp-call p-body))
       ,@(do-external-token p-body* (rdp-call p-body))
       (#(DATA "\\n") ,(lambda (c h s) (p-body c h (tail s))))
       ; Pass the generated tokens to indicate where index and bib goes.
       (#(PLACE) ,pass-token-action)
       (#() ,p-try-p)
       ))))

; OK, now dive deep.

;{{{ p-body*-split

(define (p-body*-split c h s)
  (define split-st '#(STARTTAG SPLIT ()))
  (define split-et '#(ENDTAG SPLIT))
  (p-body   			; one body
   (rdp-push
    (lambda (c h ns)
      (if (eq? s ns)		; nothing read
	  (rdp-reduce c h ns)		; leave
	  (let* ((try (p-body*-split c h ns)) ; what comes next?
		 (t (if (stream-empty? try) #f (head try))))
	    (if (and (start-tag? t)           ; a start tag
		     ; but no recursive call
		     (not (eq? t split-st)))
		(cons-stream		; put the split in front
		 split-st
		 (cons-stream
		  split-et
		  try))
		try))))			; leave as is
    c)
    h s))

;}}}

(define p-body*-simple (rdp-loop p-body))

;{{{ p-body*-frame

(hook 'p-body*-frame-body* 'set-doc!
      "Defines which style (%Body)* within an virtual
<!element body o o (%Body)* >
is used.")

(define p-body*-frame-body* (car (hook 'p-body*-frame-body* 'run
				       p-body*-split)))

(define p-body*-frame-st '#(STARTTAG BODY ()))
(define p-body*-frame-et '#(ENDTAG BODY))

(define (p-body*-frame c h s)
  (let ((try (p-body*-frame-body*
	      (rdp-push
	       (lambda (c h s)
		 (cons-stream p-body*-frame-et (rdp-reduce c h s)))
	       c)
	      h s)))
    (if (eq? (head try) p-body*-frame-et)
	(tail try)
	(cons-stream
	 p-body*-frame-st
	 try))))

;}}}

(hook 'p-body* 'set-doc!
      "Defines which virtual contents definition to use for entity %Body")
      
(define p-body* (car (hook 'p-body* 'run
			   p-body*-frame)))

;}}}

;}}}

;{{{ pretend special sections

(define (pretend-special-sections c h s)
  (let* ((get (h 'get))
	 (say (get 'PHRASE))
	 (bib (((get 'BIBDB) 'get)))
	 (idx (get 'INDEX))
	 (lang `#(LANG TOKEN ( ,(get 'LANGUAGE) )))
	 (mkst (lambda (gi)
		 `#(STARTTAG ,gi (,lang #(ID IMPLIED #f)))))
	 (mk-bib (and normalize-make-bib-section
		      (pair? bib)))
	 (mk-idx (and normalize-make-index-section
		      (not pretend-special-sections-nidx)
		      (pair? (idx 'value)))))
    (apply
     stream-insert
     (rdp-reduce
      c
      h ;((h 'set) `(BIBDB ,(normal-bib-db)) `(INDEX ,(make-index)))
      (apply
       stream-insert s
       `(,@(if mk-bib
	       `(,(mkst 'DIVISION)
		 #(STARTTAG HEADING ())
		 #(DATA ,(say 'bibl))
		 #(ENDTAG HEADING)
		 #(PLACE BIB ,bib)
		 #(ENDTAG DIVISION))
	       `())
	 ,@(if mk-idx
	       `(,(mkst 'DIVISION)
		 #(STARTTAG HEADING ())
		 #(DATA ,(say 'index))
		 #(ENDTAG HEADING)
		 #(PLACE INDEX ,((idx 'sort) string<?))
		 #(ENDTAG DIVISION))
	       `())
	 )))
     `(,@(if (not mk-bib)
	     `(#(BIBDB ,bib))
	     '())
       ,@(if (not mk-idx)
	     `(#(INDEXDB ,((idx 'sort) string<?)))
	     '())))))

(define normalize-pretend-special-sections-done #f)

; HACK HACK:
; keep the nidx-value from the outermost face attribute

(define pretend-special-sections-nidx #f)

(define (gen-p-pretend-special-sections contents)
  `((#(ENDTAG (APPENDIX DOCUMENT REPORT BOOK))
     ,(rdp-sgram
       (lambda (s)
	 (if normalize-pretend-special-sections-done
	    rdp-leave
	    (begin
	      (set! normalize-pretend-special-sections-done #t)
	      (rdp-begin
	       pretend-special-sections
	       contents))))))))

;}}}
;{{{ (gen-p-head hd div-no)

(define (gen-p-head hd div-no)
  (let* ((s-tag (lambda (token)
		  `#(STARTTAG ,hd (#(NO TOKEN ,div-no)
				   . ,(token-args token)))))
	 (e-tag `#(ENDTAG ,hd))
	 (repl (list s-tag e-tag)))
    (rdpl
     (rdp-cond
      `((#(STARTTAG HEADING) ,(rdp-repll repl p-text)) ; rm the tokens
	(#(DATA) ,(rdp-wrap s-tag e-tag p-text)))))))

;}}}
;{{{ (gen-p-div div hd inner-div)

(define (gen-p-div-bind-sd no)
  (lambda (token ids)
    (let ((name (symbol->string (token-gi token))))
      (ids 'bind no name)
      (gen-ns name))))

(define (gen-p-div-mk-div s)
  (rdp-cond
   `((#(STARTTAG (DOCUMENT REPORT))
      ,(rdp-process normalize-as-division))
     (#(STARTTAG MANPAGE)
      ,(rdp-sgram
	(lambda (sd)
	  (force promise-manpage-subdoc)
	  (rdp-process (p-manpage->xxx 'DIVISION '#(ID IMPLIED #f)))))))))

(define (gen-p-div div hd inner-div)
  (let* ((e-div `#(ENDTAG ,div))
	 (terminating (rdp-cond
		       `((#(ENDTAG)) ; should be more exact
				     ; but this would need another parameter
			 (#() ,(rdp-pretend e-div)))))
	 (contents
	  (lambda (div-no)
	    (rdp-let
	     `((DIVISION-ID ,(lambda (t) div-no) ,identity))
	     (rdp-repll			; uncond rm the terminating token
	      `((,(lambda (t ids)
		    (if (not (eq? (xatt t 'ID) 'IMPLIED))
			(ids 'bind div-no (xatv t 'ID)))
		    `#(STARTTAG ,div
				(#(NO TOKEN ,div-no)
				 . ,(token-args t))))
		 IDS)
		,e-div)
	      (gen-p-head hd div-no)
	      (rdpl p-body*)
	      (rdpl (rdp-call (inner-div div-no)))
	      terminating)))))
    (lambda (div-no)
      (rdp-cond
       `((#(STARTTAG) ,(contents div-no))
	 (#(STARTSUBDOC)
	  ,(rdp-let-fetch
	    `((IDS ,(gen-p-div-bind-sd div-no) ,(lambda (t i) i)))
	    (rdp-repll '() ;`(,identity ,identity)
		       (rdp-sgram gen-p-div-mk-div)
		       (contents div-no)))))))))

;}}}
;{{{ (gen-p-divseq tags seq div)

(define gen-p-handle-externals-and-unused-data-lines
  (rdp-cond* `(,@(do-external-token rdp-leave rdp-leave)
	       (#(STARTTAG P) ,(rdp-skip 2))
	       (#(DATA) ,(rdp-skip 1)))))

(define (gen-p-handle-incldiv-g contents)
  `(#((STARTTAG ENDTAG) InclDiv) ,(rdp-skip 1) ,contents))

(define (gen-p-divseq tags seq div)
  (let* ((s-seq `#(STARTTAG ,seq ()))
	 (e-seq `#(ENDTAG ,seq))
	 (open `#(STARTTAG ,tags)))
    (lambda (div-no)
      (let
	  ((what-to-do
	    (rdp-wrap
	     s-seq e-seq
	     (rdp-cond (list (gen-p-handle-incldiv-g rdp-leave)))
	     (rdpl (div (cons 1 div-no)))
	     gen-p-handle-externals-and-unused-data-lines
	     (let loop ((no 2))
	       (rdp-cond `((,open 
			    ,(rdpl (div (cons no div-no)))
			    ,gen-p-handle-externals-and-unused-data-lines
			    ,(rdp-call (loop (+ no 1))))
			   (#(STARTSUBDOC)
			    ,(rdpl (div (cons no div-no)))
			    ,gen-p-handle-externals-and-unused-data-lines
			    ,(rdp-call (loop (+ no 1))))
			   ,@(gen-p-pretend-special-sections
			      (rdp-call (loop no)))
			   ,(gen-p-handle-incldiv-g (rdp-call (loop no)))
			   )))
	     )))
      (rdp-cond*			; * because of mixed content
					; see below
       `((,open ,what-to-do)
	 (#(STARTSUBDOC ) ,what-to-do)
	 ; thow away what comes from the mixed content
	 (#(STARTTAG InclDiv) ,what-to-do)
	 (#(DATA) ,(rdp-skip 1))
	 (#(STARTTAG P) ,(rdp-skip 2))

	 ))))))

;}}}

;{{{ divisions

(define p-sectn (gen-p-div 'SECTN 'H4
			   (lambda (div-no) (p-sectns div-no))))

(define p-sectns (gen-p-divseq `(DIVISION )
			       'SECTNS p-sectn))

(define p-sect2 (gen-p-div 'SECT2 'H3 p-sectns))

(define p-sect2s (gen-p-divseq `(SECT2 DIVISION )
			       'SECT2S p-sect2))

(define p-sect1 (gen-p-div 'SECT1 'H2 p-sect2s))

(define p-sect1s (gen-p-divseq `(SECT1 DIVISION )
			       'SECT1S p-sect1))

(define p-sect (gen-p-div 'SECT 'H1 p-sect1s))

(define p-sects (gen-p-divseq `(SECT DIVISION )
			      'SECTS p-sect))

(define (p-fsect div-no)
  (let ((base (reverse! `(F . ,(cdr (reverse div-no))))))
    (rdp-let `((FIGURE-COUNTER
		,(lambda (old)
		   (stream-map (lambda (h) `(,h . ,base)) (ints 1)))
		,identity))
	     (p-sect div-no))))

(define p-fsects (gen-p-divseq `(SECT DIVISION )
			       'SECTS p-fsect))

;(define p-chapt (gen-p-div 'CHAPT 'H0 p-sects))

;(define p-chapts (gen-p-divseq `(CHAPT DIVISION )
;			       'CHAPTS p-chapt))

(define p0-chapt (gen-p-div 'CHAPT 'H0 p-sects))

(define (p-chapt div-no)
  (let ((base (reverse! `(F . ,(cdr (reverse div-no))))))
    (rdp-let `((FIGURE-COUNTER
		,(lambda (old)
		   (stream-map (lambda (h) `(,h . ,base)) (ints 1)))
		,identity))
	     (p0-chapt div-no))))

(define p-chapts (gen-p-divseq `(CHAPT DIVISION )
			       'CHAPTS p-chapt))

(define p-part (gen-p-div 'PART 'HP p-chapts))

(define p-parts (gen-p-divseq `(PART DIVISION )
			       'PARTS p-part))
; appendix

(define p-appdxn (gen-p-div 'APPDXN 'AH4
			    (lambda (div-no) (p-appdxns div-no))))

(define p-appdxns (gen-p-divseq `(DIVISION )
				'APPDXNS p-appdxn))

(define p-appdx2 (gen-p-div 'APPDX2 'AH3 p-appdxns))

(define p-appdx2s (gen-p-divseq `(SECT2 DIVISION )
				'APPDX2S p-appdx2))

(define p-appdx1 (gen-p-div 'APPDX1 'AH2 p-appdx2s))

(define p-appdx1s (gen-p-divseq `(SECT1 DIVISION )
				'APPDX1S p-appdx1))

(define p-appdx (gen-p-div 'APPDX 'AH1 p-appdx1s))

(define p-appdxs (gen-p-divseq `(SECT DIVISION )
			       'APPDXS p-appdx))

(define p-appendix
  (rdpl
   (rdp-cond
    `((#(STARTTAG APPENDIX) ,(rdp-repll '() (p-appdxs '(A))))))))

;}}}
;{{{

(define normalize-default-face '("2S" "1C" "NIDX"))

(define (filter-face-attribute token)
  `#(STARTTAG
     ,(token-gi token)
     (#(FACE TOKEN ,(car (hook
			  'face 'run
			  (let ((f (xat token 'FACE)))
			    (if f (arg-val f) normalize-default-face)))))
      . ,(token-args token))))
			  
;}}}
;{{{ p-document

(define p-document
  (rdpl
   (rdp-repll
    `(,filter-face-attribute ,identity)
    (rdp-begin
     (rdp-cond
      `((#(STARTTAG HEADING) ,(gen-p-head 'DTTL 'NONE)
			     ,(rdpl (rdp-call p-body*))
			     ,(rdpl (p-sects '(S)))
			     ,p-appendix))
     )))))

;}}}
;{{{ p-manpage

(define p-manpage
  (rdp-sgram
   (lambda (s)
     (force promise-manpage-subdoc)
     ; never ever put these in documents which are manpages at top level!
     (set! normalize-make-index-section #f)
     (set! normalize-make-bib-section #f)

     (if (equal? doc-output "man")
	 (rdp-begin
	  (rdp-process (p-manpage->xxx 'MANPAGE #f))
	  (rdp-repll
	   `(,filter-face-attribute ,identity)
	   (rdp-cond `((#(STARTTAG TITLE) ,(rdpp-keep p-text))))
	   (rdp-cond `((#(STARTTAG SHORT) ,(rdpp-keep p-text))))
	   (p-sects '(S))
	   ))
	 (rdp-begin
	  (rdp-process (p-manpage->xxx 'DOCUMENT #f))
	  p-document)))))

;}}}
;{{{ p-report

(define p-abstract
  (rdpl
   (rdp-cond*
    `((#(STARTTAG ABSTRACT) ,(rdpp-keep (rdp-call p-body*)))))))

(define normalize-report-intro
  (rdpl
   (rdp-repll
    `(,filter-face-attribute ,identity)
    (rdp-begin
     (rdp-cond
      `((#(STARTTAG HEADING) ,(gen-p-head 'RTTL 'NONE) ,p-abstract)))
     (lambda (c h s)
       (let ((try ((rdp-begin p-body* (p-fsects '(S))) c h s)))
	 (if (and (start-tag? (head try))
		  (eq? (token-gi (head try)) 'SECTS))
	     try
	     ((p-fsects '(S))
	      c h
	      (stream-append
	       (stream `#(STARTTAG SECT
				   (#(ID IMPLIED #f)
				    #(LANG TOKEN (,((h 'get) 'LANGUAGE)))))
		       '#(STARTTAG HEADING ())
		       `#(DATA ,(((h 'get) 'PHRASE) 'intro))
		       '#(ENDTAG HEADING))
	       s)))))
     p-appendix
     ))))

(define normalize-report
  (rdpl
   (rdp-repll
    `(,filter-face-attribute ,identity)
    (rdp-cond
     `((#(STARTTAG HEADING) ,(gen-p-head 'RTTL 'NONE)
			    ,p-abstract
			    ,(rdpl (rdp-call p-body*))
			    ,(rdpl (p-sects '(S)))
			    ,p-appendix))))))

;}}}
;{{{ p-book

(define p-book
  (rdpl
   (rdp-repll
    `(,filter-face-attribute ,identity)
    (rdp-cond*
     `((#(STARTTAG HEADING)
	,(gen-p-head 'BTTL 'NONE)
	,(rdpl
	  (rdp-cond* `((#(STARTTAG (INTRO PREFACE)) ,(rdpp-keep p-body*)))))
	,(rdp-cond (list (gen-p-handle-incldiv-g rdp-leave)))
	,gen-p-handle-externals-and-unused-data-lines
	,(rdpl (rdp-cond `((#(STARTTAG PART) ,(p-parts '(P))))))
	,(rdpl (rdp-cond `((#(STARTTAG (CHAPT DIVISION)) ,(p-chapts '(C)))
			   (#(STARTSUBDOC) ,(p-chapts '(C))))))
	,p-appendix))))))

;}}}
;{{{ gen-p-brief

(define gen-p-brief
  (let ((promise (delay (load-silent "include/brief.scm"))))
    (lambda s
      (force promise)
      p-brief)))

;}}}

(define (normalize-switch s)
  (message 1 "Starting...")
  (let* ((bib (normal-bib-db))
	 (idx (make-index))
	 (nidx (member "NIDX"
		       (hook 'face 'run 
			     (let ((a (xat (head s) 'FACE)))
			       (if a
				   (arg-val a)
				   normalize-default-face)))))
	 (lang (let ((l (xat (head s) 'LANG)))
		 (if l l '#(LANG TOKEN ("EN")))))
	 (say (phrase (car (arg-val lang))))
	 )
    (set! pretend-special-sections-nidx nidx)
    ((rdp-cond
      `((#(STARTTAG DOCUMENT) ,p-document)
	(#(STARTTAG REPORT) ,(rdp-call normalize-report))
	(#(STARTTAG BOOK) ,p-book)
	(#(STARTTAG MANPAGE) ,p-manpage)
	(#(STARTTAG BRIEF) ,(rdp-sgram gen-p-brief))
	(#(STARTTAG LINUXDOC)
	 ,(rdp-sgram
	   (lambda (s)
	     (load-silent "include/linuxdoc.scm")
	     (rdp-process linuxdoc->report)))
	 ,(rdp-call normalize-report))
	))
     `(,(lambda (c h s)
	  (if (not (stream-empty? s))
	      (fprint (current-error-port)
		      #"\nNormalize Error: Out of context: " (head s)))
	  empty-stream
	  ))
     (((make-state-env) 'def)
      `(DIVISION-ID ())
      `(IDS ,(gen-ns "Toplevel Id's"))
      `(BIBDB ,bib)
      `(INDEX-MARK ,(ints 1))
      `(ENUM-LEVEL ,(ints -1))
      `(LIST-LEVEL ,(ints -1))
      `(FIGURE-COUNTER ,(stream-map (lambda (h) `(,h F)) (ints 1)))
      `(INDEX ,idx)
      `(LANGUAGE ,(car (arg-val lang)))
      `(PHRASE ,say)
      `(EXTERNALS ,(external-handler
		    (lambda (token)
		      (hook 'external-messages 'run token))))
      `(subfiles ,(subfiles))
      )
     s)))

;{{{ (contents level)

(define (normalize-contents level . variables)
  ; unused stuff ...
  (letrec ((names '((H0 . CH0) (H1 . CH1) (H2 . CH2) (H3 . CH3) (H4 . CH4)
		    (AH1 . CAH1) (AH2 . CAH2) (AH3 . CAH3) (AH4 . CAH4)))
	   (ren (lambda (t)
		  ((rename-tag (cdr (assq (token-gi t) names))) t))))
    (lambda (s)
      (apply
       rdp-parse
       (rdp-wrap
	'#(STARTTAG CONTENTS) '#(ENDTAG CONTENTS)
       (rdp-cond*
	`((#(STARTTAG
	     (HP H0 H1 AH1 
	      . ,(if (> level 1)
		     `(H2 AH2
		       . ,(if (> level 2)
			      `(H3 AH3
				. ,(if (> level 3)
				       `(H4 AH4)
				       '()))
			      '()))
		     '())))
	   ,(rdpp-keep p-text))
	  (#(ENDTAG (BOOK DOCUMENT REPORT MANPAGE)) ,rdp-leave)
	  (#() ,(rdp-skip 1)))))
       s
       variables))))

;}}}

(define (all-doctypes s)
  (car (hook 'normalize-cooked 'run
	     (normalize-switch (car (hook 'normalize-raw 'run s))))))
