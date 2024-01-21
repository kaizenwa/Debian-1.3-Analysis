;{{{ module declaration

(module compile
	(include "compile.sch" "stream.sch")
	(export
	 replacement-alist
	 current-args
	 current-sysid
	 current-file
	 (atv sym)			; deliver the val of arg
	 notations
	 ext-entities
	 (add-ext-entity nm def)
	 (define-ext-entity line)
	 (old-apply-notation notation fn sysid)
	 (apply-notation nn notation fn sysid)
	 (old-refer-entity line)
	 compile-structure
	 (compile-structure-to-string)
	 (make-compiler lines)
	 (compile-to-file compiler outfile)
	 (scan-doctype lines)
	 doc-preface-hook
	 doc-postface-hook
	 doc-date
	 (set-date! args)

	 (make-scaner lines)
	 (inline arg-name t)
	 (inline arg-type t)
	 (inline arg-val  t)
	 (parse-arg string)
	 (inline token-type t)
	 (inline token-gi t)
	 (inline token-args t)
	 (inline data-token-data t)
	 (inline ext-type et)
	 (inline ext-def  et)
	 (inline ext-sysid et)
	 (inline ext-file  et)
	 empty-token
	 (empty-token? t)
	 (token-stream scan)

	 (start-gi? gi)
	 (end-gi? gi)
	 (xat token name)
	 (xatt token name)
	 (xatv token name)

	 (external-handler signal-function)

	 )
	(import
	 (message typeset "typeset.scm")
	 (docpath typeset "typeset.scm")
	 (files "files.scm")
	 (strings "strings.scm")
	 (chrproc "chrproc.scm")
	 (control "control.scm")
	 (stream "stream.scm")
	 ))

;}}}

;{{{ (process-processing-instruction line)

; we need to parse the processing instructions as well:

(define (process-processing-instruction line)
  (let ((len (string-length line)))
    (do ((i 0 (+ i 1)))
	((>= i len) #t)
      (let ((c (process-escape-char (string-ref line i))))
	(if c
	    (write-out c)
	    #t)))))

;}}}
;{{{ ELEMENT parsing

(define replacement-alist '())

(define current-args '())
(define current-sysid "")
(define current-file "")

(define (atv sym)
  (let* ((pp (assv sym current-args))
	 (av (if pp (cdr pp) #f)))
    av))

(define (eval-element compiler line args)
  (let* ((lsym (string->symbol line))
	 (pn (assq  lsym replacement-alist))
	 (proc (lambda (rep s/e)
		 (cond
		  ((vector? rep)
		   (let ((el (vector-ref rep s/e)))
		     (cond
		      ((procedure? el)
		       (set! current-args args)
		       (set! compile-structure (lambda (. dummy)
			     (message 0 "Error: compile-structure called from "
				      (vector-ref '#("open" "close") s/e)
				      " tag of " line ".")))
		       (el args))
		      (else (write-out el)))))))))
    ; translate it
    (if pn
	(let ((rep (cdr pn)))
	  (cond
	   ((procedure? rep ) (set! current-args args) 		      
			      (set! compile-structure compiler)
			      (rep args))
	   (else (proc rep 0)
		 (compiler)
		 (set! pn (assq lsym replacement-alist)) ;might have changed
		 (if pn (proc (cdr pn) 1)))))
	(compiler))))

;}}}

(define notations (list))		; map notation name -> sys id (cmd)

;{{{ (apply-notation nn notation file sysid)

(define (apply-notation nn notation file sysid)
  (letrec ((tmpf (make-tmp-file-name))
	   (output-port (open-output-string))
	   (tr path-to-file-name)
	   (sp (lambda (line)
		 (let ((p (string-find-char line #\% ))
		       (l (string-length line)))
		   (if p
		       (cons (substring line 0 p)
			     (if (= p (- l -1))
				 '("%")
				 (let ((next (string-ref line (+ p 1))))
				   (cons
				    (cond
				     ((char=? next #\f) file)
				     ((char=? next #\F) (tr file))
				     ((char=? next #\s) sysid)
				     ((char=? next #\S) (tr sysid))
				     ((char=? next #\%) "%")
				     (else ""))
				    (sp (substring line (+ p 2) l))))))
		       (list line))))))
    (message 1 "Using notation " nn " for file " file)
    (let* ((cmd (apply string-append (append (sp notation) `(">" ,tmpf))))
	   (nothing (message 3 cmd))
	   (err (system cmd)))
      (if (> err 0)
	  (message 0 "Processing of notation " nn   #\newline
		   " to file " file	         #\newline
		   "SGML system identifier " sysid  #\newline
		   "returned error code " err)))
    (file-cat tmpf output-port)
    (delete-file tmpf)
    (close-output-port output-port)))

(define (old-apply-notation n f s)
  (apply-notation n (cdr (assq n notations)) f s))

;}}}

;{{{ ext-entities (old style)

; map pairs entity name -> #( 'NDATA notation sysid filename )

(define ext-entities (list))

(define (add-ext-entity nm def)
  (set! ext-entities (cons (cons nm def) ext-entities)))

(define (define-ext-entity line)
  (let* ((l (string-length line))
	 (s1 (string-find-char line #\space 0))
	 (s2 (string-find-char line #\space (+ s1 1))))
    (if (not s2) (set! s2 (- l 1)))
    (add-ext-entity (substring line 0 s1)
		    (vector
		     'NDATA
		     (string->symbol
		      (substring line (+ s2 1) l))
		     current-sysid
		     current-file))))

;}}}
;{{{ entity references

; remember the representations of external extities by sysid's

(define entity-cache '())

; Recall the representations of external extities if already known.
; Otherwise apply the notation defined for them.

(define (refer-entity definition notations)
  (if
   (not (vector-ref definition 3)) ; file name
   (begin
     (message 0 "Error: No file found for SYSTEM ID "
	      (vector-ref definition 2) #\.)
     #f)
   (let* ((cached (assoc (vector-ref definition 2) entity-cache))
	  (not-name (vector-ref definition 1))
	  (not-def (assq not-name notations))
	  (file (vector-ref definition 3))
	  (sysid (vector-ref definition 2)))
     (if cached
	 (cdr cached)
	 (if not-def
	     (let ((ext (apply-notation (car not-def) (cdr not-def)
					file sysid)))
	       (set! entity-cache
		     (cons (cons (vector-ref definition 2) ext) entity-cache))
	       ext)
	     (begin
	       (message 0 "Error: Skipping element: file " file #\newline
			"SGML system identifier " sysid
			". Notation " not-name " not defined.")
	       #f))))))

(define (old-refer-entity line)
  (write-out 
   (let ((def (assoc line ext-entities)))
     (if def
	 (refer-entity (cdr def) notations)
	 (message 0 "External entity " line " not defined."))))) 

;}}}

;{{{ old style

;{{{ (make-compiler lines) // old-style

; deliver the list of args with the new pair (name value) prepended

(define (scan-arg args line)
  (let* ((length (string-length line))
	 (s1 (string-find-char line #\space  0))
	 (s2 (string-find-char line #\space (+ s1 1))))
    (if (not s2) (set! s2 (- length 1)))
    (cons (cons 
	   (string->symbol (substring line 0 s1))
	   (process-cdata (substring line (+ s2 1) length)))
    args)))

; parse the whole list made from the input lines

(define (make-compiler lines)
  (letrec
      ((exitf #f)
       (doit (lambda ()
	       (do ((args '()))
		   ((or
		     (null? lines)
		     (let ((cmd (caar lines) (caar lines))
			   (line (cdar lines) (cdar lines)))
		       (set! lines (cdr lines))
		       (case cmd
			((#\) #\}) #t)
			((#\-) (process-text line) #f)
			((#\() (eval-element dispatch line args) #f)
			((#\A) (set! args (scan-arg args line)) #f)
			((#\?)
			 (process-processing-instruction line) #f)
			((#\&) (old-refer-entity line) #f)
			((#\N)
			 (set! notations  (cons (cons (string->symbol line)
						      current-sysid)
						notations )) #f)
			((#\E) (define-ext-entity line) #f)
			((#\s) (set! current-sysid line) #f)
			((#\f) (set! current-file
				     (sgmls-entity-file-name line)) #f)
			((#\C) (exitf #t)))))
		    #f))))
       (dispatch (lambda ( . command)
		   (if (pair? command)
		       (cond
			((eq? (car command) 'exit) (exitf (cdr command)))
			(else (message 0 "compile-command " 
				       (car command) "not understood")))
		       (doit)))))
    (lambda ( . command)
      (if exitf (apply dispatch command)
	  (bind-exit (ex)
		     (set! exitf ex)
		     (apply dispatch command))))))

;}}}
;{{{ (compile-...)

(define compile-structure null-proc)

(define (compile-structure-to-string)
  (capture-out #t)
  (compile-structure)
  (capture-out #f))

(define (compile-to-file compiler outfile)
  (let ((oop output-port))
    (set! output-port
	  (if (string=? outfile "-")
	      (current-output-port)
	      (open-output-file outfile)))
    (doc-preface-hook 'run)
    (let ((pres (compiler)))
      (doc-postface-hook 'run)
      (close-output-port output-port)
      (set! output-port oop)
      pres)))

; scan forward to the doctype declaration and deliver the type

(define (scan-doctype lines)
  (if (pair? lines)
      (if (char=? #\( (caar lines))
	  (string-copy (cdar lines))
	  (scan-doctype (cdr lines)))
      #f))

;}}}
;{{{ some standard (date/language) support

(define doc-preface-hook (make-hook))
(define doc-postface-hook (make-hook))

(define doc-date (date))

(define (set-date! args)
  (let* ((pp (assv 'DATE args))
	 (pd (if pp (cdr pp) (date))))
  (if (not (equal? pd ""))
      (set! doc-date pd))))

;}}}

;}}}
;{{{ sgmls special

(define (sgmls-entity-file-name string)
  (if (eqv? (string-ref string 0) #\<)
      (path-find-file
       docpath
       (substring string
		  (+ (string-find-char string #\>) 1)
		  (string-length string)))
      string))

;}}}
; ---------------------------------------------------------------------------
;{{{ (make-scanner lines)

; maybe some day we need some more complicated scaner stuff...

(define (make-scaner lines)
  (if (null? lines)
      empty-stream
      (cons-stream (caar lines)
		   (cons-stream (cdar lines)
				(make-scaner (cdr lines))))))

;}}}
;{{{ token arguments

(define-inline (arg-name arg-token) (vector-ref arg-token 0))
(define-inline (arg-type arg-token) (vector-ref arg-token 1))
(define-inline (arg-val  arg-token) (vector-ref arg-token 2))

(define (parse-arg s)
  (let* ((length (string-length s))
	 (s1 (string-find-char s #\space 0))
	 (s2x (string-find-char s #\space (+ s1 1)))
	 (s2a (if s2x s2x length))
	 (s2b (if s2x (+ s2x 1) length))
	 (name (string->symbol (substring s 0 s1)))
	 (type (string->symbol (substring s (+ s1 1) s2a)))
	 (rawval (substring s s2b length))
	 (val (case type
		((CDATA) rawval)
		((ID)  (string-split-string rawval " "))
		((TOKEN ENTITY) (string-split-string rawval " "))
		((NOTATION) (string->symbol rawval))
		((IMPLIED) #f)
		(else (message 0 "parse-arg: argument type " type " unknown.")
		      rawval))))
    (vector name type val)))

;}}}
;{{{ token predicates

;;; Bigloo 1.8 apears  to restrict inlined procedures *not* to use cond.
  ; We avoid it for now...

;(define-inline (token-type t)
;  (cond
;   ((vector? t) (vector-ref t 0))
;   (else 'OUTPUT)))
(define-inline (token-type t)
  (if (vector? t)
      (vector-ref t 0)
      'OUTPUT))
(define-inline (token-gi t) (vector-ref t 1))
(define-inline (token-args t) (vector-ref t 2))
;(define-inline (data-token-data t)
;  (cond
;   ((vector? t) (vector-ref t 1))
;   (else t)))
(define-inline (data-token-data t)
  (if (vector? t)
      (vector-ref t 1)
      t))
(define-macro (token-size) 3)

;}}}
;{{{ external token predicates

(define-inline (ext-type e) (vector-ref e 0))
(define-inline (ext-def  e) (vector-ref e 1))
(define-inline (ext-sysid e) (vector-ref e 2))
(define-inline (ext-file e) (vector-ref e 3))

;}}}
;{{{ attribute service

(define (start-gi? gi)
  (lambda (t)
    (and (start-tag? t) (memq (token-gi t) gi))))

(define (end-gi? gi)
  (lambda (t)
    (and (end-tag? t) (memq (token-gi t) gi))))

(define (xat t n)
  (if (not (start-tag? t)) (error "xat" "Not a STARTTAG" t))
  (let loop ((a (token-args t)))
    (cond
     ((null? a) #f)
     ((eq? (arg-name (car a)) n) (car a))
     (else (loop (cdr a))))))

(define (xatv t n)
  (let ((a (xat t n)))
    (if a
	(arg-val a)
	(error "xatv" "Attribute not in token" (cons n t)))))

(define (xatt t n)
  (let ((a (xat t n)))
    (if a
	(arg-type a)
	(error "xatt" "Attribute not in token" (cons n t)))))

;}}}
;{{{ (token-stream scan)

(define empty-token (vector 'EMPTY #f #f))
(define (empty-token? t) (eq? t empty-token))

(define (token-stream-from-stream scan)
  (letrec
      ((sysid #f)
       (file #f)
       (args '())
       (loop (lambda (scan)
	       (if
		(stream-empty? scan)
		empty-stream
		(let ((t (head scan)))
		  (case t
		    ((#\-)
		     (cons-stream
		      (vector 'DATA (head (tail scan)))
		      (loop (tail (tail scan)))))
		    ((#\?)
		     (cons-stream (vector 'PI (head (tail scan)))
				  (loop (tail (tail scan)))))
		    ((#\A) (set! args (cons (parse-arg (head (tail scan)))
					    args))
			   (loop (tail (tail scan))))
		    ((#\()
		     (cons-stream
		      (vector 'STARTTAG
			      (string->symbol (head (tail scan)))
			      args)
		      (token-stream-from-stream (tail (tail scan)))))
		    ((#\))
		     (cons-stream (vector 'ENDTAG
					  (string->symbol (head (tail scan))))
				  (loop (tail (tail scan)))))
		    ((#\s) (set! sysid (head (tail scan)))
			   (loop (tail (tail scan))))
		    ((#\f) (set! file (sgmls-entity-file-name
				       (head (tail scan))))
			   (loop (tail (tail scan))))
		    ((#\E)
		     (let* ((line (head (tail scan)))
			    (l (string-length line))
			    (s1 (string-find-char line #\space 0))
			    (s2a (string-find-char line #\space (+ s1 1)))
			    (s2 (if s2a s2a (- l 1))))
		       (cons-stream
			(vector
			 'External-Definition
			 (string->symbol (substring line 0 s1))
			 (vector
			  (string->symbol (substring line (+ s1 1) s2))
			  (string->symbol (substring line (+ s2 1) l))
			  sysid
			  file))
			(token-stream-from-stream (tail (tail scan))))))
		    ((#\&)
		     (cons-stream
		      (vector 'External-Reference
			      (string->symbol (head (tail scan))))
		      (loop (tail (tail scan)))))
		    ((#\N)
		     (cons-stream
		      (vector 'NOTATION
			      (string->symbol (head (tail scan)))
			      sysid)
		      (loop (tail (tail scan)))))
		    ((#\S)		; drop it, #\{ delivers the same
		     (loop (tail (tail scan))))
		    ((#\{)
		     (cons-stream
		      (vector  'STARTSUBDOC
			       (string->symbol (head (tail scan)))
			       (vector 'SUBDOC
				       ""
				       sysid
				       file))
		      (loop (tail (tail scan)))))
		    ((#\})
		     (cons-stream
		      (vector 'ENDSUBDOC
			      (string->symbol (head (tail scan))))
		      (loop (tail (tail scan)))))
		    ((#f #\C) #f)
		    (else (cons-stream t (loop (tail scan))))))))))
    (loop scan)))

;; this one does not need a stream but the untouched list instead

;;; HACK
;; a local cons-stream which works ONLY for one stream definition
;; i. e., the one without delay.
;; -- differs from the "token-stream-from-stream as far as that one
;; depends on evaluation order, which in turn is fixed through the
;; use of "delay" or "lambda". Here we must use "let".

(define-macro (tsfll-cons-stream h t) `(cons ,h ,t))

(define (token-stream-from-line-list scan)
  (letrec
      ((sysid #f)
       (file #f)
       (args '())
       (loop (lambda (scan)
	       (if
		(null? scan)
		empty-stream
		(let ((t (caar scan)))
		  (case t
		    ((#\-)
		     (tsfll-cons-stream
		      (vector 'DATA (cdar scan))
		      (loop (cdr scan))))
		    ((#\?)
		     (tsfll-cons-stream (vector 'PI (cdar scan))
					(loop (cdr scan))))
		    ((#\A) (set! args (cons (parse-arg (cdar scan))
					    args))
			   (loop (cdr scan)))
		    ((#\()
		     (let ((token `#(STARTTAG
				     ,(string->symbol (cdar scan))
				     ,args)))
		       (tsfll-cons-stream
			token
			(token-stream-from-line-list (cdr scan)))))
		    ((#\))
		     (tsfll-cons-stream (vector 'ENDTAG
						(string->symbol (cdar scan)))
				  (loop (cdr scan))))
		    ((#\s) (set! sysid (cdar scan))
			   (loop (cdr scan)))
		    ((#\f) (set! file (sgmls-entity-file-name (cdar scan)))
			   (loop (cdr scan)))
		    ((#\E)
		     (let* ((line (cdar scan))
			    (l (string-length line))
			    (s1 (string-find-char line #\space 0))
			    (s2a (string-find-char line #\space (+ s1 1)))
			    (s2 (if s2a s2a (- l 1)))
			    (token `#(External-Definition
				      ,(string->symbol (substring line 0 s1))
				      ,(vector
					(string->symbol
					 (substring line (+ s1 1) s2))
					(string->symbol
					 (substring line (+ s2 1) l))
					sysid
					file))))
		       (tsfll-cons-stream
			token
			(token-stream-from-line-list (cdr scan)))))
		    ((#\&)
		     (tsfll-cons-stream
		      (vector 'External-Reference
			      (string->symbol (cdar scan)))
		      (loop (cdr scan))))
		    ((#\N)
		     (let ((token `#(NOTATION
				    ,(string->symbol (cdar scan))
				    ,sysid)))
		       (tsfll-cons-stream
			token
			(loop (cdr scan)))))
		    ((#\S)		; drop it, #\{ delivers the same
		     (loop (cdr scan)))
		    ((#\{)
		     (let ((token (vector  'STARTSUBDOC
					   (string->symbol (cdar scan))
					   (vector 'SUBDOC
						   ""
						   sysid
						   file))))
		       (tsfll-cons-stream
			token
			(loop (cdr scan)))))
		    ((#\})
		     (tsfll-cons-stream
		      (vector 'ENDSUBDOC
			      (string->symbol (cdar scan)))
		      (loop (cdr scan))))
		    ((#f #\C) empty-stream)
		    (else (tsfll-cons-stream t 
				       (tsfll-cons-stream
					(cdar scan)
					(loop (cdr scan)))))))))))
    (loop scan)))

(define (token-stream s) (token-stream-from-line-list s))

;}}}
;{{{ (external-handler signal-function)

(define (external-handler signal-function)
  (define (handler
	   notations			; ((notation . sysid)...)
	   ext-entities			; ((id . #(def))...)
	   )
    (define (appl-f def) (refer-entity def notations))
    (define (refer-f id)
      (let ((ent (assq id ext-entities)))
	(if ent
	    (refer-entity (cdr ent) notations)
	    (begin
	      (signal-function
	       `#(error external-handler
			("External entity undefined: " ,id)))
	      #f))))
    (lambda (token)
      (case (token-type token)
	((NOTATION)
	 (signal-function (vector 'NOTATION
				  (token-gi token) (token-args token)))
	 (handler
	  `((,(token-gi token) . ,(token-args token)) . ,notations)
	  ext-entities))
	((External-Definition)
	 (signal-function (vector 'External-Definition
				  (token-gi token) (token-args token)))
	 (handler
	  notations
	  `((,(token-gi token) . ,(token-args token)) . ,ext-entities)))
	((External-Reference)
	 (signal-function (vector 'External-Reference
				  (token-gi token)
				  (assq (token-gi token) ext-entities)))
	 (vector 'OUTPUT (refer-f (token-gi token))))
	((STARTSUBDOC)
	 (signal-function token)
	 (external-handler signal-function))
	(else
	 (signal-function token)
	 (handler notations ext-entities)))))

  (handler '() '(())))

;}}}

