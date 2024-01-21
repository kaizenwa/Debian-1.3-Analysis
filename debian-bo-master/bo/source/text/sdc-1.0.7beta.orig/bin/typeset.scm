; this file became quite large but I like to avoid imports in the loaded
; files

(module typeset
	(main main)
	(import 
	 (strings "strings.scm")
	 (files "files.scm")
	 (control "control.scm")
	 (compile "compile.scm")
	 (rdp "rdp.scm")
	 (stream "stream.scm")
	 (divert-stream "dvrtstrm.scm")
	 (namespace "ns.scm")
	 (chrproc "chrproc.scm")
	 (qsort "qsort.scm" ))
	(export
	 sgml-files sgml-opts
	 sgmls-output
	 doc-output
	 doc-basename doc-ext doc-type
	 doc-preprocess-hook
	 doc-postprocess-hook
	 *-R-option-argument*
	 docpath			; the environment
	 (v-load base dir file)
	 (load-silent file)
	 (warn level . msglist)
	 (message level . what)
	 )
	;BEGIN BIGLOO1.8
	(eval (export-all))
	;END BIGLOO1.8
)

;----------------------------------------------------------------------

(define sgml-files "")			; the files to parse
(define sgml-opts '())			; options to sgmls
(define sgml-outputfile "" )		; the intermediate file

(set! sgml-outputfile (make-tmp-file-name))
(define sgmls-output '())               ; list of lines
(define sgml-subdir #f)			; the subdir where the sgml source lives

(define doc-basename "-")	; the output file
(define doc-ext "" )			; it's extension
(define doc-output #f ) 		; standard output
(define doc-type #f )			; the document type from input
(define doc-dir "./")
(define docpath #f)
(define docpath-from-arguments '())

(define *-R-option-argument* #f)	; What's after the ":" of -R


(define *verbosely* 1)			; write debugging infos 
(define *typeset-lib* #f)               ; where to look for files

(define *typesetrc*
  (let ((home (getenv "HOME")))
    (if home
	(list (string-append home "/.typesetrc"))
	'())))

(define (warn level . something)
  (let ((ww (lambda (a) (display a (current-error-port)))))
  (if (>= *verbosely* level)
      (begin
	(for-each ww 
		  (if (pair? something) something '("Warning")) )
	(ww #\newline)))))

(define (message level . what) (apply hook 'message 'run level what))
(hook 'message 'add warn)

; compose the sgmls command (and it's environment)

(define (sgml-parse delp)
  (let* ((cat (getenv "SGML_CATALOG_FILES"))
	 (envp (getenv "DOCPATH"))
	 (dpe (if envp (string-split-string envp ":") '(".")))
	 (dp (append
	      docpath-from-arguments
	      dpe
	      (if sgml-subdir		; we use the new scheme?
		  (map (lambda (i) (string-append i "/" sgml-subdir))
		       *typeset-lib*)
		  *typeset-lib*)
	     ; HACK SGMLS  ! Don't screw with the HACK comments!
	     '(".")
	     ; END HACK SGMLS
	     ))

	 (sgmls (path-find-file *typeset-lib* "bin/sdc-sgmls"))

	 (catalog (path-find-file *typeset-lib*
				  (if sgml-subdir ; new scheme?
				      (string-append sgml-subdir "/CATALOG")
				      "CATALOG")))
	 (sgml-decl (path-find-file *typeset-lib* "/dtd/sgml-decl.sgml"))

	 ; HACK SGMLS  ! Don't screw with the HACK comments!
	 (hackfile ".target.ent")
	 ; END HACK SGMLS

	 (cmd (string-append 
	       "SGML_CATALOG_FILES=" (if cat cat "") ":"
	       (if catalog catalog " ")
	     
	       ";SGML_PATH="
	       (apply string-append (list-join "/%S:" dp)) "/%S"
	       ";SGML_SEARCH_PATH="
	       (apply string-append (list-join ":" dp))
	       "; export SGML_PATH; export SGML_SEARCH_PATH;" 
	       "export SGML_CATALOG_FILES;"

	       sgmls " " (apply string-append sgml-opts)
	       (if sgml-decl sgml-decl "")
	       " " sgml-files " > " sgml-outputfile)))

    ; HACK SGMLS  ! Don't screw with the HACK comments!

    ; We need to write a file with one ENTITY declaration per -i opt.

    (message 1 "Sorry, must scratch file " hackfile ".")
    (with-output-to-file
     hackfile
     (lambda ()
       (for-each (lambda (i)
		   (for-each display i))
		 (map (lambda (i)
			(if (eqv? (string-ref i 1) #\i)
			    `("<!ENTITY % "
			      ,(substring i 3 (string-length i))
			      #" \"INCLUDE\">\n")
			    '()))
		      sgml-opts))))

    ; END HACK SGMLS

    (set! docpath dp)

    (message 1 #"SGML-Parsing of: " sgml-files )
    (message 2 #"effective command:\n" cmd)
    (let ((err (system cmd)))
      (if (> err 0)
	  (message 1 "Parser returned error code " err)))

    ; HACK SGMLS  ! Don't screw with the HACK comments!
    (delete-file hackfile)
    ; END HACK SGMLS

    ) ; close the let* form

  (let ((port (open-input-file sgml-outputfile))
	(lines (make-queue)))
    ;BEGIN BIGLOO1.7

;    (do ((line (read-line port) (read-line port)))
;	((eof-object? line)
;	 (if delp (delete-file sgml-outputfile))
;	 (queue-value lines))
;      (enqueue lines
;	       (cons (string-ref line 0)
;		     (substring line 1 (string-length line)))))

    ;END BIGLOO1.7
    ;BEGIN BIGLOO1.8

    (do ((cmd (read-char port) (read-char port))
	 (line (read-line port) (read-line port)))
	((eof-object? cmd)
	 (if delp (delete-file sgml-outputfile))
	 (queue-value lines))
      (enqueue lines (cons cmd line)))

    ;END BIGLOO1.8

    ))

(define doc-preprocess-hook (make-hook))
(define doc-postprocess-hook (make-hook))

; scan basename and extension from argument
(define (scan-output-specification fn)
  (if fn
      (let ((fnl (string-length fn))
	    (dot (string-find-lastchar fn #\. )))
	(set! doc-basename (if dot (substring fn 0 dot) fn))
	(set! doc-ext (if dot (substring fn dot fnl) "" )))))

(define (guess-target-format)
  (if (not doc-output)
      (if (equal? doc-ext "")
	  (usage)
	  (set! doc-output (substring doc-ext 1 (string-length doc-ext))))))
	
(define (scan-argv argv)
  (set! argv (cdr argv)) ; forget the procname
  (let ((eat (lambda ()  ; deliver the next arg from the list or #f
	       (let ((res (if (pair? argv) (car argv) #f)))
		 (if res (set! argv (cdr argv))) res))))

    (do ((arg (eat) (eat))) ; Argument parsing
	((not arg) #t)

      (let* ((arglen (string-length arg))
	     (isopt (eqv? #\- (string-ref arg 0))))

	(if isopt

	 (let* ((cmd (if (> arglen 1) (string-ref arg 1) #\-))
		(oa (if (and isopt (> arglen 2)) (substring arg 2 arglen) #f)))

	   (cond
	    ((eqv? cmd #\O) (set! doc-output (if oa oa (eat))))
	    ((eqv? cmd #\V) (set! *verbosely* 
				  (string->number (if oa oa (eat)))))
	    ((eqv? cmd #\D) (set! docpath-from-arguments
				  `(,(if oa oa (eat))
				    . ,docpath-from-arguments)))

	    ((eqv? cmd #\R)
	     (set! *typesetrc* (append *typesetrc* (list (if oa oa (eat))))))

	    ((or (eqv? cmd #\i) (eqv? cmd #\m))
	     (if (not oa) (set! oa (eat)))
	     (if oa
		 (set! sgml-opts
		       (append
			sgml-opts
			(list (string-append "-" (string cmd) " " oa " "))))))

	    ((eqv? cmd #\L) (set! *typeset-lib* 
				  (string-split-string (if oa oa (eat)) ":")))
	    ((eqv? cmd #\o) (scan-output-specification (if oa oa (eat))))))
	    
	 (set! sgml-files (string-append arg " " sgml-files)))))))

; load from lib/dest/type silent or not depending on *verbosely*

(define (load-silent file)
  (with-output-to-file "/dev/null" (lambda () (load file))))

(define (v-load lib dest type)
  (let* ((fn (apply string-append 
	      (if (not (equal? dest "")) `(,dest "/" ,type ".scm")
	      `(type ".scm"))))
	 (rfn (path-find-file lib fn)))
    (if rfn
	(if (> *verbosely* 2)
	    (load rfn)
	    (load-silent rfn))
	(begin (display "Can't find File ") (display fn) (newline)))))

(define (old-compile-scheme sgmls-output doc-basename doc-ext)
  (set! doc-type (scan-doctype sgmls-output))
  (if (not doc-type)
      (begin (display "No doctype found!") (newline) (usage))
      (string-downcase! doc-type))
  
  (v-load *typeset-lib* (string-append "target/" doc-output) doc-type)

  (doc-preprocess-hook 'run)
  (let ((ret (compile-to-file (make-compiler sgmls-output)
			      (string-append doc-basename doc-ext))))
    (doc-postprocess-hook 'run)
    (if ret
	(begin (message 1 "Success") (exit 0))
	(begin (message 1 "Error in Inputfile") (exit 1)))))

(define compile-function
  (lambda ( . dummy)
    (message 0 "Shit, compile-function not initzialized.")))

(define (usage)
  (display "
typeset [options | files]*
Options:
 -O <format>
 -i <parameter entity>
 -m <catalog file>
 -o <output file>
 -D <entity search directory>
 -L <typeset library directoy>
 -R <rc file>
 -V <number>                          # verbose (for debugging)
version @@VERSION@@
")
  (exit 1))

(define (main argv)
  (scan-argv argv)
  (guess-target-format)
  (if (not *typeset-lib*)
      (let ((libts (getenv "TYPESETLIB")))
	(set! *typeset-lib* (if libts
				(string-split-string libts ":")
				'("@@LIBDIR@@")))))
  (for-each
   (lambda (f)
     (let* ((colon (string-find-char f #\:))
	    (file (if colon (substring f 0 colon) f))
	    (arg (if colon (substring f (+ colon 1) (string-length f)) #f))
	    (fn (if (file-exists? file) file
		   (path-find-file *typeset-lib*
				   (string-append "rc/" file)))))
       (set! *-R-option-argument* arg)
       (if fn
	   (if (> *verbosely* 1)
	       (begin
		 (message 2 "loading " fn)
		 (load fn))
	       (load-silent fn)))))
   *typesetrc*)
      
  (v-load *typeset-lib* (string-append "target/" doc-output) "preparse")
  (set! sgmls-output (sgml-parse (< *verbosely* 4)))

  (compile-function sgmls-output doc-basename doc-ext))

(set! compile-function old-compile-scheme)
