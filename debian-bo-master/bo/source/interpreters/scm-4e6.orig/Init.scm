;; Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; As a special exception, the Free Software Foundation gives permission
;; for additional uses of the text contained in its release of GUILE.
;;
;; The exception is that, if you link the GUILE library with other files
;; to produce an executable, this does not by itself cause the
;; resulting executable to be covered by the GNU General Public License.
;; Your use of that executable is in no way restricted on account of
;; linking the GUILE library code into it.
;;
;; This exception does not however invalidate any other reasons why
;; the executable file might be covered by the GNU General Public License.
;;
;; This exception applies only to the code released by the
;; Free Software Foundation under the name GUILE.  If you copy
;; code from other Free Software Foundation releases into a copy of
;; GUILE, as the General Public License permits, the exception does
;; not apply to the code that you add in this way.  To avoid misleading
;; anyone as to the status of such modified files, you must delete
;; this exception notice from them.
;;
;; If you write modifications of your own for GUILE, it is your choice
;; whether to permit this exception to apply to your modifications.
;; If you do not wish that, delete this exception notice.  

;;;; "Init.scm", Scheme initialization code for SCM.
;;; Author: Aubrey Jaffer.

(define (scheme-implementation-type) 'SCM)
(define (scheme-implementation-version) "4e6")

;;; Temporary hack for compatability with older versions.
(define software-type
  (cond ((eq? 'msdos (software-type))
	 (lambda () 'ms-dos))
	(else software-type)))

;;; This definition of PROGRAM-VICINITY is a copy of the definition in
;;; SLIB/require.scm.  It is used here to bootstrap
;;; IMPLEMENTATION-VICINITY and possibly LIBRARY-VICINITY.

(define program-vicinity
  (let ((*vicinity-suffix*
	 (case (software-type)
	   ((AMIGA)	'(#\: #\/))
	   ((MS-DOS WINDOWS ATARIST OS/2)	'(#\\ #\/))
	   ((MACOS THINKC)	'(#\:))
	   ((NOSVE)	'(#\: #\.))
	   ((UNIX COHERENT)	'(#\/))
	   ((VMS)	'(#\: #\])))))
    (lambda ()
      (let loop ((i (- (string-length *load-pathname*) 1)))
	(cond ((negative? i) "")
	      ((memv (string-ref *load-pathname* i) *vicinity-suffix*)
	       (substring *load-pathname* 0 (+ i 1)))
	      (else (loop (- i 1))))))))

(define in-vicinity string-append)

;;; This is the vicinity where this file resides.
(define implementation-vicinity
  (let ((vic (program-vicinity)))
    (lambda () vic)))

;;; (library-vicinity) should be defined to be the pathname of the
;;; directory where files of Scheme library functions reside.

;;; If the environment variable SCHEME_LIBRARY_PATH is undefined, use
;;; (implementation-vicinity) as (library-vicinity).  "require.scm",
;;; the first file loaded from (library-vicinity), can redirect it.

(define library-vicinity
  (let ((library-path (getenv "SCHEME_LIBRARY_PATH")))
    (if library-path (lambda () library-path)
	implementation-vicinity)))

;;; Here for backward compatability
(define scheme-file-suffix
  (case (software-type)
    ((NOSVE) (lambda () "_scm"))
    (else (lambda () ".scm"))))

(set! *features*
      (append '(getenv tmpnam abort transcript with-file
		ieee-p1178 rev4-report rev4-optional-procedures
		hash object-hash delay eval dynamic-wind
		multiarg-apply multiarg/and- logical defmacro
		string-port source current-time)
	      *features*))

(define slib:exit quit)
(define (exec-self)
  (require 'i/o-extensions)
  (execv (execpath) (program-arguments)))

(define (terms)
  (list-file (in-vicinity (implementation-vicinity) "COPYING")))

(define (list-file file)
  (call-with-input-file file
    (lambda (inport)
      (do ((c (read-char inport) (read-char inport)))
	  ((eof-object? c))
	(write-char c)))))

(define (read:eval-feature exp)
  (cond ((symbol? exp)
	 (or (memq exp *features*) (eq? exp (software-type))))
	((and (pair? exp) (list? exp))
	 (case (car exp)
	   ((not) (not (read:eval-feature (cadr exp))))
	   ((or) (if (null? (cdr exp)) #f
		     (or (read:eval-feature (cadr exp))
			 (read:eval-feature (cons 'or (cddr exp))))))
	   ((and) (if (null? (cdr exp)) #t
		      (and (read:eval-feature (cadr exp))
			   (read:eval-feature (cons 'and (cddr exp))))))
	   (else (error "read:sharp+ invalid expression " exp))))))

(define (read:array digit port)
  (define chr0 (char->integer #\0))
  (let ((rank (let readnum ((val (- (char->integer digit) chr0)))
		(if (char-numeric? (peek-char port))
		    (readnum (+ (* 10 val)
				(- (char->integer (read-char port)) chr0)))
		    val)))
	(prot (if (eq? #\( (peek-char port))
		  '()
		  (let ((c (read-char port)))
		    (case c ((#\b) #t)
			  ((#\a) #\a)
			  ((#\u) 1)
			  ((#\e) -1)
			  ((#\s) 1.0)
			  ((#\i) 1/3)
			  ((#\c) 0+i)
			  (else (error "read:array unknown option " c)))))))
    (if (eq? (peek-char port) #\()
	(list->uniform-array rank prot (read port))
	(error "read:array list not found"))))

(define (read:uniform-vector proto port)
  (if (eq? #\( (peek-char port))
      (list->uniform-array 1 proto (read port))
      (error "read:uniform-vector list not found")))

(define (read:sharp c port)
  (define (barf)
    (error "unknown # object" c))
  (case c ((#\') (read port))
	((#\+) (if (read:eval-feature (read port))
		   (read port)
		   (begin (read port) (if #f #f))))
	((#\-) (if (not (read:eval-feature (read port)))
		   (read port)
		   (begin (read port) (if #f #f))))
	((#\b) (read:uniform-vector #t port))
	((#\a) (read:uniform-vector #\a port))
	((#\u) (read:uniform-vector 1 port))
	((#\e) (read:uniform-vector -1 port))
	((#\s) (read:uniform-vector 1.0 port))
	((#\i) (read:uniform-vector 1/3 port))
	((#\c) (read:uniform-vector 0+i port))
	((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	 (read:array c port))
	((#\!) (if (= 1 (line-number))
		   (let skip () (if (eq? #\newline (peek-char port))
				    (if #f #f)
				    (begin (read-char port) (skip))))
		   (barf)))
	(else (barf))))

(define type 'type)			;for /bin/sh hack.

;;;; Here are some Revised^2 Scheme functions:
(define 1+
  (let ((+ +))
    (lambda (n) (+ n 1))))
(define -1+
  (let ((+ +))
    (lambda (n) (+ n -1))))
(define 1- -1+)
(define <? <)
(define <=? <=)
(define =? =)
(define >? >)
(define >=? >=)
(define t #t)
(define nil #f)
(define sequence begin)

(set! apply
      (let ((apply:nconc-to-last apply:nconc-to-last)
	    (@apply @apply))
	(lambda (fun . args) (@apply fun (apply:nconc-to-last args)))))
(define call-with-current-continuation
  (let ((@call-with-current-continuation @call-with-current-continuation))
    (lambda (proc) (@call-with-current-continuation proc))))

;;; VMS does something strange when output is sent to both
;;; CURRENT-OUTPUT-PORT and CURRENT-ERROR-PORT.
(case (software-type) ((VMS) (set-current-error-port (current-output-port))))

;;; OPEN_READ, OPEN_WRITE, and OPEN_BOTH are used to request the proper
;;; mode to open files in.  MS-DOS does carraige return - newline
;;; translation if not opened in `b' mode.

(define OPEN_READ (case (software-type)
		    ((MS-DOS WINDOWS ATARIST) "rb")
		    (else "r")))
(define OPEN_WRITE (case (software-type)
		     ((MS-DOS WINDOWS ATARIST) "wb")
		     (else "w")))
(define OPEN_BOTH (case (software-type)
		    ((MS-DOS WINDOWS ATARIST) "r+b")
		    (else "r+")))
(define (_IONBF mode) (string-append mode "0"))

(define could-not-open #f)

(define (open-input-file str)
  (or (open-file str OPEN_READ)
      (and (procedure? could-not-open) (could-not-open) #f)
      (error "OPEN-INPUT-FILE couldn't open file " str)))
(define (open-output-file str)
  (or (open-file str OPEN_WRITE)
      (and (procedure? could-not-open) (could-not-open) #f)
      (error "OPEN-OUTPUT-FILE couldn't open file " str)))
(define (open-io-file str) (open-file str OPEN_BOTH))

(define close-input-port close-port)
(define close-output-port close-port)
(define close-io-port close-port)

(define (call-with-input-file str proc)
  (let* ((file (open-input-file str))
	 (ans (proc file)))
    (close-input-port file)
    ans))

(define (call-with-output-file str proc)
  (let* ((file (open-output-file str))
	 (ans (proc file)))
    (close-output-port file)
    ans))

(define (with-input-from-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-input-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-output-to-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-output-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-error-to-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-error-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-input-from-file file thunk)
  (let* ((nport (open-input-file file))
	 (ans (with-input-from-port nport thunk)))
    (close-port nport)
    ans))

(define (with-output-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-output-to-port nport thunk)))
    (close-port nport)
    ans))

(define (with-error-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-error-to-port nport thunk)))
    (close-port nport)
    ans))

(if (not (defined? force-output))
    (define (force-output . a) #f))

(define (error . args)
  (define cep (current-error-port))
  (perror "ERROR")
  (errno 0)
  (display "ERROR: " cep)
  (if (not (null? args))
      (begin (display (car args) cep)
	     (for-each (lambda (x) (display #\  cep) (write x cep))
		       (cdr args))))
  (newline cep)
  (force-output cep)
  (abort))

(define set-errno errno)
(define exit quit)

(define (file-exists? str)
  (let ((port (open-file str OPEN_READ)))
    (if port (begin (close-port port) #t)
	#f)))

(define difftime -)
(define offset-time +)

(if (not (memq 'ed *features*))
    (begin
      (define (ed . args)
	(system (apply string-append
		       (or (getenv "EDITOR") "ed")
		       (map (lambda (s) (string-append " " s)) args))))
      (set! *features* (cons 'ed *features*))))

(if (not (defined? output-port-width))
    (define (output-port-width . arg) 80))

(if (not (defined? output-port-height))
    (define (output-port-height . arg) 24))

(if (not (defined? last-pair))
    (define (last-pair l) (if (pair? (cdr l)) (last-pair (cdr l)) l)))

(define (has-suffix? str suffix)
  (let ((sufl (string-length suffix))
	(sl (string-length str)))
    (and (> sl sufl)
	 (string=? (substring str (- sl sufl) sl) suffix))))

(define (identity x) x)
(define slib:error error)
(define slib:tab #\tab)
(define slib:form-feed #\page)
(define slib:eval eval)

;;; Load.
(define load:indent 0)
(define (load:pre file)
  (define cep (current-error-port))
  (cond ((> (verbose) 1)
	 (display
	  (string-append ";" (make-string load:indent #\ ) "loading " file)
	  cep)
	 (set! load:indent (modulo (+ 2 load:indent) 16))
	 (newline cep)))
  (force-output cep))

(define (load:post filesuf)
  (define cep (current-error-port))
  (errno 0)
  (cond ((> (verbose) 1)
	 (set! load:indent (modulo (+ -2 load:indent) 16))
	 (display (string-append ";" (make-string load:indent #\ )
				 "done loading " filesuf)
		  cep)
	 (newline cep)
	 (force-output cep))))

(define (scm:load file . libs)
  (define filesuf file)
  (define hss (has-suffix? file (scheme-file-suffix)))
  (load:pre file)
  (or (and (defined? link:link) (not hss)
	   (or (apply link:link file libs)
	       (and link:able-suffix
		    (let ((fs (string-append file link:able-suffix)))
		      (cond ((not (file-exists? fs)) #f)
			    ((apply link:link fs libs) (set! filesuf fs) #t)
			    (else #f))))))
      (and (null? libs) (try-load file))
      ;;HERE is where the suffix gets specified
      (and (not hss)
	   (begin (errno 0)		; clean up error from TRY-LOAD above
                  (set! filesuf (string-append file (scheme-file-suffix)))
		  (try-load filesuf)))
      (and (procedure? could-not-open) (could-not-open) #f)
      (let () (set! load:indent 0)
	   (error "LOAD couldn't find file " file)))
  (load:post filesuf))
(define load scm:load)
(define slib:load load)

(define (scm:load-source file)
  (define sfs (scheme-file-suffix))
  (define filesuf file)
  (load:pre file)
  (or (and (or (try-load file)
	       ;;HERE is where the suffix gets specified
	       (and (not (has-suffix? file sfs))
		    (begin (set! filesuf (string-append file sfs))
			   (try-load filesuf)))))
      (and (procedure? could-not-open) (could-not-open) #f)
      (error "LOAD couldn't find file " file))
  (load:post filesuf))
(define slib:load-source scm:load-source)

(load (in-vicinity (library-vicinity) "require"))

;;; DO NOT MOVE!  This has to be done after "require.scm" is loaded.
(define slib:load-source scm:load-source)
(define slib:load scm:load)

(cond ((or (defined? dyn:link)
	   (defined? vms:dynamic-link-call)
	   (file-exists? (in-vicinity (implementation-vicinity) "hobbit.tms")))
       (load (in-vicinity (implementation-vicinity) "Link"))))

(cond ((defined? link:link)
       (define (slib:load-compiled . args)
	 (or (apply link:link args)
	     (error "Couldn't link files " args)))
       (provide 'compiled)))

(define (string-upcase str) (string-upcase! (string-copy str)))
(define (string-downcase str) (string-downcase! (string-copy str)))
(define (string-capitalize str) (string-capitalize! (string-copy str)))

(define logical:logand logand)
(define logical:logior logior)
(define logical:logxor logxor)
(define logical:lognot lognot)
(define logical:ash ash)
(define logical:logcount logcount)
(define logical:integer-length integer-length)
(define logical:bit-extract bit-extract)
(define logical:integer-expt integer-expt)

(define (logical:ipow-by-squaring x k acc proc)
  (cond ((zero? k) acc)
	((= 1 k) (proc acc x))
	(else (logical:ipow-by-squaring (proc x x)
					(quotient k 2)
					(if (even? k) acc (proc acc x))
					proc))))

;defmacro from dorai@cs.rice.edu (heavily hacked by jaffer):
(define *defmacros* '())
(define (defmacro? m) (and (assq m *defmacros*) #t))

(define defmacro:transformer
  (lambda (f)
    (procedure->memoizing-macro
      (lambda (exp env)
	(copy-tree (apply f (cdr exp)))))))

(define defmacro
  (let ((defmacro-transformer
	  (lambda (name parms . body)
	    `(define ,name
	       (let ((transformer (lambda ,parms ,@body)))
		 (set! *defmacros* (acons ',name transformer *defmacros*))
		 (defmacro:transformer transformer))))))
    (set! *defmacros* (acons 'defmacro defmacro-transformer *defmacros*))
    (defmacro:transformer defmacro-transformer)))

(define (macroexpand-1 e)
  (if (pair? e) (let ((a (car e)))
		  (cond ((symbol? a) (set! a (assq a *defmacros*))
				     (if a (apply (cdr a) (cdr e)) e))
			(else e)))
      e))

(define (macroexpand e)
  (if (pair? e) (let ((a (car e)))
		  (cond ((symbol? a)
			 (set! a (assq a *defmacros*))
			 (if a (macroexpand (apply (cdr a) (cdr e))) e))
			(else e)))
      e))

(define gentemp
  (let ((*gensym-counter* -1))
    (lambda ()
      (set! *gensym-counter* (+ *gensym-counter* 1))
      (string->symbol
       (string-append "scm:G" (number->string *gensym-counter*))))))

(define defmacro:eval slib:eval)
(define defmacro:load load)

(define (slib:eval-load <filename> evl)
  (if (not (file-exists? <filename>))
      (set! <filename> (string-append <filename> (scheme-file-suffix))))
  (call-with-input-file <filename>
    (lambda (port)
      (let ((old-load-pathname *load-pathname*))
	(set! *load-pathname* <filename>)
	(do ((o (read port) (read port)))
	    ((eof-object? o))
	  (evl o))
	(set! *load-pathname* old-load-pathname)))))

(define (print . args)
  (define result #f)
  (for-each (lambda (x) (set! result x) (write x) (display #\ )) args)
  (newline)
  result)

;;; Autoloads for SLIB procedures.

(define (tracef . args) (require 'trace) (apply tracef args))
(define (trace:tracef . args) (require 'trace) (apply trace:tracef args))
(define (trace-all . args) (require 'debug) (apply trace-all args))
(define (pretty-print . args) (require 'pretty-print)
  (apply pretty-print args))

;;; Macros.

;;; Trace gets redefmacroed when tracef autoloads.
(defmacro trace x
  (if (null? x) '()
      `(begin ,@(map (lambda (x) `(set! ,x (trace:tracef ,x ',x))) x))))
(defmacro break x
  (if (null? x) '()
      `(begin ,@(map (lambda (x) `(set! ,x (break:breakf ,x ',x))) x))))

(defmacro defvar (var val)
  `(if (not (defined? ,var)) (define ,var ,val)))

(cond
 ((defined? stack-trace)

  #+breakpoint-error;; remove this line to enable breakpointing on errors
  (define (error . args)
    (define cep (current-error-port))
    (perror "ERROR")
    (errno 0)
    (display "ERROR: " cep)
    (if (not (null? args))
	(begin (display (car args) cep)
	       (for-each (lambda (x) (display #\  cep) (write x cep))
			 (cdr args))))
    (newline cep)
    (cond ((stack-trace) (newline cep)))
    (display " * Breakpoint established: (continue <val>) to return." cep)
    (newline cep) (force-output cep)
    (require 'debug) (apply breakpoint args))

  (define (user-interrupt . args)
    (define cep (current-error-port))
    (newline cep) (display "ERROR: user interrupt" cep)
    (newline cep)
    (cond ((stack-trace) (newline cep)))
    (display " * Breakpoint established: (continue <val>) to return." cep)
    (newline cep) (force-output cep)
    (require 'debug) (apply breakpoint args))
  ))

;;; ABS and MAGNITUDE can be the same.
(cond ((and (inexact? (string->number "0.0")) (not (defined? exp)))
       (if (defined? usr:lib)
	   (if (usr:lib "m")
	       (load (in-vicinity (implementation-vicinity) "Transcen")
		     (usr:lib "m"))
	       (load (in-vicinity (implementation-vicinity) "Transcen")))
	   (load (in-vicinity (implementation-vicinity) "Transcen"
			      (scheme-file-suffix))))
       (set! abs magnitude)))

(if (defined? array?)
    (begin
      (define uniform-vector? array?)
      (define make-uniform-vector dimensions->uniform-array)
;      (define uniform-vector-ref array-ref)
      (define (uniform-vector-set! u i o)
	(uniform-vector-set1! u o i))
;      (define uniform-vector-fill! array-fill!)
;      (define uniform-vector-read! uniform-array-read!)
;      (define uniform-vector-write uniform-array-write)

      (define (make-array fill . args)
	(dimensions->uniform-array args () fill))
      (define (make-uniform-array prot . args)
	(dimensions->uniform-array args prot))
      (define (list->array ndim lst)
	(list->uniform-array ndim '() lst))
      (define (list->uniform-vector prot lst)
	(list->uniform-array 1 prot lst))
      (define (array-shape a)
	(map (lambda (ind) (if (number? ind) (list 0 (+ -1 ind)) ind))
	     (array-dimensions a)))))

;;;; Initialize statically linked add-ons
(cond ((defined? scm_init_extensions)
       (scm_init_extensions)
       (set! scm_init_extensions #f)))

;;; Use *argv* instead of (program-arguments), to allow option
;;; processing to be done on it.  "ScmInit.scm" must
;;;	(set! *argv* (program-arguments))
;;; if it wants to alter the arguments which BOOT-TAIL processes.
(define *argv* #f)

;;; This loads the user's initialization file, or files named in
;;; program arguments.

(or
 (eq? (software-type) 'THINKC)
 (member "-no-init-file" (program-arguments))
 (member "--no-init-file" (program-arguments))
 (try-load
  (in-vicinity
   (let ((home (getenv "HOME")))
     (if home
	 (case (software-type)
	   ((UNIX COHERENT)
	    (if (char=? #\/ (string-ref home (+ -1 (string-length home))))
		home			;V7 unix has a / on HOME
		(string-append home "/")))
	   (else home))
	 (user-vicinity)))
   "ScmInit.scm"))
 (errno 0))

(if (not (defined? *R4RS-macro*))
    (define *R4RS-macro* #f))
(if (not (defined? *interactive*))
    (define *interactive* #f))

(define (boot-tail)
  (cond ((not *argv*) (set! *argv* (program-arguments))
		      (cond ((provided? 'getopt) (set! *optind* 1)
						 (set! *optarg* #f)))))
  (cond
   ((and (> (length *argv*) 1) (char=? #\- (string-ref (cadr *argv*) 0)))
    (require 'getopt)
;;; (else
;;;  (define *optind* 1)
;;;  (define getopt:opt #f)
;;;  (define (getopt argc argv optstring) #f))

    (let* ((simple-opts "muqvbis")
	   (arg-opts '("a kbytes" "no-init-file" "-no-init-file"
				  "-version" "-help" "p number"
				  "r feature" "f filename" "l filename"
				  "d filename" "c string" "e string"
				  "o filename"))
	   (opts (apply string-append ":" simple-opts
			(map (lambda (o)
			       (string-append (string (string-ref o 0)) ":"))
			     arg-opts)))
	   (argc (length *argv*))
	   (didsomething #f)
	   (moreopts #t)
	   (exe-name (symbol->string (scheme-implementation-type)))
	   (up-name (apply string (map char-upcase (string->list exe-name)))))

      (define (do-thunk thunk)
	(if *interactive*
	    (thunk)
	    (let ((complete #f))
	      (dynamic-wind
	       (lambda () #f)
	       (lambda ()
		 (thunk)
		 (set! complete #t))
	       (lambda () (if (not complete) (quit #f)))))))

      (define (do-string-arg)
	(require 'string-port)
	(do-thunk
	 (lambda ()
	   ((if *R4RS-macro* macro:eval eval)
	    (call-with-input-string
	     (string-append "(begin " *optarg* ")")
	     read))))
	(set! didsomething #t))

      (define (do-load file)
	(do-thunk
	 (lambda ()
	   (cond (*R4RS-macro* (require 'macro) (macro:load file))
		 (else (load file)))))
	(set! didsomething #t))

      (define (usage preopt opt postopt success?)
	(define cep (if success? (current-output-port) (current-error-port)))
	(define indent (make-string 6 #\ ))
	(define i 3)
	(cond ((char? opt) (set! opt (string opt)))
	      ;;((symbol? opt) (set! opt (symbol->string opt)))
	      )
	(display (string-append preopt opt postopt) cep)
	(newline cep)
	(display (string-append "Usage: "
				exe-name
				" [-a kbytes] [-" simple-opts "]") cep)
	(for-each
	 (lambda (o)
	   (display (string-append " [-" o "]") cep)
	   (set! i (+ 1 i))
	   (cond ((zero? (modulo i 4)) (newline cep) (display indent cep))))
	 (cdr arg-opts))
	(display " [-- | -s | -] [file] [args...]" cep) (newline cep)
	(if success? (display success? cep) (exit #f)))

      ;; -a int => ignore (handled by run_scm)
      ;; -c str => (eval str)
      ;; -e str => (eval str)
      ;; -d str => (require 'database-utilities) (open-database str)
      ;; -f str => (load str)
      ;; -l str => (load str)
      ;; -r str => (require str)
      ;; -o str => (dump str)
      ;; -p int => (verbose int)
      ;; -m     => (set! *R4RS-macro* #t)
      ;; -u     => (set! *R4RS-macro* #f)
      ;; -v     => (verbose 3)
      ;; -q     => (verbose 0)
      ;; -i     => (set! *interactive* #t)
      ;; -b     => (set! *interactive* #f)
      ;; -s     => set argv, don't execute first one
      ;; -no-init-file => don't load init file
      ;; --no-init-file => don't load init file
      ;; --help => print and exit
      ;; --version => print and exit
      ;; --     => last option

      (let loop ((option (getopt-- argc *argv* opts)))
	(case option
	  ((#\a)
	   (cond ((> *optind* 3)
		  (usage "scm: option `-" getopt:opt "' must be first" #f))
		 ((or (not (exact? (string->number *optarg*)))
		      (not (<= 1 (string->number *optarg*) 10000)))
		  ;;	This size limit should match scm.c ^^
		  (usage "scm: option `-" getopt:opt
			 (string-append *optarg* "' unreasonable") #f))))
	  ((#\e #\c) (do-string-arg))	;sh-like
	  ((#\f #\l) (do-load *optarg*)) ;(set-car! *argv* *optarg*)
	  ((#\d) (require 'database-utilities)
		 (open-database *optarg*))
	  ((#\o) (require 'dump)
		 (if (< *optind* (length *argv*))
		     (dump *optarg* #t)
		     (dump *optarg*)))
	  ((#\r) (do-thunk (lambda ()
			     (if (and (= 1 (string-length *optarg*))
				      (char-numeric? (string-ref *optarg* 0)))
				 (case (string-ref *optarg* 0)
				   ((#\2) (require 'rev3-procedures)
					  (require 'rev2-procedures))
				   ((#\3) (require 'rev3-procedures))
				   ((#\4) (require 'rev4-optional-procedures))
				   ((#\5) (require 'dynamic-wind)
					  (require 'values)
					  (require 'macro)
					  (set! *R4RS-macro* #t))
				   (else (require (string->symbol *optarg*))))
				 (require (string->symbol *optarg*))))))
	  ((#\p) (verbose (string->number *optarg*)))
	  ((#\q) (verbose 0))
	  ((#\v) (verbose 3))
	  ((#\i) (set! *interactive* #t) ;sh-like
		 (verbose (max 2 (verbose))))
	  ((#\b) (set! didsomething #t)
		 (set! *interactive* #f))
	  ((#\s) (set! moreopts #f)	;sh-like
		 (set! didsomething #t)
		 (set! *interactive* #t))
	  ((#\m) (set! *R4RS-macro* #t))
	  ((#\u) (set! *R4RS-macro* #f))
	  ((#\n) (if (not (string=? "o-init-file" *optarg*))
		     (usage "scm: unrecognized option `-n" *optarg* "'" #f)))
	  ((#\:) (usage "scm: option `-" getopt:opt "' requires an argument" #f))
	  ((#\?) (usage "scm: unrecognized option `-" getopt:opt "'" #f))
	  ((#f) (set! moreopts #f)	;sh-like
		(cond ((and (< *optind* (length *argv*))
			    (string=? "-" (list-ref *argv* *optind*)))
		       (set! *optind* (+ 1 *optind*)))))
	  (else
	   (or (cond ((not (string? option)) #f)
		     ((string-ci=? "no-init-file" option))
		     ((string-ci=? "version" option)
		      (display
		       (string-append exe-name " "
				      (scheme-implementation-version)
				      "
Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
"
				      up-name
				      " may be distributed under the terms of"
				      " the GNU General Public Licence;
certain other uses are permitted as well."
				      " For details, see the file `COPYING',
which is included in the "
				      up-name " distribution.
There is no warranty, to the extent permitted by law.
"
				      ))
		      (cond ((execpath)
			     (display " This executable was loaded from ")
			     (display (execpath))
			     (newline)))
		      (quit #t))
		     ((string-ci=? "help" option)
		      (usage "This is "
			     up-name
			     ", a Scheme interpreter."
			     (string-append
			      "Latest info: "
			      "http://www-swiss.ai.mit.edu/~jaffer/"
			      up-name ".html
"
			      ))
		      (quit #t))
		     (else #f))
	       (usage "scm: unknown option `--" option "'" #f))))

	(cond ((and moreopts (< *optind* (length *argv*)))
	       (loop (getopt-- argc *argv* opts)))
	      ((< *optind* (length *argv*)) ;No more opts
	       (set! *argv* (list-tail *argv* *optind*))
	       (set! *optind* 1)
	       (cond ((not didsomething) (do-load (car *argv*))
					 (set! *optind* (+ 1 *optind*))))
	       (cond ((and (> (verbose) 2)
			   (not (= (+ -1 *optind*) (length *argv*))))
		      (display "scm: extra command arguments unused:"
			       (current-error-port))
		      (for-each (lambda (x) (display (string-append " " x)
						     (current-error-port)))
				(list-tail *argv* (+ -1 *optind*)))
		      (newline (current-error-port)))))
	      ((and (not didsomething) (= *optind* (length *argv*)))
	       (set! *interactive* #t)))))

    (cond ((not *interactive*) (quit))
	  (*R4RS-macro*
	   (require 'repl)
	   (require 'macro)
	   (let* ((oquit quit))
	     (set! quit (lambda () (repl:quit)))
	     (set! exit quit)
	     (repl:top-level macro:eval)
	     (oquit))))
    ;;otherwise, fall into non-macro SCM repl.
    )
   (else
    (begin (errno 0)
	   (set! *interactive* #t)
	   (for-each load (cdr (program-arguments)))))))
