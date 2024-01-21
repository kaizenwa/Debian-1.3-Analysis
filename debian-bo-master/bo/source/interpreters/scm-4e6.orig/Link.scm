;; Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
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

;;;; "Link.scm", Compiling and dynamic linking code for SCM.
;;; Author: Aubrey Jaffer.

(define cc:command
  (let ((default "cc -c"))	;-O removed for HP-UX self-compile
    (case (software-type)
      ((unix) (if (memq 'sun-dl *features*)
		  "gcc -g -O -fpic -c" ; If you have problems change -fpic to
		                       ; -fPIC (see GCC info pages).
		  default))
      (else default))))

(define link:command
  (case (software-type)
    (else "cc")))

(define scm:object-suffix
  (case (software-type)
    ((MSDOS VMS) ".OBJ")
    (else (if (provided? 'sun-dl) ".so" ".o"))))

;;; This is an unusual autoload because it should load either the
;;; source or compiled version if present.
(if (not (defined? hobbit))		;Autoload for hobbit
    (define (hobbit . args)
      (require (in-vicinity (implementation-vicinity) "hobbit"))
      (provide 'hobbit)
      (apply hobbit args)))

(define (compile-file file . args)
  (apply hobbit file args)
  (require (in-vicinity (implementation-vicinity) "build"))
  (build-from-whole-argv
   (list "build" "-tdll"
	 (string-append "--compiler-options=-I" (implementation-vicinity))
	 "-c"
	 (string-append (descmify file) ".c")
	 ;; or (replace-suffix file (scheme-file-suffix) ".c")
	 "-hsystem"
	 )))

(define (link-named-scm name . modules)
  (require (in-vicinity (implementation-vicinity) "build"))
  (let* ((iv (implementation-vicinity))
	 (oss (string-append scm:object-suffix " "))
	 (command
	  (list "build" "--type=exe" "-cscm.c" "-hsystem"
		(string-append "--linker-options=-L" (implementation-vicinity))
		(apply string-append
		       "-i"
		       (map (lambda (n)
			      (string-append "init_" n))
			    modules))
		(apply string-append
		       "-j"
		       (map (lambda (n)
			      (string-append n oss)) modules))
		"-o" name)))
    (cond ((>= (verbose) 3)
	   (write command) (newline)))
    (build-from-whole-argv command)))

;;;; Dynamic linking/loading

(cond
 ((defined? dyn:link)
  (define link:modules '())
  (define link:able-suffix 
    (cond ((provided? 'shl) ".sl")
	  ((provided? 'sun-dl) ".so")
	  (else ".o")))
  (define link:link
    (lambda (file . libs)
      (define oloadpath *load-pathname*)
      (let* ((sl (string-length file))
	     (lasl (string-length link:able-suffix))
	     (*vicinity-suffix*
	      (case (software-type)
		((NOSVE)	'(#\: #\.))
		((AMIGA)	'(#\: #\/))
		((UNIX)	'(#\/))
		((VMS)	'(#\: #\]))
		((MSDOS ATARIST OS/2)	'(#\\))
		((MACOS THINKC)	'(#\:))))
	     (fname (let loop ((i (- sl 1)))
		      (cond ((negative? i) file)
			    ((memv (string-ref file i) *vicinity-suffix*)
			     (substring file (+ i 1) sl))
			    (else (loop (- i 1))))))
	     (nsl (string-length fname))
	     (name (cond ((< nsl lasl) fname)
			 ((string-ci=? (substring fname (- nsl lasl) nsl)
				       link:able-suffix)
			  (substring fname 0 (- nsl lasl)))
			 (else fname)))
	     (linkobj #f))
	(set! *load-pathname* file)
	(set! linkobj (assoc name link:modules))
	(cond (linkobj (dyn:unlink (cdr linkobj))))
	(set! linkobj (dyn:link file))
	(for-each (lambda (lib)
		    (cond ((dyn:link lib))
			  (else (slib:error "couldn't link: " lib))))
		  libs)
	(cond ((not linkobj)
	       (set! *load-pathname* oloadpath) #f)
	      ((dyn:call
		(string-append
		 "init_" (list->string (map char-downcase (string->list name))))
		linkobj)
	       (set! link:modules (acons name linkobj link:modules))
	       (set! *load-pathname* oloadpath) #t)
	      (else
	       (dyn:unlink linkobj)
	       (set! *load-pathname* oloadpath) #f)))))))

(cond
 ((defined? vms:dynamic-link-call)
  (define link:able-suffix #f)
  (define (link:link file)
    (define dir "")
    (define fil "")
    (let loop ((i (- (string-length file) 1)))
      (cond ((negative? i) (set! dir file))
	    ((memv (string-ref file i) '(#\: #\]))
	     (set! dir (substring file 0 (+ i 1)))
	     (set! fil (substring file (+ i 1) (string-length file))))
	    (else (loop (- i 1)))))
    (vms:dynamic-link-call dir fil (string-append "init_" fil)))))

(set! *catalog*
      (acons 'scmhob (in-vicinity (implementation-vicinity) "scmhob")
	     *catalog*))
(and (defined? *catalog*) (defined? link:link)
     (cond ((provided? 'dld:dyncm)
	    (define (usr:lib lib)
	      (or (and (member lib '("c" "m"))
		       (let ((sa (string-append "/usr/lib/lib" lib ".sa")))
			 (and (file-exists? sa) sa)))
		  (string-append "/usr/lib/lib" lib ".a")))
	    (define (x:lib lib) (string-append "/usr/X11/lib/lib" lib ".sa")))
	   ((provided? 'sun-dl)
	    ;; These libraries are (deferred) linked in conversion to ".so"
	    (define (usr:lib lib) #f)
	    (define (x:lib lib) #f))
	   ((provided? 'shl)
	    (define (usr:lib lib)
	      (if (member lib '("c" "m"))
		  (string-append "/lib/lib" lib link:able-suffix)
		  (string-append "/usr/lib/lib" lib link:able-suffix)))
	    (define (x:lib lib) (string-append "/usr/X11R5/lib/lib"
					       lib link:able-suffix)))
	   (else
	    (define (usr:lib lib) (string-append "/usr/lib/lib" lib ".a"))
	    (define (x:lib lib) (string-append "/usr/X11/lib/lib" lib ".sa"))))
     (begin
       (define wb:vicinity (string-append (implementation-vicinity) "../wb/"))
       (define (catalog:add-link feature ofile . libs)
	 (define fe (file-exists? ofile))
	 (cond ((or (not (require:feature->path feature)) fe)
		;; remove #f from libs list
		(set! libs (let rem ((l libs))
			     (cond ((null? l) l)
				   ((car l) (cons (car l) (rem (cdr l))))
				   (else (rem (cdr l))))))
		(set! *catalog*
		      (acons feature (cons 'compiled (cons ofile libs))
			     *catalog*))
		fe)
	       (else #f)))
       (set! *catalog*
	     (acons 'wb-table (in-vicinity wb:vicinity "wbtab") *catalog*))
       (catalog:add-link 'db
			 (in-vicinity wb:vicinity "db" link:able-suffix)
			 (in-vicinity wb:vicinity "handle" link:able-suffix)
			 (in-vicinity wb:vicinity "blink" link:able-suffix)
			 (in-vicinity wb:vicinity "prev" link:able-suffix)
			 (in-vicinity wb:vicinity "ent" link:able-suffix)
			 (in-vicinity wb:vicinity "sys" link:able-suffix)
			 (in-vicinity wb:vicinity "del" link:able-suffix)
			 (in-vicinity wb:vicinity "stats" link:able-suffix)
			 (in-vicinity wb:vicinity "blkio" link:able-suffix)
			 (in-vicinity wb:vicinity "scan" link:able-suffix)
			 (usr:lib "c"))
       (set! *catalog* (cons '(wb . db) *catalog*))
       (catalog:add-link 'turtle-graphics
			 (in-vicinity (implementation-vicinity) "turtlegr"
				      link:able-suffix)
			 (x:lib "X11")
			 (usr:lib "m")
			 (usr:lib "c"))
       (catalog:add-link 'curses
			 (in-vicinity (implementation-vicinity) "crs"
				      link:able-suffix)
			 (usr:lib "ncurses")
			 ;;(usr:lib "curses")
			 ;;(usr:lib "termcap")
			 (usr:lib "c"))
       (catalog:add-link 'edit-line
			 (in-vicinity (implementation-vicinity) "edline"
				      link:able-suffix)
			 (usr:lib "edit")
			 (usr:lib "termcap")
			 (usr:lib "c"))
       (catalog:add-link 'regex
			 (in-vicinity (implementation-vicinity) "rgx"
				      link:able-suffix)
			 (usr:lib "c"))
       (catalog:add-link 'unix
			 (in-vicinity (implementation-vicinity) "unix"
				      link:able-suffix)
			 (in-vicinity (implementation-vicinity) "ioext"
				      link:able-suffix)
			 (usr:lib "c"))
       (catalog:add-link 'posix
			 (in-vicinity (implementation-vicinity) "posix"
				      link:able-suffix)
			 (usr:lib "c"))
       (catalog:add-link 'socket
			 (in-vicinity (implementation-vicinity) "socket"
				      link:able-suffix)
			 (usr:lib "c"))
       (cond ((catalog:add-link 'i/o-extensions
				(in-vicinity (implementation-vicinity) "ioext"
					     link:able-suffix)
				(usr:lib "c"))
	      (set! *catalog* (append '((line-i/o . i/o-extensions)
					(pipe . i/o-extensions))
				      *catalog*))))
       (cond ((catalog:add-link 'rev2-procedures
				(in-vicinity (implementation-vicinity) "sc2"
					     link:able-suffix))
	      (set! *catalog* (cons '(rev3-procedures . rev2-procedures)
				    *catalog*))))
       (catalog:add-link 'record
			 (in-vicinity (implementation-vicinity) "record"
				      link:able-suffix))
       (catalog:add-link 'generalized-c-arguments
			 (in-vicinity (implementation-vicinity) "gsubr"
				      link:able-suffix))
       (catalog:add-link 'array-for-each
			 (in-vicinity (implementation-vicinity) "ramap"
				      link:able-suffix))
       ))
