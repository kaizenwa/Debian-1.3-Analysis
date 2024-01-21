#!/bin/sh
type;exec scmlit -f $0 -e"(bi)" build $*
;;; "build.scm" Build database and program	-*-scheme-*-
;;; Copyright (C) 1994, 1995, 1996 Aubrey Jaffer.
;;; See the file `COPYING' for terms applying to this program.

(require 'getopt)
(require 'parameters)
(require 'database-utilities)

;;;(define build (create-database "buildscm.scm" 'alist-table))
(define build (create-database #f 'alist-table))

(require 'batch)
(batch:initialize! build)

(define-tables build

  '(file-formats
    ((format symbol))
    ()
    ((plaintext)
     (c-source)
     (c-header)
     (scheme)
     (vax-asm)
     (cray-asm)
     (makefile)
     (MS-DOS-batch)
     (nroff)
     (texinfo)))

  '(file-categories
    ((category symbol))
    ((documentation string))
    ((documentation "Documentation file (or source for)")
     (required "File required for building executable SCM")
     (optional "File required for some feature")
     (linkable "File whose object can be dynamically linked")
     (test "File to test SCM")
     (none "No files")))

  '(build-whats
    ((name symbol))
    ((class file-categories)
     (c-proc symbol)
     (o-proc symbol)
     (spec expression)
     (documentation string))
    ((exe required compile-c-files link-c-program #f
	  "executable program")
     (lib required compile-c-files make-archive ((define "RTL"))
	  "library module")
     (dlls linkable compile-dll-c-files make-dll-archive ((define "RTL"))
	   "archived dynamically linked library object files")
     (dll none compile-dll-c-files make-nothing #f
	  "dynamically linked library object file")))

  '(manifest
    ((file string))
    ((format file-formats)
     (category file-categories)
     (documentation string))
    (("README"	plaintext	documentation	"contains a MANIFEST, INSTALLATION INSTRUCTIONS, hints for EDITING SCHEME CODE, and a TROUBLE SHOOTING GUIDE.")
     ("COPYING"	plaintext	documentation	"details the LACK OF WARRANTY for SCM and the conditions for distributing SCM.")
     ("scm.1"	nroff	documentation	"unix style man page.")
     ("scm.doc"	plaintext	documentation	"man page generated from scm.1.")
     ("QUICKREF"	plaintext	documentation	"Quick Reference card for R4RS and IEEE Scheme.")
     ("scm.texi"	Texinfo	documentation	"SCM installation and use.")
     ("ChangeLog"	plaintext	documentation	"changes to SCM.")
     ("r4rstest.scm"	Scheme	test	"tests conformance with Scheme specifications.")
     ("example.scm"	Scheme	test	"example from R4RS which uses inexact numbers.")
     ("pi.scm"	Scheme	test	"computes digits of pi [type (pi 100 5)].  Test performance against pi.c.")
     ("pi.c"	c-source	test	"computes digits of pi [cc -o pi pi.c;time pi 100 5].")
     ("bench.scm"	Scheme	test	"computes and records performance statistics of pi.scm.")
     ("Makefile"	Makefile	required	"builds SCMLIT using the `make' program.")
     ("build.scm"	Scheme	required	"database for compiling and linking new SCM programs.")
     ("build.bat"	MS-DOS-batch	optional	"invokes build.scm for MS-DOS")
     ("setjump.mar"	Vax-asm	optional	"provides setjmp and longjmp which do not use $unwind utility on VMS.")
     ("setjump.s"	Cray-asm	optional	"provides setjmp and longjmp for the Cray YMP.")
     ("Init.scm"	Scheme	required	"Scheme initialization.")
     ("Transcen.scm"	Scheme	required	"inexact builtin procedures.")
     ("Link.scm"	Scheme	required	"compiles and dynamically links.")
     ("scmfig.h"	c-header	required	"contains system dependent definitions.")
     ("patchlvl.h"	c-header	required	"patchlevel of this release.")
     ("setjump.h"	c-header	required	"continuations, stacks, and memory allocation.")
     ("continue.h"	c-header	required	"continuations.")
     ("continue.c"	c-source	required	"continuations.")
     ("scm.h"	c-header	required	"data type and external definitions of SCM.")
     ("scm.c"	c-source	required	"top level, interrupts, and non-IEEE utility functions.")
     ("findexec.c"	c-source	required	"find the executable file function.")
     ("time.c"	c-source	required	"functions dealing with time.")
     ("repl.c"	c-source	required	"error, read-eval-print loop, read, write and load.")
     ("scl.c"	c-source	required	"inexact arithmetic")
     ("eval.c"	c-source	required	"evaluator, apply, map, and foreach.")
     ("sys.c"	c-source	required	"call-with-current-continuation, opening and closing files, storage allocation and garbage collection.")
     ("subr.c"	c-source	required	"the rest of IEEE functions.")
     ("unif.c"	c-source	required	"uniform vectors.")
     ("rope.c"	c-source	required	"C interface functions.")
     ("ramap.c"	c-source	optional	"array mapping")
     ("dynl.c"	c-source	optional	"dynamically load object files.")
     ("sc2.c"	c-source	linkable	"procedures from R2RS and R3RS not in R4RS.")
     ("rgx.c"	c-source	linkable	"string regular expression match.")
     ("crs.c"	c-source	linkable	"interactive terminal control.")
     ("split.scm"	Scheme	test	"example use of crs.c.  Input, output, and diagnostic output directed to separate windows.")
     ("edline.c"	c-source	linkable	"Gnu readline input editing (get ftp.sys.toronto.edu:/pub/rc/editline.shar).")
     ("Iedline.scm"	Scheme	optional	"Gnu readline input editing.")
     ("record.c"	c-source	linkable	"proposed `Record' user definable datatypes.")
     ("gsubr.c"	c-source	linkable	"make_gsubr for arbitrary (< 11) arguments to C functions.")
     ("ioext.c"	c-source	linkable	"system calls in common between PC compilers and unix.")
     ("posix.c"	c-source	linkable	"posix library interface.")
     ("unix.c"	c-source	linkable	"non-posix system calls on unix systems.")
     ("socket.c"	c-source	linkable	"BSD socket interface.")
     ("pre-crt0.c"	c-source	optional	"loaded before crt0.o on machines which do not remap part of the data space into text space in unexec.")
     ("ecrt0.c"	c-source	optional	"standard Vax 4.2 Unix crt0.c cannot be used because it makes `envron' an initialized variable.")
     ("gmalloc.c"	c-source	optional	"Gnu malloc().")
     ("unexec.c"	c-source	optional	"Convert a running program into an a.out file.")
     ("unexelf.c"	c-source	optional	"Convert a running ELF program into an a.out file.")
     )))

(for-each (build 'add-domain)
	  '((optstring #f (lambda (x) (or (not x) (string? x))) string #f)
	    (filename #f #f string #f)
	    (build-whats #f #f symbol #f)))

(define-tables build

  '(processor-family
    ((family atom))
    ((also-runs processor-family))
    ((*unknown* #f)
     (8086 #f)
     (acorn #f)
     (cray #f)
     (hp-risc #f)
     (i386 8086)
     (m68000 #f)
     (m68030 m68000)
     (mips #f)
     (nos/ve #f)
     (pdp-10 #f)
     (pdp-11 #f)
     (pdp-8 #f)
     (powerpc #f)
     (pyramid #f)
     (sequent #f)
     (sparc #f)
     (tahoe #f)
     (vax pdp-11)
     ))

  '(platform
    ((name symbol))
    ((processor processor-family)
     (operating-system operating-system)
     (compiler symbol))
    ((*unknown* *unknown* unix *unknown*)
     (acorn-unixlib acorn *unknown* *unknown*)
     (aix powerpc aix *unknown*)
     (amiga-aztec m68000 amiga aztec)
     (amiga-dice-c m68000 amiga dice-c)
     (amiga-sas/c-5.10 m68000 amiga sas/c)
     (atari-st-gcc m68000 atari.st gcc)
     (atari-st-turbo-c m68000 atari.st turbo-c)
     (borland-c-3.1 8086 ms-dos borland-c)
     (djgpp i386 ms-dos gcc)
     (gcc *unknown* unix gcc)
     (highc.31 i386 ms-dos highc)
     (hp-ux hp-risc hp-ux *unknown*)
     (linux-aout i386 linux gcc)
     (linux i386 linux gcc)
     (microsoft-c 8086 ms-dos microsoft-c)
     (microsoft-c-nt i386 ms-dos microsoft-c)
     (microsoft-quick-c 8086 ms-dos microsoft-quick-c)
     (ms-dos 8086 ms-dos *unknown*)
     (os/2-cset i386 os/2 C-Set++)
     (os/2-emx i386 os/2 gcc)
     (sun sparc sun-os *unknown*)
     (svr4 *unknown* unix *unknown*)
     (turbo-c-2 8086 ms-dos turbo-c)
     (unicos cray unicos *unknown*)
     (unix *unknown* unix *unknown*)
     (vms vax vms *unknown*)
     (vms-gcc vax vms gcc)
     (watcom-9.0 i386 ms-dos watcom)
     ))

  '(C-libraries
    ((library symbol)
     (platform platform))
    ((compiler-flags string)
     (link-lib-flag string)
     (lib-path optstring)
     (supress-files expression))

    ((m *unknown* "" "-lm" "/usr/lib/libm.a" ())
     (c *unknown* "" "-lc" "/usr/lib/libc.a" ())
     (regex *unknown* "" "-lrgx" "/usr/lib/librgx.a" ())
     (curses *unknown* "" "-lcurses" "/usr/lib/libcurses.a" ())
     (graphics *unknown* "-I/usr/X11/include -DX11" "-lX11"
	       "/usr/X11/lib/libX11.sa" ())
     (editline *unknown* "" "-ledit" "/usr/lib/libedit.a" ())
     (termcap *unknown* "" "-ltermcap" "/usr/lib/libtermcap.a" ())
     (debug *unknown* "-g" "-g" #f ())

     (m linux-aout "" "-lm" "/usr/lib/libm.sa" ())
     (c linux-aout "" "-lc" "/usr/lib/libc.sa" ())
     (dlll linux-aout "-DDLD -DDLD_DYNCM" "-ldld" #f ("findexec.c"))
     (regex linux-aout "" "" "" ())
     (curses linux-aout "-I/usr/include/ncurses" "-lncurses"
	     "/usr/lib/libncurses.a" ())
     (nostart linux-aout "" "-nostartfiles" #f ("ecrt0.c"))
     (dump linux-aout "" "/usr/lib/crt0.o" #f ("unexelf.c"))

     (m linux "" "-lm" "/lib/libm.so" ())
     (c linux "" "-lc" "/lib/libc.so" ())
     (dlll linux "-DSUN_DL" "-ldl" #f ())
     (graphics linux "-I/usr/include/X11 -DX11" "-L/usr/X11R6/lib -lX11"
	       "/usr/X11R6/lib/libX11.so" ())
     (curses linux "" "-lcurses" "/lib/libncurses.so" ())
     (nostart linux "" "" #f ("pre-crt0.c" "ecrt0.c"))
     (dump linux "" "" #f ("unexec.c"))

     (m acorn-unixlib "" "" #f ())

     (m amiga-dice-c "" "-lm" #f ())
     (m amiga-SAS/C-5.10 "" "lcmieee.lib" #f ())
     (c amiga-SAS/C-5.10 "" "lc.lib" #f ())

     (m vms-gcc "" "" #f ())
     (m vms "" "" #f ())

     (m atari-st-gcc "" "-lpml" #f ())
     (m atari-st-turbo-c "" "" #f ())

     (m sun "" "-lm" #f ())
     (dlll sun "-DSUN_DL" "-ldl" #f ())
     (nostart sun "" "-e __start -nostartfiles -static" #f ("pre-crt0.c"))
     (dump sun "" "" #f ("unexec.c"))

     (m hp-ux "" "-lm" #f ())
     (dlll hp-ux "-DHAVE_DYNL" "-Wl,-E -ldld" #f ())
     (graphics hp-ux "-DX11" "-lX" "/usr/lib/X11R5/libX11.sl" ())

     (c djgpp "" "-lc" #f ("findexec.c"))
     (curses djgpp "-I/djgpp/contrib/pdcurses/include/"
	     "-L/djgpp/contrib/pdcurses/lib/ -lcurses"
	     "\\djgpp\\contrib\\pdcurses\\lib\\libcurse.a" ())
     (nostart djgpp "" "-nostartfiles" #f ("ecrt0.c"))
     (dump djgpp "" "c:/djgpp/lib/crt0.o" #f ("unexelf.c"))

     (c Microsoft-C "" "" #f ("findexec.c"))
     (m Microsoft-C "" "" #f ())
     (c Microsoft-C-nt "" "" #f ("findexec.c"))
     (m Microsoft-C-nt "" "" #f ())
     (c Microsoft-Quick-C "" "" #f ("findexec.c"))
     (m Microsoft-Quick-C "" "" #f ())

     (c Turbo-C-2 "" "" #f ("findexec.c"))
     (m Turbo-C-2 "" "" #f ())
     (graphics Turbo-C-2 "" "graphics.lib" #f ())

     (c Borland-C-3.1 "" "" #f ("findexec.c"))
     (m Borland-C-3.1 "" "" #f ())
     (graphics Borland-C-3.1 "" "graphics.lib" #f ())
     (windows Borland-C-3.1 "-N -W" "-W" #f ())

     (c highc.31 "" "" #f ("findexec.c"))
     (m highc.31 "" "" #f ())
     (windows highc.31 "-Hwin" "-Hwin" #f ())
     ))

  '(compile-commands
    ((name symbol)
     (platform platform))
    ((procedure expression))

    ((compile-c-files Borland-C-3.1
		      (lambda (files parms)
			(define rsp-name "temp.rsp")
			(apply batch:lines->file parms rsp-name files)
			(batch:system parms
				      "bcc" "-d" "-O" "-Z" "-G" "-w-pro" "-ml" "-c"
				      (if (member '(define "FLOATS" #t)
						  (c-defines parms))
					  "" "-f-")
				      (c-includes parms)
				      (c-flags parms)
				      (string-append "@" rsp-name))
			(replace-suffix files ".c" ".obj")))
     (link-c-program Borland-C-3.1
		     (lambda (oname objects libs parms)
		       (define lnk-name (string-append oname ".lnk"))
		       (apply batch:lines->file parms
			      lnk-name
			      (append libs objects))
		       (batch:system parms "bcc"
				     (string-append "-e" oname)
				     "-ml"
				     (string-append "@" lnk-name))
		       (string-append oname ".exe")))

     (compile-c-files Turbo-C-2
		      (lambda (files parms)
			(batch:system parms
				      "tcc" "-c" "-d" "-O" "-Z" "-G" "-ml" "-c"
				      "-Ic:\\turboc\\include"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".obj")))
     (link-c-program Turbo-C-2
		     (lambda (oname objects libs parms)
		       (let ((exe (replace-suffix (car objects) ".obj" ".exe"))
			     (oexe (string-append oname ".exe")))
			 (if (not (string-ci=? exe oexe))
			     (batch:delete-file parms oexe))
			 (batch:system parms
				       "tcc" "-Lc:\\turboc\\lib" libs objects)
			 (if (not (string-ci=? exe oexe))
			     (batch:rename-file parms exe oexe))
			 oexe)))

     (compile-c-files Microsoft-C
		      (lambda (files parms)
			(batch:system parms
				      "cl" "-c" "Oxp" "-AH"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".obj")))
     (link-c-program Microsoft-C
		     (lambda (oname objects libs parms)
		       (let ((exe (replace-suffix (car objects) ".obj" ".exe"))
			     (oexe (string-append oname ".exe")))
			 (if (not (string-ci=? exe oexe))
			     (batch:delete-file parms oexe))
			 (batch:system parms
				       "link" "/noe" "/ST:40000"
				       (apply string-join "+"
					      (map (lambda (o)
						     (replace-suffix o ".obj" ""))
						   objects))
				       libs)
			 (if (not (string-ci=? exe oexe))
			     (batch:rename-file parms exe oexe))
			 oexe)))
     (compile-c-files Microsoft-C-nt
		      (lambda (files parms)
			(batch:system parms
				      "cl" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".obj")))
     (link-c-program Microsoft-C-nt
		     (lambda (oname objects libs parms)
		       (let ((exe (replace-suffix (car objects) ".obj" ".exe"))
			     (oexe (string-append oname ".exe")))
			 (if (not (string-ci=? exe oexe))
			     (batch:delete-file parms oexe))
			 (batch:system parms
				       "link"
				       (apply string-join " "
					      (map (lambda (o)
						     (replace-suffix o ".obj" ""))
						   objects))
				       libs)
			 (if (not (string-ci=? exe oexe))
			     (batch:rename-file parms exe oexe))
			 oexe)))

     (compile-c-files Microsoft-Quick-C
		      (lambda (files parms)
			(batch:system parms
				      "qcl" "/AH" "/W1" "/Ze" "/O" "/Ot" "/DNDEBUG"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".obj")))
     (link-c-program Microsoft-Quick-C
		     (lambda (oname objects libs parms)
		       (define crf-name (string-append oname ".crf"))
		       (apply batch:lines->file parms
			      crf-name
			      `(,@(map (lambda (f) (string-append f " +"))
				       objects)
				""
				,(string-append oname ".exe")
				,(apply string-join " " libs)
				";"))
		       (batch:system parms
				     "qlink"
				     "/CP:0xffff" "/NOI" "/SE:0x80" "/ST:0x9c40"
				     crf-name)
		       (string-append oname ".exe")))

     (compile-c-files Watcom-9.0
		      (lambda (files parms)
			(batch:system parms
				      "wcc386p" "/mf" "/d2" "/ze" "/oxt" "/3s"
				      "/zq" "/w3"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".obj")))
     (link-c-program Watcom-9.0
		     (lambda (oname objects libs parms)
		       (let ((exe (replace-suffix (car objects)
						  ".obj" ".exe"))
			     (oexe (string-append oname ".exe")))
			 (if (not (string-ci=? exe oexe))
			     (batch:delete-file parms oexe))
			 (batch:system parms
				       "wlinkp" "option" "quiet" "option"
				       "stack=40000" "FILE"
				       (apply string-join ","
					      (map (lambda (o)
						     (replace-suffix o ".obj" ""))
						   objects))
				       libs)
			 (if (not (string-ci=? exe oexe))
			     (batch:rename-file parms exe oexe))
			 oexe)))
     (compile-c-files highc.31
		      (lambda (files parms)
			(define hcc-name "temp.hcc")
			(apply batch:lines->file parms hcc-name files)
			(batch:system parms
				      "\\hi_c\\hc386.31\\bin\\hc386"
				      (c-includes parms)
				      (c-flags parms)
				      "-c" (string-append "@" hcc-name))
			(replace-suffix files ".c" ".obj")))
     (link-c-program highc.31
		     (lambda (oname objects libs parms)
		       (let ((oexe (string-append oname ".exe")))
			 (define lnk-name (string-append oname ".lnk"))
			 (apply batch:lines->file parms
				lnk-name (append libs objects))
			 (batch:system parms
				       "\\hi_c\\hc386.31\\bin\\hc386" "-o" oname
				       (string-append "@" lnk-name))
			 (batch:system parms
				       "bind386" "/hi_c/pharlap.51/run386b.exe" oname
				       "-exe" oexe)
			 oexe)))

     (compile-c-files djgpp
		      (lambda (files parms)
			(batch:apply-chop-to-fit
			 batch:try-system parms
			 "gcc" "-Wall" "-O2" "-c"
			 (c-includes parms) (c-flags parms)
			 files)
			(replace-suffix files ".c" ".o")))
     (link-c-program djgpp
		     (lambda (oname objects libs parms)
		       (let ((exe (string-append oname ".exe")))
			 (or
			  (batch:try-system parms
					    "gcc" "-o" oname
					    (must-be-first
					     '("-nostartfiles"
					       "pre-crt0.o" "ecrt0.o"
					       "c:/djgpp/lib/crt0.o")
					     (append objects libs)))
			  (let ((arname (string-append oname ".a")))
			    (batch:delete-file parms arname)
			    (batch:apply-chop-to-fit
			     batch:try-system parms
			     "ar" "r" arname objects)
			    (batch:system
			     parms "gcc" "-o" oname
			     (must-be-first
			      '("-nostartfiles"
				"pre-crt0.o" "ecrt0.o"
				"c:/djgpp/lib/crt0.o")
			      (cons arname libs))))
			  (slib:error 'build "couldn't build archive"))
			 (batch:system parms "strip" oname)
			 (batch:delete-file parms exe)
			 (batch:system parms
				       "coff2exe" "-s"
				       "c:\\djgpp\\bin\\go32.exe"
				       oname)
			 exe)))

     (compile-c-files os/2-emx
		      (lambda (files parms)
			(batch:system parms
				      "gcc" "-O" "-m386" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))
     (link-c-program os/2-emx
		     (lambda (oname objects libs parms)
		       (batch:system parms
				     "gcc" "-o" (string-append oname ".exe")
				     objects libs)
		       (string-append oname ".exe")))

     (compile-c-files os/2-cset
		      (lambda (files parms)
			(batch:system parms
				      "icc.exe" "/Gd-" "/Ge+" "/Gm+" "/Q" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".obj")))
     (link-c-program os/2-cset
		     (lambda (oname objects libs parms)
		       (batch:system parms
				     "link386.exe" objects libs
				     (string-append "," oname ".exe,,,;"))
		       (string-append oname ".exe")))

     (compile-c-files HP-UX
		      (lambda (files parms)
			(batch:system parms
				      "cc" "+O1" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))
     (compile-dll-c-files HP-UX
			  (lambda (files parms)
			    (batch:system parms
					  "cc" "+O1" "-Wl,-E" "+z" "-c"
					  (c-includes parms)
					  (c-flags parms)
					  files)
			    (for-each
			     (lambda (fname)
			       (batch:rename-file parms
						  (string-append fname ".sl")
						  (string-append fname ".sl~"))
			       (batch:system parms
					     "ld" "-b" "-o"
					     (string-append fname ".sl")
					     (string-append fname ".o")))
			     (replace-suffix files ".c" ""))
			    (replace-suffix files ".c" ".sl")))
;     (make-dll-archive HP-UX
;		       (lambda (oname objects libs parms)
;			 (batch:system parms
;				       "ld" "-b" "-o" (string-append oname ".sl")
;				       objects)
;			 (string-append oname ".sl")))

     (make-dll-archive sun
		       (lambda (oname objects libs parms)
			 (batch:system parms
				       "ld" "-assert" "pure-text" "-o"
				       (string-append oname ".so.1.0")
				       objects)
			 (string-append oname ".so.1.0")))

     (compile-c-files linux-aout
		      (lambda (files parms)
			(batch:system parms
				      "gcc" "-Wall" "-O2" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))
     (compile-dll-c-files linux-aout
			  (lambda (files parms)
			    (batch:system parms
					  "gcc" "-Wall" "-O2" "-c"
					  (c-includes parms)
					  (c-flags parms)
					  files)
			    (replace-suffix files ".c" ".o")))
;;;     (make-dll-archive linux-aout
;;;		       (lambda (oname objects libs parms) #t
;;;			       oname))

     (compile-c-files linux
		      (lambda (files parms)
			(batch:system parms
				      "gcc" "-O2" "-c" (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))
     (compile-dll-c-files linux
			  (lambda (files parms)
			    (batch:system parms
					  "gcc" "-O2" "-fpic" "-c" (c-includes parms)
					  (c-flags parms)
					  files)
			    (let* ((platform (car (parameter-list-ref
						   parms 'platform)))
				   (ld-opts
				    (map (lambda (l)
					   (build:lib-ld-flag l platform))
					 (parameter-list-ref parms 'c-lib))))
			      (for-each
			       (lambda (fname)
				 (batch:system parms
					       "gcc" "-shared" "-o"
					       (string-append fname ".so")
					       (string-append fname ".o")
					       ld-opts))
			       (replace-suffix files ".c" "")))
			    (replace-suffix files ".c" ".so")))
     (make-dll-archive linux
		       (lambda (oname objects libs parms)
			 (let ((platform (car (parameter-list-ref
					       parms 'platform))))
			   (batch:system
			    parms
			    "gcc" "-shared" "-o"
			    (string-append oname ".so")
			    objects
			    (map (lambda (l) (build:lib-ld-flag l platform))
				 (parameter-list-ref parms 'c-lib))))
			 (string-append oname ".so")))
     (link-c-program linux
		     (lambda (oname objects libs parms)
		       (batch:system parms
				     "gcc" "-rdynamic" "-o" oname
				     (must-be-first
				      '("pre-crt0.o" "ecrt0.o" "/usr/lib/crt0.o")
				      (append objects libs)))
		       oname))

     (compile-c-files Unicos
		      (lambda (files parms)
			(batch:system parms
				      "cc" "-hvector2" "-hscalar2" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))
     (link-c-program Unicos
		     (lambda (oname objects libs parms)
		       (batch:system parms
				     "cc" "setjump.o" "-o" oname objects libs)
		       oname))

     (compile-c-files gcc
		      (lambda (files parms)
			(batch:system parms
				      "gcc" "-Wall" "-O2" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))

     (link-c-program gcc
		     (lambda (oname objects libs parms)
		       (batch:rename-file parms
					  oname (string-append oname "~"))
		       (batch:system parms
				     "gcc" "-o" oname
				     (must-be-first
				      '("-nostartfiles"
					"pre-crt0.o" "ecrt0.o"
					"/usr/lib/crt0.o")
				      (append objects libs)))
		       oname))

     (compile-c-files svr4
		      (lambda (files parms)
			(batch:system parms
				      "cc" "-O" "-DSVR4" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))

     (compile-c-files aix
		      (lambda (files parms)
			(batch:system parms
				      "cc" "-O" "-Dunix" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))
     (link-c-program aix
		     (lambda (oname objects libs parms)
		       (batch:system parms
				     "cc" "-lansi" "-o" oname objects libs)
		       oname))

     (compile-c-files amiga-aztec
		      (lambda (files parms)
			(batch:system parms
				      "cc" "-dAMIGA"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))
     (link-c-program amiga-aztec
		     (lambda (oname objects libs parms)
		       (batch:system parms
				     "cc" "-o" oname objects libs "-lma")
		       oname))

     (compile-c-files amiga-SAS/C-5.10
		      (lambda (files parms)
			(batch:system parms
				      "lc" "-d3" "-M" "-fi" "-O"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(batch:system parms "blink with link.amiga NODEBUG")
			(replace-suffix files ".c" ".o")))
     (link-c-program amiga-SAS/C-5.10
		     (lambda (oname objects libs parms)
		       (define lnk-name "link.amiga")
		       (apply batch:lines->file parms
			      lnk-name
			      (apply string-join "+" ">FROM LIB:c.o"
				     (map object->string objects))
			      (string-append
			       "TO " (object->string (string-append "/" oname)))
			      (append
			       (cond
				((pair? libs)
				 (cons (string-append "LIB LIB:" (car libs))
				       (map (lambda (s)
					      (string-append "    LIB:" s))
					    (cdr libs))))
				(else '()))
			       '("VERBOSE" "SC" "SD")))
		       oname))

     (compile-c-files amiga-dice-c
		      (lambda (files parms)
			(batch:system parms
				      "dcc" "-r" "-gs" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files "-o" (replace-suffix files ".c" ".o"))
			(replace-suffix files ".c" ".o")))
     (link-c-program amiga-dice-c
		     (lambda (oname objects libs parms)
		       (batch:system parms
				     "dcc" "-r" "-gs" "-o" oname objects libs)
		       oname))

     (compile-c-files atari-st-gcc
		      (lambda (files parms)
			(batch:system parms
				      "gcc" "-v" "-O" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))
     (link-c-program atari-st-gcc
		     (lambda (oname objects libs parms)
		       (batch:system parms
				     "gcc" "-v" "-o" (string-append oname ".ttp")
				     objects libs)
		       (string-append oname ".ttp")))

     (compile-c-files atari-st-turbo-c
		      (lambda (files parms)
			(batch:system parms
				      "tcc" "-P" "-W-" "-Datarist"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))
     (link-c-program atari-st-turbo-c
		     (lambda (oname objects libs parms)
		       (batch:system parms
				     "tlink" "-o" (string-append oname ".ttp")
				     objects libs "mintlib.lib" "osbind.lib"
				     "pcstdlib.lib" "pcfltlib.lib")
		       (string-append oname ".ttp")))

     (compile-c-files acorn-unixlib
		      (lambda (files parms)
			(batch:system parms
				      "cc" "-c" "-depend" "!Depend" "-IUnixLib:"
				      "-pcc" "-Dunix" "-DSVR3" "-DARM_ULIB"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))
     (link-c-program acorn-unixlib
		     (lambda (oname objects libs parms)
		       (batch:system parms
				     "link" "-o" oname objects libs
				     ":5.$.dev.gcc.unixlib36d.clib.o.unixlib")
		       (batch:system parms
				     "squeeze" oname)
		       oname))

     (compile-c-files vms
		      (lambda (files parms)
			(batch:system parms
				      "cc"
				      (c-includes parms)
				      (c-flags parms)
				      (replace-suffix files ".c" ""))
			(replace-suffix files ".c" ".obj")))
     (link-c-program vms
		     (lambda (oname objects libs parms)
		       (let ((exe (replace-suffix (car objects)
						  ".obj" ".exe"))
			     (oexe (string-append oname ".exe")))
			 (batch:system parms
				       "macro" "setjump")
			 (batch:system parms
				       "link"
				       (apply string-join ","
					      (append (map (lambda (f)
							     (replace-suffix f ".obj" ""))
							   objects)
						      '("setjump" "sys$input/opt\n   ")))
				       (apply string-join
					      "," (append (remove "" libs)
							  '("sys$share:vaxcrtl/share"))))
			 (if (not (string-ci=? exe oexe))
			     (batch:rename-file parms exe oexe))
			 oexe)))

     (compile-c-files vms-gcc
		      (lambda (files parms)
			(batch:system parms
				      "gcc"
				      (c-includes parms)
				      (c-flags parms)
				      (replace-suffix files ".c" ""))
			(replace-suffix files ".c" ".obj")))
     (link-c-program vms-gcc
		     (lambda (oname objects libs parms)
		       (let ((exe (replace-suffix (car objects)
						  ".obj" ".exe"))
			     (oexe (string-append oname ".exe")))
			 (batch:system parms
				       "macro" "setjump")
			 (batch:system parms
				       "link"
				       (apply string-join ","
					      (append objects
						      '("setjump.obj"
							"sys$input/opt\n   ")))
				       (apply string-join
					      "," (append (remove "" libs)
							  '("gnu_cc:[000000]gcclib/lib"
							    "sys$share:vaxcrtl/share"))))
			 (if (not (string-ci=? exe oexe))
			     (batch:rename-file parms exe oexe))
			 oexe)))

     (compile-c-files *unknown*
		      (lambda (files parms)
			(batch:system parms
				      "cc" "-O" "-c"
				      (c-includes parms)
				      (c-flags parms)
				      files)
			(replace-suffix files ".c" ".o")))
     (link-c-program *unknown*
		     (lambda (oname objects libs parms)
		       (batch:rename-file parms
					  oname (string-append oname "~"))
		       (batch:system parms
				     "cc" "-o" oname
				     (must-be-first
				      '("-nostartfiles"
					"pre-crt0.o" "ecrt0.o"
					"/usr/lib/crt0.o")
				      (append objects libs)))
		       oname))
     (make-archive *unknown*
		   (lambda (oname objects libs parms)
		     (let ((aname (string-append oname ".a")))
		       (batch:system parms
				     "ar rc" aname objects)
		       (batch:system parms
				     "ranlib" aname)
		       aname)))
     (compile-dll-c-files *unknown*
			  (lambda (files parms)
			    (batch:system parms
					  "cc" "-O" "-c"
					  (c-includes parms)
					  (c-flags parms)
					  files)
			    (replace-suffix files ".c" ".o")))
     (make-dll-archive *unknown*
		       (lambda (oname objects libs parms)
			 (let ((aname (string-append oname ".a")))
			   (batch:system parms
					 "ar rc" aname objects)
			   (batch:system parms
					 "ranlib" aname)
			   aname)))
     (make-nothing *unknown*
		   (lambda (oname objects libs parms)
		     (if (= 1 (length objects)) (car objects)
			 objects)))
     ))

  '(features
    ((name symbol))
    ((spec expression)
     (documentation string))
    ((lit () "Light - no features")
     (none () "No features")

     (cautious ((define "CAUTIOUS"))
	       "\
Normally, the number of arguments arguments to interpreted closures
 (from LAMBDA) are checked if the function part of a form is not a
symbol or only the first time the form is executed if the function
part is a symbol.  defining RECKLESS disables any checking.  If you
want to have SCM always check the number of arguments to interpreted
closures #define CAUTIOUS.")

     (careful-interrupt-masking ((define "CAREFUL_INTS"))
				"\
Define CAREFUL_INTS for extra checking of interrupt masking.  This is
for debugging C code in sys.c and repl.c.")

     (debug ((c-lib debug)
	     (features cautious careful-interrupt-masking stack-limit))
	    "Debugging")

     (reckless ((define "RECKLESS"))
	       "\
If your scheme code runs without any errors you can disable almost all
error checking by compiling all files with RECKLESS.")

     (stack-limit ((define ("STACK_LIMIT" "(HEAP_SEG_SIZE/2)")))
		  "\
Define STACK_LIMIT to enable checking for stack overflow.  Define
value of STACK_LIMIT to be the size to which SCM should allow the
stack to grow.  STACK_LIMIT should be less than the maximum size the
hardware can support, as not every routine checks the stack.")

     (bignums ((define "BIGNUMS"))
	      "\
Large precision integers.")

     (arrays ((define "ARRAYS"))
	     "\
Define ARRAYS if you want arrays, uniform-arrays and uniform-vectors.")

     (array-for-each ((c-file "ramap.c") (init "init_ramap"))
	     "\
array-map! and array-for-each (ARRAYS must also be defined).")

     (inexact ((define "FLOATS") (c-lib m))
	      "\
Define FLOATS if you want floating point numbers.")

     (engineering-notation ((define "ENGNOT"))
			   "\
Define ENGNOT if you want floats to display in engineering notation
 (exponents always multiples of 3) instead of scientific notation.")

     (single-precision-only ((define "SINGLESONLY"))
			    "\
Define SINGLESONLY if you want all inexact real numbers to be single
precision.  This only has an effect if SINGLES is also defined (which
is the default).  This does not affect complex numbers.")

     (sicp ((define "SICP"))
	   "\
Define SICP if you want to run code from:

    H. Abelson, G. J. Sussman, and J. Sussman,
    Structure and Interpretation of Computer Programs,
    The MIT Press, Cambridge, Massachusetts, USA

 (eq? '() '#f) is the major difference.")

     (rev2-procedures ((c-file "sc2.c") (init "init_sc2"))
		      "\
These procedures were specified in the `Revised^2 Report on Scheme'
but not in `R4RS'.")

     (record ((c-file "record.c") (init "init_record"))
	     "\
The Record package provides a facility for user to define their own
record data types.  See SLIB for documentation.")

     (compiled-closure ((define "CCLO"))
		       "\
Define CCLO if you want to use compiled closures.")

     (generalized-c-arguments ((c-file "gsubr.c") (init "init_gsubr"))
			      "\
make_gsubr for arbitrary (< 11) arguments to C functions.")

     (tick-interrupts ((define "TICKS"))
		      "\
Define TICKS if you want the ticks and ticks-interrupt functions.")

     (i/o-extensions ((c-file "ioext.c") (init "init_ioext"))
		     "\
Commonly available I/O extensions: `Exec', line I/O, file positioning,
file delete and rename, and directory functions.")

     (turtlegr
      ((c-file "turtlegr.c") (c-lib graphics) (features inexact)
			     (init "init_turtlegr"))
      "\
`Turtle' graphics calls for both Borland-C and X11.")

     (curses ((c-file "crs.c") (c-lib curses) (init "init_crs"))
	     "\
`Curses' screen management package.")

     (edit-line
      ((c-file "edline.c") (c-lib termcap editline) (compiled-init "init_edline"))
      "\
interface to the editline or GNU readline library")

     (regex ((c-file "rgx.c") (c-lib regex) (init "init_rgx"))
	    "\
String regular expression matching.")

     (socket ((c-file "socket.c") (init "init_socket"))
	     "\
BSD socket interface.")

     (posix ((c-file "posix.c") (init "init_posix"))
	    "\
Posix functions available on all `Unix-like' systems.  fork and
process functions, user and group IDs, file permissions, and `link'.")

     (unix ((c-file "unix.c") (init "init_unix"))
	   "\
Those unix features which have not made it into the Posix specs: nice,
acct, lstat, readlink, symlink, mknod and sync.")

     (windows ((c-lib windows))		; (define "NON_PREEMPTIVE")
	      "\
Microsoft Windows executable.")

     (dynamic-linking ((c-file "dynl.c") (c-lib dlll))
		      "\
Load compiled files while running.")

     (dump ((define "CAN_DUMP")
	    (c-lib dump)
	    (c-lib nostart)
	    (c-file "unexec.c")
	    (c-file "unexelf.c")
	    (c-file "gmalloc.c")
	    (c-file "ecrt0.c")
	    (c-file "pre-crt0.c"))
	   "\
Convert a running scheme program into an executable file.")

;;;; Descriptions of these parameters is in "setjump.h".
;;;	(initial-heap-size ((define "INIT_HEAP_SIZE" (* 25000 sizeof-cell))))
;;;	(heap-segment-size ((define "HEAP_SEG_SIZE" (* 8100 sizeof-cell))))
;;;	(short-aligned-stack ((define "SHORT_ALIGN")))
;;;	(initial-malloc-limit ((define "INIT_MALLOC_LIMIT" 100000)))
;;;	(number-of-hash-buckets ((define "NUM_HASH_BUCKETS" 137)))
;;;	(minimum-gc-yield ((define "MIN_GC_YIELD" "(heap_size/4)")))

     (heap-can-shrink ((define "DONT_GC_FREE_SEGMENTS"))
		      "\
Define DONT_GC_FREE_SEGMENTS if you want segments of unused heap to
not be freed up after garbage collection.  This may reduce time in GC
for *very* large working sets.")

     (cheap-continuations ((define "CHEAP_CONTINUATIONS"))
			  "\
If you only need straight stack continuations CHEAP_CONTINUATIONS will
run faster and use less storage than not having it.  Machines with
unusual stacks need this.  Also, if you incorporate new C code into
scm which uses VMS system services or library routines (which need to
unwind the stack in an ordrly manner) you may need to define
CHEAP_CONTINUATIONS.")

     (memoize-local-bindings ((define "MEMOIZE_LOCALS"))
			     "\
Saves the interpeter from having to look up local bindings for every
identifier reference")
     ))
  '(build-params
    *parameter-columns*
    *parameter-columns*
    ((1 platform single platform
	(lambda (pl) (list batch:platform))
	#f
	"what to build it for")
     (2 target-name single string (lambda (pl) '("scm")) #f
	"base name of target")
     (3 c-lib nary symbol (lambda (pl) '(c)) #f
	"C library (and include files)")
     (4 define nary string #f #f "#define FLAG")
     (5 implinit single string
	(lambda (pl) (list (object->string
			    (in-vicinity (implementation-vicinity) "Init.scm"))))
	#f "implementation vicinity")
     (6 c-file nary filename #f #f "C source files")
     (7 o-file nary filename #f #f "other object files")
     (8 init nary string #f #f "initialization calls")
     (9 compiled-init nary string #f #f "later initialization calls")
     (10 features nary symbol
	 (lambda (pl) '(arrays inexact bignums))
	 (lambda (rdb) (((rdb 'open-table) 'features #f) 'get 'spec))
	 "features to include")
     (11 what single build-whats
	 (lambda (pl) '(exe))
	 (lambda (rdb)
	   (define tab (((rdb 'open-table) 'build-whats #f) 'get 'class))
	   (define manifest ((((rdb 'open-table) 'manifest #f)
			      'row:retrieve*)))
	   (lambda (what)
	     (define catgry (tab what))
	     `((c-file
		,@(map car
		       (remove-if-not
			(lambda (row) (and (eq? 'c-source (cadr row))
					   (eq? catgry (caddr row))))
			manifest)))
	       ,@(or ((((rdb 'open-table) 'build-whats #f) 'get 'spec) what)
		     '()))))
	 "what to build")
     (12 batch-dialect single batch-dialect
	 guess-how
	 #f
	 "How to build")
     (13 who single expression (lambda (pl) (list (current-output-port))) #f
	 "name of buildfile or port")
     (14 compiler-options nary string #f #f "command-line compiler options")
     (15 linker-options nary string #f #f "command-line linker options")

     (17 batch-port nary expression #f #f
	 "port batch file will be written to.")
     (18 c-defines nary expression #f #f "#defines for C")
     (19 c-includes nary expression #f #f "library induced defines for C")
     ))
  '(build-pnames
    ((name string))
    ((parameter-index uint))
    (
     ("p" 1) ("platform" 1)
     ("o" 2) ("outname" 2)
     ("l" 3) ("libraries" 3)
     ("D" 4) ("defines" 4)
     ("s" 5) ("scheme initialization file" 5)
     ("c" 6) ("c source files" 6)
     ("j" 7) ("object files" 7)
     ("i" 9) ("initialization calls" 9)
     ("F" 10) ("features" 10)
     ("t" 11) ("type" 11)
     ("h" 12) ("batch dialect" 12)
     ("w" 13) ("script name" 13)
     ("compiler options" 14)
     ("linker options" 15)
     ))

  '(*commands*
    ((name symbol))			;or just desc:*commands*
    ((parameters parameter-list)
     (parameter-names parameter-name-translation)
     (procedure expression)
     (documentation string))
    ((build
      build-params
      build-pnames
      build:build
      "build program.")
     (*initialize*
      no-parameters
      no-parameters
      build:init
      "SCM Build Database"))))

;;;((build 'close-database))
;;;(define build (open-database! "buildscm.scm" 'alist-table))

(define build:error slib:error)
(define build:c-libraries #f)
(define build:lib-cc-flag #f)
(define build:lib-ld-flag #f)
(define build:c-supress #f)
(define plan-command #f)

;;; Look up command on a platform, but default to '*unknown* if not
;;; initially found.

(define (make-defaulting-platform-lookup getter)
  (lambda (thing plat)
    (define (look platform)
      (let ((ans (getter thing platform)))
	(cond (ans ans)
	      ((eq? '*unknown* platform)
	       (build:error "Couldn't find: " thing))
	      (else (look '*unknown*)))))
    (look plat)))

(define system:success? zero?)

(require 'alist)
(require 'common-list-functions)
(require 'object->string)

(define build:build
  (lambda (rdb)
    (lambda (parms)
      (let ((expanders
	     (map (lambda (e) (and e (lambda (s) (e s))))
		  (map (lambda (f) (if f ((slib:eval f) rdb) f))
		       ((((rdb 'open-table) 'build-params #f)
			 'get* 'expander))))))
	(parameter-list-expand expanders parms)
	(set! parms
	      (fill-empty-parameters
	       (map slib:eval
		    ((((rdb 'open-table) 'build-params #f)
		      'get* 'default)))
	       parms))
	(parameter-list-expand expanders parms))
      (let* ((platform (car (parameter-list-ref parms 'platform)))
	     (init= (apply string-append
			   (map (lambda (c)
				  (string-append c "();"))
				(parameter-list-ref parms 'init))))
	     (compiled-init=
	      (apply string-append
		     (map (lambda (c)
			    (string-append c "();"))
			  (parameter-list-ref parms 'compiled-init))))
	     (c-defines
	      `((define "IMPLINIT"
		  ,(car (parameter-list-ref parms 'implinit)))
		,@(if (string=? "" init=) '()
		      `((define "INITS" ,init=)))
		,@(if (string=? "" compiled-init=) '()
		      `((define "COMPILED_INITS" ,compiled-init=)))
		,@(map (lambda (d) (if (pair? d)
				       `(define ,@d)
				       `(define ,d #t)))
		       (parameter-list-ref parms 'define))))
	     (c-includes
	      (map (lambda (l) (build:lib-cc-flag l platform))
		   (parameter-list-ref parms 'c-lib)))
	     (batch-dialect (car (parameter-list-ref parms 'batch-dialect)))
	     (what (car (parameter-list-ref parms 'what)))
	     (c-proc (plan-command ((((rdb 'open-table) 'build-whats #f)
				     'get 'c-proc)
				    what)
				   platform)))
	(adjoin-parameters!
	 parms
	 (cons 'c-defines c-defines)
	 (cons 'c-includes c-includes)
	 )

	(let ((name (car (parameter-list-ref parms 'who))))
	  (batch:call-with-output-script
	   parms
	   name
	   (lambda (batch-port)
	     (define o-files '())
	     (adjoin-parameters!
	      parms
	      (list 'batch-port batch-port))

	     ;; ================ Write file with C defines
	     (apply batch:lines->file parms
		    "scmflags.h"
		    (defines->c-defines c-defines))

	     ;; ================ Compile C source files
	     (set! o-files
		   (let ((supressors
			  (apply append
				 (map (lambda (l) (build:c-supress l platform))
				      (parameter-list-ref parms 'c-lib)))))
		     (c-proc (remove-if (lambda (file) (member file supressors))
					(parameter-list-ref parms 'c-file))
			     parms)))

	     ;; ================ Link C object files
	     ((plan-command
	       ((((rdb 'open-table) 'build-whats #f) 'get 'o-proc) what)
	       platform)
	      (car (parameter-list-ref parms 'target-name))
	      (append o-files (parameter-list-ref parms 'o-file))
	      (append
	       (parameter-list-ref parms 'linker-options)
	       (map (lambda (l) (build:lib-ld-flag l platform))
		    (parameter-list-ref parms 'c-lib)))
	      parms))))))))

(define (c-defines parms)
  (parameter-list-ref parms 'c-defines))
(define (c-includes parms)
  (parameter-list-ref parms 'c-includes))
(define (c-flags parms)
  (parameter-list-ref parms 'compiler-options))

(define (defines->c-defines defines)
  (map
   (lambda (d)
     (case (caddr d)
       ((#t) (string-join " " "#define" (cadr d)))
       ((#f) (string-join " " "#undef" (cadr d)))
       (else (apply string-join " " "#define" (cdr d)))))
   defines))

(define (defines->flags defines)
  (map
   (lambda (d)
     (case (caddr d)
       ((#t) (string-append "-D" (cadr d)))
       ((#f) (string-append "-U" (cadr d)))
       (else (string-append "-D" (cadr d) "=" (object->string (caddr d))))))
   defines))

(define (guess-how pl)
  (let* ((plat (parameter-list-ref pl 'platform))
	 (platform (if (pair? plat) (car plat) batch:platform)))
    (let ((os (or ((((build 'open-table) 'platform #f)
		    'get 'operating-system) platform) batch:platform)))
      (cond ((not os) (slib:error "OS corresponding to " platform " unknown"))
	    (else (list (os->batch-dialect os)))))))

(define build:initializer
  (lambda (rdb)
    (set! build:c-libraries ((rdb 'open-table) 'c-libraries #f))
    (set! build:lib-cc-flag
	  (make-defaulting-platform-lookup
	   (build:c-libraries 'get 'compiler-flags)))
    (set! build:lib-ld-flag
	  (make-defaulting-platform-lookup
	   (build:c-libraries 'get 'link-lib-flag)))
    (set! build:c-supress
	  (make-defaulting-platform-lookup
	   (build:c-libraries 'get 'supress-files)))
    (set! plan-command
	  (let ((lookup (make-defaulting-platform-lookup
			 (((rdb 'open-table) 'compile-commands #f)
			  'get 'procedure))))
	    (lambda (thing plat)
	      (slib:eval (lookup thing plat)))))))
(build:initializer build)

(define (build-from-argv argv)
  (cond ((string? argv)
	 (require 'read-command)
	 (set! argv (call-with-input-string argv read-command))))
  (let ()
    (define command (string->symbol (list-ref argv *optind*)))
    (define argc (length argv))
    (cond
     ((pair? argv)
      (set! *optind* (+ 1 *optind*))
      ((make-command-server build '*commands*)
       command
       (lambda (comname comval options positions arities types
			defaults checks aliases)
	 (let* ((params (getopt->parameter-list
			 argc argv options arities types aliases))
		(fparams (fill-empty-parameters defaults params)))
	   (cond ((not (list? params)) #f)
		 ((not (check-parameters checks fparams)) #f)
		 ((not (check-arities (map arity->arity-spec arities) fparams))
		  (slib:error 'build-from-argv "arity error" fparams) #f)
		 (else (comval fparams))))))))))

(define (build-from-whole-argv argv)
  (set! *optind* 0)
  (set! *optarg* #f)
  (build-from-argv argv))

(define b build-from-whole-argv)

(define (b*)
  (require 'read-command)
  (do ((e (read-command) (read-command)))
      ((eof-object? e))
    (cond ((null? e))
	  (else
	   (cond ((not (string-ci=? (car e) "build"))
		  (set! e (cons "build" e))))
	   (write (build-from-whole-argv e))
	   (newline)))
    (display "build> ")
    (force-output)))

(define (bi) (build-from-argv *argv*))

(cond (*interactive*
       (display "type (b \"build <command-line>\") to build") (newline)
       (display "type (b*) to enter build command loop") (newline)))
