$! Modified VMSMAKE for JED - Andy Harper, Kings College London
$!	- Use logicals to define SLANG location, if defined	[16-MAY-1996]
$!	- A few bug fixes!					[16-MAY-1996]
$!
$! ver = f$verify(0)
$!  Makefile for VMS
$ axp = 0
$ if (p1 .eqs. "INSTALL") then goto install
$ defs = ""
$!
$! If you do not want floating point, comment out next line
$    defs = "FLOAT_TYPE"
$!
$! S-Lang Include directory (where slang.olb is located)
$    slanglib = "[-.-.slang.src]"
$    SLANGOBJ = SLANGLIB + "SLANG.OLB"			! AH 16-MAY-1996
$
$!
$! If logical names defining the location of the slan libraries and header
$! files exist, use them in preference to the above definition of SLANGLIB
$!	-  SLANG_ROOT:[SRC] = location of header files (slang.h)
$!	-  SLANG_LIBRARY    = (if defined) location of SLANG.OLB
$!			      (if not defined) SLANG_ROOT:[SRC]SLANG.OLB used
$
$ if f$trnlnm("SLANG_ROOT") .nes. ""			! AH 16-MAY-1996
$   then						! AH 16-MAY-1996
$     SLANGLIB := "SLANG_ROOT:[SRC]"			! AH 16-MAY-1996
$     if f$trnlnm("SLANG_LIBRARY") .nes. ""		! AH 16-MAY-1996
$       then						! AH 16-MAY-1996
$         SLANGOBJ = "SLANG_LIBRARY"			! AH 16-MAY-1996
$       else						! AH 16-MAY-1996
$         SLANGOBJ = SLANGLIB  +  "SLANG.OLB"		! AH 16-MAY-1996
$     endif						! AH 16-MAY-1996
$ endif							! AH 16-MAY-1996
$
$!
$  Link_Flags = ""
$  Link_Libs = ""
$! If compiling with GCC, uncomment next line
$! goto has_gcc
$ if (p1 .eqs. "GCC") then goto has_gcc
$!
$! There should be no need to change anything below here
$!
$ axp = f$getsyi("HW_MODEL").ge.1024
$ if axp
$ then	C_C = "cc/standard=vaxc"
$	Link_Flags = "/NONATIVE_ONLY"
$ else 
$       C_C = "cc"
$ endif
$ goto start
$ has_gcc:
$  C_C = "gcc/nocase_hack/warnings"
$  Link_Libs = ",GNU_CC:[000000]GCCLIB/LIB"
$!
$ start:
$ bfiles = "abbrev," -
	+ "buffer," -
	+ "cmds," -
	+ "file," -
	+ "indent," -
	+ "ins," -
	+ "intrin," -
	+ "keymap," -
	+ "ledit," -
	+ "line," -
	+ "lineattr," -
	+ "main," -
	+ "misc," -
	+ "paste," -
	+ "replace"
$!
$ bfiles = bfiles -
	+ ",screen," -
	+ "search," -
	+ "sig," -
	+ "syntax," -
	+ "sysdep," -
	+ "text," -
	+ "undo," -
	+ "vfile," -
	+ "vmshelp," -
	+ "vmsmail," -
	+ "vterm," -
	+ "blocal," -
	+ "mouse," -
	+ "window"
$!
$ jfiles = bfiles + ",display"
$ xfiles = bfiles + ",xterm"
$ if (p2 .eqs. "XJED") then goto make_xjed_label
$!
$ files = jfiles
$ make_return = "make_xjed_label"
$ jexec = "jed.exe"
$ goto simple_make
$!
$ make_xjed_label:
$  files = xfiles
$  make_return = "exit_label"
$  jexec = "xjed.exe"
$  goto simple_make
$!
$  exit_label:
$  exit
$!
$!
$!  simple make
$!
$  simple_make:
$!
$ copy jedconf.h config.h
$ purge config.h
$!
$  count = 0
$  next_file:
$    f = f$element(count, ",", files)
$    count = count + 1
$    if (f .eqs. ",") then goto do_link
$    objf = f$search("''f'.obj")
$    if (objf .eqs. "") then goto compile_it
$    tobj = f$file_attr(objf, "RDT")
$    tc  = f$file_attr("''f'.c", "RDT")
$    if (f .eqs. "sysdep")
$    then
$      if ( f$cvtime(tobj) .lts. f$cvtime(f$file_attr("vms.c","RDT"))) -
	 then goto compile_it
$    endif
$    if (f$cvtime(tc) .lts. f$cvtime(tobj)) then goto next_file
$  compile_it:  
$    write sys$output "''C_C'/define=(''defs',JED)/include=(''slanglib') ''f'.c"
$    'C_C'/define=('defs')/include=('slanglib') 'f'.c
$    goto next_file
$  do_link:
$  if axp
$  then
$    if make_return .eqs. "make_xjed_label"
$    then
$      set verify
$      link/exec='jexec' 'Link_Flags' 'files', 'slangobj'/LIBR 'Link_Libs'
$      set noverify
$    else
$      set verify
$      link/exec='jexec' 'Link_Flags' 'files', 'slangobj'/LIBR 'Link_Libs', -
	 sys$input/opt
         sys$share:decw$xlibshr.exe/share
$      set noverify
$    endif
$  else
$    if make_return .eqs. "make_xjed_label"
$    then
$      set verify
$      link/exec='jexec' 'Link_Flags' 'files', 'slangobj'/LIB 'Link_Libs',  -
	 sys$input/opt
         SYS$LIBRARY:VAXCRTL/SHARE
$      set noverify
$    else
$      set verify
$      link/exec='jexec' 'Link_Flags' 'files', 'slangobj'/LIB 'Link_Libs', -
	 sys$input/opt
         sys$library:decw$dwtlibshr/share
         sys$share:decw$xlibshr/share
         SYS$LIBRARY:VAXCRTL/SHARE
$      set noverify
$    endif
$  endif
$!  rename /log 'jexec' [-.bin]
$  goto 'make_return'
$!
$!  Installation
$!  
$ Install:
$   write sys$output "Sorry, no installation implemented yet."
$!   destdef = f$trnlnm("JED_LIBRARY")
$!   again:
$!   def = ""
$!   if (destdef .nes. "") then def = "(default ''destdef')"
$!   write sys$output "Installation Directory"
$!   inquire dest "''def'"
$!   if (dest .eqs. "") then dest = destdef
$!   if (dest .eqs. "") then goto again
$!   if (f$parse(dest) .nes. "") then goto do_copy
$!      write sys$output "Directory does not exist.   Creating it."
$!      create/dir/prot=(w:re) 'dest'
$!  do_copy:
$!   copy/log/prot=(w:re) *.sl, *.hlp, *.rc, *.info, jed.*in, jed.exe, *.com  -
 !     'dest'
$  The_Exit:
$  if (ver .eq. 1) then set verify
$  exit
