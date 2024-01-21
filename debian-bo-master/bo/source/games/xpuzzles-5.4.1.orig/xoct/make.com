$ save_verify='f$verify(0)
$! set ver 
$!
$!      Compile and link for xoct
$!
$! USAGE:
$! @make [debug clobber clean]
$!       debug : compile with degugger switch
$!       clean : clean all except executable
$!       clobber : clean all
$!
$! If you have
$!              XVMSUTILS library (VMS6.2 or lower) <NOT TESTED>
$! insert the correct directory instead of X11:
$ xvmsutilsf="X11:XVMSUTILS.OLB"
$!
$! Already assumes DEC C on Alpha.
$! Assume VAX C on VAX.
$ decc=0
$! Assume DEC C on VAX.
$! decc=1
$!
$ QUOTE = """""""""
$ QUOTE = QUOTE + QUOTE + QUOTE
$ score = "SCOREFILE=" + QUOTE + "[]oct.scores" + QUOTE
$ data = "DATAFILE=" + QUOTE + "[]oct.data" + QUOTE
$ defscore==''score'
$ defdata==''data'
$!
$! NOTHING SHOULD BE MODIFIED BELOW
$!
$ if p1 .eqs. "CLEAN" then goto Clean
$ if p1 .eqs. "CLOBBER" then goto Clobber
$!
$ axp=f$getsyi("HW_MODEL") .ge. 1024
$ sys_ver=f$edit(f$getsyi("version"),"compress")
$ if f$extract(0,1,sys_ver) .nes. "V"
$ then
$   type sys$input
This script will assume that the operating system version is at least V7.0. 
$!
$   sys_ver="V7.0"
$ endif
$ sys_maj=0+f$extract(1,1,sys_ver)
$ if sys_maj .lt. 7
$ then
$   xvmsutils=f$search("''xvmsutilsf'") .nes. ""
$ endif
$!
$!
$! Create .opt file
$ close/nolog optf
$ open/write optf xoct.opt
$!
$ defs=="VMS"
$ if sys_maj .lt. 7
$ then
$   if xvmsutils then defs=="''defs',USE_XVMSUTILS"
$ else
$   defs=="''defs',SRAND=srand48,LRAND=lrand48,MAXRAND=2147483648.0"
$ endif
$ defs=="''defscore',''defdata',''defs'"
$!
$!
$! Establish the Compiling Environment
$!
$! Set compiler command
$ if axp
$ then
$   cc=="cc/standar=vaxc/define=(''defs')"
$ else
$   if decc
$   then
$     cc=="cc/decc/standard=vaxc/define=(''defs')"
$   else ! VAX C
$     cc=="cc/define=(''defs')"
$   endif
$ endif
$ if p1 .eqs. "DEBUG" .or. p2 .eqs. "DEBUG" .or. p3 .eqs. "DEBUG"
$ then
$   if axp
$   then
$     cc=="cc/deb/noopt/standar=vaxc/define=(''defs')/list"
$   else
$     if decc
$     then
$       cc=="cc/deb/noopt/decc/standar=vaxc/define=(''defs')/list"
$     else ! VAX C
$       cc=="cc/deb/noopt/define=(''defs')/list"
$     endif
$   endif
$   link=="link/deb"
$ endif
$!
$ if axp .or. .not. decc
$ then
$   define/nolog sys sys$library
$ endif
$!
$ write sys$output "Compiling XOct with ''defs'"
$ call make Oct.obj   "cc Oct.c"   Oct.c OctP.h Oct.h
$ call make OctU.obj  "cc OctU.c"  OctU.c OctP.h Oct.h
$ call make rngs.obj    "cc rngs.c"    rngs.c
$ call make xoct.obj  "cc xoct.c"  xoct.c Oct.h
$!
$! Get libraries
$ if sys_maj .lt. 7
$ then
$   if xvmsutils then write optf "''xvmsutilsf'/lib"
$ endif
$! if .not. axp then write optf "sys$library:vaxcrtl/lib"
$ write optf "sys$library:vaxcrtl/lib"
$ if axp then write optf "sys$library:ucx$ipc_shr/share"
$ if axp then write optf "sys$share:decw$xextlibshr/share"
$ if axp then write optf "sys$share:decw$xtlibshrr5/share"
$ if .not. axp then write optf "sys$library:ucx$ipc/lib"
$! write optf "sys$share:decw$dxmlibshr/share"
$ write optf "sys$share:decw$xmlibshr12/share"
$ write optf "sys$share:decw$xlibshr/share"
$ close optf
$!
$! LINK
$ write sys$output "Linking XOct"
$ link/map xoct/opt
$!
$! Create .opt file
$ open/write optf xmoct.opt
$ write sys$output "Compiling XmOct with ''defs'"
$ call make Oct.obj   "cc Oct.c"   Oct.c OctP.h Oct.h
$ call make OctU.obj  "cc OctU.c"  OctU.c OctP.h Oct.h
$ call make rngs.obj    "cc rngs.c"    rngs.c
$ call make xmoct.obj "cc xmoct.c" xmoct.c Oct.h
$! Get libraries
$! if .not. axp then write optf "sys$library:vaxcrtl/lib"
$ write optf "sys$library:vaxcrtl/lib"
$ if axp then write optf "sys$library:ucx$ipc_shr/share"
$ if axp then write optf "sys$share:decw$xextlibshr/share"
$ if axp then write optf "sys$share:decw$xtlibshrr5/share"
$ if .not. axp then write optf "sys$library:ucx$ipc/lib"
$! write optf "sys$share:decw$dxmlibshr/share"
$ write optf "sys$share:decw$xmlibshr12/share"
$ write optf "sys$share:decw$xlibshr/share"
$ close optf
$!
$! LINK
$ write sys$output "Linking XmOct"
$ link/map xmoct/opt
$!
$ set noverify
$ exit
$!
$Clobber:      ! Delete executables, Purge directory and clean up object files
$!                and listings
$ delete/noconfirm/log xoct.exe;*
$ delete/noconfirm/log xmoct.exe;*
$!
$Clean:        ! Purge directory, clean up object files and listings
$ close/nolog optf
$ purge
$ delete/noconfirm/log *.lis;*
$ delete/noconfirm/log *.obj;*
$ delete/noconfirm/log *.opt;*
$ delete/noconfirm/log *.map;*
$!
$ exit
$!
! SUBROUTINE TO CHECK DEPENDENCIES
$ make: subroutine
$   v='f$verify(0)
$!   p1       What we are trying to make
$!   p2       Command to make it
$!   p3 - p8  What it depends on
$
$   if (f$extract(0,3,p2) .eqs. "cc ") then write optf "''p1'"
$   if (f$extract(0,3,p2) .eqs. "CC ") then write optf "''p1'"
$
$   if f$search(p1) .eqs. "" then goto MakeIt
$   time=f$cvtime(f$file(p1,"RDT"))
$   arg=3
$Loop:
$   argument=p'arg
$   if argument .eqs. "" then goto Exit
$   el=0
$Loop2:
$   file=f$element(el," ",argument)
$   if file .eqs. " " then goto Endl
$   afile=""
$Loop3:
$   ofile=afile
$   afile=f$search(file)
$   if afile .eqs. "" .or. afile .eqs. ofile then goto NextEl
$   if f$cvtime(f$file(afile,"RDT")) .gts. time then goto MakeIt
$   goto Loop3
$NextEL:
$   el=el+1
$   goto Loop2
$EndL:
$   arg=arg+1
$   if arg .le. 8 then goto Loop
$   goto Exit
$
$MakeIt:
$   set verify
$   'p2
$   vv='f$verify(0)
$Exit:
$   if v then set verify
$ endsubroutine
