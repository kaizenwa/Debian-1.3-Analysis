$!-------------------------------------------------------------------------
$!
$! MAKE.COM - Build SAOimage on a VMS system
$!
$!	Parameters:
$!		P1	null	Build everything
$!			LINK	Link SAOIMAGE.EXE
$!			CLEAN	Clean up build residue
$!			xxx	Source module - compile and update library
$!		P2	options	If P1=LINK, VMS Linker option (e.g. /DEBUG)
$!				If P1=source module, CC compiler option
$!
$!-------------------------------------------------------------------------
$
$
$ gosub BUILD_SETUP
$
$ if p1 .eqs. ""     then goto BUILD_ALL
$ if p1 .eqs. "LINK" then goto LINK_ONLY
$ if p1 .eqs. "CLEAN" then goto DISPOSE
$ 
$ gosub COMPILE_MODULE
$ exit
$
$
$BUILD_SETUP:
$
$ set noon
$
$ define/nolog  c$include    [.hfiles],decw$include,decc$library_include
$ define/nolog  vaxc$include c$include
$ define/nolog  X11          decw$include	! for #include <X11/...>
$ define/nolog  sys          decc$library_include ! for #include <sys/...>
$
$ compile := cc/stand=vaxc/extern_model=common_block/shared_globals -
  /nested_incl/define=(FITS,OIF,IMTOOL,LSB,PSCRIPT,NODEBUG)/nodebug/optimize
$ libname := libsao.olb
$ makefile:= make.lst
$
$ return
$
$
$COMPILE_MODULE:
$
$ p1 = p1 - ".C"
$ compile 'p1' 'p2' 'make_options'
$ library/log 'libname' 'p1'
$
$ return
$
$
$LINK_ONLY:
$
$ if f$locate("/DEBUG",p2) .eqs. f$length(p2)
$ then
$	outname = "SAOIMAGE"
$ else
$	outname = "D_SAOIMAGE"
$ endif
$
$ link 'p2' /nomap='outname' /exe='outname' -
	[]libsao/include=maininit/lib, -
	[.btnlib]libbtn/lib, -
	[.vms]libvms/lib, -
	sys$input/opt
sys$share:decw$xlibshr/share
$! sys$share:vaxcrtl/share
$
$ exit
$
$
$BUILD_ALL:
$
$
$ write sys$output "----- Building local source modules -----"
$
$ if f$search(libname) .eqs. "" then library/create 'libname'
$
$ open/read make_input 'makefile'
$
$ on control_y then goto CLEANUP
$
$ReadLoop:
$ read/end=ReadEOF/err=CLEANUP make_input line
$ first = f$extract(0,1,line)
$ if first .nes. "!" .and. first .nes. "#"
$ then
$	make_options = f$extract(f$locate("/",line),f$length(line),line)
$
$	p1  = f$edit(line,"UPCASE") - ".C" - make_options
$	p1s = p1 + ".C"
$	p1o = p1 + ".OBJ"
$	if f$search(p1s) .eqs. ""
$	then
$		write sys$output p1s + " not found!"
$		goto ReadLoop
$	endif
$	if f$search(p1o) .eqs. "" then goto Recompile
$
$	srcdate = f$file_attributes(p1s,"RDT")
$	objdate = f$file_attributes(p1o,"RDT")
$	bin_srcdate = f$cvtime(srcdate)
$	bin_objdate = f$cvtime(objdate)
$
$	if bin_srcdate .les. bin_objdate
$	then
$		write sys$output "Up-to-date... " + p1
$		goto ReadLoop
$	endif
$Recompile:
$	write sys$output "Compiling... " + p1
$
$ p1 = p1 - ".C"
$ compile 'p1' 'p2' 'make_options'
$ library/log 'libname' 'p1'
$
$ endif
$ goto ReadLoop
$
$ReadEOF:
$ close make_input
$
$
$ write sys$output "----- Building BTNLIB subdirectory -----"
$
$ set default [.btnlib]
$ @make
$ set default [-]
$
$
$ write sys$output "----- Building VMS subdirectory -----"
$
$ set default [.vms]
$ @make
$ set default [-]
$
$
$ write sys$output "----- Building special-case source modules -----"
$
$ set default [.panel]
$
$ libname := [-]libsao.olb
$
$ p1 := MAKEMENU.C
$ write sys$output "Compiling... " + "[.PANEL]" + p1
$ gosub COMPILE_MODULE
$
$ set default [-]
$
$
$ write sys$output "----- Linking SAOimage -----"
$
$ goto LINK_ONLY
$ exit
$
$DISPOSE:
$ delete [...]*.obj;, [...]*.olb;, spool.log;, saoimage.exe;
$ exit
$
$CLEANUP:
$ if f$logical("MAKE_INPUT") .nes. "" then close make_input
$ exit
