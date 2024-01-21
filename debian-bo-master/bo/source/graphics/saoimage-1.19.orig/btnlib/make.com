$!-------------------------------------------------------------------------
$!
$! MAKE.COM - Build SAOimage Button library
$!
$!	Parameters:
$!		P1	null	Compile and update all sources
$!			xxx	Source module - compile and update library
$!		P2	options	CC compiler options
$!
$!-------------------------------------------------------------------------
$
$
$ gosub BUILD_SETUP
$
$ if p1 .eqs. ""     then goto BUILD_ALL
$
$ gosub COMPILE_MODULE
$ exit
$
$
$BUILD_SETUP:
$
$ set noon
$
$ define/nolog  c$include     [-.hfiles],decw$include,decc$library_include
$ define/nolog  vaxc$include  c$include
$ define/nolog  X11           decw$include
$ define/nolog  sys           decc$library_include
$
$ compile := cc/stand=vaxc/extern_model=common_block/shared_globals -
  /nested_incl/define=(IMTOOL)/nodebug/optimize
$ libname := libbtn.olb
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
$BUILD_ALL:
$
$ if f$search(libname) .eqs. "" then library/create 'libname'
$
$ open/read make_input 'makefile'
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
$ exit
$
$
$CLEANUP:
$ if f$logical("MAKE_INPUT") .nes. "" then close make_input
$ exit
