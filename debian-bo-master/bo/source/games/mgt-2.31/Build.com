$!
$!      This file hacked by Adrian Mariano to correctly compile the
$!      mgt program.  I don't want to compile all .c files in the
$!      current directory.  
$!
$!	This procedure checks all .C files in the current directory, and
$!	compiles any whose .OBJ file is older.  Then, all files are
$!	linked.
$!
$!	CAUTION:	.H files are not checked, so if you change one,
$!			you'll need to delete the .OBJ files of any .C
$!			files that depend on that .H file.  That will allow
$!			this BUILD.COM to recompile those .C files.
$!
$!	Usage:
$!
$!		@build [-Dname[=value]] [-g] [-ename]
$!
$!	If -D is given, a corresponding /DEFINE is fed to the C compiler.
$!
$!	If -g is given, the linker is told to link a debug version.
$!
$!	If -e is given, name is used for name of executable.  If not,
$!	sources are searched for "main(" or "main (" and that source name
$!	is used for name of executable.
$!
$!	Author:  Eric Osman   8-8-90
$!
$ on warning then goto hey_stop
$ on control_y then goto hey_stop
$ cpl = "call do_cpl"
$ q = "sys$scratch:''f$getjpi("","pid")'"

	!! Find macro definitions and link switches

$ n = 1
$ macros = ""
$ link_switches = ""
$ plup: if n .le. 8
$ then	next = p'n'
$	n = n + 1
$	qual = f$edit (f$extr(0,2,next),"upcase")
$	if qual .eqs. "-D"
$	then	definition = f$extr(2,f$len(next)-2,next)
$		macros = macros + "/define=""" + definition + """"
$	else if qual .eqs. "-E"
$	then	exe_name = f$parse(f$extr(2,f$len(next)-2,next),".exe")
$	else if qual .eqs. "-G"
$	then	link_switches = link_switches + "/debug"
$	endif
$	endif
$	endif
$	goto plup
$ endif
$
$	!! Get all dates
$	!! Begin hack 
$
$ rename wrapmgt.c wrapmgt.tmp
$ rename mou.c mou.tmp
$
$!! end hack
$
$ write sys$output "[Getting list of sources and objects]"
$ dir/date/col=1/nohead/notrail/out=temp.txt *.c.0,*.obj.0/exclude=foo.*
$
$!! begin hack
$
$ rename mou.tmp mou.c
$ rename wrapmgt.tmp wrapmgt.c
$
$!! end hack
$
$ write sys$output "[Examining dates]"
$ close/nolog b_chan
$ close/nolog link_chan
$ open b_chan temp.txt
$ open/write link_chan temp.opt
$ num = 0
$ slup:
$ read/end=nomore b_chan file_line
$ read/end=nomore b_chan date_line
$ type = f$parse (file_line,,,"type")
$ name = f$parse (file_line,,,"name")
$ if "''type'" .eqs. ".C"
$ then	src_'num' = "''name'"
$	num = num + 1
$	write link_chan name + ".obj"
$	date_tag = name + "_SRC_DATE"
$ else date_tag = name + "_OBJ_DATE"
$ endif
$ date = f$edit(date_line,"trim")
$ 'date_tag' = f$cvt(f$el(0," ",date) + ":" + f$el(1," ",date))
$ goto slup
$ nomore: close b_chan
$ close link_chan
$ n_srces = num
$ num = 0
$ dlup:
$ src_name = src_'num'
$ src_date = 'src_name'_src_date
$ if f$type ('src_name'_obj_date) .eqs. ""
$ then obj_date = ""
$ else obj_date = 'src_name'_obj_date
$ endif
$ define/user x11 decw$include:
$ cpl 'src_name' "''src_date'" "''obj_date'"
$ num = num + 1
$ if num .lt. n_srces then goto dlup
$ delete temp.txt.
$
	!! Make option file specifying libraries to link with

$ app sys$input temp.opt
sys$share:vaxcrtl.exe/share
$
	!! If name of executable hasn't been specified, find which source
	!! has main routine in it.

$ if f$type (exe_name) .eqs. ""
$ then
$	def/user sys$output nl:
$	def/user sys$error nl:
$	search/out='q'.out/window=0 *.c "main(","main ("
$	close/nolog a
$	open a 'q'.out
$	read/end=better_ask a line
$	exe_name = f$parse (".exe;0",line)
$	close a
$	delete 'q'.out.
$	goto now_we_have_exe_name
$	better_ask:
$	close a
$	read/prompt="Name for executable file: " sys$command exe_name
$ endif
$ now_we_have_exe_name:
$
	!! Link objects

$ set verify
$ link /exe='exe_name' temp/opt 'link_switches'
$ delete temp.opt. ! 'f$ver(0)'
$ exit
$ hey_stop:
$ set noverify
$ exit

$ do_cpl:
$ subroutine
$ on warning then exit
$ on control_y then goto cleanup
$ src = p1 + ".c"
$ obj = p1 + ".obj"
$ if f$search (obj) .eqs. "" then goto cpl_it
$ if p2 .lts. p3
$ then
$	write sys$output "[''obj' already o.k.]"
$	exit
$ endif
$ cpl_it:
$ def/user sys sys$library:
$ set verify
$ cc 'src' 'macros'/include=(decw$include:,sys$library:)/deb/mach/list -
    /obj=temp.obj
$ rename temp.obj 'obj' ! 'f$ver(0)'
$ exit
$ cleanup:
$ set nover
$ exit %x610
$ endsubroutine

