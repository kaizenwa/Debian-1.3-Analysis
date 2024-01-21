$!=============================================================================
$! SAOSETUP.COM -- Setup for running SAOimage on VMS  [user/site-specific]
$!
$! This file should either be invoked for all users in the system-wide
$! login procedure, or users should be told where it is or provided with
$! a symbol with which to invoke it.  The SAOIMAGE.HLP file should be
$! incorporated in either the system HELPLIB.HLB, or, preferably, in
$! a locally defined additional help library.
$!
$! Parameters:
$! -----------
$! P1 = Name of node to display on (default = 0, the local node)
$! P2 = Protocol for communicating with remote node (default = DECNET)
$!=============================================================================
$ echo = "write sys$output"
$ thisuic = f$getjpi("","uic")
$ thisnode = f$element(0,":",f$trnlnm("sys$node"))
$!
$! Determine installation directory path and set SAODIR logname.
$!
$ thisproc = f$environment("procedure")
$ procdir = f$parse(thisproc,,,"device") + f$parse(thisproc,,,"directory")
$ procnm = f$parse(thisproc,,,"name")
$!
$ define/nolog SAODIR 'procdir'
$!
$! Define SAOimage symbol and set some command line options.
$! See section 1 in [.DOC]MANUAL.TEX for complete descriptions.
$!
$ SAOIMAGE :== $SAODIR:SAOIMAGE.EXE -G +500+100 -Q
$!
$! VMS SAOimage does not read the imtoolrc file from a default system
$! location.  On VMS, SAOimage relies on the existence of the logical
$! name IMTOOLRC, which it reads from the user's process environment.
$! Individual users may override this mechanism by placing a .IMTOOLRC
$! in their SYS$LOGIN directory, or by assigning IMTOOLRC themselves.
$!
$ user_cf = "SYS$LOGIN:.IMTOOLRC"
$ if f$search(user_cf) .nes. "" then define/nolog IMTOOLRC 'user_cf'
$ if f$trnlnm("IMTOOLRC") .eqs. "" then define/nolog IMTOOLRC SAODIR:IMTOOLRC.
$!
$! SAOimage prints images to PostScript printers by executing a command
$! R_DISPOSE, which is actually a global symbol.  Here are some possible
$! definitions for that symbol; select the best one.  Please note that if
$! do not have PostScript printing capabilities, you should probably disable
$! the printing option when building SAOimage by setting NOPSCRPT in the
$! MAKE.COM file.
$!
$! If you have a VMS-based PostScript printer:
$! R_DISPOSE :== print/queue=postscript/delete "%s"
$!
$! If you have a Unix-like line printer system on your VMS machine:
$! R_DISPOSE :== lpr -Plw1 -r "%s"
$!
$! If you have no way to print PostScript (the file is NOT deleted):
$ R_DISPOSE :== 'echo' """Warning: PostScript file `%s' cannot be printed."""
$!
$! Set display to local workstation node by default.  The user may override
$! by specifying a command line argument.
$!
$ defnode = thisnode
$ usrnode = p1
$ deftrans = "DECNET"
$ usrtrans = p2
$ display = f$trnlnm("DECW$DISPLAY")
$!
$ if display .eqs. "" .or. usrnode .nes. ""
$ then
$    if usrnode .eqs. ""
$    then
$	inquire/nopunct usrnode "Node for remote display [''defnode'] : "
$	if usrnode .eqs. "" then usrnode = defnode
$	if usrnode .eqs. thisnode then usrtrans = "LOCAL"
$    endif
$    if usrtrans .eqs. "" then usrtrans = deftrans
$    if display .nes. ""
$    then
$	if f$getdvi(display,"ownuic") .eqs. thisuic
$	then
$	   set display/noperm 'display'
$	endif
$    endif
$    set display/create/node="''usrnode'"/transport='usrtrans'
$ endif
$!
$! If you don't want all the following  stuff regurgitated, exit now.
$!
$! exit
$ echo "%", procnm, "-I-STATUS, SAOimage configured for display:"
$ show display
$ echo "-", procnm, "-I-MORE, reading IRAF config from ", f$parse("IMTOOLRC")
$ if f$locate("Warning",R_DISPOSE) .lt. f$length(R_DISPOSE)
$ then
$    echo "-", procnm, "-I-MORE, printing images in PostScript format disabled"
$ else
$    echo "-", procnm, "-I-MORE, printing via `", R_DISPOSE, "'"
$ endif
$!
$ exit
