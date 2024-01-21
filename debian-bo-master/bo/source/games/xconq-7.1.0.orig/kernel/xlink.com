$! XLINK.COM
$! Link as appropriate for an X11 based version
$! Defines a symbol xconq7 for execution
$!
$ link /exe=xconq_7.exe sys$input:/opt
actions.obj, -
AI.OBJ,            CMDLINE.OBJ,       COMBAT.OBJ, -
GENERIC.OBJ,       GRUTIL.OBJ,        HELP.OBJ,          HISTORY.OBJ,      -
INIT.OBJ,          LISP.OBJ,          MKNAMES.OBJ,      -
MKRIVERS.OBJ,      MKROADS.OBJ,       MKTERR.OBJ,        MKUNITS.OBJ,      -
MODULE.OBJ,        MPLAY.OBJ,         NLANG.OBJ,        -
PLAN.OBJ,          PS.OBJ,            READ.OBJ,         -
RUN.OBJ,           SCORE.OBJ,         SIDE.OBJ,               -
TABLES.OBJ,        TASK.OBJ,          TYPES.OBJ,         VMS.obj, -
UNIT.OBJ,          UTIL.OBJ,          VERSION.OBJ,       WORLD.OBJ,        -
WRITE.OBJ,         -
xconq.obj,	xinit.obj,	xmap.obj,	xhelp.obj                  -
xdraw.obj,	xout.obj,	xcmd.obj,	xtext.obj,                 -
ximf.obj, -
gnu_cc:[000000]gcclib/lib, -
sys$library:vaxcrtl/lib, -
sys$library:decw$dxmlibshr.exe/share, -
sys$library:decw$xlibshr.exe/share
$ xconq7 :== "$user3:[stevemci.xconq.v19.obj]xconq_7.exe"
$ exit
