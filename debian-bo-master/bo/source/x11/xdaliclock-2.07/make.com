$! make xdaliclock under VMS
$!
$! In case of problems with the install you might contact me at
$! m.zinser@gsi.de (preferred) or eurmpz@eur.sas.com
$!
$! Look for the compiler used
$!
$ ccopt = ""
$ if f$getsyi("HW_MODEL").ge.1024
$ then
$  if f$trnlnm("SYS").eqs."" then define sys sys$library:
$ else
$  if f$search("SYS$SYSTEM:DECC$COMPILER.EXE").eqs.""
$   then
$    if f$trnlnm("SYS").eqs."" then define sys sys$library:
$   else
$    if f$trnlnm("SYS").eqs."" then define sys decc$library_include:
$  endif
$ endif
$ if f$trnlnm("X11").eqs."" then define X11 DECW$Include
$!
$!      Build the option-file
$!
$ open/write optf xdaliclock.opt
$ write optf "Identification=""XDaliclock 2.06"""
$!
$ On Error Then GoTo XUI
$ @sys$update:decw$get_image_version sys$share:decw$xlibshr.exe decw$version
$ if f$extract(4,3,decw$version).eqs."1.0"
$ then
$   write optf "Sys$share:DECW$DWTLIBSHR.EXE/Share"
$ endif
$ if f$extract(4,3,decw$version).eqs."1.1"
$ then
$   write optf "sys$share:decw$xtshr.exe/share"
$ endif
$ if f$extract(4,3,decw$version).eqs."1.2"
$ then
$   write optf "sys$share:decw$xtlibshrr5.exe/share"
$ endif
$ GoTo MAIN
$!
$ XUI:
$!
$   write optf "Sys$share:DECW$DWTLIBSHR.EXE/Share"
$ MAIN:
$ write optf "sys$share:decw$xlibshr.exe/share"
$ close optf
$!
$! Build the thing plain or with mms
$!
$ write sys$output "Compiling XDaliclock sources ..."
$ if f$search("SYS$SYSTEM:MMS.EXE").eqs.""
$  then
$   CC /NOLIST/OBJECT=XDALICLOCK.OBJ /Define = ("BUILTIN_FONT", "BUILTIN_FONT_2") - -
     /Include = ([.numbers], [.numbers2]) XDALICLOCK.C
$   CC /NOLIST/OBJECT=DIGITAL.OBJ /Define = ("BUILTIN_FONT", "BUILTIN_FONT_2") -
     /Include = ([.numbers], [.numbers2]) DIGITAL.C
$   CC /NOLIST/OBJECT=COLORS.OBJ /Define = ("BUILTIN_FONT", "BUILTIN_FONT_2") -
     /Include = ([.numbers], [.numbers2]) COLORS.C
$   CC /NOLIST/OBJECT=RESOURCES.OBJ /Define = ("BUILTIN_FONT", "BUILTIN_FONT_2") -
     /Include = ([.numbers], [.numbers2]) RESOURCES.C
$   CC /NOLIST/OBJECT=VISUAL.OBJ /Define = ("BUILTIN_FONT", "BUILTIN_FONT_2") -
     /Include = ([.numbers], [.numbers2]) VISUAL.C
$   CC /NOLIST/OBJECT=HSV.OBJ /Define = ("BUILTIN_FONT", "BUILTIN_FONT_2") -
     /Include = ([.numbers], [.numbers2]) HSV.C
$   CC /NOLIST/OBJECT=USLEEP.OBJ /Define = ("BUILTIN_FONT", "BUILTIN_FONT_2") -
     /Include = ([.numbers], [.numbers2]) USLEEP.C
$   LINK /TRACE/NOMAP/EXEC=XDALICLOCK.EXE /NoTraceBack xdaliclock,digital.obj, -
    colors.obj,resources.obj,visual.obj,hsv.obj,usleep.obj,xdaliclock.opt/Option
$  else
$   mms/des=makefile.mms
$ endif
$ write sys$output "XDaliclock build completed"
$ exit
