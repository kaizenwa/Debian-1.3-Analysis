$! Simple file to compile and link XBanner on OpenVMS
$!
$! First let's make XBanner
$!
$ inquire xpm "Which libxpm should I use (type VAX or AXP) "
$ if xpm .nes. "vax" .and. xpm .nes. "VAX" then xpm = "axp"
$ cc /define=HAS_XOM BACKG.C,COLORS.C,EFFECT.C,PIX.C,PLASMA.C,UTIL.C, -
XBANNER.C,XRES.C,RIPPLES.C
$ link XBANNER.OBJ,BACKG.OBJ,COLORS.OBJ,EFFECT.OBJ,PIX.OBJ,PLASMA.OBJ, -
UTIL.OBJ,XRES.OBJ,RIPPLES.OBJ,'xpm'_xpm.olb/lib,SYS$INPUT:/OPT
SYS$LIBRARY:DECW$XLIBSHR.EXE /SHARE
SYS$LIBRARY:VAXCRTL.EXE /SHARE
$!
$! Now let's make Freetemp
$!
$ cc FREETEMP.C
$ link FREETEMP.OBJ,UTIL.OBJ,SYS$INPUT:/OPT
SYS$LIBRARY:DECW$XLIBSHR.EXE /SHARE
SYS$LIBRARY:VAXCRTL.EXE /SHARE
$ exit
