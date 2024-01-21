$! XCMP.COM
$! compile all x files
$!
$ VMS_DEFS := "VMS,CADDR_T"
$ INCLD := "sys$common:[decw$include],[-.kernel],[-.x11]"
$ VMS_FLAGS := "/NOCASEHACK"
$   gcc [-.x11]XCMD.c 'VMS_FLAGS' /def=('VMS_DEFS') /include=('INCLD')
$   gcc [-.x11]XCONQ.c 'VMS_FLAGS' /def=('VMS_DEFS') /include=('INCLD')
$   gcc [-.x11]XDRAW.c 'VMS_FLAGS' /def=('VMS_DEFS') /include=('INCLD')
$   gcc [-.x11]xhelp.c 'VMS_FLAGS' /def=('VMS_DEFS') /include=('INCLD')
$   gcc [-.x11]ximf.c 'VMS_FLAGS' /def=('VMS_DEFS') /include=('INCLD')
$   gcc [-.x11]XINIT.c 'VMS_FLAGS' /def=('VMS_DEFS') /include=('INCLD')
$   gcc [-.x11]XMAP.c 'VMS_FLAGS' /def=('VMS_DEFS') /include=('INCLD')
$   gcc [-.x11]XOUT.c 'VMS_FLAGS' /def=('VMS_DEFS') /include=('INCLD')
$   gcc [-.x11]xtext.c 'VMS_FLAGS' /def=('VMS_DEFS') /include=('INCLD')
$   gcc [-.x11]xutil.c 'VMS_FLAGS' /def=('VMS_DEFS') /include=('INCLD')
$exit
