! VMS MAKEFILE (DESCRIP.MSS)
! 25-MAR-91 GJC@MITECH.COM
!
! Before running this define these logical names:
! $ DEFINE X11 DECW$INCLUDE
! $ DEFINE SYS SYS$LIBRARY
!
! @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/descrip.mms,v 3.0 1996/05/06 16:04:27 william Exp $
!

CFLAGS = $(DEFINES)/debug/nooptimize/list/show=(nosource)/object=$(mms$target_name).obj

LFLAGS = /DEBUG/MAP

DEFINES = /DEFINE=(TGIF_PATH="""/user$disk/gjc/prototypes/tgif""",-
		   PSFILE_MOD="0664",-
		   EPSF_FILE_EXT="""eps""",-
		   PS_FILE_EXT="""ps""",-
		   XBM_FILE_EXT="""xbm""",-
		   XPM_FILE_EXT="""xpm""",-
		   OBJ_FILE_EXT="""obj""",-
		   SYM_FILE_EXT="""sym""",-
		   TEXT_FILE_EXT="""txt""",-
		   TELEPORT_ATTR="""warp_to=""",-
		   LAUNCH_ATTR="""launch=""",-
		   EXEC_ATTR="""exec=""",-
		   TMP_DIR="""/sys$scratch/""")

!_NO_GETTIMEOFDAY
!   Include the above line if you do not have the gettimeofday()
!   function but have the ftime() function.
!BSD_COMP
!   Include the above line if you are compiling on a Solaris (or
!   possibly an SGI) machine which requires it in <sys/ioctl.h>.
!   (If you see the error that FIONBIO is not defined when compiling
!   tcp.c, then you probably need this.)
!_HANDLE_SIGCHLD
!   Include the above line if there are defunct tgif child processes.
!_NO_LSTAT
!   Include the above line if lstat() is not available.  In this case,
!   using BrowseXBitmap and BrowseXPixmap under the File Menu may cause
!   infinite loop if the directory structure is infinitely recursive.
!_USE_XDRAWPOINT_TO_PUT_A_POINT
!   Include the above line if the grid points are not visible.  This can
!   be used to bypass some X server bugs (such as certain Linux X server
!   using VGA16 screen) where the XDrawLine() Xlib call does not work
!   properly.
!_NO_EXTERN
!   Include the above line in the DEFINES if you don't want to include
!   the extern definition for functions defined in a module.  For example,
!   if this option is set, then file "foo.c" will not include "foo.e",
!   and certain other extern definition in "foo.c" will be omitted.
!_NO_PROTO
!   Include the above line in the DEFINES if checking for function
!   prototypes is not desired.  Checking for function prototypes
!   are automatically enabled if you are using an ANSI or C++ compilor.
!   However, not all ANSI/C++ compilors are created equal.
!NO_STRSTR
!   Include the above line in the DEFINES if you do not have the strstr()
!   function in your system's string library.
!INVERT_CTM_BUG
!   Include the above line in the DEFINES if you are using PostScript
!   utilities which fails when trying to invert non-invertible CTM
!   (e.g., muluti.ps from ftp.ucc.su.oz.au:/pub/ps_printing/multi).
!HP_LINE_BUG
!   Include the above line in the DEFINES if two-vertices horizontal and
!   verticle line segments don't draw properly on a buggy HP server.
!DONTFREECOLORS
!   Include the above line in the DEFINES if you don't want the black and
!   the white colors to be freed when tgif initializes itself.  You should
!   not need to do this unless to get around some server bugs.
!USE_XAPPLRESDIR
!   Include the above line in the DEFINES if $XAPPLRESDIR/Tgif is used to
!   specify the resource file.  If your system does not have putenv()
!   (such as IBM-RT), then this won't work.  I'm not sure if VMS systems
!   have putenv(), so this might be useless for VMS systems.
!XAPPLOADDIR_DEF
!   If you define USE_XAPPLRESDIR, define this to a default directory
!   to use in case $XAPPLRESDIR isn't set.
!USE_XT_INITIALIZE
!   This will cause the XToolkit to be initialized so that the X defaults
!   are obtained from the Xt directories.  This should only be used when
!   USE_XAPPLRESDIR option is NOT used.
!NO_THIN_LINE
!   Include the above line in the DEFINES if a line width of 0 causes
!   your server to crash.  This is to get around a bug in some X servers.
!THIN_OVAL_AND_ARC
!   Include the above line in the DEFINES if you want 0 width arcs to
!   be used when the width of an arc is less than or equal to 1.
!   (Sun3 server has a bug that makes drawing ovals and arcs with
!   width other than 0 VERY slow).
!KEEP_WHEN_PRINT
!   Include the above line in the DEFINES if you don't want the temp
!   file to be deleted after sending the print job to the printer.
!   (Some VMS systems might require this flag to be used.)
!PRINT_CMD="""print"""
!   Include the above line in the DEFINES if you want to configure tgif
!   to use a default print command.
!NOTR4MODE
!   Include the above line in the DEFINES if you are running X11R3.
!   This will cause tgif NOT to use some X11R4 window manager related
!   functions that's not present in R3.
!MAPBEFORESELECT
!   Include the above line in the DEFINES to call XMapWindow() before
!   calling XSelectInput().  This might be needed to get around some
!   server bugs.
!DONT_FREE_COLORMAP="""true"""
!   Include the above line in the DEFINES if you don't want the colormap
!   to be freed when tgif exits.
!A4PAPER
!   Include the above line in the DEFINES if you want to print on A4 paper
!   (8.25in by 11.7in or about 21cm by 29.7cm).

OBJ1 =	align.obj,animate.obj,arc.obj,attr.obj,auxtext.obj,-
	box.obj,button.obj,-
	choice.obj,cmd.obj,color.obj,cutpaste.obj,cursor.obj,-
	dialog.obj,drawing.obj,dup.obj,-
	edit.obj,eps.obj,exec.obj,expr.obj,-
	file.obj,font.obj,ftp.obj,-
	grid.obj,group.obj,-
	help.obj,http.obj,-
	imgproc.obj,import.obj,-
	mainloop.obj,mainmenu.obj,mark.obj,menu.obj,move.obj,msg.obj,-
	names.obj,navigate.obj,-
	obj.obj,oval.obj,-
	page.obj,pattern.obj,poly.obj,polygon.obj,ps.o,-
	raster.obj,rcbox.obj,rect.obj,remote.obj,ruler.obj,-
	scroll.obj,select.obj,setup.obj,shape.obj,shortcut.obj-
		special.obj,spline.obj,stk.obj,stretch.obj,-
	tgif_dbg.obj,-
	tcp.obj,text.obj,-
	util.obj,-
	version.obj,vms_comp.obj,-
	wb1.obj,wb2.obj,wb3.obj,-
	xbitmap.obj,xpixmap.obj

OBJS1 = tgif.obj,$(OBJ1)

tgif.exe depends_on $(OBJS1)
 link$(LFLAGS)/exe=tgif.exe $(OBJS1),descrip.opt/opt
 setdebug tgif.exe 0

prtgif.exe depends_on prtgif.obj
 link$(LFLAGS)/exe=prtgif.exe prtgif.obj,descrip.opt/opt
 setdebug prtgif.exe 0

testdrive.exe depends_on testdrive.obj,$(OBJ1)
 link$(LFLAGS)/exe=testdrive.exe testdrive.obj,$(OBJ1),descrip.opt/opt
 setdebug testdrive.exe 0

! -------------------------------------------------------------------------
! dependencies generated by makedepend and noimake

align.obj depends_on align.c,const.h,tgif_dbg.h,types.h,align.e,auxtext.e,-
		button.e,choice.e,cmd.e,color.e,dialog.e,drawing.e,dup.e,-
		grid.e,mark.e,mainmenu.e,menu.e,move.e,msg.e,obj.e,poly.e,-
		raster.e,select.e,setup.e
animate.obj depends_on animate.c,const.h,tgif_dbg.h,types.h,animate.e,color.e,-
		dialog.e,msg.e,poly.e,raster.e,select.e,setup.e
arc.obj depends_on arc.c,const.h,tgif_dbg.h,types.h,arc.e,attr.e,auxtext.e,-
		choice.e,cmd.e,color.e,cursor.e,dialog.e,dup.e,file.e,grid.e,-
		mainloop.e,mark.e,msg.e,obj.e,pattern.e,poly.e,ps.e,raster.e,-
		ruler.e,select.e,setup.e,special.e,spline.e,util.e,xpixmap.e
attr.obj depends_on attr.c,const.h,tgif_dbg.h,types.h,attr.e,auxtext.e,-
		button.e,choice.e,choose.e,cmd.e,color.e,cursor.e,dialog.e,-
		drawing.e,dup.e,file.e,font.e,grid.e,mainloop.e,mainmenu.e,-
		mark.e,menu.e,move.e,msg.e,names.e,obj.e,pattern.e,raster.e,-
		rect.e,ruler.e,select.e,setup.e,stk.e,text.e,util.e
auxtext.obj depends_on auxtext.c,const.h,tgif_dbg.h,types.h,auxtext.e,color.e,-
		cursor.e,dialog.e,file.e,font.e,msg.e,obj.e,pattern.e,prtgif.e,-
		ps.e,raster.e,rect.e,setup.e,text.e,util.e,xbitmap.e,xpixmap.e
box.obj depends_on box.c,const.h,tgif_dbg.h,types.h,attr.e,auxtext.e,box.e,-
		cmd.e,color.e,cursor.e,file.e,grid.e,mainloop.e,msg.e,obj.e,-
		pattern.e,poly.e,ps.e,raster.e,ruler.e,select.e,setup.e,util.e,-
		xpixmap.e
button.obj depends_on button.c,const.h,tgif_dbg.h,types.h,auxtext.e,box.e,-
		button.e,cursor.e,file.e,font.e,mainloop.e,mainmenu.e,msg.e,-
		raster.e,rect.e,setup.e
choice.obj depends_on choice.c,const.h,tgif_dbg.h,types.h,align.e,arc.e,box.e,-
		choice.e,color.e,cursor.e,dialog.e,drawing.e,edit.e,file.e,-
		font.e,grid.e,mainloop.e,mainmenu.e,mark.e,msg.e,navigate.e,-
		oval.e,page.e,pattern.e,poly.e,polygon.e,raster.e,rcbox.e,-
		select.e,setup.e,shape.e,special.e,stretch.e,text.e,xbitmap.e,-
		xbm/abc.xbm
choose.obj depends_on choose.c,const.h,tgif_dbg.h,types.h,button.e,choose.e,-
		cutpaste.e,cursor.e,dialog.e,file.e,font.e,mainloop.e,-
		mainmenu.e,msg.e,names.e,raster.e,setup.e,util.e
cmd.obj depends_on cmd.c,const.h,tgif_dbg.h,types.h,attr.e,choice.e,cmd.e,-
		color.e,dialog.e,drawing.e,dup.e,imgproc.e,mark.e,move.e,msg.e,-
		obj.e,page.e,select.e,setup.e,stk.e,xpixmap.e
color.obj depends_on color.c,const.h,tgif_dbg.h,types.h,choice.e,cmd.e,color.e,-
		dialog.e,drawing.e,file.e,font.e,imgproc.e,mainloop.e,mark.e,-
		menu.e,msg.e,obj.e,page.e,pattern.e,raster.e,select.e,setup.e,-
		text.e,util.e,xpixmap.e
cursor.obj depends_on cursor.c,const.h,tgif_dbg.h,types.h,choice.e,color.e,-
		cursor.e,setup.e,xbitmap.e,xbm/null.xbm,xbm/nullmask.xbm,-
		xbm/text_cur.xbm,xbm/helphand.xbm,xbm/hhand_mk.xbm,xbm/mag.xbm,-
		xbm/mag_mask.xbm,xbm/flood.xbm,xbm/flood_mk.xbm,xbm/drip.xbm,-
		xbm/drip_msk.xbm
cutpaste.obj depends_on cutpaste.c,const.h,tgif_dbg.h,types.h,auxtext.e,box.e,-
		choice.e,cmd.e,color.e,cutpaste.e,cursor.e,dialog.e,drawing.e,-
		dup.e,file.e,font.e,grid.e,mark.e,move.e,msg.e,names.e,obj.e,-
		page.e,pattern.e,remote.e,select.e,setup.e,special.e,text.e
dialog.obj depends_on dialog.c,const.h,tgif_dbg.h,types.h,auxtext.e,box.e,-
		button.e,cutpaste.e,cursor.e,dialog.e,drawing.e,file.e,font.e,-
		mainloop.e,mainmenu.e,msg.e,raster.e,setup.e,text.e,util.e,-
		xbm/info.xbm
drawing.obj depends_on drawing.c,const.h,tgif_dbg.h,types.h,align.e,animate.e,-
		arc.e,attr.e,auxtext.e,box.e,choice.e,cmd.e,color.e,cutpaste.e,-
		cursor.e,dialog.e,drawing.e,dup.e,edit.e,eps.e,exec.e,file.e,-
		font.e,grid.e,group.e,imgproc.e,import.e,mark.e,mainloop.e,-
		menu.e,msg.e,navigate.e,names.e,obj.e,oval.e,page.e,pattern.e,-
		poly.e,polygon.e,raster.e,rcbox.e,rect.e,remote.e,ruler.e,-
		scroll.e,select.e,setup.e,shortcut.e,special.e,stk.e,stretch.e,-
		text.e,xbitmap.e,xpixmap.e,xbm/intr.xbm,xbm/trek.xbm
dup.obj depends_on dup.c,const.h,tgif_dbg.h,types.h,attr.e,auxtext.e,choice.e,-
		cmd.e,drawing.e,dup.e,grid.e,mark.e,move.e,msg.e,obj.e,page.e,-
		raster.e,select.e,setup.e,text.e,util.e,xbitmap.e,xpixmap.e
edit.obj depends_on edit.c,const.h,tgif_dbg.h,types.h,align.e,arc.e,attr.e,-
		auxtext.e,button.e,choice.e,cmd.e,color.e,cutpaste.e,cursor.e,-
		dialog.e,drawing.e,dup.e,edit.e,eps.e,font.e,grid.e,group.e,-
		mainloop.e,mark.e,menu.e,move.e,msg.e,names.e,obj.e,page.e,-
		pattern.e,poly.e,raster.e,ruler.e,select.e,setup.e,special.e,-
		spline.e,stretch.e,text.e,util.e,xbitmap.e,xpixmap.e
eps.obj depends_on eps.c,const.h,tgif_dbg.h,types.h,cmd.e,color.e,cursor.e,-
		dialog.e,drawing.e,dup.e,eps.e,file.e,grid.e,mark.e,msg.e,-
		obj.e,pattern.e,select.e,setup.e,util.e,xbitmap.e
exec.obj depends_on exec.c,const.h,tgif_dbg.h,types.h,align.e,attr.e,auxtext.e,-
		cmd.e,color.e,choice.e,cursor.e,dialog.e,drawing.e,dup.e,eps.e,-
		exec.e,expr.e,file.e,font.e,mainloop.e,menu.e,move.e,msg.e,-
		names.e,navigate.e,obj.e,pattern.e,poly.e,raster.e,remote.e,-
		select.e,setup.e,stk.e,stretch.e,text.e,util.e,xbitmap.e,-
		xpixmap.e
expr.obj depends_on expr.c,const.h,tgif_dbg.h,types.h,dialog.e,expr.e,msg.e,-
		setup.e,util.e
file.obj depends_on file.c,const.h,tgif_dbg.h,patchlvl.h,types.h,align.e,arc.e,-
		attr.e,auxtext.e,box.e,button.e,choice.e,cmd.e,color.e,-
		cutpaste.e,cursor.e,dialog.e,drawing.e,dup.e,eps.e,file.e,-
		font.e,grid.e,group.e,import.e,mainloop.e,mainmenu.e,mark.e,-
		menu.e,move.e,msg.e,names.e,navigate.e,obj.e,oval.e,page.e,-
		pattern.e,poly.e,polygon.e,prtgif.e,ps.e,raster.e,rcbox.e,-
		rect.e,remote.e,ruler.e,scroll.e,select.e,setup.e,special.e,-
		stk.e,stretch.e,text.e,util.e,version.e,xbitmap.e,xpixmap.e
font.obj depends_on font.c,const.h,tgif_dbg.h,types.h,auxtext.e,choice.e,cmd.e,-
		color.e,cursor.e,dialog.e,drawing.e,exec.e,file.e,font.e,-
		mainmenu.e,mark.e,menu.e,msg.e,obj.e,pattern.e,page.e,ps.e,-
		raster.e,select.e,setup.e,text.e,util.e,xpixmap.e
frontend.obj depends_on frontend.c
ftp.obj depends_on ftp.c,const.h,tgif_dbg.h,ftp.e,remote.e,tcp.e,util.e
grid.obj depends_on grid.c,const.h,tgif_dbg.h,types.h,choice.e,color.e,-
		cursor.e,dialog.e,drawing.e,dup.e,file.e,grid.e,mainmenu.e,-
		menu.e,msg.e,obj.e,page.e,pattern.e,raster.e,ruler.e,scroll.e,-
		select.e,setup.e,stretch.e,text.e
group.obj depends_on group.c,const.h,tgif_dbg.h,types.h,attr.e,choice.e,cmd.e,-
		drawing.e,dup.e,file.e,group.e,mark.e,msg.e,obj.e,page.e,-
		select.e,setup.e
help.obj depends_on help.c,const.h,tgif_dbg.h,types.h,patchlvl.h,color.e,-
		dialog.e,help.e,menu.e,msg.e,setup.e,util.e,version.e
http.obj depends_on http.c,const.h,tgif_dbg.h,patchlvl.h,http.e,remote.e,tcp.e,-
		util.e,version.e
imgproc.obj depends_on imgproc.c,const.h,tgif_dbg.h,types.h,choice.e,cmd.e,-
		color.e,cursor.e,dialog.e,drawing.e,dup.e,file.e,grid.e,-
		imgproc.e,mainloop.e,mainmenu.e,menu.e,msg.e,names.e,obj.e,-
		page.e,raster.e,select.e,setup.e,util.e,xbitmap.e,xpixmap.e
import.obj depends_on import.c,const.h,tgif_dbg.h,types.h,choose.e,color.e,-
		dialog.e,drawing.e,dup.e,eps.e,file.e,import.e,msg.e,menu.e,-
		names.e,obj.e,select.e,setup.e,text.e,util.e,xbitmap.e,-
		xpixmap.e
mainloop.obj depends_on mainloop.c,const.h,tgif_dbg.h,patchlvl.h,types.h,-
		animate.e,auxtext.e,choice.e,cmd.e,color.e,cutpaste.e,cursor.e,-
		dialog.e,drawing.e,exec.e,expr.e,file.e,font.e,grid.e,help.e,-
		imgproc.e,import.e,mainloop.e,mainmenu.e,menu.e,msg.e,names.e,-
		navigate.e,obj.e,page.e,ps.e,raster.e,remote.e,ruler.e,-
		scroll.e,select.e,setup.e,shape.e,shortcut.e,stk.e,text.e,-
		version.e,xbitmap.e,xpixmap.e
mainmenu.obj depends_on mainmenu.c,const.h,tgif_dbg.h,types.h,align.e,choice.e,-
		color.e,cursor.e,edit.e,file.e,font.e,grid.e,help.e,imgproc.e,-
		mainloop.e,mainmenu.e,menu.e,msg.e,navigate.e,page.e,pattern.e,-
		raster.e,setup.e,shape.e,special.e,stretch.e,text.e
mark.obj depends_on mark.c,const.h,tgif_dbg.h,types.h,choice.e,exec.e,mark.e,-
		obj.e,raster.e,rect.e,setup.e,select.e
menu.obj depends_on menu.c,const.h,tgif_dbg.h,patchlvl.h,types.h,align.e,box.e,-
		choice.e,cmd.e,color.e,cursor.e,dialog.e,drawing.e,edit.e,-
		exec.e,file.e,font.e,grid.e,imgproc.e,mainloop.e,mainmenu.e,-
		menu.e,move.e,msg.e,names.e,navigate.e,obj.e,page.e,pattern.e,-
		raster.e,rect.e,remote.e,select.e,setup.e,shape.e,special.e,-
		stk.e,text.e,version.e
move.obj depends_on move.c,const.h,tgif_dbg.h,types.h,arc.e,attr.e,cmd.e,-
		cursor.e,drawing.e,dup.e,grid.e,mainloop.e,mark.e,move.e,msg.e,-
		names.e,obj.e,oval.e,poly.e,raster.e,rcbox.e,rect.e,ruler.e,-
		select.e,setup.e,spline.e,stretch.e
msg.obj depends_on msg.c,const.h,tgif_dbg.h,types.h,patchlvl.h,button.e,-
		cutpaste.e,cursor.e,dialog.e,file.e,font.e,grid.e,mainloop.e,-
		menu.e,msg.e,navigate.e,pattern.e,ps.e,raster.e,setup.e,util.e,-
		version.e,xbitmap.e,xbm/btn1.xbm
names.obj depends_on names.c,const.h,tgif_dbg.h,types.h,auxtext.e,box.e,-
		button.e,choose.e,cutpaste.e,cursor.e,dialog.e,drawing.e,-
		file.e,font.e,import.e,mainloop.e,mainmenu.e,menu.e,msg.e,-
		names.e,navigate.e,raster.e,rect.e,remote.e,setup.e,util.e,-
		xpixmap.e
navigate.obj depends_on navigate.c,const.h,tgif_dbg.h,types.h,auxtext.e,-
		button.e,choice.e,choose.e,cmd.e,color.e,cursor.e,dialog.e,-
		drawing.e,file.e,font.e,mainloop.e,mainmenu.e,menu.e,msg.e,-
		names.e,navigate.e,obj.e,page.e,rect.e,remote.e,scroll.e,-
		select.e,setup.e,stk.e,util.e
obj.obj depends_on obj.c,const.h,tgif_dbg.h,types.h,arc.e,attr.e,auxtext.e,-
		box.e,cmd.e,cursor.e,group.e,msg.e,obj.e,oval.e,page.e,poly.e,-
		polygon.e,rcbox.e,rect.e,setup.e,spline.e,stretch.e,text.e,-
		xbitmap.e,xpixmap.e
oval.obj depends_on oval.c,const.h,tgif_dbg.h,types.h,attr.e,auxtext.e,cmd.e,-
		color.e,cursor.e,file.e,grid.e,mainloop.e,msg.e,obj.e,oval.e,-
		pattern.e,poly.e,ps.e,raster.e,rect.e,ruler.e,select.e,setup.e,-
		spline.e,util.e,xpixmap.e
page.obj depends_on page.c,const.h,tgif_dbg.h,types.h,auxtext.e,button.e,cmd.e,-
		choice.e,choose.e,color.e,cursor.e,dialog.e,drawing.e,file.e,-
		font.e,grid.e,mark.e,mainloop.e,mainmenu.e,menu.e,move.e,msg.e,-
		names.e,obj.e,page.e,raster.e,rect.e,scroll.e,select.e,setup.e,-
		stk.e,util.e
pattern.obj depends_on pattern.c,const.h,tgif_dbg.h,types.h,arc.e,choice.e,-
		color.e,cmd.e,dialog.e,drawing.e,file.e,font.e,mainmenu.e,-
		mark.e,menu.e,msg.e,obj.e,pattern.e,poly.e,raster.e,select.e,-
		setup.e,spline.e,text.e,util.e
poly.obj depends_on poly.c,const.h,tgif_dbg.h,types.h,attr.e,auxtext.e,-
		choice.e,cmd.e,color.e,cursor.e,dialog.e,drawing.e,dup.e,-
		file.e,grid.e,mainloop.e,msg.e,obj.e,pattern.e,poly.e,-
		polygon.e,ps.e,raster.e,rect.e,ruler.e,select.e,setup.e,-
		spline.e,stretch.e,util.e,xpixmap.e
polygon.obj depends_on polygon.c,const.h,tgif_dbg.h,types.h,attr.e,box.e,cmd.e,-
		color.e,choice.e,cursor.e,dialog.e,drawing.e,dup.e,grid.e,-
		file.e,mainloop.e,mark.e,msg.e,obj.e,pattern.e,poly.e,-
		polygon.e,ps.e,raster.e,rect.e,ruler.e,select.e,setup.e,-
		spline.e,util.e,xpixmap.e
prtgif.obj depends_on prtgif.c,const.h,tgif_dbg.h,prtgif.e
ps.obj depends_on ps.c,const.h,tgif_dbg.h,types.h,msg.e,ps.e,util.e
raster.obj depends_on raster.c,const.h,tgif_dbg.h,types.h,choice.e,color.e,-
		dialog.e,file.e,font.e,pattern.e,poly.e,shape.e,raster.e,-
		setup.e,util.e,xbm/arrow.xbm,xbm/text.xbm,xbm/box.xbm,-
		xbm/oval.xbm,xbm/poly.xbm,xbm/polygon.xbm,xbm/arc.xbm,-
		xbm/rcbox.xbm,xbm/freehand.xbm,xbm/vmode.xbm,xbm/rot_mode.xbm,-
		xbm/pat0.xbm,xbm/pat1.xbm,xbm/pat2.xbm,xbm/pat3.xbm,-
		xbm/pat4.xbm,xbm/pat5.xbm,xbm/pat6.xbm,xbm/pat7.xbm,-
		xbm/pat8.xbm,xbm/pat9.xbm,xbm/pat10.xbm,xbm/pat11.xbm,-
		xbm/pat12.xbm,xbm/pat13.xbm,xbm/pat14.xbm,xbm/pat15.xbm,-
		xbm/pat16.xbm,xbm/pat17.xbm,xbm/pat18.xbm,xbm/pat19.xbm,-
		xbm/pat20.xbm,xbm/pat21.xbm,xbm/pat22.xbm,xbm/pat23.xbm,-
		xbm/pat24.xbm,xbm/pat25.xbm,xbm/pat26.xbm,xbm/pat27.xbm,-
		xbm/pat28.xbm,xbm/pat29.xbm,xbm/pat30.xbm,xbm/pat31.xbm,-
		xbm/pat32.xbm,xbm/shape0.xbm,xbm/shape1.xbm,xbm/shape2.xbm,-
		xbm/shape3.xbm,xbm/shape4.xbm,xbm/shape5.xbm,xbm/shape6.xbm,-
		xbm/shape7.xbm,xbm/shape8.xbm,xbm/shape9.xbm,xbm/shape10.xbm,-
		xbm/shape11.xbm,xbm/shape12.xbm,xbm/shape13.xbm,-
		xbm/shape14.xbm,xbm/shape15.xbm,xbm/shape16.xbm,-
		xbm/shape17.xbm,xbm/shape18.xbm,xbm/shape19.xbm,xbm/just_l.xbm,-
		xbm/just_c.xbm,xbm/just_r.xbm,xbm/align_n.xbm,xbm/align_l.xbm,-
		xbm/align_c.xbm,xbm/align_r.xbm,xbm/align_t.xbm,-
		xbm/align_m.xbm,xbm/align_b.xbm,xbm/align_s.xbm,xbm/lw0.xbm,-
		xbm/lw1.xbm,xbm/lw2.xbm,xbm/lw3.xbm,xbm/lw4.xbm,xbm/lw5.xbm,-
		xbm/lw6.xbm,xbm/lt0.xbm,xbm/lt1.xbm,xbm/lt2.xbm,xbm/ls0.xbm,-
		xbm/ls1.xbm,xbm/ls2.xbm,xbm/ls3.xbm,xbm/ld0.xbm,xbm/ld1.xbm,-
		xbm/ld2.xbm,xbm/ld3.xbm,xbm/ld4.xbm,xbm/ld5.xbm,xbm/ld6.xbm,-
		xbm/ld7.xbm,xbm/ld8.xbm,xbm/lw0s.xbm,xbm/lw1s.xbm,xbm/lw2s.xbm,-
		xbm/lw3s.xbm,xbm/lw4s.xbm,xbm/lw5s.xbm,xbm/lw6s.xbm,-
		xbm/lt0s.xbm,xbm/lt1s.xbm,xbm/lt2s.xbm,xbm/ls0s.xbm,-
		xbm/ls1s.xbm,xbm/ls2s.xbm,xbm/ls3s.xbm,xbm/ld0s.xbm,-
		xbm/ld1s.xbm,xbm/ld2s.xbm,xbm/ld3s.xbm,xbm/ld4s.xbm,-
		xbm/ld5s.xbm,xbm/ld6s.xbm,xbm/ld7s.xbm,xbm/ld8s.xbm,-
		xbm/printer.xbm,xbm/latex.xbm,xbm/psfile.xbm,xbm/xbm.xbm,-
		xbm/ascii.xbm,xbm/epsi.xbm,xbm/gif.xbm,xbm/html.xbm,-
		xbm/file.xbm,xbm/landscap.xbm,xbm/special.xbm,xbm/vspace.xbm,-
		xbm/rcb_rad.xbm,xbm/const_mv.xbm,xbm/uncon_mv.xbm,xbm/edit.xbm,-
		xbm/intr.xbm,xbm/intr90.xbm,xbm/trek.xbm,xbm/stack.xbm,-
		xbm/tile.xbm,xbm/leftend.xbm,xbm/lfarrow.xbm,xbm/rtarrow.xbm,-
		xbm/rightend.xbm,xbm/upend.xbm,xbm/uparrow.xbm,xbm/dnarrow.xbm,-
		xbm/downend.xbm,xbm/chkall.xbm,xbm/unchkall.xbm,-
		xbm/stretch.xbm,xbm/nstretch.xbm,xbm/rot_0.xbm,xbm/rot_90.xbm,-
		xbm/rot_180.xbm,xbm/rot_270.xbm,xbm/btn1.xbm,xbm/btn2.xbm,-
		xbm/btn3.xbm,xbm/stop.xbm,xbm/question.xbm,xbm/info.xbm,-
		xbm/dialog.xbm
rcbox.obj depends_on rcbox.c,const.h,tgif_dbg.h,types.h,attr.e,auxtext.e,box.e,-
		cmd.e,color.e,cursor.e,file.e,grid.e,mainloop.e,msg.e,obj.e,-
		pattern.e,poly.e,ps.e,raster.e,rcbox.e,ruler.e,select.e,-
		setup.e,spline.e,util.e,xpixmap.e
rect.obj depends_on rect.c,const.h,tgif_dbg.h,types.h,arc.e,color.e,dialog.e,-
		poly.e,rect.e,setup.e,spline.e
remote.obj depends_on remote.c,const.h,tgif_dbg.h,types.h,patchlvl.h,-
		cutpaste.e,dialog.e,drawing.e,file.e,ftp.e,http.e,menu.e,msg.e,-
		names.e,navigate.e,page.e,remote.e,setup.e,tcp.e,util.e,-
		version.e
ruler.obj depends_on ruler.c,const.h,tgif_dbg.h,types.h,cursor.e,dialog.e,-
		font.e,grid.e,mainmenu.e,msg.e,raster.e,ruler.e,setup.e,util.e
scroll.obj depends_on scroll.c,const.h,tgif_dbg.h,types.h,choice.e,cursor.e,-
		drawing.e,dup.e,grid.e,mainloop.e,msg.e,obj.e,page.e,raster.e,-
		ruler.e,scroll.e,setup.e,text.e
select.obj depends_on select.c,const.h,tgif_dbg.h,types.h,button.e,choice.e,-
		cmd.e,color.e,cursor.e,dialog.e,drawing.e,dup.e,exec.e,file.e,-
		font.e,grid.e,group.e,mainloop.e,mark.e,menu.e,move.e,msg.e,-
		names.e,obj.e,page.e,poly.e,raster.e,rect.e,remote.e,ruler.e,-
		scroll.e,select.e,setup.e,stk.e,stretch.e
setup.obj depends_on setup.c,const.h,tgif_dbg.h,types.h,attr.e,auxtext.e,-
		choice.e,cmd.e,color.e,cursor.e,drawing.e,dup.e,file.e,font.e,-
		grid.e,help.e,imgproc.e,import.e,mainloop.e,mainmenu.e,mark.e,-
		menu.e,move.e,msg.e,names.e,page.e,pattern.e,ps.e,raster.e,-
		remote.e,ruler.e,scroll.e,select.e,setup.e,shape.e,shortcut.e,-
		spline.e,stk.e,stretch.e,text.e,xbitmap.e,xpixmap.e,-
		tgificon.xbm,xbm/btn1.xbm
shape.obj depends_on shape.c,const.h,tgif_dbg.h,types.h,arc.e,attr.e,cmd.e,-
		color.e,cursor.e,dialog.e,drawing.e,dup.e,group.e,menu.e,msg.e,-
		obj.e,pattern.e,poly.e,polygon.e,raster.e,select.e,setup.e,-
		shape.e,spline.e,text.e,util.e
shortcut.obj depends_on shortcut.c,const.h,tgif_dbg.h,types.h,setup.e,-
		shortcut.e
special.obj depends_on special.c,const.h,tgif_dbg.h,types.h,animate.e,attr.e,-
		auxtext.e,choice.e,cmd.e,color.e,cutpaste.e,cursor.e,dialog.e,-
		drawing.e,dup.e,edit.e,exec.e,file.e,grid.e,group.e,mainloop.e,-
		mark.e,menu.e,msg.e,move.e,names.e,obj.e,page.e,raster.e,-
		remote.e,ruler.e,scroll.e,select.e,setup.e,special.e,stk.e,-
		text.e,util.e
spline.obj depends_on spline.c,const.h,tgif_dbg.h,types.h,dialog.e,poly.e,-
		polygon.e,raster.e,rect.e,setup.e,spline.e
stk.obj depends_on stk.c,const.h,tgif_dbg.h,types.h,align.e,attr.e,box.e,-
		button.e,choice.e,cmd.e,color.e,cursor.e,dialog.e,drawing.e,-
		dup.e,file.e,font.e,grid.e,mainmenu.e,mark.e,menu.e,msg.e,-
		names.e,navigate.e,obj.e,page.e,pattern.e,raster.e,rect.e,-
		ruler.e,scroll.e,select.e,setup.e,stk.e,text.e
stretch.obj depends_on stretch.c,const.h,tgif_dbg.h,types.h,align.e,arc.e,-
		auxtext.e,choice.e,cmd.e,color.e,cursor.e,dialog.e,drawing.e,-
		dup.e,exec.e,font.e,grid.e,mainloop.e,mark.e,move.e,msg.e,-
		obj.e,poly.e,raster.e,rect.e,ruler.e,select.e,setup.e,spline.e,-
		stretch.e,text.e,xbitmap.e,xpixmap.e
tcp.obj depends_on tcp.c,const.h,tgif_dbg.h,remote.e,tcp.e,util.e
testdrive.obj depends_on testdrive.c,const.h,tgif_dbg.h,types.h,mainloop.e,-
		msg.e,obj.e,setup.e
text.obj depends_on text.c,const.h,tgif_dbg.h,types.h,attr.e,auxtext.e,-
		choice.e,cmd.e,color.e,cutpaste.e,cursor.e,dialog.e,drawing.e,-
		dup.e,file.e,font.e,grid.e,mainloop.e,mark.e,msg.e,names.e,-
		obj.e,pattern.e,poly.e,prtgif.e,raster.e,rect.e,ruler.e,-
		scroll.e,select.e,setup.e,stretch.e,text.e,util.e,xpixmap.e
tgif.obj depends_on tgif.c,const.h,tgif_dbg.h,types.h,color.e,file.e,-
		mainloop.e,msg.e,obj.e,page.e,setup.e,util.e
tgif_dbg.obj depends_on tgif_dbg.c
util.obj depends_on util.c,const.h,tgif_dbg.h,remote.e,util.e
version.obj depends_on version.c,version.e
vms_comp.obj depends_on vms_comp.c,vms_comp.h
wb1.obj depends_on wb1.c
wb2.obj depends_on wb2.c
wb3.obj depends_on wb3.c
xbitmap.obj depends_on xbitmap.c,const.h,tgif_dbg.h,types.h,attr.e,auxtext.e,-
		choice.e,cmd.e,color.e,cursor.e,dialog.e,drawing.e,dup.e,eps.e,-
		file.e,font.e,grid.e,imgproc.e,mark.e,menu.e,msg.e,names.e,-
		obj.e,page.e,pattern.e,ps.e,raster.e,rect.e,select.e,setup.e,-
		util.e,xbitmap.e,xpixmap.e
xpixmap.obj depends_on xpixmap.c,const.h,tgif_dbg.h,types.h,attr.e,choice.e,-
		cmd.e,color.e,cursor.e,dialog.e,drawing.e,dup.e,file.e,font.e,-
		grid.e,imgproc.e,mainmenu.e,mark.e,msg.e,names.e,obj.e,-
		pattern.e,ps.e,raster.e,rect.e,select.e,setup.e,util.e,-
		xbitmap.e,xpixmap.e
