# Microsoft Visual C++ generated build script - Do not modify

PROJ = VMSVC16
DEBUG = 0
PROGTYPE = 4
CALLER = 
ARGS = 
DLLS = 
D_RCDEFINES = -d_DEBUG
R_RCDEFINES = -dNDEBUG
ORIGIN = MSVC
ORIGIN_VER = 1.00
PROJPATH = C:\V\MSVC\
USEMFC = 0
CC = cl
CPP = cl
CXX = cl
CCREATEPCHFLAG = 
CPPCREATEPCHFLAG = 
CUSEPCHFLAG = 
CPPUSEPCHFLAG = 
FIRSTC =             
FIRSTCPP = VAPP.CPP    
RC = rc
CFLAGS_D_LIB = /nologo /Zp1 /W3 /vmg /Zi /AL /Od /D "_DEBUG" /GA /Fd"VMSVC16.PDB"
CFLAGS_R_LIB = /nologo /Gs /Zp1 /W3 /vmg /AL /O1 /Ox /D "NDEBUG" /I "c:\v\includew" /GA 
RCFLAGS = /nologo
RESFLAGS = /nologo
RUNFLAGS = 
OBJS_EXT = 
LIBS_EXT = 
!if "$(DEBUG)" == "1"
CFLAGS = $(CFLAGS_D_LIB)
LFLAGS = 
LIBS = 
MAPFILE = nul
RCDEFINES = $(D_RCDEFINES)
!else
CFLAGS = $(CFLAGS_R_LIB)
LFLAGS = 
LIBS = 
MAPFILE = nul
RCDEFINES = $(R_RCDEFINES)
!endif
!if [if exist MSVC.BND del MSVC.BND]
!endif
SBRS = VAPP.SBR \
		VAWINFO.SBR \
		VBASEITM.SBR \
		VBASEWIN.SBR \
		VBOXLBLC.SBR \
		VBRUSH.SBR \
		VBTNCMD.SBR \
		VCANVAS.SBR \
		VCBTNCMD.SBR \
		VCHKBOXC.SBR \
		VCLABELC.SBR \
		VCMD.SBR \
		VCMDPANE.SBR \
		VCMDPRNT.SBR \
		VCMDWIN.SBR \
		VCOLOR.SBR \
		VCOMBOC.SBR \
		VCPDC.SBR \
		VDEBUG.SBR \
		VDIALOG.SBR \
		VFILESEL.SBR \
		VFINDDLG.SBR \
		VFONT.SBR \
		VFONTSEL.SBR \
		VFRAMEC.SBR \
		VICON.SBR \
		VLABELC.SBR \
		VLISTC.SBR \
		VMEMDC.SBR \
		VMENU.SBR \
		VMODALD.SBR \
		VNOTICE.SBR \
		VPEN.SBR \
		VPROGRSC.SBR \
		VRADIOC.SBR \
		VREPLY.SBR \
		VSLIDERC.SBR \
		VSPINC.SBR \
		VTEXTC.SBR \
		VTEXTCNV.SBR \
		VTEXTED.SBR \
		VTEXTINC.SBR \
		VTHISLST.SBR \
		VTIMER.SBR \
		VUTIL.SBR \
		VWINDC.SBR \
		VWINDOW.SBR \
		VWINPRDC.SBR \
		VWINPRTR.SBR \
		VYNREPLY.SBR


VAPP_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vwindow.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcanvas.h \
	c:\v\includew\v/vcpdc.h \
	c:\v\includew\v/vwindc.h \
	c:\v\includew\v/vdc.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vprinter.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vcmdwin.h \
	c:\v\includew\v/vcmdpane.h \
	c:\v\includew\v/vthislst.h


VAWINFO_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/v_defs.h


VBASEITM_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h


VBASEWIN_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vbaseitm.h


VBOXLBLC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vboxlblc.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vcmdprnt.h


VBRUSH_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vcolor.h


VBTNCMD_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vbtncmd.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vutil.h \
	c:\v\includew\v/vicon.h


VCANVAS_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vcanvas.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vcpdc.h \
	c:\v\includew\v/vwindc.h \
	c:\v\includew\v/vdc.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vprinter.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vwindow.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vapp.h


VCBTNCMD_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vcbtncmd.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vbtncmd.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vcmdprnt.h


VCHKBOXC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vchkboxc.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vutil.h


VCLABELC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vclabelc.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vlabelc.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vcmdprnt.h


VCMD_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vutil.h


VCMDPANE_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vcmdpane.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vwindow.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vcanvas.h \
	c:\v\includew\v/vcpdc.h \
	c:\v\includew\v/vwindc.h \
	c:\v\includew\v/vdc.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vprinter.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vthislst.h


VCMDPRNT_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vbtncmd.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vcbtncmd.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vclabelc.h \
	c:\v\includew\v/vlabelc.h \
	c:\v\includew\v/vframec.h \
	c:\v\includew\v/vchkboxc.h \
	c:\v\includew\v/vradioc.h \
	c:\v\includew\v/vtextinc.h \
	c:\v\includew\v/vtextc.h \
	c:\v\includew\v/vlistc.h \
	c:\v\includew\v/vcomboc.h \
	c:\v\includew\v/vspinc.h \
	c:\v\includew\v/vsliderc.h \
	c:\v\includew\v/vprogrsc.h \
	c:\v\includew\v/vboxlblc.h


VCMDWIN_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vcmdwin.h \
	c:\v\includew\v/vwindow.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/v_defs.h


VCOLOR_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vcolor.h


VCOMBOC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vcomboc.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vutil.h


VCPDC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vcpdc.h \
	c:\v\includew\v/vwindc.h \
	c:\v\includew\v/vdc.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vprinter.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vcanvas.h


VDEBUG_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vdebug.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmd.h


VDIALOG_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vthislst.h \
	c:\v\includew\v/vclabelc.h \
	c:\v\includew\v/vlabelc.h \
	c:\v\includew\v/vcolor.h


VFILESEL_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vfilesel.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vbasewin.h


VFINDDLG_DEP = c:\v\includew\v/vfinddlg.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vicon.h


VFONT_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h


VFONTSEL_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vfontsel.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vbasewin.h


VFRAMEC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vframec.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vcmdprnt.h


VICON_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vicon.h


VLABELC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vlabelc.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vutil.h \
	c:\v\includew\v/vicon.h


VLISTC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vlistc.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h


VMEMDC_DEP = c:\v\includew\v/vmemdc.h \
	c:\v\includew\v/vwindc.h \
	c:\v\includew\v/vdc.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vprinter.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h


VMENU_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vcmdwin.h \
	c:\v\includew\v/vwindow.h


VMODALD_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h


VNOTICE_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vnotice.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vicon.h


VPEN_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h


VPROGRSC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vprogrsc.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vutil.h


VRADIOC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vradioc.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vutil.h


VREPLY_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vreply.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vicon.h


VSLIDERC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vsliderc.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h


VSPINC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vspinc.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vutil.h


VTEXTC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vtextc.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vutil.h


VTEXTCNV_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vtextcnv.h \
	c:\v\includew\v/vcanvas.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vcpdc.h \
	c:\v\includew\v/vwindc.h \
	c:\v\includew\v/vdc.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vprinter.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h


VTEXTED_DEP = c:\v\includew\v/vkeys.h \
	c:\v\includew\v/vtexted.h \
	c:\v\includew\v/vtextcnv.h \
	c:\v\includew\v/vcanvas.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vcpdc.h \
	c:\v\includew\v/vwindc.h \
	c:\v\includew\v/vdc.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vprinter.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vfinddlg.h


VTEXTINC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vtextinc.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vcmdprnt.h


VTHISLST_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vthislst.h


VTIMER_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vthislst.h


VUTIL_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vutil.h


VWINDC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vwindc.h \
	c:\v\includew\v/vdc.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vprinter.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vmemdc.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vcanvas.h \
	c:\v\includew\v/vcpdc.h \
	c:\v\includew\v/vicon.h


VWINDOW_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vwindow.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcanvas.h \
	c:\v\includew\v/vcpdc.h \
	c:\v\includew\v/vwindc.h \
	c:\v\includew\v/vdc.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vprinter.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vkeys.h \
	c:\v\includew\v/vnotice.h


VWINPRDC_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vwinprdc.h \
	c:\v\includew\v/vwindc.h \
	c:\v\includew\v/vdc.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vprinter.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h


VWINPRTR_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h


VYNREPLY_DEP = c:\v\includew\v/vwin32.h \
	c:\v\includew\v/vynreply.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vicon.h


all:	$(PROJ).LIB

VAPP.OBJ:	..\SRCWIN\VAPP.CPP $(VAPP_DEP)
	$(CPP) $(CFLAGS) $(CPPCREATEPCHFLAG) /c ..\SRCWIN\VAPP.CPP

VAWINFO.OBJ:	..\SRCWIN\VAWINFO.CPP $(VAWINFO_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VAWINFO.CPP

VBASEITM.OBJ:	..\SRCWIN\VBASEITM.CPP $(VBASEITM_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VBASEITM.CPP

VBASEWIN.OBJ:	..\SRCWIN\VBASEWIN.CPP $(VBASEWIN_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VBASEWIN.CPP

VBOXLBLC.OBJ:	..\SRCWIN\VBOXLBLC.CPP $(VBOXLBLC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VBOXLBLC.CPP

VBRUSH.OBJ:	..\SRCWIN\VBRUSH.CPP $(VBRUSH_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VBRUSH.CPP

VBTNCMD.OBJ:	..\SRCWIN\VBTNCMD.CPP $(VBTNCMD_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VBTNCMD.CPP

VCANVAS.OBJ:	..\SRCWIN\VCANVAS.CPP $(VCANVAS_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VCANVAS.CPP

VCBTNCMD.OBJ:	..\SRCWIN\VCBTNCMD.CPP $(VCBTNCMD_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VCBTNCMD.CPP

VCHKBOXC.OBJ:	..\SRCWIN\VCHKBOXC.CPP $(VCHKBOXC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VCHKBOXC.CPP

VCLABELC.OBJ:	..\SRCWIN\VCLABELC.CPP $(VCLABELC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VCLABELC.CPP

VCMD.OBJ:	..\SRCWIN\VCMD.CPP $(VCMD_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VCMD.CPP

VCMDPANE.OBJ:	..\SRCWIN\VCMDPANE.CPP $(VCMDPANE_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VCMDPANE.CPP

VCMDPRNT.OBJ:	..\SRCWIN\VCMDPRNT.CPP $(VCMDPRNT_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VCMDPRNT.CPP

VCMDWIN.OBJ:	..\SRCWIN\VCMDWIN.CPP $(VCMDWIN_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VCMDWIN.CPP

VCOLOR.OBJ:	..\SRCWIN\VCOLOR.CPP $(VCOLOR_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VCOLOR.CPP

VCOMBOC.OBJ:	..\SRCWIN\VCOMBOC.CPP $(VCOMBOC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VCOMBOC.CPP

VCPDC.OBJ:	..\SRCWIN\VCPDC.CPP $(VCPDC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VCPDC.CPP

VDEBUG.OBJ:	..\SRCWIN\VDEBUG.CPP $(VDEBUG_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VDEBUG.CPP

VDIALOG.OBJ:	..\SRCWIN\VDIALOG.CPP $(VDIALOG_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VDIALOG.CPP

VFILESEL.OBJ:	..\SRCWIN\VFILESEL.CPP $(VFILESEL_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VFILESEL.CPP

VFINDDLG.OBJ:	..\SRCWIN\VFINDDLG.CPP $(VFINDDLG_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VFINDDLG.CPP

VFONT.OBJ:	..\SRCWIN\VFONT.CPP $(VFONT_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VFONT.CPP

VFONTSEL.OBJ:	..\SRCWIN\VFONTSEL.CPP $(VFONTSEL_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VFONTSEL.CPP

VFRAMEC.OBJ:	..\SRCWIN\VFRAMEC.CPP $(VFRAMEC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VFRAMEC.CPP

VICON.OBJ:	..\SRCWIN\VICON.CPP $(VICON_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VICON.CPP

VLABELC.OBJ:	..\SRCWIN\VLABELC.CPP $(VLABELC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VLABELC.CPP

VLISTC.OBJ:	..\SRCWIN\VLISTC.CPP $(VLISTC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VLISTC.CPP

VMEMDC.OBJ:	..\SRCWIN\VMEMDC.CPP $(VMEMDC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VMEMDC.CPP

VMENU.OBJ:	..\SRCWIN\VMENU.CPP $(VMENU_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VMENU.CPP

VMODALD.OBJ:	..\SRCWIN\VMODALD.CPP $(VMODALD_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VMODALD.CPP

VNOTICE.OBJ:	..\SRCWIN\VNOTICE.CPP $(VNOTICE_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VNOTICE.CPP

VPEN.OBJ:	..\SRCWIN\VPEN.CPP $(VPEN_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VPEN.CPP

VPROGRSC.OBJ:	..\SRCWIN\VPROGRSC.CPP $(VPROGRSC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VPROGRSC.CPP

VRADIOC.OBJ:	..\SRCWIN\VRADIOC.CPP $(VRADIOC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VRADIOC.CPP

VREPLY.OBJ:	..\SRCWIN\VREPLY.CPP $(VREPLY_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VREPLY.CPP

VSLIDERC.OBJ:	..\SRCWIN\VSLIDERC.CPP $(VSLIDERC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VSLIDERC.CPP

VSPINC.OBJ:	..\SRCWIN\VSPINC.CPP $(VSPINC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VSPINC.CPP

VTEXTC.OBJ:	..\SRCWIN\VTEXTC.CPP $(VTEXTC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VTEXTC.CPP

VTEXTCNV.OBJ:	..\SRCWIN\VTEXTCNV.CPP $(VTEXTCNV_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VTEXTCNV.CPP

VTEXTED.OBJ:	..\SRCWIN\VTEXTED.CPP $(VTEXTED_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VTEXTED.CPP

VTEXTINC.OBJ:	..\SRCWIN\VTEXTINC.CPP $(VTEXTINC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VTEXTINC.CPP

VTHISLST.OBJ:	..\SRCWIN\VTHISLST.CPP $(VTHISLST_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VTHISLST.CPP

VTIMER.OBJ:	..\SRCWIN\VTIMER.CPP $(VTIMER_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VTIMER.CPP

VUTIL.OBJ:	..\SRCWIN\VUTIL.CPP $(VUTIL_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VUTIL.CPP

VWINDC.OBJ:	..\SRCWIN\VWINDC.CPP $(VWINDC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VWINDC.CPP

VWINDOW.OBJ:	..\SRCWIN\VWINDOW.CPP $(VWINDOW_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VWINDOW.CPP

VWINPRDC.OBJ:	..\SRCWIN\VWINPRDC.CPP $(VWINPRDC_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VWINPRDC.CPP

VWINPRTR.OBJ:	..\SRCWIN\VWINPRTR.CPP $(VWINPRTR_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VWINPRTR.CPP

VYNREPLY.OBJ:	..\SRCWIN\VYNREPLY.CPP $(VYNREPLY_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\SRCWIN\VYNREPLY.CPP

$(PROJ).LIB::	VAPP.OBJ VAWINFO.OBJ VBASEITM.OBJ VBASEWIN.OBJ VBOXLBLC.OBJ VBRUSH.OBJ \
	VBTNCMD.OBJ VCANVAS.OBJ VCBTNCMD.OBJ VCHKBOXC.OBJ VCLABELC.OBJ VCMD.OBJ VCMDPANE.OBJ \
	VCMDPRNT.OBJ VCMDWIN.OBJ VCOLOR.OBJ VCOMBOC.OBJ VCPDC.OBJ VDEBUG.OBJ VDIALOG.OBJ \
	VFILESEL.OBJ VFINDDLG.OBJ VFONT.OBJ VFONTSEL.OBJ VFRAMEC.OBJ VICON.OBJ VLABELC.OBJ \
	VLISTC.OBJ VMEMDC.OBJ VMENU.OBJ VMODALD.OBJ VNOTICE.OBJ VPEN.OBJ VPROGRSC.OBJ VRADIOC.OBJ \
	VREPLY.OBJ VSLIDERC.OBJ VSPINC.OBJ VTEXTC.OBJ VTEXTCNV.OBJ VTEXTED.OBJ VTEXTINC.OBJ \
	VTHISLST.OBJ VTIMER.OBJ VUTIL.OBJ VWINDC.OBJ VWINDOW.OBJ VWINPRDC.OBJ VWINPRTR.OBJ \
	VYNREPLY.OBJ $(OBJS_EXT)
	echo >NUL @<<$(PROJ).CRF
$@ /PAGESIZE:64
y
+VAPP.OBJ &
+VAWINFO.OBJ &
+VBASEITM.OBJ &
+VBASEWIN.OBJ &
+VBOXLBLC.OBJ &
+VBRUSH.OBJ &
+VBTNCMD.OBJ &
+VCANVAS.OBJ &
+VCBTNCMD.OBJ &
+VCHKBOXC.OBJ &
+VCLABELC.OBJ &
+VCMD.OBJ &
+VCMDPANE.OBJ &
+VCMDPRNT.OBJ &
+VCMDWIN.OBJ &
+VCOLOR.OBJ &
+VCOMBOC.OBJ &
+VCPDC.OBJ &
+VDEBUG.OBJ &
+VDIALOG.OBJ &
+VFILESEL.OBJ &
+VFINDDLG.OBJ &
+VFONT.OBJ &
+VFONTSEL.OBJ &
+VFRAMEC.OBJ &
+VICON.OBJ &
+VLABELC.OBJ &
+VLISTC.OBJ &
+VMEMDC.OBJ &
+VMENU.OBJ &
+VMODALD.OBJ &
+VNOTICE.OBJ &
+VPEN.OBJ &
+VPROGRSC.OBJ &
+VRADIOC.OBJ &
+VREPLY.OBJ &
+VSLIDERC.OBJ &
+VSPINC.OBJ &
+VTEXTC.OBJ &
+VTEXTCNV.OBJ &
+VTEXTED.OBJ &
+VTEXTINC.OBJ &
+VTHISLST.OBJ &
+VTIMER.OBJ &
+VUTIL.OBJ &
+VWINDC.OBJ &
+VWINDOW.OBJ &
+VWINPRDC.OBJ &
+VWINPRTR.OBJ &
+VYNREPLY.OBJ &
;
<<
	if exist $@ del $@
	lib @$(PROJ).CRF

$(PROJ).BSC: $(SBRS)
	bscmake @<<
/o$@ $(SBRS)
<<
