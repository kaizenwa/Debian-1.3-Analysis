# Microsoft Visual C++ generated build script - Do not modify

PROJ = VTMSVC16
DEBUG = 0
PROGTYPE = 0
CALLER = 
ARGS = 
DLLS = 
D_RCDEFINES = /d_DEBUG 
R_RCDEFINES = /dNDEBUG 
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
FIRSTCPP = TESTAPP.CPP 
RC = rc
CFLAGS_D_WEXE = /nologo /Zp1 /W3 /vmg /Zi /AL /Od /D "_DEBUG" /GA /Fd"VTMSVC16.PDB"
CFLAGS_R_WEXE = /nologo /Zp1 /W3 /vmg /AL /O1 /D "NDEBUG" /GA 
LFLAGS_D_WEXE = /NOLOGO /NOD /PACKC:61440 /STACK:16384 /ALIGN:16 /ONERROR:NOEXE /CO  
LFLAGS_R_WEXE = /NOLOGO /NOD /PACKC:61440 /STACK:16384 /ALIGN:16 /ONERROR:NOEXE  
LIBS_D_WEXE = oldnames libw llibcew commdlg.lib olecli.lib olesvr.lib shell.lib 
LIBS_R_WEXE = oldnames libw llibcew commdlg.lib olecli.lib olesvr.lib shell.lib 
RCFLAGS = /nologo 
RESFLAGS = /nologo 
RUNFLAGS = 
DEFFILE = VTEST.DEF
OBJS_EXT = 
LIBS_EXT = VMSVC16.LIB 
!if "$(DEBUG)" == "1"
CFLAGS = $(CFLAGS_D_WEXE)
LFLAGS = $(LFLAGS_D_WEXE)
LIBS = $(LIBS_D_WEXE)
MAPFILE = nul
RCDEFINES = $(D_RCDEFINES)
!else
CFLAGS = $(CFLAGS_R_WEXE)
LFLAGS = $(LFLAGS_R_WEXE)
LIBS = $(LIBS_R_WEXE)
MAPFILE = nul
RCDEFINES = $(R_RCDEFINES)
!endif
!if [if exist MSVC.BND del MSVC.BND]
!endif
SBRS = TESTAPP.SBR \
		VTCANVAS.SBR \
		VTCMDWIN.SBR \
		VTCW2.SBR \
		VTDIALOG.SBR \
		VTTOGDLG.SBR


VMSVC16_DEP = 

TESTAPP_DEP = c:\v\includew\v/vnotice.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h \
	c:\v\test\testapp.h \
	c:\v\includew\v/vdebug.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\test\vtcmdwin.h \
	c:\v\includew\v/vcmdwin.h \
	c:\v\includew\v/vwindow.h \
	c:\v\includew\v/vcanvas.h \
	c:\v\includew\v/vcpdc.h \
	c:\v\includew\v/vwindc.h \
	c:\v\includew\v/vdc.h \
	c:\v\includew\v/vpen.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vbrush.h \
	c:\v\includew\v/vprinter.h \
	c:\v\includew\v/vwinprtr.h \
	c:\v\includew\v/vynreply.h \
	c:\v\includew\v/vstatusp.h \
	c:\v\includew\v/vcmdpane.h \
	c:\v\includew\v/vreply.h \
	c:\v\includew\v/vfilesel.h \
	c:\v\includew\v/vprintdc.h \
	c:\v\includew\v/vwinprdc.h \
	c:\v\includew\v/vmemdc.h \
	c:\v\test\vtcanvas.h \
	c:\v\includew\v/vtextcnv.h \
	c:\v\test\vtdialog.h \
	c:\v\test\vttogdlg.h


VTCANVAS_DEP = c:\v\includew\v/vutil.h \
	c:\v\test\vtcanvas.h \
	c:\v\includew\v/vtextcnv.h \
	c:\v\includew\v/vcanvas.h \
	c:\v\test\vtcmdwin.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vcmdwin.h \
	c:\v\includew\v/vwindow.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vbasewin.h \
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
	c:\v\includew\v/vynreply.h \
	c:\v\includew\v/vdebug.h \
	c:\v\includew\v/vnotice.h \
	c:\v\includew\v/vstatusp.h \
	c:\v\includew\v/vcmdpane.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vreply.h \
	c:\v\includew\v/vfilesel.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vprintdc.h \
	c:\v\includew\v/vwinprdc.h \
	c:\v\includew\v/vmemdc.h \
	c:\v\test\vtdialog.h \
	c:\v\includew\v/vdialog.h \
	c:\v\test\vttogdlg.h


VTCMDWIN_DEP = c:\v\includew\v/vcanvas.h \
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
	c:\v\includew\v/vicon.h \
	c:\v\includew\v/vutil.h \
	c:\v\includew\v/vfontsel.h \
	c:\v\includew\v/vmemdc.h \
	c:\v\includew\v/vkeys.h \
	c:\v\test\vtcmdwin.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vcmdwin.h \
	c:\v\includew\v/vwindow.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vynreply.h \
	c:\v\includew\v/vdebug.h \
	c:\v\includew\v/vnotice.h \
	c:\v\includew\v/vstatusp.h \
	c:\v\includew\v/vcmdpane.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vreply.h \
	c:\v\includew\v/vfilesel.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vprintdc.h \
	c:\v\includew\v/vwinprdc.h \
	c:\v\test\vtcanvas.h \
	c:\v\includew\v/vtextcnv.h \
	c:\v\test\vtdialog.h \
	c:\v\includew\v/vdialog.h \
	c:\v\test\vttogdlg.h \
	c:\v\test\vtcw2.h \
	c:\v\test\bruce.vbm \
	c:\v\includew\v/vcb2x4.h


VTCW2_DEP = c:\v\includew\v/vcanvas.h \
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
	c:\v\test\vtcw2.h \
	c:\v\includew\v/vstatusp.h \
	c:\v\includew\v/vcmdpane.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmdwin.h \
	c:\v\includew\v/vwindow.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vapp.h


VTDIALOG_DEP = c:\v\test\vtdialog.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vnotice.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h \
	c:\v\includew\v/vcolor.h \
	c:\v\includew\v/vlabelc.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vutil.h


VTTOGDLG_DEP = c:\v\test\vttogdlg.h \
	c:\v\includew\v/vmodald.h \
	c:\v\includew\v/v_defs.h \
	c:\v\includew\v/vapp.h \
	c:\v\includew\v/vbaseitm.h \
	c:\v\includew\v/vfont.h \
	c:\v\includew\v/vawinfo.h \
	c:\v\includew\v/vmenu.h \
	c:\v\includew\v/vpane.h \
	c:\v\includew\v/vtimer.h \
	c:\v\includew\v/vnotice.h \
	c:\v\includew\v/vdialog.h \
	c:\v\includew\v/vbasewin.h \
	c:\v\includew\v/vcmdprnt.h \
	c:\v\includew\v/vcmd.h


all:	$(PROJ).EXE

TESTAPP.OBJ:	..\TEST\TESTAPP.CPP $(TESTAPP_DEP)
	$(CPP) $(CFLAGS) $(CPPCREATEPCHFLAG) /c ..\TEST\TESTAPP.CPP

VTCANVAS.OBJ:	..\TEST\VTCANVAS.CPP $(VTCANVAS_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\TEST\VTCANVAS.CPP

VTCMDWIN.OBJ:	..\TEST\VTCMDWIN.CPP $(VTCMDWIN_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\TEST\VTCMDWIN.CPP

VTCW2.OBJ:	..\TEST\VTCW2.CPP $(VTCW2_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\TEST\VTCW2.CPP

VTDIALOG.OBJ:	..\TEST\VTDIALOG.CPP $(VTDIALOG_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\TEST\VTDIALOG.CPP

VTTOGDLG.OBJ:	..\TEST\VTTOGDLG.CPP $(VTTOGDLG_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ..\TEST\VTTOGDLG.CPP


$(PROJ).EXE::	TESTAPP.OBJ VTCANVAS.OBJ VTCMDWIN.OBJ VTCW2.OBJ VTDIALOG.OBJ VTTOGDLG.OBJ $(OBJS_EXT) $(DEFFILE)
	echo >NUL @<<$(PROJ).CRF
TESTAPP.OBJ +
VTCANVAS.OBJ +
VTCMDWIN.OBJ +
VTCW2.OBJ +
VTDIALOG.OBJ +
VTTOGDLG.OBJ +
$(OBJS_EXT)
$(PROJ).EXE
$(MAPFILE)
c:\msvc\lib\+
VMSVC16.LIB+
$(LIBS)
$(DEFFILE);
<<
	link $(LFLAGS) @$(PROJ).CRF
	$(RC) $(RESFLAGS) $@


run: $(PROJ).EXE
	$(PROJ) $(RUNFLAGS)


$(PROJ).BSC: $(SBRS)
	bscmake @<<
/o$@ $(SBRS)
<<
