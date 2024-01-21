# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=vtstms32 - Win32 Release
!MESSAGE No configuration specified.  Defaulting to vtstms32 - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "vtstms32 - Win32 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "vtstms32.mak" CFG="vtstms32 - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "vtstms32 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "vtstms32 - Win32 Release"
CPP=cl.exe
MTL=mktyplib.exe
RSC=rc.exe
# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\vtstms32.exe"

CLEAN : 
	-@erase ".\Release\vtstms32.exe"
	-@erase ".\Release\Vtcw2.obj"
	-@erase ".\Release\Testapp.obj"
	-@erase ".\Release\Vtcanvas.obj"
	-@erase ".\Release\Vtdialog.obj"
	-@erase ".\Release\Vttogdlg.obj"
	-@erase ".\Release\Vtcmdwin.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /I "c:\v\includew" /I "c:\v\test" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/vtstms32.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/vtstms32.pdb" /machine:I386 /out:"$(OUTDIR)/vtstms32.exe" 
LINK32_OBJS= \
	".\Release\Vtcw2.obj" \
	".\Release\Testapp.obj" \
	".\Release\Vtcanvas.obj" \
	".\Release\Vtdialog.obj" \
	".\Release\Vttogdlg.obj" \
	".\Release\Vtcmdwin.obj" \
	"..\vmsvc32\Release\vmsvc32.lib"

"$(OUTDIR)\vtstms32.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

CPP_PROJ=/nologo /ML /W3 /GX /I "c:\v\includew" /I "c:\v\test" /D "WIN32" /D\
 "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)/vtstms32.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

MTL_PROJ=/nologo /D "NDEBUG" /win32 
################################################################################
# Begin Target

# Name "vtstms32 - Win32 Release"
################################################################################
# Begin Source File

SOURCE=\V\Test\Vttogdlg.cpp
DEP_CPP_VTTOG=\
	".\..\..\Test\vttogdlg.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vttogdlg.obj" : $(SOURCE) $(DEP_CPP_VTTOG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Test\Vtcanvas.cpp
DEP_CPP_VTCAN=\
	"c:\v\includew\v/vutil.h"\
	".\..\..\Test\vtcanvas.h"\
	".\..\..\Test\vtcmdwin.h"\
	"c:\v\includew\v/vtextcnv.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vynreply.h"\
	"c:\v\includew\v/vdebug.h"\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vstatusp.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vreply.h"\
	"c:\v\includew\v/vfilesel.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vprintdc.h"\
	"c:\v\includew\v/vmemdc.h"\
	".\..\..\Test\vtdialog.h"\
	".\..\..\Test\vttogdlg.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vwinprdc.h"\
	

"$(INTDIR)\Vtcanvas.obj" : $(SOURCE) $(DEP_CPP_VTCAN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Test\Vtcmdwin.cpp
DEP_CPP_VTCMD=\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vicon.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vfontsel.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vmemdc.h"\
	"c:\v\includew\v/vkeys.h"\
	".\..\..\Test\vtcmdwin.h"\
	".\..\..\Test\vtcw2.h"\
	".\..\..\Test\bruce.vbm"\
	"c:\v\includew\v/vcb2x4.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vynreply.h"\
	"c:\v\includew\v/vdebug.h"\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vstatusp.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vreply.h"\
	"c:\v\includew\v/vfilesel.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vprintdc.h"\
	".\..\..\Test\vtcanvas.h"\
	".\..\..\Test\vtdialog.h"\
	".\..\..\Test\vttogdlg.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vwinprdc.h"\
	"c:\v\includew\v/vtextcnv.h"\
	

"$(INTDIR)\Vtcmdwin.obj" : $(SOURCE) $(DEP_CPP_VTCMD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Test\Vtcw2.cpp
DEP_CPP_VTCW2=\
	"c:\v\includew\v/vcanvas.h"\
	".\..\..\Test\vtcw2.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vstatusp.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	

"$(INTDIR)\Vtcw2.obj" : $(SOURCE) $(DEP_CPP_VTCW2) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Test\Vtdialog.cpp
DEP_CPP_VTDIA=\
	".\..\..\Test\vtdialog.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vlabelc.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vtdialog.obj" : $(SOURCE) $(DEP_CPP_VTDIA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Test\Testapp.cpp
DEP_CPP_TESTA=\
	"c:\v\includew\v/vnotice.h"\
	".\..\..\Test\testapp.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vdebug.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vawinfo.h"\
	".\..\..\Test\vtcmdwin.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vynreply.h"\
	"c:\v\includew\v/vstatusp.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vreply.h"\
	"c:\v\includew\v/vfilesel.h"\
	"c:\v\includew\v/vprintdc.h"\
	"c:\v\includew\v/vmemdc.h"\
	".\..\..\Test\vtcanvas.h"\
	".\..\..\Test\vtdialog.h"\
	".\..\..\Test\vttogdlg.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vwinprdc.h"\
	"c:\v\includew\v/vtextcnv.h"\
	

"$(INTDIR)\Testapp.obj" : $(SOURCE) $(DEP_CPP_TESTA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\msvc\vmsvc32\Release\vmsvc32.lib
# End Source File
# End Target
# End Project
################################################################################
