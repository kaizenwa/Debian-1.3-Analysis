# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=viconed - Win32 Release
!MESSAGE No configuration specified.  Defaulting to viconed - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "viconed - Win32 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "viconed.mak" CFG="viconed - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "viconed - Win32 Release" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "viconed - Win32 Release"
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

ALL : "$(OUTDIR)\viconed.exe"

CLEAN : 
	-@erase ".\Release\viconed.exe"
	-@erase ".\Release\Viedapp.obj"
	-@erase ".\Release\Brshdlg.obj"
	-@erase ".\Release\Viedcnv.obj"
	-@erase ".\Release\Imageio.obj"
	-@erase ".\Release\Viedcmdw.obj"
	-@erase ".\Release\Coldlg.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /I "c:\v\includew" /I "c:\v\iconed" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/viconed.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/viconed.pdb" /machine:I386 /out:"$(OUTDIR)/viconed.exe" 
LINK32_OBJS= \
	".\Release\Viedapp.obj" \
	".\Release\Brshdlg.obj" \
	".\Release\Viedcnv.obj" \
	".\Release\Imageio.obj" \
	".\Release\Viedcmdw.obj" \
	".\Release\Coldlg.obj" \
	"..\vmsvc32\Release\vmsvc32.lib"

"$(OUTDIR)\viconed.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

CPP_PROJ=/nologo /ML /W3 /GX /I "c:\v\includew" /I "c:\v\iconed" /D "WIN32" /D\
 "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)/viconed.pch" /YX /Fo"$(INTDIR)/" /c 
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

# Name "viconed - Win32 Release"
################################################################################
# Begin Source File

SOURCE=\V\Iconed\Viedcnv.cpp
DEP_CPP_VIEDC=\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vnotice.h"\
	".\..\..\Iconed\viedcmdw.h"\
	".\..\..\Iconed\viedcnv.h"\
	".\..\..\Iconed\coldlg.h"\
	".\..\..\Iconed\imageio.h"\
	".\..\..\Iconed\paldecla.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vstatusp.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vcolor.h"\
	

"$(INTDIR)\Viedcnv.obj" : $(SOURCE) $(DEP_CPP_VIEDC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Iconed\Coldlg.cpp
DEP_CPP_COLDL=\
	".\..\..\Iconed\coldlg.h"\
	".\..\..\Iconed\viedcmdw.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vicon.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vreply.h"\
	".\..\..\Iconed\palclrs.h"\
	"c:\v\includew\v/vdialog.h"\
	".\..\..\Iconed\viedcnv.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vstatusp.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vtimer.h"\
	

"$(INTDIR)\Coldlg.obj" : $(SOURCE) $(DEP_CPP_COLDL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Iconed\Imageio.cpp
DEP_CPP_IMAGE=\
	".\..\..\Iconed\imageio.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Imageio.obj" : $(SOURCE) $(DEP_CPP_IMAGE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Iconed\Viedapp.cpp
DEP_CPP_VIEDA=\
	".\..\..\Iconed\viedapp.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vawinfo.h"\
	".\..\..\Iconed\viedcmdw.h"\
	".\..\..\Iconed\coldlg.h"\
	".\..\..\Iconed\brshdlg.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vstatusp.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vcolor.h"\
	".\..\..\Iconed\viedcnv.h"\
	

"$(INTDIR)\Viedapp.obj" : $(SOURCE) $(DEP_CPP_VIEDA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Iconed\Viedcmdw.cpp
DEP_CPP_VIEDCM=\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vfilesel.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vynreply.h"\
	"c:\v\includew\v/vreply.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vicon.h"\
	"c:\v\includew\v/vmodald.h"\
	".\..\..\Iconed\viedapp.h"\
	".\..\..\Iconed\viedcmdw.h"\
	".\..\..\Iconed\viedcnv.h"\
	".\..\..\Iconed\coldlg.h"\
	".\..\..\Iconed\brshdlg.h"\
	".\..\..\Iconed\palett.vbm"\
	".\..\..\Iconed\brushes.vbm"\
	".\..\..\Iconed\snap.vbm"\
	".\..\..\Iconed\clear.vbm"\
	".\..\..\Iconed\undo.vbm"\
	".\..\..\Iconed\drwbnorm.vbm"\
	".\..\..\Iconed\drwbsel.vbm"\
	".\..\..\Iconed\drwbabc.vbm"\
	".\..\..\Iconed\rect.vbm"\
	".\..\..\Iconed\rdrect.vbm"\
	".\..\..\Iconed\ellipse.vbm"\
	".\..\..\Iconed\line.vbm"\
	".\..\..\Iconed\point.vbm"\
	".\..\..\Iconed\fill.vbm"\
	".\..\..\Iconed\dropper.vbm"\
	".\..\..\Iconed\about.vbm"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vstatusp.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vwinprtr.h"\
	

"$(INTDIR)\Viedcmdw.obj" : $(SOURCE) $(DEP_CPP_VIEDCM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Iconed\Brshdlg.cpp
DEP_CPP_BRSHD=\
	".\..\..\Iconed\brshdlg.h"\
	".\..\..\Iconed\viedcmdw.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vicon.h"\
	".\..\..\Iconed\brush1.vbm"\
	".\..\..\Iconed\brush2.vbm"\
	".\..\..\Iconed\brush3.vbm"\
	".\..\..\Iconed\brush4.vbm"\
	".\..\..\Iconed\brush5.vbm"\
	".\..\..\Iconed\brush6.vbm"\
	".\..\..\Iconed\brush7.vbm"\
	".\..\..\Iconed\brush8.vbm"\
	".\..\..\Iconed\brush9.vbm"\
	".\..\..\Iconed\brush10.vbm"\
	"c:\v\includew\v/vdialog.h"\
	".\..\..\Iconed\viedcnv.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vstatusp.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vtimer.h"\
	

"$(INTDIR)\Brshdlg.obj" : $(SOURCE) $(DEP_CPP_BRSHD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\msvc\vmsvc32\Release\vmsvc32.lib
# End Source File
# End Target
# End Project
################################################################################
