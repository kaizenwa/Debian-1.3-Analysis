# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=vappgen - Win32 Release
!MESSAGE No configuration specified.  Defaulting to vappgen - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "vappgen - Win32 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "vappgen.mak" CFG="vappgen - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "vappgen - Win32 Release" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "vappgen - Win32 Release"
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

ALL : "$(OUTDIR)\vappgen.exe"

CLEAN : 
	-@erase ".\Release\vappgen.exe"
	-@erase ".\Release\Vgcmdw.obj"
	-@erase ".\Release\Vgdlmdlg.obj"
	-@erase ".\Release\Vgmdlg.obj"
	-@erase ".\Release\Vgapp.obj"
	-@erase ".\Release\Vgcode.obj"
	-@erase ".\Release\Vgtnmdlg.obj"
	-@erase ".\Release\Vgcnv.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /I "c:\v\includew" /I "c:\v\appgen" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/vappgen.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/vappgen.pdb" /machine:I386 /out:"$(OUTDIR)/vappgen.exe" 
LINK32_OBJS= \
	".\Release\Vgcmdw.obj" \
	".\Release\Vgdlmdlg.obj" \
	".\Release\Vgmdlg.obj" \
	".\Release\Vgapp.obj" \
	".\Release\Vgcode.obj" \
	".\Release\Vgtnmdlg.obj" \
	".\Release\Vgcnv.obj" \
	"..\vmsvc32\Release\vmsvc32.lib"

"$(OUTDIR)\vappgen.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

CPP_PROJ=/nologo /ML /W3 /GX /I "c:\v\includew" /I "c:\v\appgen" /D "WIN32" /D\
 "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)/vappgen.pch" /YX /Fo"$(INTDIR)/" /c 
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

# Name "vappgen - Win32 Release"
################################################################################
# Begin Source File

SOURCE=\V\Appgen\Vgtnmdlg.cpp
DEP_CPP_VGTNM=\
	".\..\..\Appgen\vgtnmdlg.h"\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vfilesel.h"\
	".\..\..\Appgen\vgdefs.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vgtnmdlg.obj" : $(SOURCE) $(DEP_CPP_VGTNM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Appgen\Vgcmdw.cpp
DEP_CPP_VGCMD=\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vkeys.h"\
	"c:\v\includew\v/vynreply.h"\
	".\..\..\Appgen\vgcmdw.h"\
	".\..\..\Appgen\vgcode.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vstatusp.h"\
	"c:\v\includew\v/vdebug.h"\
	".\..\..\Appgen\vgdefs.h"\
	".\..\..\Appgen\vgcnv.h"\
	".\..\..\Appgen\vgmdlg.h"\
	".\..\..\Appgen\vgdlmdlg.h"\
	".\..\..\Appgen\vgtnmdlg.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vtextcnv.h"\
	

"$(INTDIR)\Vgcmdw.obj" : $(SOURCE) $(DEP_CPP_VGCMD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Appgen\Vgcnv.cpp
DEP_CPP_VGCNV=\
	".\..\..\Appgen\vgcnv.h"\
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
	

"$(INTDIR)\Vgcnv.obj" : $(SOURCE) $(DEP_CPP_VGCNV) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Appgen\Vgcode.cpp
DEP_CPP_VGCOD=\
	".\..\..\Appgen\vgcode.h"\
	"c:\v\includew\v/vutil.h"\
	".\..\..\Appgen\vgdefs.h"\
	

"$(INTDIR)\Vgcode.obj" : $(SOURCE) $(DEP_CPP_VGCOD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Appgen\Vgdlmdlg.cpp
DEP_CPP_VGDLM=\
	".\..\..\Appgen\vgdlmdlg.h"\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vfilesel.h"\
	".\..\..\Appgen\vgdefs.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vgdlmdlg.obj" : $(SOURCE) $(DEP_CPP_VGDLM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Appgen\Vgmdlg.cpp
DEP_CPP_VGMDL=\
	".\..\..\Appgen\vgmdlg.h"\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vfilesel.h"\
	"c:\v\includew\v/vmodald.h"\
	".\..\..\Appgen\vgdefs.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vgmdlg.obj" : $(SOURCE) $(DEP_CPP_VGMDL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Appgen\Vgapp.cpp
DEP_CPP_VGAPP=\
	".\..\..\Appgen\vgapp.h"\
	"c:\v\includew\v/vdebug.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vawinfo.h"\
	".\..\..\Appgen\vgdefs.h"\
	".\..\..\Appgen\vgcmdw.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vstatusp.h"\
	".\..\..\Appgen\vgcnv.h"\
	".\..\..\Appgen\vgmdlg.h"\
	".\..\..\Appgen\vgdlmdlg.h"\
	".\..\..\Appgen\vgtnmdlg.h"\
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
	"c:\v\includew\v/vtextcnv.h"\
	

"$(INTDIR)\Vgapp.obj" : $(SOURCE) $(DEP_CPP_VGAPP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\msvc\vmsvc32\Release\vmsvc32.lib
# End Source File
# End Target
# End Project
################################################################################
