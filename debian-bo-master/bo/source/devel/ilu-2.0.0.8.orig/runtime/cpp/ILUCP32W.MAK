# Modified so that $(ILUHOME) can be used to specifiy location
# Note that if you use this as a VC++ make file, $(ILUHOME) will be \ilu
# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "ilucp32w.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
# PROP Target_Last_Scanned "Win32 Release"
MTL=MkTypLib.exe
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRelW"
# PROP Intermediate_Dir "WinRelW"
OUTDIR=.\WinRelW
INTDIR=.\WinRelW

ALL : $(ILUHOME)\bin\ilucp32w.dll $(OUTDIR)/ilucp32w.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Ox /Oy- /I "\ilu\src\runtime\kernel" /I "\ilu\src\winio" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_WINIO" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Ox /Oy- /I "$(ILUSRC)\runtime\kernel" /I\
 "$(ILUSRC)\winio" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_WINIO"\
 /Fp$(OUTDIR)/"ilucp32w.pch" /Fo$(INTDIR)/ /c /D "ILU_BUILDING_RUNTIME"
CPP_OBJS=.\WinRelW/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"ilucp32w.bsc" 

$(OUTDIR)/ilucp32w.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386
# ADD LINK32 \ilu\lib\ilu32w.lib \ilu\lib\winio32w.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386 /OUT:"\ilu\bin\ilucp32w.dll" /IMPLIB:"\ilu\lib\ilucp32w.lib"
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS=$(ILUHOME)\lib\ilu32w.lib $(ILUHOME)\lib\winio32w.lib wsock32.lib kernel32.lib\
 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib\
 ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO\
 /SUBSYSTEM:windows /DLL /INCREMENTAL:no /PDB:$(OUTDIR)/"ilucp32w.pdb"\
 /MACHINE:I386 /DEF:".\ilucp32w.def" /OUT:"$(ILUHOME)\bin\ilucp32w.dll"\
 /IMPLIB:"$(ILUHOME)\lib\ilucp32w.lib" 
DEF_FILE=".\ilucp32w.def"
LINK32_OBJS= \
	$(INTDIR)/ilu.obj \
	$(INTDIR)/iluobj.obj \
	$(INTDIR)/ilusrvr.obj

"$(ILUHOME)\bin\ilucp32w.dll" : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WinDebugW"
# PROP Intermediate_Dir "WinDebugW"
OUTDIR=.\WinDebugW
INTDIR=.\WinDebugW

ALL : $(ILUHOME)\bin\ilucp32w.dll $(OUTDIR)/ilucp32w.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Od /I "\ilu\src\runtime\kernel" /I "\ilu\src\winio" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINIO" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Od /I "$(ILUSRC)\runtime\kernel" /I\
 "$(ILUSRC)\winio" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINIO"\
 /Fp$(OUTDIR)/"ilucp32w.pch" /Fo$(INTDIR)/ /Fd$(OUTDIR)/"ilucp32w.pdb" /c /D "ILU_BUILDING_RUNTIME" $(ILU_DEBUG_CPPFLAGS)
CPP_OBJS=.\WinDebugW/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"ilucp32w.bsc" 

$(OUTDIR)/ilucp32w.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386
# ADD LINK32 \ilu\lib\ilu32w.lib \ilu\lib\winio32w.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386 /OUT:"\ilu\bin\ilucp32w.dll" /IMPLIB:"\ilu\lib\ilucp32w.lib"
# SUBTRACT LINK32 /PDB:none /MAP
LINK32_FLAGS=/PROFILE /MAP $(ILUHOME)\lib\ilu32w.lib $(ILUHOME)\lib\winio32w.lib wsock32.lib kernel32.lib\
 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib\
 ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO\
 /SUBSYSTEM:windows /DLL /INCREMENTAL:yes /PDB:$(OUTDIR)/"ilucp32w.pdb" /DEBUG\
 /MACHINE:I386 /DEF:".\ilucp32w.def" /OUT:"$(ILUHOME)\bin\ilucp32w.dll"\
 /IMPLIB:"$(ILUHOME)\lib\ilucp32w.lib" 
DEF_FILE=".\ilucp32w.def"
LINK32_OBJS= \
	$(INTDIR)/ilu.obj \
	$(INTDIR)/iluobj.obj \
	$(INTDIR)/ilusrvr.obj

"$(ILUHOME)\bin\ilucp32w.dll" : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Group "Source Files"

################################################################################
# Begin Source File

SOURCE=.\ilu.cpp
DEP_ILU_C=\
	".\ilu.hh"\
	"$(ILUSRC)\winio\winiodef.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"

$(INTDIR)/ilu.obj :  $(SOURCE)  $(DEP_ILU_C) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ilucp32w.def
# End Source File
################################################################################
# Begin Source File

SOURCE=.\iluobj.cpp
DEP_ILUOB=\
	".\ilu.hh"\
	"$(ILUSRC)\winio\winiodef.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"

$(INTDIR)/iluobj.obj :  $(SOURCE)  $(DEP_ILUOB) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ilusrvr.cpp
DEP_ILUSE=\
	"$(ILUSRC)\winio\winiodef.h"\
	".\ilu.hh"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"

$(INTDIR)/ilusrvr.obj :  $(SOURCE)  $(DEP_ILUSE) $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################
