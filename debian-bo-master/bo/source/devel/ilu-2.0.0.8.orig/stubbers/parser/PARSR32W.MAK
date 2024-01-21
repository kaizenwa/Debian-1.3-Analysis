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
!MESSAGE NMAKE /f "parsr32w.mak" CFG="Win32 Debug"
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

ALL : $(ILUHOME)\bin\parsr32w.dll .\WinRelW\parsr32w.bsc

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
 /Fp$(OUTDIR)/"parsr32w.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinRelW/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"parsr32w.bsc" 

.\WinRelW\parsr32w.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386
# ADD LINK32 \ilu\lib\winio32w.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386 /OUT:"\ilu\bin\parsr32w.dll" /IMPLIB:"\ilu\lib\parsr32w.lib"
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS=$(ILUHOME)\lib\winio32w.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL\
 /INCREMENTAL:no /PDB:$(OUTDIR)/"parsr32w.pdb" /MACHINE:I386\
 /DEF:".\parsr32w.def" /OUT:"$(ILUHOME)\bin\parsr32w.dll"\
 /IMPLIB:"$(ILUHOME)\lib\parsr32w.lib" 
DEF_FILE=.\parsr32w.def
LINK32_OBJS= \
	.\WinRelW\iluparse.obj \
	.\WinRelW\malloc.obj \
	.\WinRelW\pathname.obj \
	.\WinRelW\shs.obj \
	.\WinRelW\typeuid.obj \
	.\WinRelW\util.obj

$(ILUHOME)\bin\parsr32w.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

ALL : $(ILUHOME)\bin\parsr32w.dll .\WinDebugW\parsr32w.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Od /I "\ilu\src\runtime\kernel" /I "\ilu\src\winio" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINIO" /c
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Od /I "$(ILUSRC)\runtime\kernel" /I\
 "$(ILUSRC)\winio" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINIO"\
 /Fp$(OUTDIR)/"parsr32w.pch" /Fo$(INTDIR)/ /Fd$(OUTDIR)/"parsr32w.pdb" /c 
CPP_OBJS=.\WinDebugW/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"parsr32w.bsc" 

.\WinDebugW\parsr32w.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386
# ADD LINK32 \ilu\lib\winio32w.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386 /OUT:"\ilu\bin\parsr32w.dll" /IMPLIB:"\ilu\lib\parsr32w.lib"
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS=$(ILUHOME)\lib\winio32w.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL\
 /INCREMENTAL:yes /PDB:$(OUTDIR)/"parsr32w.pdb" /DEBUG /MACHINE:I386\
 /DEF:".\parsr32w.def" /OUT:"$(ILUHOME)\bin\parsr32w.dll"\
 /IMPLIB:"$(ILUHOME)\lib\parsr32w.lib" 
DEF_FILE=.\parsr32w.def
LINK32_OBJS= \
	.\WinDebugW\iluparse.obj \
	.\WinDebugW\malloc.obj \
	.\WinDebugW\pathname.obj \
	.\WinDebugW\shs.obj \
	.\WinDebugW\typeuid.obj \
	.\WinDebugW\util.obj

$(ILUHOME)\bin\parsr32w.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\iluparse.c
DEP_ILUPA=\
	$(ILUSRC)\winio\winiodef.h\
	.\iluptype.h\
	$(ILUSRC)\runtime\kernel\version.h

!IF  "$(CFG)" == "Win32 Release"

.\WinRelW\iluparse.obj :  $(SOURCE)  $(DEP_ILUPA) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

.\WinDebugW\iluparse.obj :  $(SOURCE)  $(DEP_ILUPA) $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\malloc.c
DEP_MALLO=\
	$(ILUSRC)\winio\winiodef.h

!IF  "$(CFG)" == "Win32 Release"

.\WinRelW\malloc.obj :  $(SOURCE)  $(DEP_MALLO) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

.\WinDebugW\malloc.obj :  $(SOURCE)  $(DEP_MALLO) $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\pathname.c
DEP_PATHN=\
	$(ILUSRC)\winio\winiodef.h

!IF  "$(CFG)" == "Win32 Release"

.\WinRelW\pathname.obj :  $(SOURCE)  $(DEP_PATHN) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

.\WinDebugW\pathname.obj :  $(SOURCE)  $(DEP_PATHN) $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\shs.c
DEP_SHS_C=\
	$(ILUSRC)\winio\winiodef.h\
	.\shs.h

!IF  "$(CFG)" == "Win32 Release"

.\WinRelW\shs.obj :  $(SOURCE)  $(DEP_SHS_C) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

.\WinDebugW\shs.obj :  $(SOURCE)  $(DEP_SHS_C) $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\typeuid.c
DEP_TYPEU=\
	$(ILUSRC)\winio\winiodef.h\
	.\iluptype.h\
	.\shs.h

!IF  "$(CFG)" == "Win32 Release"

.\WinRelW\typeuid.obj :  $(SOURCE)  $(DEP_TYPEU) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

.\WinDebugW\typeuid.obj :  $(SOURCE)  $(DEP_TYPEU) $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\util.c
DEP_UTIL_=\
	$(ILUSRC)\winio\winiodef.h\
	$(ILUSRC)\runtime\kernel\version.h\
	.\iluptype.h

!IF  "$(CFG)" == "Win32 Release"

.\WinRelW\util.obj :  $(SOURCE)  $(DEP_UTIL_) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

.\WinDebugW\util.obj :  $(SOURCE)  $(DEP_UTIL_) $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\parsr32w.def
# End Source File
# End Group
# End Project
################################################################################
