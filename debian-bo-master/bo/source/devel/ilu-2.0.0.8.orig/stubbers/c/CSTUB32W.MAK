# Modified so that $(ILUHOME) can be used to specifiy location
# Note that if you use this as a VC++ make file, $(ILUHOME) will be \ilu
# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "cstub32w.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Application")
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

ALL : $(ILUHOME)\bin\cstub32w.exe $(OUTDIR)/cstub32w.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Ox /Oy- /I "\ilu\src\stubbers\parser" /I "\ilu\src\winio" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_WINIO" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Ox /Oy- /I "$(ILUSRC)\stubbers\parser" /I\
 "$(ILUSRC)\winio" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_WINIO"\
 /Fp$(OUTDIR)/"cstub32w.pch" /Fo$(INTDIR)/ /c /I "$(ILUSRC)\runtime\kernel" 
CPP_OBJS=.\WinRelW/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo$(INTDIR)/"wcstub.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"cstub32w.bsc" 

$(OUTDIR)/cstub32w.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /MACHINE:I386
# ADD LINK32 \ilu\lib\parsr32w.lib \ilu\lib\winio32w.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /MACHINE:I386 /OUT:"\ilu\bin\cstub32w.exe"
LINK32_FLAGS=$(ILUHOME)\lib\parsr32w.lib $(ILUHOME)\lib\winio32w.lib kernel32.lib\
 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib\
 ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO\
 /SUBSYSTEM:windows /INCREMENTAL:no /PDB:$(OUTDIR)/"cstub32w.pdb" /MACHINE:I386\
 /OUT:"$(ILUHOME)\bin\cstub32w.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/cheaders.obj \
	$(INTDIR)/client.obj \
	$(INTDIR)/code.obj \
	$(INTDIR)/common.obj \
	$(INTDIR)/isl2c.obj \
	$(INTDIR)/names.obj \
	$(INTDIR)/server.obj \
	$(INTDIR)/util.obj \
	$(INTDIR)/wcstub.obj \
	$(INTDIR)/wcstub.res

$(ILUHOME)\bin\cstub32w.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

ALL : $(ILUHOME)\bin\cstub32w.exe $(OUTDIR)/cstub32w.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE CPP /nologo /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Od /I "\ilu\src\stubbers\parser" /I "\ilu\src\winio" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_WINIO" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Od /I "$(ILUSRC)\stubbers\parser" /I\
 "$(ILUSRC)\winio" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_WINIO"\
 /Fp$(OUTDIR)/"cstub32w.pch" /Fo$(INTDIR)/ /Fd$(OUTDIR)/"cstub32w.pdb" /c /I "$(ILUSRC)\runtime\kernel" 
CPP_OBJS=.\WinDebugW/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo$(INTDIR)/"wcstub.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"cstub32w.bsc" 

$(OUTDIR)/cstub32w.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DEBUG /MACHINE:I386
# ADD LINK32 \ilu\lib\parsr32w.lib \ilu\lib\winio32w.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DEBUG /MACHINE:I386 /OUT:"\ilu\bin\cstub32w.exe"
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS=$(ILUHOME)\lib\parsr32w.lib $(ILUHOME)\lib\winio32w.lib kernel32.lib\
 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib\
 ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO\
 /SUBSYSTEM:windows /INCREMENTAL:yes /PDB:$(OUTDIR)/"cstub32w.pdb" /DEBUG\
 /MACHINE:I386 /OUT:"$(ILUHOME)\bin\cstub32w.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/cheaders.obj \
	$(INTDIR)/client.obj \
	$(INTDIR)/code.obj \
	$(INTDIR)/common.obj \
	$(INTDIR)/isl2c.obj \
	$(INTDIR)/names.obj \
	$(INTDIR)/server.obj \
	$(INTDIR)/util.obj \
	$(INTDIR)/wcstub.obj \
	$(INTDIR)/wcstub.res

$(ILUHOME)\bin\cstub32w.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\cheaders.c
DEP_CHEAD=\
	.\cstubber.h\
	$(ILUSRC)\winio\winiodef.h\
	$(ILUSRC)\stubbers\parser\iluptype.h

$(INTDIR)/cheaders.obj :  $(SOURCE)  $(DEP_CHEAD) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\client.c
DEP_CLIEN=\
	.\cstubber.h\
	$(ILUSRC)\winio\winiodef.h\
	$(ILUSRC)\stubbers\parser\iluptype.h

$(INTDIR)/client.obj :  $(SOURCE)  $(DEP_CLIEN) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\code.c
DEP_CODE_=\
	.\cstubber.h\
	$(ILUSRC)\winio\winiodef.h\
	$(ILUSRC)\stubbers\parser\iluptype.h

$(INTDIR)/code.obj :  $(SOURCE)  $(DEP_CODE_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\common.c
DEP_COMMO=\
	.\cstubber.h\
	$(ILUSRC)\winio\winiodef.h\
	$(ILUSRC)\stubbers\parser\iluptype.h

$(INTDIR)/common.obj :  $(SOURCE)  $(DEP_COMMO) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\isl2c.c
DEP_ISL2C=\
	.\cstubber.h\
	$(ILUSRC)\winio\winiodef.h\
	$(ILUSRC)\stubbers\parser\iluptype.h

$(INTDIR)/isl2c.obj :  $(SOURCE)  $(DEP_ISL2C) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\names.c

$(INTDIR)/names.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\server.c
DEP_SERVE=\
	.\cstubber.h\
	$(ILUSRC)\winio\winiodef.h\
	$(ILUSRC)\stubbers\parser\iluptype.h

$(INTDIR)/server.obj :  $(SOURCE)  $(DEP_SERVE) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\util.c
DEP_UTIL_=\
	.\cstubber.h\
	$(ILUSRC)\winio\winiodef.h\
	$(ILUSRC)\stubbers\parser\iluptype.h

$(INTDIR)/util.obj :  $(SOURCE)  $(DEP_UTIL_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wcstub.c
DEP_WCSTU=\
	$(ILUSRC)\winio\winio.h\
	$(ILUSRC)\winio\winiodef.h

$(INTDIR)/wcstub.obj :  $(SOURCE)  $(DEP_WCSTU) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wcstub.rc
DEP_WCSTUB=\
	.\wcstub.ico

$(INTDIR)/wcstub.res :  $(SOURCE)  $(DEP_WCSTUB) $(INTDIR)
   $(RSC) $(RSC_PROJ)  $(SOURCE) 

# End Source File
# End Group
# End Project
################################################################################
