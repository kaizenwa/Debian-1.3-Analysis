# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "msoft32.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRel"
# PROP Intermediate_Dir "WinRel"
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : .\ncgen.exe $(OUTDIR)/msoft32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /ML /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /ML /Za /W3 /GX /YX /O2 /I "..\libsrc" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "MSDOS" /FR /c
CPP_PROJ=/nologo /ML /Za /W3 /GX /YX /O2 /I "..\libsrc" /D "NDEBUG" /D "WIN32"\
 /D "_CONSOLE" /D "MSDOS" /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/\
 /c 
CPP_OBJS=.\WinRel/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"msoft32.bsc" 
BSC32_SBRS= \
	$(INTDIR)/getfill.sbr \
	$(INTDIR)/init.sbr \
	$(INTDIR)/msofttab.sbr \
	$(INTDIR)/genlib.sbr \
	$(INTDIR)/main.sbr \
	$(INTDIR)/load.sbr \
	$(INTDIR)/close.sbr \
	$(INTDIR)/escapes.sbr \
	$(INTDIR)/getopt.sbr

$(OUTDIR)/msoft32.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ..\libsrc\netcdf.lib ..\xdr\xdr.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386 /OUT:"ncgen.exe"
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib ..\libsrc\netcdf.lib ..\xdr\xdr.lib /NOLOGO /SUBSYSTEM:console\
 /INCREMENTAL:no /PDB:$(OUTDIR)/"msoft32.pdb" /MACHINE:I386 /OUT:"ncgen.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/getfill.obj \
	$(INTDIR)/init.obj \
	$(INTDIR)/msofttab.obj \
	$(INTDIR)/genlib.obj \
	$(INTDIR)/main.obj \
	$(INTDIR)/load.obj \
	$(INTDIR)/close.obj \
	$(INTDIR)/escapes.obj \
	$(INTDIR)/getopt.obj

.\ncgen.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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
# PROP Output_Dir "WinDebug"
# PROP Intermediate_Dir "WinDebug"
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : .\ncgen.exe $(OUTDIR)/msoft32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /ML /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /ML /Za /W3 /GX /Zi /YX /Od /I "..\libsrc" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "MSDOS" /FR /c
CPP_PROJ=/nologo /ML /Za /W3 /GX /Zi /YX /Od /I "..\libsrc" /D "_DEBUG" /D\
 "WIN32" /D "_CONSOLE" /D "MSDOS" /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch"\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"msoft32.pdb" /c 
CPP_OBJS=.\WinDebug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"msoft32.bsc" 
BSC32_SBRS= \
	$(INTDIR)/getfill.sbr \
	$(INTDIR)/init.sbr \
	$(INTDIR)/msofttab.sbr \
	$(INTDIR)/genlib.sbr \
	$(INTDIR)/main.sbr \
	$(INTDIR)/load.sbr \
	$(INTDIR)/close.sbr \
	$(INTDIR)/escapes.sbr \
	$(INTDIR)/getopt.sbr

$(OUTDIR)/msoft32.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ..\libsrc\netcdf.lib ..\xdr\xdr.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386 /OUT:"ncgen.exe"
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib ..\libsrc\netcdf.lib ..\xdr\xdr.lib /NOLOGO /SUBSYSTEM:console\
 /INCREMENTAL:yes /PDB:$(OUTDIR)/"msoft32.pdb" /DEBUG /MACHINE:I386\
 /OUT:"ncgen.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/getfill.obj \
	$(INTDIR)/init.obj \
	$(INTDIR)/msofttab.obj \
	$(INTDIR)/genlib.obj \
	$(INTDIR)/main.obj \
	$(INTDIR)/load.obj \
	$(INTDIR)/close.obj \
	$(INTDIR)/escapes.obj \
	$(INTDIR)/getopt.obj

.\ncgen.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\getfill.c
DEP_GETFI=\
	.\generic.h\
	.\genlib.h

$(INTDIR)/getfill.obj :  $(SOURCE)  $(DEP_GETFI) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\init.c
DEP_INIT_=\
	.\generic.h\
	.\ncgen.h

$(INTDIR)/init.obj :  $(SOURCE)  $(DEP_INIT_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\msofttab.c
DEP_MSOFT=\
	.\ncgen.h

$(INTDIR)/msofttab.obj :  $(SOURCE)  $(DEP_MSOFT) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\genlib.c
DEP_GENLI=\
	.\generic.h\
	.\ncgen.h\
	.\genlib.h

$(INTDIR)/genlib.obj :  $(SOURCE)  $(DEP_GENLI) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\main.c
DEP_MAIN_=\
	.\genlib.h

$(INTDIR)/main.obj :  $(SOURCE)  $(DEP_MAIN_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\load.c
DEP_LOAD_=\
	.\generic.h\
	.\ncgen.h\
	.\genlib.h

$(INTDIR)/load.obj :  $(SOURCE)  $(DEP_LOAD_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\close.c
DEP_CLOSE=\
	.\generic.h\
	.\ncgen.h\
	.\genlib.h

$(INTDIR)/close.obj :  $(SOURCE)  $(DEP_CLOSE) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\escapes.c
DEP_ESCAP=\
	.\genlib.h

$(INTDIR)/escapes.obj :  $(SOURCE)  $(DEP_ESCAP) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=\USERS\DWD\src\netcdf\util\getopt.c

$(INTDIR)/getopt.obj :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

# End Source File
# End Group
# End Project
################################################################################
