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

ALL : .\nctest.exe $(OUTDIR)/msoft32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /ML /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /ML /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "MSDOS" /FR /c
CPP_PROJ=/nologo /ML /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG"\
 /D "WIN32" /D "_CONSOLE" /D "MSDOS" /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch"\
 /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinRel/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"msoft32.bsc" 
BSC32_SBRS= \
	$(INTDIR)/slabs.sbr \
	$(INTDIR)/varputg.sbr \
	$(INTDIR)/varput.sbr \
	$(INTDIR)/driver.sbr \
	$(INTDIR)/misctest.sbr \
	$(INTDIR)/atttests.sbr \
	$(INTDIR)/vargetg.sbr \
	$(INTDIR)/vardef.sbr \
	$(INTDIR)/add.sbr \
	$(INTDIR)/rec.sbr \
	$(INTDIR)/varget.sbr \
	$(INTDIR)/vputget.sbr \
	$(INTDIR)/val.sbr \
	$(INTDIR)/error.sbr \
	$(INTDIR)/vartests.sbr \
	$(INTDIR)/emalloc.sbr \
	$(INTDIR)/vputgetg.sbr \
	$(INTDIR)/dimtests.sbr \
	$(INTDIR)/cdftests.sbr

$(OUTDIR)/msoft32.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ..\libsrc\netcdf.lib ..\xdr\xdr.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386 /OUT:"nctest.exe"
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib ..\libsrc\netcdf.lib ..\xdr\xdr.lib /NOLOGO /SUBSYSTEM:console\
 /INCREMENTAL:no /PDB:$(OUTDIR)/"msoft32.pdb" /MACHINE:I386 /OUT:"nctest.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/slabs.obj \
	$(INTDIR)/varputg.obj \
	$(INTDIR)/varput.obj \
	$(INTDIR)/driver.obj \
	$(INTDIR)/misctest.obj \
	$(INTDIR)/atttests.obj \
	$(INTDIR)/vargetg.obj \
	$(INTDIR)/vardef.obj \
	$(INTDIR)/add.obj \
	$(INTDIR)/rec.obj \
	$(INTDIR)/varget.obj \
	$(INTDIR)/vputget.obj \
	$(INTDIR)/val.obj \
	$(INTDIR)/error.obj \
	$(INTDIR)/vartests.obj \
	$(INTDIR)/emalloc.obj \
	$(INTDIR)/vputgetg.obj \
	$(INTDIR)/dimtests.obj \
	$(INTDIR)/cdftests.obj

.\nctest.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

ALL : .\nctest.exe $(OUTDIR)/msoft32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /ML /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /ML /Za /W3 /GX /Zi /YX /Od /I "..\xdr" /I "..\libsrc" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "MSDOS" /FR /c
CPP_PROJ=/nologo /ML /Za /W3 /GX /Zi /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "MSDOS" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /Fd$(OUTDIR)/"msoft32.pdb" /c 
CPP_OBJS=.\WinDebug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"msoft32.bsc" 
BSC32_SBRS= \
	$(INTDIR)/slabs.sbr \
	$(INTDIR)/varputg.sbr \
	$(INTDIR)/varput.sbr \
	$(INTDIR)/driver.sbr \
	$(INTDIR)/misctest.sbr \
	$(INTDIR)/atttests.sbr \
	$(INTDIR)/vargetg.sbr \
	$(INTDIR)/vardef.sbr \
	$(INTDIR)/add.sbr \
	$(INTDIR)/rec.sbr \
	$(INTDIR)/varget.sbr \
	$(INTDIR)/vputget.sbr \
	$(INTDIR)/val.sbr \
	$(INTDIR)/error.sbr \
	$(INTDIR)/vartests.sbr \
	$(INTDIR)/emalloc.sbr \
	$(INTDIR)/vputgetg.sbr \
	$(INTDIR)/dimtests.sbr \
	$(INTDIR)/cdftests.sbr

$(OUTDIR)/msoft32.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ..\libsrc\netcdf.lib ..\xdr\xdr.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386 /OUT:"nctest.exe"
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib ..\libsrc\netcdf.lib ..\xdr\xdr.lib /NOLOGO /SUBSYSTEM:console\
 /INCREMENTAL:yes /PDB:$(OUTDIR)/"msoft32.pdb" /DEBUG /MACHINE:I386\
 /OUT:"nctest.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/slabs.obj \
	$(INTDIR)/varputg.obj \
	$(INTDIR)/varput.obj \
	$(INTDIR)/driver.obj \
	$(INTDIR)/misctest.obj \
	$(INTDIR)/atttests.obj \
	$(INTDIR)/vargetg.obj \
	$(INTDIR)/vardef.obj \
	$(INTDIR)/add.obj \
	$(INTDIR)/rec.obj \
	$(INTDIR)/varget.obj \
	$(INTDIR)/vputget.obj \
	$(INTDIR)/val.obj \
	$(INTDIR)/error.obj \
	$(INTDIR)/vartests.obj \
	$(INTDIR)/emalloc.obj \
	$(INTDIR)/vputgetg.obj \
	$(INTDIR)/dimtests.obj \
	$(INTDIR)/cdftests.obj

.\nctest.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\slabs.c
DEP_SLABS=\
	.\testcdf.h\
	.\add.h\
	.\error.h\
	.\tests.h

$(INTDIR)/slabs.obj :  $(SOURCE)  $(DEP_SLABS) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\varputg.c
DEP_VARPU=\
	.\testcdf.h\
	.\val.h\
	.\error.h\
	.\tests.h

$(INTDIR)/varputg.obj :  $(SOURCE)  $(DEP_VARPU) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\varput.c
DEP_VARPUT=\
	.\testcdf.h\
	.\val.h\
	.\error.h\
	.\tests.h

$(INTDIR)/varput.obj :  $(SOURCE)  $(DEP_VARPUT) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\driver.c
DEP_DRIVE=\
	.\tests.h

$(INTDIR)/driver.obj :  $(SOURCE)  $(DEP_DRIVE) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\misctest.c
DEP_MISCT=\
	.\testcdf.h\
	.\add.h\
	.\error.h

$(INTDIR)/misctest.obj :  $(SOURCE)  $(DEP_MISCT) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\atttests.c
DEP_ATTTE=\
	.\testcdf.h\
	.\add.h\
	.\error.h\
	.\emalloc.h\
	.\tests.h\
	.\val.h

$(INTDIR)/atttests.obj :  $(SOURCE)  $(DEP_ATTTE) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vargetg.c
DEP_VARGE=\
	.\testcdf.h\
	.\error.h\
	.\tests.h

$(INTDIR)/vargetg.obj :  $(SOURCE)  $(DEP_VARGE) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vardef.c
DEP_VARDE=\
	.\testcdf.h\
	.\add.h\
	.\error.h\
	.\tests.h

$(INTDIR)/vardef.obj :  $(SOURCE)  $(DEP_VARDE) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\add.c
DEP_ADD_C=\
	.\testcdf.h\
	.\add.h\
	.\emalloc.h

$(INTDIR)/add.obj :  $(SOURCE)  $(DEP_ADD_C) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\rec.c
DEP_REC_C=\
	.\testcdf.h\
	.\val.h\
	.\error.h\
	.\tests.h

$(INTDIR)/rec.obj :  $(SOURCE)  $(DEP_REC_C) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\varget.c
DEP_VARGET=\
	.\testcdf.h\
	.\error.h\
	.\tests.h

$(INTDIR)/varget.obj :  $(SOURCE)  $(DEP_VARGET) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vputget.c
DEP_VPUTG=\
	.\testcdf.h\
	.\add.h\
	.\val.h\
	.\error.h\
	.\tests.h\
	.\emalloc.h

$(INTDIR)/vputget.obj :  $(SOURCE)  $(DEP_VPUTG) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\val.c
DEP_VAL_C=\
	.\testcdf.h\
	.\val.h\
	.\error.h

$(INTDIR)/val.obj :  $(SOURCE)  $(DEP_VAL_C) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\error.c
DEP_ERROR=\
	.\error.h

$(INTDIR)/error.obj :  $(SOURCE)  $(DEP_ERROR) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vartests.c
DEP_VARTE=\
	.\testcdf.h\
	.\add.h\
	.\error.h\
	.\tests.h

$(INTDIR)/vartests.obj :  $(SOURCE)  $(DEP_VARTE) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\emalloc.c
DEP_EMALL=\
	.\error.h\
	.\emalloc.h

$(INTDIR)/emalloc.obj :  $(SOURCE)  $(DEP_EMALL) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\vputgetg.c
DEP_VPUTGE=\
	.\testcdf.h\
	.\add.h\
	.\val.h\
	.\error.h\
	.\tests.h\
	.\emalloc.h

$(INTDIR)/vputgetg.obj :  $(SOURCE)  $(DEP_VPUTGE) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dimtests.c
DEP_DIMTE=\
	.\testcdf.h\
	.\add.h\
	.\error.h\
	.\tests.h

$(INTDIR)/dimtests.obj :  $(SOURCE)  $(DEP_DIMTE) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cdftests.c
DEP_CDFTE=\
	.\testcdf.h\
	.\add.h\
	.\error.h\
	.\tests.h

$(INTDIR)/cdftests.obj :  $(SOURCE)  $(DEP_CDFTE) $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################
