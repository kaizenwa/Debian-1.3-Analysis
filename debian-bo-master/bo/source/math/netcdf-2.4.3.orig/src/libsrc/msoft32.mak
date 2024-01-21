# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

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
!MESSAGE "Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
# PROP Target_Last_Scanned "Win32 Debug"
CPP=cl.exe

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

ALL : .\netcdf.lib $(OUTDIR)/msoft32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR /c
CPP_PROJ=/nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinRel/
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"msoft32.bsc" 
BSC32_SBRS= \
	$(INTDIR)/xdrposix.sbr \
	$(INTDIR)/putget.sbr \
	$(INTDIR)/sharray.sbr \
	$(INTDIR)/file.sbr \
	$(INTDIR)/globdef.sbr \
	$(INTDIR)/dim.sbr \
	$(INTDIR)/cdf.sbr \
	$(INTDIR)/string.sbr \
	$(INTDIR)/var.sbr \
	$(INTDIR)/putgetg.sbr \
	$(INTDIR)/attr.sbr \
	$(INTDIR)/iarray.sbr \
	$(INTDIR)/error.sbr \
	$(INTDIR)/array.sbr \
	$(INTDIR)/jackets.sbr

$(OUTDIR)/msoft32.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=lib.exe
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO /OUT:"netcdf.lib"
LIB32_FLAGS=/NOLOGO /OUT:"netcdf.lib" 
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	$(INTDIR)/xdrposix.obj \
	$(INTDIR)/putget.obj \
	$(INTDIR)/sharray.obj \
	$(INTDIR)/file.obj \
	$(INTDIR)/globdef.obj \
	$(INTDIR)/dim.obj \
	$(INTDIR)/cdf.obj \
	$(INTDIR)/string.obj \
	$(INTDIR)/var.obj \
	$(INTDIR)/putgetg.obj \
	$(INTDIR)/attr.obj \
	$(INTDIR)/iarray.obj \
	$(INTDIR)/error.obj \
	$(INTDIR)/array.obj \
	$(INTDIR)/jackets.obj

.\netcdf.lib : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
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

ALL : .\netcdf.lib $(OUTDIR)/msoft32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /W3 /GX /Z7 /YX /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR /c
CPP_PROJ=/nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D "_DEBUG"\
 /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinDebug/
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"msoft32.bsc" 
BSC32_SBRS= \
	$(INTDIR)/xdrposix.sbr \
	$(INTDIR)/putget.sbr \
	$(INTDIR)/sharray.sbr \
	$(INTDIR)/file.sbr \
	$(INTDIR)/globdef.sbr \
	$(INTDIR)/dim.sbr \
	$(INTDIR)/cdf.sbr \
	$(INTDIR)/string.sbr \
	$(INTDIR)/var.sbr \
	$(INTDIR)/putgetg.sbr \
	$(INTDIR)/attr.sbr \
	$(INTDIR)/iarray.sbr \
	$(INTDIR)/error.sbr \
	$(INTDIR)/array.sbr \
	$(INTDIR)/jackets.sbr

$(OUTDIR)/msoft32.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=lib.exe
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO /OUT:"netcdf.lib"
LIB32_FLAGS=/NOLOGO /OUT:"netcdf.lib" 
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	$(INTDIR)/xdrposix.obj \
	$(INTDIR)/putget.obj \
	$(INTDIR)/sharray.obj \
	$(INTDIR)/file.obj \
	$(INTDIR)/globdef.obj \
	$(INTDIR)/dim.obj \
	$(INTDIR)/cdf.obj \
	$(INTDIR)/string.obj \
	$(INTDIR)/var.obj \
	$(INTDIR)/putgetg.obj \
	$(INTDIR)/attr.obj \
	$(INTDIR)/iarray.obj \
	$(INTDIR)/error.obj \
	$(INTDIR)/array.obj \
	$(INTDIR)/jackets.obj

.\netcdf.lib : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
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

SOURCE=.\xdrposix.c
DEP_XDRPO=\
	\MSVC20\INCLUDE\SYS\TYPES.H\
	.\alloc.h\
	.\netcdf.h\
	.\local_nc.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/xdrposix.obj :  $(SOURCE)  $(DEP_XDRPO) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/xdrposix.obj :  $(SOURCE)  $(DEP_XDRPO) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\putget.c
DEP_PUTGE=\
	.\local_nc.h\
	.\alloc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/putget.obj :  $(SOURCE)  $(DEP_PUTGE) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/putget.obj :  $(SOURCE)  $(DEP_PUTGE) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sharray.c
DEP_SHARR=\
	.\local_nc.h\
	.\alloc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/sharray.obj :  $(SOURCE)  $(DEP_SHARR) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/sharray.obj :  $(SOURCE)  $(DEP_SHARR) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\file.c
DEP_FILE_=\
	.\local_nc.h\
	.\alloc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/file.obj :  $(SOURCE)  $(DEP_FILE_) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/file.obj :  $(SOURCE)  $(DEP_FILE_) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\globdef.c
DEP_GLOBD=\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/globdef.obj :  $(SOURCE)  $(DEP_GLOBD) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/globdef.obj :  $(SOURCE)  $(DEP_GLOBD) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dim.c
DEP_DIM_C=\
	.\local_nc.h\
	.\alloc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/dim.obj :  $(SOURCE)  $(DEP_DIM_C) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/dim.obj :  $(SOURCE)  $(DEP_DIM_C) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cdf.c
DEP_CDF_C=\
	.\local_nc.h\
	.\alloc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/cdf.obj :  $(SOURCE)  $(DEP_CDF_C) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/cdf.obj :  $(SOURCE)  $(DEP_CDF_C) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\string.c
DEP_STRIN=\
	.\local_nc.h\
	.\alloc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/string.obj :  $(SOURCE)  $(DEP_STRIN) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/string.obj :  $(SOURCE)  $(DEP_STRIN) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\var.c
DEP_VAR_C=\
	.\local_nc.h\
	.\alloc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/var.obj :  $(SOURCE)  $(DEP_VAR_C) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/var.obj :  $(SOURCE)  $(DEP_VAR_C) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\putgetg.c
DEP_PUTGET=\
	.\local_nc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/putgetg.obj :  $(SOURCE)  $(DEP_PUTGET) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/putgetg.obj :  $(SOURCE)  $(DEP_PUTGET) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\attr.c
DEP_ATTR_=\
	.\local_nc.h\
	.\alloc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/attr.obj :  $(SOURCE)  $(DEP_ATTR_) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/attr.obj :  $(SOURCE)  $(DEP_ATTR_) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\iarray.c
DEP_IARRA=\
	.\local_nc.h\
	.\alloc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/iarray.obj :  $(SOURCE)  $(DEP_IARRA) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/iarray.obj :  $(SOURCE)  $(DEP_IARRA) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\error.c
DEP_ERROR=\
	.\local_nc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/error.obj :  $(SOURCE)  $(DEP_ERROR) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/error.obj :  $(SOURCE)  $(DEP_ERROR) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\array.c
DEP_ARRAY=\
	.\local_nc.h\
	.\alloc.h\
	.\netcdf.h

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/array.obj :  $(SOURCE)  $(DEP_ARRAY) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/array.obj :  $(SOURCE)  $(DEP_ARRAY) $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\USERS\DWD\src\netcdf\fortran\msoft32\jackets.c

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/jackets.obj :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /YX /O2 /I "..\xdr" /I "..\libsrc" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D "NO_SYSTEM_XDR_INCLUDES" /D\
 "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID" /FR$(INTDIR)/\
 /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

# ADD CPP /Za

$(INTDIR)/jackets.obj :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /Za /W3 /GX /Z7 /YX /Od /I "..\xdr" /I "..\libsrc" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "MSDOS" /D "SWAP" /D\
 "NO_SYSTEM_XDR_INCLUDES" /D "DOS_FS" /D "WINNT" /D "NO_ACCESS" /D "NO_GETPID"\
 /FR$(INTDIR)/ /Fp$(OUTDIR)/"msoft32.pch" /Fo$(INTDIR)/ /c  $(SOURCE) 

!ENDIF 

# End Source File
# End Group
# End Project
################################################################################
