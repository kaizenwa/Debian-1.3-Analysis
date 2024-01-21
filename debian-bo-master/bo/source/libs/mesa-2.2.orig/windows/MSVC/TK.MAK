# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=TK - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to TK - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "TK - Win32 Release" && "$(CFG)" != "TK - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "TK.MAK" CFG="TK - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "TK - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "TK - Win32 Debug" (based on "Win32 (x86) Static Library")
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
CPP=cl.exe

!IF  "$(CFG)" == "TK - Win32 Release"

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

ALL : "$(OUTDIR)\TK.lib"

CLEAN : 
	-@erase ".\WinRel\TK.lib"
	-@erase ".\WinRel\tkwndws.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /W3 /GX /O2 /I "\wing\include" /I "..\..\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "\wing\include" /I "..\..\include" /D\
 "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /Fp"$(INTDIR)/TK.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/TK.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/TK.lib" 
LIB32_OBJS= \
	"$(INTDIR)/tkwndws.obj"

"$(OUTDIR)\TK.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "TK - Win32 Debug"

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

ALL : "$(OUTDIR)\TK.lib"

CLEAN : 
	-@erase ".\WinDebug\TK.lib"
	-@erase ".\WinDebug\tkwndws.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "\wing\include" /I "..\..\include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MLd /W3 /GX /Z7 /Od /I "\wing\include" /I "..\..\include" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /Fp"$(INTDIR)/TK.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/TK.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/TK.lib" 
LIB32_OBJS= \
	"$(INTDIR)/tkwndws.obj"

"$(OUTDIR)\TK.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "TK - Win32 Release"
# Name "TK - Win32 Debug"

!IF  "$(CFG)" == "TK - Win32 Release"

!ELSEIF  "$(CFG)" == "TK - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE="\Mesa\src-tk\tkwndws.c"
DEP_CPP_TKWND=\
	".\..\..\include\gltk.h"\
	".\..\..\include\gl\wmesa.h"\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\GL/glu.h"\
	

"$(INTDIR)\tkwndws.obj" : $(SOURCE) $(DEP_CPP_TKWND) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
