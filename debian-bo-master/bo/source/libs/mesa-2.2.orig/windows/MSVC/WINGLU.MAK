# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=WINGLU - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to WINGLU - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "WINGLU - Win32 Release" && "$(CFG)" != "WINGLU - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "WINGLU.MAK" CFG="WINGLU - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "WINGLU - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "WINGLU - Win32 Debug" (based on "Win32 (x86) Static Library")
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

!IF  "$(CFG)" == "WINGLU - Win32 Release"

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

ALL : "$(OUTDIR)\WINGLU.lib"

CLEAN : 
	-@erase ".\WinRel\WINGLU.lib"
	-@erase ".\WinRel\glu.obj"
	-@erase ".\WinRel\quadric.obj"
	-@erase ".\WinRel\mipmap.obj"
	-@erase ".\WinRel\nurbs.obj"
	-@erase ".\WinRel\project.obj"
	-@erase ".\WinRel\tess.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /W3 /GX /O2 /I "\wing\include" /I "..\..\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "\wing\include" /I "..\..\include" /D\
 "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /Fp"$(INTDIR)/WINGLU.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/WINGLU.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/WINGLU.lib" 
LIB32_OBJS= \
	"$(INTDIR)/glu.obj" \
	"$(INTDIR)/quadric.obj" \
	"$(INTDIR)/mipmap.obj" \
	"$(INTDIR)/nurbs.obj" \
	"$(INTDIR)/project.obj" \
	"$(INTDIR)/tess.obj"

"$(OUTDIR)\WINGLU.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "WINGLU - Win32 Debug"

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

ALL : "$(OUTDIR)\WINGLU.lib"

CLEAN : 
	-@erase ".\WinDebug\WINGLU.lib"
	-@erase ".\WinDebug\glu.obj"
	-@erase ".\WinDebug\quadric.obj"
	-@erase ".\WinDebug\mipmap.obj"
	-@erase ".\WinDebug\nurbs.obj"
	-@erase ".\WinDebug\project.obj"
	-@erase ".\WinDebug\tess.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "\wing\include" /I "..\..\include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MLd /W3 /GX /Z7 /Od /I "\wing\include" /I "..\..\include" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /Fp"$(INTDIR)/WINGLU.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/WINGLU.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/WINGLU.lib" 
LIB32_OBJS= \
	"$(INTDIR)/glu.obj" \
	"$(INTDIR)/quadric.obj" \
	"$(INTDIR)/mipmap.obj" \
	"$(INTDIR)/nurbs.obj" \
	"$(INTDIR)/project.obj" \
	"$(INTDIR)/tess.obj"

"$(OUTDIR)\WINGLU.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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

# Name "WINGLU - Win32 Release"
# Name "WINGLU - Win32 Debug"

!IF  "$(CFG)" == "WINGLU - Win32 Release"

!ELSEIF  "$(CFG)" == "WINGLU - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE="\Mesa\src-glu\glu.c"
DEP_CPP_GLU_C=\
	"..\..\src-glu\gluP.h"\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\GL/glu.h"\
	

"$(INTDIR)\glu.obj" : $(SOURCE) $(DEP_CPP_GLU_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\Mesa\src-glu\quadric.c"
DEP_CPP_QUADR=\
	"..\..\src-glu\gluP.h"\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\GL/glu.h"\
	

"$(INTDIR)\quadric.obj" : $(SOURCE) $(DEP_CPP_QUADR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\Mesa\src-glu\mipmap.c"
DEP_CPP_MIPMA=\
	"..\..\src-glu\gluP.h"\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\GL/glu.h"\
	

"$(INTDIR)\mipmap.obj" : $(SOURCE) $(DEP_CPP_MIPMA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\Mesa\src-glu\nurbs.c"
DEP_CPP_NURBS=\
	"..\..\src-glu\nurbs.h"\
	"..\..\src-glu\gluP.h"\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\GL/glu.h"\
	

"$(INTDIR)\nurbs.obj" : $(SOURCE) $(DEP_CPP_NURBS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\Mesa\src-glu\project.c"
DEP_CPP_PROJE=\
	"..\..\src-glu\gluP.h"\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\GL/glu.h"\
	

"$(INTDIR)\project.obj" : $(SOURCE) $(DEP_CPP_PROJE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\Mesa\src-glu\tess.c"
DEP_CPP_TESS_=\
	"..\..\src-glu\tess.h"\
	"..\..\src-glu\gluP.h"\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\GL/glu.h"\
	

"$(INTDIR)\tess.obj" : $(SOURCE) $(DEP_CPP_TESS_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
