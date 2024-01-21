# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=GLAUX - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to GLAUX - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "GLAUX - Win32 Release" && "$(CFG)" != "GLAUX - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "GLAUX.MAK" CFG="GLAUX - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "GLAUX - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "GLAUX - Win32 Debug" (based on "Win32 (x86) Static Library")
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

!IF  "$(CFG)" == "GLAUX - Win32 Release"

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

ALL : "$(OUTDIR)\GLAUX.lib"

CLEAN : 
	-@erase ".\WinRel\GLAUX.lib"
	-@erase ".\WinRel\image.obj"
	-@erase ".\WinRel\shapes.obj"
	-@erase ".\WinRel\teapot.obj"
	-@erase ".\WinRel\glaux.obj"
	-@erase ".\WinRel\vect3d.obj"
	-@erase ".\WinRel\xform.obj"
	-@erase ".\WinRel\font.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /W3 /GX /O2 /I "\wing\include" /I "..\..\include" /I "..\..\src" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "\wing\include" /I "..\..\include" /I\
 "..\..\src" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__"\
 /Fp"$(INTDIR)/GLAUX.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/GLAUX.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/GLAUX.lib" 
LIB32_OBJS= \
	"$(INTDIR)/image.obj" \
	"$(INTDIR)/shapes.obj" \
	"$(INTDIR)/teapot.obj" \
	"$(INTDIR)/glaux.obj" \
	"$(INTDIR)/vect3d.obj" \
	"$(INTDIR)/xform.obj" \
	"$(INTDIR)/font.obj"

"$(OUTDIR)\GLAUX.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "GLAUX - Win32 Debug"

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

ALL : "$(OUTDIR)\GLAUX.lib"

CLEAN : 
	-@erase ".\WinDebug\GLAUX.lib"
	-@erase ".\WinDebug\image.obj"
	-@erase ".\WinDebug\shapes.obj"
	-@erase ".\WinDebug\teapot.obj"
	-@erase ".\WinDebug\glaux.obj"
	-@erase ".\WinDebug\vect3d.obj"
	-@erase ".\WinDebug\xform.obj"
	-@erase ".\WinDebug\font.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "\wing\include" /I "..\..\include" /I "..\..\src" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MLd /W3 /GX /Z7 /Od /I "\wing\include" /I "..\..\include" /I\
 "..\..\src" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__"\
 /Fp"$(INTDIR)/GLAUX.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/GLAUX.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/GLAUX.lib" 
LIB32_OBJS= \
	"$(INTDIR)/image.obj" \
	"$(INTDIR)/shapes.obj" \
	"$(INTDIR)/teapot.obj" \
	"$(INTDIR)/glaux.obj" \
	"$(INTDIR)/vect3d.obj" \
	"$(INTDIR)/xform.obj" \
	"$(INTDIR)/font.obj"

"$(OUTDIR)\GLAUX.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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

# Name "GLAUX - Win32 Release"
# Name "GLAUX - Win32 Debug"

!IF  "$(CFG)" == "GLAUX - Win32 Release"

!ELSEIF  "$(CFG)" == "GLAUX - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE="\Mesa\src-aux\image.c"
DEP_CPP_IMAGE=\
	".\..\..\include\gltk.h"\
	".\..\..\include\glaux.h"\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\GL/glu.h"\
	

"$(INTDIR)\image.obj" : $(SOURCE) $(DEP_CPP_IMAGE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\Mesa\src-aux\shapes.c"
DEP_CPP_SHAPE=\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\GL/glu.h"\
	".\..\..\include\glaux.h"\
	"..\..\src-aux\3d.h"\
	

"$(INTDIR)\shapes.obj" : $(SOURCE) $(DEP_CPP_SHAPE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\Mesa\src-aux\teapot.c"
DEP_CPP_TEAPO=\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\glaux.h"\
	".\..\..\include\GL/glu.h"\
	

"$(INTDIR)\teapot.obj" : $(SOURCE) $(DEP_CPP_TEAPO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\Mesa\src-aux\glaux.c"
DEP_CPP_GLAUX=\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\gltk.h"\
	".\..\..\include\glaux.h"\
	".\..\..\include\GL/glu.h"\
	

"$(INTDIR)\glaux.obj" : $(SOURCE) $(DEP_CPP_GLAUX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\Mesa\src-aux\vect3d.c"
DEP_CPP_VECT3=\
	".\..\..\include\GL/gl.h"\
	"..\..\src-aux\3d.h"\
	

"$(INTDIR)\vect3d.obj" : $(SOURCE) $(DEP_CPP_VECT3) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\Mesa\src-aux\xform.c"
DEP_CPP_XFORM=\
	".\..\..\include\GL/gl.h"\
	"..\..\src-aux\3d.h"\
	

"$(INTDIR)\xform.obj" : $(SOURCE) $(DEP_CPP_XFORM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE="\Mesa\src-aux\font.c"
DEP_CPP_FONT_=\
	".\..\..\include\gltk.h"\
	".\..\..\include\glaux.h"\
	".\..\..\include\GL/gl.h"\
	".\..\..\include\GL/glu.h"\
	

"$(INTDIR)\font.obj" : $(SOURCE) $(DEP_CPP_FONT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
