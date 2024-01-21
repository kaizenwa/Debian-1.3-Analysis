# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=WINMESA - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to WINMESA - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "WINMESA - Win32 Release" && "$(CFG)" !=\
 "WINMESA - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "Winmesa.mak" CFG="WINMESA - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "WINMESA - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "WINMESA - Win32 Debug" (based on "Win32 (x86) Static Library")
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
# PROP Target_Last_Scanned "WINMESA - Win32 Debug"
CPP=cl.exe

!IF  "$(CFG)" == "WINMESA - Win32 Release"

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

ALL : "$(OUTDIR)\Winmesa.lib"

CLEAN : 
	-@erase ".\WinRel\Winmesa.lib"
	-@erase ".\WinRel\feedback.obj"
	-@erase ".\WinRel\triangle.obj"
	-@erase ".\WinRel\teximage.obj"
	-@erase ".\WinRel\pointers.obj"
	-@erase ".\WinRel\get.obj"
	-@erase ".\WinRel\context.obj"
	-@erase ".\WinRel\texobj.obj"
	-@erase ".\WinRel\depth.obj"
	-@erase ".\WinRel\span.obj"
	-@erase ".\WinRel\enable.obj"
	-@erase ".\WinRel\alphabuf.obj"
	-@erase ".\WinRel\scissor.obj"
	-@erase ".\WinRel\masking.obj"
	-@erase ".\WinRel\copypix.obj"
	-@erase ".\WinRel\polygon.obj"
	-@erase ".\WinRel\pixel.obj"
	-@erase ".\WinRel\bitmap.obj"
	-@erase ".\WinRel\xform.obj"
	-@erase ".\WinRel\winpos.obj"
	-@erase ".\WinRel\osmesa.obj"
	-@erase ".\WinRel\accum.obj"
	-@erase ".\WinRel\wmesa.obj"
	-@erase ".\WinRel\draw.obj"
	-@erase ".\WinRel\points.obj"
	-@erase ".\WinRel\fog.obj"
	-@erase ".\WinRel\drawpix.obj"
	-@erase ".\WinRel\readpix.obj"
	-@erase ".\WinRel\bresenhm.obj"
	-@erase ".\WinRel\blend.obj"
	-@erase ".\WinRel\stencil.obj"
	-@erase ".\WinRel\dlist.obj"
	-@erase ".\WinRel\attrib.obj"
	-@erase ".\WinRel\misc.obj"
	-@erase ".\WinRel\vb.obj"
	-@erase ".\WinRel\vertex.obj"
	-@erase ".\WinRel\varray.obj"
	-@erase ".\WinRel\clip.obj"
	-@erase ".\WinRel\light.obj"
	-@erase ".\WinRel\pb.obj"
	-@erase ".\WinRel\matrix.obj"
	-@erase ".\WinRel\interp.obj"
	-@erase ".\WinRel\alpha.obj"
	-@erase ".\WinRel\image.obj"
	-@erase ".\WinRel\eval.obj"
	-@erase ".\WinRel\texture.obj"
	-@erase ".\WinRel\logic.obj"
	-@erase ".\WinRel\svgamesa.obj"
	-@erase ".\WinRel\lines.obj"
	-@erase ".\WinRel\api.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /W3 /GX /I "..\..\include" /I "\wing\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /ML /W3 /GX /I "..\..\include" /I "\wing\include" /D "NDEBUG"\
 /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /Fp"$(INTDIR)/Winmesa.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Winmesa.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/Winmesa.lib" 
LIB32_OBJS= \
	".\WinRel\feedback.obj" \
	".\WinRel\triangle.obj" \
	".\WinRel\teximage.obj" \
	".\WinRel\pointers.obj" \
	".\WinRel\get.obj" \
	".\WinRel\context.obj" \
	".\WinRel\texobj.obj" \
	".\WinRel\depth.obj" \
	".\WinRel\span.obj" \
	".\WinRel\enable.obj" \
	".\WinRel\alphabuf.obj" \
	".\WinRel\scissor.obj" \
	".\WinRel\masking.obj" \
	".\WinRel\copypix.obj" \
	".\WinRel\polygon.obj" \
	".\WinRel\pixel.obj" \
	".\WinRel\bitmap.obj" \
	".\WinRel\xform.obj" \
	".\WinRel\winpos.obj" \
	".\WinRel\osmesa.obj" \
	".\WinRel\accum.obj" \
	".\WinRel\wmesa.obj" \
	".\WinRel\draw.obj" \
	".\WinRel\points.obj" \
	".\WinRel\fog.obj" \
	".\WinRel\drawpix.obj" \
	".\WinRel\readpix.obj" \
	".\WinRel\bresenhm.obj" \
	".\WinRel\blend.obj" \
	".\WinRel\stencil.obj" \
	".\WinRel\dlist.obj" \
	".\WinRel\attrib.obj" \
	".\WinRel\misc.obj" \
	".\WinRel\vb.obj" \
	".\WinRel\vertex.obj" \
	".\WinRel\varray.obj" \
	".\WinRel\clip.obj" \
	".\WinRel\light.obj" \
	".\WinRel\pb.obj" \
	".\WinRel\matrix.obj" \
	".\WinRel\interp.obj" \
	".\WinRel\alpha.obj" \
	".\WinRel\image.obj" \
	".\WinRel\eval.obj" \
	".\WinRel\texture.obj" \
	".\WinRel\logic.obj" \
	".\WinRel\svgamesa.obj" \
	".\WinRel\lines.obj" \
	".\WinRel\api.obj"

"$(OUTDIR)\Winmesa.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"

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

ALL : "$(OUTDIR)\Winmesa.lib" "$(OUTDIR)\Winmesa.bsc"

CLEAN : 
	-@erase ".\WinDebug\Winmesa.bsc"
	-@erase ".\WinDebug\span.sbr"
	-@erase ".\WinDebug\pixel.sbr"
	-@erase ".\WinDebug\xform.sbr"
	-@erase ".\WinDebug\alpha.sbr"
	-@erase ".\WinDebug\accum.sbr"
	-@erase ".\WinDebug\alphabuf.sbr"
	-@erase ".\WinDebug\image.sbr"
	-@erase ".\WinDebug\context.sbr"
	-@erase ".\WinDebug\fog.sbr"
	-@erase ".\WinDebug\logic.sbr"
	-@erase ".\WinDebug\dlist.sbr"
	-@erase ".\WinDebug\scissor.sbr"
	-@erase ".\WinDebug\masking.sbr"
	-@erase ".\WinDebug\api.sbr"
	-@erase ".\WinDebug\copypix.sbr"
	-@erase ".\WinDebug\polygon.sbr"
	-@erase ".\WinDebug\svgamesa.sbr"
	-@erase ".\WinDebug\enable.sbr"
	-@erase ".\WinDebug\feedback.sbr"
	-@erase ".\WinDebug\light.sbr"
	-@erase ".\WinDebug\drawpix.sbr"
	-@erase ".\WinDebug\readpix.sbr"
	-@erase ".\WinDebug\depth.sbr"
	-@erase ".\WinDebug\triangle.sbr"
	-@erase ".\WinDebug\bitmap.sbr"
	-@erase ".\WinDebug\winpos.sbr"
	-@erase ".\WinDebug\osmesa.sbr"
	-@erase ".\WinDebug\stencil.sbr"
	-@erase ".\WinDebug\pointers.sbr"
	-@erase ".\WinDebug\pb.sbr"
	-@erase ".\WinDebug\clip.sbr"
	-@erase ".\WinDebug\points.sbr"
	-@erase ".\WinDebug\lines.sbr"
	-@erase ".\WinDebug\attrib.sbr"
	-@erase ".\WinDebug\draw.sbr"
	-@erase ".\WinDebug\eval.sbr"
	-@erase ".\WinDebug\blend.sbr"
	-@erase ".\WinDebug\vertex.sbr"
	-@erase ".\WinDebug\varray.sbr"
	-@erase ".\WinDebug\vb.sbr"
	-@erase ".\WinDebug\misc.sbr"
	-@erase ".\WinDebug\texobj.sbr"
	-@erase ".\WinDebug\get.sbr"
	-@erase ".\WinDebug\matrix.sbr"
	-@erase ".\WinDebug\bresenhm.sbr"
	-@erase ".\WinDebug\texture.sbr"
	-@erase ".\WinDebug\interp.sbr"
	-@erase ".\WinDebug\teximage.sbr"
	-@erase ".\WinDebug\Winmesa.lib"
	-@erase ".\WinDebug\blend.obj"
	-@erase ".\WinDebug\vertex.obj"
	-@erase ".\WinDebug\varray.obj"
	-@erase ".\WinDebug\vb.obj"
	-@erase ".\WinDebug\misc.obj"
	-@erase ".\WinDebug\texobj.obj"
	-@erase ".\WinDebug\get.obj"
	-@erase ".\WinDebug\matrix.obj"
	-@erase ".\WinDebug\bresenhm.obj"
	-@erase ".\WinDebug\texture.obj"
	-@erase ".\WinDebug\interp.obj"
	-@erase ".\WinDebug\teximage.obj"
	-@erase ".\WinDebug\span.obj"
	-@erase ".\WinDebug\pixel.obj"
	-@erase ".\WinDebug\xform.obj"
	-@erase ".\WinDebug\alpha.obj"
	-@erase ".\WinDebug\accum.obj"
	-@erase ".\WinDebug\alphabuf.obj"
	-@erase ".\WinDebug\image.obj"
	-@erase ".\WinDebug\context.obj"
	-@erase ".\WinDebug\fog.obj"
	-@erase ".\WinDebug\logic.obj"
	-@erase ".\WinDebug\dlist.obj"
	-@erase ".\WinDebug\scissor.obj"
	-@erase ".\WinDebug\masking.obj"
	-@erase ".\WinDebug\api.obj"
	-@erase ".\WinDebug\copypix.obj"
	-@erase ".\WinDebug\polygon.obj"
	-@erase ".\WinDebug\svgamesa.obj"
	-@erase ".\WinDebug\enable.obj"
	-@erase ".\WinDebug\feedback.obj"
	-@erase ".\WinDebug\light.obj"
	-@erase ".\WinDebug\drawpix.obj"
	-@erase ".\WinDebug\readpix.obj"
	-@erase ".\WinDebug\depth.obj"
	-@erase ".\WinDebug\triangle.obj"
	-@erase ".\WinDebug\bitmap.obj"
	-@erase ".\WinDebug\winpos.obj"
	-@erase ".\WinDebug\osmesa.obj"
	-@erase ".\WinDebug\stencil.obj"
	-@erase ".\WinDebug\pointers.obj"
	-@erase ".\WinDebug\pb.obj"
	-@erase ".\WinDebug\clip.obj"
	-@erase ".\WinDebug\points.obj"
	-@erase ".\WinDebug\lines.obj"
	-@erase ".\WinDebug\attrib.obj"
	-@erase ".\WinDebug\draw.obj"
	-@erase ".\WinDebug\eval.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /W3 /GX /Z7 /I "..\..\include" /I "\wing\include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /Fr /YX /c
CPP_PROJ=/nologo /MLd /W3 /GX /Z7 /I "..\..\include" /I "\wing\include" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /Fr"$(INTDIR)/"\
 /Fp"$(INTDIR)/Winmesa.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.\WinDebug/
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Winmesa.bsc" 
BSC32_SBRS= \
	".\WinDebug\span.sbr" \
	".\WinDebug\pixel.sbr" \
	".\WinDebug\xform.sbr" \
	".\WinDebug\alpha.sbr" \
	".\WinDebug\accum.sbr" \
	".\WinDebug\alphabuf.sbr" \
	".\WinDebug\image.sbr" \
	".\WinDebug\context.sbr" \
	".\WinDebug\fog.sbr" \
	".\WinDebug\logic.sbr" \
	".\WinDebug\dlist.sbr" \
	".\WinDebug\scissor.sbr" \
	".\WinDebug\masking.sbr" \
	".\WinDebug\api.sbr" \
	".\WinDebug\copypix.sbr" \
	".\WinDebug\polygon.sbr" \
	".\WinDebug\svgamesa.sbr" \
	".\WinDebug\enable.sbr" \
	".\WinDebug\feedback.sbr" \
	".\WinDebug\light.sbr" \
	".\WinDebug\drawpix.sbr" \
	".\WinDebug\readpix.sbr" \
	".\WinDebug\depth.sbr" \
	".\WinDebug\triangle.sbr" \
	".\WinDebug\bitmap.sbr" \
	".\WinDebug\winpos.sbr" \
	".\WinDebug\osmesa.sbr" \
	".\WinDebug\stencil.sbr" \
	".\WinDebug\pointers.sbr" \
	".\WinDebug\pb.sbr" \
	".\WinDebug\clip.sbr" \
	".\WinDebug\points.sbr" \
	".\WinDebug\lines.sbr" \
	".\WinDebug\attrib.sbr" \
	".\WinDebug\draw.sbr" \
	".\WinDebug\eval.sbr" \
	".\WinDebug\blend.sbr" \
	".\WinDebug\vertex.sbr" \
	".\WinDebug\varray.sbr" \
	".\WinDebug\vb.sbr" \
	".\WinDebug\misc.sbr" \
	".\WinDebug\texobj.sbr" \
	".\WinDebug\get.sbr" \
	".\WinDebug\matrix.sbr" \
	".\WinDebug\bresenhm.sbr" \
	".\WinDebug\texture.sbr" \
	".\WinDebug\interp.sbr" \
	".\WinDebug\teximage.sbr"

"$(OUTDIR)\Winmesa.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/Winmesa.lib" 
LIB32_OBJS= \
	".\WinDebug\blend.obj" \
	".\WinDebug\vertex.obj" \
	".\WinDebug\varray.obj" \
	".\WinDebug\vb.obj" \
	".\WinDebug\misc.obj" \
	".\WinDebug\texobj.obj" \
	".\WinDebug\get.obj" \
	".\WinDebug\matrix.obj" \
	".\WinDebug\bresenhm.obj" \
	".\WinDebug\texture.obj" \
	".\WinDebug\interp.obj" \
	".\WinDebug\teximage.obj" \
	".\WinDebug\span.obj" \
	".\WinDebug\pixel.obj" \
	".\WinDebug\xform.obj" \
	".\WinDebug\alpha.obj" \
	".\WinDebug\accum.obj" \
	".\WinDebug\alphabuf.obj" \
	".\WinDebug\image.obj" \
	".\WinDebug\context.obj" \
	".\WinDebug\fog.obj" \
	".\WinDebug\logic.obj" \
	".\WinDebug\dlist.obj" \
	".\WinDebug\scissor.obj" \
	".\WinDebug\masking.obj" \
	".\WinDebug\api.obj" \
	".\WinDebug\copypix.obj" \
	".\WinDebug\polygon.obj" \
	".\WinDebug\svgamesa.obj" \
	".\WinDebug\enable.obj" \
	".\WinDebug\feedback.obj" \
	".\WinDebug\light.obj" \
	".\WinDebug\drawpix.obj" \
	".\WinDebug\readpix.obj" \
	".\WinDebug\depth.obj" \
	".\WinDebug\triangle.obj" \
	".\WinDebug\bitmap.obj" \
	".\WinDebug\winpos.obj" \
	".\WinDebug\osmesa.obj" \
	".\WinDebug\stencil.obj" \
	".\WinDebug\pointers.obj" \
	".\WinDebug\pb.obj" \
	".\WinDebug\clip.obj" \
	".\WinDebug\points.obj" \
	".\WinDebug\lines.obj" \
	".\WinDebug\attrib.obj" \
	".\WinDebug\draw.obj" \
	".\WinDebug\eval.obj"

"$(OUTDIR)\Winmesa.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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

# Name "WINMESA - Win32 Release"
# Name "WINMESA - Win32 Debug"

!IF  "$(CFG)" == "WINMESA - Win32 Release"

!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=\Mesa\src\span.c
DEP_CPP_SPAN_=\
	".\..\..\src\alpha.h"\
	".\..\..\src\alphabuf.h"\
	".\..\..\src\blend.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\fog.h"\
	".\..\..\src\logic.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\masking.h"\
	".\..\..\src\scissor.h"\
	".\..\..\src\span.h"\
	".\..\..\src\stencil.h"\
	".\..\..\src\texture.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\span.obj" : $(SOURCE) $(DEP_CPP_SPAN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\span.obj" : $(SOURCE) $(DEP_CPP_SPAN_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\span.sbr" : $(SOURCE) $(DEP_CPP_SPAN_) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\scissor.c
DEP_CPP_SCISS=\
	".\..\..\src\context.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\scissor.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\scissor.obj" : $(SOURCE) $(DEP_CPP_SCISS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\scissor.obj" : $(SOURCE) $(DEP_CPP_SCISS) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\scissor.sbr" : $(SOURCE) $(DEP_CPP_SCISS) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\light.c
DEP_CPP_LIGHT=\
	".\..\..\src\context.h"\
	".\..\..\src\light.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\matrix.h"\
	".\..\..\src\vb.h"\
	".\..\..\src\xform.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\light.obj" : $(SOURCE) $(DEP_CPP_LIGHT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\light.obj" : $(SOURCE) $(DEP_CPP_LIGHT) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\light.sbr" : $(SOURCE) $(DEP_CPP_LIGHT) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\stencil.c
DEP_CPP_STENC=\
	".\..\..\src\context.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pb.h"\
	".\..\..\src\stencil.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\stencil.obj" : $(SOURCE) $(DEP_CPP_STENC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\stencil.obj" : $(SOURCE) $(DEP_CPP_STENC) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\stencil.sbr" : $(SOURCE) $(DEP_CPP_STENC) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\points.c
DEP_CPP_POINT=\
	".\..\..\src\context.h"\
	".\..\..\src\feedback.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pb.h"\
	".\..\..\src\span.h"\
	".\..\..\src\vb.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\points.obj" : $(SOURCE) $(DEP_CPP_POINT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\points.obj" : $(SOURCE) $(DEP_CPP_POINT) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\points.sbr" : $(SOURCE) $(DEP_CPP_POINT) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\draw.c
DEP_CPP_DRAW_=\
	".\..\..\src\clip.h"\
	".\..\..\src\context.h"\
	".\..\..\src\draw.h"\
	".\..\..\src\feedback.h"\
	".\..\..\src\fog.h"\
	".\..\..\src\light.h"\
	".\..\..\src\lines.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\matrix.h"\
	".\..\..\src\pb.h"\
	".\..\..\src\points.h"\
	".\..\..\src\texture.h"\
	".\..\..\src\vb.h"\
	".\..\..\src\xform.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\draw.obj" : $(SOURCE) $(DEP_CPP_DRAW_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\draw.obj" : $(SOURCE) $(DEP_CPP_DRAW_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\draw.sbr" : $(SOURCE) $(DEP_CPP_DRAW_) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\alpha.c
DEP_CPP_ALPHA=\
	".\..\..\src\alpha.h"\
	".\..\..\src\context.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\alpha.obj" : $(SOURCE) $(DEP_CPP_ALPHA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\alpha.obj" : $(SOURCE) $(DEP_CPP_ALPHA) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\alpha.sbr" : $(SOURCE) $(DEP_CPP_ALPHA) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\drawpix.c
DEP_CPP_DRAWP=\
	".\..\..\src\context.h"\
	".\..\..\src\drawpix.h"\
	".\..\..\src\feedback.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pixel.h"\
	".\..\..\src\span.h"\
	".\..\..\src\stencil.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\drawpix.obj" : $(SOURCE) $(DEP_CPP_DRAWP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\drawpix.obj" : $(SOURCE) $(DEP_CPP_DRAWP) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\drawpix.sbr" : $(SOURCE) $(DEP_CPP_DRAWP) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\lines.c
DEP_CPP_LINES=\
	".\..\..\src\bresenhm.h"\
	".\..\..\src\context.h"\
	".\..\..\src\feedback.h"\
	".\..\..\src\interp.h"\
	".\..\..\src\lines.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pb.h"\
	".\..\..\src\vb.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\lines.obj" : $(SOURCE) $(DEP_CPP_LINES) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\lines.obj" : $(SOURCE) $(DEP_CPP_LINES) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\lines.sbr" : $(SOURCE) $(DEP_CPP_LINES) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\attrib.c
DEP_CPP_ATTRI=\
	".\..\..\src\attrib.h"\
	".\..\..\src\context.h"\
	".\..\..\src\draw.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\misc.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\attrib.obj" : $(SOURCE) $(DEP_CPP_ATTRI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\attrib.obj" : $(SOURCE) $(DEP_CPP_ATTRI) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\attrib.sbr" : $(SOURCE) $(DEP_CPP_ATTRI) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\readpix.c
DEP_CPP_READP=\
	".\..\..\src\alphabuf.h"\
	".\..\..\src\context.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\feedback.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\image.h"\
	".\..\..\src\readpix.h"\
	".\..\..\src\span.h"\
	".\..\..\src\stencil.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\readpix.obj" : $(SOURCE) $(DEP_CPP_READP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\readpix.obj" : $(SOURCE) $(DEP_CPP_READP) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\readpix.sbr" : $(SOURCE) $(DEP_CPP_READP) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\pixel.c
DEP_CPP_PIXEL=\
	".\..\..\src\context.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pixel.h"\
	".\..\..\src\image.h"\
	".\..\..\src\span.h"\
	".\..\..\src\stencil.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\pixel.obj" : $(SOURCE) $(DEP_CPP_PIXEL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\pixel.obj" : $(SOURCE) $(DEP_CPP_PIXEL) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\pixel.sbr" : $(SOURCE) $(DEP_CPP_PIXEL) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\misc.c
DEP_CPP_MISC_=\
	".\..\..\src\accum.h"\
	".\..\..\src\alphabuf.h"\
	".\..\..\src\context.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\masking.h"\
	".\..\..\src\misc.h"\
	".\..\..\src\stencil.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\misc.sbr" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\accum.c
DEP_CPP_ACCUM=\
	".\..\..\src\accum.h"\
	".\..\..\src\context.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\accum.obj" : $(SOURCE) $(DEP_CPP_ACCUM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\accum.obj" : $(SOURCE) $(DEP_CPP_ACCUM) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\accum.sbr" : $(SOURCE) $(DEP_CPP_ACCUM) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\pb.c
DEP_CPP_PB_C1c=\
	".\..\..\src\alpha.h"\
	".\..\..\src\alphabuf.h"\
	".\..\..\src\blend.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\fog.h"\
	".\..\..\src\logic.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\masking.h"\
	".\..\..\src\pb.h"\
	".\..\..\src\scissor.h"\
	".\..\..\src\stencil.h"\
	".\..\..\src\texture.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\pb.obj" : $(SOURCE) $(DEP_CPP_PB_C1c) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\pb.obj" : $(SOURCE) $(DEP_CPP_PB_C1c) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\pb.sbr" : $(SOURCE) $(DEP_CPP_PB_C1c) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\bitmap.c
DEP_CPP_BITMA=\
	".\..\..\src\bitmap.h"\
	".\..\..\src\context.h"\
	".\..\..\src\feedback.h"\
	".\..\..\src\image.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pb.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\bitmap.obj" : $(SOURCE) $(DEP_CPP_BITMA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\bitmap.obj" : $(SOURCE) $(DEP_CPP_BITMA) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\bitmap.sbr" : $(SOURCE) $(DEP_CPP_BITMA) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\context.c
DEP_CPP_CONTE=\
	".\..\..\src\context.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\draw.h"\
	".\..\..\src\eval.h"\
	".\..\..\src\light.h"\
	".\..\..\src\lines.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pb.h"\
	".\..\..\src\points.h"\
	".\..\..\src\pointers.h"\
	".\..\..\src\triangle.h"\
	".\..\..\src\teximage.h"\
	".\..\..\src\texobj.h"\
	".\..\..\src\texture.h"\
	".\..\..\src\vb.h"\
	".\..\..\src\vertex.h"\
	{$(INCLUDE)}"\Sys\Types.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\context.obj" : $(SOURCE) $(DEP_CPP_CONTE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\context.obj" : $(SOURCE) $(DEP_CPP_CONTE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\context.sbr" : $(SOURCE) $(DEP_CPP_CONTE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\vertex.c
DEP_CPP_VERTE=\
	".\..\..\src\draw.h"\
	".\..\..\src\light.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\vb.h"\
	".\..\..\src\vertex.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\vertex.obj" : $(SOURCE) $(DEP_CPP_VERTE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\vertex.obj" : $(SOURCE) $(DEP_CPP_VERTE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\vertex.sbr" : $(SOURCE) $(DEP_CPP_VERTE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\copypix.c
DEP_CPP_COPYP=\
	".\..\..\src\context.h"\
	".\..\..\src\copypix.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\feedback.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pixel.h"\
	".\..\..\src\span.h"\
	".\..\..\src\stencil.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\copypix.obj" : $(SOURCE) $(DEP_CPP_COPYP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\copypix.obj" : $(SOURCE) $(DEP_CPP_COPYP) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\copypix.sbr" : $(SOURCE) $(DEP_CPP_COPYP) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\logic.c
DEP_CPP_LOGIC=\
	".\..\..\src\context.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\logic.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pb.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\logic.obj" : $(SOURCE) $(DEP_CPP_LOGIC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\logic.obj" : $(SOURCE) $(DEP_CPP_LOGIC) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\logic.sbr" : $(SOURCE) $(DEP_CPP_LOGIC) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\interp.c
DEP_CPP_INTER=\
	".\..\..\src\interp.h"\
	".\..\..\src\macros.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\interp.obj" : $(SOURCE) $(DEP_CPP_INTER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\interp.obj" : $(SOURCE) $(DEP_CPP_INTER) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\interp.sbr" : $(SOURCE) $(DEP_CPP_INTER) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\blend.c
DEP_CPP_BLEND=\
	".\..\..\src\alphabuf.h"\
	".\..\..\src\blend.h"\
	".\..\..\src\context.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pb.h"\
	".\..\..\src\span.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\blend.obj" : $(SOURCE) $(DEP_CPP_BLEND) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\blend.obj" : $(SOURCE) $(DEP_CPP_BLEND) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\blend.sbr" : $(SOURCE) $(DEP_CPP_BLEND) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\fog.c
DEP_CPP_FOG_C=\
	".\..\..\src\context.h"\
	".\..\..\src\fog.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\fog.obj" : $(SOURCE) $(DEP_CPP_FOG_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\fog.obj" : $(SOURCE) $(DEP_CPP_FOG_C) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\fog.sbr" : $(SOURCE) $(DEP_CPP_FOG_C) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\enable.c
DEP_CPP_ENABL=\
	".\..\..\src\context.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\draw.h"\
	".\..\..\src\enable.h"\
	".\..\..\src\light.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\stencil.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\enable.obj" : $(SOURCE) $(DEP_CPP_ENABL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\enable.obj" : $(SOURCE) $(DEP_CPP_ENABL) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\enable.sbr" : $(SOURCE) $(DEP_CPP_ENABL) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\get.c
DEP_CPP_GET_C=\
	".\..\..\src\context.h"\
	".\..\..\src\get.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\get.obj" : $(SOURCE) $(DEP_CPP_GET_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\get.obj" : $(SOURCE) $(DEP_CPP_GET_C) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\get.sbr" : $(SOURCE) $(DEP_CPP_GET_C) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\texture.c
DEP_CPP_TEXTU=\
	".\..\..\src\context.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pb.h"\
	".\..\..\src\teximage.h"\
	".\..\..\src\texture.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\texture.obj" : $(SOURCE) $(DEP_CPP_TEXTU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\texture.obj" : $(SOURCE) $(DEP_CPP_TEXTU) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\texture.sbr" : $(SOURCE) $(DEP_CPP_TEXTU) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\clip.c
DEP_CPP_CLIP_=\
	".\..\..\src\clip.h"\
	".\..\..\src\context.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\matrix.h"\
	".\..\..\src\vb.h"\
	".\..\..\src\xform.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\clip.obj" : $(SOURCE) $(DEP_CPP_CLIP_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\clip.obj" : $(SOURCE) $(DEP_CPP_CLIP_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\clip.sbr" : $(SOURCE) $(DEP_CPP_CLIP_) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\bresenhm.c
DEP_CPP_BRESE=\
	".\..\..\src\bresenhm.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\bresenhm.obj" : $(SOURCE) $(DEP_CPP_BRESE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\bresenhm.obj" : $(SOURCE) $(DEP_CPP_BRESE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\bresenhm.sbr" : $(SOURCE) $(DEP_CPP_BRESE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\depth.c
DEP_CPP_DEPTH=\
	".\..\..\src\context.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\depth.obj" : $(SOURCE) $(DEP_CPP_DEPTH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\depth.obj" : $(SOURCE) $(DEP_CPP_DEPTH) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\depth.sbr" : $(SOURCE) $(DEP_CPP_DEPTH) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\feedback.c
DEP_CPP_FEEDB=\
	".\..\..\src\context.h"\
	".\..\..\src\feedback.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\feedback.obj" : $(SOURCE) $(DEP_CPP_FEEDB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\feedback.obj" : $(SOURCE) $(DEP_CPP_FEEDB) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\feedback.sbr" : $(SOURCE) $(DEP_CPP_FEEDB) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\xform.c
DEP_CPP_XFORM=\
	".\..\..\src\xform.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\xform.obj" : $(SOURCE) $(DEP_CPP_XFORM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\xform.obj" : $(SOURCE) $(DEP_CPP_XFORM) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\xform.sbr" : $(SOURCE) $(DEP_CPP_XFORM) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\vb.c
DEP_CPP_VB_C3e=\
	".\..\..\src\vb.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\vb.obj" : $(SOURCE) $(DEP_CPP_VB_C3e) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\vb.obj" : $(SOURCE) $(DEP_CPP_VB_C3e) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\vb.sbr" : $(SOURCE) $(DEP_CPP_VB_C3e) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\alphabuf.c
DEP_CPP_ALPHAB=\
	".\..\..\src\alphabuf.h"\
	".\..\..\src\context.h"\
	".\..\..\src\macros.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\alphabuf.obj" : $(SOURCE) $(DEP_CPP_ALPHAB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\alphabuf.obj" : $(SOURCE) $(DEP_CPP_ALPHAB) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\alphabuf.sbr" : $(SOURCE) $(DEP_CPP_ALPHAB) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\Mesa\src\masking.c
DEP_CPP_MASKI=\
	".\..\..\src\alphabuf.h"\
	".\..\..\src\context.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\masking.h"\
	".\..\..\src\pb.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\masking.obj" : $(SOURCE) $(DEP_CPP_MASKI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\masking.obj" : $(SOURCE) $(DEP_CPP_MASKI) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\masking.sbr" : $(SOURCE) $(DEP_CPP_MASKI) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\polygon.c

!IF  "$(CFG)" == "WINMESA - Win32 Release"

DEP_CPP_POLYG=\
	".\..\..\src\context.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\polygon.h"\
	
NODEP_CPP_POLYG=\
	".\..\..\src\Paul"\
	

"$(INTDIR)\polygon.obj" : $(SOURCE) $(DEP_CPP_POLYG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"

DEP_CPP_POLYG=\
	".\..\..\src\context.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\polygon.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\polygon.obj" : $(SOURCE) $(DEP_CPP_POLYG) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\polygon.sbr" : $(SOURCE) $(DEP_CPP_POLYG) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\api.c
DEP_CPP_API_C=\
	".\..\..\src\bitmap.h"\
	".\..\..\src\context.h"\
	".\..\..\src\eval.h"\
	".\..\..\src\image.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\matrix.h"\
	".\..\..\src\teximage.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\api.obj" : $(SOURCE) $(DEP_CPP_API_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\api.obj" : $(SOURCE) $(DEP_CPP_API_C) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\api.sbr" : $(SOURCE) $(DEP_CPP_API_C) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\winpos.c
DEP_CPP_WINPO=\
	".\..\..\include\GL/gl.h"\
	".\..\..\src\draw.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\winpos.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\winpos.obj" : $(SOURCE) $(DEP_CPP_WINPO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\winpos.obj" : $(SOURCE) $(DEP_CPP_WINPO) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\winpos.sbr" : $(SOURCE) $(DEP_CPP_WINPO) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\dlist.c
DEP_CPP_DLIST=\
	".\..\..\src\accum.h"\
	".\..\..\src\alpha.h"\
	".\..\..\src\attrib.h"\
	".\..\..\src\bitmap.h"\
	".\..\..\src\blend.h"\
	".\..\..\src\clip.h"\
	".\..\..\src\context.h"\
	".\..\..\src\copypix.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\draw.h"\
	".\..\..\src\drawpix.h"\
	".\..\..\src\enable.h"\
	".\..\..\src\eval.h"\
	".\..\..\src\feedback.h"\
	".\..\..\src\fog.h"\
	".\..\..\src\image.h"\
	".\..\..\src\light.h"\
	".\..\..\src\lines.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\logic.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\masking.h"\
	".\..\..\src\matrix.h"\
	".\..\..\src\misc.h"\
	".\..\..\src\pixel.h"\
	".\..\..\src\points.h"\
	".\..\..\src\polygon.h"\
	".\..\..\src\scissor.h"\
	".\..\..\src\stencil.h"\
	".\..\..\src\texobj.h"\
	".\..\..\src\teximage.h"\
	".\..\..\src\texture.h"\
	".\..\..\src\vb.h"\
	".\..\..\src\vertex.h"\
	".\..\..\src\winpos.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\dlist.obj" : $(SOURCE) $(DEP_CPP_DLIST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\dlist.obj" : $(SOURCE) $(DEP_CPP_DLIST) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\dlist.sbr" : $(SOURCE) $(DEP_CPP_DLIST) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\eval.c
DEP_CPP_EVAL_=\
	".\..\..\src\context.h"\
	".\..\..\src\draw.h"\
	".\..\..\src\eval.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\eval.obj" : $(SOURCE) $(DEP_CPP_EVAL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\eval.obj" : $(SOURCE) $(DEP_CPP_EVAL_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\eval.sbr" : $(SOURCE) $(DEP_CPP_EVAL_) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\image.c
DEP_CPP_IMAGE=\
	".\..\..\src\image.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pixel.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\image.obj" : $(SOURCE) $(DEP_CPP_IMAGE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\image.obj" : $(SOURCE) $(DEP_CPP_IMAGE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\image.sbr" : $(SOURCE) $(DEP_CPP_IMAGE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\matrix.c
DEP_CPP_MATRI=\
	".\..\..\src\accum.h"\
	".\..\..\src\alphabuf.h"\
	".\..\..\src\context.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\matrix.h"\
	".\..\..\src\stencil.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\matrix.obj" : $(SOURCE) $(DEP_CPP_MATRI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\matrix.obj" : $(SOURCE) $(DEP_CPP_MATRI) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\matrix.sbr" : $(SOURCE) $(DEP_CPP_MATRI) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\osmesa.c
DEP_CPP_OSMES=\
	".\..\..\include\GL/osmesa.h"\
	".\..\..\src\context.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\matrix.h"\
	".\..\..\src\vb.h"\
	".\..\..\src\tritemp.h"\
	".\..\..\include\GL/gl.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\osmesa.obj" : $(SOURCE) $(DEP_CPP_OSMES) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\osmesa.obj" : $(SOURCE) $(DEP_CPP_OSMES) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\osmesa.sbr" : $(SOURCE) $(DEP_CPP_OSMES) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\pointers.c
DEP_CPP_POINTE=\
	".\..\..\src\accum.h"\
	".\..\..\src\alpha.h"\
	".\..\..\src\attrib.h"\
	".\..\..\src\bitmap.h"\
	".\..\..\src\blend.h"\
	".\..\..\src\clip.h"\
	".\..\..\src\context.h"\
	".\..\..\src\copypix.h"\
	".\..\..\src\depth.h"\
	".\..\..\src\draw.h"\
	".\..\..\src\drawpix.h"\
	".\..\..\src\enable.h"\
	".\..\..\src\eval.h"\
	".\..\..\src\feedback.h"\
	".\..\..\src\fog.h"\
	".\..\..\src\get.h"\
	".\..\..\src\light.h"\
	".\..\..\src\lines.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\logic.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\masking.h"\
	".\..\..\src\matrix.h"\
	".\..\..\src\misc.h"\
	".\..\..\src\pixel.h"\
	".\..\..\src\points.h"\
	".\..\..\src\polygon.h"\
	".\..\..\src\readpix.h"\
	".\..\..\src\scissor.h"\
	".\..\..\src\stencil.h"\
	".\..\..\src\teximage.h"\
	".\..\..\src\texobj.h"\
	".\..\..\src\texture.h"\
	".\..\..\src\varray.h"\
	".\..\..\src\vb.h"\
	".\..\..\src\vertex.h"\
	".\..\..\src\winpos.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\pointers.obj" : $(SOURCE) $(DEP_CPP_POINTE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\pointers.obj" : $(SOURCE) $(DEP_CPP_POINTE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\pointers.sbr" : $(SOURCE) $(DEP_CPP_POINTE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\svgamesa.c
DEP_CPP_SVGAM=\
	".\..\..\include\GL/svgamesa.h"\
	".\..\..\src\context.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\svgamesa.obj" : $(SOURCE) $(DEP_CPP_SVGAM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\svgamesa.obj" : $(SOURCE) $(DEP_CPP_SVGAM) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\svgamesa.sbr" : $(SOURCE) $(DEP_CPP_SVGAM) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\teximage.c
DEP_CPP_TEXIM=\
	".\..\..\src\context.h"\
	".\..\..\src\image.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\pixel.h"\
	".\..\..\src\span.h"\
	".\..\..\src\teximage.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\teximage.obj" : $(SOURCE) $(DEP_CPP_TEXIM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\teximage.obj" : $(SOURCE) $(DEP_CPP_TEXIM) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\teximage.sbr" : $(SOURCE) $(DEP_CPP_TEXIM) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\texobj.c
DEP_CPP_TEXOB=\
	".\..\..\src\context.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\teximage.h"\
	".\..\..\src\texobj.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\texobj.obj" : $(SOURCE) $(DEP_CPP_TEXOB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\texobj.obj" : $(SOURCE) $(DEP_CPP_TEXOB) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\texobj.sbr" : $(SOURCE) $(DEP_CPP_TEXOB) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\triangle.c
DEP_CPP_TRIAN=\
	".\..\..\src\depth.h"\
	".\..\..\src\feedback.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\span.h"\
	".\..\..\src\triangle.h"\
	".\..\..\src\vb.h"\
	".\..\..\src\tritemp.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\triangle.obj" : $(SOURCE) $(DEP_CPP_TRIAN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\triangle.obj" : $(SOURCE) $(DEP_CPP_TRIAN) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\triangle.sbr" : $(SOURCE) $(DEP_CPP_TRIAN) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\src\varray.c
DEP_CPP_VARRA=\
	".\..\..\src\draw.h"\
	".\..\..\src\context.h"\
	".\..\..\src\enable.h"\
	".\..\..\src\dlist.h"\
	".\..\..\src\light.h"\
	".\..\..\src\macros.h"\
	".\..\..\src\varray.h"\
	".\..\..\src\vb.h"\
	".\..\..\src\xform.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\varray.obj" : $(SOURCE) $(DEP_CPP_VARRA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"


BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\varray.obj" : $(SOURCE) $(DEP_CPP_VARRA) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\varray.sbr" : $(SOURCE) $(DEP_CPP_VARRA) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\mesa\GLonPVM\wmesa.c
DEP_CPP_WMESA=\
	{$(INCLUDE)}"\Wmesadef.h"\
	".\..\..\include\GL\wmesa.h"\
	".\..\..\GLonPVM\context.h"\
	".\..\..\src\dd.h"\
	".\..\..\src\xform.h"\
	".\..\..\src\vb.h"\
	".\..\..\src\matrix.h"\
	".\..\..\src\depth.h"\
	".\..\..\GLonPVM\profile.h"\
	"\wing\include\wing.h"\
	".\..\..\GLonPVM\mesa_extend.h"\
	".\..\..\GLonPVM\stereo.h"\
	".\..\..\GLonPVM\parallel.h"\
	".\..\..\src\tritemp.h"\
	".\..\..\src\context.h"\
	".\..\..\GLonPVM\colors.h"\
	".\..\..\include\GL/gl.h"\
	".\..\..\src\config.h"\
	{$(INCLUDE)}"\Wlpvm.h"\
	".\..\..\GLonPVM\osrender.h"\
	".\..\..\include\GL/osmesa.h"\
	

!IF  "$(CFG)" == "WINMESA - Win32 Release"


"$(INTDIR)\wmesa.obj" : $(SOURCE) $(DEP_CPP_WMESA) "$(INTDIR)"
   $(CPP) /nologo /ML /W3 /GX /I "..\..\include" /I "\wing\include" /D "NDEBUG"\
 /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /Fp"$(INTDIR)/Winmesa.pch" /YX\
 /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "WINMESA - Win32 Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
