# Microsoft Developer Studio Generated NMAKE File, Format Version 4.10
# ** DO NOT EDIT ** (yeah, right! :)

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" !=\
 "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "iluPr.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release (based on Win32 (x86) Dynamic-Link Library)"
!MESSAGE "Win32 Debug (based on Win32 (x86) Dynamic-Link Library)"
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
# PROP Target_Last_Scanned "Win32 Debug"
RSC=rc.exe
MTL=mktyplib.exe
CPP=cl.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : "$(ILUHOME)\lib\iluPr.pyd"

CLEAN : 
	-@erase "$(INTDIR)\ilualobject.obj"
	-@erase "$(INTDIR)\ilucaobject.obj"
	-@erase "$(INTDIR)\iluclobject.obj"
	-@erase "$(INTDIR)\iluftobject.obj"
	-@erase "$(INTDIR)\ilulpobject.obj"
	-@erase "$(INTDIR)\ilulrobject.obj"
	-@erase "$(INTDIR)\iluPrmodule.obj"
	-@erase "$(INTDIR)\ilusvobject.obj"
	-@erase "$(INTDIR)\iohcobject.obj"
	-@erase "$(INTDIR)\ivobject.obj"
	-@erase "$(INTDIR)\thcobject.obj"
	-@erase "$(OUTDIR)\iluPr.exp"
	-@erase "$(OUTDIR)\iluPr.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "$(ILUSRC)\runtime\kernel" /I "$(PYTHONSRC)\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "USE_DL_IMPORT" /D "HAVE_CONFIG_H" /YX /c
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "$(ILUSRC)\runtime\kernel" /I\
 "$(PYTHONSRC)\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "USE_DL_IMPORT"\
 /D "HAVE_CONFIG_H" /Fp"$(INTDIR)/iluPrModule.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/iluPrModule.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /out:"WinRel/iluPr.pyd"
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)/iluPr.pdb" /machine:I386 /out:"$(ILUHOME)/lib/iluPr.pyd"\
 /implib:"$(OUTDIR)/iluPr.lib" $(ILUHOME)\lib\ilu32.lib $(PYTHONSRC)\lib\python.lib $(PYTHONSRC)\lib\pythonc.lib /export:initiluPr 
LINK32_OBJS= \
	"$(INTDIR)\ilualobject.obj" \
	"$(INTDIR)\ilucaobject.obj" \
	"$(INTDIR)\iluclobject.obj" \
	"$(INTDIR)\iluftobject.obj" \
	"$(INTDIR)\ilulpobject.obj" \
	"$(INTDIR)\ilulrobject.obj" \
	"$(INTDIR)\iluPrmodule.obj" \
	"$(INTDIR)\ilusvobject.obj" \
	"$(INTDIR)\iohcobject.obj" \
	"$(INTDIR)\ivobject.obj" \
	"$(INTDIR)\thcobject.obj"

"$(ILUHOME)\lib\iluPr.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : "$(ILUHOME)\lib\iluPr.pyd"

CLEAN : 
	-@erase "$(INTDIR)\ilualobject.obj"
	-@erase "$(INTDIR)\ilucaobject.obj"
	-@erase "$(INTDIR)\iluclobject.obj"
	-@erase "$(INTDIR)\iluftobject.obj"
	-@erase "$(INTDIR)\ilulpobject.obj"
	-@erase "$(INTDIR)\ilulrobject.obj"
	-@erase "$(INTDIR)\iluPrmodule.obj"
	-@erase "$(INTDIR)\ilusvobject.obj"
	-@erase "$(INTDIR)\iohcobject.obj"
	-@erase "$(INTDIR)\ivobject.obj"
	-@erase "$(INTDIR)\thcobject.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\iluPr.exp"
	-@erase "$(OUTDIR)\iluPr.ilk"
	-@erase "$(OUTDIR)\iluPr.lib"
	-@erase "$(OUTDIR)\iluPr.pdb"


"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUSRC)\runtime\kernel" /I "$(PYTHONSRC)\include" /I "$(PYTHONSRC)\NT" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "USE_DL_IMPORT" /D "HAVE_CONFIG_H" /D "NT" /YX /c /Tp
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUSRC)\runtime\kernel" /I\
 "$(PYTHONSRC)\include" /I "$(PYTHONSRC)\NT" /D "_DEBUG" /D "WIN32" /D\
 "_WINDOWS" /D "USE_DL_IMPORT" /D "HAVE_CONFIG_H" /D "NT"\
 /Fp"$(INTDIR)/iluPrModule.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c /Tp 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/iluPrModule.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib $(ILUHOME)\lib\ilu32.lib $(PYTHONSRC)\lib\python.lib  $(PYTHONSRC)\lib\pythonc.lib /nologo /subsystem:windows /dll /debug /machine:I386 /out:"WinDebug/iluPr.pyd" /export:initiluPr
# SUBTRACT LINK32 /pdb:none
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib $(ILUHOME)\lib\ilu32.lib $(PYTHONSRC)\lib\python.lib $(PYTHONSRC)\lib\pythonc.lib /nologo\
 /subsystem:windows /dll /incremental:yes /pdb:"$(OUTDIR)/iluPr.pdb" /debug\
 /machine:I386 /out:"$(ILUHOME)/lib/iluPr.pyd" /implib:"$(OUTDIR)/iluPr.lib"\
 /export:initiluPr 
LINK32_OBJS= \
	"$(INTDIR)\ilualobject.obj" \
	"$(INTDIR)\ilucaobject.obj" \
	"$(INTDIR)\iluclobject.obj" \
	"$(INTDIR)\iluftobject.obj" \
	"$(INTDIR)\ilulpobject.obj" \
	"$(INTDIR)\ilulrobject.obj" \
	"$(INTDIR)\iluPrmodule.obj" \
	"$(INTDIR)\ilusvobject.obj" \
	"$(INTDIR)\iohcobject.obj" \
	"$(INTDIR)\ivobject.obj" \
	"$(INTDIR)\thcobject.obj"

"$(ILUHOME)\lib\iluPr.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "Win32 Release"
# Name "Win32 Debug"

!IF  "$(CFG)" == "Win32 Release"

!ELSEIF  "$(CFG)" == "Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\thcobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_THCOB=\
	"iluftobject.h"\
	"python.h"\
	"pythonversion.h"\
	"thcobject.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	
NODEP_CPP_THCOB=\
	"rename1.h"\
	

"$(INTDIR)\thcobject.obj" : $(SOURCE) $(DEP_CPP_THCOB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_THCOB=\
	"iluftobject.h"\
	"python.h"\
	"pythonversion.h"\
	"thcobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	
NODEP_CPP_THCOB=\
	"rename1.h"\
	

"$(INTDIR)\thcobject.obj" : $(SOURCE) $(DEP_CPP_THCOB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\ivobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_IVOBJ=\
	"ivobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	
NODEP_CPP_IVOBJ=\
	"rename1.h"\
	

"$(INTDIR)\ivobject.obj" : $(SOURCE) $(DEP_CPP_IVOBJ) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_IVOBJ=\
	"ivobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	
NODEP_CPP_IVOBJ=\
	"rename1.h"\
	

"$(INTDIR)\ivobject.obj" : $(SOURCE) $(DEP_CPP_IVOBJ) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\iohcobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_IOHCO=\
	"iohcobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	
NODEP_CPP_IOHCO=\
	"rename1.h"\
	

"$(INTDIR)\iohcobject.obj" : $(SOURCE) $(DEP_CPP_IOHCO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_IOHCO=\
	"iohcobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	
NODEP_CPP_IOHCO=\
	"rename1.h"\
	

"$(INTDIR)\iohcobject.obj" : $(SOURCE) $(DEP_CPP_IOHCO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\ilusvobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUSV=\
	"ilusvobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	
NODEP_CPP_ILUSV=\
	"rename1.h"\
	

"$(INTDIR)\ilusvobject.obj" : $(SOURCE) $(DEP_CPP_ILUSV) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUSV=\
	"ilusvobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	
NODEP_CPP_ILUSV=\
	"rename1.h"\
	

"$(INTDIR)\ilusvobject.obj" : $(SOURCE) $(DEP_CPP_ILUSV) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\iluPrmodule.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUPR=\
	"ilualobject.h"\
	"ilucaobject.h"\
	"iluclobject.h"\
	"iluftobject.h"\
	"ilugiobject.h"\
	"ilulpobject.h"\
	"ilulrobject.h"\
	"ilusvobject.h"\
	"iohcobject.h"\
	"ivobject.h"\
	"python.h"\
	"pythonversion.h"\
	"thcobject.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	
NODEP_CPP_ILUPR=\
	"rename1.h"\
	

"$(INTDIR)\iluPrmodule.obj" : $(SOURCE) $(DEP_CPP_ILUPR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUPR=\
	"ilualobject.h"\
	"ilucaobject.h"\
	"iluclobject.h"\
	"iluftobject.h"\
	"ilugiobject.h"\
	"ilulpobject.h"\
	"ilulrobject.h"\
	"ilusvobject.h"\
	"iohcobject.h"\
	"ivobject.h"\
	"python.h"\
	"pythonversion.h"\
	"thcobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	
NODEP_CPP_ILUPR=\
	"rename1.h"\
	

"$(INTDIR)\iluPrmodule.obj" : $(SOURCE) $(DEP_CPP_ILUPR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\ilulrobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILULR=\
	"ilulrobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	
NODEP_CPP_ILULR=\
	"rename1.h"\
	

"$(INTDIR)\ilulrobject.obj" : $(SOURCE) $(DEP_CPP_ILULR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILULR=\
	"ilulrobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	
NODEP_CPP_ILULR=\
	"rename1.h"\
	

"$(INTDIR)\ilulrobject.obj" : $(SOURCE) $(DEP_CPP_ILULR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\ilulpobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILULP=\
	"ilulpobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	
NODEP_CPP_ILULP=\
	"rename1.h"\
	

"$(INTDIR)\ilulpobject.obj" : $(SOURCE) $(DEP_CPP_ILULP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILULP=\
	"ilulpobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	
NODEP_CPP_ILULP=\
	"rename1.h"\
	

"$(INTDIR)\ilulpobject.obj" : $(SOURCE) $(DEP_CPP_ILULP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\iluftobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUFT=\
	"iluftobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	
NODEP_CPP_ILUFT=\
	"rename1.h"\
	

"$(INTDIR)\iluftobject.obj" : $(SOURCE) $(DEP_CPP_ILUFT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUFT=\
	"iluftobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	
NODEP_CPP_ILUFT=\
	"rename1.h"\
	

"$(INTDIR)\iluftobject.obj" : $(SOURCE) $(DEP_CPP_ILUFT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\iluclobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUCL=\
	"iluclobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	
NODEP_CPP_ILUCL=\
	"rename1.h"\
	

"$(INTDIR)\iluclobject.obj" : $(SOURCE) $(DEP_CPP_ILUCL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUCL=\
	"iluclobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	
NODEP_CPP_ILUCL=\
	"rename1.h"\
	

"$(INTDIR)\iluclobject.obj" : $(SOURCE) $(DEP_CPP_ILUCL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\ilucaobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUCA=\
	"ilucaobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	
NODEP_CPP_ILUCA=\
	"rename1.h"\
	

"$(INTDIR)\ilucaobject.obj" : $(SOURCE) $(DEP_CPP_ILUCA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUCA=\
	"ilucaobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	
NODEP_CPP_ILUCA=\
	"rename1.h"\
	

"$(INTDIR)\ilucaobject.obj" : $(SOURCE) $(DEP_CPP_ILUCA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\ilualobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUAL=\
	"ilualobject.h"\
	"iluftobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	
NODEP_CPP_ILUAL=\
	"rename1.h"\
	

"$(INTDIR)\ilualobject.obj" : $(SOURCE) $(DEP_CPP_ILUAL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUAL=\
	"ilualobject.h"\
	"iluftobject.h"\
	"python.h"\
	"pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\Include\abstract.h"\
	"$(PYTHONSRC)\Include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\Include\ceval.h"\
	"$(PYTHONSRC)\Include\classobject.h"\
	"$(PYTHONSRC)\include\config.h"\
	"$(PYTHONSRC)\Include\errors.h"\
	"$(PYTHONSRC)\Include\fileobject.h"\
	"$(PYTHONSRC)\Include\floatobject.h"\
	"$(PYTHONSRC)\Include\funcobject.h"\
	"$(PYTHONSRC)\Include\intobject.h"\
	"$(PYTHONSRC)\Include\listobject.h"\
	"$(PYTHONSRC)\Include\longobject.h"\
	"$(PYTHONSRC)\Include\mappingobject.h"\
	"$(PYTHONSRC)\Include\methodobject.h"\
	"$(PYTHONSRC)\Include\modsupport.h"\
	"$(PYTHONSRC)\Include\moduleobject.h"\
	"$(PYTHONSRC)\Include\mymalloc.h"\
	"$(PYTHONSRC)\Include\myproto.h"\
	"$(PYTHONSRC)\Include\object.h"\
	"$(PYTHONSRC)\Include\objimpl.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\Include\rangeobject.h"\
	"$(PYTHONSRC)\Include\rename2.h"\
	"$(PYTHONSRC)\Include\stringobject.h"\
	"$(PYTHONSRC)\Include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\Include\tupleobject.h"\
	
NODEP_CPP_ILUAL=\
	"rename1.h"\
	

"$(INTDIR)\ilualobject.obj" : $(SOURCE) $(DEP_CPP_ILUAL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
