# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=vmsvc32 - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to vmsvc32 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "vmsvc32 - Win32 Release" && "$(CFG)" !=\
 "vmsvc32 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "vmsvc32.mak" CFG="vmsvc32 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "vmsvc32 - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "vmsvc32 - Win32 Debug" (based on "Win32 (x86) Static Library")
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
# PROP Target_Last_Scanned "vmsvc32 - Win32 Debug"
CPP=cl.exe

!IF  "$(CFG)" == "vmsvc32 - Win32 Release"

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
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\vmsvc32.lib"

CLEAN : 
	-@erase ".\Release\vmsvc32.lib"
	-@erase ".\Release\Vchkboxc.obj"
	-@erase ".\Release\Vlistc.obj"
	-@erase ".\Release\Vcmdprnt.obj"
	-@erase ".\Release\Vdialog.obj"
	-@erase ".\Release\Vmemdc.obj"
	-@erase ".\Release\Vawinfo.obj"
	-@erase ".\Release\Vlabelc.obj"
	-@erase ".\Release\Vcbtncmd.obj"
	-@erase ".\Release\Vwindc.obj"
	-@erase ".\Release\Vfont.obj"
	-@erase ".\Release\Vcmdwin.obj"
	-@erase ".\Release\Vradioc.obj"
	-@erase ".\Release\Vtimer.obj"
	-@erase ".\Release\Vcmdpane.obj"
	-@erase ".\Release\Vcolor.obj"
	-@erase ".\Release\Vapp.obj"
	-@erase ".\Release\Vreply.obj"
	-@erase ".\Release\Vfinddlg.obj"
	-@erase ".\Release\Vtextinc.obj"
	-@erase ".\Release\Vsliderc.obj"
	-@erase ".\Release\Vutil.obj"
	-@erase ".\Release\Vtexted.obj"
	-@erase ".\Release\Vwinprdc.obj"
	-@erase ".\Release\Vmenu.obj"
	-@erase ".\Release\Vframec.obj"
	-@erase ".\Release\Vbaseitm.obj"
	-@erase ".\Release\Vfilesel.obj"
	-@erase ".\Release\Vpen.obj"
	-@erase ".\Release\Vwindow.obj"
	-@erase ".\Release\Vprogrsc.obj"
	-@erase ".\Release\Vwinprtr.obj"
	-@erase ".\Release\Vclabelc.obj"
	-@erase ".\Release\Vspinc.obj"
	-@erase ".\Release\Vnotice.obj"
	-@erase ".\Release\Vtextcnv.obj"
	-@erase ".\Release\Vbtncmd.obj"
	-@erase ".\Release\Vdebug.obj"
	-@erase ".\Release\Vboxlblc.obj"
	-@erase ".\Release\Vthislst.obj"
	-@erase ".\Release\Vbasewin.obj"
	-@erase ".\Release\Vcomboc.obj"
	-@erase ".\Release\Vcmd.obj"
	-@erase ".\Release\Vtextc.obj"
	-@erase ".\Release\Vcpdc.obj"
	-@erase ".\Release\Vmodald.obj"
	-@erase ".\Release\Vynreply.obj"
	-@erase ".\Release\Vbrush.obj"
	-@erase ".\Release\Vicon.obj"
	-@erase ".\Release\Vcanvas.obj"
	-@erase ".\Release\Vfontsel.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /I "c:\v\includew" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /I "c:\v\includew" /D "WIN32" /D "NDEBUG" /D\
 "_WINDOWS" /Fp"$(INTDIR)/vmsvc32.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/vmsvc32.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/vmsvc32.lib" 
LIB32_OBJS= \
	"$(INTDIR)/Vchkboxc.obj" \
	"$(INTDIR)/Vlistc.obj" \
	"$(INTDIR)/Vcmdprnt.obj" \
	"$(INTDIR)/Vdialog.obj" \
	"$(INTDIR)/Vmemdc.obj" \
	"$(INTDIR)/Vawinfo.obj" \
	"$(INTDIR)/Vlabelc.obj" \
	"$(INTDIR)/Vcbtncmd.obj" \
	"$(INTDIR)/Vwindc.obj" \
	"$(INTDIR)/Vfont.obj" \
	"$(INTDIR)/Vcmdwin.obj" \
	"$(INTDIR)/Vradioc.obj" \
	"$(INTDIR)/Vtimer.obj" \
	"$(INTDIR)/Vcmdpane.obj" \
	"$(INTDIR)/Vcolor.obj" \
	"$(INTDIR)/Vapp.obj" \
	"$(INTDIR)/Vreply.obj" \
	"$(INTDIR)/Vfinddlg.obj" \
	"$(INTDIR)/Vtextinc.obj" \
	"$(INTDIR)/Vsliderc.obj" \
	"$(INTDIR)/Vutil.obj" \
	"$(INTDIR)/Vtexted.obj" \
	"$(INTDIR)/Vwinprdc.obj" \
	"$(INTDIR)/Vmenu.obj" \
	"$(INTDIR)/Vframec.obj" \
	"$(INTDIR)/Vbaseitm.obj" \
	"$(INTDIR)/Vfilesel.obj" \
	"$(INTDIR)/Vpen.obj" \
	"$(INTDIR)/Vwindow.obj" \
	"$(INTDIR)/Vprogrsc.obj" \
	"$(INTDIR)/Vwinprtr.obj" \
	"$(INTDIR)/Vclabelc.obj" \
	"$(INTDIR)/Vspinc.obj" \
	"$(INTDIR)/Vnotice.obj" \
	"$(INTDIR)/Vtextcnv.obj" \
	"$(INTDIR)/Vbtncmd.obj" \
	"$(INTDIR)/Vdebug.obj" \
	"$(INTDIR)/Vboxlblc.obj" \
	"$(INTDIR)/Vthislst.obj" \
	"$(INTDIR)/Vbasewin.obj" \
	"$(INTDIR)/Vcomboc.obj" \
	"$(INTDIR)/Vcmd.obj" \
	"$(INTDIR)/Vtextc.obj" \
	"$(INTDIR)/Vcpdc.obj" \
	"$(INTDIR)/Vmodald.obj" \
	"$(INTDIR)/Vynreply.obj" \
	"$(INTDIR)/Vbrush.obj" \
	"$(INTDIR)/Vicon.obj" \
	"$(INTDIR)/Vcanvas.obj" \
	"$(INTDIR)/Vfontsel.obj"

"$(OUTDIR)\vmsvc32.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "vmsvc32 - Win32 Debug"

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
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\vmsvc32.lib"

CLEAN : 
	-@erase ".\Debug\vmsvc32.lib"
	-@erase ".\Debug\Vwinprdc.obj"
	-@erase ".\Debug\Vbrush.obj"
	-@erase ".\Debug\Vmodald.obj"
	-@erase ".\Debug\Vbaseitm.obj"
	-@erase ".\Debug\Vprogrsc.obj"
	-@erase ".\Debug\Vlistc.obj"
	-@erase ".\Debug\Vwinprtr.obj"
	-@erase ".\Debug\Vcanvas.obj"
	-@erase ".\Debug\Vclabelc.obj"
	-@erase ".\Debug\Vcpdc.obj"
	-@erase ".\Debug\Vwindc.obj"
	-@erase ".\Debug\Vtextcnv.obj"
	-@erase ".\Debug\Vdialog.obj"
	-@erase ".\Debug\Vcolor.obj"
	-@erase ".\Debug\Vawinfo.obj"
	-@erase ".\Debug\Vboxlblc.obj"
	-@erase ".\Debug\Vthislst.obj"
	-@erase ".\Debug\Vbasewin.obj"
	-@erase ".\Debug\Vapp.obj"
	-@erase ".\Debug\Vmenu.obj"
	-@erase ".\Debug\Vlabelc.obj"
	-@erase ".\Debug\Vreply.obj"
	-@erase ".\Debug\Vicon.obj"
	-@erase ".\Debug\Vcomboc.obj"
	-@erase ".\Debug\Vcmdwin.obj"
	-@erase ".\Debug\Vradioc.obj"
	-@erase ".\Debug\Vynreply.obj"
	-@erase ".\Debug\Vfilesel.obj"
	-@erase ".\Debug\Vpen.obj"
	-@erase ".\Debug\Vfontsel.obj"
	-@erase ".\Debug\Vtexted.obj"
	-@erase ".\Debug\Vmemdc.obj"
	-@erase ".\Debug\Vchkboxc.obj"
	-@erase ".\Debug\Vframec.obj"
	-@erase ".\Debug\Vfont.obj"
	-@erase ".\Debug\Vcmdprnt.obj"
	-@erase ".\Debug\Vwindow.obj"
	-@erase ".\Debug\Vspinc.obj"
	-@erase ".\Debug\Vtimer.obj"
	-@erase ".\Debug\Vcbtncmd.obj"
	-@erase ".\Debug\Vutil.obj"
	-@erase ".\Debug\Vdebug.obj"
	-@erase ".\Debug\Vcmdpane.obj"
	-@erase ".\Debug\Vtextc.obj"
	-@erase ".\Debug\Vcmd.obj"
	-@erase ".\Debug\Vnotice.obj"
	-@erase ".\Debug\Vfinddlg.obj"
	-@erase ".\Debug\Vbtncmd.obj"
	-@erase ".\Debug\Vtextinc.obj"
	-@erase ".\Debug\Vsliderc.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "c:\v\includew" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /GX /Z7 /Od /I "c:\v\includew" /D "WIN32" /D "_DEBUG"\
 /D "_WINDOWS" /Fp"$(INTDIR)/vmsvc32.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/vmsvc32.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/vmsvc32.lib" 
LIB32_OBJS= \
	"$(INTDIR)/Vwinprdc.obj" \
	"$(INTDIR)/Vbrush.obj" \
	"$(INTDIR)/Vmodald.obj" \
	"$(INTDIR)/Vbaseitm.obj" \
	"$(INTDIR)/Vprogrsc.obj" \
	"$(INTDIR)/Vlistc.obj" \
	"$(INTDIR)/Vwinprtr.obj" \
	"$(INTDIR)/Vcanvas.obj" \
	"$(INTDIR)/Vclabelc.obj" \
	"$(INTDIR)/Vcpdc.obj" \
	"$(INTDIR)/Vwindc.obj" \
	"$(INTDIR)/Vtextcnv.obj" \
	"$(INTDIR)/Vdialog.obj" \
	"$(INTDIR)/Vcolor.obj" \
	"$(INTDIR)/Vawinfo.obj" \
	"$(INTDIR)/Vboxlblc.obj" \
	"$(INTDIR)/Vthislst.obj" \
	"$(INTDIR)/Vbasewin.obj" \
	"$(INTDIR)/Vapp.obj" \
	"$(INTDIR)/Vmenu.obj" \
	"$(INTDIR)/Vlabelc.obj" \
	"$(INTDIR)/Vreply.obj" \
	"$(INTDIR)/Vicon.obj" \
	"$(INTDIR)/Vcomboc.obj" \
	"$(INTDIR)/Vcmdwin.obj" \
	"$(INTDIR)/Vradioc.obj" \
	"$(INTDIR)/Vynreply.obj" \
	"$(INTDIR)/Vfilesel.obj" \
	"$(INTDIR)/Vpen.obj" \
	"$(INTDIR)/Vfontsel.obj" \
	"$(INTDIR)/Vtexted.obj" \
	"$(INTDIR)/Vmemdc.obj" \
	"$(INTDIR)/Vchkboxc.obj" \
	"$(INTDIR)/Vframec.obj" \
	"$(INTDIR)/Vfont.obj" \
	"$(INTDIR)/Vcmdprnt.obj" \
	"$(INTDIR)/Vwindow.obj" \
	"$(INTDIR)/Vspinc.obj" \
	"$(INTDIR)/Vtimer.obj" \
	"$(INTDIR)/Vcbtncmd.obj" \
	"$(INTDIR)/Vutil.obj" \
	"$(INTDIR)/Vdebug.obj" \
	"$(INTDIR)/Vcmdpane.obj" \
	"$(INTDIR)/Vtextc.obj" \
	"$(INTDIR)/Vcmd.obj" \
	"$(INTDIR)/Vnotice.obj" \
	"$(INTDIR)/Vfinddlg.obj" \
	"$(INTDIR)/Vbtncmd.obj" \
	"$(INTDIR)/Vtextinc.obj" \
	"$(INTDIR)/Vsliderc.obj"

"$(OUTDIR)\vmsvc32.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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

# Name "vmsvc32 - Win32 Release"
# Name "vmsvc32 - Win32 Debug"

!IF  "$(CFG)" == "vmsvc32 - Win32 Release"

!ELSEIF  "$(CFG)" == "vmsvc32 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vlistc.cpp
DEP_CPP_VLIST=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vlistc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vlistc.obj" : $(SOURCE) $(DEP_CPP_VLIST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vawinfo.cpp
DEP_CPP_VAWIN=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/v_defs.h"\
	

"$(INTDIR)\Vawinfo.obj" : $(SOURCE) $(DEP_CPP_VAWIN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vbaseitm.cpp
DEP_CPP_VBASE=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	

"$(INTDIR)\Vbaseitm.obj" : $(SOURCE) $(DEP_CPP_VBASE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vbasewin.cpp
DEP_CPP_VBASEW=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	

"$(INTDIR)\Vbasewin.obj" : $(SOURCE) $(DEP_CPP_VBASEW) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vboxlblc.cpp
DEP_CPP_VBOXL=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vboxlblc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vboxlblc.obj" : $(SOURCE) $(DEP_CPP_VBOXL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vbrush.cpp
DEP_CPP_VBRUS=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcolor.h"\
	

"$(INTDIR)\Vbrush.obj" : $(SOURCE) $(DEP_CPP_VBRUS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vbtncmd.cpp
DEP_CPP_VBTNC=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vbtncmd.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vicon.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vbtncmd.obj" : $(SOURCE) $(DEP_CPP_VBTNC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vcanvas.cpp
DEP_CPP_VCANV=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	

"$(INTDIR)\Vcanvas.obj" : $(SOURCE) $(DEP_CPP_VCANV) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vcbtncmd.cpp
DEP_CPP_VCBTN=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vcbtncmd.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vbtncmd.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vcbtncmd.obj" : $(SOURCE) $(DEP_CPP_VCBTN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vchkboxc.cpp
DEP_CPP_VCHKB=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vchkboxc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vchkboxc.obj" : $(SOURCE) $(DEP_CPP_VCHKB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vclabelc.cpp
DEP_CPP_VCLAB=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vclabelc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vlabelc.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vclabelc.obj" : $(SOURCE) $(DEP_CPP_VCLAB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vcmd.cpp
DEP_CPP_VCMD_=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vcmd.obj" : $(SOURCE) $(DEP_CPP_VCMD_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vcmdpane.cpp
DEP_CPP_VCMDP=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vthislst.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	

"$(INTDIR)\Vcmdpane.obj" : $(SOURCE) $(DEP_CPP_VCMDP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vcmdprnt.cpp
DEP_CPP_VCMDPR=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbtncmd.h"\
	"c:\v\includew\v/vcbtncmd.h"\
	"c:\v\includew\v/vclabelc.h"\
	"c:\v\includew\v/vlabelc.h"\
	"c:\v\includew\v/vframec.h"\
	"c:\v\includew\v/vchkboxc.h"\
	"c:\v\includew\v/vradioc.h"\
	"c:\v\includew\v/vtextinc.h"\
	"c:\v\includew\v/vtextc.h"\
	"c:\v\includew\v/vlistc.h"\
	"c:\v\includew\v/vcomboc.h"\
	"c:\v\includew\v/vspinc.h"\
	"c:\v\includew\v/vsliderc.h"\
	"c:\v\includew\v/vprogrsc.h"\
	"c:\v\includew\v/vboxlblc.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcolor.h"\
	

"$(INTDIR)\Vcmdprnt.obj" : $(SOURCE) $(DEP_CPP_VCMDPR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vcmdwin.cpp
DEP_CPP_VCMDW=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vcmdwin.obj" : $(SOURCE) $(DEP_CPP_VCMDW) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vcolor.cpp
DEP_CPP_VCOLO=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vcolor.obj" : $(SOURCE) $(DEP_CPP_VCOLO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vcomboc.cpp
DEP_CPP_VCOMB=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vcomboc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vcomboc.obj" : $(SOURCE) $(DEP_CPP_VCOMB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vcpdc.cpp
DEP_CPP_VCPDC=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vcpdc.obj" : $(SOURCE) $(DEP_CPP_VCPDC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vdebug.cpp
DEP_CPP_VDEBU=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vdebug.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vdebug.obj" : $(SOURCE) $(DEP_CPP_VDEBU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vdialog.cpp
DEP_CPP_VDIAL=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vthislst.h"\
	"c:\v\includew\v/vclabelc.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vlabelc.h"\
	"c:\v\includew\v/vcolor.h"\
	

"$(INTDIR)\Vdialog.obj" : $(SOURCE) $(DEP_CPP_VDIAL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vfilesel.cpp
DEP_CPP_VFILE=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vfilesel.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vfilesel.obj" : $(SOURCE) $(DEP_CPP_VFILE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vfinddlg.cpp
DEP_CPP_VFIND=\
	"c:\v\includew\v/vfinddlg.h"\
	"c:\v\includew\v/vicon.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vfinddlg.obj" : $(SOURCE) $(DEP_CPP_VFIND) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vfont.cpp
DEP_CPP_VFONT=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vfont.obj" : $(SOURCE) $(DEP_CPP_VFONT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vfontsel.cpp
DEP_CPP_VFONTS=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vfontsel.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vfontsel.obj" : $(SOURCE) $(DEP_CPP_VFONTS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vframec.cpp
DEP_CPP_VFRAM=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vframec.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vframec.obj" : $(SOURCE) $(DEP_CPP_VFRAM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vicon.cpp
DEP_CPP_VICON=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vicon.h"\
	

"$(INTDIR)\Vicon.obj" : $(SOURCE) $(DEP_CPP_VICON) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vlabelc.cpp
DEP_CPP_VLABE=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vlabelc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vicon.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vlabelc.obj" : $(SOURCE) $(DEP_CPP_VLABE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vapp.cpp
DEP_CPP_VAPP_=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/vcmdpane.h"\
	"c:\v\includew\v/vthislst.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vapp.obj" : $(SOURCE) $(DEP_CPP_VAPP_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vynreply.cpp
DEP_CPP_VYNRE=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vynreply.h"\
	"c:\v\includew\v/vicon.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vynreply.obj" : $(SOURCE) $(DEP_CPP_VYNRE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vmenu.cpp
DEP_CPP_VMENU=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vcmdwin.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vmenu.obj" : $(SOURCE) $(DEP_CPP_VMENU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vmodald.cpp
DEP_CPP_VMODA=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vmodald.obj" : $(SOURCE) $(DEP_CPP_VMODA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vnotice.cpp
DEP_CPP_VNOTI=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vicon.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vnotice.obj" : $(SOURCE) $(DEP_CPP_VNOTI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vpen.cpp
DEP_CPP_VPEN_=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcolor.h"\
	

"$(INTDIR)\Vpen.obj" : $(SOURCE) $(DEP_CPP_VPEN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vprogrsc.cpp
DEP_CPP_VPROG=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vprogrsc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vprogrsc.obj" : $(SOURCE) $(DEP_CPP_VPROG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vradioc.cpp
DEP_CPP_VRADI=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vradioc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vradioc.obj" : $(SOURCE) $(DEP_CPP_VRADI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vreply.cpp
DEP_CPP_VREPL=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vreply.h"\
	"c:\v\includew\v/vicon.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vreply.obj" : $(SOURCE) $(DEP_CPP_VREPL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vsliderc.cpp
DEP_CPP_VSLID=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vsliderc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vsliderc.obj" : $(SOURCE) $(DEP_CPP_VSLID) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vspinc.cpp
DEP_CPP_VSPIN=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vspinc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vspinc.obj" : $(SOURCE) $(DEP_CPP_VSPIN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vtextc.cpp
DEP_CPP_VTEXT=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vtextc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vutil.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vtextc.obj" : $(SOURCE) $(DEP_CPP_VTEXT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vtextcnv.cpp
DEP_CPP_VTEXTC=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vtextcnv.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vtextcnv.obj" : $(SOURCE) $(DEP_CPP_VTEXTC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vtexted.cpp
DEP_CPP_VTEXTE=\
	"c:\v\includew\v/vkeys.h"\
	"c:\v\includew\v/vtexted.h"\
	"c:\v\includew\v/vfinddlg.h"\
	"c:\v\includew\v/vtextcnv.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vtexted.obj" : $(SOURCE) $(DEP_CPP_VTEXTE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vtextinc.cpp
DEP_CPP_VTEXTI=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vtextinc.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vtextinc.obj" : $(SOURCE) $(DEP_CPP_VTEXTI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vthislst.cpp
DEP_CPP_VTHIS=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vthislst.h"\
	

"$(INTDIR)\Vthislst.obj" : $(SOURCE) $(DEP_CPP_VTHIS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vtimer.cpp
DEP_CPP_VTIME=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vthislst.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vtimer.obj" : $(SOURCE) $(DEP_CPP_VTIME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vutil.cpp
DEP_CPP_VUTIL=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vutil.h"\
	

"$(INTDIR)\Vutil.obj" : $(SOURCE) $(DEP_CPP_VUTIL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vwindc.cpp
DEP_CPP_VWIND=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vmemdc.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vicon.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vcpdc.h"\
	

"$(INTDIR)\Vwindc.obj" : $(SOURCE) $(DEP_CPP_VWIND) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vwindow.cpp
DEP_CPP_VWINDO=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vwindow.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vkeys.h"\
	"c:\v\includew\v/vnotice.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcanvas.h"\
	"c:\v\includew\v/vcpdc.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vwindow.obj" : $(SOURCE) $(DEP_CPP_VWINDO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vwinprdc.cpp
DEP_CPP_VWINP=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vwinprdc.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vwinprdc.obj" : $(SOURCE) $(DEP_CPP_VWINP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vwinprtr.cpp
DEP_CPP_VWINPR=\
	"c:\v\includew\v/vwin32.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vpane.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vcmd.h"\
	

"$(INTDIR)\Vwinprtr.obj" : $(SOURCE) $(DEP_CPP_VWINPR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\V\Srcwin\Vmemdc.cpp
DEP_CPP_VMEMD=\
	"c:\v\includew\v/vmemdc.h"\
	"c:\v\includew\v/vapp.h"\
	"c:\v\includew\v/vwindc.h"\
	"c:\v\includew\v/vdc.h"\
	"c:\v\includew\v/v_defs.h"\
	"c:\v\includew\v/vfont.h"\
	"c:\v\includew\v/vpen.h"\
	"c:\v\includew\v/vbrush.h"\
	"c:\v\includew\v/vprinter.h"\
	"c:\v\includew\v/vcolor.h"\
	"c:\v\includew\v/vwinprtr.h"\
	"c:\v\includew\v/vmodald.h"\
	"c:\v\includew\v/vdialog.h"\
	"c:\v\includew\v/vbasewin.h"\
	"c:\v\includew\v/vcmdprnt.h"\
	"c:\v\includew\v/vbaseitm.h"\
	"c:\v\includew\v/vcmd.h"\
	"c:\v\includew\v/vawinfo.h"\
	"c:\v\includew\v/vmenu.h"\
	"c:\v\includew\v/vtimer.h"\
	"c:\v\includew\v/vpane.h"\
	

"$(INTDIR)\Vmemdc.obj" : $(SOURCE) $(DEP_CPP_VMEMD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
