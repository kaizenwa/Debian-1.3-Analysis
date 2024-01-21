# Microsoft Visual C++ generated build script - Do not modify

PROJ = ILUCP16W
DEBUG = 0
PROGTYPE = 4
CALLER = 
ARGS = 
DLLS = 
D_RCDEFINES = -d_DEBUG
R_RCDEFINES = -dNDEBUG
ORIGIN = MSVC
ORIGIN_VER = 1.00
PROJPATH = F:\ILU\SRC\RUNTIME\C__~1\
USEMFC = 0
CC = cl
CPP = cl
CXX = cl
CCREATEPCHFLAG = 
CPPCREATEPCHFLAG = 
CUSEPCHFLAG = 
CPPUSEPCHFLAG = 
FIRSTC =             
FIRSTCPP = ILU.CPP     
RC = rc
CFLAGS_D_LIB = /nologo /G2 /W3 /Z7 /AL /Gt0 /Gx- /Od /D "_DEBUG" /D "WIN16" /D "_WINIO" /GA 
CFLAGS_R_LIB = /nologo /Gs /G2 /W3 /AL /Gt0 /Gx- /Ox /D "NDEBUG" /D "WIN16" /D "_WINIO" /GA 
RCFLAGS = /nologo
RESFLAGS = /nologo
RUNFLAGS = 
OBJS_EXT = 
LIBS_EXT = 
!if "$(DEBUG)" == "1"
CFLAGS = $(CFLAGS_D_LIB)
LFLAGS = 
LIBS = 
MAPFILE = nul
RCDEFINES = $(D_RCDEFINES)
!else
CFLAGS = $(CFLAGS_R_LIB)
LFLAGS = 
LIBS = 
MAPFILE = nul
RCDEFINES = $(R_RCDEFINES)
!endif
!if [if exist MSVC.BND del MSVC.BND]
!endif
SBRS = ILU.SBR \
		ILUOBJ.SBR \
		ILUSRVR.SBR


ILU_DEP = $(ILUSRC)\runtime\cpp\ilu.hh \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h


ILUOBJ_DEP = $(ILUSRC)\runtime\cpp\ilu.hh \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h


ILUSRVR_DEP = $(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\cpp\ilu.hh \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h


all:	$(PROJ).LIB

ILU.OBJ:	ILU.CPP $(ILU_DEP)
	$(CPP) $(CFLAGS) $(CPPCREATEPCHFLAG) /c ILU.CPP

ILUOBJ.OBJ:	ILUOBJ.CPP $(ILUOBJ_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ILUOBJ.CPP

ILUSRVR.OBJ:	ILUSRVR.CPP $(ILUSRVR_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c ILUSRVR.CPP

$(PROJ).LIB::	ILU.OBJ ILUOBJ.OBJ ILUSRVR.OBJ $(OBJS_EXT)
	echo >NUL @<<$(PROJ).CRF
$@ /PAGESIZE:64
y
+ILU.OBJ &
+ILUOBJ.OBJ &
+ILUSRVR.OBJ &
;
<<
	if exist $@ del $@
	lib @$(PROJ).CRF

$(PROJ).BSC: $(SBRS)
	bscmake @<<
/o$@ $(SBRS)
<<
