# Microsoft Visual C++ generated build script - Do not modify

PROJ = ILUC16W
DEBUG = 0
PROGTYPE = 4
CALLER = 
ARGS = 
DLLS = 
D_RCDEFINES = -d_DEBUG
R_RCDEFINES = -dNDEBUG
ORIGIN = MSVC
ORIGIN_VER = 1.00
PROJPATH = F:\ILU\SRC\RUNTIME\C\
USEMFC = 0
CC = cl
CPP = cl
CXX = cl
CCREATEPCHFLAG = 
CPPCREATEPCHFLAG = 
CUSEPCHFLAG = 
CPPUSEPCHFLAG = 
FIRSTC = ILU.C       
FIRSTCPP =             
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
		ORB.SBR


ILU_DEP = $(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\c\iluchdrs.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(WSOCKHDR)


ORB_DEP = $(ILUSRC)\runtime\c\iluchdrs.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h


all:	$(PROJ).LIB

ILU.OBJ:	ILU.C $(ILU_DEP)
	$(CC) $(CFLAGS) $(CCREATEPCHFLAG) /c ILU.C

ORB.OBJ:	ORB.C $(ORB_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c ORB.C

$(PROJ).LIB::	ILU.OBJ ORB.OBJ $(OBJS_EXT)
	echo >NUL @<<$(PROJ).CRF
$@ /PAGESIZE:64
y
+ILU.OBJ &
+ORB.OBJ &
;
<<
	if exist $@ del $@
	lib @$(PROJ).CRF

$(PROJ).BSC: $(SBRS)
	bscmake @<<
/o$@ $(SBRS)
<<
