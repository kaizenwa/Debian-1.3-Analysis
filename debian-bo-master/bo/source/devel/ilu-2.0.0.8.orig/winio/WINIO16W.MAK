# Microsoft Visual C++ generated build script - Do not modify

PROJ = WINIO16W
DEBUG = 0
PROGTYPE = 4
CALLER = 
ARGS = 
DLLS = 
D_RCDEFINES = -d_DEBUG
R_RCDEFINES = -dNDEBUG
ORIGIN = MSVC
ORIGIN_VER = 1.00
PROJPATH = F:\ILU\SRC\WINIO\
USEMFC = 0
CC = cl
CPP = cl
CXX = cl
CCREATEPCHFLAG = 
CPPCREATEPCHFLAG = 
CUSEPCHFLAG = 
CPPUSEPCHFLAG = 
FIRSTC = WINIO.C     
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
SBRS = WINIO.SBR \
		WMHANDLR.SBR


WINIO_DEP = $(ILUSRC)\winio\wmhandlr.h \
	$(ILUSRC)\winio\winio.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\winio\winio.h \
	$(ILUSRC)\winio\winiodef.h


WMHANDLR_DEP = $(ILUSRC)\winio\wmhandlr.h


all:	$(PROJ).LIB

WINIO.OBJ:	WINIO.C $(WINIO_DEP)
	$(CC) $(CFLAGS) $(CCREATEPCHFLAG) /c WINIO.C

WMHANDLR.OBJ:	WMHANDLR.C $(WMHANDLR_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c WMHANDLR.C

$(PROJ).LIB::	WINIO.OBJ WMHANDLR.OBJ $(OBJS_EXT)
	echo >NUL @<<$(PROJ).CRF
$@ /PAGESIZE:64
y
+WINIO.OBJ &
+WMHANDLR.OBJ &
;
<<
	if exist $@ del $@
	lib @$(PROJ).CRF

$(PROJ).BSC: $(SBRS)
	bscmake @<<
/o$@ $(SBRS)
<<
