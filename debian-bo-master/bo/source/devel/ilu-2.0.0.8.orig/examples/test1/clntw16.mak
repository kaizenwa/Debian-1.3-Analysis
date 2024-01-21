# Microsoft Visual C++ generated build script - Do not modify

PROJ = CLNTW16
DEBUG = 0
PROGTYPE = 0
CALLER = 
ARGS = 
DLLS = 
D_RCDEFINES = -d_DEBUG
R_RCDEFINES = -dNDEBUG
ORIGIN = MSVC
ORIGIN_VER = 1.00
PROJPATH = F:\ILU\SRC\EXAMPLES\TEST1C\
USEMFC = 0
CC = cl
CPP = cl
CXX = cl
CCREATEPCHFLAG = 
CPPCREATEPCHFLAG = 
CUSEPCHFLAG = 
CPPUSEPCHFLAG = 
FIRSTC = CLNTW.C     
FIRSTCPP =             
RC = rc
CFLAGS_D_WEXE = /nologo /G2 /W3 /Zi /AL /YX /Od /D "_DEBUG" /D "WIN16" /D "_WINIO" /I $(ILUHOME)\include /GA /Fd"CLNTW16.PDB" /Fp"CLNTW16.PCH"
CFLAGS_R_WEXE = /nologo /W3 /AL /YX /Ox /D "NDEBUG" /D "WIN16" /D "_WINIO" /I $(ILUHOME)\include /GA /Fp"CLNTW16.PCH"
LFLAGS_D_WEXE = /NOLOGO /NOD /PACKC:61440 /STACK:10240 /ALIGN:16 /ONERROR:NOEXE /CO  
LFLAGS_R_WEXE = /NOLOGO /NOD /PACKC:61440 /STACK:10240 /ALIGN:16 /ONERROR:NOEXE  
LIBS_D_WEXE = $(ILUHOME)\lib\iluc16w.lib $(ILUHOME)\lib\ilu16w.lib $(ILUHOME)\lib\winio16w.lib $(WSOCKLIB) oldnames libw llibcew commdlg.lib olecli.lib olesvr.lib shell.lib 
LIBS_R_WEXE = $(ILUHOME)\lib\iluc16w.lib $(ILUHOME)\lib\ilu16w.lib $(ILUHOME)\lib\winio16w.lib $(WSOCKLIB) oldnames libw llibcew commdlg.lib olecli.lib olesvr.lib shell.lib 
RCFLAGS = /nologo
RESFLAGS = /nologo
RUNFLAGS = 
DEFFILE = CLNTW16.DEF
OBJS_EXT = 
LIBS_EXT = 
!if "$(DEBUG)" == "1"
CFLAGS = $(CFLAGS_D_WEXE)
LFLAGS = $(LFLAGS_D_WEXE)
LIBS = $(LIBS_D_WEXE)
MAPFILE = nul
RCDEFINES = $(D_RCDEFINES)
!else
CFLAGS = $(CFLAGS_R_WEXE)
LFLAGS = $(LFLAGS_R_WEXE)
LIBS = $(LIBS_R_WEXE)
MAPFILE = nul
RCDEFINES = $(R_RCDEFINES)
!endif
!if [if exist MSVC.BND del MSVC.BND]
!endif
SBRS = CLNTW.SBR \
		T1COMM.SBR \
		T1SURRGT.SBR \
		T2COMM.SBR \
		T2SURRGT.SBR \
		T3COMM.SBR \
		T3SURRGT.SBR


CLNTW_DEP = $(ILUHOME)\include\winio.h \
	$(ILUHOME)\include\winiodef.h \
	$(ILUHOME)\examples\test1\t1hdr.h \
	$(ILUHOME)\include\iluchdrs.h \
	$(ILUHOME)\include\iluxport.h \
	$(ILUHOME)\include\iluerror.h \
	$(ILUHOME)\include\ilubasic.h \
	$(ILUHOME)\examples\test1\t2hdr.h \
	$(ILUHOME)\examples\test1\t3hdr.h


T1COMM_DEP = $(ILUHOME)\examples\test1\t1hdr.h \
	$(ILUHOME)\include\iluchdrs.h \
	$(ILUHOME)\include\iluxport.h \
	$(ILUHOME)\include\iluerror.h \
	$(ILUHOME)\include\ilubasic.h


T1SURRGT_DEP = $(ILUHOME)\examples\test1\t1hdr.h \
	$(ILUHOME)\include\iluchdrs.h \
	$(ILUHOME)\include\iluxport.h \
	$(ILUHOME)\include\iluerror.h \
	$(ILUHOME)\include\ilubasic.h


T2COMM_DEP = $(ILUHOME)\examples\test1\t2hdr.h \
	$(ILUHOME)\examples\test1\t1hdr.h \
	$(ILUHOME)\include\iluchdrs.h


T2SURRGT_DEP = $(ILUHOME)\examples\test1\t2hdr.h \
	$(ILUHOME)\examples\test1\t1hdr.h \
	$(ILUHOME)\include\iluchdrs.h


T3COMM_DEP = $(ILUHOME)\examples\test1\t3hdr.h \
	$(ILUHOME)\examples\test1\t1hdr.h \
	$(ILUHOME)\examples\test1\t2hdr.h \
	$(ILUHOME)\include\iluchdrs.h


T3SURRGT_DEP = $(ILUHOME)\examples\test1\t3hdr.h \
	$(ILUHOME)\examples\test1\t1hdr.h \
	$(ILUHOME)\examples\test1\t2hdr.h \
	$(ILUHOME)\include\iluchdrs.h


CLNTW16_RCDEP = $(ILUHOME)\examples\test1\clntw.ico \
	$(ILUHOME)\examples\test1\clnconsl.ico


all:	$(PROJ).EXE

CLNTW.OBJ:	CLNTW.C $(CLNTW_DEP)
	$(CC) $(CFLAGS) $(CCREATEPCHFLAG) /c CLNTW.C

T1COMM.OBJ:	T1COMM.C $(T1COMM_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c T1COMM.C

T1SURRGT.OBJ:	T1SURRGT.C $(T1SURRGT_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c T1SURRGT.C

T2COMM.OBJ:	T2COMM.C $(T2COMM_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c T2COMM.C

T2SURRGT.OBJ:	T2SURRGT.C $(T2SURRGT_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c T2SURRGT.C

T3COMM.OBJ:	T3COMM.C $(T3COMM_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c T3COMM.C

T3SURRGT.OBJ:	T3SURRGT.C $(T3SURRGT_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c T3SURRGT.C

CLNTW16.RES:	CLNTW16.RC $(CLNTW16_RCDEP)
	$(RC) $(RCFLAGS) $(RCDEFINES) -r CLNTW16.RC


$(PROJ).EXE::	CLNTW16.RES

$(PROJ).EXE::	CLNTW.OBJ T1COMM.OBJ T1SURRGT.OBJ T2COMM.OBJ T2SURRGT.OBJ T3COMM.OBJ \
	T3SURRGT.OBJ $(OBJS_EXT) $(DEFFILE)
	echo >NUL @<<$(PROJ).CRF
CLNTW.OBJ +
T1COMM.OBJ +
T1SURRGT.OBJ +
T2COMM.OBJ +
T2SURRGT.OBJ +
T3COMM.OBJ +
T3SURRGT.OBJ +
$(OBJS_EXT)
$(PROJ).EXE
$(MAPFILE)
D:\MSVC\LIB\+
D:\MSVC\MFC\LIB\+
$(LIBS)
$(DEFFILE);
<<
	link $(LFLAGS) @$(PROJ).CRF
	$(RC) $(RESFLAGS) CLNTW16.RES $@
	@copy $(PROJ).CRF MSVC.BND

$(PROJ).EXE::	CLNTW16.RES
	if not exist MSVC.BND 	$(RC) $(RESFLAGS) CLNTW16.RES $@

run: $(PROJ).EXE
	$(PROJ) $(RUNFLAGS)


$(PROJ).BSC: $(SBRS)
	bscmake @<<
/o$@ $(SBRS)
<<
