# Microsoft Visual C++ generated build script - Do not modify

PROJ = CPLNTW16
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
FIRSTC =             
FIRSTCPP = T1CLICOM.CPP
RC = rc
CFLAGS_D_WEXE = /nologo /G2 /W3 /Zi /AL /YX /Od /D "_DEBUG" /D "WIN16" /D "_WINIO" /I $(ILUHOME)\include /GA /Fd"CPLNTW16.PDB" /Fp"CPLNTW16.PCH"
CFLAGS_R_WEXE = /nologo /W3 /AL /YX /Ox /D "NDEBUG" /D "WIN16" /D "_WINIO" /I $(ILUHOME)\include /GA /Fp"CPLNTW16.PCH"
LFLAGS_D_WEXE = /NOLOGO /NOD /PACKC:61440 /STACK:10240 /ALIGN:16 /ONERROR:NOEXE /CO  
LFLAGS_R_WEXE = /NOLOGO /NOD /PACKC:61440 /STACK:10240 /ALIGN:16 /ONERROR:NOEXE  
LIBS_D_WEXE = $(ILUHOME)\lib\ilucp16w.lib $(ILUHOME)\lib\ilu16w.lib $(ILUHOME)\lib\winio16w.lib $(WSOCKLIB) oldnames libw llibcew commdlg.lib olecli.lib olesvr.lib shell.lib 
LIBS_R_WEXE = $(ILUHOME)\lib\ilucp16w.lib $(ILUHOME)\lib\ilu16w.lib $(ILUHOME)\lib\winio16w.lib $(WSOCKLIB) oldnames libw llibcew commdlg.lib olecli.lib olesvr.lib shell.lib 
RCFLAGS = /nologo
RESFLAGS = /nologo
RUNFLAGS = 
DEFFILE = CPLNTW16.DEF
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
SBRS = T1CLICOM.SBR \
		T2CLICOM.SBR \
		T3CLICOM.SBR \
		CPPCLNTW.SBR


T1CLICOM_DEP = $(ILUHOME)\include\ilu.h \
	$(ILUHOME)\include\iluxport.h \
	$(ILUHOME)\include\iluerror.h \
	$(ILUHOME)\include\ilubasic.h \
	$(ILUHOME)\examples\test1\t1hdr.hh


T2CLICOM_DEP = $(ILUHOME)\include\ilu.h \
	$(ILUHOME)\include\iluxport.h \
	$(ILUHOME)\include\iluerror.h \
	$(ILUHOME)\include\ilubasic.h \
	$(ILUHOME)\examples\test1\t2hdr.hh


T3CLICOM_DEP = $(ILUHOME)\include\ilu.h \
	$(ILUHOME)\include\iluxport.h \
	$(ILUHOME)\include\iluerror.h \
	$(ILUHOME)\include\ilubasic.h \
	$(ILUHOME)\examples\test1\t3hdr.hh


CPPCLNTW_DEP = $(ILUHOME)\include\winio.h \
	$(ILUHOME)\include\winiodef.h \
	$(ILUHOME)\examples\test1\t1hdr.hh \
	$(ILUHOME)\examples\test1\t2hdr.hh \
	$(ILUHOME)\examples\test1\t3hdr.hh


CLNTW16_RCDEP = $(ILUHOME)\examples\test1\clntw.ico \
	$(ILUHOME)\examples\test1\clnconsl.ico


all:	$(PROJ).EXE

T1CLICOM.OBJ:	T1CLICOM.CPP $(T1CLICOM_DEP)
	$(CPP) $(CFLAGS) $(CPPCREATEPCHFLAG) /c T1CLICOM.CPP

T2CLICOM.OBJ:	T2CLICOM.CPP $(T2CLICOM_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c T2CLICOM.CPP

T3CLICOM.OBJ:	T3CLICOM.CPP $(T3CLICOM_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c T3CLICOM.CPP

CPPCLNTW.OBJ:	CPPCLNTW.CPP $(CPPCLNTW_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c CPPCLNTW.CPP

CLNTW16.RES:	CLNTW16.RC $(CLNTW16_RCDEP)
	$(RC) $(RCFLAGS) $(RCDEFINES) -r CLNTW16.RC


$(PROJ).EXE::	CLNTW16.RES

$(PROJ).EXE::	T1CLICOM.OBJ T2CLICOM.OBJ T3CLICOM.OBJ CPPCLNTW.OBJ $(OBJS_EXT) $(DEFFILE)
	echo >NUL @<<$(PROJ).CRF
T1CLICOM.OBJ +
T2CLICOM.OBJ +
T3CLICOM.OBJ +
CPPCLNTW.OBJ +
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
