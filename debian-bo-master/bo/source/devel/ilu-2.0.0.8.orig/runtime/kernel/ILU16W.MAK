# Microsoft Visual C++ generated build script - Do not modify

PROJ = ILU16W
DEBUG = 0
PROGTYPE = 4
CALLER = 
ARGS = 
DLLS = 
D_RCDEFINES = -d_DEBUG
R_RCDEFINES = -dNDEBUG
ORIGIN = MSVC
ORIGIN_VER = 1.00
PROJPATH = F:\ILU\SRC\RUNTIME\KERNEL\
USEMFC = 0
CC = cl
CPP = cl
CXX = cl
CCREATEPCHFLAG = 
CPPCREATEPCHFLAG = 
CUSEPCHFLAG = 
CPPUSEPCHFLAG = 
FIRSTC = ALARMUX.C   
FIRSTCPP =             
RC = rc
CFLAGS_D_LIB = /nologo /G2 /W3 /Z7 /AL /Gt0 /Gx- /Od /D "ILU_BINDING_DIRECTORY"="\"\\ilu\\bindings\"" /D "_DEBUG" /D "SUNRPC_PROTOCOL" /D "TCPIP_TRANSPORT" /D "UDPSOCKET_TRANSPORT" /D "WIN16" /D "_WINIO" /GA
CFLAGS_R_LIB = /nologo /Gs /G2 /W3 /AL /Gt0 /Gx- /Ox /D "ILU_BINDING_DIRECTORY"="\"\\ilu\\bindings\"" /D "NDEBUG" /D "SUNRPC_PROTOCOL" /D "TCPIP_TRANSPORT" /D "UDPSOCKET_TRANSPORT" /D "WIN16" /D "_WINIO" /GA
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
SBRS = ALARMUX.SBR \
		BSDMNLP.SBR \
		BSDUTILS.SBR \
		CALL.SBR \
		CONNECT.SBR \
		DEBUG.SBR \
		ERROR.SBR \
		GC.SBR \
		HASH.SBR \
		LOCKS.SBR \
		MAINLOOP.SBR \
		MEMORY.SBR \
		METHOD.SBR \
		OBJECT.SBR \
		PORT.SBR \
		PROTOCOL.SBR \
		SERVER.SBR \
		SIMPBIND.SBR \
		SUNRPC.SBR \
		TCP.SBR \
		TRANSPRT.SBR \
		TYPE.SBR \
		UDP.SBR \
		VECTOR.SBR \
		WSOCK.SBR


ALARMUX_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h


BSDMNLP_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(WSOCKHDR)


BSDUTILS_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\oscalls.h \
	$(ILUSRC)\runtime\kernel\os/win.h \
	$(WSOCKHDR)


CALL_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\protocol.h \
	$(ILUSRC)\runtime\kernel\connect.h \
	$(ILUSRC)\runtime\kernel\transprt.h \
	$(ILUSRC)\runtime\kernel\port.h \
	$(ILUSRC)\runtime\kernel\call.h \
	$(ILUSRC)\runtime\kernel\object.h \
	$(ILUSRC)\runtime\kernel\method.h \
	$(ILUSRC)\runtime\kernel\server.h \
	$(ILUSRC)\runtime\kernel\type.h \
	$(WSOCKHDR)


CONNECT_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\object.h \
	$(ILUSRC)\runtime\kernel\server.h \
	$(ILUSRC)\runtime\kernel\connect.h \
	$(ILUSRC)\runtime\kernel\transprt.h \
	$(ILUSRC)\runtime\kernel\protocol.h


DEBUG_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h


ERROR_DEP = $(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h


GC_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\object.h \
	$(ILUSRC)\runtime\kernel\call.h \
	$(ILUSRC)\runtime\kernel\server.h \
	$(ILUSRC)\runtime\kernel\type.h \
	$(ILUSRC)\runtime\kernel\vector.h


HASH_DEP = $(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h


LOCKS_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h


MAINLOOP_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\oscalls.h \
	$(ILUSRC)\runtime\kernel\os/win.h \
	$(WSOCKHDR)


MEMORY_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\oscalls.h \
	$(ILUSRC)\runtime\kernel\os/win.h


METHOD_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\method.h \
	$(ILUSRC)\runtime\kernel\type.h


OBJECT_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\object.h \
	$(ILUSRC)\runtime\kernel\server.h \
	$(ILUSRC)\runtime\kernel\type.h \
	$(ILUSRC)\runtime\kernel\protocol.h \
	$(ILUSRC)\runtime\kernel\port.h \
	$(ILUSRC)\runtime\kernel\vector.h \
	$(ILUSRC)\runtime\kernel\version.h


PORT_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\port.h \
	$(ILUSRC)\runtime\kernel\mooring.h \
	$(ILUSRC)\runtime\kernel\protocol.h \
	$(ILUSRC)\runtime\kernel\transprt.h \
	$(ILUSRC)\runtime\kernel\object.h \
	$(ILUSRC)\runtime\kernel\server.h


PROTOCOL_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\transprt.h \
	$(ILUSRC)\runtime\kernel\protocol.h


SERVER_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\server.h \
	$(ILUSRC)\runtime\kernel\object.h \
	$(ILUSRC)\runtime\kernel\type.h \
	$(ILUSRC)\runtime\kernel\connect.h \
	$(ILUSRC)\runtime\kernel\port.h


SIMPBIND_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\object.h \
	$(ILUSRC)\runtime\kernel\oscalls.h \
	$(ILUSRC)\runtime\kernel\os/win.h


SUNRPC_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\packet.h \
	$(ILUSRC)\runtime\kernel\sunrpc.h \
	$(ILUSRC)\runtime\kernel\call.h \
	$(ILUSRC)\runtime\kernel\protocol.h \
	$(ILUSRC)\runtime\kernel\connect.h \
	$(ILUSRC)\runtime\kernel\transprt.h \
	$(ILUSRC)\runtime\kernel\port.h \
	$(ILUSRC)\runtime\kernel\object.h \
	$(ILUSRC)\runtime\kernel\type.h \
	$(ILUSRC)\runtime\kernel\method.h \
	$(ILUSRC)\runtime\kernel\mooring.h


TCP_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\transprt.h \
	$(ILUSRC)\runtime\kernel\mooring.h \
	$(ILUSRC)\runtime\kernel\socket.h \
	$(WSOCKHDR) \
	$(ILUSRC)\runtime\kernel\oscalls.h \
	$(ILUSRC)\runtime\kernel\os/win.h


TRANSPRT_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\transprt.h


TYPE_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\object.h \
	$(ILUSRC)\runtime\kernel\server.h \
	$(ILUSRC)\runtime\kernel\type.h \
	$(ILUSRC)\runtime\kernel\vector.h


UDP_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\transprt.h \
	$(ILUSRC)\runtime\kernel\mooring.h \
	$(ILUSRC)\runtime\kernel\socket.h \
	$(WSOCKHDR) \
	$(ILUSRC)\runtime\kernel\oscalls.h \
	$(ILUSRC)\runtime\kernel\os/win.h


VECTOR_DEP = $(ILUSRC)\runtime\kernel\iluntrnl.h \
	$(ILUSRC)\runtime\kernel\iluxport.h \
	$(ILUSRC)\runtime\kernel\iluerror.h \
	$(ILUSRC)\runtime\kernel\ilubasic.h \
	$(ILUSRC)\winio\winiodef.h \
	$(ILUSRC)\runtime\kernel\iluerrs.h \
	$(ILUSRC)\runtime\kernel\iludebug.h \
	$(ILUSRC)\runtime\kernel\iluhash.h \
	$(ILUSRC)\runtime\kernel\vector.h


WSOCK_DEP = $(WSOCKHDR)


all:	$(PROJ).LIB

ALARMUX.OBJ:	ALARMUX.C $(ALARMUX_DEP)
	$(CC) $(CFLAGS) $(CCREATEPCHFLAG) /c ALARMUX.C

BSDMNLP.OBJ:	BSDMNLP.C $(BSDMNLP_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c BSDMNLP.C

BSDUTILS.OBJ:	BSDUTILS.C $(BSDUTILS_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c BSDUTILS.C

CALL.OBJ:	CALL.C $(CALL_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c CALL.C

CONNECT.OBJ:	CONNECT.C $(CONNECT_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c CONNECT.C

DEBUG.OBJ:	DEBUG.C $(DEBUG_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c DEBUG.C

ERROR.OBJ:	ERROR.C $(ERROR_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c ERROR.C

GC.OBJ:	GC.C $(GC_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c GC.C

HASH.OBJ:	HASH.C $(HASH_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c HASH.C

LOCKS.OBJ:	LOCKS.C $(LOCKS_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LOCKS.C

MAINLOOP.OBJ:	MAINLOOP.C $(MAINLOOP_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c MAINLOOP.C

MEMORY.OBJ:	MEMORY.C $(MEMORY_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c MEMORY.C

METHOD.OBJ:	METHOD.C $(METHOD_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c METHOD.C

OBJECT.OBJ:	OBJECT.C $(OBJECT_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c OBJECT.C

PORT.OBJ:	PORT.C $(PORT_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c PORT.C

PROTOCOL.OBJ:	PROTOCOL.C $(PROTOCOL_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c PROTOCOL.C

SERVER.OBJ:	SERVER.C $(SERVER_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c SERVER.C

SIMPBIND.OBJ:	SIMPBIND.C $(SIMPBIND_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c SIMPBIND.C

SUNRPC.OBJ:	SUNRPC.C $(SUNRPC_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c SUNRPC.C

TCP.OBJ:	TCP.C $(TCP_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c TCP.C

TRANSPRT.OBJ:	TRANSPRT.C $(TRANSPRT_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c TRANSPRT.C

TYPE.OBJ:	TYPE.C $(TYPE_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c TYPE.C

UDP.OBJ:	UDP.C $(UDP_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c UDP.C

VECTOR.OBJ:	VECTOR.C $(VECTOR_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c VECTOR.C

WSOCK.OBJ:	WSOCK.C $(WSOCK_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c WSOCK.C

$(PROJ).LIB::	ALARMUX.OBJ BSDMNLP.OBJ BSDUTILS.OBJ CALL.OBJ CONNECT.OBJ DEBUG.OBJ \
	ERROR.OBJ GC.OBJ HASH.OBJ LOCKS.OBJ MAINLOOP.OBJ MEMORY.OBJ METHOD.OBJ OBJECT.OBJ \
	PORT.OBJ PROTOCOL.OBJ SERVER.OBJ SIMPBIND.OBJ SUNRPC.OBJ TCP.OBJ TRANSPRT.OBJ TYPE.OBJ \
	UDP.OBJ VECTOR.OBJ WSOCK.OBJ $(OBJS_EXT)
	echo >NUL @<<$(PROJ).CRF
$@ /PAGESIZE:64
y
+ALARMUX.OBJ &
+BSDMNLP.OBJ &
+BSDUTILS.OBJ &
+CALL.OBJ &
+CONNECT.OBJ &
+DEBUG.OBJ &
+ERROR.OBJ &
+GC.OBJ &
+HASH.OBJ &
+LOCKS.OBJ &
+MAINLOOP.OBJ &
+MEMORY.OBJ &
+METHOD.OBJ &
+OBJECT.OBJ &
+PORT.OBJ &
+PROTOCOL.OBJ &
+SERVER.OBJ &
+SIMPBIND.OBJ &
+SUNRPC.OBJ &
+TCP.OBJ &
+TRANSPRT.OBJ &
+TYPE.OBJ &
+UDP.OBJ &
+VECTOR.OBJ &
+WSOCK.OBJ &
;
<<
	if exist $@ del $@
	lib @$(PROJ).CRF

$(PROJ).BSC: $(SBRS)
	bscmake @<<
/o$@ $(SBRS)
<<
