!==========================================================================
! MMS description file for VMS-fsp V2.7.0                      May 17, 1993
!==========================================================================
!
! 27 Dec 92  Original by S.A.Pechler, <S.A.Pechler@bdk.tue.nl>
!  3 Jan 93  Modified for version V2.6.5
! 28 Jan 93  Modified for version V2.6.5jt.3
! 11 Feb 93  Modified for version V2.6.5jt.7
!  5 Mar 93  Modified for version V2.6.5jt.8
! 17 May 93  Modified for version V2.7.0

! To build the VMS-FSP clients
! ----------------------------
! For UCX:      mms /MACRO=(UCX=1)
! For Multinet: mms /MACRO=(MUL=1)
! For Multinet without ucx$ipc library: mms /MACRO=(LIB=1)
! (One-time users will find it easier to use the MAKECLIENT.COM command
! file, which generates the client software.  Just type "@MAKECLIENT" from
! within the [.fsp.vms] directory .)

! To build the VMS-FSP server
! ---------------------------
! For UCX:	mms server /MACRO=(UCX=1)
! For Multinet: mms server /MACRO=(MUL=1)
! For Multinet without ucx$ipc library: mms server /MACRO=(LIB=1)
! (One-time users will find it easier to use the MAKESERVER.COM command
! file, which generates the server software.  Just type "@MAKESERVER" from
! within the [.fsp.vms] directory.)

! To build the fsp-server without shared libraries,
!	mms noshare

! To delete unnecessary .OBJ files,
!	mms clean

! There's no mms-makefile for the merge files, use [.vms]MAKEMERGE.COM
!  instead.

!============================================================================
! This is where you want to install the binary (only when using the
! 'install' option).
!
BIN=pool:[bdaasp]

!============================================================================
! The following variable defines the compiler to use in your compilation.
!
CC = cc
!CC=gcc

!============================================================================
! Flags for the compiler.
!
.IFDEF DEBUG
CFLAGS = /def=(DEBUG=1) \
         /include=[.include.vms] /object=$@
.ELSE
CFLAGS = /nodebug /include=[.include.vms] /object=$@
.ENDIF

!============================================================================
!  Linker command
!
LD = link

.IFDEF DEBUG
LDFLAGS = 
.ELSE
LDFLAGS = /notrace
.ENDIF

!============================================================================
!   Linker libraries
!
.IFDEF UCX
LDLIB = [.vms]ucxshare.opt
.ELSE
.IFDEF LIB
LDLIB = [.vms]libshare.opt
.ELSE
LDLIB = [.vms]mulshare.opt
.ENDIF
.ENDIF

!can't do 3 recursive .IFDEF's
.IFDEF TGV
LDLIB = [.vms]tgvshare.opt
.ENDIF

EXE =
O = .obj;

SERVER     = fspd.exe

CLIENTS    = flscmd.exe, fcdcmd.exe, fgetcmd.exe, frmcmd.exe, frmdircmd.exe,\
             fprocmd.exe, fmkdir.exe, fput.exe, fver.exe, fcatcmd.exe,\
             fgrabcmd.exe, fducmd.exe, ffindcmd.exe, fhostcmd.exe

ICLIENTS   = $(BIN)flscmd.exe, $(BIN)fcdcmd.exe, $(BIN)fgetcmd.exe, \
             $(BIN)frmcmd.exe, $(BIN)frmdircmd.exe, $(BIN)fprocmd.exe, \
             $(BIN)fmkdir.exe, $(BIN)fput.exe, $(BIN)fver.exe, \
             $(BIN)fcatcmd.exe, $(BIN)fgrabcmd.exe

MISC_OBJ    = [.vms_src]miscvms$(O),[.vms_src]convpath$(O)

SERVER_OBJ = [.server]main$(O),[.server]lib$(O),[.server]host$(O),\
	     [.server]conf$(O),[.server]file$(O),[.server]filecache$(O),\
	     [.common]udp_io$(O),[.common]random$(O),[.common]strdup$(O),\
	     [.vms_src]vmsreaddir$(O),[.vms_src]vmsmain$(O),$(MISC_OBJ)

CLIENT_OBJ = [.client]lib$(O),[.client]util$(O),[.client]lock$(O),\
	     [.common]udp_io$(O),[.vms_src]vmsmain$(O)

GLOB_OBJ   = [.bsd_src]glob$(O)

UTIL_LS_OBJ = [.bsd_src]cmp$(O),[.bsd_src]ls$(O),[.bsd_src]print$(O), \
              [.bsd_src]util$(O),[.vms_src]getopt$(O),$(GLOB_OBJ)

UTIL_PROC_OBJ = [.bsd_src]find$(O),[.bsd_src]option$(O),[.bsd_src]operator$(O),\
		[.bsd_src]function$(O),[.bsd_src]fnmatch$(O)

GETOPT_OBJ = [.vms_src]getopt$(O)

LDFLAGS2 =

default	:	init $(CLIENTS)
	@	!	Do nothing.
install :	init $(ICLIENTS)
	@	!	Do nothing.
server  :	init $(SERVER)
	@	!	Do nothing.

init     :      !descrip.mms
.IFDEF UCX
	    $def/nolog c$include [.include]
            $def/nolog sys sys$library
            $def/nolog netinet sys$library
.ELSE
	    $def/nolog c$include [.include]
            $def/nolog sys multinet_root:[multinet.include.sys]
            $def/nolog netinet multinet_root:[multinet.include.netinet]
.ENDIF

fspd.exe :	$(SERVER_OBJ)
	$(LD) /exe=FSPD.EXE $(LDFLAGS) $(SERVER_OBJ), $(LDLIB)/options

flscmd.exe :	[.clients]flscmd$(O), $(CLIENT_OBJ), $(UTIL_LS_OBJ)
	$(LD) $(LDFLAGS) [.clients]flscmd$(O),$(CLIENT_OBJ),$(UTIL_LS_OBJ),\
              $(LDLIB)/options

fcdcmd.exe :	[.clients]fcdcmd$(O), $(CLIENT_OBJ), $(GLOB_OBJ)
	$(LD) $(LDFLAGS) [.clients]fcdcmd$(O),$(CLIENT_OBJ),$(GLOB_OBJ),\
              $(LDLIB)/options

fgetcmd.exe :	[.clients]fgetcmd$(O), $(CLIENT_OBJ), $(MISC_OBJ),\
		$(GETOPT_OBJ), $(GLOB_OBJ)
	$(LD) $(LDFLAGS) [.clients]fgetcmd$(O),$(CLIENT_OBJ),$(MISC_OBJ),\
              $(GETOPT_OBJ),$(GLOB_OBJ),$(LDLIB)/options

frmcmd.exe :	[.clients]frmcmd$(O), $(CLIENT_OBJ), $(GLOB_OBJ)
	$(LD) $(LDFLAGS) [.clients]frmcmd$(O), $(CLIENT_OBJ), $(GLOB_OBJ), \
              $(LDLIB)/options

frmdircmd.exe :	[.clients]frmdircmd$(O), $(CLIENT_OBJ), $(GLOB_OBJ)
	$(LD) $(LDFLAGS) [.clients]frmdircmd$(O), $(CLIENT_OBJ), $(GLOB_OBJ), \
                $(LDLIB)/options

fprocmd.exe :	[.clients]fprocmd$(O), $(CLIENT_OBJ), $(GLOB_OBJ)
	$(LD) $(LDFLAGS) [.clients]fprocmd$(O), $(CLIENT_OBJ), $(GLOB_OBJ), \
                $(LDLIB)/options

fmkdir.exe :	[.clients]fmkdir$(O), $(CLIENT_OBJ)
	$(LD) $(LDFLAGS) [.clients]fmkdir$(O), $(CLIENT_OBJ), \
                $(LDLIB)/options

fput.exe :	[.clients]fput$(O), $(CLIENT_OBJ)
	$(LD) $(LDFLAGS) [.clients]fput$(O), $(CLIENT_OBJ), \
                $(LDLIB)/options

fver.exe :	[.clients]fver$(O), $(CLIENT_OBJ)
	$(LD) $(LDFLAGS) [.clients]fver$(O), $(CLIENT_OBJ), \
                $(LDLIB)/options

fcatcmd.exe :	[.clients]fcatcmd$(O), $(CLIENT_OBJ), $(GLOB_OBJ),\
		$(GETOPT_OBJ)
	$(LD) $(LDFLAGS) [.clients]fcatcmd$(O), $(CLIENT_OBJ), $(GLOB_OBJ), \
                $(GETOPT_OBJ),$(LDLIB)/options

fgrabcmd.exe :	[.clients]fgrabcmd$(O), $(CLIENT_OBJ), $(MISC_OBJ),\
		$(GETOPT_OBJ),$(GLOB_OBJ)
	$(LD) $(LDFLAGS) [.clients]fgrabcmd$(O),$(CLIENT_OBJ),$(MISC_OBJ),\
	      $(GETOPT_OBJ),$(GLOB_OBJ),$(LDLIB)/options

fducmd.exe   :  [.clients]fducmd$(O),$(CLIENT_OBJ),$(GETOPT_OBJ),$(GLOB_OBJ)
	$(LD) $(LDFLAGS) [.clients]fducmd$(O),$(CLIENT_OBJ),$(GETOPT_OBJ),\
	      $(GLOB_OBJ),$(LDLIB)/options

ffindcmd.exe :  [.clients]ffindcmd$(O),$(CLIENT_OBJ),$(GLOB_OBJ),\
		$(UTIL_PROC_OBJ),$(MISC_OBJ)
	$(LD) $(LDFLAGS) [.clients]ffindcmd$(O),$(CLIENT_OBJ),$(GLOB_OBJ),\
		$(UTIL_PROC_OBJ),$(MISC_OBJ),$(LDLIB)/options

fhostcmd.exe :  [.clients]fhostcmd$(O),[.vms_src]vmsmain$(O),$(GETOPT_OBJ),\
		$(MISC_OBJ)
	$(LD) $(LDFLAGS) [.clients]fhostcmd$(O),[.vms_src]vmsmain$(O),\
		$(GETOPT_OBJ),$(MISC_OBJ),$(LDLIB)/options

noshare :	$(SERVER_OBJ)
	$(LD) $(LDFLAGS) $(SERVER_OBJ), \
		sys$library:vaxcrtl.olb/library $(LDFLAGS2)

clean :
	delete $(SERVER_OBJ), $(CLIENT_OBJ), $(GLOB_OBJ), $(UTIL_LS_OBJ)
        ! you may want to change this to 'delete *.obj;*'

[.server]lib$(O)  :   [.server]lib.c [.include]server_def.h
		$(CC) $(CFLAGS) [.server]lib.c
.IFDEF DEBUG
		$(CC) $(CFLAGS) /def=(DIRENT=1,DEBUG=1) [.server]file.c
.ENDIF
[.client]util$(O) :  [.client]util.c [.include]client_def.h
		$(CC) $(CFLAGS) [.client]util.c

[.bsd_src]util$(O) : [.bsd_src]util.c [.include.vms]tweak.h
		$(CC) $(CFLAGS) [.bsd_src]util.c

[.client]lib$(O) :   [.client]lib.c [.include]client_def.h
		$(CC) $(CFLAGS) [.client]lib.c

!all other sources have default compilation rules.

