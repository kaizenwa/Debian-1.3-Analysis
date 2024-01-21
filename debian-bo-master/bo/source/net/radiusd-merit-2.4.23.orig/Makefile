#
#  [C] The Regents of the University of Michigan and Merit Network, Inc. 1992,
#  1993, 1994, 1995, 1996 All Rights Reserved
#  
# Makefile for RADIUS - Remote Authentication Dial In User Service
#	$Id: Makefile,v 1.92 1996/07/24 19:05:24 web Exp $
#
# RADIUS target versions:
#
#	radiusd:  Default is multitasking server w/o Kerberos or MNET
#		  authentication.  The client, authfile, and users (optionally)
#		  files are stored in memory.  Includes RADIUS-RADIUS relay
#		  authentication capability and accounting.
#	mkerb:    The default plus MIT Kerberos.
#	akerb:    The default plus AFS Kerberos.  Must be properly licensed.
#	dbm:	  Same as default, but with dbm(3) users database lookup.
#	mnet:	  Merit/MichNet version.  Includes multitasking, AFS Kerberos,
#		  MIT Kerberos and strange MichNet authentication.
#	kchap:	  Same as default, but with special Kerberos CHAP experimental
#		  protocol capability.  Runs on Kerberos server machine.
#	tacacs:	  Same as default, but with special TACACS protocol support.
#		  Requires mods to TACACS authentication server.  See the
#		  INSTALL file.
#	utils:	  Generates additional utility routines.
#
#
# Various client modules:
#
#   radcheck - Polls a RADIUS server to see if it's still alive.
#   radpwtst - Generates various kinds of authentication requests for
#		testing a RADIUS server.
#   sendserver - A utility function to issue requests and process replies.
#		 Used by radcheck, radpwtst, and the MINOS daemon.
#   builddbm - Generate routine to build dbm(3) database from "users" file.
#   radpass - Sets password for individual user id entries in "users" file.
#
#
# Add -DNOSHADOW to DEFS if you don't have Shadow Passwords (no shadow.h file).
# Add -DHAUGHSHADOW to your DEFS macro if you _do_ have PD Shadow Passwords.

# Where the sources (*.h and *.c) live.
SRC		= ./src
# Where the objects are compiled.
OBJ		= ./src
# Where the executables are linked.
BIN		= ./src
# Where the man pages live.
MAN		= ./man
# Where the extra documentation lives.
DOC		= ./doc
# Where the configuration files live.
RADDB		= ./raddb
# Some installation stuff.
DAEMON_INSDIR	= /usr/private/etc
BIN_INSDIR	= /usr/private/etc
MAN_INSDIR	= /usr/local/man
RADDB_INSDIR	= /usr/private/etc/raddb
RADACCT_INSDIR	= /usr/private/etc/radacct
# The server does not need to be owned by root, unless some shadow password
# scheme needs it.  You might create a user id "radius" for just this purpose.
# The O G and M macros are for AIX, OSF/1 and HP-UX, see below.
O		= -o
G		= -g
M		= -m
RADOWN		= root
RADGRP		= bin

# Define COMPRESS to hold the name of your favourite compress program:
#COMPRESS	= -DRADIUS_COMPRESS=\"/usr/local/bin/gzip\"

# Define SRV to hold any combination of server names you'd like to override:
#SRV3	  = -DDEFAULT_TACACS_SERVER=\"vms.dns.name\"
#SRV2	  = -DDEFAULT_RADIUS_SERVER=\"radius.server.dns.name\"
#SRV1	  = -DDEFAULT_SERVER=\"your.server.dns.name\"
#SRV	  = $(SRV1) $(SRV2) $(SRV3)

# Define DIRS to hold any combination of directory names you'd like to override:
#DIRS2	  = -DDEFAULT_DIR=\"../raddb\" -DDEFAULT_DIR2=\"$(RADDB_INSDIR)\"
#DIRS1	  = -DRADIUS_DIR=\"$(RADDB_INSDIR)\" -DRADACCT_DIR=\"$(RADACCT_INSDIR)\"
#DIRS	  = $(DIRS1) $(DIRS2)

# Define CHK_SHELLS to enable /etc/shells checking:
CHK_SHELLS	= -DCHK_SHELLS

# Define STATE_IN_REQUEST to include code to detect Access-Challenge requests:
#STATE_IN_REQUEST	= -DSTATE_IN_REQUEST

STUFF	  = $(DIRS) $(SRV) $(COMPRESS) $(CHK_SHELLS) $(STATE_IN_REQUEST)

# Define this to point to your AFS Kerberos source directory.
# (used as the root of the AFS source directory for the stringtokey() function)
# Refer to the comments before the sed make target near line 490 in this file.
#AFS	  = ./src

# Define SOME_ASCEND if you want limited ASCEND support.
SOME_ASCEND	= -DASCEND

# Leave these Merit specific macros defined unless you've studied the code...

MERIT_GRAND	= -DMERIT_GRANDFATHER

MERIT	  = $(MERIT_GRAND) $(SOME_ASCEND)

# What we are trying to build.  There is no need to change this line...
SERVER	  = radiusd

# Select one of the targets below for the RADIUS version you want to build:

#
# radiusd -- the default
#
#--------------------------------------------------------------------------
DEFS	  = -DHAVE_SETVBUF -DNOSHADOW $(MERIT) $(STUFF)
RADLIBS	  =
INCS	  =

#
# mkerb
#
#--------------------------------------------------------------------------
#DEFS	  = -DHAVE_SETVBUF -DM_KERB -DNOSHADOW $(MERIT) $(STUFF)
#RADLIBS	  = /usr/kerberos/lib/libkrb.a /usr/kerberos/lib/libdes.a
#INCS	  = -I/usr/kerberos/include

#
# akerb
#
#--------------------------------------------------------------------------
#DEFS	  = -DHAVE_SETVBUF -DA_KERB -DNOSHADOW $(MERIT) $(STUFF)
#RADLIBS	  = /usr/kerberos/lib/libkrb.a /usr/kerberos/lib/libdes.a
#INCS	  = -I/usr/kerberos/include

#
# mnet
#
#--------------------------------------------------------------------------
#MINOSD	  = /usr/local/mnetauth/minosd
#DEFS	  = -DMINOS -DCONFIG_FILE=\"/etc/minostab\" -DM_KERB -DA_KERB \
#	-DHAVE_SETVBUF -DTACACS -DNOSHADOW $(MERIT) $(STUFF)
#RADLIBS	  = /usr/kerberos/lib/libkrb.a /usr/kerberos/lib/libdes.a \
#	$(MINOSD)/minosauth.a /usr/local/mnetauth/kernel/libat.a
#INCS	  = -I/usr/kerberos/include -I$(MINOSD)

#
# kchap
#
#--------------------------------------------------------------------------
#DEFS	  = -DHAVE_SETVBUF -DKCHAP -DNOSHADOW $(MERIT) $(STUFF)
#RADLIBS	  = /usr/kerberos/lib/libkrb.a /usr/kerberos/lib/libdes.a
#INCS	  = -I/usr/kerberos/include

#
# tacacs
#
#--------------------------------------------------------------------------
#DEFS	  = -DHAVE_SETVBUF -DTACACS -DNOSHADOW $(MERIT) $(STUFF)
#RADLIBS	  =
#INCS	  =

#
# dbm (pick one of the two DBM macros below and maybe add -ldbm to LIBS)
#
#--------------------------------------------------------------------------
#DBM	  = -DUSE_DBM
#DBM	  = -DUSE_NDBM
#DEFS	  = -DHAVE_SETVBUF $(DBM) -DNOSHADOW $(MERIT) $(STUFF)
#RADLIBS	  =
#INCS	  =
#EXTRA	  = $(OBJ)/builddbm.o
#DBMUTIL	  = $(BIN)/builddbm

# You must un-comment one, and only one, of the above sections!
# Now, skip down to the Operating System section.

RAD_INCS  = $(SRC)/krb_conf.h $(SRC)/md5.h $(SRC)/prot.h $(SRC)/radius.h \
	$(SRC)/tacacs.h $(SRC)/conf.h

RAD_OBJS  = $(OBJ)/radiusd.o $(OBJ)/authenticate.o $(OBJ)/rad.accounting.o    \
	$(OBJ)/rad.kchap.o $(OBJ)/rad.kerberos.o $(OBJ)/rad.mnet.o            \
	$(OBJ)/rad.tacacs.o $(OBJ)/afs_stringtokey.o $(OBJ)/mit_stringtokey.o \
	$(OBJ)/id_to_key.o $(OBJ)/krb_get_in_tkt.o $(OBJ)/passchange.o        \
	$(OBJ)/version.o $(OBJ)/fsm.o

LIB_OBJS  = $(OBJ)/dict.o $(OBJ)/funcs.o $(OBJ)/md5.o $(OBJ)/sendserver.o     \
	$(OBJ)/users.o $(OBJ)/util.o

UTIL_OBJS = $(OBJ)/radcheck.o $(OBJ)/radpwtst.o $(OBJ)/radpass.o              \
	$(OBJ)/dnscheck.o

RAD_SRCS  = $(SRC)/radiusd.c $(SRC)/authenticate.c $(SRC)/rad.accounting.c    \
	$(SRC)/rad.kchap.c $(SRC)/rad.kerberos.c $(SRC)/rad.mnet.c            \
	$(SRC)/rad.tacacs.c $(SRC)/afs_stringtokey.c $(SRC)/mit_stringtokey.c \
	$(SRC)/id_to_key.c $(SRC)/krb_get_in_tkt.c $(SRC)/passchange.c        \
	$(SRC)/version.c $(SRC)/fsm.c                                         \
	$(SRC)/dict.c $(SRC)/funcs.c $(SRC)/md5.c $(SRC)/sendserver.c         \
	$(SRC)/users.c $(SRC)/util.c

RADCHECK  = $(BIN)/radcheck
RADPWTST  = $(BIN)/radpwtst
RADPASS   = $(BIN)/radpass
DNSCHECK  = $(BIN)/dnscheck

MOSTUTIL  = $(RADCHECK) $(RADPWTST) $(RADPASS) $(DBMUTIL) $(DNSCHECK)
UTILS	  = $(MOSTUTIL)

CONFIGS	  = $(RADDB)/authfile $(RADDB)/clients $(RADDB)/dictionary $(RADDB)/users

RADIUS	  = $(BIN)/radiusd

RAD_LIB	  = $(OBJ)/radlib.a

#
# Operating Systems Section:
#
# Select only one of the Operating Systems sections below for this build:

#
# SunOS 4.1.3 (dbm.h redefines NULL, which will generate warnings if USE_*DBM)
#
#--------------------------------------------------------------------------

CC	  = cc
CFLAGS	  = -g $(DEFS) $(INCS)
LDFLAGS	  =
LIBS	  =
RANLIB	  = ranlib
INSTALL	  = /bin/install

#
# Solaris 2.x
#
# Uncomment one and only one of the CC, CFLAGS and NETLIBS/LIBS entries.
#
# With ucblib, to run the server do the following or use the -R option in LIBS:
#	setenv LD_LIBRARY_PATH /usr/ucblib
#	./radiusd &
#
# Be sure to uncomment the "ranlib" and "install" macros 17 lines below.
#
#--------------------------------------------------------------------------

# With GNU compiler (recommended)
#CC	  = gcc
#CFLAGS	  = -g -Dsys5 -traditional $(DEFS) -I/usr/ucbinclude $(INCS)
# With Sun compiler (builds but not tested in production use)
#CC	  = /opt/SUNWspro/bin/cc -D__svr4__ -D__sun__
#CFLAGS	  = -O $(DEFS) -I/usr/ucbinclude $(INCS)
#LDFLAGS	  =
# With BIND 4.9.3 resolver libraries and headers
#NETLIBS	  = -lnsl -lsocket /usr/lib/libresolv.a -l44bsd
# With Sun's resolver code
#NETLIBS	  = -lnsl -lsocket -lresolv
#LIBS	  = $(NETLIBS) -R /usr/ucblib -L/usr/ucblib -lucb
#LIBS	  = -lnsl -lsocket -R /usr/ucblib -L/usr/ucblib -lucb
#RANLIB	  = /bin/true
#INSTALL	  = /usr/ucb/install

#
# Linux 1.2.3 (only use -lshadow if you use shadow passwords and have that lib)
#
#--------------------------------------------------------------------------

#CC	  = cc
#CFLAGS	  = -g $(DEFS) $(INCS)
#LDFLAGS	  =
# pick one
#LIBS	  = -lshadow
#LIBS	  = 
#RANLIB	  = ranlib
#INSTALL	  = /usr/bin/install

#
# HP-UX 9.05 or 10.x (the "O" macro is for HP version of install, -lsec opt.)
#
#--------------------------------------------------------------------------

#CC	  = cc
#CFLAGS	  = -O -D__hpux $(DEFS) $(INCS)
#CC	  = gcc
#CFLAGS	  = -g -traditional -D__hpux $(DEFS) $(INCS)
#LDFLAGS	  =
#LIBS	  = -lsec
#RANLIB	  = /bin/ranlib
#INSTALL	  = /etc/install
#O	  = -u

#
# BSDi 1.1 and 2.0 (BSDi does not have dbm(3) see the USE_*DBM macros above)
#
# On 1.0 do not define NOSHADOW and remove #include <machine/inline.h> (conf.h)
#
#--------------------------------------------------------------------------

#CC	  = cc
#CFLAGS	  = -O $(DEFS) $(INCS)
#LDFLAGS	  =
#LIBS	  = -lcompat
#MOSTUTIL  = $(RADCHECK) $(RADPWTST) $(RADPASS) $(DNSCHECK)
#RANLIB	  = ranlib
#INSTALL	  = /usr/bin/install

#
# FreeBSD (NetBSD?) (does not have dbm(3) so can't build builddbm)
#
#--------------------------------------------------------------------------

#CC	  = cc
#CFLAGS	  = -O $(DEFS) $(INCS)
#LDFLAGS	  =
#LIBS	  = -lcrypt -lcompat
#MOSTUTIL  = $(RADCHECK) $(RADPWTST) $(RADPASS) $(DNSCHECK)
#RANLIB	  = ranlib
#INSTALL	  = /usr/bin/install

#
# AIX 3.2.5 (if using xlc(1) add -D_ALL_SOURCE to CFLAGS below)
#
# The $(O), $(G) and $(M) macros below are for the AIX install program.
#
#--------------------------------------------------------------------------

#CC	  = cc
#CC	  = gcc
#CFLAGS	  = -O -Daix -DM_UNIX $(DEFS) $(INCS)
#LDFLAGS	  =
#LIBS	  = -lbsd
#RANLIB	  = /usr/bin/ranlib
#INSTALL	  = /usr/bin/install
#O		= -O
#G		= -G
#M		= -M

#
# SCO 3.2
#
# get encryption libraries from SCO in the SLS supplement "lng190"
#
#--------------------------------------------------------------------------

#CC	  = cc
#CFLAGS	  = -O $(DEFS) $(INCS)
# or
#CC	  = gcc
#CFLAGS	  = -O3 -m486 -DMAXPATHLEN=255 -DSCO $(DEFS) $(INCS)
#LDFLAGS	  =
#LIBS	  = -lsocket -lsec -lcrypt
# or
#LIBS	  = -lsocket -lsec -lcrypt -lprot -lcurses -lm -lx
#RANLIB	  = ranlib
#INSTALL	  = /usr/ucb/install

#
# Ultrix 4.3 or Ultrix 4.4
#
#--------------------------------------------------------------------------

# Add -DULTRIX_ENHANCED to CFLAGS and -lauth -lkrb to LIBS if using 
#                       Ultrix Upgrade/Enhanced security.
# 4/3/94 Jeff Marraccini <jeff@oakland.edu>

#CC	  = cc
#CFLAGS	  = -O -DRADIUS_DIR=\"/var/mnetauth/raddb\" $(DEFS) $(INCS)
#-DULTRIX_ENHANCED
#CFLAGS	  = -O -DULTRIX_ENHANCED $(DEFS) $(INCS)
#LDFLAGS	  =
#LIBS	  = -lauth -lkrb -lsecurity
#RANLIB	  = ranlib
#INSTALL	  = /usr/bin/install

#
# DEC OSF/1 (Digital Unix) (the "O" macro is for HP version of install)
#
#--------------------------------------------------------------------------

# Add -DSIA to use the SIA interface for passwd
#           authentication (for DCE or C2 security).
#
# The -taso option to cc(1) tells the Alpha to use 32 bit quantities.
# You probably don't need to use that.  The -DSIA flag and -lsecurity
# library should only be used to include support for enhanced security.
# You probably don't need -DOSF unless you need to call getprpwnam().

#CC	  = cc
#CFLAGS	  = -O -DRADIUS_DIR=\"/var/mnetauth/raddb\" -DSIA -taso $(DEFS) $(INCS)
#CFLAGS	  = -O -DOSF -DSIA $(DEFS) $(INCS)
#LDFLAGS	  =
#LIBS	  = -lsecurity
#RANLIB	  = ranlib
#INSTALL	  = /bin/install
#O	  = -u

#
# SGI IRIX 5.4.3
#
#--------------------------------------------------------------------------

#CC	  = cc
#CFLAGS	  = -g -Dsysv $(DEFS) $(INCS)
#LDFLAGS	  =
#LIBS	  = 
#RANLIB	  = /bin/true
#INSTALL	  = /bin/install NOTE: Does not work - requires manual install!

#
# UnixWare before 2.0 (as BSD)
#
# Note: UnixWare 2.0 and later replace -lucb with -lcrypt
# (these later versions may not like "-o filename.o" syntax in targets below)
# (if so, move the Makefile to ./src and adjust SRC, OBJ and BIN near line 40)
#-----------------------------------------
#CC	  = /usr/ucb/cc
#CFLAGS	  = -O -Dunixware $(DEFS) -I/usr/ucbinclude $(INCS)
#LDFLAGS	  =
#LIBS	  = -lsocket -lnsl -lcrypt -lgen -L/usr/ucblib -lucb
#RANLIB	  = ranlib
#INSTALL	  = /bin/install

# You must un-comment one, and only one, of the above sections!

# No modifications are necessary below this line except possibly near line 490.

all:	notice $(RADIUS) utils

utils:	$(LIB_OBJS) $(RAD_LIB) $(UTILS)

notice:
	@echo ""
	@echo "Merit RADIUS 2.4.23C, licensed software"
	@echo "Copyright (c) 1992, 1993, 1994, 1995, 1996 by The"
	@echo "Regents of the University of Michigan and Merit Network, Inc."
	@echo "All rights reserved."
	@echo ""

$(RAD_LIB): $(LIB_OBJS)
	ar rcv $@ $(LIB_OBJS)
	$(RANLIB) $@

# The ordering of the macros: $(RADLIBS), $(RAD_LIB) and $(LIBS) in the $(CC)
# rule for the $(RADIUS) target below is important and should remain as shown.

$(RADIUS): $(RAD_OBJS) $(RAD_LIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(RADIUS) $(RAD_OBJS) \
		$(RADLIBS) $(RAD_LIB) $(LIBS)

$(OBJ)/radiusd.o: $(SRC)/radiusd.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/radiusd.o -c $(SRC)/radiusd.c

$(OBJ)/fsm.o: $(SRC)/fsm.c $(SRC)/radius.h Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/fsm.o -c $(SRC)/fsm.c

$(OBJ)/authenticate.o: $(SRC)/authenticate.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/authenticate.o \
	-c $(SRC)/authenticate.c

$(OBJ)/rad.kchap.o: $(SRC)/rad.kchap.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/rad.kchap.o \
	-c $(SRC)/rad.kchap.c

$(OBJ)/rad.kerberos.o: $(SRC)/rad.kerberos.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/rad.kerberos.o \
	-c $(SRC)/rad.kerberos.c

$(OBJ)/rad.mnet.o: $(SRC)/rad.mnet.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/rad.mnet.o \
	-c $(SRC)/rad.mnet.c

$(OBJ)/rad.tacacs.o: $(SRC)/rad.tacacs.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/rad.tacacs.o \
	-c $(SRC)/rad.tacacs.c

$(OBJ)/rad.accounting.o: $(SRC)/rad.accounting.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/rad.accounting.o \
	-c $(SRC)/rad.accounting.c

$(OBJ)/funcs.o: $(SRC)/funcs.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/funcs.o -c $(SRC)/funcs.c

$(OBJ)/users.o: $(SRC)/users.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/users.o -c $(SRC)/users.c

#
# The two lines below are only for RADIUS servers being built with AFS
# support enabled.  Be sure to change the value of the AFS macro near
# the beginning of this Makefile, otherwise you may want to comment out
# the two lines below using the pound sign ("#") character in column one.
#

#$(SRC)/afs_stringtokey.c: $(AFS)/stringtokey.c
#	sed s/string_to_key/afs_string_to_key/g < $? > $@

$(OBJ)/afs_stringtokey.o: $(SRC)/afs_stringtokey.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/afs_stringtokey.o \
	-c $(SRC)/afs_stringtokey.c

$(OBJ)/krb_get_in_tkt.o: $(SRC)/krb_get_in_tkt.c Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/krb_get_in_tkt.o \
	-c $(SRC)/krb_get_in_tkt.c

$(OBJ)/id_to_key.o: $(SRC)/id_to_key.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/id_to_key.o \
	-c $(SRC)/id_to_key.c

$(OBJ)/mit_stringtokey.o: $(SRC)/mit_stringtokey.c $(RAD_INCS) Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/mit_stringtokey.o \
	-c $(SRC)/mit_stringtokey.c

$(OBJ)/dict.o: $(SRC)/dict.c $(SRC)/radius.h Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/dict.o -c $(SRC)/dict.c

$(OBJ)/util.o: $(SRC)/util.c $(SRC)/radius.h Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/util.o -c $(SRC)/util.c

$(OBJ)/version.o: $(SRC)/version.c $(SRC)/radius.h $(SRC)/conf.h Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/version.o -c $(SRC)/version.c

$(OBJ)/md5.o: $(SRC)/md5.c $(SRC)/md5.h Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/md5.o -c $(SRC)/md5.c

$(OBJ)/passchange.o: $(SRC)/passchange.c $(SRC)/radius.h Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/passchange.o -c $(SRC)/passchange.c

$(BIN)/builddbm: $(OBJ)/builddbm.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN)/builddbm $(OBJ)/builddbm.o \
	$(LIBS)

$(OBJ)/builddbm.o: $(SRC)/builddbm.c Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/builddbm.o -c $(SRC)/builddbm.c

$(RADPASS): $(OBJ)/radpass.o $(RAD_LIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(RADPASS) \
	$(OBJ)/radpass.o $(RADLIBS) $(RAD_LIB) $(LIBS)

$(OBJ)/radpass.o: $(SRC)/radpass.c $(SRC)/radius.h Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/radpass.o -c $(SRC)/radpass.c

$(RADPWTST): $(OBJ)/radpwtst.o $(RAD_LIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(RADPWTST) \
	$(OBJ)/radpwtst.o $(RADLIBS) $(RAD_LIB) $(LIBS)

$(OBJ)/radpwtst.o: $(SRC)/radpwtst.c $(SRC)/radius.h Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/radpwtst.o -c $(SRC)/radpwtst.c

$(RADCHECK): $(OBJ)/radcheck.o $(RAD_LIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(RADCHECK) \
	$(OBJ)/radcheck.o $(RADLIBS) $(RAD_LIB) $(LIBS)

$(OBJ)/radcheck.o: $(SRC)/radcheck.c $(SRC)/radius.h Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/radcheck.o -c $(SRC)/radcheck.c

$(DNSCHECK): $(OBJ)/dnscheck.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(DNSCHECK) $(OBJ)/dnscheck.o $(LIBS)

$(OBJ)/dnscheck.o: $(SRC)/dnscheck.c $(SRC)/radius.h Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/dnscheck.o -c $(SRC)/dnscheck.c

$(OBJ)/sendserver.o: $(SRC)/sendserver.c $(SRC)/radius.h Makefile
	$(CC) $(CFLAGS) -o $(OBJ)/sendserver.o \
	-c $(SRC)/sendserver.c

lint:
	-lint -hbacvx -DLINT $(RAD_SRCS)
	-lint -hbacvx -DLINT $(SRC)/radcheck.c $(SRC)/radpwtst.c              \
		$(SRC)/radpass.c $(SRC)/builddbm.c

tar:	./tarlist
	/bin/tar -cvf radius.tar -I ./tarlist

clean:
	/bin/rm -f $(RAD_OBJS) $(LIB_OBJS) $(UTIL_OBJS) $(RAD_LIB) \
	$(RADIUS) $(UTILS)

install-all: install util-install config-install man-install

install: $(RADIUS)
	-if [ ! -d $(DAEMON_INSDIR) ] ;\
	then \
		/bin/mkdir -p $(DAEMON_INSDIR) ;\
	fi
	$(INSTALL) $(M) 755 $(O) $(RADOWN) $(G) $(RADGRP) $(RADIUS) \
						$(DAEMON_INSDIR)/$(SERVER)

util-install: $(UTILS)
	-if [ ! -d $(BIN_INSDIR) ] ;\
	then \
		/bin/mkdir -p $(BIN_INSDIR) ;\
	fi
	$(INSTALL) $(M) 755 $(O) $(RADOWN) $(G) $(RADGRP) $(RADCHECK) \
						$(BIN_INSDIR)/radcheck
	$(INSTALL) $(M) 755 $(O) $(RADOWN) $(G) $(RADGRP) $(RADPWTST) \
						$(BIN_INSDIR)/radpwtst
	$(INSTALL) $(M) 755 $(O) $(RADOWN) $(G) $(RADGRP) $(RADPASS) \
						$(BIN_INSDIR)/radpass
	$(INSTALL) $(M) 755 $(O) $(RADOWN) $(G) $(RADGRP) $(DNSCHECK) \
						$(BIN_INSDIR)/dnscheck

man-install:
	-if [ ! -d $(MAN_INSDIR) ] ;\
	then \
		/bin/mkdir -p $(MAN_INSDIR) ;\
	fi
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(MAN)/authfile.5 \
						$(MAN_INSDIR)/man5/authfile.5
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(MAN)/clients.5 \
						$(MAN_INSDIR)/man5/clients.5
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(MAN)/dictionary.5 \
						$(MAN_INSDIR)/man5/dictionary.5
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(MAN)/radbnr.conf.5 \
						$(MAN_INSDIR)/man5/radbnr.conf.5
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(MAN)/radius.fsm.5 \
						$(MAN_INSDIR)/man5/radius.fsm.5
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(MAN)/users.5 \
						$(MAN_INSDIR)/man5/users.5
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(MAN)/builddbm.8 \
						$(MAN_INSDIR)/man8/builddbm.8
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(MAN)/radcheck.8 \
						$(MAN_INSDIR)/man8/radcheck.8
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(MAN)/radiusd.8 \
						$(MAN_INSDIR)/man8/radiusd.8
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(MAN)/radpwtst.8 \
						$(MAN_INSDIR)/man8/radpwtst.8

config-install: $(CONFIGS)
	-if [ ! -d $(RADDB_INSDIR) ] ;\
	then \
		/bin/mkdir -p $(RADDB_INSDIR) ;\
	fi
	$(INSTALL) $(M) 660 $(O) $(RADOWN) $(G) $(RADGRP) $(RADDB)/authfile \
						$(RADDB_INSDIR)/authfile
	$(INSTALL) $(M) 660 $(O) $(RADOWN) $(G) $(RADGRP) $(RADDB)/clients \
						$(RADDB_INSDIR)/clients
	$(INSTALL) $(M) 755 $(O) $(RADOWN) $(G) $(RADGRP) $(RADDB)/conversion.pl \
						$(RADDB_INSDIR)/conversion.pl
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(RADDB)/dictionary \
						$(RADDB_INSDIR)/dictionary
	$(INSTALL) $(M) 660 $(O) $(RADOWN) $(G) $(RADGRP) $(RADDB)/users \
						$(RADDB_INSDIR)/users
	$(INSTALL) $(M) 644 $(O) $(RADOWN) $(G) $(RADGRP) $(RADDB)/xas.fsm \
						$(RADDB_INSDIR)/xas.fsm
	-if [ ! -d $(RADACCT_INSDIR) ] ;\
	then \
		/bin/mkdir -p $(RADACCT_INSDIR) ;\
	fi
