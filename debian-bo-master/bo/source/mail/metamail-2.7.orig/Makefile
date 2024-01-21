# Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)
# 
# Permission to use, copy, modify, and distribute this material 
# for any purpose and without fee is hereby granted, provided 
# that the above copyright notice and this permission notice 
# appear in all copies, and that the name of Bellcore not be 
# used in advertising or publicity pertaining to this 
# material without the specific, prior written permission 
# of an authorized representative of Bellcore.  BELLCORE 
# MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
# OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
# WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
#

# System V users may change the following to remove the "-s" and all will still work.
LN = ln -s

RM = rm
# To shut rm up on SGI machines (where symlinks have mode 0000)
# RM = rm -f

# How to run Make 
MAKE = make

# How to copy files
CP = cp

# How to compile C programs
CC = cc

# You may also want to change the CFLAGS variable, which will be passed on 
# to all subdirectories.    If you do that, the changes will only work right if you
# start your make in the top-level directory, not if you start in a subdirectory.

# Note that the FOOBAR hack is to get -Bstatic on sun4's but nowhere else.
#It is very ugly, but what can you do?
FOOBAR-sun4=-Bstatic

STATICFLAG=FOOBAR${HOST_ARCH}

CFLAGS = -g -I.  ${$(STATICFLAG)} 
# The following is better if you want to make sure you run with SYSV defined
# CFLAGS = -g -I.  ${$(STATICFLAG)}  -DSYSV
# Also, for SGI Irix, compile in K&R mode
#CFLAGS = -g -I.  ${$(STATICFLAG)}  -DSYSV -cckr

# You may have to include other libraries to the link to get things like
# gethostname() resolved or use shared libraries.  This is done with the
# LDLIBS variable.
#
# For Sun and BSD systems, the following should work...
LDLIBS =
# On BSD 4.4 systems, you will need the following
# LDLIBS = -lcompat
# On SGI machines, we need -lsun for getpw...(), and -lc_s saves some space.
# LDLIBS = -lsun -lc_s
# ISC SysVr3.2.2 has a shared C library and requires libinet.a to resolve
# gethostname() references.
# LDLIBS = -linet -lc_s

#
# This is where config.h lives
CONFIGDIR = .

# This is how binaries should be installed
#
# If you are installing on the DEC Alpha running OSF/1, the install
# command uses different arguments than on other platforms.  You
# should either define INSTALL as cp, or modify all the uses of 
# the install command in the makefile to be
#
#	install -s -c $(LOCALBINDIR) $$file
#
#INSTALL = cp
INSTALL = install -c -s

# Root of installation tree
INSTROOT = /usr/local
#
# This is where binaries should be copied
LOCALBINDIR = ${INSTROOT}/bin

# This is where man pages should be copied, and their extensions
MAN1DIR = ${INSTROOT}/man/man1
MAN1EXT = 1
MAN4DIR = ${INSTROOT}/man/man4
MAN4EXT = 4

# Sets the font suffix your version of X uses - snf for X11R4, pcf for X11R5
# FONTSUFFIX=snf
FONTSUFFIX=pcf

# This is where the mailcap file should be put
MAILCAPDIR = ${INSTROOT}/etc

# This helps with the "for" constructs below on some platforms:
SHELL=/bin/sh

install: basics

basics::
	(cd metamail ; $(MAKE)  CONFIGDIR=../${CONFIGDIR}  CFLAGS="${CFLAGS}" CC="${CC}"  LDLIBS="${LDLIBS}")
	-${RM} bin/metamail
	(cd bin; ${LN} ../metamail/metamail metamail)
	-${RM} bin/mmencode
	(cd bin; ${LN} ../metamail/mmencode mmencode)
	-${RM} bin/mimencode
	(cd bin; ${LN} ../metamail/mmencode mimencode)
	-${RM} man/mimencode.1
	(cd man; ${LN} ../man/mmencode.1 mimencode.1)
	-${RM} bin/mailto
	(cd bin; ${LN} ../metamail/mailto mailto)
	-${RM} bin/splitmail
	(cd bin; ${LN} ../metamail/splitmail splitmail)
	(cd richmail ; $(MAKE)  CONFIGDIR=../${CONFIGDIR}  CFLAGS="${CFLAGS}" CC="${CC}"  LDLIBS="${LDLIBS}")
	-${RM} bin/richtext
	(cd bin; ${LN} ../richmail/richtext richtext)
	-${RM} bin/richtoatk
	(cd bin; ${LN} ../richmail/richtoatk richtoatk)
	(cd fonts ; $(MAKE)  CONFIGDIR=../${CONFIGDIR}  CFLAGS="${CFLAGS}" CC="${CC}"   LDLIBS="${LDLIBS}" FONTSUFFIX="${FONTSUFFIX}")
	-${RM} bin/shownonascii
	(cd bin; ${LN} ../fonts/shownonascii shownonascii)
	-${RM} bin/mailto-hebrew
	(cd bin; ${LN} ../fonts/mailto-hebrew mailto-hebrew)

install-all: basics 
	@(cd bin; 				\
	for file in *; 				\
	do 					\
	    target=$(LOCALBINDIR)/$$file; 	\
	    echo "installing binary $$target";	\
	    ${INSTALL} $$file $$target; 	\
	done)
	@(cd man;				\
	for file in *.1;			\
	do 					\
	    target=$(MAN1DIR)/`basename $$file .1`.$(MAN1EXT); \
	    echo "installing man page $$target";\
	    ${CP} $$file $$target; 		\
	done)
	@(cd man;				\
	for file in *.4;			\
	do 					\
	    target=$(MAN4DIR)/`basename $$file .4`.$(MAN4EXT); \
	    echo "installing man page $$target";\
	    ${CP} $$file $$target; 		\
	done)
	@echo "Renaming old mailcap file to $(MAILCAPDIR)/mailcap.old
	-mv $(MAILCAPDIR)/mailcap $(MAILCAPDIR)/mailcap.old
	@echo "installing file $(MAILCAPDIR)/mailcap"
	${CP} mailcap $(MAILCAPDIR)

uninstall-all:
	@(cd bin; 				\
	for file in *; 				\
	do 					\
	    target=$(LOCALBINDIR)/$$file; 	\
	    echo "rm-ing binary $$target";\
	    ${RM} $(LOCALBINDIR)/$$file; 	\
	done)
	@(cd man;				\
	for file in *.1;			\
	do 					\
	    target=$(MAN1DIR)/`basename $$file .1`.$(MAN1EXT); \
	    echo "rm-ing man page $$target";\
	    ${RM} $$target; 		\
	done)
	@(cd man;				\
	for file in *.4;			\
	do 					\
	    target=$(MAN4DIR)/`basename $$file .4`.$(MAN4EXT); \
	    echo "rm-ing man page $$target";\
	    ${RM} $$target; 		\
	done)
	@echo "rm-ing file $(MAILCAPDIR)/mailcap"
	@${RM} $(MAILCAPDIR)/mailcap

clean:
	-${RM} -rf *.BAK
	(cd metamail ; $(MAKE) clean)
	(cd richmail ; $(MAKE) clean)
	(cd fonts ; $(MAKE) FONTSUFFIX="${FONTSUFFIX}" clean)

cleanbin:
	-${RM} bin/metamail
	-${RM} bin/mmencode
	-${RM} bin/mimencode
	-${RM} bin/mailto
	-${RM} bin/splitmail
	-${RM} bin/richtext
	-${RM} bin/richtoatk
	-${RM} bin/shownonascii
	-${RM} bin/mailto-hebrew

cleanman:
	-${RM} man/mimencode.1

clobber: clean cleanambin cleanbin cleanman
