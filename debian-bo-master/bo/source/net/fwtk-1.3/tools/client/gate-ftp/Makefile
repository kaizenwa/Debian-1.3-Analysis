#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	based on 5.9 (Berkeley) 4/20/89
#
#
#
# MJR -
# new options:	-DPASSTHRU	enable autologin through ftp xfer gateway
# 		-DAUTH		enable code to ease use of cryptpokey auth
#		-DDEFAULT_GATEWAY=	default gateway to use
#		-DDEFAULT_GATEPORT=	default gateway port
#		-DLOG_ACTION		syslog user activity (WRL)
#

include ../../../Makefile.config

GWDEFAULTS= -DPASSTHRU_GATEWAY=\"otter.tis.com\" -DPASSTHRU_GATEPORT=21

CFLAGS= $(COPT) -I. -I../../.. -DPASSTHRU $(GWDEFAULTS)

LIBC=	/lib/libc.a
SRCS=	cmds.c cmdtab.c ftp.c glob.c main.c ruserpass.c domacro.c getpass.c
OBJS=	cmds.o cmdtab.o ftp.o glob.o main.o ruserpass.o domacro.o getpass.o
MAN=	ftp.1

all: ftp

ftp: $(OBJS) $(LIBC)
	-rm -f gate-ftp
	$(CC) $(LFLG) -o $@ $(CFLAGS) $(OBJS) $(AUXLIB)
	ln ftp gate-ftp

clean:
	rm -f $(OBJS) core ftp gate-ftp

cleandir: clean
	rm -f $(MAN) tags .depend

depend: $(SRCS)
	mkdep $(CFLAGS) $(SRCS)

install: ftp
	-if [ -f $(DEST)/ftp -a ! -f $(DEST)/ftp.orig ]; then \
		mv $(DEST)/ftp $(DEST)/ftp.orig; \
	fi
	install -s -o bin -g bin -m 755 ftp $(DEST)/ftp
	rm -f $(DEST)/gate-ftp
	ln $(DEST)/ftp $(DEST)/gate-ftp
	install -c -o bin -g bin -m 444 ftp.1 /usr/man/man1

lint: $(SRCS)
	lint $(CFLAGS) $(SRCS)

tags: $(SRCS)
	ctags $(SRCS)
