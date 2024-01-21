#
# NET-BASE	A collection of programs that form the base set of the
#		NET-2 Networking Distribution for the LINUX operating
#		system.
#
# Version:	@(#)Makefile	0.32	01/16/94
#
# Author:	Fred N. van Kempen, <waltje@uwalt.nl.mugnet.org>
#		Copyright 1988-1993 MicroWalt Corporation
#
#		Extensively modified from 01/21/94 onwards by
#		Alan Cox <A.Cox@swansea.ac.uk>
#		Copyright 1993-1994 Swansea University Computer Society
#
#		This program is free software; you can redistribute it
#		and/or  modify it under  the terms of  the GNU General
#		Public  License as  published  by  the  Free  Software
#		Foundation;  either  version 2 of the License, or  (at
#		your option) any later version.
#

RELEASE	= tools-1.1.95


.EXPORT_ALL_VARIABLES:

CC	= cc -O6 -Wall
LD	= cc

CFLAGS	= -I. -I.. -Ilib -I../lib -DLINUX #-DDEBUG
LDFLAGS	= -Llib -L../lib -N -s

LIBS	= -lsupport


PROGS	= ifconfig hostname arp netstat route slattach rarp plipconfig dnsdomainname ipfw
SUBDIRS	= nls

.c.o:		$<
		$(CC) $(CFLAGS) -c $<


all:		config.h version.h subdirs $(PROGS)

install:	all
		@sh ./Install.sh

update:		all
		@sh ./Install.sh --nobackup

clean:
		rm -f *.o config.new version.h *~
		@for i in lib $(SUBDIRS); do cd $$i && make clean && cd ..; done

clobber:	clean
		rm -f $(PROGS) config.h config.status
		@for i in lib $(SUBDIRS); do cd $$i && make clobber && cd ..; done

dist:
		@echo Creating net-$(RELEASE) in ..
		@tar cfvz ../net-$(RELEASE).tar.gz *

config.h:	Makefile config.in
		@echo "Configuring the NET-3 Base Utilities..." ; echo
		@if [ -f config.status ]; \
			then /bin/sh ./Configure.sh <config.status; \
		   else /bin/sh ./Configure.sh <config.in; \
		 fi

version.h:	Makefile
		@echo "#define RELEASE \"NET-3 Base Utilities release $(RELEASE)\"" >version.h

ifconfig:	lib/libsupport.a ifconfig.o
		$(CC) $(LDFLAGS) -o ifconfig ifconfig.o $(LIBS)

hostname:	hostname.o
		$(CC) $(LDFLAGS) -o hostname hostname.o

plipconfig:	plipconfig.o
		$(CC) $(LDFLAGS) -o plipconfig plipconfig.o

route:		lib/libsupport.a route.o
		$(CC) $(LDFLAGS) -o route route.o $(LIBS)

arp:		lib/libsupport.a arp.o
		$(CC) $(LDFLAGS) -o arp arp.o $(LIBS)

rarp:		lib/libsupport.a rarp.o
		$(CC) $(LDFLAGS) -o rarp rarp.o $(LIBS)

slattach:	lib/libsupport.a slattach.o
		$(CC) $(LDFLAGS) -o slattach slattach.o $(LIBS)

netstat:	lib/libsupport.a netstat.o
		$(CC) $(LDFLAGS) -o netstat netstat.o $(LIBS)

ipfw:		ipfw.o
		$(CC) $(LDFLAGS) -o ipfw ipfw.o $(LIBS)

dnsdomainname:	hostname
		ln -f hostname dnsdomainname

subdirs:	
		@for i in $(SUBDIRS); do (cd $$i; $(MAKE)); done

lib/libsupport.a:
		@(cd lib ; $(MAKE))

netstat.o:	netstat.c
		$(CC) $(CFLAGS) -D__KERNEL__ -c $<

# End of Makefile.
