# Makefile -- 
# Created: Mon Jan  8 11:38:13 1996 by r.faith@ieee.org
# Revised: Wed Apr 17 08:36:03 1996 by r.faith@ieee.org
# Copyright 1996 Rickard E. Faith (r.faith@ieee.org)
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 675 Mass Ave, Cambridge, MA 02139, USA.
#
# $Id: Makefile,v 1.9 1996/04/21 19:57:06 faith Exp $
#

VERSION=2.4

ifneq (,)
This makefile requires GNU Make.
endif

.SUFFIXES:

OBJS=apmlib.o
EXES=apm apmd tailf xapm
HEADERS=apm.h

CC=gcc
CFLAGS=-O -g
XTRACFLAGS=-Wall -pipe -I. -DVERSION=\"$(VERSION)\"
LDFLAGS=
XTRALDFLAGS=-L. -lapm
XLDFLAGS=-L/usr/X11R6/lib
XLIBS=-lXaw -lXmu -lXt -lXext  -lSM -lICE -lX11
LIB=libapm.a
RANLIB=ranlib

# Uncomment these lines for a production compile
#CFLAGS=-O3 -m486 -fomit-frame-pointer
#LDFLAGS=-s

all: $(EXES)

$(OBJS): $(HEADERS)

%.o: %.c
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(XTRACFLAGS) $<

%: %.o $(LIB)
	$(CC) -o $@ $< $(LDFLAGS) $(XTRALDFLAGS)

tailf: tailf.o
	$(CC)  -o $@ $< $(LDFLAGS) $(XTRALDFLAGS)

xapm.o: xapm.c
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(XTRACFLAGS) -DNARROWPROTO $<

xapm: xapm.o $(LIB)
	$(CC) -o $@ $< $(LDFLAGS) $(XTRALDFLAGS) $(XLDFLAGS) $(XLIBS)

libapm.a: $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@

.PHONY:	ChangeLog
ChangeLog:
	-rm -f ChangeLog
	rcs2log > ChangeLog

install: $(EXES)
	install        apm          /usr/bin
	install -m 644 apm.1        /usr/man/man1
	install        apmd         /usr/sbin
	install -m 644 apmd.8       /usr/man/man8
	install        xapm         /usr/X11R6/bin
	install -m 644 xapm.1       /usr/X11R6/man/man1/xapm.1x
	install        tailf        /usr/bin
	install -m 644 tailf.1      /usr/man/man1
	install -m 644 libapm.a     /usr/lib
	install -m 644 apm.h        /usr/include

.PHONY:	clean distclean
distclean clean:
	-rm -f *.o *.s *~ core a.out $(LIB) $(EXES)

dist:
	(rm -f /tmp/ChangeLog ./ChangeLog; \
	rcs2log > /tmp/ChangeLog; \
	cd /tmp; \
	rm -rf /tmp/apmd-$(VERSION); \
	cvs export -fNd apmd-$(VERSION) -r HEAD apmd; \
	cd apmd-$(VERSION); \
	mv ../ChangeLog .; \
	find -type d | xargs chmod 755; \
	find -type f | xargs chmod 644; \
	find -type d | xargs chown root:root; \
	find -type f | xargs chown root:root; \
	cd ..; \
	tar cvvf apmd-$(VERSION).tar apmd-$(VERSION); \
	gzip -9f apmd-$(VERSION).tar; \
	cp -p apmd-$(VERSION)/LSM apmd-$(VERSION).lsm; \
	cp -p apmd-$(VERSION)/ANNOUNCE apmd-$(VERSION).Announce; \
	echo Done.)
