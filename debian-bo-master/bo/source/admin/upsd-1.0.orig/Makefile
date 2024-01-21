#-------------------------------------------------------------------
#
#  Makefile for "upsd"
#
#  make all     = Make upsd
#  make install = Make all, then copy to install dirs
#
#  $Log: Makefile,v $
#  Revision 1.5  1996/12/03 03:22:24  bobh
#  Add upsd.8 path.
#
#  Revision 1.4  1996/12/03 03:21:42  bobh
#  Add upsd.8 to dist.
#
#  Revision 1.3  1996/12/03 03:09:47  root
#  Change install directories.
#
#  Revision 1.2  1996/11/26 23:00:40  bobh
#  Add install and dist targets.
#
#  Revision 1.1  1996/11/23 16:45:12  bobh
#  Initial revision
#
#-------------------------------------------------------------------

BINDIR=/usr/sbin
MANDIR=/usr/man/man8

CC=gcc
LD=gcc
#CFLAGS=-g -Wall -DDEBUG
CFLAGS=-Wall -O2
LDFLAGS=

OBJS=common.o net.o upsd.o ups.o
DIST=upsd/README upsd/COPYING upsd/Makefile upsd/upsd \
     upsd/upsd.halt upsd/upsd.init upsd/upsd.8 upsd/*.c upsd/*.h 


all: makeall

ifeq (.depend,$(wildcard .depend))
include .depend
makeall: upsd
else
makeall: depend upsd
endif

depend dep: 
	$(CC) -M *.c > .depend;

clean:
	rm $(OBJS)
	rm .depend

realclean: clean
	rm upsd

dist: 
	cd ..; tar cfz upsd-1.0.tgz $(DIST)

install: makeall
	cp upsd $(BINDIR)
	chmod 755 $(BINDIR)/upsd
	cp upsd.8 $(MANDIR)
	chmod 644 $(MANDIR)/upsd.8

upsd : $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) -o $@

.c.o:
	$(CC) $(DEBUG) $(CFLAGS) -c $< -o $@
