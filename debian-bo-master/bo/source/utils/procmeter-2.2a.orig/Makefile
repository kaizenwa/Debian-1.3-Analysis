# $Header: /home/amb/procmeter/RCS/Makefile 2.4 1997/03/15 19:46:44 amb Exp $
#
# ProcMeter - A simple performance monitor using /proc - Version 2.2a
#
# Makefile.
#
# Written by Andrew M. Bishop
#
# This file Copyright 1994,95,96 Andrew M. Bishop
# It may be distributed under the GNU Public License, version 2, or
# any higher version.  See section COPYING of the GNU Public license
# for conditions under which this file may be redistributed.
#

CC=gcc
CFLAGS=-O2

XINCLUDE=-I/usr/X11R6/include

XLIB=-L/usr/X11R6/lib

INSTDIR=/usr/local

COMPILE=$(CC) -c $(CFLAGS)

LINK=$(CC)

OBJ=procmeter.o statistics.o ProcMeter.o

########

procmeter : $(OBJ)
	$(LINK) $(OBJ) -o $@ $(XLIB) -lXaw -lXmu -lXt -lXext -lSM -lICE -lX11
#	$(LINK) $(OBJ) -o $@ $(XLIB) -lXaw -lXmu -lXt -lXext -lX11

# For X11 release 5, use the 2nd of these 2 lines, and comment out the first.

########

%.o : %.c
	$(COMPILE) $< -o $@ $(XINCLUDE)

procmeter.o   : procmeter.c   procmeter.h
statistics.o  : statistics.c  procmeter.h ProcMeter.h
ProcMeter.o   : ProcMeter.c               ProcMeter.h ProcMeterP.h

########

clean :
	-rm -f *.o *~

########

install :
	strip procmeter
	install -d $(INSTDIR)/bin
	install -d $(INSTDIR)/man/man1
	install -d $(INSTDIR)/lib/X11/app-defaults
	install -m 755 procmeter $(INSTDIR)/bin
	install -m 644 procmeter.1 $(INSTDIR)/man/man1
	install -m 644 procmeter.ad $(INSTDIR)/lib/X11/app-defaults/ProcMeter
