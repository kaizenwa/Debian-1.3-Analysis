# Generated automatically from Makefile.in by configure.
# This Makefile is used to create several test cases for Tcl's load
# command.  It also illustrates how to take advantage of configuration
# exported by Tcl to set up Makefiles for shared libraries.
# @(#) Makefile.in 1.3 95/09/03 17:28:38

SHLIB_CFLAGS =		-fpic
SHLIB_LD =		gcc -shared
SHLIB_SUFFIX =		.so
CC = gcc

CFLAGS = -O2 -I../include ${SHLIB_CFLAGS}

all: cdrom${SHLIB_SUFFIX} pkga${SHLIB_SUFFIX}

pkga${SHLIB_SUFFIX}: pkga.o
	${SHLIB_LD} pkga.o -o pkga${SHLIB_SUFFIX}

cdrom${SHLIB_SUFFIX}: cdrom.o
	${SHLIB_LD} cdrom.o -o cdrom${SHLIB_SUFFIX}

clean:
	rm -f *~ *.o *${SHLIB_SUFFIX} config.cache config.log config.status

distclean: clean
	rm -f Makefile
