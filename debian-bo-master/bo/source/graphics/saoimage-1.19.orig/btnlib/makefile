# makefile for SAOimage btnlib
#
# This version is a generic makefile, as opposed to the several editions for
# various machines that reside in the parent directory.  This file is intended
# to be driven from one of these parent makefiles which have knowledge about
# machine dependencies.  On many systems, however, this file can be run as a
# standalone makefile, i.e., if one were to type "make" in this directory,
# the library would build.  It is preferable to build btnlib as part of the
# make procedure in the parent SAOimage directory.

CFLAGS  = -c -O

LINT   = lint
RANLIB = ranlib
RM     = rm -f

.c.o:
	$(CC) $(CFLAGS) $*.c

SRCS =	buttons.h	borders.h \
	attach.c	draw.c		event.c		border.c \
	create.c	label.c		image.c		mount.c \
	press.c		remote.c	resize.c	util.c

OBJS =	attach.o	draw.o		event.o		border.o \
	create.o	label.o		image.o		mount.o \
	press.o		remote.o	resize.o	util.o

LIB	= libbtn.a
XLIBH	= /usr/include/X11/Xlib.h
#XLIBH = /usr/openwin/share/include/X11/Xlib.h  #for OpenWindows

all:	$(LIB)

$(OBJS):  buttons.h $(XLIBH)

create.o : borders.h motf.h

$(LIB): $(OBJS)
	ar cr $(LIB) $(OBJS)
	$(RM) *.o
	$(RANLIB) $(LIB) > /dev/null

lint:
	$(LINT) *.c

clean:
	$(RM) a.out core *.bak *~ *.o
