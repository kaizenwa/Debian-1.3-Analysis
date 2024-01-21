#  Makefile for xtron v1.1
# 
#  Copyright (C) 1995 Rhett D. Jacobs <rhett@hotel.canberra.edu.au>
# 
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 1, or (at your option)
#  any later version.
# 
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
# 
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#  Last Modified: 16/4/95
#
CC=		gcc
CFLAGS=		-O4 -Wall
INCLUDES=	-I/usr/X386/include/X11 -I/usr/include
LIBS=		-L/usr/X386/lib
LDLIBS=		-lXpm -lX11
DEPEND=		makedepend
OBJS=		wintype.o xtron.o main.o xpm2pixmap.o resource.o
SRCS=		wintype.c xtron.c main.c xpm2pixmap.c resource.c
HEADERS=	wintype.h xtron.h main.h xpm2pixmap.h resource.h

all:		xtron

xtron:		$(OBJS)
		$(CC) $(OBJS) $(LIBS) $(LDLIBS) -o $@

clean:
		-rm $(OBJS) *~ *flc xtron

.c.o:
		$(CC) $(CFLAGS) $(DEFINES) $(INCLUDES) -c -o $*.o $*.c
