###############################################################################
#
# hfsutils - tools for reading and writing Macintosh HFS volumes
# Copyright (C) 1996, 1997 Robert Leslie
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
###############################################################################

CC =		gcc
INCLUDES =	-I../libhfs

LIBS =		
DEFINES =	

COPTS =		-g -Wall -pedantic
CFLAGS =	$(COPTS) $(INCLUDES) $(DEFINES)
ARFLAGS =	rc
RANLIB =	ranlib

###############################################################################

TARGET =	librsrc.a
OBJS =		rsrc.o

###############################################################################

all :: $(TARGET)

again :: depend clean all

depend ::
	( sed -n '1,/^### DEPEND/p' Makefile;  \
	  echo;  \
	  $(CC) -MM $(CFLAGS) *.c;  \
	) > Makefile.new
	mv -f Makefile.new Makefile

clean ::
	rm -f $(TARGET) *.o core

###############################################################################

$(TARGET): $(OBJS)
	ar $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@

### DEPENDENCIES FOLLOW #######################################################

rsrc.o: rsrc.c ../libhfs/hfs.h
