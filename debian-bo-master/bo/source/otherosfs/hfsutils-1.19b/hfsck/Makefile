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

LIBS =		-L../libhfs -lhfs
DEFINES =	

COPTS =		-g -Wall -pedantic
CFLAGS =	$(COPTS) $(INCLUDES) $(DEFINES)

###############################################################################

TARGET =	hfsck
OBJS =		ck_mdb.o ck_btree.o hfsck.o util.o main.o

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

$(TARGET): $(OBJS) ../libhfs/libhfs.a
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o $@

### DEPENDENCIES FOLLOW #######################################################

ck_btree.o: ck_btree.c hfsck.h ../libhfs/internal.h ../libhfs/hfs.h \
 ../libhfs/data.h ../libhfs/low.h ../libhfs/record.h \
 ../libhfs/volume.h util.h ck_btree.h
ck_mdb.o: ck_mdb.c hfsck.h ../libhfs/internal.h ../libhfs/hfs.h \
 ../libhfs/data.h ../libhfs/low.h ../libhfs/record.h \
 ../libhfs/volume.h util.h ck_mdb.h
hfsck.o: hfsck.c hfsck.h ../libhfs/internal.h ../libhfs/hfs.h \
 ../libhfs/data.h ../libhfs/low.h ../libhfs/record.h \
 ../libhfs/volume.h ck_mdb.h ck_btree.h
main.o: main.c hfsck.h ../libhfs/internal.h ../libhfs/hfs.h \
 ../libhfs/data.h ../libhfs/low.h ../libhfs/record.h \
 ../libhfs/volume.h
util.o: util.c hfsck.h ../libhfs/internal.h ../libhfs/hfs.h \
 ../libhfs/data.h ../libhfs/low.h ../libhfs/record.h \
 ../libhfs/volume.h util.h
