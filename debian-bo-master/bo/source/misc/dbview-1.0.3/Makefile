#  Copyright (c) 1996  Martin Schulze <Martin.Schulze@Infodrom.North.DE>
#
#  This file is part of the dbview package, a viewer for dBase II files
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

MAN=man

CC= gcc
CFLAGS= -O6 -fomit-frame-pointer -Wall -pipe
# CFLAGS= -g -fomit-frame-pointer -Wall -pipe

# Look where your install program is
#
INSTALL = /usr/bin/install
prefix = /usr
bindir = $(prefix)/bin
mandir = $(prefix)/$(MAN)

obj = db_dump.o dbview.o version.o

.c.o:
	$(CC) ${CFLAGS} -c $*.c

all:	dbview

dbview: $(obj)
	$(CC) $(CFLAGS) -o dbview $(obj)

depend:
	makedepend *.c

install: all
	$(INSTALL) -d -o root -g root -m 755 $(bindir)
	$(INSTALL) -s -o root -g root -m 755 dbview $(bindir)
	$(INSTALL) -d -o $(MAN) -g $(MAN) -m 755 $(mandir)/man1
	$(INSTALL) -o $(MAN) -g $(MAN) -m 644 dbview.1 $(mandir)/man1

clean:
	rm -f dbview *.o *.log *~ *.orig Makefile.bak;

clobber: clean
	rm -f TAGS;

# DO NOT DELETE

db_dump.o: db_dump.h /usr/include/fcntl.h /usr/include/features.h
db_dump.o: /usr/include/sys/cdefs.h /usr/include/sys/types.h
db_dump.o: /usr/include/linux/types.h /usr/include/asm/types.h
db_dump.o: /usr/include/gnu/types.h /usr/include/linux/fcntl.h
db_dump.o: /usr/include/sys/stat.h /usr/include/linux/stat.h
db_dump.o: /usr/include/stdio.h /usr/include/libio.h /usr/include/_G_config.h
db_dump.o: /usr/include/unistd.h /usr/include/posix_opt.h
db_dump.o: /usr/include/confname.h /usr/include/malloc.h
dbview.o: version.h db_dump.h /usr/include/stdio.h /usr/include/libio.h
dbview.o: /usr/include/_G_config.h /usr/include/getopt.h
