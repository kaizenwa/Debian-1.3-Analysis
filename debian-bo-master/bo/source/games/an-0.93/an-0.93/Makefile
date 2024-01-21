#    an v0.93 - Anagram generator
#    Copyright (C) 1996 Free Software Foundation
#    Copyright (C) 1995,1996  Richard Jones
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#    Mail suggestions and bug reports to
#    richard@deep-thought.org


# Change these variables to suit your system

INSTALLDIR=/usr/games
MANDIR=/usr/man/man6


# Note that if you change CC here you must also change it in ./lib/Makefile
CC=gcc

# if using a compiler which adheres strictly to ANSI C guidelines then
# uncomment the next line.

#DEFS=-DFORCE_ANSI

CFLAGS=-O -I $(LIBDIR) $(DEFS)

# Nothing should need changing below here

LIBDIR=./lib
OBJS=an.o gan_qsort.o
BIN=an
GETOPT=$(LIBDIR)/getopt.o $(LIBDIR)/getopt1.o
MAN=an.6


$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS)
$(OBJS): gan.h
$(MAN):

$(GETOPT):
	cd $(LIBDIR) && make

$(INSTALLDIR)/$(BIN): $(BIN)
	cp $(BIN) $(INSTALLDIR)/$(BIN)
	chmod 755 $(INSTALLDIR)/$(BIN)
$(MANDIR)/$(MAN): $(MAN)
	cp $(MAN) $(MANDIR)/$(MAN)
	chmod 644 $(MANDIR)/$(MAN)


with-getopt: $(OBJS) $(GETOPT)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(GETOPT)

install: $(BIN) $(INSTALLDIR)/$(BIN) $(MANDIR)/$(MAN)

install-with-getopt: with-getopt $(INSTALLDIR)/$(BIN) $(MANDIR)/$(MAN)

clean:
	-rm -f $(BIN) *.o
	-rm -f $(LIBDIR)/*.o









