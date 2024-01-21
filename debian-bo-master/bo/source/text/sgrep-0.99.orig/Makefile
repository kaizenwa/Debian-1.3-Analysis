#
#	System: Structured text retrieval tool sgrep.
#	Module: Makefile
#	Author: Pekka Kilpeläinen & Jani Jaakkola
#	Description: Makefile for building & installing sgrep
#	Version history: Original version February 1995 by JJ & PK
#	Copyright: University of Helsinki, Dept. of Computer Science
#		   Distributed under GNU General Public Lisence
#		   See file COPYING for details
#

# This is where the executable goes
BINDIR = /usr/local/bin/
# Where sgrep library files are placed (sgreprc)
LIBDIR = /usr/local/lib
# Where the manual page goes
MANFILE = /usr/local/man/man1/sgrep.1
# Where the sample rc file goes
RCFILE = $(LIBDIR)/sgreprc
# If you can't install as root you might wan't to use this as rc file
#RCFILE = $(HOME)/.sgreprc

VERSION = 0.99
#CFLAGS = -g -Wall -DVERSION=\"$(VERSION)\" -DLIBDIR=\"$(LIBDIR)\"
CFLAGS = -O -DVERSION=\"$(VERSION)\" -DLIBDIR=\"$(LIBDIR)\"
LDFLAGS =
# if cc fails to you use gcc
CC = cc
CPPFLAGS =
LIBS =

CFILES = main.c preproc.c common.c parser.c optimize.c pmatch.c eval.c output.c 
HEADERFILES = config.h defines.h
OBJECTS = main.o preproc.o common.o parser.o optimize.o pmatch.o eval.o output.o

OTHER = README ANNOUNCE Makefile COPYING TODO sgrep.1 sgrep.lsm sample.sgreprc
SGTOOL = sgtool.tcl sgtool.icon
DISTFILES = $(OTHER) $(HEADERFILES) $(CFILES) $(SGTOOL)

all: sgrep
 
sgrep: $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o sgrep $(LIBS)

clean:
	-rm -f $(OBJECTS)

install: sgrep
	cp sgrep $(BINDIR)/sgrep
	chmod 755 $(BINDIR)/sgrep
	cp sgrep.1 $(MANFILE)
	chmod 644 $(MANFILE)

install.rc: sample.sgreprc
	cp sample.sgreprc $(RCFILE)
	chmod 644 $(RCFILE)

install.sgtool:
	cp sgtool.tcl $(BINDIR)/sgtool
	chmod 755 $(BINDIR)/sgtool

uninstall:
	rm -f $(BINDIR)/sgrep
	rm -f $(MANFILE)
	rm -f $(RCFILE)

%.o: %.c $(HEADERFILES)
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@

# These won't work unless you have gnu-make

tar: $(DISTFILES)
	cd ..;tar -cf $(notdir $(PWD))/sgrep-$(VERSION).tar \
		$(addprefix $(notdir $(PWD))/, $(DISTFILES) )

tgz: tar
	gzip sgrep-$(VERSION).tar
	mv sgrep-$(VERSION).tar.gz sgrep-$(VERSION).tgz
