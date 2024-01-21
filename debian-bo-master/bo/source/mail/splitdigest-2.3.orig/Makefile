# Makefile for splitdigest
# Copyright (c) 1994,1995 by Christopher Heng. All rights reserved.

# $Id: Makefile,v 2.9 1995/07/08 10:04:45 chris Released $


CC = gcc
DBG=-O2 -fomit-frame-pointer -s
CFLAGS = $(DBG)
LDFLAGS = $(DBG)

# You may define the following to change the default names in
# splitdigest.h
# Note that things defined here supercedes those defined in splitdigest.h
#	DEFCOMPRESS	default name of the compression program to use.
#	COMPRESSARG	default argument for compression program.
#	DEFTOCOMPRESS	whether to compress the output file by default.
#	DEFCONFIG	default pathname of the configuration file.
#			If you change this, change the macro LIBDIR below
#			as well.
#	MAXCFTABLE	maximum number of digest types in the config file.
#	OLDMETHOD	If you want the old method of not undigesting by
#			default.
# These supercedes macros in cftables.h
#	SEPCHAR		separator character between messages in a digest
DEFCOMPRESS = /usr/bin/gzip
COMPRESSARG = -9
DEFTOCOMPRESS = 0
DEFCONFIG = /usr/local/lib/splitdigest.config
MAXCFTABLE = 128
MAXSCANFBUF = 512
SEPCHAR	= -
OLDMETHOD = 0
DEFINES = -DDEFCOMPRESS=\"$(DEFCOMPRESS)\" -DCOMPRESSARG=\"$(COMPRESSARG)\"\
	-DDEFTOCOMPRESS=$(DEFTOCOMPRESS) -DDEFCONFIG=\"$(DEFCONFIG)\"\
	-DMAXCFTABLE=$(MAXCFTABLE) -DSEPCHAR=\'$(SEPCHAR)\' \
	-DOLDMETHOD=$(OLDMETHOD)

BINDIR = /usr/local/bin
MANDIR = /usr/man/man1
LIBDIR = /usr/local/lib

INSTALL = install -m 711 -o root -g root
INSTALLDATA = install -m 444 -o root -g root

# -----------------------------------------------------------------------
# End of configurable section of the Makefile.
# You should not need to modify anything below this line
# ------------------------------------------------------------------------

OBJS = main.o cftables.o uncatfile.o

FILES = ANNOUNCEMENT COPYING CHANGES README Makefile \
        main.c uncatfile.c cftables.c splitdigest.h version.h cftables.h \
        splitdigest.1 splitdigest.config splitdigest.lsm

SDVERSTR = -nR2_3
SDVERNUM = 2.3
RCSSTATE = -sReleased

all: splitdigest

splitdigest: $(OBJS)
	$(CC) $(LDFLAGS) -o splitdigest $(OBJS)

install: all
	$(INSTALL) splitdigest $(BINDIR)/
	$(INSTALLDATA) splitdigest.1 $(MANDIR)/
	$(INSTALLDATA) splitdigest.config $(LIBDIR)/

clean:
	rm -f *.o splitdigest

main.o: main.c splitdigest.h version.h cftables.h
	$(CC) $(CFLAGS) $(DEFINES) -o main.o -c main.c

cftables.o: cftables.c splitdigest.h cftables.h
	$(CC) $(CFLAGS) $(DEFINES) -o cftables.o -c cftables.c

uncatfile.o: uncatfile.c splitdigest.h cftables.h
	$(CC) $(CFLAGS) $(DEFINES) -o uncatfile.o -c uncatfile.c

checkin:
	ci $(SDVERSTR) -u$(RCSVERNUM) $(RCSSTATE) $(FILES)

checkout:
	co $(FILES)

develcheckout:
	co -l $(FILES)

mytar:	all
	cd ..;\
	tar cvvf spdig-$(SDVERNUM).tar splitdigest-$(SDVERNUM) ;\
	gzip -9 spdig-$(SDVERNUM).tar

dist:	mytar
	rm -r RCS
	rm *.o
	cd ..;\
	tar cvvf splitdigest-$(SDVERNUM).tar splitdigest-$(SDVERNUM) ;\
	gzip -9 splitdigest-$(SDVERNUM).tar
