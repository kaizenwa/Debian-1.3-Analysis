###########################################################################
## Makefile for xtris, version 1.0					 ##
## by Roger Espel Llima  <roger.espel.llima@pobox.com>			 ##
###########################################################################

###################
## Configuration ##
###################

# change this to your favorite ANSI C compiler
CC = gcc

# change this to the directory where you want the xtris binaries installed
BINDIR = /usr/local/bin

# change this to the directory where you want the xtris manpages installed
MANDIR = /usr/local/man

# change according to taste and local custom...
CFLAGS = -O3 -D__USE_FIXED_PROTOTYPES__

# on Solaris and similar systems, you'll need to uncomment this:
# EXTRALIBS = -lnsl -lsocket 

# specify X11 libdir if your system needs it
# XLIBDIR = -L/usr/X11/lib


###########################################################################
##         You shouldn't need to change anything past this.		 ##
###########################################################################

XLIBS = -lX11

all:	xtris xtserv xtbot

xtris:	xtris.c
	$(CC) $(CFLAGS) -DXTRISPATH="\"$(BINDIR)\"" xtris.c -o xtris $(XLIBDIR) $(XLIBS) $(EXTRALIBS)

xtserv:	xtserv.c
	$(CC) $(CFLAGS) xtserv.c -o xtserv $(EXTRALIBS)

xtbot:	xtbot.c
	$(CC) $(CFLAGS) xtbot.c -o xtbot $(EXTRALIBS)

clean:
	rm -f xtris xtserv xtbot core

install:	xtris xtserv xtbot
	-strip xtris xtserv xtbot
	-mkdir $(BINDIR) 2>/dev/null
	-mkdir $(MANDIR) 2>/dev/null
	-mkdir $(MANDIR)/man6 2>/dev/null
	cp xtris xtserv xtbot $(BINDIR)
	chmod 755 $(BINDIR)/xtris $(BINDIR)/xtserv $(BINDIR)/xtbot
	cp xtris.6 xtserv.6 xtbot.6 $(MANDIR)/man6
	chmod 644 $(MANDIR)/man6/xtris.6 $(MANDIR)/man6/xtserv.6 $(MANDIR)/man6/xtbot.6

tar:
	rm -f xtris.tar.gz
	mv -f Makefile Makefile.local
	cp Makefile.dist Makefile
	cd .. ; tar cf xtris/xtris.tar xtris/Makefile xtris/Makefile.dist xtris/README xtris/ChangeLog xtris/COPYING xtris/PROTOCOL xtris/xtris.lsm xtris/xtris.c xtris/xtserv.c xtris/xtbot.c xtris/xtris.6 xtris/xtserv.6 xtris/xtbot.6 ; gzip -9 xtris/xtris.tar
	mv -f Makefile.local Makefile

