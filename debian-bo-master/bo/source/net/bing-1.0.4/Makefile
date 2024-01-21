#
# Makefile,v 1.4 1995/07/21 00:03:30 pb Exp
#
MANDIR=/usr/local/man
BINDIR=/usr/local/bin
INSTALL=install -c
GROFF=groff
NROFF=nroff
RM=/bin/rm

# uncomment for Solaris 2
#CLIBS=-lnsl -lsocket -lucb

all:	bing bing.0 bing.ps

dist:	clean bing.0 bing.ps

bing:	bing.c sysdep.h
	$(CC) $(CFLAGS) $(CLIBS) -o $@ bing.c

bing.ps: bing.8
	$(GROFF) -mandoc bing.8 > bing.ps

bing.0: bing.8
	$(NROFF) -mandoc bing.8 > bing.0

clean:
	$(RM) -f bing bing.ps bing.0

install: bing bing.8
	$(INSTALL) -m 644 -o man -g man bing.8 $(MANDIR)/man8
	$(INSTALL) -m 4555 -o root -g staff bing $(BINDIR)
