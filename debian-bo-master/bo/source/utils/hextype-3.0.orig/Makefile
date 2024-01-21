# Makefile

# User-specific settings
VERSION=3.0
BINDIR=/usr/local/bin
MANDIR=/usr/local/man/man1

# Common settings
CC=gcc
RM=rm -f
MV=mv
CP=cp
CFLAGS=-Wall -O2 -fomit-frame-pointer
LDFLAGS=-s
PROGS=hextype

# Rules:

all: $(PROGS)

.c.o:
	$(CC) $(CFLAGS) -c $<

install: all
	install -m 555 hextype $(BINDIR)
	install -m 444 hextype.1 $(MANDIR)
#	ln -s hextype $(BINDIR)/hexdump
#	ln -s hextype.1 $(MANDIR)/hexdump.1

uninstall:
	$(RM) $(BINDIR)/hextype $(MANDIR)/hextype.1
#	$(RM) $(BINDIR)/hexdump $(MANDIR)/hexdump.1

deinstall: uninstall

remove: uninstall

clean:
	$(RM) $(PROGS) $(PROGS).o core *~

dist:	hextype.c Makefile hextype.1 README COPYING
	$(RM) $(PROGS) $(PROGS).o core *~ ../hextype-$(VERSION).tar.gz
	$(MV) hextype.lsm ../hextype-$(VERSION).lsm
	(cd ..;mv hextype hextype-$(VERSION); tar -cvzPf hextype-$(VERSION).tar.gz hextype-$(VERSION); mv hextype-$(VERSION) hextype)
	$(CP) ../hextype-$(VERSION).lsm hextype.lsm
