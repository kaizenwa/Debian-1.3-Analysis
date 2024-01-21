# -*- makefile -*-
# $Id: Makefile,v 1.12 1996/06/23 21:33:52 agulbra Exp $
CC = gcc
DEBUG = -g -Wall -Wformat -Wstrict-prototypes -Wmissing-prototypes 
CFLAGS = $(DEBUG)
LIBS = 
BINDIR = /usr/local/sbin
MANDIR = /usr/local/man
VERSION = 1.0
#
# this is the directory where the news is stored.
SPOOLDIR = /var/spool/news
#
# this is the directory where the configuration and group information is
# kept.  It should preferably not be on the same partition as spooldir
LIBDIR = /usr/lib/leafnode

all: nntpd fetch texpire

nntpd: nntpd.o util.o config.o
	$(CC) -o $@ nntpd.o util.o config.o

fetch: fetch.o util.o config.o
	$(CC) -o $@ fetch.o util.o config.o

texpire: texpire.o util.o config.o
	$(CC) -o $@ texpire.o util.o config.o

clean:
	-@rm -f *.o core nntpd fetch texpire

depend:
	makedepend *.c 2> /dev/null

config.o: config.c Makefile
	$(CC) -c -DSPOOLDIR=\"$(SPOOLDIR)\" -DLIBDIR=\"$(LIBDIR)\" $<

install: nntpd fetch texpire
	cp nntpd $(BINDIR)/leafnode
	cp fetch $(BINDIR)/fetch
	cp texpire $(BINDIR)/texpire
	cp *.8 $(MANDIR)/man8
	-mkdir -p $(SPOOLDIR) $(LIBDIR)
	-mkdir -p $(SPOOLDIR)/{message.id,interesting.groups,out.going}
	# this is actually quite fast, for _small_ servers
	cd $(SPOOLDIR)/message.id
	for a in 0 1 2 3 4 5 6 7 8 9 ; do for b in 0 1 2 3 4 5 6 7 8 9 ; do \
		mkdir ${a}${b}0 ${a}${b}1 ${a}${b}2 ${a}${b}3 ${a}${b}4 ; \
		mkdir ${a}${b}5 ${a}${b}6 ${a}${b}7 ${a}${b}8 ${a}${b}9 ; \
		done ; done
	cp config.example $(LIBDIR)
	chown -R news.news $(LIBDIR) $(SPOOLDIR)
	chmod 2750 $(SPOOLDIR)
	chmod 750 $(BINDIR)/{leafnode,fetch,texpire}
	chown news.news $(BINDIR)/{leafnode,fetch,texpire}
	@echo edit /etc/inetd.conf to start $(BINDIR)/leafnode

dist:
	mkdir leafnode-$(VERSION)
	sed 's/^VERSION *= *[0-9.]*/VERSION = '$(VERSION)'/' \
		 < Makefile > leafnode-$(VERSION)/Makefile
	cp *[a-z].8 leafnode.h INSTALL README COPYING config.example \
		nntpd.c texpire.c fetch.c util.c config.c \
		archivefaq.pl newsq leafnode-$(VERSION)
	chmod 644 leafnode-$(VERSION)/*
	tar cf leafnode-$(VERSION).tar leafnode-$(VERSION)
	gzip -f9 leafnode-$(VERSION).tar
	rm -rf leafnode-$(VERSION)
