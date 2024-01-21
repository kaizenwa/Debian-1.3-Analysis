#
# Makefile for dwww.
# "@(#)dwww:$Id: Makefile,v 1.13 1997/03/22 23:56:24 jim Exp $"
#

CC=gcc
CFLAGS=-Wall -O2
LDFLAGS=-s
LIBS=-lpub

prefix=/home/jim/work/debian/dwww/test

bindir=$(prefix)/usr/bin
sbindir=$(prefix)/usr/sbin
libdir=$(prefix)/usr/lib/dwww
varlibdir=$(prefix)/var/lib/dwww
htmldir=$(prefix)/var/lib/dwww/html
docdir=$(prefix)/usr/doc/dwww
etcdir=$(prefix)/etc
etcdwwwdir=$(prefix)/etc/dwww
man1dir=$(prefix)/usr/man/man1
man8dir=$(prefix)/usr/man/man8
crondir=$(prefix)/etc/cron.daily
#cgidir=$(prefix)/cgi-bin
spooldir=$(prefix)/var/spool/dwww
menudir=$(prefix)/etc/menu-methods
webdocrootdir=$(prefix)/var/www
webcgidir=$(prefix)/usr/lib/cgi-bin

lib=lib/[^Ce]*
varlib=
bin=realpath dwww
sbin=dwww-convert dwww-build dwww-cache dwww-find dwww-quickfind \
	dwww-txt2html dwww-doc-index dwww-format-man dwwwconfig
doc=README TODO
man1=realpath.1
man8=dwww-convert.8 dwww-build.8 dwww-cache.8 dwww.8
cron=dwww-daily
menumethods=menu-methods

all: realpath dwww-cache dwww-quickfind dwww-txt2html

dwww-txt2html: dwww-txt2html.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< $(LIBS)

realpath: realpath.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< $(LIBS)

dwww-cache: dwww-cache.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< $(LIBS)
	
dwww-quickfind: dwww-quickfind.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< $(LIBS)
	
clean:
	rm -f core realpath dwww-cache dwww-quickfind dwww-txt2html

install:
	for i in $(prefix) $(prefix)/usr $(prefix)/var $(prefix)/var/lib \
	  $(etcdir) $(etcdwwwdir) $(libdir) $(varlibdir) $(bindir) $(sbindir) \
	  $(docdir) $(man1dir) $(man8dir) $(crondir) $(spooldir) \
	  $(webdocrootdir) $(webcgidir) \
	  $(htmldir) $(menudir); \
	do  \
		echo "$$i"; \
		test -d $$i || install -d $$i; \
	done
	install -m 0644 $(lib) $(libdir)
	chmod a+x $(libdir)/dwww.cgi
#	rm -f $(cgidir)/dwww
#	ln -s $(libdir)/dwww.cgi $(cgidir)/dwww
	ln -s ../../../usr/lib/dwww/dwww.cgi $(webcgidir)/dwww
	ln -s ../../var/lib/dwww/html $(webdocrootdir)/dwww
	install $(bin) $(bindir)
	install $(sbin) $(sbindir)
	install -m 0644 $(doc) $(docdir)
	install -m 0644 $(man1) $(man1dir)
	install -m 0644 $(man8) $(man8dir)
	install $(cron) $(crondir)/dwww
	install $(menumethods) $(menudir)/dwww
