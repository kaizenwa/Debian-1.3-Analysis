# Makefile for vgrind         Frank Swasey
#                             32 Hickok Place
#                             Burlington, Vermont 05401-8515
#                             USA
#                             Email: Frank_Swasey@vnet.ibm.com
#

CC = gcc
CFLAGS = -O
PROG=	vfontedpr
SRCS=	regexp.c vfontedpr.c vgrindefs.c
DOCS=	vgrind.1 vgrindefs.5
OTHR=	Makefile vgrind.sh vgrindefs.src tmac.vgrind pathnames.h
INSTS=	/usr/bin/vgrind /usr/lib/groff/tmac/vgrindefs \
	/usr/lib/groff/tmac/tmac.vgrind /usr/lib/groff/tmac/vfontedpr \
	/usr/man/cat1/vgrind.1 /usr/man/cat5/vgrindefs.5

all: vfontedpr

vfontedpr: regexp.o vfontedpr.o vgrindefs.o

install: bininstall /usr/man/cat1/vgrind.1 /usr/man/cat5/vgrindefs.5
	touch install

bininstall: vfontedpr vgrind.sh vgrindefs.src tmac.vgrind
	install -c -o bin -g bin -m 555 vgrind.sh /usr/bin/vgrind
	install -c -o bin -g bin -m 444 vgrindefs.src /usr/lib/groff/tmac/vgrindefs
	install -c -o bin -g bin -m 444 tmac.vgrind /usr/lib/groff/tmac
	install -c -o bin -g bin -m 555 vfontedpr /usr/lib/groff/tmac
	touch bininstall

/usr/man/cat1/vgrind.1: vgrind.1
	nroff -mandoc vgrind.1 >/usr/man/cat1/vgrind.1

/usr/man/cat5/vgrindefs.5: vgrindefs.5
	nroff -mandoc vgrindefs.5 >/usr/man/cat5/vgrindefs.5

uninstall:
	rm -f /usr/bin/vgrind
	rm -f /usr/lib/groff/tmac/vgrindefs
	rm -f /usr/lib/groff/tmac/tmac.vgrind
	rm -f /usr/lib/groff/tmac/vfontedpr
	rm -f /usr/man/cat1/vgrind.1
	rm -f /usr/man/cat5/vgrindefs.5

clean:
	rm -f *.o vfontedpr a.out *.bak core make.log

srcdist: vgrindsrc.tgz

vgrindsrc.tgz: $(OTHR) $(SRCS) $(DOCS)
	tar cvzf vgrindsrc.tgz $(OTHR) $(SRCS) $(DOCS)

bindist: vgrindbin.tgz

vgrindbin.tgz: $(INSTS)
	tar cvzf vgrindbin.tgz $(INSTS)
