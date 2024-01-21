# This makefile is really small. Most of the
# intelligence is in debian/debstd

all:
	gcc -O2 -o build build.c
	gcc -O2 -o debpkg debpkg.c

clean:
	-rm build

install:	
	install -d $(DESTDIR)/usr/{bin,sbin}
	install uscan uupdate debi debc release deb-make debstd debchange $(DESTDIR)/usr/bin
	install debconf debsums deblint debclean todo done $(DESTDIR)/usr/bin
	install -g root -o root build debpkg $(DESTDIR)/usr/bin
	ln -s debchange $(DESTDIR)/usr/bin/dch
	install -d $(DESTDIR)/usr/lib/deb-make
	cp -r lib/* $(DESTDIR)/usr/lib/deb-make
