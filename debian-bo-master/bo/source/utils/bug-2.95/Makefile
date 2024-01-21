# This makefile is really small. Most of the
# intelligence is in debian/debstd

all: bug

bug: bug.in
	VER=`dpkg-parsechangelog | grep 'Version: [0-9\.]*$$'` ; \
	VER=$${VER#Version: } ; \
	sed -e "s/@VER@/$$VER/g" < bug.in > bug
	chmod +x bug

clean:
	rm -f bug build

install:	
	install bug $(DESTDIR)/usr/bin
