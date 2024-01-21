#! /usr/bin/make -f
#
.PHONY: clean
 
# This is the default action
all: stamp-config
	cd web2c; make CFLAGS="-O2 -fomit-frame-pointer"  LDFLAGS=-s

stamp-config:
	cd web2c; ./configure --prefix=/usr
	touch stamp-config
 
# This cleans up
clean:
	cd web2c; make distclean;
	rm -f stamp-config

# Local Variables:
#   mode: makefile
# End:

