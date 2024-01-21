#
# Makefile for the iBCS emulator files
#
# $Id: Makefile,v 1.41 1996/07/26 12:03:22 mike Exp $
# $Source: /usr/CVS/ibcs/Makefile,v $
#

######## You should not need to change anything in this file ########


SUBDIRS=	iBCSemul x286emul Tools


all:	CONFIG
	set -e; for i in $(SUBDIRS); do \
	  test ! -d $$i || \
	    { $(MAKE) -C $$i; }; done

install: CONFIG devices
	set -e; for i in $(SUBDIRS); do \
	  test ! -d $$i || \
	    { $(MAKE) -C $$i install; }; done

devices:
	cp MAKEDEV.ibcs /dev/MAKEDEV.ibcs
	cd /dev; ./MAKEDEV.ibcs

clean:	CONFIG
	set -e; for i in $(SUBDIRS); do \
	  test ! -d $$i || \
	    { $(MAKE) -C $$i clean; }; done

dep:	CONFIG
	set -e; for i in $(SUBDIRS); do \
	  test ! -d $$i || \
	    { $(MAKE) -C $$i dep; }; done

CONFIG:
	@echo "Copy or link one of CONFIG.i386, CONFIG.sparc to CONFIG first"
	@exit 1
