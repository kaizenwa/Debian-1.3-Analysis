#
# Makefile for the linux smb-filesystem routines.
#

VERSION = 2.0.1

# If you do not have kerneld support, please comment out the following
# line. It is STRONGLY recommended to enable kerneld support. If you do
# not have kerneld support enabled, smbmount will try to execute
# modprobe. If you make smbmount suid root, modprobe will be executed
# from a suid root program, which is probably a bad security hole.
KERNELD = -DHAVE_KERNELD

TOPDIR = $(shell pwd)
BINDIR = /usr/bin
MANDIR = /usr/local/man
SUBDIRS = util man

export INCLUDES VERSION KERNELD BINDIR MANDIR

all:
	for i in $(SUBDIRS); do make -C $$i; done

install:
	for i in $(SUBDIRS); do make -C $$i install; done

dep:
	for i in $(SUBDIRS); do	make -C $$i dep; done

clean:
	rm -f `find . -type f -name '*.o' -print`
	rm -f `find . -type f -name '*~' -print`
	rm -f `find . -type f -name '.depend' -print`
	rm -f `find . -type f -name '*.out' -print`
	for i in $(SUBDIRS); do	make -C $$i clean; done

mrproper: clean
	rm -f *.tgz
	make -C util mrproper

modules: smbfs.o

SRCPATH=$(shell pwd)
SRCDIR=$(shell basename $(SRCPATH))
DISTFILE=$(SRCDIR).tgz

dist: mrproper
	(cd ..; \
         tar cvf - $(SRCDIR) | \
            gzip -9 > $(DISTFILE); \
         mv $(DISTFILE) $(SRCDIR))

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
