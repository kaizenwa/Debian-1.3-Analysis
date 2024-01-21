#
# Makefile for the linux ncp-filesystem routines.
#

VERSION = 0.23

# If you are using kerneld to autoload ncp support,
# uncomment this (kerneld is in linux since about 1.3.57):
KERNELD = -DHAVE_KERNELD

TOPDIR = $(shell pwd)
BINDIR = $(prefix)/usr/bin
SBINDIR = $(prefix)/usr/sbin
MANDIR = $(prefix)/usr/man
INTERM_BINDIR = $(TOPDIR)/bin
SUBDIRS = util man

KVERSION=$(shell uname -r | cut -b3)

ifeq ($(KVERSION),2)
SUBDIRS += kernel-1.2/src
INCLUDES = -I$(TOPDIR)/kernel-1.2
endif

export INCLUDES BINDIR INTERM_BINDIR SBINDIR KERNELD VERSION
export MANDIR

all:
	for i in $(SUBDIRS); do make -C $$i all; done

dep:
	for i in $(SUBDIRS); do	make -C $$i dep; done

install:
	for i in $(SUBDIRS); do	make -C $$i install; done

clean:
	rm -f `find . -type f -name '*.o' -print`
	rm -f `find . -type f -name '*~' -print`
	rm -f `find . -type f -name '.depend' -print`
	rm -f `find . -type f -name '*.out' -print`
	for i in $(SUBDIRS); do	make -C $$i clean; done

mrproper: clean
	rm -fr $(INTERM_BINDIR)/* ncpfs.tgz
	make -C util mrproper
	(cd daemon; make clean)

modules: ncpfs.o

SRCPATH=$(shell pwd)
SRCDIR=$(shell basename $(SRCPATH))
DISTFILE=$(SRCDIR).tgz

dist: mrproper
	mv daemon ..
	(cd ..; \
         tar cvf - $(SRCDIR) | \
            gzip -9 > $(DISTFILE); \
         mv $(DISTFILE) $(SRCDIR))
	mv ../daemon .
	make dep
	make all

tgz: mrproper
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
