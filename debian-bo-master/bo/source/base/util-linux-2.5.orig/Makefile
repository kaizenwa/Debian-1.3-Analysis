# Makefile -- Makefile for util-linux Linux utilities
# Created: Sat Dec 26 20:09:40 1992
# Revised: Fri Oct  6 21:37:30 1995 by r.faith@ieee.org
# Copyright 1992, 1993, 1994, 1995 Rickard E. Faith (faith@cs.unc.edu)
# May be distributed under the terms of the GNU GPL.
#

VERSION=2.5

include ./MCONFIG

SUBDIRS= bsd \
	disk-utils \
	games \
	login-utils \
	misc-utils \
	mount \
	sys-utils \
	text-utils

ifeq "$(HAVE_SYSLOGD)" "no"
SUBDIRS:=$(SUBDIRS) syslogd
endif

.PHONEY: all install clean
all:
	@for subdir in $(SUBDIRS); do \
		(cd $$subdir && $(MAKE) $@) || exit 1; \
	done

install:
	@if [ "`whoami`" = "root" ]; then umask 022; fi
	@for subdir in $(SUBDIRS); do \
		(cd $$subdir && $(MAKE) $@) || exit 1; \
	done

clean:
	-rm -f *.o *~ core poe.diffs
	@for subdir in $(SUBDIRS) historic/selection; do \
		(cd $$subdir && $(MAKE) $@) || exit 1; \
	done

dist:
	(cd /tmp; \
	rm -rf /tmp/util-linux-$(VERSION); \
	cvs export -fNd util-linux-$(VERSION) -r HEAD util-linux; \
	cd util-linux-$(VERSION); \
	find -type d | xargs chmod 755; \
	find -type f | xargs chmod 644; \
	find -type d | xargs chown root:root; \
	find -type f | xargs chown root:root; \
	cd ..; \
	tar cvvf util-linux-$(VERSION).tar util-linux-$(VERSION); \
	gzip -9 util-linux-$(VERSION).tar; \
	cp -p util-linux-$(VERSION)/LSM util-linux-$(VERSION).lsm; \
	cp -p util-linux-$(VERSION)/ANNOUNCE util-linux-$(VERSION).Announce; \
	echo Done.)
