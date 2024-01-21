#
# Makefile	Top level makefile for the NYS project
#				(NIS, YP, Switch)
#
# Copyright (c) 1993 Signum Support AB, Sweden.
#
# This file is part of the NYS Library.
#
# The NYS Library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# The NYS Library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
# 
# You should have received a copy of the GNU Library General Public
# License along with the NYS Library; see the file COPYING.LIB.  If
# not, write to the Free Software Foundation, Inc., 675 Mass Ave,
# Cambridge, MA 02139, USA.
#
# Author: Peter Eriksson <pen@signum.se>, 5 Augh 1993
#

ifneq ($(NYS),true)

NSLDIRS= yp nis dns nsw

all: libnsl

clients: libnsl
	@set -e; for i in $(NSLDIRS) ; do \
		$(MAKE) -C $$i $(ALLFLAGS) clnts ; \
	done

libnsl:
	@set -e; for i in $(NSLDIRS) ; do \
		$(MAKE) -C $$i $(ALLFLAGS) ; \
	done
	$(RANLIB) libs/libnsl.a

clean:
	rm -f *.o *~ core \#* jump/_*
	@set -e; for i in $(NSLDIRS) ; do \
		$(MAKE) -C $$i $(ALLFLAGS) clean ; \
	done

distclean:
	rm -f *.o *~ core libs/* libs/${SHARED}* \#* jump/_*
	@set -e; for i in $(NSLDIRS) ; do \
		$(MAKE) -C $$i $(ALLFLAGS) distclean ; \
	done

#*************NO USER SERVICEABLE PARTS BELOW THIS LINE**************

include Makeconfig

else
#
# Makefile for libc-linux integration
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

DIRS=	nsw/src \
	yp/src \
	nis/src \
	nsw/src/nsw \
	nsw/src/files \
	nsw/src/yp \
	nsw/src/compat \
	nsw/src/nis \
	nsw/src/dns \
	nsw/src/dbm \
	nsw/src/misc

ifneq ($(MATH),true)
DIRS := $(DIRS)
endif

lib depend realclean clean:
	@set -e; for i in $(DIRS); \
	do \
		echo making $@ in $$i; \
		$(MAKE) NYS_LIBC=true -C $$i $@; \
	done

clnts:
	@set -e; for i in yp nis dns nsw; \
	do \
		echo making $@ in $$i; \
		$(MAKE) -C $$i $@; \
	done
endif
