#
# Makefile
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
# Author: Peter Eriksson <pen@signum.se>
#
OBJS=	yp_conf.o yp_if.o yp_clnt.o yp_xdr.o ypupdate_xdr.o

ifdef NYS_LIBC

TOPDIR=../../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I$(TOPDIR) -I../../config -I../include

SRCS=$(OBJS:.o=.c)

include $(TOPDIR)/Maketargets

else

LIB=	../lib/libyp.a

all:	$(LIB)

clean:
	-rm -f core *.o *~ \#*

install:
	cp ../lib/libyp.a /usr/lib
	cp ../include/rpcsvc/ypclnt.h /usr/include/rpcsvc

TOP=../..
include $(TOP)/Makeconfig

endif
