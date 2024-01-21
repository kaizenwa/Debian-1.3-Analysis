#
# Makefile		NIS+ client support library
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
# Author: Peter Eriksson <pen@signum.se>, 5 Aug 1993
#

OBJS=	nis_xalloc.o nis_clnt.o nis_xdr.o nis_if.o nis_conf.o nis_names.o \
	nis_alloc.o nis_print.o nis_utils.o nis_error.o nis_subr.o \
	nis_dummy.o

ifdef NYS_LIBC

TOPDIR=../../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I$(TOPDIR) -I../../config -I../include

SRCS=$(OBJS:.o=.c)

include $(TOPDIR)/Maketargets

else

LIB=	../lib/libnis.a

all:	$(LIB)

clean:
	rm -f *.o *~ 

install:
	cp ../lib/libnis.a /usr/lib

TOP=../..
include $(TOP)/Makeconfig

endif
