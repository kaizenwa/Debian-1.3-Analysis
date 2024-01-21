#
# Makefile		NSSwitch library makefile
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

ifdef NYS_LIBC

YOBJS=	yp/yp_passwd.o yp/yp_group.o yp/yp_shadow.o yp/yp_hosts.o \
	yp/yp_services.o yp/yp_rpc.o yp/yp_protocols.o \
	yp/yp_networks.o yp/yp_ethers.o yp/yp_misc.o yp/yp_gshadow.o \
	yp/yp_publickey.o

OBJS=$(notdir $(YOBJS))

TOPDIR=../../../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I$(TOPDIR) \
	-I.. \
	-I../../../config \
	-I../../../yp/include \
	-I../../../nis/include \
	-I../../../dns/include \
	-I../../../nsw/include

SRCS=$(OBJS:.o=.c)

include $(TOPDIR)/Maketargets

$(notdir $(WOBJS)): $(WOBJS:.o=.c)
nsw_passwd.o: nsw/nsw_passwd.c

endif
