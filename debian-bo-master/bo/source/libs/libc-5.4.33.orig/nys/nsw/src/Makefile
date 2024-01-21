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

SOBJS=	nsswitch.o nsw_xalloc.o

WOBJS=	nsw/nsw_passwd.o nsw/nsw_group.o nsw/nsw_shadow.o nsw/nsw_hosts.o \
	nsw/nsw_services.o nsw/nsw_rpc.o nsw/nsw_protocols.o \
	nsw/nsw_networks.o nsw/nsw_ethers.o nsw/nsw_gshadow.o

FOBJS=	files/ethers.o files/hosts.o files/networks.o files/services.o \
	files/services.o files/rpc.o files/shadow.o files/passwd.o \
	files/group.o files/misc.o files/protocols.o files/gshadow.o

YOBJS=	yp/yp_passwd.o yp/yp_group.o yp/yp_shadow.o yp/yp_hosts.o \
	yp/yp_services.o yp/yp_rpc.o yp/yp_protocols.o \
	yp/yp_networks.o yp/yp_ethers.o yp/yp_misc.o yp/yp_gshadow.o

NOBJS=	nis/nis_passwd.o nis/nis_group.o nis/nis_shadow.o nis/nis_hosts.o \
	nis/nis_services.o nis/nis_rpc.o nis/nis_protocols.o \
	nis/nis_networks.o nis/nis_ethers.o nis/nis_gshadow.o

DOBJS=	dns/dns_passwd.o dns/dns_group.o dns/dns_shadow.o dns/dns_hosts.o \
	dns/dns_services.o dns/dns_rpc.o dns/dns_protocols.o \
	dns/dns_networks.o dns/dns_ethers.o dns/dns_gshadow.o

BOBJS=	dbm/dbm_passwd.o dbm/dbm_group.o dbm/dbm_shadow.o dbm/dbm_hosts.o \
	dbm/dbm_services.o dbm/dbm_rpc.o dbm/dbm_protocols.o \
	dbm/dbm_networks.o dbm/dbm_ethers.o dbm/dbm_gshadow.o

COBJS=	compat/compat_passwd.o compat/compat_group.o compat/compat_shadow.o compat/compat_hosts.o \
	compat/compat_services.o compat/compat_rpc.o compat/compat_protocols.o \
	compat/compat_networks.o compat/compat_ethers.o compat/compat_gshadow.o


ifdef NYS_LIBC

OBJS=$(SOBJS)

TOPDIR=../../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I$(TOPDIR) \
	-I. \
	-I../../config \
	-I../../yp/include \
	-I../../nis/include \
	-I../../dns/include \
	-I../../nsw/include

SRCS=$(OBJS:.o=.c)

include $(TOPDIR)/Maketargets

$(notdir $(WOBJS)): $(WOBJS:.o=.c)
nsw_passwd.o: nsw/nsw_passwd.c


else

OBJS=	$(SOBJS) $(WOBJS) $(FOBJS) $(YOBJS) $(NOBJS) $(DOBJS) $(BOBJS) $(COBJS) $(LOBJS)

LIB=	../lib/libnsw.a

all:	$(LIB)

clean:
	rm -f *.o */*.o *~ */*~ \#* */\#*


install:
	cp ../lib/libnsw.a /usr/lib


TOP=../..
include $(TOP)/Makeconfig
CFLAGS:=$(CFLAGS) -I$(TOP)/yp/include -I$(TOP)/nis/include -I$(TOP)/dns/include
endif


