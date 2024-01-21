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
# Author: Peter Eriksson <pen@signum.se>, 5 Aug 1993
#

GCOPTS=	-g -Wall
CC=gcc
CFLAGS=$(GCOPTS)  -I../../include -I../../../dns/include
RPCLIB=

LIBS=	../../lib/libnsw.a ../../../nis/lib/libnis.a \
	../../../yp/lib/libyp.a ../../../dns/lib/libdns.a $(RPCLIB)

LDLIBS=$(LIBS)

all: hosts

HOBINS=hosts_name hosts_addr hosts_all
hosts: $(HOBINS)



hosts_name:	hosts_name.o hosts_misc.o 
hosts_addr:	hosts_addr.o hosts_misc.o 
hosts_all:	hosts_all.o hosts_misc.o 


clean:
	-rm -f core $(HOBINS) *.o *~ \#*
