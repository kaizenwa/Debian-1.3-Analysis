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
CFLAGS=$(GCOPTS) -I../../src -I../../src/nsw -I../../include
RPCLIB=

LIBS=	../../lib/libnsw.a ../../../nis/lib/libnis.a \
	../../../yp/lib/libyp.a ../../../dns/lib/libdns.a $(RPCLIB)

LDLIBS=$(LIBS)

all: passwd hosts networks shadow group rpc services protocols \
	ethers gshadow

GSBINS=gshadow_all gshadow_name
gshadow: $(GSBINS)

ETBINS=ethers_all ethers_name ethers_addr
ethers: $(ETBINS)

POBINS=protocols_all protocols_name protocols_number
protocols: $(POBINS)

SEBINS=services_all services_name services_port
services: $(SEBINS)

PWBINS=passwd_uid passwd_name passwd_all
passwd: $(PWBINS)

GRBINS=group_gid group_name group_all
group: $(GRBINS)

HOBINS=hosts_addr hosts_name hosts_all
hosts: $(HOBINS)

NEBINS=networks_all networks_addr networks_name
networks: $(NEBINS)

RPBINS=rpc_all rpc_number rpc_name
rpc: $(RPBINS)

SHBINS=shadow_all shadow_name
shadow: $(SHBINS)


passwd_uid:	passwd_uid.o passwd_misc.o 
passwd_name:	passwd_name.o passwd_misc.o 
passwd_all:	passwd_all.o passwd_misc.o 

group_gid:	group_gid.o group_misc.o 
group_name:	group_name.o group_misc.o 
group_all:	group_all.o group_misc.o 

services_all:	services_all.o services_misc.o 
services_name:	services_name.o services_misc.o 
services_port:	services_port.o services_misc.o 

hosts_addr:	hosts_addr.o hosts_misc.o 
hosts_name:	hosts_name.o hosts_misc.o 
hosts_all:	hosts_all.o hosts_misc.o 

ethers_addr:	ethers_addr.o ethers_misc.o 
ethers_name:	ethers_name.o ethers_misc.o 
ethers_all:	ethers_all.o ethers_misc.o 

networks_addr:	networks_addr.o networks_misc.o 
networks_name:	networks_name.o networks_misc.o 
networks_all:	networks_all.o networks_misc.o 

rpc_number:	rpc_number.o rpc_misc.o 
rpc_name:	rpc_name.o rpc_misc.o 
rpc_all:	rpc_all.o rpc_misc.o 

protocols_number:	protocols_number.o protocols_misc.o 
protocols_name:	protocols_name.o protocols_misc.o 
protocols_all:	protocols_all.o protocols_misc.o 

shadow_name:	shadow_name.o shadow_misc.o 
shadow_all:	shadow_all.o shadow_misc.o 

gshadow_name:	gshadow_name.o gshadow_misc.o 
gshadow_all:	gshadow_all.o gshadow_misc.o 


clean:
	-rm -f core $(PWBINS) $(HOBINS) $(NEBINS) $(SHBINS) $(GRBINS) \
		$(RPBINS) $(SEBINS) $(POBINS) $(ETBINS) $(GSBINS) *.o *~ \#*
