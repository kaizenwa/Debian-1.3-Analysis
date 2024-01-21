/*
    IPX routing daemon

    Copyright (C) 1996, Volker Lendecke <lendecke@namu01.gwdg.de>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifndef _IPXRIPD_H_
#define _IPXRIPD_H_

void
fdump_routes(FILE *file);

void
handle_rip(struct rip_packet *pkt, int len, struct sockaddr_ipx* sipx,
	   struct ipx_interface *src_ifc);

void
ipx_rip_do_aging(int rate, int do_broadcast);

int
ipx_rip_init_ifc(struct ipx_interface *ifc, IPXNet network,
		     char *device, int type, void *data);

void
ipx_rip_done(void);

void
ipx_rip_initial_broadcasts(void);

void
ipx_rip_initial_broadcast(struct ipx_interface *ifc);

void
ipx_rip_down_ifc(struct ipx_interface *ifc);

#endif /* _IPXRIPD_H_ */
