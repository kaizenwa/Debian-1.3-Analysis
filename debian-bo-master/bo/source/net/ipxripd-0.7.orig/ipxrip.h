/*

    IPX support library

    Copyright (C) 1994, 1995  Ales Dryak <e-mail: A.Dryak@sh.cvut.cz>
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
#ifndef __IPXRIP_H__

#define __IPXRIP_H__

#include <stdio.h>
#include "ipxutil.h"

#define IPX_RIP_PORT		(0x453U)
#define IPX_RIP_PTYPE		(1U)
#define IPX_RIP_OP_REQUEST	(1U)
#define IPX_RIP_GENERAL_RQ	(0xFFFFFFFFU)
#define IPX_RIP_OP_RESPONSE	(2U)
#define IPX_RIP_MAX_ENTRIES	(50U)
#define IPX_RIP_NET_DOWN	(16U)

struct rip_entry
{
	IPXNet network __attribute__ ((packed));
	hop_t  hops    __attribute__ ((packed));
	tick_t ticks   __attribute__ ((packed));
};

struct rip_packet
{
	unsigned short int operation      __attribute__ ((packed));
	struct rip_entry   rip_entries[IPX_RIP_MAX_ENTRIES]
		                          __attribute__ ((packed));
};

int ipx_rip_size(int n);
void ipx_rip_dump(struct rip_packet *pkt, int len);
void ipx_rip_fdump(FILE* file, struct rip_packet *pkt, int len);

struct rip_output
{
	struct sockaddr_ipx dest_addr;
	int                 sk;	/* the socket to send/receive on */
	int                 send_error;
	int                 entries;
	struct rip_packet   buffer;
};

int ipx_rip_output_init(struct rip_output *out, IPXNet net);
void ipx_rip_output_flush(struct rip_output *out);
void ipx_rip_output_request(struct rip_output *out,IPXNet net);
void ipx_rip_output_response(struct rip_output *out, IPXNet net, hop_t hops,
			     tick_t ticks, int down_allow);
void ipx_rip_output_set_destination(struct rip_output *out, IPXNode node,
				    IPXPort port);

extern int (*ipx_rip_output_func)(int sock, void *buffer, int size,
				   struct sockaddr_ipx *daddr);

#endif
