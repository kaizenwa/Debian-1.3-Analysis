/*
    IPX support library - RIP

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
#include <sys/socket.h>
#include <netinet/in.h>
#include "ipxrip.h"
#include "ipxd.h"

int
ipx_rip_size(int n)
{
	if (n>IPX_RIP_MAX_ENTRIES)
	{
		return 0;
	}
	return 2 + n * sizeof(struct rip_entry);
}

static int
ipx_rip_sendto(int sock, struct rip_packet *buffer, int size,
	       struct sockaddr_ipx *daddr)
{ 
	if (ipx_rip_output_func!=NULL)
	{
		return ipx_rip_output_func(sock, buffer, size, daddr);
	}
	return 0;
}

int
ipx_rip_output_init(struct rip_output *out, IPXNet net)
{
	out->entries=0;
	out->send_error = 0;
	out->buffer.operation=htons(IPX_RIP_OP_REQUEST);
	out->dest_addr.sipx_family=AF_IPX;
	out->dest_addr.sipx_type=IPX_RIP_PTYPE;
	out->dest_addr.sipx_network=htonl(net);
	return 0;
}

void
ipx_rip_output_flush(struct rip_output *out)
{
	if (out->entries==0)
	{
		return;
	}
	if (out->send_error != 0)
	{
		out->entries = 0;
		return;
	}
	if (ipx_rip_sendto(out->sk, &(out->buffer), ipx_rip_size(out->entries),
			   &(out->dest_addr)) < 0)
	{
		out->send_error = 1;
		check_request = 1;
	}
	out->entries=0;
}

void
ipx_rip_output_request(struct rip_output *out, IPXNet net)
{
	struct rip_entry *re;

	if (out->entries>=IPX_RIP_MAX_ENTRIES)
	{
		ipx_rip_output_flush(out);
	}

	if (out->buffer.operation!=htons(IPX_RIP_OP_REQUEST))
	{
		ipx_rip_output_flush(out);
		out->buffer.operation=htons(IPX_RIP_OP_REQUEST);
	}

	re = &(out->buffer.rip_entries[out->entries]);
	re->network = htonl(net);
	re->hops    = 0;
	re->ticks   = 0;
	out->entries += 1;
}

void
ipx_rip_output_response(struct rip_output *out, IPXNet net, hop_t hops,
			tick_t ticks, int down_allow)
{
	struct rip_entry* re;

	if (hops>=IPX_RIP_NET_DOWN && !down_allow)
	{
		return;
	}

	if (out->entries>=IPX_RIP_MAX_ENTRIES)
	{
		ipx_rip_output_flush(out);
	}

	if (out->buffer.operation!=htons(IPX_RIP_OP_RESPONSE))
	{
		ipx_rip_output_flush(out);
		out->buffer.operation=htons(IPX_RIP_OP_RESPONSE);
	}

	if (passive != 0)
	{
		return;
	}

	re = &(out->buffer.rip_entries[out->entries]);
	re->network = htonl(net);
	re->hops    = htons(hops);
	re->ticks   = htons(ticks);
	out->entries += 1;
}

void
ipx_rip_output_set_destination(struct rip_output *out, IPXNode node,
			       IPXPort port)
{
	ipx_rip_output_flush(out);
	ipx_assign_node(out->dest_addr.sipx_node,node);
	out->dest_addr.sipx_port=htons(port);
}

void
ipx_rip_dump(struct rip_packet *pkt, int len)
{
	ipx_rip_fdump(stdout, pkt, len);
}

void
ipx_rip_fdump(FILE *file, struct rip_packet *pkt, int len)
{
	struct rip_entry *re=pkt->rip_entries;
	int ent = (len-2) / sizeof(struct rip_entry);

	if (len<2)
	{
		return;
	}

	fprintf(file,"Operation: %i size: %i ", ntohs(pkt->operation),len);

	switch (ntohs(pkt->operation))
	{
	case IPX_RIP_OP_REQUEST:
		fprintf(file,"(Request)\n");
		if (len != ipx_rip_size(ent))
		{
			fprintf(file,"Warning: Bad RIP size (not 8*n+2)\n");
		}
		for(; ent > 0; ent -= 1)
		{
			fprintf(file,"RIP: Network: ");
			ipx_fprint_network(file,ntohl(re->network));
			fprintf(file,"\n");
			re += 1;
		}
		break;
	case IPX_RIP_OP_RESPONSE:
		fprintf(file,"(Response)\n");
		if (len != ipx_rip_size(ent))
		{
			fprintf(file,"Warning: Bad RIP size (not 8*n+2)\n");
		}
		for(; ent > 0; ent -= 1)
		{
			fprintf(file,"RIP: Network: ");
			ipx_fprint_network(file,ntohl(re->network));
			fprintf(file," HopCount: %u Ticks: %u\n",
				ntohs(re->hops),ntohs(re->ticks));
			re += 1;
		}
		break;
	default:
		fprintf(file,"(Unknown)\n");
		break;
	}
}

int (*ipx_rip_output_func)(int, void*, int, struct sockaddr_ipx*)=NULL;

