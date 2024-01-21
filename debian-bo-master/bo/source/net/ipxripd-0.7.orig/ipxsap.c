/*
   IPX support library - SAP

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
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "ipxsap.h"
#include "ipxd.h"

int 
ipx_sap_size(int n, unsigned short int operation)
{
	if (n <= 0)
	{
		return 0;
	}

	switch (operation)
	{
	case IPX_SAP_OP_REQUEST:
		return n == 1 ? IPX_SAP_REQUEST_LEN : 0;

	case IPX_SAP_OP_RESPONSE:
		if (n > IPX_SAP_MAX_ENTRIES)
		{
			return 0;
		}
		return (2 + n * sizeof(struct sap_entry));

	case IPX_SAP_OP_GNS_REQUEST:
		return n == 1 ? IPX_SAP_REQUEST_LEN : 0;

	case IPX_SAP_OP_GNS_RESPONSE:
		return n == 1 ? 2 + sizeof(struct sap_entry) : 0;
		break;
	default:
		return 0;
	}
}

void 
ipx_sap_assign_ser_name(ser_name_t dest, ser_name_t src)
{
	memset(dest, 0, IPX_SAP_SERVER_NAME_LEN);
	strncpy(dest, src, IPX_SAP_SERVER_NAME_LEN);
}

int 
ipx_sap_name_equal(ser_name_t n1, ser_name_t n2)
{
	return strncmp(n1, n2, IPX_SAP_SERVER_NAME_LEN) == 0;
}

int 
ipx_sap_type_equal(ser_type_t t1, ser_type_t t2)
{
	return t1==IPX_SAP_GENERAL_RQ || t2==IPX_SAP_GENERAL_RQ || t1 == t2;
}

static int
ipx_sap_sendto(int sock, void *buffer, int size, struct sockaddr_ipx *daddr)
{
	
	if (ipx_sap_output_func != NULL)
	{
		return ipx_sap_output_func(sock, buffer, size, daddr);
	}
	return 0;
}

int 
ipx_sap_output_init(struct sap_output *out, IPXNet net)
{
	out->entries = 0;
	out->send_error = 0;
	out->buffer.operation = htons(IPX_SAP_OP_REQUEST);
	out->dest_addr.sipx_family = AF_IPX;
	out->dest_addr.sipx_type = IPX_SAP_PTYPE;
	out->dest_addr.sipx_network = htonl(net);
	return 0;
}

void 
ipx_sap_output_flush(struct sap_output * out)
{
	if (out->entries == 0)
	{
		return;
	}
	if (out->send_error != 0)
	{
		out->entries = 0;
		return;
	}
	if (ipx_sap_sendto(out->sk, &(out->buffer),
			   ipx_sap_size(out->entries,
					ntohs(out->buffer.operation)),
			   &(out->dest_addr)) < 0)
	{
		out->send_error = 1;
		check_request = 1;
	}
	out->entries = 0;
}

void 
ipx_sap_output_request(struct sap_output * out, ser_type_t ser_type)
{
	struct sap_entry *se;
	ipx_sap_output_flush(out);
	out->buffer.operation = htons(IPX_SAP_OP_REQUEST);
	se = &(out->buffer.sap_entries[out->entries]);
	se->ser_type = htons(ser_type);
	out->entries++;
}

void 
ipx_sap_output_gns_request(struct sap_output * out, ser_type_t ser_type)
{
	struct sap_entry *se;
	ipx_sap_output_flush(out);
	out->buffer.operation = htons(IPX_SAP_OP_GNS_REQUEST);
	se = &(out->buffer.sap_entries[out->entries]);
	se->ser_type = htons(ser_type);
	out->entries++;
}

void 
ipx_sap_output_response(struct sap_output * out, ser_type_t type,
			ser_name_t name, struct sockaddr_ipx *addr,
			hop_t hops, int down_allow)
{
	struct sap_entry *se;

	if (hops >= IPX_SAP_SERVER_DOWN && !down_allow)
	{
		return;
	}
	if (out->entries >= IPX_SAP_MAX_ENTRIES)
	{
		ipx_sap_output_flush(out);
	}
	if (out->buffer.operation != htons(IPX_SAP_OP_RESPONSE))
	{
		ipx_sap_output_flush(out);
		out->buffer.operation = htons(IPX_SAP_OP_RESPONSE);
	}

	if (passive != 0)
	{
		return;
	}

	se = &(out->buffer.sap_entries[out->entries]);
	se->ser_type = htons(type);
	ipx_sap_assign_ser_name(se->ser_name, name);
	se->network = addr->sipx_network;
	ipx_assign_node(se->node, addr->sipx_node);
	se->port = addr->sipx_port;
	se->hops = htons(hops);
	out->entries++;
}

void 
ipx_sap_output_gns_response(struct sap_output * out,
			    ser_type_t type, ser_name_t name,
			    struct sockaddr_ipx *addr, hop_t hops)
{
	struct sap_entry *se;

	if (hops >= IPX_SAP_SERVER_DOWN)
	{
		return;
	}

	ipx_sap_output_flush(out);

	if (passive != 0)
	{
		return;
	}
	
	out->buffer.operation = htons(IPX_SAP_OP_GNS_RESPONSE);
	se = &(out->buffer.sap_entries[out->entries]);
	se->ser_type = htons(type);
	ipx_sap_assign_ser_name(se->ser_name, name);
	se->network = addr->sipx_network;
	ipx_assign_node(se->node, addr->sipx_node);
	se->port = addr->sipx_port;
	se->hops = htons(hops);
	out->entries++;
}

void 
ipx_sap_output_set_destination(struct sap_output * out,
			       IPXNode node, IPXPort port)
{
	ipx_sap_output_flush(out);
	ipx_assign_node(out->dest_addr.sipx_node, node);
	out->dest_addr.sipx_port = htons(port);
}

void 
ipx_sap_fprint_name(FILE * file, char *sname)
{
	char name[IPX_SAP_SERVER_NAME_LEN + 1];
	int len;

	memcpy(name, sname, IPX_SAP_SERVER_NAME_LEN);
	name[IPX_SAP_SERVER_NAME_LEN] = 0;
	len = strlen(name);
	memset(name + len, ' ', IPX_SAP_SERVER_NAME_LEN - len);
	fprintf(file, name);
}

static void 
fprint_entry(FILE * file, struct sap_entry *se)
{
	fprintf(file, "SAP: type: %04X name: ", ntohs(se->ser_type));
	ipx_sap_fprint_name(file, se->ser_name);
	fprintf(file, " \nhops: %i addr: ", ntohs(se->hops));
	ipx_fprint_network(file, ntohl(se->network));
	fprintf(file, ":");
	ipx_fprint_node(file, se->node);
	fprintf(file, ":");
	ipx_fprint_port(file, ntohs(se->port));
	fprintf(file, "\n");
}

void 
ipx_sap_dump(struct sap_packet *pkt, int len)
{
	ipx_sap_fdump(stdout, pkt, len);
}

void 
ipx_sap_fdump(FILE * file, struct sap_packet *pkt, int len)
{
	int ent;
	struct sap_entry *se = pkt->sap_entries;

	if (len < 2)
	{
		return;
	}

	fprintf(file, "Operation: %i size: %i ", ntohs(pkt->operation), len);
	switch (ntohs(pkt->operation))
	{
	case IPX_SAP_OP_REQUEST:
		fprintf(file, "(Request)\n");
		if (ipx_sap_size(1, IPX_SAP_OP_REQUEST) != len)
		{
			fprintf(file, "Warning: Bad SAP size\n");
		}
		fprintf(file, "SAP: Type: %04X\n", ntohs(se->ser_type));
		break;
	case IPX_SAP_OP_GNS_REQUEST:
		fprintf(file, "(Get Nearest Server Request)\n");
		if (ipx_sap_size(1, IPX_SAP_OP_GNS_REQUEST) != len)
		{
			fprintf(file, "Warning: Bad SAP size\n");
		}
		fprintf(file, "SAP: Type: %04X\n", ntohs(se->ser_type));
		break;
	case IPX_SAP_OP_RESPONSE:

		fprintf(file, "(Response)\n");
		ent = (len - 2) / sizeof(struct sap_entry);

		if (ipx_sap_size(ent, IPX_SAP_OP_RESPONSE) != len)
		{
			fprintf(file, "Warning: Bad SAP size\n");
		}
		for (; ent--; se++)
		{
			fprint_entry(file, se);
		}
		break;
	case IPX_SAP_OP_GNS_RESPONSE:
		fprintf(file, "(Get Nearest Server Response)\n");
		if (ipx_sap_size(1, IPX_SAP_OP_RESPONSE) != len)
		{
			fprintf(file, "Warning: Bad SAP size\n");
		}
		fprint_entry(file, se);
		break;
	default:
		fprintf(file, "(Unknown)\n");
		break;
	}
}

int (*ipx_sap_output_func) (int sock, void *, int, struct sockaddr_ipx *)
	= NULL;
