/*
   IPX service advertising daemon 

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
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <linux/ipx.h>
#include "ipxutil.h"
#include "ipxkern.h"
#include "ipxsap.h"
#include "ipxd.h"

typedef unsigned char ifc_timer;

struct sap_table
{
	ser_name_t name;
	ser_type_t type;
	hop_t hops;
	struct sockaddr_ipx addr;
	ifc_timer timers[MAX_IFACE];	/* 0<timer<EXPIRE_TIME => net is
					   reachable via iface */
	/* timer>=EXPIPE_TIME => net is not r. */
	struct sap_table *next;
};

struct sap_table *stable = NULL;

static IPXNet
ifc_net(struct ipx_interface *ifc)
{
	return ntohl(ifc->s_output.dest_addr.sipx_network);
}

static int
is_expired(ifc_timer * tm)
{
	return *tm >= EXPIRE_TIME;
}

static void
setup_timers(struct sap_table *rt, struct ipx_interface *ifc)
{
	ifc_timer *tm;

	for (tm = rt->timers; tm < rt->timers + MAX_IFACE; tm++)
	{
		*tm = EXPIRE_TIME;
	}
	rt->timers[ifc_get_index(ifc)] = 0;
}

static void
output_flushall()
{
	struct ipx_interface *ifc = first_interface();

	while (ifc != NULL)
	{
		ipx_sap_output_flush(&(ifc->s_output));
		ifc = next_interface(ifc);
	}
}

static void
output_set_destination(struct ipx_interface *ifc,
		       IPXNode dest_node, IPXPort dest_port)
{
	if (ifc == NULL)
	{
		ifc = first_interface();
		while (ifc != NULL)
		{
			ipx_sap_output_set_destination(&(ifc->s_output),
						       dest_node, dest_port);
			ifc = next_interface(ifc);
		}
	}
	else
	{
		ipx_sap_output_set_destination(&(ifc->s_output), dest_node,
					       dest_port);
	}
}

static void
output_broadcast(struct sap_table *st, int down_allow)
{
	struct ipx_interface *ifc = first_interface();

	while (ifc != NULL)
	{
		if (is_expired(&(st->timers[ifc_get_index(ifc)])))
		{
			ipx_sap_output_response(&(ifc->s_output),
						st->type, st->name,
						&(st->addr),
						st->hops, down_allow);
		}
		ifc = next_interface(ifc);
	}
}

static int
output_sendto(int sock, void *buffer, int size, struct sockaddr_ipx *daddr)
{
	int res;

	DL_ENTRY;
	DL_START;
	fprintf(log_file, "Sending SAP to ");
	ipx_fprint_saddr(log_file, daddr);
	fprintf(log_file, "\n");
	ipx_sap_fdump(log_file, buffer, size);
	DL_END;

	res = sendto(sock, (void *) buffer, size, 0,
		     (struct sockaddr *) daddr, sizeof(*daddr));

	if (res == -1)
	{
		LOG_START;
		fprintf(log_file, "sendto: %s\n", strerror(errno));
		LOG_END;
	}
	return res;
}

static void
fprint_server(FILE * file, struct sap_table *st)
{
	int i;

	LOG_START;
	fprintf(file, "SAP: type: %04X name: ", st->type);
	ipx_sap_fprint_name(file, st->name);
	fprintf(file, " \nhops: %i addr: ", st->hops);
	ipx_fprint_saddr(file, &(st->addr));
	fprintf(file, " ");

	for (i = first_ifc_index(); i >= 0; i = next_ifc_index(i))
	{
		ifc_timer *tm = &(st->timers[i]);
		fprintf(file, "%i", is_expired(tm) ? 0 : 1);
		DL_START;
		fprintf(file, "(%i)", (int) *tm);
		DL_END;
	}
	LOG_END;
}

void
fdump_servers(FILE * file)
{
	struct sap_table *st;

	LOG_START;
	fprintf(file, "IPX server database:\n");
	for (st = stable; st != NULL; st = st->next)
	{
		fprint_server(file, st);
		fprintf(file, "\n");
	}
	LOG_END;
}


static struct sap_table *
add_server(ser_type_t type, ser_name_t name, struct sockaddr_ipx *addr,
	   hop_t hops, struct ipx_interface *ifc)
{
	struct sap_table *st;

	st=(struct sap_table *)malloc(sizeof(struct sap_table));
	if (st == NULL)
	{
		LOG_START;
		fprintf(log_file, "ipxsapd: out of memory in add_server\n");
		LOG_END;
		return NULL;
	}
	st->type = type;
	ipx_sap_assign_ser_name(st->name, name);
	st->addr = *addr;
	st->hops = hops;
	setup_timers(st, ifc);
	st->next = stable;
	stable = st;
	return st;
}

static void
delete_server(struct sap_table *d)
{
	struct sap_table **s;
	for (s = &stable; *s != NULL; s = &((*s)->next))
	{
		if (*s == d)
		{
			*s = d->next;
			free(d);
			return;
		}
	}
}

static void
handle_sap_gns_request(struct sap_entry * se, struct ipx_interface *src_ifc)
{
	struct sap_table *cur;
	struct sap_table *nearest = NULL;

	if (ntohs(se->ser_type) == IPX_SAP_GENERAL_RQ)
	{
		return;
	}
	for (cur = stable; cur != NULL; cur = cur->next)
	{
		if (ipx_sap_type_equal(cur->type, ntohs(se->ser_type)))
		{
			if (nearest == NULL)
			{
				nearest = cur;
				continue;
			}
			if (nearest->hops > cur->hops)
			{
				nearest = cur;
			}
		}
	}
	if (nearest != NULL)
	{
		ipx_sap_output_gns_response(&(src_ifc->s_output),
					    nearest->type, nearest->name,
					    &(nearest->addr), nearest->hops);
	}
	else
	{
		DL_START;
		fprintf(log_file, "No servers of type %04X was found\n",
			ntohs(se->ser_type));
		DL_END;
	}
}

static void
handle_sap_request(struct sap_entry *se, struct ipx_interface *src_ifc)
{
	struct sap_table *cur;
	for (cur = stable; cur != NULL; cur = cur->next)
	{
		if (ipx_sap_type_equal(cur->type, ntohs(se->ser_type)))
		{
			ipx_sap_output_response(&(src_ifc->s_output),
						cur->type, cur->name,
						&(cur->addr), cur->hops, 0);
		}
	}
}

static void
handle_sap_response(struct sap_entry *se, struct ipx_interface *src_ifc,
		    IPXNode src_node)
{
	struct sap_table *cur;

	se->hops = ntohs(se->hops) + 1;

	for (cur = stable; cur != NULL; cur = cur->next)
	{
		if (   !ipx_sap_name_equal(cur->name, se->ser_name)
		    || !ipx_sap_type_equal(cur->type, ntohs(se->ser_type)))
		{
			/* Another server was announced */
			continue;
		}
		/* entry found */
		if (se->hops <= IPX_SAP_SERVER_DOWN)
		{
			/* server ok */
			if (se->hops > cur->hops)
			{
				return;
			}
			if (se->hops == cur->hops)
			{
				/* server has equal 'route' */
				/* update info (near not neccesary) */
				cur->addr.sipx_network = se->network;
				ipx_assign_node(cur->addr.sipx_node, se->node);
				cur->addr.sipx_port = se->port;

				/* update timer for iface */
				cur->timers[ifc_get_index(src_ifc)] = 0;
				return;
			}
			/* server has better 'route' */
			LOG_START;
			fprintf(log_file, "CHANGE ");
			fprint_server(log_file, cur);
			LOG_END;

			/* update table */
			cur->hops = se->hops;
			cur->addr.sipx_network = se->network;
			ipx_assign_node(cur->addr.sipx_node, se->node);
			cur->addr.sipx_port = se->port;
			setup_timers(cur, src_ifc);

			LOG_START;
			fprintf(log_file, "\nto     ");
			fprint_server(log_file, cur);
			fprintf(log_file, "\n");
			LOG_END;

			/* send info bcast */
			output_broadcast(cur, 0);
		}
		else
		{
			/* server down through iface */
			int src_ifc_idx = ifc_get_index(src_ifc);
			int i;

			cur->timers[src_ifc_idx] = EXPIRE_TIME;
			for (i = first_ifc_index();
			     i >= 0;
			     i = next_ifc_index(i))
			{
				ifc_timer *tm = &(cur->timers[i]);
				if (!is_expired(tm))
				{
					return;
				}
			}
			cur->timers[src_ifc_idx] = 0;
			cur->hops = IPX_SAP_SERVER_DOWN;
			output_broadcast(cur, 1);
			LOG_START;
			fprintf(log_file, "DELETE ");
			fprint_server(log_file, cur);
			fprintf(log_file, " (service down)\n");
			LOG_END;
			delete_server(cur);
		}
		return;
	}
	/* entry not found */
	if (se->hops <= IPX_SAP_SERVER_DOWN)
	{
		struct sap_table *st;
		struct sockaddr_ipx addr;

		addr.sipx_network = se->network;
		ipx_assign_node(addr.sipx_node, se->node);
		addr.sipx_port = se->port;
		addr.sipx_type = 0;
		st = add_server(ntohs(se->ser_type), se->ser_name, &addr,
				se->hops, src_ifc);
		if (st != NULL)
		{
			/* send info bcast */
			output_broadcast(st, 0);
			LOG_START;
			fprintf(log_file, "ADD ");
			fprint_server(log_file, st);
			fprintf(log_file, "\n");
			LOG_END;
		}
	}
}

void
handle_sap(struct sap_packet * pkt, int len, struct sockaddr_ipx *sipx,
	   struct ipx_interface *src_ifc)
{
	struct sap_entry *se = pkt->sap_entries;
	int nent = (len - 2) / sizeof(struct sap_entry);

	if (ifc_net(src_ifc) != ntohl(sipx->sipx_network))
	{
		LOG_START;
		fprintf(log_file, "SAP from non-local net ");
		ipx_fprint_network(log_file, ntohl(sipx->sipx_network));
		fprintf(log_file, " (ignored)\n");
		LOG_END;
		return;
	}
	if (len < 2)
	{
		LOG_START;
		fprintf(log_file,
			"SAP packet too small len=%i (ignored)\n", len);
		LOG_END;
		return;
	}
	if (   ipx_node_equal(src_ifc->ifnode, sipx->sipx_node)
	    && ((unsigned short) sipx->sipx_port == htons(IPX_SAP_PORT)))
	{
		DL_START;
		fprintf(log_file, "My packet (ignored)\n");
		DL_END;
		return;
	}
	switch (ntohs(pkt->operation))
	{
	case IPX_SAP_OP_REQUEST:
		if (len != ipx_sap_size(1, IPX_SAP_OP_REQUEST))
		{
			LOG_START;
			fprintf(log_file,
				"SAP packet invalid size (ignored)\n");
			LOG_END;
			return;
		}
		output_set_destination(src_ifc,
				       sipx->sipx_node,
				       ntohs(sipx->sipx_port));
		handle_sap_request(se, src_ifc);
		output_flushall();
		break;
	case IPX_SAP_OP_GNS_REQUEST:
		if (len != ipx_sap_size(1, IPX_SAP_OP_REQUEST))
		{
			LOG_START;
			fprintf(log_file,
				"SAP packet invalid size (ignored)\n");
			LOG_END;
			return;
		}
		output_set_destination(src_ifc,
				       sipx->sipx_node,
				       ntohs(sipx->sipx_port));
		handle_sap_gns_request(se, src_ifc);
		output_flushall();
		break;
	case IPX_SAP_OP_RESPONSE:
		/* option: ignore responses from non SAP ports
		 * if (sipx->sipx_port!=htons(IPX_SAP_PORT)) return;
		 */
		output_set_destination(NULL, IPX_BROADCAST, IPX_SAP_PORT);
		for (; nent--; se++)
		{
			handle_sap_response(se, src_ifc, sipx->sipx_node);
		}
		output_flushall();
		break;
	case IPX_SAP_OP_GNS_RESPONSE:
		LOG_START;
		fprintf(log_file,
			"GNS response should never be received (ignored)\n");
		LOG_END;
		break;
	default:
		LOG_START;
		fprintf(log_file, "Unknown SAP operation\n");
		LOG_END;
		break;
	}
}

static void
delete_invalid_servers(void)
{
	struct sap_table **s = &stable;

	while (*s != NULL)
	{
		if ((*s)->hops >= IPX_SAP_SERVER_DOWN)
		{
			struct sap_table *d = *s;
			*s = (*s)->next;
			free(d);
		}
		else
		{
			s = &((*s)->next);
		}
	}
}

void
ipx_sap_do_aging(int rate, int do_broadcast)
{
	struct sap_table *cur;
	int servers_died = 0;

	DL_START;
	fprintf(log_file, "DO SAP AGING\n");
	DL_END;

	output_set_destination(NULL, IPX_BROADCAST, IPX_SAP_PORT);
	for (cur = stable; cur != NULL; cur = cur->next)
	{
		int down = 1;
		int i;

		for (i = first_ifc_index(); i >= 0; i = next_ifc_index(i))
		{
			ifc_timer* tm = &(cur->timers[i]);;
			if (!is_expired(tm))
			{
				(*tm) += rate;
				if (!is_expired(tm))
				{
					down = 0;
				}
			}
		}
		if (down)
		{
			/* server is down */
			LOG_START;
			fprintf(log_file, "DELETE ");
			fprint_server(log_file, cur);
			fprintf(log_file, " (timed out)\n");
			LOG_END;
			/* send info bcast */
			cur->hops = IPX_SAP_SERVER_DOWN;
			output_broadcast(cur, 1);
			/* table update deferred */
			servers_died = 1;
		} else
		{
			if (do_broadcast != 0)
			{
				output_broadcast(cur, 0);
			}
		}
	}

	/* Delete bogus servers */
	if (servers_died != 0)
	{
		delete_invalid_servers();
	}
	output_flushall();
}

int
ipx_sap_init_ifc(struct ipx_interface *ifc, IPXNet network,
		 char *device, int type, void *data)
{
	struct sockaddr_ipx sipx;

	if (ipx_sap_output_init(&(ifc->s_output), network) != 0)
	{
		LOG_START;
		fprintf(log_file,
			"out of memory allocating output buffer\n");
		LOG_END;
		return -1;
	}

	if ((ifc->s_output.sk = socket(AF_IPX, SOCK_DGRAM, PF_IPX)) < 0)
	{
		LOG_START;
		fprintf(log_file,
			"can't open socket: %s\n", strerror(errno));
		LOG_END;
		return -1;
	}

	memset(&sipx, 0, sizeof(sipx));
	sipx.sipx_family = AF_IPX;
	sipx.sipx_network = htonl(network);
	ipx_assign_node(sipx.sipx_node, IPX_THIS_NODE);
	sipx.sipx_port = htons(IPX_SAP_PORT);
	sipx.sipx_type = IPX_SAP_PTYPE;

	if (bind(ifc->s_output.sk, (struct sockaddr *)&sipx, sizeof(sipx)) < 0)
	{
		LOG_START;
		fprintf(log_file, "can't bind socket: %s\n",
			strerror(errno));
		LOG_END;
		return -1;
	}

	if (ipx_kern_enable_broadcast(ifc->s_output.sk) != 0)
	{
		LOG_START;
		fprintf(log_file, "cant' enable broadcast\n");
		LOG_END;
		exit(1);
	}

	LOG_START;
	ipx_fprint_network(log_file, network);
	fprintf(log_file,":");
	ipx_fprint_node(log_file, ifc->ifnode);
	fprintf(log_file,"\n");
	LOG_END;
	return 0;
}

static void
ipx_sap_deinit_ifc(struct ipx_interface *ifc)
{
	close(ifc->s_output.sk);
}

void
ipx_sap_down_ifc(struct ipx_interface *ifc)
{
	struct sap_table *st;
	int ifc_index = ifc_get_index(ifc);

	LOG_ENTRY;
	LOG_START;
	fprintf(log_file, "SAP DOWN INTERFACE ");
	ipx_fprint_network(log_file, ifc_net(ifc));
	fprintf(log_file, "\n");
	LOG_END;

	for (st = stable; st != NULL; st = st->next)
	{
		int i;
		st->timers[ifc_index] = EXPIRE_TIME;

		for (i = first_ifc_index();
		     i >= 0;
		     i = next_ifc_index(i))
		{
			ifc_timer *tm = &(st->timers[i]);
			if (!is_expired(tm))
			{
				return;
			}
		}
		st->timers[ifc_index] = 0;
		st->hops = IPX_SAP_SERVER_DOWN;
		output_broadcast(st, 1);
		LOG_START;
		fprintf(log_file, "DELETE ");
		fprint_server(log_file, st);
		fprintf(log_file, " (interface down)\n");
		LOG_END;
	}

	delete_invalid_servers();
	ipx_sap_deinit_ifc(ifc);
}

void
ipx_sap_initial_broadcasts()
{
	struct ipx_interface *ifc;
	ipx_sap_output_func = output_sendto;
	
	output_set_destination(NULL, IPX_BROADCAST, IPX_SAP_PORT);

	ifc = first_interface();

	while (ifc != NULL)
	{
		ipx_sap_output_request(&(ifc->s_output),
				       IPX_SAP_GENERAL_RQ);
		ifc = next_interface(ifc);
	}
	output_flushall();
}

void
ipx_sap_initial_broadcast(struct ipx_interface *ifc)
{
	struct sap_table *cur;

	output_set_destination(ifc, IPX_BROADCAST, IPX_SAP_PORT);

	for (cur = stable; cur != NULL; cur = cur->next)
	{
		ipx_sap_output_response(&(ifc->s_output),
					cur->type, cur->name,
					&(cur->addr), cur->hops, 0);
	}

	ipx_sap_output_request(&(ifc->s_output), IPX_SAP_GENERAL_RQ);
	ipx_sap_output_flush(&(ifc->s_output));
}

void
ipx_sap_done()
{
	struct sap_table *cur;

	LOG_ENTRY;
	LOG_START;
	fprintf(log_file, "SAP Shutdown start\n");
	LOG_END;

	output_set_destination(NULL, IPX_BROADCAST, IPX_SAP_PORT);
	for (cur = stable; cur != NULL; cur = cur->next)
	{
		LOG_START;
		fprintf(log_file, "DELETE ");
		fprint_server(log_file, cur);
		fprintf(log_file, " (shutdown)\n");
		LOG_END;
		/* send info bcast */
		cur->hops = IPX_SAP_SERVER_DOWN;
		output_broadcast(cur, 1);
		/* update table */
		delete_server(cur);
	}
	output_flushall();

	LOG_ENTRY;
	LOG_START;
	fprintf(log_file, "SAP Shutdown end\n");
	LOG_END;
}
