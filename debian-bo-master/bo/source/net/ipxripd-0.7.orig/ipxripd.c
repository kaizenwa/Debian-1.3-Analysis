/*
    IPX routing daemon

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
/* done: filter my broadcasts for proper aging */
/* done: age each iface separately */
/* done: DONTROUTE ioctl */
/* done: shutdown */
/* done: logovani ... */
/* done: full daemonize */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <linux/ipx.h>
#include "ipxutil.h"
#include "ipxkern.h"
#include "ipxrip.h"
#include "ipxd.h"

typedef unsigned char ifc_timer;

struct rip_table
{
 	IPXNet network;
	hop_t hops;
	tick_t ticks;
	IPXNode router_node;
	struct ipx_interface *router_interface;

	ifc_timer timers[MAX_IFACE]; /* 0<timer<EXPIRE_TIME =>
					net is reachable via iface */
				     /* timer>=EXPIPE_TIME => net is not r. */
	struct rip_table* next;
};

struct rip_table *rtable = NULL;

static IPXNet
ifc_net(struct ipx_interface *ifc)
{
	return ntohl(ifc->r_output.dest_addr.sipx_network);
}

static IPXNet
rt_net(struct rip_table *rt)
{
	return ifc_net(rt->router_interface);
}

/* get some ipx socket */
static int
ipx_socket(void)
{
	struct ipx_interface *ifc = first_interface();

	if (first_interface == NULL)
	{
		LOG_START;
		fprintf(log_file, "No ipx socket available\n");
		LOG_END;
		exit(1);
	}
	return ifc->r_output.sk;
}

static int
is_expired(ifc_timer* tm)
{
	return *tm>=EXPIRE_TIME;
}

/* Do I forward packets targeted to rt->network coming over interface
 * ifc?
 */
static int
is_rt_target(struct rip_table *rt, struct ipx_interface *ifc)
{
	return is_expired(rt->timers + ifc_get_index(ifc));
}

/* set up the timers in rt so that only ifc is reachable.
 */
static void
setup_timers(struct rip_table* rt, struct ipx_interface* ifc)
{
	ifc_timer* tm;
	
	for(tm=rt->timers; tm < rt->timers+MAX_IFACE; tm += 1)
	{
		*tm=EXPIRE_TIME;
	}
	rt->timers[ifc_get_index(ifc)]=0;
}

static void
output_flushall()
{
	struct ipx_interface *ifc = first_interface();

	while (ifc != NULL)
	{
		ipx_rip_output_flush(&(ifc->r_output));
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
			ipx_rip_output_set_destination(&(ifc->r_output),
						       dest_node, dest_port);
			ifc = next_interface(ifc);
		}
	}
	else
	{
		ipx_rip_output_set_destination(&(ifc->r_output), dest_node,
					       dest_port);
	}
}

/* Broadcast the existence of the route rt to all interfaces through
 * which rt's target is not directly reachable.
 */
static void
output_broadcast(struct rip_table *rt, int down_allow)
{
	struct ipx_interface *ifc = first_interface();

	while (ifc != NULL)
	{
		if (is_expired(&(rt->timers[ifc_get_index(ifc)])))
		{
			ipx_rip_output_response(&(ifc->r_output),rt->network,
						rt->hops,rt->ticks,down_allow);
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
	fprintf(log_file,"Sending RIP to ");
	ipx_fprint_saddr(log_file,daddr);
	fprintf(log_file,"\n");
	ipx_rip_fdump(log_file,buffer,size);
	DL_END;

	res=sendto(sock,(void*)buffer,size,0,
		   (struct sockaddr*)daddr,sizeof(*daddr));
	if (res==-1)
	{
		LOG_START;
		fprintf(log_file,"sendto: %s\n",strerror(errno));
		LOG_END;
	}
	return res;
}

static void
fprint_route(FILE *file, struct rip_table *rt)
{
	int i;
	
	LOG_START;
	fprintf(file,"dst:");
	ipx_fprint_network(file,rt->network);
	fprintf(file," h:%d t:%d rtr:",rt->hops,rt->ticks);
	ipx_fprint_network(file,rt_net(rt));
	fprintf(file,":");
	ipx_fprint_node(file,rt->router_node);
	fprintf(file," ");

	for (i = first_ifc_index(); i >= 0; i = next_ifc_index(i))
	{
		ifc_timer* tm = &(rt->timers[i]);;
		fprintf(file,"%i",is_expired(tm)?0:1);
		DL_START;
		fprintf(file,"(%i)",(int)*tm);
		DL_END;
	}
	LOG_END;
}

void
fdump_routes(FILE *file)
{
	struct rip_table *rt;
	
	LOG_START;
	fprintf(file,"IPX routing database:\n");
	for(rt=rtable; rt!=NULL; rt=rt->next)
	{
		fprint_route(file,rt);
		fprintf(file,"\n");
	}
	LOG_END;
}


static struct rip_table*
add_route(IPXNet to, hop_t h, tick_t t, struct ipx_interface *ifc,
	  IPXNode rt_node)
{
	struct rip_table *rt;

	rt=(struct rip_table*)malloc(sizeof(struct rip_table));

	if (rt == NULL)
	{
		LOG_START;
		fprintf(log_file,"ipxripd: out of memory in add_route\n");
		LOG_END;
		return NULL;
	}

	rt->network=to;
	rt->hops=h;
	rt->ticks=t;
	rt->router_interface=ifc;
	ipx_assign_node(rt->router_node,rt_node);
	setup_timers(rt,ifc);
	rt->next=rtable;
	rtable=rt;
	return rt;
}

static void
delete_route(struct rip_table* d)
{
	struct rip_table **r;
	for(r=&rtable; *r!=NULL; r=&((*r)->next))
	{
		if (*r==d)
		{
			*r=d->next;
			free(d);
			return;
		}
	}
}

static void
delete_invalid_routes(void)
{
	struct rip_table **r = &rtable;

	while (*r != NULL)
	{
		if ((*r)->hops >= IPX_RIP_NET_DOWN)
		{
			struct rip_table *d = *r;
			*r = (*r)->next;
			free(d);
		}
		else
		{
			r = &((*r)->next);
		}
	}
}

static void
ipx_rip_rt_add(struct rip_table *rt)
{
	if(ipx_kern_route_add(ipx_socket(), rt->network, rt_net(rt),
			      rt->router_node) < 0)
	{
		LOG_START;
		fprintf(log_file,"Cannot add route to network ");
		ipx_fprint_network(log_file,rt->network);
		fprintf(log_file,"\n%s\n",ipx_err_string);
		LOG_END;
	}
}

static void
ipx_rip_rt_del(IPXNet net)
{
	if (ipx_kern_route_delete(ipx_socket(), net)<0)
	{
		LOG_START;
		fprintf(log_file,"Cannot delete route to network ");
		ipx_fprint_network(log_file,net);
		fprintf(log_file,"\n%s\n",ipx_err_string);
		LOG_END;
	}
}

static void
handle_rip_request(struct rip_entry *re, struct ipx_interface *src_ifc)
{
	struct rip_table *cur;

	if (re->network==htonl(IPX_RIP_GENERAL_RQ))
	{
		for(cur=rtable; cur!=NULL; cur=cur->next)
		{
			if (is_rt_target(cur, src_ifc))
			{
				ipx_rip_output_response(&(src_ifc->r_output),
							cur->network,
							cur->hops,
							cur->ticks,0);
			}
		}
	}
	else
	{
		for(cur=rtable; cur!=NULL; cur=cur->next)
		{
			if (   (htonl(re->network)==cur->network)
			    && (   (is_rt_target(cur, src_ifc))
				|| (ifc_net(src_ifc)==htonl(re->network))))
			{
				ipx_rip_output_response(&(src_ifc->r_output),
							cur->network,
							cur->hops,
							cur->ticks,0);
			}
		}
	}
}

static void
handle_rip_response(struct rip_entry *re, struct ipx_interface *src_ifc,
		    IPXNode src_node)
{
	struct rip_table *cur;

	re->hops=ntohs(re->hops)+1;
	re->ticks=ntohs(re->ticks)+src_ifc->ticks;

	for(cur=rtable; cur!=NULL; cur=cur->next)
	{
		if (cur->network != htonl(re->network))
		{
			continue;
		}
		if (cur->network==rt_net(cur))
		{
			/* don't change routes to ifaces */
			return;
		}
		if (re->hops <= IPX_RIP_NET_DOWN)
		{
			if (!(   (re->ticks < cur->ticks)
			      || (   (re->ticks == cur->ticks)
				  && (re->hops  <= cur->hops))))
			{
				/* route re-> is worse */
				return;
			}

			/* route is at least good as
			 * existing one */

			if (   (re->ticks==cur->ticks)
			    && (re->hops==cur->hops))
			{
				/* new route has the same cost - reset timer*/
				if (!(  (ifc_net(src_ifc)==rt_net(cur))
				      && (!ipx_node_equal(cur->router_node,
							  src_node))))
				{
					cur->timers[ifc_get_index(src_ifc)]=0;
				}
			}
			else
			{
				/* new route is better */
				LOG_START;
				fprintf(log_file,"CHANGE ");
				fprint_route(log_file,cur);
				LOG_END;
						
				/* update table */
				cur->ticks=re->ticks;
				cur->hops=re->hops;
				cur->router_interface = src_ifc;
				ipx_assign_node(cur->router_node,
						src_node);
				setup_timers(cur,src_ifc);

				LOG_START;
				fprintf(log_file,"\nto     ");
				fprint_route(log_file,cur);
				fprintf(log_file,"\n");
				LOG_END;
	
				/* update kernel table */
				ipx_rip_rt_add(cur);
				/* send info bcast */
				output_broadcast(cur,0);
			}
		}
		else
		{
			/* network down response */
			if (ifc_net(src_ifc)==rt_net(cur))
			{
				if (ipx_node_equal(cur->router_node, src_node))
				{
					LOG_START;
					fprintf(log_file,"DELETE ");
					fprint_route(log_file,cur);
					fprintf(log_file, " (net down)\n");
					LOG_END;
					/* update kernel table */
					ipx_rip_rt_del(cur->network);
					/* send info bcast */
					cur->hops=IPX_RIP_NET_DOWN;
					output_broadcast(cur,1);
					/* update table */
					delete_route(cur);
				}
				else
				{ /* my router alive */
				}
			}
			else
			{
				cur->timers[ifc_get_index(src_ifc)]
					=EXPIRE_TIME;
			}
		}
		return;
	}
	if (re->hops<=IPX_RIP_NET_DOWN)
	{
		struct rip_table *rt=add_route(htonl(re->network),
					       re->hops,re->ticks,
					       src_ifc,src_node);
		if (rt!=NULL)
		{
			/* update kernel table */
			ipx_rip_rt_add(rt);
			/* send info bcast */
			output_broadcast(rt,0);
			LOG_START;
			fprintf(log_file,"ADD ");	
			fprint_route(log_file,rt);
			fprintf(log_file,"\n");
			LOG_END;
		}
	}
}

void
handle_rip(struct rip_packet *pkt, int len, struct sockaddr_ipx* sipx,
	   struct ipx_interface *src_ifc)
{
	struct rip_entry *re=pkt->rip_entries;
	int nent=(len-2)/8;

	if (ifc_net(src_ifc) != ntohl(sipx->sipx_network))
	{
		LOG_START;
		fprintf(log_file,"RIP from non-local net on ifc ");
		ipx_fprint_network(log_file, ifc_net(src_ifc));
		fprintf(log_file, "\n");
                ipx_fprint_network(log_file,ntohl(sipx->sipx_network));
		fprintf(log_file," (ignored)\n");
		LOG_END;
		return;
	}
	if (len<ipx_rip_size(1))
	{
		LOG_START;
		fprintf(log_file,
			"RIP packet too small len=%i (ignored)\n",len);
		LOG_END;
		return;
	}
	if (ipx_rip_size(nent)!=len)
	{
		LOG_START;
		fprintf(log_file,"RIP packet bad size (ignored)\n");
		LOG_END;
		return;
	}
	if (   (ipx_node_equal(src_ifc->ifnode,sipx->sipx_node))
	    && ((unsigned short)sipx->sipx_port==htons(IPX_RIP_PORT)))
	{
		DL_START;
		fprintf(log_file,"My packet (ignored)\n");
		DL_END;
		return;
	}
	switch (ntohs(pkt->operation))
	{
	case IPX_RIP_OP_REQUEST:
		output_set_destination(src_ifc, sipx->sipx_node,
				       ntohs(sipx->sipx_port));
		for(;nent--;re++)
		{
			handle_rip_request(re, src_ifc);
		}
		ipx_rip_output_flush(&(src_ifc->r_output));
		output_set_destination(src_ifc, IPX_BROADCAST, IPX_RIP_PORT);
		break;
	case IPX_RIP_OP_RESPONSE:
		/* option: ignore responses from non RIP ports
		 * if (sipx->sipx_port!=htons(IPX_RIP_PORT)) return;
		 */
		output_set_destination(NULL, IPX_BROADCAST, IPX_RIP_PORT);
		for(;nent--;re++)
		{
			handle_rip_response(re, src_ifc, sipx->sipx_node);
		}
		output_flushall();
		break;
	default:
		LOG_START;
		fprintf(log_file,"Unknown RIP operation\n");
		LOG_END;
		break;
	} 
}

void
ipx_rip_do_aging(int rate, int do_broadcast)
{
	struct rip_table *cur;
	int routes_died = 0;
	
	DL_START;
	fprintf(log_file,"DO RIP AGING\n");
	DL_END;

	output_set_destination(NULL, IPX_BROADCAST, IPX_RIP_PORT);

	for(cur=rtable; cur!=NULL; cur=cur->next)
	{
		int i;
		ifc_timer* tm;
		for (i = first_ifc_index(); i >= 0; i = next_ifc_index(i))
		{
			tm = &(cur->timers[i]);
			if (!is_expired(tm))
			{
				(*tm)+=rate;
			}
		}
		tm=cur->timers+ifc_get_index(cur->router_interface);

		/* cannot timeout iface !*/
		if (cur->network==rt_net(cur))
		{
			*tm=0;
		}

		if (is_expired(tm))
		{
			/* net is down */
			LOG_START;
			fprintf(log_file,"DELETE ");
			fprint_route(log_file,cur);
			fprintf(log_file," (timed out)\n");
			LOG_END;
			/* update kernel table */
			ipx_rip_rt_del(cur->network);
			/* send info bcast */
			cur->hops=IPX_RIP_NET_DOWN;
			output_broadcast(cur,1);
			/* table to be updated, delayed */
			routes_died = 1;
		}
		else
		{
			if (do_broadcast)
			{
				output_broadcast(cur,0);
			}
		}
	}
	output_flushall();

	/* Delete all bogus routes */
	if (routes_died != 0)
	{
		delete_invalid_routes();
	}
}

int
ipx_rip_init_ifc(struct ipx_interface *ifc, IPXNet network,
		 char *device, int type, void *data)
{
	struct sockaddr_ipx sipx;

	if (ipx_rip_output_init(&(ifc->r_output), network) != 0)
	{
		LOG_START;
		fprintf(log_file,
			"out of memory allocating output buffer\n");
		LOG_END;
		return -1;
	}
	if (add_route(network,1,find_ticks(device),ifc,ifc->ifnode) == NULL)
	{
		LOG_START;
		fprintf(log_file,
			"cannot add route to iface (out of memory)\n");
		LOG_END;
		return -1;
	}

	if ((ifc->r_output.sk = socket(AF_IPX, SOCK_DGRAM, PF_IPX)) < 0)
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
	sipx.sipx_port = htons(IPX_RIP_PORT);
	sipx.sipx_type = IPX_RIP_PTYPE;

	if (bind(ifc->r_output.sk, (struct sockaddr *)&sipx, sizeof(sipx)) < 0)
	{
		LOG_START;
		fprintf(log_file, "can't bind socket: %s\n",
			strerror(errno));
		LOG_END;
		return -1;
	}

	if (ipx_kern_enable_broadcast(ifc->r_output.sk) != 0)
	{
		LOG_START;
		fprintf(log_file, "cant' enable broadcast\n");
		LOG_END;
		return -1;
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
ipx_rip_deinit_ifc(struct ipx_interface *ifc)
{
	close(ifc->r_output.sk);
}
	
static int
ipx_rip_del_old_rt(IPXNet net, IPXNet rt_net, IPXNode node,
		   int type, void *data)
{
	if ((type & IPX_KRT_ROUTE) != 0)
	{
		/* remove old routes */
		ipx_rip_rt_del(net);
	}
	return 0;
}

void
ipx_rip_del_old_routes(void)
{
	ipx_kern_scan_rtable(ipx_rip_del_old_rt, NULL);
}

void
ipx_rip_initial_broadcasts(void)
{
	struct rip_table* rt;
	struct ipx_interface* ifc;

	ipx_rip_output_func=output_sendto;

	output_set_destination(NULL, IPX_BROADCAST, IPX_RIP_PORT);
	for(rt=rtable;rt!=NULL;rt=rt->next)
	{
		output_broadcast(rt,0);
	}
	output_flushall();

	ifc = first_interface();
	while (ifc != NULL)
	{
		ipx_rip_output_request(&(ifc->r_output),
				       IPX_RIP_GENERAL_RQ);
		ifc = next_interface(ifc);
	}
	output_flushall();
}

void
ipx_rip_initial_broadcast(struct ipx_interface *ifc)
{
	struct rip_table *cur;

	output_set_destination(ifc, IPX_BROADCAST, IPX_RIP_PORT);

	for(cur=rtable; cur!=NULL; cur=cur->next)
	{
		if (is_rt_target(cur, ifc))
		{
			ipx_rip_output_response(&(ifc->r_output),
						cur->network,
						cur->hops,
						cur->ticks,0);
		}
		if (cur->router_interface == ifc)
		{
			output_broadcast(cur, 0);
		}
	}
	ipx_rip_output_request(&(ifc->r_output), IPX_RIP_GENERAL_RQ);
	output_flushall();
}
	
void
ipx_rip_down_ifc(struct ipx_interface *ifc)
{
	struct rip_table *rt;
	LOG_ENTRY;
	LOG_START;
	fprintf(log_file, "RIP DOWN INTERFACE ");
	ipx_fprint_network(log_file, ifc_net(ifc));
	fprintf(log_file, "\n");
	LOG_END;

	for (rt = rtable; rt != NULL; rt = rt->next)
	{
		if (rt->router_interface != ifc)
		{
			/* rt is not via ifc */
			continue;
		}
		
		/* cur is via ifc, so down it */
		LOG_START;
		fprintf(log_file,"DELETE ");
		fprint_route(log_file,rt);
		fprintf(log_file, " (ifc down)\n");
		LOG_END;
		/* Update of kernel table unneccessary */

		/* send info bcast */
		rt->hops=IPX_RIP_NET_DOWN;
		output_broadcast(rt,1);
	}
	output_flushall();
	delete_invalid_routes();
	ipx_rip_deinit_ifc(ifc);
}
		
void
ipx_rip_done(void)
{
	struct rip_table* cur;
	
	LOG_ENTRY;
	LOG_START;
	fprintf(log_file,"RIP Shutdown start\n");
	LOG_END;
	
	output_set_destination(NULL, IPX_BROADCAST, IPX_RIP_PORT);
	for(cur=rtable; cur!=NULL; cur=cur->next)
	{
		/* update kernel table (don't delete ifaces)*/
		if (cur->network != rt_net(cur))
		{
			ipx_rip_rt_del(cur->network);
			LOG_START
			fprintf(log_file,"DELETE ");
			fprint_route(log_file,cur);
			fprintf(log_file," (shutdown)\n");
			LOG_END
		}
		/* send info bcast */
		cur->hops=IPX_RIP_NET_DOWN;
		output_broadcast(cur,1);
		/* update table */
		delete_route(cur);
	}
	output_flushall();

	LOG_ENTRY;
	LOG_START;
	fprintf(log_file,"RIP Shutdown end\n");
	LOG_END;
}

