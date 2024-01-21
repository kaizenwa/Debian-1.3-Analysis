/* 
 * rspfd: Radio Shortest Path Daemon. A router for packet radio networks.
 * Copyright (C) 1995 Craig Small VK2XLZ
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#include <arpa/inet.h>
#include <linux/ax25.h>
#include <netinet/in.h>
#include <net/if_route.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <unistd.h>

#include "rspfd.h"
#include "rspfroute.h"
#include "queue.h"

extern struct queue *adj_queue;
extern struct queue *rt_queue;
extern struct queue *nodgrpq;
extern int debug_mode;

void del_route(u_long daddr, u_char sigbits)
{
	struct rtentry rte;
	struct rspf_route *rt;
	qmark rtqm;
	struct sockaddr_in* sin;
	int skt;
	
	bzero((char*)&rte, sizeof(rte));
	
	sin = (struct sockaddr_in*)&rte.rt_dst;
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = daddr;
	
	if (sigbits > 32)
	{
		syslog(LOG_DAEMON | LOG_ERR, "del_route(): %s has wrong sigbits of %u\n", in_ntoa(daddr), sigbits);
		return;
	}		
	sin = (struct sockaddr_in*)&rte.rt_genmask;
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = htonl(INADDR_NONE << (32 - sigbits)) ;
 	
 	if ( (skt = socket(AF_INET, SOCK_DGRAM,0)) < 0) 
	{
 		syslog(LOG_DAEMON | LOG_ERR, "add_route(): socket failed. (%m)");
 		(void) close(skt);
 		return;
 	}
 	
 	if (ioctl(skt, SIOCDELRT, &rte) < 0)
 	{
 		syslog(LOG_DAEMON | LOG_ERR, "del_route(): ioctl failed with address %s. (%m)", in_ntoa(daddr));
 		close(skt);
 		return;
 	}
 	close(skt);
 	rt = (struct rspf_route*)qmove_first(rt_queue, &rtqm);
 	while(rt != NULL)
 	{
 		if(rt->addr == daddr && rt->sigbits == sigbits)
 		{
 			del_qnode(rt_queue, rtqm, 1);
 			return;
 		}
 		rt = (struct rspf_route*)qmove_next(rt_queue, &rtqm);
 	}
 	return;
}
	
void nuke_routes()
{
	struct rtentry rte;
	struct rspf_route *rt;
	qmark rtqm;
	int skt;
	
	if ( (skt = socket(AF_INET, SOCK_DGRAM,0)) < 0) 
	{
 		syslog(LOG_DAEMON | LOG_ERR, "del_route(): socket failed. (%m)");
 		(void) close(skt);
 		return;
 	}
 	
	rt = (struct rspf_route*)qmove_first(rt_queue, &rtqm);
	while(rt != NULL)
	{
		bzero((char*)&rte, sizeof(rte));
		bcopy((char*)&rt->addr, &rte.rt_dst, 4);
	 	if (ioctl(skt, SIOCDELRT, &rt) < 0)
 		{
 			syslog(LOG_DAEMON | LOG_ERR, "del_route(): ioctl failed. (%m)");
	 		close(skt);
 			return;
	 	}	
		del_qnode(rt_queue, rtqm, 1);
		
		rt = (struct rspf_route*)qmove_first(rt_queue, &rtqm);
	}
	close(skt);
} /* nuke_routes() */	

struct rtentry *get_route(u_long addr, u_char sigbits)
{
	static struct rtentry rt;
	static char rtdev[20];
	u_long daddr, gaddr, maddr;
	ulong netmask;
	FILE *fp;
	char buf[256];
	struct sockaddr_in *sin;
	
	if ( (fp = fopen("/proc/net/route", "r")) == NULL)
	{
		syslog(LOG_ERR | LOG_DAEMON, "get_route_cost(): Cannot open proc route file (%m)");
		return NULL;
	}
	bzero ((char*)&rt, sizeof(rt));
	rt.rt_dev = rtdev;
	
	if (sigbits > 32)
	{
		syslog(LOG_DAEMON | LOG_ERR, "get_route(): %s has wrong sigbits of %u\n", in_ntoa(daddr), sigbits);
		return NULL;
	}		
	netmask = htonl(INADDR_NONE << (32 - sigbits)) ;
	
	fgets(buf, 256, fp);
	
	while(fgets(buf, 256, fp) != NULL)
	{
		if (sscanf(buf, "%20s %lX %lX %hX %hd %lu %hd %lX %lu %lu", 
			rt.rt_dev,&daddr, &gaddr, &rt.rt_flags, &rt.rt_refcnt,
			&rt.rt_use, &rt.rt_metric, &maddr, &rt.rt_mss, &rt.rt_window) != 10)
			
			continue;

		/*
		 * If it is a default route, ignore netmask to get around some
		 * quirks
		 */
		 			
		if (daddr != addr || (addr != 0 && maddr != netmask) )
			continue;
		fclose(fp);			
		/*
		 * Now fix up the addresses
		 */
		sin = (struct sockaddr_in*)&rt.rt_dst;
		sin->sin_family = AF_INET;
		sin->sin_addr.s_addr = daddr;
		
		sin = (struct sockaddr_in*)&rt.rt_gateway;
		sin->sin_family = AF_INET;
		sin->sin_addr.s_addr = gaddr;
		
		sin = (struct sockaddr_in*)&rt.rt_genmask;
		sin->sin_family = AF_INET;
		sin->sin_addr.s_addr = maddr;
		
		return &rt;
	}
	fclose(fp);
	return NULL;
}
			
	


/*
 * add_route()
 *
 * Adds a route to the kernels routing table
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	u_long		destination address of route
 *	u_char		significant bits of destination 
 *	u_long		address of gateway (0 if none)
 *	u_char		cost (metric) of route
 */
void add_route(u_long daddr, u_char sigbits, u_long gateway, u_char cost)
{
	struct rtentry rte;
	struct rtentry *oldrt;
	struct rspf_route *rt;
	struct nodegroup *ng;
	qmark rtqm, adj_qm, ng_qm;
	struct sockaddr_in *sin;
	int skt;
	char key[KEY_SIZE];
	char iface[IFNAMSIZ];
	struct rspf_adj *aptr;
	
	if (debug_mode)
	{
		printf("add_route(): addr = %s/%u  ", in_ntoa(daddr), sigbits);
		printf(" gate = %s  cost %u.\n", (gateway? in_ntoa(gateway): "NONE"),  cost);	
	}
	/*
	 * See if we already have better route
	 * Currently, the definition of this is a route without a dynamic
	 * flag.  We will make it one with a lower metric later when the
	 * kernel supports metrics.
	 */
	if ( (oldrt = get_route(daddr, sigbits)) != NULL )
	{
/*		if (oldrt->rt_metric < cost)
			return;*/
		if (!(oldrt->rt_flags & RTF_DYNAMIC))
		{
			if (debug_mode)
				printf("Not adding route, already have static one in kernel table.\n");
			return;
		}
	}  
	
	/*
	 * Now try to find what interface to use
	 */
	if (gateway)
		strcpy(key, in_ntoa(gateway));
	else
		strcpy(key, in_ntoa(daddr));
		
	ng = (struct nodegroup*)qmove_first(nodgrpq, &ng_qm);
	while(ng != NULL)
	{
		if (ng->addr == daddr)
			break;
			
		ng = (struct nodegroup*)qmove_next(nodgrpq, &ng_qm);
	}
	if ( (aptr = (struct rspf_adj*)qfind_first(adj_queue, key, &adj_qm)) != NULL)
	{
		strncpy(iface, aptr->port, IFNAMSIZ);
	}
	else 
	{
		if (ng != NULL)
		{
			strncpy(iface, ng->iface, IFNAMSIZ);
		}
		else
		{	
			if (gateway)
				syslog(LOG_DAEMON | LOG_WARNING, "add_route(): Cannot find interface for gateway %s for route to %s", key, in_ntoa(daddr));
			else					
				syslog(LOG_DAEMON | LOG_WARNING, "add_route(): Cannot find interface for host to %s", key);
			return;			
		}			
	}
	/*
	 * Add route to kernel table
	 */
	/*
	 * Zero rtentry
	 */
	bzero((char*)&rte, sizeof(rte));
	
	sin = (struct sockaddr_in*)&rte.rt_dst;
	sin->sin_family = AF_INET;
	sin->sin_port = 0;
	sin->sin_addr.s_addr = daddr;
	
	if (gateway)
	{
		sin = (struct sockaddr_in*)&rte.rt_gateway;
		sin->sin_family = AF_INET;
		sin->sin_addr.s_addr = gateway;
	}
	
	sin = (struct sockaddr_in*)&rte.rt_genmask;
	sin->sin_family = AF_INET;
	/*
	 * We have to convert from sigbits to netmask
	 */
	if (sigbits > 32)
	{
		syslog(LOG_DAEMON | LOG_ERR, "add_route(): %s has wrong sigbits of %u\n", in_ntoa(daddr), sigbits);
		return;
	}		
	sin->sin_addr.s_addr = htonl(INADDR_NONE << (32 - sigbits)) ;
	
	rte.rt_flags = RTF_UP | RTF_DYNAMIC;
	if (sigbits == 32)
		rte.rt_flags |= RTF_HOST;
	if (gateway)
		rte.rt_flags |= RTF_GATEWAY;
	
	rte.rt_metric =	cost;
	
	rte.rt_dev = iface;
	

 	if ( (skt = socket(AF_INET, SOCK_DGRAM,0)) < 0) 
	{
 		syslog(LOG_DAEMON | LOG_ERR, "add_route(): socket failed. (%m)");
 		(void) close(skt);
 		return;
 	} 	
 	if (ioctl(skt, SIOCADDRT, &rte) < 0)
 	{
 		syslog(LOG_DAEMON | LOG_ERR, "add_route(): ioctl failed. (%m)");
 		close(skt);
 		return;
 	}
 	close(skt);
 	/*
 	 * Add route to internal table
 	 */
	rt = (struct rspf_route*)qmove_first(rt_queue, &rtqm);
	while (rt != NULL)
	{
		if (rt->addr == daddr && rt->sigbits == sigbits)
			break;
		rt = (struct rspf_route*)qmove_next(rt_queue, &rtqm);
	}
	if (rt == NULL)
	{
		/* New route */
		if ( (rt = (struct rspf_route*)malloc(sizeof(struct rspf_route))) == NULL)
		{
			syslog(LOG_ERR | LOG_DAEMON, "add_route(): malloc failed (%m)");
			return;
		}
		rt->addr = daddr;
		rt->sigbits = sigbits;
		strcpy(rt->port, iface);
		add_qnode(rt_queue, (void*)rt, NULL);
		
	}
	rt->cost = cost;
	

 	return;
} /* add_route() */

	