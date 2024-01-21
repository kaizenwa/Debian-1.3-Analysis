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
#include <linux/if_ether.h>
#include <netinet/in.h>
#include <netinet/ip_icmp.h>
#include <netinet/ip.h>
#include <net/if.h>
#include <net/if_route.h>
#include <sys/ioctl.h>
#include <sys/socket.h> 
#include <sys/types.h>
#include <netdb.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>
#include <unistd.h>
#include "rspfd.h"
#include "rspfif.h"
#include "rspf_out.h"
#include "queue.h"
#include "rspfroute.h"


extern struct rspf_mib rspf_stats;
extern struct queue *routerq;
extern struct queue *outfragq;
extern int debug_mode;


/*
 * send_rrh()
 *
 * Send a RRH (Router-Router Hello) onto specifed interface
 *
 * Returns:
 *	Nothing
 * Arguments:
 *	char*: Interface name to send RRH
 */
void send_rrh(char *iface)
{
	static u_char outpack[128];
	struct rspfrrh *rrh =(struct rspfrrh*) outpack;
	u_long saddr, daddr;

	rrh->version = RSPF_VERSION;
	rrh->type = TYPE_RRH;
	rrh->checksum = 0;
	
	if( (rrh->addr = get_iface_addr(iface)) == INADDR_NONE)
	{
		syslog(LOG_DAEMON | LOG_WARNING, "send_rrh(): Cannot get interface %s address.", iface);
		return;
	}
	
	rrh->tx_pkts = htons(get_tx_pkts(iface));
	rrh->flags = 1;		/* wizzer - this needs to be changed */
	
	saddr = rrh->addr;
	
	/* Work out where to send it */
	if( (daddr = get_bcast_addr(iface)) == INADDR_NONE)
	{
		syslog(LOG_DAEMON | LOG_WARNING, "send_rrh(): Cannot get broadcast address for %s.", iface);
		return;
	}

	
	/* Calculate the checksum, we need a pseudo header like TCP/UDP :<
	 */
	rrh->checksum = rspf_check((char*)rrh, RSPF_RRH_LEN, saddr, daddr);	
	
	send_rspf(saddr, daddr, outpack, RSPF_RRH_LEN, iface);
	
	rspf_stats.rspfOutRrhs++;
}

/*
 * send_ping()
 * Sends pings (ICMP echo) to a specified address
 *
 * Returns:
 *	int		The sequence number of the ping
 *
 * Arguments:
 *	u_long		node to ping
 */
int send_ping(u_long daddr, char *iface)
{	
	static u_short pingseq = 1;
	u_char outpack[sizeof(struct icmphdr)];
	struct icmphdr *icp = (struct icmphdr*)&outpack;
	int s;
	struct sockaddr whereto;
	struct sockaddr_in *to = (struct sockaddr_in*) &whereto;
	struct rtentry *rtp;
	struct rtentry rt;
	static int icmp_protocol = -1;
	

	/* We cannot have a sequence number of 0 */
	if (pingseq == 0)
		pingseq++;
			
	/* Create the socket to send the echo on */
	bzero((char*)&whereto, sizeof(struct sockaddr));
	to->sin_family = AF_INET;
	to->sin_addr.s_addr = daddr;
	
	if ( icmp_protocol < 0 ) {
		/*
		 * The protocol won't change while the daemon's running,
		 * so there's no reason to read /etc/protocols repeatedly.
		 */
		struct protoent *proto;
		if ((proto = getprotobyname("icmp")) == NULL) {
			syslog(LOG_DAEMON | LOG_CRIT,
			 "send_ping(): unknown protocol icmp (%m)");
			return 0;
		}
		icmp_protocol = proto->p_proto;
	}
	if ((s = socket(AF_INET, SOCK_RAW, icmp_protocol)) < 0) {
		syslog(LOG_DAEMON | LOG_ERR, "send_ping(): socket failed (%m)");
		return 0;
	}
	
	/* Create ICMP header */
	icp->type = ICMP_ECHO;
	icp->code = 0;
	icp->checksum = 0;
	icp->un.echo.id = getpid();
	icp->un.echo.sequence = pingseq++;
	icp->checksum = in_cksum((u_short*)outpack, sizeof(struct icmphdr));

	/*
	 * We have to set a dummy route temporarily, if none is there
	 */
	if ( (rtp = get_route(daddr, 32)) != NULL)
	{
		rt = *rtp;
		add_route(daddr, 32, 0, 1);
		sendto(s, outpack, sizeof(struct icmphdr), 0, &whereto, sizeof(struct sockaddr));
		/*
		 * Restore the route entry
		 */
		if (ioctl(s, SIOCADDRT, &rt) < 0)
		{
			syslog(LOG_DAEMON | LOG_ERR, "send_ping(): ioctl to restor route failed. (%m)");
		}
	}			
	else
	{
		/* No route, so we can go for it */
		add_route(daddr, 32, 0 ,1);
		sendto(s, outpack, sizeof(struct icmphdr), 0, &whereto, sizeof(struct sockaddr));
		del_route(daddr, 32);
	}
	close(s);
	return icp->un.echo.sequence;
}	

/*
 * send_news()
 *
 * Sends a good or bad news packet about one link on all interfaces
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	u_long		Source address of link
 *	u_long		Destination address of link
 *	u_char		Significant bits of dest addr
 *	u_char		Cost of link
 *	char*		Name of port (ignored - wizzer remove it)
 */
void send_news(u_long saddr, u_long daddr, u_char sigbits, u_char cost, char *port)
{
	u_char buf[RSPFNODE_LEN + RSPFLINK_LEN + RSPFADJ_LEN];
	struct rspfnode_hdr *nodehdr = (struct rspfnode_hdr*)buf;
	struct rspflink_hdr *linkhdr = (struct rspflink_hdr*)(buf + RSPFNODE_LEN);
	struct rspfadj_hdr *adjhdr = (struct rspfadj_hdr*)(buf + RSPFNODE_LEN + RSPFLINK_LEN);
	
	char ifbuf[256];
	char *ifname;
	int ifcount;
	
	bcopy((char*)&saddr, nodehdr->addr, 4);
	nodehdr->seq_no = htons(rspf_stats.SequenceNumber);
	nodehdr->sub_seq_no = ++rspf_stats.SubSequenceNumber;
	nodehdr->links = 1;
	
	/* Wizzer get horizon */
	linkhdr->horizon = get_horizon(daddr, port);
	linkhdr->erp = 0;
	linkhdr->cost = cost;
	linkhdr->adjacencies = 1;
	
	adjhdr->sig_bits = sigbits | RSPFADJ_LASTFLAG;
	bcopy((char*)&daddr, adjhdr->addr, 4);

	/* Send to get stuck in an envelope and send */
	ifname = ifbuf;
	ifcount = rspf_ifaces(ifbuf, sizeof(ifbuf));
	while(ifcount-- > 0)
	{
		send_rspf_env(buf, sizeof(buf), 1, ifname);
		ifname += strlen(ifname) + 1;
	}
		
}

int send_rspf(u_long saddr, u_long daddr, u_char *buf, int buflen,  char *iface)
{
	struct sockaddr_in sin;
	int skt;
	int i;
/*	FILE *fp;*/
	static int rspf_protocol = -1;

	if ( rspf_protocol < 0 ) {
		/*
		 * No need to scan /etc/protocols more than once, and
		 * we can fall back on our hard-coded knowledge if
		 * RSPF isn't defined in /etc/protocols.
		 */
		struct protoent *proto;
		if ( (proto = getprotobyname("rspf")) == NULL)
			rspf_protocol = 73; /* Fall back on hard-coded number */
		else
			rspf_protocol = proto->p_proto;
	}
			
	if ( (skt = socket(AF_INET, SOCK_RAW, rspf_protocol)) < 0) {
		perror("RSPFd: send_rrh(): socket");
		close(skt);
		return -1 ;
	}
	
	/* We have to tell the system we really want to send a b'cast packet */
	i = 1;
	if (setsockopt(skt, SOL_SOCKET, SO_BROADCAST, (char*)&i, sizeof(i)) < 0) {
		perror("RSPFd: send_rrh(): setsockopt");
		close(skt);
		return -1;
	}
	
	/* Copy over address */
	if((sin.sin_addr.s_addr = get_bcast_addr(iface) )  == INADDR_NONE)
	{
		syslog(LOG_DAEMON | LOG_WARNING, "send_rspf(): Cannot get broadcast address for interface %s.", iface);
		close(skt);
		return -1;
	}
	
	sin.sin_family = AF_INET;
	i = sendto(skt, buf, buflen, 0, (struct sockaddr*)&sin, sizeof(struct sockaddr) );
	if (i < 0) {
		syslog(LOG_DAEMON | LOG_ERR,"send_rspf(): sendto failed (%m)");
		close(skt);
		return -1;
	}
	rspf_stats.rspfOutMsgs++;
	close(skt);
	return 0; 
} /* send_rspf */


/*
 * send_full_bulletin
 *
 * Sends a full routing bulletin on the specified interface
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	char*	Name of interface, NULL means all interfaces
 */
void send_full_bulletin(char *iface)
{
	struct router *rtr;
	qmark rtr_qm;
	u_char buf[4096];
	int bufcnt, nodecnt, tmpbuf, tmpnode, size;
	u_long addr;
	
	char ifbuf[256];
	char *ifname;
	int ifcount;
	
	bufcnt = 0;
	nodecnt = 0;



	rspf_stats.SubSequenceNumber = 0;

	/* Get routing bulletins from other systems */	
	rtr = (struct router*)qmove_first(routerq, &rtr_qm);
	while (rtr != NULL)
	{
		if ( (size = get_rtr_bull(rtr->addr, buf + bufcnt, sizeof(buf) - bufcnt)) > 0)
		{
			bufcnt += size;
			nodecnt++;
		}
		rtr = (struct router*)qmove_next(routerq, &rtr_qm);		
	}

	/*
	 * Send it on each interface, if it is not specified
	 * We have to generate our bulletin each time for this
	 */
	if (iface != NULL)
	{
		if ((addr = get_iface_addr(iface)) == INADDR_NONE)
		{
			syslog(LOG_ERR | LOG_DAEMON, "send_full_bulletin(): Cannot find interface address for %s (%m).", iface);
		} else {
			if ((size = get_my_bull(addr, buf + bufcnt, sizeof(buf)- bufcnt)) > 0)
			{
				nodecnt++;
				bufcnt += size;
			}
			send_rspf_env(buf, bufcnt, nodecnt, iface);
		}
	} else {
		tmpnode = nodecnt;
		tmpbuf = bufcnt;
		
		ifcount = rspf_ifaces(ifbuf, 256);
		ifname = ifbuf;
		while (ifcount-- > 0) 
		{
			if ((addr = get_iface_addr(ifname)) == INADDR_NONE)
			{
				syslog(LOG_ERR | LOG_DAEMON, "send_full_bulletin(): Cannot find interface address for %s (%m).", iface);
			} else {
				if ((size = get_my_bull(addr, buf, sizeof(buf))) > 0)
				{
					nodecnt++;
					bufcnt += size;
				}
				send_rspf_env(buf, bufcnt, nodecnt, ifname);
			}
		
			ifname += strlen(ifname) + 1;
			nodecnt = tmpnode;
			bufcnt = tmpbuf;
		} /* while ifcount */
	} /* no specified interface */

} /* send_full_bulletin() */
	
void send_rspf_env(u_char *buf, int size, int nodecnt, char *iface)
{
	char outbuf[2048];
	char *startbuf, *endbuf;
	struct rspfroute_hdr *rthdr = (struct rspfroute_hdr*)outbuf;
	u_long saddr, daddr;
	struct ifreq ifr;
	int sync, frag_tot, mtu, frag;
	u_int nodes, links, adjs;
	int skt;
	struct rspfnode_hdr *nodehdr;
	struct rspflink_hdr *linkhdr;
	struct outfrag *frg;
	qmark ofrg_qm;

	if (debug_mode)
		printf("send_rspf_env(): Sending packet %d bytes, %d nodes onto port %s.\n", size, nodecnt, iface);
	/* Check that we have an interface */
	if (iface == NULL)
	{
		syslog(LOG_DAEMON | LOG_ERR, "send_rspf_env(): No interface specified.");
		return;
	}
	/* Create socket */
	if ( (skt = socket(AF_INET, SOCK_DGRAM,0)) < 0) 
	{
 		syslog(LOG_DAEMON | LOG_ERR, "send_rspf_env(): socket failed. (%m)");
 		(void) close(skt);
 		return;
 	}
	/* Fill in what we can in routing envelope header */
	rthdr->version = RSPF_VERSION;
	rthdr->type = TYPE_ROUTING;
	rthdr->checksum = 0;
	rthdr->nodes = nodecnt;
	rthdr->env_no = htons(rspf_stats.EnvelopeNumber++);
		
	strcpy(ifr.ifr_name, iface);
	frag = frag_tot = 1;	
	nodes = nodecnt;
	sync = 0;
	endbuf = startbuf = buf;
	
	/* We now chop up the packet into MTU sized fragments */
	if (ioctl(skt, SIOCGIFMTU, &ifr) < 0)
	{
		syslog(LOG_DAEMON | LOG_WARNING, "send_rspf_env(): ioctl to get MTU failed for port %s (%m)", ifr.ifr_name);
		return;
	}
	mtu = ifr.ifr_mtu - RSPFROUTE_LEN ;
		
	if (mtu < RSPFNODE_LEN)
	{
		syslog(LOG_DAEMON | LOG_WARNING, "send_rspf_env(): Interface %s has too small MTU of %d.", ifr.ifr_name, ifr.ifr_mtu);
		return;
	}
	
	/* Now move through the buffer */
	while(nodes-- > 0)
	{
		if ( (endbuf - startbuf) + RSPFNODE_LEN > mtu)
		{
			/* Too big, have to send it */
			add_outfrag(frag, sync, startbuf, endbuf);
			sync = 0;
			frag++;
			startbuf = endbuf;
		}
		nodehdr = (struct rspfnode_hdr*)endbuf;
		/* Get relative value of sync byte, if not already set */
		if (sync == 0)
			sync = 4 + ((int)endbuf - (int)buf);
		endbuf += RSPFNODE_LEN;
		if ((long)endbuf - (long)buf > size)
		{
			syslog(LOG_ERR | LOG_DAEMON, "send_rspf_env(): Overrun buffer at node %d", nodes + 1);
			nodes = 0;
			break;
		}			
		links = nodehdr->links;
		if (debug_mode)
			printf("send_rspf_env(): Node %d has %d links, header at %u bytes.\n", nodes + 1, links, (u_int)((long)nodehdr - (long)buf));
		while(links-- > 0)
		{
			if ( (endbuf - startbuf) + RSPFLINK_LEN > mtu)
			{
				/* Too big, have to send it */
				add_outfrag(frag, sync, startbuf, endbuf);
				sync = 0;
				frag++;
				startbuf = endbuf;
			}
			linkhdr = (struct rspflink_hdr*)endbuf;
			endbuf += RSPFLINK_LEN;
			if ((long)endbuf - (long)buf > size)
			{
				syslog(LOG_ERR | LOG_DAEMON, "send_rspf_env(): Overrun buffer at node %d, link %d.", nodes + 1, links + 1);
				links = 0;
				nodes = 0;
				break;				
			}
			adjs = linkhdr->adjacencies;
			if (debug_mode)
				printf("send_rspf_env(): Link %d of node %d has %d adjacencies, header at %u bytes.\n", links + 1, nodes + 1, adjs, (u_int)((long)linkhdr - (long)buf));
			while(adjs-- > 0)
			{
				if ( (endbuf - startbuf) + RSPFLINK_LEN > mtu)
				{
					/* Too big, have to send it */
					add_outfrag(frag, sync, startbuf, endbuf);
					sync = 0;
					frag++;
					startbuf = endbuf;
				}
			 	endbuf += RSPFADJ_LEN;
			 	if ((long)endbuf - (long)buf > size)
			 	{
			 		syslog(LOG_ERR | LOG_DAEMON, "send_rspf_env(): Oveerun buffer at node %d, link %d, adj %d", nodes + 1, links + 1, adjs + 1);
					nodes = 0;
					links = 0;
					adjs = 0;
					break;
				}
							 		
			 }/*adjs*/
		} /*links*/
	} /* nodes */
	/* Add last fragment, if there is one */
	if (endbuf > startbuf)
	{
		add_outfrag(frag, sync, startbuf, endbuf);
		frag++;
	}
	/* Now have all the fragments in the queue, we know how many fragments
	 * we need, so we can send them
	 */
	/* Get source and destination addresses */
	if( (daddr = get_bcast_addr(ifr.ifr_name)) == INADDR_NONE)
	{
		syslog(LOG_DAEMON | LOG_WARNING, "send_rspf_env(): Cannot get broadcast address for interface %s.", ifr.ifr_name);
		return;
	}

	if( (saddr = get_iface_addr(ifr.ifr_name)) == INADDR_NONE)
	{
		syslog(LOG_DAEMON | LOG_WARNING, "send_rspf_env(): Cannot get address for interface %s.", ifr.ifr_name);
		return;
	}

	if (debug_mode)
		printf("send_rspf_env(): Have %d fragments.\n", frag - 1);
		
	frag_tot = frag - 1;
	frag = 1;
	frg = (struct outfrag*)qmove_first(outfragq, &ofrg_qm);
	while(frg != NULL)
	{

		if (frag > frag_tot)
		{
			syslog(LOG_DAEMON | LOG_ERR, "send_rspf_env(): fragment (%d) is larger than total (%d).", frag, frag_tot);
			break;
		}			
		rthdr->frag = frag++;
		rthdr->frag_tot = frag_tot;
		rthdr->sync = sync < 256 ? sync : 0;
		if (frg->datalen > sizeof(outbuf) - RSPFROUTE_LEN)
		{
			syslog(LOG_DAEMON | LOG_ERR, "send_rspf_env(): Fragment of %d bytes too big, dropping.");
		} else {			
			bcopy(frg->data, outbuf + RSPFROUTE_LEN, frg->datalen);
			rthdr->checksum = 0;
			rthdr->checksum = rspf_check(outbuf, RSPFROUTE_LEN + frg->datalen, saddr, daddr);				
			send_rspf(saddr, daddr, outbuf, RSPFROUTE_LEN + frg->datalen, ifr.ifr_name);
		}
		del_qnode(outfragq, ofrg_qm, 1);		
		frg = NULL;			
		/* Use move first because we're destructing as we go */
		frg = (struct outfrag*)qmove_first(outfragq, &ofrg_qm);
	}
	rspf_stats.rspfOutRouteEnvs++;
	(void) close(skt);
	
	/* Remove all fragments that are left */
	while (qmove_first(outfragq, &ofrg_qm) != NULL)
		del_qnode(outfragq, ofrg_qm, 1);
}	
			
void add_outfrag(int frag, int sync, u_char *startbuf, u_char *endbuf)
{
	struct outfrag *frg;
	int size = endbuf - startbuf;
	
	if (size <= 0)
	{
		syslog(LOG_DAEMON | LOG_ERR, "add_outfrag() fragment size of %d (%m)", size);
		return;
	}
	
	frg = (struct outfrag*)malloc(sizeof(struct outfrag) + size);
	if (frg == NULL)
	{
		syslog(LOG_DAEMON | LOG_ERR, "add_outfrag() Memory sequeze (%m)");
		return;
	}
	frg->frag = frag;
	frg->sync = sync;
	frg->data = (u_char*)frg + sizeof(struct outfrag);
	frg->datalen = size;
	
	if (debug_mode)
		printf("add_outfrag(): Block copying %d bytes into buffer for frag %d.\n", size, frag);
	bcopy(startbuf, frg->data, size);

	add_qnode(outfragq, (void*)frg, NULL);
}
