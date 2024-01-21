/*
 * rspfif.c: This is all the interface routines
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
#include <sys/ioctl.h>
#include <linux/if.h> 
#include <linux/ax25.h>
#include <stdio.h> 
#include <stdlib.h>
#include <syslog.h>
#include <unistd.h>
#include <netinet/in.h>

#include "queue.h"
#include "rspfif.h"
#include "rspfd.h"

struct queue *ifqueue;
extern struct rspf_mib rspf_stats;

void iface_setup(void)
{
	ifqueue = create_queue();
}		


void add_iface(char *name, u_char cost)
{
	struct rspf_if *ptr;
	qmark if_qm;
	
	ptr = (struct rspf_if*)qmove_first(ifqueue, &if_qm);
	while(ptr != NULL)
	{
		if ( strcmp(ptr->name, name) == 0)
			break;
		ptr = (struct rspf_if*)qmove_next(ifqueue, &if_qm);
	}
	if (ptr == NULL)
	{
		/* New interface */
		ptr = (struct rspf_if*)malloc(sizeof(struct rspf_if));
		if (ptr == NULL) {
			fprintf(stderr, "RSPFd: add_iface(): Memory squeze.\n");
			return;
		}
		strcpy(ptr->name, name);
		rspf_stats.rspfCurrIfaces++;
	}
	ptr->cost = cost;
	/* if name is the key */
	add_qnode(ifqueue, (void*)ptr, name);
}

/*
 * is_rspfif()
 *
 * Determines if the given interface has been configured for RSPF or not.
 * 
 * Returns:
 *	int	0 if not RSPF interface otherwise 1
 *
 * Arguments:
 *	char*	name of interface to check
 */
int is_rspfif(char *name)
{
	qmark qm;
	
	return (qfind_first(ifqueue, name, &qm) != NULL);
}


/*
 * rspf_ifaces()
 *
 * Finds all interfaces that have been configured for RSPF
 *
 * Returns:
 *	names of the interfaces, this occurs in a given pointer
 *	names are separated by a space (ASCII 32)
 *
 * Arguments:
 *	char*	The string to put it in
 *	int	Length of the string
 *
 * Caveats:
 *	The caller must free the pointer afterwards
 */
int rspf_ifaces(char *buf, int buflen)
{
	char *bufptr = buf;
	struct rspf_if *ptr;
	int ifcount = 0;
	qmark if_qm;

		
	ptr = (struct rspf_if*)qmove_first(ifqueue, &if_qm);
	while ((ptr != NULL) && (bufptr - buf < buflen) ) {
		ifcount++;
		strcpy(bufptr, ptr->name);
		bufptr += strlen(ptr->name);
		*bufptr++ = '\0';
		ptr = (struct rspf_if*)qmove_next(ifqueue, &if_qm);
	}
	*bufptr = 0;
	return ifcount;
}
	
u_char get_iface_cost(char *port)
{
	struct rspf_if *ifptr;
	qmark if_qm;
	
	ifptr = (struct rspf_if*)qmove_first(ifqueue, &if_qm);
	while(ifptr != NULL)
	{
		if (strcmp(ifptr->name, port) == 0)
			return ifptr->cost;
		ifptr = (struct rspf_if*)qmove_next(ifqueue, &if_qm);
	}
	/* Default is almost infinity */
	return 254;
}



/*
 * get_tx_pkts()	
 *
 * Find the number of packets that we've sent on an interface modulus 65536
 *
 * Returns:
 *	Number of packets sent or 0 if an error
 *
 * Arguments:
 *	char *iface: Name of interface
 *
 * Caveats:
 *	Uses the /proc/net/dev file and it's structure
 */
int get_tx_pkts(char *iface)
{
	char buf[128];
	FILE *fp;
	int tx_pkts;
	char ifname[IFNAMSIZ];
	
	fp = fopen("/proc/net/dev", "r");
	
	if (fp == NULL)	{
		syslog(LOG_DAEMON | LOG_ERR, "get_tx_pkts(): Could not open /proc/net/dev. (%m)");
		return 0;
	}
	/* Read and toss first two lines */	
	if (fgets(buf, 128, fp) == NULL) {
		fclose(fp);		
		return 0;
	}
	if (fgets(buf, 128, fp) == NULL) {
		fclose(fp);
		return 0;		
	}
	while (fgets(buf, 128, fp) != NULL) {
		if (sscanf(buf, "%*[ ]%[^:]%*c %*d %*d %*d %*d %*d %d", ifname, &tx_pkts) == 2) {
			if (strcmp(ifname, iface) == 0) {
				fclose(fp);
				return (tx_pkts & 0xffff);
			}
		}
	}
	syslog(LOG_DAEMON | LOG_WARNING, "Cannot find interface %s.\n", iface);
	fclose(fp);
	return 0;
} /* get_tx_pkts() */	

	


/*
 * get_bcast_addr()
 *
 * Finds the broadcast address for given interface
 *
 * Returns:
 *	u_long: broadcast address or INADDR_NONE if not found
 *
 * Arguments:
 *	char*: Interface name
 */
u_long get_bcast_addr(char *ifname)
{
 	int skt;
 	struct ifreq ifr;
 	struct sockaddr_in *sin;
 	
 	/* Put the interface name into the ifreq struct */
 	strcpy(ifr.ifr_name, ifname);
 	
	
 	if ( (skt = socket(AF_INET, SOCK_DGRAM,0)) < 0) {
 		syslog(LOG_DAEMON | LOG_ERR, "get_bcast_addr(): socket failed. (%m)");
 		(void) close(skt);
 		return INADDR_NONE;
 	}
 	if (ioctl(skt, SIOCGIFBRDADDR, &ifr) < 0) {
 		syslog(LOG_DAEMON | LOG_ERR, "get_bcast_addr(): ioctl failed for %s. (%m)", ifname);
 		(void) close(skt);
 		return INADDR_NONE;
 	}
	
	(void) close(skt);
	
	sin = (struct sockaddr_in*)&ifr.ifr_broadaddr;
	return sin->sin_addr.s_addr;
}

/*
 * get_iface_addr()
 *
 * Finds the interface address for given interface
 *
 * Returns:
 *	struct sockaddr: interface address or "0" if not found
 *
 * Arguments:
 *	char*: Interface name
 */
u_long get_iface_addr(char *ifname)
{
 	int skt;
 	struct ifreq ifr;
 	struct sockaddr_in *sin;
 	
 	/* Put the interface name into the ifreq struct */
 	strcpy(ifr.ifr_name, ifname);
 	
 	if ( (skt = socket(AF_INET, SOCK_DGRAM,0)) < 0) {
 		syslog(LOG_DAEMON | LOG_ERR, "get_iface_addr(): socket failed for %s. (%m)", ifname);
 		(void) close(skt);
 		return INADDR_NONE;
 	}
 	if (ioctl(skt, SIOCGIFADDR, &ifr) < 0) {
 		syslog(LOG_DAEMON | LOG_ERR, "get_iface_addr(): ioctl failed. (%m)");
 		(void) close(skt);
 		return INADDR_NONE;
 	}
	
	(void) close(skt);
	sin = (struct sockaddr_in*)&ifr.ifr_addr;
	return sin->sin_addr.s_addr;
	
}	

void clear_all_ifaces()
{
	struct rspf_if *ptr;
	qmark qm;
	
	while ( (ptr = (struct rspf_if*)qmove_first(ifqueue, &qm)) != NULL)
		del_qnode(ifqueue, qm, 1);
	
	rspf_stats.rspfCurrIfaces = 0;
		
}