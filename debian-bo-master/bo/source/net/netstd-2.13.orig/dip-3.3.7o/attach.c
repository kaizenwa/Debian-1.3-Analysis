/*
 * dip		A program for handling dialup IP connecions.
 *		This module handles setting the interface and
 *		its routing table entry.
 *
 * Version:	@(#)attach.c	3.3.6	03/30/95
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1993 MicroWalt Corporation
 *
 * Modified:    Uri Blumenthal, <uri@watson.ibm.com>
 *              (C) 1994, 1995
 *
 *		Paul Cadach, <paul@paul.east.alma-ata.su>
 *		(C) 1994
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 * 
 *		This code uses some routines from "pppd" code developed
 * 		by Carnegie Mellon University.
 *		Here's their Copyright:
 *
 * Copyright (c) 1989 Carnegie Mellon University.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Carnegie Mellon University.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "dip.h"


static struct dip  _dip;		/* Need for 'config' support */

#ifdef PAUL_SPECIAL
static int          attached = 0;	/* Need for "smart" detach() */

static int
config(int when, int direction, char *buff, char *name)
{
  struct ifcfg *c;
  char         *s, *p;

  if((c = cfg) != (struct ifcfg *)NULL)
  {
    do
    {
      if(direction == 0)
        c = c->prev;
      if(c->when == when) {
        s = stpcpy(buff, (c->type == IFC_CONFIG) ? _PATH_BIN_IFCONFIG :
                   _PATH_BIN_ROUTE);
        p = c->param;

        while(*p) {
          if(*p == '%') {
            switch(*++p) {
              case 'I':
              case 'i':
                   s = stpcpy(s, name);
                   break;

              case 'L':
              case 'l':
                   s = stpcpy(s, inet_ntoa(_dip.loc_ip));
                   break;

              case 'R':
              case 'r':
                   s = stpcpy(s, inet_ntoa(_dip.rmt_ip));
                   break;

              default:
                   *s++ = *p;
            } /* end "switch" */
          } /* end "if" */
          else
            *s++ = *p;
          ++p;
        } /* end "while" */

        *s = '\0';

        if(opt_v) {
          if((when == IFC_PRE) || (when == IFC_UP))
            syslog(LOG_ERR, ">>> ATTACH \"%s\"\n", buff);
          else
            syslog(LOG_ERR, ">>> DETACH \"%s\"\n", buff);
        }
        if(system(buff) != 0)
          return(-1);
      }

      if(direction == 1) c = c->next;

    } while(c != cfg);
  }
  return(0);
}

static void
cfg_clean(void)
{
  struct ifcfg *c, *c1;

  if((c = cfg) != (struct ifcfg *)NULL) {
    do {
      c1 = c->next;
      free(c);
    } while((c = c1) != cfg);

    cfg = (struct ifcfg *)NULL;
  }
}
#endif /* PAUL_SPECIAL */


#include <sys/socket.h>
#include <net/if.h>
#include <linux/route.h>
#include <linux/if_ether.h>
#include <netinet/in.h>
#include <time.h>

#define MAX_IFS  512

#define SET_SA_FAMILY(addr, family)			\
    memset ((char *) &(addr), '\0', sizeof(addr));	\
    addr.sa_family = (family);

int s;
time_t online;

static u_long proxy_arp = 0;

/*
 * Make a string representation of a network IP address.
 */
inline static char *
ip_ntoa(ipaddr)
u_long ipaddr;
{
    static char b[64];

    ipaddr = ntohl(ipaddr);

    sprintf(b, "%d.%d.%d.%d",
            (u_char)(ipaddr >> 24),
            (u_char)(ipaddr >> 16),
            (u_char)(ipaddr >> 8),
            (u_char)(ipaddr));
    return b;
}

static 
int get_ether_addr (u_long ipaddr, struct sockaddr *hwaddr)
{
    u_long ina, mask;
    struct ifreq *ifr, *ifend;
#if 0
    struct sockaddr_dl *dla;
    struct ifreq *ifp;
    int i;
#endif
    struct ifreq ifreq;
    struct ifconf ifc;
    struct ifreq ifs[MAX_IFS];
    
    s = socket(AF_INET, SOCK_DGRAM, 0);

    ifc.ifc_len = sizeof(ifs);
    ifc.ifc_req = ifs;
    if (ioctl(s, SIOCGIFCONF, &ifc) < 0) {
	syslog(LOG_ERR, "ioctl(SIOCGIFCONF)");
	return 0;
    }
    if (opt_v == 1)
	syslog(LOG_DEBUG, "proxy arp: scanning %d interfaces for IP %s",
		ifc.ifc_len / sizeof(struct ifreq), ip_ntoa(ipaddr));

    /*
     * Scan through looking for an interface with an Internet
     * address on the same subnet as `ipaddr'.
     */
    ifend = ifs + (ifc.ifc_len / sizeof(struct ifreq));
    for (ifr = ifc.ifc_req; ifr < ifend; ifr++) {
	if (ifr->ifr_addr.sa_family == AF_INET) {
	    ina = ((struct sockaddr_in *) &ifr->ifr_addr)->sin_addr.s_addr;
	    strncpy(ifreq.ifr_name, ifr->ifr_name, sizeof(ifreq.ifr_name));
	    if (opt_v == 1)
               syslog(LOG_DEBUG, "proxy arp: examining interface %s",
		      ifreq.ifr_name);
            /*
             * Check that the interface is up, and not point-to-point
             * or loopback.
             */
	    if (ioctl(s, SIOCGIFFLAGS, &ifreq) < 0)
		continue;
	    if ((ifreq.ifr_flags &
		 (IFF_UP|IFF_BROADCAST|IFF_POINTOPOINT|IFF_LOOPBACK|IFF_NOARP))
		!= (IFF_UP|IFF_BROADCAST))
		continue;
            /*
             * Get its netmask and check that it's on the right subnet.
             */
	    if (ioctl(s, SIOCGIFNETMASK, &ifreq) < 0)
	        continue;
	    mask = ((struct sockaddr_in *) &ifreq.ifr_addr)->sin_addr.s_addr;
	    if(opt_v == 1)
		syslog(LOG_DEBUG, "proxy arp: interface addr %s mask %lx",
		       ip_ntoa(ina), ntohl(mask));
	    if (((ipaddr ^ ina) & mask) != 0)
	        continue;
	    break;
	}
    }
    
    if (ifr >= ifend)
        return 0;

    syslog(LOG_INFO, "found interface %s for proxy arp", ifreq.ifr_name);
    /*
     * Now get the hardware address.
     */
    if (ioctl (s, SIOCGIFHWADDR, &ifreq) < 0) {
        syslog(LOG_ERR, "SIOCGIFHWADDR(%s): %m", ifreq.ifr_name);
        return 0;
    }

    hwaddr->sa_family = ARPHRD_ETHER;
#ifndef old_ifr_hwaddr
    memcpy (&hwaddr->sa_data, &ifreq.ifr_hwaddr, ETH_ALEN);
#else
    memcpy (&hwaddr->sa_data, &ifreq.ifr_hwaddr.sa_data, ETH_ALEN);
#endif

    if (opt_v == 1)
    	syslog(LOG_DEBUG,
		"proxy arp: found hwaddr %02x:%02x:%02x:%02x:%02x:%02x",
		(int) ((unsigned char *) &hwaddr->sa_data)[0],
		(int) ((unsigned char *) &hwaddr->sa_data)[1],
		(int) ((unsigned char *) &hwaddr->sa_data)[2],
		(int) ((unsigned char *) &hwaddr->sa_data)[3],
		(int) ((unsigned char *) &hwaddr->sa_data)[4],
		(int) ((unsigned char *) &hwaddr->sa_data)[5]);
    return 1;
}


/*
 * sifproxyarp - Make a proxy ARP entry for the peer.
 */

static int sifproxyarp (struct dip *dip)
{
    u_long his_adr;
    u_long my_adr;
    struct arpreq arpreq;

    memset (&arpreq, '\0', sizeof(arpreq));
    his_adr = dip->rmt_ip.s_addr;
    my_adr  = dip->loc_ip.s_addr;

   /*
    * Get the hardware address of an interface on the same subnet
    * as our local address.
    */
    if (!get_ether_addr(dip->hom_ip.s_addr, &arpreq.arp_ha)) {
	syslog(LOG_ERR, "Cannot determine ethernet address for proxy ARP");
	return 0;
    }
    
    SET_SA_FAMILY(arpreq.arp_pa, AF_INET);
    ((struct sockaddr_in *) &arpreq.arp_pa)->sin_addr.s_addr = his_adr;
    arpreq.arp_flags = ATF_PERM | ATF_PUBL;
    
    if (ioctl(s, SIOCSARP, (caddr_t)&arpreq) < 0) {
	syslog(LOG_ERR, "ioctl(SIOCSARP): %m");
	return 0;
    }
    proxy_arp = his_adr;
    return 1;
}

/*
 * cifproxyarp - Delete the proxy ARP entry for the peer.
 */

static int cifproxyarp (void)
{
    struct arpreq arpreq;
  
    memset (&arpreq, '\0', sizeof(arpreq));
    SET_SA_FAMILY(arpreq.arp_pa, AF_INET);
    
    if (proxy_arp == 0)
      return 0;
    
    ((struct sockaddr_in *) &arpreq.arp_pa)->sin_addr.s_addr = proxy_arp;
    if (ioctl(s, SIOCDARP, (caddr_t)&arpreq) < 0) {
      syslog(LOG_WARNING, "ioctl(SIOCDARP): %m");
      return 0;
    }
    if (opt_v == 1)
      syslog(LOG_DEBUG, "Deleted proxy arp for %s", ip_ntoa(proxy_arp));
    return 1;
  }


int
attach(struct dip *dip)
{
  char buff[1024];
  
  memset(buff, '\0', sizeof(buff));
  
  memcpy((char *)&_dip, (char *)dip, sizeof(struct dip));
  
  /* Some more updates will be here - made by Paul Cadach */
  
  /* Set up for "upping" the desired interface. */
  sprintf(buff, "%s %s %s pointopoint ", _PATH_BIN_IFCONFIG,
	  dip->ifname, inet_ntoa(dip->loc_ip));
  sprintf(&buff[strlen(buff)], "%s mtu %d ",
	  inet_ntoa(dip->rmt_ip), dip->mtu);
  if (dip->netmask[0] != '\0') 
    sprintf(&buff[strlen(buff)], " netmask %s",
	    dip->netmask);
  if (opt_v)
    syslog(LOG_INFO, ">>> ATTACH \"%s\"\n", buff);
  if (system(buff) != 0) return(-1);
  
  /* Add route to the remote IP addr */
  sprintf(buff, "%s add %s %s", _PATH_BIN_ROUTE, 
	  inet_ntoa(dip->rmt_ip), dip->ifname);
  if (opt_v)
    syslog(LOG_INFO, ">>> ATTACH \"%s\"\n", buff);
  if (system(buff) != 0) return(-1);
  
  /* the default route goes over the LOCAL adaptor... */
  if (dip->rtdefault == 1) {
    sprintf(buff, "%s add -net default gw %s metric %1d %s", 
	    _PATH_BIN_ROUTE, inet_ntoa(dip->rmt_ip), 1, 
	    dip->ifname);
    if (opt_v)
      syslog(LOG_INFO, ">>> ATTACH \"%s\"\n", buff);
    if (system(buff) != 0) return(-1);
  } 
#if 0
  else {
    /* hosts get added by their local address too.. */
    sprintf(buff, "%s add -host %s %s", _PATH_BIN_ROUTE, 
	    inet_ntoa(dip->loc_ip), dip->ifname);
    if (opt_v)
      syslog(LOG_INFO, ">>> ATTACH \"%s\"\n", buff);
    if (system(buff) != 0) return(-1);    
  }
#endif /* to prevent some screw-ups :-) */
 
  /* And a little trick - try to set proxy arp to remote IP...     */
  if (dip->proxyarp)
     (void) sifproxyarp(dip);
  /* Don't care about proxy arp return code */

  syslog(LOG_INFO, "%s dial-up %s/%s to remote %s/%s  with %s/%d",
        dip->name, 
	dip->local,    ip_ntoa(dip->loc_ip.s_addr),
	dip->remote,   inet_ntoa(dip->rmt_ip),
        dip->protocol, dip->mtu);

  online = time(NULL);

#ifdef CONFIG_IP_ACCT
  sprintf(buff, "%s a a all iface %s from %s to 0.0.0.0/0 ",
	  _PATH_BIN_IPFW, 
	  ip_ntoa(dip->loc_ip.s_addr),
	  ip_ntoa(dip->loc_ip.s_addr));
  (void) system(buff);
  sprintf(buff, "%s a a all iface %s from 0.0.0.0/0 to %s ",
	  _PATH_BIN_IPFW, 
	  ip_ntoa(dip->loc_ip.s_addr),
	  ip_ntoa(dip->loc_ip.s_addr));
  (void) system(buff);
#endif /* CONFIG_IP_ACCT */

  return(0);
}

#ifdef CONFIG_IP_ACCT
/* 
 * The following piece of code is taken from "ipfw.c" 
 * which is  part of Net-Tools package. All the relevant
 * Copyrights apply
 *
 *
 * Copyright (c) 1993 Daniel Boulet
 *  Linux port (c) 1994 Bob Beck
 *      Drastically cleaned up: Alan Cox <Alan.Cox@linux.org>
 *      More (major) cleanups and bug fixes by Salvador Abreu <spa@fct.unl.pt>
 *      Additional options Lutz Pre"sler <Lutz.Pressler@med-stat.gwdg.de>
 *      Masquerade client support added <Alan.Cox@linux.org>
 */

typedef struct
{
	int acct;
	unsigned long sa, da, sm, dm, iface;
	unsigned int nsp, ndp;
	unsigned long npkt, nbyt;
	unsigned int fw_pts[10];
	int fw_flg;
} fw_rec;

#define MIN(a,b) ((a)<(b)? (a): (b))
#define SRC(x)   ((x)->sa & (x)->sm)
#define DST(x)   ((x)->da & (x)->dm)

static
void list_stat(struct dip *dip, 
	u_long *byte_to,   u_long *pack_to,
	u_long *byte_from, u_long *pack_from)
{
	char buf[256];
	FILE *f = fopen("/proc/net/ip_acct", "r");
	fw_rec *rec = (fw_rec *) malloc(sizeof(fw_rec));

	if (f == (FILE *)NULL)
	{
		syslog(LOG_ERR, "ip_acct failed!!");
		return;
	}
	fgets(buf, 255, f);	/* skip title */

	/* Scan the accp rec's to get related to SL interface */
	while (fgets(buf, 255, f))
	{  /* read in the data */
	   sscanf(buf,
		    "%lX/%lX->%lX/%lX %lX %X %u %u %lu %lu %u %u %u %u %u %u %u %u %u %u",
		    &rec->sa, &rec->sm, &rec->da, &rec->dm, &rec->iface,
		    &rec->fw_flg, &rec->nsp, &rec->ndp, &rec->npkt, &rec->nbyt,
		    &rec->fw_pts[0], &rec->fw_pts[1], &rec->fw_pts[2], &rec->fw_pts[3],
		    &rec->fw_pts[4], &rec->fw_pts[5], &rec->fw_pts[6], &rec->fw_pts[7],
		    &rec->fw_pts[8], &rec->fw_pts[9]);

	   /* Is it our record? */
	   if (rec->sa == (ntohl(dip->loc_ip.s_addr))) {
		/* Yes, "from" type */
		*byte_from += rec->nbyt;
		*pack_from += rec->npkt;
	   } else if (rec->da == (ntohl(dip->loc_ip.s_addr))) { 
		/* Yes, "to" type */
		*byte_to += rec->nbyt;
		*pack_to += rec->npkt;
	   }
	}
	fclose(f);

	return;
}
#endif

void
detach(struct dip *dip)
{
  char buff[1024];
  static int flag = 0;
  extern int opt_i;
  u_long b_in=0, b_out=0, p_in=0, p_out=0;
  char  *p;
  char in_b[10], out_b[10];

  memset (buff,  '\0', sizeof(buff));
  memset (in_b,  '\0', sizeof in_b);
  memset (out_b, '\0', sizeof out_b);

  /* To make sure we're not called more than once! */
  if (flag != 0)
    return;
  flag = 1;

  /* Set up for "downing" the desired interface. */
  sprintf(buff, "%s %s down", _PATH_BIN_IFCONFIG, dip->ifname);
  if (opt_v)
    syslog(LOG_INFO, ">>> DETACH \"%s\"\n", buff);
  (void) system(buff);

  if (opt_i == 0) p = "link";
  else            p = "server link";

  online = time(NULL) - online;

#ifdef CONFIG_IP_ACCT
  list_stat(dip, &b_in, &p_in, &b_out, &p_out);

  if (b_in > 100 * 1024) {
    b_in = (b_in + 1023) / 1024;
    if (b_in > 100 * 1024) {
		b_in = (b_in + 1023) / 1024;
                strcpy (in_b, "Mb");
    } else {
                strcpy (in_b, "Kb");
    }
  } else {
    strcpy (in_b, " bytes");
  }

  if (b_out > 100 * 1024) {
    b_out = (b_out + 1023) / 1024;
    if (b_out > 100 * 1024) {
	b_out = (b_out + 1023) / 1024;
        strcpy (out_b, "Mb");
    } else {
        strcpy (out_b, "Kb");
    }
  } else {
    strcpy (out_b, " bytes");
  }

  syslog(LOG_INFO, "%s down %s %s to remote %s/%s",
	dip->name, dip->protocol, p,
	dip->remote, inet_ntoa(dip->rmt_ip));
  sleep(1);
  syslog(LOG_INFO, 
        "Total online:%lds  in:%lu%s %lu pkts; out:%lu%s %lu pkts", 
  	online, b_in, in_b, p_in, b_out, out_b, p_out);
#else
  syslog(LOG_INFO, "%s down %s %s to remote %s/%s [online %lds]", 
	dip->name, dip->protocol, p,
	dip->remote, inet_ntoa(dip->rmt_ip), online);
#endif /* CONFIG_IP_ACCT */

  /* Delete proxy arp, just in case it's set */
  if (dip->proxyarp)
    (void) cifproxyarp();

#ifdef CONFIG_IP_ACCT
  sprintf(buff, "%s d a all iface %s from %s to 0.0.0.0/0 ",
	  _PATH_BIN_IPFW, 
	  ip_ntoa(dip->loc_ip.s_addr),
	  ip_ntoa(dip->loc_ip.s_addr));
  (void) system(buff);
  sprintf(buff, "%s d a all iface %s from 0.0.0.0/0 to %s ",
	  _PATH_BIN_IPFW, 
	  ip_ntoa(dip->loc_ip.s_addr),
	  ip_ntoa(dip->loc_ip.s_addr));
  (void) system(buff);
#endif /* CONFIG_IP_ACCT */
}

