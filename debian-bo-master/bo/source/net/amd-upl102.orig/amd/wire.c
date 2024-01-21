/*
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	%W% (Berkeley) %G%
 *
 * $Id: wire.c,v 5.2.2.2 1992/06/07 18:06:46 jsp Exp jsp $
 *
 */

/*
 * This function returns the subnet (address&netmask) for the primary network
 * interface.  If the resulting address has an entry in the hosts file, the
 * corresponding name is retuned, otherwise the address is returned in
 * standard internet format.
 * As a side-effect, a list of local IP/net address is recorded for use
 * by the islocalnet() function.
 *
 * Derived from original by Paul Anderson (23/4/90)
 * Updates from Dirk Grunwald (11/11/91)
 */

#include "am.h"

#include <sys/ioctl.h>

#ifdef __svr4__
#include <sys/sockio.h>
#endif /* __svr4__ */


#define C(x)	((x) & 0xff)

/*
 * List of locally connected networks
 */
typedef struct addrlist addrlist;
struct addrlist {
	addrlist *ip_next;
	unsigned long ip_addr;
	unsigned long ip_mask;
};
static addrlist *localnets = 0;

#ifdef SIOCGIFFLAGS
#ifdef STELLIX
#include <sys/sema.h>
#endif /* STELLIX */
#include <net/if.h>
#include <netdb.h>

#if defined(IFF_LOCAL_LOOPBACK) && !defined(IFF_LOOPBACK)
#define IFF_LOOPBACK IFF_LOCAL_LOOPBACK
#endif

#define GFBUFLEN 1024
#define clist (ifc.ifc_ifcu.ifcu_req)
#define count (ifc.ifc_len/sizeof(struct ifreq))

#ifdef __svr4__
#include <nsswitch.h>
#ifdef HAS_NIS_MAPS
#include <rpcsvc/ypclnt.h>
#endif /* HAS_NIS_MAPS */

#define NSS_NAME_FILES		"files"
#define NSS_NAME_NIS		"nis"
#define NSS_NAME_NISPLUS	"nisplus"
#define WHITESPACE "\n\t "

typedef struct __nsw_lookup nsw_lookup;
typedef struct __nsw_switchconfig nsw_switchconfig;

/*
 * Perform similar actions as inet_ntoa(), but without trailing .0 octets.
 * Useful to generate net name strings such as 128.59.16 and not 128.59.16.0.
 */
static char *inet_ntoa_netnum P(( struct in_addr *inp));
static char *inet_ntoa_netnum(inp)
     struct in_addr *inp;
{
  static char name[16];
  char tmpstr[5], *p;
  int i;

  if (!inp)
    return (char *) NULL;

  memset(&name[0], 0, 16);	/* reset out buf */
  memset(&tmpstr[0], 0, 5);	/* reset tmp buf */
  for (p=(char *) &inp->s_addr,i=0; p && i++<sizeof(u_long); ++p) {
    if (*p == 0)		/* skip zeros */
      continue;
    sprintf(&tmpstr[0], "%d.", ((int) *p & 0xff));
    strcat(name, tmpstr);
  }
  if (name)
    name[strlen(name) - 1] = '\0';

  return &name[0];
}


/*
 * A replacement for getnetbyaddr() which does not need linking with 
 * -lsocket and uses TLI only.
 */
struct netent *getnetbyaddr P(( long net, int type));
struct netent *getnetbyaddr(net, type)
     long net;			/* network number to find name for */
     int type;
{
  int i, key_len, ws_len;
  static char buf[2048];
  char *netname;
  static nsw_lookup default_lookup[3];
  nsw_lookup *tmp_lookup = (nsw_lookup *) NULL;
  static nsw_switchconfig default_switchconfig;
  static nsw_switchconfig *current_switchconfig = (nsw_switchconfig *) NULL;
  static enum __nsw_parse_err current_npe = __NSW_SUCCESS;
  struct in_addr in;
  static struct netent ne;
#ifdef HAS_NIS_MAPS
  static char *yp_domain = (char *) NULL;
#endif /* HAS_NIS_MAPS */

  if (current_switchconfig == (nsw_switchconfig *) NULL) {
    /* Initialize default lookup */
    memset(default_lookup, 0, sizeof(nsw_lookup)*3);
    default_lookup[0].service_name = NSS_NAME_FILES;
    default_lookup[1].service_name = NSS_NAME_NIS;
    default_lookup[2].service_name = NSS_NAME_NISPLUS;
    for (i=0; i<2; ++i)
      default_lookup[i].next = &default_lookup[i+1];
    for (i=0; i<3; ++i)
      default_lookup[i].actions[1] = 1;	/* actions == {1,0,0,0} then */

    /* Initialize default switchconfig */
    default_switchconfig.vers = 0;
    default_switchconfig.dbase = __NSW_NETWORKS_DB;
    default_switchconfig.num_lookups = 3;
    default_switchconfig.lookups = &default_lookup[0];
  }

  /*
   * Get the configration if possible and needed.
   * Set to default as last resort.
   */
  if (current_switchconfig == (nsw_switchconfig *) NULL) {
    current_switchconfig = __nsw_getconfig(__NSW_NETWORKS_DB, &current_npe);
    if (current_switchconfig == (nsw_switchconfig *) NULL)
      current_switchconfig = &default_switchconfig;
  }

  tmp_lookup = current_switchconfig->lookups;

  /*
   * Convert net number to string
   */
  in.s_addr = ntohl(net);
  netname = inet_ntoa_netnum(&in);

  /*
   * reset out val
   */
  ne.n_name = netname;
  ne.n_aliases = (char **) NULL; /* XXX: put this code in */
  ne.n_addrtype = type;
  ne.n_net = net;

  /*
   * Do the lookups using the various methods.
   */
  while (tmp_lookup != (nsw_lookup *) NULL) {
    if (!strcmp(tmp_lookup->service_name, NSS_NAME_FILES)) {
      FILE *fp;
      /* open /etc/networks file */
      if ((fp = fopen(_PATH_NETWORKS, "r")) == (FILE *) NULL) {
	current_npe = __NSW_UNAVAIL;
	return (struct netent *) NULL;
      }
      while (fgets(buf, 2047, fp)) {
	if (buf[0] == '#')	/* skip comments */
	  continue;
	/* search for whitespace separator and compare */
	if ((key_len = strcspn(buf, WHITESPACE)) == 0)
	  continue;
	/* search for length of initial whitespace */
	if ((ws_len = strspn(&buf[key_len], WHITESPACE)) == 0)
	  continue;
	/* search for length of first non-whitespace field in value */
	i = strcspn(&buf[key_len + ws_len], WHITESPACE);
	buf[key_len + ws_len + i] = '\0';
	if (strcmp(&buf[key_len + ws_len], netname) != 0)
	  continue;
	buf[key_len] = '\0';
	fclose(fp);
	ne.n_name = &buf[0];
	return &ne;
      }
      fclose(fp);
    } else if (!strcmp(tmp_lookup->service_name, NSS_NAME_NIS)) {
#ifdef HAS_NIS_MAPS
      char *s = &buf[0];

      if (!yp_domain) {
	yp_domain = (char *) calloc(128, sizeof(char *));
	if (!yp_domain) {	/* no more memory! */
	  yp_domain = "no yp domain";
	} else {
	  yp_get_default_domain(&yp_domain);
	  /* XXX: check return value from this call */
	}
      }
      key_len = strlen(netname);
      current_npe = yp_match(yp_domain, "networks.byaddr", netname, key_len, &s, &i);
      if (current_npe == __NSW_SUCCESS) {
	s[i] = '\0';
	if ((key_len = strcspn(s, WHITESPACE)) > 0)
	  s[key_len] = '\0';
	ne.n_name = s;
	return &ne;
      } else {
	/* set npe error according to NIS status */
	if (current_npe == YPERR_KEY)
	  current_npe = __NSW_NOTFOUND;
	else if (current_npe != __NSW_SUCCESS)
	  current_npe = __NSW_UNAVAIL;
	return (struct netent *) NULL;
      }
#else
      current_npe = __NSW_UNAVAIL;
      return (struct netent *) NULL;
#endif /* HAS_NIS_MAPS */
    } else if (!strcmp(tmp_lookup->service_name, NSS_NAME_NISPLUS)) {
#ifdef HAS_NISPLUS
      current_npe = NOT_IMPLEMENTED_MATCH_NISPLUS(NIS_DOMAIN, "netmasks.byaddr", key, key_len, buf, &i); 
      if (current_npe == __NSW_SUCCESS) {
	buf[i] = '\0';
	ne.n_name = &buf[0];
	return &ne;
      } else {
	/* do I have to interpret nis+ results codes? */
	return (struct netent *) NULL;
      }
#else
      current_npe = __NSW_UNAVAIL;
      return (struct netent *) NULL;
#endif /* HAS_NISPLUS */
    }
    tmp_lookup = tmp_lookup->next;
  }
  /* if gets here then no match succeeded */
  if (current_npe == __NSW_SUCCESS) /* assume was err was not set */
    current_npe = __NSW_NOTFOUND;
  return (struct netent *) NULL;
}
#endif /* __svr4__ */

void getwire P((char **name1, char**number1, char**name2, char**number2));
void getwire(name1, number1, name2, number2)
char **name1;
char **number1;
char **name2;
char **number2;
{
	struct hostent *hp;
	struct netent *np;
	struct ifconf ifc;
	struct ifreq *ifr;
	caddr_t cp, cplim;
	unsigned long address, netmask, subnet;
	char buf[GFBUFLEN], *s;
	int fd = -1;
	unsigned long net;
	unsigned long mask;
	unsigned long subnetshift;
	char netNumberBuf[64];

	*name1 = 0;
	*number1 = 0;
	*name2 = 0;
	*number2 = 0;

#ifdef __svr4__
	/*
	 * Get suitable TLI handler
	 */
	if ((fd = t_open("/dev/udp", O_RDWR, 0)) < 0)
		goto out;
#else
	/*
	 * Get suitable socket
	 */
	if ((fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
		goto out;
#endif /* __svr4__ */

	/*
	 * Fill in ifconf details
	 */
	bzero(&buf[0], GFBUFLEN);
	ifc.ifc_len = sizeof(buf);
	ifc.ifc_buf = buf;

	/*
	 * Get network interface configurations
	 */
	if (ioctl(fd, SIOCGIFCONF, (caddr_t) &ifc) < 0)
		goto out;

	/*
	 * Upper bound on array
	 */
	cplim = buf + ifc.ifc_len;

	/*
	 * This is some magic to cope with both "traditional" and the
	 * new 4.4BSD-style struct sockaddrs.  The new structure has
	 * variable length and a size field to support longer addresses.
	 * AF_LINK is a new definition for 4.4BSD.
	 */
#ifdef AF_LINK
#define max(a, b) ((a) > (b) ? (a) : (b))
#define size(ifr) (max((ifr)->ifr_addr.sa_len, sizeof((ifr)->ifr_addr)) + sizeof(ifr->ifr_name))
#else
#define size(ifr) sizeof(*ifr)
#endif
	/*
	 * Scan the list looking for a suitable interface
	 */
	for (cp = buf; cp < cplim; cp += size(ifr)) {
		addrlist *al;
		ifr = (struct ifreq *) cp;

		if (ifr->ifr_addr.sa_family != AF_INET)
			continue;
		else
			address = ((struct sockaddr_in *) &ifr->ifr_addr)->sin_addr.s_addr;

		/*
		 * Get interface flags
		 */
		if (ioctl(fd, SIOCGIFFLAGS, (caddr_t) ifr) < 0)
			continue;

		/*
		 * If the interface is a loopback, or its not running
		 * then ignore it.
		 */
#if defined(IFF_LOOPBACK)
		if ((ifr->ifr_flags & IFF_LOOPBACK) != 0)
			continue;
#endif
#ifndef NeXT
		if ((ifr->ifr_flags & IFF_RUNNING) == 0)
#else
		if ((ifr->ifr_flags & IFF_UP) == 0)
#endif
			continue;

		/*
		 * Get the netmask of this interface
		 */
		if (ioctl(fd, SIOCGIFNETMASK, (caddr_t) ifr) < 0)
			continue;

		netmask = ((struct sockaddr_in *) &ifr->ifr_addr)->sin_addr.s_addr;

		/*
		 * Add interface to local network list
		 */
		al = ALLOC(addrlist);
		al->ip_addr = address;
		al->ip_mask = netmask;
		al->ip_next = localnets;
		localnets = al;

		/*
		 * Figure out the subnet's network address
		 */
		subnet = address & netmask;

#ifdef IN_CLASSA
		subnet = htonl(subnet); 

		if (IN_CLASSA(subnet)) {
			mask = IN_CLASSA_NET;
			subnetshift = 8;
		} else if (IN_CLASSB(subnet)) {
			mask = IN_CLASSB_NET;
			subnetshift = 8;
		} else {
			mask = IN_CLASSC_NET;
			subnetshift = 4;
		}

		/*
		 * If there are more bits than the standard mask
		 * would suggest, subnets must be in use.
		 * Guess at the subnet mask, assuming reasonable
		 * width subnet fields.
		 * XXX: Or-in at least 1 byte's worth of 1s to make
		 * sure the top bits remain set.
		 */
		while (subnet &~ mask)
			mask = (mask >> subnetshift) | 0xff000000;

		net = subnet & mask;
		while ((mask & 1) == 0)
			mask >>= 1, net >>= 1;

		/*
		 * Now get a usable name.
		 * First use the network database,
		 * then the host database,
		 * and finally just make a dotted quad.
		 */

		np = getnetbyaddr(net, AF_INET);

		/* the network address has been masked off */
		if ((subnet & 0xffffff) == 0) {
			sprintf(netNumberBuf, "%u", C(subnet >> 24));
		}
		else if ((subnet & 0xffff) == 0) {
			sprintf(netNumberBuf, "%u.%u",
				C(subnet >> 24) , C(subnet >> 16));
		}
		else if ((subnet & 0xff) == 0) {
			sprintf(netNumberBuf, "%u.%u.%u",
				C(subnet >> 24), C(subnet >> 16),
				C(subnet >> 8));
		}
		else {
			sprintf(netNumberBuf, "%u.%u.%u.%u",
				C(subnet >> 24), C(subnet >> 16),
				C(subnet >> 8), C(subnet));
		}
		if (!*number1) {
		  *number1 = strdup ( netNumberBuf );
		} else if (!*number2) {
		  *number2 = strdup ( netNumberBuf );
		} else {
		  plog(XLOG_INFO, "Another unused interface discovered: netnumber %s", netNumberBuf);
		}
#else
		/* This is probably very wrong. */
		np = getnetbyaddr(subnet, AF_INET);
#endif /* IN_CLASSA */
		if (np)
			s = np->n_name;
		else {
			subnet = address & netmask;
			hp = gethostbyaddr((char *) &subnet, 4, AF_INET);
			if (hp)
				s = hp->h_name;
			else
				s = inet_dquad(buf, subnet);
		}
		if (!*name1) {
		  *name1 = strdup ( s );
		} else if (!*name2) {
		  *name2 = strdup ( s );
		} else {
		  plog(XLOG_INFO, "Another unused interface discovered: netname %s", s);
		}
	      }

out:
	if (fd >= 0)
#ifdef __svr4__
		(void) t_close(fd);
#else
		(void) close(fd);
#endif /* __svr4__ */
	if (! *name1)
		*name1 = strdup(NO_SUBNET);
	if (! *number1)
		*number1 = "0.0.0.0";
	if (! *name2)
		*name2 = strdup(NO_SUBNET);
	if (! *number2)
		*number2 = "0.0.0.0";
}

#else

void getwire P((char **name1, char**number1, char**name2, char**number2));
void getwire(name1, number1, name2, number2)
char **name1;
char **number1;
char **name2;
char **number2;
{
	*name1 = strdup(NO_SUBNET);
	*number1 = "0.0.0.0";
	*name2 = 0;
	*number2 = 0;
}
#endif /* SIOCGIFFLAGS */

/*
 * Determine whether a network is on a local network
 * (addr) is in network byte order.
 */
int islocalnet P((unsigned long addr));
int islocalnet(addr)
unsigned long addr;
{
	addrlist *al;

	for (al = localnets; al; al = al->ip_next)
		if (((addr ^ al->ip_addr) & al->ip_mask) == 0)
			return TRUE;

#ifdef DEBUG
	{ char buf[16];
	plog(XLOG_INFO, "%s is on a remote network", inet_dquad(buf, addr));
	}
#endif
	return FALSE;
}
