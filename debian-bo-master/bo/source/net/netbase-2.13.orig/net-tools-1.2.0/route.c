/*
 * route        This file contains an implementation of the command
 *              that manages the IP routing table in the kernel.
 *
 * Usage:       route [-nv] [ {add|del} [{-net|-host}] target
 *                    [ gw ] [ netmask ] [ metric ] [ device ]]
 *
 * Version:     NetTools-1.1.51	28/09/94
 *  	derived from '@(#)route.c     1.70    01/04/94' by Fred N. van Kempen.
 *
 * Author:      Fred N. van Kempen, <waltje@uwalt.nl.mugnet.org>
 *              Modified for Net-2Debugged by Johannes Stille,
 *                      <johannes@titan.os.open.de>
 *		Changes by Linus Torvalds
 *		Further changes by Alan Cox to add the new mtu/window stuff
 */
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <net/if.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include "config.h"
#include "net-locale.h"

char           *Version = "@(#) route 1.1.51 (28/09/94)";

/* Pathnames of the PROCfs files used by NET. */
#define _PATH_PROCNET_ROUTE	"/proc/net/route"

#ifdef SIOCADDRTOLD
#define mask_in_addr(x) (((struct sockaddr_in *)&((x).rt_genmask))->sin_addr.s_addr)
#define full_mask(x) (x)
#else
#define mask_in_addr(x) ((x).rt_genmask)
#define full_mask(x) (((struct sockaddr_in *)&(x))->sin_addr.s_addr)
#endif

char * getsock(char *bufp, struct sockaddr * sap)
{
	unsigned char *ptr;
	char *sp = bufp;
	int i, val;
	struct sockaddr_in *sin;

	/* Grmpf. -FvK */
	sin = (struct sockaddr_in *) sap;
	sin->sin_family = AF_INET;
	sin->sin_port = 0;

	ptr = (unsigned char *) (&(sin->sin_addr.s_addr) + 1);
	for (i = 0; i < sizeof(sin->sin_addr.s_addr); i++) {
		val = 0;
		if (*sp == '\t')
			break;
		if (*sp >= 'A')
			val = (int) (*sp - 'A') + 10;
		else
			val = (int) (*sp - '0');
		val <<= 4;
		sp++;
		if (*sp >= 'A')
			val |= (int) (*sp - 'A') + 10;
		else
			val |= (int) (*sp - '0');
		*--ptr = (unsigned char) (val & 0377);
		sp++;
	}

	return (sp);
}

int resolve(char *name, struct sockaddr * sap)
{
	struct hostent *hp;
	struct netent  *np;
	struct sockaddr_in *sin;

	/* Grmpf. -FvK */
	sin = (struct sockaddr_in *) sap;
	sin->sin_family = AF_INET;
	sin->sin_port = 0;

	/* Default is special, meaning 0.0.0.0. */
	if (!strcmp(name, "default")) {
		sin->sin_addr.s_addr = INADDR_ANY;
		return (1);
	}
	/* Try the NETWORKS database to see if this is a known network. */
	if ((np = getnetbyname(name)) != (struct netent *) NULL) {
		sin->sin_addr.s_addr = htonl(np->n_net);
		strcpy(name, np->n_name);
		return (1);
	}
#ifdef DEBUG
	_res.options |= RES_DEBUG;
	res_init();
#endif

	if ((hp = gethostbyname(name)) == (struct hostent *) NULL) {
		errno = h_errno;
		return (-1);
	}
	memcpy((char *) &sin->sin_addr, (char *) hp->h_addr_list[0], hp->h_length);
	strcpy(name, hp->h_name);
	return 0;
#ifdef TOTALLY_BROKEN	
/*
 * assume it's a network if the least significate byte is 0,
 * as that's not a valid host address.
 */
	return ((ntohl(sin->sin_addr.s_addr) & 0xff) == 0);
#endif	
}


int rresolve(char *name, struct sockaddr * sap, int numeric)
{
	struct sockaddr_in *sin;
	struct hostent *ent;
	struct netent  *np;
	unsigned long   ad, host_ad;

	/* Grmpf. -FvK */
	sin = (struct sockaddr_in *) sap;
	if (sin->sin_family != AF_INET) {
#ifdef DEBUG
		fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_rresolve,
					    "rresolve: unsupport address family %d !\n"),
			sin->sin_family);
#endif
		errno = EAFNOSUPPORT;
		return (-1);
	}
	if (sin->sin_addr.s_addr == INADDR_ANY) {
		if (numeric & 0x8000)
			strcpy(name, "default");
		else
			strcpy(name, "*");
		return (0);
	}
	ad = (unsigned long) sin->sin_addr.s_addr;

	host_ad = ntohl(ad);
	np = NULL;
	ent = NULL;
	if ((numeric & 0x7FFF) == 0) {
		if ((host_ad & 0xFF) != 0)  {
			ent = gethostbyaddr((char *) &ad, 4, AF_INET);
			if (ent != NULL)
				strcpy(name, ent->h_name);
		} else {
			np = getnetbyaddr(host_ad, AF_INET);
			if (np != NULL) {
				strcpy(name, np->n_name);
			}
		}
	}
	if ((ent == NULL) && (np == NULL)) {
		sprintf(name, "%d.%d.%d.%d",
			(int) (ad & 0xFF), (int) ((ad >> 8) & 0xFF),
			(int) ((ad >> 16) & 0xFF),
			(int) ((ad >> 24) & 0xFF));
	}
	return (0);
}


void reserror(char *text)
{
	herror(text);
}


int             opt_n = 0;	/* numerical output flag	 */
int             opt_v = 0;	/* debugging output flag	 */
int             skfd = -1;


static void usage(void)
{
	fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_usage1,
				    "Usage: route [-nv]\n"));
	fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_usage2,
				    "       route [-v] del target\n"));
	fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_usage3,
				    "       route [-v] add {-net|-host} target [gw gateway]\n"));
	fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_usage4,
				    "                  [metric NN] [netmask mask] [mss maxsegment] [window maxwindow]\n"));
	fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_usage5,
				    "                  [[dev] device]\n"));
	NLS_CATCLOSE(catfd)
	exit(-1);
}


static void rt_print(void)
{
	char            buff[1024], iface[16], net_addr[64];
	char            gate_addr[64], mask_addr[64], flags[16];
	struct sockaddr snet, sgate, smask;
	FILE           *fp;
	int             num, iflags, refcnt, use, metric;
	int		mss, window;

	printf(NLS_CATGETS(catfd, routeSet, route_table, "Kernel routing table\n"));
	printf(NLS_CATGETS(catfd, routeSet, route_header,
	       "Destination     Gateway         Genmask         "
	       "Flags MSS    Window Use Iface\n"));
	if ((fp = fopen(_PATH_PROCNET_ROUTE, "r")) == NULL) {
		perror(_PATH_PROCNET_ROUTE);
		return;
	}
	while (fgets(buff, 1023, fp)) {
		num = sscanf(buff, "%s %s %s %X %d %d %d %s %d %d\n",
			     iface, net_addr, gate_addr,
			     &iflags, &refcnt, &use, &metric, mask_addr,
			     &mss,&window);
		if (num != 10)
			continue;

		/* Fetch and resolve the target address. */
		(void) getsock(net_addr, &snet);
		(void) rresolve(net_addr, &snet, (opt_n | 0x8000));
		net_addr[15] = '\0';

		/* Fetch and resolve the gateway address. */
		(void) getsock(gate_addr, &sgate);
		rresolve(gate_addr, &sgate, opt_n);
		gate_addr[15] = '\0';

		/* Fetch and resolve the mask. */
		(void) getsock(mask_addr, &smask);
		rresolve(mask_addr, &smask, 1);
		gate_addr[15] = '\0';

		/* Decode the flags. */
		flags[0] = '\0';
		if (iflags & RTF_UP)
			strcat(flags, "U");
		if (iflags & RTF_GATEWAY)
			strcat(flags, "G");
		if (iflags & RTF_HOST)
			strcat(flags, "H");
		if (iflags & RTF_REINSTATE)
			strcat(flags, "R");
		if (iflags & RTF_DYNAMIC)
			strcat(flags, "D");
		if (iflags & RTF_MODIFIED)
			strcat(flags, "M");

		/* Print the info. */
		printf("%-15s %-15s %-15s %-5s %-6d %-3d %6d %s\n",
		       net_addr, gate_addr, mask_addr, flags,
		       mss, window, use, iface);
	}

	(void) fclose(fp);
}

/* Add a routing table entry. */
int rt_add(char **args)
{
	struct rtentry rt;
	char target[128], gateway[128] = "NONE", netmask[128] = "default";
	int xflag, isnet;

	xflag = 0;
	if (*args == NULL)
		usage();
	if (!strcmp(*args, "-net")) {
		xflag = 1;
		args++;
	} else if (!strcmp(*args, "-host")) {
		xflag = 2;
		args++;
	}
	if (*args == NULL)
		usage();
	strcpy(target, *args++);
	/* Clean out the RTREQ structure. */
	memset((char *) &rt, 0, sizeof(struct rtentry));

	if ((isnet = resolve(target, &rt.rt_dst)) < 0) {
		reserror(target);
		return (-1);
	}
	switch (xflag) {
	case 1:
		isnet = 1;
		break;

	case 2:
		isnet = 0;
		break;

	default:
		break;
	}

	/* Fill in the other fields. */
	rt.rt_flags = (RTF_UP | RTF_HOST);
	if (isnet)
		rt.rt_flags &= ~RTF_HOST;

	/* Did we specify a GATEWAY entry? */
	while (*args) {
		if (!strcmp(*args, "metric")) {
			int metric;

			args++;
			if (!*args || !isdigit(**args))
				usage();
			metric = atoi(*args);
#ifdef SIOCADDRTOLD
			rt.rt_metric = metric + 1;
#else
			if (opt_v)
				fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_ignore, "metric %d ignored\n"),metric);
#endif
			args++;
			continue;
		}
		if (!strcmp(*args, "netmask")) {
			struct sockaddr mask;

			args++;
			if (!*args || mask_in_addr(rt))
				usage();
			strcpy(netmask, *args);
			if ((isnet = resolve(netmask, &mask)) < 0) {
				reserror(netmask);
				return (-1);
			}
			rt.rt_genmask = full_mask(mask);
			args++;
			continue;
		}
		if (!strcmp(*args,"gw") || !strcmp(*args,"gateway")) {
			args++;
			if (!*args)
				usage();
			if (rt.rt_flags & RTF_GATEWAY)
				usage();
			strcpy(gateway, *args);
			if ((isnet = resolve(gateway, &rt.rt_gateway)) < 0) {
				reserror(gateway);
				return (-1);
			}
			if (isnet) {
				fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_cant_use,
							    "%s: cannot use a NETWORK as gateway!\n"),
					gateway);
				return (-1);
			}
			rt.rt_flags |= RTF_GATEWAY;
			args++;
			continue;
		}
		if (!strcmp(*args,"mss")) {
			args++;
			rt.rt_flags |= RTF_MSS;
			rt.rt_mss = atoi(*args);
			args++;
			if(rt.rt_mss<64||rt.rt_mss>32768)
			{
				fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_MSS, "Invalid MSS.\n"));
				return -1;
			}
			continue;
		}
		if (!strcmp(*args,"window")) {
			args++;
			rt.rt_flags |= RTF_WINDOW;
			rt.rt_window = atoi(*args);
			args++;
			if(rt.rt_window<128||rt.rt_window>32768)
			{
				fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_window, "Invalid window.\n"));
				return -1;
			}
			continue;
		}
		if (!strcmp(*args,"device") || !strcmp(*args,"dev")) {
			args++;
			if (!*args)
				usage();
		} else
			if (args[1])
				usage();
		if (rt.rt_dev)
			usage();
		rt.rt_dev = *args;
		args++;
	}
	/* sanity checks.. */
	if (mask_in_addr(rt)) {
		unsigned long mask = ~ntohl(mask_in_addr(rt));
		if (rt.rt_flags & RTF_HOST) {
			fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_netmask1,
						    "route: netmask doesn't make sense with host route\n"));
			return -1;
		}
		if (mask & (mask+1)) {
			fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_netmask2,
						    "route: bogus netmask %s\n"), netmask);
			return -1;
		}
		mask = ((struct sockaddr_in *) &rt.rt_dst)->sin_addr.s_addr;
		if (mask & ~mask_in_addr(rt)) {
			fprintf(stderr, NLS_CATGETS(catfd, routeSet, route_netmask3,
						    "route: netmask doesn't match route address\n"));
			return -1;
		}
	}
	/* Tell the kernel to accept this route. */
	if (ioctl(skfd, SIOCADDRT, &rt) < 0) {
		fprintf(stderr, "SIOCADDRT: %s\n", strerror(errno));
		return (-1);
	}
	return (0);
}


/* Delete a routing table entry. */
int rt_del(char **args)
{
	char target[128];
	struct sockaddr trg;
	struct rtentry rt;

	if (!args[0] || args[1])
		usage();

	strcpy(target, *args);

	if (resolve(target, &trg) < 0) {
		reserror(target);
		return (-1);
	}
	/* Clean out the RTREQ structure. */
	memset((char *) &rt, 0, sizeof(struct rtentry));
	memcpy((char *) &rt.rt_dst, (char *) &trg, sizeof(struct sockaddr));

	/* Tell the kernel to delete this route. */
	if (ioctl(skfd, SIOCDELRT, &rt) < 0) {
		fprintf(stderr, "SIOCDELRT: %s\n", strerror(errno));
		return (-1);
	}
	return (0);
}


int main(int argc, char **argv)
{
	int i;
	char *s;

#if NLS
	setlocale (LC_MESSAGES, "");
	catfd = catopen ("nettools", MCLoadBySet);
#endif

	/* Fetch the command-line arguments. */
	argv++;
	while ((s = *argv) != NULL) {
		if (*s != '-')
			break;
		while (*++s != '\0')
			switch (*s) {
			case 'n':
				opt_n = 1;
				break;

			case 'v':
				opt_v = 1;
				break;

			default:
				usage();
			}
		argv++;
	}

	/* Do we have to show the contents of the routing table? */
	if (*argv == NULL) {
		rt_print();
		NLS_CATCLOSE(catfd)
		exit(0);
	}
	/* Fetch the command. */
	if (strcmp(*argv, "add") && strcmp(*argv, "del"))
		usage();

	/* Create a socket to the INET kernel. */
	if ((skfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		perror("socket");
		NLS_CATCLOSE(catfd)
		exit(-1);
	}
	/* See what we have to do here. */
	if (!strcmp(*argv, "add"))
		i = rt_add(++argv);
	else
		i = rt_del(++argv);

	/* Close the socket. */
	(void) close(skfd);

	NLS_CATCLOSE(catfd)
	return (i);
}
