#ifndef lint
static	char	sccsid[] = "@(#)$Id: subnet.c,v 1.16 1994/12/04 00:18:40 sob Exp sob $";
#endif

#include "../conf.h"
#ifdef TESTSUBNET
#ifndef DEBUG
#define DEBUG
#endif
#include <stdio.h>
#endif

#ifdef SUBNET

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifndef NETMASK
#include <net/if.h>
#endif
#ifdef STREAMS_TLI
#include <stropts.h>
#endif
#ifdef SVR4
#include <sys/sockio.h>
#endif
#include <sys/ioctl.h>
#ifdef USE_STREAMS_DEVICE_FOR_IF_CONFIG
#include <fcntl.h>
#endif
 
#if defined(DEBUG) && defined(SYSLOG)
# ifdef FAKESYSLOG
#  include "fakesyslog.h"
# else
#  include <syslog.h>
# endif
#endif

/*
 * The following routines provide a general interface for
 * subnet support.  Like the library function "inet_netof",
 * which returns the standard (i.e., non-subnet) network
 * portion of an internet address, "inet_snetof" returns
 * the subnetwork portion -- if there is one.  If there
 * isn't, it returns 0.
 *
 * Subnets, under 4.3, are specific to a given set of
 * machines -- right down to the network interfaces.
 * Because of this, the function "getifconf" must be
 * called first.  This routine builds a table listing
 * all the (internet) interfaces present on a machine,
 * along with their subnet masks.  Then when inet_snetof
 * is called, it can quickly scan this table.
 *
 * Unfortunately, there "ain't no graceful way" to handle
 * certain situations.  For example, the kernel permits
 * arbitrary subnet bits -- that is, you could have a
 * 22 bit network field and a 10 bit subnet field.
 * However, due to braindamage at the user level, in
 * such sterling routines as getnetbyaddr, you need to
 * have a subnet mask which is an even multiple of 8.
 * Unless you are running with class C subnets, in which
 * case it should be a multiple of 4.  Because of this rot,
 * if you have non-multiples of 4 bits of subnet, you should
 * define DAMAGED_NETMASK when you compile.  This will round
 * things off to a multiple of 8 bits.
 *
 * Finally, you may want subnet support even if your system doesn't
 * support the ioctls to get subnet mask information.  If you want
 * such a thing, you can define NETMASK to be a constant that is
 * the subnet mask for your network.
 *
 * And don't *even* get me started on how the definitions of the inet_foo()
 * routines changed between 4.2 and 4.3, making internet addresses
 * be unsigned long vs. struct in_addr.  Don't blame me if this
 * won't lint...
 */

/*
 * One structure for each interface, containing
 * the network number and subnet mask, stored in HBO.
 */
struct in_if {
	u_long	i_net;		/* Network number, shifted right */
	u_long	i_subnetmask;	/* Subnet mask for this if */
	int	i_bitshift;	/* How many bits right for outside */
};

/*
 * Table (eventually, once we malloc) of
 * internet interface subnet information.
 */
static	struct in_if	*in_ifsni;

static	int		if_count;

/*
 * Get the network interface configuration,
 * and squirrel away the network numbers and
 * subnet masks of each interface.  Return
 * number of interfaces found, or -1 on error.
 * N.B.: don't call this more than once...
 */

int
getifconf()
{
#ifndef NETMASK
	register int	i, j;
	int		s;
	struct ifconf	ifc;
	char		buf[1024];
	register struct ifreq	*ifr;
        int		inet_netof();
	u_long		addr;
#ifdef STREAMS_TLI
	struct strioctl	ioc;
#endif

	/*
	 * Find out how many interfaces we have, and malloc
	 * room for information about each one.
	 */

#ifdef USE_STREAMS_DEVICE_FOR_IF_CONFIG
	s = open("/dev/ip", O_RDONLY);
#else
	s = socket(AF_INET, SOCK_DGRAM, 0);
#endif
	if (s < 0)
		return (-1);

	ifc.ifc_len = sizeof(buf);
#ifdef STREAMS_TLI
	ioc.ic_cmd = SIOCGIFCONF;
	ioc.ic_timout = 0;
	ioc.ic_dp = (caddr_t)buf;
	ioc.ic_len = sizeof(buf);
	if(ioctl(s, I_STR, &ioc) < 0 ||
	    ioc.ic_len < sizeof(struct ifreq)) {
		(void) close(s);
		return (-1);
	}
#ifdef SIZE_RETURNED_IN_BUFFER
	ifc.ifc_len = ioc.ic_len - sizeof(int);
	ifc.ifc_buf = buf + sizeof(int);
#else /* !SIZE_RETURNED_IN_BUFFER */
	ifc.ifc_len = ioc.ic_len;
	ifc.ifc_buf = buf;
#endif /* !SIZE_RETURNED_IN_BUFFER */

#else /* !STREAMS_TLI */
	ifc.ifc_len = sizeof(buf);
	ifc.ifc_buf = buf;
	if (ioctl(s, SIOCGIFCONF, (char *)&ifc) < 0) {
		(void) close(s);
		return (-1);
	}
#endif /* !STREAMS_TLI */

	/*
	 * if_count here is the count of possible
	 * interfaces we may be interested in... actual
	 * interfaces may be less (some may not be internet,
	 * not all are necessarily up, etc.)
	 */

#if defined(DEBUG) 
#if defined(SYSLOG)
	syslog(LOG_DEBUG, "getifconf: interface count: %d", if_count);
#else
	fprintf(stderr, "getifconf: interface count: %d\n", if_count);
#endif
#endif
	if_count = ifc.ifc_len / sizeof (struct ifreq);

	in_ifsni = (struct in_if *)
		malloc((unsigned) if_count * sizeof (struct in_if));
	if (in_ifsni == 0) {
		(void) close(s);
		return (-1);
	}

	for (i = j = 0; i < if_count; ++i) {
		struct sockaddr_in *s_in;

		ifr = &ifc.ifc_req[i];
#ifdef STREAMS_TLI
		ioc.ic_cmd = SIOCGIFFLAGS;
		ioc.ic_timout = 0;
		ioc.ic_dp = (caddr_t)ifr;
		ioc.ic_len = sizeof(struct ifreq);
		if(ioctl(s, I_STR, &ioc))
#else /* !STREAMS_TLI */
		if (ioctl(s, SIOCGIFFLAGS, (char *)ifr) < 0)
#endif
			continue;
		if ((ifr->ifr_flags & IFF_UP) == 0)
			continue;
#ifdef STREAMS_TLI
		ioc.ic_cmd = SIOCGIFADDR;
		ioc.ic_timout = 0;
		ioc.ic_dp = (caddr_t)ifr;
		ioc.ic_len = sizeof(struct ifreq);
		if(ioctl(s, I_STR, &ioc))
#else /* !STREAMS_TLI */
		if (ioctl(s, SIOCGIFADDR, (char *)ifr) < 0)
#endif
			continue;
		if (ifr->ifr_addr.sa_family != AF_INET)
			continue;
		s_in = (struct sockaddr_in *) &ifr->ifr_addr;
		addr = s_in->sin_addr.s_addr;
		in_ifsni[j].i_net = inet_netof(s_in->sin_addr);
#if defined(DEBUG) 
#if defined(SYSLOG)
	syslog(LOG_DEBUG, "getifconf: interface addr: %s",
		inet_ntoa(s_in->sin_addr));
#else
	fprintf(stderr, "getifconf: interface addr: %s\n",
		inet_ntoa(s_in->sin_addr));
#endif
#endif

#ifdef STREAMS_TLI
		ioc.ic_cmd = SIOCGIFNETMASK;
		ioc.ic_timout = 0;
		ioc.ic_dp = (caddr_t)ifr;
		ioc.ic_len = sizeof(struct ifreq);
		if(ioctl(s, I_STR, &ioc))
#else /* !STREAMS_TLI */
		if (ioctl(s, SIOCGIFNETMASK, (char *)ifr) < 0)
#endif
			continue;
		s_in = (struct sockaddr_in *) &ifr->ifr_addr;
		in_ifsni[j].i_subnetmask = ntohl(s_in->sin_addr.s_addr);
#if defined(DEBUG) 
#if defined(SYSLOG)
	syslog(LOG_DEBUG, "getifconf: interface subnet mask: %s",
		inet_ntoa(s_in->sin_addr));
#else
	fprintf(stderr, "getifconf: interface subnet mask: %s\n",
		inet_ntoa(s_in->sin_addr));
#endif
#endif		/*
		 * The following should "never happen".  But under SunOS
		 * 3.4, along with the rest of their broken networking code,
		 * SIOCGIFNETMASK can get a netmask which is 0.  There
		 * really isn't anything that "right" that we can do
		 * about it, so we'll set their subnet mask to be their
		 * *net*work mask.  Which may or may not be right.
		 */
		if (in_ifsni[j].i_subnetmask == 0) {
			addr = ntohl(addr);
			if (IN_CLASSA(addr))
				in_ifsni[j].i_subnetmask = IN_CLASSA_NET;
			else if (IN_CLASSB(addr))
				in_ifsni[j].i_subnetmask = IN_CLASSB_NET;
			else if (IN_CLASSC(addr))
				in_ifsni[j].i_subnetmask = IN_CLASSC_NET;
			else			/* what to do ... */
				in_ifsni[j].i_subnetmask = IN_CLASSC_NET;
		} else
			in_ifsni[j].i_bitshift = bsr(in_ifsni[j].i_subnetmask);
		j++;
	}

	if_count = j;

	(void) close(s);

	return (if_count);

#else	/* hard-coded subnets */

	if_count = 1;

	in_ifsni = (struct in_if *) malloc(if_count * sizeof (struct in_if));
	if (in_ifsni == 0) {
		return (-1);
	}
	in_ifsni[0].i_net = 0;
	in_ifsni[0].i_subnetmask = NETMASK;
	in_ifsni[0].i_bitshift = bsr(in_ifsni[0].i_subnetmask);
	return (if_count);
#endif
}


/*
 * Return the (sub)network number from an internet address.
 * "in" is in NBO, return value in host byte order.
 * If "in" is not a subnet, return 0.
 */

u_long
inet_snetof(in)
	u_long	in;
{
	register int	j;
	register u_long	i = ntohl(in);
	register u_long	net;
	int		inet_netof(), inet_lnaof();
	struct in_addr in_a;

	in_a.s_addr = in;
	net = inet_netof(in_a);

	/*
	 * Check whether network is a subnet;
	 * if so, return subnet number.
	 */
	for (j = 0; j < if_count; ++j)
#ifdef NETMASK
		if (1) {
#else
		if (net == in_ifsni[j].i_net) {
#endif
			net = i & in_ifsni[j].i_subnetmask;
			in_a.s_addr = htonl(net);
			if (inet_lnaof(in_a) == 0)
				return (0);
			else
				return (net >> in_ifsni[j].i_bitshift);
		}

	return (0);
}


/*
 * Return the number of bits required to
 * shift right a mask into a getnetent-able entity.
 */

int
bsr(mask)
	register long	mask;
{
	register int	count = 0;

	if (mask == 0)		/* "never happen", except with SunOS 3.4 */
		return (0);

	while ((mask & 1) == 0) {
		++count;
		mask >>= 1;
	}
#ifdef DAMAGED_NETMASK
	count /= 8;			/* XXX gag retch puke barf */
	count *= 8;
#endif
	return (count);
}

#endif

#ifdef TESTSUBNET
main()
{
     if(getifconf()<0)
	printf("getifconf failed!\n"); 
     else
	printf("getifconf succeeded\n"); 
}
#endif
