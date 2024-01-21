/*
 *			B I N G
 *
 * Using the InterNet Control Message Protocol (ICMP) "ECHO" facility,
 * measure point-to-point bandwidth.
 *
 * Hack by Pierre Beyssac (pb@fasterix.freenix.fr), based on FreeBSD ping.
 * Comments and bug reports welcome !
 *
 * Original ping author -
 *	Mike Muuss
 *	U. S. Army Ballistic Research Laboratory
 *	December, 1983
 *
 * bing.c,v
 * Revision 1.16  1995/07/20  23:45:32  pb
 * Typo in "Berkeley" in the legalese.
 * Submitted by Nat Makarevith (nat@nataa.frmug.fr.net).
 * No Nat, that was not normal :-)
 *
 * Revision 1.15  1995/07/20  22:41:48  pb
 * Changed default small packet size. Gives more accurate results.
 *
 * Revision 1.14  1995/07/20  21:59:59  pb
 * Minor changes in output format.
 *
 * Revision 1.13  1995/07/20  21:57:43  pb
 * One more cast for Solaris...
 *
 * Revision 1.12  1995/07/19  23:34:47  pb
 * New sanity check on roundtrip times suggested by <jcaron@pressimage.net>.
 *
 * Revision 1.11  1995/07/19  23:14:37  pb
 * More casts to please Solaris 2 cc.
 *
 * Revision 1.10  1995/07/19  23:07:52  pb
 * Cleanups in timeval handling and a few other places.
 * Warnings on rtt displayed only at the end unless the new option -w is used.
 * Improved final stats display.
 *
 * Revision 1.9  1995/07/18  21:57:19  pb
 * Replace srand48()/mrand48() with the more portable srandom()/random()
 * (suggested for BSDI 1.0 by wolf@pasteur.fr).
 *
 * Revision 1.8  1995/07/18  21:45:33  pb
 * Fixed some warnings issued by gcc -Wall.
 *
 * Revision 1.7  1995/07/18  20:20:21  pb
 * Fixed mx_dup_ck declaration to make the Solaris compiler happy.
 *
 * Revision 1.6  1995/07/18  19:51:55  pb
 * Include sys/types.h, AIX is unable to figure this out by itself.
 *
 * Revision 1.5  1995/07/18  19:44:59  pb
 * Displaying IP options works on Linux too.
 * Submitted by <laurent@brasil.frmug.fr.net>
 *
 * Revision 1.4  1995/07/17  22:36:42  pb
 * Linux port.
 *
 * Revision 1.3  1995/07/17  20:45:32  pb
 * option -q reversed as -V
 *
 * Revision 1.2  1995/07/17  20:28:35  pb
 * fixed usage (submitted by wolf@pasteur.fr)
 *
 * Revision 1.1.1.3  1995/07/16  21:06:41  pb
 * bing release 1.0.
 *
 *
 * Copyright (c) 1995 Pierre Beyssac.
 * All rights reserved.
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
 *	This product includes software developed by Pierre Beyssac,
 *	Mike Muss, the University of California, Berkeley and its contributors.
 * 4. Neither the name of the author nor the names of any co-contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY PIERRE BEYSSAC AND CONTRIBUTORS ``AS IS'' AND
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
 */

/* The original UCB copyright notice follows */

/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Muuss.
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
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
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
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char rcsid[] = "bing.c,v 1.16 1995/07/20 23:45:32 pb Exp";
#endif /* not lint */

#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/signal.h>

#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#ifndef linux
#include <netinet/ip_var.h>
#endif /* linux */
#include <netdb.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <limits.h>

#include "sysdep.h"

#ifdef NO_STRERROR
int sys_nerr;
char *sys_errlist[];
int errno;
char *sys_unk = "Unknown error";

#define strerror(e)	(((e)>=sys_nerr||(e)<0)?sys_unk:sys_errlist[(e)])
#endif

/*
 * The default small packet size should be big enough that no padding
 * needs to be done at the physical level (ethernet typically requires this).
 *
 * The initial value was chosen to be 8 bytes, just enough to
 * contain a struct timeval, but it proved too small. The current value is
 * chosen to be around 40 bytes. If you add the IP and ICMP headers, that
 * should be more than the minimal ethernet packet size.
 *
 * 44 is nice because that's 64 bytes less than the other value, which
 * has not been changed.
 *
 * The default big packet size is not too big so as not to waste resources
 * unless the user explicitly chooses to.
 */

#define	DEFDATALEN_SMALL	44	/* default small data len */
#define	DEFDATALEN_BIG		108	/* default big data len */

#define	MAXIPLEN	60
#define	MAXICMPLEN	76
#define	MAXPACKET	(65536 - 60 - 8)/* max packet size */
#define	MAXWAIT		10		/* max seconds to wait for response */
#define	NROUTES		9		/* number of record route slots */

#define	A(bit,tbl)	(tbl)[(unsigned)(bit)>>3] /* identify byte in array */
#define	B(bit)		(1 << ((bit) & 0x07))	/* identify bit in byte */
#define	SET(bit,tbl)	(A(bit,(tbl)) |= B(bit))
#define	CLR(bit,tbl)	(A(bit,(tbl)) &= (~B(bit)))
#define	TST(bit,tbl)	(A(bit,(tbl)) & B(bit))

/* various options */
int options;
#define	F_NODELTA	0x001
#define	F_INTERVAL	0x002
#define	F_NUMERIC	0x004
#define	F_PINGFILLED	0x008
#define	F_VVERBOSE	0x010
#define	F_RROUTE	0x020
#define	F_SO_DEBUG	0x040
#define	F_SO_DONTROUTE	0x080
#define	F_VERBOSE	0x100
#define	F_RANDOMFILL	0x200
#define	F_PEDANTIC	0x400
#define	F_WARN		0x800

/* multicast options */
int moptions;
#define MULTICAST_NOLOOP	0x001
#define MULTICAST_TTL		0x002
#define MULTICAST_IF		0x004

/*
 * MAX_DUP_CHK is the number of bits in received table, i.e. the maximum
 * number of received sequence numbers we can keep track of.  Change 128
 * to 8192 for complete accuracy...
 */
#define	MAX_DUP_CHK	(8 * 128)
unsigned short mx_dup_ck = MAX_DUP_CHK;
typedef char duptable[MAX_DUP_CHK / 8];

int datalen_small = DEFDATALEN_SMALL;
int datalen_big = DEFDATALEN_BIG;

int s;				/* socket file descriptor */
u_char outpack[MAXPACKET];

int ident;			/* process id to identify our packets */

/* counters */
long npackets = 1;		/* max sampling loops */
long nsamples;			/* max samples to take in a loop */
int interval = 4;		/* interval between packets */

/* timing */

struct dst {
	double tmin;			/* minimum */
	double tmax;			/* maximum */
	double tsum;			/* sum */
	long nsamples;			/* number of samples */
};

#define	dst_newsample(dst,s)	\
	if ((s) < (dst)->tmin) (dst)->tmin = (s);	\
	if ((s) > (dst)->tmax) (dst)->tmax = (s);	\
	(dst)->tsum += (s);				\
	(dst)->nsamples++;

#define dst_init(dst)	\
	(dst)->tmin = 1000.0*(double)LONG_MAX; \
	(dst)->tmax = 0.0; \
	(dst)->tsum = 0.0; \
	(dst)->nsamples = 0;

#define dst_min(dst)	((dst)->tmin)
#define dst_max(dst)	((dst)->tmax)
#define dst_avg(dst)	((dst)->nsamples ? (dst)->tsum/(dst)->nsamples : 0.0)

struct timestats {
	/* Time stats */
	struct dst rttstats;		/* round trip time stats */
					/* including # of packets we got back */
#define	nreceived rttstats.nsamples
	long nrepeats;			/* number of duplicates */
	long ntransmitted;		/* sequence # for outbound packets = #sent */
	duptable rcvd_tbl;		/* dup table */
};

#define ts_init(ts)	\
	dst_init(&(ts)->rttstats);	\
	(ts)->nrepeats = (ts)->ntransmitted = 0;

struct hoststats {
	/* Host info */
	char hnamebuf[MAXHOSTNAMELEN];
	char *hostname;
	struct sockaddr whereto;
	struct sockaddr_in *to;

	struct timestats ts_small, ts_big;
};

struct hoststats hoststats1, hoststats2;

#define tv_usval(tmv)	((double)((tmv)->tv_sec * 1000000 + (tmv)->tv_usec))
#define tv_hash(tmv)	((tmv)->tv_sec ^ (tmv)->tv_usec)

/*
 * tvsub --
 *	Subtract 2 timeval structs:  out = out - in.
 *	Out is assumed to be >= in.
 */
void tvsub(out, in)
	register struct timeval *out, *in;
{
	if ((out->tv_usec -= in->tv_usec) < 0) {
		--out->tv_sec;
		out->tv_usec += 1000000;
	}
	out->tv_sec -= in->tv_sec;
}

void set_ip(hs, target)
	struct hoststats *hs;
	char *target;
{
	struct hostent *hp;

	hs->to = (struct sockaddr_in *) &hs->whereto;

	bzero((char *)hs->to, sizeof(struct sockaddr_in));
	hs->to->sin_family = AF_INET;
	hs->to->sin_addr.s_addr = inet_addr(target);
	if (hs->to->sin_addr.s_addr != (u_int)-1)
		hs->hostname = target;
	else {
		hp = gethostbyname(target);
		if (!hp) {
			(void)fprintf(stderr,
			    "bing: unknown host %s\n", target);
			exit(1);
		}
		hs->to->sin_family = hp->h_addrtype;
		bcopy(hp->h_addr, (caddr_t)&hs->to->sin_addr, hp->h_length);
		(void)strncpy(hs->hnamebuf, hp->h_name, sizeof(hs->hnamebuf) - 1);
		hs->hostname = hs->hnamebuf;
	}
}

int
recvfrom_timeout(buf, len, flags, from, fromlen, time_out)
	void *buf;
	int len;
	int flags;
	struct sockaddr *from;
	int *fromlen;
	u_int time_out;
{
	int cc;
	alarm(time_out);
	for (;;) {
		if ((cc = recvfrom(s, buf, len, flags, from, fromlen)) < 0) {
			if (errno == EINTR)
				return cc;
			perror("bing: recvfrom");
			continue;
		} else {
			break;
		}
	}
	alarm((u_int)0);
	return cc;
}

void randomfill(bp, len, seed)
	long *bp;
	int len;
	long seed;
{
	srandom((unsigned)seed);
	while (len > 0) {
		*bp++ = htonl(random());
		len -= sizeof(long);
	}
}

static long lastrand;
static char nrand;

void randominit(seed)
	long seed;
{
	srandom((unsigned)seed);
	nrand = 0;
}

u_char randomnext()
{
	u_char r;
	if (nrand-- == 0) {
		lastrand = random();
		nrand = 3;
	}
	r = lastrand >> 24;
	lastrand <<= 8;
	return r;
}

void
alarm_handler()
{
	/* do nothing quick */
}

/*
 * in_cksum --
 *	Checksum routine for Internet Protocol family headers (C Version)
 */
int in_cksum(addr, len)
	u_short *addr;
	int len;
{
	register int nleft = len;
	register u_short *w = addr;
	register int sum = 0;
	u_short answer = 0;

	/*
	 * Our algorithm is simple, using a 32 bit accumulator (sum), we add
	 * sequential 16 bit words to it, and at the end, fold back all the
	 * carry bits from the top 16 bits into the lower 16 bits.
	 */
	while (nleft > 1)  {
		sum += *w++;
		nleft -= 2;
	}

	/* mop up an odd byte, if necessary */
	if (nleft == 1) {
		*(u_char *)(&answer) = *(u_char *)w ;
		sum += answer;
	}

	/* add back carry outs from top 16 bits to low 16 bits */
	sum = (sum >> 16) + (sum & 0xffff);	/* add hi 16 to low 16 */
	sum += (sum >> 16);			/* add carry */
	answer = ~sum;				/* truncate to 16 bits */
	return(answer);
}

/*
 * pinger --
 * 	Compose and transmit an ICMP ECHO REQUEST packet.  The IP packet
 * will be added on by the kernel.  The ID field is our UNIX process ID,
 * and the sequence number is an ascending integer.  The first 8 bytes
 * of the data portion are used to hold a UNIX "timeval" struct in VAX
 * byte-order, to compute the round-trip time.
 */
void pinger(hs, datalen)
	struct hoststats *hs;
	int datalen;
{
	struct timestats *ts;
	register struct icmp *icp;
	register int cc;
	int i;

	if (datalen == datalen_small)
		ts = &hs->ts_small;
	else
		ts = &hs->ts_big;

	icp = (struct icmp *)outpack;
	icp->icmp_type = ICMP_ECHO;
	icp->icmp_code = 0;
	icp->icmp_cksum = 0;
	icp->icmp_seq = (ts->ntransmitted)++;
	icp->icmp_id = ident;			/* ID */

	CLR(icp->icmp_seq % mx_dup_ck, ts->rcvd_tbl);

	(void)gettimeofday((struct timeval *)&outpack[8],
	    (struct timezone *)NULL);

	if (options & F_RANDOMFILL)
		randomfill((long *)(outpack + 16), datalen - 8,
			   tv_hash((struct timeval *)&outpack[8]));

	cc = datalen + 8;			/* skips ICMP portion */

	/* compute ICMP checksum here */
	icp->icmp_cksum = in_cksum((u_short *)icp, cc);

	i = sendto(s, (char *)outpack, cc, 0, (struct sockaddr *)hs->to,
	    sizeof(struct sockaddr));

	if (i < 0 || i != cc)  {
		if (i < 0)
			perror("bing: sendto");
		(void)printf("bing: wrote %s %d chars, ret=%d\n",
		    hs->hostname, cc, i);
	}
}

/*
 * pr_iph --
 *	Print an IP header with options.
 */
void pr_iph(ip)
	struct ip *ip;
{
	int hlen;
	u_char *cp;

	hlen = ip->ip_hl << 2;
	cp = (u_char *)ip + 20;		/* point to options */

	(void)printf("Vr HL TOS  Len   ID Flg  off TTL Pro  cks      Src      Dst Data\n");
	(void)printf(" %1x  %1x  %02x %04x %04x",
	    ip->ip_v, ip->ip_hl, ip->ip_tos, ip->ip_len, ip->ip_id);
	(void)printf("   %1x %04x", ((ip->ip_off) & 0xe000) >> 13,
	    (ip->ip_off) & 0x1fff);
	(void)printf("  %02x  %02x %04x", ip->ip_ttl, ip->ip_p, ip->ip_sum);
#ifdef linux
	(void)printf(" %s ", inet_ntoa(ip->ip_src));
	(void)printf(" %s ", inet_ntoa(ip->ip_dst));
#else
	(void)printf(" %s ", inet_ntoa(*(struct in_addr *)&ip->ip_src.s_addr));
	(void)printf(" %s ", inet_ntoa(*(struct in_addr *)&ip->ip_dst.s_addr));
#endif /* linux */
	/* dump any option bytes */
	while (hlen-- > 20) {
		(void)printf("%02x", *cp++);
	}
	(void)putchar('\n');
}

/*
 * pr_retip --
 *	Dump some info on a returned (via ICMP) IP packet.
 */
void pr_retip(ip)
	struct ip *ip;
{
	int hlen;
	u_char *cp;

	pr_iph(ip);
	hlen = ip->ip_hl << 2;
	cp = (u_char *)ip + hlen;

	if (ip->ip_p == 6)
		(void)printf("TCP: from port %u, to port %u (decimal)\n",
		    (*cp * 256 + *(cp + 1)), (*(cp + 2) * 256 + *(cp + 3)));
	else if (ip->ip_p == 17)
		(void)printf("UDP: from port %u, to port %u (decimal)\n",
			(*cp * 256 + *(cp + 1)), (*(cp + 2) * 256 + *(cp + 3)));
}

#ifdef notdef
static char *ttab[] = {
	"Echo Reply",		/* ip + seq + udata */
	"Dest Unreachable",	/* net, host, proto, port, frag, sr + IP */
	"Source Quench",	/* IP */
	"Redirect",		/* redirect type, gateway, + IP  */
	"Echo",
	"Time Exceeded",	/* transit, frag reassem + IP */
	"Parameter Problem",	/* pointer + IP */
	"Timestamp",		/* id + seq + three timestamps */
	"Timestamp Reply",	/* " */
	"Info Request",		/* id + sq */
	"Info Reply"		/* " */
};
#endif

/*
 * pr_icmph --
 *	Print a descriptive string about an ICMP header.
 */
void pr_icmph(icp)
	struct icmp *icp;
{
	switch(icp->icmp_type) {
	case ICMP_ECHOREPLY:
		(void)printf("Echo Reply\n");
		/* XXX ID + Seq + Data */
		break;
	case ICMP_UNREACH:
		switch(icp->icmp_code) {
		case ICMP_UNREACH_NET:
			(void)printf("Destination Net Unreachable\n");
			break;
		case ICMP_UNREACH_HOST:
			(void)printf("Destination Host Unreachable\n");
			break;
		case ICMP_UNREACH_PROTOCOL:
			(void)printf("Destination Protocol Unreachable\n");
			break;
		case ICMP_UNREACH_PORT:
			(void)printf("Destination Port Unreachable\n");
			break;
		case ICMP_UNREACH_NEEDFRAG:
			(void)printf("frag needed and DF set\n");
			break;
		case ICMP_UNREACH_SRCFAIL:
			(void)printf("Source Route Failed\n");
			break;
		default:
			(void)printf("Dest Unreachable, Bad Code: %d\n",
			    icp->icmp_code);
			break;
		}
		/* Print returned IP header information */
		pr_retip((struct ip *)ICMP_TO_DATA(icp));
		break;
	case ICMP_SOURCEQUENCH:
		(void)printf("Source Quench\n");
		pr_retip((struct ip *)ICMP_TO_DATA(icp));
		break;
	case ICMP_REDIRECT:
		switch(icp->icmp_code) {
		case ICMP_REDIRECT_NET:
			(void)printf("Redirect Network");
			break;
		case ICMP_REDIRECT_HOST:
			(void)printf("Redirect Host");
			break;
		case ICMP_REDIRECT_TOSNET:
			(void)printf("Redirect Type of Service and Network");
			break;
		case ICMP_REDIRECT_TOSHOST:
			(void)printf("Redirect Type of Service and Host");
			break;
		default:
			(void)printf("Redirect, Bad Code: %d", icp->icmp_code);
			break;
		}
#ifdef linux
		(void)printf("(New addr: 0x%08lx)\n", icp->icmp_gwaddr);
#else
		(void)printf("(New addr: 0x%08lx)\n", icp->icmp_gwaddr.s_addr);
#endif /* linux */
		pr_retip((struct ip *)ICMP_TO_DATA(icp));
		break;
	case ICMP_ECHO:
		(void)printf("Echo Request\n");
		/* XXX ID + Seq + Data */
		break;
	case ICMP_TIMXCEED:
		switch(icp->icmp_code) {
		case ICMP_TIMXCEED_INTRANS:
			(void)printf("Time to live exceeded\n");
			break;
		case ICMP_TIMXCEED_REASS:
			(void)printf("Frag reassembly time exceeded\n");
			break;
		default:
			(void)printf("Time exceeded, Bad Code: %d\n",
			    icp->icmp_code);
			break;
		}
		pr_retip((struct ip *)ICMP_TO_DATA(icp));
		break;
	case ICMP_PARAMPROB:
#ifdef linux
		(void)printf("Parameter problem: pointer = 0x%02x\n",
		    icp->un.gateway);
#else
		(void)printf("Parameter problem: pointer = 0x%02x\n",
		    icp->icmp_hun.ih_pptr);
#endif /* linux */
		pr_retip((struct ip *)ICMP_TO_DATA(icp));
		break;
	case ICMP_TSTAMP:
		(void)printf("Timestamp\n");
		/* XXX ID + Seq + 3 timestamps */
		break;
	case ICMP_TSTAMPREPLY:
		(void)printf("Timestamp Reply\n");
		/* XXX ID + Seq + 3 timestamps */
		break;
	case ICMP_IREQ:
		(void)printf("Information Request\n");
		/* XXX ID + Seq */
		break;
	case ICMP_IREQREPLY:
		(void)printf("Information Reply\n");
		/* XXX ID + Seq */
		break;
#ifdef ICMP_MASKREQ
	case ICMP_MASKREQ:
		(void)printf("Address Mask Request\n");
		break;
#endif
#ifdef ICMP_MASKREPLY
	case ICMP_MASKREPLY:
		(void)printf("Address Mask Reply\n");
		break;
#endif
	default:
		(void)printf("Bad ICMP type: %d\n", icp->icmp_type);
	}
}

/*
 * pr_addr --
 *	Return an ascii host address as a dotted quad and optionally with
 * a hostname.
 */
char *
pr_addr(l)
	u_long l;
{
	struct hostent *hp;
	static char buf[80];

	if ((options & F_NUMERIC) ||
	    !(hp = gethostbyaddr((char *)&l, 4, AF_INET)))
		(void)sprintf(buf, "%s", inet_ntoa(*(struct in_addr *)&l));
	else
		(void)sprintf(buf, "%s (%s)", hp->h_name,
		    inet_ntoa(*(struct in_addr *)&l));
	return(buf);
}

/*
 * pr_pack --
 *	Print out the packet, if it came from us.  This logic is necessary
 * because ALL readers of the ICMP socket get a copy of ALL ICMP packets
 * which arrive ('tis only fair).  This permits multiple copies of this
 * program to be run without having intermingled output (or statistics!).
 */
int pr_pack(buf, cc, from)
	char *buf;
	int cc;
	struct sockaddr_in *from;
{
	struct timestats *ts;
	struct hoststats *hs;

	register struct icmp *icp;
	register u_long l;
	register int i, j;
	register u_char *cp,*dp;
	u_char d;
	static int old_rrlen;
	static char old_rr[MAX_IPOPTLEN];
	struct ip *ip;
	struct timeval tv, *tp;
	double triptime;
	int hlen, dupflag;

	(void)gettimeofday(&tv, (struct timezone *)NULL);

	if (from->sin_addr.s_addr == hoststats1.to->sin_addr.s_addr) {
		hs = &hoststats1;
	} else if (from->sin_addr.s_addr == hoststats2.to->sin_addr.s_addr) {
		hs = &hoststats2;
	} else {
		(void)fprintf(stderr,
		  "bing: packet (%d bytes) from unexpected host %s\n", cc,
		  inet_ntoa(*(struct in_addr *)&from->sin_addr.s_addr));
		return -1;
	}

	if (cc == datalen_small + 28)
		ts = &hs->ts_small;
	else if (cc == datalen_big + 28)
		ts = &hs->ts_big;
	else {
		(void)fprintf(stderr,
		  "bing: unexpected packet size (%d bytes) from %s\n", cc,
		  inet_ntoa(*(struct in_addr *)&from->sin_addr.s_addr));
		return -1;
	}

	/* Check the IP header */
	ip = (struct ip *)buf;
	hlen = ip->ip_hl << 2;
	if (cc < hlen + ICMP_MINLEN) {
		if (options & F_VERBOSE)
			(void)fprintf(stderr,
			  "bing: packet too short (%d bytes) from %s\n", cc,
			  inet_ntoa(*(struct in_addr *)&from->sin_addr.s_addr));
		return -1;
	}

	/* Now the ICMP part */
	cc -= hlen;
	icp = (struct icmp *)(buf + hlen);
	if (icp->icmp_type == ICMP_ECHOREPLY) {
		if (icp->icmp_id != ident)
			return -1;			/* 'Twas not our ECHO */
		tp = (struct timeval *)ICMP_TO_DATA(icp);
		tvsub(&tv, tp);
		triptime = tv_usval(&tv);

		dst_newsample(&ts->rttstats, triptime);

		if (TST(icp->icmp_seq % mx_dup_ck, ts->rcvd_tbl)) {
			++(ts->nrepeats);
			dupflag = 1;
		} else {
			SET(icp->icmp_seq % mx_dup_ck, ts->rcvd_tbl);
			dupflag = 0;
		}

		if (!(options & F_VVERBOSE))
			return 0;

		(void)printf("%d bytes from %s: icmp_seq=%u", cc,
		   inet_ntoa(*(struct in_addr *)&from->sin_addr.s_addr),
		   icp->icmp_seq);
		(void)printf(" ttl=%d", ip->ip_ttl);
		(void)printf(" time=%.3f ms", triptime/1000.0);
		if (dupflag)
			(void)printf(" (DUP!)");
		/* check the data */
		cp = ICMP_TO_DATA(icp) + 8;
		if (options & F_RANDOMFILL) {
			randominit(tv_hash(tp));
		} else {
			dp = &outpack[8 + sizeof(struct timeval)];
		}
		for (i = 8; i < cc - 8; ++i, ++cp, ++dp) {
			if (options & F_RANDOMFILL) {
				d = randomnext();
			} else {
				d = *dp;
			}
			if (*cp != d) {
	(void)printf("\nwrong data byte #%d should be 0x%x but was 0x%x",
		    i, d, *cp);
				cp = ICMP_TO_DATA(icp);
				for (i = 8; i < cc; ++i, ++cp) {
					if ((i % 32) == 8)
						(void)printf("\n\t");
					(void)printf("%x ", *cp);
				}
				break;
			}
		}
	} else {
		/* We've got something other than an ECHOREPLY */
		if (!(options & F_VERBOSE))
			return -1;
		(void)printf("%d bytes from %s: ", cc,
		    pr_addr(from->sin_addr.s_addr));
		pr_icmph(icp);
	}

	/* Display any IP options */
	cp = (u_char *)buf + sizeof(struct ip);

	for (; hlen > (int)sizeof(struct ip); --hlen, ++cp)
		switch (*cp) {
		case IPOPT_EOL:
			hlen = 0;
			break;
		case IPOPT_LSRR:
			(void)printf("\nLSRR: ");
			hlen -= 2;
			j = *++cp;
			++cp;
			if (j > IPOPT_MINOFF)
				for (;;) {
					l = *++cp;
					l = (l<<8) + *++cp;
					l = (l<<8) + *++cp;
					l = (l<<8) + *++cp;
					if (l == 0)
						(void)printf("\t0.0.0.0");
				else
					(void)printf("\t%s", pr_addr(ntohl(l)));
				hlen -= 4;
				j -= 4;
				if (j <= IPOPT_MINOFF)
					break;
				(void)putchar('\n');
			}
			break;
		case IPOPT_RR:
			j = *++cp;		/* get length */
			i = *++cp;		/* and pointer */
			hlen -= 2;
			if (i > j)
				i = j;
			i -= IPOPT_MINOFF;
			if (i <= 0)
				continue;
			if (i == old_rrlen
			    && cp == (u_char *)buf + sizeof(struct ip) + 2
			    && !bcmp((char *)cp, old_rr, i)) {
				(void)printf("\t(same route)");
				i = ((i + 3) / 4) * 4;
				hlen -= i;
				cp += i;
				break;
			}
			old_rrlen = i;
			bcopy((char *)cp, old_rr, i);
			(void)printf("\nRR: ");
			for (;;) {
				l = *++cp;
				l = (l<<8) + *++cp;
				l = (l<<8) + *++cp;
				l = (l<<8) + *++cp;
				if (l == 0)
					(void)printf("\t0.0.0.0");
				else
					(void)printf("\t%s", pr_addr(ntohl(l)));
				hlen -= 4;
				i -= 4;
				if (i <= 0)
					break;
				(void)putchar('\n');
			}
			break;
		case IPOPT_NOP:
			(void)printf("\nNOP");
			break;
		default:
			(void)printf("\nunknown option %x", *cp);
			break;
		}
	(void)putchar('\n');
	(void)fflush(stdout);
	return 0;
}

void ping_and_wait(hs, datalen, buf, buflen, timeout)
	struct hoststats *hs;
	int datalen;
	u_char *buf;
	int buflen;
	u_int timeout;
{
	struct sockaddr_in from;
	register int cc;
	int fromlen;

	pinger(hs, datalen);

	fromlen = sizeof(from);
	while ((cc = recvfrom_timeout((char *)buf, buflen, 0,
	    (struct sockaddr *)&from, &fromlen, interval)) >= 0) {
		if (pr_pack((char *)buf, cc, &from) == 0)
			break;
	}
}

void warn_rtt(min1s, min1b, min2s, min2b)
	double min1s, min1b, min2s, min2b;
{
	double deltab, deltas;
	char *pmsg = (options & F_PEDANTIC) ? " (ignored)" : "";

	if (min1s > min2s)
		fprintf(stderr,
			"warning: rtt small host1 %.3fms > rtt small host2 %.3fms%s\n",
			min1s / 1000, min2s / 1000, pmsg);
	if (min1b > min2b)
		fprintf(stderr,
			"warning: rtt big host1 %.3fms > rtt big host2 %.3fms%s\n",
			min1b / 1000, min2b / 1000, pmsg);
	if (min1b < min1s)
		fprintf(stderr,
			"warning: rtt big host1 %.3fms < rtt small host2 %.3fms%s\n",
			min1b / 1000, min1s / 1000, pmsg);
	if (min2b < min2s)
		fprintf(stderr,
			"warning: rtt big host2 %.3fms < rtt small host2 %.3fms%s\n",
			min2b / 1000, min2s / 1000, pmsg);
	deltab = min2b - min1b;
	deltas = min2s - min1s;
	if (deltab < deltas)
		fprintf(stderr,
			"warning: delta big rtts %.3fms < delta small rtts %.3fms%s\n",
			deltab / 1000, deltas / 1000, pmsg);
}

void adapt_rtt(min1s, min1b, min2s, min2b)
	double *min1s, *min1b, *min2s, *min2b;
{
	double deltab, deltas;

	if (options & F_PEDANTIC)
		return;

	if (*min1s > *min2s)
		*min2s = *min1s;
	if (*min1b > *min2b)
		*min2b = *min1b;
	if (*min1b < *min1s)
		*min1s = *min1b;
	if (*min2b < *min2s)
		*min2s = *min2b;

	deltab = min2b - min1b;
	deltas = min2s - min1s;
	if (deltab < deltas) {
		*min2s = *min2b;
		*min1s = *min1b;
	}
}

void
stopit()
{
	(void)signal(SIGINT, SIG_IGN);
	(void)fflush(stdout);
}

void
finishpa(ntransmitted, received, nrepeats, vmin, vavg, vmax)
	int ntransmitted, received, nrepeats;
	double vmin, vavg, vmax;
{
	(void)printf("%6ld%6ld", ntransmitted, received - nrepeats);
	if (nrepeats)
		(void)printf("%6ld", nrepeats);
	else
		(void)printf("      ");
	if (ntransmitted)
		if (received - nrepeats > ntransmitted)
			(void)printf("  ****\t");
		else
			(void)printf("%5d%%\t",
			    (int) (((ntransmitted - received + nrepeats)
				* 100) /
			    ntransmitted));
	else
		(void)printf("      \t");
	if (received - nrepeats)
		(void)printf("    %9.3f %9.3f %9.3f\n",
			vmin/1000.0,
			vavg/1000.0,
			vmax/1000.0);
	else
		(void)putchar('\n');
}

#define finishp(ts) finishpa((ts)->ntransmitted,	\
			     (ts)->nreceived,		\
			     (ts)->nrepeats,		\
			     dst_min(&(ts)->rttstats),	\
			     dst_avg(&(ts)->rttstats),	\
			     dst_max(&(ts)->rttstats))

/*
 * finish --
 *	Print out statistics for a host
 */

void finish(hs)
	struct hoststats *hs;
{
	(void)putchar('\n');
	(void)printf("--- %s statistics ---\n", hs->hostname);
	(void)printf(
"bytes   out    in   dup  loss\trtt (ms): min       avg       max\n");
	(void)printf("%5d", datalen_small);
	finishp(&hs->ts_small);
	(void)printf("%5d", datalen_big);
	finishp(&hs->ts_big);
}

void finishit()
{
	long bits, secs;
	double pfact, pfact_small, pfact_big;
	double maxthru, avgthru;
	double mindel, avgdel;
	double rtt1s, rtt1b, rtt2s, rtt2b;

	stopit();
	finish(&hoststats1);
	finish(&hoststats2);

	if (hoststats1.ts_big.nreceived == 0
	  || hoststats1.ts_small.nreceived == 0
	  || hoststats2.ts_big.nreceived == 0
	  || hoststats2.ts_small.nreceived == 0) {
		(void)printf(
"\nnot enough received packets to estimate link characteristics.\n");
		return;
	}

	(void)printf("\n--- estimated link characteristics ---\n");

	bits = (datalen_big - datalen_small) * (8*2);

	rtt1s = dst_min(&hoststats1.ts_small.rttstats);
	rtt1b = dst_min(&hoststats1.ts_big.rttstats);
	rtt2s = dst_min(&hoststats2.ts_small.rttstats);
	rtt2b = dst_min(&hoststats2.ts_big.rttstats);
	warn_rtt(rtt1s, rtt1b, rtt2s, rtt2b);
	adapt_rtt(&rtt1s, &rtt1b, &rtt2s, &rtt2b);
	secs = (rtt2b - rtt1b) - (rtt2s - rtt1s);

	if (secs == 0) {
		(void)printf(
"\nminimum delay difference is zero, can't estimate link throughput.\n");
		return;
	}

	maxthru = bits / (double)secs * 1000000;
	(void)printf("estimated throughput %.0fbps\n", maxthru);

	mindel = (dst_min(&hoststats2.ts_small.rttstats)
		- dst_min(&hoststats1.ts_small.rttstats)) / 1000000
		- (datalen_small * (8*2)) / maxthru;
	(void)printf("minimum delay per packet %.3fms (%.0f bits)\n",
		     mindel * 1000, mindel*maxthru);

	printf ("\naverage statistics (experimental) :\n");

	pfact_big =	  (double)hoststats2.ts_big.nreceived
			* hoststats1.ts_big.ntransmitted
			/ hoststats2.ts_big.ntransmitted
			/ hoststats1.ts_big.nreceived;

	pfact_small =	  (double)hoststats2.ts_small.nreceived
			* hoststats1.ts_small.ntransmitted
			/ hoststats2.ts_small.ntransmitted
			/ hoststats1.ts_small.nreceived;

	pfact =	  (double)(  hoststats2.ts_small.nreceived
		   + hoststats2.ts_big.nreceived)
		* (  hoststats1.ts_small.ntransmitted
		   + hoststats1.ts_big.ntransmitted)
		/ (  hoststats2.ts_small.ntransmitted
		   + hoststats2.ts_big.ntransmitted)
		/ (  hoststats1.ts_small.nreceived
		   + hoststats1.ts_small.nreceived);

	(void)printf("packet loss: small %d%%, big %d%%, total %d%%\n",
		      (int) ((1 - pfact_small) * 100),
		      (int) ((1 - pfact_big) * 100),
		      (int) ((1 - pfact) * 100));

	rtt1s = dst_avg(&hoststats1.ts_small.rttstats);
	rtt1b = dst_avg(&hoststats1.ts_big.rttstats);
	rtt2s = dst_avg(&hoststats2.ts_small.rttstats);
	rtt2b = dst_avg(&hoststats2.ts_big.rttstats);
	warn_rtt(rtt1s, rtt1b, rtt2s, rtt2b);
	adapt_rtt(&rtt1s, &rtt1b, &rtt2s, &rtt2b);
	secs = (rtt2b - rtt1b) - (rtt2s - rtt1s);

	if (secs == 0) {
		(void)printf(
"\naverage delay difference is zero, can't estimate average link throughput.\n");
		return;
	}
	avgthru = bits / (double)secs * 1000000;
	(void)printf("average throughput %.0fbps\n", avgthru);

	avgdel = (dst_avg(&hoststats2.ts_small.rttstats)
		- dst_avg(&hoststats1.ts_small.rttstats)) / 1000000
		- (datalen_small * (8*2)) / maxthru;
	(void)printf("average delay per packet %.3fms (%.0f bits)\n",
		     avgdel * 1000, avgdel*maxthru);

	(void)printf("weighted average throughput %.0fbps\n\n",
		     pfact * bits / (double)secs * 1000000);

	return;
}

void finishit_exit()
{
	(void) finishit();
	exit(0);
}

void fill(bp, patp)
	char *bp, *patp;
{
	register int ii, jj, kk;
	int pat[16];
	char *cp;

	for (cp = patp; *cp; cp++)
		if (!isxdigit(*cp)) {
			(void)fprintf(stderr,
			    "bing: patterns must be specified as hex digits.\n");
			exit(1);
		}
	ii = sscanf(patp,
	    "%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x",
	    &pat[0], &pat[1], &pat[2], &pat[3], &pat[4], &pat[5], &pat[6],
	    &pat[7], &pat[8], &pat[9], &pat[10], &pat[11], &pat[12],
	    &pat[13], &pat[14], &pat[15]);

	if (ii > 0)
		for (kk = 0; kk <= MAXPACKET - (8 + ii); kk += ii)
			for (jj = 0; jj < ii; ++jj)
				bp[jj + kk] = pat[jj];
	if (options & F_VVERBOSE) {
		(void)printf("PATTERN: 0x");
		for (jj = 0; jj < ii; ++jj)
			(void)printf("%02x", bp[jj] & 0xFF);
		(void)printf("\n");
	}
}

void usage()
{
	(void)fprintf(stderr,
	    "usage: bing [-dDnrRPvVwz] [-c count] [-e samples] [-i wait]\n\t[-p pattern] [-s small packetsize] [-S big packetsize] [-t ttl]\n\t[-I interface address] host1 host2\n");
	exit(1);
}

int main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	extern char *optarg;
	struct protoent *proto;
	struct in_addr ifaddr;
	struct hoststats *hs1, *hs2;
	int ntrans, nloops, bits;
	int i;
	int ch, hold, recv_packlen, preload;
	u_char *datap, *recv_packet;
	char *target1, *target2, *malloc();
	u_char ttl, loop;
#ifdef IP_OPTIONS
	char rspace[3 + 4 * NROUTES + 1];	/* record route space */
#endif

	hs1 = &hoststats1;
	hs2 = &hoststats2;

	preload = 0;
	datap = &outpack[8 + sizeof(struct timeval)];
	while ((ch = getopt(argc, argv, "I:LRc:dDe:fh:i:l:nPp:rS:s:t:vVwz")) != EOF)
		switch(ch) {
		case 'c':
			npackets = atoi(optarg);
			if (npackets <= 0) {
				(void)fprintf(stderr,
				    "bing: bad number of packets to transmit.\n");
				exit(1);
			}
			break;
		case 'D':
			options |= F_NODELTA;
			break;
		case 'P':
			options |= F_PEDANTIC;
			break;
		case 'w':
			options |= F_WARN;
			break;
		case 'd':
			options |= F_SO_DEBUG;
			break;
		case 'e':
			nsamples = atoi(optarg);
			if (nsamples <= 0) {
				(void)fprintf(stderr,
				    "bing: bad number of samples.\n");
				exit(1);
			}
			break;
		case 'i':		/* wait between sending packets */
			interval = atoi(optarg);
			if (interval <= 0) {
				(void)fprintf(stderr,
				    "bing: bad timing interval.\n");
				exit(1);
			}
			options |= F_INTERVAL;
			break;
		case 'n':
			options |= F_NUMERIC;
			break;
		case 'p':		/* fill buffer with user pattern */
			options |= F_PINGFILLED;
			fill((char *)datap, optarg);
			break;
		case 'V':
			options |= F_VVERBOSE;
			break;
		case 'R':
			options |= F_RROUTE;
			break;
		case 'r':
			options |= F_SO_DONTROUTE;
			break;
		case 'S':		/* size of big packet to send */
			datalen_big = atoi(optarg);
			if (datalen_big > MAXPACKET) {
				(void)fprintf(stderr,
				    "bing: big packet size too large.\n");
				exit(1);
			}
			if (datalen_big <= 0) {
				(void)fprintf(stderr,
				    "bing: illegal big packet size.\n");
				exit(1);
			}
			break;
		case 's':		/* size of small packet to send */
			datalen_small = atoi(optarg);
			if (datalen_small > MAXPACKET) {
				(void)fprintf(stderr,
				    "bing: small packet size too large.\n");
				exit(1);
			}
			if (datalen_small <= 0) {
				(void)fprintf(stderr,
				    "bing: illegal small packet size.\n");
				exit(1);
			}
			break;
		case 'v':
			options |= F_VERBOSE;
			break;
		case 'z':
			options |= F_RANDOMFILL;
			break;
		case 'L':
			moptions |= MULTICAST_NOLOOP;
			loop = 0;
			break;
		case 't':
			moptions |= MULTICAST_TTL;
			i = atoi(optarg);
			if (i < 0 || i > 255) {
				printf("ttl %u out of range\n", i);
				exit(1);
			}
			ttl = i;
			break;
		case 'I':
			moptions |= MULTICAST_IF;
			{
				int i1, i2, i3, i4;

				if (sscanf(optarg, "%u.%u.%u.%u%c",
					   &i1, &i2, &i3, &i4, &i) != 4) {
					printf("bad interface address '%s'\n",
					       optarg);
					exit(1);
				}
				ifaddr.s_addr = (i1<<24)|(i2<<16)|(i3<<8)|i4;
				ifaddr.s_addr = htonl(ifaddr.s_addr);
			}
			break;
		default:
			usage();
		}

	if (datalen_small >= datalen_big) {
		(void)fprintf(stderr,
			"bing: small packet size >= big packet size\n");
		exit(1);
	}

	if (datalen_small < sizeof(struct timeval)) /* can we time transfer ? */ {
		(void)fprintf(stderr,
			"bing: small packet size too small for timestamp\n");
		exit(1);
	}

	argc -= optind;
	argv += optind;
	
	if (argc != 2)
		usage();

	target1 = argv[0];
	target2 = argv[1];

	set_ip(hs1, target1);
	set_ip(hs2, target2);

	recv_packlen = datalen_big + MAXIPLEN + MAXICMPLEN;
	if (!(recv_packet = (u_char *)malloc((u_int)recv_packlen))) {
		(void)fprintf(stderr, "bing: out of memory.\n");
		exit(1);
	}
	if (!(options & F_PINGFILLED))
		for (i = 8; i < datalen_big; ++i)
			*datap++ = i;

	ident = getpid() & 0xFFFF;

	if (!(proto = getprotobyname("icmp"))) {
		(void)fprintf(stderr, "bing: unknown protocol icmp.\n");
		exit(1);
	}
	if ((s = socket(AF_INET, SOCK_RAW, proto->p_proto)) < 0) {
		perror("bing: socket");
		exit(1);
	}
	hold = 1;
	if (options & F_SO_DEBUG)
		(void)setsockopt(s, SOL_SOCKET, SO_DEBUG, (char *)&hold,
		    sizeof(hold));
	if (options & F_SO_DONTROUTE)
		(void)setsockopt(s, SOL_SOCKET, SO_DONTROUTE, (char *)&hold,
		    sizeof(hold));

	/* record route option */
	if (options & F_RROUTE) {
#ifdef IP_OPTIONS
		rspace[IPOPT_OPTVAL] = IPOPT_RR;
		rspace[IPOPT_OLEN] = sizeof(rspace)-1;
		rspace[IPOPT_OFFSET] = IPOPT_MINOFF;
		if (setsockopt(s, IPPROTO_IP, IP_OPTIONS, rspace,
		    sizeof(rspace)) < 0) {
			perror("bing: record route");
			exit(1);
		}
#else
		(void)fprintf(stderr,
		  "bing: record route not available in this implementation.\n");
		exit(1);
#endif /* IP_OPTIONS */
	}

	/*
	 * When pinging the broadcast address, you can get a lot of answers.
	 * Doing something so evil is useful if you are trying to stress the
	 * ethernet, or just want to fill the arp cache to get some stuff for
	 * /etc/ethers.
	 */
	hold = 48 * 1024;
	(void)setsockopt(s, SOL_SOCKET, SO_RCVBUF, (char *)&hold,
	    sizeof(hold));

#ifdef IP_MULTICAST_NOLOOP
	if (moptions & MULTICAST_NOLOOP) {
		if (setsockopt(s, IPPROTO_IP, IP_MULTICAST_LOOP,
					(char *)&loop, 1) == -1) {
			perror ("can't disable multicast loopback");
			exit(92);
		}
	}
#endif
#ifdef IP_MULTICAST_TTL
	if (moptions & MULTICAST_TTL) {
		if (setsockopt(s, IPPROTO_IP, IP_MULTICAST_TTL,
					(char *)&ttl, 1) == -1) {
			perror ("can't set multicast time-to-live");
			exit(93);
		}
	}
#endif
#ifdef IP_MULTICAST_IF
	if (moptions & MULTICAST_IF) {
		if (setsockopt(s, IPPROTO_IP, IP_MULTICAST_IF,
					(char *)&ifaddr, sizeof(ifaddr)) == -1) {
			perror ("can't set multicast source interface");
			exit(94);
		}
	}
#endif

	if (hs1->to->sin_family == AF_INET && hs2->to->sin_family == AF_INET) {
		(void)printf("BING\t%s (%s) and ",
		    hs1->hostname,
		    inet_ntoa(*(struct in_addr *)&hs1->to->sin_addr.s_addr));
		(void)printf("%s (%s)\n\t%d and %d data bytes\n",
		    hs2->hostname,
		    inet_ntoa(*(struct in_addr *)&hs2->to->sin_addr.s_addr),
		    datalen_small, datalen_big);
	} else
		(void)printf("BING %s and %s:\n\t%d and %d data bytes\n",
		    hs1->hostname, hs2->hostname, datalen_small, datalen_big);

	{
		struct sigaction sa;

		sa.sa_handler = alarm_handler;
#ifdef SA_INTERRUPT
		sa.sa_flags = SA_INTERRUPT;
#else
		sa.sa_flags = 0;
#endif
		sigemptyset(&sa.sa_mask);
		sigaddset(&sa.sa_mask,SIGALRM);

		sigaction(SIGALRM, &sa, NULL);
	}

	(void)signal(SIGINT, finishit_exit);

	bits = (datalen_big - datalen_small) * (8*2);

	for (nloops = 0; !nloops || nloops < npackets; nloops++) {
		long oldsecs = -1;

		ts_init(&hs1->ts_big);
		ts_init(&hs1->ts_small);
		ts_init(&hs2->ts_big);
		ts_init(&hs2->ts_small);

		for (ntrans = 0; !nsamples || ntrans < nsamples ; ntrans++) {
			long secs;
			double min1b, min1s, min2b, min2s;

			ping_and_wait(hs1, datalen_small,
				(char *)recv_packet, recv_packlen,
				interval);
			ping_and_wait(hs1, datalen_big,
				(char *)recv_packet, recv_packlen,
				interval);
			ping_and_wait(hs2, datalen_small,
				(char *)recv_packet, recv_packlen,
				interval);
			ping_and_wait(hs2, datalen_big,
				(char *)recv_packet, recv_packlen,
				interval);

			if (hs1->ts_big.nreceived == 0
			 || hs1->ts_small.nreceived == 0
			 || hs2->ts_big.nreceived == 0
			 || hs2->ts_small.nreceived == 0) {
				continue;
			}

			min1s = dst_min(&(hs1->ts_small.rttstats));
			min1b = dst_min(&(hs1->ts_big.rttstats));
			min2s = dst_min(&(hs2->ts_small.rttstats));
			min2b = dst_min(&(hs2->ts_big.rttstats));
			adapt_rtt(&min1s, &min1b, &min2s, &min2b);
			secs = (min2b - min1b) - (min2s - min1s);

			if ((options & F_NODELTA) || oldsecs != secs) {
				oldsecs = secs;
				if (options & F_WARN)
					warn_rtt(dst_min(&(hs1->ts_small.rttstats)),
						 dst_min(&(hs1->ts_big.rttstats)),
						 dst_min(&(hs2->ts_small.rttstats)),
						 dst_min(&(hs2->ts_big.rttstats)));
				if (secs)
					printf("%d bits in %.3fms: %.0fbps, %.6fms per bit\n",
						bits, (double)secs / 1000,
						(bits / (double)secs) * 1000000,
						(double)secs / 1000 / bits);
				else
					printf("%d bits in 0.000ms\n",
						bits);
			}
		}
		finishit();
		fprintf(stderr,"resetting after %d samples.\n", nsamples);
	}
	return 0;
}
