/*
 * snmp_var_route.c - return a pointer to the named variable.
 *
 *
 */
/***********************************************************
	Copyright 1988, 1989 by Carnegie Mellon University
	Copyright 1989	TGV, Incorporated

		      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of CMU and TGV not be used
in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

CMU AND TGV DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
EVENT SHALL CMU OR TGV BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
******************************************************************/

#define GATEWAY			/* MultiNet is always configured this way! */
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <syslog.h>
#ifndef linux
# include <sys/mbuf.h>
#endif
#include <net/if.h>
#define KERNEL		/* to get routehash and RTHASHSIZ */
#ifndef linux
# include <net/route.h>
#else
# include <malloc.h>
# include <string.h>
# include <stdlib.h>
  /* hopefully avail with kernels previous to 1.3.20: */
# include <linux/route.h>
#endif
#undef	KERNEL
#define rt_unit rt_hash		       /* Reuse this field for device # */
#include <nlist.h>
#ifndef NULL
# define NULL 0
#endif

#define CACHE_TIME (120)	    /* Seconds */

#include "asn1.h"
#include "snmp.h"
#include "snmp_impl.h"
#include "mib.h"
#include "snmp_vars.h"

/* sanity (maybe wrong, but who cares... ;-) */
#ifndef RTF_GATEWAY
# define      RTF_GATEWAY     0x0002
#endif
#ifndef RTF_DYNAMIC
# define      RTF_DYNAMIC     0x0010
#endif


#ifndef  MIN
# define  MIN(a,b)                     (((a) < (b)) ? (a) : (b))
#endif


#ifdef DEBUG
# define dfprintf	if (1) fprintf
#else
# define dfprintf	if (0) fprintf
#endif

/* fwd: */
static int qsort_compare ();
static void Route_Scan_Reload ();


static struct rtentry **rthead=0;
static int rtsize=0, rtalloc=0;

#ifndef linux

static struct nlist nl[] = {
#define N_RTHOST       0
	{ "_rthost" },
#define N_RTNET        1
	{ "_rtnet" },
#define N_RTHASHSIZE	2
	{ "_rthashsize" },
	0,
};
#endif /* ! linux */

u_char *
var_ipRouteEntry(vp, name, length, exact, var_len, access_method)
    register struct variable *vp;   /* IN - pointer to variable entry that points here */
    register oid	*name;	    /* IN/OUT - input name requested, output name found */
    register int	*length;    /* IN/OUT - length of input and output strings */
    int			exact;	    /* IN - TRUE if an exact match was requested. */
    int			*var_len;   /* OUT - length of variable or 0 if function returned. */
    int			*access_method; /* OUT - 1 if function, 0 if char pointer. */
{
#ifndef linux

    /*
     * object identifier is of form:
     * 1.3.6.1.2.1.4.21.1.1.A.B.C.D,  where A.B.C.D is IP address.
     * IPADDR starts at offset 10.
     */
    register int Save_Valid, result, RtIndex;
    static int saveNameLen=0, saveExact=0, saveRtIndex=0;
    static oid saveName[14], Current[14];
    u_char *cp;
    oid *op;

    /*
     *	OPTIMIZATION:
     *
     *	If the name was the same as the last name, with the possible
     *	exception of the [9]th token, then don't read the routing table
     *
     */

    if ((saveNameLen == *length) && (saveExact == exact)) {
	register int temp=name[9];
	name[9] = 0;
	Save_Valid = (compare(name, *length, saveName, saveNameLen) == 0);
	name[9] = temp;
    } else Save_Valid = 0;

    if (Save_Valid) {
	register int temp=name[9];    /* Fix up 'lowest' found entry */
	bcopy((char *) Current, (char *) name, 14 * sizeof(oid));
	name[9] = temp;
	*length = 14;
	RtIndex = saveRtIndex;
    } else {
	/* fill in object part of name for current (less sizeof instance part) */

	bcopy((char *)vp->name, (char *)Current, (int)(vp->namelen - 4) * sizeof(oid));

#if 0
	/*
	 *  Only reload if this is the start of a wildcard
	 */
	if (*length < 14) {
	    Route_Scan_Reload();
	}
#else
        Route_Scan_Reload();
#endif
	for(RtIndex=0; RtIndex < rtsize; RtIndex++) {
	    cp = (u_char *)&(((struct sockaddr_in *) &(rthead[RtIndex]->rt_dst))->sin_addr.s_addr);
	    op = Current + 10;
	    *op++ = *cp++;
	    *op++ = *cp++;
	    *op++ = *cp++;
	    *op++ = *cp++;

	    result = compare(name, *length, Current, 14);
	    if ((exact && (result == 0)) || (!exact && (result < 0)))
		break;
	}
	if (RtIndex >= rtsize) return(NULL);
	/*
	 *  Save in the 'cache'
	 */
	bcopy((char *) name, (char *) saveName, *length * sizeof(oid));
	saveName[9] = '\0';
	saveNameLen = *length;
	saveExact = exact;
	saveRtIndex = RtIndex;
	/*
	 *  Return the name
	 */
	bcopy((char *) Current, (char *) name, 14 * sizeof(oid));
	*length = 14;
    }

#else /* ! linux */

    /*
     * object identifier is of form:
     * 1.3.6.1.2.1.4.21.1.1.A.B.C.D,  where A.B.C.D is IP address.
     * IPADDR starts at offset 10.
     */
    oid			    lowest[14];
    oid			    current[14], *op;
    u_char		    *cp;
    int			    RtIndex, low_rtindex = -1;

    /* fill in object part of name for current (less sizeof instance part) */
    bcopy((char *)vp->name, (char *)current, (int)vp->namelen * sizeof(oid));

    Route_Scan_Reload();

    for (RtIndex = 0; RtIndex < rtsize; RtIndex++) {

	    cp = (u_char *) rthead[RtIndex]->rt_dst.sa_data;

	    op = current + 10;
	    *op++ = *cp++;
	    *op++ = *cp++;
	    *op++ = *cp++;
	    *op++ = *cp++;

	if (exact){
	    if (compare(current, 14, name, *length) == 0){
		bcopy((char *)current, (char *)lowest, 14 * sizeof(oid));
		low_rtindex = RtIndex;
		break;	/* no need to search further */
	    }
	} else {
	    if ((compare(current, 14, name, *length) > 0) &&
		 (low_rtindex < 0 || (compare(current, 14, lowest, 14) < 0))){
		/*
		 * if new one is greater than input and closer to input than
		 * previous lowest, save this one as the "next" one.
		 */
		low_rtindex = RtIndex;
		bcopy((char *)current, (char *)lowest, 14 * sizeof(oid));
	    }
	}
    }

    if ((RtIndex = low_rtindex) < 0) 
      return NULL;

    bcopy((char *)lowest, (char *)name, 14 * sizeof(oid));
    *length = 14;

#endif /* ! linux */


    *access_method = 0;
    *var_len = sizeof(long_return);
    switch(vp->magic){
	case IPROUTEDEST:
#ifndef linux
	    return(u_char *) &((struct sockaddr_in *) 
			       &rthead[RtIndex]->rt_dst)->sin_addr.s_addr;
#else
	    return(u_char *) rthead[RtIndex]->rt_dst.sa_data;
#endif
	case IPROUTEIFINDEX:
#ifdef linux
	    long_return = (u_long)rthead[RtIndex]->rt_unit;
#else
	    long_return = rthead[RtIndex]->rt_unit;
#endif
	    return (u_char *)&long_return;
	case IPROUTEMETRIC1:
	    long_return = (rthead[RtIndex]->rt_flags & RTF_GATEWAY) ? 1 : 0;
	    return (u_char *)&long_return;
	case IPROUTEMETRIC2:
	    long_return = -1;
	    return (u_char *)&long_return;
	case IPROUTEMETRIC3:
	    long_return = -1;
	    return (u_char *)&long_return;
	case IPROUTEMETRIC4:
	    long_return = -1;
	    return (u_char *)&long_return;
#ifdef linux
	case IPROUTEMETRIC5:
	    long_return = -1;
	    return (u_char *)&long_return;
#endif
	case IPROUTENEXTHOP:
#ifndef linux
	    return(u_char *) &((struct sockaddr_in *)
			       &rthead[RtIndex]->rt_gateway)->sin_addr.s_addr;
#else
	    return(u_char *) rthead[RtIndex]->rt_gateway.sa_data;
#endif
	case IPROUTETYPE:
	    long_return = (rthead[RtIndex]->rt_flags & RTF_GATEWAY) ? 4 : 3;
	    return (u_char *)&long_return;
	case IPROUTEPROTO:
	    long_return = (rthead[RtIndex]->rt_flags & RTF_DYNAMIC) ? 4 : 2;
	    return (u_char *)&long_return;
	case IPROUTEAGE:
	    long_return = 0;
	    return (u_char *)&long_return;
#ifdef linux
	case IPROUTEMASK:
	    return (u_char *) rthead[RtIndex]->rt_genmask.sa_data;
	case IPROUTEINFO:
	    { static oid rtinfo [16] = {
 		1, 3, 6, 1, 2, 1, 4, 21, 1, 13, 1, 2, 3, 4, 0, 0 };

	      /** XXX: broken - fix me **/
	      bcopy((char *) name, (char *) &rtinfo, 14 * sizeof(oid));

	      *var_len = sizeof (rtinfo);
	      return (u_char *) &rtinfo;
	    }
#endif
	default:
	    ERROR("var_ipRouteEntry(): unknown magic");
	    dfprintf (stderr, "var_ipRouteEntry(): unknown magic %d\n", 
		     vp->magic);
   }
   return NULL;
}


#ifndef linux
init_routes(){

    nlist("/vmunix",nl);
}
#endif

#if defined(linux)

static void
Route_Scan_Reload()
{
	FILE *in;
	char line [256];
	struct rtentry *rt;
	char name[16], temp[16];
	static int Time_Of_Last_Reload=0;
	struct timeval now;

	/* allow 20 seconds in cache: */
	gettimeofday(&now, (struct timezone *)0);
	if (Time_Of_Last_Reload + 20 > now.tv_sec)
	    return;
	Time_Of_Last_Reload =  now.tv_sec;

	/*
	 *  Makes sure we have SOME space allocated for new routing entries
	 */
	if (! rthead) {
	    rthead = (struct rtentry **) calloc(100, sizeof(struct rtentry *));
	    if (! rthead) {
		ERROR("malloc");
		return;
	    }
	    rtalloc = 100;
	}

	/*
	 * fetch routes from the proc file-system:
	 */

	rtsize = 0;

	if (! (in = fopen ("/proc/net/route", "r")))
	  {
	    fprintf (stderr, "cannot open /proc/net/route - burps\n");
	    return;
	  }

	while (fgets (line, 256, in))
	  {
	    struct rtentry rtent;
	    char rtent_name [32];
	    int refcnt, flags, metric;
	    unsigned use;
	    
	    rt = &rtent;
	    bzero ((char *) rt, sizeof(*rt));
	    rt->rt_dev = rtent_name;

	    /*
	     * as with 1.99.14:
	     * Iface Dest GW Flags RefCnt Use Metric Mask MTU Win IRTT
	     * eth0 0A0A0A0A 00000000 05 0 0 0 FFFFFFFF 1500 0 0 
	     */
	    if (8 != sscanf (line, "%s %lx %lx %x %u %d %d %lx\n",
			     rt->rt_dev,
			     (unsigned long *) rt->rt_dst.sa_data,
			     (unsigned long *) rt->rt_gateway.sa_data,
/* XXX: fix type of the args */
			     &flags, &refcnt, &use, &metric,
			     (unsigned long *) &rt->rt_genmask.sa_data))
	      continue;
	    
	    strcpy (name, rt->rt_dev);
	    /* linux says ``lo'', but the interface is stored as ``lo0'': */
	    if (! strcmp (name, "lo"))
	      strcat (name, "0");
	    
	    name[15] = '\0';

	    rt->rt_flags = flags, rt->rt_refcnt = refcnt;
	    rt->rt_use = use, rt->rt_metric = metric;

	    Interface_Scan_Init();
	    while (Interface_Scan_Next((int *)&rt->rt_unit, temp, 0, 0) != 0)
		if (strcmp(name, temp) == 0) break;

	    /*
	     *	Allocate a block to hold it and add it to the database
	     */
	    if (rtsize >= rtalloc) {
	      rthead = (struct rtentry **) realloc((char *)rthead, 
				   2 * rtalloc * sizeof(struct rtentry *));
	      bzero((char *) &rthead[rtalloc], rtalloc 
		    		   * sizeof(struct rtentry *));
	      rtalloc *= 2;
	    }
	    if (! rthead[rtsize])
	      rthead[rtsize] = (struct rtentry *) 
		malloc(sizeof(struct rtentry));
	    /*
	     *	Add this to the database
	     */
	    bcopy((char *)rt, (char *)rthead[rtsize], sizeof(struct rtentry));
	    rtsize++;
	  }

	fclose (in);

	/*
	 *  Sort it!
	 */
	qsort((char *)rthead,rtsize,sizeof(rthead[0]),qsort_compare);
}

#else /* ! linux */

static Route_Scan_Reload()
{
	struct mbuf **routehash, mb;
	register struct mbuf *m;
	struct ifnet ifnet;
	struct rtentry *rt;
	int i, table, qsort_compare();
	register char *cp;
	char name[16], temp[16];
	static int Time_Of_Last_Reload=0;
	struct timeval now;
	int hashsize;

	gettimeofday(&now, (struct timezone *)0);
	if (Time_Of_Last_Reload+CACHE_TIME > now.tv_sec)
	    return;
	Time_Of_Last_Reload =  now.tv_sec;

	/*
	 *  Makes sure we have SOME space allocated for new routing entries
	 */
	if (!rthead) {
	    rthead = (struct rtentry **) malloc(100 * sizeof(struct rtentry *));
	    bzero((char *)rthead, 100 * sizeof(struct rtentry *));
	    rtalloc = 100;
	}

	for (table=N_RTHOST; table<=N_RTNET; table++) {
	    klseek(nl[N_RTHASHSIZE].n_value);
	    klread((char *)&hashsize, sizeof(hashsize));
	    routehash = (struct mbuf **)malloc(hashsize * sizeof(struct mbuf *));
	    klseek(nl[table].n_value);
	    klread((char *)routehash, hashsize * sizeof(struct mbuf *));
	    for (i = 0; i < hashsize; i++) {
		if (routehash[i] == 0)
			continue;
		m = routehash[i];
		while (m) {
		    /*
		     *	Dig the route out of the kernel...
		     */
		    klseek(m);
		    klread(&mb, sizeof (mb));
		    m = mb.m_next;
		    rt = mtod(&mb, struct rtentry *);
		    if (rt->rt_ifp != 0) {
			klseek(rt->rt_ifp);
			klread((char *)&ifnet, sizeof (ifnet));
			klseek((int)ifnet.if_name);
			klread(name, 16);
			name[15] = '\0';
			cp = (char *) index(name, '\0');
			*cp++ = ifnet.if_unit + '0';
			*cp = '\0';
/*			if (strcmp(name,"lo0") == 0) continue; */
			Interface_Scan_Init();
			while (Interface_Scan_Next(&rt->rt_unit, temp, 0, 0) != 0) {
			    if (strcmp(name, temp) == 0) break;
			}
		    }
		    /*
		     *	Allocate a block to hold it and add it to the database
		     */
		    if (rtsize >= rtalloc) {
			rthead = (struct rtentry **) realloc((char *)rthead, 2 * rtalloc * sizeof(struct rtentry *));
			bzero((char *) &rthead[rtalloc], rtalloc * sizeof(struct rtentry *));
			rtalloc *= 2;
		    }
		    if (!rthead[rtsize])
			rthead[rtsize] = (struct rtentry *) malloc(sizeof(struct rtentry));
		    /*
		     *	Add this to the database
		     */
		    bcopy((char *)rt, (char *)rthead[rtsize], sizeof(struct rtentry));
		    rtsize++;
		}
	    }
	}
	/*
	 *  Sort it!
	 */
	qsort((char *)rthead,rtsize,sizeof(rthead[0]),qsort_compare);
}

#endif /* ! linux */


/*
 *	Create a host table
 */
static int qsort_compare(r1,r2)
struct rtentry **r1, **r2;
{
#ifndef linux
	register u_long dst1 = ntohl(((struct sockaddr_in *) 
				      &((*r1)->rt_dst))->sin_addr.s_addr);
	register u_long dst2 = ntohl(((struct sockaddr_in *) 
				      &((*r2)->rt_dst))->sin_addr.s_addr);
#else
	register u_long dst1 = ntohl(((struct sockaddr_in *) &((*r1)->rt_dst))->sin_addr.s_addr);
	register u_long dst2 = ntohl(((struct sockaddr_in *) &((*r2)->rt_dst))->sin_addr.s_addr);
#endif

	/*
	 *	Do the comparison
	 */
	if (dst1 == dst2) return(0);
	if (dst1 > dst2) return(1);
	return(-1);
}
