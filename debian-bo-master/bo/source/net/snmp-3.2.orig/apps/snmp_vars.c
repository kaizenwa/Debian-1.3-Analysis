/*
 * snmp_vars.c - return a pointer to the named variable.
 *
 *
 */
/***********************************************************
	Copyright 1988, 1989, 1990 by Carnegie Mellon University
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

/*
 * additions, fixes and enhancements for Linux by 
 * Erik Schoenfelder <schoenfr@gaertner.de> and
 * Juergen Schoenwaelder <schoenw@cs.utwente.nl>
 * June 1996
 */

/*
 * Small Linux specific patch by Patrick Weemeeuw
 * (patrick.weemeeuw@kulnet.kuleuven.ac.be)
 * to keep track of HrProcessorLoad more accurately.
 * -- 8 July 1996
 */

#define USE_NAME_AS_DESCRIPTION /*"se0" instead of text */
#define GATEWAY			/* MultiNet is always configured this way! */
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
/* #include <sys/time.h> */
#ifdef linux
# include <time.h>
#endif
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>
#ifndef linux
# include <sys/proc.h>
#else
# include <malloc.h>
# include <linux/tasks.h>
# include <utmp.h>
# include <dirent.h>
# include <sys/stat.h>
# include <sys/vfs.h>
# include <unistd.h>
# include <ctype.h>
#endif
#include <sys/types.h>
#ifndef linux
#include <machine/pte.h>
#include <sys/vm.h>
#endif
#include <netinet/in.h>
#include <syslog.h>
#include <sys/ioctl.h>
#include <net/if.h>
#ifndef linux
#include <net/route.h>
#include <netinet/in_pcb.h>
#include <netinet/if_ether.h>
#include <netinet/in_systm.h>
#else /* linux */
#define sunV3
#include <stdlib.h>
#endif /* linux */

#ifndef sunV3
# ifndef linux
#  include <netinet/in_var.h>
# endif
#endif
#include <netinet/ip.h>
#ifndef linux
# include <netinet/ip_var.h>
#endif
#include <netinet/tcp.h>
#ifndef linux
# include <netinet/tcp_timer.h>
# include <netinet/tcp_var.h>
# include <netinet/tcp_fsm.h>
#endif
#ifndef linux
#include <netinet/udp.h>
#include <netinet/udp_var.h>
#include <netinet/ip_icmp.h>
#include <netinet/icmp_var.h>
#endif
#include <nlist.h>
#ifndef linux
#include <sys/protosw.h>
#endif

#ifdef linux

#include "hr_processor_load.h"

#include <pwd.h>

#include <sys/time.h>
/*
 * this struct ifnet is cloned from the generic type and somewhat modified.
 * it will not work for other un*x'es...
 */

struct ifnet {
	char	*if_name;		/* name, e.g. ``en'' or ``lo'' */
	short	if_unit;		/* sub-unit for lower level driver */
	short	if_mtu;			/* maximum transmission unit */
	short	if_flags;		/* up/down, broadcast, etc. */
	int	if_metric;		/* routing metric (external only) */
	char    if_hwaddr [6];		/* ethernet address */
	int	if_type;		/* interface type: 1=generic,
					   28=slip, ether=6, loopback=24 */
	int	if_speed;		/* interface speed: in bits/sec */

	struct sockaddr if_addr;	/* interface's address */
	struct sockaddr ifu_broadaddr;	/* broadcast address */
	struct sockaddr ia_subnetmask; 	/* interface's mask */

	struct	ifqueue {
		int	ifq_len;
		int	ifq_drops;
	} if_snd;			/* output queue */
	int	if_ipackets;		/* packets received on interface */
	int	if_ierrors;		/* input errors on interface */
	int	if_opackets;		/* packets sent on interface */
	int	if_oerrors;		/* output errors on interface */
	int	if_collisions;		/* collisions on csma interfaces */
/* end statistics */
	struct	ifnet *if_next;
};


/*
 * arp struct to pass flags, hw-addr and ip-addr in bsd manner:
 */
struct arptab {
  int at_flags;
  char at_enaddr [6];
  struct in_addr at_iaddr;
};

/* in case its missing: */
#ifndef ATF_PERM
# define ATF_PERM	0x04
#endif
#ifndef ATF_COM
# define ATF_COM	0x02
#endif

/*
 * networking statistics:
 */

/*
 * (the structs are from /usr/src/linux/net/inet/snmp.h)
 */

struct ip_mib
{
 	unsigned long	IpForwarding;
 	unsigned long	IpDefaultTTL;
 	unsigned long	IpInReceives;
 	unsigned long	IpInHdrErrors;
 	unsigned long	IpInAddrErrors;
 	unsigned long	IpForwDatagrams;
 	unsigned long	IpInUnknownProtos;
 	unsigned long	IpInDiscards;
 	unsigned long	IpInDelivers;
 	unsigned long	IpOutRequests;
 	unsigned long	IpOutDiscards;
 	unsigned long	IpOutNoRoutes;
 	unsigned long	IpReasmTimeout;
 	unsigned long	IpReasmReqds;
 	unsigned long	IpReasmOKs;
 	unsigned long	IpReasmFails;
 	unsigned long	IpFragOKs;
 	unsigned long	IpFragFails;
 	unsigned long	IpFragCreates;
};

struct icmp_mib
{
 	unsigned long	IcmpInMsgs;
 	unsigned long	IcmpInErrors;
  	unsigned long	IcmpInDestUnreachs;
 	unsigned long	IcmpInTimeExcds;
 	unsigned long	IcmpInParmProbs;
 	unsigned long	IcmpInSrcQuenchs;
 	unsigned long	IcmpInRedirects;
 	unsigned long	IcmpInEchos;
 	unsigned long	IcmpInEchoReps;
 	unsigned long	IcmpInTimestamps;
 	unsigned long	IcmpInTimestampReps;
 	unsigned long	IcmpInAddrMasks;
 	unsigned long	IcmpInAddrMaskReps;
 	unsigned long	IcmpOutMsgs;
 	unsigned long	IcmpOutErrors;
 	unsigned long	IcmpOutDestUnreachs;
 	unsigned long	IcmpOutTimeExcds;
 	unsigned long	IcmpOutParmProbs;
 	unsigned long	IcmpOutSrcQuenchs;
 	unsigned long	IcmpOutRedirects;
 	unsigned long	IcmpOutEchos;
 	unsigned long	IcmpOutEchoReps;
 	unsigned long	IcmpOutTimestamps;
 	unsigned long	IcmpOutTimestampReps;
 	unsigned long	IcmpOutAddrMasks;
 	unsigned long	IcmpOutAddrMaskReps;
};

struct tcp_mib
{
 	unsigned long	TcpRtoAlgorithm;
 	unsigned long	TcpRtoMin;
 	unsigned long	TcpRtoMax;
 	unsigned long	TcpMaxConn;
 	unsigned long	TcpActiveOpens;
 	unsigned long	TcpPassiveOpens;
 	unsigned long	TcpAttemptFails;
 	unsigned long	TcpEstabResets;
 	unsigned long	TcpCurrEstab;
 	unsigned long	TcpInSegs;
 	unsigned long	TcpOutSegs;
 	unsigned long	TcpRetransSegs;
};

struct udp_mib
{
 	unsigned long	UdpInDatagrams;
 	unsigned long	UdpNoPorts;
 	unsigned long	UdpInErrors;
 	unsigned long	UdpOutDatagrams;
};


/* ugly mapping of `struct tcpstat' -> `struct tcp_mib' (but what the heck): */
#define tcpstat tcp_mib
#define tcps_connattempt TcpActiveOpens
#define tcps_accepts TcpPassiveOpens
#define tcps_conndrops TcpAttemptFails
#define tcps_drops TcpEstabResets
#define tcps_rcvtotal TcpInSegs
#define tcps_sndtotal TcpOutSegs
#define tcps_sndrexmitpack TcpRetransSegs


struct inpcb {
        struct  inpcb *inp_next;        /* pointers to other pcb's */
        struct  in_addr inp_faddr;      /* foreign host table entry */
        u_short inp_fport;              /* foreign port */
        struct  in_addr inp_laddr;      /* local host table entry */
        u_short inp_lport;              /* local port */
	int     inp_state;
	int     uid;			/* owner of the connection */
};

/* counters for the snmp group: */

#include "snmp_groupvars.h"

int snmp_inpkts = 0;
int snmp_outpkts = 0;
int snmp_inbadversions = 0;
int snmp_inbadcommunitynames = 0;
/* int snmp_inbadcommunityuses = 0;  */
int snmp_inasnparseerrors = 0;
int snmp_intoobigs = 0;
/* int snmp_innosuchnames = 0; */
int snmp_inbadvalues = 0;
int snmp_inreadonlys = 0;
int snmp_ingenerrs = 0;
int snmp_intotalreqvars = 0;
/* int snmp_intotalsetvars = 0;  */
int snmp_ingetrequests = 0;
int snmp_ingetnexts = 0;
int snmp_insetrequests = 0;
/* int snmp_ingetresponses = 0;  */
/* int snmp_intraps = 0;  */
/* int snmp_outtoobigs = 0;  */
int snmp_outnosuchnames = 0;
/* int snmp_outbadvalues = 0;  */
/* int snmp_outgenerrs = 0;  */
/* int snmp_outgetrequests = 0;  */
/* int snmp_outgetnexts = 0;  */
/* int snmp_outsetrequests = 0;  */
int snmp_outgetresponses = 0;
/* int snmp_outtraps = 0;  */

/* agents startup time stamp: */
static unsigned long uptime_stamp;

#endif /* linux */

int snmp_enableauthentraps = 2;		/* default: 2 == disabled */


#ifndef NULL
#define NULL 0
#endif
#ifndef  MIN
#define  MIN(a,b)                     (((a) < (b)) ? (a) : (b)) 
#endif

#include "asn1.h"
#include "snmp.h"
#include "snmp_impl.h"
#include "mib.h"
#include "snmp_vars.h"
#include "snmp_config.h"

#define PROCESSSLOTINDEX  0
#define PROCESSID         4
#define PROCESSCOMMAND    8
 
#ifdef vax11c
#define ioctl socket_ioctl
#define perror socket_perror
#endif vax11c

extern  int swap, mem;
extern char *Lookup_Device_Annotation();

/* fwd: */
static void ARP_Scan_Init ();
static int ARP_Scan_Next ();
extern void Interface_Scan_Init ();
extern int Interface_Scan_Next ();
static int Interface_Scan_Get_Count ();
static int Interface_Scan_By_Index ();
static int Interface_Get_Ether_By_Index ();
#ifndef linux
static int TCP_Count_Connections ();
#endif
static void TCP_Scan_Init ();
static int TCP_Scan_Next ();
static u_char * var_ntomEntry ();
#ifdef linux
static void UDP_Scan_Init ();
static int UDP_Scan_Next ();
#endif
static int compare_tree ();

#ifndef linux

#define  KNLookup(nl_which, buf, s)   (klookup((int) nl[nl_which].n_value, buf, s))


static struct nlist nl[] = {

#define N_IPSTAT	0
	{ "_ipstat"},
#define N_IPFORWARDING	1
#ifndef sparc
	{ "_ipforwarding" },
#else
	{ "_ip_forwarding" },
#endif
#define N_TCP_TTL	2
	{ "_tcp_ttl"},
#define N_UDPSTAT	3
	{ "_udpstat" },
#define N_IN_INTERFACES 4
	{ "_in_interfaces" },
#define N_ICMPSTAT	5
	{ "_icmpstat" },
#define N_IFNET		6
	{ "_ifnet" },
#define N_TCPSTAT	7
	{ "_tcpstat" },
#define N_TCB		8
	{ "_tcb" },
#define N_ARPTAB_SIZE	9
	{ "_arptab_size" },
#define N_ARPTAB        10
	{ "_arptab" },
#define N_IN_IFADDR     11
	{ "_in_ifaddr" },
#define N_BOOTTIME	12
	{ "_boottime" },
#define N_PROC		13
	{ "_proc" },
#define N_NPROC		14
	{ "_nproc" },
#define N_DMMIN		15
	{ "_dmmin" },
#define N_DMMAX		16
	{ "_dmmax" },
#define N_NSWAP		17
	{ "_nswap" },
#define N_USRPTMAP	18
 	{ "_Usrptmap" },
#define N_USRPT		19
	{ "_usrpt" },
#ifdef ibm032
#define N_USERSIZE	20
	{ "_userSIZE" },
#endif
	0,
};
#endif /* ! linux */


/*
 *	Each variable name is placed in the variable table, without the
 * terminating substring that determines the instance of the variable.  When
 * a string is found that is lexicographicly preceded by the input string,
 * the function for that entry is called to find the method of access of the
 * instance of the named variable.  If that variable is not found, NULL is
 * returned, and the search through the table continues (it will probably
 * stop at the next entry).  If it is found, the function returns a character
 * pointer and a length or a function pointer.  The former is the address
 * of the operand, the latter is a write routine for the variable.
 *
 * u_char *
 * findVar(name, length, exact, var_len, write_method)
 * oid	    *name;	    IN/OUT - input name requested, output name found
 * int	    length;	    IN/OUT - number of sub-ids in the in and out oid's
 * int	    exact;	    IN - TRUE if an exact match was requested.
 * int	    len;	    OUT - length of variable or 0 if function returned.
 * int	    write_method;   OUT - pointer to function to set variable,
 *                                otherwise 0
 *
 *     The writeVar function is returned to handle row addition or complex
 * writes that require boundary checking or executing an action.
 * This routine will be called three times for each varbind in the packet.
 * The first time for each varbind, action is set to RESERVE1.  The type
 * and value should be checked during this pass.  If any other variables
 * in the MIB depend on this variable, this variable will be stored away
 * (but *not* committed!) in a place where it can be found by a call to
 * writeVar for a dependent variable, even in the same PDU.  During
 * the second pass, action is set to RESERVE2.  If this variable is dependent
 * on any other variables, it will check them now.  It must check to see
 * if any non-committed values have been stored for variables in the same
 * PDU that it depends on.  Sometimes resources will need to be reserved
 * in the first two passes to guarantee that the operation can proceed
 * during the third pass.  During the third pass, if there were no errors
 * in the first two passes, writeVar is called for every varbind with action
 * set to COMMIT.  It is now that the values should be written.  If there
 * were errors during the first two passes, writeVar is called in the third
 * pass once for each varbind, with the action set to FREE.  An opportunity
 * is thus provided to free those resources reserved in the first two passes.
 * 
 * writeVar(action, var_val, var_val_type, var_val_len, statP, name, name_len)
 * int	    action;	    IN - RESERVE1, RESERVE2, COMMIT, or FREE
 * u_char   *var_val;	    IN - input or output buffer space
 * u_char   var_val_type;   IN - type of input buffer
 * int	    var_val_len;    IN - input and output buffer len
 * u_char   *statP;	    IN - pointer to local statistic
 * oid      *name           IN - pointer to name requested
 * int      name_len        IN - number of sub-ids in the name
 */

long		long_return;
#if !defined(ibm032) && !defined(linux)
u_char		return_buf[CLSIZE*NBPG];  
#else
u_char		return_buf[256]; /* nee 64 */
#define CLSIZE	256	/* XXX: ??? */
#endif

#ifdef linux
/*
 * run through the interface-table and return index of the ``eth0'' 
 * interface or 0 if not found.
 */
static int
get_ether_iface_index ()
{ 
  static int ether_iface_index = 0;

  if (ether_iface_index <= 0)
    {
      char tmp [32];
      Interface_Scan_Init();
      while (Interface_Scan_Next (&ether_iface_index, tmp, 0, 0) != 0)
	if (! strcmp (tmp, "eth0")) break;
    }
  return ether_iface_index;
}

#if 0
static int imin (a, b)
int a, b;
{
  return a < b ? a : b;
}
#endif

#endif /* linux */


#ifdef linux
/* overridden by /proc/version: */
char version_descr[256] = "Linux";
# else
# ifdef sun
char version_descr[256] = "SunOS";
#  else
char version_descr[256] = "Unix 4.3BSD";
# endif
#endif

void
init_snmp()
{
#ifndef linux
	nlist("/vmunix",nl);
	init_kmem("/dev/kmem");
	init_routes();
#else /* linux */
  FILE *in;
  char tmp [256];

  if ((in = fopen ("/proc/version", "r")))
    {
	if (fgets (tmp, 256, in) > 0)
	  { 
	      tmp [strlen (tmp) - 1] = 0;
	      strcpy (version_descr, tmp);
	  }
	fclose (in);
    }
  else
    {
	strcpy (version_descr, "Unknown");
	fprintf (stderr, 
   "snmpd: cannot open /proc/version - please make sure /proc is mounted.\n");
    }

    /* init startup time: */
    { struct timeval tv;
      gettimeofday (&tv, (struct timezone *) 0);
      uptime_stamp = tv.tv_sec;
    }

#endif /* linux */

    { /* use real hostname for sysname, if not configured: */
	extern char sysName [256];
	char tmp [256];

	if (! *sysName)
	  {
	      if (gethostname (tmp, 256) == 0)
		strcpy (sysName, tmp);
	      else
		strcpy (sysName, "Unknown");
	  }
    }

    { /* use authentrap from config-file, if found: */
	extern int conf_authentraps;
	if (conf_authentraps > 0)
	  snmp_enableauthentraps = conf_authentraps;      
    }
}

#define CMUMIB 		1, 3, 6, 1, 4, 1, 3
#define CMUUNIXMIB  	CMUMIB, 2, 2

#define SNMPMODULES 		1, 3, 6, 1, 6, 3

#define SNMPSTATS		SNMPMODULES, 1, 1, 1
#define SNMPV1STATS		SNMPMODULES, 1, 1, 2
#define SNMPTRAP		SNMPMODULES, 1, 1, 4
#define SNMPSET			SNMPMODULES, 1, 1, 6
#define USECMIBOBJ		SNMPMODULES, 6, 1
#define USECAGENT		USECMIBOBJ, 1
#define USECSTATS		USECMIBOBJ, 2

#define HOST                    RMONMIB, 4
#define HOSTCONTROL             HOST, 1, 1                      /* hostControlEntry */
#define HOSTTAB                 HOST, 2, 1                      /* hostEntry */
#define HOSTTIMETAB             HOST, 3, 1                      /* hostTimeEntry */
#define HOSTTOPN                RMONMIB, 5
#define HOSTTOPNCONTROL HOSTTOPN, 1, 1          /* hostTopNControlEntry */
#define HOSTTOPNTAB             HOSTTOPN, 2, 1          /* hostTopNEntry */
#define HOSTTIMETABADDRESS                                      1
#define HOSTTIMETABCREATIONORDER                        2
#define HOSTTIMETABINDEX                                        3
#define HOSTTIMETABINPKTS                                       4
#define HOSTTIMETABOUTPKTS                                      5
#define HOSTTIMETABINOCTETS                                     6
#define HOSTTIMETABOUTOCTETS                            7
#define HOSTTIMETABOUTERRORS                            8
#define HOSTTIMETABOUTBCASTPKTS                         9
#define HOSTTIMETABOUTMCASTPKTS                         10

/* various OIDs that are needed throughout the agent */

oid sysUpTimeOid[] = {1,3,6,1,2,1,1,3,0};
int sysUpTimeOidLen = sizeof(sysUpTimeOid)/sizeof(oid);


/*
 * The subtree structure contains a subtree prefix which applies to
 * all variables in the associated variable list.
 * No subtree may be a subtree of another subtree in this list.  i.e.:
 * 1.2
 * 1.2.0
 */
struct subtree {
    oid			name[16];	/* objid prefix of subtree */
    u_char 		namelen;	/* number of subid's in name above */
    struct variable	*variables;   /* pointer to variables array */
    int			variables_len;	/* number of entries in above array */
    int			variables_width; /* sizeof each variable entry */
};

/*
 * This is a new variable structure that doesn't have as much memory
 * tied up in the object identifier.  It's elements have also been re-arranged
 * so that the name field can be variable length.  Any number of these
 * structures can be created with lengths tailor made to a particular
 * application.  The first 5 elements of the structure must remain constant.
 */
struct variable2 {
    u_char          magic;          /* passed to function as a hint */
    char            type;           /* type of variable */
    u_short         acl;            /* access control list for variable */
    u_char          *(*findVar)();  /* function that finds variable */
    u_char          namelen;        /* length of name below */
    oid             name[2];       /* object identifier of variable */
};

struct variable4 {
    u_char          magic;          /* passed to function as a hint */
    char            type;           /* type of variable */
    u_short         acl;            /* access control list for variable */
    u_char          *(*findVar)();  /* function that finds variable */
    u_char          namelen;        /* length of name below */
    oid             name[4];       /* object identifier of variable */
};

struct variable7 {
    u_char          magic;          /* passed to function as a hint */
    char            type;           /* type of variable */
    u_short         acl;            /* access control list for variable */
    u_char          *(*findVar)();  /* function that finds variable */
    u_char          namelen;        /* length of name below */
    oid             name[7];       /* object identifier of variable */
};

struct variable13 {
    u_char          magic;          /* passed to function as a hint */
    char            type;           /* type of variable */
    u_short         acl;            /* access control list for variable */
    u_char          *(*findVar)();  /* function that finds variable */
    u_char          namelen;        /* length of name below */
    oid             name[13];       /* object identifier of variable */
};

/*
 * ##############################################################
 * IMPORTANT NOTE:
 * ##############################################################
 *
 * The format of the acl word in these entries has changed.  It is still
 * 2 bits per community, offset from the right by the index of the community.
 * The leftmost two bits denotes read access, and the rightmost denotes
 * write access.
 * The change is that the rightmost two bits are now reserved for the object's
 * max-access.  This is the minimum of what makes "protocol sense" for the
 * object and whether set support was implemented for that object.
 * These two bits will not map to any community.  The first community
 * entry will map to the 3rd and 4th bits.
 */

#define MTRBIGNUMBER	1
#define MTRNSAPADDRESS	2
#define MTRBITSTRING	3

struct variable2 demo_variables[] = {
    {MTRBIGNUMBER, COUNTER64, RONLY, var_demo, 1, {1}},
    {MTRNSAPADDRESS, NSAP, RONLY, var_demo, 1, {2}},
};

struct variable4 interface_variables[] = {
    {IFNUMBER, INTEGER, RONLY, var_system, 1, {1}},
    {IFINDEX, INTEGER, RONLY, var_ifEntry, 3, {2, 1, 1}},
    {IFDESCR, STRING, RONLY, var_ifEntry, 3, {2, 1, 2}},
    {IFTYPE, INTEGER, RONLY, var_ifEntry, 3, {2, 1, 3}},
    {IFMTU, INTEGER, RONLY, var_ifEntry, 3, {2, 1, 4}},
    {IFSPEED, GAUGE, RONLY, var_ifEntry, 3, {2, 1, 5}},
    {IFPHYSADDRESS, STRING, RONLY, var_ifEntry, 3, {2, 1, 6}},
    {IFADMINSTATUS, INTEGER, RWRITE, var_ifEntry, 3, {2, 1, 7}},
    {IFOPERSTATUS, INTEGER, RONLY, var_ifEntry, 3, {2, 1, 8}},
    {IFLASTCHANGE, TIMETICKS, RONLY, var_ifEntry, 3, {2, 1, 9}},
    {IFINOCTETS, COUNTER, RONLY, var_ifEntry, 3, {2, 1, 10}},
    {IFINUCASTPKTS, COUNTER, RONLY, var_ifEntry, 3, {2, 1, 11}},
    {IFINNUCASTPKTS, COUNTER, RONLY, var_ifEntry, 3, {2, 1, 12}},
    {IFINDISCARDS, COUNTER, RONLY, var_ifEntry, 3, {2, 1, 13}},
    {IFINERRORS, COUNTER, RONLY, var_ifEntry, 3, {2, 1, 14}},
    {IFINUNKNOWNPROTOS, COUNTER, RONLY, var_ifEntry, 3, {2, 1, 15}},
    {IFOUTOCTETS, COUNTER, RONLY, var_ifEntry, 3, {2, 1, 16}},
    {IFOUTUCASTPKTS, COUNTER, RONLY, var_ifEntry, 3, {2, 1, 17}},
    {IFOUTNUCASTPKTS, COUNTER, RONLY, var_ifEntry, 3, {2, 1, 18}},
    {IFOUTDISCARDS, COUNTER, RONLY, var_ifEntry, 3, {2, 1, 19}},
    {IFOUTERRORS, COUNTER, RONLY, var_ifEntry, 3, {2, 1, 20}},
    {IFOUTQLEN, GAUGE, RONLY, var_ifEntry, 3, {2, 1, 21}}
#ifdef linux
    , {IFSPECIFIC, OBJID, RONLY, var_ifEntry, 3, {2, 1, 22}}
#endif
};

struct variable2 system_variables[] = {
    {VERSION_DESCR, STRING, RONLY, var_system, 1, {1}},
    {VERSION_ID, OBJID, RONLY, var_system, 1, {2}},
    {UPTIME, TIMETICKS, RONLY, var_system, 1, {3}},
    {SYSCONTACT, STRING, RWRITE, var_system, 1, {4}},
    {SYSNAME, STRING, RWRITE, var_system, 1, {5}},
    {SYSLOCATION, STRING, RWRITE, var_system, 1, {6}},
    {SYSSERVICES, INTEGER, RONLY, var_system, 1, {7}},
    {SYSORLASTCHANGE, TIMETICKS, RONLY, var_system, 1, {8}}
};

struct variable2 or_variables[] = {
    {2, OBJID, RONLY, var_orEntry, 1, {2}},
    {3, STRING, RONLY, var_orEntry, 1, {3}},
    {4, TIMETICKS, RONLY, var_orEntry, 1, {4}},
};

struct variable2 at_variables[] = {
    {ATIFINDEX, INTEGER, RONLY, var_atEntry, 1, {1}},
    {ATPHYSADDRESS, STRING, RONLY, var_atEntry, 1, {2}},
    {ATNETADDRESS, IPADDRESS, RONLY, var_atEntry, 1, {3}}
};

struct variable4 ip_variables[] = {
    {IPFORWARDING, INTEGER, RONLY, var_ip, 1, {1 }},
    {IPDEFAULTTTL, INTEGER, RONLY, var_ip, 1, {2 }},
#if !defined(sunV3) || defined(linux)
    {IPINRECEIVES, COUNTER, RONLY, var_ip, 1, {3 }},
#endif
    {IPINHDRERRORS, COUNTER, RONLY, var_ip, 1, {4 }},
#if !defined(sunV3) || defined(linux)
    {IPINADDRERRORS, COUNTER, RONLY, var_ip, 1, {5 }},
    {IPFORWDATAGRAMS, COUNTER, RONLY, var_ip, 1, {6 }},
#endif
    {IPINUNKNOWNPROTOS, COUNTER, RONLY, var_ip, 1, {7 }},
#if !defined(sunV3) || defined(linux)
    {IPINDISCARDS, COUNTER, RONLY, var_ip, 1, {8 }},
    {IPINDELIVERS, COUNTER, RONLY, var_ip, 1, {9 }},
#endif
    {IPOUTREQUESTS, COUNTER, RONLY, var_ip, 1, {10 }},
    {IPOUTDISCARDS, COUNTER, RONLY, var_ip, 1, {11 }},
    {IPOUTNOROUTES, COUNTER, RONLY, var_ip, 1, {12 }},
    {IPREASMTIMEOUT, INTEGER, RONLY, var_ip, 1, {13 }},
#if !defined(sunV3) || defined(linux)
    {IPREASMREQDS, COUNTER, RONLY, var_ip, 1, {14 }},
    {IPREASMOKS, COUNTER, RONLY, var_ip, 1, {15 }},
    {IPREASMFAILS, COUNTER, RONLY, var_ip, 1, {16 }},
#endif
    {IPFRAGOKS, COUNTER, RONLY, var_ip, 1, {17 }},
    {IPFRAGFAILS, COUNTER, RONLY, var_ip, 1, {18 }},
    {IPFRAGCREATES, COUNTER, RONLY, var_ip, 1, {19 }},
    {IPADADDR, IPADDRESS, RONLY, var_ipAddrEntry, 3, {20, 1, 1}},
    {IPADIFINDEX, INTEGER, RONLY, var_ipAddrEntry, 3, {20, 1, 2}},
#if !defined(sunV3) || defined(linux)
    {IPADNETMASK, IPADDRESS, RONLY, var_ipAddrEntry, 3, {20, 1, 3}},
#endif
    {IPADBCASTADDR, INTEGER, RONLY, var_ipAddrEntry, 3, {20, 1, 4}},
    {IPADENTREASMMAXSIZE , INTEGER, RONLY, var_ipAddrEntry, 3, {20, 1, 5}},
#ifndef linux
    {IPROUTEDEST, IPADDRESS, RONLY, var_ipRouteEntry, 3, {21, 1, 1}},
    {IPROUTEIFINDEX, INTEGER, RONLY, var_ipRouteEntry, 3, {21, 1, 2}},
    {IPROUTEMETRIC1, INTEGER, RONLY, var_ipRouteEntry, 3, {21, 1, 3}},
    {IPROUTEMETRIC2, INTEGER, RONLY, var_ipRouteEntry, 3, {21, 1, 4}},
    {IPROUTEMETRIC3, INTEGER, RONLY, var_ipRouteEntry, 3, {21, 1, 5}},
    {IPROUTEMETRIC4, INTEGER, RONLY, var_ipRouteEntry, 3, {21, 1, 6}},
    {IPROUTENEXTHOP, IPADDRESS, RONLY, var_ipRouteEntry, 3, {21, 1, 7}},
    {IPROUTETYPE, INTEGER, RONLY, var_ipRouteEntry, 3, {21, 1, 8}},
    {IPROUTEPROTO, INTEGER, RONLY, var_ipRouteEntry, 3, {21, 1, 9}},
    {IPROUTEAGE, INTEGER, RONLY, var_ipRouteEntry, 3, {21, 1, 10}}
#else /* linux */
    {IPROUTEDEST, IPADDRESS, RWRITE, var_ipRouteEntry, 3, {21, 1, 1}},
    {IPROUTEIFINDEX, INTEGER, RWRITE, var_ipRouteEntry, 3, {21, 1, 2}},
    {IPROUTEMETRIC1, INTEGER, RWRITE, var_ipRouteEntry, 3, {21, 1, 3}},
    {IPROUTEMETRIC2, INTEGER, RWRITE, var_ipRouteEntry, 3, {21, 1, 4}},
    {IPROUTEMETRIC3, INTEGER, RWRITE, var_ipRouteEntry, 3, {21, 1, 5}},
    {IPROUTEMETRIC4, INTEGER, RWRITE, var_ipRouteEntry, 3, {21, 1, 6}},
    {IPROUTENEXTHOP, IPADDRESS, RWRITE, var_ipRouteEntry, 3, {21, 1, 7}},
    {IPROUTETYPE, INTEGER, RWRITE, var_ipRouteEntry, 3, {21, 1, 8}},
    {IPROUTEPROTO, INTEGER, RONLY, var_ipRouteEntry, 3, {21, 1, 9}},
    {IPROUTEAGE, INTEGER, RWRITE, var_ipRouteEntry, 3, {21, 1, 10}}

    , {IPROUTEMASK, IPADDRESS, RWRITE, var_ipRouteEntry, 3, {21, 1, 11}}
    , {IPROUTEMETRIC5, INTEGER, RWRITE, var_ipRouteEntry, 3, {21, 1, 12}}
/** XXX: not yet: **/
/** XXX:    , {IPROUTEINFO, OBJID, RONLY, var_ipRouteEntry, 3, {21, 1, 13}} **/
    , {IPNETTOMEDIAIFINDEX, INTEGER, /* W */ RONLY, var_ntomEntry, 3, {22, 1, 1}}
    , {IPNETTOMEDIAPHYSADDR, STRING, /* W */ RONLY, var_ntomEntry, 3, {22, 1, 2}}
    , {IPNETTOMEDIANETADDR, IPADDRESS, /* W */ RONLY, var_ntomEntry, 3, {22, 1, 3}}
    , {IPNETTOMEDIATYPE, INTEGER, /* W */ RONLY, var_ntomEntry, 3, {22, 1, 4}}
#endif /* linux */
};

struct variable2 icmp_variables[] = {
    {ICMPINMSGS, COUNTER, RONLY, var_icmp, 1, {1}},
    {ICMPINERRORS, COUNTER, RONLY, var_icmp, 1, {2}},
    {ICMPINDESTUNREACHS, COUNTER, RONLY, var_icmp, 1, {3}},
    {ICMPINTIMEEXCDS, COUNTER, RONLY, var_icmp, 1, {4}},
    {ICMPINPARMPROBS, COUNTER, RONLY, var_icmp, 1, {5}},
    {ICMPINSRCQUENCHS, COUNTER, RONLY, var_icmp, 1, {6}},
    {ICMPINREDIRECTS, COUNTER, RONLY, var_icmp, 1, {7}},
    {ICMPINECHOS, COUNTER, RONLY, var_icmp, 1, {8}},
    {ICMPINECHOREPS, COUNTER, RONLY, var_icmp, 1, {9}},
    {ICMPINTIMESTAMPS, COUNTER, RONLY, var_icmp, 1, {10}},
    {ICMPINTIMESTAMPREPS, COUNTER, RONLY, var_icmp, 1, {11}},
    {ICMPINADDRMASKS, COUNTER, RONLY, var_icmp, 1, {12}},
    {ICMPINADDRMASKREPS, COUNTER, RONLY, var_icmp, 1, {13}},
    {ICMPOUTMSGS, COUNTER, RONLY, var_icmp, 1, {14}},
    {ICMPOUTERRORS, COUNTER, RONLY, var_icmp, 1, {15}},
    {ICMPOUTDESTUNREACHS, COUNTER, RONLY, var_icmp, 1, {16}},
    {ICMPOUTTIMEEXCDS, COUNTER, RONLY, var_icmp, 1, {17}},
    {ICMPOUTPARMPROBS, COUNTER, RONLY, var_icmp, 1, {18}},
    {ICMPOUTSRCQUENCHS, COUNTER, RONLY, var_icmp, 1, {19}},
    {ICMPOUTREDIRECTS, COUNTER, RONLY, var_icmp, 1, {20}},
    {ICMPOUTECHOS, COUNTER, RONLY, var_icmp, 1, {21}},
    {ICMPOUTECHOREPS, COUNTER, RONLY, var_icmp, 1, {22}},
    {ICMPOUTTIMESTAMPS, COUNTER, RONLY, var_icmp, 1, {23}},
    {ICMPOUTTIMESTAMPREPS, COUNTER, RONLY, var_icmp, 1, {24}},
    {ICMPOUTADDRMASKS, COUNTER, RONLY, var_icmp, 1, {25}},
    {ICMPOUTADDRMASKREPS, COUNTER, RONLY, var_icmp, 1, {26}}
};

struct variable13 tcp_variables[] = {
    {TCPRTOALGORITHM, INTEGER, RONLY, var_tcp, 1, {1}},
    {TCPRTOMIN, INTEGER, RONLY, var_tcp, 1, {2}},
#if !defined(sunV3) || defined(linux)
    {TCPRTOMAX, INTEGER, RONLY, var_tcp, 1, {3}},
#endif
    {TCPMAXCONN, INTEGER, RONLY, var_tcp, 1, {4}},
#if !defined(sunV3) || defined(linux)
    {TCPACTIVEOPENS, COUNTER, RONLY, var_tcp, 1, {5}},
    {TCPPASSIVEOPENS, COUNTER, RONLY, var_tcp, 1, {6}},
    {TCPATTEMPTFAILS, COUNTER, RONLY, var_tcp, 1, {7}},
    {TCPESTABRESETS, COUNTER, RONLY, var_tcp, 1, {8}},
#endif
    {  TCPCURRESTAB, GAUGE, RONLY, var_tcp, 1, {9}},
#if !defined(sunV3) || defined(linux)
    {TCPINSEGS, COUNTER, RONLY, var_tcp, 1, {10}},
    {TCPOUTSEGS, COUNTER, RONLY, var_tcp, 1, {11} },
    {TCPRETRANSSEGS, COUNTER, RONLY, var_tcp, 1, {12}},
#endif
    {TCPCONNSTATE, INTEGER, RONLY, var_tcp, 3, {13, 1, 1}},
    {TCPCONNLOCALADDRESS, IPADDRESS, RONLY, var_tcp, 3, {13, 1, 2}},
    {TCPCONNLOCALPORT, INTEGER, RONLY, var_tcp, 3, {13, 1, 3}},
    {TCPCONNREMADDRESS, IPADDRESS, RONLY, var_tcp, 3, {13, 1, 4}},
    {TCPCONNREMPORT, INTEGER, RONLY, var_tcp, 3, {13, 1, 5}}
};

#ifdef linux
struct variable13 udp_variables[] = {
#else
struct variable2 udp_variables[] = {
#endif
    {UDPINDATAGRAMS, COUNTER, RONLY, var_udp, 1, {1}},
    {UDPNOPORTS, COUNTER, RONLY, var_udp, 1, {2}},
    {UDPINERRORS, COUNTER, RONLY, var_udp, 1, {3}},
    {UDPOUTDATAGRAMS, COUNTER, RONLY, var_udp, 1, {4}}
#ifdef linux
    , {UDPLOCALADDRESS, IPADDRESS, RONLY, var_udp, 3, {5, 1, 1}}
    , {UDPLOCALPORT, INTEGER, RONLY, var_udp, 3, {5, 1, 2}}
#endif
};

#if !defined(sparc) && !defined(linux)
struct variable2 process_variables[] = {
    {PROCESSSLOTINDEX, INTEGER, RONLY, var_process, 1, {1}},
    {PROCESSID, INTEGER, RONLY, var_proces, 1, {2}},
    {PROCESSCOMMAND, STRING, RONLY, var_process, 1, {3}}
};
#endif


#ifdef linux

struct variable2 snmp_variables[] = {
    {SNMPINPKTS, COUNTER, RONLY, var_snmp, 1, {1}},
    {SNMPOUTPKTS, COUNTER, RONLY, var_snmp, 1, {2}},
    {SNMPINBADVERSIONS, COUNTER, RONLY, var_snmp, 1, {3}},
    {SNMPINBADCOMMUNITYNAMES, COUNTER, RONLY, var_snmp, 1, {4}},
    {SNMPINBADCOMMUNITYUSES, COUNTER, RONLY, var_snmp, 1, {5}},
    {SNMPINASNPARSEERRORS, COUNTER, RONLY, var_snmp, 1, {6}},
    {SNMPINTOOBIGS, COUNTER, RONLY, var_snmp, 1, {8}},
    {SNMPINNOSUCHNAMES, COUNTER, RONLY, var_snmp, 1, {9}},
    {SNMPINBADVALUES, COUNTER, RONLY, var_snmp, 1, {10}},
    {SNMPINREADONLYS, COUNTER, RONLY, var_snmp, 1, {11}},
    {SNMPINGENERRS, COUNTER, RONLY, var_snmp, 1, {12}},
    {SNMPINTOTALREQVARS, COUNTER, RONLY, var_snmp, 1, {13}},
    {SNMPINTOTALSETVARS, COUNTER, RONLY, var_snmp, 1, {14}},
    {SNMPINGETREQUESTS, COUNTER, RONLY, var_snmp, 1, {15}},
    {SNMPINGETNEXTS, COUNTER, RONLY, var_snmp, 1, {16}},
    {SNMPINSETREQUESTS, COUNTER, RONLY, var_snmp, 1, {17}},
    {SNMPINGETRESPONSES, COUNTER, RONLY, var_snmp, 1, {18}},
    {SNMPINTRAPS, COUNTER, RONLY, var_snmp, 1, {19}},
    {SNMPOUTTOOBIGS, COUNTER, RONLY, var_snmp, 1, {20}},
    {SNMPOUTNOSUCHNAMES, COUNTER, RONLY, var_snmp, 1, {21}},
    {SNMPOUTBADVALUES, COUNTER, RONLY, var_snmp, 1, {22}},
    {SNMPOUTGENERRS, COUNTER, RONLY, var_snmp, 1, {24}},
    {SNMPOUTGETREQUESTS, COUNTER, RONLY, var_snmp, 1, {25}},
    {SNMPOUTGETNEXTS, COUNTER, RONLY, var_snmp, 1, {26}},
    {SNMPOUTSETREQUESTS, COUNTER, RONLY, var_snmp, 1, {27}},
    {SNMPOUTGETRESPONSES, COUNTER, RONLY, var_snmp, 1, {28}},
    {SNMPOUTTRAPS, COUNTER, RONLY, var_snmp, 1, {29}},
    {SNMPENABLEAUTHENTRAPS, INTEGER, RWRITE, var_snmp, 1, {30}}
};


struct variable13 hr_variables[] = {
    {HRSYSTEMUPTIME, TIMETICKS, RONLY, var_hr, 2, {1, 1}}
    , {HRSYSTEMDATE, STRING, RONLY, var_hr, 2, {1, 2}}
    , {HRSYSTEMINITIALLOADDEVICE, INTEGER, RWRITE, var_hr, 2, {1, 3}}
    , {HRSYSTEMINITIALLOADPARAMETERS, STRING, RWRITE, var_hr, 2, {1, 4}}
    , {HRSYSTEMNUMUSERS, GAUGE, RONLY, var_hr, 2, {1, 5}}
    , {HRSYSTEMPROCESSES, GAUGE, RONLY, var_hr, 2, {1, 6}}
    , {HRSYSTEMMAXPROCESSES, INTEGER, RONLY, var_hr, 2, {1, 7}}

    , {HRMEMORYSIZE, INTEGER, RONLY, var_hr, 2, {2, 2}}

    , {HRSTORAGEINDEX, INTEGER, RONLY, var_hr, 4, {2, 3, 1, 1}}
    , {HRSTORAGETYPE, OBJID, RONLY, var_hr, 4, {2, 3, 1, 2}}
    , {HRSTORAGEDESCR, STRING, RONLY, var_hr, 4, {2, 3, 1, 3}}
    , {HRSTORAGEALLOCUNITS, INTEGER, RONLY, var_hr, 4, {2, 3, 1, 4}}
    , {HRSTORAGESIZE, INTEGER, RONLY, var_hr, 4, {2, 3, 1, 5}}
    , {HRSTORAGEUSED, INTEGER, RONLY, var_hr, 4, {2, 3, 1, 6}}
    , {HRSTORAGEALLOCFAILURES, COUNTER, RONLY, var_hr, 4, {2, 3, 1, 7}}

    , {HRDEVICEINDEX, INTEGER, RONLY, var_hr, 4, {3, 2, 1, 1}}
    , {HRDEVICETYPE, OBJID, RONLY, var_hr, 4, {3, 2, 1, 2}}
    , {HRDEVICEDESCR, STRING, RONLY, var_hr, 4, {3, 2, 1, 3}}
    , {HRDEVICEID, OBJID, RONLY, var_hr, 4, {3, 2, 1, 4}}
    , {HRDEVICESTATUS, INTEGER, RONLY, var_hr, 4, {3, 2, 1, 5}}
    , {HRDEVICEERRORS, COUNTER, RONLY, var_hr, 4, {3, 2, 1, 6}}

    , {HRPROCESSORFRWID, OBJID,  RONLY, var_hr, 4, {3, 3, 1, 1 }}
    , {HRPROCESSORLOAD, INTEGER, RONLY, var_hr, 4, {3, 3, 1, 2 }}

    , {HRFSINDEX, INTEGER, RONLY, var_hr, 4, {3, 8, 1, 1 }}
    , {HRFSMOUNTPOINT, STRING, RONLY, var_hr, 4, {3, 8, 1, 2 }}
    , {HRFSREMOTEMOUNTPOINT, STRING, RONLY, var_hr, 4, {3, 8, 1, 3 }}
    , {HRFSTYPE, OBJID, RONLY, var_hr, 4, {3, 8, 1, 4 }}
    , {HRFSACCESS, INTEGER, RONLY, var_hr, 4, {3, 8, 1, 5 }}
    , {HRFSBOOTABLE, INTEGER, RONLY, var_hr, 4, {3, 8, 1, 6 }}
    , {HRFSSTORAGEINDEX, INTEGER, RONLY, var_hr, 4, {3, 8, 1, 7 }}
    , {HRFSLASTFULLBACKUPDATE, STRING, RONLY, var_hr, 4, {3, 8, 1, 8 }}
    , {HRFSLASTPARTIALBACKUPDATE, STRING, RONLY, var_hr, 4, {3, 8, 1, 9 }}

    , {HRSWOSINDEX, INTEGER, RONLY, var_hr, 2, {4, 1}}

    , {HRSWRUNINDEX, INTEGER, RONLY, var_hr, 4, {4, 2, 1, 1}}
    , {HRSWRUNNAME, STRING, RONLY, var_hr, 4, {4, 2, 1, 2}}
    , {HRSWRUNID, OBJID, RONLY, var_hr, 4, {4, 2, 1, 3}}
    , {HRSWRUNPATH, STRING, RONLY, var_hr, 4, {4, 2, 1, 4}}
    , {HRSWRUNPARAMETERS, STRING, RONLY, var_hr, 4, {4, 2, 1, 5}}
    , {HRSWRUNTYPE, INTEGER, RONLY, var_hr, 4, {4, 2, 1, 6}}
    , {HRSWRUNSTATUS, INTEGER, RONLY, var_hr, 4, {4, 2, 1, 7}}

    , {HRSWRUNPERFCPU, INTEGER, RONLY, var_hr, 4, {5, 1, 1, 1}}
    , {HRSWRUNPERFMEM, INTEGER, RONLY, var_hr, 4, {5, 1, 1, 2}}
};

struct variable13 id_variables[] = {
    {IDIDENTSTATUS, INTEGER, RONLY, var_id, 4, {1, 1, 1, 1}}
    , {IDIDENTOPSYS, STRING, RONLY, var_id, 4, {1, 1, 1, 2}}
    , {IDIDENTCHARSET, STRING, RONLY, var_id, 4, {1, 1, 1, 3}}
    , {IDIDENTUSERID, STRING, RONLY, var_id, 4, {1, 1, 1, 4}}
    , {IDIDENTMISC, STRING, RONLY, var_id, 4, {1, 1, 1, 5}}
};

struct variable13 linux_variables[] = {
    {LINUXCPU, STRING, RONLY, var_linux, 1, {1}}
    , {LINUXBOGO, INTEGER, RONLY, var_linux, 1, {2}}
};
#endif /* linux */


struct variable2 snmpstats_variables[] = {
    {1,  COUNTER, RONLY, var_snmpStats, 1, {1}},
    {3,  COUNTER, RONLY, var_snmpStats, 1, {3}},
    {11, COUNTER, RONLY, var_snmpStats, 1, {11}},
    {12, COUNTER, RONLY, var_snmpStats, 1, {12}},
    {13, COUNTER, RONLY, var_snmpStats, 1, {13}},
};

struct variable2 snmpv1stats_variables[] = {
    {101,  COUNTER, RONLY, var_snmpStats, 1, {1}},
    {102,  COUNTER, RONLY, var_snmpStats, 1, {2}},
};

struct variable2 usecagent_variables[] = {
    {1, STRING,  RONLY, var_usecAgent, 1, {1}},
    {2, GAUGE,   RONLY, var_usecAgent, 1, {2}},
    {3, GAUGE,   RONLY, var_usecAgent, 1, {3}},
    {4, INTEGER, RONLY, var_usecAgent, 1, {4}},
};

struct variable2 usecstats_variables[] = {
    {1, COUNTER, RONLY, var_usecStats, 1, {1}},
    {2, COUNTER, RONLY, var_usecStats, 1, {2}},
    {3, COUNTER, RONLY, var_usecStats, 1, {3}},
    {4, COUNTER, RONLY, var_usecStats, 1, {4}},
    {5, COUNTER, RONLY, var_usecStats, 1, {5}},
    {6, COUNTER, RONLY, var_usecStats, 1, {6}},
    {7, COUNTER, RONLY, var_usecStats, 1, {7}},
};

struct variable2 setserno_variables[] = {
    {1, INTEGER, RWRITE, var_rwstats, 1, {1}},
};

struct variable2 v2authtraps_variables[] = {
    {2, INTEGER, RWRITE, var_rwstats, 1, {4}},
};

/*
 * Note that the name field must be larger than any name that might
 * match that object.  For these variable length (objid) indexes
 * this might seem to be hard, but placing MAXINT in the first
 * subid of the index denotes an obcenely long objid, thereby ensuring that
 * none slip through.
 */
/* No access for community SNMP, RW possible for Secure SNMP */
#define PRIVRW   0x0003  
/* No access for community SNMP, RO possible for Secure SNMP */
#define PRIVRO   0x0002

u_char *var_hosttimetab();

struct subtree subtrees[] = {
    {{MIB, 1}, 7, (struct variable *)system_variables,
	 sizeof(system_variables)/sizeof(*system_variables),
	 sizeof(*system_variables)},
    {{MIB, 1, 9, 1}, 9, (struct variable *)or_variables,
	 sizeof(or_variables)/sizeof(*or_variables),
	 sizeof(*or_variables)},
    {{MIB, 2}, 7, (struct variable *)interface_variables,
	 sizeof(interface_variables)/sizeof(*interface_variables),
	 sizeof(*interface_variables)},
    {{MIB, 3, 1, 1}, 9, (struct variable *)at_variables,
	 sizeof(at_variables)/sizeof(*at_variables),
	 sizeof(*at_variables)},
    {{MIB, 4}, 7, (struct variable *)ip_variables,
	 sizeof(ip_variables)/sizeof(*ip_variables),
	 sizeof(*ip_variables)},
    {{MIB, 5}, 7, (struct variable *)icmp_variables,
	 sizeof(icmp_variables)/sizeof(*icmp_variables),
	 sizeof(*icmp_variables)},
    {{MIB, 6}, 7, (struct variable *)tcp_variables,
	 sizeof(tcp_variables)/sizeof(*tcp_variables),
	 sizeof(*tcp_variables)},
    {{MIB, 7}, 7, (struct variable *)udp_variables,
	 sizeof(udp_variables)/sizeof(*udp_variables),
	 sizeof(*udp_variables)},

#ifdef linux
    {{MIB, 11}, 7, (struct variable *)snmp_variables,
	 sizeof(snmp_variables)/sizeof(*snmp_variables),
	 sizeof(*snmp_variables)},
#if 1
    /* Identification MIB (rfc 1414): */
    {{MIB, 24}, 7, (struct variable *)id_variables,
	 sizeof(id_variables)/sizeof(*id_variables),
	 sizeof(*id_variables)},

    /* Host Resources MIB ala rfc1514: */
    {{MIB, 25}, 7, (struct variable *)hr_variables,
	 sizeof(hr_variables)/sizeof(*hr_variables),
	 sizeof(*hr_variables)},
#endif

    /* LINUX specific MIB: */
    {{1, 3, 6, 1, 4, 1, 1575, 1, 5, 2}, 10, (struct variable *)linux_variables,
	 sizeof(linux_variables)/sizeof(*linux_variables),
	 sizeof(*linux_variables)},
#endif

    {{SNMPSTATS}, 9, (struct variable *)snmpstats_variables,
	 sizeof(snmpstats_variables)/sizeof(*snmpstats_variables),
	 sizeof(*snmpstats_variables)},
    {{SNMPV1STATS}, 9, (struct variable *)snmpv1stats_variables,
	 sizeof(snmpv1stats_variables)/sizeof(*snmpv1stats_variables),
	 sizeof(*snmpv1stats_variables)},
    {{SNMPTRAP}, 9, (struct variable *)v2authtraps_variables,
	 sizeof(v2authtraps_variables)/sizeof(*v2authtraps_variables),
	 sizeof(*v2authtraps_variables)},
    {{SNMPSET}, 9, (struct variable *)setserno_variables,
	 sizeof(setserno_variables)/sizeof(*setserno_variables),
	 sizeof(*setserno_variables)},
    {{USECAGENT}, 9, (struct variable *)usecagent_variables,
	 sizeof(usecagent_variables)/sizeof(*usecagent_variables),
	 sizeof(*usecagent_variables)},
    {{USECSTATS}, 9, (struct variable *)usecstats_variables,
	 sizeof(usecstats_variables)/sizeof(*usecstats_variables),
	 sizeof(*usecstats_variables)},
};

static long setSerialNo = 0;
static int  v2EnableAuthTraps = 1;

int
in_view(name, namelen, viewIndex)
    oid *name;
    int namelen, viewIndex;
{
    viewEntry *vwp, *savedvwp = NULL;
    extern viewEntry *views;

    for( vwp = views; vwp; vwp = vwp->next ) {
	if (vwp->viewIndex != viewIndex )
	    continue;
	if (vwp->viewSubtreeLen > namelen
	    || bcmp(vwp->viewSubtree, name, vwp->viewSubtreeLen * sizeof(oid)))
	    continue;
	/* no wildcards here yet */
	if (!savedvwp){
	    savedvwp = vwp;
	} else {
	    if (vwp->viewSubtreeLen > savedvwp->viewSubtreeLen)
		savedvwp = vwp;
	}
    }
    if (!savedvwp)
	return FALSE;
    if (savedvwp->viewType == VIEWINCLUDED)
	return TRUE;
    return FALSE;
}

/*
 * getStatPtr - return a pointer to the named variable, as well as it's
 * type, length, and access control list.
 *
 * If an exact match for the variable name exists, it is returned.  If not,
 * and exact is false, the next variable lexicographically after the
 * requested one is returned.
 *
 * If no appropriate variable can be found, NULL is returned.
 */
u_char	*
getStatPtr(name, namelen, type, len, acl, exact, write_method, snmpversion,
	   noSuchObject, view)
    oid		*name;	    /* IN - name of var, OUT - name matched */
    int		*namelen;   /* IN -number of sub-ids in name, OUT - subid-is in matched name */
    u_char	*type;	    /* OUT - type of matched variable */
    int		*len;	    /* OUT - length of matched variable */
    u_short	*acl;	    /* OUT - access control list */
    int		exact;	    /* IN - TRUE if exact match wanted */
    int	       (**write_method)(); /* OUT - pointer to function called to set variable, otherwise 0 */
    int		 snmpversion;
    int		*noSuchObject;
    int		 view;
{
    struct subtree	*tp;
    struct variable *vp = 0;
    struct variable	compat_var, *cvp = &compat_var;
    register int	x;
    int			y;
    register u_char	*access = NULL;
    int			result, treeresult;
    oid 		*suffix;
    int			suffixlen;
    int 		found = FALSE;
    oid			save[MAX_NAME_LEN];
    int			savelen = 0;

    if( view == 0 ) return NULL;

    if (!exact){
	bcopy(name, save, *namelen * sizeof(oid));
	savelen = *namelen;
    }
    *write_method = NULL;
    for (y = 0, tp = subtrees; y < sizeof(subtrees)/sizeof(struct subtree);
	 tp++, y++){
	treeresult = compare_tree(name, *namelen, tp->name, (int)tp->namelen);
	/* if exact and treerresult == 0
	   if next  and treeresult <= 0 */
	if (treeresult == 0 || (!exact && treeresult < 0)){
	    result = treeresult;
	    suffixlen = *namelen - tp->namelen;
	    suffix = name + tp->namelen;
	    /* the following is part of the setup for the compatability
	       structure below that has been moved out of the main loop.
	     */
	    bcopy((char *)tp->name, (char *)cvp->name,
		  tp->namelen * sizeof(oid));

	    for(x = 0, vp = tp->variables; x < tp->variables_len;
		vp =(struct variable *)((char *)vp +tp->variables_width), x++){
		/* if exact and ALWAYS
		   if next  and result >= 0 */
		if (exact || result >= 0){
		    result = compare_tree(suffix, suffixlen, vp->name,
				     (int)vp->namelen);
		}
		/* if exact and result == 0
		   if next  and result <= 0 */
		if ((!exact && (result <= 0)) || (exact && (result == 0))){
		    /* builds an old (long) style variable structure to retain
		       compatability with var_* functions written previously.
		     */
		    bcopy((char *)vp->name, (char *)(cvp->name + tp->namelen),
			  vp->namelen * sizeof(oid));
		    cvp->namelen = tp->namelen + vp->namelen;
		    cvp->type = vp->type;
		    cvp->magic = vp->magic;
		    cvp->acl = vp->acl;
		    cvp->findVar = vp->findVar;
		    access = (*(vp->findVar))(cvp, name, namelen, exact,
						  len, write_method);
		    if (write_method)
			*acl = vp->acl;
		    if (access /*&& (snmpversion == SNMP_VERSION_2)*/
			&& !in_view(name, *namelen, view) ) {
			access = NULL;
			*write_method = NULL;
		    } else if (exact){
			found = TRUE;
		    }
		    /* this code is incorrect if there is
		       a view configuration that exludes a particular
		       instance of a variable.  It would return noSuchObject,
		       which would be an error */
		    if (access != NULL)
			break;
		}
		/* if exact and result <= 0 */
		if (exact && (result  <= 0)){
	            *type = vp->type;
		    *acl = vp->acl;
		    if (found)
			*noSuchObject = FALSE;
		    else
			*noSuchObject = TRUE;
		    return NULL;
		}
	    }
	    if (access != NULL)
		break;
	}
    }
    if (y == sizeof(subtrees)/sizeof(struct subtree)){
	if (!access && !exact){
	    bcopy(save, name, savelen * sizeof(oid));
	    *namelen = savelen;
	}
	if (found)
	    *noSuchObject = FALSE;
	else
	    *noSuchObject = TRUE;
        return NULL;
    }
    /* vp now points to the approprate struct */
    *type = vp->type;
    *acl = vp->acl;
    return access;
}

/*
{
  *write_method = NULL;
  for(tp = first; tp < end; tp = next){
      if ((in matches tp) or (in < tp)){
	  inlen -= tp->length;
	  for(vp = tp->vp; vp < end; vp = next){
	      if ((in < vp) || (exact && (in == vp))){
		  cobble up compatable vp;
		  call findvar;
		  if (it returns nonzero)
		      break both loops;
	      }
	      if (exact && (in < vp)) ???
		  return NULL;
	  }
      }      
  }
}
*/

int
compare(name1, len1, name2, len2)
    register oid	    *name1, *name2;
    register int	    len1, len2;
{
    register int    len;

#define cmpprintf	if(0) printf
    { int i;
      cmpprintf ("comparing ");
      for (i = 0; i < len1; i++)
	cmpprintf ("%ld%s", name1[i], i < len1 - 1 ? "." : "");
      cmpprintf (" with ");
      for (i = 0; i < len2; i++)
	cmpprintf ("%ld%s", name2[i], i < len2 - 1 ? "." : "");
    }

    /* len = minimum of len1 and len2 */
    if (len1 < len2)
	len = len1;
    else
	len = len2;
    /* find first non-matching byte */
    while(len-- > 0){
	if (*name1 < *name2) {
	    cmpprintf (" giving -1\n");
	    return -1;
	}
	if (*name2++ < *name1++) {
	    cmpprintf (" giving 1\n");
	    return 1;
	}
    }
    /* bytes match up to length of shorter string */
    if (len1 < len2) {
	cmpprintf (" giving -1\n");
	return -1;  /* name1 shorter, so it is "less" */
    }
    if (len2 < len1) {
	cmpprintf (" giving 1\n");
	return 1;
    }

    cmpprintf (" giving 0\n");

    return 0;	/* both strings are equal */
}

static int
compare_tree(name1, len1, name2, len2)
    register oid	    *name1, *name2;
    register int	    len1, len2;
{
    register int    len;

    /* len = minimum of len1 and len2 */
    if (len1 < len2)
	len = len1;
    else
	len = len2;
    /* find first non-matching byte */
    while(len-- > 0){
	if (*name1 < *name2)
	    return -1;
	if (*name2++ < *name1++)
	    return 1;
    }
    /* bytes match up to length of shorter string */
    if (len1 < len2)
	return -1;  /* name1 shorter, so it is "less" */
    /* name1 matches name2 for length of name2, or they are equal */
    return 0;
}



#if defined(linux) || ! defined(notdef)
/* ../snmplib/snmp.c defines this without being if'defed */
extern char sysContact[256];
extern char sysLocation[256];
extern char sysName[256];
#else
char sysContact[256] = "Unknown";
char sysLocation[256] = "Unknown";
char sysName[256] = "Unknown";
#endif


#ifdef linux
/* According to James T. Koerg ;-) */
oid version_id[] = {1, 3, 6, 1, 4, 1, 1575, 1, 5};
#else
oid version_id[] = {1, 3, 6, 1, 4, 1, 3, 1, 1};
#endif

u_long
sysUpTime(){
#ifndef linux
    struct timeval now, boottime;
    
    if (KNLookup(N_BOOTTIME, (char *)&boottime, sizeof(boottime)) == NULL) {
	return(0);
    }
    gettimeofday(&now, (struct timezone *)0);
    return (u_long) ((now.tv_sec - boottime.tv_sec) * 100
			    + (now.tv_usec - boottime.tv_usec) / 10000);
#else /* linux */
    { FILE *in = fopen ("/proc/uptime", "r");
      long uptim = 0, a, b;
      if (in)
	{
	  if (2 == fscanf (in, "%ld.%ld", &a, &b))
	    uptim = a * 100 + b;
	  fclose (in);
	}
      return uptim;
    }
#endif /* linux */
}

u_char *
var_hosttimetab(vp, name, length, exact, var_len, write_method)
        register struct variable *vp;   /* IN - pointer to variable entry that
                                                                        ** points here
                                                                        */
        register oid *name;             /* IN/OUT - input name requested,
                                                        ** output name found
                                                        */
        register int *length;   /* IN/OUT - length of input and output oid's */
        int exact;              /* IN - TRUE if an exact match was requested. */
        int *var_len;   /* OUT - length of variable or 0 if function returned. */
        int                     (**write_method)(); /* OUT - pointer to function to set
                                                                        ** variable, otherwise 0
                                                                        */
{
#ifndef linux
        oid newname[MAX_NAME_LEN];
        int result;
	int creationOrder;
	static int zero = 0;

        bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
        *write_method = 0;

	newname[vp->namelen] = (oid)1;

	if (exact){
	    creationOrder = name[vp->namelen + 1];
	    if (creationOrder > 2000)
		return NULL;
	    newname[vp->namelen + 1] = creationOrder;
	} else if (*length == vp->namelen + 2){
	    creationOrder = name[vp->namelen + 1] + 1;
	    if (creationOrder > 2000){
		if ((vp->name[vp->namelen - 1] != name[vp->namelen - 1])){
		    creationOrder = 1;
		} else {
		    return NULL;
		}
	    }
	    newname[vp->namelen + 1] = creationOrder;
	} else {
	    printf("Slow code\n");
	    creationOrder = 1;
	    while (creationOrder < 2000) {
		newname[vp->namelen + 1] = (oid)creationOrder++;
		result = compare(name, *length, newname, (int)vp->namelen + 2);
		if ((exact && (result == 0)) || (!exact && (result < 0))) {
		    break;
		}
	    }
	    if (creationOrder == 2002) {
                return NULL;
	    }
	}
        bcopy((char *)newname, (char *)name,
	      ((int)vp->namelen + 2) * sizeof(oid));
        *length = vp->namelen + 2;
        *var_len = sizeof(u_long);

        switch (vp->magic) {
                case HOSTTIMETABADDRESS:
                        *var_len = sizeof(struct ether_addr);
                        return (u_char *) "RMONRULES";
                case HOSTTIMETABCREATIONORDER:
			long_return = creationOrder;
			return (u_char *) &long_return;
                case HOSTTIMETABINDEX:
                case HOSTTIMETABINPKTS:
                case HOSTTIMETABOUTPKTS:
                case HOSTTIMETABINOCTETS:
                case HOSTTIMETABOUTOCTETS:
                case HOSTTIMETABOUTERRORS:
                case HOSTTIMETABOUTBCASTPKTS:
                case HOSTTIMETABOUTMCASTPKTS:
                        return (u_char *) &zero;
                default:
                        ERROR("");
        }

#else /* linux */
	return NULL;
#endif /* linux */
}

u_char *
var_system(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;   /* IN - pointer to variable entry that points here */
    register oid	*name;	    /* IN/OUT - input name requested, output name found */
    register int	*length;    /* IN/OUT - length of input and output oid's */
    int			exact;	    /* IN - TRUE if an exact match was requested. */
    int			*var_len;   /* OUT - length of variable or 0 if function returned. */
    int			(**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    extern int writeVersion(), writeSystem();
    oid newname[MAX_NAME_LEN];
    int result;

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    newname[8] = 0;
    result = compare(name, *length, newname, (int)vp->namelen + 1);
    if ((exact && (result != 0)) || (!exact && (result >= 0)))
	return NULL;
    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;
    *write_method = 0;
    *var_len = sizeof(long);	/* default length */
    switch (vp->magic){
	case VERSION_DESCR:
	    *var_len = strlen(version_descr);
	    /** not writable:
	     ** *write_method = writeVersion; 
	     **/
	    return (u_char *)version_descr;
	case VERSION_ID:
	    *var_len = sizeof(version_id);
	    return (u_char *)version_id;
	case UPTIME:
	case SYSORLASTCHANGE:
#ifdef linux
	/* the uptime from the agent is wanted: */
	    { struct timeval tv;
	      gettimeofday (&tv, (struct timezone *) 0);
	      long_return = (tv.tv_sec - uptime_stamp) * 100 
			+ tv.tv_usec / 10000;
	    }
#else
	    (u_long)long_return = sysUpTime();
#endif
	    return (u_char *)&long_return;
	case IFNUMBER:
	    long_return = Interface_Scan_Get_Count();
	    return (u_char *) &long_return;
	case SYSCONTACT:
	    *var_len = strlen(sysContact);
	    *write_method = writeSystem;
	    return (u_char *)sysContact;
        case SYSNAME:
	    *var_len = strlen(sysName);
	    *write_method = writeSystem;
	    return (u_char *)sysName;
        case SYSLOCATION:
	    *var_len = strlen(sysLocation);
	    *write_method = writeSystem;
	    return (u_char *)sysLocation;
	case SYSSERVICES:
	    long_return = 72;
	    return (u_char *)&long_return;
	default:
	    ERROR("");
    }
    return NULL;
}

u_char *
var_demo(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;   /* IN - pointer to variable entry that points here */
    register oid	*name;	    /* IN/OUT - input name requested, output name found */
    register int	*length;    /* IN/OUT - length of input and output oid's */
    int			exact;	    /* IN - TRUE if an exact match was requested. */
    int			*var_len;   /* OUT - length of variable or 0 if function returned. */
    int			(**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    oid newname[MAX_NAME_LEN];
    int result;
    static u_char bitstring[64] = {0, 0x83, 0};
    int bitstringlength = 3;
    static u_char nsap[128] = {0x14, 0x47, 0x00, 0x05, 0x80, 0xff, 0xff, 0x00,
				   0x00, 0x00, 0x01, 0x23, 0x01, 0x23, 0x01,
				   0x23, 0x45, 0x67, 0x89, 0xab, 0x01};
    int nsaplength = 21;
    static struct counter64 counter;

    counter.high = 0xFA202E75;
    counter.low = 0x4FE92915;

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    newname[7] = 0;
    result = compare(name, *length, newname, (int)vp->namelen + 1);
    if ((exact && (result != 0)) || (!exact && (result >= 0)))
	return NULL;
    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;
    *write_method = 0;
    *var_len = sizeof(long);	/* default length */
    switch (vp->magic){
	case MTRBITSTRING:
	    *var_len = bitstringlength;
	    return (u_char *)bitstring;
	case MTRNSAPADDRESS:
	    *var_len = nsaplength;
	    return (u_char *)nsap;
	case MTRBIGNUMBER:
	    *var_len = sizeof(counter);
	    return (u_char *) &counter;
	default:
	    ERROR("");
    }
    return NULL;
}

#if 0

/* XXX: version (sysDescr) is not writable (rfc 1213) */

#include <ctype.h>
int
writeVersion(action, var_val, var_val_type, var_val_len, statP, name, name_len)
   int      action;
   u_char   *var_val;
   u_char   var_val_type;
   int      var_val_len;
   u_char   *statP;
   oid      *name;
   int      name_len;
{
    int bigsize = 1000;
    u_char buf[sizeof(version_descr)], *cp;
    int count, size;

    if (var_val_type != STRING){
	ERROR("not string");
	return SNMP_ERR_WRONGTYPE;
    }
    if (var_val_len > sizeof(version_descr)-1){
	ERROR("bad length");
	return SNMP_ERR_WRONGLENGTH;
    }
    size = sizeof(buf);
    asn_parse_string(var_val, &bigsize, &var_val_type, buf, &size);
    for(cp = buf, count = 0; count < size; count++, cp++){
	if (!isprint(*cp)){
	    printf("not print %x\n", *cp);
	    return SNMP_ERR_WRONGVALUE;
	}
    }
    buf[size] = 0;
    if (action == COMMIT){
	strcpy(version_descr, buf);
	
    }
    return SNMP_ERR_NOERROR;
}
#endif /* 0 */


static void
save_into_conffile (key, val)
char *key, *val;
{
    extern char *snmp_configfile;
    char *p, *q, bak [1024], line [1024], newval [1000];
    int saved = 0;
    FILE *in, *out;

    /* change newlines to spaces, cuz we cannot handle newlines in 
     * the configfile. */

    for (p = val, q = newval; *p && q < newval+999; p++, q++)
      *q = *p == '\n' ? ' ' : *p;
    *q = 0;

    sprintf (bak, "%s.%d", snmp_configfile, getpid());

    if (! (in = fopen (snmp_configfile, "r")) 
	|| ! (out = fopen (bak, "w")))
      {
	  fprintf (stderr, "snmpd: cannot change config file %s\n",
		   snmp_configfile);
	  return;
      }
    
    while (fgets (line, sizeof (line), in))
      {
	  if (! strncmp (key, line, strlen (key)))
	    {
		fprintf (out, "%s\t\t%s\n", key, newval);
		saved = 1;
	    }
	  else
	    fputs (line, out);
      }
    fclose (in); 
    if (! saved)
      {
	  fprintf (out, "\n## appended by set operation:\n%s\t\t%s\n", 
		   key, newval);
      }
	  
    fclose (out);

    if (rename (bak, snmp_configfile) < 0)
      perror ("snmpd: save: cannot rename config file");
}


u_char *
var_rwstats(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;   /* IN - pointer to variable entry that points here */
    register oid	*name;	    /* IN/OUT - input name requested, output name found */
    register int	*length;    /* IN/OUT - length of input and output oid's */
    int			exact;	    /* IN - TRUE if an exact match was requested. */
    int			*var_len;   /* OUT - length of variable or 0 if function returned. */
    int			(**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    oid newname[MAX_NAME_LEN];
    int result;
    extern int writeSetSerialNumber();

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    newname[10] = 0;
    result = compare(name, *length, newname, (int)vp->namelen + 1);
    if ((exact && (result != 0)) || (!exact && (result >= 0)))
	return NULL;
    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;
    *write_method = 0;
    *var_len = sizeof(long);	/* default length */
    switch (vp->magic){
	case 1:
	    *write_method = writeSetSerialNumber;
	    return (u_char *)&setSerialNo;
	case 2:
	    return (u_char *)&v2EnableAuthTraps;
	default:
	    ERROR("");
    }
    return NULL;
}

int
writeSetSerialNumber(action, var_val, var_val_type, var_val_len, statP, name, name_len)
   int      action;
   u_char   *var_val;
   u_char   var_val_type;
   int      var_val_len;
   u_char   *statP;
   oid      *name;
   int      name_len;
{
    int size;
    long val;
    int bigsize = 1000;

    if (var_val_type != INTEGER){
	printf("not string\n");
	return SNMP_ERR_WRONGTYPE;
    }
    size = sizeof(long);
    asn_parse_int(var_val, &bigsize, &var_val_type, &val, size);
    if( val != setSerialNo ) {
	return SNMP_ERR_INCONSISTENTVALUE;
    }
    if (action == COMMIT){
	setSerialNo++;
    }
    return SNMP_ERR_NOERROR;
}


int
writeSystem(action, var_val, var_val_type, var_val_len, statP, name, name_len)
   int      action;
   u_char   *var_val;
   u_char   var_val_type;
   int      var_val_len;
   u_char   *statP;
   oid      *name;
   int      name_len;
{
    int bigsize = 1000;
    u_char buf[sizeof(version_descr)], *cp;
    int count, size;

    if (var_val_type != STRING){
	printf("not string\n");
	return SNMP_ERR_WRONGTYPE;
    }
    if (var_val_len > sizeof(version_descr)-1){
	printf("bad length\n");
	return SNMP_ERR_WRONGLENGTH;
    }
    size = sizeof(buf);
    asn_parse_string(var_val, &bigsize, &var_val_type, buf, &size);
    for(cp = buf, count = 0; count < size; count++, cp++){
	if (!isprint(*cp)){
	    printf("not print %x\n", *cp);
	    return SNMP_ERR_WRONGVALUE;
	}
    }
    buf[size] = 0;
    if (action == COMMIT){
	switch((char)name[7]){
#if 0
	  case 1:
	    strcpy(version_descr, buf);
	    break;
#endif
	  case 4:
	    strcpy(sysContact, buf);
	    save_into_conffile ("system contact:", sysContact);
	    break;
	  case 5:
	    strcpy(sysName, buf);
	    save_into_conffile ("system name:", sysName);
	    break;
	  case 6:
	    strcpy(sysLocation, buf);
	    save_into_conffile ("system location:", sysLocation);
	    break;
	}
    }
    return SNMP_ERR_NOERROR;
}

u_char *
var_orEntry(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;   /* IN - pointer to variable entry that points here */
    register oid	*name;	    /* IN/OUT - input name requested, output name found */
    register int	*length;    /* IN/OUT - length of input and output oid's */
    int			exact;	    /* IN - TRUE if an exact match was requested. */
    int			*var_len;   /* OUT - length of variable or 0 if function returned. */
    int			(**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    oid			newname[MAX_NAME_LEN];
    register int	orindex;
    int 		result, count;
    static struct orEntry {
	char   *descr;
	int	namelen;
	oid	name[32];
    } orEntries[] = {
#ifdef linux
	{"LINUX agent", 11, {1,3,6,1,4,1,1575,1,5,1,1}},
#else
	{"CMU agent", 9, {1,3,6,1,4,1,3,1,1}},
#endif
    };

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));

    count = sizeof(orEntries)/sizeof(struct orEntry);
    for(orindex = 1; orindex <= count; orindex++){
	newname[10] = (oid)orindex;
	result = compare(name, *length, newname, (int)vp->namelen + 1);
	if ((exact && (result == 0)) || (!exact && (result < 0)))
	    break;
    }
    if (orindex > count)
	return NULL;

    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;
    *write_method = 0;

    orindex--;
    switch (vp->magic){
	case 2:
		*var_len = orEntries[orindex].namelen * sizeof(oid);
		return (u_char *)orEntries[orindex].name;
	case 3:
	        *var_len = strlen(orEntries[orindex].descr);
		return (u_char *)orEntries[orindex].descr;
	case 4:
	    *var_len = sizeof(long);
#ifdef linux
	    /* the uptime from the agent is wanted: */
	    { struct timeval tv;
	      gettimeofday (&tv, (struct timezone *) 0);
	      long_return = (tv.tv_sec - uptime_stamp) * 100 
			+ tv.tv_usec / 10000;
	    }
#else
	    (u_long)long_return = sysUpTime();
#endif
	    return (u_char *)&long_return;
	default:
	    ERROR("");
    }
    return NULL;
}

u_char *
var_ifEntry(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;   /* IN - pointer to variable entry that points here */
    register oid	*name;	    /* IN/OUT - input name requested, output name found */
    register int	*length;    /* IN/OUT - length of input and output oid's */
    int			exact;	    /* IN - TRUE if an exact match was requested. */
    int			*var_len;   /* OUT - length of variable or 0 if function returned. */
    int			(**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    oid			newname[MAX_NAME_LEN];
    register int	interface;
    int result, count;
    static struct ifnet ifnet;
#if !defined(sunV3)
    static struct in_ifaddr in_ifaddr;
#endif sunV3
    static char Name[16];
    register char *cp;

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    /* find "next" interface */
    count = Interface_Scan_Get_Count();
    for(interface = 1; interface <= count; interface++){
	newname[10] = (oid)interface;
	result = compare(name, *length, newname, (int)vp->namelen + 1);
	if ((exact && (result == 0)) || (!exact && (result < 0)))
	    break;
    }
    if (interface > count)
	return NULL;

    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;
    *write_method = 0;
    *var_len = sizeof(long);

#ifdef sunV3
    Interface_Scan_By_Index(interface, Name, &ifnet);   
#else 
    Interface_Scan_By_Index(interface, Name, &ifnet, &in_ifaddr);
#endif
    switch (vp->magic){
	case IFINDEX:
	    long_return = interface;
	    return (u_char *) &long_return;
	case IFDESCR:
#define USE_NAME_AS_DESCRIPTION
#ifdef USE_NAME_AS_DESCRIPTION
	    cp = Name;
#else  USE_NAME_AS_DESCRIPTION
	    cp = Lookup_Device_Annotation(Name, "snmp-descr");
	    if (!cp)
		cp = Lookup_Device_Annotation(Name, 0);
	    if (!cp) cp = Name;
#endif USE_NAME_AS_DESCRIPTION
	    *var_len = strlen(cp);
	    return (u_char *)cp;
	case IFTYPE:
#if 0
	    cp = Lookup_Device_Annotation(Name, "snmp-type");
	    if (cp) long_return = atoi(cp);
	    else
#endif
#ifndef linux
		long_return = 1;	/* OTHER */
#else
	        long_return = ifnet.if_type;
#endif
	    return (u_char *) &long_return;
	case IFMTU: {
	    long_return = (long) ifnet.if_mtu;
	    return (u_char *) &long_return;
	}
	case IFSPEED:
#if 0
	    cp = Lookup_Device_Annotation(Name, "snmp-speed");
	    if (cp) long_return = atoi(cp);
	    else
#endif
#ifndef linux
	    (u_long)long_return = 1;	/* OTHER */
#else
	    long_return = ifnet.if_speed;
#endif
	    return (u_char *) &long_return;
	case IFPHYSADDRESS:
#if 0
	    if (Lookup_Device_Annotation(Name, "ethernet-device")) {
		Interface_Get_Ether_By_Index(interface, return_buf);
		*var_len = 6;
		return(u_char *) return_buf;
	    } else {
		long_return = 0;
		return (u_char *) long_return;
	    }
#endif
		Interface_Get_Ether_By_Index(interface, return_buf);
		*var_len = 6;
		return(u_char *) return_buf;
	case IFADMINSTATUS:
	    long_return = ifnet.if_flags & IFF_RUNNING ? 1 : 2;
	    return (u_char *) &long_return;
	case IFOPERSTATUS:
	    long_return = ifnet.if_flags & IFF_UP ? 1 : 2;
	    return (u_char *) &long_return;
	case IFLASTCHANGE:
	    long_return = 0; /* XXX */
	    return (u_char *) &long_return;
	case IFINOCTETS:
	    (u_long)long_return = ifnet.if_ipackets * 308; /* XXX */
	    return (u_char *) &long_return;
	case IFINUCASTPKTS:
	    (u_long)long_return = ifnet.if_ipackets;
	    return (u_char *) &long_return;
	case IFINNUCASTPKTS:
	    (u_long)long_return = 0; /* XXX */
	    return (u_char *) &long_return;
	case IFINDISCARDS:
	    (u_long)long_return = 0; /* XXX */
	    return (u_char *) &long_return;
	case IFINERRORS:
	    return (u_char *) &ifnet.if_ierrors;
	case IFINUNKNOWNPROTOS:
	    (u_long)long_return = 0; /* XXX */
	    return (u_char *) &long_return;
	case IFOUTOCTETS:
	    (u_long)long_return = ifnet.if_opackets * 308; /* XXX */
	    return (u_char *) &long_return;
	case IFOUTUCASTPKTS:
	    (u_long)long_return = ifnet.if_opackets;
	    return (u_char *) &long_return;
	case IFOUTNUCASTPKTS:
	    (u_long)long_return = 0; /* XXX */
	    return (u_char *) &long_return;
	case IFOUTDISCARDS:
	    return (u_char *) &ifnet.if_snd.ifq_drops;
	case IFOUTERRORS:
	    return (u_char *) &ifnet.if_oerrors;
	case IFOUTQLEN:
	    return (u_char *) &ifnet.if_snd.ifq_len;
#ifdef linux
	case IFSPECIFIC:
	    { static oid ifspecific_dont_know [] = {
		      1, 3, 6, 1, 2, 1, 2, 2, 1, 22, 0, 0 };
	      u_char *ptr = (u_char *) ifspecific_dont_know;
	      int plen = sizeof (ifspecific_dont_know);

	      *var_len = plen;
	      return (u_char *) ptr;
	    }
#endif
	default:
	    ERROR("");
    }
    return NULL;
}

/*
 * Read the ARP table
 */

u_char *
var_atEntry(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;	/* IN - pointer to variable entry that points here */
    register oid	    *name;	/* IN/OUT - input name requested, output name found */
    register int	    *length;	/* IN/OUT - length of input and output oid's */
    int			    exact;	/* IN - TRUE if an exact match was requested. */
    int			    *var_len;	/* OUT - length of variable or 0 if function returned. */
    int			    (**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    /*
     * object identifier is of form:
     * 1.3.6.1.2.1.3.1.1.1.interface.1.A.B.C.D,  where A.B.C.D is IP address.
     * Interface is at offset 10,
     * IPADDR starts at offset 12.
     */
    u_char		    *cp;
    oid			    *op;
    oid			    lowest[16];
    oid			    current[16];
    static char		    PhysAddr[6], LowPhysAddr[6];
    u_long		    Addr, LowAddr;

    /* fill in object part of name for current (less sizeof instance part) */

    bcopy((char *)vp->name, (char *)current, (int)vp->namelen * sizeof(oid));

    LowAddr = -1;      /* Don't have one yet */
    ARP_Scan_Init();
    for (;;) {
#ifdef linux
	if (ARP_Scan_Next(&Addr, PhysAddr, (int *) 0) == 0) break;
	current [10] = get_ether_iface_index ();
#else
	if (ARP_Scan_Next(&Addr, PhysAddr) == 0) break;
	current[10] = 1;	/* IfIndex == 1 (ethernet???) XXX */
#endif
	current[11] = 1;
	cp = (u_char *)&Addr;
	op = current + 12;
	*op++ = *cp++;
	*op++ = *cp++;
	*op++ = *cp++;
	*op++ = *cp++;

	if (exact){
	    if (compare(current, 16, name, *length) == 0){
		bcopy((char *)current, (char *)lowest, 16 * sizeof(oid));
		LowAddr = Addr;
		bcopy(PhysAddr, LowPhysAddr, sizeof(PhysAddr));
		break;	/* no need to search further */
	    }
	} else {
	    if ((compare(current, 16, name, *length) > 0) &&
		 ((LowAddr == -1) || (compare(current, 16, lowest, 16) < 0))){
		/*
		 * if new one is greater than input and closer to input than
		 * previous lowest, save this one as the "next" one.
		 */
		bcopy((char *)current, (char *)lowest, 16 * sizeof(oid));
		LowAddr = Addr;
		bcopy(PhysAddr, LowPhysAddr, sizeof(PhysAddr));
	    }
	}
    }
    if (LowAddr == -1) return(NULL);

    bcopy((char *)lowest, (char *)name, 16 * sizeof(oid));
    *length = 16;
    *write_method = 0;
    switch(vp->magic){
	case ATIFINDEX:
	    *var_len = sizeof long_return;
#ifndef linux
	    long_return = 1; /* XXX */
#else
	    long_return = get_ether_iface_index ();
#endif
	    return (u_char *)&long_return;
	case ATPHYSADDRESS:
	    *var_len = sizeof(LowPhysAddr);
	    return (u_char *)LowPhysAddr;
	case ATNETADDRESS:
	    *var_len = sizeof long_return;
	    long_return = LowAddr;
	    return (u_char *)&long_return;
	default:
	    ERROR("");
   }
   return NULL;
}



#ifdef linux


/*
 * Read the ARP table; this is nearly the same as var_atEntry()
 */

static u_char *
var_ntomEntry(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;	/* IN - pointer to variable entry that points here */
    register oid	    *name;	/* IN/OUT - input name requested, output name found */
    register int	    *length;	/* IN/OUT - length of input and output oid's */
    int			    exact;	/* IN - TRUE if an exact match was requested. */
    int			    *var_len;	/* OUT - length of variable or 0 if function returned. */
    int			    (**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    /*
     * object identifier is of form:
     * 1.3.6.1.2.1.4.22.1.{1,2,3,4}.interface.A.B.C.D
     * Interface is at offset 10,
     * IPADDR starts at offset 11.
     */
    u_char		    *cp;
    oid			    *op;
    oid			    lowest[15];
    oid			    current[15];
    static char		    PhysAddr[6], LowPhysAddr[6];  /* XXX: fix me */
    u_long		    Addr, LowAddr;
#ifdef linux
    int			    Flags, LowFlags = 0;
#endif

    /* fill in object part of name for current (less sizeof instance part) */
    bcopy((char *)vp->name, (char *)current, (int)vp->namelen * sizeof(oid));

    LowAddr = -1;      /* Don't have one yet */
    ARP_Scan_Init();
    for (;;) {
#ifdef linux
	if (ARP_Scan_Next(&Addr, PhysAddr, &Flags) == 0) break;
#else
	if (ARP_Scan_Next(&Addr, PhysAddr) == 0) break;
#endif
	current [10] = get_ether_iface_index ();
	cp = (u_char *) &Addr;
	op = current + 11;
	*op++ = *cp++, 	*op++ = *cp++;
	*op++ = *cp++, 	*op++ = *cp++;

	if (exact){
	    if (compare(current, 15, name, *length) == 0){
		bcopy((char *)current, (char *)lowest, 15 * sizeof(oid));
		LowAddr = Addr;
		bcopy(PhysAddr, LowPhysAddr, sizeof(PhysAddr));
#ifdef linux
		LowFlags = Flags;
#endif
		break;	/* no need to search further */
	    }
	} else {
	    if ((compare(current, 15, name, *length) > 0) &&
		 ((LowAddr == -1) || (compare(current, 15, lowest, 15) < 0))){
		/*
		 * if new one is greater than input and closer to input than
		 * previous lowest, save this one as the "next" one.
		 */
		bcopy((char *)current, (char *)lowest, 15 * sizeof(oid));
		LowAddr = Addr;
		bcopy(PhysAddr, LowPhysAddr, sizeof(PhysAddr));
#ifdef linux
		LowFlags = Flags;
#endif
	    }
	}
    }
    if (LowAddr == -1) return(NULL);

    bcopy((char *)lowest, (char *)name, 15 * sizeof(oid));
    *length = 15;
    *write_method = 0;

    switch(vp->magic){
    case IPNETTOMEDIAIFINDEX: 
      *var_len = sizeof long_return;
      long_return = get_ether_iface_index ();
      return (u_char *)&long_return;
    case IPNETTOMEDIAPHYSADDR:
      *var_len = sizeof(LowPhysAddr);
      return (u_char *)LowPhysAddr;
    case IPNETTOMEDIANETADDR: 
      *var_len = sizeof long_return;
      long_return = LowAddr;
      return (u_char *)&long_return;
    case IPNETTOMEDIATYPE:
      *var_len = sizeof long_return;
#ifdef linux
      /* 1 == other, 2 == invalid, 3 == dynamic, 4 == static */
      long_return = LowFlags & ATF_PERM ? 4 : 
		(! LowFlags ? 3 : 1);
#else
      long_return = 3;	 		/* dynamic */
#endif
      return (u_char *)&long_return;
    default:
      ERROR("");
   }
   return NULL;
}

#endif


#ifdef linux
/*
 * lucky days. since 1.1.16 the ip statistics are avail by the proc
 * file-system.
 */

static void
linux_read_ip_stat (ipstat)
struct ip_mib *ipstat;
{
  FILE *in = fopen ("/proc/net/snmp", "r");
  char line [1024];

  bzero ((char *) ipstat, sizeof (*ipstat));

  if (! in)
    return;

  while (line == fgets (line, 1024, in))
    {
      if (19 == sscanf (line,   
"Ip: %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu",
     &ipstat->IpForwarding, &ipstat->IpDefaultTTL, &ipstat->IpInReceives, 
     &ipstat->IpInHdrErrors, &ipstat->IpInAddrErrors, &ipstat->IpForwDatagrams, 
     &ipstat->IpInUnknownProtos, &ipstat->IpInDiscards, &ipstat->IpInDelivers, 
     &ipstat->IpOutRequests, &ipstat->IpOutDiscards, &ipstat->IpOutNoRoutes, 
     &ipstat->IpReasmTimeout, &ipstat->IpReasmReqds, &ipstat->IpReasmOKs, 
     &ipstat->IpReasmFails, &ipstat->IpFragOKs, &ipstat->IpFragFails, 
     &ipstat->IpFragCreates))
	break;
    }
  fclose (in);
}
#endif /* linux */




u_char *
var_ip(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;   /* IN - pointer to variable entry that points here */
    oid     *name;	    /* IN/OUT - input name requested, output name found */
    int     *length;	    /* IN/OUT - length of input and output oid's */
    int     exact;	    /* IN - TRUE if an exact match was requested. */
    int     *var_len;	    /* OUT - length of variable or 0 if function returned. */
    int     (**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
#ifndef linux
    static struct ipstat ipstat;
#else
    static struct ip_mib ipstat;
#endif
    oid newname[MAX_NAME_LEN];
    int result;

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    newname[8] = 0;
    result = compare(name, *length, newname, (int)vp->namelen + 1);
    if ((exact && (result != 0)) || (!exact && (result >= 0)))
	return NULL;
    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;

    *write_method = 0;
    *var_len = sizeof(long);	/* default length */
    /*
     *	Get the IP statistics from the kernel...
     */

#ifndef linux
    KNLookup(N_IPSTAT, (char *)&ipstat, sizeof (ipstat));

    switch (vp->magic){
	case IPFORWARDING:
	    KNLookup( N_IPFORWARDING, (char *) &i, sizeof(i));
	    fflush(stderr);
	    if (i==1) {
		long_return = 1;		/* GATEWAY */
	    } else {
		long_return = 2;	    /* HOST    */
	    }
	    return (u_char *) &long_return;
	case IPDEFAULTTTL:
	    /*
	     *	Allow for a kernel w/o TCP.
	     */
	    if (nl[N_TCP_TTL].n_value) {
		KNLookup( N_TCP_TTL, (char *) &long_return, sizeof(long_return));
	    } else long_return = 60;	    /* XXX */
	    return (u_char *) &long_return;
	case IPINRECEIVES:
	    return (u_char *) &ipstat.ips_total;
	case IPINHDRERRORS:
	    long_return = ipstat.ips_badsum + ipstat.ips_tooshort +
			  ipstat.ips_toosmall + ipstat.ips_badhlen +
			  ipstat.ips_badlen;
	    return (u_char *) &long_return;
	case IPINADDRERRORS:
	    return (u_char *) &ipstat.ips_cantforward;

	case IPFORWDATAGRAMS:
	    return (u_char *) &ipstat.ips_forward;

	case IPINUNKNOWNPROTOS:
	    long_return = 0;
	    return (u_char *) &long_return;
	case IPINDISCARDS:
	    long_return = 0;
	    return (u_char *) &long_return;
	case IPINDELIVERS:

	    long_return = ipstat.ips_total -
			 (ipstat.ips_badsum + ipstat.ips_tooshort +
			  ipstat.ips_toosmall + ipstat.ips_badhlen +
			  ipstat.ips_badlen);
	    return (u_char *) &long_return;

	case IPOUTREQUESTS:
	    long_return = 0;
	    return (u_char *) &long_return;
	case IPOUTDISCARDS:
	    long_return = 0;
	    return (u_char *) &long_return;
	case IPOUTNOROUTES:
	    return (u_char *) &ipstat.ips_cantforward;

	case IPREASMTIMEOUT:
	    long_return = IPFRAGTTL;
	    return (u_char *) &long_return;
	case IPREASMREQDS:
	    return (u_char *) &ipstat.ips_fragments;

	case IPREASMOKS:
	    return (u_char *) &ipstat.ips_fragments;

	case IPREASMFAILS:
	    long_return = ipstat.ips_fragdropped + ipstat.ips_fragtimeout;
	    return (u_char *) &long_return;

	case IPFRAGOKS:
	    long_return = 0;
	    return (u_char *) &long_return;
	case IPFRAGFAILS:
	    long_return = 0;
	    return (u_char *) &long_return;
	case IPFRAGCREATES:
	    long_return = 0;
	    return (u_char *) &long_return;
	default:
	    ERROR("");
    }

#else /* linux */

    linux_read_ip_stat (&ipstat);

    switch (vp->magic){
	case IPFORWARDING: 
		/* valid values are 1 == yup, 2 == nope:
		 * a 0 is forbidden, so patch: */
		if (! ipstat.IpForwarding)
			ipstat.IpForwarding = 2;
		return (u_char *) &ipstat.IpForwarding;
	case IPDEFAULTTTL: return (u_char *) &ipstat.IpDefaultTTL;
	case IPINRECEIVES: return (u_char *) &ipstat.IpInReceives;
	case IPINHDRERRORS: return (u_char *) &ipstat.IpInHdrErrors;
	case IPINADDRERRORS: return (u_char *) &ipstat.IpInAddrErrors;
	case IPFORWDATAGRAMS: return (u_char *) &ipstat.IpForwDatagrams;
	case IPINUNKNOWNPROTOS: return (u_char *) &ipstat.IpInUnknownProtos;
	case IPINDISCARDS: return (u_char *) &ipstat.IpInDiscards;
	case IPINDELIVERS: return (u_char *) &ipstat.IpInDelivers;
	case IPOUTREQUESTS: return (u_char *) &ipstat.IpOutRequests;
	case IPOUTDISCARDS: return (u_char *) &ipstat.IpOutDiscards;
	case IPOUTNOROUTES: return (u_char *) &ipstat.IpOutNoRoutes;
	case IPREASMTIMEOUT: return (u_char *) &ipstat.IpReasmTimeout;
	case IPREASMREQDS: return (u_char *) &ipstat.IpReasmReqds;
	case IPREASMOKS: return (u_char *) &ipstat.IpReasmOKs;
	case IPREASMFAILS: return (u_char *) &ipstat.IpReasmFails;
	case IPFRAGOKS: return (u_char *) &ipstat.IpFragOKs;
	case IPFRAGFAILS: return (u_char *) &ipstat.IpFragFails;
	case IPFRAGCREATES: return (u_char *) &ipstat.IpFragCreates;
	default:
	    ERROR("");
    }
#endif /* linux */

    return NULL;
}



u_char *
var_ipAddrEntry(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;    /* IN - pointer to variable entry that points here */
    register oid	*name;	    /* IN/OUT - input name requested, output name found */
    register int	*length;    /* IN/OUT - length of input and output oid's */
    int			exact;	    /* IN - TRUE if an exact match was requested. */
    int			*var_len;   /* OUT - length of variable or 0 if function returned. */
    int			(**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    /*
     * object identifier is of form:
     * 1.3.6.1.2.1.4.20.1.?.A.B.C.D,  where A.B.C.D is IP address.
     * IPADDR starts at offset 10.
     */
    oid			    lowest[14];
    oid			    current[14], *op;
    u_char		    *cp;
    int			    interface, lowinterface=0;
    static struct ifnet ifnet, lowin_ifnet;
#ifndef sunV3
    static struct in_ifaddr in_ifaddr, lowin_ifaddr;
#endif sunV3

    /* fill in object part of name for current (less sizeof instance part) */

    bcopy((char *)vp->name, (char *)current, (int)vp->namelen * sizeof(oid));

    Interface_Scan_Init();
    for (;;) {

#ifdef sunV3
	if (Interface_Scan_Next(&interface, (char *)0, &ifnet) == 0) break;
	cp = (u_char *)&(((struct sockaddr_in *) &(ifnet.if_addr))->sin_addr.s_addr);
#else
	if (Interface_Scan_Next(&interface, (char *)0, &ifnet, &in_ifaddr) == 0) break;
	cp = (u_char *)&(((struct sockaddr_in *) &(in_ifaddr.ia_addr))->sin_addr.s_addr);
#endif

	op = current + 10;
	*op++ = *cp++;
	*op++ = *cp++;
	*op++ = *cp++;
	*op++ = *cp++;
	if (exact){
	    if (compare(current, 14, name, *length) == 0){
		bcopy((char *)current, (char *)lowest, 14 * sizeof(oid));
		lowinterface = interface;
#ifdef sunV3
		lowin_ifnet = ifnet;
#else
		lowin_ifaddr = in_ifaddr;
#endif
		break;	/* no need to search further */
	    }
	} else {
	    if ((compare(current, 14, name, *length) > 0) &&
		 (!lowinterface || (compare(current, 14, lowest, 14) < 0))){
		/*
		 * if new one is greater than input and closer to input than
		 * previous lowest, save this one as the "next" one.
		 */
		lowinterface = interface;
#ifdef sunV3
		lowin_ifnet = ifnet;
#else
		lowin_ifaddr = in_ifaddr;
#endif
		bcopy((char *)current, (char *)lowest, 14 * sizeof(oid));
	    }
	}
    }

    if (!lowinterface) return(NULL);
    bcopy((char *)lowest, (char *)name, 14 * sizeof(oid));
    *length = 14;
    *write_method = 0;
    *var_len = sizeof(long_return);
    switch(vp->magic){
	case IPADADDR:
#ifdef sunV3
            return(u_char *) &((struct sockaddr_in *) &lowin_ifnet.if_addr)->sin_addr.s_addr;
#else
	    return(u_char *) &((struct sockaddr_in *) &lowin_ifaddr.ia_addr)->sin_addr.s_addr;
#endif
	case IPADIFINDEX:
	    long_return = lowinterface;
	    return(u_char *) &long_return;
	case IPADNETMASK:
#ifndef sunV3
	    long_return = ntohl(lowin_ifaddr.ia_subnetmask);
#endif
#ifdef linux
	    return(u_char *) &((struct sockaddr_in *) 
			       &lowin_ifnet.ia_subnetmask)->sin_addr.s_addr;
#else
	    return(u_char *) &long_return;
#endif
	case IPADBCASTADDR:
	    
#ifdef sunV3
	    long_return = ntohl(((struct sockaddr_in *) &lowin_ifnet.ifu_broadaddr)->sin_addr.s_addr) & 1;
#else
	    long_return = ntohl(((struct sockaddr_in *) &lowin_ifaddr.ia_addr)->sin_addr.s_addr) & 1;
#endif
	    return(u_char *) &long_return;	   
#ifdef linux
	  case IPADENTREASMMAXSIZE:
	    /* XXX: fix me */
	    long_return = 20480; 
	    return (u_char *) &long_return;
#endif
	default:
	    ERROR("");
    }
    return NULL;
}


#ifdef linux
/*
 * lucky days. since 1.1.16 the icmp statistics are avail by the proc
 * file-system.
 */

static void
linux_read_icmp_stat (icmpstat)
struct icmp_mib *icmpstat;
{
  FILE *in = fopen ("/proc/net/snmp", "r");
  char line [1024];

  bzero ((char *) icmpstat, sizeof (*icmpstat));

  if (! in)
    return;

  while (line == fgets (line, 1024, in))
    {
      if (26 == sscanf (line,
"Icmp: %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu\n",
   &icmpstat->IcmpInMsgs, &icmpstat->IcmpInErrors, &icmpstat->IcmpInDestUnreachs, 
   &icmpstat->IcmpInTimeExcds, &icmpstat->IcmpInParmProbs, &icmpstat->IcmpInSrcQuenchs,
   &icmpstat->IcmpInRedirects, &icmpstat->IcmpInEchos, &icmpstat->IcmpInEchoReps, 
   &icmpstat->IcmpInTimestamps, &icmpstat->IcmpInTimestampReps, &icmpstat->IcmpInAddrMasks,
   &icmpstat->IcmpInAddrMaskReps, &icmpstat->IcmpOutMsgs, &icmpstat->IcmpOutErrors,
   &icmpstat->IcmpOutDestUnreachs, &icmpstat->IcmpOutTimeExcds, 
   &icmpstat->IcmpOutParmProbs, &icmpstat->IcmpOutSrcQuenchs, &icmpstat->IcmpOutRedirects,
   &icmpstat->IcmpOutEchos, &icmpstat->IcmpOutEchoReps, &icmpstat->IcmpOutTimestamps, 
   &icmpstat->IcmpOutTimestampReps, &icmpstat->IcmpOutAddrMasks,
   &icmpstat->IcmpOutAddrMaskReps))
	break;
    }
  fclose (in);
}

#endif /* linux */


u_char *
var_icmp(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;    /* IN - pointer to variable entry that points here */
    oid     *name;	    /* IN/OUT - input name requested, output name found */
    int     *length;	    /* IN/OUT - length of input and output oid's */
    int     exact;	    /* IN - TRUE if an exact match was requested. */
    int     *var_len;	    /* OUT - length of variable or 0 if function returned. */
    int     (**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
#ifndef linux
    static struct icmpstat icmpstat;
#else
    static struct icmp_mib icmpstat;
#endif
    oid newname[MAX_NAME_LEN];
    int result;
#ifndef linux
    register int i;
#endif

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    newname[8] = 0;
    result = compare(name, *length, newname, (int)vp->namelen + 1);
    if ((exact && (result != 0)) || (!exact && (result >= 0)))
        return NULL;
    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;

    *write_method = 0;
    *var_len = sizeof(long); /* all following variables are sizeof long */

    /*
     *	Get the ICMP statistics from the kernel...
     */

#ifndef linux
    KNLookup( N_ICMPSTAT, (char *)&icmpstat, sizeof (icmpstat));

    switch (vp->magic){
	case ICMPINMSGS:
	    long_return = icmpstat.icps_badcode + icmpstat.icps_tooshort +
			  icmpstat.icps_checksum + icmpstat.icps_badlen;
	    for (i=0; i <= ICMP_MAXTYPE; i++)
		long_return += icmpstat.icps_inhist[i];
	    return (u_char *)&long_return;
	case ICMPINERRORS:
	    long_return = icmpstat.icps_badcode + icmpstat.icps_tooshort +
			  icmpstat.icps_checksum + icmpstat.icps_badlen;
	    return (u_char *)&long_return;
	case ICMPINDESTUNREACHS:
	    return (u_char *) &icmpstat.icps_inhist[ICMP_UNREACH];
	case ICMPINTIMEEXCDS:
	    return (u_char *) &icmpstat.icps_inhist[ICMP_TIMXCEED];
	case ICMPINPARMPROBS:
	    return (u_char *) &icmpstat.icps_inhist[ICMP_PARAMPROB];
	case ICMPINSRCQUENCHS:
	    return (u_char *) &icmpstat.icps_inhist[ICMP_SOURCEQUENCH];
	case ICMPINREDIRECTS:
	    return (u_char *) &icmpstat.icps_inhist[ICMP_REDIRECT];
	case ICMPINECHOS:
	    return (u_char *) &icmpstat.icps_inhist[ICMP_ECHO];
	case ICMPINECHOREPS:
	    return (u_char *) &icmpstat.icps_inhist[ICMP_ECHOREPLY];
	case ICMPINTIMESTAMPS:
	    return (u_char *) &icmpstat.icps_inhist[ICMP_TSTAMP];
	case ICMPINTIMESTAMPREPS:
	    return (u_char *) &icmpstat.icps_inhist[ICMP_TSTAMPREPLY];
	case ICMPINADDRMASKS:
	    return (u_char *) &icmpstat.icps_inhist[ICMP_MASKREQ];
	case ICMPINADDRMASKREPS:
	    return (u_char *) &icmpstat.icps_inhist[ICMP_MASKREPLY];
	case ICMPOUTMSGS:
	    long_return = icmpstat.icps_oldshort + icmpstat.icps_oldicmp;
	    for (i=0; i <= ICMP_MAXTYPE; i++)
		long_return += icmpstat.icps_outhist[i];
	    return (u_char *)&long_return;
	case ICMPOUTERRORS:
	    long_return = icmpstat.icps_oldshort + icmpstat.icps_oldicmp;
	    return (u_char *)&long_return;
	case ICMPOUTDESTUNREACHS:
	    return (u_char *) &icmpstat.icps_outhist[ICMP_UNREACH];
	case ICMPOUTTIMEEXCDS:
	    return (u_char *) &icmpstat.icps_outhist[ICMP_TIMXCEED];
	case ICMPOUTPARMPROBS:
	    return (u_char *) &icmpstat.icps_outhist[ICMP_PARAMPROB];
	case ICMPOUTSRCQUENCHS:
	    return (u_char *) &icmpstat.icps_outhist[ICMP_SOURCEQUENCH];
	case ICMPOUTREDIRECTS:
	    return (u_char *) &icmpstat.icps_outhist[ICMP_REDIRECT];
	case ICMPOUTECHOS:
	    return (u_char *) &icmpstat.icps_outhist[ICMP_ECHO];
	case ICMPOUTECHOREPS:
	    return (u_char *) &icmpstat.icps_outhist[ICMP_ECHOREPLY];
	case ICMPOUTTIMESTAMPS:
	    return (u_char *) &icmpstat.icps_outhist[ICMP_TSTAMP];
	case ICMPOUTTIMESTAMPREPS:
	    return (u_char *) &icmpstat.icps_outhist[ICMP_TSTAMPREPLY];
	case ICMPOUTADDRMASKS:
	    return (u_char *) &icmpstat.icps_outhist[ICMP_MASKREQ];
	case ICMPOUTADDRMASKREPS:
	    return (u_char *) &icmpstat.icps_outhist[ICMP_MASKREPLY];
	default:
	    ERROR("");
    }
#else /* linux */

    linux_read_icmp_stat (&icmpstat);

    switch (vp->magic){
    case ICMPINMSGS: return (u_char *) &icmpstat.IcmpInMsgs;
    case ICMPINERRORS: return (u_char *) &icmpstat.IcmpInErrors;
    case ICMPINDESTUNREACHS: return (u_char *) &icmpstat.IcmpInDestUnreachs;
    case ICMPINTIMEEXCDS: return (u_char *) &icmpstat.IcmpInTimeExcds;
    case ICMPINPARMPROBS: return (u_char *) &icmpstat.IcmpInParmProbs;
    case ICMPINSRCQUENCHS: return (u_char *) &icmpstat.IcmpInSrcQuenchs;
    case ICMPINREDIRECTS: return (u_char *) &icmpstat.IcmpInRedirects;
    case ICMPINECHOS: return (u_char *) &icmpstat.IcmpInEchos;
    case ICMPINECHOREPS: return (u_char *) &icmpstat.IcmpInEchoReps;
    case ICMPINTIMESTAMPS: return (u_char *) &icmpstat.IcmpInTimestamps;
    case ICMPINTIMESTAMPREPS: return (u_char *) &icmpstat.IcmpInTimestampReps;
    case ICMPINADDRMASKS: return (u_char *) &icmpstat.IcmpInAddrMasks;
    case ICMPINADDRMASKREPS: return (u_char *) &icmpstat.IcmpInAddrMaskReps;
    case ICMPOUTMSGS: return (u_char *) &icmpstat.IcmpOutMsgs;
    case ICMPOUTERRORS: return (u_char *) &icmpstat.IcmpOutErrors;
    case ICMPOUTDESTUNREACHS: return (u_char *) &icmpstat.IcmpOutDestUnreachs;
    case ICMPOUTTIMEEXCDS: return (u_char *) &icmpstat.IcmpOutTimeExcds;
    case ICMPOUTPARMPROBS: return (u_char *) &icmpstat.IcmpOutParmProbs;
    case ICMPOUTSRCQUENCHS: return (u_char *) &icmpstat.IcmpOutSrcQuenchs;
    case ICMPOUTREDIRECTS: return (u_char *) &icmpstat.IcmpOutRedirects;
    case ICMPOUTECHOS: return (u_char *) &icmpstat.IcmpOutEchos;
    case ICMPOUTECHOREPS: return (u_char *) &icmpstat.IcmpOutEchoReps;
    case ICMPOUTTIMESTAMPS: return (u_char *) &icmpstat.IcmpOutTimestamps;
    case ICMPOUTTIMESTAMPREPS: return (u_char *)&icmpstat.IcmpOutTimestampReps;
    case ICMPOUTADDRMASKS: return (u_char *) &icmpstat.IcmpOutAddrMasks;
    case ICMPOUTADDRMASKREPS: return (u_char *) &icmpstat.IcmpOutAddrMaskReps;

    default:
      ERROR("");
    }
#endif /* linux */

    return NULL;
}


#ifdef linux
/*
 * lucky days. since 1.1.16 the udp statistics are avail by the proc
 * file-system.
 */

static void
linux_read_udp_stat (udpstat)
struct udp_mib *udpstat;
{
  FILE *in = fopen ("/proc/net/snmp", "r");
  char line [1024];

  bzero ((char *) udpstat, sizeof (*udpstat));

  if (! in)
    return;

  while (line == fgets (line, 1024, in))
    {
      if (4 == sscanf (line, "Udp: %lu %lu %lu %lu\n",
			&udpstat->UdpInDatagrams, &udpstat->UdpNoPorts,
			&udpstat->UdpInErrors, &udpstat->UdpOutDatagrams))
	break;
    }
  fclose (in);
}

#endif /* linux */




u_char *
var_udp(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;    /* IN - pointer to variable entry that points here */
    oid     *name;	    /* IN/OUT - input name requested, output name found */
    int     *length;	    /* IN/OUT - length of input and output oid's */
    int     exact;	    /* IN - TRUE if an exact match was requested. */
    int     *var_len;	    /* OUT - length of variable or 0 if function returned. */
    int     (**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
#ifndef linux
    static struct udpstat udpstat;
#else
    static struct udp_mib udpstat;
#endif
    oid newname[MAX_NAME_LEN];
    int result;
#ifdef linux
    oid lowest[MAX_NAME_LEN], *op;
    u_char *cp;
    int i, State, LowState;
    static struct inpcb inpcb, Lowinpcb;

    if (vp->magic <= UDPOUTDATAGRAMS) {
#endif

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    newname[8] = 0;
    result = compare(name, *length, newname, (int)vp->namelen + 1);
    if ((exact && (result != 0)) || (!exact && (result >= 0)))
        return NULL;
    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;

    *write_method = 0;
    *var_len = sizeof(long);	/* default length */
    /*
     *	Get the IP statistics from the kernel...
     */

#ifndef linux
    KNLookup( N_UDPSTAT, (char *)&udpstat, sizeof (udpstat));
#else
    linux_read_udp_stat (&udpstat);
#endif

    switch (vp->magic){
	case UDPINDATAGRAMS:
#ifdef linux
            return (u_char *) &udpstat.UdpInDatagrams;
#endif
	case UDPNOPORTS:
#ifdef linux
            return (u_char *) &udpstat.UdpNoPorts;
#endif
	case UDPOUTDATAGRAMS:
#ifndef linux
	    long_return = 0;
#else
      	    return (u_char *) &udpstat.UdpOutDatagrams;
#endif
	    return (u_char *) &long_return;
	case UDPINERRORS:
#ifndef linux
	    long_return = udpstat.udps_hdrops + udpstat.udps_badsum +
			  udpstat.udps_badlen;
	    return (u_char *) &long_return;
#else
      	    return (u_char *) &udpstat.UdpInErrors;
#endif
	default:
	    ERROR("");
    }


#ifdef linux
    /*
     * cloned from var_tcp(): return udp listener:
     */

    } else {	/* Info about a particular connection */
      bcopy ((char *) vp->name, (char *) newname, 
	     (int) vp->namelen * sizeof (oid));

	/* find "next" listener */
Again:
LowState = -1;	    /* Don't have one yet */
	UDP_Scan_Init();

	for (;;) 
	  {
	    if ((i = UDP_Scan_Next (&State, &inpcb)) < 0) 
	      goto Again;
	    if (i == 0) 
	      break;	    /* Done */
	    cp = (u_char *) &inpcb.inp_laddr.s_addr;
	    op = newname + 10;
	    *op++ = *cp++,  *op++ = *cp++;
	    *op++ = *cp++,  *op++ = *cp++;
	    newname[14] = ntohs (inpcb.inp_lport);
#if 0
	    cp = (u_char *) &inpcb.inp_faddr.s_addr;
	    op = newname + 15;
	    *op++ = *cp++,  *op++ = *cp++;
	    *op++ = *cp++,  *op++ = *cp++;
	    newname[19] = ntohs(inpcb.inp_fport);
#endif
	    if (exact){
		if (compare(newname, 15, name, *length) == 0){
		    bcopy((char *)newname, (char *)lowest, 15 * sizeof(oid));
		    LowState = State;
		    Lowinpcb = inpcb;
		    break;  /* no need to search further */
		}
	    } else {
		if ((compare(newname, 15, name, *length) > 0) &&
		     ((LowState < 0) || (compare(newname, 15, lowest, 15) < 0))){
		    /*
		     * if new one is greater than input and closer to input
		     * than previous lowest, save this one as the "next" one.
		     */
		    bcopy((char *)newname, (char *)lowest, 15 * sizeof(oid));
		    LowState = State;
		    Lowinpcb = inpcb;
		}
	    }
	}

	if (LowState < 0) return(NULL);
	bcopy ((char *) lowest, (char *) name, 
		       ((int) vp->namelen + 5) * sizeof(oid));

	*length = vp->namelen + 5;
	*write_method = 0;
	*var_len = sizeof(long);

	switch (vp->magic) {
	    case UDPLOCALADDRESS:
		return (u_char *) &Lowinpcb.inp_laddr.s_addr;
	    case UDPLOCALPORT:
		long_return = ntohs (Lowinpcb.inp_lport);
		return (u_char *) &long_return;
	}
    }

#endif /* ! linux */


    return NULL;
}



#ifdef linux


/*
 * only for snmpEnableAuthenTraps:
 */

static int
write_snmp (action, var_val, var_val_type, var_val_len, statP, name, name_len)
   int      action;
   u_char   *var_val;
   u_char   var_val_type;
   int      var_val_len;
   u_char   *statP;
   oid      *name;
   int      name_len;
{
    int bigsize = 4, intval;

    if (var_val_type != INTEGER){
	ERROR("not integer");
	return SNMP_ERR_WRONGTYPE;
    }

    asn_parse_int(var_val, &bigsize, &var_val_type, &intval, sizeof (intval));
    if (intval != 1 && intval != 2) {
#ifdef DEBUG	    
	printf("not valid %x\n", intval);
#endif
	return SNMP_ERR_WRONGVALUE;
    }

    if (action == COMMIT) {
	snmp_enableauthentraps = intval;	
	save_into_conffile ("authentraps:", intval == 1 ? "yes" : "no");
    }
    return SNMP_ERR_NOERROR;
}


u_char *
var_snmp(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;    /* IN - pointer to variable entry that points here */
    oid     *name;	    /* IN/OUT - input name requested, output name found */
    int     *length;	    /* IN/OUT - length of input and output oid's */
    int     exact;	    /* IN - TRUE if an exact match was requested. */
    int     *var_len;	    /* OUT - length of variable or 0 if function returned. */
    int     (**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    oid newname[MAX_NAME_LEN];
    int result;
    extern u_long snmpStats [SNMP_LAST_STAT + 1];

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    newname[8] = 0;
    result = compare(name, *length, newname, (int)vp->namelen + 1);
    if ((exact && (result != 0)) || (!exact && (result >= 0)))
        return NULL;
    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;

    *write_method = 0;
    *var_len = sizeof(long);	/* default length */

    /* default value: */
    long_return = 0;

    switch (vp->magic){
	case SNMPINPKTS:
	    long_return = snmp_inpkts;
      	    break;
	case SNMPOUTPKTS:
	    long_return = snmp_outpkts;
      	    break;
	case SNMPINBADVERSIONS:
	    long_return = snmp_inbadversions;
      	    break;
	case SNMPINBADCOMMUNITYNAMES:
	    long_return = snmp_inbadcommunitynames
	      + snmpStats [SNMP_STAT_V1_BAD_COMMUNITY_NAMES];
      	    break;
	case SNMPINBADCOMMUNITYUSES:
	    long_return = snmpStats [SNMP_STAT_V1_BAD_COMMUNITY_USES];
      	    break;
	case SNMPINASNPARSEERRORS:
	    long_return = snmp_inasnparseerrors;
      	    break;
	case SNMPINTOOBIGS:
	    long_return = snmp_intoobigs;
      	    break;
	case SNMPINNOSUCHNAMES:
      	    break;
	case SNMPINBADVALUES:
	    long_return = snmp_inbadvalues;
      	    break;
	case SNMPINREADONLYS:
	    long_return = snmp_inreadonlys;
      	    break;
	case SNMPINGENERRS:
	    long_return = snmp_ingenerrs;
      	    break;
	case SNMPINTOTALREQVARS:
	    long_return = snmp_intotalreqvars;
      	    break;
	case SNMPINTOTALSETVARS:
      	    break;
	case SNMPINGETREQUESTS:
	    long_return = snmp_ingetrequests;
      	    break;
	case SNMPINGETNEXTS:
	    long_return = snmp_ingetnexts;
      	    break;
	case SNMPINSETREQUESTS:
	    long_return = snmp_insetrequests;
      	    break;
	case SNMPINGETRESPONSES:
      	    break;
	case SNMPINTRAPS:
      	    break;
	case SNMPOUTTOOBIGS:
      	    break;
	case SNMPOUTNOSUCHNAMES:
	    long_return = snmp_outnosuchnames;
      	    break;
	case SNMPOUTBADVALUES:
      	    break;
	case SNMPOUTGENERRS:
      	    break;
	case SNMPOUTGETREQUESTS:
      	    break;
	case SNMPOUTGETNEXTS:
      	    break;
	case SNMPOUTSETREQUESTS:
      	    break;
	case SNMPOUTGETRESPONSES:
	    long_return = snmp_outgetresponses;
      	    break;
	case SNMPOUTTRAPS:
      	    break;
	case SNMPENABLEAUTHENTRAPS:
	    *write_method = write_snmp;
	    long_return = snmp_enableauthentraps;
      	    break;
	default:
	    ERROR("unknown snmp var");
	    return NULL;
    }

    return (u_char *) &long_return;
}


/* 
 * try to find the boot-device; just a guess -- return root filesystem:
 */

static unsigned int
hr_find_initial_load_dev ()
{
    FILE *in = fopen ("/etc/mtab", "r");
    char line [512];
    char fs_path [128], mnt_path [128];
    struct stat stbuf;

    while (in && fgets (line, sizeof (line), in) 
	   && 2 == sscanf (line, "%s %s", fs_path, mnt_path))
      {
	  if (! strcmp (mnt_path, "/") && stat (fs_path, &stbuf) >= 0)
	    {
		fclose (in);
		return stbuf.st_rdev;
	    }
      }

    if (in)
      fclose (in);
    
    /* XXX: wrong, but a legal value */
    return 1;
}


/*
 * returns size (tag == HRSTORAGESIZE) or used (tag == HRSTORAGEUSED)
 * for ram (idx == 0), swap (idx == 1), disk (idx == 2), 
 * floppy (idx == 3)
 */

static int
hr_getstor (idx, tag, unit)
int idx, tag, unit;
{
    char line [1024], s [1024];
    int i, t, u;
    FILE *in;

    if (idx == 0 || idx == 1)
      {
	  if (! (in = fopen ("/proc/meminfo", "r"))) {
	      perror ("snmpd: cannot open /proc/meminfo");
	      return 9999;		/* what to do ??? */
	  }
	  
	  for (i = 0; fgets (line, sizeof(line), in); i++)
	    {
		if (! i)
		  continue;
		if (3 == sscanf (line, "%s %d %d", s, &t, &u))
		  {
		      if ((idx == 0 && ! strcmp (s, "Mem:"))
			  || (idx == 1 && ! strcmp (s, "Swap:")))
			{
			    fclose (in);
			    return (tag == HRSTORAGESIZE ? t : u) / unit;
			}
		  }
		/* XXX: add new /proc/meminfo strategie since 1.3.7x */
	    }
	  fclose (in);
	  return 9999;
      }
    
    return 9999;
}


/*
 * fill process-table for hrswrun/hrswperf table:
 * return list-start.
 */

typedef struct _pslist {
    int pid;
    char stat, *cmd, *cmd_line;
    int type, time, rss;
    struct _pslist *next;
} pslist;

static pslist *
fill_ps_list ()
{
    static pslist *all_ps = 0;
    static time_t tstamp = 0;
    time_t now;

    time (&now);
    if (tstamp + 5 > now)
      return all_ps;
    tstamp = now;

    /* XXX: free old list */
    while (all_ps)
      {
	  pslist *p = all_ps;
	  all_ps = p->next;
	  if (p->cmd) free (p->cmd);
	  if (p->cmd_line) free (p->cmd_line);
	  free (p);
      }

#ifdef linux
    { DIR *d = opendir ("/proc");
      struct dirent *de;

      while (d && (de = readdir (d)))
	{
	    pslist *nnew, **p;
	    
	    if (de->d_name [0] < '0' && de->d_name [0] > '9')
	      continue;
	    else
	      {
		  char tmp [256], c;
		  FILE *in = 0;
		  int rss, utime, stime;
		  
		  nnew = (pslist *) calloc (1, sizeof (pslist));
		  if (! nnew)
		    {
			fprintf (stderr, "warning: out of mem - ignored...\n");
			break;
		    }
		  nnew->pid = atoi (de->d_name);
		  /* XXX: still its always application: */
		  nnew->type = 4;
		  
		  sprintf (tmp, "/proc/%d/stat", nnew->pid);
		  if (! (in = fopen (tmp, "r")) ||
		      5 != fscanf (in, "%*d %s %c %*d %*d %*d %*d %*d %*d %*d %*d %*d %*d %d %d %*d %*d %*d %*d %*d %*d %*d %*d %d", 
				   tmp, &c, &utime, &stime, &rss))
		    nnew->cmd = 0, nnew->stat = '?';
		  else {
		      nnew->cmd = strdup (tmp);
		      nnew->stat = c;
		      nnew->time = utime + stime;
		      nnew->rss = rss;
		      
		      fclose (in);
		  }
		  
		  sprintf (tmp, "/proc/%d/cmdline", nnew->pid);
		  nnew->cmd_line = 0;
		  if ((in = fopen (tmp, "r")))
		    {
			int c, i;
			static len = 0;
			static char *buf;

			if (! len && ! (buf = malloc (len = 100))) {
			  fclose (in);
			  /* ignore and go on: */
			  continue;
			}
			
			for (i = 0; buf && (c = fgetc (in)) != EOF; i++)
			  {
			      if (i + 1 >= len)
				{
				    len += 10;
				    if (! (buf = realloc (buf, len)))
				      break;
				}
			      buf [i] = c ? c : ' ';
			  }
			if (buf && i > 0)
			  {
			      buf [i] = 0;
			      nnew->cmd_line = strdup (buf);
			  } 
			else if (nnew->cmd)
			  nnew->cmd_line = strdup (nnew->cmd);
			
			fclose (in);
		    }
	      }
	    
	    /* merge in: */
	    for (p = &all_ps; *p && (*p)->pid < nnew->pid; p = &(*p)->next);
	    nnew->next = *p; 
	    *p = nnew;
	}
      
      if (d) {
	closedir (d);
      }
    }
#endif

    return all_ps;
}



static char *
hr_cpu_desc (len)
int *len;
{
    char line [256], key [256], val [256];
    static char data [256];
    FILE *in;
    int i;

    if (! (in = fopen ("/proc/cpuinfo", "r"))) {
	perror ("snmpd: cannot open /proc/cpuinfo");
	*len = 7;
	return "Unknown";
    }
    
    /* return the first 5 - XXX: 2  lines 
     * (maybe we should select more appropriate info): */
    data [0] = *len = 0;
    for (i = 0; fgets (line, sizeof(line), in) && i < 2; i++)
      {
	  if (2 == sscanf (line, "%s : %s", key, val))
	    {
		sprintf (data + *len, "%s%s: %s", i ? ", " : "", key, val);
		*len = strlen (data);
	    }
      }

    /* forced by mib: (argl) */
    if (strlen (data) >= 64)
        data [strlen (data) - 1] = 0;

    fclose (in);
    return data;
}


/* initial load device index (points into hrDeviceEntry; */
static unsigned long hr_initial_load_dev = 0;


/*
 * entry for the host resources mib:
 */

u_char *
var_hr(vp, name, length, exact, var_len, write_method)
    struct variable *vp;   /* IN - pointer to var entry that points here */
    oid     *name;	   /* IN/OUT - input name req, output name found */
    int     *length;	   /* IN/OUT - length of input and output oid's */
    int     exact;	   /* IN - TRUE if an exact match was requested. */
    int     *var_len; 	   /* OUT - length of var or 0 if function returned. */
    int     (**write_method)();  /* OUT - pointer to func to set var, else 0 */
{
    oid newname[MAX_NAME_LEN];
    int result;

    /* nothing writable provided: */
    *write_method = 0;

     if (vp->magic <= HRMEMORYSIZE || vp->magic == HRSWOSINDEX) {

	bcopy((char *)vp->name, (char *)newname, vp->namelen * sizeof(oid));
	newname[9] = 0;
	result = compare(name, *length, newname, (int)vp->namelen + 1);
	if ((exact && (result != 0)) || (!exact && (result >= 0)))
	  return NULL;
	bcopy((char *)newname, (char *)name, (vp->namelen + 1) * sizeof(oid));
	*length = vp->namelen + 1;
	
	*var_len = sizeof(long);	/* default length */
	
	/* default value: */
	long_return = 0;
	
	switch (vp->magic){
	  case HRSYSTEMUPTIME:
	    long_return = 0;
	    {
#ifdef linux
		FILE *in;
		double up;
		if ((in = fopen ("/proc/uptime", "r"))) {
		    if (1 == fscanf (in, "%lf", &up)) {
			long_return = (int) (up * 100);
		    }
		    fclose (in);
		}
#endif
	    }
	    break;
	  case HRSYSTEMDATE:
	    { time_t t;
	      struct tm *tt;
	      static char ret [8];
	      time (&t);
	      tt = localtime (&t);
	      * (short *) ret = htons (tt->tm_year);
	      ret [2] = tt->tm_mon + 1;	    ret [3] = tt->tm_mday;
	      ret [4] = tt->tm_hour;	    ret [5] = tt->tm_min;
	      ret [6] = tt->tm_sec;	    ret [7] = 0;
	      *var_len = 8;
	      return ret;
	    }
	    break;

	  case HRSYSTEMINITIALLOADDEVICE:
	    if (hr_initial_load_dev == 0)
	      hr_initial_load_dev = hr_find_initial_load_dev ();
	    long_return = hr_initial_load_dev;
	    break;

	  case HRSYSTEMINITIALLOADPARAMETERS:
	    *var_len = 0;
	    return "";					/* XXX: fix me */
	    break;

	  case HRSYSTEMNUMUSERS:
	    {
		struct utmp *ut;
		long_return = 0;
		setutent ();
		while ((ut = getutent ()))
		  long_return += ut->ut_user[0] != 0
				  && strcmp (ut->ut_user, "LOGIN");
		endutent ();
	    }
	    break;
	  case HRSYSTEMPROCESSES:
	    {
#ifdef linux
		DIR *d = opendir ("/proc");
		struct dirent *de;
		int cnt = 0;
		while (d && (de = readdir (d)))
		  cnt += de->d_name [0] >= '0' && de->d_name [0] <= '9';
		if (d) {
		  closedir (d);
		}
		long_return = cnt;
#else
		long_return = 42;
#endif
	    }
	    break;
	  case HRSYSTEMMAXPROCESSES:
#ifdef linux
	    long_return = NR_TASKS;
#else
	    long_return = 99;
#endif
	    break;

	  case HRMEMORYSIZE:
	    { struct stat sbuf;
	      if (stat ("/proc/kcore", &sbuf) < 0)
		{
		    perror ("snmpd: cannot stat /proc/kcore");
		    long_return = 0;
		}
	      else	/* gives 4k more than avail for 16m - guess: */
		long_return = 1024 * (sbuf.st_size / 1024 / 1024);
  	    }
	    break;

	  case HRSWOSINDEX:
	    /* XXX: fix me: the default is a lie - but what else should 
	       we return ? */
	    long_return = 1;
	    break;
	    
	  default:
	    ERROR("unknown hostresources var");
	    return NULL;
	}
	
	return (u_char *) &long_return;

    } else if (vp->magic <= HRSTORAGEALLOCFAILURES) {

	/*
	 * storage:
	 *
	 * OID:  1.3.6.1.2.1.25.2.3.1.tag.idx
	 */

	int idx, i;
	oid lowest[MAX_NAME_LEN], *op;
	FILE *in = 0;
	char line [128], i_line [128];
	char dev [128];
	struct statfs sbuf;
	struct stat stbuf;
	int stype = 0;		/* storage type: 4 hd, 6 floppy, 1 other */

	static struct hr_stor { 
	    oid type [10];
	    char *descr;
	    int units;
	} stor [] = {
	    { /* idx 0: */ { 1,3,6,1,2,1,25,2,1,2 }, "Mem", 1024 },
	    { /* idx 1: */ { 1,3,6,1,2,1,25,2,1,3 }, "Swap", 1024 },
	};
#define HR_N_STOR	2

	bcopy ((char *) vp->name, (char *) newname, 
	       (int) vp->namelen * sizeof (oid));
	idx = -1;

	for (i = 0; ; i++) 
	  {
 	      if (i == HR_N_STOR && ! in)
		    in = fopen ("/etc/mtab", "r");
	      if (i >= HR_N_STOR)
		{
		    if (! in || ! fgets (line, sizeof (line), in))
		      break; 
		    if (strncmp (line, "/dev/sd", 7)
			&& strncmp (line, "/dev/sr", 7)
			&& strncmp (line, "/dev/hd", 7)
			&& strncmp (line, "/dev/xd", 7)
			&& strncmp (line, "/dev/fd", 7))
		      continue;

		    if (1 != sscanf (line, "%s", dev)
			|| stat (dev, &stbuf) < 0)
		      continue;
		}
	      
	      op = newname + 11;
	      *op++ = i < HR_N_STOR ? i + 1 : stbuf.st_rdev;

	    if (exact) {
		if (compare (newname, 12, name, *length) == 0) {
		    bcopy((char *)newname, (char *)lowest, 12 * sizeof(oid));
		    idx = i;
		    bcopy (line, i_line, sizeof (line));
		    break;  /* no need to search further */
		}
	    } else {
		if (compare (newname, 12, name, *length) > 0 &&
		    ( idx < 0 || compare(newname, 12, lowest, 12) < 0)) {
		    /*
		     * if new one is greater than input and closer to input
		     * than previous lowest, save this one as the "next" one.
		     */
		    bcopy((char *)newname, (char *)lowest, 12 * sizeof(oid));
		    bcopy (line, i_line, sizeof (line));
		    idx = i;
		}
	    }
	}

	if (in) {
	    fclose (in);
	}

	if (idx < 0) {
	    return NULL;
	}

	bcopy ((char *) lowest, (char *) name, 
	       ((int) vp->namelen + 1) * sizeof(oid));
	*length = vp->namelen + 1;

	if (idx >= HR_N_STOR && in)
	  {
	      char mnt [256];
	      if (1 != sscanf (i_line, "%*s %s", mnt)
		  || statfs (mnt, &sbuf) < 0) {
		  sbuf.f_blocks = sbuf.f_bfree = 0;
		  sbuf.f_bsize = 1;
	      }
	      /* hrStorageTypes: 
		 1 == other, 4 == disk, 5 == removeable, 6 == floppy */
	      stype = ! strncmp (i_line, "/dev/sd", 7) ? 4 :
	              ! strncmp (i_line, "/dev/sr", 7) ? 5 :
		      ! strncmp (i_line, "/dev/hd", 7) ? 4 :   /* XXX: cdrom */
		      ! strncmp (i_line, "/dev/xd", 7) ? 4 :
		      ! strncmp (i_line, "/dev/fd", 7) ? 6 : 1;
	  }

	*var_len = sizeof(long);

	switch (vp->magic) {
	  case HRSTORAGEINDEX:
/*	    long_return = idx + 1; */
	    long_return = name [11];
	    break;
	  case HRSTORAGETYPE:
	    { static oid otype [10] = { 1,3,6,1,2,1,25,2,1, };
	      *var_len = sizeof (otype);
	      if (idx < 2)
		return (char *) stor[idx].type;
	      otype [9] = stype;
	      return (char *) otype;
	    }
	  case HRSTORAGEDESCR:
	    { char *s = idx < 2 ? stor[idx].descr : 
		stype == 4 ? "Disk" : 
		stype == 6 ? "Floppy" : 
		stype == 5 ? "CDROM" : "Unknown";
	      *var_len = strlen (s);
	      return s;
	    }
	  case HRSTORAGEALLOCUNITS:
	    long_return = 1024;			/* in KB */
	    break;
	  case HRSTORAGESIZE:
	    long_return = idx < 2 ? hr_getstor (idx, HRSTORAGESIZE, 1024) :
		(1.0 * sbuf.f_blocks * sbuf.f_bsize) / 1024;
	    break;
	  case HRSTORAGEUSED:
	    long_return = idx < 2 ? hr_getstor (idx, HRSTORAGEUSED, 1024) :
		(1.0 * (sbuf.f_blocks-sbuf.f_bfree) * sbuf.f_bsize) / 1024;
	    break;
	  case HRSTORAGEALLOCFAILURES:
	    long_return = 0;			/* XXX: dummy */
	    break;
	  default:
	    ERROR("unknown hostresources var");
	    return NULL;
	}

	return (u_char *) &long_return;

    } else if (vp->magic <= HRDEVICEERRORS) {

	/*
	 * device:
	 *
	 * OID:  1.3.6.1.2.1.25.3.2.1.tag.idx
	 */

	int idx, i;
	oid lowest[MAX_NAME_LEN], *op;

	static struct hr_dev { 
	    oid type [10];
	    char *descr;
	    int status;
	} dev [] = {
	    { /* idx 0: */ { 1,3,6,1,2,1,25,3,1,3 }, "CPU", 2 }
	};
#define HR_N_DEV	1

	bcopy ((char *) vp->name, (char *) newname, 
	       (int) vp->namelen * sizeof (oid));
	idx = -1;

	for (i = 0; i < HR_N_DEV; i++) 
	  {
	      op = newname + 11;
	      *op++ = i + 1;

	    if (exact) {
		if (compare (newname, 12, name, *length) == 0) {
		    bcopy((char *)newname, (char *)lowest, 12 * sizeof(oid));
		    idx = i;
		    break;  /* no need to search further */
		}
	    } else {
		if (compare (newname, 12, name, *length) > 0 &&
		    ( idx < 0 || compare(newname, 12, lowest, 12) < 0)) {
		    /*
		     * if new one is greater than input and closer to input
		     * than previous lowest, save this one as the "next" one.
		     */
		    bcopy((char *)newname, (char *)lowest, 12 * sizeof(oid));
		    idx = i;
		}
	    }
	}

	if (idx < 0 || idx >= HR_N_DEV)
	  return NULL;

	bcopy ((char *) lowest, (char *) name, 
	       ((int) vp->namelen + 1) * sizeof(oid));

	*length = vp->namelen + 1;
	*var_len = sizeof(long);

	switch (vp->magic) {
	  case HRDEVICEINDEX:
	    long_return = idx + 1;
	    break;
	  case HRDEVICETYPE:
	    *var_len = sizeof (dev[idx].type);
	    return (char *) dev[idx].type;
	    break;
	  case HRDEVICEDESCR:
	    if (idx == 0)
	      return hr_cpu_desc (var_len);
	    else {
		*var_len = strlen (dev[idx].descr);
		return dev[idx].descr;
	    }
	  case HRDEVICEID:
	    { static oid no [2] = { 0, 0 };
	      *var_len = sizeof (no);
	      return (char *) no;
	    }
	    break;
	  case HRDEVICESTATUS:
	    long_return = 2;		/* running */
	    break;
	  case HRDEVICEERRORS:
	    long_return = 0;
	    break;
	  default:
	    ERROR("unknown hostresources var");
	    return NULL;
	}

	return (u_char *) &long_return;

    } else if (vp->magic <= HRPROCESSORLOAD) {

	/*
	 * device:
	 *
	 * OID:  1.3.6.1.2.1.25.3.3.1.tag.idx
	 */

	int idx;
	oid lowest[MAX_NAME_LEN], *op;

	bcopy ((char *) vp->name, (char *) newname, 
	       (int) vp->namelen * sizeof (oid));
	idx = -1;

	/* single processor assumed: */
	op = newname + 11;
	*op++ = 1;

	if (exact) {
	    if (compare (newname, 12, name, *length) == 0) {
		bcopy((char *)newname, (char *)lowest, 12 * sizeof(oid));
		idx = 0;
	    }
	} else {
	    if (compare (newname, 12, name, *length) > 0 &&
		( idx < 0 || compare(newname, 12, lowest, 12) < 0)) {
		/*
		 * if new one is greater than input and closer to input
		 * than previous lowest, save this one as the "next" one.
		 */
		bcopy((char *)newname, (char *)lowest, 12 * sizeof(oid));
		idx = 0;
	    }
	}
	
	if (idx < 0)
	  return NULL;

	bcopy ((char *) lowest, (char *) name, 
	       ((int) vp->namelen + 1) * sizeof(oid));

	*length = vp->namelen + 1;

	if (vp->magic == HRPROCESSORFRWID) {
	    static oid nulloid [2];
	    *var_len = 2 * sizeof (oid);
	    return (char *) nulloid;
	}
	else {
#ifdef linux
	  *var_len = sizeof(long);
	  long_return = get_HrProcessorLoad();
	  return (char *) &long_return;
#else
	    /* guess a processorload from the avenrun index (over the
               last minute): */
	    { FILE *in = fopen ("/proc/loadavg", "r");
	      long_return = 0;		/* fail default */
	      if (in) {
		  double d;
		  if (1 == fscanf (in, "%lf", &d))
		    long_return = d >= 1.0 ? 100 : d * 100;
		  fclose (in);
	      }
	      *var_len = sizeof(long);
	      return (char *) &long_return;
	    }
#endif
	}

    } else if (vp->magic <= HRFSLASTPARTIALBACKUPDATE) {

	/*
	 * device:
	 *
	 * OID:  1.3.6.1.2.1.25.3.8.1.tag.idx
	 */

	int idx, i;
	oid lowest[MAX_NAME_LEN], *op;
	FILE *in = fopen ("/etc/mtab", "r");
	char line [512];
	char fs_path [128], mnt_path [128];
	static char i_fs_path [128], i_mnt_path [128];
	struct stat stbuf;

	bcopy ((char *) vp->name, (char *) newname, 
	       (int) vp->namelen * sizeof (oid));
	idx = -1;

	for (i = 0; in; i++) 
	  {
	      if (! fgets (line, sizeof (line), in) 
		  || 2 != sscanf (line, "%s %s", fs_path, mnt_path))
		break;

		    if (strncmp (line, "/dev/sd", 7)
			&& strncmp (line, "/dev/sr", 7)
			&& strncmp (line, "/dev/hd", 7)
			&& strncmp (line, "/dev/xd", 7)
			&& strncmp (line, "/dev/fd", 7))
		      continue;
	      if (stat (fs_path, &stbuf) < 0)
		continue;
	      
	      op = newname + 11;
	      *op++ = stbuf.st_rdev;		/* i + 1 */

	      if (exact) {
		  if (compare (newname, 12, name, *length) == 0) {
		      bcopy((char *)newname, (char *)lowest, 12 * sizeof(oid));
		      idx = i;
		      bcopy (fs_path, i_fs_path, 128);
		      bcopy (mnt_path, i_mnt_path, 128);
		      break;  /* no need to search further */
		  }
	      } else {
		  if (compare (newname, 12, name, *length) > 0 &&
		      ( idx < 0 || compare(newname, 12, lowest, 12) < 0)) {
		      /*
		       * if new one is greater than input and closer to input
		       * than previous lowest, save this one as the "next" one.
		       */
		      bcopy((char *)newname, (char *)lowest, 12 * sizeof(oid));
		      idx = i;
		      bcopy (fs_path, i_fs_path, 128);
		      bcopy (mnt_path, i_mnt_path, 128);
		  }
	      }
	  }

	if (in)
	  fclose (in);
	
	if (idx < 0)
	  return NULL;
	
	bcopy ((char *) lowest, (char *) name, 
	       ((int) vp->namelen + 1) * sizeof(oid));
	
	*length = vp->namelen + 1;
	*var_len = sizeof(long);

	switch (vp->magic) {

	  case HRFSINDEX:
	    long_return = name [11];
	    break;
	  case HRFSMOUNTPOINT:
	    *var_len = strlen (i_fs_path);
	    return i_fs_path;
	    break;
	  case HRFSREMOTEMOUNTPOINT:
	    *var_len = strlen (i_mnt_path);
	    return i_mnt_path;
	    break;
	  case HRFSTYPE:
	    { /* hrFSUnknown */
	      static oid fsoid [10] = { 1, 3, 6, 1, 2, 1, 25, 3, 9, 2 };
	      *var_len = sizeof (fsoid);
	      return (char *) fsoid;
	    }
	    break;
	  case HRFSACCESS:
	    long_return = 1;		/* rw */
	    break;
	  case HRFSBOOTABLE:
	    /* XXX: just a guess - maybe bullshit */
	    { int flag = strcmp (i_mnt_path, "/") ? 2 : 1;
	      long_return = flag;
	    }
	    break;
	  case HRFSSTORAGEINDEX:
	    long_return = name [11];		/* same as index */
	    break;
	  case HRFSLASTFULLBACKUPDATE:
	  case HRFSLASTPARTIALBACKUPDATE:
	    { static char *epoch = "\000\000\001\001\000\000\000\000";
	      *var_len = 8;
	      return epoch;
	    }
	    break;

	  default:
	    ERROR("unknown hostresources var");
	    return NULL;
	}

	return (u_char *) &long_return;

    } else if (vp->magic <= HRSWRUNPERFMEM) {

	/*
	 * runtable:
	 *
	 * swrun-OID:   1.3.6.1.2.1.25.4.2.1.tag.idx
	 * swperf-OID:  1.3.6.1.2.1.25.5.1.1.tag.idx
	 */

	oid lowest[MAX_NAME_LEN], *op;
	pslist *ps = 0, *p;
#if 0
	XXX: FIXME
	/*
	 * restrict access to ``private'' communtiy for V1; 
	 * V2 is handled by context/view-selection.  
	 */
	
	if ((vp->pi->version == SNMP_VERSION_1 && vp->pi->community_id == 1))
	      return 0;
#endif
	bcopy ((char *) vp->name, (char *) newname, 
	       (int) vp->namelen * sizeof (oid));

	if (! (p = fill_ps_list ()))
	  return 0;

	for (; p; p = p->next)
	  {
	      op = newname + 11;
	      *op++ = p->pid;
	      
	      if (exact) {
		  if (compare (newname, 12, name, *length) == 0) {
		      bcopy((char *)newname, (char *)lowest, 12 * sizeof(oid));
		      ps = p;
		      break;  /* no need to search further */
		  }
	      } else {
		  if (compare (newname, 12, name, *length) > 0 &&
		      ( ! ps || compare(newname, 12, lowest, 12) < 0)) {
		      /*
		       * if new one is greater than input and closer to input
		       * than previous lowest, save this one as the "next" one.
		       */
		      bcopy((char *)newname, (char *)lowest, 12 * sizeof(oid));
		      ps = p;
		  }
	      }
	  }
	
	if (! ps)
	  return NULL;
	
	bcopy ((char *) lowest, (char *) name, 
	       ((int) vp->namelen + 1) * sizeof(oid));

	*length = vp->namelen + 1;
	*var_len = sizeof(long);
	
	switch (vp->magic) {
	  case HRSWRUNINDEX:
	    /* well, not a unique number for each piece of software, 
	       just simply the pid: */
	    long_return = ps->pid;
	    break;
	  case HRSWRUNNAME:
	    /* XXX: we could use configfile-entries to set descriptions
	       for selected pieces of software; currently nothing: */
	    *var_len = 9; 
	    return "(unknown)";	    
	    break;
	  case HRSWRUNID:
	    { /* unknown product id: */
	      static oid o [2] = { 0, 0 };
	      *var_len = sizeof (o);
	      return (char *) o;
	    }
	    break;
	  case HRSWRUNPATH:
	    /* return at least the basename of the program: */
	    if (ps->cmd)
	      {
		  *var_len = strlen (ps->cmd);
		  if (ps->cmd [0] == '(' && ps->cmd [*var_len - 1] == ')')
		    {
			*var_len -= 2;
			return ps->cmd + 1;
		    }
		  return ps->cmd;
	      }
	    *var_len = 9; 
	    return "(unknown)";	    
	    break;
	  case HRSWRUNPARAMETERS:
	    /* complete commandline currently known (includes argv[0]): */
	    if (ps->cmd_line)
	      {
		  *var_len = strlen (ps->cmd_line);
		  return ps->cmd_line;
	      }
	    *var_len = 9; 
	    return "(unknown)";	    
	    break;
	  case HRSWRUNTYPE:
	    long_return = ps->type;
	    break;
	  case HRSWRUNSTATUS:
	    long_return = ps->stat == 'R' ? 1 : ps->stat == 'D' ? 3 : 2;
	    break;

	  case HRSWRUNPERFCPU:
	    long_return = ps->time;
	    break;
	  case HRSWRUNPERFMEM:
	    long_return = ps->rss << 2;
	    break;

	  default:
	    ERROR("unknown hostresources var");
	    return NULL;
	}

	return (u_char *) &long_return;
    }

    return 0;
}


static char *
xstrdup (s)
char *s;
{
    char *n = malloc (strlen (s) + 1);
    if (! s)
      return "(unknown)";
    strcpy (n, s);
    return n;
}


/* format uid to name or gecos: */
static void
uid_to_str (buf, uid, len, flag)
char *buf;
int uid, len, flag;
{
    struct passwd *paw;
    typedef struct _pwds {
	int uid;
	char *name;
	char *gecos;
	struct _pwds *next;
    } pwds;
    static pwds *all_pw = 0;
    pwds *p;
    char *s;

    for (p = all_pw; p && p->uid != uid; p = p->next) ;

    if (! p)
      {
	  if (! (paw = getpwuid (uid)))
	    {
		strcpy (buf, "(unknown)");
		return;
	    }
    
	  if (paw) {
	      pwds *pnew = (pwds *) malloc (sizeof (pwds));
	      if (! pnew) {
		  strcpy (buf, "(unknown)");
		  return;
	      }
	      pnew->uid = paw->pw_uid;
	      pnew->name = xstrdup (paw->pw_name);
	      pnew->gecos = xstrdup (paw->pw_gecos);
	      pnew->next = all_pw;
	      p = all_pw = pnew;
	  }
      }
    strncpy (buf, flag ? p->gecos : p->name, len - 1);
    buf [len - 1] = 0;
    /* for US-ASCII strip 8 bit 
     * -- germans please don't even think about complaining... */
    for (s = buf; *s; s++)
      *s = *s & 0x7f;
}


/*
 * entry for the identification mib:
 */

u_char *
var_id(vp, name, length, exact, var_len, write_method)
     struct variable *vp;    /* IN - ptr to variable entry that points here */
     oid     *name;	     /* IN/OUT - input name req, output name found */
     int     *length;	     /* IN/OUT - length of input and output oid's */
     int     exact;	     /* IN - TRUE if an exact match was requested. */
     int     *var_len;	     /* OUT - length of var or 0 if function ret. */
     int     (**write_method)();   /* OUT - ptr to func to set var, else 0 */
{
    int i;
    /* static struct tcpstat tcpstat; */
    static char ret_str [256];
    oid newname[MAX_NAME_LEN], lowest[MAX_NAME_LEN], *op;
    u_char *cp;
    int State, LowState;
    static struct inpcb inpcb, Lowinpcb;

    /* mib-2.ident.identInfo.identTable.identEntry == 1.3.6.1.2.1.24.1.1.1.x */
    
    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    /* find "next" connection */
  Again:
    LowState = -1;	    /* Don't have one yet */
    TCP_Scan_Init();
    for (;;) {
	if ((i = TCP_Scan_Next(&State, &inpcb)) < 0) 
	  goto Again;
	if (i == 0) break;	    /* Done */
	cp = (u_char *)&inpcb.inp_laddr.s_addr;
	op = newname + 11;
	*op++ = *cp++, *op++ = *cp++, *op++ = *cp++, *op++ = *cp++;
	
	newname[15] = ntohs(inpcb.inp_lport);
	
	cp = (u_char *)&inpcb.inp_faddr.s_addr;
	op = newname + 16;
	*op++ = *cp++, *op++ = *cp++, *op++ = *cp++, *op++ = *cp++;
	
	newname[20] = ntohs(inpcb.inp_fport);
	
	if (exact){
	    if (compare(newname, 21, name, *length) == 0){
		bcopy((char *)newname, (char *)lowest, 21 * sizeof(oid));
		LowState = State;
		Lowinpcb = inpcb;
		break;  /* no need to search further */
	    }
	} else {
	    if ((compare(newname, 21, name, *length) > 0) &&
		((LowState < 0) || (compare(newname, 21, lowest, 21) < 0))){
		/*
		 * if new one is greater than input and closer to input than
		 * previous lowest, save this one as the "next" one.
		 */
		bcopy((char *)newname, (char *)lowest, 21 * sizeof(oid));
		LowState = State;
		Lowinpcb = inpcb;
	    }
	}
    }
    if (LowState < 0)
      return(NULL);

    bcopy((char *)lowest, (char *)name, ((int)vp->namelen + 10) * sizeof(oid));
    *length = vp->namelen + 10;
    *write_method = 0;
    *var_len = sizeof(long);

    switch (vp->magic) {
      case IDIDENTSTATUS: 
	long_return = (Lowinpcb.inp_state == 6) + 1;
	return (u_char *) &long_return;
      case IDIDENTOPSYS:
	*var_len = 4;
	return "unix";
      case IDIDENTCHARSET:
	*var_len = 8;
	return "US-ASCII";
      case IDIDENTUSERID:
	if (Lowinpcb.inp_state == 6)
	  *ret_str = 0;
	else
	  uid_to_str (ret_str, Lowinpcb.uid, 256, 0);
	*var_len = strlen (ret_str); 
	return ret_str;
      case IDIDENTMISC:
	if (Lowinpcb.inp_state == 6)
	  *ret_str = 0;
	else
	  uid_to_str (ret_str, Lowinpcb.uid, 256, 1);
	*var_len = strlen (ret_str); 
	return ret_str;
    }
    return NULL;
}

/*
 * entry for the linux mib:
 */

u_char *
var_linux(vp, name, length, exact, var_len, write_method)
     struct variable *vp;    /* IN - ptr to variable entry that points here */
     oid     *name;	     /* IN/OUT - input name req, output name found */
     int     *length;	     /* IN/OUT - length of input and output oid's */
     int     exact;	     /* IN - TRUE if an exact match was requested. */
     int     *var_len;	     /* OUT - length of var or 0 if function ret. */
     int     (**write_method)();   /* OUT - ptr to func to set var, else 0 */
{
    oid newname[MAX_NAME_LEN];
    int result;
    static char *cpu = NULL;
    static int bogo = 0;

    /* nothing writable provided: */
    *write_method = 0;

    /* default return type: */
    *var_len = sizeof(long);

    bcopy((char *)vp->name, (char *)newname, vp->namelen * sizeof(oid));
    newname[11] = 0;
    result = compare(name, *length, newname, (int)vp->namelen + 1);
    if ((exact && (result != 0)) || (!exact && (result >= 0)))
	return NULL;
    bcopy((char *)newname, (char *)name, (vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;
    
    if (! cpu) {
	char line[256], key[256], val[256];
	FILE *in = fopen ("/proc/cpuinfo", "r");
	cpu = malloc(256);
	if (in) {
	    while (fgets (line, sizeof(line), in)) {
		if (2 == sscanf (line, "%s : %s", key, val)) {
		    if (strcmp(key, "cpu") == 0) {
			strcpy(cpu, val);
		    } else if (strcmp(key, "bogomips") == 0) {
			bogo = atoi(val);
		    }
		}
	    }
	    fclose(in);
	}
    }

    switch (vp->magic) {
    case LINUXCPU:
	*var_len = strlen(cpu);
	return cpu;
	break;
    case LINUXBOGO:
	return (u_char *) &bogo;
	break;
    }

    return NULL;
}

#endif /* linux */


#ifdef linux
/*
 * lucky days. since 1.1.16 the tcp statistics are avail by the proc
 * file-system.
 */

static void
linux_read_tcp_stat (tcpstat)
struct tcp_mib *tcpstat;
{
  FILE *in = fopen ("/proc/net/snmp", "r");
  char line [1024];

  bzero ((char *) tcpstat, sizeof (*tcpstat));

  if (! in)
    return;

  while (line == fgets (line, 1024, in))
    {
      if (12 == sscanf (line, "Tcp: %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu\n",
	&tcpstat->TcpRtoAlgorithm, &tcpstat->TcpRtoMin, &tcpstat->TcpRtoMax, 
	&tcpstat->TcpMaxConn, &tcpstat->TcpActiveOpens, &tcpstat->TcpPassiveOpens,
	&tcpstat->TcpAttemptFails, &tcpstat->TcpEstabResets, &tcpstat->TcpCurrEstab, 
	&tcpstat->TcpInSegs, &tcpstat->TcpOutSegs, &tcpstat->TcpRetransSegs))
	break;
    }
  fclose (in);
}

#endif /* linux */



u_char *
var_tcp(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;    /* IN - pointer to variable entry that points here */
    oid     *name;	    /* IN/OUT - input name requested, output name found */
    int     *length;	    /* IN/OUT - length of input and output oid's */
    int     exact;	    /* IN - TRUE if an exact match was requested. */
    int     *var_len;	    /* OUT - length of variable or 0 if function returned. */
    int     (**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    int i, result;
    static struct tcpstat tcpstat;
    oid newname[MAX_NAME_LEN], lowest[MAX_NAME_LEN], *op;
    u_char *cp;
    int State, LowState;
    static struct inpcb inpcb, Lowinpcb;

    /*
     *	Allow for a kernel w/o TCP
     */

#ifndef linux
    if (nl[N_TCPSTAT].n_value == 0) return(NULL);
#endif

    if (vp->magic < TCPCONNSTATE) {

	bcopy((char *)vp->name, (char *)newname,
	      (int)vp->namelen * sizeof(oid));
	newname[8] = 0;
	result = compare(name, *length, newname, (int)vp->namelen + 1);
	if ((exact && (result != 0)) || (!exact && (result >= 0)))
	    return NULL;
	bcopy((char *)newname, (char *)name,
	      ((int)vp->namelen + 1) * sizeof(oid));
	*length = vp->namelen + 1;

	*write_method = 0;
	*var_len = sizeof(long);    /* default length */
	/*
	 *  Get the TCP statistics from the kernel...
	 */

#ifndef linux
	KNLookup( N_TCPSTAT, (char *)&tcpstat, sizeof (tcpstat));
#else
	linux_read_tcp_stat (&tcpstat);
#endif

	switch (vp->magic){
	    case TCPRTOALGORITHM:
#ifndef linux
		long_return = 4;	/* Van Jacobsen's algorithm *//* XXX */
		return (u_char *) &long_return;
#else
                if (! tcpstat.TcpRtoAlgorithm) {
		    /* 0 is illegal: assume `other' algorithm: */
		    long_return = 1;
		    return (u_char *) &long_return;
                }
                return (u_char *) &tcpstat.TcpRtoAlgorithm;
#endif
	    case TCPRTOMIN:
#ifndef linux
		long_return = TCPTV_MIN / PR_SLOWHZ * 1000;
		return (u_char *) &long_return;
#else
		return (u_char *) &tcpstat.TcpRtoMin;
#endif
	    case TCPRTOMAX:
#ifndef linux
		long_return = TCPTV_REXMTMAX / PR_SLOWHZ * 1000;
		return (u_char *) &long_return;
#else
		return (u_char *) &tcpstat.TcpRtoMax;
#endif
	    case TCPMAXCONN:
#ifndef linux
		long_return = -1;
		return (u_char *) &long_return;
#else
		return (u_char *) &tcpstat.TcpMaxConn;
#endif
	    case TCPACTIVEOPENS:

		return (u_char *) &tcpstat.tcps_connattempt;

	    case TCPPASSIVEOPENS:

		return (u_char *) &tcpstat.tcps_accepts;

	    case TCPATTEMPTFAILS:
		return (u_char *) &tcpstat.tcps_conndrops;

	    case TCPESTABRESETS:
		return (u_char *) &tcpstat.tcps_drops;

	    case TCPCURRESTAB:
#ifndef linux
		long_return = TCP_Count_Connections();
		return (u_char *) &long_return;
#else
		return (u_char *) &tcpstat.TcpCurrEstab;
#endif
	    case TCPINSEGS:
		return (u_char *) &tcpstat.tcps_rcvtotal;

	    case TCPOUTSEGS:
		return (u_char *) &tcpstat.tcps_sndtotal;
	    case TCPRETRANSSEGS:
		return (u_char *) &tcpstat.tcps_sndrexmitpack;
	    default:
		ERROR("");
	}
    } else {	/* Info about a particular connection */
	bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
	/* find "next" connection */
Again:
LowState = -1;	    /* Don't have one yet */
	TCP_Scan_Init();
	for (;;) {
	    if ((i = TCP_Scan_Next(&State, &inpcb)) < 0) goto Again;
	    if (i == 0) break;	    /* Done */
	    cp = (u_char *)&inpcb.inp_laddr.s_addr;
	    op = newname + 10;
	    *op++ = *cp++;
	    *op++ = *cp++;
	    *op++ = *cp++;
	    *op++ = *cp++;
	    
	    newname[14] = ntohs(inpcb.inp_lport);

	    cp = (u_char *)&inpcb.inp_faddr.s_addr;
	    op = newname + 15;
	    *op++ = *cp++;
	    *op++ = *cp++;
	    *op++ = *cp++;
	    *op++ = *cp++;
	    
	    newname[19] = ntohs(inpcb.inp_fport);

	    if (exact){
		if (compare(newname, 20, name, *length) == 0){
		    bcopy((char *)newname, (char *)lowest, 20 * sizeof(oid));
		    LowState = State;
		    Lowinpcb = inpcb;
		    break;  /* no need to search further */
		}
	    } else {
		if ((compare(newname, 20, name, *length) > 0) &&
		     ((LowState < 0) || (compare(newname, 20, lowest, 20) < 0))){
		    /*
		     * if new one is greater than input and closer to input than
		     * previous lowest, save this one as the "next" one.
		     */
		    bcopy((char *)newname, (char *)lowest, 20 * sizeof(oid));
		    LowState = State;
		    Lowinpcb = inpcb;
		}
	    }
	}
	if (LowState < 0) return(NULL);
	bcopy((char *)lowest, (char *)name, ((int)vp->namelen + 10) * sizeof(oid));
	*length = vp->namelen + 10;
	*write_method = 0;
	*var_len = sizeof(long);
	switch (vp->magic) {
	    case TCPCONNSTATE: {
		static int StateMap[]={1, 2, 3, 4, 5, 8, 6, 10, 9, 7, 11};
		return (u_char *) &StateMap[LowState];
	    }
	    case TCPCONNLOCALADDRESS:
		return (u_char *) &Lowinpcb.inp_laddr.s_addr;
	    case TCPCONNLOCALPORT:
		long_return = ntohs(Lowinpcb.inp_lport);
		return (u_char *) &long_return;
	    case TCPCONNREMADDRESS:
		return (u_char *) &Lowinpcb.inp_faddr.s_addr;
	    case TCPCONNREMPORT:
		long_return = ntohs(Lowinpcb.inp_fport);
		return (u_char *) &long_return;
	}
    }
    return NULL;
}


#ifndef linux
/*
 *	Print INTERNET connections
 */

static int TCP_Count_Connections()
{
	int Established;
	struct inpcb cb;
	register struct inpcb *prev, *next;
	struct inpcb inpcb;
	struct tcpcb tcpcb;

Again:	/*
	 *	Prepare to scan the control blocks
	 */
	Established = 0;

	KNLookup( N_TCB, (char *)&cb, sizeof(struct inpcb));
	inpcb = cb;
	prev = (struct inpcb *) nl[N_TCB].n_value;
	/*
	 *	Scan the control blocks
	 */
	while (inpcb.inp_next != (struct inpcb *) nl[N_TCB].n_value) {
		next = inpcb.inp_next;

		klookup( (int)next, (char *)&inpcb, sizeof (inpcb));
		if (inpcb.inp_prev != prev) {	    /* ??? */
#ifndef linux
		    /* XXX: */
			sleep(1);
#endif
			goto Again;
		}
		if (inet_lnaof(inpcb.inp_laddr) == INADDR_ANY) {
			prev = next;
			continue;
		}
		klookup( (int)inpcb.inp_ppcb, (char *)&tcpcb, sizeof (tcpcb));

		if ((tcpcb.t_state == TCPS_ESTABLISHED) ||
		    (tcpcb.t_state == TCPS_CLOSE_WAIT))
		    Established++;
		prev = next;
	}
	return(Established);
}

#endif


#ifdef linux

/*
 * we will use prev as the next-to-go pointer
 * along the udp_inpcb_list.
 */
static struct inpcb *udp_inpcb_list;
static struct inpcb udp_inpcb, *udp_prev;

static void 
UDP_Scan_Init()
{
    FILE *in;
    char line [256];
    struct inpcb **pp;
    struct timeval now;
    static unsigned long Time_Of_Last_Reload = 0;

    /*
     * save some cpu-cycles, and reload after 5 secs...
     */
    gettimeofday (&now, (struct timezone *) 0);
    if (Time_Of_Last_Reload + 5 > now.tv_sec)
      {
	udp_prev = udp_inpcb_list;
	return;
      }
    Time_Of_Last_Reload = now.tv_sec;


    if (! (in = fopen ("/proc/net/udp", "r")))
      {
	fprintf (stderr, "snmpd: cannot open /proc/net/udp ...\n");
	udp_prev = 0;
	return;
      }

    /* free old chain: */
    while (udp_inpcb_list)
      {
	struct inpcb *p = udp_inpcb_list;
	udp_inpcb_list = udp_inpcb_list->inp_next;
	free (p);
      }

    /* scan proc-file and append: */

    pp = &udp_inpcb_list;
    
    while (line == fgets (line, 256, in))
      {
	struct inpcb pcb, *nnew;
	unsigned int state, lport;

	if (3 != sscanf (line, "%*d: %x:%x %*x:%*x %x", 
			 &pcb.inp_laddr.s_addr, &lport, &state))
	  continue;

	if (state != 7)		/* fix me:  UDP_LISTEN ??? */
	  continue;

	pcb.inp_lport = htons ((unsigned short) (lport));
	pcb.inp_fport = htons (pcb.inp_fport);

	nnew = (struct inpcb *) malloc (sizeof (struct inpcb));
	*nnew = pcb;
	nnew->inp_next = 0;

	*pp = nnew;
	pp = & nnew->inp_next;
      }

    fclose (in);

    /* first entry to go: */
    udp_prev = udp_inpcb_list;
}

static 
int UDP_Scan_Next(State, RetInPcb)
int *State;
struct inpcb *RetInPcb;
{
  struct inpcb *next;

  if (! udp_prev)
    return 0;

  udp_inpcb = *udp_prev;
  *State = udp_inpcb.inp_state;
  next = udp_inpcb.inp_next;
  
  *RetInPcb = udp_inpcb;
  udp_prev = next;

  return 1;		/* "OK" */

}

#endif /* linux */



#ifdef linux
/*
 * we will use prev as the next-to-go pointer
 * along the inpcb_list.
 */
static struct inpcb *inpcb_list;
#endif


static struct inpcb inpcb, *prev;

static void
TCP_Scan_Init()
{
#ifndef linux
    KNLookup( N_TCB, (char *)&inpcb, sizeof(inpcb));
    prev = (struct inpcb *) nl[N_TCB].n_value;
#else /* linux */
    FILE *in;
    char line [256];
    struct inpcb **pp;
    struct timeval now;
    static unsigned long Time_Of_Last_Reload = 0;

    /*
     * save some cpu-cycles, and reload after 5 secs...
     */
    gettimeofday (&now, (struct timezone *) 0);
    if (Time_Of_Last_Reload + 5 > now.tv_sec)
      {
	prev = inpcb_list;
	return;
      }
    Time_Of_Last_Reload = now.tv_sec;


    if (! (in = fopen ("/proc/net/tcp", "r")))
      {
	fprintf (stderr, "snmpd: cannot open /proc/net/tcp ...\n");
	prev = 0;
	return;
      }

    /* free old chain: */
    while (inpcb_list)
      {
	struct inpcb *p = inpcb_list;
	inpcb_list = inpcb_list->inp_next;
	free (p);
      }

    /* scan proc-file and append: */

    pp = &inpcb_list;
    
    while (line == fgets (line, 256, in))
      {
	struct inpcb pcb, *nnew;
	static int linux_states [12] = { 0, 4, 2, 3, 6, 9, 10, 0, 5, 8, 1, 7 };
	int state, lp, fp, uid;

	if (6 != sscanf (line,
			 "%*d: %x:%x %x:%x %x %*X:%*X %*X:%*X %*X %d",
			 &pcb.inp_laddr.s_addr, &lp,
			 &pcb.inp_faddr.s_addr, &fp,
			 &state, &uid))
	  continue;

	pcb.inp_lport = htons ((unsigned short) lp);
	pcb.inp_fport = htons ((unsigned short) fp);

	pcb.inp_state = (state & 0xf) < 12 ? linux_states [state & 0xf] : 1;
	pcb.uid = uid;
    
	nnew = (struct inpcb *) malloc (sizeof (struct inpcb));
	*nnew = pcb;
	nnew->inp_next = 0;

	*pp = nnew;
	pp = & nnew->inp_next;
      }

    fclose (in);

    /* first entry to go: */
    prev = inpcb_list;

#endif /* linux */
}

static int
TCP_Scan_Next(State, RetInPcb)
int *State;
struct inpcb *RetInPcb;
{
	register struct inpcb *next;
#ifndef linux
	struct tcpcb tcpcb;

	if (inpcb.inp_next == (struct inpcb *) nl[N_TCB].n_value) {
	    return(0);	    /* "EOF" */
	}

	next = inpcb.inp_next;

	klookup( (int)next, (char *)&inpcb, sizeof (inpcb));
	if (inpcb.inp_prev != prev)	   /* ??? */
		return(-1); /* "FAILURE" */

	klookup ( (int)inpcb.inp_ppcb, (char *)&tcpcb, sizeof (tcpcb));
	*State = tcpcb.t_state;

#else /* linux */
	if (! prev)
	  return 0;

	inpcb = *prev;
	*State = inpcb.inp_state;
	next = inpcb.inp_next;
#endif

	*RetInPcb = inpcb;
	prev = next;
	return(1);	/* "OK" */
}

static int arptab_size, arptab_current;
static struct arptab *at=0;

static void
ARP_Scan_Init()
{
#ifndef linux
	extern char *malloc();

	if (!at) {
	    KNLookup( N_ARPTAB_SIZE, (char *)&arptab_size, sizeof arptab_size);
	    at = (struct arptab *) malloc(arptab_size * sizeof(struct arptab));
	}

	KNLookup( N_ARPTAB, (char *)at, arptab_size * sizeof(struct arptab));
#else
	FILE *in = fopen ("/proc/net/arp", "r");
	int i, n = 0;
#if 0
	struct arpreq aq;
#endif
	char line [128];
	int za, zb, zc, zd, ze, zf, zg, zh, zi, zj;

	if (! in)
	  {
	    fprintf (stderr, "snmpd: cannot open /proc/net/arp ...\n");
	    arptab_current = 0;
	    return;
	  }
	
	/* 
	 * what a fun: linux-1.1.11 has a binary /proc/net/arp file and 
	 * linux-1.1.12 now in ascii... Thanks to this genius.
	 */
#if 0	
	while (1 == fread (&aq, sizeof (aq), 1, in))
	  n++;
#else
	for (n = -1; fgets (line, 128, in); n++)
	  continue;
#endif
	/* does lseek work for a proc-file ? */
	fclose (in);
	in = fopen ("/proc/net/arp", "r");

	if (at)
	  free (at);
	arptab_size = n;

	if (arptab_size > 0)
	  at = (struct arptab *) malloc(arptab_size * sizeof(struct arptab));
	else
	  at = 0;

	for (i = 0; i < arptab_size; i++)
	  {
	    bzero (at + i, sizeof (at [i]));
#if 0
	    if (1 == fread (&aq, sizeof (aq), 1, in))
	      {
		at [i].at_flags = aq.arp_flags;
		bcopy (&aq.arp_pa.sa_data, &at [i].at_iaddr, 4);
		bcopy (&aq.arp_ha.sa_data, at [i].at_enaddr, 6);
	      }
#else /* ! 0 */
	    /*
	     * as with 1.99.14:
	     * IP-address    HW-type  Flags  HW-address         Mask  Device
	     * 194.45.135.1  0x1      0x0    00:00:00:00:00:00  *     eth0
	     */
	    
	    while (line == fgets (line, 128, in)
		   && 11 != sscanf (line,
				    "%d.%d.%d.%d 0x%*x 0x%x %x:%x:%x:%x:%x:%x",
				    &za, &zb, &zc, &zd, &at [i].at_flags,
				    &ze, &zf, &zg, &zh, &zi, &zj))
	      continue;
	    
	    at [i].at_enaddr [0] = ze;	    at [i].at_enaddr [1] = zf;
	    at [i].at_enaddr [2] = zg;	    at [i].at_enaddr [3] = zh;
	    at [i].at_enaddr [4] = zi;	    at [i].at_enaddr [5] = zj;

	    at [i].at_iaddr.s_addr = (zd << 24) | (zc << 16) | (zb << 8) | za;
#endif /* ! 0 */
	  }
	fclose (in);
#endif /* linux */
	arptab_current = 0;
}

static int 
#ifdef linux
ARP_Scan_Next(IPAddr, PhysAddr, flags)
#else
ARP_Scan_Next(IPAddr, PhysAddr)
#endif
u_long *IPAddr;
char *PhysAddr;
#ifdef linux
int *flags;
#endif
{
	register struct arptab *atab;

	while (arptab_current < arptab_size) {
		atab = &at[arptab_current++];
		if (!(atab->at_flags & ATF_COM)) continue;
		*IPAddr = atab->at_iaddr.s_addr;
#ifdef linux
		if (flags)
		  *flags = atab->at_flags & ~ATF_COM;
		bcopy((char *) atab->at_enaddr, PhysAddr, 6);
#else /* ! linux */

#if defined (sunV3) || defined(sparc)
		bcopy((char *) &atab->at_enaddr, PhysAddr, sizeof(atab->at_enaddr));
#endif
#endif /* ! linux */
#if defined(mips) || defined(ibm032) 
		bcopy((char *)  atab->at_enaddr, PhysAddr, sizeof(atab->at_enaddr));
#endif
	return(1);
	}
	return(0);	    /* "EOF" */
}


#ifndef sunV3
static struct in_ifaddr savein_ifaddr;
#endif
static struct ifnet *ifnetaddr, saveifnet, *saveifnetaddr;
static int saveIndex=0;
static char saveName[16];

#ifdef linux
/*
 * ifnetaddr is the next to go iface along the ifnetaddr_list:
 */
static struct ifnet *ifnetaddr_list;
#endif

void
Interface_Scan_Init()
{
#ifndef linux
    KNLookup (N_IFNET, (char *)&ifnetaddr, sizeof(ifnetaddr));
    saveIndex=0;
#else /* linux */
    char line [128], fullname [20], ifname_buf [20], *ifname, *ptr;
    struct ifreq ifrq;
    struct ifnet **ifnetaddr_ptr;
    FILE *devin;
    int a, b, c, d, e, i, fd;
    extern conf_if_list *if_list;
    conf_if_list *if_ptr;

    saveIndex = 0;

    /* free old list: */
    while (ifnetaddr_list)
      {
	struct ifnet *old = ifnetaddr_list;
	ifnetaddr_list = ifnetaddr_list->if_next;
	free (old->if_name);
	free (old);
      }

    ifnetaddr = 0;
    ifnetaddr_ptr = &ifnetaddr_list;

    if ((fd = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
      {
	fprintf (stderr, "cannot open inet/dgram socket - continuing...\n");
	return; /** exit (1); **/
      }

    /*
     * build up ifnetaddr list by hand: 
     */
    
    /* at least linux v1.3.53 says EMFILE without reason... */
    if (! (devin = fopen ("/proc/net/dev", "r"))
	&& ! (devin = fopen ("/proc/net/dev", "r"))
	&& ! (devin = fopen ("/proc/net/dev", "r")))
      {
	close (fd);
	fprintf (stderr, "cannot open /proc/net/dev - continuing...\n");
	return; /** exit (1); **/
      }

    i = 0;
    while (fgets (line, 256, devin))
      {
	struct ifnet *nnew;

	if (6 != sscanf (line, "%[^:]: %d %d %*d %*d %*d %d %d %*d %*d %d",
			 ifname_buf, &a, &b, &c, &d, &e))
	  continue;
	
	nnew = (struct ifnet *) malloc (sizeof (struct ifnet));	    
	bzero ((char *) nnew, sizeof (struct ifnet));
	
	/* chain in: */
	*ifnetaddr_ptr = nnew;
	ifnetaddr_ptr = &nnew->if_next;
	i++;
	
	/* linux previous to 1.3.~13 may miss transmitted loopback pkts: */
	if (! strcmp (ifname_buf, "lo") && a > 0 && ! c)
	  c = a;

	nnew->if_ipackets = a, nnew->if_ierrors = b, nnew->if_opackets = c,
	nnew->if_oerrors = d, nnew->if_collisions = e;
	
	/* ifnames are given as ``   eth0'': split in ``eth'' and ``0'': */
	for (ifname = ifname_buf; *ifname && *ifname == ' '; ifname++) ;
	
	/* set name and interface# : */
	nnew->if_name = strdup (ifname);
	for (ptr = nnew->if_name; *ptr && (*ptr < '0' || *ptr > '9'); 
	     ptr++) ;
	nnew->if_unit = (*ptr) ? atoi (ptr) : 0;
	*ptr = 0;

	sprintf (fullname, "%s%d", nnew->if_name, nnew->if_unit);

	strcpy (ifrq.ifr_name, ifname);
	if (ioctl (fd, SIOCGIFADDR, &ifrq) < 0)
	  memset ((char *) &nnew->if_addr, 0, sizeof (nnew->if_addr));
	else
	  nnew->if_addr = ifrq.ifr_addr;

	strcpy (ifrq.ifr_name, ifname);
	if (ioctl (fd, SIOCGIFBRDADDR, &ifrq) < 0)
	  memset ((char *)&nnew->ifu_broadaddr, 0, sizeof(nnew->ifu_broadaddr));
	else
	  nnew->ifu_broadaddr = ifrq.ifr_broadaddr;

	strcpy (ifrq.ifr_name, ifname);
	if (ioctl (fd, SIOCGIFNETMASK, &ifrq) < 0)
 	  memset ((char *)&nnew->ia_subnetmask, 0, sizeof(nnew->ia_subnetmask));
	else
	  nnew->ia_subnetmask = ifrq.ifr_netmask;
	  
	strcpy (ifrq.ifr_name, ifname);
	nnew->if_flags = ioctl (fd, SIOCGIFFLAGS, &ifrq) < 0 
	  		? 0 : ifrq.ifr_flags;
	
	strcpy (ifrq.ifr_name, ifname);
	if (ioctl(fd, SIOCGIFHWADDR, &ifrq) < 0)
	  bzero (nnew->if_hwaddr, 6);
	else
	  bcopy (ifrq.ifr_hwaddr.sa_data, nnew->if_hwaddr, 6);
	    
	strcpy (ifrq.ifr_name, ifname);
	nnew->if_metric = ioctl (fd, SIOCGIFMETRIC, &ifrq) < 0
	  		? 0 : ifrq.ifr_metric;
	    
	strcpy (ifrq.ifr_name, ifname);
	nnew->if_mtu = (ioctl (fd, SIOCGIFMTU, &ifrq) < 0) 
			  ? 0 : ifrq.ifr_mtu;

	for (if_ptr = if_list; if_ptr; if_ptr = if_ptr->next)
	    if (! strcmp (if_ptr->name, fullname))
	      break;

	if (if_ptr)
	  {
	    nnew->if_type = if_ptr->type;
	    nnew->if_speed = if_ptr->speed;
	  }
	else {
	  nnew->if_type = ! strcmp (nnew->if_name, "lo") ? 24 :
	    ! strcmp (nnew->if_name, "eth") ? 6 :
	      ! strcmp (nnew->if_name, "sl") ? 28 : 1;
	  
	  nnew->if_speed = nnew->if_type == 6 ? 10000000 : 
	    nnew->if_type == 24 ? 10000000 : 0;
	}

      } /* while (fgets ... */

      ifnetaddr = ifnetaddr_list;

#if 0
    { struct ifnet *x = ifnetaddr;
      printf ("* see: known interfaces:");
      while (x)
	{
	  printf (" %s", x->if_name);
	  x = x->if_next;
	}
      printf ("\n");
    } /* XXX */
#endif

    fclose (devin);
    close (fd);
#endif /* linux */
}



#ifdef sunV3
/*
**  4.2 BSD doesn't have ifaddr
**  
*/
int Interface_Scan_Next(Index, Name, Retifnet)
int *Index;
char *Name;
struct ifnet *Retifnet;
{
	struct ifnet ifnet;
	register char *cp;
#ifndef linux
	extern char *index();
#endif

	while (ifnetaddr) {
	    /*
	     *	    Get the "ifnet" structure and extract the device name
	     */
#ifndef linux
	    klookup((int)ifnetaddr, (char *)&ifnet, sizeof ifnet);
	    klookup((int)ifnet.if_name, (char *)saveName, 16);
#else
	    ifnet = *ifnetaddr;
	    strcpy (saveName, ifnet.if_name);
#endif
	    if (strcmp(saveName, "ip") == 0) {
		ifnetaddr = ifnet.if_next;
		continue;
	    }



 	    saveName[15] = '\0';
	    cp = index(saveName, '\0');
	    *cp++ = ifnet.if_unit + '0';
	    *cp = '\0';
	    if (1 || strcmp(saveName,"lo0") != 0) {  /* XXX */

		if (Index)
		    *Index = ++saveIndex;
		if (Retifnet)
		    *Retifnet = ifnet;
		if (Name)
		    strcpy(Name, saveName);
		saveifnet = ifnet;
		saveifnetaddr = ifnetaddr;
		ifnetaddr = ifnet.if_next;

		return(1);	/* DONE */
	    } 
	    ifnetaddr = ifnet.if_next;
	}
	return(0);	    /* EOF */
}


#else

int Interface_Scan_Next(Index, Name, Retifnet, Retin_ifaddr)
int *Index;
char *Name;
struct ifnet *Retifnet;
struct in_ifaddr *Retin_ifaddr;
{
	struct ifnet ifnet;
	struct in_ifaddr *ia, in_ifaddr;
	register char *cp;
	extern char *index();

	while (ifnetaddr) {
	    /*
	     *	    Get the "ifnet" structure and extract the device name
	     */
	    klookup((int)ifnetaddr, (char *)&ifnet, sizeof ifnet);
	    klookup((int)ifnet.if_name, (char *)saveName, 16);

	    saveName[15] = '\0';
	    cp = index(saveName, '\0');
	    *cp++ = ifnet.if_unit + '0';
	    *cp = '\0';
	    if (1 || strcmp(saveName,"lo0") != 0) {  /* XXX */
		/*
		 *  Try to find an address for this interface
		 */

		KNLookup(N_IN_IFADDR, (char *)&ia, sizeof(ia));
		while (ia) {
		    klookup((int)ia ,  (char *)&in_ifaddr, sizeof(in_ifaddr));
		    if (in_ifaddr.ia_ifp == ifnetaddr) break;
		    ia = in_ifaddr.ia_next;
		}

		ifnet.if_addrlist = (struct ifaddr *)ia;     /* WRONG DATA TYPE; ONLY A FLAG */
/*		ifnet.if_addrlist = (struct ifaddr *)&ia->ia_ifa;   */  /* WRONG DATA TYPE; ONLY A FLAG */

		if (Index)
		    *Index = ++saveIndex;
		if (Retifnet)
		    *Retifnet = ifnet;
		if (Retin_ifaddr)
		    *Retin_ifaddr = in_ifaddr;
		if (Name)
		    strcpy(Name, saveName);
		saveifnet = ifnet;
		saveifnetaddr = ifnetaddr;
		savein_ifaddr = in_ifaddr;
		ifnetaddr = ifnet.if_next;

		return(1);	/* DONE */
	    }
	    ifnetaddr = ifnet.if_next;
	}
	return(0);	    /* EOF */
}


#endif sunV3




#ifdef sunV3

static int Interface_Scan_By_Index(Index, Name, Retifnet)
int Index;
char *Name;
struct ifnet *Retifnet;
{
	int i;
#if 1
	static time_t last = 0;
	time_t now = time ((time_t *) 0);

	/*
	 * allow ``optimisation'' only for a period of one second;
	 * if a single var is requested, there would be never an update...
	 */
	if (last + 1 < now)
	  {
	      last = now;
	      saveIndex = -1;
	  }
#endif
	if (saveIndex != Index) {	/* Optimization! */
	    Interface_Scan_Init();
	    while (Interface_Scan_Next(&i, Name, Retifnet)) {
		if (i == Index) break;
	    }
	    if (i != Index) return(-1);     /* Error, doesn't exist */
	} else {
	    if (Retifnet)
		*Retifnet = saveifnet;
	    if (Name)
		strcpy(Name, saveName);
	}
	return(0);	/* DONE */
}

#else

static int Interface_Scan_By_Index(Index, Name, Retifnet, Retin_ifaddr)
int Index;
char *Name;
struct ifnet *Retifnet;
struct in_ifaddr *Retin_ifaddr;
{
	int i;
#if 1
	static time_t last = 0;
	time_t now = time ((time_t *) 0);

	/* allow ``optimisation'' only for a period of one second: */
	if (last + 1 < now)
	  {
	      last = now;
	      saveIndex = -1;
	  }
#endif
	if (saveIndex != Index) {	/* Optimization! */
	    Interface_Scan_Init();
	    while (Interface_Scan_Next(&i, Name, Retifnet, Retin_ifaddr)) {
		if (i == Index) break;
	    }
	    if (i != Index) return(-1);     /* Error, doesn't exist */
	} else {
	    if (Retifnet)
		*Retifnet = saveifnet;
	    if (Retin_ifaddr)
		*Retin_ifaddr = savein_ifaddr;
	    if (Name)
		strcpy(Name, saveName);
	}
	return(0);	/* DONE */
}

#endif


static int Interface_Count=0;

static int Interface_Scan_Get_Count()
{

	if (!Interface_Count) {
	    Interface_Scan_Init();
#ifdef sunV3
	    while (Interface_Scan_Next((int *)0, (char *)0, (struct ifnet *)0) != 0) {
#else
	    while (Interface_Scan_Next((int *)0, (char *)0, (struct ifnet *)0, (struct in_ifaddr *)0) != 0) {
#endif
		Interface_Count++;
	    }
	}
	return(Interface_Count);
}


static int Interface_Get_Ether_By_Index(Index, EtherAddr)
int Index;
u_char *EtherAddr;
{
	int i;
#ifndef linux
	struct arpcom arpcom;
#else
	struct arpcom {
	  char ac_enaddr [6];
	} arpcom;
#endif

	if (saveIndex != Index) {	/* Optimization! */

	    Interface_Scan_Init();

#ifdef sunV3
	    while (Interface_Scan_Next((int *)&i, (char *)0, (struct ifnet *)0) != 0) {
#else
	    while (Interface_Scan_Next((int *)&i, (char *)0, (struct ifnet *)0, (struct in_ifaddr *)0) != 0) {
#endif
		if (i == Index) break;
	    }
	    if (i != Index) return(-1);     /* Error, doesn't exist */
	}

	/*
	 *  the arpcom structure is an extended ifnet structure which
	 *  contains the ethernet address.
	 */
#ifndef linux
	klookup((int)saveifnetaddr, (char *)&arpcom, sizeof (struct arpcom));
#else
        bcopy (saveifnetaddr->if_hwaddr, arpcom.ac_enaddr, 6);
#endif
	if (strncmp("lo", saveName, 2) == 0) {
	    /*
	     *  Loopback doesn't have a HW addr, so return 00:00:00:00:00:00
	     */
	    bzero(EtherAddr, sizeof(arpcom.ac_enaddr));

	} else {
#if defined(sunV3) || defined(sparc)
	    bcopy((char *) &arpcom.ac_enaddr, EtherAddr, sizeof (arpcom.ac_enaddr));
#endif
#ifdef mips
	    bcopy((char *)  arpcom.ac_enaddr, EtherAddr, sizeof (arpcom.ac_enaddr));
#endif


	}
	return(0);	/* DONE */
}



#if defined(mips) || defined(ibm032) || defined(sunV3)


/*
**  Lets read the process table in blocks so as to 
**  minimize sys calls
*/
#ifndef linux
#define PROCBLOC 16
struct proc procbuf[PROCBLOC];
#endif


u_char *
var_process(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;   /* IN - pointer to variable entry that points here */
    register oid	*name;	    /* IN/OUT - input name requested, output name found */
    register int	*length;    /* IN/OUT - length of input and output oid's */
    int			exact;	    /* IN - TRUE if an exact match was requested. */
    int			*var_len;   /* OUT - length of variable or 0 if function returned. */
    int			(**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
#ifndef linux
    oid			newname[MAX_NAME_LEN];
    register int	slotindex;
    register int        numread, i;
    int result, count;
    off_t   procp;
    struct proc	*proc;


    /* NOW BROKEN 6/92 */
    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    bzero(return_buf, 256);

    /* find "next" process */



    if (KNLookup(N_PROC,  (char *)&procp, sizeof(procp)) == NULL) {
	return (NULL);
    }
    if (KNLookup(N_NPROC, (char *)&count, sizeof(count)) == NULL) {
	return (NULL);
    }

    proc = NULL;
    slotindex = 0;
    while ((!proc) && (slotindex < count)) {
      
        numread = MIN(count - slotindex, PROCBLOC);
        if (klookup((int)procp, (char *)procbuf,
		    numread * sizeof(struct proc)) == NULL) {
	    return(NULL);
	}
	procp += sizeof(struct proc) * numread;

	for (i=0; i < numread; i++) {
	    slotindex++;

	    if ((procbuf[i].p_stat == 0) || (procbuf[i].p_stat == SZOMB)) {
	        continue;
	    }
	    newname[13] = (oid) slotindex;
	    result = compare(name, *length, newname, (int)vp->namelen);
	    if ((exact && (result == 0)) || (!exact && (result < 0))) {
	        proc = &procbuf[i];
	        break;
	    }
	}
    }


    if (!proc) {
	return NULL;
    }

    bcopy((char *)newname, (char *)name, (int)vp->namelen * sizeof(oid));
    *length = vp->namelen;
    *write_method = 0;
    *var_len = sizeof(long);

    switch (vp->magic){
	case PROCESSSLOTINDEX:
	    long_return = slotindex;
	    return (u_char *) &long_return;
	case PROCESSID:
	    long_return = proc->p_pid;
	    return (u_char *) &long_return;
	case PROCESSCOMMAND:
	    *var_len = get_command(proc, return_buf);
	    return (u_char *)return_buf;
	default:
	    ERROR("");
    }
    return NULL;
#else /* linux */
    return NULL;
#endif /*linux */
}





int
get_command(proc, buf)
     struct proc *proc;
     char *buf;
{
#ifndef linux

#if defined(ibm032) 
    struct user u;
struct userx
{
        char userfill[UPAGES*NBPG-sizeof (struct user)];
	struct user user;
};

#ifdef BSD4_3
#define REDSIZE CLSIZE*2		/* red zone size plus reserved page */
#else
#define REDSIZE CLSIZE			/* red zone size */
#endif BSD4_3

union {
        struct	userx userx;
	char	upages[UPAGES][NBPG];
} user;
#define U	user.userx.user
#else

#define REDSIZE 0		/* red zone size */

union {
	struct	user user;
	char	upages[UPAGES][NBPG];
} user;
#define u	user.user
#endif 

    struct pte *pteaddr, apte;
    struct	pte *Usrptmap, *usrpt;

#ifdef mips
    struct pte arguutl[UPAGES];
    struct pte wpte[UPAGES];
#endif
#ifdef sunV3
    struct pte uutl[UPAGES];
    struct pte arguutl[UPAGES];
#endif


    union {
	    char	argc[CLSIZE*NBPG];
	    int 	argi[CLSIZE*NBPG/sizeof (int)];
    } argspac;



    int	argaddr;
    struct dblock db;
    register char *cp;
    register int *ip;
    char c;
    int ncl;
    int    i;
    int usersize, size;
    long addr;
    int  nbad;

    /*
     *  Handle the 2 system procs now so 
     *  we don't have to worry about them latter
     */
    if (proc->p_pid == 0){
	strcpy(buf, "swapper");
	return strlen(buf);
    }
    if (proc->p_pid == 2){
	strcpy(buf, "pagedaemon");
	return strlen(buf);
    }


#ifdef ibm032
        size = ctob(UPAGES);
#endif
#ifdef mips
	size = sizeof (struct user);
#endif
#ifdef sunV3
	size = roundup(sizeof (struct user), DEV_BSIZE);
#endif

    /*
     *  We don't deal with Zombies and the like...
     */
    if (proc->p_stat == SZOMB || proc->p_flag & (SSYS | SWEXIT)){
	strcpy((char *)buf, "");
	return strlen(buf);
    }

#ifdef ibm032

    if (KNLookup(N_USERSIZE, (char *)&usersize, sizeof(usersize)) == NULL) {
	return(0);
    }
#endif

    usrpt = (struct pte *)nl[N_USRPT].n_value;
    Usrptmap = (struct pte *)nl[N_USRPTMAP].n_value;

    /*
     *  Is our target proc in core??
     */
    if ((proc->p_flag & SLOAD) == 0){
      /*
       *  Not in core -- poke (peek, actually [hopefully]) around swap for u. struct 
       */
	lseek(swap, (long)dtob(proc->p_swaddr), 0);

	if (read(swap, (char *)user.upages, size) != size) {
	        ERROR("");
		return (0);
	}
#ifdef ibm032
	if ((i = usersize - sizeof (struct user)) > 0)
	    bcopy(((char *) &U) - i, (char *) &u, sizeof (struct user));	
	   /* fake the location of the u structure */
	else
	    u = U;   /* added 8-9-85 for consistency */ 
#endif
	argaddr = 0;
    } else {




#ifdef sunV3
	pteaddr = &Usrptmap[btokmx(sptopte(proc, CLSIZE-1))];
#endif sunV3
#ifdef ibm032
	pteaddr = &Usrptmap[btokmx(proc->p_p0br) + proc->p_szpt - 1];
#endif ibm032
#ifdef mips
	pteaddr = &Usrptmap[btokmx(proc->p_stakbr)+proc->p_stakpt-1];
#endif mips

	if (klookup((long)pteaddr, (char *)&apte, sizeof(apte)) == NULL) {
	    ERROR("");
	    return(0);
	}

#ifdef sunV3
	addr = (long)ctob(apte.pg_pfnum) + (((int)sptopte(proc, CLSIZE-1))&PGOFSET);     
#endif sunV3
#ifdef ibm032
	addr = (long)ctob(apte.pg_pfnum+1) - (UPAGES+CLSIZE+REDSIZE) * sizeof (struct pte);     
#endif ibm032
#ifdef mips
	addr = (long)ctob(apte.pg_pfnum) + NBPG - ((REDZONEPAGES+1) * sizeof(struct pte));
#endif mips

	lseek(mem, addr, 0);  
#ifdef sunV3
	if (read(mem, (char *)arguutl, sizeof(struct pte)) != sizeof(struct pte)) {
#else 
	if (read(mem, (char *)arguutl, sizeof(arguutl)) != sizeof(arguutl)) {
#endif
		printf("can't read page table for u of pid %d from /dev/mem\n",
		    proc->p_pid);
		return (0);
	}


	if (arguutl[0].pg_fod == 0 && arguutl[0].pg_pfnum) {
		argaddr = ctob(arguutl[0].pg_pfnum);
	} else {
		argaddr = 0;
	}



#ifdef mips
	if (klookup((long)proc->p_addr, (char *)wpte, sizeof(wpte)) == NULL) {
	    return(0);
	}
#endif mips



#ifdef sunV3

	pteaddr = &Usrptmap[btokmx(proc->p_addr)];
	if (klookup((long)pteaddr, (char *)&apte, sizeof(apte)) == NULL) {
	    return(0);
	}
	addr = (long)ctob(apte.pg_pfnum) + (((int)proc->p_addr)&PGOFSET);
	lseek(mem, addr, 0); 
	if (read(mem, (char *)uutl, sizeof(uutl)) != sizeof(uutl)) {
		printf("cant read page table for u of pid %d from /dev/mem\n",
		    proc->p_pid);
		return (0);
	}

#endif sunV3
	

	ncl = (size + NBPG*CLSIZE - 1) / (NBPG*CLSIZE);

	while (--ncl >= 0) {
		i = ncl * CLSIZE;
#ifdef ibm032
		addr = (long)ctob(arguutl[CLSIZE+REDSIZE+i].pg_pfnum);

		if (addr == 0) {
			bzero((char *) &u, sizeof (struct user));
			return(1);	/* faked for swapper */
		}
#endif
#ifdef mips
		addr = (long)ctob(wpte[i].pg_pfnum);
#endif mips
#ifdef sunV3
		addr = (long)ctob(uutl[i].pg_pfnum);
#endif sunV3

		lseek(mem, addr, 0);
		if (read(mem, user.upages[i], CLSIZE*NBPG) != CLSIZE*NBPG) {
			printf("cant read page from /dev/mem\n");
			return(0);
		}
	}
#ifdef ibm032
	if ((i = usersize - sizeof (struct user)) > 0)
	    bcopy(((char *) &U) - i, (char *) &u, sizeof (struct user));	
	    /* fake the location of the u structure */
	else
	  u = U;	/* return the structure */
#endif

    }


#ifdef sunV3
    if (u.u_ssize == 0) {
	(void) strcpy(buf, " (");
	(void) strncat(buf, u.u_comm, sizeof (u.u_comm));
	(void) strcat(buf, ")");
	return strlen(buf);
    }
#endif sunV3

    if ((proc->p_flag & SLOAD) == 0 || argaddr == 0){
#if !defined(ibm032) || !defined(BSD4_3)
	vstodb(0, CLSIZE, &u.u_smap, &db, 1);
#else
	vstodb(CLSIZE, CLSIZE, &u.u_smap, &db, 1);
#endif

	lseek(swap, (long)dtob(db.db_base), 0);
 	if (read(swap, (char *)&argspac, sizeof(argspac)) != sizeof(argspac)) {
	  ERROR("");
	}
    } else {
        lseek(mem, (long)argaddr, 0);
        if (read(mem, (char *)&argspac, sizeof(argspac)) != sizeof(argspac)) {
	  ERROR("");
	}
    }

#if defined(ibm032) && defined(NFL) && !defined(BSD4_3)
        ip = &argspac.argi[CLSIZE*NBPG/sizeof (int)];
	ip -= sizeof (struct fp_mach) / sizeof (int);
        ip -= 2;		/* last arg word and .long 0 */
#endif ibm032
#ifdef sunV3
        ip = &argspac.argi[CLSIZE*NBPG/sizeof (int)];
        ip -= 2;
#endif
#ifdef mips
	ip = &argspac.argi[(CLSIZE*NBPG-EA_SIZE)/sizeof (int)];
        while (*--ip == 0) {
	    if (ip == argspac.argi) {
		(void) strcpy(buf, " (");
		(void) strncat(buf, u.u_comm, sizeof (u.u_comm));
 		(void) strcat(buf, ")");
		return strlen(buf);
	    }
	}
#endif mips

    while (*--ip)
	    if (ip == argspac.argi){
		(void) strcpy(buf, " (");
		(void) strncat(buf, u.u_comm, sizeof (u.u_comm));
		(void) strcat(buf, ")");
		return strlen(buf);
	    }
    *(char *)ip = ' ';
    ip++;

    nbad = 0;

#ifndef mips
      for (cp = (char *)ip; cp < &argspac.argc[CLSIZE*NBPG]; cp++) {
#else
      for (cp = (char *)ip; cp < &argspac.argc[CLSIZE*NBPG-EA_SIZE]; cp++) {
#endif mips    
	c = *cp & 0177;
	if (c == 0) {
	  *cp = ' ';
	} else if (c < ' ' || c > 0176) {
	  if (++nbad >= 5) {
	    *cp++ = ' ';
	    break;
	  }
	  *cp = '?';
	} else if (c == '=') {
	  while (*--cp != ' ')
	    if (cp <= (char *)ip) {
	      break;
	    }
	  break;
	}
      }
      
      *cp = 0;
      while (*--cp == ' ') {
	*cp = 0;
      }
      cp = (char *)ip;
      strncpy(buf, cp, &argspac.argc[CLSIZE*NBPG] - cp);
      if (cp[0] == '-' || cp[0] == '?' || cp[0] <= ' ') {
	(void) strcat(buf, " (");
	(void) strncat(buf, u.u_comm, sizeof(u.u_comm));
	(void) strcat(buf, ")");
      }
      return strlen(buf);
#else /* linux */
      strcpy (buf, "<unknown>");
      return strlen (buf);
#endif /*linux */
}



#ifndef linux


vstodb(vsbase, vssize, dmp, dbp, rev)
	register int vsbase;
	int vssize;
	struct dmap *dmp;
	register struct dblock *dbp;
{
	int	dmmin, dmmax, nswap;
	register int blk;
	register swblk_t *ip = dmp->dm_map;

	if (KNLookup(N_DMMIN, (char *)&dmmin, sizeof(dmmin)) == NULL ||
	    KNLookup(N_DMMAX, (char *)&dmmax, sizeof(dmmax)) == NULL ||
	    KNLookup(N_NSWAP, (char *)&nswap, sizeof(nswap)) == NULL) {
	    ERROR("");
	    return(0);
	}

	blk = dmmin;
	vsbase = ctod(vsbase);
	vssize = ctod(vssize);
	if (vsbase < 0 || vsbase + vssize > dmp->dm_size) {
	    ERROR("vstodb\n");
	    return(0);
	}
	while (vsbase >= blk) {
	    vsbase -= blk;
	    if (blk < dmmax) {
	    	blk *= 2;
	    }
	    ip++;
	}
	if (*ip <= 0 || *ip + blk > nswap) {
	    ERROR("vstodb *ip\n");
	    return(0);
	}
	dbp->db_size = (vssize < (blk - vsbase)) ?  vssize : (blk - vsbase);
	dbp->db_base = *ip + (rev ? blk - (vsbase + dbp->db_size) : vsbase);
}

#endif 
#endif 

u_char *
var_snmpStats(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;    /* IN - pointer to variable entry that points here */
    oid     *name;	    /* IN/OUT - input name requested, output name found */
    int     *length;	    /* IN/OUT - length of input and output oid's */
    int     exact;	    /* IN - TRUE if an exact match was requested. */
    int     *var_len;	    /* OUT - length of variable or 0 if function returned. */
    int     (**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    oid newname[MAX_NAME_LEN];
    int result;
    extern u_long snmpStats[];

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    newname[10] = 0;
    result = compare(name, *length, newname, (int)vp->namelen + 1);
    if ((exact && (result != 0)) || (!exact && (result >= 0)))
        return NULL;
    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;

    *write_method = 0;
    *var_len = sizeof(long); /* all following variables are sizeof long */

    switch (vp->magic){
    case 1:
	return (u_char *)&snmpStats[SNMP_STAT_PACKETS];
    case 3:
	return (u_char *)&snmpStats[SNMP_STAT_ENCODING_ERRORS];
    case 11:
	return (u_char *)&snmpStats[SNMP_STAT_BAD_OPERATIONS];
    case 12:
	return (u_char *)&snmpStats[SNMP_STAT_SILENT_DROPS];
    case 13:
	return (u_char *)&snmpStats[SNMP_STAT_PROXY_DROPS];
    case 101:
	return (u_char *)&snmpStats[SNMP_STAT_V1_BAD_COMMUNITY_NAMES];
    case 102:
	return (u_char *)&snmpStats[SNMP_STAT_V1_BAD_COMMUNITY_USES];
    default:
	ERROR("");
    }
    return NULL;
}

u_char *
var_usecAgent(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;    /* IN - pointer to variable entry that points here */
    oid     *name;	    /* IN/OUT - input name requested, output name found */
    int     *length;	    /* IN/OUT - length of input and output oid's */
    int     exact;	    /* IN - TRUE if an exact match was requested. */
    int     *var_len;	    /* OUT - length of variable or 0 if function returned. */
    int     (**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    oid newname[MAX_NAME_LEN];
    int result;
    static u_long now;
    extern u_char _agentID[];
    extern u_long _agentBoots;
    extern u_long _agentStartTime;
    extern u_long _agentSize;

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    newname[10] = 0;
    result = compare(name, *length, newname, (int)vp->namelen + 1);
    if ((exact && (result != 0)) || (!exact && (result >= 0)))
        return NULL;
    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;

    *write_method = 0;
    *var_len = sizeof(long);

    switch (vp->magic){
    case 1:
        *var_len = 12;
	return (u_char *)_agentID;
    case 2:
	return (u_char *)&_agentBoots;
    case 3:
	now = _agentStartTime + time(NULL);
	return (u_char *)&now;
    case 4:
	return (u_char *)&_agentSize;
    default:
        ERROR("");
    }
    return NULL;
}

u_char *
var_usecStats(vp, name, length, exact, var_len, write_method)
    register struct variable *vp;    /* IN - pointer to variable entry that points here */
    oid     *name;	    /* IN/OUT - input name requested, output name found */
    int     *length;	    /* IN/OUT - length of input and output oid's */
    int     exact;	    /* IN - TRUE if an exact match was requested. */
    int     *var_len;	    /* OUT - length of variable or 0 if function returned. */
    int     (**write_method)(); /* OUT - pointer to function to set variable, otherwise 0 */
{
    oid newname[MAX_NAME_LEN];
    int result;
    extern u_long snmpStats[];

    bcopy((char *)vp->name, (char *)newname, (int)vp->namelen * sizeof(oid));
    newname[10] = 0;
    result = compare(name, *length, newname, (int)vp->namelen + 1);
    if ((exact && (result != 0)) || (!exact && (result >= 0)))
        return NULL;
    bcopy((char *)newname, (char *)name, ((int)vp->namelen + 1) * sizeof(oid));
    *length = vp->namelen + 1;

    *write_method = 0;
    *var_len = sizeof(long); /* all following variables are sizeof long */

    switch (vp->magic){
    case 1:
	return (u_char *)&snmpStats[USEC_STAT_UNSUPPORTED_QOS];
    case 2:
	return (u_char *)&snmpStats[USEC_STAT_NOT_IN_WINDOWS];
    case 3:
	return (u_char *)&snmpStats[USEC_STAT_UNKNOWN_USERNAMES];
    case 4:
	return (u_char *)&snmpStats[USEC_STAT_WRONG_DIGEST_VALUES];
    case 5:
	return (u_char *)&snmpStats[USEC_STAT_UNKNOWN_CONTEXT_SELECTORS];
    case 6:
	return (u_char *)&snmpStats[USEC_STAT_BAD_PARAMETERS];
    case 7:
	return (u_char *)&snmpStats[USEC_STAT_UNAUTHORIZED_OPERATIONS];
    default:
        ERROR("");
    }
    return NULL;
}
