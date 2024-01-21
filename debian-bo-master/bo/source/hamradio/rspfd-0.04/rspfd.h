#ifndef _rspfd_h_
#define _rspfd_h_
/*
 * rspfd.h
 *
 * RSPF Daemon defines.  This holds all the structures that are used by
 * RSPF itself.  Also there is some information about IP and AX.25 levels
 * to strip out the RSPF packet.
 *
 * Changes: (selected highlights!)
 *
 * 20/02/95 cs  Created (v0.01)
 * 16/03/95 cs  It now talks, move ax25 code out to functions (v0.02)
 * 23/03/95 cs  Fixed up bugs in ping queue
 * 06/04/95 cs	Changed the queues, added router incoming code
 * 12/05/95 cs  Moved a lot of stuff to other files
 * 30/06/96 cs  v0.03, nodegroups work, router addres is interface address
 *		use debug flag, use dynamic flag to determine whic routes to
 *		overwrite
 */

#define CONFIG_FILE "/etc/ax25/rspfd.conf"

/* Function prototypes */
unsigned short rspf_check(unsigned char *rh, int len,
	 unsigned long saddr, unsigned long daddr);
u_short in_cksum(u_short *addr, int len); 
u_char get_horizon(u_long addr, char *iface);
int get_rtr_bull(u_long addr, u_char *buf, int len);
int get_my_bull(u_long addr, u_char *buf, int len);
char* in_ntoa(u_long addr);

#define RSPF_VERSION 22

#define RSPF_MINLEN	11
#define PROC_PATH	"/proc/net/ax25_route"

#define ALEN	6	/* Size of AX25 address */
#define AXLEN	7	/* Size of AX25 address in packet */
#define SSIDMASK	0x1e	/* Mask for SSID bits */
#define REPEATEDMASK	0x80	/* Flag for packet that has been through repeater */

/* Defines for the PID byte */
#define	PID_SEGMENT	0x08
#define	PID_ARP		0xCD
#define	PID_NETROM	0xCF
#define	PID_IP		0xCC
#define	PID_X25		0x01
#define	PID_TEXNET	0xC3
#define	PID_FLEXNET	0xCE
#define	PID_NO_L3	0xF0

#define AXALEN		9	/* Address of callsign, plus SSID and 0 */

#define IPPROTO_RSPF	73

/* Defines for RSPF type byte */
#define TYPE_RRH	0x03
#define TYPE_ROUTING	0x01

typedef enum {
	Ok,
	Tentative,
	Suspect,
	Bad
}  AdjStatus;
	



	
	
	
struct rspfrrh {
	u_char version;
	u_char type;
	u_short checksum;
	u_long addr;
	u_short tx_pkts;
	u_char  flags;
};
#define RSPF_RRH_LEN	11



struct router {
	u_long	addr;
	u_int	seq_no;
	u_int	sub_seq_no;
	time_t	bulltime;
};

struct rspf_route {
	u_long addr;
	u_char sigbits;
	u_char cost;
	char port[IFNAMSIZ];
};

struct rspf_adj{
	u_long	addr;			/* Ip address */
	ax25_address	dladdr;		/* AX.25 address */
	char 	port[IFNAMSIZ];		/* Port that adj was heard on */
	u_int	rx_pkts;		/* pkt sequence # from /proc */
	u_int	tx_pkts;		/* # pkts sent from RRH if there*/
	time_t	rrhtime;		/* Time of last RRH, 0 if none */
	u_char	rx_ratio;		/* Rx / Tx ratio */
	int cost;
	u_char horizon;
	AdjStatus status;
};

struct link {
	u_short seq_no;
	u_long	saddr;
	u_long	daddr;
	u_char	sigbits;
	u_char	horizon;
	u_char	cost;
};

struct path {
	u_long daddr;
	u_long adjaddr;
	u_char sigbits;
	u_long paddr;
	u_char cost;
	
};

struct pings {
	u_long	addr;
	ax25_address dladdr;
	char	port[IFNAMSIZ];
	int 	id;
	char	count;
	int	echo_id;
};
		
struct rspfroute_hdr {
	u_char version;
	u_char type;
	u_char frag;
	u_char frag_tot;
	u_short checksum;
	u_char sync;
	u_char nodes;

	u_short env_no;
};	
#define RSPFROUTE_LEN 10

struct rspfnode_hdr{
	u_char	addr[4];
	u_short	seq_no;
	u_char	sub_seq_no;
	u_char	links;
};
#define RSPFNODE_LEN	8

struct rspflink_hdr {
	u_char	horizon;
	u_char	erp;
	u_char	cost;
	u_char	adjacencies;
};
#define RSPFLINK_LEN	4

struct rspfadj_hdr {
	u_char	sig_bits;
	u_char	addr[4];
};
#define RSPFADJ_SIG_BITS 0x3f
#define RSPFADJ_LASTFLAG 0x80
#define RSPFADJ_LEN	5

struct rspf_frag {
	int	timer_id;
	u_long	addr;
	int env_no;
	int	frag;
	struct rspfnode_hdr nodehdr;
	struct rspflink_hdr linkhdr;
	int nodes;
	int links;
	int adjs;
	int old_info;
	int new_info;
	int my_info;
	int incomplete;
};

struct outfrag
{
	int frag;
	int sync;
	u_char *data;
	int datalen;
};

struct nodegroup
{
	u_long addr;
	u_int sigbits;
	char iface[IFNAMSIZ];
	u_int cost;
};
/* Some statistics we'd like to keep on rspf */
struct  rspf_mib {
	unsigned long rspfInMsgs;		/* # incoming messages */
	unsigned long rspfInRrhs;		/* # incoming RRH's */
	unsigned long rspfInRouteEnvs;		/* # incoming routing envelopes */
	unsigned long rspfInHdrErrors;		/* # incoming messages with bad header */
	unsigned long rspfInUnknownTypes;	/* # incoming messages with unknown type */
	unsigned long rspfInNotIfaces;		/* # incoming rspf packs not for configured interface */

	unsigned long rspfOutMsgs;		/* # outgoing messages */
	unsigned long rspfOutRrhs;		/* # outgoing RRH's */
	unsigned long rspfOutRouteEnvs;		/* # outgoing routing envelopes */
	
	unsigned long rspfReasmTimeout;		/* re-assembly timeout in seconds */
	unsigned long rspfReasmReqds;		/* # re-assembly has been required */
	unsigned long rspfReasmOKs;		/* # of successful reassemblies */
	unsigned long rspfReasmFails;		/* # of failed reassemblies */
	
	unsigned long rspfRrhTimer;		/* secs between outgoing RRH messages */
	unsigned long rspfSusPings;		/* # of pings before adjacaency lost */
	unsigned long rspfSusTimeout;		/* secs before adj considered sus */
	unsigned long rspfPingTimer;		/* secs between pings for sus timer */
	unsigned long rspfBullTimer;		/* secs between routing bulletins */
	unsigned long rspfBullTimeout;		/* secs before we ignore prev seq # */
	unsigned long rspfCurrAdjacencies;	/* # of adjacencies we know about */
	
	unsigned long rspfCurrIfaces;		/* # of interfaces */

	unsigned int EnvelopeNumber;		/* Routing Envelope ID */
	unsigned int SequenceNumber;		/* Routing bull sequence # */
	unsigned int SubSequenceNumber;		/* Routing bull sub-seq # */
	unsigned char rspfLinkHorizon;		/* Horizon of routers */
	unsigned char rspfGroupHorizon;		/* Horizon of node groups */
	unsigned char rspfLocalHorizon;		/* Horizon of non-routers in node-groups */
	unsigned char rspfPortableHorizon;	/* Horizon of non-routers not in node-groups */
};	

#endif /* rspfd.h */

