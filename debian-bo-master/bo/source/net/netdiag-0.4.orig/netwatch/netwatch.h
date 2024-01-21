/*  stat.h is part of Statnet */
/* Statnet is protected under the GNU Public License (GPL2). */
/* Author: Jeroen Baekelandt (jeroenb@igwe.vub.ac.be)       */

#define ETH "eth0"
#define SN_UPDATE_SECS		6	/* Number of seconds between updates. */
					/* Suggest 4 to 6 seconds minimum, as */
					/* two seconds is usually too fast to */
					/* see relationships.  CPU time use is*/
					/* mostly due to packet processing if */
					/* update is greater than 3 seconds.  */

#define SN_STATS_SECS		(6*60)	/* Number of seconds between stats updates */

#define SN_NUM_PROTOCOLS	8	/* Number of Ethernet protocols to show */
#define SN_MAX_PROTO_DESC       200	/* Number of Ethernet protocols to know */
#define SN_NUM_IP_TYPES		7	/* Number of IP protocols to show */
#define SN_NUM_TCP_PORTS	9	/* Number of TCP ports to show */
#define SN_NUM_UDP_PORTS	9	/* Number of UDP ports to show */
#define SN_NUM_SAP_TYPES	7	/* Number of IP protocols to show */
#define SN_MAX_IP_PORT		256	/* Number of IP protocols to tally */
#define SN_MAX_TCP_PORTS 	1024	/* Number of TCP ports to tally */
#define SN_MAX_UDP_PORTS 	1024	/* Number of UDP ports to tally */
#define SN_MAX_SAP		256	/* Number of 802.2 SAP to tally */
#define SN_PORT_TYPE_LEN 	20	/* Length of type labels */
#define SN_LIST_SWAP		5	/* Number of packets to require movement higher in list */

#define SN_PROT_IEEE802_3	0	/* Pseudo protocol for IEEE 802.3 */
#define SN_PROT_SLIP		162	/* Pseudo protocol for SLIP */
#define SN_PROT_PPP		163	/* Pseudo protocol for PPP */
#define SN_PROT_LOOP		164	/* Pseudo protocol for Loopback */

#include <asm/byteorder.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <linux/ip.h>
#include <linux/tcp.h>
#include <linux/if_ether.h>
#include <netinet/protocols.h>

void dispdata(int errnum);
void services();
void usage(char *arg);
void clrportion(int y1, int x1, int y2, int x2);

struct registers{
	int ip_option :1;	/* Types window */
	int g :1;	/* General window */
        int prot_option :1;	/* Protocol activity */
	int at_option :1;	/* Appletalk activity */
	int tcp_option :1;	/* TCP/IP activity */
	int udp_option :1;	/* UDP activity */
	int sap_option :1;	/* SAP activity */

	/* Ethernet interface */
        int  ethercount;
        long etherbytes;

	/* PLIP interface */
	int  plipcount;
	long plipbytes;

	/* SLIP interface */
	int  slipcount;
	long slipbytes;

	/* PPP interface */
	int  pppcount;
	long pppbytes;

	/* loopback interface */
	int  loopcount;
	long loopbytes;

	int  othercount;
	long otherbytes;

	/* Appletalk types */
	int aarp;
	int rtmprd;
	int nbp;
	int atp;
	int aep;
	int rtmpreq;
	int zip;
	int adsp;

        /* IEEE802.2 protocol */
        int new_ethernet_count;

        /* unknown types */
	int unknown_type;
	int unknown_frame_type;		/* store last unknown frame code */
	int unknown_sap;		/* store last unknown sap codes */

        /* Received error count and last error code */
	int errcount;
	int errcode;			/* store last error code */

        /* Ethernet protocol types to display */
	int prot_types[SN_NUM_PROTOCOLS];

        /* IP protocol types to display */
	int IP_types[SN_NUM_IP_TYPES];

        /* TCP port numbers to display */
	int tcp_ports[SN_NUM_TCP_PORTS];

        /* UDP port numbers to display */
	int udp_ports[SN_NUM_UDP_PORTS];

        /* SAP protocol types to display */
	int SAP_types[SN_NUM_SAP_TYPES];

        };

#ifdef MAIN_LINE
#define EXTERN_DEF
#else
#define EXTERN_DEF	extern
#endif

EXTERN_DEF struct registers	regis;
EXTERN_DEF int			packet_type;
EXTERN_DEF int			frame_protocol;;
EXTERN_DEF int			rewrite_labels;
EXTERN_DEF int			redraw_screen;
EXTERN_DEF int			help_flag;
EXTERN_DEF int			temp_int;
EXTERN_DEF int			stats_countdown;
EXTERN_DEF struct enet_statistics	*last_stats;
EXTERN_DEF struct enet_statistics	*now_stats;
EXTERN_DEF struct enet_statistics	*temp_stats;
EXTERN_DEF struct enet_statistics	stat_buf1;
EXTERN_DEF struct enet_statistics	stat_buf2;

/* Note the +1 added to ints to allow direct reference to constant values   */
/* instead of having to subtract 1 due to array address starting from zero. */
/* Note the +1 added to char strings for the terminating NULL. */
EXTERN_DEF int	protocol_count[SN_NUM_PROTOCOLS+1];	/* Count of frames */
EXTERN_DEF int	protocol_num[SN_MAX_PROTO_DESC+1];	/* Protocol numbers */
EXTERN_DEF int	ip_protocol_count[SN_MAX_IP_PORT+1];
EXTERN_DEF int	tcp_port_count[SN_MAX_TCP_PORTS+1];
EXTERN_DEF int	udp_port_count[SN_MAX_UDP_PORTS+1];
EXTERN_DEF int	sap_count[SN_MAX_SAP+1];
EXTERN_DEF char	ip_protocol_types[SN_MAX_IP_PORT+1][SN_PORT_TYPE_LEN+1];
EXTERN_DEF char	tcp_port_types[SN_MAX_TCP_PORTS+1][SN_PORT_TYPE_LEN+1];
EXTERN_DEF char	udp_port_types[SN_MAX_UDP_PORTS+1][SN_PORT_TYPE_LEN+1];
EXTERN_DEF char	sap_port_types[SN_MAX_SAP+1][SN_PORT_TYPE_LEN+1];
EXTERN_DEF char	protocol_types[SN_MAX_PROTO_DESC+1][SN_PORT_TYPE_LEN+1];

struct hostinfo {
   unsigned char addr[4];
   unsigned char othaddr[4];
   char name[40];
   char ip_pr[10];
   char servicename[40];
   unsigned long pktcntsend;
   unsigned long pktcntrec;
   time_t	tstamp;
   unsigned long sendbytes;
   unsigned long recbytes;
   int disprow;
   char update;
   struct hostinfo *flink;
   struct hostinfo *blink;
   };
typedef struct hostinfo HOSTINFO;

#define DISP_TX_RC 	0
#define DISP_IPPROTO	1
#define DISP_DEST1	2
#define DISP_DEST2	3
#define DISP_ACCT	4
#define DISP_WATCH	5
#define DISP_MAX	DISP_ACCT
#define CURSESBUG	7 