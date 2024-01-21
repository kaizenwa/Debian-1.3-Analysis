/*  stat.c is part of Statnet */
/* Statnet is protected under the GNU Public License (GPL2). */
/* Author: Jeroen Baekelandt (jeroenb@igwe.vub.ac.be)       */
/* 27DEC95: Scot E. Wilcoxon (sewilco@fieldday.mn.org)     */

/* This is the main program */
#define MAIN_LINE 1

#include <ncurses.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <linux/if_ether.h>
#include <strings.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <signal.h>
#include <linux/ip.h>
#include <linux/tcp.h>
#include <linux/udp.h>
#include <netinet/protocols.h>
#include <errno.h>
#include "curs.h"
#include "stat.h"

#ifdef _LINUX_IF_ETHER_H
#undef NET_3	/* not using Net/3 definitions */
#endif

#ifndef ETH_HLEN
#define ETH_HLEN 14	/* Ethernet header length */
#endif

#ifndef ETH_P_ATALK
#define ETH_P_ATALK 0x809B	/* Appletalk */
#endif

#ifndef SOCK_PACKET
#define SOCK_PACKET SOCK_RAW	/* If SOCK_PACKET is wrong, try SOCK_RAW */
#endif

#ifndef NET_3
#define ether_head	ethhdr		/* ether_head structure */
#define ether_type	h_proto		/* Ethernet Packet Type ID field */
#define ip		iphdr		/* IP header structure */
#define ip_off		frag_off	/* IP protocol field */
#define ip_p		protocol	/* IP protocol field */
#define ip_hl		ihl		/* IP header length field */
#define th_sport	source		/* TCP source port field */
#define th_dport	dest		/* TCP destination port field */
#define ui_sport	source		/* UDP source port field */
#define ui_dport	dest		/* UDP destination port field */
#endif

#define SN_RCV_BUF_SIZE	(sizeof(struct ether_head)+ \
				 sizeof(struct ip)+ \
                                 sizeof(struct options)+ \
				 sizeof(struct tcphdr))
					/* above was 1600, but kernel discards*/
					/* excess and tells us full length,   */
					/* so we only need enough to analyze. */

struct at_frame_type {
  unsigned char	filler[20];
  __u16		appletalk_type1;  /* what is formal name? */
  unsigned char       filler2[12];
  __u8		appletalk_type2;  /* what is formal name? */
};	/* Ethernet Appletalk */

void handle_frame (unsigned char *buf, int length, struct sockaddr *saddr);
void handle_ip (struct ip *buf, int length);

main (int argc, char *argv[])
{
  int sd;
  struct ifreq ifr, oldifr;
  char *device = ETH;
  char in_char;
  struct sockaddr saddr;
  int sizeaddr;
  unsigned char buf[SN_RCV_BUF_SIZE];
  int length;

  {  /* Compound statement to make initializers vanish after init. */
    int op;

    rewrite_labels = 1;
    memset( &regis, 0, sizeof(struct registers) );

    last_stats = &stat_buf1;
    now_stats = &stat_buf2;

    for( temp_int = 0; temp_int < SN_NUM_PROTOCOLS; temp_int++ ) {
      regis.prot_types[temp_int] = -1;
    }
    for( temp_int = 0; temp_int < SN_NUM_IP_TYPES; temp_int++ ) {
      regis.IP_types[temp_int] = -1;
    }
    for( temp_int = 0; temp_int < SN_NUM_TCP_PORTS; temp_int++ ) {
      regis.tcp_ports[temp_int] = -1;
    }
    for( temp_int = 0; temp_int < SN_NUM_UDP_PORTS; temp_int++ ) {
      regis.udp_ports[temp_int] = -1;
    }
    for( temp_int = 0; temp_int < SN_NUM_SAP_TYPES; temp_int++ ) {
      regis.SAP_types[temp_int] = -1;
    }

    /* Initialize here the labels for the services listed above. */
    services(); /* default labels for these services */

    /* TCP names */
    strcpy(tcp_port_types[20],  "FTP:");
    strcpy(tcp_port_types[23],  "Telnet:");
    strcpy(tcp_port_types[25],  "SMTP:");
    strcpy(tcp_port_types[42],  "DNS:");
    strcpy(tcp_port_types[79],  "Finger:");
    strcpy(tcp_port_types[80],  "WWW:");
    strcpy(tcp_port_types[101], "NIC Host NS:");
    strcpy(tcp_port_types[103], "X400:");
    strcpy(tcp_port_types[109], "POP2:");
    strcpy(tcp_port_types[111], "RPC/NFS:");
    strcpy(tcp_port_types[119], "NNTP:");
    strcpy(tcp_port_types[137], "NetB NS:");	/* NetBIOS Name Service */
    strcpy(tcp_port_types[138], "NetB Dg:");	/* NetBIOS Datagram */
    strcpy(tcp_port_types[139], "NetBIOS:");	/* NetBIOS Session Service */
    strcpy(tcp_port_types[194], "IRC:");	/* Internet Relay Chat */
    strcpy(tcp_port_types[515], "Printer:");	/* lpd print protocol */
    strcpy(tcp_port_types[520], "RIP:");

    /* now UDP */
    strcpy(udp_port_types[20],  "FTP:");
    strcpy(udp_port_types[23],  "Telnet:");
    strcpy(udp_port_types[25],  "SMTP:");
    strcpy(udp_port_types[42],  "DNS:");
    strcpy(udp_port_types[79],  "Finger:");
    strcpy(udp_port_types[80],  "WWW:");
    strcpy(udp_port_types[101], "NIC Host NS:");
    strcpy(udp_port_types[103], "X400:");
    strcpy(udp_port_types[109], "POP2:");
    strcpy(udp_port_types[111], "RPC/NFS:");
    strcpy(udp_port_types[119], "NNTP:");
    strcpy(udp_port_types[137], "NetB NS:");	/* NetBIOS Name Service */
    strcpy(udp_port_types[138], "NetB Dg:");	/* NetBIOS Datagram */
    strcpy(udp_port_types[139], "NetBIOS:");	/* NetBIOS Session Service */
    strcpy(udp_port_types[194], "IRC:");	/* Internet Relay Chat */
    strcpy(udp_port_types[515], "Printer:");	/* lpd print protocol */
    strcpy(udp_port_types[520], "RIP:");

    help_flag = 0;	/* No help unless asked for */
    redraw_screen = 0;	/* No redraw unless asked for */

    if (argc == 1)
      {
        regis.g = 1;		/* General */
        regis.ip_option = 1;	/* IP protocols */
        regis.at_option = 1;	/* Appletalk activity */
        regis.prot_option = 1;	/* Protocol activity */
        regis.tcp_option = 1;	/* TCP/IP activity */
        regis.udp_option = 1;	/* UDP/IP activity */
        regis.sap_option = 1;	/* SAP activity */
      }
    else
      while ((op = getopt (argc, argv, "aeghipt")) != EOF)
      {
        switch (op)
  	{
	  case 'a':
  	    regis.at_option = 1;
	    break;
	  case 'g':
	    regis.g = 1;
	    break;
	  case 'i':
	    regis.ip_option = 1;
	    break;
	  case 'p':
            regis.prot_option = 1;
	    break;
	  case 's':
	    regis.sap_option = 1;
	    break;
	  case 't':
	    regis.tcp_option = 1;
	    break;
	  case 'u':
	    regis.udp_option = 1;
	    break;
	  case 'h':
	  default:
	    usage (argv[0]);
	    break;
	}
      }

  /* INIT ALARMFUCTION: Alarm triggers update of the display */

  if (signal (SIGALRM, itstime) == SIG_ERR)
    {
      close_all_subwin();
      perror ("Signal error: ");
      exit (5);
    }

  /* OPEN SOCKET */

  if ((sd = socket (AF_INET, SOCK_PACKET, htons (ETH_P_ALL))) < 0)
    {
      close_all_subwin();
      perror ("Can't get socket: ");
      exit (1);
    }

  /* SET PROMISC */

  strcpy (oldifr.ifr_name, device);
  if (ioctl (sd, SIOCGIFFLAGS, &oldifr) < 0)
    {
      close (sd);
      perror ("Can't get flags: ");
      exit (2);
    }

  /* Should this be rewritten to cooperate with other net tools? */
  ifr = oldifr;
  ifr.ifr_flags |= IFF_PROMISC;

  if (ioctl (sd, SIOCSIFFLAGS, &ifr) < 0)
    {
      close (sd);
      close_all_subwin();
      perror ("Can't set flags: ");
      exit (3);
    }

    regis.errcode = 0;
    regis.errcount = 0;

    stats_countdown = (SN_STATS_SECS/SN_UPDATE_SECS)+1;
    if_getstats( ETH, last_stats );	/* get present interface statistics */

  } /* Compound statement to make initializer variables vanish after init. */
  /* END OF INITIALISATION */

  init_curses ();	/* initialize the screen */
  clrscr ();		/* clear the screen */
  set_null ();		/* clear all variables */
  alarm (1);		/* first screen update in about a second */

  in_char = '\0';
  while (in_char != 'q' && in_char != 'Q')
  {  /* while a 'q' was not typed */
    while ( (in_char=getch ()) == ERR )
    {
        /* This is the main data-gathering loop; keep it small and fast */
        sizeaddr = SN_RCV_BUF_SIZE;
        length = recvfrom (sd, buf, SN_RCV_BUF_SIZE, 0, &saddr, &sizeaddr);

        /* if recvfrom() is interrupted by screen update, an EINTR happens. */
        if (length < 0 )
          if ( errno != EINTR )
          {  /* if error detected and error is not expected type */
	     regis.errcode = errno;
	     regis.errcount++;
	     continue;
	   }
           else
             continue;
        else
          handle_frame (buf, length, &saddr);
    }

    /* A key has been pressed; we fall out of main loop to process it. */
    /* Wanted: A HELP screen should be added somehow. */
    /* Wanted: Should show "q to quit" reminder if unknown keys are pressed. */
    switch( in_char )
    {
    case 'a':
    case 'A':
      regis.at_option = !regis.at_option;
      break;
    case 'g':
    case 'G':
      regis.g = !regis.g;
      break;
    case 'i':
    case 'I':
      regis.ip_option = !regis.ip_option;
      break;
    case 'p':
    case 'P':
      regis.prot_option = !regis.prot_option;
      break;
    case 's':
    case 'S':
      regis.sap_option = !regis.sap_option;
      break;
    case 't':
    case 'T':
      regis.tcp_option = !regis.tcp_option;
      break;
    case 'u':
    case 'U':
      regis.udp_option = !regis.udp_option;
      break;
    case '\f':
      clear();	/* Forcefully clear the screen */
      redraw_screen = 1;
      break;
    case 'q':
    case 'Q':
      break;
    default:
    case 'h':
      help_flag = !help_flag;
      break;
    }
    rewrite_labels = 1;  /* Screen may have changed, rewrite the labels */
  }  /* while a 'q' was not typed */


  /* TERMINATE */

  close_all_subwin();
  cleanup_curses ();

  /* Should this be rewritten to cooperate with other net tools? */
  if (ioctl (sd, SIOCSIFFLAGS, &oldifr) < 0)
    {
      close (sd);
      perror ("Can't set flags: ");
      exit (4);
    }

  close (sd);
  exit (0);
}


void
exatalk (struct at_frame_type *buf, int length)
{
  /* Need some documentation of these definitions and the AT packet types */
  switch (buf->appletalk_type2)  /* Appletalk */
    {
    case 0x01:
      regis.rtmprd++;
      break;
    case 0x02:
      regis.nbp++;
      break;
    case 0x03:
      regis.atp++;
      break;
    case 0x04:
      regis.aep++;
      break;
    case 0x05:
      regis.rtmpreq++;
      break;
    case 0x06:
      regis.zip++;
      break;
    case 0x07:
      regis.adsp++;
      break;
    default:
      break;
    }
}

void
handle_other (unsigned char *buf, int length)
{

  /* frame_protocol is global */
  if( frame_protocol < 1501 )
  {  /* if IEEE 802.3 packet instead of Ethernet packet (per RFC 1700) */
    if( (short int)buf[14] <= SN_MAX_SAP )
    {
      sap_count[(short int)buf[14]]++;	/* count 802.2 SAP number */
    }
    regis.new_ethernet_count++;
  }  /* if IEEE 802.3 packet instead of Ethernet packet */
  else
  {  /* else is an Ethernet packet */
  switch (((struct at_frame_type *)buf)->appletalk_type1)	/* Appletalk */
    {
    case __constant_ntohs(ETH_P_ATALK):
      if( regis.at_option ) exatalk ((struct at_frame_type *)buf, length);
      break;
    case __constant_ntohs(0x80F3):
      regis.aarp++;
      break;
    default:
      break;
    }
  }  /* else is an Ethernet packet */
}

void
handle_ip (struct ip *buf, int length)
{
  static int x;

  if( buf->ip_p <= SN_MAX_IP_PORT )
  {  /* if IP protocol type is to be tallied */
    ip_protocol_count[buf->ip_p]++;
  }  /* if IP protocol type is to be tallied */

  switch (buf->ip_p) 	/* IP Protocol */
    {

    case IPPROTO_TCP:	/* TCP */
     {
      if( buf->ip_off != 0 )
      {  /* if this is not the first frame of a fragmented packet */
        tcp_port_count[ 0 ]++;		/* count fragment */
      }  /* if this is not the first frame of a fragmented packet */
      else
      {  /* else this is the first frame of a packet */
        /* The below may count packets twice, but probably both are not displayed */
        if( buf->ip_hl == 5 )
        {  /* if IP header is normal length we can assume 20 octects length */
          if( (x=ntohs(((struct tcphdr *)((void *)buf+20))->th_sport)) <= SN_MAX_TCP_PORTS )
            tcp_port_count[ x ]++;				/* count source port */
          if( (x=ntohs(((struct tcphdr *)((void *)buf+20))->th_dport)) <= SN_MAX_TCP_PORTS )
            tcp_port_count[ x ]++;				/* count dest port */
        }  /* IP header normal length */
        else
        {  /* IP header with options */
          /* Incidentally, the (void *) is to cause byte-level math not sizeof(struct ip) */
          if( (x=ntohs(((struct tcphdr *)((void *)buf+((buf->ip_hl)*4)))->th_sport)) <= SN_MAX_TCP_PORTS )
            tcp_port_count[ x ]++;				/* count source port */
          if( (x=ntohs(((struct tcphdr *)((void *)buf+((buf->ip_hl)*4)))->th_dport)) <= SN_MAX_TCP_PORTS )
            tcp_port_count[ x ]++;				/* count dest port */
        }  /* IP header with options */
      }  /* else this is the first frame of a packet */
     }
     break;

    case IPPROTO_UDP:	/* UDP */
     {
      if( buf->ip_off != 0 )
      {  /* if this is not the first frame of a fragmented packet */
        udp_port_count[ 0 ]++;		/* count fragment */
      }  /* if this is not the first frame of a fragmented packet */
      else
      {  /* else this is the first frame of a packet */
        /* The below may count packets twice, but probably both are not displayed */
        if( buf->ip_hl == 5 )
        {  /* if IP header is normal length we can assume 20 octects length */
          if( (x=ntohs(((struct udphdr *)((void *)buf+20))->ui_sport)) <= SN_MAX_UDP_PORTS )
            udp_port_count[ x ]++;				/* count source port */
          if( (x=ntohs(((struct udphdr *)((void *)buf+20))->ui_dport)) <= SN_MAX_UDP_PORTS )
            udp_port_count[ x ]++;				/* count dest port */
        }  /* IP header normal length */
        else
        {  /* IP header with options */
          /* Incidentally, the (void *) is to cause byte-level math not sizeof(struct ip) */
          if( (x=ntohs(((struct tcphdr *)((void *)buf+((buf->ip_hl)*4)))->ui_sport)) <= SN_MAX_UDP_PORTS )
            udp_port_count[ x ]++;				/* count source port */
          if( (x=ntohs(((struct tcphdr *)((void *)buf+((buf->ip_hl)*4)))->ui_dport)) <= SN_MAX_UDP_PORTS )
            udp_port_count[ x ]++;				/* count dest port */
        }  /* IP header with options */
      }  /* else this is the first frame of a packet */
     }
 
     break;

    default:
      /* Don't increment "Other" because are not displaying "Other IP" counts */
      /* This packet does show in the "IP" total, but not elsewhere. */
      break;
    }
}

void
handle_frame (unsigned char *buf, int length, struct sockaddr *saddr)
{
  int	prot_int;
  int   search_int;
  int	prot_now;
  struct ip	*ip_ptr;

  ip_ptr = (struct ip *)((void *)buf+ETH_HLEN);

  if (length > 0 )
  {

    packet_type = ((struct ether_head *)buf)->ether_type;  /* Ethernet packet type ID field */
    frame_protocol = ntohs(packet_type);	/* Convert from network to host seq */
    if( frame_protocol < 1501 )
    {  /* if an IEEE 802.3 packet */
      frame_protocol = SN_PROT_IEEE802_3;
    }  /* if an IEEE 802.3 packet */
  
    /* The grand totals by interface type */
    if (strncmp (saddr->sa_data, "eth", 3) == 0)
      {
        regis.etherbytes += length;
        regis.ethercount++;
      }
    else if (strncmp (saddr->sa_data, "plip", 4) == 0)
      {
        regis.plipbytes += length;
        regis.plipcount++;
      }
    else if (strncmp (saddr->sa_data, "lo", 2) == 0)
      {
        regis.loopbytes += length;
        regis.loopcount++;
        frame_protocol = SN_PROT_LOOP;
      }
    else if (strncmp (saddr->sa_data, "ppp", 3) == 0)
      {
        regis.pppbytes += length;
        regis.pppcount++;
        frame_protocol = SN_PROT_PPP;
        ip_ptr = (struct ip *)((void *)buf);	/* No Ethernet Header */
      }
    else if (strncmp (saddr->sa_data, "sl", 2) == 0)
      {
        regis.slipbytes += length;
        regis.slipcount++;
        frame_protocol = SN_PROT_SLIP;
        ip_ptr = (struct ip *)((void *)buf);	/* No Ethernet Header */
      }
    else
      {
        regis.otherbytes += length;
        regis.othercount++;
      }
 
    if( frame_protocol == SN_PROT_IEEE802_3 )
    {  /* if an IEEE 802.3 packet */
      for( prot_int = 0; prot_int < SN_NUM_PROTOCOLS; prot_int++ )
      {  /* for all protocols */
        if( (prot_now = regis.prot_types[prot_int]) >= 0 )
        {  /* if a protocol to display is defined */
          if( protocol_num[prot_now] == SN_PROT_IEEE802_3 )
          {  /* if this is zero, the protocol used for IEEE 802.3 */
            protocol_count[prot_int]++;	/* tally this frame */
            break;
          }  /* if this is zero, the protocol used for IEEE 802.3 */
        }  /* if a protocol to display is defined */
      }  /* for all protocols */
      if( prot_int >= SN_NUM_PROTOCOLS )
      {  /* if this protocol type was not found */
        regis.prot_types[SN_NUM_PROTOCOLS-1] = SN_PROT_IEEE802_3;	/* mark this as IEEE 802.3 protocol */
        protocol_count[SN_NUM_PROTOCOLS-1] = 1;	/* tally this frame */
      }  /* if this protocol type was not found */
    }  /* if an IEEE 802.3 packet */
    else
    {  /* if not an IEEE 802.3 packet */

      for( prot_int = 0; prot_int < SN_NUM_PROTOCOLS; prot_int++ )
      {  /* for all protocols */
        if( (prot_now = regis.prot_types[prot_int]) >= 0 )
        {  /* if a protocol to display is defined */
          if( protocol_num[prot_now] == frame_protocol )
          {  /* if this is the protocol number which was encountered */
            protocol_count[prot_int]++;	/* tally this frame */
            break;
          }  /* if this is the protocol number which was encountered */
        }  /* if a protocol to display is defined */
      }  /* for all protocols */
  
      if( prot_int >= SN_NUM_PROTOCOLS )
      {  /* if protocol was not found */
        for( search_int = 0; search_int < SN_MAX_PROTO_DESC; search_int++ )
        {  /* for all known protocols */
          if( frame_protocol == protocol_num[search_int] )
          {  /* if packet type was found */
            if( regis.prot_types[SN_NUM_PROTOCOLS-1] >= 0  && 
			protocol_count[SN_NUM_PROTOCOLS-1] > 0 )
            {  /* if last displayed protocol was in use, tally as unknown */
              regis.unknown_type += protocol_count[SN_NUM_PROTOCOLS-1]; /* add this count */
              regis.unknown_frame_type = protocol_num[regis.prot_types[SN_NUM_PROTOCOLS-1]]; /* remember protocol */
            }  /* if last displayed protocol was in use, tally as unknown */
            regis.prot_types[SN_NUM_PROTOCOLS-1] = search_int;
            protocol_count[SN_NUM_PROTOCOLS-1] = 1;	/* tally this frame */
            break;
          }  /* if packet type was found */
        }  /* for all known protocols */

        if( search_int >= SN_MAX_PROTO_DESC )
        {  /* if protocol was not found */
          regis.unknown_type++;
          regis.unknown_frame_type = frame_protocol;
        }  /* if protocol was not found */
      }  /* if protocol was not found */
    }  /* if not an IEEE 802.3 packet */
 
    switch( frame_protocol )
      {
      case ETH_P_IP:
      case SN_PROT_PPP:
      case SN_PROT_SLIP:
      case SN_PROT_LOOP:
        if( regis.tcp_option || regis.udp_option || regis.ip_option) handle_ip (ip_ptr, length);
        break;
      case ETH_P_ATALK:
        if( regis.at_option ) exatalk ((struct at_frame_type *)((void *)buf+ETH_HLEN), length);
        break;
      default:
        if( regis.ip_option || regis.at_option ) handle_other (buf, length);
        break;
      }

    }  /* if length > 0 */
}

void
usage (char *arg)
{
  fprintf (stderr, "\n%s [-aegipth]\n\n", arg);
  fprintf (stderr, "   Display network statistics\n");
  fprintf (stderr, "   ver 2.1 \n\n");
  fprintf (stderr, "    -a  show Appletalk window\n");
  fprintf (stderr, "    -g  show General window\n");
  fprintf (stderr, "    -i  show IP Protocols window\n");
  fprintf (stderr, "    -p  show Protocols window\n");
  fprintf (stderr, "    -s  show IEEE 802.2 SAP window\n");
  fprintf (stderr, "    -t  show TCP/IP window\n");
  fprintf (stderr, "    -u  show UDP/IP window\n");
  fprintf (stderr, "    -h  show this message\n");
  fprintf (stderr, "    no options: show most windows\n");
  fprintf (stderr, "    Options may be typed while running.\n\n");
  exit (0);
}
