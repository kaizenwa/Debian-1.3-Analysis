/*  NETWATCH is based on some code from Statnet */
/* Statnet is protected under the GNU Public License (GPL2). */
/* Author: Jeroen Baekelandt (jeroenb@igwe.vub.ac.be)       */
/* 27DEC95: Scot E. Wilcoxon (sewilco@fieldday.mn.org)     */
/* 
   NETWATCH is VERY loosely based on the code from Statnet...
   thanks out to Jeroen and Scot...

   NETWATCH allows a user (superuser) to monitor an ETHERNET
   and examine activity on the network. Hostnames are highlighted
   in colours (for those supporting them) to indicate activity
   on the bus network based on time ( less than 1 minute RED,
   less than 5 minutes YELLOW, less than 30 minutes GREEN and
   otherwise BLUE). The monitor includes statistics on 
   a) Transmitted and received packets
   b) Protocol of LAST packet (TX or RC)
   c) LAST Communication partner (IP address)

   The number of hosts capable of support is a function of
   memory. They are stored in 2 doubly-linked lists (local
   and remote).

   Screen updates take place 1 per second (unless a rare 
   lockout... when linked list links are updating... in which
   case it displays in the next second)

   Keyboard usage is admittedly limited (and a kludge... due to
   some ncurses settings that need better tinkering)
   ->   Go forward to next option
   <-   Go backward to previous option
   <UP>         Go back to previous page (back 20 lines on most consoles)
   <DOWN>       Go forward to next page (forward 20 lines on most consoles)
   c    Clear counters for fresh counting
   n    Clear linked lists for new start

   It is a simple program to execute for ETHERNET under LINUX.
   It assumes that there is a "/etc/rc.d/rc.inet1" file for
   network configuration. If so, it checks for an "eth0" ifconfig
   and picks up the netmask from the file.

   For those with multiple "eth" interfaces, I am sorry it doesn't
   support both simultaneously.

   AUTHOR:      G. MacKay
   E-MAIL:      mackay@gmml.slctech.org

   P.S.
   Given the fact that I was sick for 3 days and decided to
   write this code... forgive me for not writing beautiful
   code... (those that know me, probably don't think my
   code is beautiful when I am healthy)
 */

/* This is the main program */
#define MAIN_LINE 1

#include <ncurses.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <linux/if_ether.h>
#include <strings.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <signal.h>
#include <linux/ip.h>
#include <linux/tcp.h>
#include <linux/udp.h>
#include <linux/icmp.h>
#include <netinet/protocols.h>
#include <netdb.h>
#include <errno.h>
#include "curs.h"
#include "netwatch.h"

static char *version = "0.5";

#ifdef _LINUX_IF_ETHER_H
#undef NET_3			/* not using Net/3 definitions */
#endif

#ifndef ETH_HLEN
#define ETH_HLEN 14		/* Ethernet header length */
#endif

#ifndef ETH_P_ATALK
#define ETH_P_ATALK 0x809B	/* Appletalk */
#endif

#ifndef SOCK_PACKET
#define SOCK_PACKET SOCK_RAW	/* If SOCK_PACKET is wrong, try SOCK_RAW */
#endif

#ifndef NET_3
#define ether_head	ethhdr	/* ether_head structure */
#define ether_type	h_proto	/* Ethernet Packet Type ID field */
#define ip		iphdr	/* IP header structure */
#define ip_off		frag_off	/* IP protocol field */
#define ip_p		protocol	/* IP protocol field */
#define ip_hl		ihl	/* IP header length field */
#define th_sport	source	/* TCP source port field */
#define th_dport	dest	/* TCP destination port field */
#define ui_sport	source	/* UDP source port field */
#define ui_dport	dest	/* UDP destination port field */
#endif

#define SN_RCV_BUF_SIZE	(sizeof(struct ether_head)+ \
				 sizeof(struct ip)+ \
                                 sizeof(struct options)+ \
				 sizeof(struct tcphdr))
					/* above was 1600, but kernel discards */
					/* excess and tells us full length,   */
					/* so we only need enough to analyze. */
int ESCON = 0;
#define ESC 0x1b
#define RETURNKEY 0x0d
#define MAXHELPPAGE 2
HOSTINFO ldummy, rdummy;
HOSTINFO *lfirst, *rfirst, *previous, *current, *next, *work;
struct itimerval newtm,oldtm;  /* TIMER for measuring */
time_t new;
char configfile[128]="/etc/rc.d/rc.inet1";
int newconfig = FALSE;
char ethdevname[64]="eth0";
int newethname = FALSE;
int probcnt = 0;		/*  Problem Count  on ETH reads */
int intrcnt = 0;		/*  EINTR hits  on ETH reads */

double maxburst = 0.0;
unsigned char netmask[4] =
{255, 255, 255, 0};
unsigned char localaddr[4];
int ethok = 1;
struct hostent *hostent;
int wai = 0;
int bugfix = FALSE;
int help = FALSE;
int helppage = 1;
int watch = FALSE;
int remoterow = -1;
int localrow = -1;
int localupdate = 0;
int remoteupdate = 0;
int poschange = 0;
int refreshloc = 0;
int refreshrem = 0;
int refreshgen = 0;
int llockout = 0;
int rlockout = 0;
int lydisp = 0;
int rydisp = 0;
int ydisp = 0;
int localkey = TRUE;
int dispopt = DISP_TX_RC;
int colour;
unsigned long int routeruse=0;
unsigned long int routerto=0;
unsigned long int routerfrom=0;
int disprouterstats=FALSE;
int scrmid;

struct at_frame_type
  {
    unsigned char filler[20];
    __u16 appletalk_type1;	/* what is formal name? */
    unsigned char filler2[12];
    __u8 appletalk_type2;	/* what is formal name? */
  };				/* Ethernet Appletalk */

void handle_frame (unsigned char *buf, int length, struct sockaddr *saddr);
void handle_ip (struct ip *buf, int length);
void processinetrc (unsigned char *netmask, unsigned char *local, int *peth);

void disphelp(int helppage, int xoff)
{
	switch(helppage)
	{
	case 1:
	mvprintw(6,xoff+10,"HELP WINDOW");
	mvprintw(8,xoff+3,"Normal Mode Commands");
	mvprintw(10,xoff+3,"<TAB>  - toggle REMOTE/LOCAL");
	mvprintw(11,xoff+3,"         for keyboard control");
	mvprintw(12,xoff+3,"<LEFT> - Go back ONE Option");
	mvprintw(13,xoff+3,"<RIGHT>  Go forward ONE OPTION");
	mvprintw(14,xoff+3,"<UP>   - Back ONE PAGE on");
	mvprintw(15,xoff+3,"         CURRENT (REMOTE/LOCAL)");
	mvprintw(16,xoff+3,"<DOWN> - Forward ONE PAGE on");
	mvprintw(17,xoff+3,"         CURRENT (REMOTE/LOCAL)");
	mvprintw(18,xoff+3,"<ESC>  Exit HELP MODE");
	mvprintw(19,xoff+3,"or <F10>");
	mvprintw(20,xoff+3,"or <END>");
	break;
	case 2:
	mvprintw(6,xoff+10,"HELP WINDOW");
	mvprintw(8,xoff+3,"Special Mode Commands");
	mvprintw(10,xoff+3,"c      - Clear Counters (Packets");
	mvprintw(11,xoff+3,"         and Bytes) ");
	mvprintw(12,xoff+3,"n      - NEW Host List (Restart)");
	mvprintw(13,xoff+3,"w      - Enter WATCH Mode");
	mvprintw(14,xoff+3,"         (Special Commands on");
	mvprintw(15,xoff+3,"          WATCH mode on WATCH");
	mvprintw(16,xoff+3,"          PAGE)");
	mvprintw(18,xoff+3,"<ESC>  Exit HELP MODE");
	mvprintw(19,xoff+3,"or <F10>");
	mvprintw(20,xoff+3,"or <END>");
	break;
	
	}
}

void setupauxscr(int *xoff)
{
	int xst=1;
	int xend;
	
	xend = scrmid;
	if (localkey)
	{
		xst = scrmid +1;
		xend = COLS-1;
	}
	clrportion(4,xst,LINES-1,xend);
	mvhline(4,xst+1,ACS_BLOCK,xend-xst-2);
	mvhline(LINES-3,xst+1,ACS_BLOCK,xend-xst-2);
	mvvline(5,xst+1,ACS_BLOCK,LINES-8);
	mvvline(5,xend-2,ACS_BLOCK,LINES-8);
	*xoff = xst;
}

void setuphelp()
{
	int xoff = 1; 
	setupauxscr(&xoff);
	disphelp(helppage,xoff);
}

void setupwatch()
{
	int xoff = 1; 

	setupauxscr(&xoff);
	mvprintw(6,xoff+10,"WATCH WINDOW");
	mvprintw(8,xoff+3,"Special Commands");
	mvprintw(10,xoff+3,"p   - examine packets");
	mvprintw(11,xoff+3,"s   - select host");
	mvprintw(12,xoff+3,"r   - examine router stats");
	mvprintw(13,xoff+3,"t   - trace host");
	mvprintw(14,xoff+3,"<ESC>  Exit WATCH MODE");
	mvprintw(15,xoff+3,"or <F10>");
	mvprintw(16,xoff+3,"or <END>");
}

main (int argc, char *argv[])
{
  int sd;
  int dum;
  struct ifreq ifr, oldifr;
  char *device = ETH;
  int in_char;
  struct sockaddr saddr;
  int sizeaddr;
  unsigned char buf[SN_RCV_BUF_SIZE];
  int length;

  {				/* Compound statement to make initializers vanish after init. */
    int op;


    rewrite_labels = 1;
    lfirst = &ldummy;		/* The dummy record header for linked list */
    lfirst->flink = lfirst;
    lfirst->blink = lfirst;
    lfirst->disprow = localrow;

    rfirst = &rdummy;		/* The dummy record header for linked list */
    rfirst->flink = rfirst;
    rfirst->blink = rfirst;
    rfirst->disprow = remoterow;
    /* Initialize here the labels for the services listed above. */
    services ();		/* default labels for these services */
    /* TCP names */
    strcpy (tcp_port_types[20], "FTP:");
    strcpy (tcp_port_types[23], "Telnet:");
    strcpy (tcp_port_types[25], "SMTP:");
    strcpy (tcp_port_types[42], "DNS:");
    strcpy (tcp_port_types[79], "Finger:");
    strcpy (tcp_port_types[80], "WWW:");
    strcpy (tcp_port_types[101], "NIC Host NS:");
    strcpy (tcp_port_types[103], "X400:");
    strcpy (tcp_port_types[109], "POP2:");
    strcpy (tcp_port_types[111], "RPC/NFS:");
    strcpy (tcp_port_types[119], "NNTP:");
    strcpy (tcp_port_types[137], "NetB NS:");	/* NetBIOS Name Service */
    strcpy (tcp_port_types[138], "NetB Dg:");	/* NetBIOS Datagram */
    strcpy (tcp_port_types[139], "NetBIOS:");	/* NetBIOS Session Service */
    strcpy (tcp_port_types[194], "IRC:");	/* Internet Relay Chat */
    strcpy (tcp_port_types[515], "Printer:");	/* lpd print protocol */
    strcpy (tcp_port_types[520], "RIP:");

    /* now UDP */
    strcpy (udp_port_types[20], "FTP:");
    strcpy (udp_port_types[23], "Telnet:");
    strcpy (udp_port_types[25], "SMTP:");
    strcpy (udp_port_types[42], "DNS:");
    strcpy (udp_port_types[79], "Finger:");
    strcpy (udp_port_types[80], "WWW:");
    strcpy (udp_port_types[101], "NIC Host NS:");
    strcpy (udp_port_types[103], "X400:");
    strcpy (udp_port_types[109], "POP2:");
    strcpy (udp_port_types[111], "RPC/NFS:");
    strcpy (udp_port_types[119], "NNTP:");
    strcpy (udp_port_types[137], "NetB NS:");	/* NetBIOS Name Service */
    strcpy (udp_port_types[138], "NetB Dg:");	/* NetBIOS Datagram */
    strcpy (udp_port_types[139], "NetBIOS:");	/* NetBIOS Session Service */
    strcpy (udp_port_types[194], "IRC:");	/* Internet Relay Chat */
    strcpy (udp_port_types[515], "Printer:");	/* lpd print protocol */
    strcpy (udp_port_types[520], "RIP:");

    help_flag = 0;		/* No help unless asked for */
    redraw_screen = 0;		/* No redraw unless asked for */
      while ((op = getopt (argc, argv, "hc:e:")) != EOF)
	{
	  switch (op)
	    {
	    case 'c':
		strncpy(configfile,optarg,256);
		newconfig = TRUE;		
	      break;
	    case 'e':
	        strncpy(ethdevname,optarg,64);
	        newethname = TRUE;
	      break;
	    case ':':
	    	fprintf(stderr,"Missing Parameter\n\n");
	    case 'h':
	    default:
	      usage (argv[0]);
	      break;
	    }
	}
    processinetrc (netmask, localaddr, &ethok);
    if (!ethok)
      {
	printf ("NO %s Interface to work on!!\n",ethdevname);
	exit (1);
      }

    /* INIT ALARMFUCTION: Alarm triggers update of the display */

    if (signal (SIGALRM, dispdata) == SIG_ERR)
      {
	perror ("Signal error: ");
	exit (5);
      }

    /* OPEN SOCKET */

    if ((sd = socket (AF_INET, SOCK_PACKET, htons (ETH_P_ALL))) < 0)
      {
	perror ("Can't get socket: ");
	exit (1);
      }

    /* SET PROMISC */

    strcpy (oldifr.ifr_name, ethdevname);
    if (ioctl (sd, SIOCGIFFLAGS, &oldifr) < 0)
      {
	perror ("Can't get flags: ");
	close (sd);
	exit (2);
      }

    /* Should this be rewritten to cooperate with other net tools? */
    ifr = oldifr;
    ifr.ifr_flags |= IFF_PROMISC;
    strcpy (ifr.ifr_name, ethdevname);
    if (ioctl (sd, SIOCSIFFLAGS, &ifr) < 0)
      {
	perror ("Can't set flags: ");
	close (sd);
	exit (3);
      }


  }				/* Compound statement to make initializer variables vanish after init. */
  /* END OF INITIALISATION */
  /*  init_curses (); *//* initialize the screen */
    initscr ();
    cbreak ();
    noecho ();
    nodelay (stdscr, TRUE);
    nonl ();
    keypad (stdscr, TRUE);
    intrflush (stdscr, FALSE);
    if (has_colors ())
      {
	colour = TRUE;
	start_color ();
	init_pair (4, COLOR_WHITE, COLOR_BLUE);
	init_pair (1, COLOR_WHITE, COLOR_RED);
	init_pair (2, COLOR_BLACK, COLOR_YELLOW);
	init_pair (3, COLOR_BLACK, COLOR_GREEN);
      }
    else
      colour = FALSE;
  clrscr ();			/* clear the screen */
 /*  alarm (1);	*/		/* first screen update in about a second */
 newtm.it_value.tv_sec = 1;
 newtm.it_value.tv_usec = 0;
setitimer(ITIMER_REAL,&newtm,&oldtm); /* first screen update in 1 sec (exact)*/
  ESCON = 0;
  in_char = '\0';
  /*
  	Curses Bug....
  	
  	Even though there is a NO DELAY on keyboard input, NCURSES
  	appears to WAIT for a key after an unspecified number of minutes
  	(working perfectly prior to that). To fix the problem, a variable
  	called "wai" (Where Am I) is set to specific values that indicates
  	where execution is. When "wai==CURSESBUG", the code is waiting
  	and not returning from "getch()". To get this to keep going,
  	a KLUDGE of feeding a character is done from the DISPLAY process
  	(at 1 exec. per sec). This makes the operation continue, BUT
  	has the unfortunate effect of ruining statistics in that time
  	frame (so burst statistics are ignored). The actual counts are
  	still correct, but rates are not.
  */
  while (in_char != 'q' && in_char != 'Q')
    {				/* while a 'q' was not typed */
      while ((in_char = getch ()) == ERR)
	{
	  wai = 1;
	  /* This is the main data-gathering loop; keep it small and fast */
	  sizeaddr = SN_RCV_BUF_SIZE;
	  length = recvfrom (sd, buf, SN_RCV_BUF_SIZE, 0, &saddr, &sizeaddr);
	  wai = 2;
	  /* if recvfrom() is interrupted by screen update, an EINTR happens. */
	  if (length < 0)
	  {
	    if (errno != EINTR)
	      {			/* if error detected and error is not expected type */
		probcnt++;
		continue;
	      }
	    else
	    {
	      intrcnt++;
	      continue;
	      }
	  }
	  else
	  {
	    handle_frame (buf, length, &saddr);
	  }
	  wai = CURSESBUG;
	}
	if (bugfix)
	{
	  mvprintw (0, 60, "%c", ACS_HLINE);
	  bugfix = FALSE;
	 }			
	wai = 3;
      if (localkey)
	ydisp = lydisp;
      else
	ydisp = rydisp;
      refreshgen = 0;
      /* A key has been pressed; we fall out of main loop to process it. */
      /* Wanted: A HELP screen should be added somehow. */
      /* Wanted: Should show "q to quit" reminder if unknown keys are pressed. */
      switch (in_char)
	{
	case RETURNKEY:
		if (help)
		{
			helppage++;
			if (helppage>MAXHELPPAGE)
				helppage = 1;
			setuphelp();
		}
		break;
	case ESC:
	case KEY_F(10):
	case KEY_END:
		if (watch || help)
		{
			watch = help = FALSE;
			disprouterstats = FALSE;
			if (localkey)
				refreshrem = TRUE;
			else
				refreshloc = TRUE;
		}
		break;
	case KEY_NPAGE:
	case KEY_DOWN:		/* DOWN KEY */
	  ydisp += (LINES - 5);
	  refreshgen = TRUE;
	  break;
	case KEY_LEFT:		/* LEFT KEY */
	  dispopt--;
	  if (dispopt < 0)
	    dispopt = DISP_MAX;
	  poschange = 1;
	  rewrite_labels = 1;
	  break;
	case KEY_PPAGE:
	case KEY_UP:		/* UP KEY */
	  ydisp -= (LINES - 5);
	  if (ydisp < 0)
	    ydisp = 0;
	  refreshgen = TRUE;
	  break;
	case KEY_RIGHT:		/* RIGHT KEY */
	  dispopt++;
	  if (dispopt > DISP_MAX)
	    dispopt = 0;
	  rewrite_labels = 1;
	  poschange = 1;
	  break;
	case KEY_F(1):
	case 'h':
	case 'H':
		help = TRUE;
		setuphelp();
	  break;
	case KEY_F(4):
	case 'w':
	case 'W':
		watch = TRUE;
		disprouterstats=FALSE;
		setupwatch();
	  break;
	  case 'R':
	  case 'r':
	  	if (watch)
	  	{
	  		setupauxscr(&dum);
	  		disprouterstats = TRUE;
	  	}
	  	break;
	case '\t':		/* Switch from LOCAL to REMOTE sides of screen */
	  localkey++;
	  localkey &= 1;
         if (localkey)
	   ydisp = lydisp;
         else
	   ydisp = rydisp;
	  if (watch)
	  {
		setupwatch();
  	  }
	  else if (help)
	  	setuphelp();
	 if (watch || help)
	 {
		 if (localkey)
			refreshrem = TRUE;
		 else
			refreshloc = TRUE; 
	}
	  break; 
	case 'c':
	case 'C':
	  /* Clear Counters */
	  if (watch && disprouterstats)
	  {
	  	maxburst = 0;
	  	break;
	  }
	  current = lfirst->flink;
	  while (current != lfirst)
	    {
	      current->update = 2;
	      current->pktcntsend = current->pktcntrec = 0;
	      current->sendbytes = current->recbytes = 0;
	      current = current->flink;
	    }
	  localupdate = 1;
	  current = rfirst->flink;
	  while (current != rfirst)
	    {
	      current->update = 2;
	      current->pktcntsend = current->pktcntrec = 0;
	      current->sendbytes = current->recbytes = 0;
	      current = current->flink;
	    }
	  remoteupdate = 1;
	  break;
	case 'n':
	case 'N':
	  /* Start NEW.... Delete all entries...start again */
	  llockout = 1;
	  current = lfirst->flink;
	  lfirst->flink = lfirst;
	  lfirst->blink = lfirst;
	  while (current != lfirst)
	    {
	      work = current->flink;
	      free (current);
	      current = work;
	    }
	  llockout = 0;
	  rlockout = 1;
	  current = rfirst->flink;
	  rfirst->flink = rfirst;
	  rfirst->blink = rfirst;
	  while (current != rfirst)
	    {
	      work = current->flink;
	      free (current);
	      current = work;
	    }
	  rlockout = 0;
	  rewrite_labels = 1;
	  poschange = 1;
	  break;
	case 'q':
	case 'Q':
	  break;
	case ' ':
	  mvprintw (0, 60, "%c", ACS_BLOCK);
	  bugfix = TRUE;
	
	default:
/*	  mvprintw (0, 60, "%02x", in_char); */
	  break;
	}
      if (localkey)
	{
	  lydisp = ydisp;
	  refreshloc = refreshgen;
	}
      else
	{
	  rydisp = ydisp;
	  refreshrem = refreshgen;
	}
   wai = 4;
    }				/* while a 'q' was not typed */


  /* TERMINATE */

/*
 */
  cleanup_curses ();

  /* Should this be rewritten to cooperate with other net tools? */
  strcpy (oldifr.ifr_name, ethdevname);
  if (ioctl (sd, SIOCSIFFLAGS, &oldifr) < 0)
    {
      perror ("Can't set flags: ");
      close (sd);
      exit (4);
    }

  close (sd);
  exit (0);
}


void
handle_other (unsigned char *buf, int length)
{

  /* frame_protocol is global */
  if (frame_protocol < 1501)
    {				/* if IEEE 802.3 packet instead of Ethernet packet (per RFC 1700) */
      if ((short int) buf[14] <= SN_MAX_SAP)
	{
	  sap_count[(short int) buf[14]]++;	/* count 802.2 SAP number */
	}
      regis.new_ethernet_count++;
    }				/* if IEEE 802.3 packet instead of Ethernet packet */
  else
    {				/* else is an Ethernet packet */
      switch (((struct at_frame_type *) buf)->appletalk_type1)	/* Appletalk */
	{
	case __constant_ntohs (ETH_P_ATALK):
/*	  if (regis.at_option)
	    exatalk ((struct at_frame_type *) buf, length); */
	  break;
	case __constant_ntohs (0x80F3):
	  regis.aarp++;
	  break;
	default:
	  break;
	}
    }				/* else is an Ethernet packet */
}

void
makeaddr (unsigned char naddr[], char ascii[])
{
  sprintf (ascii, "%u.%u.%u.%u        ", naddr[0], naddr[1], naddr[2], naddr[3]);
  ascii[15] = 0;
  return;
}

int
tlocal (unsigned long *addr)
{
  static unsigned char lhost[] =
  {127, 0, 0, 1};
  unsigned long *k = (unsigned long *) netmask;
  unsigned long reslocal, restest;
  if (*addr == *(unsigned long *) lhost)
    return (TRUE);
  restest = *addr & *k;
  reslocal = *(unsigned long *) localaddr & *k;
  return (restest == reslocal);
}

void
searchforinsertion (unsigned long key, HOSTINFO * first)
{
  current = first->flink;
  while (current != first && key < (unsigned long)ntohl(*(unsigned long *)current->addr))
    current = current->flink;
}

void
updatecurrent (HOSTINFO * work, struct ip *buf, int length, int opt)
{
  /* ASSUME "work" is a WORKING GLOBAL CURRENT RECORD!!!! NO!!!! */
  int x;
  unsigned long wlen;
  
  wlen = (unsigned long)ntohs(buf->tot_len);
  /* Update current entries */
  if (opt)			/* DEST */
    {
      work->pktcntrec++;
      work->recbytes += wlen;
    }
  else
    {
      work->pktcntsend++;
      work->sendbytes += wlen;
    }
  work->tstamp = new;
  x = buf->ip_p;
  if (x <= 100 && x >= 0)
    strcpy (work->ip_pr, ip_protocol_types[x]);
  else if (x > 0 && x < 256)
    sprintf (work->ip_pr, "UNK %d", x);
  else
    sprintf (work->ip_pr, "ILL %d", x);
  switch (buf->ip_p)
    {
    case IPPROTO_TCP:

      if ((x = ntohs (((struct tcphdr *) ((void *) buf + 20))->th_sport)) <= SN_MAX_TCP_PORTS)
	strcpy (work->servicename, tcp_port_types[x]);
      else
	sprintf (work->servicename, "UNK %d", x);
      break;
    case IPPROTO_UDP:
      if ((x = ntohs (((struct udphdr *) ((void *) buf + 20))->th_sport)) <= SN_MAX_UDP_PORTS)
	strcpy (work->servicename, udp_port_types[x]);
      else
	sprintf (work->servicename, "UNK %d", x);
      break;
    case IPPROTO_ICMP:
      x = *(u_char *) ((void *) buf + 20);
      if (x == ICMP_ECHO)
	strcpy (work->servicename, "ECHO");
      else if (x == ICMP_ECHOREPLY)
	strcpy (work->servicename, "ECHO REPLY");
      else
	sprintf (work->servicename, "OTH %d", x);
      break;
    default:
      strcpy (work->servicename, "?????");
    }


}

/* opt = 0 SOURCE   opt = 1  DEST. */

void
addtolocallist (unsigned long *key, unsigned long *okey, struct ip *buf, int length, int opt)
{
  static int mcnt = 0;
  static int ncnt = 0;
  unsigned char *pk = (unsigned char *) key;

  searchforinsertion ((unsigned long)ntohl(*key), lfirst);
  if (*(unsigned long *) current->addr != *key)
    {
      work = (HOSTINFO *) malloc (sizeof (*work));
      previous = current->blink;
      /* Init values to ZERO for 1st entry.... */
      work->pktcntsend = work->pktcntrec = work->sendbytes = work->recbytes = 0;
      *(unsigned long *) work->addr = *key;
      work->disprow = previous->disprow + 1;
      work->update = 1;
      localupdate = 1;
      hostent = gethostbyaddr ((char *) key, 4, AF_INET);
      if (hostent)
	strcpy (work->name, hostent->h_name);
      else
	sprintf (work->name, "%u.%u.%u.%u", pk[0], pk[1], pk[2], pk[3]);
      llockout = 1;
      previous->flink = work;
      work->flink = current;
      current->blink = work;
      work->blink = previous;
      llockout = 0;
    }
  else
    {
      work = current;
/*      mcnt++;
   mvprintw(23,70,"U %d",mcnt); */
      work->update = 2;		/* just info update */
      localupdate = 1;
    }
  *(unsigned long *) work->othaddr = *okey;
  updatecurrent (work, buf, length, opt);
}

void
addtoremotelist (unsigned long *key, unsigned long *okey, struct ip *buf, int length, int opt)
{
  unsigned char *pk = (unsigned char *) key;
  int x;

  searchforinsertion ((unsigned long)ntohl(*key), rfirst);
  if (*(unsigned long *) current->addr != *key)
    {
      work = (HOSTINFO *) malloc (sizeof (*work));
      previous = current->blink;
      /* Init values to ZERO for 1st entry.... */
      work->pktcntsend = work->pktcntrec = work->sendbytes = work->recbytes = 0;
      work->disprow = previous->disprow + 1;
      work->update = 1;
      remoteupdate = 1;
      *(unsigned long *) work->addr = *key;
      hostent = gethostbyaddr ((char *) key, 4, AF_INET);
      if (hostent)
	strcpy (work->name, hostent->h_name);
      else
	sprintf (work->name, "%u.%u.%u.%u", pk[0], pk[1], pk[2], pk[3]);
      rlockout = 1;
      previous->flink = work;
      work->flink = current;
      current->blink = work;
      work->blink = previous;
      rlockout = 0;
    }
  else
    {
      work = current;
      work->update = 2;		/* just info update */
      remoteupdate = 1;
    }
  *(unsigned long *) work->othaddr = *okey;
  updatecurrent (work, buf, length, opt);

}

void
handle_ip (struct ip *buf, int length)
{
  static int x;
  long sourcel;
  long destl;
  struct hostent *hostent;
  unsigned long wlen;

  new = time (0);
  wlen = (unsigned long)ntohs(buf->tot_len);
  if (buf->ip_p <= SN_MAX_IP_PORT)
    {				/* if IP protocol type is to be tallied */
      ip_protocol_count[buf->ip_p]++;
    }				/* if IP protocol type is to be tallied */
  if (tlocal ((long int *) &buf->saddr))
    addtolocallist ((unsigned long int *) &buf->saddr, (unsigned long int *) &buf->daddr, buf, length, 0);
  else
  {
    addtoremotelist ((unsigned long int *) &buf->saddr, (unsigned long int *) &buf->daddr, buf, length, 0);
    routeruse += wlen;
    routerfrom += wlen;
   }    
  if (tlocal ((long int *) &buf->daddr))
    addtolocallist ((unsigned long int *) &buf->daddr, (unsigned long int *) &buf->saddr, buf, length, 1);
  else
  {
    addtoremotelist ((unsigned long int *) &buf->daddr, (unsigned long int *) &buf->saddr, buf, length, 1);
    routeruse += wlen;
    routerto += wlen;
  }    
  switch (buf->ip_p)		/* IP Protocol */
    {

    case IPPROTO_TCP:		/* TCP */
      {
	if (buf->ip_off != 0)
	  {			/* if this is not the first frame of a fragmented packet */
	    tcp_port_count[0]++;	/* count fragment */
	  }			/* if this is not the first frame of a fragmented packet */
	else
	  {			/* else this is the first frame of a packet */
	    /* The below may count packets twice, but probably both are not displayed */
	    if (buf->ip_hl == 5)
	      {			/* if IP header is normal length we can assume 20 octects length */
		if ((x = ntohs (((struct tcphdr *) ((void *) buf + 20))->th_sport)) <= SN_MAX_TCP_PORTS)
		  tcp_port_count[x]++;	/* count source port */
		if ((x = ntohs (((struct tcphdr *) ((void *) buf + 20))->th_dport)) <= SN_MAX_TCP_PORTS)
		  tcp_port_count[x]++;	/* count dest port */
	      }			/* IP header normal length */
	    else
	      {			/* IP header with options */
		/* Incidentally, the (void *) is to cause byte-level math not sizeof(struct ip) */
		if ((x = ntohs (((struct tcphdr *) ((void *) buf + ((buf->ip_hl) * 4)))->th_sport)) <= SN_MAX_TCP_PORTS)
		  tcp_port_count[x]++;	/* count source port */
		if ((x = ntohs (((struct tcphdr *) ((void *) buf + ((buf->ip_hl) * 4)))->th_dport)) <= SN_MAX_TCP_PORTS)
		  tcp_port_count[x]++;	/* count dest port */
	      }			/* IP header with options */
	  }			/* else this is the first frame of a packet */
      }
      break;

    case IPPROTO_UDP:		/* UDP */
      {
	if (buf->ip_off != 0)
	  {			/* if this is not the first frame of a fragmented packet */
	    udp_port_count[0]++;	/* count fragment */
	  }			/* if this is not the first frame of a fragmented packet */
	else
	  {			/* else this is the first frame of a packet */
	    /* The below may count packets twice, but probably both are not displayed */
	    if (buf->ip_hl == 5)
	      {			/* if IP header is normal length we can assume 20 octects length */
		if ((x = ntohs (((struct udphdr *) ((void *) buf + 20))->ui_sport)) <= SN_MAX_UDP_PORTS)
		  udp_port_count[x]++;	/* count source port */
		if ((x = ntohs (((struct udphdr *) ((void *) buf + 20))->ui_dport)) <= SN_MAX_UDP_PORTS)
		  udp_port_count[x]++;	/* count dest port */
	      }			/* IP header normal length */
	    else
	      {			/* IP header with options */
		/* Incidentally, the (void *) is to cause byte-level math not sizeof(struct ip) */
		if ((x = ntohs (((struct tcphdr *) ((void *) buf + ((buf->ip_hl) * 4)))->ui_sport)) <= SN_MAX_UDP_PORTS)
		  udp_port_count[x]++;	/* count source port */
		if ((x = ntohs (((struct tcphdr *) ((void *) buf + ((buf->ip_hl) * 4)))->ui_dport)) <= SN_MAX_UDP_PORTS)
		  udp_port_count[x]++;	/* count dest port */
	      }			/* IP header with options */
	  }			/* else this is the first frame of a packet */
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
  int prot_int;
  int search_int;
  int prot_now;
  struct ip *ip_ptr;

  ip_ptr = (struct ip *) ((void *) buf + ETH_HLEN);

  if (length > 0)
    {

      packet_type = ((struct ether_head *) buf)->ether_type;	/* Ethernet packet type ID field */
      frame_protocol = ntohs (packet_type);	/* Convert from network to host seq */
      if (frame_protocol < 1501)
	{			/* if an IEEE 802.3 packet */
	  frame_protocol = SN_PROT_IEEE802_3;
	}			/* if an IEEE 802.3 packet */

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
	  ip_ptr = (struct ip *) ((void *) buf);	/* No Ethernet Header */
	}
      else if (strncmp (saddr->sa_data, "sl", 2) == 0)
	{
	  regis.slipbytes += length;
	  regis.slipcount++;
	  frame_protocol = SN_PROT_SLIP;
	  ip_ptr = (struct ip *) ((void *) buf);	/* No Ethernet Header */
	}
      else
	{
	  regis.otherbytes += length;
	  regis.othercount++;
	}

      if (frame_protocol == SN_PROT_IEEE802_3)
	{			/* if an IEEE 802.3 packet */
	  for (prot_int = 0; prot_int < SN_NUM_PROTOCOLS; prot_int++)
	    {			/* for all protocols */
	      if ((prot_now = regis.prot_types[prot_int]) >= 0)
		{		/* if a protocol to display is defined */
		  if (protocol_num[prot_now] == SN_PROT_IEEE802_3)
		    {		/* if this is zero, the protocol used for IEEE 802.3 */
		      protocol_count[prot_int]++;	/* tally this frame */
		      break;
		    }		/* if this is zero, the protocol used for IEEE 802.3 */
		}		/* if a protocol to display is defined */
	    }			/* for all protocols */
	  if (prot_int >= SN_NUM_PROTOCOLS)
	    {			/* if this protocol type was not found */
	      regis.prot_types[SN_NUM_PROTOCOLS - 1] = SN_PROT_IEEE802_3;	/* mark this as IEEE 802.3 protocol */
	      protocol_count[SN_NUM_PROTOCOLS - 1] = 1;		/* tally this frame */
	    }			/* if this protocol type was not found */
	}			/* if an IEEE 802.3 packet */
      else
	{			/* if not an IEEE 802.3 packet */

	  for (prot_int = 0; prot_int < SN_NUM_PROTOCOLS; prot_int++)
	    {			/* for all protocols */
	      if ((prot_now = regis.prot_types[prot_int]) >= 0)
		{		/* if a protocol to display is defined */
		  if (protocol_num[prot_now] == frame_protocol)
		    {		/* if this is the protocol number which was encountered */
		      protocol_count[prot_int]++;	/* tally this frame */
		      break;
		    }		/* if this is the protocol number which was encountered */
		}		/* if a protocol to display is defined */
	    }			/* for all protocols */

	  if (prot_int >= SN_NUM_PROTOCOLS)
	    {			/* if protocol was not found */
	      for (search_int = 0; search_int < SN_MAX_PROTO_DESC; search_int++)
		{		/* for all known protocols */
		  if (frame_protocol == protocol_num[search_int])
		    {		/* if packet type was found */
		      if (regis.prot_types[SN_NUM_PROTOCOLS - 1] >= 0 &&
			  protocol_count[SN_NUM_PROTOCOLS - 1] > 0)
			{	/* if last displayed protocol was in use, tally as unknown */
			  regis.unknown_type += protocol_count[SN_NUM_PROTOCOLS - 1];	/* add this count */
			  regis.unknown_frame_type = protocol_num[regis.prot_types[SN_NUM_PROTOCOLS - 1]];	/* remember protocol */
			}	/* if last displayed protocol was in use, tally as unknown */
		      regis.prot_types[SN_NUM_PROTOCOLS - 1] = search_int;
		      protocol_count[SN_NUM_PROTOCOLS - 1] = 1;		/* tally this frame */
		      break;
		    }		/* if packet type was found */
		}		/* for all known protocols */

	      if (search_int >= SN_MAX_PROTO_DESC)
		{		/* if protocol was not found */
		  regis.unknown_type++;
		  regis.unknown_frame_type = frame_protocol;
		}		/* if protocol was not found */
	    }			/* if protocol was not found */
	}			/* if not an IEEE 802.3 packet */

      switch (frame_protocol)
	{
	case ETH_P_IP:
	case SN_PROT_PPP:
	case SN_PROT_SLIP:
	case SN_PROT_LOOP:
	    handle_ip (ip_ptr, length);
	  break;
	default:
	    handle_other (buf, length);
	  break;
	}

    }				/* if length > 0 */
}

void
usage (char *arg)
{
	fprintf(stderr,"\n%s [-h][-c rcinetfile][-e ethdevice]\n\n",arg);
	fprintf(stderr,"Network Watch (Ethernet/IP)\nVersion %s\n\n",version);
	fprintf(stderr,"\t-c rcinetfile\tAlternate to System rc.inet1 file\n");
	fprintf(stderr,"\t-e ethnum\tAlternate to eth0 ( -e eth1  for eth1 )\n");
	fprintf(stderr,"\t-h\t\tHelp Message (this)\n\n");
	exit(0);
}
