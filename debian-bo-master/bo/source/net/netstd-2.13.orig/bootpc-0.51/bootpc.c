/*
  Last updated : Thu Apr 18 00:31:38 1996
  Modified by JSP from code by Charles Hawkins <ceh@eng.cam.ac.uk>,

    J.S.Peatfield@damtp.cam.ac.uk

  Copyright (c) University of Cambridge, 1993,1994,1995,1996

  $Revision: 1.7 $
*/

/* Standard headers */
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <linux/if.h>
#include <linux/sockios.h>
#include <net/if_arp.h>
#include <sys/time.h>
#include <unistd.h>
#include <string.h>
#include <getopt.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>

/* local headers */
#include "bootp.h"

#define BPCVERSION "BOOTPclient V0.51"

/* Tell the server to broadcast to reach me flag */
#define BPFLAG_BROADCAST ( 1 << 16 )

/* Back in NET2 (and before?) the ifreq.ifr_hwaddr was a char array,
   but in NET3 it is now a "sockaddr", and we need the data part.

   The code to work on older kernels has now been removed as it caused
   problems on some systems where this test no longer works (e.g. the
   AlphaLinux).  Is anyone really running 1.1.13 or earlier kernels
   anymore?

   If you still have an old kernel and need bootpc stick with version
   0.45 which is the last release to support the NET2 code.  

*/

/* for extracting the right part... */
#define use_hwaddr ifr_hwaddr.sa_data

/* Needed for getopt stuff */
extern char *optarg;
extern int optind, opterr, optopt;

/* My forward declarations */
void FatalError();

void ParseCookie(unsigned char *cookie,
		 int cookielength,
		 unsigned char *match,
		 struct in_addr my_addr) ;

void PrintList(char *name,
	       unsigned char *cookie,
	       int pos,
	       int len) ;

void PrintString(char *name,
	       unsigned char *cookie,
	       int pos,
	       int len) ;

void PrintSearch(char *name,
		 unsigned char *cookie,
		 int pos,
		 int len) ;

void safeprint(unsigned char *string,
	       int len) ;

int in2host(char *address) ;

int pingit(char *address) ;

/* My global variables */
static int verbose = 0 ;   /* verbose mode or not 10/02/94 JSP */
static int debug   = 0 ;   /* debug mode or not 14/02/94 JSP */
static int subnet  = 0 ;   /* if we have seen a subnet TAG yet (sigh) */
static int returniffail=0 ;  /* Return to the user if we fail */

int main(int argc,
	 char *argv[])
{
  int sockfd;
  struct sockaddr_in cli_addr, serv_addr;
  struct bootp *bootp_xmit, *bootp_recv;
  struct ifreq ifr;
  fd_set rfds, wfds, xfds;
  struct timeval timeout ;
  int32 rancopy ;
  int cookielength ;
  long plen ;
  struct in_addr temp_addr ;
  int retry_wait, waited=0, timeout_wait = 70;
  int waitformore=-1 ; /* How long to wait after 1st reply for more replies */

  int one=1, i, givenhwaddr ;
  struct timeval tp;
  int received_packet = 0 ;
  char *device, *bootfile, *server ;
/* See RFC1497, RFC1542  09/02/94   JSP  */
  unsigned char mincookie[] = {99,130,83,99,255} ;

/* defaults unless overridden by command line options 10/02/94  JSP */
  device = "eth0" ;             /* first ethernet card */
  bootfile = "" ;               /* No bootfile by default */
  server = "255.255.255.255" ;  /* i.e broadcast to everyone */
  givenhwaddr = 0 ;             /* i.e. use our real HW address */

/* Setting the default bootfile to "linux" seemed to cause problems for
   the CMU-2.1 bootpd, though I can't tell why, I don't run it here...
   Perhaps it really should default to "vmlinuz" ... 17/06/94  JSP
*/

  while (1) {
    int option_index = 0, option ;
    static struct option long_options[] = {
      {"bootfile", 1, 0, 1},
      {"dev", 1, 0, 2},
      {"verbose", 0, 0, 3},
      {"debug", 0, 0, 4},
      {"server", 1, 0, 5},
      {"hwaddr", 1, 0, 6},
      {"returniffail", 0, 0, 7},
      {"timeoutwait", 1, 0, 8},
      {"waitformore",1, 0, 9},
      {"in2host", 1, 0, 10},
      {"ping", 1, 0, 20},
      {"help", 0, 0, 100},
      {0, 0, 0, 0},
    } ;

    option = getopt_long (argc, argv, "", long_options, &option_index);

    if (option == -1)
      break ;

    switch (option) {
    case 1:  /* New bootfile */
      bootfile = optarg ;
      if (strlen(bootfile) > 127) { /* buffer space for 128 only */
	if (verbose)
	  fprintf(stderr, "Bootfile %s too long, truncating\n", bootfile) ;
	bootfile[127] = 0;
      }
      break ;
    case 2:  /* New device */
      device = optarg ;
      if (strlen(device) > IFNAMSIZ-1) {  /* only IFNAMSIZ space in struct */
	if (verbose)
	  fprintf(stderr, "device name %s too long, truncating\n", device) ;
	device[IFNAMSIZ -1] = 0;
      }
      break ;
    case 3:
      verbose = 1 ;
      break ;
    case 4:
      debug = 1 ;
      break ;
    case 5:
      server = optarg ;
      break ;
    case 6:
      /* This MAY be useful for some types of debugging, however all
	 the bootpd programs I have reply to the hardware address
	 given here, thus we never see the replies.  Other bootpds may
	 not so it may be possible to use this to test a bootpd will
	 respond for another HW address.  17/08/94  JSP */
      { int error, count ;
	unsigned int value ;
	char cc ;

	for (i=0; i < IFHWADDRLEN; ++i) {  /* get the MAC address from user */
	  error = sscanf(optarg, "%2x%n%[ :.]%n", &value,&count,&cc,&count) ;
	  ifr.use_hwaddr[i] = value ;
	  if (error <= 0) {   /* Not enough given */
	    if (debug)
	      fprintf(stderr, "Ran out of numbers in hwaddr, ignoring\n") ;
	    break ;
	  }
	  optarg += count ;
	}
	givenhwaddr = 1 ;
      }
      break ;
    case 7:
      returniffail = 1 ;
      break ;
    case 8:
      timeout_wait = atoi(optarg) ;
      break ;
    case 9:
      waitformore = atoi(optarg) ;
      break ;
    case 10:
      /* used for the reverse lookup to hostname */
      return in2host(optarg) ;
      break ;
    case 20:
      /* used as a sanity check on the IP number we get back in case of
	 duplicate addresses */
      return pingit(optarg) ;
      break ;
    case 100:
      fprintf (stderr, "%s is used to find the IP number and other setup\n"
	      "information for a machine\n"
	      "\n", argv[0]) ;
    default:
      fprintf (stderr, "\t%s\n", BPCVERSION) ;
      fprintf (stderr,
	       "Usage: %s\t[--dev device] [--bootfile file] [--verbose]\n"
	       "\t\t[--server address] [--hwaddr mac-address]\n"
	       "\t\t[--timeoutwait seconds]\n"
	       "\t\t[--waitformore seconds]\n"
	       "\t\t[--in2host address]\n"
	       "\t\t[--ping    address]\n"
	       "\t\t[--help] [--returniffail]\n", argv[0]) ;
      exit (1) ;
    }
  }

  if (verbose) {
    fprintf (stderr, "\t%s\n\tdevice=%s  bootfile=%s timeout=%d\n\n",
	     BPCVERSION, device, bootfile, timeout_wait ) ;
  }

/* zero structure before use */
  memset((char *) &serv_addr, 0, sizeof(serv_addr));

  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = inet_addr(server) ;
  serv_addr.sin_port = htons(IPPORT_BOOTPS);

  if((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("bootpc: socket failed");
    FatalError();
  }
  
  if (setsockopt(sockfd,SOL_SOCKET,SO_BROADCAST,&one,sizeof(one))==-1) {
    perror("bootpc: setsockopt failed");
    FatalError();
  }
  
  memset((char *) &cli_addr, 0, sizeof(cli_addr));
  cli_addr.sin_family = AF_INET;
  cli_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  cli_addr.sin_port = htons(IPPORT_BOOTPC);

  if(bind(sockfd, (struct sockaddr *) &cli_addr, sizeof(cli_addr)) < 0) {
    perror("bootpc: bind failed");
    FatalError();
  }

/* allocate bootp packet before we use it */
  bootp_xmit = (struct bootp *) malloc(BUFSIZ) ;
  memset((char *) bootp_xmit, 0, BUFSIZ) ;

  bootp_recv = (struct bootp *) malloc(BUFSIZ) ;
  memset((char *) bootp_recv, 0, BUFSIZ) ;

/* Don't do this if we were given the MAC address to use.  27/09/94  JSP */
  if (givenhwaddr) {
    /* If I'm spoofing my HW address then have the reply broadcast */
    bootp_xmit->bp_flags |= BPFLAG_BROADCAST;
    ifr.ifr_hwaddr.sa_family = ARPHRD_ETHER ;  /* Assuming ETHER if given HW */
  } else {
/* Get the hardware address, and family information */
    memcpy(ifr.ifr_name, device, strlen(device)+1);
    if (ioctl(sockfd, SIOCGIFHWADDR, &ifr) < 0) {
      perror("bootpc: ioctl failed");
      FatalError();
    }
  }

/* Check the network family if in NET3 or later, before NET3 you couldn't
   examine this info (as far as I know.) */

/* set the htype field from the sa.family of the hardware address, if
   this doesn't work for your interface type let me know please. */

  bootp_xmit->bp_htype = ifr.ifr_hwaddr.sa_family;
  bootp_xmit->bp_hlen = IFHWADDRLEN ;  /* All MAC addresses are the same length */
  
  if (debug)
    fprintf(stderr, "Got family=%d (Ether=%d)\n",
	    bootp_xmit->bp_htype, ARPHRD_ETHER);

/* If we have the time seed with it xor the hardware address, otherwise
   use the hardware address 12/02/94 JSP */
  if (gettimeofday(&tp, NULL) == -1)
    rancopy = 0 ;
  else
    rancopy = tp.tv_sec + tp.tv_usec ;

/* Do the XOR */
  for (i=0; i < IFHWADDRLEN ; ++i) {
    ((unsigned char *)&rancopy)[ i % sizeof(rancopy) ] ^=
      ((unsigned char *)(ifr.use_hwaddr))[i] ;
  }
/* and set the seed */
  srand(rancopy) ;

  if(debug) {
    fprintf(stderr, "hardware addr is :") ;
    for (i=0; i < bootp_xmit->bp_hlen ; ++i)
      fprintf (stderr, "%2.2X ", ((unsigned char *)(ifr.use_hwaddr))[i]) ;
    fprintf(stderr, "\n") ;
  }

/* Now fill in the packet. */
  bootp_xmit->bp_op = BOOTREQUEST ;

/* Now with my understanding of the bootp protocol we *should* just
   need to copy the hwaddr over, but it seems that at least ARCNET
   bootb servers are wird in this respect.  So here is a switch in
   case of other weirdness.  JSP */

  switch(bootp_xmit->bp_htype) {
/* ARCNET uses a "fake" ethernet address, with the ARCNET address at
   the wrong end.  At least the Novell bootp server on ARCNET assumes
   this.  Thanks to Tomasz Motylewski <motyl@tichy.ch.uj.edu.pl> for
   reporting this.  */
  case ARPHRD_ARCNET :
    memcpy(bootp_xmit->bp_chaddr+IFHWADDRLEN-1, (char *)(ifr.use_hwaddr), 1) ;
    bootp_xmit->bp_htype=ARPHRD_ETHER;
    bootp_xmit->bp_hlen=IFHWADDRLEN;
    break ;

/* Add other network weirdness here */

/* For sensible networks the rest is normal */
  default :
    memcpy(bootp_xmit->bp_chaddr,
	   (char *)(ifr.use_hwaddr),
	   bootp_xmit->bp_hlen) ;
  }

/* Must start with zero here, see RFC1542 09/02/94 JSP */
  bootp_xmit->bp_secs = 0;

/* Put in the minimal RFC1497 Magic cookie 09/02/94 JSP */
  memcpy(bootp_xmit->bp_vend, mincookie, sizeof(mincookie));

/* Put the user precified bootfile name in place 12/02/94 */
  memcpy(bootp_xmit->bp_file, bootfile, strlen(bootfile)+1);

/* put a random value in here, but keep a copy to check later 09/02/94  JSP */
  bootp_xmit->bp_xid = rancopy = rand() ;

  retry_wait = 2 ;
  if (verbose)
    fprintf(stderr,"BOOTPclient broadcast...\n");

  while (((waited <= timeout_wait) && !received_packet) ||
	 ((waited <= waitformore) && received_packet)) {
    
    if (!received_packet) {  /* Move this to a sendpacket function */
      /* set time of this timeout  09/02/94  JSP */
      bootp_xmit->bp_secs = waited ;
      if (verbose) {
	fprintf(stderr,"."); fflush(stderr);
      }
      if (debug) {
	printf("Size = %ld\n", (long)sizeof(struct bootp)) ;
      }

      if(sendto(sockfd, bootp_xmit, sizeof(struct bootp), 0, 
		(struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) {
	perror("bootpc: sendto");
	FatalError();
      }
    }

    /* Move rest of this loop to a receivepacket function */
    FD_SET(sockfd,&rfds);
    FD_ZERO(&wfds);
    FD_ZERO(&xfds);

/* Randomise the delays a little as suggested in RFC1542  09/02/94  JSP */
    timeout.tv_sec = retry_wait + (1+(rand() & (retry_wait-1))) ;
    timeout.tv_usec = 0;
    waited += timeout.tv_sec ;  /* Add this to the total time we have waited */

    if(select(sockfd+1, &rfds, &wfds, &xfds, &timeout)<0) {
      perror("bootpc: select");
      FatalError();
    }

    if(!FD_ISSET(sockfd, &rfds)) {
      retry_wait = retry_wait*2;
    } else {
      if ((plen = recvfrom(sockfd, bootp_recv, BUFSIZ, 0,
			   (struct sockaddr *)NULL, (int *)NULL)) < 0){
	perror("bootpc: recvfrom");
	FatalError();
      }

      if (debug) {
	fprintf(stderr, "plen = %ld  plen - sizeof(struct bootp) = %ld\n",
		(long)plen, (long)(plen - sizeof(struct bootp))) ;
      }
      cookielength = 64 + plen - sizeof(struct bootp) ;
      
/* Check xid is what we asked for  09/02/94  JSP */
      if (bootp_recv->bp_xid != rancopy) {
	fprintf(stderr, "WARNING bp_xid mismatch got 0x%lx sent 0x%lx\n",
		(long)bootp_recv->bp_xid, (long)rancopy) ;
	if (!debug) /* Unless debugging, ignore packet */
/* WRONG, but we need to do something fatal, we actually
   want to go back to the receive part again to listen
   for more packets */
	  exit(1) ;
      }

      if (!received_packet) {
	/* If we haven't already recieved a packet then set the time to wait
	   further to be now + time user specified */
	waitformore += waited ;
	received_packet = 1 ;
      } else {
	/* To make it look a bit prettier */
	printf ("\n") ;
      }

      temp_addr.s_addr = bootp_recv->bp_siaddr.s_addr ;
      printf("SERVER=%s\n", inet_ntoa(temp_addr));
      temp_addr.s_addr = bootp_recv->bp_yiaddr.s_addr ;
      printf("IPADDR=%s\n", inet_ntoa(temp_addr));
      PrintString("BOOTFILE",
		  (unsigned char *)bootp_recv->bp_file, 0,
		  strlen(bootp_recv->bp_file)) ;

/* Pass the cookie info, the mincookie to look for and our address to
   the cookie parser.  It needs our address to get the network and
   broadcast bits right if the SUBNET is defined in the cookie.
   10/02/94  JSP */

      ParseCookie((unsigned char *)&(bootp_recv->bp_vend),
		  cookielength,
		  mincookie,
		  temp_addr) ;

/* No SUBNET TAG in the cookie so we fake guess here, if this is wrong
   then fix your bootp server to tell us the answer rather than
   hacking this code. */

      if (!subnet) {
	struct in_addr network ;
	int type ;
	
	if (verbose)
	  fprintf(stderr, "Guessing network from IP address range\n") ;

	type = ntohl(temp_addr.s_addr) ;
	if ((type & 0x80000000) == 0) {
	  /* Class A */
	  network.s_addr = htonl(0xFF000000) ;
	} else if ((type & 0x40000000) == 0) {
	  /* Class B */
	  network.s_addr = htonl(0xFFFF0000) ;
	} else if ((type & 0x20000000) == 0) {
	  /* Class C */
	  network.s_addr = htonl(0xFFFFFF00) ;
	} else { /* GOD KNOWS... other classes are weird */
	  if (verbose)
	    fprintf(stderr, "IP number not Class A,B or C,\n"
		    "setting NETMASK to zero\n") ;
	  network.s_addr = htonl(0x00000000) ;
	}
	temp_addr.s_addr &= network.s_addr ;
	printf("NETWORK=%s\n", inet_ntoa(temp_addr));
	temp_addr.s_addr |= ~network.s_addr ;
	printf("BROADCAST=%s\n", inet_ntoa(temp_addr));
      }
    }
  }
  if (!received_packet) {
    fprintf(stderr, "\nNo response from BOOTP server\n");
    FatalError();
  }

  return 0 ;  /* Normal exit */
}
    
void FatalError()
{
  if (debug)
    fprintf(stderr, "In FatalError(), errno was %d\n", errno) ;

  if (returniffail) {
    fprintf(stderr, "bootpc failed to locate a network address\n") ;
    exit(1) ;
  }

  fprintf(stderr,
	  " Unable to locate an IP address for this host.\n"
	  "     ***Please report this problem**\n\n"
	  "          [Unable to continue]\n\n");

  if (debug)
    fprintf(stderr, "Will now loop forerver, break out of this to fix\n\n") ;

  while(1) {
    /* your eyes are getting heavy.... */
    sleep(1000) ;
  }
}

/* Parse Magic cookies as specified in RFC1497, well only the bits we
   are actually interested in...  09/02/94 JSP
*/
void ParseCookie(unsigned char *cookie,
		 int cookielength,
		 unsigned char *match,
		 struct in_addr my_addr)
{
  int i=0, len, tag ;
  struct in_addr temp ;

  if (debug) {  /* dump cookie contents in HEX 10/02/94  JSP */
    for (i=0; i<cookielength; i++) {
      if ((i%8) == 0)
	fprintf(stderr, "\n %2.2d :", i) ;
      fprintf(stderr, " 0x%2.2X", cookie[i]) ;
    }
    fprintf(stderr, "\n") ;
  }

/* Must get the same cookie back as we sent  09/02/94  JSP */
  for (i=0; i < 4; ++i) {
    if (cookie[i] != match[i]) {
      if (verbose)
	fprintf(stderr, "RFC1497 Cookie mismatch at offset %d\n", i) ;
      return ;
    }
  }

  if (verbose)
    fprintf(stderr, "found valid RFC1497 cookie, parsing...\n") ;

/* Carry on after the cookie for other data  09/02/94  JSP */
  while (i < cookielength) {
    tag = cookie[i] ;

    if (verbose)
      fprintf(stderr, "cookie position %d is %d\n", i, tag) ;

/* If we arn't at the end of the cookie and we will need it extract len */
    if ((i < cookielength - 1) && (tag != TAG_PAD) && (tag != TAG_END))
      len = cookie[i+1] ;
    else
      len = 0 ;

/* Warn if the "length" takes us out of the cookie and truncate */
    if (len + i > cookielength) {
      if (verbose)
	fprintf(stderr, "TAG %d at %d.  len %d, overrun %d\n",
		cookie[i], i, len, i + len - cookielength) ;
      /* And truncate in any case even with no warning */
      len = cookielength - i ;
    }

    switch (cookie[i]) {  /* The monster switch statement ... */
/* PAD cookie */
    case TAG_PAD :
      i++ ;
      break ;

/* SUBNET we are in */
    case TAG_SUBNET_MASK :
      if (verbose && len != 4)
	fprintf(stderr, "WARNING len of tag 1 is %d not 4\n", len) ;
      memcpy((char *)&temp, cookie + i + 2, 4) ;
      printf("NETMASK=%s\n", inet_ntoa(temp)) ;

/* Both values are in network order so this doesn't care about the
   ordering 10/02/94 JSP */
      my_addr.s_addr &=  temp.s_addr ;
      printf("NETWORK=%s\n", inet_ntoa(my_addr)) ;
      my_addr.s_addr |= ~temp.s_addr ;
      printf("BROADCAST=%s\n", inet_ntoa(my_addr)) ;

/* defined so we know later that subnet info has been printed 11/02/94  JSP */
      subnet = 1 ;
      i += len + 2 ;
      break ;

/* Time of day */
    case TAG_TIME_OFFSET :
/* WANTS CHANGING TO BE PORTABLE ABOUT READING VALUE */
      printf("TIMEOFFSET=%li\n", ntohl(*(unsigned int *)&cookie[i+2]) ) ;
      i += len + 2 ;
      break ;

/* IP Gateways (routers) */
    case TAG_GATEWAY :
      PrintList("GATEWAYS", cookie, i+2, len) ;
      i += len + 2 ;
      break ; 

/* Timeservers (see RFC-868) */
    case TAG_TIME_SERVER :
      PrintList("TIMESRVS", cookie, i+2, len) ;
      i += len + 2 ;
      break ; 

/* IEN-116 Nameservers */
    case TAG_NAME_SERVER :
      PrintList("IEN116SRVS", cookie, i+2, len) ;
      i += len + 2 ;
      break ; 

/* DNS Nameservers */
    case TAG_DOMAIN_SERVER :
      PrintList("DNSSRVS", cookie, i+2, len) ;
      i += len + 2 ;
      break ; 

/* LOGGING servers */
    case TAG_LOG_SERVER :
      PrintList("LOGSRVS", cookie, i+2, len) ;
      i += len + 2 ;
      break ; 

/* Quote of day/Cookie servers */
    case TAG_COOKIE_SERVER :
      PrintList("QODSRVS", cookie, i+2, len) ;
      i += len + 2 ;
      break ; 

/* LPR servers */
    case TAG_LPR_SERVER :
      PrintList("LPRSRVS", cookie, i+2, len) ;
      i += len + 2 ;
      break ; 

/* Impress (Imogen) servers */
    case TAG_IMPRESS_SERVER :
      PrintList("IMPRESSSRVS", cookie, i+2, len) ;
      i += len + 2 ;
      break ; 

/* Remote Location Protocol servers */
    case TAG_RLP_SERVER :
      PrintList("RLPSRVS", cookie, i+2, len) ;
      i += len + 2 ;
      break ; 

/* HOSTNAME (may be fqdn or leaf) */
    case TAG_HOST_NAME :
      PrintString("HOSTNAME", cookie, i+2, len) ;
      i += len + 2 ;
      break ;

/* BOOT File Size (ignored) */
    case TAG_BOOT_SIZE :
/* WANTS CHANGING TO BE PORTABLE ABOUT READING VALUE */
      printf("BOOTSIZE=%i\n", ntohs(*(unsigned short int *)&cookie[i+2]) ) ;
      i += len + 2 ;
      break ;

/* Merit DUMP File name (ignored) */
    case TAG_DUMP_FILE :
      i += len + 2 ;
      break ;

/* DOMAIN */
    case TAG_DOMAIN_NAME :
      PrintString("DOMAIN", cookie, i+2, len) ;
      PrintSearch("SEARCH", cookie, i+2, len) ;
      i += len + 2 ;
      break ;

/* SWAPServer address */
    case TAG_SWAP_SERVER :
      PrintList("SWAPSRVR", cookie, i+2, len) ;
      i += len + 2 ;
      break ;

/* Root pathname to mount as root filesystem (ignored) */
    case TAG_ROOT_PATH :
      i += len + 2 ;
      break ;

/* Extensions.  Name of further Cookie data (ignored) */
    case TAG_EXTEN_FILE :
      i += len + 2 ;
      break ;

/* NIS (formerly YP) domain name */
    case TAG_NIS_DOMAIN :
      PrintString("YPDOMAIN", cookie, i+2, len) ;
      i += len + 2 ;
      break ;
       
/* NIS (formerly YP) server */
    case TAG_NIS_SERVER :
      PrintList("YPSRVR", cookie, i+2, len) ;
      i += len + 2 ;
      break ;
       
/* Time servers */
    case TAG_NTP_SERVER :
      PrintList("NTPSRVS", cookie, i+2, len) ;
      i += len + 2 ;
      break ;

/* END of cookie (phew) */
    case TAG_END :
      if (verbose)
	fprintf(stderr, "end of cookie parsing, END tag found\n") ;
      return ;

    default:
      { char name[30] ;
	if (verbose) {
	  if (tag >= 128 && tag <= 254) /* reserved */
	    fprintf(stderr, "Reserved TAG %d at %d (len %d)\n", tag, i, len) ;
	  else
	    fprintf(stderr, "Unknown TAG %d at %d (len %d)\n", tag, i, len) ;
	}
	sprintf(name, "T%3.3d", tag) ;
	PrintString(name, cookie, i+2, len) ;
	i += 2 + len ;
      }
      break ;
    }
  }
}


/* Print out a list of IP addresses */
void PrintList(char *name,
	       unsigned char *cookie,
	       int pos,
	       int len)
{
  struct in_addr temp ;

  if (verbose)
    fprintf(stderr, "%s found len=%d\n", name, len) ;

  if ((len % 4) != 0) {
    if (verbose)
      fprintf (stderr, "ERROR %s length (%d) not 4 div\n", name, len) ;
    return ;
  }
  if (len == 0) /* Nothing to do  10/02/94  JSP */
    return ;

  printf("%s='", name) ;
  for ( ; len; len -= 4, pos += 4) {
    memcpy((char *)&temp, cookie + pos, 4) ;
    printf("%s", inet_ntoa(temp)) ;  /* inet_ntoa() is safe to print I hope */
    if (len > 4)
      putchar(' ') ;
  }
  printf("'\n") ;
}

/* Prints the string passed */
void PrintString(char *name,
	       unsigned char *cookie,
	       int pos,
	       int len)
{
  printf("%s='", name) ;
  safeprint(cookie + pos, len);
  printf ("'\n") ;
}

/* Prints the string as usable in a DNS search.  This is doing the
   same as the old default BIND (pre 4.9.3) did with a DOMAIN line,
   for backwards compatibility, and since BOOTP doesn't allow a way to
   specify the search path explicitly */
void PrintSearch(char *name,
		 unsigned char *cookie,
		 int pos,
		 int len)
{
  unsigned char *ptr, *nptr ;
  unsigned char buf[258] ;  /* Max len is 255 */

  strncpy((char *)buf, (char *)(cookie + pos), len) ;
  buf[ len + 1 ] = 0 ;  /* Null terminate it */

  ptr = buf ;
  printf("%s='", name) ;
  while (len) {
    safeprint(ptr, len) ;
    /* Goto next bit */
    nptr = (unsigned char *)strchr((char *)ptr, '.') ;  /* Cast cast cast */
    if (nptr == NULL) {
      len = 0 ; /* End of string I hope */
    } else {
      if (strchr((char *)nptr + 1, '.') == NULL) {
	/* Trad to not use last component */
	len = 0 ;
      } else {
	len -= (nptr - ptr) + 1 ;
	ptr = nptr + 1 ;
	putchar(' ') ;
      }
    }
  }
  printf ("'\n") ;
}

/* Takes an address and returns useful bits of the name after lookup,
   this was a seperate program, but it is more compact to have both
   together.  17/02/94  JSP */

int in2host(char *address)
{
  struct in_addr sin_addr;
  struct hostent *hp;
  char *c ;

/* convert to standard network form */
  sin_addr.s_addr = inet_addr(address);

/* perform lookup, must have DNS running or have local hosts file at
   this point */

  hp = gethostbyaddr((char *)&sin_addr, sizeof(sin_addr), AF_INET) ;

  if (hp == NULL) {
    perror ("bootpc: gethostbyaddr") ;
    return -1;
  }

/* Print out a known name to stop repeated calls */
  printf("DONEIN2HOST=1\n") ;

/* Print out full name as returned by the call */
  PrintString("HOSTFULL", (unsigned char *)(hp->h_name),
	      0, strlen(hp->h_name)) ;

  for(c=(char *)hp->h_name; *c ; ++c)
    if(*c == '.') {
/* Zap first 'dot' to give leaf and domain names */
      PrintString("HOSTDOMAIN", (unsigned char *)(c+1), 0, strlen(c+1)) ;
      PrintSearch("HOSTSEARCH", (unsigned char *)(c+1), 0, strlen(c+1)) ;
      *c = 0 ;
      PrintString("HOSTLEAF", (unsigned char *)(hp->h_name),
		  0, strlen(hp->h_name)) ;
      return 0 ;
    }
  return 0 ;
}

/* I think we want to remove the ping code completely since it's value is "LOW" */

/* This is lifted from the BSD ping.c */

/*
 * in_cksum --
 *	Checksum routine for Internet Protocol family headers (C Version)
 */
u_short in_cksum(addr, len)
	u_short *addr;
	int len;
{
	register int nleft = len;
	register u_short *w = addr;
	register int32 sum = 0;
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

#define icmp_type type
#define icmp_code code
#define icmp_cksum checksum
#define icmp_id un.echo.id
#define icmp_seq un.echo.sequence
#define icmp_gwaddr un.gateway
#define ip_hl ihl
#define ip_v version
#define ip_tos tos
#define ip_len tot_len
#define ip_id id
#define ip_off frag_off
#define ip_ttl ttl
#define ip_p protocol
#define ip_sum check
#define ip_src saddr
#define ip_dst daddr

#define ICMP_MINLEN	32
#define	DEFDATALEN	(64 - 8)	/* default data length */
#define	MAXIPLEN	60
#define	MAXICMPLEN	76
#define	MAXPACKET	(65536 - 60 - 8)/* max packet size */
#define	MAXWAIT		10		/* max seconds to wait for response */
#define	NROUTES		9		/* number of record route slots */


/*
   Takes an address and sends an ICMP request to it.  If we get a reply then
   this is not a good address to use.  If we don't get a reply then hopefully
   we are ok.  This is a very simple version of ping not suitable for
   industrial use.  This code is basically lifted out of the BSD ping client
   but let me know if it fails.
*/

#define PRETRIES     (6)
#define TIMEOUTSEC   (0)
#define TIMEOUTUSEC  (200000)

/*
   We send 6 Pings out waiting a total of 1.2 seconds (0.2 sec each),
   if you have a very lossy network, (say 10%), then the chances of
   this failing (assuming a losses are at random, which isn't fair)
   are "a million to one".  But it might just happen!
*/

int pingit(char *address)
{
  struct sockaddr whereto;	/* who to ping */
  struct sockaddr_in *to ;
  struct protoent *proto;
  char packet[BUFSIZ] ;         /* packet we will send */
  int sockfd, ident, datalen = DEFDATALEN ;
  struct icmphdr *icp;
  int cc, i, count = PRETRIES ;
  fd_set rfds, wfds, xfds;
  struct timeval timeout ;
  struct iphdr *ip;

  memset((char *)&whereto, 0, sizeof(struct sockaddr));
  to = (struct sockaddr_in *)&whereto;
  to->sin_family = AF_INET;
  to->sin_addr.s_addr = inet_addr(address);

  ident = getpid() & 0xFFFF;

  if (!(proto = getprotobyname("icmp"))) {
    (void)fprintf(stderr, "bootpc: unknown protocol icmp.\n");
    FatalError();
  }
  if ((sockfd = socket(AF_INET, SOCK_RAW, proto->p_proto)) < 0) {
    perror("bootpc: socket");
    FatalError();
  }

  while (count--) {
    if (debug)
      printf("bootpc: Ping %s count = %d\n", address, count) ;
    icp = (struct icmphdr *)packet;
    icp->icmp_type = ICMP_ECHO;
    icp->icmp_code = 0;
    icp->icmp_cksum = 0;
    icp->icmp_seq = count;
    icp->icmp_id = ident;			/* ID */

    cc = datalen + 8;			/* skips ICMP portion */
    /* compute ICMP checksum here */
    icp->icmp_cksum = in_cksum((u_short *)icp, cc);

    i = sendto(sockfd, packet, cc, 0, &whereto, sizeof(struct sockaddr));

    if (i < 0 || i != cc)  {
      if (i < 0)
	perror("bootpc: sendto");
      (void)printf("bootpc: wrote %s %d chars, ret=%d\n",
		   address, cc, i);
    }

    FD_SET(sockfd,&rfds);
    FD_ZERO(&wfds);
    FD_ZERO(&xfds);
    timeout.tv_sec =  TIMEOUTSEC ;
    timeout.tv_usec = TIMEOUTUSEC ;
  
    if(select(sockfd+1, &rfds, &wfds, &xfds, &timeout)<0) {
      perror("bootpc: select");
      FatalError();
    }

    if(!FD_ISSET(sockfd, &rfds)) {
      continue ;
    } else {
      if (recvfrom(sockfd, (char *)packet, datalen + MAXIPLEN + MAXICMPLEN, 0,
		   (struct sockaddr *)NULL, (int *)NULL) < 0){
	if (errno != EINTR)
	  perror("bootpc: recvfrom");
	continue ;
      }
      ip = (struct iphdr *)packet;
      icp = (struct icmphdr *)(packet + (ip->ip_hl <<2));
      if (icp->icmp_type == ICMP_ECHOREPLY) {  /* a reply */
	if (icp->icmp_id == ident)  /* OUR reply */
	  if (verbose)
	    printf("%s is alive\n", address) ;
	  return 0 ;
      }
    }
  }
  if (verbose)
    printf("No reply from %s\n", address) ;
  return 1 ;
}


/* print out those bits of a string which are alphanumeric or in a
   "safe" list of characters. */
void safeprint(unsigned char *string,
	       int len)
{
  char safe[] = "./:-_=+[]~()%&*^#@! " ;
  int i, c ;

  for (i =0 ; i < len; ++i) {
    c = string[i] ;
    if (isalnum(c))
      putchar(c) ;   /* alphanumeric */
    else {  /* Not alphanumeric */
      if (strchr(safe, c) != NULL) {
	putchar(c) ; /* but safe */
      } else {
	putchar('?') ; /* NOT safe */
	if (verbose)
	  fprintf(stderr, "Illegal char 0x%2.2X\n", c) ;
      }
    }
  }
}
