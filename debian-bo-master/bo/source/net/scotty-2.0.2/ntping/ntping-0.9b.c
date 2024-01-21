/*
 * ntping.c:						Jan 1993
 * (schoenfr@ibr.cs.tu-bs.de)
 *
 * Copyright (c) 1993, 1994 by Erik Schoenfelder and Juergen Schoenwaelder 
 * TU Braunschweig, Germany, Institute for Operating Systems 
 * and Computer Networks.
 *
 * This is ping/traceroute for tkined (with scotty and tcl).
 *
 * XXX: this is ntping-0.9b.c this version is the latest known-to-work
 * XXX: version, but it uses static buffer space and behaves bad when
 * XXX: using large delays.
 * XXX: but you may replace ntping.c by this version, if you have 
 * XXX: problems. 
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#include <signal.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#ifdef HAVE_SYS_SELECT_H
# include <sys/select.h>
#endif
#include <errno.h>
#if defined(pyr) || (defined(MACH) && defined (MTXINU))
/* should make into the configure script... */
extern int errno;
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifdef DB_MALLOC
# include <dbmalloc.h>
#endif

#ifdef __alpha
/* 
 * thanks to <mosedale@genome.stanford.edu> Dan Mosedale for the DEC
 * Alpha patches and <grunwald@foobar.cs.colorado.edu> Dirk Grunwald 
 * for his help.
 */
typedef unsigned int ipaddr_t;
#else
typedef unsigned long ipaddr_t;
#endif
typedef unsigned int int32;


/* aix failes to use IP_TTL correct: */
#ifndef _AIX
#if defined(IP_TTL) && ! defined(USE_DLPI)
/*
 * set this, if we will not send our own handmade ip (udp) packets.
 * so we open a ordinariy udp socket and set the ttl via
 * setsockopt with IP_TTL.
 * This is used for HP-UX and SVR4-boxes.
 */
#define USE_DLPI
#endif /* IP_TTL && ! USE_DLPI */
#endif /* ! aix */

#ifdef linux
/*
 * this is for linux around 0.99.15 and above:
 */
struct ip { u_char ip_hl:4, ip_v:4; u_char ip_tos; short ip_len; 
	    u_short ip_id; short ip_off; u_char ip_ttl; u_char ip_p; 
	    u_short ip_sum; struct in_addr ip_src,ip_dst;
};
struct icmp { u_char  icmp_type; u_char icmp_code; u_short icmp_cksum;
	      u_short icmp_id, icmp_seq; char icmp_data [1];
};
struct udphdr { u_short uh_sport, uh_dport; short uh_ulen; u_short uh_sum; };

#include <linux/wait.h>

#define ICMP_MINLEN     	8
#define ICMP_ECHO       	8
#define ICMP_ECHOREPLY       	0
#define ICMP_UNREACH    	3
#define ICMP_SOURCEQUENCH	4
#define ICMP_TIMXCEED   	11
#define ICMP_TSTAMP		13
#define ICMP_TSTAMPREPLY	14
#define ICMP_MASKREQ    	17
#define ICMP_MASKREPLY  	18
#define ICMP_UNREACH_PORT	3
#define ICMP_TIMXCEED_INTRANS	0
#else /* ! linux */
/*
 * headers for our sun (and mostly 4.2 - i guess):
 */
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <netinet/udp.h>
#endif /* ! linux */
#include <netdb.h>

#define PROGNAME	"ntping"		/* my name */

static char *version = "ntping  v0.9b  Mar 1995";

/* length of the data space of a ping packet: */
#define DEFAULT_DATALEN		44

/* seems to work for most systems: */
#define MAX_DATALEN		2020

/* default base port for ttl probes: */
#define BASE_PORT		50000
#define MAX_BASE_PORT		60000

/* default number of retries: */
#define DEFAULT_RETRIES		5

/* timeout in s before giving up: */
#define DEFAULT_TIMEOUT		5

/* ident for the ping packets: */
static unsigned short icmp_ident;

/* emit debug messages: */
static int do_debug = 0;

/* number of retries: */
static int retries = DEFAULT_RETRIES;

/* default waittime before giving up: */
static int timeout = DEFAULT_TIMEOUT;

/* time in ms to wait between retries: */
static int wait_time = (1000 * DEFAULT_TIMEOUT) / (DEFAULT_RETRIES + 1);

/* the sockets: */
static int icsock = -1;
static int ipsock = -1;

/* datalen of the ping-pkt: */
static int data_len = DEFAULT_DATALEN;

/* base port for ttl probes: */
static int base_port = BASE_PORT;

#ifdef USE_DLPI
/* port # our udp-socket is bound to: */
static unsigned short src_port;
/* second ip-socket: */
static int ipsock2 = -1;
#endif

/* read hostnames from stdin: */
static int interactive = 0;

/* run in bones mode: */
static int bones = 0;

/* do a traceroute: */
static int do_ttl = 0;

/* do a icmp mask-request: */
static int do_mask = 0;

/* do a timestamp request: */
static int do_tstamp = 0;

/* reply the string given for the trace-target, if target reached: */
static int reply_same = 0;

/* milli-seconds to sleep if we have sent a packet: */
static int delay_time = 0;			/* default: don't sleep */

/* sigh - check for bytesex in unreach/exceed icmp's too. 
   the synoptics are dumb enough to reply wrong packets: */
static int check_sex = 1;
/* emit a warning, if a byte-swapped port in a reply is found: */
static int warn_swapped = 1;

/* bones argument string: */
static char bbuf [20480];
static int blen = 0;

/*
 * the use of these fileds is somewhat confusing. some are used for
 * ping's others for ttl-probes. this needs surely a clean-up.
 */
typedef struct _a_job {
  	char tag;			/* 'p'ing or 't'tl ? */
	int done;			/* this job is done */
	int job_no;			/* sequence number */
	int ttl;			/* hops for traceroute */
	int tdone;			/* was it the final hop ? */
	unsigned short port;		/* dest port for traceroute */
	ipaddr_t mask;			/* mask/tstamp return val (ugly) */
	unsigned short id;		/* unique icmp id */
	char *hname;			/* hostname */
	struct sockaddr_in sa;		/* hosts ip-address */
	struct sockaddr_in hop;		/* hop's ip-address */
	int tim;			/* time is ms */
	int cur_seq;			/* next seq-num to sent */
	struct timeval tv;		/* time ttl probe sent. */
	struct _a_job *next;
} a_job;

static a_job *jobs = 0;

/*
 * udp ttl packet:
 */
typedef struct _a_pkt {
#ifndef USE_DLPI
    /* 
     * we are sending simple udp packets; no additional headers are
     * needed.
     */
    struct ip ip;
    struct udphdr udph;
#endif
    /* arg: no additional bytes are replied icmp - this is nonsense. */
    char seq;
    char ttl;
    struct timeval tv;
    char buf [1];
} a_pkt;


/*
 * save-malloc: aborts on error.
 */

#define TALLOC(T)	((T *) xmalloc (sizeof (T)))
#define xstrdup(s)	strcpy (xmalloc (strlen (s) + 1), s)
#define xfree(s)	free ((char *) s)

static char *
xmalloc (n)
int n;
{
	char *nnew;
	int i;
	
	for (i = 0; ! (nnew = malloc ((unsigned) n)) && i < 10; i++)
		sleep (6);
	if (! nnew)
	{
		fprintf (stderr, "ntping: out of mem... aborting.\n");
		exit (1);
	}
	return nnew;
}

static char *
xrealloc (p, n)
char *p;
int n;
{
	char *nnew;
	int i;

	for (i = 0; ! (nnew = realloc (p, (unsigned) n)) && i < 10; i++)
		sleep (6);
	if (! nnew)
	{
		fprintf (stderr, "ntping: out of mem... aborting.\n");
		exit (1);
	}
	return nnew;
}


#ifdef HAVE_USLEEP
/* avoid clash, if defined and in libc: */
#define m_usleep usleep
#else /* HAVE_USLEEP */
/*
 * fake an easy usleep() using select on an emty filedescriptor-set:
 */

static void
m_usleep (n)
unsigned n;
{
	struct timeval tv;
	tv.tv_sec = n / 1000000;
	tv.tv_usec = n % 1000000;

	if (select (1, 0, 0, 0, &tv) < 0)
		if (do_debug)
                        perror ("ntping: m_usleep: select failed; reason");
}

#endif /* HAVE_USLEEP */


/*
 * have a delay of ``n'' milliseconds. i'll assume a usleep (or select
 * or homebrewed usleep) will at least take 20 milliseconds. on a ss2
 * usleep (1) taks about 20ms :-( - wait active for a smaller delay.  
 *
 * how to have a short delay (1 to 20 ms ?):
 * 1. loop with gettimeofday's() and compare the wasted time.
 * 2. calc the delay of a empty loop and use this to waste time.
 *
 * both is ugly, but what should i do ?
 */

/*
 * DELAY_WITH_GETTIMEOFDAY:
 * if defined: wait active in a loop of gettimeofday's
 * if undef'd: wait active in an empty loop
 */

/* 
 * alpha boxes are too fast for the empty loop; they end up getting
 * divide by 0 errors while attempting to time stuff. 
 */

#ifdef __alpha
#define DELAY_WITH_GETTIMEOFDAY
#else
#undef DELAY_WITH_GETTIMEOFDAY
#endif

static void
have_delay (n)
int n;
{
  static long bogo = -1;
  static long cnt = 0;
  struct timeval tv1, tv2;

  if (n > 20)
  {
	  /* micro-secs to usleep: */
	  m_usleep ((unsigned) (n * 1000));
	  return;
   }
  
#ifndef DELAY_WITH_GETTIMEOFDAY
  /*
   * guess the loop-counter for a short interval:
   * (recalculate every 100 calls)
   */

  if (bogo < 0 || ! (cnt % 100))
  {
	  int i, t, j;

	  if (gettimeofday (&tv1, (struct timezone *) 0) < 0)
		  if (do_debug)
			  perror ("ntping: cannot call gettimeofday; reason");
	  /* if the scheduler hits us in this loop, 
	   * it will be a longer delay... */
	  for (i = 0, j = 10000; i < j; i++)
		  continue;
	  if (gettimeofday (&tv2, (struct timezone *) 0) < 0)
		  if (do_debug)
			  perror ("ntping: cannot call gettimeofday; reason");
	  t = (tv2.tv_sec - tv1.tv_sec) * 1000000;
	  t += tv2.tv_usec - tv1.tv_usec;

	  /* check about backward running clocks: */
	  if (t < 0)
	    {
	      fprintf (stderr,
      "ntping: warning: oops in have_delay: backward running clock ?\n");
	      t = 1;
	    }

	  /*
	   * check about faulty gettimeofday() routine:
	   * (will take some years, if computers can make this loop in
	   * less than 1 micro-second)
	   */
	  if (! t)
	    t = 1;

	  bogo = 10000 * 1000 / t;	/* bogos for ~ 1 ms */
	  if (do_debug)
		  fprintf (stderr, "* got %ld bogos.\n", bogo);
  }
  cnt++;

  /* now waste the time with active wating: */
  {
    long i, j;
    /* loop a little around (~ n - 1 ms): */
    for (i = 0, j = (n - 1) * bogo; i < j; i++)
      continue;
  }

  return;

#else /* DELAY_WITH_GETTIMEOFDAY */

  if (gettimeofday (&tv1, (struct timezone *) 0) < 0)
	  if (do_debug)
		  perror ("ntping: cannot call gettimeofday; reason");
  tv1.tv_usec += n * 1000;
  tv1.tv_sec += tv1.tv_usec / 1000000;
  tv1.tv_usec = tv1.tv_usec % 1000000;

  do {
	  /* look if we have to run around anymore: */
	  if (gettimeofday (&tv2, (struct timezone *) 0) < 0)
		  if (do_debug)
			  perror ("ntping: cannot call gettimeofday; reason");
  } while (tv1.tv_sec > tv2.tv_sec 
	   || (tv1.tv_sec == tv2.tv_sec && tv1.tv_usec > tv2.tv_usec));

  return;

#endif /* DELAY_WITH_GETTIMEOFDAY */
}


/*
 * do we have a digit here ?
 */
static int
mis_dig (c)
int c;
{
	return c >= '0' && c <= '9';
}

/* 
 * make a.b.c.d string:
 */

static char *
my_inaddr (addr)
struct sockaddr_in *addr;
{
	static char buf [99];
	ipaddr_t x = ntohl (addr->sin_addr.s_addr);
	sprintf (buf, "%d.%d.%d.%d", (int) (x >> 24) & 0xff, 
		 (int) (x >> 16) & 0xff,
		 (int) (x >> 8) & 0xff, (int) x & 0xff);
	return buf;
}


/*
 * return 1, if fd is avail for reading; time to wait `wtim' in ms:
 */

static int
fd_ready (fd, wtim)
int fd, wtim;
{
	fd_set fds;
	struct timeval tv;
	int rc;

	FD_ZERO (&fds);
	FD_SET (fd, &fds);
	
	tv.tv_usec = (wtim * 1000) % 1000000;
	tv.tv_sec = (wtim * 1000) / 1000000;

	if ((rc = select (32, &fds, (fd_set *) 0, (fd_set *) 0, &tv)) < 0)
	  {
	    if (errno != EINTR)
	      {
	      perror ("ntping: select failed; reason");
	      exit (1);
	    }
	    return 0;
	  }
	
	return rc > 0;
}


/* swap a short: */
static int sexy (x)
unsigned x;
{
	return (x & 0xff) << 8 | (x & 0xff00) >> 8;
}


/*
 * search for a specific job.
 * the id is either the src-port of our own udp-packet (== job->id) or
 * for the dlpi udp-packet the static src_port.
 */

static a_job *
find_job_port (ip, udph)
struct ip *ip;
struct udphdr *udph;
{
    unsigned short id = ntohs (udph->uh_sport);
    unsigned short port = ntohs (udph->uh_dport);
    a_job *job;
    
    if (do_debug)
      fprintf (stderr, "* looking for src %u (0x%lx)  dest %u (0x%lx) ...", 
	       (unsigned) id, (int) id, (unsigned) port, (int) port);

    for (job = jobs; job; job = job->next)
      {
	  unsigned short src;
	  int got_it = 0;

#ifndef USE_DLPI
	  src = job->id;
#else
	  src = src_port;
#endif

	  if (do_debug)
#ifndef USE_DLPI
	    fprintf (stderr, " %u", src);
#else
	    fprintf (stderr, "  %u (0x%lx)", (unsigned) job->port, 
		     (int) job->port);
#endif
	  
	  if (job->port == port && src == id)
	    got_it = 1;
	  
	  if (! got_it && check_sex)
	    {
		if ((job->port == sexy (port) && src == sexy (id))
		    || (job->port == port && src == sexy (id))
		    || (job->port == sexy (port) && src == id))
		  {
		      got_it = 1;
		      if (warn_swapped)
			fprintf (stderr,
"ntping: warning: got icmp-reply from 0x%08lx with byte-swapped port in the reply\n", 
				 ntohl (ip->ip_src.s_addr));
		  }
	    }
	  
	  if (got_it)
	    {
		if (do_debug)
		  fprintf (stderr, " got it.\n");
		return job;
	    }
      }

    if (do_debug)
      fprintf (stderr, " nope...\n");

    return (a_job *) 0;
}


static a_job *
find_job_id (id)
unsigned short id;
{
	a_job *job;

	if (do_debug)
		fprintf (stderr, "* looking for job id %u ...", (unsigned) id);

	for (job = jobs; job; job = job->next)
	{
		if (do_debug)
			fprintf (stderr, " %u", (unsigned) job->id);
		if (job->id == id)
		{
			if (do_debug)
				fprintf (stderr, " got it.\n");
			return job;
		}
	}

	if (do_debug)
		fprintf (stderr, " nope...\n");
	return (a_job *) 0;
}


/*
 * return 0 if no more jobs are waiting, or 1 else.
 */

static int
jobs_to_go ()
{
    a_job *j;

    for (j = jobs; j; j = j->next)
      if (! j->done)
	return 1;
    return 0;
}


/*
 * fill in sockaddr_in from given hname (decimal or name);
 * return 0 on error.
 */

static int
make_addr (addr, hname)
struct sockaddr_in *addr;
char *hname;
{
	memset ((char *) addr, 0, sizeof (struct sockaddr_in));
	if (*hname >= '0' && *hname  <= '9')
	{
		int a, b, c, d;
		ipaddr_t naddr;
		
		if (4 == sscanf (hname, "%d.%d.%d.%d", &a, &b, &c, &d))
		{
			naddr = a << 24 | b << 16 | c << 8 | d;
			naddr = ntohl (naddr);
			/** XXX hack alert - but what the heck ;-) **/
			addr->sin_family = AF_INET;
			addr->sin_addr.s_addr = naddr;
			return 1;
		}
	}
	else {
		struct hostent *hp = gethostbyname (hname);
		if (hp)
		{
			addr->sin_family = hp->h_addrtype;
			memcpy ((char *) &(addr->sin_addr), (char *) hp->h_addr,
			       hp->h_length);
			return 1;
		}
	}
	
	if (do_debug)
		fprintf (stderr, "ntping: cannot resolve `%s'\n", hname);
	
	return 0;
}



/*
 * beeeep - an icmp packet arrived; fetch all in the queue.
 */

static void
do_receive (stime)
int stime;
{
    char packet [MAX_DATALEN + 128];
    int len = sizeof (packet);
    struct ip *ip = (struct ip *) packet;
    struct icmp *icp;		/* for pings */
    struct udphdr *udph;		/* for ttl's */
    struct sockaddr_in sfrom;
    struct timeval tp1, tp2;
    int fromlen = sizeof (sfrom);
    int hlen = 0, is_ttl = 0, is_mask, is_tstamp, cc, rc, ttl_is_done;
    int do_continue;
    ipaddr_t val = 0;
    a_job *job = 0;
    
    for (;;) 
      {
	  /* nothing more to get: */
	  if (! jobs_to_go ())
	    return;

	  rc = fd_ready (icsock, stime);		
	  if (! rc)
	    return;
	  
	  cc = recvfrom (icsock, (char *) packet, len, 0,
			 (struct sockaddr *) &sfrom, &fromlen);
	  
	  if (gettimeofday (&tp2, (struct timezone *) 0) == -1) 
	    {
		if (do_debug)
		  perror ("ntping: gettimeofday");
		return;
	    }
	  
	  if (do_debug)
	    printf ("... recv got: rc %d\n", cc);
	  
	  if (cc < 0 && errno != EINTR)
	    {
		if (do_debug)
		  perror ("ntping: warning: recvfrom");
		return;
	    }
	  
	  if (cc == 0)
	    return;
	  
	  /* how to play this game correct ???
	   * raw-socket with icmp-protocol should send and receive
	   * ip packets with complete header:
	   */
#ifdef old_and_broken_linux
	  /* still not correct in net-2e-alpha-2: ... */
	  /* and in net-2e-alpha-3 too :-( */
#define IP_VERS		4
#define IP_PKTLEN	(sizeof (struct ip) / sizeof (int))
	  
	  if (ip->ip_v != IP_VERS && ip->ip_hl != IP_PKTLEN)
	    {
		/* seems to be a missing ip-header: */
		if (do_debug)
		  fprintf (stderr, 
			   "* linux: missing ip-header ...\n");
	    }
	  else
	    /* in linux 0.99.15a this is fixed - hurray ! */
#else
	    hlen = ip->ip_hl << 2;
#endif
	  if (cc < hlen + ICMP_MINLEN)
	    continue;
	  
	  icp = (struct icmp *) (packet + hlen);
	  
	  udph = (struct udphdr *) (icp->icmp_data + sizeof (struct ip));
	  
	  if (do_debug)
	    fprintf (stderr,
		     "... got icmp type %d  code %d  id %u (0x%x)\n", 
		     icp->icmp_type, icp->icmp_code,
		     (unsigned) icp->icmp_id,
		     (unsigned) icp->icmp_id);
	  
	  do_continue = is_tstamp = is_mask = ttl_is_done = 0;
	  
	  switch (icp->icmp_type) {
	    case ICMP_ECHOREPLY:
	      if (do_debug)
		fprintf (stderr, "* got ECHOREPLY.\n");
	      is_ttl = 0;
	      break;
	    case ICMP_MASKREPLY:
	      if (do_debug)
		fprintf (stderr, "* got MASKREPLY.\n");
	      is_ttl = 0, is_mask = 1;
	      break;
	    case ICMP_TSTAMPREPLY:
	      if (do_debug)
		fprintf (stderr, "* got TSTAMPREPLY.\n");
	      is_ttl = is_mask = 0; is_tstamp = 1;
	      break;
	    case ICMP_UNREACH:
	      if (do_debug)
		fprintf (stderr, "* got UNREACHABLE.\n");
	      if (icp->icmp_code != ICMP_UNREACH_PORT)
		{
		    if (do_debug)
		      fprintf (stderr,
			       "* bad code - discarded.\n");
		    do_continue = 1;
		}
	      is_ttl = 1;
	      ttl_is_done = 1;
	      break;
	    case ICMP_TIMXCEED:
	      if (do_debug)
		fprintf (stderr, "* got TIMXCEED_INTRANS.\n");
	      if (icp->icmp_code != ICMP_TIMXCEED_INTRANS)
		{
		    if (do_debug)
		      fprintf (stderr,
			       "* bad code - discarded.\n");
		    do_continue = 1;
		}
	      is_ttl = 1;
	      break;
	    case ICMP_SOURCEQUENCH:
	      { struct ip *ip = (struct ip *) ((char *) icp + 4);
		if (do_debug)
		  fprintf (stderr, 
			   "* got icmp sourcequench from 0x%lx for 0x%lx\n",
			   (long) ip->ip_dst.s_addr,
			   (long) ip->ip_src.s_addr);
	    }
	      /* fall through */
	    default:
	      if (do_debug)
		fprintf (stderr, "* packet discarded.\n");
	      do_continue = 1;
	  }
	  
	  if (do_continue == 1)
	    continue;
	  
	  /* XXX: oops bert nase: why this ? */
	  if (rc <= 0)
	    rc = 1;
	  
	  if ((! is_ttl && ! (job = find_job_id (icp->icmp_id)))
	      || (is_ttl && ! (job = find_job_port (ip, udph))))
	    continue;
	  
	  /* this one is still done: */
	  if (job->done)
	    {
		if (do_debug)
		  fprintf (stderr, "* still done :-)\n");
		continue;
	    }
	  
	  if (is_ttl != (job->tag == 't'))
	    fprintf (stderr, "oops - ping or ttl ? (tell nase)\n");
	  
	  if (! is_ttl)
	    {
		if (is_mask)
		  {
		      val = ntohl (* (ipaddr_t *) icp->icmp_data);
		      job->mask = val;
		      tp1 = job->tv;
		  }
		else if (is_tstamp)
		  {
		      if (do_debug) {
			  fprintf (stderr,
				   "** tstamp: got %d, %d, %d\n",
			           ntohl (((int32 *) icp->icmp_data) [0]),
			           ntohl (((int32 *) icp->icmp_data) [1]),
			           ntohl (((int32 *) icp->icmp_data) [2]));
			  fprintf (stderr,
				   "** tstamp: diff is %d\n",
			           ntohl (((int32 *) icp->icmp_data) [1]) -
			           ntohl (((int32 *) icp->icmp_data) [0]));
		      }
		      
		      val = ntohl (((int32 *) icp->icmp_data) [1]) -
			ntohl (((int32 *) icp->icmp_data) [0]);
		      
		      job->mask = val;
		      tp1 = job->tv;
		  }
		else
		  memcpy ((char *) &tp1, (char *) icp->icmp_data,
			  sizeof (struct timeval)); 
	    }
	  else {
	      tp1 = job->tv;
	      job->tdone = ttl_is_done;
	  }
	  
	  rc = (tp2.tv_sec - tp1.tv_sec) * 1000 
	    + (tp2.tv_usec - tp1.tv_usec) / 1000;
	  
	  job->tim = (rc <= 0) ? 1 : rc;
	  job->done = 1;
	  job->hop = sfrom;
	  
	  if (do_debug)
	    {
		fprintf (stderr, "* .. got host %s (with tim %d)\n", 
			 job->hname, job->tim);
		if (is_mask)
		  fprintf (stderr, "* .. and mask 0x%lx\n",
			   (long) val);
		if (is_tstamp)
		  fprintf (stderr, "* .. and tstamp %ld\n",
			   (long) val);
		if (job->tag == 't')
		  {
		      ipaddr_t l = sfrom.sin_addr.s_addr;
		      fprintf (stderr, "* ttl hop is %s (0x%lx)\n", 
			       my_inaddr (&sfrom), (long) l);
		  }
	    }
      }
    /* not reached */
}


/* 
 * make checksum:
 */

static int 
in_cksum (buf, n)
unsigned short *buf;
int n;
{
	int sum = 0, nleft = n;
	unsigned short ret, *ptr = buf, odd_byte = 0;

	while (nleft > 1)
		sum += *ptr++, nleft -= 2;

	if (nleft == 1)
		*(unsigned char *) (&odd_byte) = *(unsigned char *) ptr,
		sum += odd_byte;

	sum = (sum >> 16) + (sum & 0xffff);
	sum += (sum >> 16);
	ret = ~sum;

	return ret;
}


/*
 * send a ttl probe pkt:
 */

static void
send_ttl (job)
a_job *job;
{
	char *datap, outpack [2048];
	a_pkt *pkt = (a_pkt *) outpack;
#ifndef USE_DLPI
	struct ip *ip = &pkt->ip;
	struct udphdr *udph = &pkt->udph;
#endif /* ! USE_DLPI */
	struct sockaddr_in *sto = &job->sa;
	int i, j;
	
	if (job->done)
	    return;

#ifndef USE_DLPI
	ip->ip_hl = sizeof (struct ip) / sizeof (int32);
	ip->ip_v = 4;			/* take this - live and in color */
	ip->ip_tos = 0;
	ip->ip_id = job->id;		/* ??? */
	ip->ip_sum = 0;
	/* fixed by Jan L. Peterson (jlp@math.byu.edu): */
        ip->ip_src.s_addr = 0;          /* jlp */

	ip->ip_off = 0;
	ip->ip_p = IPPROTO_UDP;
	ip->ip_len = data_len;
	ip->ip_ttl = job->ttl;
	ip->ip_dst = sto->sin_addr;	       /* needed for linux (no bind) */

	udph->uh_sport = htons (job->id);
#if 1
	/* job->port is set to (base_port + ttl) - same pkt for retries: */
	udph->uh_dport = htons (job->port);
#else
	udph->uh_dport = htons (job->port + job->cur_seq); /* karl +job_no */
#endif
	udph->uh_ulen = htons ((u_short) (data_len - sizeof (struct ip)));
	udph->uh_sum = 0;

#else /* USE_DLPI */
	sto->sin_port = htons (job->port);
#endif /* USE_DLPI */

	pkt->seq = job->cur_seq++;
	pkt->ttl = job->ttl;	
	if (gettimeofday (&pkt->tv, (struct timezone *) 0) == -1) 
	{
	    if (do_debug)
		perror ("ntping: gettimeofday");
	    return;
	}
	job->tv = pkt->tv;

	datap = pkt->buf;
	for (i = sizeof (struct timeval) + 2, j = 'A'; i < data_len; i++, j++)
		datap [i] = j;

/*
 * may be simply: #ifdef IP_TTL  (eventually wrapped around a USE_DLPI) ?
 */
#ifdef USE_DLPI
	{ int opt_ttl = job->ttl;
	  int opt_ttl_len = sizeof (opt_ttl);

	  if (setsockopt(ipsock, IPPROTO_IP, IP_TTL, 
			 &opt_ttl, opt_ttl_len) < 0)
	      perror ("ntping: setsockopt: cannot set ttl; reason");

	  if (getsockopt (ipsock, IPPROTO_IP, IP_TTL,
			  &opt_ttl, &opt_ttl_len) < 0)
	      perror ("ntping: getsockopt: ttl not set; reason");
	  else if (job->ttl != opt_ttl)
	      fprintf (stderr, "ntping: dlpi: cannot set ttl - ouch...\n");
        }
#endif

	/* fprintf(stderr, "data_len %d\n", data_len); */	/* karl */
	i = sendto (ipsock, (char *) outpack, data_len, 0, 
		    (struct sockaddr *) sto, sizeof (struct sockaddr_in));
	
	if (i < 0 || i != (data_len))  
	  {
	    if (do_debug)
	      perror ("ntping: sendto");
	  }
	else if (do_debug)
	  fprintf (stderr, "* ttl %d sent to %s  port %u (0x%x)...\n",
		   job->ttl, job->hname, (unsigned) job->port,
		   (unsigned) job->port);
}




/*
 * guess the pingtime:
 */

static void
send_ping (job)
a_job *job;
{
	char outpack [2048];
	struct icmp *icp = (struct icmp *) outpack;
	char *datap = icp->icmp_data;
	int i, data_offset;
	struct sockaddr_in *sto = &job->sa;
	struct timeval tv;

	if (job->done)
		return;
	
	icp->icmp_type = (do_mask) ? ICMP_MASKREQ : 
	  		((do_tstamp) ? ICMP_TSTAMP : ICMP_ECHO);
	icp->icmp_code = 0;
	icp->icmp_cksum = 0;
	icp->icmp_seq = job->cur_seq++;
	icp->icmp_id = job->id;

	if (! do_mask)
	  {
	    if (gettimeofday (&tv, (struct timezone *) 0) == -1)
	      {
		if (do_debug)perror ("ntping: gettimeofday");
		return;
	      }
	  }
	
	if (do_tstamp)
	  {
	    * (int32 *) datap = htonl ((tv.tv_sec % 86400) * 1000
					       + (tv.tv_usec / 1000));
	    data_offset = sizeof (int32);
	  }
	else if (do_mask)
	    data_offset = 0;
	else 
	  {    /* ping: */
	    * (struct timeval *) datap = tv;
	    data_offset = sizeof (struct timeval);
	  }
	
	for (i = data_offset; i < data_len; i++)
		datap [i] = i;
	
	/* icmp checksum: */
	icp->icmp_cksum = in_cksum ((unsigned short *) icp, data_len);

	i = sendto (icsock, (char *) outpack, data_len, 0, 
		    (struct sockaddr *) sto, sizeof (struct sockaddr_in));
	
	if (i < 0 || i != data_len)  
	  {
	    if (do_debug)
	      perror ("ntping: sendto");
	  }
	else if (do_debug)
	  fprintf (stderr, "* %s ping %d sent to %s...\n",
		   (do_mask) ? "mask" : 
		   ((do_tstamp) ? "tstamp" : "regular"),
		   job->cur_seq - 1, job->hname);
	
	/* lets wait a little time: */
	if (delay_time)
	  have_delay (delay_time);
}


static void
send_pending ()
{
	a_job *job;

	for (job = jobs; job; job = job->next)
	{
		if (! job->done && job->cur_seq <= retries + 1)
		  {
			if (job->tag == 'p')
				send_ping (job);
			else
				send_ttl (job);

			/*
			 * XXX: this is new:
			 * (this may remove a packet from the queue, but 
			 * the next-pointer should still be valid.
			 * look, if a packet is in the receive queue: 
			 */
			do_receive (0);
		}
	}
#if 0
	/* check for exceeded tries: */
        for (job = jobs; job; job = job->next)
	  {
	      /* are we in need to resend this job: */
	      if (! job->done && job->cur_seq > retries)
		{
		    job->done = 1;
		    job->tim = -1;
		}
	  }
#endif
}


/*
 * return a string with the mask in dot notation: eg:  255.255.0.0
 * note: static bufferspace is returned.
 */

static char *
mask_str (mask)
ipaddr_t mask;
{
	static char buf [80];
	int a = (mask >> 24) & 0xff, b = (mask >> 16) & 0xff;
	int c = (mask >> 8) & 0xff, d = mask & 0xff;
	sprintf (buf, "%d.%d.%d.%d", a, b, c, d);
	return buf;
}


/*
 * this job seems to be done; report the result:
 */

static void
report (job)
a_job *job;
{
  if (job->cur_seq > retries)
    {
      printf ("%s %s\n", job->hname, (do_mask) ? mask_str ((ipaddr_t) 0) : 
	      ((do_tstamp) ? "9999" : "-1"));
    }
  else
    {
      if (do_mask)
	      printf ("%s %s\n", (job->tag == 'p') ? job->hname 
		      : my_inaddr (&job->hop), mask_str (job->mask));
      else if (do_tstamp)
	      printf ("%s %ld\n", (job->tag == 'p') ? job->hname 
		      : my_inaddr (&job->hop), (long) job->mask);
      else
	      printf ("%s %d\n", (job->tag == 'p') ? job->hname 
		      : ((reply_same && job->tdone && mis_dig (job->hname[0]))
			 ? job->hname
			 : my_inaddr (&job->hop)),
		      job->tim);
    }
  fflush (stdout);
}


/*
 * step through the job list and decide which are done; report
 * and remove.
 */

static void
check_results (wtime)
int wtime;
{
	a_job **job = &jobs;
	int i;

	for (i = 0; i < 2; i++)
	{
		if (i)
			do_receive (wtime);

		while (*job)
		{
			if ((*job)->done || (*job)->cur_seq > retries)
			{
				a_job *f = *job;
				report (f);
				if (do_debug)
					fprintf (stderr,
						 "* job `%c' %s discarded\n",
						 f->tag, f->hname);
				*job = (*job)->next;
				/* xfree (f);  better leave it intact */
			}
			if (*job)
				job = &(*job)->next;
		}
	}
}


/*
 * parse the options, given in a interactive entered line.
 * the options are same as cmd-line options:
 * return a ptr to the remaining string:
 */

static char *
scan_options (buf)
char *buf;
{
	int n;

#define skip_white(s)		while (*s == ' ' || *s == '\t') s++;
#define skip_non_white(s)	while (*s && *s != ' ' && *s != '\t') s++;

	/* on interactive mode, assume no ttl probe: */
	do_ttl = 0;
	/* and default is ping'ing: */
	do_mask = 0;
	do_tstamp = 0;

	skip_white (buf);
	while (*buf == '-')
	{
		if (1 == sscanf (buf, "-ttl %d", &n)
		    || 1 == sscanf (buf, "-trace %d", &n))
		{
			if (! strncmp (buf, "-trace", 6))
				reply_same = 1;
			else
				reply_same = 0;

			if (n <= 0)
			{
				if (do_debug)
					fprintf (stderr, "* bad ttl %d\n", n);
			}
			else {
				if (do_debug)
					fprintf (stderr, "* ttl = %d\n", n);
				do_ttl = n;
			}
			skip_non_white (buf);
			skip_white (buf);
			skip_non_white (buf);
		}
		else if (1 == sscanf (buf, "-size %d", &n)
			 || 1 == sscanf (buf, "-s %d", &n))
		{
			if (n <= 44 || n > MAX_DATALEN)
			{
				if (do_debug)
					fprintf (stderr, "* bad size %d\n", n);
				if (n <= 44)
					n = 44;
				else if (n > MAX_DATALEN)
					n = MAX_DATALEN;
			}
			if (do_debug)
				fprintf (stderr, "* size = %d\n", n);
			data_len = n;

			skip_non_white (buf);
			skip_white (buf);
			skip_non_white (buf);
		}
		else if (1 == sscanf (buf, "-delay %d", &n)
			 || 1 == sscanf (buf, "-d %d", &n))
		{
			if (n < 0)
			{
				if (do_debug)
				       fprintf (stderr, "* bad delay %d\n", n);
				delay_time = 0;
			}
			else {
				if (n > 999999)
					n = 999999;
				if (do_debug)
					fprintf (stderr, "* delay = %d\n", n);
				delay_time = n;
			}
			skip_non_white (buf);
			skip_white (buf);
			skip_non_white (buf);
		}
		else if (1 == sscanf (buf, "-retries %d", &n)
			 || 1 == sscanf (buf, "-r %d", &n))
		{
			if (n < 0)
			{
				if (do_debug)
					fprintf (stderr, "* bad retr %d\n", n);
				retries = 0;
			}
			else {
				if (do_debug)
					fprintf (stderr, "* retr = %d\n", n);
				retries = n;
			}
			wait_time = (1000 * timeout) / (retries + 1);
			skip_non_white (buf);
			skip_white (buf);
			skip_non_white (buf);
		}
		else if (1 == sscanf (buf, "-timeout %d", &n)
			 || 1 == sscanf (buf, "-t %d", &n))
		{
			if (n <= 0)
			{
				if (do_debug)
					fprintf (stderr, "* bad timo %d\n", n);
				timeout = 1;
			}
			else {
				if (do_debug)
					fprintf (stderr, "* timo = %d\n", n);
				timeout = n;
			}
			wait_time = (1000 * timeout) / (retries + 1);
			skip_non_white (buf);
			skip_white (buf);
			skip_non_white (buf);
		}
		else if (! strncmp (buf, "-mask", strlen ("-mask"))
			 || ! strncmp (buf, "-m", strlen ("-m")))
		 {
			 do_mask = 1;
			 skip_non_white (buf);
		 }
		else if (! strncmp (buf, "-timestamp", strlen ("-timestamp"))
			 || ! strncmp (buf, "-tstamp", strlen ("-tstamp")))
		 {
		   	do_tstamp = 1;
			skip_non_white (buf);
		 }
		else if (! strncmp (buf, "-same", strlen ("-same")))
			 reply_same = 1;
		else if (! strncmp (buf, "-nocheckswap", 
				    strlen ("-nocheckswap")))
			check_sex = 0;
		else if (! strncmp (buf, "-checkswap", 
				    strlen ("-checkswap")))
			check_sex = 1;
		else {
			fprintf (stderr, "ntping: bad option in `%s'\n", buf);
			break;
		}
		skip_white (buf);
	}

	return buf;
}



static void
bones_add_out (buf, job)
char *buf;
a_job *job;
{
	if (do_mask)
		sprintf (buf, "{%s %s} ", (job->tag == 'p') ? job->hname
			 : my_inaddr (&job->hop), mask_str (job->mask));
	else if (do_tstamp)
	{
		/* write an empty list-element for scotty: */
		if (job->cur_seq <= retries)
			sprintf (buf, "{%s %ld} ", (job->tag == 'p') ? 
				 job->hname : my_inaddr (&job->hop), 
				 (long) job->mask);
		else
			sprintf (buf, "{%s {}} ", (job->tag == 'p') ?
				 job->hname : my_inaddr (&job->hop));
	}
	else
		sprintf (buf, "{%s %d} ", (job->tag == 'p')
			 ? job->hname 
			 : ((job->tdone && reply_same
			     && mis_dig (job->hname[0]))
			    ? job->hname
			    : my_inaddr (&job->hop)),
			 job->tim);
}


/*
 * the bones check routine; if all jobs are done, build the response
 * and discard all jobs.  return 0, if not done.
 */

static int
got_bones_result ()
{
	a_job *j, **job = &jobs;
	char *res;
	int res_len, rlen, i;

	do_receive (1);		/* check queue */
	
	for (j = jobs; j; j = j->next)
	{
/**
		if (! j->done && j->cur_seq > retries + 1)
		{
			j->done = 1;
			j->tim = -1;
		}
**/
		if (! j->done)
		{
			if (do_debug)
				fprintf (stderr, "* bones_result: nope\n");
			return 0;
		}
	}
	
	rlen = 0;
	res_len = 1024;
	res = xmalloc (1024);
	*res = 0;		/* init to EMPTY result */

	for (i = 0; jobs; i++)
	{
		for (job = &jobs; *job;  )
		{
			if ((*job)->job_no == i)
			{
				a_job *f = *job;
				if (rlen + 256 > res_len)
				{
					res_len += 1024;
					res = xrealloc (res, res_len);
				}
				bones_add_out (res + rlen, f);
				rlen = strlen (res);
				if (do_debug)
					fprintf (stderr,
					 "* job `%c' no %d  %s discarded\n",
						 f->tag, f->job_no, 
						 (f->tag == 'p') ? f->hname 
						 : my_inaddr (&f->hop));
				*job = (*job)->next;
				if (f->hname) 
					xfree (f->hname);
				xfree (f);
				break;
			}
			if (*job)
				job = &(*job)->next;
		}
	}
	
	printf ("%s\n", res);
	fflush (stdout);
	
	xfree (res);

	return 1;
}


/*
 * create a new job (or jobs) and make an job entry.
 * return 1 on success, 0 aon failure.
 */

static int
make_job (hname)
char *hname;
{			
	a_job *job = TALLOC (a_job);
	char tag = 'p';
	int len, ttl = 30;

	/*
	 * remove leading/trailing whites:
	 */
	while (*hname == ' ')
	  hname++;
	while ((len = strlen (hname)) > 0
	       && (hname [len - 1] == '\n' || hname [len - 1] == ' '))
	  hname [len - 1] = '\0';
	
	if (do_debug)
		fprintf (stderr, "* examining `%s'\n", hname);
	
	/*
	 * check about -ttl option:
	 */
	if (do_ttl)
	{
		tag = 't';
		ttl = do_ttl;
	}

	if (tag == 't' && do_debug)
		fprintf (stderr, "* got ttl order: %d `%s'\n", ttl, hname);
	
	job->done = 0;
	job->tim = -1;
	job->cur_seq = 0;

	if (! make_addr (&job->sa, hname))
	{
		if (bones)
			job->done = 1, job->cur_seq = retries + 1;
		else {
		  	if (do_mask)
			    printf ("%s 0.0.0.0\n", hname);
			else 
			    printf ("%s %d\n", hname, (do_tstamp) ? 9999 : -1);
			xfree (job);
			return 0;
		}
	}
	job->tag = tag;
	job->hname = xstrdup (hname);
	job->ttl = ttl;
	job->tdone = 0;
	job->port = base_port + ttl;
	job->id = icmp_ident++;
	job->hop.sin_addr.s_addr = 0;		/* clear */
	job->mask = 0;				/* 0 == unknown */
	
	if (do_debug)
		fprintf (stderr, "* made job: `%s' `%c' (ttl %d)\n",
			 hname, tag, ttl);
	job->next = jobs;
	jobs = job;

	/* give some space to the next one: */
	base_port += 30;
	if (base_port > MAX_BASE_PORT)
		base_port = BASE_PORT;

	return 1;
}

static void
make_jobs (buf)
char *buf;
{
	char *sep = " \t\r\n\f{}";
	char *ptr = strtok (buf, sep);
	int i;

	for (i = 0; ptr && *ptr; i++)
	{
                if (make_job (ptr))
			jobs->job_no = i;
                ptr = strtok ((char *) 0, sep);
        }
}


/*
 * split this line into a bones job-list:
 */

static void
make_bones_job (buf)
char *buf;
{
	char *ptr = buf, *sep = " \t\r\n\f{}";
	int got_ttl = 0, ttl = 30, i;

	ptr = strtok (buf, sep);

	if (do_ttl)
	{
		got_ttl = 1;
		ttl = do_ttl;
	}

	for (i = 0; ptr && *ptr; i++)
	{
		if (make_job (ptr))
		{
			jobs->job_no = i;
			if (got_ttl)
			{
				jobs->tag = 't';
				jobs->ttl = ttl;
			}
		}
		ptr = strtok ((char *) 0, sep);
	}
}


/*
 * here we are entering the normal interactive part: read from stdin and
 * process the read line:
 */

static void
do_interactive ()
{
	char *ptr, tmp [102400];		/* XXX: static buffer */

	for (;;)
	{
		while (fd_ready (fileno (stdin), 0))
		{
			if (tmp != fgets (tmp, sizeof (tmp), stdin))
			{
				while (jobs)
				{
					check_results (wait_time);      
					send_pending ();
				}
				if (do_debug)
					fprintf (stderr, "* bye.\n");
				exit (0);
			}
			ptr = scan_options (tmp);
			make_jobs (ptr);
		}
		send_pending ();
		check_results (wait_time);	/* eg. 1 ms waittime */
	}
	/* not reached */
}


/*
 * here we are entering the interactive part for bones. read a list of
 * names from stdin and process as a chunk.
 */

static void
do_bones_interactive ()
{
	char *ptr, tmp [102400];		/* XXX: static size ... */
	a_job *job;
	int i;

	if (do_debug)
		fprintf (stderr, "* do_bones_interactive...\n");

	while (tmp == fgets (tmp, sizeof (tmp), stdin))
	{
		/* fprintf(stderr,"got '%s'\n", tmp); */	/* karl */
		tmp [102400-1] = 0;		/* XXX: static size */

		ptr = scan_options (tmp);
		make_bones_job (ptr);
		if (do_debug)
			fprintf (stderr, "** now see:\n");
		for (i = 0; i < retries + 1; i++)
		{
			if (do_debug)
				fprintf (stderr, "** sendloop %d\n", i);
			send_pending ();
			do_receive (0);

			if (! got_bones_result ())
			  do_receive (wait_time);
			else
			  break;
		}

		/* mark unfinished as done: */
		for (job = jobs; job; job = job->next)
		  {
		      if (! job->done)
			{
			    job->done = 1;
			    job->tim = -1;
			}
		  }
		
		/* report, if not done: */
		if (i == retries + 1)
		  got_bones_result ();
	}

	if (do_debug)
		fprintf (stderr, "* bye.\n");
	exit (0);
	/* not reached */
}


/*
 * get an icmp socket and an ip_raw socket:
 */

static int
init_socks ()
{
	struct protoent *proto;
	int icmp_proto = 1;			/* Capt'n Default */
	struct sockaddr_in maddr;
#ifndef linux
#ifdef IP_HDRINCL
	int on = 1; /* karl */
#endif
#endif
#ifdef USE_DLPI
	struct sockaddr_in tmp_addr;
	int ta_len = sizeof (tmp_addr);
#endif
	int init_data_len = data_len + sizeof (struct ip);


	/*
	 * sanity: dont tell about cannot open socket, if no root
	 * permissions avail. check uid by hand.
	 */

	if (geteuid ())
	  {
	      fprintf (stderr,
 "ntping: warning: running with euid %d -- not with root permissions.\n",
		       geteuid ());
	      fprintf (stderr,
 "ntping: warning: expect missing permissions getting the icmp socket.\n");
	  }


	if ((proto = getprotobyname ("icmp")) == NULL) 
		{ /* perror ("ntping: icmp protocol unknown; reason"); */ }
	else
		icmp_proto = proto->p_proto;
	
	if ((icsock = socket (AF_INET, SOCK_RAW, icmp_proto)) < 0) 
	{
		perror ("ntping: cannot get icmp socket; reason");
		return 0;
	}
#ifdef USE_DLPI
	/*
	 * My mom told me: if it hurts don't do it ...
	 * this looks quite useable.
	 */
	if ((ipsock = socket (AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) 
	{
		perror ("ntping: cannot get udp socket; reason");
		return 0;
	}
#else /* ! USE_DLPI */
	if ((ipsock = socket (AF_INET, SOCK_RAW, IPPROTO_RAW)) < 0) 
	{
		perror ("ntping: cannot get ip socket; reason");
		return 0;
	}
#endif /* ! USE_DLPI */
	/*
	 * SO_SNDBUF and IP_HDRINCL fix from karl@sugar.NeoSoft.COM:
	 * seems to be neccesary for 386bsd and others.
	 */
#if defined(SO_SNDBUF)
	/* in interactive mode, we can have varying sizes: */
	if (interactive)
		init_data_len = MAX_DATALEN + sizeof (struct ip);
	if (setsockopt(ipsock, SOL_SOCKET, SO_SNDBUF, (char *)&init_data_len,
		       sizeof(init_data_len)) < 0) {
		perror("ntping: SO_SNDBUF");
		exit(6);
	}
#endif /* SO_SNDBUF */

#if !defined(linux) && !defined(USE_DLPI)
#ifdef IP_HDRINCL
	if (setsockopt(ipsock, IPPROTO_IP, IP_HDRINCL, (char *)&on,
		       sizeof(on)) < 0) {
		perror("ntping: IP_HDRINCL");
		exit(6);
}
#endif /* IP_HDRINCL */
#endif /* ! linux && ! USE_DLPI */

	memset ((char *) &maddr, 0, sizeof (maddr));
	maddr.sin_family = AF_INET;
	maddr.sin_addr.s_addr = INADDR_ANY;
	maddr.sin_port = 0;
#if defined(linux) && ! defined(USE_DLPI)
	/* linux pukes on bind, but seems to do the tracing stuff.
	 * try it on your own risk.... */
#else
	if (bind (ipsock, (struct sockaddr *) &maddr, sizeof (maddr)) < 0)
	  {
	    perror ("ntping: cannot bind socket; reason");
	    return 0;
	  }
#endif

#ifdef USE_DLPI
	/*
	 * we cannot send a datagramm with a src-port of our own 
	 * choice - too bad.
	 * therefore we connot identfify the icmp-port-unreachable 
	 * with our homebrewed id. save the original port and check about:
	 */
	
	if (getsockname (ipsock, (struct sockaddr *) &tmp_addr, &ta_len) < 0)
	{
	    perror ("ntping: cannot get sockname; reason");
	    return 0;
	}
	src_port = ntohs (tmp_addr.sin_port);
	if (do_debug)
	    printf ("* dlpi: my port is %d (0x%lx)\n", (int) src_port,
		    (int) src_port);

	/*
	 * let's try to work around a funny bug: open the complement
	 * port, to allow reception of icmp-answers with byte-swapped
	 * src-port field.  if we cannot get this port, silently
	 * ignore and continue normal.
	 */

	if ((ipsock2 = socket (AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) 
	{
	    if (do_debug)
	      perror ("* cannot get udp socket #2; reason");
	    ipsock2 = -1;
	}
	else {
	    struct sockaddr_in maddr2;

	    memset ((char *) &maddr2, 0, sizeof (maddr2));
	    maddr2.sin_family = AF_INET;
	    maddr2.sin_addr.s_addr = INADDR_ANY;
	    maddr2.sin_port = htons (sexy (src_port));
	    
	    if (bind (ipsock2, (struct sockaddr *) &maddr2, 
		      sizeof (maddr2)) < 0)
	      {
		  if (do_debug)
		    perror ("* cannot bind socket #2; reason");
		  ipsock2 = -1;
	      }
	}

	if (ipsock2 != -1 && do_debug)
	  printf ("* got the second port # %d (0x%lx)\n", sexy (src_port),
		  sexy (src_port));

#endif /* USE_DLPI */

	/* everything is fine: */
	return 1;
}


/*
 * what we are willing to manage ?
 */

static void
usage ()
{
	fprintf (stderr, "\nUse : ntping [<options>] [<hosts>] ");
	fprintf (stderr, "\noptions are:\n");
	fprintf (stderr, "\t-V              show version and exit.\n");
/*	fprintf (stderr, "\t-D              give verbose debug output.\n"); */
	fprintf (stderr, "\t-b(ones)        run in `scotty' mode.\n");
	fprintf (stderr, "\t-s(ize) <n>     set size of packets.\n");
	fprintf (stderr, "\t-r(etries) <n>  set # of retries.\n");
	fprintf (stderr, "\t-t(imeout) <n>  set timeout for an answer.\n");
	fprintf (stderr, "\t-d(elay) <n>    set the send delay to <n> ms.\n");
	fprintf (stderr, 
          "\t-ttl <n>        trace a hop with time-to-live set to n.\n");
	fprintf (stderr, 
 	  "\t-trace <n>      same as -ttl, but the destination is returned\n");
	fprintf (stderr, 
	  "\t                for the last hop, if it is a dotted number.\n");
	fprintf (stderr, "\t-mask           send an icmp-mask request.\n");
	fprintf (stderr, "\t-tstamp         send a icmp-timestamp request.\n");
	fprintf (stderr, "\n");
	
	exit (1);
}

static void 
examine_arguments (argc, argv)
int argc;
char *argv[];
{
	int val;

	/*
	 * parse the given options and override the default, if given;
	 */

	while (++argv, --argc > 0)
	{
		if (! strcmp ("-V", *argv))
		{
			fprintf (stderr, "Version:  %s\n", version);
			exit (0);
		}
		else if (! strcmp ("-D", *argv))
		{
			do_debug = 1;
		}
		else if (! strcmp ("-bones", *argv) 
			 || ! strcmp ("-scotty", *argv)
			 || ! strcmp ("-b", *argv))
		{
			bones = 1;
		}
		else if (! strcmp ("-size", *argv) || ! strcmp ("-s", *argv))
		{
			if (--argc <= 0)
				usage ();
			val = atoi (*++argv);
			if (val >= 64 && val <= MAX_DATALEN)
				data_len = val;
			else
				usage();
		}
		else if (! strcmp ("-retries", *argv) 
			 || ! strcmp ("-r", *argv))
		{
			if (--argc <= 0)
				usage ();
			val = atoi (*++argv);
			if (val < 0)
				val = 0;
			retries = val;
			wait_time = (1000 * timeout) / (retries + 1);
		}
		else if (! strcmp ("-timeout", *argv) 
			 || ! strcmp ("-t", *argv))
		{
			if (--argc <= 0)
				usage ();
			val = atoi (*++argv);
			if (val < 0)
				usage ();
			timeout = val;
			wait_time = (1000 * timeout) / (retries + 1);
		}
		else if (! strcmp ("-delay", *argv) 
			 || ! strcmp ("-d", *argv))
		{
			if (--argc <= 0)
				usage ();
			val = atoi (*++argv);
			if (val < 0)
				val = 0;
			if (val > 999999)
				val = 999999;
			delay_time = val;
		}
		else if (! strcmp ("-ttl", *argv) 
			 || ! strcmp ("-trace", *argv))
		{
			if (! strcmp ("-trace", *argv))
				reply_same = 1;
			else
				reply_same = 0;

			if (--argc <= 0)
                                usage ();
                        val = atoi (*++argv);
                        if (val <= 0)
			  	val = 1;
			do_ttl = val;
		}
		else if (! strcmp ("-mask", *argv) || ! strcmp ("-m", *argv))
			do_mask = 1;
		else if (! strcmp ("-timestamp", *argv) 
			 || ! strcmp ("-tstamp", *argv))
			do_tstamp = 1;
		else if (! strcmp ("-same", *argv))
			reply_same = 1;
		else if (! strcmp ("-nocheckswap", *argv))
			check_sex = 0;
		else if (! strcmp ("-checkswap", *argv))
			check_sex = 1;
		else if (*argv[0] == '-')
			usage ();
		else if (bones)
		{
			sprintf (bbuf + blen, " %s", *argv);
			blen = strlen (bbuf);
		}
		else
			make_job (*argv);
	}

	if (bones && ! bbuf [0])
		interactive = 1;
	else if (! bones && ! jobs) 
	{
		if (do_debug)
			fprintf (stderr, "* using interactive mode.\n");
		interactive = 1;
	}

	if (interactive && do_ttl)
	{
		fprintf (stderr,
	 "ntping: sorry - cannot mix `-ttl' option with interactive mode.\n");
		exit (1);
	}
}


/*
 * 	M A I N :
 */

int
main (argc, argv)
int argc;
char *argv[];
{
	/* we will need the id urgently... */
	icmp_ident = (getpid () & 0xff) << 8;

	/* now have a look: aye aye sir! */
	examine_arguments (argc, argv);

	/* fetch and initialize the icmp and the ip socket: */
	if (! init_socks ())
	  return 1;

	/* back to normal rights: */
	setuid (getuid ());

	if (interactive)
	{
		if (bones)
			do_bones_interactive ();
	  	else
			do_interactive ();
		/* not reached */
	}
		
	/*
	 * not interactive: process hosts list and exit:
	 */

	if (bones) 
	{
		if (do_debug)
			fprintf (stderr, "* looking at `%s'...\n", bbuf);
		make_bones_job (bbuf);
		while (! got_bones_result ())
		{
			send_pending ();
			do_receive (wait_time);	    /* eg. 500 ms waittime */
		}
	}
	

	while (jobs)
	  {
	    send_pending ();
	    check_results (wait_time);		/* eg. 500 ms waittime */
	  }

	if (do_debug)
		fprintf (stderr, "* bye.\n");

	return 0;
}

/* end of ntping.c */
