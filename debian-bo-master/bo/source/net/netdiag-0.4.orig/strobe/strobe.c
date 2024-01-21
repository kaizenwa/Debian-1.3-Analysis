/*
 * Strobe v0.92 (c) 1994 *Proff* (proff@suburbia.apana.org.au), All
 * rights reserved.
 *
 * use of this program is at your own, or others risk. the author is
 * not considered a member of either group. comprehend?
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <string.h>
#include <errno.h>

#if !defined(solaris) && !defined(linux) && !defined(__FreeBSD__) && !defined(__NetBSD__) && !defined(__GCC__)
#  define fvoid
extern int optind;
extern char *optarg;
#else
#  define fvoid void
#endif
#define bool char

#ifndef INADDR_NONE
#  define INADDR_NONE ((unsigned long)-1)
#endif

/*
 * the below should be set via the Makefile, but if not...
 */

#ifndef ETC_SERVICES
#  define ETC_SERVICES "/etc/services"
#endif
#ifndef STROBE_SERVICES
#  define STROBE_SERVICES "strobe.services"
#endif
#ifndef LIB_STROBE_SERVICES
#  define LIB_STROBE_SERVICES "/usr/local/lib/strobe.services"
#endif

int a_timeout = 20;
char *a_output = NULL;
char *a_services = "strobe.services";
char *a_input = NULL;
int a_start = 1;
int a_end = 65535;
int a_sock_max = 64;
int a_abort = 0;
bool f_linear = 0;
bool f_verbose = 0;
bool f_verbose_stats = 0;
bool f_fast = 0;
bool f_stats = 0;
bool f_quiet = 0;
bool f_delete_dupes = 0;

bool f_set;
int connects;
int hosts_done=-1;
int attempts_done;
struct timeval time_start;

int host_n;
int Argc;
char **Argv;

FILE *fh_input;

#define HO_ABORT 1
#define HO_COMPLETING 2

struct hosts_s
{
    char *name;
    struct in_addr in_addr;
    unsigned int port;
    struct timeval time_used;
    struct timeval time_start;
    int attempts;
    int attempts_done;
    int attempts_highest_done;
    int connects;
    time_t notice_abort;
    int status;
};
struct hosts_s *hosts;

#define HT_SOCKET 1
#define HT_CONNECTING 2

struct htuple
{
    char *name;
    struct in_addr in_addr;
    unsigned int port;
    int sfd;
    int status;
    struct timeval sock_start;
    int timeout;
    struct hosts_s *host;
};

struct htuple ht_initial;
struct htuple *attempt;

struct port_desc_s
{
    char *name;
    char *portname;
    struct port_desc_s *next;
};

struct port_desc_s *port_descs[65536];

char *
Smalloc (len)
  int len;
{
    char *p;
    while (!(p = malloc (len)))
	sleep (1);
    return p;
}

fvoid
sock_block (sfd)
  int sfd;
{
    int flags;
    flags = (~O_NONBLOCK) & fcntl (sfd, F_GETFL);
    fcntl (sfd, F_SETFL, flags);
}

fvoid
sock_unblock (sfd)
  int sfd;
{
    int flags;
    flags = O_NONBLOCK | fcntl (sfd, F_GETFL);
    fcntl (sfd, F_SETFL, flags);
}

int
timeval_subtract (result, x, y)
  struct timeval *result, *x, *y;
{
/* Perform the carry for the later subtraction by updating y. */
if (x->tv_usec < y->tv_usec) {
 int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
 y->tv_usec -= 1000000 * nsec;
 y->tv_sec += nsec;
}
if (x->tv_usec - y->tv_usec > 1000000) {
 int nsec = (y->tv_usec - x->tv_usec) / 1000000;
 y->tv_usec += 1000000 * nsec;
 y->tv_sec -= nsec;
}

/* Compute the time remaining to wait.
  `tv_usec' is certainly positive. */
result->tv_sec = x->tv_sec - y->tv_sec;
result->tv_usec = x->tv_usec - y->tv_usec;

/* Return 1 if result is negative. */
return x->tv_sec < y->tv_sec;
}

fvoid
attempt_clear (h)
  struct htuple *h;
{
    if (h->status & HT_SOCKET)
    {
	struct timeval tv1, tv2;
	gettimeofday(&tv1, NULL);
	timeval_subtract(&tv2, &tv1, &(h->sock_start));
	h->host->time_used.tv_sec+=tv2.tv_sec;
	if ((h->host->time_used.tv_usec+=tv2.tv_usec) >= 1000000)
	{
	    h->host->time_used.tv_usec -= 1000000;
	    h->host->time_used.tv_sec++;
	}
        attempts_done++;
	h->host->attempts_done++;
	if (h->port > h->host->attempts_highest_done)
	    h->host->attempts_highest_done=h->port;
	sock_unblock (h->sfd);
	shutdown (h->sfd, 2);
	close (h->sfd);
    }
    *h = ht_initial;
}

fvoid
clear_all ()
{
    int n;
    for (n = 0; n < a_sock_max; n++)
	attempt_clear (&attempt[n]);
}

fvoid
attempt_init ()
{
    int n;
    for (n = 0; n < a_sock_max; n++)
	attempt[n] = ht_initial;
}

int
sc_connect (h)
  struct htuple *h;
{
    struct sockaddr_in sa_in;
    int sopts1 = 1;
    struct linger slinger;
    sa_in.sin_family = AF_INET;
    sa_in.sin_addr = h->in_addr;
    sa_in.sin_port = htons (h->port);
    if ((h->sfd = socket (PF_INET, SOCK_STREAM, 0)) == -1)
	return 0;
    h->status |= HT_SOCKET;
    gettimeofday(&(h->sock_start), NULL);
    sock_unblock (h->sfd);
    setsockopt (h->sfd, SOL_SOCKET, SO_REUSEADDR, (char *) &sopts1, sizeof (sopts1));
    setsockopt (h->sfd, SOL_SOCKET, SO_OOBINLINE, (char *) &sopts1, sizeof (sopts1));
    slinger.l_onoff = 0;	/* off */
    setsockopt (h->sfd, SOL_SOCKET, SO_LINGER, (char *) &slinger, sizeof (slinger));
    if (connect (h->sfd, (struct sockaddr *) &sa_in, sizeof (sa_in)) == -1)
    {
	switch (errno)
	{
	case EINPROGRESS:
	case EWOULDBLOCK:
	    break;
	case ETIMEDOUT:
	case ECONNREFUSED:
	case EADDRNOTAVAIL:
	    if (f_verbose)
	    {
		fprintf(stderr, "%s:%d ", h->name, h->port);
		perror("");
	    }
	    h->host->attempts++;
	    attempt_clear (h);
	    return 1;
	default:
	    if (!f_quiet)
	    {
	    	fprintf(stderr, "%s:%d ", h->name, h->port);
	    	perror ("");
	    }
	    attempt_clear (h);
	    return 0;
	}
    }
    h->host->attempts++;
    h->status |= HT_CONNECTING;
    sock_block (h->sfd);
    return 1;
}

int
gatherer_tcp (h)
  struct htuple *h;
{
    struct port_desc_s *pd;
    if ((pd = port_descs[h->port]))
    {
	    printf ("%-30s %-16s %5d/tcp %s\n", h->name, pd->portname, h->port, pd->name);
	while (!f_delete_dupes && (pd=pd->next))
	    printf ("#%-29s %-16s %5d/tcp %s\n", h->name, pd->portname, h->port, pd->name);
    }
    else
	    printf ("%-30s %-16s %5d/tcp unassigned\n", h->name, "unknown", h->port);
    h->host->connects++;
    connects++;
        attempt_clear (h);
    return 1;
}

bool
gather ()
{
    fd_set set_sel_r;
    fd_set set_sel_w;
    struct timeval timeout;
    struct htuple *h;
    int n;
    /* We regenerate the entire set rather than just use set=old_set
     * because in some implimentations fs_set is just an array
     */
    FD_ZERO(&set_sel_r);
    FD_ZERO(&set_sel_w);
    for (f_set=n=0; n<a_sock_max; n++)
    {
        h=&attempt[n];
	if (h->status & HT_CONNECTING)
        {
            FD_SET(h->sfd, &set_sel_r);
            FD_SET(h->sfd, &set_sel_w);
	    f_set=1;
        }
    }
    if (!f_set) return 1;
    timeout.tv_sec = 0;
    timeout.tv_usec = 50;
    switch (select (FD_SETSIZE, &set_sel_r, &set_sel_w, 0, &timeout))
    {
    case -1:
	perror ("select");
	return 0;
    case 0:
	return 1;
    }
    for (n = 0; n < a_sock_max; n++)
    {
	h = &attempt[n];
	if (h->status & HT_CONNECTING)
	{
	    if (FD_ISSET (h->sfd, &set_sel_r) || FD_ISSET (h->sfd, &set_sel_w))
	    {
		struct sockaddr_in in;
		int len = sizeof (in);
		/* select() lies occasionaly
                 */
		if (getpeername (h->sfd, (struct sockaddr *) &in, &len) == 0)
		    gatherer_tcp (h);
		else
		    attempt_clear (h);
	    }
	}
    }
    return 1;
}

bool
add_attempt (add)
  struct htuple *add;
{
    struct htuple *h;
    static time_t oldtime;
    int n;
    for (;;)
    {
	for (n = 0; n < a_sock_max; n++)
	{
	    time_t tim = time (0);
	    h = &attempt[n];
	    if (!h->status)
		goto foundfree;
	    if ((h->status & HT_SOCKET) &&
		((h->sock_start.tv_sec + h->timeout) < tim))
	    {
		attempt_clear (h);
		goto foundfree;
	    }
	}
	gather ();
	continue;
      foundfree:
	*h = *add;
	if (!sc_connect (h))
	{
	    gather ();
	    continue;
	}
	if ((oldtime + 1) < time (0))
	{
	    oldtime = time (0);
	    gather ();
	}
	break;
    }
    return 1;
}

int
scatter (host, timeout)
  struct hosts_s *host;
  int timeout;
{
    static struct htuple add;
    add = ht_initial;
    add.host = host;
    add.name = host->name;
    add.in_addr = host->in_addr;
    add.port = host->port;
    add.timeout = a_timeout;
    if (f_verbose)
	fprintf (stderr, "attempting port=%d host=%s\n", add.port, add.name);
    add_attempt (&add);
    return 1;
}

fvoid
wait_end (t)
  int t;
{
    time_t st;
    st = time (0);
    while ((st + t) > time (0))
    {
	gather ();
	if (!f_set) break;
    }
}

struct in_addr
resolve (name)
  char *name;
{
    static struct in_addr in;
    unsigned long l;
    struct hostent *ent;
    if ((l = inet_addr (name)) != INADDR_NONE)
    {
	in.s_addr = l;
	return in;
    }
    if (!(ent = gethostbyname (name)))
    {
	perror (name);
	in.s_addr = INADDR_NONE;
	return in;
    }
    return *(struct in_addr *) ent->h_addr;
}

char *
next_host ()
{
    static char lbuf[512];
    hosts_done++;
    if (a_input)
    {
	int n;
reread:
	if (!fgets (lbuf, sizeof (lbuf), fh_input))
	{
	    fclose (fh_input);
	    return NULL;
	}
	if (strchr("# \t\n", lbuf[0])) goto reread;
	n = strcspn (lbuf, " \t\n");
	if (n)
	    lbuf[n] = '\0';
	return lbuf;
    }
    if (++host_n >= Argc)
	return NULL;
    return Argv[host_n];
}

bool
host_init (h, name, nocheck)
  struct hosts_s *h;
  char *name;
  bool nocheck;
{
    int n;
    h->name = 0;
    h->in_addr = resolve (name);
    if (h->in_addr.s_addr == INADDR_NONE)
	return 0;
    if (!nocheck)
        for (n=0; n<a_sock_max; n++)
   	{ 
	    if (hosts[n].name && hosts[n].in_addr.s_addr==h->in_addr.s_addr)
	    {
		if (!f_quiet)
		    fprintf(stderr, "ip duplication: %s == %s (last host ignored)\n",
		        hosts[n].name, name);
		return 0;
	    }
        }
    h->name = (char *) Smalloc (strlen (name) + 1);
    strcpy (h->name, name);
    h->port = a_start;
    h->time_used.tv_sec
      = h->time_used.tv_usec
      = h->attempts_done 
      = h->attempts
      = h->connects
      = h->notice_abort
      = h->attempts_highest_done
      = h->status = 0;
    gettimeofday(&(h->time_start), NULL);
    return 1;
}

fvoid
host_clear (h)
  struct hosts_s *h;
{
    if (h->name)
    {
    	free (h->name);
    	h->name = NULL;
    }
}

fvoid
host_stats (h)
  struct hosts_s *h;
{
    struct timeval tv, tv2;
    float t, st;
    gettimeofday(&tv, NULL);
    timeval_subtract(&tv2, &tv, &(h->time_start));
    t = tv2.tv_sec+(float)tv2.tv_usec/1000000.0;
    st = h->time_used.tv_sec+(float)h->time_used.tv_usec/1000000.0;
    fprintf(stderr, "stats: host = %s trys = %d cons = %d time = %.2fs trys/s = %.2f trys/ss = %.2f\n",
	h->name, h->attempts_done, h->connects, t, h->attempts_done/t, h->attempts_done/st);
}

fvoid
final_stats()
{
    struct timeval tv, tv2;
    float t;
    gettimeofday(&tv, NULL);
    timeval_subtract(&tv2, &tv, &(time_start));
    t = tv2.tv_sec+(float)tv2.tv_usec/1000000.0;
    fprintf(stderr, "stats: hosts = %d trys = %d cons = %d time = %.2fs trys/s = %.2f\n",
	hosts_done, attempts_done, connects, t, attempts_done/t);
}

bool skip_host(h)
  struct hosts_s *h;
{
    if (a_abort && !h->connects && (h->attempts_highest_done >= a_abort)) /* async pain */
    {
	if (h->status & HO_ABORT)
	{
	    if ((time(NULL)-h->notice_abort)>a_timeout)
	    {
		if (f_verbose)
		    fprintf(stderr, "skipping: %s (no connects in %d attempts)\n",
			h->name, h->attempts_done);
		return 1;
	    }
	} else 
        {
		h->notice_abort=time(NULL);
		h->status|=HO_ABORT;
	}
    }
    return 0;
}

fvoid
scan_ports_linear ()
{
    struct hosts_s host;
    char *name;
    int n;
    while ((name = next_host ()))
    {
	if (!host_init(&host, name, 1)) continue;
	for (n = a_start; n <= a_end; n++)
	{
	    if (f_fast && !port_descs[n])
		continue;
	    host.port = n;
	    scatter (&host, a_timeout);
	    if (skip_host(&host)) break;
	}
	wait_end (a_timeout);
	if (f_verbose_stats)
	    host_stats (&host);
	clear_all ();
	host_clear(&host);
    }
}

/* Huristics:
 *  o  fast connections have priority == maximise bandwidth i.e 
 *     a port in the hand is worth two in the bush
 *  o  newer hosts have priority == lower ports checked more quickly
 *  o  all hosts eventually get equal "socket time" == despite
 *     priorities let no one host hog the sockets permanently
 *  o  when host usage times are equal (typically on or shortly after
 *     initial startup) distribute hosts<->sockets evenly rather than
 *     play a game of chaotic bifurcation ping-pong
 */
          
fvoid
scan_ports_paralell ()
{
    int n;
    struct timeval smallest_val;
    int smallest_cnt;
    char *name;
    struct hosts_s *h, *smallest = &hosts[0];
    while (smallest)
    {
	smallest_val.tv_sec=0xfffffff;
	smallest_val.tv_usec=0;
	for (n = 0, smallest_cnt = 0xfffffff, smallest = NULL; n < a_sock_max; n++)
	{
	    h = &hosts[n];
	    if (!h->name && ((name = next_host ())))
		if (!host_init (h, name, 0))
		{
		    host_clear (h);
		    continue;
		}
	    if (h->name)
	    {
		if (((h->time_used.tv_sec < smallest_val.tv_sec) ||
		     ((h->time_used.tv_sec == smallest_val.tv_sec) &&
		      (h->time_used.tv_usec <= h->time_used.tv_usec))) &&
		    (((h->time_used.tv_sec!=smallest_val.tv_sec) &&
		      (h->time_used.tv_usec!=smallest_val.tv_sec)) ||
		     (h->attempts < smallest_cnt)))
	        {
	  	    smallest_cnt = h->attempts;
		    smallest_val = h->time_used;
		    smallest = h;
		 }
	    }
	}
	if (smallest)
	{
	    if (!(smallest->status & HO_COMPLETING))
	    {
		scatter (smallest, a_timeout);
	        if (f_fast)
	  	    while ((++(smallest->port) < a_end) &&
		       !port_descs[(smallest->port)]);
	        else
		    smallest->port++;
	        if (smallest->port >= a_end) smallest->status|=HO_COMPLETING;
	    }
	    else
		gather();
	    if (((smallest->status & HO_COMPLETING) &&
                 (smallest->attempts_done == smallest->attempts)) ||
                skip_host(smallest))
	    {
		if (f_verbose_stats) host_stats (smallest);
		host_clear (smallest);
	    }
	}
    }
}

fvoid
loaddescs ()
{
    FILE *fh;
    char lbuf[1024];
    char desc[256];
    char portname[17];
    char prot[4];
    int port;
    prot[3]='\0';
    if (!(fh = fopen (a_services, "r")) &&
        !(fh = fopen (LIB_STROBE_SERVICES, "r")) &&
        !(fh = fopen (ETC_SERVICES, "r")))
    {
	perror (a_services);
	exit (1);
    }
    while (fgets (lbuf, sizeof (lbuf), fh))
    {
	char *p;
	struct port_desc_s *pd, *pdp;
	if (strchr("*# \t\n", lbuf[0])) continue;
	if (!(p = strchr (lbuf, '/'))) continue;
	*p = ' ';
	desc[0]='\0';
	if (sscanf (lbuf, "%16s %u %3s %255[^\r\n]", portname, &port, prot, desc) <3 || strcmp (prot, "tcp") || (port > 65535))
	    continue;
	pdp = (struct port_desc_s *) Smalloc (sizeof (*pd) + strlen (desc) + 1 + strlen (portname) + 1);
	if ((pd = port_descs[port]))
	{
	    for (; pd->next; pd = pd->next);
	    pd->next = pdp;
	    pd = pd->next;
	}
	else 
	{
	    pd = pdp;
	    pd->next = NULL;
	    port_descs[port] = pd;
	}
	pd->name = (char *) (pd) + sizeof (*pd);
	pd->portname = pd->name + strlen(desc)+1;
	strcpy (pd->name, desc);
	strcpy (pd->portname, portname);
    }
}

fvoid
usage ()
{
    fprintf (stderr, "\
usage: strobe [-v(erbose)]\n\
	      [-V(erbose_stats]\n\
	      [-s(tatistics)]\n\
	      [-q(uiet)]\n\
              [-o output_file]\n\
              [-b begin_port_n]\n\
              [-e end_port_n]\n\
              [-t timeout_n]\n\
              [-n num_sockets_n]\n\
              [-S services_file]\n\
              [-i hosts_input_file]\n\
              [-l(inear)]\n\
              [-f(ast)]\n\
              [-a abort_port_n]\n\
              [host1 [...host_n]]\n");
    exit (1);
}

int
main (argc, argv)
  int argc;
  char **argv;
{
    char c;
    if (argc < 2)
	usage ();
    while ((c = getopt (argc, argv, "o:dvVb:e:a:t:n:S:i:lfsq")) != -1)
	switch (c)
	{
	case 'o':
	    a_output = optarg;
	    break;
	case 'd':
	    f_delete_dupes=1;
	    break;
	case 'v':
	    f_verbose = 1;
	    break;
	case 'V':
	    f_verbose_stats = 1;
	    break;
	case 'b':
	    a_start = atoi (optarg);
	    break;
	case 'e':
	    a_end = atoi (optarg);
	    break;
	case 'a':
	    a_abort = atoi (optarg);
	    break;
	case 't':
	    a_timeout = atoi (optarg);
	    break;
	case 'n':
	    a_sock_max = atoi (optarg);
	    break;
	case 'S':
	    a_services = optarg;
	    break;
	case 'i':
	    a_input = optarg;
	    break;
	case 'l':
	    f_linear = 1;
	    break;
	case 'f':
	    f_fast = 1;
	    break;
	case 's':
	    f_stats = 1;
	    break;
        case 'q':
	    f_quiet = 1;
	    break;
	case '?':
	default:
	    fprintf (stderr, "unknown option %s\n", argv[optind]);
	    usage ();
	    /* NOT_REACHED */
	}
    host_n = optind - 1;
    if (!f_quiet)
        fprintf (stderr, "strobe (c) 1994 *Proff* All Rights Reserved.\n");
    if (a_input)
    {
	if (!(fh_input = fopen (a_input, "r")))
	{
	    perror (a_input);
	    exit (1);
	}
    }
    if (a_output)
    {
        int fd;
        if ((fd=open(a_output, O_WRONLY|O_CREAT|O_TRUNC, 0666))==-1)
	{
		perror(a_output);
		exit(1);
	}
	dup2(fd, 1);
    }
    Argc = argc;
    Argv = argv;
    if (!a_input && optind==argc-1)
	f_linear=1;
    attempt = (struct htuple *) Smalloc (a_sock_max * sizeof (struct htuple));
    if (!f_linear)
    	hosts = (struct hosts_s *) Smalloc (a_sock_max * sizeof (struct hosts_s));
    loaddescs ();
    gettimeofday(&time_start, NULL);
    f_linear ? scan_ports_linear ():
 	       scan_ports_paralell ();
    if (f_stats || f_verbose_stats)
	final_stats();
    exit (0);
}
