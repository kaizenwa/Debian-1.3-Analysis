/* 
                            NO WARRANTY

    THERE IS NO WARRANTY FOR THIS PROGRAM, TO THE EXTENT PERMITTED BY
  APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
  HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
  OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
  IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
  ALL NECESSARY SERVICING, REPAIR OR CORRECTION.  
                              
  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL
  ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
  REDISTRIBUTE THE PROGRAM, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
  GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
  USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
  DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
  PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
  EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGES.  
                                
*/

#ifndef lint
	char sccsid[]="@(#) netdate.c 1.16 85/08/21";
#endif
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/time.h>

#include <netinet/in.h>

#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <netdb.h>
#include <setjmp.h>
#include <signal.h>
#include <utmp.h>
#define WTMP "/etc/wtmp"

struct utmp wtmp[2] = {
	{ 0, 0, "|", "", 0, "", "", 0},
	{ 0, 0, "{", "", 0, "", "", 0}
};

char *service = "time";
char *defaultproto = "udp";
/* difference between 1900 (RFC868) and 1970 (UNIX) base times */
#define NETBASE	2208988800

long limit = 5;
#define MAXHOSTS 20

#define LOCALHOST "localhost"
char *whoami;
char hostname[65];
struct timeval now;
struct timehost {
	char *hostname;
	short local;
	short bad;
	char *protoname;
	long protonumber;
	int socktype;
	struct timeval asked;
	struct timeval then;
	struct timeval acked;
	long difference;
	long count;
} timehosts[MAXHOSTS];
struct timehost *tophost = &timehosts[MAXHOSTS];

void	usage (void);
int	setproto (char *, struct timehost *);
void	main (int, char **);
int	getdiff (struct timehost *);
int	getdate (struct timehost *);
void	printit (struct timehost *);
void	tvsub (struct timeval *, struct timeval *, struct timeval *);
void	printdiff (char *, struct timeval *);
void	timeout ();
static	internettime (struct timehost *thishost);
long	getport (char *protoname);

static int internettime (struct timehost *thishost);
struct timehost *mungediffs(struct timehost *);


void
usage (void)
{
	fprintf (stderr,
"usage: %s [ -l limit ] host ...\n"
"%s tries to find a group of at least two hosts whose times agree\n"
"within %d seconds, and sets the time to that of the first host in the group.\n",
		whoami, whoami, limit);
	fprintf (stderr,
"The limit may be set with the -l option.  Setting it to zero (or supplying\n"
"only one host name argument) will set the time to that of the first host to\n"
"respond. The caller must be super-user for the system time to be set.\n");

	exit (1);
}

int rdate = 0;
int verbose = 0;
int debug = 0;

void
main (int argc, char **argv)
{
	extern char *rindex();
	struct timehost *mungediffs();
	register struct timehost *thishost;
	int hostargs = 0;

	if ((whoami = rindex(*argv, '/')) != NULL)
		whoami++;
	else
		whoami = *argv;
	if (strcmp (whoami, "rdate") == 0) {	/* emulate SMI rdate command */
		rdate = 1;
		defaultproto = "tcp";
		limit = 0;
	}
	if (gethostname(hostname, (int)sizeof (hostname)) == -1) {
		perror ("gethostname");
		exit (1);
	}
	while (*++argv != NULL && **argv == '-') {
		switch (argv[0][1]) {
		case 'd':
			debug++;
			break;
		case 'v':
			verbose++;
			break;
		case 'l':
			if (*++argv == NULL)
				usage();
			limit = atoi(*argv);
			break;
		default:
			fprintf (stderr, "Unknown option:  %s\n", *argv);
			usage();
			break;
		}
	}
	if (*argv == NULL)
		usage();
	if (debug)
		fprintf (stderr, "%s: rdate %d; verbose %d; limit %d.\n", 
			whoami, rdate, verbose, limit);
	for (thishost = &timehosts[0]; *argv != NULL; argv++) {
		if (thishost >= tophost) {
			fprintf(stderr, "Too many hosts: ignoring");
			do {
				fprintf (stderr, " %s", *argv);
			} while (*++argv != NULL);
			fprintf (stderr, "\n");
			break;
		}
		if (setproto(*argv, thishost))
			continue;
		thishost -> hostname = *argv;
		thishost -> bad = 0;
		if (strcmp (thishost -> hostname, LOCALHOST) == 0)
			thishost -> local = 1;
		if (++hostargs == 1 && argv[1] == NULL)	/* Only one host arg, */
			limit = 0;			/* so just set to it. */
		if (limit == 0) {
			if (!getdate(thishost))
				continue;
			exit(0);
		}
		if (!getdiff (thishost))
			continue;
		thishost++;
	}
	if (limit == 0)
		exit(1);
	if (thishost == &timehosts[0])
		exit(1);
	if ((thishost = mungediffs(thishost)) == NULL) {
		fprintf (stderr,
			"No two hosts agree on the time within %d seconds\n",
			limit);
		exit(1);
	}
	if (!getdate (thishost))
		exit (1);
	exit(0);
}

int
setproto(char *what, struct timehost *thishost)
{
	static	char *protoname;
	static	long protonumber;
	static	int socktype;
	register struct protoent *pp;

	setprotoent(1);
	if ((pp = getprotobyname (what)) == NULL) {
		if (protoname == NULL)
			if (!setproto(defaultproto, thishost)) {
				fprintf(stderr,
		"Default protocol %s was not found in /etc/protocols.\n",
						defaultproto);
				exit(1);
			}
		thishost -> protoname = protoname;
		thishost -> protonumber = protonumber;
		thishost -> socktype = socktype;
		return(0);
	}
	protoname = what;	/*pp -> p_name;	this is static:  don't use it.*/
	protonumber = pp -> p_proto;
	switch (protonumber) {
		case IPPROTO_TCP:
			socktype = SOCK_STREAM;
			if (debug)
				fprintf(stderr, "%s SOCK_STREAM\n", protoname);
			break;
		case IPPROTO_UDP:
			socktype = SOCK_DGRAM;
			if (debug)
				fprintf(stderr, "%s SOCK_DGRAM\n", protoname);
			break;
		default:
			fprintf(stderr, "Unknown protocol:  %s\n", protoname);
			exit(1);
			break;
	}
	return(1);
}

int
getdiff(struct timehost *thishost)
{
	if (!internettime (thishost))
		return(0);
	thishost -> difference = thishost -> then.tv_sec - now.tv_sec;
	if (!rdate)
		printit(thishost);
	return(1);
}


/*
	Find the largest group of hosts which agree within the limit
	and return the first of that group.  If no two hosts agree,
	give up.
 */
 
struct timehost *
mungediffs(struct timehost *tophost)
{
	register struct timehost *thishost, *ahost, *goodhost;
	long diff;

	tophost--;	/* simplifies the comparisons */
	goodhost = &timehosts[0];
	for (thishost = &timehosts[0]; thishost < tophost; thishost++) {
		if (thishost -> bad)
			continue;
		thishost -> count = 1;
		if (verbose)
			printf ("%s", thishost -> hostname);
		for (ahost = thishost + 1; ahost <= tophost; ahost++) {
			if (thishost -> bad)
				continue;
			diff = ahost -> difference - thishost -> difference;
			if (abs(diff) < limit) {
				thishost -> count++;
				if (verbose)
					printf (" %s", ahost -> hostname);
			}
		}
		if (verbose) {
			printf (" %d\n", thishost -> count);
			(void)fflush(stdout);
		}
		if (thishost -> count > goodhost -> count)
			goodhost = thishost;
	}
	if (goodhost -> count > 1)
		return(goodhost);
	return(NULL);
}

int
getdate (struct timehost *thishost)
{
	int set = 0;

	if (!internettime (thishost))
		return (0);
	if (thishost -> local) {
		printf ("Local host %s has best time, so not setting date\n",
			hostname);
		printit(thishost);
		exit(0);
	}
	if (limit != 0
	&& abs((thishost -> then.tv_sec - now.tv_sec) - thishost -> difference)
	    > limit) {
		fprintf (stderr,
		"Time from %s has varied more than the limit of %d seconds\n",
			thishost -> hostname, limit);
		printit(thishost);
		exit(1);
	}
	if (settimeofday (&thishost -> then, (struct timezone *)0) == -1)
		perror ("netdate; settimeofday");
	else {
		int wf;
		if ((wf = open(WTMP, 1)) >= 0) {
			wtmp[0].ut_time = now.tv_sec;
			wtmp[1].ut_time = thishost -> then.tv_sec;
			(void)lseek(wf, 0L, 2);
			(void)write(wf, (char *)wtmp, sizeof(wtmp));
			(void)close(wf);
		}
		set = 1;
	}
	printit(thishost);
	return(set);
}

void
printit(struct timehost *thishost)
{
	extern char *ctime();
	struct tm *tp, *localtime();
	struct timeval diff;
	char newstring[128];

	if (rdate)
		printf ("%s", ctime((unsigned long *)&thishost -> then.tv_sec));
	else {
		(void)sprintf(newstring, "%s ", thishost -> hostname);
		tvsub(&diff, &thishost -> then, &now);
		printdiff(&newstring[strlen(newstring)], &diff);
		printf ("%-24s %.19s.%03d", newstring,
			ctime((unsigned long *)&thishost -> then.tv_sec),
				thishost -> then.tv_usec / 1000);
		if (verbose) {
			tp = localtime((unsigned long *)&thishost -> acked);
			printf(" at %02d:%02d:%02d.%03d",
				tp -> tm_hour, tp -> tm_min, tp -> tm_sec,
				thishost -> acked.tv_usec / 1000);
			tvsub(&diff, &thishost -> acked, &thishost -> asked);
			printdiff(newstring, &diff);
			printf(" delay %s", newstring);
		}
		printf("\n");
	}
	(void)fflush (stdout);
}

void
tvsub(tdiff, t1, t0)
struct timeval *tdiff, *t1, *t0;
{
	tdiff->tv_sec = t1->tv_sec - t0->tv_sec;
	tdiff->tv_usec = t1->tv_usec - t0->tv_usec;
	if (tdiff->tv_sec < 0 && tdiff->tv_usec > 0)
		tdiff->tv_sec++, tdiff->tv_usec -= 1000000;
	if (tdiff->tv_sec > 0 && tdiff->tv_usec < 0)
		tdiff->tv_sec--, tdiff->tv_usec += 1000000;
}

void
printdiff(char *where, struct timeval *diff)
{
	(void) sprintf (where, "%c%d.%.03d",
		(diff->tv_sec < 0 || diff->tv_usec < 0) ? '-' : '+',
		abs(diff->tv_sec), abs(diff->tv_usec) / 1000);
}

static	jmp_buf jb;
void
timeout()
{
	longjmp(jb, 1);
}

static int
internettime (struct timehost *thishost)
{
	register struct hostent *hp;
	struct sockaddr_in sin;
	long port;
	int nread;
	static int s = -1;

	if (thishost -> local) {
		if (gettimeofday (&now, (struct timezone *)0) == -1) {
			perror ("netdate: gettimeofday");
			exit (1);
		}
		thishost -> asked = now;
		thishost -> then = now;
		thishost -> acked = now;
		return(1);
	}
	timerclear(&thishost -> then);
	if (setjmp(jb))
		goto bad;
	(void)signal(SIGALRM, timeout);
	if (s != -1)
		(void) close (s), s = -1;
	port = getport(thishost -> protoname);
	bzero((char *)&sin, sizeof (sin));
	sethostent(1);
	if ((hp = gethostbyname(thishost -> hostname)) == NULL) {
		fprintf(stderr, "%s: %s: unknown host\n",
			whoami, thishost -> hostname);
		goto out;
	}
	sin.sin_family = hp->h_addrtype;
	(void)alarm(20);
	s = socket(hp->h_addrtype, thishost -> socktype, 0 /*protonumber*/);
	if (s < 0) {
		perror("netdate: socket");
		(void)alarm(0);
		goto out;
	}
	if (thishost -> socktype == SOCK_STREAM) {
		if (bind(s, (struct sockaddr *)&sin, sizeof (sin)) < 0) {
			perror("netdate: bind");
			goto bad;
		}
	}
	bcopy(hp->h_addr, (char *)&sin.sin_addr, hp->h_length);
	sin.sin_port = port;
	(void)gettimeofday (&thishost -> asked, (struct timezone *)0);
	if (connect(s, (struct sockaddr *)&sin, sizeof (sin)) < 0) {
		perror("netdate: connect");
		goto bad;
	}
	if (thishost -> socktype == SOCK_DGRAM) {
		if (write (s, "\n", 1) < 0) {
			perror ("netdate: send");
			goto bad;
		}
	}
	nread = read (s, (char *)&thishost -> then, sizeof (thishost -> then));
	(void)gettimeofday (&thishost -> acked, (struct timezone *)0);
	(void)alarm(0);
	now = thishost -> acked;
	if (nread < sizeof(thishost -> then.tv_sec)) {
		perror ("netdate: read");
		goto bad;
	}
	/* RFC 868 only allows seconds, but what the hell */
	if (nread == sizeof(thishost -> then))
		thishost -> then.tv_usec = ntohl(thishost -> then.tv_usec);
	else
		thishost -> then.tv_usec = 0L;
	thishost -> then.tv_sec = ntohl (thishost -> then.tv_sec) - NETBASE;
	return (1);	/* don't close before returning to avoid delays */
bad:
	(void)alarm(0);
	(void) close (s), s = -1;
out:
	if (gettimeofday (&now, (struct timezone *)0) == -1) {
		perror ("netdate: gettimeofday");
		exit (1);
	}
	thishost -> asked = now;
	thishost -> then = now;
	thishost -> acked = now;
	thishost -> bad = 1;
	fprintf (stderr, "Connection with %s to %s failed.\n",
		thishost -> protoname, thishost -> hostname);
	return(0);
}

long
getport(char *protoname)
{
	register struct servent *sp;
	static long port;

	if (port != 0)
		return(port);
	if ((sp = getservbyname(service, protoname)) == 0) {
		fprintf(stderr, "%s: %s/%s: unknown service\n",
			whoami, service, protoname);
		exit(1);
	}
	return (port = sp->s_port);
}
