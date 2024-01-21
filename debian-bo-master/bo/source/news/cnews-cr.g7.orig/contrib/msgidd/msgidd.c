/*
 * msgidd -- message ID daemon
 * vix 24may90 [written]
 * with mods ken@sdd.hp.com 01jul90
 * speedups and hiding 4BSD network goo by Geoff Collyer, 1992
 *
 * $Header: msgidd.c,v 1.8 91/05/21 19:47:48 vixie Locked $
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/time.h>
#ifdef hpux
#include <sys/param.h>
#include <libBSD.h>
#endif

#define NEEDMSGS
#include "msgid.h"
#include "ipc.h"

#define ASSERT(e, m) if (!(e)) {fputs("assert failed... ", stderr);\
				perror(m); exit(1);}

#define STREQ(s1, s2)		(*(s1) == *(s2) && strcmp(s1, s2) == 0)
#define STRN_EQ(s1, s2, n)	(*(s1) == *(s2) && strncmp(s1, s2, n) == 0)

#define FLAGS_RESETLOG	0x02
#define FLAGS_FLUSHLOG	0x04

#define MAX_AGE		10
#define ALARM_TIME	300

#define HASHSIZE 1024

#if 0
#define dprintf fprintf
#else
#define dprintf (void)
#endif

/* imports */
char *malloc();
time_t time();
extern int errno;
extern char *optarg;
extern int optind;

int log = 0, flags = 0;
time_t hold_time = MAX_AGE *60;
char *hosts[100], *lfn, *ptime();
FILE *logfp = NULL;

static int transaction();
int onpipe();

struct {
	int	connected;
	int	connects, drops;
	int	new, dup, cancel;
	int	freed;
} stats;

struct el {
	struct el *next;
	time_t age;
	int	refcnt;
	char	id[1];
} *ids[HASHSIZE];

char	*months[12] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

static unsigned	
mkhash(str)
char *str;
{
	register unsigned char *s = (unsigned char *)str;
	register unsigned hash = 0;
	register unsigned char c;

	while ((c = *s++) != '\0')
		hash += c;
	return hash;
}

static char *
strdup(x)
char *x;
{
	register char *y = malloc(strlen(x) + 1);

	if (y)
		(void) strcpy(y, x);
	return y;
}

static void
savepid()
{
	FILE *pidfp;

	if ((pidfp = fopen(PIDFILE, "w")) == NULL)
		return;
	(void) fprintf(pidfp, "%d\n", getpid());
	(void) fclose(pidfp);
}

char *
ptime(now)
time_t now;
{
	register struct tm *tmp = localtime(&now);
	static char buf[50];

	(void) sprintf(buf, "%s %2d %02d:%02d:%02d",  months[tmp->tm_mon],
		tmp->tm_mday, tmp->tm_hour,  tmp->tm_min, tmp->tm_sec);
	return buf;
}

static void 
usage(me)
char *me;
{
	(void) fprintf(stderr, "usage: %s [-s socket][-l log][-h mins]\n", me);
	exit(1);
}

static void
openlogfile()
{
	if (logfp)
		(void) fclose(logfp);
	if (log && (logfp = fopen(lfn, "a+")) == NULL) {
		syslog(LOG_ERR, "Unable to open %s: %m", lfn);
		log = 0;
		logfp = NULL;
	}
}

static void
bye()
{
	if (log)
		(void) fclose(logfp);
	dprintf(stderr, "Bye !!\n");
	exit(0);
}

static void
resetlog() 
{
	flags |= FLAGS_RESETLOG;
}

static void 
pstats()
{
	char msgbuf[1024];

	if (log)
		flags |= FLAGS_FLUSHLOG;
	sprintf(msgbuf,
"stats: %d connected, %d connects, %d drops, %d dups, %d new, %d cancel, %d freed\n",
		stats.connected, stats.connects, stats.drops, stats.dup,
		stats.new, stats.cancel, stats.freed);
	dprintf(stderr, "%s\n", msgbuf);
	syslog(LOG_INFO, msgbuf);
	stats.connects = stats.drops = stats.new = stats.cancel = stats.dup =
		stats.freed = 0;
	alarm(ALARM_TIME);
}

main(argc, argv)
int argc;
char *argv[];
{
	register char *sn = SOCKNAME;
	register int s;
	int highest_fd;
	fd_set clients;

	while ((s = getopt(argc, argv, "l:h:s:")) != EOF)
		switch (s) {
		case 'h':
			hold_time = 60 * atoi(optarg);
			if (hold_time <= 0 || hold_time > (24 * 3600))
				usage(argv[0]);
			break;
		case 's':
			sn = strdup(optarg);
			break;
		case 'l':
			log++;
			lfn = strdup(optarg);
			break;
		default:
			usage(argv[0]);
			break;
		}

	if (optind != argc)
		usage(argv[0]);

	if (log) {
		openlogfile();
		if (!log) {
			(void) fprintf(stderr, "%s: Unable to open log file (%s)\n",
				       argv[0], lfn);
			exit(1);
		}
	}

	savepid();

#ifdef LOG_DAEMON
	openlog("msgidd", LOG_PID, LOG_DAEMON);
#else
	openlog("msgidd", LOG_PID);
#endif

	(void) unlink(sn);
	highest_fd = s = ipccreat(ipcpath("localhost", "unix", sn), "");
	ASSERT(s >= 0, "socket");
	FD_ZERO(&clients);

	if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
		signal(SIGHUP, resetlog);
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, bye);
	signal(SIGUSR1, bye);		/* for profiling, etc. */
	signal(SIGALRM, pstats);
	signal(SIGPIPE, onpipe);
	alarm(ALARM_TIME);

	for (; ; ) {
		register int nfound, fd;
		fd_set readfds;

		if (flags) {
			if (flags & FLAGS_FLUSHLOG)
				(void) fflush(logfp);
			if (flags & FLAGS_RESETLOG)
				openlogfile();
			flags = 0;
		}
		readfds = clients;	/* we want to select the clients... */
		FD_SET(s, &readfds);	/* ...and the main server socket. */
		nfound = select(highest_fd+1, &readfds, (fd_set *)NULL,
			(fd_set *)NULL, NULL);
		if (nfound < 0 && errno == EINTR)
			continue;
		ASSERT(0 <= nfound, "select");
		for (fd = 0; fd <= highest_fd; fd++)
			if (FD_ISSET(fd, &readfds))
				if (fd == s)
					fd = newconn(s, &highest_fd, &clients);
				else if (FD_ISSET(fd, &clients)) {
					/* process message from `fd' */
					if (transaction(fd) == -1)
						lostconn(fd, &clients);
				} else {
					dprintf(stderr,
						"Bad fd %d from select\n", fd);
				}
	}
}

int
newconn(s, hifdp, clntp)
int s;
register int *hifdp;
fd_set *clntp;
{
	register ipcinfo *ip = ipclisten(s);
	register int fd = (ip != NULL? ipcaccept(ip): -1);

	if (fd == -1)
		syslog(LOG_ERR, "Accept failed: %m");
	else {
		FD_SET(fd, clntp);
		if (fd > *hifdp)
			*hifdp = fd;
		stats.connects++;
		stats.connected++;
	}
	return fd;
}

lostconn(fd, clntp)
register int fd;
fd_set *clntp;
{
	(void) close(fd);
	FD_CLR(fd, clntp);
	stats.connected--;
	stats.drops++;
	if (hosts[fd]) {
		if (log)
			(void) fprintf(logfp, "%s Disconnect %s\n",
				ptime(time((time_t * )NULL)), hosts[fd]);
		dprintf(stderr, "Disconnect(%d/%s)\n", fd, hosts[fd]);
		free(hosts[fd]);
		hosts[fd] = NULL;
	}
}

int sigpiped;

static int	
reply(fd, ans)
register int fd, ans;
{
	int status;

	sigpiped = 0;
	while ((status = write(fd, (ans ? "\001" : "\000"), 1)) < 0 &&
	    errno == EINTR && !sigpiped)
		;
	if (status < 0) {
		if (log) {
			register time_t now = time((time_t *)NULL);

			(void) fprintf(logfp, "%s write failed (fd %d)\n",
				ptime(now), fd);
		}
		syslog(LOG_ERR, "write failed, closing connection: %m");
		return -1;
	}
	return 0;
}

struct el *
delelnxt(elp, prev, hash)
register struct el *elp, *prev;
unsigned hash;
{
	register struct el *nxt = elp->next;

	if (prev == NULL)
		ids[hash] = nxt;
	else
		prev->next = nxt;
	free((char *)elp);
	return nxt;
}

static void
cancel(fd, bufp, now)
register int fd;
register char *bufp;
time_t now;
{
	register struct el *i, *p;
	register unsigned found = 0, hash = mkhash(bufp) % HASHSIZE;

	for (i = ids[hash], p = NULL; i != NULL; p = i, i = i->next)
		if (STREQ(i->id, bufp)) {
			found++;
			break;
		}
	if (found) {
		if (log)
			(void) fprintf(logfp, "%s Cancel %s %s\n",
			   ptime(now), hosts[fd]? hosts[fd]: "", i->id);
		dprintf(stderr, "Cancel(%d/%s): `%s'\n", fd, hosts[fd], bufp);
		(void) delelnxt(i, p, hash);
		stats.cancel++;
	} else {
		if (log)
			(void) fprintf(logfp, "%s Error cancel %s %s\n",
				ptime(now), hosts[fd]? hosts[fd]: "", bufp);
		dprintf(stderr, "Bad-cancel(%d/%s): `%s'\n", fd, hosts[fd],
			bufp);
		syslog(LOG_ERR, "Cancel, %s not found", bufp);
	}
}

static void
add(fd, bufp, n, now)
register int fd;
register char *bufp;
register int n;
time_t now;
{
	/*
	 * this malloc includes the id[1] array, which means that there's
	 * already room for strcpy's null
	 */
	register struct el *i = (struct el *)malloc(sizeof(struct el) + n);
	register int hash = mkhash(bufp) % HASHSIZE;

	if (i == NULL)
		return;
	i->age = now;
	(void) strcpy(i->id, bufp);
	if (log) {
		i->refcnt = 1;
		(void) fprintf(logfp, "%s Add %s %s\n", ptime(now),
			       hosts[fd]? hosts[fd]: "", bufp);
	}
	i->next = ids[hash];
	ids[hash] = i;
	stats.new++;
	dprintf(stderr, "Add(%d/%s): `%s'\n", fd, hosts[fd], bufp);
}

static int	
search(fd, bufp, now)
register int fd;
register char *bufp;			/* no leading '<' */
time_t now;
{
	register struct el *i, *p;
	register int searched = 0;
	register unsigned hash = mkhash(bufp) % HASHSIZE;
	register int found = 0, tooold = 0;

	/* search the appropriate list. */
	for (i = ids[hash], p = NULL; i != NULL; p = i, i = i->next) {
		/*
		 * if too old, everything from here to the end can be nuked
		 * (we always add at the top).
		 */
		if ((now - i->age) > hold_time) {
			tooold++;
			break;
		}
		searched++;
		if (STREQ(i->id, bufp)) {
			found++;
			break;
		}
	}
	if (tooold)
		/*
		 * everything from here to the end can be nuked
		 * (we always add at the top).
		 */
		while (i != NULL) {
			i = delelnxt(i, p, hash);
			stats.freed++;
		}
	else if (found) {
		if (log) {
			i->refcnt++;
			(void) fprintf(logfp, "%s Lose %s %d %ld %s\n",
				ptime(now), hosts[fd]? hosts[fd]: "",
				i->refcnt,  (now - i->age), i->id);
		}
		stats.dup++;
	}
	dprintf(stderr, "Search(%d/%s): %s(%d) `%s'\n", fd, hosts[fd],
		(found? "dup": "new"), searched, bufp);
	return found;
}

/*
 * returns: -1 == client is gone, close this fd please; 0 == success
 */
static int	
transaction(fd)
register fd;
{
	register int n;
	register char *bufp, *cmdp;
	register time_t now = time((time_t *)NULL);
	char buf[1023];

	/* read the request.  zero-length read means connection is gone. */
	do {
		n = read(fd, buf, sizeof buf);
		if (n == 0)
			return -1;
	} while (n < 0 && errno == EINTR);
	ASSERT(n > 0, "read");

	if (hosts[fd]) {
		dprintf(stderr, "Parse(%d/%s): `%s'\n", fd, hosts[fd], buf);
	}

	/* Separate cmd from id */
	cmdp = buf;
	bufp = buf + 4;
	n -= 4;

	/* find the first useful character, saving it and its address. */
	if (*bufp == '<') {
		bufp++;
		n--;
	}

	/* rip out useless characters at end, remembering real length. */
	while (n > 0) {
		register int x = n -1;
		register int ch = bufp[x];

		if (ch == '\n' || ch == '\r' || ch == '>')
			n = x;
		else
			break;
	}
	bufp[n] = '\0';

	/* Which cmd ? */
	if (STRN_EQ(cmdp, msgs[MCANCEL], 4)) {
		cancel(fd, bufp, now);
		return reply(fd, 0);
	} else if (STRN_EQ(cmdp, msgs[MADD], 4)) {
		if (search(fd, bufp, now))
			return reply(fd, 1);
		else {
			add(fd, bufp, n, now);
			return reply(fd, 0);
		}
	} else if (STRN_EQ(cmdp, msgs[MOLD], 4)) {
		if (log)
			(void) fprintf(logfp, "%s Old %s %s\n", ptime(now),
				       hosts[fd]? hosts[fd]: "", bufp);
		dprintf(stderr, "Old(%d/%s): `%s'\n", fd, hosts[fd], bufp);
		return reply(fd, 0);
	} else if (STRN_EQ(cmdp, msgs[MHOST], 4)) {
		if (hosts[fd])
			free(hosts[fd]);
		hosts[fd] = strdup(bufp);
		if (log)
			(void) fprintf(logfp, "%s Connect %s\n", ptime(now),
			     hosts[fd]);
		dprintf(stderr, "Connect(%d/%s)\n", fd, hosts[fd]);
		return reply(fd, 0);
	} else {
		syslog(LOG_ERR, "Unknown command %s", cmdp);
		if (log)
			(void) fprintf(logfp, "%s Error %s unknown-cmd %s\n",
				       ptime(now),  hosts[fd], cmdp);
		dprintf(stderr, "Error(%d/%s) unknown-cmd %s\n", fd,
		     hosts[fd], cmdp);
		return reply(fd, 0);
	}
#ifdef lint
	/*NOTREACHED*/
	return 0;
#endif
}

onpipe()
{
	register time_t now = time((time_t *)NULL);

	if (log)
		fprintf(logfp, "%s Got SIGPIPE\n", ptime(now));
	sigpiped++;
}
