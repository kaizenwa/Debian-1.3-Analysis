/*
 * snntpd - barebones nntpd that only understands transport reception.
 *	Mark Moraes, University of Toronto
 *
 * "NNTP newsreaders make me and my machine ill"
 *				- Ian Dickinson, vato@csv.warwick.ac.uk
 *
 * TODO:
 *	Timeouts.
 *	No options supported yet, don't bother editing config.h.
 *
 * Access control could be elaborated endlessly, but it's only netnews.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "msgid.h"

#include "libcnews.h"
#include "config.h"
#include "fgetfln.h"
#include "history.h"

#include "batch.h"
#include "msgs.h"
#include "debug.h"
#include "netdata.h"
#include "log.h"

#ifndef F_OK
#define F_OK 0
#endif
#ifndef W_OK
#define W_OK 2		/* not actually used */
#endif

#define NULLSTR		((char *) NULL)
#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)
#define CISTREQN(a,b,n)	(cistrncmp((a), (b), (n)) == 0)

/* NOTE: x *MUST* be an array or you'll only get 3 chars */
#define SAY(x, y) \
	if (net_ackwrite((x), sizeof (x) - 1, (y), stdout) == 0) \
		; \
	else \
		unlock(), error("net_ackwrite(%s) failed", (x))

/* imports */
extern char *mktemp();
void postarticle(/* FILE *fp,  struct netdata *ndp */);

/* forwards */
static void process(/* FILE *in */);
static char *munge_msgid();
static void readarticle(/* FILE *fp, char *msgid, struct netdata *ndp */);
static void sendarticle(/* char *msgid */);

/* for getopt */
extern int optind;
extern char *optarg;
extern int errno;

extern char *getrealpeername();

char *progname;
unsigned int debug;
FILE *dfp;
long artmax = ARTMAX;	/* toss articles bigger than this size */

int sendarts, sendnope, postarts, postfail;
static char *hostlock;

/*
 * main - parse arguments and handle options
 */
int
main(argc, argv)
int argc;
char *argv[];
{
	int c, errflg = 0;
	char *hname;
	extern long spacefor();

	progname = argv[0];
	dfp = stderr;
	while ((c = getopt(argc, argv, "dD:")) != EOF) {
		switch (c) {
		case 'd':
			debug++;
			break;
		case 'D':
			dfp = efopen(optarg, "a+");
			break;
		default:
			errflg++;
			break;
		}
	}

	sendarts = postarts = sendnope = postfail = 0;
	hname = getrealpeername(0);
	if (hname != NULL)
		hostsetup(hname);
	if (errflg || optind < argc) {
		(void) fprintf(stderr, "usage: %s [-d] [-D debugfile]\n",
			       progname);
		unlock();
		exit(1);
	}

	(void) signal(SIGPIPE, SIG_IGN);	/* we check write returns */
	if (access(ctlfile("LOCKnntp"), F_OK) == 0) {
		dprintf(dfp, "%s seen\n", ctlfile("LOCKnntp"));
		SAY(NNTP_DENIED, "NNTP reception temporarily disabled.");
		unlock();
		exit(0);
	}

	ddprintf(dfp, "chdir(%s)\n", artfile(NULLSTR));
	cd(artfile(NULLSTR));
	if (spacefor(1L, artfile("in.coming"), 5000L, 1000L, 250000L) <= 0) {
		sleep(10);		/* defeat rabid feeders */
		SAY(NNTP_DENIED, "out of space; try again later");
		unlock();
		exit(0);
	}

	SAY(NNTP_HI, NULLSTR);
	process(stdin);
	batchend();
	if (sendarts > 0 || sendnope > 0)
		log3int("sent %d missed %d articles", sendarts, sendnope, 0);
	if (postarts > 0 || postfail > 0)
		log3int("posted %d failed %d postings", postarts, postfail, 0);
	unlock();
	return 0;
}

hostsetup(hname)
char *hname;
{
	int fd, ret, ok = 1;
	char *temp, *line = NULL;
	FILE *fp;

	/* must be a socket, so we syslog and errlog */
	loginit("snntpd");
	logstr = strsave(hname);
	if (freopen(ctlfile(LOGFILE), "a+", stderr) == NULL) {
		SAY(NNTP_EH, "failed to open error log file");
		exit(1);
	}

	/* see if this host is allowed to send us netnews */
	fp = fopen(ctlfile("nntp.allow"), "r");
	if (fp != NULL) {
		ok = 0;
		while ((line = fgetln(fp)) != NULL) {
			trim(line);
			if (STREQ(line, hname) || domainmatch(line, hname)) {
				ok++;
				break;
			}
		}
		(void) fclose(fp);
	}
	if (!ok) {			/* don't know this guy? */
		/* 
		 * some scurvy vermin is trying to read news with NNTP,
		 * so kick off the reference nntpd (if any) to deal with him;
		 * we don't dirty our hands with such nonsense.
		 */
		(void) execl("in.nntpd", "/usr/etc/in.nntpd", (char *)NULL);
		(void) execl("nntpd", "/usr/etc/nntpd", (char *)NULL);
		SAY(NNTP_EH, "sorry, we don't allow news reading via NNTP.");
		exit(1);
	}

	/* see if this host is already connected for transfer */
	temp = strsave(ctlfile("L.XXXXXX"));
	(void) mktemp(temp);
	fd = creat(temp, 0444);
	if (fd < 0) {
		SAY(NNTP_EH, "can't make temporary");
		exit(1);
	}
	(void) close(fd);

	hostlock = str3save(ctlfile("LOCK."), "", hname);
	ret = link(temp, hostlock);
	(void) unlink(temp);
	if (ret < 0) {
		sleep(10);		/* slow down rabid feeders */
		SAY(NNTP_DENIED,	/* piss off, ya tit */
	"NNTP transfer from your machine already in progress, greedy guts.");
		exit(1);
	}
	free(temp);
	/* we have the lock for this host now; remove at exit */
}

static int
domainmatch(dom, host)		/* is dom (.dom.ain) the end of host? */
register char *dom, *host;
{
	register int domlen, hostlen;

	if (dom[0] != '.')
		return 0;
	domlen = strlen(dom);
	hostlen = strlen(host);
	if (hostlen <= domlen)
		return 0;
	return STREQ(dom, &host[hostlen-domlen]);
}

unlock()
{
	if (hostlock != NULL)
		(void) unlink(hostlock);
}

/*
 * process - process input file
 */
static void
process(in)
FILE *in;
{
	register char *line;
	sizeint len;
	struct netdata *ndp;

	ndp = net_new();
	while ((line = net_cmdread(in, &len)) != NULL)
		if (CISTREQN(line, "ihave", 5)) {
			line = munge_msgid(line + 5);
			readarticle(in, line, ndp);
		} else if (CISTREQN(line, "quit", 4)) {
			SAY(NNTP_BYE, NULLSTR);
			return;
		}
#ifdef READING
		else if (CISTREQN(line, "article", 7)) {
			line = munge_msgid(line + 7);
			sendarticle(line);
		} else if (CISTREQN(line, "post", 4)) {
			if (line[4] != '\0')
				SAY(NNTP_EH, NULLSTR);
			else
				postarticle(in, ndp);
		}
#endif						/* READING */
		else
			SAY(NNTP_EH, NULLSTR);
	/* only get here if we hit an error on stdin or EOF */
	unlock();
	error("abruptly terminated", "");
}

/*
 * Skip leading whitespace, lowercase the hostpart.
 */
static char *
munge_msgid(s)
register char *s;
{
	while (isascii(*s) && isspace(*s))
		s++;
	s = rfc822ize(s);
	ddprintf(dfp, "message-id is `%s'\n", s);
	return s;
}

static void
readarticle(fp, msgid, ndp)
FILE *fp;
char *msgid;
struct netdata *ndp;
{
	char *histent = histlook(msgid);

	if (histent != NULL || msgidseen(msgid)) {	/* got msgid already? */
/*		ddprintf(dfp, "histent = `%s'\n", histent);	*/
		SAY(NNTP_NEXT, NULLSTR);
		bt_rej++;
	} else {
		SAY(NNTP_WANT, NULLSTR);
		if (net_getdata(fp, ndp) != 0 ||
		    batchadd(ndp->nd_buf, ndp->nd_bufcnt, ndp->nd_bytes,
			     ndp->nd_fp, ndp->nd_spilled) != 0) {
			SAY(NNTP_FAIL, NULLSTR);
			bt_fail++;
		} else
			SAY(NNTP_OK, NULLSTR);
	}
}

int
msgidseen(id)		/* has msgidd seen this id? */
char *id;
{
	char *idcopy = strsave(id);
	int ret = msgid(idcopy, MADD) != 0;

	free(idcopy);
	return ret;
}

static void
sendarticle(msgid)
char *msgid;
{
	char *histent = histlook(msgid), *cp;
#define MAXFIELDS 3
	char *fields[MAXFIELDS];
	FILE *fp;
	int len, nf;

	if (histent == NULL) {
		SAY(NNTP_NO, NULLSTR);
		sendnope++;
		return;
	} else {
		nf = split(histent, fields, MAXFIELDS, "\t");
		if (nf < 3) {
			ddprintf(dfp, "history entry has %d fields\n", nf);
			SAY(NNTP_NO, NULLSTR);
			sendnope++;
			return;
		}
		ddprintf(dfp, "files = `%s'\n", fields[2]);
		cp = histslash(strtok(fields[2], " "));
		ddprintf(dfp, "first file = `%s'\n", cp);
		if (cp == NULL || *cp == '\0' ||(fp = fopen(cp, "r")) == NULL) {
			SAY(NNTP_NO, NULLSTR);
			sendnope++;
			return;
		}
		SAY(NNTP_HERE, fields[0]);
		while ((cp = fgetline(fp, &len)) != NULL) {
			if (cp[--len] == '\n')
				cp[len] = '\0';
			else
				len++;	/* shouldn't happen */
			if (net_writeline(cp, len, stdout) != 0) {
				unlock();
				error("net_writeline(%s) failed", cp);
			}
		}
		if (net_endwrite(stdout) != 0) {
			unlock();
			error("net_endwrite(stdout) failed", "");
		}
		sendarts++;
		(void) fclose(fp);
	}
}
