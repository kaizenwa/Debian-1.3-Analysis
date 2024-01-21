/*
 * expire - expire old news
 *
 * One modest flaw:  links are not preserved in archived copies, i.e. you
 * get multiple copies of multiply-posted articles.  Since link preservation
 * is arbitrarily hard when control files get complex, to hell with it.
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include "fixerrno.h"
#include <time.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/stat.h>
#include "libc.h"
#include "news.h"
#include "config.h"
#include "fgetfln.h"
#include "case.h"
#include "dbz.h"
#include "ngmatch.h"

/* basic parameters of time, used in back() (and debugging code) */
#ifndef EPOCH
#define	EPOCH	((time_t)0)	/* the origin of time_t */
#endif
#ifndef FOREVER
#define	FOREVER	1e37		/* FOREVER > max value of time_t */
#endif
#define	DAY	((double)24*60*60)

/* structure for expiry-control records */
struct ctl {
	struct ctl *next;
	char *groups;		/* newsgroups */
	NGPAT *pat;		/* parsed "groups", if actually a pattern */
	int ismod;		/* moderated? */
#		define	UNMOD	'u'
#		define	MOD	'm'
#		define	EITHER	'x'
	time_t retain;		/* earliest arrival date not expired */
	time_t normal;		/* earliest not expired in default case */
	time_t purge;		/* latest arrival date always expired */
	char *dir;		/* Archive dir or NULL. */
	long ngroups;		/* for explist, number of groups dealt with */
	int lineno;		/* line number in explist */
};

/* header for internal form of control file */
struct ctl *ctls = NULL;
struct ctl *lastctl = NULL;

/*
 * Headers for lists by newsgroup, derived (mostly) from active file.
 * Hashing is by length of newsgroup name; this is quick and works well,
 * and there is no simple variation that does much better.  (Actually,
 * that statement is obsolete; there are now too many newsgroups for
 * this to work particularly well, although it's not a major profiling
 * hot spot.  Improvements are planned.)
 */
#define	NHASH	80
struct ctl *ngs[NHASH] = { NULL };

struct ctl *holdover = NULL;	/* "/expired/" control record */
struct ctl *bounds = NULL;	/* "/bounds/" control record */

int debug = 0;			/* for inews routines */
int expdebug = 0;		/* expire debugging */

int printexpiring = 0;		/* print info line for expiring articles? */
char *defarch = NULL;		/* default archive dir */
int spacetight = 0;		/* error-recovery actions remove evidence? */

char *subsep = "~";		/* subfield separator in middle field */
int checkonly = 0;		/* check control information only */
int testing = 0;		/* testing only, leave articles alone */
int leaders = 0;		/* only first link ("leader") is hard link */
int verbose = 0;		/* report statistics */
int rebuild = 1;		/* rebuild history files */
int holdarch = 0;		/* hold rather than archiving */
int dategrump = 0;		/* report incomprehensible expiry dates */
char *histdir = NULL;		/* where to find history files */

long nkept = 0;			/* count of articles not expired */
long ngone = 0;			/* count of articles removed (no links left) */
long nresid = 0;		/* count of residual entries kept */
long narched = 0;		/* count of links archived */
long njunked = 0;		/* count of links just removed */
long nmissing = 0;		/* count of links missing at cp/rm time */

long ncpfail = 0;		/* count of failed cp()s */

char dont[] = "don't";		/* magic cookie for whereexpire() return */

time_t now;			/* set once in startup, as reference */
#define	NODATE	((time_t)(-1))	/* time_t value indicating date not given */
time_t latest =	0;		/* most recent arrival date */

char subject[200] = "";		/* Subject line for -p, minus header */

/* Buffer etc. for readline and friends. */
#ifdef SMALLMEM
#define	RLBSIZ	(BUFSIZ)
#else
#define	RLBSIZ	(BUFSIZ*8)	/* we're going to read a biiiig file */
#endif
char rlbuf[RLBSIZ+1];		/* +1 for sentinel */
size_t rlbufsiz = RLBSIZ;
int rlnleft = 0;
char *rest = rlbuf;		/* relies on rlbuf initialized to '\0's */
int nlocked = 0;		/* has readline() locked the news system? */
char *rlline = NULL;		/* malloced when we need to copy a line */
size_t rllsiz = 200;		/* good initial size */

/*
 * Archive-copying buffer.
 * 8KB buffer is large enough to take most articles at one gulp,
 * and also large enough for virtual certainty of getting the
 * Subject: line in the first bufferload.
 */
#ifdef SMALLMEM
char abuf[2*1024];		/* expire reported to be tight on 11 */
#else
char abuf[8*1024];
#endif

char *progname;

extern struct tm *gmtime();
extern time_t time();

extern time_t getindate();

/* forwards */
FILE *eufopen();
void eufclose();
char *whereexpire();
time_t back();
void checkadir();
void fail();
void die();
void control();
void prime();
void checkused();
void doit();
void cd();
time_t readdate();
char *doarticle();
void warning();
void complain();
void printstuff();
int expire();
char *readline();
void mkparents();
void getsubj();
void refill();
void printlists();
void pctl();
void fillin();
void ctlline();

/*
 - main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	register int c;
	register int errflg = 0;
	register FILE *cf;
	extern int optind;
	extern char *optarg;

	progname = argv[0];
	now = time((time_t *)NULL);

	while ((c = getopt(argc, argv, "pa:sF:cn:tlvrhgH:B:d")) != EOF)
		switch (c) {
		case 'p':	/* print info line for archived articles */
			printexpiring = 1;
			break;
		case 'a':	/* archive in this directory */
			defarch = optarg;
			break;
		case 's':	/* maximize space during error recovery */
			spacetight = 1;
			break;
		case 'F':	/* subfield separator in middle field */
			subsep = optarg;
			break;
		case 'c':	/* check control-file format only */
			checkonly = 1;
			break;
		case 'n':	/* set value of "now" for testing */
			now = atol(optarg);
			break;
		case 't':	/* testing, do not mess with articles */
			testing = 1;
			break;
		case 'l':	/* leaders */
			leaders = 1;
			break;
		case 'v':	/* verbose -- report some statistics */
			verbose = 1;
			break;
		case 'r':	/* suppress history-file rebuild */
			rebuild = 0;
			break;
		case 'h':	/* hold all files meant to be archived */
			holdarch = 1;
			break;
		case 'g':	/* report incomprehensible expiry dates */
			dategrump = 1;
			break;
		case 'H':	/* where to find history files */
			histdir = optarg;
			break;
		case 'B':	/* internal buffer size */
			rlbufsiz = atoi(optarg);
			break;
		case 'd':	/* debug */
			expdebug = 1;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg || optind < argc-1) {
		fprintf(stderr, "Usage: %s [-p] [-s] [-c] [-a archdir] [ctlfile]\n",
								progname);
		exit(2);
	}
	if (expdebug)
		setbuf(stderr, (char *)NULL);

	if (optind < argc) {
		cf = eufopen(argv[optind], "r");
		control(cf);
		(void) fclose(cf);
	} else
		control(stdin);
	prime(ctlfile("active"));

	if (expdebug)
		printlists();
	if (histdir == NULL)
		histdir = strsave(ctlfile((char *)NULL));
	if (defarch != NULL)
		checkadir(defarch);
	if (checkonly)
		exit(0);


	(void) umask(newsumask());
	doit();			/* side effect: newslock() */
	newsunlock();

	if (latest > time((time_t *)NULL)) {
		complain("some article arrival dates are in the future!", "");
		complain("\tis your system clock set wrong?", "");
	}

	if (verbose) {
		fprintf(stderr, "%ld kept, %ld expired\n", nkept, ngone);
		fprintf(stderr, "%ld residual lines\n", nresid);
		fprintf(stderr, "%ld links archived, %ld junked, %ld missing\n",
						narched, njunked, nmissing);
	}
	exit(0);
}

/*
 - control - pick up a control file
 */
void
control(f)
register FILE *f;
{
	register char *p;
	register int gotone = 0;
	register int lineno = 0;

	while ((p = fgetline(f, (size_t *)NULL)) != NULL) {
		lineno++;
		if (*p != '#')
			ctlline(p, lineno);
		gotone = 1;
	}

	if (!gotone)
		die("control file empty!", "");
}

/*
 - ctlline - process one control-file line
 */
void
ctlline(ctl, lineno)
char *ctl;
int lineno;
{
	register struct ctl *ct;
	char *field[4];
	char datebuf[50];
	char *dates[3];
	register int nf;
	int ndates;

	nf = split(ctl, field, 4, "");
	if (nf == 0)
		return;		/* blank line */
	if (nf != 4)
		die("control line for `%s' hasn't got 4 fields", field[0]);

	ct = (struct ctl *)malloc(sizeof(struct ctl));
	if (ct == NULL)
		fail("out of memory for control list", "");

	ct->groups = strsave(field[0]);
	ct->pat = ngparse(strsave(ct->groups));
	if (ct->pat == NULL)
		die("can't parse control file pattern `%s'", ct->groups);
	if (STREQ(field[1], "m"))
		ct->ismod = MOD;
	else if (STREQ(field[1], "u"))
		ct->ismod = UNMOD;
	else if (STREQ(field[1], "x"))
		ct->ismod = EITHER;
	else
		die("strange mod field `%s' in control file", field[1]);

	if (strlen(field[2]) > sizeof(datebuf)-1)
		die("date specification `%s' too long", field[2]);
	(void) strcpy(datebuf, field[2]);
	ndates = split(datebuf, dates, 3, "-");
	switch (ndates) {
	case 3:
		ct->retain = back(dates[0]);
		ct->normal = back(dates[1]);
		ct->purge = back(dates[2]);
		break;
	case 2:
		ct->retain = (bounds != NULL) ? bounds->retain : back("0");
		ct->normal = back(dates[0]);
		ct->purge = back(dates[1]);
		break;
	case 1:
		ct->retain = (bounds != NULL) ? bounds->retain : back("0");
		ct->normal = back(dates[0]);
		ct->purge = (bounds != NULL) ? bounds->purge : back("never");
		break;
	default:
		die("invalid date specification `%s'", field[2]);
		/* NOTREACHED */
		break;
	}
	if (ct->retain < ct->normal && ndates <= 2)	/* stretch defaults */
		ct->retain = ct->normal;
	if (ct->normal < ct->purge && ndates == 1)
		ct->purge = ct->normal;

	if (ct->retain < ct->normal || ct->normal < ct->purge)
		die("preposterous dates: `%s'", field[2]);

	if (STREQ(field[3], "-"))
		ct->dir = NULL;
	else if (STREQ(field[3], "@")) {
		if (defarch == NULL)
			die("@ in control file but no -a", "");
		ct->dir = defarch;
	} else {
		ct->dir = strsave(field[3]);
		checkadir(ct->dir);
	}

	ct->ngroups = 0;
	ct->lineno = lineno;

	/* put it where it belongs */
	if (STREQ(ct->groups, "/expired/"))
		holdover = ct;
	else if (STREQ(ct->groups, "/bounds/"))
		bounds = ct;
	else if (ct->groups[0] == '/')
		die("unknown special line name `%s'", ct->groups);
	else {
		ct->next = NULL;
		if (lastctl == NULL)
			ctls = ct;
		else
			lastctl->next = ct;
		lastctl = ct;
	}
}

/*
 - prime - prime control lists from active file
 */
void
prime(afile)
char *afile;
{
	register char *line;
	register FILE *af;
	register struct ctl *ct;
#	define	NFACT	4
	char *field[NFACT];
	int nf;
	register int hash;

	af = eufopen(afile, "r");
	while ((line = fgetline(af, (size_t *)NULL)) != NULL) {
		nf = split(line, field, NFACT, "");
		if (nf != NFACT)
			die("wrong number of fields in active for `%s'", field[0]);
		ct = (struct ctl *)malloc(sizeof(struct ctl));
		if (ct == NULL)
			fail("out of memory at newsgroup `%s'", field[0]);
		ct->groups = strsave(field[0]);
		ct->ismod = (strchr(field[3], 'm') != NULL) ? MOD : UNMOD;
		fillin(ct);
		hash = strlen(field[0]);
		if (hash > NHASH-1)
			hash = NHASH-1;
		ct->next = ngs[hash];
		ngs[hash] = ct;
	}
	(void) fclose(af);

	checkused();
}

/*
 - fillin - fill in a ctl struct for a newsgroup from the control-file list
 */
void
fillin(ct)
register struct ctl *ct;
{
	register struct ctl *cscan;
	char grump[100];

	for (cscan = ctls; cscan != NULL; cscan = cscan->next)
		if ((cscan->ismod == ct->ismod || cscan->ismod == EITHER) &&
		    ngpatmat(cscan->pat, ct->groups)) {
			ct->retain = cscan->retain;
			ct->normal = cscan->normal;
			ct->purge = cscan->purge;
			ct->dir = cscan->dir;
			cscan->ngroups++;
			return;
		}

	/* oooooops... */
	sprintf(grump, "group `%%s' (%smoderated) not covered by control file",
					(ct->ismod == MOD) ? "" : "un");
	die(grump, ct->groups);
}

/*
 - checkused - check that all lines of the control file got used
 */
void
checkused()
{
	register struct ctl *cscan;
	char grump[100];

	for (cscan = ctls; cscan != NULL; cscan = cscan->next)
		if (cscan->ngroups == 0) {
			sprintf(grump,
	"warning: line %d of control file controls no active newsgroups",
								cscan->lineno);
			complain(grump, "");
		}
}

/*
 - doit - file manipulation and master control
 */
void
doit()
{
	register int old;
	register FILE *new;
	extern void mainloop();

	cd(histdir);
	old = open("history", 0);
	if (old < 0)
		fail("cannot open `%s'", "history");
	if (rebuild) {
		(void) remove("history.n");
		(void) remove("history.n.dir");
		(void) remove("history.n.pag");
		if (spacetight)
			(void) remove("history.o");
		new = eufopen("history.n", "w");
		(void) fclose(eufopen("history.n.dir", "w"));
		(void) fclose(eufopen("history.n.pag", "w"));
		(void) dbzincore(1);
		errno = 0;
		if (dbzagain("history.n", "history") < 0)
			fail("dbzagain(history.n) failed", "");
	}

	cd(artfile((char *)NULL));
	mainloop(old, new);
	/* side effect of mainloop():  newslock() */

	(void) close(old);
	if (rebuild) {
		eufclose(new, "history.n");
		if (dbmclose() < 0)
			fail("dbmclose() failed", "");
	}

	if (testing)
		return;
	if (rebuild) {
		cd(histdir);
		if (rename("history", "history.o") != 0)
			fail("can't move history", "");
		if (rename("history.n", "history") != 0)
			fail("disaster -- can't reinstate history!", "");
		if (rename("history.n.dir", "history.dir") != 0)
			fail("disaster -- can't reinstate history.dir!", "");
		if (rename("history.n.pag", "history.pag") != 0)
			fail("disaster -- can't reinstate history.pag!", "");
	}
}

/*
 - mainloop - main loop, reading old history file and building new one
 */
void
mainloop(old, new)
register int old;
register FILE *new;
{
	register char *line;
	long here;
	datum lhs;
	datum rhs;
	register int ret;
#	define	NF	3
	char *field[NF];	/* fields in line */
	register int nf;
#	define	NSF	3	/* lump subfields after second into one */
	char *subfield[NSF];	/* subfields in middle field */
	register long lineno = 0;
	char linenobuf[20];
	register int nsf;
	register int i;

	while ((line = readline(old)) != NULL) {
		lineno++;
		if (expdebug) {
			fprintf(stderr, "\nline %ld `", lineno);
			fputs(line, stderr);
			fputs("'\n", stderr);
		}
		nf = split(line, field, NF, "\t");
		if (nf == 2)
			field[2] = NULL;

		if (nf < 2 || nf > 3 || *field[0] == '\0') {
			sprintf(linenobuf, "%ld", lineno);
			complain("garbled history entry, line %s", linenobuf);
			ret = 0;
		} else {
			nsf = split(field[1], subfield, NSF, subsep);
			if (nsf > NSF)
				nsf = NSF;
			ret = doline(field, nf, subfield, nsf);
		}

		if (ret >= 0 && rebuild) {
			/* make the DBM entry */
			lhs.dptr = field[0];
			lhs.dsize = strlen(field[0])+1;
			here = ftell(new);
			rhs.dptr = (char *)&here;
			rhs.dsize = sizeof(here);
			ret = dbzstore(lhs, rhs);
			if (ret < 0)
				die("dbzstore failure on `%s'", field[0]);

			/* make the history entry */
			fputs(field[0], new);
			putc('\t', new);
			fputs(subfield[0], new);
			for (i = 1; i < nsf; i++) {
				putc(*subsep, new);
				fputs(subfield[i], new);
			}
			if (field[2] != NULL) {
				putc('\t', new);
				fputs(field[2], new);
			}
			putc('\n', new);

			if (expdebug)
				fprintf(stderr, "new line `%s\t%s%c%s\t%s'\n",
					field[0], subfield[0], *subsep,
					subfield[1],
					(field[2] == NULL) ? "" : field[2]);
		}
	}
	/* side effect of readline() == NULL:  newslock() */
}

/*
 - doline - handle one history line, possibly modifying it
 */
int				/* 0 keep it, <0 don't */
doline(field, nf, subfield, nsf)
char *field[NF];		/* fields in line */
register int nf;
char *subfield[NSF];		/* subfields in middle field */
register int nsf;
{
	register time_t recdate;
	register time_t expdate;
	static char expbuf[25];		/* plenty for decimal time_t */
	register char *oldf2;
	register char *sf1 = subfield[1];

	/* sort out the dates */
	if (nsf < 2 || *sf1 == '\0' || (*sf1 == '-' && *(sf1+1) == '\0'))
		expdate = NODATE;
	else {
		expdate = readdate(sf1);
		if (expdate == NODATE && dategrump)
			complain("ignoring bad expiry date `%s',", sf1);
	}
	recdate = readdate(subfield[0]);
	if (recdate == NODATE) {
		complain("bad arrival date `%s' -- expiring", subfield[0]);
		recdate = readdate("0");
		expdate = recdate;
	}
	if (recdate > latest)
		latest = recdate;
	if (expdebug)
		fprintf(stderr, "rec %ld, expire %ld\n", (long)recdate,
								(long)expdate);

	/* deal with it */
	oldf2 = field[2];
	field[2] = doarticle(oldf2, recdate, expdate, field[0]);
	if (oldf2 != NULL) {
		if (field[2] == NULL)
			ngone++;
		else
			nkept++;
	}
	if (field[2] == NULL) {
		if (holdover == NULL || shouldgo(recdate, NODATE, holdover))
			return(-1);	/* easy case -- get rid of it */
		nresid++;
	}

	/* hard case -- must rebuild expiry date */
	if (expdate != NODATE) {
		sprintf(expbuf, "%ld", (long)expdate);
		subfield[1] = expbuf;
	} else
		subfield[1] = "-";
	return(0);
}

/*
 - readdate - turn a date into internal form
 */
time_t
readdate(text)
char *text;
{
	register time_t ret;

	/* heuristic:  non-numeric dates never involve numbers >4 digits */
	ret = atol(text);
	if (ret < 10000 && strspn(text, "0123456789") != strlen(text)) {
		ret = getindate(text, (struct timeb *)NULL);
		if (ret == -1)
			ret = NODATE;
	}

	return(ret);
}

/*
 - doarticle - possibly expire an article
 *
 * Re-uses the space of its first argument.
 */
char *				/* new name list, in space of old, or NULL */
doarticle(oldnames, recdate, expdate, msgid)
char *oldnames;			/* may be destroyed */
time_t recdate;
time_t expdate;
char *msgid;			/* for printstuff() */
{
	register char *src;
	register char *dst;
	register char *name;
	register char *dir;
	register char *p;
	register char srcc;
	register int nleft;
	register int nexpired;
	register int ret;
#	define	NDELIM	" ,"

	if (oldnames == NULL)
		return(NULL);

	src = oldnames;
	dst = oldnames;
	nleft = 0;
	nexpired = 0;
	for (;;) {
		src += strspn(src, NDELIM);
		name = src;
		src += strcspn(src, NDELIM);
		srcc = *src;
		*src = '\0';
		if (*name == '\0')
			break;		/* NOTE BREAK OUT */
		if (expdebug)
			fprintf(stderr, "name `%s'\n", name);

		ret = -1;
		dir = whereexpire(recdate, expdate, name);
		if (dir != dont && !(leaders && nleft == 0 && srcc != '\0')) {
			if (expdebug)
				fprintf(stderr, "expire into `%s'\n",
					(dir == NULL) ? "(null)" : dir);
			for (p = strchr(name, '.'); p != NULL;
							p = strchr(p+1, '.'))
				*p = '/';
			ret = expire(name, dir);
			if (ret < 0) {	/* oops -- couldn't expire it! */
				for (p = strchr(name, '/'); p != NULL;
							p = strchr(p+1, '/'))
					*p = '.';
				p = strrchr(name, '.');
				if (p != NULL)
					*p = '/';
			}
		}
		if (ret > 0) {		/* we got rid of it */
			if (dir != NULL && printexpiring)
				printstuff(msgid, name, recdate);
			nexpired++;
		} else if (ret < 0) {	/* it's still around */
			if (dst != oldnames)
				*dst++ = ' ';
			while (*name != '\0')
				*dst++ = *name++;
			nleft++;
		}
		*src = srcc;
	}

	if (nleft == 0)
		return(NULL);
	*dst++ = '\0';
	if (leaders && nleft == 1 && nexpired > 0)	/* aging leader */
		return(doarticle(oldnames, recdate, expdate, msgid));
	return(oldnames);
}

/*
 - whereexpire - where should this name expire to, and should it?
 *
 * The "dont" variable's address is used as the don't-expire return value,
 * since NULL means "to nowhere".
 */
char *				/* archive directory, NULL, or dont */
whereexpire(recdate, expdate, name)
time_t recdate;
time_t expdate;
char *name;
{
	register char *group;
	register char *slash;
	register struct ctl *ct;
	register int hash;

	group = name;
	slash = strchr(group, '/');
	if (slash == NULL)
		die("no slash in article path `%s'", name);
	else
		*slash = '\0';
	if (strchr(slash+1, '/') != NULL) {
		*slash = '/';
		die("multiple slashes in article path `%s'", name);
	}

	/* find applicable expiry-control struct (make it if necessary) */
	hash = strlen(group);
	if (hash > NHASH-1)
		hash = NHASH-1;
	for (ct = ngs[hash]; ct != NULL && !STREQ(ct->groups, group);
								ct = ct->next)
		continue;
	if (ct == NULL) {	/* oops, there wasn't one */
		if (expdebug)
			fprintf(stderr, "new group `%s'\n", group);
		ct = (struct ctl *)malloc(sizeof(struct ctl));
		if (ct == NULL)
			fail("out of memory for newsgroup `%s'", group);
		ct->groups = strsave(group);
		ct->ismod = UNMOD;	/* unknown -- treat it as mundane */
		fillin(ct);
		ct->next = ngs[hash];
		ngs[hash] = ct;
	}
	*slash = '/';

	/* and decide */
	if (!shouldgo(recdate, expdate, ct) || (holdarch && ct->dir != NULL))
		return(dont);
	else
		return(ct->dir);
}

/*
 - shouldgo - should article with these dates expire now?
 */
int				/* predicate */
shouldgo(recdate, expdate, ct)
time_t recdate;
time_t expdate;
register struct ctl *ct;
{
	if (recdate >= ct->retain)	/* within retention period */
		return(0);
	if (recdate <= ct->purge)	/* past purge date */
		return(1);
	if (expdate != NODATE) {
		if (now >= expdate)	/* past its explicit date */
			return(1);
		else
			return(0);
	} else {
		if (recdate < ct->normal)	/* past default date */
			return(1);
		else
			return(0);
	}
	/* NOTREACHED */
}

/*
 - expire - expire an article
 */
int				/* >0 success, 0 missing, <0 failure */
expire(name, dir)
char *name;
char *dir;
{
	register char *old;
	register char *new;
	register int ret;

	if (testing) {
		if (dir != NULL)
			fprintf(stderr, "copy %s %s ; ", name, dir);
		fprintf(stderr, "remove %s\n", name);
		return(1);
	}

	if (dir != NULL) {		/* should archive */
		/* sort out filenames */
		old = strsave(artfile(name));
		if (*dir == '=') {
			new = strrchr(name, '/');
			if (new == NULL)
				die("no slash in `%s'", name);
			new++;
			new = str3save(dir+1, "/", new);
		} else
			new = str3save(dir, "/", name);

		/* cp() usually succeeds, so try it before getting fancy */
		ret = cp(old, new);
		if (ret == 'n')		/* new could not be created */
			if (*dir != '=') {
				mkparents(name, dir);
				ret = cp(old, new);	/* try again */
			}
		switch (ret) {		/* report problems, if any */
		case 0:			/* success */
			narched++;
			break;
		case 'o':		/* old did not exist */
			nmissing++;
			ret = 0;	/* not strictly an error */
			break;
		case 'n':		/* new could not be created */
			warning("can't create `%s'", new);
			ncpfail++;
			break;
		case 'r':		/* read error */
			warning("error reading `%s'", old);
			ncpfail++;
			break;
		case 'w':		/* write error */
			warning("error writing `%s'", new);
			ncpfail++;
			break;
		default:
			warning("error archiving `%s'", name);
			ncpfail++;
			break;
		}

		/* limited patience for archiving failures */
		if (ret != 0 && ncpfail == 3) {
			warning("multiple archiving failures, forcing -h", "");
			holdarch = 1;
		}

		free(new);
		free(old);
		if (ret != 0)
			return(-1);	/* without removing original */
	}
	if (unlink(artfile(name)) < 0) {
		if (errno != ENOENT) {
			warning("can't remove `%s'", name);
			return(-1);
		} else
			nmissing++;
	} else if (dir == NULL)
		njunked++;
	return(1);
}

/*
 - cp - try to copy an article (top level, administration)
 */
int				/* 0 success, other character failure */
cp(old, new)
char *old;			/* pathnames good from here */
char *new;
{
	register int ret;
	register int in, out;

	in = open(old, 0);
	if (in < 0)
		return('o');
	out = creat(new, 0666);
	if (out < 0) {
		(void) close(in);
		return('n');
	}

	ret = cploop(in, out);

	(void) close(in);
	if (fsync(out) < 0)
		ret = 'w';
	if (close(out) < 0)
		ret = 'w';

	return(ret);
}

/*
 - cploop - try to copy an article (bottom level, copy loop)
 */
int				/* 0 success, other character failure */
cploop(in, out)
int in;
int out;
{
	register int ret;
	register int count;
	register int firstblock = 1;

	while ((count = read(in, abuf, sizeof(abuf))) > 0) {
		ret = write(out, abuf, count);
		if (ret != count)
			return('w');
		if (firstblock) {
			getsubj(abuf, count);
			firstblock = 0;
		}
	}
	if (count < 0)
		return('r');

	return(0);
}

/*
 - getsubj - try to find the Subject: line in a buffer
 *
 * Result goes in "subject", and is never empty.  Tabs become spaces,
 * since they are the output delimiters.
 */
void
getsubj(buf, bsize)
char *buf;
int bsize;
{
	register char *scan;
	register char *limit;
	register int len;
	register int clipped;
	static char sline[] = "Subject:";

	len = strlen(sline);
	limit = buf + bsize - len;
	for (scan = buf; scan < limit; scan++)
		if (CISTREQN(scan, sline, len) &&
				(scan == buf || *(scan-1) == '\n')) {
			scan += len;
			for (limit = scan; limit < buf+bsize; limit++)
				if (*limit == '\n')
					break;
			while (scan < limit && isascii(*scan) && isspace(*scan))
				scan++;
			len = limit-scan;
			clipped = 0;
			if (len > sizeof(subject)-1) {
				len = sizeof(subject) - 1 - strlen("...");
				clipped = 1;
			}
			if (len > 0) {
				(void) strncpy(subject, scan, len);
				subject[len] = '\0';
			} else
				(void) strcpy(subject, "???");
			if (clipped)
				(void) strcat(subject, "...");
			for (scan = strchr(subject, '\t'); scan != NULL;
					scan = strchr(scan+1, '\t'))
				*scan = ' ';
			return;
		} else if (*scan == '\n' && scan+1 < limit && *(scan+1) == '\n')
			break;		/* empty line terminates header */

	/* didn't find one -- fill in *something* */
	(void) strcpy(subject, "???");
}

/*
 - mkparents - try to make directories for archiving an article
 *
 * Assumes it can mess with first argument if it puts it all back at the end.
 */
void
mkparents(art, dir)
char *art;			/* name relative to dir */
char *dir;
{
	register char *cmd;
	register char *ocmd;
	register char *p;

	ocmd = str3save("PATH=", ctlfile("bin"), ":");
	cmd = str3save(ocmd, binfile((char *)NULL), ":");
	free(ocmd);
	ocmd = cmd;
	/* the semicolon here avoids problems with some buggy shells */
	cmd = str3save(ocmd, newspath(), " ; mkpdir ");
	free(ocmd);
	ocmd = cmd;
	cmd = str3save(ocmd, dir, "/");
	free(ocmd);
	p = strrchr(art, '/');
	*p = '\0';
	ocmd = cmd;
	cmd = str3save(ocmd, art, "");
	free(ocmd);
	*p = '/';
	(void) system(cmd);
	free(cmd);
}

char *months[12] = {
	"Jan",
	"Feb",
	"Mar",
	"Apr",
	"May",
	"Jun",
	"Jul",
	"Aug",
	"Sep",
	"Oct",
	"Nov",
	"Dec",
};

/*
 - printstuff - print information about an expiring article
 */
void
printstuff(msgid, name, recdate)
char *msgid;
char *name;
time_t recdate;
{
	struct tm *gmt;

	gmt = gmtime(&recdate);
	printf("%s\t%s\t%d-%s-%d\t%s\n", name, msgid, gmt->tm_mday,
			months[gmt->tm_mon], gmt->tm_year+1900, subject);
}

/*
 - eufopen - fopen, with fail if doesn't succeed
 */
FILE *
eufopen(name, mode)
char *name;
char *mode;
{
	FILE *f;
	static char grump[50] = "can't open `%s' for `";

	f = fopen(name, mode);
	if (f == NULL) {
		(void) strcat(grump, mode);
		(void) strcat(grump, "'");
		fail(grump, name);
	}
	return(f);
}

/*
 - eufclose - fclose with failure checking
 */
void
eufclose(f, name)
FILE *f;
char *name;
{
	if (nfclose(f) == EOF)
		fail("error in closing file `%s'", name);
}

/*
 - checkadir - check archiving directory is real, writable, and full pathname
 */
void				/* set -h if not */
checkadir(dir)
char *dir;
{
	struct stat stbuf;
	register int hforce = 0;
#	define	GRUMP(a,b)	{warning(a, b); hforce = 1;}

	if (*dir == '=')	/* disregard leading '=' */
		dir++;
	errno = 0;
	if (stat(dir, &stbuf) < 0)
		GRUMP("archiving directory `%s' does not exist", dir);
	if (access(dir, 02) < 0)
		GRUMP("archiving directory `%s' not writable", dir);
	if (dir[0] != '/')
		GRUMP("archiving directory `%s' not a full pathname", dir);
	if (hforce) {
		warning("forcing -h option as a stopgap", "");
		holdarch = 1;
	}
}

/*
 - back - get a date n days back, with overflow check
 *
 * Requires that "now" be set first.
 */
time_t
back(ndaystr)
char *ndaystr;
{
	register double goback;		/* how far before now it is */

	if (STREQ(ndaystr, "never"))
		goback = FOREVER;	/* > now-EPOCH */
	else
		goback = atof(ndaystr) * DAY;

	if (goback > now-EPOCH)		/* before EPOCH */
		return(EPOCH);
	return((time_t)(now - goback));
}

/*
 - printlists - print control lists for debugging
 */
void
printlists()
{
	register int i;
	register struct ctl *ct;

	fprintf(stderr, "control file:\n");
	for (ct = ctls; ct != NULL; ct = ct->next)
		pctl(ct);
	fprintf(stderr, "\n");

	for (i = 0; i < NHASH; i++)
		if (ngs[i] != NULL) {
			fprintf(stderr, "list %d:\n", i);
			for (ct = ngs[i]; ct != NULL; ct = ct->next)
				pctl(ct);
		}
	fprintf(stderr, "\n");
}

/*
 - pctl - print one control-list entry
 */
void
pctl(ct)
register struct ctl *ct;
{
#	define	DAYS(x)	((now-(x))/DAY)

	fprintf(stderr, "%s(%c) %.2f-%.2f-%.2f %s\n", ct->groups, ct->ismod,
			DAYS(ct->retain), DAYS(ct->normal), DAYS(ct->purge),
			(ct->dir == NULL) ? "(null)" : ct->dir);
}

/*
 - unprivileged - no-op needed to keep the pathname stuff happy
 */
void
unprivileged(reason)
char *reason;
{
}

/*
 - fail - call errunlock, possibly after cleanup
 */
void
fail(s1, s2)
char *s1;
char *s2;
{
	int saveerr = errno;

	if (spacetight) {
		cd(histdir);
		(void) remove("history.n");
		(void) remove("history.n.dir");
		(void) remove("history.n.pag");
	}
	errno = saveerr;
	errunlock(s1, s2);
	/* NOTREACHED */
}

/*
 - die - like fail, but errno contains no information
 */
void
die(s1, s2)
char *s1;
char *s2;
{
	errno = 0;
	fail(s1, s2);
}

/*
 - readline - read history line (sans newline), with locking when we hit EOF
 *
 * Data pointed to may be altered but not extended.  Note that initialization
 * is cleverly set up so that the first time this is called, it falls through
 * to the "hard case" logic.
 *
 * Minor flaw:  will lose a last line which lacks a newline.
 */
char *				/* NULL is EOF */
readline(fd)
int fd;				/* Note descriptor, not FILE *. */
{
	register char *line;		/* line buffer */
	register size_t linesize;
	register char *linep;		/* unused part of line buffer */
	register char *endp;		/* newline */
	register size_t len;		/* length of line (fragment) */
	register int n;
	extern void refill();

	/* try for the easy case -- whole line in buffer */
	endp = strchr(rest, '\n');
	if (endp != NULL) {
		*endp++ = '\0';
		rlnleft -= endp - rest;
		line = rest;
		rest = endp;
		return(line);
	}

	/* oh well, have to put it together in malloced area... */
	line = rlline;
	linesize = rllsiz;
	if (line == NULL) {
		line = malloc(linesize);
		if (line == NULL)
			fail("out of space when reading history", "");
	}

	linep = line;
	for (;;) {
		if (rlnleft <= 0) {
			refill(fd);
			if (rlnleft <= 0)	/* refill gave up. */
				return(NULL);
		}

		endp = strchr(rest, '\n');
		if (endp == NULL)	/* hit the sentinel */
			len = rlnleft;
		else
			len = endp - rest + 1;
		while (linep + len > line + linesize) {	/* not enough room */
			linesize = (linesize * 3) / 2;
			n = linep - line;
			line = realloc(line, linesize);
			if (line == NULL)
				fail("out of memory in readline", "");
			linep = line + n;
		}

		(void) memcpy(linep, rest, len);
		linep += len;
		rest += len;
		rlnleft -= len;
		if (endp != NULL) {
			*(linep-1) = '\0';
			rlline = line;
			rllsiz = linesize;
			return(line);
		}
	}
	/* NOTREACHED */
}

/*
 - refill - refill readline's buffer, with locking on EOF
 */
void
refill(fd)
int fd;
{
	register int ret;

	/* Just in case... */
	if (rlnleft > 0)
		return;

	/* Try ordinary read. */
	ret = read(fd, rlbuf, (int)rlbufsiz);
	if (ret < 0)
		fail("read error in history", "");
	if (ret > 0) {
		rlnleft = ret;
		rest = rlbuf;
		rlbuf[ret] = '\0';	/* sentinel */
		return;
	}

	/* EOF. */
	if (nlocked)
		return;		/* We're really done. */

	/* EOF but we haven't locked yet.  Lock and try again. */
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGTERM, SIG_IGN);
	newslock();
	nlocked = 1;
	refill(fd);
}
