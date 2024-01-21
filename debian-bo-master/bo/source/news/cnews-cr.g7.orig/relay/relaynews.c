/*
 * relaynews - relay Usenet news (version C)
 * See the file COPYRIGHT for the copyright notice.
 *
 * Written by Geoff Collyer, 15-20 November 1985 and revised periodically
 * since.
 *
 * relaynews parses article headers, rejects articles by newsgroup &
 * message-id, files articles, updates the active & history files,
 * transmits articles, and honours (infrequent) control messages, which do
 * all sorts of varied and rococo things.  Most control messages are
 * implemented by separate programs.  relaynews reads a "sys" file to
 * control the transmission of articles but can function as a promiscuous
 * leaf node without one.  See Internet RFC 1036 for the whole story and
 * RFC 850 for background.
 *
 * relaynews must be invoked under the news user and group ids,
 * typically from cron.  It must *not* be made setuid nor setgid.
 *
 * A truly radical notion: people may over-ride via environment variables
 * the compiled-in default directories so IHCC kludges are not needed and
 * testing is possible (and encouraged) in alternate directories.  This
 * does cause a loss of privilege, to avoid spoofing.
 *
 * The disused old unbatched ihave/sendme protocol is gone because it was
 * too wasteful; use the batched form instead (see the ihave sys flag
 * ("I") instead).
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>		/* to make locking safe */
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>
#include <sys/stat.h>

#include "libc.h"
#include "news.h"
#include "config.h"
#include "fgetfln.h"
#include "active.h"
#include "headers.h"
#include "relay.h"
#include "history.h"

/* imports */
extern int optind;			/* set by getopt */
extern char *optarg;
extern statust cpinsart();		/* from procart.c */
extern statust clshdrstrm();

/* exports */
char *progname = "relaynews";
struct options opts;
struct newsconf newsconf;
char *slinkfile = NULL;

/* privates */
static boolean uunlink = NO;

/*
 * SystemV getcwd simulation, courtesy peter honeyman
 */
static char *
getpwd(path, size)
register char *path;
int size;
{
	register FILE *fp;

	fp = popen("PATH=/bin:/usr/bin pwd 2>/dev/null", "r");
	if (fp == NULL)
		return NULL;
	if (fgets(path, size, fp) == NULL)
		path = NULL;			/* signal failure */
	else
		trim(path);
	return (pclose(fp) != 0? NULL: path);
}

/*
 * if argv contains relative file name arguments, save current directory name
 * in malloced memory, in opts.currdir.
 * then change directory to the spool directory ($NEWSARTS).
 */
void
getwdandcd(argc, argv)
int argc;
char **argv;
{
	register int argind;
	boolean needpwd = NO;
	char dirtmp[MAXPATH];			/* much bigger than needed */

	if (opts.currdir == NULL) {			/* not yet set? */
		for (argind = optind; argind < argc; argind++)
			if (argv[argind][0] != FNDELIM)
				needpwd = YES;

		opts.currdir = "/???";		/* pessimism */
		if (needpwd && getpwd(dirtmp, sizeof dirtmp) != 0)
			opts.currdir = dirtmp;
		opts.currdir = strsave(opts.currdir); /* save a smaller copy */
	}
	cd(fullartfile((char *)NULL));		/* move to spool directory */
}

/*
 * reset various environmental things for safety: umask, alarm,
 * environment variables (PATH, IFS), standard file descriptors,
 * user & group ids.  May exit, so call before locking the news system.
 */
/* ARGSUSED argv */
void
prelude(argv)				/* setuid daemon prelude */
char **argv;
{
	register char *newpath;

	(void) umask(newsumask());
	(void) alarm(0);		/* cancel any pending alarm */
	/* TODO: suppress chatter on failure here */
	newpath = str3save("PATH=", newspath(), "");
	if (newpath == NULL)
		exit(1);		/* no chatter until stdfdopen */
	if (putenv(newpath) ||
	    putenv("SHELL=/bin/sh") ||
	    putenv("IFS= \t\n"))
		exit(1);		/* no chatter until stdfdopen */
	closeall(1);			/* closes all but std descriptors */
	stdfdopen();			/* ensure open standard descriptors */
}

STATIC boolean
debugon(dbopt)
register char *dbopt;
{
	statust status = YES;

	for (; *dbopt != '\0'; dbopt++)
		switch (*dbopt) {
		case 'f':
			filedebug(YES);
			break;
		case 'h':
			hdrdebug(YES);
			break;
		case 'l':
			lockdebug(YES);
			break;
		case 'm':
			matchdebug(YES);
			break;
		case 't':
			transdebug(YES);
			break;
		default:
			status = NO;	/* unknown debugging option */
			(void) fprintf(stderr, "%s: bad -d %c\n",
				progname, *dbopt);
			break;
		}
	return status;
}

/*
 * parse options and set flags
 */
void
procopts(argc, argv, optsp)
int argc;
char **argv;
register struct options *optsp;
{
	int c, errflg = 0;

	while ((c = getopt(argc, argv, "a:b:c:d:hn:o:sux")) != EOF)
		switch (c) {
		case 'a':
			optsp->dupsokay = YES;
			optsp->dupsite = optarg;
			break;
		case 'b':
			optsp->blvxref = YES;
			optsp->blvsite = optarg;
			break;
		case 'c':
			if (optarg[0] != FNDELIM) {
				(void) fprintf(stderr,
					"%s: %s: must be absolute path\n",
					progname, optarg);
				exit(1);
			}
			optsp->currdir = optarg;
			break;
		case 'd':		/* -d debug-options; thanks, henry */
			if (!debugon(optarg))
				errflg++;	/* debugon has complained */
			break;
		case 'h':		/* keep no history of rejects */
			optsp->histreject = NO;
			break;
		case 'n':		/* note use of symlinks in this file */
			slinkfile = optarg;
			break;
		case 'o':
			/* "oldness": drop articles older than this many days */
			optsp->staledays = atol(optarg);
			break;
		case 's':		/* dropping input is serious (inews) */
			optsp->okrefusal = NO;
			break;
		case 'u':		/* unlink good batches when done */
			uunlink = YES;
			break;
		case 'x':
			optsp->genxref = NO;
			break;
		default:
			errflg++;
			break;
		}
	if (errflg) {
		(void) fprintf(stderr,
"usage: %s [-c currdir][-hsux][-d fhlmt][-o days][-b xrefsite][-a dupsite]\n",
			progname);
		exit(1);
	}
}

/*
 * Is line a batcher-produced line (#! rnews count)?
 * If so, return the count through charcntp.
 * This is slightly less convenient than sscanf, but a lot smaller.
 */
boolean
batchln(line, charcntp)
register char *line;
register long *charcntp;
{
	static char numbang[] = "#!";
	static char rnews[] = "rnews";

	*charcntp = 0;
	if (!STREQN(line, numbang, STRLEN(numbang)))
		return NO;
	line = skipsp(line + STRLEN(numbang));
	if (!STREQN(line, rnews, STRLEN(rnews)))
		return NO;
	line = skipsp(line + STRLEN(rnews));
	if (isascii(*line) && isdigit(*line)) {
		*charcntp = atol(line);
		return YES;
	} else
		return NO;
}

/*
 * Unwind "in" and insert each article.
 * For each article, call cpinsart to copy the article from "in" into
 * a (temporary) file in the news spool directory and rename the temp file
 * to the correct final name if it isn't right already.
 *
 * If the unbatcher gets out of sync with the input batch, the unbatcher
 * will print and discard each input line until it gets back in sync.
 */
statust
unbatch(in, inname)
register FILE *in;
char *inname;
{
	register int c;
	/* register */ char *line;
	register statust status = ST_OKAY;
	int insynch = YES;
	long charcnt;

	while (!(status&ST_NEEDATTN) && (c = getc(in)) != EOF) {
		(void) ungetc(c, in);
		while ((line = fgetln(in)) != NULL &&
		    !batchln(line, &charcnt)) {		/* returns charcnt */
			status |= ST_DROPPED|ST_NEEDATTN;	/* save batch */
			if (insynch)
				persistent(NOART, 'b',
					   "unbatcher out of synch in file %s",
					   inname);
			insynch = NO;
		}
		if (!feof(in) && charcnt > 0)	/* anything to do? */
			status |= cpinsart(in, inname, charcnt, YES);
	}
	if (ferror(in))
		errunlock("error reading `%s'", inname);
	return status;
}

/*
 * compute the largest number that can be stored in a long.  in theory, 
 * #define MAXLONG ((long)(~(unsigned long)0 >> 1))
 * will do the job, but old compilers don't have "unsigned long", don't
 * like casts in initialisers, or otherwise miscompute.
 */
STATIC long
maxlong()
{
	register int bits = 0;
	register unsigned word = 1;		/* "unsigned" avoids overflow */
	static long savemaxlong = 0;

	if (savemaxlong > 0)
		return savemaxlong;
	for (bits = 0, word = 1; word != 0; word <<= 1)
		bits++;
	/* bits/sizeof word = bits per char; all bits on but the sign bit */
	savemaxlong = ~(1L << (bits/sizeof word * sizeof savemaxlong - 1));
	if (savemaxlong <= 0) {			/* sanity check */
		errno = 0;
		errunlock("maxlong is non-positive; your compiler is broken", "");
	}
	return savemaxlong;
}

/*
 * process - process input file
 * If it starts with '#', assume it's a batch and unravel it,
 * else it's a single article, so just inject it.
 */
statust
process(in, inname)
FILE *in;
char *inname;
{
	register int c;

	if ((c = getc(in)) == EOF)
		return ST_OKAY; 		/* normal EOF */
	(void) ungetc(c, in);
	if (c == '#')
		return unbatch(in, inname);
	else
		/* -SIZENUL is to avoid overflow later during +SIZENUL */
		return cpinsart(in, inname, maxlong() - SIZENUL, NO);
}

/*
 * process a (relative) file name.
 * in unlink mode, try to unlink good batches, but not very hard.
 */
statust
relnmprocess(name)
char *name;
{
	register statust status = ST_OKAY;
	register FILE *in;
	register char *fullname = (name[0] != FNDELIM?
		str3save(opts.currdir, SFNDELIM, name): strsave(name));

	in = fopenwclex(fullname, "r");
	if (in != NULL) {
		status |= process(in, fullname);
		(void) nfclose(in);
		if (uunlink && !(status&(ST_DROPPED|ST_SHORT)))
			(void) unlink(fullname);
	}
	free(fullname);
	return status;
}

/*
 * process files named as arguments (or implied)
 */
statust
procargs(argc, argv)
int argc;
char **argv;
{
	register statust status = ST_OKAY;

	if (optind == argc)
		status |= process(stdin, "(stdin)");
	else
		for (; optind < argc; optind++)
			status |= relnmprocess(argv[optind]);
	return status;
}

/*
 * main - take setuid precautions, switch to "news" ids, ignore signals,
 * handle options, lock news system, process files & unlock news system.
 */
int
main(argc, argv)
int argc;
char *argv[];
{
	statust status = ST_OKAY;

	if (argc > 0)
		progname = argv[0];
	opts.okrefusal = YES;
	opts.genxref = YES;		/* needed for overview data */
	opts.histreject = YES;		/* retain history of rejects normally */
	opts.currdir = NULL;		/* pessimism */
#ifdef CSRIMALLOC
	{
		char *maldebug = getenv("MALDEBUG");

		if (maldebug != NULL)
			mal_debug(atoi(maldebug));
		mal_leaktrace(0);	/* was 1 */
	}
#endif
	prelude(argv);		/* various precautions; switch to "news" */

	/* ignore signals (for locking). relaynews runs quickly, so don't worry. */
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGTERM, SIG_IGN);
	(void) signal(SIGPIPE, SIG_IGN);	/* we check write returns */

	newsconf.nc_link = YES;		/* HACK */
	newsconf.nc_symlink = YES;	/* HACK */
	procopts(argc, argv, &opts);

	(void) morefds();		/* ask Unix for more descriptors */
	newslock();			/* done here due to dbm internal cacheing */

	getwdandcd(argc, argv);
	status |= procargs(argc, argv);

	status |= synccaches();		/* being cautious: write & close caches */
	status |= closehist();
	status |= clshdrstrm();
	(void) fflush(stdout);		/* log file */
	(void) fflush(stderr);		/* errlog file */

#ifdef notdef
#ifdef CSRIMALLOC
	mal_dumpleaktrace(fileno(stderr));
#endif
#endif
	newsunlock();
	exit((status&ST_NEEDATTN)? 2: (status != ST_OKAY? 1: 0));
	/* NOTREACHED */
}
