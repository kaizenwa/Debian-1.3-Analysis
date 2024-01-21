/*
 * fileart - file an article, given its temporary file name and its headers
 *
 * It may be desirable to, some day, prevent cross-postings across
 * "universes", where a universe might be "alt" or "comp,news".
 *
 * There are three classes of newsgroup for the purposes of filing:
 * "wanted" (in the active file and missing the "x" flag);
 * "not wanted" ("x"ed in active, or not in active and not matched by sys
 *	file's subscription list for this machine), so ignore it; or
 * "don't know it" (not in active and matched by subscription list,
 *	so file the article in junk once, iff there are no good groups).
 * junk *must* be in the active file or it's an error (ST_DROPPED),
 * but junk may have an "x" flag to prevent filing.
 *
 * Use the active file 'x' flag to snuff groups quietly, even when your
 * subscription list permits them, without filing in junk.
 *
 * Constraints.
 *
 * Article filing is more subtle than it looks at first, and there are quite
 * a few constraints on it.  The problems are primarily with cross-posted
 * articles.  We prefer to make real Unix links where possible, so that
 * each newsgroup mentioned costs only a directory entry and each article
 * an i-node.  Where this isn't feasible (the news spool is split across
 * file systems or the operating system doesn't support links), we try to
 * make symbolic links.  Where that isn't feasible (the underlying
 * operating system doesn't support symbolic links), we make copies; this
 * is costly in disk space and bandwidth, but provides a worst-case
 * fall-back strategy.
 *
 * To complicate the situation, we may have to create a temporary link for
 * an article with a message header too large to fit in memory, so that we
 * can dump what we have in memory to disk and thence copy the rest of the
 * article there.  And if we must generate an Xref: header (either because
 * the article is cross-posted or because we were asked on the command line
 * to do so), we clearly must emit it before we emit any of the message body,
 * so the names of the links must all be known for certain upon reaching
 * the end of the message header, which means we must at least have created
 * all the links by then, even if they aren't all populated.  In fact, on
 * systems where we must make copies, we will have to go back and make a
 * second pass to populate one link per filesystem.
 *
 * It is tempting to mandate that message headers shall be small, but they
 * continue to grow over time and gatewayed RFC 822 mail messages may contain
 * arbitrary numbers of large Received: headers, for example.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>
#include <sys/stat.h>

#include "libc.h"
#include "news.h"
#include "config.h"
#include "headers.h"
#include "relay.h"
#include "active.h"
#include "history.h"
#include "ngmatch.h"
#include "system.h"

#define XREFDELIM ':'

/*
 * If a_unlink is true, there is a temporary link.
 *
 * If tmplink is false, use a_tmpf to hold the name of the first permanent link,
 * and art->a_artf as its stdio stream.  !tmplink means "Newsgroups:" was seen
 * in time, and is normally true.
 * If tmplink is true, just make links to a_tmpf, which is already open as
 * art->a_artf.  This is only the case if we had to dump the headers early.
 */
#define tmplink(art)	(art)->a_unlink

/* imports */
extern void prefuse();

/* privates */
struct link {
	char	*l_grp;			/* group name, not directory */
	char 	*l_num;			/* NULL or article number */
	char	l_type;			/* 's', 'l' or '\0' */
	boolean	l_fillme;		/* still needs work? */
	dev_t	l_dev;			/* -1 if not known */
};
static struct link *links;		/* array of links */
static struct link *link1;		/* first permanent link */
static struct link *linklim;		/* just past last link in use */
static struct link templink;
static struct link *tlp;
static long artnum;			/* asgnartnum sets artnum */
static int goodngs;			/* asgnartnum reads goodngs */
static int junkgroups;			/* count "junked" groups */
static boolean debug = NO;
static boolean slinknoted = NO;		/* have we noted use of symlinks? */
extern char *slinkfile;			/* in relaynews.c */

static char dbglink[] = "linking `%s' to `%s'... ";
static char dbgsymlink[] = "symlinking `%s' to `%s'... ";
static char dbgempty[] = "couldn't link or symlink; making empty `%s'... ";
static char dbgopen[] = "opening `%s'... ";
static char logxcljunk[] = "no known groups in `%s' and %s group is excluded in active\n";
static char lognojunk[] = "no known groups in `%s' and no %s group\n";
static char logxcl[] = "all groups `%s' excluded in active\n";
static char dbgcopy[] = "couldn't link or symlink; copying `%s' to `%s'... ";

/* forward */
STATIC int lnkcmp();

void
filedebug(state)		/* set debugging state */
boolean state;
{
	debug = state;
}

/* Append ng/artnumstr to art's list of files, and bump goodngs. */
STATIC void
gotgoodng(art, lp)
struct article *art;
struct link *lp;
{
	++goodngs;
	histupdfiles(art, lp->l_grp, lp->l_num);
}

STATIC
filllink(lp, grp, num, type, fillme, dev)
register struct link *lp;
char *grp, *num;
char type;
boolean fillme;
dev_t dev;
{
	lp->l_grp = grp;
	lp->l_num = num;
	lp->l_type = type;
	lp->l_fillme = fillme;
	lp->l_dev = dev;
}

STATIC char *					/* malloced */
linkname(lp)
register struct link *lp;
{
	register char *name;

	if (lp->l_grp == NULL || lp->l_grp[0] == '\0')
		name = strsave(lp->l_num);	/* mostly for SPOOLTMP */
	else {
		name = str3save(lp->l_grp, SFNDELIM, lp->l_num);
		mkfilenm(name);
	}
	return name;
}

STATIC int
trylink(olp, artname, lp)
char *artname;
register struct link *olp, *lp;
{
	register int worked = NO;

	if (newsconf.nc_link) {			/* e.g. Unix */
		char *oname = linkname(olp);

		if (debug)
			(void) fprintf(stderr, dbglink, oname, artname);
		worked = link(oname, artname) == 0;
		if (worked && lp != NULL) {
			lp->l_type = 'l';
			lp->l_dev = olp->l_dev;
		}
		free(oname);
	}
	return worked;
}

STATIC int
trysymlink(olp, artname, lp)
char *artname;
register struct link *olp, *lp;
{
	register int worked = NO;

	if (newsconf.nc_symlink) {			/* e.g. 4.2, Eunice */
		char *oname = linkname(olp);

		if (debug)
			(void) fprintf(stderr, dbgsymlink, fullartfile(oname),
				artname);
		worked = symlink(fullartfile(oname), artname) == 0;
		if (worked && lp != NULL)
			lp->l_type = 's';
		if (worked && !slinknoted && slinkfile != NULL) {
			register FILE *f = fopen(slinkfile, "w");

			if (f != NULL) {
				fprintf(f, "%ld\n", (long)time((time_t *)NULL));
				fclose(f);
			}
			slinknoted = 1;
		}
		free(oname);
	}
	return worked;
}

/*
 * we have to create the links before we have seen the entire article,
 * so just make empty links for now; later on, we will copy into them.
 */
STATIC int
trycopy(artname, lp)
char *artname;
register struct link *lp;
{
	register FILE *out;
	register int worked = NO;
	struct stat statb;

	if (debug)
		(void) fprintf(stderr, dbgempty, artname);
	out = fopenexcl(artname);
	worked = out != NULL;
	if (worked) {
		lp->l_fillme = YES;		/* revisit me later */
		lp->l_type = 'l';
		if (fstat(fileno(out), &statb) >= 0)
			lp->l_dev = statb.st_dev;
		(void) fclose(out);
	}
	return worked;
}

/*
 * If we get here, things look pretty bleak.
 * Links to the first link have failed.
 * We either have no link facilities at all (e.g. Plan 9),
 * or we're trying to link across file systems and can't (e.g. V7).
 * Making copies wastes space, so try to make a link
 * to any other link, if possible, first.
 * If all else fails, make a copy; with luck a later link can be made to it.
 */
STATIC int
tryanything(olp, artname, lp)
char *artname;
register struct link *olp, *lp;
{
	register int worked = NO;
	register struct link *plp;
	char *destdir = strsave(artname);
	char *slp = strrchr(destdir, FNDELIM);
	struct stat statb;

	/* have we got a link on this device already? */
	if (slp != NULL)
		*slp = '\0';		/* form directory name */
	if (stat(destdir, &statb) >= 0)
		lp->l_dev = statb.st_dev;
	free(destdir);
	for (plp = link1; plp < lp; plp++)
		if (plp->l_dev == lp->l_dev && plp->l_dev != (dev_t)-1)
			break;
	if (plp < lp)			/* yes, we do */
		if (plp->l_type == 'l') {
			worked = trylink(plp, artname, lp);
			if (!worked)
				worked = trysymlink(link1, artname, lp);
			/* else make a copy */
		} else if (plp->l_type == 's' && olp->l_type != 's')
			worked = trysymlink(link1, artname, lp);
	if (!worked)
		worked = trycopy(artname, lp);
	return worked;
}

filetmp(art)				/* make temporary link */
register struct article *art;
{
	if (art->a_artf == NULL) {
		nnfree(&art->a_tmpf);
		art->a_tmpf = strsave(SPOOLTMP);
		(void) mktemp(art->a_tmpf);
		art->a_unlink = YES;
		art->a_artf = fopenwclex(art->a_tmpf, "w+");
		if (art->a_artf == NULL)
			persistent(art, '\0', "", "");	/* can't open article */
	}
}

STATIC boolean
fileopen(art, lp, artname)		/* create first permanent link */
register struct article *art;
register struct link *lp;
char *artname;
{
	register boolean worked = NO;
	struct stat statb;

	if (debug)
		(void) fprintf(stderr, dbgopen, artname);
	nnfree(&art->a_tmpf);
	art->a_tmpf = strsave(artname);
	art->a_artf = fopenexcl(art->a_tmpf);
	worked = art->a_artf != NULL;
	if (worked && lp != NULL) {
		lp->l_type = 'l';
		if (fstat(fileno(art->a_artf), &statb) >= 0)
			lp->l_dev = statb.st_dev;
	}
	return worked;
}

/*
 * Try to make a link of "olp" to artname.
 * If "olp" is NULL, record and open artname iff no
 * link yet exists to that name (by any file, to avoid overwriting
 * existing articles, e.g. due to an out-of-date active file).
 * Result goes in "lp".
 */
STATIC boolean
openorlink(artname, art, olp, lp)
register char *artname;
register struct article *art;
struct link *olp, *lp;
{
	register boolean worked = NO;	/* open or link worked? */

	errno = 0;			/* paranoia */
	if (olp == NULL)
		worked = fileopen(art, lp, artname);
	else {				/* temp. or perm. link(s) exist */
		/* try links to the first link first */
		worked = trylink(olp, artname, lp);
		if (!worked && errno != ENOENT && olp->l_type != 's')
			worked = trysymlink(link1, artname, lp);
		if (!worked && errno != ENOENT)	/* e.g. v7 over fs's, Plan 9 */
			worked = tryanything(olp, artname, lp);
	}
	if (debug)
		if (worked)
			(void) fprintf(stderr, "success.\n");
		else
			warning("failed.", "");
	return worked;
}

/*
 * Try to link art to artname.
 * If the attempt fails, maybe some intermediate directories are missing,
 * so create any missing directories and try again.  If the second attempt
 * also fails, look at errno; if it is EEXIST, artname already exists
 * (presumably because the active file is out of date, or the existing
 * file is a directory such as net/micro/432), so indicate that higher
 * levels should keep trying, otherwise we are unable to create the
 * necessary directories, so complain and set bad status in art.
 *
 * Returns YES iff there is no point in trying to file this article again,
 * usually because it has been successfully filed, but sometimes because
 * the necessary directories cannot be made.
 */
STATIC boolean
mkonelink(art, olp, lp, lnkstatp, artname)
register struct article *art;
struct link *olp, *lp;
statust *lnkstatp;
register char *artname;
{
	if (openorlink(artname, art, olp, lp))
		return YES;
	else if (errno == ENOENT) {
		(void) mkdirs(artname, getuid(), getgid());
		if (openorlink(artname, art, olp, lp))
			return YES;
		else if (errno != EEXIST) {
			persistent(NOART, 'f', "can't link to `%s'", artname);
			*lnkstatp |= ST_DROPPED; /* really can't make a link */
			return YES;		/* hopeless - give up */
		} else
			return NO;
	} else
		return NO;
}

/*
 * Construct a link name (slashng/artnum) for this article,
 * and try to link "olp" to it, with results in "lp".
 *
 * We changed directory to spooldir in main(), so the generated name
 * is relative to spooldir, therefore artname can be used as is.
 *
 * Return value is the same as mkonelink's.
 */
STATIC boolean
tryartnum(art, olp, lp, lnkstatp)
struct article *art;
register struct link *olp, *lp;
statust *lnkstatp;
{
	register char *artname;		/* article file name */
	register boolean ret;
	char artnumstr[30];

	(void) sprintf(artnumstr, "%ld", artnum);
	nnfree(&lp->l_num);		/* in case we are trying again */
	lp->l_num = strsave(artnumstr);
	artname = linkname(lp);
	ret = mkonelink(art, olp, lp, lnkstatp, artname);
	free(artname);
	return ret;
}

/*
 * Assign a permanent name and article number to the existing link "olp",
 * in newsgroup lp->l_grp & store the ascii form of the article
 * number into lp->l_num, returning the article number in "artnum".
 * If lp->l_num is non-null initially, it's the ascii article number
 * to file under.
 *
 * If tmplink is false and goodngs is zero, set inname to artname,
 * fopen artname and store the result in art->a_artf.
 */
STATIC statust
asgnartnum(art, olp, lp)
register struct article *art;
register struct link *olp, *lp;
{
	statust lnkstat = ST_OKAY;

	/* field active 'x' flag: don't file this group, quietly */
	if (unwanted(lp->l_grp)) {
		artnum = -1;
		logaudit(art, 'a', "group `%s' is 'x'ed", lp->l_grp);
		return ST_REFUSED;
	}
	if (lp->l_num != NULL) {		/* number supplied? */
		artnum = atol(lp->l_num);	/* believe number */
		if (!tryartnum(art, olp, lp, &lnkstat)) {
			char *s1;
			char number[30];

			(void) sprintf(number, "%ld", artnum);
			s1 = str3save("article #", number,
				      " in group `%s' supplied but occupied!");
			errno = 0;
			transient(NOART, 'f', s1, lp->l_grp);
			free(s1);
			lnkstat |= ST_DROPPED;
		}
	} else
		while ((artnum = nxtartnum(lp->l_grp)) >= 1 &&
		    !tryartnum(art, olp, lp, &lnkstat))
				;
	return lnkstat;
}

/*
 * File once in "junk" iff no ngs were filed due to absence from
 * active, but some were permitted by sys.  This will make one junk
 * link, no matter how many bad groups, and only if all are bad
 * (e.g. rec.drugs,talk.chew-the-fat).
 */
STATIC void
mkjunklink(art)
register struct article *art;
{
	register statust lnkstat = ST_OKAY;
	struct link jlink;
	register struct link *jlp = &jlink;

	if (goodngs != 0)
		return;		/* shouldn't be here, with valid groups */

	if (junkgroups > 0) {
		/* All groups were "junked"; try to file this article in junk */
		filllink(jlp, JUNK, (char *)NULL, '\0', NO, (dev_t)-1);
		lnkstat = asgnartnum(art, tlp, jlp);
		art->a_status |= lnkstat;
		if (artnum >= 1 && lnkstat == ST_OKAY) {
			gotgoodng(art, jlp);
			logaudit(art, '\0', "article junked", "");
			art->a_status |= ST_JUNKED;
		} else
		/* couldn't file article in junk.  why? */
		if (lnkstat&ST_REFUSED) {	/* junk is 'x'ed */
			prefuse(art);
			(void) printf(logxcljunk, art->h.h_ngs, JUNK);
		} else {			/* junk is missing? */
			persistent(art, 'f',
		"can't file in %s group; is it absent from active?", JUNK);
			art->a_status |= ST_REFUSED;
			prefuse(art);
			(void) printf(lognojunk, art->h.h_ngs, JUNK);
		}
	} else {
		/*
		 * Groups were permitted by subscription list, but all
		 * were 'x'ed in active, or otherwise refused.
		 */
		if (opts.histreject)
			history(art, NOLOG);
		prefuse(art);
		(void) printf(logxcl, art->h.h_ngs);
		transient(art, '\0', "article rejected due to groups", "");
	}
}

static char *
xrefngs(art)		/* hack up Xref: value and return ng:num pairs */
register struct article *art;
{
	register char *ngs = NULL, *site, *groups;

	errno = 0;
	if (art->h.h_xref == NULL)
		transient(art, 'b', "no Xref: in believe-Xref mode", "");
	else {
		for (site = groups = skipsp(art->h.h_xref);
		     *groups != '\0' && isascii(*groups) && !isspace(*groups);
		     groups++)
			;			/* skip over site name */
		if (*groups != '\0')
			*groups++ = '\0';	/* terminate site name */
		groups = skipsp(groups);
		if (!STREQ(site, opts.blvsite))
			transient(art, 'b',
	"article received from wrong site `%s' in believe-Xref mode", site);
		else
			ngs = groups;		/* site is cool; rest is ngs */
	}
	return ngs;
}

static char *
histngs(art)
register struct article *art;
{
	register char *ngs = NULL, *site, *groups = NULL, *histent = NULL;
	static char *lasthist;

	if (lasthist != NULL)
		free(lasthist);
	lasthist = histent = gethistory(art->h.h_msgid);
	errno = 0;
	if (histent == NULL)
		transient(art, 'b', "no history entry in duplicate-feed mode",
			"");
	else if ((groups = findfiles(histent)) == NULL)
		transient(art, 'b',
			"expired history entry in duplicate-feed mode", "");
	else {
		site = strsvto(art->h.h_path, '!');
		if (!STREQ(site, opts.dupsite))
			transient(art, 'b',
	"article received from wrong site `%s' in duplicate-feed mode", site);
		else {
			ngs = groups;
			/* convert group/art list to group:art list */
			stranslit(ngs, FNDELIM, XREFDELIM);
		}
		free(site);
	}
	return ngs;
}

/*
 * extract list of newsgroups (and possibly article numbers) from article
 * headers and history file, as options indicate.  If article numbers are
 * being dictated by incoming Xref: or old history entry, the article numbers
 * will be attached to the end of the group names by a colon as in Xref:
 * (e.g. comp.lang.c:25780,general:12).
 */
static char *
extngs(art)
register struct article *art;
{
	register char *ngs = NULL, *groups;

	errno = 0;
	if (opts.blvxref)
		ngs = xrefngs(art);
	else if (opts.dupsokay)
		ngs = histngs(art);
	else {
		groups = art->h.h_ctlcmd != NULL? CONTROL: art->h.h_ngs; /* NCMP */
		if (strchr(groups, XREFDELIM) != NULL)
			transient(art, 'b',
				"colon not permitted in Newsgroups: list", ""); 
		else
			ngs = groups;
	}
	if (ngs == NULL)
		transient(art, '\0', "no groups in headers", "");
	else {
		/* convert any whitespace to commas for fileart */
		stranslit(ngs, ' ', NGSEP);
		stranslit(ngs, '\t', NGSEP);		/* probably overkill */
	}
	return ngs;
}

STATIC boolean
linkonce(art, olp, lp)
register struct article *art;
register struct link *olp, *lp;
{
	register statust lst = asgnartnum(art, olp, lp);
	register boolean ret;

	ret = artnum >= 1 && lst == ST_OKAY;
	if (ret)
		gotgoodng(art, lp);
	else if (!(lst&ST_REFUSED) && ngpatmat(oursys()->sy_trngs, lp->l_grp))
	    	++junkgroups;
	art->a_status |= lst&~ST_REFUSED;
	return ret;
}

STATIC struct link *				/* malloced */
parsengs(ngs)					/* parse ngs into links array */
register char *ngs;
{
	register char *ng, *comma, *numb;
	register struct link *lp;

	links = lp = (struct link *)
		nemalloc((unsigned)(charcount(ngs, NGSEP)+1) * sizeof *links);
	for (; ngs != NULL; ngs = comma) {
		ngs = skipsp(ngs);		/* allow for user stupidity */
		STRCHR(ngs, NGSEP, comma);
		if (comma != NULL)
			*comma = '\0';		/* will be restored below */
		STRCHR(ngs, XREFDELIM, numb);	/* in case of opts.blvxref */
		if (numb != NULL)		/* number supplied? */
			*numb++ = '\0';		/* cleave number from group */

		ng = realngname(ngs);
		if (ng == NULL)
			ng = strsave(ngs);
		if (ng[0] != '\0')		/* ignore null groups */
			filllink(lp++, ng, (numb == NULL? NULL: strsave(numb)),
				'\0', NO, (dev_t)-1);
		else
			free(ng);
		if (numb != NULL)		/* number supplied? */
			*--numb = XREFDELIM;	/* restore lost byte */
		if (comma != NULL)
			*comma++ = NGSEP;	/* step past comma */
	}
	linklim = lp;
	return links;
}

/*
 * Store in spooldir.  Link temp file to spooldir/ng/article-number
 * for each ng.  Control messages go in CONTROL.
 *
 * The plan is: for each newsgroup, map the group name to its local
 * equivalent (due to = active flag, etc.) for filing, try to file the
 * article, if (no such group in active or link failed, and the group
 * wasn't 'x'ed in active, but our subscription list permits this group),
 * then set flag to file it under "junk" later, if the article number was
 * assigned and the link succeeded, then update art->a_files list for history,
 * and finally clear ST_REFUSED in the article's status, since only this
 * single group was refused (by asgnartnum).
 */
STATIC void
mklinks(art)
register struct article *art;
{
	register struct link *lp, *olp;
	register char *ngs;
	struct stat statb;

	if (art->a_filed)
		return;				/* don't file twice */
	art->a_filed = YES;			/* make a note */
	if (art->a_status&ST_REFUSED)
		canthappen(art, 'i',
			"mklinks called with ST_REFUSED set (can't happen)", "");
	artnum = goodngs = junkgroups = 0;
	ngs = extngs(art);
	if (ngs == NULL) {
		link1 = links = NULL;
		return;
	}
	link1 = links = parsengs(ngs);

	/* sort links and strip duplicates (due to =grp or user stupidity) */
	qsort((char *)links, (size_t)(linklim - links), sizeof *links, lnkcmp);
	olp = links;
	for (lp = links + 1; lp < linklim; lp++)
		if (STREQ(olp->l_grp, lp->l_grp))
			lp->l_grp[0] = '\0';	/* mark the link as a dup */
		else
			olp = lp;		/* starting a new group */

	/* make first permanent link without using symlinks. */
	if (tmplink(art)) {  /* we've already got one; belatedly register it */
		tlp = &templink;
		/* 's' is a white lie: it prevents symlinks to this link */
		filllink(tlp, "", art->a_tmpf, 's', NO, (art->a_artf != NULL &&
			fstat(fileno(art->a_artf), &statb) >= 0?
			statb.st_dev: (dev_t)-1));
	} else
		tlp = NULL;
	for (lp = links; lp < linklim; lp++)
		if (lp->l_grp[0] != '\0')	/* not a duplicate link? */
	 		if (linkonce(art, tlp, lp)) {
				link1 = lp++;
				break;	/* kept trying until the sucker took */
			}

	/* create all links after 1st that took; copies to be filled in later */
	for (; lp < linklim; lp++)
		if (lp->l_grp[0] != '\0')	/* not a duplicate link? */
			(void) linkonce(art, link1, lp);
}

STATIC int
lnkcmp(a1, a2)
char *a1, *a2;
{
	return strcmp(((struct link *)a1)->l_grp, ((struct link *)a2)->l_grp);
}

/*
 * File in the spool directory the article in art & fill in art->a_files.
 * Generate Xref: header if needed (successfully cross-posted).
 * (N.B.: Thus must be called before emitting any article body!)
 */
void
fileart(art)
register struct article *art;
{
	mklinks(art);
	if (goodngs == 0)
		mkjunklink(art);
	/* -g or article crossposted, and article is open? */
	if ((opts.genxref && goodngs > 0 || goodngs > 1) && art->a_artf != NULL)
		emitxref(art);
}

/*
 * we have to create the links before we have seen the entire article,
 * so just make empty links for now; later on, we will copy into them.
 */
STATIC int
fill(oname, in, artname, lp)
char *oname, *artname;
FILE *in;
register struct link *lp;
{
	register FILE *out;
	register int worked = NO;
	struct stat statb;

	if (debug)
		(void) fprintf(stderr, dbgcopy, oname, artname);
	out = fopen(artname, "w");
	worked = out != NULL;
	if (worked) {
		register int cnt;
		char buf[8192];

		rewind(in);
		while ((cnt = fread(buf, 1, sizeof buf, in)) > 0)
			(void) fwrite(buf, 1, cnt, out);
		lp->l_type = 'l';
		if (fstat(fileno(out), &statb) >= 0)
			lp->l_dev = statb.st_dev;
		lp->l_fillme = NO;
		worked = fclose(out) != EOF;
	}
	return worked;
}

mkcopies(art)			/* if copies must be made, fill them in here */
register struct article *art;
{
	register struct link *lp;

	if (art->a_status&ST_REFUSED)
		canthappen(art, 'i',
			"mkcopies called with ST_REFUSED set (can't happen)", "");
	if (links == NULL)	/* fileart failed? */
		return;

	/* fill in any empty links */
	for (lp = link1; lp < linklim; lp++) {
		if (lp->l_fillme) {
			char *artname = linkname(lp);

			if (!fill(art->a_tmpf, art->a_artf, artname, lp))
				persistent(art, '\0',
					"can't fill an empty link", "");
			free(artname);
		}
		nnfree(&lp->l_grp);
		nnfree(&lp->l_num);
	}
	free((char *)links);
}
