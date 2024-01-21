/*
 * active file access functions (big, fast, in-memory version)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>
#include <sys/stat.h>

#include "libc.h"
#include "news.h"
#include "config.h"
#include "active.h"
#include "hdbm.h"

/* private */
static char *active = NULL;	/* cache: points at entire active file */
static int actsize;		/* bytes in active: type int fixed by fread */
static char **actlnps;		/* point at lines in active file */
static unsigned actlines;	/* lines in actlnps actually used */
static HASHTABLE *acttbl;

/* imports from active.c */
extern char actrelnm[];

STATIC statust
actmkindx()			/* build actlnps index for active */
{
	register statust status = ST_OKAY;
	unsigned lnpsz;
	int maxlines;		/* should this really be long? */

	active[actsize] = '\0';		/* make a proper string */
	/* +1 for a possible partial line +1 for a dummy to check overflow */
	maxlines = charcount(active, '\n') + 2;
	lnpsz = sizeof(char *) * (long) maxlines;
	if (lnpsz != sizeof(char *) * (long)maxlines ||
	    (actlnps = (char **)malloc(lnpsz)) == NULL) {
		persistent(NOART, 'm', "`%s' index won't fit in memory",
			ctlfile(actrelnm));
		status |= ST_DROPPED|ST_NEEDATTN;
	} else {
		actlnps[maxlines - 2] = "";	/* in case no partial line */
		actlnps[maxlines - 1] = "";	/* end sentinel */
		actlines = linescan(active, actlnps, maxlines);
		if (actlines >= maxlines) {
			canthappen(NOART, 'i', "too many newsgroups in `%s'",
				ctlfile(actrelnm));
			status |= ST_DROPPED|ST_NEEDATTN;
		}
	}
	return status;
}

STATIC statust
hashlines()
{
	register statust status = ST_OKAY;
	register char *pos;
	register unsigned line = 0;

	acttbl = hdbmcreate(1000, (unsigned (*)())NULL);
	while (pos = actlnps[line], line++ < actlines && pos[0] != '\0') {
		register char *sp;
		HDBMDATUM key, data;

		STRCHR(pos, ' ', sp);
		if (sp == NULL)
			continue;		/* junk */
		key.dat_len = sp - pos;
		key.dat_ptr = pos;
		data.dat_len = 0;		/* fake */
		data.dat_ptr = pos;
		errno = 0;
		if (!hdbmstore(acttbl, key, data)) {
			persistent(NOART, 'f', "can't store active hash item",
				"");
			status |= ST_DROPPED|ST_NEEDATTN;
			break;
		}
	}
	return status;
}

STATIC void
freeactive()
{
	nnfree(&active);
	nnafree(&actlnps);
	hdbmdestroy(acttbl);
	acttbl = NULL;
}

statust
actfload(fp)
FILE *fp;
{
	statust status = ST_OKAY;

	if (fp != NULL && active == NULL) {
		struct stat sb;

		errno = 0;
		if (fstat(fileno(fp), &sb) < 0)
			persistent(NOART, 'f', "can't fstat `%s'",
				   ctlfile(actrelnm));
		else if (actsize = sb.st_size, /* squeeze into an int */
		    (unsigned)actsize != sb.st_size)
			persistent(NOART, 'f', "`%s' won't fit into memory",
				   ctlfile(actrelnm));
		else if ((active = malloc((unsigned)actsize+1)) == NULL)
			persistent(NOART, 'm', "can't allocate memory for `%s'",
				ctlfile(actrelnm));
		else {
			rewind(fp);
			/*
			 * If we read with fgetms, we might be able to avoid
			 * calling linescan().
			 */
			if (fread(active, 1, actsize, fp) != actsize) {
				persistent(NOART, 'f', "error reading `%s'",
					ctlfile(actrelnm));
				status |= ST_DROPPED|ST_NEEDATTN;
			} else
				status |= actmkindx();
		}
		if (active == NULL) {
			persistent(NOART, '\0', "can't read active", "");
			status |= ST_DROPPED|ST_NEEDATTN;
		}
		if (status == ST_OKAY)
			status |= hashlines();
		if (status != ST_OKAY)
			freeactive();
	}
	return status;
}

/*
 * Store in lnarray the addresses of the starts of lines in s.
 * Return the number of lines found; if greater than nent,
 * store only nent and return nent.
 * Thus lnarray should be one bigger than needed to detect overflow.
 */
int
linescan(s, lnarray, nent)
char *s;
char **lnarray;
register int nent;
{
	register char **lnarrp = lnarray;
	register int i = 0;
	register char *nlp = s;

	if (i < nent)
		*lnarrp++ = nlp;
	while (++i < nent && (nlp=strchr(nlp, '\n')) != NULL && *++nlp != '\0')
		*lnarrp++ = nlp;
	return i;		/* number of addrs stored */
}

statust
actfsync(fp)			/* write to disk, fp is open */
FILE *fp;
{
	statust status = ST_OKAY;

	rewind(fp);
	if (active != NULL) {
		if (fwrite(active, actsize, 1, fp) != 1) {
			persistent(NOART, '\0', "write failed", "");
			status |= ST_DROPPED|ST_NEEDATTN;
		}
		freeactive();
	}
	return status;
}

/* ARGSUSED fp */
char *
actfind(fp, ng, nglen)
FILE *fp;
register char *ng;
register int nglen;
{
	HDBMDATUM key, data;

	key.dat_ptr = ng;
	key.dat_len = nglen;
	data = hdbmfetch(acttbl, key);
	return data.dat_ptr;
}

/* ARGSUSED */
statust
actfwrnum(fp, pos)
FILE *fp;
char *pos;
{
	return ST_OKAY;
}
