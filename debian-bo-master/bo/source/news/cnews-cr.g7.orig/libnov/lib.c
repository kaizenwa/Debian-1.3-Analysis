/*
 * library to access news history adjunct data
 * See the file COPYRIGHT for the copyright notice.
 */

#include <stdio.h>
#include <string.h>
#include <fgetmfs.h>
#include <fgetfln.h>
#include <hash.h>
#include <hdbm.h>
#include "newsoverview.h"

#define NEWSARTS "/usr/spool/news"	/* default news spool */

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

/* imports */
extern char *malloc(), *strsave(), *str3save();
extern char *progname;

static char *newsarts = NEWSARTS;	/* news spool */

novartdir(dir)
char *dir;
{
	newsarts = (dir == NULL? NEWSARTS: dir);
}

static struct novgroup *			/* malloced */
novnew()
{
	register struct novgroup *gp = (struct novgroup *)malloc(sizeof *gp);

	if (gp != NULL) {
		gp->g_first = gp->g_curr = NULL;
		gp->g_msgids = gp->g_roots = NULL;
		gp->g_dir = NULL;
		gp->g_stream = NULL;
	}
	return gp;
}

struct novgroup *				/* malloced cookie */
novopen(grp)					/* change to group grp */
char *grp;
{
	register struct novgroup *gp = novnew();
	register char *sgrp;

	if (gp == NULL)
		return NULL;
	sgrp = strsave(grp);
	if (sgrp == NULL) {
		free((char *)gp);
		return NULL;
	}
	mkfilenm(sgrp);
	gp->g_dir = str3save(newsarts, "/", sgrp);
	free(sgrp);
	return gp;
}

struct novgroup *
novstream(fp)
register FILE *fp;
{
	register struct novgroup *gp = novnew();

	if (gp != NULL)
		gp->g_stream = fp;
	return gp;
}

struct novart *
novall(gp)
register struct novgroup *gp;
{
	if (gp->g_first == NULL)	/* new group? */
		(void) prsoverview(gp);
	return gp->g_first;
}

struct novart *
novnext(gp)
register struct novgroup *gp;			/* cookie from novopen */
{
	register struct novart *thisart;

	if (gp->g_first == NULL)	/* new group? */
		(void) prsoverview(gp);
	thisart = gp->g_curr;
	if (thisart != NULL)
		gp->g_curr = thisart->a_nxtnum;
	return thisart;
}

static
freeart(art)
register struct novart *art;
{
	if (art->a_refs != NULL)
		free(art->a_refs);
	if (art->a_parent != NULL)
		free(art->a_parent);
	if (art->a_num != NULL)
		free(art->a_num);	/* the original input line, chopped */
	free((char *)art);
}

#define MAXFIELDS 9		/* last field is "other" fields */
#define DEFREFS 20

#define PRSFAIL 0		/* disaster (out of memory, etc.) */
#define PRSOKAY 1
#define PRSBAD  2		/* bad syntax */

static int
prsovline(line, gp, art, prevart)
register char *line;		/* malloced; will be chopped up */
register struct novgroup *gp;
register struct novart *art, *prevart;
{
	register int nf, nrefs, len;
	char *fields[MAXFIELDS], *refs[DEFREFS];
	char **refsp = refs;
	static struct novart zart;

	*art = zart;		/* make freeart safe if we bail out early */
	len = strlen(line);
	if (len > 0 && line[len-1] == '\n')
		line[len-1] = '\0';	/* make field count straightforward */
	nf = split(line, fields, MAXFIELDS, "\t");
	if (nf < MAXFIELDS - 1)	/* only "others" fields are optional */
		return PRSBAD;	/* skip this line */
	while (nf < MAXFIELDS)
		fields[nf++] = "";	/* fake missing fields */

	/*
	 * duplicate message-ids would confuse the threading code and anyway
	 * should not happen (now that relaynews suppresses multiple links
	 * within a group for the same article), so ignore any entries for
	 * duplicate message-ids.
	 */
	if (hashfetch(gp->g_msgids, fields[4]) != NULL)
		return PRSBAD;

	art->a_parent = NULL;
	art->a_refs = strsave(fields[5]); /* fields[5] will be split below */
	if (art->a_refs == NULL)
		return PRSFAIL;
	if (art->a_refs[0] != '\0') {	/* at least one ref? */
		nrefs = awksplit(fields[5], &refsp, DEFREFS, "");
		if (refsp == NULL)
			return PRSFAIL;
		if (nrefs > 0) {	/* last ref is parent */
			if (refsp[nrefs - 1] == NULL)
				return PRSFAIL;
			art->a_parent = strsave(refsp[nrefs - 1]);
			if (art->a_parent == NULL)
				return PRSFAIL;
			if (refsp != refs)
				free((char *)refsp);
		}
	}
	art->a_num = fields[0];		/* line */
	art->a_subj = fields[1];
	art->a_from = fields[2];
	art->a_date = fields[3];
	art->a_msgid = fields[4];
	/* see above for fields[5] */
	art->a_bytes = fields[6];
	art->a_lines = fields[7];
	art->a_others = fields[8];
	art->a_nxtnum = NULL;

	if (!hashstore(gp->g_msgids, art->a_msgid, (char *)art))
		return PRSFAIL;
	if (gp->g_first == NULL)
		gp->g_first = art;
	if (prevart != NULL)
		prevart->a_nxtnum = art;
	return PRSOKAY;
}

static int
prsoverview(gp)
register struct novgroup *gp;			/* cookie from novopen */
{
	register struct novart *art, *prevart = NULL;
	register int prssts;
	char *line;

	gp->g_curr = gp->g_first = NULL;
	if (gp->g_dir == NULL && gp->g_stream == NULL)
		return 0;
	if (gp->g_stream == NULL) {
		line = str3save(gp->g_dir, "/", ".overview");
		if (line == NULL)
			return 0;
		gp->g_stream = fopen(line, "r");
		free(line);
		if (gp->g_stream == NULL)
			return 0;
	}

	/* parse input and store in gp->g_msgids for later traversal */
	gp->g_msgids = hashcreate(200, (unsigned (*)())NULL);
	if (gp->g_msgids == NULL) {
		if (gp->g_dir != NULL)		/* we opened the stream? */
			(void) fclose(gp->g_stream);
		return 0;
	}

	while ((line = fgetms(gp->g_stream)) != NULL) {
		art = (struct novart *)malloc(sizeof *art);
		if (art == NULL ||
		    (prssts = prsovline(line, gp, art, prevart)) == PRSFAIL) {
			if (gp->g_dir != NULL)	/* we opened the stream? */
				(void) fclose(gp->g_stream);
			if (art != NULL)
				freeart(art);
			return 0;
		}
		if (prssts == PRSOKAY)
			prevart = art;
		else
			freeart(art);
	}
	if (gp->g_dir != NULL)		/* we opened the stream? */
		(void) fclose(gp->g_stream);
	gp->g_curr = gp->g_first;
	return 1;
}

/*
 * if this article has no parent, enter it in the roots hash table.
 * if it has a parent, make this article the parent's first child,
 * even it means making the existing first child our first sibling.
 */
/* ARGSUSED */
static
numvisit(key, data, hook)
char *key, *data, *hook;
{
	register struct novart *art = (struct novart *)data, *parent = NULL;
	register char *msgid;
	register struct novgroup *gp = (struct novgroup *)hook;

	if (gp->g_roots == NULL) {
		gp->g_roots = hashcreate(500, (unsigned (*)())NULL);
		if (gp->g_roots == NULL)	/* better not happen */
			return;
	}

	msgid = art->a_msgid;
	if (art->a_parent != NULL)
		parent = (struct novart *)hashfetch(gp->g_msgids, art->a_parent);
	if (parent != NULL) {
		if (parent->a_child1 != NULL) {
			if (art->a_sibling != NULL)
				return;	/* sibling in use; better not happen */
			art->a_sibling = parent->a_child1;
		}
		parent->a_child1 = msgid;
	} else {				/* no parent - must be a root */
		art->a_parent = NULL;
		if (!hashstore(gp->g_roots, msgid, (char *)art))
			return;		/* better not happen */
	}
}

novthread(gp)
register struct novgroup *gp;
{
	if (gp->g_first == NULL)	/* new group? */
		(void) prsoverview(gp);
	/* build trees */
	if (gp->g_first != NULL)
		hashwalk(gp->g_msgids, numvisit, (char *)gp);
}

novclose(gp)
register struct novgroup *gp;
{
	register struct novart *art, *next;

	hashdestroy(gp->g_msgids);
	hashdestroy(gp->g_roots);
	if (gp->g_dir != NULL)
		free(gp->g_dir);
	for (art = gp->g_first; art != NULL; art = next) {
		next = art->a_nxtnum;
		freeart(art);
	}
}
