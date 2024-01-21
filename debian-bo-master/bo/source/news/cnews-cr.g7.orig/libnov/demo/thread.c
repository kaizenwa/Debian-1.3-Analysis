/*
 * thread - construct skein of threads from .overview index file
 */

#include <stdio.h>
#include <string.h>
#include <fgetfln.h>
#include <hash.h>
#include <hdbm.h>
#include "newsoverview.h"

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

/* imports */
extern int optind;
extern char *optarg;
extern char *strsave(), *str3save(), *emalloc(), *malloc();

/* exports */
char *progname = "";
int debug;

/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c, errflg = 0;

	if (argc > 0)
		progname = argv[0];
	while ((c = getopt(argc, argv, "d")) != EOF)
		switch (c) {
		case 'd':
			++debug;
			break;
		default:
			errflg++;
			break;
		}
	if (errflg) {
		(void) fprintf(stderr, "usage: %s [-d] [file]...\n", progname);
		exit(2);
	}

	if (optind >= argc)
		process(stdin, "stdin");
	else
		for (; optind < argc; optind++)
			if (STREQ(argv[optind], "-"))
				process(stdin, "-");
			else {
				FILE *in = NULL;

				process(in, argv[optind]);
			}
	exit(0);
}

/* ARGSUSED */
static
rootvisit(key, data, hook)
char *key, *data, *hook;
{
	prtree((struct novgroup *)hook, ((struct novart *)data)->a_msgid, 1);
}

/*
 * process - process input file
 */
/* ARGSUSED in */
process(in, inname)
FILE *in;
char *inname;
{
	register struct novgroup *gp;

	gp = novopen(inname);
	if (gp == NULL)
		error("can't open overview file for group `%s'", inname);

	novthread(gp);

	/* traverse each tree from root, printing as we go */
	if (gp->g_roots != NULL)
		hashwalk(gp->g_roots, rootvisit, (char *)gp);

	novclose(gp);
}

prtree(gp, msgid, level)
register struct novgroup *gp;
register char *msgid;
register int level;
{
	register int i;
	register struct novart *art;

	if (gp == NULL || gp->g_msgids == NULL || msgid == NULL)
		return;

	art = (struct novart *)hashfetch(gp->g_msgids, msgid);
	if (art == NULL)
		return;

#ifdef OLDSTYLE
	(void) printf("%d %s\t%s\t%s\n", level, msgid, art->a_subj, art->a_from);
#else						/* OLDSTYLE */

	for (i = 1; i < level; i++)
		(void) putchar('\t');
	(void) printf("%s\t%s", art->a_from, msgid);
	if (level == 1)
		(void) printf("\t%s", art->a_subj);
	(void) putchar('\n');

	prtree(gp, art->a_child1, level + 1);
	prtree(gp, art->a_sibling, level);	/* next sibling */
#endif						/* OLDSTYLE */
}
