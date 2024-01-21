/*
 * batcher - send a bunch of news articles as an unbatch script
 *
 * Usage: batcher listfile
 *
 *	where listfile is a file containing a list, one per line, of
 *	names of files containing articles.  Only the first
 *	field of each line is looked at, so there can be more if needed
 *	for other things.  Non-absolute pathnames are understood to lie
 *	under the current directory; chdiring to the right place is the
 *	parent's problem.
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fgetfln.h"

#ifndef READSIZE
#define READSIZE 8192	/* allows for even 4.2 worst case file systems */
#endif
char buffer[READSIZE];

char *progname;

int debug = 0;			/* Debugging? */

char filler[] = "--- Filler to cover I/O error ---\n";
#define	NFILL	(sizeof(filler)-1)

main(argc, argv)
int argc;
char *argv[];
{
	int c;
	int errflg = 0;
	extern int optind;
	extern char *optarg;
	register FILE *list;

	progname = argv[0];
	while ((c = getopt(argc, argv, "x")) != EOF)
		switch (c) {
		case 'x':	/* Debugging. */
			debug++;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg || optind < argc-1) {
		(void) fprintf(stderr,
			"Usage: batcher [listfile]\n");
		exit(2);
	}

	if (optind == argc-1) {
		list = fopen(argv[optind], "r");
		if (list == NULL)
			error("unable to open `%s'", argv[optind]);
		procfile(list, argv[optind]);
	} else
		procfile(stdin, "<standard input>");

	exit(0);
}

/*
 - procfile - process a file of article names
 */
procfile(list, filename)
FILE *list;
char *filename;
{
	char *article;

	if (debug)
		fprintf(stderr, "procfile(%s)\n", filename);
	while ((article = fgetline(list, (size_t *)NULL)) != NULL)
		procart(article);

	if (!feof(list))
		error("fgetline failure in `%s'", filename);
}

/*
 - procart - process an article
 */
procart(article)
register char *article;
{
	register int artfile;
	register int count;
	struct stat sbuf;
	register char *endp;

	if (debug)
		fprintf(stderr, "procart(%s)\n", article);
	endp = strchr(article, '\t');
	if (endp == NULL)
		endp = strchr(article, ' ');
	if (endp != NULL)
		*endp = '\0';
	if (debug)
		fprintf(stderr, "article is %s\n", article);

	artfile = open(article, 0);
	if (artfile < 0) {
		/*
		 * Can't read the article.  This isn't necessarily a
		 * disaster, since things like cancellations will do
		 * this.  Mumble and carry on.
		 */
		if (debug)
			warning("can't find `%s'", article);
		return;
	}

	if (fstat(artfile, &sbuf) < 0)
		error("internal disaster, can't fstat", "");

	printf("#! rnews %ld\n", (long)sbuf.st_size);
	while ((count = read(artfile, buffer, sizeof buffer)) > 0) {
		if (fwrite(buffer, sizeof(char), count, stdout) != count)
			error("write failure in `%s'", article);
		sbuf.st_size -= count;
	}
	if (count < 0) {
		/* tricky -- should avoid infinite loops on disk errors */
		warning("read failure in `%s'", article);
		while (sbuf.st_size >= NFILL) {
			if (fwrite(filler, sizeof(char), NFILL, stdout) != NFILL)
				error("write failure in `%s'", article);
			sbuf.st_size -= NFILL;
		}
		while (sbuf.st_size > 0) {
			putc('\n', stdout);
			sbuf.st_size--;
		}
	}

	(void) close(artfile);
}
