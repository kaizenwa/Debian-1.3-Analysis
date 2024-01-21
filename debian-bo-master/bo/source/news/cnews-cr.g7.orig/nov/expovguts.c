/*
 * expovguts - do the inner-loop work of expov
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "fgetfln.h"
#include "news.h"

int debug = 0;
char *progname;

char *inname;
char *tmpname;

extern void error();
#define	mkprogname(a)	(((a) == NULL) ? "?noname?" : (a))

void mapfill();
void filter();

/*
 - main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c;
	int errflg = 0;
	long start, stop;
	char *amap;
	FILE *in;
	FILE *out;
	extern int optind;
	extern char *optarg;
	extern FILE *efopen();

	progname = mkprogname(argv[0]);

	while ((c = getopt(argc, argv, "X")) != EOF)
		switch (c) {
		case 'X':	/* Debugging. */
			debug++;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg || optind != argc-3) {
		fprintf(stderr, "usage: %s ", progname);
		fprintf(stderr, "max min overviewfile\n");
		exit(2);
	}

	start = atol(argv[optind+1]);
	stop = atol(argv[optind]);		/* tentatively */
	inname = argv[optind+2];
	if (start == 0) {
		fprintf(stderr, "%s: in expiring `%s',\n", progname, inname);
		fprintf(stderr, "\tfound problem in active file:  min == 0\n",
							argv[optind+1]);
		exit(2);
	}
	if (start > stop+1) {
		fprintf(stderr, "%s: in expiring `%s',\n", progname, inname);
		fprintf(stderr, "\tfound problem in active file:  min (%s) > max (%s) + 1\n",
					argv[optind+1], argv[optind]);
		exit(2);
	}
	stop += 100;				/* a bit of headroom */
	if (debug)
		printf("start %ld, stop %ld\n", start, stop);

	tmpname = str3save(inname, ".tmp", "");

	amap = malloc((size_t)(stop - start));
	if (amap == NULL)
		error("cannot allocate map", "");
	memset(amap, (size_t)(stop - start), 0);

	mapfill(amap, start, stop, stdin);

	in = efopen(inname, "r");
	out = efopen(tmpname, "w");
	filter(in, out, amap, start, stop);
	if (ferror(in) || fclose(in) != 0)
		error("I/O error in reading %s", inname);
	if (ferror(out) || fclose(out) != 0)
		error("I/O error in writing %s", tmpname);

	if (rename(tmpname, inname) != 0)
		error("cannot rename %s", tmpname);

	exit(0);
}

/*
 - mapfill - fill in the map with the input filenames
 */
void
mapfill(map, start, stop, f)
char *map;
long start;
long stop;
FILE *f;
{
	register char *line;
	register char *p;
	register long fno;
	register int nbad = 0;

	while ((line = fgetline(f, (size_t *)NULL)) != NULL) {
		fno = 0;
		for (p = line; isdigit(*p); p++)
			fno = fno*10 + (*p) - '0';
		if (debug)
			printf("name `%s', number %ld\n", line, fno);
		if (*p == '\0') {	/* it was all numeric */
			if (fno >= start && fno < stop) {
				map[fno - start] = 1;
				if (debug)
					printf("setting %ld\n", fno-start);
			} else if (fno < start)
				nbad++;
			/* fno >= stop presumably will be filed later */
		}
	}

	if (nbad > 0) {
		fprintf(stderr, "%s: (warning) in expiring `%s',\n", progname,
								inname);
		fprintf(stderr, "\tfound %d files with numbers < min (%ld),\n",
								nbad, start);
		fprintf(stderr, "\tindicating problems with upact\n");
	}
}

/*
 - filter - pass overview file through, checking against map
 */
void
filter(in, out, map, start, stop)
FILE *in;
FILE *out;
char *map;
long start;
long stop;
{
	char *line;
	register char *p;
	register long no;

	while ((line = fgetline(in, (size_t *)NULL)) != NULL) {
		p = strchr(line, '\t');
		if (p != NULL) {	/* quietly drop malformed lines */
			*p = '\0';
			no = atol(line);
			*p = '\t';
			if (no >= start && (no >= stop || map[no-start] != 0)) {
				fputs(line, out);
				putc('\n', out);
			}
		}
	}
}
