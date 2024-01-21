/*
 * sizeof - report total size of files
 *
 * You might ask, why couldn't this be a shell program invoking ls -l?
 * Well, apart from the variations in the format of ls -l, there's also
 * the problem that the Berkloid ls -l doesn't follow symlinks unless
 * asked to (with an unportable option).
 */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

int debug = 0;
int indiv = 0;
off_t multiplier = 1;
char *progname;

extern void error();

/*
 - main - do it all
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c;
	int errflg = 0;
	struct stat statbuf;
	extern int optind;
	extern char *optarg;
	register off_t total = 0;

	progname = argv[0];

	while ((c = getopt(argc, argv, "im:x")) != EOF)
		switch (c) {
		case 'i':	/* Individual files. */
			indiv = 1;
			break;
		case 'm':	/* Multiplier. */
			multiplier = (off_t)atol(optarg);
			break;
		case 'x':	/* Debugging. */
			debug++;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg) {
		fprintf(stderr, "usage: %s ", progname);
		fprintf(stderr, "[file] ...\n");
		exit(2);
	}

	for (; optind < argc; optind++)
		if (strcmp(argv[optind], "-m") == 0) {
			optind++;
			if (optind < argc)
				multiplier = (off_t)atol(argv[optind]);
		} else if (stat(argv[optind], &statbuf) >= 0) {
			total += statbuf.st_size * multiplier;
			if (debug || indiv)
				printf("%s %ld\n", argv[optind],
					(long)(statbuf.st_size * multiplier));
		}
	if (!indiv)
		printf("%ld\n", (long)total);
	exit(0);
}
