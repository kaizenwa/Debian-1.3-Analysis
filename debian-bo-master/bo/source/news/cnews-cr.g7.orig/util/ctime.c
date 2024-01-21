/*
 * ctime time_t ... - print the ascii time of time_t(s)
 */

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <sys/types.h>
#include <stdlib.h>

/* imports */
extern char *ctime(), *asctime();
extern struct tm *gmtime();
extern time_t time();
extern char *ascingmtime();

/* Forwards. */
extern void process();
extern int optind;
extern char *optarg;

char *progname;

/* privates */
static int gmt = 0;
static int inetfmt = 0;

/*
 - main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	register int c, errflg = 0;

	progname = argv[0];

	while ((c = getopt(argc, argv, "iu")) != EOF)
		switch (c) {
		case 'i':
			inetfmt++;
			gmt++;		/* too painful to do local time */
			break;
		case 'u':
			++gmt;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg || optind == argc) {
		(void) fprintf(stderr, "Usage: %s [-iu] ascii_time ...\n",
			       progname);
		exit(2);
	}

	for (; optind < argc; optind++)
		process(argv[optind]);
	exit(0);
}

/*
 * process - print time_t of tm
 */
void
process(tms)
char *tms;
{
	time_t tm;

	if (strcmp(tms, "now") == 0)
		tm = time(&tm);
	else
		tm = atol(tms);
	if (gmt) {
		register struct tm *prstime = gmtime(&tm);

		if (inetfmt)
			(void) fputs(ascingmtime(prstime), stdout);
		else
			(void) fputs(asctime(prstime), stdout);
	} else
		(void) fputs(ctime(&tm), stdout);
}
