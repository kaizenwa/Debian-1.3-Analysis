/*
 * ovsplit - distribute mkov output lines into overview files
 * Kinda annoying that not everybody has a modern awk, which can do this...
 * but this is probably faster anyway.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fgetfln.h>

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

/* imports */
extern int optind;
extern char *optarg;
extern char *strsave(), *str3save();
extern FILE *efopen();

/* exports */
char *progname = "";
int debug;

char *dirname = NULL;
char *ovname = NULL;
FILE *ov = NULL;

/*
 * main - do it
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c, errflg = 0;
	char *line;
	register char *t;
	register char *prefix;

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
	if (errflg || optind != argc-1) {
		(void) fprintf(stderr, "usage: %s [-d] $NEWSOV\n",
			progname);
		exit(2);
	}
	prefix = str3save(argv[optind], "/", "");

	while ((line = fgetline(stdin, (size_t *)NULL)) != NULL) {
		t = strchr(line, '\t');
		if (t == NULL)
			error("bad input line %.40s...", line);
		*t = '\0';
		if (dirname == NULL || !STREQ(dirname, line)) {
			if (dirname != NULL)
				free(dirname);
			if (ov != NULL)
				if (nfclose(ov) == EOF)
					error("nfclose(%s) failed", ovname);
			if (ovname != NULL)
				free(ovname);
			dirname = strsave(line);
			ovname = str3save(prefix, dirname, "/.overview");
			ov = efopen(ovname, "a");
		}
		*t = '\t';
		t++;
		if (debug)
			printf("%.40s... to %s\n", t, ovname);
		fputs(t, ov);
		putc('\n', ov);
	}

	if (ov != NULL)
		if (nfclose(ov) == EOF)
			error("nfclose(%s) failed", ovname);
	exit(0);
}
