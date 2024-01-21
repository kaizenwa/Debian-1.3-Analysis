/* @(#) getopt.c,v 1.3 1994/12/14 13:39:39 nm4 Exp */
/*
 * This file is in the public domain.
 */

/*
 * shell interface to the getopt(3) function for implementing a standard
 * option interface.  Usage in a shell script:
 *
 *	set -- `getopt [-n progname] [-q] $*`
 *
 * -n sets the program name to use for usage messages, -q disables
 * a usage message from getopt(3).  Beyond these additional flags,
 * see the System V getopt(1) man page for usage instructions.
 */
#include <stdio.h>
extern char *optarg;
extern int optind, opterr;

int prevopt = 0;
char *program;

main(argc, argv)
	int argc;
	char **argv;
{
	int c;
	char *optstr;
	int exitflag = 0;		/* != 0 ==> error encountered */

	program = *argv;

	for (;;) {
		if (argc > 2 && strcmp(argv[1], "-q") == 0) {
			opterr = 0;
			argv++;
			--argc;
			continue;
		}
		if (argc > 2 && strncmp(argv[1], "-n", 2) == 0) {
			if (argv[1][2] == '\0') {
				argv += 2;
				argc -= 2;
				program = argv[0];
			} else {
				argv++;
				--argc;
				program = &argv[0][2];
			}
			continue;
		}
		break;
	}
	if (argc < 2) {
		fprintf(stderr,
			"Usage: %s [ -q ] [ -n name ] opt-string [args]\n",
			argv[0]);
		exit(2);
	}

	optstr = *++argv;
	--argc;
	*argv = program;
	while ((c = getopt(argc, argv, optstr)) != EOF) {
		static char buf[] = "-o";

		/* catch errors for later exiting */
		if (c == '?') {
			exitflag = 1;
		}

		/* process the arg we have found */
		if (c != '?' || opterr == 0) {
			buf[1] = c;
			outarg(buf);
		}
		if (optarg) {
			outarg(optarg);
		}
	}
	outarg("--");
	while (optind < argc) {
		outarg(argv[optind++]);
	}
	putchar('\n');
	exit(exitflag);
}

outarg(s)
	char *s;
{
	if (prevopt) {
		putchar(' ');
	}
	prevopt = 1;
	fputs(s, stdout);
}
