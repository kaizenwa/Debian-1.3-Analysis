/* @(#) execm.c,v 1.2 1990/10/24 05:17:22 tron Exp */

/*
 * execm.c
 *
 * This program is a substitute for Xenix's /usr/lib/mail/execmail.
 * It is for use in systems running Smail 3.  If you install this program
 * as /usr/lib/mail/execmail, and then add the line "set execmail" to
 * /usr/lib/mail/mailrc, then the Xenix /usr/bin/mail program will accept
 * addresses of the form "user@some.domain".
 *
 * NOTE:  Installing this program disables the Xenix aliasing and routing
 *        facilities (and Micnet).  If you really _need_ Micnet, then it
 *        may be possible to configure Smail to call the original
 *        execmail program.  Send me a note if you want to try it.
 *
 * Written by Chip Salzenberg at A T Engineering <chip@ateng.uucp>.
 * Released to Usenet on 01 Dec 1987.
 * Modified 25 Jul 1988 for use with Smail 3.x.
 *
 * Do what you want with this program.
 * I'm not responsible for lost mail -- not that I expect problems. :-)
 */

#include <stdio.h>
#include <signal.h>

/*
 * Library functions
 */
extern  char    *malloc();

/*
 * Globals used for getopt()
 */
extern  char    *optarg;
extern  int     optind, opterr;

/*
 * The Program
 */
main(argc, argv)
int     argc;
char    **argv;
{
	char    *progname = argv[0];
	char    **sav;
	int     sac, ch, badopts;

	/*
	 * Allocate memory for new arguments, and set the program name.
	 * Note the magic number eight; sorry.
	 */
	if ((sav = (char **) malloc((argc + 8) * sizeof(char *))) == 0)
	{
		fprintf(stderr, "%s: out of memory?!\n", progname);
		exit(1);
	}

	sav[0] = "smail";
	sac = 1;

	/*
	 * Translate the execmail options to Smail 3.x options.
	 */
	badopts = 0;
	while ((ch = getopt(argc, argv, "f:h:mnr")) != EOF)
	{
		switch (ch)
		{
		case 'f':       /* Who is this message from? */
			sav[sac++] = "-f";
			sav[sac++] = optarg;
			break;

		case 'h':       /* Max hop count */
			sav[sac++] = "-h";
			sav[sac++] = optarg;
			break;

		case 'm':       /* Include sender in alias expansion */
			sav[sac++] = "-m";
			break;

		case 'n':       /* Disable alias expansion */
			sav[sac++] = "-n";
			break;

		case 'r':       /* Remote -- via UUCP, not Micnet */
			break;  /* smail doesn't understand Micnet anyway */

		default:        /* Illegal option */
			++badopts;
			break;
		}
	}

	/*
	 * If invalid options or no addresses, print usage message and leave.
	 */
	if (badopts || optind >= argc)
	{
		fprintf(stderr,
		  "usage: %s [-f from][-h hopcount][-m][-n][-r] addresses\n",
		  progname);
		exit(1);
	}

	/*
	 * Finish the argument list.
	 */
	while (optind < argc)
		sav[sac++] = argv[optind++];
	sav[sac] = 0;

	/*
	 * Finally, let smail take over.
	 */
	execv("/bin/smail", sav);
	execv("/usr/bin/smail", sav);

	fprintf(stderr, "%s: can't execute smail!\n", progname);
	exit(1);
}
