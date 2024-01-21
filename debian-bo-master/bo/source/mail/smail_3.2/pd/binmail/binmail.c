/* @(#) binmail.c,v 1.5 1991/09/17 11:38:26 tron Exp */

/* This program will be used in place of /bin/mail on System V sites.
 * It looks at the arguments and decides whether to call
 * SMAIL for sending mail, or LMAIL for reading mail.
 *
 * Modified extensively from /bin/mail distributed with smail1.1.
 *
 * before installing as /bin/mail, move the stock /bin/mail to /bin/lmail
 *
 * This program works with SVR3 and SVR4 distributions from AT&T.
 */

#include <stdio.h>
#include "defs.h"

#ifndef LMAIL
#define LMAIL "/bin/lmail"
#endif

#ifndef SMAIL
#define SMAIL "/usr/lib/sendmail"
#endif

#define TRUE 1
#define FALSE 0

char *program;

extern void perror(), exit();
extern int getopt();
static void usage();

extern int optind;
extern char *optarg;

main(argc, argv)
int argc;
char *argv[];
{
	int i, j, c;		/* indexes */
	int reading = FALSE;	/* TRUE => user read mail, run LMAIL */
	int sending = FALSE;	/* TRUE => sending mail, call Smail directly */
	int F_flag = FALSE;	/* TRUE => -F flag specified, run LMAIL */
	char *debug = NULL;	/* non-NULL => debugging level */
	char *T_arg = NULL;	/* non-NULL => configuration file */

	/*
	 * parse args
	 */
	program = argv[0];
	while((c = getopt(argc, argv, "ehpPqrf:F:toswm:T:x:")) != EOF) {
		switch(c) {
		case 'e':
		case 'h':
		case 'p':
		case 'P':
		case 'q':
		case 'r':
		case 'f':
			reading = TRUE;
			break;

		case 'F':
			F_flag = TRUE;
			reading = TRUE;
			break;

		case 't':
		case 'o':
		case 's':
		case 'w':
		case 'm':
			sending = TRUE;
			break;

		case 'T':
			T_arg = optarg;
			sending = TRUE;

		case 'x':
			debug = optarg;
			break;

		default:
			usage();
			exit(1);
		}
	}
	/* any arguments left over -> sending */
	if(argc > optind && F_flag == FALSE) {
		sending = TRUE;
	}
	if((reading == TRUE) && (sending == TRUE)) {
		usage();
		exit(1);
	}

	/*
	 * form arguments
	 */
	if(sending == TRUE) {
		argv[0] = SMAIL;
		i = 1;
		if (T_arg) {
			argv[i++] = "-C";
			argv[i++] = T_arg;
		}
		if (debug) {
			argv[i++] = "-d";
			argv[i++] = debug;
		}
		for(j = optind; i < argc; i++, j++) {
			argv[i] = argv[j];
		}
		argv[i] = NULL;
	} else {
		argv[0] = LMAIL;
	}

	/*
	 * exec our real program
	 */
	(void) execvp(argv[0], argv);
	(void) fprintf(stderr, "%s: cannot exec %s: ", program, argv[0]);
	perror("");
	exit(1);
}

static void
usage()
{
	int i;
	static char *usage_text[] = {
	"Usage:	%s [-ehpPqr] [-f file] [-x debug]",
	"	%s [-tosw] [-m message_type] [-T file] [-x debug] addr ...",
	"	%s [-x debuglevel] -F addr ..."
	};

	for (i = 0; i < sizeof(usage_text) / sizeof(*usage_text); i++) {
		fprintf(stderr, usage_text[i], program);
		putc('\n', stderr);
	}
}
