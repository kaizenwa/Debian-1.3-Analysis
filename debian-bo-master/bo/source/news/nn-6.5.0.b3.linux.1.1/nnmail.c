/*
 *	(c) Copyright 1990, Kim Fabricius Storm.  All rights reserved.
 *
 * 	nnmail - a mailer that understands @ addressing
 *		 when you don't have sendmail or smail
 */

#include "config.h"
#include "library.h"

#include "options.h"

char * MAILER = MAILX;
static int print_vers, test_mode;

Option_Description( mail_options ) {

    'v', Bool_Option( print_vers ),
    'm', String_Option( MAILER ),
    't', Bool_Option( test_mode ),

    '\0',
};

extern char **environ;
extern FILE *route_trace;

main(argc, argv)
int argc;
char **argv;
{
    int i, n;
    char route[512];
    char *getenv(), *envmail;

    if (envmail = getenv("NNMAILER"))
	MAILER = envmail;

    n = parse_options(argc, argv, (char *)NULL,
		      mail_options, (char *)NULL, NULL_FCT);

    if (print_vers) {
	printf("Release %s\n", version_id);
	nn_exit(0);
    }

#ifndef HAVE_ROUTING
    if (test_mode) {
	route_trace = stdout;
    }
#endif

    argv[0] = MAILER;

#ifndef HAVE_ROUTING
    for (i = 1; i <= n; i++)
	if (reroute(route, argv[i])) {
	    if (test_mode) {
		printf("%s \t-->  %s\n", argv[i], route);
		continue;
	    }
	    argv[i] = newstr(strlen(route)+1);
	    strcpy(argv[i], route);
	} else
	    if (test_mode)
		printf("%s \t***  no route found\n", argv[i]);
#endif

    if (test_mode) nn_exit(0);

    execve(MAILER, argv, environ);
    fprintf(stderr, "Mailer '%s' not found\n", MAILER);
    nn_exit(7);
}

void
nn_exit(n)
{
    exit(n);
}

/*VARARGS*/
nn_exitmsg(va_alist)
va_dcl
{
    char *fmt;
    int n;
    use_vararg;

    start_vararg;
    n = va_arg1(int);
    fmt = va_arg2(char *);
    vprintf(fmt, va_args3toN);
    putchar(NL);
    end_vararg;

    nn_exit(n);
    /*NOTREACHED*/
}

#ifdef HAVE_JOBCONTROL
void
suspend_nn()
{}
#endif
