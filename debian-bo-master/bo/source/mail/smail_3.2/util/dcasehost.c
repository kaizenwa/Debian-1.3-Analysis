/* @(#) dcasehost.c,v 1.4 1992/09/06 01:10:10 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * dcasehost - downcase the host field of standard pathalias output
 *
 * Downcase the first field (non-while chars up to whitespace).
 * In the common case of pathalias, this downcases the hostname
 * while leaving the path untouched.
 *
 * If the -c option is used, then input is assumed to be of
 * pathalias -c form:
 *	cost hostname path
 * and is converted to:
 *	hostname path cost
 * before downcase is done.
 */

#include <stdio.h>
#include <ctype.h>
#include "defs.h"

char *program;			/* argv[0] from main */

char buf[BUFSIZ+1+1];		/* i/o line buffer */
char buf2[BUFSIZ+1+1];		/* holder for field one switch (-c) */
char buf3[BUFSIZ+1+1];		/* holder for other fields (-c) */

void
main(argc, argv)
    int argc;		/* arg count */
    char *argv[];	/* args */
{
    char *f1_white;	/* first whitespace beyond field 1 */
    int cflag = 0;	/* cflag == 1 ==> field switch */

    /*
     * parse args
     */
    program = argv[0];
    if ( argc > 2 ) {
	fprintf(stderr, "%s: usage: %s [-c]\n", program, program);
	exit(-1);
    } else if (argc == 2) {
	if (strcmp(argv[1], "-c") != 0) {
	    fprintf(stderr, "%s: usage: %s [-c]\n", program, program);
	    exit(-1);
	}
	cflag = 1;
    }

    /*
     * process input lines until EOF
     */
    while (fgets(buf, BUFSIZ, stdin) != NULL) {

	/*
	 * if -c, move the fields
	 */
	if (cflag) {
	    char *f2_field;	/* first char of field 2 */
	    char *f_end;	/* last char */

	    /* find first whitespace char beyond field 1 */
	    f1_white = strpbrk(&buf[strspn(buf, " \t\n")], " \t\n");

	    /* find first char of field 2 */
	    if (f1_white != NULL) {
		f2_field = &f1_white[strspn(f1_white, " \t\n")];
	    }

	    /* find last char */
	    f_end = &buf[strlen(buf)-1];

	    /* switch fields if firewall checks  allow it */
	    if (f1_white != NULL && *f1_white != '\0' &&
		*f2_field != '\0' &&
		f_end > buf && *f_end == '\n') {

	        /* save field 1 with newline and NULL */
	        strncpy(buf2, buf, (int)(f1_white-buf));
		buf2[f1_white-buf] = '\n';
		buf2[f1_white-buf+1] = '\0';

		/* move field 2 and beyond to front of line */
		strcpy(buf3, f2_field);
		strcpy(buf, buf3);

		/*
		 * add field 1 on the end of new line
		 *
		 * use white space before field 1 to separate
		 * field 1 from the end of the line, if we can
		 */
		if (buf2[0] == ' ' || buf2[0] == '\t') {
		    /* overwrite the old trailing newline */
		    strncpy(&buf[f_end-f2_field], buf2, f1_white-buf+2);
		    
		} else {
		    /* separate old line end and field 1 with a tab */
		    buf[f_end-f2_field] = '\t';
		    strncpy(&buf[f_end-f2_field+1], buf2, f1_white-buf+2);
		}
	    }
	}

	/*
	 * downcase the first field
	 */
	/* find first whitespace char beyond field 1 */
	f1_white = strpbrk(&buf[strspn(buf, " \t\n")], " \t\n");
	/* downcase field 1 if it exists */
	if (f1_white != NULL) {
	    register char *p;	/* index */

	    for (p=buf; p < f1_white; ++p) {
		*p = lowercase(*p);
	    }
	}

	/*
	 * print the resulting line
	 */
	fputs(buf, stdout);
    }
    exit(0);
}
