/* @(#) mkline.c,v 1.3 1992/07/11 11:40:08 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * mkline.c:
 *	Take an alias, pathalias or similarly formed file and collapse
 *	into single-line records with comments stripped.
 */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "defs.h"
#include "smail.h"
#include "extern.h"
#include "field.h"
#include "addr.h"
#include "dys.h"
#include "exitcodes.h"

char *program;				/* argv[0] from main */
int debug = 0;
FILE *errfile = stderr;
int use_tabs_p = FALSE;
int ignore_keys_p = FALSE;

/*ARGSUSED*/
void
main(argc, argv)
    int argc;
    char **argv;
{
    int list_files_p = FALSE;		/* TRUE for -l option */

    program = *argv++;

    /* if -l, process mailing list files */
    while (*argv && (*argv)[0] == '-' && (*argv)[1] != '\0') {
	register char *p;

	for (p = *argv + 1; *p; p++) {
	    switch (*p) {
	    case 'l':
		list_files_p = TRUE;
		break;

	    case 't':
		use_tabs_p = TRUE;
		break;

	    case 'n':
		ignore_keys_p = TRUE;
		break;

	    default:
		(void) fprintf(stderr, "Usage: %s [-ltn] [file ...]\n",
			       program);
		exit(EX_USAGE);
	    }
	}
	argv++;
    }

    if (*argv == NULL) {
	if ((list_files_p? mklistline(stdin): mkaliasline(stdin)) < 0) {
	    exit(EX_DATAERR);
	}
    }
    while (*argv) {
	FILE *f;

	if (EQ(*argv, "-")) {
	    f = stdin;
	} else {
	    f = fopen(*argv, "r");
	    if (f == NULL) {
		(void) fprintf(stderr, "%s: cannot open %s: ", program, *argv);
		perror("");
		exit(errno);
	    }
	}
	if ((list_files_p? mklistline(f): mkaliasline(f)) < 0) {
	    exit(EX_DATAERR);
	}
	argv++;
    }

    exit(0);
}

mkaliasline(f)
    FILE *f;
{
    char *s;

    /*
     * read an entry of the form:
     *
     *		name:value
     *  or	name value
     */
    while (s = read_entry(f)) {
	register char *sp = s;
	char *error;
	struct token *tokens;
	register struct token *toks;

	if (! ignore_keys_p) {
	    while (!isspace(*sp) && *sp != ':') {
		sp++;
	    }

	    /* split the line in two */
	    *sp++ = '\0';
	}

	/* turn it into tokens */
	error = tokenize(sp, &tokens, TRUE, TRUE);

	if (error) {
	    (void) fprintf(stderr, "%s: tokenize error: %s\n", program, error);
	    return -1;
	}

	if (! ignore_keys_p) {
	    /* write out the name */
	    printf((use_tabs_p? "%s\t": "%s:"), s);
	}

	/* write out the data, with most white space squeezed out */
	for (toks = tokens; toks; toks = toks->succ) {
	    fputs(toks->text, stdout);
	    if ((QUOTETOK(toks->form) || TEXTTOK(toks->form)) &&
		toks->succ &&
		(QUOTETOK(toks->succ->form) || TEXTTOK(toks->succ->form)))
	    {
		putchar(' ');
	    }
	}
	putchar('\n');
    }

    return 0;
}

mklistline(f)
    register FILE *f;
{
    struct str s;
    register int c;
    char *error = NULL;
    struct addr *list = NULL;
    struct addr *cur;

    STR_INIT(&s);
    while ((c = getc(f)) != EOF) {
	STR_NEXT(&s, c);
    }
    STR_NEXT(&s, '\0');
    (void) process_field(NULL, s.p, NULL, NULL, &list, F_ALIAS, &error);
    if (error) {
	(void) fprintf(stderr, "%s: %s\n", program, error);
	return -1;
    }

    for (cur = list; cur; cur = cur->succ) {
	printf("%s\n", cur->in_addr);
    }

    return 0;
}

/*
 * standalone versions of some referenced routines
 */
char *
xmalloc(len)
    int len;
{
    char *malloc();
    register char *ret = malloc(len);

    if (ret == NULL) {
	(void) fprintf(stderr, "%s: out of memory!\n", program);
    }
    return ret;
}

char *
xrealloc(s, len)
    char *s;
    int len;
{
    char *realloc();
    register char *ret = realloc(s, len);

    if (ret == NULL) {
	(void) fprintf(stderr, "%s: out of memory!\n", program);
    }
    return ret;
}

void
xfree(s)
    char *s;
{
    free(s);
}

/*ARGSUSED*/
char *
qualify_domain(s)
    char *s;
{
    return NULL;
}

struct addr *
alloc_addr()
{
    register struct addr *ret = (struct addr *)xmalloc(sizeof(struct addr));

    bzero((char *)ret, sizeof(struct addr));
    return ret;
}

/*ARGSUSED*/
char *
preparse_address(s, error)
    char *s;
    char **error;
{
    return s;
}
