/* @(#) mksort.c,v 1.5 1992/09/06 01:10:32 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * mksort.c:
 *	Take a list of lines and sort them.  If the flag -f is given,
 *	then ignore case in comparisons.
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

static int cmp(), cmpic();
static char **mkvectors();
static int read_lines();

/*ARGSUSED*/
void
main(argc, argv)
    int argc;
    char **argv;
{
    struct str strings;
    char **mkvectors();
    int (*compare_fun)() = cmp;
    int ct = 0;
    char **v;
    int i;

    program = *argv++;

    /* if -f, then perform case-folded comparisons */
    if (*argv && EQ(*argv, "-f")) {
	compare_fun = cmpic;
	argv++;
    }

    STR_INIT(&strings);
    if (*argv == NULL) {
	ct += read_lines(&strings, stdin);
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
	ct += read_lines(&strings, f);
	argv++;
    }

    v = mkvectors(ct, strings.p);
    qsort((char *)v, ct, sizeof(char *), compare_fun);

    for (i = 0; i < ct; i++) {
	printf("%s\n", v[i]);
    }

    exit(0);
}

static int
read_lines(ssp, f)
    register struct str *ssp;
    register FILE *f;
{
    register int c;
    int ct = 0;

    while ((c = getc(f)) != EOF) {
	if (c == '\n') {
	    STR_NEXT(ssp, '\0');
	    ct++;
	} else {
	    STR_NEXT(ssp, c);
	}
    }

    return ct;
}

static char **
mkvectors(ct, strings)
    int ct;				/* count of strings */
    char *strings;			/* null-terminated strings */
{
    char **v;
    register char **vp;
    register char *sp = strings;

    v = vp = (char **)xmalloc(ct * sizeof(char *));
    for (sp = strings; ct; ct--, sp += strlen(sp) + 1) {
	*vp++ = sp;
    }
    return v;
}

static int
cmp(a, b)
    char **a;
    char **b;
{
    char *s1 = *a;
    char *s2 = *b;
    register int c1, c2;

    for (;;) {
	c1 = *s1++;
	c2 = *s2++;
	if (isspace(c1) || c1 == ':')
	    c1 = '\0';
	if (isspace(c2) || c2 == ':')
	    c2 = '\0';
	if (c1 != c2)
	    return c1 - c2;
	if (c1 == '\0')
	    break;
    }
    return c1 - c2;
}

static int
cmpic(a, b)
    char **a;
    char **b;
{
    char *s1 = *a;
    char *s2 = *b;
    register int c1, c2;

    for (;;) {
	c1 = lowercase(*s1++);
	c2 = lowercase(*s2++);
	if (isspace(c1) || c1 == ':')
	    c1 = '\0';
	if (isspace(c2) || c2 == ':')
	    c2 = '\0';
	if (c1 != c2)
	    return c1 - c2;
	if (c1 == '\0')
	    break;
    }
    return c1 - c2;
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
