/* $Id: getopt.c,v 1.1 1991/05/13 18:36:55 chip Exp $
 *
 * A version of the public-domain getopt() function, as found
 * in the SVID and fine Unix manuals everywhere.
 *
 * $Log: getopt.c,v $
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include <stdio.h>
#include "config.h"
#include "misc.h"

/*----------------------------------------------------------------------
 * Get command line options.
 * This is essentially the public domain version, just reformatted to
 * match the rest of the deliver program.
 */

#ifndef HAS_GETOPT

int opterr = 1;
int optind = 1;
int optopt = 0;
char *optarg = NULL;

#define ERR(what,c) \
    if (!opterr) {} else fprintf(stderr,"%s: %s -- %c\n", argv[0], what, c)

int
getopt(argc, argv, opts)
int argc;
char **argv;
char *opts;
{
    static int sp = 1;
    int c;
    char *cp;

    if (sp == 1)
    {
	if (optind >= argc
	    || argv[optind][0] != '-' || argv[optind][1] == '\0')
	    return EOF;

	if (strcmp(argv[optind], "--") == NULL)
	{
	    optind++;
	    return EOF;
	}
    }

    optopt = c = argv[optind][sp];

    if (c == ':' || (cp = strchr(opts, c)) == NULL)
    {
	ERR("illegal option", c);
	if (argv[optind][++sp] == '\0')
	{
	    optind++;
	    sp = 1;
	}
	return '?';
    }

    if (*++cp == ':')
    {
	if (argv[optind][sp + 1] != '\0')
	    optarg = &argv[optind++][sp + 1];
	else if (++optind >= argc)
	{
	    ERR("option requires an argument", c);
	    sp = 1;
	    return '?';
	}
	else
	    optarg = argv[optind++];

	sp = 1;
    }
    else
    {
	if (argv[optind][++sp] == '\0')
	{
	    sp = 1;
	    optind++;
	}

	optarg = NULL;
    }

    return c;
}

#endif	/* !HAS_GETOPT */
