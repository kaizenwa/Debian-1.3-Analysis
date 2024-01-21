/*
 *	(c) Copyright 1992, Luc Rooijakkers.  All rights reserved.
 *
 *	Character set support (rudimentary)
 */

#include "config.h"
#include "chset.h"

static struct chset chsets[]= {
    "us-ascii",		7,
    "iso-8859-1",	8,
    "iso-8859-2",	8,
    "iso-8859-3",	8,
    "iso-8859-4",	8,
    "iso-8859-5",	8,
    "iso-8859-6",	8,
    "iso-8859-7",	8,
    "iso-8859-8",	8,
    "iso-8859-9",	8,
    "unknown",		0,
    NULL,		0,
};

export struct chset *curchset = chsets;

struct chset *getchset(name)
char *name;
{
    struct chset *csp;
    char *sp;

    for (sp = name; *sp; sp++)
	if (isupper(*sp))
	    *sp = tolower(*sp);

    for (csp = chsets; csp->cs_name != NULL; csp++) {
	if (strcmp (csp->cs_name, name) == 0)
	    return csp;
    }

    return NULL;
}
