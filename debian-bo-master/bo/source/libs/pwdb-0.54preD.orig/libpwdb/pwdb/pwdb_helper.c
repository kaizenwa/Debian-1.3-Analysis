/*
 * $Id: pwdb_helper.c,v 1.1 1996/10/16 22:16:04 morgan Exp $
 *
 * This file contains the pwdb public helper functions.
 */

/*
 * $Log: pwdb_helper.c,v $
 * Revision 1.1  1996/10/16 22:16:04  morgan
 * Initial revision
 *
 */

#include <pwdb/pwdb_public.h>
#include "pwdb_module.h"

/*
 * clean up a string and then free() it. usage: old = _pwdb_delete(old)
 */

char *_pwdb_delete_string(char *s)
{
    char *x=s;

    if (x) while (*x)
        *x++ = '\0';
    if (s)
        free(s);
    return (char *)NULL;
}

/*
 * return a copy of a string.
 */

char *_pwdb_dup_string(const char *x)
{
    char *r;

    if (x == NULL)
        return NULL;

    if ((r = malloc(strlen(x)+1)) == NULL)
        return NULL;

    strcpy(r,x);
    return r;
}

/* -- end of file -- */
