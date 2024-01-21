/*
 * tixCompat.c --
 *
 *	Some compatibility functions for Tix.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */
#include <tkPort.h>
#include <tkInt.h>
#include <tixInt.h>

#ifdef NO_STRDUP

/* strdup not a POSIX call */

char * tixStrDup(char * s)
{
    size_t len = strlen(s)+1;
    char * new_string;

    new_string = (char*)ckalloc(len);
    strcpy(new_string, s);

    return new_string;
}

#endif /* NO_STRDUP */
