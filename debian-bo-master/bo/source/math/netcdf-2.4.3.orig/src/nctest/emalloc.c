/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf/nctest/emalloc.c,v 1.10 1996/07/02 15:11:05 russ Exp $
 *********************************************************************/

/*LINTLIBRARY*/
#include <stdio.h>
#include <stdlib.h>
#include "error.h"
#include "emalloc.h"

void *
emalloc (size)			/* check return from malloc */
     size_t size;
{
    void   *p;

    if (size == 0)
      return 0;

    p = (void *) malloc (size);

    if (p == 0) {
	error ("out of memory\n");
	exit (1);
    }
    return p;
}

void *
erealloc (ptr, size)		/* check return from realloc */
     void *ptr;
     size_t size;
{
    void *p;

    p = (void *) realloc (ptr, size);

    if (p == 0) {
 	error ("out of memory");
	exit(1);
    }
    return p;
}
