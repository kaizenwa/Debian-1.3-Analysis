/* calloc.c - C standard library routine.
   Copyright (c) 1989, 1993  Michael J. Haertel
   You may redistribute this library under the terms of the
   GNU Library General Public License (version 2 or any later
   version) as published by the Free Software Foundation.
   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY EXPRESS OR IMPLIED
   WARRANTY.  IN PARTICULAR, THE AUTHOR MAKES NO REPRESENTATION OR
   WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS
   SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE. */

#include <string.h>
#include "malloc.h"

#ifdef __ELF__
#pragma weak calloc = __libc_calloc
#endif

/* Allocate space for the given number of elements of the given
   size, initializing the whole region to binary zeroes. */
void *
__libc_calloc(size_t nelem, size_t size)
{
    void *result;

    result = __libc_malloc(size * nelem);
    if (result)
	memset(result, 0, nelem * size);
    return result;
}
