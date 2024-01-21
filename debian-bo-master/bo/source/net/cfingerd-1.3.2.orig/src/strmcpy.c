/*
 * CFINGERD
 * String copy with automatic memory allocation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include <stdlib.h>
#include <string.h>
#include <syslog.h>

/*
 * STRMCPY
 *
 * DO NOT PREVIOUSLY ALLOCATE MEMORY!  This routine will automatically do
 * the memory allocations for you.  This is an advanced version of strcpy,
 * but returns memory allocation.  Very handy.
 */
void strmcpy(char **t, const char *s)
{
    if (*t != NULL)
	free(*t);

    *t = malloc(strlen(s) + 1);

    if (*t == NULL) {
	syslog(LOG_ERR, "strmcpy: alloc error");
	exit(1);
    }

    (void) strcpy(*t, s);
}
