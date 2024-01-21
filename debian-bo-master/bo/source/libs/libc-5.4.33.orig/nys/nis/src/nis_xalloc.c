/*
** xalloc.c           Memory allocation routines.
**
** Copyright (c) 1993 Signum Support AB, Sweden
**
** This file is part of the NYS Library.
**
** The NYS Library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with the NYS Library; see the file COPYING.LIB.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Peter Eriksson <pen@signum.se>
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xalloc.h"

void *xalloc(void **buf, int len, int size)
{
    if (buf == NULL)
	return NULL;

    if (len == 0 || size == 0)
    {
	fprintf(stderr, "Notice: Calling xalloc(..., %d, %d)\n", len, size);
	len = size = 1;
    }
    
	
    if (*buf == NULL)
    {
	*buf = (void *) malloc(len*size);
	memset(*buf, 0, len*size);
	return *buf;
    }
    else
	return *buf = (void *) realloc(*buf, len*size);
}

void xfree(void **buf)
{
    if (buf && *buf)
    {
	free(*buf);
	*buf = NULL;
    }
}

char *xstrdup(char *str)
{
    if (str == NULL)
	return NULL;

    return strdup(str);
}

void *xdup(void *buf, int len)
{
    void *nbuf;


    if (len == 0)
    {
	fprintf(stderr, "Notice: Calling xdup(..., %d)\n", len);
	return NULL;
    }

    nbuf = malloc(len);
    if (nbuf == NULL)
	return NULL;
    
    memcpy(nbuf, buf, len);
    return nbuf;
}
