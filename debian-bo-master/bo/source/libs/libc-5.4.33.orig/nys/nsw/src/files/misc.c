/*
** misc.c                         Misc utility functions
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
#include "misc.h"


int _nsw_getline(char **buf, int *size, FILE *fp)
{
    int len;
    int c;

    
    if (buf == NULL || size == NULL || fp == NULL)
	return -1;

    if (feof(fp))
	return -1;
    
    if (*buf == NULL)
    {
	if (*size == 0)
	    *size = BUFSIZ;
	
	*buf = malloc(*size);
	if (*buf == NULL)
	    return -1;
    }

    len = 0;
    while ((c = getc(fp)) != EOF && c != '\n')
    {
	if (len+1 >= *size)
	{
	    *buf = realloc(*buf, *size += BUFSIZ);
	    if (*buf == NULL)
		return -1;
	}

	(*buf)[len++] = c;
    }

    if (len == 0 && c == EOF)
	return -1;
    
    (*buf)[len] = '\0';

    if (*size > BUFSIZ*2 && len < BUFSIZ/2)
    {
	*buf = realloc(*buf, *size = BUFSIZ);
	if (*buf == NULL)
	    return -1;
    }
    
    return len;
}
