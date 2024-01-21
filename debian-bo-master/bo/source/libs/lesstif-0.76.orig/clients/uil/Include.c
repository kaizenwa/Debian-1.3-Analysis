/**
 *
 * $Id: Include.c,v 1.4 1996/11/04 07:11:29 u27113 Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

#include <stdio.h>
#include "misc.h"

#define MAX_PATHS 10  /* any person using more than 10 '-I' on the command line */
                      /* is bonkers! */
#define MAXPATHLEN 256

static char *Paths[MAX_PATHS];
static int nDirs = 0;

void IncludeAddDirectory(char *d)
{
  if (nDirs >= MAX_PATHS)
    Exit(LOC, "too many '-I' on command line\n");
  Paths[nDirs++] = Store(d);
}

FILE *IncludeOpenFile(char *f)
{
  int i;
  FILE *r = NULL;
  char b[MAXPATHLEN];
  
  if (f) {
    if ('/' == f[0])
      return fopen(f,"r");
    for ( strcpy(b,f), i = 0; 
	 (i <= nDirs) && (NULL == (r = fopen(b,"r"))); 
	 sprintf(b,"%s/%s", Paths[i++], f)) ;
  }
  return r;
}


