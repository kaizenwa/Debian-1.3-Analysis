/*
 * lftp and utils
 *
 * Copyright (c) 1996-1997 by Alexander V. Lukyanov (lav@yars.free.net)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef XMALLOC_H
#define XMALLOC_H

#include <string.h>
#include <stdlib.h>

#ifdef DBMALLOC
#include "dbmalloc.h"
#endif

extern "C" {
void *xmalloc(size_t);
void *xrealloc(void *,size_t);
static inline char *xstrdup(const char *s)
{
   if(!s)
      return 0;
   return strcpy((char*)xmalloc(strlen(s)+1),s);
}
static inline void xfree(void *p)
{
   if(p)
      free(p);
}

}

#endif  XMALLOC_H
