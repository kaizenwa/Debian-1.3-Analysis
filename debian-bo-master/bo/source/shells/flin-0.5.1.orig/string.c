/*
Manipulates strings for easier parsing
Copyright (C) 1995  Brian Cully

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 675 Mass
Ave, Cambridge, MA 02139, USA.

please send patches or advice to: `shmit@kublai.com'
*/
#ifdef HAVE_CONFIG_H
#   include <config.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "string.h"


char escape=ESCAPE;

int getline(const char *src, char *dst) {
   int index=0;

   while ((*(src+index) != CR) && *(src+index)) {
      *(dst+index) = *(src+index);
      index++;
   }

   *(dst+index) = '\0';
   if (*(src+index))
      return index;
   return -1;
}

/* get a substring from a string, substrings are non-delimiter characters
** separated by delimiters
** On entry:
**    src == source string
**    dst == destination string
**    delim == delimiter
** On exit:
**    -1 if '\0' was found
**    otherwise, index offset in src where dst ends
*/

int substr(const char *src, char *dst, char delim) {
   int dindex=0, index=0;

   while ((*(src+index) != delim) && *(src+index)) {
      /* Escape character */
      if (*(src+index) == escape && *(src+index+1))
         index++;
      *(dst+dindex) = *(src+index);
      index++; dindex++;
   }

   *(dst+dindex) = '\0';
   if (*(src+index))
      return index;
   return -1;
}
