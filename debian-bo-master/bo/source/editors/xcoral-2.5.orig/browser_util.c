/* ########################################################################

			     browser_util.c

   File: browser_util.c
   Path: /home/fournigault/c/X11/xcoral-2.31/browser_util.c
   Description: 
   Created: Fri Jan 27 10:49:52 MET 1995
   Author: Dominique Leveque
   Modified: Fri Jan 27 10:49:53 MET 1995
   Last maintained by: Dominique Leveque

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Dominique Leveque

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#include "result_types.h"
#include "browser_util.h"
#include <stdio.h>
#include <string.h>
#if defined (apollo) || defined (__FreeBSD__)
#include <stdlib.h>
#else
#include <malloc.h>
#endif

/*------------------------------------------------------------------------------
*/
char* xmalloc(size)
  int size;
{
  char* result;
   
  size = (size + 15) & 0xfffffff0;

  result = (char *) malloc(size);
  if (result == Null) {
    fprintf(stderr, ">>>>>>  Browser Error\n");
    fprintf(stderr, "    >>  Memory exhausted\n");
  }
  return(result);
}


/*------------------------------------------------------------------------------
*/
int sort_util(i,j)
  char** i;
  char** j;
{
  return(strcmp(*i, *j));
}


