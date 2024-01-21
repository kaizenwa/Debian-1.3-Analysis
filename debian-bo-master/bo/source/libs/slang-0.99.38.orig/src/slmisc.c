/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */
#include "config.h"

#include <stdio.h>
#include <string.h>

#include "slang.h"
#include "_slang.h"

char *SLmake_string(char *str)
{
   return SLmake_nstring(str, strlen (str));
}

char *SLmake_nstring (char *str, unsigned int n)
{
   char *ptr;
   
   if (NULL == (ptr = (char *) SLMALLOC(n + 1)))
     {
	SLang_Error = SL_MALLOC_ERROR;
	return(NULL);
     }
   SLMEMCPY (ptr, str, n);
   ptr[n] = 0;
   return(ptr);
}

void SLmake_lut (unsigned char *lut, unsigned char *range, unsigned char reverse)
{
   register unsigned char *l = lut, *lmax = lut + 256;
   int i, r1, r2;

   while (l < lmax) *l++ = reverse;
   
   while (*range)
     {
	r1 = *range;
	if (*(range + 1) == '-')
	  {
	     range += 2;
	     r2 = *range;
	  }
	else r2 = r1;
	
	for (i = r1; i <= r2; i++) lut[i] = !reverse;
	if (*range) range++;
     }
}

