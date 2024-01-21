/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */


/* These routines are fast memcpy, memset routines.  When available, I
   use system rouines.  For msdos, I use inline assembly. */

/* The current versions only work in the forward direction only!! */
      
#include <stdio.h>
#include "slang.h"
#include "_slang.h"

void jed_memset(char *p, char space, int n)
{
#if !defined(msdos) || defined(__WIN32__)
   register char *pmax;

   pmax = p + (n - 4);
   n = n % 4;
   while(p <= pmax) 
     {
	*p = space; *(p + 1) = space; *(p + 2) = space; *(p + 3) = space;
	p += 4;
     }
   while (n--) *p++ = space;
#else
   asm mov al, space
   asm mov dx, di
   asm mov cx, n
   asm les di, p
   asm cld
   asm rep stosb
   asm mov di, dx
#endif
}
