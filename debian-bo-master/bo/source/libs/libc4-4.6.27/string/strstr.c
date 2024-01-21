/* Copyright (C) 1994 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

/*
 * My personal strstr() implementation that beats most other algorithms.
 * Until someone tells me otherwise, I assume that this is the
 * fastest implementation of strstr() in C.
 * I deliberately chose not to comment it.  You should have at least
 * as much fun trying to understand it, as I had to write it :-).
 *
 * Stephen R. van den Berg, berg@pool.informatik.rwth-aachen.de	*/

#include <ansidecl.h>
#include <string.h>
#include <sys/types.h>

typedef unsigned chartype;

char *
DEFUN(strstr, (phaystack, pneedle),
	const char *phaystack AND const char *pneedle)
{
  register const unchar *haystack, *needle;
  register chartype b, c;

  haystack = (const unchar *)phaystack;

  if ((b= *(needle=(const unchar*)pneedle)))
    {
      haystack--;				/* possible ANSI violation */
      do
	if (!(c= *++haystack))
	  goto ret0;
      while (c!=b);

      if (!(c= *++needle))
	goto foundneedle;
      ++needle;
      goto jin;

      for (;;)
        { 
          register chartype a;
	  register const unchar *rhaystack, *rneedle;

	  do
	    {
	      if (!(a= *++haystack))
		goto ret0;
	      if (a==b)
		break;
	      if (!(a= *++haystack))
		goto ret0;
shloop:     }
          while (a!=b);

jin:	  if (!(a= *++haystack))
	    goto ret0;

	  if (a!=c)
	    goto shloop;

	  if (*(rhaystack=haystack--+1) == (a= *(rneedle=needle)))
	    do
	      {
		if (!a)
		  goto foundneedle;
		if (*++rhaystack!=(a= *++needle))
		  break;
		if (!a)
		  goto foundneedle;
	      }
	    while (*++rhaystack == (a= *++needle));

	  needle=rneedle;		   /* took the register-poor aproach */

	  if (!a)
	    break;
        }
    }
foundneedle:
  return (char*)haystack;
ret0:
  return 0;
}
