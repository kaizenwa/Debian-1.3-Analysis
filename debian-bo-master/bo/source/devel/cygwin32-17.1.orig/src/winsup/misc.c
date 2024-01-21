/* Miscellaneous non-posix routines.  

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/* This file contains various routines that various applications need
   that aren't found in Posix.  They're kept here instead of files
   that also contain Posix routines to avoid namespace pollution.  */


#include <unistd.h>
#include <fcntl.h>
#include "sys/types.h"
#include "stddef.h"
#include "stdarg.h"
#include "syscalls.h"


int
creat (const char *path, mode_t mode)
{
  return open (path, O_WRONLY | O_CREAT | O_TRUNC, mode);
}

void
__assertfail ()
{
  exit (99);
}



int wcscmp (wchar_t *s1, wchar_t *s2)
{
  while (*s1  && *s1 == *s2)
    {
      s1++;
      s2++;
    }

  return (*(unsigned short *) s1) - (*(unsigned short *) s2);
}

int wcslen (wchar_t *s1)
{
  int l = 0;
  while (s1[l])
    l++;
  return l;
}

/* forgive me 
   to do this right, I've got to work out the usoft va_list machine
    and use wsvprintfW instead, or some other jiggery pokery
*/
int wprintf (const char *fmt, ...)
{
  va_list ap;
  int ret;

  va_start (ap, fmt);
  ret = vprintf (fmt, ap);
  va_end (ap);
  return ret;
}

int tgetent ()
{
  return -1;
}

int vhangup ()
{
  set_errno (ENOSYS);
  return -1;
}


_PTR	 memccpy (_PTR out, const _PTR in , int c, size_t len)
{
  const char *inc = in;
  char *outc = out;
  while (len)
    {
      char x = *inc++;
      *outc++ = x;
      if (x == c)
	return outc;
      len --;
    }
  return 0;
}


/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#define MARK() mark__FPCci(__FILE__,__LINE__)
char *
strsep ( char **stringp,
	 const char *delim)
{
	register char *s;
	register const char *spanp;
	register int c, sc;
	char *tok;

	if ((s = *stringp) == NULL)
		return (NULL);
	for (tok = s;;) {
		c = *s++;
		spanp = delim;
		do {
			if ((sc = *spanp++) == c) {
				if (c == 0)
					s = NULL;
				else
					s[-1] = 0;
				*stringp = s;
				return (tok);
			}
		} while (sc != 0);
	}
	/* NOTREACHED */
}

int nice () 
{
  return 0;
}

