/* Checker stubs for functions defined in malloc.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include <stddef.h>
#include "checker_api.h"

#ifndef PTR
#define PTR void *
#endif

PTR 
chkr$calloc (size_t nmemb, size_t size)
{
  return calloc (nmemb, size);
}

void
chkr$free (PTR ptr)
{
  free (ptr);
}

PTR
chkr$malloc (size_t real_size)
{
  return malloc (real_size);
}

PTR
chkr$valloc (size_t size)
{
  return valloc (size);
}

PTR
chkr$memalign (size_t alignment, size_t size)
{
  return memalign (alignment, size);
}

PTR
chkr$realloc (PTR ptr, size_t size)
{
  return realloc (ptr, size);
}

PTR
chkr$sbrk (int incr)
{
  return sbrk (incr);
}
