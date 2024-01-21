/* Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.
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

#include <ansidecl.h>
#include <stdio.h>
#include <string.h>

#ifndef HAVE_GNU_LD
#define _sys_errlist sys_errlist
#define _sys_nerr sys_nerr
#endif

#if NLS
#include "../nls/nl_types.h"
#endif

/* Return a string descibing the errno code in ERRNUM.
   The storage is good only until the next call to strerror.
   Writing to the storage causes undefined behavior.  */
char *
DEFUN(strerror, (errnum), int errnum)
{
#if NLS
  libc_nls_init();
#endif

  if (errnum < 0 || errnum >= _sys_nerr)
    {
      static char unknown_error[] = "Unknown error 000000000000000000000";
      static char fmt[] = "Unknown error %d";
#ifdef __linux__
      sprintf(unknown_error, fmt, errnum);
#else
      size_t len = sprintf(unknown_error, fmt, errnum);
      if (len < sizeof(fmt) - 2)
	return NULL;
      unknown_error[len] = '\0';
#endif
      return unknown_error;
    }
#if NLS
  return catgets(_libc_cat, ErrorListSet, errnum +1, (char *) _sys_errlist[errnum]);
#else
  return (char *) _sys_errlist[errnum];
#endif
}
