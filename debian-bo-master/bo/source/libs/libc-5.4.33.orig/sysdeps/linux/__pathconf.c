/* Copyright (C) 1991 Free Software Foundation, Inc.
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
#include <errno.h>
#include <stddef.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/vfs.h>
#endif


/* Get file-specific information about PATH.  */
long int
DEFUN(__pathconf, (path, name), CONST char *path AND int name)
{
  if (path == NULL)
    {
      errno = EINVAL;
      return -1;
    }

#ifdef __linux__
  switch (name)
    {
    default:
      return __fpathconf (0, name);

    case _PC_NAME_MAX:
      {
        struct statfs buf;
	if (__statfs (path, &buf) < 0) return -1;
	else return buf.f_namelen;
      }
    }
#else
  return __fpathconf (0, name);
#endif
}

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__pathconf, pathconf);
#endif

