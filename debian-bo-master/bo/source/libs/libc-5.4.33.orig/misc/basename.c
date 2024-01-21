/* basename.c -- return the last element in a path
   Copyright (C) 1994 Free Software Foundation, Inc.
   This file is part of the Linux C Library.

The Linux C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Linux C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <string.h>
#include <unistd.h>

/* GCC says we have to have a prototype */
CONST char *
DEFUN(__basename, (name), CONST char *name);

/* Return NAME with any leading path stripped off.  */

CONST char *
DEFUN(__basename, (name), CONST char *name)
{
  char *base;

  base = strrchr (name, '/');
  return base ? base + 1 : name;
}

#if defined(__ELF__) || defined(__GNU_LIBRARY__)
#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__basename, basename);
#endif
#endif
