/* main.c -- trivial hello test program
   Copyright (C) 1996 Free Software Foundation, Inc.
   This file is part of GNU Libtool.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Gordon Matzigkeit <gord@gnu.ai.mit.edu> */
#include "foo.h"
#include <stdio.h>

int
main (int argc, char **argv)
{
  printf ("Welcome to GNU Hell!\n");

  /* Just call the functions and check return values. */
  if (foo () != FOO_RET)
    return 1;

  if (hello () != HELLO_RET)
    return 2;

  return 0;
}
