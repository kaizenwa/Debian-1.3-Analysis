/*
 * This file is part of the C library for Linux and is
 * covered by the GNU Library General Public license version 2, or
 * any later version.
 * 
 * Copyright (C) 1992, 1993 Hoongjiu Lu
 *
 */
#include <ansidecl.h>
#include <string.h>
#include <ctype.h>


/* Compare S1 and S2, ignoring case, returning less than, equal to or
   greater than zero if S1 is lexiographically less than,
   equal to or greater than S2.  */
int
DEFUN(strcasecmp, (s1, s2), CONST char *s1 AND CONST char *s2)
{
  register CONST unsigned char *p1 = (CONST unsigned char *) s1;
  register CONST unsigned char *p2 = (CONST unsigned char *) s2;
  register int ret;
  unsigned char c1;

  if (p1 == p2)
    return 0;

  for (; !(ret = (c1 = tolower(*p1)) - tolower(*p2)); p1++, p2++)
    if (c1 == '\0') break;
  return ret;
}
