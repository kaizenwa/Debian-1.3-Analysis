/* Checker stubs for functions defined in ctype.h
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

#include "available-stubs.h"

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#include "checker_api.h"

#if 0
#define HAVE_isalnum
#define HAVE_isalpha
#define HAVE_iscntrl
#define HAVE_isdigit
#define HAVE_islower
#define HAVE_isgraph
#define HAVE_isprint
#define HAVE_ispunct
#define HAVE_isspace
#define HAVE_isupper
#define HAVE_isxdigit
#define HAVE_isblank
#define HAVE_tolower
#define HAVE_toupper
#define HAVE_isascii
#define HAVE_toascii
#define HAVE__toupper
#define HAVE_tolower
#define HAVE__tolower
#endif

/* compiled from: . */
#ifdef HAVE_isalnum
/* From `/usr/include/ctype.h:103'.  */
int
chkr$isalnum (int c)
{
#if USE_BI_JUMP
  __builtin_jump (isalnum);
#else
  return isalnum (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isalnum */

#ifdef HAVE_isalpha
/* From `/usr/include/ctype.h:104'.  */
int
chkr$isalpha (int c)
{
#if USE_BI_JUMP
  __builtin_jump (isalpha);
#else
  return isalpha (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isalpha */

#ifdef HAVE_iscntrl
/* From `/usr/include/ctype.h:105'.  */
int
chkr$iscntrl (int c)
{
#if USE_BI_JUMP
  __builtin_jump (iscntrl);
#else
  return iscntrl (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_iscntrl */

#ifdef HAVE_isdigit
/* From `/usr/include/ctype.h:106'.  */
int
chkr$isdigit (int c)
{
#if USE_BI_JUMP
  __builtin_jump (isdigit);
#else
  return isdigit (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isdigit */

#ifdef HAVE_islower
/* From `/usr/include/ctype.h:107'.  */
int
chkr$islower (int c)
{
#if USE_BI_JUMP
  __builtin_jump (islower);
#else
  return islower (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_islower */

#ifdef HAVE_isgraph
/* From `/usr/include/ctype.h:108'.  */
int
chkr$isgraph (int c)
{
#if USE_BI_JUMP
  __builtin_jump (isgraph);
#else
  return isgraph (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isgraph */

#ifdef HAVE_isprint
/* From `/usr/include/ctype.h:109'.  */
int
chkr$isprint (int c)
{
#if USE_BI_JUMP
  __builtin_jump (isprint);
#else
  return isprint (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isprint */

#ifdef HAVE_ispunct
/* From `/usr/include/ctype.h:110'.  */
int
chkr$ispunct (int c)
{
#if USE_BI_JUMP
  __builtin_jump (ispunct);
#else
  return ispunct (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ispunct */

#ifdef HAVE_isspace
/* From `/usr/include/ctype.h:111'.  */
int
chkr$isspace (int c)
{
#if USE_BI_JUMP
  __builtin_jump (isspace);
#else
  return isspace (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isspace */

#ifdef HAVE_isupper
/* From `/usr/include/ctype.h:112'.  */
int
chkr$isupper (int c)
{
#if USE_BI_JUMP
  __builtin_jump (isupper);
#else
  return isupper (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isupper */

#ifdef HAVE_isxdigit
/* From `/usr/include/ctype.h:113'.  */
int
chkr$isxdigit (int c)
{
#if USE_BI_JUMP
  __builtin_jump (isxdigit);
#else
  return isxdigit (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isxdigit */

#ifdef HAVE_isblank
/* From `/usr/include/ctype.h:116'.  */
int
chkr$isblank (int c)
{
#if USE_BI_JUMP
  __builtin_jump (isblank);
#else
  return isblank (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isblank */

#ifdef HAVE_tolower
/* From `/usr/include/ctype.h:121'.  */
int
chkr$tolower (int c)
{
#if USE_BI_JUMP
  __builtin_jump (tolower);
#else
  return tolower (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tolower */

#ifdef HAVE_toupper
/* From `/usr/include/ctype.h:124'.  */
int
chkr$toupper (int c)
{
#if USE_BI_JUMP
  __builtin_jump (toupper);
#else
  return toupper (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_toupper */

#ifdef HAVE_isascii
/* From `/usr/include/ctype.h:131'.  */
int
chkr$isascii (int c)
{
#if USE_BI_JUMP
  __builtin_jump (isascii);
#else
  return isascii (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isascii */

#ifdef HAVE_toascii
/* From `/usr/include/ctype.h:135'.  */
int
chkr$toascii (int c)
{
#if USE_BI_JUMP
  __builtin_jump (toascii);
#else
  return toascii (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_toascii */

#ifdef HAVE__toupper
/* From `/usr/include/ctype.h:141'.  */
int
chkr$_toupper (int c)
{
#if USE_BI_JUMP
  __builtin_jump (_toupper);
#else
  return _toupper (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE__toupper */

#ifdef HAVE__tolower
/* From `/usr/include/ctype.h:142'.  */
int
chkr$_tolower (int c)
{
#if USE_BI_JUMP
  __builtin_jump (_tolower);
#else
  return _tolower (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE__tolower */

#endif /* HAVE_CTYPE_H */
