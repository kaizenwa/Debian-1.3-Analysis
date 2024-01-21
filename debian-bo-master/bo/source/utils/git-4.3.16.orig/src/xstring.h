/* xstring.h -- Prototypes for the functions in xstring.c.  */

/* Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Tudor Hulubei and Andrei Pitis.  */


#ifndef _GIT_XSTRING_H
#define _GIT_XSTRING_H


#include <sys/types.h>

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#if defined(STDC_HEADERS) || defined(HAVE_STRING_H)

#include <string.h>
/* An ANSI string.h and pre-ANSI memory.h might conflict.  */

#if !defined(STDC_HEADERS) && defined(HAVE_MEMORY_H)
#include <memory.h>
#endif /* !STDC_HEADERS and HAVE_MEMORY_H */

#else /* !STDC_HEADERS and !HAVE_STRING_H */

#include <strings.h>
/* memory.h and strings.h conflict on some systems.  */
#endif /* !STDC_HEADERS and !HAVE_STRING_H */


#include "stdc.h"


#ifndef HAVE_STRCASECMP
extern int strcasecmp PROTO ((const char *, const char *));
#endif /* !HAVE_STRCASECMP */

#ifndef HAVE_STRNCASECMP
extern int strncasecmp PROTO ((const char *, const char *, size_t));
#endif /* !HAVE_STRNCASECMP */

#ifndef HAVE_STRSTR
extern char *strstr PROTO ((const char *, const char *));
#endif /* !HAVE_STRSTR */

#ifndef HAVE_MEMMOVE
extern void *memmove PROTO ((void *, const void *, size_t));
#endif /* !HAVE_MEMMOVE */

extern char *xstrdup PROTO ((const char *));

#endif  /* _GIT_XSTRING_H */
