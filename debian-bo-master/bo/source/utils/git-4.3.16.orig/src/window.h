/* window.h -- Data structures and function prototypes used by window.c.  */

/* Copyright (C) 1993, 1994, 1995, 1996 Free Software Foundation, Inc.

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


#ifndef _GIT_WINDOW_H
#define _GIT_WINDOW_H


#include <sys/types.h>

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#include "stdc.h"


typedef struct
{
    size_t lines, columns;
    size_t x, y;
} window_t;


extern window_t *window_init PROTO ((size_t, size_t, size_t, size_t));
extern void window_end PROTO ((window_t *));
extern void window_resize PROTO ((window_t *, size_t, size_t, size_t, size_t));
extern size_t window_puts PROTO ((char *, size_t));
extern size_t window_putc PROTO ((char));
extern void window_goto PROTO ((window_t *, size_t, size_t));
extern void window_update PROTO (());


#endif  /* _GIT_WINDOW_H */
