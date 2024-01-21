/* window.c -- A *very* simple window management.  */

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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#else /* !HAVE_STDLIB_H */
#include "ansi_stdlib.h"
#endif /* !HAVE_STDLIB_H */

#include <sys/types.h>

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#include "window.h"
#include "xmalloc.h"
#include "tty.h"


window_t *
window_init(x, y, lines, columns)
    size_t x, y, lines, columns;
{
    window_t *this  = (window_t *)xmalloc(sizeof(window_t));

    window_resize(this, x, y, lines, columns);
    return this;
}


void
window_end(win)
    window_t *win;
{
    if (win)
	xfree(win);
}


void
window_resize(this, x, y, lines, columns)
    window_t *this;
    size_t x, y, lines, columns;
{
    this->x       = x;
    this->y       = y;
    this->lines   = lines;
    this->columns = columns;
}


size_t
window_puts(str, length)
    char *str;
    size_t length;
{
    return tty_puts(str, length);
}


size_t
window_putc(c)
    char c;
{
    return tty_putc(c);
}


void
window_goto(this, y, x)
    window_t *this;
    size_t y, x;
{
    tty_goto(y + this->y, x + this->x);
}


void
window_update()
{
    tty_update();
}
