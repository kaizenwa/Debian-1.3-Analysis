/* Copyright 1996 Free Software Foundation, Inc.
   Contributed by Marcin Dalecki <dalecki@sub994.sub.uni-goettingen.de>

   This file is part of the Linux modutils.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2 of the License, or (at your
   option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef MISC_H
#define MISC_H

#include <stdio.h>

#include "module.h"

#define kernel_sym  old_kernel_sym

/*
 * Globally used data definitions.
 */

extern int flag_debug;

/*
 * String manipulation utilities.
 */

extern char *strip_o (char *fname);

extern char *resolve_string (const char *str, char *buf, int size);

/*
 * File manipulation
 */

extern char *read_and_preprocess_file (const char *);
char *get_concat_line (char *from, int *lines);

/*
 * Error logging facilities.
 */

extern int log;

void
error (const char *fmt,...)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;

     void
       lprintf (const char *fmt,...)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;

     extern void
       setsyslog (const char *program);

#endif /* MISC_H */
