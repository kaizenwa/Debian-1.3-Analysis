/* system.h -- Prototypes and #defines for the stuff in system.c.  */

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


#ifndef _GIT_SYSTEM_H
#define _GIT_SYSTEM_H


#include "stdc.h"


extern char *stdout_log_name;
extern char *stderr_log_name;


extern int start PROTO ((char *, int));
extern void removelog PROTO (());
extern void display_errors PROTO ((char *));


#endif  /* _GIT_SYSTEM_H */
