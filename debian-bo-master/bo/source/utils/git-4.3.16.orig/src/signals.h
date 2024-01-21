/* signal.h -- Function prototypes for those stupid functions in signal.c.  */

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


#ifndef _GIT_SIGNAL_H
#define _GIT_SIGNAL_H


#include "stdc.h"


#define SIG_OFF         0
#define SIG_ON          1


extern int UserHeartAttack;


extern RETSIGTYPE suspend PROTO ((int));
extern RETSIGTYPE resume PROTO ((int));
extern RETSIGTYPE user_panic PROTO ((int));

extern void signals_dfl	PROTO (());
extern void signals PROTO ((int));
extern void ignore_signals PROTO (());
extern void restore_signals PROTO (());


#endif  /* _GIT_SIGNAL_H */
