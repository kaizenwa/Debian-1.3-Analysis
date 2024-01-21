/* Psignal.h - Declarations for emulating POSIX signals
   Copyright (C) 1995, 1996 Malc Arnold.

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


/****************************************************************************/
/* RCS info. */

#define PSIGID	"$Id: psignal.h,v 1.2 1996/03/17 01:08:15 malc Exp $"

/****************************************************************************/
/* We'll need to specify our own type for signal sets */

typedef int sigset_t;

/****************************************************************************/
/* Define values we need that aren't in BSD signals */

#define SIG_BLOCK	1
#define SIG_UNBLOCK	2
#define SIG_SETMASK	3

/****************************************************************************/
/* And declare the POSIX signal-handling functions */

extern int sigemptyset(), sigaddset();
extern int sigmember(), sigprocmask();
extern int sigpending();

/****************************************************************************/
