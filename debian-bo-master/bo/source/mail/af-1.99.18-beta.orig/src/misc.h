/* Misc.h - declarations for af's utility routines.
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

#ifndef lint
#define MISCID	"$Id: misc.h,v 1.2 1996/03/17 01:10:47 malc Exp $"
#endif /* ! lint */

/****************************************************************************/
/* Define MAXPATHLEN if not available */

#ifndef MAXPATHLEN
#define MAXPATHLEN	1024
#endif /* ! MAXPATHLEN */

/****************************************************************************/
/* Define MAXSYMLINKS if not available */

#ifndef MAXSYMLINKS
#define MAXSYMLINKS	32
#endif /* ! MAXSYMLINKS */

/****************************************************************************/
/* The structure that holds details of an active timer */

typedef struct {
	RETSIGTYPE (*handler)();	/* The alarm signal handler */
	int blocked, pending;		/* Was SIGALRM blocked or pending? */
	unsigned seconds;		/* Seconds remaining on timer */
	time_t disabled;		/* Time the timer was disabled */
} ATIMER;

/****************************************************************************/
