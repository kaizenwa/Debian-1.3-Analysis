/* Readline.h - Definitions for line editing routines.
   Copyright (C) 1990, 1991, 1992, 1995, 1996 Malc Arnold.

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
/* RCS info */

#define READLINEID	"$Id: readline.h,v 1.9 1996/03/17 01:10:47 malc Exp $"

/****************************************************************************/
/* The size (in chars) which we grab at a time for extensible char *s */

#define TEXTBUFSIZ	128

/****************************************************************************/
/* The prompt used when we have scrolled along a line */

#define SCROLL_PROMPT	"$"

/* The character used to indicate a line extending beyond the screen */

#define CONT_CHAR	'$'

/****************************************************************************/
/* The character that replaces nulls in non-binary edits */

#define NULL_CHAR	'@'

/****************************************************************************/
/* A value to show there is no mark set when moving text */

#define NO_MARK		-1

/****************************************************************************/
/* The history buffer data types and definitions */

#define HISTSIZE	256

typedef struct history {
	char *text;			/* The text of the history entry */
	int len;			/* The length of the entry */
	CLIST *(*complete)();		/* Completion type of the entry */
	struct history *prev, *next;	/* Pointers to other entries */
} HISTORY;

/****************************************************************************/
