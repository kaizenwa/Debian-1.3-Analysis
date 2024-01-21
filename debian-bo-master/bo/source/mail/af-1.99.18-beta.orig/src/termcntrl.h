/* Termcntrl.h - Definitions of terminal control structure for af.
   Copyright (C) 1992, 1996 Malc Arnold.

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

#define TERMID	"$Id: termcntrl.h,v 1.5 1996/08/28 17:44:08 malc Exp $"

/****************************************************************************/
/* Define the structure containing the terminal capabilities */

typedef struct termcntrl {
	int lines;			/* Number of lines */
	int columns;			/* Number of columns */
	int tab_spacing;		/* Spaces between tabs */

	char *cls;			/* Clear screen */
	char *home;			/* Top left of screen */
	char *clr_to_eol;		/* Clear to end of current line */

	char *move_cursor;		/* Absolute cursor motion */
	char *cr;			/* Carriage return character */
	char *backspace;		/* Back space character */
	char *nd_space;			/* Non-destructive space */

	char *bell;			/* Audible bell */
} TERMCNTRL;

/****************************************************************************/
/* The default bell string (used if none specified) */

#define BELL		"\007"

/****************************************************************************/
