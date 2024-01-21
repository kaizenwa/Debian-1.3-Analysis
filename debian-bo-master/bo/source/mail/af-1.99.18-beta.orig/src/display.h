/* Display.h - Declarations for af's screen display routines.
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

#ifndef lint
static char *DisplayId = "$Id: display.h,v 1.5 1996/03/17 01:10:47 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* The structure for a line of the virtual (ie. internal) terminal display */

typedef struct {
	short nchanged;			/* No of characters changed */
	char *text;			/* Text of line */
} VT_LINE;

/****************************************************************************/
/* A special value of the virtual terminal's 'nchanged' field */

#define VT_REDRAW	-1

/****************************************************************************/
