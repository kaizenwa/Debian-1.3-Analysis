/* Screen.c - Low-level terminal handling for af.
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


#include <stdio.h>
#include "af.h"
#include "termcntrl.h"

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: screen.c,v 1.6 1996/08/28 17:44:08 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *tgoto();
extern int tputs();
extern void free();

/* Local function declarations */

static int tputc();

/****************************************************************************/
/* Import the terminal capabilities from term.c */

extern TERMCNTRL tcntrl;

/****************************************************************************/
int tlines()
{
	/* Return the number of lines of the screen */

	return(tcntrl.lines);
}
/****************************************************************************/
int tcols()
{
	/* Return the number of columns of the screen */

	return(tcntrl.columns);
}
/****************************************************************************/
int ttabspace()
{
	/* Return the number of spaces between tabs on the screen */

	return(tcntrl.tab_spacing);
}
/****************************************************************************/
void tclear()
{
	/* Clear the screen and home the cursor */

	(void) tputs(tcntrl.cls, tcntrl.lines, tputc);
	return;
}
/****************************************************************************/
void tmove(col, row)
int col, row;
{
	/* Move the cursor to the specified location */

	int i;

	/* We can do this the easy way, or the hard way... */

	if (tcntrl.move_cursor != NULL) {
		/* Dead simple, just tell the terminal where to move to */

		tputs(tgoto(tcntrl.move_cursor, col, row), 1, tputc);
	} else {
		/* The hard way; position the cursor ourselves */

		tputs(tcntrl.home, 1, tputc);

		for (i = 0; i < col; i++) {
			tputs(tcntrl.nd_space, 1, tputc);
		}
		for (i = 0; i < row; i++) {
			tputs('\n', 1, tputc);
		}
	}

	return;
}
/****************************************************************************/
void treturn()
{
	/* Do a carriage return (cursor to left edge of current line) */

	tputs(tcntrl.cr, 1, tputc);
	return;
}
/****************************************************************************/
void tbackspace()
{
	/* Move the cursor left one character */

	tputs(tcntrl.backspace, 1, tputc);
	return;
}
/****************************************************************************/
void tclrline(spaces)
int spaces;
{
	/* Clear to the end of the current terminal line */

	int i;

	/* Do we do this automatically or by hand? */

	if (tcntrl.clr_to_eol != NULL) {
		/* Use the clear-to-eol capability */

		tputs(tcntrl.clr_to_eol, 1, tputc);
	} else {
		/* Space out the rest of the line */

		for (i = 0; i < spaces; i++) {
			(void) tputc(' ');
		}
		for (i = 0; i < spaces; i++) {
			tputs(tcntrl.backspace, 1, tputc);
		}
	}

	return;
}
/****************************************************************************/
void tbeep()
{
	/* Sound the terminal's bell */

	(void) fputs(tcntrl.bell, stdout);
	return;
}
/****************************************************************************/
static int tputc(c)
int c;
{
	/* A putchar-like function for use with tputs */

	return(putchar(c));
}
/****************************************************************************/
