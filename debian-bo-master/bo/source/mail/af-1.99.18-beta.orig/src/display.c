/* Display.c - High-level screen handling functions for af.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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
#include <ctype.h>
#include "af.h"
#include "display.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "mode.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: display.c,v 1.36 1997/03/05 21:23:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc();
extern char *xstrdup(), *get_vtext();
extern int loading(), executing();
extern int keys_pending(), get_vval();
extern int tlines(), tcols();
extern unsigned cmodes();
extern void free(), unformat(), tbeep();
extern void emsgl(), tmove(), tclrline();

/* Local function declarations */

void vtredraw(), display(), redisplay();
MESSAGE *find_prev(), *find_next();
MESSAGE *find_start();
static char *get_status();
static int point_visible(), vtshowline();
static void vtnewline(), vtchgline();
static void do_move(), put_message();
static MESSAGE *find_last(), *optimise();

/****************************************************************************/
/* Import the current window from commands.c */

extern WINDOW *cwin;

/****************************************************************************/
/* The virtual terminal image, used to optimise redisplay */

static VT_LINE *vterm = NULL;

/* And the size of the virtual terminal */

static int vt_lines = 0;
static int vt_columns = 0;

/****************************************************************************/
/* The next virtual terminal line to update */

static int vt_next = 0;

/****************************************************************************/
/*
 * We have a file-static global here, representing the
 * screen position of the current message. It is kept here
 * for speed, as recalculating it would rapidly get tedious.
 */

static int active_line = 0;

/****************************************************************************/
void init_vterm()
{
	/* Initialise the virtual terminal */

	int i;

	/* Get the size of the terminal */

	vt_lines = tlines() - 1;
	vt_columns = tcols();

	/* Allocate the lines array */

	vterm = (VT_LINE *) xmalloc(vt_lines * sizeof(VT_LINE));

	/* Allocate and fill each line */

	for (i = 0; i < vt_lines; i++) {
		vterm[i].text = xstrdup("");
		vterm[i].nchanged = 0;
	}

	return;
}
/****************************************************************************/
#ifdef HAVE_RESIZING
void reinit_vterm()
{
	/* Resize the virtual terminal after a window size change */

	int old_lines = vt_lines, i;

	/* Get the new size of the terminal */

	vt_lines = tlines() - 1;
	vt_columns = tcols();
		
	/* Deallocate any now-redundant lines */

	for (i = vt_lines; i < old_lines; i++) {
		free(vterm[i].text);
	}

	/* Reallocate the lines array */

	vterm = (VT_LINE *) xrealloc(vterm, vt_lines * sizeof(VT_LINE));

	/* Allocate any new lines */

	for (i = old_lines; i < vt_lines; i++) {
		vterm[i].text = xstrdup("");
		vterm[i].nchanged = 0;
	}

	return;
}
#endif /* HAVE_RESIZING */
/****************************************************************************/
int vtupdate(force)
int force;
{
	/* Redraw the virtual terminal to the physical one after changes */

	int updated = FALSE;

	/* Loop over the lines, updating any that need it */

	while (vt_next < vt_lines) {
		/* Abort if we're not forcing and the are pending keys */

		if ((!force || loading() || executing())
		    && keys_pending()) {
			return(updated);
		}

		/* Show the line and mark as shown */

		updated = (vtshowline(vt_next++) || updated);
	}

	/* And return whether we updated the screen */

	return(updated);
}
/****************************************************************************/
void vtredraw()
{
	/* Mark the virtual terminal as needing every line redrawn */

	int vtrow = 0;

	/* Only act if we have a virtual terminal */

	if (vterm != NULL) {
		/* Mark all the lines as needing redraw */

		for (vtrow = 0; vtrow < vt_lines; vtrow++) {
			vterm[vtrow].nchanged = VT_REDRAW;
		}

		/* We'll need to start redrawing from the top */

		vt_next = 0;
	}
	return;
}
/****************************************************************************/
static int vtshowline(vtrow)
int vtrow;
{
	/* Update a physical terminal line if required */

	int c;

	if (vterm[vtrow].nchanged) {
		/* Move to the start of the line */

		tmove(0, vtrow);

		/* Do we need to redraw the whole line? */

		if (vterm[vtrow].nchanged == VT_REDRAW) {
			/* Redraw the whole line */

			(void) fputs(vterm[vtrow].text, stdout);
			tclrline(vt_columns - strlen(vterm[vtrow].text));
		} else {
			/* Redraw the required columns */

			for (c = 0; c < vterm[vtrow].nchanged; c++) {
				(void) putchar(vterm[vtrow].text[c]);
			}
		}

		/* The line has now been updated */

		vterm[vtrow].nchanged = 0;
		return(TRUE);
	}

	/* This line didn't need updating */

	return(FALSE);
}
/****************************************************************************/
static void vtnewline(vtrow, line)
int vtrow;
char *line;
{
	/* Replace the virtual terminal line at line vtrow with line */

	if (line == NULL) {
		/* Simply clear the line if not already done */

		if (vterm[vtrow].text[0] != '\0') {
			vterm[vtrow].text[0] = '\0';
			vterm[vtrow].nchanged = VT_REDRAW;
		}
	} else if (strlen(line) == strlen(vterm[vtrow].text)) {
		/* Length unchanged - update the line */

		vtchgline(vtrow, line);
	} else {
		/* Replace the line and set the redraw flag */

		free(vterm[vtrow].text);
		vterm[vtrow].text = line;
		vterm[vtrow].nchanged = VT_REDRAW;
	}

	/* Update the 'next line to display' counter */

	vt_next = (vtrow < vt_next) ? vtrow : vt_next;
	return;
}
/****************************************************************************/
static void vtchgline(vtrow, line)
int vtrow;
char *line;
{
	/* Update the virtual terminal line at line vtrow with line */

	int i;

	/* Update the changed substring of the line */

	for (i = 0; line[i] != '\0'; i++) {
		if (line[i] != vterm[vtrow].text[i]) {
			/* Make this character correct */

			vterm[vtrow].text[i] = line[i];

			/* Set the no of chars changed */
			
			if (vterm[vtrow].nchanged != VT_REDRAW
			    && vterm[vtrow].nchanged < i + 1) {
				vterm[vtrow].nchanged = i + 1;
			}
		}
	}

	/* Update the 'next line to display' counter */

	vt_next = (vtrow < vt_next) ? vtrow : vt_next;

	/* Free the new line and return */

	free(line);
	return;
}
/****************************************************************************/
void display(win)
WINDOW *win;
{
	/* Display the (probably modified) contents of a window */

	int line;
	unsigned old_modes;
	MESSAGE *m;

	/* Save and update the current modes */

	old_modes = cmodes(win->buf->modes);

	/* Display or clear each line in the window as required */

	m = win->first;
	for (line = win->top; line < win->bottom; line++) {
		/* Display any message appropriate to this line */

		if (m != NULL) {
			put_message(win, m, line);

			/* Is this the active line on the screen? */

			if (cwin == win && m == win->point) {
				active_line = line;
			}

			/* This is the last line so far */

			win->last = m;
			m = find_next(m);
		} else {
			/* Clear the virtual terminal line */

			vtnewline(line, NULL);
		}
	}

	/* Finally, output the window's status line */

	vtnewline(win->bottom, get_status(win));

	/* Restore the original modes and return */

	(void) cmodes(old_modes);
	return;
}
/****************************************************************************/
void first_display(win)
WINDOW *win;
{
	/* Display a window for the first time */

	MESSAGE *m;

	/* If the user wants us to, go to the first unread message */

	if (get_vval(V_FIRSTNEW)) {
		for (m = win->first; m->text != NULL; m = m->next) {
			if (m->visible && !m->read) {
				win->point = m;
				break;
			}
		}
	}

	/* Try to optimise the screen usage before we display */

	win->first = optimise(win, (win->bottom - win->top) / 2);

	/* Now display the buffer */

	display(win);
	return;
}
/****************************************************************************/
void redisplay(win)
WINDOW *win;
{
	/* Update a window after lines have been inserted or deleted */

	if (!point_visible(win) || win->point == win->first &&
	    win->point->text == NULL && win->bottom > win->top + 1) {
		/* We need to reposition the window before redrawing */

		win->first = optimise(win, (win->bottom - win->top) / 2);
	}

	/* Redraw the window and return */

	display(win);
	return;
}
/****************************************************************************/
void alldisplay(buf)
MAILBUF *buf;
{
	/* Update all windows displaying buf */

	WINDOW *w = cwin;

	/* Loop through each active window */

	do {
		/* Update if required */

		if (w->buf == buf) {
			redisplay(w);
		}

		w = w->next;
	} while (w != cwin);

	return;
}
/****************************************************************************/
void disp_kill(buf, message)
MAILBUF *buf;
MESSAGE *message;
{
	/* Update windows before killing message in buf */

	WINDOW *w = cwin;

	/* Loop through each active window */

	do {
		/* Does the window display the buffer? */

		if (w->buf == buf) {
			/* Update first, point and mark */

			if (w->first == message) {
				w->first = find_next(w->first);
			}
			if (w->point == message) {
				w->point = find_next(w->point);
			}
			if (w->mark == message) {
				w->mark = find_next(w->mark);
			}
		}

		w = w->next;
	} while (w != cwin);

	return;
}
/****************************************************************************/
void disp_narrow(win)
WINDOW *win;
{
	/* Update windows after narrowing or widening a buffer */

	WINDOW *w = win;

	/* Loop through each active window */

	do {
		/* Does the window display the buffer? */

		if (w->buf == win->buf) {
			/* Update first, point and mark */

			w->first = find_start(w->buf->messages);
			w->point = find_start(w->point);
			w->mark = find_start(w->mark);
		}

		w = w->next;
	} while (w != win);

	return;
}
/****************************************************************************/
void show_status(win, buf)
WINDOW *win;
MAILBUF *buf;
{
	/* Update the status line for windows displaying buf */

	unsigned old_modes;
	WINDOW *w = win;

	/* Save the old modes */

	if ((old_modes = cmodes(0)) & M_TYPEOUT) {
		/* Show the typeout status line */

		vtnewline(vt_lines - 1, get_status(NULL));
		return;
	}

	/* Loop through each window */

	do {
		/* Update the status line if required */

		if (buf == NULL || w->buf == buf) {
			/* Update the modes and redraw status line */

			(void) cmodes(w->buf->modes);
			vtnewline(w->bottom, get_status(w));
		}
		w = w->next;
	} while (w != win);

	/* Restore the original modes and return */

	(void) cmodes(old_modes);
	return;
}
/****************************************************************************/
void type_show(line, vtrow)
char *line;
int vtrow;
{
	/* Update a line and force its redisplay */

	int old_next = vt_next;

	/* Update and display the line */

	vtnewline(vtrow, (line != NULL) ? xstrdup(line) : NULL);
	(void) vtshowline(vtrow);

	/* Restore the next line to show */

	vt_next = old_next;
	return;
}
/****************************************************************************/
void type_display(first)
MSG_TEXT *first;
{
	/* Display a screen of typeout lines */

	int line = 0;
	MSG_TEXT *t;

	/* Show the lines of the typeout */

	for (t = first; t != NULL && line < vt_lines - 1; t = t->next) {
		vtnewline(line++, xstrdup(t->line));
	}

	/* Clear any remaining lines */

	while (line < vt_lines - 1) {
		vtnewline(line++, NULL);
	}

	/* Finally, show the status line */

	vtnewline(line, get_status(NULL));
	return;
}
/****************************************************************************/
static void put_message(win, message, vtrow)
WINDOW *win;
MESSAGE *message;
int vtrow;
{
	/* Write a single message to the virtual terminal */

	char *format, *line;

	/* Set up the line format and allocate space for the line */

	format = get_vtext(V_HDRLINE);
	line = xmalloc(win->cols);

	/* Now expand the format string into line */

	unformat(win, message, format, line, win->cols - 1, FALSE);

	/* Update the virtual terminal */

	vtnewline(vtrow, line);
	return;
}
/****************************************************************************/
static char *get_status(win)
WINDOW *win;
{
	/* Write the status line for a window */

	char *format, *line;
	int columns;
	MESSAGE *message;

	/* Get the message and the width for the line */

	message = (win != NULL) ? win->point : NULL;
	columns = (win != NULL) ? win->cols - 1 : vt_columns - 1;

	/* Set up the line format and allocate space for the line */

	format = get_vtext(V_MODELINE);
	line = xmalloc(columns + 1);

	/* Now expand the format string into line */

	unformat(win, message, format, line, columns, TRUE);
	return(line);
}
/****************************************************************************/
int move_point(win, nlines, backwards)
WINDOW *win;
int nlines, backwards;
{
	/* Move point nlines lines within the buffer */

	int scrolled = FALSE;
	MESSAGE *m, *next;

	/* Check there is at least one message to move to */

	m = win->point;
	if ((next = (backwards) ? find_prev(m) : find_next(m)) == NULL) {
		tbeep();
		return(FALSE);
	}

	/* Now find the new location of point */

	while (next != NULL && nlines--) {
		/* Check if we need to scroll */

		scrolled = (scrolled || backwards && m == win->first
			    || !backwards && m == win->last);
		m = next;
		next = (backwards) ? find_prev(next) : find_next(next);
	}

	/* Update the display as required */

	if (scrolled) {
		win->point = m;
		win->first = optimise(win, (win->bottom - win->top) / 2);
		display(win);
	} else {
		do_move(win, m);
	}

	return(TRUE);
}
/****************************************************************************/
void set_point(win, line)
WINDOW *win;
int line;
{
	/*
	 * Set the point to the given line of the buffer, or
	 * from the end of the buffer if line is negative.
	 */

	int lines;
	MESSAGE *next;

	/* Handle negative positions */

	line = (line < 0) ? cwin->buf->no_msgs + 1 + line : line;

	/* Initialise the pointers for the loop */

	win->point = find_start(win->buf->messages);
	next = find_next(win->point);
	lines = 1;

	/* Count through to the desired message */

	while (next != NULL && lines++ < line) {
		win->point = next;
		next = find_next(next);
	}

	/* Redraw the screen and return */

	redisplay(win);
	return;
}
/****************************************************************************/
void set_line(win, line)
WINDOW *win;
int line;
{
	/* Move point to a given position within a window */

	int lines = 0;
	MESSAGE *m = win->first;

	/* Find the new position of point */

	while (m != win->last && lines++ < line) {
		m = find_next(m);
	}

	/* And move point to the new position */

	do_move(win, m);
	return;
}
/****************************************************************************/
int scroll_win(win, nlines, down)
WINDOW *win;
int nlines, down;
{
	/* Scroll a window nlines lines, or a page if nlines is < 0 */

	MESSAGE *next;

	/* Check there is at least one message to scroll to */

	next = (down) ? find_prev(win->first) : find_next(win->last);

	if (next == NULL) {
		/* We can't scroll in this direction */

		emsgl((down) ? "Beginning" : "End", " of buffer", NULL);
		return(FALSE);
	}

	/* Now scroll as many lines as possible */

	while (next != NULL && nlines--) {
		/* Update the first and last message visible */

		win->first = (down) ? next : find_next(win->first);
		win->last = (down) ? find_prev(win->last) : next;

		/* And get the next line in the buffer */

		next = (down) ? find_prev(win->first) : find_next(win->last);
	}

	/* Now make sure that point is still visible */

	if (!point_visible(win)) {
		win->point = (down) ? find_last(win) : win->first;
	}

	/* Update the display and return */

	display(win);
	return(TRUE);
}
/****************************************************************************/
void centre(win, line)
WINDOW *win;
int line;
{
	/* Move point to the given line in the window */

	win->first = optimise(win, line);
	display(win);

	return;
}
/****************************************************************************/
static void do_move(win, point)
WINDOW *win;
MESSAGE *point;
{
	/* Move the point and redraw if required */

	unsigned old_modes;
	MESSAGE *m, *old_point;

	/* Save and update the current modes */

	old_modes = cmodes(win->buf->modes);

	/* Update the point */

	old_point = win->point;
	win->point = point;

	/* Redraw if the window being updated is active */

	if (win == cwin) {
		/* Redraw the original active line */
	
		put_message(win, old_point, active_line);

		/* Find the new active line */

		active_line = win->top;
		m = win->first;
		while (m != win->point) {
			m = find_next(m);
			active_line++;
		}

		/* Update the new active line  */

		put_message(win, win->point, active_line);
	}

	/* Update the status line */

	vtnewline(win->bottom, get_status(win));

	/* Restore the original modes and return */

	(void) cmodes(old_modes);
	return;
}
/****************************************************************************/
static int point_visible(win)
WINDOW *win;
{
	/* Check if point is visible in the window */

	int line = win->top;
	MESSAGE *m;

	/* Simply count through the lines looking for point */

	for (m = win->first; m != NULL && line < win->bottom; m = m->next) {
		/* Check for point or update the message count */

		if (m == win->point) {
			return(TRUE);
		} else if (m->visible) {
			line++;
		}
	}

	/* Point isn't visible in this window */

	return(FALSE);
}
/****************************************************************************/
static MESSAGE *find_last(win)
WINDOW *win;
{
	/* Return the last message visible in the window */

	int line = win->top;
	MESSAGE *m = win->first;
	MESSAGE *last = NULL;

	/* Simply count through the lines until the last */

	while (line < win->bottom && m->next != NULL) {
		/* Update the last message and line count */

		last = (m->visible) ? m : last;
		line += (m->visible) ? 1 : 0;
		m = m->next;
	}

	/* Now return the last line in the window */

	return(last);
}
/****************************************************************************/
static MESSAGE *optimise(win, line)
WINDOW *win;
int line;
{
	/*
	 * Return a first message for the window so that point
	 * would be as close to the given line from the top of
	 * the window (or the bottom of the window if line is
	 * negative) as possible, while still displaying as
	 * many messages as possible.
	 */

	int nlines, lines;
	MESSAGE *first, *next;

	/* How many lines are there in the window? */
	
	nlines = win->bottom - win->top;

	/* Handle negative line and check positive values */

	line = (line < 0) ? nlines + line : line;
	line = (line >= nlines) ? nlines - 1 : (line < 0) ? 0 : line;

	/* Initialise ready to check screen positions */

	first = win->point;
	lines = 0;

	/* Are there enough messages above the current one? */

	next = find_prev(win->point);
	while (next != NULL && lines < line) {
		first = next;
		next = find_prev(next);
		lines++;
	}

	/* Return the new first message */

	return(first);
}
/****************************************************************************/
MESSAGE *find_start(list)
MESSAGE *list;
{
	/* Return the first visible message in list */
	
	while (list != NULL && !(list->visible)) {
		list = list->next;
	}
	
	return(list);
}
/****************************************************************************/
MESSAGE *find_prev(list)
MESSAGE *list;
{
	/* Return the first visible message before list */

	while (list->prev != NULL && !(list->prev->visible)) {
		list = list->prev;
	}

	return(list->prev);
}
/****************************************************************************/
MESSAGE *find_next(list)
MESSAGE *list;
{
	/* Return the first visible message after list */

	while (list->next != NULL && !(list->next->visible)) {
		list = list->next;
	}

	return(list->next);
}
/****************************************************************************/
