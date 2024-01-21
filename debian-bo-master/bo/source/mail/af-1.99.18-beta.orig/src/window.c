/* Window.c - Window handling for af.
   Copyright (C) 1992, 1994, 1996 Malc Arnold.

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
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: window.c,v 1.11 1996/10/17 18:53:02 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc();
extern int tlines(), tcols();
extern void free(), emsg();
extern void display(), redisplay();
extern MESSAGE *find_start();

/* Local function declarations */

WINDOW *add_window();

/****************************************************************************/
WINDOW *init_windows(nwindows)
int nwindows;
{
	/* Initialise the windows ready for use */

	int nlines;
	WINDOW *newwin, *win, *owin;

	/* Allocate the space for the window */

	newwin = (WINDOW *) xmalloc(sizeof(WINDOW));

	/* Initialise the window */

	newwin->buf = newwin->other = NULL;
	newwin->first = newwin->last = NULL;
	newwin->point = newwin->mark = NULL;

	/* The window occupies the entire screen less echo area */

	newwin->top = 0;
	newwin->bottom = tlines() - 2;
	newwin->cols = tcols();

	/* The window is the only entry in the list */

	newwin->prev = newwin->next = newwin;

	/* Now split the window as required */

	win = newwin;
	while (nwindows-- > 1) {
		/* How big will the window be? */

		if ((nlines = (win->bottom - win->top + 1) / 2) >= 2) {
			/* There's room to split the window */
	
			owin = add_window(win, nlines);
			win = owin->next;
		}
	}

	/* Return the initialised window list */

	return(newwin);
}
/****************************************************************************/
WINDOW *add_window(win, nlines)
WINDOW *win;
int nlines;
{
	/* Split win into two smaller windows */

	int olines;
	WINDOW *newwin;
	
	/* How big are the current and new windows? */

	olines = win->bottom - win->top + 1;
	nlines = (nlines < 0) ? olines + nlines : nlines;

	/* Check that there is room for another window */

	if (olines - nlines < 2 || nlines < 2) {
		emsg("Can't split window: resulting window too small");
		return(NULL);
	}

	/* Allocate the space for the window */

	newwin = (WINDOW *) xmalloc(sizeof(WINDOW));

	/* Initialise the window */

	newwin->buf = newwin->other = NULL;
	newwin->first = newwin->last = NULL;
	newwin->point = newwin->mark = NULL;

	/* Add the new window below the current one */

	newwin->bottom = win->bottom;
	win->bottom -= nlines;
	newwin->top = win->bottom + 1;
	newwin->cols = tcols();

	/* And therefore after it in the list */

	newwin->prev = win;
	newwin->next = win->next;
	win->next = newwin;
	newwin->next->prev = newwin;

	/* Return the new window */

	return(newwin);
}
/****************************************************************************/
WINDOW *del_window(win)
WINDOW *win;
{
	/* Delete the specified window, returning an alternative */

	WINDOW *newwin;

	/* Check the delete is permitted */

	if (win->prev == win) {
		emsg("Can't delete window: only one window");
		return(NULL);
	}

	/* Select the new window and reset the size */

	if (win->top != 0) {
		newwin = win->prev;
		newwin->bottom = win->bottom;
	} else {
		newwin = win->next;
		newwin->top = win->top;
	}

	/* Unlink the window from the list */

	win->prev->next = win->next;
	win->next->prev = win->prev;

	/* Set the displayed buffer's copies of the point and mark */

	win->buf->point = win->point;
	win->buf->mark = win->mark;

	/* Free the space occupied by the window */

	free(win);

	/* Return the other window */

	return(newwin);
}
/****************************************************************************/
WINDOW *resize_window(win, mod)
WINDOW *win;
int mod;
{
	/*
	 * Modify the size of size win by the number of lines
	 * given in mod, returning the other modified window.
	 */

	/* Check there is another window */

	if (win->prev == win) {
		emsg("Can't resize window: only one window");
		return(NULL);
	}

	/* Check that the resize is valid */

	if (win->bottom - win->top + mod < 1) {
		emsg("Can't resize window: resulting window too small");
		return(NULL);
	}

	/* May be modifying the previous or next window */

	if (win->top < win->prev->top) {
		/* Check that the resize is valid */

		if (win->next->bottom - win->next->top - mod < 1) {
			emsg("Can't resize window: resulting window too small");
			return(NULL);
		}

		/* And resize the window */

		win->next->top += mod;
		win->bottom += mod;
		return(win->next);
	} else {
		/* Check that the resize is valid */

		if (win->prev->bottom - win->prev->top - mod < 1) {
			emsg("Can't resize window: resulting window too small");
			return(NULL);
		}

		/* And resize the window */

		win->prev->bottom -= mod;
		win->top -= mod;
		return(win->prev);
	}
	/*NOTREACHED*/
}
/****************************************************************************/
int count_windows(list)
WINDOW *list;
{
	/* Count the number of active windows */

	WINDOW *win = list;
	int nwindows = 0;

	/* Loop over the existing windows */

	do {
		nwindows++;
		win = win->next;
	} while (win != list);

	/* Return the number of windows */

	return(nwindows);
}
/****************************************************************************/
void show_buffer(win, buf)
WINDOW *win;
MAILBUF *buf;
{
	/* Display buffer buf in window win */

	WINDOW *w;

	/* Save the current point and mark in the buffer */

	if (win->buf != NULL) {
		win->buf->point = win->point;
		win->buf->mark = win->mark;
	}

	/* Save any current buffer as the other buffer */

	win->other = win->buf;

	/* Recover the details of the buffer to be displayed */

	win->buf = buf;
	win->first = find_start(buf->messages);
	win->point = buf->point;
	win->mark = buf->mark;

	/* Get point and mark of displayed buffers from the window */

	for (w = win->next; w != win; w = w->next) {
		if (w->buf == buf) {
			win->point = w->point;
			win->mark = w->mark;
			break;
		}
	}

	return;
}
/****************************************************************************/
void show_new_buffer(win, buf, newbuf)
WINDOW *win;
MAILBUF *buf, *newbuf;
{
	/* Make any windows displaying buf display newbuf instead */

	WINDOW *w = win;

	/* Loop through the available windows */

	do {
		/* Does the window display buf? */

		if (w->buf == buf) {
			show_buffer(w, newbuf);
			redisplay(win);
		}

		/* Or is buf the window's other buffer? */

		if (w->other == buf) {
			w->other = NULL;
		}
		w = w->next;
	} while (w != win);

	return;
}
/****************************************************************************/
int buf_displayed(win, buf)
WINDOW *win;
MAILBUF *buf;
{
	/* Return TRUE if buf is being displayed in a window */
	
	WINDOW *w = win;
	
	/* Just check each available window for buf */
	
	do {
		if (w->buf == buf) {
			return(TRUE);
		}
		w = w->next;
	} while (w != win);

	/* Buffer not displayed in any window */

	return(FALSE);
}
/****************************************************************************/
#ifdef HAVE_RESIZING
void fix_windows(win)
WINDOW *win;
{
	/* Update the window sizes after SIGWINCH caught */

	int toadd, offset;
	WINDOW *w = win, *x;
	WINDOW *first = NULL;

	/* Reset the columns and find the top window */

	do {
		if (w->top == 0) {
			first = w;
		}

		w->cols = tcols();
		w = w->next;
	} while (w != win);

	/* Find the number of lines added and the size offset */

	toadd = tlines() - 2 - first->prev->bottom;
	offset = (toadd > 0) ? 1 : -1;

	/* Update the window sizes */

	for (w = first; toadd != 0; w = w->next) {
		/* Update the size of this window */

		w->bottom += offset;
		toadd -= offset;

		/* And update any remaining windows */

		for (x = w->next; x != first; x = x->next) {
			/* Update the position of this window */

			x->top += offset;
			x->bottom += offset;
		}
	}

	/* Redisplay all windows */

	w = first;
	do {
		display(w);
		w = w->next;
	} while (w != first);

	return;
}
#endif /* HAVE_RESIZING */
/****************************************************************************/
