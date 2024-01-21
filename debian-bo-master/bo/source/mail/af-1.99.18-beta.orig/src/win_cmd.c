/* Win_cmd.c - Window handling commands for af.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */


#include <stdio.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "commands.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: win_cmd.c,v 1.6 1996/03/17 01:10:47 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern void show_buffer(), redisplay(), display(), emsg();
extern WINDOW *add_window(), *del_window(), *resize_window();
extern ARGUMENT *form_or_arg();

/* Local function declarations */

static int change_win();

/****************************************************************************/
/* Import the current window from commands.c */

extern WINDOW *cwin;

/****************************************************************************/
/*ARGSUSED*/
FORM *split_win(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Divide the current window in two horizontally */

	int nlines;
	WINDOW *newwin;

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* How big should we make the new window? */

	nlines = (arg != NULL) ? (arg->negative) ? -(arg->value)
		: arg->value : (cwin->bottom - cwin->top + 1) / 2;

	/* Make the new window and set it to the current buffer */

	if ((newwin = add_window(cwin, nlines)) == NULL) {
		return(c_errored());
	}
	show_buffer(newwin, cwin->buf);

	/* Display the modified windows */

	cwin = newwin;
	redisplay(cwin->prev);
	redisplay(cwin);

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *del_cwin(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Delete the current window */

	WINDOW *newwin;

	/* Delete the window if we can */

	if ((newwin = del_window(cwin)) != NULL) {
		cwin = newwin;
		display(cwin);
		return(c_t());
	}

	/* Error deleting the window */

	return(c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *del_owin(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Delete all windows but the current window */

	/* Loop over the windows, deleting them */

	while (cwin->next != cwin) {
		(void) del_window(cwin->next);
	}

	/* Update the remaining window */

	display(cwin);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *other_win(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Make the other (ie. next) window current */

	int nwins, backwards;

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Set up how many windows to change and in what direction */

	nwins = (arg != NULL) ? arg->value : 1;
	backwards = (arg != NULL && arg->negative);

	/* Now change window as required */

	return(change_win(nwins, backwards) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *prev_win(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Make the previous window current */

	int nwins, backwards;

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Set up how many windows to change and in what direction */

	nwins = (arg != NULL) ? arg->value : 1;
	backwards = (arg == NULL || !arg->negative);

	/* Now change window as required */

	return(change_win(nwins, backwards) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *enlarge_win(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Make the current window larger */

	int nlines;
	WINDOW *otherwin;

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Determine how many lines to enlarge the window */

	nlines = (arg != NULL) ? (arg->negative)
		? -(arg->value) : arg->value : 1;

	/* Resize the window and check success */

	if ((otherwin = resize_window(cwin, nlines)) == NULL) {
		return(c_errored());
	}
	
	/* Redraw the changed windows */

	redisplay(otherwin);
	redisplay(cwin);

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *shrink_win(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Make the current window smaller */

	int nlines;
	WINDOW *otherwin;

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Determine how many lines to shrink the window */

	nlines = (arg != NULL) ? (arg->negative)
		? arg->value : -(arg->value) : -1;

	/* Resize the window and check success */

	if ((otherwin = resize_window(cwin, nlines)) == NULL) {
		return(c_errored());
	}
	
	/* Redraw the changed windows */

	redisplay(otherwin);
	redisplay(cwin);

	return(c_t());
}
/****************************************************************************/
static int change_win(nwins, backwards)
int nwins, backwards;
{
	/* Handle changing focus from one window to another */

	WINDOW *old_win = cwin;

	/* Check there is another window */

	if (cwin->next == cwin) {
		emsg("Can't change window: only one window");
		return(FALSE);
	}

	/* Switch window as required */

	while (nwins--) {
		cwin = (backwards) ? cwin->prev : cwin->next;
	}

	/* And update the display */

	display(old_win);
	display(cwin);

	return(TRUE);
}
/****************************************************************************/
