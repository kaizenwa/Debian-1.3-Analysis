/* Move_cmd.c - Commands for moving around a buffer in af.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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
#include <ctype.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "commands.h"
#include "variable.h"
#include "mode.h"
#include "complete.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: move_cmd.c,v 1.16 1996/03/17 01:10:47 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *get_estr(), *strkey(), *utos();
extern int atoi(), chk_msg(), get_vval();
extern int move_point(), scroll_win();
extern unsigned cmodes(), position();
extern void set_point(), set_line(), centre();
extern void vtredraw(), msg(), cmsg(), emsg();
extern ARGUMENT *form_or_arg();

/****************************************************************************/
/* Import the current window and last command executed from commands.c */

extern WINDOW *cwin;
extern COMMAND *last_command;

/****************************************************************************/
FORM *prev_line(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move to the previous line, scrolling if necessary */

	int nlines, backwards;

	/* Let the minibuffer and typeout do their own thing */

	if (cmodes(0) & M_MBUF) {
		return(prev_hist(seq, arg, forms));
	} else if (cmodes(0) & M_TYPEOUT) {
		return(to_prev(seq, arg, forms));
	}

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Check how far we are to move in which direction */

	nlines = (arg != NULL) ? arg->value : 1;
	backwards = (arg == NULL || !arg->negative);

	/* Update point as required */

	return((move_point(cwin, nlines, backwards)) ? c_t() : c_errored());
}
/****************************************************************************/
FORM *next_line(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move to the next message, scrolling if necessary */

	int nlines, backwards;

	/* Let the minibuffer and typeout do their own thing */

	if (cmodes(0) & M_MBUF) {
		return(next_hist(seq, arg, forms));
	} else if (cmodes(0) & M_TYPEOUT) {
		return(to_next(seq, arg, forms));
	}

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Check how far we are to move in which direction */

	nlines = (arg != NULL) ? arg->value : 1;
	backwards = (arg != NULL && arg->negative);

	/* Update point as required */

	return((move_point(cwin, nlines, backwards)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *move_win(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move point to a given line within the window */

	int line, nlines;

	/* How many lines are there in the window? */

	if ((nlines = cwin->bottom - cwin->top) > cwin->buf->no_msgs + 1) {
		nlines = cwin->buf->no_msgs + 1;
	}

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate which line to move to */

	if (arg != NULL && !arg->negative) {
		line = arg->value;
	} else if (arg != NULL && arg->negative) {
		line = nlines - arg->value;
	} else {
		line = nlines / 2;
	}

	/* And move to that line */

	set_line(cwin, line);
	return(c_t());
}
/****************************************************************************/
FORM *goto_line(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move to an arbitrary line by number */

	char *deflt, *ltext;
	int line_no;

	/* Let typeout do it's own thing */

	if (cmodes(0) & M_TYPEOUT) {
		return(to_goto(seq, arg, forms));
	}

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Get the line from the argument or interactively */

	if (arg != NULL) {
		/* Argument gives line number to go to */

		line_no = (arg->negative) ? -(arg->value) : arg->value;
	} else {
		/* If bound to a numeric key use that as a default */

		deflt = (isdigit(LASTKEY(seq))) ?
			strkey(LASTKEY(seq), SK_KEYSEQ) : NULL;

		/* Get the line number to go to and convert to integer */

		if ((ltext = get_estr(NULL, "Go to line: ", deflt)) == NULL) {
			return(c_errored());
		}
		line_no = atoi(ltext);
	}

	/* Go to the specified line */

	set_point(cwin, line_no);
	return(c_t());
}
/****************************************************************************/
FORM *buf_start(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Go to the first message in the buffer */

	int line = 1;

	/* Let the minibuffer and typeout do their own thing */

	if (cmodes(0) & M_MBUF) {
		return(hist_start(seq, arg, forms));
	} else if (cmodes(0) & M_TYPEOUT) {
		return(to_start(seq, arg, forms));
	}

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* With an argument moves to a percentage of the file */

	if (arg != NULL && !arg->negative) {
		line = arg->value * (cwin->buf->no_msgs + 1) / 10 + 1;
	} else if (arg != NULL && arg->negative) {
		line = (10 - arg->value) * (cwin->buf->no_msgs + 1) / 10 + 1;
	}

	/* Set the mark and do the update */

	cwin->mark = cwin->point;
	set_point(cwin, line);
	msg("(Mark set)");

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *buf_end(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Go to the last message in the buffer */

	int line = cwin->buf->no_msgs + 1;

	/* Let the minibuffer and typeout do their own thing */

	if (cmodes(0) & M_MBUF) {
		return(hist_end(seq, arg, forms));
	} else if (cmodes(0) & M_TYPEOUT) {
		return(to_end(seq, arg, forms));
	}

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* With an argument moves to a percentage of the file */

	if (arg != NULL && !arg->negative) {
		line = (10 - arg->value) * (cwin->buf->no_msgs + 1) / 10 + 1;
	} else if (arg != NULL && arg->negative) {
		line = arg->value * (cwin->buf->no_msgs + 1) / 10 + 1;
	}

	/* Set the mark and do the update */

	cwin->mark = cwin->point;
	set_point(cwin, line);
	msg("(Mark set)");

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *down_scroll(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Scroll the current window down */

	int wlines, nlines, down;

	/* Tyepout has its own scrolling routines */

	if (cmodes(0) & M_TYPEOUT) {
		return(to_down(seq, arg, forms));
	}

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Check how many lines we are to scroll by default */

	if ((wlines = cwin->bottom - cwin->top - get_vval(V_CONTEXT)) < 1) {
		wlines = 1;
	}

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Check how far we are to scroll in which direction */

	nlines = (arg != NULL) ? arg->value : wlines;
	down = (arg == NULL || !arg->negative);

	/* Scroll the window as required */

	return((scroll_win(cwin, nlines, down)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *up_scroll(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Scroll the current window up */

	int wlines, nlines, down;

	/* Tyepout has its own scrolling routines */

	if (cmodes(0) & M_TYPEOUT) {
		return(to_up(seq, arg, forms));
	}

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Check how many lines we are to scroll by default */

	if ((wlines = cwin->bottom - cwin->top - get_vval(V_CONTEXT)) < 1) {
		wlines = 1;
	}

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Check how far we are to scroll in which direction */

	nlines = (arg != NULL) ? arg->value : wlines;
	down = (arg != NULL && arg->negative);

	/* Scroll the window as required */

	return((scroll_win(cwin, nlines, down)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *owin_scroll(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Scroll the other window up a page */

	int wlines, nlines, down;

	/* Check there is another window */

	if (cwin->next == cwin) {
		emsg("There is no other window");
		return(c_errored());
	}

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin->next, FALSE)) {
		return(c_errored());
	}

	/* Check how many lines we are to scroll by default */

	if ((wlines = cwin->bottom - cwin->top - get_vval(V_CONTEXT)) < 1) {
		wlines = 1;
	}

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Check how far we are to scroll in which direction */

	nlines = (arg != NULL) ? arg->value : wlines;
	down = (arg != NULL && arg->negative);

	/* Scroll the window as required */

	return((scroll_win(cwin->next, nlines, down)) ? c_t() : c_errored());
}
/****************************************************************************/
FORM *recenter(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Redraw the window with the current line at the centre */

	/* Let the minibuffer and typeout do their own thing */

	if (cmodes(0) & M_MBUF) {
		return(redraw(seq, arg, forms));
	} else if (cmodes(0) & M_TYPEOUT) {
		return(to_redraw(seq, arg, forms));
	}

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* This command works slightly differently with no argument */

	if (arg != NULL) {
		/* Place point as required by arg */

		centre(cwin, (arg->negative) ? -(arg->value) : arg->value);
	} else {
		/* Centre point in the window and redraw the display */

		centre(cwin, (cwin->bottom - cwin->top) / 2);
		vtredraw();
	}
	return(c_t());
}
/****************************************************************************/
FORM *cursor_pos(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Display the position within the buffer */

	/* Let typeout do it's own thing */

	if (cmodes(0) & M_TYPEOUT) {
		return(to_cursor(seq, arg, forms));
	}

	/* Simply display the current position */

	msg("Message ");
	cmsg(utos(position(cwin->buf->messages, cwin->point)));
	cmsg(" of ");
	cmsg(utos(cwin->buf->no_msgs));

	return(c_t());
}
/****************************************************************************/
