/* Buf_cmd.c - Buffer-handling commands for af.
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
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "commands.h"
#include "mode.h"
#include "complete.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: buf_cmd.c,v 1.24 1996/08/28 17:44:08 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *vstrcat(), *get_cstr(), *get_dcstr();
extern int confirm(), long_confirm(), active();
extern int set_typeout_file(), error_in_typeout();
extern int chk_msg(), sort_msgs();
extern unsigned count_messages();
extern void free(), show_buffer(), show_new_buffer();
extern void rm_buffer(), list_buffers(), toggle_mode();
extern void emsg(), display(), redisplay();
extern void alldisplay(), disp_narrow();
extern MAILBUF *find_buffer(), *add_buffer();
extern WINDOW *add_window(), *del_window();
extern REGION *get_region();
extern CLIST *buf_complete(), *sort_complete();

/* Local function declarations */

static int do_switch(), do_kill();

/****************************************************************************/
/* Import the current window and user quit flag from commands.c */

extern WINDOW *cwin;
extern int user_quit;

/****************************************************************************/
/*ARGSUSED*/
FORM *switch_buf(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Set a named buffer as the active buffer */

	return((do_switch(cwin, forms, "Switch to buffer: "))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *owin_switch(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Switch buffer in the other window, creating it if required */

	int newlines = 0;
	WINDOW *oldwin = cwin;

	/* Make the new window if required */

	if ((cwin = cwin->next) == oldwin) {
		/* How many lines will the window have? */

		newlines = (cwin->bottom - cwin->top + 1) / 2;

		/* Now try to create the other window */

		if ((cwin = add_window(cwin, newlines)) == NULL) {
			return(c_errored());
		}

		/* Display the current buffer in the new window */

		show_buffer(cwin, oldwin->buf);
	}

	/* Switch to the new buffer */

	if (!do_switch(cwin, forms, "Switch to buffer in other window: ")) {
		/* Delete any new window and fail */

		if (newlines > 0) {
			(void) del_window(cwin);
		}
		cwin = oldwin;
		return(c_errored());
	}

	/* Update the old window and return */

	redisplay(oldwin);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *kill_buf(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Delete a named buffer */

	char *bufnam;
	MAILBUF *buf;

	/* We are not allowed to delete the last buffer */

	if (cwin->buf->next == cwin->buf) {
		emsg("Can't kill buffer: only one buffer");
		return(c_errored());
	}

	/* Get the buffer to delete */

	if ((bufnam = get_dcstr(forms, "Kill buffer: ", cwin->buf->name,
				buf_complete, C_STRICT)) == NULL) {
		return(c_errored());
	}

	/* Find the buffer and delete it */

	buf = cwin->buf;
	do {
		if (!strcmp(buf->name, bufnam)) {
			/* Kill the buffer and return status */

			return((do_kill(cwin, buf)) ? c_t() : c_errored());
		}

		/* Check the next buffer */

		buf = buf->next;
	} while (buf != cwin->buf);

	/* If we got here the buffer didn't exist */

	emsg("No such buffer");
	return(c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *kill_some(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Loop through the buffers asking about killing them */

	char *prompt;
	MAILBUF *buf;

	/* We are not allowed to delete the last buffer */

	if (cwin->buf->next == cwin->buf) {
		emsg("Can't kill buffer: only one buffer");
		return(c_errored());
	}

	/* Loop through the buffers */

	buf = cwin->buf;
	do {
		/* Ask if we want to kill the buffer */

		prompt = vstrcat("Kill buffer ", buf->name, "? ", NULL);
		if (confirm(prompt, FALSE) && !do_kill(cwin, buf)) {
			/* Failed to kill the buffer */

			free(prompt);
			return(c_errored());
		}

		/* Free the prompt and move on to the next buffer */

		free(prompt);
		buf = buf->next;

	} while (!user_quit && cwin->buf->next != cwin->buf
		 && buf != cwin->buf);

	/* Clean up the screen and return */

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *buf_list(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* List all buffers to typeout */

	/* Check that there are buffers available */

	if (cwin == NULL) {
		emsg("No buffers available");
		return(c_errored());
	}

	/* Redirect typeout to a file if argument given */

	if (!set_typeout_file(forms, arg, "buffer list")) {
		return(c_errored());
	}

	/* List the buffers and return status */

	list_buffers(cwin);
	return((error_in_typeout()) ? c_errored() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *widen(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Widen the current buffer if narrowed */

	MESSAGE *m;

	/* Check the buffer has been narrowed */

	if (!active(cwin->buf, M_NARROW)) {
		emsg("Can't widen: buffer is not narrowed");
		return(c_errored());
	}

	/* Make all messages in the buffer visible */

	for (m = cwin->buf->messages; m != NULL; m = m->next) {
		m->visible = TRUE;
	}

	/* Turn off narrowed mode in the buffer */

	toggle_mode(cwin->buf, M_NARROW);

	/* Update the number of messages in the buffer */

	cwin->buf->no_msgs = count_messages(cwin->buf->messages, FALSE);

	/* Update the display and return success */

	disp_narrow(cwin);
	alldisplay(cwin->buf);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *sort_buffer(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Sort the current buffer into a semblance of order */

	char *sortname;

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Get which sort order the user wants */

	if ((sortname = get_cstr(forms, "Sort buffer by: ", sort_complete,
				 C_STRICT)) == NULL) {
		return(c_errored());
	}

	/* Now do the sorting */

	if (!sort_msgs(cwin, "buffer", sortname,
		       cwin->buf->messages, NULL, NULL)) {
		/* Error sorting the messages */

		return(c_errored());
	}

	/* Update the display and return success */

	if (!active(cwin->buf, M_READONLY)) {
		cwin->buf->st_mod = TRUE;
	}
	alldisplay(cwin->buf);

	return(c_t());
}
/****************************************************************************/
static int do_switch(win, forms, prompt)
WINDOW *win;
FORM *forms;
char *prompt;
{
	/* Actually handle switching buffers */

	char *bufnam, *deflt;
	MAILBUF *newbuf;

	/* Do we default to the other or next buffer? */

	deflt = (win->other != NULL) ? win->other->name
				     : win->buf->next->name;

	/* Ask the user which buffer to switch to */

	if ((bufnam = get_dcstr(forms, prompt, deflt, buf_complete,
				C_PERMISSIVE)) == NULL) {
		return(FALSE);
	}

	/* Find or create the required buffer */

	if ((newbuf = find_buffer(win->buf, bufnam)) == NULL) {
		newbuf = add_buffer(win->buf, bufnam, NULL, NULL, M_MAIL);
	}

	/* Update the display and return success */

	show_buffer(win, newbuf);
	redisplay(win);
	return(TRUE);
}
/****************************************************************************/
static int do_kill(win, buf)
WINDOW *win;
MAILBUF *buf;
{
	/* Actually kill a buffer */

	/* Check if the buffer has been modified */

	if (buf->mod && !long_confirm("Buffer modified; delete anyway? ",
				      TRUE)) {
		return(FALSE);
	}

	/* Fix the display if the buffer was onscreen */

	show_new_buffer(win, buf, buf->next);

	/* Delete the buffer and return success */

	rm_buffer(buf);
	return(TRUE);
}
/****************************************************************************/
