/* File_cmd.c - File-handling commands for af.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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
#include "variable.h"
#include "mode.h"
#include "complete.h"
#include STRING_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: file_cmd.c,v 1.29 1997/03/31 18:32:19 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xstrdup(), *vstrcat(), *strerror();
extern char *expand(), *fullpath(), *pathfind(), *utos();
extern char *get_str(), *get_cstr(), *get_vtext();
extern int chk_readonly(), chk_pop3(), loading();
extern int confirm(), long_confirm(), write_buffer();
extern int sync_buffer(), read_failed(), buf_pending();
extern int mb_touched(), mb_cleared();
extern unsigned count_messages(), count_bad_messages();
extern void free(), msg(), msgl(), emsg(), emsgl(), cmsg();
extern void insert(), show_buffer(), show_new_buffer();
extern void first_display(), display(), redisplay();
extern void alldisplay(), show_status(), set_point();
extern void rm_buffer(), toggle_mode(), free_messages();
extern void new_killbuf(), kill_message();
extern FORM *load();
extern ARGUMENT *form_or_arg();
extern MESSAGE *get_messages();
extern MAILBUF *find_by_file(), *add_buffer();
extern WINDOW *add_window(), *del_window();
extern CLIST *fn_complete();

/* Local function declarations */

static int do_find(), do_read(), do_save();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the current window and user quit flag from commands.c */

extern WINDOW *cwin;
extern int user_quit;

/****************************************************************************/
/*ARGSUSED*/
FORM *find_file(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Read a file into a new buffer and display it */

	return((do_find(cwin, forms, "Find file: ", M_MAIL))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *owin_find(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Find a file in the other window, creating it if required */

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

	/* Find the new file */

	if (!do_find(cwin, forms, "Find file in other window: ", M_MAIL)) {
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
FORM *find_readonly(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Read a file into a new buffer and display it in Read-Only mode */

	return((do_find(cwin, forms, "Find file read-only: ",
			M_MAIL | M_READONLY)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *find_alternate(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Replace the current buffer with a named file */

	char *filnam, *prompt;
	int status, read_status;

	/* If we've changed the buffer then offer to save */

	if (cwin->buf->mod && cwin->buf->file != NULL) {
		/* See if we want to save the buffer */

		prompt = vstrcat("Save modified ",
				 cwin->buf->file, "? ", NULL);
		status = confirm(prompt, FALSE);
		free(prompt);

		/* Check for a quit or the save failing */

		if (!user_quit && status && !do_save(cwin, cwin->buf)) {
			return(c_errored());
		}
	}

	/* Get the filename to read */

	if ((filnam = get_cstr(forms, "Find alternate file: ", fn_complete,
			       C_PERMISSIVE)) == NULL) {
		return(c_errored());
	}

	/* Expand the file name to it's full form */

	filnam = fullpath(filnam);

	/* Read the new buffer */

	read_status = do_read(cwin, NULL, filnam);

	/* Free the file name and return */

	free(filnam);
	return((read_status) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *read_pending(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Append a folder to the buffer and make it pending */

	char *filnam;

	/* Check that the buffer isn't read only or POP3 */

	if (!chk_readonly(cwin) || !chk_pop3(cwin)) {
		return(c_errored());
	}

	/* Check that the buffer doesn't have a pending folder */

	if (cwin->buf->pending != NULL) {
		emsgl("Can't read pending file: Buffer already ",
		      " pending on ", cwin->buf->pending, NULL);
		return(c_errored());
	}

	/* Get the filename to read */

	if ((filnam = get_cstr(forms, "Read pending file: ", fn_complete,
			       C_CAUTIOUS)) == NULL) {
		return(c_errored());
	}

	/* Expand the file name to it's full form */

	filnam = fullpath(filnam);

	/* Check if the file is already being displayed */

	if (find_by_file(cwin->buf, filnam) != NULL) {
		emsgl("Can't make ", filnam, " pending: ",
		      "Already read into a buffer", NULL);
		return(c_errored());
	}

	/* Make the specified file pending */

	(void) buf_pending(cwin->buf, filnam);
	alldisplay(cwin->buf);

	/* And return success */

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *revert_buffer(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Reread the current buffer from disk */

	/* Check the buffer has a file to revert to */

	if (cwin->buf->file == NULL) {
		emsg("Can't revert buffer: No file");
		return(c_errored());
	}

	/* If we've changed the buffer then OK discarding changes */

	if (cwin->buf->mod && !long_confirm("Discard changes? ", TRUE)) {
		return(c_errored());
	}

	/* Read the new buffer */

	return(do_read(cwin, cwin->buf->name, cwin->buf->file)
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *insert_file(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Insert the contents of a file into the buffer */

	char *filnam;
	unsigned new_hdrs, new_bad;
	MESSAGE *new_msgs, *m;

	/* Check that the buffer isn't read only */

	if (!chk_readonly(cwin) || !chk_pop3(cwin)) {
		return(c_errored());
	}

	/* Get the file to insert */

	if ((filnam = get_cstr(forms, "Insert file: ", fn_complete, 
			       C_CAUTIOUS)) == NULL) {
		return(c_errored());
	}
	filnam = expand(filnam);

	/* Let the user know what we're doing */

	msgl("Reading ", filnam, "...", NULL);
	
	/* Read any messages in the folder */

	new_msgs = get_messages(filnam, NULL, 0L);
	free(filnam);

	/* Check that messages were read */

	if (!count_messages(new_msgs, TRUE)) {
		/* Free the messages and confirm status */

		free_messages(new_msgs);

		if (!read_failed()) {
			emsg("Read 0 messages");
			return(c_t());
		}
		return(c_errored());
	}

	/* Was the read successful? */

	if (!read_failed()) {
		/* Count the number of (bad) messages read */

		new_hdrs = count_messages(new_msgs, TRUE);
		new_bad = count_bad_messages(new_msgs, TRUE);

		/* Confirm the read was successful */

		msgl("(Read ", utos(new_hdrs), " message",
		     (new_hdrs != 1) ? "s" : "", NULL);

		/* Report messages with bad headers */

		if (new_bad) {
			cmsg("; including ");
			cmsg(utos(new_bad));
			cmsg(" with bad headers");
		}
		cmsg(")");
	}

	/* Remove the null message from the list */

	for (m = new_msgs; m->next->text != NULL; m = m->next) {
		/* NULL LOOP */
	}
	free(m->next);
	m->next = NULL;

	/* Do the insert and update the display */

	insert(cwin, new_msgs);
	cwin->buf->mod = TRUE;
	alldisplay(cwin->buf);

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *resync(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Resync the current buffer - make it identical to its file */

	unsigned last_no;

	/* Check that there is a file associated with the buffer */

	if (cwin->buf->file == NULL) {
		emsg("Can't resynchronise buffer: No file");
		return(c_errored());
	}

	/* Store the last message number so we can return to it */
	
	last_no = cwin->buf->no_msgs;

	/* Display a message for the user */

	msgl("Resynchronising ", cwin->buf->file, "...", NULL);

	if (sync_buffer(cwin->buf, FALSE)) {
		/* Update the display and go to the first new message */

		alldisplay(cwin->buf);
		set_point(cwin, last_no + 1);
	} else if (!user_quit && !read_failed()) {
		msgl("Resynchronising ", cwin->buf->file, "... Done", NULL);
	}

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *not_modified(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Change the modification status of the buffer */

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Now modify or unmodify the buffer */

	if (arg != NULL) {
		/* Mark the buffer as modified */

		cwin->buf->mod = TRUE;
		msg("(Modification flag set)");
	} else {
		/* Mark the buffer as unmodified */

		cwin->buf->mod = cwin->buf->st_mod = FALSE;
		msg("(Modification flag cleared)");
	}

	/* Show the buffer's status and return success */

	show_status(cwin, cwin->buf);
	return(c_t());
}
/****************************************************************************/
FORM *save(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Save the current buffer to disk */

	/* If no changes then just return */

	if (!cwin->buf->mod && !cwin->buf->st_mod) {
		msg("(No changes to save)");
		return(c_t());
	}

	/* May need to save to a named file if no name set */

	if (cwin->buf->file == NULL) {
		return(save_name(seq, arg, forms));
	}

	/* Save the file and return status */

	return((do_save(cwin, cwin->buf)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *save_name(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Save the current buffer to a named file */

	char *filnam, *prompt;
	char *oldfile, *oldpending;
	int status;
	long oldeofpos, oldpeofpos;

	/* Get the file name to write to */

	if ((filnam = get_cstr(forms, "Write file: ",
		fn_complete, C_PERMISSIVE)) == NULL) {
		return(c_errored());
	}

#ifdef READ_VIA_POP3
	/* We can't write a buffer to a POP3 folder */

	if (POP3_MBOX(filnam)) {
		emsg("Can't write buffer to a POP3 folder");
		return(c_errored());
	}
#endif /* READ_VIA_POP3 */

	/* Check if the file already exists */

	if (access(filnam, 00) == 0) {
		/* File exists, check we mean to overwrite it */

		prompt = vstrcat("Overwrite existing ", filnam, "? ", NULL);
		status = long_confirm(prompt, TRUE);
		free(prompt);

		/* Return if confirmation not obtained */

		if (!status) {
			return(c_errored());
		}
	}

	/* Save the buffer's files and sizes */

	oldfile = cwin->buf->file;
	oldpending = cwin->buf->pending;
	oldeofpos = cwin->buf->eofpos;
	oldpeofpos = cwin->buf->peofpos;

	/* Set the buffer's file names and end of files */

	cwin->buf->file = fullpath(filnam);
	cwin->buf->pending = NULL;
	cwin->buf->eofpos = -1;
	cwin->buf->peofpos = 0;

	/* Set the modified flag, to force the write */

	cwin->buf->mod = TRUE;

	/* Write the file and check status */

	if (!do_save(cwin, cwin->buf)) {
		/* Restore the old file and pending */

		free(cwin->buf->file);
		cwin->buf->file = oldfile;
		cwin->buf->pending = oldpending;
		cwin->buf->eofpos = oldeofpos;
		cwin->buf->peofpos = oldpeofpos;

		/* And return failure */

		return(c_errored());
	}

	/* Succeeded; this buffer can't be in POP3 mode now */

	if (cwin->buf->modes & M_POP3) {
		/* Turn off Pop3 mode and update the mode line */

		toggle_mode(cwin->buf, M_POP3);
		show_status(cwin, cwin->buf);
	}

	/* Free the old filenames and return success */

	if (oldfile != NULL) {
		free(oldfile);
	}
	if (oldpending != NULL) {
		free(oldpending);
	}
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *save_some(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Arbitrarily save some of the changed buffers */

	char *prompt;
	int status, needed_save = FALSE;
	MAILBUF *buf;

	/* Loop through each buffer, saving it. */

	buf = cwin->buf;
	do {
		/* Has the buffer been modified? */

		if (buf->file != NULL && (buf->mod || buf->st_mod)) {
			/* Some buffers needed saving */

			needed_save = TRUE;

			/* Check if we want to save the buffer */

			prompt = vstrcat("Save file ", buf->file, "? ", NULL);
			status = confirm(prompt, FALSE);
			free(prompt);

			/* Check for a quit or the save failing */

			if (user_quit || status && !do_save(cwin, buf)) {
				return(c_errored());
			}
		}

		/* Check the next buffer */

		buf = buf->next;
	} while (buf != cwin->buf);

	/* Output a message if no work required */

	if (!needed_save) {
		msg("(No files need saving)");
	}

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *save_all(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Save all of the changed buffers to their files */

	int needed_save = FALSE;
	MAILBUF *buf;

	/* Loop through each buffer, saving it. */

	buf = cwin->buf;
	do {
		/* Has the buffer been modified? */

		if (buf->file != NULL && (buf->mod || buf->st_mod)) {
			/* Some buffers needed saving */

			needed_save = TRUE;

			if (!do_save(cwin, buf)) {
				return(c_errored());
			}
		}

		/* Check the next buffer */

		buf = buf->next;
	} while (buf != cwin->buf);

	/* Output a message if no work required */

	if (!needed_save) {
		msg("(No files need saving)");
	}

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *load_file(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Load a file and execute the commands it contains */

	char *filnam;
	FORM *status;

	/* Get the file to load */

	if ((filnam = get_cstr(forms, "Load file: ",
		fn_complete, C_CAUTIOUS)) == NULL) {
		return(c_errored());
	}

	/* Expand + and ~ in the file name */

	filnam = expand(filnam);

	/* Give the user a message if not already loading a file */

	if (!loading()) {
		msgl("Loading ", filnam, "...", NULL);
		(void) mb_touched();
	}

	/* Load the file and confirm the load */

	if ((status = load(filnam)) != NULL && !ERRORED(status)
	    && !loading() && !mb_touched() && !mb_cleared()) {
		/* No output, confirm the load */

		cmsg(" Done");
	}

	/* Free space and return status */

	free(filnam);
	return(status);
}
/****************************************************************************/
/*ARGSUSED*/
FORM *load_lib(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Load a library using the load-path variable */

	char *filnam, *fullnam;
	FORM *status;

	/* Get the file to load */

	if ((filnam = get_str(forms, "Load library: ")) == NULL) {
		return(c_errored());
	}

	/* Find the file in the load path */

	if ((fullnam = pathfind(filnam, get_vtext(V_LOADPATH),
				AFLISP)) == NULL) {
		/* No such file, report an error */

		emsgl("Can't open load file ", filnam,
		      ": ", strerror(errno), NULL);
		return(c_errored());
	}

	/* Give the user a message if not already loading a file */

	if (!loading()) {
		msgl("Loading ", fullnam, "...", NULL);
	}

	/* Load the library and confirm the load */

	if ((status = load(fullnam)) != NULL
	    && !ERRORED(status) && !loading()) {
		cmsg(" Done");
	}

	/* Free space and return status */

	free(fullnam);
	return(status);
}
/****************************************************************************/
static int do_find(win, forms, prompt, modes)
WINDOW *win;
FORM *forms;
char *prompt;
unsigned modes;
{
	/* Actually handle finding a file */

	char *filnam;
	MAILBUF *newbuf;

	/* Get the filename to find */

	if ((filnam = get_cstr(forms, prompt, fn_complete,
			       C_PERMISSIVE)) == NULL) {
		return(FALSE);
	}

	/* Expand the file name to it's full form */

	filnam = fullpath(filnam);

	/* Find or create and read the buffer and then display it */

	if ((newbuf = find_by_file(win->buf, filnam)) != NULL) {
		show_buffer(win, newbuf);
		redisplay(win);
	} else if ((newbuf = add_buffer(win->buf, NULL, filnam,
					NULL, modes)) != win->buf) {
		show_buffer(win, newbuf);
		first_display(win);
	} else  {
		return(FALSE);
	}
	free(filnam);

	/* And return success */

	return(TRUE);
}
/****************************************************************************/
static int do_read(win, bufnam, filnam)
WINDOW *win;
char *bufnam, *filnam;
{
	/* Handle replacing a buffer with a new one */

	MAILBUF *newbuf, *oldbuf;

	/* Find or create and read the buffer */

	if ((win->buf->file == NULL || strcmp(win->buf->file, filnam))
	    && (newbuf = find_by_file(win->buf, filnam)) != NULL) {
		/* Just display the buffer */

		show_buffer(win, newbuf);
		redisplay(win);
		return(TRUE);
	} else if ((newbuf = add_buffer(win->buf, bufnam, filnam,
					NULL, M_MAIL)) == win->buf) {
		return(FALSE);
	}

	/* Fix the buffer name if required */

	if (win->buf->file != NULL && !strcmp(win->buf->file, newbuf->file)) {
		/* Ensure the buffer name is correct */

		free(newbuf->name);
		newbuf->name = xstrdup(win->buf->name);
	}

	/* Update the current window */

	oldbuf = win->buf;
	show_buffer(win, newbuf);
	first_display(win);

	/* Update windows and delete the old buffer */

	show_new_buffer(win, oldbuf, newbuf);
	rm_buffer(oldbuf);
	return(TRUE);
}
/****************************************************************************/
static int do_save(win, buf)
WINDOW *win;
MAILBUF *buf;
{
	/* Handle saving a buffer to disk */

	int status, msg_no = 1;
	int killed = FALSE;
	MESSAGE *msg, *next;

#ifdef READ_VIA_POP3
	MESSAGE *m;
#endif /* READ_VIA_POP3 */

       	/* Write the buffer, checking for failure */

	status = write_buffer(buf);

	/* Kill any deleted messages and update positions */

	msg = buf->messages;
	while (status && msg != NULL && msg->text != NULL) {
		/* Get the next message in the list */

		next = msg->next;

		/* Kill or renumber this message as required */

		if (msg->deleted) {
			/* Do we need to set up a new kill buffer? */

			if (!killed) {
				new_killbuf();
				killed = TRUE;
			}
#ifdef READ_VIA_POP3
			/* Renumber later messages in a POP3 folder */

			for (m = buf->messages; m != NULL && m->text != NULL
			     && POP3_MBOX(buf->file); m = m->next) {
				/* Renumber this message */

				m->pos -= (m->pos > msg->pos) ? 1 : 0;
			}
#endif /* READ_VIA_POP3 */

			/* Now kill the message */

			kill_message(buf, msg, FALSE);
#ifdef READ_VIA_POP3
		} else if (!POP3_MBOX(buf->file)) {
#else /* ! READ_VIA_POP3 */
		} else {
#endif /* ! READ_VIA_POP3 */
			/* Renumber this message */

			msg->pos = msg_no++;
		}

		/* And move on to the next message */

		msg = next;
	}

	/* Update the screen and return status */

	alldisplay(buf);
	return(status);
}
/****************************************************************************/
