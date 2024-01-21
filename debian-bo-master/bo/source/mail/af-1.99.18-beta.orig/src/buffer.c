/* Buffer.c - Buffer handling for af.
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
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "mode.h"
#include "complete.h"
#include "io.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: buffer.c,v 1.53 1997/03/31 18:32:19 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup();
extern char *vstrcat(), *strerror(), *utos();
extern char *get_vtext();
extern int long_confirm(), active(), is_blank();
extern int buf_displayed(), read_failed();
extern int write_messages(), sort_default();
extern long atol(), filesize();
extern void free(), msg(), msgl(), cmsg();
extern void typeout(), show_new_buffer();
extern void set_sys_tags(), toggle_mode();
extern MESSAGE *get_messages(), *null_msg();
extern MESSAGE *find_start(), *find_prev();
extern MESSAGE *find_next();
extern CLIST *add_clist();

/* Local function declarations */

int sync_buffer(), new_messages();
unsigned count_messages();
unsigned count_bad_messages();
void free_messages(), free_mtext();
void rm_buffer();
static char *buf_name();
static int read_buffer();
static int reread_buffer();
static int identical();
static MAILBUF *new_buffer();
static MESSAGE *find_identical();

/****************************************************************************/
/* Import the current window from commands.c */

extern WINDOW *cwin;

/****************************************************************************/
MAILBUF *add_buffer(list, bufnam, filnam, pending, modes)
MAILBUF *list;
char *bufnam, *filnam, *pending;
unsigned modes;
{
	/*
	 * Add a new buffer, named bufnam if bufnam is non-null, or
	 * otherwise deriving the name from the file name (which
	 * must be non-null).  The contents of the named file, if
	 * non-null, are to be read into the buffer.
	 * The buffer is inserted before the current buffer.
	 */

	char *newnam;
	MAILBUF *newbuf;
	
	/* Set the buffer name and generate a buffer */

	newnam = (bufnam != NULL) ? xstrdup(bufnam) : buf_name(list, filnam);
	newbuf = new_buffer(newnam, filnam, pending, modes);
	free(newnam);

	/* Read any file associated with the buffer */

	if (filnam != NULL && !read_buffer(newbuf)) {
		rm_buffer(newbuf);
		return(list);
	} else if (filnam == NULL) {
		newbuf->messages = newbuf->point = null_msg(NULL);
	}

	/* Add the buffer to the list */

	if (list == NULL) {
		newbuf->prev = newbuf->next = newbuf;
	} else {
		list->prev->next = newbuf;
		newbuf->prev = list->prev;
		list->prev = newbuf;
		newbuf->next = list;
	}

	/* Return the new buffer */

	return(newbuf);
}
/****************************************************************************/
int buf_pending(buf, filnam)
MAILBUF *buf;
char *filnam;
{
	/*
	 * Make a buffer pending on the named file.  This means that
	 * the buffer will be considered to contain two files, and
	 * will be resynchronised from both.  When the buffer is
	 * saved, the pending folder is truncated.
	 */

	int no_old, no_bad = 0;
	MESSAGE *first;

	/* How many old messages are there? */

	no_old = count_messages(buf->messages, TRUE);

	/* First read the pending file */

	if ((first = get_messages(filnam, buf->messages, 0L)) == NULL
	    && read_failed()) {
		return(FALSE);
	}

	/* If the buffer was empty then set up the message list */

	buf->messages = (!no_old && first != NULL) ? first : buf->messages;

	/* Set the buffer's other variables */

	buf->pending = xstrdup(filnam);
	buf->peofpos = filesize(buf->pending);
	buf->no_msgs = count_messages(buf->messages, TRUE);
	buf->st_mod = (buf->st_mod || first != NULL);

	/* Count the number of bad headers */

	no_bad = count_bad_messages(buf->messages, TRUE);

	/* Confirm the reads were successful */

	msgl("(Read ", utos(no_old), " pending and ", NULL);
	cmsg(utos(buf->no_msgs - no_old));
	cmsg(" new message");
	cmsg((buf->no_msgs != no_old + 1) ? "s" : "");

	/* And report any with bad headers */

	if (no_bad) {
		cmsg("; including ");
		cmsg(utos(no_bad));
		cmsg(" with bad headers");
	}
	cmsg(")");

	/* We can return success */

	return(TRUE);
}
/****************************************************************************/
void rm_buffer(buf)
MAILBUF *buf;
{
	/* Remove a buffer from the list of buffers */

	buf->next->prev = buf->prev;
	buf->prev->next = buf->next;

	/* And free the space it uses */

	free(buf->name);
	if (buf->file != NULL) {
		free(buf->file);
	}
	if (buf->pending != NULL) {
		free(buf->pending);
	}
	free_messages(buf->messages);
	free(buf);

	return;
}
/****************************************************************************/
MAILBUF *find_buffer(list, bufnam)
MAILBUF *list;
char *bufnam;
{
	/* Return the buffer named bufnam, or NULL if no such buffer */

	MAILBUF *buf = list;

	/* Loop through the existing buffers */

	do {
		/* Is this the buffer we're looking for? */

		if (!strcmp(buf->name, bufnam)) {
			return(buf);
		}
		buf = buf->next;
	} while (buf != list);

	/* If we reach here there's no such buffer */

	return(NULL);
}
/****************************************************************************/
MAILBUF *find_by_file(list, filnam)
MAILBUF *list;
char *filnam;
{
	/* Return the buffer displaying file filnam, or NULL if none */

	MAILBUF *buf = list;

	/* Search for the file in the buffer list */

	do {
		/* Does this buffer display the file? */

		if (buf->file != NULL && !strcmp(buf->file, filnam) ||
		    buf->pending != NULL && !strcmp(buf->pending, filnam)) {
			return(buf);
		}
		buf = buf->next;
	} while (buf != list);

	/* If we reach here no buffer displays the file */

	return(NULL);
}
/****************************************************************************/
void insert(win, list)
WINDOW *win;
MESSAGE *list;
{
	/* Insert the list into the buffer at point */

	MESSAGE *prev, *next, *m;

	/* Check if we are inserting at the top of the window */

	if (win->point == win->first) {
		win->first = list;
	}

	/* Find the first visible message before point */

	prev = find_prev(win->point);

	/* Insert the list into the buffer */

	if ((list->prev = prev) == NULL) {
		next = win->buf->messages;
		win->buf->messages = list;
	} else {
		next = prev->next;
		prev->next = list;
	}

	/* Search for the last new message and then set it up */

	win->buf->no_msgs++;
	for (m = list; m->next != NULL; m = m->next) {
		win->buf->no_msgs++;
	}
	m->next = next;
	next->prev = m;

	/* Set the mark at the first new message */

	win->mark = list;
	return;
}
/****************************************************************************/
int sync_buffer(buf, silent)
MAILBUF *buf;
int silent;
{
	/* Make a buffer identical to its file */

	int empty_buf;
	unsigned old_hdrs, old_bad, new_bad;
	MESSAGE *first, *more;

	/* Check if the files have grown */

	if (filesize(buf->file) <= buf->eofpos &&
	    (buf->pending == NULL ||
	     filesize(buf->pending) <= buf->peofpos)) {
		return(FALSE);
	}

	/* Check if the buffer is empty */

	empty_buf = (buf->messages == NULL || buf->messages->text == NULL);

	/* And store the original number of (bad) messages */

	old_hdrs = buf->no_msgs;
	old_bad = count_bad_messages(buf->messages, TRUE);

	/* That done, get any new mail */

	first = (filesize(buf->file) > buf->eofpos) ?
		get_messages(buf->file, buf->messages, buf->eofpos) : NULL;
	more = 	(buf->pending != NULL &&
		 filesize(buf->pending) > buf->peofpos) ?
			 get_messages(buf->pending, buf->messages,
				      buf->peofpos) : NULL;

	/* Check for errors resyncing the buffer */

	if (filesize(buf->file) > buf->eofpos && first == NULL
	    || buf->pending != NULL && filesize(buf->pending)
	    > buf->eofpos && more == NULL) {
		return(FALSE);
	}

	/* Set up what we found */

	first = (first == NULL) ? more : first;

	/* If the buffer was empty then set up the message list */

	buf->messages = (empty_buf) ? first : buf->messages;

	/* Set the buffer size, used to check against when writing */

	buf->eofpos = filesize(buf->file);
	buf->peofpos = (buf->pending != NULL) ? filesize(buf->pending) : 0;

	/* Set the buffer's other variables */

	buf->no_msgs = count_messages(first, TRUE);
	buf->st_mod = (buf->st_mod || more != NULL
		       || new_messages(first));

	/* Count the number of new bad messages */

	new_bad = count_bad_messages(buf->messages, TRUE) - old_bad;

	/* Confirm the resynchronisation */

	msgl("(Read ", utos(buf->no_msgs - old_hdrs), " message", 
	     (buf->no_msgs != old_hdrs + 1) ? "s" : "", NULL);

	/* Report messages with bad headers */

	if (new_bad) {
		cmsg("; including ");
		cmsg(utos(new_bad));
		cmsg(" with bad headers");
	}
	cmsg(")");

	/* Resynchronised; return success */

	return(TRUE);
}
/****************************************************************************/
int write_buffer(buf)
MAILBUF *buf;
{
	/* Write a single buffer out to disk */

	char *prompt, *efile = NULL;
	int status, pstatus = WM_OK;

	/* Check whether the files have shrunk */

	if (filesize(buf->file) < buf->eofpos) {
		/* The buffer's file is truncated */

		efile = buf->file;
	} else if (buf->pending != NULL &&
		   filesize(buf->pending) < buf->peofpos) {
		/* The pending file is truncated */

		efile = buf->pending;
	}

	/* Get confirmation if a file has shrunk */

	if (efile != NULL) {
		/* Set up the prompt and confirm for it */

		prompt = vstrcat("File ", efile, " has shrunk; ",
				 "save anyway? ", NULL);
		status = long_confirm(prompt, TRUE);
		free(prompt);

		/* Return if confirmation not obtained */

		if (!status) {
			return(FALSE);
		}
	}

	/* Write the buffer and clear any pending folder */

	if ((status = write_messages(buf->messages, buf->file,
				     buf->eofpos)) == WM_OK
	    && buf->pending != NULL && filesize(buf->pending)) {
		/* Clear the pending folder */

		pstatus = write_messages(NULL, buf->pending, buf->peofpos);
	}

	/* Update the buffer's sizes if required */

	buf->eofpos = (status == WM_OK) ? filesize(buf->file) : buf->eofpos;
	buf->peofpos = (pstatus == WM_OK) ? 0L : buf->peofpos;

	/* Resynchronise or reread the buffer if required */
  
	if ((status == WM_REREAD || pstatus == WM_REREAD)
	    && reread_buffer(buf)) {
		/* Confirm the reread */

		msgl("Reread ", utos(count_messages(buf->messages, FALSE)),
		     " messages to resynchronise with mailbox", NULL);
	} else if ((status == WM_RESYNC || pstatus == WM_RESYNC)
		   && sync_buffer(buf, FALSE)) {
		/* Confirm the resync */

		msgl("(New mail has arrived in ",
		     (status == WM_RESYNC) ? buf->file :
		     buf->pending, ")", NULL);
	}

	/* Reset the buffer's status flags */

	if (status == WM_OK && pstatus == WM_OK) {
		buf->mod = buf->st_mod = FALSE;
	} else if (status == WM_FAILED || pstatus == WM_FAILED) {
		buf->mod = TRUE;
	}

	/* And return whether we saved successfully */

	return(status == WM_OK && pstatus == WM_OK);
}
/****************************************************************************/
int count_buffers(list)
MAILBUF *list;
{
	/* Count the number of active buffers */

	int nbuffers = 0;
	MAILBUF *buf = list;

	/* Loop through the buffers */

	do {
		nbuffers++;
		buf = buf->next;
	} while (buf != list);

	/* Return the number of buffers */

	return(nbuffers);
}
/****************************************************************************/
static char *buf_name(list, folder)
MAILBUF *list;
char *folder;
{
	/* Return a unique buffer name based on folder */

	char *p, *basenam, *bufnam = NULL;
	unsigned buf_ext = 1;

	/*
	 * Set the buffer name.  This defaults to the last component
	 * of the pathname used to open the file.
	 */

	if ((p = strrchr(folder, DIRSEP)) != NULL) {
		basenam = p + 1;
	} else {
		basenam = folder;
	}

	/* If the list is NULL then that's it */

	if (list == NULL) {
		return(xstrdup(basenam));
	}

	/* Loop until the buffer name is unique */

	do {
		/* Select the name to try */

		if (bufnam != NULL) {
			free(bufnam);
			bufnam = vstrcat(basenam, "<",
				 utos(buf_ext++), ">", NULL);
		} else {
			bufnam = xstrdup(basenam);
		}
	} while (find_buffer(list, bufnam) != NULL);
					
	/* Return the unique buffer name */

	return(bufnam);
}
/****************************************************************************/
static MAILBUF *new_buffer(bufnam, filnam, pending, modes)
char *bufnam, *filnam, *pending;
unsigned modes;
{
	/* Initialise a new mail buffer ready for use */

	MAILBUF *buf;
	
	/* Allocate the space for the buffer */

	buf = (MAILBUF *) xmalloc(sizeof(MAILBUF));

	/* Set the name and file for the buffer */

	buf->name = xstrdup(bufnam);
	buf->file = (filnam != NULL) ? xstrdup(filnam) : NULL;
	buf->pending = (pending != NULL) ? xstrdup(pending) : NULL;

	/* Initialise the buffer */

	buf->messages = buf->point = buf->mark = NULL;
	buf->eofpos = buf->peofpos = 0;
	buf->no_msgs = 0;
	buf->modes = modes;
	buf->mod = buf->st_mod = FALSE;
	buf->prev = buf->next = buf;

#ifdef READ_VIA_POP3
	/* We may need POP3 mode on the buffer too */

	if (filnam != NULL && POP3_MBOX(filnam)) {
		toggle_mode(buf, M_POP3);
	}
#endif /* READ_VIA_POP3 */

	return(buf);
}
/****************************************************************************/
static int read_buffer(buf)
MAILBUF *buf;
{
	/* Read the file associated with the buffer, returning status */

	char *sortname;
	unsigned old_hdrs, pdg_hdrs, new_hdrs;
	unsigned old_bad, new_bad;
	MESSAGE *first = NULL;

	/* Give the user a message to confirm the read */

	msgl("Reading ", buf->file, "...", NULL);

	/* Count the original messages and errors in the list */

	old_hdrs = buf->no_msgs;
	old_bad = count_bad_messages(buf->messages, TRUE);

	/* Now read the messages into the buffer */

	if ((buf->messages = buf->point =
	     get_messages(buf->file, buf->messages, 0L)) == NULL
	    || read_failed()) {
		/* Error reading the folder */

		return(FALSE);
	}

	/* Count the number of pending messages read */

	pdg_hdrs = count_messages(buf->messages, TRUE) - old_hdrs;

	/* Read the pending folder too if there is one */

	if (buf->pending != NULL &&
	    (first = get_messages(buf->pending, buf->messages, 0L)) == NULL
	    && read_failed()) {
		/* Error reading the pending folder */

		return(FALSE);
	}

	/* If the buffer was empty then set up the message list */

	buf->messages = (!pdg_hdrs && first != NULL) ? first : buf->messages;
	
	/* Set the buffer's other variables */

	buf->eofpos = filesize(buf->file);
	buf->peofpos = (buf->pending != NULL) ? filesize(buf->pending) : 0;
	buf->no_msgs = count_messages(buf->messages, TRUE);
	buf->st_mod = (new_messages(buf->messages) ||
		       buf->no_msgs > pdg_hdrs + old_hdrs);

	/* Sort the new buffer if required */

	if (buf->no_msgs && (sortname = get_vtext(V_INIT_SORT)) != NULL) {
		(void) sort_default(buf, sortname);
	}

	/* Count the number of messages read */

	new_hdrs = buf->no_msgs - old_hdrs;
	new_bad = count_bad_messages(buf->messages, TRUE) - old_bad;

	/* Confirm that the read was successful */

	msgl("(Read ", utos(pdg_hdrs), NULL);
	if (buf->pending != NULL) {
		new_hdrs -= pdg_hdrs;
		cmsg(" pending and ");
		cmsg(utos(new_hdrs));
		cmsg(" new");
	}
	cmsg(" message");
	cmsg((new_hdrs != 1) ? "s" : "");

	/* Report messages with bad headers */

	if (new_bad) {
		cmsg("; including ");
		cmsg(utos(new_bad));
		cmsg(" with bad headers");
	}
	cmsg(")");

	/* All's well; return success */

	return(TRUE);
}
/****************************************************************************/
static int reread_buffer(buf)
MAILBUF *buf;
{
	/* Make a buffer identical to its file by rereading it */

	int skip;
	MAILBUF *newbuf;
	MESSAGE *message;
	MESSAGE *m, *n;

	/* Find or create and read the buffer */

	if ((newbuf = add_buffer(buf, buf->name, buf->file,
				 NULL, M_MAIL)) == buf) {
		return(FALSE);
	}

	/* Ensure the buffer name is correct */

	free(newbuf->name);
	newbuf->name = xstrdup(buf->name);

	/* Now preserve any status flags on messages */

	for (m = buf->messages; m != NULL && m->text != NULL; m = m->next) {
		/* Find any identical messages in the buffer */

		skip = 0;
		for (n = buf->messages; n != m; n = n->next) {
			/* Are these messages identical? */

			skip += (identical(m, n)) ? 1 : 0;
		}

		/* Does this message have flags to preserve? */

		if (((!m->visible) || !(m->new) || m->read || m->deleted ||
		     m->replied || m->forwarded || m->saved || m->printed)
		    && (message = find_identical(newbuf, m, skip)) != NULL) {
			/* Update the new buffer's modification flags */

			newbuf->mod = (newbuf->mod || m->deleted);
			newbuf->st_mod = (newbuf->st_mod ||
					  !message->read && m->read);

			/* Copy the status of this message across */

			message->visible = m->visible;
			message->new = m->new;
			message->read = m->read;
			message->deleted = m->deleted;
			message->replied = m->replied;
			message->forwarded = m->forwarded;
			message->saved = m->saved;
			message->printed = m->printed;

			/* And update the system tags */

			set_sys_tags(message);
		}

		/* Is this message the original point or mark? */

		if ((cwin->point == m || cwin->mark == m) &&
		    (message = find_identical(newbuf, m, skip)) != NULL) {
			/* Set the new buffer's point or mark */

			newbuf->point = (cwin->point == m) ?
				message : newbuf->point;
			newbuf->mark = (cwin->mark == m) ?
				message : newbuf->mark;
		}
	}

	/* Update windows and delete the old buffer */

	show_new_buffer(cwin, buf, newbuf);
	rm_buffer(buf);
	return(TRUE);
}
/****************************************************************************/
static MESSAGE *find_identical(buf, message, skip_identical)
MAILBUF *buf;
MESSAGE *message;
int skip_identical;
{
	/* Find the skip_identical message identical to message in buf */

	MESSAGE *m;

	/* Loop over the messages in the buffer */

	for (m = buf->messages; m != NULL && m->text != NULL; m = m->next) {
		/* Are these two messages identical? */

		if (identical(m, message) && skip_identical-- <= 0) {
			/* This is the message we want */

			return(m);
		}
	}

	/* No identical message found in the buffer */

	return(NULL);
}
/****************************************************************************/
static int identical(message, original)
MESSAGE *message, *original;
{
	/* Return TRUE if two messages are identical */

	int headers = TRUE;
	MSG_TEXT *t, *u;

	/* If we have an ID for both messages then just compare them */

	if (message->id != NULL && original->id != NULL) {
		/* Return the comparison of the ids */

		return(!strcmp(message->id, original->id));
	}

	/* Set up to compare the text of the messages */

	t = message->text;
	u = original->text;

	/* Loop over the text of the messages, comparing them */

	while (t != NULL && u != NULL) {
		/* Are we processing the headers? */

		headers = (headers && !is_blank(t->line));

		/* Skip From and status lines in the message */

		while (headers &&
		       (!strncmp(t->line, MFROM, strlen(MFROM)) ||
			!strncasecmp(t->line, STATUS, strlen(STATUS)) ||
			!strncasecmp(t->line, X_STATUS, strlen(X_STATUS)) ||
			!strncasecmp(t->line, AFSTATUS, strlen(AFSTATUS)))) {
			/* Skip the From or status line */

			t = t->next;
		}

		/* Skip From and status lines in the original */

		while (headers &&
		       (!strncmp(u->line, MFROM, strlen(MFROM)) ||
			!strncasecmp(u->line, STATUS, strlen(STATUS)) ||
			!strncasecmp(u->line, X_STATUS, strlen(X_STATUS)) ||
			!strncasecmp(u->line, AFSTATUS, strlen(AFSTATUS)))) {
			/* Skip the From or status line */

			u = u->next;
		}
			
		/* Does this line of the messages match? */

		if (strcmp(t->line, u->line)) {
			return(FALSE);
		}

		/* Move on the the next line of each message */

		t = t->next;
		u = u->next;
	}

	/* Now check the message lengths */

	return(t == NULL && u == NULL);
}
/****************************************************************************/
void free_messages(list)
MESSAGE *list;
{
	/* Free a list of messages */

	int ref;

	/* This is a no-op if list is null */

	if (list != NULL) {
		/* Free any later messages */

		free_messages(list->next);

		/* And then free this message */

		if (list->from != NULL) {
			free(list->from);
		}
		if (list->addr != NULL) {
			free(list->addr);
		}
		if (list->subject != NULL) {
			free(list->subject);
		}
		if (list->reply != NULL) {
			free(list->reply);
		}
		if (list->group != NULL) {
			free(list->group);
		}
		if (list->cc != NULL) {
			free(list->cc);
		}
		if (list->contype != NULL) {
			free(list->contype);
		}
		if (list->encoding != NULL) {
			free(list->encoding);
		}
		if (list->date != NULL) {
			free(list->date);
		}
		for (ref = 0; ref < NO_REFERENCES; ref++) {
			if (list->refs[ref] != NULL) {
				free(list->refs[ref]);
			}
		}
		if (list->id != NULL) {
			free(list->id);
		}
		if (list->sys_tags != NULL) {
			free(list->sys_tags);
		}
		if (list->user_tags != NULL) {
			free(list->user_tags);
		}
		free_mtext(list->text);
		free(list);
	}
	return;
}
/****************************************************************************/
void free_mtext(text)
MSG_TEXT *text;
{
	/* Free the text of a message */

	if (text != NULL) {
		free_mtext(text->next);
		free(text->line);
		free(text);
	}
	return;
}
/****************************************************************************/
unsigned count_messages(list, all)
MESSAGE *list;
int all;
{
	/* Return the number of (visible) messages in the list */

	int nmesg = 0;
	MESSAGE *m;

	/* Just loop over the messages, counting them */
	
	for (m = list; m != NULL && m->text != NULL; m = m->next) {
		/* Messages may only count if they're visible */

		if (all || m->visible) {
			nmesg++;
		}
	}
	return(nmesg);
}
/****************************************************************************/
unsigned count_bad_messages(list, all)
MESSAGE *list;
int all;
{
	/* Return the number of (visible) bad messages in the list */

	int nerror = 0;
	MESSAGE *m;

	/* Just loop over the messages, counting them */

	for (m = list; m != NULL && m->text != NULL; m = m->next) {
		/* Messages may only count if they're visible */

		if (m->bad && (all || m->visible)) {
			nerror++;
		}
	}
	return(nerror);
}
/****************************************************************************/
unsigned position(list, message)
MESSAGE *list, *message;
{
	/* Return the position of message in list */

	int line_no = 1;
	MESSAGE *m;

	/* Count the line number of the message */

	for (m = find_start(list); m != message; m = find_next(m)) {
		line_no++;
	}
	return(line_no);
}
/****************************************************************************/
int new_messages(list)
MESSAGE *list;
{
	/* Return TRUE if any messages are new, FALSE otherwise */

	MESSAGE *m;

	/* Search the list looking for visible new messages */

	for (m = list; m != NULL && m->text != NULL; m = m->next) {
		if (m->visible && m->new) {
			return(TRUE);
		}
	}

	/* No new messages in the list */

	return(FALSE);
}
/****************************************************************************/
CLIST *buf_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of buffer names completing base */

	MAILBUF *b = cwin->buf;
 
	/* Build the list of possible values */

	do {
		/* Is this a possible completion? */

		if (!strncmp(base, b->name, strlen(base))) {
			list = add_clist(list, b->name, TRUE);
		}
		b = b->next;
	} while (b != cwin->buf);

	return(list);
}
/****************************************************************************/
void list_buffers(win)
WINDOW *win;
{
	/* List all the existing buffers to typeout */

	char line[BUFSIZ];
	MAILBUF *b = win->buf;

	/* Put some initial information */

	typeout("MR  Buffer            Msgs  File\n");
	typeout("--  ------            ----  ----\n");

	/* Display available buffers */

	do {
		/* Show information for the buffer */

		(void) sprintf(line, "%c%c  %-16s %5u  %s\n",
			       (buf_displayed(win, b)) ? BS_VISIBLE : ' ',
			       (b->mod) ? BS_CHANGED :
			       (b->st_mod) ? BS_STATUS :
			       (active(b, M_READONLY)) ? BS_READONLY : ' ',
			       b->name, b->no_msgs,
			       (b->file != NULL) ? b->file : "");
		typeout(line);

		/* And move to the next buffer */

		b = b->next;
	} while (b != win->buf);

	typeout(NULL);
	return;
}
/****************************************************************************/
