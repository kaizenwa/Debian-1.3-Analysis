/* Killbuf.c - Kill buffer handling for af.
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
#include "killbuf.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "tags.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: killbuf.c,v 1.20 1997/03/31 18:32:19 malc Exp $";
static char *KillbufId = KILLBUFID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xstrdup();
extern int get_vval(), set_tags();
extern void disp_kill(), free_messages();
extern void set_sys_tags(), mask_tags();
extern void free_tlist();
extern TAG_LIST *taglist();

/* Local function declarations */

static void do_kill(), add_killbuf();
static MESSAGE *cp_message();

/****************************************************************************/
/* The static kill ring and its size */

static KILLBUF *killbuf = NULL;
static int ring_size = 0;

/****************************************************************************/
void new_killbuf()
{
	/* Set up a new kill buffer in the ring */

	KILLBUF *k;

	/* Is the kill ring as large as it can get? */

	if (killbuf == NULL || ring_size < get_vval(V_KILL_RING)) {
		/* Add a new entry to the kill ring */

		k = (KILLBUF *) xmalloc(sizeof(KILLBUF));
		k->prev = (killbuf != NULL) ? killbuf : k;
		k->next = (killbuf != NULL) ? killbuf->next : k;
		k->prev->next = k->next->prev = k;
		k->messages = NULL;
		ring_size++;
		killbuf = k;
	} else {
		/* Clear and use the next kill ring entry */

		killbuf = killbuf->next;
		free_messages(killbuf->messages);
		killbuf->messages = NULL;
	}

	return;
}
/****************************************************************************/
void kill_message(buffer, message, prepend)
MAILBUF *buffer;
MESSAGE *message;
int prepend;
{
	/* Kill the message and add it to the kill buffer */

	do_kill(buffer, message, TRUE, prepend);
	return;
}
/****************************************************************************/
void copy_message(message)
MESSAGE *message;
{
	/* Copy the message into the kill buffer */

	add_killbuf(cp_message(message, FALSE), FALSE);
	return;
}
/****************************************************************************/
void del_message(buffer, message)
MAILBUF *buffer;
MESSAGE *message;
{
	/* Delete the message without adding it to the kill buffer */

	do_kill(buffer, message, FALSE, FALSE);
	return;
}
/****************************************************************************/
MESSAGE *get_killbuf()
{
	/* Return the kill buffer for a yank */

	MESSAGE *yank_buf, *m, *b;

	/* If the kill buffer is empty return NULL */

	if (killbuf == NULL) {
		return(NULL);
	}

	/* Copy the first entry in the kill buffer */

	yank_buf = cp_message(killbuf->messages, TRUE);

	m = killbuf->messages;
	b = yank_buf;

	/* And copy the rest */

	while (m->next != NULL) {
		b->next = cp_message(m->next, TRUE);
		b->next->prev = b;
		m = m->next;
		b = b->next;
	}

	return(yank_buf);
}
/****************************************************************************/
void pop_killbuf(no_pops, forwards)
int no_pops, forwards;
{
	/* Pop the kill ring the specified number of times */

	while (no_pops-- && killbuf != NULL) {
		killbuf = (forwards) ? killbuf->next : killbuf->prev;
	}
	return;
}
/****************************************************************************/
static void do_kill(buffer, message, save, prepend)
MAILBUF *buffer;
MESSAGE *message;
int save, prepend;
{
	/* Handle killing of messages from buffers */

	/* Update the windows for after the kill */

	disp_kill(buffer, message);

	/* Check if we are deleting at the start of the list */

	if (buffer->messages == message) {
		buffer->messages = message->next;
	}

	/* Do the deletion */

	if ((message->next->prev = message->prev) != NULL) {
		message->prev->next = message->next;
	}
	buffer->no_msgs--;

	/* Add the message to the kill buffer or free it */

	message->prev = message->next = NULL;
	if (save) {
		add_killbuf(message, prepend);
	} else {
		free_messages(message);
	}

	return;
}
/****************************************************************************/
static void add_killbuf(message, prepend)
MESSAGE *message;
int prepend;
{
	/* Add message to the current kill buffer */

	static MESSAGE *last_kbuf;

	/* May need to append or prepend the list */

	if (killbuf->messages == NULL) {
		/* Set the kill buffer to message */

		killbuf->messages = last_kbuf = message;
	} else if (prepend) {
		/* Prepend message to the kill buffer */

		message->next = killbuf->messages;
		killbuf->messages->prev = message;
		killbuf->messages = message;
	} else {
		/* Append the list to the kill buffer */

		last_kbuf->next = message;
		message->prev = last_kbuf;
		last_kbuf = message;
	}

	return;
}
/****************************************************************************/
static MESSAGE *cp_message(message, persistent)
MESSAGE *message;
int persistent;
{
	/* Return an allocated copy of message */

	int ref;
	MESSAGE *buf;
	TAG_LIST *tlist;
	MSG_TEXT *p, *q;

	/* Allocate space for the message */

	buf = (MESSAGE *) xmalloc(sizeof(MESSAGE));

	/* Copy the basic information */

	buf->from = xstrdup(message->from);
	buf->addr = xstrdup(message->from);
	buf->subject = xstrdup(message->subject);
	buf->reply = (message->reply != NULL)
		? xstrdup(message->reply) : NULL;
	buf->group = (message->group != NULL)
		? xstrdup(message->group) : NULL;
	buf->cc = (message->cc != NULL)
		? xstrdup(message->cc) : NULL;

	/* Copy the MIME information */

	buf->contype = (message->contype != NULL)
		? xstrdup(message->contype) : NULL;
	buf->encoding = (message->encoding != NULL)
		? xstrdup(message->encoding) : NULL;

	/* Copy the date if there is one */

	if (message->date != NULL) {
		buf->date = (DATEZONE *) xmalloc(sizeof(DATEZONE));
		buf->date->d_date = message->date->d_date;
		buf->date->d_zone = message->date->d_zone;
	} else {
		buf->date = NULL;
	}

	/* Copy the references */

	for (ref = 0; ref < NO_REFERENCES; ref++) {
		buf->refs[ref] = (message->refs[ref] != NULL)
			? xstrdup(message->refs[ref]) : NULL;
	}

	/* Copy the position in the buffer and network ID */

	buf->pos = message->pos;
	buf->id = (message->id != NULL) ? xstrdup(message->id) : NULL;

#ifdef MTA_CONTENT_LENGTH
	/* Copy the message's length */

	buf->length = message->length;
#endif /* MTA_CONTENT_LENGTH */

	/* Set the message's (persistent?) tags */

	buf->user_tags = NULL;

	if (persistent) {
		tlist = taglist(message->user_tags, TL_SET);
		mask_tags(tlist);
		(void) set_tags(buf, tlist);
		free_tlist(tlist);
	} else if (message->user_tags != NULL) {
		buf->user_tags = xstrdup(message->user_tags);
	}

	/* The copied message must be visible and undeleted */

	buf->visible = TRUE;
	buf->deleted = FALSE;

	/* Copy the message's status across */

	buf->nontext = message->nontext;
	buf->bad = message->bad;
	buf->new = message->new;
	buf->read = message->read;
	buf->replied = message->replied;
	buf->forwarded = message->forwarded;
	buf->saved = message->saved;
	buf->printed = message->printed;

	/* Set the message's system tags */

	buf->sys_tags = NULL;
	set_sys_tags(buf);

	/* And finally, zero the list data */

	buf->prev = buf->next = NULL;

	/* Now copy the first line of the text */

	buf->text = (MSG_TEXT *) xmalloc(sizeof(MSG_TEXT));
	buf->text->line = xstrdup(message->text->line);
	buf->text->next = NULL;

	p = message->text;
	q = buf->text;

	/* And then copy the rest of the text */

	while (p->next != NULL) {
		p = p->next;
		q->next = (MSG_TEXT *) xmalloc(sizeof(MSG_TEXT));
		q->next->line = xstrdup(p->line);
		q->next->next = NULL;
		q = q->next;
	}

	/* All done; return the new message */

	return(buf);
}
/****************************************************************************/
