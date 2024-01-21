/* Mark_cmd.c - Mark and kill handling commands for af.
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
static char *RcsId = "$Id: mark_cmd.c,v 1.20 1996/05/06 10:11:30 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *get_cstr();
extern int chk_msg(), chk_readonly();
extern int chk_pop3(), sort_msgs(), active();
extern unsigned cmodes(), count_messages();
extern void new_killbuf(), kill_message();
extern void copy_message(), del_message();
extern void pop_killbuf(), insert();
extern void toggle_mode(), disp_narrow();
extern void msg(), emsg(), redisplay();
extern void alldisplay();
extern ARGUMENT *form_or_arg();
extern REGION *get_region();
extern MESSAGE *get_killbuf();
extern MESSAGE *find_prev(), *find_next();
extern CLIST *sort_complete();

/* Local function declarations */

static int need_new_killbuf();

/****************************************************************************/
/* Import the current window and last command executed from commands.c */

extern WINDOW *cwin;
extern COMMAND *last_command;

/****************************************************************************/
/* Was the last yank or yank-pop valid? */

static int last_yank_ok = FALSE;

/****************************************************************************/
FORM *set_mark(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Set the mark at the current line */

	/* Minibuffer and typeout have their own marks */

	if (cmodes(0) & M_MBUF) {
		return(mb_mark(seq, arg, forms));
	} else if (cmodes(0) & M_TYPEOUT) {
		return(to_mark(seq, arg, forms));
	}

	/* Couldn't really be simpler */

	cwin->mark = cwin->point;
	msg("(Mark set)");

	return(c_t());
}
/****************************************************************************/
FORM *exchange(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Exchange the point and the mark in the current window */

	MESSAGE *mark;

	/* Let the minibuffer and typeout look after themselves */

	if (cmodes(0) & M_MBUF) {
		return(mb_exchange(seq, arg, forms));
	} else if (cmodes(0) & M_TYPEOUT) {
		return(to_exchange(seq, arg, forms));
	}

	/* Check there is a mark set */

	if (cwin->mark == NULL) {
		emsg("No mark set in this window");
		return(c_errored());
	}

	/* Do the exchange */

	mark = cwin->mark;
	cwin->mark = cwin->point;
	cwin->point = mark;
	redisplay(cwin);

	/* And return success */

	return(c_t());
}
/****************************************************************************/
FORM *kill_line(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Kill the current line in the buffer */

	int nlines, backwards;
	MESSAGE *m, *next;

	/* The minibuffer has separate kill commands */

	if (cmodes(0) & M_MBUF) {
		return(mb_lkill(seq, arg, forms));
	}

	/* Check that we can kill messages */

	if (!chk_readonly(cwin) || !chk_pop3(cwin) || !chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Determine how many lines are to be killed */

	nlines = (arg != NULL) ? arg->value : 1;
	backwards = (arg != NULL && arg->negative);

	/* Set up the first message to be killed */

	if ((m = (backwards) ? find_prev(cwin->point) :
	     cwin->point) == NULL) {
		emsg("No previous message to kill");
		return(c_errored());
	}

	/* Set a new killbuf entry if required */

	if (need_new_killbuf()) {
		new_killbuf();
	}

	/* Kill the messages */

	while (nlines-- && m != NULL && m->text != NULL) {
		next = (backwards) ? find_prev(m) : find_next(m);
		kill_message(cwin->buf, m, backwards);
		m = next;
	}

	/* And update the display */

	cwin->buf->mod = TRUE;
	alldisplay(cwin->buf);
	return(c_t());
}
/****************************************************************************/
FORM *kill_region(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Kill the region in the current buffer */

	REGION *region;
	MESSAGE *start, *end;
	MESSAGE *m, *next;

	/* The minibuffer has separate kill commands */

	if (cmodes(0) & M_MBUF) {
		return(mb_rkill(seq, arg, forms));
	}

	/* Check the buffer isn't read only */

	if (!chk_readonly(cwin) || !chk_pop3(cwin)) {
		return(c_errored());
	}

	/* Get the region to kill */

	if ((region = get_region(cwin)) == NULL) {
		return(c_errored());
	}

	/* Set a new kill buffer entry if required */

	if (need_new_killbuf()) {
		new_killbuf();
	}

	/* Set the boundaries for the kill */

	start = (region->above_point) ? find_prev(region->end)
				      : region->start;
	end = (region->above_point) ? find_prev(region->start)
				    : region->end;

	/* Kill each visible message in the region */

	m = start;
	while (m != end) {
		/* Get the next message; kill this one and move on */

		next = (region->above_point) ? find_prev(m) : find_next(m);
		kill_message(cwin->buf, m, region->above_point);
		m = next;
	}

	/* And update the display */

	cwin->buf->mod = TRUE;
	alldisplay(cwin->buf);

	/* And return success */

	return(c_t());
}
/****************************************************************************/
FORM *copy_region(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Copy the region into the kill buffer as if killed */

	REGION *region;
	MESSAGE *m;

	/* The minibuffer has separate kill commands */

	if (cmodes(0) & M_MBUF) {
		return(mb_rcopy(seq, arg, forms));
	}

	/* Get the region to copy */

	if ((region = get_region(cwin)) == NULL) {
		return(c_errored());
	}

	/* Set a new kill buffer entry */

	new_killbuf();

	/* Copy each visible message in the region */

	m = region->start;
	while (m != region->end) {
		copy_message(m);
		m = find_next(m);
	}

	/* And return success */

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *nrw_region(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Narrow the current buffer to the region */

	REGION *region;
	MESSAGE *m;

	/* Get the region to narrow to */

	if ((region = get_region(cwin)) == NULL) {
		return(c_errored());
	}

	/* Deselect all messages outside the region */

	for (m = cwin->buf->messages; m != region->start; m = m->next) {
		m->visible = FALSE;
	}
	for (m = region->end; m != NULL && m->text != NULL; m = m->next) {
		m->visible = FALSE;
	}

	/* Turn on narrowed mode in the buffer */

	if (!active(cwin->buf, M_NARROW)) {
		toggle_mode(cwin->buf, M_NARROW);
	}

	/* Update the number of messages in the buffer */

	cwin->buf->no_msgs = count_messages(cwin->buf->messages, FALSE);

	/* Update the display */

	disp_narrow(cwin);
	alldisplay(cwin->buf);

	/* And return success */

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *sort_region(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Sort the current region into a semblance of order */

	char *sortname;
	REGION *region;
	
	/* Get the region to sort */

	if ((region = get_region(cwin)) == NULL) {
		return(c_errored());
	}

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Get which sort order the user wants */

	if ((sortname = get_cstr(forms, "Sort region by: ", sort_complete,
				 C_STRICT)) == NULL) {
		return(c_errored());
	}

	/* Now do the sorting */

	if (!sort_msgs(cwin, "region", sortname, region->start,
		       region->end, NULL)) {
		/* Error sorting the messages */

		return(c_errored());
	}

	/* Update the display */

	if (!active(cwin->buf, M_READONLY)) {
		cwin->buf->st_mod = TRUE;
	}
	alldisplay(cwin->buf);

	/* And return success */

	return(c_t());
}
/****************************************************************************/
FORM *yank(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Yank the kill buffer back into the buffer */

	MESSAGE *kb;

	/* The minibuffer has separate kill commands */

	if (cmodes(0) & M_MBUF) {
		return(mb_yank(seq, arg, forms));
	}

	/* Check if yanking is allowed */

	if (!chk_readonly(cwin) || !chk_pop3(cwin)) {
		last_yank_ok = FALSE;
		return(c_errored());
	}

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* An argument implies a kill buffer pop */

	if (arg != NULL && arg->value > 1) {
		pop_killbuf(arg->value - 1, arg->negative);
	}

	/* Get the kill buffer */

	if ((kb = get_killbuf()) == NULL) {
		emsg("No messages in kill buffer");
		last_yank_ok = FALSE;
		return(c_errored());
	}

	/* Do the yank and update the display */

	insert(cwin, kb);
	cwin->buf->mod = TRUE;
	alldisplay(cwin->buf);

	/* The yank succeeded */

	last_yank_ok = TRUE;
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *yank_pop(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Rotate the kill ring, making the previous entry current */

	int npops, forwards;
	MESSAGE *m, *next;

	/* Check if the buffer is read only */

	if (!chk_readonly(cwin) || !chk_pop3(cwin)) {
		last_yank_ok = FALSE;
		return(c_errored());
	}

	/* Check we have yanked something */

	if (last_command == NULL || last_command->func != yank &&
	    last_command->func != yank_pop || !last_yank_ok) {
		emsg("Last command was not a yank");
		last_yank_ok = FALSE;
		return(c_errored());
	}

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Determine how many times to pop the kill ring */

	npops = (arg != NULL) ? arg->value : 1;
	forwards = (arg != NULL && arg->negative);

	/* Delete each message in the region */

	m = cwin->mark;
	while (m != cwin->point) {
		next = m->next;
		del_message(cwin->buf, m);
		m = next;
	}

	/* Pop the kill ring and insert the resulting kill buffer */

	pop_killbuf(npops, forwards);
	insert(cwin, get_killbuf());

	/* And update the display */

	cwin->buf->mod = TRUE;
	alldisplay(cwin->buf);

	/* This yank_pop was successful */

	last_yank_ok = TRUE;
	return(c_t());
}
/****************************************************************************/
static int need_new_killbuf()
{
	/* Return TRUE if the last command wasn't a kill */

	return((last_command == NULL || last_command->func != kill_line
		&& last_command->func != kill_region));
}
/****************************************************************************/
