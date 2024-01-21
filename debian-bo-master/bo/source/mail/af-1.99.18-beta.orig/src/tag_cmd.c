/* Tag_cmd.c - Tag handling commands for af.
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
#include "mode.h"
#include "tags.h"
#include "complete.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: tag_cmd.c,v 1.12 1997/05/05 02:50:01 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *get_dstr(), *get_cstr();
extern int chk_msg(), chk_readonly();
extern int chk_pop3(), set_tags();
extern int tagset(), active(), sort_msgs();
extern unsigned count_messages();
extern void free(), free_texpr(), free_tlist();
extern void set_sys_tags(), new_killbuf();
extern void kill_message(), copy_message();
extern void disp_narrow(), toggle_mode(), msg();
extern void msgl(), emsg(), alldisplay();
extern MESSAGE **thread();
extern TAG_LIST *taglist();
extern TAG_EXPR *tagexpr();
extern CLIST *sort_complete();

/****************************************************************************/
/* Import the current window from commands.c */

extern WINDOW *cwin;

/****************************************************************************/
/*ARGSUSED*/
FORM *del_msg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Flag the current message as deleted */

	/* Check we can delete the message */

	if (!chk_readonly(cwin) || !chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Mark the message as deleted */

	if (!cwin->point->deleted) {
		cwin->point->deleted = TRUE;
		cwin->buf->mod = TRUE;
		set_sys_tags(cwin->point);
		alldisplay(cwin->buf);
	}

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *undel_msg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Remove any deleted flag on the current message */

	/* Check we can undelete the message */

	if (!chk_readonly(cwin) || !chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Remove any deleted flag on the message */

	if (cwin->point->deleted) {
		cwin->point->deleted = FALSE;
		cwin->buf->mod = TRUE;
		set_sys_tags(cwin->point);
		alldisplay(cwin->buf);
	}

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *tag_msg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Set tags on the current message */

	char *tags;
	TAG_LIST *tlist;

	/* Check there is a message to tag */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Get the tags to set */

	if ((tags = get_dstr(forms, "Tag message: ", DEFAULT_TAG)) == NULL) {
		return(c_errored());
	}

	/* Make a tag list from the tags */

	if ((tlist = taglist(tags, TL_SET)) == NULL) {
		return(c_errored());
	}

	/* Set the tags and redraw if required */

	if (set_tags(cwin->point, tlist)) {
		if (!active(cwin->buf, M_READONLY)) {
			cwin->buf->st_mod = TRUE;
		}
		alldisplay(cwin->buf);
	}

	/* Free space and return */

	free_tlist(tlist);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *tag_thread(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Set tags on the current thread */

	char *tags;
	int m, changed = FALSE;
	TAG_LIST *tlist;
	MESSAGE **messages;

	/* Check there is a message to thread */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Check the message has an Id to thread */

	if (cwin->point->refs[NO_REFERENCES - 1] == NULL) {
		emsg("Message has no Message-Id to thread");
		return(c_errored());
	}

	/* Get the tags to set */

	if ((tags = get_dstr(forms, "Tag thread: ",
			     DEFAULT_TAG)) == NULL) {
		return(c_errored());
	}

	/* Make a tag list from the tags */

	if ((tlist = taglist(tags, TL_SET)) == NULL) {
		return(c_errored());
	}

	/* Now get the thread from the message and tag it */

	messages = thread(cwin->point);

	for (m = 0; messages[m] != NULL; m++) {
		changed = (set_tags(messages[m], tlist) || changed);
	}

	/* Redraw the display if required */

	if (changed) {
		if (!active(cwin->buf, M_READONLY)) {
			cwin->buf->st_mod = TRUE;
		}
		alldisplay(cwin->buf);
	}

	/* Free space and return */

	free(messages);
	free_tlist(tlist);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *untag_msg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Clear tags on the current message */

	char *tags;
	TAG_LIST *tlist;

	/* Check there is a message to tag */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Get the tags to unset */

	if ((tags = get_dstr(forms, "Untag message: ",
			     DEFAULT_TAG)) == NULL) {
		return(c_errored());
	}

	/* Make a tag list from the tags */

	if ((tlist = taglist(tags, TL_UNSET)) == NULL) {
		return(c_errored());
	}

	/* Unset the tags and redraw if required */

	if (set_tags(cwin->point, tlist)) {
		if (!active(cwin->buf, M_READONLY)) {
			cwin->buf->st_mod = TRUE;
		}
		alldisplay(cwin->buf);
	}

	/* Free space and return */

	free_tlist(tlist);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *rm_tags(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Clear tags on all messages in buffer */

	char *tags;
	int changed = FALSE;
	MESSAGE *m;
	TAG_LIST *tlist;

	/* Get the tags to remove */

	if ((tags = get_dstr(forms, "Remove tags: ",
			     DEFAULT_TAG)) == NULL) {
		return(c_errored());
	}

	/* Make a tag list from the tags */

	if ((tlist = taglist(tags, TL_UNSET)) == NULL) {
		return(c_errored());
	}

	/* Loop through the visible messages removing tags */

	for (m = cwin->buf->messages; m->text != NULL; m = m->next) {
		if (m->visible && set_tags(m, tlist)) {
			changed = TRUE;
		}
	}

	/* Redraw the screen if required */

	if (changed) {
		if (!active(cwin->buf, M_READONLY)) {
			cwin->buf->st_mod = TRUE;
		}
		alldisplay(cwin->buf);
	}

	/* Free space and return */

	free_tlist(tlist);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *kill_tagset(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Kill all messages in a tagset */

	char *tags;
	TAG_EXPR *texpr;
	MESSAGE *m, *next;

	/* Check we can kill messages */

	if (!chk_readonly(cwin) || !chk_pop3(cwin)
	    || !chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Get the tags defining the tagset to kill */

	if ((tags = get_dstr(forms, "Kill messages tagged: ",
			     DEFAULT_TAG)) == NULL) {
		return(c_errored());
	}

	/* Convert the tags to a tag expression */

	if ((texpr = tagexpr(cwin, tags)) == NULL) {
		return(c_errored());
	}

	/* Set up a new kill buffer */

	new_killbuf();

	/* Kill each message in the tagset */

	m = cwin->buf->messages;
	while (m->text != NULL) {
		/* Kill the message if required and move on */

		next = m->next;
		if (m->visible && tagset(m, texpr)) {
			kill_message(cwin->buf, m, FALSE);
		}
		m = next;
	}
	free_texpr(texpr);

	/* Update the display */

	cwin->buf->mod = TRUE;
	alldisplay(cwin->buf);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *copy_tagset(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Copy a tagset to the kill buffer as if killed */

	char *tags;
	TAG_EXPR *texpr;
	MESSAGE *m;

	/* Check there are messages to copy */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Get the tags defining the tagset to copy */

	if ((tags = get_dstr(forms, "Copy messages tagged: ",
			     DEFAULT_TAG)) == NULL) {
		return(c_errored());
	}

	/* Convert the tags to a tag expression */

	if ((texpr = tagexpr(cwin, tags)) == NULL) {
		return(c_errored());
	}

	/* Set up a new kill buffer */

	new_killbuf();

	/* Copy each message in the tagset */

	for (m = cwin->buf->messages; m->text != NULL; m = m->next) {
		/* Kill the message if required */

		if (m->visible && tagset(m, texpr)) {
			copy_message(m);
		}
	}

	free_texpr(texpr);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *nrw_tagset(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Narrow the current buffer to a tagset */

	char *tags;
	TAG_EXPR *texpr;
	MESSAGE *m;

	/* Check there are messages to narrow to */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Get the tags defining the tagset to narrow to */

	if ((tags = get_dstr(forms, "Narrow to messages tagged: ",
			     DEFAULT_TAG)) == NULL) {
		return(c_errored());
	}

	/* Convert the tags to a tag expression */

	if ((texpr = tagexpr(cwin, tags)) == NULL) {
		return(c_errored());
	}

	/* Deselect all messages not matching the pattern */

	for (m = cwin->buf->messages; m->text != NULL; m = m->next) {
		m->visible = m->visible && tagset(m, texpr);
	}
	free_texpr(texpr);

	/* Turn on narrowed mode in the buffer */

	if (!active(cwin->buf, M_NARROW)) {
		toggle_mode(cwin->buf, M_NARROW);
	}

	/* Update the number of messages in the buffer */

	cwin->buf->no_msgs = count_messages(cwin->buf->messages, FALSE);

	/* Update the display */

	disp_narrow(cwin);
	alldisplay(cwin->buf);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *sort_tagset(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Sort the messages in a tagset into a semblance of order */

	char *tags, *sortname;
	TAG_EXPR *texpr;
	
	/* Get the tagset to sort */

	if ((tags = get_dstr(forms, "Sort messages tagged: ",
			     DEFAULT_TAG)) == NULL) {
		return(c_errored());
	}
	forms = (forms != NULL) ? forms->next : NULL;

	/* Convert the tags to a tag expression */

	if ((texpr = tagexpr(cwin, tags)) == NULL) {
		return(c_errored());
	}

	/* Get which sort order the user wants */

	if ((sortname = get_cstr(forms, "Sort tagset by: ", sort_complete,
				 C_STRICT)) == NULL) {
		return(c_errored());
	}

	/* Now do the sorting */

	if (!sort_msgs(cwin, "tagset", sortname,
		       cwin->buf->messages, NULL, texpr)) {
		/* Error sorting the messages */

		free_texpr(texpr);
		return(c_errored());
	}

	/* Update the display */

	if (!active(cwin->buf, M_READONLY)) {
		cwin->buf->st_mod = TRUE;
	}
	alldisplay(cwin->buf);

	/* Free the tag expression and return success */

	free_texpr(texpr);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *msg_tags(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Display the tags of the current message */

	char buf[MAX_SYS_TAGS + MAX_USER_TAGS + 1];

	/* Check there is a current message */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Check that there are tags on the message */

	if (cwin->point->sys_tags == NULL &&
		cwin->point->user_tags == NULL) {
		msg("Message is not tagged");
		return(c_t());
	}

	/* Put the system and user tags into a buffer */

	(void) strcpy(buf, (cwin->point->sys_tags != NULL) ?
				cwin->point->sys_tags : "");
	(void) strcat(buf, (cwin->point->user_tags != NULL) ?
				cwin->point->user_tags : "");

	/* Display the tags */

	msgl("Message is tagged: ", buf, NULL);
	return(c_t());
}
/****************************************************************************/
