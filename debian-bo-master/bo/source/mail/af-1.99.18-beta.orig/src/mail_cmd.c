/* Mail_cmd.c - Message sending and handling commands for af.
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
#include <ctype.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "commands.h"
#include "variable.h"
#include "mode.h"
#include "tags.h"
#include "complete.h"
#include "io.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: mail_cmd.c,v 1.31 1997/03/05 21:23:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xstrdup(), *vstrcat(), *tempnam();
extern char *getenv(), *strerror(), *expand(), *formtext();
extern char *strdate(), *some_addresses(), *local_part();
extern char *get_vtext(), *get_str(), *get_cstr();
extern char *get_dstr(), *get_dcstr(), *utos();
extern int unlink(), chk_msg(), active(), get_vval();
extern int send_reply(), send_forward(), send_bounce();
extern int is_blank(), mmdf_form(), write_text();
extern int non_ascii(), listed(), confirm(); 
extern int edit_file(), tagset(), open_digest();
extern int close_pipe(), vtupdate();
extern unsigned count_messages();
extern void free(), free_messages(), free_texpr();
extern void set_sys_tags(), send_mail(), insert();
extern void msg(), msgl(), emsg(), emsgl(), cmsg();
extern void typeout(), iso_typeout(), alldisplay();
extern void show_buffer();
extern FILE *open_pipe();
extern MAILBUF *find_buffer();
extern MESSAGE *get_messages(), *get_one_message();
extern REGION *get_region();
extern TAG_EXPR *tagexpr();
extern ARGUMENT *form_or_arg();
extern CLIST *fn_complete(), *buf_complete();

/* Local function declarations */

static char *sname();
static int skip(), msg_touched(), write_mime();
static int do_reply(), do_page(), do_save();
static int do_print(), do_pipe();
static void show_typeout(), show_pager();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the current window and user quit flag from commands.c */

extern WINDOW *cwin;
extern int user_quit;

/****************************************************************************/
/*ARGSUSED*/
FORM *mail(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Send a mail message to a specified user or users */

	char *to;

	/* Get the destination of the message */

	to = (forms != NULL) ? formtext(forms) : NULL;

	/* And send the mail */

	send_mail(to, NULL);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *reply(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Reply to the current message */

	return(do_reply(FALSE) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *group_reply(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Group reply to the current message */

	return(do_reply(TRUE) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *forward(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Forward a mail message to a specified user or users */

	char *to;

	/* Check there is a message to forward */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Get the destination of the message */

	to = (forms != NULL) ? formtext(forms) : NULL;

	/* Forward the mail and update status */

	if (send_forward(to, cwin->point) &&
	    msg_touched(cwin->point, ST_FORWARDED)) {
		alldisplay(cwin->buf);
	}

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *bounce(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Bounce a mail message to a specified user or users */

	char *to;

	/* Check there is a message to bounce */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Get the destination of the mail */

	to = (forms != NULL) ? formtext(forms) : NULL;

	/* Bounce the mail and update status */

	if (send_bounce(to, cwin->point) &&
	    msg_touched(cwin->point, ST_FORWARDED)) {
		alldisplay(cwin->buf);
	}

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *open_msg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* "Open" a message - ie. display it to typeout or to a pager */

	return((do_page(get_vtext(V_PAGER), forms, arg, TRUE))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *page_msg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Send a message to the user's preferred pager */

	return((do_page(getenv(PAGER), forms, arg, FALSE)) ?
	       c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *save_msg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Save the current message to a file */

	/* Check there is a message to save */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Get a file and save the message */

	return((do_save(MS_MESSAGE, forms, arg, cwin->point,
			cwin->point->next, NULL)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *save_region(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Save the messages in the region to a file */

	REGION *region;

	/* Get the region */

	if ((region = get_region(cwin)) == NULL) {
		return(c_errored());
	}

	/* Get a file and save the messages */

	return((do_save(MS_REGION, forms, arg, region->start,
			region->end, NULL)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *save_tagset(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
	FORM *forms;
{
	/* Save the messages in a tagset to a file */

	char *prompt, *tags;
	int status;
	TAG_EXPR *texpr;

	/* Set up the prompt for the user */

	prompt = vstrcat("Save ", (arg != NULL) ? "bodies of " : "",
			 "messages tagged: ", NULL);

	/* Get the tags defining the tagset to save */

	if ((tags = get_dstr(forms, prompt, DEFAULT_TAG)) == NULL) {
		free(prompt);
		return(c_errored());
	}
	forms = (forms != NULL) ? forms->next : NULL;
	free(prompt);

	/* Convert the tags to a tag expression */

	if ((texpr = tagexpr(cwin, tags)) == NULL) {
		return(c_errored());
	}

	/* Get a file and save the messages in the tagset */

	status = do_save(MS_TAGSET, forms, arg,
			 cwin->buf->messages, NULL, texpr);
	free_texpr(texpr);
	return((status) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *print_msg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print the current message */

	/* Check there is a message to print */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Print the message */

	return((do_print(MS_MESSAGE, forms, arg, cwin->point,
			 cwin->point->next, NULL)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *print_region(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print the messages in the region */

	REGION *region;

	/* Get the region */

	if ((region = get_region(cwin)) == NULL) {
		return(c_errored());
	}

	/* Print the messages in the region */

	return((do_print(MS_REGION, forms, arg, region->start,
			 region->end, NULL)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *print_tagset(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print the messages in a tagset */

	char *prompt, *tags;
	int status;
	TAG_EXPR *texpr;

	/* Set up the prompt for the user */

	prompt = vstrcat("Print ", (arg != NULL) ? (arg->negative)
			 ? "bodies of " : "with all headers " : "",
			 "messages tagged: ", NULL);

	/* Get the tags defining the tagset to print */

	if ((tags = get_dstr(forms, prompt, DEFAULT_TAG)) == NULL) {
		free(prompt);
		return(c_errored());
	}
	forms = (forms != NULL) ? forms->next : NULL;
	free(prompt);

	/* Convert the tags to a tag expression */

	if ((texpr = tagexpr(cwin, tags)) == NULL) {
		return(c_errored());
	}

	/* Print the messages in the tagset */

	status = do_print(MS_TAGSET, forms, arg,
			  cwin->buf->messages, NULL, texpr);
	free_texpr(texpr);
	return((status) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *pipe_msg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Pipe the current message into a command */

	/* Check there is a message to pipe */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Get a command and pipe the message */

	return((do_pipe(MS_MESSAGE, forms, arg, cwin->point,
			cwin->point->next, NULL)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *pipe_region(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Pipe the messages in the region into a command */

	REGION *region;

	/* Get the region to pipe */

	if ((region = get_region(cwin)) == NULL) {
		return(c_errored());
	}

	/* Get a command and pipe the messages */

	return((do_pipe(MS_REGION, forms, arg, region->start,
			region->end, NULL)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *pipe_tagset(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Pipe the messages in a tagset into a command */

	char *prompt, *tags;
	int status;
	TAG_EXPR *texpr;

	/* Set up the prompt for the user */

	prompt = vstrcat("Pipe ", (arg != NULL) ? (arg->negative)
			 ? "bodies of " : "with all headers " : "",
			 "messages tagged: ", NULL);

	/* Get the tags defining the tagset to pipe */

	if ((tags = get_dstr(forms, prompt, DEFAULT_TAG)) == NULL) {
		free(prompt);
		return(c_errored());
	}
	forms = (forms != NULL) ? forms->next : NULL;
	free(prompt);

	/* Convert the tags to a tag expression */

	if ((texpr = tagexpr(cwin, tags)) == NULL) {
		return(c_errored());
	}

	/* Get a command and pipe the messages in the tagset */

	status = do_pipe(MS_TAGSET, forms, arg,
			 cwin->buf->messages, NULL, texpr);
	free_texpr(texpr);
	return((status) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *explode(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Explode a mail digest into many messages */

	MESSAGE *messages = NULL, *m;

	/* Check there is a current message */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Let the user know what we're doing */

	msg("Exploding digest...");

	/* Set up the digest for parsing and parse it */

	if (open_digest(cwin->point)) {
		messages = get_messages(NULL, NULL, 0L);
	}

	/* Check the message was a digest */
	
	if (messages == NULL || count_messages(messages, TRUE) < 2) {
		emsg("Message is not a valid digest");
		free_messages(messages);
		return(c_errored());
	}

	/* Remove the null message from the list */

	for (m = messages; m->next->text != NULL; m = m->next) {
		/* NULL LOOP */
	}
	free(m->next);
	m->next = NULL;

	/* Insert the messages after the digest */

	cwin->point = cwin->point->next;
	insert(cwin, messages);
	cwin->buf->mod = TRUE;

	/* And update the display */

	alldisplay(cwin->buf);
	cmsg(" Done");

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *edit_msg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Store the mail in a buffer, and then call an editor on it */

	char *tfile;
	int status;
	MESSAGE *newpoint, *oldpoint;
	WINDOW *win;
	FILE *fp;

	/* Check there is a message to edit */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Get a temporary file */

	if ((tfile = tempnam(TFILEDIR, TFILEPFX)) == NULL) {
		emsg("Can't create temporary file: Can't make unique name");
		return(c_errored());
	}

	if ((fp = fopen(tfile, "w")) == NULL) {
		emsgl("Can't open temporary file: ", strerror(errno), NULL);
		return(c_errored());
	}

	/* We need to write the file in local mail format */

#ifdef MTA_MMDF_FORMAT
	if (status = write_text(fp, cwin->point, NULL, NULL,
		(arg != NULL) ? HS_ALL : HS_NONE, TRUE)) {
#else /* ! MTA_MMDF_FORMAT */
	if (status = write_text(fp, cwin->point, NULL, NULL,
		(arg != NULL) ? HS_ALL : HS_NONE, FALSE)) {
#endif /* ! MTA_MMDF_FORMAT */
		/* Error writing the temporary file */

		emsgl("Error writing temporary file: ",
		      strerror(status), NULL);
		(void) fclose(fp);
		(void) unlink(tfile);
		(void) free(tfile);
		return(c_errored());
	}
	(void) fclose(fp);

	/* Assume that we're going to fail */

	status = FALSE;

	/* Edit the file and read back the message */

	if (edit_file(tfile)) {
		/* Let the user know what we're doing */

		(void) vtupdate(TRUE);
		msgl("Reading ", tfile, "...", NULL);

		/* Now read the file and check status */

		if ((newpoint = get_one_message(cwin->point, tfile,
						arg != NULL)) == NULL) {
			/* Error; clean up and return failure */

			(void) unlink(tfile);
			(void) free(tfile);
			return(c_errored());
		}

		/* Succeeded, update the buffer's pointers */

		if (cwin->buf->messages == cwin->point) {
			cwin->buf->messages = newpoint;
		}

		/* Update the new message's pointers */

		if ((newpoint->prev = cwin->point->prev) != NULL) {
			newpoint->prev->next = newpoint;
		}
		newpoint->next = cwin->point->next;
		newpoint->next->prev = newpoint;

		/* Remove the old message from the list */

		oldpoint = cwin->point;
		oldpoint->next = oldpoint->prev = NULL;

		/* Now update all the windows' pointers */

		win = cwin;
		do {
			/* Update the window's first, point and mark */

			if (win->first == oldpoint) {
				win->first = newpoint;
			}
			if (win->point == oldpoint) {
				win->point = newpoint;
			}
			if (win->mark == oldpoint) {
				win->mark = newpoint;
			}

			/* Move on to the next window */

			win = win->next;
		} while (win != cwin);

		/* Now free the old point */

		free_messages(oldpoint);

		/* Mark the buffer as modified */

		cwin->buf->mod = TRUE;

		/* Update status, message, and the display */

		cmsg(" Done");
		(void) msg_touched(cwin->point, ST_READ);
		alldisplay(cwin->buf);
		status = TRUE;
	}

	/* Remove the temporary file */

	(void) unlink(tfile);
	(void) free(tfile);

	/* And return status */

	return((status) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *msg_info(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Give miscellaneous info about the current message */

	char *dstr;
	unsigned lines = 0;
	MSG_TEXT *line;

	/* Check there is a current message */

	if (!chk_msg(cwin, TRUE)) {
		return(c_errored());
	}

	/* Count the number of lines in the message */

	for (line = cwin->point->text; line != NULL; line = line->next) {
		lines++;
	}

	/* Get the string representing the date the message was sent */

	dstr = strdate(cwin->point->date, get_vval(V_SHOW_LTIME));

	/* Output the basic message info */

	msgl("Sent ", dstr, ", ", utos(lines), " lines", NULL);

	/* Now append the message status */

	if (cwin->point->new) {
		cmsg(", New");
	}
	if (!cwin->point->read) {
		cmsg(", Unread");
	}
	if (cwin->point->replied) {
		cmsg(", Replied");
	}
	if (cwin->point->forwarded) {
		cmsg(", Forwarded");
	}
	if (cwin->point->saved) {
		cmsg(", Saved");
	}
	if (cwin->point->printed) {
		cmsg(", Printed");
	}

	/* Now return success */

	return(c_t());
}
/****************************************************************************/
static int do_reply(group)
int group;
{
	/* Handle replying to messages */

	char *to, *cc, *ignored;

	/* Check there is a message to reply to */

	if (!chk_msg(cwin, TRUE)) {
		return(FALSE);
	}

	/* Check we know where to reply to */

	if ((to = (group) ? cwin->point->group :
	     cwin->point->reply) == NULL) {
		emsgl("Can't reply: No ", (group) ? "group" :
		      "return", " address", NULL);
		return(FALSE);
	}

	/* Check which addresses are to be ignored */

	ignored = (group) ? get_vtext(V_ADDRESSES) : NULL;

	/* Strip any ignored addresses in the list */

	if ((to = some_addresses(to, ignored)) == NULL) {
		emsg("Can't reply: All addresses ignored");
		return(FALSE);
	}

	/* Set up any carbon-copies */

	cc = (group && get_vval(V_KEEP_CC)) ?
		some_addresses(cwin->point->cc,	ignored) : NULL;

	/* The user may want a warning if we've already replied */

	if (get_vval(V_MREP_WARN) && cwin->point->replied) {
		if (!confirm("Already replied; reply again? ", TRUE)) {
			free(to);
			return(FALSE);
		}
	}

	/* Do the actual replying and update status */

	if (send_reply(to, cc, cwin->point) &&
	    msg_touched(cwin->point, ST_REPLIED)) {
		alldisplay(cwin->buf);
	}
	free(to);

	/* It all went ok */

	return(TRUE);
}
/****************************************************************************/
static int do_page(pager, forms, arg, use_mime)
char *pager;
FORM *forms;
ARGUMENT *arg;
int use_mime;
{
	/* Handle displaying messages */

	char *defcmd = DEFPAGER;
	char *cmd, *hdrlist;
	int skip_hdrs;

	/* Check there is a message to page */

	if (!chk_msg(cwin, TRUE)) {
		return(FALSE);
	}

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Which headers are we displaying? */

	skip_hdrs = skip(arg);

	/* Get the command to use for paging */

	if (cwin->point->nontext && skip_hdrs == HS_SHOW &&
	    use_mime && (cmd = get_vtext(V_MIMEPAGER)) != NULL) {
		/* We can't skip any headers */

		skip_hdrs = HS_NONE;
	} else if ((cmd = pager) == NULL && (cmd = getenv(PAGER)) == NULL) {
		/* No variable or environment, use default */

		cmd = defcmd;
	}

	/* Which headers are we to skip? */

	hdrlist = (skip_hdrs == HS_SHOW) ? get_vtext(V_NOTDISP) : NULL;

	/* Page the message as required */

	if (!strcmp(cmd, V_USE_TYPEOUT)) {
		show_typeout(cwin->point, skip_hdrs, hdrlist);
	} else {
		show_pager(cwin->point, cmd, skip_hdrs, hdrlist);
	}

	/* Update the message status */

	if (msg_touched(cwin->point, ST_READ)) {
		alldisplay(cwin->buf);
	}

	return(TRUE);
}
/****************************************************************************/
static int do_save(stype, forms, arg, start, end, texpr)
int stype;
FORM *forms;
ARGUMENT *arg;
MESSAGE *start, *end;
TAG_EXPR *texpr;
{
	/* Handle saving of messages to a file */

	char *mcmd, *prompt, *deflt;
	char *filnam = NULL;
	int skip_hdrs, all_mime;
	int status, mmdf = FALSE;
	MESSAGE *m;
	FILE *fp = NULL;

	/* Convert any second form into an argument */

	if (forms != NULL) {
		arg = form_or_arg(forms->next, arg);
	}

	/* Which headers are we saving? */

	skip_hdrs = (arg != NULL) ? HS_ALL : HS_MBOX;

	/* What command do we use for saving MIME messages */

	mcmd = (skip_hdrs == HS_ALL) ? get_vtext(V_MIMESAVER) : NULL;

	/* Find the first message to be saved */

	while (!start->visible || texpr != NULL && !tagset(start, texpr)) {
		start = start->next;
	}

	/* Check if all messages to be saved are in MIME format */

	all_mime = (mcmd != NULL);
	for (m = start; all_mime && m->text != NULL
	     && m != end; m = m->next) {
		if (m->visible && (texpr == NULL || tagset(m, texpr))
		    && !(m->nontext)) {
			all_mime = FALSE;
		}
	}

	/* Form the prompt for the file name if required */

	if (!all_mime) {
		prompt = vstrcat("Save ", sname(stype, skip_hdrs),
				 " to file: ", NULL);

		/* Get the file to save the messages to */

		if (skip_hdrs == HS_MBOX && strcmp(start->addr, ERRUSER)) {
			/* Default the file from the first messages */

			deflt = vstrcat("+", local_part(start->addr), NULL);
			filnam = get_dcstr(forms, prompt, deflt, fn_complete,
					   C_PERMISSIVE);
			free(deflt);
		} else {
			/* No default file in this case */

			filnam = get_cstr(forms, prompt, fn_complete,
					  C_PERMISSIVE);
		}
		free(prompt);

		/* Check and expand the file name */

		if (filnam == NULL) {
			return(FALSE);
		}
		filnam = expand(filnam);

		/* Give the user a message */

		msgl("Writing ", sname(stype, skip_hdrs),
		     " to ", filnam, "...", NULL);

		/* Check the type of the target file */

		mmdf = mmdf_form(filnam);

		/* Open the file we're writing to */

		if ((fp = fopen(filnam, "a")) == NULL) {
			emsgl("Can't open ", filnam, ": ",
			      strerror(errno), NULL);
			free(filnam);
			return(FALSE);
		}
	}

	/* Now save the text of the messages */

	for (m = start; m->text != NULL && m != end; m = m->next) {
		/* Is this message one we should save? */

		if (m->visible && (texpr == NULL || tagset(m, texpr))) {
			/* Print the message and check the status */

			status = (m->nontext && mcmd != NULL)
				? write_mime(mcmd, "save", m)
				: write_text(fp, m, NULL, NULL,
					     skip_hdrs, mmdf);

			/* Check for errors printing the message */

			if (status && !(m->nontext || mcmd == NULL)) {
				/* Display the error, clean up, and fail */

				emsgl("Error writing ", filnam, ": ",
				      strerror(status), NULL);
				(void) fclose(fp);
				free(filnam);
				return(FALSE);
			} else if (status && !all_mime) {
				/* Clean up and fail */

				(void) fclose(fp);
				free(filnam);
				return(FALSE);
			} else if (status) {
				/* Just return failure */

				return(FALSE);
			}

			/* Flag the message as saved */

			(void) msg_touched(m, ST_SAVED);
		}
	}

	/* Close the file if we opened it */

	if (!all_mime) {
		(void) fclose(fp);
	}

	/* Confirm the write and show any changed tags */

	msgl("Writing ", sname(stype, skip_hdrs), " to ",
	     filnam, "... Done", NULL);
	alldisplay(cwin->buf);

	/* Free the file name and return */

	free(filnam);
	return(TRUE);
}
/****************************************************************************/
static int do_print(stype, forms, arg, start, end, texpr)
int stype;
FORM *forms;
ARGUMENT *arg;
MESSAGE *start, *end;
TAG_EXPR *texpr;
{
	/* Handle printing of messages */

	char *cmd, *mcmd, *hdrlist;
	int skip_hdrs, status;
	MESSAGE *m;
	FILE *fp;

	/* Convert any form into an argument */

	arg = form_or_arg(forms, arg);

	/* Which headers are we displaying? */

	skip_hdrs = skip(arg);

	/* Get the commands to print with */

	if ((cmd = get_vtext(V_PRINT_CMD)) == NULL) {
		emsg("Can't print: print-command variable not set");
		return(FALSE);
	}
	mcmd = get_vtext(V_MIMEPRINTER);

	/* Check we really mean to do this */

	if (get_vval(V_ASK_PRINT) && !confirm("Confirm print? ", TRUE)) {
		return(FALSE);
	}

	/* Which headers are we to print? */

	hdrlist = (skip_hdrs == HS_SHOW) ? get_vtext(V_NOTDISP) : NULL;

	/* Print a message for the user */

	msgl("Printing ", sname(stype, skip_hdrs), "...", NULL);

	/* Open a pipe to the print command */

	if ((fp = open_pipe(cmd, "w", TRUE)) == NULL) {
		(void) close_pipe(fp, TRUE, FALSE);
		emsgl("Can't start process ", cmd,
		      ": ", strerror(errno), NULL);
		return(FALSE);
	}

	/* Now print the text of the messages */

	for (m = start; m->text != NULL && m != end; m = m->next) {
		/* Is this message one we should print? */

		if (m->visible && (texpr == NULL || tagset(m, texpr))) {
			/* Print the message and save the status */

			status = (m->nontext && mcmd != NULL)
				? write_mime(mcmd, "print", m)
				: write_text(fp, m, NULL, hdrlist,
					     skip_hdrs, FALSE);

			/* Check for errors printing the message */

			if (status) {
				/* Only print an error for non-MIME */

				if (!m->nontext || mcmd == NULL) {
					emsgl("Error writing pipe: ",
					      strerror(status), NULL);
				}
				(void) close_pipe(fp, TRUE, FALSE);
				return(FALSE);
			}

			/* Flag the message as printed */

			(void) msg_touched(m, ST_PRINTED);
		}
	}

	/* Close the pipe */

	(void) close_pipe(fp, TRUE, FALSE);

	/* Confirm the print and show changed tags */

	msgl("Printing ", sname(stype, skip_hdrs), "... Done", NULL);
	alldisplay(cwin->buf);

	return(TRUE);
}
/****************************************************************************/
static int do_pipe(stype, forms, arg, start, end, texpr)
int stype;
FORM *forms;
ARGUMENT *arg;
MESSAGE *start, *end;
TAG_EXPR *texpr;
{
	/* Handle piping of messages */

	char *prompt, *cmd, *hdrlist;
	int skip_hdrs, status;
	MESSAGE *m;
	FILE *fp;

	/* Convert any second form into an argument */

	if (forms != NULL) {
		arg = form_or_arg(forms->next, arg);
	}

	/* Which headers are we displaying? */

	skip_hdrs = skip(arg);

	/* Form the prompt for the command */

	prompt = vstrcat("Pipe ", sname(stype, skip_hdrs),
			 " into command: ", NULL);

	/* Get the command to pipe into */

	if ((cmd = get_str(forms, prompt)) == NULL) {
		free(prompt);
		return(FALSE);
	}
	free(prompt);

	/* Which headers are we to print? */

	hdrlist = (skip_hdrs == HS_SHOW) ? get_vtext(V_NOTDISP) : NULL;

	/* Open a pipe to the command */

	if ((fp = open_pipe(cmd, "w", TRUE)) == NULL) {
		emsgl("Can't start process ", cmd, ": ",
		      strerror(errno), NULL);
		return(FALSE);
	}

	/* Actually write the text */

	for (m = start; m->text != NULL && m != end; m = m->next) {
		/* Is this message one we should pipe? */

		if (m->visible && (texpr == NULL || tagset(m, texpr))) {
			if (status = write_text(fp, m, NULL, hdrlist,
						skip_hdrs, FALSE)) {
				/* Error writing the pipe */

				(void) close_pipe(fp, TRUE, FALSE);
				emsgl("Error writing pipe: ",
				      strerror(status), NULL);
				return(FALSE);
			}
			(void) msg_touched(m, ST_READ);
		}
	}

	/* Close the pipe and show any changed tags */

	(void) close_pipe(fp, TRUE, TRUE);
	alldisplay(cwin->buf);

	return(TRUE);
}
/****************************************************************************/
static int write_mime(cmd, op, message)
char *cmd, *op;
MESSAGE *message;
{
	/* Handle MIME-format messages via a pipe */

	int status;
	FILE *fp;

	/* Open a pipe to the command */

	if ((fp = open_pipe(cmd, "w", TRUE)) == NULL) {
		status = errno;
		emsgl("Can't start process ", cmd, " to ", op,
		      " message: ", strerror(status), NULL);
		return(status);
	}

	/* Actually write the text */

	if (status = write_text(fp, message, NULL, NULL, HS_NONE, FALSE)) {
		(void) close_pipe(fp, TRUE, FALSE);
		emsgl("Error writing pipe: ", strerror(status), NULL);
		return(status);
	}
	(void) msg_touched(message, ST_READ);

	/* Close the pipe and show any changed tags */

	(void) close_pipe(fp, TRUE, TRUE);
	alldisplay(cwin->buf);

	return(0);
}
/****************************************************************************/
static void show_typeout(message, skip_hdrs, hdrlist)
MESSAGE *message;
int skip_hdrs;
char *hdrlist;
{
	/* Display a message via typeout */

	int found = FALSE;
	MSG_TEXT *t;

	/* Write the headers of the message to typeout */

	for (t = message->text; !user_quit && t != NULL
	     && !is_blank(t->line); t = t->next) {
		/* Display the line if required */

		if (skip_hdrs == HS_NONE || skip_hdrs == HS_SHOW
		    && !listed(t->line, hdrlist)) {
			/* We need to typeout this line */

			typeout(t->line);
			found = TRUE;
		}
	}

	/* Skip the blank line if no headers displayed */

	t = (!found && t != NULL) ? t->next : t;

	/* Handle typing-out 8-bit messages */

	if (non_ascii(message)) {
		iso_typeout();
	}

	/* Display the text of the message */

	while (!user_quit && t != NULL && t->line != NULL) {
		typeout(t->line);
		t = t->next;
	}

	/* End the typeout and return */

	typeout(NULL);
	return;
}
/****************************************************************************/
static void show_pager(message, cmd, skip_hdrs, hdrlist)
MESSAGE *message;
char *cmd, *hdrlist;
int skip_hdrs;
{
	/* Display a message via a pager */

	FILE *fp;

	/* Start up the pager if possible */

	if ((fp = open_pipe(cmd, "w", TRUE)) == NULL) {
		return;
	}

	/* Write the text of the message to the pipe */

	(void) write_text(fp, message, NULL, hdrlist, skip_hdrs, FALSE);

	/* Close the pipe and return */

	(void) close_pipe(fp, TRUE, get_vval(V_PAUSE));
	return;
}
/****************************************************************************/
static int skip(arg)
ARGUMENT *arg;
{
	/* Calculate which headers we should skip according to arg */

	return((arg != NULL) ? (arg->negative) ? HS_ALL : HS_NONE : HS_SHOW);
}
/****************************************************************************/
static char *sname(stype, skip_hdrs)
int stype, skip_hdrs;
{
	/* Return text describing a selection of messages */

	static char *msg_types[] = {
		"message", "region", "tagset"
	};
	static char *hdr_types[] = {
		"message with all headers", "region with all headers",
		"tagset with all headers"
	};
	static char *bdy_types[] = {
		"message body", "region bodies", "tagset bodies"
	};

	/* Simply return the correct entry from the arrays */

	return((skip_hdrs == HS_ALL) ? bdy_types[stype] :
		(skip_hdrs == HS_NONE) ? hdr_types[stype]
					: msg_types[stype]);
}
/****************************************************************************/
static int msg_touched(message, operation)
MESSAGE *message;
int operation;
{
	/*
	 * The message has been touched, update the status and
	 * tags according to the operation carried out.
	 * Return TRUE if the system tags were changed.
	 */

	int modified;

	/* Check for status change */

	modified = (message->new || !message->read);

	/* Update the message's new and read flags */
	
	message->new = FALSE;
	message->read = TRUE;

	/* Handle the operation-specific settings */

	switch (operation) {
	case ST_REPLIED:
		modified = (modified || !message->replied);
		message->replied = TRUE;
		break;
	case ST_FORWARDED:
		modified = (modified || !message->forwarded);
		message->forwarded = TRUE;
		break;
	case ST_SAVED:
		modified = (modified || !message->saved);
		message->saved = TRUE;
		break;
	case ST_PRINTED:
		modified = (modified || !message->printed);
		message->printed = TRUE;
		break;
	}

	/* Set the system tags and modification flag if possible */

	if (modified) {
		if (!active(cwin->buf, M_READONLY)) {
			cwin->buf->st_mod = TRUE;
		}
		set_sys_tags(message);
	}

	/* Return TRUE if message statuses have changed */

	return(modified);
}
/****************************************************************************/
