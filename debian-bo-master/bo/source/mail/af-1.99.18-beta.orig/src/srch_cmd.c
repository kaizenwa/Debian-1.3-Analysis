/* Srch_cmd.c - Searching command handlers for af.
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
#include <regex.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "commands.h"
#include "variable.h"
#include "mode.h"
#include "tags.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: srch_cmd.c,v 1.10 1997/02/15 12:46:23 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xstrdup(), *vstrcat();
extern char *utos(), *get_dstr();
extern int chk_msg(), get_vval(), active();
extern int tagset(), set_tags(), is_blank();
extern unsigned cmodes();
extern void free(), free_tlist(), free_texpr();
extern void msgl(), emsg(), emsgl();
extern void redisplay(), alldisplay();
extern ARGUMENT *form_or_arg();
extern TAG_LIST *taglist();
extern TAG_EXPR *tagexpr();

/* Local function declarations */

static int do_esearch(), do_tsearch(), find_expr();

/****************************************************************************/
/* Import the current window and last command from commands.c */

extern WINDOW *cwin;
extern COMMAND *last_command;

/****************************************************************************/
/* The last search expressions used, for use as defaults */

static char *last_expr = NULL;
static char *last_tags = NULL;

/* And whether the last search was valid */

static int last_search_ok = FALSE;

/****************************************************************************/
FORM *se_fwd(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Search history, typeout or messages forwards */

	return((cmodes(0) & M_MBUF) ? hsch_fwd(seq, arg, forms)
	       : (cmodes(0) & M_TYPEOUT) ? to_sfwd(seq, arg, forms)
	       : (do_esearch(FALSE, forms, arg)) ? c_t() : c_errored());
}
/****************************************************************************/
FORM *se_back(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Search history, typeout or messages forwards */

	return((cmodes(0) & M_MBUF) ? hsch_back(seq, arg, forms)
	       : (cmodes(0) & M_TYPEOUT) ? to_sback(seq, arg, forms)
	       : (do_esearch(TRUE, forms, arg)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *st_fwd(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Search messages forwards for a tag expression */

	return((do_tsearch(FALSE, forms)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *st_back(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Search messages backwards for a tag expression */

	return((do_tsearch(TRUE, forms)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *se_tag(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Search messages for an expression and set tags */

	char *expr, *prompt;
	char *tags, *errbuf;
	int expr_flags, status;
	unsigned no_changed = 0;
	size_t errlen;
	regex_t rexpr;
	TAG_LIST *tlist;
	MESSAGE *m;

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		return(c_errored());
	}

	/* Convert any third form to an argument */

	if (forms != NULL) {
		arg = form_or_arg(forms->next->next, arg);
	}

	/* Get the string to search for */

	if ((expr = get_dstr(forms, (arg != NULL) ? "Header search for: "
			     : "Search for: ", last_expr)) == NULL) {
		return(c_errored());
	}
	forms = (forms != NULL) ? forms->next : NULL;

	/* What flags should we compile the expression with? */

	expr_flags = (get_vval(V_CASEFOLD)) ? REG_ICASE : 0;

	/* Compile and check the expression */

	if (status = regcomp(&rexpr, expr, expr_flags)) {
		/* Getting a POSIX error message is convoluted */

		errlen = regerror(status, &rexpr, NULL, 0);
		errbuf = xmalloc(errlen + 1);
		(void) regerror(status, &rexpr, errbuf, errlen);
		emsgl("Invalid expression: ", errbuf, NULL);

		/* Clean up and fail */

		free(errbuf);
		regfree(&rexpr);
		return(c_errored());
	}

	/* Form the prompt for getting the tags */

	prompt = vstrcat((arg != NULL) ? "Header search" : "Search",
			 " for: ", expr, " And tag with: ", NULL);

	/* Get and check the tags to set */

	tags = get_dstr(forms, prompt, DEFAULT_TAG);
	free(prompt);

	if (tags == NULL || (tlist = taglist(tags, TL_SET)) == NULL) {
		return(c_errored());
	}

	/* Store the expression for the next default */

	if (last_expr != NULL) {
		free(last_expr);
	}
	last_expr = xstrdup(expr);

	/* Let the user know we're searching */

	msgl("Searching ", (arg != NULL) ? "headers "
	     : "", "for ", expr, "...", NULL);

	/* Set the tags on any messages matching the expression */

	for (m = cwin->buf->messages; m->text != NULL; m = m->next) {
		if (m->visible && find_expr(m, &rexpr, arg == NULL)
		    && set_tags(m, tlist)) {
			no_changed++;
		}
	}
	free_tlist(tlist);
	regfree(&rexpr);

	/* Redraw the screen if required */

	if (no_changed) {
		if (!active(cwin->buf, M_READONLY)) {
			cwin->buf->st_mod = TRUE;
		}
		alldisplay(cwin->buf);
	}

	/* Let the user know what happened */

	msgl("(Tagged ", utos(no_changed), " message",
	     (no_changed != 1) ? "s)" : ")", NULL);
	return(c_t());
}
/****************************************************************************/
static int do_esearch(backward, forms, arg)
int backward;
FORM *forms;
ARGUMENT *arg;
{
	/* Handle searching for regular expressions */

	char *prompt, *expr, *errbuf;
	int repeating, headers;
	int expr_flags, status;
	size_t errlen;
	regex_t rexpr;
	MESSAGE *m;

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		last_search_ok = FALSE;
		return(FALSE);
	}

	/* Convert any second form to an argument */

	if (forms != NULL) {
		arg = form_or_arg(forms->next, arg);
	}

	/* Are we only searching the headers? */

	headers = (arg != NULL);

	/* Form the prompt for getting the search expression */

	prompt = vstrcat((headers) ? "Header search" : "Search", " ",
			 (backward) ? "backward" : "forward", ": ", NULL);
	
	/* Get the string to search for */

	if ((expr = get_dstr(forms, prompt, last_expr)) == NULL) {
		free(prompt);
		last_search_ok = FALSE;
		return(FALSE);
	}
	free(prompt);

	/* What flags should we compile the expression with? */

	expr_flags = (get_vval(V_CASEFOLD)) ? REG_ICASE : 0;

	/* Compile and check the expression */

	if (status = regcomp(&rexpr, expr, expr_flags)) {
		/* Getting a POSIX error message is convoluted */

		errlen = regerror(status, &rexpr, NULL, 0);
		errbuf = xmalloc(errlen + 1);
		(void) regerror(status, &rexpr, errbuf, errlen);
		emsgl("Invalid expression: ", errbuf, NULL);

		/* Clean up and fail */

		free(errbuf);
		regfree(&rexpr);
		last_search_ok = FALSE;
		return(FALSE);
	}

	/* Are we continuing a prior search? */

	repeating = (last_search_ok && last_command != NULL
		     && (last_command->func == se_fwd ||
			 last_command->func == se_back)
		     && !strcmp(expr, last_expr));

	/* Store the expression for the next default */

	if (last_expr != NULL) {
		free(last_expr);
	}
	last_expr = xstrdup(expr);
	last_search_ok = TRUE;

	/* Let the user know we're searching */

	msgl("Searching ", (headers) ? "headers "
	     : "", "for ", expr, "...", NULL);

	/* Set the first message to search */

	if (backward) {
		m = (repeating || cwin->point->text == NULL)
			? cwin->point->prev : cwin->point;
	} else {
		m = (repeating) ? cwin->point->next : cwin->point;
	}

	/* Search for a matching message and move point */

	while (m != NULL && m->text != NULL) {
		/* Does this message match the expression? */

		if (m->visible && find_expr(m, &rexpr, !headers)) {
			break;
		}
		m = (backward) ? m->prev : m->next;
	}
	regfree(&rexpr);

	/* If m is NULL or the null message then there's no match */

	if (m == NULL || m->text == NULL) {
		emsgl("No match for ", expr, " found ", (repeating)
		      ? (backward) ? "before" : "after"
		      : "from", " point", NULL);
		return(FALSE);
	}

	/* Update point and mark as required */

	if (!repeating) {
		cwin->mark = cwin->point;
	}
	cwin->point = m;

	/* Confirm the search and update the display */

	msgl("(Found ", expr, ")", NULL);
	redisplay(cwin);

	return(TRUE);
}
/****************************************************************************/
static int do_tsearch(backward, forms)
int backward;
FORM *forms;
{
	/* Handle searching for messages matching a tag expression */

	char *prompt, *tags;
	int repeating;
	TAG_EXPR *texpr;
	MESSAGE *m;

	/* Check there are messages in the buffer */

	if (!chk_msg(cwin, FALSE)) {
		last_search_ok = FALSE;
		return(FALSE);
	}

	/* If the last tag expression is NULL, default it */

	if (last_tags == NULL) {
		last_tags = xstrdup(DEFAULT_TAG);
	}

	/* Form the prompt for getting the search tags */

	prompt = vstrcat("Tag search ", (backward)
			 ? "backward" : "forward", ": ", NULL);

	/* Get the tags to search for */

	if ((tags = get_dstr(forms, prompt, last_tags)) == NULL) {
		last_search_ok = FALSE;
		free(prompt);
		return(FALSE);
	}
	free(prompt);

	/* Make a tag expression from the tags */

	if ((texpr = tagexpr(NULL, tags)) == NULL) {
		last_search_ok = FALSE;
		return(FALSE);
	}

	/* Are we continuing a prior search? */

	repeating = (last_search_ok && last_command != NULL
		     && (last_command->func == st_fwd ||
			 last_command->func == st_back)
		     && !strcmp(tags, last_tags));

	/* Store the expression for the next default */

	free(last_tags);
	last_tags = xstrdup(tags);

	/* This search is valid, although it may not match */

	last_search_ok = TRUE;

	/* Let the user know we're searching */

	msgl("Searching for ", tags, "...", NULL);

	/* Set the first message to search */

	if (backward) {
		m = (repeating || cwin->point->text == NULL)
			? cwin->point->prev : cwin->point;
	} else {
		m = (repeating) ? cwin->point->next : cwin->point;
	}

	/* Search for a matching message and move point */

	while (m != NULL && m->text != NULL) {
		if (m->visible && tagset(m, texpr)) {
			break;
		}
		m = (backward) ? m->prev : m->next;
	}
	free_texpr(texpr);

	/* If m is NULL or the null message then there's no match */

	if (m == NULL || m->text == NULL) {
		emsgl("No match for ", tags, " found ", (repeating)
		      ? (backward) ? "before" : "after"
		      : "from", " point", NULL);
		return(FALSE);
	}

	/* Update point and mark as required */

	if (!repeating) {
		cwin->mark = cwin->point;
	}
	cwin->point = m;

	/* Confirm the search and update the display */

	msgl("(Found ", tags, ")", NULL);
	redisplay(cwin);

	return(TRUE);
}
/****************************************************************************/
static int find_expr(message, rexpr, body)
MESSAGE *message;
regex_t *rexpr;
int body;
{
	/* Return TRUE if message matches the last regular expression */

	MSG_TEXT *t;

	/* Search for a matching line in the message or headers */

	for (t = message->text; t != NULL &&
	     (body || !is_blank(t->line)); t = t->next) {
		/* Does this line match the expression? */

		if (!regexec(rexpr, t->line, 0, NULL, 0)) {
			return(TRUE);
		}
	}

	/* No match in the message or headers */

	return(FALSE);
}
/****************************************************************************/
