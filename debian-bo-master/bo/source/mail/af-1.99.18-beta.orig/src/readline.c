/* Readline.c - Edit in a string via the message line.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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
#include "readline.h"
#include STRING_HDR

#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif /* HAVE_MEMORY_H */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: readline.c,v 1.31 1997/03/05 21:23:45 malc Exp $";
static char *ReadlineId = READLINEID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup();
extern char *vstrcat(), *strseq(), *strchar();
extern char *strcanon(), *formtext();
extern int strcasecmp(), strncasecmp(), mklower();
extern int mkupper(), mb_touched(), to_touched();
extern int tcols(), get_key(), get_vval();
extern int charlen(), atoi(), vtupdate();
extern unsigned cmodes();
extern void free(), free_seq(), free_forms();
extern void tclrline(), treturn(), tbackspace();
extern void tbeep(), emsg(), clearmsg();
extern void hide_cursor(), typeout();
extern void unget_key(), show_completions();
extern FORM *exec_mbuf_key();
extern ARGUMENT *form_or_arg();
extern COMPLETE *do_completion();
extern KEYSEQ *new_seq(), *make_seq();

#ifndef HAVE_MEMORY_H
extern char *memcpy();
#endif /* HAVE_MEMORY_H */

/* Local function declarations */

static char *find_word();
static int backlen(), scroll(), del_char();
static int del_word(), case_word(), move_hist();
static int search_hist();
static void block_insert(), move_point();
static void set_killbuf(), new_hist();
static void print_prompt(), print_line();
static void handle_quit();
static FORM *mb_error();
static KEYSEQ *read_seq();
static HISTORY *find_hist();

/****************************************************************************/
/* Import the last and current commands and user quit flag from commands.c */

extern COMMAND *last_command;
extern COMMAND *this_command;
extern int user_quit;

/****************************************************************************/
/* Far too many file-static variables; readline is NOT reentrant */

static unsigned mbuf_modes = M_NULL;	/* Modes active in minibuffer */
static char *prompt, *deflt;		/* The current prompt and default */
static char *text = NULL;		/* The current text we're editing */
static char *point = NULL;		/* The current position within text */
static int textsiz = 0;			/* Allocated size of the text */
static int textlen = 0;			/* Length of the input line */
static int offset = 0;			/* Number of chars we've scrolled */
static int startx, xpos;		/* Initial and current x position */

static char *mark = NULL;		/* The current mark within text */
static char *killbuf = NULL;		/* The minibuffer kill buffer */
static int killbuf_len = 0;		/* The length of the kill buffer */
static CLIST *(*complete)();		/* Current completion function */
static int ctype = 0;			/* Type of completion to use */
static int cstatus = 0;			/* Did the last completion fail? */
static HISTORY *cmd_hist, *curr_hist;	/* Last and active history lines */

static unsigned old_modes = M_NULL;	/* Modes active before minibuffer */
static COMMAND *old_last = NULL;	/* Last command before minibuffer */
static COMMAND *old_this = NULL;	/* Command that called minibuffer */

/****************************************************************************/
char *readline(new_prompt, new_deflt, edit_deflt,
	       new_complete, new_ctype, password)
char *new_prompt, *new_deflt, *edit_deflt;
CLIST *(*new_complete)();
int new_ctype, password;
{
	/* Get a line of text from the user and return it */

	KEYSEQ *seq;

	/* Simply call read_seq to get a key sequence */

	seq = read_seq(new_prompt, new_deflt, edit_deflt,
		       new_complete, new_ctype, password, FALSE);

	/* And return the text of the sequence */
	
	return((seq != NULL) ? seq->keys : NULL);
}
/****************************************************************************/
KEYSEQ *readseq(new_prompt, new_deflt, edit_deflt, new_complete, new_ctype)
char *new_prompt, *new_deflt, *edit_deflt;
CLIST *(*new_complete)();
int new_ctype;
{
	/* Get a binary key sequence from the user and return it */

	return(read_seq(new_prompt, new_deflt, edit_deflt,
			new_complete, new_ctype, FALSE, TRUE));
}
/****************************************************************************/
static KEYSEQ *read_seq(new_prompt, new_deflt, edit_deflt,
			new_complete, new_ctype, password, binary)
char *new_prompt, *new_deflt, *edit_deflt;
CLIST *(*new_complete)();
int new_ctype, password, binary;
{
	/*
	 * Get a key for a line we're building and handle it.
	 * We pass the string to prompt with, any default response
	 * and the function to use to handle completion, if any.
	 */

	static KEYSEQ seq;
	char *canon_prompt;
	int len;
	FORM *status;

	/* Recursive use of the minibuffer must fail */

	if (point != NULL) {
		emsg("Command attempted to use minibuffer while in minibuffer");
		return(NULL);
	}

	/* Save the current commands and buffer modes */

	old_last = last_command;
	old_this = this_command;
	old_modes = cmodes(0);

	/* Initialise text */

	textsiz = TEXTBUFSIZ;
	text = xmalloc(textsiz);
	textlen = 0;
	*text = '\0';

	/* Set up the default and completion function */

	deflt = new_deflt;
	complete = new_complete;
	ctype = new_ctype;

	/* Set up the prompt */

	prompt = (deflt == NULL) ? xstrdup(new_prompt) :
		vstrcat(new_prompt, "(default ", deflt, ") ", NULL);
	canon_prompt = xstrdup(strcanon(prompt, SK_DISPLAY));
	len = strlen(canon_prompt);
	free(prompt);

	/* Make sure the prompt won't fill the screen */

	prompt = (len < tcols() * 3 / 4) ? canon_prompt
		: canon_prompt + (len - tcols() * 3 / 4);

	/* Initialise the window position variables */

	startx = xpos = strlen(prompt);
	offset = 0;
	point = text;
	mark = NULL;

	/* Set up the current history's context */

	cmd_hist->complete = new_complete;

	/* Output the prompt */

	print_prompt();
	tclrline(tcols() - 1 - xpos);

	/* Handle a default that is to be edited */

	if (edit_deflt != NULL) {
		block_insert(edit_deflt, strlen(edit_deflt));
	}

	/* Minibuffer or typeout not touched yet */

	(void) mb_touched();
	(void) to_touched();

	/* Decide what modes we must scan keymaps in */

	mbuf_modes = M_MBUF;
	mbuf_modes |= (ctype != C_NONE) ? M_COMPLETE : M_NULL;
	mbuf_modes |= (binary) ? M_BINARY : M_NULL;
	mbuf_modes |= (password) ? M_PASSWORD : M_NULL;

	/* Make sure we're in the correct mode */

	(void) cmodes(mbuf_modes);

	/* Now accept commands until one returns NULL */

	while ((status = exec_mbuf_key()) != NULL) {
		/* Redraw the minibuffer after typeout */

		if (to_touched()) {
			(void) mb_touched();
			(void) vtupdate(TRUE);
			(void) redraw(NULL, NULL, NULL);
		}

		/* And free the status */

		free_forms(status);
	}

	/* Handle the user having quit */

	if (user_quit) {
		handle_quit();
	}

	/* Restore the old commands and buffer modes */

	last_command = old_last;
	this_command = old_this;
	(void) cmodes(old_modes);

	/* Clean up local storage */

	free(canon_prompt);
	point = NULL;

	/* Finally, return the key sequence */

	seq.keys = text;
	seq.len = textlen;
	return((text != NULL) ? &seq : NULL);
}
/****************************************************************************/
/*ARGSUSED*/
FORM *self_insert(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Insert key into the line at the current position */

	char *p;
	int key, ppos, mpos;
	int nchars, c;

	/* Get the key to insert if required */

	if (forms != NULL && forms->type == FT_STRING) {
		/* Only allowed one character in a string */

		if (forms->value.atom->len != 1) {
			return(mb_error());
		}
		key = forms->value.atom->keys[0];
	} else if (forms != NULL) {
		/* Just set the key to the form's value */

		key = atoi(formtext(forms));
	} else {
		/* Use the last key of the sequence */

		key = LASTKEY(seq);
	}
	forms = (forms != NULL) ? forms->next : NULL;

	/* Convert any second form to an argument */

	arg = form_or_arg(forms, arg);

	/* We can only insert null characters in binary mode */

	if (key == '\0' && !(mbuf_modes & M_BINARY)) {
		return(mb_error());
	}

	/* How many times will we insert the character? */

	nchars = (arg != NULL) ? (arg->negative) ? 0 : arg->value : 1;

	/* Check if we have room for the characters */

	while (textlen + nchars >= textsiz) {
		/* Reallocate text; preserving pointers */

		ppos = point - text;
		mpos = (mark != NULL) ? mark - text : NO_MARK;
		textsiz += TEXTBUFSIZ;
		text = xrealloc(text, textsiz);
		point = text + ppos;
		mark = (mpos != NO_MARK) ? text + mpos : NULL;
	}

	/* Right-shift any characters right of the insert point */

	for (p = text + textlen; p >= point; p--) {
		*(p + nchars) = *p;
	}

	/* Right-shift the mark if required */

	mark = (mark > point) ? mark + nchars : mark;

	/* Add the characters to the line and update the display */

	for (c = 0; c < nchars; c++) {
		*point++ = (char) key;
	}
	textlen += nchars;
	print_line(point - nchars, xpos, FALSE);

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *quote_char(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/*
	 * Insert the next key into the line.  If the next keys
	 * are an octal value less than 400, then they represent
	 * the octal value of the key to be inserted.
	 */

	int key, okey = 0;
	int ndigits = 1;
	KEYSEQ *newseq;
	FORM *status;

	/* If forms are supplied, just call self-insert */

	if (forms != NULL) {
		return(self_insert(seq, arg, forms));
	}

	/* Get the key to insert  */

	key = get_key();

	/* Check for an octal value to insert */

	if (ISOFDIGIT(key)) {
		/* Gather the octal key value */

		while (ISODIGIT(key) && ndigits++ < MAXOKEYLEN) {
			/* Update the value and get the next key */

			okey = okey * 8 + (key - '0');
			key = get_key();
		}

		/* Handle or ignore the last key */

		if (ISODIGIT(key)) {
			/* Add this key to the value */

			okey = okey * 8 + (key - '0');
		} else {
			/* Unget this key for later */

			unget_key(key);
		}

		/* And replace the key with the octal value */

		key = okey;
	}

	/* Add the key to the sequence */

	newseq = new_seq(seq, key);

	/* Insert the key and free the sequence */

	status = self_insert(newseq, arg, NULL);
	free_seq(newseq);
	return(status);
}
/****************************************************************************/
/*ARGSUSED*/
FORM *mb_mark(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Set the mark at point */

	mark = point;
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *mb_exchange(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Exchange point and mark */

	char *new_mark;

	/* If we have a mark this is simple */

	if (mark != NULL) {
		new_mark = point;
		move_point(mark);
		mark = new_mark;

		return(c_t());
	}

	/* No mark set in the minibuffer */

	return(mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *del_fwd(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Delete the character under the cursor */

	int nchars, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the repeat count for the delete */

	nchars = (arg != NULL) ? arg->value : 1;
	back = (arg != NULL && arg->negative);

	/* Do the deletion */

	return((del_char(nchars, back)) ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *del_back(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Delete the character before the cursor */

	int nchars, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the repeat count for the delete */

	nchars = (arg != NULL) ? arg->value : 1;
	back = (arg == NULL || !arg->negative);

	/* Do the deletion */

	return((del_char(nchars, back)) ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *fwd_kill_word(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Delete to the end of the next word */

	int nwords, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the repeat count for the kill */

	nwords = (arg != NULL) ? arg->value : 1;
	back = (arg != NULL && arg->negative);

	/* Do the kill */

	return((del_word(nwords, back)) ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *back_kill_word(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Delete to the start of the previous word */

	int nwords, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the repeat count for the kill */

	nwords = (arg != NULL) ? arg->value : 1;
	back = (arg == NULL || !arg->negative);

	/* Do the kill */

	return((del_word(nwords, back)) ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *mb_lkill(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Delete from the cursor position to the end of the line */

	/* Check we're not already at end of line */

	if (point - text >= textlen) {
		return(mb_error());
	}

	/* Set the kill buffer */

	set_killbuf(point, textlen - (point - text), FALSE);

	/* Update the mark if required */

	mark = (mark > point) ? point : mark;

	/* Do the deletion */

	textlen = (point - text);
	tclrline(tcols() - 1 - xpos);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *mb_rkill(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Delete from point to mark in the line */

	char *start, *end;

	/* Check there is a mark set not at point */

	if (mark == NULL || mark == point) {
		return(mb_error());
	}

	/* Select the region to delete */

	start = (point < mark) ? point : mark;
	end = (point < mark) ? mark : point;

	/* Add the region to the kill buffer */

	set_killbuf(start, end - start, (point > mark));

	/* Update the point and mark */

	move_point(start);
	mark = start;

	/* Left shift text after the start */

	while (end - text < textlen) {
		*start++ = *end++;
	}
	textlen -= (end - start);

	/* Update the display */

	print_line(point, xpos, TRUE);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *mb_rcopy(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Copy from point to mark into the kill buffer */

	char *start, *end;

	/* Check there is a valid mark not at point */

	if (mark == NULL || mark == point) {
		return(mb_error());
	}

	/* Select the region to copy */

	start = (point < mark) ? point : mark;
	end = (point < mark) ? mark : point;

	/* Add the region to the kill buffer */

	set_killbuf(start, end - start, (point > mark));
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *mb_yank(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Yank back the contents of the kill buffer */

	/* Check there is something in the kill buffer */

	if (killbuf == NULL) {
		return(mb_error());
	}

	/* Set the mark and do the yank */

	mark = point;
	block_insert(killbuf, killbuf_len);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *ucase_word(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Convert one or more words to upper case */

	int nwords, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the repeat count for the command */

	nwords = (arg != NULL) ? arg->value : 1;
	back = (arg != NULL && arg->negative);

	/* And do the conversion */

	return(case_word(nwords, back, mkupper, mkupper)
	       ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *dcase_word(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Convert one or more words to lower case */

	int nwords, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the repeat count for the command */

	nwords = (arg != NULL) ? arg->value : 1;
	back = (arg != NULL && arg->negative);

	/* And do the conversion */

	return(case_word(nwords, back, mklower, mklower)
	       ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *cap_word(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Capitalise one or more words */

	int nwords, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the repeat count for the command */

	nwords = (arg != NULL) ? arg->value : 1;
	back = (arg != NULL && arg->negative);

	/* And do the conversion */

	return(case_word(nwords, back, mkupper, mklower)
	       ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *transpose(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Transpose the characters either side of point */

	char *p, t, *old_point;
	int nchars, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the repeat count for the transpose */

	nchars = (arg != NULL) ? arg->value : 1;
	back = (arg != NULL && arg->negative);

	/* Check there are characters to transpose */

	if (point == text || back && point == (text + 1)
	    || textlen < 2) {
		return(mb_error());
	}

	/* Check how many characters can be transposed */

	if (back && nchars >= point - text) {
		nchars = (point - text) - 1;
	} else if (!back && point - text >= textlen) {
		nchars = 1;
	} else if (!back && nchars >= textlen - (point - text)) {
		nchars = textlen - (point - text);
	}

	/* Store and update point */

	old_point = (back || point - text >= textlen) ? point - 1 : point;
	point = (back) ? old_point - nchars : old_point + nchars;

	/* Now loop over the characters, transposing them */

	for (p = old_point; p != point; p = (back) ? p - 1 : p + 1) {
		/* Swap the characters */

		t = *p;
		*p = *(p - 1);
		*(p - 1) = t;
	}

	/* Update the display and return success */

	print_prompt();
	print_line(text + offset, startx, TRUE);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *fwd_char(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move point forward one or more characters */

	int nchars, back, maxchars;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* How many characters are we to move? */

	nchars = (arg != NULL) ? arg->value : 1;
	back = (arg != NULL && arg->negative);

	/* Check the maximum distance we can move */

	if ((maxchars = (back) ? point - text :
	     textlen - (point - text)) == 0) {
		return(mb_error());
	} else if (nchars > maxchars) {
		nchars = maxchars;
	}

	/* Do the move */

	move_point((back) ? point - nchars : point + nchars);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *back_char(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move point backward one or more characters */

	int nchars, back, maxchars;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* How many characters are we to move? */

	nchars = (arg != NULL) ? arg->value : 1;
	back = (arg == NULL || !arg->negative);

	/* Check the maximum distance we can move */

	if ((maxchars = (back) ? point - text :
	     textlen - (point - text)) == 0) {
		return(mb_error());
	} else if (nchars > maxchars) {
		nchars = maxchars;
	}

	/* Do the move */

	move_point((back) ? point - nchars : point + nchars);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *fwd_word(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move point forward one or more words */

	char *end, *old_point;
	int nwords, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the number of words to move */

	nwords = (arg != NULL) ? arg->value : 1;
	back = (arg != NULL && arg->negative);

	/* Save the old position of point */

	old_point = point;

	/* Move the point as far as possible or required */

	while (nwords-- && (end = find_word(back)) != NULL) {
		move_point(end);
	}

	/* Ok if point moved, error otherwise */

	return((point == old_point) ? mb_error() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *back_word(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move point backward one or more words */

	char *end, *old_point;
	int nwords, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the number of words to move */

	nwords = (arg != NULL) ? arg->value : 1;
	back = (arg == NULL || !arg->negative);

	/* Save the old position of point */

	old_point = point;

	/* Move the point as far as possible or required */

	while (nwords-- && (end = find_word(back)) != NULL) {
		move_point(end);
	}

	/* Ok if point moved, error otherwise */

	return((point == old_point) ? mb_error() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *start_of_line(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move the cursor to the start of the line */

	move_point(text);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *end_of_line(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move the cursor to the end of the line */

	move_point(text + textlen);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *prev_hist(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Call up the previous line in the history buffer */

	int nlines, prev;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the number of lines to move */

	nlines = (arg != NULL) ? arg->value : 1;
	prev = (arg == NULL || !arg->negative);

	/* Move to the the history line */

	return((move_hist(nlines, prev)) ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *next_hist(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Call up the next line in the history buffer */

	int nlines, prev;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Calculate the number of lines to move */

	nlines = (arg != NULL) ? arg->value : 1;
	prev = (arg != NULL && arg->negative);

	/* Move to the the history line */

	return((move_hist(nlines, prev)) ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *hist_start(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Call up the first line in the history buffer */

	HISTORY *hist = curr_hist, *prev;

	/* Find the first line */

	while ((prev = find_hist(hist, TRUE)) != NULL) {
		hist = prev;
	}

	/* Move to the new history line */

	new_hist(hist);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *hist_end(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Call up the last line in the history buffer */

	new_hist(cmd_hist);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *hsch_fwd(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Search forward through the history */

	return((search_hist(FALSE)) ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *hsch_back(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Search backward through the history */

	return((search_hist(FALSE)) ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *newline(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Add a line to the history buffer and return exit status */

	/* Null-terminate the text (just in case) */

	*(text + textlen) = '\0';

	/* Free the current history line if not reading a password */

	if (cmd_hist->text != NULL && !(mbuf_modes & M_PASSWORD)) {
		free(cmd_hist->text);
	}

	/* Resize the text line (if one was supplied) */

	text = (textlen) ? xrealloc(text, textlen + 1) : text;

	/* Now add non-null non-password strings to the history */

	if (textlen > 0 && !(mbuf_modes & M_PASSWORD)) {
		/* Add the line to the history */

		cmd_hist->text = text;
		cmd_hist->len = textlen;
		cmd_hist->complete = complete;
		cmd_hist = cmd_hist->next;
	} else if (!textlen) {
		/* Set line to the default */

		free(text);
		text = (deflt != NULL) ? xstrdup(deflt) : NULL;
		if (!(mbuf_modes & M_PASSWORD)) {
			cmd_hist->text = text;
			cmd_hist->len = (text != NULL) ? strlen(text) : 0;
			cmd_hist->complete = complete;
		}
	}

	/* Set the current history pointer back to the current line */

	curr_hist = cmd_hist;

	/* Clear the minibuffer and exit */

	clearmsg();
	return(NULL);
}
/****************************************************************************/
/*ARGSUSED*/
FORM *redraw(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Redraw the line from scratch */

	print_prompt();
	print_line(text + offset, startx, TRUE);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *clear_mb(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Clear the minibuffer */

	clearmsg();
	return(c_t());
}
/****************************************************************************/
FORM *mb_complete(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Complete up to end of line if possible */

	COMPLETE *cdata;

	/* Do the completion */

	*(text + textlen) = '\0';
	cdata = do_completion(text, complete, FALSE);

	/* Handle the tail */

	if (cdata->tail != NULL) {
		/* Append the tail to the text */

		(void) end_of_line(seq, arg, forms);
		block_insert(cdata->tail, strlen(cdata->tail));
	} else if (!cstatus && last_command != NULL &&
		   (last_command->func == mb_complete ||
		    last_command->func == mb_word_complete ||
		    last_command->func == mb_exit_complete)) {
		/* Two successive failures; show the alternatives */

		*(text + textlen) = '\0';
		show_completions(text, complete);
		return(c_errored());
	}

	/* Update the completion status flag and return status */

	cstatus = (cdata->tail != NULL || cdata->complete);
	return((cstatus) ? c_t() : mb_error());
}
/****************************************************************************/
FORM *mb_word_complete(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Complete up to end of word if possible */

	COMPLETE *cdata;

	/* Actually do the completion */

	*(text + textlen) = '\0';
	cdata = do_completion(text, complete, TRUE);

	/* Handle the tail */

	if (cdata->tail != NULL) {
		/* Append the tail to the text */

		(void) end_of_line(seq, arg, forms);
		block_insert(cdata->tail, strlen(cdata->tail));
	} else if (!cstatus && last_command != NULL &&
		   (last_command->func == mb_complete ||
		    last_command->func == mb_word_complete ||
		    last_command->func == mb_exit_complete)) {
		/* Two successive failures; show the alternatives */

		*(text + textlen) = '\0';
		show_completions(text, complete);
		return(c_errored());
	}

	/* Update the completion status flag and return status */

	cstatus = (cdata->tail != NULL || cdata->complete);
	return((cstatus) ? c_t() : mb_error());
}
/****************************************************************************/
FORM *mb_exit_complete(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Complete and return line if possible */

	COMPLETE *cdata;

	/* Check if completion is required */

	if (textlen == 0 || ctype == C_NONE || ctype == C_PERMISSIVE) {
		return(newline(seq, arg, forms));
	}

	/* Actually do the completion */

	*(text + textlen) = '\0';
	cdata = do_completion(text, complete, FALSE);

	/* Handle the tail */

	if (cdata->tail != NULL) {
		/* Append the tail to the text */

		(void) end_of_line(seq, arg, forms);
		block_insert(cdata->tail, strlen(cdata->tail));
	} else if (!cstatus && last_command != NULL &&
		   (last_command->func == mb_complete ||
		    last_command->func == mb_word_complete ||
		    last_command->func == mb_exit_complete)) {
		/* Two successive failures; show the alternatives */

		*(text + textlen) = '\0';
		show_completions(text, complete);
		return(c_errored());
	}

	/* Update the completion status flag */

	cstatus = (cdata->tail != NULL || cdata->complete);

	/* We may accept the line if it is complete */

	if (cdata->complete && (ctype == C_STRICT ||
				cdata->tail == NULL)) {
		return(newline(seq, arg, forms));
	}

	/* Otherwise return the completion status */

	return((cstatus) ? c_t() : mb_error());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *mb_list_completions(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* List the possible completions of a line */

	*(text + textlen) = '\0';
	show_completions(text, complete);
	return(c_t());
}
/****************************************************************************/
static FORM *mb_error()
{
	/* Handle an error in the minibuffer */

	tbeep();
	return(c_errored());
}
/****************************************************************************/
static void block_insert(block, len)
char *block;
int len;
{
	/* Insert block before the point */

	char *p;
	int ppos, mpos;

	/* Save the old point and mark positions */

	ppos = point - text;
	mpos = (mark != NULL) ? mark - text : NO_MARK;

	/* Check the block will fit */

	while (textlen + len >= textsiz) {
		/* Reallocate text, preserving pointers */

		textsiz += TEXTBUFSIZ;
		text = xrealloc(text, textsiz);
		point = text + ppos;
		mark = (mpos != NO_MARK) ? text + mpos : NULL;
	}

	/* Right-shift the text after the point */

	for (p = text + textlen; p >= point; p--) {
		*(p + len) = *p;
	}

	/* Update the mark if required */

	mark = (mark > point) ? mark + len : mark;

	/* Copy the block into place */

	(void) memcpy(point, block, len);
	
	/* Check for invalid null characters */

	for (p = point; !(mbuf_modes & M_BINARY)
	     && p < point + len; p++) {
		*p = (*p == '\0') ? NULL_CHAR : *p;
	}

	/* Update point and textlen */

	point += len;
	textlen += len;

	/* Update the line and return */

	print_line(text + ppos, xpos, TRUE);
	return;
}
/****************************************************************************/
static void move_point(new_point)
char *new_point;
{
	/* Move the point to the new location and update the screen */

	char *old_point;
	int len;

	/* Save the old point and update it */

	old_point = point;
	point = new_point;

	/* Handle scrolling */

	if (scroll()) {
		print_line(text + offset, startx, TRUE);
		return;
	}

	/* Move the cursor back as required */

	while (old_point > point) {
		len = backlen(xpos);
		xpos -= len;
		while (!(mbuf_modes & M_PASSWORD) && len--) {
			tbackspace();
		}
		old_point--;
	}

	/* Or forward as required */

	while (old_point < point) {
		if (!(mbuf_modes & M_PASSWORD)) {
			(void) fputs(strchar(*old_point, xpos,
					     FALSE), stdout);
		}
		xpos += charlen(*old_point++, xpos, FALSE);
	}

	return;
}
/****************************************************************************/
static int del_char(nchars, back)
int nchars, back;
{
	/* Handle deletion of characters */

	char *p;
	int ndels = nchars;

	/* Check there is a character to delete */

	if (back && point == text || !back
	    && (point - text) >= textlen) {
		return(FALSE);
	}

	/* Check how many characters can be deleted */

	if (back && ndels > point - text) {
		ndels = point - text;
	} else if (!back && nchars >= textlen - (point - text)) {
		ndels = textlen - (point - text);
	}

	/* Update point and mark if required */

	if (back) {
		move_point(point - ndels);
	}
	mark = (mark > point) ? mark - ndels : mark;

	/* Delete the characters */

	for (p = point; p - text < textlen; p++) {
		*p = *(p + ndels);
	}
	textlen -= ndels;

	/* Update the display */

	print_line(point, xpos, TRUE);
	return(TRUE);
}
/****************************************************************************/
static char *find_word(backward)
int backward;
{
	/* Return the start of the previous or next word */

	char *p;
	int step, check;
	
	/* Check there is something to find */

	if (backward && point == text || !backward
	    && point - text >= textlen) {
		return(NULL);
	}

	/* Set the details for the search */

	step = (backward) ? -1 : 1;
	check = (backward) ? -1 : 0;

	/* Find the near edge of the word */

	for (p = point; !isalnum(*(p + check)); p += step) {
		/* Check for end of line */

		if (backward && p == text || !backward
		    && p - text >= textlen) {
			return(p);
		}
	}

	/* Find the far edge of the word */

	while (isalnum(*(p + check)) &&
	       (backward && p > text || !backward && p - text < textlen)) {
		p += step;
	}

	return(p);
}
/****************************************************************************/
static int del_word(nwords, back)
int nwords, back;
{
	/* Handle killing of words */

	char *old_point, *lastword;
	char *start, *end;
	int len;

	/* Handle the case of killing 0 words */

	if (!nwords) {
		return(TRUE);
	}

	/* The killed region is bounded by point */

	old_point = point;

	/* Find the start and end of the words to be killed */

	while (nwords-- && (lastword = find_word(back)) != NULL) {
		point = lastword;
	}

	/* Calculate the start and end of the region */

	start = (back) ? point : old_point;
	end = (back) ? old_point : point;

	/* And reset the point */

	point = old_point;

	/* How long is the killed text? */

	if ((len = (end - start)) == 0) {
		return(FALSE);
	}

	/* Add the text to the kill buffer */

	set_killbuf(start, len, back);

	/* Move the point and the mark if required */

	move_point(start);
	mark = (mark > start) ? (mark > end) ? mark - len : start : mark;

	/* Left shift text after the start */

	while (end - text < textlen) {
		*start++ = *end++;
	}
	textlen -= len;

	/* Update the display */

	print_line(point, xpos, TRUE);
	return(TRUE);
}
/****************************************************************************/
static int case_word(nwords, back, first, rest)
int nwords, back;
int (*first)(), (*rest)();
{
	/* Change the case of one or more words */

	char *old_point, *far_edge, *near_edge;
	char *start, *end, *p;

	/* Store the original location of point */

	old_point = point;

	/* Case the words as required */

	while (nwords-- && (far_edge = find_word(back)) != NULL) {
		/* Find the near edge of the word */

		move_point(far_edge);
		near_edge = find_word(!back);

		/* Set up the start and end of the word */

		start = (back) ? far_edge : near_edge;
		end = (back) ? near_edge : far_edge;

		/* Change the case of the word */

		*start = first(*start);
		for (p = start + 1; p != end; p++) {
			*p = rest(*p);
		}
	}

	/* Redraw the line to display the changed words */

	if (point != old_point) {
		print_prompt();
		print_line(text + offset, startx, TRUE);
	}

	/* Return whether any words were found */

	return(!nwords || point != old_point);
}
/****************************************************************************/
static void set_killbuf(new_text, new_len, prepend)
char *new_text;
int new_len, prepend;
{
	/* Add new_text to the kill buffer */

	char *new_killbuf;

	/* Handle cleared kill buffer */

	if (killbuf == NULL || last_command == NULL ||
	    last_command->func != kill_line &&
	    last_command->func != kill_region &&
	    last_command->func != fwd_kill_word &&
	    last_command->func != back_kill_word) {
		/* Clear any exiting kill buffer */

		if (killbuf != NULL) {
			free(killbuf);
		}

		/* And set up the new kill buffer */

		killbuf = xmalloc(new_len);
		(void) memcpy(killbuf, new_text, new_len);
		killbuf_len = new_len;
		return;
	}

	/* Need to add the text to the kill buffer */

	new_killbuf = xmalloc(killbuf_len + new_len);
	if (prepend) {
		(void) memcpy(new_killbuf, new_text, new_len);
		(void) memcpy(new_killbuf + new_len, killbuf, killbuf_len);
	} else {
		(void) memcpy(new_killbuf, killbuf, killbuf_len);
		(void) memcpy(new_killbuf + killbuf_len, new_text, new_len);
	}

	/* Update the kill buffer and return */

	free(killbuf);
	killbuf = new_killbuf;
	killbuf_len += new_len;
	return;
}
/****************************************************************************/
static int move_hist(nlines, prev)
int nlines, prev;
{
	/* Move through the minibuffer history lines */

	HISTORY *new_line = curr_hist, *next;

	/* Check there is a history line to move to */

	if (find_hist(new_line, prev) == NULL) {
		return(FALSE);
	}

	/* Move as far as required or possible through the history */

	while (nlines-- && (next = find_hist(new_line, prev)) != NULL) {
		new_line = next;
	}

	/* Update the line and return success */

	new_hist(new_line);
	return(TRUE);
}
/****************************************************************************/
static void new_hist(hist)
HISTORY *hist;
{
	/* Change the history line accessed by the user */

	/* Check if the history line has changed */

	if (hist == curr_hist) {
		return;
	}

	/* Save the last line, if current */

	if (curr_hist == cmd_hist) {
		if (cmd_hist->text != NULL) {
			free(cmd_hist->text);
		}
		cmd_hist->text = xstrdup(text);
	}

	/* Clear the line */

	curr_hist = hist;
	textlen = offset = 0;
	point = text;
	mark = NULL;

	/* Update the line */

	print_prompt();
	block_insert(curr_hist->text, curr_hist->len);
	tclrline(tcols() - 1 - xpos);
	return;
}
/****************************************************************************/
static int search_hist(prev)
int prev;
{
	/* Actually do a search through the history */

	int len, casefold;
	HISTORY *hist = curr_hist;

	/* Set and check the length of the text to match */

	if ((len = (point - text)) == 0) {
		return(FALSE);
	}

	/* Is this search case-independent? */

	casefold = get_vval(V_CASEFOLD);

	/* Find the next matching history line */

	while ((hist = find_hist(hist, prev)) != NULL) {
		if (casefold && !strncasecmp(hist->text, text, len)
		    || !strncmp(hist->text, text, len)) {
			/* Found a match; set the mark if not a repeat */

			if (last_command == NULL ||
			    last_command->func != se_fwd &&
			    last_command->func != se_back) {
				mark = point;
			}				

			/* Move to the matching line and set point */

			new_hist(hist);
			move_point(text + len);
			return(TRUE);
		}
	}

	/* No match found in the history */

	return(FALSE);
}
/****************************************************************************/
static HISTORY *find_hist(hist, prev)
HISTORY *hist;
int prev;
{
	/* Find the next or previous relevant history entry */

	HISTORY *new_line = hist;

	/* Move through the possible history lines */

	while (prev && (new_line->prev != cmd_hist
			&& new_line->prev->text != NULL)
	       || !prev && new_line != cmd_hist) {
		/* Move to the history line */

		new_line = (prev) ? new_line->prev : new_line->next;

		/* Is the context of this history line correct? */

		if (new_line->complete == complete) {
			return(new_line);
		}
	}

	/* No relevant history lines found */

	return(NULL);
}
/****************************************************************************/
static void print_prompt()
{
	/* Redraw the prompt */

	hide_cursor();
	treturn();

	/* Print either the real or continuation prompt */

	if (offset == 0 || (mbuf_modes & M_PASSWORD)) {
		(void) fputs(prompt, stdout);
		xpos = startx = strlen(prompt);
	} else {
		(void) fputs(SCROLL_PROMPT, stdout);
		xpos = startx = strlen(SCROLL_PROMPT);
	}

	return;
}
/****************************************************************************/
static void print_line(pos, col, clear)
char *pos;
int col, clear;
{
	/* Redraw the current line from pos to the end */

	int len;

	/* Check if we need to scroll the line and initialise */

	if (scroll()) {
		pos = text + offset;
		col = startx;
		clear = TRUE;
	}

	/* Redraw the line, if not in password mode */

	while (col + (len = charlen(*pos, col, FALSE)) < tcols() - 1) {
		/* Check if this is the current position */

		if (pos == point) {
			xpos = col;
		}

		/* Check for end of line */

		if (pos - text >= textlen) {
			if (clear) {
				tclrline(tcols() - 1 - col);
			}
			break;
		}

		/* Print the character if not in password mode */

		if (!(mbuf_modes & M_PASSWORD)) {
			(void) fputs(strchar(*pos++, col, FALSE), stdout);
		}
		col += len;
	}

	/* Is the line longer than the space available? */
	     
	if (pos - text < textlen && !(mbuf_modes & M_PASSWORD)) {
		if (col > tcols() - 2) {
			tbackspace();
		}

		/* Pad the line and add the continuation character */

		while (col < tcols() - 2) {
			(void) putchar(' ');
			col++;
		}
		(void) putchar(CONT_CHAR);
		col++;
	}

	/* Move the cursor back to the right place */

	while (col-- > xpos && !(mbuf_modes & M_PASSWORD)) {
		tbackspace();
	}

	return;
}
/****************************************************************************/
static int scroll()
{
	/* Check if we need to scroll and calculate if so */

	char *p, *last = NULL;
	int col, scrolled = FALSE;

	/* Find the last character on the line */

	col = startx;
	for (p = text + offset; p < text + textlen; p++) {
		/* Update the column and last character */

		if ((col = col + charlen(*p, col, FALSE)) > tcols() - 2) {
			last = (p - 1);
			break;
		}
	}

	/* Handle scrolling if required */

	if (last != NULL && point > last || point < text + offset) {
		/* Set the 'needed to scroll' flag */

		scrolled = TRUE;

		/* Check if we can show the start of the line */

		col = xpos = startx = strlen(prompt);

		for (p = text; p <= point && col < tcols() - 1; p++) {
			col += charlen(*p, col, FALSE);
		}

		/* Set the scroll offset for the line */

		if (col > tcols() - 2) {
			/* Try and centre the point on the line */

			col = xpos = startx = strlen(SCROLL_PROMPT);
			p = point;

			while (p != text && col < tcols() / 2) {
				col += charlen(*--p, startx, FALSE);
			}
			offset = (p != text) ? p - text : 1;
		} else {
			offset = 0;
		}

		/* Output the prompt */

		print_prompt();
	}

	/* Return whether we scrolled the line */

	return(scrolled);
}
/****************************************************************************/
static int backlen(pos)
int pos;
{
	/* Return the length of the character before column pos */

	char *p;
	int nchars, len;

	/* Initialise for the scan */

	p = text + offset;
	nchars = startx;
	len = charlen(*p, nchars, FALSE);

	/* Search along until we reach the character */

	while (nchars + len < pos) {
		nchars += len;
		len = charlen(*++p, nchars, FALSE);
	}

	return(len);
}
/****************************************************************************/
static void handle_quit()
{
	/* Clean up the minibuffer after the user has quit */

	/* Free the text */

	free(text);
	text = NULL;

	/* And update the history pointer too */

	curr_hist = cmd_hist;
	return;
}
/****************************************************************************/
void init_history()
{
	/* Initialise the history buffer */

	HISTORY *list;
	int i;

	/* Initialise the first node in the history */

	list = (HISTORY *) xmalloc(sizeof(HISTORY));
	list->text = NULL;
	list->len = 0;
	list->complete = NULL;
	cmd_hist = curr_hist = list;

	/* Then initialise the rest of the nodes */

	for (i = 1; i < HISTSIZE; i++) {
		list->next = (HISTORY *) xmalloc(sizeof(HISTORY));
		list->next->text = NULL;
		list->next->len = 0;
		list->next->complete = NULL;
		list->next->prev = list;
		list = list->next;
	}

	/* Make the history list circular */

	list->next = cmd_hist;
	cmd_hist->prev = list;
	return;
}
/****************************************************************************/
void list_history()
{
	/* List the minibuffer history to typeout */

	HISTORY *h = cmd_hist->next;
	KEYSEQ *seq;

	/* Loop through the history printing non-null entries */

	do {
		/* Print this entry if non-null */

		if (h->text != NULL) {
			seq = make_seq(h->text, h->len);
			typeout(strseq(seq, SK_STRING));
			typeout("\n");
			free_seq(seq);
		}
		h = h->next;
	} while (!user_quit && h != cmd_hist);

	return;
}
/****************************************************************************/
