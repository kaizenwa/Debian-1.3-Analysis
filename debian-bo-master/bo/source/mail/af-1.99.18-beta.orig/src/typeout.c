/* Typeout.c - Typeout handling for af.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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
#include <ctype.h>
#include <regex.h>
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
static char *RcsId = "$Id: typeout.c,v 1.23 1997/04/20 10:32:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup();
extern char *vstrcat(), *strerror(), *expand();
extern char *get_dstr(), *get_dcstr(), *get_estr();
extern char *get_vtext(), *strkey(), *strchar();
extern char *utos();
extern int atoi(), get_vval(), is_blank();
extern int charlen(), tlines(), tcols();
extern unsigned cmodes();
extern void free(), free_forms(), free_mtext();
extern void msg(), msgl(), cmsg(), emsg(), emsgl();
extern void clearmsg(), display(), interactive();
extern void vtupdate(), vtredraw(), hide_cursor();
extern void type_show(), type_display();
extern FORM *exec_type_key();
extern ARGUMENT *form_or_arg();
extern CLIST *fn_complete();

/* Local function declarations */

unsigned to_lines();
void typeout();
static char *indent();
static int scroll(), search();
static void show_to_file(), init_typeout(), end_typeout();
static void set_point(), set_search(), trim_blank_lines();
static MSG_TEXT *show_text(), *format_text();
static MSG_TEXT *add_text(), *find_line();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the window, commands, and user quit flag from commands.c */

extern WINDOW *cwin;
extern COMMAND *last_command;
extern COMMAND *this_command;
extern int user_quit;

/****************************************************************************/
/* Far too many file-static variables; typeout is NOT reentrant */

static char *type_file = NULL;		/* File for typeout redirection */
static FILE *type_fp = NULL;		/* Pointer to redirection file */
static int type_error = 0;		/* Error while writing typeout */
static int type_iso = FALSE;		/* Show ISO characters in typeout? */
static MSG_TEXT *type_lines = NULL;	/* List of lines we're typing out */
static char *type_search = NULL;	/* Expression we're searching for */
static regex_t *type_expr = NULL;	/* Compiled search expression */
static int type_repeat = FALSE;		/* Repeat search in typeout? */

static int type_count = 0;		/* Count of lines in typeout */
static int type_point = 0;		/* Number of first line on screen */
static int type_mark = 0;		/* Number of current mark line */
static int type_tenths = 0;		/* New position within typeout */

static int type_needed = 0;		/* Lines needed to fill window */
static int type_more = FALSE;		/* Need to read an extra line? */
static int type_end = FALSE;		/* Read to end of typeout? */
static int type_indent = FALSE;		/* Are we indenting lines? */

static unsigned old_modes = M_NULL;	/* Modes active before typeout */
static COMMAND *old_last = NULL;	/* Last command before typeout */
static COMMAND *old_this = NULL;	/* Command which called typeout */
static int touched_to = FALSE;		/* Typeout has been used */

/****************************************************************************/
int set_typeout_file(forms, arg, output_type)
FORM *forms;
ARGUMENT *arg;
char *output_type;
{
	/* Check if typeout should be redirected to a file */

	char *prompt, *filnam;

	/* Check if the user requested file output */

	if (arg == NULL && forms == NULL) {
		return(TRUE);
	}

	/* Check this wasn't called from within typeout */

	if (type_lines != NULL) {
		/* Let the user know what's happened */

		emsg("Command attempted to use typeout while in typeout");
		return(FALSE);
	}

	/* Build the prompt */

	prompt = vstrcat("Save ", output_type, " to file: ", NULL);

	/* Get the name of the typeout redirection file */

	if ((filnam = get_dcstr(forms, prompt, TYPEOUT_FILE, fn_complete,
				C_PERMISSIVE)) == NULL) {
		free(prompt);
		return(FALSE);
	}
	free(prompt);
	type_file = expand(filnam);
	
	/* Open the redirection file */

	if ((type_fp = fopen(type_file, "a")) == NULL) {
		/* Failed to open the file; fail */

		emsgl("Can't open ", type_file, ": ",
		      strerror(errno), NULL);
		free(type_file);
		type_file = NULL;
		return(FALSE);
	}

	/* We haven't had an error so far */

	type_error = 0;

	/* Now let the user know what we're doing */

	msgl("Writing ", type_file, "...", NULL);
	return(TRUE);
}
/****************************************************************************/
void iso_typeout()
{
	/* Make the next typeout show iso 8-bit characters */

	type_iso = TRUE;
	return;
}
/****************************************************************************/
void typeout(text)
char *text;
{
	/* 
	 * Display text via typeout mode.  The overall structure
	 * of this routine is more than a little strange.  In
	 * essence, it returns, leaving typeout active when more
	 * input is needed, and expires, terminating typeout, if
	 * the user quits or exits typeout.  The user_quit flag
	 * is set on quit, and any further input will be ignored.
	 * Typeout is terminated when text is equal to NULL.
	 */

	static int type_active = FALSE;
	FORM *status;

	/* Recursive use of typeout must fail */

	if (type_active && text != NULL && !user_quit) {
		/* Let the user know what's happened */

		emsg("Command attempted to use typeout while in typeout");
		user_quit = TRUE;
		return;
	} else if (type_active || user_quit) {
		/* Ignore the typeout */

		return;
	}

	/* Let typeout to a file look after itself */

	if (type_fp != NULL) {
		show_to_file(text);
		return;
	}

	/* Initialise typeout if required */

	if (type_lines == NULL && !type_needed
	    && !type_more && text != NULL) {
		init_typeout(TRUE);
	}

	/* Now add the text to the list and display it */

	type_lines = show_text(type_lines, text);
	if (type_end) {
		trim_blank_lines();
	}

	/* Set up ready for processing commands */

	if (type_lines != NULL && !type_needed && !type_more) {
		/* Set up modes and point */

		set_point(type_point, 0);
	}

	/* Now process user input if required */

	while (type_lines != NULL && !type_needed && !type_more) {
		/* Disable typeout while the command executes */

		type_active = TRUE;

		/* Execute the command and check for quit */

		if ((status = exec_type_key()) == NULL) {
			end_typeout();
		}

		/* Now clean up and reenable typeout */

		free_forms(status);
		type_active = FALSE;
	}

	return;
}
/****************************************************************************/
void showtext(text)
char *text;
{
	/* 
	 * Display text via typeout show mode, where only the
	 * first page is displayed, and typeout then exits.
	 * Typeout is terminated when text is equal to NULL.
	 */

	/* Ignore lines after the first page */

	if (user_quit) {
		return;
	}

	/* Initialise typeout if required */

	if (type_lines == NULL && !type_needed && !type_more) {
		init_typeout(FALSE);
	}

	/* Now add the text to the list and display it */

	type_lines = show_text(type_lines, text);

	/* Now handle displaying the first page */

	if (type_lines != NULL && !type_needed && !type_more) {
		/* Display the page and end typeout */

		(void) cmodes(M_TYPEOUT | M_SHOW);
		set_point(type_point, 0);
		vtupdate(TRUE);
		hide_cursor();
		end_typeout();
	}

	return;
}
/****************************************************************************/
void table(name, entry)
char *name, *entry;
{
	/* Print name followed by a tabulated entry to typeout */

	char *key;
	int len = 0;

	/* Print the name of the table line */

	typeout(name);

	/* Calculate the length of the name */

	for (key = name; *key != '\0'; key++) {
		len += charlen(*key, len, type_iso);
	}

	/* And print the entry if non-null */

	if (entry != NULL) {
		type_indent = TRUE;
		typeout(indent(len));
		typeout(entry);
	}
	typeout("\n");

	return;
}
/****************************************************************************/
int error_in_typeout()
{
	/* Return any error encountered by the last typeout */

	return(type_error);
}
/****************************************************************************/
int to_touched()
{
	/* Return whether typeout has been used and clear the flag */

	int status = touched_to;

	/* Clear the flag and return the old value */

	touched_to = FALSE;
	return(status);
}
/****************************************************************************/
unsigned to_lines()
{
	/* Return the number of lines in the current typeout */

	return(type_count);
}
/****************************************************************************/
unsigned to_position()
{
	/* Return the position of point in the current typeout */

	return(type_point);
}
/****************************************************************************/
int to_reading()
{
	/* Return whether we are still reading typeout */

	return(!type_end);
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_scroll(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Scroll up a page, or exit if at end-of-typeout */

	int nlines, back, status;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Set up how many lines to scroll by default */

	nlines = tlines() - 2 - get_vval(V_CONTEXT);
	nlines = (nlines < 1) ? 1 : nlines;

	/* Check for any argument */

	nlines = (arg != NULL) ? arg->value : nlines;
	back = (arg != NULL && arg->negative);

	/* Now do the scrolling and check status */

	if ((status = scroll(nlines, back, FALSE)) || back || !type_end) {
		/* Return the command's status */

		return((status) ? c_t() : c_errored());
	}

	/* Exit due to end-of-typeout */

	end_typeout();
	return(NULL);
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_up(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Scroll up a page, or as far as possible */

	int nlines, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Set up how many lines to scroll by default */

	nlines = tlines() - 2 - get_vval(V_CONTEXT);
	nlines = (nlines < 1) ? 1 : nlines;

	/* Check for any argument */

	nlines = (arg != NULL) ? arg->value : nlines;
	back = (arg != NULL && arg->negative);

	/* Now do the scrolling and return status */

	return(scroll(nlines, back, TRUE) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_down(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Scroll down a page, or as far as possible */

	int nlines, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Set up how many lines to scroll by default */

	nlines = tlines() - 2 - get_vval(V_CONTEXT);
	nlines = (nlines < 1) ? 1 : nlines;

	/* Check for any argument */

	nlines = (arg != NULL) ? arg->value : nlines;
	back = (arg == NULL || !arg->negative);

	/* Now do the scrolling and return status */

	return(scroll(nlines, back, TRUE) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_next(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Scroll up a line if possible */

	int nlines, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Check for any argument */

	nlines = (arg != NULL) ? arg->value : 1;
	back = (arg != NULL && arg->negative);

	/* Now do the scrolling and return status */

	return(scroll(nlines, back, TRUE) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_prev(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Scroll down a line if possible */

	int nlines, back;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Check for any argument */

	nlines = (arg != NULL) ? arg->value : 1;
	back = (arg == NULL || !arg->negative);

	/* Now do the scrolling and return status */

	return(scroll(nlines, back, TRUE) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_start(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move to the start of typeout */

	int tenths = 0;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* With an argument moves to a percentage of the text */
	
	if (arg != NULL && !arg->negative) {
		tenths = arg->value;
	} else if (arg != NULL && arg->negative) {
		tenths = (10 - arg->value);
	}

	/* Set the mark and update point we move */

	type_mark = type_point;
	set_point(1, tenths);
	msg("(Mark set)");

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_end(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Move to the end of typeout */

	int tenths = 0;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* With an argument moves to a percentage of the text */
	
	if (arg != NULL && !arg->negative) {
		tenths = -(arg->value);
	} else if (arg != NULL && arg->negative) {
		tenths = -(10 - arg->value);
	}

	/* Set the mark and update point we move */

	type_mark = type_point;
	set_point(-1, tenths);
	msg("(Mark set)");

	return(c_t());
}
/****************************************************************************/
FORM *to_goto(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Go to a typeout line by number */

	char *deflt, *ltext;
	int point;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Get the line from the argument or interactively */

	if (arg != NULL) {
		/* Argument gives line number to go to */

		point = (arg->negative) ? -(arg->value) : arg->value;
	} else {
		/* If bound to a numeric key use that as a default */

		deflt = (isdigit(LASTKEY(seq))) ?
			strkey(LASTKEY(seq), SK_KEYSEQ) : NULL;

		/* Get the line number to go to and convert to integer */

		if ((ltext = get_estr(NULL, "Go to line: ", deflt)) == NULL) {
			return(c_errored());
		}
		point = atoi(ltext);
	}

	/* Update point and return */

	set_point(point, 0);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_redraw(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Redraw the current typeout screen */

	vtredraw();
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_mark(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Set the typeout mark at the current line */

	type_mark = type_point;
	msg("(Mark set)");
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_exchange(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Exchange point and mark in typeout */

	int old_point;

	/* Check that the mark is set */

	if (type_mark == 0) {
		emsg("No mark set in this window");
		return(c_errored());
	}

	/* Swap point and mark and redraw screen */

	old_point = type_point;
	type_point = type_mark;
	type_mark = old_point;
	type_display(find_line(type_point));

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_sfwd(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Search forward for a line matching an expresssion */

	return((search(FALSE, forms)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_sback(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Search backward for a line matching an expresssion */

	return((search(TRUE, forms)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_cursor(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Display the current position in typeout */

	msg("Line ");
	cmsg(utos(type_point));
	cmsg(" of ");
	cmsg(utos(type_count));

	/* Let the user know if we're still reading */

	if (!type_end) {
		cmsg(" read to date");
	}
	return(c_t());
}
/****************************************************************************/
static void init_typeout(clear_echo_area)
int clear_echo_area;
{
	/* Initialise the typeout position variables */

	type_lines = NULL;
	type_search = NULL;
	type_expr = NULL;
	type_count = type_tenths = 0;
	type_point = type_mark = 0;
	type_end = type_indent = FALSE;
	type_needed = tlines() - 1;
	type_more = FALSE;

	/* If we weren't interactive before, we are now */

	interactive();

	/* Save the current commands and buffer modes */

	old_last = last_command;
	old_this = this_command;
	old_modes = cmodes(0);

	/* We are now using typeout */

	(void) cmodes(M_TYPEOUT);
	touched_to = TRUE;

	/* Clear the echo area if required */

	if (clear_echo_area) {
		clearmsg();
	}
	return;
}
/****************************************************************************/
static void end_typeout()
{
	/* Clean up when typeout exits */

	WINDOW *w = cwin;

	/* Free the line list */

	free_mtext(type_lines);
	type_lines = NULL;
	type_needed = 0;
	type_more = type_iso = FALSE;

	/* Ensure that any partial line is cleared */

	free_mtext(format_text(NULL, NULL, 0));

	/* Restore the old commands and buffer modes */

	last_command = old_last;
	this_command = old_this;
	(void) cmodes(old_modes);

	/* Redraw the windows and return */

	if (w != NULL) {
		do {
			display(w);
			w = w->next;
		} while (w != cwin);
	}

	return;
}
/****************************************************************************/
static MSG_TEXT *show_text(list, text)
MSG_TEXT *list;
char *text;
{
	/* Format text and add it to the line list */

	int line = type_count + 1;
	MSG_TEXT *lines, *l;

	/* Format the text into the list of lines */

	lines = format_text(list, text);

	/* Now show the lines if required */

	for (l = lines; l != NULL; l = l->next) {
		/* Show the line if it will be visible */

		if (type_needed > 1 && type_needed <= tlines() - 1) {
			type_show(l->line, tlines() - 1 - type_needed);
		}
		type_needed -= (type_needed > 0) ? 1 : 0;

		/* Do we need to read an extra line? */

		type_more = (!type_needed && l->next == NULL
					&& is_blank(l->line));
	}

	/* Check for end-of-typeout */

	if (type_end = (text == NULL)) {
		/* Fail any current search */

		if (type_search != NULL) {
			set_search(0, FALSE, FALSE, NULL, NULL);
		}

		/* There are no more lines to read */

		type_needed = 0;
		type_more = FALSE;
		return((list == NULL) ? lines : list);
	}

	/* And check if we've matched a search expression */

	for (l = lines; l != NULL && type_expr != NULL; l = l->next) {
		/* Does this line match the expression? */

		if (!regexec(type_expr, l->line, 0, NULL, 0)) {
			set_search(line, FALSE, FALSE, NULL, NULL);
		}
		line++;
	}

	/* Return the list of lines */

	return((list == NULL) ? lines : list);
}
/****************************************************************************/
static void show_to_file(text)
char *text;
{
	/* Just write the text to the file and return */

	static char *separator = "\n";
	MSG_TEXT *lines, *l;

	/* Format the text or a separator into a list of lines */

	lines = format_text(NULL, (text != NULL) ? text : separator);

	/* Now append the lines to the file */

	for (l = lines; l != NULL && !type_error; l = l->next) {
		if (fputs(l->line, type_fp) == EOF ||
		    putc('\n', type_fp) == EOF) {
			/* Error writing the line */

			type_error = errno;
			user_quit = TRUE;
			break;
		}
	}

	/* Free the list of formatted lines */

	free_mtext(lines);

	/* Handle confirmation at end-of-typeout */

	if (text == NULL && type_error) {
		emsgl("Error writing ", type_file,
		      ": ", strerror(type_error), NULL);
	} else if (text == NULL) {
		msgl("Writing ", type_file, "... Done", NULL);
	}

	/* Clean up static variables at end-of-typeout */

	if (text == NULL) {
		(void) fclose(type_fp);
		type_fp = NULL;
		free(type_file);
		type_file = NULL;
	}

	return;
}
/****************************************************************************/
static MSG_TEXT *format_text(list, text)
MSG_TEXT *list;
char *text;
{
	/*
	 * Format text into a linked list of lines.  Partial
	 * lines are stored in a static buffer until the end
	 * of the line is read.  Returns the first line added,
	 * and updates the linked list as a side-effect.
	 */

	static char *line = NULL;
	static int col = 0;
	char *cstr, wrap, *p, *q;
	MSG_TEXT *first = NULL;

	/* Handle partial lines at end-of-typeout */

	if (text == NULL && col > 0) {
		/* Add the partial line and reset */

		first = add_text(list, first, line, col);
		list = (list == NULL) ? first : list;
		line = NULL;
		col = 0;

		return(first);
	} else if (text == NULL) {
		/* Return with no new text */

		return(NULL);
	}

	/* Allocate space for the line if needed */

	if (line == NULL) {
		line = xmalloc(tcols());
	}

	/* Now copy the text into the line */

	for (p = text; *p != '\0'; p++) {
		/* Get the printable text of the character */

		cstr = strchar(*p, col, type_iso);

		/* Add non-newline characters to the line */

		for (q = cstr; *p != '\n' && *q != '\0'; q++) {
			/* Check if we need to wrap the line */

			if (col >= tcols() - 1) {
				/* Need to mark a line-wrap */

				wrap = line[col - 1];
				line[col - 1] = '\\';

				/* Add the line and reallocate it */

				first = add_text(list, first, line, col);
				list = (list == NULL) ? first : list;
				line = xmalloc(tcols());

				/* Indent the line as required */

				(void) strcpy(line, indent(0));
				col = strlen(line);

				/* Append the wrapped character */

				line[col++] = wrap;
			}

			/* Append the character to the line */

			line[col++] = *q;
		}

		/* Handle newlines */

		if (*p == '\n') {
			/* Add the line and reallocate it */

			first = add_text(list, first, line, col);
			list = (list == NULL) ? first : list;
			line = xmalloc(tcols());

			/* We aren't indenting any more */

			type_indent = FALSE;
			col = 0;
		}
	}

	/* Return the first line added */

	return(first);
}
/****************************************************************************/
static MSG_TEXT *add_text(list, first, line, len)
MSG_TEXT *list, *first;
char *line;
int len;
{
	/* Add a line to the list of typeout lines */

	static MSG_TEXT *last = NULL;
	MSG_TEXT *node;

	/* Terminate and resize the line */

	line[len] = '\0';
	line = xrealloc(line, len + 1);

	/* Update the typeout line count */

	type_count++;

	/* Allocate and fill the new node */

	node = (MSG_TEXT *) xmalloc(sizeof(MSG_TEXT));
	node->line = line;
	node->next = NULL;

	/* Append the line to the list */

	if (list != NULL) {
		last->next = node;
	}
	last = node;

	/* Return the updated first flag */

	return((first != NULL) ? first : node);
}
/****************************************************************************/
static char *indent(namelen)
int namelen;
{
	/* Indent to the indentation column */

	static char spaces[] = "                                ";
	static char nothing[] = "";
	char *indent_str = spaces;

	/* Handle ridiculously narrow displays */

	if (strlen(spaces) > tcols() * 3 / 4) {
		indent_str += (strlen(spaces) - tcols() * 3 / 4);
	}

	/* Return enough spaces to reach the indent column */

	return((type_indent && namelen < strlen(indent_str))
	       ? indent_str + namelen : nothing);
}
/****************************************************************************/
static int scroll(nlines, back, verbose)
int nlines, back, verbose;
{
	/* Scroll the screen as required */

	int point;

	/* Check that we can actually scroll */

	if (back && type_point == 1 || !back && type_end
	    && type_point + tlines() - 3 >= type_count) {
		/* Can't scroll in this direction */

		if (verbose && back) {
			emsg("Beginning of buffer");
		} else if (verbose && !back) {
			emsg("End of buffer");
		}
		return(FALSE);
	}

	/* Calculate the new position of point */

	point = type_point + ((back) ? -nlines : nlines);
	point = (point < 1) ? 1 : point;

	/* Update point and return success */

	set_point(point, 0);
	return(TRUE);
}
/****************************************************************************/
static int search(back, forms)
int back;
FORM *forms;
{
	/* Handle searching for regular expressions */

	static char *last_expr = NULL;
	static int last_search_ok = FALSE;
	char *prompt, *expr, *errbuf;
	int line = 1, match = 0, repeating;
	int expr_flags, status;
	size_t errlen;
	regex_t *rexpr;
	MSG_TEXT *t;

	/* Form the prompt for getting the search expression */

	prompt = vstrcat("Search ", (back) ? "backward"
			 : "forward", ": ", NULL);

	/* Get the string to search for */

	if ((expr = get_dstr(forms, prompt, last_expr)) == NULL) {
		free(prompt);
		last_search_ok = FALSE;
		return(FALSE);
	}
	free(prompt);

	/* What flags should we compile the expression with? */

	expr_flags = (get_vval(V_CASEFOLD)) ? REG_ICASE : 0;

	/* Allocate space for the compiled expression */

	rexpr = (regex_t *) xmalloc(sizeof(regex_t));

	/* Compile and check the expression */

	if (status = regcomp(rexpr, expr, expr_flags)) {
		/* Getting a POSIX error message is convoluted */

		errlen = regerror(status, rexpr, NULL, 0);
		errbuf = xmalloc(errlen + 1);
		(void) regerror(status, rexpr, errbuf, errlen);
		emsgl("Invalid expression: ", errbuf, NULL);

		/* Clean up and fail */

		free(errbuf);
		regfree(rexpr);
		free(rexpr);
		last_search_ok = FALSE;
		return(FALSE);
	}

	/* Are we continuing a prior search? */

	repeating = (last_search_ok && !strcmp(expr, last_expr) &&
		     last_command != NULL && (last_command->func == se_fwd ||
					      last_command->func == se_back));

	/* Store the expression for the next default */

	if (last_expr != NULL) {
		free(last_expr);
	}
	last_expr = xstrdup(expr);
	last_search_ok = TRUE;

	/* Let the user know we're searching */

	msgl("Searching for ", expr, "...", NULL);

	/* Search the lines for a match */

	for (t = type_lines; t != NULL; t = t->next) {
		/* Check if this is our line */

		if ((back && line < type_point || !back && line > type_point)
		    && !regexec(rexpr, t->line, 0, NULL, 0)) {
			/* This line matches the expression */

			match = (back || !match) ? line : match;
		}
		line++;
	}

	/* Update point and mark after the search */

	set_search(match, back, repeating, xstrdup(expr), rexpr);

	/* Return the status of the search */

	return(match != 0 || !type_end);
}
/****************************************************************************/
static void set_point(point, tenths)
int point, tenths;
{
	/* Set up point and check if more lines are needed */

	int row = 0, last;
	MSG_TEXT *t;

	/* We need to handle pending typeout tenths */

	tenths = (type_tenths) ? type_tenths : tenths;

	/* Handle moving to a percentage of typeout */

	if (tenths && type_end) {
		point = tenths * type_count / 10 + 1;
	}
	type_tenths = (type_end) ? 0 : tenths;

	/* Convert negative points to positive */

	if (point < 0 && type_end) {
		point = type_count + point - (tlines() - 4);
		point = (point < 1) ? 1 : point;
	}

	/* Now limit the point to the possible values */

	point = (point == 0) ? 1 : point;
	type_point = (type_end && point > type_count)
		? type_count : point;

	/* Calculate the last line displayed */

	last = type_point + tlines() - 3;

	/* Check if we need to read more typeout lines */

	if (!type_end && (type_point < 0 || type_tenths)) {
		/* We need to read the rest of the input */

		type_needed = -1;
	} else if (!type_end && last > type_count) {
		/* Set the number of lines needed */

		type_needed = last - type_count + 1;

		/* Print as many lines as we can */

		for (t = find_line(type_point); t != NULL; t = t->next) {
			type_show(t->line, row++);
		}
	} else {
		/* We just need to display the lines */

		type_display(find_line(type_point));
	}
	return;
}
/****************************************************************************/
static void set_search(point, back, repeat, expr, rexpr)
int point, back, repeat;
char *expr;
regex_t *rexpr;
{
	/* Handle setting point after a search */

	if (!point && !type_end && !back) {
		/* We need to set up a search */

		type_search = expr;
		type_expr = rexpr;
		type_repeat = repeat;
		type_needed = -1;
		return;
	}

	/* Now confirm the search status */

	if (point) {
		/* Update the point and mark */

		msgl("Found ", (expr != NULL) ? expr : type_search, NULL);
		type_mark = (repeat || type_repeat)
			? type_mark : type_point;
		set_point(point, 0);
	} else {
		emsgl("No match found for ", (expr != NULL)
		      ? expr : type_search, NULL);
	}

	/* Free any stored values */

	if (type_search != NULL) {
		free(type_search);
		type_search = NULL;
		regfree(type_expr);
		free(type_expr);
		type_expr = NULL;
		type_repeat = FALSE;
	}

	/* And free any local values */

	if (expr != NULL) {
		regfree(rexpr);
		free(rexpr);
		free(expr);
	}
	return;
}
/****************************************************************************/
static void trim_blank_lines()
{
	/* Trim trailing blank lines from the typeout list */

	int line = 1, lastpos = 0;
	MSG_TEXT *t, *last = NULL;

	/* Find the last non-blank line in the list */

	for (t = type_lines; t != NULL; t = t->next) {
		last = (!is_blank(t->line)) ? t : last;
		lastpos = (!is_blank(t->line)) ? line : lastpos;
		line++;
	}

	/* Trim the lines if required */

	if (last != NULL && last->next != NULL) {
		free_mtext(last->next);
		last->next = NULL;
		type_count = lastpos;
	}
	return;
}
/****************************************************************************/
static MSG_TEXT *find_line(pos)
int pos;
{
	/* Find the line at row pos in typeout */

	int line = 1;
	MSG_TEXT *t;

	/* Loop over the lines until we reach pos */

	for (t = type_lines; t != NULL && line < pos; t = t->next) {
		line++;
	}

	/* And return the node from the list */

	return(t);
}
/****************************************************************************/
