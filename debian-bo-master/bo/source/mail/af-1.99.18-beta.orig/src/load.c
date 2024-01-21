/* Load.c - Afl file loading for af.
   Copyright (C) 1995, 1996 Malc Arnold.

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
#include "af.h"
#include "load.h"
#include "keyseq.h"
#include "functions.h"
#include "commands.h"
#include "variable.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: load.c,v 1.4 1996/10/06 17:35:48 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc();
extern char *strerror(), *utos();
extern int numeric(), esckey();
extern int mkupper();
extern void free(), free_forms();
extern void free_seq(), msgl();
extern void emsgl();
extern FORM *cons(), *atom(), *atomstr();
extern FORM *list(), *eval(), *add_form();
extern KEYSEQ *null_seq(), *make_seq();
extern KEYSEQ *new_seq(), *add_seq();

/* Local function declarations */

static char *l_strerror();
static int find_form(), read_key();
static int read_octal(), read_ctrl();
static int read_caret(), read_meta();
static FORM *read_form(), *read_list();
static FORM *read_string(), *read_char();
static FORM *read_quote(), *read_symbol();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* The depth of the current source */

static int depth = -1;

/* Static pointers to the files we're loading from */

static FILE *load_fp[LOAD_DEPTH];

/****************************************************************************/
/* The error flag for loading files */

int l_errno = LERR_NONE;

/****************************************************************************/
FORM *load(filnam)
char *filnam;
{
	/* Read a file and evaluate the commands stored in it */

	FORM *form, *last;

	/* Check this wouldn't exceed nesting depth */

	if (depth >= LOAD_DEPTH) {
		emsgl("Can't load ", filnam, ": too many load files", NULL);
		return(c_errored());
	}

	/* Initialise last in case there are no forms */

	last = c_errored();

	/* Open the file and update the load depth */

	if ((load_fp[++depth] = fopen(filnam, "r")) == NULL) {
		emsgl("Can't open ", filnam, ": ", strerror(errno), NULL);
		depth--;
		return(c_errored());
	}

	/* We haven't had an error yet */

	l_errno = LERR_NONE;

	/* Now load and evaluate each command in the file */

	while ((form = read_form(load_fp[depth])) != NULL) {
		/* Free the previous last form */

		free_forms(last);

		/* Evaluate the form and check status */

		if ((last = eval(form)) == NULL || ERRORED(last)) {
			/* Error evaluating the form */

			free_forms(form);
			break;
		}

		/* Finally, free the forms */

		free_forms(form);
	}

	/* Check if the load terminated due to an error */

	if (l_errno != LERR_NONE) {
		/* Display the error and return failure */

		emsgl("Error loading ", filnam, ": ", l_strerror(), NULL);
		free_forms(last);
		last = c_errored();
	}

	/* Finally, close the file and update load depth */

	(void) fclose(load_fp[depth--]);
	return(last);
}
/****************************************************************************/
int loading()
{
	/* Return TRUE if we're reading commands from a file */

	return(depth >= 0);
}
/****************************************************************************/
/*ARGSUSED*/
static FORM *read_form(fp)
FILE *fp;
{
	/* Read the next form from the source file */

	int start;

	/* Find and then read the next form in the file */

	switch (start = find_form(fp)) {
	case EOF:
		return(NULL);
	case LS_LIST:
		return(read_list(fp));
	case LS_QUOTE:
		return(read_quote(fp));
	case LS_STRING:
		return(read_string(fp));
	case LS_CHAR:
		return(read_char(fp));
	default:
		return(read_symbol(start, fp));
	}
	/*NOTREACHED*/
}
/****************************************************************************/
static FORM *read_list(fp)
FILE *fp;
{
	/* Read a list or form and append it to forms */

	int start;
	FORM *forms = NULL;
	FORM *subform;

	/* Now read the rest of the list from the file */

	while (TRUE) {
		/* Process the list item according to type */

		switch (start = find_form(fp)) {
		case EOF:
			l_errno = LERR_LIST;
			free_forms(forms);
			return(NULL);
		case LE_LIST:
			return(list(forms));
		case LS_LIST:
			subform = read_list(fp);
			break;
		case LS_QUOTE:
			subform = read_quote(fp);
			break;
		case LS_STRING:
			subform = read_string(fp);
			break;
		case LS_CHAR:
			subform = read_char(fp);
			break;
		default:
			subform = read_symbol(start, fp);
			break;
		}

		/* Check for an error reading the subform */

		if (subform == NULL) {
			free_forms(forms);
			return(NULL);
		}

		/* Add the subform to the list */

		forms = add_form(forms, subform);
	}
	/*NOTREACHED*/
}
/****************************************************************************/
static FORM *read_quote(fp)
FILE *fp;
{
	/* Read a quoted object and append it to forms */

	FORM *subform;

	/* First read in the quoted value */

	if ((subform = read_form(fp)) == NULL) {
		/* Error reading the form */

		l_errno = (l_errno == LERR_NONE) ? LERR_LIST : l_errno;
		return(NULL);
	}

	/* Build and return a form for the quote */

	return(cons(atomstr(QUOTE_FUNC, FT_SYMBOL), subform));
}
/****************************************************************************/
static FORM *read_string(fp)
FILE *fp;
{
	/* Read a string into a form and append it to forms */

	int key, escaped = FALSE;
	KEYSEQ *seq = NULL;

	/* Initialise the key sequence */

	seq = null_seq();

	/* Loop until we reach the end of the string */

	while ((key = read_key(fp, TRUE, &escaped)) != EOF) {
		/* Check for the end of the string */

		if (!escaped && key == LE_STRING) {
			/* At end-of-string; return the form */

			return(atom(seq, FT_STRING));
		}

		/* Add the key to the sequence */

		seq = add_seq(seq, key);
	}

	/* Reached end-of-file; error in string */

	l_errno = (l_errno == LERR_NONE) ? LERR_STRING : l_errno;
	free_seq(seq);
	return(NULL);
}
/****************************************************************************/
static FORM *read_char(fp)
FILE *fp;
{
	/* Read a character into a form and append it to forms */

	int key, escaped = FALSE;

	/* Get the character itself */

	if ((key = read_key(fp, TRUE, &escaped)) == EOF) {
		l_errno = (l_errno == LERR_NONE) ? LERR_CHAR : l_errno;
		return(NULL);
	}

	/* Build and return a form */

	return(atomstr(utos(key), FT_NUMBER));
}
/****************************************************************************/
static FORM *read_symbol(start, fp)
int start;
FILE *fp;
{
	/* Read a symbol name into a form and return the form */

	int key, escaped = FALSE;
	int seen_escape = FALSE;
	KEYSEQ *seq = NULL;

	/* Push the first character back onto the file */

	(void) ungetc(start, fp);

	/* Now get the symbol name */

	while ((key = read_key(fp, FALSE, &escaped)) != EOF) {
		/* Check if this character is part of the symbol */

		if (!escaped && (strchr(AFL_SPECIALS, key) != NULL
				 || !isascii(key) || iscntrl(key))) {
			/* We shouldn't have read the last key */

			(void) ungetc(key, fp);
			break;
		}

		/* And add the key to the sequence */

		seq = add_seq(seq, key);

		/* Update whether we've seen an escaped character */

		seen_escape = (seen_escape || escaped);
	}

	/* Check for an error in the symbol name */

	if (l_errno != LERR_NONE) {
		free_seq(seq);
		return(NULL);
	} else if (seq == NULL) {
		l_errno = LERR_SYMBOL;
		return(NULL);
	}

	/* Otherwise build and return a form */

	return(atom(seq, (!seen_escape && numeric(seq))
		    ? FT_NUMBER : FT_SYMBOL));
}
/****************************************************************************/
static int find_form(fp)
FILE *fp;
{
	/* Find the start of the next form in the file */

	int key;

	/* Skip any ignored characters */

	while ((key = getc(fp)) != EOF) {
		/* Skip the character if required */

		if (key == LS_COMMENT) {
			/* Skip the rest of the comment */

			while (key != EOF && key != LE_COMMENT) {
				key = getc(fp);
			}
		} else if (!isascii(key) || !isspace(key)) {
			/* This is the start of the form */

			return(key);
		}
	}

	/* Reached EOF, so return it */

	return(EOF);
}
/****************************************************************************/
static int read_key(fp, string, escaped)
FILE *fp;
int string, *escaped;
{
	/*
	 * Read a logical key from fp and append it to seq.  As
	 * a side effect, return whether the character read was
	 * escaped (with \) in escaped.  The string flag tells
	 * us whether to handle special escape sequences.
	 */

	int key;

	/* The character isn't escaped yet */

	*escaped = FALSE;

	/* Get the first character from the file */

	if ((key = getc(fp)) == EOF) {
		/* End-of-file; return NULL */

		return(EOF);
	}

	/* Check if the key escapes the character */

	if (key == LS_ESCAPE) {
		/* This key is escaped */

		*escaped = TRUE;

		/* Read the escaped key value */

		if ((key = getc(fp)) == EOF) {
			/* End-of-file; error */

			l_errno = LERR_ESC;
			return(EOF);
		}

		/* Check for an escaped newline */

		if (string && key == '\n') {
			return(read_key(fp, TRUE, escaped));
		}

		/* Check for an octal escape value */

		if (string && ISOFDIGIT(key)) {
			return(read_octal(fp, key));
		}

		/* Check for a caret control character */

		if (string && key == LS_CARET) {
			return(read_caret(fp));
		}

		/* Check for a control character */

		if (string && key == LS_CTRL) {
			return(read_ctrl(fp));
		}

		/* Check for a metacharacter */

		if (string && key == LS_META) {
			return(read_meta(fp));
		}

		/* Handle specially-read keys */

		key = (string) ? esckey(key) : key;
	}

	/* Return the key we read */

	return(key);
}
/****************************************************************************/
static int read_octal(fp, first)
FILE *fp;
int first;
{
	/* Read an octal value into a key */

	int key = first, okey = 0, ndigits = 0;
	
	/* Gather the octal key value */

	while (ISODIGIT(key) && ndigits++ < MAXOKEYLEN) {
		/* Update key value and get next */

		okey = okey * 8 + (key - '0');
		key = getc(fp);
	}

	/* Ignore non-digit keys */

	if (!ISODIGIT(key) && key != EOF) {
		(void) ungetc(key, fp);
	}

	/* And return the key value */

	return(okey);
}
/****************************************************************************/
static int read_caret(fp)
FILE *fp;
{
	/* Read a caret-prefixed control character */

	int key;

	/* Read the control character */

	if ((key = getc(fp)) == EOF) {
		/* Error in escape sequence */

		l_errno = LERR_ESC;
		return(EOF);
	}

	/* Convert the key to a control character */

	key = (isascii(key)) ? mkupper(key) : key;
	key = (isascii(key)) ? CTRL(key) : key;

	/* And check the key converted correctly */
			
	if (!isascii(key) || !iscntrl(key)) {
		/* Invalid control character */

		l_errno = LERR_CTRL;
		return(EOF);
	}

	/* Return the generated character */

	return(key);
}
/****************************************************************************/
static int read_ctrl(fp)
FILE *fp;
{
	/* Read a control character */

	int key, akey, meta, escaped = FALSE;

	/* Read the prefix and the control character */

	if (getc(fp) != LC_PREFIX || (key = read_key(fp,
			     TRUE, &escaped)) == EOF) {
		/* Invalid escape sequence */

		l_errno = LERR_ESC;
		return(EOF);
	}

	/* Convert the key to a control character */

	meta = (!isascii(key));
	akey = mkupper(toascii(key));
	key = (meta) ? META(CTRL(akey)) : CTRL(akey);

	/* And check the key converted correctly */
			
	if (iscntrl(akey) || !iscntrl(toascii(key))) {
		/* Invalid control character */

		l_errno = LERR_CTRL;
		return(EOF);
	}

	/* Return the generated key */

	return(key);
}
/****************************************************************************/
static int read_meta(fp)
FILE *fp;
{
	/* Read a metacharacter */

	int key, escaped = FALSE;

	/* Read the prefix and the metacharacter */

	if (getc(fp) != LC_PREFIX
	    || (key = read_key(fp, TRUE, &escaped)) == EOF
	    || !isascii(key)) {
		/* Invalid escape sequence */

		l_errno = LERR_ESC;
		return(EOF);
	}

	/* Return the key as a metacharacter */

	return(META(key));
}
/****************************************************************************/
static char *l_strerror()
{
	/* Return the text associated with an error */

	/* The list of error messages */

	static char *lerr_text[] = {
		LTEXT_NONE, LTEXT_LIST, LTEXT_STRING,
		LTEXT_CHAR, LTEXT_SYMBOL, LTEXT_ESC,
		LTEXT_CTRL
	};

	/* Return the appropriate error string */

	return(lerr_text[l_errno]);
}
/****************************************************************************/
