/* Io.c - Terminal input and output functions for af.
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
#include <ctype.h>
#include <fcntl.h>
#include <varargs.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "complete.h"
#include "io.h"
#include STRING_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: io.c,v 1.39 1997/03/05 21:23:45 malc Exp $";
static char *IoId = IOID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xstrdup(), *vstrcat();
extern char *strkey(), *formtext();
extern char *readline();
extern int strcasecmp(), get_vval();
extern int mklower(), tlines();
extern int tcols(), vtupdate();
extern void free(), typeout(), tbeep();
extern void tclear(), tclrline();
extern void tmove(), treturn(), panic();
extern time_t time();
extern KEYSEQ *readseq();

/* Local function declarations */

char *get_str();
void msg(), clearmsg(), hide_cursor();
static char *get_form();
static int readkey(), readahead();
static void new_msg(), show_msg(), end_msg();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the user quit flag from commands.c */

extern int user_quit;

/****************************************************************************/
/* The input functions are stored here as pointers */

static IOFUNC io_default = { readkey, readahead, readline, readseq };
static IOFUNC *io_funcs = &io_default;

/****************************************************************************/
/* The postprocess function is stored here too */

static int (*postprocess_key)() = NULL;

/****************************************************************************/
/* This static flags are set when in interactive or scripting modes */

static int fullscreen = FALSE;
static int scripting = FALSE;

/****************************************************************************/
/* This flag specifies that errors are to be sent to typeout */

static int type_errors = FALSE;

/****************************************************************************/
/* We store the message length here */

static int msg_len = 0;

/****************************************************************************/
/* Any typeahead character is stored here */

static int type_buf = EOF;

/****************************************************************************/
/* This array stores the last LOSSAGE_SIZE characters typed */

static char lossage[LOSSAGE_SIZE];
static char *lossage_start = lossage;
static char *lossage_end = lossage;

/****************************************************************************/
/* This flag indicates that the minibuffer has been touched */

static int touched_mb = FALSE;

/****************************************************************************/
void interactive()
{
	/* Put af into interactive mode */

	if (!fullscreen) {
		fullscreen = TRUE;
		tclear();
	}
	return;
}
/****************************************************************************/
void silent()
{
	/* Put af into scripting mode */

	scripting = TRUE;
	return;
}
/****************************************************************************/
int mb_touched()
{
	/* Report if the minibuffer has been modifed and clear the flag */

	int status = touched_mb;
	touched_mb = FALSE;
	return(status);
}
/****************************************************************************/
int mb_cleared()
{
	/* Report if the minibuffer has been cleared */

	return(msg_len == 0);
}
/****************************************************************************/
void errors_to_typeout(value)
int value;
{
	/* Determine whether errors are to be sent to typeout */

	int old_value = type_errors;

	/* Reset the typeout errors flag */

	type_errors = value;

	/* And end any typeout if required */

	if (old_value && !value) {
		typeout(NULL);
	}
	return;
}
/****************************************************************************/
IOFUNC *set_input(io_new)
IOFUNC *io_new;
{
	/*
	 * Set the input function pointers to use the
	 * functions specified in io_new.  If any pointer
	 * is NULL then it is set with the current function.
	 */

	IOFUNC *io_old = io_funcs;

	/* Default any null pointers within the structure */

	io_new->key = (io_new->key != NULL) ? io_new->key : readkey;
	io_new->type = (io_new->type != NULL) ? io_new->type : readahead;
	io_new->line = (io_new->line != NULL) ? io_new->line : readline;
	io_new->seq = (io_new->seq != NULL) ? io_new->seq : readseq;

	/* Set the current input and return the old */

	io_funcs = io_new;
	return(io_old);
}
/****************************************************************************/
void set_postprocess(postfunc)
int (*postfunc)();
{
	/* Set or clear the postprocess key function */

	postprocess_key = postfunc;
	return;
}
/****************************************************************************/
int get_key()
{
	/* Return a key from typeahead or the current key input pointer */

	int key;

	/* Get the key as required and clear any typeahead */

	key = (type_buf != EOF) ? type_buf : io_funcs->key(0);
	type_buf = EOF;

	/* Now return the key */

	return(key);
}
/****************************************************************************/
int get_tkey(delay)
unsigned delay;
{
	/* Return a key with a possible timeout */

	int key;

	/* Get the key as required and clear any typeahead */

	key = (type_buf != EOF) ? type_buf : io_funcs->key(delay);
	type_buf = EOF;

	/* Now return the key */

	return(key);
}
/****************************************************************************/
void unget_key(c)
char c;
{
	/*
	 * Push key c back onto the input stream.  This routine
	 * only works for pushing one character between inputs,
	 * but that's enough for af.
	 */

	type_buf = c;
	return;
}
/****************************************************************************/
int keys_pending()
{
	/* Return TRUE if there are input characters pending */

	return(io_funcs->type());
}
/****************************************************************************/
int confirm(prompt, quit)
char *prompt;
int quit;
{
	/* Get a yes or no response and return TRUE if it's yes */

	int qchar, c;

	/* Get the user's quit character */

	qchar = mklower(get_vval(V_QUITCHAR));

	/* Force the screen to be up to date */

	if (fullscreen) {
		(void) vtupdate(TRUE);
	}

	/* Output the prompt and options */

	msg(prompt);

	/* Get a key from the user */

	c = get_key();

	/* Check we have a valid key */

	while (c != qchar && mklower(c) != 'y' && mklower(c) != 'n') {
		/* Invalid key; beep and try again */

		tbeep();
		c = get_key();
	}

	/* This may set the user quit flag */

	user_quit = (c == qchar);

	/* Output a message if quit set */

	if (user_quit || quit && mklower(c) != 'y') {
		tbeep();
		msg("Quit");
	} else {
		clearmsg();
	}

	return(mklower(c) == 'y');
}
/****************************************************************************/
int long_confirm(prompt, quit)
char *prompt;
int quit;
{
	/* Get a yes or no response and return TRUE if it's yes */

	char *full_prompt, *s;
	int ok;

	/* Force the screen to be up to date */

	if (fullscreen) {
		(void) vtupdate(TRUE);
	}

	/* Add the options to the prompt */

	full_prompt = vstrcat(prompt, "(yes or no) ", NULL);

	/* Get the response */

	do {
		s = get_str(NULL, full_prompt);
	} while (!user_quit && (s == NULL || strcasecmp(s, "yes")
				&& strcasecmp(s, "no")));

	/* Have we responded yes? */

	ok = (!user_quit && !strcasecmp(s, "yes"));

	/* Output a message if quit set */

	if (user_quit || quit && !ok) {
		tbeep();
		msg("Quit");
	} else {
		clearmsg();
	}

	/* Free the prompt and return status */

	free(full_prompt);
	return(ok);
}
/****************************************************************************/
char *get_str(form, prompt)
FORM *form;
char *prompt;
{
	/* Get a string from a form or the get_a_line interface */

	if (form == NULL && fullscreen) {
		tmove(0, tlines() - 1);
	}
	return((form != NULL) ? get_form(form) :
	       io_funcs->line(prompt, NULL, NULL, NULL, C_NONE, FALSE));
}
/****************************************************************************/
char *get_cstr(form, prompt, complete, ctype)
FORM *form;
char *prompt;
CLIST *(*complete)();
int ctype;
{
        /* Get a string with completion active */

	if (fullscreen) {
		tmove(0, tlines() - 1);
	}
        return((form != NULL) ? get_form(form) :
	       io_funcs->line(prompt, NULL, NULL, complete, ctype, FALSE));
}
/****************************************************************************/
char *get_dstr(form, prompt, deflt)
FORM *form;
char *prompt, *deflt;
{
        /* Get a string with a default */

	if (fullscreen) {
		tmove(0, tlines() - 1);
	}
        return((form != NULL) ? get_form(form) :
	       io_funcs->line(prompt, deflt, NULL, NULL, C_NONE, FALSE));
}
/****************************************************************************/
char *get_estr(form, prompt, deflt)
FORM *form;
char *prompt, *deflt;
{
        /* Get a string with a default to edit */

	if (fullscreen) {
		tmove(0, tlines() - 1);
	}
        return((form != NULL) ? get_form(form) :
	       io_funcs->line(prompt, NULL, deflt, NULL, C_NONE, FALSE));
}
/****************************************************************************/
char *get_dcstr(form, prompt, deflt, complete, ctype)
FORM *form;
char *prompt, *deflt;
CLIST *(*complete)();
int ctype;
{
	/* Get a string with completion and a default */

	if (fullscreen) {
		tmove(0, tlines() - 1);
	}
	return((form != NULL) ? get_form(form) :
	       io_funcs->line(prompt, deflt, NULL, complete, ctype, FALSE));
}
/****************************************************************************/
char *get_ecstr(form, prompt, deflt, complete, ctype)
FORM *form;
char *prompt, *deflt;
CLIST *(*complete)();
int ctype;
{
        /* Get a string with completion and a default to edit */

	if (fullscreen) {
		tmove(0, tlines() - 1);
	}
        return((form != NULL) ? get_form(form) :
	       io_funcs->line(prompt, NULL, deflt, complete, ctype, FALSE));
}
/****************************************************************************/
char *get_pwstr(prompt)
char *prompt;
{
	/* Get a password string from the get_a_line interface */

	if (fullscreen) {
		tmove(0, tlines() - 1);
	}
	return(io_funcs->line(prompt, NULL, NULL, NULL, C_NONE, TRUE));
}
/****************************************************************************/
KEYSEQ *get_keyseq(form, prompt)
FORM *form;
char *prompt;
{
	/* Get a key sequence from a form or the get_a_line interface */

	if (form != NULL) {
		return(form->value.atom);
	}
	if (fullscreen) {
		tmove(0, tlines() - 1);
	}
	return(io_funcs->seq(prompt, NULL, NULL, NULL, C_NONE));
}
/****************************************************************************/
static char *get_form(form)
FORM *form;
{
	/* Return text from a form in a static buffer */

	static char *buf = NULL;

	/* Free any old buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* Buffer and return the form's text */

	return(buf = xstrdup(formtext(form)));
}
/****************************************************************************/
void msg(text)
char *text;
{
	/* Display a message for the user */

	if (!scripting) {
		new_msg();
		show_msg(text);
		end_msg();
	}
	return;
}
/****************************************************************************/
void cmsg(text)
char *text;
{
	/* Continue a user message */

	if (!scripting) {
		hide_cursor();
		show_msg(text);
	}
 	return;
}
/****************************************************************************/
void emsg(text)
char *text;
{
	/* Display an error message for the user */

	if (type_errors) {
		typeout(text);
		typeout("\n");
	} else {
		tbeep();
		new_msg();
		show_msg(text);
		end_msg();
	}
	return;
}
/****************************************************************************/
/*VARARGS*/
void msgl(va_alist)
va_dcl
{
	/*
	 * Display a list of strings for the user.
	 * The calling sequence is :
	 * 	msgl(arg1, arg2, ... , NULL)
	 *
	 * All arguments must be strings.  This could be replaced
	 * by a function calling vsprintf, but there is little
	 * need, and vsprintf is not universally available.
	 */
	
	char *arg;
	va_list arglist;

	/* We don't show messages in scripts */

	if (!scripting) {
		/* Initialise the varargs handling and message length */

		va_start(arglist);
		msg_len = 0;

		/* Set up for a new message */

		new_msg();

		/* Now loop through the arguments, printing them */

		while ((arg = va_arg(arglist, char *)) != NULL) {
			show_msg(arg);
		}

		/* End the message and clean up */

		end_msg();
		va_end(arglist);
	}
	return;
}
/****************************************************************************/
/*VARARGS*/
void emsgl(va_alist)
va_dcl
{
	/*
	 * Beep and display a list of strings for the user.
	 * The calling sequence is :
	 * 	emsgl(arg1, arg2, ... , NULL)
	 *
	 * All arguments must be strings.  This could be replaced
	 * by a function calling vsprintf, but there is little
	 * need, and vsprintf is not universally available.
	 */
	
	char *arg;
	va_list arglist;

	/* Initialise the varargs handling */

	va_start(arglist);

	/* Beep and set up for a new message */

	tbeep();
	new_msg();

	/* Now loop through the arguments, printing them */
	
	while ((arg = va_arg(arglist, char *)) != NULL) {
		if (type_errors) {
			typeout(arg);
		} else {
			show_msg(arg);
		}
	}

	/* Flush the output or end the line and clean up */

	if (type_errors) {
		typeout("\n");
	}
	end_msg();
	va_end(arglist);

	return;
}
/****************************************************************************/
void clearmsg()
{
	/* Clear any previously displayed message */

	if (!scripting) {
		new_msg();
		end_msg();
	}
	return;
}
/****************************************************************************/
void hide_cursor()
{
	/* Move the cursor to the end of the current message */

	if (fullscreen) {
		tmove(msg_len, tlines() - 1);
	} else if (msg_len == 0) {
		treturn();
	}
	(void) fflush(stdout);

	return;
}
/****************************************************************************/
void show_lossage()
{
	/* Display the lossage to typeout */

	char *p = lossage_start, *keystr;
	int col = 0;

	/* Typeout each character in the lossage */

	while (p != lossage_end) {
		/* Get the character and see if it will fit */

		keystr = xstrdup(strkey(*p, SK_KEYSEQ));
		if ((col = col + strlen(keystr) + 1) >= tcols()) {
			/* A line break here would look good */

			typeout("\n");
			col = 0;
		}

		/* Type-out the character and a space */

		typeout(keystr);
		typeout(" ");
		free(keystr);

		/* And increment the pointer */

		p = (p >= lossage + LOSSAGE_SIZE) ? lossage : p + 1;
	}

	/* End the typeout and return */

	typeout("\n");
	typeout(NULL);
	return;
}
/****************************************************************************/
static void new_msg()
{
	/* Position the cursor ready to show a new message */

	if (fullscreen && !scripting) {
		tmove(0, tlines() - 1);
	} else if (!scripting) {
		treturn();
	}
	touched_mb = TRUE;
	msg_len = 0;

	return;
}
/****************************************************************************/
static void show_msg(text)
char *text;
{
	/* Handle displaying of messages to the echo area */

	char *c, *k;

	for (c = text; *c != '\0' && msg_len < tcols() - 1; c++) {
		/* Canonicalise and output the character */

		for (k = strkey(*c, SK_DISPLAY); *k != '\0'
		     && msg_len < tcols() - 1; k++) {
			(void) putchar(*k);
			msg_len++;
		}
	}
	return;
}
/****************************************************************************/
static void end_msg()
{
	/* Clean up after displaying a message */

	if (scripting) {
		(void) fputs("\r\n", stdout);
	} else {
		tclrline(tcols() - msg_len - 1);
	}
	(void) fflush(stdout);
	return;
}
/****************************************************************************/
static int readkey(delay)
unsigned delay;
{
	/*
	 * Grab an ASCII key from the keyboard and return it,
	 * or if delay is non-zero time out and return EOF if
	 * the key is not available within delay seconds.
	 */

	char c;
	int key, flags = 0;
	time_t timeout;

	/* Flush the standard output to keep things sane */

	(void) fflush(stdout);

	/* Set up when to timeout the read */

	timeout = (delay) ? time(NULL) + delay : 0;

	/* We may want nodelay mode on the standard input */

	if (delay) {
		flags = fcntl(0, F_GETFL, 0);
		(void) fcntl(0, F_SETFL, flags|O_NDELAY);
	}

	/* Get the key */

	while (read(fileno(stdin), &c, 1) <= 0) {
		if (delay && time(NULL) > timeout) {
			(void) fcntl(0, F_SETFL, flags);
			return(EOF);
		}
	}
	key = c;

	/* May need to turn nodelay mode off again */

	if (delay) {
		(void) fcntl(0, F_SETFL, flags);
	}

	/* Add the character to the lossage */

	*lossage_end = key;
	lossage_end = (lossage_end >= lossage + LOSSAGE_SIZE)
		? lossage : lossage_end + 1;
	lossage_start = (lossage_end > lossage_start ||
			 lossage_end == lossage + LOSSAGE_SIZE)
		? lossage : lossage_end + 1;

	/* Postprocess the key if required and return it */

	if (postprocess_key != NULL) {
		key = postprocess_key(key);
	}
	return(key);
}
/****************************************************************************/
static int readahead()
{
	/* Return TRUE if there are input characters pending */

	char c;
	int flags, nchar;

	/* Set nodelay mode on the standard input */

	flags = fcntl(0, F_GETFL, 0);
	(void) fcntl(0, F_SETFL, flags|O_NDELAY);

	/* Get the character */

	nchar = read(fileno(stdin), &c, 1);

	/* Turn nodelay mode off again */

	(void) fcntl(0, F_SETFL, flags);

	/* Now see if we have read a character */

	if (nchar == 1) {
		type_buf = c;
		return(TRUE);
	}

	/* There weren't any characters pending */

	return(FALSE);
}
/****************************************************************************/
