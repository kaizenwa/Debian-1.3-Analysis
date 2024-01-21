/* Macros.c - Keyboard macro support for af.
   Copyright (C) 1995, 1996, 1997 Malc Arnold.

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
#include <regex.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "commands.h"
#include "keymaps.h"
#include "macros.h"
#include "variable.h"
#include "complete.h"
#include "io.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: macros.c,v 1.7 1997/03/05 21:23:45 malc Exp $";
static char *MacroId = MACROID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup();
extern char *strseq(), *strcanon(), *bindings();
extern int strcasecmp(), strncasecmp();
extern int get_key(), get_tkey(), get_vval();
extern int vtupdate();
extern void free(), free_seq(), set_postprocess();
extern void msg(), emsg(), emsgl(), clearmsg();
extern void show_status(), typeout(), table();
extern void tbeep(), vtredraw();
extern COMMAND *find_command();
extern FUNCTION *find_function();
extern KEYMAP *find_keymap();
extern KEYSEQ *make_seq(), *add_seq();
extern CLIST *add_clist();
extern IOFUNC *set_input();

/* Local function declarations */

MACRO *find_macro();
static int add_to_macro(), read_macro();
static int type_macro();
static MACRO *add_macro();

/****************************************************************************/
/* Import the user quit flag from commands.c */

extern int user_quit;

/****************************************************************************/
/* The last and currently defined keyboard macros */

static KEYSEQ *last_macro = NULL;
static KEYSEQ *this_macro = NULL;

/****************************************************************************/
/* The list of named keyboard macros */

static MACRO *macros = NULL;

/****************************************************************************/
/* The stack of keyboard macros currently being executed */

static EXEC_STATUS exec_macros[MACRO_DEPTH];
static int depth = -1;

/* And a flag to show that we are querying the user */

static int querying = FALSE;

/****************************************************************************/
/* The current and original input functions */

static IOFUNC io_macro = { read_macro, type_macro, NULL, NULL };
static IOFUNC *io_orig = NULL;

/****************************************************************************/
int defining()
{
	/* Return whether we are defining a keyboard macro */

	return(this_macro != NULL);
}
/****************************************************************************/
int executing()
{
	/* Return whether we are executing a keyboard macro */

	return(depth >= 0 && !querying);
}
/****************************************************************************/
int record_macro(win, append)
WINDOW *win;
int append;
{
	/* Record a new macro or append keys to the current one */

	/* Check that we can record a macro */

	if (append && last_macro == NULL) {
		/* Can't append to a null macro */

		emsg("No kbd macro defined");
		return(FALSE);
	} else if (this_macro != NULL) {
		/* We're already defining a macro */

		emsg("Already defining kbd macro");
		return(FALSE);
	}

	/* Initialise the current keyboard macro */

	this_macro = make_seq((append) ? last_macro->keys : NULL,
			      (append) ? last_macro->len : 0);

	/* And set up to record the macro */

	set_postprocess(add_to_macro);
	show_status(win, NULL);
	msg("(Defining kbd macro...)");

	return(TRUE);
}
/****************************************************************************/
int end_recording(win, seq)
WINDOW *win;
KEYSEQ *seq;
{
	/* End recording of a macro */

	/* Check we are defining a macro */

	if (this_macro == NULL) {
		emsg("Not defining kbd macro");
		return(FALSE);
	}

	/* Stop recording keystrokes now */

	set_postprocess(NULL);

	/* Strip any sequence from the macro */

	if (seq != NULL && seq->len <= this_macro->len) {
		this_macro->len -= seq->len;
		this_macro->keys = xrealloc(this_macro->keys,
					    this_macro->len);
	}

	/* Replace any old last macro with the new */

	if (last_macro != NULL) {
		free_seq(last_macro);
	}
	last_macro = this_macro;
	this_macro = NULL;

	/* Let the user know what's happened */

	show_status(win, NULL);
	msg("(Keyboard macro defined)");

	return(TRUE);
}
/****************************************************************************/
int run_macro(macro, repeat)
MACRO *macro;
int repeat;
{
	/* Execute the (last) macro repeat times */

	KEYSEQ *seq;

	/* Check we can execute the macro */

	if (macro == NULL && (seq = last_macro) == NULL) {
		emsg("No kbd macro defined");
		return(FALSE);
	} else if (depth >= MACRO_DEPTH) {
		emsg("Exceeded kbd macro recursion depth");
		return(FALSE);
	}

	/* Get the key sequence to execute */

	seq = (macro != NULL) ? macro->keys : last_macro;

	/* And reset the input pointers if required */

	if (repeat > 0 && seq->len > 0) {
		/* Set up to read input from the macro */

		if (depth++ < 0) {
			io_macro.line = NULL;
			io_orig = set_input(&io_macro);
		}

		/* And set the macro's parameters */

		exec_macros[depth].keys = seq;
		exec_macros[depth].repeat = repeat;
		exec_macros[depth].pos = 0;
	}
	return(TRUE);
}
/****************************************************************************/
void kill_macros(win)
WINDOW *win;
{
	/* Kill the current keyboard macro after an error */

	if (this_macro != NULL) {
		/* Discard the current macro */

		set_postprocess(NULL);
		free_seq(this_macro);
		this_macro = NULL;
		show_status(win, NULL);
	} else if (depth >= 0) {
		/* Kill all executing macros */

		depth = -1;
		(void) set_input(io_orig);
	}
	return;
}
/****************************************************************************/
int query_user()
{
	/* Query the user during execution of a macro */

	int key;
	IOFUNC *io_now;

	/* Check that we are executing a macro */

	if (this_macro != NULL) {
		/* Defining a macro; do nothing */

		return(TRUE);
	} else if (depth < 0) {
		/* This only works during a keyboard macro */

		emsg("Not defining or executing kbd macro");
		return(FALSE);
	}

	/* Loop until the user selects an option */

	for (;;) {
		/* We are now querying the user */

		querying = TRUE;

		/* It would be nice if the screen was up-to-date */
	
		(void) vtupdate(TRUE);

		/* Prompt the user with the options */

		msg("Proceed with macro? (SPC, RET, ESC, or C-l) ");

		/* Reset the input routines and get the key */

		io_now = set_input(io_orig);
		key = get_key();
		(void) set_input(io_now);
		clearmsg();

		/* We're not qeurying any more */

		querying = FALSE;

		/* Check for a quit character */

		if (key == get_vval(V_QUITCHAR)) {
			/* The user quit; cancel the macro */

			user_quit = TRUE;
			emsg("Quit");
			return(FALSE);
		}

		/* Now process the key as required */

		switch (key) {
		case QUERY_CONTINUE:
			/* Continue with the macro */

			return(TRUE);
		case QUERY_SKIP:
			/* Skip the rest of the macro and continue */

			exec_macros[depth].pos = exec_macros[depth].keys->len;
			return(TRUE);
		case QUERY_ABORT:
			/* Abort all further repetitions of the macro */

			exec_macros[depth].pos = exec_macros[depth].keys->len;
			exec_macros[depth].repeat = 0;
			return(TRUE);
		case QUERY_REDRAW:
			/* Redraw the screen and ask again */

			vtredraw();
			break;
		default:
			/* Invalid input from the user */

			tbeep();
		}
	}
	/*NOTREACHED*/
}
/****************************************************************************/
int named_macro(name, value)
char *name;
KEYSEQ *value;
{
	/*
	 * Make a macro with the given name and value if not
	 * NULL, or the last keyboard macro otherwise.
	 */

	char *keys;
	int len;

	/* Check that we can use the last macro */

	if (value == NULL && last_macro == NULL) {
		emsg("No kbd macro defined");
		return(FALSE);
	}

	/* Set up the keys and their length */

	keys = (value == NULL) ? last_macro->keys : value->keys;
	len = (value == NULL) ? last_macro->len : value->len;

	/* Now check if we can use the name */

	if (find_command(name) != NULL || find_function(name) != NULL) {
		/* Can't overwrite a function name */

		emsgl("Can't create ", name,
		      ": already defined as a function", NULL);
		return(FALSE);
	} else if (find_keymap(name) != NULL) {
		/* Can't overwrite a keymap name */

		emsgl("Can't create ", name,
		      ": already defined as a keymap", NULL);
		return(FALSE);
	}

	/* Add the macro and return success */

	macros = add_macro(name, keys, len);
	return(TRUE);
}
/****************************************************************************/
MACRO *find_macro(name)
char *name;
{
	/* Find a macro with the specified name */

	MACRO *macro;

	/* Loop over the existing named macros */

	for (macro = macros; macro != NULL; macro = macro->next) {
		/* Is this the macro we're looking for? */

		if (!strcasecmp(macro->name, name)) {
			return(macro);
		}
	}

	/* No such macro exists */

	return(NULL);
}
/****************************************************************************/
void list_macros(rexpr, headings, def, force)
regex_t *rexpr;
int headings, def, force;
{
	/* Typeout the named macros (or those matching a regex) */

	MACRO *macro;

	/* Check that we can output something */

	if (macros == NULL && force) {
		msg("No kbd macros defined");
		return;
	}

	/* Loop through the macros */

	for (macro = macros; !user_quit &&
	     macro != NULL; macro = macro->next) {
		/* Should we print this macro? */

		if (rexpr == NULL || !regexec(rexpr, macro->name,
					      0, NULL, 0)) {
			/* Print a heading if required */

			if (headings) {
				typeout("\nKbd Macros:\n");
				headings = FALSE;
			}

			/* And print the keyboard macro */

			table(macro->name, (def) ? strseq(macro->keys,
				SK_KEYSEQ) : bindings(macro->name));
		}
	}
	return;
}
/****************************************************************************/
CLIST *macro_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of named macros completing base */

	MACRO *macro;

	/* Build the list of possible values */

	for (macro = macros; macro != NULL; macro = macro->next) {
		/* Add this macro if it matches the base */

		if (!strncasecmp(base, macro->name, strlen(base))) {
			list = add_clist(list, macro->name, FALSE);
		}
	}
	return(list);
}
/****************************************************************************/
static int add_to_macro(key)
int key;
{
	/* Add a key to the macro currently being defined */

	this_macro = add_seq(this_macro, key);
	return(key);
}
/****************************************************************************/
/*ARGSUSED*/
static int read_macro(delay)
int delay;
{
	/* Return the next key from the macro */

	/* Check if we're reached the end of the macro */

	while (exec_macros[depth].pos >= exec_macros[depth].keys->len) {
		/* End of macro; check next repeat */

		exec_macros[depth].pos = 0;
		if (exec_macros[depth].repeat-- <= 1) {
			/* End of repetions; check depth */

			if (depth-- <= 0) {
				/* Restore original inputs */

				(void) set_input(io_orig);
				return(get_tkey(delay));
			}
		}
	}

	/* Now return the key from the macro */

	return(exec_macros[depth].keys->keys[exec_macros[depth].pos++]);
}
/****************************************************************************/
/*ARGSUSED*/
static int type_macro()
{
	/* Return TRUE if the current macro has typeahead */

	int level;

	/* Check each recursion level for more input */

	for (level = depth; level >= 0; level--) {
		/* Check if this macro has more input */

		if (exec_macros[level].pos < exec_macros[level].keys->len
		    || exec_macros[level].repeat > 1) {
			/* This macro has more input */

			return(TRUE);
		}
	}

	/* There is no more input from the macro(s) */

	return(FALSE);
}
/****************************************************************************/
static MACRO *add_macro(name, keys, len)
char *name, *keys;
int len;
{
	/* Add a named macro which runs the specified keys */

	MACRO *macro, *m;

	/* Are we updating an existing macro? */

	if ((macro = find_macro(name)) != NULL) {
		/* Replace the old definition with the new */

		free_seq(macro->keys);
		macro->keys = make_seq(keys, len);
		return(macros);
	}

	/* We need to make a new named macro */

	macro = (MACRO *) xmalloc(sizeof(MACRO));
	macro->name = xstrdup(name);
	macro->keys = make_seq(keys, len);

	/* And to add it to the list */

	for (m = macros; m != NULL; m = m->next) {
		/* Check if this is where the macros should go */

		if (strcasecmp(m->name, name) < 0 &&
		    (m->next == NULL ||
		     strcasecmp(m->next->name, name) > 0)) {
			/* Insert the new macro after this one */

			macro->next = m->next;
			m->next = macro;
			return(macros);
		}
	}

	/* We need to prepend the macro to the list */

	macro->next = macros;
	return(macro);
}
/****************************************************************************/
int user_macros(fp)
FILE *fp;
{
	/* Write definitions for any named macros to fp */

	char *macname, *macval;
	MACRO *m;

	/* Output a comment before the macros */

	if (macros != NULL && fputs("\n; Kbd Macros\n", fp) == EOF) {
		return(FALSE);
	}

	/* Write each macro to the file */

	for (m = macros; m != NULL; m = m->next) {
		/* Set up the macro name and value */

		macname = strcanon(m->name, SK_READSYM);
		macval = strseq(m->keys, SK_READKEY);

		/* Output the macro definition */

		if (m->keys->len > 0 &&
		    fprintf(fp, "(define-kbd-macro '%s \"%s\")\n",
			    macname, macval) == EOF) {
			/* Error writing the file */

			return(FALSE);
		}
	}

	/* All written ok */

	return(TRUE);
}
/****************************************************************************/
