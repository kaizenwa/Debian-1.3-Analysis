/* Conf_cmd.c - Configuration-handling commands for af.
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
#include "keymaps.h"
#include "variable.h"
#include "mode.h"
#include "complete.h"
#include STRING_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: conf_cmd.c,v 1.21 1997/01/29 18:33:12 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xstrdup(), *vstrcat(), *get_home();
extern char *expand(), *alias(), *realname(), *a_strerror();
extern char *formtext(), *strseq(), *strdate(), *get_str();
extern char *get_cstr(), *get_dcstr(), *get_estr();
extern int isalias(), confirm(), long_confirm(), set_var();
extern int fset_var(), get_key(), new_keymap(), set_key();
extern int record_macro(), end_recording(), query_user();
extern int run_macro(), named_macro(), user_vars();
extern int user_macros(), user_keymaps(), user_bindings();
extern unsigned cmodes();
extern void free(), free_seq(), toggle_mode();
extern void new_alias(), msg(), msgl(), cmsg();
extern void emsg(), emsgl(), show_status();
extern KEYMAP *mode_keymap();
extern KEYSEQ *get_prefix();
extern DATEZONE *date_now();
extern ARGUMENT *form_or_arg();
extern CLIST *fn_complete(), *var_complete();
extern CLIST *cmdmap_complete();

/* Local function declarations */

static int do_set_key();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the current window and user quit flag from commands.c */

extern WINDOW *cwin;
extern int user_quit;

/****************************************************************************/
/*ARGSUSED*/
FORM *readonly(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Toggle the current setting of the Read-Only mode */

	toggle_mode(cwin->buf, M_READONLY);
	show_status(cwin, cwin->buf);

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *set(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Set a variable */

	char *name;

	/* Get the variable name */

	if ((name = get_cstr(forms, "Set variable: ", var_complete,
			     C_STRICT)) == NULL) {
		return(c_errored());
	}

	/* Now set the variable as appropriate */

	return(((forms != NULL) ? fset_var(name, forms->next)
		: set_var(name)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *set_alias(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Set an alias, usually for the sender of the current message */

	char *prompt, *newprompt;
	char *aname, *rname, *users;
	int status;

	/* Initialise the prompt */

	prompt = xstrdup("Set alias: ");

	/* Get the alias name */

	if ((aname = get_str(forms, prompt)) == NULL) {
		return(c_errored());
	}
	forms = (forms != NULL) ? forms->next : NULL;

	/* Check if the alias already exists */

	if (isalias(aname)) {
		newprompt = vstrcat("Alias ", aname, " already exists;", 
				    "  replace? ", NULL);
		status = confirm(newprompt, TRUE);
		free(newprompt);

		/* Return if no confirmation */

		if (!status) {
			return(c_errored());
		}
	}

	/* Copy the name (it's in a static buffer) */

	aname = xstrdup(aname);

	/* Update the prompt */

	newprompt = vstrcat(prompt, aname, " Real name: ", NULL);
	free(prompt);
	prompt = newprompt;

	/* Get the real name */

	rname = (cwin == NULL || cwin->buf == NULL ||
		 cwin->point->text == NULL) ? NULL : cwin->point->from;

	if ((rname = get_estr(forms, prompt, rname)) == NULL) {
		free(prompt);
		free(aname);
		return(c_errored());
	}
	forms = (forms != NULL) ? forms->next : NULL;

	/* Check the real name */

	if ((rname = realname(rname)) == NULL) {
		emsgl("Bad real name", rname, NULL);
		free(prompt);
		free(aname);
		return(c_errored());
	}

	/* Update the prompt */

	newprompt = vstrcat(prompt, rname, " Addresses: ", NULL);
	free(prompt);
	prompt = newprompt;

	/* Get the default address list */

	users = (cwin != NULL && cwin->buf != NULL)
		? cwin->point->reply : NULL;

	/* And prompt the user for the user list */

	if ((users = get_estr(forms, prompt, users)) == NULL) {
		free(prompt);
		free(aname);
		free(rname);
		return(c_errored());
	}

	/* Now canonicalise and expand the users */

	if ((users = alias(users)) == NULL) {
		emsg(a_strerror());
		free(prompt);
		free(aname);
		free(rname);
		return(c_errored());
	}

	/* Actually add the alias and free space */

	new_alias(aname, rname, users);
	free(prompt);
	free(aname);
	free(rname);
	free(users);

	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *gl_set_key(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Bind a command to a key in the global keymaps */

	return((do_set_key(forms, "Global set key: ", M_NULL, FALSE))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *gl_unset_key(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Unbind a key in the global keymaps */

	return((do_set_key(forms, "Global unset key: ", M_NULL, TRUE))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *lo_set_key(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Bind a command to a key in the local keymaps */

	return((do_set_key(forms, "Local set key: ", cmodes(0), FALSE))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *lo_unset_key(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Unbind a key in the local keymaps */

	return((do_set_key(forms, "Local unset key: ",
		cmodes(0), TRUE)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *mb_set_key(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Bind a key in the minibuffer */

	return((do_set_key(forms, "Minibuffer set key: ", M_MBUF, FALSE))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *mb_unset_key(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Unbind a key in the minibuffer */

	return((do_set_key(forms, "Minibuffer unset key: ", M_MBUF, TRUE))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_set_key(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Bind a key in typeout */

	return((do_set_key(forms, "Typeout set key: ", M_TYPEOUT, FALSE))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *to_unset_key(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Unbind a key in typeout */

	return((do_set_key(forms, "Typeout unset key: ", M_TYPEOUT, TRUE))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *make_keymap(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Make a new keymap with a given name */

	char *mapname;

	/* Get the name of the keymap to add */

	if ((mapname = get_str(forms, "Make keymap: ")) == NULL) {
		return(c_errored());
	}

	return((new_keymap(mapname)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *start_macro(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Start defining a keyboard macro */

	/* Convert any forms to an argument */

	arg = form_or_arg(forms, arg);

	/* We need to repeat a macro to append to */

	if (arg != NULL && !run_macro(NULL, 1)) {
		/* Error executing macro to append to */

		return(c_errored());
	}

	/* Start recording keystrokes now */

	return((record_macro(cwin, arg != NULL)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *end_macro(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* End defining a keyboard macro */

	int repeat;

	/* Convert any forms to an argument */

	arg = form_or_arg(forms, arg);

	/* How many times do we want to repeat the macro? */

	repeat = (arg != NULL && !(arg->negative)) ? arg->value : 0;

	/* Stop recording keystrokes and run the macro if required */

	return((end_recording(cwin, seq) &&
		run_macro(NULL, repeat)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *call_macro(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Execute the last keyboard macro */

	int repeat;

	/* Convert any forms to an argument */

	arg = form_or_arg(forms, arg);

	/* Check how many times to repeat the macro */

	repeat = (arg != NULL) ? (arg->negative) ? 0 : arg->value : 1;

	/* Execute the macro and return status */

	return((run_macro(NULL, repeat)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *macro_query(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Query the user during a keyboard macro */

	return((query_user()) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *name_macro(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Name the last keyboard macro */

	char *name;

	/* Check there is a keyboard macro to name */

	if (!run_macro(NULL, 0)) {
		return(c_errored());
	}

	/* Get the name for the macro */

	if ((name = get_str(forms, "Name last kbd Macro: ")) == NULL) {
		return(c_errored());
	}

	/* Name the last macro and return status */

	return((named_macro(name, NULL)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *write_config(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Write the configuration into a named filed */

	char *deflt, *filnam, *prompt;
	int status;
	FILE *fp;

	/* Get the file to store the configuration in */

	deflt = vstrcat(get_home(NULL), "/", STARTUPFILE, NULL);
	if ((filnam = get_dcstr(forms, "Write configuration to file: ",
				deflt, fn_complete, C_PERMISSIVE)) == NULL) {
		return(c_errored());
	}
	free(deflt);

	/* Handle ~ or + in the file name */
	
	filnam = expand(filnam);

	/* If the file exists, confirm we want to overwrite it */

	if (access(filnam, 00) == 0) {
		/* Make the prompt and get confirmation */

		prompt = vstrcat("Overwrite existing ", filnam, "? ", NULL);
		status = long_confirm(prompt, TRUE);
		free(prompt);

		/* Quit if we didn't get confirmation */

		if (!status) {
			free(filnam);
			return(c_errored());
		}
	}

	/* Let the user know what we're doing */

	msgl("Writing configuration to ", filnam, "...", NULL);

	/* Open the file */

	if ((fp = fopen(filnam, "w")) == NULL) {
		emsgl("Can't open ", filnam, ": ", strerror(errno), NULL);
		free(filnam);
		return(c_errored());
	}

	/* Write a header into the file */

	if (fprintf(fp, "; Af startup file, auto-generated on %s\n",
		    strdate(date_now(), TRUE)) == EOF) {
		status = FALSE;
	} else {
		/* Write any modified configuration info */

		status = user_vars(fp) && user_macros(fp) &&
			user_keymaps(fp) && user_bindings(fp);
	}

	/* Check that all went well */

	if (!status) {
		emsgl("Error writing ", filnam, ": ", strerror(errno), NULL);
	} else {
		cmsg(" Done");
	}

	/* Close the file and return status */

	(void) fclose(fp);
	free(filnam);
	return((status) ? c_t() : c_errored());
}
/****************************************************************************/
static int do_set_key(forms, prompt, mode, unset)
FORM *forms;
char *prompt;
unsigned mode;
int unset;
{
	/* Handle binding a key in a given mode */

	char *cmdname = NULL;
	KEYMAP *map;
	KEYSEQ *seq;

	/* Set the base map to search */

	map = mode_keymap(mode, ANY_KEY);

	/* If we have a form then set the key from that */

	if (forms != NULL) {
		return(set_key(map->name, forms->value.atom,
			       (unset || NIL(forms->next))
			       ? NULL : formtext(forms->next)));
	}

	/* Get the key sequence to bind to */

	if ((seq = get_prefix(map, mode, prompt)) == NULL) {
		/* Error in entered sequence */

		return(FALSE);
	}

	/* That gives us the key, now get the command */

	prompt = vstrcat(prompt, strseq(seq, SK_KEYSEQ), " To: ", NULL);
	if (!unset && (cmdname = get_cstr(forms, prompt, cmdmap_complete,
					  C_STRICT)) == NULL && user_quit) {
		/* User quit; return failure */

		free(prompt);
		free_seq(seq);
		return(FALSE);
	}
	free(prompt);

	/* We unset keys if the binding is NULL too */

	unset = (unset || cmdname == NULL);

	/* Now set the key and check status */

	if (!set_key(map->name, seq, cmdname)) {
		free_seq(seq);
		return(FALSE);
	}

	/* Confirm the key was set or unset */

	msgl("(", strseq(seq, SK_KEYSEQ), (unset) ? " unset" :
	     " set to ", (unset) ? "" : cmdname, ")", NULL);

	/* Free space and return success */

	free_seq(seq);
	return(TRUE);
}
/****************************************************************************/
