/* Help_cmd.c - Help-related commands for af.
   Copyright (C) 1995, 1996 Malc Arnold.

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
#include <regex.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "commands.h"
#include "keymaps.h"
#include "macros.h"
#include "variable.h"
#include "mode.h"
#include "complete.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: help_cmd.c,v 1.6 1996/09/22 16:42:01 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xstrdup(), *vstrcat();
extern char *strerror(), *expand(), *strseq();
extern char *strbinding(), *get_str(), *get_cstr();
extern char *get_vtext(), *majorname();
extern int set_typeout_file(), error_in_typeout();
extern int describe(), macro_describe(), runcmd();
extern int shellout();
extern unsigned cmodes();
extern void free(), free_seq(), list_aliases();
extern void list_bindings(), list_commands();
extern void list_functions(), list_macros();
extern void list_keymaps(), list_history();
extern void list_vars(), show_lossage();
extern void typeout(), msg(), cmsg();
extern void msgl(), emsg(), emsgl();
extern FUNCTION *find_function();
extern MACRO *find_macro();
extern FORM *exec_help_key();
extern BINDING *dereference();
extern KEYSEQ *get_prefix();
extern CLIST *func_complete(), *mode_complete();
extern CLIST *var_complete();

/* Local function declarations */

static int do_describe();
static int do_key_describe();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the current window, last command, and quit flag from commands.c */

extern WINDOW *cwin;
extern COMMAND *last_command;
extern int user_quit;

/****************************************************************************/
/*ARGSUSED*/
FORM *help_help(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print the help text for help */

	if (last_command != NULL && last_command->func == help_help) {
		(void) describe(D_AF, D_HELP, TRUE);
	}
	msg(HELP_STRING);

	/* Now rescan the help keymap for the next keypress */

	return(exec_help_key(seq, arg));
}
/****************************************************************************/
/*ARGSUSED*/
FORM *alias_list(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* List all defined aliases to typeout */

	/* Redirect typeout to a file if argument given */

	if (!set_typeout_file(forms, arg, "alias list")) {
		return(c_errored());
	}

	list_aliases();
	typeout(NULL);

	return((error_in_typeout()) ? c_errored() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *bind_list(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* List all key bindings to typeout */

	/* Redirect typeout to a file if argument given */

	if (!set_typeout_file(forms, arg, "binding list")) {
		return(c_errored());
	}

	list_bindings();
	typeout(NULL);

	return((error_in_typeout()) ? c_errored() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *cmd_list(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* List all available commands to typeout */

	/* Redirect typeout to a file if argument given */

	if (!set_typeout_file(forms, arg, "command list")) {
		return(c_errored());
	}

	list_commands(NULL, FALSE);
	typeout(NULL);

	return((error_in_typeout()) ? c_errored() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *func_list(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* List all available functions to typeout */

	/* Redirect typeout to a file if argument given */

	if (!set_typeout_file(forms, arg, "function list")) {
		return(c_errored());
	}

	list_functions(NULL, FALSE);
	typeout(NULL);

	return((error_in_typeout()) ? c_errored() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *macro_list(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* List all available keyboard macros to typeout */

	/* Redirect typeout to a file if argument given */

	if (!set_typeout_file(forms, arg, "macro list")) {
		return(c_errored());
	}

	list_macros(NULL, FALSE, TRUE, TRUE);
	typeout(NULL);

	return((error_in_typeout()) ? c_errored() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *map_list(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* List all available keymaps to typeout */

	/* Redirect typeout to a file if argument given */

	if (!set_typeout_file(forms, arg, "keymap list")) {
		return(c_errored());
	}

	list_keymaps();
	typeout(NULL);

	return((error_in_typeout()) ? c_errored() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *hist_list(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* List the minibuffer history to typeout */

	/* Redirect typeout to a file if argument given */

	if (!set_typeout_file(forms, arg, "minibuffer history list")) {
		return(c_errored());
	}

	list_history();
	typeout(NULL);

	return((error_in_typeout()) ? c_errored() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *var_list(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* List all defined variables to typeout */

	/* Redirect typeout to a file if argument given */

	if (!set_typeout_file(forms, arg, "variable list")) {
		return(c_errored());
	}

	list_vars(NULL, FALSE);
	typeout(NULL);

	return((error_in_typeout()) ? c_errored() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *func_describe(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print the help text relating to a function or macro */

	char *func;
	MACRO *macro;

	/* Get the function to describe */

	if ((func = get_cstr(forms, "Describe function: ",
			     func_complete, C_STRICT)) == NULL) {
		return(c_errored());
	}
	forms = (forms != NULL) ? forms->next : NULL;

	/* Check if we're describing a macro */

	if ((macro = find_macro(func)) != NULL) {
		return((macro_describe(macro, arg, forms))
		       ? c_t() : c_errored());
	}

	/* Now describe the function or command */

	return((do_describe(forms, arg, (find_function(func) != NULL)
			    ? D_FUNCTION : D_COMMAND, func))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *key_describe(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Describe the command bound to a given key */

	return((do_key_describe(forms, arg, "Describe key: ", FALSE))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *brf_describe(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Report the command bound to a given key */

	return((do_key_describe(forms, NULL, "Describe key briefly: ", TRUE))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *major_describe(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print the text relating to the current major mode */

	return((do_describe(forms, arg, D_MODE, majorname(cmodes(0))))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *mode_describe(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print the text relating to a mode */

	char *mode;

	/* Get the name of the mode to describe */

	if ((mode = get_cstr(forms, "Describe mode: ",
		mode_complete, C_STRICT)) == NULL) {
		return(c_errored());
	}
	forms = (forms != NULL) ? forms->next : NULL;

	/* And describe it */

	return((do_describe(forms, arg, D_MODE, mode))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *var_describe(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print the text relating to a variable */

	char *var;

	/* Get the name of the variable to describe */

	if ((var = get_cstr(forms, "Describe variable: ",
			var_complete, C_STRICT)) == NULL) {
		return(c_errored());
	}
	forms = (forms != NULL) ? forms->next : NULL;

	/* And describe it */

	return((do_describe(forms, arg, D_VARIABLE, var))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *apropos(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print all entities that match a regular expression */

	char *expr, *errbuf;
	int status;
	size_t errlen;
	regex_t rexpr;

	/* Get the expression to match */

	if ((expr = get_str(forms, "Apropos: ")) == NULL && user_quit) {
		return(c_errored());
	}
	expr = (expr != NULL) ? xstrdup(expr) : NULL;
	forms = (forms != NULL) ? forms->next : NULL;

	/* Redirect typeout to a file if argument given */

	if (!set_typeout_file(forms, arg, "apropos list")) {
		if (expr != NULL) {
			free(expr);
		}
		return(c_errored());
	}

	/* Compile and check the expression */

	if (expr != NULL && (status = regcomp(&rexpr, expr, REG_ICASE))) {
		/* Getting a POSIX regex error message is convoluted */

		errlen = regerror(status, &rexpr, NULL, 0);
		errbuf = xmalloc(errlen + 1);
		(void) regerror(status, &rexpr, errbuf, errlen);
		emsgl("Invalid expression: ", errbuf, NULL);

		/* Clean up and fail */

		free(expr);
		free(errbuf);
		regfree(&rexpr);
		return(c_errored());
	}

	/* Typeout a header */

 	typeout("Apropos listing for ");
	typeout((expr != NULL) ? expr : "NULL");
	typeout("\n");

	/* List any appropriate entries */

	list_commands((expr != NULL) ? &rexpr : NULL, TRUE);
	list_functions((expr != NULL) ? &rexpr : NULL, TRUE);
	list_vars((expr != NULL) ? &rexpr : NULL, TRUE);

	/* Clean up and return */

	if (expr != NULL) {
		free(expr);
		regfree(&rexpr);
	}
	typeout(NULL);

	return((error_in_typeout()) ? c_errored() : c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *info(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Browse the af manual in info format */

	char *cmd;
	
	/* Get the command line to execute */

	if ((cmd = get_vtext(V_INFO)) == NULL) {
		emsg("Can't browse: info-browser variable not set");
		return(c_errored());
	}
	
	/* Check if we're displaying the file to typeout */

	if (!strcmp(cmd, V_USE_TYPEOUT)) {
		/* Execute the typeout browser to typeout */

		return((runcmd(TYPEINFOBROWSER, FALSE))
		       ? c_t() : c_errored());
	}

	/* Execute the command */

	return((shellout(cmd, FALSE)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *news(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print the help text for news */

	return((do_describe(forms, arg, D_AF, D_NEWS))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *copying(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print the help text for copying */

	return((do_describe(forms, arg, D_AF, D_COPYING))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *warranty(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Print the help text for no warranty */

	return((do_describe(forms, arg, D_AF, D_WARRANTY))
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *view_lossage(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Show the lossage, ie. the last 100 characters typed */

	/* Redirect typeout to a file if argument given */

	if (!set_typeout_file(forms, arg, "lossage")) {
		return(c_errored());
	}

	show_lossage();
	return((error_in_typeout()) ? c_errored() : c_t());
}
/****************************************************************************/
static int do_describe(forms, arg, type, name)
FORM *forms;
ARGUMENT *arg;
char *type, *name;
{
	/* Get an object name and describe it */

	char *prompt;
	int status;

	/* Copy the name, just in case */

	name = xstrdup(name);

	/* Redirect typeout to a file if required */

	prompt = vstrcat("Help for ", type, " ", name, NULL);
	status = set_typeout_file(forms, arg, prompt);
	free(prompt);

	/* If all's well so far, describe the object */

	status = (status && describe(type, name, FALSE));

	/* Free the name and return status */

	free(name);
	return(status);
}
/****************************************************************************/
static int do_key_describe(forms, arg, prompt, brief)
FORM *forms;
ARGUMENT *arg;
char *prompt;
int brief;
{
	/* Get a key sequence and describe it */

	int status = TRUE;
	BINDING *binding;
	KEYSEQ *seq;
	FORM *argform;

	/* Get and check the key sequence */

	if ((seq = (forms != NULL) ? forms->value.atom :
	     get_prefix(NULL, cmodes(0), prompt)) == NULL) {
		/* Error in entered prefix */

		return(FALSE);
	}
	argform = (forms != NULL) ? forms->next : NULL;

	/* Now describe the resulting sequence */

	binding = dereference(NULL, cmodes(0), seq);

	/* Display either brief or verbose information */

	if (binding == NULL) {
		msgl(strseq(seq, SK_KEYSEQ), " is not bound", NULL);
	} else if (brief) {
		msgl(strseq(seq, SK_KEYSEQ), " runs the command ",
		     strbinding(binding), NULL);
	} else {
		status = do_describe(argform, arg, D_COMMAND,
				     strbinding(binding), FALSE);
	}

	/* Free the key sequence and return status */

	if (forms == NULL) {
		free_seq(seq);
	}
	return(status);
}
/****************************************************************************/
