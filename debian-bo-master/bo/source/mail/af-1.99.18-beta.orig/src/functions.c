/* Functions.c - Function handling for af.
   Copyright (C) 1995, 1996, 1997 Malc Arnold.

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
#include "load.h"
#include "complete.h"
#include "funclist.h"
#include "constlist.h"
#include "version.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: functions.c,v 1.9 1997/05/05 02:50:01 malc Exp $";
static char *FuncId = FUNCTIONID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xstrdup(), *vstrcat();
extern char *strseq(), *getenv(), *syscmd();
extern int fset_var(), set_key(), named_macro();
extern int finish_startup(), startup_display();
extern int strcasecmp(), strncasecmp();
extern unsigned cmodes();
extern void free(), free_seq(), msg();
extern void emsg(), emsgl(), typeout();
extern KEYSEQ *make_seq();
extern COMMAND *find_command();
extern KEYMAP *find_keymap();
extern MACRO *find_macro();
extern VARIABLE *find_variable();
extern CLIST *add_clist();
extern CLIST *cmd_complete();

/* Local function declarations */

char *formtext(), *listtext();
char *strform(), *strlist();
void free_forms();
CONSTANT *find_constant();
FUNCTION *find_function();
FORM *eval(), *cons(), *list(), *atom();
FORM *atomstr(), *add_form(), *get_vform();
static int formcmp();
static FORM *eval_func(), *eval_cmd();
static FORM *eval_args(), *copy_form();
static FORM *copy_list();

/****************************************************************************/
/* Import the current window and user quit flag from commands.c */

extern WINDOW *cwin;
extern int user_quit;

/****************************************************************************/
FORM *eval(form)
FORM *form;
{
	/* Evaluate a form, returning its status */

	char *symname;
	FORM *args;
	FUNCTION *func;
	COMMAND *cmd;
	CONSTANT *constant;

	/* Check if the form is a list to be executed */

	if (form->type == FT_LIST && form->value.list != NULL) {
		/* Get the arguments to the function */

		args = form->value.list->next;

		/* Get the name of the symbol */

		symname = formtext(form->value.list);

		/* Check the type of the symbol */

		if (form->value.list->type != FT_SYMBOL) {
			emsgl("Invalid function ", 
			      strform(form->value.list), NULL);
			return(c_errored());
		}

		/* And evaluate the list as required */

		if ((func = find_function(symname)) != NULL) {
			return(eval_func(func, args));
		} else if ((cmd = find_command(symname)) != NULL) {
			return(eval_cmd(cmd, args));
		}

		/* The form wasn't a valid list */

		emsgl("Invalid function ", symname, NULL);
		return(c_errored());
	}

	/* Check if the form self-evaluates */

	if (form->type == FT_LIST || form->type == FT_STRING
	    || form->type == FT_NUMBER) {
		/* These forms evaluate to themselves */

		return(copy_form(form));
	}

	/* Check if the form is a constant */

	if (form->type == FT_SYMBOL &&
	    (constant = find_constant(formtext(form))) != NULL) {
		/* Return the constant's cononical value */

		return(constant->evalfunc());
	}

	/* Or the form may be a variable */

	if (form->type == FT_SYMBOL &&
	    find_variable(formtext(form)) != NULL) {
		/* Return the evaluated variable */

		return(get_vform(formtext(form)));
	}

	/* Unknown variable; fail with an error */

	emsgl("Unknown variable ", strform(form), NULL);
	return(c_errored());
}
/****************************************************************************/
static FORM *eval_func(func, args)
FUNCTION *func;
FORM *args;
{
	/* Evaluate a function-headed list */

	FORM *form, *newargs;

	/* Evaluate and check the arguments */

	newargs = eval_args(func->name, func->args,
			    func->special, args);

	/* Handle an error or quit in the arguments */

	if (args != NULL && newargs == NULL ||
	    newargs != NULL && ERRORED(newargs)) {
		return(newargs);
	}

	/* Now call the function to handle the function */

	form = func->func(newargs);
	free_forms(newargs);
	return(form);
}
/****************************************************************************/
static FORM *eval_cmd(cmd, args)
COMMAND *cmd;
FORM *args;
{
	/* Evaluate a command-headed list */

	FORM *form, *newargs;

	/* Check we can run the command in the current mode */

	if (!(cmd->modes & cmodes(0))) {
		emsgl("Can't execute ", cmd->name, " in this buffer", NULL);
		return(c_errored());
	}

	/* Evaluate and check the arguments */

	newargs = eval_args(cmd->name, cmd->args, FALSE, args);

	/* Handle an error or quit in the arguments */

	if (args != NULL && newargs == NULL
	    || newargs != NULL && ERRORED(newargs)) {
		return(newargs);
	}

	/* If this command needs buffers, then load them now */

	if (cmd->defer && finish_startup() && !startup_display()) {
		/* Error loading the buffers, abort eval */

		return(c_errored());
	}

	/* Haven't had a user quit yet */

	user_quit = FALSE;

	/* Now call the function to handle the command */

	form = cmd->func(NULL, NULL, newargs);
	free_forms(newargs);
	return(form);
}
/****************************************************************************/
static FORM *eval_args(funcname, argspecs, special, args)
char *funcname;
ARGSPEC *argspecs;
int special;
FORM *args;
{
	/* Evaluate and check the arguments to a function */

	int repeating = FALSE;
	FORM *form, *earg;
	FORM *newargs = NULL;
	ARGSPEC *spec = argspecs;

	/* Check each supplied argument */

	for (form = args; form != NULL; form = form->next) {
		/* Check the argument matches the spec */

		if (spec == NULL || *spec == FT_NULL) {
			/* Too many arguments supplied */

			emsgl("Invalid argument count for function ",
			      funcname, NULL);
			free_forms(newargs);
			return(c_errored());
		}

		/* Evaluate or copy this argument */

		if ((earg = (special) ? form : eval(form)) == NULL
		    || ERRORED(earg)) {
			/* Error or quit evaluating argument */

			return(earg);
		}

		/* Add the argument to the list */

		newargs = (special) ? NULL : add_form(newargs, earg);

		/* Now type-check the argument */

		if (!(earg->type & *spec)) {
			/* Argument is of invalid type */

			emsgl("Invalid arguments to function ",
			      funcname, NULL);
			free_forms(newargs);
			return(c_errored());
		}

		/* And update the argument specifier */

		spec = (*spec & FT_REPEATS) ? spec : spec + 1;
		repeating = (*spec & FT_REPEATS);
	}

	/* Check we have enough arguments */

	if (spec != NULL && *spec != FT_NULL &&
	    !repeating && !(*spec & FT_OPTIONAL)) {
		/* Too few arguments supplied */

		emsgl("Invalid argument count for function ",
		      funcname, NULL);
		free_forms(newargs);
		return(c_errored());
	}

	/* And copy the arguments for special forms */

	newargs = (special) ? copy_list(args) : newargs;

	/* Now return the evaluated arguments */

	return(newargs);
}
/****************************************************************************/
FORM *add_form(list, form)
FORM *list, *form;
{
	/* Append the form to the list */

	if (list == NULL) {
		return(form);
	}

	/* Append the form later in the list */

	list->next = add_form(list->next, form);
	return(list);
}
/****************************************************************************/
static FORM *copy_list(forms)
FORM *forms;
{
	/* Return a copy of a list of forms */

	FORM *form, *newforms = NULL;

	/* Now loop over each form in the list */

	for (form = forms; form != NULL; form = form->next) {
		/* Add this form to the new list */

		newforms = add_form(newforms, copy_form(form));
	}

	/* Return the new list */

	return(newforms);
}
/****************************************************************************/
static FORM *copy_form(form)
FORM *form;
{
	/* Return an allocated copy of a form */

	if (form->type == FT_LIST) {
		/* Build and return a new list */

		return(list(copy_list(form->value.list)));
	}

	/* Otherwise build and return a new atom */

	return(atom(make_seq(form->value.atom->keys, form->value.atom->len),
		    form->type));
}
/****************************************************************************/
void free_forms(forms)
FORM *forms;
{
	/* Free the entries in a list of forms */

	FORM *next;

	/* Loop over the entries and free them */

	while (forms != NULL) {
		/* Free the contents of the form */

		if (forms->type == FT_LIST) {
			free_forms(forms->value.list);
		} else {
			free_seq(forms->value.atom);
		}

		/* Now free the form itself */

		next = forms->next;
		free(forms);
		forms = next;
	}

	/* That's all folks */

	return;
}
/****************************************************************************/
FORM *cons(head, tail)
FORM *head, *tail;
{
	/* Make a list containing the two forms */

	return(list(add_form(head, tail)));
}
/****************************************************************************/
FORM *list(forms)
FORM *forms;
{
	/* Make a list containing the forms */

	FORM *form = NULL;

	/* First create the form itself */

	form = (FORM *) xmalloc(sizeof(FORM));
	form->type = FT_LIST;
	form->value.list = forms;
	form->next = NULL;

	/* And return the generated form */

	return(form);
}
/****************************************************************************/
FORM *atom(seq, type)
KEYSEQ *seq;
short type;
{
	/* Make an atom containing the key sequence */

	FORM *form;

	/* Build a form containing the atom */

	form = (FORM *) xmalloc(sizeof(FORM));
	form->type = type;
	form->value.atom = seq;
	form->next = NULL;

	/* And return the form */

	return(form);
}
/****************************************************************************/
FORM *atomstr(text, type)
char *text;
int type;
{
	/* Make an atom containing a verbatim copy of text */

	return(atom(make_seq(text, strlen(text)), type));
}
/****************************************************************************/
char *formtext(form)
FORM *form;
{
	/* Return the bare text of a form */

	static char *buf = NULL;

	/* Free any return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* What we return depends on the form's type */

	buf = xstrdup((form->type == FT_LIST) ? listtext(form)
		      : strseq(form->value.atom, SK_NONE));
	return(buf);
}
/****************************************************************************/
char *listtext(form)
FORM *form;
{
	/* Return the bare text of a list of forms */

	static char *buf = NULL;
	char *new, *old;
	FORM *f;

	/* Initialise the old buffer */

	old = (form != NULL) ? xstrdup("") : NULL;

	/* Now add the forms to the list */

	for (f = form; f != NULL; f = f->next) {
		/* Append the form to the buffer */

		new = vstrcat(old, (f == form) ? "" : " ",
			      formtext(f), NULL);
		free(old);
		old = new;
	}

	/* Free the return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* And return the list or nil as appropriate */

	return(buf = (old != NULL) ? old : xstrdup(""));
}
/****************************************************************************/
char *strform(form)
FORM *form;
{
	/* Return the read format of form */

	static char *buf = NULL;

	/* Free any return buffer */

	if (buf != NULL) {
		free(buf);
		buf = NULL;
	}

	/* How we print the form depends on it's type */

	switch(form->type) {
	case FT_LIST:				/* List object */
		return(buf = vstrcat("(", strlist(form->value.list),
				     ")", NULL));
	case FT_STRING:				/* String */
		return(buf = vstrcat("\"", strseq(form->value.atom,
						  SK_READSTR), "\"", NULL));
	case FT_NUMBER:				/* Number */
		return(buf = xstrdup(strseq(form->value.atom, SK_READNUM)));
	default:				/* Symbol */
		return(buf = xstrdup(strseq(form->value.atom, SK_READSYM)));
	}
	/*NOTREACHED*/
}
/****************************************************************************/
char *strlist(form)
FORM *form;
{
	/* Return the read format for a list of forms */

	static char *buf = NULL;
	char *new, *old;
	FORM *f;

	/* Initialise the old buffer */

	old = (form != NULL) ? xstrdup("") : NULL;

	/* Now add the forms to the list */

	for (f = form; f != NULL; f = f->next) {
		/* Append the form to the buffer */

		new = vstrcat(old, (f == form) ? "" : " ",
			      strform(f), NULL);
		free(old);
		old = new;
	}

	/* Free the return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* And return the list or nil as appropriate */

	return(buf = (old != NULL) ? old : xstrdup("nil"));
}
/****************************************************************************/
static int formcmp(form1, form2)
FORM *form1, *form2;
{
	/* Compare two forms and return TRUE if they're equal */

	int key;
	FORM *f1, *f2;

	/* First check the types of the forms */

	if (form1->type != form2->type) {
		return(FALSE);
	}

	/* Now check the forms if lists */

	if (form1->type == FT_LIST) {
		f1 = form1->value.list;
		f2 = form2->value.list;

		/* Compare each element of the lists */

		while (f1 != NULL && f2 != NULL) {
			if (!formcmp(f1, f2)) {
				return(FALSE);
			}
			f1 = f1->next;
			f2 = f2->next;
		}

		/* And check the lengths are the same */

		return(f1 == NULL && f2 == NULL);
	}
				
	/* Now check the lengths of the forms */

	if (form1->value.atom->len != form2->value.atom->len) {
		return(FALSE);
	}

	/* Now compare the atoms' values */

	for (key = 0; key < form1->value.atom->len; key++) {
		/* Check these keys match */

		if (form1->value.atom->keys[key] !=
		    form2->value.atom->keys[key]) {
			return(FALSE);
		}
	}

	/* The two atoms are equal */

	return(TRUE);
}
/****************************************************************************/
FUNCTION *find_function(funcname)
char *funcname;
{
	/* Return the FUNCTION entry relating to the named function */
	
	FUNCTION *func;

	/* Check the list of internal functions */

	for (func = functions; func->name != NULL; func++) {
		/* Is this the function we wanted? */

		if (!strcasecmp(func->name, funcname)) {
			return(func);
		}
	}

	/* No such function */

	return(NULL);
}
/****************************************************************************/
CONSTANT *find_constant(constname)
char *constname;
{
	/* Return the CONSTANT entry relating to the named constant */

	CONSTANT *constant;

	/* Check the list of internal constants */

	for (constant = constants; constant->name != NULL; constant++) {
		/* Is this the constant we wanted? */

		if (!strcasecmp(constant->name, constname)) {
			return(constant);
		}
	}

	/* No such constant */

	return(NULL);
}
/****************************************************************************/
void list_functions(rexpr, headings)
regex_t *rexpr;
int headings;
{
	/* Typeout the af functions (or those matching a regex) */

	FUNCTION *func;

	/* Loop through the functions */

	for (func = functions; !user_quit && func->name != NULL; func++) {
		/* Print the function if it matches the regex */

		if (rexpr == NULL || !regexec(rexpr, func->name, 0, NULL)) {
			/* Print a header if required */

			if (headings) {
				typeout("\nFunctions:\n");
				headings = FALSE;
			}

			/* And then print the function */

			typeout(func->name);
			typeout("\n");
		}
	}
	return;
}
/****************************************************************************/
CLIST *func_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of functions or commands completing base */

	FUNCTION *func;

	/* First add the commands to the list */

	list = cmd_complete(list, base);

	/* Now add all the function values to the list */

	for (func = functions; func->name != NULL; func++) {
		/* Does this function match the base? */

		if (!strncasecmp(base, func->name, strlen(base))) {
			list = add_clist(list, func->name, FALSE);
		}
	}

	/* And return the list */

	return(list);
}
/****************************************************************************/
FORM *f_set(forms)
FORM *forms;
{
	/* Set a variable to the value specified in forms */

	/* Set the variable and check value */

	if (!fset_var(formtext(forms), forms->next)) {
		/* Error setting value; fail */

		return(c_errored());
	}
		
	/* And return the value of the variable */

	return(copy_form(forms->next));
}
/****************************************************************************/
FORM *f_setq(forms)
FORM *forms;
{
	/* Set a quoted variable to the value specified in forms */

	FORM *value;

	/* First evaluate the value */

	if ((value = eval(forms->next)) == NULL) {
		/* Error evaluating the value */

		return(c_errored());
	}

	/* Now set the variable and check value */

	if (!fset_var(formtext(forms), value)) {
		/* Error setting value; fail */

		free_forms(value);
		return(c_errored());
	}
		
	/* And return the value of the variable */

	return(value);
}
/****************************************************************************/
FORM *f_defkey(forms)
FORM *forms;
{
	/* Bind a key in a specific keymap */

	char *map, *binding;
	KEYSEQ *keys;
	FORM *status;

	/* Get the arguments into strings */

	map = xstrdup(formtext(forms));
	binding = (NIL(forms->next->next)) ? NULL :
		xstrdup(formtext(forms->next->next));
	keys = forms->next->value.atom;

	/* Bind the key and return status */

	status = (set_key(map, keys, binding)) ?
		copy_form(forms->next->next) : c_errored();

	/* Free space and return status */

	free(map);
	free(binding);
	return(status);
}
/****************************************************************************/
FORM *f_defmac(forms)
FORM *forms;
{
	/* Define a named keyboard macro */

	return(named_macro(formtext(forms), NIL(forms->next) ?
			   NULL : forms->next->value.atom)
	       ? copy_form(forms->next) : c_errored());
}
/****************************************************************************/
FORM *f_quote(forms)
FORM *forms;
{
	/* Quote a form, returning it as is */

	return(copy_form(forms));
}
/****************************************************************************/
FORM *f_progn(forms)
FORM *forms;
{
	/* Set a variable to the value specified in forms */

	FORM *f, *last = NULL;

	/* Loop over each form in the list */

	for (f = forms; f != NULL; f = f->next) {
		/* Set the last form to this form */

		last = f;
	}

	/* Return a copy of the last form */

	return((last != NULL) ? copy_form(last) : NULL);
}
/****************************************************************************/
FORM *f_if(forms)
FORM *forms;
{
	/* Evaluate the second form if the first is non-nil */

	FORM *value;

	/* Evaluate the first form first */

	if ((value = eval(forms)) != NULL && !NIL(value)) {
		/* Free the return and evaluate the second form */

		free_forms(value);
		value = eval(forms->next);
	} else if (value != NULL && NIL(value) && forms->next->next != NULL) {
		/* Free the return and evaluate the third form */

		free_forms(value);
		value = eval(forms->next->next);
	}

	/* Return the value of the expression */

	return(value);
}
/****************************************************************************/
FORM *f_and(forms)
FORM *forms;
{
	/* Return t if all forms are non-nil, nil otherwise */

	FORM *form, *value;

	/* Evaluate the forms in order */

	for (form = forms; form != NULL; form = form->next) {
		if ((value = eval(form)) == NULL) {
			/* Error evaluating the form */

			return(c_errored());
		} else if (NIL(value)) {
			/* This form is nil */

			free_forms(value);
			return(c_nil());
		}
		
		/* Free the returned value */

		free_forms(value);
	}

	/* Condition true, return t */

	return(c_t());
}
/****************************************************************************/
FORM *f_or(forms)
FORM *forms;
{
	/* Return t if either form is non-nil, nil otherwise */

	FORM *form, *value;

	/* Evaluate the forms in order */

	for (form = forms; form != NULL; form = form->next) {
		if ((value = eval(form)) == NULL || ERRORED(value)) {
			/* Error or quit evaluating the form */

			return(value);
		} else if (!NIL(value)) {
			/* This form is non-nil */

			free_forms(value);
			return(c_t());
		}

		/* Free the returned value */

		free_forms(value);
	}

	/* Condition false, return nil */

	return(c_nil());
}
/****************************************************************************/
FORM *f_not(forms)
FORM *forms;
{
	/* Return t if the form is nil, nil otherwise */

	return(NIL(forms) ? c_t() : c_nil());
}
/****************************************************************************/
FORM *f_equal(forms)
FORM *forms;
{
	/* Return t if the two forms are equal */

	return((formcmp(forms, forms->next)) ? c_t() : c_nil());
}
/****************************************************************************/
FORM *f_message(forms)
FORM *forms;
{
	/* Display the supplied forms in the echo area */

	msg(strlist(forms));
	return(copy_form(forms));
}
/****************************************************************************/
FORM *f_error(forms)
FORM *forms;
{
	/* Beep and display the supplied forms in the echo area */

	emsg(strlist(forms));
	return(c_errored());
}
/****************************************************************************/
FORM *f_getenv(forms)
FORM *forms;
{
	/* Return the value of the named environment variable */

	char *value;

	/* Get the value from the environment */

	if ((value = getenv(strseq(forms->value.atom, SK_READSTR))) == NULL) {
		/* No such value; return nil */

		return(c_nil());
	}

	/* Return the value as a string */

	return(atomstr(value, FT_STRING));
}
/****************************************************************************/
FORM *f_system(forms)
FORM *forms;
{
	/* Run a shell command and return it's output */

	char *output, *canon;
	char *start, *end, *p, *q;
	FORM *value;

	/* Get the output from the command */

	if ((output = syscmd(strseq(forms->value.atom,
				   SK_READSTR))) == NULL) {
		/* No such value; return nil */

		return(c_nil());
	}

	/* Skip leading spaces in the output */

	for (start = output; isspace(*start); start++) {
		/* NULL LOOP */
	}

	/* And trailing spaces and the newline */

	end = start + strlen(start);
	while (end > start && isascii(*(end - 1))
	       && isspace(*(end - 1))) {
		end--;
	}

	/* Allocate space for the canonical output and fill it */

	q = canon = xmalloc(end - start + 1);
	p = start;

	while (p < end) {
		if (isspace(*p)) {
			/* Simply include a canonical space */

			*q++ = ' ';
			while (isspace(*(p + 1))) {
				p++;
			}
		} else {
			/* Just copy the character */

			*q++ = *p++;
		}
	}
	*q = '\0';

	/* Set the function value as a string */

	value = atomstr(canon, FT_STRING);

	/* Clean up and return the value */

	free(output);
	free(canon);
	return(value);
}
/****************************************************************************/
FORM *c_t()
{
	/* Return a form containing t */

	return(atomstr("t", FT_SYMBOL));
}
/****************************************************************************/
FORM *c_a()
{
	/* Return a form containing a */

	return(atomstr("a", FT_SYMBOL));
}
/****************************************************************************/
FORM *c_nil()
{
	/* Return a form containing nil */

	return(list(NULL));
}
/****************************************************************************/
FORM *c_errored()
{
	/* Return a error-marking form */

	return(atom(NULL, FT_ERRORED));
}
/****************************************************************************/
FORM *c_version()
{
	/*
	 * Return a string containing af's version.  This really
	 * should be a variable, but since we don't have those
	 * yet it's a constant.  So sue me.
	 */

	return(atomstr(VERSION, FT_STRING));
}
/****************************************************************************/
