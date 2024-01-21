/* Variable.c - Variable handling functions for af.
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
#include <regex.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "tags.h"
#include "complete.h"
#include "varlist.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: variable.c,v 1.51 1997/03/05 21:23:45 malc Exp $";
static char *VariableId = VARIABLEID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xstrdup(), *vstrcat();
extern char *getenv(), *get_dstr(), *get_estr();
extern char *get_dcstr(), *get_ecstr(), *get_host();
extern char *get_home(), *get_realname(), *expand();
extern char *alias(), *canonical(), *realname();
extern char *a_strerror(), *strkey(), *strform();
extern char *formtext(), *strcanon(), *strtags();
extern char *unmessage(), *utos();
extern int atoi(), strcasecmp(), strncasecmp();
extern int get_key(), set_length(), retextual();
extern unsigned alarm();
extern void free(), msg(), cmsg(), emsg();
extern void emsgl(), typeout(), table();
extern void display(), free_tlist();
extern FORM *atomstr(), *list(), *add_form();
extern TAG_LIST *taglist();
extern CLIST *fn_complete(), *sort_complete();
extern CLIST *add_clist();

/* Local function declarations */

VARIABLE *find_variable();
static void set_deflt();
static CLIST *vb_complete(), *vt_complete();
static CLIST *ve_complete();

/****************************************************************************/
/* Import the current window and user quit flag from commands.c */

extern WINDOW *cwin;
extern int user_quit;

/****************************************************************************/
void init_vars()
{
	/* Initialise the variable list */

	char *sigfile, *folder;
	VARIABLE *var;

	/* Set the default mail domain */

	set_deflt(find_variable(V_DOMAIN), get_host());

	/* Set the default real name and organization */

	set_deflt(find_variable(V_REALNAME), get_realname());
	set_deflt(find_variable(V_ORG), getenv(ORGANIZATION));

	/* Set up the user's preferred editor */

	set_deflt(find_variable(V_EDITOR), getenv(VISUAL));
	set_deflt(find_variable(V_EDITOR), getenv(EDITOR));

	/* Set the default path for af libraries */

	set_deflt(find_variable(V_LOADPATH), getenv(AFLOADPATH));
	set_deflt(find_variable(V_LOADPATH), AFLIBDIR);

	/* Now set up the default mail folder */

	if ((folder = getenv(FOLDER)) == NULL) {
		folder = vstrcat(get_home(NULL), "/", DEFMAILDIR, NULL);
		set_deflt(find_variable(V_FOLDER), folder);
		free(folder);
	} else {
		set_deflt(find_variable(V_FOLDER), folder);
	}

	/* And set up the default news folder */

	if ((folder = getenv(SAVEDIR)) == NULL) {
		folder = vstrcat(get_home(NULL), "/", DEFNEWSDIR, NULL);
		set_deflt(find_variable(V_NEWSFOLDER), folder);
		free(folder);
	} else {
		set_deflt(find_variable(V_NEWSFOLDER), folder);
	}
		
	/* Make the default signature file and set it */

	sigfile = vstrcat(get_home(NULL), "/.signature", NULL);
	set_deflt(find_variable(V_SIGFILE), sigfile);
	free(sigfile);

	/* Now we need to set the actual values from the defaults */

	for (var = variables; var->name != NULL; var++) {
		(void) var->setfunc(var, var->deflt);
	}

	return;
}
/****************************************************************************/
void reset_var(varname, text)
char *varname, *text;
{
	/* Update the value of a variable to text */

	VARIABLE *var;

	/* Get the details of the variable */

	if ((var = find_variable(varname)) != NULL) {
		var->setfunc(var, text);
	}

	return;
}
/****************************************************************************/
int set_var(varname)
char *varname;
{
	/* Get a new value for the variable and set it */

	char *prompt, *text;
	VARIABLE *var;

	/* Get the details of the variable */

	if ((var = find_variable(varname)) == NULL) {
		emsgl("Variable ", varname, " does not exist", NULL);
		return(FALSE);
	}

	/* Get the new value for the variable */

	prompt = vstrcat("Set variable: ", var->name, " To: ", NULL);
	text = var->getfunc(var, prompt);
	free(prompt);

	/* Set the variable and return status */

	return((user_quit) ? FALSE : var->setfunc(var, text));
}
/****************************************************************************/
int fset_var(varname, form)
char *varname;
FORM *form;
{
	/* Set a variable from an afl form */

	char *text;
	VARIABLE *var;

	/* Get the details of the variable */

	if ((var = find_variable(varname)) == NULL) {
		emsgl("Variable ", varname, " does not exist", NULL);
		return(FALSE);
	}

	/* Get the value to set the variable to */

	if ((text = var->formfunc(form)) == NULL) {
		/* The form is invalid for this variable */

		emsgl("Invalid value ", strform(form),
		      " for variable ", var->name, NULL);
		return(FALSE);
	}

	/* Set or unset the variable and return status */

	return(var->setfunc(var, (*text != '\0') ? text : NULL));
}
/****************************************************************************/
char *get_vtext(varname)
char *varname;
{
	/* Return the text of the specified variable */

	VARIABLE *var;

	/* Get the variable and return the text */

	var = find_variable(varname);
	return((var != NULL) ? var->text : NULL);
}
/****************************************************************************/
int get_vval(varname)
char *varname;
{
	/* Return the value of the specified variable */

	VARIABLE *var;

	/* Get the variable and return the value */

	var = find_variable(varname);
	return((var != NULL) ? var->value : 0);
}
/****************************************************************************/
FORM *get_vform(varname)
char *varname;
{
	/* Return a form containing the variable's value */

	VARIABLE *var;

	/* First find the variable */

	if ((var = find_variable(varname)) == NULL) {
		emsgl("Unknown variable ", varname, NULL);
		return(c_errored());
	}

	/* Use the evaluating function to set the return */

	return(var->evalfunc(var));
}
/****************************************************************************/
char *strvar(varname)
char *varname;
{
	/* Return a variable's printable text */

	VARIABLE *var;

	/* Get the details of the variable */

	if ((var = find_variable(varname)) == NULL) {
		return(NULL);
	}

	/* The print function returns the text in a static buffer */

	return(var->printfunc(var, FALSE));
}
/****************************************************************************/
static void set_deflt(var, deflt)
VARIABLE *var;
char *deflt;
{
	/* Set the default for a variable if there isn't one yet */

	if (var->deflt == NULL && deflt != NULL) {
		var->deflt = xstrdup(deflt);
	}
	return;
}
/****************************************************************************/
/* Routines to get values for variables */
/****************************************************************************/
static char *vg_string(var, prompt)
VARIABLE *var;
char *prompt;
{
	/* Return a value suitable for setting a string variable */

	return(get_estr(NULL, prompt, var->text));
}
/****************************************************************************/
static char *vg_file(var, prompt)
VARIABLE *var;
char *prompt;
{
	/* Return a value suitable for setting a file variable */

	return(get_ecstr(NULL, prompt, var->text, fn_complete, C_PERMISSIVE));
}
/****************************************************************************/
static char *vg_sort(var, prompt)
VARIABLE *var;
char *prompt;
{
	/* Return a value suitable for setting a sort variable */

	return(get_ecstr(NULL, prompt, var->text, sort_complete, C_STRICT));
}
/****************************************************************************/
/*ARGSUSED*/
static char *vg_char(var, prompt)
VARIABLE *var;
char *prompt;
{
	/* Return a value suitable for setting a char variable */

	static char buf[2];
	int key;

	/* Get the key */

	msg(prompt);
	key = get_key();
	cmsg(strkey(key, SK_KEYSEQ));

	/* Set and return the text string */

	(void) sprintf(buf, "%c", key);
	return(buf);
}
/****************************************************************************/
static char *vg_boolean(var, prompt)
VARIABLE *var;
char *prompt;
{
	/* Return a value suitable for setting a boolean variable */

	return(get_dcstr(NULL, prompt, var->text, vb_complete, C_STRICT));
}
/****************************************************************************/
static char *vg_tristate(var, prompt)
VARIABLE *var;
char *prompt;
{
	/* Return a value suitable for setting a tristate variable */

	return(get_dcstr(NULL, prompt, var->text, vt_complete, C_STRICT));
}
/****************************************************************************/
static char *vg_edithdrs(var, prompt)
VARIABLE *var;
char *prompt;
{
	/* Return a value suitable for setting an edithdrs variable */

	return(get_dcstr(NULL, prompt, var->text, ve_complete, C_STRICT));
}
/****************************************************************************/
static char *vg_numeric(var, prompt)
VARIABLE *var;
char *prompt;
{
	/* Return a value suitable for setting a numeric variable */

	return(get_dstr(NULL, prompt, var->text));
}
/****************************************************************************/
/* Functions to check variable form values */
/****************************************************************************/
static char *vf_string(form)
FORM *form;
{
	/* Check a string form and convert it to a string */

	char *nullstring = "";
	int key;

	/* The form must be a string or nil */

	if (form->type != FT_STRING && !NIL(form)) {
		return(NULL);
	}

	/* Handle nil right here */

	if (NIL(form)) {
		return(nullstring);
	}

	/* Check for null characters in the value */

	for (key = 0; key < form->value.atom->len; key++) {
		if (form->value.atom->keys[key] == '\0') {
			return(NULL);
		}
	}

	/* Set and return the buffer */

	return(formtext(form));
}
/****************************************************************************/
static char *vf_char(form)
FORM *form;
{
	/* Check a character form and convert it to a string */

	int value;

	/* The form must be a number or string */

	if (form->type != FT_NUMBER &&
	    (form->type != FT_STRING || form->value.atom->len != 1)) {
		return(NULL);
	}

	/* Get the numeric value of the variable */

	value = (form->type == FT_NUMBER) ? atoi(formtext(form))
		: (unsigned) form->value.atom->keys[0];

	/* Set and return the buffer */

	return(strkey(value, SK_NONE));
}
/****************************************************************************/
static char *vf_tristate(form)
FORM *form;
{
	/* Check a tristate form and convert it to a string */

	static char *t = "t";
	static char *a = "a";
	static char *nil = "f";
	
	/* Simply return the canonical value */

	if (NIL(form)) {
		return(nil);
	} else if (A(form)) {
		return(a);
	}

	/* Any other value implies t */

	return(t);
}
/****************************************************************************/
static char *vf_numeric(form)
FORM *form;
{
	/* Check a numeric form and convert it to a string */

	int value;

	/* The form must be a number */

	if (form->type != FT_NUMBER) {
		return(NULL);
	}

	/* Get the numeric value of the variable */

	value = atoi(formtext(form));
	value = (value < 0) ? 0 : value;

	/* And return the numeric value as a string */

	return(utos((unsigned) value));
}
/****************************************************************************/
static char *vf_list(form)
FORM *form;
{
	/* Check a list form and convert it to a string */

	char *string, *new, *text;
	FORM *f;

	/* The form must be a list */

	if (form->type != FT_LIST) {
		return(NULL);
	}

	/* Initialise the string */

	string = xstrdup("");

	/* Now add the forms to the string */

	for (f = form->value.list; f != NULL; f = f->next) {
		/* This form must be a string */

		if (f->type != FT_STRING) {
			free(string);
			return(NULL);
		}

		/* Get the text value of this form */

		text = formtext(f);

		/* Colons are not valid in list elements */

		if (strchr(text, ':') != NULL) {
			free(string);
			return(NULL);
		}

		/* Add the element to the string */

		new = vstrcat(string, (f == form->value.list)
			      ? "" : ":", text, NULL);
		free(string);
		string = new;
	}

	/* Now return the generated string */

	return(string);
}
/****************************************************************************/
/* Functions to set variables */
/****************************************************************************/
static int vs_string(var, text)
VARIABLE *var;
char *text;
{
	/* Set the variable to the string in text */

	/* Free any existing text */

	if (var->text != NULL) {
		free(var->text);
	}

	/* Set the variable's text and zero the value */

	var->text = (text != NULL) ? xstrdup(text) : NULL;
	var->value = 0;

	return(TRUE);
}
/****************************************************************************/
static int vs_nonnull(var, text)
VARIABLE *var;
char *text;
{
	/* Set the variable to a non-null string */

	/* Can't unset the text */

	if (text == NULL) {
		emsgl("Can't unset ", var->name, NULL);
		return(FALSE);
	}

	/* Set the variable as a string */

	(void) vs_string(var, text);
	return(TRUE);
}
/****************************************************************************/
static int vs_file(var, text)
VARIABLE *var;
char *text;
{
	/* Set the variable to the file named in text */

	char *fname;

	/* Handle unsetting the variable */

	if (text == NULL) {
		return(vs_string(var, text));
	}

	/* Expand the file name */

	fname = expand(text);

	/* Set the variable as a string */

	(void) vs_string(var, fname);
	free(fname);

	return(TRUE);
}
/****************************************************************************/
static int vs_format(var, text)
VARIABLE *var;
char *text;
{
	/* Set the format variable var to text */

	MESSAGE *message;

	/* Get the message to unformat */

	message = (cwin != NULL) ? cwin->point : NULL;

	/* Check the format by expanding it */

	if (text != NULL && unmessage(message, text, 0) == NULL) {
		return(FALSE);
	}

	/* Update the variable as a string */

	(void) vs_string(var, text);
	return(TRUE);
}
/****************************************************************************/
static int vs_address(var, text)
VARIABLE *var;
char *text;
{
	/* Set the variable to the address in text */

	char *addr;

	/* Handle unsetting the variable */

	if (text == NULL) {
		return(vs_string(var, text));
	}

	/* Alias and expand the address */

	if ((addr = alias(text)) == NULL) {
		emsg(a_strerror());
		return(FALSE);
	}

	/* Set the variable as a string */

	(void) vs_string(var, addr);
	free(addr);

	return(TRUE);
}
/****************************************************************************/
static int vs_name(var, text)
VARIABLE *var;
char *text;
{
	/* Set the variable to the name in text */

	char *name;

	/* Handle unsetting the variable */

	if (text == NULL) {
		return(vs_string(var, text));
	}

	/* Expand the name */

	if ((name = realname(text)) == NULL) {
		emsgl("Invalid name ", text, NULL);
		return(FALSE);
	}

	/* Set the variable as a string */

	(void) vs_string(var, name);
	free(name);

	return(TRUE);
}
/****************************************************************************/
static int vs_domain(var, text)
VARIABLE *var;
char *text;
{
	/* Set the variable to the domain in text */

	char *addr, *canon;

	/* Can't unset the text */

	if (text == NULL) {
		emsgl("Can't unset ", var->name, NULL);
		return(FALSE);
	}

	/* Convert the domain to an address and check it */

	addr = vstrcat("malc@", text, NULL);
	if ((canon = canonical(addr)) == NULL) {
		emsgl("Invalid domain ", text, NULL);
		free(addr);
		return(FALSE);
	}
	free(addr);
	free(canon);

	/* Set the variable as a string */

	(void) vs_string(var, text);
	return(TRUE);
}
/****************************************************************************/
static int vs_tags(var, text)
VARIABLE *var;
char *text;
{
	/* Set the variable to the tag list in text */

	char *tags;
	TAG_LIST *tlist;

	/* Handle unsetting the variable */

	if (text == NULL) {
		return(vs_string(var, text));
	}

	/* Translate text into a tag list */

	if ((tlist = taglist(text, TL_SET)) == NULL) {
		return(FALSE);
	}

	/* You can't make the default tag persistent */

	if (DEFTAG_USED(tlist)) {
		emsg("Can't make '+' tag persistent");
		free_tlist(tlist);
		return(FALSE);
	}

	/* Make a string from the tag list */

	tags = strtags(tlist);
	free_tlist(tlist);

	/* Set the variable as a string */

	(void) vs_string(var, tags);
	free(tags);

	return(TRUE);
}
/****************************************************************************/
static int vs_char(var, text)
VARIABLE *var;
char *text;
{
	/* Set the variable to the character in text */

	/* You can't set char variables to metachars */

	if (strlen(text) != 1 || !isascii(*text)) {
		emsgl("Invalid prefix character ",
		      strcanon(text, SK_KEYSEQ));
		return(FALSE);
	}
		
	/* Set the variable's text and value */

	(void) vs_string(var, text);
	var->value = *text;

	return(TRUE);
}
/****************************************************************************/
static int vs_numeric(var, text)
VARIABLE *var;
char *text;
{
	/* Set the numeric variable to the value in text */

	int value;

	/* Get the value, then set the variable */

	value = atoi(text);
	value = (value < 0) ? 0 : value;
	(void) vs_string(var, utos((unsigned) value));
	var->value = value;

	return(TRUE);
}
/****************************************************************************/
static int vs_timer(var, text)
VARIABLE *var;
char *text;
{
	/* Set the timer variable to the value in text */

	int old_value, value, offset;
	unsigned timer;

	/* Get the old and new values */

	old_value = var->value;
	value = atoi(text);

	/* Check for value being too low */

	if (value > 0 && value < MINRESYNC) {
		value = MINRESYNC;
	}

	/* Set the variable */

	(void) vs_string(var, utos((unsigned) value));
	var->value = value;

	/* Update the alarm timer if required */

	if (value == 0 || old_value == 0) {
		(void) alarm((unsigned) value);
	} else if (timer = alarm(0)) {
		offset = value - old_value;
		timer = ((int) timer + offset > 0) ? timer + offset : 1;
		(void) alarm(timer);
	}

	return(TRUE);
}
/****************************************************************************/
static int vs_echo(var, text)
VARIABLE *var;
char *text;
{
	/* Set the echo variable to the value in text */

	int value;

	/* Get the value checking it's value */

	if ((value = atoi(text)) > MAXECHO) {
		value = MAXECHO;
	}

	/* Now set the variable */

	(void) vs_string(var, utos((unsigned) value));
	var->value = value;

	return(TRUE);
}
/****************************************************************************/
static int vs_boolean(var, text)
VARIABLE *var;
char *text;
{
	/* Set the boolean variable var from the given text */

	switch (*text) {
	case 't':
		(void) vs_string(var, VI_TRUE);
		var->value = V_TRUE;
		return(TRUE);
	case 'f':
		(void) vs_string(var, VI_FALSE);
		var->value = V_FALSE;
		return(TRUE);
	default:
		emsgl(text , " invalid for variable ", var->name, NULL);
		return(FALSE);
	}
	/*NOTREACHED*/
}
/****************************************************************************/
static int vs_tristate(var, text)
VARIABLE *var;
char *text;
{
	/* Set the tristate variable var from the given text */

	switch (*text) {
	case 'a':
		(void) vs_string(var, VI_ASK);
		var->value = V_ASK;
		return(TRUE);
	default:
		return(vs_boolean(var, text));
	}
	/*NOTREACHED*/
}
/****************************************************************************/
static int vs_edithdrs(var, text)
VARIABLE *var;
char *text;
{
	/* Set the edit-initial-headers variable from the given text */

	switch (*text) {
	case 'a':
		(void) vs_string(var, VI_ACCEPT);
		var->value = V_ASK;
		return(TRUE);
	default:
		return(vs_boolean(var, text));
	}
}
/****************************************************************************/
static int vs_arrow(var, text)
VARIABLE *var;
char *text;
{
	/* Set the header-line-arrow variable var to text */

	/* Can't unset the text */

	if (text == NULL) {
		emsgl("Can't unset ", var->name, NULL);
		return(FALSE);
	}

	/* Update the variable as a string */

	(void) vs_string(var, text);

	/* And redisplay any active window */

	if (cwin != NULL) {
		display(cwin);
	}
	return(TRUE);
}
/****************************************************************************/
static int vs_hdr_format(var, text)
VARIABLE *var;
char *text;
{
	/* Set the header-line-format variable var to text */

	WINDOW *w;

	/* Can't unset the text */

	if (text == NULL) {
		emsgl("Can't unset ", var->name, NULL);
		return(FALSE);
	}

	/* Check the format by recalculating length */

	if (!set_length(text, FALSE)) {
		return(FALSE);
	}

	/* Update the variable as a string */

	(void) vs_string(var, text);

	/* And update any active windows */

	if ((w = cwin) != NULL) {
		do {
			display(w);
			w = w->next;
		} while (w != cwin);
	}

	return(TRUE);
}
/****************************************************************************/
static int vs_mode_format(var, text)
VARIABLE *var;
char *text;
{
	/* Set the mode-line-format variable var to text */

	WINDOW *w;

	/* Can't unset the text */

	if (text == NULL) {
		emsgl("Can't unset ", var->name, NULL);
		return(FALSE);
	}

	/* Check the format by recalculating length */

	if (!set_length(text, TRUE)) {
		return(FALSE);
	}

	/* Update the variable as a string */

	(void) vs_string(var, text);

	/* And update any active windows */

	if ((w = cwin) != NULL) {
		do {
			display(w);
			w = w->next;
		} while (w != cwin);
	}

	return(TRUE);
}
/****************************************************************************/
static int vs_localtime(var, text)
VARIABLE *var;
char *text;
{
	/* Update the show-dates-in-local-times variable */

	WINDOW *w;

	/* Update the variable as a boolean */

	if (!vs_boolean(var, text)) {
		/* Error setting the variable */

		return(FALSE);
	}

	/* And update any active windows */

	if ((w = cwin) != NULL) {
		do {
			display(w);
			w = w->next;
		} while (w != cwin);
	}

	return(TRUE);
}	
/****************************************************************************/
static int vs_viewable(var, text)
VARIABLE *var;
char *text;
{
	/* Set the viewable-charsets variable var to text */

	WINDOW *w;

	/* Update the variable as a string */

	(void) vs_string(var, text);

	/* And reset the MIME flag on messages */

	if ((w = cwin) != NULL && retextual(cwin->buf)) {
		/* Redisplay all the windows */

		do {
			display(w);
			w = w->next;
		} while (w != cwin);
	}

	return(TRUE);
}
/****************************************************************************/
/* Functions used to print variables */
/****************************************************************************/
static char *vp_string(var, readform)
VARIABLE *var;
int readform;
{
	/*
	 * Return the printable text of a string variable in a
	 * static buffer.  Readform specifies that we should
	 * return the afl read form, rather then display form.
	 */

	static char *buf = NULL;
	int form;

	/* Free any old return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* Set up the form of output we want */

	form = (readform) ? SK_READSTR : SK_STRING;

	/* Generate and return the canonical value */

	buf = vstrcat("\"", (var->text != NULL) ?
		      strcanon(var->text, form) : "", "\"", NULL);
	return(buf);
}
/****************************************************************************/
static char *vp_char(var, readform)
VARIABLE *var;
int readform;
{
	/*
	 * Return the printable text of a char variable in a
	 * static buffer.  Readform specifies that we should
	 * return the afl read form, rather then display form.
	 */

	static char *buf = NULL;

	/* Free any old return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* Generate and return the canonical value */

	buf = (!readform) ? xstrdup(strcanon(var->text, SK_KEYSEQ)) :
		vstrcat("\"", strcanon(var->text, SK_READKEY), NULL);
	return(buf);
}
/****************************************************************************/
/*ARGSUSED*/
static char *vp_numeric(var, readform)
VARIABLE *var;
int readform;
{
	/*
	 * Return the printable text of a numeric variable in a
	 * static buffer.  Readform is ignored, since afl read
	 * form for numerics is identical to display form.
	 */

	return(var->text);
}
/****************************************************************************/
static char *vp_tristate(var, readform)
VARIABLE *var;
int readform;
{
	/*
	 * Return the printable text of a boolean or tristate
	 * variable in a static buffer.  Readform specifies that
	 * we should return the afl read form, rather then display
	 * form.
	 */

	/* Print a boolean or tristate variable */

	static char *readconst[] = { "nil", "t", "a" };

	/* Generate and return the canonical value */

	return((readform) ? readconst[var->value] : var->text);
}
/****************************************************************************/
/* Functions used to evaluate variables */
/****************************************************************************/
static FORM *ve_string(var)
VARIABLE *var;
{
	/* Evaluate a string variable */

	return((var->text != NULL) ? atomstr(var->text, FT_STRING) : c_nil());
}
/****************************************************************************/
static FORM *ve_char(var)
VARIABLE *var;
{
	/* Evaluate a character variable */

	return(atomstr(var->text, FT_STRING));
}
/****************************************************************************/
static FORM *ve_numeric(var)
VARIABLE *var;
{
	/* Evaluate a numeric variable */

	return(atomstr(var->text, FT_NUMBER));
}
/****************************************************************************/
static FORM *ve_tristate(var)
VARIABLE *var;
{
	/* Evaluate a boolean or tristate variable */

	return(atomstr(vp_tristate(var, TRUE), FT_SYMBOL));
}
/****************************************************************************/
static FORM *ve_list(var)
VARIABLE *var;
{
	/*
	 * Evaluate a list variable.  Assumes that all the
	 * elements of the list are strings, which is true
	 * so far, at least.
	 */

	char *start, *colon, *element;
	FORM *varlist = NULL;

	/* Set up the text we're to evaluate */

	if ((start = var->text) == NULL) {
		/* No elements, return nil */

		return(c_nil());
	}

	/* Loop over the elements in the path */

	while ((colon = strchr(start, ':')) != NULL) {
		/* Copy the path element out of the list */

		element = xmalloc(colon - start + 1);
		(void) strncpy(element, start, colon - start);
		element[colon - start] = '\0';

		/* Add this element to the list */

		varlist = add_form(varlist, atomstr(element, FT_STRING));
		free(element);

		/* And move on to the next element */

		start = colon + 1;
	}

	/* Now add the last element to the path and return the list */

	varlist = add_form(varlist, atomstr(start, FT_STRING));
	return(list(varlist));
}
/****************************************************************************/
VARIABLE *find_variable(varname)
char *varname;
{
	/* Return the details of the named variable */

	VARIABLE *v;

	/* Search for the variable in the list */

	for (v = variables; v->name != NULL; v++) {
		/* Is this the variable we want? */

		if (!strcasecmp(v->name, varname)) {
			return(v);
		}
	}

	/* No such variable */

	return(NULL);
}
/****************************************************************************/
void list_vars(rexpr, headings)
regex_t *rexpr;
int headings;
{
	/* Typeout the af variables (or those matching a regex) */

	VARIABLE *v;

	for (v = variables; !user_quit && v->name != NULL; v++) {
		/* Should this variable be listed? */

		if (rexpr == NULL || !regexec(rexpr, v->name, 0, NULL, 0)) {
			if (headings) {
				typeout("\nVariables:\n");
				headings = FALSE;
			}
			table(v->name, v->printfunc(v, FALSE));
		}
	}

	return;
}
/****************************************************************************/
int user_vars(fp)
FILE *fp;
{
	/* Write definitions for any non-default variables to fp */

	char *def, *name;
	char *quote, *val;
	int found = FALSE;
	VARIABLE *v;
	FORM *form;

	/* Scan each variable in turn */

	for (v = variables; v->name != NULL; v++) {
		/* Get the default and current value */

		def = v->deflt;
		val = v->text;

		/* Check if the variable is not set to the default */

		if (val != NULL && def == NULL || val == NULL && def != NULL
		    || val != NULL && def != NULL && strcmp(val, def)) {
			/* Output a comment if first found */

			if (!found) {
				if (fputs("\n; Variables\n", fp) == EOF) {
					return(FALSE);
				}
				found = TRUE;
			}

			/* Set up the variable's name and value */

			name = strcanon(v->name, SK_READSYM);
			form = v->evalfunc(v);
			quote = (form->type == FT_LIST) ? "'" : "";
			val = strform(form);

			/* Output the variable definition */

			if (fprintf(fp, "(setq %s %s%s)\n",
				    name, quote, val) == EOF) {
				/* Error writing the file */

				return(FALSE);
			}
		}
	}

	/* All written ok */

	return(TRUE);
}
/****************************************************************************/
CLIST *var_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of variable names completing base */

	VARIABLE *v;

	/* Build the list of possible values */

	for (v = variables; v->name != NULL; v++) {
		/* Does this variable complete the base? */

		if (!strncasecmp(base, v->name, strlen(base))) {
			list = add_clist(list, v->name, FALSE);
		}
	}

	return(list);
}
/****************************************************************************/
static CLIST *vb_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of boolean values completing base */

	if (!strncasecmp(base, VI_FALSE, strlen(base))) {
		list = add_clist(list, VI_FALSE, FALSE);
	}
	if (!strncasecmp(base, VI_TRUE, strlen(base))) {
		list = add_clist(list, VI_TRUE, FALSE);
	}

	return(list);
}
/****************************************************************************/
static CLIST *vt_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of tristate values completing base */

	if (!strncasecmp(base, VI_ASK, strlen(base))) {
		list = add_clist(list, VI_ASK, FALSE);
	}

	/* Can also be a boolean value */

	return(vb_complete(list, base));
}
/****************************************************************************/
static CLIST *ve_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of edithdrs values completing base */

	if (!strncasecmp(base, VI_ACCEPT, strlen(base))) {
		list = add_clist(list, VI_ACCEPT, FALSE);
	}

	/* Can also be a boolean value */

	return(vb_complete(list, base));
}
/****************************************************************************/
