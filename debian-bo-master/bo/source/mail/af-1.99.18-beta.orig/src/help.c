/* Help.c - Handling of help entries for af.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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
#include "complete.h"
#include "macros.h"
#include "version.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: help.c,v 1.17 1996/08/28 17:44:08 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *strerror(), *vstrcat(), *strvar();
extern char *strseq(), *bindings();
extern int strcasecmp(), strncasecmp();
extern int set_typeout_file(), error_in_typeout();
extern void free(), emsgl(), typeout(), showtext();
extern CLIST *add_clist();

/* Local function declarations */

int describe();
static int find_help();
static void help_title();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the user quit flag from commands.c */

extern int user_quit;

/****************************************************************************/
int describe(type, name, show)
char *type, *name;
int show;
{
	/* Display help on name to typeout */

	char buf[BUFSIZ];
	FILE *fp;

	/* Open the help file */

	if ((fp = fopen(HELPFILE, "r")) == NULL) {
		/* This implies a broken installation */

		emsgl("Can't open help file: ", strerror(errno), NULL);
		return(FALSE);
	}
	
	/* Find the entry for name */

	if (!find_help(fp, type, name)) {
		/* No entry available for this */

	       emsgl("No help available on ", type, " ", name,	NULL);
	       return(FALSE);
       }

	/* If we're not showing then display a title */

	if (!show) {
		help_title(type, name);
	}

	/* Now Read and display the help entry */

	while (!user_quit && fgets(buf, BUFSIZ, fp) != NULL
	       && buf[0] != ':') {
		/* Show or typeout this line of the entry */

		if (show) {
			showtext(buf);
		} else {
			typeout(buf);
		}
	}

	/* Clean up and exit typeout */

	(void) fclose(fp);
	if (show) {
		showtext(NULL);
	} else {
		typeout(NULL);
	}

	/* Return the typeout status */

	return(error_in_typeout() == 0);
}
/****************************************************************************/
int macro_describe(macro, arg, forms)
MACRO *macro;
ARGUMENT *arg;
FORM *forms;
{
	/* Typeout or show the description of a macro */

	char *output_type;

	/* Redirect typeout to a file if required */

	output_type = vstrcat("Help for keyboard macro ", macro->name, NULL);
	if (!set_typeout_file(forms, arg, output_type)) {
		free(output_type);
		return(FALSE);
	}
	free(output_type);

	/* Now show the help for the macro */

	typeout(macro->name);
	typeout(" is a kbd macro; defined as: ");
	typeout(strseq(macro->keys, SK_KEYSEQ));
	typeout("\n");
	typeout(NULL);

	return(TRUE);
}
/****************************************************************************/
void show_initial_msg()
{
	/* Show the af startup message via showtext */

	showtext("This is ");
	showtext(PROGNAME);
	showtext(" version ");
	showtext(VERSION);
	showtext("; released ");
	showtext(RELEASE_DATE);
	showtext(" by ");
	showtext(AUTHOR);
	showtext(".\n");
	(void) describe(D_AF, D_STARTUP, TRUE);

	return;
}
/****************************************************************************/
static int find_help(fp, type, name)
FILE *fp;
char *type, *name;
{
	/* Find the start of the help entry for type/name */

	char buf[BUFSIZ], *entry, *space;

	/* Loop until we run out of help file */

	while (fgets(buf, BUFSIZ, fp) != NULL) {
		/* Have we found the start of an entry of the right type? */

		if (buf[0] == ':' &&
		    !strncasecmp(buf + 1, type, strlen(type))) {
			/* Find the start of the entry name */

			for (entry = buf; !isspace(*entry); entry++) {
				/* NULL LOOP */
			}
			while (isspace(*entry)) {
				entry++;
			}

			/* Trim spaces after the entry name */

			for (space = entry; !isspace(*space); space++) {
				/* NULL LOOP */
			}
			*space = '\0';

			/* Does the name match the one we're looking for? */

			if (!strcasecmp(entry, name)) {
				return(TRUE);
			}
		}
	}

	/* No such help entry found */

	return(FALSE);
}
/****************************************************************************/
static void help_title(type, name)
char *type, *name;
{
	/* Print any title information for a help entry */

	char *bind, *value;

	/* Typeout a simple title */

	typeout("Help for ");
	typeout(type);
	typeout(" ");
	typeout(name);
	typeout("\n\n");

	/* Display the bindings of commands */

	if (!strcmp(type, D_COMMAND)) {
		/* Get the command's active bindings */

		if ((bind = bindings(name)) != NULL) {
			/* Display the bindings */

			typeout("Bound to ");
			typeout(bind);
		} else {
			/* Not bound; report this */

			typeout("Not bound to any key sequence");
		}
		typeout("\n\n");
	}

	/* Display the value of variables */

	if (!strcmp(type, D_VARIABLE)) {
		/* Get the variable's values */

		if ((value = strvar(name)) != NULL) {
			/* Set; display the value */

			typeout("Set to ");
			typeout(value);
		} else {
			/* Not set; report this */

			typeout("Not set");
		}
		typeout("\n\n");
	}

	return;
}
/****************************************************************************/
