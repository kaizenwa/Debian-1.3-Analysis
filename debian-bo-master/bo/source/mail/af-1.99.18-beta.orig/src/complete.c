/* Complete.c - Completion functions for af.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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
#include "complete.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: complete.c,v 1.8 1996/08/28 17:44:08 malc Exp $";
static char *CompleteId = COMPLETEID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup();
extern int strcasecmp(), strncasecmp(), mklower();
extern void free(), msg(), typeout();

/* Local function declarations */

CLIST *add_clist();
static void free_clist();

/****************************************************************************/
/* Import the user quit flag from commands.c */

extern int user_quit;

/****************************************************************************/
COMPLETE *do_completion(base, complete, one_word)
char *base;
CLIST *(*complete)();
int one_word;
{
	/* Complete base from the possibles given by complete */

	static COMPLETE cbuf = { NULL, FALSE };
	char *p1, *p2;
	int len;
	CLIST *list, *e;

	/* Reset the return buffer */

	if (cbuf.tail != NULL) {
		free(cbuf.tail);
		cbuf.tail = NULL;
	}
	cbuf.complete = FALSE;

	/* Check that completion is active */

	if (complete == NULL) {
		return(&cbuf);
	}

	/* Set the length of the base */

	len = strlen(base);

	/* Get the list of completions */

	list = complete(NULL, base);

	/* Loop through the entries checking for the common part */

	for (e = list; e != NULL; e = e->next) {
		/* On a first match just set tail */

		if (cbuf.tail == NULL) {
			cbuf.tail = xstrdup(e->entry + len);
			cbuf.complete = TRUE;
			continue;
		}

		/* Calculate length of multiple match */

		p1 = cbuf.tail;
		p2 = e->entry + len;

		/* Loop over the matches */

		while (*p1 != '\0') {
			/* Check if new entry is subset of old */

			if (*p2 == '\0') {
				/* We have a complete match */

				*p1 = '\0';
				cbuf.complete = TRUE;
				break;
			}

			/* Check if the new entry differs */

			if (!e->case_dep && *p1 != *p2 || e->case_dep
			    && mklower(*p1) != mklower(*p2)) {
				/* We have several matches */

				*p1 = '\0';
				cbuf.complete = FALSE;
				break;
			}

			p1++;
			p2++;
		}
	}

	/* Resize the tail as appropriate */

	if (cbuf.tail != NULL && strlen(cbuf.tail) > 0) {
		/* Trim the tail to one word if required */

		for (p1 = cbuf.tail; one_word && *p1 != '\0'; p1++) {
			if (!isalnum(*p1)) {
				*(p1 + 1) = '\0';
			}
		}

		/* And reallocate the tail as required */

		cbuf.tail = xrealloc(cbuf.tail, strlen(cbuf.tail) + 1);
	} else if (cbuf.tail != NULL) {
		/* Free the now-redundant tail */

		free(cbuf.tail);
		cbuf.tail = NULL;
	}

	/* Free the list of possible completions and return */

	free_clist(list);
	return(&cbuf);
}
/****************************************************************************/
void show_completions(base, complete)
char *base;
CLIST *(*complete)();
{
	/* Display all possible completions of base from entries */

	CLIST *list, *e;

	/* Get the possible entries */

	list = complete(NULL, base);

	/* Typeout a header */

	if (list != NULL) {
		typeout("Possible completions for '");
		typeout(base);
		typeout("':\n\n");
	} else {
		typeout("No possible completion for '");
		typeout(base);
		typeout("'.\n");
	}
		
	/* And the possible completions */

	for (e = list; !user_quit && e != NULL; e = e->next) {
		typeout(e->entry);
		typeout("\n");
	}

	/* Free the list, end the typeout, and return */

	free_clist(list);
	typeout(NULL);

	return;
}
/****************************************************************************/
CLIST *add_clist(list, entry, case_dep)
CLIST *list;
char *entry;
int case_dep;
{
	/* Add the possible completion entry to the list */

	/* We store the end position of the list for faster building */

	static CLIST *last = NULL;
	CLIST *lnode;

	/* Now allocate and fill the entry's node */

	lnode = (CLIST *) xmalloc(sizeof(CLIST));
	lnode->entry = xstrdup(entry);
	lnode->case_dep = case_dep;
	lnode->next = NULL;

	/* And add the node to the list */

	if (list == NULL) {
		last = lnode;
		return(lnode);
	} else {
		last->next = lnode;
		last = lnode;
		return(list);
	}
	/*NOTREACHED*/
}
/****************************************************************************/
static void free_clist(list)
CLIST *list;
{
	/* Free all the space associated with list */

	if (list != NULL) {
		free_clist(list->next);
		free(list->entry);
		free(list);
	}
	return;
}
/****************************************************************************/
