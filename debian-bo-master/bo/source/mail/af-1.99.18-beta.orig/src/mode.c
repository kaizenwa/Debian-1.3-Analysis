/* Mode.c - Mode handling functions for af.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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
#include "af.h"
#include "mode.h"
#include "complete.h"
#include "modelist.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: mode.c,v 1.22 1996/08/28 17:44:08 malc Exp $";
static char *ModeId = MODEID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xstrdup(), *xrealloc();
extern int strncasecmp(), defining();
extern void free();
extern CLIST *add_clist();

/* Local function declarations */

char *modedisp();

/****************************************************************************/
void toggle_mode(buf, mode)
MAILBUF *buf;
unsigned mode;
{
	/* Toggle the mode specified by modenam */

	buf->modes = buf->modes ^ mode;
	return;
}
/****************************************************************************/
int active(buf, mode)
MAILBUF *buf;
unsigned mode;
{
	
	/* Return whether the mode is set or not */

	return(buf->modes & mode);
}
/****************************************************************************/
unsigned cmodes(newmodes)
unsigned newmodes;
{
	/* Set or retrieve the currently active modes */

	static unsigned amodes = M_MAIL;
	unsigned omodes = amodes;

	/* Update the active modes if required */

	amodes = (newmodes) ? newmodes : amodes;

	/* Return the original modes, adding defining if required */

	return((defining()) ? omodes | M_DEFINING : omodes);
}
/****************************************************************************/
char *modename(modeval)
unsigned modeval;
{
	/* Return the full name of the mode in modeval */

	static char *buf = NULL;
	MODE *m;

	/* Free any previous buffer */

	if (buf != NULL) {
		free(buf);
		buf = NULL;
	}

	/* Now find the first active mode */

	for (m = modes; m->name != NULL; m++) {
		/* Is this mode active? */

		if (modeval & m->mode) {
			buf = xstrdup(m->name);
			return(buf);
		}
	}

	/* Should never reach here */

	return(buf);
}
/****************************************************************************/
char *modedisp(modeval)
unsigned modeval;
{
	/* Return the display text of the mode in modeval */

	static char *buf = NULL;
	MODE *m;

	/* Free any previous buffer */

	if (buf != NULL) {
		free(buf);
		buf = NULL;
	}

	/* Now find the first active mode */

	for (m = modes; m->name != NULL; m++) {
		/* Is this mode active? */

		if (modeval & m->mode) {
			buf = xstrdup(m->disp);
			return(buf);
		}
	}

	/* Should never reach here */

	return(buf);
}
/****************************************************************************/
char *majorname(modeval)
unsigned modeval;
{
	/* Return the full name of the major mode in modeval */

	return(modename(MAJOR(modeval)));
}
/****************************************************************************/
char *modelist(modeval)
unsigned modeval;
{
	/* Return the display modes in a static buffer */

	static char *buf = NULL;
	unsigned m;

	/* Free any previous buffer */

	if (buf != NULL) {
		free(buf);
		buf = NULL;
	}

	/* Now build the list of active modes */

	for (m = M_FIRST; m <= M_LAST; m = m << 1) {
		/* Add this mode to the list if set */

		if ((modeval & m) && buf == NULL) {
			buf = xstrdup(modedisp(m));
		} else if (modeval & m) {
			buf = xrealloc(buf, strlen(buf) +
				       strlen(modedisp(m)) + 2);
			(void) strcat(buf, " ");
			(void) strcat(buf, modedisp(m));
		}
	}

	return(buf);
}
/****************************************************************************/
CLIST *mode_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of mode names completing base */

	MODE *m;

	/* Build the list of possible values */

	for (m = modes; m->name != NULL; m++) {
		/* Does this mode complete the base? */

		if (!strncasecmp(base, m->name, strlen(base))) {
			list = add_clist(list, m->name, FALSE);
		}
	}

	return(list);
}
/****************************************************************************/
