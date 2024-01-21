/* Misc_cmd.c - Miscellaneous command handlers for af.
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
#include "keyseq.h"
#include "functions.h"
#include "commands.h"
#include "complete.h"
#include "version.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: misc_cmd.c,v 1.12 1996/08/28 17:44:08 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *strerror(), *expand(), *get_pwd();
extern char *get_home(), *get_str(), *get_dcstr();
extern int chdir(), long_confirm(), shellout();
extern int runcmd(), error_in_typeout();
extern void free(), free_forms(), msgl();
extern void emsgl(), clearmsg();
extern CLIST *fn_complete();

#ifdef HAVE_JOBCONTROL
extern void pause_af();
#endif /* HAVE_JOBCONTROL */

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the current window from commands.c */

extern WINDOW *cwin;

/****************************************************************************/
#ifdef HAVE_JOBCONTROL
/*ARGSUSED*/
FORM *suspend(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Suspend af via a SIGTSTP */

	pause_af();
	return(c_t());
}
#endif /* HAVE_JOBCONTROL */
/****************************************************************************/
/*ARGSUSED*/
FORM *shell(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Shell out of af, executing $SHELL or /bin/sh */

	return((shellout(NULL, FALSE)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *shellcmd(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Execute a command via the shell */

	char *cmd;
	
	/* Get the command line to execute */

	if ((cmd = get_str(forms, "Shell command: ")) == NULL) {
		return(c_errored());
	}
	
	/* Execute the command */

	return((shellout(cmd, TRUE)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *typecmd(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Execute a command to typeout via the shell */

	char *cmd;
	
	/* Get the command line to execute */

	if ((cmd = get_str(forms, "Shell command to typeout: ")) == NULL) {
		return(c_errored());
	}
	
	/* Execute the command */

	return((runcmd(cmd, TRUE) && !error_in_typeout())
	       ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *change_dir(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Change the current working directory */

	char *dir;
	
	/* Get the directory to move to */

	if ((dir = get_dcstr(forms, "Change default directory: ",
		get_home(NULL), fn_complete, C_CAUTIOUS)) == NULL) {
		return(c_errored());
	}

	/* Expand the directory name */

	dir = expand(dir);

	/* Attempt to chdir to dir */

	if (chdir(dir) < 0) {
		emsgl("Can't cd to ", dir, ": ", strerror(errno), NULL);
		free(dir);
		return(c_t());
	}

	/* Update the current directory cache */

	(void) get_pwd(TRUE);

	/* Clean up and return */

	free(dir);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *current_dir(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Report the current working directory */

	char *dir;

	/* Try to get the current directory */

	if ((dir = get_pwd(FALSE)) == NULL) {
		emsgl("Can't get current directory: ",
		      strerror(errno), NULL);
		return(c_errored());
	}

	/* Report the current directory and return */

	msgl("Current directory: ", dir, NULL);
	return(c_t());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *version(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Report the version and release date of the program */

	msgl("This is ", PROGNAME, " version ", VERSION,
	     "; released ", RELEASE_DATE, " by ", AUTHOR, NULL);
	return(c_t());
}
/****************************************************************************/
FORM *exit_af(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Quit the program, updating all modified buffers */

	int modified = FALSE;
	MAILBUF *buf = NULL;
	FORM *status;

	/* Save any modified buffers if desired */

	status = save_some(seq, arg, forms);
	if (ERRORED(status)) {
		return(status);
	}
	free_forms(status);

	/* Check if we have modified buffers */

	buf = cwin->buf;
	do {
		/* Check if this buffer is modified */

		modified = (buf->file != NULL && buf->mod);
		buf = buf->next;
	} while (!modified && buf != cwin->buf);

	/* Get confirmation if there are modified buffers */

	if (modified &&
	    !long_confirm("Modified buffers exist; really exit? ", FALSE)) {
		return(c_errored());
	}

	/* Return exit status */

	clearmsg();
	return(NULL);
}
/****************************************************************************/
FORM *save_and_exit(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Quit the program, updating all modified buffers */

	FORM *status;

	/* Save any modified buffers */

	status = save_all(seq, arg, forms);
	if (ERRORED(status)) {
		return(status);
	}

	/* Return exit status */

	clearmsg();
	return(NULL);
}
/****************************************************************************/
