/* Exec.c - Af routines to execute external programs.
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
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <varargs.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include STRING_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: exec.c,v 1.29 1997/05/06 16:16:26 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xrealloc(), *xstrdup(), *vstrcat();
extern char *getenv(), *strerror(), *get_vtext();
extern char *utos();
extern int pclose(), kill(), killpg(), wait();
extern int isatty(), strncasecmp(), get_key();
extern unsigned alarm();
extern void _exit(), free(), emsgl(), clearmsg();
extern void typeout(), vtredraw(), tclear();
extern void init_tmodes(), end_tmodes();
extern RETSIGTYPE (*signal())();
extern FILE *popen();

/* Local function declarations */

int shellout(), close_pipe();
FILE *open_pipe();
void reset_signals();
static void init_escape(), end_escape();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the user quit flag from commands.c */

extern int user_quit;

/****************************************************************************/
FILE *open_pipe(cmd, mode, interactive)
char *cmd, *mode;
int interactive;
{
	/* Open a pipe in the given mode to run the named command */

	int fds[2];
	FILE *fp = NULL;

	/* Set up the file descriptors for the pipe */

	if (pipe(fds) < 0) {
		return(NULL);
	}

	/* Reset the terminal before starting an interactive process */

	if (interactive) {
		init_escape();
	}

	/* Now we fork */

	switch (fork()) {
	case -1:
		return(NULL);
	case 0:
		/* Redirect the file descriptors */

		switch (*mode) {
		case 'r':
			(void) close(fds[0]);
			(void) close(0);
			(void) dup2(fds[1], 1);
			(void) dup2(fds[1], 2);
			break;
		case 'w':
			(void) close(fds[1]);
			(void) dup2(fds[0], 0);
			break;
		}

		/* Set up signal handlers and process group */

		reset_signals();
		if (!interactive) {
			(void) setpgid(getpid(), getpid());
		}

		/* Execute the command or abort */

		(void) execlp(DEFSHELL, DEFSHELL,
			      SHELLARG, cmd, NULL);
		_exit(errno);
	default:
		/* Connect a file pointer to the pipe */

		switch (*mode) {
		case 'r':
			(void) close(fds[1]);
			fp = fdopen(fds[0], "r");
			break;
		case 'w':
			(void) close(fds[0]);
			fp = fdopen(fds[1], "w");
			break;
		}
	}

	/* Return the file pointer */

	return(fp);
}
/****************************************************************************/
int close_pipe(fp, interactive, keypress)
FILE *fp;
int interactive, keypress;
{
	/* Close a pipe, possibly pausing for a key press */

	int status = 0;

	/* Wait for the command to terminate */

	(void) fclose(fp);
	(void) wait(&status);

	/* Clean up shell escapes after interactive processes */

	if (interactive) {
		end_escape(keypress, TRUE);
	}

	/* And return the command's status */

	return(status);
}
/****************************************************************************/
int shellout(cmd, interactive)
char *cmd;
int interactive;
{
	/* Run a command in a subshell, or shell out if cmd is NULL */

	char *shell, *defshell = DEFSHELL;
	int status;
	RETSIGTYPE (*old_quit)(), (*old_term)();
#ifdef HAVE_JOBCONTROL	
	RETSIGTYPE (*old_tstp)();
#endif /* HAVE_JOBCONTROL */

	/* We shell out to $SHELL or use DEFSHELL */

	if (cmd != NULL || (shell = getenv(SHELL)) == NULL) {
		shell = defshell;
	}

	/* Restore the terminal modes before starting the shell */

	init_escape();

	/* Now we fork */

	switch(fork()) {
	case -1:
		/* Failed; report the reason */

		emsgl("Can't start process ", (cmd != NULL) ? cmd
		      : shell, ": ", strerror(errno), NULL);
		end_escape(FALSE, FALSE);
		return(FALSE);
	case 0:
		/* Reset the signal mask and handlers */

		reset_signals();

		/* Execute the shell or shell command */

		if (cmd != NULL) {
			(void) execlp(shell, shell, SHELLARG, cmd, NULL);
		} else {
			(void) execlp(shell, shell, NULL);
		}

		/* Something horrible has happened */

		_exit(FATAL_RETURN);
	default:
		/* Allow SIGTSTP but ignore SIGQUIT and SIGTERM */

#ifdef HAVE_JOBCONTROL
		old_tstp = signal(SIGTSTP, SIG_DFL);
#endif /* HAVE_JOBCONTROL */
		old_quit = signal(SIGQUIT, SIG_IGN);
		old_term = signal(SIGTERM, SIG_IGN);

		/* Wait for the child to exit */

		(void) wait(&status);

		/* Reset the SIGTSTP and SIGTERM handling */

#ifdef HAVE_JOBCONTROL
		(void) signal(SIGTSTP, old_tstp);
#endif /* HAVE_JOBCONTROL */
		(void) signal(SIGTERM, old_quit);
		(void) signal(SIGTERM, old_term);
	}

	/* Print the return value of the command? */

	if (!RETURNFATAL(status) && interactive) {
		printf("--------\nCommand %s %d; ",
		       (!RETURNSIG(status)) ? "returned" :
		       "terminated due to signal", RETURNVAL(status));
	}

	/* Reset the terminal modes for af */

	end_escape(interactive && !RETURNFATAL(status), TRUE);

	/* Handle a fatal error executing the command or shell */

	if (RETURNFATAL(status)) {
		emsgl("Error executing \"", (cmd != NULL)
		      ? cmd : shell, "\"", NULL);
	}
	return(!RETURNFATAL(status));
}
/****************************************************************************/
int runcmd(cmd, verbose)
char *cmd;
int verbose;
{
	/* Run a command in a subshell displaying output via typeout */

	char buf[BUFSIZ];
	int status;
	FILE *fp;

	/* Open the pipe for reading */

	if ((fp = open_pipe(cmd, "r", FALSE)) == NULL) {
		/* Error starting the command */

		emsgl("Can't start process ", cmd,
		      ": ", strerror(errno), NULL);
		return(FALSE);
	}

	/* Now display each line of output to typeout */

	while (!user_quit && fgets(buf, BUFSIZ, fp) != NULL) {
		typeout(buf);
	}

	/* Close the pipe and gather status */

	status = close_pipe(fp, FALSE, FALSE);

	/* Show the return value of the command */

	if (verbose) {
		typeout("--------\nCommand ");
		typeout((!RETURNSIG(status)) ? "returned "
			: "terminated due to signal ");
		typeout(utos((unsigned) RETURNVAL(status)));
	}

	/* End typeout and return success */

	typeout(NULL);
	return(TRUE);
}
/****************************************************************************/
char *syscmd(cmd)
char *cmd;
{
	/* Run a command and return the first line of output */

	char buf[BUFSIZ];
	char *output;
	int status;
	FILE *fp;

	/* Open the pipe for reading */

	if ((fp = open_pipe(cmd, "r", FALSE)) == NULL) {
		/* Error starting the command */

		emsgl("Can't start process ", cmd,
		      ": ", strerror(errno), NULL);
		return(NULL);
	}

	/* Default the output to the empty string */

	output = xstrdup("");

	/* Now collect the output of the command */

	while (fgets(buf, BUFSIZ, fp) != NULL) {
		/* Add the line to the output */

		output = xrealloc(output, strlen(output) + strlen(buf) + 1);
		(void) strcat(output, buf);
	}

	/* Close the pipe and check the status */

	status = close_pipe(fp, FALSE, FALSE);

	/* Check for an error executing the command */

	if (!strlen(output) && (RETURNSIG(status) || RETURNVAL(status))) {
		/* Set the output to the null string */

		free(output);
		output = NULL;
	}

	/* Return the command's output */

	return(output);
}
/****************************************************************************/
#ifdef HAVE_JOBCONTROL
void pause_af()
{
	/* Put af into the background (using a SIGTSTP) */

	/* Reset the terminal modes before suspending */

	init_escape();

	/* Stop the process while not connected to a terminal */

	do {
		(void) kill(0, SIGSTOP);
	} while (!isatty(fileno(stdin)));

	/* And restore the terminal modes */

	end_escape(FALSE, TRUE);
	return;
}
#endif /* HAVE_JOBCONTROL */
/****************************************************************************/
int edit_file(filnam)
char *filnam;
{
	/* Execute an editor on filnam */

	char *cmd, *prog, *defprog = DEFEDITOR;
	int status;

	/* What editor do we want to use? */

	if ((prog = get_vtext(V_EDITOR)) == NULL
	    && (prog = getenv(VISUAL)) == NULL
	    && (prog = getenv(EDITOR)) == NULL) {
		/* Give up and use the default */

		prog = defprog;
	}

	/* Build the argument list and edit the file */

	cmd = vstrcat(prog, " ", filnam, NULL);
	status = shellout(cmd, FALSE);

	/* Clean up and return */

	free(cmd);
	return(status);
}
/****************************************************************************/
void reset_signals()
{
	/* Reset the signal handlers to their default state */

	sigset_t sigset;

	/* Clear the signal mask */

	(void) sigemptyset(&sigset);
	(void) sigprocmask(SIG_SETMASK, &sigset, NULL);

	/* Clear the alarm timer */

	(void) alarm(0);

	/* And default the signal handlers */

	(void) signal(SIGINT, SIG_DFL);
	(void) signal(SIGQUIT, SIG_DFL);
	(void) signal(SIGTERM, SIG_DFL);

	(void) signal(SIGALRM, SIG_DFL);
	(void) signal(SIGPIPE, SIG_DFL);

#ifdef HAVE_JOBCONTROL
	(void) signal(SIGTSTP, SIG_DFL);
#endif /* HAVE_JOBCONTROL */

	return;
}
/****************************************************************************/
static void init_escape()
{
	/* Set up for a shell escape */

	clearmsg();
	end_tmodes();

	return;
}
/****************************************************************************/
static void end_escape(keypress, redraw)
int keypress, redraw;
{
	/* Restart after a shell escape */

	/* Set the terminal modes back to af state */

	init_tmodes();

	/* Wait for a key if required */

	if (keypress) {
		(void) fputs("Press a key", stdout);
		(void) fflush(stdout);
		(void) get_key();
	}

	/* Redraw the screen if required */

	if (redraw) {
		tclear();
		vtredraw();
		clearmsg();
	}
	return;
}
/****************************************************************************/
