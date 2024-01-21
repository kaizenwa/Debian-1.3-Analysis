/* Term.c - Terminal configuration for af.
   Copyright (C) 1992, 1996 Malc Arnold.

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
#include "termcntrl.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifdef HAVE_TERMIOS
#include <termios.h>
#else /* ! TERMIOS */
#ifdef HAVE_TERMIO
#include <termio.h>
#else /* ! HAVE_TERMIO */
#include <sgtty.h>
#endif /* ! HAVE_TERMIO */
#endif /* ! HAVE_TERMIOS */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: term.c,v 1.11 1996/08/28 17:44:08 malc Exp $";
static char *TermId = TERMID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *getenv(), *tgetstr();
extern int isatty(), atoi();
extern int tgetent(), tgetflag();
extern int tgetnum();
extern void exit();

/* Local function declarations */

void term_size();
static void check_tcntrl();

/****************************************************************************/
/* The global structure containing the terminal capabilities */

TERMCNTRL tcntrl;

/****************************************************************************/
/* The global variables referenced by termcap */

#ifdef OSPEED_EXTERN
extern short ospeed;				/* Terminal baud rate */
#else /* ! OSPEED_EXTERN */
short ospeed = 0;				/* Terminal baud rate */
#endif /* ! OSPEED_EXTERN */

char PC = '\0';					/* The pad character */
char *BC = NULL;				/* Backspace control */
char *UP = NULL;				/* Up control */

/****************************************************************************/
/* The original terminal modes */

#ifdef HAVE_TERMIOS
static struct termios orig_tmodes;
#else /* ! HAVE_TERMIOS */
#ifdef HAVE_TERMIO
static struct termio orig_tmodes;
#else /* ! HAVE_TERMIO */
static struct sgttyb orig_tmodes;
#endif /* ! HAVE_TERMIO */
#endif /* ! HAVE_TERMIOS */

/****************************************************************************/
void init_tcntrl()
{
	/* Initialise the terminal control structure from termcap */

	/* The termcap area */

	static char tcap_area[BUFSIZ];

	/* Defaults for values */

	static char *defcr = "\r";
	static char *defbackspace = "\b";
	static char *defbell = BELL;

	/* The termcap buffer */

	char bp[1024];
	char *pad, *term, *area = tcap_area;

	/* Get the type of terminal we're using */

	if ((term = getenv("TERM")) == NULL) {
		(void) fprintf(stderr, "Terminal type not set\n");
		exit(2);
	}

	/* Not try to get the terminal's entry */

	if (tgetent(bp, term) <= 0) {
		(void) fprintf(stderr, "Terminal type %s unknown\n", term);
		exit(2);
	}

	/* Set up the size of the screen */

	tcntrl.lines = tgetnum("li");
	tcntrl.columns = tgetnum("co");
	tcntrl.tab_spacing = 8;

	/* Check overrides on the default screen size */

	term_size();

	/* Now get the basic screen controls */

	tcntrl.cls = tgetstr("cl", &area);
	tcntrl.home = tgetstr("ho", &area);
	tcntrl.clr_to_eol = tgetstr("ce", &area);
	tcntrl.move_cursor = tgetstr("cm", &area);

	/* Set up the carriage return and backspace handling */

	if ((tcntrl.cr = tgetstr("cr", &area)) == NULL) {
		tcntrl.cr = defcr;
	}
	if (tgetflag("bs") ||
	    (tcntrl.backspace = tgetstr("bc", &area)) == NULL) {
		tcntrl.backspace = defbackspace;
	}
	tcntrl.nd_space = tgetstr("nd", &area);

	/* We'll default the bell character here */

	tcntrl.bell = defbell;

	/* Now check that the terminal is smart enough */

	check_tcntrl(term);

	/* Set the globals referenced by termcap */

	PC = ((pad = tgetstr("pc", &area)) != NULL) ? *pad : '\0';
	BC = tcntrl.backspace;
	UP = tgetstr("up", &area);

	return;
}
/****************************************************************************/
void term_size()
{
	/* Override the default value from environment */

	char *env;
	int value;

#ifdef HAVE_RESIZING
	/* Check the window size (BSD under X etc) */

	struct winsize wsiz;

	/* Get the window size data */

	if (!ioctl(0, TIOCGWINSZ, &wsiz) &&
	    wsiz.ws_row > 0 && wsiz.ws_col > 0) {
		/* Update the terminal data */

		tcntrl.lines = wsiz.ws_row;
		tcntrl.columns = wsiz.ws_col;
		return;
	}
#endif /* HAVE_RESIZING */

	/* Check if LINES and COLUMNS are defined in the environment */

	if ((env = getenv(LINES)) != NULL && (value = atoi(env)) > 0) {
		tcntrl.lines = value;
	}
	if ((env = getenv(COLUMNS)) != NULL && (value = atoi(env)) > 0) {
		tcntrl.columns = value;
	}

	return;
}
/****************************************************************************/
static void check_tcntrl(term)
char *term;
{
	/* Check the terminal is smart enough for us */

	/* Check we know the screen size */

	if (tcntrl.lines <= 0 || tcntrl.columns <= 0) {
		(void) fprintf(stderr, "Can't determine size of terminal %s\n",
			       term);
		exit(2);
	}

	/* Check we can clear the screen */

	if (tcntrl.cls == NULL) {
		(void) fprintf(stderr, "Terminal %s can't clear screen\n",
			       term);
		exit(2);
	}

	/* Check for cursor motion - need absolute or alternatives */

	if (tcntrl.move_cursor == NULL &&
	    (tcntrl.home == NULL || tcntrl.nd_space == NULL)) {
		(void) fprintf(stderr, "Terminal %s can't position cursor\n",
			       term);
		exit(2);
	}

	/* If we reach here, then all's well */

	return;
}
/****************************************************************************/
#ifdef HAVE_TERMIO
/****************************************************************************/
void init_tmodes()
{
	/* Set the terminal modes (using termio or termios) */

	int tty;

	/* Select the right structure type */

#ifdef HAVE_TERMIOS
	struct termios tmodes;
#else /* ! HAVE_TERMIOS */
	struct termio tmodes;
#endif /* ! HAVE_TERMIOS */

	/* Abort if stdout is not a terminal */

	if (!isatty(tty = fileno(stdout))) {
		return;
	}

	/* Get the current terminal modes */

#ifdef HAVE_TERMIOS
	(void) tcgetattr(tty, &tmodes);
	(void) tcgetattr(tty, &orig_tmodes);
#else /* ! HAVE_TERMIOS */
	(void) ioctl(tty, TCGETA, &tmodes);
	(void) ioctl(tty, TCGETA, &orig_tmodes);
#endif /* ! HAVE_TERMIOS */

	/* Set the output speed global for termcap */

	ospeed = (orig_tmodes.c_cflag & CBAUD);

	/* Modify the modes we care about */

	tmodes.c_iflag &= ~(IXON | IXOFF | INLCR | ICRNL | IGNCR);
	tmodes.c_oflag &= ~OPOST;
	tmodes.c_cflag |= CS8;
	tmodes.c_cflag &= ~PARENB;
	tmodes.c_lflag &= ~(ICANON | ECHO | ISIG);
	tmodes.c_cc[VMIN] = 1;

#ifdef HAVE_TERMIOS
	/* These can only be set using termios */

	tmodes.c_cc[VSUSP] = 0;
	tmodes.c_cc[VSTART] = 0;
	tmodes.c_cc[VSTOP] = 0;
#endif /* HAVE_TERMIOS */

	/* Set the modified terminal modes */

#ifdef HAVE_TERMIOS
	(void) tcsetattr(tty, TCSADRAIN, &tmodes);
#else /* ! HAVE_TERMIOS */
	(void) ioctl(tty, TCSETAW, &tmodes);
#endif /* ! HAVE_TERMIOS */

	return;
}
/****************************************************************************/
void end_tmodes()
{
	/* Restore the tty modes to their original state (via termio) */

	int tty;

	/* Ignore if stdout is not a terminal */

	if (isatty(tty = fileno(stdout))) {
#ifdef HAVE_TERMIOS
		(void) tcsetattr(tty, TCSADRAIN, &orig_tmodes);
#else /* ! HAVE_TERMIOS */
		(void) ioctl(tty, TCSETAW, &orig_tmodes);
#endif /* ! HAVE_TERMIOS */
	}

	return;
}
/****************************************************************************/
#else /* ! HAVE_TERMIO */
/****************************************************************************/
void init_tmodes()
{
	/* Set the terminal modes (using sgtty) */

	int tty;
	struct sgttyb tmodes;

	/* Abort if stdout is not a terminal */

	if (!isatty(tty = fileno(stdout))) {
		return;
	}

	/* Get the current terminal modes */

	(void) ioctl(tty, TIOCGETP, &tmodes);
	(void) ioctl(tty, TIOCGETP, &orig_tmodes);

	/* Set the output speed global for termcap */

	ospeed = orig_tmodes.sg_ospeed;

	/* Modify the modes we care about */

	tmodes.sg_flags |= RAW;
	tmodes.sg_flags &= ~(ECHO|XTABS);

	/* Set the modified terminal modes */

	(void) ioctl(tty, TIOCSETP, &tmodes);

	return;
}
/****************************************************************************/
void end_tmodes()
{
	/* Restore the tty modes to their original state (via sgtty) */

	int tty;

	/* Ignore if stdout is not a terminal */

	if (isatty(tty = fileno(stdout))) {
		(void) ioctl(tty, TIOCSETP, &orig_tmodes);
	}

	return;
}
/****************************************************************************/
#endif /* ! HAVE_TERMIO */
/****************************************************************************/
