/* Commands.c - Command handling for af.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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
#include <signal.h>
#include <regex.h>
#include <sys/types.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "commands.h"
#include "keymaps.h"
#include "macros.h"
#include "variable.h"
#include "mode.h"
#include "complete.h"
#include "cmdlist.h"
#include "misc.h"
#include STRING_HDR

#ifndef HAVE_SIGPROCMASK
#ifdef HAVE_SIGSETMASK
#include <psignal.h>
#define HAVE_SIGPROCMASK 1
#endif /* HAVE_SIGSETMASK */
#endif /* ! HAVE_SIGPROCMASK */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: commands.c,v 1.53 1997/03/05 21:23:45 malc Exp $";
static char *CmdId = COMMANDID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *strkey(), *strseq();
extern char *formtext(), *bindings(), *expand();
extern char *get_cstr();
extern int strcasecmp(), strncasecmp(), active();
extern int atoi(), mb_touched(), mb_cleared();
extern int sync_buffer(), get_key(), get_tkey();
extern int get_vval(), run_macro(), vtupdate();
extern unsigned alarm(), cmodes();
extern void free(), free_seq(), free_forms();
extern void kill_macros(), list_macros();
extern void unget_key(), typeout(), table();
extern void msg(), msgl(), emsg(), emsgl();
extern void cmsg(), clearmsg(), hide_cursor();
extern void redisplay();
extern KEYMAP *find_keymap(), *mode_keymap();
extern BINDING *find_binding();
extern KEYSEQ *new_seq(), *add_seq();
extern MACRO *find_macro();
extern FORM *load();
extern CLIST *fn_complete(), *macro_complete();
extern CLIST *add_clist();
extern RETSIGTYPE (*signal())();

#ifndef HAVE_SIGPROCMASK
extern int kill();
extern pid_t getpid();
extern unsigned save_atimer();
extern unsigned restore_atimer();
#endif /* ! HAVE_SIGPROCMASK */

#ifdef HAVE_RESIZING
extern void term_size(), reinit_vterm();
extern void fix_windows(), vtredraw();
#ifndef HAVE_SIGPROCMASK
extern int tlines(), tcols();
#endif /* ! HAVE_SIGPROCMASK */
#endif /* HAVE_RESIZING */

/* Local function declarations */

COMMAND *find_command(), *find_by_func();
ARGUMENT *form_or_arg();
CLIST *cmd_complete();
static int cmd_key(); 
static void block_signals();
static void unblock_signals();
static FORM *exec_macro(), *exec_key();
static FORM *gather_arg();
static RETSIGTYPE autosync();

#ifdef HAVE_RESIZING
static RETSIGTYPE resize();
#endif /* HAVE_RESIZING */

/****************************************************************************/
/*
 * The current window is stored globally here, to simplify parameters
 * of command-handling functions and to allow the buffer and window
 * handlers to reference the list.
 */

WINDOW *cwin = NULL;

/****************************************************************************/
/* This flag indicates whether the user has quit or not */

int user_quit = FALSE;

/****************************************************************************/
/* We store the last and current commands executed here */

COMMAND *last_command = NULL;
COMMAND *this_command = NULL;

/****************************************************************************/
/* Are we echoing keystrokes, and what is the delay before echoing? */

static int echoing = FALSE;
static unsigned echodelay = 0;

/****************************************************************************/
/* Are signals currently blocked and to what depth? */

static int sig_depth = 0;

/****************************************************************************/
/* Variables used to store details about inactive signals */

#ifndef HAVE_SIGPROCMASK
static ATIMER *sig_tbuf = 0;
static int sig_lines = 0;
static int sig_cols = 0;
#endif /* ! HAVE_SIGPROCMASK */

/****************************************************************************/
void do_commands()
{
	/* Handle af's interactive mode, handling keypresses */

	FORM *status = NULL;

	/* Set up auto-syncronization of buffers */	

	(void) signal(SIGALRM, autosync);
	(void) alarm((unsigned) get_vval(V_RESYNC));

#ifdef HAVE_RESIZING
	/* Set up handling for SIGWINCH */

	(void) signal(SIGWINCH, resize);
#endif /* HAVE_RESIZING */

	/* Handle the key-command cycle until one returns exit status */

	do {
		/* Free any old return value */

		free_forms(status);

		/* Initialise key sequence echoing */

		echodelay = get_vval(V_ECHO);
		echoing = FALSE;

		/* Make sure we're using the right modes */

		(void) cmodes(cwin->buf->modes);
	} while ((status = exec_key(NULL, NULL, NULL, NULL)) != NULL);

	/* End of interactive command handling */

	return;
}
/****************************************************************************/
void run_script(script)
char *script;
{
	/* Run an af script, using the load routines */

	free_forms(load(script));
	return;
}
/****************************************************************************/
FORM *exec_help_key(seq, arg)
KEYSEQ *seq;
ARGUMENT *arg;
{
	/* Scan the help keymap after help-for-help or similar */

	/*
	 * Make sure the help is displayed until the next
	 * keypress by getting it here and then ungetting it
	 */

	unget_key(get_key());

	/* Turn on key sequence echoing */

	echoing = TRUE;
	echodelay = 0;

	/* Force the last command to be up to date */

	last_command = this_command;

	/* Now rescan the help keymap for the next keypress */

	return(exec_key(seq, arg, find_keymap(HELP_MAP), NULL));
}
/****************************************************************************/
FORM *exec_mbuf_key()
{
	/* Handle minibuffer key-to-command parsing */

	/* We may delay to clean up messages */

	echoing = FALSE;
	echodelay = (mb_touched()) ? ECHO_DELAY : 0;

	/* Now scan the minibuffer keymaps for the next keypress */

	return(exec_key(NULL, NULL, NULL, NULL));
}
/****************************************************************************/
FORM *exec_type_key()
{
	/* Handle typeout key-to-command parsing */

	/* Initialise key sequence echoing */

	echodelay = get_vval(V_ECHO);
	echoing = FALSE;

	/* Now scan the typeout keymaps for the next keypress */

	return(exec_key(NULL, NULL, NULL, NULL));
}
/****************************************************************************/
/*ARGSUSED*/
FORM *exec_command(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Get the name of a command and execute it */

	char *name;
	COMMAND *cmd;
	MACRO *macro = NULL;

	/* Reset key echoing in case it's needed */

	echodelay = get_vval(V_ECHO);
	echoing = FALSE;

	/* Get the command to execute */

	if ((name = get_cstr(forms, "M-x ", cmd_complete,
			     C_STRICT)) == NULL) {
		return(c_errored());
	}

	/* Find the command to execute */

	if ((cmd = find_command(name)) == NULL
	    && (macro = find_macro(name)) == NULL) {
		/* Can't find anything to execute */

		emsgl("No command ", name, NULL);
		return(c_errored());
	}

	/* Check the command can be executed */

	if (cmd != NULL && !(cmd->modes & cmodes(0))) {
		emsgl("Can't execute ", cmd->name, " in this buffer", NULL);
		return(c_errored());
	}

	/* Update the command being executed */

	this_command = cmd;

	/* Execute the command and return status */

	return((cmd != NULL) ? cmd->func(NULL, arg, NULL)
			     : exec_macro(macro, arg));
}
/****************************************************************************/
static FORM *exec_macro(macro, arg)
MACRO *macro;
ARGUMENT *arg;
{
	/* Execute the last keyboard macro */

	int repeat;

	/* Check how many times to repeat the macro */

	repeat = (arg != NULL) ? (arg->negative) ? 0 : arg->value : 1;

	/* Execute the macro and return status */

	return((run_macro(macro, repeat)) ? c_t() : c_errored());
}
/****************************************************************************/
/*ARGSUSED*/
FORM *univ_arg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Gather a universal argument and execute a command */

	return(gather_arg(seq, arg, forms, TRUE, FALSE));
}
/****************************************************************************/
FORM *digit_arg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Gather a numeric argument and execute a command */

	return(gather_arg(seq, arg, forms, FALSE, FALSE));
}
/****************************************************************************/
FORM *neg_arg(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/* Gather a negative argument and execute a command */

	return(gather_arg(seq, arg, forms, FALSE, TRUE));
}
/****************************************************************************/
/*ARGSUSED*/
FORM *kbd_quit(seq, arg, forms)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
{
	/*
	 * Quit whatever we are doing, including typeout or the
	 * minibuffer if we are at the top level.
	 */

	/* Show that we have quit */

	if (cmodes(0) & M_TYPEOUT) {
		msg("Quit");
	} else {
		emsg("Quit");
	}

	/* Set the user quit flag */

	user_quit = TRUE;

	/* And return quit or error */

	return((seq != NULL && seq->len < 2 &&
		(cmodes(0) & (M_MBUF | M_TYPEOUT)))
	       ? NULL : c_errored());
}
/****************************************************************************/
static FORM *exec_key(seq, arg, map, gmap)
KEYSEQ *seq;
ARGUMENT *arg;
KEYMAP *map, *gmap;
{
	/* Get a key sequence and run the related command */

	int key;
	KEYSEQ *newseq;
	BINDING *local, *global;
	FORM *status;

	/* Haven't had a user quit yet */

	user_quit = FALSE;

	/* Get the next keypress and make a new key sequence */

	key = cmd_key(seq, FALSE);
	newseq = new_seq(seq, key);

	/* Select the keymaps and dereference the key */

	gmap = (map == NULL) ? find_keymap(GLOBAL_MAP) : gmap;
	map = (map == NULL) ? mode_keymap(cmodes(0), key) : map;
	global = (gmap != NULL) ? find_binding(gmap, key) : NULL;
	local = find_binding(map, key);

	/* See if we got a better global match than local */

	local = (local == NULL || local->key != key
		 && global != NULL && IS_ACTIVE(global, cmodes(0))
		 && global->key == key) ? global : local;

	/* Check if the key was bound */

	if (local == NULL || !IS_ACTIVE(local, cmodes(0))) {
		/* Error this command and kill any macro */

		emsgl(strseq(newseq, SK_KEYSEQ), " not bound", NULL);
		this_command = last_command = NULL;
		kill_macros(cwin);
		free_seq(newseq);
		return(c_errored());
	}

	/* Redraw the minibuffer after a message */

	if ((cmodes(0) & M_MBUF) && echodelay) {
		(void) redraw(NULL, NULL, NULL);
	}

	/* If a keymap then recurse to descend the keymap tree */

	if (IS_KEYMAP(local)) {
		/* Select the mode-based and global keymaps to check */

		map = local->object.map;
		gmap = (global != local && IS_KEYMAP(global))
			? global->object.map : NULL;

		/* Now recurse to scan the next keymap pair */

		return(exec_key(newseq, arg, map, gmap));
	}

	/* Clean up any echoed keystrokes */

	if (!(cmodes(0) & M_MBUF)) {
		clearmsg();
	}

	/* Block signals while the command is in progress */

	block_signals();

	/* Save the command we are executing */

	this_command = (IS_COMMAND(local)) ? local->object.cmd : NULL;

	/* Run the command and store its status */

	status = (IS_COMMAND(local)) ? this_command->func(newseq, arg, NULL)
		: exec_macro(local->object.macro, arg);

	/* Cancel any kbd macro after an error */

	if (status != NULL && ERRORED(status)) {
		kill_macros(cwin);
	}

	/* Update the last command executed */

	last_command = this_command;

	/* Unblock signals and free the key sequence */

	unblock_signals();
	free_seq(newseq);

	/* Return the status of the command */

	return(status);
}
/****************************************************************************/
static int cmd_key(seq, for_argument)
KEYSEQ *seq;
int for_argument;
{
	/* Get a keypress for a command sequence */

	int updated, key;
	unsigned delay;

	/* Update the screen */

	updated = vtupdate(FALSE);

	/* Redraw the minibuffer after a full redraw */

	if ((cmodes(0) & M_MBUF) &&
	    (updated || echodelay && mb_cleared())) {
		(void) redraw(NULL, NULL, NULL);
		echodelay = 0;
	}

	/* Or hide the cursor if not in the minibuffer */

	if (!(cmodes(0) & M_MBUF)) {
		hide_cursor();
	}

	/* What is the timeout for the keypress? */

	delay = ((cmodes(0) & M_MBUF) || seq != NULL) ? echodelay : 0;

	/* Get the key to process, handling timeouts */

	while ((key = get_tkey(delay)) == EOF) {
		/* Can only happen once */

		if ((cmodes(0) & M_MBUF)) {
			/* Clean up after a message */

			(void) redraw(NULL, NULL, NULL);
		} else {
			/* Start echoing keypresses */

			msgl(strseq(seq, SK_KEYSEQ), " ", NULL);
			echoing = TRUE;
		}
		echodelay = 0;
	}

	/* Echo the keypress if required and appropriate */

	if (echoing && (!for_argument || isdigit(key) || key == '-')) {
		cmsg(strkey(key, SK_KEYSEQ));
		cmsg(" ");
	}

	/* Return the key pressed */

	return(key);
}
/****************************************************************************/
static FORM *gather_arg(seq, arg, forms, universal, negative)
KEYSEQ *seq;
ARGUMENT *arg;
FORM *forms;
int universal, negative;
{
	/* Handle numeric and universal arguments */

	int key;
	KEYSEQ *newseq = NULL;
	ARGUMENT *newarg;
	FORM *status;

	/* Convert any form to an argument */

	arg = form_or_arg(forms, arg);

	/* Create a new argument */

	newarg = (ARGUMENT *) xmalloc(sizeof(ARGUMENT));

	/* Set the flags for the argument and default the value */

	newarg->universal = universal;
	newarg->negative = (arg != NULL && !arg->universal) ?
		arg->negative != negative : negative;
	newarg->value = (universal) ? ARG_MULTIPLIER : 0;
	
	/* Set the value of the argument */

	if (universal && arg != NULL && arg->universal
	    && arg->value * newarg->value > arg->value) {
		newarg->value *= arg->value;
	} else if (!universal && arg != NULL && !arg->universal) {
		newarg->value = arg->value;
	}

	/* Handle a digit argument bound to a numeric key */

	if (!universal && seq != NULL && isdigit(LASTKEY(seq))) {
		newarg->value = newarg->value * 10 + LASTKEY(seq) - '0';
	}

	/* Redisplay an echoed sequence; it will have been cleared */

	if (echoing && seq != NULL) {
		msgl(strseq(seq, SK_KEYSEQ), " ", NULL);
	}

	/* Get a keypress to check for an argument */

	key = cmd_key(seq, TRUE);

	/* Handle an explicit universal argument */

	if (universal && (isdigit(key) || key == '-')) {
		newarg->universal = FALSE;
		newarg->value = 0;
	}

	/* Update the sequence and argument for any digits typed */

	while (isdigit(key) || key == '-') {
		if (key == '-') {
			newarg->negative = !newarg->negative;
		} else if (newarg->value * 10 + key - '0' > newarg->value) {
			newarg->value = newarg->value * 10 + key - '0';
		}
		newseq = (newseq == NULL) ? new_seq(seq, key)
					  : add_seq(newseq, key);
		key = cmd_key(newseq, TRUE);
	}

	/* Leave the last (non-argument) key pressed */

	unget_key(key);

	/* Get and execute another command, storing status */

	status = exec_key((newseq != NULL) ? newseq :
			  seq, newarg, NULL, NULL);

	/* Free the key sequence and argument and return status */

	if (newseq != NULL) {
		free_seq(newseq);
	}
	free(newarg);

	return(status);
}
/****************************************************************************/
COMMAND *find_command(cmdname)
char *cmdname;
{
	/* Return the COMMAND entry relating to the named command */
	
	COMMAND *cmd;

	/* Check the list of internal commands */

	for (cmd = commands; cmd->name != NULL; cmd++) {
		/* Is this the command we're looking for? */

		if (!strcasecmp(cmd->name, cmdname)) {
			return(cmd);
		}
	}

	/* No such command */

	return(NULL);
}
/****************************************************************************/
COMMAND *find_by_func(func)
FORM *(*func)();
{
	/* Find the command which calls func */

	COMMAND *cmd;

	/* Search the list of commands */

	for (cmd = commands; cmd->func != NULL; cmd++) {
		/* Is this the command we're looking for? */

		if (cmd->func == func) {
			return(cmd);
		}
	}

	/* No command calls func */

	return(NULL);
}
/****************************************************************************/
REGION *get_region(win)
WINDOW *win;
{
	/* Return the details of the region in a window */

	static REGION region;
	MESSAGE *m;

	/* Check there is a region to return */

	if (win->mark == NULL) {
		emsg("No mark set in this window");
		return(NULL);
	} else if (win->point == win->mark) {
		emsg("No lines in region");
		return(NULL);
	}

	/* Check if the region lies above the point */

	region.above_point = TRUE;

	for (m = win->point; m != NULL; m = m->next) {
		if (m == win->mark) {
			region.above_point = FALSE;
			break;
		}
	}

	/* Set the boundaries of the region */

	region.start = (region.above_point) ? win->mark : win->point;
	region.end = (region.above_point) ? win->point : win->mark;

	/* Return the region */

	return(&region);
}
/****************************************************************************/
int chk_msg(win, notnull)
WINDOW *win;
int notnull;
{
	/* Check we have messages to work on */

	if (!win->buf->no_msgs) {
		emsg("No messages in buffer");
		return(FALSE);
	} else if (notnull && win->point->text == NULL) {
 		emsg("No such message");
		return(FALSE);
	} else {
		return(TRUE);
	}
	/*NOTREACHED*/
}
/****************************************************************************/
int chk_readonly(win)
WINDOW *win;
{
	/* Check that the window's buffer isn't read-only */

	if (active(win->buf, M_READONLY)) {
		emsg("Buffer is Read-Only");
		return(FALSE);
	}

	/* Buffer isn't read-only */

	return(TRUE);
}
/****************************************************************************/
int chk_pop3(win)
WINDOW *win;
{
	/* Check that a window's buffer isn't in POP3 mode */

	if (active(win->buf, M_POP3)) {
		emsg("Buffer is in Pop3 mode");
		return(FALSE);
	}

	/* Buffer isn't in POP3 mode */

	return(TRUE);
}
/****************************************************************************/
ARGUMENT *form_or_arg(form, arg)
FORM *form;
ARGUMENT *arg;
{
	/* Generate an argument from the form or the argument */

	static ARGUMENT argbuf;
	long value;

	/* Generate a new argument if form is non-null */

	if (form != NULL) {
		/* Get the numeric value of the form */

		value = atoi(formtext(form));
		
		/* And set up the argument */

		argbuf.universal = FALSE;
		argbuf.negative = (value < 0);
		argbuf.value = (value < 0) ? -value : value;

		/* New return the generated argument */

		return(&argbuf);
	}

	/* No form, return the original argument */

	return(arg);
}
/****************************************************************************/
void list_commands(rexpr, headings)
regex_t *rexpr;
int headings;
{
	/* Typeout the af commands (or those matching a regex) */

	int cmd_headings = headings;
	COMMAND *cmd;

	/* Loop through the commands */

	for (cmd = commands; !user_quit && cmd->name != NULL; cmd++) {
		/* Check if this command should be listed */

		if (rexpr == NULL ||
		    !regexec(rexpr, cmd->name, 0, NULL, 0)) {
			/* Print a heading for the commands? */
			if (cmd_headings) {
				typeout("\nCommands:\n");
				cmd_headings = FALSE;
			}

			/* And print the command */

			table(cmd->name, bindings(cmd->name));
		}
	}

	/* And then add the keyboard macros */

	list_macros(rexpr, headings, FALSE, FALSE);
	return;
}
/****************************************************************************/
CLIST *cmd_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of commands completing base */

	COMMAND *cmd;

	/* Build the list of possible values */

	for (cmd = commands; cmd->name != NULL; cmd++) {
		/* Is this command a possible completion? */

		if (!strncasecmp(base, cmd->name, strlen(base))) {
			list = add_clist(list, cmd->name, FALSE);
		}
	}

	/* Then add the named kbd macros to the list */

	return(macro_complete(list, base));
}
/****************************************************************************/
static void block_signals()
{
	/* Set or fake up blocking on SIGWINCH and SIGALRM */

#ifdef HAVE_SIGPROCMASK
	sigset_t sigset;
#endif /* HAVE_SIGPROCMASK */

	/* Check if signals were already blocked */

	if (sig_depth++ > 0) {
		return;
	}

#ifdef HAVE_SIGPROCMASK
	/* Set up a set of the signals to block */

	(void) sigemptyset(&sigset);
	(void) sigaddset(&sigset, SIGALRM);
#ifdef HAVE_RESIZING
	(void) sigaddset(&sigset, SIGWINCH);
#endif /* HAVE_RESIZING */

	/* And block the signals in this process */

	(void) sigprocmask(SIG_BLOCK, &sigset, NULL);

#else /* ! HAVE_SIGSETMASK */

	/* First we need to suspend the alarm timer */

	(void) save_atimer(&sig_tbuf);

#ifdef HAVE_RESIZING
	/* We need to unset the SIGWINCH handler */

	(void) signal(SIGWINCH, SIG_IGN);

	/* And store the current screen size */

	sig_lines = tlines();
	sig_cols = tcols();
#endif /* HAVE_RESIZING */
#endif /* ! HAVE_SIGPROCMASK */
	return;
}
/****************************************************************************/
static void unblock_signals()
{
	/* Clear or fake up unblocking SIGWINCH and SIGALRM */

#ifdef HAVE_SIGPROCMASK
	sigset_t sigset;
#endif /* HAVE_SIGPROCMASK */

	/* Check if signals will still be blocked */

	if (--sig_depth > 0) {
		return;
	}

#ifdef HAVE_SIGPROCMASK
	/* Set up a set of the signals to unblock */

	(void) sigemptyset(&sigset);
	(void) sigaddset(&sigset, SIGALRM);
#ifdef HAVE_RESIZING
	(void) sigaddset(&sigset, SIGWINCH);
#endif /* HAVE_RESIZING */

	/* And unblock the signals in this process */

	(void) sigprocmask(SIG_UNBLOCK, &sigset, NULL);
#else /* ! HAVE_SIGSETMASK */

#ifdef HAVE_RESIZING
	/* Restore SIGWINCH handling */

	(void) signal(SIGWINCH, resize);

	/* Get the new window size and check it */

	term_size();
	if (tlines() != sig_lines || tcols() != sig_cols) {
		(void) kill(getpid(), SIGWINCH);
	}
#endif /* HAVE_RESIZING */

	/* Now we should restore the alarm timer */

	(void) restore_atimer(&sig_tbuf);
#endif /* ! HAVE_SIGPROCMASK */

	return;
}
/****************************************************************************/
/*ARGSUSED*/
static RETSIGTYPE autosync(st)
int st;
{
	/* Resynchronize all buffers on alarm signal */

	MAILBUF *buf = cwin->buf;
	WINDOW *win;

	/* Ignore alarm signals while we resync */

	(void) signal(SIGALRM, SIG_IGN);

	/* Loop through all existing buffers */

	do {
		if (buf->file != NULL && sync_buffer(buf, TRUE)) {
			/* Update all windows displaying the buffer */

			win = cwin;
			do {
				if (win->buf == buf) {
					redisplay(win);
				}
				win = win->next;
			} while (win != cwin);

			/* Announce the arraival of new mail */

			msgl("(New mail has arrived in ",
			     buf->name, ")", NULL);
		}
		buf = buf->next;
	} while (buf != cwin->buf);

	/* Update the display */

	vtupdate(FALSE);
	hide_cursor();

	/* Reset the SIGALRM handler and timer */

	(void) signal(SIGALRM, autosync);
	(void) alarm((unsigned) get_vval(V_RESYNC));
}
/****************************************************************************/
#ifdef HAVE_RESIZING
/*ARGSUSED*/
static RETSIGTYPE resize(st)
int st;
{
	/* Resize the display after catching a SIGWINCH */

	/* Get the new size and update vterm and windows */

	term_size();
	reinit_vterm();
	fix_windows(cwin);
	
	/* Force the screen display to be up to date */

	vtredraw();
	vtupdate(TRUE);
	msg("(Window size changed)");

	/* Reset the SIGWINCH handler */

	(void) signal(SIGWINCH, resize);
}
#endif /* HAVE_RESIZING */
/****************************************************************************/
