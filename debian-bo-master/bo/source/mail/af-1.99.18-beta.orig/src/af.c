/* Af.c - Main module for af.
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
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "mode.h"
#include "version.h"
#include STRING_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

/****************************************************************************/
/*
 * RCS info.  Includes af.h, config.h and version.h
 * defintions so they only appear once in the object.
 */

#ifndef lint
static char *RcsId = "$Id: af.c,v 1.42 1997/05/05 02:50:01 malc Exp $";
static char *HeaderId = HEADERID;
static char *ConfigId = CONFIGID;
static char *VersionId = VERSIONID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup();
extern char *vstrcat(), *get_user(), *get_home();
extern char *get_mailbox(), *get_vtext(), *expand();
extern char *fullpath(), *alias(), *a_strerror();
extern int getopt(), isatty(), nonempty();
extern int count_buffers(), count_windows();
extern void free(), free_forms(), exit(), init_keymaps();
extern void init_history(), init_vars(), reset_var();
extern void read_aliases(), send_mail(), quiet_mail();
extern void init_tcntrl(), init_tmodes(), init_vterm();
extern void end_tmodes(), interactive(), silent();
extern void errors_to_typeout(), msg(), msgl(), cmsg();
extern void emsgl(), show_initial_msg(), tbeep();
extern void show_buffer(), first_display(), hide_cursor();
extern void do_commands(), run_script();
extern RETSIGTYPE (*signal())();
extern WINDOW *init_windows();
extern MAILBUF *add_buffer();
extern FORM *load();

#ifndef READ_VIA_PIPES
extern void save_ids();
#endif /* ! READ_VIA_PIPES */

#ifdef READ_VIA_POP3
extern void kill_pop3();
#endif /* READ_VIA_POP3 */

/* Local function declarations */

int finish_startup(), startup_display();
void panic();
static int read_startup(), read_one_file();
static void set_signals(), add_folder();
static void del_folder(), chk_pending();
static void chk_exists(), show_version();
static void usage();
static RETSIGTYPE terminate();
static USEROPTS *getargs();

/****************************************************************************/
/* Import the argument, index and error values for getopt() */

extern char *optarg;
extern int optind, opterr;

/****************************************************************************/
/* Import the current window from commands.c */

extern WINDOW *cwin;

/****************************************************************************/
/* The user options are file-scoped */

static USEROPTS *options;

/****************************************************************************/
int main(argc, argv)
int argc;
char **argv;
{
	/* Process and handle the arguments */

	char *users = NULL;
	int sending;

#ifndef READ_VIA_PIPES
	/* Revert to the real uid and gid */

	save_ids();
#endif /* ! READ_VIA_PIPES */

	/* Set up the signal handling */

	set_signals();

	/* Initialise the keymaps, variables and history */

	init_keymaps();
	init_vars();
	init_history();

	/* Parse the command line arguments */

	options = getargs(argc, argv);

	/* Handle just printing the version */

	if (options->version) {
		show_version();
		return(0);
	}

	/* Are we in sending or interactive mode? */

	sending = (options->users != NULL || options->edit);

	/* Initialise the terminal */

	init_tcntrl();
	init_tmodes();
	init_vterm();

	/* Check that stdin & stdout are ok */

	if (!isatty(fileno(stdin)) && !sending) {
		panic("Stdin not a tty");
	}
	if (!isatty(fileno(stdout)) && (!sending || options->edit)) {
		panic("Stdout not a tty");
	}

	/* Handle the 'does mail exist' and 'only if exists' options */

	if (!sending && (options->exists || options->zero)) {
		chk_exists(options);
	}

	/* Become interactive if required */

	if (!sending && options->script == NULL) {
		interactive();
		show_initial_msg();
	} else if (options->script != NULL) {
		silent();
	}

	/* Redirect errors to typeout */

	errors_to_typeout(TRUE);

	/* Read the startup file, verbosely unless sending from stdin */

	if (!read_startup(options->loadfile, options->nostartup,
			  isatty(fileno(stdin)) || options->edit)) {
		/* Exit in startup file; honour it */

		end_tmodes();
		return(0);
	}

	/* Finish initialising af */

	(void) finish_startup();

	/* Handle sending mode */

	if (sending) {
		/* Alias and expand the user list */

		if (options->users != NULL &&
		    (users = alias(options->users)) == NULL) {
			/* Managed to fail the alias */

			panic(a_strerror());
		}

		if (options->edit || isatty(fileno(stdin))) {
			send_mail(users, options->subject);
		} else {
			quiet_mail(users, options->subject);
		}

		/* Clean up the display before we exit */

		end_tmodes();
		(void) putchar('\n');
		return(0);
	}

	/* Start up buffers and windows */

	(void) startup_display();

	/* Now go and handle commands, from the keyboard or a script file */

	if (options->script != NULL) {
		run_script(options->script);
	} else {
		do_commands();
	}

#ifdef READ_VIA_POP3
	/* Close any open POP3 folders */

	kill_pop3();
#endif /* READ_VIA_POP3 */

	/* Clean up the terminal and exit */

	hide_cursor();
	end_tmodes();
	return(0);
}
/****************************************************************************/
int finish_startup()
{
	/* Finish basic af initialisation */

	static int startup_completed = FALSE;
	int sending;

	/* First check if we've already initialised */

	if (startup_completed) {
		return(FALSE);
	}

	/* Are we in sending or interactive mode? */

	sending = (options->users != NULL || options->edit);

	/* Read the alias files */

	read_aliases(isatty(fileno(stdin)) || options->edit);

	/* Normalise errors unless required later */

	errors_to_typeout(!sending && options->folders[1] != NULL);

	/* Handle the 'edit headers' option */

	if (options->hdrs) {
		(void) reset_var(V_EDIT_IHDRS, VI_TRUE);
	}

	/* We have now finished basic af startup */

	startup_completed = TRUE;
	return(TRUE);
}
/****************************************************************************/
int startup_display()
{
	/* Finish initialising af from the user uptions */

	char *pfolder;
	int nwindows, nbuffers;
	int folder;
	MAILBUF *buflist, *buf;
	WINDOW *winlist, *win;

	/* Check if we've already started the display */

	if (cwin != NULL || options->users != NULL || options->edit) {
		/* Success if already done; failed otherwise */

		return(cwin != NULL);
	}

	/* Check if we have a pending folder */

	chk_pending(options);

	/* Initialise the scratch buffer */

	buf = buflist = add_buffer(NULL, SCRATCHBUF, NULL, NULL, M_MAIL);

	/* Make and read the required buffers */

	for (folder = 0; options->folders[folder] != NULL; folder++) {
		/* Is there a pending folder for this buffer? */

		pfolder = (options->pending) ?
			fullpath(get_mailbox(NULL)) : NULL;

		/* Make and read the buffer */

		buf = add_buffer(buf, NULL, options->folders[folder],
				 pfolder, M_MAIL);
	}

	/* Count the active buffers */

	nbuffers = count_buffers(buflist) - 1;
	nbuffers = (nbuffers > 0) ? nbuffers : 1;

	/* Set up as many windows as we need */

	cwin = winlist = init_windows((options->windows) ? nbuffers : 1);
	nwindows = count_windows(winlist);

	/* Show each buffer in a window */

	win = winlist;
	while (nwindows--) {
		show_buffer(win, buf);
		win = win->next;
		buf = buf->next;
	}

	/* Ensure errors are being handled normally */

	errors_to_typeout(FALSE);

	/* Display all the available windows */

	win = winlist;
	do {
		first_display(win);
		win = win->next;
	} while(win != winlist);

	/* And hide the cursor */

	hide_cursor();

	/* Now we return success */

	return(TRUE);
}
/****************************************************************************/
void panic(text)
char *text;
{
	/* Internal panic - shut down and exit, displaying text */

	tbeep();
	msg(text);
	end_tmodes();
	(void) putchar('\n');

	exit(2);
}
/****************************************************************************/
static void set_signals()
{
	/* Set up the signal handling for several signals */

	(void) signal(SIGINT, SIG_IGN);		/* Ignore interrupts */
	(void) signal(SIGQUIT, terminate);	/* Cleanup on SIGQUIT */
	(void) signal(SIGTERM, terminate);	/* Cleanup on SIGTERM */

	(void) signal(SIGALRM, SIG_IGN);	/* Ignore alarm signals */
	(void) signal(SIGPIPE, SIG_IGN);	/* Ignore bad pipe writes */

#ifdef HAVE_JOBCONTROL
	(void) signal(SIGTSTP, SIG_IGN);	/* Ignore stop signal */
#endif /* HAVE_JOBCONTROL */

	return;
}
/****************************************************************************/
/*ARGSUSED*/
static RETSIGTYPE terminate(st)
int st;
{
	/* Terminate after catching a signal */

	panic("Af terminated by signal");
	/*NOTREACHED*/
}
/****************************************************************************/
static USEROPTS *getargs(argc, argv)
int argc;
char **argv;
{
	/* Check and parse the user options */

	static USEROPTS options;
	int all_folders = FALSE;
	int opt, i, len = 0;

	/* Turn off getopt() messages */

	opterr = 0;

	/* Initialize the user options */

	options.exists = options.zero = FALSE;
	options.edit = options.hdrs = FALSE;
	options.nostartup = options.windows = FALSE;
	options.pending = options.version = FALSE;
	options.folders = NULL;
	options.loadfile = options.script = NULL;
	options.subject = options.users = NULL;

	/* Now parse the command line */

	while ((opt = getopt(argc, argv, OPTS)) != EOF) {
		switch (opt) {
		case 'e':
			options.exists = TRUE;
			break;
		case 'z':
			options.zero = TRUE;
			break;
		case 'E':
			options.edit = TRUE;
			break;
		case 'H':
			options.hdrs = TRUE;
			break;
		case 'n':
			options.nostartup = TRUE;
			break;
		case 'w':
			options.windows = TRUE;
			break;
		case 'l':
			options.loadfile = expand(optarg);
			break;
		case 'f':
			add_folder(&options, fullpath(optarg));
			break;
		case 'F':
			all_folders = TRUE;
			break;
		case 'u':
			add_folder(&options, fullpath(get_mailbox(optarg)));
			break;
		case 'S':
			if (options.script != NULL) {
				usage(argv[0]);
			}
			options.script = expand(optarg);
			break;
		case 's':
			if (options.subject != NULL) {
				usage(argv[0]);
			}
			options.subject = xstrdup(optarg);
			break;
		case 'v':
			options.version = TRUE;
			break;
		case '?':
			/* Give a usage message and quit */

			usage(argv[0]);
		}
	}

	/* Handle any additional arguments as folders if required */

	for (i = optind; all_folders && i < argc; i++) {
		/* Add this argument to the list of folders */

		add_folder(&options, fullpath(argv[i]));
	}

	/* Default folder to mailbox if none given */

	if (options.folders == NULL) {
		/* Set up the default folder */

		add_folder(&options, fullpath(get_mailbox(NULL)));
		options.pending = TRUE;
	}

	/* How long is the list of users to mail to? */

	for (i = optind; !all_folders && i < argc; i++) {
		len += strlen(argv[i]) + 1;
	}

	/* Allocate space and copy the first address */

	if (len) {
		options.users = xmalloc(len);
		(void) strcpy(options.users, argv[optind]);
	}

	/* And copy the remaining addresses into the list */

	for (i = optind + 1; len && i < argc; i++) {
		/* Add this address to the list */

		(void) strcat(options.users, " ");
		(void) strcat(options.users, argv[i]);
	}

	/* Return a pointer to the static buffer */

	return(&options);
}
/****************************************************************************/
static int read_startup(loadfile, nostartup, verbose)
char *loadfile;
int nostartup, verbose;
{
	/* Read the system and user startup files */

	char *filnam;

	/* Execute any system startup file */

	if (!read_one_file(SYS_STARTUPFILE, verbose)) {
		return(FALSE);
	}

	/* Execute any per-user startup file if required */

	filnam = vstrcat(get_home(NULL), "/", STARTUPFILE, NULL);
	if (!nostartup && !read_one_file(filnam, verbose)) {
		free(filnam);
		return(FALSE);
	}
	free(filnam);

	/* Finally, load any startup file requested by the user */

	if (loadfile != NULL && !read_one_file(loadfile, verbose)) {
		return(FALSE);
	}

	/* Completed startup files */

	return(TRUE);
}
/****************************************************************************/
static int read_one_file(filnam, verbose)
char *filnam;
int verbose;
{
	/* Read a single startup file and return status */

	char *fullname, *suffixed;
	FORM *status;

	/* Generate the suffixed form of the name */

	suffixed = vstrcat(filnam, AFLISP, NULL);

	/* Check if the file exists */

	if (!access(filnam, 00)) {
		/* The file exists, copy the name */

		free(suffixed);
		fullname = xstrdup(filnam);
	} else if (!access(suffixed, 00)) {
		/* The suffixed file exists */

		fullname = suffixed;
	} else {
		/* No file to read */

		free(suffixed);
		return(TRUE);
	}

	/* Give the user a message if verbose */

	if (verbose) {
		msgl("Loading ", fullname, "...", NULL);
	}

	/* Load the file and check status */

	if ((status = load(fullname)) == NULL) {
		/* Honour the exit */

		free(fullname);
		return(FALSE);
	}

	/* Clean up and return success */

	free_forms(status);
	free(fullname);
	return(TRUE);
}
/****************************************************************************/
static void add_folder(options, folder)
USEROPTS *options;
char *folder;
{
	/* Add the folder to the list of folders in options */

	char **f;
	int nfolders = 0;

	/* Check if we need to initialise the list */

	if (options->folders == NULL) {
		options->folders = (char **) xmalloc(2 * sizeof(char *));
	} else {
		/* Count the number of folders set so far */

		for (f = options->folders; *f != NULL; f++) {
			nfolders++;
		}

		/* Resize the array */

		options->folders = (char **)
			xrealloc(options->folders, (nfolders + 2)
				 * sizeof(char *));
	}

	/* Add the folder to the list */

	options->folders[nfolders] = folder;
	options->folders[nfolders + 1] = NULL;
	return;
}
/****************************************************************************/
static void del_folder(options, folder)
USEROPTS *options;
int folder;
{
	/* Delete the folder from the list of folders in options */

	char **f;
	int nfolders = 0;

	/* Left-shift the entries in the list */

	while(options->folders[folder] != NULL) {
		options->folders[folder] = options->folders[folder + 1];
		folder++;
	}

	/* Count the number of folders set so far */

	for (f = options->folders; *f != NULL; f++) {
		nfolders++;
	}

	/* Resize the array */

	options->folders = (char **)
		xrealloc(options->folders, (nfolders + 1) * sizeof(char *));
	return;
}
/****************************************************************************/
static void chk_exists(options)
USEROPTS *options;
{
	/* Handle the exists and nonzero options if set */

	int folder = 0, exists;

	/* Loop over each specified folder */

	while (options->folders[folder] != NULL) {
		/* Is there mail in the folder? */

		exists = nonempty(options->folders[folder]);

		/* Print a message for the exists option */

		if (options->exists) {
			/* Restore the terminal modes while printing */

			end_tmodes();
			(void) printf("%s mail in %s.\n", (exists)
				      ? "There is" : "No",
				      options->folders[folder]);
			init_tmodes();
		}

		/* Delete the folder from the list if required */

		if (!exists) {
			del_folder(options, folder);
		} else {
			folder++;
		}
	}

	/* Exit if exists option set or no folders left */

	if (options->exists || options->folders[0] == NULL) {
		end_tmodes();
		exit((options->folders[0] != NULL) ? 0 : 1);
	}
	return;
}
/****************************************************************************/
static void chk_pending(options)
USEROPTS *options;
{
	/* Set up to handle a pending folder */

	char *pfolder = NULL;

	/* Check if we are handling a pending folder */

	if (options->pending && (pfolder = get_vtext(V_PENDING)) != NULL) {
#ifdef READ_VIA_POP3
		/* Can't make a buffer pending on a POP3 folder */

		if (POP3_MBOX(pfolder)) {
			/* Give an error and ignore pfolder */

			emsgl("Can't use POP3 mailbox ", pfolder,
			      " as a pending folder", NULL);
			options->pending = FALSE;
			return;
		}
#endif /* READ_VIA_POP3 */

		/* Substitute the pending folder for the mailbox */

		del_folder(options, 0);
		add_folder(options, fullpath(pfolder));
	}

	/* Update the pending options flag */

	options->pending = (options->pending && pfolder != NULL);
	return;
}
/****************************************************************************/
static void show_version()
{
	/* Display the version information to stdout */

	printf("This is %s version %s, released %s by %s\n",
	       PROGNAME, VERSION, RELEASE_DATE, AUTHOR);
	return;
}
/****************************************************************************/
static void usage(prognam)
char *prognam;
{
	/* Print a usage message and exit abnormally */

	(void) fprintf(stderr, "Usage : %s [ -envwzEFHX ][ -l loadfile ]\
[ -S scriptfile ]\n\t[ -f folder ] [ -u user ][ -s subject ] [ user ... ]\n",
		       prognam);
	exit(2);
}
/****************************************************************************/
