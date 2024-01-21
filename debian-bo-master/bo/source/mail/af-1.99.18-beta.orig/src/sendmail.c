/* Sendmail.c - Af routines to send mail outwards.
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
#include <fcntl.h>
#include <ctype.h>
#include <varargs.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "sendmail.h"
#include "variable.h"
#include "complete.h"
#include "misc.h"
#include "io.h"
#include STRING_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: sendmail.c,v 1.48 1997/05/06 16:16:26 malc Exp $";
static char *SendmailId = SENDMAILID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup(), *tempnam();
extern char *vstrcat(), *ttyname(), *strerror(), *strdate();
extern char *unmessage(), *alias(), *get_vtext(), *get_ecstr();
extern char *utos(), *get_hdr(), *get_line(), *copy_stdin_mail();
extern int wait(), isatty(), strcasecmp(), strncasecmp();
extern int mklower(), write_text(), get_vval(), confirm();
extern int get_key(), shellout();
extern unsigned sleep(), save_atimer(), restore_atimer();
extern void free(), _exit(), free_hdrs(), typeout(), showtext();
extern void msg(), msgl(), emsgl(), cmsg(), tbeep();
extern void reset_signals();
extern HEADER *init_hdrs(), *form_stdin_mail();
extern HEADER *form_hdrs(), *form_mail();
extern HEADER *posting_hdrs(), *saving_hdrs();
extern DATEZONE *date_now();
extern CLIST *fn_complete();

#ifdef MTA_IS_SMTP
extern int smtp_deliver();
extern char *get_addr();
#endif /* MTA_IS_SMTP */

#ifndef MTA_MMDF_FORMAT
extern char *get_addr(), *strudate();
#endif /* ! MTA_MMDF_FORMAT */

#ifdef MTA_NEEDS_ARGS
extern char **addr_args();
#endif /* MTA_NEEDS_ARGS */

/* Local function declarations */

int cp_hdrs();
static char *get_sigfile(), **mailargs();
static int do_send(), reset_stdin(), ask_copy();
static int cp_text(), exec_mta(), post();
static int save_outbound();
static unsigned mail_size();
static void summarize_mail(), list_mail();
static void spell_check_mail();

#ifndef MTA_IS_SMTP
static char **argvec();
#endif /* ! MTA_IS_SMTP */

#ifdef MTA_NEEDS_ARGS
static char *set_addrs();
#endif /* MTA_NEEDS_ARGS */

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the user quit flag from commands.c */

extern int user_quit;

/****************************************************************************/
void send_mail(to, subject)
char *to, *subject;
{
	/* Simple send out of edited file */

	(void) do_send(to, NULL, subject, NULL, T_SEND);
	return;
}
/****************************************************************************/
int send_reply(to, cc, orig_msg)
char *to, *cc;
MESSAGE *orig_msg;
{
	/* Send mail in reply to some source */

	return(do_send(to, cc, orig_msg->subject, orig_msg, T_REPLY));
}
/****************************************************************************/
int send_forward(to, orig_msg)
char *to;
MESSAGE *orig_msg;
{
	/* Send existing message, possibly editing */

	return(do_send(to, NULL, orig_msg->subject, orig_msg, T_FORWARD));
}
/****************************************************************************/
int send_bounce(to, orig_msg)
char *to;
MESSAGE *orig_msg;
{
	/* Bounce mail - send with original headers */

	return(do_send(to, NULL, NULL, orig_msg, T_BOUNCE));
}
/****************************************************************************/
void quiet_mail(to, subject)
char *to, *subject;
{
	/* Send from standard input with no interaction */

	(void) do_send(to, NULL, subject, NULL, T_SILENT);
	return;
}
/****************************************************************************/
static int do_send(to, cc, subject, orig_msg, mail_type)
char *to, *cc, *subject;
MESSAGE *orig_msg;
int mail_type;
{
	/* Form a mail message, and send it after confirmation */

	char *tfile, *etfile, *sigfile;
	char *spellcheck;
	int fd, copy, status;
	HEADER *hdrs;

	/* Get a temporary file */

	if ((tfile = tempnam(TFILEDIR, TFILEPFX)) == NULL) {
		emsgl("Can't create temporary file ", strerror(errno), NULL);
		return(FALSE);
	}
	if ((fd = creat(tfile, 0666)) < 0) {
		emsgl("Can't create temporary file ", strerror(errno), NULL);
		free(tfile);
		return(FALSE);
	}
	(void) close(fd);

	/* Initialise the headers */

	hdrs = init_hdrs(to, cc, subject, orig_msg, mail_type);

	/* If stdin isn't a tty then read it into the temporary file */

	if (!isatty(fileno(stdin)) &&
	    ((etfile = copy_stdin_mail()) == NULL || !reset_stdin() ||
	     (hdrs = form_stdin_mail(tfile, etfile, hdrs,
				     mail_type)) == NULL)) {
		/* Failed to copy the input or reset the tty */

		if (etfile != NULL) {
			(void) unlink(etfile);
			free(etfile);
		}
		(void) unlink(tfile);
		free(tfile);
		free_hdrs(hdrs);
		return(FALSE);
	}

	/* Form the default headers for the message */

	if ((hdrs = form_hdrs(hdrs, mail_type)) == NULL) {
		(void) unlink(tfile);
		free(tfile);
		return(FALSE);
	}

	/* See if we want to copy or edit the message */
		
	copy = (mail_type == T_REPLY) ? ask_copy() : orig_msg != NULL;
		
	/* Check if the user quit */

	if (user_quit) {
		(void) unlink(tfile);
		free(tfile);
		free_hdrs(hdrs);
		return(FALSE);
	}

	/* Check what signature file to use */

	if ((sigfile = get_sigfile(mail_type)) == NULL && user_quit) {
		(void) unlink(tfile);
		free(tfile);
		free_hdrs(hdrs);
		return(FALSE);
	}

	/* If we are copying text then do so */

	if (copy && !cp_text(orig_msg, tfile, mail_type)) {
		(void) unlink(tfile);
		free(tfile);
		free_hdrs(hdrs);
		return(FALSE);
	}

	/* Do any initial edit of the outgoing text and get the headers */

	if ((hdrs = form_mail(tfile, sigfile, hdrs,
			      mail_type, TRUE)) == NULL) {
		(void) unlink(tfile);
		free(tfile);
		return(FALSE);
	}

	/* Post silent mail now */

	if (mail_type == T_SILENT) {
		(void) post(tfile, hdrs, FALSE);
		(void) unlink(tfile);
		free(tfile);
		free_hdrs(hdrs);
		return(TRUE);
	}

	/* Get the interactive spelling checker to use */

	spellcheck = get_vtext(V_SPELLCHECK);

	/* Summarise the message's headers */

	summarize_mail(hdrs);

	/* Loop until the user selects send or forget */

	for (;;) {
		/* Prompt for what to do next */

		msgl("Send, Edit, ", (spellcheck == NULL) ? "" :
		     "Check Spelling, ", "List or Forget?", NULL);

		/* Now handle the user response */

		switch (mklower(get_key())) {
		case 's':
			/* Send the mail to the MTA */

			status = post(tfile, hdrs, TRUE);
			(void) unlink(tfile);
			free(tfile);
			free_hdrs(hdrs);
			return(status);

		case 'e':
			/* Edit the outgoing message again */

			if ((hdrs = form_mail(tfile, NULL, hdrs, mail_type,
					      FALSE)) == NULL) {
				(void) unlink(tfile);
				free(tfile);
				return(FALSE);
			}
			summarize_mail(hdrs);
			break;

		case 'c':
			/* Spell check the outgoing message */

			if (spellcheck != NULL) {
				spell_check_mail(spellcheck, tfile, hdrs);
				summarize_mail(hdrs);
			} else {
				tbeep();
			}
			break;

		case 'l':
			/* List the message to typeout */

			list_mail(tfile, hdrs);
			summarize_mail(hdrs);
			break;

		case 'f':
			/* Forget about the message */

			msg("(Mail forgotten)");
			(void) unlink(tfile);
			free(tfile);
			free_hdrs(hdrs);
			return(FALSE);

		default:
			/* Invalid user response */

			tbeep();
			break;
		}
	}
	/*NOTREACHED*/
}
/****************************************************************************/
static int reset_stdin()
{
	/* Reset stdin to the terminal */

	char *term;

	/* Get the name of the terminal from stdout */

	term = ttyname(fileno(stdout));

	/* Reopen stdin to the terminal */

	if (freopen(term, "r", stdin) == NULL) {
		emsgl("Can't open ", term, ": ", strerror(errno), NULL);
		return(FALSE);
	}

	/* All went ok */

	return(TRUE);
}
/****************************************************************************/
static int ask_copy()
{
	/* Ask the user whether to copy message on reply */

	int yes;

	switch (get_vval(V_COPY)) {
	case V_FALSE:
		return(FALSE);
	case V_TRUE:
		return(TRUE);
	}

	/* Ask if we want to copy */

	yes = confirm("Copy message? ", FALSE);
	return(yes);
}
/****************************************************************************/
static char *get_sigfile(mail_type)
int mail_type;
{
	/* Determine and return the user's signature file */

	char *sigfile, *prompt;
	char *base_prompt = "Signature file: ";

	/* Get the users's default signature file */

	sigfile = get_vtext(V_SIGFILE);

	/* Check if a signature file is defined or required */

	if (sigfile == NULL || mail_type == T_BOUNCE) {
		return(NULL);
	}

	/* If ask: is not set then just return sigfile */

	if (strncasecmp(sigfile, V_ASK_SIGFILE, strlen(V_ASK_SIGFILE))) {
		return(sigfile);
	}

	/* Get the default signature file */

	sigfile = sigfile + strlen(V_ASK_SIGFILE);

	/* If we are mailing silently then return the default signature */

	if (mail_type == T_SILENT) {
		return(sigfile);
	}

	/* Ask the user which signature file */

	prompt = xstrdup(base_prompt);
	sigfile = get_ecstr(NULL, prompt, sigfile, fn_complete, C_CAUTIOUS);
	free(prompt);

	return(sigfile);
}
/****************************************************************************/
static int cp_text(orig_msg, tfile, mail_type)
MESSAGE *orig_msg;
char *tfile;
int mail_type;
{
	/* Copy the text of orig_msg into tfile */
	
	char *hlist, *prefix, *preface;
	int  hskip, status;
	FILE *fp;

	/* Open the file */

	if ((fp = fopen(tfile, "w")) == NULL) {
		emsgl("Can't open ", tfile, ": ", strerror(errno), NULL);
		return(FALSE);
	}

	/* Decide which headers to copy */

	hskip = (mail_type == T_BOUNCE) ? HS_FROM : HS_COPY;
	hlist = (mail_type == T_BOUNCE) ? NULL : get_vtext(V_TOCOPY);

	/* Determine the prefix for the copy */

	prefix = (mail_type == T_REPLY || mail_type == T_FORWARD)
		? get_vtext(V_COPY_PFX) : NULL;

	/* Preface the copied text if required */

	if (mail_type != T_BOUNCE && (preface = get_vtext(V_PREFACE)) != NULL
	    && (preface = unmessage(orig_msg, preface, FOLD_WIDTH)) != NULL
	    && (fputs(preface, fp) == EOF || fputc('\n', fp) == EOF)) {
		/* Error writing copy preface */

		emsgl("Error writing temporary file: ",
		      strerror(errno), NULL);
		(void) fclose(fp);
		return(FALSE);
	}

	/* Write the text */

	if (status = write_text(fp, orig_msg, prefix, hlist, hskip, FALSE)) {
		/* Error writing copied text */

		emsgl("Error writing temporary file: ",
		      strerror(status), NULL);
		(void) fclose(fp);
		return(FALSE);
	}

	/* Close the file and return ok */

	(void) fclose(fp);
	return(TRUE);
}
/****************************************************************************/
static void summarize_mail(hdrs)
HEADER *hdrs;
{
	/* Summarize the mail; to be sent via showtext */

	HEADER *h;

	/* First Let the user know what they can see */

	showtext("Summary of message headers: \n\n");

	/* Then list the headers via showtext */

	for (h = hdrs; !user_quit && h != NULL; h = h->next) {
		/* Should this header be listed? */

		if (h->edit && h->hdr_text != NULL) {
			showtext(h->hdr_name);
			showtext(" ");
			showtext(h->hdr_text);
			showtext("\n");
		}
	}

	/* End the text and reset the quit flag */

	showtext(NULL);
	user_quit = FALSE;
	return;
}
/****************************************************************************/
static void list_mail(tfile, hdrs)
char *tfile;
HEADER *hdrs;
{
	/* List the mail to be sent to typeout */

	char buf[BUFSIZ];
	FILE *fp;
	HEADER *h;

	/* First list the headers */

	for (h = hdrs; !user_quit && h != NULL; h = h->next) {
		/* Show the header if it is set */

		if (h->hdr_text != NULL) {
			typeout(h->hdr_name);
			typeout(" ");
			typeout(h->hdr_text);
			typeout("\n");
		}
	}

	/* Now list the message, if any */

	if (!user_quit && (fp = fopen(tfile, "r")) != NULL) {
		/* Show the blank line */

		typeout("\n");

		/* And the the message text */

		while (!user_quit && fgets(buf, BUFSIZ, fp) != NULL) {
			typeout(buf);
		}

		/* Now close the file */

		(void) fclose(fp);
	}

	/* End the typeout and reset the quit flag */

	typeout(NULL);
	user_quit = FALSE;
	return;
}
/****************************************************************************/
void spell_check_mail(spellcheck, tfile, hdrs)
char *spellcheck, *tfile;
HEADER *hdrs;
{
	/* Interactively spell-check an outgoing message */

	char *cmd;
	ATIMER tbuf;

	/* Build the command to run the spell checker */

	cmd = vstrcat(spellcheck, " ", tfile, NULL);

	/* Now run the interactive spell checker */

	if (!shellout(cmd, FALSE)) {
		/* Spell check failed, update the display */

		summarize_mail(hdrs);

		/* And pause so the user can see the message */

		(void) save_atimer(&tbuf);
		(void) sleep(ECHO_DELAY);
		(void) restore_atimer(&tbuf);
	}

	/* Clean up and return */

	free(cmd);
	return;
}
/****************************************************************************/
static int post(mailfile, hdrs, verbose)
char *mailfile;
HEADER *hdrs;
int verbose;
{
	/* Send mail via the MTA */

	char **argv, **p;
	char *ofolder, *prompt;
	int status, othreshold;
	unsigned mlines;
	FILE *fp;

	/* Get the folder and threshold for saving outbound messages */

	ofolder = get_vtext(V_OUTBOUND);
	othreshold = get_vval(V_THRESHOLD);
	mlines = mail_size(mailfile);

	/* Check if we want to save a long message */

	if (ofolder != NULL && (othreshold > 0) && othreshold < mlines) {
		prompt = vstrcat("Save ", utos(mlines), "-line message to ",
				 ofolder, "? ", NULL);

		/* Get confirmation for the save if possible */

		if (!verbose || !confirm(prompt, FALSE)) {
			/* Check if the user quit */

			if (verbose && user_quit) {
				return(FALSE);
			}
			ofolder = NULL;
		}
		free(prompt);
	}

	/* Tell the user what we're doing */

	msg("Sending mail...");

	/* Open the mail body file */

	if ((fp = fopen(mailfile, "r")) == NULL) {
		emsgl("Can't open temporary file: ", strerror(errno), NULL);
		return(FALSE);
	}

	/* Set up the command we're going to execute */

	argv = mailargs(hdrs);

	/* Add any headers needed before we send */

	hdrs = posting_hdrs(hdrs);

	/* Now submit the mail by the required method */

#ifdef MTA_IS_SMTP
	/* Submit the mail to a remote host via SMTP */

	status = smtp_deliver(get_vtext(V_SMTP_HOST),
			get_addr(), argv, fp, hdrs);
#else /* ! MTA_IS_SMTP */
	/* Exec the MTA and pipe the mail into it */

	status = exec_mta(argv, fp, hdrs);
#endif /* MTA_IS_SMTP */

	/* Close the mail body file */

	(void) fclose(fp);

	/* Save the message to the outbound folder if required */
	
	if (status && save_outbound(ofolder, mailfile, hdrs)) {
		cmsg(" Done");
	}

	/* Free the argument vector and return */

	for (p = argv; *p != NULL; p++) {
		free(*p);
	}
	free(argv);
	return(status);
}
/****************************************************************************/
static int exec_mta(argv, mail_fp, hdrs)
char **argv;
FILE *mail_fp;
HEADER *hdrs;
{
	/* Actually send the mail, by spawning the MTA */

	char buf[BUFSIZ];
	int fds[2], fd;
	int status = 0;
	FILE *fp;

	/* Set up the file descriptors for the pipe */

	if (pipe(fds) < 0) {
		emsgl("Mail delivery failed: ", strerror(errno), NULL);
		return(FALSE);
	}

	/* Now we fork */

	switch(fork()) {
	case -1:						/* Failed */
		emsgl("Mail delivery failed: ", strerror(status), NULL);
		return(FALSE);

	case 0:							/* Child */
		/* Make stdin the read end of the pipe */

		(void) close(fds[1]);
		(void) dup2(fds[0], 0);

		/* Redirect stdout & stderr to /dev/null if possible */

		if ((fd = open(BITBUCKET, O_WRONLY, 0)) >= 0) {
			/* Redirect stdout and stderr */

			(void) dup2(fd, 1);
			(void) dup2(fd, 2);
		}

		/* Reset the signal mask and handlers */

		reset_signals();

		/* Exec the MTA or exit if we have a disaster */

		(void) execv(argv[0], argv);
		_exit(1);

	default:						/* Parent */
		/* Open a file pointer from the write descriptor */

		(void) close(fds[0]);
		fp = fdopen(fds[1], "w");

		/* Write the message headers and body to the pipe */

		(void) cp_hdrs(fp, hdrs, TRUE);
		while (fgets(buf, BUFSIZ, mail_fp) != NULL) {
			(void) fputs(buf, fp);
		}

		/* Close the pipe and wait for the child */

		(void) fclose(fp);
		(void) wait(&status);

		/* And return success */

		return(TRUE);
	}
	/*NOTREACHED*/
}
/****************************************************************************/
static unsigned mail_size(mailfile)
char *mailfile;
{
	/* Return the number of lines in mailfile */

	char buf[LINESIZE + 1];
	unsigned lines = 0;
	FILE *fp;

	/* Open the mail body file */

	if ((fp = fopen(mailfile, "r")) == NULL) {
		return(0);
	}

	/* Scan the file for lines */

	while (fgets(buf, LINESIZE, fp) != NULL) {
		lines++;
	}

	/* Close the file and return the number of lines */

	(void) fclose(fp);
	return(lines);
}
/****************************************************************************/
static int save_outbound(outbound, mailfile, hdrs)
char *outbound, *mailfile;
HEADER *hdrs;
{
	/* Save a copy of the message to any outbound folder */

	char *fromline, *buf;
	int status;
	FILE *fp, *ofp;

	/* If no outbound folder then return TRUE */

	if (outbound == NULL) {
		return(TRUE);
	}

	/* Open the mail body file */

	if ((fp = fopen(mailfile, "r")) == NULL) {
		emsgl("Error opening temporary file: ",
		      strerror(errno), NULL);
		return(FALSE);
	}

	/* Open the outbound folder to append */

	if ((ofp = fopen(outbound, "a")) == NULL) {
		emsgl("Can't open ", outbound, ": ",
		      strerror(errno), NULL);
		(void) fclose(fp);
		return(FALSE);
	}

	/* Add any headers needed before we save */

	hdrs = saving_hdrs(hdrs, mailfile);

#ifdef MTA_MMDF_FORMAT
	/* Write a header delimiter to the outbound folder */

	if (fputs(MMDF_DELIM, ofp) == EOF) {
		emsgl("Error writing ", outbound,
		      ": ", strerror(errno), NULL);
		(void) fclose(fp);
		(void) fclose(ofp);
		return(FALSE);
	}
#endif /* ! MTA_MMDF_FORMAT */

	/* Write a From line to the outbound folder */

	fromline = vstrcat(MFROM, get_addr(), " ", 
			   strudate(date_now()), "\n", NULL);

	if (fputs(fromline, ofp) == EOF) {
		emsgl("Error writing ", outbound,
		      ": ", strerror(errno), NULL);
		free(fromline);
		(void) fclose(fp);
		(void) fclose(ofp);
		return(FALSE);
	}
	free(fromline);

	/* Append the headers to the outbound folder */

	if (status = cp_hdrs(ofp, hdrs, TRUE)) {
		emsgl("Error writing ", outbound,
		      ": ", strerror(status), NULL);
		(void) fclose(fp);
		(void) fclose(ofp);
		return(FALSE);
	}

	/* And the message text */

	while ((buf = get_line(fp, FALSE)) != NULL) {
#ifndef MTA_MMDF_FORMAT
		/* Quote Delimiters in the message body */

		if (!strncmp(buf, MFROM, strlen(MFROM))
		    && fputs(FROM_QUOTE, ofp) == EOF) {
			emsgl("Error writing ", outbound,
			      ": ", strerror(errno), NULL);
			free(buf);
			(void) fclose(fp);
			(void) fclose(ofp);
			return(FALSE);
		}
#endif /* ! MTA_MMDF_FORMAT */

		/* Now write the line to the folder */

		if (fputs(buf, ofp) == EOF) {
			emsgl("Error writing ", outbound,
			      ": ", strerror(errno), NULL);
			free(buf);
			(void) fclose(fp);
			(void) fclose(ofp);
			return(FALSE);
		}

		/* And free the buffer */

		free(buf);
	}

	/* Close the mail body file */

	(void) fclose(fp);

	/* Append a delimiter to the folder */

#ifdef MTA_MMDF_FORMAT
	if (fputs(MMDF_DELIM, ofp) == EOF) {
		emsgl("Error writing ", outbound,
		      ": ", strerror(errno), NULL);
		(void) fclose(ofp);
		return(FALSE);
	}
#else /* ! MTA_MMDF_FORMAT */
	if (putc('\n', ofp) == EOF) {
		emsgl("Error writing ", outbound,
		      ": ", strerror(errno), NULL);
		(void) fclose(ofp);
		return(FALSE);
	}
#endif /* ! MTA_MMDF_FORMAT */

	/* Close the outbound folder and return */

	(void) fclose(ofp);
	return(TRUE);
}
/****************************************************************************/
int cp_hdrs(fp, hdrs, send)
FILE *fp;
HEADER *hdrs;
int send;
{
	/* Write the headers contained in hdrs to the file pointer fp */

	HEADER *h;
	
	/* Loop over the headers */

	for (h = hdrs; h != NULL; h = h->next) {
		/* Do we need to send this header? */

		if (send && (h->hdr_text != NULL)
		    || !send && h->edit && h->show) {
#ifdef NO_MTA_CC
			/* Handle the Bcc header when sending */

			if (send && (!strcasecmp(h->hdr_name, BCC) ||
				     !strcasecmp(h->hdr_name, RESENT_BCC))) {
				/* Might want an empty Bcc: header */

				if (get_hdr(TO, hdrs) == NULL &&
				    get_hdr(RESENT_TO, hdrs) == NULL
				    && get_hdr(CC, hdrs) == NULL &&
				    get_hdr(RESENT_CC, hdrs) == NULL
				    && (fputs(h->hdr_name, fp) == EOF
					|| putc('\n', fp) == EOF)) {
					/* Error writing the header */

					return(errno);
				}

				/* Skip the rest of the header text */

				continue;
			}
#endif /* NO_MTA_CC */
			/* Put the header name and text */

			if (fputs(h->hdr_name, fp) == EOF
			    || putc(' ', fp) == EOF) {
				return(errno);
			}
			if (h->hdr_text != NULL && 
			    fputs(h->hdr_text, fp) == EOF) {
				return(errno);
			}
			if (putc('\n', fp) == EOF) {
				return(errno);
			}
		}
	}

	/* Output a blank line following the headers */

	if (putc('\n', fp) == EOF) {
		return(errno);
	}

	return(0);
}
/****************************************************************************/
/*ARGSUSED*/
static char **mailargs(hdrs)
HEADER *hdrs;
{
	/*
	 * Form the shell command line to exec MAILPROG.
	 * If MTA_NEEDS_ARGS is set then we need to put
	 * the list of destinations on the command line,
	 * otherwise we just call MAILPROG.
	 */

	char **args = NULL;

#ifdef MTA_NEEDS_ARGS
	char *users;
#endif /* MTA_NEEDS_ARGS */

#ifdef MTA_IS_SMTP
	/* Initialise the arguments to empty */

	args = (char **) xmalloc(sizeof(char *));
	args[0] = NULL;
#else /* ! MTA_IS_SMTP */
	/* Build the basic argument vector from MAILPROG */

	args = argvec(MTA, NULL);
#endif /* ! MTA_IS_SMTP */

#ifdef MTA_NEEDS_ARGS
	/* Find the headers specifying destination */

	users = set_addrs(hdrs);

	/* Add the destinations to the argument vector */

	args = addr_args(args, users);
	free(users);
#endif /* MTA_NEEDS_ARGS */

	/* Return the argument list for the delivery */

	return(args);
}
/****************************************************************************/
#ifndef MTA_IS_SMTP
/*VARARGS 1*/
static char **argvec(prog, va_alist)
char *prog;
va_dcl
{
	/*
	 * Build an argument vector for prog and the arguments.
	 * The calling sequence is :
	 *	argvec(prog, arg1, arg2, ... , NULL)
	 *
	 * All arguments must be strings.
	 *
	 * NB: This function does not handle quoting or backslash escapes.
	 */

	char *line, **argv = NULL;
	char *arg, *space;
	int argno;
	va_list arglist;

	/* Initialise vararg handling */

	va_start(arglist);
	
	/* Copy prog into line */

	line = xmalloc(strlen(prog) + 1);
	(void) strcpy(line, prog);

	/* Now loop through the arguments, adding them to line */

	while ((arg = va_arg(arglist, char *)) != NULL) {
		line = xrealloc(line, strlen(line) + strlen(arg) + 2);
		(void) strcat(line, " ");
		(void) strcat(line, arg);
	}

	/* Clean up the varargs handling */

	va_end(arglist);

	/* Now build the argument vector */

	arg = line;
	argno = 0;

	while (*arg != '\0') {
		/* Find the next space in the line */

		for (space = arg; *space != '\0'; space++) {
			if (isspace(*space)) {
				*space++ = '\0';

				/* Skip multiple whitespace characters */

				while (isspace(*space)) {
					space++;
				}

				break;
			}
		}

		/* Now (re)allocate the argument vector */

		if (argv == NULL) {
			argv = (char **) xmalloc((argno + 2) *
						 sizeof(char *));
		} else {
			argv = (char **) xrealloc(argv, (argno + 2)
						  * sizeof(char *));
		}

		/* Copy the argument into the vector */

		argv[argno] = xmalloc(strlen(arg) + 1);
		(void) strcpy(argv[argno], arg);

		argno++;
		arg = space;
	}

	/* Add the terminator to the vector */

	argv[argno] = NULL;

	/* Clean up and return the vector */

	free(line);
	return(argv);
}
#endif /* ! MTA_IS_SMTP */
/****************************************************************************/
#ifdef MTA_NEEDS_ARGS
static char *set_addrs(hdrs)
HEADER *hdrs;
{
	/* Form a list of destinations from hdrs and return it */

	char *users = NULL, *new_users;
	int resent = FALSE, new_resent;
	HEADER *h;

	/* Loop through the headers handling each one */

	for (h = hdrs; h != NULL; h = h->next) {
		/* Initialise for this header */

		new_users = NULL;
		new_resent = FALSE;

		/* Is it a Resent-To: or To: header? */

		if (!strcasecmp(h->hdr_name, RESENT_TO)) {
			new_users = h->hdr_text;
			new_resent = TRUE;
		} else if (!resent && !strcasecmp(h->hdr_name, TO)) {
			new_users = h->hdr_text;
			new_resent = FALSE;

#ifdef NO_MTA_CC
		/* The MTA doesn't handle Cc or Bcc, do it ourselves */

		} else if (!strcasecmp(h->hdr_name, RESENT_CC)
			   || !strcasecmp(h->hdr_name, RESENT_BCC)) {
			new_users = h->hdr_text;
			new_resent = TRUE;
		} else if (!resent && (!strcasecmp(h->hdr_name, CC) ||
				       !strcasecmp(h->hdr_name, BCC))) {
			new_users = h->hdr_text;
			new_resent = FALSE;
#endif /* NO_MTA_CC */
		}

		/* If new_users is non-null then add them */

		if (new_users != NULL) {
			/*
			 * Discard the existing user list if the new header
			 * found is a Resent- header and the previous ones
			 * weren't.
			 */

			if (!resent && new_resent) {
				if (users != NULL) {
					free(users);
					users = NULL;
				}
				resent = TRUE;
			}

			/* Now add the address to users */

			if (users == NULL) {
				users = xstrdup(new_users);
			} else {
				users = xrealloc(users, strlen(users) +
						 strlen(new_users) + 2);
				(void) strcat(users, " ");
				(void) strcat(users, new_users);
			}
		}
	}

	return(users);
}
#endif /* MTA_NEEDS_ARGS */
/****************************************************************************/
