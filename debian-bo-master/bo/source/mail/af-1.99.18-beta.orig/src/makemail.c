/* Makemail.c - Routines to form outgoing mail.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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
#include <signal.h>
#include <sys/types.h>
#include "af.h"
#include "makemail.h"
#include "sendmail.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "misc.h"
#include "version.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: makemail.c,v 1.47 1997/03/06 01:30:52 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup(), *vstrcat();
extern char *tempnam(), *strerror(), *a_strerror(), *strdate();
extern char *alias(), *get_line(), *get_estr(), *get_vtext();
extern char *get_home(), *get_addr(), *utos();
extern int link(), unlink(), charlen(), mklower(), get_vval();
extern int strcasecmp(), strncasecmp(), edit_file();
extern int is_header(), is_blank(), cp_hdrs(), count_addresses();
extern unsigned sleep(), save_atimer(), restore_atimer();
extern long ftell(), filesize();
extern void free(), emsg(), emsgl(), typeout();
extern DATEZONE *date_now();

#ifdef NO_MTA_ID
extern char *strid();
#endif /* NO_MTA_ID */

/* Local function declarations */

char *get_hdr();
void free_hdrs();
static char *make_from(), *make_org(), *make_reply_to();
static char *make_in_reply_to(), *make_refs();
static char *make_mailer();
static int sign_mail(), make_edit_file(), check_dest_hdrs();
static void save_unsent_mail();
static HEADER *find_hdr(), *add_hdr(), *set_hdr(), *get_dest();
static HEADER *get_subject(), *autofold(), *read_edit_file();
static HEADER *user_hdrs(), *no_hdrs_found(), *deleted_hdrs();
static HEADER *add_uhdr(), *set_uhdr(), *chk_sender();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the user quit flag from commands.c */

extern int user_quit;

/****************************************************************************/
HEADER *init_hdrs(to, cc, subject, orig_msg, mail_type)
char *to, *cc, *subject;
MESSAGE *orig_msg;
int mail_type;
{
	/* Form the basic headers for a mail message */

	int resent = FALSE;
	HEADER *hdrs = NULL;
	DEF_HDR *def;

#ifndef NO_MTA_RESENT
	/* Should we use resent headers in this message? */

	resent = (mail_type == T_BOUNCE);
#endif /* ! NO_MTA_RESENT */

	/* Create the resent headers if required */

	for (def = def_resent; resent && def->hdr_name != NULL; def++) {
		hdrs = add_hdr(def->hdr_name, def->edit, def->show,
			       def->reqd, def->chk_func, hdrs);
	}

	/* Now create the standard headers */

	for (def = def_headers; def->hdr_name != NULL; def++) {
		hdrs = add_hdr(def->hdr_name, def->edit, def->show,
			       def->reqd, def->chk_func, hdrs);
	}

	/* Set the originator details */

	hdrs = set_hdr((resent) ? RESENT_FROM : FROM, make_from(), hdrs);
	hdrs = set_hdr((resent) ? RESENT_ORG : ORG, make_org(), hdrs);
        hdrs = set_hdr((resent) ? RESENT_REPLY_TO : REPLY_TO,
		       make_reply_to(), hdrs);

	/* Set the subject */

	if (subject != NULL) {
		hdrs = set_hdr(SUBJECT, subject, hdrs);
	}

	/* Set the destinations */

	hdrs = set_hdr((resent) ? RESENT_TO : TO, to, hdrs);
	hdrs = set_hdr((resent) ? RESENT_CC : CC, cc, hdrs);

	/* Default the MIME headers for a message */

	hdrs = set_hdr(MIME_VERSION, DEFVERSION, hdrs);
	hdrs = set_hdr(CONTENT_TYPE, DEFCONTENT, hdrs);
	hdrs = set_hdr(C_T_ENCODING, DEFENCODING, hdrs);

	/* Form an In-Reply-To: header if required */

	if (mail_type == T_REPLY) {
		hdrs = set_hdr(IN_REPLY_TO,
			       make_in_reply_to(orig_msg->from,
						orig_msg->date), hdrs);
	}

	/* Set up the References: header if required */

	if (mail_type == T_FORWARD || mail_type == T_REPLY) {
		hdrs = set_hdr(REFERENCES, make_refs(orig_msg->refs), hdrs);
	}

	/* Set the X-Mailer header */

	hdrs = set_hdr(MAILER, make_mailer(), hdrs);

	/* Return the initialised headers */

	return(hdrs);
}
/****************************************************************************/
char *copy_stdin_mail()
{
	/* Copy the standard input into a file as if edited */

	FILE *fp;
	char *etfile, buf[BUFSIZ];

	/* Get a second temporary file */

	if ((etfile = tempnam(TFILEDIR, TFILEPFX)) == NULL) {
		emsgl("Can't create temporary file: ", strerror(errno), NULL);
		return(NULL);
	}

	/* Open the destination file */

	if ((fp = fopen(etfile, "w")) == NULL) {
		emsgl("Can't open ", etfile, ": ", strerror(errno), NULL);
		return(NULL);
	}

	/* And copy stdin into it up to an EOF */

	while (fgets(buf, BUFSIZ, stdin) != NULL) {
		if (fputs(buf, fp) == EOF) {
			emsgl("Error writing temporary file: ",
			      strerror(errno), NULL);
			(void) fclose(fp);
			(void) unlink(etfile);
			return(NULL);
		}
	}

	/* Close the file and return its name */

	(void) fclose(fp);
	return(etfile);
}
/****************************************************************************/
HEADER *form_stdin_mail(tfile, etfile, hdrs, mail_type)
char *tfile, *etfile;
HEADER *hdrs;
int mail_type;
{
	/* Read headers from the temporary file and copy the body back */

	hdrs = read_edit_file(tfile, etfile, hdrs, mail_type,
			      (get_vval(V_EDIT_IHDRS)) ? V_ACCEPT : V_FALSE);

	/* Clean up and return the headers */

	(void) unlink(etfile);
	free(etfile);

	return(hdrs);
}
/****************************************************************************/
HEADER *form_hdrs(hdrs, mail_type)
HEADER *hdrs;
int mail_type;
{
	/* Form the initial headers for a mail message */

	int resent = FALSE;
	int dest_ok = FALSE;
	ATIMER tbuf;

	/* Don't ask the user questions for silent mail */

	if (mail_type == T_SILENT) {
		return(hdrs);
	}

#ifndef NO_MTA_RESENT
	/* Should we use resent headers in this message? */

	resent = (mail_type == T_BOUNCE);
#endif /* ! NO_MTA_RESENT */

	/* Loop until we have valid destination addresses */

	while (!dest_ok) {
		/* Find out who we're sending the mail to */

		if ((hdrs = get_dest("To: ", (resent) ? RESENT_TO :
				     TO, hdrs, mail_type)) == NULL) {
			return(NULL);
		}

		/* May want to check for users to carbon-copy to */

		if (get_vval(V_ASK_CC) &&
		    (hdrs = get_dest("Cc: ", (resent) ? RESENT_CC :
				     CC, hdrs, mail_type)) == NULL) {
			return(NULL);
		}

		/* And maybe even ask about blind carbon copies */

		if (get_vval(V_ASK_BCC) &&
		    (hdrs = get_dest("Bcc: ", (resent) ? RESENT_BCC :
				     BCC, hdrs, mail_type)) == NULL) {
			return(NULL);
		}

		/* Check the user specified at least one destination */

		if (!(dest_ok = check_dest_hdrs(hdrs, resent))) {
			/* No destinations; report the error */

			emsg("No destination addresses specified");

			/* Handle alarm here since some sleeps won't */

			(void) save_atimer(&tbuf);
			(void) sleep(ECHO_DELAY);
			(void) restore_atimer(&tbuf);
		}
	}

	/* Get the subject if not already specified */

	if ((hdrs = get_subject(hdrs, mail_type)) == NULL) {
		return(NULL);
	}

	/* Canonicalise and fold the the generated headers */

	return(autofold(hdrs));
}
/****************************************************************************/
HEADER *form_mail(tfile, sigfile, hdrs, mail_type, first)
char *tfile, *sigfile;
HEADER *hdrs;
int mail_type, first;
{
	/* Form the message body and headers */

	char *etfile;
	int edit, edit_hdrs;
	int add_sig, edit_sig;
	
	/* Get a second temporary file */

	if ((etfile = tempnam(TFILEDIR, TFILEPFX)) == NULL) {
		emsgl("Can't create temporary file: ", strerror(errno), NULL);
		free_hdrs(hdrs);
		return(NULL);
	}

	/* Are we going to edit the file? */

	edit = (!first || mail_type != T_BOUNCE && mail_type != T_SILENT);

	/* Do we want to output headers for editing or read them in? */

	edit_hdrs = (!first) ? V_TRUE : (edit) ?
		get_vval(V_EDIT_IHDRS) : V_FALSE;

	/* Do we want to add a signature, or edit it? */

	add_sig = (sigfile != NULL && mail_type != T_BOUNCE);
	edit_sig = (add_sig && get_vval(V_EDIT_ISIG));

	/* Copy the message into the file for editing */

	if (!make_edit_file(etfile, tfile, (edit_sig) ? sigfile : NULL,
			    (edit_hdrs == V_TRUE) ? hdrs : NULL)) {
		(void) unlink(etfile);
		free(etfile);
		free_hdrs(hdrs);
		return(NULL);
	}

	/* Now execute the editor */

	if (edit && !edit_file(etfile)) {
		(void) unlink(etfile);
		free(etfile);
		free_hdrs(hdrs);
		return(NULL);
	}

	/* Add any signature if it wasn't to be edited */

	if (add_sig && !edit_sig && !sign_mail(etfile, sigfile)) {
		(void) unlink(etfile);
		free(etfile);
		free_hdrs(hdrs);
		return(NULL);
	}

	/* Read headers from the edit file and copy the body back */

	hdrs = read_edit_file(tfile, etfile, hdrs, mail_type, edit_hdrs);

	/* Clean up and return the headers */

	(void) unlink(etfile);
	free(etfile);

	return(hdrs);
}
/****************************************************************************/
HEADER *posting_hdrs(hdrs)
HEADER *hdrs;
{
	/* Set any headers required just before mail is sent */

#ifdef NO_MTA_DATE
	int resent = FALSE;
	DATEZONE *date;

	/* Check if the message is Resent- */

	resent = (get_hdr(RESENT_FROM, hdrs) != NULL ||
		  get_hdr(RESENT_SENDER, hdrs) != NULL);

	/* Get the current date */

	date = date_now();

#else /* ! NO_MTA_DATE */
#ifdef NO_MTA_ID

	int resent = FALSE;
	DATEZONE *date;

	/* Check if the message is Resent- */

	resent = (get_hdr(RESENT_FROM, hdrs) != NULL ||
		  get_hdr(RESENT_SENDER, hdrs) != NULL);

	/* Get the current date */

	date = date_now();
#endif /* NO_MTA_ID */
#endif /* NO_MTA_DATE */

#ifdef NO_MTA_ID
	/* Set the Id of the message */

	hdrs = set_hdr((resent) ? RESENT_ID : MESSAGE_ID,
		       strid(date), hdrs);
#endif /* NO_MTA_ID */

#ifdef NO_MTA_DATE
	/* Set the date of the message */

	hdrs = set_hdr((resent) ? RESENT_DATE : DATE,
		       strdate(date, TRUE), hdrs);
#endif /* NO_MTA_DATE */

	/* Return the headers to be sent */

	return(hdrs);
}
/****************************************************************************/
/*ARGSUSED*/
HEADER *saving_hdrs(hdrs, mailfile)
HEADER *hdrs;
char *mailfile;
{
	/* Set any headers required just before mail is saved */

#ifndef NO_MTA_DATE
	int resent = FALSE;
	DATEZONE *date;

	/* Check if the message is Resent- */

	resent = (get_hdr(RESENT_FROM, hdrs) != NULL ||
		  get_hdr(RESENT_SENDER, hdrs) != NULL);

	/* Get the current date */

	date = date_now();

	/* Set the date of the message */

	hdrs = set_hdr((resent) ? RESENT_DATE : DATE,
		       strdate(date, TRUE), hdrs);
#endif /* ! NO_MTA_DATE */

#ifdef MTA_CONTENT_LENGTH
	/* Set the Content-length to the required value */

	hdrs = set_hdr(CONTENT_LENGTH, utos(filesize(mailfile)), hdrs);
#endif /* MTA_CONTENT_LENGTH */

	/* Return the headers to be sent */

	return(hdrs);
}
/****************************************************************************/
char *get_hdr(hdrname, hdrs)
char *hdrname;
HEADER *hdrs;
{
	/* Return the text associated with the named header */

	HEADER *h;

	/* Find the appropriate header */

	for (h = hdrs; h != NULL; h = h->next) {
		/* Is this the header we want? */

		if (!strcasecmp(h->hdr_name, hdrname)) {
			return(h->hdr_text);
		}
	}

	/* No such header */

	return(NULL);
}
/****************************************************************************/
void free_hdrs(hdrs)
HEADER *hdrs;
{
	/* Free up the space used up in the list of headers */

	if (hdrs != NULL) {
		/* Free the next header, then this one */

		free_hdrs(hdrs->next);
		free(hdrs);
	}

	return;
}
/****************************************************************************/
static HEADER *find_hdr(name, hdrs)
char *name;
HEADER *hdrs;
{
	/* Find a header from the list by name */

	HEADER *h;

	/* Search the list for the header */

	for (h = hdrs; h != NULL; h = h->next) {
		/* Is this the header we're looking for? */

		if (!strcasecmp(h->hdr_name, name)) {
			return(h);
		}
	}

	/* The header wasn't found in the list */

	return(NULL);
}
/****************************************************************************/
static HEADER *add_hdr(name, edit, show, reqd, chk_func, hdrs)
char *name;
unsigned edit, show, reqd;
char *(*chk_func)();
HEADER *hdrs;
{
	/*
	 * Add a header of the specified name to the list, setting
	 * its edit flag and checking function appropiately.
	 */

	HEADER *node, *h;

	/* Generate the node */

	node = (HEADER *) xmalloc(sizeof(HEADER));

	/* Set the header's name and parameters */

	node->hdr_name = xstrdup(name);
	node->edit = edit;
	node->show = show;
	node->reqd = reqd;
	node->found = TRUE;
	node->chk_func = chk_func;

	/* Initialise text and next to NULL */ 

	node->hdr_text = NULL;
	node->next = NULL;

	/* Find the end of the list and append the node */

	if (hdrs == NULL) {
		hdrs = node;
	} else {
		for (h = hdrs; h->next != NULL; h = h->next) {
			/* NULL LOOP */
		}
		h->next = node;
	}

	/* Return the list of headers */

	return(hdrs);
}
/****************************************************************************/
static HEADER *set_hdr(name, value, hdrs)
char *name, *value;
HEADER *hdrs;
{
	/* Set the value of a header's text to value */

	HEADER *h;

	/* Does this header already exist? */

	if ((h = find_hdr(name, hdrs)) != NULL) {
		/* Free any previous value */

		if (h->hdr_text != NULL) {
			free(h->hdr_text);
			h->hdr_text = NULL;
		}

		/* Set the text to the new value */

		if (value != NULL && strlen(value) > 0) {
			h->hdr_text = xstrdup(value);
		}
		return(hdrs);
	}

	/* If we reach here then there's no such header */

	return(hdrs);
}
/****************************************************************************/
static HEADER *get_dest(prompt, hdrname, hdrs, mail_type)
char *prompt, *hdrname;
HEADER *hdrs;
int mail_type;
{
	/* Get a list of destinations for the message */

	char *deflt, *addrs;
	ATIMER tbuf;

	/* Get the default destinations from the headers */

	deflt = get_hdr(hdrname, hdrs);

	/* Do we need to ask for the destinations? */

	if ((mail_type != T_REPLY || !get_vval(V_EDIT_REPLY))
	    && deflt != NULL) {
		return(hdrs);
	}

	/* Get and alias the destination(s) */

	while ((addrs = get_estr(NULL, prompt, deflt)) != NULL
	       && (addrs = alias(addrs)) == NULL) {
		/* Error in destination; print a message for a while */

		emsg(a_strerror());

		/* Handle alarm here since some sleeps won't */

		(void) save_atimer(&tbuf);
		(void) sleep(ECHO_DELAY);
		(void) restore_atimer(&tbuf);
	}

	/* Check for a user quit */

	if (addrs == NULL && user_quit) {
		free_hdrs(hdrs);
		return(NULL);
	}

	/* Update and return the headers */

	return(set_hdr(hdrname, addrs, hdrs));
}
/****************************************************************************/
static HEADER *get_subject(hdrs, mail_type)
HEADER *hdrs;
int mail_type;
{
	/* Get the subject for a message */

	char *deflt, *subject;

	/* Get the default subject from the headers */

	deflt = get_hdr(SUBJECT, hdrs);

	/* Use default for silent, bounce or send if set */

	if (mail_type == T_SILENT || mail_type == T_BOUNCE
	    || mail_type == T_SEND && deflt != NULL ) {
		return(hdrs);
	}

	/* Set up the default for the subject */

	if (mail_type == T_REPLY &&
	    strncasecmp(deflt, REPLY_PFX, strlen(REPLY_PFX))) {
		/* Prepend the reply prefix to the subject */

		deflt = vstrcat(REPLY_PFX, deflt, NULL);
	} else {
		/* Just copy the default subject */

		deflt = (deflt != NULL) ? xstrdup(deflt) : NULL;
	}

	/* Get the subject and set the header */

	if ((subject = get_estr(NULL, "Subject: ", deflt)) == NULL
	    && user_quit) {
		free_hdrs(hdrs);
		return(NULL);
	}

	/* Free space and update headers */

	if (deflt != NULL) {
		free(deflt);
	}

	return(set_hdr(SUBJECT, subject, hdrs));
}
/****************************************************************************/
static char *make_from()
{
	/* Return a static buffer containing From: details */

	static char *frombuf = NULL;
	char *rname;

	/* Free any previous return buffer */

	if (frombuf != NULL) {
		free(frombuf);
	}

	/* Get the user's real name */

	rname = get_vtext(V_REALNAME);

	/* Now generate an appropriate from address */

	frombuf = (rname == NULL) ? xstrdup(get_addr()) :
		vstrcat(rname, " <", get_addr(), ">", NULL);

	/* And return the buffer */

	return(frombuf);
}
/****************************************************************************/
static char *make_org()
{
	/* Return the Organization: details */

	return(get_vtext(V_ORG));
}
/****************************************************************************/
static char *make_reply_to()
{
	/* Return the Reply-To: details */

	return(get_vtext(V_REPLY));
}
/****************************************************************************/
static char *make_in_reply_to(from, date)
char *from;
DATEZONE *date;
{
	/* Return the In-Reply-To: details in a static buffer */

	static char *irstr = NULL;
	char *dstr;

	/* Free any previous return value */

	if (irstr != NULL) {
		free(irstr);
	}

	/* Get the date details */

	dstr = strdate(date, FALSE);

	/* Form and return the details */

	irstr = vstrcat(from, "'", (mklower(from[strlen(from) - 1]) != 's')
			? "s" : "", INREPLY_TEXT, dstr, NULL);
	return(irstr);
}
/****************************************************************************/
static char *make_refs(refs)
char **refs;
{
	/* Return the References: details in a static buffer */

	static char *rstr = NULL;
	int ref;

	/* Free any previous return value */

	if (rstr != NULL) {
		free(rstr);
	}

	/* Initialise the text to NULL */

	rstr = NULL;

	/* Copy any available references into the text */

	for (ref = 0; refs != NULL && ref < NO_REFERENCES; ref++) {
		/* Is this reference available? */

		if (refs[ref] == NULL) {
			continue;
		}

		/* Append the reference */

		if (rstr == NULL) {
			rstr = xstrdup(refs[ref]);
		} else {
			rstr = xrealloc(rstr, strlen(rstr) +
					strlen(refs[ref]) + 2);
			(void) strcat(rstr, " ");
			(void) strcat(rstr, refs[ref]);
		}
	}

	/* Return the details */

	return(rstr);
}
/****************************************************************************/
static char *make_mailer()
{
	/* Return the X-Mailer: definition in a static buffer */

	static char *mstr = NULL;

	/* Don't bother if the return text has already been set */

	if (mstr == NULL) {
		/* Copy the program name and version into the text */

		mstr = vstrcat(PROGNAME, " v", VERSION, NULL);
	}
	return(mstr);
}
/****************************************************************************/
static int make_edit_file(etfile, tfile, sigfile, hdrs)
char *etfile, *tfile, *sigfile;
HEADER *hdrs;
{
	/* Make a version of tfile ready for editing */

	char buf[BUFSIZ];
	int status;
	FILE *efp, *fp;

	/* Open the edit file */

	if ((efp = fopen(etfile, "w")) == NULL) {
		emsgl("Can't open ", etfile, ": ", strerror(errno), NULL);
		return(FALSE);
	}

	/* Copy the headers into the file if required */

	if (hdrs != NULL && (status = cp_hdrs(efp, hdrs, FALSE))) {
		emsgl("Error writing ", etfile, ": ", strerror(status), NULL);
		(void) fclose(efp);
		return(FALSE);
	}

	/* Open the original temporary file */

	if ((fp = fopen(tfile, "r")) == NULL) {
		emsgl("Can't open ", tfile, ": ", strerror(errno), NULL);
		(void) fclose(efp);
		return(FALSE);
	}

	/* Now copy the body */

	while (fgets(buf, BUFSIZ, fp) != NULL) {
		if (fputs(buf, efp) == EOF) {
			emsgl("Error writing ", etfile, ": ",
			      strerror(errno), NULL);
			(void) fclose(fp);
			(void) fclose(efp);
			return(FALSE);
		}
	}

	/* Close the files */

	(void) fclose(fp);
	(void) fclose(efp);
		
	/* Add any signature if it is to be edited */

	if (sigfile != NULL && !sign_mail(etfile, sigfile)) {
		return(FALSE);
	}

	/* All went ok */

	return(TRUE);
}
/****************************************************************************/
static HEADER *read_edit_file(tfile, etfile, hdrs, mail_type, edited)
char *tfile, *etfile;
HEADER *hdrs;
int mail_type, edited;
{
	/*
	 * Copy the edit file back into the temporary file,
	 * translating any headers if required.
	 */

	char buf[BUFSIZ];
	FILE *efp, *fp;

	/* Open the edit file */

	if ((efp = fopen(etfile, "r")) == NULL) {
		emsgl("Can't open ", etfile, ": ", strerror(errno), NULL);
		free_hdrs(hdrs);
		return(NULL);
	}

	/* Calculate the new headers, if applicable */

	if ((hdrs = user_hdrs(efp, hdrs, mail_type, edited)) == NULL) {
		/* This condition only occurs for silent mail */

		(void) fclose(efp);
		emsg("Bad headers in outgoing mail");
		save_unsent_mail(etfile);
		return(NULL);
	}

	/* Open the temporary file */

	if ((fp = fopen(tfile, "w")) == NULL) {
		emsgl("Can't open ", tfile, ": ", strerror(errno), NULL);
		(void) fclose(efp);
		free_hdrs(hdrs);
		return(NULL);
	}

	/* Copy the body back to the temporary file */

	while (fgets(buf, BUFSIZ, efp) != NULL) {
		if (fputs(buf, fp) == EOF) {
			emsgl("Error writing ", tfile, ": ",
			      strerror(errno), NULL);
			(void) fclose(fp);
			(void) fclose(efp);
			free_hdrs(hdrs);
			return(NULL);
		}
	}

	/* Close the files and return the headers */

	(void) fclose(fp);
	(void) fclose(efp);

	return(hdrs);
}
/****************************************************************************/
static HEADER *user_hdrs(fp, hdrs, mail_type, edited)
FILE *fp;
HEADER *hdrs;
int mail_type, edited;
{
	/*
	 * Return a list of headers as modified by those given in the
	 * file to which fp is open.  Leaves the file pointer fp
	 * pointing at the start of the message body.
	 */

	char *buf, *orig_dest;
	char *orig_dest_hdr;
	int resent;
	long filepos;

	/* Do we want to translate headers in the edit file? */

	if (!edited && mail_type != T_BOUNCE) {
		return(hdrs);
	}

	/* Save the current position of fp so we can restore it */

	filepos = ftell(fp);

	/* Check if the message is Resent- */

	resent = (get_hdr(RESENT_FROM, hdrs) != NULL ||
		  get_hdr(RESENT_SENDER, hdrs) != NULL);

	/* Save the current destination headers */

	orig_dest_hdr = xstrdup((resent) ? RESENT_TO : TO);
	if ((orig_dest = get_hdr(orig_dest_hdr, hdrs)) == NULL
	    || !count_addresses(orig_dest)) {
		/* No To: addresses, try Cc: instead */

		free(orig_dest_hdr);
		orig_dest_hdr = xstrdup((resent) ? RESENT_CC : CC);
		if ((orig_dest = get_hdr(orig_dest_hdr, hdrs)) == NULL
		    || !count_addresses(orig_dest)) {
			/* No Cc: addresses, try Bcc: instead */

			free(orig_dest_hdr);
			orig_dest_hdr = xstrdup((resent) ? RESENT_BCC : BCC);
			orig_dest = get_hdr(orig_dest_hdr, hdrs);
		}
	}

	/* Copy the original destination */

	orig_dest = (orig_dest != NULL) ? xstrdup(orig_dest) : NULL;

	/* We haven't found any of the headers yet */

	hdrs = no_hdrs_found(hdrs);

	/* Loop until all the headers have been processed */

	while (hdrs != NULL && (buf = get_line(fp, TRUE)) != NULL) {
		/* Check the line is a header */

		if (!is_header(buf)) {
			/* Skip blank lines, preserve others */

			if (!is_blank(buf)) {
				(void) fseek(fp, filepos, 0);
			}
			free(buf);
			break;
		}

		/* Add the header with checking and free the buffer */

		hdrs = add_uhdr(buf, hdrs, mail_type, edited);
		free(buf);

		/* Reset the file seek position */

		filepos = ftell(fp);
	}

	/* Unset any headers that the user deleted */

	hdrs = deleted_hdrs(hdrs, mail_type, edited);

	/* Now check if we have destinations specified */

	if (orig_dest != NULL && !check_dest_hdrs(hdrs, resent)) {
		/* Let the user know we have a problem */

		typeout("No destinations specified: restoring original ");
		typeout(orig_dest_hdr);
		typeout(" header\n");

		/* Restore old destination headers */

		set_hdr(orig_dest_hdr, orig_dest, hdrs);
	}

	/* Free the original destination */

	if (orig_dest != NULL) {
		free(orig_dest);
	}
	free(orig_dest_hdr);

	/* End any typeout of errors generated by add_uhdr */

	typeout(NULL);

	/* Canonicalise and fold the the headers */

	return(autofold(hdrs));
}
/****************************************************************************/
static HEADER *add_uhdr(hline, hdrs, mail_type, edited)
char *hline;
HEADER *hdrs;
int mail_type, edited;
{
	/* Set the header given by text into hdrs with checking */

	char *name, *text;

	/* If bouncing, ignore Resent- headers on first pass */

	if (!edited && mail_type == T_BOUNCE &&
	    !strncasecmp(hline, RESENT, strlen(RESENT))) {
		return(hdrs);
	}

	/* Make the header name */
	
	text = strchr(hline, ':') + 1;
	name = xmalloc(text - hline + 1);
	(void) strncpy(name, hline, text - hline);
	name[text - hline] = '\0';

	/* Remove white space before the header text */

	while (isspace(*text)) {
		text++;
	}

	/* And the newline after the text */

	if (*text != '\0') {
		text[strlen(text) - 1] = '\0';
	}

	/* Get the text into an allocated string */

	text = (*text != '\0') ? xstrdup(text) : NULL;

	/* Add the header */

	hdrs = set_uhdr(name, text, hdrs, mail_type, edited);

	/* Clean up and return */

	free(name);
	if (text != NULL) {
		free(text);
	}
	return(hdrs);
}
/****************************************************************************/
static HEADER *no_hdrs_found(hdrs)
HEADER *hdrs;
{
	/* Mark all the header in the list as not found */

	HEADER *h;

	/* Loop over the headers setting found */

	for (h = hdrs; h != NULL; h = h->next) {
		h->found = FALSE;
	}

	/* Return the updated headers */

	return(hdrs);
}
/****************************************************************************/
static HEADER *deleted_hdrs(hdrs, mail_type, edited)
HEADER *hdrs;
int mail_type, edited;
{
	/* Unset any headers that the user deleted */

	HEADER *h;

	/* Do we need to check for deleted headers? */

	if (edited != V_TRUE)  {
		/* The user didn't edit all the headers */

		return(hdrs);
	}
	
	/* Loop over the headers checking if we found them */

	for (h = hdrs; h != NULL; h = h->next) {
		/* Is this header expected but missing? */

		if (h->edit && h->show && !(h->found)) {
			/* Check and delete the header */

			if (h->reqd && mail_type == T_SILENT) {
				/* Fatal error; fail the mail */

				free_hdrs(hdrs);
				return(NULL);
			} else if (h->reqd) {
				/* We can't delete this header */

				typeout("Can't delete ");
				typeout(h->hdr_name);
				typeout(" header\n");
			} else if (h->hdr_text != NULL) {
				/* Update the header in the list */

				hdrs = set_hdr(h->hdr_name, NULL, hdrs);
			}
		}
	}

	/* Return the updated headers */

	return(hdrs);
}
/****************************************************************************/
static HEADER *set_uhdr(name, text, hdrs, mail_type, edited)
char *name, *text;
HEADER *hdrs;
int mail_type, edited;
{
	/* Set a header value from user-supplied data */

	char *ctext = NULL;
	HEADER *h;

	/* Does the header already exist? */

	if ((h = find_hdr(name, hdrs)) != NULL) {
		/* Note that we've found the header */

		h->found = TRUE;

		/* Check if the value has changed */

		if (h->hdr_text == NULL && text == NULL
		    || h->hdr_text != NULL && text != NULL
		    && !strcmp(h->hdr_text, text)) {
			return(hdrs);
		}
			
		/* Check we can modify the header */

		if (!h->edit && mail_type == T_SILENT) {
			/* Fatal error; fail the mail */

			free_hdrs(hdrs);
			return(NULL);
		} else if (!h->edit && !edited && mail_type == T_BOUNCE) {
			/* Ignore the error this time */

			return(hdrs);
		} else if (!h->edit) {
			/* Tell the user about the error */

			typeout("Can't modify ");
			typeout(h->hdr_name);
			typeout(" header\n");
			return(hdrs);
		}

		/* Check the header if required */

		if (text != NULL && h->chk_func != NULL) {
			/* Run the check function */

			if ((ctext = h->chk_func(text)) == NULL) {
				/* Error in header text */

				if (mail_type == T_SILENT) {
					free_hdrs(hdrs);
					return(NULL);
				}
				typeout(a_strerror());
				typeout(" in ");
				typeout(name);
				typeout(" header\n");
				return(hdrs);
			}

			/* Update the header text */

			text = ctext;
		} else if (h->reqd && text == NULL &&
				mail_type == T_SILENT) {
			/* Fatal error; fail the mail */

			free_hdrs(hdrs);
			return(NULL);
		} else if (h->reqd && text == NULL) {
			/* We can't unset this header */

			typeout("Can't delete ");
			typeout(h->hdr_name);
			typeout(" header\n");
			return(hdrs);
		}

		/* Update the header in the list */

		hdrs = set_hdr(name, text, hdrs);

		/* Free the checked text if required */

		if (ctext != NULL) {
			free(ctext);
		}

		/* Check if we need to add a Sender: header */

		hdrs = chk_sender(name, text, hdrs, mail_type);

		/* Return the modified headers */

		return(hdrs);
	}

	/* We are adding a new header to the list */

	hdrs = add_hdr(name, TRUE, TRUE, FALSE, NULL, hdrs);
	hdrs = set_hdr(name, text, hdrs);

	return(hdrs);
}
/****************************************************************************/
static HEADER *chk_sender(name, text, hdrs, mail_type)
char *name, *text;
HEADER *hdrs;
int mail_type;
{
	/*
	 * Node has been modified, check if we need to add a Sender:
	 * or Resent-Sender: header as a result of this.
	 */

	/* Check if it was a (Resent-) From: header */

	if (mail_type == T_BOUNCE && strcasecmp(name, RESENT_FROM)) {
		return(hdrs);
	}
	if (mail_type != T_BOUNCE && strcasecmp(name, FROM)) {
		return(hdrs);
	}

	/* Check the user didn't try to delete the header */

	if (text == NULL && mail_type == T_SILENT) {
		free_hdrs(hdrs);
		return(NULL);
	} else if (text == NULL) {
		/* Report the error */

		typeout("Can't delete ");
		typeout(name);
		typeout("header.\n");

		/* Repair the damage */

		return(set_hdr(name, make_from(), hdrs));
	}

	/* Add a (Resent-)Sender: header */

	if (mail_type == T_BOUNCE) {
		hdrs = set_hdr(RESENT_SENDER, make_from(), hdrs);
	} else {
		hdrs = set_hdr(SENDER, make_from(), hdrs);
	}

	return(hdrs);
}
/****************************************************************************/
static int check_dest_hdrs(hdrs, resent)
HEADER *hdrs;
int resent;
{
	/* Check that there is at least one destination address */

	char *addrs;

	/* Simply check the three destination headers for addresses */

	return((addrs = get_hdr((resent) ? RESENT_TO : TO, hdrs)) != NULL
	       && count_addresses(addrs) ||
	       (addrs = get_hdr((resent) ? RESENT_CC : CC, hdrs)) != NULL
	       && count_addresses(addrs) ||
	       (addrs = get_hdr((resent) ? RESENT_BCC : BCC, hdrs)) != NULL
	       && count_addresses(addrs));
}
/****************************************************************************/
static int sign_mail(tfile, sigfile)
char *tfile, *sigfile;
{
	/* Append the user's signature file to the mail */

	FILE *sfp, *fp;
	char buf[BUFSIZ];
	char *separator;
	int line = 0;

	/* Open the signature file - failure isn't an error */

	if (sigfile == NULL || (sfp = fopen(sigfile, "r")) == NULL) {
		return(TRUE);
	}

	/* Open the temporary file */

	if ((fp = fopen(tfile, "a+")) == NULL) {
		(void) fclose(sfp);
		emsgl("Can't open ", tfile, ": ", strerror(errno), NULL);
		return(FALSE);
	}

	/*
	 * Check the last character in the file is a newline,
	 * since some editors don't force a newline at the end
	 * of the file, which makes signatures look strange.
	 */
	
	if (!fseek(fp, -1L, 2) && getc(fp) != '\n') {
		(void) fseek(fp, 0L, 2);
		if (fputc('\n', fp) == EOF) {
			emsgl("Error writing ", tfile, ": ",
			      strerror(errno), NULL);
			(void) fclose(sfp);
			(void) fclose(fp);
			return(FALSE);
		}
	}

	/* Move back to the end of the file */

	(void) fseek(fp, 0L, 2);

	/* Append the user's signature separation string */

	if ((separator = get_vtext(V_SEPARATOR)) != NULL
	    && (fputs(separator, fp) == EOF
		|| (fputc('\n', fp) == EOF))) {
		/* Failed to write the separator */

		emsgl("Error writing ", tfile, ": ",
		      strerror(errno), NULL);
		(void) fclose(sfp);
		(void) fclose(fp);
		return(FALSE);
	}

	/* Copy the signature file, handling limits if set */

	while (fgets(buf, BUFSIZ, sfp) != NULL) {

#ifdef SIGCOLS
#if SIGCOLS > 0 && SIGCOLS < BUFSIZ
		buf[SIGCOLS - 1] = '\n';
		buf[SIGCOLS] = '\0';
#endif /* SIGCOLS > 0 */
#endif /* SIGCOLS */

		if (fputs(buf, fp) == EOF) {
			emsgl("Error writing ", tfile, ": ",
			      strerror(errno), NULL);
			(void) fclose(sfp);
			(void) fclose(fp);
			return(FALSE);
		}

#ifdef SIGLINES
#if SIGLINES > 0
		if (++line >= SIGLINES) {
			break;
		}
#endif /* SIGLINES > 0 */
#endif /* SIGLINES */
	}

	(void) fclose(sfp);
	(void) fclose(fp);

	return(TRUE);
}
/****************************************************************************/
static void save_unsent_mail(tfile)
char *tfile;
{
	/* Append tfile to DEADFILE after an error sending quiet mail */

	char buf[BUFSIZ];
	char *dfile;
	int error = FALSE;
	FILE *dfp, *fp;

	/* Form the dead file name */

	dfile = vstrcat(get_home(NULL), "/", DEADFILE, NULL);

	/* Try and save the mail */

	if ((dfp = fopen(dfile, "a")) == NULL) {
		error = TRUE;
	} else if ((fp = fopen(tfile, "r")) == NULL) {
		(void) fclose(dfp);
		error = TRUE;
	} else {
		while (fgets(buf, BUFSIZ, fp) != NULL) {
			if (fputs(buf, dfp) == EOF) {
				emsgl("Error writing ", dfile, ": ",
				      strerror(errno), NULL);
				error = TRUE;
				break;
			}

			(void) fclose(fp);
			(void) fclose(dfp);
		}
	}

	/* Report the error to the user */

	if (!error) {
		emsgl("(Mail stored in ", DEADFILE, ")", NULL);
	} else {
		emsgl("(Can't open ", DEADFILE, " to store mail)", NULL);
	}

	return;
}
/****************************************************************************/
static HEADER *autofold(hdrs)
HEADER *hdrs;
{
	/*
	 * For each header in hdrs, canonicalise any newlines in
	 * the text to folds (to stop it all going horribly wrong),
	 * and, if required, fold the header to a suitable width.
	 */

	char *new_text, *break1, *break2, *p1, *p2;
	int add_folds, comma_breaks, prev_was_ok, width;
	HEADER *h;

	/* Check if we should add folds as required */

	add_folds = (get_vval(V_AUTOFOLD));

	/* Loop through each header in the list */

	for (h = hdrs; h != NULL; h = h->next) {
		/* Skip headers with no text defined */

		if (h->hdr_text == NULL) {
			continue;
		}

		/* Do we break addresses after commas? */

		comma_breaks = (h->chk_func == alias);

		/* Can't more than double the size of the text */

		new_text = xmalloc(strlen(h->hdr_text) * 2 + 1);

		/* The intitial width includes the name */

		width = strlen(h->hdr_name) + 1;

		/* Haven't found any breaks yet */

		break1 = break2 = NULL;

		/* Can't break at the start of an address list */

		prev_was_ok = !comma_breaks;

		/* Copy the text, folding as required */

		p1 = h->hdr_text;
		p2 = new_text;

		while (*p1 != '\0') {
			/* Check for newline */

			if (IS_NEWLINE(*p1)) {
				/* Check for newline at end of line */

				if (*(p1 + 1) == '\0') {
					break;
				}

				/* Ignore folds if auto-folding */

				if (add_folds) {
					/* Replace the fold with a space */

					while (IS_LWSP(*(p1 + 1))) {
						p1++;
					}
					*p1 = ' ';
				} else {
					/* Copy the newline */

					*p2++ = '\n';

					/* Check for a bad fold */

					if (IS_LWSP(*(p1 + 1))) {
						p1++;
					} else {
						*p1 = '\t';
					}

					/* Set the width checking details */

					width = 0;
					break1 = break2 = NULL;
					prev_was_ok = (!comma_breaks);
				}
			}

			/* Check for a break */

			if (IS_LWSP(*p1) && prev_was_ok) {
				break1 = p1;
				break2 = p2;
			}

			/* Set the width after this character */

			width = width + charlen(*p1, width, FALSE);

			/* See if we need to add a fold */

			if (add_folds && width > FOLD_WIDTH
			    && break1 != NULL) {
				/* Jump back to last break and recopy */

				p1 = break1;
				p2 = break2;
				break1 = break2 = NULL;

				/* Skip any white space after the fold */

				while (IS_LWSP(*(p1 + 1))) {
					p1++;
				}

				/* Insert a newline and set up a tab */

				*p2++ = '\n';
				*p1 = '\t';

				/* Reset the width */

				width = charlen('\t', 0, FALSE);
			}

			/* Check if this character allows next to break */

			prev_was_ok = (!comma_breaks || *p1 == ',');

			/* Copy the character */

			*p2++ = *p1++;
		}

		/* Terminate and resize the new text */

		*p2 = '\0';
		new_text = xrealloc(new_text, strlen(new_text) + 1);

		/* Replace the original text with the new */

		free(h->hdr_text);
		h->hdr_text = new_text;
	}

	/* Return the modified headers */

	return(hdrs);
}
/****************************************************************************/
