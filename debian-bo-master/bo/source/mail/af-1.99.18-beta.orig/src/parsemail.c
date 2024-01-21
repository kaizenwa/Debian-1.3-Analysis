/* Parsemail.c - Mailfile parser for af.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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
#include <varargs.h>
#include "af.h"
#include "atom.h"
#include "parsemail.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "tags.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: parsemail.c,v 1.24 1997/03/31 18:32:19 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* External function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup(), *vstrcat();
extern char *strerror(), *strcanon(), *utos(), *canonical();
extern char *mail_addresses(), *addrnames(), *refextract();
extern char *get_line(), *get_digest(), *atext();
extern char *c_contype(), *c_encoding();
extern int atol(), strncasecmp(), get_vval();
extern int close_folder(), empty_folder();
extern int textual(), set_tags(), mmdf_form();
extern int is_header(), is_fromline(), is_blank();
extern unsigned count_messages();
extern void free(), afree(), free_messages();
extern void msgl(), emsgl(), set_sys_tags();
extern void mask_tags(), free_tlist();
extern ATOM *tokenise(), *find_token();
extern ATOM *asearch(), *acut();
extern DATEZONE *parse_date();
extern FILE *open_folder();
extern TAG_LIST *taglist();

#ifdef READ_VIA_POP3
extern char *read_pop3();
extern int close_pop3();
extern void netid_pop3();
extern FILE *open_pop3();
#endif /* READ_VIA_POP3 */

/* Local function declarations */

MESSAGE *null_msg();
static void parse_header(), parse_refs(), add_ref();
static MESSAGE *parse_mail(), *new_msg(), *add_mail();
static MESSAGE *add_msg(), *msg_error(), *trim_text();
static MSG_TEXT *add_text();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* The error status of the last folder read */

static int last_read_failed = FALSE;

/****************************************************************************/
/* A static to tell us if we've seen a Reply-To header yet */

static int reply_found = FALSE;

/* Another to say if we've seen a Message-ID yet */

static int id_found = FALSE;

/* Another to say if we've seen a Mime-Version yet */

static int version_found = FALSE;

/* And another to indicate only MIME errors were found */

static int mime_errors_only = FALSE;

/****************************************************************************/
MESSAGE *get_messages(folder, old_list, offset)
char *folder;
MESSAGE *old_list;
long offset;
{
	/*
	 * This function extracts the header information from a
	 * file, or from an internal digest if folder is NULL.
	 * The information is used to form a doubly-linked list
	 * of messages.
	 *
	 * If old_list is non-null, then add the entries to it
	 * rather than overwriting.  If offset is non-zero then
	 * skip that many bytes before reading.
	 *
	 */

	int mmdf;
	FILE *fp;
	MESSAGE *new_list = NULL;

	/* No errors so far */

	last_read_failed = FALSE;

	/* If required read the contents of the file or buffer */

	if (folder == NULL) {
		/* Parse the contents of the digest */

		new_list = parse_mail(NULL, NULL, old_list,
				      get_digest, FALSE);
#ifdef READ_VIA_POP3
	} else if (POP3_MBOX(folder)) {
		/* Open a connection to the POP3 folder */

		if ((fp = open_pop3(folder, offset, TRUE)) == NULL) {
			last_read_failed = TRUE;
			return(NULL);
		}

		/* Parse the contents of the folder */

		new_list = parse_mail(NULL, folder, old_list,
				      read_pop3, FALSE);

		/* Close the folder and check status */

		if (close_pop3(FALSE)) {
			last_read_failed = TRUE;
			free_messages(new_list);
			return(NULL);
		}

		/* And set the UIDL IDs for each message */

		netid_pop3(new_list);
#endif /* READ_VIA_POP3 */
	} else if (!empty_folder(folder)) {
		/* Check if the file is in MMDF format */

		mmdf = mmdf_form(folder);

		/* Open the folder */

		if ((fp = open_folder(folder, FALSE, offset)) == NULL) {
			last_read_failed = TRUE;
			return(NULL);
		}

		/* Parse the contents of the folder */

		new_list = parse_mail(fp, folder, old_list, get_line, mmdf);

		/* Close the folder and check status */

		if (close_folder(folder, fp)) {
			last_read_failed = TRUE;
			free_messages(new_list);
			return(NULL);
		}
	}

	/* Add the terminating message if not updating */

	if (old_list == NULL) {
		new_list = null_msg(new_list);
	}

	/* And return the modified list */

	return(new_list);
}
/****************************************************************************/
MESSAGE *get_one_message(message, file, old_hdrs)
MESSAGE *message;
char *file;
int old_hdrs;
{
	/*
	 * Read a single message from a file, updating message
	 * with the headers (if hdrs is true) and body of the
	 * message in the file.  Returns the modified message
	 * on success, NULL if an error occurs.
	 */

	char *delim, *line, *colon;
	int fromlines = TRUE;
	int headers = TRUE;
	MESSAGE *node = NULL;
	MSG_TEXT *t;
	FILE *fp;

#ifdef MTA_CONTENT_LENGTH
	unsigned length = 0;
#else /* ! MTA_CONTENT_LENGTH */
	char *new_line;
#endif /* ! MTA_CONTENT_LENGTH */

	/* What is the delimiter for this file? */

	delim = xstrdup((mmdf_form(file)) ? MMDF_DELIM : MFROM);

	/* Set the global flags back to their defaults */

	version_found = mime_errors_only = FALSE;

	/* Now open the file if we can */

	if ((fp = fopen(file, "r")) == NULL) {
		emsgl("Can't open ", file, ": ", strerror(errno), NULL);
		free(delim);
		return(NULL);
	}

	/* Copy the original message's headers if required */

	for (t = message->text; old_hdrs && headers
	     && t != NULL; t = t->next) {
		/* Do any special handling this line needs */

		if (node == NULL) {
			/* Initialise the new message */

			node = new_msg(t->line, message->pos);
			node->prev = node->next = NULL;
		} else if (is_header(t->line)) {
			/* Parse the header into the new message */

			parse_header(node, t->line);
		}

		/* Add this line to the text */

		node->text = add_text(xstrdup(t->line), node->text);

		/* Are we processing headers now? */

		headers = !is_blank(t->line);
	}

	/* Now add the contents of the file to the list */

	while ((line = get_line(fp, headers)) != NULL) {
		/* Do any special handling this line needs */

		if (node == NULL) {
			/* Initialise the new message */

			node = new_msg(line, message->pos);
			node->prev = node->next = NULL;

			/* Check for an error reading the message */

			if (node->bad) {
				/* The file isn't a valid mail message */

				emsgl("Error reading file: ",
				      "Not a valid mail message", NULL);
				free_messages(node);
				free(line);
				free(delim);
				return(NULL);
			}
#ifndef MTA_CONTENT_LENGTH
		} else if (!strncmp(line, delim, strlen(delim))) {
			/* Quote any delimiter within a message */

			new_line = vstrcat(DELIM_PFX, line, NULL);
			free(line);
			line = new_line;
#endif /* ! MTA_CONTENT_LENGTH */
			
		} else if (headers && is_header(line)) {
			/* We aren't handling from lines any more */

			fromlines = FALSE;

			/* Parse the header into the new message */

			parse_header(node, line);

			/* Check for an invalid header */

			if (node->bad && (version_found ||
					  !mime_errors_only)) {
				/* Get the name of the header */

				if ((colon = strchr(line, ':')) != NULL) {
					*(colon + 1) = '\0';
				}

				/* And handle the error */

				emsgl("Error reading file: ", line,
				      " header invalid", NULL);
				free_messages(node);
				free(line);
				free(delim);
				return(NULL);
			}
		} else if (headers && (!fromlines || !is_fromline(line))) {
			/* Headers must be followed by a blank line */

			if (!is_blank(line)) {
				node->text = add_text(xstrdup("\n"),
						      node->text);
			}
			fromlines = headers = FALSE;
#ifdef MTA_CONTENT_LENGTH
			/* We need to update the message length */

			length += strlen(line);
		} else {
			/* Update the length of the message body */

			length += strlen(line);
#endif /* MTA_CONTENT_LENGTH */
		}

 		/* Add this line to the text */

		node->text = add_text(line, node->text);
	}

	/* Close the file */

	(void) fclose(fp);

	/* Messages may "validly" have some invalid MIME headers */

	node->bad = (node->bad && (version_found || !mime_errors_only));

#ifdef MTA_CONTENT_LENGTH
	/* Update the message's content-length */

	node->length = length;
#endif /* MTA_CONTENT_LEN */

	/* Set up the new message's global data */

	node = add_mail(node, NULL);
	if (message->user_tags != NULL) {
		node->user_tags = xstrdup(message->user_tags);
	}

	/* Clean up and return the new message */

	free(delim);
	return(node);
}
/****************************************************************************/
MESSAGE *null_msg(list)
MESSAGE *list;
{
	/* Append a null message to list */

	MESSAGE *node;

	/* Form the null message */

	node = new_msg(NULL, 0);
	node->prev = node->next = NULL;

	/* Append the node to the list and return it */

	return(add_msg(node, list));
}
/****************************************************************************/
int read_failed()
{
	/* Return whether the last folder read failed */

	return(last_read_failed);
}
/****************************************************************************/
static MESSAGE *parse_mail(fp, folder, list, readfunc, mmdf)
FILE *fp;
char *folder;
MESSAGE *list;
char *(*readfunc)();
int mmdf;
{
	/*
	 * Actually parse a mailbox, appending the messages read
	 * to list.
	 */

	char *delim, *line;
	int fromlines = TRUE;
	int headers = TRUE;
	int added_msg, update;
	unsigned msg_no;
	MESSAGE *node = NULL;

#ifdef MTA_CONTENT_LENGTH
	unsigned length = 0;
#endif /* MTA_CONTENT_LENGTH */
	
#ifdef MTA_BLANK_SEPARATED
	int last_line_blank = TRUE;
#endif /* MTA_BLANK_SEPARATED */

	/* Set the initial message number */

	msg_no = count_messages(list, TRUE) + 1;

	/* How often should we update the message count? */

	update = (folder != NULL) ? get_vval(V_MSG_UPDATE) : 0;

	/* What is the delimiter for this folder? */

	delim = xstrdup((mmdf) ? MMDF_DELIM : MFROM);

	/* Process each line as it is read */

	while ((line = readfunc(fp, headers)) != NULL) {
		/* Check for a new message */

#ifdef MTA_CONTENT_LENGTH
		if (node == NULL || length + strlen(line) >= node->length
		    && !strncmp(line, delim, strlen(delim))) {
#else /* ! MTA_CONTENT_LENGTH */
#ifdef MTA_BLANK_SEPARATED
		if (node == NULL || last_line_blank &&
		    !strncmp(line, delim, strlen(delim))) {
#else /* ! MTA_BLANK_SEPARATED */
		if (node == NULL || !strncmp(line, delim, strlen(delim))) {
#endif /* ! MTA_BLANK_SEPARATED */
#endif /* ! MTA_CONTENT_LENGTH */
			/* Update the message count for the first message */

			if (node == NULL && update) {
				msgl("Reading ", folder, "; message ",
				     utos(msg_no), "...", NULL);
			}

			/* Is this message not empty? */

			added_msg = (node != NULL && node->text != NULL);

#ifdef MTA_CONTENT_LENGTH
			/* Update the node's content-length */

			if (node != NULL) {
				node->length = length;
				length = 0;
			}
#endif MTA_CONTENT_LENGTH
			/* Add any previous message to the list */

			list = add_mail(node, list);
			
			/* Skip MMDF delimiter line(s) */

			while (mmdf && line != NULL &&
				!strncmp(line, delim, strlen(delim))) {
				line = readfunc(fp, FALSE);
			}

			/* Update the message number as required */

			msg_no = (added_msg) ? msg_no + 1 : msg_no;

			/* Create a new node for the message */

			node = new_msg(line, msg_no);
			fromlines = headers = TRUE;

			/* Update the message count if required */

			if (update && added_msg && (msg_no % update) == 0) {
				msgl("Reading ", folder, "; message ",
				     utos(msg_no), "...", NULL);
			}
		} else if (headers && is_header(line)) {
			/* Parse header lines */

			parse_header(node, line);
			fromlines = FALSE;
		} else if (headers && (!fromlines || !is_fromline(line))) {
			/* Headers must be followed by a blank line */

			if (!is_blank(line)) {
				node->text = add_text(xstrdup("\n"),
						      node->text);
			}
			fromlines = headers = FALSE;
#ifdef MTA_CONTENT_LENGTH
			/* We need to update the message length */

			length += strlen(line);
		} else {
			/* Update the length of the message body */

			length += strlen(line);
#endif /* MTA_CONTENT_LENGTH */
		}

		/* Add non-null lines to the message */

		if (line != NULL) {
			node->text = add_text(line, node->text);
		}

#ifdef MTA_BLANK_SEPARATED
		/* Store whether this line was blank */

		last_line_blank = !headers && is_blank(line);
#endif /* MTA_BLANK_SEPARATED */
	}

#ifdef MTA_CONTENT_LENGTH
	/* Update any final message's content-length */

	if (node != NULL) {
		node->length = length;
	}
#endif MTA_CONTENT_LENGTH

	/* Add the last message to the list */
		
	list = add_mail(node, list);

	/* Clean up and return the list of messages */

	free(delim);
	return(list);
}
/****************************************************************************/
static MESSAGE *new_msg(line, position)
char *line;
unsigned position;
{
	/*
	 * Set up the node structure for a new message.  If line is
	 * NULL them the message is a dummy, otherwise the from,
	 * group, reply and date fields are set from the details
	 * specified in line, if available.  All other fields are
	 * set to default values.
	 */

	char *sender, *date;
	int ref;
	MESSAGE *node;

	/* Get the storage for the new node */

	node = (MESSAGE *) xmalloc(sizeof(MESSAGE));

	/* Set the default values for the fields */

	node->from = xstrdup(ERRUSER);
	node->addr = xstrdup(ERRUSER);
	node->subject = xstrdup(DEFSUBJECT);

	node->group = node->reply = node->cc = NULL;
	node->contype = node->encoding = NULL;
	for (ref = 0; ref < NO_REFERENCES; ref++) {
		node->refs[ref] = NULL;
	}
	node->date = NULL;
	node->text = NULL;
	node->pos = position;
	node->id = NULL;

#ifdef MTA_CONTENT_LENGTH
	node->length = 0;
#endif /* MTA_CONTENT_LENGTH */
	
	node->sys_tags = node->user_tags = NULL;
	node->visible = node->new = TRUE;
	node->nontext = node->deleted = node->bad = FALSE;
	node->read = node->replied = node->forwarded = FALSE;
	node->saved = node->printed = FALSE;

	/* Check if line implies an erroneous message */

	if (line == NULL || strncmp(line, MFROM, strlen(MFROM))) {
		return((line == NULL) ? node : msg_error(node));
	}

	/* Now find the sender and date in the line */

	sender = strchr(line, ' ') + 1;
	if ((date = strchr(sender, ' ')) != NULL) {
		*date++ = '\0';
	}

	/* Set the from and reply fields */

	node = msg_from(node, sender);

	/* Set the date field if possible */

	if (date != NULL) {
		node = msg_date(node, date);
		*(date - 1) = ' ';
	}

	return(node);
}
/****************************************************************************/
static MESSAGE *add_mail(node, list)
MESSAGE *node, *list;
{
	/* Handle the global processing after each message is formed */

	char *group;

	/* Check there was a message */

	if (node == NULL) {
		return(list);
	} else if (node->text == NULL) {
		free(node);
		return(list);
	}

	/* Set the group reply address for the message */

	if (node->group != NULL && node->reply != NULL) {
		group = xmalloc(strlen(node->reply) +
				strlen(node->group) + 3);
		(void) sprintf(group, "%s, %s", node->reply, node->group);
		free(node->group);
		node->group = canonical(group);
		free(group);
	}

	/* Update the error and mime values if no mime-version found */

	if (!version_found && node->bad && mime_errors_only) {
		/* Unset the message's MIME values */

		if (node->contype != NULL) {
			free(node->contype);
			node->contype = NULL;
		}
		if (node->encoding != NULL) {
			free(node->encoding);
			node->encoding = NULL;
		}

		/* We don't count this node as invalid */

		node->bad = FALSE;
	}

	/* Check if the message can be handled internally */

	node->nontext = !textual(node);

	/* Set the system tags for the message */

	set_sys_tags(node);

	/* Trim the message text and append the node to the list */

	node = trim_text(node);
	list = add_msg(node, list);

	/* Set the global flags back to their defaults */

	reply_found = id_found = FALSE;
	version_found = mime_errors_only = FALSE;

	return(list);
}
/****************************************************************************/
static MESSAGE *add_msg(node, list)
MESSAGE *node, *list;
{
	/* This function simply adds a node to the list of headers */

	MESSAGE *m;

	/* Handle adding to a null list */

	if (list == NULL) {
		/* The new list is the node */

		node->prev = node->next = NULL;
		return(node);
	} else if (list->text == NULL) {
		/* We need to insert the entry before this null message */

		node->prev = NULL;
		node->next = list;
		list->prev = node;

		return(node);
	} else {
		/*
		 * Find the last entry in the list.  We don't use
		 * recursion 'cos it's too slow on large lists.
		 */

		for (m = list; m->next != NULL &&
		     m->next->text != NULL; m = m->next) {
			/* NULL LOOP */
		}

		/* Append the new node */

		node->prev = m;
		if ((node->next = m->next) != NULL) {
			m->next->prev = node;
		}
		m->next = node;

		return(list);
	}
	/*NOTREACHED*/
}
/****************************************************************************/
static MSG_TEXT *add_text(line, list)
char *line;
MSG_TEXT *list;
{
	/*
	 * Add a line to a list of strings - basic list handling,
	 * except that we enhance performance by keeping a static
	 * pointer to the last entry.
	 */

	static MSG_TEXT *last;
	MSG_TEXT *node;

	node = (MSG_TEXT *) xmalloc(sizeof(MSG_TEXT));
	node->line = line;
	node->next = NULL;

	/* Add the node to the list */

	if (list == NULL) {
		list = node;
	} else {
		last->next = node;
	}

	/* Set the copy of the last entry before we exit */

	last = node;
	return(list);
}
/****************************************************************************/
static MESSAGE *trim_text(node)
MESSAGE *node;
{
	/* Trim all trailing null lines from a message */

	char *null_line = "\n";
	MSG_TEXT *line, *next, *last_line = NULL;

	/* Find the first trailing null line */

	for (line = node->text; line->next != NULL; line = line->next) {
		last_line = (!strcmp(line->next->line, null_line)) ?
			(last_line != NULL) ? last_line : line : NULL;
	}

	/* Did we find any null lines? */

	if (last_line != NULL) {
		/* Remove the null lines */

		line = last_line->next;
		last_line->next = NULL;
		while (line != NULL) {
#ifdef MTA_CONTENT_LENGTH
			/* Update the node's content-length */

			node->length--;
#endif /* MTA_CONTENT_LENGTH */

			/* And free the line */

			next = line->next;
			free(line->line);
			free(line);
			line = next;
		}
	}

	return(node);
}
/****************************************************************************/
static void parse_header(node, line)
MESSAGE *node;
char *line;
{
	/* Process the header if it requires it */

	char *colon;
	int i, namelen;

	/* Find the colon in the line */

	colon = strchr(line, ':');

	/* Calculate the length of the header name */

	namelen = colon - line + 1;

	/* Look for a handling function */

	for (i = 0; h_ptab[i].h_name != NULL; i++) {
		if (!strncasecmp(h_ptab[i].h_name, line, namelen)) {
			/* Call the function to handle the header */

			node = h_ptab[i].h_func(node, colon + 1);
			break;
		}
	}

	return;
}
/****************************************************************************/
static MESSAGE *msg_date(node, date)
MESSAGE *node;
char *date;
{
	/* Set the date field of a message from a Date: header */

	DATEZONE *date_val;

	/* Parse the dat and handle failure */

	if ((date_val = parse_date(date)) != NULL) {
		/* Set the node's date to the value */

		if (node->date != NULL) {
			free(node->date);
		}
		node->date = date_val;
	} else {
		/* Error in the date header */

		node = msg_error(node, FALSE);
	}

	return(node);
}
/****************************************************************************/
static MESSAGE *msg_from(node, addrs)
MESSAGE *node;
char *addrs;
{
	/*
	 * Set the from, group and reply fields of a message from
	 * a From: header, if not already specified by a To: or
	 * Reply-To: header respectively.
	 */

	char *group;

	/* Get the canonical form of the address list */

	if ((group = canonical(addrs)) == NULL) {
		return(msg_error(node, FALSE));
	}

	/* Set the names associated with the addresses */

	if (node->from != NULL) {
		free(node->from);
	}
	node->from = xstrdup(strcanon(addrnames(), SK_HEADER));
	if (node->addr != NULL) {
		free(node->addr);
	}
	node->addr = mail_addresses(group);

	/* Set the reply address if not set via Reply-To */

	if (!reply_found) {
		/* Update the node's reply address */

		if (node->reply != NULL) {
			free(node->reply);
		}
		node->reply = group;
	} else {
		/* Free the unwanted group */

		free(group);
	}

	return(node);
}
/****************************************************************************/
static MESSAGE *msg_reply(node, addrs)
MESSAGE *node;
char *addrs;
{
	/* Set the reply field of a message from a Reply-To: header */

	char *reply;

	/* Get the canonical form of the address */

	if ((reply = canonical(addrs)) == NULL) {
		return(msg_error(node, FALSE));
	}

	/* Update the node's reply address */

	if (node->reply != NULL) {
		free(node->reply);
	}
	node->reply = reply;

	/* Note that we've found a Reply-To header */

	reply_found = TRUE;
	return(node);
}
/****************************************************************************/
static MESSAGE *msg_subject(node, subject)
MESSAGE *node;
char *subject;
{
	/* Set the subject of a message from a Subject: header */

	char *start, *end, *buf;
	int len;

	/*
	 * First we must canonicalise the subject string -
	 * Trim leading and trailing white spaces.
	 */

	/* Skip leading spaces in the subject */

	for (start = subject; isspace(*start); start++) {
		/* NULL LOOP */
	}

	/* And trailing spaces and the newline */

	end = start + strlen(start);
	while (end > start && isascii(*(end - 1))
	       && isspace(*(end - 1))) {
		end--;
	}

	/* Allocate space for the subject and fill it */

	if ((len = end - start) > 0) {
		/* Copy the subject into the buffer */

		buf = xmalloc(len * MAX_KEY_LEN + 1);
		(void) strncpy(buf, start, len);
		buf[len] = '\0';

		/* Update the node's subject and free the buffer */

		node->subject = xstrdup(strcanon(buf, SK_HEADER));
		free(buf);
	}

	return(node);
}
/****************************************************************************/
static MESSAGE *msg_to(node, addrs)
MESSAGE *node;
char *addrs;
{
	/* Set the group field of a message from a To: header */

	char *group;

	/* Canonicalise the address list */

	if ((group = canonical(addrs)) == NULL) {
		return(msg_error(node, FALSE));
	}

	/* And update the node's group addresses */

	if (node->group != NULL) {
		free(node->group);
	}
	node->group = group;

	return(node);
}
/****************************************************************************/
static MESSAGE *msg_cc(node, addrs)
MESSAGE *node;
char *addrs;
{
	/* Set the cc field of a message from a Cc: header */

	char *cc;

	/* Canonicalise the address list */

	if ((cc = canonical(addrs)) == NULL) {
		return(msg_error(node, FALSE));
	}

	/* And update the node's cc addresses */

	if (node->cc != NULL) {
		free(node->cc);
	}
	node->cc = cc;

	return(node);
}
/****************************************************************************/
/*ARGSUSED*/
static MESSAGE *msg_version(node, version)
MESSAGE *node;
char *version;
{
	/* Check that the message's MIME-Version header is correct */

	version_found = TRUE;
	return(node);
}
/****************************************************************************/
static MESSAGE *msg_type(node, ctype)
MESSAGE *node;
char *ctype;
{
	/* Set the type of a message from a Content-Type: header */

	char *type;

	/* Canonicalise the content type */

	if ((type = c_contype(ctype)) == NULL) {
		return(msg_error(node, TRUE));
	}

	/* And update the node's content type */

	if (node->contype != NULL) {
		free(node->contype);
	}
	node->contype = type;

	return(node);
}
/****************************************************************************/
static MESSAGE *msg_encoding(node, cte)
MESSAGE *node;
char *cte;
{
	/*
	 * Set the encoding of a message from a
	 * Content-Transfer-Encoding: header.
	 */

	char *enc;

	/* Canonicalise the encoding */

	if ((enc = c_encoding(cte)) == NULL) {
		return(msg_error(node, TRUE));
	}

	/* And update the node's encoding */

	if (node->encoding != NULL) {
		free(node->encoding);
	}
	node->encoding = enc;

	return(node);
}
/****************************************************************************/
static MESSAGE *msg_id(node, id)
MESSAGE *node;
char *id;
{
	/* Set the references of a message from a Message-ID header */

	parse_refs(node, id, REFS_FROM_MSG_ID);
	return(node);
}
/****************************************************************************/
static MESSAGE *msg_inreply(node, id)
MESSAGE *node;
char *id;
{
	/* Set the references of a message from an In-Reply-To header */

	parse_refs(node, id, REFS_FROM_IN_REPLY_TO);
	return(node);
}
/****************************************************************************/
static MESSAGE *msg_refs(node, refs)
MESSAGE *node;
char *refs;
{
	/* Set the references of a message from a references header */

	parse_refs(node, refs, REFS_FROM_REFERENCES);
	return(node);
}
/****************************************************************************/
#ifdef MTA_CONTENT_LENGTH
static MESSAGE *msg_length(node, length)
MESSAGE *node;
char *length;
{
	/* Set the length of the body of the current message */

	node->length = atol(length);
	return(node);
}
#endif /* MTA_CONTENT_LENGTH */
/****************************************************************************/
static MESSAGE *msg_status(node, status)
MESSAGE *node;
char *status;
{
	/* Set the status fields of a message */

	char *p;

	/* Handle each option set in the status list */

	for (p = status; *p != '\0'; p++) {
		switch(*p) {
		case ST_NEW:
			node->new = TRUE;
			node->read = FALSE;
			break;
		case ST_OLD:
			node->new = FALSE;
			break;
		case ST_READ:
			node->read = TRUE;
			break;
		case ST_UNREAD:
			node->read = FALSE;
			break;
		case ST_SAVED:
			node->saved = TRUE;
			break;
		case ST_PRINTED:
			node->printed = TRUE;
			break;
		case ST_REPLIED:
			node->replied = TRUE;
			break;
		case ST_FORWARDED:
			node->forwarded = TRUE;
			break;
		default:
			break;
		}
	}

	/* Return the updated node */

	return(node);
}
/****************************************************************************/
static MESSAGE *msg_tags(node, tags)
MESSAGE *node;
char *tags;
{
	/* Set the tags of a message, as modified by persistent-tags */

	TAG_LIST *tlist;

	/* Make a tag list from the tags and mask it */

	tlist = taglist(tags, TL_SET);
	mask_tags(tlist);

	/* Set the remaining tags on the message */

	(void) set_tags(node, tlist);
	free_tlist(tlist);

	return(node);
}
/****************************************************************************/
static MESSAGE *msg_error(node, mime)
MESSAGE *node;
int mime;
{
	/* Handle an error in the headers of the current message */

	mime_errors_only = (mime && (mime_errors_only || !(node->bad)));
	node->bad = TRUE;
	return(node);
}
/****************************************************************************/
static void parse_refs(node, refs, whence)
MESSAGE *node;
char *refs;
int whence;
{
	/* 
	 * Parse the message ids in the string refs and if it
	 * is valid copy the canonical forms into the references
	 * for node.
	 */

	char *crefs, *cref;
	int ref_found = FALSE;
	ATOM *alist, *start, *end;

	/* First canonicalise and tokenise the reference string */

	if ((crefs = refextract(refs)) == NULL
	    || (alist = tokenise(crefs)) == NULL) {
		/* Invalid references; check for Message-Id */

		if (whence == REFS_FROM_MSG_ID) {
			(void) msg_error(node, FALSE);
		}

		/* Free the references and return */

		if (crefs != NULL) {
			free(crefs);
		}
		return;
	}

	/* Free the canonical reference text */

	free(crefs);

	/* Loop through the atom list finding references */

	while ((start = asearch(alist, AT_LANGLEB)) != NULL &&
	       (end = asearch(start, AT_RANGLEB)) != NULL) {
		/* Cut out this reference */

		afree(acut(alist, start));
		alist = end->next;
		end->next = NULL;

		/* Form the text of the reference */

		cref = atext(NULL, start, AC_FULL);
		afree(start);

		/* Add the reference to the list */

		add_ref(node, cref, whence);
		ref_found = TRUE;

		/* Give up now if parsing a Message-Id */

		if (whence == REFS_FROM_MSG_ID) {
			break;
		}
	}

	/* Check for an erroneous Message-Id: header */

	if (whence == REFS_FROM_MSG_ID && !ref_found) {
		(void) msg_error(node, FALSE);
	}

	/* Set the Message-ID found flag */

	id_found = (id_found || ref_found && whence == REFS_FROM_MSG_ID);

	/* Free any trailing tokens in alist and return */

	afree(alist);
	return;
}
/****************************************************************************/
static void add_ref(node, cref, whence)
MESSAGE *node;
char *cref;
int whence;
{
	/* Add a reference to the node's references */

	int last, ref;

	/* Which references are we allowed to modify? */

	last = (whence == REFS_FROM_MSG_ID || !id_found)
		? NO_REFERENCES - 1 : NO_REFERENCES - 2;
	
	/* Check if the reference was already defined */

	for (ref = 0;  ref < NO_REFERENCES; ref++) {
		if (node->refs[ref] != NULL &&
		    !strcmp(node->refs[ref], cref)) {
			/* The reference was already defined */

			return;
		}
	}

	/* Free the oldest reference if set */

	if (node->refs[0] != NULL) {
		free(node->refs[0]);
	}

	/* Shift the references as required */

	for (ref = 0; ref < last; ref++) {
		node->refs[ref] = node->refs[ref + 1];
	}

	/* Add the new value to the references */

	node->refs[last] = cref;
	return;
}
/****************************************************************************/
