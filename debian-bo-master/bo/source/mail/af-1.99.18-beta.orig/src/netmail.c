/* Netmail.c - POP3 and SMTP mail handlers
   Copyright (C) 1995, 1996, 1997 Malc Arnold, Ian Dickinson.

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
#include <errno.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <ctype.h>
#include <fcntl.h>
#include <varargs.h>
#include "af.h"
#include "sendmail.h"
#include "netmail.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "io.h"
#include "misc.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info. */

#ifndef lint
static char *RcsId = "$Id: netmail.c,v 1.9 1997/05/05 02:50:01 malc Exp $";
static char *NetmailId = NETMAILID;
#endif /* ! lint */

/****************************************************************************/
/* Ensure that ETIMEDOUT has a reasonable definition */

#ifndef ETIMEDOUT
#define ETIMEDOUT EPIPE
#endif /* ! ETIMEDOUT */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup();
extern char *vstrcat(), *strerror(), *get_user();
extern char *get_host(), *get_vtext(), *get_hdr();
extern char *get_line(), *get_pwstr(), *utos();
extern int atoi();
extern time_t time();
extern unsigned alarm();
extern unsigned save_atimer();
extern unsigned restore_atimer();
extern void free(), msg(), cmsg();
extern void msgl(), emsgl();
extern void clearmsg(), typeout();
extern RETSIGTYPE (*signal())();

#ifdef MTA_IS_SMTP
static int nethdrs(), netfile();
#ifdef NO_MTA_CC
extern int strcasecmp();
#endif /* NO_MTA_CC */
#endif/* MTA_IS_SMTP */

#ifdef READ_VIA_POP3
extern char *strudate();
extern DATEZONE *date_now();
FILE *open_pop3();
int close_pop3();
static char *netslurp();
static char *get_uidl_id();
static int check_uidl_ids();
static void free_pfolder();
static void mask_pw();
static POPFOLDER *add_pfolder();
static POPFOLDER *get_pfolder();
#endif /* READ_VIA_POP3 */

/* Local function declarations */

static char *netread(), *tget_line();
static int netsend(), netclose();
static int reopen_pop3();
static int get_password();
static CONNECTION *netopen();
static RETSIGTYPE time_out();

/****************************************************************************/
/* Import the global and host error flags */

extern int errno, h_errno;

/****************************************************************************/
/* We store the network error value here */

static int net_error = 0;

/****************************************************************************/
/* The POP3 code stores the per-folder connection details and cache here */

#ifdef READ_VIA_POP3
static POPFOLDER *pop3_folder = NULL;
static POPFOLDER *pop3_cache = NULL;
#endif /* READ_VIA_POP3 */

/****************************************************************************/
/* A static buffer for the timeout jump */

static jmp_buf timeout_jump;

/****************************************************************************/
/* The code to handle direct SMTP submission of mail */

#ifdef MTA_IS_SMTP
/****************************************************************************/
int smtp_deliver(hostname, from, to, fp, hdrs)
char *hostname, *from, **to;
FILE *fp;
HEADER *hdrs;
{
	/* Actually deliver mail via a direct SMTP connection */

	char *buf, **addr;
	int receipts = 0;
	CONNECTION *conn;

	/* Open the SMTP connection */

	if ((conn = netopen(hostname, SMTP_SERVICE, SMTP_PROTOCOL,
			    SMTP_PORT, TRUE)) == NULL) {
		/* Error establishing the connection */

		return(FALSE);
	}

	/* Let the user know what we're doing */

	msgl("Sending mail via ", hostname, "...", NULL);

	/* Read and check the SMTP banner */

	if (netread(conn, SMTP_BANNER) == NULL) {
		return(FALSE);
	}

	/* Now introduce ourself to the server */

	if (!netsend(conn, "HELO ", get_host(), NULL)
	    || netread(conn, SMTP_OK) == NULL) {
		return(FALSE);
	}

	/* Set up the message's originator */

	if (!netsend(conn, "MAIL FROM:<", from, ">", NULL)
	    || netread(conn, SMTP_OK) == NULL) {
		return(FALSE);
	}

	/* Set up the message's recipients */

	for (addr = to; *addr != NULL; addr++) {
		/* Add this recipient for the mail */

		if ((!netsend(conn, "RCPT TO:<", *addr, ">", NULL))
		    || (buf = netread(conn, NULL)) == NULL) {
			return(FALSE);
		}

		/* Check the recipient was valid */

		if (strncmp(buf, SMTP_OK, strlen(SMTP_OK))) {
			/* Invalid recipient; notify the user */

			typeout("Invalid recipient ");
			typeout(*addr);
			typeout(": \n");
			typeout(buf);
			typeout(": \n");
		} else {
			/* Count the number of recipients */

			receipts++;
		}
	}

	/* Check if the message was sent */

	if (!receipts) {
		typeout("No valid recipents for mail...\n");
		typeout(NULL);
		return(FALSE);
	}
		
	/* Now set up to send the message's headers and body */

	if (!netsend(conn, "DATA", NULL) ||
	    netread(conn, SMTP_SEND) == NULL) {
		return(FALSE);
	}

	/* Actually send the message's headers and body */

	if (!nethdrs(conn, hdrs) || !netfile(conn, fp)) {
		return(FALSE);
	}

	/* Terminate the message and check the response */

	if (!netsend(conn, ".", NULL) ||
	    netread(conn, SMTP_OK) == NULL) {
		return(FALSE);
	}

	/* Now shut down the SMTP server */

	if (!netsend(conn, "QUIT", NULL) ||
	    netread(conn, SMTP_QUIT) == NULL) {
		return(FALSE);
	}

	/* Close the connection and return success */

	(void) netclose(conn);
	typeout(NULL);
	return(TRUE);
}
/****************************************************************************/
int nethdrs(conn, hdrs)
CONNECTION *conn;
HEADER *hdrs;
{
	/* Write the headers to the specified network connection */

	HEADER *h;
	
	/* Loop over the standard headers */

	for (h = hdrs; h != NULL; h = h->next) {
		/* Send any header which is defined */

		if (h->hdr_text != NULL) {
#ifdef NO_MTA_CC
			/* Handle the Bcc header when sending */

			if (!strcasecmp(h->name, BCC) ||
			    !strcasecmp(h->name, RESENT_BCC)) {
				/* Might want an empty Bcc: header */

				if (get_hdr(TO, hdrs) == NULL &&
				    get_hdr(RESENT_TO, hdrs) == NULL
				    && get_hdr(CC, hdrs) == NULL &&
				    get_hdr(RESENT_CC, hdrs) == NULL &&
				    !netsend(conn, h->name, "\n", NULL)) {
					/* Error sending the header */

					return(FALSE);
				}

				/* Skip the rest of the header text */

				continue;
			}
#endif /* NO_MTA_CC */
			/* Send the text and check the return */

			if (!netsend(conn, h->hdr_name, " ",
				     h->hdr_text, NULL)) {
				/* Error sending the text */

				return(FALSE);
			}
		}
	}

	/* Output a blank line following the headers */

	return(netsend(conn, NULL));
}
/****************************************************************************/
#endif /* MTA_IS_SMTP */
/****************************************************************************/
/* The following routines relate to reading mail via POP3 */

#ifdef READ_VIA_POP3
/****************************************************************************/
char *fullfolder(folder)
char *folder;
{
	/* Return the folder in :user@fqdn form in an allocated buffer */

	char *at, *user, *server;
	char *fqdn, *newfolder;
	struct hostent *host;

	/* Extract the folder's user and host names */

	if ((at = strchr(folder, '@')) != NULL) {
		/* Extract the user name from the folder */

		user = xmalloc(at - folder);
		(void) strncpy(user, folder + 1, at - folder);
		user[at - folder - 1] = '\0';

		/* And the server name follows the at */

		server = at + 1;
	} else {
		/* Use the current user name for the folder */

		user = xstrdup(get_user());

		/* And the folder name is only the server  */

		server = folder + 1;
	}

	/* Try calling gethostbyname to get the FQDN */

	fqdn = ((host = gethostbyname(server)) != NULL)
		? (char *) host->h_name : server;

	/* Now build the new folder name */

	newfolder = vstrcat(":", user, "@", fqdn, NULL);

	/* Clean up and return the folder name */

	free(user);
	return(newfolder);
}
/****************************************************************************/
FILE *open_pop3(folder, offset, verbose)
char *folder;
long offset;
int verbose;
{
	/*
	 * Open and authenticate a connection to a POP3 server
	 * specified by the folder name in the form :user@host.
	 */

	char *buf;

	/* Check if the folder's details are cached */

	if ((pop3_folder = get_pfolder(folder)) != NULL
	    && pop3_folder->conn != NULL &&
	    reopen_pop3(pop3_folder, offset)) {
		/* The Connection's still ok; return it */

		return(pop3_folder->conn->rsock);
	}

	/* Create a new folder cache entry if required */

	if (pop3_folder == NULL) {
		pop3_folder = add_pfolder(folder);
	}

	/* Open a connection to the POP3 server */

	if ((pop3_folder->conn =
	     netopen(pop3_folder->server, POP3_SERVICE, POP3_PROTOCOL,
		     POP3_PORT, verbose)) == NULL) {
		/* Error establishing the connection */

		free_pfolder(pop3_folder);
		return(NULL);
	}

	/* We may need a password for the folder */

	if (!get_password(pop3_folder)) {
		/* User quit; abort the connection now */

		netclose(pop3_folder->conn);
		free_pfolder(pop3_folder);
		return(NULL);
	}

	/* Let the user know we're authenticating */

	if (verbose) {
		msgl("Authenticating ", pop3_folder->user,
		     " on ", pop3_folder->server, "...", NULL);
	}

	/* Now read the POP3 banner */

	if (!netread(pop3_folder->conn, POP3_OK)) {
		free_pfolder(pop3_folder);
		return(NULL);
	}

	/* First we announce ourselves to the server */

	if (!netsend(pop3_folder->conn, "USER ", pop3_folder->user, NULL)
	    || netread(pop3_folder->conn, POP3_OK) == NULL) {
		free_pfolder(pop3_folder);
		return(NULL);
	}

	/* Then we authenticate ourselves to the server */

	mask_pw(pop3_folder->pw, pop3_folder->mask);
	if (!netsend(pop3_folder->conn, "PASS ", pop3_folder->pw, NULL)
	    || netread(pop3_folder->conn, POP3_OK) == NULL) {
		/* Failed to get authentication */

		mask_pw(pop3_folder->pw, pop3_folder->mask);
		free_pfolder(pop3_folder);
		return(NULL);
	}
	mask_pw(pop3_folder->pw, pop3_folder->mask);

	/* Confirm the authentication to the user */

	if (verbose) {
		cmsg(" Done");
	}

	/* Attempt to find out how many messages there are */

	if (!netsend(pop3_folder->conn, "STAT", NULL) ||
	    (buf = netread(pop3_folder->conn, POP3_OK)) == NULL) {
		free_pfolder(pop3_folder);
		return(NULL);
	}

	/* Set up the remaining connection details */

	pop3_folder->size = (unsigned) atoi(buf + strlen(POP3_OK));
	pop3_folder->msg = pop3_folder->offset = offset;
	pop3_folder->synced = pop3_folder->closed = FALSE;
	pop3_folder->error = 0;

	/* We've got a valid connection; return the file */

	return(pop3_folder->conn->rsock);
}
/****************************************************************************/
int close_pop3(update)
int update;
{
	/* Shut down and disconnect the current POP3 folder */

	if (update || pop3_folder->error) {
		/* Close the folder to update or clear any errors */

		if ((update || netsend(pop3_folder->conn, "RSET", NULL)
		     && netread(pop3_folder->conn, POP3_OK) != NULL)
		    && netsend(pop3_folder->conn, "QUIT", NULL)
		    && netread(pop3_folder->conn, POP3_OK) != NULL) {
			/* Close the actual connection */

			(void) netclose(pop3_folder->conn);
		}

		/* And update the folder's status */

		pop3_folder->conn = NULL;
		pop3_folder->closed = TRUE;
	}

	/* Now return the folder's status */

	return(pop3_folder->error);
}
/****************************************************************************/
void kill_pop3()
{
	/* Kill off all outstanding POP3 connections */

	POPFOLDER *pfolder;

	/* Loop through the cache entries closing connections */

	for (pfolder = pop3_cache; pfolder != NULL; pfolder = pfolder->next) {
		/* Is this folder still connected to a server? */

		if (pfolder->conn != NULL) {
			/* Reset and close the connection */
			
			if (reopen_pop3(pfolder, pfolder->offset) &&
			    netsend(pfolder->conn, "RSET", NULL) &&
			    netread(pfolder->conn, POP3_OK) != NULL &&
			    netsend(pfolder->conn, "QUIT", NULL) &&
			    netread(pfolder->conn, POP3_OK) != NULL) {
				/* Close the actual connection */

				(void) netclose(pop3_folder->conn);
			}

			/* And update the folder's status */

			pop3_folder->conn = NULL;
			pop3_folder->closed = TRUE;
		}
	}

	/* That's all folks */

	return;
}
/****************************************************************************/
/*ARGSUSED*/
char *read_pop3(fp, hdrs)
FILE *fp;
int hdrs;
{
	/* Read and return a single logical line from a POP3 server */

	char *line = NULL;

	/* If we can just read another line then do so */

	if (pop3_folder->msg > pop3_folder->offset) {
		if ((line = netslurp(pop3_folder->conn, hdrs)) != NULL) {
			/* We got a line, just return it */

			return(line);
		}
	}

	/* Update the POP3 error number */

	pop3_folder->error = (pop3_folder->msg > pop3_folder->offset)
		? net_error : 0;

	/* Couldn't read a line, can we read another message? */

	if (!pop3_folder->error && pop3_folder->msg++ < pop3_folder->size) {
		/* There is a message, can we retrieve it? */

		if (!netsend(pop3_folder->conn, "RETR ",
			     utos(pop3_folder->msg), NULL) ||
		    netread(pop3_folder->conn, POP3_OK) == NULL) {
			/* Error sending the command */

			pop3_folder->conn = NULL;
			pop3_folder->error = net_error;
			return(NULL);
		}

		/* Return a separator for the message */

		return(vstrcat(MFROM, get_user(), "@", get_vtext(V_DOMAIN),
			       " ", strudate(date_now()), "\n", NULL));
	}

	/* End of input; set synchonised flag and return */

	pop3_folder->synced = TRUE;
	return(NULL);
}
/****************************************************************************/
int write_pop3(messages, folder, size)
MESSAGE *messages;
char *folder;
unsigned size;
{
	/*
	 * Update a POP3 folder with messages.  Of course, we can't
	 * do a proper update, since POP3 is so limited in the
	 * facilities it offers.  So all we do is delete any messages
	 * that are marked for deletion, which is about all that POP3
	 * will let us do.  Alternatively, if the message list is
	 * NULL, then we delete all the messages in the mailbox.
	 */

	int saved = 0, msg_no;
	MESSAGE *m;

	/* Let the user know what we're doing */

	if (messages != NULL) {
		msgl("Writing ", folder, "...", NULL);
	}

	/* First we see if we can open the folder */

	if (!open_pop3(folder, 0L, messages != NULL)) {
		/* Failed to open the folder */

		return(WM_FAILED);
	}

	/* Once the folder's open, run a UIDL sanity check */

	if (!(pop3_folder->synced) && !check_uidl_ids(messages)) {
		(void) close_pop3(FALSE);
		return(WM_REREAD);
	}

	/* Now check if the folder has grown since last read */

	if (pop3_folder->size > size) {
		(void) close_pop3(FALSE);
		return(WM_RESYNC);
	}

	/* Do we really need to delete all the messages? */

	for (msg_no = 1; messages == NULL &&
	     msg_no <= pop3_folder->size; msg_no++) {
		/* Delete this message in the mailbox */

		if ((!netsend(pop3_folder->conn, "DELE ", utos(msg_no), NULL)
		     || netread(pop3_folder->conn, POP3_OK) == NULL)) {
			/* Error deleting the message */

			pop3_folder->conn = NULL;
			return(WM_FAILED);
		}
	}

	/* Update the size of the folder */

	pop3_folder->size = (messages == NULL) ? 0 : pop3_folder->size;

	/* Now delete only the messages we need to */

	for (m = messages; m != NULL && m->text != NULL; m = m->next) {
		/* Update the count of messages "saved" */

		saved = (m->deleted) ? saved : saved + 1;

		/* Delete this message if required */

		if (m->deleted &&
		    (!netsend(pop3_folder->conn, "DELE ", utos(m->pos), NULL)
		     || netread(pop3_folder->conn, POP3_OK) == NULL)) {
			/* Error deleting the message */

			pop3_folder->conn = NULL;
			return(WM_FAILED);
		}

		/* Update the size of the folder */

		pop3_folder->size -= (m->deleted) ? 1 : 0;
	}

	/* Deleted the messages; now update the folder */

	if (close_pop3(TRUE)) {
		/* Error updating the folder */

		return(WM_FAILED);
	}

	/* Confirm the write and return success */

	if (messages != NULL) {
		msgl("(Wrote ", utos(saved), " message",
		     (saved == 1) ? ")" : "s)", NULL);
	}

	/* Update the folder size and return success */

	pop3_folder->size = saved;
	return(WM_OK);
}
/****************************************************************************/
void netid_pop3(messages)
MESSAGE *messages;
{
	/* Set the POP3 UIDL IDs for each message in the list */

	char *id;
	MESSAGE *m;

	/* Loop over the message, extracting IDs */

	for (m = messages; m != NULL && m->text != NULL; m = m->next) {
		/* Get the UIDL id from the server */

		if ((id = get_uidl_id(m->pos)) == NULL) {
			/* This server doesn't handle UIDL */

			return;
		}

		/* Now set the message's ID */

		if (m->id != NULL) {
			free(m->id);
		}
		m->id = xstrdup(id);
	}

	/* That's it */

	return;
}
/****************************************************************************/
long size_pop3(folder)
char *folder;
{
	/* Return the size of the POP3 folder in messages */

	if ((pop3_folder = get_pfolder(folder)) != NULL
	    && pop3_folder->closed) {
		/* Use the size when we closed the folder */

		pop3_folder->closed = FALSE;
		return(pop3_folder->size);
	} else if (open_pop3(folder, 0L, FALSE) != NULL) {
		/* Got the size, now return it */

		(void) close_pop3(FALSE);
		return(pop3_folder->size);
	}

	/* Can't access folder; return 0 */

	return(0);
}
/****************************************************************************/
static int reopen_pop3(folder, offset)
POPFOLDER *folder;
long offset;
{
	/* Update a folder's details, checking silently for errors */

	char *buf;

	/* Try and get the status of the POP3 folder */

	if (fprintf(folder->conn->wsock, "STAT\r\n") != EOF
	    && fflush(folder->conn->wsock) != EOF
	    && (buf = tget_line(folder->conn->rsock,
				NET_TIMEOUT, FALSE)) != NULL) {
		/* Connection still ok; update and use it */

		folder->size = (unsigned) atoi(buf + strlen(POP3_OK));
		folder->msg = folder->offset = offset;
		return(TRUE);
	}

	/* The connection has died; close the socket and fail */

	(void) netclose(folder->conn);
	folder->conn = NULL;
	return(FALSE);
}
/****************************************************************************/
static int get_password(folder)
POPFOLDER *folder;
{
	/* Get a password for the folder if required */

	char *prompt;

	/* Do we already have a password for the folder? */

	if (folder->pw == NULL) {
		/* Build the prompt and ask for the password */

		prompt = vstrcat("Password for ", folder->user,
				 " on ", folder->server, ": ", NULL);
		if ((folder->pw = get_pwstr(prompt)) == NULL) {
			/* User quit; clean up and fail */

			free(prompt);
			return(FALSE);
		}

		/* Mask the password and free the prompt */

		mask_pw(pop3_folder->pw, pop3_folder->mask);
		free(prompt);
	}

	/* And now return success */

	return(TRUE);
}
/****************************************************************************/
static POPFOLDER *add_pfolder(folder)
char *folder;
{
	/* Get and add the details for a POP3 server */

	char *at;
	POPFOLDER *pfolder;

	/* Allocate the space for the server's details */

	pfolder = (POPFOLDER *) xmalloc(sizeof(POPFOLDER));

	/* Set the server's folder, server and user names */

	pfolder->folder = xstrdup(folder);
	at = strchr(folder, '@');
	pfolder->user = xmalloc(at - folder);
	(void) strncpy(pfolder->user, folder + 1, at - folder);
	pfolder->user[at - folder - 1] = '\0';
	pfolder->server = xstrdup(at + 1);

	/* Set up or default the rest of the details */

	pfolder->pw = NULL;
	pfolder->mask = (char) (time(NULL) & 0xFF);
	pfolder->conn = NULL;
	pfolder->size = 0;
	pfolder->offset = 0;
	pfolder->msg = 0;
	pfolder->error = 0;

	/* And push the server entry onto the list */

	pfolder->next = pop3_cache;
	pop3_cache = pfolder;

	/* Now return the new server entry */

	return(pfolder);
}
/****************************************************************************/
static POPFOLDER *get_pfolder(folder)
char *folder;
{
	/* Find details of the folder in the POP connection cache */

	POPFOLDER *pfolder;

	/* Loop through the cache entries looking for the right one */

	for (pfolder = pop3_cache; pfolder != NULL; pfolder = pfolder->next) {
		/* Is this the correct folder entry? */

		if (!strcmp(pfolder->folder, folder)) {
			/* Return the entry for this folder */

			return(pfolder);
		}
	}

	/* No server entry found */

	return(NULL);
}
/****************************************************************************/
static void free_pfolder(pfolder)
POPFOLDER *pfolder;
{
	/* Free a server entry, which is at the head of the list */

	pop3_cache = pfolder->next;

	/* Updated the list, now free the pfolder */

	free(pfolder->folder);
	free(pfolder->server);
	free(pfolder->user);
	free(pfolder->pw);
	free(pfolder);

	return;
}
/****************************************************************************/
static void mask_pw(pw, mask)
char *pw, mask;
{
	/*
	 * 'Mask' a password string by XORing each character
	 * in it with a given mask.  We do this so that af
	 * core dumps won't contain peoples' passwords in
	 * clear text, which is probably a good thing.  The
	 * operation is reversible, so masking a password
	 * that's already masked gives us the original.
	 */

	char *p;

	/* This is a really trivial operation */

	for (p = pw; *p != '\0'; p++) {
		*p = (*p ^ mask);
	}
	return;
}
/****************************************************************************/
static char *get_uidl_id(msg_no)
int msg_no;
{
	/* Return the POP3 UIDL IDs for the message */

	static char *idbuf = NULL;
	char *buf, *pos, *id;

	/* Try sending a UIDL request to the server */

	if (!netsend(pop3_folder->conn, "UIDL ", utos(msg_no), NULL)
	    || (buf = netread(pop3_folder->conn, NULL)) == NULL) {
		/* Network error; abort */

		pop3_folder->conn = NULL;
		return(NULL);
	}

	/* Extract the id from the buffer */

	pos = strchr(buf, ' ');
	id = (pos != NULL) ? strchr(pos + 1, ' ') : NULL;

	/* Check that the server gave a valid response */

	if (pos == NULL || id == NULL ||
	    (pos - buf) != strlen(POP3_OK) ||
	    strncmp(buf, POP3_OK, pos - buf)) {
		/* This server doesn't support UIDL properly */

		return(NULL);
	}

	/* Free any old return value */

	if (idbuf != NULL) {
		free(idbuf);
	}

	/* Copy and return the message's ID */

	idbuf = xstrdup(id + 1);
	return(idbuf);
}
/****************************************************************************/
static int check_uidl_ids(messages)
MESSAGE *messages;
{
	/* Use UIDL to check the IDs of the messages in the folder */

	char *id;
	MESSAGE *m;

	/* Loop over the messages, checking the IDs */

	for (m = messages; m != NULL && m->text != NULL; m = m->next) {
		/* Check the ID if the message has one */

		if (m->id == NULL || (id = get_uidl_id(m->pos)) == NULL
		     || strcmp(id, m->id)) {
			/* The UIDL check failed */

			return(FALSE);
		}
	}

	/* All the message IDs are valid */

	return(TRUE);
}
/****************************************************************************/
#endif /* READ_VIA_POP3 */
/****************************************************************************/
static CONNECTION *netopen(hostname, service, protocol, port, verbose)
char *hostname, *service, *protocol;
unsigned short port;
int verbose;
{
	/* Open and connect a socket to a service on the host */

	int sockdesc;
	struct hostent *host;
	struct servent *server;
	struct sockaddr_in name;
	CONNECTION *conn;

	/* First let the user know what we're doing */

	if (verbose) {
		msgl("Connecting to ", hostname, "...", NULL);
	}

	/* First look up the named host */

	if ((host = gethostbyname(hostname)) == NULL) {
		net_error = h_errno;
		emsgl("Error looking up ", hostname,
		      ": ", strerror(net_error), NULL);
		return(NULL);
	}

	/* Copy the host's address and family into the host */

	memcpy(&(name.sin_addr), host->h_addr, host->h_length);
	name.sin_family = host->h_addrtype;

	/* Now look up the required service */

	if ((server = getservbyname(service, protocol)) != NULL ) {
		name.sin_port = server->s_port;
	} else {
		name.sin_port = htons(port);
	}

	/* Now attempt to create and connect a socket to the port */

	if ((sockdesc = socket(name.sin_family, SOCK_STREAM, 0)) < 0
	    || connect(sockdesc, (struct sockaddr *)
		       &name, sizeof(name)) < 0) {
		/* Error creating or connecting the socket */

		net_error = errno;
		emsgl("Error connecting to ", hostname,
		      ": ", strerror(net_error), NULL);
		return(NULL);
	}

	/* We've succeeded; allocate and fill conn */

	conn = (CONNECTION *) xmalloc(sizeof(CONNECTION));
	conn->rsock = fdopen(sockdesc, "r");
	conn->wsock = fdopen(sockdesc, "w");
	conn->server = xstrdup(host->h_name);

	/* Confirm and return a successful connection */

	if (verbose) {
		cmsg(" Done");
	}
	net_error = 0;
	return(conn);
}
/****************************************************************************/
static int netclose(conn)
CONNECTION *conn;
{
	/* Close a network connection gracefully */

	int rstat, wstat;

	/* First close the socket */

	rstat = fclose(conn->rsock);
	wstat = fclose(conn->wsock);

	/* And then free the conn structure */

	free(conn->server);
	free(conn);

	/* Now return status */

	return((rstat) ? rstat : wstat);
}
/****************************************************************************/
static char *netread(conn, response)
CONNECTION *conn;
char *response;
{
	/*
	 * Read a single line from a server and check that
	 * it matches response if supplied.  Response is a
	 * string which is matched up to its own length.
	 */
	   
	static char *buf = NULL;
	char *cr, *spc;

	/* Free any old return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* Read the line and check status */

        if ((buf = tget_line(conn->rsock, NET_TIMEOUT, FALSE)) == NULL) {
		/* Error reading the line; handle the error */

		emsgl("Error reading ", conn->server,
		      ": ", strerror(errno), NULL);
		netclose(conn);
		return(NULL);
	}

	/* Convert any trailing CRLF to a newline */

	if ((cr = strchr(buf, '\r')) != NULL && *(cr + 1) == '\n') {
		/* We can assume that this is at end-of-line */

		(void) strcpy(cr, "");
	}

	/* Now check the response if required */

	if (response != NULL && strncmp(buf, response, strlen(response))) {
		/* Invalid response; handle the error */

		spc = strchr(buf, ' ');
		emsgl(conn->server, ": ", (spc != NULL)
		      ? spc + 1 : buf, NULL);
		netclose(conn);
		return(NULL);
	}

	/* And return the buffer */

	return(buf);
}
/****************************************************************************/
/*VARARGS 1*/
static int netsend(conn, va_alist)
CONNECTION *conn;
va_dcl
{
	/* Send a line of text to a server */

	char *arg, *line;
	va_list arglist;

	/* Initialise the varargs handling */

	va_start(arglist);

	/* Initialise the line's text */

	line = xmalloc(1);
	*line = '\0';

	/* Now loop through adding the arguments to the line */

	while ((arg = va_arg(arglist, char *)) != NULL) {
		line = xrealloc(line, strlen(line) + strlen(arg) + 1);
		(void) strcat(line, arg);
	}

	/* Send the line and check for an error */

	if (fprintf(conn->wsock, "%s\r\n", line) == EOF
	    || fflush(conn->wsock) == EOF) {
		/* Error trying to send the line */

		emsgl("Error writing ", conn->server,
		      ": ", strerror(errno), NULL);
		netclose(conn);
		free(line);
		return(FALSE);
	}

	/* Clean up and return success */

	free(line);
	va_end(arglist);
	return(TRUE);
}
/****************************************************************************/
#ifdef READ_VIA_POP3
static char *netslurp(conn, hdrs)
CONNECTION *conn;
int hdrs;
{
	/* Slurp in one logical line from a multiline response */

	char *buf, *newbuf, *b, *n;

	/* Read the line and check status */

        if ((buf = tget_line(conn->rsock, NET_TIMEOUT, hdrs)) == NULL) {
		/* Error reading the line; handle the error */

		emsgl("Error reading ", conn->server,
		      ": ", strerror(net_error), NULL);
		netclose(conn);
		return(NULL);
	}

	/* Check for a termination line */

	if (!strcmp(buf, ".\r\n") || !strcmp(buf, ".\n")) {
		/* Clean up and return null */

		free(buf);
		return(NULL);
	}

	/* Allocate the space for the line */

	n = newbuf = xmalloc(strlen(buf) + 2);

	/* Quote any leading "From " in the line */

	if (!strncmp(buf, MFROM, strlen(MFROM))) {
		/* Prepend a delimiter prefix to the line */

		(void) strcpy(newbuf, DELIM_PFX);
		n = newbuf + strlen(newbuf);
	}

	/* Copy the line, converting CRLF to '\n' */

	for (b = buf; *b != '\0'; b++) {
		/* Copy the character if required */

		if (*b != '\r' || *(b + 1) != '\n') {
			*n++ = *b;
		}
	}
	*n = '\0';
	free(buf);
		
	/* Handle quoted dots in the text we read */

	if (*newbuf == '.' && strcmp(newbuf, ".\n")) {
		/* Quoted dot, strip the first dot */

		buf = xstrdup(newbuf + 1);
		free(newbuf);
		newbuf = buf;
	}

	/* Return the canonicalised buffer */

	return(newbuf);
}
#endif /* READ_VIA_POP3 */
/****************************************************************************/
#ifdef MTA_IS_SMTP
static int netfile(conn, fp)
CONNECTION *conn;
FILE *fp;
{
	/*
	 * Send the specified file to the network socket opened by
	 * net_fp.  Handles '.' transparency and EOL conversion.
	 */

	char buf[BUFSIZ];
	char *newline;

	/* Loop over each line of the file */

	while (fgets(buf, BUFSIZ, fp) != NULL) {
		/* Remove any newline in the line */

		if ((newline = strchr(buf, '\n')) != NULL) {
			*newline = '\0';
		}

		/* Now output the line, quoting leading dots */

		if (!netsend(conn, (*buf == '.') ? "." : "", buf, NULL)) {
			return(FALSE);
		}
	}

	/* The file was successfully transmitted */

	return(TRUE);
}
#endif /* MTA_IS_SMTP */
/****************************************************************************/
char *tget_line(fp, delay, hdrs)
FILE *fp;
int delay, hdrs;
{
	/* Get a line from fp using func, with a timeout of delay seconds */

	char *line;
	ATIMER tbuf;

	/* Save any currently-active alarm timer */

	(void) save_atimer(&tbuf);

	/* Set up a jump position to handle a timeout */

        if (setjmp(timeout_jump)) {
		/* Read timed out; clean up and fail */

		net_error = ETIMEDOUT;
		(void) restore_atimer(&tbuf);
		return(NULL);
	}

	/* Set up the alarm timer for the read */

	(void) signal(SIGALRM, time_out);
	(void) alarm(delay);

	/* Read the line and save the error number */

	line = get_line(fp, hdrs);
	net_error = (line == NULL) ? errno : 0;

	/* Now reset the signal handling and return */

	(void) restore_atimer(&tbuf);
        return(line);
}
/****************************************************************************/
/*ARGSUSED*/
static RETSIGTYPE time_out(status)
int status;
{
	/* Handle a timeout during a read */

	longjmp(timeout_jump, 1);
}
/****************************************************************************/
