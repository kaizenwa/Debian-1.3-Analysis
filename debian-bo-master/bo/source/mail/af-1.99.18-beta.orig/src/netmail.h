/* Netmail.h - declarations for af's network mail handling.
   Copyright (C) 1995, 1996, 1997 Malc Arnold.

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


/****************************************************************************/
/* RCS info. */

#ifndef lint
#define NETMAILID	"$Id: netmail.h,v 1.4 1997/03/31 18:32:19 malc Exp $"
#endif /* ! lint */

/****************************************************************************/
/* The services, protocols and port numbers we'll use */

#define SMTP_SERVICE	"smtp"
#define SMTP_PROTOCOL	"tcp"
#define SMTP_PORT	25
#define POP3_SERVICE	"pop3"
#define POP3_PROTOCOL	"tcp"
#define POP3_PORT	110

/****************************************************************************/
/* Definitions for SMTP return codes */

#define SMTP_BANNER	"220"
#define SMTP_OK		"250"
#define SMTP_SEND	"354"
#define SMTP_QUIT	"221"

/****************************************************************************/
/* Definitions for POP3 return codes */

#define POP3_OK		"+OK"
#define POP3_ERR	"-ERR"

/****************************************************************************/
/* The structure that holds details of a network connection */

typedef struct {
	FILE *rsock, *wsock;	/* File pointers to socket */
	char *server;		/* The FQDN of the server */
} CONNECTION;

/****************************************************************************/
/* The structure that holds details of POP3 folder connections */

typedef struct popfolder {
	char *folder;		/* The folder we are caching */
	char *server, *user;	/* The server's FQDN and username */
	char *pw, mask;		/* The user's password and it's mask */
	CONNECTION *conn;	/* The connection to the server */
	long size;		/* The size of the POP3 mailbox */
	int offset, msg;	/* The read offset and last msg */
	int synced, closed;	/* Connection in sync or closed? */
	int error;		/* Error encountered in connection */
	struct popfolder *next;	/* The next folder in the list */
} POPFOLDER;

/****************************************************************************/
