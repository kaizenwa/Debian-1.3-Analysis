/* Af.h - Definitions for af.
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


/****************************************************************************/
/* RCS info */

#define HEADERID	"$Id: af.h,v 1.56 1997/04/20 10:32:45 malc Exp $"

/****************************************************************************/
/* Include the configuration header file */

#include "config.h"

/* The ubiquitous boolean definitions */

#ifndef TRUE
#define TRUE		1
#define FALSE		0
#endif

/* The environment variables used by af */

#define HOME		"HOME"
#define MAIL		"MAIL"
#define SHELL		"SHELL"
#define EDITOR		"EDITOR"
#define VISUAL		"VISUAL"
#define PAGER		"PAGER"
#define FOLDER		"FOLDER"
#define SAVEDIR		"SAVEDIR"
#define NAME		"NAME"
#define ORGANIZATION	"ORGANIZATION"
#define AFLOADPATH	"AFLOADPATH"
#define LINES		"LINES"
#define COLUMNS		"COLUMNS"

/* The header lines that we deal with */

#define MFROM		"From "
#define UUCPFROM	">From "
#define RETURN_PATH	"Return-Path:"
#define RECEIVED	"Received:"
#define DATE		"Date:"
#define FROM		"From:"
#define ORG		"Organization:"
#define SUBJECT		"Subject:"
#define SENDER		"Sender:"
#define REPLY_TO	"Reply-To:"
#define TO		"To:"
#define BCC		"Bcc:"
#define CC		"Cc:"
#define MIME_VERSION	"MIME-Version:"
#define CONTENT_DESC	"Content-Description:"
#define CONTENT_DISP	"Content-Disposition:"
#define CONTENT_TYPE	"Content-Type:"
#define C_T_ENCODING	"Content-Transfer-Encoding:"
#define CONTENT_ID	"Content-ID:"
#define MESSAGE_ID	"Message-ID:"
#define IN_REPLY_TO	"In-Reply-To:"
#define REFERENCES	"References:"
#define CONTENT_LENGTH	"Content-Length:"
#define STATUS		"Status:"
#define X_STATUS	"X-Status:"
#define AFSTATUS	"X-Af-Status:"
#define AFTAGS		"X-Af-Tags:"
#define MAILER		"X-Mailer:"

/* The Resent- headers (used when bouncing) */

#define RESENT		"Resent-"
#define RESENT_DATE	"Resent-Date:"
#define RESENT_FROM	"Resent-From:"
#define RESENT_SENDER	"Resent-Sender:"
#define RESENT_ORG	"Resent-Organization:"
#define RESENT_REPLY_TO	"Resent-Reply-To:"
#define RESENT_TO	"Resent-To:"
#define RESENT_CC	"Resent-Cc:"
#define RESENT_BCC	"Resent-Bcc:"
#define RESENT_ID	"Resent-Message-ID:"

/* Defaults for incoming header fields */

#define ERRUSER		"Unknown sender"
#define DEFSUBJECT	"No subject"

/* The prefix used to quote delimiters in a message's body */

#define DELIM_PFX	">"

/* The prefix found before reply subjects */

#define REPLY_PFX	"Re:"

/* The number of references stored by af */

#define NO_REFERENCES	5

/* Values found in the Status: or X-Status: header */

#define ST_NEW		'N'
#define ST_OLD		'O'
#define ST_READ		'R'
#define ST_UNREAD	'P'
#define ST_SAVED	'S'
#define ST_PRINTED	'p'
#define ST_REPLIED	'r'
#define ST_FORWARDED	'f'

/* The flags used to display buffer status */

#define BS_VISIBLE	'.'
#define BS_CHANGED	'*'
#define BS_STATUS	'+'
#define BS_READONLY	'%'

/* The buffer we always create when starting af */

#define SCRATCHBUF	"*scratch*"

/* Characters valid as options to af */

#define OPTS		"EFHS:ef:l:ns:u:vwz"

/* The default typeout redirection file */

#define TYPEOUT_FILE	"af-typeout"

/* The number of keystrokes stored in the lossage */

#define LOSSAGE_SIZE	100

/* Miscellany */

#define DIRSEP		'/'
#define MAXUSTRLEN	32

/****************************************************************************/
/* The structure used to hold a date and time zone */

typedef struct datezone {
	long d_date;				/* Seconds from epoch */
	long d_zone;				/* Zone offset in seconds */
} DATEZONE;

/****************************************************************************/
/* The structure for the message text */

typedef struct text_list {
	char *line;				/* The text of the line */
	struct text_list *next;			/* Next line in the list */
} MSG_TEXT;

/****************************************************************************/
/* The structure used to hold the doubly linked list of messages */

typedef struct message {
	char *from, *addr, *subject;		/* Name address and subject */
	char *reply, *group, *cc;		/* Reply addresses */
	char *contype, *encoding;		/* MIME information */
	DATEZONE *date;				/* Date message sent */
	char *refs[NO_REFERENCES];		/* References */
	MSG_TEXT *text;				/* Text of message */
	unsigned pos;				/* Position in mailbox */
	char *id;				/* UIDL or IMAP ID */
#ifdef MTA_CONTENT_LENGTH
	unsigned length;			/* The length of the body */
#endif /* MTA_CONTENT_LENGTH */
	char *sys_tags;				/* Message system tags */
	char *user_tags;			/* Message user tags */
	unsigned visible : 1;			/* Message is visible */
	unsigned deleted : 1;			/* Message to be deleted */
	unsigned nontext : 1, bad : 1;		/* Message status flags */
	unsigned new : 1, read : 1;
	unsigned replied : 1, forwarded : 1;
	unsigned saved : 1, printed : 1;
	struct message *prev, *next;		/* List pointers */
} MESSAGE;

/****************************************************************************/
/* The structure used to represent a buffer */

typedef struct mailbuf {
	char *name, *file;			/* Buffer and file names */
	char *pending;				/* Pending file name */
	MESSAGE *messages;			/* Messages in buffer */
	MESSAGE *point, *mark;			/* Saved point and mark */
	long eofpos, peofpos;			/* End of file positions */
	unsigned no_msgs;			/* No of messages in buffer */
	unsigned modes;				/* Modes active in buffer */
	unsigned mod : 1, st_mod : 1;		/* Modification flags */
	struct mailbuf *prev, *next;		/* List pointers */
} MAILBUF;

/****************************************************************************/
/* The structure used to store a list of windows */

typedef struct window {
	MAILBUF *buf, *other;			/* Active and other buffer */
	int top, bottom;			/* Top and bottom line */
	int cols;				/* Width of window */
	MESSAGE *first, *last;			/* Range of visible lines */
	MESSAGE *point, *mark;			/* Current and marked line */
	struct window *prev, *next;		/* List pointers */
} WINDOW;

/****************************************************************************/
/* Finally, the structure to hold the user options */

typedef struct {
	unsigned exists : 1;			/* Report existence (-e) */
	unsigned zero : 1;			/* Only if exists (-z) */
	unsigned edit : 1;			/* Edit stdin mail (-E) */
	unsigned hdrs : 1;			/* Edit headers (-H) */
	unsigned nostartup : 1;			/* Don't load .afrc (-n) */
	unsigned windows : 1;			/* Multiple windows (-w) */
	unsigned pending : 1;			/* Handle pending folder */
	unsigned version : 1;			/* Just print version */
	char **folders;				/* Folders (-u or -f) */
	char *loadfile;				/* File to load (-l) */
	char *script;				/* Script to run (-S) */
	char *subject;				/* Outgoing subject (-s) */
	char *users;				/* Users to mail to */
} USEROPTS;

/****************************************************************************/
