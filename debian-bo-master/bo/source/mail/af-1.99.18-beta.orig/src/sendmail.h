/* Sendmail.h - Declarations for af's mail sending routines.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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

#define SENDMAILID	"$Id: sendmail.h,v 1.11 1996/03/17 01:10:47 malc Exp $"

/****************************************************************************/
/* The structure used when editing headers */

typedef struct header {
	char *hdr_name, *hdr_text;	/* Header name and text */
	unsigned edit : 1;		/* Can we edit this header? */
	unsigned show : 1;		/* Show this header in edits? */
	unsigned reqd : 1;		/* Is this header required? */
	unsigned found : 1;		/* Did we find this header? */
	char *(*chk_func)();		/* Function to check header */
	struct header *next;		/* Pointer to next header */
} HEADER;

/****************************************************************************/
/* The possible types of outgoing mail */

#define	T_SEND		0
#define T_REPLY		1
#define T_FORWARD	2
#define T_BOUNCE	3
#define T_SILENT	4

/****************************************************************************/
/* The width of a header or body line */

#define FOLD_WIDTH	72
#define LINESIZE	80

/****************************************************************************/
/* The maximum length of a message ID excluding host name */

#define BASEIDLEN	23

/****************************************************************************/
/* The base text we use for In-Reply-To: headers */

#define INREPLY_TEXT	" mail of "

/****************************************************************************/
/* The string used to quote unwanted 'From ' lines */

#define FROM_QUOTE	">"

/****************************************************************************/
/* A few macros for my sanity's sake. */

/* Is character c a newline character */

#define IS_NEWLINE(c)	((c) == '\n' || (c) == '\r')

/* Is character c RFC 822 linear-white-space? */

#define IS_LWSP(c)	((c) == ' ' || (c) == '\t')

/****************************************************************************/
