/* Parsemail.h - Declarations for mail parsing.
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


/****************************************************************************/
/* RCS info */

#ifndef lint
static char *ParseId = "$Id: parsemail.h,v 1.10 1997/03/31 18:32:19 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* The structure used for header handling */

typedef struct hdr_ptable {
	char *h_name;
	MESSAGE *(*h_func)();
} HDR_PTABLE;

/****************************************************************************/
/* Declarations of the header-handling functions */

static MESSAGE *msg_date(), *msg_from(), *msg_subject();
static MESSAGE *msg_reply(), *msg_to(), *msg_cc();
static MESSAGE *msg_version(), *msg_type(), *msg_encoding();
static MESSAGE *msg_id(), *msg_inreply(), *msg_refs();
static MESSAGE *msg_status(), *msg_tags();

/* We may be handling Content-Length headers */

#ifdef MTA_CONTENT_LENGTH
static MESSAGE *msg_length();
#endif /* MTA_CONTENT_LENGTH */

/****************************************************************************/
/* The headers where references are found */

#define REFS_FROM_MSG_ID	0
#define REFS_FROM_IN_REPLY_TO	1
#define REFS_FROM_REFERENCES	2

/****************************************************************************/
/*
 * This table defines the function to call to handle any given
 * header received.  Headers that do not need handling do not
 * have an entry in the table.
 */

static HDR_PTABLE h_ptab[] = {
	{ DATE,			msg_date },
	{ FROM,			msg_from },
	{ SUBJECT,		msg_subject },
	{ REPLY_TO,		msg_reply },
	{ TO,			msg_to },
	{ CC,			msg_cc },
	{ MIME_VERSION,		msg_version },
	{ CONTENT_TYPE,		msg_type },
	{ C_T_ENCODING,		msg_encoding },
	{ MESSAGE_ID,		msg_id },
	{ IN_REPLY_TO,		msg_inreply },
	{ REFERENCES,		msg_refs },
#ifdef MTA_CONTENT_LENGTH
	{ CONTENT_LENGTH,	msg_length },
#endif /* MTA_CONTENT_LENGTH */
	{ STATUS,		msg_status },
	{ X_STATUS,		msg_status },
	{ AFSTATUS,		msg_status },
	{ AFTAGS,		msg_tags },
	{ NULL,			NULL }
};

/****************************************************************************/
