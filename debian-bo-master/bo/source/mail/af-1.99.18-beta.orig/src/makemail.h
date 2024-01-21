/* Makemail.h - Declarations for generating outgoing headers.
   Copyright (C) 1994, 1995, 1996 Malc Arnold.

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
static char *MakemailId = "$Id: makemail.h,v 1.6 1997/03/31 18:32:19 malc Exp $";
#endif /* lint */

/****************************************************************************/
/* Functions used to check headers */

extern char *alias(), *contype(), *encoding(), *references();

/****************************************************************************/
/* The structure for storing the default headers */

typedef struct {
	char *hdr_name;		/* The name of the header */
	unsigned edit : 1;	/* Can we edit this header? */
	unsigned show : 1;	/* Show this header in edits? */
	unsigned reqd : 1;	/* Must we have this header? */
	char *(*chk_func)();	/* Function to check header */
} DEF_HDR;

/****************************************************************************/
/* Defaults for outgoing headers */

#define DEFVERSION	"1.0"
#define DEFCONTENT	"text/plain; charset=us-ascii"
#define DEFENCODING	"7bit"

/****************************************************************************/
/* The list of default outgoing headers */

static DEF_HDR def_headers[] = {
	{ RETURN_PATH, FALSE, FALSE, FALSE, NULL },
	{ RECEIVED, FALSE, FALSE, FALSE, NULL },
	{ MESSAGE_ID, FALSE, FALSE, FALSE, NULL },
	{ DATE, FALSE, FALSE, TRUE, NULL },
	{ FROM, TRUE, TRUE, TRUE, alias },
	{ SENDER, FALSE, FALSE, FALSE, alias },
	{ ORG, TRUE, TRUE, FALSE, NULL },
	{ REPLY_TO, TRUE, TRUE, FALSE, alias },
	{ SUBJECT, TRUE, TRUE, FALSE, NULL },
	{ TO, TRUE, TRUE, FALSE, alias },
	{ CC, TRUE, TRUE, FALSE, alias },
	{ BCC, TRUE, TRUE, FALSE, alias },
	{ MIME_VERSION, FALSE, FALSE, FALSE, NULL },
	{ CONTENT_TYPE, TRUE, TRUE, FALSE, contype },
	{ C_T_ENCODING, TRUE, TRUE, FALSE, encoding },
	{ CONTENT_DESC, TRUE, TRUE, FALSE, NULL },
	{ IN_REPLY_TO, TRUE, TRUE, FALSE, NULL },
	{ REFERENCES, TRUE, FALSE, FALSE, references },
	{ CONTENT_LENGTH, FALSE, FALSE, FALSE, NULL },
	{ STATUS, FALSE, FALSE, FALSE, NULL },
	{ X_STATUS, FALSE, FALSE, FALSE, NULL },
	{ AFSTATUS, FALSE, FALSE, FALSE, NULL },
	{ AFTAGS, FALSE, FALSE, FALSE, NULL },
	{ MAILER, FALSE, FALSE, FALSE, NULL },
	{ NULL, FALSE, FALSE, FALSE, NULL }
};

/****************************************************************************/
/* The list of default resent headers */

static DEF_HDR def_resent[] = {
	{ RESENT_ID, FALSE, FALSE, FALSE, NULL },
	{ RESENT_DATE, FALSE, FALSE, FALSE, NULL },
	{ RESENT_FROM, TRUE, TRUE, TRUE, alias },
	{ RESENT_SENDER, FALSE, FALSE, FALSE, alias },
	{ RESENT_ORG, TRUE, TRUE, FALSE, NULL },
	{ RESENT_REPLY_TO, TRUE, TRUE, FALSE, alias },
	{ RESENT_TO, TRUE, TRUE, TRUE, alias },
	{ RESENT_CC, TRUE, TRUE, FALSE, alias },
	{ RESENT_BCC, TRUE, TRUE, FALSE, alias },
	{ NULL, FALSE, FALSE, FALSE, NULL }
};

/****************************************************************************/
