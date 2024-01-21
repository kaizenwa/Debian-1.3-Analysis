/* Mailsort.h - Declarations for af's sorting routines.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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
static char *SortId = "$Id: mailsort.h,v 1.5 1996/03/17 01:10:47 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* The functions used to compare messages when sorting */

static int ms_address(), ms_date(), ms_lines(), ms_mailbox();
static int ms_sender(), ms_status(), ms_subject(), ms_tags();

/****************************************************************************/
/* The possible values to return from comparison functions */

#define SC_LESS_THAN	-1
#define SC_GREATER_THAN	1

/****************************************************************************/
/* The structure used to hold the messages to be sorted */

typedef struct {
	MESSAGE *msg;			/* Message to be sorted */
	int index;			/* Ordering of messages */
} SORT_MSG;

/****************************************************************************/
/* The structure used to hold details of the messages to be sorted */

typedef struct {
	MESSAGE *prev, *next;		/* Pointers into buffer */
	int point, mark, first;		/* Is message one of these? */
} SORT_INFO;

/****************************************************************************/
/* The structure used to store details about sort types */

typedef struct {
	char *name;			/* Name of the sort type */
	char reverse;			/* Is the comparison reversed? */
	int  (*func)();			/* Message comparison function */
} SORT;

/****************************************************************************/
/* The list of available sort types and their functions */

static SORT sorts[] = {
	{ "address",		FALSE,	ms_address },
	{ "date",		FALSE,	ms_date },
	{ "lines",		FALSE,	ms_lines },
	{ "mailbox",		FALSE,	ms_mailbox },
	{ "sender",		FALSE,	ms_sender },
	{ "status",		FALSE,	ms_status },
	{ "subject",		FALSE,	ms_subject },
	{ "tags",		FALSE,	ms_tags },
	{ "reverse-address",	TRUE,	ms_address },
	{ "reverse-date",	TRUE,	ms_date },
	{ "reverse-lines",	TRUE,	ms_lines },
	{ "reverse-mailbox",	TRUE,	ms_mailbox },
	{ "reverse-sender",	TRUE,	ms_sender },
	{ "reverse-status",	TRUE,	ms_status },
	{ "reverse-subject",	TRUE,	ms_subject },
	{ "reverse-tags",	TRUE,	ms_tags },
	{ NULL,			FALSE,	NULL }
};

/****************************************************************************/
