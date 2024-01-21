/* Address.h - Declarations for af address handling.
   Copyright (C) 1992, 1996 Malc Arnold.

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

#define	ADDRESSID	"$Id: address.h,v 1.3 1996/03/17 01:10:47 malc Exp $"

/****************************************************************************/
/* The parse tree into which an address list will get broken down */

typedef struct address {
	ATOM *name;			/* The address's real-name */
	ATOM *route, *local;		/* The route and local-part */
	ATOM *proute, *domain;		/* The percent-route and domain */
	struct address *next;		/* The next address in the list */
} ADDRESS;

typedef struct group {
	ATOM *name, *comment;		/* The group's name and comment */
	ADDRESS *addresses;		/* The addresses in the group */
	struct group *next;		/* The next group in the list */
} GROUP;

/****************************************************************************/
/* The type returned by a parsing function */

typedef ATOM *(*PARSEFUNC)();

/****************************************************************************/
/* The states we may be in while parsing an address */

#define ST_INITIAL	0
#define ST_GROUP	1
#define ST_BRACKET	2
#define ST_ROUTE	3
#define ST_LOCAL	4
#define ST_PROUTE	5
#define ST_DOMAIN	6

/****************************************************************************/
