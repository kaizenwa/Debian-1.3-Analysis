/* Alias.h - Declarations for alias list handling in af.
   Copyright (C) 1991, 1992, 1996 Malc Arnold.

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
static char *AliasId = "$Id: alias.h,v 1.5 1996/03/17 01:10:47 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* The type for an alias list */

typedef struct alias {
	char *a_alias;			/* The name of the alias */
	GROUP *a_group;			/* The group it expands to */
	unsigned a_status;		/* Status during processing */
	struct alias *next;		/* The next alias in the list */
} ALIAS;

/****************************************************************************/
/* The actual definition of the list */

static ALIAS *aliases;

/****************************************************************************/
/* The possible statuses associated with an alias */

#define A_RAW		0
#define A_EXPANDING	1
#define A_EXPANDED	2

/****************************************************************************/
