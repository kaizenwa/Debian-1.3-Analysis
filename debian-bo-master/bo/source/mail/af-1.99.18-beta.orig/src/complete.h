/* Complete.h - Definitions for completion within af.
   Copyright (C) 1990, 1991, 1992, 1996 Malc Arnold.

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

#define COMPLETEID	"$Id: complete.h,v 1.4 1996/03/17 01:10:47 malc Exp $"

/****************************************************************************/
/* The structure used to return details about a completion atempt */

typedef struct {
	char *tail;		/* The text which completion generated */
	char complete;		/* Is the returned entry complete? */
} COMPLETE;

/****************************************************************************/
/* The list into which entries to be checked for completion are stored */

typedef struct clist {
	char *entry;		/* The entry to be checked */
	int case_dep;		/* Is the entry case-dependent? */
	struct clist *next;	/* The next entry in the list */
} CLIST;

/****************************************************************************/
/* The types of completion available */

#define C_NONE		0
#define C_STRICT	1
#define C_CAUTIOUS	2
#define C_PERMISSIVE	3

/****************************************************************************/
