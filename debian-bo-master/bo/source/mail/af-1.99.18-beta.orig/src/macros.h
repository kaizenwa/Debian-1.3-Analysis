/* Macro.h - Declarations for keyboard macro handling in af.
   Copyright (C) 1995, 1996 Malc Arnold.

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

#define MACROID		"$Id: macros.h,v 1.2 1996/03/17 01:10:47 malc Exp $"

/****************************************************************************/
/* The structure to hold the details of a macro */

typedef struct macro {
	char *name;			/* The name of the macro */
	KEYSEQ *keys;			/* The keys making up the macro */
	struct macro *next;		/* The next macro in the list */
} MACRO;

/****************************************************************************/
/* The structure to hold details of macros being called */

typedef struct exec_status {
	KEYSEQ *keys;			/* The keys to run */
	int repeat;			/* The repeat count */
	int pos;			/* Position in macro */
} EXEC_STATUS;

/****************************************************************************/
/* The keys which have meaning in kbd-macro-query */

#define QUERY_ABORT	CTRL('[')
#define QUERY_CONTINUE	' '
#define QUERY_SKIP	CTRL('M')
#define QUERY_REDRAW	CTRL('L')

/****************************************************************************/
