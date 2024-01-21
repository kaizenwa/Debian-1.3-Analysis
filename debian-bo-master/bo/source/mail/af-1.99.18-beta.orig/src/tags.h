/* Tags.h - Declarations for tag handling in af.
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

#define TAGID	"$Id: tags.h,v 1.5 1996/03/17 01:10:47 malc Exp $"

/****************************************************************************/
/* The structure used to hold parsed tag lists */

typedef int TAG_LIST;

/****************************************************************************/
/* System tag characters used in af */

#define TAG_DELETED	'D'
#define TAG_ERROR	'E'
#define TAG_MIME	'M'
#define TAG_NEW		'N'
#define TAG_UNREAD	'O'
#define TAG_FORWARDED	'F'
#define TAG_REPLIED	'R'
#define TAG_SAVED	'S'
#define TAG_PRINTED	'P'

/****************************************************************************/
/* The maximum number of tags of each type we can have */

#define MAX_SYS_TAGS	7
#define MAX_USER_TAGS	27

/****************************************************************************/
/* The default user tag and a macro to test if it is used */

#define DEFAULT_TAG	"+"
#define DEFTAG_USED(t)	(*(t) != TL_IGNORE)

/****************************************************************************/
/* The values that may be set on elements of tag lists */

#define TL_SET		1
#define TL_UNSET	-1
#define TL_IGNORE	0

/****************************************************************************/
/* Macros to convert between tag character and position in list */

#define TAGNO(t)	(((t) == '+') ? 0 : (islower(t)) ? (t) - 'a' + 1 : -1)
#define TAGCHAR(t)	(((t) == 0) ? '+' : 'a' + (t) - 1)

/****************************************************************************/
/* The structure used to hold parsed tag expressions */

typedef struct tag_expr {
	char tag;				/* The tag referred to */
	char operator;				/* Operation on tag */
	struct tag_expr *left, *right;		/* Left and right subtrees */
} TAG_EXPR;

/****************************************************************************/
/* The structure used to hold References while building a thread */

typedef struct reference {
	char *ref;				/* The Reference */
	struct reference *next;			/* Next item in list */
} REFERENCE;

/****************************************************************************/
/* Valid tag expression operators in order of increasing precedence */

#define EXPR_OPS	"|^&!("

/* And the valid tags for tag expressions */

#define EXPR_TAGS	"DEFMNOPRS+abcdefghijklmnopqrstuvwxyz"

/****************************************************************************/
/* The open and close paren characters for tag expressions */

#define EXPR_LPAREN	'('
#define EXPR_RPAREN	')'

/****************************************************************************/
/* The operators that may be applied in tag expressions */

#define TE_TAG		5
#define TE_PAREN	4
#define TE_NOT		3
#define TE_AND		2
#define TE_XOR		1
#define TE_OR		0

/****************************************************************************/
/* Macros to classify and check nodes in a tag expression */

#define ISOP(op)	((op) <= TE_NOT)
#define ISATOM(op)	((op) >= TE_PAREN)

#define FOLLOWOP(op)	((op) >= TE_NOT)
#define FOLLOWATOM(op)	((op) <= TE_AND)
#define BEFOREEND(op)	((op) >= TE_PAREN)

/****************************************************************************/
/* Macro to determine if a node can be pushed onto the tree */

#define EXPR_PUSH(t,n)	((n) != TE_PAREN && (t) > (n))

/****************************************************************************/
