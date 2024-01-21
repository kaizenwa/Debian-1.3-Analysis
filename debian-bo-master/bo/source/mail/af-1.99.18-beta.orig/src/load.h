/* Load.h - Function structure and value definitions for af
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

#define LOADID		"$Id: load.h,v 1.2 1996/03/17 01:10:47 malc Exp $"

/****************************************************************************/
/* The maximum nesting depth of load files */

#define LOAD_DEPTH	8

/****************************************************************************/
/* The special characters within afl files */

#define AFL_SPECIALS	" \t\n\r;'\"()?"

/****************************************************************************/
/* The delimiters of a token */

#define LS_COMMENT	';'
#define LE_COMMENT	'\n'
#define LS_LIST		'('
#define LE_LIST		')'
#define LS_STRING	'"'
#define LE_STRING	'"'
#define LS_QUOTE	'\''
#define LS_CHAR		'?'
#define LS_ESCAPE	'\\'

/****************************************************************************/
/* The prefix characters for control and metacharacters */

#define LS_CTRL		'C'
#define LS_META		'M'
#define LC_PREFIX	'-'
#define LS_CARET	'^'

/****************************************************************************/
/* The possible error values for loading files */

#define LERR_NONE	0	/* No error */
#define LERR_LIST	1	/* EOF while reading list */
#define LERR_STRING	2	/* EOF while reading string */
#define LERR_CHAR	3	/* EOF while reading character */
#define LERR_SYMBOL	4	/* Invalid character in symbol name */
#define LERR_ESC	5	/* Invalid escape character syntax */
#define LERR_CTRL	6	/* Invalid control character */

/* The error messages related to each error */

#define LTEXT_NONE	"no error"
#define LTEXT_LIST	"Reached end-of-file while reading list"
#define LTEXT_STRING	"Reached end-of-file while reading string"
#define LTEXT_CHAR	"Reached end-of-file while reading character"
#define LTEXT_SYMBOL	"Invalid character in symbol name"
#define LTEXT_ESC	"Invalid escape character syntax"
#define LTEXT_CTRL	"Invalid control character"

/* And the basic error message to print */

#define ERROR_MSG	"Reach end-of-file during parsing"

/****************************************************************************/
