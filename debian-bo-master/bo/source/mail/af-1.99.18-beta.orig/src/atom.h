/* Atom.h - Declarations for af's tokenisation routines.
   Copyright (C) 1992, 1993, 1994, 1996, 1997 Malc Arnold.

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

#define ATOMID	"$Id: atom.h,v 1.8 1997/03/31 18:32:19 malc Exp $"

/****************************************************************************/
/* The type of an atom in the list formed by tokenising */

typedef struct atom {
	char *text;
	int type;
	struct atom *next;
} ATOM;

/****************************************************************************/
/* The possible types of an atom */

#define AT_ATOM		0
#define AT_QSTRING	1
#define AT_DLITERAL	2
#define AT_DOT		3
#define AT_PERCENT	4
#define AT_AT		5
#define AT_COMMA	6
#define AT_LANGLEB	7
#define AT_RANGLEB	8
#define AT_COLON	9
#define AT_SEMI		10
#define AT_COMMENT	11
#define AT_WS		12
#define AT_SLASH	13
#define AT_QUERY	14
#define AT_EQUALS	15

/* Macro to determine if an atom is white space */

#define IS_WS(a)	((a)->type == AT_WS || (a)->type == AT_COMMENT)

/****************************************************************************/
/* The delimiters of an atom */

#define SC_ATOM		""
#define TC_ATOM		" \t\n\r\"()[].@%,<>;:\\"
#define TC_MATOM	" \t\n\r\"()[]@%,<>;:\\/?="

#define SC_QSTRING	""
#define TC_QSTRING	"\""

#define SC_DLITERAL	""
#define TC_DLITERAL	"]"

#define SC_COMMENT	"("
#define TC_COMMENT	")"

/****************************************************************************/
/* Characters that need escaping in some contexts */

#define EC_QSTRING	"\""
#define EC_COMMENT	"()"

/****************************************************************************/
/* The possible canonicalisation levels when forming text */

#define AC_NONE		0
#define AC_TRIM		1
#define AC_UNCOMMENT	2
#define AC_UNQUOTE	3
#define AC_FULL		4

/****************************************************************************/
/* The possible error values for tokenising or parsing */

#define AERR_NONE	0	/* No error */
#define AERR_NULL	1	/* Address list evaluates to null */
#define AERR_ADDRESS	2	/* Miscellaneous syntax error */
#define AERR_BRACKET	3	/* Invalid bracket address */
#define AERR_ROUTE	4	/* Invalid route */
#define AERR_LOCAL	5	/* Invalid local-part */
#define AERR_DOMAIN	6	/* Invalid domain */
#define AERR_UUCP	7	/* Invalid UUCP address */
#define AERR_QSTRING	8	/* Unterminated quoted string */
#define AERR_DLITERAL	9	/* Unterminated domain literal */
#define AERR_COMMENT	10	/* Unterminated comment */

#define CERR_NULL	11	/* No content type specified */
#define CERR_TYPE	12	/* Invalid type/subtype pair */
#define CERR_PARAM	13	/* Invalid parameter */

#define EERR_NULL	14	/* No encoding specified */
#define EERR_ENCODING	15	/* Miscellaneous syntax error */

#define RERR_TOKEN	16	/* Invalid token */
#define RERR_BRACKET	17	/* No closing bracket */
#define RERR_REFERENCE	18	/* Invalid reference */

/* The base text for error messages */

#define AERR_BASE_TEXT	"Invalid address list: "
#define CERR_BASE_TEXT	"Invalid content type: "
#define EERR_BASE_TEXT	"Invalid encoding: "
#define RERR_BASE_TEXT	"Invalid reference list: "

/* The text for an error at the end of the atom list */

#define END_ERRTEXT	"at end of line"

/* The error messages related to each error */

#define ATEXT_NONE	"no error"
#define ATEXT_NULL	"no addresses specified"
#define ATEXT_ADDRESS	"invalid address"
#define ATEXT_BRACKET	"invalid bracket address"
#define ATEXT_ROUTE	"invalid route"
#define ATEXT_LOCAL	"invalid local-part"
#define ATEXT_DOMAIN	"invalid domain"
#define ATEXT_UUCP	"invalid UUCP address"
#define ATEXT_QSTRING	"unterminated quoted string"
#define ATEXT_DLITERAL	"unterminated domain literal"
#define ATEXT_COMMENT	"unterminated comment"

#define CTEXT_NULL	"no content type specified"
#define CTEXT_TYPE	"Invalid type/subtype pair"
#define CTEXT_PARAM	"invalid parameter"

#define ETEXT_NULL	"no encoding specified"
#define ETEXT_ENCODING	"invalid transfer encoding"

#define RTEXT_TOKEN	"invalid token"
#define RTEXT_BRACKET	"unterminated angle bracket"
#define RTEXT_REFERENCE	"invalid reference"

/****************************************************************************/
