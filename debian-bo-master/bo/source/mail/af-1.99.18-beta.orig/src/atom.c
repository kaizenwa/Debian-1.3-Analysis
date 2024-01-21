/* Atom.c - Code to handle atoms of RFC 822 headers.
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


#include <stdio.h>
#include <ctype.h>
#include "af.h"
#include "atom.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: atom.c,v 1.16 1997/03/31 18:32:19 malc Exp $";
static char *AtomId = ATOMID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc();
extern char *xstrdup(), *vstrcat();
extern void free();

/* Local function declarations */

char *atext();
void afree();
ATOM *add_atom();
static char *get_atom(), *set_char();
static char *set_str(), *set_ws();
static char *avalue(), *escape_chars();
static char *unescape_chars();
static ATOM *atomise();

/****************************************************************************/
/* The error flag and text for tokenising and parsing */

int a_errno = AERR_NONE;
char *a_errtext = NULL;

/****************************************************************************/
ATOM *tokenise(text)
char *text;
{
	/* Break down some text into its component parts */

	return(atomise(text, FALSE));
}
/****************************************************************************/
ATOM *ctokenise(text)
char *text;
{
	/* Break down a content type into its component parts */

	return(atomise(text, TRUE));
}
/****************************************************************************/
static ATOM *atomise(text, mime)
char *text;
int mime;
{
	/* Break down some text into its component parts */

	char *start, *end;
	int type, tmp;

	/* The atom list we're building */

	ATOM *alist = NULL;

	/* Haven't had an error yet */

	a_errno = AERR_NONE;
	if (a_errtext != NULL) {
		free(a_errtext);
		a_errtext = NULL;
	}

	/* Initialise the pointers */

	if ((start = end = text) == NULL) {
		/* No text to tokenise */

		return(NULL);
	}

	/* Loop until all atoms are exhausted */

	while (*start != '\0') {
		/* Get the next atomic item from the text */

		if ((end = get_atom(start, mime, &type)) == NULL) {
			afree(alist);
			return(NULL);
		}

		/* Store the character after the atom */

		tmp = *end;
		*end = '\0';

		/* Add the atom to the list */

		alist = add_atom(alist, start, type);

		/* Restore the original character and advance start */

		*end = tmp;
		start = end;
	}

	return(alist);
}
/****************************************************************************/
static char *get_atom(start, mime, type)
char *start;
int mime;
int *type;
{
	/* Find the end of the current atom and set its type */

	/* We don't know what type this atom is yet */

	*type = AT_WS;

	/* Generate the atom */

	switch (*start) {
	case ' ':				/* Whitespace */
	case '\t':
	case '\n':
	case '\r':
		return(set_ws(start, type));
	case '(':				/* Comment */
		return(set_str(start, SC_COMMENT, TC_COMMENT,
			       AT_COMMENT, type));
	case '"':				/* Quoted string */
		return(set_str(start, SC_QSTRING, TC_QSTRING,
			       AT_QSTRING, type));
	case '[':				/* Domain literal */
		return(set_str(start, SC_DLITERAL, TC_DLITERAL,
			       AT_DLITERAL, type));
	case '.':
		return((!mime) ? set_char(start, AT_DOT, type) :
		       set_str(start, SC_ATOM, TC_ATOM, AT_ATOM, type));
	case '@':
		return(set_char(start, AT_AT, type));
	case '%':
		return(set_char(start, AT_PERCENT, type));
	case ',':
		return(set_char(start, AT_COMMA, type));
	case '<':
		return(set_char(start, AT_LANGLEB, type));
	case '>':
		return(set_char(start, AT_RANGLEB, type));
	case ':':
		return(set_char(start, AT_COLON, type));
	case ';':
		return(set_char(start, AT_SEMI, type));
	case ')':
	case ']':
	case '\\':
		a_errno = AERR_ADDRESS;
		a_errtext = xstrdup(start);
		return(NULL);
	case '/':
		return((mime) ? set_char(start, AT_SLASH, type) :
			set_str(start, SC_ATOM, TC_ATOM, AT_ATOM, type));
	case '?':
		return((mime) ? set_char(start, AT_QUERY, type) :
			set_str(start, SC_ATOM, TC_ATOM, AT_ATOM, type));
	case '=':
		return((mime) ? set_char(start, AT_EQUALS, type) :
			set_str(start, SC_ATOM, TC_ATOM, AT_ATOM, type));
	default:
		return(set_str(start, SC_ATOM, (mime) ? TC_MATOM :
			       TC_ATOM, AT_ATOM, type));
	}
	/*NOTREACHED*/
}
/****************************************************************************/
ATOM *add_atom(alist, atom, type)
ATOM *alist;
char *atom;
int type;
{
	/* Add an atom to the list */

	ATOM *node;

	if (alist == NULL) {
		/* Initialise the new node */
	
		node = (ATOM *) xmalloc(sizeof(ATOM));
		node->text = xstrdup(atom);
		node->type = type;
		node->next = NULL;
		return(node);
	} else {
		alist->next = add_atom(alist->next, atom, type);
		return(alist);
	}
	/*NOTREACHED*/
}
/****************************************************************************/
static char *set_char(start, atype, type)
char *start;
int atype, *type;
{
	/* Set the type for a single-character atom of type atype */

	*type = atype;

	/* And return the first character after the atom */

	return(start + 1);
}
/****************************************************************************/
static char *set_str(atom, start, term, atype, type)
char *atom, *start, *term;
int atype, *type;
{
	/* Handle multi-character atoms */

	char *p;
	int nested = 0;

	/* Set the type of the atom */

	*type = atype;

	/* Find the end of the atom */

	for (p = atom; *p != '\0'; p++) {
		/* Handle escaped characters */

		if (atype != AT_ATOM && *p == '\\' && *(p + 1) != '\0') {
			/* Quoted special character; skip */

			p++;
		} else if (p != atom && strchr(term, *p) != NULL) {
			/* Termination character; check nesting */

			if (nested-- <= 0) {
				return((atype == AT_ATOM) ? p : p + 1);
			}
		} else if (p != atom && strchr(start, *p) != NULL) {
			/* Start character; increment nesting level */

			nested++;
		}
	}

 	/* No termination character; error anything but an atom */

	switch (atype) {
	case AT_QSTRING:
		a_errno = AERR_QSTRING;
		break;
	case AT_DLITERAL:
		a_errno = AERR_DLITERAL;
		break;
	case AT_COMMENT:
		a_errno = AERR_COMMENT;
		break;
	default:
		return(p);
	}

	/* If we reached here we need to set the error text */

	a_errtext = xstrdup(atom);
	return(NULL);
}
/****************************************************************************/
static char *set_ws(start, type)
char *start;
int *type;
{
	/* Handle whitespace */

	char *p;

	/* Set the type of the atom */

	*type = AT_WS;

	/* Find the end of the atom */

	for (p = start; p != '\0' && isspace(*p); p++) {
		/* NULL LOOP */
	}
	return(p);
}
/****************************************************************************/
ATOM *find_token(token)
ATOM *token;
{
	/*
	 * Return the first token that isn't whitespace or comment
	 * in the list passed in, or NULL if none found.
	 */

	ATOM *a;

	/* Loop over all the whitespace or comment tokens */

	for (a = token; a != NULL && IS_WS(a); a = a->next) {
		/* NULL LOOP */
	}

	return(a);
}
/****************************************************************************/
ATOM *asearch(alist, type)
ATOM *alist;
int type;
{
	/* Return the first token of type 'type' in alist */

	ATOM *a;

	/* Loop until we find a token of the right type */

	for (a = alist; a != NULL && a->type != type; a = a->next) {
		/* NULL LOOP */
	}

	return(a);
}
/****************************************************************************/
ATOM *adiscard(alist, token)
ATOM *alist, *token;
{
	/* Discard token from the list and return the start of the list */

	ATOM *a;

	/* Check the token isn't at the head of the list */

	if (alist == token) {
		alist = alist->next;
	} else {
		/* Search until we find token */

		for (a = alist; a != NULL; a = a->next) {
			/* And strip it out of the list */

			if (a->next == token) {
				a->next = token->next;
				break;
			}
		}
	}

	/* Now free the discarded token */

	free(token->text);
	free(token);

	return(alist);
}
/****************************************************************************/
ATOM *acut(alist, end)
ATOM *alist, *end;
{
	/*
	 * Strip atoms start up to (but not including) end out of
	 * a list and return the start token of those cut out.
	 */

	ATOM *a;

	/* This is a no-op if end is null */

	if (end != NULL) {
		/* Check there is something to cut */

		if (alist == end) {
			return(NULL);
		}

		/* Search for the end token */

		for (a = alist; a != NULL; a = a->next) {
			/* And cut the list before the end token */

			if (a->next == end) {
				a->next = NULL;
				break;
			}
		}
	}

	/* Now return the modified list */

	return(alist);
}
/****************************************************************************/
ATOM *acopy(alist)
ATOM *alist;
{
	/* Return an allocated copy of alist */

	ATOM *a, *new_list = NULL;

	/* Simply add each token to the new list */

	for (a = alist; a != NULL; a = a->next) {
		new_list = add_atom(new_list, a->text, a->type);
	}

	return(new_list);
}
/****************************************************************************/
ATOM *acat(alist, next)
ATOM *alist, *next;
{
	/* Concatenate next to alist and return the new list */

	ATOM *a;

	/* Return next if the list is empty */

	if (alist == NULL) {
		return(next);
	}

	/* Now search for the end of the list */

	for (a = alist; a != NULL; a = a->next) {
		/* And append next to the list */

		if (a->next == NULL) {
			a->next = next;
			break;
		}
	}

	/* Return the new list */

	return(alist);
}
/****************************************************************************/
ATOM *asplit(alist, token, pos)
ATOM *alist, *token;
char *pos;
{
	/*
	 * Split token into two atoms at the point in the text
	 * specified by pos, discarding the character at pos.
	 * return the newly created atom as the head of a
	 * new list, separate from the old one.
	 */

	ATOM *a, *new_list = NULL;

	/* Check for pos lying at the start of the token */

	if (pos == token->text) {
		/* Pos is at start; token starts the new list */

		for (a = alist; a != NULL; a = a->next) {
			if (a->next == token) {
				a->next = NULL;
				return(token);
			}
		}

		/* Can't split with token at the head of the list */

		return(NULL);
	} else if (*(pos + 1) == '\0') {
		/* Pos is at end; token ends the old list */

		*pos = '\0';
		new_list = token->next;
	} else {
		/* Need to split the atom */

		*pos++ = '\0';
		new_list = add_atom(NULL, pos, token->type);
		new_list->next = token->next;
	}

	/* Terminate the original list and return the new one */

	token->next = NULL;
	return(new_list);
}
/****************************************************************************/
ATOM *acomment(alist)
ATOM *alist;
{
	/* Return an atom comtaining alist as a comment */

	char *buf, *qbuf, *ebuf;
	ATOM *new_list;

	/* Allocate a new string and set it to the comment text */
  
	buf = atext(NULL, alist, AC_UNQUOTE);
	qbuf = vstrcat("(", buf, ")", NULL);
  
	/* Check the text is a valid comment */
  
	if ((new_list = tokenise(qbuf)) == NULL
	    || new_list->next != NULL) {
		/* We need to fix parens in the comment */

		free(qbuf);
		afree(new_list);
		ebuf = escape_chars(buf, EC_COMMENT);
		qbuf = vstrcat("(", ebuf, ")", NULL);
		new_list = add_atom(NULL, qbuf, AT_QSTRING);
	}
  
  	/* Clean up and return the new list */

	free(buf);
	free(qbuf);
	return(new_list);
}
/****************************************************************************/
ATOM *aquote(alist)
ATOM *alist;
{
	/* Return an atom containing alist as a quoted string */

	char *buf, *ebuf, *qbuf;
	ATOM *new_list;

	/* Allocate a new string and set it to the quoted string */

	buf = atext(NULL, alist, AC_UNQUOTE);
	ebuf = escape_chars(buf, EC_QSTRING);
	qbuf = vstrcat("\"", ebuf, "\"", NULL);

	/* Make an atom list containing the string and a space */

	new_list = add_atom(NULL, qbuf, AT_QSTRING);

  	/* Clean up and return the new list */
  
  	free(buf);
	free(qbuf);
	return(new_list);
}
/****************************************************************************/
static char *avalue(atom)
ATOM *atom;
{
	/*
	 * Return the "value" of the atom in a static buffer.  The
	 * "value" is the text without any quoting characters.
	 */

	static char *buf = NULL;
	char *ubuf;

	/* Free any old return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* Now extract the unquoted text of the atom */

	if (atom->type == AT_QSTRING || atom->type == AT_COMMENT) {
		/* Allocate and fill the return buffer */

		buf = xmalloc(strlen(atom->text) - 1);
		(void) strncpy(buf, atom->text + 1, strlen(atom->text) - 2);
		buf[strlen(atom->text) - 2] = '\0';
	} else {
		/* Just copy the atom's text into the buffer */

		buf = xstrdup(atom->text);
	}

	/* Now unescape any quotes in quoted strings */

	if (atom->type == AT_QSTRING) {
		/* Copy and unescape the buffer */

		ubuf = unescape_chars(buf, EC_QSTRING);
		free(buf);
		buf = xstrdup(ubuf);
	}

	/* And return the buffer */

	return(buf);
}
/****************************************************************************/
char *atext(buf, alist, canon)
char *buf;
ATOM *alist;
int canon;
{
	/*
	 * Return an allocated string containing the concatenation of the
	 * string held in buf (if non-null) and the atom list supplied.
	 * Canon specifies the canonicalsation required: none, trim
	 * leading and trailing white space, trim white space or full.
	 */

	size_t len = 0;
	ATOM *a, *start, *end;

	/* Return buf unchanged if alist is NULL */

	if (alist == NULL) {
		return(buf);
	}

	/* Set the start and end positions in the list */

	start = end = NULL;
	for (a = alist; canon != AC_NONE && a != NULL; a = a->next) {
		if (a->type != AT_WS) {
			start = (start == NULL) ? a : start;
			end = a->next;
		}
	}
	start = (start == NULL) ? alist : start;

	/* Find out how long the text will be */

	for (a = start; a != end; a = a->next) {
		if (a->type == AT_QSTRING && canon == AC_UNQUOTE ||
		    a->type == AT_COMMENT && canon == AC_UNCOMMENT) {
			/* We will strip the start and end of this token */

			len += strlen(a->text) - 2;
		} else if (a->type == AT_WS && canon == AC_UNQUOTE) {
			/* We'll add a single whitespace character */

			len++;
		} else if (a->type == AT_SEMI && canon == AC_FULL) {
			/* We'll be adding a space after the semi */

			len += strlen(a->text) + 1;
		} else if (canon != AC_FULL || a->type != AT_WS
			   && a->type != AT_COMMENT) {
			/* Add the length of this token */

			len += strlen(a->text);
		}
	}

	/* Allocate or reallocate the string */

	if (buf == NULL) {
		buf = xmalloc(len + 1);
		*buf = '\0';
	} else {
		buf = xrealloc(buf, strlen(buf) + len + 1);
	}

	/* Concatenate the list to the string */

	for (a = start; a != end; a = a->next) {
		if (a->type == AT_QSTRING && canon == AC_UNQUOTE ||
		    a->type == AT_COMMENT && canon == AC_UNCOMMENT) {
			/* Add the token with no delimiters */

			(void) strcat(buf, avalue(a));
		} else if (a->type == AT_WS && canon == AC_UNQUOTE) {
			/* Add a single (canonical) space */

			(void) strcat(buf, " ");
		} else if (a->type == AT_SEMI && canon == AC_FULL) {
			/* Add the semi foillowed by a space */

			(void) strcat(buf, a->text);
			(void) strcat(buf, " ");
		} else if (canon != AC_FULL || a->type != AT_WS
			   && a->type != AT_COMMENT) {
			/* Add the token to the buffer */

			(void) strcat(buf, a->text);
		}
	}

	/* Return the string */

	return(buf);
}
/****************************************************************************/
void afree(alist)
ATOM *alist;
{
	/* Free the atom list alist */

	if (alist != NULL) {
		afree(alist->next);
		free(alist->text);
		free(alist);
	}
	return;
}
/****************************************************************************/
char *a_strerror()
{
	/* Return the text associated with an error */

	/* The list of base messages */

	static char *aerr_base[] = {
		AERR_BASE_TEXT, AERR_BASE_TEXT, AERR_BASE_TEXT,
		AERR_BASE_TEXT, AERR_BASE_TEXT, AERR_BASE_TEXT,
		AERR_BASE_TEXT, AERR_BASE_TEXT, AERR_BASE_TEXT,
		AERR_BASE_TEXT, AERR_BASE_TEXT, CERR_BASE_TEXT,
		CERR_BASE_TEXT, CERR_BASE_TEXT, EERR_BASE_TEXT,
		EERR_BASE_TEXT, RERR_BASE_TEXT, RERR_BASE_TEXT,
		RERR_BASE_TEXT
	};

	/* The list of error messages */

	static char *aerr_text[] = {
		ATEXT_NONE, ATEXT_NULL, ATEXT_ADDRESS, ATEXT_BRACKET, 
		ATEXT_ROUTE, ATEXT_LOCAL, ATEXT_DOMAIN, ATEXT_UUCP,
		ATEXT_QSTRING, ATEXT_DLITERAL, ATEXT_COMMENT,
		CTEXT_NULL, CTEXT_TYPE, CTEXT_PARAM, ETEXT_NULL,
		ETEXT_ENCODING, RTEXT_TOKEN, RTEXT_BRACKET,
		RTEXT_REFERENCE
	};

	/* A static buffer for the error message */

	static char *err_buf = NULL;

	/* Free the buffer if previously allocated */

	if (err_buf != NULL) {
		free(err_buf);
	}

	/* Allocate the buffer */

	if (a_errtext != NULL) {
		err_buf = xmalloc(strlen(aerr_base[a_errno]) +
				  strlen(aerr_text[a_errno]) +
				  strlen(a_errtext) + 2);
	} else {
		err_buf = xmalloc(strlen(aerr_base[a_errno]) +
				  strlen(aerr_text[a_errno]) + 1);
	}

	/* Fill the buffer */

	(void) strcpy(err_buf, aerr_base[a_errno]);
	(void) strcat(err_buf, aerr_text[a_errno]);

	/* Append the text of the error if any */

	if (a_errtext != NULL) {
		(void) strcat(err_buf, " ");
		(void) strcat(err_buf, a_errtext);
	}

	/* Return the error string */

	return(err_buf);
}
/****************************************************************************/
static char *escape_chars(text, echars)
char *text, *echars;
{
	/*
	 * Return a static buffer containing text with any character
	 * listed in echars escaped with a backslash.
	 */

	static char *buf = NULL;
	char *p, *q;
	int escaped = FALSE;

	/* (Re)allocate the return buffer */

	q = buf = (buf == NULL) ? xmalloc(strlen(text) * 2 + 1)
		: xrealloc(buf, strlen(text) * 2 + 1);

	/* Now copy the text, escaping as required */

	p = text;
	while (*p != '\0') {
		/* Escape this character if required */

		if (!escaped && strchr(echars, *p) != NULL) {
			/* Insert a backslash before the character */

			*q++ = '\\';
		}

		/* Check if the next character is escaped */

		escaped = (!escaped && *p == '\\');

		/* And copy the character itself */

		*q++ = *p++;
	}
	*q = '\0';

	/* Resize and return the static buffer */

	buf = xrealloc(buf, strlen(buf) + 1);
	return(buf);
}
/****************************************************************************/
static char *unescape_chars(text, echars)
char *text, *echars;
{
	/*
	 * Return a static buffer containing text with any backslash
	 * escapes of characters listed in echars removed.
	 */

	static char *buf = NULL;
	char *p, *q;
	int escaped = FALSE;

	/* (Re)allocate the return buffer */

	q = buf = (buf == NULL) ? xmalloc(strlen(text) + 1)
		: xrealloc(buf, strlen(text) + 1);

	/* Now copy the text, escaping as required */

	p = text;
	while (*p != '\0') {
		/* Unescape this character if required */

		if (!escaped && *(p + 1) != '\0' &&
		    strchr(echars, *(p + 1)) != NULL) {
			/* Skip the backslash character */

			p++;
		}

		/* Check if the next character is escaped */

		escaped = (!escaped && *p == '\\');

		/* And copy the character itself */

		*q++ = *p++;
	}
	*q = '\0';

	/* Resize and return the static buffer */

	buf = xrealloc(buf, strlen(buf) + 1);
	return(buf);
}
/****************************************************************************/
