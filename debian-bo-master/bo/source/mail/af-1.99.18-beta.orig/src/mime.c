/* Mime.c - MIME header checking and translation for af.
   Copyright (C) 1994, 1995, 1996, 1997 Malc Arnold.

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
#include "af.h"
#include "atom.h"
#include "mime.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: mime.c,v 1.8 1997/03/05 21:23:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup();
extern char *vstrcat(), *atext(), *get_vtext();
extern int strcasecmp(), strncasecmp();
extern void afree(), free(), set_sys_tags();
extern ATOM *tokenise(), *ctokenise(), *find_token();
extern ATOM *asearch(), *acut(), *adiscard();

/* Local function declarations */

static char *mime_type(), *mime_encoding();
static int viewable();
static void mfree();
static MIMETYPE *parse_contype();
static MIMEPARAM *parse_params();

/****************************************************************************/
/* Import the error flag and text for parsing and translation */

extern int a_errno;
extern char *a_errtext;

/****************************************************************************/
char *contype(ctype)
char *ctype;
{
	/*
	 * Form a Content-Type header from the string passed,
	 * and return it in a newly-allocated string.
	 */

	return(mime_type(ctype, TRUE, AC_TRIM));
}
/****************************************************************************/
char *c_contype(ctype)
char *ctype;
{
	/*
	 * Form a fully-canonical Content-Type header from the string
	 * passed, and return it in a newly-allocated string.
	 */

	return(mime_type(ctype, TRUE, AC_FULL));
}
/****************************************************************************/
char *typeonly(ctype)
char *ctype;
{
	/*
	 * Form a Content-Type header without parameters
	 * and return it in a newly-allocated string.
	 */

	return(mime_type(ctype, FALSE, AC_FULL));
}
/****************************************************************************/
char *encoding(cte)
char *cte;
{
	/*
	 * Form a Content-Transfer-Encoding header from the string
	 * passed, and return it in a newly-allocated string.
	 */

	return(mime_encoding(cte, AC_TRIM));
}
/****************************************************************************/
char *c_encoding(cte)
char *cte;
{
	/*
	 * Form a fully-canonical Content-Transfer-Encoding header from
	 * the string passed, and return it in a newly-allocated string.
	 */

	return(mime_encoding(cte, AC_FULL));
}
/****************************************************************************/
int textual(message)
MESSAGE *message;
{
	/*
	 * Return TRUE if the message can be regarded as textual,
	 * ie. it is not encoded, and is of type text/plain with
	 * either no specified charset, or one listed as viewable.
	 */

	static char *text_encodings[] = TEXTENCODINGS;
	char **e;
	MIMETYPE *mtype;
	MIMEPARAM *param;

	/* Parse the content type string if we have one */

	if (message->contype != NULL &&
	    (mtype = parse_contype(message->contype, AC_FULL)) != NULL) {
		/* Check the type and subtype of the message */

		if (mtype->type != NULL && strcasecmp(mtype->type, TEXTTYPE)
		    || mtype->subtype != NULL && 
		    strcasecmp(mtype->subtype, TEXTSUBTYPE)) {
			/* This message isn't text/plain */

			mfree(mtype);
			return(FALSE);
		}

		/* Now search for a charset parameter */

		for (param = mtype->params; param != NULL;
		     param = param->next) {
			/* Is this the charset parameter? */

			if (!strcasecmp(param->name, TEXTCHARSET)
			    && !viewable(param->value)) {
				/* This charset isn't textual */

				mfree(mtype);
				return(FALSE);
			}
		}

		/* Free the mime type */

		mfree(mtype);
	}

	/* Now check if this message is encoded */

	if (message->encoding == NULL) {
		/* No encoding specified; assume none */

		return(TRUE);
	}

	/* We have an encoding, check if it's ok */

	for (e = text_encodings; *e != NULL; e++) {
		/* Is this encoding textual? */

		if (!strcasecmp(message->encoding, *e)) {
			/* This is a textual encoding */

			return(TRUE);
		}
	}

	/* The message is encoded in a non-text format */

	return(FALSE);
}
/****************************************************************************/
int retextual(buf)
MAILBUF *buf;
{
	/* Update the Mime flag after viewable-charsets changes */

	MAILBUF *b = buf;
	MESSAGE *m;
	int changed = FALSE;

	/* Loop over all available buffers */

	do {
		/* Check all messages in this buffer */

		for (m = b->messages; m != NULL &&
		     m->text != NULL; m = m->next) {
			/* Check if the message may not be textual */

			if ((m->contype != NULL || m->encoding != NULL)
			    && textual(m) == m->nontext) {
				/* This message's status has changed */

				m->nontext = !(m->nontext);
				set_sys_tags(m);
				changed = TRUE;
			}
		}

		/* Move on to the next buffer */

		b = b->next;
	} while (b != buf);

	/* Return whether any message statuses changed */

	return(changed);
}
/****************************************************************************/
int non_ascii(message)
MESSAGE *message;
{
	/* Return whether the message is 7-bit ASCII text or not */
	
	MIMETYPE *mtype;
	MIMEPARAM *param;

	/* Parse the content-type of viewable MIME messages */

	if (!message->nontext && message->contype != NULL &&
	    (mtype = parse_contype(message->contype, AC_FULL)) != NULL) {
		/* Now search for a charset parameter */

		for (param = mtype->params; param != NULL;
		     param = param->next) {
			/* Is this the charset parameter? */

			if (!strcasecmp(param->name, TEXTCHARSET) &&
			    strcasecmp(param->value, ASCIICHARSET)) {
				/* This message uses an 8-bit charset */

				mfree(mtype);
				return(TRUE);
			}
		}

		/* Free the mime type */

		mfree(mtype);
	}

	/* This message must be in ASCII */

	return(FALSE);
}
/****************************************************************************/
static char *mime_type(text, params, canon)
char *text;
int params, canon;
{
	/* Parse a Content-Type and return it as a string */

	char *type;
	MIMETYPE *mtype;
	MIMEPARAM *param;

	/* Parse the content type and parameters */

	if ((mtype = parse_contype(text, canon)) == NULL) {
		/* Failed parsing content type */

		return(NULL);
	}

	/* Make the new content-type string */

	type = vstrcat(mtype->type, "/", mtype->subtype, NULL);

	/* Add on any supplied parameters */

	for (param = mtype->params; params &&
	     param != NULL; param = param->next) {
		/* Append the parameter to the string */

		type = xrealloc(type, strlen(type) + strlen(param->name)
				+ strlen(param->value) + 4);
		(void) strcat(type, "; ");
		(void) strcat(type, param->name);
		(void) strcat(type, "=");
		(void) strcat(type, param->value);
	}

	/* Free the type info and return the content-type string */

	mfree(mtype);
	return(type);
}
/****************************************************************************/
static char *mime_encoding(text, canon)
char *text;
int canon;
{
	/* Parse a Content-Transfer-Encoding header */

	char *enc;
	ATOM *alist, *start;

	/* Tokenise the encoding string */

	if ((alist = tokenise(text)) == NULL) {
		return(NULL);
	}

	/* Find the encoding value in the first atom */

	if ((start = find_token(alist)) == NULL) {
		a_errno = EERR_NULL;
		afree(alist);
		return(NULL);
	}

	/* Check there are no trailing tokens */

	if (find_token(start->next) != NULL) {
		a_errno = EERR_ENCODING;
		afree(alist);
		return(NULL);
	}

	/* Now copy the text into a buffer */

	enc = atext(NULL, alist, canon);

	/* Free the atom list and return the encoding */

	afree(alist);
	return(enc);
}
/****************************************************************************/
static MIMETYPE *parse_contype(ctype, canon)
char *ctype;
int canon;
{
	/* Parse a Content-Type: header */

	ATOM *alist, *type, *slash;
	ATOM *subtype, *semi;
	MIMETYPE *mtype;

	/* Tokenise the content-type string */

	if ((alist = ctokenise(ctype)) == NULL) {
		/* Failed to tokenise the string */

		return(NULL);
	}

	/* Find the type and subtype in the atom list */

	type = find_token(alist);
	slash = (type != NULL) ? find_token(type->next) : NULL;
	subtype = (slash != NULL) ? find_token(slash->next) : NULL;
	semi = (subtype != NULL) ? find_token(subtype->next) : NULL;

	/* Check we got a valid type and subtype? */

	if (type == NULL || type->type != AT_ATOM || slash == NULL
	    || slash->type != AT_SLASH || subtype == NULL
	    || subtype->type != AT_ATOM
	    || semi != NULL && semi->type != AT_SEMI) {
		/* Invalid type/subtype string, set the error */
	    
		a_errno = (type == NULL) ? CERR_NULL : CERR_TYPE;
		if (a_errtext != NULL) {
			free(a_errtext);
		}
		a_errtext = (type != NULL) ? atext(NULL, type, AC_NONE)
					   : xstrdup(END_ERRTEXT);
		return(NULL);
	}

	/* Split the atom list into it's components */

	type = acut(alist, slash);
	subtype = adiscard(slash, slash);
	subtype = acut(subtype, semi);

	/* Allocate and set up the mime type structure */

	mtype = (MIMETYPE *) xmalloc(sizeof(MIMETYPE));
	mtype->type = atext(NULL, type, canon);
	mtype->subtype = atext(NULL, subtype, canon);
	mtype->params = NULL;

	/* Free the type atom lists */

	afree(type);
	afree(subtype);

	/* Now parse the parameters */

	if (semi != NULL &&
	    (mtype->params = parse_params(semi, canon)) == NULL) {
		/* Error parsing parameters */

		mfree(mtype);
		return(NULL);
	}

	/* Now return the type structure */

	return(mtype);
}
/****************************************************************************/
static MIMEPARAM *parse_params(alist, canon)
ATOM *alist;
int canon;
{
	/* Parse a content-type's parameters */

	ATOM *semi, *name, *equals, *value, *newsemi;
	MIMEPARAM *params = NULL, *p = NULL;

	/* We start at the first semicolon */

	semi = alist;

	/* Now loop over each parameter */

	while (semi != NULL) {
		/* Extract the parameter from the atom list */

		name = find_token(semi->next);
		equals = (name != NULL) ? find_token(name->next) : NULL;
		value = (equals != NULL) ? find_token(equals->next) : NULL;
		newsemi = (value != NULL) ? find_token(value->next) : NULL;

		/* Check we got a valid parameter */

		if (semi->type != AT_SEMI || name == NULL ||
		    name->type != AT_ATOM || equals == NULL ||
		    equals->type != AT_EQUALS || value == NULL ||
		    value->type != AT_ATOM && value->type != AT_QSTRING ||
		    newsemi != NULL && newsemi->type != AT_SEMI) {

			/* Invalid parameter string, set the error */
	    
			a_errno = CERR_PARAM;
			if (a_errtext != NULL) {
				free(a_errtext);
			}
			a_errtext = atext(NULL, semi, AC_NONE);
			afree(semi);
			return(NULL);
		}

		/* Split the atom list into it's components */

		name = adiscard(semi, semi);
		name = acut(name, equals);
		value = adiscard(equals, equals);
		value = acut(value, newsemi);

		/* Add the parameter to the list */

		if (params == NULL) {
			params = p = (MIMEPARAM *) xmalloc(sizeof(MIMEPARAM));
		} else {
			 p->next = (MIMEPARAM *) xmalloc(sizeof(MIMEPARAM));
			 p = p->next;
		}
		p->name = atext(NULL, name, canon);
		p->value = atext(NULL, value, canon);
		p->next = NULL;

		/* And free the parameter lists */

		afree(name);
		afree(value);
		/* Now update the current semicolon */

		semi = newsemi;
	}

	/* Parsed ok, return the parameter list */

	return(params);
}
/****************************************************************************/
static int viewable(charset)
char *charset;
{
	/* Return TRUE if the character set can be viewed */

	char *viewed, *canonical;
	char *cset, *end;
	unsigned len;
	ATOM *alist;

	/* First canonicalise the charset */

	if ((alist = ctokenise(charset)) == NULL) {
		/* Not a valid charset */

		return(FALSE);
	}
	canonical = atext(NULL, alist, AC_UNQUOTE);
	afree(alist);

	/* Get the list of viewable charsets */

	if ((viewed = get_vtext(V_VIEWABLE)) == NULL) {
		/* No character sets are viewable */

		free(canonical);
		return(FALSE);
	}

	/* Loop through checking charset against viewed */

	cset = viewed;
	while (cset != NULL && *cset != '\0') {
		/* Find the end of the viewable charset */

		end = strchr(cset, ':');

		/* How long is the current charset? */

		len = (end != NULL) ? end - cset : strlen(cset);

		/* Does this charset match the current one? */

		if (!strncasecmp(canonical, cset, len)) {
			free(canonical);
			return(TRUE);
		}

		/* Update the loop counter */

		cset = (end != NULL) ? end + 1 : NULL;
	}

	/* The character set is not viewable */

	free(canonical);
	return(FALSE);
}
/****************************************************************************/
static void mfree(mtype)
MIMETYPE *mtype;
{
	/* Free a mime type structure */

	MIMEPARAM *param = mtype->params, *next;

	/* Free the type and subtype */

	free(mtype->type);
	free(mtype->subtype);

	/* Free the parameters */
	
	while (param != NULL) {
		/* Save next param and free this one */

		next = param->next;
		free(param->name);
		free(param->value);
		free(param);

		/* Update current parameter */

		param = next;
	}

	/* Now free the structure itself */

	free(mtype);
	return;
}
/****************************************************************************/
