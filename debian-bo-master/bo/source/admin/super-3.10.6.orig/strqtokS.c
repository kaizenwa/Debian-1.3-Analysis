#include <ctype.h>
#include <string.h>

#ifndef NULL
#define NULL 0
#endif

/*
 *      Copyright (c) 1993 by California Institute of Technology.
 *      Written by William Deich.  Not derived from licensed software.

 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 */

/* strqtokS.c, v1.1
 * Derived from strqtok.c v3.0.
 * Stripped down for simplicity and speed.
 */

/* The pointer into the string, indicating where to start next token search.
 * The purpose of making it extern is that the caller can stash away
 * a copy of strqS_start, process another string, then restore the pointer
 * and finish processing the first string.
 */
char *strqS_start=NULL;

/* strqS_qm and strqS_cc give the current sets of quotemarks and comment
 * characters, used if the quotemarks or commentchars argument, respectively,
 * is a NULL ptr.  Each of these should point an array of 256 chars;
 * each of those chars should be 1 (0) if the corresponding character
 * is (is not) in the set.

 * If you pass an actual string in, then the arrays strqS_qm_set and
 * strqS_cc_set will be initialized, and strqS_qm and strqS_cc will point
 * to them.  But if you pass null ptrs for quotemarks and commentchars,
 * then strqS_qm and strqS_cc will be used as they are -- so you have to
 * make sure they point to something valid.  For instance, you can initialize
 * strqS_qm_set and strqS_cc_set, and make strqS_qm & strqS_cc point to them.
 * Or you can initialize your own array and make strqS_qm and strqS_cc point
 * to your own arrays.
 */
unsigned char *strqS_qm=NULL, *strqS_cc=NULL; 
unsigned char strqS_qm_set[256], strqS_cc_set[256]; 

/* strqS_delim is set to the character that terminated the last token
 * (and was replaced with a null character).
 */
char strqS_delim='\0';

/* strqtokS(s, delim, quotemarks, commentchars, flags)
 * ...is same as strtok(s, delim), except that

 *	o  quotemarks: tokens are quoted by pairs of these characters,
 *		allowing characters that are normally
 *		token delimiters to be part of a token.

 *	o  commentchars: if one of these characters occurs outside a
 *		quoted token, everything is ignored up to the next
 *		\n or \0, inclusive.

 *	o  flags:
 *		flags & 01:	quotemarks are stripped from tokens.
 *			Default: quotemarks are left in returned tokens.

 *	o The following approximation of Bourne shell rules is used for
 *		interpreting backslash-X:
 *		A) If X is null: the backslash is discarded.
 *		B) \newline is discarded entirely.
 *		C) Otherwise, if outside a quote: \X is replaced by plain X, and
 *		  not treated as a delimiter, quotemark, or comment character.
 *		D) Otherwise we are inside a quote: discard the backslash if
 *		  preceding left quote or another backslash; and in any case
 *		  the following character is plain:
 *			\\ -> \
 *			\Q -> Q (Q is the char that started the quote)
 *			\X -> \X (ie the backslash is a plain character)
 *		  If flag 0200 is enabled, the backslash must NOT be part of the
 *		  delimiter, quotemark, or comment char sets, or else the
 *		  results are undefined.

 *	o  Backslash-newline is always completely discarded, as if it had
 *		never been typed.
 *	
 *	Note: strqtokS(), strqtok(), and strtok() are completely independent,
 *	so one may safely use them in parallel.

 */

char *
strqtokS(s, delim, quotemarks, commentchars, flags)
    char *s;		/* String to tokenize.  NULL to continue same str */
    char *delim;	/* Token delimiters.  Can be changed w/ each call. */
    char *quotemarks;	/* Quote marks.  Can be changed w/ each call. */
    char *commentchars;	/* Comment chars. Can be changed w/ ea call. */
    unsigned int flags;	/*	flags &  01 ->	strip quotes; */
{
    register unsigned char *p, *q;
    register unsigned char *qm, *cc;
    unsigned char leftquote = '\0';
    char *token;

    register int inquote, bn, incomment;

    /* Strip quotemarks from tokens? */
    int stripquote = (flags & 01);

    /* New string? */
    if (s)
	strqS_start = s;

    /* Need new qm or cc arrays? */
    if (quotemarks) {
	memset(strqS_qm_set, '\0', sizeof(strqS_qm_set));
	for (p=(unsigned char *) quotemarks; *p; )
	    strqS_qm_set[*p++] = 1;
	qm = strqS_qm_set;
    } else {
	qm = strqS_qm;
    }
    if (commentchars) {
	memset(strqS_cc_set, '\0', sizeof(strqS_cc_set));
	for (p=(unsigned char *) commentchars; *p; )
	    strqS_cc_set[*p++] = 1;
	cc = strqS_cc_set;
    } else {
	cc = strqS_cc;
    }

    if (!strqS_start)
	return (char *) NULL;
    
    /* Skip leading delimiters and comments. */
    /* Use p and q to walk through the user's string:
     *    p will follow input characters;
     *    q will overwrite w/ outputted characters, minus possibly-stripped
     *		quotes and including nulls after each token.
     */
    p = (unsigned char *) strqS_start;
    /* Advance pointer p past delimiters and comments */
    for (bn=0; p && *p && (strchr(delim, *p) || cc[*p]
		|| (bn = (*p=='\\' && *(p+1) == '\n'))) ; ) {
	if (cc[*p]) {
	    /* It's a comment */
	    p = (unsigned char *) strchr((char *)p, '\n');
	    if (p)
		p++;
	} else if (bn) {
	    /* `\n' */
	    p += 2;
	    bn = 0;
	} else {
	    p++;
	}
    }
    if (!p || !(*p))
	return (char *) NULL;

    /* `token' will be used to return a ptr to the token */
    q = p;
    token = (char *) p;
    inquote = incomment = 0;
    /* Now figure out each arg */
    /* NOTE: we break out of the while loop at the end of a token. */
    while (*p) {
	if (*p == '\\') {
	    /* at ``\X''; Advance to X character and process it */

	    if (! *(++p))
		break;	/* reached end of string */
	    
	    if (*p == '\n') {
		p++;	/* Discard backslash-newline entirely */
	    } else if (inquote && (*p == '\\' || *p == leftquote)) {
		*q++ = *p++;
	    } else if (inquote) {
		*q++ = '\\';
		*q++ = *p++;
	    } else {
		*q++ = *p++;
	    }

	} else if (!inquote) {
	    if (strchr(delim, *p) || cc[*p]) {
		strqS_delim = (char) *q; /* Reached end of token */
		*q = '\0';
		p++;			/* advance p for next token */
		break;			/* break out of while loop */
	    } else if (qm[*p]) {
		inquote = 1;		/* Beginning a quoted segment */
		leftquote = *p++;	/* Save quote char for matching with */
		if (!stripquote)
		    *q++ = leftquote;
	    } else {
		*q++ = *p++;
	    }

	} else if (inquote && leftquote == *p) {
	    inquote = 0;		/* Ending a quoted segment */
	    p++;
	    if (!stripquote)
		*q++ = leftquote;

	} else {
	    *q++ = *p++;
	}
    }
    strqS_start = (char *) p;
    if (*q) {
	strqS_delim = (char) *q;
	*q = '\0';
    }

    return token;
}
