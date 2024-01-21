/* Keyseq.c - Key sequence handling for af.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */


#include <stdio.h>
#include <ctype.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include STRING_HDR

#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif /* HAVE_MEMORY_H */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: keyseq.c,v 1.10 1996/05/06 10:11:30 malc Exp $";
static char *KeyseqId = KEYSEQID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup();
extern int get_vval(), ttabspace(), mklower();
extern void free();

#ifndef HAVE_MEMORY_H
extern char *memcpy();
#endif /* HAVE_MEMORY_H */

/* Local function declarations */

int numeric(), charlen();
void free_seq();
KEYSEQ *make_seq();

static char *seq_as_str(), *key_as_str();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* The lists of specially-handled key sequences */

static SPECIAL disp_keys[] = DISP_KEYS;
static SPECIAL hdr_keys[] = HDR_KEYS;
static SPECIAL read_keys[] = READ_KEYS;

/****************************************************************************/
KEYSEQ *null_seq()
{
	/* Make a new key sequence containing a null string */

	return(make_seq("", 0));
}
/****************************************************************************/
KEYSEQ *make_seq(keys, len)
char *keys;
int len;
{
	/* Make a new key sequence which contains keys to len chars */

	KEYSEQ *newseq;

	/* Allocate the space for the new key sequence */

	newseq = (KEYSEQ *) xmalloc(sizeof(KEYSEQ));

	/* Fill the new key sequence */

	newseq->keys = xmalloc((len > 0) ? len : 1);
	(void) memcpy(newseq->keys, keys, len);
	newseq->len = len;

	/* Return the new key sequence */

	return(newseq);
}
/****************************************************************************/
KEYSEQ *new_seq(seq, key)
KEYSEQ *seq;
char key;
{
	/* Make a new key sequence which concatenates seq and key */

	KEYSEQ *newseq;

	/* Allocate the space for the new key sequence */

	newseq = (KEYSEQ *) xmalloc(sizeof(KEYSEQ));

	/* Initialise the new key sequence */

	newseq->len = (seq == NULL) ? 1 : seq->len + 1;
	newseq->keys = xmalloc(newseq->len);

	/* And set up the keys of the sequence */

	if (seq != NULL) {
		(void) memcpy(newseq->keys, seq->keys, seq->len);
	}
	newseq->keys[newseq->len - 1] = key;

	/* Return the new key sequence */

	return(newseq);
}
/****************************************************************************/
KEYSEQ *add_seq(seq, key)
KEYSEQ *seq;
char key;
{
	/* Add key to the key sequence stored in seq */

	/* Allocate space for the key sequence if required */

	if (seq == NULL) {
		seq = (KEYSEQ *) xmalloc(sizeof(KEYSEQ));
		seq->keys = xmalloc(1);
		seq->len = 0;
	}

	/* Add the key to the sequence */

	seq->keys = xrealloc(seq->keys, seq->len + 1);
	seq->keys[seq->len++] = key;

	return(seq);
}
/****************************************************************************/
void free_seq(seq)
KEYSEQ *seq;
{
	/* Free the space used by a key sequence */

	if (seq != NULL) {
		free(seq->keys);
		free(seq);
	}
	return;
}
/****************************************************************************/
char *strcanon(str, form)
char *str;
int form;
{
	/* Return the canonical output format of str in form */

	static char *buf = NULL;
	KEYSEQ *seq;

	/* Free any old return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* It's easiest to do this via an intermediate sequence */

	seq = make_seq(str, strlen(str));
	buf = xstrdup(seq_as_str(seq, form));
	free_seq(seq);

	return(buf);
}
/****************************************************************************/
char *strseq(seq, form)
KEYSEQ *seq;
int form;
{
	/* Return a string containing seq in printable form */

	static char *buf = NULL;

	/* Free any previous return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* Now fill and return the return buffer */

	buf = xstrdup(seq_as_str(seq, form));
	return(buf);
}
/****************************************************************************/
char *strkey(key, form)
char key;
int form;
{
	/* Return a string containing key in printable form */

	static char buf[MAX_KEY_LEN + 1];

	/* Now fill and return the return buffer */

	(void) strcpy(buf, key_as_str(key, form));
	return(buf);
}
/****************************************************************************/
int numeric(seq)
KEYSEQ *seq;
{
	/* Return TRUE if seq contains a valid number */

	int key, first;

	/* Check if we have enough sequence to be a number */

	if (seq == NULL || seq->len == 0) {
		return(FALSE);
	}

	/* Check if we need to skip a leading sign */

	first = (seq->len > 1 && (seq->keys[0] == '-' ||
			seq->keys[0] == '+')) ? 1 : 0;
									   
	/* Now check if the remainder is numeric */

	for (key = first; key < seq->len; key++) {
		/* Check if this character is ASCII and numeric  */

		if (!isascii(seq->keys[key]) || !isdigit(seq->keys[key])) {
			/* This isn't a number */

			return(FALSE);
		}
	}

	/* This must be a number */

	return(TRUE);
}	
/****************************************************************************/
int esckey(key)
int key;
{
	/* Return the value of the escape sequence \<key> */

	char buf[MAX_KEY_LEN + 1];
	SPECIAL *s;

	/* Write the escape sequence into the buffer */

	(void) sprintf(buf, "\\%c", key);

	/* Check if the string is a specially-read key */

	for (s = read_keys; s->string != NULL; s++) {
		if (!strcmp(s->string, buf)) {
			return(s->key);
		}
	}

	/* The key simply represents itself */

	return(key);
}
/****************************************************************************/
char *strchar(c, pos, iso)
char c;
int pos, iso;
{
	/* Return a string containing c in printable form */

	static char buf[MAX_KEY_LEN + 1];
	static char *spaces = "        ";

	/* We handle tabs specially on-screen */

	if (c == '\t') {
		/* Fill the buffer with spaces and truncate it */

		(void) strcpy(buf, spaces);
		buf[charlen(c, pos, iso)] = '\0';

		/* Now return the buffer */

		return(buf);
	}

	/* Check for characters we should just return */

	if (iso && !isascii(c) && toascii(c) >= ' ') {
		(void) sprintf(buf, "%c", c);
		return(buf);
	}

	/* Otherwise we just return the key via key_as_str */

	(void) strcpy(buf, key_as_str(c, SK_DISPLAY));
	return(buf);
}
/****************************************************************************/
int charlen(c, pos, iso)
char c;
int pos, iso;
{
	/* Return the number of screen characters c occupies */

	if (c == '\t') {
		return(ttabspace() - (pos % ttabspace()));
	}

	/*
	 * Metachars take 4 columns (or 1 if we're printing an ISO
	 * character set), controls 2, and printable characters 1.
	 */

	return((!isascii(c)) ? (iso && toascii(c) >= ' ')
	       ? 1 : 4 : (!isprint(c)) ? 2 : 1);
}
/***************************************************************************/
static char *seq_as_str(seq, form)
KEYSEQ *seq;
int form;
{
	/* Return a string containing seq in printable form */

	static char *buf = NULL;
	int key;

	/* Free any previous return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* Allocate the buffer to the maximum possible size */

	buf = xmalloc(seq->len * MAX_KEY_LEN + 1);
	*buf = '\0';

	/* Check if we need to quote a read symbol */

	if (form == SK_READSYM && numeric(seq)) {
		(void) strcat(buf, READ_PREFIX);
	}

	/* Now put the keys into the buffer */

	for (key = 0; key < seq->len; key++) {
		if (key && form == SK_KEYSEQ) {
			(void) strcat(buf, " ");
		}
		(void) strcat(buf, key_as_str(seq->keys[key], form));
	}

	/* Resize and return the string */

	buf = xrealloc(buf, strlen(buf) + 1);
	return(buf);
}
/****************************************************************************/
static char *key_as_str(key, form)
char key;
int form;
{
	/* Return a string containing key in printable form */

	static char buf[MAX_KEY_LEN + 1];
	SPECIAL *s;

	/* Initialise the return buffer */

	buf[0] = '\0';

	/* Handle metacharacters by expanding them */

	if (!isascii(key) && KEYFORM(form)) {
		/* Print this metacharacter as required */

		key = toascii(key);
		(void) sprintf(buf, "%s%s", READFORM(form)
			       ? READ_PREFIX : "", META_PREFIX);
	} else if (!isascii(key) && form != SK_NONE && !SYMFORM(form)) {
		/* Print the character as an octal value */

		(void) sprintf(buf, "%s%03o", OCTL_PREFIX,
			       (unsigned char) key);
		return(buf);
	}

	/* Check for specially-printed output keys */

	for (s = disp_keys; form == SK_KEYSEQ
	     && s->string != NULL; s++) {
		if (s->key == key) {
			(void) strcat(buf, s->string);
			return(buf);
		}
	}

	/* And for specially-printed header keys */

	for (s = hdr_keys; form == SK_HEADER
	     && s->string != NULL; s++) {
		if (s->key == key) {
			(void) strcat(buf, s->string);
			return(buf);
		}
	}

	/* And for specially-printed read keys */

	for (s = read_keys; READFORM(form) &&
	     (KEYFORM(form) || STRFORM(form)) && s->string != NULL; s++) {
		if (s->key == key) {
			(void) strcat(buf, s->string);
			return(buf);
		}
	}

	/* Now expand control characters if required */
	
	if (iscntrl(key) && form != SK_NONE && !SYMFORM(form)) {
		/* Print this control character as required */

		(void) sprintf(buf + strlen(buf), "%s%s", READFORM(form)
			       ? READ_PREFIX : "", KEYFORM(form)
			       ? CTRL_PREFIX : SHOW_PREFIX);
		key = (KEYFORM(form)) ? mklower(UNCTRL(key)) : UNCTRL(key);
	}

	/* Check for quoted characters in read format */

	if (SYMFORM(form) && (!isascii(key) || iscntrl(key)
			      || strchr(SYM_QUOTE, key))
	    || READFORM(form) && strchr(STR_QUOTE, key)) {
		/* We need to quote this character */

		(void) strcat(buf, READ_PREFIX);
	}

	/* Now append the key to the buffer */

	(void) sprintf(buf + strlen(buf), "%c", key);

	/* Return the buffer */

	return(buf);
}
/****************************************************************************/
