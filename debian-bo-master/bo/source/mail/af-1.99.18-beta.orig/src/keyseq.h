/* Keyseq.h - Declarations for af's key sequence handling.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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

#define KEYSEQID	"$Id: keyseq.h,v 1.8 1997/03/31 18:32:19 malc Exp $"

/****************************************************************************/
/* The structure used to store a key sequence */

typedef struct {
	char *keys;			/* The keys in the sequence */
	short len;			/* The length of the sequence */
} KEYSEQ;

/****************************************************************************/
/* The structure used to hold a list of key sequences */

typedef struct seq_list {
	KEYSEQ sequence;		/* The key sequence */
	struct seq_list *next;		/* The next sequence */
} SEQ_LIST;

/****************************************************************************/
/* A macro to extract the last key from a sequence */

#define LASTKEY(s)	(((s) != NULL) ? (s)->keys[(s)->len - 1] : '\0')

/****************************************************************************/
/* Macros for converting between control and printable characters */

#define CTRL(x)		(((x) == '?') ? '\177' : ((x) - '@'))
#define UNCTRL(x)	(((x) == '\177') ? '?' : ((x) + '@'))

/* And a macro to generate a metacharacter */

#define META(x)		((x) | '\200')

/****************************************************************************/
/* Is a character part of of an octal key value? */

#define ISOFDIGIT(x)	((x) == '0' || (x) == '1' || (x) == '2' || (x) == '3')
#define ISODIGIT(x)	(isascii(x) && isdigit(x) && (x) != '8' && (x) != '9')

/* The maximum length of an actal key value */

#define MAXOKEYLEN	3

/****************************************************************************/
/* The maximum length of a single printed character */

#define MAX_KEY_LEN	8

/****************************************************************************/
/* The formats that we can display keys in */

#define SK_NONE		0
#define SK_KEYSEQ	1
#define SK_STRING	2
#define SK_DISPLAY	3
#define SK_HEADER	4
#define SK_READKEY	5
#define SK_READSTR	6
#define SK_READSYM	7
#define SK_READNUM	8

/* And macros for checking the format */

#define READFORM(f)	((f) >= SK_READKEY)
#define KEYFORM(f)	((f) == SK_KEYSEQ || (f) == SK_READKEY)
#define STRFORM(f)	((f) == SK_STRING || (f) == SK_READSTR)
#define SYMFORM(f)	((f) == SK_READSYM || (f) == SK_READNUM)

/****************************************************************************/
/* The prefixes to write before control and meta-character */

#define CTRL_PREFIX	"C-"
#define META_PREFIX	"M-"
#define SHOW_PREFIX	"^"
#define OCTL_PREFIX	"\\"
#define READ_PREFIX	"\\"

/****************************************************************************/
/* The keys that need to be quoted in read format */

#define SYM_QUOTE	" \"'();?"
#define STR_QUOTE	"\""

/****************************************************************************/
/* The type of a list of keys to be handled specially */

typedef struct {
	char key;
	char *string;
} SPECIAL;

/****************************************************************************/
/* The list of specially-printed keys */

#define DISP_KEYS	{ { '\0', "NUL" }, { '\n', "LFD" },\
{ '\r', "RET" }, { '\t', "TAB" }, { CTRL('['), "ESC" },\
{ ' ', "SPC" }, { CTRL('?'), "DEL" }, { '\0', NULL } }

/* The list of special header keys */

#define HDR_KEYS	{ { '\t', " " }, { '\n', "" },\
{ '\r', "" }, { '\0', NULL } }

/* The list of specially-read keys */

#define READ_KEYS	{ { CTRL('G'), "\\a" }, { '\b', "\\b" },\
{ CTRL('['), "\\e" }, { '\f', "\\f" }, { '\n', "\\n" },\
{ '\r', "\\r" }, { '\t', "\\t" }, { CTRL('K'), "\\v" },\
{ '\\', "\\\\" }, { '\0', NULL } }

/****************************************************************************/
