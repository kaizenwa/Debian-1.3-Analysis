/* Regex.h - Declarations for POSIX regular expression package
   Copyright (C) 1996 Malc Arnold.

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
/* RCS info. */

#define REGEXID	"$Id: regex.h,v 1.2 1996/12/27 16:49:40 malc Exp $"

/****************************************************************************/
/* The type of an offset into the string to match */

typedef int regoff_t;

/****************************************************************************/
/* The type returned by the regex matching functions */

typedef struct {
	regoff_t rm_so, rm_eo;	/* Start and end of match in the string */
} regmatch_t;

/****************************************************************************/
/* The type of an atom in a regex */

typedef struct __regatom {
	regoff_t (*ra_matchfunc)();	/* Function to match the atom */
	char *ra_value;			/* Literal or class to match */
	int ra_subexpr;			/* Number of related subexpression */
	int ra_length;			/* Length of this subexpression */
	int ra_min, ra_max;		/* Minimum and maximum values */
	struct __regatom *ra_ref;	/* Atom referred to by this one */
	struct __regatom *ra_prev;	/* Previous atom in the list */
	struct __regatom *ra_next;	/* Next atom in the list */
} __regatom_t;

/****************************************************************************/
/* The type of a compiled regex */

typedef struct regex {
	__regatom_t *re_atoms;		/* The atoms of the regex */
	char re_char;			/* Fixed character to match */
	int re_offset, re_minlength;	/* Fixed offset and min length */
	int re_nsub, re_nrpt;		/* No of subexprs and repeats */
	int re_notbol, re_noteol;	/* Notbol or noteol options active */
	int re_nosub, re_icase;		/* Nosub or icase options active */
	__regatom_t **re_saved;		/* Array of regexp's saved atoms */
} regex_t;

/****************************************************************************/
/* Declarations of the global regex functions */

extern int regcomp(), regexec(), regerror();
extern void regfree();

/****************************************************************************/
/* The maximum value of a bound, if not already defined */

#ifndef RE_DUP_MAX
#define RE_DUP_MAX	255
#endif /* ! RE_DUP_MAX */

/****************************************************************************/
/* Flags which can be passed to regcomp */

#define REG_EXTENDED	0x01
#define REG_ICASE	0x02
#define REG_NOSUB	0x04
#define REG_NEWLINE	0x08
#define REG_POSIX	0x10

/****************************************************************************/
/* Flags which can be passed to regexec */

#define REG_NOTBOL	0x01
#define REG_NOTEOL	0x02

/****************************************************************************/
/* Values which can be returned by regcomp or regexec */

#define REG_NOMATCH	1
#define REG_BADBR	2
#define REG_BADPAT	3
#define REG_BADRPT	4
#define REG_ECOLLATE	5
#define REG_ECTYPE	6
#define REG_EESCAPE	7
#define REG_ESUBREG	8
#define REG_EBRACK	9
#define REG_EPAREN	10
#define REG_EBRACE	11
#define REG_ERANGE	12
#define REG_ESPACE	13

/****************************************************************************/
