/* Regex.c - POSIX regular expression matching routines.
   Copyright (C) 1996, 1997 Malc Arnold.

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
#include "regex.h"

/* Handle which string header we include */

#ifdef HAVE_STRING_H
#include <string.h>
#else /* ! HAVE_STRING_H */
#include <strings.h>
#ifndef strchr
#define strchr(s, c) index(s, c)
#endif /* ! strchr */
#endif /* ! HAVE_STRING_H */

/****************************************************************************/
/* RCS info. */

#ifndef lint
static char *RcsId = "$Id: regex.c,v 1.4 1997/03/31 18:31:32 malc Exp $";
static char *RegexId = REGEXID;
#endif /* ! lint */

/****************************************************************************/
/*LINTLIBRARY*/
/****************************************************************************/
/* The number of characters in a compile table */

#define NO_CHARS	256

/****************************************************************************/
/* The characters that are valid within a word */

#define WORD_CHARS	"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
abcdefghijklmnopqrstuvwxyz0123456789"

/****************************************************************************/
/* The ubiquitous boolean values */

#ifndef TRUE
#define TRUE 		1
#define FALSE 		0
#endif /* TRUE */

/****************************************************************************/
/* Values returned when matching atoms */

#define NOMATCH		-1
#define ERROR		-2

/****************************************************************************/
/* Global function declarations */

extern char *malloc();
extern void free();

/* Functions used to compile expressions */

static char *compile_quoted(), *compile_char(), *compile_any();
static char *compile_class(), *compile_word_class();
static char *compile_non_word_class(), *compile_beginning();
static char *compile_end(), *compile_word_start();
static char *compile_word_end(), *compile_word_boundary();
static char *compile_non_word_boundary(), *compile_subexpr();
static char *compile_backref(), *compile_alternative();
static char *compile_repeat(), *compile_bound();
static char *end_class(), *end_subexpr(), *end_bound();

/* Functions used to execute expressions */

static regoff_t match_fast_text(), match_text(), match_atoms();
static regoff_t match_char(), match_any(), match_any_newline();
static regoff_t match_class(), match_neg_class();
static regoff_t match_neg_class_newline();
static regoff_t match_beginning(), match_beginning_newline();
static regoff_t match_end(), match_end_newline();
static regoff_t match_word_start(), match_word_end();
static regoff_t match_word_boundary(), match_non_word_boundary();
static regoff_t match_start_subexpr(), match_end_subexpr();
static regoff_t match_backref(), match_start_alternative();
static regoff_t match_end_alternative(), match_start_repeat();
static regoff_t match_end_repeat();

/* Other local functions */

static char *(*compile_func())();
static int minlength(), read_bound(), compile_len();
static void init_compile_tables(), find_fixed();
static void clear_matches(), restore_matches();
static __regatom_t **build_save_array();
static __regatom_t *add_atom(), *insert_atom();
static regmatch_t *find_match(), *save_matches();

/****************************************************************************/
/* The compilation tables used while compiling expressions */

static char *(**compile_table)() = NULL;
static char *(**quoted_table)() = NULL;

/****************************************************************************/
/* The regular expression we're executing */

static regex_t *regex = NULL;

/****************************************************************************/
/* Variables used while compiling a pattern */

static __regatom_t *atoms = NULL;
static __regatom_t *last = NULL;
static int no_subexpressions = 0;
static int no_repeats = 0;
static int reg_error = 0;

/****************************************************************************/
int regcomp(expr, pattern, flags)
regex_t *expr;
char *pattern;
int flags;
{
	/* Compile the regular expression pattern into expr */

	char *(*compile[NO_CHARS])();
	char *(*quoted[NO_CHARS])();
	char *pos;

	/* First initialise the compile tables */

	init_compile_tables(compile, quoted, flags);

	/* Initialise the expression we're compiling */

	expr->re_atoms = NULL;
	expr->re_char = '\0';
	expr->re_offset = expr->re_minlength = 0;
	expr->re_nsub = expr->re_nrpt = 0;
	expr->re_notbol = expr->re_noteol = 0;
	expr->re_nosub = (flags & REG_NOSUB) ? 1 : 0;
	expr->re_icase = (flags & REG_ICASE) ? 1 : 0;
	expr->re_saved = NULL;

	/* Now initialise the static values */

	atoms = last = NULL;
	no_subexpressions = 0;
	no_repeats = 0;

	/* Now compile the expression */

	pos = pattern;
	while (pos != NULL && *pos != '\0') {
		/* Compile this atom into the expression */

		pos = compile_table[(unsigned char) *pos](pos, flags);
	}

	/* Set the expression's atom list and item counts */

	expr->re_atoms = atoms;
	expr->re_nsub = no_subexpressions;
	expr->re_nrpt = no_repeats;

	/* Calculate the expression's minimum length */

	expr->re_minlength = minlength(expr->re_atoms, NULL);

	/* Try to find a fixed character we can match */

	find_fixed(expr);

	/* And set up the subexpression and repeat save array */

	if (build_save_array(expr) == NULL) {
		/* Error allocating the array */

		pos = NULL;
	}

	/* Now return the status of the compile */

	return((pos == NULL) ? reg_error : 0);
}
/****************************************************************************/
static void init_compile_tables(compile, quoted, flags)
char *(**compile)(), *(**quoted)();
int flags;
{
	/* Set up the arrays which control compiling expressions */

	int a;

	/* Set the static pointers to refer to the buffers */

	compile_table = compile;
	quoted_table = quoted;

	/* Default all characters to literal handling */

	for (a = 0; a < NO_CHARS; a++) {
		compile[a] = quoted[a] = compile_char;
	}

	/* Now set up the handlers for special characters */

	compile['\\'] = compile_quoted;
	compile['.'] = compile_any;
	compile['['] = compile_class;
	compile['^'] = compile_beginning;
	compile['$'] = compile_end;
	compile['*'] = compile_repeat;

	/* Handle additional repeats if permissible */

	if ((flags & REG_EXTENDED) || !(flags & REG_POSIX)) {
		compile['+'] = compile_repeat;
		compile['?'] = compile_repeat;
	}

	/* Handle ranges if permissible */

	if (flags & REG_EXTENDED) {
		compile['{'] = compile_bound;
	} else if (!(flags & REG_POSIX)) {
		quoted['{'] = compile_bound;
	}

	/* Handle subexpressions and alternatives */

	if (flags & REG_EXTENDED) {
		compile['('] = compile_subexpr;
		compile['|'] = compile_alternative;
	} else {
		quoted['('] = compile_subexpr;
		quoted['|'] = compile_alternative;
	}

	/* Handle back references if permissible */

	if (!(flags & REG_EXTENDED) || !(flags & REG_POSIX)) {
		for (a = '1'; a <= '9'; a++) {
			quoted[a] = compile_backref;
		}
	}

	/* Handle extensions to POSIX if permissible */

	if (!(flags & REG_POSIX)) {
		quoted['w'] = compile_word_class;
		quoted['W'] = compile_non_word_class;
		quoted['<'] = compile_word_start;
		quoted['>'] = compile_word_end;
		quoted['b'] = compile_word_boundary;
		quoted['B'] = compile_non_word_boundary;
	}

	/* Now handle cases that must be errors */

	compile[']'] = end_class;

	if (flags & REG_EXTENDED) {
		compile[')'] = end_subexpr;
		compile['}'] = end_bound;
	} else {
		quoted[')'] = end_subexpr;
		quoted['}'] = end_bound;
	}

	/* That's the tables initialised */

	return;
}
/****************************************************************************/
static char *(*compile_func(pos))()
char *pos;
{
	/* Return the compile function which will be called on pos */

	return((compile_table[(unsigned char) *pos] == compile_quoted)
	       ? quoted_table[(unsigned char) *(pos + 1)]
	       : compile_table[(unsigned char) *pos]);
}
/****************************************************************************/
static int compile_len(pos)
char *pos;
{
	/* Return the length of the compilation atom at pos */

	return((*pos == '\\' && *(pos + 1) != '\0') ? 2 : 1);
}
/****************************************************************************/
static char *compile_quoted(pos, flags)
char *pos;
int flags;
{
	/* Compile a quoted character into the expression */

	/* Check we have a quoted character */

	if (*(pos + 1) == '\0') {
		/* Invalid trailing quote */

		reg_error = REG_EESCAPE;
		return(NULL);
	}

	/* Now evaluate the quoted character */

	return(quoted_table[(unsigned char) *(pos + 1)](pos + 1, flags));
}
/****************************************************************************/
static char *compile_char(pos, flags)
char *pos;
int flags;
{
	/* Compile a literal character into the expression */

	char class[3];

	/* Handle the character as a class if ignoring case */

	if (flags & REG_ICASE && isascii(*pos) && islower(*pos)) {
		/* Generate a class containing the character */

		(void) sprintf(class, "%c%c\n", *pos, toupper(*pos));
		atoms = add_atom(atoms, match_class, class, 2, 1);
	} else if (flags & REG_ICASE && isascii(*pos) && isupper(*pos)) {
		/* Generate a class containing the character */

		(void) sprintf(class, "%c%c\n", tolower(*pos), *pos);
		atoms = add_atom(atoms, match_class, class, 2, 1);
	} else {
		/* Simply add the character to be matched */

		atoms = add_atom(atoms, match_char, pos, 1, 1);
	}
	return((atoms != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
static char *compile_any(pos, flags)
char *pos;
int flags;
{
	/* Compile "match any character" into the expression */

	atoms = add_atom(atoms, !(flags & REG_NEWLINE) ? match_any :
			 match_any_newline, pos, 1, 1);
	return((atoms != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
/*ARGSUSED*/
static char *compile_class(pos, flags)
char *pos;
int flags;
{
	/*
	 * Compile a character class into the expression.
	 * Doesn't handle the [:, [., or [= escapes yet,
	 * but then, it doesn't handle locales either.
	 */

	char class[NO_CHARS], *start;
	char cstring[NO_CHARS + 1], *p;
	int negated, c, from = '\0';

	/* Remove all characters from the class */

	for (c = 0; c < NO_CHARS; c++) {
		class[c] = 0;
	}

	/* Check if the class is negated */

	negated = (*(pos + 1) == '^');

	/* Get the start of the characters in the class */

	pos = start = (negated) ? pos + 2 : pos + 1;

	/* Now scan the characters in the class */

	while (*pos != '\0' &&
	       (pos == start || compile_func(pos) != end_class)) {
		/* Handle a range of characters */

		if (pos > start && *pos == '-' &&
		    compile_func(pos + 1) != end_class) {
			/* Move on to the end of the range */

			pos++;

			/* Check we have a valid range */

			if (from == '\0' || from > (unsigned char) *pos) {
				reg_error = REG_ERANGE;
				return(NULL);
			}

			/* Add the range to the class */

			for (c = from; c <= (unsigned char) *pos; c++) {
				class[c] = 1;
			}

			/* We don't have a valid start-of-range */

			from = '\0';

			/* Move on to the next character */

			pos++;
		} else {
			/* Add this character to the class */

			class[(unsigned char) *pos] = 1;

			/* And store this character for a range */

			from = *pos++;
		}
	}

	/* Check for an error in the class */

	if (compile_func(pos) != end_class) {
		reg_error = REG_EBRACK;
		return(NULL);
	}

	/* Now handle ignoring case in the class */

	for (c = 0; (flags & REG_ICASE) && c < NO_CHARS; c++) {
		/* Case-fold any alphabetic characters */

		if (class[c] && isascii(c) && islower(c)) {
			class[toupper(c)] = 1;
		}
		if (class[c] && isascii(c) && isupper(c)) {
			class[tolower(c)] = 1;
		}
	}

	/* Build the string of characters in the class */

	p = cstring;
	for (c = 0; c < NO_CHARS; c++) {
		if (class[c]) {
			*p++ = c;
		}
	}
	*p = '\0';

	/* And add the class to the expression */

	atoms = add_atom(atoms, (negated) ? match_neg_class :
			 match_class, cstring, strlen(cstring), 1);
	return((atoms != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
/*ARGSUSED*/
static char *compile_word_class(pos, flags)
char *pos;
int flags;
{
	/*
	 * Compile a word constituent class into the expression.
	 * This will need to be rewritten to handle locales.
	 */

	atoms = add_atom(atoms, match_class, WORD_CHARS,
			 strlen(WORD_CHARS), 1);
	return((atoms != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
static char *compile_non_word_class(pos, flags)
char *pos;
int flags;
{
	/*
	 * Compile a negated word constituent class into the
	 * expression.  This will need to be rewritten to handle
	 * locales properly.
	 */

	atoms = add_atom(atoms, !(flags & REG_NEWLINE) ? match_neg_class :
			 match_neg_class_newline, WORD_CHARS,
			 strlen(WORD_CHARS), 1);
	return((atoms != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
static char *compile_beginning(pos, flags)
char *pos;
int flags;
{
	/* Compile beginning of line into the expression */

	if (!(flags & REG_EXTENDED) && last != NULL
	    && last->ra_matchfunc != match_start_subexpr) {
		/* Treat this character as a literal match */

		return(compile_char(pos, flags));
	}

	/* Actually match the beginning of the line */

	atoms = add_atom(atoms, !(flags & REG_NEWLINE) ? match_beginning :
			 match_beginning_newline, NULL, 0, 0);
	return((atoms != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
static char *compile_end(pos, flags)
char *pos;
int flags;
{
	/* Compile end of line into the expression */

	if (!(flags & REG_EXTENDED) && *(pos + 1) != '\0'
	    && compile_func(pos + 1) != end_subexpr) {
		/* Treat this character as a literal match */

		return(compile_char(pos, flags));
	}

	/* Actually match the end of the line */

	atoms = add_atom(atoms, !(flags & REG_NEWLINE) ? match_end :
			 match_end_newline, NULL, 0, 0);
	return((atoms != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
/*ARGSUSED*/
static char *compile_word_start(pos, flags)
char *pos;
int flags;
{
	/* Compile start of word into the expression */

	atoms = add_atom(atoms, match_word_start, NULL, 0, 0);
	return((atoms != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
/*ARGSUSED*/
static char *compile_word_end(pos, flags)
char *pos;
int flags;
{
	/* Compile end of word into the expression */

	atoms = add_atom(atoms, match_word_end, NULL, 0, 0);
	return((atoms != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
/*ARGSUSED*/
static char *compile_word_boundary(pos, flags)
char *pos;
int flags;
{
	/* Compile word boundary into the expression */

	atoms = add_atom(atoms, match_word_boundary, NULL, 0, 0);
	return((atoms != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
/*ARGSUSED*/
static char *compile_non_word_boundary(pos, flags)
char *pos;
int flags;
{
	/* Compile not word boundary into the expression */

	atoms = add_atom(atoms, match_non_word_boundary, NULL, 0, 0);
	return((atoms != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
static char *compile_subexpr(pos, flags)
char *pos;
int flags;
{
	/* Compile a subexpression into the expression */

	int subexpr = 0;
	__regatom_t *start;

	/* Store the number of the current subexpression */

	subexpr = ++no_subexpressions;

	/* Add the start of subexpression as a new atom */

	if ((atoms = add_atom(atoms, match_start_subexpr,
			      NULL, 0, 0)) == NULL) {
		/* Out of memory compiling the expression */

		return(NULL);
	}

	/* Store the start of the subexpression */

	start = last;

	/* Skip the start of the subexpression */

	pos++;

	/* Now compile the expression */

	while (pos != NULL && *pos != '\0' &&
	       compile_func(pos) != end_subexpr) {
		/* Compile this atom into the expression */

		pos = compile_table[(unsigned char) *pos](pos, flags);
	}

	/* Check for an error parsing the subexpression */

	if (pos == NULL || compile_func(pos) != end_subexpr) {
		/* Set the error status and return */

		reg_error = (pos != NULL) ? REG_EPAREN : reg_error;
		return(NULL);
	}

	/* Add the end of subexpression as a new atom */

	if ((atoms = add_atom(atoms, match_end_subexpr,
			      NULL, 0, 0)) == NULL) {
		/* Out of memory compiling the expression */

		return(NULL);
	}

	/* Now set up the subexpression's details */

	start->ra_subexpr = subexpr;
	start->ra_ref = last;
	last->ra_subexpr = subexpr;
	last->ra_ref = start;

	/* And return the position in the expression */

	return(pos + compile_len(pos));
}
/****************************************************************************/
/*ARGSUSED*/
static char *compile_backref(pos, flags)
char *pos;
int flags;
{
	/* Compile a back reference into the expression */

	__regatom_t *atom = atoms;

	/* First add the back reference as an atom */

	if ((atoms = add_atom(atoms, match_backref, NULL, 0, -1)) == NULL) {
		/* Out of memory compiling the expression */

		return(NULL);
	}		
	last->ra_subexpr = (*pos - '0');

	/* Check the subexpression can be back referenced */

	while (atom != NULL && (atom->ra_matchfunc != match_end_subexpr ||
				atom->ra_subexpr != last->ra_subexpr)) {
		/* Move on to the next atom */

		atom = atom->ra_next;
	}

	/* Set up the error, just in case */

	reg_error = REG_ESUBREG;

	/* And return the position in the expression */

	return((atom != NULL) ? pos + 1 : NULL);
}
/****************************************************************************/
static char *compile_alternative(pos, flags)
char *pos;
int flags;
{
	/* Compile an alternative into the expression */

	__regatom_t *atom, *start;
	__regatom_t *alternative;

	/* Find the start of the first alternative */

	atom = last;
	while (atom != NULL && atom->ra_matchfunc != match_start_subexpr
	       && atom->ra_matchfunc != match_start_alternative) {
		/* Move to the previous logical atom in the expression */

		atom = (atom->ra_matchfunc == match_end_subexpr) ?
			atom->ra_ref->ra_prev : atom->ra_prev;
	}

	/* Check if we have consecutive alternates */

	while (atom != NULL && atom->ra_matchfunc ==
	       match_start_alternative) {
		/* We have consecutive alternates */

		atom = atom->ra_prev;
	}

	/* Ensure that subexpressions have higher precedence */

	atom = (atom != NULL && atom->ra_matchfunc == match_start_subexpr)
		? atom->ra_next : atom;

	/* Check if we've found the start of the alternative */

	start = (atom != NULL) ? atom : atoms;

	/* Insert a 'start of alternative' before the LHS */

	if ((atoms = insert_atom(atoms, start, match_start_alternative,
				 NULL, 0, -1)) == NULL) {
		/* Out of memory compiling the expression */

		return(NULL);
	}		
	start = (start != NULL) ? start->ra_prev : atoms;

	/* Add and set up the end of the first alternative */

	if ((atoms = add_atom(atoms, match_end_alternative,
			     NULL, 0, -1)) == NULL) {
		/* Out of memory compiling the expression */

		return(NULL);
	}		
	start->ra_ref = alternative = last;

	/* Now skip to the start of the next expression */

	pos++;

	/* And compile the second alternative */

	while (pos != NULL && *pos != '\0' && compile_func(pos) !=
	       end_subexpr && compile_func(pos) != compile_alternative) {
		/* Compile this atom into the expression */

		pos = compile_table[(unsigned char) *pos](pos, flags);
	}

	/* Add and set up the end of the second alternative */

	if ((atoms = add_atom(atoms, match_end_alternative,
			      NULL, 0, -1)) == NULL) {
		/* Out of memory compiling the expression */

		return(NULL);
	}
	alternative->ra_ref = last;

	/* And return the updated position */

	return(pos);
}
/****************************************************************************/
static char *compile_repeat(pos, flags)
char *pos;
int flags;
{
	/* Compile a repeat into the expression */

	__regatom_t *start;

	/* Check that this repeat is valid */

	if (last == NULL || last->ra_length <= 0
	    && last->ra_matchfunc != match_end_subexpr) {
		/* An error for extended, or a char match for basic REs */

		reg_error = REG_BADRPT;
		return((!(flags & REG_EXTENDED) &&
			(last == NULL || last->ra_matchfunc
			 != match_end_repeat))
		       ? compile_char(pos, flags) : NULL);
	}

	/* Update the repeat count for this expression */

	no_repeats++;

	/* Extract the last subexpresssion from the expression */

	start = (last->ra_matchfunc == match_end_subexpr)
		? last->ra_ref : last;

	/* Add and set up the start of the repeat */

	if ((atoms = insert_atom(atoms, start, match_start_repeat,
				 NULL, 0, -1)) == NULL) {
		/* Out of memory compiling the expression */

		return(NULL);
	}		
	start = (start != NULL) ? start->ra_prev : atoms;
	start->ra_min = (*pos == '+') ? 1 : 0;
	start->ra_max = (*pos == '?') ? 1 : -1;

	/* Add and set up the end of the repeat */

	if ((atoms = add_atom(atoms, match_end_repeat,
			      NULL, 0, -1)) == NULL) {
		/* Out of memory compiling the expression */

		return(NULL);
	}
	last->ra_min = last->ra_max = 0;
	last->ra_ref = start;
	start->ra_ref = last;

	/* And return the updated position */

	return(pos + 1);
}
/****************************************************************************/
/*ARGSUSED*/
static char *compile_bound(pos, flags)
char *pos;
int flags;
{
	/* Compile a bounded repeat into the expression */

	char *p = pos + 1;
	int min, max, have_brace;
	__regatom_t *repeat, *old_last;

	/* Move to the start of the bound's details */

	pos++;

	/* Check if there is a closing brace for the bound */

	while (*p != '\0' && compile_func(p) != end_bound) {
		/* Move on to the next position in the pattern */

		p += compile_len(p);
	}
	have_brace = (*p != '\0');

	/* Set up the minimum number of repeats */

	if ((min = read_bound(&pos)) < 0) {
		/* Invalid bound supplied; error or treat as literal */

		reg_error = (have_brace) ? REG_BADBR : REG_BADRPT;
		return((flags & REG_EXTENDED) ?
		       compile_char(pos - 1, flags) : NULL);
	}
	max = min;

	/* Was a maximum number of repeats supplied? */

	if (*pos == ',') {
		/* Move to the start of the maximum value */

		pos++;

		/* And set up the maximum number of repeats */

		max = read_bound(&pos);
	}

	/* We have a repeat, but is it valid? */

	if (last == NULL || last->ra_length <= 0 &&
	    last->ra_matchfunc != match_end_subexpr) {
		/* This is an error */

		reg_error = REG_BADRPT;
		return(NULL);
	}

	/* Check we've reached the end of the bound */

	if (compile_func(pos) != end_bound) {
		/* Set the error flag and return failure */

		reg_error = (have_brace) ? REG_BADBR : REG_EBRACE;
		return(NULL);
	}

	/* Check the bounds of the repeat for sanity */

	if (max >= 0 && max < min || min > RE_DUP_MAX
	    || max > RE_DUP_MAX) {
		/* These bounds aren't legal */

		reg_error = REG_BADBR;
		return(NULL);
	}

	/* Update the repeat count for this expression */

	no_repeats++;

	/* Extract the last subexpresssion from the expression */

	repeat = (last->ra_matchfunc == match_end_subexpr)
		? last->ra_ref : last;
	old_last = last;
	last = repeat->ra_prev;
	repeat->ra_prev = NULL;
	atoms = (last != NULL) ? atoms : NULL;

	/* Add and set up the start of the repeat */

	if ((atoms = add_atom(atoms, match_start_repeat,
			      NULL, 0, -1)) == NULL) {
		/* Out of memory compiling the expression */

		return(NULL);
	}		
	last->ra_min = min;
	last->ra_max = max;
	last->ra_next = repeat;
	repeat->ra_prev = last;
	repeat = last;
	last = old_last;

	/* Add and set up the end of the repeat */

	if ((atoms = add_atom(atoms, match_end_repeat,
			      NULL, 0, -1)) == NULL) {
		/* Out of memory compiling the expression */

		return(NULL);
	}		
	last->ra_min = last->ra_max = 0;
	last->ra_ref = repeat;
	repeat->ra_ref = last;

	/* And return the updated position */

	return((flags & REG_EXTENDED) ? pos + 1 : pos + 2);
}
/****************************************************************************/
/*ARGSUSED*/
static char *end_class(pos, flags)
char *pos;
int flags;
{
	/* Calling this function means an error in the expression */

	reg_error = REG_EBRACK;
	return(NULL);
}
/****************************************************************************/
/*ARGSUSED*/
static char *end_subexpr(pos, flags)
char *pos;
int flags;
{
	/* Calling this function means an error in the expression */

#ifndef POSIX_BOTCH_FIXED
	/* An error in POSIX implies this should be a char match */

	if (flags & REG_EXTENDED) {
		return(compile_char(pos, flags));
	}
#endif /* ! POSIX_BOTCH_FIXED */

	reg_error = REG_EPAREN;
	return(NULL);
}
/****************************************************************************/
/*ARGSUSED*/
static char *end_bound(pos, flags)
char *pos;
int flags;
{
	/* Calling this function may mean an error in the expression */

#ifndef POSIX_BOTCH_FIXED
	/* An error in POSIX implies this should be a char match */

	if (flags & REG_EXTENDED) {
		return(compile_char(pos, flags));
	}
#endif /* ! POSIX_BOTCH_FIXED */

	reg_error = REG_EBRACE;
	return(NULL);
}
/****************************************************************************/
static __regatom_t *add_atom(atoms, func, value, vlen, len)
__regatom_t *atoms;
regoff_t (*func)();
char *value;
int vlen, len;
{
	/* Add a simple atom to the end of an expression */

	__regatom_t *node = NULL;

	/* Allocate space for the node */

	if ((node = (__regatom_t *) malloc(sizeof(__regatom_t))) == NULL) {
		/* Error allocating space, report the error */

		reg_error = REG_ESPACE;
		return(NULL);
	}

	/* Now initialise the node */

	node->ra_matchfunc = func;
	node->ra_value = (value != NULL) ? malloc(vlen + 1) : NULL;
	node->ra_subexpr = node->ra_min = node->ra_max = 0;
	node->ra_prev = node->ra_next = node->ra_ref = NULL;

	/* Set any value of the node */

	if (value != NULL) {
		(void) strncpy(node->ra_value, value, vlen);
		node->ra_value[vlen] = '\0';
	}

	/* And set up the length of the node */

	node->ra_length = len;

	/* Add the atom to the list */

	if (atoms != NULL && last != NULL) {
		last->ra_next = node;
	}
	node->ra_prev = last;
	last = node;

	/* And return the updated list */

	return((atoms != NULL) ? atoms : node);
}
/****************************************************************************/
static __regatom_t *insert_atom(atoms, pos, func, value, vlen, len)
__regatom_t *atoms, *pos;
regoff_t (*func)();
char *value;
int vlen, len;
{
	/* Insert a simple atom before pos in the expression */

	__regatom_t *node = NULL;

	/* If pos is NULL then simply append the node */

	if (pos == NULL) {
		return(add_atom(atoms, func, value, vlen, len));
	}

	/* Allocate space for the node */

	if ((node = (__regatom_t *) malloc(sizeof(__regatom_t))) == NULL) {
		/* Error allocating space, report the error */

		reg_error = REG_ESPACE;
		return(NULL);
	}

	/* Now initialise the node */

	node->ra_matchfunc = func;
	node->ra_value = (value != NULL) ? malloc(vlen + 1) : NULL;
	node->ra_subexpr = node->ra_min = node->ra_max = 0;

	/* Set any value of the node */

	if (value != NULL) {
		(void) strncpy(node->ra_value, value, vlen);
		node->ra_value[vlen] = '\0';
	}

	/* And set up the length of the node */

	node->ra_length = len;

	/* Insert or prepend the node to the list */

	node->ra_prev = pos->ra_prev;
	node->ra_next = pos;
	if (pos->ra_prev != NULL) {
		pos->ra_prev->ra_next = node;
	}
	pos->ra_prev = node;

	/* And return the updated list */

	return((pos != atoms) ? atoms : node);
}
/****************************************************************************/
static int minlength(atoms, last)
__regatom_t *atoms, *last;
{
	/* Calculate the minimum length of a regular expression */

	int len = 0, lhs, rhs;
	__regatom_t *atom = atoms;

	/* Loop over the atom, adding lengths */

	while (atom != last && atom != NULL) {
		/* An atom's length depends on its type */

		if (atom->ra_matchfunc == match_start_alternative) {
			/* Get the lengths of the alternatives */

			lhs = minlength(atom->ra_next, atom->ra_ref);
			rhs = minlength(atom->ra_ref->ra_next,
					atom->ra_ref->ra_ref);

			/* Use the shorter length */

			len += (lhs < rhs) ? lhs : rhs;

			/* And skip over the alternative */

			atom = atom->ra_ref->ra_ref->ra_next;
		} else if (atom->ra_matchfunc == match_start_repeat) {
			/* Size is minimum repeats times repeat size */

			len += (!(atom->ra_min)) ? 0 : atom->ra_min *
				minlength(atom->ra_next, atom->ra_ref);

			/* And skip over the repeat */

			atom = atom->ra_ref->ra_next;
		} else {
			/* Add the length of this atom to the expression */

			len += (atom->ra_length > 0) ? atom->ra_length : 0;

			/* And move on to the next atom */

			atom = atom->ra_next;
		}
	}

	/* And return the minimum length */

	return(len);
}
/****************************************************************************/
static void find_fixed(expr)
regex_t *expr;
{
	/* Find a fixed character at a constant position in the expression */

	int len = 0;
	__regatom_t *atom;

	/* We haven't found a fixed character yet */

	expr->re_char = '\0';
	expr->re_offset = 0;

	/* Loop over the atoms, calculating lengths */

	for (atom = expr->re_atoms; atom != NULL; atom = atom->ra_next) {
		/* Check if we've found a fixed character or failed */

		if (atom->ra_matchfunc == match_char) {
			/* Set up the fixed match and return */

			expr->re_char = *(atom->ra_value);
			expr->re_offset = len;
			return;
		} else if (atom->ra_length < 0) {
			/* No fixed character in expression */
 
			return;
		}

		/* Update the length and carry on looking */

		len += atom->ra_length;
	}

	/* No fixed character in the expression */

	return;
}
/****************************************************************************/
static __regatom_t **build_save_array(expr)
regex_t *expr;
{
	/*
	 * Set up expr's save array, containing pointers to each
	 * subexpression and repeat atom in the list.  This array
	 * will be used to optimise saving and restoring match
	 * positions when backtracking in an expression.  Returns
	 * a pointer to the save array, or NULL on error.
	 */

	int repeat;
	__regatom_t *atom;

	/* Allocate the save array if required */

	if ((expr->re_saved = (__regatom_t **)
	     malloc((expr->re_nsub + expr->re_nrpt + 1) *
		    sizeof(__regatom_t *))) == NULL) {
		/* Can't allocate space for the array */

		reg_error = REG_ESPACE;
		return(NULL);
	}

	/* Clear the location of the saved total match */

	expr->re_saved[0] = NULL;

	/* Initialise the first repeat atom position */

	repeat = expr->re_nsub + 1;

	/* Loop through the atoms, saving subexpr and repeat positions */

	for (atom = expr->re_atoms; atom != NULL; atom = atom->ra_next) {
		/* Store subexpression and repeat positions */

		if (atom->ra_matchfunc == match_start_subexpr) {
			/* Copy the positions for this subexpression */

			expr->re_saved[atom->ra_subexpr] = atom;
		} else if (atom->ra_matchfunc == match_end_repeat) {
			/* Repeats are identified by order in expression */

			expr->re_saved[repeat++] = atom;
		}
	}

	/* Now return success */

	return(expr->re_saved);
}
/****************************************************************************/
static int read_bound(pos)
char **pos;
{
	/*
	 * Read a numeric bound entry from a string at pos.  As a side
	 * effect, update pos to point at the first character after the
	 * bound entry.  Return -1 and leave pos unchanged on error.
	 */

	char *start = *pos;
	int bound = 0;

	/* Loop over the bound value */

	while (isascii(**pos) && isdigit(**pos)) {
		/* Add this value to the bound */

		bound = bound * 10 + *(*pos)++ - '0';
	}

	/* Return the appropriate values */

	return((*pos == start) ? -1 : bound);
}	
/****************************************************************************/
int regexec(expr, text, nmatch, matches, flags)
regex_t *expr;
char *text;
int nmatch, flags;
regmatch_t *matches;
{
	/* Execute a previously-compiled regular expression */

	int m;

	/* Set the flags on the expression */

	expr->re_notbol = (flags & REG_NOTBOL) ? 1 : 0;
	expr->re_noteol = (flags & REG_NOTEOL) ? 1 : 0;

	/* Store the current expression statically */

	regex = expr;

	/* Initialise the atoms' match positions */

	clear_matches(expr->re_atoms, NULL);

	/* Initialise the match position buffer */

	for (m = 0; matches != NULL && !(expr->re_nosub)
	     && m < nmatch; m++) {
		matches[m].rm_so = matches[m].rm_eo = -1;
	}

	/* We'll assume that any error is a match failure */

	reg_error = REG_NOMATCH;

	/* Now search for the expression and return status */

	return((match_fast_text(expr, text, nmatch, matches) < 0 &&
		match_text(expr, text, nmatch, matches) < 0) ? reg_error : 0);
}
/****************************************************************************/
static regoff_t match_fast_text(expr, text, nmatch, matches)
regex_t *expr;
char *text;
int nmatch;
regmatch_t *matches;
{
	/* Search for a match for atoms in text the fast way */

	char *pos, *lastpos;
	int m;
	regoff_t offset, end;
	regmatch_t *new_matches;

	/* Set up the last possible fast match position */

	if (!(expr->re_char) || (lastpos = text + strlen(text) -
				 regex->re_minlength) < text) {
		/* No possible fast match within text */

		return(NOMATCH);
	}

	/* Set up the search position and offset */

	offset = regex->re_offset;
	pos = strchr(text + offset, regex->re_char);

	/* Search for a possible start position in text */

	while (pos != NULL && pos <= lastpos + offset) {
		/* Do we have a match at this position? */

		if ((end = match_atoms(expr->re_atoms, text,
				       pos - offset)) >= 0) {
			/* We have a match; set up the match position */

			if (matches != NULL && nmatch > 0 &&
			    !(expr->re_nosub)) {
				matches[0].rm_so = pos - offset - text;
				matches[0].rm_eo = end;
			}

			/* Now set up the subexpression match positions */

			if ((new_matches = save_matches(expr)) == NULL) {
				/* Ran out of memory saving the matches */

				reg_error = REG_ESPACE;
				return(ERROR);
			}

			/* Copy the matches into the buffer */

			for (m = 1; matches != NULL && !(expr->re_nosub)
			     && m < nmatch; m++) {
				/* Store this match in the array */

				matches[m].rm_so = (m <= expr->re_nsub)
					? new_matches[m].rm_so : -1;
				matches[m].rm_eo = (m <= expr->re_nsub)
					? new_matches[m].rm_eo : -1;
			}

			/* Clean up and return the end of the match */

			free(new_matches);
			return(end);
		} else if (end == ERROR) {
			/* Out of memory executing the expression */ 

			reg_error = REG_ESPACE;
			return(ERROR);
		}

		/* Search for the next match position */

		pos = strchr(pos + 1, regex->re_char);
	}

	/* No match for the expression found */

	return(NOMATCH);
}
/****************************************************************************/
static regoff_t match_text(expr, text, nmatch, matches)
regex_t *expr;
char *text;
int nmatch;
regmatch_t *matches;
{
	/* Search for a match for expr in text the slow way */

	char *pos, *lastpos;
	int m;
	regoff_t end;
	regmatch_t *new_matches;

	/* Set up the last possible slow match position */

	if (expr->re_char || (lastpos = text + strlen(text) -
			      regex->re_minlength) < text) {
		/* No possible match within text */

		return(NOMATCH);
	}

	/* Loop through text looking for a match */

	for (pos = text; pos <= lastpos; pos++) {
		/* Do we have a match at this position? */

		if ((end = match_atoms(expr->re_atoms, text, pos)) >= 0) {
			/* We have a match; set up the match position */

			if (matches != NULL && nmatch > 0 &&
			    !(expr->re_nosub)) {
				matches[0].rm_so = pos - text;
				matches[0].rm_eo = end;
			}

			/* Now set up the subexpression match positions */

			if ((new_matches = save_matches(expr)) == NULL) {
				/* Ran out of memory saving the matches */

				reg_error = REG_ESPACE;
				return(ERROR);
			}

			/* Copy the matches into the buffer */

			for (m = 1; matches != NULL && !(expr->re_nosub)
			     && m < nmatch; m++) {
				/* Store this match in the array */

				matches[m].rm_so = (m <= expr->re_nsub)
					? new_matches[m].rm_so : -1;
				matches[m].rm_eo = (m <= expr->re_nsub)
					? new_matches[m].rm_eo : -1;
			}

			/* Clean up and return the end of the match */

			free(new_matches);
			return(end);
		} else if (end == ERROR) {
			/* Out of memory executing the expression */ 

			reg_error = REG_ESPACE;
			return(ERROR);
		}
	}

	/* No match for the expression found */

	return(NOMATCH);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_atoms(atoms, text, pos)
__regatom_t *atoms;
char *text, *pos;
{
	/* Match atoms against text at pos */

	regoff_t end = (pos - text);
	__regatom_t *atom;

	/* Now try to match the regex at text */

	for (atom = atoms; atom != NULL; atom = atom->ra_next) {
		/* First try to match this atom at pos */

		if ((end = atom->ra_matchfunc(atom, text, pos)) < 0) {
			/* Failed to match the expression */

			return(end);
		}

		/*
		 * Matching an alternative or repeat implicitly
		 * matches the rest of the expression, so abort
		 * after we've matched one of these.
		 */

		if (atom->ra_matchfunc == match_start_repeat ||
		    atom->ra_matchfunc == match_end_repeat ||
		    atom->ra_matchfunc == match_start_alternative) {
			/* Finished executing the expression */

			return(end);
		}

		/*
		 * If we encounter an end alternative with a reference
		 * atom, then it's the end of the LHS of an alternative.
		 * In this case, skip the alternative's RHS.
		 */

		if (atom->ra_matchfunc == match_end_alternative
		    && atom->ra_ref != NULL) {
			atom = atom->ra_ref;
		}

		/* Move pos to the position after the match */

		pos = text + end;
	}

	/* Return the details of the match */

	return(end);
}
/****************************************************************************/
static regoff_t match_char(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match a literal character at pos */

	return((*pos == *(atom->ra_value)) ? pos - text + 1 : NOMATCH);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_any(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match any character at pos */

	return((*pos != '\0') ? pos - text + 1 : NOMATCH);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_any_newline(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match any character except newline at pos */

	return((*pos != '\0' && *pos != '\n') ? pos - text + 1 : NOMATCH);
}
/****************************************************************************/
static regoff_t match_class(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match pos against a character class */

	return((*pos != '\0' && strchr(atom->ra_value, *pos) != NULL)
	       ? pos - text + 1 : NOMATCH);
}
/****************************************************************************/
static regoff_t match_neg_class(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match pos against a negated character class */

	return((*pos != '\0' && strchr(atom->ra_value, *pos) == NULL)
	       ? pos - text + 1 : NOMATCH);
}
/****************************************************************************/
static regoff_t match_neg_class_newline(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match pos against a negated character class or newline */

	return((*pos != '\0' && *pos != '\n' &&
		strchr(atom->ra_value, *pos) == NULL)
	       ? pos - text + 1 : NOMATCH);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_beginning(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match the null string at the beginning of the line */

	return((!(regex->re_notbol) && pos == text) ? pos - text : NOMATCH);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_beginning_newline(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match the null string at newline or the beginning of the line */

	return((!(regex->re_notbol) && pos == text || pos > text
		&& *(pos - 1) == '\n') ? pos - text : NOMATCH);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_end(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match the null string at the end of the line */

	return((!(regex->re_noteol) && *pos == '\0') ? pos - text : NOMATCH);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_end_newline(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match the null string at newline or the end of the line */

	return((!(regex->re_noteol) && *pos == '\0'
		|| *pos == '\n') ? pos - text : NOMATCH);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_word_start(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match the null string at the start of a word */

	int pos_word, prev_word;

	/* Check if pos and the previous character are word chars */

	pos_word = (isascii(*pos) && isalnum(*pos));
	prev_word = (pos != text && isascii(*(pos - 1))
		     && isalnum(*(pos - 1)));

	/* Return the end position if we're at start-of-word */

	return((pos_word && !prev_word) ? pos - text : NOMATCH);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_word_end(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match the null string at the end of a word */

	int pos_word, prev_word;

	/* Check if pos and the previous characters are word chars */

	pos_word = (isascii(*pos) && isalnum(*pos));
	prev_word = (pos != text && isascii(*(pos - 1))
		     && isalnum(*(pos - 1)));

	/* Return the end position if we're at end-of-word */

	return((!pos_word && prev_word) ? pos - text : NOMATCH);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_word_boundary(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match the null string at the start or end of a word */

	int pos_word, prev_word, next_word;

	/* Check if pos and the surrounding characters are word chars */

	pos_word = (isascii(*pos) && isalnum(*pos));
	prev_word = (pos != text && isascii(*(pos - 1))
		     && isalnum(*(pos - 1)));
	next_word = (*pos != '\0' && isascii(*(pos + 1))
		     && isalnum(*(pos + 1)));

	/* Return the end position if we're at a word boundary */

	return((pos_word && (!prev_word || !next_word) ||
		!pos_word && (prev_word || next_word))
	       ? pos - text : NOMATCH);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_non_word_boundary(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match the null string anywhere but the start or end of a word */

	int pos_word, prev_word, next_word;

	/* Check if pos and the surrounding characters are word chars */

	pos_word = (isascii(*pos) && isalnum(*pos));
	prev_word = (pos != text && isascii(*(pos - 1))
		     && isalnum(*(pos - 1)));
	next_word = (*pos != '\0' && isascii(*(pos + 1))
		     && isascii(*(pos + 1)));

	/* Return the end position if we're at a word boundary */

	return((pos_word && prev_word && next_word ||
		!pos_word && !prev_word && !next_word)
	       ? pos - text : NOMATCH);
}
/****************************************************************************/
static regoff_t match_start_subexpr(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Initialise before matching a subexpression */

	atom->ra_min = atom->ra_ref->ra_min = pos - text;
	atom->ra_max = atom->ra_ref->ra_max = -1;
	return(pos - text);
}
/****************************************************************************/
static regoff_t match_end_subexpr(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Store the match position after matching a subexpression */

	atom->ra_max = atom->ra_ref->ra_max = pos - text;
	return(pos - text);
}
/****************************************************************************/
static regoff_t match_backref(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match a back reference to a subexpression against pos */

	char *matchtext;
	int matchlen;
	regmatch_t *match;

	/* Get the subexpression's match position */

	match = find_match(atom->ra_subexpr);

	/* Check the subexpression was matched */

	if (match->rm_so < 0 || match->rm_eo < 0) {
		/* Can't match an unmatched subexpression */

		return(NOMATCH);
	}

	/* Get the start and length of the text to match */

	matchtext = text + match->rm_so;
	matchlen = match->rm_eo - match->rm_so;

	/* Now match the subexpression at pos */

	return((regex->re_icase && !strncasecmp(pos, matchtext, matchlen)
		|| !(regex->re_icase) && !strncmp(pos, matchtext, matchlen))
	       ? (pos - text) + matchlen : NOMATCH);
}
/****************************************************************************/
static regoff_t match_start_alternative(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match one of two alternatives against pos */

	int lhs_preferred;
	regoff_t lhs_end = NOMATCH, rhs_end = NOMATCH;
	regmatch_t *matches, *lhs_matches = NULL;

	/* Initialise the alternative match positions */

	atom->ra_max = atom->ra_ref->ra_ref->ra_max = -1;

	/* Match the first alternative and save the matches */

	if ((matches = save_matches(regex)) == NULL ||
	    (lhs_end = match_atoms(atom->ra_next, text, pos)) >= 0
	    && (lhs_matches = save_matches(regex)) == NULL
	    || lhs_end == ERROR) {
		/* Out of memory executing the expression */

		if (matches != NULL) {
			free(matches);
		}
		reg_error = REG_ESPACE;
		return(ERROR);
	}

	/* Restore the original match positions */

	restore_matches(regex, matches);

	/* And match the second alternative */

	if ((rhs_end = match_atoms(atom->ra_ref->ra_next,
				   text, pos)) == ERROR) {
		/* Out of memory executing the expression */

		free(matches);
		if (lhs_matches != NULL) {
			free(lhs_matches);
		}
		reg_error = REG_ESPACE;
		return(ERROR);
	}

	/* Now decide which alternative we prefer */

	lhs_preferred = (lhs_end > rhs_end || lhs_end == rhs_end &&
			 atom->ra_ref->ra_max > atom->ra_ref->ra_ref->ra_max); 

	/* Restore the first match positions if required */

	if (lhs_end >= 0 && lhs_preferred) {
		/* Clear the RHS matches and restore the LHS */

		clear_matches(atom->ra_ref->ra_next, atom->ra_ref->ra_ref);
		restore_matches(regex, lhs_matches);
	}

	/* Clear any matches in the unmatched side(s) */

	if (lhs_end < 0 || !lhs_preferred) {
		/* Clear any matches in the LHS */

		clear_matches(atom->ra_next, atom->ra_ref);
	}
	if (rhs_end < 0 || lhs_preferred) {
		/* Clear any matches in the RHS */

		clear_matches(atom->ra_ref->ra_next, atom->ra_ref->ra_ref);
	}

	/* Free any saved match positions */

	free(matches);
	if (lhs_matches != NULL) {
		free(lhs_matches);
	}

	/* Now return the end position */

	return((lhs_end >= rhs_end) ? lhs_end : rhs_end);
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_end_alternative(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Set the position at the end of an alternative */

	atom->ra_max = pos - text;

	/* And return the position unchanged */

	return(pos - text);
}
/****************************************************************************/
static regoff_t match_start_repeat(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Match a repeated subexpression against pos */

	/* Reset the last position and maximum repeat count */

	atom->ra_ref->ra_min = pos - text;
	atom->ra_ref->ra_max = 0;

	/* And match the repeat from the end (to allow zero repeats) */

	return(match_end_repeat(atom->ra_ref, text, pos));
}
/****************************************************************************/
/*ARGSUSED*/
static regoff_t match_end_repeat(atom, text, pos)
__regatom_t *atom;
char *text, *pos;
{
	/* Check another iteration of a repeated subexpression */

	regoff_t last, zend = NOMATCH, end = NOMATCH;
	regmatch_t *matches = NULL;

	/*
	 * Update the maximum repeat count and last position.
	 * These are stored in the end atom of the repeat, the
	 * maximum and minimum values are still held in the
	 * start atom.
	 */

	last = (atom->ra_max) ? atom->ra_min : -1;
	atom->ra_min = pos - text;
	atom->ra_max++;

	/* Try matching if we don't repeat the loop any more */

	if ((last == atom->ra_min || atom->ra_max > atom->ra_ref->ra_min)
	    && (zend = match_atoms(atom->ra_next, text, pos)) >= 0 &&
	    (matches = save_matches(regex)) == NULL || zend == ERROR) {
		/* Out of memory executing the expression */

		reg_error = REG_ESPACE;
		return(ERROR);
	}

	/* Now attempt to match the body of the repeat once more */

	if (pos != '\0' && last != atom->ra_min &&
	    (atom->ra_ref->ra_max < 0 || atom->ra_max <= atom->ra_ref->ra_max)
	    && (end = match_atoms(atom->ra_ref->ra_next, text, pos)) < zend) {
		/* Check for an error executing the expression */

		if (end == ERROR) {
			/* Out of memory executing the expression */

			if (matches != NULL) {
				free(matches);
			}
			reg_error = REG_ESPACE;
			return(ERROR);
		}

		/* No valid match, restore the match positions */

		if (matches != NULL) {
			restore_matches(regex, matches);
		}
	}

	/* Free any saved match positions */

	if (matches != NULL) {
		free(matches);
	}

	/* Now return whichever match was longer */

	return((zend > end) ? zend : end);
}
/****************************************************************************/
static regmatch_t *find_match(subexpr)
int subexpr;
{
	/* Return the match positions of subexpr */

	static regmatch_t match = { 0, 0 };
	__regatom_t *atom;

	/* Search the atom list for the subexpression */

	for (atom = atoms; atom != NULL; atom = atom->ra_next) {
		/* Is this the subexpression we want? */

		if (atom->ra_subexpr == subexpr) {
			/* Return the match position */

			match.rm_so = atom->ra_min;
			match.rm_eo = atom->ra_max;
			return(&match);
		}
	}

	/* No match found for the expression */

	match.rm_so = match.rm_eo = -1;
	return(&match);
}
/****************************************************************************/
static void clear_matches(atoms, last)
__regatom_t *atoms, *last;
{
	/* Clear the match positions of any subexpressions in atoms */

	__regatom_t *atom;

	/* Loop over the atoms, clearing positions */

	for (atom = atoms; atom != NULL && atom != last;
	     atom = atom->ra_next) {
		/* Clear subexpression or end repeat positions */

		if (atom->ra_matchfunc == match_start_subexpr
		    || atom->ra_matchfunc == match_end_subexpr) {
			atom->ra_min = atom->ra_max = -1;
		}
	}

	return;
}		
/****************************************************************************/
static regmatch_t *save_matches(expr)
regex_t *expr;
{
	/* Return an allocated copy of the match positions */

	int save;
	regmatch_t *matches;
	
	/* Allocate the array to hold the match positions */

	if ((matches = (regmatch_t *)
	     malloc((expr->re_nsub + 1 + expr->re_nrpt) *
		    sizeof(regmatch_t))) == NULL) {
		/* Can't allocate space for the array */

		return(NULL);
	}

	/* Initialise the position of the global match */

	matches[0].rm_so = matches[0].rm_eo = -1;

	/* Save the position of the subexpressions and repeats */

	for (save = 1; save < expr->re_nsub + expr->re_nrpt + 1; save++) {
		/* Save the positions for this atom */

		matches[save].rm_so = expr->re_saved[save]->ra_min;
		matches[save].rm_eo = expr->re_saved[save]->ra_max;
	}

	/* And return the saved matches */

	return(matches);
}
/****************************************************************************/
static void restore_matches(expr, matches)
regex_t *expr;
regmatch_t *matches;
{
	/* Restore the match positions stored in matches */

	int save;

	/* Restore the positions of the subexpressions and repeats */

	for (save = 1; save < expr->re_nsub + expr->re_nrpt + 1; save++) {
		/* Save the positions for this atom */

		expr->re_saved[save]->ra_min = matches[save].rm_so;
		expr->re_saved[save]->ra_max = matches[save].rm_eo;
	}

	return;
}
/****************************************************************************/
void regfree(expr)
regex_t *expr;
{
	/* Free a regular expression buffer */

	__regatom_t *atom = expr->re_atoms, *next;

	/* First free the expression's saved array */

	if (expr->re_saved != NULL) {
		free(expr->re_saved);
	}

	/* Loop over and free the expression's atoms */

	while (atom != NULL) {
		/* Get the next atom in the list */

		next = atom->ra_next;

		/* Free this atom */

		if (atom->ra_value != NULL) {
			free(atom->ra_value);
		}
		free(atom);

		/* And move on to the next */

		atom = next;
	}
	return;
}
/****************************************************************************/
/*ARGSUSED*/
int regerror(status, expr, msgbuf, msgsiz)
int status, msgsiz;
regex_t *expr;
char *msgbuf;
{
	/* Store an error message for error in msgbuf */

	static char *reg_messages[] = {
		"no error", "no match found", "invalid repetition count",
		"invalid regular expression", "no operand for repetition",
		"invalid collating element", "invalid character class",
		"trailing backslash", "invalid back reference",
		"unbalanced brackets", "unbalanced parentheses",
		"unbalanced braces", "invalid character range",
		"out of memory"
	};

	/* Fill the message buffer if required */

	if (msgsiz > 0) {
		/* Copy as much as the buffer will hold */

		(void) strncpy(msgbuf, reg_messages[status], msgsiz - 1);
		msgbuf[msgsiz - 1] = '\0';
	}

	/* And return the length of the error message */

	return(strlen(reg_messages[status]));
}
/****************************************************************************/
