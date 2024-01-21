/* Conversion of files between different charsets and usages.
   Copyright (C) 1990, 1993, 1994 Free Software Foundation, Inc.
   Francois Pinard <pinard@iro.umontreal.ca>, 1988.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if HAVE_LOCALE_H
# include <locale.h>
#else
# define setlocale(Category, Locale)
#endif

#include <libintl.h>
#define _(Text) gettext ((Text))

#include <stdio.h>

#if STDC_HEADERS
# include <stdlib.h>
#endif

#if DIFF_HASH
# ifdef HAVE_LIMITS_H
#  include <limits.h>
# endif
# ifndef CHAR_BIT
#  define CHAR_BIT 8
# endif
#endif

/* Some systems do not define EXIT_*, even with STDC_HEADERS.  */
#ifndef EXIT_SUCCESS
# define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
# define EXIT_FAILURE 1
#endif

#ifndef __P
# if __STDC__
#  define __P(Args) Args
# else
#  define __P(Args) ()
# endif
#endif

void *xmalloc __P ((int));
char *xstrdup __P ((const char *));
int argmatch __P ((const char *, const char *const *));
void error __P ((int, int, const char *, ...));
extern void (*error_print_progname) __P ((void));

/* Description of a charset.  */

typedef const char* DOUBLE_TABLE[8];

typedef struct charset CHARSET;

struct charset
  {
    const char *name;		/* main name */
    int ignore;			/* non zero if should be ignored */
    DOUBLE_TABLE *table;	/* double table for RFC 1345 */
    int size;			/* size of each DOUBLE_TABLE entry */
  };

/* Description of a single step of recoding.  */

enum quality
  {
    REVERSIBLE,			/* reversible one to one recoding */
    ONE_TO_ONE,			/* one character to one */
    ONE_TO_MAYBE,		/* one character to none or one */
    ONE_TO_MANY,		/* one character to none, one or many */
    MANY_TO_ONE,		/* one or many characters to one */
    MANY_TO_MANY		/* one or many characters to one or many */
  };

/* The HP-UX 9.01 compiler says `Unknown size "QUALITY"', if I put the
   typedef before the enum declaration, as I'm used to do.  Sigh!  */
typedef enum quality QUALITY;

typedef struct step STEP;

struct step
  {
    CHARSET *before;		/* charset before conversion */
    CHARSET *after;		/* charset after conversion */
    QUALITY quality;		/* recoding quality */
    void (*init_recode) __P ((STEP *));
    int (*file_recode) __P ((const STEP *, FILE *, FILE *));
    const unsigned char *one_to_one; /* recoding array of 256 chars */
    const char *const *one_to_many; /* recoding array of 256 strings */
    int conversion_cost;	/* cost for this single step only */
  };

typedef struct known_pair KNOWN_PAIR;

struct known_pair
  {
    unsigned char left;		/* first character in pair */
    unsigned char right;	/* second character in pair */
  };

/* Description of list formats.  */

enum list_format
  {
    NO_FORMAT,			/* format not decided yet */
    DECIMAL_FORMAT,		/* concise tabular list using decimal */
    OCTAL_FORMAT,		/* concise tabular list using octal */
    HEXADECIMAL_FORMAT,		/* concise tabular list using hexadecimal */
    FULL_FORMAT			/* full list, one character per line */
  };

/* recode.c.  */

extern int ascii_graphics;
extern char diaeresis_char;
extern int diacritics_only;
extern int strict_mapping;
extern enum list_format list_format;

extern int decoding_charset_flag;
extern const unsigned char *one_to_same;
extern CHARSET *rfc1345;

void usage __P ((int));
const char *quality_to_string __P ((QUALITY));
QUALITY merge_qualities __P ((QUALITY, QUALITY));
void declare_step __P ((const char *, const char *, QUALITY, void (*) (STEP *),
			int (*) (const STEP *, FILE *, FILE *)));
void declare_double_step __P ((DOUBLE_TABLE *, const char *, int));
unsigned char *invert_table __P ((const unsigned char *));
void complete_pairs __P ((STEP *, int, const KNOWN_PAIR *, int, int));
int file_one_to_one __P ((const STEP *, FILE *, FILE *));
int file_one_to_many __P ((const STEP *, FILE *, FILE *));

/* charname.c.  */

char *symbol_to_charname __P ((const char *));

/* charset.c.  */

extern CHARSET charset_array[];
extern int number_of_charsets;

void decode_known_pairs __P ((const char *));
void prepare_charset_initialization __P ((void));
CHARSET *find_charset __P ((const char *));
void declare_alias __P ((const char *, const char *));
void make_argmatch_array __P ((void));
const char *clean_charset_name __P ((const char *));

void list_all_charsets __P ((CHARSET *));
void init_table_for_rfc1345 __P ((STEP *));
void list_concise_charset __P ((CHARSET *));
void list_full_charset __P ((CHARSET *));

/* Debugging the memory allocator.  */

#if WITH_DMALLOC
# define DMALLOC_FUNC_CHECK
# include <dmalloc.h>
#endif
