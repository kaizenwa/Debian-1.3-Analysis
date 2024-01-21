/* Functions.h - Function structure and value definitions for af
   Copyright (C) 1995, 1996, 1997 Malc Arnold.

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

#define FUNCTIONID	"$Id: functions.h,v 1.3 1997/03/05 21:23:45 malc Exp $"

/****************************************************************************/
/* The possible types of a form */

#define FT_LIST		0x01
#define FT_STRING	0x02
#define FT_NUMBER	0x04
#define FT_SYMBOL	0x08
#define FT_ERRORED	0x10

/* And flags to mark optional or repeating arguments */

#define FT_OPTIONAL	0x20
#define FT_REPEATS	0x40

/* And convenient values for argument specifiers */

#define FT_ANY		0x0f
#define FT_NULL		0x00

/****************************************************************************/
/* Useful macros to detect certain form values */

#define NIL(f)		((f)->type == FT_LIST && (f)->value.list == NULL)
#define A(f)		((f)->type == FT_SYMBOL && (f)->value.atom->len == 1 \
			 && (f)->value.atom->keys[0] == 'a')
#define ERRORED(f)	((f)->type == FT_ERRORED)

/****************************************************************************/
/* The internal representation of a form */

typedef struct form {
	unsigned short type;		/* The type of this form */
	union {
		struct form *list;	/* List value of the form */
		KEYSEQ *atom;		/* Atomic value of the form */
	} value;
	struct form *next;		/* The next form to evaluate */
} FORM;

/****************************************************************************/
/* The type of a function's argument spec */

typedef unsigned short ARGSPEC;

/****************************************************************************/
/* The internal representation of a function */

typedef struct {
	char *name;			/* The name of the function */
	FORM *(*func)();		/* The function to call */
	ARGSPEC *args;			/* The arguments to the function */
	unsigned special : 1;		/* Special function flag */
} FUNCTION;

/****************************************************************************/
/* And the internal representation of a constant */

typedef struct {
	char *name;			/* The name of the constant */
	FORM *(*evalfunc)();		/* How to evaluate the constant */
} CONSTANT;

/****************************************************************************/
/* The function used for quoting forms */

#define QUOTE_FUNC	"quote"

/****************************************************************************/
/* Function handlers defined in functions.c */

FORM *f_set(), *f_setq(), *f_defkey(), *f_defmac();
FORM *f_quote(), *f_progn(), *f_if(), *f_and(), *f_or();
FORM *f_not(), *f_equal(), *f_message(), *f_error();
FORM *f_getenv(), *f_system();

/* Constant handlers defined in functions.c */

FORM *c_t(), *c_a(), *c_nil(), *c_errored(), *c_version();

/****************************************************************************/
