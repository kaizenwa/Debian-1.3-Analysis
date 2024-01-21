/*
 * Defines for a generic argv and argc processor...
 *
 * Copyright 1993 by Gray Watson and the Antaire Corporation
 *
 * This file is part of the argv library.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose and without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies, and that
 * the name of Antaire not be used in advertising or publicity pertaining to
 * distribution of the document or software without specific, written prior
 * permission.
 *
 * The Antaire Corporation makes no representations about the suitability of
 * the software described herein for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The author may be contacted at gray.watson@antaire.com
 *
 * $Id: argv.h.1,v 1.4 1994/02/28 02:07:13 gray Exp $
 */

#ifndef __ARGV_H__
#define __ARGV_H__

/* Note: I added this -- fpm */
#ifdef __cplusplus
extern "C" {
#endif
  
/*
 * Version string for the library
 */

/*
 * NOTE to gray: whenever this is changed, a corresponding Changlog
 * entry *must* be entered.
 */
/*static	char	*argv_version = "1.2.2";*/

/* Version Date: $Date: 1994/02/28 02:07:13 $ */

/*
 * for those systems who do not understand const define to nothing
 */
#ifndef const
/* const.h.  Generated automatically by configure.  */
/*
 * small test for const availability
 *
 * Copyright 1993 by the Antaire Corporation
 *
 * This file is part of the argv library.
 *
 * $Id: const.h.in,v 1.2 1994/02/18 03:18:53 gray Exp $
 */

/*
 * this is designed to be inserted into argv.h to define const
 * to be const/nothing for ansi/non-ansi systems.
 */
#define const	const
#endif /* ! const : from argv.h.1 */
/* NOTE: start of $Id: argv.h.2,v 1.6 1994/02/20 03:45:02 gray Exp $ */

/*
 * Generic and standardized argument processor.  You describe the arguments
 * that you are looking for along with their types and these routines do the
 * work to convert them into values.
 *
 * These routines also provide standardized error and usage messages as well
 * as good usage documentation and long and short options.
 */

/*
 * argument information structure.  this specifies the allowable options
 * and some information about each one.
 *
 * { 'O',  "optimize",  ARGV_BOOL,  &optimize,  NULL,  "turn on optimization" }
 * { 'c',  "config",  ARGV_CHARP,  &config,  "file",  "configuration file" }
 */
typedef struct {
  char		ar_short_arg;		/* the char of the arg, 'd' if '-d' */
  char		*ar_long_arg;		/* long version of arg, 'delete' */
  short		ar_type;		/* type of option, see values below */
#if defined(__STDC__) && __STDC__ == 1
  void		*ar_variable;		/* address of variable that is arg */
#else
  char		*ar_variable;		/* address of variable that is arg */
#endif
  char		*ar_var_label;		/* label for variable descriptions */
  char		*ar_comment;		/* comment for usage message */
} argv_t;

/*
 * argument array type.  when ARGV_ARRAY is |'d with the ar_type in the above
 * structure then multiple instances of the option are allowed and each
 * instance is stored into the following structure that MUST be in ar_variable
 * in the above arg_t structure.
 * NOTE: after the arguments have been processed, if aa_entryn is > 0 then
 * aa_entries needs to be free'd by user. argv_cleanup() can be used for this
 */
typedef struct {
  int		aa_entryn;		/* number of elements in aa_entrees */
#if defined(__STDC__) && __STDC__ == 1
  void		*aa_entries;		/* entry list specified */
#else
  char		*aa_entries;		/* entry list specified */
#endif
} argv_array_t;

/* define to extract from an argv array VAR an entry of TYPE at pos WHICH */
#define ARGV_ARRAY_ENTRY(var, type, which) \
  (((type *)(var).aa_entries)[which])

/* special ar_short_arg value to mark the last entry in the argument array */
#define ARGV_LAST	((char)255)

/*
 * special ar_short_arg value to mark mandatory arguments (i.e. arguments that
 * *must* be specified.  for arguments that are not optional like [-b].
 * to have a variable number of mandatory args then make the last MAND
 * entry be a ARG_ARRAY type.
 */
#define ARGV_MAND	((char)254)

/*
 * special ar_short_arg value to mark that there is the possibility of
 * a mandatory argument here if one is specified.
 */
#define ARGV_MAYBE	((char)253)

/*
 * special ar_short_arg value to say that the previous and next arguments in
 * the list should not be used together.
 * {'a'...}, {ARG_OR}, {'b'...}, {ARG_OR}, {'c'...} means
 * the user should only specific -a or -b or -c but not 2 or more.
 */
#define ARGV_OR		((char)252)

/*
 * special ar_short_arg value that is the same as ARGV_OR but one of the args
 * must be used.
 * {'a'...}, {ARG_OR}, {'b'...}, {ARG_OR}, {'c'...} means
 * the user must specify one of -a or -b or -c but not 2 or more.
 */
#define ARGV_XOR	((char)251)

/*
 * ar_type values of arg_t
 * NOTE: if this list is changed, some defines in argv_loc need to be changed
 */
#define ARGV_BOOL	1		/* boolean type, sets to ARGV_TRUE */
#define ARGV_CHAR	2		/* single character */
#define ARGV_CHARP	3		/* character string */
#define ARGV_FLOAT	4		/* floating pointer number */
#define ARGV_SHORT	5		/* integer number */
#define ARGV_INT	6		/* integer number */
#define ARGV_LONG	7		/* long integer number */
#define ARGV_BIN	8		/* binary number (0s and 1s) */
#define ARGV_OCT	9		/* octal number, (base 8) */
#define ARGV_HEX	10		/* hexadecimal number, (base 16) */
#define ARGV_BOOL_NEG	11		/* like bool but sets to ARGV_FALSE */

#define ARGV_TYPE(t)	((t) & 0x0F)	/* strip off the array flag */
#define ARGV_ARRAY	(1 << 15)	/* OR with type to indicate array */

/* argv_usage which argument values */
#define ARGV_USAGE_SHORT	1	/* print short usage messages */
#define ARGV_USAGE_LONG		2	/* print long-format usage messages */

#ifndef ARGV_USAGE_DEFAULT
#define ARGV_USAGE_DEFAULT	ARGV_USAGE_SHORT  /* default usage messages */
#endif

/* boolean type settings */
#define ARGV_FALSE		0
#define ARGV_TRUE		1

/* global variable and procedure scoping for code readability */
#undef	IMPORT
#define	IMPORT		extern

/*<<<<<<<<<<  The below prototypes are auto-generated by fillproto */

/* this is a processed version of argv[0], pre-path removed: /bin/ls -> ls */
IMPORT	char	argv_program[/* PROGRAM_NAME + 1 */];

/* a global value of argv from main after argv_process has been called */
IMPORT	char	**argv_argv;

/* a global value of argc from main after argv_process has been called */
IMPORT	int	argv_argc;

/* this should be set externally to provide general program help to user */
IMPORT	char	*argv_help_string;

/* this should be set externally to provide version information to the user */
IMPORT	char	*argv_version_string;

/*
 * processes ARGC number of arguments from ARGV depending on argument
 * info array ARGS (if null then an empty array is used).  this
 * routine will not modify the argv array in any way.
 */
IMPORT	void	argv_process(const argv_t * args, const int argc,
			     char ** argv);

/*
 * print the standard usage messages for argument array ARGS (if null
 * then an empty array is used).  WHICH chooses between long or short
 * messages (see argv.h).
 * NOTE: if this is called before argv_process then the program name
 * may be messed up.
 */
IMPORT	void	argv_usage(const argv_t * args, const int which);

/*
 * frees up any allocations in ARGS that may have been done by
 * argv_process.  This should be done at the end of the program or
 * after all the arguments have been referenced.
 */
IMPORT	void	argv_cleanup(const argv_t * args);

/*
 * copy all the args (after the 0th), one after the other, into BUF of
 * MAX_SIZE.  NOTE: you can get the 0th argument from argv_argv[0].
 */
IMPORT	void	argv_copy_args(char * buf, const int max_size);

/*<<<<<<<<<<   This is end of the auto-generated output from fillproto. */

/* Note: I added this -- fpm */
#ifdef __cplusplus
}
#endif

#endif /* ! __ARGV_H__ */



