/*
 * Local defines for the argv module
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
 * $Id: argv_loc.h,v 1.14 1994/02/18 03:06:09 gray Exp $
 */

#ifndef __ARGV_LOC_H__
#define __ARGV_LOC_H__

/*
 * global variable and procedure scoping for code readability
 */
#undef	EXPORT
#define	EXPORT

#undef	IMPORT
#define	IMPORT		extern

#undef	LOCAL
#define	LOCAL		static

/*
 * generic constants
 */
#undef	NULL
#define NULL		0L

#undef	MIN
#define MIN(a,b)	((a) < (b) ? (a) : (b))

/*
 * local argv defines
 */

#define NO_VALUE		(-1)		/* no mandatory args value */
#define ARRAY_INCR		10		/* increment by 10 every 10 */
#define SHORT_USAGE_INDENT	5		/* indent short-usage column */
#define USAGE_WIDTH		79		/* short usage width */

/* some sizeof defines */
#define SHORT_PREFIX_LENGTH	(sizeof(SHORT_PREFIX) - 1)
#define LONG_PREFIX_LENGTH	(sizeof(LONG_PREFIX) - 1)
#define UNKNOWN_ARG_LENGTH	(sizeof(UNKNOWN_ARG) - 1)
#define LONG_LABEL_LENGTH	(sizeof(LONG_LABEL) - 1)
#define ARG_EQUALS_LENGTH	(sizeof(ARG_EQUALS) - 1)

/* error messages */
#define USAGE_ERROR_NAME	"usage problem"
#define INTERNAL_ERROR_NAME	"internal argv error"

/*
 * settings to vary the output of the argument routines
 */

#define PROGRAM_NAME		256		/* max size of program name */
#define EXIT_CODE		1		/* argv exit code for errors */

/* special argument definitions */
#define LAST_ARG		"--"		/* arg to mark end of args */
#define LONG_PREFIX		"--"		/* prefix for long args */
#define SHORT_PREFIX		"-"		/* prefix for short args */
#define UNKNOWN_ARG		"??"		/* unknown argument output */
#define ARG_EQUALS		"="		/* to assign value to option */

/* how to produce the env var using sprintf and the argv_program variable */
#define ENVIRON_FORMAT		"ARGV_%s"

/* special short-argument strings */
#define USAGE_CHAR_ARG		'?'		/* default short-opt usage */

/* special long-argument strings */
#define HELP_ARG		"help"		/* default help option */
#define USAGE_ARG		"usage"		/* default usage option */
#define USAGE_LONG_ARG		"usage-long"	/* default usage-long opt */
#define USAGE_SHORT_ARG		"usage-short"	/* default usage-short opt */
#define VERSION_ARG		"version"	/* default version option */

/* spacing on line for usage messages */
#define SHORT_COLUMN		2	/* spaces to indent for short-args */
#define LONG_COLUMN		20	/* column for long options */
#define COMMENT_COLUMN		43	/* column for comments */

#define MAX_ARGUMENT		10	/* max size of argument string */
#define MAX_LONG_OPTION		15	/* max size of long-option string */
#define MAX_COMMENT		30	/* max size of description string */

/* some in-line "labels" for comments */
#define USAGE_LABEL		"Usage: "	/* usage message start */
#define VERSION_LABEL		""		/* default:no version prefix */
#define LONG_LABEL		"or "		/* put before long-option */
#define COMMENT_LABEL		"= "		/* put before comments */
#define ARRAY_CHAR		'+'		/* identifying arrays */

/******************************** argv types *********************************/

/* matching the type values (from argv.h) EXACTLY for printing of % types */
#define ARGV_TYPE_CHARS		"?tcsfddlbox"

/* sizes of the associated types */
LOCAL	int	argv_type_size[] = {
  0,					/* unknown */
  sizeof(char),				/* bool */
  sizeof(char),				/* char */
  sizeof(char *),			/* char * */
  sizeof(float),			/* float */
  sizeof(short),			/* short */
  sizeof(int),				/* int */
  sizeof(long),				/* long */
  sizeof(int),				/* int */
  sizeof(int),				/* int */
  sizeof(int),				/* int */
  0					/* unknown */
  };

/******************************** queue code *********************************/

/*
 * Generic inline queue defines.
 */

/* declare the arguments needed to maintain a queue for type TYPE */
#define QUEUE_DECLARE(type)	\
  type	*queue_alloc = NULL; \
  int	queue_head = 0; \
  int	queue_tail = 0; \
  int	queue_count = 0; \
  int	queue_max = 1
#define LOCAL_QUEUE_DECLARE(type)	\
  static	type	*queue_alloc = NULL; \
  static	int	queue_head = 0; \
  static	int	queue_tail = 0; \
  static	int	queue_count = 0; \
  static	int	queue_max = 1

/* test to see if the queue has not been allocated */
#define QUEUE_IS_NULL()	(queue_alloc == NULL)

/* allocate a mini-queue */
#define QUEUE_ALLOC(type, num)	\
  do { \
    queue_alloc = (type *)malloc(sizeof(type) * num); \
    queue_head = 0; \
    queue_tail = 0; \
    queue_count = 0; \
    queue_max  = num; \
  } while(0)

/* allocate a mini-queue's allocation */
#define QUEUE_FREE()		free(queue_alloc)

/* enqueue items from the mini-queue */
#define QUEUE_ENQUEUE(item)	\
  do { \
    queue_alloc[queue_head] = item; \
    queue_head = (queue_head + 1) % queue_max; \
    queue_count++; \
  } while(0)

/* dequeue items from the mini-queue */
#define QUEUE_DEQUEUE(item)	\
  do { \
    item = queue_alloc[queue_tail]; \
    queue_tail = (queue_tail + 1) % queue_max; \
    queue_count--; \
  } while(0)

/* count the number of items in the mini-queue */
#define QUEUE_COUNT()		queue_count

#endif /* ! __ARGV_LOC_H__ */
