/*
 *  magicfilter.h
 *
 *  Copyright © 1993-95 H. Peter Anvin
 *
 *  Header file for the magic lpr/lpd filter
 */

#include "config.h"
#include <stdio.h>
#ifdef HAVE_PATHS_H
#include <paths.h>
#endif
#include <string.h>
#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <ctype.h>

/*
 * These *must* be in the same order as the table in parseconfig.c!
 */
enum actions
{
  ACT_CAT,			/* Just output the file */
  ACT_DROP,			/* Silently ignore the file */
  ACT_REJECT,			/* Mail the user and complain */
  ACT_FILTER,			/* Run through filter, then done */
  ACT_PIPETHRU,			/* Pipe through command, then reprocess */
  ACT_ADDCR,			/* Like ACT_CAT, but \n -> \r\n, \f -> \r\f */
  ACT_PS,			/* Like ACT_ADDCR, but add PSEOJ (^D) at end */
  ACT_FFILTER,			/* Like ACT_FILTER, but use a temp file */
  ACT_FPIPE,			/* Like ACT_PIPETHRU, but use a temp file */
  
  ACT_ERR = -1
};

/*
 * Special magic actions
 */

#define MAG_ERR       (-1)	/* Invalid line */
#define MAG_COMMENT   (-2)	/* Line is blank or comment */
#define MAG_DEFAULT   (-3)	/* Action is the default action */
#define MAG_SET       (-4)	/* Action is a set command */

struct datatype
{
  struct datatype *next;	/* Linked list pointer */
  int offset;			/* Where in file is ID string */
  int length;			/* Length of ID string */
  char *magic;			/* Expected ID string */
  char *mask;			/* Mask for ID string */
  int action;			/* What to do with it */
  char *command;		/* Command line, or for ACT_REJECT, */
				/* text of complaint message */
};

/* Stuff to make stupid C compilers happy, and to fill in gaps */

#ifdef NOVOID
#define void
#endif
#ifdef NOCONST
#define const
#endif
#ifndef DEBUG
#define DEBUG 0
#endif

/* Function prototypes */

struct datatype *load_config(char *file, int *in_block_size);
int getoffset(char *p, char **endpos);
int getmagic(char *p, char **endpos, char *magic, char *mask);
enum actions getaction(char *pos, char **cmd);

/* Global variables */

extern int debug_flag;
extern char NO_WILD[];		/* Used as senitel value by getmagic() */
