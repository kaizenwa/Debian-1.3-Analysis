/*
 * Test file for the argv routines...
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
 */

#define ARGV_TEST_MAIN

#include "argv.h"

#ifdef ANTAIRE
#include "malloc_dbg.h"
#endif

#if INCLUDE_RCS_IDS
static	char	*rcs_id =
  "$Id: argv_test.c,v 1.13 1993/09/29 22:23:36 gray Exp $";
#endif

/* argument variables */
static	char		apple		= ARGV_FALSE;
static	int		berry		= 0;
static	argv_array_t	baseball;
static	char		little		 = ' ';
static	char		w		= ARGV_FALSE;
static	char		x		= ARGV_FALSE;
static	char		y		= ARGV_FALSE;
static	char		z		= ARGV_FALSE;
static	argv_array_t	left;

static	argv_t	args[] = {
  { 'a',	"apple",	ARGV_BOOL,			&apple,
      "apple",		"an apple a day..." },
  { 'b',	"berry",	ARGV_INT,			&berry,
      "berry",		"eat BERRY strawberrys" },
  { ARGV_MAND,	0L,		ARGV_CHARP | ARGV_ARRAY,	&left,
      "file1 file2 [files]",	"miscellaneous file arguments........." },

  { 'B',	"baseball",	ARGV_CHARP | ARGV_ARRAY,	&baseball,
      "field",		"array of baseball FIELDS" },
  { 'w',	"w-mark",	ARGV_BOOL,			&w,
      0L,		"w marks the spot" },
  { ARGV_OR },
  { 'x',	"x-mark",	ARGV_BOOL,			&x,
      0L,		"x marks the spot" },
  { 'y',	0L,		ARGV_BOOL,			&y,
      0L,		"y marks the spot" },
  { ARGV_XOR },
  { 'z',	"z-mark",	ARGV_BOOL,			&z,
      0L,		"z marks the spot" },
  
  { ARGV_LAST }
};

int	main(int argc, char ** argv)
{
  int		count;
  
  argv_help_string = "Argv library test program.";
  argv_version_string = "$Revision: 1.13 $";
  
  argv_process(args, argc, argv);
  
  (void)printf("program name = '%s' arg0 = '%s'\n",
	       argv_program, argv_argv[0]);
  
  (void)printf("apple = %s\n", (apple ? "true" : "false"));
  (void)printf("berry = %d\n", berry);
  
  if (baseball.aa_entryn == 0)
    (void)printf("No baseballs specified.\n");
  else {
    for (count = 0; count < baseball.aa_entryn; count++)
      (void)printf("baseball[%d]: '%s'\n",
		   count, ARGV_ARRAY_ENTRY(baseball, char *, count));
  }
  
  (void)printf("little = '%c'\n", little);
  (void)printf("w = %s\n", (w ? "true" : "false"));
  (void)printf("x = %s\n", (x ? "true" : "false"));
  (void)printf("y = %s\n", (y ? "true" : "false"));
  (void)printf("z = %s\n", (z ? "true" : "false"));
  
  if (left.aa_entryn > 0)
    for (count = 0; count < left.aa_entryn; count++)
      (void)printf("left[%d]: '%s'\n",
		   count, ARGV_ARRAY_ENTRY(left, char *, count));
  
  (void)printf("\n");
  
  {
    char	buf[128];
    argv_copy_args(buf, sizeof(buf));
    (void)printf("All args = '%s %s'\n", argv_argv[0], buf);
  }
  
  argv_cleanup(args);
  
#ifdef ANTAIRE
  malloc_shutdown();
#endif
  
  (void)exit(0);
}
