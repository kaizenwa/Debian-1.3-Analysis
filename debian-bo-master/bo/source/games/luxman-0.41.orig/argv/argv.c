/*
 * Generic argv processor...
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

#include <ctype.h>
#include <stdio.h>

#ifdef ANTAIRE
#include "malloc_dbg.h"
#endif

#include "conf.h"
#include "argv.h"
#include "argv_loc.h"

#if INCLUDE_RCS_IDS
LOCAL	char	*rcs_id =
  "$Id: argv.c,v 1.36 1994/02/28 02:09:48 gray Exp $";
#endif

/* internal routines */
EXPORT	void	argv_usage(const argv_t * args, const int which);

/*
 * exported variables
 */
/* this is a processed version of argv[0], pre-path removed: /bin/ls -> ls */
EXPORT	char	argv_program[PROGRAM_NAME + 1] = "Unknown";

/* a global value of argv from main after argv_process has been called */
EXPORT	char	**argv_argv		= NULL;
/* a global value of argc from main after argv_process has been called */
EXPORT	int	argv_argc		= 0;

/* this should be set externally to provide general program help to user */
EXPORT	char	*argv_help_string	= NULL;
/* this should be set externally to provide version information to the user */
EXPORT	char	*argv_version_string	= NULL;

/* local variables */
LOCAL	int	argv_usage_on_error	= 0;	/* print usage on error? */
LOCAL_QUEUE_DECLARE(int);			/* args waiting for values */

LOCAL	argv_t	empty[] = { { ARGV_LAST }};	/* empty argument array */

/***************************** general utilities *****************************/

/*
 * binary STR to integer translation
 */
LOCAL	int	_argv_btoi(const char * str)
{
  int		ret = 0;
  
  /* strip off spaces */
  for (; isspace(*str); str++);
  
  for (; *str == '0' || *str == '1'; str++) {
    ret *= 2;
    ret += *str - '0';
  }
  
  return ret;
}

/*
 * octal STR to integer translation
 */
LOCAL	int	_argv_otoi(const char *str)
{
  int		ret = 0;
  
  /* strip off spaces */
  for (; isspace(*str); str++);
  
  for (; *str >= '0' && *str <= '7'; str++) {
    ret *= 8;			/* converting octal nums is a snap */
    ret += *str - '0';
  }
  
  return ret;
}

/*
 * hexadecimal STR to integer translation
 */
LOCAL	int	_argv_htoi(const char * str)
{
  int		ret = 0;
  
  /* strip off spaces */
  for (; isspace(*str); str++);
  
  /* skip a leading 0[xX] */
  if (*str == '0' && (*(str + 1) == 'x' || *(str + 1) == 'X'))
    str += 2;
  
  for (; isdigit(*str) ||
       (*str >= 'a' && *str <= 'f') || (*str >= 'A' && *str <= 'F');
       str++) {
    ret *= 16;
    if (*str >= 'a' && *str <= 'f')
      ret += *str - 'a' + 10;
    else if (*str >= 'A' && *str <= 'F')
      ret += *str - 'A' + 10;
    else
      ret += *str - '0';
  }
  
  return ret;
}

/*
 * break STR and return an array of char * whose values are tokenized by TOK.
 * it passes back the number of tokens in TOKN.
 * NOTE: the return value should be freed later and the STR should stay around
 * until that time.
 */
LOCAL	char	**_argv_vectorize(char * str, const char * tok, int * tokn)
{
  char	**vectp;
  char	*tmp, *tokp;
  int	tokc;
  
  /* count the tokens */
  tmp = (char *)strdup(str);
  tokp = (char *)strtok(tmp, tok);
  for (tokc = 0; tokp != NULL; tokc++)
    tokp = (char *)strtok(NULL, tok);
  free(tmp);
  
  *tokn = tokc;
  
  if (tokc == 0)
    return NULL;
  
  /* allocate the pointer grid */
  vectp = (char **)malloc(sizeof(char *) * tokc);
  
  /* load the tokens into the list */
  vectp[0] = (char *)strtok(str, tok);
  
  for (tokc = 1; tokc < *tokn; tokc++)
    vectp[tokc] = (char *)strtok(NULL, tok);
  
  return vectp;
}

/****************************** local utilities ******************************/

/*
 * preprocess argument array ARGS of NUM_ARGS entries and set the MAND
 * and MAYBE boolean arrays
 */
LOCAL	void	preprocess_array(const argv_t * args, const int num_args,
				 char * mand, char * maybe)
{
  argv_t	*argp;
  char		mand_array = ARGV_FALSE, maybe_field = ARGV_FALSE;
  
  /* count the args and find the first mandatory */
  for (argp = (argv_t *)args; argp < args + num_args; argp++) {
    
    /* do we have a mandatory-array? */
    if (argp->ar_short_arg == ARGV_MAND) {
      if (mand_array) {
	(void)fprintf(stderr,
		      "%s: %s, no ARGV_MAND's can follow a ARGV_MAND array\n",
		      argv_program, INTERNAL_ERROR_NAME);
	(void)exit(EXIT_CODE);
      }
      if (maybe_field) {
	(void)fprintf(stderr,
		      "%s: %s, no ARGV_MAND's can follow a ARGV_MAYBE\n",
		      argv_program, INTERNAL_ERROR_NAME);
	(void)exit(EXIT_CODE);
      }
      
      if (argp->ar_long_arg != NULL) {
	(void)fprintf(stderr,
		      "%s: %s, ARGV_MAND's should not have long-options\n",
		      argv_program, INTERNAL_ERROR_NAME);
	(void)exit(EXIT_CODE);
      }
      
      mand[argp - args] = ARGV_TRUE;
      
      if (ARGV_ARRAY & argp->ar_type)
	mand_array = ARGV_TRUE;
    }
    
    /* do we have a maybe field? */
    if (argp->ar_short_arg == ARGV_MAYBE) {
      if (ARGV_ARRAY & argp->ar_type) {
	(void)fprintf(stderr,
		      "%s: %s, ARGV_MAYBE's cannot have array types\n",
		      argv_program, INTERNAL_ERROR_NAME);
	(void)exit(EXIT_CODE);
      }
      
      maybe[argp - args] = ARGV_TRUE;
      maybe_field = ARGV_TRUE;
    }
    
    /* handle initializing the argument array */
    if (ARGV_ARRAY & argp->ar_type) {
      argv_array_t	*arrp = (argv_array_t *)argp->ar_variable;
      
      if (ARGV_TYPE(argp->ar_type) == ARGV_BOOL
	  || ARGV_TYPE(argp->ar_type) == ARGV_BOOL_NEG) {
	(void)fprintf(stderr,
		      "%s: %s, cannot have an array of boolean values\n",
		      argv_program, INTERNAL_ERROR_NAME);
	(void)exit(EXIT_CODE);
      }
      arrp->aa_entryn = 0;
    }
    
    /* must have a valid ar_short_arg */
    if (argp->ar_short_arg == '\0') {
      (void)fprintf(stderr,
		    "%s: %s, short-option character is '\\0'\n",
		    argv_program, INTERNAL_ERROR_NAME);
      (void)exit(EXIT_CODE);
    }
    
    /* verify variable pointer */
    if (argp->ar_variable == NULL
	&& argp->ar_short_arg != ARGV_OR
	&& argp->ar_short_arg != ARGV_XOR) {
      (void)fprintf(stderr,
		    "%s: %s, NULL variable specified in arg array\n",
		    argv_program, INTERNAL_ERROR_NAME);
      (void)exit(EXIT_CODE);
    }
    
    /* verify [X]OR's */
    if (argp->ar_short_arg == ARGV_OR
	|| argp->ar_short_arg == ARGV_XOR) {
      
      /* that they are not at the start or end of list */
      if (argp == args || argp >= (args + num_args - 1)) {
	(void)fprintf(stderr,
		      "%s: %s, ARGV_[X]OR entries cannot be at start or end of array\n",
		      argv_program, INTERNAL_ERROR_NAME);
	(void)exit(EXIT_CODE);
      }
      
      /* that two aren't next to each other */
      if ((argp - 1)->ar_short_arg == ARGV_OR
	  || (argp - 1)->ar_short_arg == ARGV_XOR) {
	(void)fprintf(stderr,
		      "%s: %s, two ARGV_[X]OR entries cannot be next to each other\n",
		      argv_program, INTERNAL_ERROR_NAME);
	(void)exit(EXIT_CODE);
      }
    }
  }
}

/*
 * translate string argument ARG into VAR depending on its TYPE
 */
#if defined(__STDC__) && __STDC__ == 1
LOCAL	void	translate_value(const char * arg, void * var, const short type)
#else
LOCAL	void	translate_value(const char * arg, char * var, const short type)
#endif
{
  argv_array_t	*arrp;
  int		val_type = ARGV_TYPE(type), size = argv_type_size[val_type];
  
  if (ARGV_ARRAY & type) {
    size = argv_type_size[val_type];
    arrp = (argv_array_t *)var;
    
    if (arrp->aa_entryn == 0)
      arrp->aa_entries = (char *)malloc(ARRAY_INCR * size);
    else if (arrp->aa_entryn % ARRAY_INCR == 0)
      arrp->aa_entries =
	(char *)realloc(arrp->aa_entries, (arrp->aa_entryn + ARRAY_INCR) *
			size);
    
    var = (char *)(arrp->aa_entries) + arrp->aa_entryn * size;
    arrp->aa_entryn++;    
  }
  
  /* translate depending on type */
  switch (val_type) {
    
  case ARGV_BOOL:
    *(char *)var = ARGV_TRUE;
    break;
    
  case ARGV_BOOL_NEG:
    *(char *)var = ARGV_FALSE;
    break;
    
  case ARGV_CHAR:
    *(char *)var = *arg;
    break;
    
  case ARGV_CHARP:
    *(char **)var = (char *)arg;
    break;
    
  case ARGV_FLOAT:
    *(float *)var = (float)atof(arg);
    break;
    
  case ARGV_SHORT:
    *(short *)var = (short)atoi(arg);
    break;
    
  case ARGV_INT:
    *(int *)var = atoi(arg);
    break;
    
  case ARGV_LONG:
    *(long *)var = atol(arg);
    break;
    
  case ARGV_BIN:
    *(int *)var = _argv_btoi(arg);
    break;
    
  case ARGV_OCT:
    *(int *)var = _argv_otoi(arg);
    break;
    
  case ARGV_HEX:
    *(int *)var = _argv_htoi(arg);
    break;
  }
}

/*
 * check out if WHICH argument from ARGS has an *or* specified
 * attached to it.  the USED array is needed to determine if a usage
 * problem exists.  returns ARGV_TRUE or ARGV_FALSE if we are okay
 */
LOCAL	char	check_or(const argv_t * args, const int num_args,
			 const char * used, const int which)
{
  int	other = NO_VALUE, argc;
  
  /* check ORs below */
  for (argc = which - 2; argc >= 0
       && (args[argc + 1].ar_short_arg == ARGV_OR
	   || args[argc + 1].ar_short_arg == ARGV_XOR);
       argc -= 2)
    if (used[argc]) {
      other = argc;
      break;
    }
  
  /* check ORs above */
  if (other == NO_VALUE) {
    for (argc = which + 2; argc < num_args
	 && (args[argc - 1].ar_short_arg == ARGV_OR
	     || args[argc - 1].ar_short_arg == ARGV_XOR);
	 argc += 2)
      if (used[argc]) {
	other = argc;
	break;
      }
  }
  
  /* did we find a problem? */
  if (other == NO_VALUE)
    return ARGV_TRUE;
  
  (void)fprintf(stderr,
		"%s: %s, cannot specify more than one of the following at once:\n",
		argv_program, USAGE_ERROR_NAME);
  
  for (;;) {
    if (args[other].ar_long_arg == NULL)
      (void)fprintf(stderr, "     %s%c\n",
		    SHORT_PREFIX, args[other].ar_short_arg);
    else
      (void)fprintf(stderr, "     %s%c (%s%s)\n",
		    SHORT_PREFIX, args[other].ar_short_arg,
		    LONG_PREFIX, args[other].ar_long_arg);
    
    if (other == which)
      break;
    other = which;
  }
  
  return ARGV_FALSE;
}

/*
 * find all the XOR arguments and make sure each group has at least
 * one specified in it.  returns ARGV_TRUE or ARGV_FALSE if we are
 * okay
 */
LOCAL	char	check_xor(const argv_t * args, const int num_args,
			  const char * used)
{
  char	okay;
  int	argc, inc = 0;
  
  /* run through the list of arguments */
  for (argc = 0; argc < num_args; argc++) {
    if (args[argc].ar_short_arg != ARGV_XOR)
      continue;
    
    okay = ARGV_FALSE;
    if (used[argc - 1] || used[argc + 1])
      okay = ARGV_TRUE;
    
    /* check XORs above */
    for (inc = argc + 2; inc < num_args && args[inc].ar_short_arg == ARGV_XOR;
	 inc += 2) {
      if (used[inc + 1])
	okay = ARGV_TRUE;
    }
    
    if (! okay)
      break;
    
    /* we want argc to start checking at inc + 1 */
    argc = inc;
  }
  
  /* did we find a problem? */
  if (argc >= num_args)
    return ARGV_TRUE;
  
  /*
   * NOTE: argc - 1 is now the first entry and inc - 1 the last
   */
  
  (void)fprintf(stderr, "%s: %s, must specify one of:\n",
		argv_program, USAGE_ERROR_NAME);
  
  for (argc--; argc < inc; argc += 2) {
    if (args[argc].ar_long_arg == NULL)
      (void)fprintf(stderr, "     %s%c\n",
		    SHORT_PREFIX, args[argc].ar_short_arg);
    else
      (void)fprintf(stderr, "     %s%c (%s%s)\n",
		    SHORT_PREFIX, args[argc].ar_short_arg,
		    LONG_PREFIX, args[argc].ar_long_arg);
  }
  
  return ARGV_FALSE;
}

/*
 * process a list of arguments ARGV and ARGV as it applies to ARGS.
 * USED is a boolean array to mark args that have been specified and
 * DO_COPY tells do_list to rearrange arguments to be returned to
 * main().  MAND is a bool array to mark the mandatory args.
 */
LOCAL	void	do_list(const argv_t * args, const int num_args,
			const int argc, char ** argv, char * used,
			char * mand, char * maybe)
{
  int		charc, slotc, arrc;
  int		match, len;
  char		last_arg = ARGV_FALSE, okay = ARGV_TRUE;
  const char	*arg;
#if NO_CLOSE_ARGUMENT == 0
  char		*closep;
#endif
  
  /* run throught rest of arguments */
  for (slotc = 0; slotc < argc; slotc++) {
    
    arg = argv[slotc];
    
    /* have we reached the LAST_ARG marker? */
    if (! last_arg && strcmp(LAST_ARG, arg) == 0) {
      last_arg = ARGV_TRUE;
      continue;
    }
    
    /* are we processing a long option? */
    if (! last_arg && strncmp(LONG_PREFIX, arg, LONG_PREFIX_LENGTH) == 0) {
      
#if NO_CLOSE_ARGUMENT == 0
      closep = (char *)strstr(arg, ARG_EQUALS);
      if (closep != NULL) {
	*closep = '\0';
	closep += ARG_EQUALS_LENGTH;
      }
#endif
      
      /* get length of rest of argument */
      len = strlen(arg) - LONG_PREFIX_LENGTH;
      
      /* we need more than the prefix */
      if (len <= 0) {
	(void)fprintf(stderr,
		      "%s: %s, empty long-option prefix '%s'\n",
		      argv_program, USAGE_ERROR_NAME, arg);
	
	okay = ARGV_FALSE;
	continue;
      }
      
      match = NO_VALUE;
      
      /* run though long options looking for a match */
      for (arrc = 0; arrc < num_args; arrc++) {
	if (args[arrc].ar_long_arg == NULL)
	  continue;
	
	if (strncmp(arg + LONG_PREFIX_LENGTH,
		    args[arrc].ar_long_arg, len) == 0) {
	  if (match != NO_VALUE) {
	    (void)fprintf(stderr,
			  "%s: %s, '%s' might be '%s' or '%s'\n",
			  argv_program, USAGE_ERROR_NAME, arg,
			  args[arrc].ar_long_arg, args[match].ar_long_arg);
	    
	    okay = ARGV_FALSE;
	    break;
	  }
	  
	  /* record a possible match */
	  match = arrc;
	  
	  /* don't break, need to see if another one matches */
	}
      }
      
      /* if we found a match but quit then we must have found two matches */
      if (match != NO_VALUE && arrc < num_args)
	continue;
      
      /* did we not find long-option match? */
      if (match == NO_VALUE) {
	
	/* check for special usage value */
	if (strncmp(USAGE_ARG, arg + LONG_PREFIX_LENGTH, len) == 0) {
	  argv_usage(args, ARGV_USAGE_DEFAULT);
	  (void)exit(0);
	}
	
	/* check for special short-usage value */
	if (strncmp(USAGE_SHORT_ARG, arg + LONG_PREFIX_LENGTH, len) == 0) {
	  argv_usage(args, ARGV_USAGE_SHORT);
	  (void)exit(0);
	}
	
	/* check for special long-usage value */
	if (strncmp(USAGE_LONG_ARG, arg + LONG_PREFIX_LENGTH, len) == 0) {
	  argv_usage(args, ARGV_USAGE_LONG);
	  (void)exit(0);
	}
	
	/* check for special help value */
	if (strncmp(HELP_ARG, arg + LONG_PREFIX_LENGTH, len) == 0) {
	  if (argv_help_string == NULL)
	    (void)fprintf(stderr, "%s: I'm sorry, no help is available.\n",
			  argv_program);
	  else
	    (void)fprintf(stderr, "%s: %s\n", argv_program, argv_help_string);
	  (void)exit(0);
	}
	
	/* check for special version value */
	if (strncmp(VERSION_ARG, arg + LONG_PREFIX_LENGTH, len) == 0) {
	  if (argv_version_string == NULL)
	    (void)fprintf(stderr,
			  "%s: I'm sorry, no version information is available.\n",
			  argv_program);
	  else
	    (void)fprintf(stderr, "%s: %s%s\n",
			  argv_program, VERSION_LABEL, argv_version_string);
	  (void)exit(0);
	}
	
	(void)fprintf(stderr,
		      "%s: %s, unknown long option '%s'.\n",
		      argv_program, USAGE_ERROR_NAME, arg);
	
	okay = ARGV_FALSE;
	continue;
      }
      
#if USE_ONCE
      /*
       * have we used this one before?
       * NOTE: should this be a warning or a non-error altogether?
       */
      if (used[match] && (! ARGV_ARRAY & args[match].ar_type)) {
	(void)fprintf(stderr,
		      "%s: %s, you've already specified the '%s' argument\n",
		      argv_program, USAGE_ERROR_NAME, args[match].ar_long_arg);
	okay = ARGV_FALSE;
	continue;
      }
#endif
      
      /* check arguments that must be OR'd */
      if (check_or(args, num_args, used, match) == ARGV_FALSE) {
	okay = ARGV_FALSE;
	/*
	 * don't continue here else we might generate an XOR error
	 * because the argument wasn't specified
	 */
      }
      
      /* put this one in the value queue if not a boolean */
      if (ARGV_TYPE(args[match].ar_type) == ARGV_BOOL
	  || ARGV_TYPE(args[match].ar_type) == ARGV_BOOL_NEG)
	translate_value(NULL, args[match].ar_variable, args[match].ar_type);
#if NO_CLOSE_ARGUMENT == 0
      else if (closep != NULL)
	translate_value(closep, args[match].ar_variable, args[match].ar_type);
#endif
      else
	QUEUE_ENQUEUE(match);
      
#if NO_CLOSE_ARGUMENT == 0
      closep = NULL;
#endif
      used[match] = ARGV_TRUE;
      continue;
    }
    
    /* are we processing a short option? */
    if (! last_arg && strncmp(SHORT_PREFIX, arg, SHORT_PREFIX_LENGTH) == 0) {
      
#if NO_CLOSE_ARGUMENT == 0
      closep = (char *)strstr(arg, ARG_EQUALS);
      if (closep != NULL) {
	*closep = '\0';
	closep += ARG_EQUALS_LENGTH;
      }
#endif
      
      /* get length of rest of argument */
      len = strlen(arg) - SHORT_PREFIX_LENGTH;
      
      /* we need more than the prefix */
      if (len <= 0) {
	(void)fprintf(stderr,
		      "%s: %s, empty short-option prefix '%s'\n",
		      argv_program, USAGE_ERROR_NAME, arg);
	okay = ARGV_FALSE;
	continue;
      }
      
      /* run through the chars in this option */
      for (charc = 0; charc < len; charc++) {
	
	/* run through the arg list looking for a match */
	for (match = 0; match < num_args; match++)
	  if (args[match].ar_short_arg == arg[SHORT_PREFIX_LENGTH + charc])
	    break;
	
	/* did not find argument */
	if (match >= num_args) {
	  
	  /* check for special usage value */
	  if (arg[SHORT_PREFIX_LENGTH + charc] == USAGE_CHAR_ARG) {
	    argv_usage(args, ARGV_USAGE_DEFAULT);
	    (void)exit(0);
	  }
	  
	  /* create an error string */
	  (void)fprintf(stderr, "%s: %s, unknown short option '%s%c'.\n",
			argv_program, USAGE_ERROR_NAME, SHORT_PREFIX,
			arg[SHORT_PREFIX_LENGTH + charc]);
	  okay = ARGV_FALSE;
	  continue;
	}
	
#if USE_ONCE
	/*
	 * have we used this one before?
	 * NOTE: should this be a warning or a non-error altogether?
	 */
	if (used[match]) {
	  (void)fprintf(stderr,
			"%s: %s, you've already specified the '%c' argument\n",
			argv_program, USAGE_ERROR_NAME,
			args[match].ar_short_arg);
	  okay = ARGV_FALSE;
	  continue;
	}
#endif
	
	/* check arguments that must be OR'd */
	if (check_or(args, num_args, used, match) == ARGV_FALSE) {
	  okay = ARGV_FALSE;
	  /*
	   * don't continue here else we might generate an XOR error
	   * because the argument wasn't specified
	   */
	}
	
	/* put this one in the value queue if not a boolean */
	if (ARGV_TYPE(args[match].ar_type) == ARGV_BOOL
	    || ARGV_TYPE(args[match].ar_type) == ARGV_BOOL_NEG)
	  translate_value(NULL, args[match].ar_variable, args[match].ar_type);
#if NO_CLOSE_ARGUMENT == 0
	else if (closep != NULL && charc + 1 >= len)
	  translate_value(closep, args[match].ar_variable,
			  args[match].ar_type);
#endif
	else
	  QUEUE_ENQUEUE(match);
	
	used[match] = ARGV_TRUE;
      }
      
#if NO_CLOSE_ARGUMENT == 0
      closep = NULL;
#endif
      
      continue;
    }
    
    /* could this be a value? */
    if (num_args > 0 && QUEUE_COUNT() > 0) {
      QUEUE_DEQUEUE(match);
      translate_value(arg, args[match].ar_variable, args[match].ar_type);
      continue;
    }
    
    /* process mandatory args if some left to process */
    for (arrc = 0; arrc < num_args; arrc++)
      if (mand[arrc])
	break;
    if (arrc < num_args) {
      /* absorb another mand. arg */
      translate_value(arg, args[arrc].ar_variable, args[arrc].ar_type);
      if (! (ARGV_ARRAY & args[arrc].ar_type))
	mand[arrc] = ARGV_FALSE;
      continue;
    }
    
    /* process maybe args if some left to process */
    for (arrc = 0; arrc < num_args; arrc++)
      if (maybe[arrc])
	break;
    if (arrc < num_args) {
      /* absorb another maybe arg */
      translate_value(arg, args[arrc].ar_variable, args[arrc].ar_type);
      maybe[arrc] = ARGV_FALSE;
      continue;
    }
    
    /* default is an error */
    (void)fprintf(stderr, "%s: %s, unwanted additional arguments\n",
		  argv_program, USAGE_ERROR_NAME);
    if (argv_usage_on_error)
      argv_usage(args, ARGV_USAGE_DEFAULT);
    okay = ARGV_FALSE;
  }
  
  /*
   * we got here because we ran out of args
   */
  
  slotc = QUEUE_COUNT();
  if (slotc > 0) {
    (void)fprintf(stderr,
		  "%s: %s, %d more option-argument%s must be specified\n",
		  argv_program, USAGE_ERROR_NAME,
		  slotc, (slotc == 1 ? "" : "s"));
    okay = ARGV_FALSE;
  }
  
  /* see if there are any mandatory args left */
  slotc = 0;
  for (arrc = 0; arrc < num_args; arrc++)
    if (mand[arrc] && ! (ARGV_ARRAY & args[arrc].ar_type))
      slotc++;
  if (slotc > 0) {
    (void)fprintf(stderr,
		  "%s: %s, %d more mandatory-argument%s must be specified\n",
		  argv_program, USAGE_ERROR_NAME,
		  slotc, (slotc == 1 ? "" : "s"));
    okay = ARGV_FALSE;
  }
  
  if (! check_xor(args, num_args, used))
    okay = ARGV_FALSE;
  
  if (! okay) {
    if (argv_usage_on_error)
      argv_usage(args, ARGV_USAGE_DEFAULT);
    (void)exit(EXIT_CODE);
  }
}

/****************************** usage routines *******************************/

/*
 * print the special long options
 */
LOCAL	void	print_long_special(void)
{
  int	col = LONG_COLUMN - LONG_PREFIX_LENGTH;
  
  if (ARGV_USAGE_DEFAULT == ARGV_USAGE_LONG) {
    (void)fprintf(stderr,
		  "%s%-*.*s%sDisplay this long-usage info (or %s%s).\n",
		  LONG_PREFIX, col, col, USAGE_ARG, COMMENT_LABEL,
		  LONG_PREFIX, USAGE_LONG_ARG);
    (void)fprintf(stderr, "%s%-*.*s%sDisplay short-usage info.\n",
		  LONG_PREFIX, col, col, USAGE_SHORT_ARG, COMMENT_LABEL);
  }
  else {
    (void)fprintf(stderr, "%s%-*.*s%sDisplay this long-usage info.\n",
		  LONG_PREFIX, col, col, USAGE_LONG_ARG, COMMENT_LABEL);
    (void)fprintf(stderr, "%s%-*.*s%sDisplay short-usage info (or %s%s).\n",
		  LONG_PREFIX, col, col, USAGE_ARG, COMMENT_LABEL,
		  LONG_PREFIX, USAGE_SHORT_ARG);
  }
  if (argv_help_string != NULL)
    (void)fprintf(stderr, "%s%-*.*s%sShow program help message.\n",
		  LONG_PREFIX, col, col, HELP_ARG, COMMENT_LABEL);
  if (argv_version_string != NULL)
    (void)fprintf(stderr, "%s%-*.*s%sShow program version message.\n",
		  LONG_PREFIX, col, col, VERSION_ARG, COMMENT_LABEL);
}

/*
 * print a short-format usage message
 */
LOCAL	void	usage_short(const argv_t * args)
{
  int		colc = 0;
  argv_t	*argp;
  char		mark = ARGV_FALSE;
  
  /* print the usage message header */
  (void)fprintf(stderr, "%s%s", USAGE_LABEL, argv_program);
  colc += strlen(USAGE_LABEL) + strlen(argv_program);
  
  /*
   * print all of the boolean arguments first.
   * NOTE: we assume they all fit on the line
   */
  for (argp = (argv_t *)args; argp->ar_short_arg != ARGV_LAST; argp++) {
    
    /* skip non booleans and or-specifiers */
    if (argp->ar_short_arg == ARGV_OR
	|| argp->ar_short_arg == ARGV_XOR
	|| (ARGV_TYPE(argp->ar_type) != ARGV_BOOL
	    && ARGV_TYPE(argp->ar_type) != ARGV_BOOL_NEG))
      continue;
    
    if (! mark) {
      (void)fprintf(stderr, " [%s", SHORT_PREFIX);
      colc += 1 + 1 + SHORT_PREFIX_LENGTH;
      mark = ARGV_TRUE;
    }
    
    (void)fprintf(stderr, "%c", argp->ar_short_arg);
    colc++;
  }
  
  if (mark) {
    (void)fprintf(stderr, "]");
    colc++;
  }
  
  /* print remaining (non-boolean) arguments */
  for (argp = (argv_t *)args; argp->ar_short_arg != ARGV_LAST; argp++) {
    int		tmp;
    char	*str;
    
    /* skip booleans and or-specifiers */
    if (ARGV_TYPE(argp->ar_type) == ARGV_BOOL
	|| ARGV_TYPE(argp->ar_type) == ARGV_BOOL_NEG
	|| argp->ar_short_arg == ARGV_OR
	|| argp->ar_short_arg == ARGV_XOR)
      continue;
    
    /* print the mandatory argument desc */
    if (argp->ar_short_arg == ARGV_MAND) {
      if (argp->ar_var_label == NULL) {
	tmp = 1 + UNKNOWN_ARG_LENGTH;
	str = UNKNOWN_ARG;
      }
      else {
	tmp = 1 + strlen(argp->ar_var_label);
	str = argp->ar_var_label;
      }
      if (colc + tmp > USAGE_WIDTH) {
	(void)fprintf(stderr, "\n%*.*s",
		      SHORT_USAGE_INDENT, SHORT_USAGE_INDENT, "");
	colc = SHORT_USAGE_INDENT;
      }
      (void)fprintf(stderr, " %s", str);
      colc += tmp;
      continue;
    }
    
    /* print the maybe argument desc */
    if (argp->ar_short_arg == ARGV_MAYBE) {
      if (argp->ar_var_label == NULL) {
	tmp = 1 + 1 + UNKNOWN_ARG_LENGTH + 1;
	str = UNKNOWN_ARG;
      }
      else {
	tmp = 1 + 1 + strlen(argp->ar_var_label) + 1;
	str = argp->ar_var_label;
      }
      if (colc + tmp > USAGE_WIDTH) {
	(void)fprintf(stderr, "\n%*.*s",
		      SHORT_USAGE_INDENT, SHORT_USAGE_INDENT, "");
	colc = SHORT_USAGE_INDENT;
      }
      (void)fprintf(stderr, " [%s]", str);
      colc += tmp;
      continue;
    }
    
    /*
     * + " [" + short_prefix + char
     * NOTE: we check against usage-1 because we need room for ]
     */
    tmp = 1 + 1 + SHORT_PREFIX_LENGTH + 1;
    if (colc + tmp > USAGE_WIDTH - 1) {
      (void)fprintf(stderr, "\n%*.*s",
		    SHORT_USAGE_INDENT, SHORT_USAGE_INDENT, "");
      colc = SHORT_USAGE_INDENT;
    }
    (void)fprintf(stderr, " [%s%c", SHORT_PREFIX, argp->ar_short_arg);
    colc += tmp;
    
    /* NOTE: we know we have a non-boolean here */
    
    if (argp->ar_var_label == NULL) {
      tmp = 1 + UNKNOWN_ARG_LENGTH + 1;
      if (colc + tmp > USAGE_WIDTH) {
	(void)fprintf(stderr, "\n%*.*s",
		      SHORT_USAGE_INDENT, SHORT_USAGE_INDENT, "");
	colc = SHORT_USAGE_INDENT;
      }
      (void)fprintf(stderr, " %s]", UNKNOWN_ARG);
      colc += tmp;
    }
    else {
      tmp = 1 + strlen(argp->ar_var_label) + 1;
      if (colc + tmp > USAGE_WIDTH) {
	(void)fprintf(stderr, "\n%*.*s",
		      SHORT_USAGE_INDENT, SHORT_USAGE_INDENT, "");
	colc = SHORT_USAGE_INDENT;
      }
      (void)fprintf(stderr, " %s]", argp->ar_var_label);
      colc += tmp;
    }
  }
  
  /* print the usage message header */
  (void)fprintf(stderr, "\n");
}

/*
 * print a long-format usage message
 */
LOCAL	void	usage_long(const argv_t * args)
{
  int		colc;
  argv_t	*argp;
  
  /* print the usage message header */
  (void)fprintf(stderr, "%s%s\n", USAGE_LABEL, argv_program);
  
  /* run through the argument structure */
  for (argp = (argv_t *)args; argp->ar_short_arg != ARGV_LAST; argp++) {
    
    /* skip or specifiers */
    if (argp->ar_short_arg == ARGV_OR || argp->ar_short_arg == ARGV_XOR)
      continue;
    
    /* indent to the short-option colc */
    (void)fprintf(stderr, "%*.*s", SHORT_COLUMN, SHORT_COLUMN, "");
    
    /* start column counter */
    colc = SHORT_COLUMN;
    
    /*
     * print the mandatory argument desc
     */
    if (argp->ar_short_arg == ARGV_MAND) {
      
      if (argp->ar_var_label == NULL) {
	(void)fprintf(stderr, "%-.*s",
		      COMMENT_COLUMN - SHORT_COLUMN - 1,
		      UNKNOWN_ARG);
	colc += MIN(COMMENT_COLUMN - SHORT_COLUMN - 1,
		    UNKNOWN_ARG_LENGTH);
      }
      else {
	(void)fprintf(stderr, "%-.*s",
		      COMMENT_COLUMN - SHORT_COLUMN - 1,
		      argp->ar_var_label);
	colc += MIN(COMMENT_COLUMN - SHORT_COLUMN - 1,
		    strlen(argp->ar_var_label));
      }
    }
    else if (argp->ar_short_arg == ARGV_MAYBE) {
      
      if (argp->ar_var_label == NULL) {
	(void)fprintf(stderr, "[%-.*s]",
		      (COMMENT_COLUMN - SHORT_COLUMN - 1) - 1 - 1,
		      UNKNOWN_ARG);
	colc += MIN(COMMENT_COLUMN - SHORT_COLUMN - 1,
		    1 + UNKNOWN_ARG_LENGTH + 1);
      }
      else {
	(void)fprintf(stderr, "[%-.*s]",
		      (COMMENT_COLUMN - SHORT_COLUMN - 1) - 1 - 1,
		      argp->ar_var_label);
	colc += MIN(COMMENT_COLUMN - SHORT_COLUMN - 1,
		    1 + strlen(argp->ar_var_label) + 1);
      }
    }
    else {
      /* must be a - prefaced argument */
      (void)fprintf(stderr, "[%s%c",
		    SHORT_PREFIX, argp->ar_short_arg);
      colc += 2 + SHORT_PREFIX_LENGTH;
      
      switch(ARGV_TYPE(argp->ar_type)) {
	
      case ARGV_BOOL:
      case ARGV_BOOL_NEG:
	(void)fprintf(stderr, "]");
	colc++;
	break;
	
      case ARGV_CHAR:
      case ARGV_CHARP:
      case ARGV_FLOAT:
      case ARGV_SHORT:
      case ARGV_INT:
      case ARGV_LONG:
      case ARGV_BIN:
      case ARGV_OCT:
      case ARGV_HEX:
	if (argp->ar_var_label == NULL) {
	  (void)fprintf(stderr, " %-.*s]",
			MAX_ARGUMENT, UNKNOWN_ARG);
	  colc += 2 + MIN(MAX_ARGUMENT, UNKNOWN_ARG_LENGTH);
	}
	else {
	  (void)fprintf(stderr, " %-.*s]",
			MAX_ARGUMENT, argp->ar_var_label);
	  colc += 2 + MIN(MAX_ARGUMENT,
			  strlen(argp->ar_var_label));
	}
	break;
      }
    }
    
    /* put the long-option message on the correct line */
    if (colc < LONG_COLUMN) {
      (void)fprintf(stderr, "%*.*s",
		    LONG_COLUMN - colc, LONG_COLUMN - colc, "");
      colc = LONG_COLUMN;
    }
    
    /* print the long-option message */
    if (argp->ar_long_arg != NULL) {
      (void)fprintf(stderr, "%s%s%-.*s",
		    LONG_LABEL, LONG_PREFIX,
		    MAX_LONG_OPTION, argp->ar_long_arg);
      colc += LONG_LABEL_LENGTH + LONG_PREFIX_LENGTH +
	MIN(MAX_LONG_OPTION, strlen(argp->ar_long_arg));
    }
    
    /* put the comment message on the correct line */
    if (colc < COMMENT_COLUMN) {
      (void)fprintf(stderr, "%*.*s",
		    COMMENT_COLUMN - colc,
		    COMMENT_COLUMN - colc, "");
      colc = COMMENT_COLUMN;
    }
    
    /* print the help-message */
    if (argp->ar_comment != NULL) {
      (void)fprintf(stderr, "%s%-.*s",
		    COMMENT_LABEL,
		    MAX_COMMENT, argp->ar_comment);
      (void)fprintf(stderr, " %%%c%c",
		    ARGV_TYPE_CHARS[ARGV_TYPE(argp->ar_type)],
		    (ARGV_ARRAY & argp->ar_type ? ARRAY_CHAR : ' '));
    }
    
    (void)fprintf(stderr, "\n");
  }
  
  /* print the usage message header */
  (void)fprintf(stderr, "\n");
  print_long_special();
}

/***************************** exported routines *****************************/

/*
 * processes ARGC number of arguments from ARGV depending on argument
 * info array ARGS (if null then an empty array is used).  this
 * routine will not modify the argv array in any way.
 */
EXPORT	void	argv_process(const argv_t * args, const int argc,
			     char ** argv)
{
  char		*used = NULL, *mand = NULL, *maybe = NULL;
  const char	*progp;
  int		arrc, num_args;
  argv_t	*argp;
  
  if (args == NULL)
    args = empty;
  
  if (argc < 0) {
    (void)fprintf(stderr,
		  "%s: %s, argc argument to argv_process is %d\n",
		  __FILE__, INTERNAL_ERROR_NAME, argc);
    (void)exit(EXIT_CODE);
  }
  
  if (argv == NULL) {
    (void)fprintf(stderr,
		  "%s: %s, argv argument to argv_process is NULL\n",
		  __FILE__, INTERNAL_ERROR_NAME);
    (void)exit(EXIT_CODE);
  }
  
  /* set global variables */
  argv_argv = argv;
  argv_argc = argc;
  
  /* process argv[0] */
  progp = (char *)rindex(*argv, '/');
  if (progp == NULL)
    progp = *argv;
  else
    progp++;
  
  /* so we can step on the environmental space */
  (void)strncpy(argv_program, progp, PROGRAM_NAME);
  
  /* count the args */
  num_args = 0;
  for (argp = (argv_t *)args; argp->ar_short_arg != ARGV_LAST; argp++)
    num_args++;
  
  /* initialize marker arrays */
  if (num_args > 0) {
    used = (char *)malloc(num_args * sizeof(char));
    for (arrc = 0; arrc < num_args; arrc++)
      used[arrc] = ARGV_FALSE;
    mand = (char *)malloc(num_args * sizeof(char));
    for (arrc = 0; arrc < num_args; arrc++)
      mand[arrc] = ARGV_FALSE;
    maybe = (char *)malloc(num_args * sizeof(char));
    for (arrc = 0; arrc < num_args; arrc++)
      maybe[arrc] = ARGV_FALSE;
    
    /* allocate our value queue */
    QUEUE_ALLOC(int, num_args);
  }
  
  /* count and verify the argument array */
  preprocess_array(args, num_args, mand, maybe);
  
  /*
   * handle the args from the ENV variable
   */
  {
    int		envc, envn;
    char	**vectp, env_name[256], *environp;
    
    /* create the env variable */
    (void)sprintf(env_name, ENVIRON_FORMAT, argv_program);
    
    /* NOTE: by default the env name is all uppercase */
    for (environp = env_name; *environp != '\0'; environp++)
      if (islower(*environp))
	*environp = toupper(*environp);
    
    environp = (char *)getenv(env_name);
    if (environp != NULL) {
      
      /* break the list into tokens and do the list */
      environp = (char *)strdup(environp);
      vectp = _argv_vectorize(environp, " \t", &envn);
      if (vectp != NULL) {
	do_list(args, num_args, envn, vectp, used, mand, maybe);
	
	/* free token list */
	for (envc = 0; envc < envn; envc++)
	  free(vectp[envc]);
	free(vectp);
      }
      free(environp);
    }
  }
  
  /* do the external args */
  do_list(args, num_args, argc - 1, argv + 1, used, mand, maybe);
  
  /* if we allocated the space then free it */
  if (num_args > 0) {
    free(used);
    free(mand);
    free(maybe);
    QUEUE_FREE();
  }
}

/*
 * print the standard usage messages for argument array ARGS (if null
 * then an empty array is used).  WHICH chooses between long or short
 * messages (see argv.h).
 * NOTE: if this is called before argv_process then the program name
 * may be messed up.
 */
EXPORT	void	argv_usage(const argv_t * args, const int which)
{
  if (args == NULL)
    args = empty;
  
  if (which == ARGV_USAGE_LONG)
    usage_long(args);
  else
    usage_short(args);
}

/*
 * frees up any allocations in ARGS that may have been done by
 * argv_process.  This should be done at the end of the program or
 * after all the arguments have been referenced.
 */
EXPORT	void	argv_cleanup(const argv_t * args)
{
  argv_t	*argp;
  
  if (args == NULL)
    args = empty;
  
  /* run through the argument structure */
  for (argp = (argv_t *)args; argp->ar_short_arg != ARGV_LAST; argp++) {
    if (ARGV_ARRAY & argp->ar_type) {
      argv_array_t	*arrp = (argv_array_t *)argp->ar_variable;
      
      if (arrp->aa_entryn > 0)
	free(arrp->aa_entries);
      arrp->aa_entryn = 0;
    }
  }
}

/*
 * copy all the args (after the 0th), one after the other, into BUF of
 * MAX_SIZE.  NOTE: you can get the 0th argument from argv_argv[0].
 */
EXPORT	void	argv_copy_args(char * buf, const int max_size)
{
  char	**argvp, *bufp = buf, *argp;
  int	argc, sizec = max_size;
  
  if (max_size == 0)
    return;
  
  *bufp = '\0';
  
  if (argv_argv == NULL || max_size == 1)
    return;
  
  for (argvp = argv_argv + 1, argc = 1; argc < argv_argc; argvp++, argc++) {
    
    if (sizec < 2)
      break;
    
    if (argvp > argv_argv + 1) {
      *bufp++ = ' ';
      sizec--;
    }
    
    for (argp = *argvp; *argp != '\0' && sizec >= 2; sizec--)
      *bufp++ = *argp++;
  }
  
  *bufp = '\0';
}
