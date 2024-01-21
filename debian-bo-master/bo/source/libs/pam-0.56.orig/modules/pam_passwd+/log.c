/*
 * this file contains ALL the logging routines
 */

#include "passwd.h"

/*
 * these are keywords for log levels
 */
struct logkey {
	const char *word;	/* what the user types */
	int wlen;		/* how long it is */
	unsigned short level;	/* what bit to set */
} levels[] = {
	{ "syntax",	6,	LG_SYNTAX,	},
	{ "use",	3,	LG_USE,		},
	{ "result",	6,	LG_RESULT,	},
	{ "item",	4,	LG_ITEM,	},
	{ "debug",	5,	LG_DEBUG,	},
	{ "system",	6,	LG_SYSTEM,	},
	{ "all",	3,	LG_ALL,		},
	{ (char *) 0,	0,	LG_NONE,	},
};

/*
 * initialize the logging
 */

void initlog (struct _options *opt)
  {
    beginlog (opt, (const char *) LG_INIT);
  }

/*
 * load the logging level
 */

void beginlog (struct _options *opt, const char *line)
  {
    int            sgn;		/* 1 if number is negative */
    int            indx;	/* counter in a for loop */
    const char	   *l1;		/* pointer in a for loop */
    char           *marker;	/* Pointer to the file name in memory */
    struct logkey  *lp;		/* walks logging list */
    struct loginfo *log;	/* Pointer to the free item */
    unsigned short logwhat;	/* what is to be logged */
/*
 * Fetch the initial logging level
 */
    logwhat = opt->log_default;
/*
 * skip leading blanks
 */
    while(isspace(*line))
      {
	++line;
      }
/*
 * now just walk the line
 */
    log = (struct loginfo *) 0;
    for (;;)
      {
	if (*line == '\0')
	  {
	    line = LG_OUTDEF;
	    break;
	  }
/*
 * skip leading blanks and commas
 */
	if (*line == ' ' || *line == ',')
	  {
	    line++;
	    continue;
	  }
/*
 * if at the end of the types, drop out
 */
	if (*line == '\t' || *line == '\0')
	  {
	    break;
	  }
/*
 * is it negation?
 */
	sgn = 0;
	if (*line == '!')
	  {
	    sgn = 1;
	    line++;
	  }
/*
 * look for the special case "clear"
 */
	if (strncmp(line, "clear", 5) == 0 && log_end(line[5]))
	  {
	    if (!sgn)
	      {
		log_reset(logwhat, LG_ALL);
	      }
	    line = &line[5];
	    continue;
	  }
/*
 * see if the word matches anything
 */
	lp = levels;
	for (;;)
	  {
	    if (lp->word == (char *) 0)
	      {
		while (*line && !isspace(*line) && *line != ',')
		  {
		    line++;
		  }
		break;
	      }

	    if (strncmp (line, lp->word, lp->wlen) == 0 &&
		!isalnum (line [lp->wlen]))
	      {
		if (sgn)
		  {
		    log_reset(logwhat, lp->level);
		  }
		else
		  {
		    log_set(logwhat, lp->level);
		  }
		line = &line[lp->wlen];
		break;
	      }

	    ++lp;
	  }
      }
/*
 * now go for the output
 */
    while (isspace(*line))
      {
	++line;
      }
/*
 * Ignore the spaces between the log type and the file name.
 */
    l1 = line;
    while (*line) {
        ++line;
	if (!isspace (*line))
	    break;
    }
/*
 * Generate the marker file
 */
    marker = malloc (strlen (line) + 2);
    if (marker == (char *) 0)
      {
	logfunc (opt, LG_SYSTEM,
		 "memory allocation error on line %d",
		 opt->linect);
	return;
      }
	
    *marker = *l1;
    strcpy (&marker[1], line);
/*
 * See if the current log file is open.
 * Find the first empty slot.
 */
    for (indx = 0; indx < MAXLOGTO; indx++)
      {
	if (! opt->logto[indx].flags)
	  {
	    if (log == (struct loginfo *) 0)
	      {
		log = &opt->logto[indx];
	      }
	    continue;
	  }
/*
 * if cleared, close it
 */
	if (strcmp(opt->logto[indx].loc, marker) == 0)
	  {
	    if (logwhat == LG_NONE)
	      {
		endlogging (opt, indx);
		free (marker);
		return;
	      }
/*
 * change the flags and quit
 */
	    opt->logto[indx].log = logwhat;
	    free (marker);
	    return;
	  }
      }
/*
 * if you're clearing something not set, you're done!
 */
    if (logwhat == LG_NONE)
      {
	free (marker);
	return;
      }
/*
 * If you don't have a free slot then complain.
 */
    if (log == (struct loginfo *) 0)
      {
	logfunc (opt, LG_SYSTEM,
		 "attempt to open more than %d log files on line %d",
		 MAXLOGTO, opt->linect);
	free (marker);
	return;
      }
/*
 * Define the appropriate file information.
 */
    do {
        if (strncmp("stderr", line, 6) == 0)	/* standard error */
	  {
	    log->locp  = stderr;
	    log->flags = LG_STDERR;
	    break;
	  }

	if (strncmp("syslog", line, 6) == 0)	/* syslog */
	  {
	    log->locp  = stderr;
	    log->flags = LG_SYSLOG;
	    break;
	  }

	if (*l1 == '|')			/* to a program */
	  {
	    log->locp  = popen(line, "w");
	    log->flags = LG_PIPE;
	    break;
	  }

	if (*l1 == '>')			/* to a file */
	  {
	    log->locp  = fopen(line, "a");
	    log->flags = LG_FILE;
	    break;
	  }

	logfunc (opt, LG_SYNTAX,
		 "bad location to log to on line %d (at \"%s\")",
		 opt->linect, l1);

	free (marker);
	return;
    } while (0);
/*
 * can't open a program or a file
 */
    if (log->locp == (FILE *) 0)
      {
	logfunc (opt, LG_SYSTEM, "cannot log to \"%s\" on line %d",
		 l1, opt->linect);
	free (marker);
	marker     = (char *) 0;
	logwhat    = 0;
	log->flags = 0;
      }

    log->log = logwhat;
    log->loc = marker;
  }

/*
 * logging function
 */

void logfunc (struct _options *opt, unsigned int flag, const char *fmt, ...)
  {
    va_list ap;
    register int indx;			/* counter in a for loop */

    va_start(ap, fmt);
/*
 * log the message in all appropriate logs
 */
    for (indx = 0; indx < MAXLOGTO; indx++)
      {
	if (log_test(opt->logto[indx].log, flag))
	  {
	    plus_logout (opt, indx, fmt, ap);
	  }
      }

    va_end (ap);
  }

/*
 * does the actual logging for one log
 */

void plus_logout (struct _options *opt, int logno, const char *fmt, va_list ap)
  {
/*
 * syslog is treated special if SYSLOG is defined
 * the rest just go out. Stderr is also special for the PAM routines.
 */
    if (opt->logto[logno].flags == LG_SYSLOG)
      {
	char buffer [BUFSIZ];
	vsprintf (buffer, fmt, ap);
	_pam_log_error ("%s", buffer);
	return;
      }

    if (opt->logto[logno].flags == LG_STDERR)
      {
	char buffer [BUFSIZ];
	vsprintf (buffer, fmt, ap);
	do_converse (opt, 0, (const char *) buffer);
	return;
      }

    vfprintf (opt->logto[logno].locp, fmt, ap);
  }

/*
 * close a log
 */

void endlogging (struct _options *opt, int logno)
{
    switch(opt->logto[logno].flags)
      {
    case LG_STDERR:			/* NEVER close stderr! */
	break;

    case LG_SYSLOG:			/* break connection on syslog() */
	break;

    case LG_PIPE:			/* close pipe for program */
	(void) pclose(opt->logto[logno].locp);
        break;

    case LG_FILE:			/* close file for file */
	(void) fclose(opt->logto[logno].locp);
        break;
      }
/*
 * Erase the current file
 */
    opt->logto[logno].log   = 0;
    opt->logto[logno].flags = 0;
    opt->logto[logno].loc   = 0;
    opt->logto[logno].locp  = (FILE *) 0;

    if (opt->logto[logno].loc) {
        free (opt->logto[logno].loc);
	opt->logto[logno].loc = (char *) 0;
    }
}

/*
 * report error on a pattern (malformed ...)
 */

void paterr(struct _options *opt, const char *msg)
{
    logfunc (opt, LG_SYNTAX,
	     "%s at line %d (at \"%s\")",
	     msg, opt->linect, opt->lptr-1);
}

/*
 * report system errors
 */

void sysyyerror (struct _options *opt, char *msg)
{
    char buf[BUFSIZ];	/* buffer for error message */
    char *ermsg;
/*
 * print the system error message if available,
 * or the error number if not
 */
    ermsg = strerror (errno);
    if (!ermsg || !*ermsg) {
        sprintf (buf, "unknown error #%d", errno);
	ermsg = buf;
    }

    logfunc (opt, LG_SYSTEM, "line %d: %s: %s at \"%s\"",
	     opt->linect, msg, ermsg, opt->lptr-1);
}
