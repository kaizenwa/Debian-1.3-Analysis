/*
 * various utilities, like stream readers, error handlers, etc.
 */

#include <stdio.h>
#include "passwd.h"
#include <signal.h>

/*
 * like fgets, but it clobbers the newline and returns -1 on EOF,
 * >= 0 on non-EOF.
 */

int mgets (struct _options *opt, char *buf, int n, FILE *fp)
  {
    int c;		/* input character */
    char *b = buf;	/* walks buffer storing characters */
/*
 * If the file is at the end then terminate now.
 */
    if (feof (fp))
      {
	return -1;
      }
/*
 * Load characters until EOF is found or the end of line is indicated.
 */
    for (;;)
      {
	c = getc (fp);
	if (c == EOF)
	  {
	    *b = '\0';
	    break;
	  }

	c &= 0xFF;
/*
 * Stop the read when the end of line condition is received.
 */
	if (c == '\n')
	  {
	    *b = '\0';
	    break;
	  }
/*
 * Do not attempt to store more than 'n' characters.
 */
	if (n > 1)
	  {
	    *b++ = (char) c;
	    --n;
	  }
      }
    
    return (b - buf);
  }

/*
 * Attempt to match the string (not as a pattern) against the contents of
 * the indicated file. Only 'pwsig' characters from the string are compared
 * to the file contents.
 */

int strfp (struct _options *opt, int neg, char *str,
	   char *name, FILE *(*fopn)(const char *,const char *),
	   int (*fclo)(FILE *))
  {
    char buf[BUFSIZ];	/* buffer for read (output) word */
    FILE *fp;			/* points to (output) stream */
    void (*child)(int);

    child = signal(SIGCHLD, SIG_IGN);
/*
 * open the file or output stream
 */
    fp = (*fopn) ((char *) name, "r");
    if (fp == (FILE *) 0)
      {
	sysyyerror (opt, name);
	(void) signal(SIGCHLD, child);
	return(!neg);
      }
/*
 * read each line and compare
 */
    while (mgets(opt, buf, BUFSIZ, fp) >= 0)
      {
	logfunc (opt, LG_DEBUG, "strfp: %s\n", buf);
/*
 * If a match then report success.
 */
	if (strncmp(str, buf, opt->pwsig) == 0)
	  {
	    (void) (*fclo)(fp);
	    (void) signal(SIGCHLD, child);
	    return (neg);
	  }
      }
/*
 * close the stream; report failure
 */
    (void) (*fclo)(fp);
    (void) signal(SIGCHLD, child);
    return(!neg);
  }

/*
 * Treat the 'str' as the pattern. See if the pattern is matched in the
 * file.
 */

int patfp(struct _options *opt, int neg, char *pat,
	  char *name,
	  FILE *(*fopn)(const char *, const char *), int (*fclo)(FILE *))
  {
    char buf[BUFSIZ];	/* buffer for read (output) word */
    FILE *fp;			/* points to (output) stream */
    void (*child)(int);
/*
 * if matching, set up the right routine
 */
    if (smatch(opt, pat))
        return(neg);

    child = signal(SIGCHLD, SIG_IGN);
/*
 * open the file or output stream
 */
    fp = (*fopn) ((char *) name, "r");
    if (fp == (FILE *) 0)
        sysyyerror (opt, name);
    else
      {
/*
 * read each line and compare
 */
	while (mgets (opt, buf, BUFSIZ, fp) >= 0)
	  {
	    logfunc (opt, LG_DEBUG, "patfp: %s\n", buf);
	    if (match (opt, buf))
	      {
/*
 * a match; report success
 */
		(void) (*fclo)(fp);
		(void) signal(SIGCHLD, child);
		free_pattern (opt);
		return(neg);
	      }
	  }
	(void) (*fclo)(fp);
      }
/*
 * close the stream; report failure
 */
    (void) signal(SIGCHLD, child);
    free_pattern (opt);
    return(!neg);
  }

/*
 * read the 'file' as a series of patterns and compare it to the input str.
 */

int patinfp (struct _options *opt, int neg, char *str,
	     char *name,
	     FILE *(*fopn)(const char *,const char *), int (*fclo)(FILE *))
  {
    char buf[BUFSIZ];		/* buffer for read (output) word */
    FILE *fp;			/* points to (output) stream */
    void (*child)(int);

    child = signal(SIGCHLD, SIG_IGN);
/*
 * open the file or output stream
 */
    fp = (*fopn) ((char *) name, "r");
    if (fp == (FILE *) 0)
      {
	sysyyerror (opt, name);
	(void) signal(SIGCHLD, child);
	return (!neg);
      }
/*
 * read each line and compare
 */
    while (mgets(opt, buf, BUFSIZ, fp) >= 0)
      {
	logfunc (opt, LG_DEBUG, "patinfp: %s\n", buf);
	if (smatch(opt, buf) == 0)
	  {
/*
 * If successful, stop the search.
 */
	    if (match(opt, str))
	      {
		(void) (*fclo)(fp);
		(void) signal(SIGCHLD, child);
		free_pattern (opt);
		return(neg);
	      }
	    free_pattern (opt);
	  }
      }
/*
 * close the stream; report failure
 */
    (void) (*fclo)(fp);
    (void) signal(SIGCHLD, child);
    return(!neg);
  }

/*
 * get the first line of output from a file or program
 */

int firstline (struct _options *opt, char *name,
	       char *buf, int nbuf,
	       FILE *(*fopn)(const char *,const char *), int (*fclo)(FILE *))
  {
    int rval = 1;			/* return value */
    FILE *fp;			/* points to (output) stream */
    void (*child)(int);

    child = signal(SIGCHLD, SIG_IGN);
/*
 * open the file or output stream
 */
    fp = (*fopn)((char *) name, "r");
    if (fp == (FILE *) 0)
      {
	sysyyerror (opt, name);
	(void) signal(SIGCHLD, child);
	return(0);
      }
/*
 * read and save first line; error on EOF
 * otherwise just toss the rest of the lines
 */
    rval = mgets(opt, buf, nbuf, fp);
    if (rval >= 0)
        rval = 1;

    logfunc (opt, LG_DEBUG, "firstline: %s\n", buf);
/*
 * close the stream; report failure
 */
    (void) (*fclo)(fp);
    (void) signal(SIGCHLD, child);
    return(rval);
}
