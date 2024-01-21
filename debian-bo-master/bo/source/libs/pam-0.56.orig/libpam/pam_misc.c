/* pam_misc.c -- This is random stuff */

/* $Id: pam_misc.c,v 1.8 1997/02/15 15:59:46 morgan Exp $
 *
 * $Log: pam_misc.c,v $
 * Revision 1.8  1997/02/15 15:59:46  morgan
 * modified ..strCMP comment
 *
 * Revision 1.7  1996/12/01 03:14:13  morgan
 * use _pam_macros.h
 *
 * Revision 1.6  1996/11/10 20:05:52  morgan
 * name convention _pam_ enforced. Also modified _pam_strdup()
 *
 * Revision 1.5  1996/07/07 23:57:14  morgan
 * deleted debuggin function and replaced it with a static function
 * defined in pam_private.h
 *
 * Revision 1.4  1996/06/02 08:00:56  morgan
 * added StrTok function
 *
 * Revision 1.3  1996/05/21 04:36:58  morgan
 * added debugging information
 * replaced the _pam_log need for a local buffer with a call to vsyslog()
 * [Al Longyear had some segfaulting problems related to this]
 *
 * Revision 1.2  1996/03/16 21:55:13  morgan
 * changed pam_mkargv to _pam_mkargv
 *
 */

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <ctype.h>

#include "pam_private.h"

/* caseless string comparison: POSIX does not define this.. */
int _pam_strCMP(const char *s, const char *t)
{
    int cf;

    do {
	cf = tolower(*s) - tolower(*t);
	++t;
    } while (!cf && *s++);

    return cf;
}

char *_pam_StrTok(char *from, const char *format, char **next)
/*
 * this function is a variant of the standard strtok, it differs in that
 * it takes an additional argument and doesn't nul terminate tokens until
 * they are actually reached.
 */
{
     char table[256];
     int i;

     if (from == NULL && (from = *next) == NULL)
	  return from;

     for (i=1; i<256; table[i++] = '\0');
     for (i=0; format[i] ; table[(int)format[i++]] = 'y');

     /* look for first non-blank char */

     while (*from && table[(int)*from]) {
	  ++from;
     }

     if (*from) {
	  char *end;

	  /* look for next blank char */

	  for (end=from; *end && !table[(int)*end]; ++end);

	  if (*end)
	       *end++ = '\0';

	  if (*end) {
	       *next = end;
	  } else {
	       *next = NULL;                      /* have found last token */
	  }
     } else {
	  from = *next = NULL;                    /* no tokens left */
     }

     return from;
}

/*
 * Safe duplication of character strings. "Paranoid"; don't leave
 * evidence of old token around for later stack analysis.
 */

char *_pam_strdup(const char *x)
{
     register char *new=NULL;

     if (x != NULL) {
	  register int i;

	  for (i=0; x[i]; ++i);                       /* length of string */
	  if ((new = malloc(++i)) == NULL) {
	       i = 0;
	       _pam_log_error("_pam_strdup: failed to get memory");
	  } else {
	       while (i-- > 0) {
		    new[i] = x[i];
	       }
	  }
	  x = NULL;
     }

     return new;                 /* return the duplicate or NULL on error */
}

/* Generate argv, argc from s */
/* caller must free(argv)     */

int _pam_mkargv(char *s, char ***argv, int *argc)
{
    int l;
    int argvlen = 0;
    char *sbuf, *sbuf_start;
    char **our_argv = NULL;
    char **argvbuf;
    char *argvbufp;
#ifdef DEBUG
    int count=0;
#endif

    D(("_pam_mkargv called: %s",s));

    *argc = 0;

    l = strlen(s);
    if (l) {
	if ((sbuf = sbuf_start = _pam_strdup(s)) == NULL) {
	    _pam_log_error("pam_mkargv: null returned by _pam_strdup");
	    D(("arg NULL"));
	} else {
	    /* Overkill on the malloc, but not large */
	    argvlen = (l + 1) * ((sizeof(char)) + sizeof(char *));
	    if ((our_argv = argvbuf = malloc(argvlen)) == NULL) {
		_pam_log_error("pam_mkargv: null returned by malloc");
	    } else {
		char *tmp=NULL;

		argvbufp = (char *) argvbuf + (l * sizeof(char *));
		D(("[%s]",sbuf));
		while ((sbuf = _pam_StrTok(sbuf, " \n\t", &tmp))) {
		    D(("arg #%d",++count));
		    D(("->[%s]",sbuf));
		    strcpy(argvbufp, sbuf);
		    D(("copied token"));
		    *argvbuf = argvbufp;
		    argvbufp += strlen(argvbufp) + 1;
		    D(("stepped in argvbufp"));
		    (*argc)++;
		    argvbuf++;
		    sbuf = NULL;
		    D(("loop again?"));
		}
		_pam_drop(sbuf_start);
	    }
	}
    }
    
    *argv = our_argv;

    D(("_pam_mkargv returned"));

    return(argvlen);
}

void _pam_log_error(const char *format, ...)
{
    va_list args;

#ifdef DEBUG
#define ADDPERROR |LOG_PERROR
#else
#define ADDPERROR
#endif

    va_start(args, format);
    openlog("pam", LOG_CONS|LOG_PID ADDPERROR, LOG_AUTH);
    vsyslog(LOG_CRIT, format, args);
    va_end(args);
    closelog();
}

