/* @(#) striphdrs.c,v 1.2 1992/05/23 01:01:55 tron Exp */

/*
 * Clean up headers in mail destined for a mailing list. I usually invoke
 * this from the smail alias file as follows:
 *
 * foo:	"|/usr/local/smail/striphdrs|/usr/local/bin/smail -oi -q -f foo-request foo-redist"
 *
 * Written January 1991 (or there abouts) by Lyndon Nerenberg.
 * This program is in the public domain.
 *
 * --lyndon@ampr.ab.ca
 */

#ifndef lint
static char RCSid[] = "striphdrs.c,v 1.2 1992/05/23 01:01:55 tron Exp";
#endif /* ! lint */

#include <stdio.h>
#include <ctype.h>
#ifdef SYSV
#include <string.h>
#else /* ! SYSV */
#include <strings.h>
#endif /* ! SYSV */
#ifndef NO_MALLOC_H
#include <malloc.h>
#endif /* NO_MALLOC_H */

/*
 * Define MAILLIST if you're using this to filter messages destined for
 * a mailing list exploder. This ensures messages have a "Precedence: bulk"
 * header, and strips out any Return-Receipt-To: header - something you
 * don't want leaking out to the entire mailing list.
 */

#if !defined(MAILLIST) && !defined(NO_MAILLIST)
#define MAILLIST
#endif	/* ! MAILLIST && ! NO_MAILLIST */

#ifdef sun
extern void exit();
#endif /* sun */

#define INBUFSIZE 4096		/* Size of input buffer. Lines longer than */
				/* this will be truncated. */
#define	TRUE	1
#define FALSE	0

static int strncasecmp();

static char *hdr_del[] = {	/* NULL terminated list of hdrs to delete */
  "Return-Path:",
  "Received:",
  "Errors-To:",
#ifdef MAILLIST
  "Return-Receipt-To:",
#endif /* MAILLIST */
  "Sender:",
  "Precedence:",
  NULL };

main(argc, argv)
  int argc; char *argv[];
{
  
  register char *inbuf;		/* Input buffer */
  register char **c;		/* Temporary pointer */
  int  in_headers = TRUE;	/* Set to 0 when last header encountered */
  int  deleting = FALSE;	/* Set to 1 if actively deleting header */
  
  inbuf = (char *) malloc((unsigned) INBUFSIZE);
  if (inbuf == NULL) {
    (void) fprintf(stderr, "%s: malloc(INBUFSIZE) failed!\n", argv[0]);
    exit(1);
  }
  
  while ((fgets(inbuf, INBUFSIZE, stdin)) != NULL) {
    
    if (in_headers) {
      
      if (*inbuf == '\n') {
	in_headers = FALSE;	/* Header/body seperator found */
#ifdef MAILLIST
	(void) fputs("Precedence: bulk\n\n", stdout);
#else /* ! MAILLIST */
	(void) fputs("\n\n", stdout);
#endif /* ! MAILLIST */
	continue;
      }

      if (deleting && ((*inbuf == ' ') || (*inbuf == '\t')))
	continue;		/* Skip any continuation lines */
      else
	deleting = FALSE;
      
      /* See if this is a bogus header */
      for (c = hdr_del; *c != NULL; c++)
	if (strncasecmp(inbuf, *c, strlen(*c)) == 0)
	  deleting = TRUE;

      if (!deleting)
	(void) fputs(inbuf, stdout);
    }
    else
      (void) fputs(inbuf, stdout);
  }
  exit(0);
/*NOTREACHED*/
}

static int
strncasecmp(s1, s2, n)
    char *s1;
    char *s2;
    int n;
{
    int c1, c2;
    int cnt = n;

    while (*s1 && *s2 && cnt > 0) {
	if (isupper(c1 = *s1++)) {
	    c1 = tolower(c1);
	}
	if (isupper(c2 = *s2++)) {
	    c2 = tolower(c2);
	}
	if (c1 != c2) {
		return c1-c2;
	}
	cnt--;
    }

    return cnt ? (int)((*s1)-(*s2)) : 0;
}
