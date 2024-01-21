/* $Id: util2.c,v 3.6 1994/10/28 11:11:11 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "nntp.h"
#include "nntpauth.h"
#include "util.h"
#include "INTERN.h"
#include "util2.h"

/* return ptr to little string in big string, NULL if not found */

char *
instr(big, little, case_matters)
char *big, *little;
bool_int case_matters;
{
    register char *t, *s, *x;

    for (t = big; *t; t++) {
	for (x=t,s=little; *s; x++,s++) {
	    if (!*x)
		return Nullch;
	    if (case_matters == TRUE) {
		if(*s != *x)
		    break;
	    } else {
		register char c,d;
		if (isupper(*s)) 
		    c = tolower(*s);
		else
		    c = *s;
		if (isupper(*x)) 
		    d = tolower(*x);
		else
		    d = *x;
		if ( c != d )
		    break;
	   }
	}
	if (!*s)
	    return t;
    }
    return Nullch;
}

#ifdef SETENV
static bool firstexport = TRUE;
extern char **environ;

void
export(nam,val)
char *nam, *val;
{
    register int i=envix(nam);		/* where does it go? */

    if (!environ[i]) {			/* does not exist yet */
	if (firstexport) {		/* need we copy environment? */
	    int j;
#ifndef lint
	    char **tmpenv = (char**)	/* point our wand at memory */
		safemalloc((MEM_SIZE) (i+2) * sizeof(char*));
#else
	    char **tmpenv = Null(char **);
#endif /* lint */
    
	    firstexport = FALSE;
	    for (j=0; j<i; j++)		/* copy environment */
		tmpenv[j] = environ[j];
	    environ = tmpenv;		/* tell exec where it is now */
	}
#ifndef lint
	else
	    environ = (char**) saferealloc((char*) environ,
		(MEM_SIZE) (i+2) * sizeof(char*));
					/* just expand it a bit */
#endif /* lint */
	environ[i+1] = Nullch;	/* make sure it's null terminated */
    }
    environ[i] = safemalloc((MEM_SIZE) strlen(nam) + strlen(val) + 2);
					/* this may or may not be in */
					/* the old environ structure */
    sprintf(environ[i],"%s=%s",nam,val);/* all that work just for this */
}

int
envix(nam)
char *nam;
{
    register int i, len = strlen(nam);

    for (i = 0; environ[i]; i++) {
	if (strnEQ(environ[i],nam,len) && environ[i][len] == '=')
	    break;			/* strnEQ must come first to avoid */
    }					/* potential SEGV's */
    return i;
}
#endif
