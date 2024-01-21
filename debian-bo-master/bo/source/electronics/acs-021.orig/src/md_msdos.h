/*$Id: md_msdos.h,v 11.39 96/03/30 16:05:00 al Exp $ -*- C++ -*-
 * Special stuff for msdos (640k)
 */
#ifndef MD_MSDOS_H
#define MD_MSDOS_H
/*--------------------------------------------------------------------------*/
/* standard collection of includes */
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <memory.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* std c++ collection of includes */
#include <complex.h>
#include <new.h>

/* non-standard collection of includes */
#include <conio.h>
#include <direct.h>
#include <fcntl.h>
#include <float.h>
#include <io.h>
#include <malloc.h>
#include <values.h>

/* constants related to memory size, word size, etc */
enum {
  BUFLEN = 128,
  BIGBUFLEN = 2048,
  MAXWIDTH = 256,
  PROBECOUNT = 200,
  MAXEVENTCOUNT = 500,
  MAXFAULTS = 24,
  DCNEST = 4,
  RECURSE = 8,
  LABELEN = 8,
  MAXHANDLE = ((int)((CHAR_BIT*sizeof(int))-1))
};

/* file names, etc. */
#define BEGINDIR	""
#define DIRSEP		"/\\"
#define	ENDDIR		"/\\"
#define PATHSEP		';'
#define SYSTEMSTARTFILE	"acs.rc"
#define SYSTEMSTARTPATH	getenv("PATH")
#define USERSTARTFILE	"acs.rc"
#define	USERSTARTPATH	getenv("HOME")
#define STEPFILE   	"/tmp/SXXXXXX"
#define PLOTFILE    	"acs.plt"
#define HELPFILE    	"acs.hlp"
#define HELPPATH	getenv("PATH")


/* a work around for the coming standard on complex */
typedef complex COMPLEX;

/* stuff that seems to be always missing */
#define F_OK (00)		/* io.h */
#define X_OK (01)
#define W_OK (02)
#define R_OK (04)

#define MAXPATHLEN (1024)

#include "md_bool.h"

/* a work around for old compilers (line this) that don't do const correctly */
#define mutable
#define CONST

/* Borland's DBL_MAX is defined to a variable that has been initialized
 * to NaN.  It is supposed to be a constant with the biggest legal value.
 * Redefine it here to a real constant.
 */
#undef DBL_MAX
#define DBL_MAX MAXDOUBLE

/* there's no getrusage.  fake it */
#define RUSAGE_SELF	0
struct timeval {
	long	tv_sec;		/* seconds */
	long	tv_usec;	/* and microseconds */
};
struct	rusage {
	struct timeval ru_utime;	/* user time used */
	struct timeval ru_stime;	/* system time used */
};
void getrusage(int,struct rusage*);

/* exp() has underflow problems, work around */
inline double Exp(double x){return (x>-200.) ? exp(x) : 0.;}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
