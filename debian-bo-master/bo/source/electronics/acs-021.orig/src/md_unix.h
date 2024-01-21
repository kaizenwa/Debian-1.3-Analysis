/*$Id: md_unix.h,v 11.39 96/03/30 16:05:02 al Exp $ -*- C++ -*-
 * Special stuff for all unix systems
 */
#ifndef MD_UNIX_H
#define MD_UNIX_H
/*--------------------------------------------------------------------------*/
/* standard collection of includes */
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>		/* limits must precede float (SGI) */
#include <float.h>
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

/* usual but may be non-standard collection of includes */
#include <unistd.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/time.h>		/* time must precede resource */
#include <sys/resource.h>

/* constants related to memory size, word size, etc */
enum {
  BUFLEN = 256,
  BIGBUFLEN = 2048,
  MAXWIDTH = 512,
  PROBECOUNT = 500,
  MAXEVENTCOUNT = 2000,
  MAXFAULTS = 100,
  DCNEST = 4,
  RECURSE = 8,
  LABELEN = 8,
  MAXHANDLE = ((int)((CHAR_BIT*sizeof(int))-1))
};

/* file names, etc. */
#define BEGINDIR	""
#define DIRSEP		"/"
#define	ENDDIR		"/"
#define PATHSEP		':'
#define SYSTEMSTARTFILE	"acs.rc"
#define SYSTEMSTARTPATH	getenv("PATH")
#define USERSTARTFILE	".acsrc"
#define	USERSTARTPATH	getenv("HOME")
#define STEPFILE   	"/tmp/SXXXXXX"
#define PLOTFILE    	"acs.plot"
#define HELPFILE    	"acs.hlp"
#define HELPPATH	getenv("PATH")
/*--------------------------------------------------------------------------*/
#if defined(__GNUG__)
  #define MANUAL_TEMPLATES
#else
  #define LINK_TEMPLATES
#endif

#if defined(__hppa__)
  extern "C" int  getrusage(int,struct rusage*);
  //#include <sys/syscall.h>
  //inline void getrusage(int a, struct rusage* b){syscall(SYS_GETRUSAGE,a,b);}
#elif defined(SGI_CC)
  inline double pow(double x, int e){return pow(x,(double)e);}
#elif defined(__sun__)
  extern "C" int  getrusage(int,struct rusage*);
  #if !defined(__GNUG__)
    #define SIGNALARGS int,...
    inline void free(void* x){free((char*)x);}
    inline void* realloc(void* x, size_t n){return realloc((char*)x, n);}
  #endif 
#elif defined(__NeXT__)
  #define INLINE_MATH
#endif
/*--------------------------------------------------------------------------*/
#if defined(__STD_COMPLEX)
  typedef complex<double> COMPLEX;
#else
  typedef complex COMPLEX;
#endif

#if defined(HAS_EXP_BUG)
   inline double Exp(double x){return (x>-200.) ? exp(x) : 0.;}
#else
   inline double Exp(double x){return exp(x);}
#endif

#if !defined(SIGNALARGS)
  #define SIGNALARGS ...
#endif

#include "md_bool.h"
#include "md_const.h"
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
