/*
 * proto.h -- prototypes for library functions.
 */

/*
 * The following code is operating-system dependent. [@proto.01].
 *  Prototypes for library functions.
 */

#if PORT
Deliberate Syntax Error
#endif					/* PORT */

#if AMIGA
#if LATTICE
#include <dos.h>
#endif					/* LATTICE */
#include "../h/dproto.h"
#endif					/* AMIGA */

#if ATARI_ST
#include "../h/dproto.h"
#endif					/* ATARI_ST */

#if ARM
#include <stdlib.h>
#include <string.h>

/* Our verson of unlink, which will fail if the named file does not exist */
extern int unlink (const char *name);

/* Force a cast of the function parameter to qsort() */
#define	qsort(base,n,size,cmp) \
	(qsort)((base),(n),(size),(int (*) (const void *, const void *))(cmp))
#endif					/* ARM */


#if MACINTOSH
#if MPW || THINK_C
#include <stdlib.h>
#include <string.h>
char    *strchar    Params((char *s, int i));
char	*ecvt		Params((double value, int count, int *dec, int* sign));
#else					/* MPW || THINK_C */
#include "../h/dproto.h"
#endif					/* MPW || THINK_C */
#endif					/* MACINTOSH */

#if MSDOS || OS2
#if HIGHC_386
#include <stdlib.h>
#include <string.h>
int	brk		Params((pointer p));
pointer sbrk		Params((msize n));
#endif					/* HIGHC_386 */
#if INTEL_386
#include <dos.h>
#include <stdlib.h>
#include <string.h>
int	brk		Params((pointer p));
#endif					/* INTEL_386 */
#if OS2_32
/* C Set/2 has gcvt #defined */
#if CSET2
#ifdef IconGcvt
#undef gcvt			/* gets rid of warning message */
#include <stdlib.h>
#undef gcvt
#define gcvt icon_gcvt
#else					/* IconGcvt */
#include <stdlib.h>
#endif					/* IconGcvt */
#else					/* C Set/2 */
#include <stdlib.h>
#endif					/* C Set/2 */
#include <string.h>
#endif					/* OS2_32 */
#if MICROSOFT || TURBO || ZTC_386 || WATCOM || NT || BORLAND_286 || BORLAND_386 || SCCX_MX
#include <dos.h>
#ifdef StandardLib
#include <string.h>
#include <stdlib.h>
#else					/* StandardLib */
#include "../h/dproto.h"
#endif					/* StandardLib */
#endif					/* MICROSOFT || TURBO || ZTC_386 ... */
#endif					/* MSDOS || OS2 */

#if UNIX
#ifdef StandardLib
#include <stdlib.h>
#include <string.h>
/*
 * index() is not part of the standard library.  We use no prototype
 *  because various systems disagree on the details, and they complain.
 */
#ifndef index
char	*index	();
#endif					/* index */
#else					/* StandardLib */
#include "../h/dproto.h"
#endif					/* StandardLib */
#endif					/* UNIX */

#if VM || MVS
/* ****  TEMPORARY *** */
novalue abort           Params((noargs));
double  atof            Params((char *));
long    atol            Params((char *));
pointer calloc          Params((unsigned,unsigned));
novalue exit            Params((int));
char    *getenv         Params((char *));
pointer malloc          Params((msize));
pointer realloc         Params((pointer, unsigned));
#ifdef StandardC
#include <string.h>
#else                                   /* StandardC */
char    *strchr         Params((char *s, int i));
int     strcmp          Params((char *s1, char *s2));
char    *strcpy         Params((char *s1, char *s2));
char    *strncat        Params((char *s1, char *s2, int n));
int     strncmp         Params((char *s1, char *s2, int n));
char    *strncpy        Params((char *s1, char *s2, int n));
char	*ecvt		Params((double value, int count, int *dec, int* sign));
double	pow		Params((double x, double y));
#ifdef SystemFnc
int	system		Params((char *));
#endif					/* SystemFnc */
#endif                                  /* StandardC */
#endif                                  /* VM || MVS */

#if VMS
#include "../h/dproto.h"
#endif					/* VMS */

/*
 * End of operating-system specific code.
 */

#ifdef KeyboardFncs
int	getch		Params((noargs));
int	getche		Params((noargs));
int	kbhit		Params((noargs));
#endif					/* KeyboardFncs */

#ifdef MSWindows
#if BORLAND_286
#define lstrlen longstrlen
#include <ctype.h>
#endif
#endif
#include "../h/mproto.h"

#ifdef ConsoleWindow
int Consolefprintf(FILE *file, char *format, ...);
int Consoleprintf(char *format, ...);
int Consoleputc(int c, FILE *file);
int Consolefflush(FILE *file);
#endif					/* ConsoleWindow */
/*
 * These must be after prototypes to avoid clash with system
 * prototypes.
 */

#if IntBits == 16
#define sbrk lsbrk
#define strlen lstrlen
#define qsort lqsort
#endif					/* IntBits == 16 */
