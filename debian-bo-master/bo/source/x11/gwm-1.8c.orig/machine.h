/* Copyright 1989 GROUPE BULL -- See license conditions in file COPYRIGHT
 * Copyright 1989 Massachusetts Institute of Technology
 */
/*******************************\
*                               *
*  Machine - dependent patches  *
*                               *
\*******************************/

/*****************************************************************************\
* 				OS Description                                *
\*****************************************************************************/
/* To describe your OS, you can #define those flags:
 *
 * NO_GETPAGESIZE    if you dont have getpagesize()
 * SYSV_TIME         if you cannot use BSD ftime function
 * SYSV_STRINGS      if you include <string.h> instead of <strings.h>
 * SYSV_UTSNAME      if you don't have gethostname()
 * SYSV_SIGNALS      if you dont have wait3()
 * NO_BCOPY          if you dont have bcopy and friends
 * HAS_STRCHR	     if strchr already exists
 * NO_MALLOC_DECLARE if your includes already declares char *malloc()
 */

#if defined(SYSV) || defined(SVR4)			/* Generic SYSV */
#define NO_GETPAGESIZE
#define SYSV_TIME
#define SYSV_STRINGS
#define SYSV_UTSNAME
#define SYSV_SIGNALS
#ifndef VOID_MALLOC
#define VOID_MALLOC
#endif
#ifndef VOID_SIGNALS
#define VOID_SIGNALS
#endif
#define NO_BCOPY
#define HAS_STRCHR
#endif /* defined(SYSV) || defined(SVR4) */

#ifdef BOSF				/* BULL OSF Unix. Mix of BSD and SYSV*/
#define SYSV_TIME
#define NO_MALLOC_DECLARE
#define HAS_STRCHR
#endif /* BOSF */

#ifdef __USLC__				/* UnixWare 1.1.2 (SVR4.2) C compiler */
#define DO_NOT_REDEFINE_MALLOC 
#endif
					/* BSD means no flags (normal) */

/*****************************************************************************\
* 			   automatic customizations                           *
\*****************************************************************************/

#ifdef sparc
#define NO_BCOPY			/* bzero buggy on sun4 */
#endif

#ifdef apollo				/* no struct field acces on apollo */
#define NO_STRUCTURE_OFFSETS
#endif

#ifdef linux
#ifndef LINUX				/* linux ==> LINUX */
#define LINUX
#endif
#endif

#ifdef LINUX
#ifndef _BSD_SOURCE
#define SYSV
#endif
#define VOID_SIGNALS
#define VOID_MALLOC
#define DO_NOT_REDEFINE_MALLOC
#endif /* LINUX */

#ifdef __FreeBSD__
#define SYSV_TIME
#define VOID_SIGNALS
#define VOID_MALLOC
#define DO_NOT_REDEFINE_MALLOC
#endif

#ifdef _AIX
#ifndef NEED_SELECT_H
#define NEED_SELECT_H
#endif /* !NEED_SELECT_H */
#endif /* _AIX */


/*****************************************************************************\
* 				    bcopy                                     *
\*****************************************************************************/

#ifdef NO_BCOPY

#define bcopy(source, dest, count) memcpy(dest, source, count)
#define bzero(dest, count) memset(dest, 0, count)
#define bcmp(source, dest, count) memcmp(source, dest, count)

#endif /* NO_BCOPY */

/*****************************************************************************\
* 			 strchr implemented as index                          *
\*****************************************************************************/

#ifdef HAS_STRCHR

#ifdef DO_NOT_DECLARE_STRCHR
#define DECLARE_strchr
#define DECLARE_strrchr
#else
#define DECLARE_strchr extern char *strchr()
#define DECLARE_strrchr extern char *strrchr()
#endif

#else /* HAS_STRCHR */

#define DECLARE_strchr extern char *index()
#define DECLARE_strrchr extern char *rindex()
#define strchr(string, char) index(string, char)
#define strrchr(string, char) rindex(string, char)

#endif /* HAS_STRCHR */

#ifdef __GNUC__
#define	Abs(x)	__builtin_abs(x)
#endif
#ifdef __HIGHC__
#define	Abs(x)	_abs(x)
#endif

#ifdef USE_STANDARD_MALLOC
#ifdef DO_NOT_REDEFINE_MALLOC
#undef DO_NOT_REDEFINE_MALLOC
#endif
#endif
