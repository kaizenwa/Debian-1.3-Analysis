/* Smail SCCS ID: @(#) config.h,v 1.7 1993/12/07 01:08:05 tron Exp */
/* pathalias -- by steve bellovin, as told to peter honeyman */

/**************************************************************************
 * +--------------------------------------------------------------------+ *
 * |                    begin configuration section                     | *
 * +--------------------------------------------------------------------+ *
 **************************************************************************/

/*
 * For smail3, get definitions from the defs.h file and convert them
 * for use with pathalias
 */

#include "defs.h"
#undef lowercase	/* to avoid conflict with SMAIL macro */

#define SMAIL_3		/* changes for SMAIL-3 are #ifdef'd with this */

#undef STRCHR
#define BZERO
#undef MEMSET

#undef UNAME
#undef GETHOSTNAME
#ifdef HAVE_UNAME
#define UNAME
#else
#ifdef HAVE_GETHOSTNAME
#define GETHOSTNAME
#endif
#endif

/* default place for dbm output of makedb (or use -o at run-time) */
#ifdef PATH_PATHS_FILE
#define ALIASDB PATH_PATHS_FILE
#else
#define	ALIASDB	"/usr/local/lib/palias"
#endif

/*
 * pathalias defines an external named link, which clashes with
 * the link system call.  The link system call may have been defined
 * in one of the header files included by defs.h.
 */

#define link	palias_link

/**************************************************************************
 * +--------------------------------------------------------------------+ *
 * |                    end of configuration section                    | *
 * +--------------------------------------------------------------------+ *
 **************************************************************************/



#ifdef MAIN
#ifndef lint
static char	*c_sccsid = "@(#)config.h	9.2 89/03/03";
#endif /*lint*/
#endif /*MAIN*/

/*
 * malloc/free fine tuned for pathalias.
 *
 * MYMALLOC should work everwhere, so it's not a configuration
 * option (anymore).  nonetheless, if you're getting strange
 * core dumps (or panics!), comment out the following manifest,
 * and use the inferior C library malloc/free.
 */
#if !defined(SMAIL_3) && !defined(NO_PALIAS_MYMALLOC)
#define MYMALLOC	/**/
#endif

#ifdef MYMALLOC
#define malloc mymalloc
#define calloc(n, s) malloc ((n)*(s))
#define free(s)
#define cfree(s)
extern char *memget();
#else /* !MYMALLOC */
#if defined(SMAIL_3) && defined(ANSI_C) && !defined(NO_VOID_CALLOC)
extern void *calloc();
#else
extern char *calloc();
#endif
#endif /* MYMALLOC */

#ifndef SMAIL_3
#ifdef STRCHR
#define index strchr
#define rindex strrchr
#else
#define strchr index
#define strrchr rindex
#endif
#endif	/* SMAIL_3 */

#ifdef BZERO
#define strclear(s, n)	((void) bzero((s), (n)))
#else /*!BZERO*/

#ifdef MEMSET
extern char	*memset();
#define strclear(s, n)	((void) memset((s), 0, (n)))
#else /*!MEMSET*/
extern void	strclear();
#endif /*MEMSET*/

#endif /*BZERO*/

#if defined(SMAIL_3) && defined(ANSI_C) && !defined(NO_VOID_MALLOC)
extern void	*malloc();
#else
extern char	*malloc();
#endif
#ifndef SMAIL_3
extern char	*strcpy(), *index(), *rindex();
#endif

#ifndef STATIC

#ifdef DEBUG
#define STATIC extern
#else /*DEBUG*/
#define STATIC static
#endif /*DEBUG*/

#endif /*STATIC*/
