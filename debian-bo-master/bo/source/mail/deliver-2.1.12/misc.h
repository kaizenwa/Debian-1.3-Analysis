/* $Id: misc.h,v 1.6 1993/10/28 16:49:51 chip Exp $
 *
 * Miscellaneous definitions.
 *
 * $Log: misc.h,v $
 * Revision 1.6  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.5  1991/08/27  17:41:21  chip
 * Use DECLARE_*.
 *
 * Revision 1.4  1991/08/27  15:39:57  chip
 * Add asctime().  Remove malloc(), realloc(), free().
 *
 * Revision 1.3  1991/08/21  22:15:33  chip
 * Careful creation for NFS.
 *
 * Revision 1.2  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

/*
 * Constants
 */

#undef NULL
#define NULL    0		/* The One True NULL */

#define FALSE   0
#define TRUE    1

/*
 * Macros.
 */

/* Function parameter definition macro.  */

#if ANSI_C
# define P(x)  x
#else
# define P(x)  ()
#endif

/* Length parameter for fgets() on given buffer. */

#define GETSIZE(buf)    (int) (sizeof(buf) - 1)

/* How to declare functions that don't return
   and functions that are like printf.  */

#ifdef __GNUC__
# if __GNUC__ < 2
#  define NORETURN	volatile
#  define ATTRIBUTE(x)	/**/
# else
#  define NORETURN	/**/
#  define ATTRIBUTE(x)	__attribute__(x)
# endif
#else  /* not GCC */
# define NORETURN	/**/
# define ATTRIBUTE(x)	/**/
#endif /* GCC */

/*
 * Public data.
 */

extern char **environ;

#ifdef DECLARE_TZNAME
extern char *tzname[2];
#endif

#ifdef DECLARE_ERRNO
extern int errno;
#endif

/*
 * Declare library functions.
 */

extern char *ctime();
extern char *asctime();
extern char *getenv();
extern char *getlogin();
extern char *mktemp();
extern int putenv();
extern long lseek();
extern long time();
extern void exit();

#ifdef DECLARE_SIGNAL
extern SIGTYPE(*signal()) ();
#endif
