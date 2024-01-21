/* Process this file with autoheader to produce config.h.in */
/**** Things below you can change/configure */

/*  The default font to use */
#define SC_FONT "9x15"

/*  Define this if you want backups saved of the files */
#define DOBACKUPS

/* File name to use for an emergency save */
#define SAVENAME "SC.SAVE"

/*  The default file viewer */
#define DFLT_PAGER "less"

/*  Define CRYPT_PATH if you want crypt support compiled in */
#undef CRYPT_PATH

/*  If you want to run "des" instead of "crypt" for crypting. */
#undef DES

/*
 * Set SHOWCURSOR to TRUE if you want the default action to show a cursor,
 * set it to FALSE otherwise.  Set SHOWCURSORTAG the same way to change
 * the manual page.
 */
#define SHOWCURSOR	1

/* Define if you have case insensitive command lines (dos, etc.) */
#undef CASEINSENSITIVECMDLINE

/*
 * Set INTERNATIONAL if you need 8 bit characters.  You should
 * not set this if you are running 5.3.0.  I think it is OK in 5.3.1.
 */
#undef INTERNATIONAL


/**************************************************/
/* Things that might help with curses bugs on various system/curses versions */

/*
 * Define CURSES_NO_NONL_BUG if your curses has the nl/nonl bug
 * if it does and you don't set CURSES_NO_NONL_BUG, the display will
 * be staggered across the screen. Also try IDLOKBAD below.
 */
#undef CURSES_NO_NONL_BUG

/*
 * **** SYSV curses bugs... ****
 * Try setting IDLOKBAD to fix (with an empty spreadsheet):
 *	a) Redrawing the bottom half of the screen when you
 *	 	move between row 9 <-> 10
 *	b) the highlighted row labels being trash when you
 *		move between row 9 <-> 10
 *	c) On an xterm on Esix Rev. D+ from eating lines
 *		 -goto (or move) a few lines (or more) past the bottom
 *		 of the screen, goto (or move) to the top line on the
 *		 screen, move upward and the current line is deleted, the
 *		 others move up even when they should not, check by
 *		 noticing the rows become 2, 3, 40, 41, 42... (etc).
 *	Known systems/terminfos w/ curses problems:
 *	{Esix Rev. D+, AT&T SysV3.2.1}:at386-m,xterm, HP-UX7.0:(not sure)
 */
#undef	IDLOKBAD

/*
 * If moving right off the screen causes the screen to not redraw
 * properly, define RIGHT_CBUG to get around a curses problem on some
 * boxes, this forces screen redraws when going right off the screen
 */
#undef RIGHT_CBUG

/**************************************************/
/* Below here 'configure' should have everything set the way you need it */



/*  Some yuck I found where it turned abs() into some inline routine?? */
#if defined(convex)
#define __NO_INLINE
#endif

/*
 * Should function prototypes be seen?
 *
 * PROTO is used with: func PROTO ((arg1, arg2))
 * The outer set of () pass the inner set of () to the macro.
 * In the prototype version we just pass the (arg1,arg2) on, in the K&R
 * version we just create the empty function arg list.
 */
#ifdef __STDC__
#undef PROTO
#define PROTO(x)	x
#else
#undef PROTO
#define PROTO(x)	()
#endif /* __STDC__ */


/*  The regular expression handlers
 *  RE_COMP for system re_comp/re_exec()   (BSD, SUN)
 *  REGCMP for standard system regcmp/regex handlers (SVR3/4)
 *  REGCOMP for spencer lib use
 *  REG_COMP for POSIX.2 FIPS 
 */

/*  386BSD has patches to use RE_COMP, but some people may not have them */
/*
#if defined(__386BSD__) || defined(__hpux)
#define REGCOMP
#else

#if defined(SYSV)
#define REGCMP
#else
*/
/* defined(mips) || defined(sun) || defined(__convex__) || defined(__osf__) */
/*
#define RE_COMP

#endif
#endif
*/
/* my note: __sgi__ for sgi machines */


/**** The things below are created by configure ****/
@TOP@

/* These are rules we use to insert code segments */

/* Define if you have X11 header files.  */
#undef HAVE_X11_X_H

/* Define if you shouldn't use termcap.  */
#undef NO_TERMCAP

@BOTTOM@


/* Standard include files */

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#ifdef HAVE_STRINGS_H
#include <strings.h>
#elif defined(HAVE_STRING_H)
#include <string.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#elif defined(HAVE_SYS_TIME_H)
#include <sys/time.h>
#endif
