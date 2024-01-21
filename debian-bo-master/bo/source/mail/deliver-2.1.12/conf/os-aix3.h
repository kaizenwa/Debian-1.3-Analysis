/* $Id: os-aix3.h,v 1.3 1991/06/17 14:01:13 chip Exp $
 *
 * Deliver configuration for AIX 3.x.
 *
 * If you favor the BSD universe, you may want to redefine SAFEPATH
 * in "local.h" to put /usr/ucb before /usr/bin.
 *
 * $Log: os-aix3.h,v $
 * Revision 1.3  1991/06/17  14:01:13  chip
 * Add SAFEPATH.
 *
 * Revision 1.2  1991/06/04  18:14:53  chip
 * Created.
 *
 */

/* Mostly it's System V. */

#include <os-sysv.h>

/* Then again... */

#define HH_UNISTD		/* Has <unistd.h>			*/
#define HH_STDARG		/* Has <stdarg.h>			*/
#undef  HH_VARARGS		/* Has <varargs.h>			*/

#undef  SAFEPATH
#define SAFEPATH    "/bin:/usr/bin:/usr/ucb"  /* Safe dirs for PATH	*/

#undef  SYSV_USRMAIL		/* Mailboxes in /usr/mail, as per SysV	*/

#define HAS_LONGNAMES		/* Long filenames (>14) supported	*/
