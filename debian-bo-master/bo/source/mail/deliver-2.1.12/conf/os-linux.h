/* $Id: os-linux.h,v 1.1 1993/10/28 16:50:11 chip Exp $
 *
 * Deliver configuration for Linux.
 *
 * $Log: os-linux.h,v $
 * Revision 1.1  1993/10/28  16:50:11  chip
 * Initial revision
 *
 */

/* Mostly it's System V. */

#include <os-sysv.h>

/* Then again... */

#define HH_UNISTD		/* Has <unistd.h>			*/
#define HH_STDARG		/* Has <stdarg.h>			*/
#undef  HH_VARARGS		/* Has <varargs.h>			*/

#undef  SYSV_USRMAIL		/* Mailboxes in /usr/mail, as per SysV	*/

#undef  HAS_UNAME		/* Has uname()		     (SVID)	*/
#define HAS_GETHOSTNAME		/* Has gethostname()	     (BSD)	*/

#undef  LOCK_LOCKF		/* Use lockf(F_LOCK, ...)    (SVID)	*/
#define LOCK_FCNTL		/* Use fcntl(F_SETLKW, ...)  (SVID)	*/

#define HAS_USLEEP		/* Has usleep() (in microseconds)	*/
