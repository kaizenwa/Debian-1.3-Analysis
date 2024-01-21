/* $Id: os-cxux.h,v 1.1 1991/08/26 17:48:49 chip Exp $
 *
 * Deliver configuration for Harris CX/UX.
 *
 * $Log: os-cxux.h,v $
 * Revision 1.1  1991/08/26  17:48:49  chip
 * Initial revision
 *
 */

/* Mostly it's System V. */

#include <os-sysv.h>

/* Then again... */

#define HH_UNISTD		/* Has <unistd.h>			*/
#define HH_STDARG		/* Has <stdarg.h>			*/
#undef  HH_VARARGS		/* Has <varargs.h>			*/

#undef  HAS_UNAME		/* Has uname()		     (SVID)	*/
#define HAS_GETHOSTNAME		/* Has gethostname()	     (BSD)	*/

#undef  LOCK_LOCKF		/* Use lockf(F_LOCK, ...)    (SVID)	*/
#define LOCK_FLOCK		/* Use flock(..., LOCK_EX)   (BSD)	*/

#undef  ML_DOTLOCK		/* Create <mailbox>.lock		*/

#undef  HAS_GETTOD		/* Has gettimeofday()           (BSD)	*/
