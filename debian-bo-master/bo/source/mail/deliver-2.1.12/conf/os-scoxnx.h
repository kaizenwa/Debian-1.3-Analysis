/*
 * $Id: os-scoxnx.h,v 1.1 1991/11/21 15:40:45 chip Exp $
 *
 * Deliver configuration for SCO Xenix System V.
 *
 * NOTE: If you have a very old version of Xenix, you may have to edit
 * "local.h" to include the statement "#define SETVBUF_TYPE_BUF".  If
 * this means you, your first clue will probably be a core dump.
 *
 * $Log: os-scoxnx.h,v $
 * Revision 1.1  1991/11/21  15:40:45  chip
 * Initial revision
 *
 */

/* Mostly it's System V. */

#include <os-sysv.h>

/* Then again... */

#ifndef lint
#define HH_STDARG		/* Has <stdarg.h>			*/
#undef  HH_VARARGS		/* Has <varargs.h>			*/
#endif

#undef  SYSV_USRMAIL		/* Mailboxes in /usr/mail, as per SysV	*/

#undef  LOCK_LOCKF		/* Use lockf(F_LOCK, ...)    (SVID)	*/
#define LOCK_LOCKING		/* Use locking(LK_LOCK, ...) (Xenix)	*/

#undef  ML_DOTLOCK		/* Create <mailbox>.lock		*/
#define ML_DOTMLK		/* Create <basename>.mlk     (Xenix)	*/
