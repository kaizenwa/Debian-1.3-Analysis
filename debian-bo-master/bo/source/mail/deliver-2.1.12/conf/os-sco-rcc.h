/* $Id: os-sco-rcc.h,v 1.3 1991/08/05 18:19:50 chip Exp $
 *
 * Deliver configuration for SCO UNIX and Open Desktop (ODT) using
 * "rcc" (the AT&T C compiler).
 *
 * NOTE: If you have version UNIX 3.2 or 3.2v2, or ODT 1.0 or 1.1, and
 * have not installed the Security Supplement "unx257", I strongly
 * recommend that you do so.  It will make C2 less obtrusive and add
 * support for group vectors.  If you don't install "unx257", then you
 * must edit "local.h" to include the statement "#undef GROUP_VECTOR".
 *
 * $Log: os-sco-rcc.h,v $
 * Revision 1.3  1991/08/05  18:19:50  chip
 * Describe interaction of unx257 and group vectors.
 *
 * Revision 1.2  1991/06/04  18:14:53  chip
 * Created.
 *
 */

/* Mostly it's System V. */

#include <os-sysv.h>

/* Then again... */

#define HH_UNISTD		/* Has <unistd.h>			*/

#undef  SYSV_USRMAIL		/* Mailboxes in /usr/mail, as per SysV	*/
