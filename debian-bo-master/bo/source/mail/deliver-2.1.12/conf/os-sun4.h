/* $Id: os-sun4.h,v 1.5 1992/07/29 17:08:35 chip Exp $
 *
 * Deliver configuration for SunOS 4.x.
 *
 * $Log: os-sun4.h,v $
 * Revision 1.5  1992/07/29  17:08:35  chip
 * SunOS 4.x has vprintf().
 *
 * Revision 1.4  1991/08/05  18:19:27  chip
 * Rename to "sun4" since discovery that 4.0 also works.
 *
 * Revision 1.3  1991/06/19  13:07:24  chip
 * Lock with ".lock".  Use putenv() and getopt() from library.
 *
 * Revision 1.2  1991/06/17  13:49:19  chip
 * Submitted by G. Paul Ziemba.
 *
 */

/* Mostly it's BSD. */

#include <os-bsd.h>

/* Then again... */

#define ML_DOTLOCK		/* Create <mailbox>.lock		*/

#define HAS_VPRINTF		/* Has vprintf()			*/
#define HAS_PUTENV		/* Has putenv()				*/
#define HAS_GETOPT		/* Has getopt()				*/
#define HAS_VOIDSIG		/* Signal handlers return void		*/
