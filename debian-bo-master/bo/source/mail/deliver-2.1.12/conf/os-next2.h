/* $Id: os-next2.h,v 1.3 1992/07/15 14:01:30 chip Exp $
 *
 * Deliver configuration for NextOS 2.x.
 *
 * $Log: os-next2.h,v $
 * Revision 1.3  1992/07/15  14:01:30  chip
 * Next OS has vprintf().
 *
 * Revision 1.2  1991/08/05  18:18:05  chip
 * As per experience of c.s.r reviewer.
 *
 */

/* Mostly it's BSD. */

#include <os-bsd.h>

/* Then again... */

#define ML_DOTLOCK		/* Create <mailbox>.lock		*/

#define HAS_VPRINTF		/* Has vprintf()			*/
#define HAS_GETOPT		/* Has getopt()				*/
#define HAS_VOIDSIG		/* Signal handlers return void		*/
