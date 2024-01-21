/* $Id: os-3b1.h,v 1.1 1991/08/27 15:41:45 chip Exp $
 *
 * Deliver configuration for AT&T 3B1.
 *
 * $Log: os-3b1.h,v $
 * Revision 1.1  1991/08/27  15:41:45  chip
 * Initial revision
 *
 */

/* Mostly it's System V. */

#include <os-sysv.h>

/* Then again... */

#define HH_UNISTD		/* Has <unistd.h>			*/

#undef  HAS_VOIDSIG		/* Signal handlers return void		*/
