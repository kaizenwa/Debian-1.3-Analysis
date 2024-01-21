/* $Id: os-hpux8.h,v 1.1 1991/11/21 15:14:04 chip Exp $
 *
 * Deliver configuration for HP-UX 8.x.
 *
 * $Log: os-hpux8.h,v $
 * Revision 1.1  1991/11/21  15:14:04  chip
 * Initial revision
 *
 */

/* Mostly it's System V (externally, anyway). */

#include <os-sysv.h>

/* Then again... */

#define HH_UNISTD		/* Has <unistd.h>			*/
