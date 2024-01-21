/* $Id: os-sysvr4.h,v 1.1 1991/08/27 15:41:45 chip Exp $
 *
 * Deliver configuration for System V Release 4.
 *
 * $Log: os-sysvr4.h,v $
 * Revision 1.1  1991/08/27  15:41:45  chip
 * Initial revision
 *
 */

/* Mostly it's System V. */

#include <os-sysv.h>

/* Then again... */

#define HH_UNISTD		/* Has <unistd.h>			*/
#define HH_STDARG		/* Has <stdarg.h>			*/
#undef  HH_VARARGS		/* Has <varargs.h>			*/

#undef  SAFEPATH
#define SAFEPATH    "/usr/bin"	/* Safe dirs for PATH			*/

#define HAS_NAP			/* Has nap() (in milliseconds)		*/
