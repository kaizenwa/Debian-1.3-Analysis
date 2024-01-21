/* $Id: os-risc-b.h,v 1.1 1992/07/15 13:59:22 chip Exp $
 *
 * Deliver configuration for MIPS risc/os 4.x (bsd universe)
 *
 * $Log: os-risc-b.h,v $
 * Revision 1.1  1992/07/15  13:59:22  chip
 * Initial revision
 *
 */

/* Mostly it's BSD. */

#include <os-bsd.h>

/* Then again... */

#define HH_STDARG		/* Has <stdarg.h>			*/
#define HH_STRING		/* Has <string.h>			*/

#undef  SAFEPATH
#define SAFEPATH "/bsd43/bin:/usr/ucb:/bin:/usr/bin"  /* Safe dirs for PATH */

#define ML_DOTLOCK		/* Create <mailbox>.lock		*/
#undef  ML_KERNEL		/* Use kernel locking as defined above	*/

#define HAS_STRCHR		/* Has strchr() and strrchr()		*/
#define HAS_MEMFUNCS		/* Has memcpy() and memset()		*/
#define HAS_VPRINTF		/* Has vprintf()			*/
#define HAS_GETOPT		/* Has getopt()				*/
