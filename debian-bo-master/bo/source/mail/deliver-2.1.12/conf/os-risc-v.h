/* $Id: os-risc-v.h,v 1.1 1992/07/15 13:59:22 chip Exp $
 *
 * Deliver configuration for MIPS risc/os 4.x (sysv universe)
 *
 * $Log: os-risc-v.h,v $
 * Revision 1.1  1992/07/15  13:59:22  chip
 * Initial revision
 *
 */

/* Mostly it's System V. */

#include <os-sysv.h>

/* Then again... */

#define HH_UNISTD		/* Has <unistd.h>			*/
#define HH_STDARG		/* Has <stdarg.h>			*/

#undef  ML_KERNEL		/* Use kernel locking as defined above	*/

#define HAS_LONGNAMES		/* Long filenames (>14) supported	*/
