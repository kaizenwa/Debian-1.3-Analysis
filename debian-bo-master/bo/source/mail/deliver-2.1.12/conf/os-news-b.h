/* $Id: os-news-b.h,v 1.1 1991/08/26 17:48:49 chip Exp $
 *
 * Deliver configuration for Sony NeWS-OS 3.2 (BSD-based).
 *
 * $Log: os-news-b.h,v $
 * Revision 1.1  1991/08/26  17:48:49  chip
 * Initial revision
 *
 */

/* Mostly it's BSD. */

#include <os-bsd.h>

/* Then again... */

#define HH_STRING		/* Has <string.h>			*/
#undef  HH_STRINGS		/* Has <strings.h>			*/

#define HAS_TZNAME		/* Has global variable tzname[]		*/
#define HAS_FTIME		/* Has ftime()                  (V7)	*/

#define HAS_STRCHR		/* Has strchr() and strrchr()		*/
#define HAS_GETOPT		/* Has getopt()				*/

/* Bug workarounds */

#define DECLARE_TZNAME		/* Declare global variable tzname[]	*/
