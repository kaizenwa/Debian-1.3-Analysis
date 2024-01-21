/* $Id: os-bsd.h,v 1.8 1993/10/28 16:50:34 chip Exp $
 *
 * Deliver configuration for generic BSD.
 *
 * $Log: os-bsd.h,v $
 * Revision 1.8  1993/10/28  16:50:34  chip
 * Add HAS_USLEEP (undefined).
 *
 * Revision 1.7  1991/10/23  19:21:07  chip
 * Remove HH_FCNTL.
 *
 * Revision 1.6  1991/08/27  17:40:30  chip
 * BSD needs us to declare errno.
 *
 * Revision 1.5  1991/08/26  17:47:33  chip
 * New parameters.
 * BSD doesn't have vprintf().
 *
 * Revision 1.4  1991/06/19  13:06:42  chip
 * Put /usr/ucb first in SAFEPATH, for Sun (/bin is really /usr/bin).
 * BSD has neither strchr() nor getopt().
 *
 * Revision 1.3  1991/06/17  13:49:36  chip
 * Use <strings.h>.
 *
 * Revision 1.2  1991/06/17  13:24:23  chip
 * Submitted by Neil Rickert.
 *
 */

/* Header files */

#undef  HH_UNISTD		/* Has <unistd.h>			*/
#undef  HH_STDARG		/* Has <stdarg.h>			*/
#define HH_VARARGS		/* Has <varargs.h>			*/
#undef  HH_STRING		/* Has <string.h>			*/
#define HH_STRINGS		/* Has <strings.h>			*/
#define HH_SYS_FILE		/* Needs <sys/file.h> for O_XXX		*/

/* Pathnames */

#define SAFEPATH    "/usr/ucb:/bin:/usr/bin"  /* Safe dirs for PATH	*/

#undef  SYSV_USRMAIL		/* Mailboxes in /usr/mail, as per SysV	*/

/* How to get the host name -- define one */

#undef  HAS_UNAME		/* Has uname()		     (SVID)	*/
#define HAS_GETHOSTNAME		/* Has gethostname()	     (BSD)	*/
/* #define HOSTFILE "/etc/systemid" */  /* File contataining the hostname */
/* #define HOSTNAME "cleese" */		/* Hard-coded hostname */

/* Kernel locking methods -- define no more than one */

#undef  LOCK_LOCKF		/* Use lockf(F_LOCK, ...)    (SVID)	*/
#undef  LOCK_FCNTL		/* Use fcntl(F_SETLKW, ...)  (SVID)	*/
#define LOCK_FLOCK		/* Use flock(..., LOCK_EX)   (BSD)	*/
#undef  LOCK_LOCKING		/* Use locking(LK_LOCK, ...) (Xenix)	*/

/* Mailbox locking methods -- define as many as desired */

#undef  ML_DOTLOCK		/* Create <mailbox>.lock		*/
#undef  ML_DOTMLK		/* Create <basename>.mlk     (Xenix)	*/
#define ML_KERNEL		/* Use kernel locking as defined above	*/

/* Times and seasons */

#undef  HAS_TZNAME		/* Has global variable tzname[]		*/
#undef  HAS_TIMEZONE		/* Has global variable timezone (SVID)	*/
#define HAS_GETTOD		/* Has gettimeofday()           (BSD)	*/
#undef  HAS_FTIME		/* Has ftime()                  (V7)	*/

/* Miscellaneous features */

#undef  HAS_STRCHR		/* Has strchr() and strrchr()		*/
#undef  HAS_MEMFUNCS		/* Has memcpy() and memset()		*/
#define HAS_BFUNCS		/* Has bcopy() and bzero()		*/
#undef  HAS_VPRINTF		/* Has vprintf()			*/
#undef  HAS_PUTENV		/* Has putenv()				*/
#undef  HAS_GETOPT		/* Has getopt()				*/
#undef  HAS_SETVBUF		/* Has setvbuf()			*/
#define HAS_SETLINEBUF		/* Has setlinebuf()			*/
#undef  HAS_NAP			/* Has nap() (in milliseconds)		*/
#undef  HAS_USLEEP		/* Has usleep() (in microseconds)	*/
#undef  HAS_VOIDSIG		/* Signal handlers return void		*/
#define HAS_BSD_GROUPS		/* Has BSD (not POSIX) group vectors	*/
#define HAS_LONGNAMES		/* Long filenames (>14) supported	*/

/* Bug workarounds */

/* If your <signal.h> doesn't declare signal(): */
#undef  DECLARE_SIGNAL		/* Declare signal() ourselves		*/

/* If your <time.h> doesn't declare tzname[]: */
#undef  DECLARE_TZNAME		/* Declare global variable tzname[]	*/

/* If your <errno.h> doesn't declare errno: */
#define DECLARE_ERRNO		/* Declare global variable errno	*/

/* If your setvbuf() is broken (some Xenix, SVR0, SVR1): */
#undef  SETVBUF_TYPE_BUF	/* Call setvbuf(..., type, buf, ...)	*/
