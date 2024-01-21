/* $Id: os-v7.h,v 1.5 1993/10/28 16:51:08 chip Exp $
 *
 * Deliver configuration for UNIX V7.
 *
 * $Log: os-v7.h,v $
 * Revision 1.5  1993/10/28  16:51:08  chip
 * Add HAS_USLEEP (undefined).
 *
 * Revision 1.4  1991/10/23  19:21:07  chip
 * Remove HH_FCNTL.
 *
 * Revision 1.3  1991/08/26  17:48:10  chip
 * New parameters.
 *
 * Revision 1.2  1991/06/17  14:37:36  chip
 * Created.
 *
 */

/* Header files */

#undef  HH_UNISTD		/* Has <unistd.h>			*/
#undef  HH_STDARG		/* Has <stdarg.h>			*/
#undef  HH_VARARGS		/* Has <varargs.h>			*/
#undef  HH_STRING		/* Has <string.h>			*/
#undef  HH_STRINGS		/* Has <strings.h>			*/
#undef  HH_SYS_FILE		/* Needs <sys/file.h> for O_XXX		*/

/* Pathnames */

#define SAFEPATH    "/bin:/usr/bin"  /* Safe dirs for PATH		*/

#undef  SYSV_USRMAIL		/* Mailboxes in /usr/mail, as per SysV	*/

/* How to get the host name -- define one */
/* Suggestion: define HOSTNAME in "local.h" */

#undef  HAS_UNAME		/* Has uname()		     (SVID)	*/
#undef  HAS_GETHOSTNAME		/* Has gethostname()	     (BSD)	*/
/* #define HOSTFILE "/etc/systemid" */  /* File contataining the hostname */
/* #define HOSTNAME "cleese" */		/* Hard-coded hostname */

/* Kernel locking methods -- define no more than one */

#undef  LOCK_LOCKF		/* Use lockf(F_LOCK, ...)    (SVID)	*/
#undef  LOCK_FCNTL		/* Use fcntl(F_SETLKW, ...)  (SVID)	*/
#undef  LOCK_FLOCK		/* Use flock(..., LOCK_EX)   (BSD)	*/
#undef  LOCK_LOCKING		/* Use locking(LK_LOCK, ...) (Xenix)	*/

/* Mailbox locking methods -- define as many as desired */

#define ML_DOTLOCK		/* Create <mailbox>.lock		*/
#undef  ML_DOTMLK		/* Create <basename>.mlk     (Xenix)	*/
#undef  ML_KERNEL		/* Use kernel locking as defined above	*/

/* Times and seasons */

#undef  HAS_TZNAME		/* Has global variable tzname[]		*/
#undef  HAS_TIMEZONE		/* Has global variable timezone (SVID)	*/
#undef  HAS_GETTOD		/* Has gettimeofday()           (BSD)	*/
#define HAS_FTIME		/* Has ftime()                  (V7)	*/

/* Miscellaneous features */

#undef  HAS_STRCHR		/* Has strchr() and strrchr()		*/
#undef  HAS_MEMFUNCS		/* Has memcpy() and memset()		*/
#undef  HAS_BFUNCS		/* Has bcopy() and bzero()		*/
#undef  HAS_VPRINTF		/* Has vprintf()			*/
#undef  HAS_PUTENV		/* Has putenv()				*/
#undef  HAS_GETOPT		/* Has getopt()				*/
#undef  HAS_SETVBUF		/* Has setvbuf()			*/
#undef  HAS_SETLINEBUF		/* Has setlinebuf()			*/
#undef  HAS_NAP			/* Has nap() (in milliseconds)		*/
#undef  HAS_USLEEP		/* Has usleep() (in microseconds)	*/
#undef  HAS_VOIDSIG		/* Signal handlers return void		*/
#undef  HAS_BSD_GROUPS		/* Has BSD (not POSIX) group vectors	*/
#undef  HAS_LONGNAMES		/* Long filenames (>14) supported	*/

/* Bug workarounds */

/* If your <signal.h> doesn't declare signal(): */
#undef  DECLARE_SIGNAL		/* Declare signal() ourselves		*/

/* If your <time.h> doesn't declare tzname[]: */
#undef  DECLARE_TZNAME		/* Declare global variable tzname[]	*/

/* If your <errno.h> doesn't declare errno: */
#define DECLARE_ERRNO		/* Declare global variable errno	*/

/* If your setvbuf() is broken (some Xenix, SVR0, SVR1): */
#undef  SETVBUF_TYPE_BUF	/* Call setvbuf(..., type, buf, ...)	*/
