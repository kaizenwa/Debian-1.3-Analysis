/* $Id: os-sysv.h,v 1.6 1993/10/28 16:50:34 chip Exp $
 *
 * Deliver configuration for generic System V.
 *
 * $Log: os-sysv.h,v $
 * Revision 1.6  1993/10/28  16:50:34  chip
 * Add HAS_USLEEP (undefined).
 *
 * Revision 1.5  1991/10/23  19:21:07  chip
 * Remove HH_FCNTL.
 *
 * Revision 1.4  1991/08/26  17:48:20  chip
 * New parameters.
 *
 * Revision 1.3  1991/06/17  14:37:46  chip
 * Define minimal SAFEPATH.
 *
 * Revision 1.2  1991/06/04  18:14:53  chip
 * Created.
 *
 */

/* Header files */

#undef  HH_UNISTD		/* Has <unistd.h>			*/
#undef  HH_STDARG		/* Has <stdarg.h>			*/
#define HH_VARARGS		/* Has <varargs.h>			*/
#define HH_STRING		/* Has <string.h>			*/
#undef  HH_STRINGS		/* Has <strings.h>			*/
#undef  HH_SYS_FILE		/* Needs <sys/file.h> for O_XXX		*/

/* Pathnames */

#define SAFEPATH    "/bin:/usr/bin"  /* Safe dirs for PATH		*/

#define SYSV_USRMAIL		/* Mailboxes in /usr/mail, as per SysV	*/

/* How to get the host name -- define one */

#define HAS_UNAME		/* Has uname()		     (SVID)	*/
#undef  HAS_GETHOSTNAME		/* Has gethostname()	     (BSD)	*/
/* #define HOSTFILE "/etc/systemid" */  /* File contataining the hostname */
/* #define HOSTNAME "cleese" */		/* Hard-coded hostname */

/* Kernel locking methods -- define no more than one */

#define LOCK_LOCKF		/* Use lockf(F_LOCK, ...)    (SVID)	*/
#undef  LOCK_FCNTL		/* Use fcntl(F_SETLKW, ...)  (SVID)	*/
#undef  LOCK_FLOCK		/* Use flock(..., LOCK_EX)   (BSD)	*/
#undef  LOCK_LOCKING		/* Use locking(LK_LOCK, ...) (Xenix)	*/

/* Mailbox locking methods -- define as many as desired */

#define ML_DOTLOCK		/* Create <mailbox>.lock		*/
#undef  ML_DOTMLK		/* Create <basename>.mlk     (Xenix)	*/
#define ML_KERNEL		/* Use kernel locking as defined above	*/

/* Times and seasons */

#define HAS_TZNAME		/* Has global variable tzname[]		*/
#define HAS_TIMEZONE		/* Has global variable timezone (SVID)	*/
#undef  HAS_GETTOD		/* Has gettimeofday()           (BSD)	*/
#undef  HAS_FTIME		/* Has ftime()                  (V7)	*/

/* Miscellaneous features */

#define HAS_STRCHR		/* Has strchr() and strrchr()		*/
#define HAS_MEMFUNCS		/* Has memcpy() and memset()		*/
#undef  HAS_BFUNCS		/* Has bcopy() and bzero()		*/
#define HAS_VPRINTF		/* Has vprintf()			*/
#define HAS_PUTENV		/* Has putenv()				*/
#define HAS_GETOPT		/* Has getopt()				*/
#define HAS_SETVBUF		/* Has setvbuf()			*/
#undef  HAS_SETLINEBUF		/* Has setlinebuf()			*/
#undef  HAS_NAP			/* Has nap() (in milliseconds)		*/
#undef  HAS_USLEEP		/* Has usleep() (in microseconds)	*/
#define HAS_VOIDSIG		/* Signal handlers return void		*/
#undef  HAS_BSD_GROUPS		/* Has BSD (not POSIX) group vectors	*/
#undef  HAS_LONGNAMES		/* Long filenames (>14) supported	*/

/* Bug workarounds */

/* If your <signal.h> doesn't declare signal(): */
#undef  DECLARE_SIGNAL		/* Declare signal() ourselves		*/

/* If your <time.h> doesn't declare tzname[]: */
#undef  DECLARE_TZNAME		/* Declare global variable tzname[]	*/

/* If your <errno.h> doesn't declare errno: */
#undef  DECLARE_ERRNO		/* Declare global variable errno	*/

/* If your setvbuf() is broken (some Xenix, SVR0, SVR1): */
#undef  SETVBUF_TYPE_BUF	/* Call setvbuf(..., type, buf, ...)	*/
