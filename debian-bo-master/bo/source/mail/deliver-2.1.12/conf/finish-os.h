/* $Id: finish-os.h,v 1.11 1993/10/28 17:09:03 chip Exp $
 *
 * Spell out the implications of a given OS configuration.
 *
 * $Log: finish-os.h,v $
 * Revision 1.11  1993/10/28  17:09:03  chip
 * Depend on _POSIX_VERSION instead of _SC_NGROUPS_MAX.
 *
 * Revision 1.10  1991/11/21  15:13:29  chip
 * Simplify test for multiple locks, for HP-UX preprocessor.
 * Define LOCK_ANY before use.  (!)
 *
 * Revision 1.9  1991/10/23  19:20:27  chip
 * Always include <fcntl.h>.  Define FD_CLOEXEC if it's missing.
 *
 * Revision 1.8  1991/08/27  17:40:47  chip
 * Use DECLARE_* elsewhere.
 *
 * Revision 1.7  1991/08/27  15:40:45  chip
 * Add <stdlib.h>, malloc(), realloc(), free().
 * Define MBX_* according to SYSV_USRMAIL.
 *
 * Revision 1.6  1991/08/26  17:46:41  chip
 * Add tzname, errno, strerror.  Validate time configuration.
 *
 * Revision 1.5  1991/08/21  22:20:48  chip
 * Declare pathconf() and fpathconf().
 *
 * Revision 1.4  1991/08/21  22:15:32  chip
 * Careful creation for NFS.
 *
 * Revision 1.3  1991/08/05  19:02:41  chip
 * Terminate a comment.  (!)
 *
 * Revision 1.2  1991/06/04  18:14:53  chip
 * Created.
 *
 */

/*------------------------------------------------------------------------
 * Validate kernel locking configuration.
 */

#undef LOCK_ANY

/* lockf() */
#ifdef LOCK_LOCKF
# ifdef LOCK_ANY
#  define LOCK_MULT
# else
#  define LOCK_ANY
# endif
#endif

/* fcntl() */
#ifdef LOCK_FCNTL
# ifdef LOCK_ANY
#  define LOCK_MULT
# else
#  define LOCK_ANY
# endif
#endif

/* flock() */
#ifdef LOCK_FLOCK
# ifdef LOCK_ANY
#  define LOCK_MULT
# else
#  define LOCK_ANY
# endif
#endif

/* locking() */
#ifdef LOCK_LOCKING
# ifdef LOCK_ANY
#  define LOCK_MULT
# else
#  define LOCK_ANY
# endif
#endif

#ifdef LOCK_MULT
ACK! "Define only one of LOCK_LOCKF, LOCK_FCNTL, LOCK_FLOCK and LOCK_LOCKING.";
#endif

/*------------------------------------------------------------------------
 * Validate time configuration.
 */

#if !defined(HAS_TIMEZONE) && !defined(HAS_GETTOD) && !defined(HAS_FTIME)
ACK! "Define either HAS_TIMEZONE, HAS_GETTOD or HAS_FTIME.";
#endif

/*----------------------------------------------------------------------
 * POSIX systems have <unistd.h> which defines lots of interesting
 * things.
 */

#ifdef HH_UNISTD
#include <unistd.h>
extern long sysconf();
extern long pathconf(), fpathconf();
#endif

/*----------------------------------------------------------------------
 * Declare memory allocation functions.
 */

#ifdef ANSI_C
#include <stdlib.h>
#else
extern char *malloc();
extern char *realloc();
extern void free();
#endif

/*----------------------------------------------------------------------
 * Declare string functions.
 */

#ifdef HH_STRING
#include <string.h>
#endif

#ifdef HH_STRINGS
#include <strings.h>
#endif

#if !defined(HH_STRING) && !defined(HH_STRINGS)
extern char *strchr();
extern char *strrchr();
#endif

/*----------------------------------------------------------------------
 * Include miscellaneous file-access headers.
 */

#include <fcntl.h>

#ifdef HH_SYS_FILE
#include <sys/file.h>
#endif

/*----------------------------------------------------------------------
 * Declare FD_xxx file descriptor flags.
 */

#ifndef FD_CLOEXEC
#define FD_CLOEXEC 1
#endif

/*----------------------------------------------------------------------
 * Declare O_xxx macros for open().
 */

#ifndef O_RDONLY
#define O_RDONLY   0
#define O_WRONLY   1
#define O_RDWR     2
#endif

/*----------------------------------------------------------------------
 * If the O_CREAT flag is available and NFS is not,
 *     then we can create files safely without linking.
 * If safe creation and kernel locking are available,
 *     then we can update files safely without lock files.
 */

#if defined(O_CREAT) && !defined(HAS_NFS)
#define SAFE_CREATE
#endif

#if defined(LOCK_ANY) && defined(SAFE_CREATE)
#define SAFE_UPDATE
#endif

/*----------------------------------------------------------------------
 * Signal function type.
 * Signal handlers have this return value.
 */

#ifdef HAS_VOIDSIG
#define SIGTYPE void
#else
#define SIGTYPE int
#endif

/*----------------------------------------------------------------------
 * Signal flag type.
 * Variables of this type may be set by signal catching routines.
 */

#ifdef ANSI_C
#define SIGFLAG sig_atomic_t
#else
#define SIGFLAG short	/* or "volatile short" for aggressive optimizers */
#endif

/*----------------------------------------------------------------------
 * ANSI C declares errno in <errno.h>.
 */

#ifdef ANSI_C
#undef DECLARE_ERRNO
#endif

/*----------------------------------------------------------------------
 * ANSI C defines strerror() to return system error messages.
 */

#ifdef ANSI_C
#define HAS_STRERROR
#endif

/*----------------------------------------------------------------------
 * Determine maximum filename (not pathname) length.
 */

#ifdef HAS_LONGNAMES
#define MAX_NAMESIZE	255
#else
#define MAX_NAMESIZE	14
#endif

/*----------------------------------------------------------------------
 * Group vector definitions.
 *
 *     GROUP_VECTOR         Defined if kernel supports group vectors.
 *     GRVEC_T              Type of group vector array's elements.
 *     GETGROUPS(n,v)       Get group vector into v[0..n]
 *     SETGROUPS(n,v)       Set group vector from v[0..n].
 *     MAXGROUPS()          Return group vector max length.
 *
 * >>> Defining GRVEC_T incorrectly creates a security hole. <<<
 */

#ifdef HAS_BSD_GROUPS

#define GROUP_VECTOR
#define GRVEC_T		int
#define GETGROUPS(n,g)	getgroups(n,g)
#define SETGROUPS(n,g)	setgroups(n,g)
#define MAXGROUPS()	32	/* BE SURE THIS IS LARGE ENOUGH */

#else /* not BSD groups */
#ifdef _POSIX_VERSION

#define GROUP_VECTOR
#define GRVEC_T		gid_t
#define GETGROUPS(n,g)	getgroups(n,g)
#define SETGROUPS(n,g)	setgroups(n,g)
#define MAXGROUPS()	sysconf(_SC_NGROUPS_MAX)

#endif	/* POSIX groups */
#endif	/* not BSD groups */

/*----------------------------------------------------------------------
 * Define string search functions.
 */

#ifndef HAS_STRCHR
#define strchr          index
#define strrchr         rindex
#endif

/*-----------------------------------------------------------------------
 * Declare memory copy and zero functions.
 */

#ifdef HAS_MEMFUNCS
#define Copy(d,s,n)     (void) memcpy(d,s,n)
#define Zero(d,n)       (void) memset(d,0,(unsigned)(n))
#else
#ifdef HAS_BFUNCS
#define Copy(d,s,n)     bcopy(s,d,n)
#define Zero(d,n)       bzero(d,n)
#else
#define NEED_COPYZERO	/* define Copy() and Zero() in sysdep.c */
#endif
#endif

/*-----------------------------------------------------------------------
 * Handle line buffering on stdio files.
 */

#ifdef HAS_SETVBUF

#ifdef SETVBUF_TYPE_BUF
#define Linebuf(f)      (void) setvbuf((f), _IOLBF, (char *)NULL, BUFSIZ)
#else
#define Linebuf(f)      (void) setvbuf((f), (char *)NULL, _IOLBF, BUFSIZ)
#endif

#else /* not HAS_SETVBUF */
#ifdef HAS_SETLINEBUF

#define Linebuf(f)      (void) setlinebuf(f)

#else
/* If line buffering isn't possible, don't buffer at all. */

#define Linebuf(f)      (void) setbuf((f), (char *)NULL)

#endif /* HAS_SETLINEBUF */
#endif /* not HAS_SETVBUF */

/*-----------------------------------------------------------------------
 * Standard mailbox location.
 *
 * Define either MBX_NAME or MBOX_DIR.
 * If MBX_NAME is defined, then the default mailbox is a file with
 * that name in the user's home directory.
 * If MBX_DIR is defined, then the default mailbox is a file in that
 * directory with the same name as the user.
 *
 * Define MBX_GROUP if all mailboxes must be owned by a specific group.
 * (System V requires this feature.)  If MBX_GROUP is not defined,
 * mailboxes will have their groups set to the recipients' default group.
 *
 * Define MBX_MODE to the file access modes for new mailboxes.
 * (System V requires group write permissions, i.e. 0020.)
 */

#ifdef SYSV_USRMAIL
/* #define MBX_NAME   "mbox" */
#define MBX_DIR     "/usr/mail"
#define MBX_MODE    0660
#define MBX_GROUP   "mail"
#else
/* #define MBX_NAME   "mbox" */
#define MBX_DIR     "/usr/spool/mail"
#define MBX_MODE    0600
#endif
