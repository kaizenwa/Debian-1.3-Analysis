/* $Id: deliver.h,v 1.9 1993/10/28 16:49:51 chip Exp $
 *
 * General pull-it-together include file.
 *
 * $Log: deliver.h,v $
 * Revision 1.9  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.8  1991/11/12  20:44:42  chip
 * Add logsize().
 *
 * Revision 1.7  1991/10/23  19:47:48  chip
 * Declare errname().
 *
 * Revision 1.6  1991/09/27  16:21:53  chip
 * Declare variadic functions if using stdarg.
 *
 * Revision 1.5  1991/08/27  15:39:31  chip
 * Add tmzone().
 *
 * Revision 1.4  1991/08/21  22:15:33  chip
 * Careful creation for NFS.
 *
 * Revision 1.3  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.2  1991/05/29  17:23:32  chip
 * Rearrange global vars.  Make "sysmb_gid" global; set it in main().
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <signal.h>

#include "config.h"
#include "misc.h"
#include "context.h"
#include "dest.h"

/*----------------------------------------------------------------------
 * Global constants.
 */

#define FROMSIZE    5		/* "From "                              */


/*----------------------------------------------------------------------
 * Global data
 */

#ifdef DELHOME
extern char delhome[];		/* Deliver's home directory		*/
#endif

extern int verbose;		/* Print debugging messages?            */
extern int dryrun;		/* Are we making a dry run?             */
extern int rundfiles;		/* Run delivery files at all?           */
extern int printaddrs;		/* Address resolution only?             */
extern int leavetemps;		/* Leave temp files for later perusal   */
extern int boxdelivery;		/* Args are mailboxes, not addresses    */

extern char *progname;		/* Name this program was invoked under  */
extern char version[];		/* Version and patchlevel               */
extern char *shell;		/* Shell used to run delivery files     */

extern int rec_level;		/* If recursing, recursion level        */
extern int rec_parent;		/* If recursing, parent deliver's pid   */
extern char *sys_deliver;	/* Systemwide delivery file             */
extern char *post_deliver;	/* Post-user delivery file              */
extern char *err_deliver;	/* Error delivery file                  */
extern char *user_deliver;	/* User delivery file                   */
extern char *local_sender;	/* Local identity of sender             */
extern char *orig_sender;	/* Claimed identity of original sender  */
extern char *hostname;		/* Name of this host                    */

extern int eff_uid;		/* Returned by geteuid()                */
extern int eff_gid;		/* Returned by getegid()                */
extern int real_uid;		/* Returned by getuid()                 */
extern int real_gid;		/* Returned by getgid()                 */
extern int maxgroups;		/* Returned by sysconf() by POSIX       */

#ifdef MBX_GROUP
extern int sysmb_gid;		/* Group that owns system mailboxes	*/
#endif

extern int trusted_user;	/* Is user who called us trusted?       */
extern int mailer_user;		/* Is user who called us a mailer?      */

extern CONTEXT *eff_ct;		/* Context of effective uid             */
extern CONTEXT *real_ct;	/* Context of real uid                  */

extern char *eff_group;		/* Name of effective gid		*/
extern char *real_group;	/* Name of real gid			*/

extern FILE *log;		/* File to log deliveries               */
extern char *t_logfile;		/* Temporary file for log               */
extern char *f_logfile;		/* Final file for log			*/

extern FILE *errlog;		/* File to log messages and errors      */
extern char *t_errlogfile;	/* Temporary file for error log         */
extern char *f_errlogfile;	/* Final file for error log		*/

extern int tty_input;		/* Is our input coming from a tty?      */
extern SIGFLAG got_sig;		/* Did we catch a deadly signal?        */

/* Temp file indices: */
#define T_HDR      0		/* Message header                       */
#define T_BODY     1		/* Message body                         */
#define T_HDRCOPY  2		/* Copy of message header               */
#define T_BODYCOPY 3		/* Copy of message body                 */
#define T_MAX      4		/* Number of temp files                 */

extern char *ttype[T_MAX];	/* Temp file types (for messages)       */
extern char *tfile[T_MAX];	/* Temp file names                      */
extern char *tenv[T_MAX];	/* Temp file environment names          */
extern int tfd[T_MAX];		/* Temp file fd's                       */


/*----------------------------------------------------------------------
 * Global functions
 */

void dumpdests();
void openlogs();
void tosslogs();
void savelogs();
void applog();
void logreport();
void logstate();
void logerrinfo();
void logstart();
void logdone();
void errstart();
void errdone();
void alloc_env();
void del_env();
void snooze();

char *group_name();
char *tempfile();
char *basename();
char *unique();
char *relpath();
char *errname();
char *gethost();
char *skipfrom();
char *tmzone();
char *copystr();
char *derrmsg();
char *zalloc();
char *srealloc();

CONTEXT *name_context();
CONTEXT *uid_context();

FILE *ftcreate();
FILE *ct_fopenv();

DEST *addr_dest();
DEST *dest();
DEST *dest_undel();
DEST *first_dest();
DEST *next_dest();
DEST **dest_array();

DCLASS addr_class();

long logsize();

time_t unctime();

#ifdef HH_STDARG
# define PFMT P((char *fmt, ...))
#else
# define PFMT ()
#endif
void message PFMT ATTRIBUTE((format(printf,1,2)));
void error PFMT   ATTRIBUTE((format(printf,1,2)));
void syserr PFMT  ATTRIBUTE((format(printf,1,2)));
void verror();
void vsyserr();

NORETURN void nomem P((void)) ATTRIBUTE((noreturn));
NORETURN void leave P((int)) ATTRIBUTE((noreturn));

/*----------------------------------------------------------------------
 * Tricks to shut lint up.
 */

#ifdef lint

extern size_t alloc_dummy_size;

#define talloc(t,n)     (alloc_dummy_size = (n), (t *)0)

#else

#define talloc(t,n)	(t *)zalloc((n) * sizeof(t))

#endif
