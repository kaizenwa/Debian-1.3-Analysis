/* $Id: config.h,v 1.9 1992/12/17 20:09:32 chip Exp $
 *
 * Deliver configuration.
 *
 * $Log: config.h,v $
 * Revision 1.9  1992/12/17  20:09:32  chip
 * Define HAS_NFS before including finish-os.h.
 *
 * Revision 1.8  1992/01/20  20:36:05  chip
 * Allow for UUX_OPTS to be a list.
 * Support UUX_DASH_A, so UUCP failure messages are mailed to original sender.
 *
 * Revision 1.7  1991/11/26  16:50:42  chip
 * Add TEMPDIR.
 *
 * Revision 1.6  1991/10/30  21:50:44  chip
 * Add support for MMDF.
 *
 * Revision 1.5  1991/08/27  15:38:41  chip
 * Add SYSV_FROM.  Move MBX_* definitions to conf/finish-os.h
 *
 * Revision 1.4  1991/08/21  22:15:33  chip
 * Careful creation for NFS.
 *
 * Revision 1.3  1991/06/17  13:57:06  chip
 * Always pick up OS config header name from macro.
 * Only provide SAFEPATH if OS config didn't.
 *
 * Revision 1.2  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.1  91/05/13  18:36:55  chip
 * Initial revision
 * 
 */

/*----------------------------------------------------------------------
 * ANSI C is useful.
 * If your compiler is ANSI but pretends not to be, you can
 * add "-DANSI_C" to DEFS in the Makefile.
 */

#if __STDC__ && !defined(ANSI_C)
#define ANSI_C 1
#endif

/*----------------------------------------------------------------------
 * NFS support.
 * If you write mailboxes across NFS, safe creation of mailboxes and
 * lock files cannot in general be guaranteed.  Defining LOCK_LOCKF
 * should work for mailboxes -- unless your server's locking daemon
 * is broken.  (Depressingly many are.)
 *
 * Define HAS_NFS to trigger worst-case assumptions for file creation
 * behavior.  (And believe me: NFS *is* the worst case.)
 *
 * NOTE: You must define HAS_NFS before including <finish-os.h>.
 *       Therefore, you CANNOT define HAS_NFS in <local.h>.
 *       Define it here, or edit Makefile thus: "UDEFS = -DHAS_NFS".
 */

/* #define HAS_NFS */

/*----------------------------------------------------------------------
 * Get operating system configuration.
 * Edit this include statement to refer to the correct file,
 * or edit Makefile thus: "OSHEADER = os-whatever.h".
 */

#include OSINCLUDE

#include <finish-os.h>

/*----------------------------------------------------------------------
 * System V mailbox format support.
 * If you use a genuine System V mail user agent (poor you),
 * define SYSV_FROM to get the System V format "From " line.
 */

/* #define SYSV_FROM */

/*----------------------------------------------------------------------
 * MMDF mailbox format support.
 * Define MBX_MMDF to write mailboxes in MMDF format, in which
 * each message is preceeded and followed by "\1\1\1\1\n".
 */

/* #define MBX_MMDF */

/*----------------------------------------------------------------------
 * Trusted users.
 * Deliver permits "trusted" users to specify delivery filenames
 * without renouncing setuid privileges.  Essentially, these users
 * are given the root password.  Beware!
 */

#define TRUSTED_USERS   "root"

/*----------------------------------------------------------------------
 * Mailer users.
 * Deliver permits "mailer" users to send mail that claims to be
 * from someone else.
 */

#define MAILER_USERS    "root", "daemon", "uucp"

/*----------------------------------------------------------------------
 * UUCP configuration.
 *
 * UUCP_NAMESIZE        Maximum size of a UUCP system name.
 * UUX_ARGCOUNT         Maximum count of arguments for uux.
 * UUX_ARGSIZE          Maximum total size of arguments for uux.
 * UUX_OPTS             A comma-separated list of options for uux.
 *                        Option "-r" often means: queue, but don't call.
 * UUX_DASH_A           Define if your uux understands "-asender".
 */

#define UUCP_NAMESIZE 16
#define UUX_ARGCOUNT  16
#define UUX_ARGSIZE   512
#define UUX_OPTS      "-r"
#define UUX_DASH_A

/*----------------------------------------------------------------------
 * Default shell for executing delivery files and pipes.
 * (Now that Deliver recognizes the "#!" hack, this value is less
 * important than it used to be.)
 * Note that the default shell must support the "-c" option.
 */

#define SHELL   "/bin/sh"

/*----------------------------------------------------------------------
 * Characters that may not appear in addresses.
 * (This string should include all metacharacters for your chosen shell.)
 */

#define SANITIZE   "$*?=\\`'\"|^&;{}()<> \t\n"

/*----------------------------------------------------------------------
 * Safe directories for child processes' PATH variables.
 * Note that including "." is a security hole.
 * For the superuser, "/etc:" is automatically prepended.
 * If DELHOME is defined, DELHOME/bin is automatically prepended.
 */

#ifndef SAFEPATH
#define SAFEPATH  "/bin:/usr/bin"
#endif

/*----------------------------------------------------------------------
 * File creation mask.
 * Bits turned on here are turned off in newly created files.
 * This mask is the default when executing delivery files,
 * but mailboxes have their own mode value (MBX_MODE).
 */

#define UMASK   022

/*----------------------------------------------------------------------
 * Recursion limit.
 * If Deliver detects recursion deeper than this value,
 * it will assume infinite recursion and abort.
 */

#define REC_LIMIT  8

/*----------------------------------------------------------------------
 * Name of mailbox for undelivered mail.
 */

#define MBX_UNDEL     "Undel.mail"

/*----------------------------------------------------------------------
 * Delivery file directives.
 * When delivery files output these strings as "user names", they
 * are considered instructions.
 */

#define DFILE_DROP    "DROP"	/* Drop this message    */

/*----------------------------------------------------------------------
 * Names of delivery files.
 *
 * SYS_DELIVER          system-wide delivery file
 * POST_DELIVER         post-user delivery file
 * ERR_DELIVER          error delivery file
 * USER_DELIVER         user delivery file (in user's home directory)
 */

#ifdef DELHOME
#define SYS_DELIVER     "lib/sys"
#define POST_DELIVER    "lib/post"
#define ERR_DELIVER     "lib/err"
#else
#define SYS_DELIVER     "/usr/local/lib/deliver.sys"
#define POST_DELIVER    "/usr/local/lib/deliver.post"
#define ERR_DELIVER     "/usr/local/lib/deliver.err"
#endif

#define USER_DELIVER    ".deliver"

/*----------------------------------------------------------------------
 * Directory for temporary files.
 *
 * If the filesystem containing this directory becomes full, Deliver
 * will fail ungracefully.
 */

#define TEMPDIR		"/tmp"

/*----------------------------------------------------------------------
 * Log file names.
 * Errors and warnings are output to stderr and to the error log file.
 * To disable delivery logging, don't define LOG.
 * To disable error logging, don't define ERRLOG.
 *
 * Define LOGLOCK to be the temp file controlling access to log files.
 * This macro does not use TEMPDIR because log files may be shared with
 * other systems; if so, LOGLOCK must also be shared.
 */

#ifdef DELHOME
#define LOG             "log/logfile"
#define ERRLOG          "log/errlog"
#else
#define LOG             "/usr/adm/deliver.log"
#define ERRLOG          "/usr/adm/deliver.errlog"
#endif

#define LOGLOCK         "/tmp/dl.loglock"

/*----------------------------------------------------------------------
 * Environment variables passed to child processes.
 */

#define ENV_DPID        "DELPID"	/* Deliver process id           */
#define ENV_DLEVEL      "DELLEVEL"	/* Level of recursion           */
#define ENV_DFLAGS      "DELFLAGS"	/* Flags: [-[Avdt...]]          */

#define ENV_HOSTNAME    "HOSTNAME"	/* Name of this host            */
#define ENV_SYSDEL      "SYSDELFILE"	/* System delivery file         */
#define ENV_POSTDEL     "POSTDELFILE"	/* Post-user delivery file      */
#define ENV_ERRDEL      "ERRDELFILE"	/* Error delivery file          */
#define ENV_USERDEL     "USERDELFILE"	/* User delivery file           */

#define ENV_SENDER      "SENDER"	/* Message original sender      */
#define ENV_LOCALSENDER "LOCALSENDER"	/* Message local sender         */

#define ENV_HEADER      "HEADER"	/* Message header file          */
#define ENV_BODY        "BODY"		/* Message body file            */

/*----------------------------------------------------------------------
 * Perform local configuration.
 * It's last so it can override the content of this file.
 */

#include <local.h>
