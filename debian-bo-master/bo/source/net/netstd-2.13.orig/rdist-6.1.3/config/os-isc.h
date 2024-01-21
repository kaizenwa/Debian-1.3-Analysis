/*
 * Copyright (c) 1993 Michael A. Cooper
 * Copyright (c) 1993 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * $Id: os-isc.h,v 1.7 1994/03/31 04:33:54 mcooper Exp $
 */

/*
 *  os-isc.h file (INTERACTIVE UNIX)
 */

/*
 * Define the following name for use in #ifdef's.
 * The value should be all upper-case with no periods (.).
 */
#if	!defined(ISC)
#define ISC
#endif

/*
 * Set process args to messages that show up when running ps(1)
 *
 * Under some OS's, the SETARGS code will cause ": is not an identifier"
 * errors for "special" commands.
 */
#define SETARGS

/*
 * Define the type of directory routines your system has.
 */
#define DIR_TYPE	DIR_DIRENT

/*
 * Determine what routines we have to get filesystem info.
 */
#define FSI_TYPE	FSI_GETMNTENT2

/*
 * Type of non-blocking I/O.
 */
#define NBIO_TYPE	NBIO_FCNTL

/*
 * Type of wait() function to use.
 */
#define WAIT_TYPE	WAIT_WAITPID

/*
 * Type of argument passed to wait() (above).
 */
#define WAIT_ARG_TYPE	int

/*
 * Select the type of executable file format.
 */
#define EXE_TYPE	EXE_COFF

/*
 * Select the type of statfs() system call (if any).
 */
#define STATFS_TYPE	STATFS_SYSV

/*
 * Type of arg functions we have.
 */
#define ARG_TYPE	ARG_VARARGS

/*
 * UID argument type for chown()
 */
typedef uid_t CHOWN_UID_T;

/*
 * GID argument type for chown()
 */
typedef gid_t CHOWN_GID_T;

/*
 * Our types, usually these are uid_t and gid_t.
 */
typedef long UID_T;	/* Must be signed */
typedef long GID_T;	/* Must be signed */

/*
 * Generic pointer, used by memcpy, malloc, etc.  Usually char or void.
 */
typedef void POINTER;

/*
 * Type of set file time function available
 */
#define SETFTIME_TYPE	SETFTIME_UTIME

/*
 * Type of set line buffering function available
 */
#define SETBUF_TYPE	SETVBUF

/*
 * Our socket pair type
 */
#define SOCKPAIR_TYPE	SOCKPAIR_SPIPE

/*
 * Things we have
 */
/*#define HAVE_FCHOWN			/* Have fchown() */
/*#define HAVE_FCHMOD			/* Have fchmod() */
#define HAVE_SELECT			/* Have select() */
#define HAVE_SAVED_IDS			/* Have POSIX style saved [ug]id's */
#define POSIX_SIGNALS			/* Have POSIX signals */

/*
 * Things we need
 */
#define NEED_UTIME_H			/* Need <utime.h> */
#define NEED_LIMITS_H			/* Need <limits.h> */
/*#define NEED_UNISTD_H			/* Need <unistd.h> */

/*
 * Path to the remote shell command.
 * Define this only if the pathname is different than
 * that which appears in "include/paths.h".
 */
/*#define _PATH_REMSH	"/usr/bin/rsh"			/**/

/*
 * Miscellaneous
 */
#define MNTTAB 		"/etc/mnttab"
#define NFS_TYPE_NAME 	"NFS"
#define MAXPATHLEN 	PATH_MAX
#define MNTTAB_H	<mnttab.h>	/* Name of mnttab.h include file */
#include <sys/bsdtypes.h>

/*
 * Filesystem compatibility
 */
#define mnt_mountp	mt_filsys
#define mnt_fstype	mt_fstyp
