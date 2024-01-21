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
 * $Id: os-dynixptx.h,v 6.11 1994/03/31 04:09:15 mcooper Exp $
 * @(#)os-dynixptx.h
 */

/*
 * System V Release 4 (dynixptx)
 */

/*
 * Define the following name for use in #ifdef's.
 * The value should be all upper-case with no periods (.).
 */
#if	!defined(dynixptx)
#define dynixptx
#endif

/*
 * We need <utime.h>
 */
#define NEED_UTIME_H

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
#define FSI_TYPE	FSI_GETMNTENT

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
#define FILEHDR_H	<filehdr.h>	/* Name of <filehdr.h> include file */

/*
 * Select the type of statfs() system call (if any).
 */
#define STATFS_TYPE	STATFS_SYSV

/*
 * Type of arg functions we have.
 */
#define ARG_TYPE	ARG_VARARGS

/*
 * Do we have select()?
 */
#define HAVE_SELECT

/*
 * Type of signal handling functions.
 */
#if	!defined(POSIX_SIGNALS)
#define POSIX_SIGNALS
#endif
#include <signal.h>
#define signal(s,a) sigset(s,(void(*)())a)

/*
 * System V compatibility
 */
#include <string.h>
#include <fcntl.h>
#include <sys/select.h>
#include <time.h>
#include <netdb.h>

#define bcopy(a,b,c) 	memcpy(b,a,c)
#define bzero(a,b) 	memset(a,0,b)
#define setlinebuf(a)	setvbuf(a, NULL, _IOLBF, BUFSIZ)


/*
 * UID argument type for chown()
 */
typedef uid_t CHOWN_UID_T;

/*
 * GID argument type for chown()
 */
typedef gid_t CHOWN_GID_T;

/*
 * Our types
 */
typedef uid_t UID_T;	/* Must be signed */
typedef gid_t GID_T;	/* Must be signed */

/*
 * Generic pointer, used by memcpy, malloc, etc.  Usually char or void.
 */
typedef char POINTER;

/*
 * Type of set file time function available
 */
#define SETFTIME_TYPE	SETFTIME_UTIME

#define MOUNTED MNTTABNAME

/*
 * We have fchown()
 */
#undef HAVE_FCHOWN

/*
 * We have fchmod()
 */
#undef HAVE_FCHMOD

/*
 * We don't have u_char
 */
typedef unsigned char u_char;

/*
 * Path to the remote shell command
 */
#define _PATH_REMSH	"/usr/bin/resh"
