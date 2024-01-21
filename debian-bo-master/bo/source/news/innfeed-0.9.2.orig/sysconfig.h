/* -*- c -*-
 *
 * Author:      James A. Brister <brister@vix.com> -- berkeley-unix --
 * Start Date:  Thu, 01 Feb 1996 22:17:51 +1100
 * Project:     INN -- innfeed
 * File:        sysconfig.h
 * RCSId:       $Id: sysconfig.h,v 1.9 1996/11/26 08:56:05 brister Exp $
 *
 * Copyright:   Copyright (c) 1996 by Internet Software Consortium
 *
 *              Permission to use, copy, modify, and distribute this
 *              software for any purpose with or without fee is hereby
 *              granted, provided that the above copyright notice and this
 *              permission notice appear in all copies.
 *
 *              THE SOFTWARE IS PROVIDED "AS IS" AND INTERNET SOFTWARE
 *              CONSORTIUM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 *              SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *              MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL INTERNET
 *              SOFTWARE CONSORTIUM BE LIABLE FOR ANY SPECIAL, DIRECT,
 *              INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *              WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *              WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 *              TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 *              USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Description: This file should be the only thing you need to touch when
 *              moving innfeed across platforms.
 *
 *              This file is broken into two sections. The first contains
 *              all defines etc. that are private to innfeed. The second
 *              contains defines etc. that identical to those in inn. By
 *              compiling with '-DUSE_INN_INCLUDES' and an appropriate '-I'
 *              you won't actually need to touch this second section.
 * 
 */

/* PLEASE FORWARD ANY CHANGES YOU HAVE TO MAKE TO <brister@vix.com> AND LET
   ME KNOW WHAT, WHY AND FOR WHAT PLATFORM. THANKS. */

#ifndef sysconfig_h__
#define sysconfig_h__ 1

/***************************************************************************
 **                      INNFEED PRIVATE SECTION                          **
 ***************************************************************************/

/*
** BSD/OS
*/

#if defined (__bsdi__)
#define MAX_WRITEV_VEC 1024
#endif

/*
** FreeBSD
*/

#if defined (__FreeBSD__)
#define MAX_WRITEV_VEC 1024
#endif

/*
** NetBSD
**
*/

#if defined (__NetBSD__)
#define MAX_WRITEV_VEC 1024
#endif


/*
** LINUX
*/

#if defined (linux)

/* Note: If you are running version 5.4.3 or better of libc, then 16 is the
   number to use. Lower than that, and you should use 1. */
#define MAX_WRITEV_VEC 1
#endif


/*
** DEC Unix
**
*/

#if defined (__osf__)
#define GETSOCKOPT_ARG  char *
#define MAX_WRITEV_VEC  1024
#endif


/*
** SOLARIS
**
*/

#if defined (SOLARIS)
/* #define DO_NEED_STRERROR 1 */  /* this is needed for version < 2.5 */
#define GETSOCKOPT_ARG  char *
#define MAX_WRITEV_VEC  16
#define DO_NEED_STREAM 1

#define wait3(a,b,c) waitpid(-1,a,b)
#endif /* defined (SOLARIS) */


/*
** SunOS 4.x
*/

#if defined (sun) && ! defined (SOLARIS)
#define MAX_WRITEV_VEC 16
#define DO_NEED_STRERROR 1
#endif


/*
** IRIX
**
*/

#if defined (__sgi)
#define MAX_WRITEV_VEC 16    /* actually bigger on 5.2, but this on 5.3 and 6.x */
#endif


/*
** NEC UX/4800
**
*/
#if defined (_nec_ews)
#define DO_NEED_STRDUP 1
#define MAX_WRITEV_VEC 16
#define DO_NEED_STREAM 1
#endif


/*
** AIX 3.2.x, 4.1.3
** 
** NOTE FOR 3.2.x. Be careful ! GCC ONLY !!  If you compile
** innfeed 0.9.1 with AIX cc, machine will be rebooted during building
** connection.o. You can't believe it but I tested it with two different
** AIX 3.2.5 boxes 10 times !! :)
** 1996/11/10, seokchan lee <chan@plaza.snu.ac.kr>
*/

#if defined (_AIX32)
#define DO_NEED_STREAM 1
#define DONT_USE_UNION_WAIT 1
#define FDCOUNT_GETDTAB 1
#define MAX_WRITEV_VEC 16
#define UIO_MAXIOV 16
#define GETSOCKOPT_ARG  char *
/* If UIO_MAXIOV not defined, AIX 4.1.x defines UIO_MAXIOV as 1024. (see
   /usr/include/sys/socket.h). But "make check-maxiov" says it should
   be 16 and it works. There is no UIO_MAXIOV predefines for 3.2.x */
#define LOG_PERROR 0
#define _BSD 44
#endif



  /* Defaults below here. If you need to change something it should really
     be done in the architecture specific section just above. */

/* define DONT_NEED_STRDUP or DO_NEED_STRDUP depending on
   if you a strdup on your machine or not. */
#if ! defined (DO_NEED_STRDUP) && ! defined (DONT_NEED_STRDUP)
#define DONT_NEED_STRDUP 1
#endif

/* define DONT_NEED_U_INT or DO_NEED_U_INT depending on if you
   have a `u_long', `u_int', `u_short' in your system include path or not */
#if ! defined (DO_NEED_U_INT) && ! defined (DONT_NEED_U_INT)
#define DONT_NEED_U_INT 1
#endif

/* define DONT_NEED_BOOL or DO_NEED_BOOL depending on if you have a `bool'
   in your include path or not */
#if ! defined (DO_NEED_BOOL) && ! defined (DONT_NEED_BOOL)
#define DO_NEED_BOOL 1
#endif


/* maximum number of struct iovec in a writev.  Build and run the program
   uio_maxiov to determine the proper value to use here (or look for the
   define for UIO_MAXIOV in your system's include file).. Please send me
   <brister@vix.com> value you determined and the appropriate CPP symbols
   to test for your system. 16 is the smallest number I've come across
   yet. Having a number that's too small won't break anything. Having a
   number that's too big will. */
#if ! defined (MAX_WRITEV_VEC)
#define MAX_WRITEV_VEC 16
#endif


/* Defined DO_NEED_STREAM or DONT_NEED_STREAM depending on if you need to
   include <sys/stream.h> included (Solaris and other SVR4(?)) */
#if ! defined (DO_NEED_STREAM) && ! defined (DONT_NEED_STREAM)
#define DONT_NEED_STREAM 1
#endif


/* Define DONT_NEED_STRERROR or DO_NEED_STRERROR depending on if you have
   strerror() in your libraries */
#if ! defined (DO_NEED_STRERROR) && ! defined (DONT_NEED_STRERROR)
#define DONT_NEED_STRERROR 1
#endif


/* argument type for 4th argument to getsockopt. */
#if ! defined (GETSOCKOPT_ARG)
#define GETSOCKOPT_ARG void *
#endif



/***************************************************************************
 **                      INN SHARED SECTION                               **
 ***************************************************************************/

/*
 * If you compile with -DUSE_INN_INCLUDES (and an appropriate '-I' from
 * within) the INN source tree, then you shouldn't need to touch this next
 * section 
 */

#if defined (USE_INN_INCLUDES)
#include "configdata.h"
#include "logging.h"
#else

/* all the defines below here match INN in its configdata.h or logging.h  */

#if defined (__bsdi__)          /* BSD/OS */

#elif defined (__NetBSD__)      /* NetBSD */

#define DONT_USE_UNION_WAIT 1

#elif defined (linux)             /* LINUX */

#elif defined (__osf__)         /* DEC Unix */

#define DONT_USE_UNION_WAIT 1

#elif defined (SOLARIS)         /* SunOS 5.x */

#define DO_HAVE_WAITPID 1
#define LOG_PERROR 0

#elif defined (sun) && !defined (SOLARIS) /* SunOS 4.x */

#define LOG_PERROR 0

#elif defined (_nec_ews)

#define DO_HAVE_WAITPID 1
#define LOG_PERROR 0

#endif


/* how syslog should be opened. LOG_PID not needed--is added automatically. */
#define L_OPENLOG_FLAGS (LOG_NDELAY | LOG_PERROR)

/* same as INN's define DO_HAVE_UNISTD or DONT_HAVE_UNISTD */
#if ! defined (DO_HAVE_UNISTD) && ! defined (DONT_HAVE_UNISTD)
#define DO_HAVE_UNISTD 1
#endif

/* same as INN's define DO_HAVE_UNIX_DOMAIN or DONT_HAVE_UNIX_DOMAIN */
#if ! defined (DO_HAVE_UNIX_DOMAIN) && ! defined (DONT_HAVE_UNIX_DOMAIN)
#define DO_HAVE_UNIX_DOMAIN 1
#endif

/* same as INN's for maximum number of file descriptors. Choose one of these */
/* #define FDCOUNT_GETRLIMIT 1 */
/* #define FDCOUNT_GETDTAB 1 */
/* #define FDCOUNT_SYSCONF 1 */
/* #define FDCOUNT_ULIMIT 1 */
/* #define FDCOUNT_CONSTANT 1 */

#if ! defined (FDCOUNT_GETRLIMIT) && ! defined (FDCOUNT_GETDTAB)
#if ! defined (FDCOUNT_SYSCONF) && ! defined (FDCOUNT_ULIMIT)
#if ! defined (FDCOUNT_CONSTANT)
#define FDCOUNT_GETRLIMIT 1
#endif
#endif
#endif


/* Same as INN's. defined DO_USE_UNION_WAIT or DONT_USE_UNION_WAIT */
#if ! defined (DO_USE_UNION_WAIT) && ! defined (DONT_USE_UNION_WAIT)
#define DONT_USE_UNION_WAIT 1
#endif

/* define DO_HAVE_WAITPID or DONT_HAVE_WAITPID depending on if you have to
   use waitpid() rather than wait3() */
#if ! defined (DO_HAVE_WAITPID) && ! defined (DONT_HAVE_WAITPID)
#define DONT_HAVE_WAITPID 1
#endif
 
/* define NBIO_FCNTL or NBIO_IOCTL depending on the type of non-blocking
   i/o you use. */
#if ! defined (NBIO_FCNTL) && ! defined (NBIO_IOCTL)
#define NBIO_FCNTL 1
#endif

/* define DO_NEED_TIME or DONT_NEED_TIME depending on if you need <time.h> as
   well as <sys/time.h> */
#if ! defined (DO_NEED_TIME) && ! defined (DONT_NEED_TIME)
#define DO_NEED_TIME 1
#endif

/* define DO_BIND_USE_SIZEOF or DONT_BIND_USE_SIZEOF depending on your
   AF_UNIX bind use sizeof for the socket size? */
#if ! defined (DO_BIND_USE_SIZEOF) && ! defined (DONT_BIND_USE_SIZEOF)
#define DO_BIND_USE_SIZEOF 1
#endif

/* define DIR_DIRENT or DIR_DIRECT depending on the type of your system's
   directory reading routines */
#if ! defined (DIR_DIRENT) && ! defined (DIR_DIRECT)
#define DIR_DIRENT 1
#endif


/* define DO_HAVE_SETBUFFER if you have setbuffer(), or DONT_HAVE_SETBUFFER
   to use setbuf(). */
#if ! defined (DO_HAVE_SETBUFFER) && ! defined (DONT_HAVE_SETBUFFER)
#define DONT_HAVE_SETBUFFER 1
#endif

#endif /* defined (USE_INN_INCLUDES) */
#endif /* sysconfig_h__ */
