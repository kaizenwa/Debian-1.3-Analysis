#ifndef CONF_H
#define CONF_H

/*
 *
 *	RADIUS
 *	Remote Authentication Dial In User Service
 *
 *
 *	Livingston Enterprises, Inc.
 *	6920 Koll Center Parkway
 *	Pleasanton, CA   94566
 *
 *	Copyright 1992 Livingston Enterprises, Inc.
 *
 *	Permission to use, copy, modify, and distribute this software for any
 *	purpose and without fee is hereby granted, provided that this
 *	copyright and permission notice appear on all copies and supporting
 *	documentation, the name of Livingston Enterprises, Inc. not be used
 *	in advertising or publicity pertaining to distribution of the
 *	program without specific prior permission, and notice be given
 *	in supporting documentation that copying and distribution is by
 *	permission of Livingston Enterprises, Inc.
 *
 *	Livingston Enterprises, Inc. makes no representations about
 *	the suitability of this software for any purpose.  It is
 *	provided "as is" without express or implied warranty.
 *
 */

/*
 *	@(#)conf.h	0.1 10/11/94
 */

#define	RADIUS_VERSION		" 2.4.23C " /* MUST be surrounded by spaces */

#if defined(__alpha)
typedef unsigned int UINT4;
typedef int INT4;
#else	/* defined(alpha) */
typedef unsigned long UINT4;
typedef long INT4;
#endif	/* defined(alpha) */

#if defined(unixware) || defined(sys5) || defined(M_UNIX) || defined(sun)
#include	<string.h>
#else	/* unixware */
#include	<strings.h>
#endif	/* unixware */

#if defined(bsdi)
#include	<machine/inline.h>
#include	<machine/endian.h>
#else	/* bsdi */
#ifndef	__FreeBSD__
#include	<malloc.h>
#endif	/* __FreeBSD__ */
#endif	/* bsdi */

/* #ifdef SYSV this is needed even on SunOS 4.1.3 */
#include	<pwd.h>
/* #endif XXX SYSV */

#if defined(aix)
#include	<sys/select.h>
#endif	/* aix */

#if defined(__hpux)
#include	<unistd.h>
#endif	/* __hpux */

#ifdef	USE_NDBM
#include	<ndbm.h>
#include	<fcntl.h>
#endif	/* USE_NDBM */

#if defined(USE_DBM)
#include	<dbm.h>
#if defined(sun)
#define NULL 0
#endif	/* sun */
#if defined(__sgi)
#ifdef dbm_error64
#define store store64
#define dbminit dbminit64
#define dbmclose dbmclose64
#endif	/* dbm_error64 */
#endif	/* __sgi */
#endif	/* USE_DBM */

#if defined(linux)
#include	<unistd.h>
#define MIN(a, b)      ((a) < (b) ? (a) : (b))
#define MAX(a, b)      ((a) > (b) ? (a) : (b))
#endif	/* linux */

#endif /* CONF_H */
