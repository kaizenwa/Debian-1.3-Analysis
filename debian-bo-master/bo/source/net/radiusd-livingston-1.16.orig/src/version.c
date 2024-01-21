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

static char sccsid[] =
"@(#)version.c	1.2 Copyright 1992 Livingston Enterprises Inc";

#include        <sys/types.h>
#include	<stdio.h>
#include	"radius.h"

extern char	*progname;

/*  If you make any changes to this software please update this
 *  version number; contact support@livingston.com if you
 *  would like a range of versions allocated for your use.
 */

#define		VERSION		"1.16 95/01/06"

/*************************************************************************
 *
 *	Function: version
 *
 *	Purpose: Display the revision number for this program
 *
 *************************************************************************/

version()
{

	fprintf(stderr, "%s: RADIUS version %s\n", progname, VERSION);

	/* here are all the conditional feature flags */
#if defined(DBM)
	fprintf(stderr," DBM");
#endif
#if defined(NOSHADOW)
	fprintf(stderr," NOSHADOW");
#endif

	/* here are all the system definitions compilation uses */
#if defined(__alpha)
	fprintf(stderr," __alpha");
#endif
#if defined(__osf__)
	fprintf(stderr," __osf__");
#endif
#if defined(aix)
	fprintf(stderr," aix");
#endif
#if defined(bsdi)
	fprintf(stderr," bsdi");
#endif
#if defined(sun)
	fprintf(stderr," sun");
#endif
#if defined(sys5)
	fprintf(stderr," sys5");
#endif
#if defined(unixware)
	fprintf(stderr," unixware");
#endif
#if defined(M_UNIX)
	fprintf(stderr," M_UNIX");
#endif
	fprintf(stderr,"\n");
	exit(-1);
}
