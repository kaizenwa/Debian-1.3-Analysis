/*
 *
 *	RADIUS -- Remote Authentication Dial In User Service
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
 *      Public entry points in this file:
 *
 *      version
 */

static char     sccsid[] =
		"@(#)version.c	1.1 Copyright 1992 Livingston Enterprises Inc";

static char     rcsid[] = "$Id: version.c,v 1.16 1996/05/17 13:59:56 web Exp $";

#include        <sys/types.h>
#include	<netinet/in.h>
#include	<stdio.h>
#include	<time.h>

#include	"radius.h"

/*
 *	If you make any changes to this software, please update the
 *	version number (in conf.h).  Contact support@livingston.com
 *	if you would like a range of versions allocated for your use.
 */

/*************************************************************************
 *
 *	Function: version
 *
 *	Purpose: Return version information for this build of the server.
 *
 *************************************************************************/

char *
version ()

{
	static char        buf[256];

	sprintf (buf, "Version %s", RADIUS_VERSION);

	/* here are all the conditional feature flags */

#if defined(USE_DBM)
	strcat (buf, " DBM");
#endif

#if defined(USE_NDBM)
	strcat (buf, " NDBM");
#endif

#if defined(ASCEND)
	strcat (buf, " ASCEND(limited)");
#endif

#if defined(NOSHADOW)
	strcat (buf, " NOSHADOW");
#endif

	/* here are all the system definitions compilation uses */

#if defined(__alpha)
	strcat (buf, " __alpha");
#endif

#if defined(__osf__)
	strcat (buf, " __osf__");
#endif

#if defined(aix)
	strcat (buf, " aix");
#endif

#if defined(bsdi)
	strcat (buf, " bsdi");
#endif

#if defined(linux)
	strcat (buf, " linux");
#endif

#if defined(sun)
	strcat (buf, " sun");
#endif

#if defined(sys5)
	strcat (buf, " sys5");
#endif

#if defined(unixware)
	strcat (buf, " unixware");
#endif

#if defined(M_UNIX)
	strcat (buf, " M_UNIX");
#endif

#if defined(__sgi)
	strcat (buf, " __sgi");
#endif

#if defined(__hpux)
	strcat (buf, " HP-UX");
#endif

#if defined(M_KERB)
	strcat (buf, " M_KERB");
#endif

#if defined(A_KERB)
	strcat (buf, " A_KERB");
#endif

#if defined(MINOS)
	strcat (buf, " MINOS");
#endif

#if defined(TACACS)
	strcat (buf, " TACACS");
#endif

#if defined(KCHAP)
	strcat (buf, " KCHAP");
#endif

#if defined(ultrix) || defined(ULTRIX_ENHANCED)
#if defined(ultrix)
	strcat (buf, " ultrix");
#else
	strcat (buf, " ULTRIX_ENHANCED");
#endif	/* ultrix */
#endif

#if defined(MERIT_LAS)
	strcat (buf, " LAS = ");
	strcat (buf, LAS_VERSION);
#endif	/* LAS */

#if defined(MERIT_ORGANIZATION)
	strcat (buf, " OAS = ");
	strcat (buf, OAS_VERSION);
#endif	/* OAS */

#if defined(MERIT_HUNTGROUP)
	strcat (buf, " HGAS = ");
	strcat (buf, HGAS_VERSION);

#if defined(MERIT_HUNTGROUP_DAC)
	strcat (buf, " DAC");
#endif	/* DAC */

#if defined(MERIT_HUNTGROUP_SHP)
	strcat (buf, " SHP");
#endif	/* SHP */
#endif	/* MERIT_HUNTGROUP */

	return buf;
} /* end of version () */
