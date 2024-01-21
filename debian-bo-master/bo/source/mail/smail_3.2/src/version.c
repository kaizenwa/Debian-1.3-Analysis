/*
#ident	"@(#)smail/src:RELEASE-3_2:version.c,v 1.13 1996/02/28 06:47:25 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * version:
 *	return the current smail version
 */
#include <sys/types.h>
#include <stdio.h>
#include "defs.h"
#include "version.h"
#ifndef DEPEND
# include "extern.h"
#endif

/*
 * version - keep track of the smail version number
 *
 * external functions:
 *	version
 */
char *version_number = VERSION;
char *release_date = RELEASE_DATE;

static char *fmtver = NULL;	/* no version in the beginning */

char *
version()
{
    /*
     * form the version string for the first time if needed
     */
    if (!fmtver) {
	fmtver = xmalloc(sizeof("Smail- x #dddddd") + strlen(version_number) + strlen(release_date));
	(void) sprintf(fmtver, "Smail-%s %s #%d", version_number, release_date, compile_num);
    }

    /*
     * return the formatted version string
     */
    return fmtver;
}
