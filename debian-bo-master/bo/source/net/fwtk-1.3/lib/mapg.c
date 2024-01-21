/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
 */
static	char	RcsId[] = "Header: mapg.c,v 1.2 94/05/27 13:28:09 mjr rel ";


#include	<sys/types.h>
#include	<grp.h>
#include	<ctype.h>
#include	<syslog.h>

#include	"firewall.h"


int
mapgid(gnam)
char	*gnam;
{
	struct group	*gp;

	if(isalldigits(gnam))
		return(atoi(gnam));
	if((gp = getgrnam(gnam)) == (struct group *)0)
		return(-1);
	return(gp->gr_gid);
}
