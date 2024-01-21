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
static	char	RcsId[] = "Header: mapu.c,v 1.3 94/05/27 13:27:25 mjr rel ";


#include	<pwd.h>
#include	<ctype.h>
#include	<syslog.h>

#include	"firewall.h"


int
mapuid(user)
char	*user;
{
	struct passwd	*pw;

	if(isalldigits(user))
		return(atoi(user));
	if((pw = getpwnam(user)) == (struct passwd *)0)
		return(-1);
	return(pw->pw_uid);
}
