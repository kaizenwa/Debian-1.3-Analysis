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
static	char	RcsId[] = "Header: alldi.c,v 1.1 93/10/20 11:13:39 mjr rel ";


#include	<ctype.h>



isalldigits(s)
char	*s;
{
	while(isdigit(*s))
		s++;
	return(*s == '\0');
}
