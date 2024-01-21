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
static	char	RcsId[] = "Header: rand.c,v 1.2 94/02/11 11:03:54 mjr rel ";


#include	"firewall.h"

#ifdef	USE_RANDOM
long
randomnumber()
{
	static	int	initt = 0;

	if(!initt) {
		long	tbuf;

		time(&tbuf);
		srandom(((int)tbuf % getpid()) + getppid());
		initt++;
	}
	return(random());
}
#endif

#ifdef	USE_RAND
long
randomnumber()
{
	static	int	initt = 0;

	if(!initt) {
		long	tbuf;

		time(&tbuf);
		srand(((int)tbuf % getpid()) + getppid());
		initt++;
	}
	return(rand());
}
#endif
