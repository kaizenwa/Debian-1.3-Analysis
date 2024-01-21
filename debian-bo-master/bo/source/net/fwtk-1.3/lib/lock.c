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
static	char	RcsId[] = "Header: lock.c,v 1.2 93/11/11 16:25:52 mjr rel ";


#include	<sys/types.h>
#include	<sys/file.h>

#include "firewall.h"

#ifdef	LOCK_LOCKF
#include	<unistd.h>
#endif


/*
	This stupid little module implements system specific
locking protocols. If you need to add another one, feel free to.
The only requirement for locking is that it survive across an
exec().
*/



extern	int	errno;
lock_fd(f)
int	f;
{
#ifdef	LOCK_LOCKF
	return(lockf(f,F_LOCK,(off_t)0));
#endif
#ifdef	LOCK_FLOCK
	return(flock(f,LOCK_EX));
#endif
}



lockun_fd(f)
int	f;
{
#ifdef	LOCK_LOCKF
	return(lockf(f,F_ULOCK,(off_t)0));
#endif
#ifdef	LOCK_FLOCK
	return(flock(f,LOCK_UN));
#endif
}



locktest_fd(f)
int	f;
{
#ifdef	LOCK_LOCKF
	return(lockf(f,F_TLOCK,(off_t)0));
#endif
#ifdef	LOCK_FLOCK
	return(flock(f,LOCK_EX|LOCK_NB));
#endif
}
