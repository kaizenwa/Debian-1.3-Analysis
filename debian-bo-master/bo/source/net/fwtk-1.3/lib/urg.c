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
static	char	RcsId[] = "Header: urg.c,v 1.3 94/11/01 11:55:35 mjr rel ";

#include	"firewall.h"

#include	<sys/types.h>
#include	<sys/ioctl.h>
#include	<sys/socket.h>
#include	<fcntl.h>
#include	<syslog.h>


/* set out of band signalling on the specified file descriptor */
set_oob_notification(fd)
int	fd;
{
#ifdef	USE_F_SETOWN
	if(fcntl(fd,F_SETOWN,getpid()) < 0) {
		syslog(LLEV,"fwtksyserr: F_SETOWN: %m");
		return(1);
	}
	return(0);
#endif



#ifdef	USE_SIOCSPGRP
	{
		int	xpid;
		int	async;

		xpid = getpid();
		async = 1;
		if(ioctl(0,SIOCSPGRP,&xpid) || ioctl(0,FIOASYNC,&async)) {
			syslog(LLEV,"fwtksyserr: SIOCSPGRP: %m");
			return(1);
		}
	}
	return(0);
#endif
}
