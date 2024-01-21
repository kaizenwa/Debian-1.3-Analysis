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
static	char	RcsId[] = "Header: conn.c,v 1.2 93/11/11 16:25:47 mjr rel ";


#include	<ctype.h>
#include	<sys/types.h>
#include	<sys/time.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<netdb.h>
#include	<syslog.h>
extern	int	errno;
extern	char	*sys_errlist[];

#include	"firewall.h"


conn_server(srv,portnum,priv,rbuf)
char	*srv;
int	portnum;
int	priv;
char	*rbuf;
{
	struct	sockaddr_in	addr;
	struct	hostent		*hp;
	int			fd;
	char			*p;

	p = srv;
	while(*p != '\0' && (*p == '.' || isdigit(*p)))
		p++;

	/* not all digits or dots */
	if(*p != '\0') {
		if((hp = gethostbyname(srv)) == (struct hostent *)0) {
			if(rbuf != (char *)0)
				strcpy(rbuf,"hostname unknown");
			return(-1);
		}

		(void)bcopy(hp->h_addr,(char *)&addr.sin_addr,hp->h_length);
	} else {
		unsigned long	f;

		if((f = inet_addr(srv)) == -1L) {
			if(rbuf != (char *)0)
				strcpy(rbuf,"hostname unparsable");
			return(-1);
		}
		(void)bcopy((char *)&f,(char *)&addr.sin_addr,sizeof(f));
	}

	addr.sin_port = htons(portnum);
	addr.sin_family = AF_INET;

	if(priv) {
		int	lport = IPPORT_RESERVED - 1;
		fd = rresvport(&lport);
	} else
		fd = socket(AF_INET,SOCK_STREAM,0);
	
	if(fd < 0) {
		if(rbuf != (char *)0)
			sprintf(rbuf,"socket: %s",sys_errlist[errno]);
		return(-2);
	}

	if(connect(fd,(struct sockaddr *)&addr,sizeof(addr)) < 0) {
		if(rbuf != (char *)0)
			sprintf(rbuf,"connect: %s",sys_errlist[errno]);
		return(-3);
	}
	return(fd);
}
