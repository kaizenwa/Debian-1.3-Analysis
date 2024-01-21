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
static	char	RcsId[] = "Header: hnam.c,v 1.2 94/05/27 13:27:22 mjr rel ";

#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<netdb.h>
#include	<syslog.h>
#include	<ctype.h>

extern	char	*inet_ntoa();


#include	"firewall.h"


char	*
maphostname(name)
char	*name;
{
	struct	hostent		*hp;
	struct	sockaddr_in	sin;
	char			*p;

	p = name;
	while(*p != '\0' && (*p == '.' || *p == '*' || isdigit(*p)))
		p++;

	if(*p == '\0')
		return(name);

	if((hp = gethostbyname(name)) == (struct hostent *)0)
		return(name);

	bcopy(hp->h_addr,&sin.sin_addr,hp->h_length);
	return(inet_ntoa(sin.sin_addr));
}
