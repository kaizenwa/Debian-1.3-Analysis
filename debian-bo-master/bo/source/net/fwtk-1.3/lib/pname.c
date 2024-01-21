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
static	char	RcsId[] = "Header: pname.c,v 1.5 94/11/01 11:55:30 mjr rel ";


#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<netdb.h>
#include	<syslog.h>
#include	<ctype.h>

#include	"firewall.h"

static	char	unknown[] = "unknown";

/*
	tell me who or what is connected to the other side of 'fd'.
	for paranoia's sake, we get the name from the address, then
	get the address for the name and cross-check them, flagging
	it as a questionable situation if they don't match. hosts
	that have "weird" name entries or mismatch or don't have
	reverse names, are set to "unknown"
	ALL NAMES ARE SQUASHED TO LOWER CASE
*/
peername(fd,lname,sname,z)
int	fd;
char	*lname;
char	*sname;
int	z;
{
	struct	sockaddr_in	a;
	struct	in_addr		*p_addr;
	struct	hostent		*p;
	int			y;
	int			eq = 0;

	y = sizeof(a);
	if(getpeername(fd,(struct sockaddr *)&a,&y) < 0)
		return(1);
	strncpy(sname,inet_ntoa(a.sin_addr),z);
	sname[z - 1] = '\0';

	p = gethostbyaddr(&a.sin_addr,sizeof(a.sin_addr),AF_INET);
	if(p != (struct hostent *)0) {
		char	*x;

		strncpy(lname,p->h_name,z);
		lname[z - 1] = '\0';

		p = gethostbyname(lname);
		if(p == (struct hostent *)0) {
			syslog(LLEV,"%s/%s host name lookup failed",lname,sname);
			goto badguy;
		}

		while((p_addr = (struct in_addr *)*p->h_addr_list++) != (struct in_addr *)0) {
			if(bcmp(p_addr,(char *)&a.sin_addr,p->h_length) == 0) {
				eq = 1;
				break;
			}
		}
		if(!eq) {
			bcopy(p->h_addr,&a.sin_addr,p->h_length);
			syslog(LLEV,"securityalert: possible spoof %s/%s != %s name lookup mismatch",lname,sname,inet_ntoa(a.sin_addr));
			goto badguy;
		}

#ifdef IP_OPTIONS
		{
			unsigned char	optbuf[1024 / 3];
			unsigned char	*cp;
			char		lbuf[1024];
			char		*lp;
			int		optsize;
			int		ipproto;
			struct protoent	*ip;

			if((ip = getprotobyname("ip")) != (struct protoent *)0)
				ipproto = ip->p_proto;
			else
				ipproto = IPPROTO_IP;
			optsize = sizeof(optbuf);
			if(getsockopt(0,ipproto,IP_OPTIONS,(char *)optbuf,&optsize) == 0 && optsize != 0) {
				lp = lbuf;
				for(cp = optbuf; optsize > 0; cp++, optsize--, lp += 3)
					sprintf(lp," %2.2x",*cp);
				syslog(LLEV,"securityalert: Connection received using IP options (ignored):%s",lbuf);
				if(setsockopt(0,ipproto,IP_OPTIONS,(char *)0,optsize) != 0) {
					syslog(LLEV,"setsockopt IP_OPTIONS NULL: %m");
					goto badguy;
				}
			}
		}
#endif

		for(x = lname; *x != '\0'; x++)
			if(isupper(*x))
				*x = tolower(*x);
		return(0);
	}
badguy:
	strcpy(lname,unknown);
	return(0);
}
