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
static	char	RcsId[] = "Header: srvio.c,v 1.3 94/05/27 14:10:15 mjr rel ";
#include	<stdio.h>
#include	<syslog.h>
#include	<sys/signal.h>

#include	"firewall.h"
#include	"auth.h"


/* #define	DEBUG */


srvsayinit(confp,rl,ri)
Cfg	*confp;
char	*rl;
char	*ri;
{
	Cfg	*cf;

	cf = cfg_get("hosts",confp);
	while(cf != (Cfg *)0) {
		if(cf->argc < 1)
			goto skip;

		if(hostmatch(cf->argv[0],ri)) {
			if(cf->flags & PERM_DENY)
				goto deny;
			return(0);
		}
skip:
		cf = cfg_get("hosts",(Cfg*)0);
	}
deny:
	syslog(LLEV,"deny host=%s/%s",rl,ri);
	return(1);
}



srvsay(buf)
char	*buf;
{
	int	x;

#ifdef	DEBUG
	syslog(LLEV,"srvsay %s\n",buf);
#endif
	x = strlen(buf);
	if(write(0,buf,x) != x || write(0,"\n",1) != 1)
		return(1);
	return(0);
}


srvhear(buf,siz)
char	*buf;
int	siz;
{
	int	x = 0;

	while(1) {
		if(x + 1 == siz) {
			syslog(LLEV,"fwtksyserr: srvhear: buffer overflow");
			return(1);
		}
		switch(read(0,&buf[x],1)) {
		case 0:
			return(1);
		case 1:
			break;
		default:
			syslog(LLEV,"fwtksyserr: srvhear: read error: %m");
			return(1);
		}
		/* KLUDGE! */
		if(buf[x] == '\r')
			continue;
		if(buf[x] == '\n')
			break;
		x++;
	}
	buf[x] = '\0';

#ifdef	DEBUG
	syslog(LLEV,"srvhear %s\n",buf);
#endif
	return(0);
}
