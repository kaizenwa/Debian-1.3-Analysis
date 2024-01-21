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
static	char	RcsId[] = "Header: cliio.c,v 1.3 94/11/01 11:52:16 mjr rel ";
#include	<stdio.h>
#include	<syslog.h>
#include	<sys/signal.h>

#include	"firewall.h"
#include	"auth.h"


#define	DEBUG 
static	int	rem_authfd = -1;

auth_open(confp)
Cfg	*confp;
{
	int	authport = AUTH_DEFAULTPORT;
	char	*authserver = AUTH_DEFAULTSERVER;
	Cfg	*cf;

	if(rem_authfd != -1)
		return(0);

	if(confp != (Cfg *)-1 && confp != (Cfg *)0) {
		if((cf = cfg_get("authserver",confp)) != (Cfg *)0) {
			if(cf->argc < 1) {
				syslog(LLEV,"fwtkcfgerr: authserver not provided, line %d",cf->ln);
				return(1);
			}
			authserver = cf->argv[0];
			if(cf->argc > 1) {
				authport = atoi(cf->argv[1]);
				if(authport <= 0) {
					syslog(LLEV,"fwtkcfgerr: invalid authport %s, line %d",cf->argv[1],cf->ln);
					return(1);
				}
			}
		}
	}
	if(authserver == (char *)0) {
		syslog(LLEV,"fwtkcfgerr: no authserver defined! cannot authenticate!");
		return(1);
	}
	if(authport == -1) {
		syslog(LLEV,"fwtkcfgerr: no authport defined! cannot authenticate!");
		return(1);
	}

	rem_authfd = conn_server(authserver,authport,0,(char *)0);
	if(rem_authfd < 0)
		return(-1);
	signal(SIGPIPE,SIG_IGN);
	return(0);
}




auth_close()
{
	if(rem_authfd != -1)
		close(rem_authfd);
	rem_authfd = -1;
	return(0);
}

auth_send(buf)
char	*buf;
{
	int	x;

	if(rem_authfd < 0)
		return(1);

#ifdef	DEBUG
	syslog(LLEV,"auth_send %s\n",buf);
#endif
	x = strlen(buf);
	if(write(rem_authfd,buf,x) != x || write(rem_authfd,"\n",1) != 1)
		return(1);
	return(0);
}


auth_recv(buf,siz)
char	*buf;
int	siz;
{
	int	x = 0;

	if(rem_authfd < 0)
		return(1);

	while(1) {
		if(x + 1 == siz) {
			syslog(LLEV,"fwtksyserr: auth_recv: buffer overflow");
			return(1);
		}
		if(read(rem_authfd,&buf[x],1) != 1) {
			syslog(LLEV,"fwtksyserr: auth_recv: read error: %m");
			return(1);
		}
		if(buf[x] == '\n')
			break;
		x++;
	}
	buf[x] = '\0';

#ifdef	DEBUG
	syslog(LLEV,"auth_recv %s\n",buf);
#endif
	return(0);
}




auth_perm(confp, name, oper, dest, op)
Cfg	*confp;
char	*name;
char	*oper;
char	*dest;
char	*op;
{

	char	sbuf[512];
	int	hadtoopen = 0;

	/* Check to see if the auth database needs to be opened */
	if(rem_authfd < 0) {
		if(auth_open(confp)) {
			return(1);
		}
		hadtoopen = 1;
		if(auth_recv(sbuf,sizeof(sbuf)))
			goto lostconn;
		if(strncmp(sbuf,"Authsrv ready",13)) {
			auth_close();
			return(1);
		}
	}


	/*
	Build the string for authsrv resultant string should look like:
	operation user smith telnet-gw relay.tis.com
	*/
	sbuf[0] = '\0';

	if(op == (char *)0)
		sprintf(sbuf,"operation user %s %s %s", name,oper,dest);
	else
		sprintf(sbuf,"operation user %s %s %s",name,oper,dest,op);

	if(auth_send(sbuf))
		goto lostconn;

	if(auth_recv(sbuf,sizeof(sbuf)))
		goto lostconn;

	if(!strncmp(sbuf,"ok ",3))
		return(0);

	if(!strncmp(sbuf,"Perm",4))
		return(1);
	return(-1);

lostconn:
	if(hadtoopen)
		auth_close();
	return(-1);
}
