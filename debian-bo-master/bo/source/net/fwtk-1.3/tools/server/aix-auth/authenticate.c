/*-
 * Copyright (c) 1994, Morten Hermanrud, Nordic S&TS, IBM
 * All rights reserved.
 *
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
 */
static	char	RcsId[] = "Header";


#include	<stdio.h>
#include	<ctype.h>
#include	<syslog.h>
#include	"firewall.h"


Cfg		*confp;

main(int argc,char **argv,char **envp) {
	Cfg		*cp;

#ifndef	LOG_DAEMON
	openlog("authenticate",LOG_PID);
#else
	openlog("authenticate",LOG_PID|LOG_NDELAY,LFAC);
#endif


	if((confp = cfg_read("authenticate")) == (Cfg *)-1) {
		syslog(LLEV,"fwtkcfgerr: Cannot read config.\n");
		exit(1);
	}

	if(argc!=2){
		syslog(LLEV,"fwtkcfgerr: Need one argument.\n");
		exit(1);
	}

	/* found user, now authenticate them */
	if(getauth(argv[1])){
		syslog(LLEV,"Authentication failed <%s>\n",argv[1]);
		exit(1);
	}

	/* authenticated OK */
	syslog(LLEV,"User logged in <%s>\n",argv[1]);
	exit(0);
}





getauth(nam)
char	*nam;
{
	char		cbuf[BUFSIZ];
	char		pbuf[512];
	int		bad = 5;
	char		*p;

	/* open connection to auth server */
	if(auth_open(confp)) {
		fprintf(stderr,"Cannot connect to authentication server");
		return(1);
	}

	/* get welcome message from auth server */
	if(auth_recv(cbuf,sizeof(cbuf)))
		goto lostconn;
	if(strncmp(cbuf,"Authsrv ready",13)) {
		syslog(LLEV,"%s\n",cbuf);
		auth_close();
		return(1);
	}

	while(bad--) {
		sprintf(cbuf,"authorize %s",nam);

		if(auth_send(cbuf))
			goto lostconn;
		if(auth_recv(cbuf,sizeof(cbuf)))
			goto lostconn;

		if(!strncmp(cbuf,"challenge ",10)) {
			syslog(LLEV,"%s",&cbuf[10]);
			if(fgets(pbuf,sizeof(pbuf),stdin) == (char *)0)
				return(1);
			if((p = index(pbuf,'\n')) != (char *)0)
				*p = '\0';
			p = pbuf;
		} else {
			p = getpass("Password: ");
		}

		if(strlen(p) > 64) {
			syslog(LLEV,"Response too long\n");
			continue;
		}

		/* send response */
		sprintf(cbuf,"response '%s'",p);
		if(auth_send(cbuf))
			goto lostconn;
		if(auth_recv(cbuf,sizeof(cbuf)))
			goto lostconn;

		/* no-go */
		if(strncmp(cbuf,"ok",2)) {
			syslog(LLEV,"%s\n",cbuf);
			continue;
		}

		/* go */
		auth_close();
		if(cbuf[2] != '\0')
			syslog(LLEV,"%s\n",&cbuf[2]);
		return(0);
	}
	auth_close();
	syslog(LLEV,"Too many failed logins\n");
	return(1);

lostconn:
	auth_close();
	syslog(LLEV,"Lost connection to auth server\n");
	return(1);
}
