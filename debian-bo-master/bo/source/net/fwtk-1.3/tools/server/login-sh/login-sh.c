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
static	char	RcsId[] = "Header: login-sh.c,v 1.1 93/10/20 11:17:17 mjr rel ";


#include	<stdio.h>
#include	<pwd.h>
#include	<ctype.h>
#include	<syslog.h>

extern	char	*index();
extern	char	*rindex();
extern	char	*getpass();

#include	"firewall.h"


Cfg		*confp;

main(ac,av)
int	ac;
char	*av[];
{
	Cfg		*cp;
	FILE		*shelf;
	char		*shf;
	struct	passwd	*pw;
	char		buf[BUFSIZ];
	char		tbuf[BUFSIZ];
	char		*tokav[512];
	int		tokac;
	int		logging = 0;

#ifndef	LOG_DAEMON
	openlog("login-sh",LOG_PID);
#else
	openlog("login-sh",LOG_PID|LOG_NDELAY,LFAC);
#endif

	/* if av[0] = log-login-sh then log */
	if((shf = rindex(av[0],'/')) != (char *)0)
		*shf = '\0';
	else
		shf = av[0];
	if(!strcmp(shf,"log-login-sh"))
		logging = 1;

	if((confp = cfg_read("login-sh")) == (Cfg *)-1) {
		fprintf(stderr,"Cannot read config.\n");
		exit(1);
	}

	if((cp = cfg_get("shellfile",confp)) == (Cfg *)0) {
		fprintf(stderr,"No \"shellfile\" defined.\n");
		exit(1);
	}
	if(cp->argc != 1) {
		fprintf(stderr,"shellfile may only have one parameter, line %d\n",cp->ln);
		exit(1);
	}

	if((pw = getpwuid(getuid())) == (struct passwd *)0) {
		fprintf(stderr,"No password entry for uid %d\n",getuid());
		exit(1);
	}

	/* open shells file */
	shf = cp->argv[0];
	if((shelf = fopen(shf,"r")) == (FILE *)0) {
		perror(shf);
		exit(1);
	}

	/* look for user in shells file */
	while(1) {
		if(fgets(buf,sizeof(buf),shelf) == (char *)0) {
			fprintf(stderr,"%s is not in the shells file\n",pw->pw_name);
			exit(1);
		}

		if(buf[0] == '\n' || buf[0] == '#')
			continue;
		tokac = enargv(buf,tokav,512,tbuf,sizeof(tbuf));
		if(tokac <= 0)
			continue;

		if(!strcmp(pw->pw_name,tokav[0]))
			break;
	}
	fclose(shelf);

	if(tokac < 3) {
		fprintf(stderr,"Not enough shell arguments for %s\n",pw->pw_name);
		exit(1);
	}

	/* found user, now authenticate them */
	if(getauth(pw->pw_name))
		exit(1);

	/* set the SHELL environment variable to the real shell */
	(void)setenv("SHELL",tokav[1],1);

	/* authenticated them OK, now invoke the login shell */
	if(logging)
		syslog(LLEV,"securityalert: authenticate user=%s",pw->pw_name);
	execv(tokav[1],&tokav[2]);
	perror(tokav[1]);
	exit(1);
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
		fprintf(stderr,"%s\n",cbuf);
		auth_close();
		return(1);
	}

	while(bad--) {
		if(strlen(nam) + 100 > sizeof(cbuf))
			sprintf(cbuf,"authorize %s",nam);
		else
			sprintf(cbuf,"authorize %s 'login-sh'",nam);

		if(auth_send(cbuf))
			goto lostconn;
		if(auth_recv(cbuf,sizeof(cbuf)))
			goto lostconn;

		if(!strncmp(cbuf,"challenge ",10)) {
			fprintf(stderr,"%s",&cbuf[10]);
			if(fgets(pbuf,sizeof(pbuf),stdin) == (char *)0)
				return(1);
			if((p = index(pbuf,'\n')) != (char *)0)
				*p = '\0';
			p = pbuf;
		} else
		if(!strncmp(cbuf,"password",8)) {
			p = getpass("Password: ");
		} else {
			fprintf(stderr,"%s\n",cbuf);
			return(1);
		}

		if(strlen(p) > 64) {
			fprintf(stderr,"Response too long\n");
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
			fprintf(stderr,"%s\n",cbuf);
			continue;
		}

		/* go */
		auth_close();
		if(cbuf[2] != '\0')
			fprintf(stderr,"%s\n",&cbuf[2]);
		return(0);
	}
	auth_close();
	fprintf(stderr,"Too many failed logins\n");
	return(1);

lostconn:
	auth_close();
	fprintf(stderr,"Lost connection to auth server\n");
	return(1);
}
