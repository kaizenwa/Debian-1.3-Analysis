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
static	char	RcsId[] = "Header: authmgr.c,v 1.4 94/05/27 14:09:02 mjr rel ";


#include	<stdio.h>
#include	<syslog.h>

#include	"firewall.h"
#include	"auth.h"

extern	char	*rindex();
extern	char	*index();
extern	char	*getpass();

#define	FLG_LOCAL	1
typedef	struct {
	char	*cnam;
	int	flg;
	char	*help;
	int	(*cfun)();
} Cmd;

static	int	logged_in = 0;


static	int	do_quit();
static	int	do_login();
static	int	do_passwd();
static	int	do_help();
static	int	do_multiline();
static Cmd ctab[] = {
"login",	FLG_LOCAL,	"login",			do_login,
"adduser",	0,	"adduser username [longname]",		0,
"deluser",	0,		"deluser username",		0,
"display",	FLG_LOCAL,	"display username",		do_multiline,
"list",		FLG_LOCAL,	"list [group]",			do_multiline,
"enable",	0,		"enable username",		0,
"disable",	0,		"disable username",		0,
"group",	0,		"group username group",		0,
"rename",	0,	"rename user newname [longname]",	0,
"proto",	0,		"proto username auth-proto",	0,
"wiz",		0,		"wiz username",			0,
"unwiz",	0,		"unwiz username",		0,
"password",	FLG_LOCAL,	"password [user [pass]]",	do_passwd,
"quit",		FLG_LOCAL,	"quit",				do_quit,
"exit",		FLG_LOCAL,	"exit",				do_quit,
"?",		FLG_LOCAL,	"?",				do_help,
"help",		FLG_LOCAL,	"help",				do_help,
0,		0,		0,				0
};


static	int	stin;	/* terminal is standard input */
static	Cfg	*confp;

main(ac,av)
int	ac;
char	*av[];
{
	Cmd	*cp;
	char	buf[BUFSIZ];
	char	*xa[128];
	char	xb[BUFSIZ];
	int	xc;
	char	*p;
	int	nl;

#ifndef	LOG_NDELAY
	openlog("authmgr",LOG_PID);
#else
	openlog("authmgr",LOG_PID|LOG_NDELAY,LOG_USER);
#endif

	confp = cfg_read("authmgr");

	/* this uses confp to get server/port numbers */
	if(auth_open(confp)) {
		fprintf(stderr,"Cannot open auth service\n");
		exit(1);
	}

	if(auth_recv(buf,sizeof(buf)))
		lostconn();
	if(strncmp(buf,"Authsrv ready",13)) {
		fprintf(stderr,"Cannot connect to server, response: %s\n",buf);
		exit(1);
	}
	fprintf(stderr,"Connected to server\n");

	stin = isatty(fileno(stdin));
	while(1) {
		if(stin) {
			fprintf(stderr,"authmgr-> ");
			fflush(stderr);
		}

		if(fgets(buf,sizeof(buf),stdin) == (char *)0)
			break;
		if((p = rindex(buf,'\n')) != (char *)0)
			*p = '\0';

		xc = enargv(buf,xa,sizeof(xa)/sizeof(char *),xb,sizeof(xb));
		if(xc < 0) {
			fprintf(stderr,"Too many command parameters.\n");
			continue;
		}
		if(xc == 0)
			continue;

		nl = strlen(xa[0]);
		for(cp = ctab; cp->cnam != (char *)0; cp++)
			if(!strncasecmp(cp->cnam,xa[0],nl))
				break;

		if(cp->cnam == (char *)0) {
			printf("Command \"%s\" unrecognized.\n",xa[0]);
			continue;
		}

		if(cp->flg & FLG_LOCAL) {
			(*cp->cfun)(xc,xa,buf);
			continue;
		}

		if(auth_send(buf))
			lostconn();
		if(auth_recv(xb,sizeof(xb)))
			lostconn();
		printf("%s\n",xb);
	}
	auth_close();
}



static	int
do_quit()
{
	auth_close();
	exit(0);
}


static	int
do_help()
{
	Cmd	*cp;

	printf("Command List:\n");
	printf("(Commands may be invoked with unique leading abbreviation)\n");
	for(cp = ctab; cp->cnam != (char *)0; cp++)
		printf(" %s\n",cp->help);
	return(0);
}


static	int
do_login(ac,av,original)
int	ac;
char	*av[];
char	*original;
{
	char		usrbuf[512];
	char		rbuf[512];
	char		pbuf[512];
	char		*p;

	logged_in = 0;
	if(ac > 1)
		strcpy(usrbuf,av[1]);
	else {
		fprintf(stderr,"Username: ");
		fflush(stderr);
		if(fgets(usrbuf,sizeof(usrbuf),stdin) == (char *)0)
			do_quit();
	}
	sprintf(rbuf,"authorize %s",usrbuf);
	if(auth_send(rbuf))
		lostconn();
	if(auth_recv(rbuf,sizeof(rbuf)))
		lostconn();
	if(!strncmp(rbuf,"challenge ",10)) {
		fprintf(stderr,"%s",&rbuf[10]);
		gets(pbuf);
		p = pbuf;
	} else
	if(!strncmp(rbuf,"password",8)) {
		p = getpass("Password: ");
	} else {
		fprintf(stderr,"%s\n",rbuf);
		return(1);
	}
	if(p == (char *)0)
		return(1);
	sprintf(rbuf,"response '%s'",p);
	if(auth_send(rbuf))
		lostconn();
	if(auth_recv(rbuf,sizeof(rbuf)))
		lostconn();
	if(strncmp(rbuf,"ok",2)) {
		fprintf(stderr,"%s\n",rbuf);
		return(1);
	}
	if(stin) {
		if(rbuf[2] != '\0')
			fprintf(stderr,"Logged in %s\n",&rbuf[2]);
		else
			fprintf(stderr,"Logged in\n");
	}
	logged_in = 1;
	return(0);
}


static	int
do_passwd(ac,av,original)
int	ac;
char	*av[];
char	*original;
{
	char		rbuf[512];
	char		*p;
	char		*pwp1;
	char		*pwp2;
	char		pbuf[128];
	char		pxuf1[128];
	char		pxuf2[128];

	if(!logged_in) {
		fprintf(stderr,"Log in, first\n");
		return(0);
	}

	pwp1 = "Password:";
	pwp2 = "Repeat Password:";
	if(ac > 1) {
		sprintf(pxuf1,"Password for %s:",av[1]);
		sprintf(pxuf2,"Repeat Password for %s:",av[1]);
		pwp1 = pxuf1;
		pwp2 = pxuf2;
	}
	if(ac > 2)
		strcpy(pbuf,av[2]);
	else {
		if((p = getpass(pwp1)) == (char *)0) {
			fprintf(stderr,"Password not changed\n");
			return(0);
		}
		strcpy(pbuf,p);
		if((p = getpass(pwp2)) == (char *)0) {
			fprintf(stderr,"Password not changed\n");
			return(0);
		}
		if(strcmp(p,pbuf)) {
			fprintf(stderr,"Passwords differ - not changed\n");
			return(0);
		}
	}

	if(index(pbuf,'"') != (char *)0 || index(pbuf,'\'') != (char *)0) {
		fprintf(stderr,"Passwords cannot contain quotes - not changed\n");
		return(0);
	}

	if(ac > 1)
		sprintf(rbuf,"password %s \"%s\"",av[1],pbuf);
	else
		sprintf(rbuf,"password \"%s\"",pbuf);

	if(auth_send(rbuf))
		lostconn();
	if(auth_recv(rbuf,sizeof(rbuf)))
		lostconn();
	fprintf(stderr,"%s\n",rbuf);
	return(0);
}


/* send a whole line over intact and expect back a multiline response */
static	int
do_multiline(ac,av,original)
int	ac;
char	*av[];
char	*original;
{
	char		rbuf[1024];

	if(auth_send(original))
		lostconn();
	while(1) {
		if(auth_recv(rbuf,sizeof(rbuf)))
			lostconn();
		if(rbuf[0] == '.' && rbuf[1] == '\0')
			break;
		if(rbuf[0] == '.')
			fprintf(stderr,"%s\n",&rbuf[1]);
		else
			fprintf(stderr,"%s\n",rbuf);
	}
	return(0);
}



lostconn()
{
	fprintf(stderr,"Lost connection to auth service\n");
	auth_close();
	exit(1);
}
