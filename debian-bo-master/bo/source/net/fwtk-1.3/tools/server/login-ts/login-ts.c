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
static	char	RcsId[] = "Header: login-ts.c,v 1.1 93/10/20 11:17:17 mjr rel ";


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
	char		*p;
	struct	passwd	*pw;
	char		buf[BUFSIZ];
	char		tbuf[BUFSIZ];
	char		pbuf[256];
	char		username[128];
	int		useruid;
	int		gotone = 1;
	char		*defdir = (char *)0;
	char		*tokav[512];
	int		tokac;
	int		bad = 5;

#ifndef	LOG_DAEMON
	openlog("login-ts",LOG_PID);
#else
	openlog("login-ts",LOG_PID|LOG_NDELAY,LFAC);
#endif


	if((confp = cfg_read("login-ts")) == (Cfg *)-1) {
		syslog(LLEV,"fwtkcfgerr: Cannot read config.\n");
		cfgexit(1);
	}

	/* passed username on command line login-style */
	if(ac > 1) {
		if(ac > 2 && !strcmp(av[1],"-p"))
			strcpy(username,av[2]);
		else
			strcpy(username,av[1]);
		gotone = 0;
	}


	if((cp = cfg_get("chroot",confp)) != (Cfg *)0) {
		if(cp->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: chroot must have one parameter, line %d\n",cp->ln);
			cfgexit(1);
		}
		defdir = cp->argv[0];
	}

	/* get users file */
	if((cp = cfg_get("userfile",confp)) == (Cfg *)0) {
		syslog(LLEV,"fwtkcfgerr: No \"userfile\" defined.\n");
		cfgexit(1);
	}
	if(cp->argc != 1) {
		syslog(LLEV,"fwtkcfgerr: userfile may only have one parameter, line %d\n",cp->ln);
		cfgexit();
	}

	/* open users file */
	shf = cp->argv[0];
	if((shelf = fopen(shf,"r")) == (FILE *)0) {
		syslog(LLEV,"fwtksyserr: %s: %m",shf);
		cfgexit(1);
	}

	/* open auth server */
	if(auth_open(confp)) {
		syslog(LLEV,"fwtksyserr: Cannot connect to authentication server");
		cfgexit(1);
	}

	/* get welcome message from auth server */
	if(auth_recv(buf,sizeof(buf)))
		goto lostconn;
	if(strncmp(buf,"Authsrv ready",13)) {
		syslog(LLEV,"%s",buf);
		auth_close();
		cfgexit(1);
	}

	if(defdir != (char *)0) {
		if(chdir(defdir)) {
			syslog(LLEV,"fwtksyserr: chdir %s: %m",defdir);
			cfgexit();
		}
		if(chroot(defdir)) {
			syslog(LLEV,"fwtksyserr: chroot %s: %m",defdir);
			cfgexit();
		}
	}

	/* main loop */
	while(bad--) {
		if(gotone) {
			fprintf(stderr,"Username: ");
			fflush(stderr);
			if(fgets(username,sizeof(username),stdin) == (char *)0)
				exit(1);
			if((shf = rindex(username,'\n')) != (char *)0)
				*shf = '\0';
		}
		gotone++;

		sprintf(buf,"authorize %s 'login-ts'",username);
		if(auth_send(buf))
			goto lostconn;
		if(auth_recv(buf,sizeof(buf)))
			goto lostconn;

		if(!strncmp(buf,"challenge ",10)) {
			fprintf(stderr,"%s",&buf[10]);
			if(fgets(pbuf,sizeof(pbuf),stdin) == (char *)0)
				return(1);
			if((shf = index(pbuf,'\n')) != (char *)0)
				*shf = '\0';
			p = pbuf;
		} else
		if(!strncmp(buf,"password",8)) {
			p = getpass("Password: ");
		} else {
			fprintf(stderr,"%s\n",buf);
			continue;
		}

		if(strlen(p) > 64) {
			fprintf(stderr,"Response too long\n");
			continue;
		}

		/* send response */
		sprintf(buf,"response '%s'",p);
		if(auth_send(buf))
			goto lostconn;
		if(auth_recv(buf,sizeof(buf)))
			goto lostconn;

		/* no-go */
		if(strncmp(buf,"ok",2)) {
			fprintf(stderr,"%s\n",buf);
			continue;
		}

		if(buf[2] != '\0')
			fprintf(stderr,"%s\n",&buf[2]);


		/* look for user in shells file */
		fseek(shelf,0,0);
		while((shf = fgets(buf,sizeof(buf),shelf)) != (char *)0) {
			char	ubuf[512];
			char	sbuf[512];

			if(buf[0] == '\n' || buf[0] == '#')
				continue;
			if(skin(buf,ubuf,sizeof(ubuf),sbuf,sizeof(sbuf),&useruid))
				continue;
			if(!strcmp(username,ubuf)) {
				tokac = enargv(sbuf,tokav,512,tbuf,sizeof(tbuf));
				if(tokac < 1)
					goto bounce;
				break;
			}
		}

		if(shf != (char *)0)
			break;

bounce:
		fprintf(stderr,"Invalid username\n");
		syslog(LLEV,"user %s not in user file",username);
	}

	if(bad < 1) {
		auth_close();
		fprintf(stderr,"Too many failed logins\n");
		exit(1);
	}
	
	fclose(shelf);
	auth_close();

	/* set the SHELL environment variable to the real shell */
	(void)setenv("SHELL",tokav[0],1);
	(void)setenv("PATH","/bin:.",1);
	(void)setenv("USER",username,1);
	(void)setenv("HOME","/",1);

	/* fixup the '-sh' crud */
	if(tokac == 1) {
		char	*jp;

		tokav[1] = tokav[0];
		tokav[2] = (char *)0;
		if((jp = rindex(tokav[1],'/')) != (char *)0) {
			if(jp != tokav[1]) {
				char	*mp;

				if((mp = malloc(strlen(jp) + 1)) == (char *)0) {
					perror("malloc");
					exit(1);
				}
				strcpy(mp,jp);
				*mp = '-';
				tokav[1] = mp;
			}
		}
	}

	/* authenticated them OK, now invoke the login shell */
	if(setruid(useruid)) {
		perror("setruid");
		syslog(LLEV,"fwtkcfgerr: setruid: %m");
	}
	if(setuid(useruid)) {
		perror("setuid");
		syslog(LLEV,"fwtkcfgerr: setuid: %m");
	}
	syslog(LLEV,"authenticate user=%s",username);
	execv(tokav[0],&tokav[1]);
	perror("exec");
	syslog(LLEV,"fwtkcfgerr: exec %s: %m",tokav[0]);
	exit(1);

lostconn:
	auth_close();
	fprintf(stderr,"Lost connection to auth server\n");
	exit(1);
}



/* minimalist skin of /etc/passwd */
skin(s,ubuf,usiz,sbuf,ssiz,uidp)
char	*s;
char	*ubuf;
int	usiz;
char	*sbuf;
int	ssiz;
int	*uidp;
{
	char	*p1;
	char	*p2;
	char	*p3;
	char	*p4;
	char	*p5;
	char	*p6;
	char	*p7;
	char	*j;

	p1 = s;
	if((p2 = index(s,':')) == (char *)0)
		return(1);
	*p2++ = '\0';
	if((p3 = index(p2,':')) == (char *)0)
		return(1);
	*p3++ = '\0';
	if((p4 = index(p3,':')) == (char *)0)
		return(1);
	*p4++ = '\0';
	if((p5 = index(p4,':')) == (char *)0)
		return(1);
	*p5++ = '\0';
	if((p6 = index(p5,':')) == (char *)0)
		return(1);
	*p6++ = '\0';
	if((p7 = index(p6,':')) == (char *)0)
		return(1);
	*p7++ = '\0';
	if((j = index(p7,'\n')) == (char *)0)
		return(1);
	*j = '\0';

	if(strlen(p1) > usiz - 1 || strlen(p7) > ssiz - 1)
		return(1);

	strcpy(ubuf,p1);
	strcpy(sbuf,p7);

	if(uidp != (int *)0) {
		if(p3[0] == '\0')
			*uidp = 0;
		else
			*uidp = atoi(p3);
	}
	return(0);
}



cfgexit()
{
	fprintf(stderr,"Service unavailable due to configuration problem.\n");
	fprintf(stderr,"See your systems administrator.\n");
	sleep(2);
	exit(1);
}
