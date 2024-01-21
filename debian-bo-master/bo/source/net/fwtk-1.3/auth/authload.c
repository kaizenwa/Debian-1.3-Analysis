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
static	char	RcsId[] = "Header: authload.c,v 1.1 93/10/20 10:54:22 mjr rel ";
#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/file.h>
#include	<ndbm.h>
#include	<syslog.h>

#include	"firewall.h"
#include	"auth.h"

static	int	ln = 0;
static	Cfg	*confp;
static	int	tin;

main(ac,av)
int	ac;
char	*av[];
{
	Auth	ab;
	char	user[128];
	int	recs = 0;

	tin = isatty(2);
	if((confp = cfg_read("authsrv")) == (Cfg *)-1) {
		if(tin)
			fprintf(stderr,"Warning: cannot read configuration file\n");
		confp = (Cfg *)0;
	}

	if(auth_dbconfig(confp) || auth_dbopen()) {
		fprintf(stderr,"Cannot open auth database\n");
		exit(1);
	}

	while(getarec(user,&ab)) {
		if(auth_dbputu(user,&ab)) {
			fprintf(stderr,"Cannot write record\n");
			exit(1);
		}
		recs++;
		if(tin)
			fprintf(stderr,".");
	}

	auth_dbclose();
	if(tin)
		fprintf(stderr,"\n%d records loaded\n",recs);
	exit(0);
}


static	char *
nlgets(b,bs,fd)
char	*b;
int	bs;
FILE	*fd;
{
	char		*p;
	extern	char	*rindex();

	if(fgets(b,bs,fd) == (char *)0)
		return((char *)0);
	if((p = rindex(b,'\n')) != (char *)0)
		*p = '\0';
	return(b);
}


getarec(u,a)
char	*u;
Auth	*a;
{
	char	buf[512];

	/* leading newline */
	if(nlgets(buf,sizeof(buf),stdin) == (char *)0)
		return(0);
	ln++;
	if(buf[0] != '\0') {
		fprintf(stderr,"bad format: missing blank line, line %d\n",ln);
		return(0);
	}


	/* user */
	if(nlgets(buf,sizeof(buf),stdin) == (char *)0)
		return(0);
	ln++;
	if(strncmp(buf,"user=",5)) {
		fprintf(stderr,"bad format: missing user=, line %d\n",ln);
		return(0);
	}
	strcpy(u,&buf[5]);


	/* longname */
	if(nlgets(buf,sizeof(buf),stdin) == (char *)0)
		return(0);
	ln++;
	if(strncmp(buf,"longname=",9)) {
		fprintf(stderr,"bad format: missing longname=, line %d\n",ln);
		return(0);
	}
	strcpy(a->ln,&buf[9]);



	/* group */
	if(nlgets(buf,sizeof(buf),stdin) == (char *)0)
		return(0);
	ln++;
	if(strncmp(buf,"group=",6)) {
		fprintf(stderr,"bad format: missing group=, line %d\n",ln);
		return(0);
	}
	strcpy(a->gp,&buf[6]);


	/* password */
	if(nlgets(buf,sizeof(buf),stdin) == (char *)0)
		return(0);
	ln++;
	if(strncmp(buf,"pass=",5)) {
		fprintf(stderr,"bad format: missing pass=, line %d\n",ln);
		return(0);
	}
	strcpy(a->pw,&buf[5]);


	/* flags */
	if(nlgets(buf,sizeof(buf),stdin) == (char *)0)
		return(0);
	ln++;
	if(strncmp(buf,"flags=",6)) {
		fprintf(stderr,"bad format: missing flags=, line %d\n",ln);
		return(0);
	}
	sscanf(&buf[6],"%o",&(a->flgs));


	/* bcnt */
	if(nlgets(buf,sizeof(buf),stdin) == (char *)0)
		return(0);
	ln++;
	if(strncmp(buf,"bad_count=",10)) {
		fprintf(stderr,"bad format: missing bad_count=, line %d\n",ln);
		return(0);
	}
	sscanf(&buf[10],"%d",&(a->bcnt));


	/* proto */
	if(nlgets(buf,sizeof(buf),stdin) == (char *)0)
		return(0);
	ln++;
	if(strncmp(buf,"proto=",6)) {
		fprintf(stderr,"bad format: missing proto=, line %d\n",ln);
		return(0);
	}
	a->atyp = buf[6];


	/* last */
	if(nlgets(buf,sizeof(buf),stdin) == (char *)0)
		return(0);
	ln++;
	if(strncmp(buf,"last=",5)) {
		fprintf(stderr,"bad format: missing last=, line %d\n",ln);
		return(0);
	}
	a->last = atol(&buf[5]);
	return(1);
}
