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
static	char	RcsId[] = "Header: authdump.c,v 1.1 93/10/20 10:54:20 mjr rel ";
#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/file.h>
#include	<ndbm.h>
#include	<syslog.h>

#include	"firewall.h"
#include	"auth.h"

static	DBM	*dbfile = (DBM *)0;
static	int	lockfd = -1;
static	Cfg	*confp;
static	int	tin;

main(ac,av)
int	ac;
char	*av[];
{
	Auth	ab;
	char	u[512];
	int	recs = 0;

	tin  = (isatty(2) && !isatty(1));

	if((confp = cfg_read("authsrv")) == (Cfg *)-1) {
		if(tin)
			fprintf(stderr,"Warning: cannot read configuration file\n");
		confp = (Cfg *)0;
	}

	if(auth_dbconfig(confp) || auth_dbopen()) {
		fprintf(stderr,"Cannot open auth database!\n");
		exit(1);
	}
	if(auth_dbtraversestart(u,&ab) == 0) {
		dumpauth(u,&ab);
		recs++;
		if(tin)
			fprintf(stderr,".");
		while(auth_dbtraversenext(u,&ab) == 0) {
			dumpauth(u,&ab);
			if(tin)
				fprintf(stderr,".");
			recs++;
		}
	}
	auth_dbclose();
	if(tin)
		fprintf(stderr,"\n%d records dumped\n",recs);
	exit(0);
}

dumpauth(user,a)
char	*user;
Auth	*a;
{
	printf("\nuser=%s\n",user);
	printf("longname=%s\n",a->ln);
	printf("group=%s\n",a->gp);
	printf("pass=%s\n",a->pw);
	printf("flags=%o\n",a->flgs);
	printf("bad_count=%d\n",a->bcnt);
	printf("proto=%c\n",a->atyp);
	printf("last=%d\n",a->last);
}
