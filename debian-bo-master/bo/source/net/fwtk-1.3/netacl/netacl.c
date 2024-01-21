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
static	char	RcsId[] = "Header: netacl.c,v 1.5 94/11/01 11:56:09 mjr rel ";


#include	<syslog.h>
#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/file.h>
#include	<pwd.h>
#include	<time.h>

#include	"firewall.h"

static	int	acceptrule();

static	char	rhost[512];
static	char	raddr[512];
static	char	*service;

main(ac,av)
int	ac;
char	*av[];
{
	Cfg	*cfp;
	Cfg	*cf;
	char	nbuf[512];

#ifndef	LOG_DAEMON
	openlog("netacl",LOG_PID);
#else
	openlog("netacl",LOG_PID|LOG_NDELAY,LFAC);
#endif

	if(ac == 1)
		service = av[0];
	else
		service = av[1];

	if(strlen(service) + 9 > sizeof(nbuf)) {
		syslog(LLEV,"fwtkcfgerr: name of av[](%s) too long",service);
		exit(1);
	}

	strcpy(nbuf,"netacl-");
	strcat(nbuf,service);

	if((cfp = cfg_read(nbuf)) == (Cfg *)-1) {
		syslog(LLEV,"fwtkcfgerr: %s exiting - cannot read configuration",service);
		exit(1);
	}

	if(peername(0,rhost,raddr,sizeof(rhost))) {
		syslog(LLEV,"fwtksyserr: %s exiting - cannot get peername",service);
		exit(1);
	}


	/* get list of permitted/denied hosts */
	for(cf = cfg_get("hosts",cfp); cf != (Cfg *)0; cf = cfg_get("hosts",(Cfg *)0)) {
		int	x;

		for(x = 0; x < cf->argc; x++) {

			/*
			if we hit a value with a leading '-' we are
			past processing the host names
			*/
			if(!strcmp(cf->argv[x],"-exec"))
				break;
			if(!strcmp(cf->argv[x],"-user"))
				break;
			if(!strcmp(cf->argv[x],"-chroot"))
				break;

			if(hostmatch(cf->argv[x],raddr))
				acceptrule(service,cf,cf->argc,cf->argv);
		}
	}

	syslog(LLEV,"deny host=%s/%s service=%s",rhost,raddr,service);
	exit(1);
}


static	int
acceptrule(srv,c,ac,av)
char	*srv;
Cfg	*c;
int	ac;
char	**av;
{
	int	avbase = 0;
	char	*chrdir = (char *)0;
	int	runuid = -1;
	int	rungid = -1;
	int	xecind = -1;
	int	x;

	if(c->flags & PERM_DENY) {
		syslog(LLEV,"deny host=%s/%s service=%s",rhost,raddr,srv);
		exit(1);
	}

	for(x = 0; x < ac; x++)
		if(!strcmp(av[x],"-exec")) {
			xecind = x;
			break;
		}

	if(xecind < 0) {
		syslog(LLEV,"fwtkcfgerr: no exec clause in rule line %d",c->ln);
		exit(1);
	}

	for(x = 0; x < xecind; x++)
		if(!strcmp(av[x],"-user")) {
			if((runuid  = mapuid(av[x+1])) == -1) {
				syslog(LLEV,"fwtkcfgerr: cannot map uid %s",av[x+1]);
				exit(1);
			}
			break;
		}

	for(x = 0; x < xecind; x++)
		if(!strcmp(av[x],"-group")) {
			if((rungid  = mapgid(av[x+1])) == -1) {
				syslog(LLEV,"fwtkcfgerr: cannot map gid %s",av[x+1]);
				exit(1);
			}
			break;
		}

	for(x = 0; x < xecind; x++)
		if(!strcmp(av[x],"-chroot")) {
			chrdir = av[x+1];
			break;
		}


	if(chrdir != (char *)0) {
		chdir("/");
		if((chdir(chrdir) != 0 || chroot(chrdir) != 0)) {
			syslog(LLEV,"fwtksyserr: cannot chroot %s: %m",chrdir);
			exit(1);
		}
		chdir("/");
	}

	if(rungid != -1 && setuid(rungid)) {
		syslog(LLEV,"fwtksyserr: cannot setgid %d: %m",rungid);
		exit(1);
	}

	if(runuid != -1 && setuid(runuid)) {
		syslog(LLEV,"fwtksyserr: cannot setuid %d: %m",runuid);
		exit(1);
	}

	syslog(LLEV,"permit host=%s/%s service=%s execute=%s",rhost,raddr,service,av[xecind + 1]);
	execv(av[xecind + 1],&av[xecind + 1]);
	syslog(LLEV,"fwtksyserr: exec: %s : %m",av[xecind + 1]);
	exit(1);
}
