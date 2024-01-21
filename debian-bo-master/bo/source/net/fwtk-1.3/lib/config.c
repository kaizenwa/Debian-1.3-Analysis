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
static	char	RcsId[] = "Header: config.c,v 1.4 94/11/01 11:55:18 mjr rel ";


#include	<stdio.h>
#include	<ctype.h>
#include	<syslog.h>

extern	char	*strtok();
extern	char	*index();

#include	"firewall.h"

static	char	*nullav[] = { (char *)0 } ; 
static	char	outofmemory[] = "fwtksyserr: cannot allocate memory: %m";
static	char	*pfile = PERMFILE;

static	char	fwtk_version_string[] = FWTK_VERSION;


cfg_setpfile(s)
char	*s;
{
	if((pfile = malloc(strlen(s) + 1)) == (char *)0)
		return(1);
	strcpy(pfile,s);
	return(0);
}



Cfg	*
cfg_read(app)
char	*app;
{
	Cfg	*ret = (Cfg *)0;
	Cfg	*endp = (Cfg *)0;
	Cfg	*np;
	FILE	*inf;
	int	lnum = 0;
	char	*p;
	char	*xp;
	int	flag;
	char	*facility;
	char	buf[BUFSIZ];
	char	obuf[BUFSIZ];
	char	*av[256];
	int	ac;

	if((inf = fopen(pfile,"r")) == (FILE *)0) {
		syslog(LLEV,"fwtksyserr: configuring %s, cannot open %s: %m",app,pfile);
		goto bomb;
	}

	while(fgets(buf,sizeof(buf),inf) != (char *)0) {
		lnum++;

		/* comments */
		if(buf[0] == '#')
			continue;

		/* all blank */
		for(xp = buf; *xp == ' ' || *xp == '\t' || *xp == '\n'; xp++)
			;
		if(*xp == '\0')
			continue;


		if((xp = index(buf,':')) == (char *)0) {
			syslog(LLEV,"fwtkcfgerr: %s line %d malformed",pfile,lnum);
			continue;
		}
		/* save pointer to clauses */
		*xp++ = '\0';

		/* check for matching app name or '*' */
		p = strtok(buf," \t\n,");
		while(p != (char *)0) {
			if(!strcmp(p,app) || !strcmp(p,"*"))
				break;
			p = strtok((char *)0," \t\n,:");
		}
		if(p == (char *)0)
			continue;

		if((p = strtok(xp,"\t ")) == (char *)0) {
			syslog(LLEV,"fwtkcfgerr: %s line %d malformed",pfile,lnum);
			continue;
		}

		flag = 0;
		facility = p;
		if(!strncmp(p,"deny-",5)) {
			flag |= PERM_DENY;
			facility = &p[5];
		} else
			if(!strncmp(p,"permit-",7)) {
				flag |= PERM_ALLOW;
				facility = &p[7];
			}

		/* grab the rest of the line and break it into args */
		if((p = strtok((char *)0,"\n")) == (char *)0) {
			syslog(LLEV,"fwtkcfgerr: %s line %d missing parameter",pfile,lnum);
			continue;
		}
		ac = enargv(p,av,sizeof(av) / sizeof(char *),obuf,sizeof(obuf));
		if(ac < 0) {
			syslog(LLEV,"fwtkcfgerr: %s line %d too long or misquoted",pfile,lnum);
			continue;
		}

		/* ok, we actually have something that means something. */
		if((np = (Cfg *)malloc(sizeof(Cfg))) == (Cfg *)0) {
			syslog(LLEV,outofmemory);
			goto bomb;
		}
		np->argc = ac;
		if(ac == 0)
			np->argv = nullav;
		else {
			int	x;

			np->argv = (char **)malloc((ac + 1) * sizeof(char *));
			if(np->argv == (char **)0) {
				syslog(LLEV,outofmemory);
				goto bomb;
			}
			for(x = 0; x < ac; x++) {
				np->argv[x] = malloc(strlen(av[x]) + 1);
				if(np->argv[x] == (char *)0) {
					syslog(LLEV,outofmemory);
					goto bomb;
				}
				strcpy(np->argv[x],av[x]);
			}
			np->argv[ac] = (char *)0;
		}
		np->op = malloc(strlen(facility) + 1);
		if(np->op == (char *)0) {
			syslog(LLEV,outofmemory);
			goto bomb;
		}
		strcpy(np->op,facility);
		np->flags = flag;
		np->ln = lnum;
		np->next = (Cfg *)0;

		if(ret == (Cfg *)0) {
			ret = np;
			endp = ret;
		} else {
			endp->next = np;
			endp = np;
		}
	}
	fclose(inf);
	return(ret);

bomb:
	if(inf != (FILE *)0)
		fclose(inf);
	return((Cfg *)-1);
}



Cfg	*
cfg_get(fac,cp)
char	*fac;
Cfg	*cp;
{
	static	Cfg	*savp = (Cfg *)0;

	if(cp == (Cfg *)0)
		if((cp = savp) == (Cfg *)0)
			return(cp);
	while(cp != (Cfg *)0 && strcmp(fac,cp->op))
		cp = cp->next;
	if(cp != (Cfg *)0)
		savp = cp->next;
	else
		savp = cp;
	return(cp);
}
