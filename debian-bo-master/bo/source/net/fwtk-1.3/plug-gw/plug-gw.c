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
static	char	RcsId[] = "Header: plug-gw.c,v 1.4 94/05/27 14:13:24 mjr rel ";

/* #define	BINDDEBUG */
/* #define	BINDDEBUGPORT 6565 */

#include	<stdio.h>
#include	<syslog.h>
#include	<sys/types.h>
#include	<sys/time.h>
#include	<sys/socket.h>
#include	<netdb.h>
#include	<netinet/in.h>

#include	"firewall.h"

static	char		rhost[512];
static	char		raddr[512];
static	struct timeval	timo;
static	struct timeval	*tp = (struct timeval *)0;
static	time_t		ontime;
static	time_t		offtime;
static	int		portid = -1;


main(ac,av)
int	ac;
char	*av[];
{
	Cfg		*cfp;
	Cfg		*cf;
	char		*srvnam;

#ifndef	LOG_DAEMON
	openlog("plug-gw",LOG_PID);
#else
	openlog("plug-gw",LOG_PID|LOG_NDELAY,LFAC);
#endif

#ifdef	BINDDEBUG
	/*
	useful for debugging. if you define this it will bind the
	port and listen directly (circumventing inetd) so you can
	run the program under a debugger
	*/
	debugbind();
#endif

	time(&ontime);

	/* if called with an argument, it is the connecting service name */
	if(ac > 1)
		srvnam = av[1];
	else
		srvnam = av[0];

	if(isalldigits(srvnam))
		portid = atoi(srvnam);
	else {
		struct servent	*sp;

		if((sp = getservbyname(srvnam,"tcp")) != (struct servent *)0)
			portid = ntohs(sp->s_port);
		else {
			syslog(LLEV,"fwtkcfgerr: cannot decode %s as port",srvnam);
			exit(1);
		}
	}


	if(peername(0,rhost,raddr,sizeof(rhost))) {
		syslog(LLEV,"cannot get remote host: %m");
		exit(1);
	}


	if((cfp = cfg_read("plug-gw")) == (Cfg *)-1)
		exit(1);


	timo.tv_usec = 0;
	if((cf = cfg_get("timeout",cfp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: timeout must have one parameter, line %d",cf->ln);
			exit(1);
		}
		timo.tv_sec = atoi(cf->argv[0]);
		tp = &timo;
	}


	for(cf = cfg_get("port",cfp); cf != (Cfg *)0; cf = cfg_get("port",(Cfg *)0)) {
		int	x;

		if(cf->argc < 3) {
			syslog(LLEV,"fwtkcfgerr: missing parameter, line %d",cf->ln);
			continue;
		}

		/* see if ports match */
		if(strcmp(cf->argv[0],"any")) {
	
			if(portid == -1)
				continue;

			if(isalldigits(cf->argv[0]))
				x = atoi(cf->argv[0]);
			else {
				struct servent	*sp;

				sp = getservbyname(cf->argv[0],"tcp");
				if(sp == (struct servent *)0) {
					syslog(LLEV,"fwtkcfgerr: unknown service %s, line %d",cf->argv[0],cf->ln);
					continue;
				}
				x = ntohs(sp->s_port);
			}
			if(x != portid)
				continue;
		}


		/* if ports match, now see if calling host matches */
		for(x = 1; x < cf->argc - 1; x++) {
			if(!strcmp(cf->argv[x],"-port")) {
				x++;
				break;
			}
			if(!strcmp(cf->argv[x],"-plug-to")) {
				x++;
				break;
			}
			if(!strcmp(cf->argv[x],"-privport"))
				break;
			if(hostmatch(cf->argv[x],raddr))
				acceptrule(portid,cf,cf->argc,cf->argv);
		}
	}

	if(portid == -1)
		syslog(LLEV,"deny host=%s/%s",rhost,raddr);
	else
		syslog(LLEV,"deny host=%s/%s service=%s",rhost,raddr,av[1]);
	exit(1);
}




acceptrule(p,c,ac,av)
int	p;
Cfg	*c;
int	ac;
char	*av[];
{
	fd_set		redy;
	fd_set		iced;
	int		ib = 0;
	int		ob = 0;
	int		x;
	int		srvind = -1;
	int		serfd;
	int		privport = 0;
	char		buf[1024 * 4];


	if(c->flags & PERM_DENY) {
		syslog(LLEV,"deny host=%s/%s",rhost,raddr);
		exit(1);
	}

	for(x = 0; x < ac; x++ )
		if(!strcmp(av[x],"-plug-to")) {
			srvind = x;
			break;
		}

	if(srvind == -1 || srvind == ac - 1) {
		syslog(LLEV,"fwtkcfgerr: no -plug-to server given, line %d",c->ln);
		exit(1);
	}

	for(x = 0; x < srvind; x++ )
		if(!strcmp(av[x],"-privport")) {
			privport = 1;
			break;
		}

	for(x = srvind + 2; x < ac - 1; x++ )
		if(!strcmp(av[x],"-port")) {
			if(isalldigits(av[x + 1]))
				portid = atoi(av[x + 1]);
			else {
				struct servent	*sp;

				sp = getservbyname(av[x + 1],"tcp");
				if(sp == (struct servent *)0) {
					syslog(LLEV,"fwtkcfgerr: unknown service %s, line %d",av[x + 1],c->ln);
					continue;
				}
				portid = ntohs(sp->s_port);
			}
			break;
		}


	if(portid == -1) {
		syslog(LLEV,"fwtkcfgerr: not sure what port to connect to, line %d",c->ln);
		exit(1);
	}

	if((serfd = conn_server(av[srvind + 1],portid,privport,(char *)0)) < 0) {
		syslog(LLEV,"cannot connect to server %s/%d: %m",av[srvind + 1],portid);
		exit(1);
	}

	syslog(LLEV,"connect host=%s/%s destination=%s/%d",rhost,raddr,av[srvind + 1],portid);

	FD_ZERO(&iced);
	FD_SET(0,&iced);
	FD_SET(serfd,&iced);

	while(1) {
		(void)bcopy(&iced,&redy,sizeof(fd_set));

		if(select(serfd + 2,&redy,(fd_set *)0,(fd_set *)0,tp) <= 0) {
			syslog(LLEV,"connection timeout: %d sec",timo.tv_sec);
			break;
		}

		if(FD_ISSET(0,&redy)) {
			if((x = read(0,buf,sizeof(buf))) <= 0)
				break;
			if(write(serfd,buf,x) != x)
				break;
			ob += x;
		}
		if(FD_ISSET(serfd,&redy)) {
			if((x = read(serfd,buf,sizeof(buf))) <= 0)
				break;
			if(write(0,buf,x) != x)
				break;
			ib += x;
		}
	}
	time(&offtime);
	syslog(LLEV,"disconnect host=%s/%s destination=%s/%d in=%d out=%d duration=%d",rhost,raddr,av[srvind + 1],portid,ib,ob,offtime - ontime);
	exit(0);
}



#ifdef	BINDDEBUG
debugbind()
{
	struct	sockaddr_in	mya;
	int	x;
	int	nread;

	if((x = socket(AF_INET,SOCK_STREAM,0)) < 0) {
		perror("socket");
		exit(1);
	}
	mya.sin_family = AF_INET;
	bzero(&mya.sin_addr,sizeof(mya.sin_addr));
	mya.sin_port = htons(BINDDEBUGPORT);
	if(bind(x,(struct sockaddr *)&mya,sizeof(mya))) {
		perror("bind");
		exit(1);
	}
	if(listen(x,1) < 0) {
		perror("listen");
		exit(1);
	}
	if((nread = accept(x,0,0)) < 0) {
		perror("accept");
		exit(1);
	}
	close(0);
	dup(nread);
	close(1);
	dup(nread);
}
#endif
