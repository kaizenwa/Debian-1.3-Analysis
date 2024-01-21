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
static	char	RcsId[] = "Header: rlogin-gw.c,v 1.9 94/11/01 11:56:57 mjr rel ";


#include	<stdio.h>
#include	<syslog.h>
#include	<sys/types.h>
#include	<sys/time.h>
#include	<sys/socket.h>
#include	<sys/signal.h>
#include	<sys/ioctl.h>
#include	<fcntl.h>
#include	<netdb.h>
#include	<netinet/in.h>

#include	"firewall.h"

extern	char	*index();

#define	RLOGINPORT	513


extern	char	*maphostname();


static	int	cmd_quit();
static	int	cmd_help();
static	int	cmd_connect();
static	int	cmd_password();
static	int	cmd_password();
static	int	cmd_xforward();

static	void	trap_sigurg();


typedef	struct	{
	char	*name;
	char	*hmsg;
	int	(*op)();
} RlOp;
static	RlOp	ops[] = {
	"connect",	"   connect hostname",		cmd_connect,
	"open",		0,				cmd_connect,
	"x-gw",		"   x-gw [hostname:display]",	cmd_xforward,
	"help",		"   help/?",			cmd_help,
	"password",	"   password",			cmd_password,
	"quit",		"   quit/exit",			cmd_quit,
	"close",	0,				cmd_quit,
	"exit",		0,				cmd_quit,
	"?",		0,				cmd_help,
	0,		0,				0
};

static	Cfg		*confp;
static	char		**validests = (char **)0;

static	int		authneeded = 0;
static	char		authuser[512];
static	int		changeok = 0;
static	int		gotatflg = 0;
static	char		*xforwarder = (char *)0;
static	int		xgwok = 0;

static	char		rhost[512];
static	char		raddr[512];
static	char		dest[512];
static	char		rusername[512];
static	char		lusername[512];
static	char		termname[512];
static	time_t		ontime;
static	time_t		offtime;
static	struct timeval	timo;
static	struct timeval	*tp = (struct timeval *)0;
static	int		ib = 0;
static	int		ob = 0;
static	int		serfd;
static	int		extendperm = 0;


main(ac,av)
int	ac;
char	*av[];
{
	Cfg		*cf;
	fd_set		redy;
	fd_set		iced;
	int		x;
	char		buf[1024 * 4];
	char		*prompt;
	char		*p;
	static	char	lost_connection[] = "lost connection to server";
	int		sret;

#ifndef	LOG_NDELAY
	openlog("rlogin-gw",LOG_PID);
#else
	openlog("rlogin-gw",LOG_PID|LOG_NDELAY,LFAC);
#endif
	time(&ontime);
	strcpy(authuser,"unauth");


#ifdef	BINDDEBUG
	/*
	useful for debugging. if you define this it will bind the
	port and listen directly (circumventing inetd) so you can
	run the program under a debugger
	*/
	debugbind();
#endif


	if(peername(0,rhost,raddr,sizeof(rhost))) {
		syslog(LLEV,"cannot get remote host: %m");
		exit(1);
	}


	if((confp = cfg_read("rlogin-gw")) == (Cfg *)-1)
		exit(1);


	timo.tv_usec = 0;
	if((cf = cfg_get("timeout",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: timeout must have one parameter, line %d",cf->ln);
			exit(1);
		}
		timo.tv_sec = atoi(cf->argv[0]);
		tp = &timo;
	}


	if((cf = cfg_get("prompt",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: prompt must have one parameter, line %d",cf->ln);
			exit(1);
		}
		prompt = cf->argv[0];
	} else
		prompt = "rlogin-gw-> ";



	if((cf = cfg_get("xforwarder",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: xforwarder must have one parameter, line %d",cf->ln);
			exit(1);
		}
		xforwarder = cf->argv[0];
	}



	if((cf = cfg_get("directory",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: chroot must have one parameter, line %d",cf->ln);
			exit(1);
		}
		chdir("/");
		if(chdir(cf->argv[0])) {
			syslog(LLEV,"fwtksyserr: chdir %s: %m",cf->argv[0]);
			exit(1);
		}
		if(chroot(cf->argv[0])) {
			syslog(LLEV,"fwtksyserr: chroot %s: %m",cf->argv[0]);
			exit(1);
		}
		chdir("/");
	}


#ifdef	SO_KEEPALIVE
	x = 1;
	(void)setsockopt(0,SOL_SOCKET,SO_KEEPALIVE,&x,sizeof(x));
#endif


	/* Here we spoof the client side rlogin protocol */
	read(0,buf,1);
	if(buf[0] != 0)
		exit(1);
	if(getstr(lusername,sizeof(lusername)))
		bombout("local username too long");
	if(getstr(rusername,sizeof(rusername)))
		bombout("remote username too long");
	if(getstr(termname,sizeof(termname)))
		bombout("terminal name too long");
	write(0,"",1);


	/* check and configure permissions */
	if(!oktotalkto()) {
		if((cf = cfg_get("denial-msg",confp)) != (Cfg *)0) {
			if(cf->argc != 1) {
				syslog(LLEV,"fwtkcfgerr: denial-msg must have one parameter, line %d",cf->ln);
				exit(1);
			}
			if(sayfile(1,cf->argv[0])) {
				syslog(LLEV,"fwtksyserr: cannot display denial-msg %s: %m",cf->argv[0]);
				exit(1);
			}
			exit(0);
		} else {
			sprintf(buf,"%s/%s is not authorized to use the rlogin proxy",rhost,raddr);
			exit(say(1,buf));
		}
	}

	/* if present a host name, chop and save username and hostname */
	dest[0] = '\0';
	if((p = index(rusername,'@')) != (char *)0) {
		char	*namp;

		*p++ = '\0';
		if(*p == '\0')
			p = "localhost";
		strncpy(dest,p,sizeof(dest) - 1);
		dest[sizeof(dest) - 1] = '\0';
		gotatflg++;
	}


	/* authentication break */
	if(authneeded && getauth()) {
		syslog(LLEV,"exit host=%s/%s no auth",rhost,raddr);
		exit(1);
	}

	if(dest[0] != '\0') {
		char	*namp;
		if((namp = maphostname(dest)) != (char *)0) {
			char	ebuf[512];

			if(rusername[0] != '\0')
				sprintf(ebuf,"Trying %s@%s...",rusername,namp);
			else
				sprintf(ebuf,"Trying %s...",namp);
			if(say(0,ebuf))
				return(1);
		}
		if((serfd = conn_server(dest,RLOGINPORT,1,buf)) < 0)
			bombout(buf);
	} else {
		/* display a welcome file or message */
		if((cf = cfg_get("welcome-msg",confp)) != (Cfg *)0) {
			if(cf->argc != 1) {
				syslog(LLEV,"fwtkcfgerr: welcome-msg must have one parameter, line %d",cf->ln);
				exit(1);
			}
			if(sayfile(0,cf->argv[0])) {
				syslog(LLEV,"fwtksyserr: cannot display welcome %s: %m",cf->argv[0]);
				exit(1);
			}
		} else {
			char	huf[512];
			char	xuf[512];

			if(gethostname(huf,sizeof(huf)))
				strcpy(huf,"unknown");
			sprintf(xuf,"%s rlogin proxy (Version %s) ready:",huf,FWTK_VERSION_MINOR);
			if(say(0,xuf))
				exit(1);
		}
		while((x = usercmd(prompt)) == 0)
			;
		if(x == 1)
			goto leave;
	}

	/* set up out of band handler */
	signal(SIGURG,trap_sigurg);
	set_oob_notification(serfd);

	if(write(serfd,"",1) != 1)
		bombout(lost_connection);
	x = strlen(lusername) + 1;
	if(write(serfd,lusername,x) != x)
		bombout(lost_connection);
	x = strlen(rusername) + 1;
	if(write(serfd,rusername,x) != x)
		bombout(lost_connection);
	x = strlen(termname) + 1;
	if(write(serfd,termname,x) != x)
		bombout(lost_connection);
	if(read(serfd,buf,1) != 1)		/* read a NULL */
		bombout(lost_connection);


	/* set up FD set */
	FD_ZERO(&iced);
	FD_SET(0,&iced);
	FD_SET(serfd,&iced);


	/* loop and copy */
	while(1) {
		(void)bcopy(&iced,&redy,sizeof(fd_set));

		sret = select(32,&redy,(fd_set *)0,(fd_set *)0,tp);
		if(sret == 0)
			break;
		if(sret < 0)
			continue;

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
			if(write(1,buf,x) != x)
				break;
			ib += x;
		}
	}

leave:
	time(&offtime);
	syslog(LLEV,"exit host=%s/%s dest=%s in=%d out=%d user=%s duration=%d",
		rhost,raddr,dest,ib,ob,authuser,offtime - ontime);
	exit(0);
}



static	void
trap_sigurg()
{
	int	atmark;
	char	buf[512];
	char	c;

	signal(SIGURG,trap_sigurg);
	while(1) {
		if(ioctl(serfd,SIOCATMARK,(char *)&atmark) < 0)
			return;
		if(atmark)
			break;
		read(serfd,buf,sizeof(buf));
	}
	if(recv(serfd,(char *)&c,sizeof(c),MSG_OOB) < 0)
		return;
	send(0,(char *)&c,sizeof(c),MSG_OOB);
}



usercmd(prompt)
char	*prompt;
{
	RlOp		*op;
	static char	badcmd[] = "Command not understood";
	char		buf[1024];
	char		tokbuf[1024];
	char		*tokav[56];
	int		tokac;
	int		l;

	l = strlen(prompt);
	if(write(0,prompt,l) != l)
		return(1);
	if(getline((unsigned char *)buf,sizeof(buf) - 1,prompt,l,1) < 0)
		return(1);
	if(buf[0] == '\0')
		return(0);

	tokac = enargv(buf,tokav,56,tokbuf,sizeof(tokbuf));
	if(tokac < 0) {
		if(sayn(0,badcmd,sizeof(badcmd)))
			return(1);
		return(0);
	}
	if(tokac == 0)
		return(0);

	l = strlen(tokav[0]);
	for(op = ops; op->name != (char *)0; op++) {
		if(!strncasecmp(tokav[0],op->name,l))
			return((*op->op)(tokac,tokav,buf));
	}

	sprintf(buf,"Command \"%s\" not recognized",tokav[0]);
	return(say(0,buf));
}



static	int
cmd_connect(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	static	char	usage[] = "usage: connect hostname";
	char		buf[1024];
	char		*namp;

	if(ac != 2)
		return(sayn(0,usage,sizeof(usage)));

	if(validests != (char **)0) {
		char	**xp;
		int	x;

		for(xp = validests; *xp != (char *)0; xp++) {
			if(**xp == '!' && hostmatch(*xp + 1,av[1])) {
				return(baddest(0,av[1]));
			} else {
				if(hostmatch(*xp,av[1]))
					break;
			}
		}
		if(*xp == (char *)0)
			return(baddest(0,av[1]));
	}

	/* Extended permissions processing goes here */
	if(extendperm) {
		int	msg_int;
		char	mbuf[512];

	 	msg_int = auth_perm(confp,authuser,"rlogin-gw",av[1],(char *)0);
		if(msg_int == 1) {
			sprintf(mbuf,"Permission denied for user %s to connect to %s",authuser,av[1]);
			syslog(LLEV,"deny host=%s/%s connect to %s user=%s",raddr,rhost,av[1],authuser);
			say(0,mbuf);
			return(1); 
		} else {
			if(msg_int == -1) {
				sprintf(mbuf,"No match in netperm-table for %s, to rlogin to %s",authuser,av[1]);
				syslog(LLEV,"deny host=%s/%s connect to %s user=%s",raddr,rhost,av[1],authuser);
				say(0,mbuf);
				return(1);
			}
		}
	}

	if((namp = maphostname(av[1])) != (char *)0) {
		char	ebuf[512];

		syslog(LLEV,"permit host=%s/%s connect to %s",rhost,raddr,namp);
		if(strlen(namp) > 20)
			namp[20] = '\0';
		if(rusername[0] != '\0')
			sprintf(ebuf,"Trying %s@%s...",rusername,namp);
		else
			sprintf(ebuf,"Trying %s...",namp);
		if(say(0,ebuf))
			return(1);
	} else
		syslog(LLEV,"permit host=%s/%s connect to %s",rhost,raddr,av[1]);
	if((serfd = conn_server(av[1],RLOGINPORT,1,buf)) < 0) {
		char	ebuf[512];

		if(strlen(av[1]) > 60)
			av[1][60] = '\0';
		strcpy(ebuf,buf);
		sprintf(buf,"%s: %s",av[1],ebuf);
		return(say(0,buf));
	}

	syslog(LLEV,"connected host=%s/%s to %s",rhost,raddr,av[1]);
	strcpy(dest,av[1]);
	return(2);
}



static	int
cmd_xforward(ac,av,cbuf)   
int	ac;
char	*av[];
char	*cbuf;
{
	Cfg     *confx, *cf;
	char	*fakav[16];
	char	buf[256];
	int	pid;

	if(xforwarder == (char *)0) {
		say(0,"X forwarder not available");
		return(0);
	}

	if(!xgwok) {
		say(0,"X forwarder not permitted");
		return(0);
	}

	fakav[0] = "x-gw";
	fakav[1] = "-from";
	fakav[2] = rhost;
	fakav[3] = "-user";
	fakav[4] = authuser;
	fakav[5] = (char *)0;
	fakav[6] = (char *)0;
	fakav[7] = (char *)0;
	fakav[8] = (char *)0;
	fakav[9] = (char *)0;

	pid = 5;
	if(ac > 1) {
		fakav[5] = "-display";
		fakav[6] = av[1];
		pid = 7;
	}

	if((confx=cfg_read("x-gw")) != (Cfg *)-1 && (cf=cfg_get("timeout",confx)) != (Cfg *)0) {
		fakav[pid++] = "-time";
		fakav[pid] = cf->argv[0];
	}

	if((pid = fork()) < 0) {
		syslog(LLEV, "fwtksyserr: cannot fork x-gw: %m" );
		say(0,"Cannot fork child process: system error");
		return(1);
	}

	if(pid > 0)
		return(0);

	if(execv(xforwarder,fakav) < 0) {
		sprintf(buf,"Can't execute: %s",cbuf);
		say(0,buf);
	}
	exit(0);
}




baddest(fd,dst)
int	fd;
char	*dst;
{
	Cfg	*cf;
	char	buf[BUFSIZ];

	if((cf = cfg_get("denydest-msg",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: denydest-msg must have one parameter, line %d",cf->ln);
			return(1);
		}
		if(sayfile(fd,cf->argv[0])) {
			syslog(LLEV,"fwtksyserr: cannot display denydest-msg %s: %m",cf->argv[0]);
			return(1);
		}
		return(0);
	}
	sprintf(buf,"Not permitted to connect to %s",dst);
	syslog(LLEV,"deny host=%s/%s connect to %s",raddr,rhost,dst);
	return(say(fd,buf));
}




static	int
cmd_quit(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	static char	gby[] = "Disconnecting...";

	(void)sayn(0,gby,sizeof(gby));
	syslog(LLEV,"exit host=%s/%s dest=%s in=%d out=%d",
		rhost,raddr,dest,ib,ob);
	exit(0);
}



static	int
cmd_help(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	Cfg		*cf;
	RlOp		*op;
	static char	hhdr[] = "Valid commands are: (unique abbreviations may be used)";

	if((cf = cfg_get("help-msg",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: help-msg must have one parameter, line %d",cf->ln);
			return(1);
		}
		if(sayfile(0,cf->argv[0])) {
			syslog(LLEV,"fwtksyserr: cannot display help message %s: %m",cf->argv[0]);
			return(1);
		}
		return(0);
	}
	if(sayn(0,hhdr,sizeof(hhdr)))
		return(1);
	for(op = ops; op->name != (char *)0; op++) {
		if(op->hmsg != (char *)0)
			if(say(0,op->hmsg))
				return(1);
	}
	return(0);
}



oktotalkto()
{
	Cfg	*cf;
	int	x;

	cf = cfg_get("hosts",confp);
	while(cf != (Cfg *)0) {
		if(cf->argc < 1)
			goto skip;
		
		for(x = 0; x < cf->argc && cf->argv[x][0] != '-'; x++) {
			if(hostmatch(cf->argv[x],raddr)) {
				if(cf->flags & PERM_DENY) {
					syslog(LLEV,"deny host=%s/%s use of gateway",rhost,raddr);
					return(0);
				}
				syslog(LLEV,"permit host=%s/%s use of gateway",rhost,raddr);
				return(acceptrule(cf) == 0);
			}
		}
skip:
		cf = cfg_get("hosts",(Cfg*)0);
	}
	syslog(LLEV,"deny host=%s/%s use of gateway",rhost,raddr);
	return(0);
}




static	void
accept_setdest(v,c)
char	*v;
int	c;
{
	int	dests = 0;

	if(validests == (char **)0)
		validests = (char **)malloc(sizeof(char *) * 2);
	else {
		for(dests = 0; validests[dests] != (char *)0; dests++)
			;
		validests = (char **)realloc(validests,(dests + 2) * sizeof(char *));
	}
	if(validests == (char **)0)
		return;
	validests[dests] = v;
	validests[dests + 1] = (char *)0;
}




acceptrule(c)
Cfg	*c;
{
	int	x;
	void	(*op)();

	for(x = 1; x < c->argc; x++) {
		if(c->argv[x][0] != '-')
			continue;

		if(!strcmp(c->argv[x],"-auth")) {
			authneeded = 1;
			continue;
		}

		if(!strcmp(c->argv[x],"-passok")) {
			changeok = 1;
			continue;
		}

		if(!strcmp(c->argv[x],"-extnd")) {
			extendperm = 1;
			continue;
		}

		if(!strcmp(c->argv[x],"-xok")) {
			xgwok = 1;
			continue;
		}

		/* options that take parameters and lists */
		op = 0;
		if(!strcmp(c->argv[x],"-dest"))
			op = accept_setdest;
		if(op == 0) {
			syslog(LLEV,"fwtkcfgerr: bad option line %d: %s",c->ln,c->argv[x]);
			exit(1);
		}
		if(++x >= c->argc) {
			syslog(LLEV,"fwtkcfgerr: malformed line %d: missing option",c->ln);
			exit(1);
		}
		if(!strcmp(c->argv[x],"{")) {
			while(1) {
				if(++x >= c->argc) {
					syslog(LLEV,"fwtkcfgerr: malformed line %d: missing option",c->ln);
					exit(1);
				}
				if(!strcmp(c->argv[x],"}"))
					break;
				(*op)(c->argv[x],c->ln);
			}
		} else
			(*op)(c->argv[x],c->ln);
	}
	return(0);
}



say(fd,s)
int	fd;
char	*s;
{
	return(sayn(fd,s,strlen(s)));
}



sayn(fd,s,n)
int	fd;
char	*s;
int	n;
{
	if(write(fd,s,n) != n)
		return(1);
	return(write(fd,"\r\n",2) != 2);
}



/* ugly */
sayfile(fd,fn)
int	fd;
char	*fn;
{
	FILE	*f;
	char	buf[BUFSIZ];
	char	*c;

	if((f = fopen(fn,"r")) == (FILE *)0)
		return(1);
	while(fgets(buf,sizeof(buf),f) != (char *)0) {
		if((c = index(buf,'\n')) != (char *)0)
			*c = '\0';
		if(say(fd,buf)) {
			fclose(f);
			return(1);
		}
	}
	fclose(f);
	return(0);
}



bombout(s)
char	*s;
{
	say(1,s);
	exit(1);
}




static	int
cmd_password(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	static char	nochang[] = "Changing passwords not permitted from this host";
	static char	chprom[] = "Changing passwords";
	static char	prom[] = "Enter Username: ";
	static char	pprom[] = "Old Password: ";
	static char	npprom[] = "New Password: ";
	static char	nrpprom[] = "Repeat New Password: ";
	static char	badnpas[] = "Passwords do not match";
	static char	badfpas[] = "Passwords cannot contain quote characters";
	static char	noauth[] = "Cannot connect to authentication server";
	static char	toobad[] = "Too many failed login attempts";
	static char	toobig[] = "User-ID too long";
	static char	lostit[] = "Lost connection to authentication server";
	char		buf[512];
	char		ubuf[128];
	char		nbuf[128];
	int		bad = 5;
	int		x;

	if(!changeok) {
		if(sayn(0,nochang,sizeof(nochang)))
			return(1);
		return(0);
	}

	if(sayn(0,chprom,sizeof(chprom)))
		return(1);

	/* open connection to auth server */
	if(auth_open(confp)) {
		(void)sayn(0,noauth,sizeof(noauth));
		return(1);
	}

	/* get welcome message from auth server */
	if(auth_recv(buf,sizeof(buf)))
		goto lostconn;
	if(strncmp(buf,"Authsrv ready",13)) {
		(void)say(0,buf);
		auth_close();
		return(1);
	}

	while(bad--) {
		/* prompt for user ID */
		if(write(0,prom,sizeof(prom)) != sizeof(prom))
			return(1);

		/* get user ID from user */
		if(getline((unsigned char *)buf,sizeof(buf) - 1,prom,sizeof(prom),1) < 0)
			return(1);

		/* send user ID to auth server */
		if((x = strlen(buf)) == 0)
			continue;
		if(x > 24) {
			if(sayn(0,toobig,sizeof(toobig)))
				return(1);
			continue;
		}

		strcpy(ubuf,buf);

		if(strlen(rhost) + strlen(raddr) + strlen(buf) + 100 > 512)
			sprintf(cbuf,"authorize %s",buf);
		else
			sprintf(cbuf,"authorize %s 'change-pw %s/%s'",buf,rhost,raddr);
		if(auth_send(cbuf))
			goto lostconn;
		if(auth_recv(buf,sizeof(buf)))
			goto lostconn;
		if(!strncmp(buf,"challenge ",10)) {
			x = strlen(&buf[10]);
			if(write(0,&buf[10],x) != x)
				return(1);
			if(getline((unsigned char *)buf,sizeof(buf) - 1,cbuf,strlen(cbuf),1) < 0)
				return(1);
		} else
		if(!strncmp(buf,"password",8)) {
			if(write(0,pprom,sizeof(pprom)) != sizeof(pprom))
				return(1);
			if(getline((unsigned char *)buf,sizeof(buf) - 1,pprom,sizeof(pprom),0) < 0)
				return(1);
		} else {
			if(say(0,buf))
				return(1);
			continue;
		}
		if(strlen(buf) > 64) {
			if(sayn(0,toobig,sizeof(toobig)))
				return(1);
			continue;
		}

		sprintf(cbuf,"response '%s'",buf);
		if(auth_send(cbuf))
			goto lostconn;
		if(auth_recv(buf,sizeof(buf)))
			goto lostconn;
		if(strncmp(buf,"ok",2)) {
			if(say(0,buf))
				return(1);
			continue;
		}
		if(buf[2] != '\0')
			return(say(0,&buf[2]));

new_password:
		/* get new password */
		if(write(0,npprom,sizeof(npprom)) != sizeof(npprom))
			return(1);
		if(getline((unsigned char *)buf,sizeof(buf) - 1,npprom,sizeof(npprom),0) < 0)
			return(1);
		if(write(0,nrpprom,sizeof(nrpprom)) != sizeof(nrpprom))
			return(1);
		if(getline((unsigned char *)nbuf,sizeof(nbuf) - 1,nrpprom,sizeof(nrpprom),0) < 0)
			return(1);
		if(strcmp(buf,nbuf)) {
			if(sayn(0,badnpas,sizeof(badnpas)))
				return(1);
			goto new_password;
		}
		if(index(nbuf,'"') != (char *)0 || index(nbuf,'\'') != (char *)0) {
			if(sayn(0,badfpas,sizeof(badfpas)))
				return(1);
			goto new_password;
		}
		sprintf(buf,"password \"%s\"",nbuf);
		if(auth_send(buf))
			goto lostconn;
		if(auth_recv(buf,sizeof(buf)))
			goto lostconn;
		auth_close();
		return(say(0,buf));
	}
	auth_close();
	(void)sayn(0,toobad,sizeof(toobad));
	return(1);

lostconn:
	auth_close();
	(void)sayn(0,lostit,sizeof(lostit));
	return(1);
}



getauth()
{
	static char	prom[] = "Username: ";
	static char	pprom[] = "Password: ";
	static char	noauth[] = "Cannot connect to authentication server";
	static char	toobad[] = "Too many failed login attempts";
	static char	toobig[] = "User-ID too long";
	static char	lostit[] = "Lost connection to authentication server";
	static char	loggedin[] = "Login Accepted";
	char		buf[1024];
	char		cbuf[128];
	char		ubuf[128];
	char		usrbuf[128];
	int		bad = 5;
	int		x;

	/* open connection to auth server */
	if(auth_open(confp)) {
		write(0,noauth,sizeof(noauth));
		return(1);
	}

	/* get welcome message from auth server */
	if(auth_recv(buf,sizeof(buf)))
		goto lostconn;
	if(strncmp(buf,"Authsrv ready",13)) {
		(void)say(0,buf);
		auth_close();
		return(1);
	}

	usrbuf[0] = '\0';
	lusername[0] = '\0';
	while(bad--) {
		/* prompt for user ID */
		if(write(0,prom,sizeof(prom)) != sizeof(prom))
			goto close_go;

		/* get user ID from user */
		if(getline((unsigned char *)buf,sizeof(buf) - 1,prom,sizeof(prom),1) < 0)
			goto close_go;

		/* send user ID to auth server */
		if((x = strlen(buf)) == 0)
			continue;
		if(strlen(buf) < sizeof(usrbuf) - 1)
			strcpy(usrbuf,buf);
		if(x > 24) {
			if(sayn(0,toobig,sizeof(toobig)))
				goto close_go;
			continue;
		}

		strcpy(ubuf,buf);
		if(strlen(rhost) + strlen(raddr) + strlen(buf) + 100 > 512)
			sprintf(cbuf,"authorize %s",buf);
		else
			sprintf(cbuf,"authorize %s 'rlogin-gw %s/%s'",buf,rhost,raddr);
		if(auth_send(cbuf))
			goto lostconn;
		if(auth_recv(buf,sizeof(buf)))
			goto lostconn;
		if(!strncmp(buf,"challenge ",10)) {
			x = strlen(&buf[10]);
			if(write(0,&buf[10],x) != x)
				goto close_go;
			if(getline((unsigned char *)buf,sizeof(buf) - 1,cbuf,strlen(cbuf),1) < 0)
				goto close_go;
		} else
		if(!strncmp(buf,"password",8)) {
			if(write(0,pprom,sizeof(pprom)) != sizeof(pprom))
				goto close_go;
			if(getline((unsigned char *)buf,sizeof(buf) - 1,pprom,sizeof(pprom),0) < 0)
				goto close_go;
		} else {
			if(say(0,buf))
				goto close_go;
			continue;
		}
		if(strlen(buf) > 64) {
			if(sayn(0,toobig,sizeof(toobig)))
				goto close_go;
			continue;
		}

		sprintf(cbuf,"response '%s'",buf);
		if(auth_send(cbuf))
			goto lostconn;
		if(auth_recv(buf,sizeof(buf)))
			goto lostconn;
		if(strncmp(buf,"ok",2)) {
			if(say(0,buf))
				goto close_go;
			continue;
		}
		auth_close();
		syslog(LLEV,"authenticate user=%s",ubuf);
		if(buf[2] != '\0')
			return(say(0,&buf[2]));
		strcpy(lusername,usrbuf);
		strcpy(authuser,usrbuf);
		if(!gotatflg)
			strcpy(rusername,usrbuf);
		return(sayn(0,loggedin,sizeof(loggedin)));
	}
	auth_close();
	(void)sayn(0,toobad,sizeof(toobad));
	return(1);

lostconn:
	auth_close();
	(void)sayn(0,lostit,sizeof(lostit));
	return(1);
close_go:
	auth_close();
	return(1);
}



/* rlogin-protocol null-terminated strings */
getstr(buf,cnt)
char *buf;
int cnt;
{
	while(1) {
		if(read(0,buf,1) != 1)
			return(1);
		if(--cnt < 0)
			exit(1);
		if(*buf == '\0')
			break;
		buf++;
	}
	return(0);
}



/* terminal newline/cr terminated strings */
getline(buf,siz,pro,plen,echo)
unsigned char	*buf;
int		siz;
char		*pro;
int		plen;
int		echo;
{
	int		x = 0;
	static int	sawcr = 0;

	while(1) {
		if(read(0,(char *)&buf[x],1) != 1)
			return(-1);

		if(buf[x] == '\0')
			continue;

		/* get \r\n - read a char and throw it away */
		if(buf[x] == '\r') {
			if(write(1,"\r\n",2) != 2)
				return(-1);
			buf[x] = '\0';
			return(x);
		}

		if(buf[x] == '\n') {
			if(sawcr) {
				sawcr = 0;
				continue;
			}
			if(write(1,"\r\n",2) != 2)
				return(-1);
			buf[x] = '\0';
			return(x);
		} else
			sawcr = 0;

		/* control R */
		if(buf[x] == 18) {
			if(write(1,"\r\n",2) != 2)
				return(-1);
			if(write(1,pro,plen) != plen)
				return(-1);
			buf[x] = '\0';
			if(write(1,buf,x) != x)
				return(-1);
			continue;
		}

		/* control U/ control C */
		if(buf[x] == 3 || buf[x] == 21) {
			x = 0;
			buf[x] = '\0';
			return(x);
		}

		/* control D */
		if(buf[x] == 4) {
			(void)write(0,"\r\nEOF\r\n",7);
			return(-1);
		}

		if(buf[x] == 127 || buf[x] == 8) {
			unsigned char	xab[4];
			int		xac;

			/* backspace or bell */
			if(x > 0) {
				xab[0] = 0x08;
				xab[1] = 0x20;
				xab[2] = 0x08;
				xac = 3;
				x--;
			} else {
				xab[0] = 007;
				xac = 1;
			}
			if(write(1,xab,xac) != xac)
				return(-1);
			continue;
		}

		if(!isprint(buf[x])) {
			unsigned char	xab[1];

			xab[0] = 007;
			if(write(1,xab,1) != 1)
				return(-1);
			continue;
		}

		if(echo) {
			if(write(1,(char *)&buf[x],1) != 1)
				return(-1);
		} else {
			if(write(1,"#",1) != 1)
				return(-1);
		}
		if(++x >= siz)
			return(-1);
	}
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
#ifndef	BINDDEBUGPORT
	mya.sin_port = htons(RLOGINPORT);
#else
	mya.sin_port = htons(BINDDEBUGPORT);
#endif
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
