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
static	char	RcsId[] = "Header: ftp-gw.c,v 1.10 94/11/01 11:54:59 mjr rel ";


#include	<stdio.h>
#include	<ctype.h>
#include	<syslog.h>
#include	<sys/signal.h>
#include	<sys/ioctl.h>
#include	<sys/errno.h>
extern	int	errno;
extern	char	*sys_errlist[];
#include	<arpa/ftp.h>
#include	<arpa/telnet.h>
#include	<sys/time.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>

extern	char	*rindex();
extern	char	*index();
extern	char	*strtok();
extern	char	*inet_ntoa();

extern	char	*optarg;

#include	"firewall.h"


#ifndef	BSIZ
#define	BSIZ	2048
#endif

#ifndef	FTPPORT
#define	FTPPORT	21
#endif

static	Cfg			*confp;
static	char			**validests = (char **)0;
static	int			blockinput = 0;
static	int			blockoutput = 0;

static	int			rfd = -1;	/* fd to remote */
static	int			boundport = -1;
static	int			outgoing = -1;	/* fd for outgoing PORT data */
static	int			incoming = -1;	/* fd for outgoing PORT data */
static	struct sockaddr_in	clntport;
static	char			**saveresp = (char **)0;
static	int			saveresps = 0;
static	char			riaddr[512];
static	char			rladdr[512];

static	int			authenticated = 0;
static	int			authallflg = 0;
static	int			extendperm = 0;
static	int			authopened = 0;
static	char			authuser[128];
static	char			autheduser[128];	/* successful */

static	time_t			ontime;
static	time_t			offtime;
static	int			inbytcnt = 0;
static	int			outbytcnt = 0;
static	int			cmdcnt = 0;


static	int	cmd_user();
static	int	cmd_authorize();
static	int	cmd_response();
static	int	cmd_port();
static	int	cmd_quit();
static	int	cmd_help();
static	int	cmd_noop();
static	int	cmd_abor();
static	int	cmd_passthru();
static	void	saveline();
static	void	flushsaved();
static	void	trap_sigurg();

#define	OP_CONN	001	/* only valid if connected */
#define	OP_WCON	002	/* writethrough if connected */
#define	OP_DENY	004	/* deny op */
#define	OP_LOG	010	/* log op */
#define	OP_AUTH	020	/* must be authenticated */
#define	OP_AOK	040	/* never needs to be authenticated */

typedef	struct	{
	char	*name;
	int	flg;
	int	(*op)();
} FtpOp;
static	FtpOp	ops[] = {
	"port",		OP_CONN,			cmd_port,
	"user",		OP_AOK|OP_WCON,			cmd_user,
	"retr",		OP_CONN,			0,
	"stor",		OP_CONN,			0,
	"pass",		OP_AOK|OP_WCON,			cmd_response,
	"cwd",		OP_CONN,			0,
	"pwd",		OP_CONN,			0,
	"list",		OP_CONN,			0,
	"nlst",		OP_CONN,			0,
	"site",		OP_CONN,			0,
	"cdup",		OP_CONN,			0,
	"smnt",		OP_CONN,			0,
	"help",		OP_WCON|OP_AOK,			cmd_help,
	"noop",		OP_WCON,			cmd_noop,
	"stou",		OP_CONN,			0,
	"appe",		OP_CONN,			0,
	"allo",		OP_CONN,			0,
	"rest",		OP_CONN,			0,
	"rmd",		OP_CONN,			0,
	"mkd",		OP_CONN,			0,
	"pwd",		OP_CONN,			0,
	"syst",		OP_CONN,			0,
	"acct",		OP_CONN,			0,
	"smnt",		OP_CONN,			0,
	"quit",		OP_AOK,				cmd_quit,
	"authorize",	OP_AOK,				cmd_authorize,
	"auth",		OP_AOK,				cmd_authorize,
	"response",	OP_AOK,				cmd_response,
	"resp",		OP_AOK,				cmd_response,
	"rein",		OP_CONN,			0,
	"pasv",		OP_CONN,			0,
	"type",		OP_CONN,			0,
	"stru",		OP_CONN,			0,
	"mode",		OP_CONN,			0,
	"rnfr",		OP_CONN,			0,
	"rnto",		OP_CONN,			0,
	"abor",		OP_CONN,			cmd_abor,
	"stat",		OP_CONN,	/* overload */	cmd_abor,
	"dele",		OP_CONN,			0,
	"rmd",		OP_CONN,			0,
	"mkd",		OP_CONN,			0,
	/* compatibility with DEC version */
	"passerve",	OP_WCON,			cmd_passthru,
	/* alias the connect command */
	"connect",	OP_WCON,	/* overload */	cmd_passthru,
	"conn",		OP_WCON,	/* overload */	cmd_passthru,
	0,		0,				0
};


main(ac,av)
int	ac;
char	*av[];
{
	Cfg		*cf;
	int		timeout;
	struct timeval	timo;
	fd_set		rdy;
	int		x;
	int		runuid = -1;
	int		rungid = -1;
	char		xuf[1024];
	char		huf[128];
	char		*passuser = (char *)0;	/* passed user as av */

#ifndef	LOG_DAEMON
	openlog("ftp-gw",LOG_PID);
#else
	openlog("ftp-gw",LOG_PID|LOG_NDELAY,LFAC);
#endif
	time(&ontime);
	signal(SIGURG,trap_sigurg);

	if((confp = cfg_read("ftp-gw")) == (Cfg *)-1)
		exit(1);

	authuser[0] = '\0';
	strcpy(autheduser,"unauth");

	/* yes, it now takes command line options */
	while((x = getopt(ac,av,"u:a:")) != -1) {
		switch(x) {
		case 'u':
			passuser = optarg;
			break;
		case 'a':
			strcpy(autheduser,optarg);
			authenticated = 1;
			break;
		default:
			syslog(LLEV,"fwtkcfgerr: bad command line option -%c",x);
			exit(1);
		}
	}
	
#ifdef	BINDDEBUG
	/*
	useful for debugging. if you define this it will bind the
	port and listen directly (circumventing inetd) so you can
	run the program under a debugger
	*/
	debugbind();
#endif

	if(peername(0,rladdr,riaddr,sizeof(riaddr))) {
		syslog(LLEV,"fwtksyserr: cannot get peer name");
		exit(1);
	}


	if((cf = cfg_get("groupid",confp)) != (Cfg *)0) {

		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: groupid must have one parameter, line %d",cf->ln);
			exit(1);
		}
		if((rungid = mapgid(cf->argv[0])) == -1) {
			syslog(LLEV,"fwtkcfgerr: cannot map %s to gid",cf->argv[0]);
			exit(1);
		}
	}

	if((cf = cfg_get("userid",confp)) != (Cfg *)0) {

		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: userid must have one parameter, line %d",cf->ln);
			exit(1);
		}
		if((runuid = mapuid(cf->argv[0])) == -1) {
			syslog(LLEV,"fwtkcfgerr: cannot map %s to uid",cf->argv[0]);
			exit(1);
		}
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

	if(rungid != -1 && setgid(rungid)) {
		syslog(LLEV,"fwtksyserr: cannot setgid %d: %m",rungid);
		exit(1);
	}
	if(runuid != -1 && setuid(runuid)) {
		syslog(LLEV,"fwtksyserr: cannot setuid %d: %m",runuid);
		exit(1);
	}

#ifdef	SO_KEEPALIVE
	x = 1;
	(void)setsockopt(0,SOL_SOCKET,SO_KEEPALIVE,&x,sizeof(x));
#endif


	/* see if this is someone we are even willing to converse with */
	if(!oktotalkto()) {
		if((cf = cfg_get("denial-msg",confp)) != (Cfg *)0) {
			if(cf->argc != 1) {
				syslog(LLEV,"fwtkcfgerr: denial-msg must have one parameter, line %d",cf->ln);
				exit(1);
			}
			if(sayfile(0,cf->argv[0],500)) {
				syslog(LLEV,"fwtksyserr: cannot display denial-msg %s: %m",cf->argv[0]);
				exit(1);
			}
			exit(0);
		} else {
			sprintf(xuf,"500 %s/%s not authorized to use FTP proxy",rladdr,riaddr);
			exit(say(0,xuf));
		}
	}



	if((cf = cfg_get("timeout",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: timeout must have one parameter, line %d",cf->ln);
			exit(1);
		}
		if((timeout = atoi(cf->argv[0])) <= 0) {
			syslog(LLEV,"fwtkcfgerr: timeout %s invalid, line %d",cf->argv[0],cf->ln);
			exit(1);
		}
	} else
		timeout = 60*60;


	/* display a welcome file or message */
	if(passuser == (char *)0) {
		if((cf = cfg_get("welcome-msg",confp)) != (Cfg *)0) {
			if(cf->argc != 1) {
				syslog(LLEV,"fwtkcfgerr: welcome-msg must have one parameter, line %d",cf->ln);
				exit(1);
			}
			if(sayfile(0,cf->argv[0],220)) {
				syslog(LLEV,"fwtksyserr: cannot display welcome %s: %m",cf->argv[0]);
				exit(1);
			}
		} else {
			if(gethostname(huf,sizeof(huf)))
				strcpy(huf,"unknown");
			if(authallflg)
				if(say(0,"220-Proxy first requires authentication"))
					exit(1);
			sprintf(xuf,"220 %s FTP proxy (Version %s) ready.",huf,FWTK_VERSION_MINOR);
			if(say(0,xuf))
				exit(1);
		}
	} else {
		char	*fakav[3];
	
		fakav[0] = "user";
		fakav[1] = passuser;
		fakav[2] = (char *)0;

		if(authallflg && !authenticated) {
			if(rindex(fakav[1],'@') != (char *)0 || index(fakav[1],'!') != (char *)0) {
				if(say(0,"551 Proxy first requires authentication"))
					exit(1);
			}
		} else
			if(cmd_user(2,fakav,"user internal"))
				exit(1);
	}

	/* main loop */
	while(1) {
		FD_ZERO(&rdy);
		FD_SET(0,&rdy);
		if(rfd != -1)
			FD_SET(rfd,&rdy);
		if(boundport != -1)
			FD_SET(boundport,&rdy);
		if(outgoing != -1)
			FD_SET(outgoing,&rdy);
		if(incoming != -1)
			FD_SET(incoming,&rdy);



		timo.tv_sec = timeout;
		timo.tv_usec = 0;
		if((x = select(32,&rdy,(fd_set *)0,(fd_set *)0,&timo)) < 0) {
			syslog(LLEV,"fwtksyserr: select: %m");
			exit(1);
		}
		if(x == 0)
			break;


		if(FD_ISSET(0,&rdy))
			if(usercmd())
				break;


		if(rfd != -1 && FD_ISSET(rfd,&rdy))
			if(peerreply())
				break;


		if(boundport != -1 && FD_ISSET(boundport,&rdy))
			if(callback())
				break;


		if(outgoing != -1 && FD_ISSET(outgoing,&rdy))
			if(copyout())
				break;


		if(incoming != -1 && FD_ISSET(incoming,&rdy))
			if(copyin())
				break;
	}
	time(&offtime);
	syslog(LLEV,"exit host=%s/%s cmds=%d in=%d out=%d user=%s duration=%d",
		rladdr,riaddr,cmdcnt,inbytcnt,outbytcnt,autheduser,offtime - ontime);
	exit(0);
}




/* read it and weep */
static	void
trap_sigurg()
{
	int	atmark;
	char	buf[512];

	signal(SIGURG,trap_sigurg);
	while(1) {
		if(ioctl(0,SIOCATMARK,(char *)&atmark) < 0)
			return;
		if(atmark)
			break;
		read(0,buf,sizeof(buf));
	}
}




oktotalkto()
{
	Cfg	*cf;
	int	x;

	cf = cfg_get("hosts",confp);
	while(cf != (Cfg *)0) {
		if(cf->argc < 1)
			goto skip;

		for(x = 0; x < cf->argc; x++) {
			if(cf->argv[x][0] == '-')
				break;
			if(hostmatch(cf->argv[x],riaddr)) {
				if(cf->flags & PERM_DENY) {
					syslog(LLEV,"deny host=%s/%s use of gateway",rladdr,riaddr);
					return(0);
				}
				syslog(LLEV,"permit host=%s/%s use of gateway",rladdr,riaddr);
				return(acceptrule(cf) == 0);
			}
		}

skip:
		cf = cfg_get("hosts",(Cfg*)0);
	}
	syslog(LLEV,"deny host=%s/%s use of gateway",rladdr,riaddr);
	return(0);
}



static	void
accept_setdeny(v,c)
char	*v;
int	c;
{
	FtpOp	*op;

	for(op = ops; op->name != (char *)0; op++)
		if(!strcasecmp(v,op->name)) {
			op->flg |= OP_DENY;
			return;
		}
	if(op->name == (char *)0)
		syslog(LLEV,"fwtkcfgerr: config line %d: bad operand %s",c,v);
}



static	void
accept_setauth(v,c)
char	*v;
int	c;
{
	FtpOp	*op;

	for(op = ops; op->name != (char *)0; op++)
		if(!strcasecmp(v,op->name)) {
			op->flg |= OP_AUTH;
			return;
		}
	if(op->name == (char *)0)
		syslog(LLEV,"fwtkcfgerr: config line %d: bad operand %s",c,v);
}



static	void
accept_setlog(v,c)
char	*v;
int	c;
{
	FtpOp	*op;

	for(op = ops; op->name != (char *)0; op++)
		if(!strcasecmp(v,op->name)) {
			op->flg |= OP_LOG;
			return;
		}
	if(op->name == (char *)0)
		syslog(LLEV,"fwtkcfgerr: config line %d: bad operand %s",c,v);
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
		/* skip nonoptions */
		if(c->argv[x][0] != '-')
			continue;

		/* options that take no parameters */
		if(!strcmp(c->argv[x],"-nooutput")) {
			blockoutput = 1;
			continue;
		}
		if(!strcmp(c->argv[x],"-noinput")) {
			blockinput = 1;
			continue;
		}

		if(!strcmp(c->argv[x],"-extnd")) {
			extendperm = 1;
			continue;
		}

		/* default turn on auth for ALL transactions */
		if(!strcmp(c->argv[x],"-authall")) {
			FtpOp	*op;

			for(op = ops; op->name != (char *)0; op++)
				if((op->flg & OP_AOK) == 0)
					op->flg |= OP_AUTH;
			authallflg++;
			continue;
		}


		/* options that take parameters and lists */
		op = 0;
		if(!strcmp(c->argv[x],"-log"))
			op = accept_setlog;
		if(!strcmp(c->argv[x],"-auth"))
			op = accept_setauth;
		if(!strcmp(c->argv[x],"-deny"))
			op = accept_setdeny;
		if(!strcmp(c->argv[x],"-dest"))
			op = accept_setdest;
		if(op == 0) {
			syslog(LLEV,"fwtkcfgerr: bad option line %d: %s",c->ln,c->argv[x]);
			return(1);
		}
		if(++x >= c->argc) {
			syslog(LLEV,"fwtkcfgerr: malformed line %d: missing option",c->ln);
			return(1);
		}
		if(!strcmp(c->argv[x],"{")) {
			while(1) {
				if(++x >= c->argc) {
					syslog(LLEV,"fwtkcfgerr: malformed line %d: missing option",c->ln);
					return(1);
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



static	int
cmd_user(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	static char	narg[] = "501 Missing or extra username";
	static char	noad[] = "501 Use user@site to connect via proxy";
	char		buf[1024];
	char 		mbuf[512];
	char		*p;
	char		*dest;
	char		*user;
	int		x;
	int		msg_int;
	short		port = FTPPORT;

	/* kludgy but effective. if authorizing everything call auth instead */
	if(authallflg && !authenticated)
		return(cmd_authorize(ac,av,cbuf));

	if(ac < 2)
		return(sayn(0,narg,sizeof(narg)));


	if(ac > 2 && (port = atoi(av[2])) <= 0) {
		sprintf(buf,"501 Bad port number: \"%s\"",av[2]);
		return(say(0,buf));
	}


	if((p = rindex(av[1],'@')) != (char *)0) {
		*p++ = '\0';
		dest = p;
		user = av[1];
	} else {
		if((p = index(av[1],'!')) != (char *)0) {
			*p++ = '\0';
			user = p;
			dest = av[1];
		} else
			return(sayn(0,noad,sizeof(noad)));
	}

	if(*dest == '\0')
		dest = "localhost";


	if(validests != (char **)0) {
		char	**xp;
		int	x;

		for(xp = validests; *xp != (char *)0; xp++) {
			if(**xp == '!' && hostmatch(*xp + 1,dest)) {
				return(baddest(0,dest));
			} else {
				if(hostmatch(*xp,dest))
					break;
			}
		}
		if(*xp == (char *)0)
			return(baddest(0,dest));
	}
	
	/* Extended permissions processing goes in here for destination */
	if(extendperm) {
		msg_int = auth_perm(confp, authuser, "ftp-gw", dest,(char *)0);
		if(msg_int == 1) {
			sprintf(mbuf,"Permission denied for user %s to connect to %s",authuser,dest);
			syslog(LLEV,"deny host=%s/%s connect to %s user=%s",rladdr,riaddr,dest,authuser);
			say(0,mbuf);
			return(1);
		} else {
			if(msg_int == -1) {
				sprintf(mbuf,"No match in netperm-table for %s to ftp to %s",authuser,dest);
				say(0,mbuf);
				return(1);
			}	
		}
	}
	syslog(LLEV,"permit host=%s/%s connect to %s",rladdr,riaddr,dest);

	if((rfd = conn_server(dest,port,0,buf)) < 0) {
		char	ebuf[512];

		strcpy(ebuf,buf);
		sprintf(buf,"521 %s: %s",dest,ebuf);
		return(say(0,buf));
	}
	sprintf(buf,"----GATEWAY CONNECTED TO %s----",dest);
	saveline(buf);

	/* we are now connected and need to try the autologin thing */
	x = getresp(rfd,buf,sizeof(buf),1);
	if(x / 100 != COMPLETE) {
		sendsaved(0,-1);
		return(say(0,buf));
	}
	saveline(buf);

	sprintf(buf,"USER %s",user);
	if(say(rfd,buf))
		return(1);
	x = getresp(rfd,buf,sizeof(buf),1);
	if(sendsaved(0,x))
		return(1);
	return(say(0,buf));
}



static	int
cmd_authorize(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	static char	narg[] = "501 Missing or extra username";
	static char	nserv[] = "501 Cannot connect to authentication server";
	static char	nbad[] = "501 User name too large";
	char		lbuf[512];
	char		rbuf[512];

	authuser[0] = '\0';
	authenticated = 0;
	if(ac < 2)
		return(sayn(0,narg,sizeof(narg)));
	if(strlen(av[1]) > 24)
		return(sayn(0,nbad,sizeof(nbad)));
	if(strlen(av[1]) == 0)
		return(sayn(0,narg,sizeof(narg)));
	if(!authopened)
		if(auth_open(confp))
			return(sayn(0,nserv,sizeof(nserv)));

	/* get welcome message from auth server */
	if(!authopened) {
		if(auth_recv(rbuf,sizeof(rbuf)))
			goto lostconn;
		if(strncmp(rbuf,"Authsrv ready",13)) {
			sprintf(lbuf,"551 %s",rbuf);
			auth_close();
			return(say(0,lbuf));
		}
	}
	authopened++;
	/* send username */
	if(strlen(rladdr) + strlen(riaddr) + strlen(av[1]) + 100 > 512)
		sprintf(lbuf,"authorize %s",av[1]);
	else
		sprintf(lbuf,"authorize %s 'ftp-gw %s/%s'",av[1],rladdr,riaddr);
	if(auth_send(lbuf))
		goto lostconn;
	if(auth_recv(rbuf,sizeof(rbuf)))
		goto lostconn;

	if(!strncmp(rbuf,"challenge ",10))
		sprintf(lbuf,"331 %s",&rbuf[10]);
	else
	if(!strncmp(rbuf,"password",8))
		sprintf(lbuf,"331 Enter authentication password for %s",av[1]);
	else {
		sprintf(lbuf,"551 %s",rbuf);
		return(say(0,lbuf));
	}
	strcpy(authuser,av[1]);
	return(say(0,lbuf));

lostconn:
	auth_close();
	authopened = 0;
	return(sayn(0,nserv,sizeof(nserv)));
}



static	int
cmd_response(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	static char	noad[] = "501 Use \"authorize\" command first";
	static char	nserv[] = "501 Cannot connect to authentication server";
	static char	tolo[] = "501 Response too long";
	static char	logok[] = "230 User authenticated to proxy";
	char		buf[512];
	char		*resp;

	/*
	need to tokenize it ourselves because ftp tokenizes whitespace.
	this is so ugly it hurts a lot.
	*/
	if(ac < 2)
		resp = "";
	else {
		resp = cbuf;
		while(!isspace(*resp))
			resp++;
		while(isspace(*resp))
			resp++;
	}
	if(authuser[0] == '\0' || !authopened)
		return(sayn(0,noad,sizeof(noad)));
	if(strlen(resp) > 512)
		return(sayn(0,tolo,sizeof(tolo)));

	/* send response */
	if(index(resp,'\'') == (char *)0 && index(resp,'\"') == (char *)0)
		sprintf(buf,"response '%s'",resp);
	else
		sprintf(buf,"response %s",resp);
	if(auth_send(buf))
		goto lostconn;
	if(auth_recv(buf,sizeof(buf)))
		goto lostconn;
	if(strncmp(buf,"ok",2)) {
		char	ebuf[512];
		auth_close();
		authopened = 0;
		sprintf(ebuf,"501 %s",buf);
		return(say(0,ebuf));
	}
	authenticated = 1;
	syslog(LLEV,"authenticate user=%s",authuser);
	strcpy(autheduser,authuser);
	auth_close();
	authopened = 0;
	if(buf[2] != '\0') {
		char	ebuf[512];

		sprintf(ebuf,"230 User authenticated to proxy: %s",&buf[2]);
		return(say(0,ebuf));
	}
	return(sayn(0,logok,sizeof(logok)));

lostconn:
	auth_close();
	authopened = 0;
	return(sayn(0,nserv,sizeof(nserv)));
}




baddest(fd,dest)
int	fd;
char	*dest;
{
	Cfg	*cf;
	char	buf[BUFSIZ];

	if((cf = cfg_get("denydest-msg",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: denydest-msg must have one parameter, line %d",cf->ln);
			return(1);
		}
		if(sayfile(fd,cf->argv[0],500)) {
			syslog(LLEV,"fwtksyserr: cannot display denydest-msg %s: %m",cf->argv[0]);
			return(1);
		}
		return(0);
	}
	sprintf(buf,"501 Not permitted to connect to %s",dest);
	syslog(LLEV,"deny host=%s/%s connect to %s",rladdr,riaddr,dest);
		return(say(fd,buf));
}



/* some code duplication */
static	int
cmd_passthru(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	static char	narg[] = "501 Missing destination name";
	int		port = FTPPORT;
	char		buf[1024];
	char		mbuf[512];
	int		msg_int;
	int		x;

	if(ac < 2)
		return(sayn(0,narg,sizeof(narg)));


	if(ac > 2 && (port = atoi(av[2])) <= 0) {
		sprintf(buf,"501 Bad port number: \"%s\"",av[2]);
		return(say(0,buf));
	}

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
		syslog(LLEV,"permit host=%s/%s connect to %s",rladdr,riaddr,av[1]);
	}

	/* Extended permissions processing goes in here for destination */
	if(extendperm) {
		msg_int = auth_perm(confp, authuser, "ftp-gw", av[1], '\0');
		if(msg_int == 1) {
			sprintf(mbuf,"Permission denied for user %s to connect to %s",authuser,av[1]);
			say(0,mbuf);
			return(1);
	        }
		else {
			if(msg_int == -1) {
				sprintf(mbuf,"No match in netperm-table for %s to ftp to %s",authuser,av[1]);
				say(0,mbuf);
				return(1);
			}	
	        }
	}

	if((rfd = conn_server(av[1],port,0,buf)) < 0) {
		char	ebuf[512];

		strcpy(ebuf,buf);
		sprintf(buf,"521 %s: %s",av[1],ebuf);
		return(say(0,buf));
	}

	sprintf(buf,"----GATEWAY CONNECTED TO %s----",av[1]);
	saveline(buf);

	x = getresp(rfd,buf,sizeof(buf),1);
	if(x / 100 != COMPLETE) {
		sendsaved(0,-1);
		return(say(0,buf));
	}
	if(sendsaved(0,x))
		return(1);
	return(say(0,buf));
}



static	int
cmd_port(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	struct sockaddr_in	r;
	struct sockaddr_in	n;
	int			x;
	unsigned char		*k;
	unsigned char		*l;
	static char		narg[] = "500 no port specified";
	static char		nadr[] = "500 cannot decode port specified";
	static char		nprn[] = "500 cannot get peername";
	char			buf[512];

	if(ac < 2)
		return(sayn(0,narg,sizeof(narg)));

	/* save port address for callback later */
	if(porttoaddr(av[1],&clntport))
		return(sayn(0,nadr,sizeof(nadr)));


	/* paranoid: check that we are really PORTing to the client */
	x = sizeof(r);
        if(getpeername(0,(struct sockaddr *)&r,&x) < 0)
		return(sayn(0,nprn,sizeof(nprn)));
	if(bcmp((char *)&clntport.sin_addr,(char *)&r.sin_addr,sizeof(r.sin_addr))) {
		char	xaf[128];

		strcpy(xaf,inet_ntoa(clntport.sin_addr));
		sprintf(buf,"521 PORT %s mismatch %sn",
			inet_ntoa(r.sin_addr),xaf);
		return(say(0,buf));
	}


	/* ok, now build and bind a socket */
	if(boundport != -1)
		close(boundport);
	if((boundport = socket(AF_INET,SOCK_STREAM,0)) < 0) {
		sprintf(buf,"521 PORT socket: %s",sys_errlist[errno]);
		return(say(0,buf));
	}
	r.sin_family = AF_INET;
	bzero((char *)&r.sin_addr,sizeof(r.sin_addr));
	r.sin_port = 0;
	if(bind(boundport,(struct sockaddr *)&r,sizeof(r))) {
		sprintf(buf,"521 PORT bind: %s",sys_errlist[errno]);
		return(say(0,buf));
	}
	if(listen(boundport,1) < 0) {
		sprintf(buf,"521 PORT listen: %s",sys_errlist[errno]);
		return(say(0,buf));
	}


	/* learn enough about the socket to send the PORT */
	x = sizeof(n);
	if(getsockname(rfd,(struct sockaddr *)&n,&x) < 0) {
		sprintf(buf,"521 PORT getsockname: %s",sys_errlist[errno]);
		return(say(0,buf));
	}
	bcopy((char *)&n.sin_addr,(char *)&r.sin_addr,sizeof(n.sin_addr));
	x = sizeof(n);
	if(getsockname(boundport,(struct sockaddr *)&n,&x) < 0) {
		sprintf(buf,"521 PORT getsockname: %s",sys_errlist[errno]);
		return(say(0,buf));
	}
	r.sin_port = n.sin_port;


	/* encode and send over our port to the remote server */
	k = (unsigned char *)&(r.sin_addr);
	l = (unsigned char *)&(r.sin_port);
#define UC(c)   (((int)c) & 0xff)
	sprintf(buf,"PORT %d,%d,%d,%d,%d,%d",UC(k[0]),UC(k[1]),UC(k[2]),
		UC(k[3]),UC(l[0]),UC(l[1]));
	return(say(rfd,buf));
}



/* here we dummy up the abort signalling per FTP (ripped from BSD net2) */
static	int
cmd_abor(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	unsigned char	kbuf[4];

	kbuf[0] = IAC;
	kbuf[1] = IP;
	kbuf[2] = IAC;
	if(send(rfd,(char *)kbuf,3,MSG_OOB) != 3)
		return(1);
	kbuf[0] = DM;
	if(write(rfd,(char *)kbuf,1) != 1)
		return(1);
	return(say(rfd,cbuf));	/* cbuf, we can handle STAT or ABOR */
}



/* mediate quit just for the hell of it */
static	int
cmd_quit(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	char	buf[1024];
	int	x;

	time(&offtime);
	syslog(LLEV,"exit host=%s/%s cmds=%d in=%d out=%d user=%s duration=%d",
		rladdr,riaddr,cmdcnt,inbytcnt,outbytcnt,autheduser,offtime - ontime);
	if(rfd != -1) {
		if(sayn(rfd,"QUIT",4))
			exit(0);
		x = getresp(rfd,buf,sizeof(buf),1);
		sendsaved(0,x);
		if(say(0,buf))
			exit(0);
	}
	exit(0);
}




static	int
cmd_help(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	Cfg	*cf;

	if((cf = cfg_get("help-msg",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: help-msg must have one parameter, line %d",cf->ln);
			return(1);
		}
		if(sayfile(0,cf->argv[0],220)) {
			syslog(LLEV,"fwtksyserr: cannot display welcome %s: %m",cf->argv[0]);
			return(1);
		}
		return(0);
	}
	return(sayn(0,"214 no help available",21));
}




static	int
cmd_noop(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	return(sayn(0,"200 NOOP from proxy successful",30));
}



/* call back to the client on the address they gave as PORT */
callback()
{
	/* if we haven't gotten a valid PORT scrub the connection */
	if((outgoing = accept(boundport,(struct sockaddr *)0,(int *)0)) < 0 || clntport.sin_port == 0)
		goto bomb;

	if((incoming = socket(AF_INET,SOCK_STREAM,0)) < 0)
		goto bomb;

	if(connect(incoming,(struct sockaddr *)&clntport,sizeof(clntport)) < 0)
		goto bomb;

	/* invalidate the port */
	clntport.sin_port = 0;
	return(0);

bomb:
	clntport.sin_port = 0;
	close(boundport);
	boundport = -1;
	if(outgoing != -1)
		close(outgoing);
	outgoing = -1;
	if(incoming != -1)
		close(incoming);
	incoming = -1;
	return(0);
}


peerreply()
{
	char	buf[BSIZ];
	int	x;
	static char	pclos[] = "530 Peer has closed connection";

	if((x = read(rfd,buf,sizeof(buf))) <= 0) {
		if(sayn(0,pclos,sizeof(pclos)))
			return(1);
		close(rfd);
		rfd = -1;
	}
	if(write(0,buf,x) != x)
		return(1);
	return(0);
}


usercmd()
{
	FtpOp		*op;
	static char	badcmd[] = "500 command not understood";
	static char	beconn[] = "500 must be connected to remote server";
	static char	denied[] = "500 command not permitted through gateway";
	static char	gottauth[] = "500 command requires user authentication";
	char		buf[BSIZ];
	char		tokbuf[BSIZ];
	char		mbuf[512];
	char		*tokav[56];
	int		tokac;
	int		x;
	int		msg_int;

	if((x = getline(0,(unsigned char *)buf,sizeof(buf) - 1)) < 0)
		return(1);
	if(buf[0] == '\0')
		return(sayn(0,badcmd,sizeof(badcmd)));
	cmdcnt++;

	tokac = enargv(buf,tokav,56,tokbuf,sizeof(tokbuf));
	if(tokac <= 0)
		return(sayn(0,badcmd,sizeof(badcmd)));

	for(op = ops; op->name != (char *)0; op++) {
		if(!strcasecmp(tokav[0],op->name)) {
			if(op->flg & OP_LOG)
				syslog(LLEV,"%s/%s: %s",rladdr,riaddr,buf);

			if(rfd == -1 && (op->flg & OP_CONN))
				return(sayn(0,beconn,sizeof(beconn)));

			if(op->flg & OP_DENY) {
				syslog(LLEV,"deny host=%s/%s operation %s",rladdr,riaddr,buf);
				return(sayn(0,denied,sizeof(denied)));
			}

			/* Extended permissions go here I think */
			if(extendperm) {			
				msg_int = auth_perm(confp,authuser,"ftp-gw",riaddr,tokav[0]);
				if(msg_int == 1 || msg_int == 0) {
					sprintf(mbuf,"Permission denied for operation");
					syslog(LLEV,"deny host=%s/%s operation %s - extended permissions",rladdr,riaddr,buf);
					say(0,mbuf);
					return(1);
				}
			}

			if((op->flg & OP_AUTH)) {
				if(!authenticated) {
					syslog(LLEV,"deny host=%s/%s operation %s - not authenticated",rladdr,riaddr,buf);
					return(sayn(0,gottauth,sizeof(gottauth)));
				}
			}

			if(op->op == 0 ||
				(rfd != -1 && (op->flg & OP_WCON))) {
				return(sayn(rfd,buf,x));
			}
			return((*op->op)(tokac,tokav,buf));
		}
	}
	return(sayn(0,badcmd,sizeof(badcmd)));
}


copyin()
{
	char	buf[2048];
	int	x;

	if(blockoutput)
		goto bomb;
	x = read(incoming,buf,sizeof(buf));
	if(x <= 0)
		goto bomb;
	if(write(outgoing,buf,x) != x)
		goto bomb;
	outbytcnt += x;
	return(0);

bomb:
	close(incoming);
	incoming = -1;
	if(outgoing != -1) {
		close(outgoing);
		outgoing = -1;
	}
	return(0);
}


copyout()
{
	char	buf[2048];
	int	x;

	if(blockinput)
		goto bomb;
	x = read(outgoing,buf,sizeof(buf));
	if(x <= 0)
		goto bomb;
	if(write(incoming,buf,x) != x)
		goto bomb;
	inbytcnt += x;
	return(0);

bomb:
	close(outgoing);
	outgoing = -1;
	if(incoming != -1) {
		close(incoming);
		incoming = -1;
	}
	return(0);
}



/*
this code is not optimized to be high performance. who cares?
here we handle some odd cases by processing telnet options and
THROWING THEM AWAY. the reason for this is because we want stuff
like ABORT to come to us as normal commands so we can trap them
and pass them to the remote with our own options processing.
this avoids having to process OOB stuff ourselves.
*/
getline(fd,buf,siz)
int		fd;
unsigned char	*buf;
int		siz;
{
	int	x = 0;

	while(1) {
		if(read(fd,(char *)&buf[x],1) != 1)
			return(-1);

		/* get \r\n - read a char and throw it away */
		if(buf[x] == '\r') {
			if(read(fd,(char *)&buf[x],1) != 1)
				return(-1);
			buf[x] = '\0';
			return(x);
		}

		if(buf[x] == '\n') {
			buf[x] = '\0';
			return(x);
		}

		if(buf[x] == IAC) {
			unsigned char	j;
			unsigned char	jbuf[4];

			if(read(fd,&j,1) != 1)
				return(-1);

			switch(j) {
			case WILL:
			case WONT:
				if(read(fd,(char *)&j,1) != 1)
					return(-1);
				jbuf[0] = IAC;
				jbuf[1] = DONT;
				jbuf[2] = j;
				if(write(fd,(char *)jbuf,3) != 3)
					return(-1);
				continue;
			case DO:
			case DONT:
				if(read(fd,(char *)&j,1) != 1)
					return(-1);
				jbuf[0] = IAC;
				jbuf[1] = WONT;
				jbuf[2] = j;
				if(write(fd,(char *)jbuf,3) != 3)
					return(-1);
				continue;
			case IAC:
				x = 0;
				continue;
			case IP:
				if(read(fd,(char *)&j,1) != 1)
					return(-1);
				if(j == DM)
					x = 0;
				continue;
			}
		}

		if(++x >= siz) {
			syslog(LLEV,"fwtksyserr: getline: buffer overrun");
			return(-1);
		}
	}
}



/* save lines from remote server for possible later use */
static void
saveline(s)
char	*s;
{
	if(saveresp == (char **)0)
		saveresp = (char **)malloc(sizeof(char *));
	else
		saveresp = (char **)realloc(saveresp,(saveresps + 1) * sizeof(char *));
	if(saveresp == (char **)0)
		return;
	saveresp[saveresps] = malloc(strlen(s) + 1);
	if(saveresp[saveresps] == (char *)0)
		return;
	strcpy(saveresp[saveresps],s);
	saveresps++;
}




static void
flushsaved()
{
	int	x;

	for(x = 0; x < saveresps - 1; x++)
		free(saveresp[x]);
	saveresps = 0;
	if(saveresp != (char **)0) {
		(void)free(saveresp);
		saveresp = (char **)0;
	}
}




/* flush saved lines prefixing them with 'code' */
sendsaved(fd,code)
int	fd;
int	code;
{
	int	l;
	int	x;

	for(x = 0; x < saveresps; x++) {
		if(code != -1) {
			char	xuf[5];
			xuf[0] = (code / 100) + '0';
			xuf[1] = ((code / 10) % 10) + '0';
			xuf[2] = (code % 10) + '0';
			xuf[3] = '-';
			xuf[4] = '(';
			if(write(fd,xuf,5) != 5)
				return(-1);
		}
		l = strlen(saveresp[x]);
		if(write(fd,saveresp[x],l) != l)
			return(-1);
		if(code != -1 && write(fd,")",1) != 1)
			return(-1);
		if(write(fd,"\r\n",2) != 2)
			return(-1);
	}
	flushsaved();
	return(0);
}



getresp(fd,b,siz,save)
int	fd;
char	*b;
int	siz;
int	save;
{
	while(1) {
		if(getline(fd,(unsigned char *)b,siz) < 0)
			return(-1);
		if(isdigit(b[0]) && isdigit(b[1]) && isdigit(b[2]) && b[3] == ' ') {
			return(((int)(b[0] - '0') * 100) +
				((int)(b[1] - '0') * 10) +
				(int)(b[2] - '0'));
		}
		if(save)
			saveline(b);
	}
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
sayfile(fd,fn,code)
int	fd;
char	*fn;
int	code;
{
	FILE	*f;
	char	buf[BUFSIZ];
	char	yuf[BUFSIZ];
	char	*c;
	int	x;

	if((f = fopen(fn,"r")) == (FILE *)0)
		return(1);
	while(fgets(buf,sizeof(buf),f) != (char *)0) {
		if((c = index(buf,'\n')) != (char *)0)
			*c = '\0';
		x = fgetc(f);
		if(feof(f))
			sprintf(yuf,"%3.3d %s",code,buf);
		else {
			sprintf(yuf,"%3.3d-%s",code,buf);
			ungetc(x,f);
		}
		if(say(fd,yuf)) {
			fclose(f);
			return(1);
		}
	}
	fclose(f);
	return(0);
}


porttoaddr(s,a)
char			*s;
struct sockaddr_in	*a;
{
	unsigned char	*c;
	char		*x;
	static char	d[] = ",";

	bzero((char *)a,sizeof(struct sockaddr_in));

	/* strip out host bits */
	c = (unsigned char *)(&(a->sin_addr));
	if((x = strtok(s,d)) == (char *)0)
		return(1);
	c[0] = atoi(x);
	if((x = strtok((char *)0,d)) == (char *)0)
		return(1);
	c[1] = atoi(x);
	if((x = strtok((char *)0,d)) == (char *)0)
		return(1);
	c[2] = atoi(x);
	if((x = strtok((char *)0,d)) == (char *)0)
		return(1);
	c[3] = atoi(x);

	/* now strip out port bits */
	c = (unsigned char *)(&(a->sin_port));
	if((x = strtok((char *)0,d)) == (char *)0)
		return(1);
	c[0] = atoi(x);
	if((x = strtok((char *)0,d)) == (char *)0)
		return(1);
	c[1] = atoi(x);
	a->sin_family = AF_INET;
	return(0);
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
	mya.sin_port = htons(FTPPORT);
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
