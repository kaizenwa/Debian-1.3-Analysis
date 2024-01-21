/*
#define BINDDEBUG
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
static	char	RcsId[] = "Header: tn-gw.c,v 1.9 94/11/01 11:58:13 mjr rel ";


#include	<stdio.h>
#include	<ctype.h>
#include	<syslog.h>
#include	<fcntl.h>
#include	<sys/ioctl.h>
#include	<sys/errno.h>
extern	int	errno;
extern	char	*sys_errlist[];
#include	<sys/signal.h>
#include	<arpa/telnet.h>
#include	<sys/time.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<netdb.h>

extern	char	*index();

#include	"firewall.h"

#ifndef	BSIZ
#define	BSIZ	2048
#endif

#ifndef	TNPORT
#define	TNPORT	23
#endif

static	Cfg			*confp;
static  Cfg			*confx;
static	char			**validests = (char **)0;

static	int			authneeded = 0;
static	int			extendperm = 0;
static	char			authuser[512];
static	int			changeok = 0;
static	int			xgwok = 0;

static	int			rfd = -1;	/* fd to remote */
static	char			riaddr[512];
static	char			rladdr[512];
static	char			*xforwarder = (char *)0;
static	char			dest[512];
static	unsigned char		prebuf[BUFSIZ];
static	int			prebufc;
static	int			kludgeraw = 0;

static	time_t			ontime;
static	time_t			offtime;
static	int			inbytcnt = 0;
static	int			outbytcnt = 0;


extern	char	*maphostname();

static	void	trap_sigurg();
static	int	raw_telnet();

static	int	cmd_quit();
static	int	cmd_help();
static	int	cmd_connect();
static	int	cmd_passwd();
static	int	cmd_xforward();

typedef	struct	{
	char	*name;
	char	*hmsg;
	int	(*op)();
} TnOp;
static	TnOp	ops[] = {
	"connect",	"   connect hostname [serv/port]",	cmd_connect,
	"telnet",	"   telnet hostname [serv/port]",	cmd_connect,
	"open",		0,					cmd_connect,
	"x-gw",		"   x-gw [hostname/display]",		cmd_xforward,
	"help",		"   help/?",				cmd_help,
	"quit",		"   quit/exit",				cmd_quit,
	"password",	"   password",				cmd_passwd,
	"close",	0,					cmd_quit,
	"exit",		0,					cmd_quit,
	"?",		0,					cmd_help,
	0,		0,					0
};


main()
{
	Cfg		*cf;
	struct timeval	timeout;
	struct timeval	*timeoutp = (struct timeval *)0;
	fd_set		rdy;
	int		x;
	int		runuid = -1;
	int		rungid = -1;
	char		*clientnull = (char *)0;
	char		xuf[1024];
	char		huf[128];
	char		*prompt;
	int		promptlen;


#ifndef	LOG_DAEMON
	openlog("tn-gw",LOG_PID);
#else
	openlog("tn-gw",LOG_PID|LOG_NDELAY,LFAC);
#endif
	time(&ontime);
	strcpy(dest,"none");
	strcpy(authuser,"unauth");

	if((confp = cfg_read("tn-gw")) == (Cfg *)-1)
		exit(1);

#ifdef	BINDDEBUG
	/*
	useful for debugging. if you define this it will bind the
	port and listen directly (circumventing inetd) so you can
	run the program under a debugger
	*/
	debugbind();
#endif

	signal(SIGURG,trap_sigurg);
	set_oob_notification(0);

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
			if(sayfile(0,cf->argv[0])) {
				syslog(LLEV,"fwtksyserr: cannot display denial-msg %s: %m",cf->argv[0]);
				exit(1);
			}
			exit(0);
		} else {
			sprintf(xuf,"%s/%s is not authorized to use the telnet proxy",rladdr,riaddr);
			exit(say(0,xuf));
		}
	}


	if((cf = cfg_get("timeout",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: timeout must have one parameter, line %d",cf->ln);
			exit(1);
		}
		timeout.tv_sec = atoi(cf->argv[0]);
		timeout.tv_usec = 0;
		timeoutp = &timeout;
	}



	/* prompt */
	if((cf = cfg_get("prompt",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: prompt must have one parameter, line %d",cf->ln);
			exit(1);
		}
		prompt = cf->argv[0];
	} else
		prompt = "tn-gw-> ";
	promptlen = strlen(prompt);

	/* ugly. send some telnet options. */
	if(initialize())
		exit(1);


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
		if(gethostname(huf,sizeof(huf)))
			strcpy(huf,"unknown");
		sprintf(xuf,"%s telnet proxy (Version %s) ready:",huf,FWTK_VERSION_MINOR);
		if(say(0,xuf))
			exit(1);
	}

	/* if they should authenticate and don't just log out. */
	if(authneeded && getauth()) {
		syslog(LLEV,"exit host=%s/%s no auth",rladdr,riaddr);
		exit(1);
	}


	/* prompt them until done interacting */
	while((x = usercmd(prompt,promptlen)) == 0)
		;
	if(x == 1)
		goto leave;


	/* undo our telnet options -- this needed for TN3270 */
	if(deinitialize())
		exit(1);

	/* main loop */
	while(1) {
		FD_ZERO(&rdy);
		FD_SET(0,&rdy);
		if(rfd != -1)
			FD_SET(rfd,&rdy);


		if((x = select(32,&rdy,(fd_set *)0,(fd_set *)0,timeoutp)) < 0) {
			syslog(LLEV,"select: %m");
			exit(1);
		}
		if(x == 0)
			break;


		if(!kludgeraw && FD_ISSET(0,&rdy)) {
			if(rfd == -1) {
				say(0,"Remote server has closed connection");
				goto leave;
			}
			x = read(0,xuf,sizeof(xuf));
			if(x <= 0)
				goto leave;

			/*
			this is really kludgy and weird.
			it seems that some BSD-based telnet clients send
			a null as the very first thing to the remote
			server. it looks like an old bug with how cr/nul
			terminated lines are handled, but in any case it
			sometimes confuses the remote peer going through
			the proxy (for reasons not fully understood). as
			a work around, we detect it and drop it on the
			floor, and suddenly the problem goes away.
			*/
			if(clientnull == (char *)0 && xuf[0] == 0) {
				x--;
				clientnull = &xuf[1];
			} else {
				clientnull = xuf;
			}

			if(write(rfd,clientnull,x) != x) {
				say(0,"Remote server has closed connection");
				goto leave;
			}
			outbytcnt += x;
		}


		if(kludgeraw && FD_ISSET(0,&rdy))
			if(raw_telnet())
				goto leave;

		if(rfd != -1 && FD_ISSET(rfd,&rdy)) {
			x = read(rfd,xuf,sizeof(xuf));
			if(x <= 0) {
				say(0,"Remote server has closed connection");
				goto leave;
			}
			if(write(0,xuf,x) != x)
				goto leave;
			inbytcnt += x;
		}
	}
leave:
	time(&offtime);
	syslog(LLEV,"exit host=%s/%s dest=%s in=%d out=%d user=%s duration=%d",
		rladdr,riaddr,dest,inbytcnt,outbytcnt,authuser,offtime - ontime);
	exit(0);
}



/*
we're not talking to a real telnet on the other side.
this means the proxy has to tell the client that any
telnet ops it wants to do are not allowed. this is
very suboptimal code, but then telnet is pretty
suboptimal in general.

PCNFS clients don't do correct newline mapping in
telnet mode (they only send ^M) so they still lose.
it's tempting to add prosthetic support but that
might break something else. feh.
*/
static	int
raw_telnet()
{
	unsigned char	j;
	unsigned char	jbuf[4];
	unsigned char	xbuf[512];
	unsigned char	obuf[512];
	int		bcnt;
	int		ocnt;
	int		bcur;

	/* this kludgy bit is because some telnet clients like
	to send a null as the first thing they transmit */
	static	int	sawnul = 0;

	bcnt = read(0,xbuf,sizeof(xbuf));
	if(bcnt <= 0)
		return(-1);

	ocnt = 0;
	for(bcur = 0; bcur < bcnt; bcur++) {
		/* kludge #1 */
		if(!sawnul && xbuf[bcur] == 0) {
			sawnul++;
			continue;
		}

		/* gather stuff to send to server */
		if(xbuf[bcur] != IAC) {
			obuf[ocnt++] = xbuf[bcur];
			continue;
		}

		/* shoot, it's a telnet option, spoof it */
		if(bcur + 1 == bcnt) {
			if(read(0,&j,1) != 1)
				return(-1);
		} else
			j = xbuf[++bcur];

		switch(j) {
		case WILL:
		case WONT:
			if(bcur + 1 == bcnt) {
				if(read(0,&j,1) != 1)
					return(-1);
			} else
				j = xbuf[++bcur];
			jbuf[0] = IAC;
			jbuf[1] = DONT;
			jbuf[2] = j;
			if(write(0,(char *)jbuf,3) != 3)
				return(-1);
			continue;

		case DO:
		case DONT:
			if(bcur + 1 == bcnt) {
				if(read(0,&j,1) != 1)
					return(-1);
			} else
				j = xbuf[++bcur];
			jbuf[0] = IAC;
			jbuf[1] = WONT;
			jbuf[2] = j;
			if(write(0,(char *)jbuf,3) != 3)
				return(-1);
			continue;
		}
	}
	if(ocnt > 0 && write(rfd,obuf,ocnt) != ocnt)
		return(-1);
	outbytcnt += ocnt;
	return(0);
}



static	void
trap_sigurg()
{
	int	atmark;
	char	buf[512];
	char	c;

	signal(SIGURG,trap_sigurg);
	while(1) {
		if(ioctl(0,SIOCATMARK,(char *)&atmark) < 0)
			return;
		if(atmark)
			break;
		read(0,buf,sizeof(buf));
	}
	if(rfd != -1) {
		if(recv(0,(char *)&c,sizeof(c),MSG_OOB) < 0)
			return;
		send(rfd,(char *)&c,sizeof(c),MSG_OOB);
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
		
		for(x = 0; x < cf->argc && cf->argv[x][0] != '-'; x++) {
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



static	int
cmd_xforward(ac,av,cbuf)   
int	ac;
char	*av[];
char	*cbuf;
{
	Cfg     *confx, *cf;
	char	*fakav[16];
	char	buf[256];
	int	pid=0;

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
	fakav[2] = rladdr;
	fakav[3] = "-user";
	fakav[4] = authuser;
	fakav[5] = (char *)0;
	fakav[6] = (char *)0;
	fakav[7] = (char *)0;
	fakav[8] = (char *)0;
	fakav[9] = (char *)0;

	pid=5;
	if(ac > 1) {
		fakav[5] = "-display";
		fakav[6] = av[1];
		pid=7;
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


static	int
cmd_connect(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	static	char	narg[] = "No remote destination specified";
	char		buf[1024];
	char		*namp;
	short		port = TNPORT;

	if(ac < 2)
		return(sayn(0,narg,sizeof(narg)));


	if(ac > 2) {
		if(isalldigits(av[2]))
			port = atoi(av[2]);
		else {
			struct servent	*sp;

			sp = getservbyname(av[2],"tcp");
			if(sp != (struct servent *)0)
				port = ntohs(sp->s_port);
			else {
				sprintf(buf,"Unknown port/service: \"%s\"",av[2]);
				return(say(0,buf));
			}
		}
	}


	if(validests != (char **)0) {
		char	**xp;

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

	if(extendperm) {
		int	msg_int;
		char	mbuf[512];

		msg_int = auth_perm(confp,authuser,"telnet-gw",av[1],(char *)0);
		if(msg_int == 1) {
			sprintf(mbuf,"Permission denied for user %s to connect to %s",authuser,av[1]);
			say(0,mbuf);
			return(1);
		} else {
			if(msg_int == -1) {
				sprintf(mbuf,"No match in netperm-table for %s, to telnet to %s ",authuser,av[1]);
				say(0,mbuf);
				return(1);
		  	}
		}
	}


	if((namp = maphostname(av[1])) != (char *)0) {
		char	ebuf[512];

		syslog(LLEV,"permit host=%s/%s destination=%s",rladdr,riaddr,namp);
		sprintf(ebuf,"Trying %s port %d...",namp,port);
		if(say(0,ebuf))
			return(1);
	} else
		syslog(LLEV,"permit host=%s/%s destination=%s",rladdr,riaddr,av[1]);

	if(port != TNPORT) {
		unsigned char	xuf[9];

		xuf[0] = IAC;
		xuf[1] = WONT;
		xuf[2] = TELOPT_ECHO;
		xuf[3] = IAC;
		xuf[4] = WONT;
		xuf[5] = TELOPT_SGA;
		xuf[6] = IAC;
		xuf[7] = WONT;
		xuf[8] = TELOPT_TTYPE;
		if(write(0,xuf,9) != 9)
			return(1);
		kludgeraw = 1;
	}

	if((rfd = conn_server(av[1],port,0,buf)) < 0) {
		char	ebuf[512];

		strcpy(ebuf,buf);
		sprintf(buf,"%s: %s",av[1],ebuf);
		return(say(0,buf));
	}

	syslog(LLEV,"connected host=%s/%s destination=%s",rladdr,riaddr,av[1]);
	strcpy(dest,av[1]);
	return(2);
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
		if(sayfile(fd,cf->argv[0])) {
			syslog(LLEV,"fwtksyserr: cannot display denydest-msg %s: %m",cf->argv[0]);
			return(1);
		}
		return(0);
	}
	sprintf(buf,"Not permitted to connect to %s",dest);
	syslog(LLEV,"deny host=%s/%s destination=%s",rladdr,riaddr,dest);
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
	time(&offtime);
	syslog(LLEV,"exit host=%s/%s dest=%s in=%d out=%d user=%s duration=%d",
		rladdr,riaddr,dest,inbytcnt,outbytcnt,authuser,offtime - ontime);

	exit(0);
}




static	int
cmd_help(ac,av,cbuf)
int	ac;
char	*av[];
char	*cbuf;
{
	Cfg		*cf;
	TnOp		*op;
	static char	hhdr[] = "Valid commands are: (unique abbreviations may be used)";

	if((cf = cfg_get("help-msg",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: help-msg must have one parameter, line %d",cf->ln);
			return(1);
		}
		if(sayfile(0,cf->argv[0])) {
			syslog(LLEV,"fwtksyserr: cannot display help file %s: %m",cf->argv[0]);
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




static	int
cmd_passwd(ac,av,cbuf)
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
		if(strlen(rladdr) + strlen(riaddr) + strlen(buf) + 100 > 512)
			sprintf(cbuf,"authorize %s",buf);
		else
			sprintf(cbuf,"authorize %s 'change-pw %s/%s'",buf,rladdr,riaddr);
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
		if(buf[2] != '\0') {
			if(say(0,&buf[2]))
				return(1);
			continue;
		}

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
	char		buf[BSIZ];
	char		cbuf[128];
	char		ubuf[128];
	int		bad = 5;
	int		x;

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
			goto close_go;

		/* get user ID from user */
		if(getline((unsigned char *)buf,sizeof(buf) - 1,prom,sizeof(prom),1) < 0)
			goto close_go;

		/* send user ID to auth server */
		if((x = strlen(buf)) == 0)
			continue;
		if(x > 24) {
			if(sayn(0,toobig,sizeof(toobig)))
				goto close_go;
			continue;
		}

		strcpy(ubuf,buf);
		if(strlen(rladdr) + strlen(riaddr) + strlen(buf) + 100 > 512)
			sprintf(cbuf,"authorize %s",buf);
		else
			sprintf(cbuf,"authorize %s 'tn-gw %s/%s'",buf,rladdr,riaddr);
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
		strcpy(authuser,ubuf);
		if(buf[2] != '\0')
			return(say(0,&buf[2]));
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



usercmd(prompt,promptlen)
char	*prompt;
int	promptlen;
{
	TnOp		*op;
	static char	badcmd[] = "Command not understood";
	char		buf[BSIZ];
	char		tokbuf[BSIZ];
	char		*tokav[56];
	int		tokac;
	int		l;

	if(write(0,prompt,promptlen) != promptlen)
		return(1);
	if(getline((unsigned char *)buf,sizeof(buf) - 1,prompt,promptlen,1) < 0)
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



/*
semi recursive descent grab of telnet options.
we store some and we echo some.
*/
static	int
stash_option()
{
	unsigned char	xuf[3];
	static	int	sgahack = 0;
	static	int	echhack = 0;

	/* store the IAC */
	if(prebufc + 1 > sizeof(prebuf))
		return(0);
	prebuf[prebufc++] = IAC;

	/* is it a will/wont? */
	if(prebufc + 1 > sizeof(prebuf))
		return(0);
	if(read(0,&prebuf[prebufc++],1) <= 0)
		return(1);

	switch(prebuf[prebufc - 1]) {
	case DO:
		if(prebufc + 1 > sizeof(prebuf))
			return(0);
		if(read(0,&prebuf[prebufc++],1) <= 0)
			return(1);
		if(prebuf[prebufc - 1] == TELOPT_SGA && sgahack == 0) {
			xuf[0] = IAC;
			xuf[1] = WILL;
			xuf[2] = prebuf[prebufc - 1];
			if(write(0,xuf,3) != 3)
				return(1);
			sgahack = 1;
		}
		if(prebuf[prebufc - 1] == TELOPT_ECHO && echhack == 0) {
			xuf[0] = IAC;
			xuf[1] = WILL;
			xuf[2] = prebuf[prebufc - 1];
			if(write(0,xuf,3) != 3)
				return(1);
			echhack = 1;
		}

		return(0);
	case WILL:
		if(prebufc + 1 > sizeof(prebuf))
			return(0);
		if(read(0,&prebuf[prebufc++],1) <= 0)
			return(1);
		if(prebuf[prebufc - 1] == TELOPT_TTYPE) {
			xuf[0] = IAC;
			xuf[1] = DO;
			xuf[2] = TELOPT_TTYPE;
			if(write(0,xuf,3) != 3)
				return(1);
		}
		return(0);
	case WONT:
	case DONT:
		if(prebufc + 1 > sizeof(prebuf))
			return(0);
		if(read(0,&prebuf[prebufc++],1) <= 0)
			return(1);
		return(0);
	default:
		return(0);
	}
}



/*
BRUTAL kludgery necessary!!
*/
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

		/* stash telnet options wherever we see them */
		if(buf[x] == IAC) {
			if(stash_option())
				return(-1);
			continue;
		}

		/* some telnets send NULL-terminated lines in NOECHO :( */
		if(buf[x] == '\0')
			continue;

		/* get \r\n - read a char and throw it away */
		if(buf[x] == '\r') {
			if(write(0,"\r\n",2) != 2)
				return(-1);
			sawcr = 1;
			buf[x] = '\0';
			return(x);
		}

		if(buf[x] == '\n') {
			if(sawcr) {
				sawcr = 0;
				continue;
			}
			if(write(0,"\r\n",2) != 2)
				return(-1);
			buf[x] = '\0';
			return(x);
		} else
			sawcr = 0;

		/* control R */
		if(buf[x] == 18) {
			if(write(0,"\r\n",2) != 2)
				return(-1);
			if(write(0,pro,plen) != plen)
				return(-1);
			buf[x] = '\0';
			if(echo && write(0,buf,x) != x)
				return(-1);
			continue;
		}

		/* control U/ control C */
		if(buf[x] == 3 || buf[x] == 21) {
			if(write(0,"\r\n",2) != 2)
				return(-1);
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
			if(write(0,xab,xac) != xac)
				return(-1);
			continue;
		}

		if(!isprint(buf[x])) {
			unsigned char	xab[1];

			xab[0] = 007;
			if(write(0,xab,1) != 1)
				return(-1);
			continue;
		}

		if(echo) {
			if(write(0,(char *)&buf[x],1) != 1)
				return(-1);
		} else {
			if(write(0,"#",1) != 1)
				return(-1);
		}
		if(++x >= siz) {
			syslog(LLEV,"fwtksyserr: getline: buffer overrun");
			return(-1);
		}
	}
}



initialize()
{
	unsigned char	xuf[6];

	xuf[0] = IAC;
	xuf[1] = WILL;
	xuf[2] = TELOPT_ECHO;
	xuf[3] = IAC;
	xuf[4] = WILL;
	xuf[5] = TELOPT_SGA;
	if(write(0,xuf,6) != 6)
		return(1);
	return(0);
}



deinitialize()
{
	fd_set		fs;
	struct timeval	to;
	unsigned char	xuf[256];
	int		rbi = 0;
	int		n = 0;
	int		x = 0;

	to.tv_usec = 0;
	to.tv_sec = 1;

	while(rbi < prebufc) {
		if(prebuf[rbi++] != IAC)
			continue;

		xuf[0] = IAC;
		xuf[1] = prebuf[rbi++];
		xuf[2] = prebuf[rbi++];

		/* undo */
		switch (xuf[1]) {
		case DO:
			xuf[1] = WONT;
			break;
		case DONT:
			xuf[1] = WONT;
			break;
		case WILL:
			xuf[1] = DONT;
			break;
		case WONT:
			xuf[1] = DONT;
			break;
		}

		n = 0;
		switch(xuf[1]) {
		case DONT:
		case WONT:
			n = 3;
			if(write(0,xuf,n) != n)
				return(1);
			break;
		}

		if(!n)
			continue;

		for(x = 0; x < sizeof(xuf); ) {
			FD_ZERO(&fs);
			FD_SET(0,&fs);
			if(select(1,&fs,(fd_set *)0,(fd_set *)0,&to) < 1)
				break;
			if(FD_ISSET(0,&fs)) {
				if(read(0,&xuf[x++],1) <= 0)
					return(1);
			}
		}
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
	mya.sin_port = htons(TNPORT);
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
