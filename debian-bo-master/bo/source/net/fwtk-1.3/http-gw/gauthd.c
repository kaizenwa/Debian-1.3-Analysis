/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
 *	Rewritten to be gopher auth daemon by
 *		Peter J. Churchyard, Trusted Information Systems, Inc.
 */
static	char	RcsId[] = "Header: gauthd.c,v 1.1 94/09/22 10:36:54 mjr rel ";


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

static void trap_sigurg();

#ifndef	VERSION
#define	VERSION	"1.3"
#endif

#ifndef	BSIZ
#define	BSIZ	2048
#endif

#ifndef	GAPORT
#define	GAPORT	7001
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
static	int			authopened = 0;
static	char			authuser[128];
static	char			autheduser[128];	/* successful */

static	time_t			last;

/* Gopher auth daemon stuff */
struct buffer{
struct buffer	*next;
char		data[1024];
int		len;
int		pos;
};

struct buffer *new_buffer();

struct conn{
struct conn 	*next;
int 		fd;
time_t		last;
struct buffer	*inbuf;
struct buffer	*outlist;
char		*data;		/* point to specific stuff for this object type*/
};

struct conn *clist, *slist;	/* client and server list heads */
struct conn *new_conn();
struct conn *process_client_input();
struct conn *process_server_input();

static int DEBUG;

#define Debug(x) if(DEBUG)fprintf x;else;

main(ac,av)
int	ac;
char	*av[];
{
	Cfg		*cf;
	struct timeval	timeout, ttimeout;
	fd_set		rdy, ordy;
	int		x;
	int		runuid = -1;
	int		rungid = -1;
	time_t		now;
	struct conn	*cp;
	int		maxfd;

#ifndef	LOG_DAEMON
	openlog("gauthd",LOG_PID);
#else
	openlog("gauthd",LOG_PID|LOG_NDELAY,LFAC);
#endif
	time(&last);
	signal(SIGURG,trap_sigurg);

	if((confp = cfg_read("gauthd")) == (Cfg *)-1)
		exit(1);

	authuser[0] = '\0';
	strcpy(autheduser,"unauth");

	while(( x = getopt(ac, av, "D")) != -1){
		switch(x){
		case 'D':
			DEBUG = 1;
			Debug( (stderr,"Debug ON\n"))
			break;
		default:
			syslog(LLEV,"bad command line option -%c", x);
			exit(1);
		}
	}

	if((cf = cfg_get("groupid",confp)) != (Cfg *)0) {

		if(cf->argc != 1) {
			syslog(LLEV,"groupid must have one parameter, line %d",cf->ln);
			exit(1);
		}
		if((rungid = mapgid(cf->argv[0])) == -1) {
			syslog(LLEV,"cannot map %s to gid",cf->argv[0]);
			exit(1);
		}
	}

	if((cf = cfg_get("userid",confp)) != (Cfg *)0) {

		if(cf->argc != 1) {
			syslog(LLEV,"userid must have one parameter, line %d",cf->ln);
			exit(1);
		}
		if((runuid = mapuid(cf->argv[0])) == -1) {
			syslog(LLEV,"cannot map %s to uid",cf->argv[0]);
			exit(1);
		}
	}


	if((cf = cfg_get("directory",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"chroot must have one parameter, line %d",cf->ln);
			exit(1);
		}
		chdir("/");
		if(chdir(cf->argv[0])) {
			syslog(LLEV,"chdir %s: %m",cf->argv[0]);
			exit(1);
		}
		if(chroot(cf->argv[0])) {
			syslog(LLEV,"chroot %s: %m",cf->argv[0]);
			exit(1);
		}
		chdir("/");
	}

	if(rungid != -1 && setgid(rungid)) {
		syslog(LLEV,"cannot setgid %d: %m",rungid);
		exit(1);
	}
	if(runuid != -1 && setuid(runuid)) {
		syslog(LLEV,"cannot setuid %d: %m",runuid);
		exit(1);
	}


	timeout.tv_usec = 0;
	if((cf = cfg_get("timeout",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"timeout must have one parameter, line %d",cf->ln);
			exit(1);
		}
		if((timeout.tv_sec = atoi(cf->argv[0])) <= 0) {
			syslog(LLEV,"timeout %s invalid, line %d",cf->argv[0],cf->ln);
			exit(1);
		}
	} else
		timeout.tv_sec = 60;

	dobind();



Debug((stderr,"timeout= %d %d\n", timeout.tv_sec, timeout.tv_usec))
	/* main loop */
	while(1) {
		time(&now);
		
		if( now - last > 60l){	/* do some timeout stuff */
			
			last = now;
		}


		maxfd = 0;
		FD_ZERO(&ordy);
		FD_ZERO(&rdy);
		FD_SET(0,&rdy);

		cp = clist;
		while(cp){
			FD_SET(cp->fd, &rdy);
			if( cp->fd > maxfd)
				maxfd = cp->fd+1;
			if( cp->outlist){
				FD_SET(cp->fd, &ordy);
			}
			cp = cp->next;
		}

		cp = slist;
		while(cp){
			FD_SET(cp->fd, &rdy);
			if( cp->fd > maxfd)
				maxfd = cp->fd+1;
			if( cp->outlist){
				FD_SET(cp->fd, &ordy);
			}
			cp = cp->next;
		}

		ttimeout.tv_sec = 10;
		ttimeout.tv_usec= 0;

		if((x = select(32,&rdy, &ordy,(fd_set *)0,&ttimeout)) < 0) {
Debug((stderr,"select returned %x\n", x))
			perror("select");
			exit(1);
		}
		if(x == 0)
			continue;

		if( FD_ISSET(0, &rdy)){
			process_new_connection();
		}

		cp = clist;
		while(cp){
			if( FD_ISSET(cp->fd, &rdy)){
				cp = process_client_input(cp);
			}else
				cp = cp->next;
		}

		cp = slist;
		while(cp){
			if( FD_ISSET(cp->fd, &rdy)){
				cp = process_server_input(cp);
			}else
				cp = cp->next;
		}

	}
	/* NOTREACHED */
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


int process_connect(sockfd)
int sockfd;
{
	Cfg		*cf;

	/* see if this is someone we are even willing to converse with */
	if(!oktotalkto(sockfd)) {
		return 1;
	}
	return 0;
}



oktotalkto(sockfd)
int sockfd;
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
	return(0);
}



static	void
accept_setdeny(v,c)
char	*v;
int	c;
{
}



static	void
accept_setauth(v,c)
char	*v;
int	c;
{
}



static	void
accept_setlog(v,c)
char	*v;
int	c;
{
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

		/* default turn on auth for ALL transactions */
		if(!strcmp(c->argv[x],"-authall")) {
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
			syslog(LLEV,"bad option line %d: %s",c->ln,c->argv[x]);
			continue;
		}
		if(++x >= c->argc) {
			syslog(LLEV,"malformed line %d: missing option");
			continue;
		}
		if(!strcmp(c->argv[x],"{")) {
			while(1) {
				if(++x >= c->argc) {
					syslog(LLEV,"malformed line %d: missing option");
					continue;
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


baddest(fd,dest)
int	fd;
char	*dest;
{
	Cfg	*cf;
	char	buf[BUFSIZ];

	if((cf = cfg_get("denydest-msg",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"denydest-msg must have one parameter, line %d",cf->ln);
			return(1);
		}
		if(sayfile(fd,cf->argv[0],500)) {
			syslog(LLEV,"cannot display denydest-msg %s: %m",cf->argv[0]);
			return(1);
		}
		return(0);
	}
	sprintf(buf,"501 Not permitted to connect to %s",dest);
	syslog(LLEV,"deny host=%s/%s connect to %s",rladdr,riaddr,dest);
		return(say(fd,buf));
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
			syslog(LLEV,"getline: buffer overrun");
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
			return(((b[0] - '0') * 100) + ((b[1] - '0') * 10) + (b[2] - '0'));
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




dobind()
{
	struct	sockaddr_in	mya;
	int	x;
	int	nread;

	close(0);
	if((x = socket(AF_INET,SOCK_STREAM,0)) < 0) {
		perror("socket");
		exit(1);
	}
	mya.sin_family = AF_INET;
	bzero(&mya.sin_addr,sizeof(mya.sin_addr));
#ifndef	BINDDEBUGPORT
	mya.sin_port = htons(GAPORT);
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
Debug((stderr,"Port %d bound to %d and listening\n", GAPORT, x))
}


int process_new_connection()
{	int	new_fd;
	struct sockaddr_in sin;
	int len;
	struct conn *cp;
Debug((stderr,"Process_new_connection\n"))

	len = sizeof(struct sockaddr_in);
	new_fd = accept(0, (struct sockaddr *)&sin, &len);

Debug((stderr,"Newfd=%d\n", new_fd))
	if(peername(new_fd,rladdr,riaddr,sizeof(riaddr))) {
Debug((stderr, "failed to get peer name\n"))
		syslog(LLEV,"cannot get peer name");
		goto broke;
	}
	
	if( !oktotalkto(new_fd)){
Debug((stderr, "Not ok to talk to\n"))
		goto broke;
	}

	cp = new_conn();
	if( cp == NULL)
		goto broke;

	cp->fd = new_fd;
/* queue it into the active list */
	cp->next = clist;
	clist = cp;

Debug((stderr,"New connection queued\n"))
	return 0;

broke:
	close(new_fd);
	return 1;
}


struct buffer *new_buffer()
{	struct buffer *p;

	p = (struct buffer *)malloc(sizeof(struct buffer));

	if( p == NULL){
		syslog(LLEV,"No memory in new_buffer");
		return NULL;
	}
	p->data[0] = '\0';
	p->len = 0;
	p->pos = 0;
	return p;
}

void free_buffer(buf)
struct buffer *buf;
{
	if( buf == NULL)
		return;
	free(buf);
}


static struct conn *new_conn()
{	struct conn *cp;

Debug((stderr,"new_conn\n"))
	cp = (struct conn *)malloc( sizeof(struct conn));
	
	if( cp == NULL){
		syslog(LLEV,"No mem in new_conn");
		exit(1);
	}
	
	cp->next = NULL;
	cp->inbuf= NULL;
	cp->outlist= NULL;
	cp->data = NULL;
	cp->fd = 0;
	time(&cp->last);

	return cp;
}




struct conn *process_client_input(cp)
struct conn *cp;
{	int cnt;
	char buf[2];
	struct buffer *bp;

	bp = cp->inbuf;

	if( bp == NULL){
		cp->inbuf = new_buffer();
		bp = cp->inbuf;

		if( bp == NULL){
			syslog(LLEV,"client in: failed to alloc buffer");
			exit(1);
		}
	}
	

	cnt = read(cp->fd, buf, 1);

	if( cnt <= 0){
		struct conn *ncp = cp->next;
Debug((stderr,"Connection %d closed\n", cp->fd))

		if( clist == cp)
			clist = ncp;
		else if( slist == cp)
			slist = ncp;
		close(cp->fd);

		/* client connection has closed */
		return ncp;
	}
	bp->data[bp->pos] = buf[0];
	if( bp->pos < 1023)
		bp->pos++;
	bp->data[bp->pos] = '\0';
	if( bp->pos > bp->len)
		bp->len = bp->pos;
Debug((stderr,"%2d:%s\n", cp->fd, bp->data))
	cp = cp->next;
	return cp;
}


struct conn *process_server_input(cp)
struct conn *cp;
{	int cnt;
	char buf[2];
	struct buffer *bp;

	bp = cp->outlist;

	cnt = read(cp->fd, buf, 1);

	if( cnt <= 0){
		struct conn *ncp = cp->next;

		/* server connection has closed */
		return ncp;
	}
	bp->data[bp->pos] = buf[0];
	if( bp->pos < 1023)
		bp->pos++;
	bp->data[bp->pos] = '\0';
	if( bp->pos > bp->len)
		bp->len = bp->pos;
	cp = cp->next;
	return cp;
}



