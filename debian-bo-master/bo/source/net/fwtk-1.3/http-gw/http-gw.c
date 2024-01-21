/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
 *		Peter J. Churchyard, Trusted Information Systems, Inc.
 *
 *	22-Aug-1994	Started to add the Gopher+ stuff. (pjc)
 */
static	char	RcsId[] = "Header: http-gw.c,v 1.2 94/11/01 13:53:37 mjr rel ";


#include	<stdio.h>
#include	<ctype.h>
#include	<syslog.h>
#include	<sys/signal.h>
#include	<sys/ioctl.h>
#include	<sys/errno.h>
#include	<sys/param.h>
#include	<limits.h>
extern	int	errno;
extern	char	*sys_errlist[];
#include	<arpa/ftp.h>
#include	<arpa/telnet.h>
#include	<sys/time.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<netdb.h>
#include	<string.h>
#include	<varargs.h>


#include	<net/if.h>
#ifndef VERSION
#include	"version.h"
#endif

extern	char	*rindex();
extern	char	*index();
extern	char	*strtok();
extern	char	*inet_ntoa();

extern	char	*optarg;

#include	"firewall.h"

#ifndef	VERSION
#define	VERSION	"0.9"
#endif

#ifndef	BSIZ
#define	BSIZ	2048
#endif

#ifndef	FTPPORT
#define	FTPPORT	21
#endif


#ifndef GOPORT
#define GOPORT 70
#endif

#ifndef HTTPPORT
#define HTTPPORT 80
#endif

#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

/* Define some data types used to determine what to do for this transaction */
/* These are returned by the routines that parse the 'path' */
#define TYPE_UNKNOWN 0
#define TYPE_DIR 1
#define TYPE_TEXT 2
#define TYPE_BINARY 4
#define TYPE_EXEC 8
#define TYPE_WAIS 0x10

#define MASK_BASE (TYPE_DIR|TYPE_TEXT|TYPE_BINARY|TYPE_EXEC|TYPE_WAIS)

/* the following are not 'base' types */
#define TYPE_READ 0x40		/* It is a read data type */
#define TYPE_WRITE 0x80		/* it is a write data type */
#define	TYPE_FTP 0x100		/* Its an ftp type operation */
#define TYPE_PLUS 0x200		/* It is a gopher plus operation */
#define TYPE_HTTP 0x400		/* It is an http operation */
#define TYPE_HTTPREQ 0x800	/* need to talk to an HTTP server */
#define TYPE_PROXYCLIENT 0x1000	/* The client knows about proxies */
#define TYPE_LOCAL	0x2000	/* Auth stuff etc */
#define TYPE_GOPHER	0x4000
#define TYPE_TELNET	0x8000

static	Cfg			*confp;
static	char			**validests = (char **)0;

static	Cfg			*fconfp;		/* used to get ftp-gw entries */
static	int			blockinput = 0;
static	int			blockoutput = 0;

static	int			rfd = -1;	/* fd to remote */
static	int			boundport = -1;
static	struct sockaddr_in	clntport;
static	char			riaddr[512];
static	char			rladdr[512];

static	int			authenticated = 0;
static	int			authopened = 0;
static	char			authuser[128];
static	char			autheduser[128];	/* successful */

static	time_t			ontime;
static	time_t			offtime;
static	int			inbytcnt = 0;
static	int			outbytcnt = 0;
static	int			cmdcnt = 0;

static	struct timeval	timeout;
/* START OF GOPHER SPECIFIC STUFF */

#ifdef BINDDEBUG
static int			DEBUG = 0;
static int			goport= GOPORT;		/* only used for debugging */
FILE *debugf;

#define Debug(x) if(DEBUG)fprintf x;
#else
#define Debug(x);
#endif

static	char			ourname[512];
static	int			ourport;
static	struct hostent		*ourhe;

static	void	trap_sigurg();
static	void	net_timeout();

#define GO_REQ_LEN (PATH_MAX+10+MAXHOSTNAMELEN+5)	/* This should be enough*/
static	char	go_request[GO_REQ_LEN];
static	char	orig_request[GO_REQ_LEN*3];		/* needed by auth code (allows for worse case escapes)*/
static	char	go_directory[PATH_MAX];
static	char	def_server[MAXHOSTNAMELEN+6];		/* where to hand off to */
static	int	def_port;
static char	def_httpd[MAXHOSTNAMELEN+6];
static	int	def_hport;

static 	char	ftp_proxy[PATH_MAX];			/* what ftp-proxy to use */
static	int	ftp_proxy_port;
static	char	ftp_user[MAXHOSTNAMELEN+20];		/* what usercmd to send */
static	char	ftp_pass[80];
static	char	ftp_server[MAXHOSTNAMELEN];


static	char	rem_server[MAXHOSTNAMELEN+6];
static	int	rem_type;				/* what type of request */
static	int	rem_typech;				/* the gopher type char */
static	char	rem_path[PATH_MAX];
static	int	rem_port;
static	int	chk_type_ch;				/* Set by check_req */

static	char	nouse_msg[PATH_MAX];			/* name of use denial 
							   msg file. also used 
							   as the flag! */
/* Now define the permission flags that affect what they can do */
#define G_NOPERM	0
#define G_DIR		1	/* can get directories */
#define G_READ		2	/* can read files */
#define G_EXEC		4	/* can 'exec' */
#define G_WAIS		8	/* can do WAIS searches */
#define G_PLUS		0x10	/* can do gopher+ and HTTP/1.x stuff */
#define G_WRITE		0x20	/* can do gopher+ or HTTP post uploads */
#define G_FTP		0x40	/* can do ftp commands */
#define G_HTTP		0x80	/* can do http commands */
#define G_GOPHER	0x100	/* can do gopher requests */
#define G_ALL		0xffff	/* can do every thing */

static	int	permissions=G_NOPERM;
static	int	filter_replies = G_NOPERM;
static	int	logging;
static	int	auth_funcs;		/* What we need auth for */
static	char	auth_token[1024];
static	char	*add_auth();
static	char	*add_auth_to_url();
static	char	tmp_auth_buf[GO_REQ_LEN+512];

static char errbuf[GO_REQ_LEN*3];	/* used by many, tmp storage */

/* Some HTTP specific variables */
static	char	http_protocol[80];	/* also used as a flag... */
static	char	http_method[80];	/* what method, "" == GET */

/* handle TELNET type stuff through proxy */

static	int		can_plug;
static	char		priaddr[512];
static	char		prladdr[512];


void http_exit(ret)
int ret;
{
	syslog(LLEV,"exit host=%s/%s code=%d", rladdr, riaddr, ret);
	exit(ret);
}

/* Start of main code */

main(ac,av)
int	ac;
char	*av[];
{
	Cfg		*cf;
	fd_set		rdy;
	int		x;
	int		runuid = -1;
	int		rungid = -1;
	char		xuf[1024];
	char		huf[128];
	char		*passuser = (char *)0;	/* passed user as av */
	extern		int optind;
	static struct sockaddr_in serv_addr;
	int		length = sizeof(serv_addr);

#ifndef	LOG_DAEMON
	openlog("http-gw",LOG_PID);
#else
	openlog("http-gw",LOG_PID|LOG_NDELAY,LFAC);
#endif
	time(&ontime);
	signal(SIGURG,trap_sigurg);

	authuser[0] = '\0';
	strcpy(autheduser,"unauth");

	/* yes, it now takes command line options */
	while((x = getopt(ac,av,"d:Du:a:")) != -1) {
		switch(x) {
		case 'u':
			passuser = optarg;
			break;
		case 'a':
			strcpy(autheduser,optarg);
			authenticated = 1;
			break;
#ifdef BINDDEBUG
		case 'D':
			DEBUG |= 1;
			debugf = stderr;
			break;

		case 'd':
			DEBUG |= 2;
			debugf = fopen(optarg, "a");
			if( debugf == NULL){
				DEBUG = 0;
				syslog(LLEV,"failed to append to file %s", optarg);
			}
			setvbuf(debugf, NULL, _IONBF, (size_t)0);
			break;
#endif
		default:
			syslog(LLEV,"bad command line option -%c",x);
			exit(1);
		}
	}
	

#ifdef	BINDDEBUG

	if( DEBUG && optind < ac){
		goport = atoi(av[optind++]);
	}

	if(DEBUG&1){
		debugbind();
	}
#endif
	confp = cfg_read("http-gw");

	fconfp = cfg_read("ftp-gw");
	if( confp == NULL || confp == (Cfg *)-1)
		confp = fconfp;

	if( confp == NULL || confp == (Cfg *)-1){
		Debug((debugf,"Failed to get any net-perm entries\n"))
		syslog(LLEV,"Failed to get any netperm-table entries");
		exit(1);
	}

	if(gethostname(ourname,sizeof(ourname)))
		strcpy(ourname,"unknown");
#ifndef NO_GETHOSTBYNAME
	ourhe = gethostbyname ( ourname);
	if( NULL != ourhe){
		strcpy(ourname, ourhe->h_name);
	}
#endif


	if( getsockname(0, (struct sockaddr *) &serv_addr, &length)){
		syslog(LLEV,"cannot get our port");
		exit(1);
	}
	ourport = ntohs(serv_addr.sin_port);

	if(peername(0,rladdr,riaddr,sizeof(riaddr))) {
		Debug( (debugf,"cannot get peer name\n"))
		syslog(LLEV,"cannot get peer name");
		exit(1);
	}


	if((cf = cfg_get("groupid",confp)) != (Cfg *)0) {

		if(cf->argc != 1) {
			syslog(LLEV,"groupid must have one parameter, line %d",cf->ln);
			http_exit(1);
		}
		if((rungid = mapgid(cf->argv[0])) == -1) {
			syslog(LLEV,"cannot map %s to gid",cf->argv[0]);
			http_exit(1);
		}
	}

	if((cf = cfg_get("userid",confp)) != (Cfg *)0) {

		if(cf->argc != 1) {
			syslog(LLEV,"userid must have one parameter, line %d",cf->ln);
			http_exit(1);
		}
		if((runuid = mapuid(cf->argv[0])) == -1) {
			syslog(LLEV,"cannot map %s to uid",cf->argv[0]);
			http_exit(1);
		}
	}



	if((cf = cfg_get("directory",confp)) != NULL){
		if(cf->argc != 1) {
			syslog(LLEV,"directory must have one parameter, line %d",cf->ln);
			http_exit(1);
		}
		strncpy(go_directory, cf->argv[0], PATH_MAX-1);

		chdir("/");
		if( chdir(cf->argv[0])){
			syslog(LLEV,"chdir %s: %m", cf->argv[0]);
			http_exit(1);
		}

		if( chroot(cf->argv[0])){
			syslog(LLEV,"chroot %s: %m", cf->argv[0]);
			http_exit(1);
		}
		chdir("/");
	}

	if((cf = cfg_get("default-gopher",confp)) != NULL){
		if(cf->argc != 1) {
			syslog(LLEV,"default-gopher must have one parameter, line %d",cf->ln);
			http_exit(1);
		}
		strncpy(def_server, cf->argv[0], MAXHOSTNAMELEN-1+6);
		def_port = get_port(def_server,GOPORT);
	}
	if((cf = cfg_get("default-httpd",confp)) != NULL){
		if(cf->argc != 1) {
			syslog(LLEV,"default-httpd must have one parameter, line %d",cf->ln);
			http_exit(1);
		}
		strncpy(def_httpd, cf->argv[0], MAXHOSTNAMELEN-1+6);
		def_hport = get_port(def_httpd,HTTPPORT);
	}
	if((cf = cfg_get("ftp-proxy",confp)) != NULL){
		if(cf->argc != 1) {
			syslog(LLEV,"ftp-proxy must have one parameter, line %d",cf->ln);
			http_exit(1);
		}
		strncpy(ftp_proxy, cf->argv[0], MAXHOSTNAMELEN-1+6);
		ftp_proxy_port = get_port(ftp_proxy,FTPPORT);
		rem_port = ftp_proxy_port;
		Debug( (debugf,"ftp_proxy: %s(%u)\n", ftp_proxy, ftp_proxy_port))
	}

	if(rungid != -1 && setgid(rungid)) {
		syslog(LLEV,"cannot setgid %d: %m",rungid);
		http_exit(1);
	}
	if(runuid != -1 && setuid(runuid)) {
		syslog(LLEV,"cannot setuid %d: %m",runuid);
		http_exit(1);
	}

	/* see if this is someone we are even willing to converse with */
	if(!oktotalkto()) {
		sprintf(nouse_msg,"Sorry, Access denied for %s (%s)",rladdr,riaddr);
	}

	timeout.tv_usec = 0;
	if((cf = cfg_get("timeout",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"timeout must have one parameter, line %d",cf->ln);
			http_exit(1);
		}
		if((timeout.tv_sec = atoi(cf->argv[0])) <= 0) {
			syslog(LLEV,"timeout %s invalid, line %d",cf->argv[0],cf->ln);
			http_exit(1);
		}
	} else
		timeout.tv_sec = 5*60;


/* read the request */
	if( getline(0, go_request, 1023) >= 0)
		process_request(0, go_request);


	time(&offtime);
	syslog(LLEV,"exit host=%s/%s cmds=1 in=%d out=%d user=%s duration=%d",
		rladdr,riaddr,inbytcnt, outbytcnt,autheduser, offtime - ontime);
	go_error(0, 0, NULL);	/* flush error messages */
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

static void 
net_timeout()
{
	syslog(LLEV,"Network timeout signal after %d seconds", timeout.tv_sec);
	http_exit(1);
}

/* Check to see if we even talk to this client! */

oktotalkto()
{
	Cfg	*cf, *tp;
	int	x;

	permissions = G_ALL;		/* Start with all */

	tp = confp;
	while(tp){
		cf = cfg_get("hosts",tp);
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
					syslog(LLEV,"permit host=%s/%s use of gateway (%s)",rladdr,riaddr, VERSION);
					return(acceptrule(cf) == 0);
				}
			}
	
	skip:
			cf = cfg_get("hosts",(Cfg*)0);
		}
		if( tp == fconfp){
			break;
		}
		permissions = G_ALL & ~G_EXEC;
		tp = fconfp;
	}
	syslog(LLEV,"deny host=%s/%s use of gateway (%s)",rladdr,riaddr, VERSION);
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


/* Gopher specific options processing
 */
struct pars_opts{
char *name;
int type;
}p_opts[] = {
{"all", G_ALL},
{"dir", G_DIR},
{"read",G_READ},
{"exec",G_EXEC},
{"wais",G_WAIS},
{"plus",G_PLUS},
{"write",G_WRITE},
{"ftp" ,G_FTP},
{"http",G_HTTP},
{"gopher",G_GOPHER},
/* Now come some of the ftp-gw style options */
{"retr",G_READ},
{"stor",G_WRITE},
{ NULL, G_NOPERM}
};


int	parse_options(s)
char *s;
{	struct pars_opts *t = p_opts;

	while(t->name){
		if( !strcasecmp(s, t->name))
			return t->type;
		t++;
	}
	return G_NOPERM;
}

static void accept_options(v, c)
char *v;
int	c;
{
	permissions |= parse_options(v);
}

static void
deny_options(v, c)
char *v;
int	c;
{	int x;

	x = parse_options(v);
	if( x != G_NOPERM){
		permissions &= ~x;
	}
}

static void
filter_options(v, c)
char *v;
int	c;
{
	filter_replies |= parse_options(v);
}



static void
log_options(v, c)
char *v;
int	c;
{
	logging |= parse_options(v);
}


static void
auth_options(v, c)
char *v;
int	c;
{
	auth_funcs |= parse_options(v);
	return;
}

acceptrule(c)
Cfg	*c;
{
	int	x, y;
	void	(*op)();

	for(x = 1; x < c->argc; x++) {
		/* skip nonoptions */
		if(c->argv[x][0] != '-')
			continue;

		/* options that take no parameters */
		if(!strcmp(c->argv[x],"-nooutput")) {
			permissions &= ~G_WRITE;	/* No write */
			blockoutput = 1;
			continue;
		}
		if(!strcmp(c->argv[x],"-noinput")) {
			permissions &= ~G_READ;		/* No read */
			blockinput = 1;
			continue;
		}
		if(!strcmp(c->argv[x],"-authall")){
			auth_funcs = G_ALL;
			continue;
		}
		if(!strcmp(c->argv[x],"-plugok")){
			can_plug = 1;
			continue;
		}
		if(!strcmp(c->argv[x],"-gopher")){
			if( x+1 >= c->argc){
				syslog(LLEV,"config line %d: -gopher needs an argument.", c->ln);
				return 0;
			}
			strncpy(def_server, c->argv[x+1], MAXHOSTNAMELEN+5);
			x++;
			continue;
		}
		if(!strcmp(c->argv[x],"-httpd")){
			if( x+1 >= c->argc){
				syslog(LLEV,"config line %d: -httpd needs an argument.", c->ln);
				return 0;
			}
			strncpy(def_httpd, c->argv[x+1], MAXHOSTNAMELEN+5);
			x++;
			continue;
		}

		/* options that take parameters and lists */
		op = 0;
		if(!strcmp(c->argv[x],"-dest"))
			op = accept_setdest;
		else if(!strcmp(c->argv[x],"-permit")){
			permissions = G_NOPERM;
			op = accept_options;
		}
		else if(!strcmp(c->argv[x],"-deny"))
			op = deny_options;
		else if(!strcmp(c->argv[x],"-filter"))
			op = filter_options;
		else if(!strcmp(c->argv[x],"-log"))
			op = log_options;
		else if(!strcmp(c->argv[x],"-auth"))
			op = auth_options;

		if(op == 0) {
			syslog(LLEV,"bad option line %d: %s",c->ln,c->argv[x]);
			continue;
		}
		if(++x >= c->argc) {
			syslog(LLEV,"malformed line %d: missing option",c->ln);
			continue;
		}
		if(!strcmp(c->argv[x],"{")) {
			while(1) {
				if(++x >= c->argc) {
					syslog(LLEV,"malformed line %d: missing option",c->ln);
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

/* read a line from the connection */

getline(fd,buf,siz)
int		fd;
unsigned char	*buf;
int		siz;
{
	int	x = 0;
	int	ret = -1;

	(void) signal(SIGALRM, net_timeout);
	(void) alarm(timeout.tv_sec);
	while(1) {
		if(read(fd,(char *)&buf[x],1) != 1)
			goto done;

		/* get \r\n - read a char and throw it away */
		if(buf[x] == '\r') {
			if(read(fd,(char *)&buf[x],1) != 1)
				goto done;
			buf[x] = '\0';
			ret = x;
			goto done;
		}

		if(buf[x] == '\n') {
			buf[x] = '\0';
			ret = x;
			goto done;
		}

		if(buf[x] == IAC) {
			unsigned char	j;
			unsigned char	jbuf[4];

			if(read(fd,&j,1) != 1)
				goto done;

			switch(j) {
			case WILL:
			case WONT:
				if(read(fd,(char *)&j,1) != 1)
					goto done;
				jbuf[0] = IAC;
				jbuf[1] = DONT;
				jbuf[2] = j;
				if(write(fd,(char *)jbuf,3) != 3)
					goto done;
				continue;
			case DO:
			case DONT:
				if(read(fd,(char *)&j,1) != 1)
					goto done;
				jbuf[0] = IAC;
				jbuf[1] = WONT;
				jbuf[2] = j;
				if(write(fd,(char *)jbuf,3) != 3)
					goto done;
				continue;
			case IAC:
				x = 0;
				continue;
			case IP:
				if(read(fd,(char *)&j,1) != 1)
					goto done;
				if(j == DM)
					x = 0;
				continue;
			}
		}

		if(++x >= siz) {
			syslog(LLEV,"getline: buffer overrun");
			goto done;
		}
	}
done:
	(void) alarm(0);
	(void) signal(SIGALRM, SIG_IGN);
	return ret;
}


say_sub(fd,s)
int	fd;
char	*s;
{	int	n;
	n = strlen(s);

	return(write(fd,s,n) != n);
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



#ifdef	BINDDEBUG

debugbind()
{
	static	int	x;
	int	nread;


	if( x == 0){
		x = make_conn(goport);
		if(listen(x,5) < 0) {
			syslog(LLEV,"Listen failed");
			perror("listen");
			http_exit(1);
		}
	}
	if((nread = accept(x,0,0)) < 0) {
		syslog(LLEV,"Accept failed");
		perror("accept");
		http_exit(1);
	}
	close(0);
	dup(nread);
	close(1);
	dup(nread);
}


#endif

/* Start of the main gopher specific code */

/* reply parse
 *
 * return a pointer to a static array of pointers to the tab seperated
 * fields of the buffer.
 *
 * NOTE! This routine over writes the tabs in the buffer with '\0's.
 *
 * returns NULL if it thinks something is wrong!
 */

static char *parse_vec[10];

char **reply_parse(buf)
char *buf;
{	char **pv;
	char *p;
	int cnt,x;

	pv = parse_vec;
	cnt = 0;
	*pv = NULL;

	while(buf && *buf && cnt < 9){
		*pv++ = buf;
		*pv = NULL;
		cnt++;
		while(*buf && *buf != '\t')buf++;
		if( *buf == '\t')
			*buf++ = '\0';
	}
	if( cnt >= 4)
		return parse_vec;
/* undo the mods */
	for(x=0; x < cnt; x++){
		p = parse_vec[x];
		while(*p)p++;
		*p = '\t';
	}
	*p = '\0';		/* need null at end of last one! */
	return NULL;
}

/* This is the main guts of the gopher proxy
 * 
 * Here we process the request from the client and
 * pass it on to the server.
 */

int process_request(sockfd, buf)
int sockfd;
char *buf;
{	Cfg	*cf;
	int x;

/* first check to see if we permit use of proxy.. */

Debug((debugf,"Process:%s\n",buf))

/* Check to see if request is a proxy special. 
 * If not, try to hand off else it is an error */

	rem_type = proxy_form(sockfd, buf);
	if( (rem_type & TYPE_HTTP) == 0){
		rem_type |= TYPE_GOPHER;
	}

	if( (MASK_BASE & rem_type) == TYPE_UNKNOWN){
	/*(NOT A SPECIAL FORM)*/

		if((rem_type & TYPE_LOCAL)== 0){
			if( (rem_type & TYPE_HTTP) && def_httpd[0]){
/* default actions from a WWW client */
				strncpy(rem_server, def_httpd, MAXHOSTNAMELEN-1+6);
				def_port = def_hport;
				rem_type |= TYPE_HTTPREQ;
				if( *buf == '\0')
					buf = "/";

			}else if(  def_server[0]){
/* default action from a GOPHER client */
				strncpy(rem_server, def_server, MAXHOSTNAMELEN-1+6);
			}else{
				rem_type = check_req(sockfd, buf);
				go_error(sockfd, 404, "No server configured to handle your request");
				go_error(sockfd, 404, "%s", buf);
				return 1;
			}
		}else{
Debug((debugf,"BUF=%s\n", buf))
			if( !strncasecmp(buf, "ptelnet:", 8)){
				if( can_plug){
					do_plug(sockfd, buf);
					return 1;
				}
				go_error(sockfd, 404, "Not authorised to telnet via http-gw");
				return 1;
			}
			if( !strncasecmp(buf, "pwais:", 6)){
				if( can_plug){
					do_plug(sockfd, buf);
					return 1;
				}
				go_error(sockfd, 404, "Not authorised to use WAIS via http-gw");
				return 1;
			}
		}

		rem_type = check_req(sockfd, buf) | (rem_type &0xff00);
		if( (MASK_BASE&rem_type) == TYPE_UNKNOWN){
			go_error(sockfd, 404, "Invalid path specified!");
			go_error(sockfd, 404, "%s", buf);
			return 1;
		}
		strncpy(rem_path, buf, PATH_MAX-1);
	}
	rem_typech = chk_type_ch;

/* check to see if we were denied with a reason */
	if( nouse_msg[0]){
		go_error(sockfd, 403, "%s", nouse_msg);
		return 0;
	}

/* Now look for gopher+ information in the request */

	rem_type = check_plus(sockfd, rem_path,rem_type);

/* Now look for the special auth stuff and process if required */
	check_for_auth(sockfd, rem_path);

/* request has been parsed, rem_server is where to connect to. rem_path is the
 * selector to use. rem_type is the type of the request. */

/* check to see if we need to validate on destination */

	if( check_dest(sockfd)){
		return 1;
	}

/* do logging */
	if( logging){
		char *proto = "GOPHER";
		char *cmd = "get";

		if( rem_type & TYPE_FTP)
			proto = "FTP";
		else if( rem_type & TYPE_HTTPREQ)
			proto = "HTTP";

		if( rem_type & TYPE_DIR)
			cmd = "dir";
		else if( rem_type & TYPE_WRITE)
			cmd = "put";
		else if( rem_type & TYPE_WAIS)
			cmd = "wais";
		else if( rem_type & TYPE_EXEC)
			cmd = "exec";

		if( in_perm_set(rem_type, logging)){
			syslog(LLEV,"log host=%s/%s protocol=%s cmd=%s dest=%s path=%s", rladdr, riaddr, proto, cmd, rem_server, rem_path);
		}
	}

/* validate the gopher operation requested */

	if( !can_do(sockfd, rem_type)){
		return 1;
	}

/* now process the reply */
	if( rem_type & TYPE_HTTPREQ){
		forward_http(sockfd, rem_path, rem_server);
	}else if( rem_type & TYPE_DIR){
		forward_dir(sockfd, rem_path, rem_server);
	}else{
		forward_file(sockfd, rem_path, rem_server);
	}
	return 0;
}



int starthtml(sockfd, msg)
int sockfd;
char *msg;
{
		if( rem_type & TYPE_FTP){
			say_sub(sockfd,"<h1>FTP "); 
		}else{
			say_sub(sockfd,"<h1>Gopher "); 
		}
		if( msg){
			say_sub(sockfd,msg);
		}
		say(sockfd,"</h1>");
#ifdef ADVERT
		say(sockfd," translated by the <A");
		strcpy(errbuf,"HREF=\"http://www.tis.com/\"");
		trans_anchor(sockfd, errbuf, "HTTP");
		say(sockfd,"> Trusted Information Systems </A> http/gopher gateway");
#endif
	return 0;
}

int gostartmenu(sockfd)
int sockfd;
{
	if( rem_type & TYPE_HTTP){
		starthtml(sockfd,"Menu"); 
		say(sockfd,"<HR><MENU>");
	}
	return 0;
}

int goendmenu(sockfd)
int sockfd;
{
	if( rem_type & TYPE_HTTP){
		say(sockfd,"</MENU>");
	}
	return 0;
}

int gostarthtml(sockfd)
int sockfd;
{
	if( rem_type & TYPE_HTTP){
		starthtml(sockfd,"Text");
		say(sockfd,"<HR><PRE>");
	}
	return 0;
}

int goendhtml(sockfd)
int sockfd;
{
	say(sockfd,"</PRE>");
	return 0;
}


int goputmenu(sockfd, menu)
int sockfd;
char *menu;
{	char *p, *q;
	int ch;
	char **parse_vec;
	static char abuf[GO_REQ_LEN+512];

	if( rem_type & TYPE_HTTP){
		say(sockfd, "<LI>");
		parse_vec = reply_parse( menu);
		if( parse_vec == NULL){
			say(sockfd, menu);
			return 0;
		}else if(rem_type & TYPE_PROXYCLIENT){
			sprintf(abuf, "<A HREF=\"http://%s:%s/%s\">%s</A>",
				parse_vec[2], parse_vec[3], parse_vec[1], parse_vec[0]);
		}else {
			sprintf(abuf, "<A HREF=\"gopher://%s:%s/7%s\">%s</A>",
				parse_vec[2], parse_vec[3], parse_vec[1], parse_vec[0]);
		}
		say(sockfd, abuf);
	}else{
		say(sockfd, menu);
	}

	return 0;
}

/* proxy form
 *
 * Test the path to see if it is one of our special formats.
 */

int proxy_form(sockfd, path)
int sockfd;
char *path;
{	char *p, *q;
	char lpath[2];
	int ret;

/* first look for http requests */
	ret = 0;
	if( (ret = http_form(path, &q)) ){
		p = strrchr(q, ' ');
		if( p){
			strncpy(http_protocol, p, 79);
			*p = '\0';
		}
		p = q;
		if( *p == '/')	/* if from normal WWW client */
			p++;
		else
			ret |= TYPE_PROXYCLIENT;
		unesc(rem_path, p);

		strcpy(path, rem_path);	/* copy it back */
		rem_path[0] = '\0';	/* re init since may be dirty */

		ret &= ~(TYPE_TEXT|TYPE_BINARY|TYPE_DIR); /* pjc 10/10/94 */
	}

	copy_with_escapes(orig_request, path, GO_REQ_LEN*3);

	if( !strncasecmp(path, "ptelnet:", 8) ||
	    !strncasecmp(path, "pwais:", 6) ){
		return ret | TYPE_LOCAL;
	}

/* Now look for auth specials */
	if( !strncasecmp(path, "waissrc:", 8)){	/* auth special format? */
		p = &path[8];
		if( !strncasecmp(p, "/user/", 6)){	/* have username */
			p = &p[6];
			q = strchr(p, '\t');
			if( q == NULL)
				return ret;	/* no username */
			*q++ = '\0';
			unesc(rem_path, p);
			strcpy(authuser, q);
			strcpy(path, rem_path);
			rem_path[0] = '\0';
			copy_with_escapes(orig_request, path, GO_REQ_LEN*3);
			ret |= TYPE_LOCAL;

		}else if( !strncasecmp(p, "/pass/", 6)){ /* have user and password */
			p = &p[6];
			q = authuser;
			while(*p && *p != '/' && q != &authuser[127])*q++ = *p++;
			*q = '\0';
			if( *p)
				p++;
			if( ret & TYPE_HTTP)
				q = strchr(p, '?');
			else
				q = strchr(p, '\t');
			if( q == NULL)
				return ret;	/* no password */
			*q++ = '\0';
			unesc(rem_path, p);
			strcpy(path, rem_path);
			rem_path[0] = '\0';

			authenticate(authuser, q);
			if( authenticated){
				copy_with_escapes(orig_request, path, GO_REQ_LEN*3);
			}else{
				unesc(path, orig_request);
			}
			ret |= TYPE_LOCAL;
		}else{
Debug((debugf,"Wais: %s\n", path))
		}
	}

/* Now look for our special formats (URL's) */
	if( !strncmp(path, "gopher://", 9)){
		ret &= ~MASK_BASE;
		p = &path[9];
		q = rem_server;
		while(*p && *p != '/'){
			*q++ = *p++;
		}
		*q = '\0';
		if( *p == '/' && p[1]){
			p++;
			lpath[0] = *p++;	/* skip gopher type */
			lpath[1] = '\0';
			strncpy(rem_path, p, PATH_MAX-1);
			return check_req(sockfd, lpath)|ret;
		}
		return check_req(sockfd, rem_path)|ret;

	}else if( !strncmp(path, "http://", 7)){
		p = &path[7];
		q = rem_server;
		while(*p && *p != '/'){
			*q++ = *p++;
		}
		*q = '\0';
		strcpy(rem_path, p);
		ret |= TYPE_HTTPREQ;

		return check_req(sockfd, rem_path)|ret;

	}else if( !strncmp(path, "ftp://", 6) || !strncmp(path, "file://", 7)){

		ret &= ~MASK_BASE;
		p = strchr(path, '/');
		p += 2;
		q = p;
/* now process host part */
		while(*p && *p != '/')p++;
		if(*p)
			*p++ = '\0';
		strncpy(rem_server, q, MAXHOSTNAMELEN-1);
		strcpy(ftp_server, rem_server);

		if( ftp_proxy[0]){
			strcpy(rem_server, ftp_proxy);
			sprintf(ftp_user,"USER anonymous@%s", ftp_server);
		}else{
			strcpy(ftp_user, "USER anonymous");
		}

		strncpy(&rem_path[2], p, PATH_MAX-1);
		rem_path[1] = '/';

/* from the filename try and guess a reasonable gopher type */
		rem_path[0] = guess_gopher_type(&rem_path[2]);

		sprintf(ftp_pass,"PASS -gopher@%s", ourname);

		return check_req(sockfd, rem_path)|TYPE_FTP|ret;
	}
	return ret;
}


/* Look for HTTP methods... */
struct tag_http {
char	*method;
int	len;
int	type;
} http_methods[] = 
{
	{"GET ",	4, TYPE_READ|TYPE_HTTP},
	{"POST ",	5, TYPE_WRITE|TYPE_HTTP},
	{NULL, 0, 0}
};

int http_form(path, ptr)
char *path, **ptr;
{	struct tag_http *t = http_methods;

	while(t->method != NULL){
		if( strncmp(path, t->method, t->len) == 0){
			*ptr = &path[t->len];
			strncpy(http_method, path, t->len);
			http_method[t->len] = '\0';
			return t->type;
		}
		t++;
	}
	return 0;
}

/* check request
 * 
 * parse the standard format request.
 *
 */

int check_req(sockfd, buf)
int sockfd;
char *buf;
{	char lbuf[2];

	chk_type_ch = buf[0];	/* save it away */
	switch(buf[0]){
	case '\0':
	case '\t':
	case '1':
		return TYPE_DIR;

	case '+':
		return TYPE_PLUS|TYPE_DIR;

	case 'I':
	case '9':
	case 's':
	case '5':
	case 'g':
		return TYPE_BINARY;

	case 'e':
		if(strncmp(buf, "exec:", 5)== 0){
			return TYPE_EXEC;
		}
		return TYPE_UNKNOWN;

	case 'f':
		if(strncmp(buf, "ftp:", 4) == 0){
			return parse_ftp(buf);
		}
		return TYPE_UNKNOWN;

	case 'w':
		if(strncmp(buf, "waissrc:", 8)== 0){
			return TYPE_WAIS;
		}
		if( strncmp(buf, "waisdocid:", 10) == 0){
			return TYPE_WAIS;
		}
		return TYPE_UNKNOWN;

	case '7':
		return TYPE_WAIS|TYPE_DIR;
	
	case 'T':				/* TELNET / TN3270 */
	case '8':
		return TYPE_EXEC;

	case '2':
		return TYPE_EXEC;		/* CSO */

	case '/':
		lbuf[0]= guess_gopher_type(buf);
		lbuf[1]= '\0';
		return check_req(sockfd, lbuf);

	default:
		break;
	}
	return TYPE_TEXT;
}

/* parse_ftp
 *
 * NOTE! This modifies the buffer passed in.
 * It also sets up the following..
 *	ftp_server	the real ftp server.
 *	rem_server	what we are connecting to
 *	ftp_user	USER username  (username@host if proxying)
 *	ftp_pass	PASS -gopher@here
 */

int parse_ftp(path)
char *path;
{	char *p;

	p = index(path, '@');
	if( p == NULL){
		return TYPE_UNKNOWN;
	}
	*p++ = '\0';
	strncpy(rem_server, &path[4], MAXHOSTNAMELEN-1);

	strcpy(path, p);	/* move up whats left */

	if( ftp_proxy[0]){
		sprintf(ftp_user,"USER anonymous@%s",rem_server);
		strcpy(ftp_server, rem_server);
		strcpy(rem_server, ftp_proxy);		/* Use proxy */
	}else {
		sprintf(ftp_user,"USER anonymous");
		strcpy(ftp_server, rem_server);
	}
	sprintf(ftp_pass,"PASS -gopher@%s", ourname);
	return TYPE_FTP|TYPE_DIR;
}

/* This routine checks the request type against the set of permission
 * bits to see if it is ok or not. Used by filter and logging. Can_do
 * is used to check permissions.
 */

int in_perm_set(req_type, set)
int req_type, set;
{	int base_type;

	base_type = req_type;

	if( (base_type & (TYPE_TEXT|TYPE_BINARY|TYPE_READ)))
		return( set & G_READ);

	if( (base_type & (TYPE_DIR)))
		return( set & G_DIR);

	if( (base_type & (TYPE_EXEC)))
		return( set & G_EXEC);

	if( (base_type & (TYPE_WRITE)))
		return( set & G_WRITE);

	if( (base_type & (TYPE_WAIS)))
		return( set & G_WAIS);

	return 0;
}



/* get_port
 *
 * returns the port from a string of the form
 * host:port
 */

int get_port(host,def_port)
char *host;
{	int port;
	char *p;

	port = def_port;
	if((p = rindex(host, ':'))!= NULL){
		*p++ = '\0';
		port = atoi(p);
	}
	return port;
}


/* Some support routines for the FTP part of the proxy
 *
 */

int make_conn(port)
int port;
{	int x;
	int	reuse = 1;
	struct linger linger;
	struct	sockaddr_in	mya;

	(void) signal(SIGALRM, net_timeout);
	(void) alarm(timeout.tv_sec);

	if((x = socket(AF_INET,SOCK_STREAM,0)) < 0) {
		syslog(LLEV,"Socket failed");
		goto broke;
	}
	mya.sin_family = AF_INET;
	bzero(&mya.sin_addr,sizeof(mya.sin_addr));
	mya.sin_port = htons(port);
	if(bind(x,(struct sockaddr *)&mya,sizeof(mya))) {
		syslog(LLEV,"Bind failed");
		goto broke;
	}
	linger.l_onoff = linger.l_linger = 0;
	if( setsockopt(x, SOL_SOCKET, SO_LINGER, (char *)&linger, sizeof(linger)) < 0){
		syslog(LLEV,"setsockopt linger failed");
		goto broke;
	}
	
	if(setsockopt(x, SOL_SOCKET, SO_REUSEADDR, (char *) &reuse, sizeof(reuse)) < 0){
		syslog(LLEV,"setsockopt reuseaddr failed");
		goto broke;
	}
	(void) alarm(0);
	(void) signal(SIGALRM, SIG_IGN);

	return x;

broke:
	return -1;
}

/* sockfd = connection to ftp server */

int port_num(sockfd, rfd, haddr)
int sockfd, rfd;
unsigned char *haddr;
{	struct sockaddr_in serv_addr;
	struct sockaddr_in data_addr;
	int length = sizeof(struct sockaddr_in );
	int port, err;
	unsigned char *addr;
	struct ifreq freq;
	char ifname[17];


	if( (err = getsockname(sockfd, (struct sockaddr *)&serv_addr, &length))!= 0){
		return 0;
	}

	length = sizeof(struct sockaddr_in );
	if( (err = getsockname(rfd, (struct sockaddr *)&data_addr, &length))!= 0){
		return 0;
	}


	port = ntohs(data_addr.sin_port);
	addr = (unsigned char *)&serv_addr.sin_addr;
	haddr[0] = addr[0];
	haddr[1] = addr[1];
	haddr[2] = addr[2];
	haddr[3] = addr[3];
	return port;
}


char * port_cmd_for(sockfd, rfd)
int rfd,sockfd;
{	static char portbuf[80];
	int port;
	unsigned char haddr[4];

	portbuf[0] = '\0';

	port= port_num(sockfd, rfd, haddr);
	if( port == 0)
		return portbuf;

	sprintf(portbuf,"PORT %d,%d,%d,%d,%d,%d",
		(((int)haddr[0])&0xff),(((int)haddr[1])&0xff),
		(((int)haddr[2])&0xff),(((int)haddr[3])&0xff),
		(port/256)&0xff, port&0xff);
	return portbuf;
}

static char ftp_reply_buf[1024];

int	get_ftp_reply(rfd)
int rfd;
{	struct timeval timeout;
	fd_set	rdy;
	int cnt;

	sprintf(ftp_reply_buf,"501 Unknown ftp problem");

	cnt = getline(rfd, ftp_reply_buf, sizeof(ftp_reply_buf)-1);

	if( cnt <= 0)
		return '5';		/* code 5 is error? */

/* Look for continuation lines... */
	strncpy(errbuf, ftp_reply_buf, 5);
	while(errbuf[0]== ' ' || errbuf[3] == '-'){
		timeout.tv_sec = 0;
		timeout.tv_usec= 100000l;

		FD_ZERO(&rdy);
		FD_SET(rfd, &rdy);
		if( select(32,&rdy, (fd_set *)0, (fd_set *)0, &timeout) <= 0)
			break;
		getline(rfd, errbuf, sizeof(errbuf)-1);
	}
	return ftp_reply_buf[0];
}


/* Translate the ftp type reply into a gopher type reply
 */

int trans_ftp_reply(stem, buf)
char *stem;
char *buf;
{	char path[PATH_MAX];
	char x;

	strncpy(path, buf, PATH_MAX-1);
	path[PATH_MAX-1] = '\0';

	if( stem == NULL)
		stem = "";

	x = guess_gopher_type(path);

	if( *stem && *stem != '/' && stem[1] == '/')
		stem++;			/* skip the gopher type character */
	if( *stem == '/')
		stem++;

	if( ftp_proxy[0]){
		sprintf(buf,"%c%s\t%c/%s%s\t%s\t%u", x, path, x, stem, path, ftp_proxy, ftp_proxy_port);
	}else{
		sprintf(buf,"%c%s\t%c/%s%s\t%s\t%u", x, path, x, stem, path, ftp_server, rem_port);
	}
	return 0;
}


/* ftp_login
 *
 * returns a string of the form
 * [user[:password]@]hostname[:port]
 * as taken from the WWW rfc1630.
 */
char *ftp_login(host, port)
char *host,*port;
{
	static char login_buf[MAXHOSTNAMELEN+80];

	if( port == NULL)
		port = "";
	if( *port && strcmp(port, "21")){
		sprintf(login_buf,"%s:%s", host, port);
	}else{
		strcpy(login_buf, host);
	}
	return login_buf;
}

int ftp_setup(sockfd, rfd, ftp_listen)
int sockfd, rfd, *ftp_listen;
{
	if( get_ftp_reply(rfd) != '2')
		goto broken;

	say(rfd, ftp_user);
	if( get_ftp_reply(rfd) != '3')
		goto broken;

	say(rfd, ftp_pass);
	if( get_ftp_reply(rfd) != '2')
		goto broken;

	*ftp_listen = make_conn(0);
	if( *ftp_listen < 0)
		goto broken;

	if( listen(*ftp_listen, 2))
		goto broken;
	say(rfd, port_cmd_for(rfd, *ftp_listen));
	if( get_ftp_reply(rfd) != '2')
		goto broken;

	return 0;
broken:
	return 1;
}


/* forward_dir
 *
 * pass on a basic gopher menu (directory ) request.
 * and translate the reply.
 *
 * The routine is mostly the code to talk to an FTP server/proxy
 */


int forward_dir(sockfd, buf, host)
int sockfd;
char *buf, *host;
{	int port;
	char *p;
	int ftp_control, ftp_listen, ftp_data;
	struct sockaddr_in serv_addr;
	int length = sizeof( serv_addr);
	char ftp_command[PATH_MAX+40];

	ftp_control = ftp_listen = ftp_data = -1;

	port = def_port;
	if( port == 0)
		port = GOPORT;
	if( rem_type & TYPE_FTP)
		port = FTPPORT;
	else if( rem_type & TYPE_HTTPREQ)
		port = HTTPPORT;

	port = get_port(host, port);
	rem_port = port;

	Debug( (debugf,"forward: %s to %s (%u)\n", buf, host, port))

	/* 20-oct-94 */
	if( (!strncasecmp(buf,"waissrc:", 8) || !strncmp(buf, "7/", 2))&& (rem_type&TYPE_HTTP)){
		p = index(buf, '?');
		if( p ){
			Debug((debugf, "query=%s\n", p+1))
			*p = '\t';
		}else{
Debug((debugf,"return INDEXABLE html\n"))
			starthtml(sockfd," Searchable Index");
			sprintf(go_request,"<ISINDEX>");
			say(sockfd,go_request);
			return 0;
		}
	}

	if( (rfd = conn_server(host, port, 0, errbuf)) < 0){
		go_error(sockfd, 404, "Requested Information not available");
		go_error(sockfd, 404, "failed to connect to server %s (%d)", host, port);
		syslog(LLEV,"failed to connect to server %s (%d)", host, port);
		return 0;
	}

	if( rem_type & TYPE_FTP){	/* Ok do the FTP startup stuff */
		if( ftp_setup(sockfd, rfd, &ftp_listen))
			goto broken;

/* ok, now send the NLST -LF command */
		ftp_control = rfd;
		p = rem_path;			/* strip gopher type char if it is there */
		if( *p != '/' && p[1] == '/')
			p++;
		sprintf(ftp_command,"NLST -LF %s", p);
		say(ftp_control, ftp_command);
		if( get_ftp_reply(ftp_control) != '1')
			goto broken;

		ftp_data = accept(ftp_listen, (struct sockaddr *)&serv_addr, &length);
		close(ftp_listen);
		if( ftp_data < 0){
			close(ftp_control);
			goto broken;
		}
		rfd = ftp_data;
	}else {
		say(rfd,buf);

		if(rem_type & TYPE_PLUS){
			if(rem_type & TYPE_WRITE){
				if( getline(sockfd, go_request, GO_REQ_LEN-1) < 0){
					goto broken;
				}
				Debug((debugf,"PLUS: %s\n", go_request))
				if( can_do(sockfd, rem_type)){
					if( putblock(sockfd, rfd, go_request))
						goto broken;
				}else {
					putblock(sockfd, -1, go_request);
					goto broken;
				}
			}
		}
	}

/* OK now process the reply */
	if( !translate_reply(sockfd, rfd)){
		goto broken;
	}

	return 1;
broken:
	go_error(sockfd, 404, "Requested Information not available");
	return 0;
}


int forward_file(sockfd, buf, host)
int sockfd;
char *buf, *host;
{	int port;
	char *p;
	int ftp_control, ftp_listen, ftp_data;
	struct sockaddr_in serv_addr;
	int length = sizeof( serv_addr);
	char ftp_command[PATH_MAX+40];
	char gt[2];

	ftp_control = ftp_listen = ftp_data = -1;

	port = GOPORT;
	if( rem_type & TYPE_FTP)
		port = FTPPORT;
	else if( rem_type & TYPE_HTTPREQ)
		port = HTTPPORT;

	port = get_port(host, port);
	rem_port = port;

Debug( (debugf,"forward file: %s from %s (%u)type\n", buf, host, port))
Debug((debugf,"type=%x (%c)\n", rem_type, rem_typech))

	if( (rfd = conn_server(host, port, 0, errbuf)) < 0){
		go_error(sockfd, 404, "Requested Information not available");
		go_error(sockfd, 404, "failed to connect to %sserver %s (%d)",(rem_type & TYPE_FTP)? "ftp ":"", host, port);
		syslog(LLEV,"failed to connect to server %s (%d)", host, port);
		return 0;
	}

	if( rem_type & TYPE_FTP){	/* Ok do the FTP startup stuff */

		if( ftp_setup(sockfd, rfd, &ftp_listen))
			goto broken;

		ftp_control = rfd;

		gt[0] = guess_gopher_type(rem_path);
		gt[1] = '\0';
		if( check_req(sockfd, gt) & TYPE_BINARY){
			say(rfd,"TYPE I");
			if( get_ftp_reply(rfd) != '2')
				goto broken;
		}

		rem_typech = chk_type_ch;
		p = rem_path;			/* strip gopher type char if it is there */
		if( *p != '/' && p[1] == '/')
			p++;
		sprintf(ftp_command,"RETR %s", p);
		say(ftp_control, ftp_command);
		if( get_ftp_reply(ftp_control) != '1')
			goto broken;

		ftp_data = accept(ftp_listen, (struct sockaddr *)&serv_addr, &length);
		close(ftp_listen);
		if( ftp_data < 0){
			close(ftp_control);
			goto broken;
		}
		rfd = ftp_data;
	}else {
		say(rfd,buf);

		if(rem_type & TYPE_PLUS){
			if(rem_type & TYPE_WRITE){
				if( getline(sockfd, go_request, GO_REQ_LEN-1) < 0){
					goto broken;
				}
				if( can_do(sockfd, rem_type)){
					if( putblock(sockfd, rfd, go_request))
						goto broken;
				}else {
					putblock(sockfd, -1, go_request);
					goto broken;
				}
			}
		}
	}

	if( rem_typech == 'h'){	/* assume html so needs translation */
		trans_html_file(sockfd, rfd, "ftp", -1);
	}else{
		copyfiletype(sockfd, rfd);
	}

	return 1;

broken:
	return 0;
}


int forward_http(sockfd, buf, host)
int sockfd;
char *buf, *host;
{	int port, n, cnt;
	char *p;
	struct sockaddr_in serv_addr;
	int length = sizeof( serv_addr);

	port = HTTPPORT;

	port = get_port(host, port);
	rem_port = port;

	if( (rfd = conn_server(host, port, 0, errbuf)) < 0){
		go_error(sockfd, 404, "Requested Information not available");
		go_error(sockfd, 404, "failed to connect to http server %s (%d)", host, port);
		syslog(LLEV,"failed to connect to http server %s (%d)", host, port);
		return 0;
	}
	
	if( http_method[0] == '\0')
		strcpy(http_method, "GET ");

	if( permissions & G_PLUS){
		sprintf(go_request,"%s%s%s", http_method, buf, http_protocol);
	}else{
		sprintf(go_request,"%s%s", http_method, buf);
	}
	say(rfd, go_request);

	if( http_protocol[0]){	/* Was there http protocol info? */
		int saved = sockfd;
		int hlen = 0;

		if( (permissions & G_PLUS) == 0)
			sockfd = -1;
 		if( copy_http_headers(rfd, sockfd, (rem_type&TYPE_WRITE), &hlen) &&
			(rem_type&TYPE_WRITE)){
			sockfd = saved;
			trans_html_file(rfd, sockfd, "http", hlen);
		}
		if( sockfd == -1){
			go_error(sockfd,403,"Write operation not allowed!");
			return 0;
		}
		sockfd = saved;
	}

/* Get possible response line (maybe an old server)*/
	if( get_http_response(sockfd, rfd)){

		if( copy_http_headers(sockfd, rfd, 1, NULL)){
			trans_html_file(sockfd, rfd, "http", -1);
		}
	}else{
		/* guess document type from extension */
		if( rem_typech == 'h' || rem_typech == 'H' || rem_typech == '1'){
			trans_html_file(sockfd, rfd, "http", -1);
		}else{
			while( 1){
				cnt = GO_REQ_LEN-1;
				if( (n = read(rfd, go_request, cnt)) > 0){
					inbytcnt += n;
					if( n != write(sockfd, go_request, n))
						goto broken;
				}else
					break;
			}
		}
	}

	return 1;

broken:
	return 0;
}

int copy_http_headers(sockfd, rfd,expect_data, lptr)
int sockfd, rfd;
int expect_data, *lptr;
{	int len, cnt;
	char *p;
	int n;
	static char content_len[80];
	static char content_type[80];

	content_len[0] = '\0';
	content_type[0]= '\0';

	if( lptr)
		*lptr = 0;

	while(1){
		if( getline(rfd, go_request, GO_REQ_LEN-1) < 0){
			goto broken;
		}
		if( go_request[0] == '\0'){
			break;
		}
		if( !strncasecmp(go_request, "content-length:", 15)){
			strncpy(content_len, go_request, 79);
			content_len[79] = '\0';
			if( lptr){
				*lptr = atoi(content_len);
			}
		}else if( !strncasecmp(go_request, "content-type:", 13)){
			strncpy(content_type, go_request, 79);
			content_type[79] = '\0';
			if( sockfd != -1)say(sockfd, go_request);
		}else if( !strncasecmp(go_request, "location:", 9) ){
			if( (rem_type&TYPE_PROXYCLIENT)==0){
				p = strchr(go_request, ':');
				p++;
				while(*p == ' ' || *p == '\t')p++;
				sprintf(errbuf, "Location: http://%s:%u/%s", ourname, ourport, p);
				if( sockfd != -1)say(sockfd, errbuf);
			}else{
				if( sockfd != -1)say(sockfd, go_request);
			}
		}else {
			if( sockfd != -1)say(sockfd, go_request);
		}
	}
	len = -1;
	if( content_len[0]){
		p = strchr(content_len, ':');
		if(p ){
			p++;
			while(*p == ' ' || *p == '\t')p++;
			len = atoi(p);
		}
	}
	if( lptr){
		*lptr = len;
	}
	if( content_type[0] ){
		p = strchr(content_type, ':');
		if( p == NULL){
			return 1;
		}
		p++;
		while(*p == ' ' || *p == '\t')p++;
		if( !strncasecmp(p, "text/html", 9)){
			goto ishtml;
		}
/* not an html file? so just block copy */
		if( content_len[0]){
			if( sockfd != -1)say(sockfd,content_len);
		}
		if( sockfd != -1)say(sockfd,"");

		if(expect_data){
			while( len){
				cnt = GO_REQ_LEN-1;
				if( len > 0 && cnt > len)
					cnt = len;
				if( (n = read(rfd, go_request, cnt)) > 0){
					inbytcnt += n;
					if( sockfd != -1)write(sockfd, go_request, n);
					len -= n;
				}else
					break;
			}
		}
		return 0;
	}
ishtml:
	if( lptr){	/* its a POST so need content-length: */
		if(content_len[0] && sockfd != -1)say(sockfd, content_len);
	}
	if( sockfd != -1)say(sockfd,"");
	return 1;

broken:
	return 0;
}

/* Look for the http response line. */

int get_http_response(sockfd, rfd)
int sockfd, rfd;
{	int	x = 0;
	int	ret = -1;
	char 	*p;

	(void) signal(SIGALRM, net_timeout);
	(void) alarm(timeout.tv_sec);
	p = go_request;
	while(1) {
		if(read(rfd, p,1) != 1)
			goto done;
		if(write(sockfd, p, 1) != 1)
			goto done;
		if( x == GO_REQ_LEN)
			goto done;
		if( (*p & 0x80) )
			goto done;
		if( x > 0 && *p == '\n')
			break;
		p++;
		x++;
	}
	if( strncasecmp(go_request, "http", 4)){
		goto done;
	}
/* ok it is most likely to be a response line */

	p[-1] = '\0';
	(void) alarm(0);
	(void) signal(SIGALRM, SIG_IGN);
	return 1;

/* Come here on error or odd response line */
done:
	go_request[60] = '\0';
	(void) alarm(0);
	(void) signal(SIGALRM, SIG_IGN);
	return 0;
}

/* translate_reply
 *
 * translate returned gopher items into new format!
 * use gopher://xxx or ftp://xxx forms depending on whether
 * we used the gopher or ftp protocols.
 *
 * uses:	global variables rem_type, rem_port, ourname, ourport.
 * modifies:	go_request
 */

int translate_reply(sockfd, rfd)
int sockfd, rfd;
{	char **parse_vec;
	static char new_reply[GO_REQ_LEN];
	static char xbuf[80];
	char *p;
	int i,type_rep;
	int port;
	static char ftp_port_str[10];

	port = rem_port;

	gostartmenu(sockfd);
	while(1){
		if( getline(rfd, go_request, GO_REQ_LEN-1) < 0){
			break;
		}
		if( rem_type & TYPE_FTP){
			trans_ftp_reply(rem_path, go_request);
		}
		if( go_request[0] == ' ' ||
			go_request[0] == '.'){
			say(sockfd, go_request);
			continue;
		}
		parse_vec = reply_parse(go_request);
		if( parse_vec == NULL){
			Debug((debugf,"Failed to parse reply: %s\n", go_request))
			say(sockfd, go_request);
			continue;
		}
		type_rep = check_req(sockfd, parse_vec[0]);

		if( in_perm_set(type_rep, filter_replies)){
			continue;
		}
/* If the path points to a ftp: type then we want to translate it into an ftp URL */
		if( !strncasecmp(parse_vec[1], "ftp:", 4) || 
		    !strncasecmp(&parse_vec[1][1], "ftp:", 4)){
			type_rep |= TYPE_FTP;
			p = index(&parse_vec[1][1], ':');
			p++;
			parse_vec[2] = p;	/* the new host name */
			p = index(p, '@');
			if( p == NULL)
				p = "/";
			else
				*p++ = '\0';
			parse_vec[1] = p;	/* the new path */

			i = get_port(parse_vec[2], FTPPORT);
			sprintf(ftp_port_str, "%u", i);
			parse_vec[3] = ftp_port_str;
		}

		if( authenticated){
			parse_vec[1] = add_auth(parse_vec[1]);
		}

		if( (rem_type & TYPE_FTP) || (type_rep & TYPE_FTP) ){
			p = parse_vec[1];	/* skip the type char and / */
			if( *p != '/' && p[1] == '/')
				p++;
			if( *p == '/')
				p++;

			sprintf(new_reply,"%s\tftp://%s/%s\t%s\t%u",
				parse_vec[0], 
				ftp_login(parse_vec[2], parse_vec[3]),
				p,
				ourname, ourport);
		}else {
			sprintf(new_reply,"%s\tgopher://%s:%s/%c%s\t%s\t%u",
				parse_vec[0], parse_vec[2],
				parse_vec[3], parse_vec[0][0],
				parse_vec[1],
				ourname, ourport);
		}
		p = new_reply;
		while(*p)p++;

		for(i=4; parse_vec[i]; i++){
			*p++ = '\t';
			strcpy(p, parse_vec[i]);
			while(*p)p++;
		}
/* now apply any html mods to the new selector */
		if( (rem_type & (TYPE_HTTP))==TYPE_HTTP){
			parse_vec = reply_parse(new_reply);
			if( parse_vec){
				if( rem_type & TYPE_PROXYCLIENT){
					switch(parse_vec[0][0]){
					case '8':
						{	char *p2;

							p2 = index(parse_vec[1],':');
							if( p2 && can_plug){
							    	sprintf(go_request,
									"<LI><A HREF=\"ptelnet%s\">%s</A>",
									p2, &parse_vec[0][1]);
								say(sockfd, go_request);
							}else{
							    	sprintf(go_request,
									"<LI>%s",
									&parse_vec[0][1]);
								say(sockfd, go_request);
							}
						}
						break;
					default:
					    	sprintf(go_request,
							"<LI><A HREF=\"%s\">%s</A>",
							parse_vec[1], &parse_vec[0][1]);
						say(sockfd, go_request);
						break;
					}
				}else{
				    int typech;
 
				    p = parse_vec[1];
				    if( !strncasecmp(p, "gopher", 6) ||
					!strncasecmp(p, "ftp", 3) ||
					!strncasecmp(p, "file", 4)){
					typech = gopher_type(p);;
					sprintf(go_request, 
					"<LI><A HREF=\"gopher://%s:%s/%c%s\">%s</A>", 
					parse_vec[2], parse_vec[3],typech, 
					parse_vec[1], &parse_vec[0][1]);
				    }else{
					sprintf(go_request, 
					"<LI><A HREF=\"http://%s:%s/%s\">%s</A>", 
					parse_vec[2], parse_vec[3], 
					parse_vec[1], &parse_vec[0][1]);
				    }
				    say(sockfd, go_request);
				}
			}
		}else{
			say(sockfd, new_reply);
		}
	}
	goendmenu(sockfd);
	return 1;
}



/* copyfiletype
 */

int copyfiletype(sockfd, rfd)
int sockfd, rfd;
{	int pre = 0;
	int	cnt;

	(void) signal(SIGALRM, net_timeout);
	(void) alarm(timeout.tv_sec);
	if( (rem_type & (TYPE_HTTP|TYPE_TEXT)) == (TYPE_HTTP|TYPE_TEXT)){
		gostarthtml(sockfd);
		pre = 1;
	}
	while(1){
		
		cnt = read(rfd, errbuf, sizeof(errbuf));
		if( cnt == 0)
			break;
		if( cnt < 0)
			goto broke;
		if(write(sockfd, errbuf, cnt) != cnt){
			goto broke;
		}
		inbytcnt += cnt;
	}
	if( pre ){
		goendhtml(sockfd);
	}

	(void) alarm(0);
	(void) signal(SIGALRM, SIG_IGN);
	return 0;
broke:
	syslog(LLEV,"connection broke");
	http_exit(1);
	return 0;
}

/* If list of valid destinations exist then check the destination */

int check_dest(sockfd)
int sockfd;
{	char **xp;
	int x;
	char *p;

	if( validests != NULL){
		p = rem_server;
		while(*p && *p!= ':')p++;
		if( *p == ':'){
			*p = '\0';
		}else
			p = NULL;

		for(xp = validests; *xp != NULL; xp++){
			if( **xp == '!' && hostmatch(*xp+1, rem_server)){
				goto denied;
			}else{
				if(hostmatch(*xp, rem_server))
					break;
			}
		}
		if( *xp == NULL){
			goto denied;
		}
		if(p)
			*p = ':';	/* put the colon back */
	}

	return 0;

denied:
	go_error(sockfd, 403, "Access denied to host %s", rem_server);
	syslog(LLEV,"deny host=%s/%s connect to %s", rladdr, riaddr, rem_server);
	return 1;
}


/* check request type against permissions */
struct req_perm {
int	type;
int	perm;
char	*msg;
}req_perm_table[] = 
{	
	{	TYPE_DIR,	G_DIR,	"Not permitted to list directories"},	
	{	TYPE_TEXT,	G_READ, "Not permitted to read files"},		
	{	TYPE_BINARY,	G_READ, "Not permitted to read files"},		
	{	TYPE_EXEC,	G_EXEC, "Not permitted to exec commands"},
	{	TYPE_WAIS,	G_WAIS,	"Not permitted to do wais commands"},	
	{	TYPE_PLUS,	G_PLUS, "Not permitted to do Gopher+ requests"},
	{	TYPE_FTP,	G_FTP,	"Not permitted to do ftp requests"},
	{	TYPE_HTTP,	G_HTTP,	"Not permitted to do http requests"},
	{	TYPE_GOPHER,	G_GOPHER,"Not permitted to do gopher requests"},
	{	TYPE_WRITE,	G_WRITE,"Not permitted to write data"},
	{	TYPE_UNKNOWN,	G_NOPERM,"Not permitted to perform request"},		
	{	0,		G_NOPERM,"OOPs, proxy error! contact <pjc@tis.com>"},		
};


int can_do_sub(sockfd, req)
int sockfd, req;
{	int x;

	for(x=0; x < sizeof req_perm_table;x++){
		if( (req_perm_table[x].type & req) == req_perm_table[x].type)
			break;
	}
	if(permissions & req_perm_table[x].perm){
		if( auth_funcs & req_perm_table[x].perm){
			if( is_authenticated(sockfd, req)){
				return 1;
			}
			return 0;
		}else{ 
			return 1;
		}
	}
/* Not allowed to do it. */
	if( req_perm_table[x].msg == NULL){
		can_do(sockfd, TYPE_UNKNOWN);
		return 0;
	}

	go_error(sockfd, 403, "%s", req_perm_table[x].msg);
	return 0;
}

int can_do(sockfd, req)
int sockfd, req;
{	int x;

	x = req & ~MASK_BASE;

	if( x && !can_do_sub(sockfd, x)){
		return 0;
	}
	return can_do_sub(sockfd, req & MASK_BASE);
}

/* guess_gopher_type
 */
struct ext_table {
char *ext;
int type;
}ext_tab[] = 
{
	{ ".gif", 'g'},
	{ ".zip", '5'}, { ".zoo", '5'}, { ".arj", '5'}, { ".arc",'5'}, { ".lzh", '5'},	/* common dos archive formats */
	{ ".exe", '9'}, { ".com", '9'}, { ".dll", '9'}, { ".lib",'9'}, { ".sys",'9'},	/* Dos- windows extensions... */
	{ ".jpg", 'I'}, { ".jpeg", 'I'}, {".pict", 'I'}, {".pct",'I'},
	{ ".tiff",'I'}, { ".tif", 'I'}, { ".pcx", 'I'},
	{ ".tar", '9'}, { ".z", '9'}, { ".gz", '9'},
	{ ".hqx", '4'},		/* MAC binhex */
	{ ".au", 's' }, { ".snd", 's'}, { ".wav", 's'},	/* sounds */
	{ ".doc", '9'}, { ".wri", '9'},
	{ ".html", 'h'},{ ".htm", 'h'},	/* its html */
	{ ".ps", 'I'},	/* for postscript use binary mode */
	{ ".txt", 'I'},	/* let NCSA Mosaic add the html wrapping */
	{ NULL,	'0'}	/* This must be the last entry */
};

int guess_gopher_type(path)
char *path;
{	char *p;
	char *ext;
	struct ext_table *t;

	if( *path == '\0')
		return '1';		/* null so must be a dir*/

	p = rindex(path,'/');
	if( p != NULL && p[1] == '\0')
		return '1';		/* It is a directory */
/* check for well known file extensions... */
	if( p == NULL)
		p = path;
	ext = rindex(p, '.');
	if( ext != NULL){
		for(t = ext_tab; t->ext;t++)
			if( !strcasecmp(t->ext, ext))
				break;
		return t->type;
	}
	return '0';
}

/* return a gopher type character given a URL type string.
 * used by the html URL translation stuff 
 */
struct gopher_type_rec {
char *proto;
int	len;
int	func;
char	type;
} gopher_type_tab[] = {
{ "gopher://",	9, 0, '1'},
{ "ftp://",	6, 2, '1'},
{ "file://",	7, 2, '1'},
{ "telnet://",	9, 1, '8'},
{ "ptelnet://",	10, 1, '8'},
{ NULL,		0, 0, '0'}
};

int gopher_type(buf)
char *buf;
{	int typech = '0';
	char *p;
	struct gopher_type_rec *t = gopher_type_tab;


	while(t && t->proto != NULL){
		if( !strncasecmp(buf, t->proto, t->len)){
			switch(t->func){
			default:
			case 0:
				p = &buf[t->len];
				p = strchr(p, '/');
				if( p){
					if( p[1])
						typech = p[1];
					else
						typech = t->type;
				}
				return typech;

			case 1:
				return t->type;
			
			case 2:
				p = &buf[t->len];
				p = strchr(p, '/');
				if( p)
					p++;
				typech = guess_gopher_type(p);
				return typech;
			}
		}
		t++;
	}

	return typech;
}




/* check plus
 *
 * check the selector for Gopher+ forms
 */

int check_plus(sockfd, buf, type)
int sockfd;
char *buf;
int type;
{	char *p;
	
	p = index(buf, '\t');
	if( p){
		if(rem_typech == '7' || rem_typech == 'w' || rem_typech == 'W')
			p = index(&p[1], '\t');
	}
	if( p){
		if( *p)
			p++;
		Debug((debugf,"Plus stuff: %s\n", p))
		type |= TYPE_PLUS;
		
/* look for the more data flag */
		p = index(p, '\t');
		if( p && p[1] == '1')
			type |= TYPE_WRITE;
	}
	
	return type;
}


/* put block
 *
 * Put a gopher+ block.
 *
 * eob specifies how the block will be terminated.
 */

int putblock(sockfd, rfd, eob)
int sockfd, rfd;
char *eob;
{	int	length;		/* length or -1 or -2 */

	say(rfd, eob);		/* pass on the length stuff */
	if(*eob == '+')
		eob++;
	if(*eob == '-')
		length = -atoi(&eob[1]);
	else
		length = atoi(eob);

	if( length > 0){
		while(length){
			int cnt = length;
			
			if( cnt > GO_REQ_LEN)
				cnt = GO_REQ_LEN-1;
			cnt = read(sockfd, go_request, cnt);
go_request[cnt+1] = '\0';
			if( cnt <= 0)
				break;
			if( rfd != -1){		/* allow write? */
				if( write(rfd, go_request, cnt) != cnt)
					break;
				outbytcnt += cnt;
			}
			length -= cnt;
		}
	}else {

		while(1){
			if( getline(sockfd, go_request, GO_REQ_LEN-1) < 0)
				break;
			if( rfd != -1){
				say(rfd, go_request);
				outbytcnt += strlen(go_request)+2;
			}
			if( go_request[0] == '.' && go_request[1] == '\0')
				break;
		}
	}
	return 0;
}

/* some HTTP specific stuff */
int alpha(ch)
int ch;
{
	if( (ch >= 'a' && ch <= 'z') ||
	    (ch >= 'A' && ch <= 'Z'))
		return 1;
	return 0;
}

int alphanum(ch)
int ch;
{
	if( alpha(ch))return 1;
	if( ch >= '0' && ch <= '9')return 1;
	return 0;
}



/* unesc
 *
 * remove the %xx escapes from the selector and strip off
 * the HTTP/version string.
 */

int unesc(t, f)
char *t, *f;
{	 int n;
	char ch;
	char *p, *st = t;

	while(*f){
		if( *f == '%'){
			if(f[1] == '\0' || f[2] == '\0'){	/* Broken URL */
				while(*t++ = *f++);	/* copy across whats left */
				break;
			}
			ch = f[1];
			if( ch >= '0' && ch <= '9')
				n = 16*(ch-'0');
			else if( ch >= 'A' && ch <= 'F')
				n = 16*(ch-'A'+10);
			else if( ch >= 'a' && ch <= 'f')
				n = 16*(ch-'a'+10);
			else { /* not valid hex escape */
				while(*t++ = *f++);
				break;
			}
			ch = f[2];
			if( ch >= '0' && ch <= '9')
				n += ch-'0';
			else if( ch >= 'A' && ch <= 'F')
				n += ch-'A'+10;
			else if( ch >= 'a' && ch <= 'f')
				n += ch-'a'+10;
			else {
				while(*t++ = *f++);
				break;
			}
			*t++ = n;
			f = &f[3];
		}else {
			*t++ = *f++;
		}
	}

	return 0;
}

/* Translate an html file */

int trans_html_file(sockfd, rfd, protocol, len)
int sockfd,rfd, len;
char *protocol;
{	int cnt;
	int ch;
	char buf[2];
	static char newurl[1025];
	static char element[80];	/* name of this element */
	char *p;


	cnt = 0;
	buf[1] = '\0';
	while(rfd != -1){
		if( read(rfd, buf, 1) <= 0)
			break;	
		inbytcnt++;
		cnt++;
		if( cnt == len)
			break;
		ch = buf[0];
		write(sockfd, buf, 1);
		if( ch == '<'){
			if( read(rfd, buf, 1) <= 0)
				break;
			inbytcnt++;
			cnt++;
			if( cnt == len)
				break;
			ch = buf[0];
			write(sockfd, buf, 1);
			if( !alpha(ch)){
				continue;
			}
			p = element;
			*p++ = ch;
			*p = '\0';
			while(1){
				if( read(rfd, buf, 1) <= 0){
					break;
				}
				inbytcnt++;
				cnt++;
				if( cnt == len)
					break;
				ch = buf[0];
				write(sockfd, buf, 1);
				if( !alpha(ch))
					break;
				*p++ = ch;
				*p = '\0';
				if( p == &element[79])
					break;
			}
			if( strcasecmp(element, "a") &&
			    strcasecmp(element, "img") &&
			    strcasecmp(element, "form") ){
				/* dont process this one. */
				continue;
			}
/* either an anchor 'A' or image 'IMG' or FORM 'FORM' */
			p = newurl;
			*p = '\0';

/* get the anchor attributes */
			while(1){
				if( read(rfd, buf, 1) <= 0){
					rfd = -1;
					break;
				}
				cnt++;
				if( cnt == len)
					break;
				inbytcnt++;
				ch = buf[0];
				if( ch == '>' || p == &newurl[1023]){
					trans_anchor(sockfd, newurl, protocol);
					write(sockfd, ">", 1);
					break;
				}
				*p++ = ch;
				*p = '\0';
			}
		}
	}
	return cnt;
}


/* some simple parsing stuff */

char *skipws(buf)
char *buf;
{	while(*buf == ' ' || *buf == '\t')buf++;
	return buf;
}

int endofurl(ch, delim)
int ch,delim;
{
	if( ch == delim)
		return 1;

	if( ch == ' ' || ch == '\t')
		return 1;
	if( ch == '\r' || ch == '\n')
		return 1;
	return 0;
}

/* Translate an anchor */
/* has to deal with relative
 * and absolute forms...
 *
 * Don't change xxx://yyy 
 */
static char newbuf[1024];

char *new_anchor(buf, protocol)
char *buf;
char *protocol;
{
	char *p, *q, *path,*path2;
	int cnt, ch, len, ch2;

	p = strchr(buf,':');
	if( p ){
		sprintf(newbuf, "%s", buf);
		return newbuf;
	}

/* not an absolute anchor */
	if( !strncmp(buf, "//", 2)){
		sprintf(newbuf, "%s:%s", protocol, buf);
		return newbuf;
	}

	if( buf[0] == '/'){
		sprintf(newbuf,"%s://%s:%u%s", protocol, rem_server, rem_port, buf);
		return newbuf;
	}

	p = strrchr(rem_path, '/');
	if( p){
		p++;
		ch = *p;
		*p = '\0';
		sprintf(newbuf, "%s://%s:%u%s%s", protocol, rem_server, rem_port, rem_path, buf);
		*p = ch;
		return newbuf;
	}

	return buf;
}

/* maybe_plug_it *
 * see if this URL needs to use plug gateway interface.
 */
struct plug_tab_rec {
char	*proto;
int	len;
int	func;
} plug_tab[] =
{	{ "telnet:", 7, 0},
	{ NULL, 0, 0}
};


char *maybe_plug_it(oldurl)
char *oldurl;
{	char *ret;
	struct plug_tab_rec *t = plug_tab;

	ret = oldurl;
	while( can_plug && t && t->proto != NULL){
		if( !strncasecmp(oldurl, t->proto, t->len)){
			switch(t->func){
			case 0:
			default:
				sprintf(newbuf,"p%s", oldurl);
				break;
			}
			return newbuf;
		}
		t++;
	}

	return ret;
}

int filter_anchor(attrib, oldurl)
char *attrib, *oldurl;
{	int ret = 0;
	char abuf[2];
	int url_type;

	abuf[0] = gopher_type(oldurl);
	abuf[1] = '\0';

	url_type = check_req(0, abuf);

	if( in_perm_set(url_type, filter_replies)){
Debug((debugf,"Remove it...\n"))
		return 1;
	}
	return ret;
}

char *no_touch_tab[] = {
"internal-gopher-menu",
"internal-gopher-binary",
NULL
};

int dont_touch(url)
char *url;
{	char **t = no_touch_tab;

	while(*t){
		if( !strcasecmp(url, *t))
			return 1;
		t++;
	}
	return 0;
}



/* Translate attributes that need to be translated 
 * also inserts the auth stuff if needed.
 */

int trans_anchor(sockfd, buf, protocol)
int sockfd;
char *buf;
char *protocol;
{	int quoted = 0, delim, ch, typech;
	char *p;
	char *buf2, *oldurl;
	static char newurl[1024];
	static char attrib[80];

	while(*buf){
		while(*buf == ' '){
			write(sockfd, buf, 1);
			buf++;
		}
		if( !quoted && 
			(!strncasecmp(buf, "href", 4) || 
			 !strncasecmp(buf, "src", 3) ||
			 !strncasecmp(buf, "action", 6))
				){
			buf2 = buf;
			p = attrib;
			while(alpha(*buf)){
				*p++ = *buf++;
				*p = '\0';
				if( p == &attrib[79])
					break;
			}
			buf= skipws(buf);
			if( *buf++ != '='){ buf = buf2; goto bad; }
			buf = skipws(buf);
			if( *buf == '"'){
				delim = '"';
				buf++;
			}else{
				delim = '\0';
			}
			oldurl = buf;
			while(!endofurl(*buf, delim))buf++;
			ch = *buf;
			*buf = '\0';
/* Now have the URL in buf*/
			oldurl = maybe_plug_it(oldurl);
			if( filter_anchor(attrib, oldurl)){
				sprintf(newurl,"%s=\"filtered://-removed-\"", attrib);
			}else if( dont_touch(oldurl)){
				sprintf(newurl,"%s=\"%s\"", attrib, oldurl);
			}else if( rem_type & TYPE_PROXYCLIENT){
				if( authenticated){
					oldurl = add_auth_to_url(oldurl);
				}
				sprintf(newurl,"%s=\"%s\"", attrib, oldurl);
			}else{
				p = new_anchor(oldurl, protocol);
				if( authenticated){
					p = add_auth_to_url(p);
				}
				if( !strncasecmp(p, "gopher", 6) ||
				    !strncasecmp(p, "ftp", 3) ||
				    !strncasecmp(p, "file", 4)){
					typech = gopher_type(p);;
					sprintf(newurl,"%s=\"gopher://%s:%u/%c%s\"",
					 attrib, ourname, ourport, typech, p);
				}else{
					sprintf(newurl,"%s=\"http://%s:%u/%s\"",
					 attrib, ourname, ourport,p);
				}
			}
			*buf = ch;
			if(*buf)
				buf++;
			p = newurl;	/* output the new URL */
			while(*p){
				write(sockfd, p, 1);
				p++;
			}
			/* let main loop print rest of the attributes */
		}else{
bad:
			if( *buf == quoted){
				quoted = 0;
				write(sockfd, buf, 1);
				buf++;
				continue;
			}
			if( !quoted && *buf == '"'){
				quoted = *buf;
				write(sockfd, buf, 1);
				buf++;
				continue;
			}
			write(sockfd, buf, 1);
			buf++;
		}
	}
	return 0;
}


/* Authorization specific code 
 * add_auth adds extra info to the item and
 * check_for_auth processes it
 *
 * looks for auth:user:token:
 */

int check_for_auth(sockfd, buf)
int sockfd;
char *buf;
{	char *p, *q;

	if( *buf && buf[1] == '/'){
		buf = &buf[2];
	}else if( *buf == '/'){
		buf++;
	}

	if( strncasecmp(buf,"auth:", 5)==0){
		p = &buf[5];
		q = authuser;
		while( *p && (*q = *p++) != ':' && q != &authuser[127])q++;
		*q = '\0';

		q = auth_token;
		while( *p && (*q = *p++) != ':' && q != &auth_token[1023])q++;
		*q = '\0';

		strcpy(buf, p);	/* get rid of the auth:... stuff */

		if( check_token( auth_token, authuser)){
			authenticated = 1;
			strcpy(autheduser, authuser);
		}else{
			authuser[0] = '\0';
			auth_token[0] = '\0';
		}
	}
	return 0;
}


static char *add_auth(buf)
char *buf;
{
	static char abuf[1024];
	char *p;

	p = abuf;
	if( *buf && buf[1] == '/'){
		*p++ = *buf++;
		*p++ = *buf++;
	}
	sprintf(p,"auth:%s:%s:%s", autheduser, auth_token, buf);
	return abuf;
}

static char *add_auth_to_url(buf)
char *buf;
{	char *p, *p1, *q;
	static char abuf[1024];
	
	p = buf;
	q = abuf;

	while(*p && *p != '/' && *p != ':')p++;
	if( *p == ':'){
		p++;		/* colon found first, assume proto:// form */
		if( *p == '/' && p[1] == '/')
			p += 2;
		while(*p && *p != '/')p++;
		if( *p)p++;
		p1 = buf;

		while(p1 != p){
			*q++ = *p1++;
		}
	}else{
		p = buf;
	}
	sprintf(q,"auth%%3a%s%%3a%s%%3a%s", autheduser, auth_token, p);
	return abuf;
}


/* connect to the auth server and send the authorize command */

char *auth_login(user, token, path, rbuf, rbuflen)
char *user;
char *rbuf;
char *token, *path;
int rbuflen;
{	

	if( path == NULL)
		path = "";
	if( token == NULL)
		token = "";

	if( auth_open(confp)){
		return NULL;
	}
	if( auth_recv(tmp_auth_buf, sizeof(tmp_auth_buf))){
		return NULL;
	}
	if(strncmp(tmp_auth_buf,"Authsrv ready", 13)){
		auth_close();
		return NULL;
	}

	if( strlen(rladdr)+strlen(riaddr)+strlen(user)+100+strlen(token)+strlen(path) > sizeof(tmp_auth_buf)){
		sprintf(tmp_auth_buf, "authorize %.490s 'basic %s/%s'", user, rladdr, riaddr);
	}else{
		if( token[0]){
			sprintf(tmp_auth_buf, "authorize %s 'token %s/%s\t%s\t%s'", user, rladdr, riaddr, token, path);
		}else{
			sprintf(tmp_auth_buf, "authorize %s 'ftp-gw %s/%s'", user, rladdr, riaddr);
		}
	}
	if( auth_send(tmp_auth_buf)){
		goto lostauth;
	}
	if( auth_recv(rbuf, rbuflen))
		goto lostauth;
	return rbuf;

lostauth:
	return NULL;
}

int ask_for_username(sockfd)
int sockfd;
{
	gostartmenu(sockfd);
	sprintf(errbuf,"1Access to selected menu requires Authorization\t/\t%s\t%u", ourname, ourport);
	goputmenu(sockfd, errbuf);
	sprintf(errbuf,"7Enter Username (at Index search prompt)\twaissrc:/user/%s\t%s\t%u",orig_request,ourname, ourport);
	goputmenu(sockfd, errbuf);
	goendmenu(sockfd);
	return 0;
}

ask_for_password(sockfd, rbuf)
int sockfd;
char *rbuf;
{
	gostartmenu(sockfd);
	sprintf(errbuf,"1Access to selected menu requires Authorization\t/\t%s\t%u", ourname, ourport);
	goputmenu(sockfd, errbuf);
	if( !strncasecmp(rbuf, "challenge", 10)){
		sprintf(errbuf,"7Enter Response to '%s' (at Index search prompt)\twaissrc:/pass/%s/%s\t%s\t%u",
		authuser, &rbuf[10], orig_request,ourname, ourport);
	}else{
		sprintf(errbuf,"7Enter Password for '%s' (at Index search prompt)\twaissrc:/pass/%s/%s\t%s\t%u",
		authuser, authuser, orig_request,ourname, ourport);
	}
	goputmenu(sockfd, errbuf);
	goendmenu(sockfd);
	return 0;
}

int is_authenticated(sockfd, req)
int sockfd, req;
{	static char rbuf[512];
	char *p;

	if( authuser[0] == '\0'){	/* don't know who yet... */
		if( (req & TYPE_DIR) == 0){
			syslog(LLEV,"NOAUTH host=%s/%s to=%s:%u path=%s", rladdr, riaddr, rem_server, rem_port, rem_path);
			return 0;
		}
		ask_for_username(sockfd);
		return 0;
	}

	if( authenticated)
		return 1;		/* every thing must be ok */

	if( rem_typech != 'h' && (req&TYPE_DIR) == 0){
			syslog(LLEV,"NOAUTH host=%s/%s to=%s:%u path=%s", rladdr, riaddr, rem_server, rem_port, rem_path);
			return 0;
	}

	p = auth_login(authuser, NULL, NULL, rbuf, sizeof(rbuf));

	if( p == NULL){
		goto lostconn;
	}
	if( !strncasecmp(p, "ok", 2)){
		return 1;
	}

	ask_for_password(sockfd, rbuf);
	return 0;
	

lostconn:
	go_error(sockfd, 403,"Authorization failed! Sorry.");
	return 0;
}

/* Save the original request. */

int must_escape(ch)
int ch;
{
	if( ch == '\t')
		return 1;
	return 0;
}

int copy_with_escapes(dest, src,len)
char *dest, *src;
int len;
{	int cnt;

	cnt = 1;
	while(*src){
		if( must_escape(*src) ){
			sprintf(dest, "%%%02x", *src & 0xff);
			dest += 3;
			cnt += 3;
			if( cnt > (len -3))
				break;
			src++;
		}else{
			*dest++ = *src++;
			cnt++;
			if( cnt >= len)
				break;
		}
	}
	*dest = '\0';
	return 0;
}


int authenticate(user, pass)
char *user, *pass;
{ 	char *p;

	tmp_auth_buf[0] = '\0';
	p = auth_login(user, NULL, NULL, tmp_auth_buf, sizeof(tmp_auth_buf));

	if( p == NULL)
		return 0;		/* auth attempt fAILED */

	if( strchr(pass, '\'') && strchr(pass, '"')){
		sprintf(tmp_auth_buf, "response '%s'", pass);
	}else{
		sprintf(tmp_auth_buf, "response %s", pass);
	}
	if( auth_send(tmp_auth_buf)){
		goto lostconn;
	}
	if( auth_recv(tmp_auth_buf, sizeof(tmp_auth_buf))){
		goto lostconn;
	}
	if( strncmp(tmp_auth_buf,"ok", 2)){
		Debug((debugf,"Password failed for user=%s\n", user))
		return 0;
	}
	if( tmp_auth_buf[2]){
		strncpy(auth_token, &tmp_auth_buf[2], 1023);
	}else{
		strcpy(auth_token, "XXXX");
		syslog(LLEV, "No authtoken available");
	}
	strcpy(autheduser, user);
	authenticated = 1;
	return 0;

lostconn:
	Debug((debugf,"authenticate: lost authsrv connection\n"))
	syslog(LLEV,"lost authsrv connection");
	return 0;
}

int check_token(auth_token, authuser)
char *auth_token, *authuser;
{	static char rbuf[512];
	char *p;

	p = auth_login(authuser, auth_token, rem_path, rbuf, sizeof(rbuf));

	if( p ){
		if( !strncasecmp(p, "ok", 2))
			return 1;
	}
	return 0;
}

/* 12 oct 94 new error processing */
int gostarterror(sockfd, errno)
int sockfd, errno;
{
	if( (rem_type & TYPE_HTTP) || rem_typech == 'h'){
		if( rem_type & TYPE_FTP){
			say_sub(sockfd,"<h1>FTP Error"); 
		}else{
			say_sub(sockfd,"<h1>Error"); 
		}
		sprintf(errbuf, "- %d</h1>", errno);
		say(sockfd,errbuf);
		say(sockfd,"<HR>");
	}else {
		if( rem_type& TYPE_DIR){
			if(rem_type & TYPE_FTP)
				say(sockfd,"-FTP Error\t\t\t");
			else
				say(sockfd,"-Error\t\t\t");
			say(sockfd,"---------------------------------------\t\t\t");
		}else if( rem_type & TYPE_TEXT){
			if(rem_type & TYPE_FTP)
				say(sockfd,"FTP Error");
			else
				say(sockfd,"Error");
			say(sockfd,"----------------------------------------");
		}
	}
	return 0;
}

int goenderror(sockfd)
int sockfd;
{
	return 0;
}

int go_error(sockfd, errno, msg, va_alist)
int sockfd;
int errno;
char *msg;
va_dcl
{	static int last_errno = 0;
	va_list marker;

	if( errno != last_errno){	/* flush previous error stuff */
		if( last_errno){
			goenderror(sockfd);
		}
		last_errno = errno;
		if( last_errno){
			gostarterror(sockfd, errno);
		}
	}
	if( msg != NULL){
		va_start(marker);
		vsprintf(errbuf, msg, marker);
		va_end(marker);
		if( (rem_type & (TYPE_HTTP|TYPE_DIR)) == (TYPE_DIR)){
			say_sub(sockfd,"-");
		}
		say_sub(sockfd, errbuf);
		if( (rem_type & TYPE_HTTP) || rem_typech == 'h'){
			say(sockfd, "<br>");
		}else if( rem_type & TYPE_DIR){
			say(sockfd, "\t\t\t");
		}else {
			say(sockfd, " ");
		}
	}

	return 0;
}


/* do the plug-gw stuff */

int do_plug(sockfd, buf)
int sockfd;
char *buf;
{	int plug_fd, plugdata;
	int port, rem_fd;
	struct sockaddr serv_addr;
	int length;
	int cnt;
	char *p, protocol[16];
	fd_set	redy;
	fd_set	iced;
	struct timeval *tp = NULL;
	int ib = 0, ob = 0;
	unsigned char haddr[4];
	int single = 1;

	strcpy(tmp_auth_buf, buf);
	buf = tmp_auth_buf;


	plug_fd = make_conn(0);
	if( plug_fd < 0)
		goto broken;

	if( listen(plug_fd, 2))
		goto broken;

	port = port_num(sockfd, plug_fd, haddr);
	if( port == 0)
		goto broken;

	sprintf(go_request,"HTTP/1.0 302 Found");
	say(sockfd, go_request);

	strncpy(protocol, buf, 15);
	p = index(protocol, ':');
	if( p)
		*p = '\0';
	
	p = index(buf, ':');
	if( p == NULL)
		goto broken;
	p++;
	if( *p == '/')p++;
	if( *p == '/')p++;

	strcpy(errbuf, p);
	p = index(p, '@');
	if( p ){
		p++;
		strcpy(rem_server, p);
		p = index(errbuf,'@');
		*p = '\0';
		p = index(rem_server,'/');
		if( p )
			*p = '\0';
	}else{
		strcpy(rem_server, errbuf);
		p = rem_server;
		p = index(p, '/');
		if( p == NULL)
			goto broken;
		*p++ = '\0';
		if(*p == '8')p++;
		strcpy(errbuf, p);
	}
	

	if( !strcasecmp(protocol, "ptelnet")){
		sprintf(go_request,"Location: %s://%s@%s:%u/", &protocol[1], errbuf, ourname, port);
	}else{
		sprintf(go_request,"Location: %s://%s:%u/", &protocol[1], ourname, port);
	}
	say(sockfd, go_request);
	say(sockfd, "");

	rem_port = 23;
	rem_port = get_port(rem_server, rem_port);
	
	close(sockfd);
	close(1);
	close(2);
	sockfd = -1;

loop:
	timeout.tv_sec = 120;

	(void) signal(SIGALRM, net_timeout);
	(void) alarm(timeout.tv_sec);

Debug((debugf, "Start alarm\n"))
	length = sizeof(struct sockaddr );
	plugdata = accept(plug_fd, (struct sockaddr *)&serv_addr, &length);
	if( plugdata < 0){
		goto broken;
	}

	if(peername(plugdata,prladdr,priaddr,sizeof(riaddr))) {
		syslog(LLEV,"cannot get peer name");
		http_exit(1);
	}

	if( strcmp(prladdr, rladdr) || strcmp(priaddr, riaddr)){
		syslog(LLEV,"deny host=%s/%s should have been %s/%s",
			prladdr, priaddr, rladdr,riaddr);
		http_exit(1);
	}

	(void) alarm(0);
	(void) signal(SIGALRM, SIG_IGN);

	syslog(LLEV,"start host=%s/%s dest=%s path=%s", rladdr, riaddr, rem_server, buf);

	if( !strcasecmp(protocol,"ptelnet")){
		sprintf(go_request,"\r\nHTTP-GW Experimental Telnet gateway");
		say(plugdata, go_request);
		sprintf(go_request,"Trying to connect to %s port %u", rem_server, rem_port);
		say(plugdata, go_request);
	
		if( (rem_fd = conn_server(rem_server, rem_port, 0, errbuf)) < 0){
			sprintf(go_request,"Failed to connect.");
			say(plugdata, go_request);
			cnt = read(plugdata,go_request, 1);
			syslog(LLEV,"failed to connect to server %s (%d)", rem_server, rem_port);
			http_exit(1);
		}
	}else{
		if( (rem_fd = conn_server(rem_server, rem_port, 0, errbuf)) < 0){
			syslog(LLEV,"failed to connect to server %s (%d)", rem_server, rem_port);
			http_exit(1);
		}
		single = 0;
	}

	FD_ZERO(&iced);
	FD_SET(plugdata, &iced);
	FD_SET(rem_fd, &iced);
	ib = 0;
	 ob = 0;
	
	while(1){
		(void) bcopy(&iced, &redy, sizeof(fd_set));

		if(select(4, &redy, (fd_set *) 0, (fd_set *)0, tp) <= 0){
			break;
		}

		if( FD_ISSET(plugdata, &redy)){
			if( (cnt = read(plugdata, go_request, 1024)) <= 0)
				break;
			if( write(rem_fd, go_request, cnt) != cnt)
				break;
			ib += cnt;
		}

		if( FD_ISSET(rem_fd, &redy)){
			if( (cnt = read(rem_fd, go_request, 1024)) <= 0)
				break;
			if( write(plugdata, go_request, cnt) != cnt)
				break;
			ob += cnt;
		}
	}
	inbytcnt += ib;
	outbytcnt += ob;

	if( !single){
		close(plugdata);
		close(rem_fd);
		plugdata = rem_fd = -1;
Debug((debugf,"Stop %s/%s %d %d\n", rladdr, riaddr, inbytcnt, outbytcnt))
		syslog(LLEV,"stop host=%s/%s in=%d out=%d",
			rladdr,riaddr,inbytcnt, outbytcnt);
		inbytcnt = 0;
		outbytcnt= 0;
		goto loop;
	}
	return 0;

broken:
	if( sockfd != -1){
		go_error(sockfd, 404, "Could not open connection for telnet");
	}
	return 0;
}
