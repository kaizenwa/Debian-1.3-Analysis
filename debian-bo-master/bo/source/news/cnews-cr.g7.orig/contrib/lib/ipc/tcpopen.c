/* tcpopen - easily establish tcp connections (4.2bsd version) */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/errno.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "ipc.h"

#ifndef INADDR_NONE
#define INADDR_NONE	0xffffffff	/* unsigned 32-bit -1 */
#endif
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN	128
#endif

/* imports */
extern char *strsave(), *str3save();
extern char *sys_errlist[];
extern int errno, sys_nerr, h_errno;

/* forwards */
static int servnametonum(), is_ipaddr();

/* private */
static char *protocol_name = "tcp";
static char *tcperr;

static int
tcp_sock()				/* create an anonymous (tcp) socket */
{
	register struct protoent *pp;	/* protocol we're going to use */

	errno = 0;
	/* map protocol name (generally tcp) to a number */
	tcperr = "getprotobyname";
	pp = getprotobyname(protocol_name);
	if (pp == NULL)
		return -1;

	/* create the socket */
	tcperr = "socket";
	return socket(AF_INET, SOCK_STREAM, pp->p_proto);
}

static char *
getmyhostname()
{
	static char *myhostname = NULL;

	if (myhostname == NULL) {
		static char namebuf[MAXHOSTNAMELEN];

		tcperr = "gethostname";
		if (gethostname(namebuf, sizeof namebuf) < 0)
			return NULL;
		myhostname = namebuf;
	}
	return myhostname;
}

/*
 * parse a dotted-quad address (e.g. 192.74.137.5) into a hostent,
 * even if gethostbyaddr can't reverse map it to a name.
 */
static struct hostent *
prshstno(hostname)
char *hostname;
{
	register struct hostent *hp;
	struct in_addr ipaddr;

	tcperr = "inet_addr";
	ipaddr.s_addr = inet_addr(hostname);
	if (ipaddr.s_addr == INADDR_NONE)
		return NULL;
	tcperr = "gethostbyaddr";
	hp = gethostbyaddr((char *)&ipaddr, sizeof ipaddr, AF_INET);
	if (hp == NULL) {
		static struct hostent htmp;
		static char fakehaddr[sizeof (struct in_addr)];
		static char *fkhadrlist[2] = { fakehaddr, 0 };

		hp = &htmp;
		hp->h_name = "*unknown*";
		hp->h_aliases = &fkhadrlist[1];
		hp->h_addrtype = AF_INET;
		hp->h_length = sizeof fakehaddr;
		(void) memcpy((char *)fakehaddr,
			      (char *)&ipaddr.s_addr, hp->h_length);
#ifdef h_addr
		hp->h_addr_list = fkhadrlist;
#else
		hp->h_addr = fakehaddr;
#endif
	}
	return hp;
}

static int
mkconnect(sock, svp, hp, service, ipaddrp)
int sock;
register struct sockaddr_in *svp;
register struct hostent *hp;
char *service;
char *ipaddrp;
{
	tcperr = "connect";
	(void) memset((char *)svp, 0, sizeof *svp);
	svp->sin_family = hp->h_addrtype;
	svp->sin_port = servnametonum(service);
	if (svp->sin_port == (unsigned short)-1)
		return -1;
	(void) memcpy((caddr_t)&svp->sin_addr, ipaddrp, hp->h_length);
	/* connect the socket to the server */
	return connect(sock, (struct sockaddr *)svp, sizeof *svp);
}

/*
 * tcpopen returns a TCP/IP socket's file descriptor to the specified host
 * on the port corresponding to the specified service.  Service may be a
 * string representation of a number, in which case that port number will
 * be used.
 *
 * try all addresses for a given host if we have the 4.3BSD resolver (i.e.
 * hp->h_addr_list[n] for all n).
 *
 * even a successful gethostby* can result in h_errno being set, usually
 * to HOST_NOT_FOUND, because of the iteration in the resolver search.
 */
int
tcpopen(hostname, service)
char *hostname, *service;
{
	register struct hostent *hp;
	struct sockaddr_in svaddr;      /* server's internet descriptor */
	register struct sockaddr_in *svp = &svaddr;
	register char **addr;
	register int sock;

	/* default to this host */
	errno = 0;
	h_errno = 0;
	if (hostname == NULL) {
		hostname = getmyhostname();
		if (hostname == NULL)
			return -1;
	}

	/* map hostname (or ip addr) to a list of ip addrs */
	if (is_ipaddr(hostname))
		hp = prshstno(hostname);
	else {
		tcperr = "gethostbyname";
		hp = gethostbyname(hostname);
	}
	if (hp == NULL)
		return -1;
	h_errno = 0;

	/* acquire a socket. */
	sock = tcp_sock();
	if (sock < 0)
		return -1;

	/*
	 * set up a sockaddr_in for connect, connect the socket to the
	 * (remote) server.
	 */
#ifdef h_addr				/* 4.3 resolver */
	for (addr = hp->h_addr_list; *addr != 0; addr++)
		if (mkconnect(sock, svp, hp, service, *addr) >= 0)
			return sock;
#else					/* 4.2 resolver: you get one shot */
	if (mkconnect(sock, svp, hp, service, hp->h_addr) >= 0)
		return sock;
#endif
	(void) close(sock);
	return -1;
}

/*
 * N.B.: after socket/bind/listen+accept/close+close, the port remains
 * busy for the duration of the TCP TIME_WAIT state (30 seconds or more),
 * with bind returning EADDRINUSE.  Setting SO_REUSEADDR will override
 * the TIME_WAIT and let us bind, yet will not let us bind if there really
 * is another server on the port.  The joy of TCP.
 */
static int
tcpsinlstn(service, lsp)		/* common code called from below */
char *service;
register struct sockaddr_in *lsp;
{
	register int sock;		/* listen here for new connections */
	register int res, saveport;
	int on = 1;

	errno = 0;
	lsp->sin_port = saveport = servnametonum(service);
	if (lsp->sin_port == (unsigned short)-1)
		return -1;

	sock = tcp_sock();
	if (sock < 0)
		return -1;

	/* attach the socket to above-named port. */
	tcperr = "setsockopt";
	if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof on)
	    < 0) {
		(void) close(sock);
		return -1;
	}
	tcperr = "bind";
	res = bind(sock, (struct sockaddr *)lsp, sizeof *lsp);
	if (res < 0) {
		int saverrno = errno;		/* fprintf can set errno */

		if (lsp->sin_addr.s_addr != htonl(INADDR_ANY) ||
		    lsp->sin_port != saveport)
			(void) fprintf(stderr, "bind is fucking me over!\n");
		errno = saverrno;	/* but close of an open fd can't */
		(void) close(sock);
		return -1;
	}

	/* declare intent to accept connections */
	tcperr = "listen";
	if (listen(sock, 100) < 0) {	/* kernel truncates 100 to 5 */
		(void) close(sock);
		return -1;
	}

	return sock;
}

/*
 * Date: Sat, 9 Nov 1991 19:43:42 -0500
 * From: Mark Moraes <moraes@cs.toronto.edu>
 *
 * The socket stuff is bizarre.  Basically, I need lsp->sin_addr.s_addr to
 * be the address of the remote host when binding the data sockets so that
 * they can be listened on.  Don't want any random joe from the Internet
 * connecting to that port; also, as a subtle feature, the sin_addr.s_addr
 * address on the new port that tcplisten returns will be the local
 * machine's address if lsp->sin_addr.s_addr is the remote addr; if
 * lsp->sin_addr.s_addr is INADDR_ANY, then the sin_addr.s_addr on the port
 * returned by tcplisten is 0.0.0.0, which is not useful when one is trying
 * to send this to the remote end via a PORT command.  If you find this
 * hard to follow, I'm not surprised -- I had to decipher this from the BSD
 * ftp client; documentation, what documentation.  I have a splitting
 * headache; supper and some cool air will hopefully solve that.
 */

/*
 * tcpadrlstn returns a TCP/IP socket on which new connections will arrive
 * on the port corresponding to the specified service.  tcpadrlstn will
 * only accept connections from the address that oldport is connected
 * to/from.  Service may be a string representations of a number, in which
 * case that port number will be used.
 */
tcpadrlstn(service, oldport)
char *service;
int oldport;				/* live fd */
{
	struct sockaddr_in lsad;	/* oldport's net addr */
  	register struct sockaddr_in *lsp = &lsad;
 	int lsplen = sizeof lsad;
  
	if (getsockname(oldport, (struct sockaddr *)lsp, &lsplen) < 0) {
		tcperr = "getsockname";
		return -1;
	}
	return tcpsinlstn(service, lsp);
}

/*
 * tcplisten returns a TCP/IP socket on which new connections will arrive
 * on the port corresponding to the specified service.  Service may be a
 * string representation of a number, in which case that port number will be
 * used.
 */
int
tcplisten(service)
char *service;
{
	struct sockaddr_in lsad;		/* net addr */
	register struct sockaddr_in *lsp = &lsad;

	(void) memset((char *)lsp, 0, sizeof lsad);
	lsp->sin_family = AF_INET;
	lsp->sin_addr.s_addr = htonl(INADDR_ANY);
	return tcpsinlstn(service, lsp);
}

static int
servnametonum(service)			/* map service to a port number */
register char *service;
{
	if (service == 0)
		return 0;
	else if (isascii(*service) && isdigit(*service))
		return htons((unsigned short)atoi(service));
	else {
		struct servent *sp = getservbyname(service, protocol_name);

		tcperr = "getservbyname";
		return (sp == NULL? -1: sp->s_port);
	}
}

/*
 * accept a new connection from `listsock' and return it.
 * on success, *sap will have the address of the peer machine and *slp will
 * have *sap's real size.
 */
int
tcpconn(listsock, infop)
int listsock;
register ipcinfo *infop;
{
	int newfd;
	struct sockaddr_in addr;
	int addrlen = sizeof addr;
	struct hostent *hp;
	static char port[30];

	tcperr = "accept";
	newfd = accept(listsock, (struct sockaddr *)&addr, &addrlen);
	if (newfd < 0)
		return -1;

	(void) sprintf(port, "%d", ntohs(addr.sin_port));
	infop->name = port;
	infop->param = "";
	hp = gethostbyaddr((char *)&addr.sin_addr, sizeof addr.sin_addr,
		AF_INET);
	if (hp == NULL) {
		static char mch[60];

		(void) strcpy(mch, inet_ntoa(addr.sin_addr));
		infop->machine = mch;
	} else {
		register char *p;

		infop->machine = strsave(hp->h_name);
		/* internet host names are defined as case-independent */
		for (p = infop->machine; *p != '\0'; p++)
			if (isascii(*p) && isupper(*p))
				*p = tolower(*p);
	}
	infop->user = "nobody";
	infop->uid = 1;
	infop->gid = 1;
	return newfd;
}

char *
tcperror()
{
	static char *errbuf;

	if (errbuf != NULL)
		free(errbuf);
	if (errno > 0 && errno < sys_nerr) {
		char *errdetail = str3save("(", sys_errlist[errno], ")");

		errbuf = str3save(tcperr, " ", errdetail);
		free(errdetail);
		errno = 0;
#ifdef notdef
	} else if (h_errno > 0 && h_errno < h_nerr) {
		char *errdetail = str3save("(", h_errlist[h_errno], ")");

		errbuf = str3save(tcperr, " ", errdetail);
		free(errdetail);
		h_errno = 0;
#endif
	} else
		errbuf = strsave(tcperr);
	return errbuf;
}

static int
is_ipaddr(s)
register char *s;
{
	for (; *s != '\0'; s++)
		if (!isascii(*s) || (!isdigit(*s) && *s != '.'))
			return 0;
	return 1;
}

#ifdef TEST
int
main()
{
	int fd = tcpopen("smoke.cs.toronto.edu", "smtp");
	int cc;
	char buf[1024];

	if (fd < 0) {
		perror("tcpopen");
		printf("system call that failed: %s\n", tcperror());
		exit(1);
	}
	cc = read(fd, buf, sizeof buf);
	if (cc < 0) {
		perror("read");
		exit(1);
	}
	(void) write(1, buf, cc);
	(void) close(fd);
	return 0;
}
#endif
