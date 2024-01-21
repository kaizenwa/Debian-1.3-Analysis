/* routines for easily establishing Unix domain socket connections */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/errno.h>
#include <sys/un.h>
#include <arpa/inet.h>

#include "ipc.h"

/* imports */
extern char *memset();
extern char *strsave(), *str3save();
extern char *sys_errlist[];
extern int errno, sys_nerr, h_errno;

/* private */
static char *unixerr;

static int
unix_sock()				/* create an anonymous (unix) socket */
{
	unixerr = "socket";
	/* 0 means "no protocol", which is mandatory for AF_UNIX */
	return socket(AF_UNIX, SOCK_STREAM, 0);
}

/*
 * unixopen returns a Unix domain socket's file descriptor to the file
 * named `service'.
 */
/* ARGSUSED hostname */
int
unixopen(hostname, service)
char *hostname, *service;
{
	struct sockaddr_un svaddr;      /* server's internet descriptor */
	register struct sockaddr_un *svp = &svaddr;
	register int sock;

	errno = 0;
	(void) memset((char *)svp, 0, sizeof svaddr);
	svp->sun_family = AF_UNIX;
	(void) strncpy(svp->sun_path, service, sizeof svp->sun_path);

	sock = unix_sock();
	if (sock < 0)
		return -1;

	unixerr = "connect";
	/* connect the socket to the server */
	if (connect(sock, (struct sockaddr *)svp, sizeof svaddr) >= 0)
		return sock;
	(void) close(sock);
	return -1;
}

/*
 * unixlisten returns a Unix domain socket on which new connections will
 * arrive on the file named `service'.
 */
int
unixlisten(service)
char *service;
{
	register int sock;		/* listen here for new connections */
	struct sockaddr_un lsad;	/* sock's net addr */
	register struct sockaddr_un *lsp = &lsad;
	register int res;

	errno = 0;
	(void) memset((char *)lsp, 0, sizeof lsad);
	lsp->sun_family = AF_UNIX;
	(void) strncpy(lsp->sun_path, service, sizeof lsp->sun_path);

	sock = unix_sock();
	if (sock < 0)
		return -1;

	/* attach the socket to above-named port. */
	unixerr = "bind";
	res = bind(sock, (struct sockaddr *)lsp, sizeof lsad);
	if (res < 0) {
		(void) close(sock);
		return -1;
	}

	/* declare intent to accept connections */
	unixerr = "listen";
	if (listen(sock, 100) < 0) {	/* kernel truncates 100 to 5 */
		(void) close(sock);
		return -1;
	}

	return sock;
}

/*
 * accept a new connection from `listsock' and return it.
 * on success, *sap will have the address of the peer machine
 * and *slp will have *sap's real size.
 */
int
unixconn(listsock, infop)
int listsock;
register ipcinfo *infop;
{
	int newfd;
	struct sockaddr_un addr;
	int addrlen = sizeof addr;

	unixerr = "accept";
	newfd = accept(listsock, (struct sockaddr *)&addr, &addrlen);
	if (newfd < 0)
		return -1;

	addr.sun_path[addrlen] = '\0';		/* paranoia */
	infop->name = strsave(addr.sun_path);	/* TODO: free */
	infop->param = "";
	infop->machine = "localhost";
	infop->user = "nobody";
	infop->uid = 1;
	infop->gid = 1;
	return newfd;
}

char *
unixerror()
{
	static char *errbuf;

	if (errbuf != NULL)
		free(errbuf);
	if (errno > 0 && errno < sys_nerr) {
		char *errdetail = str3save("(", sys_errlist[errno], ")");

		errbuf = str3save(unixerr, " ", errdetail);
		free(errdetail);
		errno = 0;
#ifdef notdef
	} else if (h_errno > 0 && h_errno < h_nerr) {
		char *errdetail = str3save("(", h_errlist[h_errno], ")");

		errbuf = str3save(unixerr, " ", errdetail);
		free(errdetail);
		h_errno = 0;
#endif
	} else
		errbuf = strsave(unixerr);
	return errbuf;
}
