/*
 * rpcmisc	Miscellaneous functions for RPC startup and shutdown.
 *		This code is partially snarfed from rpcgen -s tcp -s udp,
 *		partly written by Mark Shand, Donald Becker, and Rick 
 *		Sladkey. It was tweaked slightly by Olaf Kirch to be
 *		useable by both nfsd and mountd.
 *
 *		This software may be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */

#include "system.h"
#include <stdio.h>
#include <stdlib.h>
#include <rpc/pmap_clnt.h> 
#include <string.h> 
#include <signal.h>
#include <sys/ioctl.h> 
#include <sys/types.h> 
#include <sys/stat.h> 
#include <fcntl.h> 
#include <memory.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "rpcmisc.h"
#include "logging.h"

static _PRO (int makesock,	(int port, int proto, int socksz)	);

#define _RPCSVC_CLOSEDOWN	120
time_t	closedown = 0;
int	_rpcpmstart = 0;
int	_rpcfdtype = 0;
int	_rpcsvcdirty = 0;

void
rpc_init(name, prog, verstbl, dispatch, defport, bufsiz)
char	*name;
int	prog;
int	*verstbl;
void	(*dispatch)();
int	defport;
int	bufsiz;
{
	struct sockaddr_in saddr;
	SVCXPRT	*transp;
	int	sock, i, vers;
	size_t	asize;

	/* When started from inetd, initialize only once */
	if (_rpcpmstart)
		return;

	asize = sizeof(saddr);
	sock = 0;
	if (getsockname(0, (struct sockaddr *) &saddr, &asize) == 0) {
		size_t	ssize = sizeof (int);
		if (saddr.sin_family != AF_INET)
			goto not_inetd;
		if (getsockopt(0, SOL_SOCKET, SO_TYPE,
				(char *)&_rpcfdtype, &ssize) == -1)
			goto not_inetd;
		background_logging();	/* no more logging to stderr */
		closedown = time(NULL) + _RPCSVC_CLOSEDOWN;
		_rpcpmstart = 1;
	} else {
not_inetd:
		_rpcfdtype = 0;
		for (i = 0; (vers = verstbl[i]) != 0; i++)
			pmap_unset(prog, vers);
		sock = RPC_ANYSOCK;
	}

	if ((_rpcfdtype == 0) || (_rpcfdtype == SOCK_DGRAM)) {
		if (_rpcfdtype == 0 && defport != 0 &&
		    ((sock = makesock(defport, IPPROTO_UDP, bufsiz)) < 0)) {
			Dprintf(L_FATAL, "could not make a udp socket\n");
		}
		transp = svcudp_create(sock);
		if (transp == NULL) {
			Dprintf(L_FATAL, "cannot create udp service.");
		}
		for (i = 0; (vers = verstbl[i]) != 0; i++) {
			if (!svc_register(transp, prog, vers, dispatch, IPPROTO_UDP)) {
				Dprintf(L_FATAL,
					"unable to register (%s, %d, udp).",
					name, vers);
			}
		}
	}

	if ((_rpcfdtype == 0) || (_rpcfdtype == SOCK_STREAM)) {
		if (_rpcfdtype == 0 && defport != 0 &&
		    ((sock = makesock(defport, IPPROTO_TCP, bufsiz)) < 0)) {
			Dprintf(L_FATAL, "could not make a tcp socket\n");
		}
		transp = svctcp_create(sock, 0, 0);
		if (transp == NULL) {
			Dprintf(L_FATAL, "cannot create tcp service.");
		}
		for (i = 0; (vers = verstbl[i]) != 0; i++) {
			if (!svc_register(transp, prog, vers, dispatch, IPPROTO_TCP)) {
				Dprintf(L_FATAL,
					"unable to register (%s, %d, tcp).",
					name, vers);
			}
		}
	}

	/* We ignore SIGPIPE. SIGPIPE is being sent to a daemon when trying
	 * to do a sendmsg() on a TCP socket whose peer has disconnected.
	 */
	if (!_rpcpmstart) {
		struct sigaction pipeact;

		memset(&pipeact, 0, sizeof(pipeact));
		pipeact.sa_handler = SIG_IGN;
		sigaction(SIGPIPE, &pipeact, NULL);
	}
}

void rpc_exit(prog, verstbl)
int	*verstbl;
{
	int	i, vers;

	if (_rpcpmstart)
		return;
	for (i = 0; (vers = verstbl[i]) != 0; i++)
		pmap_unset(prog, vers);
}

void rpc_closedown(void)
{
	struct sockaddr_in	sin;
	static int		size = 0;
	time_t			now = time(NULL);
	int			i, len;

	if (!_rpcpmstart || now < closedown)
		return;
	if (_rpcsvcdirty == 0) {
		if (_rpcfdtype == SOCK_DGRAM)
			exit(0);

		/* Okay, this is a TCP socket. Check whether we're still
		 * connected */
		if (size == 0) {
			size = getdtablesize();
		}
		for (i = 0; i < size; i++) {
			if (!FD_ISSET(i, &svc_fdset))
				continue;
			len = sizeof(sin);
			if (getpeername(i, (struct sockaddr *) &sin, &len) >= 0)
				exit(0);
		}
	}
	closedown = now + _RPCSVC_CLOSEDOWN;
}

static int makesock(port, proto, socksz)
int port;
int proto;
int socksz;
{
	struct sockaddr_in sin;
	int	s;
	int	sock_type;

	sock_type = (proto == IPPROTO_UDP) ? SOCK_DGRAM : SOCK_STREAM;
	s = socket(AF_INET, sock_type, proto);
	if (s < 0) {
		Dprintf(L_ERROR, "Could not make a socket: %s\n",
					strerror(errno));
		return (-1);
	}
	memset((char *) &sin, 0, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = htons(port);

#ifdef DEBUG
	{
	int	val = 1;
	if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &val, sizeof(val)) < 0)
		Dprintf(L_ERROR, "setsockopt failed: %s\n", strerror(errno));
	}
#endif

#ifdef SO_SNDBUF
	if (socksz != 0) {
		int sblen, rblen;

		/* 1024 for rpc & transport overheads */
		sblen = rblen = 8 * (socksz + 1024);
		if (setsockopt(s, SOL_SOCKET, SO_SNDBUF, &sblen, sizeof sblen) < 0 ||
		    setsockopt(s, SOL_SOCKET, SO_RCVBUF, &rblen, sizeof rblen) < 0)
			Dprintf(L_ERROR, "setsockopt failed: %s\n", strerror(errno));
	}
#endif				/* SO_SNDBUF */

	if (bind(s, (struct sockaddr *) &sin, sizeof(sin)) == -1) {
		Dprintf(L_ERROR, "Could not bind name to socket %s:%d: %s\n",
					inet_ntoa(sin.sin_addr), 
					ntohs(sin.sin_port),
					strerror(errno));
		return (-1);
	}
	return (s);
}
