/* @(#)clnt_generic.c	2.2 88/08/01 4.0 RPCSRC */
/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
#if !defined(lint) && !defined(SABER)
static char sccsid[] = "@(#)clnt_generic.c 1.4 87/08/11 (C) 1987 SMI";
static char *rcsid = "$Id: clnt_generic.c,v 1.3 1993/12/30 01:49:14 stolcke Exp $ ICSI (Berkeley)";
#endif

/*
 * Copyright (C) 1987, Sun Microsystems, Inc.
 */
#include <string.h>
#include <sys/time.h>
#define PORTMAP
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <sys/socket.h>
#include <sys/errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <errno.h>

extern int errno;

/*
 * Generic client creation: takes (hostname, program-number, protocol) and
 * returns client handle. Default options are set, which the user can 
 * change using the rpc equivalent of ioctl()'s.
 */
CLIENT *
clnt_create(hostname, prog, vers, proto)
#ifdef __STDC__
	const char *hostname;
	const u_long prog;
	const u_long vers;
	const char *proto;
#else
	char *hostname;
	unsigned prog;
	unsigned vers;
	char *proto;
#endif
{
	struct hostent *h;
	struct protoent *p;
	struct sockaddr_in sin;
	int sock;
	struct timeval tv;
	CLIENT *client;

	h = gethostbyname(hostname);
	if (h == NULL) {
		rpc_createerr.cf_stat = RPC_UNKNOWNHOST;
		return (NULL);
	}
	if (h->h_addrtype != AF_INET) {
		/*
		 * Only support INET for now
		 */
		rpc_createerr.cf_stat = RPC_SYSTEMERROR;
		rpc_createerr.cf_error.re_errno = EAFNOSUPPORT; 
		return (NULL);
	}
	sin.sin_family = h->h_addrtype;
	sin.sin_port = 0;
	memset(sin.sin_zero, 0, sizeof(sin.sin_zero));
	memcpy(&sin.sin_addr, h->h_addr, h->h_length);
	p = getprotobyname(proto);
	if (p == NULL) {
		rpc_createerr.cf_stat = RPC_UNKNOWNPROTO;
		rpc_createerr.cf_error.re_errno = EPFNOSUPPORT; 
		return (NULL);
	}

	/*
	 * Make sure the socket created is priviledged when
	 * invoked by root.
	 */
	if (geteuid() != 0) {
		sock = RPC_ANYSOCK;
	} else {
		sock = socket(AF_INET,
			      (p->p_proto == IPPROTO_TCP ?
				SOCK_STREAM : SOCK_DGRAM),
			      p->p_proto);
		if (sock < 0) {
			rpc_createerr.cf_stat = RPC_SYSTEMERROR;
			rpc_createerr.cf_error.re_errno = errno;
			return (NULL);
		}
		/* attempt to bind to priv port */
		if (bindresvport(sock, (struct sockaddr_in *)0) < 0)
			perror("bindresvport");
	}

	switch (p->p_proto) {
	case IPPROTO_UDP:
		/*
		 * Make sure the socket created is priviledged when
		 * invoked by root.
		 */
#ifdef FIONBIO
		if (sock != RPC_ANYSOCK) {
			int dontblock = 1;
			/* the sockets rpc controls are non-blocking */
			(void)ioctl(sock, FIONBIO, (char *) &dontblock);
		}
#endif

		tv.tv_sec = 5;
		tv.tv_usec = 0;
		client = clntudp_create(&sin, prog, vers, tv, &sock);
		if (client == NULL) {
			if (sock != RPC_ANYSOCK) (void)close(sock);
			return (NULL);
		}
		tv.tv_sec = 25;
		clnt_control(client, CLSET_TIMEOUT, (char *)&tv);
		break;
	case IPPROTO_TCP:
		/*
		 * Make sure the socket created is priviledged when
		 * invoked by root.
		 */
		if (sock != RPC_ANYSOCK) {
			u_short port;

			/*
			 * Fill in port number
			 */
			if ((port = pmap_getport(&sin, prog, vers,
			                         IPPROTO_TCP)) == 0) {
				rpc_createerr.cf_stat = RPC_SYSTEMERROR;
				rpc_createerr.cf_error.re_errno = EPFNOSUPPORT;
				(void)close(sock);
				return (NULL);
			}
			sin.sin_port = htons(port);

			/*
			 * Connect socket
			 */
			if (connect(sock, (struct sockaddr *)&sin,
			                  sizeof(sin)) < 0) {
				rpc_createerr.cf_stat = RPC_SYSTEMERROR;
				rpc_createerr.cf_error.re_errno = errno;
				(void)close(sock);
				return (NULL);
			}
		}

		client = clnttcp_create(&sin, prog, vers, &sock, 0, 0);
		if (client == NULL) {
			if (sock != RPC_ANYSOCK) (void)close(sock);
			return (NULL);
		}
		tv.tv_sec = 25;
		tv.tv_usec = 0;
		clnt_control(client, CLSET_TIMEOUT, (char *)&tv);
		break;
	default:
		rpc_createerr.cf_stat = RPC_SYSTEMERROR;
		rpc_createerr.cf_error.re_errno = EPFNOSUPPORT; 
		return (NULL);
	}

#ifdef CLSET_FD_CLOSE
	/*
	 * Make sure the socket is close on clnt_destroy()
	 */
	clnt_control(client, CLSET_FD_CLOSE, (char *)0);
#endif

	return (client);
}
