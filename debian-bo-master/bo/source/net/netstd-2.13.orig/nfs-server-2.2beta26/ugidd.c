/* UNFSD - copyright Mark A Shand, May 1988.
 * This software maybe be used for any purpose provided
 * the above copyright notice is retained.  It is supplied
 * as is, with no warranty expressed or implied.
 *
 * Authors:	Mark A. Shand
 *		Olaf Kirch <okir@monad.swb.de>
 */

#include "site.h"

/* Only compile ugidd if nfs server has support for it. */
#ifdef REAL_UGIDD

#include "system.h"
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <sys/ioctl.h>
#include <getopt.h>
#include "ugid.h"
#include "logging.h"
#ifdef HAVE_LIBWRAP_BUG
#include <syslog.h>
#endif


static _PRO (void ugidprog_1,	(struct svc_req *rqstp, SVCXPRT *transp));
static _PRO (void usage,	(void)					);

#ifndef HAVE_RPCGEN_C
#define authenticate_1_svc	authenticate_1
#define name_uid_1_svc		name_uid_1
#define group_gid_1_svc		group_gid_1
#define uid_name_1_svc		uid_name_1
#define gid_group_1_svc		gid_group_1
#endif

#ifdef HOSTS_ACCESS

#define IP_HASH_MASK	0xF
#define IP_HASH(a)	((((a) >> 24) ^ ((a) >> 16) ^ ((a) >> 8) ^ (a)) & IP_HASH_MASK)

typedef struct ugid_clnt {
	struct ugid_clnt	*next;
	struct in_addr		clnt_addr;
	char			status;
} ugid_clnt;

static ugid_clnt		*clients[IP_HASH_MASK+1];

static _PRO (int  check_client,	(struct svc_req *rqstp)			);
extern _PRO (int  hosts_ctl, (char *, char *, char *, char *)		);
static _PRO (RETSIGTYPE clnt_flush, (int sig)				);

/* libwrap.a from tcp_wrappers-7.2 references these variables when built
 * with OPTIONS support, but does not define them.
 */
#ifdef HAVE_LIBWRAP_BUG
int	deny_severity = LOG_WARNING;
int	allow_severity = LOG_INFO;
#endif

#else
#define check_client(rqstp)	1
#endif /* HOSTS_ACCESS */

static struct option longopts[] = {
	{ "debug", 0, 0, 'd' },
	{ NULL, 0, 0, 0 }
};

int
main(argc, argv)
int	argc;
char	**argv;
{
	SVCXPRT	*transp;
	int	c, longind;
	int	foreground = 0;

#ifndef HOSTS_ACCESS
	fprintf(stderr,
		"\n *** WARNING: This copy of ugidd has been compiled without\n"
		" *** support for host_access checking. This is very risky.\n"
		" *** Please consider recompiling it with access checking.\n");
	sleep(1);
#endif

	while ((c = getopt_long(argc, argv, "d", longopts, &longind)) != EOF) {
		switch (c) {
		case 'd':
			foreground = 1;
			enable_logging("ugid");
			break;
		default:
			usage();
		}
	}

        (void)pmap_unset(UGIDPROG, UGIDVERS);

        transp = svcudp_create(RPC_ANYSOCK);
        if (transp == NULL) {
                (void)fprintf(stderr, "cannot create udp service.\n");
                exit(1);
        }
        if (!svc_register(transp, UGIDPROG, UGIDVERS, ugidprog_1, IPPROTO_UDP)) {
                fprintf(stderr, "unable to register (UGIDPROG, UGIDVERS, UDP)\n");
                exit(1);
        }

        transp = svctcp_create(RPC_ANYSOCK, 0, 0);
        if (transp == NULL) {
                fprintf(stderr, "cannot create tcp service.\n");
                exit(1);
        }
        if (!svc_register(transp, UGIDPROG, UGIDVERS, ugidprog_1, IPPROTO_TCP)) {
                fprintf(stderr, "unable to register (UGIDPROG, UGIDVERS, TCP)\n");
                exit(1);
        }

	if (!foreground) {
		if ((c = fork()) > 0)
			exit(0);
		if (c < 0) {
			fprintf(stderr, "ugidd: cannot fork: %s\n",
						strerror(errno));
			exit(-1);
		}
		close(0);
		close(1);
		close(2);
#ifdef HAVE_SETSID
		setsid();
#else
		{
			int fd;

			if ((fd = open("/dev/tty", O_RDWR)) >= 0) {
				ioctl(fd, TIOCNOTTY, (char *) NULL);
				close(fd);
			}
		}
#endif
	}

	log_open("ugidd", foreground);

	svc_run();
	Dprintf(L_ERROR, "svc_run returned\n");
	return 1;
}

static void
usage()
{
	fprintf(stderr, "rpc.ugidd: [-d]\n");
	exit (2);
}

static void
ugidprog_1(rqstp, transp)
	struct svc_req *rqstp;
	SVCXPRT *transp;
{
	union {
		int authenticate_1_arg;
		ugname name_uid_1_arg;
		ugname group_gid_1_arg;
		int uid_name_1_arg;
		int gid_group_1_arg;
	} argument;
	char *result;
	xdrproc_t xdr_argument, xdr_result;
	char *(*local)();

	if (!check_client(rqstp))
		return;

	switch (rqstp->rq_proc) {
	case NULLPROC:
		(void)svc_sendreply(transp, (xdrproc_t)xdr_void, (char *)NULL);
		return;

	case AUTHENTICATE:
		xdr_argument = (xdrproc_t) xdr_int;
		xdr_result = (xdrproc_t) xdr_int;
		local = (char *(*)()) authenticate_1_svc;
		break;

	case NAME_UID:
		xdr_argument = (xdrproc_t) xdr_ugname;
		xdr_result = (xdrproc_t) xdr_int;
		local = (char *(*)()) name_uid_1_svc;
		break;

	case GROUP_GID:
		xdr_argument = (xdrproc_t) xdr_ugname;
		xdr_result = (xdrproc_t) xdr_int;
		local = (char *(*)()) group_gid_1_svc;
		break;

	case UID_NAME:
		xdr_argument = (xdrproc_t) xdr_int;
		xdr_result = (xdrproc_t) xdr_ugname;
		local = (char *(*)()) uid_name_1_svc;
		break;

	case GID_GROUP:
		xdr_argument = (xdrproc_t) xdr_int;
		xdr_result = (xdrproc_t) xdr_ugname;
		local = (char *(*)()) gid_group_1_svc;
		break;

	default:
		svcerr_noproc(transp);
		return;
	}
	bzero((char *)&argument, sizeof(argument));
	if (!svc_getargs(transp, xdr_argument, &argument)) {
		svcerr_decode(transp);
		return;
	}
	result = (*local)(&argument, rqstp);
	if (result != NULL && !svc_sendreply(transp, xdr_result, result)) {
		svcerr_systemerr(transp);
	}
	if (!svc_freeargs(transp, xdr_argument, &argument)) {
		(void)fprintf(stderr, "unable to free arguments\n");
		exit(1);
	}
}

int *
authenticate_1_svc(argp, rqstp)
	int *argp;
	struct svc_req *rqstp;
{
	static int res;
	int	s;
	struct sockaddr_in	sendaddr, destaddr;
	int	dummy;
	short	lport;

	bzero(&res, sizeof res);
	destaddr = *svc_getcaller(rqstp->rq_xprt);
	destaddr.sin_port = htons(*argp);
	if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
		goto bad;
	setsockopt(s, SOL_SOCKET, SO_LINGER, 0, 0);
	bzero((char *) &sendaddr, sizeof sendaddr);
	/* find a reserved port */
	lport = IPPORT_RESERVED - 1;
	sendaddr.sin_family = AF_INET;
	sendaddr.sin_addr.s_addr = INADDR_ANY;
	for (;;)
	{
		sendaddr.sin_port = htons((u_short)lport);
		if (bind(s, (struct sockaddr *)&sendaddr, sizeof sendaddr) >= 0)
			break;
		if (errno != EADDRINUSE && EADDRNOTAVAIL)
			goto bad;
		lport--;
		if (lport <= IPPORT_RESERVED / 2)
			/* give up */
			break;
	}
	if (sendto(s, &dummy, sizeof dummy, 0,
			(struct sockaddr *)&destaddr, sizeof destaddr) < 0)
		goto bad;

	close(s);
	res = 0;
	return (&res);
    bad:
	close(s);
	res = errno == 0 ? -1 : errno;
	return (&res);
}

int *
name_uid_1_svc(argp, rqstp)
	ugname *argp;
	struct svc_req *rqstp;
{
	static int res;
	struct passwd	*pw;

	bzero(&res, sizeof(res));
	if ((pw = getpwnam(*argp)) == NULL)
		res = NOBODY;
	else
		res = pw->pw_uid;

	return (&res);
}


int *
group_gid_1_svc(argp, rqstp)
	ugname *argp;
	struct svc_req *rqstp;
{
	static int res;
	struct group	*gr;

	bzero(&res, sizeof(res));
	if ((gr = getgrnam(*argp)) == NULL)
		res = NOBODY;
	else
		res = gr->gr_gid;

	return (&res);
}


ugname *
uid_name_1_svc(argp, rqstp)
	int *argp;
	struct svc_req *rqstp;
{
	static ugname res;
	struct passwd	*pw;

	bzero(&res, sizeof(res));
	if ((pw = getpwuid(*argp)) == NULL)
		res = "";
	else
		res = pw->pw_name;

	return (&res);
}


ugname *
gid_group_1_svc(argp, rqstp)
	int *argp;
	struct svc_req *rqstp;
{
	static ugname res;
	struct group	*gr;

	bzero(&res, sizeof(res));
	if ((gr = getgrgid(*argp)) == NULL)
		res = "";
	else
		res = gr->gr_name;

	return (&res);
}

#ifdef HOSTS_ACCESS
int
check_client(rqstp)
	struct svc_req *rqstp;
{
	struct sockaddr_in *sin;
	struct in_addr	   addr;
	struct ugid_clnt   *up;
	char		   addr_s[20];
	int		   hash;

	sin = svc_getcaller(rqstp->rq_xprt);
	addr = (struct in_addr)(sin->sin_addr);
	strcpy(addr_s, inet_ntoa(addr));

	Dprintf(D_UGID, "call to procedure %d from client %s\n", 
					rqstp->rq_proc, addr_s);
	if (rqstp->rq_proc == NULLPROC)
		return 1;

	if (ntohs(sin->sin_port) >= IPPORT_RESERVED) {
		Dprintf(L_ERROR,
			"client %s connected from unreserved port %d\n",
					addr_s, ntohs(sin->sin_port));
		return 0;
	}

	hash = IP_HASH(addr.s_addr);
	for (up = clients[hash]; up != NULL; up = up->next) 
		if (up->clnt_addr.s_addr == addr.s_addr) break;

	if (up == NULL) {
		up = (ugid_clnt *) xmalloc(sizeof(*up));
		up->next = clients[hash];
		clients[hash] = up;

		up->clnt_addr = addr;
		up->status = hosts_ctl("ugidd", "unknown", addr_s, "root");
	}

	if (!up->status) {
		Dprintf(L_ERROR,
			"access from host %s rejected\n", addr_s);
	}

	return up->status;
}

static RETSIGTYPE
clnt_flush(int sig)
{
	ugid_clnt	*up, *next;
	int		i;

	Dprintf(D_UGID, "flushed all clients\n");
	for (i = 0; i < IP_HASH_MASK+1; i++) {
		for (up = clients[i]; up != NULL; up = next) {
			next = up->next;
			free (up);
		}
		clients[i] = NULL;
	}
	signal (SIGINT, clnt_flush);
}
#endif


#else /* REAL_UGIDD */

#include <stdio.h>

int
main(argc, argv)
	int	argc;
	char	**argv;
{
	fprintf(stderr, 
	"\nThis copy of the Universal NFS server has been compiled without\n");
	fprintf(stderr, "support for the ugidd RPC uid/gid map daemon.\n");
	fprintf(stderr, "This is a dummy program.\n");
	return 1;
}

#endif /* REAL_UGIDD */
