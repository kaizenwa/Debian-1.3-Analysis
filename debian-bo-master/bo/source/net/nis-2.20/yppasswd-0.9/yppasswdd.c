/* 
 * yppasswdd.c	RPC Server routines.
 *
 * Copyright 1994, 1995, 1996 Olaf Kirch, <okir@monad.swb.de>
 *
 * This program is covered by the GNU General Public License, version 2.
 * It is provided in the hope that it is useful. However, the author
 * disclaims ALL WARRANTIES, expressed or implied. See the GPL for details.
 */

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <termios.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>
#ifdef NO_PATHS_H
#include <paths.h>
#endif
#ifdef NO_GETOPT_H
#include <getopt.h>
#endif
#include <syslog.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include "yppasswd.h"
#include "version.h"


/* How often to retry locking the passwd file...
 */
#define MAX_RETRIES 5


static char *program_name = "";
static char *version = "yppasswdd " VERSION;

int	use_shadow = 0;
int	allow_chsh = 0,
	allow_chfn = 0;

#define xprt_addr(xprt)	(svc_getcaller(xprt)->sin_addr)
#define xprt_port(xprt)	ntohs(svc_getcaller(xprt)->sin_port)
void	yppasswdprog_1(struct svc_req *rqstp, SVCXPRT *transp);
void	reaper(int sig);

/*==============================================================*
 * RPC dispatch function
 *==============================================================*/
void
yppasswdprog_1(struct svc_req *rqstp, SVCXPRT *transp)
{
    union {
        yppasswd yppasswdproc_update_1_arg;
    } argument;
    char        *result;
    xdrproc_t   xdr_argument, xdr_result;
    char *(*local)();

    switch (rqstp->rq_proc) {
    case NULLPROC:
        svc_sendreply(transp, (xdrproc_t)xdr_void, (char *)NULL);
        return;

    case YPPASSWDPROC_UPDATE:
        xdr_argument = (xdrproc_t) xdr_yppasswd;
        xdr_result   = (xdrproc_t) xdr_int;
#ifndef SHADOWPWD
        local = (char *(*)()) yppasswdproc_pwupdate_1;
#else
        if (use_shadow) {
            local = (char *(*)()) yppasswdproc_spwupdate_1;
        } else {
            local = (char *(*)()) yppasswdproc_pwupdate_1;
        }
#endif
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
    if (result != NULL
     && !svc_sendreply(transp, (xdrproc_t)xdr_result, result)) {
        svcerr_systemerr(transp);
    }
    if (!svc_freeargs(transp, xdr_argument, &argument)) {
        (void)fprintf(stderr, "unable to free arguments\n");
        exit(1);
    }
}

static void
usage(FILE *fp, int n)
{
    fprintf (fp, "usage: %s [-h] [-v]\n", program_name );
    exit(n);
}

void
reaper(int sig)
{
    wait(NULL);
}

void
install_reaper(void)
{
    struct sigaction act, oact;
    sigset_t s;

    sigemptyset(&s);
    act.sa_handler = reaper;
    act.sa_mask = s;
    act.sa_flags = SA_RESTART;
    sigaction( SIGCHLD, &act, &oact );
}


int
main(int argc, char **argv)
{
    SVCXPRT *	transp;
    char *	fname = NULL;
    char *	sp;
    int		opterr;
    int		c;

    program_name = argv[0];
    if ((sp = strrchr(program_name, '/')) != NULL) {
    	program_name = ++sp;
    }

    /* Parse the command line options and arguments. */
    opterr = 0;
    while ((c = getopt(argc, argv, "e:f:hsv")) != EOF)
        switch (c) {
	case 'e':
	    if (!strcmp(optarg, "chsh"))
	    	allow_chsh = 1;
	    else if (!strcmp(optarg, "chfn"))
	    	allow_chfn = 1;
	    else
	    	usage(stderr, 1);
	    break;
	case 'f':
	    fname = optarg;
	    break;
        case 'h':
            usage(stdout, 0);
            break;
        case 's':
#ifdef SHADOWPWD
            use_shadow = 1;
            break;
#else
	    fprintf(stderr, "%s: support for /etc/shadow not available.\n",
	    			program_name);
	    fprintf(stderr, "\tTo support shadow-style passwords,\n"
	    		    "\tplease recompile the program.\n");
	    exit(2);
#endif
        case 'v':
            printf("%s\n", version);
            exit(0);
        case 0:
            break;
        case '?':
        default:
            usage(stderr, 1);
        }

    /* No more arguments allowed. */
    if (optind != argc)
        usage(stderr, 1);

    if (fname && use_shadow) {
	path_shadow = fname;
    } else if (fname) {
    	path_passwd = fname;
    }

#ifndef RPC_SVC_FG
    /* We first fork off a child. */
    if ((c = fork()) > 0)
        exit(0);
    if (c < 0) {
        fprintf(stderr, "yppasswdd: cannot fork: %s\n", strerror(errno));
        exit(-1);
    }
    /* Now we remove ourselves from the foreground. */
    close(0);
    close(1);
    close(2);
#ifdef TIOCNOTTY
    if ((c = open("/dev/tty", O_RDWR)) >= 0) {
        ioctl(c, TIOCNOTTY, (char *) NULL);
        close(c);
    }
#else
    setsid();
#endif
#endif /* not RPC_SVC_FG */

    /* Initialize logging.
     */
    openlog ( "yppasswdd", LOG_PID, LOG_AUTH );

    /* Register a signal handler to reap children after they terminated
     */
    install_reaper();

    /*
     * Create the RPC server
     */
    pmap_unset(YPPASSWDPROG, YPPASSWDVERS);

    transp = svcudp_create(RPC_ANYSOCK);
    if (transp == NULL) {
        fprintf(stderr, "cannot create udp service.\n");
        exit(1);
    }
    if (!svc_register(transp, YPPASSWDPROG, YPPASSWDVERS, yppasswdprog_1,
            IPPROTO_UDP)) {
        fprintf(stderr, "unable to register yppaswdd udp service.\n");
        exit(1);
    }

    /*
     * Run the server
     */
    svc_run();
    fprintf(stderr, "svc_run returned\n");

    return 1;
}

