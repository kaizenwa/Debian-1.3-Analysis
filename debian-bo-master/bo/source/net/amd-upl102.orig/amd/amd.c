/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	%W% (Berkeley) %G%
 *
 * $Id: amd.c,v 5.2.2.1 1992/02/09 15:08:15 jsp beta $
 *
 */

/*
 * Automounter
 */

#include "am.h"
#include <sys/signal.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <setjmp.h>
#ifdef HAS_PLOCK
#include <sys/lock.h>
#endif

char pid_fsname[16 + MAXHOSTNAMELEN];	/* "kiska.southseas.nz:(pid%d)" */
char *progname;				/* "amd" */
#ifdef HAS_HOST
#ifdef HOST_EXEC
char *host_helper;
#endif /* HOST_EXEC */
#endif /* HAS_HOST */
char *auto_dir = "/a";
char *hostdomain = "unknown.domain";
char hostname[MAXHOSTNAMELEN] = "localhost"; /* Hostname */
char hostd[2*MAXHOSTNAMELEN];		/* Host+domain */
char *op_sys = OS_REP;			/* Name of current op_sys */
char *op_sys_ver = OSVER_REP;		/* Name of current op_sys version */
char *arch = ARCH_REP;			/* Name of current architecture */
char *endian = ARCH_ENDIAN;		/* Big or Little endian */
char *PrimNetName;			/* name of primary network */
char *PrimNetNum;			/* number of primary network */
char *SubsNetName;			/* name of subsidiary network */
char *SubsNetNum;			/* number of subsidiary network */
int foreground = 1;			/* This is the top-level server */
int mypid;				/* Current process id */
int immediate_abort;			/* Should close-down unmounts be retried */
struct in_addr myipaddr;		/* (An) IP address of this host */
serv_state amd_state;
struct amd_stats amd_stats;		/* Server statistics */
time_t do_mapc_reload = 0;		/* mapc_reload() call required? */
jmp_buf select_intr;
int select_intr_valid;
int orig_umask;
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
sigset_t masked_sigs;
#endif

/*
 * Signal handler:
 * SIGINT - tells amd to do a full shutdown, including unmounting all
 *	 filesystem.
 * SIGTERM - tells amd to shutdown now.  Just unmounts the automount nodes.
 */
static SIG_HNDL_TYP sigterm(sig)
int sig;
{
#ifdef SYS5_SIGNALS
	signal(sig, sigterm);
#endif /* SYS5_SIGNALS */

	switch (sig) {
	case SIGINT:
		immediate_abort = 15;
		break;

	case SIGTERM:
		immediate_abort = -1;
		/* fall through... */

	default:
		plog(XLOG_WARNING, "WARNING: automounter going down on signal %d", sig);
		break;
	}
	if (select_intr_valid)
		longjmp(select_intr, sig);
}

/*
 * Hook for cache reload.
 * When a SIGHUP arrives it schedules a call to mapc_reload
 */
/*ARGSUSED*/
static SIG_HNDL_TYP sighup(sig)
int sig;
{
#ifdef SYS5_SIGNALS
	signal(sig, sighup);
#endif /* SYS5_SIGNALS */

#ifdef DEBUG
	if (sig != SIGHUP)
		dlog("spurious call to sighup");
#endif /* DEBUG */
	/*
	 * Force a reload by zero'ing the timer
	 */
	if (amd_state == Run)
		do_mapc_reload = 0;
}

/*ARGSUSED*/
static SIG_HNDL_TYP parent_exit(sig)
int sig;
{
	exit(0);
}

static int daemon_mode(P_void)
{
	int bgpid;

#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
	struct sigaction sa, osa;

	sa.sa_handler = parent_exit;
	sa.sa_flags = 0;
	sigemptyset(&(sa.sa_mask));
	sigaddset(&(sa.sa_mask), SIGQUIT);
	sigaction(SIGQUIT, &sa, &osa);
#else
	signal(SIGQUIT, parent_exit);
#endif

	bgpid = background();

	if (bgpid != 0) {
		/*
		 * Now wait for the automount points to
		 * complete.
		 */
		for (;;)
			pause();
#ifdef notdef
		exit(0);	/* never reaches here */
#endif
	}

#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
	sigaction(SIGQUIT, &osa, NULL);
#else
	signal(SIGQUIT, SIG_DFL);
#endif

	if (print_pid) {
		printf("%d\n", mypid);
		fflush(stdout);
		(void) fclose(stdout);
	}

	/*
	 * Pretend we are in the foreground again
	 */
	foreground = 1;

#ifdef TIOCNOTTY
	{
		int t = open("/dev/tty", O_RDWR);
		if (t < 0) {
			if (errno != ENXIO)	/* not an error if already no controlling tty */
				plog(XLOG_WARNING, "Could not open controlling tty: %m");
		} else {
			if (ioctl(t, TIOCNOTTY, 0) < 0 && errno != ENOTTY)
				plog(XLOG_WARNING, "Could not disassociate tty (TIOCNOTTY): %m");
			(void) close(t);
		}
	}
#else
	(void) setpgrp();
#endif /* TIOCNOTTY */

	return getppid();
}

int main(argc, argv)
int argc;
char *argv[];
{
	char *domdot;
	int ppid = 0;
	int error;
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
	struct sigaction sa;
#endif
#ifdef __svr4__
	int ret;
	void *handlep;
	struct netconfig *ncp;
	struct nd_addrlist *addrs = (struct nd_addrlist *) NULL;
	struct nd_hostserv service;
#else
	struct sockaddr_in sin;
#endif /* __svr4__ */


	/*
	 * Make sure some built-in assumptions are true before we start
	 */
	assert(sizeof(nfscookie) >= sizeof (unsigned int));
	assert(sizeof(int) >= 4);

	/*
	 * Set processing status.
	 */
	amd_state = Start;

	/*
	 * Determine program name
	 */
	if (argv[0]) {
		progname = strrchr(argv[0], '/');
		if (progname && progname[1])
			progname++;
		else
			progname = argv[0];
	}

	if (!progname)
		progname = "amd";

	/*
	 * Initialise process id.  This is kept
	 * cached since it is used for generating
	 * and using file handles.
	 */
	mypid = getpid();

	/*
	 * Get local machine name
	 */
	if (gethostname(hostname, sizeof(hostname)) < 0) {
		plog(XLOG_FATAL, "gethostname: %m");
		going_down(1);
	}
	/*
	 * Check it makes sense
	 */
	if (!*hostname) {
		plog(XLOG_FATAL, "host name is not set");
		going_down(1);
	}
	/*
	 * Partially initialise hostd[].  This
	 * is completed in get_args().
	 */
	if (domdot = strchr(hostname, '.')) {
		/*
		 * Hostname already contains domainname.
		 * Split out hostname and domainname
		 * components
		 */
		*domdot++ = '\0';
		hostdomain = domdot;
	}
	strcpy(hostd, hostname);

	/*
	 * Trap interrupts for shutdowns.
	 */
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
	sa.sa_handler = sigterm;
	sa.sa_flags = 0;
	sigemptyset(&(sa.sa_mask));
	sigaddset(&(sa.sa_mask), SIGINT);
	sigaddset(&(sa.sa_mask), SIGTERM);
	sigaction(SIGINT, &sa, NULL);
	sigaction(SIGTERM, &sa, NULL);
#else
	(void) signal(SIGINT, sigterm);
#endif

	/*
	 * Hangups tell us to reload the cache
	 */
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
	sa.sa_handler = sighup;
	sa.sa_flags = 0;
	sigemptyset(&(sa.sa_mask));
	sigaddset(&(sa.sa_mask), SIGHUP);
	sigaction(SIGHUP, &sa, NULL);
#else
	(void) signal(SIGHUP, sighup);
#endif

	/*
	 * Trap Terminate so that we can shutdown gracefully (some chance)
	 */
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
	sa.sa_handler = sigterm;
	sa.sa_flags = 0;
	sigemptyset(&(sa.sa_mask));
	sigaddset(&(sa.sa_mask), SIGTERM);
	sigaction(SIGTERM, &sa, NULL);
#else
	(void) signal(SIGTERM, sigterm);
#endif

	/*
	 * Trap Death-of-a-child.  These allow us to
	 * pick up the exit status of backgrounded mounts.
	 * See "sched.c".
	 */
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
	sa.sa_handler = sigchld;
	sa.sa_flags = 0;
	sigemptyset(&(sa.sa_mask));
	sigaddset(&(sa.sa_mask), SIGCHLD);
	sigaction(SIGCHLD, &sa, NULL);

	/*
	 * construct global "masked_sigs" used in nfs_start.c 
	 */
	sigemptyset(&masked_sigs);
	sigaddset(&masked_sigs, SIGHUP);
	sigaddset(&masked_sigs, SIGCHLD);
	sigaddset(&masked_sigs, SIGTERM);
	sigaddset(&masked_sigs, SIGINT);
#else
	(void) signal(SIGCHLD, sigchld);
#endif

	/*
	 * Fix-up any umask problems.  Most systems default
	 * to 002 which is not too convenient for our purposes
	 */
	orig_umask = umask(0);

	/*
	 * Figure out primary network name
	 */
	getwire(&PrimNetName, &PrimNetNum, &SubsNetName, &SubsNetNum);

	/*
	 * Determine command-line arguments
	 */
	get_args(argc, argv);

	/*
	 * Get our own IP address so that we
	 * can mount the automounter.
	 */
#ifdef __svr4__
	handlep = setnetconfig();
	ncp = getnetconfig(handlep);
	service.h_host = HOST_SELF_CONNECT;
	service.h_serv = (char *) NULL;
	ret = netdir_getbyname(ncp, &service, &addrs);
	if (ret || !addrs || addrs->n_cnt < 1) {
	  plog(XLOG_FATAL, "cannot get local host address. using 127.0.0.1");
	  myipaddr.s_addr = 0x7f000001;
	} else {
	  /*
	   * XXX: there may be more more than one address for this local
	   * host.  Maybe something can be done with those.
	   */
	  myipaddr.s_addr = ((struct sockaddr_in *)
			     addrs->n_addrs[0].buf)->sin_addr.s_addr;
	}
	endnetconfig(handlep); /* free's up internal resources too */
#else
	bzero((char *)&sin, sizeof(sin));
	get_myaddress(&sin);
	myipaddr.s_addr = sin.sin_addr.s_addr;
#endif /* __svr4__ */
	plog(XLOG_INFO, "My ip addr is 0x%x", myipaddr.s_addr);

	/*
	 * Now check we are root.
	 */
	if (geteuid() != 0) {
		plog(XLOG_FATAL, "Must be root to mount filesystems (euid = %d)", geteuid());
		going_down(1);
	}

	/*
	 * lock process, text and data segment in memory.
	 */
#ifdef HAS_PLOCK
	if (noswap)
		if (plock(PROCLOCK) != 0) {
			plog(XLOG_WARNING, "Couldn't lock process text and data segment in memory: %m");
		} else {
			plog(XLOG_INFO, "Locked process text and data segment in memory: %m");
		}
#endif

#ifdef HAS_NIS_MAPS
	/*
	 * If the domain was specified then bind it here
	 * to circumvent any default bindings that may
	 * be done in the C library.
	 */
	if (domain && yp_bind(domain)) {
		plog(XLOG_FATAL, "Can't bind to domain \"%s\"", domain);
		going_down(1);
	}
#endif /* HAS_NIS_MAPS */

#ifdef DEBUG
	Debug(D_DAEMON)
#endif /* DEBUG */
	ppid = daemon_mode();

	sprintf(pid_fsname, "%s:(pid%d)", hostname, mypid);

	do_mapc_reload = clocktime() + ONE_HOUR;

	/*
	 * Register automounter with system
	 */
	error = mount_automounter(ppid);
	if (error && ppid)
		kill(SIGALRM, ppid);
	going_down(error);

	abort();
}
