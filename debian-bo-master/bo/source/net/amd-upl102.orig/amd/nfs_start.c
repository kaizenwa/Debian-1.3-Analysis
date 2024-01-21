/*
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
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
 * $Id: nfs_start.c,v 5.2.2.1 1992/02/09 15:08:51 jsp beta $
 *
 */

#include "am.h"
#include "amq.h"
#include <sys/signal.h>
#include <setjmp.h>
extern jmp_buf select_intr;
extern int select_intr_valid;

#ifdef __svr4__
#include <rpc/pmap_prot.h>
#endif /* __svr4__ */

#ifndef AMQ_SIZE
#define AMQ_SIZE 16384
#endif

#ifndef SELECT_MAXWAIT
#define SELECT_MAXWAIT 16
#endif

#ifdef HAS_TFS
/*
 * Use replacement for RPC/UDP transport
 * so that we do NFS gatewaying.
 */
#define	svcudp_create svcudp2_create
extern SVCXPRT *svcudp2_create P((int));
#endif /* HAS_TFS */

extern void nfs_program_2();
extern void amq_program_1();

unsigned short nfs_port;
SVCXPRT *nfsxprt;
#ifdef __svr4__
struct netconfig *nfsncp;
#endif /* __svr4__ */

extern int fwd_sock;
int max_fds = -1;

#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
extern sigset_t masked_sigs;
#else
#define	MASKED_SIGS	(sigmask(SIGINT)|sigmask(SIGTERM)|sigmask(SIGCHLD)|sigmask(SIGHUP))
#endif

#ifdef DEBUG
/*
 * Check that we are not burning resources
 */
static void checkup(P_void)
{

static int max_fd = 0;
static char *max_mem = 0;

	int next_fd = dup(0);
#ifdef		__hpux
	extern void *sbrk P((int));
#else
	extern caddr_t sbrk P((int));
#endif		/* __hpux */
	caddr_t next_mem = sbrk(0);
	close(next_fd);

	/*if (max_fd < 0) {
		max_fd = next_fd;
	} else*/ if (max_fd < next_fd) {
		dlog("%d new fds allocated; total is %d",
			next_fd - max_fd, next_fd);
		max_fd = next_fd;
	}

	/*if (max_mem == 0) {
		max_mem = next_mem;
	} else*/ if (max_mem < next_mem) {
#ifdef __svr4__
		dlog("%#x bytes of memory allocated; total is %#x",
			next_mem - max_mem,
			next_mem);
#else
		dlog("%#x bytes of memory allocated; total is %#x (%d pages)",
			next_mem - max_mem,
			next_mem,
			((int)next_mem+getpagesize()-1)/getpagesize());
#endif /* __svr4__ */
		max_mem = next_mem;

	}
}
#endif /* DEBUG */

static int do_select(smask, fds, fdp, tvp)
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
sigset_t smask;
#else
int smask;
#endif
int fds;
fd_set *fdp;
struct timeval *tvp;
{
#if defined(HPUX_9) && defined(__GNUC__)
/*
 * This is an attempt to fix some warnings from gcc in nfs_start.h
 * HPUX 9.01 declares fd_set and such in header files, but the select()
 * system call seems to want an int, according to the man page select(2).
 * Furthermore, the header files for HPUX-9.01 don't even prototype the
 * syscall, but set it as "extern int select()".  HP's fault.
 * Erez Zadok <ezk@cs.columbia.edu>
 */
extern int select();
#endif /* defined(HPUX_9) && defined(__GNUC__) */

	int sig;
	int nsel;
	if (sig = setjmp(select_intr)) {
		select_intr_valid = 0;
		/* Got a signal */
		switch (sig) {
		case SIGINT:
		case SIGTERM:
			amd_state = Finishing;
			reschedule_timeout_mp();
			break;
		}
		nsel = -1;
		errno = EINTR;
	} else {
		select_intr_valid = 1;
		/*
		 * Invalidate the current clock value
		 */
		clock_valid = 0;
		/*
		 * Allow interrupts.  If a signal
		 * occurs, then it will cause a longjmp
		 * up above.
		 */
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
		sigprocmask(SIG_SETMASK, &smask, NULL);
#else
		(void) sigsetmask(smask);
#endif
		/*
		 * Wait for input
		 */
		nsel = select(fds, fdp, (fd_set *) 0, (fd_set *) 0,
			      tvp->tv_sec ? tvp : (struct timeval *) 0);
	}

#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
	sigprocmask(SIG_BLOCK, &masked_sigs, NULL);
#else
	(void) sigblock(MASKED_SIGS);
#endif

	/*
	 * Perhaps reload the cache?
	 */
	if (do_mapc_reload < clocktime()) {
		mapc_reload();
		do_mapc_reload = clocktime() + ONE_HOUR;
	}
	return nsel;
}

/*
 * Determine whether anything is left in
 * the RPC input queue.
 */
static int rpc_pending_now()
{
#if defined(HPUX_9) && defined(__GNUC__)
/*
 * This is an attempt to fix some warnings from gcc in nfs_start.h
 * HPUX 9.01 declares fd_set and such in header files, but the select()
 * system call seems to want an int, according to the man page select(2).
 * Furthermore, the header files for HPUX-9.01 don't even prototype the
 * syscall, but set it as "extern int select()".  HP's fault.
 * Erez Zadok <ezk@cs.columbia.edu>
 */
extern int select();
#endif /* defined(HPUX_9) && defined(__GNUC__) */

	struct timeval tvv;
	int nsel;
#ifdef FD_SET
	fd_set readfds;

	FD_ZERO(&readfds);
	FD_SET(fwd_sock, &readfds);
#else
	int readfds = (1 << fwd_sock);
#endif /* FD_SET */

	tvv.tv_sec = tvv.tv_usec = 0;
	nsel = select(max_fds+1, &readfds, (fd_set *) 0, (fd_set *) 0, &tvv);
	if (nsel < 1)
		return(0);
#ifdef FD_SET
	if (FD_ISSET(fwd_sock, &readfds))
		return(1);
#else
	if (readfds & (1 << fwd_sock))
		return(1);
#endif
	return(0);
}

static serv_state run_rpc(P_void)
{
#ifdef GETDTABLESIZE
	int dtbsz = getdtablesize();
#else
	int dtbsz = max_fds + 1;
#endif
#if defined(_POSIX_SOURCE)  && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) ||  defined(__svr4__)
	sigset_t smask;
	sigprocmask(SIG_BLOCK, &masked_sigs, &smask);
#else
	int smask = sigblock(MASKED_SIGS);
#endif

	next_softclock = clocktime();

	amd_state = Run;

	/*
	 * Keep on trucking while we are in Run mode.  This state
	 * is switched to Quit after all the file systems have
	 * been unmounted.
	 */
	while ((int)amd_state <= (int)Finishing) {
		struct timeval tvv;
		int nsel;
		time_t now;
#ifdef RPC_4
		fd_set readfds;
#ifdef bogus
		readfds = svc_fdset;
#else
		bcopy(&svc_fdset, &readfds, sizeof svc_fdset);
#endif
		FD_SET(fwd_sock, &readfds);
#else
#ifdef FD_SET
		fd_set readfds;
		FD_ZERO(&readfds);
		readfds.fds_bits[0] = svc_fds;
		FD_SET(fwd_sock, &readfds);
#else
		int readfds = svc_fds | (1 << fwd_sock);
#endif /* FD_SET */
#endif /* RPC_4 */

#ifdef DEBUG
		checkup();
#endif /* DEBUG */

		/*
		 * If the full timeout code is not called,
		 * then recompute the time delta manually.
		 */
		now = clocktime();

		if (next_softclock <= now) {
			if (amd_state == Finishing)
				umount_exported();
			tvv.tv_sec = softclock();
		} else {
			tvv.tv_sec = next_softclock - now;
		}
		tvv.tv_usec = 0;

		if (amd_state == Finishing && last_used_map < 0) {
			flush_mntfs();
			amd_state = Quit;
			break;
		}

		if (tvv.tv_sec <= 0) tvv.tv_sec = SELECT_MAXWAIT;
#ifdef DEBUG
		if (tvv.tv_sec)
			dlog("Select waits for %ds", tvv.tv_sec);
		else
			dlog("Select waits for Godot");
#endif /* DEBUG */

		nsel = do_select(smask, dtbsz, &readfds, &tvv);


		switch (nsel) {
		case -1:
			if (errno == EINTR) {
#ifdef DEBUG
				dlog("select interrupted");
#endif /* DEBUG */
				continue;
			}
			perror("select");
			break;

		case 0:
#ifdef DEBUG
			/*dlog("select returned 0");*/
#endif /* DEBUG */
			break;

		default:
			/* Read all pending NFS responses at once to avoid
			   having responses queue up as a consequence of
			   retransmissions. */
#ifdef FD_SET
			if (FD_ISSET(fwd_sock, &readfds)) {
				FD_CLR(fwd_sock, &readfds);
#else
			if (readfds & (1 << fwd_sock)) {
				readfds &= ~(1 << fwd_sock);
#endif
				--nsel;	
				do {
					fwd_reply();
				} while (rpc_pending_now() > 0);
			}

			if (nsel) {
				/*
				 * Anything left must be a normal
				 * RPC request.
				 */
#ifdef RPC_4
				svc_getreqset(&readfds);
#else
#ifdef FD_SET
				svc_getreq(readfds.fds_bits[0]);
#else
				svc_getreq(readfds);
#endif /* FD_SET */
#endif /* RPC_4 */
			}
			break;
		}
	}

#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__)
		sigprocmask(SIG_SETMASK, &smask, NULL);
#else
		(void) sigsetmask(smask);
#endif

	if (amd_state == Quit)
		amd_state = Done;

	return amd_state;
}

#ifdef __svr4__
static int bindnfs_port()
#else
static int bindnfs_port(so)
int so;
#endif /* __svr4__ */
{
	unsigned short port;
#ifdef __svr4__
	int error = bind_resv_port2(&port); 
#else
	int error = bind_resv_port(so, &port);
#endif /* __svr4__ */
	if (error == 0)
		nfs_port = port;
	return error;
}

void unregister_amq(P_void)
{
#ifdef DEBUG
	Debug(D_AMQ)
#endif /* DEBUG */
#ifdef __svr4__
	(void) rpcb_unset(AMQ_PROGRAM, AMQ_VERSION, (struct netconfig *) NULL);
#else
	(void) pmap_unset(AMQ_PROGRAM, AMQ_VERSION);
#endif /* __svr4__ */
}

int mount_automounter(ppid)
int ppid;
{
	/*
	 * Old code replaced by rpc-trash patch.
	 * Erez Zadok <ezk@cs.columbia.edu>
  	int so = socket(AF_INET, SOCK_DGRAM, 0);
	 */
	SVCXPRT *amqp = NULL;
	int nmount;
	int soNFS;
	int soAMQ;

#ifdef __svr4__
	struct netconfig *amqncp;

	/*
	 * Create the nfs service for amd
	 */
	if ((nfsncp = getnetconfigent("ticlts")) == (struct netconfig *) NULL) {
	  plog(XLOG_ERROR, "cannot getnetconfigent for ticlts");
	  return 1;
	}
	if ((nfsxprt = svc_tli_create(RPC_ANYFD, nfsncp, (struct t_bind *) NULL, 0, 0)) == (SVCXPRT *) NULL) {
	  plog(XLOG_ERROR, "cannot create tli service for amd");
	  return 1;
	}

	/*
	 * Get the service file descriptor and check its number to see if
	 * the t_open failed.  If it succeeded, then go on to binding to a
	 * reserved nfs port.
	 */
	soNFS = nfsxprt->xp_fd;
	if (soNFS < 0 || bindnfs_port() < 0) {
	  plog(XLOG_ERROR, "Can't create privileged nfs port");
	  return 1;
	}

	if (svc_reg(nfsxprt, NFS_PROGRAM, NFS_VERSION, nfs_program_2, (struct netconfig *) NULL) != 1) {
	  plog(XLOG_ERROR, "could not register amd nfs service");
	  return 1;
	}

	/*
	 * (partially) create the amq service for amd
	 * to be completed further in this function.
	 */
	if ((amqncp = getnetconfigent(NC_UDP)) == (struct netconfig *) NULL) {
	  plog(XLOG_ERROR, "cannot getnetconfigent for %s", NC_UDP);
	  return 1;
	}
	if ((amqp = svc_tli_create(RPC_ANYFD, amqncp, (struct t_bind *) NULL, 0, 0)) == (SVCXPRT *) NULL) {
	  plog(XLOG_ERROR, "cannot create tli service for amq");
	  return 1;
	}
	soAMQ = amqp->xp_fd;

#else /* __svr4__ is not defined */

	soNFS = socket(AF_INET, SOCK_DGRAM, 0);
	soAMQ = socket(AF_INET, SOCK_DGRAM, 0);

	if (soNFS < 0 || bindnfs_port(soNFS) < 0) {
		perror("Can't create privileged nfs port");
		return 1;
	}

	if ((nfsxprt = svcudp_create(soNFS)) == NULL ||
#ifdef HAS_TFS
			(amqp = svcudp_create(soAMQ)) == NULL) {
#else
			(amqp = svcudp_bufcreate(soAMQ, AMQ_SIZE, AMQ_SIZE)) == NULL) {
#endif
		plog(XLOG_FATAL, "cannot create rpc/udp service");
		plog(XLOG_FATAL, "... and nfsxprt=%x and amqp=%x", nfsxprt, amqp );
		return 2;
	}

	if (!svc_register(nfsxprt, NFS_PROGRAM, NFS_VERSION, nfs_program_2, 0)) {
		plog(XLOG_FATAL, "unable to register (NFS_PROGRAM, NFS_VERSION, 0)");
		return 3;
	}
#endif /* __svr4__ */

	/*
	 * Start RPC forwarding
	 */
	if (fwd_init() != 0)
		return 3;

	/*
	 * One or other of so, fwd_sock
	 * must be the highest fd on
	 * which to select.
	 */
	if (soNFS > max_fds)
		max_fds = soNFS;
	if (soAMQ > max_fds)
		max_fds = soAMQ;
	if (fwd_sock > max_fds)
		max_fds = fwd_sock;

	/*
	 * Construct the root automount node
	 */
	make_root_node();

	/*
	 * Pick up the pieces from a previous run
	 * This is likely to (indirectly) need the rpc_fwd package
	 * so it *must* come after the call to fwd_init().
	 */
	if (restart_existing_mounts)
		restart();

	/*
	 * Mount the top-level auto-mountpoints
	 */
	nmount = mount_exported();

	/*
	 * Now safe to tell parent that we are up and running
	 */
	if (ppid)
		kill(ppid, SIGQUIT);

	if (nmount == 0) {
		plog(XLOG_FATAL, "No work to do - quitting");
		amd_state = Done;
		return 0;
	}

#ifdef DEBUG
	Debug(D_AMQ) {
#endif /* DEBUG */
#ifdef __svr4__
	/*
	 * Complete registration of amq
	 */
	unregister_amq();
	if (svc_reg(amqp, AMQ_PROGRAM, AMQ_VERSION, amq_program_1, amqncp) != 1) {
	  plog(XLOG_FATAL, "unable to register (AMQ_PROGRAM, AMQ_VERSION, udp)");
	  return 3;
	}
#else
	/*
	 * Register with amq
	 */
	unregister_amq();
	if (!svc_register(amqp, AMQ_PROGRAM, AMQ_VERSION, amq_program_1, IPPROTO_UDP)) {
		plog(XLOG_FATAL, "unable to register (AMQ_PROGRAM, AMQ_VERSION, udp)");
		return 3;
	}
#endif /* __svr4__ */
#ifdef DEBUG
	}
#endif /* DEBUG */

	/*
	 * Start timeout_mp rolling
	 */
	reschedule_timeout_mp();

	/*
	 * Start the server
	 */
	if (run_rpc() != Done) {
		plog(XLOG_FATAL, "run_rpc failed");
		amd_state = Done;
	}

	return 0;
}
