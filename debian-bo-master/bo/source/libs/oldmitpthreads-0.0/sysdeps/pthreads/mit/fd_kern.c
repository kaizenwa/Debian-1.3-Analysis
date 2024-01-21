/* ==== fd_kern.c ============================================================
 * Copyright (c) 1993, 1994 by Chris Provenzano, proven@mit.edu
 * All rights reserved.
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
 *  This product includes software developed by Chris Provenzano.
 * 4. The name of Chris Provenzano may not be used to endorse or promote 
 *	  products derived from this software without specific prior written
 *	  permission.
 *
 * THIS SOFTWARE IS PROVIDED BY CHRIS PROVENZANO ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL CHRIS PROVENZANO BE LIABLE FOR ANY 
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
 * SUCH DAMAGE.
 *
 * Description : Deals with the valid kernel fds.
 *
 *  1.00 93/09/27 proven
 *      -Started coding this file.
 *
 *	1.01 93/11/13 proven
 *		-The functions readv() and writev() added.
 *
 * Changes:
 *   96/06/23 schutz@patpserv.epfl.ch
 *   Changed all the 'msg_accrights(len)' to 'msg_control(len)'.
 *   This change is due to the changes made when passing from 4.3BSD to 4.4BSD.
 *   The names in the structure 'msghdr' in file 'socket.h' have changed, but
 *   fortunately not the functionalities.
 */

#ifndef lint
static const char rcsid[] = "fd_kern.c,v 1.6 1995/09/13 02:10:28 hjl Exp";
#endif

#include <pthread.h>
#include <unistd.h>
#ifdef __linux__
#include <dirent.h>
#include <sys/stat.h>
#include <termios.h>
#include <pthread/mit/sys/compat.h>
#include <pthread/mit/machdep.h>
#else
#include <sys/compat.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <stdarg.h>
#include <signal.h>
#include <fcntl.h>
#include <errno.h>
#include <pthread/mit/posix.h>

#include "config.h"

#undef creat

extern int  machdep_sys_select        (int, fd_set *, fd_set *, fd_set *, 
                                       struct timeval * tv);
extern void pthread_resched_resume    (enum pthread_state);
extern void pthread_sched_resume      (void);
extern DIR *__libc_opendir            (__const char *);
extern struct dirent *__libc_readdir  (DIR * __dirp);
extern int  __libc_closedir           (DIR * __dirp);

/*
 * These functions don't seem to be implemented. vch
 */
extern int  __machdep_sys_ioctl       (int, int, struct termios *);
extern int  sleep_cancel              (struct pthread * );
extern void sig_handler_pause         (void);
extern void machdep_sys_sigprocmask   (int, sigset_t *, sigset_t *);
extern int	machdep_sys_lseek         (int, int, int);
extern int  machdep_sys_open          (char *, int, ...);
extern int  machdep_sys_close         (int);
extern int  machdep_sys_pipe          (int *);
extern int  machdep_sys_fchown        (int, uid_t, gid_t);
extern int  machdep_sys_fchmod        (int, mode_t);
extern int  machdep_sys_dup           (int);
extern int  machdep_sys_dup2          (int, int);
extern int  machdep_sys_ftruncate     (int, size_t);
extern int  machdep_sys_ioctl         (int, unsigned long, caddr_t);

/*
 * Prototypes
 */
#if defined (HAVE_SYSCALL_SENDTO) && !defined (HAVE_SYSCALL_SEND)
pthread_ssize_t      machdep_sys_send      (int, const void *, size_t, int);
#endif
#if defined (HAVE_SYSCALL_RECVFROM) && !defined (HAVE_SYSCALL_RECV)
pthread_ssize_t      machdep_sys_recv      (int, void *, size_t, int);
#endif
void                 fd_kern_poll          (void);
void                 fd_kern_wait          (void);
pthread_ssize_t      __fd_kern_read        (union fd_data, int, void *, size_t,
                                            struct timespec *);
int                  __fd_kern_readv       (union fd_data, int,
                                            const struct iovec *, int,
                                            struct timespec *);
pthread_ssize_t      __fd_kern_write       (union fd_data, int, const void *,
                                            size_t, struct timespec *);
int                  __fd_kern_writev      (union fd_data, int,
                                            const struct iovec *, int,
                                            struct timespec *);
int                  __fd_kern_fcntl       (union fd_data, int, int, int);
int                  __fd_kern_close       (union fd_data, int);
off_t                __fd_kern_lseek       (union fd_data, int, off_t, int);
int                  open                  (const char *, int, ...);
#ifndef __ELF__
int                  create                (const char *, mode);
#endif
int                  creat                 (const char *, mode_t);
int                  fchown                (int, uid_t, gid_t);
int                  fchmod                (int, mode_t);
int                  ftruncate             (int, size_t);
int                  pipe                  (int fds[2]);
void                 fd_kern_reset         (int);
#ifdef __linux__
static inline int    isatty_basic          (int);
static inline char * ttyname_r_basic       (int, char *, int);
#endif
void                 fd_kern_init          (int);
static int           fd_kern_gettableentry (const int, int);
int                  fd_kern_exec          (const int);
void                 fd_kern_fork          (void);
#if defined (HAVE_SYSCALL_SOCKET) || defined (HAVE_SYSCALL_SOCKETCALL)
int                  socket                (int, int, int);
#endif
#if defined (HAVE_SYSCALL_BIND) || defined (HAVE_SYSCALL_SOCKETCALL)
int                  bind                  (int, const struct sockaddr *, int);
#endif
#if defined (HAVE_SYSCALL_CONNECT) || defined (HAVE_SYSCALL_SOCKETCALL)
int                  connect               (int, const struct sockaddr *, int);
#endif
#if defined (HAVE_SYSCALL_ACCEPT) || defined (HAVE_SYSCALL_SOCKETCALL)
int                  accept                (int, const struct sockaddr *,
                                            int *);
#endif
#if defined (HAVE_SYSCALL_LISTEN) || defined (HAVE_SYSCALL_SOCKETCALL)
int                  listen                (int, int); 
#endif
#if defined (HAVE_SYSCALL_SEND) || defined (HAVE_SYSCALL_SOCKETCALL)
ssize_t              send_timedwait        (int, void *, size_t, unsigned int,
                                            struct timespec *);
ssize_t              send                  (int, const void *, size_t, unsigned int);
#endif
#if defined (HAVE_SYSCALL_SENDTO) || defined (HAVE_SYSCALL_SOCKETCALL)
ssize_t              sendto_timedwait      (int, void *, size_t, unsigned int,
                                            struct sockaddr *, int,
                                            struct timespec *);
ssize_t              sendto                (int, const void *, size_t,
                                            unsigned int,
                                            const struct sockaddr *, int);
#endif
#if defined (HAVE_SYSCALL_SENDMSG) || defined (HAVE_SYSCALL_SOCKETCALL)
ssize_t              sendmsg_timedwait     (int, const struct msghdr *, int,
                                            struct timespec *);
ssize_t              sendmsg               (int, const struct msghdr *,
                                            unsigned int);
#endif
#if defined (HAVE_SYSCALL_RECV) || defined (HAVE_SYSCALL_SOCKETCALL)
ssize_t              recv_timedwait        (int, void *, size_t,
                                            unsigned int, struct timespec *);
ssize_t              recv(int, void *, size_t, unsigned int);
#endif
#if defined (HAVE_SYSCALL_RECVFROM) || defined (HAVE_SYSCALL_SOCKETCALL)
ssize_t              recvfrom_timedwait    (int, void *, size_t, int,
                                            struct sockaddr *, int *,
                                            struct timespec *);
ssize_t              recvfrom              (int, void *, size_t, unsigned int,
                                            struct sockaddr *, int *);
#endif
#if defined (HAVE_SYSCALL_RECVMSG) || defined (HAVE_SYSCALL_SOCKETCALL)
ssize_t              recvmsg_timedwait     (int, struct msghdr *, int,
                                            struct timespec *); 
#endif
#if defined (HAVE_SYSCALL_SHUTDOWN) || defined (HAVE_SYSCALL_SOCKETCALL)
int                  shutdown              (int, int);
#endif
#if defined (HAVE_SYSCALL_SETSOCKOPT) || defined (HAVE_SYSCALL_SOCKETCALL)
int                  setsockopt            (int, int, int, const void *, int);
#endif
#if defined (HAVE_SYSCALL_GETPEERNAME) || defined (HAVE_SYSCALL_SOCKETCALL)
int                  getpeername           (int, struct sockaddr *, int *);
#endif
#if defined (HAVE_SYSCALL_GETSOCKOPT) || defined (HAVE_SYSCALL_SOCKETCALL)
int                  getsockopt            (int, int, int, void *, int *);
#endif

/*
 * End of prototypes.
 */

/*
 * Begin Functions.
 */

#if defined (HAVE_SYSCALL_SENDTO) && !defined (HAVE_SYSCALL_SEND)

pthread_ssize_t machdep_sys_send (int fd, const void *msg, size_t len,
 int flags)
{
  return machdep_sys_sendto (fd, msg, len, flags,
			     (const struct sockaddr *) 0, 0);
}

#endif

#if defined (HAVE_SYSCALL_RECVFROM) && !defined (HAVE_SYSCALL_RECV)

pthread_ssize_t machdep_sys_recv (int fd, void *buf, size_t len, int flags)
{
  return machdep_sys_recvfrom (fd, buf, len, flags,
			       (struct sockaddr *) 0, (int *) 0);
}

#endif

/* ==========================================================================
 * Variables used by both fd_kern_poll and fd_kern_wait
 */
struct pthread_queue fd_wait_read = PTHREAD_QUEUE_INITIALIZER;
struct pthread_queue fd_wait_write = PTHREAD_QUEUE_INITIALIZER;
struct pthread_queue fd_wait_select = PTHREAD_QUEUE_INITIALIZER;

/* ==========================================================================
 * fd_kern_poll()
 *
 * Called only from context_switch(). The kernel must be locked.
 *
 * This function uses a linked list of waiting pthreads, NOT a queue.
 */ 

void fd_kern_poll(void)
{
	struct timeval __fd_kern_poll_timeout = { 0, 0 };
	fd_set fd_set_read, fd_set_write, fd_set_except;
	struct pthread *pthread, *deq;
	int count, i;

	if (fd_wait_read.q_next || fd_wait_write.q_next || fd_wait_select.q_next) {
		FD_ZERO(&fd_set_read);
		FD_ZERO(&fd_set_write);
		FD_ZERO(&fd_set_except);
		for(pthread = fd_wait_read.q_next; pthread; pthread = pthread->next) {
			FD_SET(pthread->data.fd, &fd_set_read);
		}
		for(pthread = fd_wait_write.q_next; pthread; pthread = pthread->next) {
			FD_SET(pthread->data.fd, &fd_set_write);
		}
		for (pthread = fd_wait_select.q_next; pthread; pthread = pthread->next){
			for (i = 0; i < pthread->data.select_data->nfds; i++) {
				if (FD_ISSET(i, &pthread->data.select_data->exceptfds)) {
					FD_SET(i, &fd_set_except);
				}
				if (FD_ISSET(i, &pthread->data.select_data->writefds)) {
					FD_SET(i, &fd_set_write);
				}
				if (FD_ISSET(i, &pthread->data.select_data->readfds)) {
					FD_SET(i, &fd_set_read);
				}
			}
		}
					

		if ((count = machdep_sys_select(dtablesize, &fd_set_read,
		  &fd_set_write, NULL, &__fd_kern_poll_timeout)) < OK) {
			if (count == -EINTR) {
				return;
			}
			PANIC();
		}
	
		for (pthread = fd_wait_read.q_next; count && pthread; ) {
			if (FD_ISSET(pthread->data.fd, &fd_set_read)) {
				count--;
				deq = pthread;
				pthread = pthread->next;
				pthread_queue_remove(&fd_wait_read, deq);
				if (SET_PF_DONE_EVENT(deq) == OK) {
					pthread_prio_queue_enq(pthread_current_prio_queue, deq);
					deq->state = PS_RUNNING;
				}
				continue;
			} 
			pthread = pthread->next;
		}
					
		for (pthread = fd_wait_write.q_next; count && pthread; ) {
			if (FD_ISSET(pthread->data.fd, &fd_set_write)) {
				count--;
				deq = pthread;
				pthread = pthread->next;
				pthread_queue_remove(&fd_wait_write, deq);
				if (SET_PF_DONE_EVENT(deq) == OK) {
					pthread_prio_queue_enq(pthread_current_prio_queue, deq);
					deq->state = PS_RUNNING;
				}
				continue;
			} 
			pthread = pthread->next;
		}

		for (pthread = fd_wait_select.q_next; count && pthread; ) {
			int found_one = 0;

			for (i = 0; i < pthread->data.select_data->nfds; i++) {
				int count_dec = 0; 

				if ((FD_ISSET(i, &pthread->data.select_data->exceptfds) && 
				  ! FD_ISSET(i, &fd_set_except))) {
					FD_CLR(i, &pthread->data.select_data->exceptfds);
				} else {
					count_dec++;
				}
				if ((FD_ISSET(i, &pthread->data.select_data->writefds) && 
				  ! FD_ISSET(i, &fd_set_write))) {
					FD_CLR(i, &pthread->data.select_data->writefds);
				} else {
					count_dec++;
				}
				if ((FD_ISSET(i, &pthread->data.select_data->readfds) && 
				  ! FD_ISSET(i, &fd_set_read))) {
					FD_CLR(i, &pthread->data.select_data->readfds);
				} else {
					count_dec++;
				}
				if (count_dec) {
					found_one++;
					count--;
				}
			}
			if (found_one) {
				deq = pthread;
				pthread = pthread->next;
				pthread_queue_remove(&fd_wait_select, deq);
				if (SET_PF_DONE_EVENT(deq) == OK) {
					pthread_prio_queue_enq(pthread_current_prio_queue, deq);
					deq->state = PS_RUNNING;
				}
			} else {
				pthread = pthread->next;
			}
		}
	}
}

/* ==========================================================================
 * fd_kern_wait()
 *
 * Called when there is no active thread to run.
 */
extern struct timeval __fd_kern_wait_timeout;

void fd_kern_wait(void)
{
	fd_set fd_set_read, fd_set_write, fd_set_except;
	struct pthread *pthread, *deq;
	sigset_t sig_to_block, oset;
	int count, i;

	if (fd_wait_read.q_next || fd_wait_write.q_next || fd_wait_select.q_next) {
		FD_ZERO(&fd_set_read);
		FD_ZERO(&fd_set_write);
		FD_ZERO(&fd_set_except);
		for (pthread = fd_wait_read.q_next; pthread; pthread = pthread->next) {
			FD_SET(pthread->data.fd, &fd_set_read);
		}
		for (pthread = fd_wait_write.q_next; pthread; pthread = pthread->next) {
			FD_SET(pthread->data.fd, &fd_set_write);
		}
		for (pthread = fd_wait_select.q_next; pthread; pthread = pthread->next){
			for (i = 0; i < pthread->data.select_data->nfds; i++) {
				if (FD_ISSET(i, &pthread->data.select_data->exceptfds)) {
					FD_SET(i, &fd_set_except);
				}
				if (FD_ISSET(i, &pthread->data.select_data->writefds)) {
					FD_SET(i, &fd_set_write);
				}
				if (FD_ISSET(i, &pthread->data.select_data->readfds)) {
					FD_SET(i, &fd_set_read);
				}
			}
		}

		/* Turn off interrupts for real while we set the timer.  */

		sigfillset(&sig_to_block);
		machdep_sys_sigprocmask(SIG_BLOCK, &sig_to_block, &oset);

		machdep_unset_thread_timer(NULL); 
		__fd_kern_wait_timeout.tv_usec = 0;
		__fd_kern_wait_timeout.tv_sec = 3600;

		machdep_sys_sigprocmask(SIG_UNBLOCK, &sig_to_block, &oset);

		/*
		 * There is a small but finite chance that an interrupt will
		 * occure between the unblock and the select. Because of this
		 * sig_handler_real() sets the value of __fd_kern_wait_timeout
		 * to zero causing the select to do a poll instead of a wait.
		 */

		while ((count = machdep_sys_select(dtablesize, &fd_set_read,
		  &fd_set_write, NULL, &__fd_kern_wait_timeout)) < OK) {
			if (count == -EINTR) {
				return;
			}
			PANIC();
		}
	
		for (pthread = fd_wait_read.q_next; count && pthread; ) {
			if (FD_ISSET(pthread->data.fd, &fd_set_read)) {
				count--;
				deq = pthread;
				pthread = pthread->next;
				pthread_queue_remove(&fd_wait_read, deq);
				if (SET_PF_DONE_EVENT(deq) == OK) {
					pthread_prio_queue_enq(pthread_current_prio_queue, deq);
					deq->state = PS_RUNNING;
				}
				continue;
			} 
			pthread = pthread->next;
		}
					
		for (pthread = fd_wait_write.q_next; count && pthread; ) {
			if (FD_ISSET(pthread->data.fd, &fd_set_write)) {
				count--;
				deq = pthread;
				pthread = pthread->next;
				pthread_queue_remove(&fd_wait_write, deq);
				if (SET_PF_DONE_EVENT(deq) == OK) {
					pthread_prio_queue_enq(pthread_current_prio_queue, deq);
					deq->state = PS_RUNNING;
				}
				continue;
			} 
			pthread = pthread->next;
		}

		for (pthread = fd_wait_select.q_next; count && pthread; ) {
			int found_one = 0;

			for (i = 0; i < pthread->data.select_data->nfds; i++) {
				int count_dec = 0; 

				if ((FD_ISSET(i, &pthread->data.select_data->exceptfds) && 
				  ! FD_ISSET(i, &fd_set_except))) {
					FD_CLR(i, &pthread->data.select_data->exceptfds);
				} else {
					count_dec++;
				}
				if ((FD_ISSET(i, &pthread->data.select_data->writefds) && 
				  ! FD_ISSET(i, &fd_set_write))) {
					FD_CLR(i, &pthread->data.select_data->writefds);
				} else {
					count_dec++;
				}
				if ((FD_ISSET(i, &pthread->data.select_data->readfds) && 
				  ! FD_ISSET(i, &fd_set_read))) {
					FD_CLR(i, &pthread->data.select_data->readfds);
				} else {
					count_dec++;
				}
				if (count_dec) {
					found_one++;
					count--;
				}
			}
			if (found_one) {
				deq = pthread;
				pthread = pthread->next;
				pthread_queue_remove(&fd_wait_select, deq);
				if (SET_PF_DONE_EVENT(deq) == OK) {
					pthread_prio_queue_enq(pthread_current_prio_queue, deq);
					deq->state = PS_RUNNING;
				}
			} else {
				pthread = pthread->next;
			}
		}
	} else {
		/* No threads, waiting on I/O, do a sigsuspend */
		sig_handler_pause();
	}
}

/* ==========================================================================
 * Special Note: All operations return the errno as a negative of the errno
 * listed in errno.h
 * ======================================================================= */

/* ==========================================================================
 * read()
 */
/*
 * GCC throws out the 'const' from 'const void *buf'. vch
 */
pthread_ssize_t __fd_kern_read(union fd_data fd_data, int flags,
                               void *buf, size_t nbytes,
                               struct timespec * timeout)
{
	int fd = fd_data.i;
	int ret;

	while ((ret = machdep_sys_read(fd, buf, nbytes)) < OK) { 
		if (!(flags & __FD_NONBLOCK) &&
		  ((ret == -EWOULDBLOCK) || (ret == -EAGAIN))) {
			pthread_sched_prevent();

			/* queue pthread for a FDR_WAIT */
			SET_PF_WAIT_EVENT(pthread_run);
			pthread_run->data.fd = fd;
			pthread_queue_enq(&fd_wait_read, pthread_run);

			if (timeout) {
				/* get current time */
				struct timespec current_time;
				machdep_gettimeofday(&current_time);
				sleep_schedule(&current_time, timeout);

				pthread_resched_resume(PS_FDR_WAIT);

				/* We're awake */
				pthread_sched_prevent();
				if (sleep_cancel(pthread_run) == NOTOK) {
					CLEAR_PF_DONE_EVENT(pthread_run);
					pthread_sched_resume();
					SET_ERRNO(ETIMEDOUT);
					ret = -ETIMEDOUT;
					break;
				}
				pthread_sched_resume();
			} else {
				pthread_resched_resume(PS_FDR_WAIT);
			}
			CLEAR_PF_DONE_EVENT(pthread_run);
		} else {
			SET_ERRNO(-ret);
			ret = NOTOK;
			break;
		}
	}
	return(ret);
}

/* ==========================================================================
 * readv()
 */
/*
 * GCC throws out the 'const' from 'const void *buf'. vch
 */
int __fd_kern_readv(union fd_data fd_data, int flags,
                    const struct iovec *iov, int iovcnt,
                    struct timespec * timeout)
{
	int fd = fd_data.i;
	int ret;

	while ((ret = machdep_sys_readv(fd, iov, iovcnt)) < OK) { 
		if (!(flags & __FD_NONBLOCK) &&
		  ((ret == -EWOULDBLOCK) || (ret == -EAGAIN))) {
			pthread_sched_prevent();

			/* queue pthread for a FDR_WAIT */
			pthread_run->data.fd = fd;
			SET_PF_WAIT_EVENT(pthread_run);
			pthread_queue_enq(&fd_wait_read, pthread_run);

			if (timeout) {
				/* get current time */
				struct timespec current_time;
				machdep_gettimeofday(&current_time);
				sleep_schedule(&current_time, timeout);

				pthread_resched_resume(PS_FDR_WAIT);

				/* We're awake */
				pthread_sched_prevent();
				if (sleep_cancel(pthread_run) == NOTOK) {
					CLEAR_PF_DONE_EVENT(pthread_run);
					pthread_sched_resume();
					SET_ERRNO(ETIMEDOUT);
					ret = -ETIMEDOUT;
					break;
				}
				pthread_sched_resume();
			} else {
				pthread_resched_resume(PS_FDR_WAIT);
			}
			CLEAR_PF_DONE_EVENT(pthread_run);
		} else {
			SET_ERRNO(-ret);
			ret = NOTOK;
			break;
		}
	}
	return(ret);
}

/* ==========================================================================
 * write()
 */
/*
 * GCC throws out the 'const' from 'const void *buf'. vch
 */
pthread_ssize_t __fd_kern_write(union fd_data fd_data, int flags,
                                const void *buf, size_t nbytes,
                                struct timespec * timeout)
{
	int fd = fd_data.i;
	int ret;

    while ((ret = machdep_sys_write(fd, buf, nbytes)) < OK) { 
		if (!(flags & __FD_NONBLOCK) &&
          ((ret == -EWOULDBLOCK) || (ret == -EAGAIN))) {
			pthread_sched_prevent();

			/* queue pthread for a FDW_WAIT */
			pthread_run->data.fd = fd;
			SET_PF_WAIT_EVENT(pthread_run);
			pthread_queue_enq(&fd_wait_write, pthread_run);

			if (timeout) {
				/* get current time */
				struct timespec current_time;
				machdep_gettimeofday(&current_time);
				sleep_schedule(&current_time, timeout);

				pthread_resched_resume(PS_FDW_WAIT);

				/* We're awake */
				pthread_sched_prevent();
				if (sleep_cancel(pthread_run) == NOTOK) {
					CLEAR_PF_DONE_EVENT(pthread_run);
					pthread_sched_resume();
					SET_ERRNO(ETIMEDOUT);
					ret = -ETIMEDOUT;
					break;
				}
				pthread_sched_resume();
			} else {
				pthread_resched_resume(PS_FDW_WAIT);
			}
			CLEAR_PF_DONE_EVENT(pthread_run);
        } else {
			SET_ERRNO(-ret);
			ret = NOTOK;
            break;
        }
    }
    return(ret);
}

/* ==========================================================================
 * writev()
 */
/*
 * GCC throws out the 'const' from 'const void *buf'. vch
 */
int __fd_kern_writev(union fd_data fd_data, int flags,
                     const struct iovec *iov, int iovcnt,
                     struct timespec * timeout)
{
	int fd = fd_data.i;
	int ret;

    while ((ret = machdep_sys_writev(fd, iov, iovcnt)) < OK) { 
		if (!(flags & __FD_NONBLOCK) &&
          ((ret == -EWOULDBLOCK) || (ret == -EAGAIN))) {
			pthread_sched_prevent();

			/* queue pthread for a FDW_WAIT */
			pthread_run->data.fd = fd;
			SET_PF_WAIT_EVENT(pthread_run);
			pthread_queue_enq(&fd_wait_write, pthread_run);

			if (timeout) {
				/* get current time */
				struct timespec current_time;
				machdep_gettimeofday(&current_time);
				sleep_schedule(&current_time, timeout);

				pthread_resched_resume(PS_FDW_WAIT);

				/* We're awake */
				pthread_sched_prevent();
				if (sleep_cancel(pthread_run) == NOTOK) {
					CLEAR_PF_DONE_EVENT(pthread_run);
					pthread_sched_resume();
					SET_ERRNO(ETIMEDOUT);
					ret = -ETIMEDOUT;
					break;
				}
				pthread_sched_resume();
			} else {
				pthread_resched_resume(PS_FDW_WAIT);
			}
			CLEAR_PF_DONE_EVENT(pthread_run);
        } else {
			SET_ERRNO(-ret);
			ret = NOTOK;
            break;
        }
    }
    return(ret);
}

/* ==========================================================================
 * For blocking version we really should set an interrupt
 * fcntl()
 */
int __fd_kern_fcntl(union fd_data fd_data, int flags, int cmd, int arg)
{
	int fd = fd_data.i;

	return(machdep_sys_fcntl(fd, cmd, arg));
}

/* ==========================================================================
 * close()
 */
int __fd_kern_close(union fd_data fd_data, int flags)
{
	int fd = fd_data.i;

	return(machdep_sys_close(fd));
}

/* ==========================================================================
 * lseek()
 */
off_t __fd_kern_lseek(union fd_data fd_data, int f, off_t offset, int whence)
{
	int fd = fd_data.i;

	return(machdep_sys_lseek(fd, offset, whence));
}

/*
 * File descriptor operations
 */

/* 
 * Why do some of these produce:
 * 'warning: initialization from incompatible pointer type'
 * and others do not??? vch
 */
/* Normal file operations */
static struct fd_ops __fd_kern_ops = {
	__fd_kern_write, 
   __fd_kern_read,
   __fd_kern_close,
   __fd_kern_fcntl,
	__fd_kern_writev,
   __fd_kern_readv,
   __fd_kern_lseek,
   1
};

/* NFS file opperations */

/* FIFO file opperations */

/* Device operations */

/* ==========================================================================
 * open()
 *
 * Because open could potentially block opening a file from a remote
 * system, we want to make sure the call will timeout. We then try and open
 * the file, and stat the file to determine what operations we should
 * associate with the fd.
 *
 * This is not done yet
 *
 * A regular file on the local system needs no special treatment.
 */
/*
 * GCC throws out the 'const' from 'const void *buf'. vch
 */

int open(const char *path, int flags, ...)
{
	int fd, mode, fd_kern;
	struct stat stat_buf;
	va_list ap;

	/* If pthread scheduling == FIFO set a virtual timer */
	if (flags & O_CREAT) {
		va_start(ap, flags);
		mode = va_arg(ap, int);
		va_end(ap);
	} else {
		mode = 0;
	}

	if (!((fd = fd_allocate()) < OK)) {
		fd_table[fd]->flags = flags;
		flags |= __FD_NONBLOCK;

		if (!((fd_kern = machdep_sys_open(path, flags, mode)) < OK)) {

			/* fstat the file to determine what type it is */
			if (machdep_sys_fstat(fd_kern, &stat_buf)) {
				PANIC();
			}
			if (S_ISREG(stat_buf.st_mode)) {
				fd_table[fd]->ops = &(__fd_kern_ops);
				fd_table[fd]->type = FD_HALF_DUPLEX;
			} else {
				fd_table[fd]->ops = &(__fd_kern_ops);
				fd_table[fd]->type = FD_FULL_DUPLEX;
			}
			fd_table[fd]->fd.i = fd_kern; 
			return(fd);
		}

		fd_table[fd]->count = 0;
		SET_ERRNO(-fd_kern);
	}
	return(NOTOK);
}

#ifdef __ELF__
#pragma weak create=creat
#else
/* ==========================================================================
 * create()
 */

int create(const char *path, mode_t mode)
{
  return creat (path, mode);
}
#endif

/* ==========================================================================
 * creat()
 */
#undef creat

/*
 * The 'const' in 'const char *path' gets discarded by GCC.  The SYSV
 * manual doesn't require it for 'ctreat' or 'open'. vch
 */
int creat(const char *path, mode_t mode)
{
  return open (path, O_CREAT | O_TRUNC | O_WRONLY, mode);
}

/* ==========================================================================
 * fchown()
 */
int fchown(int fd, uid_t owner, gid_t group)
{
	int ret;

	if ((ret = fd_lock(fd, FD_WRITE, NULL)) == OK) {
		if ((ret = machdep_sys_fchown(fd_table[fd]->fd.i, owner, group)) < OK) {
			SET_ERRNO(-ret);
			ret = NOTOK;
		}
		fd_unlock(fd, FD_WRITE);
	}
	return(ret);
}

/* ==========================================================================
 * fchmod()
 */
int fchmod(int fd, mode_t mode)
{
	int ret;

	if ((ret = fd_lock(fd, FD_WRITE, NULL)) == OK) {
		if ((ret = machdep_sys_fchmod(fd_table[fd]->fd.i, mode)) < OK) {
			SET_ERRNO(-ret);
			ret = NOTOK;
		}
		fd_unlock(fd, FD_WRITE);
	}
	return(ret);
}

/* ==========================================================================
 * ftruncate()
 */
int ftruncate(int fd, size_t length)
{
	int ret;

	if ((ret = fd_lock(fd, FD_WRITE, NULL)) == OK) {
		if ((ret = machdep_sys_ftruncate(fd_table[fd]->fd.i, length)) < OK) {
			SET_ERRNO(-ret);
			ret = NOTOK;
		}
		fd_unlock(fd, FD_WRITE);
	}
	return(ret);
}

/* ==========================================================================
 * pipe()
 */
int pipe(int fds[2])
{
	int kfds[2];
	int ret;

	if ((fds[0] = fd_allocate()) >= OK) {
		if ((fds[1] = fd_allocate()) >= OK) {
			if ((ret = machdep_sys_pipe(kfds)) >= OK) {
				fd_table[fds[0]]->flags = machdep_sys_fcntl(kfds[0],
                     F_GETFL, (int)NULL);
				machdep_sys_fcntl(kfds[0], F_SETFL, (int)fd_table[fds[0]]->flags
                     | __FD_NONBLOCK);
				fd_table[fds[1]]->flags = machdep_sys_fcntl(kfds[1], F_GETFL,
                     (int)NULL);
				machdep_sys_fcntl(kfds[1], F_SETFL, (int)fd_table[fds[1]]->flags
                     | __FD_NONBLOCK);

				fd_table[fds[0]]->ops = &(__fd_kern_ops);
				fd_table[fds[1]]->ops = &(__fd_kern_ops);

				/* Not really full duplex but ... */
				fd_table[fds[0]]->type = FD_FULL_DUPLEX;
				fd_table[fds[1]]->type = FD_FULL_DUPLEX;

				fd_table[fds[0]]->fd.i = kfds[0];
				fd_table[fds[1]]->fd.i = kfds[1];

				return(OK);
			} else {
				SET_ERRNO(-ret);
			}
			fd_table[fds[1]]->count = 0;
		}
		fd_table[fds[0]]->count = 0;
	}
	return(NOTOK);
}

/* ==========================================================================
 * fd_kern_reset()
 * Change the fcntl blocking flag back to NONBLOCKING. This should only
 * be called after a fork.
 */
void fd_kern_reset(int fd)
{
	switch (fd_table[fd]->type) {
	case FD_TEST_HALF_DUPLEX:
		machdep_sys_fcntl(fd_table[fd]->fd.i, F_SETFL,
          fd_table[fd]->flags | __FD_NONBLOCK);
		fd_table[fd]->type = FD_HALF_DUPLEX;
		break;
	case FD_TEST_FULL_DUPLEX:
		machdep_sys_fcntl(fd_table[fd]->fd.i, F_SETFL,
		  fd_table[fd]->flags | __FD_NONBLOCK);
		fd_table[fd]->type = FD_FULL_DUPLEX;
		break;
	default:
		break;
	}
}

#ifdef __linux__
static inline int
isatty_basic (int fd)
{
  struct termios term;
  return __machdep_sys_ioctl(fd, TCGETS, &term) == 0;
}

/* Return the pathname of the terminal FD is open on, or NULL on errors.
   The returned storage is good only until the next call to this function.  */
static inline char *
ttyname_r_basic (int fd, char *name, int namelen)
{
  static const char dev[] = "/dev";
  struct stat st;
  dev_t mydev;
  ino_t myino;
  DIR *dirstream;
  struct dirent *d;
  int d_namlen;

  if (__machdep_sys_fstat (fd, &st) < 0)
    return NULL;
  mydev = st.st_dev;
  myino = st.st_ino;

  dirstream = __libc_opendir (dev);
  if (dirstream == NULL)
    return NULL;

  (void) memcpy (name, dev, sizeof (dev) - 1);
  name[sizeof (dev) - 1] = '/';
  
  while ((d = __libc_readdir (dirstream)) != NULL)
    if (d->d_ino == myino)
      {
	d_namlen = strlen (d->d_name) + 1;
#if 0
	/* It should never happend. */
	if (sizeof (dev) + d_namlen > namelen)
	   rerturn NULL;
#endif

	(void) memcpy (&name[sizeof (dev)], d->d_name, d_namlen);
	if (stat (name, &st) == 0 && st.st_dev == mydev)
	  {
	    (void) __libc_closedir (dirstream);
	    return name;
	  }
      }

  (void) __libc_closedir (dirstream);
  return NULL;
}
#endif

/* ==========================================================================
 * fd_kern_init()
 *
 * Assume the entry is locked before routine is invoked
 *
 * This may change. The problem is setting the fd to nonblocking changes
 * the parents fd too, which may not be the desired result.
 *
 * New added feature: If the fd in question is a tty then we open it again
 * and close the original, this way we don't have to worry about the
 * fd being NONBLOCKING to the outside world.
 */
void fd_kern_init(int fd)
{
	if ((fd_table[fd]->flags = machdep_sys_fcntl(fd, F_GETFL,
                                               (int)NULL)) >= OK) {
		if (isatty_basic(fd)) {
			int new_fd;
#ifdef __linux__
			/* That should be big enough. */
			char name [_POSIX_PATH_MAX];
			ttyname_r_basic (fd, name, sizeof (name));
#else
			char *name = ttyname_basic (fd);
#endif

			if ((new_fd = machdep_sys_open(name, O_RDWR)) >= OK) {
				if (machdep_sys_dup2(new_fd, fd) == OK) {
					/* Should print a warning */

					/* Should also set the flags to that of opened outside of
					process */
				}
				machdep_sys_close(new_fd);
			}
		}
		/* We do these things regaurdless of the above results */
		machdep_sys_fcntl(fd, F_SETFL, fd_table[fd]->flags | __FD_NONBLOCK);
		fd_table[fd]->ops 	= &(__fd_kern_ops);
		fd_table[fd]->type 	= FD_HALF_DUPLEX;
		fd_table[fd]->fd.i 	= fd;
		fd_table[fd]->count = 1;

	}
}

/* ==========================================================================
 * fd_kern_gettableentry()
 *
 * Remember only return a a file descriptor that I will modify later.
 * Don't return file descriptors that aren't owned by the child, or don't
 * have kernel operations.
 */
static int fd_kern_gettableentry(const int child, int fd)
{
	int i;

	for (i = 0; i < dtablesize; i++) {
		if (fd_table[i]) {
			if (fd_table[i]->fd.i == fd) {
				if (child) {
					if ((fd_table[i]->type != FD_TEST_HALF_DUPLEX) &&
		   		 	  (fd_table[i]->type != FD_TEST_FULL_DUPLEX)) {
						continue;
					}
				} else {
					if ((fd_table[i]->type == FD_NT) ||
           		 	  (fd_table[i]->type == FD_NIU)) {
						continue;
					}
				}
				/* Is it a kernel fd ? */
				if ((!fd_table[i]->ops) || 
				  (!fd_table[i]->ops->use_kfds)) {
					continue;
				}
				return(i);
			}
		}
	}
	return(NOTOK);
}

/* ==========================================================================
 * fd_kern_exec()
 *
 * Fixup the fd_table such that (fd == fd_table[fd]->fd.i) this way
 * the new immage will be OK.
 *
 * Only touch those that won't be used by the parent if we're in a child
 * otherwise fixup all.
 *
 * Returns:
 * 0 no fixup necessary
 * 1 fixup without problems
 * 2 failed fixup on some descriptors, and clobbered them.
 */
int fd_kern_exec(const int child)
{
	int ret = 0;
	int fd, i;

	for (fd = 0; fd < dtablesize; fd++) {
		if (fd_table[fd] == NULL) {
			continue;
		}
		/* Is the fd already in use ? */
		if (child) {
			if ((fd_table[fd]->type != FD_TEST_HALF_DUPLEX) &&
		      (fd_table[fd]->type != FD_TEST_FULL_DUPLEX)) {
				continue;
			}
		} else {
			if ((fd_table[fd]->type == FD_NT) ||
              (fd_table[fd]->type == FD_NIU)) {
				continue;
			}
		}
		/* Is it a kernel fd ? */
		if ((!fd_table[fd]->ops) || 
		  (!fd_table[fd]->ops->use_kfds)) {
			continue;
		}
		/* Does it match ? */
		if (fd_table[fd]->fd.i == fd) {
			continue;
		}
		/* OK, fixup entry: Read comments before changing. This isn't obvious */ 

		/* i is the real file descriptor fd currently represents */
		if (((i = fd_table[fd]->fd.i) >= dtablesize) || (i < 0)) {
			/* This should never happen */
			PANIC();
		}

		/*
		 * if the real file descriptor with the same number as the fake file
		 * descriptor number fd is actually in use by the program, we have
         * to move it out of the way
		 */
		if ((machdep_sys_fcntl(fd, F_GETFL, (int)NULL)) >= OK) {
			/* fd is busy */
			int j;

			/*
			 * j is the fake file descriptor that represents the real file
			 * descriptor that we want to move. This way the fake file
			 * descriptor fd can move its real file descriptor i such that
			 * fd == i.
			 */
			if ((j = fd_kern_gettableentry(child, fd)) >= OK) {

				/*
				 * Since j represents a fake file descriptor and fd represents
				 * a fake file descriptor. If j < fd then a previous pass
				 * should have set fd_table[j]->fd.i == j.
				 */
				if (fd < j) {
					if ((fd_table[j]->fd.i = machdep_sys_dup(fd)) < OK) {
						/* Close j, there is nothing else we can do */
  						fd_table[j]->type = FD_NIU;
						ret = 2;
					}
				} else {
					/* This implies fd_table[j]->fd.i != j */
					PANIC();
				}
			}
		}

		/*
		 * Here the real file descriptor i is set to equel the fake file
		 * descriptor fd
		 */
		machdep_sys_dup2(i, fd);

		/*
		 * Now comes the really complicated part: UNDERSTAND before changing
		 *
		 * Here are the things this routine wants to do ...
		 *
		 * Case 1. The real file descriptor has only one fake file descriptor
		 * representing it. 
		 * fd -> i, fd != i ===>  fd -> fd, close(i)
		 * Example fd = 4, i = 2: then close(2), set fd -> i = 4
		 * 
		 * Case 2. The real file descriptor has more than one fake file
		 * descriptor representing it, and this is the first fake file
		 * descriptor representing the real file descriptor
		 * fd -> i, fd' -> i, fd != i ===> fd -> fd, fd' -> fd, close(i)
		 *
		 * The problem is achiving the above is very messy and difficult,
		 * but I should be able to take a short cut. If fd > i then there
		 * will be no need to ever move i, this is because the fake file
		 * descriptor foo that we would have wanted to represent the real
		 * file descriptor i has already been processed. If fd < i then by
		 * moving i to fd all subsequent fake file descriptors fd' should fall
		 * into the previous case and won't need aditional adjusting.
		 *
		 * Does this break the above fd < j check .... It shouldn't because j
		 * is a fake file descriptor and if j < fd then j has already moved 
		 * its real file descriptor foo such that foo <= j therefore foo < fd
		 * and not foo == fd therefor j cannot represent the real 
		 * filedescriptor that fd want to move to and be less than fd
		 */
		if (fd < i) {
			fd_table[fd]->fd.i = fd;
			machdep_sys_close(i);
		}
		if (ret < 1) {
			 ret = 1;
		}
	}
	return ret;
}

/* ==========================================================================
 * fd_kern_fork()
 */
void fd_kern_fork()
{
	pthread_mutex_t *mutex;
	int fd;

	for (fd = 0; fd < dtablesize; fd++) {
		if (fd_table[fd] == NULL) {
			continue;
		}
		mutex = & (fd_table[fd]->mutex);
		if (pthread_mutex_trylock(mutex)) {
			continue;
		}
		if ((fd_table[fd]->r_owner) || (fd_table[fd]->w_owner)) {
			pthread_mutex_unlock(mutex);
			continue;
		}
		/* Is it a kernel fd ? */
		if ((!fd_table[fd]->ops) || (!fd_table[fd]->ops->use_kfds)) {
			pthread_mutex_unlock(mutex);
			continue;
		}
		switch (fd_table[fd]->type) {
		case FD_HALF_DUPLEX:
			machdep_sys_fcntl(fd_table[fd]->fd.i, F_SETFL, fd_table[fd]->flags);
			fd_table[fd]->type = FD_TEST_HALF_DUPLEX;
			break;
		case FD_FULL_DUPLEX:
			machdep_sys_fcntl(fd_table[fd]->fd.i, F_SETFL, fd_table[fd]->flags);
			fd_table[fd]->type = FD_TEST_FULL_DUPLEX;
			break;
		default:
			break;
		}
		pthread_mutex_unlock(mutex);
	}
}

/* ==========================================================================
 * Here are the berkeley socket functions. These are not POSIX.
 * ======================================================================= */

#if defined (HAVE_SYSCALL_SOCKET) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * socket()
 */
int socket(int af, int type, int protocol)
{
	int fd, fd_kern;

	 if (!((fd = fd_allocate()) < OK)) {

        if (!((fd_kern = machdep_sys_socket(af, type, protocol)) < OK)) {
		    int tmp_flags;

			tmp_flags = machdep_sys_fcntl(fd_kern, F_GETFL, 0);
			machdep_sys_fcntl(fd_kern, F_SETFL, tmp_flags | __FD_NONBLOCK);

            /* Should fstat the file to determine what type it is */
            fd_table[fd]->ops 	= & __fd_kern_ops;
            fd_table[fd]->type 	= FD_FULL_DUPLEX;
			fd_table[fd]->fd.i	= fd_kern;
        	fd_table[fd]->flags = tmp_flags;
            return(fd);
        }

        fd_table[fd]->count = 0;
		SET_ERRNO(-fd_kern);
    }
    return(NOTOK);
}

#endif

#if defined (HAVE_SYSCALL_BIND) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * bind()
 */
int bind(int fd, const struct sockaddr *name, int namelen)
{
	/* Not much to do in bind */
	int ret;

	if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
        if ((ret = machdep_sys_bind(fd_table[fd]->fd.i, name, namelen)) < OK) { 
			SET_ERRNO(-ret);
		}
		fd_unlock(fd, FD_RDWR);
	}
	return(ret);
}

#endif

#if defined (HAVE_SYSCALL_CONNECT) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * connect()
 */
int connect(int fd, const struct sockaddr *name, int namelen)
{
	struct sockaddr tmpname;
	int ret, tmpnamelen;

	if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
		if ((ret = machdep_sys_connect(fd_table[fd]->fd.i, name, namelen))
         < OK) {
            if (!(fd_table[fd]->flags & __FD_NONBLOCK) &&
              ((ret == -EWOULDBLOCK) || (ret == -EINPROGRESS) ||
		      (ret == -EALREADY) || (ret == -EAGAIN))) {
				pthread_sched_prevent();

				/* queue pthread for a FDW_WAIT */
				SET_PF_WAIT_EVENT(pthread_run);
				pthread_run->data.fd = fd_table[fd]->fd.i;
				pthread_queue_enq(&fd_wait_write, pthread_run);

				pthread_resched_resume(PS_FDW_WAIT);
				CLEAR_PF_DONE_EVENT(pthread_run);

				tmpnamelen = sizeof(tmpname);
				/* OK now lets see if it really worked */
				if (((ret = machdep_sys_getpeername(fd_table[fd]->fd.i,
				  &tmpname, &tmpnamelen)) < OK) && (ret == -ENOTCONN)) {

					/* Get the error, this function should not fail */
					machdep_sys_getsockopt(fd_table[fd]->fd.i, SOL_SOCKET,
					  SO_ERROR, &pthread_run->error, &tmpnamelen); 
				}
            } else {
				SET_ERRNO(-ret);
				ret = NOTOK;
			}
		}
		fd_unlock(fd, FD_RDWR);
	}
	return(ret);
}

#endif

#if defined (HAVE_SYSCALL_ACCEPT) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * accept()
 */
/* 
 * GCC discards the 'const' from 'const struct sockaddr' so it should be
 * deleted from the definition.  However, I didn't want to change
 * <socket.h>. THe 'const' is not required by the SYSV manual. vch
 * 
 */
int accept(int fd, const struct sockaddr *name, int *namelen)
{
	int ret, fd_kern;

	if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
		while ((fd_kern = machdep_sys_accept(fd_table[fd]->fd.i, name, namelen))
         < OK) {
            if (!(fd_table[fd]->flags & __FD_NONBLOCK) &&
              ((fd_kern == -EWOULDBLOCK) || (fd_kern == -EAGAIN))) {
				pthread_sched_prevent();

				/* queue pthread for a FDR_WAIT */
				SET_PF_WAIT_EVENT(pthread_run);
				pthread_run->data.fd = fd_table[fd]->fd.i;
				pthread_queue_enq(&fd_wait_read, pthread_run);
				
				pthread_resched_resume(PS_FDR_WAIT);
				CLEAR_PF_DONE_EVENT(pthread_run);
            } else {
				fd_unlock(fd, FD_RDWR);
				SET_ERRNO(-fd_kern);
				return(fd_kern);
			}
		}
		fd_unlock(fd, FD_RDWR);

	 	if (!((ret = fd_allocate()) < OK)) {

			/* This may be unnecessary */
			machdep_sys_fcntl(fd_kern, F_SETFL, __FD_NONBLOCK);

            /* Should fstat the file to determine what type it is */
            fd_table[ret]->ops 		= & __fd_kern_ops;
            fd_table[ret]->type 	= FD_FULL_DUPLEX;
			fd_table[ret]->fd.i		= fd_kern;
        	fd_table[ret]->flags 	= 0;
		}
	}
	return(ret);
}

#endif

#if defined (HAVE_SYSCALL_LISTEN) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * listen()
 */
int listen(int fd, int backlog) 
{
  int ret;

  if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
    if ((ret = machdep_sys_listen(fd_table[fd]->fd.i, backlog)) < OK) {
      SET_ERRNO(-ret);
      ret = NOTOK;
    }
    fd_unlock(fd, FD_RDWR);
  }
  return(ret);
}

#endif

#if defined (HAVE_SYSCALL_SEND) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * send_timedwait()
 */
/*  Changed from:
 * ssize_t send_timedwait(int fd, const void * msg, size_t len,
 *      unsigned int flags, struct timespec * timeout)
 *  Because gcc discards 'const' anyhow. vch
 */
ssize_t send_timedwait(int fd, void * msg, size_t len,
  unsigned int flags, struct timespec * timeout)
{
	int ret;

	if ((ret = fd_lock(fd, FD_WRITE, timeout)) == OK) {
		while ((ret = machdep_sys_send(fd_table[fd]->fd.i,
          msg,  len, flags)) < OK) {
            if (!(fd_table[fd]->flags & __FD_NONBLOCK) &&
              ((ret == -EWOULDBLOCK) || (ret == -EAGAIN))) {
				pthread_sched_prevent();

				/* queue pthread for a FDW_WAIT */
				SET_PF_WAIT_EVENT(pthread_run);
				pthread_run->data.fd = fd_table[fd]->fd.i;
				pthread_queue_enq(&fd_wait_write, pthread_run);

				if (timeout) {
					/* get current time */
					struct timespec current_time;
					machdep_gettimeofday(&current_time);
					sleep_schedule(&current_time, timeout);

					pthread_resched_resume(PS_FDW_WAIT);

					/* We're awake */
					pthread_sched_prevent();
					if (sleep_cancel(pthread_run) == NOTOK) {
						CLEAR_PF_DONE_EVENT(pthread_run);
						pthread_sched_resume();
						SET_ERRNO(ETIMEDOUT);
						ret = -ETIMEDOUT;
						break;
					}
					pthread_sched_resume();
				} else {
					pthread_resched_resume(PS_FDW_WAIT);
				}
				CLEAR_PF_DONE_EVENT(pthread_run);
            } else {
				SET_ERRNO(-ret);
				ret = NOTOK;
				break;
			}
		}
		fd_unlock(fd, FD_WRITE);
	}
	return(ret);
}

/* ==========================================================================
 * send()
 */
/*  
 *  GCC discards the 'const' from 'const void * msg'.  It isn't required
 *  according the the SYSV manual.  I left it in because I didn't want
 *  change <socket.h>. vch
 */
ssize_t send(int fd, const void * msg, size_t len, unsigned int flags)
{
	return(send_timedwait(fd, msg, len, flags, NULL));
}

#endif

#if defined (HAVE_SYSCALL_SENDTO) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * sendto_timedwait()
 */
/*  Changed from:
 * ssize_t sendto_timedwait(int fd, const void * msg, size_t len,
 *        unsigned int flags, const struct sockaddr *to, int to_len,
 *        struct timespec * timeout)
 *  Because gcc discards 'const' anyhow. vch
 */
ssize_t sendto_timedwait(int fd, void * msg, size_t len,
 unsigned int flags, struct sockaddr *to, int to_len,
 struct timespec * timeout)
{
	int ret;

	if ((ret = fd_lock(fd, FD_WRITE, timeout)) == OK) {
		while ((ret = machdep_sys_sendto(fd_table[fd]->fd.i,
          msg, len, flags, to, to_len)) < OK) {
            if (!(fd_table[fd]->flags & __FD_NONBLOCK) &&
              ((ret == -EWOULDBLOCK) || (ret == -EAGAIN))) {
				pthread_sched_prevent();

				/* queue pthread for a FDW_WAIT */
				SET_PF_WAIT_EVENT(pthread_run);
				pthread_run->data.fd = fd_table[fd]->fd.i;
				pthread_queue_enq(&fd_wait_write, pthread_run);

				if (timeout) {
					/* get current time */
					struct timespec current_time;
					machdep_gettimeofday(&current_time);
					sleep_schedule(&current_time, timeout);

					pthread_resched_resume(PS_FDW_WAIT);

					/* We're awake */
					pthread_sched_prevent();
					if (sleep_cancel(pthread_run) == NOTOK) {
						CLEAR_PF_DONE_EVENT(pthread_run);
						pthread_sched_resume();
						SET_ERRNO(ETIMEDOUT);
						ret = -ETIMEDOUT;
						break;
					}
					pthread_sched_resume();
				} else {
					pthread_resched_resume(PS_FDW_WAIT);
				}
				CLEAR_PF_DONE_EVENT(pthread_run);
            } else {
				SET_ERRNO(-ret);
				ret = NOTOK;
				break;
			}
		}
		fd_unlock(fd, FD_WRITE);
	}
	return(ret);
}

/* ==========================================================================
 * sendto()
 */
/*
 * GCC throws out the 'const' from 'const void *buf'. vch
 */
ssize_t sendto(int fd, const void * msg, size_t len, unsigned int flags,
               const struct sockaddr *to, int to_len)
{
	return(sendto_timedwait(fd, msg, len, flags, to, to_len, NULL));
}

#endif

#if defined (HAVE_SYSCALL_SENDMSG) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * sendmsg_timedwait()
 */
/*
 * GCC throws out the 'const' from 'const struct msghdr *msg'. vch
 */
ssize_t sendmsg_timedwait(int fd, const struct msghdr *msg, int flags,
  struct timespec * timeout)
{
	int passed_fd, ret, i;

	/* Handle getting the real file descriptor */
	for(i = 0; i < (((struct omsghdr *)msg)->msg_controllen/sizeof(i)); i++) {
		passed_fd = *(((int *)((struct omsghdr *)msg)->msg_control) + i);
		if ((ret = fd_lock(passed_fd, FD_RDWR, NULL)) == OK) {
			*(((int *)((struct omsghdr *)msg)->msg_control) + i)
			  = fd_table[passed_fd]->fd.i;
			machdep_sys_fcntl(fd_table[passed_fd]->fd.i, F_SETFL, 
			  fd_table[passed_fd]->flags);
			switch(fd_table[passed_fd]->type) {
			case FD_TEST_FULL_DUPLEX:
			case FD_TEST_HALF_DUPLEX:
				break;
			case FD_FULL_DUPLEX:
				fd_table[passed_fd]->type =  FD_TEST_FULL_DUPLEX;
				break;
			case FD_HALF_DUPLEX:
				fd_table[passed_fd]->type =  FD_TEST_HALF_DUPLEX;
				break;
			default:
				PANIC();
			}
		} else {
			fd_unlock(fd, FD_RDWR);
			SET_ERRNO(EBADF);
			return(NOTOK);
		}
		fd_unlock(fd, FD_RDWR);
	}

	if ((ret = fd_lock(fd, FD_WRITE, timeout)) == OK) {
		while((ret = machdep_sys_sendmsg(fd_table[fd]->fd.i, msg, flags)) < OK){
            if (!(fd_table[fd]->flags & __FD_NONBLOCK) &&
			  ((ret == -EWOULDBLOCK) || (ret == -EAGAIN))) {
				pthread_sched_prevent();

				/* queue pthread for a FDW_WAIT */
				SET_PF_WAIT_EVENT(pthread_run);
				pthread_run->data.fd = fd_table[fd]->fd.i;
				pthread_queue_enq(&fd_wait_write, pthread_run);
				
				if (timeout) {
					/* get current time */
					struct timespec current_time;
					machdep_gettimeofday(&current_time);
					sleep_schedule(&current_time, timeout);

					pthread_resched_resume(PS_FDW_WAIT);

					/* We're awake */
					pthread_sched_prevent();
					if (sleep_cancel(pthread_run) == NOTOK) {
						CLEAR_PF_DONE_EVENT(pthread_run);
						pthread_sched_resume();
						SET_ERRNO(ETIMEDOUT);
						ret = -ETIMEDOUT;
						break;
					}
					pthread_sched_resume();

				} else {
					pthread_resched_resume(PS_FDW_WAIT);
				}
				CLEAR_PF_DONE_EVENT(pthread_run);
            } else {
				SET_ERRNO(-ret);
				ret = NOTOK;
				break;
			}
		}
		fd_unlock(fd, FD_READ);
	}
	return(ret);
}

/* ==========================================================================
 * sendmsg()
 */
ssize_t sendmsg(int fd, const struct msghdr *msg, unsigned int flags)
{
	return(sendmsg_timedwait(fd, msg, flags, NULL));
}

#endif

#if defined (HAVE_SYSCALL_RECV) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * recv_timedwait()
 */
ssize_t recv_timedwait(int fd, void * buf, size_t len,
	unsigned int flags, struct timespec * timeout)
{
	int ret;

	if ((ret = fd_lock(fd, FD_READ, timeout)) == OK) {
		while ((ret = machdep_sys_recv(fd_table[fd]->fd.i,
		  buf, len, flags)) < OK) {
            if (!(fd_table[fd]->flags & __FD_NONBLOCK) &&
              ((ret == -EWOULDBLOCK) || (ret == -EAGAIN))) {
				pthread_sched_prevent();

				/* queue pthread for a FDR_WAIT */
				SET_PF_WAIT_EVENT(pthread_run);
				pthread_run->data.fd = fd_table[fd]->fd.i;
				pthread_queue_enq(&fd_wait_read, pthread_run);

				if (timeout) {
					/* get current time */
					struct timespec current_time;
					machdep_gettimeofday(&current_time);
					sleep_schedule(&current_time, timeout);

					pthread_resched_resume(PS_FDR_WAIT);

					/* We're awake */
					pthread_sched_prevent();
					if (sleep_cancel(pthread_run) == NOTOK) {
						CLEAR_PF_DONE_EVENT(pthread_run);
						pthread_sched_resume();
						SET_ERRNO(ETIMEDOUT);
						ret = -ETIMEDOUT;
						break;
					}
					pthread_sched_resume();
				} else {
					pthread_resched_resume(PS_FDR_WAIT);
				}
				CLEAR_PF_DONE_EVENT(pthread_run);
            } else {
				SET_ERRNO(-ret);
				ret = NOTOK;
				break;
			}
		}
		fd_unlock(fd, FD_READ);
	}
	return(ret);
}

/* ==========================================================================
 * recv()
 */
ssize_t recv(int fd, void * buf, size_t len, unsigned int flags)
{
	return(recv_timedwait(fd, buf, len, flags, NULL));
}

#endif

#if defined (HAVE_SYSCALL_RECVFROM) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * recvfrom_timedwait()
 */
ssize_t recvfrom_timedwait(int fd, void * buf, size_t len, int flags,
  struct sockaddr * from, int * from_len, struct timespec * timeout)
{
	int ret;

	if ((ret = fd_lock(fd, FD_READ, timeout)) == OK) {
		while ((ret = machdep_sys_recvfrom(fd_table[fd]->fd.i,
		  buf, len, flags, from, from_len)) < OK) {
            if (!(fd_table[fd]->flags & __FD_NONBLOCK) &&
			  ((ret == -EWOULDBLOCK) || (ret == -EAGAIN))) {
				pthread_sched_prevent();

				/* queue pthread for a FDR_WAIT */
				SET_PF_WAIT_EVENT(pthread_run);
				pthread_run->data.fd = fd_table[fd]->fd.i;
				pthread_queue_enq(&fd_wait_read, pthread_run);

				if (timeout) {
					/* get current time */
					struct timespec current_time;
					machdep_gettimeofday(&current_time);
					sleep_schedule(&current_time, timeout);

					pthread_resched_resume(PS_FDR_WAIT);

					/* We're awake */
					pthread_sched_prevent();
					if (sleep_cancel(pthread_run) == NOTOK) {
						CLEAR_PF_DONE_EVENT(pthread_run);
						pthread_sched_resume();
						SET_ERRNO(ETIMEDOUT);
						ret = -ETIMEDOUT;
						break;
					}
					pthread_sched_resume();

				} else {
					pthread_resched_resume(PS_FDR_WAIT);
				}
				CLEAR_PF_DONE_EVENT(pthread_run);
            } else {
				SET_ERRNO(-ret);
				ret = NOTOK;
				break;
			}
		}
		fd_unlock(fd, FD_READ);
	}
	return(ret);
}

/* ==========================================================================
 * recvfrom()
 */
ssize_t recvfrom(int fd, void * buf, size_t len, unsigned int flags,
  struct sockaddr * from, int * from_len)
{
	return(recvfrom_timedwait(fd, buf, len, flags, from, from_len, NULL));
}

#endif

#if defined (HAVE_SYSCALL_RECVMSG) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * recvmsg_timedwait()
 */
ssize_t recvmsg_timedwait(int fd, struct msghdr *msg, int flags,
  struct timespec * timeout) 
{
	struct stat stat_buf;
	int passed_fd, ret, i;

	if ((ret = fd_lock(fd, FD_READ, timeout)) == OK) {
		while ((ret = machdep_sys_recvmsg(fd_table[fd]->fd.i, msg, flags))
         < OK) {
            if (!(fd_table[fd]->flags & __FD_NONBLOCK) &&
			  ((ret == -EWOULDBLOCK) || (ret == -EAGAIN))) {
				pthread_sched_prevent();

				/* queue pthread for a FDR_WAIT */
				SET_PF_WAIT_EVENT(pthread_run);
				pthread_run->data.fd = fd_table[fd]->fd.i;
				pthread_queue_enq(&fd_wait_read, pthread_run);
		
				if (timeout) {
					/* get current time */
					struct timespec current_time;
					machdep_gettimeofday(&current_time);
					sleep_schedule(&current_time, timeout);

					pthread_resched_resume(PS_FDR_WAIT);

					/* We're awake */
					pthread_sched_prevent();
					if (sleep_cancel(pthread_run) == NOTOK) {
						CLEAR_PF_DONE_EVENT(pthread_run);
						pthread_sched_resume();
						SET_ERRNO(ETIMEDOUT);
						ret = -ETIMEDOUT;
						break;
					}
					pthread_sched_resume();

				} else {
					pthread_resched_resume(PS_FDR_WAIT);
				}
				CLEAR_PF_DONE_EVENT(pthread_run);
            } else {
				SET_ERRNO(-ret);
				ret = NOTOK;
				break;
			}
		}
		fd_unlock(fd, FD_READ);

		/* Handle getting the real file descriptor */
		for (i = 0; i < (((struct omsghdr *)msg)->msg_controllen / sizeof(i));
           i++) {
			passed_fd = *(((int *)((struct omsghdr *)msg)->msg_control) + i);
			if (!((fd = fd_allocate()) < OK)) {
				fd_table[fd]->flags = machdep_sys_fcntl(passed_fd, F_GETFL);

				if (!( fd_table[fd]->flags & __FD_NONBLOCK)) {
					machdep_sys_fcntl(passed_fd, F_SETFL,  
					  fd_table[fd]->flags | __FD_NONBLOCK);
				}

				/* fstat the file to determine what type it is */
				machdep_sys_fstat(passed_fd, &stat_buf);
           		if (S_ISREG(stat_buf.st_mode)) {
           	    	fd_table[fd]->type = FD_HALF_DUPLEX;
           		} else {
           	    	fd_table[fd]->type = FD_FULL_DUPLEX;
           		}
				*(((int *)((struct omsghdr *)msg)->msg_control) + i) = fd;
           		fd_table[fd]->ops = &(__fd_kern_ops);
           		fd_table[fd]->fd.i = passed_fd;
			} else {
				SET_ERRNO(EBADF);
				return(NOTOK);
				break;
			}
		}
	}
	return(ret);
}

/* ==========================================================================
 * recvmsg()
 */
ssize_t recvmsg(int fd, struct msghdr *msg, unsigned int flags) 
{
	return(recvmsg_timedwait(fd, msg, flags, NULL));
}

#endif

#if defined (HAVE_SYSCALL_SHUTDOWN) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * shutdown()
 */
int shutdown(int fd, int how)
{
	int ret;

	switch(how) {
	case 0: /* Read */
		if ((ret = fd_lock(fd, FD_READ, NULL)) == OK) {
			if ((ret = machdep_sys_shutdown(fd_table[fd]->fd.i, how)) < OK) {
				SET_ERRNO(-ret);
				ret = NOTOK;
			}
			fd_unlock(fd, FD_READ);
		}
	case 1: /* Write */
		if ((ret = fd_lock(fd, FD_WRITE, NULL)) == OK) {
			if ((ret = machdep_sys_shutdown(fd_table[fd]->fd.i, how)) < OK) {
				SET_ERRNO(-ret);
				ret = NOTOK;
			}
			fd_unlock(fd, FD_WRITE);
		}
	case 2: /* Read-Write */
		if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
			if ((ret = machdep_sys_shutdown(fd_table[fd]->fd.i, how)) < OK) {
				SET_ERRNO(-ret);
				ret = NOTOK;
			}
			fd_unlock(fd, FD_RDWR);
		}
	default:
		SET_ERRNO(EBADF);
		ret = NOTOK;
		break;
	}
	return(ret);
}

#endif

#if defined (HAVE_SYSCALL_SETSOCKOPT) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * setsockopt()
 */
/* 
 * GCC discards the 'const' from 'const void * optval'.  The SYSV manual
 * doesn't designate this argument as constant.  vch
 */
int setsockopt(int fd, int level, int optname, const void * optval, int optlen)
{
   int ret;

   if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
     	if ((ret = machdep_sys_setsockopt(fd_table[fd]->fd.i, level,
		  optname, optval, optlen)) < OK) {
			SET_ERRNO(-ret);
			ret = NOTOK;
     	}
    	fd_unlock(fd, FD_RDWR);
   	}
	return ret;
}

#endif

#if defined (HAVE_SYSCALL_GETSOCKOPT) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * getsockopt()
 */
int getsockopt(int fd, int level, int optname, void * optval, int * optlen)
{
   int ret;

   if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
     	if ((ret = machdep_sys_getsockopt(fd_table[fd]->fd.i, level,
		  optname, optval, optlen)) < OK) {
			SET_ERRNO(-ret);
			ret = NOTOK;
     	}
    	fd_unlock(fd, FD_RDWR);
   	}
	return ret;
}

#endif

#if defined (HAVE_SYSCALL_GETPEERNAME) || defined (HAVE_SYSCALL_SOCKETCALL)

/* ==========================================================================
 * getpeername()
 */
int getpeername(int fd, struct sockaddr * peer, int * paddrlen)
{
   int ret;

   if ((ret = fd_lock(fd, FD_READ, NULL)) == OK) {
     	if ((ret = machdep_sys_getpeername(fd_table[fd]->fd.i, 
		  peer, paddrlen)) < OK) {
			SET_ERRNO(-ret);
			ret = NOTOK;
     	}
    	fd_unlock(fd, FD_READ);
   	}
	return ret;
}

#endif

#if defined(__ELF__) || defined(__GNU_LIBRARY__)
#include <gnu-stabs.h>
#ifdef elf_alias
elf_alias (fchmod, __fchmod);
elf_alias (fchown, __fchown);
elf_alias (open, __open);
elf_alias (pipe, __pipe);
#endif
#endif
