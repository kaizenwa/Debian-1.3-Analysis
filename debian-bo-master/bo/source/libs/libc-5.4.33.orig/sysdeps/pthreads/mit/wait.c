/* ==== wait.c ============================================================
 * Copyright (c) 1994 by Chris Provenzano, proven@mit.edu
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
 * Description : All the appropriate wait routines.
 *
 *  1.38 94/06/13 proven
 *      -Started coding this file.
 *
 */

#ifndef lint
static const char rcsid[] = "$Id: wait.c,v 1.2 1995/08/06 06:19:06 hjl Exp $";
#endif

#include <pthread.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <pthread/mit/posix.h>
#ifdef __linux__
#include <pthread/mit/sys/compat.h>
#else
#include <sys/compat.h>
#endif
#include <sys/wait.h>

/* This is an UGLY hack to get wait to compile, something better is needed. */
/* #define _POSIX_SOURCE
#undef _POSIX_SOURCE
*/

struct pthread_queue wait_queue = { NULL, NULL, NULL };

extern void sig_handler_real        (int);
extern void pthread_sched_resume    (void);
extern void pthread_resched_resume  (enum pthread_state);

/*
 * Prototypes
 */
int    wait_wakeup   (void);
pid_t  waitpid       (pid_t, int *, int);
pid_t  wait3         (__WAIT_STATUS, int, struct rusage *);
pid_t  wait          (__WAIT_STATUS);

/* ==========================================================================
 * wait_wakeup()
 *
 * This routine is called by the interrupt handler which has locked
 * the current kthread semaphore. Since only threads owned by the 
 * current kthread can be queue here, no additional locks are necessary.
 */
int wait_wakeup( void )
{
	struct pthread *pthread;
	int ret = 0;

	if ((pthread = pthread_queue_deq(& wait_queue))) {
		/* Wakeup all threads, and enqueue them on the run queue */
		do {
			pthread->state = PS_RUNNING;
			if (pthread->pthread_priority > ret) {
				ret = pthread->pthread_priority;
			}
			pthread_prio_queue_enq(pthread_current_prio_queue, pthread);
		} while ((pthread = pthread_queue_deq(&wait_queue)));
		return(ret);
	}
	return(NOTOK);
}

/* ==========================================================================
 * For the wait calls, it is important that the current kthread is locked
 * before the apropriate wait syscall is preformed. This way we ensure
 * that there is never a case where a thread is waiting for a child but
 * missed the interrupt for that child.
 */

/* ==========================================================================
 * waitpid()
 */
pid_t waitpid(pid_t pid, int *status, int options)
{
	pid_t ret;

	pthread_sched_prevent();
	while ((ret = machdep_sys_waitpid(pid, status, options | WNOHANG)) == OK) {

		/* Enqueue thread on wait queue */
		pthread_queue_enq(&wait_queue, pthread_run);

		/* reschedule unlocks scheduler */
		pthread_resched_resume(PS_WAIT_WAIT);

		pthread_sched_prevent();
	}
	pthread_sched_resume();
	return(ret);
}

/* ==========================================================================
 * wait3()
 */
pid_t wait3(__WAIT_STATUS status, int options, struct rusage * rusage)
{
	semaphore * lock;
	pid_t ret;

/*
 * The prototype is:      vch
 * int machdep_sys_wait3 __P((int *, int, int *));
 * "/usr/include/sys/wait.h    " line 72 : #define __WAIT_STATUS int *
 * "/usr/include/linux/wait.h  " line 4  : #define WNOHANG 0x00000001
 * "/usr/include/pthread/mit/state.def" line 52 :
 * 	__pthread_defstate (PS_WAIT_WAIT, "wait")
 */

	pthread_sched_prevent();
	while ((ret = machdep_sys_wait3(status, options | WNOHANG,
          (int *)rusage)) == OK) {

		/* Enqueue thread on wait queue */
		pthread_queue_enq(&wait_queue, pthread_run);

		/* reschedule unlocks scheduler */
		pthread_resched_resume(PS_WAIT_WAIT);

		pthread_sched_prevent();
	}
	pthread_sched_resume();
	return(ret);
}

/* ==========================================================================
 * wait()
 */
pid_t wait(__WAIT_STATUS status)
{
	return(waitpid((pid_t)-1, (int *)status, 0));
}

#if defined(__ELF__) || defined(__GNU_LIBRARY__)
#include <gnu-stabs.h>
#ifdef elf_alias
elf_alias (wait, __wait);
elf_alias (wait3, __wait3);
elf_alias (waitpid, __waitpid);
#endif
#endif
