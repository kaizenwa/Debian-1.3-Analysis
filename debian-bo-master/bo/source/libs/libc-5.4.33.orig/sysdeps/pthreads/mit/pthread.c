/* ==== pthread.c ============================================================
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
 * Description : Pthread functions.
 *
 *  1.00 93/07/26 proven
 *      -Started coding this file.
 */

#ifndef lint
static const char rcsid[] = "pthread.c,v 1.1 1995/06/11 00:21:38 hjl Exp";
#endif

#include <stdlib.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>
#include <string.h>

extern void pthread_cleanupspecific(void);
extern void pthread_resched_resume (enum pthread_state);

/*
 * Prototypes.
 */

int        sched_yield      (void);
void       pthread_yield    (void);
pthread_t  pthread_self     (void);
int        pthread_equal    (pthread_t, pthread_t);
void       pthread_exit     (void *);
int        pthread_create   (pthread_t *, const pthread_attr_t *,
                             void * (*start_routine)(void *), void *);
/* ==========================================================================
 * sched_yield()
 */
int sched_yield(void)
{
	sig_handler_fake(SIGVTALRM);
	return(OK);
}

/* ==========================================================================
 * pthread_yield()
 */
void pthread_yield(void)
{
	sig_handler_fake(SIGVTALRM);
}

/* ==========================================================================
 * pthread_self()
 */
pthread_t pthread_self(void)
{
	return(pthread_run);
}

/* ==========================================================================
 * pthread_equal(void)
 */
int pthread_equal(pthread_t t1, pthread_t t2)
{
	return(t1 == t2);
}

/* ==========================================================================
 * pthread_exit()
 */


void pthread_exit(void *status)
{
	pthread_t pthread;

	/* Save return value */
	pthread_run->ret = status;

	/* First execute all cleanup handlers */
	while (pthread_run->cleanup) {
		pthread_cleanup_pop(1);
	}

	/* Don't forget the cleanup attr */
	if (pthread_run->attr.cleanup_attr) {
		pthread_run->attr.cleanup_attr(pthread_run->attr.arg_attr);
	}

	/* Next run thread-specific data desctructors */
	if (pthread_run->specific_data) {
		pthread_cleanupspecific();
	}

	pthread_sched_prevent();

	/*
	 * Are there any threads joined to this one,
	 * if so wake them and let them detach this thread.
	 */
	while ((pthread = pthread_queue_deq(&(pthread_run->join_queue)))) {
		pthread_prio_queue_enq(pthread_current_prio_queue, pthread);
		pthread->state = PS_RUNNING;
	}

	/* This thread will never run again */
	pthread_run->next = pthread_dead;
	pthread_dead = pthread_run;
	pthread_resched_resume(PS_DEAD);
	PANIC();
}

/* ==========================================================================
 * pthread_create()
 *
 * After the new thread structure is allocated and set up, it is added to
 * pthread_run_next_queue, which requires a sig_prevent(),
 * sig_check_and_resume()
 */
int pthread_create(pthread_t *thread, const pthread_attr_t *attr,
  void * (*start_routine)(void *), void *arg)
{
	pthread_t new_thread;
        pthread_size_t pthread_pagesize = getpagesize();
	long nsec = 100000000;
	void *stack;
	size_t size;

	if ((new_thread = (pthread_t)malloc(sizeof(struct pthread)))) {

		if (! attr) { attr = &pthread_attr_default; }

		/* Get a stack, if necessary */
		stack = attr->stackaddr_attr;
		if (!stack) {
			size = ((attr->stacksize_attr - 1) / pthread_pagesize + 2)
				* pthread_pagesize;
			stack = (void *)malloc(size);
		}

		if (stack) {
			machdep_pthread_create(&(new_thread->machdep_data),
			  start_routine, arg, attr->stacksize_attr, stack, nsec);

			memcpy(&new_thread->attr, attr, sizeof(pthread_attr_t));
			if (new_thread->attr.flags & PTHREAD_INHERIT_SCHED) {
				new_thread->pthread_priority = pthread_run->pthread_priority; 
				new_thread->attr.prio = pthread_run->pthread_priority;
				new_thread->attr.schedparam_policy = 
				  pthread_run->attr.schedparam_policy;
			}  else {
				new_thread->pthread_priority = new_thread->attr.prio;
			}

			if (!(new_thread->attr.flags & PTHREAD_NOFLOAT)) {
				machdep_save_float_state(new_thread);
			}

			/* Initialize signalmask */
			new_thread->sigmask = pthread_run->sigmask;
			sigemptyset(&(new_thread->sigpending));
			new_thread->sigcount = 0;

			pthread_queue_init(&(new_thread->join_queue));
			new_thread->specific_data = NULL;
			new_thread->specific_data_count = 0;
			new_thread->cleanup = NULL;
			new_thread->queue = NULL;
			new_thread->next = NULL;
			new_thread->flags = 0;

			new_thread->error_p = NULL;
			new_thread->sll = NULL;

			pthread_sched_prevent();

			/* Add to the link list of all threads. */
			new_thread->pll = pthread_link_list;
			pthread_link_list = new_thread;
			(*thread) = new_thread;
			
			pthread_sched_other_resume(new_thread);
			return(OK);
		}
		free(new_thread);
	}
	return(EAGAIN);
}

/* ==========================================================================
 * pthread_cancel()
 *
 * This routine will also require a sig_prevent/sig_check_and_resume()
 */
