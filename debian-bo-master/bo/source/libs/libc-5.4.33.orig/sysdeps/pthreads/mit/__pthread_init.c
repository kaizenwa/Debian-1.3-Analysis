/* ==== __pthread_init.c ======================================================
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
 * Description : Pthread_init routine.
 *
 *  1.00 94/09/20 proven
 *      -Started coding this file.
 */

#ifndef lint
static const char rcsid[] = "__pthread_init.c,v 1.3 1995/08/05 05:07:03 hjl Exp";
#endif

#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <libc-lock.h>

/* 
 * errno is declared here to prevent the linker from pulling in errno
 * from the C library (and whatever else is in that file). I also use
 * errno as the default location for error numbers for the initial thread
 * giving some backwards compatibility.
 */
#ifdef errno
#undef errno
#endif

/* This is used by the linker to put on the constructor list. */
static void __pthread_init (void) __attribute__ ((constructor));

extern int errno;

extern void sig_init(void);

/* ==========================================================================
 * __pthread_init()
 *
 * This function should be called in crt0.o before main() is called.
 * But on some systems It may not be possible to change crt0.o so currently
 * I'm requiring this function to be called first thing after main.
 * Actually I'm assuming it is, because I do no locking here.
 */
static void
__pthread_init(void)
{
  struct machdep_pthread machdep_data = MACHDEP_PTHREAD_INIT;

  /* Only call this once */
  if (pthread_initial) {
    return;
  }

  /* Initialize the first thread */
  if ((pthread_initial = (pthread_t)malloc(sizeof(struct pthread))) &&
      (pthread_current_prio_queue = (struct pthread_prio_queue *)
       malloc(sizeof(struct pthread_prio_queue)))) {
    memcpy(&(pthread_initial->machdep_data), &machdep_data, 
           sizeof(machdep_data));
    pthread_initial->pthread_priority = PTHREAD_DEFAULT_PRIORITY;
    pthread_initial->state = PS_RUNNING;

    pthread_queue_init(&(pthread_initial->join_queue));
    pthread_initial->cleanup = NULL;
    pthread_initial->queue = NULL;
    pthread_initial->next = NULL;
    pthread_initial->pll = NULL;
    pthread_initial->flags = 0;

    /* Ugly errno hack */
    pthread_initial->error_p = &errno;
    pthread_initial->error = 0;

    pthread_prio_queue_init(pthread_current_prio_queue);
    pthread_link_list = pthread_initial;
    pthread_run = pthread_initial;

    /* XXX can I assume the mask and pending siganl sets are empty. */
    pthread_initial->sigmask = __SIGEMPTYSET;
    pthread_initial->sigpending = __SIGEMPTYSET;
    pthread_initial->sigcount = 0;

    /* Initialize the signal handler. */
    sig_init();

    /* Initialize the fd table. */
    fd_init();

    /* Start the scheduler */
    machdep_set_thread_timer(&(pthread_run->machdep_data));

    return;
  }
  PANIC();
}

/* That is used by libio */
__libc_lock_define_initialized(, __libc_libio_lock);

/* That is used by malloc. */
__libc_lock_define_initialized(, __libc_malloc_lock);

/* localtime lock. */
__libc_lock_define_initialized(, __libc_localtime_lock);

/* gmtime lock. */
__libc_lock_define_initialized(, __libc_gmtime_lock);
