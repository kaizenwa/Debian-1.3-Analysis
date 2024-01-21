/* ==== signal.c ============================================================
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
 * Description : Queue functions.
 *
 *  1.00 93/07/21 proven
 *      -Started coding this file.
 */

#ifndef lint
static const char rcsid[] = "$Id: signal.c,v 1.7 1996/10/04 22:53:37 hjl Exp $";
#endif

#include <pthread.h>
#include <signal.h>
#include <config.h>

#ifndef __ELF__
/* This will force init.o to get dragged in; if you've got support for
   C++ initialization, that'll cause pthread_init to be called at
   program startup automatically, so the application won't need to
   call it explicitly.  */

extern char __pthread_init_hack;
char *__pthread_init_hack_2 = &__pthread_init_hack;
#endif

/*
 * Time which select in fd_kern_wait() will sleep.
 * If there are no threads to run we sleep for an hour or until
 * we get an interrupt or an fd thats awakens. To make sure we
 * don't miss an interrupt this variable gets reset too zero in
 * sig_handler_real().
 */
struct timeval __fd_kern_wait_timeout = { 0, 0 };

/*
 * Global for user-kernel lock, and blocked signals
 */

static sig_atomic_t signum_to_process[SIGMAX + 1] = { 0, };
static sig_atomic_t sig_to_process = 0;

/* static volatile	sigset_t sig_to_process; */
static volatile	int	sig_count = 0;

#if 0
    /* These prototypes were in the file but don't really exist vch */
void          sig_prevent       (void);
void          sig_resume        (void);
#endif

/* Make this global so BROKEN signal semantics can restart signals */
#if defined(SA_RESTART) || defined(SA_RESETHAND)
struct sigaction act;
#endif

extern void fd_kern_poll            (void);
extern void free                    (__ptr_t __ptr);
extern void fd_kern_wait            (void);
extern int  machdep_sys_sigsuspend  (sigset_t *);
extern int  pthread_sig_register    (int);
extern int  sleep_wakeup            (void);
extern int  wait_wakeup             (void);
extern void pthread_sig_process     (void);
extern int  pthread_signal          (int, void (*dispatch)(int));

/*
 * These don't seem to be implemented. vch
 */
extern void machdep_sys_sigprocmask (int, sigset_t *, sigset_t *);

/*
 * Prototypes.
 */
static void          context_switch              (void);
#if !defined(HAVE_SYSCALL_SIGSUSPEND) && defined(HAVE_SYSCALL_SIGPAUSE)
int                  machdep_sys_sigsuspend      (sigset_t * set);
#endif
void                 sig_handler_pause           (void);
void                 context_switch_done         (void);
static void          set_thread_timer            (void);
static inline void   sigvtalrm                   (void); 
static inline void   sigdefault                  (int);
static inline void   sig_handler_switch          (int);
static void          sig_handler                 (int);
void                 sig_handler_real            (int);
void                 sig_handler_fake            (int);
void                 pthread_sched_other_resume  (struct pthread *);
void                 pthread_resched_resume      (enum pthread_state);
void                 pthread_sched_resume        (void);
void                 pthread_sched_prevent       (void);
void                 sig_init                    (void);

/* ==========================================================================
 * context_switch()
 *
 * This routine saves the current state of the running thread gets
 * the next thread to run and restores it's state. To allow different
 * processors to work with this routine, I allow the machdep_restore_state()
 * to either return or have it return from machdep_save_state with a value
 * other than 0, this is for implementations which use setjmp/longjmp. 
 */
static void context_switch()
{
  struct pthread *next, *last, **dead;

  if (pthread_run->state == PS_RUNNING) {
    /* Put current thread back on the queue */
    pthread_prio_queue_enq(pthread_current_prio_queue, pthread_run);
  }

  /* save floating point registers if necessary */
  if (!(pthread_run->attr.flags & PTHREAD_NOFLOAT)) {
    machdep_save_float_state(pthread_run);
  }
  /* save state of current thread */
  if (machdep_save_state()) {
    return;
  }

  /* Poll all fds */
  fd_kern_poll();

  /* Remove dead threads */
  for (dead = &pthread_dead; *dead; ) {
    if ((*dead) == pthread_run) {
      dead = &((*dead)->next);
      continue;
    }
    if ((*dead)->attr.flags & PTHREAD_DETACHED) {
      struct pthread **current;
      if (!((*dead)->attr.stackaddr_attr)) {
        free (machdep_pthread_cleanup(&((*dead)->machdep_data)));
      }
      for (current = &pthread_link_list; (*current) != (*dead);
           current = &((*current)->pll));
      *dead = (*current)->next;
      next = (*current)->pll;
      free (*current);
      *current = next;
    } else {
      dead = &((*dead)->next);
    }
  }

  last = pthread_run;

 context_switch_reschedule:;
  /* Are there any threads to run */
  if ((pthread_run = pthread_prio_queue_deq(pthread_current_prio_queue))) {
    /* restore floating point registers if necessary */
    if (!(pthread_run->attr.flags & PTHREAD_NOFLOAT)) {
      machdep_restore_float_state();
    }
    /* restore state of new current thread */
    machdep_restore_state();
    return;
  }

  /* Are there any threads at all */
  for (next = pthread_link_list; next; next = next->pll) {
    if (next->state != PS_DEAD) {
      sigset_t sig_to_block, oset;

      sigfillset(&sig_to_block);

      /*
       * Check sig_to_process before calling fd_kern_wait, to handle
       * things like zero timeouts to select() which would register
       * a signal with the sig_handler_fake() call.
       *
       * This case should ignore SIGVTALRM
       */
      machdep_sys_sigprocmask(SIG_BLOCK, &sig_to_block, &oset);
      signum_to_process[SIGVTALRM] = 0;
      if (sig_to_process) {
        /* Process interrupts */
        /*
         * XXX pthread_run should not be set places where it
         *  core dumps should be fixed to check pthread_run
         */
        sig_handler(0);
      } else {
        machdep_sys_sigprocmask(SIG_UNBLOCK, &sig_to_block, &oset);
        /*
         * Do a wait, timeout is set to a hour unless we get an 
         * intr. before the select in wich case it polls.
         */
        fd_kern_wait();
        machdep_sys_sigprocmask(SIG_BLOCK, &sig_to_block, &oset);
        /* Check for interrupts, but ignore SIGVTALR */
        signum_to_process[SIGVTALRM] = 0;
        if (sig_to_process) {
          /* Process interrupts */
          sig_handler(0); 
        }
      }
      machdep_sys_sigprocmask(SIG_UNBLOCK, &sig_to_block, &oset); 
      goto context_switch_reschedule;
    }
  }

  /* There are no threads alive. */
  pthread_run = last;
  exit(0);
}

#if !defined(HAVE_SYSCALL_SIGSUSPEND) && defined(HAVE_SYSCALL_SIGPAUSE)

/* ==========================================================================
 * machdep_sys_sigsuspend()
 */ 
int machdep_sys_sigsuspend(sigset_t * set)
{
  return(machdep_sys_sigpause(* set));
}

#endif

/* ==========================================================================
 * sig_handler_pause()
 * 
 * Wait until a signal is sent to the process.
 */
void sig_handler_pause(void)
{
  sigset_t sig_to_block, sig_to_pause, oset;

  sigfillset(&sig_to_block);
  sigemptyset(&sig_to_pause);
  machdep_sys_sigprocmask(SIG_BLOCK, &sig_to_block, &oset);
  if (!sig_to_process) {
    machdep_sys_sigsuspend(&sig_to_pause);
  }
  machdep_sys_sigprocmask(SIG_UNBLOCK, &sig_to_block, &oset);
}

/* ==========================================================================
 * context_switch_done()
 *
 * This routine does all the things that are necessary after a context_switch()
 * calls the machdep_restore_state(). DO NOT put this in the context_switch()
 * routine because sometimes the machdep_restore_state() doesn't return
 * to context_switch() but instead ends up in machdep_thread_start() or
 * some such routine, which will need to call this routine and
 * sig_check_and_resume().
 */
void context_switch_done()
{
  signum_to_process[SIGVTALRM] = 0;
  set_thread_timer();
}

/* ==========================================================================
 * set_thread_timer()
 *
 * Assums kernel is locked.
 */
static void set_thread_timer()
{
  static int last_sched_attr = SCHED_RR;

  switch (pthread_run->attr.schedparam_policy) {
  case SCHED_RR:
    machdep_set_thread_timer(&(pthread_run->machdep_data));
    break;
  case SCHED_FIFO:
    if (last_sched_attr != SCHED_FIFO) {
      machdep_unset_thread_timer(NULL);
    }
    break;
  case SCHED_IO:
    if ((last_sched_attr != SCHED_IO) && (!sig_count)) {
      machdep_set_thread_timer(&(pthread_run->machdep_data));
    }
    break;
  default:
    machdep_set_thread_timer(&(pthread_run->machdep_data));
    break;
  } 
  last_sched_attr = pthread_run->attr.schedparam_policy;
}

/* ==========================================================================
 * sigvtalrm()
 */
static inline void sigvtalrm() 
{
  if (sig_count) {
    sigset_t sigall, oset;

    sig_count = 0;

    /* Unblock all signals */
    sigemptyset(&sigall);
    machdep_sys_sigprocmask(SIG_SETMASK, &sigall, &oset); 
  }
  context_switch();
  context_switch_done();
}

/* ==========================================================================
 * sigdefault()
 */
static inline void sigdefault(int sig)
{
  int ret;

  ret = pthread_sig_register(sig);
  if (pthread_run && (ret > pthread_run->pthread_priority)) {
    sigvtalrm();
  }
}

/* ==========================================================================
 * sig_handler_switch()
 */
static inline void sig_handler_switch(int sig)
{
  int ret;

  switch(sig) {
  case 0:
    break;
  case SIGVTALRM:
    sigvtalrm();
    break;
  case SIGALRM:
    signum_to_process[SIGALRM] = 0;
    switch (ret = sleep_wakeup()) {
    default:
      if (pthread_run && (ret > pthread_run->pthread_priority)) {
        sigvtalrm();
      }
    case 0:
      break;
    case NOTOK:
      /* Do the registered action, no threads were sleeping */
      sigdefault(sig);
      break;
    } 
    break;
  case SIGCHLD:
    signum_to_process[SIGCHLD] = 0;
    switch (ret = wait_wakeup()) {
    default:
      if (pthread_run && (ret > pthread_run->pthread_priority)) {
        sigvtalrm();
      }
    case 0:
      break;
    case NOTOK:
      /* Do the registered action, no threads were waiting */
      sigdefault(sig);
      break;
    } 
    break;
#ifdef SIGINFO
  case SIGINFO:
    pthread_dump_info ();
    /* Then fall through, invoking the application's
       signal handler after printing our info out.

       I'm not convinced that this is right, but I'm not
       100% convinced that it is wrong, and this is how
       Chris wants it done...  */
#endif

  default:
    /* Do the registered action */
    signum_to_process[sig] = 0;
    sigdefault(sig);
    break;
  }
}

/* ==========================================================================
 * sig_handler()
 *
 * Process signal that just came in, plus any pending on the signal mask.
 * All of these must be resolved.
 *
 * Assumes the kernel is locked. 
 */
static void sig_handler(int sig)
{
  if (pthread_kernel_lock != 1) PANIC();

  if (sig) sig_handler_switch(sig);

  while (sig_to_process) {
    for (sig_to_process = 0, sig = 1; sig <= SIGMAX; sig++) {
      if (signum_to_process[sig]) {
        sig_handler_switch(sig);
      }
    }
  }
}

/* ==========================================================================
 * sig_handler_real()
 * 
 * On a multi-processor this would need to use the test and set instruction
 * otherwise the following will work.
 */
void sig_handler_real(int sig)
{
  /* Get around systems with BROKEN signal handlers */
#if defined(SA_RESETHAND) || defined(SA_RESTART)
  __sigaction(sig, &act, NULL);
#endif /* SA_RESETHAND */

  if (pthread_kernel_lock) {
    __fd_kern_wait_timeout.tv_sec = 0;
    signum_to_process[sig] = 1;
    sig_to_process = 1;
    return;
  }
  pthread_kernel_lock++;

  sig_count++;
  sig_handler(sig);

  /* Handle any signals the current thread might have just gotten */
  if (pthread_run && pthread_run->sigcount)
    pthread_sig_process();

  pthread_kernel_lock--;
}

/* ==========================================================================
 * sig_handler_fake()
 */
void sig_handler_fake(int sig)
{
  if (pthread_kernel_lock) {
    signum_to_process[sig] = 1;
    sig_to_process = 1;
    return;
  }
  pthread_kernel_lock++;
  sig_handler(sig);
  while (!(--pthread_kernel_lock)) {
    if (sig_to_process) {
      pthread_kernel_lock++;
      sig_handler(0);
    } else {
      break;
    }
  }
}

/* ==========================================================================
 * pthread_sched_other_resume()
 *
 * Check if thread to be resumed is of higher priority and if so
 * stop current thread and start new thread.
 */
void pthread_sched_other_resume(struct pthread * pthread)
{
  pthread->state = PS_RUNNING;
  pthread_prio_queue_enq(pthread_current_prio_queue, pthread);

  if (pthread->pthread_priority > pthread_run->pthread_priority) {
    if (pthread_kernel_lock == 1) {
      sig_handler(SIGVTALRM);
    }
  }

  /* Only bother if we are truly unlocking the kernel */
  while (!(--pthread_kernel_lock)) {
    if (sig_to_process) {
      pthread_kernel_lock++;
      sig_handler(0);
      continue;
    } 
    if (pthread_run && pthread_run->sigcount) {
      pthread_kernel_lock++;
      pthread_sig_process();
      continue;
    }
    break;
  }
}

/* ==========================================================================
 * pthread_resched_resume()
 *
 * This routine assumes that the caller is the current pthread, pthread_run
 * and that it has a lock the kernel thread and it wants to reschedule itself.
 */
void pthread_resched_resume(enum pthread_state state)
{
  pthread_run->state = state;
  sig_handler(SIGVTALRM);

  /* Only bother if we are truely unlocking the kernel */
  while (!(--pthread_kernel_lock)) {
    if (sig_to_process) {
      pthread_kernel_lock++;
      sig_handler(0);
      continue;
    } 
    if (pthread_run && pthread_run->sigcount) {
      pthread_kernel_lock++;
      pthread_sig_process();
      continue;
    }
    break;
  }
}

/* ==========================================================================
 * pthread_sched_resume()
 */
void pthread_sched_resume( void )
{
  /* Only bother if we are truely unlocking the kernel */
  while (!(--pthread_kernel_lock)) {
    if (sig_to_process) {
      pthread_kernel_lock++;
      sig_handler(0);
      continue;
    }
    if (pthread_run && pthread_run->sigcount) {
      pthread_kernel_lock++;
      pthread_sig_process();
      continue;
    }
    break;
  }
}

/* ==========================================================================
 * pthread_sched_prevent()
 */
void pthread_sched_prevent(void)
{
  pthread_kernel_lock++;
}

/* ==========================================================================
 * sig_init()
 *
 * SIGVTALRM	(NOT POSIX) needed for thread timeslice timeouts.
 *				Since it's not POSIX I will replace it with a 
 *				virtual timer for threads.
 * SIGALRM		(IS POSIX) so some special handling will be
 * 				necessary to fake SIGALRM signals
 */
#ifndef SIGINFO
#define SIGINFO 0
#endif
void sig_init(void)
{
  static const int sig_init[] = { SIGCHLD, SIGALRM, SIGVTALRM, SIGINFO, 0 };
  static const int sig_ign[]  = { SIGKILL, SIGSTOP, 0 };
  int i, j;

#if defined(SA_RESTART) || defined(SA_RESETHAND)
  act.sa_handler = sig_handler_real;
  sigemptyset(&(act.sa_mask));
#if defined(SA_RESTART)
  act.sa_flags = SA_RESTART;
#else /* !SA_RESTART */
#if !defined(hpux)
  act.sa_flags = SA_RESETHAND;
#else /* hpux */
  act.sa_flags = 0;
#endif /* !hpux */
#endif /* SA_RESTART */
#endif /* SA_RESTART || SA_RESETHAND */

  /* Initialize the signals */
  for (j = 1; j < SIGMAX; j++) {

#if defined(SA_RESTART) || defined(SA_RESETHAND)
    if (__sigaction(j, &act, NULL)) {
#else
    if (signal(j, sig_handler_real)) {
#endif
      for (i = 0; sig_init[i]; i++) if (sig_init[i] == j) PANIC();
    }

    for (i = 0; sig_init[i]; i++) if (sig_init[i] == j) goto sig_next;
    for (i = 0; sig_ign[i];  i++) if (sig_ign[i]  == j) goto sig_next;

    pthread_signal(j, SIG_DFL);

    sig_next:;
  }
}
