/* Linuxthreads - a simple clone()-based implementation of Posix        */
/* threads for Linux.                                                   */
/* Copyright (C) 1996 Xavier Leroy (Xavier.Leroy@inria.fr)              */
/*                                                                      */
/* This program is free software; you can redistribute it and/or        */
/* modify it under the terms of the GNU Library General Public License  */
/* as published by the Free Software Foundation; either version 2       */
/* of the License, or (at your option) any later version.               */
/*                                                                      */
/* This program is distributed in the hope that it will be useful,      */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of       */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        */
/* GNU Library General Public License for more details.                 */

/* Internal data structures */

/* Includes */

#include <setjmp.h>
#include <linux/mm.h>

#include "pt-machine.h"

/* The type of thread descriptors */

struct _pthread {
  pthread_t p_nextlive, p_prevlive; /* Double chaining of active threads */
  pthread_t p_nextwaiting;      /* Next element in the queue holding the thr */
  int p_pid;                    /* PID of Unix process */
  int p_spinlock;               /* Spinlock for synchronized accesses */
  int p_signal;                 /* last signal received */
  sigjmp_buf * p_signal_jmp;    /* where to siglongjmp on a signal or NULL */
  sigjmp_buf * p_cancel_jmp;    /* where to siglongjmp on a cancel or NULL */
  char p_terminated;            /* true if terminated e.g. by pthread_exit */
  char p_detached;              /* true if detached */
  char p_exited;                /* true if the assoc. process terminated */
  void * p_retval;              /* placeholder for return value */
  int p_retcode;                /* placeholder for return code */
  pthread_t p_joining;          /* thread joining on that thread or NULL */
  struct _pthread_cleanup_buffer * p_cleanup; /* cleanup functions */
  char p_cancelstate;           /* cancellation state */
  char p_canceltype;            /* cancellation type (deferred/async) */
  char p_canceled;              /* cancellation request pending */
  int p_errno;                  /* error returned by last system call */
  int p_h_errno;                /* error returned by last netdb function */
  void * p_specific[PTHREAD_KEYS_MAX]; /* thread-specific data */
};

/* The type of messages sent to the thread manager thread */

struct pthread_request {
  pthread_t req_thread;         /* Thread doing the request */
  enum {                        /* Request kind */
    REQ_CREATE, REQ_FREE, REQ_PROCESS_EXIT, REQ_MAIN_THREAD_EXIT
  } req_kind;
  union {                       /* Arguments for request */
    struct {                    /* For REQ_CREATE: */
      const pthread_attr_t * attr; /* thread attributes */
      void * (*fn)(void *);     /*   start function */
      void * arg;               /*   argument to start function */
      sigset_t mask;            /*   signal mask */
    } create;
    struct {                    /* For REQ_FREE: */
      pthread_t thread;         /*   ID of thread to free */
    } free;
    struct {                    /* For REQ_PROCESS_EXIT: */
      int code;                 /*   exit status */
    } exit;
  } req_args;
};

/* Signals used for suspend/restart and for cancellation notification.
   FIXME: shoud use new, unallocated signals. */

#define PTHREAD_SIG_RESTART SIGUSR1
#define PTHREAD_SIG_CANCEL SIGUSR2

/* Descriptor of the initial thread */

extern struct _pthread pthread_initial_thread;

/* Descriptor of the manager thread */

extern struct _pthread pthread_manager_thread;

/* Descriptor of the main thread */

extern pthread_t pthread_main_thread;

/* Limit between the stack of the initial thread (above) and the
   stacks of other threads (below). Aligned on a STACK_SIZE boundary.
   Initially 0, meaning that the current thread is (by definition)
   the initial thread. */

extern char * pthread_initial_thread_bos;

/* File descriptor for sending requests to the thread manager.
   Initially -1, meaning that pthread_initialize must be called. */

extern int pthread_manager_request;

/* Other end of the pipe for sending requests to the thread manager. */

extern int pthread_manager_reader;

/* Limits of the thread manager stack. */

extern char * pthread_manager_thread_bos;
extern char * pthread_manager_thread_tos;

/* Pending request for a process-wide exit */

extern int pthread_exit_requested, pthread_exit_code;

/* Fill in defaults left unspecified by pt-machine.h.  */

/* The page size we can get from the system.  This should likely not be
   changed by the machine file but, you never know.  */
#ifndef PAGE_SIZE
#define PAGE_SIZE  (sysconf(_SC_PAGE_SIZE))
#endif

/* The max size of the thread stack segments.  If the default
   THREAD_SELF implementation is used, this must be a power of two and
   a multiple of PAGE_SIZE.  */
#ifndef STACK_SIZE
#define STACK_SIZE  (2 * 1024 * 1024)
#endif

/* The initial size of the thread stack.  Must be a multiple of PAGE_SIZE.  */
#ifndef INITIAL_STACK_SIZE
#define INITIAL_STACK_SIZE  (4 * PAGE_SIZE)
#endif

/* Size of the thread manager stack */
#ifndef THREAD_MANAGER_STACK_SIZE
#define THREAD_MANAGER_STACK_SIZE  (2 * PAGE_SIZE - 32)
#endif

/* The base of the "array" of thread stacks.  The array will grow down from
   here.  Defaults to the calculated bottom of the initial application
   stack.  */
#ifndef THREAD_STACK_START_ADDRESS
#define THREAD_STACK_START_ADDRESS  pthread_initial_thread_bos
#endif

/* Get some notion of the current stack.  Need not be exactly the top
   of the stack, just something somewhere in the current frame.  */
#ifndef CURRENT_STACK_FRAME
#define CURRENT_STACK_FRAME  ({ char __csf; &__csf; })
#endif

/* Recover thread descriptor for the current thread */
  
extern inline pthread_t thread_self (void) __attribute__((const));
extern inline pthread_t thread_self (void)
  {
#ifdef THREAD_SELF
  THREAD_SELF
#else
  char *sp = CURRENT_STACK_FRAME;
  if (sp >= pthread_initial_thread_bos)
    return &pthread_initial_thread;
  else if (sp >= pthread_manager_thread_bos && sp < pthread_manager_thread_tos)
    return &pthread_manager_thread;
  else
    return (pthread_t)(((unsigned long)sp | (STACK_SIZE-1))+1) - 1;
#endif
  }

/* Debugging */

#ifdef DEBUG
#include <assert.h>
#define ASSERT assert
#define MSG pthread_message
#else
#define ASSERT(x)
#define MSG(msg,arg)
#endif

/* Internal global functions */

void pthread_destroy_specifics(void);
void pthread_perform_cleanup(void);
void pthread_sighandler(int sig);
void pthread_message(char * fmt, long arg);
int pthread_manager(int reqfd);
void pthread_manager_sighandler(int sig);
void pthread_reset_main_thread(void);
void fresetlockfiles_np(void);

/* Missing system calls */

int clone(int (*child_function)(), void ** child_stack, int flags,
          int nargs, ...);
/* These are system calls missing from libc 5.2.18 on the Intel */
#ifdef __i386__
int nanosleep(const struct timespec * rqtp, struct timespec * rmtp);
int sched_yield(void);
int sched_setparam(pid_t __pid, const struct sched_param *__param);
int sched_getparam(pid_t __pid, struct sched_param *__param);
int sched_setscheduler(pid_t __pid, int __policy,
                       const struct sched_param *__param);
int sched_getscheduler(pid_t __pid);
int sched_get_priority_max(int __policy);
int sched_get_priority_min(int __policy);
int sched_rr_get_interval(pid_t __pid, struct timespec *interval);
#endif
