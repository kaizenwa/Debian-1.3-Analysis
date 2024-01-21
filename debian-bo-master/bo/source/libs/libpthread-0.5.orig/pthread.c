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

/* Thread creation, initialization, and basic low-level routines */

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include "pthread.h"
#include "internals.h"
#include "restart.h"

/* Descriptor of the initial thread */

struct _pthread pthread_initial_thread = {
  &pthread_initial_thread,    /* pthread_t p_nextlive */
  &pthread_initial_thread,    /* pthread_t p_prevlive */
  NULL,                       /* pthread_t p_nextwaiting */
  0,                          /* int p_pid */
  0,                          /* int p_spinlock */
  0,                          /* int p_signal */
  NULL,                       /* sigjmp_buf * p_signal_buf */
  NULL,                       /* sigjmp_buf * p_cancel_buf */
  0,                          /* char p_terminated */
  0,                          /* char p_detached */
  0,                          /* char p_exited */
  NULL,                       /* void * p_retval */
  0,                          /* int p_retval */
  NULL,                       /* pthread_t p_joining */
  NULL,                       /* struct _pthread_cleanup_buffer * p_cleanup */
  0,                          /* char p_cancelstate */
  0,                          /* char p_canceltype */
  0,                          /* char p_canceled */
  0,                          /* int p_errno */
  0,                          /* int p_h_errno */
  {NULL}                      /* void * p_specific[PTHREAD_KEYS_MAX] */
};

/* Descriptor of the manager thread; none of this is used but the error
   variables and the address for identification.  */

struct _pthread pthread_manager_thread = {
  NULL,                       /* pthread_t p_nextlive */
  NULL,                       /* pthread_t p_prevlive */
  NULL,                       /* pthread_t p_nextwaiting */
  0,                          /* int p_pid */
  0,                          /* int p_spinlock */
  0,                          /* int p_signal */
  NULL,                       /* sigjmp_buf * p_signal_buf */
  NULL,                       /* sigjmp_buf * p_cancel_buf */
  0,                          /* char p_terminated */
  0,                          /* char p_detached */
  0,                          /* char p_exited */
  NULL,                       /* void * p_retval */
  0,                          /* int p_retval */
  NULL,                       /* pthread_t p_joining */
  NULL,                       /* struct _pthread_cleanup_buffer * p_cleanup */
  0,                          /* char p_cancelstate */
  0,                          /* char p_canceltype */
  0,                          /* char p_canceled */
  0,                          /* int p_errno */
  0,                          /* int p_h_errno */
  {NULL}                      /* void * p_specific[PTHREAD_KEYS_MAX] */
};

/* Pointer to the main thread (the father of the thread manager thread) */
/* Originally, this is the initial thread, but this changes after fork() */

pthread_t pthread_main_thread = &pthread_initial_thread;

/* Limit between the stack of the initial thread (above) and the
   stacks of other threads (below). Aligned on a STACK_SIZE boundary. */

char * pthread_initial_thread_bos = NULL;

/* File descriptor for sending requests to the thread manager. */
/* Initially -1, meaning that the thread manager is not running. */

int pthread_manager_request = -1;

/* Other end of the pipe for sending requests to the thread manager. */

int pthread_manager_reader;

/* Limits of the thread manager stack */

char * pthread_manager_thread_bos = NULL;
char * pthread_manager_thread_tos = NULL;

/* For process-wide exit() */

int pthread_exit_requested = 0;
int pthread_exit_code = 0;

/* Forward declarations */

static void pthread_exit_process(int retcode, void * arg);
static void pthread_handle_sigcancel(int sig);

/* Initialize the pthread library.
   Initialization is split in two functions:
   - a constructor function that blocks the PTHREAD_SIG_RESTART signal
     (must do this very early, since the program could capture the signal
      mask with e.g. sigsetjmp before creating the first thread);
   - a regular function called from pthread_create when needed. */

static void pthread_initialize(void) __attribute__((constructor));

static void pthread_initialize(void)
{
  struct sigaction sa;
  sigset_t mask;

  /* For the initial stack, reserve at least STACK_SIZE bytes of stack
     below the current stack address, and align that on a
     STACK_SIZE boundary. */
  pthread_initial_thread_bos = 
    (char *)(((long)CURRENT_STACK_FRAME - 2 * STACK_SIZE) & ~(STACK_SIZE - 1));
  /* Update the descriptor for the initial thread. */
  pthread_initial_thread.p_pid = getpid();
  /* If we have special thread_self processing, initialize that for the
     main thread now.  */
#ifdef INIT_THREAD_SELF
  INIT_THREAD_SELF(&pthread_initial_thread);
#endif
  /* Setup signal handlers for the initial thread.
     Since signal handlers are shared between threads, these settings
     will be inherited by all other threads. */
  sa.sa_handler = pthread_sighandler;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_RESTART; /* does not matter for regular threads, but
                               better for the thread manager */
  sigaction(PTHREAD_SIG_RESTART, &sa, NULL);
  sa.sa_handler = pthread_handle_sigcancel;
  sa.sa_flags = 0;
  sigaction(PTHREAD_SIG_CANCEL, &sa, NULL);

  /* Initially, block PTHREAD_SIG_RESTART. Will be unblocked on demand. */
  sigemptyset(&mask);
  sigaddset(&mask, PTHREAD_SIG_RESTART);
  sigprocmask(SIG_BLOCK, &mask, NULL);
  /* Register an exit function to kill all other threads. */
  /* Do it early so that user-registered atexit functions are called
     before pthread_exit_process. */
  on_exit(pthread_exit_process, NULL);
}

static int pthread_initialize_manager(void)
{
  int manager_pipe[2];

  /* Setup stack for thread manager */
  pthread_manager_thread_bos = malloc(THREAD_MANAGER_STACK_SIZE);
  if (pthread_manager_thread_bos == NULL) return -1;
  pthread_manager_thread_tos =
    pthread_manager_thread_bos + THREAD_MANAGER_STACK_SIZE;
  /* Setup pipe to communicate with thread manager */
  if (pipe(manager_pipe) == -1) {
    free(pthread_manager_thread_bos);
    return -1;
  }
  pthread_manager_request = manager_pipe[1]; /* writing end */
  pthread_manager_reader = manager_pipe[0]; /* reading end */
  /* Start the thread manager */
  if (clone(pthread_manager, (void **) pthread_manager_thread_tos,
            CLONE_VM | CLONE_FS | CLONE_FILES | CLONE_SIGHAND,
            1, manager_pipe[0]) == -1) {
    free(pthread_manager_thread_bos);
    close(manager_pipe[0]);
    close(manager_pipe[1]);
    pthread_manager_request = -1;
    return -1;
  }
  return 0;
}

/* Thread creation */

int pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                   void * (*start_routine)(void *), void *arg)
{
  pthread_t self = thread_self();
  struct pthread_request request;
  if (pthread_manager_request < 0) {
    if (pthread_initialize_manager() < 0) return EAGAIN;
  }
  request.req_thread = self;
  request.req_kind = REQ_CREATE;
  request.req_args.create.attr = attr;
  request.req_args.create.fn = start_routine;
  request.req_args.create.arg = arg;
  sigprocmask(SIG_SETMASK, (const sigset_t *) NULL,
              &request.req_args.create.mask);
  write(pthread_manager_request, (char *) &request, sizeof(request));
  suspend(self);
  if (self->p_retcode == 0) *thread = (pthread_t) self->p_retval;
  return self->p_retcode;
}

/* Simple operations on thread identifiers */

pthread_t pthread_self(void)
{
  return thread_self();
}

int pthread_equal(pthread_t thread1, pthread_t thread2)
{
  return thread1 == thread2;
}

/* Thread scheduling */

int pthread_setschedparam(pthread_t target_thread, int policy,
                          const struct sched_param *param)
{
  if (sched_setscheduler(target_thread->p_pid, policy, param) == -1)
    return errno;
  else
    return 0;
}

int pthread_getschedparam(pthread_t target_thread, int *policy,
                          struct sched_param *param)
{
  int p = sched_getscheduler(target_thread->p_pid);
  if (p == -1) return errno;
  if (sched_getparam(target_thread->p_pid, param) == -1) return errno;
  *policy = p;
  return 0;
}

/* Process-wide exit() request */

static void pthread_exit_process(int retcode, void * arg)
{
  struct pthread_request request;
  pthread_t self = thread_self();

  if (pthread_manager_request >= 0) {
    request.req_thread = self;
    request.req_kind = REQ_PROCESS_EXIT;
    request.req_args.exit.code = retcode;
    write(pthread_manager_request, (char *) &request, sizeof(request));
    suspend(self);
  }
}

/* The handler for the RESTART signal just records the signal received
   in the thread descriptor, and optionally performs a siglongjmp
   (for pthread_cond_timedwait). Also used in sigwait.
   For the thread manager thread, redirect the signal to 
   pthread_manager_sighandler. */

void pthread_sighandler(int sig)
{
  pthread_t self = thread_self();
  if (self == &pthread_manager_thread) {
    pthread_manager_sighandler(sig);
  } else {
    self->p_signal = sig;
    if (self->p_signal_jmp != NULL) siglongjmp(*self->p_signal_jmp, 1);
  }
}

/* The handler for the CANCEL signal checks for cancellation
   (in asynchronous mode) and for process-wide exit and exec requests. */

static void pthread_handle_sigcancel(int sig)
{
  pthread_t self = thread_self();
  sigjmp_buf * jmpbuf;

  if (pthread_exit_requested)
    _exit(pthread_exit_code);
  if (self->p_canceled && self->p_cancelstate == PTHREAD_CANCEL_ENABLE) {
    if (self->p_canceltype == PTHREAD_CANCEL_ASYNCHRONOUS)
      pthread_exit(PTHREAD_CANCELED);
    jmpbuf = self->p_cancel_jmp;
    if (jmpbuf != NULL) {
      self->p_cancel_jmp = NULL;
      siglongjmp(*jmpbuf, 1);
    }
  }
}

/* Reset the state of the thread machinery after a fork().
   Close the pipe used for requests and set the main thread to the forked
   thread.
   Notice that we can't free the stack segments, as the forked thread
   may hold pointers into them. */

void pthread_reset_main_thread()
{
  pthread_t self = thread_self();

  /* Free the thread manager stack */
  free(pthread_manager_thread_bos);
  pthread_manager_thread_bos = pthread_manager_thread_tos = NULL;
  /* Close the two ends of the pipe */
  close(pthread_manager_request);
  close(pthread_manager_reader);
  pthread_manager_request = pthread_manager_reader = -1;
  /* Update the pid of the main thread */
  self->p_pid = getpid();
  /* Make the forked thread the main thread */
  pthread_main_thread = self;
  self->p_nextlive = self;
  self->p_nextlive = self;
}

/* Process-wide exec() request */

void pthread_kill_other_threads_np(void)
{
  /* Terminate all other threads and thread manager */
  pthread_exit_process(0, NULL);
  /* Make current thread the main thread in case the calling thread
     changes its mind, does not exec(), and creates new threads instead. */
  pthread_reset_main_thread();
}

/* Debugging aid */

#ifdef DEBUG

void pthread_message(char * fmt, long arg)
{
  char buffer[1024];
  sprintf(buffer, "%05d : ", getpid());
  sprintf(buffer + 8, fmt, arg);
  write(2, buffer, strlen(buffer));
}

#endif
