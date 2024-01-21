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

/* The "thread manager" thread: manages creation and termination of threads */

#define _REENTRANT
#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>           /* for select */
#include <sys/types.h>          /* for select */
#include <sys/mman.h>           /* for mmap */
#include <sys/wait.h>           /* for waitpid macros */
#include <linux/sched.h>

#include "pthread.h"
#include "internals.h"
#include "spinlock.h"
#include "restart.h"

/* Boolean array indicating which stack segments (areas aligned on a
   STACK_SIZE boundary) are currently in use.  */

static char * stack_segments = NULL;
static int num_stack_segments = 0;

/* Mapping between thread descriptors and stack segments */

#define THREAD_SEG(seg) \
  ((pthread_t)(THREAD_STACK_START_ADDRESS - (seg) * STACK_SIZE) - 1)
#define SEG_THREAD(thr) \
  (((size_t)THREAD_STACK_START_ADDRESS - (size_t)(thr+1)) / STACK_SIZE)

/* Flag set in signal handler to record child termination */

static int terminated_children = 0;

/* Flag set when the initial thread is blocked on pthread_exit waiting
   for all other threads to terminate */

static int main_thread_exiting = 0;

/* Forward declarations */

static int pthread_handle_create(pthread_t *thread, const pthread_attr_t *attr,
                                 void * (*start_routine)(void *), void *arg,
                                 sigset_t mask, int father_pid);
static void pthread_handle_free(pthread_t th);
static void pthread_handle_exit(pthread_t issuing_thread, int exitcode);
static void pthread_reap_children();
static void pthread_kill_all_threads(int sig, int main_thread_also);

/* The server thread managing requests for thread creation and termination */

int pthread_manager(int reqfd)
{
  sigset_t mask;
  fd_set readfds;
  struct timeval timeout;
  int n;
  struct pthread_request request;

  /* If we have special thread_self processing, initialize it.  */
#ifdef INIT_THREAD_SELF
  INIT_THREAD_SELF(&pthread_manager_thread);
#endif
  /* Block all signals except PTHREAD_SIG_RESTART */
  sigfillset(&mask);
  sigdelset(&mask, PTHREAD_SIG_RESTART);
  sigprocmask(SIG_SETMASK, &mask, NULL);
  /* Enter server loop */
  while(1) {
    FD_ZERO(&readfds);
    FD_SET(reqfd, &readfds);
    timeout.tv_sec = 2;
    timeout.tv_usec = 0;
    n = select(FD_SETSIZE, &readfds, NULL, NULL, &timeout);
    /* Check for termination of the main thread */
    if (getppid() == 1) {
      pthread_kill_all_threads(SIGKILL, 0);
      return 0;
    }
    /* Check for dead children */
    if (terminated_children) {
      terminated_children = 0;
      pthread_reap_children();
    }
    /* Read and execute request */
    if (n == 1 && FD_ISSET(reqfd, &readfds)) {
      n = read(reqfd, (char *)&request, sizeof(request));
      ASSERT(n == sizeof(request));
      switch(request.req_kind) {
      case REQ_CREATE:
        request.req_thread->p_retcode =
          pthread_handle_create((pthread_t *) &request.req_thread->p_retval,
                                request.req_args.create.attr,
                                request.req_args.create.fn,
                                request.req_args.create.arg,
                                request.req_args.create.mask,
                                request.req_thread->p_pid);
        restart(request.req_thread);
        break;
      case REQ_FREE:
        pthread_handle_free(request.req_args.free.thread);
        break;
      case REQ_PROCESS_EXIT:
        pthread_handle_exit(request.req_thread,
                            request.req_args.exit.code);
        break;
      case REQ_MAIN_THREAD_EXIT:
        main_thread_exiting = 1;
        if (pthread_main_thread->p_nextlive == pthread_main_thread) {
          restart(pthread_main_thread);
          return 0;
        }
        break;
      }
    }
  }
}

/* Allocate or reallocate stack segments */

static int pthread_grow_stack_segments()
{
  int new_num_sseg;
  char * new_sseg;

  if (num_stack_segments == 0) {
    new_num_sseg = 128;
    new_sseg = malloc(new_num_sseg);
  } else {
    new_num_sseg = num_stack_segments * 2;
    new_sseg = realloc(stack_segments, new_num_sseg);
  }
  if (new_sseg == NULL) return -1;
  memset(new_sseg + num_stack_segments, 0, new_num_sseg - num_stack_segments);
  stack_segments = new_sseg;
  num_stack_segments = new_num_sseg;
  return 0;
}

/* Process creation */

static int pthread_start_thread(void * (*start_routine)(void *), void *arg,
                                sigset_t mask, pthread_t self)
{
  void * outcome;
  /* Initialize special thread_self processing, if any.  */
#ifdef INIT_THREAD_SELF
  INIT_THREAD_SELF(self);
#endif
  /* Make sure our pid field is initialized, just in case we get there
     before our father has initialized it. */
  self->p_pid = getpid();
  /* Initial signal mask is that of the creating thread. (Otherwise,
     we'd just inherit the mask of the thread manager.) */
  sigprocmask(SIG_SETMASK, &mask, NULL);
  /* Run the thread code */
  outcome = start_routine(arg);
  /* Exit with the given return value */
  pthread_exit(outcome);
  return 0;
}

static int pthread_handle_create(pthread_t *thread, const pthread_attr_t *attr,
                                 void * (*start_routine)(void *), void *arg,
                                 sigset_t mask, int father_pid)
{
  size_t sseg;
  int pid;
  pthread_t new_thread;
  int i;

  /* Find a free stack segment for the current stack */
  sseg = 0;
  while (1) {
    while (1) {
      if (sseg >= num_stack_segments) {
        if (pthread_grow_stack_segments() == -1) return EAGAIN;
      }
      if (stack_segments[sseg] == 0) break;
      sseg++;
    }
    stack_segments[sseg] = 1;
    new_thread = THREAD_SEG(sseg);
    /* Allocate space for stack and thread descriptor. */
    if (mmap((caddr_t)((char *)(new_thread+1) - INITIAL_STACK_SIZE),
             INITIAL_STACK_SIZE, PROT_READ | PROT_WRITE | PROT_EXEC,
             MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED | MAP_GROWSDOWN, -1, 0)
        != (caddr_t) -1) break;
    /* It seems part of this segment is already mapped. Leave it marked
       as reserved (to speed up future scans) and try the next. */
    sseg++;
  }      
  /* Initialize the thread descriptor */
  new_thread->p_nextwaiting = NULL;
  new_thread->p_spinlock = 0;
  new_thread->p_signal = 0;
  new_thread->p_signal_jmp = NULL;
  new_thread->p_cancel_jmp = NULL;
  new_thread->p_terminated = 0;
  new_thread->p_detached = attr == NULL ? 0 : attr->detachstate;
  new_thread->p_exited = 0;
  new_thread->p_retval = NULL;
  new_thread->p_joining = NULL;
  new_thread->p_cleanup = NULL;
  new_thread->p_cancelstate = PTHREAD_CANCEL_ENABLE;
  new_thread->p_canceltype = PTHREAD_CANCEL_DEFERRED;
  new_thread->p_canceled = 0;
  new_thread->p_errno = 0;
  new_thread->p_h_errno = 0;
  for (i = 0; i < PTHREAD_KEYS_MAX; i++) new_thread->p_specific[i] = NULL;
  /* Do the cloning */
  pid = clone(pthread_start_thread, (void **) new_thread,
              CLONE_VM | CLONE_FS | CLONE_FILES | CLONE_SIGHAND |
              PTHREAD_SIG_RESTART,
              4, start_routine, arg, mask, new_thread);
  /* Check if cloning succeeded */
  if (pid == -1) {
    /* Free the stack */
    munmap((caddr_t)((char *)(new_thread+1) - INITIAL_STACK_SIZE),
           INITIAL_STACK_SIZE);
    stack_segments[sseg] = 0;
    return EAGAIN;
  }
  /* Set the priority and policy for the new thread, if available. */
  if (attr != NULL && attr->schedpolicy != SCHED_OTHER) {
    switch(attr->inheritsched) {
    case PTHREAD_EXPLICIT_SCHED:
      sched_setscheduler(pid, attr->schedpolicy, &attr->schedparam);
      break;
    case PTHREAD_INHERIT_SCHED:
      { struct sched_param father_param;
        int father_policy;
        father_policy = sched_getscheduler(father_pid);
        sched_getparam(father_pid, &father_param);
        sched_setscheduler(pid, father_policy, &father_param);
      }
      break;
    }
  }
  /* Insert new thread in doubly linked list of active threads */
  new_thread->p_prevlive = pthread_main_thread;
  new_thread->p_nextlive = pthread_main_thread->p_nextlive;
  pthread_main_thread->p_nextlive->p_prevlive = new_thread;
  pthread_main_thread->p_nextlive = new_thread;
  /* Set pid field of the new thread, in case we get there before the
     child starts. */
  new_thread->p_pid = pid;
  /* We're all set */
  *thread = new_thread;
  return 0;
}

/* Free the resources of a thread. */

static void pthread_free(pthread_t th)
{
  size_t sseg;
  /* If initial thread, nothing to free */
  if (th == &pthread_initial_thread) return;
  /* Free the stack and thread descriptor area */
  ASSERT(th->p_exited);
  sseg = SEG_THREAD(th);
  munmap((caddr_t) ((char *)(th+1) - STACK_SIZE), STACK_SIZE);
  stack_segments[sseg] = 0;
}

/* Handle threads that have exited */

static void pthread_exited(pid_t pid)
{
  pthread_t th;
  int detached;
  /* Find thread with that pid */
  for (th = pthread_main_thread->p_nextlive;
       th != pthread_main_thread;
       th = th->p_nextlive) {
    if (th->p_pid == pid) {
      /* Remove thread from list of active threads */
      th->p_nextlive->p_prevlive = th->p_prevlive;
      th->p_prevlive->p_nextlive = th->p_nextlive;
      /* Mark thread as exited, and if detached, free its resources */
      acquire(&th->p_spinlock);
      th->p_exited = 1;
      detached = th->p_detached;
      release(&th->p_spinlock);
      if (detached) pthread_free(th);
      break;
    }
  }
  /* If all threads have exited and the main thread is pending on a
     pthread_exit, wake up the main thread and terminate ourselves. */
  if (main_thread_exiting &&
      pthread_main_thread->p_nextlive == pthread_main_thread) {
    restart(pthread_main_thread);
    _exit(0);
  }
}

static void pthread_reap_children()
{
  pid_t pid;
  int status;

  while ((pid = waitpid(-1, &status, WNOHANG | __WCLONE)) > 0) {
    pthread_exited(pid);
    if (WIFSIGNALED(status)) {
      /* If a thread died due to a signal, send the same signal to
         all other threads, including the main thread. */
      pthread_kill_all_threads(WTERMSIG(status), 1);
      _exit(0);
    }
  }
}

/* Free the resources of a thread */

static void pthread_handle_free(pthread_t th)
{
  acquire(&th->p_spinlock);
  if (th->p_exited) {
    pthread_free(th);
  } else {
    /* The Unix process of the thread is still running.
       Mark the thread as detached so that the thread manager will
       deallocate its resources when the Unix process exits. */
    th->p_detached = 1;
    release(&th->p_spinlock);
  }
}

/* Send a signal to all running threads */

static void pthread_kill_all_threads(int sig, int main_thread_also)
{
  pthread_t th;
  for (th = pthread_main_thread->p_nextlive;
       th != pthread_main_thread;
       th = th->p_nextlive) {
    kill(th->p_pid, sig);
  }
  if (main_thread_also) {
    kill(pthread_main_thread->p_pid, sig);
  }
}

/* Process-wide exit() */

static void pthread_handle_exit(pthread_t issuing_thread, int exitcode)
{
  pthread_t th;
  pthread_exit_requested = 1;
  pthread_exit_code = exitcode;
  /* Send the CANCEL signal to all running threads, including the main
     thread, but excluding the thread from which the exit request originated
     (that thread must complete the exit, e.g. calling atexit functions
     and flushing stdio buffers). */
  for (th = issuing_thread->p_nextlive;
       th != issuing_thread;
       th = th->p_nextlive) {
    kill(th->p_pid, PTHREAD_SIG_CANCEL);
  }
  restart(issuing_thread);
  _exit(0);
}

/* Handler for PTHREAD_SIG_RESTART in thread manager thread */

void pthread_manager_sighandler(int sig)
{
  terminated_children = 1;
}

