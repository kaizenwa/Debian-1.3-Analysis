/* Copyright (C) 1992, the Florida State University
   Distributed by the Florida State University under the terms of the
   GNU Library General Public License.

This file is part of Pthreads.

Pthreads is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation (version 2).

Pthreads is distributed "AS IS" in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with Pthreads; see the file COPYING.  If not, write
to the Free Software Foundation, 675 Mass Ave, Cambridge,
MA 02139, USA.

Report problems and direct all questions to:

  pthreads-bugs@ada.cs.fsu.edu

  @(#)pthread.h	2.5 4/12/95

*/

#ifndef _pthread_pthread_h
#define _pthread_pthread_h

/*
 * Pthreads interface definition
 */

#ifndef	_POSIX_THREADS

#include <pthread/config.h>
#include <pthread/unistd.h>
#include <pthread/signal.h>
#include <stdio.h>
#include <pthread/limits.h>
#include <sys/time.h>            /* needed for struct timeval */
#include <pthread/errno.h>
#include <pthread/pthread_asm.h>
#include <setjmp.h>
#ifdef IO_NP
#include <sys/asynch.h>
#include <sys/types.h>
#endif IO_NP
#ifdef MALLOC_NP
#include <malloc.h>
#endif MALLOC_NP

/* contentionscope attribute values */
#define PTHREAD_SCOPE_GLOBAL 0
#define PTHREAD_SCOPE_LOCAL  1

/* delivery modes */
#define PTHREAD_PROCSYNC     0
#define PTHREAD_PROCASYNC    1

/* inheritsched attribute values */
#define PTHREAD_INHERIT_SCHED 0
#define PTHREAD_DEFAULT_SCHED 1

/* Allow variable stack sizes */
#ifndef _POSIX_THREAD_ATTR_STACKSIZE
#define _POSIX_THREAD_ATTR_STACKSIZE
#endif

#if defined (__GNUC__) || defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus)

#ifndef	_C_PROTOTYPE
#define	_C_PROTOTYPE(arglist)	arglist
#endif !_C_PROTOTYPE

typedef void *any_t;

#else !(defined (__GNUC__) || defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus))

#ifndef	_C_PROTOTYPE
#define	_C_PROTOTYPE(arglist)	()
#endif	not _C_PROTOTYPE

typedef char *any_t;

#ifndef const
#define const
#endif

#ifndef volatile
#define volatile
#endif

#endif defined(__GNUC__) || defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus)

#ifdef STAND_ALONE_NP
typedef char *malloc_t;
extern malloc_t malloc _C_PROTOTYPE((unsigned __size));
#endif STAND_ALONE_NP

#ifndef	TRUE
#define	TRUE	1
#define	FALSE	0
#endif

#if !defined(__sys_stdtypes_h) && !defined(_SIZE_T)
typedef	unsigned int	size_t;
#endif

/*
 * Mutex objects.
 */

typedef int pthread_protocol_t;

typedef struct pthread_queue {
          struct pthread *head;
          struct pthread *tail;
} *pthread_queue_t;

typedef struct mutex {
	struct pthread_queue queue;
	char lock;
        struct pthread *owner;
        int flags;
#ifdef _POSIX_THREADS_PRIO_PROTECT
        int prio_ceiling;
        pthread_protocol_t protocol;
	int prev_max_ceiling_prio;
#endif
} pthread_mutex_t;

typedef struct pthread_mutexattr {
        int flags;
#ifdef _POSIX_THREADS_PRIO_PROTECT
        int prio_ceiling;
        pthread_protocol_t protocol;
#endif
} pthread_mutexattr_t;

/******************************/
/* Mutex Attributes Functions */
/*       Mutex Functions      */
/******************************/

#ifdef __cplusplus
extern "C" {
#endif

extern int pthread_mutex_lock     _C_PROTOTYPE((pthread_mutex_t *__mutex));
extern int pthread_mutex_trylock  _C_PROTOTYPE((pthread_mutex_t *__mutex));
extern int pthread_mutex_unlock   _C_PROTOTYPE((pthread_mutex_t *__mutex));
extern int pthread_mutex_init     _C_PROTOTYPE((pthread_mutex_t *__mutex,
						pthread_mutexattr_t *__attr));
extern int pthread_mutex_destroy  _C_PROTOTYPE((pthread_mutex_t *__mutex));
extern int pthread_mutex_setprio_ceiling
                                  _C_PROTOTYPE((pthread_mutex_t *__mutex,
						int __prio_ceiling));
extern int pthread_mutex_getprio_ceiling
                                  _C_PROTOTYPE((pthread_mutex_t __mutex));
extern int pthread_mutexattr_init _C_PROTOTYPE((pthread_mutexattr_t *__attr));
extern int pthread_mutexattr_destroy
                                  _C_PROTOTYPE((pthread_mutexattr_t *__attr));
extern int pthread_mutexattr_setprotocol
                                  _C_PROTOTYPE((pthread_mutexattr_t *__attr,
						pthread_protocol_t __protocol));
extern pthread_protocol_t pthread_mutexattr_getprotocol
				  _C_PROTOTYPE((pthread_mutexattr_t __attr));

extern int pthread_mutexattr_setprio_ceiling
                                  _C_PROTOTYPE((pthread_mutexattr_t *__attr,
						int __prio_ceiling));
extern int pthread_mutexattr_getprio_ceiling
                                  _C_PROTOTYPE((pthread_mutexattr_t __attr));
extern int pthread_mutexattr_getpshared
                                  _C_PROTOTYPE((pthread_mutexattr_t *__attr,
						int *__pshared));
extern int pthread_mutexattr_setpshared
                                  _C_PROTOTYPE((pthread_mutexattr_t *__attr,
						int __pshared));

#ifdef __cplusplus
} /* extern "C" */
#endif

/*
 * Once Objects.
 */

typedef	struct {
	short init;
	short exec;
	pthread_mutex_t mutex;
} pthread_once_t;

#define PTHREAD_ONCE_INIT	{FALSE,FALSE,0, };

/*
 * Condition variables.
 */
typedef struct condition {
	struct pthread_queue queue;
        int flags;
        int waiters;
        pthread_mutex_t *mutex;
} pthread_cond_t;

typedef struct condattr {
        int flags;
} pthread_condattr_t;

/*******************************/
/*      Condition Functions    */
/*Condition Attribute Functions*/
/*******************************/

#ifdef __cplusplus
extern "C" {
#endif
 
extern int pthread_cond_destroy     _C_PROTOTYPE((pthread_cond_t *__cond));
extern int pthread_cond_init        _C_PROTOTYPE((pthread_cond_t *__cond,
						  pthread_condattr_t *__attr));
extern int pthread_condattr_init    _C_PROTOTYPE((pthread_condattr_t *__attr));
extern int pthread_condattr_destroy _C_PROTOTYPE((pthread_condattr_t *__attr));
extern int pthread_cond_wait        _C_PROTOTYPE((pthread_cond_t *__cond,
						  pthread_mutex_t *__mutex));
extern int pthread_cond_timedwait   _C_PROTOTYPE((pthread_cond_t *__cond,
						  pthread_mutex_t *__mutex,
						  struct timespec *__timeout));
extern int pthread_cond_signal      _C_PROTOTYPE((pthread_cond_t *__cond));
extern int pthread_cond_broadcast   _C_PROTOTYPE((pthread_cond_t *__cond));
extern int pthread_condattr_getpshared
                                    _C_PROTOTYPE((pthread_condattr_t *__attr,
						  int *__pshared));
extern int pthread_condattr_setpshared
                                    _C_PROTOTYPE((pthread_condattr_t *__attr,
						  int __pshared));

#ifdef __cplusplus
} /* extern "C" */
#endif

/*
 * Threads.
 */

typedef any_t (*pthread_func_t) _C_PROTOTYPE((any_t __arg));

typedef struct pthread_attr {
  int flags;
#ifdef _POSIX_THREAD_ATTR_STACKSIZE
  int stacksize;
#endif
  int contentionscope;
  int inheritsched;
  int detachstate;
  int sched;
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
  int prio;                      /* (Active) priority of Thread        */
#endif
  struct timespec starttime, deadline, period;
} pthread_attr_t;

/*
 * Queue indices
 */
#define PRIMARY_QUEUE 0
#define ALL_QUEUE     1
/* this slot may be used by the TIMER_QUEUE with index 2 */
#define NUM_QUEUES    3

typedef int pthread_key_t;

typedef struct cleanup *cleanup_t;

typedef struct pthread {
	sigjmp_buf context;                /* save area for context switch   */
	sigjmp_buf body;                   /* save area for pthread_body     */
	volatile int errno;                /* thread-specific errno          */
	volatile int ret;                  /* return value (EINTR --> -1)    */
	char *stack_base;                  /* bottom of run-time stack       */
	int state;                         /* thread state, -> pthread_asm.h */
        struct pthread *next[NUM_QUEUES];  /* links for queues               */
	int num_timers;			   /* number of timers for thread    */
	struct timeval interval;	   /* time left for SCHED_RR 	     */
        struct p_siginfo sig_info[NNSIG];  /* info for user handlers         */
	int sig;                           /* latest received signal         */
	int code;                          /* latest received signal code    */
	int osp, opc;                      /* save area for old context sw   */
        struct context_t *nscp;            /* UNIX signal context (new sig)  */
        struct context_t *scp;             /* UNIX signal context (current)  */
        struct pthread_queue joinq;        /* queue to await termination     */
        pthread_cond_t *cond;              /* cond var. if in cond_wait      */
        pthread_queue_t queue;/* primary queue thread is contained in        */
        sigset_t mask;        /* set of signals thread has masked out        */
        sigset_t pending;     /* set of signals pending on thread            */
        sigset_t sigwaitset;  /* set of signals thread is waiting for        */
	pthread_func_t func;  /* actual function to call upon activation     */
	any_t arg;            /* argument list to above function             */
        any_t result;         /* return value of above function              */
	any_t key[_POSIX_DATAKEYS_MAX];    /* thread specific data           */
        cleanup_t cleanup_top;             /* stack of cleanup handlers      */
        struct pthread_attr attr;          /* attributes                     */
        int base_prio;                     /* Base priority of thread        */
	int max_ceiling_prio; /* Max of ceiling prio among locked mutexes    */
        int new_prio;         /* New Priority                                */
#ifdef IO_NP
	volatile struct aio_result_t resultp;/*result of asynchronous I/O ops*/
        int wait_on_select;                /* more information in fds        */
	int width;                         /* pertinent file desc. set width */
	int how_many;                      /* how many amoung  0 .. width -1 */
	fd_set readfds;                    /* read file descriptor set       */
	fd_set writefds;                   /* write file descriptor set      */
	fd_set exceptfds;                  /* except. file descriptor set    */
#endif !IO_NP
#ifdef STAND_ALONE_NP
	pthread_func_t timer_func;	  /* function to be called on timeout*/
	any_t timer_arg;                  /* arg of above function	     */
        struct timespec tp;               /* wake-up time                    */
	int dummy;			  /* filler for stack alignment      */
#else !STAND_ALONE_NP
        struct timeval tp;                /* wake-up time                    */
#endif !STAND_ALONE_NP
} *pthread_t;


#if defined(__GNUC__) && defined(_PARAMS)
#define IO_SIZE_T __SIZE_TYPE__
/*
 * for gcc 2.5.8 or before use:
 * #define IO_SIZE_T unsigned int
 */
#define SIGSUSPEND_CONST
#else
#define IO_SIZE_T size_t
#define SIGSUSPEND_CONST const
#endif

/******************************/
/*      Thread Functions      */
/* Thread Attribute Functions */
/******************************/

#ifdef __cplusplus
extern "C" {
#endif
 
extern pthread_t pthread_self           _C_PROTOTYPE ((void));
extern void pthread_init                _C_PROTOTYPE ((void));
extern int pthread_create               _C_PROTOTYPE((pthread_t *__thread,
						      pthread_attr_t *__attr,
						      pthread_func_t __func,
						      any_t __arg));
extern int pthread_equal                _C_PROTOTYPE((pthread_t __t1,
						      pthread_t __t2));
extern int pthread_detach               _C_PROTOTYPE((pthread_t *__thread));
extern int pthread_join                 _C_PROTOTYPE((pthread_t __thread,
						      any_t *__status));
extern void pthread_yield               _C_PROTOTYPE((any_t __arg));
extern void pthread_exit                _C_PROTOTYPE((any_t __status));
extern int pthread_attr_init            _C_PROTOTYPE((pthread_attr_t *__attr));
extern int pthread_attr_destroy         _C_PROTOTYPE((pthread_attr_t *__attr));
extern int pthread_getschedattr         _C_PROTOTYPE((pthread_t __thread,
						      pthread_attr_t *__attrs));
extern int pthread_setschedattr         _C_PROTOTYPE((pthread_t __thread,
						      pthread_attr_t __attrs));
extern int pthread_attr_setstacksize    _C_PROTOTYPE((pthread_attr_t *__attr,
						      size_t __stacksize));
extern int pthread_attr_getstacksize    _C_PROTOTYPE((pthread_attr_t *__attr,
						      size_t *__stacksize));
extern int pthread_attr_setscope        _C_PROTOTYPE((pthread_attr_t *__attr,
						      int __contentionscope));
extern int pthread_attr_setinheritsched _C_PROTOTYPE((pthread_attr_t *__attr,
						      int __inherit));
extern int pthread_attr_setsched        _C_PROTOTYPE((pthread_attr_t *__attr,
						      int __scheduler));
extern int pthread_attr_setprio         _C_PROTOTYPE((pthread_attr_t *__attr,
						      int __priority));
extern int pthread_attr_getscope        _C_PROTOTYPE((pthread_attr_t __attr));
extern int pthread_attr_getinheritsched _C_PROTOTYPE((pthread_attr_t *__attr));
extern int pthread_attr_getsched        _C_PROTOTYPE((pthread_attr_t *__attr));
extern int pthread_attr_getprio         _C_PROTOTYPE((pthread_attr_t *__attr));

extern int pthread_attr_getstarttime_np _C_PROTOTYPE((pthread_attr_t *__attr,
						      struct timespec *__tp));
extern int pthread_attr_setstarttime_np _C_PROTOTYPE((pthread_attr_t *__attr,
						      struct timespec *__tp));
extern int pthread_attr_getdeadline_np  _C_PROTOTYPE((pthread_attr_t *__attr,
						      struct timespec *__tp));
extern int pthread_attr_setdeadline_np  _C_PROTOTYPE((pthread_attr_t *__attr,
						      struct timespec *__tp,
						      pthread_func_t func));
extern int pthread_attr_getperiod_np    _C_PROTOTYPE((pthread_attr_t *__attr,
						      struct timespec *__tp));
extern int pthread_attr_setperiod_np    _C_PROTOTYPE((pthread_attr_t *__attr,
						      struct timespec *__tp,
						      pthread_func_t func));

extern int pthread_key_create           _C_PROTOTYPE((pthread_key_t *__key,
						      void (*__func)
						      (any_t __value)));
extern int pthread_setspecific          _C_PROTOTYPE((pthread_key_t __key,
						      any_t __value));
extern int pthread_getspecific          _C_PROTOTYPE((pthread_key_t __key,
						      any_t *__value));
extern int pthread_cleanup_push         _C_PROTOTYPE((void (*__func)
						      (any_t __value),
						      any_t __arg));
extern int pthread_cleanup_pop          _C_PROTOTYPE((int __execute));
extern int sched_get_priority_max       _C_PROTOTYPE((int __policy));
extern int sched_get_priority_min       _C_PROTOTYPE((int __policy));
extern int pthread_attr_setdetachstate  _C_PROTOTYPE((pthread_attr_t *__attr,
						      int *__detachstate));
extern int pthread_attr_getdetachstate  _C_PROTOTYPE((pthread_attr_t *__attr));
extern int pthread_once                 _C_PROTOTYPE((pthread_once_t *__once_c,
						      void (*__func)
						      (any_t __value)));

/*
 * implementation-defined extensions
 */
extern int pthread_setprio_np		_C_PROTOTYPE((pthread_t __thread,
						      int __prio));
extern void pthread_setsigcontext_np    _C_PROTOTYPE((struct context_t *__scp,
						      jmp_buf __env,
						      int __val));
extern int pthread_lock_stack_np	_C_PROTOTYPE((pthread_t __p));


/******************************/
/*      Signal Functions      */
/******************************/

extern int sigwait             _C_PROTOTYPE((sigset_t *__set));
extern int sigprocmask         _C_PROTOTYPE((int __how,
					     const sigset_t *__set,
					     sigset_t *__oset));
extern int sigpending          _C_PROTOTYPE((sigset_t *__set));
extern int sigsuspend          _C_PROTOTYPE((SIGSUSPEND_CONST sigset_t *__set));
extern int pause               _C_PROTOTYPE((void));
extern int raise               _C_PROTOTYPE((int __sig));
extern int pthread_kill        _C_PROTOTYPE((pthread_t __thread,
					     int __sig));
extern int pthread_cancel      _C_PROTOTYPE((pthread_t __thread));
extern int pthread_setintr     _C_PROTOTYPE((int __state));
extern int pthread_setintrtype _C_PROTOTYPE((int __type));
extern void pthread_testintr   _C_PROTOTYPE((void));
extern int sigaction           _C_PROTOTYPE((int __sig,
					     const struct sigaction *__act,
					     struct sigaction *__oact));
/* yet to come...
extern unsigned int alarm      _C_PROTOTYPE((unsigned int __seconds));
*/
extern int nanosleep           _C_PROTOTYPE((const struct timespec *__rqtp,
					     struct timespec *__rmtp));
extern unsigned int sleep      _C_PROTOTYPE((unsigned int __seconds));
extern int clock_gettime       _C_PROTOTYPE((int __clock_id,
					     struct timespec *__tp));

/******************************/
/*    Low-Level Functions     */
/******************************/

extern int setjmp         _C_PROTOTYPE((jmp_buf __env));
extern void longjmp       _C_PROTOTYPE((jmp_buf __env,
					int __val));
extern int sigsetjmp      _C_PROTOTYPE((sigjmp_buf __env,
					int __savemask));
extern void siglongjmp    _C_PROTOTYPE((sigjmp_buf __env,
					int __val));

/******************************/
/*       I/O Functions        */
/******************************/

extern int read  _C_PROTOTYPE((int __fd,
			       void *__buf,
			       IO_SIZE_T __nbytes));
extern int write _C_PROTOTYPE((int __fd,
			       const void *__buf,
			       IO_SIZE_T __nbytes));

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif	_POSIX_THREADS

#endif /*!_pthread_pthread_h*/
