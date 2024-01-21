/* $Id */

#ifndef _INCLUDE_THREADED_MAINLOOP_H
#define _INCLUDE_THREADED_MAINLOOP_H

#include <iluxport.h>
#include <iluconf.h>

/* provided by "threaded_mainloop.c" */
void initialize_threaded_mainloop(void);
#ifdef C_THREADS_SUPPORT
void initialize_threaded_C_runtime(void);
#endif
#ifdef CPP_THREADS_SUPPORT
void initialize_threaded_CPP_runtime(void);
#endif

/* provided by "threaded_ml.c" */
extern ilu_MainLoop threaded_ml;
void threaded_ml_fork(void (*proc)(void *arg), void *arg);

/* provided by "threaded_lt.c" */
extern ilu_LockTech threaded_lt;

/* provided by "threaded_wt.c" */
extern ilu_WaitTech threaded_wt;

#define LOOPLIST { SOLARIS_MAINLOOP, PTHREAD_MAINLOOP }

#if !defined(SOLARIS_MAINLOOP) && !defined(PTHREAD_MAINLOOP)
#error One of { SOLARIS_MAINLOOP, PTHREAD_MAINLOOP } should be defined!
#endif

#ifdef SOLARIS_MAINLOOP
#include <synch.h>
#include <thread.h>

#define DECLARE_MUTEX(m) mutex_t m
#define DECLARE_CONDITION(c) cond_t c
#define DECLARE_THREAD(t) thread_t t

#define INIT_MUTEX(m) (mutex_init(&m, USYNC_THREAD, NULL) == 0)
#define DESTROY_MUTEX(m) (mutex_destroy(&m))
#define LOCK_MUTEX(m) (mutex_lock(&m))
#define UNLOCK_MUTEX(m) (mutex_unlock(&m))

#define CURRENT_THREAD thread_t current_thread = thr_self()
#define GET_CURRENT_THREAD() thr_self()
#define SAME_THREAD(x, y) (x == y)

#define INIT_CONDITION(c) (cond_init(&c, USYNC_THREAD, 0) == 0)
#define DESTROY_CONDITION(c) (cond_destroy(&c))
#define CONDITION_BROADCAST(c) (cond_broadcast(&c))
#define CONDITION_WAIT(c, m) (cond_wait(&c, &m))
#define CONDITION_TIMEDWAIT(c, m, t) (cond_timedwait(&c, &m, &t))

#define DISPATCH_THREAD(function, args, id) (thr_create(NULL, 0, function, args, 0, &id) == 0)

#if defined(PTHREAD_MAINLOOP)
#error Only one of { PTHREAD_MAINLOOP, SOLARIS_MAINLOOP } should be defined.
#endif  /* PTHREAD_MAINLOOP */

#endif /* SOLARIS_MAINLOOP */

#ifdef PTHREAD_MAINLOOP
#include <pthread.h>

#define DECLARE_MUTEX(m) pthread_mutex_t m
#define DECLARE_CONDITION(c) pthread_cond_t c
#define DECLARE_THREAD(t) pthread_t t

#define INIT_MUTEX(m) (pthread_mutex_init(&m, pthread_mutexattr_default) == 0)
#define DESTROY_MUTEX(m) (pthread_mutex_destroy(&m))
#define LOCK_MUTEX(m) (pthread_mutex_lock(&m))
#define UNLOCK_MUTEX(m) (pthread_mutex_unlock(&m))

#define CURRENT_THREAD pthread_t current_thread = pthread_self()
#define GET_CURRENT_THREAD() (pthread_self())
#define SAME_THREAD(x, y) ((x.field1 == y.field1) && (x.field2 == y.field2))

#define INIT_CONDITION(c) (pthread_cond_init(&c, pthread_condattr_default) == 0)
#define DESTROY_CONDITION(c) (pthread_cond_destroy(&c))
#define CONDITION_BROADCAST(c) (pthread_cond_broadcast(&c))
#define CONDITION_WAIT(c, m) (pthread_cond_wait(&c, &m))
#define CONDITION_TIMEDWAIT(c, m, t) (pthread_cond_timedwait(&c, &m, &t))

#define DISPATCH_THREAD(function, args, id) (pthread_create(&id, pthread_attr_default, function, args) == 0)

#if defined(SOLARIS_MAINLOOP)
#error Only one of { PTHREAD_MAINLOOP, SOLARIS_MAINLOOP } should be defined.
#endif /* SOLARIS_MAINLOOP */

#endif /* PTHREAD_MAINLOOP */

#if !defined(ENABLE_DEBUGGING) || !defined(SET_THREAD_SPECIFIC)
#undef SET_THREAD_SPECIFIC
#undef GET_THREAD_SPECIFIC
#undef UNSET_THREAD_SPECIFIC
#define SET_THREAD_SPECIFIC(d1, d2) 
#define GET_THREAD_SPECIFIC(d1, d2) *d1 = "unavailable"; *d2 = ""
#define UNSET_THREAD_SPECIFIC()
#endif /* ENABLE_DEBUGGING */

#endif /* _INCLUDE_THREADED_MAINLOOP_H */



