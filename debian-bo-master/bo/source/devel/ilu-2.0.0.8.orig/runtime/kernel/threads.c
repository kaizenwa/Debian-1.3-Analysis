/*
Copyright (c) 1991-1996 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: threads.c,v 1.22 1996/07/11 16:35:14 spreitze Exp $ */
/* Last edited by Mike Spreitzer July 11, 1996 9:17 am PDT */

/* This code was kindly donated by Digital Creations, Inc. */

/* Win32 support added - larner */


#include <iluntrnl.h>
#include <iludebug.h>

#include "threads.h"

#include <stdio.h>

#ifndef WIN32
#include <sys/time.h>
#include <sys/types.h>
#endif

#include "oscalls.h"
#include <errno.h>


#ifdef WIN32

#include <process.h>

#ifdef ENABLE_DEBUGGING
DWORD ShowLastError(char* pc_file, int i_line) {
	DWORD d_result = GetLastError();
	DEBUG(THREAD_DEBUG, (stderr, "GetLastError() = 0x%x in %s, line %d\n", d_result, pc_file, i_line));
	return d_result;
}
#endif
 

/* ----------------------------------------------------------------- */
/* Construct a condition out of NT thread synchronization primitives */
/* ----------------------------------------------------------------- */

#define BROADCAST_EVENT		0
#define NOTIFY_EVENT		1

typedef struct condition_struct Condition;

struct condition_struct {

	/* the number of threads waiting on the condition */
	long m_l_num_waiting_threads; 

    /* events that are signalled when the condition is 
	   Broadcast or notified.  After a broadcast, the last 
	   thread to wake up resets the manual reset event
	   m_h_occurred[BROADCAST_EVENT],  The auto reset
	   event m_h_occurred[NOTIFY_EVENT] is reset 
	   automatically after the pulse event used in notify. */
	HANDLE m_h_occurred[2];
};


/* ----------------------------------------------- */
/* initialize a condition - returns 0 on success, 
   else result of call to WIN32_GETLASTERROR()           */

DWORD condition_init (Condition* p_condition) {

	/* broadcast event is a manual reset event */
	p_condition->m_h_occurred[BROADCAST_EVENT] = CreateEvent(NULL, TRUE, FALSE, NULL);

	if (p_condition->m_h_occurred[BROADCAST_EVENT] == NULL)
		return WIN32_GETLASTERROR();

	/* notify event is a autoreset event */
	p_condition->m_h_occurred[NOTIFY_EVENT] = CreateEvent(NULL, FALSE, FALSE, NULL);

	if (p_condition->m_h_occurred[NOTIFY_EVENT] == NULL) {
		DWORD dw_retvalue = WIN32_GETLASTERROR();
		CloseHandle(p_condition->m_h_occurred[BROADCAST_EVENT]);
		return dw_retvalue;
	}

	p_condition->m_l_num_waiting_threads = 0;

	return 0;
}


/* ----------------------------------------------- */
/* destroy a condition - returns 0 on success, 
   else result of call to WIN32_GETLASTERROR()           */

DWORD condition_destroy (Condition* p_condition) {

	DWORD dw_retvalue = 0;

	if (CloseHandle(p_condition->m_h_occurred[BROADCAST_EVENT]) == FALSE)
		dw_retvalue = WIN32_GETLASTERROR();

	if (CloseHandle(p_condition->m_h_occurred[NOTIFY_EVENT]) == FALSE)
		return WIN32_GETLASTERROR();

	return dw_retvalue;
}


/* ----------------------------------------------- */
/* broadcast to all threads waiting on a condition 
   returns 0 on success, else result of call to 
   WIN32_GETLASTERROR()                                  */


DWORD condition_broadcast( Condition* p_condition ) {
	
	if ((p_condition->m_l_num_waiting_threads > 0) && 

		/* note that checking m_l_num_waiting_threads is safe here because
		all operations on it are assumed to occur under the protection of
		the mutex being used with the condition */

		(SetEvent( p_condition->m_h_occurred[BROADCAST_EVENT] ) == FALSE))
			return WIN32_GETLASTERROR();

	return 0;
}


/* ----------------------------------------------- */
/* signal to a threads waiting on a condition 
   returns 0 on success, else result of call to 
   WIN32_GETLASTERROR()                                  */


DWORD condition_signal( Condition* p_condition ) {
	
	if ((p_condition->m_l_num_waiting_threads > 0) && 

		/* note that checking m_l_num_waiting_threads is safe here because
		all operations on it are assumed to occur under the protection of
		the mutex being used with the condition */

		(SetEvent( p_condition->m_h_occurred[NOTIFY_EVENT] ) == FALSE))
			return WIN32_GETLASTERROR();

	return 0;
}


/* ----------------------------------------------- */
/* wait on a condition - returns 0 on success, 
   else result of call to WIN32_GETLASTERROR()           */

DWORD condition_wait( Condition* p_condition, HANDLE* p_h_mutex, DWORD dw_milliseconds  ) {
  
  DWORD dw_result = 0;

	/* Add this thread to the number of waiting threads. Note this is under the 
	protection of the p_h_mutex mutex.*/
	p_condition->m_l_num_waiting_threads++;

	/* Release the mutex. */
	if (ReleaseMutex( *p_h_mutex ) == FALSE) {
			p_condition->m_l_num_waiting_threads--;
			return WIN32_GETLASTERROR();
	}

	/* here is the area of vulnerability [between ReleaseMutex( *p_h_mutex ) and 
	   WaitForMultipleObjects( ...)] we are avoiding by using the events */

	/* Wait for condition_broadcast to signal the condition. */
	dw_result = WaitForMultipleObjects(2, p_condition->m_h_occurred, FALSE, dw_milliseconds );
	if (dw_result == WAIT_FAILED) {
		dw_result = WIN32_GETLASTERROR();
		WaitForSingleObject( *p_h_mutex, INFINITE ); /* always reaquire mutex */
		p_condition->m_l_num_waiting_threads--;
		return dw_result;
	}

	/* Reaquire the mutex. */
	WaitForSingleObject( *p_h_mutex, INFINITE );

	/* If this is the last thread to wake up from the broadcast, reset "occurred" */

	p_condition->m_l_num_waiting_threads--;

	if ((p_condition->m_l_num_waiting_threads <= 0) &&
		(dw_result != WAIT_TIMEOUT) &&
		(dw_result - WAIT_OBJECT_0 == BROADCAST_EVENT) &&
		(ResetEvent( p_condition->m_h_occurred[BROADCAST_EVENT] ) == FALSE))
		  return WIN32_GETLASTERROR();

	if (dw_result != WAIT_TIMEOUT)
		dw_result = 0;

	return dw_result;
}



/* ----------------------------------------------------------------- */

#endif /* WIN32 */


/* ----------------------------------------------- */
/* defines                                         */

#define PN(x) (x != NIL ? x : "(null)")

#define CAST_MUTEX(as_any, as_mutex) ilukt_Mutex *as_mutex = (ilukt_Mutex *)as_any

#define CAST_CONDITION(as_any, as_cond) ilukt_Condition *as_cond = (ilukt_Condition *)as_any

/* should more checking be done to ensure m is valid (e.g. examining the
   structure of m->mutex to make sure it looks like a viable mutex)? */
#define ASSERT_VALID_MUTEX(m, e) if (m == NIL) { ILU_ERR_CONS1(bad_param, e, minor, ilu_bpm_nil, 0); return; }

#define ASSERT_VALID_CONDITION(c, e) ASSERT_VALID_MUTEX(c, e)

#define ASSERT_HOLD_MUTEX(m, e) if (!(m->locked && (SAME_THREAD(m->owner_thread, current_thread)))) { ILU_ERR_CONS0(bad_locks, e, 0); return; }



/* ----------------------------------------------- */
/* thread value printing                           */

static char *no_mem_msg = "(insufficient memory to format thread)";
static ilu_string no_ftt = "(unformatted thread due to timing splinter)";

static void FmtBytes(char *dest, unsigned char *src, int len)
{
  static char     hexes[16] = "0123456789abcdef";
  int             i, di;
#ifdef WORDS_BIGENDIAN
  di = 1;
#else
  di = -1;
  src += len - 1;
#endif
  for (i = 0; i < len; i++) {
    unsigned char   c = *src;
    *(dest++) = hexes[c / 16];
    *(dest++) = hexes[c & 15];
    src += di;
  }
  *dest = 0;
  return;
}

static ilu_string FmtThread(DECLARE_THREAD(t))
{
#ifdef DEBUG_THREAD_REFERENT
  int             sz = 2 * (sizeof(t) + sizeof(*t) + 1);
#else
  int             sz = 2 * sizeof(t) + 1;
#endif
  char           *s = (char *) ilu_malloc(sz);
  unsigned char  *b = (unsigned char *) &t;
  if (s == NIL)
    return no_mem_msg;
  FmtBytes(s, b, sizeof(t));
#ifdef DEBUG_THREAD_REFERENT
  s[2 * sizeof(t)] = ':';
  FmtBytes(s + 2 * sizeof(t) + 1, (unsigned char *) t, sizeof(*t));
#endif
  return s;
}

static void FreeThreadFmt(ilu_string s)
{
  if (s != no_mem_msg && s != no_ftt)
    ilu_free(s);
}

#if 0

#define ILU_LOCK_MUTEX(m) Lock_Mutex_Work(&(m))
#define ILU_UNLOCK_MUTEX(m) Unlock_Mutex_Work(&(m))

static void Lock_Mutex_Work(DECLARE_MUTEX(*pm))
{
  char            fm[sizeof(*pm) * 2 + 1];
  FmtBytes(fm, (unsigned char *) pm, sizeof(*pm));
  DEBUG(LOCK_DEBUG, (stderr, "Lock_Mutex(*%p = {%s}) entered.\n",
		     pm, fm));
  LOCK_MUTEX(*pm);
  FmtBytes(fm, (unsigned char *) pm, sizeof(*pm));
  DEBUG(LOCK_DEBUG, (stderr, "Lock_Mutex(*%p = {%s}) finished.\n",
		     pm, fm));
}

static void Unlock_Mutex_Work(DECLARE_MUTEX(*pm))
{
  char            fm[sizeof(*pm) * 2 + 1];
  UNLOCK_MUTEX(*pm);
  FmtBytes(fm, (unsigned char *) pm, sizeof(*pm));
  DEBUG(LOCK_DEBUG, (stderr, "Unlock_Mutex(*%p = {%s}) finished.\n",
		     pm, fm));
}

#else

#define ILU_LOCK_MUTEX(m) LOCK_MUTEX(m)
#define ILU_UNLOCK_MUTEX(m) UNLOCK_MUTEX(m)

#endif


/* ----------------------------------------------- */
/* mutex structure                                 */

typedef struct
{
  DECLARE_MUTEX(mutex);
  DECLARE_THREAD(owner_thread);
  ilu_string d1, d2;
  ilu_boolean locked; /* Is it guaranteed that a thread ID will never be 0 
			 (or some other value)?  If so, we could elminiate
			 this locked field and just set owner_thread to some
			 impossible value when the mutex is unlocked.  Too bad 
			 there doesn't seem to be a user-level equivalent of 
			 the "mutex_owned" kernel operation.  Any 
			 suggestions?!? */
} ilukt_Mutex;


/* ----------------------------------------------- */
/* Condition structure                             */

typedef struct
{
  DECLARE_CONDITION(condition);
  ilu_string d1, d2;
} ilukt_Condition;


/* ----------------------------------------------- */
/* create a mutex                                  */

static ilu_Mutex
  ilukt_LT_mcreate(ilu_string d1, ilu_string d2)
{
  ilukt_Mutex *new_mutex = (ilukt_Mutex *)ilu_malloc(sizeof(ilukt_Mutex));

  DEBUG(LOCK_DEBUG, (stderr, "ilukt_LT_mcreate('%s','%s')\n", d1, d2));

  if (!new_mutex)
    /* couldn't allocate space, so fail out */
    {
      return NIL;
    }

  if (!INIT_MUTEX(new_mutex->mutex))
    /* mutex initialization failed for some reason, so clean up and fail out */
    {
      ilu_free(new_mutex);
      return NIL;
    }

  if (d1 != NIL)
    {
      new_mutex->d1 = _ilu_Strdup(d1);
      if (new_mutex->d1 == NIL)
	{
	  DESTROY_MUTEX(new_mutex->mutex);
	  ilu_free(new_mutex);
	  return NIL;
	}
    }
  else new_mutex->d1 = NIL;

  if (d2 != NIL)
    {
      new_mutex->d2 = _ilu_Strdup(d2);
      if (new_mutex->d2 == NIL)
	{
	  DESTROY_MUTEX(new_mutex->mutex);
	  if (new_mutex->d1 != NIL)
	    {
	      ilu_free(new_mutex->d1);
	    }
	  ilu_free(new_mutex);
	  return NIL;
	}
    }
  else new_mutex->d2 = NIL;

  new_mutex->locked = ilu_FALSE;

  DEBUG(LOCK_DEBUG, (stderr, "ilukt_LT_mcreate succeeded: %p\n", new_mutex));

  return (ilu_Mutex) new_mutex;
}


/* ----------------------------------------------- */
/* Reveals strings given to create                 */

static void 
  ilukt_LT_muncons(ilu_Mutex m, ilu_string *d1, ilu_string *d2, ILU_ERRS((bad_param)) *err)
{
  CAST_MUTEX(m, mutex_obj);

  ASSERT_VALID_MUTEX(mutex_obj, err);

  if (d1 == NIL || d2 == NIL)
    {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, 0);
      return;
    }

  *d1 = mutex_obj->d1;
  *d2 = mutex_obj->d2;

  ILU_CLER(*err);

  return;
}

/* ----------------------------------------------- */
/* Acquire mutex                                   */

static void 
  ilukt_LT_acquire(ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) *err)
{
  CAST_MUTEX(m, mutex_obj);
  DECLARE_THREAD(current_thread);
  current_thread = GET_CURRENT_THREAD();

#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & LOCK_DEBUG) {
    char           *ftt = FmtThread(GET_CURRENT_THREAD());
    DEBUG(LOCK_DEBUG,
	  (stderr, "(thread %s) waiting for (mutex [%s%s])\n",
	   ftt, mutex_obj->d1, mutex_obj->d2));
    FreeThreadFmt(ftt);
  }
#endif /* ENABLE_DEBUGGING */

  ASSERT_VALID_MUTEX(mutex_obj, err);

  if (mutex_obj->locked &&
      (SAME_THREAD(mutex_obj->owner_thread, current_thread))) {
    ILU_ERR_CONS0(bad_locks, err, 0);
    return;
  }

  ILU_LOCK_MUTEX(mutex_obj->mutex);

#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & LOCK_DEBUG) {
    char           *ftt = FmtThread(GET_CURRENT_THREAD());
    DEBUG(LOCK_DEBUG,
	  (stderr, "(thread %s) acquired (mutex [%s%s])\n",
	   ftt, mutex_obj->d1, mutex_obj->d2));
    FreeThreadFmt(ftt);
  }
#endif /* ENABLE_DEBUGGING */

  mutex_obj->owner_thread = current_thread;
  mutex_obj->locked = ilu_TRUE;

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* Checks that the caller holds the given mutex    */

static void 
  ilukt_LT_hold(ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) *err)
{
  CAST_MUTEX(m, mutex_obj);
  DECLARE_THREAD(current_thread);
  current_thread = GET_CURRENT_THREAD();

  ASSERT_VALID_MUTEX(mutex_obj, err);

  ASSERT_HOLD_MUTEX(mutex_obj, err);

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* Releases the mutex                              */

static void
  ilukt_LT_release(ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) *err)
{
  CAST_MUTEX(m, mutex_obj);
  DECLARE_THREAD(current_thread);
  current_thread = GET_CURRENT_THREAD();

#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & LOCK_DEBUG) {
    char           *ftt = FmtThread(GET_CURRENT_THREAD());
    DEBUG(LOCK_DEBUG,
	  (stderr, "(thread %s) releasing (mutex [%s%s])\n",
	   ftt, mutex_obj->d1, mutex_obj->d2));
    FreeThreadFmt(ftt);
  }
#endif /* ENABLE_DEBUGGING */

  ASSERT_VALID_MUTEX(mutex_obj, err);

  ASSERT_HOLD_MUTEX(mutex_obj, err);

  mutex_obj->locked = ilu_FALSE;

  ILU_UNLOCK_MUTEX(mutex_obj->mutex);

  ILU_CLER(*err);

  return;
}



/* ----------------------------------------------- */
/* Creates a condition                             */

static ilu_Condition
  ilukt_LT_ccreate(ilu_string d1, ilu_string d2)
{
  ilukt_Condition *new_condition;
  new_condition = (ilukt_Condition *)ilu_malloc(sizeof(ilukt_Condition));

  DEBUG(LOCK_DEBUG, (stderr, "ilukt_LT_ccreate called\n"));

  if (new_condition == NIL)
    return NIL;

  if (!INIT_CONDITION(new_condition->condition))
    {
      ilu_free(new_condition);
      return NIL;
    }

  if (d1 != NIL)
    {
      new_condition->d1 = _ilu_Strdup(d1);
      if (new_condition->d1 == NIL)
	{
	  DESTROY_CONDITION(new_condition->condition);
	  ilu_free(new_condition);
	  return NIL;
	}
    }
  else new_condition->d1 = NIL;

  if (d2 != NIL)
    {
      new_condition->d2 = _ilu_Strdup(d2);
      if (new_condition->d2 == NIL)
	{
	  DESTROY_CONDITION(new_condition->condition);
	  if (new_condition->d1 != NIL)
	    {
	      ilu_free(new_condition->d1);
	    }
	  ilu_free(new_condition);
	  return NIL;
	}
    }
  else new_condition->d2 = NIL;

  DEBUG(LOCK_DEBUG, (stderr, "ilukt_LT_ccreate succeeded: %p\n", new_condition));

  return new_condition;
}


/* ----------------------------------------------- */
/* Reveals strings given to create                 */

static void
  ilukt_LT_cuncons(ilu_Condition c, ilu_string *d1, ilu_string *d2, ILU_ERRS((bad_param )) *err)
{
  CAST_CONDITION(c, condition_obj);

  ASSERT_VALID_CONDITION(condition_obj, err);

  if (d1 == NIL || d2 == NIL)
    {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, 0);
      return;
    }

  *d1 = condition_obj->d1;
  *d2 = condition_obj->d2;

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* Notifies the condition                          */

static void
  ilukt_LT_notify(ilu_Condition c, ILU_ERRS((bad_param)) *err)
{
  CAST_CONDITION(c, condition_obj);

#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & LOCK_DEBUG) {
    char           *ftt = FmtThread(GET_CURRENT_THREAD());
    DEBUG(LOCK_DEBUG,
	  (stderr, "(thread %s) notifying (condition [%s%s])\n",
	   ftt,
	   (condition_obj->d1 ? condition_obj->d1 : ""),
	   (condition_obj->d2 ? condition_obj->d2 : "")));
    FreeThreadFmt(ftt);
  }
#endif /* ENABLE_DEBUGGING */

  ASSERT_VALID_CONDITION(condition_obj, err);

  CONDITION_BROADCAST(condition_obj->condition);

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* Destroys the condition                          */

static void
  ilukt_LT_cdestroy(ilu_Condition c, ILU_ERRS((bad_param)) *err)
{
  CAST_CONDITION(c, condition_obj);

  DEBUG(LOCK_DEBUG,
	(stderr, "ilukt_LT_cdestroy called: (condition = %p)\n",
	 condition_obj));

  ASSERT_VALID_CONDITION(condition_obj, err);

  DESTROY_CONDITION(condition_obj->condition);

  if (condition_obj->d1 != NIL)
    {
      ilu_free(condition_obj->d1);
    }
  if (condition_obj->d2 != NIL)
    {
      ilu_free(condition_obj->d2);
    }

  ilu_free(condition_obj);

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* Waits on the condition                          */

static void
  ilukt_LT_wait(ilu_Condition c, ilu_Mutex m, ilu_Mutex m2, ILU_ERRS((bad_param, bad_locks)) *err)
{
  CAST_CONDITION(c, condition_obj);
  CAST_MUTEX(m, mutex_obj);
  DECLARE_THREAD(current_thread);
  ilukt_Mutex    *mutex_obj2 = (ilukt_Mutex *) m2;
  ilu_string      ftt = no_ftt;
  ilu_string      d1 = (condition_obj->d1 ? condition_obj->d1 : "");
  ilu_string      d2 = (condition_obj->d2 ? condition_obj->d2 : "");
  current_thread = GET_CURRENT_THREAD();

#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & LOCK_DEBUG)
      ftt = FmtThread(current_thread);
#endif /* ENABLE_DEBUGGING */

  ASSERT_VALID_CONDITION(condition_obj, err);
  ASSERT_VALID_MUTEX(mutex_obj, err);
  ASSERT_VALID_MUTEX(mutex_obj2, err);
    
  ASSERT_HOLD_MUTEX(mutex_obj, err);
  if (mutex_obj != mutex_obj2) {
    ASSERT_HOLD_MUTEX(mutex_obj2, err);

    DEBUG(LOCK_DEBUG,
	  (stderr,
    "(thread %s) unlocking 2nd mutex [%s%s] for condition [%s%s]\n",
	   ftt, mutex_obj2->d1, mutex_obj2->d2, d1, d2));

    ILU_UNLOCK_MUTEX(mutex_obj2->mutex);
  }
  
  DEBUG(LOCK_DEBUG,
	(stderr,
    "(thread %s) waiting on (condition [%s%s]) wrt mutex [%s%s]\n",
	 ftt, d1, d2, mutex_obj->d1, mutex_obj->d2));

  CONDITION_WAIT(condition_obj->condition, mutex_obj->mutex);
  
  if (mutex_obj == mutex_obj2) {
    mutex_obj->owner_thread = current_thread;
    mutex_obj->locked = ilu_TRUE;
    DEBUG(LOCK_DEBUG,
	  (stderr,
	   "(thread %s) resumed from wait on cond [%s%s] wrt mutex[%s%s]\n",
	   ftt, d1, d2, mutex_obj->d1, mutex_obj->d2));
  } else {
    ILU_UNLOCK_MUTEX(mutex_obj->mutex);
    DEBUG(LOCK_DEBUG,
	  (stderr,
	   "(thread %s) resuming from wait on cond [%s%s], relocking mutex[%s%s]\n",
	   ftt, d1, d2, mutex_obj2->d1, mutex_obj2->d2));
    ILU_LOCK_MUTEX(mutex_obj2->mutex);
    mutex_obj2->owner_thread = current_thread;
    mutex_obj2->locked = ilu_TRUE;
    DEBUG(LOCK_DEBUG,
	  (stderr,
	   "(thread %s) resuming from wait on cond [%s%s], relocking mutex[%s%s]\n",
	   ftt, d1, d2, mutex_obj->d1, mutex_obj->d2));
    ILU_LOCK_MUTEX(mutex_obj->mutex);
    mutex_obj->owner_thread = current_thread;
    mutex_obj->locked = ilu_TRUE;
    DEBUG(LOCK_DEBUG,
	  (stderr,
	   "(thread %s) resumed from wait on cond [%s%s] wrt mutexes [%s%s] and [%s%s]\n",
	   ftt, d1, d2, mutex_obj->d1, mutex_obj->d2,
	   mutex_obj2->d1, mutex_obj2->d2));
  }
  
  if (_ilu_DebugLevel & LOCK_DEBUG)
    FreeThreadFmt(ftt);

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* LockTech structure                              */

static ilu_LockTech
  _ilu_threaded_lt = {
    ilukt_LT_mcreate,
    ilukt_LT_muncons,
    ilukt_LT_acquire,
    ilukt_LT_hold,
    ilukt_LT_release,
    ilukt_LT_ccreate,
    ilukt_LT_cuncons,
    ilukt_LT_notify,
    ilukt_LT_cdestroy,
    ilukt_LT_wait };
			 
	

/* ----------------------------------------------- */
/* read wait                                       */

static void
  ilukt_WT_read_wait(int fd, ilu_boolean *sure, ilu_FineTime *limit,
                     ILU_ERRS((interrupted)) *err)
{
  fd_set          read_set, exn_set;
  int             stat;
  struct timeval  time, *time_p;
  ilu_string      ftt = no_ftt;
  
#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & LOCK_DEBUG)
    ftt = FmtThread(GET_CURRENT_THREAD());
#endif /* ENABLE_DEBUGGING */

  DEBUG(LOCK_DEBUG,
	(stderr, "ilukt_WT_read_wait(%d, thr=%s) entered.\n",
	 fd, ftt));

  FD_ZERO(&read_set);
  FD_SET(fd, &read_set);
  FD_ZERO(&exn_set);
  FD_SET(fd, &exn_set);

  if (limit != 0) {
    time.tv_sec = limit->ft_s;
    time.tv_usec = ilu_rescale(limit->ft_s, ilu_FineTimeRate, 1000000);
    time_p = &time;
  } else {
    time_p = NULL;
  }

  if ((stat = select(fd + 1, &read_set, NULL, &exn_set, time_p)) > 0) {
    *sure = ilu_TRUE;

    DEBUG(LOCK_DEBUG,
	  (stderr,
	"ilukt_WT_read_wait(%d, thr=%s) got input=%s && exn=%s.\n",
	   fd, ftt,
	   (FD_ISSET(fd, &read_set) ? "T" : "F"),
	   (FD_ISSET(fd, &exn_set) ? "T" : "F")));

    ILU_CLER(*err);

  } else if (stat == -1) {	/* an error occured; hopefully an
				 * interrupt! */
    int             theerr = sockerrno;
    switch (theerr) {
    case SOCKERRID(INTR):
      *sure = ilu_FALSE;
      DEBUG(LOCK_DEBUG,
	    (stderr,
	     "ilukt_WT_read_wait(%d, thr=%s) interrupted.\n",
	     fd, ftt));
      ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, 0);
      break;
#ifdef WIN32
    case SOCKERRID(NOTSOCK):
#else
    case SOCKERRID(BADF):
#endif
      /* FD closed in another thread */
      *sure = TRUE;
      DEBUG(LOCK_DEBUG,
	    (stderr,
	     "ilukt_WT_read_wait(%d, thr=%s) assuming FD closed.\n",
	     fd, ftt));
      ILU_CLER(*err);
      break;
    default:
      ASSERT(FALSE, buf,
	     (buf,
	  "ilukt_WT_read_wait(%d) got select failure, errno=%d=%s",
	      fd, errno, strerror(errno)));
    }

  } else {
    *sure = ilu_FALSE;

    DEBUG(LOCK_DEBUG,
	  (stderr,
	   "ilukt_WT_read_wait(%d, thr=%s) timed out.\n",
	   fd, ftt));

    ILU_CLER(*err);
  }

  if (_ilu_DebugLevel & LOCK_DEBUG)
    FreeThreadFmt(ftt);

  return;
}


/* ----------------------------------------------- */
/* write wait                                      */

static void
  ilukt_WT_write_wait(int fd, ilu_boolean *sure, ilu_FineTime *limit, ILU_ERRS((interrupted)) *err)
{
  fd_set          write_set, exn_set;
  int             stat;
  struct timeval  time, *time_p;
  ilu_string      ftt = no_ftt;
  
#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & LOCK_DEBUG)
    ftt = FmtThread(GET_CURRENT_THREAD());
#endif /* ENABLE_DEBUGGING */

  DEBUG(LOCK_DEBUG,
	(stderr, "ilukt_WT_write_wait(%d, thr=%s) entered.\n",
	 fd, ftt));

  FD_ZERO(&write_set);
  FD_SET(fd, &write_set);
  FD_ZERO(&exn_set);
  FD_SET(fd, &exn_set);

  if (limit != 0) {
    time.tv_sec = limit->ft_s;
    time.tv_usec = ilu_rescale(limit->ft_s, ilu_FineTimeRate, 1000000);
    time_p = &time;
  } else {
    time_p = NULL;
  }

  if ((stat = select(fd + 1, NULL, &write_set, &exn_set, time_p)) > 0) {
    *sure = ilu_TRUE;

    DEBUG(LOCK_DEBUG,
	  (stderr,
      "ilukt_WT_write_wait(%d, thr=%s) got output=%s && exn=%s.\n",
	   fd, ftt,
	   (FD_ISSET(fd, &write_set) ? "T" : "F"),
	   (FD_ISSET(fd, &exn_set) ? "T" : "F")
	   ));

    ILU_CLER(*err);
  } else if (stat == -1) {	/* an error occured; hopefully an
				 * interrupt! */
    int             theerr = errno;
    switch (theerr) {
    case SOCKERRID(INTR):
      *sure = ilu_FALSE;
      DEBUG(LOCK_DEBUG,
	    (stderr,
	     "ilukt_WT_write_wait(%d, thr=%s) interrupted.\n",
	     fd, ftt));
      ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, 0);
      break;
#ifdef WIN32
    case SOCKERRID(NOTSOCK):
#else
    case SOCKERRID(BADF):
#endif
      /* FD closed in another thread */
      *sure = TRUE;
      DEBUG(LOCK_DEBUG,
	    (stderr,
	   "ilukt_WT_write_wait(%d, thr=%s) assuming FD closed.\n",
	     fd, ftt));
      ILU_CLER(*err);
      break;
    default:
      ASSERT((errno == SOCKERRID(INTR)), buf,
	(buf, "ilukt_WT_write_wait(%d):select failed, errno=%d=%s",
	 fd, errno, strerror(errno)));
    }

  } else {
    *sure = ilu_FALSE;

    DEBUG(LOCK_DEBUG,
	  (stderr, "ilukt_WT_write_wait(%d, thr=%s) timed out.\n",
	   fd, ftt));

    ILU_CLER(*err);
  }

  if (_ilu_DebugLevel & LOCK_DEBUG)
    FreeThreadFmt(ftt);

  return;
}


/* ----------------------------------------------- */
/* WaitTech                                        */

static ilu_WaitTech
  _ilu_threaded_wt = {
    ilukt_WT_read_wait,
    ilukt_WT_write_wait };


/* ----------------------------------------------- */
/* Alarm struct                                    */

typedef struct
{
  DECLARE_MUTEX(alarm_mutex);
  DECLARE_CONDITION(alarm_condition);
  ilu_FineTime fire_time;
  void (*proc)(ilu_private rock);
  ilu_private rock;
} ilukt_Alarm;


/* ----------------------------------------------- */
/* this impedance-matching struct is needed because Solaris wants 
functions called by thr_create to have a return a void * */
typedef struct
{
  void (*proc)(void *arg);
  void *arg;
} function_and_argument;


/* ----------------------------------------------- */
/* function that basically runs the thread's procedure */

#ifndef WIN32
static void *
  run_function(void *func_and_args)
{
  function_and_argument *proc_args;
  ilu_string      ftt = no_ftt;
  proc_args = (function_and_argument *) func_and_args;

#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & THREAD_DEBUG)
    ftt = FmtThread(GET_CURRENT_THREAD());
#endif

  DEBUG(THREAD_DEBUG,
	(stderr,
	 "run_function(fn = %p, arg = %p, thr = %s) starting.\n",
	 proc_args->proc, proc_args->arg, ftt));

  proc_args->proc(proc_args->arg);

  DEBUG(THREAD_DEBUG,
	(stderr,
	 "run_function(fn = %p, arg = %p, thr = %s) finishing.\n",
	 proc_args->proc, proc_args->arg, ftt));

  ilu_free(proc_args);
  FreeThreadFmt(ftt);

  return NULL;
}
#else
static void 
  run_function(void *func_and_args)
{
  function_and_argument *proc_args;
  ilu_string      ftt = no_ftt;
  proc_args = (function_and_argument *)func_and_args;

#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & THREAD_DEBUG)
    ftt = FmtThread(GET_CURRENT_THREAD());
#endif

  DEBUG(THREAD_DEBUG,
	(stderr,
	 "run_function(fn = %p, arg = %p, thr = %s) starting.\n",
	 proc_args->proc, proc_args->arg, ftt));

  proc_args->proc(proc_args->arg);

  DEBUG(THREAD_DEBUG,
	(stderr,
	 "run_function(fn = %p, arg = %p, thr = %s) finishing.\n",
	 proc_args->proc, proc_args->arg, ftt));

  ilu_free(proc_args);
  FreeThreadFmt(ftt);
}
#endif


/* ----------------------------------------------- */
/* run an alarm                                    */

#ifndef WIN32
static void *
#else
static void
#endif
  ilukt_ML_run_alarm(void *alarm_in_disguise)
{
  ilukt_Alarm    *alarm = (ilukt_Alarm *) alarm_in_disguise;
  ilu_string      ftt = no_ftt;
#ifdef ILU_SOLARIS2_THREADS
  timestruc_t     set_time;
#endif
#ifdef ILU_POSIX_THREADS
  struct timespec set_time;
  int             additional_info;
#endif
  int             err;

#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & THREAD_DEBUG)
    ftt = FmtThread(GET_CURRENT_THREAD());
  DEBUG(THREAD_DEBUG,
	(stderr,
	 "ilukt_ML_run_alarm(%p): started on thread %s.\n",
	 alarm, ftt));
#endif

  ILU_LOCK_MUTEX(alarm->alarm_mutex);

  DEBUG(THREAD_DEBUG,
	(stderr,
	 "ilukt_ML_run_alarm(%p): mutex locked.\n",
	 alarm));

  while (1) {
    if (alarm->fire_time.ft_s != 0) {

#ifdef ILU_WIN32_THREADS
      ilu_FineTime    the_time_now, delta;
      static ilu_FineTime zeroFT = {0, 0};
      DWORD           relative_time_in_msec;
      the_time_now = ilu_FineTime_Now();
      delta = ilu_FineTime_Sub(alarm->fire_time, the_time_now);
      if (ilu_FineTime_Cmp(delta, zeroFT) > 0) {
	relative_time_in_msec = delta.ft_s * 1000
	  + ilu_rescale(delta.ft_t, ilu_FineTimeRate, 1000);

	DEBUG(THREAD_DEBUG,
	      (stderr,
	"ilukt_ML_run_alarm(%p): waiting for (%ld) milliseconds.\n",
	       alarm, relative_time_in_msec));

	err = CONDITION_TIMEDWAIT(alarm->alarm_condition,
				  alarm->alarm_mutex,
				  relative_time_in_msec);
      } else
	err = 0;

#else
      set_time.tv_sec = alarm->fire_time.ft_s;
      set_time.tv_nsec = ilu_rescale(alarm->fire_time.ft_t,
				     ilu_FineTimeRate, 1000000000);

      DEBUG(THREAD_DEBUG,
	    (stderr,
	     "ilukt_ML_run_alarm(%p): waiting for t=%ld.%09ld.\n",
	     alarm, set_time.tv_sec, set_time.tv_nsec));

      err = CONDITION_TIMEDWAIT(alarm->alarm_condition,
				alarm->alarm_mutex,
				set_time);

#endif


      if (
#ifdef ILU_SOLARIS2_THREADS
	  err == ETIME
#endif
#ifdef ILU_POSIX_THREADS
	  err == ETIME		/* on Solaris 2 */
	  || err == ETIMEDOUT	/* on Linux, AIX */
#endif
#ifdef ILU_WIN32_THREADS
	  err == WAIT_TIMEOUT
#endif
	) {
	/* Timeout.  Fire proc, with mutex unlocked.  */

	alarm->fire_time.ft_s = 0;
	ILU_UNLOCK_MUTEX(alarm->alarm_mutex);

	DEBUG(THREAD_DEBUG,
	      (stderr,
	       "ilukt_ML_run_alarm(%p): firing! (in thread %s)\n",
	       alarm, ftt));

	alarm->proc(alarm->rock);

	ILU_LOCK_MUTEX(alarm->alarm_mutex);

	DEBUG(THREAD_DEBUG,
	      (stderr,
	       "ilukt_ML_run_alarm(%p): mutex re-locked, by thread %s.\n",
	       alarm, ftt));

      } else {
	char            buf[64];
	if (err)
	  sprintf(buf, "wait returned %d in ilukt_ML_run_alarm", err);
	_ilu_Assert(!err, buf);
	/*
	 * Normal termination; this means that the condition
	 * variable was signalled (i.e. the alarm was reset), so
	 * we'll just loop back to the top and reset the time
	 */
	DEBUG(THREAD_DEBUG,
	      (stderr,
	       "ilukt_ML_run_alarm(%p): woken early; rethinking.\n",
	       alarm));
      }
    } else {
      /*
       * The alarm must've been unset, so wait until something else
       * happens.
       */
      DEBUG(THREAD_DEBUG,
	    (stderr,
	  "ilukt_ML_run_alarm(%p): waiting for alarm to be set.\n",
	     alarm));
      CONDITION_WAIT(alarm->alarm_condition, alarm->alarm_mutex);
    }
  }
}


/* ----------------------------------------------- */
/* Create an alarm                                 */

static ilu_refany  ilukt_ML_create_alarm(void)
{
  ilukt_Alarm    *new_alarm = ilu_malloc(sizeof(ilukt_Alarm));
  int             status;
  DECLARE_THREAD(thread_id);

  if (new_alarm == NIL) {
    DEBUG(THREAD_DEBUG,
	  (stderr,
      "ilukt_ML_create_alarm:  can't allocate space for alarm\n"));
    return NIL;			/* couldn't allocate space, so
				 * return */
  }

  memset(new_alarm, 0, sizeof(*new_alarm));

  if (!INIT_CONDITION(new_alarm->alarm_condition)) {
    ilu_free(new_alarm);
    DEBUG(THREAD_DEBUG,
	  (stderr, "ilukt_ML_create_alarm:  "
	   "something went wrong with initialization of condition variable\n"));
    return NIL;
  }
  if (!INIT_MUTEX(new_alarm->alarm_mutex)) {
    DEBUG(THREAD_DEBUG,
	  (stderr, "ilukt_ML_create_alarm:  "
	   "something went wrong with mutex initialization,"
	   " so clean up and fail\n"));
    DESTROY_CONDITION(new_alarm->alarm_condition);
    ilu_free(new_alarm);
    return NIL;
  }
  status = DISPATCH_THREAD(ilukt_ML_run_alarm, (void *) new_alarm,
			   thread_id);
  if (status != 0) {
    DEBUG(THREAD_DEBUG,
	  (stderr, "ilukt_ML_create_alarm:  "
	   "couldn't spawn a new thread (errno %d)\n",
	   status));
    goto givup;
  }
  status = DETACH_THREAD(thread_id);
  if (status != 0) {
    DEBUG(THREAD_DEBUG,
	  (stderr, "ilukt_ML_create_alarm:  "
	   "couldn't detach new thread (errno %d)\n",
	   status));
    goto givup;
  }
  DEBUG(THREAD_DEBUG,
	(stderr,
	 "ilukt_ML_create_alarm:  new alarm %p\n",
	 new_alarm));
  return new_alarm;
givup:
  DESTROY_CONDITION(new_alarm->alarm_condition);
  DESTROY_MUTEX(new_alarm->alarm_mutex);
  ilu_free(new_alarm);
  return NIL;
}


/* ----------------------------------------------- */
/* Set an alarm                                    */

static void
  ilukt_ML_set_alarm(ilu_refany alarm_in_disguise, ilu_FineTime t, void (*proc)(ilu_private rock), ilu_private rock)
{
  ilukt_Alarm    *alarm = (ilukt_Alarm *) alarm_in_disguise;
  ilu_string      ftt = no_ftt;

#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & THREAD_DEBUG)
    ftt = FmtThread(GET_CURRENT_THREAD());
#endif

  DEBUG(THREAD_DEBUG,
	(stderr,
	 "ilukt_ML_set_alarm(%p): called by thread %s.\n",
	 alarm, ftt));

  ILU_LOCK_MUTEX(alarm->alarm_mutex);

  DEBUG(THREAD_DEBUG,
	(stderr,
	 "ilukt_ML_set_alarm(%p): mutex locked by thread %s\n",
	 alarm, ftt));

  alarm->fire_time = t;
  alarm->proc = proc;
  alarm->rock = rock;

  CONDITION_BROADCAST(alarm->alarm_condition);
  ILU_UNLOCK_MUTEX(alarm->alarm_mutex);

  DEBUG(THREAD_DEBUG,
	(stderr,
	 "ilukt_ML_set_alarm(%p): signalled and unlocked by thread %s.\n",
	 alarm, ftt));

  FreeThreadFmt(ftt);
  return;
}


/* ----------------------------------------------- */
/* Unset an alarm                                  */

static void
  ilukt_ML_unset_alarm(ilu_refany alarm_in_disguise)
{
  ilukt_Alarm    *alarm = (ilukt_Alarm *) alarm_in_disguise;
  ilu_string      ftt = no_ftt;

#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & THREAD_DEBUG)
    ftt = FmtThread(GET_CURRENT_THREAD());
#endif

  DEBUG(THREAD_DEBUG,
	(stderr,
	 "ilukt_ML_unset_alarm(%p): called by thread %s.\n",
	 alarm, ftt));

  ILU_LOCK_MUTEX(alarm->alarm_mutex);

  DEBUG(THREAD_DEBUG,
	(stderr,
	 "ilukt_ML_unset_alarm(%p): mutex locked by thread %s.\n",
	 alarm, ftt));

  alarm->fire_time.ft_s = 0;

  CONDITION_BROADCAST(alarm->alarm_condition);
  ILU_UNLOCK_MUTEX(alarm->alarm_mutex);

  DEBUG(THREAD_DEBUG,
	(stderr,
	 "ilukt_ML_unset_alarm(%p): cond. signalled and mutex unlocked by thread %s.\n",
	 alarm, ftt));

  FreeThreadFmt(ftt);
  return;
}


/* ----------------------------------------------- */
/* ilu_MainLoop                                    */

static ilu_MainLoop
  _ilu_threaded_ml = {
    NULL, NULL, NULL, NULL, NULL, NULL, 
    ilukt_ML_create_alarm,
    ilukt_ML_set_alarm,
    ilukt_ML_unset_alarm };


/* ----------------------------------------------- */
/* ilu_InitializeOSThreading                   */

ilu_boolean
  ilu_InitializeOSThreading(ILU_ERRS((bad_param, no_memory,
				      no_resources, internal)) * err)
{
  static ilu_boolean initialized = FALSE;

  if (initialized)
    return ILU_CLER(*err);
  initialized = TRUE;

  ilu_SetWaitTech(&_ilu_threaded_wt);
  ilu_SetLockTech(&_ilu_threaded_lt, err);
  if (ILU_ERRNOK(*err)) {
    ILU_HANDLED(*err);
    return (ilu_FALSE);
  }
  ilu_SetMainLoop(&_ilu_threaded_ml);
  return TRUE;
}


/* ----------------------------------------------- */
/* ilu_OSForkNewThread                               */

ilu_boolean
  ilu_OSForkNewThread (void (*proc)(void *arg), void *arg,
		       ILU_ERRS((no_memory, no_resources,
				 internal)) *err)
{
  int             status;
  DECLARE_THREAD(thread_id);
  function_and_argument *func_args;
  func_args = ilu_MallocE(sizeof(*func_args), err);
  if (ILU_ERRNOK(*err))
    return FALSE;

  func_args->proc = proc;
  func_args->arg = arg;

  status = DISPATCH_THREAD(run_function, func_args, thread_id);
  if (status != 0) {
    DEBUG(THREAD_DEBUG,
	  (stderr,
	   "OS-fork(%p, %p) returns error code %d\n",
	   proc, arg, status));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_threadFork,
			 ilu_FALSE);
  }
#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel & THREAD_DEBUG) {
    ilu_string      ftt = FmtThread(thread_id);
    DEBUG(THREAD_DEBUG, (stderr, "ilu_OSForkNewThread(%p, %p) = %s\n",
			 proc, arg, ftt));
    FreeThreadFmt(ftt);
  }
#endif
  status = DETACH_THREAD(thread_id);
  if (status != 0) {
    DEBUG(THREAD_DEBUG,
	  (stderr,
	   "OS-detach(new thread) returns error code %d\n",
	   status));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_threadFork,
			 ilu_FALSE);
  }
  return ILU_CLER(*err);
}


/* ----------------------------------------------- */
/* end of file                                     */
/* ----------------------------------------------- */
