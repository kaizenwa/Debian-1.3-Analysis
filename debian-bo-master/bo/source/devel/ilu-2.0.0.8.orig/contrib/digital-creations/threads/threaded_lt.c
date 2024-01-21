/* $Id: threaded_lt.c,v 1.1 1996/04/16 00:06:26 janssen Exp $ */

#include "threaded_mainloop.h"

#include <iluntrnl.h>
#include <iludebug.h>

#include <stdio.h>

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
} threaded_lt_mutex;

typedef struct
{
  DECLARE_CONDITION(condition);
  ilu_string d1, d2;
} threaded_lt_condition;

#define PN(x) (x != ILU_NIL ? x : "(null)")

#define MUTEX(m) threaded_lt_mutex *mutex_obj = (threaded_lt_mutex *)m
#define CONDITION(c) threaded_lt_condition *condition_obj = (threaded_lt_condition *)c

/* should more checking be done to ensure m is valid (e.g. examining the
   structure of m->mutex to make sure it looks like a viable mutex)? */
#define ASSERT_VALID_MUTEX(m, e) if (m == ILU_NIL) { ILU_ERR_CONS1(bad_param, e, minor, ilu_bpm_nil, 0); return; }
#define ASSERT_VALID_CONDITION(c, e) ASSERT_VALID_MUTEX(c, e)

#define ASSERT_HOLD_MUTEX(m, e) if (!(m->locked && (SAME_THREAD(m->owner_thread, current_thread)))) { ILU_ERR_CONS0(bad_locks, e, 0); return; }

static ilu_Mutex
threaded_lt_mcreate(ilu_string d1, ilu_string d2)
{
  threaded_lt_mutex *new_mutex = (threaded_lt_mutex *)ilu_malloc(sizeof(threaded_lt_mutex));

  DEBUG(LOCK_DEBUG, (stderr, "threaded_lt_mcreate('%s','%s')\n", d1, d2));

  if (!new_mutex)
    /* couldn't allocate space, so fail out */
    {
      return ILU_NIL;
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
	  return ILU_NIL;
	}
    }

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
	  return ILU_NIL;
	}
    }

  new_mutex->locked = ilu_FALSE;

  DEBUG(LOCK_DEBUG, (stderr, "threaded_lt_mcreate succeeded: %p\n", new_mutex));

  return (ilu_Mutex) new_mutex;
}

static void 
threaded_lt_muncons(ilu_Mutex m, ilu_string *d1, ilu_string *d2, ILU_ERRS((bad_param)) *err)
{
  MUTEX(m);

  ASSERT_VALID_MUTEX(mutex_obj, err);

  if (d1 == ILU_NIL || d2 == ILU_NIL)
    {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, 0);
      return;
    }

  *d1 = mutex_obj->d1;
  *d2 = mutex_obj->d2;

  ILU_CLER(*err);

  return;
}

static void 
threaded_lt_acquire(ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) *err)
{
  MUTEX(m);
  CURRENT_THREAD;

#ifdef ENABLE_DEBUGGING
  ilu_string t1, t2;
  
  if (_ilu_DebugLevel & LOCK_DEBUG)
    {
      GET_THREAD_SPECIFIC(&t1, &t2);
      fprintf(stderr, "(thread [%s%s]) waiting for (mutex [%s%s])\n", t1, t2, mutex_obj->d1, mutex_obj->d2);
    }
#endif /* ENABLE_DEBUGGING */

  ASSERT_VALID_MUTEX(mutex_obj, err);

  if (mutex_obj->locked && (SAME_THREAD(mutex_obj->owner_thread, current_thread)))
    {
      ILU_ERR_CONS0(bad_locks, err, 0);
      return;
    }

  LOCK_MUTEX(mutex_obj->mutex);

  DEBUG(LOCK_DEBUG, (stderr, "(thread [%s%s]) acquired (mutex [%s%s])\n", t1, t2, mutex_obj->d1, mutex_obj->d2));

  mutex_obj->owner_thread = current_thread;
  mutex_obj->locked = ilu_TRUE;

  ILU_CLER(*err);

  return;
}

static void 
threaded_lt_hold(ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) *err)
{
  MUTEX(m);
  CURRENT_THREAD;

  ASSERT_VALID_MUTEX(mutex_obj, err);

  ASSERT_HOLD_MUTEX(mutex_obj, err);

  ILU_CLER(*err);

  return;
}

static void
threaded_lt_release(ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) *err)
{
  MUTEX(m);
  CURRENT_THREAD;

#ifdef ENABLE_DEBUGGING
  ilu_string t1, t2;
  
  if (_ilu_DebugLevel & LOCK_DEBUG)
    {
      GET_THREAD_SPECIFIC(&t1, &t2);
      fprintf(stderr, "(thread [%s%s]) releasing (mutex [%s%s])\n", t1, t2, mutex_obj->d1, mutex_obj->d2);
    }
#endif /* ENABLE_DEBUGGING */

  ASSERT_VALID_MUTEX(mutex_obj, err);

  ASSERT_HOLD_MUTEX(mutex_obj, err);

  mutex_obj->locked = ilu_FALSE;

  UNLOCK_MUTEX(mutex_obj->mutex);

  ILU_CLER(*err);

  return;
}

static ilu_Condition
threaded_lt_ccreate(ilu_string d1, ilu_string d2)
{
  threaded_lt_condition *new_condition = (threaded_lt_condition *)ilu_malloc(sizeof(threaded_lt_condition));

  DEBUG(LOCK_DEBUG, (stderr, "threaded_lt_ccreate called\n"));

  if (new_condition == NIL)
    return ILU_NIL;

  if (!INIT_CONDITION(new_condition->condition))
    {
      ilu_free(new_condition);
      return ILU_NIL;
    }

  if (d1 != NIL)
    {
      new_condition->d1 = _ilu_Strdup(d1);
      if (new_condition->d1 == NIL)
	{
	  DESTROY_CONDITION(new_condition->condition);
	  ilu_free(new_condition);
	  return ILU_NIL;
	}
    }

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
	  return ILU_NIL;
	}
    }

  DEBUG(LOCK_DEBUG, (stderr, "threaded_lt_ccreate succeeded: %p\n", new_condition));

  return new_condition;
}

static void
threaded_lt_cuncons(ilu_Condition c, ilu_string *d1, ilu_string *d2, ILU_ERRS((bad_param )) *err)
{
  CONDITION(c);

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

static void
threaded_lt_notify(ilu_Condition c, ILU_ERRS((bad_param)) *err)
{
  CONDITION(c);

#ifdef ENABLE_DEBUGGING
  ilu_string t1, t2;
  
  if (_ilu_DebugLevel & LOCK_DEBUG)
    {
      GET_THREAD_SPECIFIC(&t1, &t2);
      fprintf(stderr, "(thread [%s%s]) notifying (condition [%s%s])\n", t1, t2, condition_obj->d1, condition_obj->d2);
    }
#endif /* ENABLE_DEBUGGING */

  ASSERT_VALID_CONDITION(condition_obj, err);

  CONDITION_BROADCAST(condition_obj->condition);

  ILU_CLER(*err);

  return;
}

static void
threaded_lt_cdestroy(ilu_Condition c, ILU_ERRS((bad_param)) *err)
{
  CONDITION(c);

  DEBUG(LOCK_DEBUG, (stderr, "threaded_lt_cdestroy called: (condition = %p)\n", condition_obj));

  ASSERT_VALID_CONDITION(condition_obj, err);

  DESTROY_CONDITION(condition_obj->condition);

  if (condition_obj->d1 != ILU_NIL)
    {
      ilu_free(condition_obj->d1);
    }
  if (condition_obj->d2 != ILU_NIL)
    {
      ilu_free(condition_obj->d2);
    }

  ilu_free(condition_obj);

  ILU_CLER(*err);

  return;
}

static void
threaded_lt_wait(ilu_Condition c, ilu_Mutex m, ilu_Mutex m2, ILU_ERRS((bad_param, bad_locks)) *err)
{
  CONDITION(c);
  MUTEX(m);
  threaded_lt_mutex *mutex_obj2 = (threaded_lt_mutex *)m2;
  CURRENT_THREAD;
#ifdef ENABLE_DEBUGGING
  ilu_string t1, t2;

  if (_ilu_DebugLevel & LOCK_DEBUG)
    {
      GET_THREAD_SPECIFIC(&t1, &t2);
    }
#endif /* ENABLE_DEBUGGING */

  ASSERT_VALID_CONDITION(condition_obj, err);
  ASSERT_VALID_MUTEX(mutex_obj, err);
  ASSERT_VALID_MUTEX(mutex_obj2, err);
    
  ASSERT_HOLD_MUTEX(mutex_obj, err);
  if (mutex_obj != mutex_obj2)
    {
      ASSERT_HOLD_MUTEX(mutex_obj2, err);

      DEBUG(LOCK_DEBUG, (stderr, "(thread [%s%s]) unlocking mutex [%s%s]\n", t1, t2, mutex_obj2->d1, mutex_obj2->d2));

      UNLOCK_MUTEX(mutex_obj2->mutex);
    }
  
  DEBUG(LOCK_DEBUG, (stderr, "(thread [%s%s]) waiting on (condition [%s%s])\n", t1, t2, condition_obj->d1, condition_obj->d2));

  CONDITION_WAIT(condition_obj->condition, mutex_obj->mutex);

  DEBUG(LOCK_DEBUG, (stderr, "(thread [%s%s]) signalled by (condition [%s%s])\n", t1, t2, condition_obj->d1, condition_obj->d2));

  if (mutex_obj != mutex_obj2)
    {
      DEBUG(LOCK_DEBUG, (stderr, "(thread [%s%s]) locking mutex [%s%s]\n", t1, t2, mutex_obj2->d1, mutex_obj2->d2));

      LOCK_MUTEX(mutex_obj2->mutex);

      DEBUG(LOCK_DEBUG, (stderr, "(thread [%s%s]) locked mutex [%s%s]\n", t1, t2, mutex_obj2->d1, mutex_obj2->d2));

      mutex_obj2->owner_thread = current_thread;
      mutex_obj2->locked = ilu_TRUE;

    }

  DEBUG(LOCK_DEBUG, (stderr, "(thread [%s%s]) locking mutex [%s%s]\n", t1, t2, mutex_obj->d1, mutex_obj->d2));

  LOCK_MUTEX(mutex_obj->mutex);
  mutex_obj->owner_thread = current_thread;
  mutex_obj->locked = ilu_TRUE;

  DEBUG(LOCK_DEBUG, (stderr, "(thread [%s%s]) locked mutex [%s%s]\n", t1, t2, mutex_obj->d1, mutex_obj->d2));

  ILU_CLER(*err);

  return;
}

ilu_LockTech threaded_lt = { threaded_lt_mcreate,
			     threaded_lt_muncons,
			     threaded_lt_acquire,
			     threaded_lt_hold,
			     threaded_lt_release,
			     threaded_lt_ccreate,
			     threaded_lt_cuncons,
			     threaded_lt_notify,
			     threaded_lt_cdestroy,
			     threaded_lt_wait };
			    

