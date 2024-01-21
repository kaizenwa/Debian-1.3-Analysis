/* $Id: threaded_ml.c,v 1.1 1996/04/16 00:06:26 janssen Exp $ */

#include "threaded_mainloop.h"

#include <iluntrnl.h>

#include <errno.h>

#define MicrosoftSucks 1

typedef struct
{
  DECLARE_MUTEX(alarm_mutex);
  DECLARE_CONDITION(alarm_condition);
  ilu_FineTime fire_time;
  void (*proc)(ilu_private rock);
  ilu_private rock;
} threaded_ml_alarm;

/* this silliness is because Solaris wants functions called by thr_create to have a return a void * */
typedef struct
{
  void (*proc)(void *arg);
  void *arg;
} function_and_argument;

static void *
run_function(void *func_and_args)
{
  function_and_argument *proc_args = (function_and_argument *)func_and_args;

  DEBUG(MAINLOOP_DEBUG, (stderr, "thread (%u) in run_function (function = %p, arg = %p)\n", GET_CURRENT_THREAD(), proc_args->proc, proc_args->arg));

  proc_args->proc(proc_args->arg);

  ilu_free(proc_args);

  return NULL;
}

void 
threaded_ml_fork(void (*proc)(void *arg), void *arg)
{
  DECLARE_THREAD(thread_id);

  function_and_argument *func_args = ilu_must_malloc(sizeof(function_and_argument));

  func_args->proc = proc;
  func_args->arg = arg;

  (void) DISPATCH_THREAD(run_function, func_args, thread_id);

  DEBUG(MAINLOOP_DEBUG, (stderr, "spawned new thread (%u) (function = %p, arg = %p)\n", thread_id, proc, arg));

  return;
}

static void *
threaded_ml_run_alarm(void *alarm_in_disguise)
{
  threaded_ml_alarm *alarm = (threaded_ml_alarm *) alarm_in_disguise;
#ifdef SOLARIS_MAINLOOP
  timestruc_t set_time;
#endif
#ifdef PTHREAD_MAINLOOP
  struct timespec set_time;
  int additional_info;
#endif
  int err;

  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_run_alarm: alarm (%p) started on thread (%u)\n", alarm, GET_CURRENT_THREAD()));

  LOCK_MUTEX(alarm->alarm_mutex);

  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_run_alarm: mutex on alarm (%p) locked\n", alarm));

  if (alarm->fire_time.ft_s == 0)
    /* initally unset, so wait for a signal indicating someone's set our timer */
    {
      DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_run_alarm: waiting for alarm (%p) to be set\n", alarm));

      CONDITION_WAIT(alarm->alarm_condition, alarm->alarm_mutex);
    }      

  while (MicrosoftSucks)
    {
      if (alarm->fire_time.ft_s != 0)
	{
	  set_time.tv_sec = alarm->fire_time.ft_s;
	  set_time.tv_nsec = (alarm->fire_time.ft_t * (1000000000 / ilu_FineTimeRate));

	  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_run_alarm: alarm (%p) set for (%ld) seconds and (%ld) nanoseconds\n", alarm, set_time.tv_sec, set_time.tv_nsec));

	  err = CONDITION_TIMEDWAIT(alarm->alarm_condition, alarm->alarm_mutex, set_time);
	      	  
#ifdef PTHREAD_MAINLOOP
	  if (err)
	    {
	      additional_info = errno;
	    }
#endif

#ifdef SOLARIS_MAINLOOP
	  if (err == ETIME)
#endif
#ifdef PTHREAD_MAINLOOP
	  if ((err != 0) && additional_info == EAGAIN)
#endif
	    {
	      /* abnormal termination; this means a timeout error occured */
	      /* unlock ourselves in case alarm->proc (or someone else) wants
		 to set the alarm */

	      DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_run_alarm: firing alarm (%p)\n", alarm));

	      alarm->fire_time.ft_s = 0;
	      UNLOCK_MUTEX(alarm->alarm_mutex);
	      
	      DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_run_alarm: mutex on alarm (%p) unlocked\n", alarm));

	      alarm->proc(alarm->rock);

	      LOCK_MUTEX(alarm->alarm_mutex);

	      DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_run_alarm: mutex on alarm (%p) locked\n", alarm));

	      if (alarm->fire_time.ft_s == 0)
		{
		  /* the alarm hasn't been reset, so we'll wait until it it is */
		  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_run_alarm: waiting for alarm (%p) to be set\n"));

		  CONDITION_WAIT(alarm->alarm_condition, alarm->alarm_mutex);
		}
	    }
	  else if (err != 0)
	    {
	      /* some other error occured, probably an invalid time specified,
		 so wait for the alarm to be reset */
	      DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_run_alarm: strange condition occured; waiting for alarm (%p) to be set\n", alarm));

	      CONDITION_WAIT(alarm->alarm_condition, alarm->alarm_mutex);
	    }
	  else
	    {
	      /* normal termination; this means that the condition variable
		 was signalled (i.e. the alarm was reset), so we'll just loop
		 back to the top and reset the time */
	      DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_run_alarm: resetting alarm (%p)\n", alarm));
	    }
	}
      else
	{
	  /* the alarm must've been unset, so wait until something else
	     happens */
	  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_run_alarm: alarm (%p) unset, waiting for set\n", alarm));
	  CONDITION_WAIT(alarm->alarm_condition, alarm->alarm_mutex);
	}
    }
}

static ilu_refany
threaded_ml_create_alarm()
{
  threaded_ml_alarm *new_alarm = ilu_malloc(sizeof(threaded_ml_alarm));
  DECLARE_THREAD(thread_id);

  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_create_alarm called\n"));

  if (new_alarm == NIL)
    return ILU_NIL;  /* couldn't allocate space, so return */
  
  if (!INIT_CONDITION(new_alarm->alarm_condition))
    {
      ilu_free(new_alarm); /* something went wrong with initialization of condition variable */
      return ILU_NIL;
    }

  if (!INIT_MUTEX(new_alarm->alarm_mutex))
    {
      /* something went wrong with mutex initialization, so clean up and fail */
      DESTROY_CONDITION(new_alarm->alarm_condition);
      ilu_free(new_alarm);
      return ILU_NIL;
    }

      if (!DISPATCH_THREAD(threaded_ml_run_alarm, (void *) new_alarm, thread_id)) 
      {
	/* couldn't spawn a new thread, so clean up and fail */
	DESTROY_CONDITION(new_alarm->alarm_condition);
	DESTROY_MUTEX(new_alarm->alarm_mutex);
	ilu_free(new_alarm);

	return ILU_NIL;
      }
    else
      {
	new_alarm->fire_time.ft_s = 0;

	DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_create_alarm successful: %p\n", new_alarm));

	return new_alarm;
      }
}

static void
threaded_ml_set_alarm(ilu_refany alarm_in_disguise, ilu_FineTime t, void (*proc)(ilu_private rock), ilu_private rock)
{
  threaded_ml_alarm *alarm = (threaded_ml_alarm *) alarm_in_disguise;

  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_set_alarm called by thread (%u) on alarm (%p)\n", GET_CURRENT_THREAD(), alarm));

  LOCK_MUTEX(alarm->alarm_mutex);

  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_set_alarm: mutex on alarm (%p) locked by thread (%u)\n", alarm, GET_CURRENT_THREAD()));

  alarm->fire_time = t;
  alarm->proc = proc;
  alarm->rock = rock;

  CONDITION_BROADCAST(alarm->alarm_condition);
  UNLOCK_MUTEX(alarm->alarm_mutex);

  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_set_alarm: alarm (%p) signalled and unlocked by thread (%u)\n", alarm, GET_CURRENT_THREAD()));

  return;
}

static void
threaded_ml_unset_alarm(ilu_refany alarm_in_disguise)
{
  threaded_ml_alarm *alarm = (threaded_ml_alarm *) alarm_in_disguise;

  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_unset_alarm called by thread (%u) on alarm (%p)\n", GET_CURRENT_THREAD(), alarm));

  LOCK_MUTEX(alarm->alarm_mutex);
  
  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_unset_alarm: alarm (%p) locked by thread (%u)\n", alarm, GET_CURRENT_THREAD()));

  alarm->fire_time.ft_s = 0;

  CONDITION_BROADCAST(alarm->alarm_condition);
  UNLOCK_MUTEX(alarm->alarm_mutex);

  DEBUG(MAINLOOP_DEBUG, (stderr, "threaded_ml_unset_alarm: alarm (%p) signalled and unlocked by thread (%u)\n", alarm, GET_CURRENT_THREAD()));

  return;
}

ilu_MainLoop threaded_ml = { NULL, NULL, NULL, NULL, NULL, NULL, 
	       threaded_ml_create_alarm, threaded_ml_set_alarm, threaded_ml_unset_alarm };




