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

/* Thread cancellation */

#include "pthread.h"
#include "internals.h"
#include "restart.h"

int pthread_setcancelstate(int state, int * oldstate)
{
  pthread_t self = thread_self();
  if (state < PTHREAD_CANCEL_ENABLE || state > PTHREAD_CANCEL_DISABLE)
    return EINVAL;
  if (oldstate != NULL) *oldstate = self->p_cancelstate;
  self->p_cancelstate = state;
  if (self->p_canceled && 
      self->p_cancelstate == PTHREAD_CANCEL_ENABLE &&
      self->p_canceltype == PTHREAD_CANCEL_ASYNCHRONOUS)
    pthread_exit(PTHREAD_CANCELED);
  return 0;
}

int pthread_setcanceltype(int type, int * oldtype)
{
  pthread_t self = thread_self();
  if (type < PTHREAD_CANCEL_DEFERRED || type > PTHREAD_CANCEL_ASYNCHRONOUS)
    return EINVAL;
  if (oldtype != NULL) *oldtype = self->p_canceltype;
  self->p_canceltype = type;
  if (self->p_canceled && 
      self->p_cancelstate == PTHREAD_CANCEL_ENABLE &&
      self->p_canceltype == PTHREAD_CANCEL_ASYNCHRONOUS)
    pthread_exit(PTHREAD_CANCELED);
  return 0;
}

int pthread_cancel(pthread_t thread)
{
  thread->p_canceled = 1;
  kill(thread->p_pid, PTHREAD_SIG_CANCEL);
  return 0;
}

void pthread_testcancel(void)
{
  pthread_t self = thread_self();
  if (self->p_canceled && self->p_cancelstate == PTHREAD_CANCEL_ENABLE)
    pthread_exit(PTHREAD_CANCELED);
}

void _pthread_cleanup_push(struct _pthread_cleanup_buffer * buffer,
                           void (*routine)(void *), void * arg)
{
  pthread_t self = thread_self();
  buffer->routine = routine;
  buffer->arg = arg;
  buffer->prev = self->p_cleanup;
  self->p_cleanup = buffer;
}

void _pthread_cleanup_pop(struct _pthread_cleanup_buffer * buffer,
                          int execute)
{
  pthread_t self = thread_self();
  if (execute) buffer->routine(buffer->arg);
  self->p_cleanup = buffer->prev;
}

void _pthread_cleanup_push_defer(struct _pthread_cleanup_buffer * buffer,
                                    void (*routine)(void *), void * arg)
{
  pthread_t self = thread_self();
  buffer->routine = routine;
  buffer->arg = arg;
  buffer->canceltype = self->p_canceltype;
  buffer->prev = self->p_cleanup;
  self->p_canceltype = PTHREAD_CANCEL_DEFERRED;
  self->p_cleanup = buffer;
}

void _pthread_cleanup_pop_restore(struct _pthread_cleanup_buffer * buffer,
                                     int execute)
{
  pthread_t self = thread_self();
  if (execute) buffer->routine(buffer->arg);
  self->p_cleanup = buffer->prev;
  self->p_canceltype = buffer->canceltype;
  if (self->p_canceled && 
      self->p_cancelstate == PTHREAD_CANCEL_ENABLE &&
      self->p_canceltype == PTHREAD_CANCEL_ASYNCHRONOUS)
    pthread_exit(PTHREAD_CANCELED);
}

void pthread_perform_cleanup(void)
{
  pthread_t self = thread_self();
  struct _pthread_cleanup_buffer * c;
  for (c = self->p_cleanup; c != NULL; c = c->prev) c->routine(c->arg);
}

