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

/* Thread-specific data */

#include <stddef.h>
#include "pthread.h"
#include "internals.h"

typedef void (*destr_function)(void *);

/* Table of keys. */

struct pthread_key_struct {
  int in_use;                   /* already allocated? */
  destr_function destr;         /* destruction routine */
};

static struct pthread_key_struct pthread_keys[PTHREAD_KEYS_MAX] =
  { { 0, NULL } };

/* Mutex to protect access to pthread_keys */

static pthread_mutex_t pthread_keys_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Create a new key */

int pthread_key_create(pthread_key_t * key, destr_function destr)
{
  int i;
  pthread_mutex_lock(&pthread_keys_mutex);
  for (i = 0; i < PTHREAD_KEYS_MAX; i++) {
    if (! pthread_keys[i].in_use) {
      pthread_keys[i].in_use = 1;
      pthread_keys[i].destr = destr;
      pthread_mutex_unlock(&pthread_keys_mutex);
      *key = i;
      return 0;
    }
  }
  pthread_mutex_unlock(&pthread_keys_mutex);
  return EAGAIN;
}

/* Delete a key */

int pthread_key_delete(pthread_key_t key)
{
  pthread_mutex_lock(&pthread_keys_mutex);
  if (key >= PTHREAD_KEYS_MAX || !pthread_keys[key].in_use) {
    pthread_mutex_unlock(&pthread_keys_mutex);
    return EINVAL;
  }
  pthread_keys[key].in_use = 0;
  pthread_keys[key].destr = NULL;
  pthread_mutex_unlock(&pthread_keys_mutex);
  return 0;
}

/* Set the value of a key */

int pthread_setspecific(pthread_key_t key, const void * pointer)
{
  pthread_t self = thread_self();
  if (key >= PTHREAD_KEYS_MAX) return EINVAL;
  self->p_specific[key] = (void *) pointer;
  return 0;
}

/* Get the value of a key */

void * pthread_getspecific(pthread_key_t key)
{
  pthread_t self = thread_self();
  if (key >= PTHREAD_KEYS_MAX)
    return NULL;
  else
    return self->p_specific[key];
}
  
/* Call the destruction routines on all keys */

void pthread_destroy_specifics()
{
  int i;
  pthread_t self = thread_self();
  destr_function destr;
  void * data;

  for (i = 0; i < PTHREAD_KEYS_MAX; i++) {
    destr = pthread_keys[i].destr;
    data = self->p_specific[i];
    if (destr != NULL && data != NULL) destr(data);
  }
}
