/* Linuxthreads - a simple clone()-based implementation of Posix        */
/* threads for Linux.                                                   */
/* Copyright (C) 1996 Xavier Leroy (Xavier.Leroy@inria.fr)              */
/*                                                                      */
/* This program is free software; you can redistribute it and/or        */
/* modify it under the terms of the GNU General Public License          */
/* as published by the Free Software Foundation; either version 2       */
/* of the License, or (at your option) any later version.               */
/*                                                                      */
/* This program is distributed in the hope that it will be useful,      */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of       */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        */
/* GNU General Public License for more details.                         */

/* Wrappers around malloc functions, making them thread-safe */

#include <stdlib.h>
#include "../pthread.h"

static pthread_mutex_t pthread_malloc_mutex = PTHREAD_MUTEX_INITIALIZER;

extern void * __libc_malloc(size_t size);
extern void * __libc_calloc(size_t nmemb, size_t size);
extern void __libc_free(void * ptr);
extern void * __libc_realloc(void * ptr, size_t size);
extern void * __libc_memalign (size_t alignment, size_t size);

void * malloc(size_t size)
{
  void * result;
  pthread_mutex_lock(&pthread_malloc_mutex);
  result = __libc_malloc(size);
  pthread_mutex_unlock(&pthread_malloc_mutex);
  return result;
}

void * calloc(size_t nmemb, size_t size)
{
  void * result;
  pthread_mutex_lock(&pthread_malloc_mutex);
  result = __libc_calloc(nmemb, size);
  pthread_mutex_unlock(&pthread_malloc_mutex);
  return result;
}

void free(void * ptr)
{
  pthread_mutex_lock(&pthread_malloc_mutex);
  __libc_free(ptr);
  pthread_mutex_unlock(&pthread_malloc_mutex);
}

void * realloc(void * ptr, size_t size)
{
  void * result;
  pthread_mutex_lock(&pthread_malloc_mutex);
  result = __libc_realloc(ptr, size);
  pthread_mutex_unlock(&pthread_malloc_mutex);
  return result;
}

void * memalign (size_t alignment, size_t size)
{
  void * ret;
  pthread_mutex_lock(&pthread_malloc_mutex);
  ret = __libc_memalign (alignment, size);
  pthread_mutex_unlock(&pthread_malloc_mutex);
  return ret;
}

