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

/* Stdio stream locking */

#define _REENTRANT
#include <stdio.h>
#include <linux/limits.h>
#include "pthread.h"
#include "internals.h"
#include "queue.h"
#include "restart.h"
#include "spinlock.h"

/* Table of I/O locks, indexed by Unix file descriptor */

static pthread_mutex_t pthread_file_locks[OPEN_MAX] =
  { PTHREAD_MUTEX_INITIALIZER }; /* all zeros */

static inline pthread_mutex_t * pthread_fd_lock(int fd)
{
  pthread_mutex_t * m = &pthread_file_locks[fd];
  m->m_kind = PTHREAD_MUTEX_RECURSIVE_NP;
  return m;
}

void flockfile(FILE *stream)
{
  int fd = fileno(stream);
  if (fd >= 0 && fd < OPEN_MAX) 
    pthread_mutex_lock(pthread_fd_lock(fd));
}

void funlockfile(FILE *stream)
{
  int fd = fileno(stream);
  if (fd >= 0 && fd < OPEN_MAX) 
    pthread_mutex_unlock(pthread_fd_lock(fd));
}

int ftrylockfile(FILE *stream)
{
  int fd = fileno(stream);
  if (fd >= 0 && fd < OPEN_MAX) 
    return pthread_mutex_trylock(pthread_fd_lock(fd));
  else
    return -1;
}

void ffreelockfile_np(int fd)
{
  if (fd >= 0 && fd < OPEN_MAX) {
    pthread_mutex_init(&pthread_file_locks[fd], NULL);
  }
}

void fresetlockfiles_np()
{
  int fd;
  for (fd = 0; fd < OPEN_MAX; fd++) 
    pthread_mutex_init(&pthread_file_locks[fd], NULL);
}

