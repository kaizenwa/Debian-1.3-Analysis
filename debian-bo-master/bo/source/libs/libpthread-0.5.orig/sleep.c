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

/* A thread-safe sleep() function. */

#include <stddef.h>
#include <unistd.h>
#include <sys/time.h>
#include "pthread.h"
#include "internals.h"

unsigned int sleep(unsigned int seconds)
{
  struct timespec tp;
  tp.tv_sec = (int) seconds;
  tp.tv_nsec = 0;
  if (nanosleep(&tp, &tp) == 0)
    return 0;
  else
    return (tp.tv_sec + 1);     /* round up */
}
