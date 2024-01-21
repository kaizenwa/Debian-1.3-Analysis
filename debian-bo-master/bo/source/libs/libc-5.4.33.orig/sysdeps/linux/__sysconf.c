/* Copyright (C) 1991, 1993 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <errno.h>
#include <limits.h>
#include <unistd.h>
#include <stddef.h>
#include <stdio.h>
#include <time.h>
#include <sys/param.h>

/* Get the value of the system variable NAME.  */
long int
DEFUN(__sysconf, (name), int name)
{
  switch (name)
    {
    default:
      errno = EINVAL;
      return -1;

    case _SC_AIO_LISTIO_MAX:
#ifdef	AIO_LISTIO_MAX
      return AIO_LISTIO_MAX;
#else
      return -1;
#endif

    case _SC_AIO_MAX:
#ifdef	AIO_MAX
      return AIO_MAX;
#else
      return -1;
#endif

    case _SC_AIO_PRIO_DELTA_MAX:
#ifdef	AIO_PRIO_DELTA_MAX
      return AIO_PRIO_DELTA_MAX;
#else
      return -1;
#endif

    case _SC_ARG_MAX:
#ifdef	ARG_MAX
      return ARG_MAX;
#else
      return -1;
#endif

    case _SC_CHILD_MAX:
#ifdef	CHILD_MAX
      return CHILD_MAX;
#else
      return -1;
#endif

    case _SC_CLK_TCK:
      return CLK_TCK;

    case _SC_DELAYTIMER_MAX:
#ifdef	DELAYTIMER_MAX
      return DELAYTIMER_MAX;
#else
      return -1;
#endif

    case _SC_MQ_OPEN_MAX:
#ifdef	MQ_OPEN_MQX
      return MQ_OPEN_MQX;
#else
      return -1;
#endif

    case _SC_MQ_PRIO_MAX:
#ifdef	MQ_PRIO_MAX
      return MQ_PRIO_MAX;
#else
      return -1;
#endif

    case _SC_NGROUPS_MAX:
#ifdef	NGROUPS_MAX
      return NGROUPS_MAX;
#else
      return -1;
#endif

    case _SC_OPEN_MAX:
      return OPEN_MAX;

    case _SC_RTSIG_MAX:
#ifdef	RTSIG_MAX
      return RTSIG_MAX;
#else
      return -1;
#endif

    case _SC_SEM_NSEMS_MAX:
#ifdef	SEM_NSEMS_MAX
      return SEM_NSEMS_MAX;
#else
      return -1;
#endif

    case _SC_SEM_VALUE_MAX:
#ifdef	SEM_VALUE_MAX
      return SEM_VALUE_MAX;
#else
      return -1;
#endif

    case _SC_SIGQUEUE_MAX:
#ifdef	SIGQUEUE_MAX
      return SIGQUEUE_MAX;
#else
      return -1;
#endif

    case _SC_STREAM_MAX:
      return STREAM_MAX;

    case _SC_TIMER_MAX:
#ifdef	TIMER_MAX
      return TIMER_MAX;
#else
      return -1;
#endif

    case _SC_TZNAME_MAX:
#ifdef __linux__
      return TZNAME_MAX;
#else
      return __tzname_max ();
#endif

    case _SC_ASYNCHRONOUS_IO:
#ifdef	_POSIX_ASYNCHRONOUS_IO
      return 1;
#else
      return -1;
#endif
    case _SC_FSYNC:
#ifdef	_POSIX_FSYNC
      return 1;
#else
      return -1;
#endif
    case _SC_JOB_CONTROL:
#ifdef	_POSIX_JOB_CONTROL
      return 1;
#else
      return -1;
#endif
    case _SC_MAPPED_FILES:
#ifdef	_POSIX_MAPPED_FILES
      return 1;
#else
      return -1;
#endif
    case _SC_MEMLOCK:
#ifdef	_POSIX_MEMLOCK
      return 1;
#else
      return -1;
#endif
    case _SC_MEMLOCK_RANGE:
#ifdef	_POSIX_MEMLOCK_RANGE
      return 1;
#else
      return -1;
#endif
    case _SC_MEMORY_PROTECTION:
#ifdef	_POSIX_MEMORY_PROTECTION
      return 1;
#else
      return -1;
#endif
    case _SC_MESSAGE_PASSING:
#ifdef	_POSIX_MESSAGE_PASSING
      return 1;
#else
      return -1;
#endif
    case _SC_PRIORITIZED_IO:
#ifdef	_POSIX_PRIORITIZED_IO
      return 1;
#else
      return -1;
#endif
    case _SC_PRIORITY_SCHEDULING:
#ifdef	_POSIX_PRIORITY_SCHEDULING
      return 1;
#else
      return -1;
#endif
    case _SC_REALTIME_SIGNALS:
#ifdef	_POSIX_REALTIME_SIGNALS
      return 1;
#else
      return -1;
#endif
    case _SC_SAVED_IDS:
#ifdef	_POSIX_SAVED_IDS
      return 1;
#else
      return -1;
#endif
    case _SC_SEMAPHORES:
#ifdef	_POSIX_SEMAPHORES
      return 1;
#else
      return -1;
#endif
    case _SC_SHARED_MEMORY_OBJECTS:
#ifdef	_POSIX_SHARED_MEMORY_OBJECTS
      return 1;
#else
      return -1;
#endif
    case _SC_SYNCHRONIZED_IO:
#ifdef	_POSIX_SYNCHRONIZED_IO
      return 1;
#else
      return -1;
#endif
    case _SC_TIMERS:
#ifdef	_POSIX_TIMERS
      return 1;
#else
      return -1;
#endif
    case _SC_VERSION:
      return _POSIX_VERSION;

    case _SC_PAGESIZE:
#ifdef  EXEC_PAGESIZE
      return EXEC_PAGESIZE;
#else   /* No EXEC_PAGESIZE.  */
#ifdef  NBPG
#ifndef CLSIZE
#define CLSIZE  1
#endif  /* No CLSIZE.  */
      return NBPG * CLSIZE;
#else   /* No NBPG.  */
      return NBPC;
#endif  /* NBPG.  */
#endif  /* EXEC_PAGESIZE.  */

    case _SC_BC_BASE_MAX:
#ifdef	BC_BASE_MAX
      return BC_BASE_MAX;
#else
      return -1;
#endif

    case _SC_BC_DIM_MAX:
#ifdef	BC_DIM_MAX
      return BC_DIM_MAX;
#else
      return -1;
#endif

    case _SC_BC_SCALE_MAX:
#ifdef	BC_SCALE_MAX
      return BC_SCALE_MAX;
#else
      return -1;
#endif

    case _SC_BC_STRING_MAX:
#ifdef	BC_STRING_MAX
      return BC_STRING_MAX;
#else
      return -1;
#endif

    case _SC_EQUIV_CLASS_MAX:
#ifdef	EQUIV_CLASS_MAX
      return EQUIV_CLASS_MAX;
#else
      return -1;
#endif

    case _SC_EXPR_NEST_MAX:
#ifdef	EXPR_NEST_MAX
      return EXPR_NEST_MAX;
#else
      return -1;
#endif

    case _SC_LINE_MAX:
#ifdef	LINE_MAX
      return LINE_MAX;
#else
      return -1;
#endif

    case _SC_RE_DUP_MAX:
#ifdef	RE_DUP_MAX
      return RE_DUP_MAX;
#else
      return -1;
#endif


    case _SC_2_VERSION:
      /* This is actually supposed to return the version
	 of the 1003.2 utilities on the system {POSIX2_VERSION}.  */
      return _POSIX2_C_VERSION;

    case _SC_2_C_BIND:
#ifdef	_POSIX2_C_BIND
      return _POSIX2_C_BIND;
#else
      return -1;
#endif

    case _SC_2_C_DEV:
#ifdef	_POSIX2_C_DEV
      return _POSIX2_C_DEV;
#else
      return -1;
#endif

    case _SC_2_FORT_DEV:
#ifdef	_POSIX2_FORT_DEV
      return _POSIX2_FORT_DEV;
#else
      return -1;
#endif

    case _SC_2_LOCALEDEF:
#ifdef	_POSIX2_LOCALEDEF
      return _POSIX2_LOCALEDEF;
#else
      return -1;
#endif

    case _SC_2_SW_DEV:
#ifdef	_POSIX2_SW_DEV
      return _POSIX2_SW_DEV;
#else
      return -1;
#endif
    }
}

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__sysconf, sysconf);
#endif
