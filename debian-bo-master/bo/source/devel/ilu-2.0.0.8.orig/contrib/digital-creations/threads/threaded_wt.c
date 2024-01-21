/* $Id */

#include "threaded_mainloop.h"

#include <iluntrnl.h>

#include <sys/time.h>
#include <sys/types.h>

#include <errno.h>

#define SOCKERRID(x) (E##x)

static void
threaded_wt_read_wait(int fd, ilu_boolean *sure, ilu_FineTime *limit, ILU_ERRS((interrupted)) *err)
{
  fd_set read_set;
  int stat;
  struct timeval time, *time_p;
  
#ifdef ENABLE_DEBUGGING
  ilu_string t1, t2;

  if (_ilu_DebugLevel & LOCK_DEBUG)
    {
      GET_THREAD_SPECIFIC(&t1, &t2);
    }

  DEBUG(LOCK_DEBUG, (stderr, "(thread [%s%s]) waiting for read on (fd [%d])\n", t1, t2, fd));
#endif /* ENABLE_DEBUGGING */

  FD_ZERO(&read_set);
  FD_SET(fd, &read_set);

  if (limit != 0)
    {
      time.tv_sec = limit->ft_s;
      time.tv_usec = ilu_rescale(limit->ft_s, ilu_FineTimeRate, 1000000);
      time_p = &time;
    } 
  else
    {
      time_p = NULL;
    }

  if ((stat = select(fd + 1, &read_set, NULL, &read_set, time_p)) > 0)
    {
      *sure = ilu_TRUE;

      DEBUG(LOCK_DEBUG, (stderr, "waiting input or exceptional condition (thread [%s%s]) on (fd [%d])\n", t1, t2, fd));

      ILU_CLER(*err);

      return;
    }
  else if (stat == -1) /* an error occured; hopefully an interrupt! */
    {
      ASSERT((errno == SOCKERRID(INTR)), buf, (buf, "threaded_wt_read_wait:select failed, errno=%d=%s", errno, strerror(errno)));

      *sure = ilu_FALSE;

      DEBUG(LOCK_DEBUG, (stderr, "interrupted read wait (thread [%s%s]) on (fd [%d])\n", t1, t2, fd));

      ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, 0);
      return;
    }
  else 
    {
      *sure = ilu_FALSE;
      
      DEBUG(LOCK_DEBUG, (stderr, "read wait timeout (thread [%s%s]) on (fd [%d])\n", t1, t2, fd));

      ILU_CLER(*err);
      return;
    }
}

static void
threaded_wt_write_wait(int fd, ilu_boolean *sure, ilu_FineTime *limit, ILU_ERRS((interrupted)) *err)
{
  fd_set write_set;
  int stat;
  struct timeval time, *time_p;
  
#ifdef ENABLE_DEBUGGING
  ilu_string t1, t2;

  if (_ilu_DebugLevel & LOCK_DEBUG)
    {
      GET_THREAD_SPECIFIC(&t1, &t2);
    }

  DEBUG(LOCK_DEBUG, (stderr, "(thread [%s%s]) waiting for write on (fd [%d])\n", t1, t2, fd));
#endif /* ENABLE_DEBUGGING */

  FD_ZERO(&write_set);
  FD_SET(fd, &write_set);

  if (limit != 0)
    {
      time.tv_sec = limit->ft_s;
      time.tv_usec = ilu_rescale(limit->ft_s, ilu_FineTimeRate, 1000000);
      time_p = &time;
    } 
  else
    {
      time_p = NULL;
    }

  if ((stat = select(fd + 1, NULL, &write_set, &write_set, time_p)) > 0)
    {
      *sure = ilu_TRUE;

      DEBUG(LOCK_DEBUG, (stderr, "waiting input or exceptional condition (thread [%s%s]) on (fd [%d])\n", t1, t2, fd));

      ILU_CLER(*err);

      return;
    }
  else if (stat == -1) /* an error occured; hopefully an interrupt! */
    {
      ASSERT((errno == SOCKERRID(INTR)), buf, (buf, "threaded_wt_write_wait:select failed, errno=%d=%s", errno, strerror(errno)));

      *sure = ilu_FALSE;

      DEBUG(LOCK_DEBUG, (stderr, "interrupted write wait (thread [%s%s]) on (fd [%d])\n", t1, t2, fd));

      ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, 0);
      return;
    }
  else 
    {
      *sure = ilu_FALSE;
      
      DEBUG(LOCK_DEBUG, (stderr, "write wait timeout (thread [%s%s]) on (fd [%d])\n", t1, t2, fd));

      ILU_CLER(*err);
      return;
    }
}

ilu_WaitTech threaded_wt = { threaded_wt_read_wait, threaded_wt_write_wait };


