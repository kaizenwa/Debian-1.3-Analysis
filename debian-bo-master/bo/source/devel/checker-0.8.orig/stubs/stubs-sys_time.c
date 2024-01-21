/* Checker stubs for functions defined in sys/time.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "available-stubs.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#include "checker_api.h"

#undef HAVE_adjtime
#undef HAVE_utimes

#if 0
#define HAVE_select
#define HAVE_FD_SET
#define HAVE_FD_CLR
#define HAVE_FD_ISSET
#define HAVE_FD_ZERO
#endif

#ifdef HAVE_chkr_func
void
stubs_chkr_check_timeval (const struct timeval *tv, int right)
{
  stubs_chkr_check_addr (&tv->tv_sec, sizeof (tv->tv_sec), right, "tv->tv_sec");
  stubs_chkr_check_addr (&tv->tv_usec, sizeof (tv->tv_usec), right, "tv->tv_usec");
}

void
stubs_chkr_set_right_timeval (struct timeval *tv, int right)
{
  stubs_chkr_set_right (&tv->tv_sec, sizeof (tv->tv_sec), right);
  stubs_chkr_set_right (&tv->tv_usec, sizeof (tv->tv_usec), right);
}
#else
void stubs_chkr_check_timeval (const struct timeval *tv, int right);
void stubs_chkr_set_right_timeval (struct timeval *tv, int right);
#endif

/* compiled from: . */
#ifdef HAVE_gettimeofday
/* From `/usr/include/sys/time.h:40'.  */
int
chkr$gettimeofday (struct timeval *tv, struct timezone *tz)
{
  int res;
  
  if (tv)
    stubs_chkr_check_timeval (tv, CHKR_TW);
  if (tz)
    {
      stubs_chkr_check_addr (&tz->tz_minuteswest, sizeof (tz->tz_minuteswest), CHKR_TW, "tz->tz_minuteswest");
      stubs_chkr_check_addr (&tz->tz_dsttime, sizeof (tz->tz_dsttime), CHKR_TW, "tz->tz_dsttime");
    }
  res = gettimeofday (tv, tz);
  if (res == 0)
    {
      if (tv)
        stubs_chkr_set_right_timeval (tv, CHKR_RW);
      if (tz)
        {
          stubs_chkr_set_right (&tz->tz_minuteswest, sizeof (tz->tz_minuteswest), CHKR_RW);
          stubs_chkr_set_right (&tz->tz_dsttime, sizeof (tz->tz_dsttime), CHKR_RW);
        }
    }
  return res;
}
#endif /* HAVE_gettimeofday */

#ifdef HAVE_settimeofday
/* From `/usr/include/sys/time.h:44'.  */
int
chkr$settimeofday (const struct timeval *tv, const struct timezone *tz)
{
  if (tv)
    stubs_chkr_check_timeval (tv, CHKR_RO);
  if (tz)
    {
      stubs_chkr_check_addr (&tz->tz_minuteswest, sizeof (tz->tz_minuteswest), CHKR_RO, "tz->tz_minutewest");
      stubs_chkr_check_addr (&tz->tz_dsttime, sizeof (tz->tz_dsttime), CHKR_RO, "tz->tz_dsttime");
    }
#if USE_BI_JUMP
  __builtin_jump (settimeofday);
#else
  return settimeofday (tv, tz);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_settimeofday */

#ifdef HAVE_select
/* From `/usr/include/sys/time.h:51'.  */
int
chkr$select (int numfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout)
{
  fd_set fd_used;
  int i;

  /* Only one warning by fd ! */
  FD_ZERO (&fd_used);

  /* Only the first NBR_FD are used */
  numfds = numfds > FD_SETSIZE ? FD_SETSIZE : numfds;

  /* | could replace FD_SET, but the later one is more portable */
  if (readfds)
    {
      stubs_chkr_check_addr (readfds, sizeof (fd_set), CHKR_RW, "readfds");
      for (i = 0; i < numfds; i++)
        if (FD_ISSET (i, readfds))
          FD_SET (i, &fd_used);
    }
    
  if (writefds)
    {
      stubs_chkr_check_addr (writefds, sizeof (fd_set), CHKR_RW, "writefds");
      for (i = 0; i < numfds; i++)
        if (FD_ISSET (i, writefds))
          FD_SET (i, &fd_used);
    }
    
  if (exceptfds)
    {
      stubs_chkr_check_addr (exceptfds, sizeof (fd_set), CHKR_RW, "exceptfds");
      for (i = 0; i < numfds; i++)
        if (FD_ISSET (i, exceptfds))
          FD_SET (i, &fd_used);
    }

  if (timeout)
    stubs_chkr_check_timeval (timeout, CHKR_RW);
	  
  /* Now we can check each fd */
  for (i = 0; i < numfds; i++)
    if (FD_ISSET (i, &fd_used))
      if (!fd_used_by_prog (i))
        {
	  /* This fd is reserved by the system, so remove it from 
	   * the lists. */
	  if (readfds)
	    FD_CLR (i, readfds);
	  if (writefds)
	    FD_CLR (i, writefds);
	  if (exceptfds)
	    FD_CLR (i, exceptfds);
	}
	
#if USE_BI_JUMP
  __builtin_jump (select);
#else
  return select (numfds, readfds, writefds, exceptfds, timeout);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_select */

#ifdef HAVE_getitimer
/* From `/usr/include/sys/time.h:56'.  */
int
chkr$getitimer (int which, struct itimerval *value)
{
  int res;
  
  stubs_chkr_check_timeval (&value->it_interval, CHKR_TW);
  stubs_chkr_check_timeval (&value->it_value, CHKR_TW);
  res = getitimer (which, value);
  if (res == 0)
    {
      stubs_chkr_set_right_timeval (&value->it_interval, CHKR_RW);
      stubs_chkr_set_right_timeval (&value->it_value, CHKR_RW);
    }
  return res;
}
#endif /* HAVE_getitimer */

#ifdef HAVE_setitimer
/* From `/usr/include/sys/time.h:62'.  */
int
chkr$setitimer (int which, const struct itimerval *value, struct itimerval *ovalue)
{
  int res;
  
  stubs_chkr_check_timeval (&ovalue->it_interval, CHKR_TW);
  stubs_chkr_check_timeval (&ovalue->it_value, CHKR_TW);
  stubs_chkr_check_timeval (&value->it_interval, CHKR_TW);
  stubs_chkr_check_timeval (&value->it_value, CHKR_TW);
  
  res = setitimer (which, value, ovalue);
  if (res == 0)
    {
      stubs_chkr_set_right_timeval (&ovalue->it_interval, CHKR_RW);
      stubs_chkr_set_right_timeval (&ovalue->it_value, CHKR_RW);
    }
  return res;
}
#endif /* HAVE_setitimer */

#ifdef HAVE_adjtime
/* From `/usr/include/sys/time.h:67'.  */
int
chkr$adjtime (struct timeval * arg0, struct timeval * arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct timeval), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (struct timeval), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (adjtime);
#else
  {
    int res;
    res = adjtime (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_adjtime */

#ifdef HAVE_utimes
/* From `/usr/include/sys/time.h:70'.  */
int
chkr$utimes (char * arg0, struct timeval * arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (struct timeval), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (utimes);
#else
  {
    int res;
    res = utimes (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_utimes */

#ifdef HAVE_FD_CLR
void          
chkr$FD_CLR (int fd, fd_set *set)
{
  stubs_chkr_check_addr (set, sizeof (fd_set), CHKR_RW, "set");
  FD_CLR (fd, set);
}
#endif /* HAVE_FD_CLR */

#ifdef HAVE_FD_ISSET
int          
chkr$FD_ISSET (int fd, fd_set *set)
{
  stubs_chkr_check_addr (set, sizeof (fd_set), CHKR_RO, "set");
  return FD_ISSET (fd, set);
}
#endif /* HAVE_FD_CLR */

#ifdef HAVE_FD_SET
void          
chkr$FD_SET (int fd, fd_set *set)
{
  stubs_chkr_check_addr (set, sizeof (fd_set), CHKR_RW, "set");
  FD_SET (fd, set);
}
#endif /* HAVE_FD_SET */

#ifdef HAVE_FD_ZERO
void          
chkr$FD_ZERO (fd_set *set)
{
  stubs_chkr_set_right (set, sizeof (fd_set), CHKR_RW);
  FD_ZERO (set);
}
#endif /* HAVE_FD_ZERO */

#endif /* HAVE_SYS_TIME_H */
