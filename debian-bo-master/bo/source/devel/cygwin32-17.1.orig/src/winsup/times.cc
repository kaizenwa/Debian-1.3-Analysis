/* times stuff for WIN32.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <sys/times.h>
#include <utime.h>
#include <stdio.h>
#include "winsup.h"

#define FACTOR (0x19db1ded53ea710LL)
#define NSPERSEC 10000000

static long long
__to_clock_t (FILETIME * src)
{
  long long total = ((long long) src->dwHighDateTime << 32) + ((unsigned)src->dwLowDateTime);

  /* Convert into clock ticks - the total is in 10ths of a usec.  */
  total -= FACTOR;
 
  return total / (long long)(NSPERSEC / CLOCKS_PER_SEC);
}

clock_t
times (struct tms * buf)
{
  FILETIME creation, exit, kernel, user;

  DWORD ticks = GetTickCount ();
  GetProcessTimes (GetCurrentProcess (), &creation, &exit, &kernel, &user);

  buf->tms_stime = __to_clock_t (&kernel);
  buf->tms_utime = __to_clock_t (&user);
  buf->tms_cstime = 0;
  buf->tms_cutime = 0;

  /* Ticks is in milliseconds, convert to our ticks. Use long long to prevent
     overflow. */
   return (clock_t)((long long)ticks * CLOCKS_PER_SEC / 1000);
}

clock_t
_times (struct tms * buf)
{
  return times (buf);
}

int
settimeofday (const struct timeval *, const struct timezone *)
{
  set_errno (EPERM);
  return -1;
}

char *
timezone ()
{
  static char b[20];
  TIME_ZONE_INFORMATION tz;
  GetTimeZoneInformation (&tz);
  sprintf (b,"GMT-%d:00", (int)(tz.Bias / 60));
  return b;
}

void
totimeval (struct timeval *dst, FILETIME *src, int sub)
{
  long long x = __to_clock_t (src);

  x *= (int)(1e6) / CLOCKS_PER_SEC; /* Turn x into usecs */
  x -= (long long)sub * CLOCKS_PER_SEC; 

  dst->tv_usec = x % (long long)(1e6); /* And split */
  dst->tv_sec = x / (long long)(1e6);
}

int
gettimeofday (struct timeval *p, struct timezone *z)
{
  SYSTEMTIME t;
  FILETIME f;
  TIME_ZONE_INFORMATION tz;
  GetTimeZoneInformation (&tz);

  GetSystemTime (&t);
  /* fixme !! ignores tz */
  SystemTimeToFileTime (&t, &f);

  if (p)  
    {
      totimeval (p, &f, tz.Bias * 60);
    }
  if (z)
    {
      z->tz_minuteswest = tz.Bias;
      z->tz_dsttime = tz.StandardBias != tz.Bias;
    }
  return 1;
}

int
_gettimeofday (struct timeval *p, struct timezone *z) 
{
  return gettimeofday (p, z);
}

#if 0
/* Work out magic constant below */
genf ()
{
  SYSTEMTIME s;
  FILETIME f;
  s.wYear = 1970;
  s.wMonth = 1;
  s.wDayOfWeek = 5;
  s.wDay = 1;
  s.wHour = 0;
  s.wMinute = 0;
  s.wSecond = 0;
  s.wMilliseconds = 1;
  SystemTimeToFileTime (&s, &f);

  small_printf ("FILE TIME is %08x%08x\n",
	       f.dwHighDateTime,
	       f.dwLowDateTime);

}
#endif

void
time_t_to_filetime (time_t time, FILETIME *out)
{
  long long x= time * NSPERSEC + FACTOR;
  out->dwHighDateTime = x >> 32;
  out->dwLowDateTime = x;
}

static void
timeval_to_filetime (timeval *time, FILETIME *out)
{
  long long x= time->tv_sec * NSPERSEC + time->tv_usec * (NSPERSEC/1000000) + FACTOR;
  out->dwHighDateTime = x >> 32;
  out->dwLowDateTime = x;
}

static timeval
time_t_to_timeval (time_t in)
{
  timeval res;
  res.tv_sec = in;
  res.tv_usec = 0;
  return res;
}

long
to_time_t (FILETIME *ptr)
{
  /* A file time is the number of 100ns since jan 1 1601
     stuffed into two long words.
     A time_t is the number of seconds since jan 1 1970.  */

  long long x = ((long long) ptr->dwHighDateTime << 32) + ((unsigned)ptr->dwLowDateTime);
  x -= FACTOR;			/* number of 100ns between 1601 and 1970 */
  x /= NSPERSEC;		/* number of 100ns in a second */
  return x;
}

/* Return number of seconds since the first midnight, jan1, 1970 */
time_t
time (time_t * ptr)
{
  time_t res;
  SYSTEMTIME systemtime;
  FILETIME filetime;

  GetSystemTime (&systemtime);
  SystemTimeToFileTime (&systemtime, &filetime);
  res = to_time_t (&filetime);
  if (ptr)
    *ptr = res;
  return res;
}

/*
 * localtime_r.c
 * Original Author:	Adapted from tzcode maintained by Arthur David Olson.
 *
 * Converts the calendar time pointed to by tim_p into a broken-down time
 * expressed as local time. Returns a pointer to a structure containing the
 * broken-down time.
 */

#define SECSPERMIN	60
#define MINSPERHOUR	60
#define HOURSPERDAY	24
#define SECSPERHOUR	(SECSPERMIN * MINSPERHOUR)
#define SECSPERDAY	(SECSPERHOUR * HOURSPERDAY)
#define DAYSPERWEEK	7
#define MONSPERYEAR	12

#define YEAR_BASE	1900
#define EPOCH_YEAR      1970
#define EPOCH_WDAY      4

#define isleap(y) ((((y) % 4) == 0 && ((y) % 100) != 0) || ((y) % 400) == 0)

static _CONST int mon_lengths[2][MONSPERYEAR] = {
  {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
  {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
};

static _CONST int year_lengths[2] = {
  365,
  366
};

static struct tm *
corelocaltime (const time_t * tim_p)
{
  long days, rem;
  int y;
  int yleap;
  _CONST int *ip;
  static struct tm localtime_buf;

  time_t tim = *tim_p;
  struct tm *res = &localtime_buf;

  days = ((long) tim) / SECSPERDAY;
  rem = ((long) tim) % SECSPERDAY;
  
  while (rem < 0) 
    {
      rem += SECSPERDAY;
      --days;
    }
  while (rem >= SECSPERDAY)
    {
      rem -= SECSPERDAY;
      ++days;
    }
 
  /* compute hour, min, and sec */  
  res->tm_hour = (int) (rem / SECSPERHOUR);
  rem %= SECSPERHOUR;
  res->tm_min = (int) (rem / SECSPERMIN);
  res->tm_sec = (int) (rem % SECSPERMIN);

  /* compute day of week */
  if ((res->tm_wday = ((EPOCH_WDAY + days) % DAYSPERWEEK)) < 0)
    res->tm_wday += DAYSPERWEEK;

  /* compute year & day of year */
  y = EPOCH_YEAR;
  if (days >= 0)
    {
      for (;;)
	{
	  yleap = isleap (y);
	  if (days < year_lengths[yleap])
	    break;
	  y++;
	  days -= year_lengths[yleap];
	}
    }
  else
    {
      do
	{
	  --y;
	  yleap = isleap (y);
	  days += year_lengths[yleap];
	} while (days < 0);
    }

  res->tm_year = y - YEAR_BASE;
  res->tm_yday = days;
  ip = mon_lengths[yleap];
  for (res->tm_mon = 0; days >= ip[res->tm_mon]; ++res->tm_mon)
    days -= ip[res->tm_mon];
  res->tm_mday = days + 1;

  /* set daylight saving time flag */
  res->tm_isdst = -1;
  
  return (res);
}

struct tm *
gmtime (const time_t *tim_p)
{
  time_t tim = *tim_p;
  struct tm* rtm;

  TIME_ZONE_INFORMATION tz;
  GetTimeZoneInformation (&tz);
  tim -= tz.Bias * SECSPERMIN;

  rtm = corelocaltime (&tim);

  {
#define MON_MAX 32
    int toy_tim = MON_MAX*rtm->tm_mon + rtm->tm_mday;
    int toy_dst_start = MON_MAX*tz.DaylightDate.wMonth+tz.DaylightDate.wDay;
    int toy_dst_stop = MON_MAX*tz.StandardDate.wMonth+tz.StandardDate.wDay;
    if ((toy_tim > toy_dst_start) && (toy_tim < toy_dst_stop)) {
      tim -= tz.DaylightBias * 60;
      rtm = corelocaltime (&tim);
    }
  }
  return rtm;
}

struct tm *
localtime (const time_t *tim_p)
{
  time_t tim = *tim_p;

  return (corelocaltime (&tim));
}

#if 0
static void dump_filetime (FILETIME t)
{
  SYSTEMTIME s;
  FileTimeToSystemTime (&t, &s);
  small_printf ("filetime %08x%08x: ", t.dwHighDateTime,
	       t.dwLowDateTime);

  small_printf ("year %d, month %d, dayofweek %d, day %d, hour %d, minute %d, second %d, mili %d\n",
	     
	       s.wYear,
	       s.wMonth,
	       s.wDayOfWeek,
	       s.wDay,
	       s.wHour,
	       s.wMinute,
	       s.wSecond,
	       s.wMilliseconds);

}
#endif

int
utimes (const char *path, struct timeval *tvp)
{
  int res;
  struct timeval tmp[2];
  SECURITY_ATTRIBUTES sa;
  sa.nLength = sizeof (sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = 0;

  HANDLE h = CreateFileA (path_conv (path).get_win32 (),
			  GENERIC_WRITE,
			  FILE_SHARE_READ | FILE_SHARE_WRITE,
			  &sa,
			  OPEN_EXISTING, 
			  FILE_ATTRIBUTE_NORMAL, 0);

  if (h == INVALID_HANDLE_VALUE)
    {
      res = -1;
      __seterrno ();
    }
  else
    {
      if (tvp == 0)
	{
	  gettimeofday (&tmp[0], 0);
	  tmp[1] = tmp[0];
	  tvp = tmp;
	}

      FILETIME lastaccess;
      FILETIME lastwrite;

      timeval_to_filetime (tvp + 0, &lastaccess);
      timeval_to_filetime (tvp + 1, &lastwrite);

      debug_printf ("incoming lastaccess %08x %08x\n",
		   tvp->tv_sec,
		   tvp->tv_usec);

//      dump_filetime (lastaccess);
//      dump_filetime (lastwrite);

      if (!SetFileTime (h, 0, &lastaccess, &lastwrite))
	{
	  __seterrno ();
	  res = -1;
	}
      else
	res = 0;
      CloseHandle (h);
    }

  syscall_printf ("%d = utimes (%s, %x); (h%d)\n", 
		  res, path, tvp, h);
  return 0;
  return res;
}

int
utime (const char *path, struct utimbuf *buf)
{
  struct timeval tmp[2];

  if (buf == 0)
    return utimes (path, 0);

  debug_printf ("incoming utime act %x\n", buf->actime);
  tmp[0] = time_t_to_timeval (buf->actime);
  tmp[1] = time_t_to_timeval (buf->modtime);
  return utimes (path, tmp);
}
