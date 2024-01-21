/* Checker stubs for functions defined in time.h
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

#ifdef HAVE_TIME_H
#include <time.h>
#include "checker_api.h"

#undef HAVE_strptime
#undef HAVE_timegm
#undef HAVE_timelocal

#if 0
#define HAVE_stime
#define HAVE_clock
#define HAVE_time
#define HAVE_difftime
#define HAVE_mktime
#define HAVE_asctime
#define HAVE_ctime
#define HAVE_strftime
#define HAVE_tzset
#define HAVE_gmtime
#define HAVE_localtime
#endif

#ifdef HAVE_mktime
void
check_struct_tm (const struct tm *p, int mode, int all)
{
  stubs_chkr_check_addr (&(p->tm_sec), sizeof (int), mode, "tm->tm_sec");
  stubs_chkr_check_addr (&(p->tm_min), sizeof (int), mode, "tm->tm_min");
  stubs_chkr_check_addr (&(p->tm_hour), sizeof (int), mode, "tm->tm_hour");
  stubs_chkr_check_addr (&(p->tm_mday), sizeof (int), mode, "tm->tm_mday");
  stubs_chkr_check_addr (&(p->tm_mon), sizeof (int), mode, "tm->tm_mon");
  stubs_chkr_check_addr (&(p->tm_year), sizeof (int), mode, "tm->tm_year");
  if (all & 1)
    stubs_chkr_check_addr (&(p->tm_wday), sizeof (int), mode, "tm->tm_wday");
  if (all & 3)
    stubs_chkr_check_addr (&(p->tm_yday), sizeof (int), mode, "tm->tm_yday");
  stubs_chkr_check_addr (&(p->tm_isdst), sizeof (int), mode, "tm->tm_isdst");
}
#else
void check_struct_tm (const struct tm *p, int mode, int all);
#endif

/* compiled from: . */
#ifdef HAVE_stime
int
chkr$stime (time_t *t)
{
  stubs_chkr_check_addr (t, sizeof (time_t), CHKR_RO, "t");
#if USE_BI_JUMP
  __builtin_jump (stime);
#else
  return stime (t);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_stime */

#ifdef HAVE_clock
clock_t
chkr$clock (void)
{
#if USE_BI_JUMP
  __builtin_jump (clock);
#else
  return clock ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_clock */

#ifdef HAVE_time
time_t
chkr$time (time_t *t)
{
  if (t)
    stubs_chkr_check_addr (t, sizeof (time_t), CHKR_WO, "t");
#if USE_BI_JUMP
  __builtin_jump (time);
#else
  return time (t);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_time */

#ifdef HAVE_difftime
double
chkr$difftime (time_t arg0, time_t arg1)
{
#if USE_BI_JUMP
  __builtin_jump (difftime);
#else
  return difftime (arg0, arg1);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_difftime */

#ifdef HAVE_mktime
time_t
chkr$mktime (struct tm *timep)
{
  time_t res;
  check_struct_tm (timep, CHKR_RO, 0);
  res = mktime (timep);
  if (res != (time_t)-1)
    check_struct_tm (timep, CHKR_WO, 3);
  return res;
}
#endif /* HAVE_mktime */

#ifdef HAVE_asctime
char *
chkr$asctime (const struct tm *timep)
{
  check_struct_tm (timep, CHKR_RO, 1);
#if USE_BI_JUMP
  __builtin_jump (asctime);
#else
  return asctime (timep);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_asctime */

#ifdef HAVE_ctime
char *
chkr$ctime (const time_t *timer)
{
  stubs_chkr_check_addr (timer, sizeof (time_t), CHKR_RO, "timer");
#if USE_BI_JUMP
  __builtin_jump (ctime);
#else
  return ctime (timer);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ctime */

#ifdef HAVE_strftime
size_t
chkr$strftime (char *s, size_t maxsize, const char *format, const struct tm *timep)
{
  size_t res;
  stubs_chkr_check_str (format, CHKR_RO, "format");
  stubs_chkr_check_addr (s, maxsize, CHKR_MW, "s");
  check_struct_tm (timep, CHKR_RO, 0);	/* FIXME */
  res = strftime (s, maxsize, format, timep);
  if (res > 0)
    stubs_chkr_set_right (s, res + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_strftime */

#ifdef HAVE_strptime
char *
chkr$strptime (char * arg0, const char * arg1, struct tm * arg2)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX)
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX)
  stubs_chkr_check_addr (arg2, sizeof (struct tm), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (strptime);
#else
  {
    char * res;
    res = strptime (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strptime */

#ifdef HAVE_tzset
void
chkr$tzset (void)
{
#if USE_BI_JUMP
  __builtin_jump (tzset);
#else
  tzset ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tzset */

#ifdef HAVE_gmtime
struct tm *
chkr$gmtime (const time_t *timer)
{
  stubs_chkr_check_addr (timer, sizeof (time_t), CHKR_RO, "timer");
#if USE_BI_JUMP
  __builtin_jump (gmtime);
#else
  {
    struct tm * res;
    res = gmtime (timer);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_gmtime */

#ifdef HAVE_localtime
struct tm *
chkr$localtime (const time_t *timer)
{
  stubs_chkr_check_addr (timer, sizeof (time_t), CHKR_RO, "timer");
#if USE_BI_JUMP
  __builtin_jump (localtime);
#else
  {
    struct tm * res;
    res = localtime (timer);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_localtime */

#ifdef HAVE_timegm
time_t
chkr$timegm (struct tm * arg0)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct tm), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (timegm);
#else
  {
    time_t res;
    res = timegm (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_timegm */

#ifdef HAVE_timelocal
time_t
chkr$timelocal (struct tm * arg0)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct tm), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (timelocal);
#else
  {
    time_t res;
    res = timelocal (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_timelocal */

#endif /* HAVE_TIME_H */
