/* Copyright (C) 1991,1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Time and date routines for Extended Pascal   

The GNU Pascal Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU Pascal Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU Pascal Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include "rts.h"

/*
 * #define HAVE_NO_TIME is you have no time functions at all.
 */
 
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

/*
 * Extended Pascal routine:
 *
 * GetTimeStamp (VAR t);
 */
void
_p_gettimestamp (t)
struct GPC_TIMESTAMP *t;
{
#ifdef GPC_PACKED_STRUCTURES
    abort();
#endif

#ifdef HAVE_NO_TIME
    /* The values are specified in the standard, even if the Valid fields are False */
    t->Year  = 1;
    t->Month = 1;
    t->Day   = 1;
    t->Datevalid = 0; /* Date is invalid */
    t->Hour  = 0;
    t->Minute= 0;
    t->Second= 0;
    t->Timevalid = 0; /* So is the time */

#else /* Time functions are defined */

    int year;
    struct tm *gnu; /* Is it a trademark after all? */

#ifdef HAVE_GETIMEOFDAY
    struct timeval gmt;

    if (gettimeofday (&gmt, 0))
	_p_error (ABORT, "Can not read system time");

    gnu = localtime (&gmt.tv_sec); /* Convert time zones, dst etc & return struct tm */
#else
    time_t gmt;

    gmt =  time ((time_t *) 0);
    gnu = localtime (&gmt); /* Convert time zones, dst etc & return struct tm */
#endif

    year = gnu->tm_year;

    if (year < 100)
      year  += (year >= 93) ? 1900 : 2000;
    
    t->Year  = year;
    t->Month = gnu->tm_mon + 1; /* 1 == January */
    t->Day   = gnu->tm_mday;
    t->Datevalid = 1;
    t->Hour  = gnu->tm_hour;
    t->Minute= gnu->tm_min;
    t->Second= gnu->tm_sec;
    t->Timevalid = 1;
#endif /* HAVE_NO_TIME */
}

/*
 * current date in string format.
 *
 * @@@ This is not complete.
 * Should actually check that the day of the month is also valid.
 */
static char *month_names[12] = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

void
_p_date (t, buffer)
struct GPC_TIMESTAMP *t;
char *buffer;
{
  char Date[ GPC_DATE_LENGTH+1 ];

  if (! t->Datevalid
      || t->Month < 1 || t->Month > 12
      || t->Day < 1   || t->Day > 31)
    _p_error (ABORT, "Invalid date supplied to library function `Date'");

  (void) sprintf (Date, "%2d %s %4d", t->Day, month_names [ t->Month - 1 ], t->Year);

  (void) strncpy (buffer, Date, GPC_DATE_LENGTH);
}

/*
 * Return current time in a string
 * Length is implementation dependent.
 *
 * Caller gives the buffer that must be of correct length
 */
void
_p_time (t, buffer)
struct GPC_TIMESTAMP *t;
char *buffer;
{
  char Time[ GPC_TIME_LENGTH+1 ];

  if (! t->Timevalid
      || t->Hour < 0   || t->Hour > 23
      || t->Minute < 0 || t->Minute > 59
      || t->Second < 0 || t->Second > 59)
    _p_error (ABORT, "Invalid time supplied to library function `Time'");

  (void) sprintf (Time, "%02d:%02d:%02d",  t->Hour, t->Minute, t->Second);
  
  (void) strncpy (buffer, Time, GPC_TIME_LENGTH);
}
