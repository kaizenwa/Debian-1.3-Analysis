/* Copyright (C) 1991, 1992, 1993, 1994 Free Software Foundation, Inc.
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

/* Mitch DSouza - m.dsouza@mrc-apu.cam.ac.uk - Tue Sep 13 22:16:56  1994.

   This conforms to XOpen as checked by Patrick D'Cruze. Some extra comments:

	XPG4 say that %a = %A = the locale's full or abbreviated weekday name.
	However GNU defines it so that:
		a = the locale's abbreviated weekday name
		A = the locale's full weekday name
	This matches the equivalent functions in strftime.c.

	Also XPG4 simply states that %b = %h = %B = the locale's full or
	abbreviated month name. In strftime.c these options are defined as:
		b = h = the locale's abbreviated month name
		B = the locale's full month name

   The coding below caters for both standards.

   Please test the following with (for example)
	gcc -DDEBUG strptime.c
	./a.out "%x %X %A %b" "%x %X %a %B"
 */

#include <ansidecl.h>
#include <localeinfo.h>
#include <ctype.h>
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if !defined(HAVE_GNU_LD) && !defined (__ELF__)
#define	__tzname	tzname
#define	__daylight	daylight
#define	__timezone	timezone
#endif

#define PERCENT			case '%'
#define DAY_OF_WEEK_NAME	case 'a':\
				case 'A'	/* locale week name */
#define MONTH_NAME		case 'b':\
				case 'B':\
				case 'h'	/* locale month name */
#define DATE_AND_TIME		case 'c'	/* as in %x %X */
#define DATE_AND_TIME_LOCALE	case 'C'	/* locale format */
#define DAY_OF_MONTH		case 'd':\
				case 'e'	/* 01-31 */
#define DATE_IN_SLASHES		case 'D'	/* e.g. %m/%d/%y */
#define HOUR_IN_24		case 'H':\
				case 'k'	/* 00-23 */
#define HOUR_IN_12		case 'I':\
				case 'i'	/* 01-12 */
#define DAY_IN_YEAR		case 'j'	/* 001-366 */
#define MONTH_NUMBER		case 'm'	/* 01-12 */
#define MINUTE			case 'M'	/* 00-59 */
#define AM_PM			case 'p'	/* locale format for AM/PM */
#define TIME_WITH_COLONS	case 'r'	/* e.g. %I:%M:%S %p */
#define HR_MIN_WITH_COLONS	case 'R'	/* %H:%M */
#define SECONDS			case 'S'	/* 00-61 - leap sec */
#define TIME_WITH_COLONS_24HR	case 'T'	/* %H:%M:%S */
#define WEEK_NUM		case 'U'	/* week number of the year */
#define WEEK_DAY_NUM		case 'w'	/* weekday number [0, 6] with
						   Sunday as the first day of
						   the week */
#define WEEK_DAY_NUM_2		case 'W'	/* as %w except Monday is the
						   first day of the week */
#define DATE_LOCALE_FORMAT	case 'x'	/* locale format for date */
#define TIME_LOCALE_FORMAT	case 'X'	/* locale format for time */
#define YEAR_IN_CENTURY		case 'y'	/* 00-99 - unfortunately
						   assumes that we are stuck
						   in the 20th century */
#define YEAR_WITH_CENTURY	case 'Y'	/* e.g. 1994 - expects 1900 */

static char *
next_non_digit (char *p)
{
  while (p && isdigit (*p))
    p++;
  return p;
}

char *
DEFUN (strptime, (buf, format, tm),
       char *buf AND CONST char *format AND register struct tm *tm)
{
  const char *pf = format;	/* pointer to the format pointer */
  char *pb = buf;		/* pointer to the buffer pointer */
  char *p;
  int len,
    found,
    i;

  if (!pf || !pb)
    return NULL;

  for (; *pf && *pb; pf++) {

    if (*pf == *pb) {
      pb++;
      continue;
    }
    if (*pf != '%')
      continue;

    switch (*++pf) {

    PERCENT:
      break;

    DAY_OF_WEEK_NAME:
      for (i = 0; i < 7; i++)
	if (found = !strncasecmp (pb,
			      _time_info->full_wkday[i],
			      len = strlen (_time_info->full_wkday[i])))
	  break;
      if (!found)
	for (i = 0; i < 7; i++)
	  if (found = !strncasecmp (pb,
				_time_info->abbrev_wkday[i],
				len = strlen (_time_info->abbrev_wkday[i])))
	    break;
      if (found) {
	pb += len;
	tm->tm_wday = i;
      }
      break;

    MONTH_NAME:
      for (i = 0; i < 12; i++)
	if (found = !strncasecmp (pb,
			      _time_info->full_month[i],
			      len = strlen (_time_info->full_month[i])))
	  break;
      if (!found)
	for (i = 0; i < 12; i++)
	  if (found = !strncasecmp (pb,
				_time_info->abbrev_month[i],
				len = strlen (_time_info->abbrev_month[i])))
	    break;
      if (found) {
	pb += len;
	tm->tm_mon = i;
      }
      break;

    DATE_AND_TIME:
      p = strptime (pb, _time_info->date_time, tm);
      pb += p - pb;
      break;

    DATE_AND_TIME_LOCALE:
      p = strptime (pb, _time_info->date_time, tm);
      pb += p - pb;
      break;

    DAY_OF_MONTH:
      p = next_non_digit (pb);
      sscanf (pb, "%.2d", &tm->tm_mday);
      pb += p - pb;
      break;

    DATE_IN_SLASHES:
      p = strptime (pb, "%m/%d/%y", tm);
      pb += p - pb;
      break;

    HOUR_IN_24:
      p = next_non_digit (pb);
      sscanf (pb, "%.2d", &tm->tm_hour);
      pb += p - pb;
      break;

    HOUR_IN_12:
      p = next_non_digit (pb);
      sscanf (pb, "%.2d", &tm->tm_hour);
      tm->tm_hour += 12;
      pb += p - pb;
      break;

    DAY_IN_YEAR:
      p = next_non_digit (pb);
      sscanf (pb, "%.3d", &tm->tm_yday);
      if (tm->tm_yday)
	tm->tm_yday--;
      pb += p - pb;
      break;

    MONTH_NUMBER:
      p = next_non_digit (pb);
      sscanf (pb, "%.2d", &tm->tm_mon);
      if (tm->tm_mon)
	tm->tm_mon--;
      pb += p - pb;
      break;

    MINUTE:
      p = next_non_digit (pb);
      sscanf (pb, "%.2d", &tm->tm_min);
      pb += p - pb;
      break;

    AM_PM:
      if (!strncmp (pb, _time_info->ampm[0],
		    len = strlen (_time_info->ampm[0])))
	pb += len;
      else if (!strncmp (pb, _time_info->ampm[1],
			 len = strlen (_time_info->ampm[1])))
	pb += len;
      break;

    TIME_WITH_COLONS:
      p = strptime (pb, "%I:%M:%S %p", tm);
      pb += p - pb;
      break;

    HR_MIN_WITH_COLONS:
      p = strptime (pb, "%H:%M", tm);
      pb += p - pb;
      break;

    SECONDS:
      p = next_non_digit (pb);
      sscanf (pb, "%.2d", &tm->tm_sec);
      pb += p - pb;
      break;

    TIME_WITH_COLONS_24HR:
      p = strptime (pb, "%H:%M:%S", tm);
      pb += p - pb;
      break;

    DATE_LOCALE_FORMAT:
      p = strptime (pb, _time_info->date, tm);
      pb += p - pb;
      break;

    TIME_LOCALE_FORMAT:
      p = strptime (pb, _time_info->time, tm);
      pb += p - pb;
      break;

    WEEK_DAY_NUM:
      p = next_non_digit (pb);
      sscanf (pb, "%.2d", &tm->tm_wday);
      pb += p - pb;
      break;

    YEAR_IN_CENTURY:
      p = next_non_digit (pb);
      sscanf (pb, "%.2d", &tm->tm_year);
      pb += p - pb;
      break;

    YEAR_WITH_CENTURY:
      p = next_non_digit (pb);
      sscanf (pb, "%.4d", &tm->tm_year);
      tm->tm_year -= 1900;
      pb += p - pb;
      break;

    default: /* silent failure */
#ifdef DEBUG
      fprintf (stderr, "%s: unknown format specifier `%%%c'\n", __FILE__, *pf);
#endif
    }

  }
  return pb;
}

#ifdef DEBUG
main (int argc, char **argv)
{
  struct tm *tm1,
    tm2;
  char buf[1024];
  char format[1024];
  int len = 1023,
    err = 0;
  time_t now;

  if (argv[1])
    strcpy (format, argv[1]);
  else
    strcpy (format, "%I:%M:%S %p %m/%d/%y %a %b %y %j");

  time (&now);
  tm1 = localtime (&now);
  strftime (buf, len, format, tm1);
  printf ("Start executing strftime(buf,\"%s\",tm)\n", format);
  printf ("\treturned buf=\"%s\" \n",buf);

  if (argv[2])
    strcpy (format, argv[2]);

  printf ("Now executing strptime(\"%s\",\"%s\",tm)\n",buf,format);
  bzero (&tm2, sizeof (struct tm));
  strptime (buf, format, &tm2);
  if (tm1->tm_sec != tm2.tm_sec)
    err++, printf ("\ttm_sec is different (%d - %d)\n", tm1->tm_sec, tm2.tm_sec);
  if (tm1->tm_min != tm2.tm_min)
    err++, printf ("\ttm_min is different (%d - %d)\n", tm1->tm_min, tm2.tm_min);
  if (tm1->tm_hour != tm2.tm_hour)
    err++, printf ("\ttm_hour is different (%d - %d)\n", tm1->tm_hour, tm2.tm_hour);
  if (tm1->tm_mday != tm2.tm_mday)
    err++, printf ("\ttm_mday is different (%d - %d)\n", tm1->tm_mday, tm2.tm_mday);
  if (tm1->tm_mon != tm2.tm_mon)
    err++, printf ("\ttm_mon is different (%d - %d)\n", tm1->tm_mon, tm2.tm_mon);
  if (tm1->tm_year != tm2.tm_year)
    err++, printf ("\ttm_year is different (%d - %d)\n", tm1->tm_year, tm2.tm_year);
  if (tm1->tm_wday != tm2.tm_wday)
    err++, printf ("\ttm_wday is different (%d - %d)\n", tm1->tm_wday, tm2.tm_wday);
  if (tm1->tm_yday != tm2.tm_yday)
    err++, printf ("\ttm_yday is different (%d - %d)\n", tm1->tm_yday, tm2.tm_yday);
  if (tm1->tm_isdst != tm2.tm_isdst)
    err++, printf ("\ttm_isdst is different (%d - %d)\n", tm1->tm_isdst, tm2.tm_isdst);

  if (!err)
    printf ("Ok: time tm struct has been filled incorrectly\n");
  else
    printf ("%d out of %d fields filled correctly\n", 8 - err, 8);
}
#endif
