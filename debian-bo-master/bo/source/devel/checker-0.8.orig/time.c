/*  time.c  Just print the current date.  */

#include <time.h>
#include <sys/time.h>
#include "checker.h"

#define SECSPERMIN	60
#define MINSPERHOUR	60
#define HOURSPERDAY	24
#define DAYSPERWEEK	7
#define DAYSPERNYEAR	365
#define DAYSPERLYEAR	366
#define SECSPERHOUR	(SECSPERMIN * MINSPERHOUR)
#define SECSPERDAY	((long) SECSPERHOUR * HOURSPERDAY)
#define MONSPERYEAR	12

#define TM_SUNDAY	0
#define TM_MONDAY	1
#define TM_TUESDAY	2
#define TM_WEDNESDAY	3
#define TM_THURSDAY	4
#define TM_FRIDAY	5
#define TM_SATURDAY	6

#define TM_YEAR_BASE	1900

#define EPOCH_YEAR	1970
#define EPOCH_WDAY	TM_THURSDAY

/*
** Accurate only for the past couple of centuries;
** that will probably do.
*/

#define isleap(y) ((((y) % 4) == 0 && ((y) % 100) != 0) || ((y) % 400) == 0)

static const int mon_lengths[2][MONSPERYEAR] = {
  { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
  { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
};

static const int year_lengths[2] = { DAYSPERNYEAR, DAYSPERLYEAR };

static const char wday_name[DAYSPERWEEK][4] = {
  "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};

static const char mon_name[MONSPERYEAR][4] = {
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

void
disp_date (void)
{
 long days;
 long rem;
 const int *ip;
 int y;
 int yleap;
 struct timeval tv;
 struct timezone tz;
 struct tm tmp;
 
 if (gettimeofday (&tv, &tz) != 0)
   {
     chkr_printf ("(date unknown)");
     return;
   }

 days = tv.tv_sec / SECSPERDAY;
 rem = tv.tv_sec % SECSPERDAY;
 rem += tz.tz_minuteswest * SECSPERMIN;
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
 tmp.tm_hour = (int) (rem / SECSPERHOUR);
 rem = rem % SECSPERHOUR;
 tmp.tm_min = (int) (rem / SECSPERMIN);
 tmp.tm_sec = (int) (rem % SECSPERMIN);
 tmp.tm_wday = (int) ((EPOCH_WDAY + days) % DAYSPERWEEK);
 if (tmp.tm_wday < 0)
   tmp.tm_wday += DAYSPERWEEK;
 y = EPOCH_YEAR;
 if (days >= 0)
   for ( ; ; )
     {
       yleap = isleap (y);
       if (days < (long) year_lengths[yleap])
         break;
       ++y;
       days = days - (long) year_lengths[yleap];
     }
  else 
    do
      {
        --y;
        yleap = isleap(y);
        days = days + (long) year_lengths[yleap];
      }
    while (days < 0);
 tmp.tm_year = y - TM_YEAR_BASE;
 ip = mon_lengths[yleap];
 for (tmp.tm_mon = 0; days >= (long) ip[tmp.tm_mon]; ++(tmp.tm_mon))
   days = days - (long) ip[tmp.tm_mon];
 tmp.tm_mday = (int) (days + 1);
 chkr_printf ("(%s %s%3d %02d:%02d:%02d %d)",
		wday_name[tmp.tm_wday],
		mon_name[tmp.tm_mon],
		tmp.tm_mday, tmp.tm_hour,
		tmp.tm_min, tmp.tm_sec,
		TM_YEAR_BASE + tmp.tm_year);
}
