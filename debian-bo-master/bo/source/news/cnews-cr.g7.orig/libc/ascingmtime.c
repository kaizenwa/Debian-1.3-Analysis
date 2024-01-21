/*
 * ascingmtime - convert GMT to ascii Internet format
 */

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>

#define HIGH(nn) ((nn) / 10)
#define LOW(nn)  ((nn) % 10)

char *
ascingmtime(tm)
register struct tm *tm;			/* better be GMT */
{
	static char chtime[128];
	static char *days[] =
		{ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
	static char *months[] = {
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
	};

	(void) sprintf(chtime, "%s, %d %s %d %d%d:%d%d:%d%d GMT\n",
		days[tm->tm_wday],
		tm->tm_mday, months[tm->tm_mon], tm->tm_year + 1900,
		HIGH(tm->tm_hour), LOW(tm->tm_hour),
		HIGH(tm->tm_min),  LOW(tm->tm_min),
		HIGH(tm->tm_sec),  LOW(tm->tm_sec));
	return chtime;
}
