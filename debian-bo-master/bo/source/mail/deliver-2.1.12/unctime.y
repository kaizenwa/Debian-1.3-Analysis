/*
 * $Id: unctime.y,v 1.7 1993/10/28 16:49:51 chip Exp $
 *
 * Conversion of ctime-style date string back to a time_t.
 * This module is in a different style from the rest of deliver
 * because Chip Salzenberg didn't write it.
 *
 * $Log: unctime.y,v $
 * Revision 1.7  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.6  1992/11/19  16:26:03  chip
 * Make yylex() and yyerror() public.
 *
 * Revision 1.5  1991/11/12  20:43:10  chip
 * Ignore return value of ftime().
 *
 * Revision 1.4  1991/08/26  17:41:21  chip
 * Prioritize methods for determining timezone.
 * Make correction, and related expressions, long.
 *
 * Revision 1.3  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.2  1991/05/23  17:23:19  chip
 * Follow RFC822 definition of header syntax.
 * Guard isxxx() macros against negative values.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

/* time_t
   unctime(s)
   char *s;

Convert s, which may be in almost any reasonable date format, to
a time_t integer suitable for consumption by ctime(3).  Coincidentally
destroys the contents of s.  Return -1 if s is not a recognizable legal date.

Any parts of the time left unspecified take on the current values.

"4 CST + 23[:10]" adds 23 minutes and optionally 10 seconds to the correction.
"# nnnnnn" forces exactly nnnnnn seconds GMT since Jan. 1, 1970.
  
Copyright 1988, Michael J. Haertel.  Use this routine at your own risk.
You may redistribute verbatim copies of this file.  You may redistribute
modified versions of this file so long as (1) you state who last changed
it, and (2) this copyright notice appears unmodified.

Some debugging by W. Anthony Smith.

Bug fix, minor enhancements, and non-BSD modifications by David MacKenzie.

Several changes by Chip Salzenberg for use with deliver:
    Include "deliver.h".
    Handle military timezones as per RFC822.
    Don't modify input string.
    Consider extra junk in date string an error.
    Indent so as to be readable to humans.
*/

%{
#include "deliver.h"
#include <ctype.h>

#ifdef HAS_TIMEZONE
# define USE_TIMEZONE
#else
# ifdef HAS_GETTOD
#  define USE_GETTOD
# else
#  define USE_FTIME
# endif
#endif

#ifdef USE_GETTOD
# include <sys/time.h>
#else
# include <time.h>
# ifdef USE_FTIME
#  include <sys/timeb.h>
# endif
#endif

/* Delta is correction, in minutes, to turn specified time into GMT. */
/* if (zoneflag), a timezone was explicitly specified. */
static year, month, day, hour, minute, second, delta;
static zoneflag, errorflag, iflag;
static long iresult;

#define YYSTYPE long
%}

%token NUM MONTH AM PM

%%

date:
  day time year
  | day year time
  | time day year
  | time day
  | day time
  | day year
  | day
  | time
  | '#' NUM		{ iflag = TRUE; iresult = $2; }
  ;			/* previous line forces exact time in seconds GMT */

day:
  NUM MONTH		{ month = $2; day = $1; }
  | MONTH NUM		{ month = $1; day = $2; }
  | NUM '/' NUM		{ month = $1; day = $3; }
  ;

year:
  ',' NUM		{ year = $2; }
  | '/' NUM		{ year = $2; }
  | NUM			{ year = $1; }
  ;

time:
  clock AM		{ hour %= 12; }
  | clock PM		{ hour = hour % 12 + 12; }
  | clock
  ;

clock:
  NUM ':' NUM ':' NUM	{ hour = $1; minute = $3; second = $5; }
  | NUM ':' NUM		{ hour = $1; minute = $3; }
  ;

%%

/* Return 0 if s is the same word as t.
   Return 1 if s is a prefix of t; e.g. prefix("mar", "march") returns 1.
   Return -1 if s is not a prefix of t.
   Note that comparison is case-insensitive. */

static int
wordeq(s, t)
char *s, *t;
{
    while (*s && *t)
    {
	int i, j;

	i = *s++ & 0xFF;
	j = *t++ & 0xFF;
	if (isupper(i))
	    i = tolower(i);
	if (isupper(j))
	    j = tolower(j);
	if (i != j)
	    return -1;
    }

    if (*s)
	return -1;
    return (*t == 0);
}

static char *lexptr;

static void
initlex(s)
char *s;
{
    lexptr = s;
}

static char *months[] =
{
    "jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec",
    0
};

struct zonename
{
    char *name;			/* Name of the time zone. */
    int delta;			/* Correction, in minutes, to add to GMT */
};

static struct zonename zones[] =
{
    "gmt", 0,
    "ut", 0,
    "est", -5 * 60,		/* North American time zones */
    "edt", -6 * 60,
    "cst", -6 * 60,
    "cdt", -7 * 60,
    "mst", -7 * 60,
    "mdt", -8 * 60,
    "pst", -8 * 60,
    "pdt", -9 * 60,
    "z", 0,			/* Military time zones */
    "a", -1 * 60,
    "b", -2 * 60,
    "c", -3 * 60,
    "d", -4 * 60,
    "e", -5 * 60,
    "f", -6 * 60,
    "g", -7 * 60,
    "h", -8 * 60,
    "i", -9 * 60,
    "k", -10 * 60,
    "l", -11 * 60,
    "m", -12 * 60,
    "n", 1 * 60,
    "o", 2 * 60,
    "p", 3 * 60,
    "q", 4 * 60,
    "r", 5 * 60,
    "s", 6 * 60,
    "t", 7 * 60,
    "u", 8 * 60,
    "v", 9 * 60,
    "w", 10 * 60,
    "x", 11 * 60,
    "y", 12 * 60,
    0, 0
};

/* Lexical analyzer.  Gather alphabetics into tokens; if they are unknown
   strings ignore them, and if they are months return the appropriate value.
   If the token is the name of the time zone set delta = correction and
   zoneflag = TRUE, and skip ahead to the next token (the parser itself
   never sees time zones).
   If the token is a number, return its value.
   If it is a punctuation mark, return the character code.
   Ignore white space.  */

int
yylex()
{
    register i;
    char token[40];		/* Probably paranoid. */

    for (;;)
    {
	while (isspace(*lexptr & 0xFF))
	    lexptr++;
	if (*lexptr == 0)
	    return 0;
	else if (isalpha(*lexptr & 0xFF))
	{
	    i = 0;
	    while (isalpha(*lexptr & 0xFF))
		token[i++] = *lexptr++;
	    token[i] = '\0';
	    for (i = 0; months[i]; i++)
	    {
		if (wordeq(months[i], token) >= 0)
		{
		    yylval = i + 1;
		    return MONTH;
		}
	    }
	    for (i = 0; zones[i].name; i++)
	    {
		if (wordeq(zones[i].name, token) == 0)
		{
		    int oper, next;

		    zoneflag = TRUE;
		    delta = zones[i].delta;
		    oper = yylex();
		    /* Syntax: "4 CST + 23[:10]" adds 23 minutes and
		    optionally 10 seconds to delta (the correction). */
		    if (oper == '+' || oper == '-')
		    {
			(void) yylex();
			delta += (oper == '+' ? 60 : -60) * yylval;
			next = yylex();
			if (next == ':')
			{
			    (void) yylex();
			    delta += (oper == '+' ? 1 : -1) * yylval;
			}
			else
			    return next;
		    }
		    else
			return oper;
		}
	    }
	    if (wordeq("pm", token) == 0 || wordeq("p.m.", token) == 0)
		return PM;
	    if (wordeq("am", token) == 0 || wordeq("a.m.", token) == 0)
		return AM;
	    continue;
	}
	else if (isdigit(*lexptr & 0xFF))
	{
	    i = 0;
	    while (isdigit(*lexptr & 0xFF))
		token[i++] = *lexptr++;
	    token[i] = '\0';
	    yylval = atoi(token);
	    return NUM;
	}
	else
	    return *lexptr++;
    }
}

/* ARGSUSED */
int
yyerror(s)
char *s;
{
    errorflag = TRUE;
    return 0;
}

/* Is y a leap year? */
#define leap(y) (((y) % 4 == 0 && (y) % 100 != 0) || (y) % 400 == 0)

/* Number of leap years from 1970 to y (not including y itself) */
#define nleap(y) (((y) - 1969) / 4 - ((y) - 1901) / 100 + ((y) - 1601) / 400)

/* This macro returns the "day" number of the sunday immediately
   preceding or equal to the argument in the current year. */
#define FIRST_SUNDAY 3
#define dayofepoch(day) ((day) + (year - 1970) * 365 + nleap(year))
#define sunday(day)  ((day) - (dayofepoch(day) + 7 - FIRST_SUNDAY) % 7)

/* correction()
   returns the daylight savings correction, in seconds, to ADD to GMT
   to get correct local time.

   Since we are converting local back to GMT, we SUBTRACT this later on
   (local = gmt + correction(); gmt = local - correction()).

   While we're at it, we also add the longitude correction for minutes
   west of Greenwich.  To do this, we have all these fascinating tables
   here . . .  */

#ifdef USE_GETTOD

struct dstinfo
{
    int year;			/* This year, or default if zero. */
    int start;			/* DST begins sunday before this day. */
    int end;			/* DST ends sunday before this day. */
};

/* USA. */
static struct dstinfo usa_dst[] =
{
    1974, 5, 333,
    1975, 58, 303,
    0, 119, 303
};

/* Australia. */
static struct dstinfo aus_dst[] =
{
    1970, 999, 0,
    1971, 303, 0,
    1972, 303, 58,
    0, 303, 65
};

/* Western Europe. */
static struct dstinfo weur_dst[] =
{
    1983, 89, 296,
    0, 89, 303
};

/* Middle Europe (also used for Eastern Europe, for lack of better
   information). */
static struct dstinfo meur_dst[] =
{
    1983, 89, 296,
    0, 89, 272
};

/* Canada is same as US, except no early 70's insanity. */
#ifdef DST_CAN
static struct dstinfo can_dst[] =
{
    0, 119, 303
};
#endif

struct dst_rules
{
    int magic;			/* Gettimeofday magic number for rule type */
    struct dstinfo *entry;	/* Pointer to struct dstinfo array. */
    int correction;		/* Correction in minutes to GMT. */
};

static struct dst_rules dstrules[] =
{
    DST_USA, usa_dst, 60,
    DST_AUST, aus_dst, -60,	/* Southern hemisphere */
    DST_WET, weur_dst, 60,
    DST_MET, meur_dst, 60,
    DST_EET, meur_dst, 60,
#ifdef DST_CAN
    DST_CAN, can_dst, 60,
#endif
    -1, 0, 0
};

static long
correction(day, tz)
int day;			/* Day number in current year.  */
struct timezone *tz;
{
    int i, correc = 0;
    struct dstinfo *dst;

    /* Did the user specify in the input string a timezone correction? */
    if (zoneflag)
	return (long)delta * 60;

    /* Since no correction was explicitly specified, we use local time zone and
       DST, as returned by gettimeofday() earlier . . . */
    if (tz->tz_dsttime)
    {
	for (i = 0; dstrules[i].magic != -1; i++)
	{
	    if (dstrules[i].magic == tz->tz_dsttime)
	    {
		dst = dstrules[i].entry;
		while (dst->year != year && dst->year)
		    dst++;
		if (sunday(dst->start) <= day && day <= sunday(dst->end)
		/* For some reason, DST starts/ends at 2 am sunday mornings. */
		    && !(day == sunday(dst->start) && hour < 2)
		    && !(day == sunday(dst->end) && hour >= 2))
		    correc = dstrules[i].correction;
		break;
	    }
	}
    }
    correc -= tz->tz_minuteswest;
    return (long)correc * 60;
}

#else /* not USE_GETTOD */

static long
correction()
{
    /* Did the input string specify a timezone correction? */
    if (zoneflag)
	return (long)delta * 60;

    /* No.  Use local time zone. */

#ifdef USE_TIMEZONE
    {
	extern long timezone;
	return -timezone;
    }
#endif

#ifdef USE_FTIME
    {
	struct timeb tb;
	(void) ftime(&tb);
	return (long)tb.timezone * -60;
    }
#endif
}

#endif /* not USE_GETTOD */

static short monthlens[] =
{
    31,				/* January */
    28,				/* February */
    31,				/* March */
    30,				/* April */
    31,				/* May */
    30,				/* June */
    31,				/* July */
    31,				/* August */
    30,				/* September */
    31,				/* October */
    30,				/* November */
    31				/* December */
};

time_t
unctime(s)
char *s;
{
#ifdef USE_GETTOD
    struct timeval tv;
    struct timezone tz;
#else
    time_t now;
#endif
    struct tm *tm;
    int dayofyear;

#ifdef USE_GETTOD
    (void) gettimeofday(&tv, &tz);
    /* The cast is required to shut lint up.  Berkeley goes to all the effort
       to define time_t, why don't they use it? */
    tm = localtime((time_t *) & tv.tv_sec);
#else
    (void) time(&now);
    tm = localtime(&now);
#endif
    year = tm->tm_year;
    month = tm->tm_mon + 1;
    day = tm->tm_mday;
    hour = tm->tm_hour;
    minute = tm->tm_min;
    second = tm->tm_sec;
    zoneflag = FALSE;
    errorflag = FALSE;

    initlex(s);
    (void) yyparse();

    if (errorflag)
	return -1;

    /* If garbage beyond valid date, that's an error. */
    while (isspace(*lexptr & 0xFF))
	++lexptr;
    if (*lexptr)
	return -1;

    /* User forced the exact time in seconds GMT, no further work necessary. */
    if (iflag)
	return iresult;

    /* Try to keep the year reasonable (i.e., within the domain of ctime()). */
    if (year < 1970)
	year += 1900;
    if (year < 1970)
	year += 100;

    /* Check for preposterous months/days/times. */
    if (month < 1 || month > 12 || day < 1
	|| (day > monthlens[month - 1]
	    && !(month == 2 && day == 29 && leap(year)))
	|| hour > 23 || minute > 59 || second > 59)
    {
	return -1;
    }

    /* Mostly for convenience in sunday() macro, we use zero-origin days. */
    dayofyear = day - 1;
    if (month > 2 && leap(year))
	++dayofyear;
    while (--month > 0)
	dayofyear += monthlens[month - 1];

    /* Wow! */
    return 86400L * (long)(dayofyear + 365 * (long)(year - 1970) + nleap(year))
	+ 3600 * (long)hour
	+ 60 * (long)minute
	+ second
#ifdef USE_GETTOD
	- correction(dayofyear, &tz)
#else
	- correction()
#endif
	;
}
