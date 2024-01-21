#ifndef lint
static char *RCSid = "$Id: time.c,v 1.6 1996/12/08 13:08:54 drd Exp $";
#endif

/* GNUPLOT - time.c */
/*
 * Copyright (C) 1986 - 1993, 1996   Thomas Williams, Colin Kelley
 *
 * Permission to use, copy, and distribute this software and its
 * documentation for any purpose with or without fee is hereby granted, 
 * provided that the above copyright notice appear in all copies and 
 * that both that copyright notice and this permission notice appear 
 * in supporting documentation.
 *
 * Permission to modify the software is granted, but not the right to
 * distribute the modified code.  Modifications are to be distributed 
 * as patches to released version.
 *  
 * This software is provided "as is" without express or implied warranty.
 * 
 *
 * AUTHORS
 * 
 *   Original Software:
 *     Thomas Williams,  Colin Kelley.
 * 
 *   Gnuplot 2.0 additions:
 *       Russell Lang, Dave Kotz, John Campbell.
 *
 *   Gnuplot 3.0 additions:
 *       Gershon Elber and many others.
 * 
 */

/* some systems may not implement time very well ; in particular,
 * things might break as the year 2000 approaches.
 * This module either adds a routine gstrptime() to read a formatted time,
 * augmenting the standard suite of time routines provided by ansi,
 * or it completely replaces the whole lot with a new set of routines,
 * which count time relative to the year 2000. Default is to use the
 * new routines. define SYSTEM_TIME to use the system routines, at your own
 * risk. One problem in particular is that not all systems allow
 * the time with integer value 0 to be represented symbolically, which
 * prevents use of relative times.
 */


#include <time.h>
#include <assert.h>

#ifdef TEST_TIME  /* build as a standalone test */
#include <stdio.h>
#define int_error(x,y) /* nowt */
/* need (only) these from plot.h */
#define ZERO_YEAR	2000
#define JAN_FIRST_WDAY 6  /* 1st jan, 2000 is a Saturday (cal 1 2000 on unix) */
#define SEC_OFFS_SYS	946684800.0		/*  zero gnuplot (2000) - zero system (1970) */
#define YEAR_SEC	31557600.0	/* avg, incl. leap year */
#define MON_SEC		2629800.0	/* YEAR_SEC / 12 */
#define WEEK_SEC	604800.0
#define DAY_SEC		86400.0
#else

#include "plot.h"
#include "setshow.h" /* for month names etc */

#endif /* TEST_TIME */

#ifdef DEBUGGING
#define DEBUG(x) printf x
#else
#define DEBUG(x) /*nothing*/
#endif

static char *read_int __PROTO((char *s, int nr, int *d));
static int gdysize __PROTO((int yr));


static char *
read_int(s,nr,d)
char *s;
int nr, *d;
{
	int result=0;

	while (--nr >= 0 && *s >= '0' && *s <= '9' )
		result = result*10 + (*s++ - '0');

	*d = result;
	return(s);
}



#ifndef SYSTEM_TIME

/* a new set of routines to completely replace the ansi ones
 * Use at your own risk
 */


static int mndday[12] = { 31,28,31,30,31,30,31,31,30,31,30,31};

static int xstrftime __PROTO((char *buf, int bufsz, char *fmt, struct tm *tm));

/* days in year */
static int
gdysize(yr)
int yr;
{

	if (!(yr%4)) {
		if ((!(yr%100)) && yr%400) 
			return(365);
		return(366);
	}
	return(365);
}


/* new strptime() and gmtime() to allow time to be read as 24 hour,
 * and spaces in the format string. time is converted to seconds from
 * year 2000.... */  

char *
gstrptime(s,fmt,tm)
char *s;
char *fmt;
struct tm *tm;
{
	int yday, date;

	date = yday = 0;
	tm->tm_mday = 1;
	tm->tm_mon = tm->tm_hour = tm->tm_min = tm->tm_sec = 0;
	tm->tm_year = ZERO_YEAR; /* make relative times work (user-defined tic step) */

	/* we do not yet calculate wday or yday, so make them illegal
	 * [but yday will be read by %j]
	 */

	tm->tm_yday = tm->tm_wday = -1;
	 
	while ( *fmt != '\0' )
	{
		if ( *fmt != '%' )
			if (*fmt == *s)
			{
				++s;
				++fmt;
				continue;
			}
			else
				break; /* literal match has failed */

		/* we are processing a percent escape */

		switch (*++fmt)
		{
			case 'd': /* read a day of month */
				s = read_int(s,2,&tm->tm_mday);
				date++;
				break;
			case 'm':
				s = read_int(s,2,&tm->tm_mon);
				date++;
				--tm->tm_mon;
				break;
			case 'y':
				s = read_int(s,2,&tm->tm_year);
				date++;
				tm->tm_year += 1900;
				break;
			case 'Y':
				s = read_int(s,4,&tm->tm_year);
				date++;
				/* tm->tm_year -= 1900; */
				/* HOE tm->tm_year %= 100; */
				break;
			case 'j':
				s = read_int(s,3,&tm->tm_yday);
				tm->tm_yday--;
				date++;
				yday++;
				break;
			case 'H':
				s = read_int(s,2,&tm->tm_hour);
				break;
			case 'M':
				s = read_int(s,2,&tm->tm_min);
				break;
			case 'S':
				s = read_int(s,2,&tm->tm_sec);
				break;
			default:
				int_warn("Bad time format in string", NO_CARET);
		}
		fmt++;
	}

	DEBUG(("read date-time : %d/%d/%d:%d:%d:%d\n", tm->tm_mday, tm->tm_mon+1, tm->tm_year, tm->tm_hour, tm->tm_min, tm->tm_sec));

	/* now check the date/time entered, normalising if necessary
	 * read_int cannot read a -ve number, but can read %m=0 then decrement
	 * it to -1
	 */
	 
#define S (tm->tm_sec)
#define M (tm->tm_min)
#define H (tm->tm_hour)

	if (S>=60) { M += S/60; S%=60; }

	if (M>=60) { H += M/60; M%=60; }

	if (H>=24)
	{
		if (yday) tm->tm_yday += H/24;
		tm->tm_mday += H/24;
		H %= 24;
	}

#undef S
#undef M
#undef H

	DEBUG(("normalised time : %d/%d/%d:%d:%d:%d\n", tm->tm_mday, tm->tm_mon+1, tm->tm_year, tm->tm_hour, tm->tm_min, tm->tm_sec));

	if (date)
	{
		if (yday)
		{

			if (tm->tm_yday < 0)
				int_error("Illegal day of year", NO_CARET);

			/* we just set month to jan, day to yday, and let the
			 * normalising code do the work.
			 */

			tm->tm_mon=0;
			tm->tm_mday = tm->tm_yday+1;  /* yday is 0->365, day is 1->31 */
		}

		if (tm->tm_mon < 0 )
		{
			int_error("illegal month",NO_CARET);
			return(NULL);
		}
			
		if ( tm->tm_mday < 1)
		{
			int_error("illegal day of month",NO_CARET);
			return(NULL);
		}
		
		if (tm->tm_mon > 11)
		{
			tm->tm_year += tm->tm_mon / 12;
			tm->tm_mon %= 12;
		}
		
		{
			int days_in_month;
			while (tm->tm_mday > (days_in_month = (mndday[tm->tm_mon] + (tm->tm_mon == 1 && (gdysize(tm->tm_year)>365)))))
			{
				if (++tm->tm_mon == 12)
				{
					++tm->tm_year;
					tm->tm_mon = 0;
				}
				tm->tm_mday -= days_in_month;
			}
		}
	}
	
	return(s);
}

int 
gstrftime(s,bsz,fmt,clock)
char *s;
int bsz;
char *fmt;
double clock;
{
	struct tm tm;

	ggmtime(&tm,clock);
#if 0
	if ((tm.tm_zone = (char *) malloc(strlen(xtm->tm_zone)+1))) 
		strcpy(tm.tm_zone,xtm->tm_zone);
	/* printf("zone: %s - %s\n",tm.tm_zone,xtm->tm_zone); */
#endif

	return(xstrftime(s,bsz,fmt,&tm));
}

static int 
xstrftime(str,bsz,fmt,tm)
char *str;
int bsz;
char *fmt;
struct tm *tm;
{
	int l;
	char *p, *s;

	p = fmt;
	s = str;
	memset(s,'\0',bsz);
	l=0;
	while (*p != '\0') {
		if (*p != '%') {
			if ( l >= bsz ) return(0);
			*s++ = *p++;
			l++;
		} else {
			p++;
			if ( *p == '%' ) {
				if ( l >= bsz ) return(0);
				*s = '%';
			} else if ( *p == 'a' ) {	
				if ((l+strlen(abbrev_day_names[tm->tm_wday])) > bsz ) return(0);
				sprintf(s,"%s",abbrev_day_names[tm->tm_wday]);
			} else if ( *p == 'A' ) {	
				if(l+strlen(full_day_names[tm->tm_wday]) > bsz) return(0);
				sprintf(s,"%s",full_day_names[tm->tm_wday]);
			} else if ( *p == 'b' || *p == 'h' ) {
				if(l+strlen(abbrev_month_names[tm->tm_mon])> bsz) return(0);
				sprintf(s,"%s",abbrev_month_names[tm->tm_mon]);
			} else if ( *p == 'B' ) {
				if(l+strlen(full_month_names[tm->tm_mon]) > bsz) return(0);
				sprintf(s,"%s",full_month_names[tm->tm_mon]);
			} else if ( *p == 'c' ) {
				if (!xstrftime(s,bsz-l,"%x %X",tm)) {
					return(0);
				}
#if 0
			} else if ( *p == 'C' ) {
				if (!xstrftime(s,bsz-l,dtc->ldate_format,tm)) {
					return(0);
				}
#endif
			} else if ( *p == 'd' ) {
				if ( bsz - l < 2 ) return(0);
				sprintf(s,"%02d",tm->tm_mday);
			} else if ( *p == 'D' ) {
				if (!xstrftime(s,bsz-l,"%m/%d/%y",tm)) {
					return(0);
				}
			} else if ( *p == 'e' ) {
				if ( bsz - l < 2 ) return(0);
				sprintf(s,"%2d",tm->tm_mday);
			} else if ( *p == 'H' ) {
				if ( bsz - l < 2 ) return(0);
				sprintf(s,"%02d",tm->tm_hour);
			} else if ( *p == 'I' ) {
				if ( bsz - l < 2 ) return(0);
				sprintf(s,"%02d",tm->tm_hour%12);
			} else if ( *p == 'j' ) {
				if ( bsz - l < 3 ) return(0);
				sprintf(s,"%03d",tm->tm_yday+1);
			} else if ( *p == 'k' ) {
				if ( bsz - l < 2 ) return(0);
				sprintf(s,"%2d",tm->tm_hour);
			} else if ( *p == 'l' ) {
				if ( bsz - l < 2 ) return(0);
				sprintf(s,"%2d",tm->tm_hour%12);
			} else if ( *p == 'm' ) {
				if ( bsz - l < 2 ) return(0);
				sprintf(s,"%02d",tm->tm_mon+1);
			} else if ( *p == 'M' ) {
				if ( bsz - l < 2 ) return(0);
				sprintf(s,"%02d",tm->tm_min);
			} else if ( *p == 'n' ) {
				if ( bsz >= l ) return(0);
				*s = '\n';
			} else if ( *p == 'p' ) {
				if(l+strlen((tm->tm_hour<12)?"am":"pm") > bsz) return(0);
				sprintf(s,"%s",(tm->tm_hour<12)?"am":"pm");
			} else if ( *p == 'r' ) {
				if (!xstrftime(s,bsz-l,"%I:%M:%S %p",tm)) {
					return(0);
				}
			} else if ( *p == 'R' ) {
				if (!xstrftime(s,bsz-l,"%H:%M",tm)) {
					return(0);
				}
			} else if ( *p == 'S' ) {
				if ( bsz - l < 2 ) return(0);
				sprintf(s,"%02d",tm->tm_sec);
			} else if ( *p == 't' ) {
				if ( bsz >= l ) return(0);
				*s = '\t';
			} else if ( *p == 'T' ) {
				if (!xstrftime(s,bsz-l,"%H:%M:%S",tm)) {
					return(0);
				}
			} else if ( *p == 'W' ) { /* mon 1 day of week */
				int week, bw;

				if ( bsz - l < 2 ) return(0);
				if ( tm->tm_yday <= tm->tm_wday ) {
					week = 1;
					if ( (tm->tm_mday - tm->tm_yday) > 4 ) {
						week = 52;
					} 
					if ( tm->tm_yday == tm->tm_wday && tm->tm_wday == 0 ) week = 52;
				} else {
					bw = tm->tm_yday - tm->tm_wday; /* sun prev week */
					if ( tm->tm_wday > 0 ) bw += 7; /* sun end of week */
					week = (int) bw/7;
					if ( (bw%7) > 2 ) { /* jan 1 is before friday */
						week++;
					}
				}
				sprintf(s,"%02d",week);
			} else if ( *p == 'U' ) { /* sun 1 day of week */
				int week, bw;

				if ( bsz - l < 2 ) return(0);
				if ( tm->tm_yday <= tm->tm_wday ) {
					week = 1;
					if ( (tm->tm_mday - tm->tm_yday) > 4 ) {
						week = 52;
					} 
				} else {
					bw = tm->tm_yday - tm->tm_wday-1; /* sat prev week */
					if ( tm->tm_wday >= 0 ) bw += 7; /* sat end of week */
					week = (int) bw/7;
					if ( (bw%7) > 1 ) { /* jan 1 is before friday */
						week++;
					}
				}
				sprintf(s,"%02d",week);
			} else if ( *p == 'w' ) { /* day of week, sun=0 */
				if ( bsz - l < 2 ) return(0);
				sprintf(s,"%02d",tm->tm_wday);
#if 0
			} else if ( *p == 'x' ) { /* locales date format */
				if (!xstrftime(s,bsz-l,dtc->sdate_format,tm)) {
					return(0);
				}
			} else if ( *p == 'X' ) { /* locales time format */
				if (!xstrftime(s,bsz-l,dtc->time_format,tm)) {
					return(0);
				}
#endif
			} else if ( *p == 'y' ) {
				if ( bsz - l < 2 ) return(0);
				sprintf(s,"%02d",tm->tm_year%100);
			} else if ( *p == 'Y' ) {
				if ( bsz - l < 4 ) return(0);
				sprintf(s,"%04d",tm->tm_year);
#if 0
			} else if ( *p == 'Z' ) {
				if ( bsz - l < strlen(tm->tm_zone) ) return(0);
				sprintf(s,"%s",tm->tm_zone);
#endif
			}
			p++;
			while ( *s != '\0' ) {
				s++;
				l++;
			}
		}
	}
	return(l);
}

/* time_t  */
double
gtimegm(tm)
struct tm *tm;
{
	register int i;
	/* returns sec from year ZERO_YEAR, defined in plot.h */
	double dsec;

	dsec = 0;
	if ( tm->tm_year < ZERO_YEAR ) {
		for(i=tm->tm_year;i<ZERO_YEAR;i++) {
			dsec -= (double) gdysize(i);
		}
	} else {
		for(i=ZERO_YEAR;i<tm->tm_year;i++) {
			dsec += (double) gdysize(i);
		}
	}
	if ( tm->tm_mday > 0 ) {
		for (i=0;i<tm->tm_mon;i++) {
			dsec += (double) mndday[i] + (i==1 && (gdysize(tm->tm_year)>365));
		}
		dsec += (double) tm->tm_mday-1;
	} else {
		dsec += (double) tm->tm_yday;
	}
	dsec *= (double) 24;

	dsec += tm->tm_hour;
	dsec *= 60.0;
	dsec += tm->tm_min;
	dsec *= 60.0;
	dsec += tm->tm_sec;
	
	DEBUG(("broken-down time : %d/%d/%d:%d:%d:%d = %g seconds\n", tm->tm_mday, tm->tm_mon+1, tm->tm_year, tm->tm_hour, tm->tm_min, tm->tm_sec, dsec));

	return(dsec);
}

int
ggmtime(tm,clock)
struct tm *tm;
/* time_t clock; */
double clock;
{
	/* clock is relative to ZERO_YEAR, jan 1, 00:00:00,defined in plot.h */
	int i, days;

	/* dodgy way of doing wday - i hope it works ! */
	
	int wday = JAN_FIRST_WDAY; /* eg 6 for 2000 */
	
	DEBUG(("%g seconds = ", clock));
	
	tm->tm_year = ZERO_YEAR;
	tm->tm_mday = tm->tm_yday = tm->tm_mon = tm->tm_hour = tm->tm_min = tm->tm_sec = 0; 
	if ( clock < 0 ) {
		while ( clock < 0 ) {
			int days_in_year = gdysize(--tm->tm_year);
			clock += days_in_year*DAY_SEC; /* 24*3600 */
			/* adding 371 is noop in modulo 7 arithmetic, but keeps wday +ve */
			wday += 371 - days_in_year;
		}
	} else {
		for (;;) {
			int days_in_year = gdysize(tm->tm_year);
			if (clock < days_in_year*DAY_SEC) break;
			clock -= days_in_year*DAY_SEC;
			tm->tm_year ++;
			/* only interested in result modulo 7, but %7 is expensive */
			wday += (days_in_year - 364);
		}
	}
	tm->tm_yday = (int)(clock/DAY_SEC);
	clock -= tm->tm_yday*DAY_SEC;
	tm->tm_hour = (int)clock/3600;
	clock -= tm->tm_hour*3600;
	tm->tm_min = (int)clock/60;
	clock -= tm->tm_min*60;
	tm->tm_sec = (int)clock;

	days = tm->tm_yday;

	/* wday%7 should be day of week of first day of year */
	tm->tm_wday = (wday+days)%7;

	while ( days >= (i = mndday[tm->tm_mon] + (tm->tm_mon==1 && (gdysize(tm->tm_year)>365)))) {
		days -= i;
		tm->tm_mon ++;
	}
	tm->tm_mday = days+1;
	
	DEBUG(("broken-down time : %d/%d/%d:%d:%d:%d\n", tm->tm_mday, tm->tm_mon+1, tm->tm_year, tm->tm_hour, tm->tm_min, tm->tm_sec));

	return(0);
}




#else  /* SYSTEM_TIME */

/* define gnu time routines in terms of system time routines */

int gstrftime(buf, bufsz, fmt, clock)
char *buf;
int bufsz;
char *fmt;
double clock;
{
	time_t t = (time_t) clock;
	return strftime(buf, bufsz, fmt, gmtime(&t));
}

double gtimegm(tm)
struct tm *tm;
{
	return (double) mktime(tm);
}

int ggmtime(tm, clock)
struct tm *tm;
double clock;
{
	time_t t = (time_t) clock;
	struct tm *m = gmtime(&t);
	*tm = *m; /* can any non-ansi compilers not do this ? */
}
#define NOTHING	
#define LETTER(L, width, field, extra) \
  case L: s=read_int(s,width,&tm->field); extra; continue

/* supplemental routine gstrptime() to read a formatted time */

char *
gstrptime(s,fmt,tm)
char *s;
char *fmt;
struct tm *tm;
{
	DEBUG(("gstrptime(\"%s\", \"%s\")\n", s, fmt));

	/* linux does not appear to like years before 1902
	 * NT complains if its before 1970
	 * initialise fields to midnight, 1st Jan, 1970 (for relative times)
	 */
	tm->tm_sec = tm->tm_min = tm->tm_hour = 0;
	tm->tm_mday = 1; tm->tm_mon=0; tm->tm_year=70;
	/* oops - it goes wrong without this */
	tm->tm_isdst=0;
	
	for (;*fmt && *s;++fmt) {
		if (*fmt != '%')
		{
			if (*s != *fmt)
				return s;
			++s;
			continue;
		}
		assert(*fmt == '%');

		switch(*++fmt)
		{
			case 0:
				/* uh oh - % is last character in format */
				return s;
			case '%':
				/* literal % */
				if (*s++ != '%') return s-1;
				continue;

			LETTER('d', 2, tm_mday, NOTHING);
			LETTER('m', 2, tm_mon, NOTHING);
			LETTER('y', 2, tm_year, NOTHING);
			LETTER('Y', 4, tm_year, tm->tm_year -= 1900);
			LETTER('H', 2, tm_hour, NOTHING);
			LETTER('M', 2, tm_min, NOTHING);
			LETTER('S', 2, tm_sec, NOTHING);

			default:
				int_error("incorrect time format character", NO_CARET);
		}
	}

	DEBUG(("Before mktime : %d/%d/%d:%d:%d:%d\n", tm->tm_mday, tm->tm_mon, tm->tm_year, tm->tm_hour, tm->tm_min, tm->tm_sec));
	/* mktime range-checks the time */

	if (mktime(tm) == -1)
	{
		DEBUG(("mktime() was not happy\n"));
		int_error("Invalid date/time [mktime() did not like it]", NO_CARET);
	}

	DEBUG(("After mktime : %d/%d/%d:%d:%d:%d\n", tm->tm_mday, tm->tm_mon, tm->tm_year, tm->tm_hour, tm->tm_min, tm->tm_sec));

	return s;
}


#endif






#ifdef TEST_TIME


int main(int argc, char *argv[])
{
	struct tm tm;
	char output[80];
	
	if (argc < 3)
	{
		fprintf(stderr, "usage : test 'format' 'time'\n");
		return 1;
	}

	gstrptime(argv[2], argv[1],&tm);
	
	puts(asctime(&tm));
	return 0;
}

#endif /* TEST_TIME */
