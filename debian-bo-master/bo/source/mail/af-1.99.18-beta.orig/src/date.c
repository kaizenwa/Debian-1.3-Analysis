/* Date.c - Date reading and display for af.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include "af.h"
#include "atom.h"
#include "date.h"
#include STRING_HDR
#include TIME_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: date.c,v 1.13 1996/10/17 18:53:02 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *get_host();
extern int strcasecmp();
extern pid_t getpid();
extern void afree();
extern time_t time();
extern struct tm *localtime(), *gmtime();
extern ATOM *tokenise(), *find_token();

#ifdef HAVE_TZSET
extern void tzset();
#endif /* HAVE_TZSET */

/* Local function declarations */

static int day_of_week(), month(), atomval();
static long tz_offset();
static DATEZONE *sysdate(), *posixdate(), *dateval();
static ATOM *get_zone();

/****************************************************************************/
DATEZONE *parse_date(date)
char *date;
{
	/* 
	 * Parse the date specified in the string date, and return
	 * a DATEZONE value giving the number of seconds since the
	 * epoch (1 Jan 70), and the timezone offset in seconds.
	 * A return of NULL indicates an error.
	 *
	 * The date may be in POSIX or system form, and may contain
	 * comments or arbitrary white space.
	 */

	int is_posix = FALSE;
	DATEZONE *date_val = NULL;
	ATOM *alist, *start;

	/* Tokenise the date string */

	if ((alist = tokenise(date)) == NULL) {
		return(NULL);
	}

	/* Find the first token in the list */

	if ((start = find_token(alist)) == NULL) {
		afree(alist);
		return(NULL);
	}

	/* Now skip the day of week */

	if (day_of_week(start) >= 0) {
		/* Skip the day of week, checking for errors */

		if ((start = find_token(start->next)) == NULL) {
			afree(alist);
			return(NULL);
		}

		/* Skip the comma which follows in POSIX dates */

		if (start->type == AT_COMMA) {
			/* Skip the comma, checking for errors */

			if ((start = find_token(start->next)) == NULL) {
				afree(alist);
				return(NULL);
			}

			/* This must be a POSIX date */

			is_posix = TRUE;
		}
	}

	/* Check if the date is POSIX or system format */

	is_posix = (is_posix || month(start) < 0);

	/* Parse the date according to its type */

	date_val = (is_posix) ? posixdate(start) : sysdate(start);

	/* Free the atom list and return the date value */

	afree(alist);
	return(date_val);
}
/****************************************************************************/
static int day_of_week(token)
ATOM *token;
{
	/*
	 * Return a value for the day of the week specified in
	 * token, 0 for Sunday through to 6 for Saturday.
	 * Returns -1 if the token is not a weekday.
	 */

	int i, wday = -1;

	/* Search the weekday array for the token */

	for (i = 0; weekdays[i] != NULL; i++) {
		/* Is this the weekday? */

		if (!strcasecmp(weekdays[i], token->text)) {
			wday = i;
			break;
		}
	}

	return(wday);
}
/****************************************************************************/
static int month(token)
ATOM *token;
{
	/*
	 * Return a value for the month specified in token,
	 * 0 for January through to 11 for December.
	 * Returns -1 if the token is not a month.
	 */

	int i, mo = -1;

	/* Search the month array for the token */

	for (i = 0; months[i] != NULL; i++) {
		/* Is this the month? */

		if (!strcasecmp(months[i], token->text)) {
			mo = i;
			break;
		}
	}

	return(mo);
}
/****************************************************************************/
static int atomval(token)
ATOM *token;
{
	/*
	 * Return the integer value stored as text within token,
	 * which must be of type atom.
	 * Returns -1 on error.
	 */

	char *p;
	int val = 0;

	/* Check the token is an atom */

	if (token->type != AT_ATOM) {
		return(-1);
	}

	/* Get the value of the token */

	for (p = token->text; *p != '\0'; p++) {
		/* This had better be a digit */

		if (!isdigit(*p)) {
			return(-1);
		}

		/* Update the value */

		val *= 10;
		val += *p - '0';
	}

	return(val);
}
/****************************************************************************/
static DATEZONE *posixdate(start)
ATOM *start;
{
	/*
	 * Read a POSIX format date.  The general format is
	 * dd Month yy hh:mm[:ss] zone
	 */

	int day, mo, year, hour, min, sec;
	long zone;
	ATOM *a = start;

	/* The start token is the day of the month */

	if ((day = atomval(a)) < 1) {
		return(NULL);
	}

	/* Get the month */

	if ((a = find_token(a->next)) == NULL || (mo = month(a)) < 0) {
		return(NULL);
	}

	/* Get the year */

	if ((a = find_token(a->next)) == NULL || (year = atomval(a)) < 0) {
		return(NULL);
	}

	/* Get the hour */

	if ((a = find_token(a->next)) == NULL || (hour = atomval(a)) < 0) {
		return(NULL);
	}

	/* Should be a colon after the hour */

	if ((a = find_token(a->next)) == NULL || a->type != AT_COLON) {
		return(NULL);
	}

	/* Get the minute */

	if ((a = find_token(a->next)) == NULL || (min = atomval(a)) < 0) {
		return(NULL);
	}

	/* Get the next token */

	if ((a = find_token(a->next)) != NULL && a->type == AT_COLON) {
		/* Get the second */

		if ((a = find_token(a->next)) == NULL
		    || (sec = atomval(a)) < 0) {
			return(NULL);
		}

		/* Find any atom containing the time zone */

		a = find_token(a->next);
	} else {
		sec = 0;
	}

	/* Set the time zone */

	a = get_zone(a, &zone);

	/* Now process the date values */

	return(dateval(day, mo, year, hour, min, sec, zone));
}
/****************************************************************************/
static DATEZONE *sysdate(start)
ATOM *start;
{
	/*
	 * Read a system format date.  The general format is
	 * Month dd hh:mm:ss zone yyyy
	 */

	int day, mo, year, hour, min, sec;
	long zone;
	ATOM *a = start;

	/* The start token is the month */

	if ((mo = month(a)) < 0) {
		return(NULL);
	}

	/* Get the day of the month */

	if ((a = find_token(a->next)) == NULL || (day = atomval(a)) < 1) {
		return(NULL);
	}

	/* Get the hour */

	if ((a = find_token(a->next)) == NULL || (hour = atomval(a)) < 0) {
		return(NULL);
	}

	/* Should be a colon after the hour */

	if ((a = find_token(a->next)) == NULL || a->type != AT_COLON) {
		return(NULL);
	}

	/* Get the minute */

	if ((a = find_token(a->next)) == NULL || (min = atomval(a)) < 0) {
		return(NULL);
	}

	/* Get the next token */

	if ((a = find_token(a->next)) != NULL && a->type == AT_COLON) {
		/* Get the second */

		if ((a = find_token(a->next)) == NULL
		    || (sec = atomval(a)) < 0) {
			return(NULL);
		}

		/* Find any atom containing the time zone */

		a = find_token(a->next);
	} else {
		sec = 0;
	}

	/* Set the time zone */

	a = get_zone(a, &zone);

	/* Get the year */

	if (a == NULL || (year = atomval(a)) < 0) {
		return(NULL);
	}

	/* Now process the date values */

	return(dateval(day, mo, year, hour, min, sec, zone));
}
/****************************************************************************/
static ATOM *get_zone(start, zone)
ATOM *start;
long *zone;
{
	/*
	 * Return the offset for the time zone specified in
	 * zero, one or two atoms beginning at start.
	 */

	int i;
	ATOM *a = start;

	/* Default the return value to zero */

	*zone = 0L;

	/* Check that a zone was specified */

	if (a == NULL || a->type != AT_ATOM || atomval(a) >= 0) {
		return(a);
	}

	/* Handle [+-]dddd time zone specifications */

	if ((a->text[0] == '+' || a->text[0] == '-')
	    && strlen(a->text) == CHARS_IN_ZONE
	    && isdigit(a->text[1]) && isdigit(a->text[2])
	    && isdigit(a->text[3]) && isdigit(a->text[4])) {
		/* The timezone calculation is actually possible */

		*zone = ((a->text[1] - '0') * 10 + a->text[2] - '0')
			* SECONDS_PER_HOUR;
		*zone += ((a->text[3] - '0') * 10 + a->text[4] - '0')
			* SECONDS_PER_MINUTE;
		*zone *= (a->text[0] == '-') ? -1 : 1;

		/* Can't have additional tokens if in this form */

		return(find_token(a->next));
	}

	/* Try looking up the time zone in the internal table */

	for (i = 0; tzlist[i].zone != NULL; i++) {
		if (!strcasecmp(tzlist[i].zone, a->text)) {
			*zone = tzlist[i].offset;
			break;
		}
	}

	/*
	 * Check if a second atom indicates DST.  This is a nasty
	 * kludge used in some systems, eg WET DST not WDT.
	 */

	if ((a = find_token(a->next)) != NULL && IS_DST(a)) {
		SET_DST(*zone);
		return(find_token(a->next));
	}

	/* There wasn't an extra atom used to specify DST */

	return(a);
}
/****************************************************************************/
static DATEZONE *dateval(day, mo, year, hour, min, sec, zone)
int day, mo, year, hour, min, sec;
long zone;
{
	/* Return the seconds from the epoch given by the values passed */

	int i;
	time_t dval = 0L;
	DATEZONE *date = NULL;

	/* Handle years specified as two digits */

	if (year < 100) {
		/* Update the year */

		year += BASE_YEAR;

		/* And make sure it's valid */

		if (year < EPOCH_YEAR) {
			year += 100;
		}
	}

	/* Check the year */

	if (year < EPOCH_YEAR) {
		return(NULL);
	}

	/* Set seconds from epoch to start of year */

	for (i = EPOCH_YEAR; i < year; i++) {
		dval += SECONDS_PER_YEAR;
		if (IS_LEAP_YEAR(i)) {
			dval += SECONDS_PER_DAY;
		}
	}

	/* Check the month */

	if (mo > NO_MONTHS) {
		return(NULL);
	}

	/* Add seconds from start of year to start of month */

	for (i = 0; i < mo; i++) {
		dval += mo_days[i] * SECONDS_PER_DAY;
		if (IS_LEAP_YEAR(year) && IS_LEAP_MONTH(i)) {
			dval += SECONDS_PER_DAY;
		}
	}

	/* Check the day of the month */

	if ((!IS_LEAP_YEAR(year) || !IS_LEAP_MONTH(mo))
	    && day > mo_days[mo] || day > mo_days[mo] + 1) {
		return(NULL);
	}

	/* Add seconds from start of month to start of day */

	dval += SECONDS_PER_DAY * (day - 1);

	/* Check the hour, minute and second */

	if (hour >= HOURS_PER_DAY || min >= MINUTES_PER_HOUR
	    || sec >= SECONDS_PER_MINUTE) {
		return(NULL);
	}

	/* Add seconds from start of day to specified time */

	dval += SECONDS_PER_HOUR * (hour);
	dval += SECONDS_PER_MINUTE * (min);
	dval += sec;

	/* Correct the date for the time zone */

	dval -= zone;
	dval += (dval < 0) ? zone : 0;

	/* Set the return structure and return it */

	date = (DATEZONE *) xmalloc(sizeof(DATEZONE));
	date->d_date = dval;
	date->d_zone = zone;

	return(date);
}
/****************************************************************************/
char *strdate(date, local)
DATEZONE *date;
int local;
{
	/*
	 * Return a static string containing the POSIX date which
	 * is given by date.  The date is shown in local time if
	 * local is TRUE, or in the original time zone otherwise.
	 */

	/* The buffer to hold the return string */

	static char dstr[MAXDATELEN + 1];

	long offset;
	time_t dval;
	int tzhours, tzmins, tzsign;
	struct tm *tval;

	/* Check the date is valid */

	if (date == NULL) {
		(void) strcpy(dstr, BAD_MESSAGE);
		return(dstr);
	}

	/* Get the offset represented by the time zone */

	offset = (local) ? tz_offset() : date->d_zone;

	/* Modify the date for the relevant time zone */

	if  ((dval = date->d_date + offset) < 0) {
		/* Can't convert; ignore the offset */

		dval -= offset;
		offset = 0;
	}

	/* Calculate the time zone value in hours and minutes */
	
	tzsign = (offset < 0) ? '-' : '+';
	offset = (offset < 0) ? -offset : offset;
	tzhours = offset / SECONDS_PER_HOUR;
	tzmins = (offset - tzhours * SECONDS_PER_HOUR) / SECONDS_PER_MINUTE;

	/* Break the date down into its components */

	tval = gmtime(&dval);

	/* Write the time into the string and return it */

	(void) sprintf(dstr, "%s, %d %s %d %02d:%02d %c%02d%02d",
		       weekdays[tval->tm_wday], tval->tm_mday,
		       months[tval->tm_mon], tval->tm_year % 100,
		       tval->tm_hour, tval->tm_min, tzsign, tzhours, tzmins);
	return(dstr);
}
/****************************************************************************/
char *strqdate(date, local, with_time)
DATEZONE *date;
int local, with_time;
{
	/*
	 * Return a static string containing the date given by date
	 * in short format, including the time if with_time is
	 * TRUE.  The date is shown in local time if local is
	 * TRUE, or in the original time zone otherwise.
	 */

	/* The buffer to hold the return string */

	static char dstr[MAXQDATELEN + 1];

	long offset;
	time_t dval;
	int recent;
	struct tm *tval;

	/* Check the date is valid */

	if (date == NULL) {
		(void) strcpy(dstr, "");
		return(dstr);
	}

	/* Get the offset represented by the time zone */

	offset = (local) ? tz_offset() : date->d_zone;

	/* Modify the date for the relevant time zone */

	if  ((dval = date->d_date + offset) < 0) {
		/* Can't convert; ignore the offset */

		dval -= offset;
		offset = 0;
	}

	/* Check if we will show the date in recent form */

	recent = (time(NULL) < dval + RECENT_THRESHOLD);

	/* Break the date down into its components */

	tval = gmtime(&dval);

	/* Write the date into the string and return it */

	if (with_time && recent) {
		(void) sprintf(dstr, "%02d %s %02d:%02d", tval->tm_mday,
			       months[tval->tm_mon], tval->tm_hour,
			       tval->tm_min);
	} else if (recent) {
		(void) sprintf(dstr, "%02d %s", tval->tm_mday,
			       months[tval->tm_mon]);
	} else if (with_time) {
		(void) sprintf(dstr, "%02d %s  %4d", tval->tm_mday,
			       months[tval->tm_mon], tval->tm_year
			       + BASE_YEAR);
	} else {
		(void) sprintf(dstr, "%s %02d", months[tval->tm_mon],
			       tval->tm_year % 100);
	}

	return(dstr);
}
/****************************************************************************/
char *strudate(date)
DATEZONE *date;
{
	/*
	 * Return a static string containing the UNIX date which
	 * is given by date.  The date is shown in GMT.
	 */

	/* The buffer to hold the return string */

	static char dstr[MAXUDATELEN + 1];
	time_t dval;
	struct tm *tval;

	/* Check the date is valid */

	if (date == NULL) {
		(void) strcpy(dstr, "");
		return(dstr);
	}

	/* Break the date down into its components */

	dval = date->d_date;
	tval = gmtime(&dval);

	/* Write the date into the string and return it */

	(void) sprintf(dstr, "%s %s %d %02d:%02d GMT %4d",
		       weekdays[tval->tm_wday], months[tval->tm_mon],
		       tval->tm_mday, tval->tm_hour, tval->tm_min,
		       tval->tm_year + BASE_YEAR);
	return(dstr);
}
/****************************************************************************/
#ifdef NO_MTA_ID
char *strid(date)
DATEZONE *date;
{
	/*
	 * Return a static string containing a Message ID
	 * formed from the local date PID and hostname.
	 */

	/* The buffer to hold the return string */

	static char *idstr = NULL;

	char *hnam;
	long offset;
	time_t dval;
	struct tm *tval;

        /* Set the local system name */

        hnam = get_host();

	/* Allocate the space for the id - hnam can't change */

	if (idstr == NULL) {
		idstr = xmalloc(BASEIDLEN + strlen(hnam) + 1);
	}

	/* Get the offset represented by the time zone */

	offset = date->d_zone;

	/* Modify the date for the relevant time zone */

	if  ((dval = date->d_date + offset) < 0) {
		/* Can't convert; ignore offset */

		dval -= offset;
		offset = 0;
	}

	/* Break the date down into its components */

	tval = gmtime(&dval);

	/* Write the time into the string and return it */

	(void) sprintf(idstr, ID_FORMAT, tval->tm_year % 100,
		       tval->tm_mon + 1, tval->tm_mday, tval->tm_hour,
		       tval->tm_min, (unsigned) getpid(), hnam);
	return(idstr);
}
#endif /* NO_MTA_ID */
/****************************************************************************/
DATEZONE *date_now()
{
	/*
	 * Return a pointer to a static DATEZONE structure
	 * containing the local date and zone.
	 */

	static DATEZONE date;

	/* Set the date (stored in UT) and zone */

	date.d_date = time(NULL);
	date.d_zone = 0L;

	/* Return the information */

	return(&date);
}
/****************************************************************************/
static long tz_offset()
{
	/* Return the offset for the time zone on the local system */

#ifndef HAVE_TM_GMTOFF
	/* Import the timezone offset from libc */

	extern time_t timezone;
#endif /* HAVE_TM_GMTOFF */

	long offset;
	time_t tval;
	struct tm *date;

#ifdef HAVE_TZSET
	/* Make sure that timezone is accurate */

	tzset();
#endif /* HAVE_TZSET */

	/* Get the local time (so we can find out about daylight savings) */

	tval = time(NULL);
        date = localtime(&tval);

	/* Set the base offset for the local time zone */

#ifdef HAVE_TM_GMTOFF
	offset = date->tm_gmtoff;

#else /* ! HAVE_TM_GMTOFF */

	offset = timezone;

	/* Update for daylight savings time if in effect */

	if (date->tm_isdst > 0) {
		SET_DST(offset);
	}
#endif /* ! HAVE_TM_GMTOFF */

	return(offset);
}
/****************************************************************************/
