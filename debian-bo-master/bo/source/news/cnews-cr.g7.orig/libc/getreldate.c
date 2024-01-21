/*
 * getreldate - parse relative dates
 *
 * relative dates are of the form
 *	<timeunits> before <date>
 *	<timeunits> after|from <date>
 *	<timeunits> ago
 *	<timeunits> hence
 *	<timeunits>
 *	<qualifier> <unit>|<day>|<month> ["in"|"of" <month>|<year>]
 * where <timeunits> is a sequence of the form { [<number>] <unit> } +
 * <unit> is one of second, minute, hour, day, week, month or year
 *	(plurals are okay too).
 * <date> is "now" or an absolute date (see getabsdate(3)).
 * <qualifier> is "this", "first", "last", "next", "second", "third",
 *	..., "twelfth"
 * <day> is a day-of-the-week name
 * <month> is a month name.
 * <year> is a numeric year.
 *
 * note: 1 month ago means the same day last month, not 30 or 31 days ago.
 * If this day didn't exist in that month, then it's the last day.  so "1
 * month before May 31" means April 30.  This system may be non-intuitive
 * -- blame the bozos who thought up the calendar system.  1 year ago means
 * this day and month last year.  Same caveats apply.  We first rewind years,
 * then months.  This has subtle effects on leap years.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <stdlib.h>

#include "datetok.h"
#include "dateconv.h"

#define STREQ(a, b)	(*(a) == *(b) && strcmp(a, b) == 0)

#define MAXDATEFIELDS 50

extern time_t time();
extern datetkn datereltoks[];
extern unsigned int szdatereltoks;

struct parsestate {
	time_t	secs;
	time_t	months;
	time_t	before;		/* 0 means after, 1 means before */
	time_t	lastnum;
};

/*
 * parse and convert relative date in timestr (the normal interface)
 */
time_t
getreldate(timestr, now)
char *timestr;
struct timeb *now;
{
	int tz = 0;
	struct tm date;

	return prsreldate(timestr, now, &date, &tz) < 0? -1:
		dateconv(&date, tz);
}

/* try to parse "now" or an absolute date */
static int
trydate(fields, nf, now, tm, tzp)
register char **fields;
int nf;
struct timeb *now;
struct tm *tm;
int *tzp;
{
	if (nf == 1 &&
	    (STREQ(fields[0], "now") || STREQ(fields[0], "today") ||
	     STREQ(fields[0], "tomorrow") || STREQ(fields[0], "yesterday"))) {
		struct timeb fakenow;

		if (now == NULL)
			now = &fakenow;
		(void) ftime(now);
		if (STREQ(fields[0], "tomorrow"))
			now->time += 24L*60L*60L;
		else if (STREQ(fields[0], "yesterday"))
			now->time -= 24L*60L*60L;
		*tm = *gmtime(&now->time);
		*tzp = 0;
		return 0;
	} else
		return tryabsdate(fields, nf, now, tm, tzp);
}

/* TODO: absdate may not be DEC date */

static int			/* token type, 0 for done, -1 for failure */
prsrdtoken(fields, nf, i, now, tm, tzp, psp)
register char **fields;
int nf;
register int i;
struct timeb *now;
register struct tm *tm;
int *tzp;
register struct parsestate *psp;
{
	register char c;

	if (i >= nf)
		return -1;	/* index out of range */
	c = fields[i][0];
	if (isascii(c) && isdigit(c)) {
		if (psp->lastnum != -1)
			return -1;
		psp->lastnum = atol(fields[i]);
		return NUMBER;
	} else {
		register datetkn *tp =
			datebsearch(fields[i], datereltoks, szdatereltoks);
		register time_t val;

		if (tp == NULL)		/* probably an absolute date */
			return trydate(fields, nf, now, tm, tzp);
		val = tp->value;
		switch (tp->type) {
		case ORDINAL:
			if (psp->lastnum != -1)
				return -1;
			psp->lastnum = val;
			break;
		case YEARS:
			val *= 12;
			/* FALLTHROUGH */
		case MONTHS:
			psp->months +=
				(psp->lastnum == -1? val: val * psp->lastnum);
			psp->lastnum = -1;
			break;
		case DAYS:
			val *= 24;
			/* FALLTHROUGH */
		case HOURS:
			val *= 3600;
			/* FALLTHROUGH */
		case SECONDS:
			psp->secs +=
				(psp->lastnum == -1? val: val * psp->lastnum);
			psp->lastnum = -1;
			break;
		case NUMBER:
			if (psp->lastnum != -1)
				if (trydate(fields, nf, now, tm, tzp) == -1)
					return -1;
				else
					return NUMBER;
			psp->lastnum = 1;
			break;
		case BEFORE:
		case AFTER:
		case AGO:		/* and hence */
			psp->before = val;
			break;
		case IGNORE:
			break;
		default:
			return -1;	/* CANTHAPPEN */
		}
		return tp->type;
	}
}

/*
 * just parse the relative date in timestr and get back a broken-out date.
 *
 * Numbers are positive.  If we parse a number, we stick it in lastnum.
 * Two numbers in succession is an error.  A seconds or months token is
 * associated with the preceding lastnum (no lastnum means 1, even if this
 * renders plurals counterintuitive!!).  If we hit one of BEFORE, AFTER or
 * AGO, we assume date follows BEFORE or AFTER (AGO == BEFORE now) and
 * parse it, then hop back appropriate number of months and secs.  Note
 * that we first hop back by months, then by secs.
 */
int
prsreldate(timestr, now, tm, tzp)
char *timestr;
struct timeb *now;
register struct tm *tm;
int *tzp;
{
	register int i;
	struct parsestate ps;
	register struct parsestate *psp = &ps;
	register int type;
	int nf;
	time_t dt, modifier;
	char *cp;
	char *fields[MAXDATEFIELDS];
	static char delims[] = "- \t\n/,";

	nf = split(timestr, fields, MAXDATEFIELDS, delims+1);
	if (nf > MAXDATEFIELDS)
		return -1;
	cp = strchr(fields[nf - 1], '\n');
	if (cp != NULL)
		*cp = '\0';

	psp->secs = psp->months = 0;
	psp->before = psp->lastnum = -1;
	/* try to parse <qualifier> ... syntax */
	type = prsrdtoken(fields, nf, 0, now, tm, tzp, psp);
	if (type == ORDINAL) {
		/* TODO: finish this up */
		int ord = psp->lastnum;

		type = prsrdtoken(fields, nf, 1, now, tm, tzp, psp);
		switch (type) {
		case -1:
		default:
			return -1;
		case 0 /* UNIT */: case DAY: case MONTH:
			break;
		}
		/* TODO: ignore noise word "in" or "of" */
		/* TODO: parse month name or year */
		/* TODO: relativistic computation */
	}
	/* parse <timeunits> [<keyword>] */
	for (i = 0; i < nf && psp->before < 0; i++) {
		type = prsrdtoken(fields, nf, i, now, tm, tzp, psp);
		if (type == -1 || type == 0)
			return type;	/* utter success or utter failure */
	}

	/* parse <date> and modify it according to <timeunits> <keyword> */
	if (psp->before >= 0 && type == AGO) {
		if (i < nf)	/* trailing noise after "ago"|"hence"? */
			return -1;
		/* no <date> after "ago"|"hence"; <timeunits> before/after now */
		dt = time(&dt);
	} else if (psp->before < 0 && i >= nf) {
		/* no trailing <keyword> nor <date>; <timeunits> after now */
		psp->before = 0;
		dt = time(&dt);
	} else if (psp->before >= 0 && i >= nf)
		/* <keyword> but no trailing <date> */
		return -1;
	else {
		/* <timeunits> <keyword> <date> */
		struct timeb *ft = NULL;

		dt = trydate(fields + i, nf - i, ft, tm, tzp);
		if (dt == -1)
			dt = trydate(fields, nf, ft, tm, tzp);
		if (dt == -1)
			return -1;
		dt = dateconv(tm, *tzp);
	}
	/* TODO: get the subtleties right here instead of approximating */
	modifier = psp->secs + psp->months*30.5*24L*60L*60L;
	if (psp->before)
		dt -= modifier;
	else
		dt += modifier;
	*tm = *gmtime(&dt);
	*tzp = 0;
	return 0;
}
