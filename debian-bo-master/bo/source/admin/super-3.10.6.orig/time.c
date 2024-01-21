#include "super.h"

#define ANYDAY 7

struct weekDay {
    char name[20];
    char abbr[20];
};

#ifdef HAVE_LOCALE_H
    /* We will pick up weekday names dynamically; make the first name a
     * null string so we know that initialization has yet to be done.
     */
    static struct weekDay weekday[7] = { { "", ""}, };
#else
    static struct weekDay weekday[7] = {
		{ "sunday",	"sun", },
		{ "monday",	"mon", },
		{ "tuesday",	"tue", },
		{ "wednesday",	"wed", },
		{ "thursday",	"thu", },
		{ "friday",	"fri", },
		{ "saturday",	"sat"  }
    };
#endif

char *
dayname(n)
int n;		/* day number: 0..6 = sun..sat; 7 = any */
{
    /* Returns ptr to day of week */
    static char *anyday="*";

#ifdef HAVE_LOCALE_H
    if (*(weekday[0].name) == '\0')
	readtime_init();
#endif

    if (n < 0 || n > 6)
	return anyday;
    else
	return weekday[n].name;
}

int
daynum(t)
int t;		/* A Unix time */
{
    /* Returns day of week, with 0 = sunday.  This routine takes the input
     * time as given -- it doesn't assume GMT or any such thing -- and
     * returns the day number: you should use localtime() or gmtime() if
     * you have them available.
     */
    int jd = (int) (2440587.5 + ((double) t) / 86400.);
    int i = (jd+1) / 7;
    return jd - 7 * i + 1;
}

int
InsertTimeList(str, wdlist, tl, timetype, invert)
char *str;	/* Contains one of the time types accepted by readtime(),
		 * and extended to allow brace-expansion */
char **wdlist;	/* list-expanded form of str */
TimeList *tl;	/* Insert time list elements at tl->next */
char *timetype;	/* "per-cmd" or "global": a string to use in info msgs */
int invert;	/* Inverts the test */
{
    /* Checks if time is in the range specified by the time string str.
     * Returns
     *	-1 on syntax error, malloc error, or similar;
     *	0 otherwise.
     * Side effect: sets the matches.time field.
     */
    int match;
    int iwd;
    char *tok;
    TimeList *new;

    for (iwd=0, match=0; (tok=wdlist[iwd]); iwd++) {
	new = (TimeList *) malloc(sizeof(TimeList));
	if (!new)
	    return Error(0, 0, "%t\n\tFailed to malloc space for time entry\n");
	new->next = tl->next;
	new->te.invert = invert;
	/* interpret pat */
	if (readtime(tok, &new->te.begin, &new->te.end, &new->te.day) == -1)
	    return -1;
	if (debug)
		(void) fprintf(stderr,
	"\tInsert %s time pattern: %stime~%s (min=%d-%d day=%d [dayname=%s])\n",
		    timetype,
		    invert ? "!" : "", tok, new->te.begin, new->te.end,
		    new->te.day, dayname(new->te.day));
	tl->next = new;
    }
    return 0;
}

void
matchtime(our, tl)
OurTime *our;	/* A time entry to match */
TimeList *tl;	/* A list of times to match against; 1st used is tl->next */
{
    /* The time list created by the InsertTimeList function
     * is in reverse order, so we only need to find the first
     * entry in the list that is a match (+ or -) and stop there.

     * But if there are no matches, we scan all entries.

     * Side effects:
     *	If an entry is matched, matches.time is set to 0 or 1,
     *	    according as the entry is/is not inverted.  Otherwise unmodified.
     *	If an entry is non-inverted, matches.alltime_invert is set to 0,
     *      otherwise unmodified.  Note that this will include all entries
     *      only if there was no match, because scanning stops with a match.

     */

    /* The caller should interpret the returned matches as
     * follows.
     * Case 1:  Our time 12:30
     *       time~8-17    !time~12-13   time~23-24
     * a. time~8-17 matches, user allowed.
     * b. time~12-13 matches, ! means user disallowed.
     * c. time~23-24 doesn't match, doesn't change allow/disallow.
     * Therefore user disallowed.

     * Case 2:  Our time 12:30
     *     time~8-17    !time~12-13   !time~23-24
     * a. time~8-17 matches, user allowed.
     * b. time~12-13 matches, ! means user disallowed.
     * c. time~23-24 doesn't match, doesn't change allow/disallow.
     * Therefore user disallowed.

     * Case 3:  Our time 12:30
     *     !time~23-24
     * a. time~23-24 doesn't match, doesn't change allow/disallow.
     * b. Thus fall back on default.  Default is
     *		i)  allow if _all_ statements are inverted;
     *		ii) else disallow.
     * Therefore user is allowed.

     * Case 4:  Our time 17:30
     *     time~8-17    !time~12-13   !time~23-24
     * a. time~8-17 doesn't match, doesn't change allow/disallow.
     * b. time~12-13 doesn't match, doesn't change allow/disallow.
     * c. time~23-24 doesn't match, doesn't change allow/disallow.
     * d. Thus fall back on default.  Default is
     *		i)  allow if _all_ statements are inverted;
     *		ii) else disallow.
     * Therefore user is disallowed.
     */

    int match;

    for (tl=tl->next, match=0; tl && !match; tl=tl->next) {
	if (!tl->te.invert)
	    matches.allinverted = 0;

	if (our->min >= tl->te.begin && our->min <= tl->te.end &&
			    (tl->te.day == ANYDAY || our->day == tl->te.day)) {
	    /* matches */
	    match = 1;
	    matches.time = tl->te.invert ? 0 : 1;
	    if (debug)
		(void) fprintf(stderr, "\t%s: %stime~%d:%.2d-%d:%.2d/%s\n",
		    tl->te.invert ? "Permission denied" : "Permission allowed",
		    tl->te.invert ? "!" : "", 
		    tl->te.begin/60, tl->te.begin%60,
		    tl->te.end/60, tl->te.end%60,
		    dayname(tl->te.day));
	} else {
	    /* Our time is not in the range of this time entry. */
	    if (debug) {
	    (void) fprintf(stderr,
		"\tNot applicable: %stime~%d:%.2d-%d:%.2d/%s\n",
		    tl->te.invert ? "!" : "", 
		    tl->te.begin/60, tl->te.begin%60,
		    tl->te.end/60, tl->te.end%60,
		    dayname(tl->te.day));
	    }
	}
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Frees all elements in a TimeList, except the one it's given.
 * The "next" field of that element is set NULL.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
free_TimeList(tl)
TimeList *tl;
{
    TimeList *tlp;

    if (!tl || !tl->next)
	return;
    tlp = tl->next;
    tl->next = NULL;
    for (tl=tl->next ; tl; tl = tlp) {
	tlp = tl->next;
	free(tl);
    }
}
#ifdef HAVE_LOCALE_H
void
readtime_init()
{
    struct tm ti;
    int i;

    /* We know the following to be a Sunday */
    ti.tm_sec = 0;
    ti.tm_min = 0;
    ti.tm_hour = 12;
    ti.tm_mday = 4;
    ti.tm_wday = 0;
    ti.tm_mon = 0;
    ti.tm_year = 70;

    /* Figure out the days of the week in the current locale */
    if (debug)
	fprintf(stderr,
	"\tInitializing days of week.  Full names and official abbr's:\n\t");

    for (i = 0; i < 7; i++) {
	strftime(weekday[i].name, sizeof(weekday[0].name)-1, "%A", &ti);
	strtolower(weekday[i].name);
	strftime(weekday[i].abbr, sizeof(weekday[0].abbr)-1, "%a", &ti);
	strtolower(weekday[i].abbr);
	if (debug) {
	    fprintf(stderr, "%s (%s)  ", weekday[i].name, weekday[i].abbr);
	    if (i == 3)
		fputs("\n\t", stderr);
	}
	ti.tm_mday++;
	ti.tm_wday++;
    }

    if (debug)
	fputs("\n", stderr);
}
#endif

int
readtime(str, t1, t2, d)
char *str;
short *t1;	/* Returned with 1st time, units=minutes, range = 0..1439 */
short *t2;	/* Returned with 2nd time, units=minutes, range = 0..1439 */
char *d;	/* Returned with day number (0=Sunday) or ANYDAY */
{
    /* str is a string containing one of the following patterns:
     *
     * ""			(implies all days, all hours)
     * [/]dayname			(implies all hours)
     * hh[:mm]-hh[:mm]/dayname
     * hh[:mm]-hh[:mm]		(implies all days)
     * xhh[:mm]/dayname		(implies all days; x is one of <, >, <=, >= )
     * xhh[:mm]			(implies all days; x is one of <, >, <=, >= )
     *
     * The valid daynames are (case-insensitive) either an official abbreviated
     * day name in the current locale; a 3-or-more character abbreviation
     * of the full weekday; or "*", meaning any day.

     * For convenience, the upper time can be 24:00, but it is converted
     * to 11:59.

     * Returns: -1 on invalid time pattern; 0 otherwise.

     */
    char *s, *p;
    int hh1, mm1;
    int hh2, mm2;
    int i, l;
    int has_relop;
#ifdef HAVE_ENUM
    enum { LT, LE, GT, GE } relop;
#else
    /* No enums! */
#define LT 0
#define LE 1
#define GT 2
#define GE 3
    int relop;
#endif

#ifdef HAVE_LOCALE_H
    if (*(weekday[0].name) == '\0')
	readtime_init();
#endif

    s = str;
    has_relop = 0;
    if (*s == '<' || *s == '>') {
	/* Must be xhh[:mm][/dayname] */
	has_relop = 1;
	switch (*s++) {
	case '<':
	    relop = (*s == '=') ? (s++, LE) : LT;
	    break;
	case '>':
	    relop = (*s == '=') ? (s++, GE) : GT;
	    break;
	default:
	    return Error(0, 0, "%t\n\tInvalid time range <%s>\n", str);
	}
	if (!isdigit(*s))
	    return Error(0, 0,
		"%t\n\tInvalid time range <%s> (invalid hour part)\n",
	    str);

	hh1 = strtol(s, &p, 10);
	if (p == s || hh1 > 24 || hh1 < 0)
	    return Error(0, 0,
		"%t\n\tInvalid time range <%s> (invalid hour part)\n", str);

	s = p;
	if (*s != ':') {
	    /* minutes not given */
	    mm1 = 0;
	} else {
	    mm1 = strtol(++s, &p, 10);
	    if (p == s || mm1 > 59 || mm1 < 0 || (mm1 > 0 && hh1 > 24))
		return Error(0, 0,
		"%t\n\tInvalid time range <%s> (invalid minute part)\n", str);
	}

	s = p;

	switch (relop) {
	case LT:
	    *t1 = 0;
	    *t2 = hh1*60 + mm1 - 1;
	    break;
	case LE:
	    *t1 = 0;
	    *t2 = hh1*60 + mm1;
	    break;
	case GT:
	    *t1 = hh1*60 + mm1 + 1;
	    *t2 = 24*60 - 1;
	    break;
	case GE:
	    *t1 = hh1*60 + mm1;
	    *t2 = 24*60 - 1;
	    break;
	default:
	    return Error(0, 0,
			"%t\n\tInternal time range error -- relop is %d\n",
			relop);
	}

    } else if (isdigit(*str)) {
	/* It must begin hh:mm-hh:mm */
	s = str;

	hh1 = strtol(s, &p, 10);
	if (p == s || hh1 > 24 || hh1 < 0)
	    return Error(0, 0,
		"%t\n\tInvalid time range <%s> (invalid first hr)\n", str);

	s = p;
	if (*s != ':') {
	    mm1 = 0;
	} else {
	    mm1 = strtol(++s, &p, 10);
	    if (p == s || mm1 > 59 || mm1 < 0 || (mm1 > 0 && hh1 > 24))
		return Error(0, 0,
		    "%t\n\tInvalid time range <%s> (invalid first min)\n", str);
	}

	*t1 = hh1*60 + mm1;

	s = p;
	if (*s++ != '-')
	    return Error(0, 0,
	    "%t\n\tInvalid time range <%s> (expected '-' in hh:mm-hh:mm)\n",
	    str);

	hh2 = strtol(s, &p, 10);
	if (p == s || hh2 > 24 || hh2 < 0)
	    return Error(0, 0,
		"%t\n\tInvalid time range <%s> (invalid second hr)\n", str);

	s = p;
	if (*s != ':') {
	    mm2 = 0;
	} else {
	    mm2 = strtol(++s, &p, 10);
	    if (p == s || mm2 > 59 || mm2 < 0 || (mm2 > 0 && hh2 > 24))
		return Error(0, 0,
		    "%t\n\tInvalid time range <%s> (invalid second min)\n",
		    str);
	}

	s = p;
	if (*s != '\0' && *s != '/')
	    return Error(0, 0,
	    "%t\n\tInvalid time range <%s> (invalid char after time range)\n",
	    str);

	*t2 = hh2*60 + mm2;

    } else {
	/* We've seen no time part; must be dayname only; time is all hrs */
	s = str;
	*t1 = 0;
	*t2 = 24 * 60;
    }

    if (*s == '/')	/* Skip field separator, if present */
	s++;

    if (*s == '\0' || strcmp(s, "*") == 0) {
	*d = ANYDAY;	/* all days */
    } else {
	/* It must be a weekday, expressed in the current locale.
	 * Allow just dayname or /dayname.
	 */
	char buf[20];
	strcpy(buf, s);
	strtolower(buf);
	for (i=0; i<7; i++) {
#if 0
	    fprintf(stderr, "HERE: s=<%s> buf=<%s> day=<%s> abbr=<%s>\n",
		s, buf, weekday[i].name, weekday[i].abbr);
#endif
	    if (strlen(buf) < 3) {
		;	/* not accepted -- must be at least three chars */
	    } else if (strncmp(weekday[i].name, buf, (l=strlen(buf))) == 0) {
		*d = i;
		break;
	    } else if (strcmp(weekday[i].abbr, buf) == 0) {
		*d = i;
		break;
	    }
	}
	if (i >= 7)
	    return Error(0, 0,
		"%t\n\tInvalid time range <%s> (invalid day field)\n",
		str);
    }
    /* Convert 24:00 to 23:59 */
    if (*t1 == 24*60)
	(*t1)--;
    if (*t2 == 24*60)
	(*t2)--;
    if (*t1 < 0 || *t2 < 0 || *t1 > 24*60-1 || *t2 > 24*60-1)
	return Error(0, 0, "%t\n\tInvalid time range <%s>\n", str);
    return 0;
}
