/*
 *	(c) Copyright 1990, Kim Fabricius Storm.  All rights reserved.
 *
 *	Calculate an approximate "time_stamp" value for a date
 *	string.  The actual value is not at all critical,
 *	as long as the "ordering" is ok.
 *
 *	The result is NOT a time_t value, i.e. ctime() will
 *	not produce the original Date string.
 *
 *	The date must have format:  [...,] [D]D Mmm YY hh:mm:ss TZONE
 *
 *	Thanks to Wayne Davison for the timezone decoding code.
 */

#include "config.h"

/* pack_date.c */

static int tzone __APROTO((register char *date));
static next_int __APROTO((char **dp));

/* #define DATE_TEST */ /* never define this !! */

#define NN_YEAR_ORIGIN 1987

#undef W
#undef E
#undef DST
#undef UTC
#define W	* (-60) - 
#define E	* 60 +
#define DST	+ 60
#define UTC	60 *

static struct zonetab {
    char *tz_name;
    int tz_offset;
} ztab[] = {
    "a",	UTC 1,		/* UTC+1h */
    "acst",	9 E 30,		/* Cent. Australia */
    "adt",	4 W 0 DST,	/* Atlantic (Canada) */
    "aest",	10 E 0,		/* E. Australia */
    "ast",	4 W 0,		/* Atlantic (Canada) */
    "awst",	8 E 0,		/* W. Australia */
    "b",	UTC 2,		/* UTC+2h */
    "bst",	0 E 0 DST,	/* Great Britain summertime */
    "c",	UTC 3,		/* UTC+3h */
    "cdt",	6 W 0 DST,	/* Central */
    "cest",	1 E 0 DST,	/* Central Europe */
    "cet",	1 E 0,		/* Central Europe */
    "cetdst",	1 E 0 DST,	/* Central Europe */
    "cst",	6 W 0,		/* Central */
    "d",	UTC 4,		/* UTC+4h */
    "dnt",	1 E 0,		/* Denmark */
    "dst",	1 E 0 DST,	/* Denmark */
    "e",	UTC 5,		/* UTC+5h */
    "edt",	5 W 0 DST,	/* Eastern US */
    "eest",	2 E 0 DST,	/* Eastern Europe */
    "eet",	2 E 0,		/* Eastern Europe */
    "eetdst",	2 E 0 DST,	/* Eastern Europe */
    "est",	5 W 0,		/* Eastern US */
    "f",	UTC 6,		/* UTC+6h */
    "g",	UTC 7,		/* UTC+7h */
    "gmt",	0,		/*  */
    "h",	UTC 8,		/* UTC+8h */
    "hdt",	10 W 0 DST,	/* Hawaii/Alaska */
    "hkt",	8 E 0,		/* Hong Kong */
    "hst",	10 W 0,		/* Hawaii/Alaska */
    "i",	UTC 9,		/* UTC+9h */
    "ist",	2 E 0,		/* Israel */
    "jst",	9 E 0,		/* Japan */
    "k",	UTC 10,		/* UTC+10h */
    "kdt",	9 E 0 DST,	/* Korea */
    "kst",	9 E 0,		/* Korea */
    "l",	UTC 11,		/* UTC+11h */
    "m",	UTC 12,		/* UTC+12h */
    "mdt",	7 W 0 DST,	/* Mountain US */
    "mest",	1 E 0 DST,	/* Central Europe */
    "met",	1 E 0,		/* Central Europe */
    "metdst",	1 E 0 DST,	/* Central Europe */
    "mst",	7 W 0,		/* Mountain */
    "n",	UTC -1,		/* UTC-1h */
    "ndt",	3 W 30 DST,	/* Nfld. (Canada) */
    "nst",	3 W 30,		/* Nfld. (Canada) */
    "nzdt",	12 E 0 DST,	/* New Zealand */
    "nzst",	12 E 0,		/* New Zealand */
    "o",	UTC -2,		/* UTC-2h */
    "p",	UTC -3,		/* UTC-3h */
    "pdt",	8 W 0 DST,	/* Pacific */
    "pst",	8 W 0,		/* Pacific */
    "q",	UTC -4,		/* UTC-4h */
    "r",	UTC -5,		/* UTC-5h */
    "s",	UTC -6,		/* UTC-6h */
    "t",	UTC -7,		/* UTC-7h */
    "u",	UTC -8,		/* UTC-8h */
    "ut",	UTC 0,		/* UTC */
    "utc",	UTC 0,		/* UTC */
    "v",	UTC -9,		/* UTC-9h */
    "w",	UTC -10,	/* UTC-10h */
    "west",	0 E 0 DST,	/* Western Europe */
    "wet",	0 E 0,		/* Western Europe */
    "wetdst",	0 E 0 DST,	/* Western Europe */
    "wst",	8 E 0,		/* Alternate W. Australia */
    "x",	UTC -11,	/* UTC-11h */
    "y",	UTC -12,	/* UTC-12h */
    "ydt",	9 W 0 DST,	/* Yukon */
    "yst",	9 W 0,		/* Yukon */
    "z",	UTC 0,		/* UTC */
    NULL,	0
};

#undef MAXZ
#define MAXZ	10

static int tzone(date)
register char *date;
{
    register int i, n;
    static char zone[MAXZ], num[MAXZ];
    register struct zonetab *z;
    int adjust, sign;

    i = 0;
    while (*date && isascii(*date) && isspace(*date)) date++;
	
    for ( ; *date && isascii(*date) ; date++) {
	if (*date == '+' || *date == '-' || isdigit(*date))
	    goto numeric_zone;
	if (isspace(*date)) break;
	if (!isalpha(*date)) continue;	/* p.s.t. -> pst */
	if (i == MAXZ) continue;
	zone[i++] = isupper(*date) ? tolower(*date) : *date;
    }

    while (*date && isascii(*date) && isspace(*date)) date++;

    /* huh? */
    if (*date && i < MAXZ-3) {
	if (date[0] != 'D' && date[0] != 'd') goto no_dst;
	if (date[1] != 'S' && date[1] != 's') goto no_dst;
	if (date[2] != 'T' && date[2] != 't') goto no_dst;
	zone[i++] = 'd';
	zone[i++] = 's';
	zone[i++] = 't';
    }

 no_dst:
    if (i == 0) return 0;
    adjust = 0;
    if (*date != '+' && *date != '-') goto non_numeric;

 numeric_zone:			/* {+-}[H]H[MM] */
    switch (*date) {
     case '-':
	date++;
	sign = -1;
	break;
     case '+':
	date++;
     default:
	sign = 1;
	break;
    }

    adjust = 0;
    for (n = 0; n < MAXZ && *date && isascii(*date) && isdigit(*date); )
	num[n++] = *date++;
    num[n] = NUL;

    switch (n) {
     case 0:
	break;
     case 3:	/* +HMM */
	adjust = atoi(num+1);
     case 1:	/* +H */
	adjust += (num[0] - '0') * 60;
	break;
     default:	/* +HHMM */
	num[4] = NUL;
	adjust = atoi(num + 2);
	num[2] = NUL;
     case 2:	/* +HH */
	adjust += atoi(num) * 60;
	break;
    }
    adjust *= sign;
    if (i == 0) return adjust;

 non_numeric:
    zone[i] = NUL;

    for (z = ztab; z->tz_name != NULL; z++) {
	i = zone[0] - z->tz_name[0];
	if (i == 0)		/* same first byte? */
	    i = strcmp(zone, z->tz_name);
	if (i > 0)
	    continue;
	if (i < 0)
	    break;
	return z->tz_offset + adjust;
    }
    return adjust;
}

static int
next_int(dp)
char **dp;
{
    register char *str = *dp;
    register i = 0;

    while (*str && !isdigit(*str)) str++;

    while (*str && isdigit(*str))
	i = (i * 10) + *str++ - '0';

    *dp = str;
    return i;
}

static int month_table[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
#define leap_year(y) ((y)&3 == 0  &&  ((y)%100 != 0 || (y)%400 == 0))

static int
month_days(year, mon)
int year, mon;
{
    return month_table[mon] + (mon==1 && leap_year(year));
}

time_stamp pack_date(date)
char *date;
{
    register time_stamp res;
    register int min, hour, day, mon, year;

    if (date == NULL || (day = next_int(&date)) == 0) return 0;

    while (*date && isspace(*date)) date++;

    if (date[0] == NUL || date[1] == NUL || date[2] == NUL) return 0;
    switch (date[0]) {
     case 'J': case 'j':
	if (date[1] == 'a' || date[1] == 'A') { mon = 0; break; }
	if (date[2] == 'n' || date[2] == 'N') { mon = 5; break; }
	mon = 6; break;
     case 'F': case 'f':
	mon = 1; break;
     case 'M': case 'm':
	if (date[2] == 'r' || date[2] == 'R') { mon = 2; break; }
	mon = 4; break;
     case 'A': case 'a':
	if (date[1] == 'p' || date[1] == 'P') { mon = 3; break; }
	mon = 7; break;
     case 'S': case 's':
	mon = 8; break;
     case 'O': case 'o':
	mon = 9; break;
     case 'N': case 'n':
	mon = 10; break;
     case 'D': case 'd':
	mon = 11; break;
     default:
	return 0;
    }

    year = next_int(&date);
    hour = next_int(&date);
    min = next_int(&date);
    if (*date == ':') next_int(&date);

    if (year >= 100) year -= 1900;	/* xxYY -> YY */
    year -= NN_YEAR_ORIGIN - 1900;	/* base is NN_YEAR_ORIGIN */
    if (year < 0) year += 100;

    /* Set `min' to be the number of minutes after midnight UTC.  */
    min += hour*60 - tzone(date);
    for (;  min < 0;  min += 24 * 60)
	if (--day <= 0) {
	    if (--mon < 0) {
		--year;
		mon = 11;
	    }
	    day = month_days(year + NN_YEAR_ORIGIN, mon);
	}
    for (;  24 * 60 <= min;  min -= 24 * 60)
	if (month_days(year + NN_YEAR_ORIGIN, mon) < ++day) {
	    if (11 < ++mon) {
		++year;
		mon = 0;
	    }
	    day = 1;
	}

    res = (year * 12 + mon) * 31 + day - 1;
    res *= 24 * 60;
    res += min;

    return res;
}


#ifdef DATE_TEST


main()
{
    char buffer[128];
    char *dp;
    unsigned long t;

    while (fgets(buffer, 128, stdin)) {
	if (strncmp(buffer, "Date:", 5)) continue;

	dp = strchr(buffer, ':');
	if (dp == NULL) continue;
	dp++;
	while (isspace(*dp)) dp++;
	t = pack_date(dp);
	printf("%lu\t%s\n", t, dp);
    }

    exit(0);
}

#endif
