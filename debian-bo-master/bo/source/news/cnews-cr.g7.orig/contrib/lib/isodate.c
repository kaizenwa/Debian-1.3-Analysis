/* Conversion routines for ISO3307 dates.  Used in ftp MDTM and nntp2 */
#include <stdio.h>
#include <time.h>
#include "isodate.h"

/*
 * Takes a struct tm and formats it as a date according to ISO3307.
 * struct tm should be properly canonicalized in GMT.
 */
char *
tm2isodate(tmp)
struct tm *tmp;
{
	static char isobuf[sizeof "YYYYMMDDHHMMSS"];

	(void) sprintf(isobuf, "%04.4d%02.2d%02.2d%02.2d%02.2d%02.2d",
		tmp->tm_year + 1900, tmp->tm_mon + 1, tmp->tm_mday,
		tmp->tm_hour, tmp->tm_min, tmp->tm_sec);
	return isobuf;
}

char *
time2isodate(t)
time_t t;
{
	return tm2isodate(gmtime(&t));
}

/*
 * Extract N chars from startp to buf and NUL-terminate.  startp and
 * endp finish at startp + N
 */
#define ADVANCE(N) \
	endp += N; \
	bufp = buf; \
	while (startp != endp) \
		*bufp++ = *startp++; \
	*bufp = '\0';
	
/*
 * Takes an ISO3307 date string and parses it into a struct tm.  Returns
 * NULL if the date string is not one of YYYYMMDDHHMMSS, YYYYMMDDHHMM or
 * YYYYMMDD.
 */
struct tm *
isodate2tm(isodatestr)
char *isodatestr;
{
	char buf[sizeof "YYYY"];
	register char *bufp, *startp, *endp;
	int len, isolen = sizeof "YYYYMMDDHHMMSS" - 1;
	static struct tm tmstruct;
	register struct tm *tmp = &tmstruct;

	endp = startp = isodatestr;
	len = strlen(startp);
	if (len != isolen && len != isolen - 2 && len != isolen - 6)
		return NULL;
	ADVANCE(4);
	tmp->tm_year = atoi(buf) - 1900;
	ADVANCE(2);
	tmp->tm_mon = atoi(buf) - 1;
	ADVANCE(2);
	tmp->tm_mday = atoi(buf);
	if (*startp) {
		ADVANCE(2);
		tmp->tm_hour = atoi(buf);
		ADVANCE(2);
		tmp->tm_min = atoi(buf);
		if (*startp) {
			ADVANCE(2);
			tmp->tm_sec = atoi(buf);
		} else
			tmp->tm_sec = 0;
	} else {
		tmp->tm_hour = tmp->tm_min = tmp->tm_sec = 0;
	}
	tmp->tm_wday = tmp->tm_yday = tmp->tm_isdst = 0;
#ifdef HAVE_GMTOFF
	/*
	 * SunOS, Ultrix have these.  Irix doesn't. 4.3reno comment
	 * indicates they may go away. Not essential for our needs, I
	 * hope nothing else relies on them.
	 */
	tmp->tm_gmtoff = 0;
	tmp->tm_zone = "GMT";
#endif
	return tmp;
}

#ifdef TEST
int
main(argc, argv)
int argc;
char **argv;
{
	time_t t = argv[1] ? atol(argv[1]) : time((time_t *) 0);
	struct tm *tmptr = gmtime(&t), *tmp2;
	char buf[32], *cp = tm2isodate(tmptr);

	tmp2 = isodate2tm(cp);
	printf("%ld %s %s\n", t, cp, time2isodate(t));
	strftime(buf, sizeof buf, "%Y%m%d%H%M%S", tmp2);
	printf("%s\n", buf);
	cp[12] = '\0';
	tmp2 = isodate2tm(cp);
	strftime(buf, sizeof buf, "%Y%m%d%H%M%S", tmp2);
	printf("%s\n", buf);
	cp[8] = '\0';
	tmp2 = isodate2tm(cp);
	strftime(buf, sizeof buf, "%Y%m%d%H%M%S", tmp2);
	printf("%s\n", buf);
	cp[7] = '\0';
	tmp2 = isodate2tm(cp);
	if (tmp2 != NULL) {
		strftime(buf, sizeof buf, "%Y%m%d%H%M%S", tmp2);
		printf("Non NULL tmp! %s\n", buf);
	} else
		printf("NULL\n");
	return 0;
}
#endif
