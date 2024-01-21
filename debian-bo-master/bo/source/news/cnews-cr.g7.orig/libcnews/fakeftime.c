/*
 * fake ftime, assumes you have gettimeofday
 */

#include <sys/types.h>
#include <sys/time.h>
/* <sys/time.h> should have included <time.h>, cross your fingers... */
#include <sys/timeb.h>
#include <stdlib.h>

#define	DUNNO	1000000L	/* impossible seconds timezone offset */

/*
 - secswest - intuit the timezone, portably
 */
static long			/* seconds west of GMT, or DUNNO */
secswest(now, localp)
time_t now;
struct tm *localp;		/* result of localtime(&now) */
{
	struct tm localt;
	struct tm gmt;
	register struct tm *a;
	register struct tm *b;
#	define	LG	a = &localt, b = &gmt
#	define	GL	a = &gmt, b = &localt
	register long sw;
	register char *newstz;

	/* special hack primarily for testing */
	newstz = getenv("NEWSTZ");
	if (newstz != NULL)
		return(atol(newstz));

	/* get local and GMT, broken down */
	localt = *localp;
	gmt = *gmtime(&now);

	/* simplify things: decide whether local is ahead of or behind GMT */
	if (localt.tm_year < gmt.tm_year)
		LG;				/* local first */
	else if (localt.tm_year > gmt.tm_year)
		GL;				/* GMT first */
	else if (localt.tm_yday < gmt.tm_yday)
		LG;
	else if (localt.tm_yday > gmt.tm_yday)
		GL;
	else if (localt.tm_hour < gmt.tm_hour)
		LG;
	else if (localt.tm_hour > gmt.tm_hour)
		GL;
	else if (localt.tm_min < gmt.tm_min)
		LG;
	else if (localt.tm_min > gmt.tm_min)
		GL;
	else if (localt.tm_sec < gmt.tm_sec)
		LG;
	else if (localt.tm_sec > gmt.tm_sec)
		GL;
	else
		return(0);		/* no difference -- local is GMT! */
	/* a is now the earlier time, b the later */

	/* deal with year boundaries */
	if (a->tm_year != b->tm_year && a->tm_yday >= 364 && b->tm_yday == 0)
		a->tm_yday = -1;	/* Dec 31 == Jan 0 */
	else if (a->tm_year != b->tm_year)
		return(DUNNO);		/* something strange here */

	/* we're now in the same year; deal with day boundaries */
	if (a->tm_yday != b->tm_yday && a->tm_yday == b->tm_yday - 1)
		a->tm_hour -= 24;	/* 2200 Sun == -0200 Mon */
	else if (a->tm_yday != b->tm_yday)
		return(DUNNO);

	/* we're now in the same day, so now it's easy */
	sw = (b->tm_hour - a->tm_hour)*3600L + (b->tm_min - a->tm_min)*60L +
						(b->tm_sec - a->tm_sec);

	/* and get the sign right */
	if (a == &gmt)			/* local ahead, i.e. east, of GMT */
		return(-sw);
	else
		return(sw);
}

int
ftime(tb)
struct timeb *tb;
{
	struct timeval tv;
	int ret;
	struct tm *nowp;
	long sw;

	ret = gettimeofday(&tv, (struct timezone *)NULL);

	tb->time = tv.tv_sec;
	tb->millitm = (tv.tv_usec + 500) / 1000;

	nowp = localtime(&tb->time);
	tb->dstflag = nowp->tm_isdst;

	sw = secswest(tb->time, nowp);
	if (sw != DUNNO)
		tb->timezone = (sw + 30) / 60;
	else
		tb->timezone = 0;	/* for lack of anything better to do */

	return(ret);
}
