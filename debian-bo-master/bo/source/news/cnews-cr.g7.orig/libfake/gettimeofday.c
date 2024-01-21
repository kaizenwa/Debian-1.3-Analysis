/*
 * fake gettimeofday
 *
 * You might think that times() could be used to get sub-second timer
 * resolution, but apart from miscellaneous practical problems, there
 * is no guarantee that its second boundaries are the same as time()'s.
 */

#include <sys/types.h>
#include <time.h>
#include <sys/time.h>
#include <stdlib.h>

/* ARGSUSED */
int
gettimeofday(tvp, tzp)
struct timeval *tvp;
struct timezone *tzp;
{
	(void) time(&tvp->tv_sec);
	tvp->tv_usec = 0;		/* no good alternative */

	/* assert(tzp == NULL); */
}
