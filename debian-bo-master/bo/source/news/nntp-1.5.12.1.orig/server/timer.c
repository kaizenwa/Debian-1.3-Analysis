/*
 * Machinery to run routines off timers.
 */
#include "common.h"

#ifdef TIMERS
#ifndef lint
static char rcsid[] =
    "@(#) $Id: timer.c,v 1.9 1994/12/03 21:54:30 sob Exp sob $ (NNTP with TIMERS)";
#endif
#else
#ifndef lint
static char rcsid[] =
    "@(#) $Id: timer.c,v 1.9 1994/12/03 21:54:30 sob Exp sob $ (NNTP without TIMERS)";
#endif
#endif

#ifdef TIMERS
#include <sys/time.h>
#include "timer.h"
#if !defined(USG) || defined(SVR4)
#ifndef FD_SETSIZE
/* Forward compatability */
#define FD_SET(n, p)    ((p)->fds_bits[0] |= (1<<(n)))
#define FD_CLR(n, p)    ((p)->fds_bits[0] &= ~(1<<(n)))
#define FD_ISSET(n, p)  ((p)->fds_bits[0] & (1<<(n)))
#define FD_ZERO(p)      ((p)->fds_bits[0] = 0)
#endif
#endif
/* non-portable */
#ifndef BSD_44
#define BUFFERED_DATA(f) ((f)->_cnt > 0)
#endif

#ifdef SVR4
#include	<signal.h>
#endif

static long lastsecs;

/*
 * Should be called before first call to timer_sleep()
 */
void
timer_init(timers, ntimer)
	register struct timer *timers;
	register int ntimer;
{
	register int i;
	register struct timer *tp;

#ifdef SYSLOG
	if (ntimer <= 0)
		syslog(LOG_ERR,
		    "timer_init(): configuration error, %d timers\n", ntimer);
#endif

	/* Reset all timers */
	for (i = ntimer, tp = timers; i > 0; --i, ++tp)
		tp->left = tp->seconds;

	/* Start clock */
	lastsecs = time((long *)0);
}

/*
 * Sleep until input or next timer needs to be run and then run any
 * expired timers. Returns true if input is available to be read.
 */
int
timer_sleep(timers, ntimer)
	register struct timer *timers;
	register int ntimer;
{
#ifdef	SVR4
	SIGRET nullfunc();
#endif
	register int i, n;
	register struct timer *tp;
	register long secs;
#if defined(USG) && !defined(SVR4) && !defined(hpux)
	long timeout;
	long readfds;
#else
	register struct timeval *timeoutp;
	struct timeval timeout;
	fd_set readfds;
#endif

#ifdef BUFFERED_DATA
	/* No need to do the select if there are characters in the buffer */
	if (BUFFERED_DATA(stdin))
		return(1);
#endif

	/* Length of next timeout is minimum of all "timers" */
#if defined(USG) && !defined(SVR4) && !defined(hpux)
	timeout = -1;
	for (i = ntimer, tp = timers; i > 0; --i, ++tp)
		if (tp->left >= 0 &&
		    (tp->left < timeout || timeout < 0))
			timeout = tp->left;

	/* If active timeouts (this can easily happen), block until input */
	if (timeout < 0)
		timeout = 0;
#ifdef EXCELAN
	readfds = 1<<(fileno(stdin));
	timeout = timeout * 1000;     /* timeout needs to be in milliseconds */
#endif /* EXCELAN */
#else
	timeout.tv_sec = -1;
	timeout.tv_usec = 0;
	for (i = ntimer, tp = timers; i > 0; --i, ++tp)
		if (tp->left >= 0 &&
		    (tp->left < timeout.tv_sec || timeout.tv_sec < 0))
			timeout.tv_sec = tp->left;

	/* If active timeouts (this can easily happen), block until input */
	if (timeout.tv_sec <= 0)
		timeoutp = 0;
	else {
#ifdef SVR4
		signal(SIGALRM, nullfunc);
		alarm(timeout.tv_sec);
		timeoutp = 0;
#else
		timeoutp = &timeout;
#endif
	}

	/* Do select */
	FD_ZERO(&readfds);
	FD_SET(fileno(stdin), &readfds);
#endif /* !USG */
	errno = 0;
#ifdef EXCELAN
	n = select(fileno(stdin) + 1, &readfds, (long*)0, timeout);
#else
	n = select(fileno(stdin) + 1,
	    &readfds, (fd_set*)0, (fd_set*)0, timeoutp);
#endif
	/* "Interrupted system call" isn't a real error */
#ifdef SVR4
	i = errno;
	if (timeout.tv_sec > 0) {
		alarm(0);
		signal(SIGALRM, SIG_IGN);
	}
	errno = i;
#endif
	if (n < 0 && errno != EINTR) {
#ifdef SYSLOG
		syslog(LOG_ERR, "%s read select: %m", hostname);
#endif
		exit(1);
	}

	/* Calculate off seconds since last time */
	secs = time((long *)0) - lastsecs;
	if (secs < 0)
		secs = 0;

	/* Subtract time from "timers" that have time remaining */
	for (i = ntimer, tp = timers; i > 0; --i, ++tp)
		if (tp->left > 0 && (tp->left -= secs) < 0)
			tp->left = 0;

	/* Update lastsecs */
	lastsecs += secs;

	/* If we have input, reset clock on guys that like it that way */
	if (n > 0)
		for (i = ntimer, tp = timers; i > 0; --i, ++tp)
			if (tp->resetoninput)
				tp->left = tp->seconds;

	/* Process "timers" that have timed out */
	for (i = ntimer, tp = timers; i > 0; --i, ++tp) {
		if (tp->left == 0) {
			(tp->subr)();
			/* resetoninput guys only get "reset on input" */
			if (tp->resetoninput)
				tp->left = -1;
			else
				tp->left = tp->seconds;
		}
	}

	/* Indicate no input */
	if (n <= 0)
		return(0);
	return(1);
	
}

#ifdef SVR4
SIGRET
nullfunc()
{
	signal(SIGALRM, nullfunc);
#ifndef VOIDSIG
	return 0;
#endif
}
#endif /* SVR4 */
#endif /* TIMERS */
