#ifndef lint
static char	sccsid[] = "@(#)$Id: fakesyslog.c,v 1.9 1994/12/09 02:52:18 sob Exp sob $";
#endif

/*
 * Fake syslog routines for systems that don't have syslog.
 * Taken from an idea by Paul McKenny, <mckenny@sri-unix.arpa>.
 * (Unfortunately, Paul, I can't distribute the real syslog code
 * as you suggested ... sigh.)
 *
 * Warning: this file contains joe code that may offend you.
 */

#include "../conf.h"

#ifdef FAKESYSLOG

#include "fakesyslog.h"

#include <stdio.h>
#include <sys/signal.h>
#include <sys/types.h>

#ifdef FAKEAPPEND
#include <sys/file.h>
#endif

#ifdef FCNTL
#include <fcntl.h>
#endif

extern	int	errno;
extern	int	sys_nerr;
extern	char	*sys_errlist[];

static FILE	*logfp;
static int	failed = 0;
static char	*ident = "syslog";
static int 	opt = 0;
#ifdef LOG_DAEMON
static int	fac = 0;
#endif

extern char	*strcpy(), *strcat(), *ctime();
extern time_t	time();

SIGRET
resetlog()
{
	closelog();
	failed = 0;
	if (logfp == NULL) {
#ifdef LOG_DAEMON
		openlog(ident, opt, fac);
#else
		openlog(ident, opt);
#endif
		if (logfp == NULL)
			failed = 1;
	}
	(void)signal(SIGHUP, resetlog);
#ifndef VOIDSIG
	return 0;
#endif
}

#ifdef LOG_DAEMON
openlog(newident,logopt,facility)
	char *newident;
	int logopt, facility;
#else
openlog(newident,logopt)
	char *newident;
	int logopt;
#endif
{
#ifdef FAKEAPPEND
/*
 * why can't stdio give us the capability of O_APPEND?
 */
	int fd;

	fd = open(FAKESYSLOG, O_WRONLY|O_APPEND, 0664);
	if (fd < 0)
		logfp = NULL;
	else
		logfp = fdopen(fd, "a");
#else
	logfp = fopen(FAKESYSLOG, "a");
#endif

	(void)signal(SIGHUP, resetlog);

	if (newident && *newident)
		ident = newident;
	opt = logopt;
#ifdef LOG_DAEMON
	fac = facility;
#endif
}

closelog()
{
	if (logfp) {
		(void)fclose(logfp);
		failed = 0;
		logfp = NULL;
	}
}

/*ARGSUSED*/
setlogmask(maskpri)
	int maskpri;
{
}

syslog(pri, msg, x1, x2, x3, x4, x5, x6)
	int	pri;
	char	*msg, *x1, *x2, *x3, *x4, *x5, *x6;
{
	char		buf[1024];
	char		*cp, *bp;
	time_t		clock;

	if (failed)
		return;

	if (logfp == NULL) {
#ifdef LOG_DAEMON
		openlog(ident, opt, fac);
#else
		openlog(ident, opt);
#endif
		if (logfp == NULL) {
			failed = 1;
			return;
		}
	}

	(void) time(&clock);
	(void) strcpy(buf, ctime(&clock)+4);
	*(bp = buf + 16) = '\0';

#if 0
	(void) sprintf(bp, "localhost %s", ident ? ident : "");
#else
	(void) sprintf(bp, "local %s", ident ? ident : "");
#endif
	bp += strlen(bp);

	if (opt&LOG_PID) {
		/* don't cache getpid() - who knows when we'll fork() */
		(void) sprintf(bp, "[%d]", getpid());
		bp += strlen(bp);
	}

	if (ident) {
		(void) strcat(bp, ": ");
		bp += 2;
	} else {
		(void) strcat(bp, " ");
		bp ++;
	}

	for (cp = msg; *cp; cp++) {
		if (*cp == '%' && cp[1] == 'm') {
			*bp = '\0';
			if (errno >= sys_nerr || errno < 0) {
				char	work[32];
				(void)sprintf(work, "unknown error #%d", errno);
				(void)strcat(bp, work);
			} else
				(void)strcat(bp, sys_errlist[errno]);
			bp = buf + strlen(buf);
			cp++;
		} else {
			*bp++ = *cp;
		}
	}
	*bp = '\0';
	/* Ah, the semantic security of C ... */
	if (bp[-1] != '\n')
		(void) strcat(bp, "\n");

	fprintf(logfp, buf, x1, x2, x3, x4, x5, x6);
	(void) fflush(logfp);
}

#endif
