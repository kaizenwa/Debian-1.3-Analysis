#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>
#include "config.h"
#include "libcnews.h"
#include "debug.h"
#include "log.h"

#include <fcntl.h>	/* sys/file.h on some systems? */

/* imports */
time_t time();

char *logstr;				/* remote host name for logs */

static time_t lastlogtime = 0;
static struct tms last_tms;

void
log3int(fmt, i, j, k)
char *fmt;
int i, j, k;
{
	/*
	 * magic length buffer , we check it.  Note that on some
	 * machines, syslog croaks on strings above a certain length.
	 * This length is implementation dependent.
	 */
	char buf[256];
	int fmtlen;
	int len;

	if (logstr == NULL)
		return;
	len = strlen(logstr);
	fmtlen = strlen(fmt) + 1 + 3 * 20 + 1; /* space + 3 ints + NUL */
	if (fmtlen + len > sizeof buf) {
		len = sizeof buf - fmtlen;
		(void) strncpy(buf, logstr, len);
	} else
		(void) strcpy(buf, logstr);
	buf[len++] = ' ';
	buf[len] = '\0';
	(void) sprintf(&buf[len], fmt, i, j, k);
	log(buf);
}

void
logtimes(s)
char *s;
{
	struct tms tmsbuf;
	double dtu, dts;
#define FMT "%.32s %.32s user %.2g system %.2g elapsed %ld"
	char buf[64 + 2 * 32 + 20 + sizeof FMT];	/* 2 doubles, 1 int */
	time_t now;

	if (s == NULL && lastlogtime != 0)
		return;
	if (times(&tmsbuf) < 0)
		error("times() failed", "");
	now = time(&now);
	if (now < 0)
		error("time() failed", "");
	if (s != NULL && lastlogtime != 0) {
		dtu = (tmsbuf.tms_utime - last_tms.tms_utime) / HZ;
		dts = (tmsbuf.tms_stime - last_tms.tms_stime) / HZ;
		sprintf(buf, FMT, logstr, s, dtu, dts,
			(long) (now - lastlogtime));
		log(buf);
	}
	ddprintf(dfp, "lastlogtime = %ld, now = %ld, last_tms = {%ld, %ld, %ld, %ld}, tmsbuf = {%ld, %ld, %ld, %ld}\n",
		 (long) lastlogtime,(long) now,
		 (long) last_tms.tms_utime, (long) last_tms.tms_stime,
		 (long) last_tms.tms_cutime, (long) last_tms.tms_cstime,
		 (long) tmsbuf.tms_utime, (long) tmsbuf.tms_stime,
		 (long) tmsbuf.tms_cutime, (long) tmsbuf.tms_cstime);
	lastlogtime = now;
	last_tms = tmsbuf;
}

#ifdef LOGFILE	/* Use a file to log statistics */
#include <sys/timeb.h>

FILE *logfp;
char *logcookie;

/* ARGSUSED */
void
loginit(s)
char *s;
{
	char pidstr[32];	/* large enough for an int in decimal */
	char *logfile = LOGFILE;
	int fd;

	if (logfile[0] != '/')
		logfile = ctlfile(logfile);
#ifdef O_APPEND
	if ((fd = open(logfile, O_WRONLY|O_APPEND|O_CREAT, 0666)) < 0)
		error("open(%s, O_WRONLY|O_APPEND|O_CREAT) failed", logfile);
	if ((logfp = fdopen(fd, "a+")) == NULL)
		error("fdopen(open(%s), \"a+\") failed", logfile);
#else
	logfp = efopen(logfile, "a+");
	fd = 0;					/* avoid lint noise */
#endif
	sprintf(pidstr, "[%d]: ", getpid());
	/*
	 * In normal syslog, the "." would be the hostname.  That's
	 * redundant here, so we ignore it.  The . keeps the column
	 * count right for scripts that scan the logs.  We also don't
	 * bother to glue the string s before the pid, since we'll
	 * usually have one file per daemon type.
	 */
	logcookie = str3save(" . ", "" /* s */, pidstr);
	logtimes((char *) 0);
}

void
log(s)
char *s;
{
	timestamp(logfp, (time_t *) NULL);
	(void) fputs(logcookie, logfp);
	(void) fputs(s, logfp);
	(void) putc('\n', logfp);
}
#endif /* LOGFILE */



#ifdef LOGLEVEL	/* Use BSD syslog */

#include <syslog.h>

void
loginit(s)
char *s;
{
#ifdef LOG_DAEMON
	openlog(s, LOG_PID, LOGLEVEL);
#else
	openlog(s, LOG_PID);
#endif
	logtimes((char *) 0);
}

void
log(s)
char *s;
{
	syslog(LOG_INFO, "%s", s);
}
#endif /* LOGLEVEL */
