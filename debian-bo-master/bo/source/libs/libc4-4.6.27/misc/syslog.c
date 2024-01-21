/*
 * Copyright (c) 1983, 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)syslog.c	8.4 (Berkeley) 3/18/94";
#endif /* LIBC_SCCS and not lint */

/*
 * SYSLOG -- print message on log file
 *
 * This routine looks a lot like printf, except that it outputs to the
 * log file instead of the standard output.  Also:
 *	adds a timestamp,
 *	prints the module name in front of the message,
 *	has some other formatting types (or will sometime),
 *	adds a newline on the end of the message.
 *
 * The output of this routine is intended to be read by syslogd(8).
 *
 * Author: Eric Allman
 * Modified to use UNIX domain IPC by Ralph Campbell
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <sys/syslog.h>
#if 0
#include "syslog.h"
#include "pathnames.h"
#endif

#include <sys/uio.h>
#include <sys/wait.h>
#include <netdb.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>
#include <stdarg.h>
#include <paths.h>
#include <stdio.h>

static int	LogFile = -1;		/* fd for log */
static int	connected;		/* have done connect */
static int	LogStat = 0;		/* status bits, set by openlog() */
static const char *LogTag = "syslog";	/* string to tag the entry with */
static int	LogFacility = LOG_USER;	/* default facility code */

/*
 * syslog, vsyslog --
 *     print message on log file; output is intended for syslogd(8).
 */
void
syslog(int pri, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vsyslog(pri, fmt, ap);
	va_end(ap);
}

void
vsyslog(pri, fmt, ap)
	int pri;
	const char *fmt;
	va_list ap;
{
	register int cnt;
	register char *p;
	time_t now;
	int fd, saved_errno;
#define TBUFSIZ 2048
#define FMTSIZ  1024
	char tbuf[TBUFSIZ+3], fmt_cpy[FMTSIZ+1], *stdp;

	saved_errno = errno;

	/* See if we should just throw out this message. */
	if (!LOG_MASK(LOG_PRI(pri)) || (pri &~ (LOG_PRIMASK|LOG_FACMASK)))
		return;
	if (LogFile < 0 || !connected)
		openlog(LogTag, LogStat | LOG_NDELAY, 0);

	/* Set default facility if none specified. */
	if ((pri & LOG_FACMASK) == 0)
		pri |= LogFacility;

	/* Build the message. */
	(void)time(&now);
	(void)snprintf(tbuf, TBUFSIZ, "<%d>%.15s ", pri, ctime(&now) + 4);
	for (p = tbuf; *p; ++p);
	if (LogStat & LOG_PERROR)
		stdp = p;
	if (LogTag) {
		(void)strcpy(p, LogTag);
		for (; *p; ++p);
	}
	if (LogStat & LOG_PID) {
		(void)snprintf(p, TBUFSIZ-(p-tbuf), "[%d]", getpid());
		for (; *p; ++p);
	}
	if (LogTag) {
		*p++ = ':';
		*p++ = ' ';
	}

	/* Substitute error message for %m. */
	{
		register char ch, *t1;
		char *strerror();

		for (t1 = fmt_cpy; (ch = *fmt) != '\0' && t1<fmt_cpy+FMTSIZ; ++fmt)
			if (ch == '%' && fmt[1] == 'm') {
				++fmt;
				t1 += snprintf(t1, FMTSIZ-(t1-fmt_cpy),
					       "%s", strerror(saved_errno));
			}
			else
				*t1++ = ch;
		*t1 = '\0';
	}

	p += vsnprintf(p, TBUFSIZ-(p-tbuf), fmt_cpy, ap);
	cnt = p - tbuf;

	/* Output to stderr if requested. */
	if (LogStat & LOG_PERROR) {
		struct iovec iov[2];
		register struct iovec *v = iov;

		v->iov_base = stdp;
		v->iov_len = cnt - (stdp - tbuf);
		++v;
		v->iov_base = "\n";
		v->iov_len = 1;
		(void)writev(STDERR_FILENO, iov, 2);
	}

	/* Output the message to the local logger. */
#if 1
	/* Use NUL as a message delimiter. */
	if (write(LogFile, tbuf, cnt + 1) >= 1)
	{
		return;
	}
	else
	{
		/* If the write fails, we try to reconnect it next
		 * time. */
		closelog ();
	}
#else
	if (write(LogFile, tbuf, cnt) >= 0)
		return;
#endif
	/*
	 * Output the message to the console; don't worry about blocking,
	 * if console blocks everything will.  Make sure the error reported
	 * is the one from the syslogd failure.
	 */
	/* should mode be `O_WRONLY | O_NOCTTY' ? -- Uli */
	if (LogStat & LOG_CONS &&
	    (fd = open(_PATH_CONSOLE, O_WRONLY, 0)) >= 0) {
		(void)strcat(tbuf, "\r\n");
		cnt += 2;
		p = index(tbuf, '>') + 1;
		(void)write(fd, p, cnt - (p - tbuf));
		(void)close(fd);
	}
}

static struct sockaddr SyslogAddr;	/* AF_UNIX address of local logger */
/*
 * OPENLOG -- open system log
 */
void
openlog(ident, logstat, logfac)
	const char *ident;
	int logstat, logfac;
{
	if (ident != NULL)
		LogTag = ident;
	LogStat = logstat;
	if (logfac != 0 && (logfac &~ LOG_FACMASK) == 0)
		LogFacility = logfac;
	if (LogFile == -1) {
		SyslogAddr.sa_family = AF_UNIX;
		(void)strncpy(SyslogAddr.sa_data, _PATH_LOG,
		    sizeof(SyslogAddr.sa_data));
		if (LogStat & LOG_NDELAY) {
			if ((LogFile = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
				return;
/*			fcntl(LogFile, F_SETFD, 1); */
		}
	}
	if (LogFile != -1 && !connected &&
#if 0
	    connect(LogFile, &SyslogAddr, sizeof(SyslogAddr.sa_family)+
			strlen(SyslogAddr.sa_data)) != -1)
#else
	    connect(LogFile, &SyslogAddr, sizeof(SyslogAddr) -
			sizeof(SyslogAddr.sa_data) +
			strlen(SyslogAddr.sa_data)) != -1)
#endif
		connected = 1;
}

/*
 * CLOSELOG -- close the system log
 */
void
closelog()
{
	(void) close(LogFile);
	LogFile = -1;
	connected = 0;
}

static int	LogMask = 0xff;		/* mask of priorities to be logged */
/*
 * SETLOGMASK -- set the log mask level
 */
int
setlogmask(pmask)
	int pmask;
{
	int omask;

	omask = LogMask;
	if (pmask != 0)
		LogMask = pmask;
	return (omask);
}
