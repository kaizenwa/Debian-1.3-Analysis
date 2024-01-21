/* 
 * error - print best error message possible and exit.  Exit status 1
 * means permanant error of some kind (will not change by retrying
 * later), exit status 2 means temporary error, can be retried in a
 * while.  Putting some sort of exponential or ramped backoff on retry
 * times and a small limit on retries is advised, since some of the
 * retry errors can be caused by resource exhaustion, something which
 * may not get fixed very quickly.
 */

#include <stdio.h>
#include "error.h"

extern char *emalloc();
extern void batchend();

/* 
 * I've tried to collect a reasonable intersecting list of temporary
 * failure error codes for modern networking systems.  A regrettable set
 * of omissions from this list is the set of STREAMS errors -- ENOSR,
 * ENOMSG, ETIME, which Ultrix and 4BSD lack.
 */
int
error_status(saverrno, savherrno)
int saverrno, savherrno;
{
	/* Nameserver error.  Pity this wasn't just added to errno.h */
	if (savherrno == TRY_AGAIN)
		/* Non-Authoritative Host not found, or SERVERFAIL */
		return ERR_RETRY;
	else if (savherrno != 0)
		return ERR_PERM;

	switch (saverrno) {
	case ENOMEM:		/* Not enough core */
	case EAGAIN:		/* Resource temporarily unavailable? */
	case ENFILE:		/* File table overflow */
	case EMFILE:		/* Too many open files */
	case ENOSPC:		/* No space left on device */
	case EPIPE:		/* Broken pipe */
	case EADDRINUSE:	/* Address already in use */
	case EADDRNOTAVAIL:	/* Can't assign requested address */
	case ENETDOWN:		/* Network is down */
	case ENETUNREACH:	/* Network is unreachable */
	case ENETRESET:		/* Network dropped connection on reset */
	case ECONNABORTED:	/* Software caused connection abort */
	case ECONNRESET:	/* Connection reset by peer */
	case ENOBUFS:		/* No buffer space available */
	case ETIMEDOUT:		/* Connection timed out */
	case EHOSTDOWN:		/* Host is down */
	case EHOSTUNREACH:	/* No route to host */
	case ESTALE:		/* Stale NFS file handle */
		return ERR_RETRY;
	}
	return ERR_PERM;
}

void
error(s1, s2)
char *s1;
char *s2;
{
	static int again, saverrno, savherrno;

	if (again++ == 0) {
		saverrno = errno;
		savherrno = h_errno;
		warning(s1, s2);
		batchend();
	}
	exit(error_status(saverrno, savherrno));
}

char *
itos(fmt, i)
char *fmt;
int i;
{
	static char *intbuf;

	if (intbuf)
		free(intbuf);
	intbuf = emalloc(strlen(fmt) + 60);	/* format + number + NUL */
	(void) sprintf(intbuf, fmt, i);
	return intbuf;
}
