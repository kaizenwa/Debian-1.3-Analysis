/*
 * relaynews error handling (also used by some other bits)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>
#include <sys/timeb.h>		/* solely for getindate call */

#include "libc.h"
#include "news.h"
#include "active.h"
#include "headers.h"
#include "relay.h"
#include "msgs.h"

/* common log reporting */
static
log(stream, art, code, fmt, arg, sverrno)
register FILE *stream;
register struct article *art;
int code;
char *fmt, *arg;
register int sverrno;
{
	time_t now;

	if (code != '\0') {
		timestamp(stream, &now);
		(void) putc(' ', stream);
		if (art != NULL)
			(void) fputs(sendersite(nullify(art->h.h_path)),
				stream);
		(void) putc(' ', stream);
		(void) putc(code, stream);
		(void) putc(' ', stream);
		if (art != NULL && art->h.h_msgid != NULL)
			(void) fputs(art->h.h_msgid, stream);
		else
			(void) putc('-', stream);
		(void) putc(' ', stream);
		(void) fprintf(stream, fmt, arg);
		if (sverrno != 0)
			(void) fprintf(stream, " (%s)", strerror(sverrno));
		(void) putc('\n', stream);
		if (ferror(stream))
			fulldisk(art, "a log file");
	}
}

/*
 * log an audit report and continue
 * article status unaffected
 */
logaudit(art, code, fmt, arg)
struct article *art;
int code;
char *fmt, *arg;
{
	log(stdout, art, code, fmt, arg, 0);
}

/*
 * log a complaint about bad input and continue
 * set ST_REFUSED in article status
 */
transient(art, code, fmt, arg)
register struct article *art;
int code;
char *fmt, *arg;
{
	log(stdout, art, code, fmt, arg, 0);
	if (art != NULL)
		art->a_status |= ST_REFUSED;
}

/*
 * the news system needs attention; complain and shut down gracefully
 * sets ST_NEEDATTN and ST_DROPPED in article status
 */
persistent(art, code, fmt, arg)
register struct article *art;
int code;
char *fmt, *arg;
{
	log(stderr, art, code, fmt, arg, errno);
	if (art != NULL)
		art->a_status |= ST_NEEDATTN|ST_DROPPED;
}

/*
 * something impossible has happened; complain and shut down ASAP
 * either quits or sets ST_NEEDATTN and ST_DROPPED in article status
 */
/* ARGSUSED art code */
canthappen(art, code, fmt, arg)
struct article *art;
int code;
char *fmt, *arg;
{
	errunlock(fmt, arg);
	/* NOTREACHED */
}

void
fulldisk(art, file)			/* complain once & set status bits */
register struct article *art;
char *file;
{
	if (!(art->a_status&ST_DISKFULL))
		art->a_status |= prfulldisk(file);
}
