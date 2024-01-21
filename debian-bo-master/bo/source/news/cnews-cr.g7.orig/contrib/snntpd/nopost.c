#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "libcnews.h"
#include "config.h"
#include "fgetfln.h"
#include "history.h"

#include "batch.h"
#include "msgs.h"
#include "debug.h"
#include "netdata.h"
#include "log.h"

#define NULLSTR		((char *) NULL)

/* NOTE: x *MUST* be an array or you'll only get 3 chars */
#define SAY(x, y) \
	if (net_ackwrite((x), sizeof (x) - 1, (y), stdout) == 0) \
		; \
	else \
		unlock(), error("net_ackwrite(%s) failed", (x))

/* imports */
extern char *mktemp();
extern int postarts, postfail;
extern unsigned int debug;
extern FILE *dfp;

void
postarticle(fp, ndp)
FILE *fp;
struct netdata *ndp;
{
	SAY(NNTP_NOPOST, NULLSTR);
	postfail++;
}
