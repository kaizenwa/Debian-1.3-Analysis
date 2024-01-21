/*
 * Implement the Usenet ihave/sendme control messages, as per RFC 1036. (NCMP)
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>

#include "libc.h"
#include "news.h"
#include "config.h"
#include "batchnames.h"
#include "headers.h"
#include "relay.h"
#include "history.h"
#include "fgetmfs.h"
#include "msgs.h"

#ifndef AVEARTSIZE
#define AVEARTSIZE 3000
#endif
#ifndef SENDMEBATFILE
#define SENDMEBATFILE ".ihave/togo"
#endif
#ifndef SENDITBATFILE
#define SENDITBATFILE ".sendme/togo"
#endif

#define PROTO_IHAVE 0
#define PROTO_SENDME 1

/* imports */
extern char *ismsgidbad();

/*
 * write the name of the article file on the batch file appropriate to
 * this remotesys and protocol.
 */
STATIC void
appmsgnmtonxtbatfile(art, remotesys, proto)
register struct article *art;
char *remotesys;
int proto;
{
	register char *batchname;
	register FILE *batf;
	extern boolean safecmd();

	if (!safecmd(remotesys)) {	/* nasty site name? a bit paranoid */
		errno = 0;
		transient(art, 'b', "site name `%s' in ihave/sendme is unsafe",
			remotesys);
		return;
	}
	batchname = str3save(artfile(BTCHDIR), remotesys,
		(proto == PROTO_IHAVE? SENDMEBATFILE: SENDITBATFILE));
	batf = fopenwclex(batchname, "a");
	if (batf != NULL) {
		(void) fputs(art->a_tmpf, batf);
		(void) putc('\n', batf);
		if (fclose(batf) == EOF)
			fulldisk(art, batchname);
	}
	free(batchname);
}

STATIC void
doproto(args, art, proto)
char *args;
register struct article *art;
int proto;
{
	register char *argscp = skipsp(args), *remotesys, *p;
	char *myname;

	if (*argscp == '\n' || *argscp == '\0')	/* no args */
		return;

	argscp = strsave(argscp);

	/* dig out the remote system name */
	remotesys = NULL;
	for (p = argscp; *p != '\0'; p++)
		if (iswhite(*p))
			remotesys = p;
	if (remotesys == NULL)			/* no msg-ids in command */
		remotesys = argscp;
	else {
		remotesys = argscp + strlen(argscp) - 1;	/* last byte */
		while (isascii(*remotesys) && isspace(*remotesys))
			*remotesys-- = '\0';	/* back up to non-whitespace */
		remotesys = NULL;
		for (p = argscp; *p != '\0'; p++)
			if (iswhite(*p))
				remotesys = p;
		if (remotesys == NULL)		/* no msg-ids in command */
			remotesys = argscp;
		else
			*remotesys++ = '\0';	/* split msg-ids & sys name */
	}
	myname = hostname();
	if (!STREQ(remotesys, myname))		/* remotesys may not be me */
		appmsgnmtonxtbatfile(art, remotesys, proto);
	free(argscp);
}

/*
 *	ihave [message-ID-list] remotesys	generate a sendme for remotesys
 *						from message-ID-list
 * Read message-IDs from args or control message body,
 * look them up in history, post a sendme to to.remotesys (via the batcher
 * to avoid deadlock) consisting of the message-IDs not in history.
 * The "posting" consists of transmitting to a system matching
 * "to.remotesys" in sys, which had better have the I flag on.
 */
void
ihave(args, art)
char *args;
struct article *art;
{
	doproto(args, art, PROTO_IHAVE);
}

/*
 *	sendme [message-ID-list] remotesys	send articles named to remotesys
 * Read message-IDs from args or control message body,
 * transmit the corresponding articles to a system matching
 * "to.remotesys/sendme" in sys, which will typically name a batch file.
 */
void
sendme(args, art)
char *args;
struct article *art;
{
	doproto(args, art, PROTO_SENDME);
}
