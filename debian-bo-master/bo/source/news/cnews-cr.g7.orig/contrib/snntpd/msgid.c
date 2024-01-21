/*
 * msgid -- message ID client interface
 * vix 13feb91 [negative caching]
 * vix 24may90 [written]
 * with mods ken@sdd.hp.com 01jul90
 * hide 4BSD networking goo, Geoff Collyer, October 1992
 *
 * $Header: msgid.c,v 1.6 91/08/17 12:04:11 vixie Locked $
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>

#ifdef hpux
#include <sys/param.h>
#include <libBSD.h>
#endif
#include <syslog.h>

#define NEEDMSGS
#include "msgid.h"
#include "ipc.h"

#define SERVERTIMEOUT 30

static char hostname[256];
static int s = -1;		/* connection to msgidd */

static int read_answer();

/*
 * Protocol:
 *    Return value as used here is from the server to us.  Note that this
 *    may not be the same as the return value from msgid().
 *
 *    3 message types:
 *	MCANCEL: Delete an id from the holding queues.  Return value is
 *		non-0 for failure.
 *	MADD: Check for dup and add as needed.  Return value is non-0 for dup.
 *	MHOST: inform the server who is on the other end of this nntpd.
 *		Return value is non-0 for failure.  Used only in msgid_init().
 *	MOLD: produce a log message of type "old"
 */

static int
sndmsg(type, msg, fail)		/* send a message and get an answer */
int type;
char *msg;
{
	char buf[300];

	(void) strcpy(buf, msgs[type]);
	(void) strcat(buf, msg);
	if (write(s, buf, strlen(buf)) < 0) {
		(void) close(s);
		s = -1;
		syslog(LOG_ERR, "msgid: host message write: %m", SOCKNAME);
		return fail;
	}
	return read_answer();
}

/* 
 * returns: 0 for ok, 1 for failure
 */
msgid_init()
{
	static int dead_server_count = 0;

	s = ipcopen(ipcpath("localhost", "unix", SOCKNAME), "");
	if (s < 0) {
		/*
		 * only syslog every 128 messages, so that dead msgidd
		 * doesn't lead to multi-megabyte syslog files (vix,
		 * 13feb91)
		 */
		if ((dead_server_count++ % 128) == 0)
			syslog(LOG_ERR, "msgid: can't connect to %s: %m",
				SOCKNAME);
		return 1;
	}
	if (hostname[0] == '\0')
		(void) gethostname(hostname, sizeof hostname);
	return sndmsg(MHOST, hostname, 1);
}

/* 
 * returns: nonzero = duplicate, return value doesn't mean much for the
 *          MADD or MOLD messages
 */
int
msgid(id, mtype)
char *id;
int mtype;
{
	register char *cp = id;

	if (s == -1 && msgid_init())
		return 0;

	/*
	 * We need to do this just because gethistent does it
	 * "in place" so add vs old gets fried ...
	 *
	 * If running Bnews, converts "id" to lower case.
	 * If running Cnews, converts "id" per rfc822.
	 */
#ifndef BNEWS
	cp = strrchr(id, '@');        /* look for @ in message id */
#endif
	if (cp != NULL)
		for (; *cp != '\0'; ++cp)
			if (isupper(*cp))
				*cp = tolower(*cp);
	return sndmsg(mtype, id, 0);
}

static int	
read_answer()
{
	register int i;
	fd_set readfds;
	register fd_set *rfdp = &readfds;
	char c;
	struct timeval to;

	FD_ZERO(rfdp);
	FD_SET(s, rfdp);
	to.tv_sec = SERVERTIMEOUT;
	to.tv_usec = 0;
	i = select(s + 1, rfdp, (fd_set *)NULL, (fd_set *)NULL, &to);
	if (i < 0)
		syslog(LOG_ERR, "msgid: select: %m");
	else if (i == 0 || FD_ISSET(s, rfdp) == 0 ||
	    (i = read(s, &c, 1)) == 0)
		syslog(LOG_ERR, "msgid: read timeout");
	else if (i < 0)
		syslog(LOG_ERR, "msgid: read: %m");
	else
		return c != 0;

	(void) close(s);
	s = -1;
	return 0;
}
