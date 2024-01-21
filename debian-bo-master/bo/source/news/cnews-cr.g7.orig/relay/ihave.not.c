/*
 * Reject the Usenet ihave/sendme control messages. (NCMP)
 */

#include <stdio.h>
#include <sys/types.h>

#include "news.h"
#include "headers.h"
#include "relay.h"

static void
ignore(art, cmd, args)
struct article *art;
char *cmd, *args;
{
	transient(art, 'b', "`ihave (or sendme) %s' control ignored", args);
}

void
ihave(args, art)
char *args;
struct article *art;
{
	ignore(art, "ihave", args);
}

void
sendme(args, art)
char *args;
struct article *art;
{
	ignore(art, "sendme", args);
}
