/*
 * print common messages
 */

#include <stdio.h>
#include <sys/types.h>
#include "news.h"
#include "msgs.h"

statust
prfulldisk(file)			/* complain & return bad status */
char *file;
{
	persistent(NOART, 'f', "error writing `%s'", file);
	return ST_DISKFULL|ST_NEEDATTN|ST_DROPPED;	/* disk full? */
}
