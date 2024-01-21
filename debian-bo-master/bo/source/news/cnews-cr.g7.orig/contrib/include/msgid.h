/*
 * Default place for the socket
 */
#define SOCKNAME	"/usr/lib/news/nntp_msgid"
#define PIDFILE         "/usr/lib/news/msgidd.pid"

#ifdef NEEDMSGS
/*
 * Message types from client to server
 *	MADD: Check for dup and add as needed.  Return value is non-0 for dup.
 *	MCANCEL: Delete an id from the holding queues.  Return value is
 *		non-0 for failure.
 *	MHOST: inform the server who is on the other end of this nntpd.
 *		Return value is non-0 for failure.  Used only in msgid_init().
 *	MOLD: produce a log message of type "old"
 */
char *msgs[] = {
	"add-",	/* Check for dup and add as needed. */
	"can-",	/* Delete an id from the holding queues. */
	"hst-",	/* inform the server who is on the other end */
	"old-"	/* produce a log message of type "old" */
};
#endif

/* 
 * Messages to client side called with (MUST BE IN SAME ORDER AS msgs[])
 */
#define MADD	0	/* Check for dup and add as needed. */
#define	MCANCEL 1	/* Delete an id from the holding queues. */
#define	MHOST	2	/* inform the server who is on the other end */
#define MOLD	3	/* produce a log message of type "old" */
