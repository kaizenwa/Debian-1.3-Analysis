#ifndef lint
static char	sccsid[] = "@(#)$Id: ihave.c,v 1.20 1994/11/01 06:08:21 sob Exp sob $";
#endif

#include "common.h"
#ifdef MSGID
#include "msgid.h"
#endif

#ifdef LOG
int	ih_accepted;
int	ih_rejected;
int	ih_failed;
#endif

/*
 * IHAVE <messageid>
 *
 * Accept an article for transferral if we haven't seen it before.
 */

void
ihave(argc, argv)
	int		argc;
	char		*argv[];
{
	char		errbuf[2 * NNTP_STRLEN];
	int		retcode;
	register char	*cp;
#ifdef MSGID
	int		dup = 0;
#endif
  
	if (!canxfer)
		{
#ifdef LOG
		syslog(LOG_INFO, "%s ihave attempted without permission",
			hostname);
#endif
		printf("%d You do not have transfer permission\r\n",
			ERR_GOODBYE);
		(void) fflush(stdout);
		return;
		}

	if (argc != 2) {
		printf("%d Usage: IHAVE <message-id>.\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

#ifdef MSGID
	if (msgid(argv[1], MADD))
		dup++;

	if (!dup) {
		cp = gethistent(argv[1], 1);
		if (cp != NULL) {
			dup++;
			(void) msgid(argv[1], MOLD);
		}
	}
	if (dup) {
#else
	cp = gethistent(argv[1], 1);
	if (cp != NULL) {
#endif /*MSGID*/
		printf("%d Got it.\r\n", ERR_GOTIT);
		(void) fflush(stdout);
#ifdef LOG
		ih_rejected++;
#ifdef IHAVE_DEBUG
		syslog(LOG_DEBUG, "%s ihave %s rejected", hostname, argv[1]);
#endif
#endif
		return;
	}

	if (!space(MINFREE)) {
	    /* force error reporting code into sending */
	    /* an out-of-space error message	       */
	    if (gethostname(errbuf, MAXHOSTNAMELEN) < 0)
		(void) strcpy(errbuf, "Amnesiac");

	    (void) strcat(errbuf, " NNTP server out of space. Try later.");

	    retcode = 0;		/* indicates that an error occurred */
	} else 
#ifdef BATCHED_INPUT
	    /* C news input hook */
	    retcode = batch_input_article(CONT_XFER, ERR_XFERFAIL,
		errbuf, argv[1]);
#else
	    retcode = spawn(rnews, "rnews", (char *) 0, CONT_XFER,
		ERR_XFERFAIL, errbuf, argv[1]);
#endif

	if (retcode <= 0){
               /* Reject if "*:<optional_whitespace>inbound*", else fail */
               register int i;

#ifdef MSGID
	       (void) msgid(argv[1], MCANCEL);
#endif

               i = ERR_XFERFAIL;
               if (cp = index(errbuf,':')) {
                       for (++cp; isspace(*cp); ++cp)
                               ;
                       if (strncasecmp(cp, "inbound", 7) == 0)
                               i = ERR_XFERRJCT;
               }
               printf("%d %s\r\n", i, errbuf);
       }
        else
               printf("%d Thanks.\r\n", OK_XFERED);
        (void) fflush(stdout);

#ifdef LOG
	if (retcode == 1)
		ih_accepted++;
	else
		ih_failed++;
		
#ifdef IHAVE_DEBUG
	syslog(LOG_DEBUG, "%s ihave %s accepted %s",
		hostname, argv[1], retcode == 1 ? "succeeded" : "failed");
#endif
#endif /* LOG */

}
