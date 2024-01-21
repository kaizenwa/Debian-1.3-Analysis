#ifndef lint
static char	sccsid[] = "@(#)$Id: help.c,v 1.12 1994/12/03 21:54:30 sob Exp sob $";
#endif

#include "common.h"

/*
 * HELP
 *
 * Provide a naive user with a brief help message.
 *
 */

void
help(argc, argv)
int	argc;
char	*argv[];
{
	printf("%d This server accepts the following commands:\r\n", INF_HELP);
	printf("ARTICLE     BODY         GROUP\r\n");
	printf("HEAD        LAST         LIST\r\n");
	printf("NEXT        POST         QUIT\r\n");
	printf("STAT        NEWGROUPS    HELP\r\n");
	printf("IHAVE       NEWNEWS      SLAVE\r\n");
	printf("DATE\r\n");
	printf("\r\nAdditionally, the following extensions are supported:\r\n\r\n");
#ifdef	XHDR
	printf("XHDR        Retrieve a single header line from a range of articles.\r\n");
#endif
#ifdef LISTGROUP
	printf("LISTGROUP   Retrieve a list of valid article-numbers.\r\n");
#endif
#ifdef	XOVER
	printf("XOVER       Return news overview data.\r\n");
#endif
	printf("XGTITLE     Same as LIST NEWSGROUPS (for backward compatibility).\r\n");
#ifdef XINDEX
	printf("XINDEX      Retrieve a tin style index file.\r\n");
#endif	
#ifdef	XTHREAD
	printf("XTHREAD     Retrieve trn thread file for the current group.\r\n");
#endif
	printf("\r\n");
	printf("Bugs to Stan Barber (Internet: nntp@academ.com)\r\n");
	printf(".\r\n");
	(void) fflush(stdout);
}
