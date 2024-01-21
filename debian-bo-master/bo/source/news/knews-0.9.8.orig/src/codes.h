#define STRINGIFY(a)	#a
#define CODE_TO_STR(a)	(a - a) + STRINGIFY(a)

#define	NNTP_INF_HELP		100	/* Help text on way */
#define	NNTP_INF_DEBUG		199	/* Debug output */

#define	NNTP_OK_CANPOST		200	/* Hello; you can post */
#define	NNTP_OK_NOPOST		201	/* Hello; you can't post */
#define	NNTP_OK_SLAVE		202	/* Slave status noted */
#define	NNTP_OK_GOODBYE		205	/* Closing connection */
#define	NNTP_OK_GROUP		211	/* Group selected */
#define	NNTP_OK_GROUPS		215	/* Newsgroups follow */
#define	NNTP_OK_ARTICLE		220	/* Article (head & body) follows */
#define	NNTP_OK_HEAD		221	/* Head follows */
#define	NNTP_OK_BODY		222	/* Body follows */
#define	NNTP_OK_NOTEXT		223	/* No text sent -- stat, next, last */
#define NNTP_OK_XOVER   	224     /* XOVER OK */
#define	NNTP_OK_NEWNEWS		230	/* New articles by message-id follow */
#define	NNTP_OK_NEWGROUPS	231	/* New newsgroups follow */
#define	NNTP_OK_XFERED		235	/* Article transferred successfully */
#define	NNTP_OK_POSTED		240	/* Article posted successfully */
#define NNTP_OK_AUTH		281	/* Authentication accepted */

#define NNTP_CONT_XFER		335	/* Continue to send article */
#define	NNTP_CONT_POST		340	/* Continue to post article */
#define NNTP_CONT_AUTH		381	/* More authentication needed */

#define	NNTP_ERR_GOODBYE	400	/* Have to hang up for some reason */
#define	NNTP_ERR_NOGROUP	411	/* No such newsgroup */
#define	NNTP_ERR_NCING		412	/* Not currently in newsgroup */
#define	NNTP_ERR_NOCRNT		420	/* No current article selected */
#define	NNTP_ERR_NONEXT		421	/* No next article in this group */
#define	NNTP_ERR_NOPREV		422	/* No previous article in this group */
#define	NNTP_ERR_NOARTIG	423	/* No such article in this group */
#define NNTP_ERR_NOART		430	/* No such article at all */
#define NNTP_ERR_GOTIT		435	/* Already got that article */
#define NNTP_ERR_XFERFAIL	436	/* Transfer failed */
#define	NNTP_ERR_XFERRJCT	437	/* Article rejected, don't resend */
#define	NNTP_ERR_NOPOST		440	/* Posting not allowed */
#define	NNTP_ERR_POSTFAIL	441	/* Posting failed */
#define NNTP_ERR_NEED_AUTH	480	/* Authentication required */
#define NNTP_ERR_FAIL_AUTH	481	/* Authentication rejected */

#define	NNTP_ERR_COMMAND	500	/* Command not recognized */
#define	NNTP_ERR_CMDSYN		501	/* Command syntax error */
#define	NNTP_ERR_ACCESS		502	/* Access to server denied */
#define NNTP_ERR_FAULT		503	/* Program fault, cpmd not performed */
