#ifndef lint
static char	sccsid[] = "@(#)$Id: access.c,v 1.28 1994/12/03 21:54:30 sob Exp sob $";
#endif

#include "common.h"
#if defined(EXCELAN) || defined(TLI)
#include <netinet/in.h>
#ifdef TLI
#include <netdb.h>
#endif
#endif
#include <sys/socket.h>

#define	SNETMATCH	1
#define	NETMATCH	2

/*
 * host_access -- determine if the client has permission to
 * read, transfer, and/or post news.  read->transfer.
 * We switch on socket family so as to isolate network dependent code.
 *
 *	Parameters:	"canread" is a pointer to storage for
 *			an integer, which we set to 1 if the
 *			client can read news, 0 otherwise.
 *
 *			"canpost" is a pointer to storage for
 *			an integer,which we set to 1 if the
 *			client can post news, 0 otherwise.
 *
 *			"canxfer" is a pointer to storage for
 *			an integer,which we set to 1 if the
 *			client can transfer news, 0 otherwise.
 *
 *			"gdlist" is a comma separated list of
 *			newsgroups/distributions which the client
 *			can access.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	None.
 */

#ifdef EXCELAN
extern struct sockaddr_in current_peer;
#endif

#ifdef TLI
extern char **Argv;
extern struct hostent *gethostbyname();
#endif

#ifdef AUTH
extern	int Needauth;
#endif /* AUTH */

void
host_access(canread, canpost, canxfer, gdlist)
	int		*canread, *canpost, *canxfer;
	char		*gdlist;
{
	int		sockt;
	int		length;
	struct sockaddr	sa;
#ifdef TLI
	struct sockaddr_in *sin = (struct sockaddr_in *) &sa;
	struct hostent	*hp;
	int		argcnt = 0;
#endif
	int		match = 0;
	int		count;
	char		hostornet[MAXHOSTNAMELEN];
	char		host_name[MAXHOSTNAMELEN];
	char		net_name[MAXHOSTNAMELEN];
	char		snet_name[MAXHOSTNAMELEN];
	char		readperm[MAXBUFLEN];
	char		postperm[MAXBUFLEN];
	char		groups[MAXBUFLEN];
	char		line[MAXBUFLEN];
	register char	*cp;
	register FILE	*acs_fp;

	gdlist[0] = '\0';

#ifdef NO_ACCESS_CHECK				/* for special debugging */
	*canread = *canpost = *canxfer = 1;
	return;
#endif

	*canread = *canpost = *canxfer = 0;

	sockt = fileno(stdin);
	length = sizeof (sa);

#ifdef TLI
	if (t_getpeername(sockt, &sa, &length) < 0) {
		if (isatty(sockt)) {
			(void) strcpy(hostname, "stdin");
			*canread = 1;
		} else {
#ifdef SYSLOG
			syslog(LOG_ERR, "host_access: getpeername: %m");
#endif
			(void) strcpy(hostname, "unknown");
		}
		return;
	}

#else /* !TLI */

#ifdef EXCELAN
	if (raddr(current_peer.sin_addr) == NULL) {
#else
	if (getpeername(sockt, &sa, &length) < 0) {
#endif
		if (isatty(sockt)) {
			(void) strcpy(hostname, "stdin");
			*canread = 1;
		} else {
#ifdef SYSLOG
			syslog(LOG_ERR, "host_access: getpeername: %m");
#endif
			(void) strcpy(hostname, "unknown");
		}
		return;
	}
#ifdef EXCELAN
	else bcopy(&current_peer,&sa,length);
#endif
#endif /* !TLI */

	switch (sa.sa_family) {
	case AF_INET:
		inet_netnames(sockt, &sa, net_name, snet_name, host_name);
		break;

#ifdef DECNET
	case AF_DECnet:
		dnet_netnames(sockt, &sa, net_name, snet_name, host_name);
		break;
#endif

	default:
#ifdef SYSLOG
		syslog(LOG_ERR, "unknown address family %ld", sa.sa_family);
#endif
		return;
	};

	/* Normalize host name to lower case */

	for (cp = host_name; *cp; cp++)
		if (isupper(*cp))
			*cp = tolower(*cp);

#ifdef LOG
	syslog(LOG_INFO, "%s connect\n", host_name);
#endif
	(void) strcpy(hostname, host_name);

	/*
	 * We now we have host_name, snet_name, and net_name.
	 * Our strategy at this point is:
	 *
	 * for each line, get the first word
	 *
	 *	If it matches "host_name", we have a direct
	 *		match; parse and return.
	 *
	 *	If it matches "snet_name", we have a subnet match;
	 *		parse and set flags.
	 *
	 *	If it matches "net_name", we have a net match;
	 *		parse and set flags.
	 *
	 *	If it matches the literal "default", note we have
	 *		a net match; parse.
	 */

#if defined(DEBUG) && defined(SYSLOG)
	if (debug)
		syslog(LOG_DEBUG,
			"host_access(): host_name=%s, snet_name=%s, net_name=%s",
			host_name ? host_name : "NIL",
			snet_name ? snet_name : "NIL",
			net_name ? net_name : "NIL");
#endif

	acs_fp = fopen(accessfile, "r");
	if (acs_fp == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "access: fopen %s: %m", accessfile);
#endif
		return;
	}

	while (fgets(line, sizeof(line), acs_fp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		if ((cp = index(line, '#')) != NULL)
			*cp = '\0';
		if (*line == '\0')
			continue;

		count = sscanf(line, "%s %s %s %s",
				hostornet, readperm, postperm, groups);

		if (count < 4) {
			if (count < 3)
				continue;
			groups[0] = '\0';	/* No groups specified */
		}
#if defined(DEBUG) && defined(SYSLOG)
	if (debug)
		syslog(LOG_DEBUG,
			"host_access(): hostornet=%s, readperm=%s, postperm=%s, groups=%s",
			hostornet ? hostornet : "NIL",
			readperm ? readperm : "NIL",
			postperm ? postperm : "NIL",
			groups ? groups : "NIL");
#endif

#ifdef DOMAINMATCH
		for (cp = hostornet; *cp; cp++)
			if (isupper(*cp))
				*cp = tolower(*cp);
		if (wildmat(host_name, hostornet)) {
#else
		if (!strcasecmp(hostornet, host_name)) {
#endif
			*canread = (readperm[0] == 'r' || readperm[0] == 'R');
			*canxfer = (readperm[0] == 'X' || readperm[0] == 'x');
			if (readperm[0] == 'B' || readperm[0] == 'b')
				*canxfer = *canread = 1;
			*canpost = (postperm[0] == 'p' || postperm[0] == 'P');
			(void) strcpy(gdlist, groups);
			break;
		}

		if (*snet_name && !strcasecmp(hostornet, snet_name)) {
			match = SNETMATCH;
			*canread = (readperm[0] == 'r' || readperm[0] == 'R');
			*canxfer = (readperm[0] == 'X' || readperm[0] == 'x');
			if (readperm[0] == 'B' || readperm[0] == 'b')
				*canxfer = *canread = 1;
			*canpost = (postperm[0] == 'p' || postperm[0] == 'P');
			(void) strcpy(gdlist, groups);
		}

		if (match != SNETMATCH && (!strcasecmp(hostornet, net_name) ||
		    !strcasecmp(hostornet, "default"))) {
			match = NETMATCH;
			*canread = (readperm[0] == 'r' || readperm[0] == 'R');
			*canxfer = (readperm[0] == 'X' || readperm[0] == 'x');
			if (readperm[0] == 'B' || readperm[0] == 'b')
				*canxfer = *canread = 1;
			*canpost = (postperm[0] == 'p' || postperm[0] == 'P');
			(void) strcpy(gdlist, groups);
		}
	}
/*
 * The access check expects there to be spaces between the group names.
 * In the access file, there are commas between the groupnames.
 * Here, we change the commas to spaces.
 */
         {
 	  char *pointer=gdlist;
 	  
 	  while (*pointer)
 	    {
 	      if (*pointer == ',') *pointer=' ';
 	      pointer++;
 	    }
 	}

	(void) fclose(acs_fp);

#ifdef AUTH
	Needauth = 0;
	/* do we require a userid and password for this guy? */
	if (isupper(readperm[0]) || isupper(postperm[0]))
		Needauth = 1;
#endif /* AUTH */
}
