#ifndef lint
static char	sccsid[] = "@(#)list.c	1.11	(Berkeley) 5/11/89";
#endif

#include "common.h"

/*
 * LIST
 *
 * List active newsgroups, newsgroup descriptions, distributions,
 * active.times, and subscriptions.
 *
 */

void
list(argc, argv)
	int		argc;
	char		*argv[];
{
	char		line[NNTP_STRLEN];
	char		*grparray[2];
	char		*filename;
	char		*items;
	char		*format;
	register char	*cp;
	register FILE	*list_fp;
	char		*wildarg = NULL;
	int		argc_ok = (argc == 2 || argc == 3);
	
	if (argc == 1 || (argc_ok && !strcasecmp(argv[1],"active"))){
		if (argc == 3) {
			int first, last;
			int num = find_group(argv[2], &first, &last);
			if (num >= 0) {
				printf("%d list:\r\n%s\r\n.\r\n",
					OK_GROUPS,group_array[num]);
				(void) fflush(stdout);
				return;
			}
			wildarg = argv[2];
		}
                num_groups = read_groups();
                if (num_groups == 0){ /* can't get a group list */
                  printf("%d Group update failed. Try later.\r\n",
                         ERR_FAULT);
                  (void) fflush(stdout);
#ifdef LOG
                  syslog(LOG_INFO, "%s group update failed in LIST", hostname);
#endif
                  exit(1);
		}
		filename = activefile;
		items = "active newsgroups";
		format = "Newsgroups in form \"group high low flags\".";
	} else if (argc == 2 && !strcasecmp(argv[1],"distributions")){
		filename = distributionsfile;
		items = "newsgroup distributions";
		format = "Distributions in form \"area description\".";
	} else if (argc_ok && !strcasecmp(argv[1],"newsgroups")){
		if (argc == 3)
			wildarg = argv[2];
		filename = newsgroupsfile;
		items = "newsgroup descriptions";
		format = "Descriptions in form \"group description\".";
	} else if (argc == 2 && !strcasecmp(argv[1],"subscriptions")){
		filename = subscriptionsfile;
		items = "group subscription list";
		format = "New-user subscription list in form \"group\".";
	} else if (argc == 2 && !strcasecmp(argv[1],"overview.fmt")){
		filename = overviewfmtfile;
		items = "overview format";
		format = "Order of fields in overview database.";
	} else if (argc == 2 && !strcasecmp(argv[1],"active.times")){
		filename = activetimesfile;
		items = "active times";
		format = "Active times in form \"group time email name\".";
	} else {
		printf("%d Usage: LIST [ACTIVE|NEWSGROUPS|DISTRIBUTIONS|SUBSCRIPTIONS|ACTIVE.TIMES]\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	grparray[0] = line;
	grparray[1] = NULL;

	list_fp = fopen(filename, "r");

	if (list_fp == NULL) {
		printf("%d No list of %s available.\r\n", ERR_FAULT,
			items);
		(void) fflush(stdout);
#ifdef SYSLOG
		syslog(LOG_ERR, "list: fopen %s: %m", filename);
#endif
		return;
	}

	printf("%d %s\r\n",OK_GROUPS,format);

	while (fgets(line, sizeof(line), list_fp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		for (cp = line; *cp && !isspace(*cp); cp++) ;
		if (*cp)
			*cp = '\0';
		else
			cp = NULL;
		if (ngpermcount) {
			if (ngmatch(s1strneql, ALLBUT,
			    ngpermlist, ngpermcount,
			    grparray, 1) == 0)
				continue;
		}
		else if (ALLBUT==0) break; /* ngpermcnt==0 && ALLBUT == 0 means
					    * don't print the list, right? */
		if (!wildarg || wildmat(line, wildarg)) {
			if (cp)
				*cp = ' ';
			putline(line);
		}
	}
	(void) fclose(list_fp);

	putline(".");
	(void) fflush(stdout);
}

void
xgtitle(argc, argv)
	int		argc;
	char		*argv[];
{
	char *v[3];

	if (argc != 2) {
		printf("%d Usage: XGTITLE pattern\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	v[0] = "list";
	v[1] = "newsgroups";
	v[2] = argv[1];
	list(3, v);
}
