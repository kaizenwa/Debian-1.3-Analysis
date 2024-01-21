#ifndef lint
static char	sccsid[] = "@(#)newgroups.c	1.13	(Berkeley) 2/6/88";
#endif

#include "common.h"
#include "date.h"

/*
 * NEWGROUPS date time ["GMT"] [<distributions>]
 *
 * Display new newsgroups since a given date and time, but only
 * for those in <distributions>.
 */

void
newgroups(argc, argv)
	int		argc;
	char		*argv[];
{
	char		line[NNTP_STRLEN];
	register char	*cp;
	static char	**dist_list = (char **) NULL;
	int		distcount = 0;
	int		i, low_msg, high_msg;
	long		date;
	register FILE	*date_fp;
	char		*reqlist[2];

	if (argc < 3) {
printf("%d Usage: NEWGROUPS yymmdd hhmmss [\"GMT\"] [<distributions>].\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	date_fp = fopen(activetimesfile, "r");
	if (date_fp == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "newgroups: fopen %s: %m", activetimesfile);
#endif
		printf("%d Cannot open active.times file.\r\n", ERR_FAULT);
		(void) fflush(stdout);
		return;
	}

	/*	    YYMMDD		    HHMMSS	*/
	if (strlen(argv[1]) != 6 || strlen(argv[2]) != 6) {
		printf("%d Date/time must be in form YYMMDD HHMMSS.\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		(void) fclose(date_fp);
		return;
	}

	(void) strcpy(line, argv[1]);			/* yymmdd */
	(void) strcat(line, argv[2]);			/* hhmmss */

	date = dtol(line);
	if (date < 0) {
		printf("%d Invalid date specification.\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		(void) fclose(date_fp);
		return;
	}

	argc -= 3;
	argv += 3;

	if (argc > 0 && !strcasecmp(*argv, "GMT")) { /* We store stuff in GMT */
		++argv;				/* anyway, so this is */
		--argc;				/* a "noop" */
	} else 					/* But that means not GMT */
		date = local_to_gmt(date);	/* is a definite "op" */

	if (argc > 0) {
		distcount = get_distlist(&dist_list, *argv);
		if (distcount < 0) {
			printf("%d Bad distribution list: %s\r\n",
				ERR_CMDSYN, *argv);
			(void) fflush(stdout);
			(void) fclose(date_fp);
			return;
		}
	}

	printf("%d New newsgroups since %s follow.\r\n", OK_NEWGROUPS, line);

	while (fgets(line, sizeof(line), date_fp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		if ((cp = index(line, ' ')) != NULL)
			*cp = '\0';
		if (atoi(cp + 1) < date)
			continue;

		if (distcount == 0) {
			reqlist[0] = line;
			reqlist[1] = NULL;

			if (ngpermcount) {
				if (ngmatch(s1strneql, ALLBUT, ngpermlist,
					    ngpermcount, reqlist, 1) == 0) {
					continue;
				}
			} else if (ALLBUT == 0)
				continue;
		} else {
			cp = index(line, '.');
			if (cp == NULL)
				continue;
			*cp = '\0';
			for (i = 0; i < distcount; ++i) {
				if (strcmp(line, dist_list[i]) == 0) {
					*cp = '.';
					break;
				}
			}
			if (i == distcount)
				continue;
		}
		i = find_group(line, &low_msg, &high_msg);
		if (i < 0)
			continue;
		cp = group_array[i] + strlen(line);
		while (isdigit(*cp) || isspace(*cp)) cp++;
		if (*cp == '\0' || *cp == 'x' || *cp == '=')
			continue;
		putline(group_array[i]);
	}
	putline(".");
	(void) fflush(stdout);
	(void) fclose(date_fp);
}
