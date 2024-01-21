#ifndef lint
static char	sccsid[] = "@(#)$Id: xhdr.c,v 1.13 1994/11/01 06:08:21 sob Exp sob $";
#endif

#include "common.h"

#ifdef XHDR

#ifdef XOVER
#include "xover.h"
#endif

/* forward declarations */
void print_header();

/*
 * XHDR header [<messageid>|articlerange]
 *
 * header is a case-insensitive header field, minus any colons.
 *
 * articlerange is one of:
 *	an article number
 *	an article number followed by a dash to indicate all following
 *	an article number followed by a dash followed by another
 *		article number.
 * e.g.,
 * XHDR subject			retrieve subject of current article
 * XHDR subject 5589-6325	retrieve subject of arts 5589 to 6325
 * XHDR subject 5589-		retrieve subject of arts 5589 and up
 * XHDR subject 5589		retrieve subject of art 5589 only
 * XHDR subject <123@ucbvax>	retrieve subject of art <123@ucbvax>
 *
 * This command is an extension, and not included in RFC 977.
 */

void
xhdr(argc, argv)
	int		argc;
	char		*argv[];
{
	char		buf[MAXPATHLEN];
	register int	artptr;
	register int	artnum;
	register int	low, high;
	register FILE	*fp;
#ifdef XOVER
	int		over_candidate, hdr;
#endif

	if (argc < 2 || argc > 3) {
		printf("%d Usage: XHDR headerfield [artrange|<message-id>]\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	if (!canread) {
		printf("%d You only have permission to transfer, sorry.\r\n",
			ERR_ACCESS);
		(void) fflush(stdout);
		return;
	}

	/* Handle message-id requests */

	if (argc == 3 && *argv[2] == '<') {	/* Message ID */
		fp = openartbyid(argv[2]);
		if (fp == NULL) {
			printf("%d No article by message-id %s, sorry.\r\n",
				ERR_NOART, argv[2]);
			(void) fflush(stdout);
			return;
		}
		printf("%d %d %s header of article %s.\r\n%s ",
			OK_HEAD, group_artnum, argv[1], argv[2], argv[2]);
		print_header(fp, argv[1]);
		(void) fclose(fp);

		putline(".");
		(void) fflush(stdout);
		return;
	}

	/*
	 * It must be a range of articles, which means that we need
	 * to be in a newsgroup already.
	 */

	if (!ingroup) {
		printf("%d You are not currently in a newsgroup.\r\n",
			ERR_NCING);
		(void) fflush(stdout);
		return;
	}

	if (argc == 2) {
		if (art_ptr < 0 || art_ptr >= num_arts) {
			printf("%d No article is currently selected.\r\n",
				ERR_NOCRNT);
			(void) fflush(stdout);
			return;
		}
		high = low = art_array[art_ptr];
		artptr = art_ptr;
	} else {
		register char *cp = index(argv[2], '-');
		if (cp == NULL)
			low = high = atoi(argv[2]);
		else {
			*cp++ = '\0';
			low = atoi(argv[2]);
			high = atoi(cp);
			if (high < low)
				if (num_arts > 0)
					high = art_array[num_arts-1];
				else
					high = low;
		}
		artptr = 0;
	}

	printf("%d %s fields follow\r\n", OK_HEAD, argv[1]);

#ifdef XOVER
	if (over_is_cheap(low, high))
		over_candidate = ((hdr = over_header(argv[1])) > 0);
	else
		over_candidate = 0;
#endif
	for (;artptr<num_arts; artptr++) {
		if ((artnum = art_array[artptr]) < low)
			continue;
		if (artnum > high)
			break;

		(void) sprintf(buf, "%d", artnum);
		printf("%s ", buf);
#ifdef XOVER
		if (over_candidate) {
			if (xfind(&over,artnum)) {
				over_grab_header(hdr, 1);
				continue;
			} else if (over.num == -1)
				over_candidate = 0;
		}
#endif
		fp = fopen(buf, "r");
		if (fp == NULL)
			continue;
		print_header(fp, argv[1]);
		(void) fclose(fp);
	}

	putline(".");
	(void) fflush(stdout);
}


void
print_header(fp, header)
	register FILE	*fp;
	register char	*header;
{
	char		line[NNTP_STRLEN];
	register char	*cp, *cp2;
	register int	found = 0;

	cp = line;
	while (fgets(line, sizeof line, fp) != NULL) {
		if (cp && *line == '\n') {
			break;
		}
		cp2 = cp;
		cp = index(line, '\n');
		if (!cp2 || isspace(*line)) {
			if (found) {
				if (cp2) {
					for (cp2 = line+1; isspace(*cp2); cp2++)
						;
					*--cp2 = ' ';
				}
				else
					cp2 = line;
				if (cp)
					*cp = '\0';
				printf("%s", cp2);
			}
		} else if (found)
			break;
		else if ((cp2 = index(line, ':')) != NULL) {
			*cp2 = '\0';
			if (strcasecmp(header, line) == 0) {
				cp2 += 2;
				if (cp)
					*cp = '\0';
				printf("%s", cp2);
				found = 1;
			}
		}
	}
	if (!found)
		printf("(none)");
	putchar('\r');
	putchar('\n');
}

#else /* !XHDR */

/* Kludge to get around Greenhills C compiler */

xhdr_greenkluydge()
{
}

#endif
