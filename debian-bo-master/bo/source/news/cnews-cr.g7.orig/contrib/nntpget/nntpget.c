/*
 * asks a remote server for a list of message-ids (from stdin or command
 * line) and outputs a relaynews batch on stdout
 */
/* Mark Moraes, University of Toronto */

#include <stdio.h>
#include "fgetfln.h"
#include "debug.h"
#include "config.h"
#include "libcnews.h"
#include "netdata.h"
#include "batch.h"

#define NULLSTR		((char *) NULL)
#define	STREQN(a, b, n)	(*(a) == *(b) && strncmp((a), (b), (n)) == 0)

/* for getopt */
extern int optind;
extern char *optarg;
extern int errno;

extern char *tcperror();

char *progname;
unsigned int debug;
FILE *dfp;

/* forwards */
static void fopensock(/* char *hname, char *service, FILE *fp[2] */);

/*
 * main - parse arguments and handle options
 */
int
main(argc, argv)
int argc;
char **argv;
{
	FILE *fp[2];
	char *line, *port = "nntp";
	int len, c, tmpbatches = 0, errflg = 0, cmdline = 0;
	struct netdata *ndp;

	progname = argv[0];
	dfp = stderr;
	while ((c = getopt(argc, argv, "dD:p:t")) != EOF) {
		switch (c) {
		case 'd':
			debug++;
			break;
		case 'D':
			dfp = efopen(optarg, "a+");
			break;
		case 'p':
			port = optarg;
			break;
		case 't':
			tmpbatches++;
			break;
		default:
			errflg++;
			break;
		}
	}
	if (optind >= argc || errflg > 0) {
		fprintf(stderr, "Usage: nntpget [-d] [-D] [-p port] [-t] hostname [msgid ...]\n");
		exit(1);
	}
	if (tmpbatches)
		cd(artfile(NULLSTR));
	else
		batchinit(stdout);
	fopensock(argv[optind], port, fp);
	optind++;
	cmdline = optind < argc;
	ndp = net_new();
	for (;;) {
		if (cmdline) {
			line = argv[optind++];
		} else {
			line = fgetline(stdin, &len);
			if (line != NULL)
				if (line[len - 1] == '\n')
					line[--len] = '\0';
		}
		if (line == NULL)
			break;
		if (net_ackwrite("ARTICLE", 7, line, fp[1]) != 0)
			error("net_ackwrite(%s) failed", line);
		/* get reply, check it's a 200 code */
		if ((line = net_getreply(fp[0], &len, NULLFUNC, NULLSTR))
		    == NULL || *line != '2')
			continue;
		if (net_getdata(fp[0], ndp) != 0) {
			warning("error fetching article", line);
			continue;
		}
		if (batchadd(ndp->nd_buf, ndp->nd_bufcnt, ndp->nd_bytes,
			     ndp->nd_fp, ndp->nd_spilled) != 0)
			warning("error batching article", line);
	}
	(void) net_ackwrite("QUIT", 4, NULLSTR, fp[1]);
	batchend();
	return 0;
}

static void
fopensock(hname, service, fp)
char *hname, *service;
FILE *fp[2];
{
	int fd, len;
	char *line;

	fd = tcpopen(hname, service);
	if (fd < 0)
		error("tcpopen failed in %s", tcperror());
	fp[0] = fdopen(fd, "r");
	if (fp[0] == NULL)
		error("fdopen(socket, \"r\") failed", "");
	fp[1] = fdopen(fd, "w");
	if (fp[1] == NULL)
		error("fdopen(socket, \"w\") failed", "");
	if ((line = net_getreply(fp[0], &len, NULLFUNC, NULLSTR)) == NULL)
		error("net_getreply failed", "");
	if (*line != '2') {
		errno = 0;
		error("remote server responded with %s", line);
	}
}
