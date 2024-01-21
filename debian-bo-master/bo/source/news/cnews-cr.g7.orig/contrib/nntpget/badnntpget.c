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
	if (optind >= argc) {
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
			if (line && line[len - 1] == '\n')
				line[--len] = '\0';
		}
		if (line == NULL)
			break;
		(void) rewind(fp[0]);
		if (net_ackwrite("ARTICLE ", 8, line, fp[0]) != 0)
			error("net_ackwrite(%s) failed", line);
		(void) rewind(fp[0]);
		/* get reply, check it's a 200 code */
		if ((line = net_cmdread(fp[0], &len)) == NULL || *line != '2')
			continue;
		if (net_getdata(fp[0], ndp) != 0) {
			warning("error fetching article", "");
			continue;
		}
		if (batchadd(ndp->nd_buf, ndp->nd_bufcnt, ndp->nd_bytes,
			     ndp->nd_fp, ndp->nd_spilled) != 0)
			warning("error batching article", "");
	}
	(void) rewind(fp[0]);
	(void) net_ackwrite("QUIT ", 5, line, fp[0]);
	batchend();
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
	fp[0] = fdopen(fd, "r+");
	if (fp[0] == NULL)
		error("fdopen(socket, \"r+\") failed", "");
#if 0
	fp[1] = fdopen(fd, "w");
	if (fp[1] == NULL)
		error("fdopen(socket, \"w\") failed", "");
#endif
	line = net_cmdread(fp[0], &len);
	if (*line != '2') {
		errno = 0;
		error("remote server responded with %s", line);
	}
	if (fflush(fp[0]) != 0)
		error("fflush on socket failed", "");
}
