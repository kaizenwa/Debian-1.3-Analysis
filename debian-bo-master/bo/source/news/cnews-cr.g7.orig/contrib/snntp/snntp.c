/*
 * snntp, mark 2 - transmit news articles via NNTP
 *
 * snntp site <batch >newbatch 2>log
 *
 * exit stati:	0: normal, newbatch lists any unsent articles
 *		1: didn't send a single article to site; throw away newbatch
 *		other: something horrid happened
 *
 * batch is in C News "n" format (relative-file message-id)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <setjmp.h>

#include "fgetfln.h"
#include "ipc.h"
#include "config.h"

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

#define YES 1
#define NO 0

#define EX_OK 0
#define EX_NOSEND 1
#define EX_OTHER 2

/* max. sec.s to send one article (consider SLIP link) */
#define MAXARTTIME (20*60)

#define SENDLATER (-1)		/* ``count'' to indicate `try again later' */

/* imports */
extern int optind;
extern char *optarg;

/* exports */
char *progname = "";
int debug;
int hooting = NO;

/* privates */

jmp_buf errbuf;

/* ARGSUSED */
static
spring(sig)
int sig;
{
	signal(sig, spring);
	longjmp(errbuf, 1);
	/* NOTREACHED */
}

/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c, errflg = 0;

	if (argc > 0)
		progname = argv[0];
	while ((c = getopt(argc, argv, "dv")) != EOF)
		switch (c) {
		case 'd':
			++debug;
			break;
		case 'v':
			hooting = YES;
			break;
		default:
			errflg++;
			break;
		}
	if (errflg || optind != argc - 1) {
		(void) fprintf(stderr, "usage: %s [-dv] site\n", progname);
		exit(2);
	}
	cd(fullartfile((char *)NULL));		/* move to spool directory */
	if (setjmp(errbuf) != 0) {
		fprintf(stderr,
			"%s: i/o error or timeout on network connection\n",
			progname);
		exit(EX_OTHER);
	}
	signal(SIGPIPE, spring);	/* in case the remote shuts down */
	signal(SIGALRM, spring);
	exit(sendsite(argv[optind]));
	/* NOTREACHED */
}

int					/* response code or -1 on error */
getresp(netfp)				/* read nntp response code line */
FILE *netfp;
{
	int respcode = -1;		/* assume protocol error */
	char resp[1024];

	(void) fseek(netfp, 0L, 1);	/* allow i/o reversal */
	resp[0] = 'x';
	while (tgets(resp, sizeof resp, netfp) != EOF && resp[0] == '1')
		;			/* ignore 1xx informational msgs */
	if (isascii(resp[0]) && isdigit(resp[0]))
		respcode = atoi(resp);
	return respcode;
}

/*
 * sendfile - send articles named on stdin (in "n" format) to site.
 * names of articles wanted but not sent are copied to stdout.
 */
int					/* exit status */
sendsite(site)
char *site;
{
	register char *line, *sp, *msgid;
	register int count;
	int net, filessent = 0, respcode;
	char *ipcname, *file;
	FILE *netfp;

	ipcname = ipcpath(site, "tcp", "nntp");
	net = ipcopen(ipcname, "heavy");
	if (net < 0) {
		fprintf(stderr, "%s: can't dial %s (%s)\n",
			progname, ipcname, errstr);
		return EX_OTHER;
	}
	netfp = fdopen(net, "r+");
	if (netfp == NULL) {
		fprintf(stderr, "%s: can't fdopen network connection\n",
			progname);
		return EX_OTHER;
	}
	respcode = getresp(netfp);
	if (respcode != 200 && respcode != 201) {
		fprintf(stderr, "%s: got %d, not 200 nor 201 greeting\n",
			progname, respcode);
		return EX_OTHER;		/* protocol error */
	}
	while ((line = fgetln(stdin)) != NULL) {
		trim(line);
		sp = strchr(line, ' ');
		if (sp == NULL)
			continue;		/* malformed input */
		*sp = '\0';
		file = line;
		msgid = sp + 1;
		count = sendart(netfp, file, msgid);
		if (count == SENDLATER) {
			/* try sending again later */
			(void) printf("%s %s\n", file, msgid);
			count = 0;		/* no article was sent */
		}
		filessent += count;
	}
	if (setjmp(errbuf) != 0) {
		fprintf(stderr, "%s: i/o error on network connection\n",
			progname);
		exit(EX_OTHER);
	}
	(void) fseek(netfp, 0L, 1);		/* allow i/o reversal */
	(void) tputs("quit\n", netfp);
	if (fclose(netfp) == EOF) {
		fprintf(stderr, "%s: i/o error on network connection\n",
			progname);
		exit(EX_OTHER);
	}
	return filessent == 0? EX_NOSEND: EX_OK;
}

int				/* count of files sent; SENDLATER to retry */
sendart(netfp, file, msgid)
FILE *netfp;
char *file, *msgid;
{
	int count = 0, art;
	static int netlive = YES;

	if (!netlive)
		return SENDLATER;	/* don't even try, no point */
	art = open(file, 0);
	if (art < 0)
		return count;		/* missing or unreadable: don't copy */

	if (setjmp(errbuf) != 0) {	/* protocol time-out? */
		fprintf(stderr,
			"%s: i/o error or timeout on network connection\n",
			progname);
		netlive = NO;		/* communication has broken down */
		count = SENDLATER;
	} else {
		(void) alarm(MAXARTTIME);
		count = netsend(netfp, art, msgid); /* may longjmp on signal */
	}
	(void) alarm(0);		/* stop timing protocol transactions */
	(void) close(art);
	return count;
}

int				/* count of files sent; SENDLATER to retry */
netsend(netfp, art, msgid)
FILE *netfp;
int art;
char *msgid;
{
	char buff[8192+1];
	int respcode, bytes, lastc = '\n';

	(void) fseek(netfp, 0L, 1);		/* allow i/o reversal */
	(void) tputs("ihave ", netfp);
	(void) tputs(msgid, netfp);
	(void) tputs("\n", netfp);

	respcode = getresp(netfp);
	if (respcode == 435)
		return 0;			/* got it; don't send */
	else if (respcode != 335) {
		fprintf(stderr, "%s: got %d, not 335 nor 435 for %s\n",
			progname, respcode, msgid);
		return SENDLATER;		/* protocol error */
	} else {
		/* 335: haven't got it, please send */
		(void) fseek(netfp, 0L, 1);	/* allow i/o reversal */
		while ((bytes = read(art, buff, sizeof buff-1)) > 0) {
			buff[bytes] = '\0';
			lastc = buff[bytes-1];
			if (tputs(buff, netfp) == EOF) {
				lastc = 'x';
				break;
			}
		}
		if (lastc != '\n')
			(void) tputs("\n", netfp);
		(void) putc('.', netfp);	/* prevent dot hiding */
		(void) tputs("\n", netfp);

		respcode = getresp(netfp);
		if (ferror(netfp)) {
			fprintf(stderr, "%s: i/o error on network connection\n",
				progname);
			return SENDLATER;
		}
		if (respcode == 235)
			return 1;		/* success: got it okay */
		else if (respcode == 437)
			return 1;  /* success: got it, rejected, don't resend */
		else if (respcode == 436) {
			fprintf(stderr, "%s: transfer of %s failed\n", progname,
				msgid);
			return SENDLATER;
		} else {
			fprintf(stderr,
				"%s: got %d, not 235 nor 436 nor 437 for %s\n",
				progname, respcode, msgid);
			return SENDLATER;	/* protocol error */
		}
	}
}
