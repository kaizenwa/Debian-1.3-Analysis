/*
 * quick kludge to filter out telnet protocol goop.  low quality code, do NOT
 * use for production.  Do NOT redistribute!
 */
/* Mark Moraes, University of Toronto */

#include <stdio.h>
#include <string.h>
#include "telnet.h"

#define MAXSTR 1024	/* 10 for debugging */

static char *progname;
static char eol = LF;
static int debug = 0;

#define dprintf	if (debug == 0) ; else printf

/* For getopt() */
extern int optind;
extern char *optarg;

/*
 * from_nvt() is the half that filters stuff sent by the telnet Network
 * Virtual Terminal (NVT) -- the filtered output is suitable for sending
 * to a tty.  We need a to_nvt() that transforms CR to CR NUL (optionally,
 * since we may already be getting CR LF) and IAC to IAC IAC.
 *
 * might be nice to make it possible to transform CR LF to LF instead of
 * CR, optionally, for both from_nvt() and to_nvt().
 *
 * might be nice to send DO NO_GA DONT ECHO to try to get
 * character at a time in to_nvt();
 *
 * should we sent the BINARY option and use unsigned char?
 */

int
main(argc, argv)
int argc;
char **argv;
{
	int c, dofrom = 0, doto = 0, errs = 0;

	progname = argv[0] ? argv[0] : "(no-argv[0])";
	if (debug > 0)
		setbuf(stdout, (char *) NULL);
	while((c = getopt(argc, argv, "dftr")) != EOF) {
		/* optarg has the argument if the option was followed by ':' */
		switch (c) {
		case 'd':
			debug++;
			break;
		case 'f':
			dofrom = 1;
			break;
		case 't':
			doto = 1;
			break;
		case 'r':
			eol = CR;
			break;
		case '?':
			errs++;
			break;
		}
	}
	if (errs > 0 || (dofrom == 0 && doto == 0) || optind < argc) {
		fprintf(stderr, "Usage: %s -f|-t", progname);
		exit(1);
	}
	if (dofrom > 0)
		from();
	else
		to();
	return 0;
}

from()
{
	char buf[MAXSTR];
	int cc;

	while((cc = read(0, buf, sizeof buf)) > 0) {
		from_nvt(buf, cc);
	}
	if (cc < 0) {
		strcat(strcpy(buf, progname), ": read");	/* yechh! */
		perror(buf);
		exit(1);
	}
}

from_nvt(buf, buflen)
char *buf;
int buflen;
{
	register char *cp = buf;
	register char *end = buf + buflen;
	register char *start = cp;
	register char c;
#define SBASE	0
#define SCMD	1
#define SOPT	2
#define SCRLF	3
	static int savestate = SBASE;
	register int state = savestate;

	while(cp != end) {
		c = *cp++;
		/* state machine to ignore telnet option stuff */
		switch(state) {
		case SBASE:
			/* copying */
			if (c == IAC || c == CR) {
				/* first flush buffer */
				cp--;
				if (cp > start) {
					dprintf("SBASE, writing %d chars\n",
						cp - start);
					(void) write(1, start, cp - start);
				}
				cp++;
				if (c == IAC) {
					/* IAC, next char is cmd */
					state = SCMD;
					dprintf("IAC -> SCMD\n");
				} else {
					/* CR, expect LF or NUL */
					state = SCRLF;
					dprintf("CR -> SCRLF\n");
				}
			} else {
				dprintf("CHAR\n");
			}
			break;
		case SCMD:
			if (c == IAC) {
				/* "IAC IAC" sequence means IAC */
				start = cp - 1;
				state = SBASE;
				dprintf("IAC -> SBASE\n");
			} else if (c == DO || c == DONT ||
				   c == WILL || c == WONT) {
				/* IAC DO|DONT|WILL|WONT, expect option */
				state = SOPT;
				dprintf("DO|DONT|WILL|WONT -> SOPT\n");
			} else {
				/* IAC CMD, ignore it, start copying again */
				start = cp;
				state = SBASE;
				dprintf("CMD -> SBASE\n");
			}
			break;
		case SOPT:
			/*
			 * "IAC DO|DONT|WILL|WONT option", ignore, go back
			 * to copying
			 */
			start = cp;
			state = SBASE;
			dprintf("option -> SBASE\n");
			break;
		case SCRLF:
			if (c == LF || c == NUL) {
				/* put out eol, go back to copying */
				(void) write(1, &eol, 1);
				start = cp;
				state = SBASE;
				dprintf("LF|NUL -> SBASE\n");
			}
			break;
		}
	}
	savestate = state;
	if (state == SBASE && cp > start) {
		dprintf("SBASE, writing %d chars\n", cp - start);
		(void) write(1, start, cp - start);
	}
}

to()
{
	char buf[MAXSTR];
	int cc;

	while((cc = read(0, buf, sizeof buf)) > 0) {
		to_nvt(buf, cc);
	}
	if (cc < 0) {
		strcat(strcpy(buf, progname), ": read");	/* yechh! */
		perror(buf);
		exit(1);
	}
}

to_nvt(buf, buflen)
char *buf;
int buflen;
{
	register char *cp = buf;
	register char *end = buf + buflen;
	register char *start = cp;
	register char c;

	while(cp != end) {
		c = *cp++;
		if (c == IAC) {	/* IAC = IAC IAC */
			char tc = IAC;

			if (cp > start)
				(void) write(1, start, cp - start);
			(void) write(1, &tc, 1);
			start = cp;
		} else if (c == eol) { /* eol -> CR LF */
			char *ts = "\r\n";

			cp--;
			if (cp > start)
				(void) write(1, start, cp - start);
			(void) write(1, ts, 2);
			cp++;
			start = cp;
		} else if (c == '\r' && eol != '\r') {	/* add NUL */
			char tc = '\0';
			
			if (cp > start)
				(void) write(1, start, cp - start);
			(void) write(1, &tc, 1);
			start = cp;
		}
	}
	if (cp > start)
		(void) write(1, start, cp - start);
}
