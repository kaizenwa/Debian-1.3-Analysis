/*
 * ascii-xfr	Ascii file transfer.
 *
 * Usage:	ascii-xfr -s|-r [-dnv] [-c character delay] [-l line delay]
 *
 * Version:	@(#)ascii-xfr  1.00  18-Feb-1996  MvS.
 *
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>

/*
 *	Externals.
 */
extern int getopt();
extern int optind;
extern char *optarg;

/*
 *	Global variables.
 */
char *Version = "@(#)ascii-xfr  1.00  18-Feb-1996  MvS";

int cdelay = 0;
int ldelay = 0;
int dotrans = 1;
int eofchar = 26;
int verbose = 0;
time_t start;
time_t last;
unsigned long bdone = 0;

/*
 *	Millisecond delay.
 */
void ms_delay(ms)
int ms;
{
#if defined(__linux__) || defined(BSD)
  usleep(1000 * ms);
#endif
}

/*
 *	Output a line and delay if needed.
 */
void lineout(line)
char *line;
{
  while(*line) {
	putchar(*line++);
	if (cdelay) {
		fflush(stdout);
		ms_delay(cdelay);
	}
  }
}

/*
 *	Show the up/download statistics.
 */
void stats(force)
int force;
{
  time_t now;
  time_t dif;

  if (!verbose) return;

  time(&now);
  dif = now - last;
  if (!force && dif < 2) return;
  if (dif < 1) dif = 1;
  last = now;

  fprintf(stderr, "\r%.1f Kbytes transferred at %d CPS",
	(float)bdone / 1024, (int)(bdone / dif));
  fflush(stderr);
}

/*
 *	Send a file in ASCII mode.
 */
int asend(file)
char *file;
{
  FILE *fp;
  char line[1024];
  char *s;
  int first = 1;

  if ((fp = fopen(file, "r")) == NULL) {
	perror(file);
	return -1;
  }

  while(fgets(line, 1024, fp) != NULL) {
	if (dotrans && (s = strrchr(line, '\n')) != NULL) {
		if (s > line && *(s - 1) == '\r')
			s--;
		*s = 0;
		lineout(line);
		lineout("\r\n");
		bdone += strlen(line) + 2;
	} else {
		lineout(line);
		bdone += strlen(line);
	}
	if (ldelay) {
		fflush(stdout);
		ms_delay(ldelay);
	}
	stats(first);
	first = 0;
  }
  putchar(eofchar);
  fflush(stdout);
  fclose(fp);

  return 0;
}

/*
 *	Receive a file in ASCII mode.
 */
int arecv(file)
char *file;
{
  FILE *fp;
  char line[1024];
  char *s;
  int n;
  int first = 1;

  if ((fp = fopen(file, "w")) == NULL) {
	perror(file);
	return -1;
  }

  while((n = read(0, line, 1024)) > 0) {
	for(s = line; n-- >0; s++) {
		if (*s == eofchar) break;
		if (dotrans && *s == '\r') continue;
		bdone++;
		fputc(*s, fp);
	}
	stats(first);
	first = 0;
	if (*s == eofchar) break;
  }
  fclose(fp);

  return 0;
}

void usage()
{
  fprintf(stderr, "\
Usage: ascii-xfr -s|-r [-dvn] [-l linedelay] [-c character delay] filename\n\
       -s:  send\n\
       -r:  receive\n\
       -d:  set End Of File character to Control-D (instead of Control-Z)\n\
       -v:  verbose (statistics on stderr output)\n\
       -n:  do not translate CRLF <--> LF\n\
       Delays are in milliseconds.\n");
  exit(1);
}

int main(argc, argv)
int argc;
char **argv;
{
  int c;
  int what = 0;
  char *file;
  int ret;

  while((c = getopt(argc, argv, "srdvnl:c:")) != EOF) {
	switch(c) {
		case 's':
		case 'r':
			what = c;
			break;
		case 'd':
			eofchar = 4; /* Unix, CTRL-D */
			break;
		case 'v':
			verbose++;
			break;
		case 'n':
			dotrans = 0;
			break;
		case 'l':
			ldelay = atoi(optarg);
			break;
		case 'c':
			cdelay = atoi(optarg);
			break;
		default:
			usage();
			break;
	}
  }
  if (optind != argc - 1 || what == 0) usage();
  file = argv[optind];

  time(&start);
  last = start;

  if (what == 's') {
	fprintf(stderr, "ASCII upload of \"%s\"\n", file);
	if (cdelay || ldelay)
		fprintf(stderr, "Line delay: %d ms, character delay %d ms\n",
			ldelay, cdelay);
	fprintf(stderr, "\n");
	fflush(stderr);
	ret = asend(file);
  } else {
	fprintf(stderr, "ASCII download of \"%s\"\n\n", file);
	fflush(stderr);
	ret = arecv(file);
  }
  if (verbose) {
	stats(1);
	fprintf(stderr, "... Done.\n");
	fflush(stdout);
  }

  return (ret < 0 ? 1 : 0);
}

