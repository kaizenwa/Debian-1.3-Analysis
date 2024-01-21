/*
 * Misc. routines useful for reading/writing the netdata spec used by
 * nntp and smtp.  Reading uses Geoff's fgetfln, writing uses stdio.
 */
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <memory.h>

#include "libcnews.h"
#include "fgetfln.h"
#include "debug.h"
#include "config.h"
#include "netdata.h"

static unsigned timeout = 0;	/* if non-zero, alarm(timeout) on network I/O */

/*
 * sets a timeout on all net_ calls that read or write.  Non-positive
 * values are equivalent to no timeout -- I/O will block until complete.
 */
int
net_timeout(n)
unsigned n;
{
	unsigned ret = timeout;

	timeout = n;
	return ret;
}

/* fgetline with timeout */
static char *
nfgetline(fp, lenp)
FILE *fp;
sizeint *lenp;
{
	char *line;
	
	if (timeout > 0)
		(void) alarm(timeout);
	line = fgetline(fp, lenp);
	if (timeout > 0)
		(void) alarm(0);
	return line;
}

/*
 * reads a line and trims off the trailing newline, strips off leading
 * period.  Returns NULL on end of article, EOF or error.
 */
char *
net_readline(fp, lenp)
FILE *fp;
sizeint *lenp;
{
	sizeint len;
	register char *line = nfgetline(fp, &len);
	register int l = len;

	if (line == NULL)
		return line;
	/*
	 * trim trailing CR LF -- if they're missing, be tolerant,
	 * shouldn't happen
	 */
	if (--l >= 0 && line[l] == '\n')
		line[l] = '\0';
	else
		++l;
	if (--l >= 0 && line[l] == '\r')
		line[l] = '\0';
	else
		++l;
	if (*line == '.') {
		line++;
		l--;
		if (*line == '\0')
			return NULL;
	}
	*lenp = l;
	return line;
}

/* reads a line and trims off trailing whitespace */
char *
net_cmdread(fp, lenp)
FILE *fp;
sizeint *lenp;
{
	sizeint len;
	register char c, *line = nfgetline(fp, &len);
	register int l = len;

	if (line == NULL)
		return line;
	do {
		c = line[--l];
	} while (isascii(c) && isspace(c));
	line[++l] = '\0';
	if (lenp)
		*lenp = l;
	dprintf(dfp, "> %s\n", line);
	return line;
}

#ifndef isascii		/* no isascii() in ANSI C.*/
#define isascii(x)
#endif
#define ISDIGIT(x)	(isascii(x) && isdigit(x))

/* Gobbles in a potentially multiline reply from a server.   Eg.
 * 200 Hello
 * or
 * 200-Hello
 *  Some random text
 * 200 Hello
 * See section 4.2 "FTP REPLIES" in RFC 959.
 * If function pfunc() is supplied, it is called for each line of the
 * reply.  net_getreply() returns the last line of the reply.
 */
char *
net_getreply(fp, lenp, pfunc, arg)
register FILE *fp;
register sizeint *lenp;
register void (*pfunc)(/* char *line, sizeint *lenp, char *arg */);
char *arg;
{
	register char *line;

	line = net_cmdread(fp, lenp);
	if (line && ISDIGIT(line[0]) && ISDIGIT(line[1]) && ISDIGIT(line[2]) &&
	    line[3] == '-') {
		do {
			if (pfunc != NULLFUNC)
				(*pfunc)(line, lenp, arg);
			line = net_cmdread(fp, lenp);
		} while (line && !(ISDIGIT(line[0]) && ISDIGIT(line[1]) &&
			   ISDIGIT(line[2]) && line[3] != '-'));
	}
	if (line && pfunc != NULLFUNC)
		(*pfunc)(line, lenp, arg);
	return line;
}

int
net_ackwrite(cp, len, str, fp)
char *cp;
sizeint len;
register char *str;
register FILE *fp;
{
	int ret;
	
	if (timeout > 0)
		(void) alarm(timeout);
	dprintf(dfp, "< %s%s%s\n", cp, (str && *str != '\0') ? " " : "",
		str != NULL? str: "");
	(void) fwrite(cp, 1, len, fp);
	if (str && *str != '\0') {
		(void) putc(' ', fp);
		(void) fputs(str, fp);
	}
	(void) putc('\r', fp);
	(void) putc('\n', fp);
	ret = fflush(fp);
	if (timeout > 0)
		(void) alarm(0);
	return ret;
}

int
net_writeline(cp, len, fp)
char *cp;
sizeint len;
register FILE *fp;
{
	if (timeout > 0)
		(void) alarm(timeout);
	if (*cp == '.')
		(void) putc('.', fp);
	(void) fwrite(cp, 1, len, fp);
	(void) putc('\r', fp);	/* !! suppress in IMAGE mode */
	(void) putc('\n', fp);
	if (timeout > 0)
		(void) alarm(0);
	return !ferror(fp)? 0: EOF;
}

int
net_endwrite(fp)
register FILE *fp;
{
	int ret;

	if (timeout > 0)
		(void) alarm(timeout);
	(void) fputs(".\r\n", fp);
	ret = fflush(fp);
	if (timeout > 0)
		(void) alarm(timeout);
	return ret;
}

struct netdata *
net_new()
{
	register struct netdata *ndp =
		(struct netdata *)emalloc(sizeof(struct netdata));

	ndp->nd_fp = NULL;					/* important */
	ndp->nd_spilled = ndp->nd_bufcnt = ndp->nd_bytes = 0;	/* aesthetic */
	return ndp;
}

int
net_getdata(fp, ndp)
FILE *fp;
register struct netdata *ndp;
{
	register char *bufp = ndp->nd_buf, *line;
	register sizeint left = ARTBATCHSIZE, nbytes = 0;
	sizeint len;

	ndp->nd_spilled = ndp->nd_bufcnt = ndp->nd_bytes = 0;
	if (ndp->nd_fp)
		rewind(ndp->nd_fp);
	while ((line = net_readline(fp, &len)) != NULL) {
		dddprintf(dfp, "line `%s'\n", line);
		line[len++] = '\n';		/* overwriting NUL */
		nbytes += len;
		dddprintf(dfp, "len %d, %d left in buf\n", len, left);
		while (left <= len) {
			/* unlikely case */
			if (left != 0)
				/* copy as much as possible */
				(void) memcpy(bufp, line, left);
			line += left;
			len -= left;
			if (ndp->nd_fp == NULL) {
				ndp->nd_fp = tmpfile();
				if (ndp->nd_fp == NULL)
					error("tmpfile() failed", "");
				ddprintf(dfp, "opened tmpfile\n");
			}
			dddprintf(dfp, "writing %d\n", ARTBATCHSIZE);
			ndp->nd_spilled += ARTBATCHSIZE;
			if (fwrite(ndp->nd_buf, 1, ARTBATCHSIZE, ndp->nd_fp) !=
			    ARTBATCHSIZE)
				return EOF;
			bufp = ndp->nd_buf;
			left = ARTBATCHSIZE;
		}
		if (len > 0) {
			dddprintf(dfp,"adding %d to buf, %d left\n", len, left);
			(void) memcpy(bufp, line, len);
			bufp += len;
			left -= len;
		}
	}
	if (ndp->nd_spilled > 0 && fflush(ndp->nd_fp) != 0)
		return EOF;
	ndp->nd_bytes = nbytes;
	ndp->nd_bufcnt = ARTBATCHSIZE - left;
	ddprintf(dfp, "article was %d bytes\n", nbytes);
	return 0;
}
