/*
 * tgets, tputs - fgets & fputs that cope with the Internet's whacko
 * CRLF end of line convention and dot-hiding.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define YES 1
#define NO 0

extern int hooting;

int				/* EOF or strlen(line) */
tgets(line, size, fi)		/* fgets from TCP */
register char *line;
int size;
FILE *fi;
{
	register char *cr, *from, *to;

	*line = '\0';
	if (fgets(line, size, fi) == NULL)
		return EOF;

	/* expose hidden dot */
	if (line[0] == '.' && (line[1] == '\r' || line[1] == '\n')) {
		strncpy(line, ".\n", size);
		return EOF;
	}
	if (line[0] == '.' && line[1] == '.')
		for (to = line, from = line + 1; (*to++ = *from++) != '\0'; )
			;

	/* convert \r\n -> \n */
	cr = strchr(line, '\n');
	if (cr == NULL)
		return strlen(line);
	if (cr > line && cr[-1] == '\r') {
		*cr = '\0';
		*--cr = '\n';
	}
	if (hooting)
		(void) fprintf(stderr, "<<< %s", line);
	return cr + 1 - line;
}

/*
 * minor bug: there is only on wasnl flag for all streams, not one per stream.
 * it hasn't mattered yet.
 */
int
tputs(line, fo)				/* fputs to TCP */
register char *line;
register FILE *fo;
{
	register char *nl;
	static int wasnl = YES;		/* true if last byte was newline */

	if (hooting)
		(void) fprintf(stderr, ">>> %s", line);
	for (; *line != '\0'; line = nl + 1) {
		/* we may be in the middle of a line here */
		if (wasnl) {
			/* we are at the start of a line here */
			if (line[0] == '.')
				(void) putc('.', fo);
		}
		wasnl = NO;
		nl = strchr(line, '\n');
		if (nl == NULL) {
			(void) fputs(line, fo);
			break;
		}
		if (nl > line)
			(void) fwrite(line, 1, nl - line, fo);
		(void) fwrite("\r\n", 1, 2, fo);
		wasnl = YES;
		if (fflush(fo) == EOF)
			return EOF;
	}
	return 0;
}
