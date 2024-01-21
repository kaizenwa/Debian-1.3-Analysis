/*
 * mkov - turn news headers into news overview index lines
 * See the file COPYRIGHT for the copyright notice.
 *
 * to add additional headers to the database, search for "step" and
 * follow the simple directions.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fgetfln.h>

#define DEFXREF 50

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

/* imports */
extern int optind;
extern char *optarg;
extern char *gethdr(), *strsave();
extern FILE *efopen();

/* exports */
char *progname = "";
int debug;

/* forwards */
char *str(), *parsehdrs();

/* privates */
struct headers {
	int	h_cnt;
	/* common headers */
	char *h_num;
	char *h_from;
	char *h_subj;
	char *h_date;
	char *h_msgid;
	char *h_refs;
	char *h_bytes;
	char *h_lines;
	/* used internally; not normally put in the database */
	char *h_xref;
	char *h_intxref;
	/* extension headers */
	/* step 1 of 4: add new field definition before this line */
};
static int articles = 0;	/* if true, read article files */
static time_t oldtime = 0;
static int prgrps = 0;	/* if true, print group name(s) first, even under -a */
static int readnames = 0;	/* if true, read filenames from stdin */

/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c, errflg = 0;
	char *older = NULL;

	if (argc > 0)
		progname = argv[0];
	while ((c = getopt(argc, argv, "adino:")) != EOF)
		switch (c) {
		case 'a':
			articles++;
			break;
		case 'd':
			++debug;
			break;
		case 'i':
			readnames++;
			break;
		case 'n':
			prgrps++;
			break;
		case 'o':
			older = optarg;
			break;
		default:
			errflg++;
			break;
		}
	if (errflg || (readnames && optind < argc)) {
		(void) fprintf(stderr, "usage: %s [-adin] [-o file] [file]...\n",
			progname);
		exit(2);
	}

	if (older != NULL) {
		struct stat statb;

		if (stat(older, &statb) < 0)
			error("can't stat %s", older);
		oldtime = statb.st_mtime;
	}

	if (readnames) {
		char *name;

		while ((name = fgetline(stdin, (size_t *)NULL)) != NULL) {
			if (!(articles && !artname(name))) {
				FILE *in;

				in = fopen(name, "r");
				if (in == NULL)
					warning("can't open `%s'", name);
				else {
					process(in, name);
					(void) fclose(in);
				}
			}
		}
	} else if (optind >= argc)
		process(stdin, "stdin");
	else
		for (; optind < argc; optind++) {
			char *name = argv[optind];

			if (STREQ(name, "-"))
				process(stdin, "-");
			else if (!(articles && !artname(name))) {
				FILE *in;

				in = fopen(name, "r");
				if (in == NULL)
					warning("can't open `%s'", name);
				else {
					process(in, name);
					(void) fclose(in);
				}
			}
		}
	exit(0);
}

int
artname(s)
char *s;
{
	register char *base = strrchr(s, '/');

	if (base == NULL)
		base = s;
	else
		base++;
	return isascii(*base) && isdigit(*base);
}

/*
 * process - process input file
 */
process(in, inname)
FILE *in;
char *inname;
{
	register char *hdr;
	char *base = strrchr(inname, '/'), *artnumstr;
	char bytes[25];
	struct headers hdrs;

	if (base == NULL)
		base = inname;
	else
		base++;
	if (articles) {
		struct stat statb;

		if (fstat(fileno(in), &statb) < 0)
			error("fstat of %s failed", inname);
		if ((statb.st_mode&S_IFMT) == S_IFDIR ||
		    (oldtime > 0 && statb.st_mtime > oldtime))
			return;		/* ignore directories and new files */
		artnumstr = base;
		(void) sprintf(bytes, "%ld", (long)statb.st_size);
	}
	do {
		hdr = parsehdrs(in, &hdrs);	/* hdr is malloced; don't free */
		if (hdrs.h_cnt == 0) {		/* no valid headers parsed? */
			if (hdr == NULL)	/* EOF? */
				break;
			else if (*hdr == '\n')	/* blank line? */
				continue;	/* ignore it and try again */
			else {
				fprintf(stderr, "%s: garbage input: %s",
					progname, hdr);
				continue;
			}
		}
		/* print index line from headers */
		if (articles) {
			hdrs.h_num = strsave(artnumstr);
			hdrs.h_bytes = strsave(bytes);
			if (!prgrps)
				prhdrs(&hdrs);
			else if (hdrs.h_intxref == NULL)
				fprintf(stderr, "%s: no Xref: header in %s\n",
					progname, hdrs.h_msgid);
			else
				prstridx(&hdrs);
		} else if (hdrs.h_intxref == NULL)
			fprintf(stderr, "%s: no Xref: header in %s\n", progname,
				hdrs.h_msgid);
		else				/* reading header stream */
			prstridx(&hdrs);

		freehdrs(&hdrs);
	} while (!articles && hdr != NULL);
}

char *					/* NULL at EOF, or malloced, don't free */
parsehdrs(in, hdrp)
FILE *in;
register struct headers *hdrp;
{
	register char *hdr;
	int ishdr = 0;
	long lim;
	static struct headers zhdrs;

	*hdrp = zhdrs;
	while ((lim = -1, hdr = gethdr(in, &lim, &ishdr)) != NULL && ishdr) {
		register char *kwp;
		char *colon = strchr(hdr, ':');

		if (colon == NULL)
			break;			/* can't happen */
		*colon = '\0';

		/* canonicalise case */
		for (kwp = hdr; *kwp != '\0'; kwp++)
			if (isascii(*kwp) && isupper(*kwp))
				*kwp = tolower(*kwp);
		/* record initial address for later freeing */
		if (STREQ(hdr, "number"))
			hdrp->h_num = str(colon+1, 0);
		else if (STREQ(hdr, "from"))
			hdrp->h_from = str(colon+1, 0);
		else if (STREQ(hdr, "subject"))
			hdrp->h_subj = str(colon+1, 0);
		else if (STREQ(hdr, "date"))
			hdrp->h_date = str(colon+1, 0);
		else if (STREQ(hdr, "message-id"))
			hdrp->h_msgid = str(colon+1, 0);
		else if (STREQ(hdr, "references"))
			hdrp->h_refs = str(colon+1, 0);
		else if (STREQ(hdr, "bytes"))
			hdrp->h_bytes = str(colon+1, 0);
		else if (STREQ(hdr, "lines"))
			hdrp->h_lines = str(colon+1, 0);
		/* step 2 of 4: add new header recognition before this line */
		else if (STREQ(hdr, "xref")) {
			hdrp->h_xref = str(colon+1, 0);
			hdrp->h_intxref = str(colon+1, 1);
		}
		hdrp->h_cnt++;
		*colon = ':';
		/* must not free hdr; gethdr does that internally */
	}
	return hdr;
}

prfld(fld)
register char *fld;
{
	if (fld != NULL)
		(void) fputs(fld, stdout);
}

putfld(fld)
char *fld;
{
	(void) putchar('\t');
	prfld(fld);
}

putextfld(keywd, extfld)
char *keywd, *extfld;
{
	(void) putchar('\t');
	if (extfld != NULL) {
		(void) fputs(keywd, stdout);
		(void) fputs(": ", stdout);
		(void) fputs(extfld, stdout);
	}
}

prhdrs(hdrp)
register struct headers *hdrp;
{
	register char *p;

	prfld(hdrp->h_num);
	putfld(hdrp->h_subj);
	putfld(hdrp->h_from);
	putfld(hdrp->h_date);
	putfld(hdrp->h_msgid);
	putfld(hdrp->h_refs);
	putfld(hdrp->h_bytes);
	putfld(hdrp->h_lines);
	/* put Xref in only if there is more than one locator in it */
	if (hdrp->h_xref != NULL)
		p = strchr(hdrp->h_xref, ' ');	/* p -> space after relayer */
	else
		p = NULL;
	if (p != NULL)
		p = strchr(p, ':');	/* p -> colon in first location */
	if (p != NULL)
		p = strchr(p+1, ':');	/* p -> colon in second location */
	if (p != NULL)			/* at least two locations! */
		putextfld("xref", hdrp->h_xref);
	/* step 3 of 4: add new header output before this line */
	(void) putchar('\n');
}

prstridx(hdrp)		/* print index lines from header stream */
register struct headers *hdrp;
{
	register int i, nxrefs;
	char *xref[DEFXREF];
	char **xrefp = xref;

	nxrefs = awksplit(hdrp->h_intxref, &xrefp, DEFXREF, " ");
	if (xrefp == NULL)
		fprintf(stderr, "%s: out of memory\n", progname);
	else if (nxrefs < 2)
		fprintf(stderr, "%s: too few Xref:s\n", progname);
	else {
		/* start at 1 to skip site name */
		for (i = 1; i < nxrefs; i++) {
			char *colon = strchr(xrefp[i], ':');

			if (colon == NULL)
				fprintf(stderr, "%s: bad Xref %s in %s\n",
					progname, xrefp[i], hdrp->h_msgid);
			else {
				*colon = '\0';
				printf("%s\t", xrefp[i]);
				hdrp->h_num = strsave(colon+1);
				prhdrs(hdrp);
				*colon = ':';
			}
		}
		if (xrefp != xref)
			free((char *)xrefp);
	}
}

nnfree(s)
register char *s;
{
	if (s != NULL)
		free(s);
}

freehdrs(hdrp)
register struct headers *hdrp;
{
	nnfree(hdrp->h_num);
	nnfree(hdrp->h_subj);
	nnfree(hdrp->h_from);
	nnfree(hdrp->h_date);
	nnfree(hdrp->h_msgid);
	nnfree(hdrp->h_refs);
	nnfree(hdrp->h_bytes);
	nnfree(hdrp->h_lines);
	nnfree(hdrp->h_xref);
	/* don't free h_intxref here; it's special */
	/* step 4 of 4: free new header before this line */
}

char *					/* malloced */
str(hdrln, xref)		/* strip leading & trailing whitespace */
char *hdrln;
register int xref;
{
	register char *p, *kwp, *copy;
	register char c;
	register int inspace = 0;

	for (p = hdrln; *p != '\0' && isascii(*p) && isspace(*p); p++)
		;		/* skip leading whitespace */
	copy = strsave(p);
	p = NULL;
	for (kwp = copy; (c = *kwp) != '\0'; kwp++) {
		if (c == '\t' || c == '\n')
			*kwp = c = ' ';	/* keep format sane */
		else if (xref && c == '.')
			*kwp = c = '/';	/* group -> dir. */
		if (!inspace && c == ' ') {
			inspace++;
			p = kwp;
		}
		if (c != ' ')
			inspace = 0;
	}
	if (p != NULL)
		*p = '\0';	/* trim trailing whitespace */
	return copy;
}
