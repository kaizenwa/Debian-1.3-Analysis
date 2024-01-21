/* xover.c */

#include "common.h"
#include "overview.h"
#include "xover.h"
#ifndef lint
static char sccsid[] = "$Id: xover.c,v 1.2 1994/11/01 06:08:21 sob Exp sob $";
#endif
#if defined(XOVER) || defined(XROVER)
void
numlist(argc, argv, obj)
	int		argc;
	char		*argv[];
	struct xobj	*obj;
{
	register int	low, high;
	int		artnum, artptr;
	FILE		*fp;

	if (!canread) {
		printf("%d You only have permission to transfer, sorry.\r\n",
			ERR_ACCESS);
		(void) fflush(stdout);
		return;
	}

	if (!ingroup) {
		printf("%d You are not currently in a newsgroup.\r\n",
			ERR_NCING);
		(void) fflush(stdout);
		return;
	}
	if (argc != 1 && argc != 2) {
		printf("%d Usage: %s [artrange]\r\n", ERR_CMDSYN, obj->cmd);
		(void) fflush(stdout);
		return;
	}

	if (argc == 1) {
		if (art_ptr < 0 || art_ptr >= num_arts) {
			printf("%d No article is currently selected.\r\n",
				ERR_NOCRNT);
			(void) fflush(stdout);
			return;
		}
		high = low = art_array[art_ptr];
		artptr = art_ptr;
	} else {
		register char *cp = index(argv[1], '-');
		if (cp == NULL)
			low = high = atoi(argv[1]);
		else {
			*cp++ = '\0';
			low = atoi(argv[1]);
			high = atoi(cp);
			if (high < low)
				if (num_arts > 0)
					high = art_array[num_arts-1];
				else
					high = low;
		}
		artptr = 0;
	}

	xfind(obj, low);
	fp = obj->fp;

	/* Return the desired data.  This is written carefully to avoid
	 * over-long lines. */
	printf("%d %s data follows\r\n", OK_OVER, obj->name);

	for (;artptr < num_arts; artptr++) {
		if ((artnum = art_array[artptr]) < low)
			continue;
		if (artnum > high)
			break;

		if (obj->num > 0 && obj->num < artnum)
			xfind(obj, artnum);
		if (obj->num == artnum) {
			register int c;
			printf("%d", artnum);
			while ((c = getc(fp)) != EOF && c != '\n')
				putchar(c);
			if (fscanf(fp, "%d", &obj->num) != 1)
				obj->num = -1;
			printf("\r\n");
		} else {
			printf("%d", artnum);
			(obj->fake)(artnum);
		}
	}
	printf(".\r\n");
	(void) fflush(stdout);
}
#endif

#ifdef XOVER
FILE *over_open();
void over_fake();
struct xobj over = {0,-1,0,over_open,over_fake,"overview","XOVER"};

void
doxover(argc, argv)
	int		argc;
	char		*argv[];
{
	numlist(argc, argv, &over);
}

int
over_is_cheap(low, high)
int low, high;
{
	if (over.fp)
		return 1;
	if (over.open_tried)
		return 0;
	return high >= low;
}

FILE *
over_open()
{
	FILE *fp;
#ifdef OVERVIEW_DIR
	char name_buff[MAXPATHLEN], *cp;
	sprintf(name_buff, "%s/", OVERVIEW_DIR);
	cp = name_buff + strlen(name_buff);
	strcpy(cp, group_name);
	while ((cp = index(cp, '.')) != (char *) NULL)
		*cp = '/';
	sprintf(name_buff+strlen(name_buff), "/%s", OVER_NAME);
	fp = fopen(name_buff, "r");
#else
	fp = fopen(OVER_NAME, "r");
#endif
	return fp;
}

#define TOLOWER(c) (isupper(c) ? tolower(c) : (c))

int
over_header(s)
char *s;
{
	int i;
	char ch;
	ch = (isascii(*s)? TOLOWER(*s) : *s);

	for (i = 1; i < OVER_FIELD_COUNT; i++) {
		if (ch == *over_field[i]) {
			if (strcasecmp(s, over_field[i]) == 0)
				return i;
#ifdef OVER_UNIQUE_1ST_LTRS
			/* optimization assumes no 2 keywords w/same 1st ltr */
			break;
#endif
		}
	}
	return -1;
}

char *
over_grab_header(hdr, output)
int hdr;
int output;
{
	int c;
	int i = hdr;
	char *buf = NULL;

#ifdef OVER_GROUP_FIELD
	if (i > OVER_GROUP_FIELD)
		i = OVER_GROUP_FIELD;
#endif
	while (i--) {
		register int c;
		while ((c = getc(over.fp)) != '\t')
			if (c == EOF || c == '\n')
				goto no_contents;
	}
	c = getc(over.fp);
#ifdef OVER_GROUP_FIELD
	if (hdr >= OVER_GROUP_FIELD) {
		for (i = 0; c != EOF && c != '\n'; i++) {
			if (c == ':' && !over_field[hdr][i]) {
				do {
					c = getc(over.fp);
				} while (c == ' ');
				break;
			}
			if (TOLOWER(c) != over_field[hdr][i]) {
				while (c != '\t') {
					if (c == EOF || c == '\n')
						goto no_contents;
					c = getc(over.fp);
				}
				i = -1;
			}
			c = getc(over.fp);
		}
	}
#endif
	if (c == EOF || c == '\n' || c == '\t') {
no_contents:
		if (output)
			printf("(none)\r\n");
	} else if (output) {
		do {
			putchar(c);
			c = getc(over.fp);
		} while (c != EOF && c != '\n' && c != '\t');
		printf("\r\n");
	} else {
		char		*malloc(), *realloc();
		register int	size = 1024;
		buf = malloc(size);
		if (buf) {
			register int	pos = 0;
			do {
				if (pos >= size-1) {
					size += 1024;
					buf = realloc(buf, size);
					if (!buf)
						break;
				}
				buf[pos++] = c;
				c = getc(over.fp);
			} while (c != EOF && c != '\n' && c != '\t');
			if (buf)
				buf[pos] = '\0';
		}
	}
	while (c != EOF && c != '\n')
		c = getc(over.fp);
	if (c == EOF || fscanf(over.fp, "%d", &over.num) != 1)
		over.num = -1;
	return buf;
}

void
over_fake(artnum)
int artnum;
{
	char		line[NNTP_STRLEN];
	register FILE	*fp;
	register char	*cp, *cp2;
	register int	hdr;
	char		*array[OVER_FIELD_COUNT];
	char		*malloc(), *realloc();

	(void) sprintf(line, "%d", artnum);
	fp = fopen(line, "r");
	if (fp == NULL)
		return;

	for (hdr = OVER_FIELD_COUNT-1; hdr > 0; hdr--) {
		array[hdr] = 0;
	}
	cp = line;
	while (fgets(line, sizeof line, fp) != NULL) {
		if (cp && *line == '\n') {
			break;
		}
		cp2 = cp;
		cp = index(line, '\n');
		if (!cp2 || isspace(*line)) {
			if (hdr > 0 && array[hdr]) {
				register int len = strlen(array[hdr]);
				if (cp2) {
					for (cp2 = line+1; isspace(*cp2); cp2++)
						;
					*--cp2 = ' ';
				}
				else
					cp2 = line;
				if (cp)
					*cp = '\0';
				array[hdr] = realloc(array[hdr],
						     strlen(cp2) + len + 1);
				if (array[hdr])
					strcpy(array[hdr] + len, cp2);
			}
		} else if ((cp2 = index(line, ':')) != NULL) {
			*cp2 = '\0';
			if ((hdr = over_header(line)) > 0) {
				if (array[hdr])
					continue;  /* a duplicate header?!? */
				cp2 += 2;
				if (cp)
					*cp = '\0';
				array[hdr] = malloc(strlen(cp2) + 1);
				if (array[hdr])
					strcpy(array[hdr], cp2);
			}
		} else
			hdr = 0;
	}

	for (hdr = 1; hdr < OVER_FIELD_COUNT; hdr++) {
		putchar('\t');
		if (array[hdr]) {
#if defined(OVER_XREFS) && defined(OVER_XREF_PREFIX)
			if (hdr == 8)
				printf("xref: ");
#endif
			printf("%s", array[hdr]);
			free(array[hdr]);
		} else if (hdr == 6) {		/* Fudge the byte header */
			struct stat s;
			fstat(fileno(fp), &s);
			printf("%u", s.st_size);
		}
	}
	printf("\r\n");

	(void) fclose(fp);
}
#endif

#ifdef XROVER
FILE *rover_open();
void rover_fake();
struct xobj rover = {0,-1,0,rover_open,rover_fake,"reference","XROVER"};

void
doxrover(argc, argv)
	int		argc;
	char		*argv[];
{
	numlist(argc, argv, &rover);
}

FILE *
rover_open()
{
	FILE *fp;
#ifdef ROVER_DIR
	char name_buff[MAXPATHLEN], *cp;
	sprintf(name_buff, "%s/", ROVER_DIR);
	cp = name_buff + strlen(name_buff);
	strcpy(cp, group_name);
	while ((cp = index(cp, '.')) != (char *) NULL)
		*cp = '/';
	sprintf(name_buff+strlen(name_buff), "/%s", ROVER_NAME);
	fp = fopen(name_buff, "r");
#else
	fp = fopen(ROVER_NAME, "r");
#endif
	return fp;
}

void
rover_fake(artnum)
	int artnum;
{
	char		line[NNTP_STRLEN];
	register FILE	*fp;
	register char	*cp, *cp2;
	char		*references = NULL;
	char		*malloc(), *realloc();

#ifdef XOVER
	if (over_is_cheap(artnum, artnum)) {
		int hdr = over_header("references");
		if (hdr >= 0 && xfind(&over, artnum)) {
			references = over_grab_header(hdr, 0);
			goto output_refs;
		}
	}
#endif
	(void) sprintf(line, "%d", artnum);
	fp = fopen(line, "r");
	if (fp == NULL)
		return;

	cp = line;
	while (fgets(line, sizeof line, fp) != NULL) {
		if (cp && *line == '\n') {
			break;
		}
		cp2 = cp;
		cp = index(line, '\n');
		if (!cp2 || isspace(*line)) {
			if (references) {
				register int len = strlen(references);
				if (cp2) {
					for (cp2 = line+1; isspace(*cp2); cp2++)
						;
					*--cp2 = ' ';
				}
				else
					cp2 = line;
				if (cp)
					*cp = '\0';
				references = realloc(references,
						     strlen(cp2) + len + 1);
				if (references)
					strcpy(references + len, cp2);
			}
		} else if (references)
			break;
		else if ((cp2 = index(line, ':')) != NULL) {
			*cp2 = '\0';
			if (strcasecmp(line, "references") == 0) {
				cp2 += 2;
				if (cp)
					*cp = '\0';
				references = malloc(strlen(cp2) + 1);
				if (references)
					strcpy(references, cp2);
			}
		}
	}

	(void) fclose(fp);

#ifdef XOVER
output_refs:
#endif
	if (references) {
		cp2 = references + strlen(references) - 1;
		while ((cp = rindex(references, '<')) != NULL) {
			while (cp2 >= cp
			    && ((unsigned char)*cp2 <= ' ' || *cp2 == ','))
				cp2--;
			cp2[1] = '\0';
			/* Quit parsing references if this one is garbage. */
			if (!valid_message_id(cp, cp2))
				break;
			/* Dump all domains that end in '.' */
			if (cp2[-1] == '.')
				break;
			if (!gethistent(cp, 0) || group_artnum == 0)
				printf(" %s", cp);
			else {
				printf(" %d", group_artnum);
				break;
			}
			*cp = '\0';
			cp2 = cp-1;
		}
		free(references);
	}
	printf("\r\n");
}

/* Check if the string we've found looks like a valid message-id reference.
*/
int
valid_message_id(start, end)
register char *start, *end;
{
	char *mid;

	if (start == end)
		return 0;

	if (*end != '>') {
		/* Compensate for space cadets who include the header in their
		** subsitution of all '>'s into another citation character. */
		if (*end == '<' || *end == '-' || *end == '!' || *end == '%'
		 || *end == ')' || *end == '|' || *end == ':' || *end == '}'
		 || *end == '*' || *end == '+' || *end == '#' || *end == ']'
		 || *end == '@' || *end == '$')
			*end = '>';
	} else if (end[-1] == '>') {
		*(end--) = '\0';
	}
	/* Id must be "<...@...>" */
	if (*start != '<' || *end != '>' || (mid = index(start, '@')) == NULL
	 || mid == start+1 || mid+1 == end)
		return 0;
	return 1;
}
#endif

#if defined(XOVER) || defined(XROVER)
int
xfind(obj, artnum)
	struct xobj	*obj;
	int		artnum;
{
	FILE		*fp = obj->fp;
	int		file_num = obj->num;
	if (fp) {
		if (file_num < 0 || file_num > artnum) {
			fseek(fp, 0L, 0);
			file_num = 0;
		}
	} else {
		if (!obj->open_tried)
			obj->fp = fp = (obj->open)();
		obj->open_tried = 1;
		if (!fp) {
			obj->num = -1;
			return 0;
		}
		file_num = 0;
	}
	if (!file_num)
		fscanf(fp, "%d", &file_num);
	while (1) {
		register int c;
		if (feof(fp)) {
			file_num = -1;
			break;
		}
		if (file_num >= artnum) {
			break;
		}
		while ((c = getc(fp)) != EOF && c != '\n')
			continue;
		fscanf(fp, "%d", &file_num);
	}
	obj->num = file_num;
	return file_num == artnum;
}

void
xclose(obj)
struct xobj *obj;
{
	if (obj->fp) {
		fclose(obj->fp);
		obj->fp = NULL;
		obj->num = -1;
	}
	obj->open_tried = 0;
}

void
close_xfiles()
{
#ifdef XOVER
	xclose(&over);
#endif
#ifdef XROVER
	xclose(&rover);
#endif
}
#endif
