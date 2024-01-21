/* $Id: header.c,v 1.4 1993/10/28 16:49:51 chip Exp $
 *
 * A program to parse RFC 822 mail/news headers.
 *
 * usage: header [-c] [-n] [-v] [-f field] ... files
 *
 * Default action is to print entire header.  If one or more -f options
 * are given, only the specified fields are printed.  The field names are
 * not printed unless -n is specified.  Field name comparisons are case
 * insensitive unless -c is specified.  If -v is specified, all headers
 * except those specified with -f are printed.  NOTE: -v implies -n.
 *
 * Output lines are preceeded by the filename if more than one file is
 * specified.
 *
 * This program is intended for use in delivery files, to extract multi-
 * line header fields.
 *
 * $Log: header.c,v $
 * Revision 1.4  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.3  1992/11/13  19:11:07  chip
 * Make "-v" really imply "-n".
 *
 * Revision 1.2  1991/05/23  17:23:19  chip
 * Follow RFC822 definition of header syntax.
 * Guard isxxx() macros against negative values.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include <stdio.h>
#include <ctype.h>

/*
 * Manifest constants
 */

#define TRUE 1
#define FALSE 0

/*
 * Other useful macros.
 */

#define GETSIZE(buf)    (sizeof(buf) - 1)

#define ISFROM(p) ((p)[0] == 'F' && (p)[1] == 'r' && (p)[2] == 'o' \
		&& (p)[3] == 'm' && (p)[4] == ' ')

/* How to declare functions that don't return
   and functions that are like printf.  */

#ifdef __GNUC__
# if __GNUC__ < 2
#  define NORETURN	volatile
#  define ATTRIBUTE(x)	/**/
# else
#  define NORETURN	/**/
#  define ATTRIBUTE(x)	__attribute__(x)
# endif
#else  /* not GCC */
# define NORETURN	/**/
# define ATTRIBUTE(x)	/**/
#endif /* GCC */

/*
 * External data.
 */

/* Variables set by getopt() [blech] */

extern int optind, opterr;
extern char *optarg;

/*
 * Library functions.
 */

extern char *malloc();
extern char *realloc();
extern void free();

/*
 * Global data
 */

int field_count = 0;
int field_alloc = 0;
char **field_names = NULL;

int nocasematch = TRUE;		/* ignore case in header matches */
int printnames = FALSE;		/* print field names with data */
int except = FALSE;		/* reverse sense of -f */

/*
 * Global functions.
 */

void usage();
void header();

NORETURN void nomem() ATTRIBUTE((noreturn));

/*----------------------------------------------------------------------
 * The Program.
 */

int
main(argc, argv)
int argc;
char **argv;
{
    int c, errors;

    field_alloc = 8;
    field_names = (char **) malloc(field_alloc * sizeof(char **));
    if (field_names == NULL)
	nomem();

    errors = FALSE;
    while ((c = getopt(argc, argv, "cnvf:")) != EOF)
    {
	switch (c)
	{
	case 'c':
	    nocasematch = FALSE;
	    break;
	case 'n':
	    printnames = TRUE;
	    break;
	case 'v':
	    except = TRUE;
	    printnames = TRUE;
	    break;
	case 'f':
	    if (field_count >= field_alloc)
	    {
		field_alloc *= 2;
		field_names =
		    (char **) realloc((char *) field_names,
				      field_alloc * sizeof(char **));
		if (field_names == NULL)
		    nomem();
	    }
	    field_names[field_count++] = optarg;
	    break;
	default:
	    errors = TRUE;
	    break;
	}
    }

    if (errors)
	usage();

    if (optind == argc)
	header(stdin, (char *) NULL);
    else
    {
	FILE *fp;
	int a, filenames;

	filenames = ((argc - optind) > 1);
	for (a = optind; a < argc; ++a)
	{
	    if ((fp = fopen(argv[a], "r")) == NULL)
	    {
		errors = TRUE;
		perror(argv[a]);
		continue;
	    }

	    header(fp, (filenames ? argv[a] : (char *) NULL));
	    (void) fclose(fp);
	}
    }

    exit(errors ? 1 : 0);
    /* NOTREACHED */
}

void
usage()
{
    (void) fprintf(stderr,
		 "usage: header [-c] [-n] [-v] [-f fieldname] ... files\n");
    exit(1);
}

NORETURN void
nomem()
{
    (void) fprintf(stderr, "header: out of memory\n");
    exit(1);
}

void
header(fp, filename)
FILE *fp;
char *filename;
{
    char buf[1024];

    if (fgets(buf, GETSIZE(buf), fp) == NULL)
	return;

    /* Ignore From_ line(s). */

    while (ISFROM(buf) || buf[0] == '>')
    {
	if (fgets(buf, GETSIZE(buf), fp) == NULL)
	    return;
    }

    while (buf[0] != '\n')
    {
	char *p;
	int print_this;

	p = buf;
	while (*p
	       && !isspace(*p & 0xFF)
	       && !iscntrl(*p & 0xFF)
	       && *p != ':')
	    ++p;
	if (p == buf || *p != ':')
	    break;
	print_this = field(buf, p - buf);
	if (except)
	    print_this = !print_this;
	if (print_this)
	{
	    if (filename)
	    {
		(void) fputs(filename, stdout);
		(void) fputc(':', stdout);
	    }
	    ++p;
	    if (*p == ' ' || *p == '\t')
		++p;
	    if (field_count == 0 || printnames)
		(void) fputs(buf, stdout);
	    else
		(void) fputs(p, stdout);
	}

	/* get the next input line */
	if (fgets(buf, GETSIZE(buf), fp) == NULL)
	    break;

	/* deal with continuation lines */
	while (buf[0] == ' ' || buf[0] == '\t')
	{
	    if (print_this)
	    {
		if (filename)
		{
		    (void) fputs(filename, stdout);
		    (void) fputc(':', stdout);
		}
		(void) fputs(buf, stdout);
	    }

	    if (fgets(buf, GETSIZE(buf), fp) == NULL)
	    {
		buf[0] = '\n';
		break;
	    }
	}
    }
}

int
field(s, n)
char *s;
int n;
{
    int i;

    if (field_count == 0)
	return TRUE;

    for (i = 0; i < field_count; ++i)
    {
	char *f = field_names[i];

	if (strlen(f) == n)
	{
	    if (nocasematch)
	    {
		if (ci_strncmp(f, s, n) == 0)
		    return TRUE;
	    }
	    else
	    {
		if (strncmp(f, s, n) == 0)
		    return TRUE;
	    }
	}
    }

    return FALSE;
}

int
ci_strncmp(s, t, n)
char *s, *t;
int n;
{
    char c, d;

    while (n-- > 0)
    {
	c = *s++ & 0xFF;
	d = *t++ & 0xFF;
	if ((c == 0) && (d == 0))
	    break;
	if (isupper(c))
	    c = tolower(c);
	if (isupper(d))
	    d = tolower(d);
	if (c > d)
	    return 1;
	if (c < d)
	    return -1;
    }

    return 0;
}
