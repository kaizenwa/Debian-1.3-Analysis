/* @(#) gleem.c,v 1.4 1992/07/11 11:39:57 tron Exp */
/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * gleem - gleem useful map info from standard input
 *
 * usage:  
 *	gleem [-n name] [-c] [-f [-d dir]] [-p] [-s] [-F] file ...
 *
 *	-n name		name mode, convert to:  name { ... } form, 
 *			    imples -c, ignores -s
 *	-c		strip most comments
 *	-f		print:  file { filename }  before reading each file
 *	-d dir		add dir to filenames not starting with '/'
 *	-p		print:  private {}  after reading each file
 *	-s		convert delete & adjust directives to whitespace + error
 *	-F		convert file directives to spaces + error
 *	file ...	files to read, - means stdin & -f shows [stdin]
 *
 *
 * If name mode:  (-n name)
 *
 *	Since -c is implied,  comments and blank lines are discarded.  
 *	If the entire line is discarded, no further processing is done.
 *
 *	Next, the <line> is processed as follows:  The whitespace 
 *	within ()'s is removed.  Whitespace between a token and a 
 *	'(' is removed.  All tokens are separated by commas and all 
 *	whitespace is removed.  The final comma and newline, if
 *	any, is removed.
 *
 * 	Last, the string:  "name {<line>}" is printed.
 *
 * In not name mode: (no -n)
 *
 *	Print the contents of each file, striping comments if -c was given.
 *
 * A comment begins with a # and ends with a newline.  A token is a set of
 * chars that does not include whitespace, commas and ()'s.  A whitespace
 * char is a tab newline.  Blank lines are lines containing zero or more
 * whitespace chars.
 *
 * Note: Comment lines are not completely stripped.  Doing so would alter the
 *       line counts and throw off error reporting.  Also pathalias allows
 *	 comments to exist in the middle of a line.  If a comment is found
 *	 in the middle of the line, it is replaced by a single '#'.  If
 *	 the single '#' would start the line, a space is placed in before
 *	 the single '#'.
 */

#include <stdio.h>
#include "defs.h"

#define BUFFER_SIZE 4096		/* length of longest line */

void pack();				/* pack a line in second stage */
void strip_com();			/* strip comments from a string */
void cat_file();			/* cat a file in a special way */
void usage();				/* print a usage message and exit */
void safemap();				/* perform safemap striping */
void file_2_space();			/* convert file {...} to spaces */
char *program;				/* our name */
int errors = 0;				/* 1 ==> errors were encountered */

main(argc, argv)
    int argc;				/* arg count */
    char *argv[];			/* args */
{
    int c;				/* operand */
    int file_to_space = 0;		/* 1 ==> convert file to spaces */
    int strip_comments = 0;		/* 1 ==> strip input comments */
    int safemap_mode = 0;		/* 1 ==> remove delete, adjust lines */
    int print_private = 0;		/* 1 ==> print private {} */
    int print_filename = 0;		/* 1 ==> print file { filename } */
    char *dirname = NULL;		/* dirname for non / based files */
    char *name = NULL;			/* non-NULL ==> name mode */
    char *filename;			/* the filename we are reading */
    extern int optind;			/* operand index */
    extern char *optarg;		/* operand argument */

    /*
     * parse args
     */
    program = argv[0];
    while ((c = getopt(argc, argv, "n:cfd:psF")) != EOF) {
	switch (c) {
	case 'n':		/* token name, token mode */
	    name = optarg;
	    break;
	case 'c':		/* strip comments */
	    strip_comments = 1;
	    break;
	case 'f':		/* print leading file { filename } names */
	    print_filename = 1;
	    break;
	case 'd':		/* directory base for non / based files */
	    dirname = optarg;
	    break;
	case 'p':		/* print trailing private {} */
	    print_private = 1;
	    break;
	case 's':		/* strip delete and adjust directives */
	    safemap_mode = 1;
	    break;
	case 'F':		/* convert file directives to spaces */
	    file_to_space = 1;
	    break;
	default:
	    usage();
	    break;
	}
    }
    /* be sure we have filenames */
    if (optind >= argc) {
	usage();
    }
    /* name mode -n imples -c, disables -s */
    if ( name != NULL ) {
	strip_comments = 1;		/* -c implied */
	safemap_mode = 0;		/* -s disabled */
    }

    /*
     * process each file
     */
    for (filename=argv[optind]; optind < argc; ++optind,filename=argv[optind]) {

	/*
	 * if -f, print a file directive
	 */
	if ( print_filename ) {

	    /* catch the stdin case */
	    if (strcmp(filename, "-") == 0) {
		puts("file {[stdin]}");

	    /* catch the /-root based filename, or if we have no dirname */
	    } else if (filename[0] == '/' || dirname == NULL) {
		printf("file {%s}\n", filename);

	    /* non /-root based file, append dirname */
	    } else {
		printf("file {%s/%s}\n", dirname, filename);
	    }
	}

	/*
	 * proces the file
	 */
	cat_file(filename, name, strip_comments, safemap_mode, file_to_space);

	/*
	 * print a private directive if needed
	 */
	if ( print_private ) {
	    puts("private {}");
	}
    }

    /*
     * exit noting if errors happened
     */
    exit(errors);
}

/*
 * pack - pack a line for name mode
 *
 *     The whitespace within ()'s is removed.  Whitespace between a token
 *     and a '(' is removed.  All tokens are separated by commas and
 *     all whitespace is removed.  The final comma and newline, if
 *     any, is removed.
 */
void
pack( line )
    char *line;			/* the line to pack */
{
    int in_paren;		/* 1 ==> inside ()'s */
    int space;			/* 1 ==> last char was a space */
    int comma;			/* 1 ==> last char was a comma */
    char *p;			/* pointer */
    char *q;			/* pointer */
    char buf[BUFFER_SIZE+3];	/* I/O buffer */
    char *malloc();		/* stroage allocator */

    /*
     * remove whitespace within ()'s
     */
    q = line;		/* from */
    p = buf;		/* to */
    in_paren = 0;	/* not inside ()'s */
    while ( *q ) {
	/* detect open and close ()'s */
	switch (*q) {
	case '(':
	    in_paren = 1;
	    *p++ = *q++;
	    break;
	case ')':
	    in_paren = 0;
	    *p++ = *q++;
	    break;
	case ' ':
	case '\t':
	    if (in_paren == 0) {
		*p++ = *q++;
	    } else {
		++q;	/* skip whitespace insize a () */
	    }
	    break;
	case '\n':
	    ++q;	/* skip newline */
	    break;
	default:
	    *p++ = *q++;
	    break;
	}
    }
    *p = '\0';

    /*
     * remove leading whitespace, convert multiple spaces into a
     * comma, except when before a ( or a comma
     */
    q = buf;		/* from */
    p = line;		/* to */
    space = 0;		/* have not seen spaces */
    comma = 0;		/* have not seen commas */
    while (*q == ' ' || *q == '\t') {	  /* skip leading whitespace */
	++q;
    }
    while ( *q ) {
	switch (*q) {
	case ' ':
	case '\t':
	    ++q;		/* skip whitespace until later, maybe */
	    space = 1;
	    break;
	case '(':
	    *p++ = *q++;	/* no comma before ( or comma */
	    space = 0;
	    comma = 0;
	    break;
	case ',':
	    if (comma == 0) {
		*p++ = *q++;	/* copy only one comma */
		comma = 1;
	    } else {
		++q;		/* skip multiple commas */
	    }
	    break;
	default:
	    if (space == 1 && comma == 0) {
		*p++ = ',';	/* replace multi whitespace with a comma */
	    }
	    comma = 0;
	    *p++ = *q++;
	    space = 0;
	    break;
	}
    }

    /*
     * remove any trailing commas
     */
    while (--p > line && *p == ',') {
    }
    if (*p == ',') {
	*p = '\0';
    } else {
	*(p+1) = '\0';
    }
}

/*
 * strip_com - perform -c comment striping
 *
 * This routine removes most comments and all trailing whitespace from 
 * the string passed to it.  The trailing newline is always removed.
 *
 * A comment begins with a '#' and ends with a newline. A whitespace
 * char is a tab newline.  Blank lines are lines containing zero or 
 * more whitespace chars.
 *
 * Note: Comment lines are not completely stripped.  Doing so would alter the
 *       line counts and throw off error reporting.  Also pathalias allows
 *	 comments to exist in the middle of a line.  If a comment is found
 *	 in the middle of the line, it is replaced by a single '#'.  If
 *	 the single '#' would start the line, a space is placed in before
 *	 the single '#'.
 */
void
strip_com( buf )
    char *buf;			/* string to strip */
{
    register char *p;		/* index */
    int found_comment=0;	/* 1 => we removed a #comment */

    /*
     * remove a comment
     */
    p = index(buf, '#');
    if (p != NULL) {

	/* strip */
	*p = '\0';	

	/* if entire lne was a comment, remove it all and return */
	if (p == buf) {
	    return;
	}

	/* note mid line comment found */
	found_comment = 1;
    }

    /*
     * scan over any trailing whitespace
     */
    p = buf + strlen(buf) - 1;
    while (p >= buf && (*p == ' ' || *p == '\t' || *p == '\n')) {
	--p;
    }

    /*
     * clear out any whitespace
     *
     * If we removed a comment, put the '#' back.
     */
    if (found_comment) {
	/* if the '#' would start a line, blank fill first */
	if (p < buf) {
	    *++p = ' ';
	}
	/* minimal comment */
	*++p = '#';	
    }
    *++p = '\0';	/* strip */
    return;
}

/*
 * cat_file - output a file
 *
 * opens a filename, writes it contents to stadard output.  Strips comments, 
 * removes problem lines and pack for name mode if needed.  Problems lines
 * are lines that begin with the token "delete" or "adjust" or "file".
 */
void
cat_file( filename, name, strip_comments, safemap_mode, file_to_space )
    char *filename;		/* the name of the file to output */
    char *name;			/* non-NULL ==> name mode */
    int strip_comments;		/* 1 ==> strip input comments */
    int safemap_mode;		/* 1 ==> remove delete, adjust lines */
    int file_to_space;		/* 1 ==> convert file to spaces */
{
    int linenum;		/* current line number */
    char buf[BUFFER_SIZE+3];	/* I/O buffer */
    FILE *stream;		/* file stream to read */

    /*
     * try to open the file
     */
    if (strcmp(filename, "-") == 0) {
	stream = stdin;		/* - ==> read from stdin */
	filename = "[stdin]";
    } else {
	stream = fopen(filename, "r");
    }
    if (stream == NULL) {
	/* unable to open, object and return */
	fprintf(stderr, "%s: unable to open file: %s\n", program, filename);
	errors = 1;
	return;
    }

    /*
     * process each line
     *
     * We allow lines to be up to BUFFER_SIZE chars long.  But we
     * read BUFFER_SIZE+1 chars (i.e., fgets value of BUFFER_SZIE+2)
     * chars to detect a longer line.  If the line were BUFFER_SIZE+1
     * chars, then the BUFFER_SIZE+1-th char would not be '\0'.
     */
    clearerr(stream);	/* no errors yet */
    linenum = 0;	/* no lines read yet */
    buf[BUFFER_SIZE] = '\0';	/* used to detect line long lines */
    while (fgets(buf, BUFFER_SIZE+2, stream) != NULL) {

	/*
	 * count the line
	 */
	++linenum;

	/*
	 * firewall - see if the line is too long
	 */
	if (buf[BUFFER_SIZE] != '\0') {
	    fprintf(stderr, "%s: file: %s line: %d longer than %d chars\n",
		program, filename, linenum, BUFFER_SIZE);
	    errors = 1;
	    break;
	}

	/*
	 * strip comments if needed
	 */
	if (name != NULL || strip_comments) {
	    strip_com(buf);
	}

	/*
	 * convert file directives to spaces if needed
	 */
	if (file_to_space) {
	    file_2_space(buf);
	}

	/*
	 * pack the line, if name mode
	 */
	if (name != NULL) {
	    pack(buf);
	}

	/*
	 * perform safemap processing if needed
	 */
	if (safemap_mode && name == NULL) {
	    safemap(filename, linenum, buf);
	}

	/*
	 * print the newly formed line
	 */
	if (name != NULL) {
	    /* output in name mode style */
	    if (buf[0] != '\0') {
		printf("%s {%s}\n", name, buf);
	    }
	} else {
	    /* output line, being sure to write a newline */
	    if ( strip_comments ) {
		puts(buf);		/* replace the stripped newline */
	    } else {
		fputs(buf, stdout);	/* newline is already there */
	    }
        }
    }

    /*
     * examine stop reason
     */
    if ( ferror(stream) ) {
	fprintf(stderr, "%s: error in reading file: %s line: %d\n", 
	    program, filename, linenum);
	errors = 1;
    }

    /*
     * flush & close if not stdin
     */
    fflush(stdout);
    if (stream != stdin) {
	(void) fclose(stream);
    }
}

/*
 * usage - print usage message and exit
 */
void
usage()
{
    fprintf(stderr, 
	"usage: %s [-n name] [-f [-d dir]] [-c] [-p] [-s] file ...\n",
	program);
    exit(2);
}

/*
 * safemap - avoid all delete, adjust and file directives
 *
 * Any lines of the form:
 *
 *	^delete[ \t]*{
 *	^adjust[ \t]*{
 *
 * are removed.  The 'delete' and 'adjust' result in errors.
 */
void
safemap( filename, linenum, buf )
    char *filename;		/* filename or [stdin] being read */
    int linenum;		/* current line number */
    char *buf;			/* the buffer to check for unsafe words */
{
    register char *p;		/* pointer */

    /*
     * rule out good lines quickly
     */
    switch (buf[0]) {
    case 'd':
    case 'a':
	break;
    default:
	return;		/* nothing unsafe about this line */
    }

    /* 
     * begins with an unsafe letter, is it really an unsafe word? 
     */
    if (strncmp(buf, "delete", 6) == 0) {

	/* leading chars are ok, but is if followed by space/tab? */
	for (p = buf+6; *p; ++p) {

	    /* look for start of {} group after word */
	    if (*p == '{' /*}*/) {
		/* we found end of word, start of {} group, unsafe! */
		buf[0] = '\0';
		fprintf(stderr,
		    "%s: bogus delete directive in file: %s line: %d\n",
		    program, filename, linenum);
		errors = 1;
		break;
	    }

	    /* must be between words then */
	    if (*p != ' ' && *p != '\t') {
		break;		/* not an unsafe word */
	    }
	}

    } else if (strncmp(buf, "adjust", 6) == 0) {

	/* leading chars are ok, but is if followed by space/tab? */
	for (p = buf+6; *p; ++p) {

	    /* look for start of {} group after word */
	    if (*p == '{' /*}*/) {
		/* we found end of word, start of {} group, unsafe! */
		buf[0] = '\0';
		fprintf(stderr,
		    "%s: bogus adjust directive in file: %s line: %d\n",
		    program, filename, linenum);
		errors = 1;
		break;
	    }

	    /* must be between words then */
	    if (*p != ' ' && *p != '\t') {
		break;		/* not an unsafe word */
	    }
	}
    }
    return;
}

/*
 * file_2_space - convert file {...} lines to spaces
 *
 * This allows one to strip file directives from uuwho input without changing
 * the position within the file.
 */
void
file_2_space( buf )
    char *buf;			/* the string to examine */
{
    char *p;			/* pointer */

    /*
     * look for a file directive
     */
    if (strncmp(buf, "file", 4) == 0) {

	/* leading chars are ok, but is if followed by space/tab? */
	for (p = buf+4; *p; ++p) {

	    /* look for start of {} group after word */
	    if (*p == '{' /*}*/) {

		/*
		 * convert a file directive into spaces
		 */
		for (p=buf; *p && *p != '\n'; ++p) {
		    *p = ' ';
		}
		return;
	    }
	    
	    /* must be between words then */
	    if (*p != ' ' && *p != '\t') {
		break;		/* not an unsafe word */
	    }
	}
    }
    return;
}
