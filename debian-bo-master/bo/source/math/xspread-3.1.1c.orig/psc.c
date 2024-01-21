/* Sc parse routine
 *
 * usage psc options
 * options:
 *   -C n	Increment by n between columns
 *   -d c	Use c as the delimiter between the fields.
 *   -f		suppress 'format' lines in output
 *   -h		Print this help
 *   -k		Keep all delimiters - Default is strip multiple delimiters to 1.
 *   -L		Left justify strings.  Default is right justify.
 *   -n n	Length of the row (column) should be n.
 *   -P		Use numbers only when there is no [-+eE] (plain numbers only)
 *   -r		Assemble data into rows first, not columns.
 *   -R	n	Increment by n between rows 
 *   -S		Use strings vs numbers for numbers
 *   -s v	Top left location in the spreadsheet should be v; eg, k5
 *   -v		Print version
 *
 *  Author: Robert Bond
 *  Adjustments: Jeff Buhrt and Eric Putz
 */
char *rev = "$Revision: 6.21 A $";

#include <config.h>
#include <ctype.h>
#include <stdio.h>
#include "sc.h"

#define END	0
#define NUM	1
#define ALPHA	2
#define SPACE	3
#define EOL	4

extern char *optarg;
extern int   optind, getopt();

char	*coltoa PROTO((int));
int	getrow PROTO((char *));
int	getcol PROTO((char *));
void	print_help PROTO((void));
int	scan PROTO((void));

char	*progname;


/*#if defined(SYSV3) || defined(SUNOS4) || defined (__FreeBSD__)
extern void exit();
#else
extern int exit();
#endif
*/

int	*fwidth;
int	*precision;
int	maxcols;
int	*realfmt;

/* option flags reset */
int	colfirst = FALSE;
int	leftadj = FALSE;
int	r0 = 0;
int	c0 = 0;
int	rinc = 1;
int	cinc = 1;
int	len = 20000;
char	delim1 = ' ';
char	delim2 = '\t';
int	strip_delim = TRUE;
int	drop_format = FALSE;
int	strnums	= FALSE;
int	plainnums	= FALSE;

char token[1000];

void main(argc, argv)
int argc;
char **argv;
{
    int curlen;
    int cur_col, coff;
    int cur_row, roff;
    int first;
    int c;
    register effr, effc;
    int i,j;
    register char *p;

    progname = argv[0];
    while ((c = getopt(argc, argv, "rfLks:R:C:n:d:SPv")) != EOF) {
	switch(c) {
	case 'r':
	    colfirst = TRUE;
	    break;
	case 'L':
	    leftadj = TRUE;
	    break;
	case 's':
	    c0 = getcol(optarg);
	    r0 = getrow(optarg);
	    break;
	case 'R':
	    rinc = atoi(optarg);
	    break;
	case 'C':
	    cinc = atoi(optarg);
	    break;
	case 'n':
	    len = atoi(optarg);
	    break;
	case 'd':
	    delim1 = optarg[0];
	    delim2 = '\0';
	    break;
	case 'k':
	    strip_delim = FALSE;
	    break;
	case 'f':
	    drop_format = TRUE;
	    break;
	case 'S':
	    strnums = TRUE;
	    break;
	case 'P':
	    plainnums = TRUE;
	    break;
	case 'v':
	    (void) fprintf(stderr,"%s:\n %s\n", progname, version);
	case 'h':
	default:
	    print_help();
	    exit(1);
        }
    }

    if (optind < argc) {
	    print_help();
	    exit(1);
    }

	/* setup the spreadsheet arrays */
    if (!growtbl(GROWNEW, 0, 0))
	exit(1);

    curlen = 0;
    cur_col = c0; coff = 0;
    cur_row = r0; roff = 0;
    first = TRUE;

    while(TRUE) {

	effr = cur_row+roff;
	effc = cur_col+coff;

	switch(scan()) {
	case END:
	    if(drop_format) exit(0);
	    for (i = 0; i<maxcols; i++) {
		if (fwidth[i])
		    (void) printf("format %s %d %d\n", coltoa(i), 
			fwidth[i]+1, precision[i]);
	    }
	    exit(0);
	case NUM:
	    first = FALSE;
	    (void) printf("let %s%d = %s\n", coltoa(effc), effr, token);
	    if (effc >= maxcols - 1)
	    {	if (!growtbl(GROWCOL, 0, effc))
		{	(void) fprintf(stderr, "Invalid column used: %s\n", coltoa(effc));
			continue;
		}
	    }
	    i = 0;
	    j = 0;
	    p = token;
	    while (*p && *p != '.') {
		p++; i++;
	    }
	    if (*p) {
		p++; i++;
	    }
	    while (*p) {
		p++; i++; j++;
	    }
	    {   int	ow, nw;

		ow = fwidth[effc] - precision[effc];
		if (precision[effc] < j)
			precision[effc] = j;
	
		if (fwidth[effc] < i)
			fwidth[effc] = i;

		/* now make sure:
		 *	1234.567890 (format 11 6)
		 *	1234567.890 (format 11 3)
		 *	both show (format 14 6)
		 *		(really it uses 15 6 to separate columns)
		 */
		if ((nw = i - j) > ow)
			fwidth[effc] += nw - (fwidth[effc] - precision[effc]);
	    }
	    break;
	case ALPHA:
	    first = FALSE;
	    if (leftadj)
		(void) printf("leftstring %s%d = \"%s\"\n", coltoa(effc),effr,token); 
	    else
		(void) printf("rightstring %s%d = \"%s\"\n",coltoa(effc),effr,token); 
	    if (effc >= maxcols - 1)
	    {	if (!growtbl(GROWCOL, 0, effc))
		{	(void) fprintf(stderr, "Invalid column used: %s\n", coltoa(effc));
			continue;
		}
	    }
	    i = strlen(token);
	    if (i > fwidth[effc])
		fwidth[effc] = i;
	    break;
	case SPACE:
	    if (first && strip_delim)
		break;
	    if (colfirst)
		roff++;
	    else
		coff++;
	    break;
	case EOL:
	    curlen++;
	    roff = 0;
	    coff = 0;
	    first = TRUE;
	    if (colfirst) {
		if (curlen >= len) {
		    cur_col = c0;
		    cur_row += rinc;
		    curlen = 0;
		} else {
		    cur_col += cinc;
		}
	    } else {
		if (curlen >= len) {
		    cur_row = r0;
		    cur_col += cinc;
		    curlen = 0;
		} else {
		    cur_row += rinc;
		}
	    }
	    break;
	}
    }
}

void
print_help()
{	fprintf(stdout,
"Usage: %s [-fhkLPrSv] [-s v] [-R i] [-C i] [-n i] [-d c]\n", progname);
puts("   Flag    Description                             Default");
puts("   -C #  Increment by n between columns            1");
puts("   -d c  Use c as the delimiter between the fields. space & tab");
puts("   -f    suppress 'format' lines in output         false");
puts("   -h    Print this help");
puts("   -k    Keep all delimiters - Default is strip multiple delimiters to 1.");
puts("   -L    Left justify strings.                     right justify");
puts("   -n #  Length of the row (column) should be #    20000");
puts("   -P    Returns 'numbers' with [-+eE] in them as strings  false");
puts("   -r    Assemble data into rows first             columns");
puts("   -R #  Increment by # between rows               1");
puts("   -S    Return numbers as strings                 numbers");
puts("   -s [col][row]   New top left starting cell      a0");
puts("   -v    Print version");
}

int
scan()
{
    register int c;
    register char *p;
    register int founddigit;
#ifndef __STDC__
    extern int	ungetc();
#endif

    p = token;
    c = getchar();

    if (c == EOF)
	return(END);

    if (c == '\n')
	return(EOL);

    if (c == delim1 || c == delim2) {
	if (strip_delim) {
	    while ((c = getchar()) && (c == delim1 || c == delim2))
	        ;
	    (void)ungetc(c, stdin);
	} 
	return(SPACE);
    }

    if (c == '\"') {
	while ((c = getchar()) && c != '\"' && c != '\n' && c != EOF)
	    *p++ = c;
	if (c != '\"')
	    (void)ungetc(c, stdin);
	*p = '\0';
	return(ALPHA);
    }

    while (c != delim1 && c != delim2 && c!= '\n' && c != EOF) {
	*p++ = c;
	c = getchar();
    }
    *p = '\0';
    (void)ungetc(c, stdin);

	/* we now have a token (value/string), classify it NUM/ALPHA */
    p = token;
    c = *p;
    founddigit = FALSE;
    /*
     * str_nums always returns numbers as strings
     * plainnums returns 'numbers' with [-+eE] in them as strings
     * lastprtnum makes sure a number ends in one of [0-9eE.]
     */
    if (!strnums && (isdigit(c) || c == '.' || c == '-' || c == '+')) {
	int	lastprtnum = FALSE;

	while(isdigit(c) || c == '.' || (!plainnums && (c == '-' ||
					c == '+' || c == 'e' || c == 'E'))) {
		if (isdigit(c)) 
			lastprtnum = founddigit = TRUE;
		else
		if (!(c == '.' || c == 'e' || c == 'E'))
			lastprtnum = FALSE;
		c = *p++;
	}
	if (c == '\0' && founddigit && lastprtnum)
	    return(NUM);
	else
	    return(ALPHA);
    }

    return(ALPHA);
}
    
/* turns [A-Z][A-Z] into a number */
int
getcol(p)
char *p;
{
    register  col;

    col = 0;
    if (!p)
	return(0);
    while(*p && !isalpha(*p)) 
	p++; 
    if (!*p)
	return(0);
    col = ((*p & 0137) - 'A');
    if (isalpha(*++p)) 
	col = (col + 1)*26 + ((*p & 0137) - 'A');
    return(col);
}

/* given a string turn it into a row number */
int
getrow(p)
char *p;
{
    int row;

    row = 0;
    if (!p)
	return(0);
    while(*p && !isdigit(*p))
	p++; 
    if (!*p)
	return(0);
    while(*p && isdigit(*p))
    {	row = row * 10 + *p - '0';
	p++;
    }
    return(row);
}

/* turns a column number into [A-Z][A-Z] */
char *
coltoa(col)
int col;
{
    static char rname[3];
    register char *p = rname;

    if (col < 0 || col > 27*26)	/* A-Z, AA-ZZ */
	(void) fprintf(stderr,"coltoa: invalid col: %d", col);

    if (col > 25) {
	*p++ = col/26 + 'A' - 1;
	col %= 26;
    }
    *p++ = col+'A';
    *p = '\0';
    return(rname);
}
