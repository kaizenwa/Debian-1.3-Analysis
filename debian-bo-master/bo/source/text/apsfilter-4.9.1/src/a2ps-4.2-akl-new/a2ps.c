/************************************************************************/
/*									*/
/* Description: Ascii to PostScript printer program.			*/
/* File: imag:/users/local/src/a2ps/a2ps.c				*/
/* Created: Mon Dec 21 18:51:15 1992 by santana@imag.fr (Miguel Santana)*/
/* Version: 4.2								*/
/*									*/
/* Edit history:							*/
/* 1) Derived of shell program written by evan@csli (Evan Kirshenbaum).	*/
/*    Written in C for improve speed execution and portability. Many	*/
/*    improvements have been added.					*/
/* Fixes by Oscar Nierstrasz @ cui.uucp:				*/
/* 2) Fixed incorrect handling of stdin (removed error if no file names)*/
/* 3) Added start_page variable to eliminate blank pages printed for	*/
/*	files that are exactly multiples of 132 lines (e.g., man pages)	*/
/* Modified by santana@imag.fr:						*/
/* 4) Added new options at installation : sheet format (height/width in	*/
/*    inches), page format (number of columns per line and of lines per	*/
/*    page).								*/
/* Modified by santana@imag.fr:						*/
/* 5) Added new option to print n copies of a same document.		*/
/* 6) Cut long filenames if don't fit in the page header.		*/
/* Modified by Tim Clark (T.Clark@warwick.ac.uk):			*/
/* 7) Two additional modes of printing (portrait and wide format modes)	*/
/* 8) Fixed to cope with filenames which contain a character which must	*/
/*    be escaped in a PostScript string.				*/
/* Modified by santana@imag.fr to					*/
/* 9) Added new option to suppress heading printing.			*/
/* 10) Added new option to suppress page surrounding border printing.	*/
/* 11) Added new option to change font size. Lines and columns are	*/
/*     automatically adjusted, depending on font size and printing mode	*/
/* 12) Minor changes (best layout, usage message, etc).			*/
/* Modified by tullemans@apolloway.prl.philips.nl			*/
/* 13) Backspaces (^H) are now handled correctly.			*/
/* Modified by Johan Vromans (jv@mh.nl) to				*/
/* 14) Added new option to give a header title that replaces use of	*/
/*     filename.							*/
/* Modified by craig.r.stevenson@att.com to				*/
/* 15) Print last modification date/time in header                      */
/* 16) Printing current date/time on left side of footer (optional)	*/
/* Modified by erikt@cs.umu.se:						*/
/* 17) Added lpr support for the BSD version				*/
/* 18) Added som output of pages printed.				*/
/* Modified by wstahw@lso.win.tue.nl:					*/
/* 19) Added option to allowing the printing of 2 files in one sheet	*/
/* Modified by mai@wolfen.cc.uow.oz					*/
/* 20) Added an option to set the lines per page to a specified value.	*/
/* 21) Added support for printing nroff manuals				*/
/* Modified by santana@imag.fr						*/
/* 22) Integration of changes.						*/
/* 23) No more standard header file (printed directly by a2ps).		*/
/* 24) New format for command options.					*/
/* 25) Other minor changes.						*/
/* Modified by Johan Garpendahl (garp@isy.liu.se) and santana@imag.fr:	*/
/* 26) Added 8-bit characters printing as ISO-latin 1 chars		*/
/* Modified by John Interrante (interran@uluru.stanford.edu) and	*/
/* santana@imag.fr:							*/
/* 27) Two pages per physical page in portrait mode			*/
/* Modified by santana@imag.fr:						*/
/* 28) New option for two-sided printing				*/
/* 29) Several fixes							*/
/* Modified by Chris Adamo (adamo@ll.mit.edu) and			*/
/*     Larry Barbieri (lbarbieri@ll.mit.edu) 3/12/93			*/
/* 30) Output format enhancements.					*/
/* 31) Added login_id flag (for SYSV and BSD only) for printing user's	*/
/*     login ID at top of page.  Added command line parameter (-nL) to	*/
/*     suppress this feature.						*/
/* 33) Added filename_footer flag for printing file name at bottom	*/
/*     of page.  Added command line parameter (-nu) to suppress this	*/
/*     feature.								*/
/* 34) Added -B (-nB) options to enable (disable) bold font		*/
/*									*/
/************************************************************************/

/*
 * Copyright (c) 1992, 1993, Miguel Santana, santana@imag.fr
 *
 * Permission is granted to copy and distribute this file, for noncommercial
 * use, provided (a) this copyright notice is preserved, (b) no attempt
 * is made to restrict redistribution of this file, and (c) this file is
 * not distributed as part of any collection whose redistribution is
 * restricted by a compilation copyright.
 */


/************************************************************************/
/*                                                                      */
/*			I n c l u d e   f i l e s			*/
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef __STDC__
#include <stdlib.h>
#include <time.h>
#include <string.h>
#else
#include <strings.h>
#ifdef SYSV
#include <sys/timeb.h>
#include <time.h>
#else
#ifndef BSD
#define BSD	1
#endif
#include <sys/time.h>
#endif
#endif


/************************************************************************/
/*									*/
/*	     P r e p r o c e s s i n g   d e f i n i t i o n s		*/
/*									*/
/************************************************************************/

/*
 * Common definitions
 */
#define	FALSE		0
#define	TRUE		1
#ifndef NULL
#define NULL		0
#endif
#ifndef NUL
#define NUL		'\0'
#endif
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS	0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE	1
#endif


/*
 * Version
 */
#define VERSION	"4.2"


/*
 * Default page dimensions
 */

#ifndef WIDTH
#define	WIDTH	8.27
#endif

#ifndef HEIGHT
#define	HEIGHT	11.64
#endif

#ifndef MARGIN
#define	MARGIN	1.4
#endif


/*
 * Pathname separator for file system
 */
#ifndef DIR_SEP
#define	DIR_SEP	'/'
#endif


/*
 * Printing parameters
 */
#ifndef LPR_PRINT
#define LPR_PRINT	TRUE
#endif

#if LPR_PRINT

#ifndef LPR_COMMAND
#define LPR_COMMAND	"lpr"
#endif

#ifndef LPR_OPT
#define	LPR_OPT	"-l"
#endif

#if defined(ONESIDED) && defined(TWOSIDED)
#define RECTO_VERSO_PRINTING
#ifndef TWOSIDED_DFLT
#define TWOSIDED_DFLT	TRUE
#endif
#endif

#endif
    

/*
 * Configuration values
 */
#define	PORTRAIT_HEADER		0.29
#define	LANDSCAPE_HEADER	0.22
#define	PIXELS_INCH		72
#define MAXFILENAME		32
#define MAX_LINES               160		/* max. lines per page */
#define MAN_LINES               66		/* no lines for a man */
#define IS_ROMAN		0		/* normal font */
#define IS_BOLD			1		/* bold sequence flag */
#if defined(SYSV) || defined(BSD)
#define MAX_HOSTNAME		40
#endif


/************************************************************************/
/*									*/
/*		   G l o b a l   d e f i n i t i o n s			*/
/*									*/
/************************************************************************/


/*
 * Global types
 */
typedef enum { BOLD, NORMAL } WEIGHT;		/* font weights */


/*
 * Function declarations.
 */
#ifdef __STDC__
	/* Function prototypes */

#ifdef AKL
void set_paper_format(char *arg);
#endif

void usage(int failure);
void set_global_option(char *arg);
void set_positional_option(char *arg);
int mygetc(int *statusp);
int is_binaryfile(char *name);
void cut_filename(char *old_name, char *new_name);
int printchar(unsigned char c);
void skip_page(void);
int fold_line(char *name);
int cut_line(void);
void printpage(void);
void startpage(void);
void cleanup(void);
void endpage(void);
void print_file_prologue(char *name, char *title);
void print_file(char *name, char *header);
void print_prologue(void);
void print_standard_prologue(char *datestring);
int main(int argc, char *argv[]);
#if defined(SYSV) || defined(BSD)
char *getlogin(void);
int   gethostname(char *name, int namelen);
#endif

#else
	/* Only forward declarations */
int is_binaryfile();
void print_standard_prologue();
void startpage();
void endpage();
#if defined(SYSV) || defined(BSD)
char *getlogin();
int   gethostname();
#endif

#endif


/*
 * Flags related to options.
 */
int numbering = FALSE;		/* Line numbering option */
int folding = TRUE;		/* Line folding option */
int restart = FALSE;		/* Don't restart page number after each file */
int only_printable = FALSE;	/* Replace non printable char by space */
int interpret = TRUE;		/* Interpret TAB, FF and BS chars option */
int print_binaries = FALSE;	/* Force printing for binary files */ 
int landscape = TRUE;		/* Otherwise portrait format sheets */
int new_landscape = TRUE;	/* To scrute changes of landscape option */
int twinpages = TRUE;		/* 2 pages per sheet if true, 1 otherwise */
int new_twinpages = TRUE;	/* To scrute changes of twinpages option */
int twinfiles = FALSE;		/* Allow 2 files per sheet */
int no_header = FALSE;		/* TRUE if user doesn't want the header */
int no_border = FALSE;		/* Don't print the surrounding border ? */
int printdate = FALSE;		/* Print current date as footnote */
int filename_footer = TRUE;	/* Print file name at bottom of page */
WEIGHT fontweight = NORMAL;	/* Control font weight: BOLD or NORMAL */
WEIGHT new_fontweight = NORMAL;	/* To scrute changes of bold option */
#if defined(SYSV) || defined(BSD)
int login_id = TRUE;		/* Print login ID at top of page */
#endif	
#if LPR_PRINT
int lpr_print = LPR_PRINT;	/* Fork a lpr process to do the printing */
#ifdef RECTO_VERSO_PRINTING
int rectoverso = TWOSIDED_DFLT; /* Two-side printing */
#endif
#endif
int ISOlatin1 = FALSE;		/* Print 8-bit characters? */


/*
 * Counters of different kinds.
 */
int column = 0;			/* Column number (in current line) */
int line = 0;			/* Line number (in current page) */
int line_number = 0;		/* Source line number */
int pages = 0;			/* Number of logical pages printed */
int sheets = 0;			/* Number of physical pages printed */
int old_pages, old_sheets;	/* Value before printing current file */
int sheetside = 0;		/* Side of the sheet currently printing */
int linesperpage;		/* Lines per page */
int lines_requested = 0;	/* Lines per page requested by the user */
int new_linesrequest = 0;	/* To scrute new values for lines_requested */
int columnsperline;		/* Characters per output line */
int nonprinting_chars, chars;	/* Number of nonprinting and total chars */
int copies_number = 1;		/* Number of copies to print */
int column_width = 8;	        /* Default column tab width (8) */


/*
 * Other global variables.
 */
int first_page;			/* First page for a file */
int no_files = TRUE;		/* No file until now */
int prefix_width;		/* Width in characters for line prefix */
float fontsize = 0.0;		/* Size of a char for body font */
float new_fontsize = 0.0;	/* To scrute new values for fontsize */
char *command;			/* Name of a2ps program */
char *lpr_opt = NULL;		/* Options to lpr */
char *header_text = NULL;	/* Allow for different header text */
char *prologue = NULL;		/* postscript header file */
#if defined(SYSV) || defined(BSD)
char *login = NULL;		/* user's login name and host machine */
#ifdef AKL
char *logname, *host = NULL;	/* new commandline flags ... */
#endif
#endif


/*
 * Sheet dimensions
 */
#ifdef AKL
double page_height_inch = HEIGHT;	/* Paper height in inch */
double page_width_inch = WIDTH;		/* Paper width  in inch */
double page_height;			/* for Paper height in pixels */
double page_width;			/* for Paper width  in pixels */
double margin = MARGIN;
#else
double page_height = HEIGHT;	/* Paper height */
double page_width = WIDTH;	/* Paper width */
#endif


/************************************************************************/
/*									*/
/*									*/
/************************************************************************/

/*
 * Print a usage message.
 */
void
usage(failure)
    int failure;		/* Must we exit with a failure code? */
{
    fprintf(stderr,"A2ps v%s usage: %s [pos. or global options] [ f1 [ [pos. options] f2 ...] ]\n", VERSION, command);
    fprintf(stderr,"pos.   =  -#num\t\tnumber of copies to print\n");
    fprintf(stderr,"          -1\t\tone page per sheet\n");
    fprintf(stderr,"          -2\t\tTWIN PAGES per sheet\n");
    fprintf(stderr,"          -d\t-nd\tprint (DON'T PRINT) current date at the bottom\n");
    fprintf(stderr,"          -Fnum\t\tfont size, num is a float number\n");
    fprintf(stderr,"          -Hstr\t\tuse str like header title for subsequent files\n");
#if defined(SYSV) || defined(BSD)	
    fprintf(stderr,"          \t-nL\tdon't print login ID on top of page\n");
#endif	
    fprintf(stderr,"          -l\t\tprint in LANDSCAPE mode\n");
    fprintf(stderr,"          -lnum\t\tuse num lines per page\n");
    fprintf(stderr,"          -m\t\tprocess the file as a man\n");
    fprintf(stderr,"          -n\t-nn\tNUMBER (don't number) line files\n");
    fprintf(stderr,"          -p\t\tprint in portrait mode\n");
    fprintf(stderr,"          -s\t-ns\tPRINT (don't print) surrounding borders\n");
    fprintf(stderr, "\n");
    fprintf(stderr,"global =  -?\t\tprint this information\n");
    fprintf(stderr,"          -B\t-nB\tprint (DON'T PRINT) in bold font\n");
    fprintf(stderr,"          -b\t-nb\tforce (DON'T FORCE) binary printing\n");
    fprintf(stderr,"          -c\t-nc\tallow (DON'T ALLOW) two files on the same sheet\n");
    fprintf(stderr,"          -f\t-nf\tFOLD (don't fold) lines\n");
    fprintf(stderr,"          \t-nH\tdon't print any header\n");
    fprintf(stderr,"          -h\t\tprint this information\n");
    fprintf(stderr,"          -Ifile\tinclude this file as a2ps prologue\n");
    fprintf(stderr,"          -i\t-ni\tINTERPRET (don't interpret) tab, bs and ff chars\n");
#if LPR_PRINT
    fprintf(stderr,"          -Pprinter -nP\tSEND (don't send) directly to the printer");
#ifdef LPR_OPT
    if (LPR_OPT != NULL && sizeof(LPR_OPT) > 0)
	fprintf(stderr,"\n\t\t\t(with options '%s' and -Pprinter)", LPR_OPT);
#endif
    fprintf(stderr, "\n");
#endif
    fprintf(stderr,"          -r\t-nr\tRESTART (don't restart) page number after each file\n");
#ifdef RECTO_VERSO_PRINTING
#ifdef TWOSIDED_DFLT
    fprintf(stderr,"          -s1\t-s2\tone-sided (TWO-SIDED) printing\n");
#else
    fprintf(stderr,"          -s1\t-s2\tONE-SIDED (two-sided) printing\n");
#endif
#endif
    fprintf(stderr,"          -tnum\t\tset tab size to n\n");
    fprintf(stderr,"          \t-nu\tdon't print a filename footer\n");
    fprintf(stderr,"          -v\t-nv\tVISIBLE (blank) display of unprintable chars\n");
    fprintf(stderr,"          -8\t-n8\tdisplay (DON'T DISPLAY) 8-bit chars\n");
#ifdef AKL
    fprintf(stderr,"          -Xformat\tghostscripts page format = a0,a1,...,letter\n");
    fprintf(stderr,"          -Mmargin\tmargin size in inch, default: 1.4 inch\n");
#endif
    exit(failure);
}

#ifdef AKL

/*
 * AKL - set sheet dimensions using ghostscripts format names and sizes
 *	 see: GS_LIBDIR/gs_statd.ps, note: these values are given in 1/72 inch
 */

void
set_paper_format(arg)
char *arg;
{
	if (strstr(arg,"letter")) {
		page_width_inch = 8.50;		page_height_inch = 11.00;
	} else
	if (strstr(arg,"note")) {
		page_width_inch = 7.50;		page_height_inch = 10.00;
	} else
	if (strstr(arg,"legal")) {
		page_width_inch = 8.50;		page_height_inch = 14.00;
	} else
	if (strstr(arg,"a0")) {
		page_width_inch = 33.06;	page_height_inch = 46.78;
	} else
	if (strstr(arg,"a1")) {
		page_width_inch = 23.39;	page_height_inch = 33.06;
	} else
	if (strstr(arg,"a2")) {
		page_width_inch = 16.53;	page_height_inch = 23.39;
	} else
	if (strstr(arg,"a3")) {
		page_width_inch = 11.69;	page_height_inch = 16.53;
	} else
	if (strstr(arg,"a4")) {
		page_width_inch = 8.26;		page_height_inch = 11.69;
	} else
	if (strstr(arg,"a5")) {
		page_width_inch = 5.85;		page_height_inch = 8.26;
	} else
	if (strstr(arg,"a6")) {
		page_width_inch = 4.12;		page_height_inch = 5.85;
	} else
	if (strstr(arg,"a7")) {
		page_width_inch = 2.92;		page_height_inch = 4.12;
	} else
	if (strstr(arg,"a8")) {
		page_width_inch = 2.06;		page_height_inch = 2.92;
	} else
	if (strstr(arg,"a9")) {
		page_width_inch = 1.46;		page_height_inch = 2.06;
	} else
	if (strstr(arg,"a10")) {
		page_width_inch = 1.03;		page_height_inch = 1.46;
	} else
	if (strstr(arg,"b0")) {
		page_width_inch = 39.39;	page_height_inch = 55.67;
	} else
	if (strstr(arg,"b1")) {
		page_width_inch = 27.83;	page_height_inch = 39.39;
	} else
	if (strstr(arg,"b2")) {
		page_width_inch = 19.69;	page_height_inch = 27.83;
	} else
	if (strstr(arg,"b3")) {
		page_width_inch = 13.92;	page_height_inch = 19.69;
	} else
	if (strstr(arg,"b4")) {
		page_width_inch = 9.85;		page_height_inch = 13.92;
	} else
	if (strstr(arg,"b5")) {
		page_width_inch = 6.96;		page_height_inch = 9.85;
	} else
	if (strstr(arg,"archE")) {
		page_width_inch = 36.00;	page_height_inch = 48.00;
	} else
	if (strstr(arg,"archD")) {
		page_width_inch = 24.00;	page_height_inch = 36.00;
	} else
	if (strstr(arg,"archC")) {
		page_width_inch = 18.00;	page_height_inch = 24.00;
	} else
	if (strstr(arg,"archB")) {
		page_width_inch = 12.00;	page_height_inch = 18.00;
	} else
	if (strstr(arg,"archA")) {
		page_width_inch = 9.00;		page_height_inch = 12.00;
	} else
	if (strstr(arg,"flsa")) {
		page_width_inch = 8.50;		page_height_inch = 13.00;
	} else
	if (strstr(arg,"flse")) {
		page_width_inch = 8.50;		page_height_inch = 13.00;
	} else
	if (strstr(arg,"halfletter")) {
		page_width_inch = 5.50;		page_height_inch = 8.50;
	} else
	if (strstr(arg,"11x17")) {
		page_width_inch = 11.00;	page_height_inch = 17.00;
	} else
	if (strstr(arg,"ledger")) {
		page_width_inch = 17.00;	page_height_inch = 11.00;
	} else {
		fprintf(stderr,"a2ps: %s : unknown paper format or\n",arg);
		fprintf(stderr,"        ^--- missing paper format\n");
		exit(1);
	}
}

#endif	/* AKL */

/*
 * Set an option only if it's global.
 */
void
set_global_option(arg)
char *arg;
{
    switch (arg[1]) {
    case '?':				/* help */
    case 'h':
	usage(EXIT_SUCCESS);
    case 'b':				/* print binary files */
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
	print_binaries = TRUE;
	break;
    case 'c':				/* allow two files per sheet */
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
	twinfiles = TRUE;
	break;
    case 'f':				/* fold lines too large */
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
	folding = TRUE;
	break;
    case 'I':				/* include this file as a2ps prologue */
	if (arg[2] == NUL)
	    usage(EXIT_FAILURE);
	    prologue = arg+2;
	break;
    case 'i':				/* interpret control chars */
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
	interpret = TRUE;
	break;
    case 'n':
	if (arg[2] == NUL)
	    return;
	if (arg[3] != NUL)
	    usage(EXIT_FAILURE);
	switch (arg[2]) {
	case 'b':			/* don't print binaries */
	    print_binaries = FALSE;
	    break;
	case 'c':			/* don't allow 2 files/sheet */
	    twinfiles = FALSE;
	    break;
	case 'f':			/* cut lines too long */
	    folding = FALSE;
	    break;
	case 'H':			/* don't print header */
	    no_header = TRUE;
	    break;
	case 'i':			/* don't interpret ctrl chars */
	    interpret = FALSE;
	    break;
#if LPR_PRINT
	case 'P':			/* don't lpr */
	    lpr_print = FALSE;
	    break;
#endif
	case 'r':			/* don't restart sheet number */
	    restart = FALSE;
	    break;
	case 'v':			/* only printable chars */
	    only_printable = TRUE;
	    break;
	case '8':			/* don't print 8-bit chars */
	    ISOlatin1 = FALSE;
	    break;
	case 'B':
	case 'd':
	case 'L':
	case 'm':
	case 'n':
	case 's':
	case 'u':
	    if (arg[3] != NUL)
		usage(EXIT_FAILURE);
	    return;
	default:
	    usage(EXIT_FAILURE);
	}
	break;
#if LPR_PRINT
    case 'P':				/* fork a process to print */ 
	if (arg[2] != NUL) {
	    lpr_opt = (char *)malloc(strlen(arg)+1);
	    strcpy(lpr_opt, arg);
	}
	lpr_print = TRUE;
	break;
#endif
    case 'r':					/* restart sheet number */
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
	restart = TRUE;
	break;
    case 's':
	if (arg[2] == NUL)
	    return;
#ifdef RECTO_VERSO_PRINTING
	if (arg[3] == NUL) {
	    if (arg[2] == '1') {		/* one-sided printing */
		rectoverso = FALSE;
		break;
	    }
	    if (arg[2] == '2') {		/* two-sided printing */
		rectoverso = TRUE;
		break;
	    }
	}
#endif
	usage(EXIT_FAILURE);
	break;
    case 't':				/* set tab size */
	if (arg[2] == NUL || (column_width = atoi(arg+2)) <= 0)
	    usage(EXIT_FAILURE);
	break;
    case 'v':				/* print control chars */
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
	only_printable = FALSE;
	break;
    case '8':				/* print 8-bit chars */
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
	ISOlatin1 = TRUE;
	break;
    case '1':
    case '2':
    case 'B':
    case 'd':
    case 'm':
    case 'p':
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
    case '#':
    case 'F':
    case 'H':
    case 'l':
	return;
#ifdef AKL
    case 'X':			/* gs page dimensions */
	set_paper_format(arg);
	break;
    case 'M':
	if (arg[2] == NUL || (margin = atof(arg+2)) <= 0)
	    usage(EXIT_FAILURE);
	break;
    case 'Q':
	logname=arg+2;
	break;
    case 'Z':
	host=arg+2;
	break;
#endif
    default:
	usage(EXIT_FAILURE);
    }
    arg[0] = NUL;
}

/*
 * Set an option of the command line. This option will be applied
 * to all files that will be found in the rest of the command line.
 * The -H option is the only exception: it is applied only to the
 * file.
 */
void
set_positional_option(arg)
char *arg;
{
    int copies;
    int lines;
    float size;

    switch (arg[1]) {
    case NUL:				/* global option */
	break;
    case '#':				/* n copies */
	if (sscanf(&arg[2], "%d", &copies) != 1 || copies <= 0)
	    fprintf(stderr, "Bad number of copies: '%s'. Ignored\n", &arg[2]);
	else
	    copies_number = copies;
	printf("/#copies %d def\n", copies_number);
	break;
    case '1':				/* 1 logical page per sheet */
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
	new_twinpages = FALSE;
	break;
    case '2':				/* twin pages */
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
	new_twinpages = TRUE;
	break;
    case 'B':
	new_fontweight = BOLD;		/* use bold font */
	break;
    case 'd':				/* print current date/time */
	printdate = TRUE;
	break;
    case 'F':				/* change font size */
	if (arg[2] == NUL || sscanf(&arg[2], "%f", &size) != 1 || size == 0.0) {
	    fprintf(stderr, "Wrong value for option -F: '%s'. Ignored\n",
		    &arg[2]);
	    break;
	}
	new_fontsize = size;
	break;
    case 'H':				/* header text */
	header_text = arg+2;
	break;
    case 'l':
	if (arg[2] == NUL) {		/* landscape format */
	    new_landscape = TRUE;
	    break;
	}
					/* set lines per page */
	/* Useful with preformatted files. Scaling is automatically	*/
	/* done when necessary.						*/
	if (sscanf(&arg[2], "%d", &lines) != 1
	    || lines < 0 || lines > MAX_LINES)
	{
	    fprintf(stderr, "Wrong value for option -l: '%s'. Ignored\n",
		    &arg[2]);
	    break;
	}
	new_linesrequest = lines;
	break;
    case 'm':				/* Process file as a man */
	new_linesrequest = MAN_LINES;
	numbering = FALSE;
	break;
    case 'n':				/* number file lines */
	if (arg[2] == NUL) {
	    numbering = TRUE;
	    break;
	}
	switch (arg[2]) {
	case 'B':			/* disable bold text */
	    new_fontweight = NORMAL;
	    break;
	case 'd':			/* don't print date/time */
	    printdate = FALSE;
	    break;
#if defined(SYSV) || defined(BSD)					
	case 'L':			/* no login name in footer */
	    login_id = FALSE;
	    break;
#endif					
	case 'l':			/* portrait format */
	    new_landscape = FALSE;
	    break;
	case 'm':			/* stop processing as a man */
	    new_linesrequest = 0;
	    break;
	case 'n':			/* don't number lines */
	    numbering = FALSE;
	    break;
	case 'p':			/* landscape format */
	    new_landscape = TRUE;
	    break;
	case 's':			/* no surrounding border */
	    no_border = TRUE;
	    break;
	case 'u':			/* no filename in footer */
	    filename_footer = FALSE;
	    break;
	default:
	    usage(EXIT_FAILURE);
	}
	break;
    case 'p':				/* portrait format */
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
	new_landscape = FALSE;
	break;
    case 's':				/* surrounding border */
	if (arg[2] != NUL)
	    usage(EXIT_FAILURE);
	no_border = FALSE;
	break;
    default:
	usage(EXIT_FAILURE);
    }
}


/****************************************************************/
/*			Service routines			*/
/****************************************************************/

/*
 * This routine buffers a line of input, release one character at a time
 * or a whole sequence of characters with some meaning like bold sequences
 * produced by nroff (no others sequences are recognized by the moment):
 *        <c><\b><c><\b><c><\b><c>
 */
int
mygetc(statusp)
int *statusp;
{
#define BUFFER_SIZE	512
    static int curr = 0;
    static int size = 0;
    static unsigned char buffer[BUFFER_SIZE+1];
    int c;

    *statusp = IS_ROMAN;

    /* Read a new line, if necessary */
    if (curr >= size) {
	if (fgets((char *)buffer, BUFFER_SIZE+1, stdin) == NULL)
	    return  EOF;
	size = strlen(buffer);
	if (size < BUFFER_SIZE && buffer[size-1] != '\n') {
	    buffer[size] = '\n';
	    buffer[++size] = '\0';
	}
	curr = 0;
    }
    if (buffer[curr+1] != '\b')		/* this is not a special sequence */
	return  buffer[curr++];

    /* Check if it is a bold sequence */
    if ((c = buffer[curr++]) == buffer[curr+1] &&
	buffer[curr]	== buffer[curr+2] &&
	c		== buffer[curr+3] &&
	buffer[curr]	== buffer[curr+4] &&
	c		== buffer[curr+5])
    {
	*statusp = IS_BOLD;
	curr += 6;
    }

    /* Return the first character of the sequence */
    return  c;
}

/*
 * Test if we have a binary file.
 */
int
is_binaryfile(name)
char *name;
{
    if (chars > 120 || pages > 1) {
	first_page = FALSE;
	if (chars && !print_binaries && (nonprinting_chars*100 / chars) >= 60) {
	    fprintf(stderr, "%s is a binary file: printing aborted\n", name);
	    return TRUE;
	}
    }
    return FALSE;
}

/*
 * Cut long filenames.
 */
void
cut_filename(old_name, new_name)
char *old_name, *new_name;
{
    register char *p;
    register int i;
    char *separator;

    p = old_name + (strlen(old_name)-1);
    separator = NULL;
    i = 1;
    while (p >= old_name && i < MAXFILENAME) {
	if (*p == DIR_SEP)
	    separator = p;
	p--;
	i++;
    }
    if (separator != NULL)
	p = separator;
    else if (p >= old_name)
	while (p >= old_name && *p != DIR_SEP) p--;

    for (i = 0, p++; *p != NUL; i++)
	*new_name++ = *p++;
    *new_name = NUL;
}

/*
 * Print a char in a form accepted by postscript printers.
 */
int
printchar(c)
unsigned char c;
{
    if (c >= ' ' && c < 0177) {
	if (c == '(' || c == ')' || c == '\\')
	    putchar('\\');
	putchar(c);
	return 0;
    }

    if (ISOlatin1 && (c > 0177)) {
	printf("\\%o", c);
	return 0;
    }

    if (only_printable) {
	putchar(' ');
	return 1;
    }
    
    if (c > 0177) {
	printf("M-");
	c &= 0177;
    }
    if (c < ' ') {
/* bonnes 27.6.95  bon@elektron.ikp.physik.th-darmstadt.de 
   Delete carriage return from DOS-Files
*/  
      if (c != 015) {
 
	putchar('^');
	if ((c = c + '@') == '(' || c == ')' || c == '\\')
	    putchar('\\');
	putchar(c);
      }
    }
    else if (c == 0177)
	printf("^?");
    else {
	if (c == '(' || c == ')' || c == '\\')
	    putchar('\\');
	putchar(c);
    }

    return 1;
}

/*
 * Begins a new physical page.
 */
void
skip_page()
{
    if (twinpages == FALSE || sheetside == 0)
	printf("%%%%Page: %d %d\n", sheets+1, sheets+1);
    startpage();
}

/*
 * Fold a line too long.
 */
int
fold_line(name)
char *name;
{
    column = 0;
    printf(") s\n");
    if (++line >= linesperpage) {
	endpage();
	skip_page();
	if (first_page && is_binaryfile(name))
	    return FALSE;
	line = 0;
    }
    if (numbering)
	printf("(     +");
    else
	printf("( ");
    
    return TRUE;
}

/*
 * Cut a textline too long to the size of a page line.
 */
int
cut_line()
{
    int c;
    int status;

    while ((c = mygetc(&status)) != EOF && c != '\n' && c != '\f');
    return c;
}


/****************************************************************/
/*			"Postscript" routines.			*/
/****************************************************************/

/*
 * Print a physical page.
 */
void
printpage()
{
    sheetside = 0;
    sheets++;
    printf("/side 0 def\n");
    if (no_border == FALSE)
	printf("%d sheetnumber\n", sheets - (restart ? old_sheets : 0));
    if (printdate)
	printf("currentdate\n");
    if (filename_footer && landscape)
	printf("filenamefooter\n");
#if defined(SYSV) || defined(BSD)	
    if (login_id)
	printf("login prlogin\n");
#endif	
    printf("showpage\n");
}

/*
 * Prints page header and page border and
 * initializes printing of the file lines.
 */
void
startpage()
{
    if (sheetside == 0) {
#ifdef RECTO_VERSO_PRINTING
	if (rectoverso && (sheets & 0x1)) {
	    /* Shift to left backside pages.  */
	    printf("rightmargin neg 0 translate\n");
	}
#endif
	if (landscape) {
	    printf("sheetwidth 0 translate\n");
	    printf("90 rotate\n");
	}
    }
    pages++;
    if (no_header == FALSE)
	printf("%d header\n", pages - old_pages);
    if (no_border == FALSE) {
	printf("border\n");
	if (no_header == FALSE)
	    printf("hborder\n");
    }
    printf("/x0 upperx %d get bodymargin add def\n", sheetside);
    printf("/y0 uppery %d get bodymargin bodyfontsize add %s add sub def\n",
	   sheetside, no_header ? "0" : "headersize");
    printf("x0 y0 moveto\n");
    printf("bodyfont setfont\n");
}

/*
 * Terminates printing, flushing last page.
 */
void
cleanup()
{
    if (twinpages && sheetside == 1)
	printpage();
#ifdef RECTO_VERSO_PRINTING
    if (!twinfiles && rectoverso && (sheets & 0x1) != 0) {
	sheetside = 0;
	sheets++;
	printf("showpage\n");
    }
#endif
}

/*
 * Adds a sheet number to the page (footnote) and prints the formatted
 * page (physical impression). Activated at the end of each source page.
*/
void
endpage()
{
    if (twinpages && sheetside == 0) {
	sheetside = 1;
	printf("/side 1 def\n");
    }
    else
	printpage();
}


/****************************************************************/
/*		Printing a file					*/
/****************************************************************/

/*
 * Print the file prologue.
 */
void
print_file_prologue(name, title)
char *name, *title;
{
    int new_format, new_font;
    char *p, *string;
    int lines;
    float char_width, header_size;
    struct stat statbuf;
    char new_title[512];

    /* Print last page of previous file, if necessary */
    if (pages > 0 && !twinfiles)
	cleanup();

    /* Initialize variables related to the format */
    new_format = FALSE;
    if (new_landscape != landscape || new_twinpages != twinpages) {
	landscape = new_landscape;
	twinpages = new_twinpages;
	new_format = TRUE;
	printf("/landscape %s def\n", landscape ? "true" : "false");
	printf("/twinpages %s def\n", twinpages ? "true" : "false");
	printf("%% Character size for fonts.\n");
	printf("/filenmfontsize %d def\n", landscape ? 11 : twinpages ? 10 : 15);
	printf("/datefontsize filenmfontsize 0.8 mul def\n");
	printf("/datefont /Helvetica datefontsize getfont def\n");
	printf("/datewidth datefont setfont currdate stringwidth pop def\n");
	printf("/stdfilenmfont filenmfontname filenmfontsize getfont def\n");
	printf("/headermargin filenmfontsize 0.25 mul def\n");
    }

    /* Initialize variables related to the header */
    if (no_header && name == title) {
	header_size = 0.0;
	printf("/headersize 0.0 def\n");
    }
    else {
	if (landscape || twinpages)
	    header_size = LANDSCAPE_HEADER * PIXELS_INCH;
	else
	    header_size = PORTRAIT_HEADER * PIXELS_INCH;
	if (strlen(title) > MAXFILENAME) {
	    cut_filename(title, new_title);
	    title = new_title;
	}
	printf("/headersize %g inch def\n",
	       landscape || twinpages ? LANDSCAPE_HEADER : PORTRAIT_HEADER);
    }

    /* Initialize variables related to the font size */
    new_font = FALSE;
    if (fontsize != new_fontsize || new_format ||
	lines_requested != new_linesrequest || fontweight != new_fontweight)
    {
	if (new_fontsize == 0.0 || (fontsize == new_fontsize && new_format))
	    new_fontsize = landscape ? 6.8 : twinpages ? 6.4 : 9.0;
	if (lines_requested != new_linesrequest) {
	    if ((lines_requested = new_linesrequest) != 0) {
		/* Scale fontsize */
		if (landscape)
		    lines = (int)((page_width-header_size) / new_fontsize) - 1;
		else if (twinpages)
		    lines = (int)(((page_height - 2*header_size) / 2) / new_fontsize) - 2;
		else
		    lines = (int)((page_height-header_size) / new_fontsize) - 1;
		new_fontsize *= (float)lines / (float)lines_requested;
	    }
	}
	fontsize = new_fontsize;
	new_font = TRUE;
	printf("/bodyfontsize %g def\n", fontsize);
	printf("/bodyfont /CourierBack bodyfontsize getfont def\n");
	printf("/boldfont /Courier-Bold bodyfontsize getfont def\n");
	printf("/bodymargin bodyfontsize 0.7 mul def\n");
	switch (new_fontweight) {
	case BOLD:
	    printf("/bodyfont /Courier-Bold bodyfontsize getfont def\n");
	    break;
	case NORMAL:
	    printf("/bodyfont /CourierBack bodyfontsize getfont def\n");
	    break;
	default:
	    break;				
	}
	fontweight = new_fontweight;
    }

    /* Initialize file printing, if there is any change */
    if (new_format || new_font) {
	char_width = 0.6 * fontsize;
	if (landscape) {
	    linesperpage = (int)((page_width - header_size) / fontsize) - 1;
	    if (! twinpages)
		columnsperline = (int)(page_height / char_width) - 1;
	    else
		columnsperline = (int)((page_height / 2) / char_width) - 1;
	}
	else {
	    if (!twinpages)
		linesperpage = (int)((page_height - header_size) / fontsize) - 1;
	    else
		linesperpage = (int)(((page_height - 2*header_size) / 2) / fontsize)
		    - 2;
	    columnsperline = (int)(page_width / char_width) - 1;
	}
	if (lines_requested > 0)
	    linesperpage = lines_requested;
	if (linesperpage <= 0 || columnsperline <= 0) {
	    fprintf(stderr, "Font %g too big !!\n", fontsize);
	    exit(EXIT_FAILURE);
	}
	printf("/lines %d def\n", linesperpage);
	printf("/columns %d def\n", columnsperline);
	printf("\n%% Logical page attributs (a half of a sheet).\n");
	printf("/pagewidth\n");
	printf("   bodyfont setfont (0) stringwidth pop columns mul bodymargin dup add add\n");
	printf("   def\n");
	printf("/pageheight\n");
	printf("   bodyfontsize lines mul bodymargin dup add add headersize add\n");
	printf("   def\n");
	printf("/filenmroom\n");
	printf("      pagewidth\n");
	printf("      filenmfontsize 4 mul datewidth add (Page 999) stringwidth pop add\n");
	printf("    sub\n");
	printf("  def\n");
	printf("\n%% Coordinates for upper corner of a logical page and for\n");
	printf("%% sheet number. Coordinates depend on format mode used.\n");
	printf("/topmargin margin twinpages {3} {2} ifelse div def\n");
	printf("/side 0 def\n");
	if (landscape) {
	    printf("\n%% Landscape format\n");
	    printf("/uppery [ rightmargin pageheight add bodymargin add\n");
	    printf("          dup ] def\n");
	    printf("/sheetnumbery datefontsize datefontsize add def\n");
	    printf("/sheetnumberx sheetheight topmargin datefontsize add sub def\n");
	    printf("/datey sheetnumbery def\n");
	    printf("/datex topmargin datefontsize add def\n");
	    if (twinpages) {
		printf("%% In twinpages mode, coordinate x of upper corner\n");
		printf("%% is not the same for both pages: upperx is an\n");
		printf("%% array of 2 elements, indexed by the sheetside.\n");
		printf("\n%% Two logical pages\n");
		printf("/upperx [ topmargin			%% left page\n");
		printf("          dup 2 mul pagewidth add	%% right page\n");
		printf("        ] def\n");
	    }
	    else {
		printf("\n%% Only one logical page\n");
		printf("/upperx [ topmargin dup ] def\n");
	    }
	    printf("/sheetcenterx sheetheight 2 div def\n");
	}
	else {
	    printf("\n%% Portrait format\n");
	    printf("/upperx [ leftmargin dup ] def\n");
	    printf("/sheetnumbery topmargin datefontsize 2 mul sub def\n");
	    printf("/sheetnumberx sheetwidth rightmargin sub datefontsize sub def\n");
	    printf("/datey sheetnumbery def\n");
	    printf("/datex leftmargin def\n");
	    if (twinpages) {
		printf("%% In twinpages mode, coordinate y of upper corner\n");
		printf("%% is not the same for both pages: uppery is an\n");
		printf("%% array of 2 elements, indexed by the sheetside.\n");
		printf("\n%% Two logical pages\n");
		printf("/uppery [ topmargin pageheight add 2 mul %% up\n");
		printf("          topmargin pageheight add	 %% down\n");
		printf("        ] def\n");
	    }
	    else {
		printf("\n%% Only one logical page\n");
		printf("/uppery [ sheetheight topmargin sub dup ] def\n");
	    }
	    printf("/datey sheetnumbery def\n");
	    printf("/datex leftmargin def\n");
	    printf("/sheetcenterx sheetwidth 2 div def\n");
	}
	printf("/filenamefootery datey def\n");
	printf("/filenamefooterx sheetcenterx def\n");
	printf("/loginy filenmfontsize 2 div uppery side get add def\n");
	printf("/loginx sheetnumberx def\n");
    }

    /* Retrieve file modification date and hour */
    if (fstat(fileno(stdin), &statbuf) == -1) {
	fprintf(stderr, "Error getting file modification time\n");
	exit(EXIT_FAILURE);
    }
#ifdef AKL
    /* modifications for FreeBSD */
    /* Do we have a fifo ? */
    if (S_ISFIFO(statbuf.st_mode))
	printf("/date currdate def\n");
    /* Do we have a pipe ? */
    else if (!statbuf.st_mode)
	printf("/date currdate def\n");
#else
    /* Do we have a pipe? */
    if (S_ISFIFO(statbuf.st_mode))
	printf("/date currdate def\n");
#endif
    else {
	string = ctime(&statbuf.st_mtime);
	printf("/date (%.6s %.4s %.5s) def\n", string+4, string+20, string+11);
    }


    /* Start file impression */
    putchar('(');
    for (p = title; *p != NUL;)
	printchar(*p++);
    printf(") newfile\n");
}

/*
 * Print one file.
 */
void
print_file(name, header)
char *name, *header;
{
    register int c;
    int nchars;
    int start_line, start_page;
    int continue_exit;
    int status, new_status;

    /* Reinitialize postscript variables depending on positional options */
    print_file_prologue(name, header == NULL ? name : header);
    
    /*
     * Boolean to indicates that previous char is \n (or interpreted \f)
     * and a new page would be started, if more text follows
     */
    start_page = FALSE;
    
    /*
     * Printing binary files is not very useful. We stop printing
     * if we detect one of these files. Our heuristic to detect them:
     * if 75% characters of first page are non-printing characters,
     * the file is a binary file.
     * Option -b force binary files impression.
     */
    nonprinting_chars = chars = 0;
    
    /* Initialize printing variables */
    column = 0;
    line = line_number = 0;
    first_page = TRUE;
    start_line = TRUE;
    prefix_width = numbering ? 6 : 1;

    /* Start printing */
    skip_page();

    /* Process each character of the file */
    status = IS_ROMAN;
    c = mygetc(&new_status);
    while (c != EOF) {
	/*
	 * Preprocessing (before printing):
	 * - TABs expansion (see interpret option)
	 * - FF and BS interpretation
	 * - replace non printable characters by a space or a char sequence
	 *   like:
	 *     ^X for ascii codes < 0x20 (X = [@, A, B, ...])
	 *     ^? for del char
	 *     M-c for ascii codes > 0x3f
	 * - prefix parents and backslash ['(', ')', '\'] by backslash
	 *   (escape character in postcript)
	 */
	/* Form feed */
	if (c == '\f' && interpret) {
	    /* Close current line */
	    if (!start_line) {
		printf(") s\n");
		start_line = TRUE;
	    }
	    /* start a new page ? */
	    if (start_page)
		skip_page();
	    /* Close current page and begin another */
	    endpage();
	    start_page = TRUE;
	    /* Verification for binary files */
	    if (first_page && is_binaryfile(name))
		return;
	    line = 0;
	    column = 0;
	    if ((c = mygetc(&new_status)) == EOF)
		break;
	}
	
	/* Start a new line ? */
	if (start_line)	{
	    if (start_page) {
		/* only if there is something to print! */
		skip_page();
		start_page = FALSE ;
	    }
	    if (numbering)
		printf("(%4d|", ++line_number);
	    else
		printf("( ");
	    start_line = FALSE;
	}
	
	/* Is a new font ? This feature is used only to detect bold	*/
	/* sequences produced by nroff (man pages), in connexion with	*/
	/* mygetc.							*/
	if (status != new_status) {
	    printf(")\n");
	    printf("%s", status == IS_ROMAN ? "b" : "st");
	    printf(" (");
	    status = new_status;
	}

	/* Interpret each character */
	switch (c) {
	case '\b':
	    if (!interpret)
		goto print;
	    /* A backspace is converted to 2 chars ('\b'). These chars	*/
	    /* with the Courier backspace font produce correct under-	*/
	    /* lined strings.	*/
	    if (column)
		column--;
	    putchar('\\');
	    putchar('b');
	    break;
	case '\n':
	    column = 0;
	    start_line = TRUE;
	    printf(") s\n");
	    if (++line >= linesperpage) {
		endpage();
		start_page = TRUE ;
		if (first_page && is_binaryfile(name))
		    return;
		line = 0;
	    }
	    break;
	case '\t':
	    if (interpret) {
		continue_exit = FALSE;
		do {
		    if (++column + prefix_width > columnsperline) {
			if (folding) {
			    if (fold_line(name) == FALSE)
				return;
			}
			else {
			    c = cut_line();
			    continue_exit = TRUE;
			    break;
			}
		    }
		    putchar(' ');
		} while (column % column_width);
		if (continue_exit)
		    continue;
		break;
	    }
	default:
	print:
	    if (only_printable)
		nchars = 1;
	    else if (! ISOlatin1) {
		nchars = c > 0177 ? 2 : 0;
		nchars += (c&0177) < ' ' || (c&0177) == 0177 ? 2 : 1;
	    }
	    else
		nchars = c < ' ' || (c >= 0177 && c < 144) ? 2 : 1;
	    if (prefix_width + (column += nchars) > columnsperline) {
		if (folding) {
		    if (fold_line(name) == FALSE)
			return;
		}
		else {
		    c = cut_line();
		    new_status = IS_ROMAN;
		    continue;
		}
	    }
	    nonprinting_chars += printchar(c);
	    chars++;
	    break;
	}
	c = mygetc(&new_status);
    }
    
    if (!start_line)
	printf(") s\n");
    if (!start_page)
	endpage();
}


/****************************************************************/
/*		Print a postscript prologue for a2ps.		*/
/****************************************************************/

/*
 * Print the a2ps prologue.
 */
void
print_prologue()
{
    register int c;
    FILE *f;
    char *datestring;
#if defined(SYSV) || defined(BSD)
#ifndef AKL
    char *logname, *host;
#endif
    int rt;
#endif

    /* Retrieve date and hour */
#ifdef __STDC__
    time_t date;

    if (time(&date) == -1) {
	fprintf(stderr, "Error calculating time\n");
	exit(EXIT_FAILURE);
    }
    datestring = ctime(&date);
#else
#ifdef BSD
    struct timeval date;
    struct tm *p;

    (void) gettimeofday(&date, (struct timezone *)0);
    p = localtime(&date.tv_sec);
    datestring = asctime(p);
#else
#ifdef SYSV
    struct timeb date;

    (void)ftime(&date);
    datestring = ctime(&date.time);
#else

    datestring = "--- --- -- --:--:-- ----";
#endif
#endif
#endif

#if defined(SYSV) || defined(BSD)
    /* Retrieve user's login name and hostname */
#ifdef AKL
    if (!logname)
	logname = getlogin();
    if (!host) 
    {
    	host = (char *)malloc(MAX_HOSTNAME);
    	if (host != NULL)
	{
		if ((rt = gethostname(host, MAX_HOSTNAME)) == -1 || host[0] == NULL)
		{
	    		free(host);
	    		host = NULL;
		}
    	}
    }
#else
    logname = getlogin();
    host = (char *)malloc(MAX_HOSTNAME);
    if (host != NULL) {
	if ((rt = gethostname(host, MAX_HOSTNAME)) == -1 || host[0] == NULL) {
	    free(host);
	    host = NULL;
	}
    }
#endif /* AKL */
#endif
    
    /* Print a general prologue */
    if (prologue == NULL)
	print_standard_prologue(datestring);
    else if ((f = fopen(prologue, "r")) != NULL) {
	/* Header file printing */
	while ((c = getc(f)) != EOF)
	    putchar(c);
    }
    else {
	fprintf(stderr, "Postscript header missing: %s\n", prologue);
	exit(EXIT_FAILURE);
    }

    /* Completes the prologue with a2ps static variables */
    printf("\n%% Initialize page description variables.\n");
    printf("/x0 0 def\n");
    printf("/y0 0 def\n");
#ifdef AKL
    printf("/sheetheight %g inch def\n", (double)page_height_inch);
    printf("/sheetwidth %g inch def\n", (double)page_width_inch);
    printf("/margin %g inch def\n", margin);
#else
    printf("/sheetheight %g inch def\n", (double)HEIGHT);
    printf("/sheetwidth %g inch def\n", (double)WIDTH);
    printf("/margin %g inch def\n", (double)MARGIN);
#endif
    printf("/rightmargin margin 3 div def\n");
    printf("/leftmargin margin 2 mul 3 div def\n");
    printf("/twinfiles %s def\n", twinfiles ? "true" : "false");
    printf("/date () def\n");

    /* And print them */
    printf("/currdate (%.6s %.4s %.5s) def\n",
	   datestring+4, datestring+20, datestring+11);

#if defined(SYSV) || defined(BSD)
    /* Add the user's login name string to the Postscript output */
    if (logname != NULL || host != NULL) {
	if (logname != NULL && host != NULL)
	    printf("/login (Printed by %s from %s) def\n", logname, host);
	else if (logname != NULL)
	    printf("/login (Printed by %s) def\n", logname);
	else
	    printf("/login (Printed from %s) def\n", host);
    }

#ifndef AKL
	/* AKL: causes Segmentation fault, when running twice ... */
    /* If the host string was allocated via malloc, release the	memory */
    if (host != NULL)
	free(host);
#endif
#endif

    /* Close prolog */
    printf("%%%%EndProlog\n\n");
    
    /* Go on */
    printf("/docsave save def\n");
}

/*
 * Print the standard prologue.
 */
void
print_standard_prologue(datestring)
char *datestring;
{
    printf("%%!PS-Adobe-3.0\n");
    printf("%%%%Creator: A2ps version %s\n", VERSION);
    printf("%%%%CreationDate: %.24s\n", datestring);
    printf("%%%%Pages: (atend)\n");
    printf("%%%%DocumentFonts: Courier Courier-Bold Helvetica Helvetica-Bold\n");
    printf("%%%%EndComments\n");
    printf("%% Copyright (c) 1992, 1993, Miguel Santana, santana@imag.fr\n");
    printf("\n/$a2psdict 100 dict def\n");
    printf("$a2psdict begin\n");
    printf("\n%% General macros.\n");
    printf("/xdef {exch def} bind def\n");
    printf("/getfont {exch findfont exch scalefont} bind def\n");

    if (ISOlatin1) {
	printf("\n%% Set up ISO Latin 1 character encoding\n");
	printf("/reencodeISO {\n");
	printf("	dup dup findfont dup length dict begin\n");
	printf("	{ 1 index /FID ne { def }{ pop pop } ifelse\n");
	printf("	} forall\n");
	printf("	/Encoding ISOLatin1Encoding def\n");
	printf("	currentdict end definefont\n");
	printf("} def\n");
	printf("/Helvetica-Bold reencodeISO def\n");
	printf("/Helvetica reencodeISO def\n");
	printf("/Courier reencodeISO def\n");
	printf("/Courier-Bold reencodeISO def\n");
    }

    printf("\n%% Create Courier backspace font\n");
    printf("/backspacefont {\n");
    printf("    /Courier findfont dup length dict begin\n");
    printf("	{ %% forall\n");
    printf("	    1 index /FID eq { pop pop } { def } ifelse\n");
    printf("	} forall\n");
    printf("	currentdict /UniqueID known { %% if\n");
    printf("	    /UniqueID UniqueID 16#800000 xor def\n");
    printf("	} if\n");
    printf("	CharStrings length 1 add dict begin\n");
    printf("	    CharStrings { def } forall\n");
    printf("	    /backspace { -600 0 0 0 0 0 setcachedevice } bind def\n");
    printf("	    currentdict\n");
    printf("	end\n");
    printf("	/CharStrings exch def\n");
    printf("	/Encoding Encoding 256 array copy def\n");
    printf("	Encoding 8 /backspace put\n");
    printf("	currentdict\n");
    printf("    end\n");
    printf("    definefont pop\n");
    printf("} bind def\n");

    printf("\n%% FUNCTIONS\n");
    printf("\n%% Function newfile: Initialize file printing.\n");
    printf("/newfile\n");
    printf("{ /filenm xdef\n");
    printf("  /filenmwidth filenm stringwidth pop def\n");
    printf("  /filenmfont\n");
    printf("       filenmwidth filenmroom gt\n");
    printf("       {\n");
    printf("	       filenmfontname\n");
    printf("	       filenmfontsize filenmroom mul filenmwidth div\n");
    printf("	     getfont\n");
    printf("       }\n");
    printf("       { stdfilenmfont }\n");
    printf("     ifelse\n");
    printf("  def\n");
    printf("} bind def\n");
    printf("\n%% Function header: prints page header. no page\n");
    printf("%% is passed as argument.\n");
    printf("/header\n");
    printf("  { upperx side get  uppery side get headersize sub 1 add  moveto\n");
    printf("    datefont setfont\n");
    printf("    gsave\n");
    printf("      upperx side get uppery side get moveto\n");
    printf("      0 headersize 2 div neg rmoveto \n");
    printf("      headersize setlinewidth\n");
    printf("      0.95 setgray\n");
    printf("      pagewidth 0 rlineto stroke\n");
    printf("    grestore\n");
    printf("    gsave\n");
    printf("      datefontsize headermargin rmoveto\n");
    printf("      date show				%% date/hour\n");
    printf("    grestore\n");
    printf("    gsave\n");
    printf("      pnum cvs pop				%% page pop up\n");
    printf("        pagewidth (Page 999) stringwidth pop sub\n");
    printf("        headermargin\n");
    printf("	  rmoveto\n");
    printf("      (Page ) show pnum show		%% page number\n");
    printf("    grestore\n");
    printf("    empty pnum copy pop\n");
    printf("    gsave\n");
    printf("      filenmfont setfont\n");
    printf("         filenmroom filenm stringwidth pop sub 2 div datewidth add\n");
    printf("          bodymargin 2 mul \n");
    printf("        add \n");
    printf("        headermargin\n");
    printf("      rmoveto\n");
    printf("        filenm show			%% file name\n");
    printf("      grestore\n");
    printf("    } bind def\n");
    printf("\n%% Function border: prints border page\n");
    printf("/border \n");
    printf("{ upperx side get uppery side get moveto\n");
    printf("  gsave				%% print four sides\n");
    printf("    0.7 setlinewidth		%% of the square\n");
    printf("    pagewidth 0 rlineto\n");
    printf("    0 pageheight neg rlineto\n");
    printf("    pagewidth neg 0 rlineto\n");
    printf("    closepath stroke\n");
    printf("  grestore\n");
    printf("} bind def\n");
    printf("\n%% Function hborder: completes border of the header.\n");
    printf("/hborder \n");
    printf("{ gsave\n");
    printf("	0.7 setlinewidth\n");
    printf("	0 headersize neg rmoveto\n");
    printf("	pagewidth 0 rlineto\n");
    printf("	stroke\n");
    printf("  grestore\n");
    printf("} bind def\n");
    printf("\n%% Function sheetnumber: prints the sheet number.\n");
    printf("/sheetnumber\n");
    printf("    { sheetnumberx sheetnumbery moveto\n");
    printf("      datefont setfont\n");
    printf("      pnum cvs\n");
    printf("	  dup stringwidth pop (0) stringwidth pop sub neg 0 rmoveto show\n");
    printf("      empty pnum copy pop\n");
    printf("    } bind def\n");
    printf("\n%% Function prlogin: prints the login id of the requestor.\n");
    printf("/prlogin\n");
    printf("    { loginx loginy moveto\n");
    printf("      datefont setfont\n");
    printf("      dup stringwidth pop neg 0 rmoveto show\n");
    printf("    } bind def\n");
    printf("\n%% Function currentdate: prints the current date.\n");
    printf("/currentdate\n");
    printf("    { datex datey moveto\n");
    printf("      datefont setfont\n");
    printf("      (Printed: ) show\n");
    printf("      currdate show\n");
    printf("    } bind def\n");
    printf("\n%% Function filename_footer: prints the file name at bottom of page.\n");
    printf("/filenamefooter\n");
    printf("    { filenamefooterx filenamefootery moveto\n");
    printf("      datefont setfont\n");
    printf("      filenm center show\n");
    printf("    } bind def\n");
    printf("\n%% Function center: centers text.\n");
    printf("/center\n");
    printf("    { dup stringwidth pop\n");
    printf("      2 div neg 0 rmoveto\n");
    printf("    } bind def\n");
    printf("\n%% Function s: print a source line\n");
    printf("/s  { show\n");
    printf("      /y0 y0 bodyfontsize sub def\n");
    printf("      x0 y0 moveto\n");
    printf("    } bind def\n");
    printf("\n%% Functions b and st: change to bold or standard font\n");
    printf("/b  { show\n");
    printf("      boldfont setfont\n");
    printf("    } bind def\n");
    printf("/st { show\n");
    printf("      bodyfont setfont\n");
    printf("    } bind def\n");
    printf("\n%% Strings used to make easy printing numbers\n");
    printf("/pnum 12 string def\n");
    printf("/empty 12 string def\n");
    printf("\n%% Global initializations\n");
    printf("\n/CourierBack backspacefont\n");
    printf("/filenmfontname /Helvetica-Bold def\n");
    printf("/inch {72 mul} bind def\n");
}


/*
 * Main routine for a2ps.
 */
int
main(argc, argv)
int argc;
char *argv[];
{
    register int narg;
    register char *arg;
    int number;
#if LPR_PRINT
    int fd[2];
    char *lpr_args[10];
#endif
    
    /* Process global options  */
    command = argv[0];
    arg = argv[narg = 1];
    while (narg < argc) {
	if (arg[0] == '-')
	    set_global_option(arg);
	arg = argv[++narg];
    }

#if LPR_PRINT
    /* Start lpr process */
    if (lpr_print) {
	pipe(fd);
	if (fork() == 0) {
	    dup2(fd[0], 0);
	    close(fd[0]); close(fd[1]);
	    narg = 0;
	    lpr_args[narg++] = LPR_COMMAND;
#ifdef LPR_OPT
	    lpr_args[narg++] = LPR_OPT;
#endif
	    if (lpr_opt)
		lpr_args[narg++] = lpr_opt;
#ifdef RECTO_VERSO_PRINTING
	    if (rectoverso)
		lpr_args[narg++] = TWOSIDED;
	    else
		lpr_args[narg++] = ONESIDED;
#endif
	    lpr_args[narg] = (char *)0;
	    execvp(LPR_COMMAND, lpr_args);
	    fprintf(stderr, "Error starting lpr process \n");
	    exit(EXIT_FAILURE);
	}
	dup2(fd[1],1);
	close(fd[0]);
	close(fd[1]);
    }
#endif

    /* Initialize variables not depending of positional options */
    landscape = twinpages = -1;	/* To force format switching */
    fontsize = -1.0;			/* To force fontsize switching */
#ifdef AKL
    page_height = (page_height_inch - margin) * PIXELS_INCH;
    page_width  = (page_width_inch  - margin) * PIXELS_INCH;
# ifdef AKL_DEBUG
    fprintf(stderr,"DEBUG... margin           : %.1f\n", margin);
    fprintf(stderr,"DEBUG... page_width_inch  : %.2f\n", page_width_inch);
    fprintf(stderr,"DEBUG... page_height_inch : %.2f\n", page_height_inch);
    fprintf(stderr,"DEBUG... page_width  pixel: %.2f\n", page_width);
    fprintf(stderr,"DEBUG... page_height pixel: %.2f\n", page_height);
# endif /* AKL_DEBUG */
#else
    page_height = (double)(HEIGHT - MARGIN) * PIXELS_INCH;
    page_width = (double)(WIDTH - MARGIN) * PIXELS_INCH;
#endif /* AKL */
    
    /* Postcript prologue printing */
    print_prologue();
    
    /* Print files designated or standard input */
    arg = argv[narg = 1];
    while (narg < argc) {
	if (arg[0] != NUL) {
	    if (arg[0] == '-')
		set_positional_option(arg);
	    else {
		if (freopen(arg, "r", stdin) == NULL) {
		    fprintf(stderr, "Error opening %s\n", arg);
		    cleanup();
		    printf("\n%%%%Trailer\ndocsave restore end\n\4");
		    exit(EXIT_FAILURE);
		}
		no_files = FALSE;

		/* Save counters values */
		old_pages = pages;
		if (twinfiles && twinpages)
		    old_sheets = sheets;
		else
		    old_sheets = sheets + sheetside;

		/* Print the file */
		print_file(arg, header_text);

		/* Print the number of pages and sheets printed */
		number = pages - old_pages;
		fprintf(stderr, "[%s: %d page%s on ", arg,
			number, number == 1 ? "" : "s");
		number = sheets - old_sheets + sheetside;
#ifdef RECTO_VERSO_PRINTING
		if (rectoverso)
		    number = (number+1) / 2;
#endif
		fprintf(stderr, "%d sheet%s]\n", number, number == 1 ? "" : "s");

		/* Reinitialize header title */
		header_text = NULL;
	    }
	}
	arg = argv[++narg];
    }
    if (no_files)
	print_file("stdin", header_text);

    /* Print the total number of lines printed */
    if (pages != old_pages) {
	fprintf(stderr, "[Total: %d page%s on ", pages, pages == 1 ? "" : "s");
	number = sheets + sheetside;
#ifdef RECTO_VERSO_PRINTING
	if (rectoverso)
	    number = (number+1) / 2;
#endif
	fprintf(stderr, "%d sheet%s]\n", number, number == 1 ? "" : "s");
    }

    /* And stop */
    cleanup();
    printf("\n%%%%Trailer\n");
    printf("%%%%Pages: %d\n", number);
    printf("docsave restore end\n");

    exit(EXIT_SUCCESS);
}
