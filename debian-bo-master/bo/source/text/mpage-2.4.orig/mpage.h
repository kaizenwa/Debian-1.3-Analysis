/*
 * mpage.h
 */

/*
 * mpage:	A program to reduce pages of print so that several pages
 * 	  	of output appear on one sheet of paper.
 *
 * Copyright (c) 1988 Mark P. Hahn, Herndon, Virginia
 * Copyright (c) 1994-1996 Marcel J.E. Mol, Rijswijk, The Netherlands
 *                    marcel@mesa.nl
 *  
 * Written by:
 *   ...!uunet!\                       Mark Hahn, Sr Systems Engineer
 *              >pyrdc!mark            Pyramid Technology Corporation
 * ...!pyramid!/                       Vienna, Va    (703)848-2050
 *
 *
 *     Permission is granted to anyone to make or distribute verbatim
 *     copies of this document as received, in any medium, provided
 *     that this copyright notice is preserved, and that the
 *     distributor grants the recipient permission for further
 *     redistribution as permitted by this notice.
 *
 */

/*
 * Through-out the program comments I have tried to refer to pages a the
 * logical printed page of text that gets reduced.  Sheets refer to physical
 * pieces of paper.  Hence, mulitple pages appear on a sheet.  "page" is a
 * logical or virtual entity, and "sheet" is physical entity.
 */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

# define	VERSION		"2.4 September 1996"

# define	TRUE		1
# define	FALSE		0

# define	LINESIZE	1024

# define	FILE_EOF	1
# define	FILE_MORE	2

# define	LINE_MORE	5
# define	LINE_EOF_NOW	4
# define	LINE_EOF	3
# define	LINE_OVERPRINT	2
# define	LINE_BLANK	1

# define	TSIZE		12
# define 	HSIZE		TSIZE+2

# define       DEFAULTPMARGIN  4
#if !defined(DEFAULTSMARGIN)
# define        DEFAULTSMARGIN  20
#endif
# define        DEFAULTTMARGIN  0
# define        DEFAULTTABSTOP  8
# define	MAXINT		(1 << 30)
#if !defined(DEFAULT_ENCODING)
# define        DEFAULT_ENCODING  0
#endif

/*
 * to turn on debugging, define the preprocessor macro DEBUG and set
 * the variable Debug_flag to the sum of the sections to debug.
 */
# ifdef DEBUG
# define Debug(f,s,a)	if (Debug_flag & f) printf(s,a)
# define DB_GETLINE	0x0000001
# define DB_ONEPAGE	0x0000002
# define DB_STDIN	0x0000004
# define DB_PSDOC	0x0000008
# define DB_PSPAGE	0x0000010
# define DB_PSCHECK	0x0000020
# define DB_PSROFF	0x0000040
# define DB_PSMPAGE	0x0000080
# define DB_POINTS	0x0000100
# define DB_POST	0x0000200
# define DB_UNUSED	0x0000400
extern int Debug_flag;
extern int errno;
# else
# define Debug(f,s,a)
# endif /* DEBUG */

/*
 * definitions for sorting out types of postscript input
 */
# define	PS_NONE		0
# define	PS_PSROFF	1
# define	PS_MPAGE	2
# define	PS_CONFORM	3
# define	PS_OTHER	4
# define	PS_MSWINDOWS	5
# define	PS_TEX		6


/*
 * Input file type selection types
 */
# define	IN_AUTO		0
# define	IN_ASCII	1
# define	IN_PS		2


/*
 * define page sizes
 */
#define PAGE_A4        1
#define PAGE_LETTER    2
#define PAGE_LEGAL     3
#if !defined(PAGE_DEF)
# define PAGE_DEF	PAGE_A4
#endif

/*
 * define print spooler types
 */
#define ATT_SPOOLER    1
#define BSD_SPOOLER    2
#if !defined(SPOOLER)
# define SPOOLER	BSD_SPOOLER
#endif

/*
 * printing definitions
 */
#if !defined(PRPROG)
# define PRPROG		"pr"
#endif
#if !defined(PRINTPROG)
# if SPOOLER == ATT_SPOOLER
#  define PRINTPROG   "lp"
# else /* BSD_SPOOLER */
#  define PRINTPROG   "lpr"
# endif
#endif
#if !defined(QUEARG)
# if SPOOLER == ATT_SPOOLER
#  define QUEARG              "-d"
# else /* BSD_SPOOLER */
#  define QUEARG              "-P"
# endif
#endif

/*
 * "Conforming" postscript flag string  (remember ps_check strips
 * the "%!" flag from PS files
 */
# define	PS_FLAG		"PS-Adobe-"

/*
 * some basic PS parameters
 */
extern int ps_width;	/* number of points in the X direction (8.5 inches) */
extern int ps_height;	/* number of points in the Y direction (11 inches) */
extern char * media;	/* name of output page media */

/*
 * a sheet describes the measurements and orientatation of a page for
 * use in constructing a sheet preabmles.
 */
struct sheet {
	int sh_cwidth;		/* number of characters across a page */
	int sh_plength;		/* number of lines down a page */
	int (*sh_width)();	/* postscript width across a printed page */
	int (*sh_height)();	/* postscript height of a printed page */
	int sh_rotate;		/* angle to rotate the page */
	void (*sh_outline)();	/* text to print as outline for */
				/*    the printed sheet*/
	struct	pagepoints *sh_pagepoints; /* where to put pages on */
					     /*    the printed sheet */
};

/*
 * simple x and y coordinates for putting pages of output on printed sheet
 * skip to skip this page???
 */
struct pagepoints {
	int (*pp_origin_x)();
	int (*pp_origin_y)();
	int skip;
};

/*
 * Definition of an optional annotate box around part of text
 */
struct pagebox {
	int over; /* over from the left column */
	int lift; /* lift from the bottom line */
	int wide; /* columns wide */
	int high; /* lines high */
	int thick;/* line thickness */
};

/* array of sheets where pages are ordered for coli ??? */
extern struct sheet coli[];

/* array of sheets where pages are ordered for left to right reading */
extern struct sheet *left_right[];

/* arrays for sheets where pages are ordered for top to bottom reading  */
extern struct sheet *up_down[];

/* definitions for aspect and reading directions */
# define PORTRAIT	0
# define LANDSCAPE	1
# define LANDSCAPE_PORTRAIT	2
# define UPDOWN		0
# define LEFTRIGHT	1

/*
 * Variables for holding the chosen options,  The defaults are set here.
 * the sheetlist pointer is set to point to the array for either up/down
 * or left/right reading.  This array is index by sheetorder, and then
 * sheetindex.  sheetindex encodes the number of reduced pages per printed
 * sheet and indexes into the sheet list (0 = 1 page, 1 = two pages, 2 =
 * four pages, 3 = eight pages).
 */
extern struct sheet **sheetlist;/* array of sheet lists (up/down or left/right) */
extern int sheetaspect;		/* either normal or landscape */
extern int sheetorder;		/* up/down or left/right flag */
extern int sheetindex;		/* index to number of pages of sheet */
extern int sheetmargin_left;	/* non-printable border on sheet */
extern int sheetmargin_right;	/* non-printable border on sheet */
extern int sheetmargin_top;	/* non-printable border on sheet */
extern int sheetmargin_bottom;	/* non-printable border on sheet */
extern int pagemargin_left;	/* border for pages */
extern int pagemargin_right;	/* border for pages */
extern int pagemargin_top;	/* border for pages */
extern int pagemargin_bottom;	/* border for pages */
extern int textmargin_left;	/* border for textbox */
extern int textmargin_right;	/* border for textbox */
extern int textmargin_top;	/* border for textbox */
extern int textmargin_bottom;	/* border for textbox */
extern int sheetheader_left;    /* space for physical sheetheader */
extern int sheetheader_right;   /* space for physical sheetheader */
extern int sheetheader_top;     /* space for physical sheetheader */
extern int sheetheader_bottom;  /* space for physical sheetheader */

extern struct pagepoints *points;
extern int orientation;		/* final orientation of text */
extern int fsize;		/* font scale size */
extern int opt_pr;		/* boolean, if true use pr to format output */
extern int opt_tabstop;		/* define tabstop width */
extern int opt_indent;		/* starting column for text printing */
extern int opt_lines;		/* number of lines to fit on an reduced page */
extern int opt_width;		/* number of columns to fit on reduced page */
extern int opt_mp_header;	/* let mapge create a header */
extern int  opt_sheetheader;    /* let mpage create sheetheaders */
extern int opt_page;		/* sheets size: a4 or us letter */
extern int opt_fold;		/* fold long lines */
extern int opt_outline;		/* print a nice outline around pages */
extern int opt_verbose;		/* print a count of pages sent to printer */
extern int opt_square;		/* Make pages with same aspect as sheets */
extern int opt_reverse;		/* Print sheets in reverse order */
extern int opt_first;		/* First sheet # to print, 1 = first */
extern int opt_last;		/* Last sheet # to print, 0 = EOF */
extern int opt_alt;		/* Print every Nth sheet */
extern int opt_file;            /* should each file appera on a new sheet */
extern int opt_duplex;          /* duplex mode*/
extern int opt_tumble;          /* tumble every second pages */
extern int opt_textbox;         /* print a nice box box around text*/
extern int opt_input;           /* set input file type */
extern int opt_encoding;        /* use default encoding or not */

extern struct pagebox textbox;


extern char * opt_header;	/* the header for pr's -h option */
extern char * printque;		/* the printer queuename */
extern char * prprog;		/* the pr filter program */
extern char * printprog;	/* the print program */
extern char * printarg;		/* define print queue to printprog */
extern int    doprint;		/* send output to printer or not */
extern char * charvec_file;	/* file to read character definitions from */
extern char * libdir;		/* pointer to get library files from */
extern char * fontname;		/* Font to use */
extern char * dateformat;	/* Date/time format for headers */
extern char * sheethead;	/* Header for each physical page */


/*
 * various global information
 */
extern char MPAGE[];		/* program name */
extern int ps_pagenum;		/* current page count */
extern int had_ps;              /* did we process ps files */
extern int first_encoding;	/* first encoding in character set */
extern int last_encoding;	/* last encoding in character set */
extern int mpage_level;		/* keep track of multilevel mpaga calls */
extern int Coli;		/* value of 0=don't mess, 1 = 4,1 (outside pages), */


/* args.c */
int do_args();
int do_env();
/* file.c */
void do_file();
void do_pr_file();
void do_stdin();
void do_sheets();
/* glob.c */
void usage();
/* page.c */
void set_page();
int xbase1(), xbase2();
int ybase1(), ybase2(), ybase3(), ybase4();
int ytop1(), ytop2(), ytop3(), ytop4();
int xwid1(), xwid2();
int yht1(), yht2(), yht4();
void outline_1();       
void outline_2();       
void outline_4();       
void outline_8();       
void sheetheader();
void mp_outline();
/* post.c */
int ps_check();
void do_ps_doc();
/* text.c */
void do_text_doc();
