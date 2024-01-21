/*
 * glob.c
 */

/*
 * mpage:	a program to reduce pages of print so that several pages
 * 	  	of output appear on one printed page.
 *
 * Written by:
 *   ...!uunet!\                       Mark Hahn, Sr Systems Engineer
 *              >pyrdc!mark            Pyramid Technology Corporation
 * ...!pyramid!/                       Vienna, Va    (703)848-2050
 *
 *
 * Copyright (c) 1988 Mark P. Hahn, Herndon, Virginia
 * Copyright (c) 1994-1996 Marcel J.E. Mol, Rijswijk, The Netherlands
 *                    marcel@mesa.nl
 *  
 *     Permission is granted to anyone to make or distribute verbatim
 *     copies of this document as received, in any medium, provided
 *     that this copyright notice is preserved, and that the
 *     distributor grants the recipient permission for further
 *     redistribution as permitted by this notice.
 *
 */

#include "mpage.h"

/*
 * to turn on debugging, define the preprocessor macro DEBUG and set
 * the variable Debug_flag to the sum of the sections to debug.
 */
# ifdef DEBUG
int Debug_flag = DB_PSMPAGE;
# endif


/*
 * some basic PS parameters
 */
int ps_width;	/* number of points in the X direction see set_page() */
int ps_height;	/* number of points in the Y direction */
char *media;	/* name of output media */

/*
 * the structures describe where to put the reduced pages of output on the
 * printed page.
 */
/* empty page */
struct pagepoints points_empty[] = {
	{  0,  0,  0 }
};
/* base point for one page, normal aspect */
struct pagepoints one_normal[] = {
	{ xbase1, ybase1,  0 },
	{  0,  0,  0 }
};
/* base points for two pages, normal aspect */
struct pagepoints two_normal[] = {
	{ xbase1, ytop4,  0 },	{ xbase1 , ytop2,  0 },
	{  0,  0,  0 }
};

/* GPN outside 2 pages */
struct pagepoints two_normal_co[] =   {
	   {xbase1, ytop2, 0},	{0, 0, 1},
	   {0,  0,  1},		{xbase1, ytop4, 0},
	   {0,  0,  0}

};
/* GPN. inside 2 pages */
struct pagepoints two_normal_ci[] =   {
	   {0, 0, 1},		{xbase1, ytop4, 0},
	   {xbase1, ytop2, 0},	{0, 0, 1},
	   {0,  0,  0}
};

/* base points for four pages, normal aspect, running reduced pages
 * read from left to right */
struct pagepoints lr_four_normal[] = {
  	{ xbase1, ybase3,  0 },	{ xbase2, ybase3,  0 },
	{ xbase1, ybase1,  0 },	{ xbase2, ybase1,  0 },
	{  0,  0,  0 }
};
/* base points for four pages, normal aspect, running reduced pages
 * read from top to bottom (up/down) */
struct pagepoints ud_four_normal[] = {
  	{ xbase1, ybase3,  0 },	{ xbase1, ybase1,  0 },
	{ xbase2, ybase3,  0 },	{ xbase2, ybase1,  0 },
	{  0,  0,  0 }
};
/* base points for four pages, normal aspect, running reduced pages
 * read from left to right, adjusting for the fact that we have a landscape
 * input */
struct pagepoints land_lr_four_normal[] =
{
	{ xbase1, ybase1,  0 }, { xbase1, ybase3,  0 },
	{ xbase2, ybase1,  0 }, { xbase2, ybase3,  0 },
	{ 0,  0,  0}
};
/* base points for four pages, normal aspect, running reduced pages
 * read from top to bottom (up/down), adjusting for the fact that we have a
 * landscape input */
struct pagepoints land_ud_four_normal[] =
{
	{ xbase1, ybase1,  0 }, { xbase2, ybase1,  0 },
	{ xbase1, ybase3,  0 }, { xbase2, ybase3,  0 },
	{ 0,  0,  0}
};
/* base points for eight pages, normal aspect, running reduced pages
 * read from left to right */
struct pagepoints lr_eight_normal[] = {
	{ xbase2, ytop4,  0 },	{ xbase2, ytop3,  0 },
	{ xbase2, ytop2,  0 },	{ xbase2, ytop1,  0 },
	{ xbase1, ytop4,  0 },	{ xbase1, ytop3,  0 },
	{ xbase1, ytop2,  0 },	{ xbase1, ytop1,  0 },
	{  0,  0,  0 }
};
/* base points for eight pages, normal aspect, running reduced pages
 * read from top to bottom (up/down) */
struct pagepoints ud_eight_normal[] = {
	{ xbase2, ytop4,  0 },	{ xbase1, ytop4,  0 },
	{ xbase2, ytop3,  0 },	{ xbase1, ytop3,  0 },
	{ xbase2, ytop2,  0 },	{ xbase1, ytop2,  0 },
	{ xbase2, ytop1,  0 },	{ xbase1, ytop1,  0 },
	{  0,  0,  0 }
};
/* base points for eight pages, normal aspect, running reduced pages
 * read from left to right, adjusting for the fact that we have a landscape
 * input */
struct pagepoints land_lr_eight_normal[] =
{
	{ xbase1, ytop4,  0 },  { xbase2, ytop4,  0 },
	{ xbase1, ytop3,  0 },  { xbase2, ytop3,  0 },
	{ xbase1, ytop2,  0 },  { xbase2, ytop2,  0 },
	{ xbase1, ytop1,  0 },  { xbase2, ytop1,  0 },
	{ 0,  0,  0 }
};
/* base points for eight pages, normal aspect, running reduced pages
 * read from top to bottom (up/down), adjusting for the fact that we have a
 * landscape input */
struct pagepoints land_ud_eight_normal[] =
{
	{ xbase1, ytop4,  0 },  { xbase1, ytop3,  0 },
	{ xbase1, ytop2,  0 },  { xbase1, ytop1,  0 },
	{ xbase2, ytop4,  0 },  { xbase2, ytop3,  0 },
	{ xbase2, ytop2,  0 },  { xbase2, ytop1,  0 },
	{ 0,  0,  0}
};
/* base point for one page, in landscape */
struct pagepoints one_landscape[] = {
  	{ xbase1, ytop4,  0 },
	{  0,  0,  0 }
};
/* base points for two pages, in landscape */
struct pagepoints two_landscape[] = {
  	{ xbase1, ybase3,  0 },	{ xbase1, ybase1,  0 },
	{  0,  0,  0 }
};
/* base points for four pages, in landscape, running reduced pages
 * read from left to right */
struct pagepoints lr_four_landscape[] = {
  	{ xbase2, ytop4,  0 },	{ xbase2, ytop2,  0 },
	{ xbase1, ytop4,  0 },	{ xbase1, ytop2,  0 },
	{  0,  0,  0 }
};
/* base points for four pages, in landscape, running reduced pages
 * read from top to bottom (up/down) */
struct pagepoints ud_four_landscape[] = {
  	{ xbase2, ytop4,  0 },	{ xbase1, ytop4,  0 },
  	{ xbase2, ytop2,  0 },	{ xbase1, ytop2,  0 },
	{  0,  0,  0 }
};
/* base points for eight pages, in landscape, running reduced pages
 * read from left to right */
struct pagepoints lr_eight_landscape[] = {
	{ xbase1, ybase4,  0 },	{ xbase2, ybase4,  0 },
	{ xbase1, ybase3,  0 },	{ xbase2, ybase3,  0 },
	{ xbase1, ybase2,  0 },	{ xbase2, ybase2,  0 },
	{ xbase1, ybase1,  0 },	{ xbase2, ybase1,  0 },
	{  0,  0,  0 }
};
/* base points for eight pages, in landscape, running reduced pages
 * read from top to bottom (up/down) */
struct pagepoints ud_eight_landscape[] = {
	{ xbase1, ybase4,  0 },	{ xbase1, ybase3,  0 },
	{ xbase1, ybase2,  0 },	{ xbase1, ybase1,  0 },
	{ xbase2, ybase4,  0 },	{ xbase2, ybase3,  0 },
	{ xbase2, ybase2,  0 },	{ xbase2, ybase1,  0 },
	{  0,  0,  0 }
};

/* list of sheets (printed page formats) for
 * left to right reading, in normal aspect */
struct sheet lr_normal[] = {
/* 0 */	{ 80, 66, xwid1, yht1,    0, outline_1, one_normal },
/* 1 */	{ 80, 66, yht2,  xwid1, -90, outline_2, two_normal },
/* 2 */	{ 80, 66, xwid2, yht2,    0, outline_4, lr_four_normal },
/* 3 */	{ 80, 66, yht4,  xwid2, -90, outline_8, lr_eight_normal },
};

/* list of sheets (printed page formats) for landscape input
 * left to right reading, in normal aspect */
struct sheet land_lr_normal[] = {
/* 0 */ { 80, 66, xwid1, yht1,    0, outline_1, one_normal },
/* 1 */ { 80, 66, yht2,  xwid1, -90, outline_2, two_normal },
/* 2 */ { 80, 66, xwid2, yht2,    0, outline_4, land_lr_four_normal },
/* 3 */ { 80, 66, yht4,  xwid2, -90, outline_8, land_lr_eight_normal },
};

/* list of sheets (printed page formats) for
 * top to bottom reading, in normal aspect */
struct sheet ud_normal[] = {
/* 0 */	{ 80, 66, xwid1, yht1,    0, outline_1, one_normal },
/* 1 */	{ 80, 66, yht2,  xwid1, -90, outline_2, two_normal },
/* 2 */	{ 80, 66, xwid2, yht2,    0, outline_4, ud_four_normal },
/* 3 */	{ 80, 66, yht4,  xwid2, -90, outline_8, ud_eight_normal },
};

/* list of sheets (printed page formats) for
 * left to right reading, in landscape */
struct sheet lr_landscape[] = {
/* 0 */	{ 132, 52, yht1,  xwid1, -90, outline_1, one_landscape },
/* 1 */	{ 132, 52, xwid1, yht2,    0, outline_2, two_landscape },
/* 2 */	{ 132, 52, yht2,  xwid2, -90, outline_4, lr_four_landscape },
/* 3 */	{ 132, 52, xwid2, yht4,    0, outline_8, lr_eight_landscape },
};

/* list of sheets (printed page formats) for
 * top to bottom reading, in landscape */
struct sheet ud_landscape[] = {
/* 0 */	{ 132, 52, yht1,  xwid1, -90, outline_1, one_landscape },
/* 1 */	{ 132, 52, xwid1, yht2,    0, outline_2, two_landscape },
/* 2 */	{ 132, 52, yht2,  xwid2, -90, outline_4, ud_four_landscape },
/* 3 */	{ 132, 52, xwid2, yht4,    0, outline_8, ud_eight_landscape },
};
	      
/* list of sheets (printed page formats) for landscape input
 * top to bottom reading, in landscape */
struct sheet land_ud_normal[] = {
/* 0 */ { 80, 66, xwid1, yht1,    0, outline_1, one_normal },
/* 1 */ { 80, 66, yht2,  xwid1, -90, outline_2, two_normal },
/* 2 */ { 80, 66, xwid2, yht2,    0, outline_4, land_ud_four_normal },
/* 3 */ { 80, 66, yht4,  xwid2, -90, outline_8, land_ud_eight_normal },
};

/* GPN. sheet */
struct sheet coli [] =   {
  	/* 1 */ { 80, 66, yht2,  xwid1, -90, outline_2, two_normal_co },
  	/* 1 */ { 80, 66, yht2,  xwid1, -90, outline_2, two_normal_ci },
};

/* array of sheet lists for left to right reading printed pages */
struct sheet *left_right[] = {
	lr_normal,
	lr_landscape,
	land_lr_normal
  };

/* arrays for top to bottom reading printed pages */
struct sheet *up_down[] = {
	ud_normal,
	ud_landscape,
	land_ud_normal
  };

/*
 * Variables for holding the chosen options,  The defaults are set here.
 * the sheetlist pointer is set to point to the array for either up/down
 * or left/right reading.  This array is index by sheetorder, and then
 * sheetindex.  sheetindex encodes the number of reduced pages per printed
 * page and indexes into the sheet list (0 = 1 page, 1 = two pages, 2 =
 * four pages, 3 = eight pages).
 */
struct sheet **sheetlist;/* array of sheet lists (up/down or left/right) */
int sheetaspect = PORTRAIT;		/* either normal or landscape */
int sheetorder = UPDOWN;		/* up/down or left/right flag */
int sheetindex = 2;	/* index to number of pages of output per printed */
int sheetmargin_left  = DEFAULTSMARGIN;/* non-printable border on sheet */
int sheetmargin_right = DEFAULTSMARGIN;/* non-printable border on sheet */
int sheetmargin_top   = DEFAULTSMARGIN;/* non-printable border on sheet */
int sheetmargin_bottom= DEFAULTSMARGIN;/* non-printable border on sheet */
int pagemargin_left   = DEFAULTPMARGIN;/* border for pages */
int pagemargin_right  = DEFAULTPMARGIN;/* border for pages */
int pagemargin_top    = DEFAULTPMARGIN;/* border for pages */
int pagemargin_bottom = DEFAULTPMARGIN;/* border for pages */
int textmargin_left   = DEFAULTTMARGIN;/* border for textbox */
int textmargin_right  = DEFAULTTMARGIN;/* border for textbox */
int textmargin_top    = DEFAULTTMARGIN;/* border for textbox */
int textmargin_bottom = DEFAULTTMARGIN;/* border for textbox */
int sheetheader_left  = 0;   /* space for physical sheetheader */
int sheetheader_right = 0;   /* space for physical sheetheader */
int sheetheader_top   = 0;   /* space for physical sheetheader */
int sheetheader_bottom= 0;   /* space for physical sheetheader */
struct pagepoints *points = points_empty;
int orientation;			/* final orientation of text */
int fsize = TSIZE;			/* font scale size */
int opt_indent = 0;			/* starting column for ascii printing */
int opt_tabstop = DEFAULTTABSTOP;	/* width of a tab */
int opt_lines = 0;		/* number of lines to fit on an reduced page */
int opt_width = 0;	/* number of columns to fit on an reduced page */
int opt_last = MAXINT;	/* print as many as supplied */
int opt_page = PAGE_DEF;/* default paper size */
/* boolean's: set default to 0 or 1 */
int opt_pr = 0;		/* if true use pr(1) to format output */
int opt_mp_header = 0;  /* let mpage create headers */
int opt_sheetheader = 0;/* let mpage create sheetheaders */
int opt_fold = 0;	/* fold lines longer than page width */
int opt_outline = 1;	/* don't normally outline the pages */
int opt_verbose = 0;	/* by default, print a count of pages produced */
int opt_square = 1;	/* by default print pages with natural aspect ratio */
int opt_reverse = 0;	/* by default print sheets in forward order */
int opt_first = 1;	/* start with first sheet */
int opt_alt = 1;	/* by default print all sheets, odd+even */
int opt_file = 1;       /* should each file appera on a new sheet */
int opt_duplex = 0;     /* duplex mode flag, OFF by default */
int opt_tumble = 0;     /* tumble overy second pages, OFF by default */
int opt_textbox = 0;    /* don't normally draw box around text */
int opt_input = IN_AUTO;/* select input file format */
int opt_encoding = DEFAULT_ENCODING;	/* use default encoding or not */
struct pagebox textbox = {0, 0, 80, 66, 0};

char * opt_header = NULL;	/* the header for pr's -h option */
char * printque = NULL;	/* the printer queuename */
char * prprog = PRPROG;	/* the pr filter program */
char * printprog = PRINTPROG; /* the print program */
char * printarg = QUEARG; /* define print queue to printprog */
int doprint = 0;	/* send it to the printer */
char * charvec_file;	/*  file to read character definitions from */
char * libdir = LIBDIR;	/* pointer to get library files from */
char * fontname = "Courier";	/* Font to use */
char * dateformat = "%c";	/* Date/time format for date in headers */
char * sheethead = "";	/* Leave empty to get default sheetheader:
                                    current filename + physical pagenumber */

/*
 * various global information
 */
char MPAGE[] = "mpage";	/* program name */
int ps_pagenum = 0;	/* current sheet count */
int had_ps = 0;         /* did we process ps files */

int first_encoding = -1; /* first encoding in character set */
int last_encoding;	/* last encoding in character set */

/* GPN. for coli */
int Coli = 0;           /* value of 0=don't mess, 1 = 4,1 (outside pages), */


void usage(int errcode)
{
    fprintf(stderr, "\
 mpage - print multiple pages on postscript, version %s\n\n\
 mpage [-acflortvEOHRST1248] [-d(a|p)] [-B[textboxmargins]]\n\
       [-m[sheetmargins]] [-M[pagemargins]] [-p[prprog]] [-j<pages>]\n\
       [-Iindent] [-Llines] [-Wwidth] [-hheader] [-P[printque]]\n\
       [-Fpsfontname] [-C[encodingfile]] [-zprintcommand] [-Zqueuearg]\n\
       [-Ddateformat] [-stabstop] -X[header]] [-bpapersize] [files...]\n",
 VERSION
);

    fprintf(stderr, "\n\
 -1, -2, -4, -8  Pages per sheet (4)    -D strftime format for date specs\n\
 -da Force ascii input format           -dp Force postscript input format\n\
 -a Toggle across/updown layout (u)	-l Toggle portrait/landscape (p)\n\
 -f Toggle folding long lines (off)	-o Toggle printing outlines (on)\n\
 -r Reverse printing, last->first sheet	-v Toggle verbose output, (on)\n\
 -F Text font to be used (Courier)      -C Character encoding filename\n\
 -E Print every second and third page   -O Print every first and fourth page\n\
 -b papersize: A4|Letter|Legal (default %s)\n\
 -s Define tabstop width (default %d)\n\
 -R Switch to across mode, with first page at lower left corner\n\
 -H Create page header for each page (text files only)\n\
 -X Print physical page header (default filename + physical pagenumber)\n\
 -c Toggle concat pages of different files on same sheet (off)\n\
 -S Don't square pages (default uniform X/Y shrink, postscript files only)\n\
 -B Specify textbox margin/thickness (no space): [<num>[lrtb]*]*\n\
 -m Specify sheetmargin (no space): [<num>[lrtb]*]*\n\
 -M Specify pagemargins (no space): [<num>[lrtb]*]*\n\
 -p Pipe through prprog (no space), pr(1) is default.\n\
    Mpage assumes the specified pr understands -l, -w and -h options.\n\
 -P Specify printer queue (no space). -P only uses default queue.\n\
 -z Specify print command (%s).\n\
 -Z Specify print command print queue option (%s).\n\
 -j Print specified sheets: -j first[-last][%%interval]\n\
    -j 1-10 does first 10 sheets, -j 1%%2 prints odd ones, -j 2%%2 even ones.\n\
 -t Toggle printing both sides of the paper (Duplex mode, off)\n\
 -T Toggle tumble of every second pages when printing in duplex mode (off)\n",
#if   PAGE_DEF == PAGE_A4
        "A4"
#elif PAGE_DEF == PAGE_LETTER
        "US letter"
#elif PAGE_DEF == PAGE_LEGAL
        "US legal"
#else
        "unknown???"
#endif
    , opt_tabstop, printprog, printarg
    );
    fprintf(stderr, "\n(c) 1993-1996 Marcel Mol, marcel@mesa.nl (MESA Consulting)\n");

    return;

} /* usage */
