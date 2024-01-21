#ifndef lint
static char SccsCgraphId[] = "%W%  %G%";
#endif

/* Module:	Cgraph.h
 * Purpose:	Define the structs for color graph parameters and some
 *		related constants
 * Modified:	{0} Michael VanHilst	initial version		   9 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#define MAXPOINTS 512		/* COLMAPSZ * 2 */
#define HASH_RAY 2
#define HASH_SZ 4

#define BDRWDTH 1
#define INDENT 3

struct colgRec {		/* ensemble for one color */
  int active;			/* this-color-is-being-manipulated */
  int unset;			/* line-does-not-match-current-state */
  struct subtableRec *table;	/* color information */
  GCspec *draw;			/* info for color gc to draw this graph */
  int active_vertex;		/* vertex being manipulated */
  int active_hash;		/* hash mark being manipulated */
  int hash_0;			/* index in table of hash[0] */
  int queue_index;		/* position in drawing queue */
  int hash_cnt;			/* number of hash marks */
  int pad;			/* allignment space */
  XRectangle hash[PSEUDOSZ];	/* hash marks for X */
  XPoint line[MAXPOINTS];	/* graph line for this color */
};

/* struct for drawing a colorbar (one under display, one under color graph */
struct colbarRec {		/* parameters for color bar */
  Display *display;		/* display handle of colorbox */
  Window ID;			/* handle of bar subwindow */
  int ref_width, ref_height;	/* colorbox size when last checked */
  unsigned int width, height;	/* dimensions of bar subwindow */
  int ncolors;			/* number of shades represented */
  XImage *image;		/* uses image struct of colorbox */
  int data_size;		/* data allocated (both byte and bit) */
  int bytes_per_bit_line;	/* bytes per line for bitmap */
  char *byte_data;		/* one byte per pixel */
  char *bit_data;		/* one bit per pixel (after byte_data */
};

struct cgraphRec {
  int inactive;			/* do-not-draw-graph */
  int vertical;			/* 1=vertical sweep, else horizontal */
  int ncolors;			/* number of image color cells */
  int point_cnt;		/* applies to all color lines */
  int bargraph;			/* use-bar-type-instead-of-line */
  Font font;			/* font id for XDrawImageString */
  XFontStruct *fontstruct;	/* struct for font size info */
  GCspec *disp;			/* gc info to draw bar */
  GCspec *menu;			/* gc info for text */
  GCspec *black;		/* gc info for black text and lines */
  struct colbarRec bar;		/* parameters for color bar */
  struct {			/* parameters to label the bar */
    Window min_ID;		/* lowest color (0) subwindow */
    Window max_ID;		/* highest color (ncolors-1) subwindow */
    int min_x, min_y;		/* lowest color label placement */
    int max_x, max_y;		/* highest color label placement */
    int width, height;		/* size of both label subwindows */
    int base_width;		/* dimension of basic label for centering */
    int base_height;
  } barlabel;
  struct {			/* parameters for color graph */
    Display *display;		/* display handle of graphbox */
    Window ID;			/* handle of graph subwindow */
    int ref_width, ref_height;	/* size of graphbox when last checked */
    int ncolors;		/* number of shades represented */
    int width, height;		/* size of graph subwindow */
    int xzero, yzero;		/* graph area offset */
    int xwidth, yheight;	/* graph area dimensions */
    int xmax, ymax;		/* maximum drawing coordinate */
    double Xwidth, Yheight;	/* often used double version */
    double Xinc, Yinc;		/* color spacing (ncolors/width or height) */
  } graph;
  struct {			/* parameters to label the graph */
    Window gamma_ID;		/* window in which to label gamma */
    Window minmax_ID;		/* window in which to label min & max */
    int width, height;		/* size of both label subwindows */
    Window geq_ID;		/* borderless box with "g=" */
    Window red_ID;		/* red border box for red gamma */
    Window green_ID;		/* green border box for green gamma */
    Window blue_ID;		/* blue border box for blue gamma */
    int box_width, box_height;	/* size of each gamma val boxes */
    int box_x, box_y;		/* text string coord in box */
    int geq_width, geq_height;	/* size of "g=" box */
    int geq_text_x, geq_text_y;	/* text string coord in "g=" box */
    int three_limit;		/* minimum size to hold 3 gamma vals */
    int four_limit;		/* three_limit plus the "g=" */
    int geq_x, geq_y;		/* placement of "g=" box */
    int red_x, red_y;		/* placement of red box */
    int green_x, green_y;	/* placement of green box */
    int blue_x, blue_y;		/* placement of blue box */
    int min_x, min_y;		/* placement of "min" text string */
    int max_x, max_y;		/* placement of "max" text string */
    int minmax_xoff;		/* coordinate offset from right */
    int minmax_yoff;		/* coordinate offset from bottom */
    int active;			/* label the color bar and graph */
    int geq_active;		/* include "g=" in gamma label */
  } graphlabel;
  struct {			/* special values for hash marks */
    double Xzero, Yzero;	/* offset by HASH_RAY */
    double Xwidth, Yheight;	/* decremented by inc */
    double Ymax;		/* perceptually, y goes upward from max */
  } hash;
  struct colgRec red;		/* ensemble for red */
  struct colgRec green;		/* ensemble for green */
  struct colgRec blue;		/* ensemble for blue */
  int queue_cnt;		/* number of color ensembles in queue */
  struct colgRec *queue[3];	/* pointers to establish order of drawing */
};
