#ifndef lint
static char SccsMagnifyId[] = "%W%  %G%";
#endif

/* Module:	Magnify.h
 * Purpose:	Quick access list of parameters for the magnifier window
 * Modified:	{0} Michael VanHilst	initial version		  6 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

struct magRec {
  struct {
    Display *display;		/* display for magnibox */
    Window ID;			/* magnibox drawable */
    int x, y;			/* drawing position in magnibox */
    int width, height;		/* drawing dimensions of magnibox */
  } win;
  struct {
    short *shortbuf;		/* pointer at image data buffer */
    int width;			/* width of image data buffer */
    float X, Y;			/* most recent magnified coordinates */
    float X1lim;		/* min value of bufx to catch any data */
    float X2lim;		/* max value of bufx to catch any data */
    float X2bdr;		/* max value of bufx before showing edge */
    float Y1lim;		/* min value of bufy to catch any data */
    float Y2lim;		/* max value of bufy to catch any data */
    float Y2bdr;		/* max value of bufy before showing edge */
    float Xcen;			/* left edge to hot spot in buf coords */
    float Ycen;			/* top edge to hot spot in buf coords */
  } buf;
  int view;			/* turn on magnifier tracking */
  int label;			/* turn on label tracking */ 
  XImage *image;		/* X image drawing structure */
  GCspec *gcset_disp;		/* GC values for image drawing */
  GCspec *gcset_aim;		/* for aim mark foreground and background */
  GCspec *gcset_label;		/* for the text label */
  int data_size;		/* buffer size */
  char *data;			/* drawing image buffer (image->data) */
  int data_x_hot, data_y_hot;	/* coords of hot spot in data */
  unsigned char *lookup;	/* signed indexing pointer at scalemap */
  int halftone;			/* flag if halftone display */
  int magnify;			/* disp to magnibox zoom */
  int zoom_rep;			/* 1/blocking factor buffer to magnibox */
  int bytes_per_line;		/* num of shorts in one row of bitmap */
  int inverse;			/* dithering positive or inverse color */
  int bitmap_size;		/* number of bytes used for bitmap */
  short *matrix;		/* dithering matrix (16x16) */
  short *matrix_end;		/* first element beyond end of matrix */
  struct {
    int x_x, y_x, val_x;	/* x coord for x & y coords and val */
    int y;			/* y coord of x,y,val string */
    int proportional;		/* flag that font is proportional */
    int width;			/* width of entire string */
    int space;			/* width of one character */
    int dot;
    int dash;
    int e;
    int x_xoff;			/* offset from left to beginning of string */
    int y_xoff, val_xoff;	/* offsets from x to next two numbers */
    int yoff;			/* y offset from bottom */
    XFontStruct *fontstruct;	/* font info for measuring drawing text */
    Font font;
    unsigned long foreground;	/* text drawing colors */
    unsigned long background;
    int numsz[10];
  } text;
};
