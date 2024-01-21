#ifndef lint
static char SccsWndwId[] = "%W%  %G%";
#endif

/* Module:	Window.h
 * Purpose:	Define the struct for each window's parameters and some
 *		related constants
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

/* standard width of auziliary windows */
#define AUXWIDTH	128
/* codes to identify corner to be anchored when making a window adjustment */
#define ULCORNER	1
#define LLCORNER	2
#define URCORNER	3
#define LRCORNER	4
/* codes to indicate type of window adjustment */
#define ADJ_MOVE	1
#define ADJ_RESIZE	2
/* indices for array of adjustment parameters */
#define ADJ_REF		0
#define ADJ_X		1
#define ADJ_Y		2
#define ADJ_W		3
#define ADJ_H		4

/*
 *  struct windowRec
 *
 *  record structure with useful parameters for a window
 */
struct windowRec {
  Window ID;			/* window's nom de plume (ptr) */
  Display *display;		/* display server handle */
  int screen;			/* display screen number */
  Window parent;		/* parent of this window */
  unsigned int depth;		/* window depth */
  int active;			/* flag to say window is there */
  int x, y;			/* upper left screen coords */
  unsigned int width, height;	/* window width (-1 border) */
  unsigned int bdrwidth;	/* width of the border */
  int bdrtotal;			/* width added by both borders */
  int rightx, lowery;		/* useful screen coordinates */
  int xzero, yzero;		/* u-l corner of drawing area */
  int xwidth, yheight;		/* size of drawing area */
  int moored;			/* flag to move window with disp */
  int data_size;		/* byte size of image data buf */
  XImage image;			/* for XPutImage */
  Visual *visual;		/* visual type */
  unsigned long valuemask;	/* mask to set attributes */
  XSetWindowAttributes attrs;	/* for XCreateWindow */
  XSizeHints hints;		/* for XSetNormalHints */
};

/*
 * typical attributes neeeding to be set:
 *  attrs.cursor = [window cursor];
 *  attrs.background_pixel = [window backcolor];
 *  attrs.event_mask = [window eventmask];
 *  valuemask = CWBackPixel | CWCursor;		(event_mask set separately)
 */

/*
 * typical hints to set and use:
 *  hints.flags = USPosition | USSize | PMinSize;
 *  hints.x =
 *  hints.y =
 *  hints.width =
 *  hints.height =
 *  hints.min_width =
 *  hints.min_height =
 */

#ifdef SAMPLE
struct windowRec foobox = {
 0,		/* Window ID;			window's nom de plume (ptr) */
 NULL,		/* Display *display;		display server handle */
 0,		/* int screen;			display screen number */
 0,		/* Window parent;		parent of this window */
 0,		/* unsigned int depth;		window depth */
 0,		/* int active;			flag to say window is there */
 0, 0,		/* int x, y;			upper left screen coords */
 0, 0,		/* unsigned int width, height;	window width (-1 border) */
 3,		/* unsigned int bdrwidth;	width of the border */
 6,		/* int bdrtotal;		width added by both borders */
 0, 0,		/* int rightx, lowery;		useful screen coordinates */
 0, 0,		/* int xzero, yzero;		u-l corner of drawing area */
 0, 0,		/* int xwidth, yheight;		size of drawing area */
 1,		/* int moored;			move window with disp box */
 0,		/* int data_size;		byte size of image data buf */
 {		/* XImage image;		for XPutImage */
   0, 0,	/*  int width, height;			0, 0 */
   0,		/*  int xoffset;			0 */
   0,		/*  int format;				XYBitmap, XY/ZPixmap */
   NULL,	/*  char *data;				NULL */
   LSBFirst,	/*  int byte_order;			LSBFirst */
   8,		/*  int bitmap_unit;			8 */
   LSBFirst,	/*  int bitmap_bit_order;		LSBFirst */
   8,		/*  int bitmap_pad;			8 */
   0,		/*  int depth;				1 or runtime set */
   0,		/*  int bytes_per_line;			0 */
   8,		/*  int bits_per_pixel;			1 or 8 */
   0, 0, 0,	/*  unsigned long red_mask, green_mask, blue_mask; runtime */
   NULL,	/*  char *obdata;			NULL */
   { 0 } },	/*  { struct func f };			{ NULL } */
 NULL,		/* Visual *visual;		visual type */
 0,		/* unsigned long valuemask;	mask to set attributes */
 {		/* XSetWindowAttributes attrs;	for XCreateWindow */
   0,		/*  Pixmap background_pixmap;		CWBackPixmap */
   0,		/*  unsigned long background_pixel;	CWBackPixel */
   0,		/*  Pixmap border_pixmap;		CWBorderPixmap */
   0,		/*  unsigned long border_pixel		CWBorderPixel */
   0,		/*  int bit_gravity;			CWBitGravity */
   0,		/*  int win_gravity;			CWWinGravity */
   0,		/*  backing_store;			CWBackingStore */
   0,		/*  unsigned long backing_planes;	CWBackingPlanes */
   0,		/*  unsigned long backing_pixel;	CWBackingPixel */
   0,		/*  Bool save_under;			CWSaveUnder */
   0,		/*  long event_mask;			CWEventMask */
   0,		/*  long do_not_propogate_mask;		CWDontPropagate */
   0,		/*  Bool override_redirect;		CWOverrideRedirect */
   0,		/*  Colormap colormap;			CWColormap */
   0 },		/*  Cursor cursor;			CWCursor */
 {	 	/* XSizeHints hints;		for XSetNormalHints */
   0,		/*  long flags;				defined fields */
   0, 0,	/*  int x, y;				USPosition PPosition */
   0, 0,	/*  int width, height;			USSize PSize */
   0, 0,	/*  int min_width, min_height;		PMinSize */
   0, 0,	/*  int max_width, max_height;		PMaxSize */
   0, 0,	/*  int width_inc, height_inc;		PResizeInc */
   { 0 },	/*  { int min_aspect.x, min_aspect.y }	PAspect */
   { 0 } }	/*  { int max_aspect.x, max_aspect.y }	 x/y=ratio */
}
#endif

#ifdef NOTES
application notes: What Must Be Set At Runtime, When Exactly and Where

init_time:
	- init_main
		display, screen,
	- init_color
		visual, depth, ximage.depth, attrs.colormap, ximage.format,
	- init_mousecursors
		attrs.cursor,
	- init_window
		attrs.background_pixel, attrs.border_pixel,
		valuemask,
		hints.x, hints.y, hints.width, hints.height,
		bdrwidth, bdrtotal,
		x, y, width, height, xwidth, yheight, active,
		ximage.width, ximage.height, ximage.bytes_per_line,
	- init_buffer
		data_buf, buf_size, ximage.data,
	- init_drawing	 	
		gc,

change_colormode:
		ximage.format, ximage.bytes_per_line,
		[attrs.colormap, data_buf, buf_size, ximage.data],

change_window:
		x, y, width, height, xwidth, yheight,
		data_buf, buf_size, ximage.data,
		ximage.width, ximage.height, ximage.bytes_per_line,

map/unmap_window:
		active,
#endif
