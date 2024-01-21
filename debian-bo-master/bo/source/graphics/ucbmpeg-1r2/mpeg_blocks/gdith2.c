/*
 * Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */
#include <math.h>
#include "video.h"
#include "proto.h"
#include "dither.h"
#include "rgbl.h"
#include "show.h"
#include "tcl.h"
#include "tk.h"


#define STATUS_TOP  20


/* Range values for lum, cr, cb. */
int LUM_RANGE;
int CR_RANGE;
int CB_RANGE;

/* Array that remaps color numbers to actual pixel values used by X server. */

unsigned char pixel[256];

/* Arrays holding quantized value ranged for lum, cr, and cb. */

int *lum_values;
int *cr_values;
int *cb_values;

/* Declaration of global variable containing dither type. */

extern int ditherType;

/* Structures used by the X server. */

Display *display;

static XImage *ximage = NULL;
static Colormap cmap;
Window window;
static GC gc;
static int displayX, displayY;
static int firstFrame;
float framesPerSecond;
int	windowReady = FALSE;
int	imageHeight = 0, imageWidth = 0;
int windowWidth, windowHeight;
static int centerWinWidth;
int origHeight, origWidth;


void	FramesToStopWatch _ANSI_ARGS_((int numFrames, char *text));
void DecodeFPS _ANSI_ARGS_((VidStream *vid_stream));
void ShowStatusOutline _ANSI_ARGS_((void));
void	ShowFrameInfo _ANSI_ARGS_((void));
void ShowFPS _ANSI_ARGS_((void));
void ResizeWindow _ANSI_ARGS_((void));
void InitXImage _ANSI_ARGS_((void));
void	ScaleImage _ANSI_ARGS_((char *orig, char *image));


XImage	*GetImage()
{
    return ximage;
}


void	SetFirstFrame()
{
    firstFrame = TRUE;
}


void	SetDisplayPosition(x, y)
    int x;
    int y;
{
    displayX = x;  
    displayY = y;
}


void FindWindow(browserWindow)
    Window browserWindow;
{
    window = browserWindow;
}


void FindGC(browserGC)
    GC browserGC;
{
    gc = browserGC;
}


/*
 *--------------------------------------------------------------
 *
 * InitColor --
 *
 *	Initialized lum, cr, and cb quantized range value arrays.
 *
 * Results: 
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void
InitColor()
{
  int i;

  for (i=0; i<LUM_RANGE; i++) {
    lum_values[i] = ((i * 256) / (LUM_RANGE)) + (256/(LUM_RANGE*2));
  }

  for (i=0; i<CR_RANGE; i++) {
    cr_values[i] = ((i * 256) / (CR_RANGE)) + (256/(CR_RANGE*2));
  }

  for (i=0; i<CB_RANGE; i++) {
    cb_values[i] = ((i * 256) / (CB_RANGE)) + (256/(CB_RANGE*2));
  }

}


/*
 *--------------------------------------------------------------
 *
 * ConvertColor --
 *
 *	Given a l, cr, cb tuple, converts it to r,g,b.
 *
 * Results:
 *	r,g,b values returned in pointers passed as parameters.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static void
ConvertColor(l, cr, cb, r, g, b)
     unsigned char l, cr, cb;
     unsigned char *r, *g, *b;
{
  double fl, fcr, fcb, fr, fg, fb;

  fl = (double) l;
  fcr =  ((double) cr) - 128.0;
  fcb =  ((double) cb) - 128.0;


  fr = fl + (1.36600 * fcb);
  fg = fl - (0.70000 * fcb) - (0.33400 * fcr);
  fb = fl + (1.73200 * fcr);
 
/*
  fr = fl + (1.40200 * fcb);
  fg = fl - (0.71414 * fcb) - (0.34414 * fcr);
  fb = fl + (1.77200 * fcr);
*/

  if (fr < 0.0) fr = 0.0;
  else if (fr > 255.0) fr = 255.0;

  if (fg < 0.0) fg = 0.0;
  else if (fg > 255.0) fg = 255.0;

  if (fb < 0.0) fb = 0.0;
  else if (fb > 255.0) fb = 255.0;

  *r = (unsigned char) fr;
  *g = (unsigned char) fg;
  *b = (unsigned char) fb;

}

#ifdef SH_MEM

int gXErrorFlag = 0;

int HandleXError(dpy, event)
     Display *dpy;
     XErrorEvent *event;
{
  gXErrorFlag = 1;

  return 0;
}

void InstallXErrorHandler()
{
  int HandleXError();

  XSetErrorHandler(HandleXError);
  XFlush(display);
}

void DeInstallXErrorHandler()
{
  XSetErrorHandler(NULL);
  XFlush(display);
}
#endif


/*
 *--------------------------------------------------------------
 *
 * MakeWindow --
 *
 *	Create X Window
 *
 * Results:
 *	Read the code.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

#ifdef SH_MEM
int CompletionType = -1;
#endif

static void 
MakeWindow(name) 
char *name;
{
  
  XSizeHints hint;
  unsigned int fg, bg;
  char *hello = "MPEG Show";
  int screen;
  Window CreateFullColorWindow();

  if (ditherType == NO_DITHER) return;

  display = XOpenDisplay(name);
  if (display == NULL) {
    fprintf(stderr, "Can not open display\n");
    exit(-2);
  }

#ifdef SH_MEM
  if(shmemFlag)
    CompletionType = XShmGetEventBase(display) + ShmCompletion;
#endif

  screen = DefaultScreen (display);
  
  /* Fill in hint structure */

  hint.x = 200;
  hint.y = 300;
  hint.width = 150;
  hint.height = 150;
  hint.flags = PPosition | PSize;
  
  /* Get some colors */
  
  bg = WhitePixel (display, screen);
  fg = BlackPixel (display, screen);
  
  /* Make the window */
  
  if (ditherType == FULL_COLOR_DITHER) {
    window = CreateFullColorWindow (display, hint.x, hint.y, hint.width, hint.height);
    if (window == 0) {
      fprintf (stderr, "-color option only valid on full color display\n");
      exit (-1);
    }
  } else if (ditherType == MONO_DITHER || ditherType == MONO_THRESHOLD ||
	     ditherType == MONO_FS4_DITHER || ditherType == HALFTONE_DITHER) {
    window = XCreateSimpleWindow (display,
				  DefaultRootWindow (display),
				  hint.x, hint.y,
				  hint.width, hint.height,
				  4, fg, bg);
  } else {
    XVisualInfo vinfo;
    
    if (!XMatchVisualInfo (display, screen, 8, PseudoColor, 
			   &vinfo)) {

      if (!XMatchVisualInfo(display, screen, 8, GrayScale, 
			    &vinfo)) {

	fprintf(stderr, "-requires 8 bit display\n");
	exit(-1);
      }
    }

    window = XCreateSimpleWindow (display,
				 DefaultRootWindow (display),
				 hint.x, hint.y,
				 hint.width, hint.height,
				 4, fg, bg);
  }
  
  XSelectInput(display, window, StructureNotifyMask);

  /* Tell other applications about this window */
  
  XSetStandardProperties (display, window, hello, hello, None, NULL, 0, &hint);
  
  /* Map window. */

  XMapWindow(display, window);

  /* Wait for map. */
  while(1) {
    XEvent	xev;

    XNextEvent(display, &xev);
    if(xev.type == MapNotify && xev.xmap.event == window)
      break;
  }
  XSelectInput(display, window, NoEventMask);
}
  

/*
 *--------------------------------------------------------------
 *
 * InitDisplay --
 *
 *	Initialized display, sets up colormap, etc.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void InitDisplay(name)
char *name;
{
  int ncolors = LUM_RANGE*CB_RANGE*CR_RANGE;
  XColor xcolor;
  int i, lum_num, cr_num, cb_num;
  unsigned char r, g, b;
  Colormap dcmap;

  if (ditherType == NO_DITHER) return;

  dcmap = cmap = XDefaultColormap(display, DefaultScreen(display));

  xcolor.flags = DoRed | DoGreen | DoBlue;

  retry_alloc_colors:
  for (i=0; i<ncolors; i++) {

    lum_num = (i / (CR_RANGE*CB_RANGE))%LUM_RANGE;
    cr_num = (i / CB_RANGE)%CR_RANGE;
    cb_num = i % CB_RANGE;

    ConvertColor(lum_values[lum_num], cr_values[cr_num], cb_values[cb_num], &r, &g, &b);

    xcolor.red = r * 256;
    xcolor.green = g * 256;
    xcolor.blue = b * 256;

    if(XAllocColor(display, cmap, &xcolor) == 0 && cmap == dcmap) {
      int j;
      long tmp_pixel;
      XWindowAttributes xwa;

      if (!quietFlag) {
	fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for(j = 0; j < i; j ++) {
	tmp_pixel = pixel[j];
        XFreeColors(display, cmap, &tmp_pixel, 1, 0);
      }

      XGetWindowAttributes(display, window, &xwa);
      cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
      XSetWindowColormap(display, window, cmap);

      goto retry_alloc_colors;
    }
    pixel[i] = xcolor.pixel;
  }

  ximage = NULL;
}

void InitRGBLDisplay(name)
char *name;
{
  int ncolors = R_RANGE+B_RANGE+G_RANGE;
  XColor xcolor;
  unsigned int r, g, b;
  Colormap dcmap;
  int i;

  dcmap = cmap = XDefaultColormap(display, DefaultScreen(display));

  xcolor.flags = DoRed | DoGreen | DoBlue;

 retry_alloc_colors:
  for (i=0; i<ncolors; i++) {

    if (i < R_RANGE) {
      r = r_values[i]; g = b = 0;
    }
    else if (i < R_RANGE+G_RANGE) {
      g = g_values[i-R_RANGE]; r = b = 0;
    }
    else if (i < R_RANGE+G_RANGE+B_RANGE) {
      b = b_values[i-R_RANGE-G_RANGE]; r = g = 0;
    }
    else {
      perror("mpeg_show: rgbl color table init error");
      exit(-1);
    }

    if (r > 255) r = 255;
    if (g > 255) g = 255; 
    if (b > 255) b = 255;

    xcolor.red = r * 256;
    xcolor.green = g * 256;
    xcolor.blue = b * 256;

    if(XAllocColor(display, cmap, &xcolor) == 0 && cmap == dcmap) {
      int j;
      long tmp_pixel;
      XWindowAttributes xwa;

      if (!quietFlag) {
	fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for(j = 0; j < i; j ++) {
	tmp_pixel = pixel[j];
        XFreeColors(display, cmap, &tmp_pixel, 1, 0);
      }

      XGetWindowAttributes(display, window, &xwa);
      cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
      XSetWindowColormap(display, window, cmap);

      goto retry_alloc_colors;
    }

    pixel[i] = xcolor.pixel;
  }

  for (i=0; i<ncolors; i++) {
    if (i < R_RANGE) {
      r_values[i] = pixel[i];
    }
    else if (i < R_RANGE+G_RANGE) {
      g_values[i-R_RANGE] = pixel[i];
    }
    else if (i < R_RANGE+G_RANGE+B_RANGE) {
      b_values[i-R_RANGE-G_RANGE] = pixel[i];
    }
  }

  ximage = NULL;
}


/*
 *--------------------------------------------------------------
 *
 * InitGrayDisplay --
 *
 *	Initialized display for gray scale dither.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

#define NUM_COLORS 128

void InitGrayDisplay(name)
char *name;
{
  int ncolors = NUM_COLORS;
  XColor xcolor;
  int i;
  Colormap dcmap;

  dcmap = cmap = XDefaultColormap(display, DefaultScreen(display));

  xcolor.flags = DoRed | DoGreen | DoBlue;

  retry_alloc_grays:
  for (i=0; i<ncolors; i++) {

    xcolor.red = (i*2) * 256;
    xcolor.green = (i*2) * 256;
    xcolor.blue = (i*2) * 256;

    if(XAllocColor(display, cmap, &xcolor) == 0 && cmap == dcmap) {
      int j;
      long tmp_pixel;
      XWindowAttributes xwa;

      if (!quietFlag) {
	fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for(j = 0; j < i; j ++) {
	tmp_pixel = pixel[j*2];
        XFreeColors(display, cmap, &tmp_pixel, 1, 0);
      }

      XGetWindowAttributes(display, window, &xwa);
      cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
      XSetWindowColormap(display, window, cmap);

      goto retry_alloc_grays;
    }
    pixel[(i*2)] = xcolor.pixel;
    pixel[(i*2)+1] = xcolor.pixel;
  }

  ximage = NULL;
}


/*
 *--------------------------------------------------------------
 *
 * InitMonoDisplay --
 *
 *	Initialized display for monochrome dither.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void InitMonoDisplay(name)
char *name;
{
  XGCValues xgcv;

  ximage = NULL;
}


/*
 *--------------------------------------------------------------
 *
 * InitColorDisplay --
 *
 *	Initialized display for full color output.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void InitColorDisplay(name)
char *name;
{

  ximage = NULL;
}


/*
 *--------------------------------------------------------------
 *
 * ExecuteDisplay --
 *
 *	Actually displays display plane in previously created window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

int
ExecuteDisplay(vid_stream)
     VidStream *vid_stream;
{
    extern Tk_Window w;
  static double lastTime = 0.0;
  double thisTime, sleepTime;
  Visual *FindFullColorVisual();

  totNumFrames++;

  if (ditherType == NO_DITHER) return FALSE;

  if (!noDisplayFlag) {
#ifdef SH_MEM
    if (shmemFlag) {
      XShmPutImage(display, window, gc, vid_stream->current->ximage, 
		   0, 0, 0, 0,
		   vid_stream->current->ximage->width, 
		   vid_stream->current->ximage->height, True);

      XFlush(display);

      while(1) {
	XEvent xev;
	
	XNextEvent(display, &xev);
	if(xev.type == CompletionType)
	  break;
      }
    }
    else 
#endif
      
      {
	if ( firstFrame && (! windowReady) )
	{
	    extern Tcl_Interp *interp;
	    char command[256];

	    switch(ditherType)
	    {
		case RGBL_DITHER:
		    imageHeight = vid_stream->mb_height*48;
		    imageWidth = vid_stream->mb_width*48;
		    break;
		case Twox2_DITHER:
		case BIG_ORDERED_DITHER:
		    imageHeight = vid_stream->mb_height*32;
		    imageWidth = vid_stream->mb_width*32;
		    break;		    
		default:
		    imageHeight = vid_stream->mb_height*16;
		    imageWidth = vid_stream->mb_width*16;
		    break;
	    }

	    origHeight = imageHeight;
	    origWidth = imageWidth;

	    DecodeFPS(vid_stream);

	    windowWidth = imageWidth+2*IMAGE_LEFT;
	    if ( windowWidth < 205 )
		windowWidth = 205;
	    windowHeight = imageHeight+IMAGE_TOP+STATUS_HEIGHT+
			    2*STATUS_BORDER+30;

	    XSetForeground(display, gc, BlackPixelOfScreen(Tk_Screen(w)));

	    sprintf(command, "wm geometry . %dx%d", windowWidth, windowHeight);
	    Tcl_Eval(interp, command);

	    ResizeWindow();

	    sprintf(command, "wm minsize . 205 %d",
		    IMAGE_TOP+STATUS_HEIGHT+2*STATUS_BORDER+30+32);
	    Tcl_Eval(interp, command);
	    Tcl_Eval(interp, "wm maxsize . 900 900");

	    windowReady = TRUE;
	}

	if ( ximage == NULL )
	    InitXImage();

	if ( ximage->data == NULL )
	    ximage->data = (char *) malloc(ximage->width*ximage->height*
					   sizeof(char));

	if ( (imageHeight == origHeight) && (imageWidth == origWidth) )
	    bcopy((char *) vid_stream->current->display, (char *)ximage->data,
		  ximage->width*ximage->height*sizeof(char));
	else
	    ScaleImage((char *)vid_stream->current->display,
		       (char *)ximage->data);

	XPutImage(display, window, gc, ximage, 0, 0, displayX, displayY,
		  imageWidth, imageHeight);

	ShowFrameInfo();

	firstFrame = FALSE;
      }
  }

    return TRUE;
}


void	ShowFrameInfo()
{
    char someText[256];
    extern int numSkipped;
    extern Tcl_Interp *interp;

    sprintf(someText, "set Frame(current) %d", totNumFrames+numSkipped);
    Tcl_Eval(interp, someText);
}


void	FramesToStopWatch(numFrames, text)
    int numFrames;
    char *text;
{
    int	minutes, seconds, tenths;
    int length;
    float   floatNumFrames;

    floatNumFrames = numFrames;

    minutes = (floatNumFrames/framesPerSecond)/60.0;
    floatNumFrames -= (float)minutes*60.0*framesPerSecond;
    seconds = floatNumFrames/framesPerSecond;
    floatNumFrames -= (float)seconds*framesPerSecond;
    tenths = (10.0*floatNumFrames)/framesPerSecond;
    sprintf(text, "%d:%2d.%d", minutes, seconds, tenths);
    length = strlen(text);
    if ( seconds < 10 )
	text[length-4] = '0';
}


void FillStatusBar()
{
}


void	UpdateStatusBar(bytesRead)
    int bytesRead;
{
}


void DecodeFPS(vid_stream)
    VidStream *vid_stream;
{
    extern float realFPS;
    extern Tcl_Interp *interp;
    int	intFPS;
    char command[256];

    /* see MPEG-1 standard, section 2.4.3.2 (Sequence header) */

    switch(vid_stream->picture_rate)
    {
	case 0:
	    fprintf(stderr, "FORBIDDEN PICTURE RATE! (assuming 30fps)\n");
	    framesPerSecond = 30.0;
	    break;
	case 1:
	    framesPerSecond = 24000.0/1001.0;
	    break;
	case 2:
	    framesPerSecond = 24.0;
	    break;
	case 3:
	    framesPerSecond = 25.0;
	    break;
	case 4:
	    framesPerSecond = 30000.0/1001.0;
	    break;
	case 5:
	    framesPerSecond = 30.0;
	    break;
	case 6:
	    framesPerSecond = 50.0;
	    break;
	case 7:
	    framesPerSecond = 60000.0/1001.0;
	    break;
	case 8:
	    framesPerSecond = 60.0;
	    break;
	default:
	    fprintf(stderr, "reserved pict rate...assuming ");
	    framesPerSecond = 30.0;
	    break;
    }

    realFPS = framesPerSecond;
    intFPS = (int) (realFPS+0.5);
    sprintf(command, "set Mpeg(fps) %d", intFPS);
    Tcl_Eval(interp, command);
    sprintf(command, "ScaleHandler %d", intFPS);
    Tcl_Eval(interp, command);
}


void ShowStatusOutline()
{
}


void ShowFPS()
{
}


void	RefreshCurrentImage()
{
    XPutImage(display, window, gc, ximage, 0, 0, displayX, displayY,
	      imageWidth, imageHeight);
}


void ResizeWindow()
{
    extern Tcl_Interp *interp;
    char command[256];

    centerWinWidth = windowWidth/2;

    imageHeight = windowHeight-(IMAGE_TOP+STATUS_HEIGHT+2*STATUS_BORDER+30);
    if ( origWidth*16+2*IMAGE_LEFT < 205 )
	imageWidth = origWidth;
    else
	imageWidth = windowWidth-2*IMAGE_LEFT;

    XClearArea(display, window, 0,
	       IMAGE_TOP,
	       windowWidth,
	       windowHeight-(IMAGE_TOP),
	       0 /* FALSE */ );
}


void InitXImage()
{
    extern VidStream *theStream;
    extern int inPlayLoop;
    char dummy;
    Visual *fc_visual;
    int depth;

    if ( ximage != NULL )
	XDestroyImage(ximage);

    if (ditherType == RGBL_DITHER) {
      ximage = XCreateImage(display, None, 8, ZPixmap, 0, &dummy,
			    imageWidth, imageHeight, 8, 0);
    }
    else if ((ditherType == Twox2_DITHER) ||
	(ditherType == BIG_ORDERED_DITHER)) {
      ximage = XCreateImage(display, None, 8, ZPixmap, 0, &dummy,
			    imageWidth, imageHeight, 8, 0);
    } else if (ditherType == FULL_COLOR_DITHER) {
      fc_visual = FindFullColorVisual(display, &depth);
      ximage = XCreateImage (display, fc_visual, depth, ZPixmap,
			     0, &dummy, imageWidth, imageHeight, 32, 0);
    } else if (ditherType == MONO_DITHER || ditherType == MONO_THRESHOLD ||
		ditherType == MONO_FS4_DITHER) {
      ximage = XCreateImage (display, None, 1, XYBitmap, 0, &dummy,
			     imageWidth, imageHeight, 8, 0);
      ximage->byte_order = MSBFirst;
      ximage->bitmap_bit_order = MSBFirst;
    } else if (ditherType == HALFTONE_DITHER) {
      ximage = XCreateImage (display, None, 1, XYBitmap, 0, &dummy,
			     4*imageWidth, 4*imageHeight, 8, 0);
      ximage->byte_order = MSBFirst;
      ximage->bitmap_bit_order = MSBFirst;
    } else {
      ximage = XCreateImage(display, None, 8, ZPixmap, 0, &dummy,
			    imageWidth, imageHeight, 8, 0);
    }

    ximage->data = NULL;

    if ( ! inPlayLoop )
    {
	    ximage->data = (char *) malloc(ximage->width*ximage->height*
					   sizeof(char));

	if ( ((imageHeight == origHeight) && (imageWidth == origWidth)) )
	    bcopy((char *) theStream->current->display, (char *)ximage->data,
		  ximage->width*ximage->height*sizeof(char));
	else
	    ScaleImage((char *)theStream->current->display,
		       (char *)ximage->data);	
    }
}


void	ScaleImage(orig, image)
    char *orig;
    char *image;
{
    register int x, y;
    char *iconPtr;
    char *imagePtr, *imageStart;
    float   xScale, yScale;
    int	imageX, imageY;
    int	xCoverage, yCoverage;

    /* need to scale */
    xScale = (float)origWidth/(float)imageWidth;
    yScale = (float)origHeight/(float)imageHeight;
    xCoverage = (int)(xScale+0.5);
    if ( xCoverage == 0 )
	xCoverage = 1;
    yCoverage = (int)(yScale+0.5);
    if ( yCoverage == 0 )
	yCoverage = 1;

    iconPtr = image;
    imageStart = orig;
    for ( y = 0; y < imageHeight; y++ )
    {
	imageY = ((int)(2.0*(float)y*yScale)+1)/2;
/*		imageY += ((rand() % (yCoverage << 1)) - yCoverage);	*/
	if ( imageY >= origHeight )
	    imageY = origHeight-1;
	else if ( imageY < 0 )
	    imageY = 0;
	imagePtr = imageStart + origWidth*imageY;

	for ( x = 0; x < imageWidth; x++ )
	{
	    imageX = ((int)(2.0*(float)x*xScale)+1)/2;
/*		    imageX += ((rand() % (xCoverage << 1)) - xCoverage); */
	    if ( imageX >= origWidth )
		imageX = origWidth-1;
	    else if ( imageX < 0 )
		imageX = 0;

	    *iconPtr = imagePtr[imageX];
	    iconPtr++;
	}
    }
}










