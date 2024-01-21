#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#ifndef vms
#include <malloc.h>
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "xb_config.h"
#include "xbanner.h"

static XImage *ximg;
static unsigned char *pimg;

#ifdef INT_PLASMA
int _grain;
#endif

/* Plasma cloud routines */
#define pputpixel(ARRAY,XL,YL,PIXEL) (ARRAY)[(XL)+((YL)*(pi->width))]=(PIXEL)
#define pgetpixel(ARRAY,XL,YL)       (ARRAY)[(XL)+((YL)*(pi->width))]

/* define frand() */
#ifdef linux
#define frand() drand48()
#else
#ifdef OTHER_FRAND
#define frand() OTHER_FRAND()
#else
#define frand() (((double)rand())/(double)RAND_MAX)
#endif
#endif

/*
  NOTE: There is no documentation to the Plasma algorithm, because I am
        not sure I understand it properly. I ripped it off some program,
        which had no copyright notice anywhere near it, and not even a
        name to credit. I translated it from Pascal to C, and generalized
        it. In short, you take a rectangular area, choose colors which
        deviate by something from the average of two adjacent vertices,
        and color the mid-pixel. Do to all 4 mid-pixels on all 4 sides, 
        then split the area to 4 equal rectangles, and do the same on them.

	The adjust() function actually chooses the color for the mid-pixel.
*/

static _inline void adjust(Plasma_Info *pi,unsigned char *pimg,
		    int xa, int ya, int x, int y, int xb, int yb)
{
  static int v,d;
  
  /* is the mid pixel colored already? */
  if(pgetpixel(pimg,x,y)!=0)
    return;
  d = abs(xa-xb) + abs(ya-yb);
#ifdef INT_PLASMA
  v = ((pgetpixel(pimg,xa,ya)+pgetpixel(pimg,xb,yb))>>1) 
  	+ (((_grain*d*((rand()&0x7f)-64))>>14)-1);
#else
  v = ((pgetpixel(pimg,xa,ya)+pgetpixel(pimg,xb,yb))>>1) + 
  	(int)((pi->Grain)*d*(frand()-0.5)-1.0);
#endif
  if (v < 1)
    v=1;
  if (v >= pi->colors)
    v=pi->colors-1;
  pputpixel(pimg,x,y,v);
} /* adjust () */

/*
   The subdivide() function calls adjust() for all 4 sides,
   and subdive()s again.
*/
static void subdivide(Plasma_Info *pi, unsigned char *pimg,
		 int x1, int y1, int x2, int y2)
{
  int x,y,v;

  /* first check if we are done */
  if((x2-x1 < 2) && (y2-y1 < 2))
    return;

  x = (x1+x2)>>1;
  y = (y1+y2)>>1;

  adjust(pi,pimg,x1,y1,x,y1,x2,y1);
  adjust(pi,pimg,x2,y1,x2,y,x2,y2);
  adjust(pi,pimg,x1,y2,x,y2,x2,y2);
  adjust(pi,pimg,x1,y1,x1,y,x1,y2);
  
  if(pgetpixel(pimg,x,y)==0)
  {
    v=( pgetpixel(pimg,x1,y1) + pgetpixel(pimg,x2,y1) + 
        pgetpixel(pimg,x2,y2) + pgetpixel(pimg,x1,y2)  ) >> 2;
    pputpixel(pimg,x,y,v);
  }
  subdivide(pi,pimg,x1,y1,x,y);
  subdivide(pi,pimg,x,y1,x2,y);
  subdivide(pi,pimg,x,y,x2,y2);
  subdivide(pi,pimg,x1,y,x,y2);
} /* subdivide() */

/* main plasma drawing thingy */
Stat DoPlasma(Display *disp, Plasma_Info *pi)
{
  int x, y, scrn_no, depth, bpp;

  scrn_no = DefaultScreen(disp);
  depth   = DisplayPlanes(disp,scrn_no);
  bpp     = (depth/sizeof(char)) + (depth%sizeof(char)>0?1:0);

  /* first create an XImage by copying the root window */
  ximg = XCreateImage(disp,DefaultVisual(disp,0),DefaultDepth(disp,0),
		ZPixmap,0,(char*)malloc(bpp*(pi->width)*(pi->height)),
		pi->width,pi->height,BitmapPad(disp),0);
  if(ximg == NULL)
  {
    error(disp,mgc,"Could not get XImage for Plasma effect",False);
    return XIMAGE;
  }
  /* now get an image where the plasma takes place */
  pimg = (unsigned char *)calloc(pi->width,pi->height);
  if(pimg == NULL)
  {
    error(disp,mgc,"Could not allocate memory for Plasma effect PImage",False);
    XDestroyImage(ximg);
    return NOMEM;
  }
  /* randomly color the 4 corners */
  pputpixel(pimg,          0,           0,rand()%(pi->colors));
  pputpixel(pimg,          0,pi->height-1,rand()%(pi->colors));
  pputpixel(pimg,pi->width-1,           0,rand()%(pi->colors));
  pputpixel(pimg,pi->width-1,pi->height-1,rand()%(pi->colors));
#ifdef INT_PLASMA
  _grain = (int)(pi->Grain*128.0);
#endif
  /* do the plasma */
  subdivide(pi,pimg,0,0,pi->width-1,pi->height-1);

  /* copy the PImage to the XImage... */
  for(y=0;y<pi->height;y++)
    for(x=0;x<pi->width;x++)
      XPutPixel(ximg,x,y,(pi->grad)->grad[pgetpixel(pimg,x,y)].pixel);

  /* copy the XImage back to the drawable */
  XPutImage(disp,pi->d,pi->gc,ximg,0,0,0,0,pi->width,pi->height);

  /* free the resources */
  XDestroyImage(ximg);
  return OK;
}
