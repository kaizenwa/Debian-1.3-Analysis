#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#ifndef vms
#include <malloc.h>
#endif

/* use this to change if you want ripples with more than 256 colors */
#define RIPPLE_USES char

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "xb_config.h"
#include "xbanner.h"

static XImage *ximg;
static unsigned RIPPLE_USES *rimg;

/* Ripple routines */
#define rputpixel(ARRAY,XL,YL,PIXEL) (ARRAY)[(XL)+((YL)*(ri->width))]=(PIXEL)
#define rgetpixel(ARRAY,XL,YL)       (ARRAY)[(XL)+((YL)*(ri->width))]

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

static _inline int PDist(XPoint p, int x, int y,float scale)
{
  return (int) (
  		sqrt(
  		     (float)
  		     (
  		      (p.x-x) * (p.x-x)  +
  		      (p.y-y) * (p.y-y)
  		     )
  		    )*scale + frand()
  	       );
}

/* main ripple drawing thingy */
Stat DoRipples(Display *disp, Ripple_Info *ri)
{
  int p, x, y, v, scrn_no, depth, bpp;
  float  *scales;
  XPoint *pt_list;

  scrn_no = DefaultScreen(disp);
  depth   = DisplayPlanes(disp,scrn_no);
  bpp     = (depth/sizeof(char)) + (depth%sizeof(char)>0?1:0);

  /* first create an XImage by copying the root window */
  ximg = XCreateImage(disp,DefaultVisual(disp,0),DefaultDepth(disp,0),
		ZPixmap,0,(char*)malloc(bpp*(ri->width)*(ri->height)),
		ri->width,ri->height,BitmapPad(disp),0);
  if(ximg == NULL)
  {
    error(disp,mgc,"Could not get XImage for Ripples effect",False);
    return XIMAGE;
  }
  /* now get an image where the ripples take place */
  rimg = (unsigned char *)calloc(ri->width,ri->height);
  if(rimg == NULL)
  {
    error(disp,mgc,"Could not allocate memory for Ripples effect RImage",False);
    XDestroyImage(ximg);
    return NOMEM;
  }

  /* now create the ripple centers list */
  pt_list = (XPoint*)calloc(ri->ripples,sizeof(XPoint));
  if(pt_list == NULL)
  {
    error(disp,mgc,"Could not allocate memory for Ripple centers list",False);
    XDestroyImage(ximg);
    free(rimg);
    return NOMEM;
  }

  /* now create the ripple "speeds" list */
  scales = (float*)calloc(ri->ripples,sizeof(float));
  if(scales == NULL)
  {
    error(disp,mgc,"Could not allocate memory for Ripple speeds list",False);
    XDestroyImage(ximg);
    free(rimg);
    free(pt_list);
    return NOMEM;
  }

  /* choose the centers by random and random speeds */
  for(x=0;x<ri->ripples;x++)
  {
    scales[x] = (x+frand()+0.5)/3.0;
    pt_list[x].x = rand()%ri->width;
    pt_list[x].y = rand()%ri->height;
  }

  /* main loop */
  for(y=0;y<ri->height;y++)	/* for every pixel in the drawable */
    for(x=0;x<ri->width;x++)
    {
      p = rand()%ri->ripples;
      v = PDist(pt_list[p],x,y,scales[p]);
      rputpixel(rimg,x,y,((ri->colors)-1-(v%ri->colors)));
    }
  /* copy the RImage to the XImage... */
  for(y=0;y<ri->height;y++)
    for(x=0;x<ri->width;x++)
      XPutPixel(ximg,x,y,(ri->grad)->grad[rgetpixel(rimg,x,y)].pixel);

  /* copy the XImage back to the drawable */
  XPutImage(disp,ri->d,ri->gc,ximg,0,0,0,0,ri->width,ri->height);

  /* free the resources */
  XDestroyImage(ximg);
  return OK;
}
