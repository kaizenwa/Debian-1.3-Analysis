#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef vms
#include <malloc.h>
#endif
#include <math.h>

#include "xb_config.h"
#include "xbanner.h"

char BGCOLOR[CNLEN];

BGStyle bgstyle = BGSTYLE_DEFVAL;

/* used for background fill */
XColor xbgc;
Bool do_fill = DO_FILL_DEFVAL;
Bool auto_fill = AUTO_FILL_DEFVAL;

/* some externals */
int bar_size          = BARSIZE_DEFVAL;     /* default bar-size  */
int BgGrad            = -1;		    /* no default BgGrad */
int BgGradRepeat      = BGGRAD_REPEAT_DEFVAL;/* no repeat         */

Bool BgGradCyc = False;
CycleType BgCycDir = CYC_FORWARD;

/* the root window's info for plasma clouds */
double Grain	      = PLASMA_GRAIN_DEFVAL; /* default graininess of plasme */
int num_plasma_colors = PLASMACOL_DEFVAL;  /* default # of colors for plasma */

/* info for the ripple effects */
int num_ripple_colors = RIPPLECOL_DEFVAL;
int num_ripples = RIPPLES_DEFVAL;

static void FillRoot(Display *disp, Window root, GC mgc, Pixel pixel)
{
  XSetWindowBackground(disp,root,pixel);
  XClearWindow(disp,root);
  /* not really necessary, but some Xservers would not work otherwise */
  XSetForeground(disp,mgc,pixel);
  XFillRectangle(disp,root,mgc,0,0,sc_w,sc_h);
}

void DoBackground(void)
{
  XPoint pts[4];
  int i,j,bars,xadd,yadd;
  int scrn_no, depth, bpp;
  Plasma_Info pi;
  Ripple_Info ri;

  scrn_no = DefaultScreen(disp);
  depth = DisplayPlanes(disp,scrn_no);
  bpp = (depth/sizeof(char)) + (depth%sizeof(char)>0?1:0);

  if(bar_size < BARSIZE_MINVAL)
    bar_size = BARSIZE_MINVAL;

  /* Do bgfill before other effects... */
  if(bgstyle == FILL || do_fill)
    FillRoot(disp,root,mgc,xbgc.pixel);

  switch(bgstyle)
  {
    case FILL:
      /*
         This was done by above if() but I don't want gcc to complain that
         FILL is not referenced by the switch() statement...
      */
      break;
    case RIPPLESBG:
      ri.d = root;			ri.gc = mgc;
      ri.width = sc_w;			ri.height = sc_h;
      ri.colors = num_ripple_colors;	ri.grad = &(grad[BgGrad]);
      ri.ripples = num_ripples;
      /* allocate the colors for it... */
      if(BgGradCyc == True && BgGrad != -1)
      {
        if(AllocGrad(BgGrad,num_ripple_colors,True)==True)
        {
          cyc_inf[num_used_cyc].grad_num = BgGrad;
          cyc_inf[num_used_cyc].only_first = False;
          cyc_inf[num_used_cyc].dir = BgCycDir;
	  num_used_cyc++;
        }
      }
      else
        AllocGrad(BgGrad,num_ripple_colors,False);

      if(auto_fill)
        FillRoot(disp,root,mgc,grad[BgGrad].grad[num_ripple_colors/2].pixel);

      DoRipples(disp,&ri);
      break;
    case PLASMA:
      /* fill in the information */
      pi.d = root;			pi.gc = mgc;
      pi.width = sc_w;			pi.height = sc_h;
      pi.colors = num_plasma_colors;	pi.grad = &(grad[BgGrad]);
      pi.Grain = Grain;
      /* allocate the colors for it... */
      if(BgGradCyc == True && BgGrad != -1)
      {
        if(AllocGrad(BgGrad,num_plasma_colors,True)==True)
        {
          cyc_inf[num_used_cyc].grad_num = BgGrad;
          cyc_inf[num_used_cyc].only_first = False;
          cyc_inf[num_used_cyc].dir = BgCycDir;
	  num_used_cyc++;
        }
      }
      else
        AllocGrad(BgGrad,num_plasma_colors,False);

      if(auto_fill)
        FillRoot(disp,root,mgc,grad[BgGrad].grad[num_plasma_colors/2].pixel);

      DoPlasma(disp,&pi);
      break;
#ifdef HAS_XPM
    case BGPIX:
      DoTilePixmap(disp,root);
      break;
#endif
    case FANBG:
      bars = ((sc_h*2 + sc_w)/BgGradRepeat) / bar_size + 1;
      if(BgGradCyc == True && BgGrad != -1)
      {
        if(AllocGrad(BgGrad,bars,True)==True)
        {
          cyc_inf[num_used_cyc].grad_num = BgGrad;
          cyc_inf[num_used_cyc].only_first = False;
          cyc_inf[num_used_cyc].dir = BgCycDir;
	  num_used_cyc++;
        }
      }
      else
        AllocGrad(BgGrad,bars,False);

      if(auto_fill)
        FillRoot(disp,root,mgc,grad[BgGrad].grad[bars/2].pixel);

      for(i=0;i<sc_h/bar_size+1;i++)
      {
        XSetForeground(disp,mgc,grad[BgGrad].grad[i%bars].pixel);
	pts[0].x = sc_w/2;     pts[0].y = sc_h;
	pts[1].x = 0;          pts[1].y = sc_h-i*bar_size;
	pts[2].x = 0;          pts[2].y = sc_h-(i+1)*bar_size;
	pts[3].x = sc_w/2;     pts[3].y = sc_h;
	XFillPolygon(disp,root,mgc,pts,4,Convex,CoordModeOrigin);
      }
      j=i;	/* old value of pixel */
      for(i=0;i<(sc_w/bar_size)+1;i++,j++)
      {
        XSetForeground(disp,mgc,grad[BgGrad].grad[j%bars].pixel);
	pts[0].x = sc_w/2;         pts[0].y = sc_h;
	pts[1].x = i*bar_size;     pts[1].y = 0;
	pts[2].x = (i+1)*bar_size; pts[2].y = 0;
	pts[3].x = sc_w/2;         pts[3].y = sc_h;
	XFillPolygon(disp,root,mgc,pts,4,Convex,CoordModeOrigin);
      }
      for(i=0;i<sc_h/bar_size+1;i++,j++)
      {
        XSetForeground(disp,mgc,grad[BgGrad].grad[j%bars].pixel);
	pts[0].x = sc_w/2;     pts[0].y = sc_h;
	pts[1].x = sc_w;       pts[1].y = i*bar_size;
	pts[2].x = sc_w;       pts[2].y = (i+1)*bar_size;
	pts[3].x = sc_w/2;     pts[3].y = sc_h;
	XFillPolygon(disp,root,mgc,pts,4,Convex,CoordModeOrigin);
      }
      break;
    case RIGHTSPLIT:
      bars = (sc_h/BgGradRepeat) / bar_size + 1;
      if(BgGradCyc == True && BgGrad != -1)
      {
        if(AllocGrad(BgGrad,bars,True)==True)
        {
          cyc_inf[num_used_cyc].grad_num = BgGrad;
          cyc_inf[num_used_cyc].only_first = False;
          cyc_inf[num_used_cyc].dir = BgCycDir;
	  num_used_cyc++;
        }
      }
      else
        AllocGrad(BgGrad,bars,False);

      if(auto_fill)
        FillRoot(disp,root,mgc,grad[BgGrad].grad[bars/2].pixel);

      for(i=0;i<(sc_h/bar_size)+1;i++)
      {
	XSetForeground(disp,mgc,grad[BgGrad].grad[i%bars].pixel);
	pts[0].x = sc_w;       pts[0].y = 0;
	pts[1].x = 0;          pts[1].y = i*bar_size;
	pts[2].x = 0;
	if((i+1)*bar_size < sc_h)
	  pts[2].y = (i+1)*bar_size;
	else
	  pts[2].y = sc_h;
	pts[3].x = sc_w;       pts[3].y = 0;
	XFillPolygon(disp,root,mgc,pts,4,Convex,CoordModeOrigin);
	pts[0].x = 0;          pts[0].y = sc_h;
	pts[1].x = sc_w;       pts[1].y = sc_h-i*bar_size;;
	pts[2].x = sc_w;
	if((i+1)*bar_size < sc_h)
	  pts[2].y = sc_h-(i+1)*bar_size;
	else
	  pts[2].y = 0;
	pts[3].x = 0;          pts[3].y = sc_h;
	XFillPolygon(disp,root,mgc,pts,4,Convex,CoordModeOrigin);
      }
      break;
    case LEFTSPLIT:
      bars = (sc_h/BgGradRepeat) / bar_size + 1;
      if(BgGradCyc == True && BgGrad != -1)
      {
        if(AllocGrad(BgGrad,bars,True)==True)
        {
          cyc_inf[num_used_cyc].grad_num = BgGrad;
          cyc_inf[num_used_cyc].only_first = False;
          cyc_inf[num_used_cyc].dir = BgCycDir;
	  num_used_cyc++;
        }
      }
      else
        AllocGrad(BgGrad,bars,False);

      if(auto_fill)
        FillRoot(disp,root,mgc,grad[BgGrad].grad[bars/2].pixel);

      for(i=0;i<sc_h/bar_size+1;i++)
      {
	XSetForeground(disp,mgc,grad[BgGrad].grad[i%bars].pixel);
	pts[0].x = 0;          pts[0].y = 0;
	pts[1].x = sc_w;       pts[1].y = i*bar_size;
	pts[2].x = sc_w;
        if((i+1)*bar_size < sc_h)
	  pts[2].y = (i+1)*bar_size;
	else
	  pts[2].y = sc_h;
	pts[3].x = 0;          pts[3].y = 0;
	XFillPolygon(disp,root,mgc,pts,4,Convex,CoordModeOrigin);
	XSync(disp,False);
	pts[0].x = sc_w;       pts[0].y = sc_h;
	pts[1].x = 0;          pts[1].y = sc_h-i*bar_size;;
	pts[2].x = 0;
	if((i+1)*bar_size < sc_h)
	  pts[2].y = sc_h-(i+1)*bar_size;
	else
	  pts[2].y = 0;
	pts[3].x = sc_w;       pts[3].y = sc_h;
	XFillPolygon(disp,root,mgc,pts,4,Convex,CoordModeOrigin);
	XSync(disp,False);
      }
      break;
    case TOPDOWN:
      bars = (sc_h/BgGradRepeat) / bar_size + 1;

      if(BgGradCyc == True && BgGrad != -1)
      {
        if(AllocGrad(BgGrad,bars,True)==True)
        {
          cyc_inf[num_used_cyc].grad_num = BgGrad;
          cyc_inf[num_used_cyc].only_first = False;
          cyc_inf[num_used_cyc].dir = BgCycDir;
	  num_used_cyc++;
        }
      }
      else
        AllocGrad(BgGrad,bars,False);

      if(auto_fill)
        FillRoot(disp,root,mgc,grad[BgGrad].grad[bars/2].pixel);

      for(i=0;i<bars*BgGradRepeat;i++)
      {
	XSetForeground(disp,mgc,grad[BgGrad].grad[i%bars].pixel);
	XFillRectangle(disp,root,mgc,0,i*bar_size,sc_w,bar_size);
      }
      break;
    case LEFTRIGHT:
      bars = (sc_w/BgGradRepeat) / bar_size + 1;
      if(BgGradCyc == True && BgGrad != -1)
      {
        if(AllocGrad(BgGrad,bars,True)==True)
        {
          cyc_inf[num_used_cyc].grad_num = BgGrad;
          cyc_inf[num_used_cyc].only_first = False;
          cyc_inf[num_used_cyc].dir = BgCycDir;
	  num_used_cyc++;
        }
      }
      else
        AllocGrad(BgGrad,bars,False);

      if(auto_fill)
        FillRoot(disp,root,mgc,grad[BgGrad].grad[bars/2].pixel);

      for(i=0;i<bars*BgGradRepeat;i++)
      {
	XSetForeground(disp,mgc,grad[BgGrad].grad[i%bars].pixel);
	XFillRectangle(disp,root,mgc,i*bar_size,0,bar_size,sc_h);
      }
      break;
    case DIAGL:
      bars = (sc_w/BgGradRepeat) / bar_size;
      if(BgGradCyc == True && BgGrad != -1)
      {
        if(AllocGrad(BgGrad,bars,True)==True)
        {
          cyc_inf[num_used_cyc].grad_num = BgGrad;
          cyc_inf[num_used_cyc].only_first = False;
          cyc_inf[num_used_cyc].dir = BgCycDir;
	  num_used_cyc++;
        }
      }
      else
        AllocGrad(BgGrad,bars,False);

      if(auto_fill)
        FillRoot(disp,root,mgc,grad[BgGrad].grad[bars/2].pixel);

      xadd = (sc_w / bars) * 2 + 1;
      yadd = (sc_h / bars) * 2 + 1;
      for(i=0;i<bars*BgGradRepeat;i++)
      {
	XSetForeground(disp,mgc,grad[BgGrad].grad[i%bars].pixel);
	pts[0].x = i*xadd;     pts[0].y = 0;
	pts[1].x = (i+1)*xadd; pts[1].y = 0;
	pts[2].x = 0;          pts[2].y = (i+1)*yadd;
	pts[3].x = 0;          pts[3].y = i*yadd;
	XFillPolygon(disp,root,mgc,pts,4,Convex,CoordModeOrigin);
      }
      break;
    case DIAGR:
      bars = (sc_w/BgGradRepeat) / bar_size;
      if(BgGradCyc == True && BgGrad != -1)
      {
        if(AllocGrad(BgGrad,bars,True)==True)
        {
          cyc_inf[num_used_cyc].grad_num = BgGrad;
          cyc_inf[num_used_cyc].only_first = False;
          cyc_inf[num_used_cyc].dir = BgCycDir;
	  num_used_cyc++;
        }
      }
      else
        AllocGrad(BgGrad,bars,False);

      if(auto_fill)
        FillRoot(disp,root,mgc,grad[BgGrad].grad[bars/2].pixel);

      xadd = (sc_w / bars) * 2 + 1;
      yadd = (sc_h / bars) * 2 + 1;
      for(i=0;i<bars*BgGradRepeat;i++)
      {
	XSetForeground(disp,mgc,grad[BgGrad].grad[i%bars].pixel);
	pts[0].x = sc_w-i*xadd;         pts[0].y = 0;
	pts[1].x = sc_w-(i+1)*xadd;     pts[1].y = 0;
	pts[2].x = sc_w-1;              pts[2].y = (i+1)*yadd;
	pts[3].x = sc_w-1;              pts[3].y = i*yadd;
	XFillPolygon(disp,root,mgc,pts,4,Convex,CoordModeOrigin);
      }
      break;
    default:
      ;
  }
} /* DoBackground */
