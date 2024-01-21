/* our config file... */
#include "xb_config.h"

/* C library includes */
#include <string.h>

#include <stdio.h>
#include <ctype.h>
#ifndef vms
#include <malloc.h>
#endif

/* X11 include(s) */
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/* our include */
#include "xbanner.h"

/* global vars for this file */
static XImage *scrimg;
static unsigned char *ZImage;
static unsigned char *FImage;

int FgCycNumCols  = FGCYC_NUMCOL_DEFVAL;
int FgGradBarSize = FGGRAD_BARSIZE_DEFVAL;

/* draw the main thing... the FG one - no shadows and stuff */
static _inline void DrawFG(void)
{
  XSetForeground(disp,mgc,xfgc.pixel);
  XDrawString(disp,rootpix,mgc,final_x,final_y,BANNER,strlen(BANNER));
}

/*
   Draws the Underline under the text. If this is in the same color as the
   FGC, and hopefully inside the pixmap / XImage - the effect might be
   rendered on the underline too
*/
static void DrawUL(void)
{
  int x1,y,x2,i,j,t;
  XGCValues gcv;

  x1 = final_x;
  x2 = x1+bwid;
  y  = rel_y+final_y;

  /* each effect needs to take into consideration different variables */
  switch(effect)
  {
    case FGPLASMA:
    case FGGRAD:
    case SHADOW:
    case FATTEXT:
      x2+=SHD_X_OFFSET;
      y+=3*SHD_Y_OFFSET;
      gcv.line_width = ULthickness;
      XChangeGC(disp,mgc,GCLineWidth,&gcv);
      XSetForeground(disp,mgc,xshdc.pixel);
      XDrawLine(disp,rootpix,mgc,
      	x1+SHD_X_OFFSET,y+SHD_Y_OFFSET,x2+SHD_X_OFFSET,y+SHD_Y_OFFSET);
      gcv.line_width = 1;
      XChangeGC(disp,mgc,GCLineWidth,&gcv);
      break;
    case SHAKE:
      y+=18;
      break;
    case OUTLINE:
      y+=SUR_MAX;
      x1+=SUR_MIN;
      x2+=SUR_MAX;
      break;
    case SHADOUTL:
      y+=2*(SUR_MAX-SUR_MIN)+2*SHD_Y_OFFSET;
      x1+=SUR_MIN;
      x2+=SUR_MAX+SHD_X_OFFSET;
      gcv.line_width = ULthickness;
      XChangeGC(disp,mgc,GCLineWidth,&gcv);

      XSetForeground(disp,mgc,xshdc.pixel);
      for(i=SUR_MIN;i<SUR_MAX+1;i++)
        for(j=SUR_MIN;j<SUR_MAX+1;j++)
          XDrawLine(disp,rootpix,mgc,
            x1+SHD_X_OFFSET+i,y+SHD_Y_OFFSET+j,
            x2+SHD_X_OFFSET+i,y+SHD_Y_OFFSET+j);
      XSetForeground(disp,mgc,xlghtc.pixel);
      for(i=SUR_MIN;i<SUR_MAX+1;i++)
        for(j=SUR_MIN;j<SUR_MAX+1;j++)
          XDrawLine(disp,rootpix,mgc,x1+i,y+j,x2+i,y+j);
      gcv.line_width = 1;
      XChangeGC(disp,mgc,GCLineWidth,&gcv);
      break;
    case SHD3D:
      x1+=SUR_MIN;
      y+=(SUR_MAX+SHD_Y_OFFSET)*(SHD3D_SHADOWS+1);
      x2+=(SUR_MAX+SHD_X_OFFSET)*SHD3D_SHADOWS;
      break;
    case THICK:
      y+=2*(SUR_MAX+Thickness);
      x1+=SUR_MIN;
      x2+=2*SUR_MAX+Thickness;
      gcv.line_width = ULthickness;
      XChangeGC(disp,mgc,GCLineWidth,&gcv);
      for(t=Thickness ; t>0 ; t-=1)
      {
        XSetForeground(disp,mgc,xshdc.pixel);
        XDrawLine(disp,rootpix,mgc,
        	x1+t+SUR_MIN,y+t+SUR_MAX,
        	x2+t+SUR_MIN,y+t+SUR_MAX);
        XDrawLine(disp,rootpix,mgc,
        	x1+t+SUR_MAX,y+t+SUR_MIN,
        	x2+t+SUR_MAX,y+t+SUR_MIN);
        XDrawLine(disp,rootpix,mgc,
        	x1+t+SUR_MIN,y+t+SUR_MIN,
        	x2+t+SUR_MIN,y+t+SUR_MIN);
        XDrawLine(disp,rootpix,mgc,
        	x1+t+SUR_MAX,y+t+SUR_MAX,
        	x2+t+SUR_MAX,y+t+SUR_MAX);
      }
      XSetForeground(disp,mgc,xlghtc.pixel);
      for(i=SUR_MIN;i<SUR_MAX+1;i++)
        for(j=SUR_MIN;j<SUR_MAX+1;j++)
          XDrawLine(disp,rootpix,mgc,x1+i,y+j,x2+i,y+j);
      gcv.line_width = 1;
      XChangeGC(disp,mgc,GCLineWidth,&gcv);
      break;
    case FADE:
      x1=0;
      y+=2*Thickness;
      x2+=2*Thickness;
      break;
    case STANDIN:
    case STANDOUT:
    case POPART:
    case COIN:
    case BACKLIGHT:
      y+=2*Thickness+ULthickness;;
      x1-=2*Thickness;
      x2+=2*Thickness;
      break;
    case NONE:		/* these will have the same X/Y of the underline */
    case STDOUT2:
    case STDIN2:
    case FUNNYOUTL:
      y+=2*Thickness+ULthickness;
      break;
  } /* switch(effect) */  

  /* set the thickness */
  gcv.line_width = ULthickness;
  XChangeGC(disp,mgc,GCLineWidth,&gcv);
  XSetForeground(disp,mgc,xulc.pixel);
  XDrawLine(disp,rootpix,mgc,x1,y,x2,y);
  /* return the line width */
  gcv.line_width = 1;
  XChangeGC(disp,mgc,GCLineWidth,&gcv);
} /* DrawUl() */

/* get the XImage of the area with the FG drawn already for the effects */
static void GetXImage(void)
{
  /* rootpix contains only the relevant area */
  scrimg = XGetImage(disp,rootpix,0,0,rel_w,rel_h,0xffffffff,ZPixmap);
  if(scrimg==NULL)
    error(disp,mgc,"REPORT: XImage of screen",True);

  /* Allocate memory for the height map and filter area */
  ZImage = (unsigned char*)calloc(rel_w*rel_h,1);
  if(ZImage==NULL)
    error(disp,mgc,"REPORT: ZImage",True);

  /* use FImage only for those who need it */
  if(effect == STANDOUT || effect == STANDIN || 
     effect == COIN     || effect == STDOUT2 ||
     effect == STDIN2)
  {
    FImage = (unsigned char*)calloc(rel_w*rel_h,1);
    if(FImage==NULL)
      error(disp,mgc,"REPORT: FImage",True);
  }
} /* GetXImage() */

static void PutXImage(void)
{
  XPutImage(disp,rootpix,mgc,scrimg,0,0,0,0,rel_w,rel_h);
  /* we don't need it after we put it back... */
  XDestroyImage(scrimg);
  free(ZImage);
  /* paranoia - don't try to free a NULL */
  if(effect == STANDOUT || effect == STANDIN ||
     effect == COIN     || effect == STDOUT2 ||
     effect == STDIN2)
    free(FImage);
}

/* some def's for FindCorners */
#define PIX_UpLeft	(1<<0)
#define PIX_UpRight	(1<<1)
#define PIX_DownLeft	(1<<2)
#define PIX_DownRight	(1<<3)
#define PIX_Up		(1<<4)
#define PIX_Down	(1<<5)
#define PIX_Left	(1<<6)
#define PIX_Right	(1<<7)

/* FindCorners finds the corners we can glint on */
static void FindCorners(void)
{
  int corners_found=0;
  int x,y;
  unsigned char pix_around;
  Pixel fgc;
  
  /* for convenience */
  fgc = xfgc.pixel;

  /* loop to find those corners ... */
  for(y=1 ; y < rel_h-1 ; y++)
    for(x=1 ; x < rel_w-1 ; x++)
      if(XGetPixel(scrimg,x,y)==fgc)
      {
        pix_around = 0;
        if(XGetPixel(scrimg,x-1,y)==fgc)
          pix_around |= PIX_Left;
        if(XGetPixel(scrimg,x+1,y)==fgc)
          pix_around |= PIX_Right;
        if(pix_around == (PIX_Left|PIX_Right))	/* both left and right... */
          continue;

        if(XGetPixel(scrimg,x,y-1)==fgc)
          pix_around |= PIX_Up;
        if(XGetPixel(scrimg,x,y+1)==fgc)
          pix_around |= PIX_Down;
	if((pix_around & (PIX_Up|PIX_Down)) == (PIX_Up|PIX_Down))
	  continue;			/* both Up and Down */

        if(XGetPixel(scrimg,x-1,y-1)==fgc)
          pix_around |= PIX_UpLeft;
        if(XGetPixel(scrimg,x+1,y-1)==fgc)
          pix_around |= PIX_UpRight;
        if(XGetPixel(scrimg,x-1,y+1)==fgc)
          pix_around |= PIX_DownLeft;
        if(XGetPixel(scrimg,x+1,y+1)==fgc)
          pix_around |= PIX_DownRight;

	/* make sure it really is a corner */
        if( ((pix_around & (PIX_Left|PIX_Up)) == (PIX_Left|PIX_Up)) &&
            ((pix_around & PIX_UpRight)  == 0) &&
            ((pix_around & PIX_DownLeft) == 0) )
	  corners_found++;
        if( ((pix_around & (PIX_Left|PIX_Down)) == (PIX_Left|PIX_Down)) &&
            ((pix_around & PIX_UpLeft)    == 0) &&
            ((pix_around & PIX_DownRight) == 0) )
	  corners_found++;
        if( ((pix_around & (PIX_Right|PIX_Up)) == (PIX_Right|PIX_Up)) &&
            ((pix_around & PIX_UpLeft)    == 0) &&
            ((pix_around & PIX_DownRight) == 0) )
	  corners_found++;
        if( ((pix_around & (PIX_Right|PIX_Down)) == (PIX_Right|PIX_Down)) &&
            ((pix_around & PIX_UpRight)  == 0) &&
            ((pix_around & PIX_DownLeft) == 0) )
	  corners_found++;
      } /* if(XGetPixel(scrimg,x,y)==fgc) */

  NumCorners = corners_found;	/* reflect this */

  /* allocate memory to keep the corners */
  CornerList = (Corner*)calloc(NumCorners,sizeof(Corner));
  if(CornerList==NULL)
  {
    error(disp,mgc,"No mem for corner-list, Glint disabled",False);
    NumCorners = 0;
    return;
  }
  
  corners_found=0;	/* now use it as a counter */

  /* now write them down... */
  for(y=1 ; (y < rel_h-1) && (corners_found < NumCorners) ; y++)
    for(x=1 ; x < rel_w-1 && (corners_found < NumCorners) ; x++)
      if(XGetPixel(scrimg,x,y)==fgc)
      {
        pix_around = 0;
        if(XGetPixel(scrimg,x-1,y)==fgc)
          pix_around |= PIX_Left;
        if(XGetPixel(scrimg,x+1,y)==fgc)
          pix_around |= PIX_Right;
        if(pix_around == (PIX_Left|PIX_Right))	/* both left and right... */
          continue;

        if(XGetPixel(scrimg,x,y-1)==fgc)
          pix_around |= PIX_Up;
        if(XGetPixel(scrimg,x,y+1)==fgc)
          pix_around |= PIX_Down;
	if((pix_around & (PIX_Up|PIX_Down)) == (PIX_Up|PIX_Down))
	  continue;			/* both Up and Down */

        if(XGetPixel(scrimg,x-1,y-1)==fgc)
          pix_around |= PIX_UpLeft;
        if(XGetPixel(scrimg,x+1,y-1)==fgc)
          pix_around |= PIX_UpRight;
        if(XGetPixel(scrimg,x-1,y+1)==fgc)
          pix_around |= PIX_DownLeft;
        if(XGetPixel(scrimg,x+1,y+1)==fgc)
          pix_around |= PIX_DownRight;

	/* make sure it really is a corner */
        if( ((pix_around & (PIX_Left|PIX_Up)) == (PIX_Left|PIX_Up)) &&
            ((pix_around & PIX_UpRight)  == 0) &&
            ((pix_around & PIX_DownLeft) == 0) )
        {
          CornerList[corners_found].x     = x + rel_x;
          CornerList[corners_found].y     = y + rel_y;
          CornerList[corners_found].ctype = DownRight;
	  if(CornerMask & DownRight)	/* do we want these corners? */
	    corners_found++;
        }
        if( ((pix_around & (PIX_Left|PIX_Down)) == (PIX_Left|PIX_Down)) &&
            ((pix_around & PIX_UpLeft)    == 0) &&
            ((pix_around & PIX_DownRight) == 0) )
        {
          CornerList[corners_found].x     = x + rel_x;
          CornerList[corners_found].y     = y + rel_y;
          CornerList[corners_found].ctype = UpRight;
	  if(CornerMask & UpRight)
	    corners_found++;
        }
        if( ((pix_around & (PIX_Right|PIX_Up)) == (PIX_Right|PIX_Up)) &&
            ((pix_around & PIX_UpLeft)    == 0) &&
            ((pix_around & PIX_DownRight) == 0) )
        {
          CornerList[corners_found].x     = x + rel_x;
          CornerList[corners_found].y     = y + rel_y;
          CornerList[corners_found].ctype = DownLeft;
	  if(CornerMask & DownLeft)
	    corners_found++;
        }
        if( ((pix_around & (PIX_Right|PIX_Down)) == (PIX_Right|PIX_Down)) &&
            ((pix_around & PIX_UpRight)  == 0) &&
            ((pix_around & PIX_DownLeft) == 0) )
        {
          CornerList[corners_found].x     = x + rel_x;
          CornerList[corners_found].y     = y + rel_y;
          CornerList[corners_found].ctype = UpLeft;
	  if(CornerMask & UpLeft)
	    corners_found++;
        }
      } /* XGetPixel == fgc */
  NumCorners=corners_found;
} /* FindCorners */

/*
   This routine handles a main part in many effects. It takes an XImage,
   and creates a height map of it. Each pixel in the FG color is set to
   Thickness+1 height. Others are zeroed already by calloc(). If direction
   in INWARDS it reverses these.
   
   The next stage is to start "drawing outlines" around the text in the
   ZImage buffer. Each outline is drawn with a value corresponding to a
   height. The first outline will be height Thickness, then Thickness-1 etc.
   all the way to 0 (which is the background).

   Some of the effects are handled directly in here. PopArt is actually
   just drawing those outlines in alternating colors. Fade needs a special
   type of outline. Only pixels diagonally to the right and down from the
   pixel checked are given the height.
   
   Performance issues made me search for the pixels that will comprise the
   first outline, write them down into an array, and then iterate these
   only, writing their newly-outlined neighbors to a second array, then
   alternating the arrays.
   
   Originally, INWARDS direction was handled by simply inverting the ZImage
   buffer - non FGC colored pixels would be given height Thickness+1. This
   caused the INWARDS effects to take a lot more than others. I changed this
   so that now I still give them the same height as before, but I now look
   for pixels with height 0 and scan their neighbors instead of searching
   for pixels with height Thickness+1 etc.
*/
static void MakeZImageOutline(Zmethod direction)
{
#ifdef EXPLICIT_REGISTER_VARIABLES
  register n,x=0,y=0;
#else
  int n,x=0,y=0;		/* iterators */
#endif
  Pixel fgc,shdc,lghtc;
  int swid = Thickness;
  int xadd[8] = { -1,  0,  1, -1, 1, -1, 0, 1 };
  int yadd[8] = { -1, -1, -1,  0, 0,  1, 1, 1 };
  int neighbors[8];
  int i,k=0,pixels=0;
  long *pix1,*pix2;

  /* must initialize neighbors like this for Ultrix and some other ANSI CCs */
  neighbors[0] = -rel_w-1;
  neighbors[1] = -rel_w;
  neighbors[2] = -rel_w+1;
  neighbors[3] = -1;
  neighbors[4] = 1;
  neighbors[5] = rel_w+1;
  neighbors[6] = rel_w;
  neighbors[7] = rel_w-1;

  fgc   = xfgc.pixel;	/* for ease of use */
  shdc  = xshdc.pixel;
  lghtc = xlghtc.pixel;

  if(XB_Glint==True)
    FindCorners();	/* find them corners, willya? */

  /* fill in the ZImage height map according to the XImage */
  if(direction==OUTWARDS)
  {
    for(y=0 ; y < rel_h ; y++)
      for(x=0 ; x < rel_w ; x++)
        if(XGetPixel(scrimg,x,y)==fgc)
          ZImage[x+y*rel_w]=swid+1;
  }
  else
  {
    for(y=0 ; y < rel_h ; y++)
      for(x=0 ; x < rel_w ; x++)
        if(XGetPixel(scrimg,x,y)!=fgc)
          ZImage[x+y*rel_w]=swid+1;
  }

  /* let's check how many pixels are going to be in the outline */
  if(direction==OUTWARDS)
  {
    for(y=1 ; y < rel_h-1 ; y++)
      for(x=1 ; x < rel_w-1 ; x++)
        if (ZImage[x+rel_w*y] == swid+1)
          for(n=0;n<8;n++)
            if(ZImage[x+rel_w*y+neighbors[n]]==0)
              k++;
  }
  else /* INWARDS */
  {
    for(y=1 ; y < rel_h-1 ; y++)
      for(x=1 ; x < rel_w-1 ; x++)
        if (ZImage[x+rel_w*y] == 0)
          for(n=0;n<8;n++)
            if(ZImage[x+rel_w*y+neighbors[n]]==swid+1)
            {
              k++;
              break;
            }
  }

  /* allocate memory for the first array */
  pix1 = (long*)malloc(k*sizeof(long));
  if(pix1==NULL)
    error(disp,mgc,"REPORT: Alloc pix1",True);

  /* main loop first time */
  for(y=1 ; y < rel_h-1 ; y++)
  {
    for(x=1 ; x < rel_w-1 ; x++)
    {
      if(direction==OUTWARDS)
      {
        if (ZImage[x+rel_w*y] == swid+1)
        {
          if(effect==FADE)
          {
            if(ZImage[x+rel_w*y+rel_w+1]==0)
            {
              ZImage[x+rel_w*y+rel_w+1]=swid;
              pix1[pixels++]=x+rel_w*y+rel_w+1;
            }
          }
          else /* effect != FADE */
          {
            for(n=0;n<8;n++)
            {
              if(ZImage[x+rel_w*y+neighbors[n]]==0)
              {
                ZImage[x+rel_w*y+neighbors[n]]=swid;
                pix1[pixels++]=x+rel_w*y+neighbors[n];
                if(effect==POPART)
                  XPutPixel(scrimg,x+xadd[n],y+yadd[n],(swid&1?lghtc:shdc));
              }
            } /* n */
          } /* if effect */
        } /* if (ZImage[x+rel_w*y] == swid+1) */
      }
      else /* direction */
      {
        if (ZImage[x+rel_w*y] == 0)
        {
          for(n=0;n<8;n++)
          {
            if(ZImage[x+rel_w*y+neighbors[n]]==swid+1)
            {
              ZImage[x+rel_w*y+neighbors[n]]=swid;
              pix1[pixels++]=x+rel_w*y+neighbors[n];
              break;
            }
          } /* n */
        } /* if */
      } /* direction */
    } /* x */
  } /* y */

  /* loop for the rest of the outlines */
  while(--swid)
  {
    /* the allocation is always no more than 1.5 * pixels in previous outline */
    pix2 = (long*)malloc((pixels*3*sizeof(long))/2); k=0;
    if(pix2==NULL)
      error(disp,mgc,"REPORT: Alloc pix2",True);
    for(i=0;i<pixels;i++)
    {
      if(effect==POPART)	/* for XPutPixel */
      {
        x=pix1[i]%rel_w;
        y=pix1[i]/rel_w;
      }
      if(effect==FADE)
      {
        if(ZImage[pix1[i]+rel_w+1]==0)
        {
          ZImage[pix1[i]+rel_w+1]=swid;
          pix2[k++]=pix1[i]+rel_w+1;
        }
      }
      else /* effect != FADE */
      {
        for(n=0;n<8;n++)
        {
          if(ZImage[pix1[i]+neighbors[n]]==0)
          {
            ZImage[pix1[i]+neighbors[n]]=swid;
            pix2[k++]=pix1[i]+neighbors[n];
            if(effect==POPART)			/* notice x,y are set before */
              XPutPixel(scrimg,x+xadd[n],y+yadd[n],(swid&1?lghtc:shdc));
          }
        } /* n */
      } /* effect */
    } /* for i=0 i<pixels etc... */
    pixels=k; free(pix1); pix1=pix2;
  }

} /* MakeZImageOutline */

/*
   Notice that the difference between Fade and Backlight is that Backlight
   scans all its neighbors but Fade scans only the one that sits diagonally
   down and right from checked pixel...

   so DoFadeEffect almost= DoBacklightEffect!

   Actually this function also does the FatText effect!
*/

int FgCycGrad= -1;	/* no grad defined for fg-cycle  */
int BkltGrad= -1;	/* no grad defined for backlight */
int FadeGrad= -1;	/* no grad defined for fade      */
int FatTextGrad= -1;	/* no grad defined for fattext   */

Bool BkltGradCyc = False;
Bool FatGradCyc  = False;
Bool FadeGradCyc = False;

CycleType BkltCycDir = CYC_FORWARD;
CycleType FatCycDir = CYC_FORWARD;
CycleType FadeCycDir = CYC_FORWARD;

void DoBacklightEffect(void)
{
  int x,y;		/* iterators */
  int n,gradnum= -1;
  Bool Cyc;
  CycleType cdir;

  /* let's use the correct grad and its correct cycle direction */
  switch(effect)
  {
    case BACKLIGHT:
      gradnum=BkltGrad;
      Cyc = BkltGradCyc;
      cdir = BkltCycDir;
      break;
    case FADE:
      gradnum=FadeGrad;
      Cyc = FadeGradCyc;
      cdir = FadeCycDir;
      break;
    case FATTEXT:
      gradnum=FatTextGrad;
      Cyc = FatGradCyc;
      cdir = FadeCycDir;
      break;
    default:
      gradnum= -1;
      Cyc = False;
      cdir = CYC_FORWARD;
      break;
  }

  /* get the fading colors */
  if( Cyc == True )
  {
    if(AllocGrad(gradnum,Thickness,True)==True)
    {
      cyc_inf[num_used_cyc].grad_num = gradnum;
      cyc_inf[num_used_cyc].only_first = False;
      cyc_inf[num_used_cyc].dir = cdir;
      num_used_cyc++;
    }
  }
  else
    AllocGrad(gradnum,Thickness,False);

  /* main part - do the outlines */
  if(effect==FATTEXT)
    MakeZImageOutline(INWARDS);
  else
    MakeZImageOutline(OUTWARDS);

  /* now paint the outlines in fading colors */
  for(y=0 ; y < rel_h ; y++)
    for(x=0 ; x < rel_w ; x++)
    {
      n=ZImage[x+rel_w*y];
      if(n==0 || n==Thickness+1)
        continue;
      XPutPixel(scrimg,x,y,grad[gradnum].grad[n-1].pixel);
    } /* x iter */
} /* BackLight Effect */

int FgGradGrad= -1;
Bool FgGradCyc=False;
CycleType FgGradCycDir = CYC_FORWARD;
void DoFGGradEffect(void)
{
  int x,y;		/* iterators */
  int n,hy=32767,ly=0;
  int clrs;

  /* find highest and lowest pixel lines */
  for(y=0 ; y < rel_h ; y++)
    for(x=0 ; x < rel_w ; x++)
      if(XGetPixel(scrimg,x,y)==xfgc.pixel)
      {
        if (y<hy) hy=y;
        if (y>ly) ly=y;
        ZImage[x+y*rel_w]=y-hy+1;
      }

  /* figure out number of colors to allocate */
  clrs=((ly-hy+1)/FgGradBarSize) + ((ly-hy+1)%FgGradBarSize);

  /* get the fading colors */
  if(FgGradCyc==True)
  {
    if(AllocGrad(FgGradGrad,clrs,True)==True)
    {
      cyc_inf[num_used_cyc].grad_num = FgGradGrad;
      cyc_inf[num_used_cyc].only_first = False;
      cyc_inf[num_used_cyc].dir = FgGradCycDir;
      num_used_cyc++;
    }
  }
  else
    AllocGrad(FgGradGrad,clrs,False);

  /* call this here because FgGrad does not use MakeZImageOutline */
  if(XB_Glint==True)
    FindCorners();
    
  /* now paint in fading colors */
  for(y=0 ; y < rel_h ; y++)
    for(x=0 ; x < rel_w ; x++)
    {
      n=ZImage[x+rel_w*y];
      if(n==0)
        continue;
      XPutPixel(scrimg,x,y,grad[FgGradGrad].grad[(n-1)/FgGradBarSize].pixel);
    } /* x iter */
} /* FG-Grad Effect */

void DoShakeEffect(void)
{
  int x,y;		/* iterators */
  Pixel p=xfgc.pixel;

  /* now do it... check if this pixel is above the text */
  for(y=0 ; y < rel_h-9 ; y+=3)
    for(x=0 ; x < rel_w ; x++)
    {
      if(XGetPixel(scrimg,x,y+3)==p ||
         XGetPixel(scrimg,x,y+6)==p ||
         XGetPixel(scrimg,x,y+9)==p)
      XPutPixel(scrimg,x,y,p);
    } /* x iter */

  /* in reverse for pixels below the text */
  for(y=rel_h ; y > 9 ; y-=3)
    for(x=0 ; x < rel_w ; x++)
    {
      if(XGetPixel(scrimg,x,y-9)==p ||
         XGetPixel(scrimg,x,y-6)==p ||
         XGetPixel(scrimg,x,y-3)==p)
      XPutPixel(scrimg,x,y,p);
    } /* x iter */
} /* Shake Effect */

int FgPlasmaGrad = -1;
int FgPlasma_ncol = FGPL_NCOL_DEFVAL;
double FgPlasmaGrain = FGPL_GRAIN_DEFVAL;
Bool FgplGradCyc = False;
CycleType FgplCycDir = CYC_FORWARD;
void DoFGPlasmaEffect(void)
{
  Plasma_Info pi;
  XImage *ximg;
  int x,y;

  /* make us a nice drawable */
  pi.d = XCreatePixmap(disp,root,rel_w,rel_h,     /* create the pixmap */
                            DefaultDepth(disp,XDefaultScreen(disp)));
  /* fill in more details */
  pi.gc = mgc; pi.width=rel_w; pi.height=rel_h;
  /* figure out number of colors to allocate */
  pi.colors = FgPlasma_ncol;
  pi.Grain = FgPlasmaGrain;

  /* get the fading colors */
  if(FgplGradCyc==True)
  {
    if(AllocGrad(FgPlasmaGrad,pi.colors,True)==True)
    {
      cyc_inf[num_used_cyc].grad_num = FgPlasmaGrad;
      cyc_inf[num_used_cyc].only_first = False;
      cyc_inf[num_used_cyc].dir = FgplCycDir;
      num_used_cyc++;
    }
  }
  else
    AllocGrad(FgPlasmaGrad,pi.colors,False);

  pi.grad = &(grad[FgPlasmaGrad]);

  /* is this necessary ??? */
  if(XB_Glint==True)
    FindCorners();
    
  /* actually do it */
  DoPlasma(disp,&pi);
  ximg = XGetImage(disp,pi.d,0,0,pi.width,pi.height,0xffffffff,ZPixmap);
  if(ximg == NULL)
    error(disp,mgc,"REPORT: PlasmaCloud XImage",True);

  /* now paint it */
  for(y=0 ; y < rel_h ; y++)
    for(x=0 ; x < rel_w ; x++)
      if(XGetPixel(scrimg,x,y)==xfgc.pixel)
        XPutPixel(scrimg,x,y,XGetPixel(ximg,x,y));
  /* destroy the image and pixmap */
  XDestroyImage(ximg);
  XFreePixmap(disp,pi.d);
} /* FG-Grad Effect */

void DoFunnyOutline(void)
{
  int x,y;		/* iterators */
  Pixel fgc,lghtc;
  int n;

  fgc   = xfgc.pixel;	/* for ease of use */
  lghtc = xlghtc.pixel;
  
  /* Do the oulines (actually inlines) */
  MakeZImageOutline(INWARDS);
  
  /* paint the outlines in the same color (=HiColor) */
  for(y=0 ; y < rel_h ; y++)
    for(x=0 ; x < rel_w ; x++)
    {
      n=ZImage[x+rel_w*y];
      if(n==0 || n==Thickness+1)
        continue;
      XPutPixel(scrimg,x,y,lghtc);
    } /* x iter */
} /* FunnyOutline Effect */

void DoStandOut(Zmethod direction)
{
  int x,y;		/* iterators */
  Pixel fgc,shdc,lghtc;
  int n;
  int neighbors[8];
  int xsum,ysum,sum;

  /* init needed in this form for Ultrix's cc */
  neighbors[0] = -rel_w-1;
  neighbors[1] = -rel_w;
  neighbors[2] = -rel_w+1;
  neighbors[3] = -1;
  neighbors[4] = 1;
  neighbors[5] = rel_w+1;
  neighbors[6] = rel_w;
  neighbors[7] = rel_w-1;

  fgc   = xfgc.pixel;	/* for ease of use */
  shdc  = xshdc.pixel;
  lghtc = xlghtc.pixel;

  /* outline (or inline) the text */
  MakeZImageOutline(direction);

  /*
     filter the ZImage to smooth things out - each pixel in FImage is the
     sum of its neighbors in ZImage plus itself.
  */
  for(y=1 ; y < rel_h-1 ; y++)
    for(x=1 ; x < rel_w-1 ; x++)
    {
      if( ZImage[x+rel_w*y] == 0 )
        continue;
      sum=ZImage[x+rel_w*y];
      for(n=0;n<8;n++)
        sum+=ZImage[x+rel_w*y+neighbors[n]];
      FImage[x+rel_w*y]=sum;
    } /* x iter */

  /* now paint the outlines according to their slope using FImage */
  for(y=1 ; y < rel_h-1 ; y++)
  {
    for(x=1 ; x < rel_w-1 ; x++)
    {
      if( ZImage[x+rel_w*y] == Thickness+1 ||	/* !interesting pixel */
          ZImage[x+rel_w*y] == 0 )
	continue;
      /*
         note: xsum is done on left and right pixel while ysum on pixels
               in neighboring rows
      */
      xsum=FImage[x+(rel_w*y)-1]-FImage[x+(rel_w*y)+1];
      ysum=FImage[x+rel_w*(y-1)]-FImage[x+rel_w*(y+1)];
      if(effect==STANDOUT || effect==STDIN2)
      {
        if(xsum > (-ysum))
          XPutPixel(scrimg,x,y,shdc);
        else
          XPutPixel(scrimg,x,y,lghtc);
      } /* StandOut */
      else /* standin... or StandOut2 */
      {
        if(xsum < (-ysum))
          XPutPixel(scrimg,x,y,shdc);
        else
          XPutPixel(scrimg,x,y,lghtc);
      }
    } /* x iter */
  } /* y iter */
} /* StandOut Effect */

/*
   Notice that the only difference between Coin and StandOut is that Coin
   let's the height go up "rim" iterations before going down again as in 
   StandOut. This means that the FG colored pixels will be painted by the
   last painting loop. Hence the call to DrawFG() again in DrawIt()...
*/
void DoCoinEffect(void)
{
#ifdef EXPLICIT_REGISTER_VARIABLES
  register n,x,y;
#else
  int n,x,y;			/* iterators */
#endif
  Pixel fgc,shdc,lghtc;
  int rim = (Thickness/3);	/* calc rim size */
  int swid = Thickness - rim;
  int hgt=swid+1;		/* start at height Thickness+1 */
  int neighbors[8];
  int xsum,ysum,sum;
  long *pix1,*pix2;
  int i,pixels=0,k=0;

  if(swid<1) swid=1;	/* just in case */
  if(rim<1)  rim=1;	/* just in case */

  /* needed for Ultrix */
  neighbors[0] = -rel_w-1;
  neighbors[1] = -rel_w;
  neighbors[2] = -rel_w+1;
  neighbors[3] = -1;
  neighbors[4] = 1;
  neighbors[5] = rel_w+1;
  neighbors[6] = rel_w;
  neighbors[7] = rel_w-1;

  fgc   = xfgc.pixel;	/* for ease of use */
  shdc  = xshdc.pixel;
  lghtc = xlghtc.pixel;
  
  /* Do ZImage ourselves! MakeZImageOutline() cannot handle a rim going up */

  for(y=0 ; y < rel_h ; y++)
    for(x=0 ; x < rel_w ; x++)
      if(XGetPixel(scrimg,x,y)==fgc)
        ZImage[x+y*rel_w]=hgt;
  
  for(y=1 ; y < rel_h-1 ; y++)
    for(x=1 ; x < rel_w-1 ; x++)
      if (ZImage[x+rel_w*y] == hgt)
        for(n=0;n<8;n++)
          if( ZImage[x+rel_w*y+neighbors[n]] == 0 )
            k++;
  pix1 = (long*)malloc(k*sizeof(long));
  if(pix1==NULL)
    error(disp,mgc,"REPORT: Alloc pix1 in Coin",True);
  for(y=1 ; y < rel_h-1 ; y++)
  {
    for(x=1 ; x < rel_w-1 ; x++)
    {
      if (ZImage[x+rel_w*y] == hgt)
      {
        for(n=0;n<8;n++)
        {
          if(ZImage[x+rel_w*y+neighbors[n]]==0)
          {
            ZImage[x+rel_w*y+neighbors[n]]=hgt+1;
            pix1[pixels++]=x+rel_w*y+neighbors[n];
          }
        } /* n */
      }
    } /* x */
  } /* y */
  hgt++;

  while(--rim)
  {
    /* the allocation is always less than 1.5 * pixels in previous outline */
    pix2 = (long*)malloc((pixels*3*sizeof(long))/2); k=0;
    if(pix2==NULL)
      error(disp,mgc,"REPORT: Alloc pix2 in Coin",True);
    for(i=0;i<pixels;i++)
    {
      for(n=0;n<8;n++)
      {
        if(ZImage[pix1[i]+neighbors[n]]==0)
        {
          ZImage[pix1[i]+neighbors[n]]=hgt;
          pix2[k++]=pix1[i]+neighbors[n];
        }
      } /* n */
    }
    pixels=k; free(pix1); pix1=pix2;
    hgt++;
  } /* rim iter */

  hgt--;
  while(--swid)
  {
    /* the allocation is always less than 1.5 * pixels in previous outline */
    pix2 = (long*)malloc((pixels*3*sizeof(long))/2); k=0;
    if(pix2==NULL)
      error(disp,mgc,"REPORT: Alloc pix2 in Coin",True);
    for(i=0;i<pixels;i++)
    {
      for(n=0;n<8;n++)
      {
        if(ZImage[pix1[i]+neighbors[n]]==0)
        {
          ZImage[pix1[i]+neighbors[n]]=hgt;
          pix2[k++]=pix1[i]+neighbors[n];
        }
      } /* n */
    }
    pixels=k; free(pix1); pix1=pix2;
    hgt--;
  } /* swid iter */

  for(y=1 ; y < rel_h-1 ; y++)
  {
    for(x=1 ; x < rel_w-1 ; x++)
    {
      if(ZImage[x+rel_w*y] == 0) /* !interesting - ZImage is init properly */
        continue;
      sum=ZImage[x+rel_w*y];
      for(n=0;n<8;n++)
        sum+=ZImage[x+rel_w*y+neighbors[n]];
      FImage[x+rel_w*y]=sum;
    } /* x iter */
  } /* y iter */

  for(y=1 ; y < rel_h-1 ; y++)
  {
    for(x=1 ; x < rel_w-1 ; x++)
    {
      if(ZImage[x+rel_w*y] == 0)	/* !interesting pixel */
        continue;
      /*
         note: xsum is done on left and right pixel while ysum on pixels
               in neighboring rows
      */
      xsum=FImage[x+(rel_w*y)-1]-FImage[x+(rel_w*y)+1];
      ysum=FImage[x+rel_w*(y-1)]-FImage[x+rel_w*(y+1)];
      if(xsum > (-ysum))
        XPutPixel(scrimg,x,y,shdc);
      else
        XPutPixel(scrimg,x,y,lghtc);
    } /* x iter */
  } /* y iter */
} /* Coin Effect */

Bool FgcGradCyc = False;
void DrawIt(void)
{
  Zmethod direction = OUTWARDS;		/* default is outwards */
  int i,j,t;

  switch(effect)
  {
    case NONE:
      /* do we make an underline? */
      if(Underlined)
        DrawUL();
      DrawFG();
      break;
    case SHAKE:
      DrawFG();		/* draw the FG so we can search what to do to it */
      if(Underlined)
        DrawUL();
      GetXImage();
      DoShakeEffect();	/* scan the image and add the shake signs */
      PutXImage();
      break;
    case SHADOW:
      /* do we make an underline? */
      if(Underlined)
        DrawUL();
      XSetForeground(disp,mgc,xshdc.pixel);
      XDrawString(disp,rootpix,mgc,
      			final_x+SHD_X_OFFSET,
      			final_y+SHD_Y_OFFSET,
      			BANNER,strlen(BANNER));
      DrawFG();
      break;
    case OUTLINE:
      /* do we make an underline? */
      if(Underlined)
        DrawUL();
      XSetForeground(disp,mgc,xlghtc.pixel);
      for(i=SUR_MIN;i<SUR_MAX+1;i++)
        for(j=SUR_MIN;j<SUR_MAX+1;j++)
          XDrawString(disp,rootpix,mgc,
          		final_x+i,final_y+j,BANNER,strlen(BANNER));
      DrawFG();
      break;
    case SHADOUTL:
      /* do we make an underline? */
      if(Underlined)
        DrawUL();
      XSetForeground(disp,mgc,xshdc.pixel);
      for(i=SUR_MIN;i<SUR_MAX+1;i++)
        for(j=SUR_MIN;j<SUR_MAX+1;j++)
          XDrawString(disp,rootpix,mgc,
          		final_x+i+SHD_X_OFFSET,
          		final_y+j+SHD_Y_OFFSET,
          		BANNER,strlen(BANNER));
      XSetForeground(disp,mgc,xlghtc.pixel);
      for(i=SUR_MIN;i<SUR_MAX+1;i++)
        for(j=SUR_MIN;j<SUR_MAX+1;j++)
          XDrawString(disp,rootpix,mgc,
          		final_x+i,final_y+j,BANNER,strlen(BANNER));
      DrawFG();
      break;
    case SHD3D:
      /* do we make an underline? */
      if(Underlined)
        DrawUL();
      for(t=SHD3D_SHADOWS ; t>0 ; t--)
      {
        XSetForeground(disp,mgc,xshdc.pixel);
        for(i=SUR_MIN;i<SUR_MAX+1;i++)
          for(j=SUR_MIN;j<SUR_MAX+1;j++)
            XDrawString(disp,rootpix,mgc,
            	final_x+i+(t*SHD_X_OFFSET),
            	final_y+j+(t*SHD_Y_OFFSET),
            	BANNER,strlen(BANNER));
        XSetForeground(disp,mgc,xfgc.pixel);
        XDrawString(disp,rootpix,mgc,
       		final_x+(t*SHD_X_OFFSET),
       		final_y+(t*SHD_Y_OFFSET),
       		BANNER,strlen(BANNER));
      }
      XSetForeground(disp,mgc,xlghtc.pixel);
      for(i=SUR_MIN;i<SUR_MAX+1;i++)
        for(j=SUR_MIN;j<SUR_MAX+1;j++)
          XDrawString(disp,rootpix,mgc,final_x+i,final_y+j,BANNER,strlen(BANNER));
      DrawFG();
      break;
    case THICK:
      /* do we make an underline? */
      if(Underlined)
        DrawUL();
      for(t=Thickness ; t>0 ; t-=1)
      {
        XSetForeground(disp,mgc,xshdc.pixel);
        XDrawString(disp,rootpix,mgc,
        	final_x+t+SUR_MIN,
        	final_y+t+SUR_MAX,
        	BANNER,strlen(BANNER));
        XDrawString(disp,rootpix,mgc,
        	final_x+t+SUR_MAX,
        	final_y+t+SUR_MIN,
        	BANNER,strlen(BANNER));
        XDrawString(disp,rootpix,mgc,
        	final_x+t+SUR_MIN,
        	final_y+t+SUR_MIN,
        	BANNER,strlen(BANNER));
        XDrawString(disp,rootpix,mgc,
        	final_x+t+SUR_MAX,
        	final_y+t+SUR_MAX,
        	BANNER,strlen(BANNER));
      }
      XSetForeground(disp,mgc,xlghtc.pixel);
      for(i=SUR_MIN;i<SUR_MAX+1;i++)
        for(j=SUR_MIN;j<SUR_MAX+1;j++)
          XDrawString(disp,rootpix,mgc,final_x+i,final_y+j,BANNER,strlen(BANNER));
      DrawFG();
      break;
    case STDOUT2:
    case STDIN2:
      direction = INWARDS;	/* this affects the insides of DoStandOut... */
    case STANDIN:
      /* StandIn will be an opposite of StandOut by simply switching the color
      	 criteria in the algorythm ... */
      /* notice that there is no break statement here on purpose! */
    case STANDOUT:
      DrawFG();		/* draw the FG so we can search what to do to it */
      if(Underlined)
        DrawUL();
      GetXImage();
      DoStandOut(direction);	/* scan the image and add the shadowed outline */
      PutXImage();
      break;
    case POPART:
      DrawFG();
      if(Underlined)
        DrawUL();
      GetXImage();
      MakeZImageOutline(OUTWARDS);
      PutXImage();
      break;
    case COIN:
      DrawFG();
      if(Underlined)
        DrawUL();
      GetXImage();
      DoCoinEffect();
      PutXImage();
      DrawFG();
      if(Underlined)
        DrawUL();
      break;
    case FGPLASMA:
      XSetForeground(disp,mgc,xshdc.pixel);
      XDrawString(disp,rootpix,mgc,
    			final_x+SHD_X_OFFSET,
    			final_y+SHD_Y_OFFSET,
    			BANNER,strlen(BANNER));
      DrawFG();
      if(Underlined)
        DrawUL();
      GetXImage();
      DoFGPlasmaEffect();
      PutXImage();
      break;
    case FGGRAD:
      XSetForeground(disp,mgc,xshdc.pixel);
      XDrawString(disp,rootpix,mgc,
    			final_x+SHD_X_OFFSET,
    			final_y+SHD_Y_OFFSET,
    			BANNER,strlen(BANNER));
      DrawFG();
      if(Underlined)
        DrawUL();
      GetXImage();
      DoFGGradEffect();
      PutXImage();
      break;
    case FADE:		/* Fade and Backlight are the same with one tiny diff.*/
    case BACKLIGHT:	/* handled inside the function of Backlight itself!   */
    case FATTEXT:	/* FatText is Backlight Inwards...		      */
      if(effect==FATTEXT)
      {
        XSetForeground(disp,mgc,xshdc.pixel);
        XDrawString(disp,rootpix,mgc,
      			final_x+SHD_X_OFFSET,
      			final_y+SHD_Y_OFFSET,
      			BANNER,strlen(BANNER));
      }
      DrawFG();
      if(Underlined)
        DrawUL();
      GetXImage();
      DoBacklightEffect();
      PutXImage();
      break;
    case FUNNYOUTL:
      DrawFG();
      if(Underlined)
        DrawUL();
      GetXImage();
      DoFunnyOutline();
      PutXImage();
      break;
  } /* switch(effect) */

  /* handle -cycle FGC */
  if(FgcGradCyc==True && FgCycGrad != -1)
  {
    if(AllocGrad(FgCycGrad,FgCycNumCols,False)==True)
    {
      cyc_inf[num_used_cyc].grad_num = FgCycGrad;
      cyc_inf[num_used_cyc].only_first = True;
      cyc_inf[num_used_cyc].dir = CYC_FORWARD;
      num_used_cyc++;
    }
    else
      error(disp,mgc,"Could not allocate grad for FGC cycle???",False);
  }
} /* DrawIt() */
