#include "xb_config.h"

#ifndef vms
#include <unistd.h>
#else
#include <ssdef.h>
#endif
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>

#include <X11/Xlib.h>
#include <X11/Xatom.h>

/*
#ifdef vms
#define fork() vfork()
#endif
*/

#include "xbanner.h"

/* globals which go external */
int  CornerMask   = DownRight|DownLeft|UpRight|UpLeft; /* corner types to glint on */
int  GlintSizeMin = GLINTSIZE_MINVAL;   /* glint stars are at least this size */
int  GlintSizeMax = GLINTSIZE_MAXVAL;      /* and no larger than this size  */
int  glint_speed  = GLINTSPEED_DEFVAL;     /* speed of stars		    */
int  CycleSpeed   = CYCLESPEED_DEFVAL;     /* divisor of LINGER_DELAY       */
Bool XB_Linger    = XB_LINGER_DEFVAL;      /* linger after drawing is done? */
Bool XB_Glint     = XB_GLINT_DEFVAL;       /* do we do the stars thingy?    */
long GlintMaxTime = GLINTMAXTIME_DEFVAL;   /* max time between glint sparks */
long GlintMinTime = GLINTMINTIME_DEFVAL;   /* min time between glint sparks */

/* Glinting stuff */
Corner *CornerList;
int NumCorners = 0;

#if 0
/* cycle the red-component. Not yet adapted to the new cycling method */
void RedCycle(Display *disp,Bool store)
{
  static int i;
  static unsigned short r;

  r = CycleColors[0].red;
  for(i=0;i<NumCycleColors-1;i++)
    CycleColors[i].red = CycleColors[i+1].red;
  CycleColors[i].red = r;
  if(store)
    XStoreColors(disp,cmap,CycleColors,NumCycleColors);
}

/* cycle the green-component. Not yet adapted to the new cycling method */
void GreenCycle(Display *disp,Bool store)
{
  static int i;
  static unsigned short g;

  g = CycleColors[0].green;
  for(i=0;i<NumCycleColors-1;i++)
    CycleColors[i].green = CycleColors[i+1].green;
  CycleColors[i].green = g;
  if(store)
    XStoreColors(disp,cmap,CycleColors,NumCycleColors);
}

/* cycle the blue-component. Not yet adapted to the new cycling method */
void BlueCycle(Display *disp,Bool store)
{
  static int i;
  static unsigned short b;

  b = CycleColors[0].blue;
  for(i=0;i<NumCycleColors-1;i++)
    CycleColors[i].blue = CycleColors[i+1].blue;
  CycleColors[i].blue = b;
  if(store)
    XStoreColors(disp,cmap,CycleColors,NumCycleColors);
}
#endif

/* Show one star only. This draws a single star on a given location */
static void DoStar(Display *disp,Window w,GC mgc,int x,int y,int size,
			Pixel pixel)
{
  /* choose the color */
  XSetForeground(disp,mgc,pixel);

  /* make the central cross */
  XDrawLine(disp,w,mgc,x-size/2,y,x+size/2,y);
  XDrawLine(disp,w,mgc,x,y-size/2,x,y+size/2);
  if(size>12)	/* make the middle thicker */
  {
    XDrawLine(disp,w,mgc,x-size/4,y+1,x+size/4,y+1);
    XDrawLine(disp,w,mgc,x-size/4,y-1,x+size/4,y-1);
    XDrawLine(disp,w,mgc,x+1,y-size/4,x+1,y+size/4);
    XDrawLine(disp,w,mgc,x-1,y-size/4,x-1,y+size/4);
    XDrawPoint(disp,w,mgc,x-2,y-2);	/* the points are for smoothing */
    XDrawPoint(disp,w,mgc,x+2,y-2);
    XDrawPoint(disp,w,mgc,x-2,y+2);
    XDrawPoint(disp,w,mgc,x+2,y+2);
    if(size>24)	/* make the middle even thicker */
    {
      XDrawLine(disp,w,mgc,x-size/8,y+2,x+size/8,y+2);
      XDrawLine(disp,w,mgc,x-size/8,y-2,x+size/8,y-2);
      XDrawLine(disp,w,mgc,x+2,y-size/8,x+2,y+size/8);
      XDrawLine(disp,w,mgc,x-2,y-size/8,x-2,y+size/8);
      XDrawPoint(disp,w,mgc,x-3,y-3);
      XDrawPoint(disp,w,mgc,x+3,y-3);
      XDrawPoint(disp,w,mgc,x-3,y+3);
      XDrawPoint(disp,w,mgc,x+3,y+3);
    }
  }
  /* look out! no XSync() here! */
} /* DoStar() */

/* make a star appear and disappear, from small size to large and back */
void BlinkStar(Display *disp, Window w, int cx, int cy, int size, unsigned long pixel)
{
  Pixmap    bgpix;		/* to keep old area before star was drawn */
  XGCValues gcv;
  int       i,scrn_no;
  volatile  int j;	/* volatile so optimizers won't remove my idle-loop */

  /* bounds checks - don't try to draw where you shouldn't */
  if(cx-size/2-1 < 0 || cx+size/2+1 >= sc_w ||
     cy-size/2-1 < 0 || cy+size/2+1 >= sc_h)
    return;

  scrn_no = XDefaultScreen(disp);       /* get the default screen #     */
  mgc = XCreateGC(disp,w,0,&gcv);	/* create us a GC */
  /* make a pixmap and copy the area into it taking some margin too */
  bgpix = XCreatePixmap(disp,w,size+3,size+3,DefaultDepth(disp,scrn_no));
  XCopyArea(disp,w,bgpix,mgc,cx-size/2-1,cy-size/2-1,size+3,size+3,0,0);

  /* 
     Blinking one star is simple. I copy the area under it, draw it from
     size 3 to the requested size in glint_speed steps, then I return
     the copied area, and draw smaller until it disappears
  */

  /* 0 is a blinking speed - for really slow machines */
  if(glint_speed != 0)
  {
    if(glint_speed > 0)
    {
      for (i=3 ; i<size ; i += glint_speed)
      {
        DoStar(disp,w,mgc,cx,cy,i,pixel);
        XSync(disp,False);			/* must do that here! */
      } /* grow star */
      for ( ; i>2 ; i -= glint_speed)
      {
        XCopyArea(disp,bgpix,w,mgc,0,0,size+3,size+3,cx-size/2-1,cy-size/2-1);
        DoStar(disp,w,mgc,cx,cy,i-1,pixel);
        XSync(disp,False);
      } /* shrink star */
    }
    else /* glint_speed < 0 */
    {
      for (i=3 ; i<size ; i ++)
      {
        DoStar(disp,w,mgc,cx,cy,i,pixel);
        XSync(disp,False);			/* must do that here! */
        for(j=glint_speed;j;j++) ;		/* j is volatile - delay */
      } /* grow star */
      for ( ; i>2 ; i --)
      {
        XCopyArea(disp,bgpix,w,mgc,0,0,size+3,size+3,cx-size/2-1,cy-size/2-1);
        DoStar(disp,w,mgc,cx,cy,i-1,pixel);
        XSync(disp,False);
        for(j=glint_speed;j;j++) ;
      } /* shrink star */
    } /* else of if (glint_speed > 0) */
  }
  else /* glint_speed == 0! */
  {
    DoStar(disp,w,mgc,cx,cy,size,pixel);	/* show the star */
    XSync(disp,False);				/* must do that here! */
    usleep(BLINK_SPEED_TIME);			/* sleep a while */
  }
  /* restore normality */
  XCopyArea(disp,bgpix,w,mgc,0,0,size+3,size+3,cx-size/2-1,cy-size/2-1);
  XFreeGC(disp,mgc);		/* and free resources */
  XFreePixmap(disp,bgpix);
  XSync(disp,False);
} /* BlinkStar() */

/*
  The Linger() algorithm:
  
  First client writes a property named XPROPERTY, and gives it the value
  "Li%d" where %d is a single digit stating this client's number. In this
  case it will be 0 because it is the first.
  
  Each additional client simply adds one to the number, and adds the new
  digit to the end of the property.
  
  When freetemp runs, it reads the property, and selects the last client,
  writes the first 2 chars of the property as "X%d", to ask that client
  to exit. The client will respond with "D%d". That's X for eXit, and D
  for Done.
*/
static void Linger(Display *disp)
{
  int            cnt=0,idx,format;
  Bool           done=False;
  Atom           xprop,proptype;
  unsigned long	 items,bytesleft;
  unsigned char *propval;
  unsigned char  newval[21];
  int		 my_number=0;
  int            scrn_no;
  GC		 mgc;
  XEvent	 evnt;

  /* some initializations */
  scrn_no = XDefaultScreen(disp);       /* get the default screen #     */
  mgc     = DefaultGC(disp,scrn_no);    /* get the root window GC	*/
  GlintMaxTime *= 1000;			/* user specifies in millisecs	*/
  GlintMinTime *= 1000;			/* but usleep needs it in usec	*/

  /* Get an Atom for the property, or make one */
  xprop = XInternAtom(disp,XPROPERTY,False);

  /* check if the property already exists */
  if(XBPropExists(disp,root,xprop))
  {
    /* check if it is our property */
    XGetWindowProperty(disp,	/* Display	 */
    		root,		/* Window	 */
    		xprop,		/* Property	 */
    		0,20,		/* offset,len	 */
    		False,		/* delete	 */
    		XA_STRING,	/* type		 */
    		&proptype,	/* actual type	 */
    		&format,	/* actual format */
    		&items,		/* items read	 */
    		&bytesleft,	/* bytes left	 */
    		&propval);	/* data return	 */
    /* check the type to see it really is ours */
    if(proptype != XA_STRING || format != 8 || bytesleft > 0)
      error(disp,mgc,"Can't linger, property conflict",True);

    /* check to make sure no race conditions occur */
    if(propval[0]!='L' || propval[1]!='i')	/* Freetemp already running? */
      error(disp,mgc,"Can't linger. Freetemp is running",True);

    /* add our number into the linger-list */
    my_number = 1 + propval[strlen((char*)propval)-1] - '0';
    if(my_number>9)
      error(disp,mgc,"Too many linger processes",True);
    sprintf((char*)newval,"%s%d",propval,my_number);
    XFree(propval);		/* free it up */
  } /* XBPropExists() */
  else
    strcpy((char*)newval,"Li0");

  /* set the property (or change it to show we are waiting also) */
  XChangeProperty(disp,root,		/* disp & win	*/
  		xprop,			/* property	*/
  		XA_STRING,		/* type		*/
  		8,			/* format	*/
  		PropModeReplace,	/* mode		*/
  		newval,			/* data		*/
  		strlen((char*)newval));	/* num elem	*/

  /* prepare to do stars - determine first random time */
  if((XB_Glint==True) && (NumCorners > 0))
    cnt = (rand()%(GlintMaxTime-GlintMinTime)+GlintMinTime)/LINGER_DELAY;
        
  /* main linger loop */
  while(!done)
  {
    /* first let's see if we were asked to exit */
    XGetWindowProperty(disp,	/* Display	 */
    		root,		/* Window	 */
    		xprop,		/* Property	 */
    		0,20,		/* offset,len	 */
    		False,		/* delete	 */
    		XA_STRING,	/* type		 */
    		&proptype,	/* actual type	 */
    		&format,	/* actual format */
    		&items,		/* items read	 */
    		&bytesleft,	/* bytes left	 */
    		&propval);	/* data return	 */
    /* no need to check type, we assume nobody will have changed it */
    if(propval[0]=='X' && propval[1]-'0' == my_number)	/* X for eXit */
      break;	/* exit the while() loop */

    /* Last thing is to free up the propval thingy */
    XFree(propval);

    /* Let's remove events from the queue so we don't get swollen */
    while(XPending(disp))
      XNextEvent(disp,&evnt);
 
    /* done checking if we need to exit - so sleep a while */
    usleep(LINGER_DELAY / CycleSpeed);

    /* ======= from here we start checking the effects ==================== */

    /* let's see if there's anything to glint on */
    if((XB_Glint==True) && (NumCorners > 0))
    {
      if(--cnt==0)
      {
        /* choose next time randomly */
        cnt = (rand()%(GlintMaxTime-GlintMinTime)+GlintMinTime)/(LINGER_DELAY/CycleSpeed);
	/* choose a corner randomly */
        idx = rand() % NumCorners;
	/* and blink it */
        BlinkStar(disp,root,
        	CornerList[idx].x,CornerList[idx].y,
        	(rand()%(GlintSizeMax-GlintSizeMin))+GlintSizeMin,
        	WhitePixel(disp,scrn_no));
      }
    } /* glint = true, corners>0 */

    /* color-cycle. This func checks if there's anything to do at all */
    DoColorCycling(disp);

  } /* while (!done) */

  /* so we are exiting, eh? Let's tell freetemp */
  propval[0]='D';		/* D for Done */
  propval[1]=my_number+'0';	/* just for safety */

  /* re-set the property to signify we are exiting */
  XChangeProperty(disp,root,		/* disp & win	*/
		xprop,			/* property	*/
		XA_STRING,		/* type		*/
		8,			/* format	*/
		PropModeReplace,	/* mode		*/
		propval,		/* data		*/
		strlen((char*)propval));/* num elem	*/
  /* free the propval */
  XFree(propval);

  /* just for safety - only freetemp will really free those resources */
  XSetCloseDownMode(disp,RetainTemporary);
  XCloseDisplay(disp);
#ifdef vms
  exit(SS$_NORMAL);
#else
  exit(0);
#endif
} /* Linger() */

/* This is the function that calls fork() - to make the child */
void DoLinger(void)
{
  Display *disp;

  /* Must open the display for the lingering child before the parent closes */
  if(dispname[0]=='\0')
    disp = XOpenDisplay(NULL);
  else
    disp = XOpenDisplay(dispname);
  if(disp==NULL)
  {
    fprintf(stderr,"%s: DoLinger() failed to open the display!\n",PRGCLASS);
    return;
  }

  if(fork() == 0)	/* the child lingers... */
    Linger(disp);
  /* the parent returns to main() and exits */
}
