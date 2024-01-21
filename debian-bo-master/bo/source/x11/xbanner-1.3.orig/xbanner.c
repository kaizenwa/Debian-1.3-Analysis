/* our config file */

#include "xb_config.h"

/* C library includes */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>

#ifdef vms
#include <ssdef.h>
#endif

/* X11 include(s) */
#include <X11/Xlib.h>

#include "xbanner.h"

char BANNER[MAX_BANNER_LEN] = BANNER_DEFVAL;

int  XDEFOFFSET       = XOFFSET_DEFVAL;
int  YDEFOFFSET       = YOFFSET_DEFVAL;
int  SHD_X_OFFSET     = SHDXOFFS_DEFVAL;
int  SHD_Y_OFFSET     = SHDYOFFS_DEFVAL;

int ULthickness	      = ULTHICK_DEFVAL;

char FONTN[256] = FONT_DEFVAL;

int SUR_MIN = SURMIN_DEFVAL;
int SUR_MAX = SURMAX_DEFVAL;

int Thickness = THICKNESS_DEFVAL;

int SHD3D_SHADOWS = SHD3DSHADOWS_DEFVAL;

/* no more defines */

Display        *disp;
GC		mgc;
Colormap	cmap;
Pixmap		rootpix;
Window		root;

/* for any R/W colors capable display */
Pixel color_cell;

int sc_w,sc_h;			/* screen width/height			*/
int dir,fasc,fdsc,lbear,rbear;	/* font ascent/descent/dir/bearing	*/
int bwid;			/* banner pixel width			*/
int final_x,final_y;		/* where the text will finally appear	*/
Placement placement = PLACEMENT_DEFVAL;
Effect effect = EFFECT_DEFVAL;
Bool Underlined = UNDERLINED_DEFVAL;

/* relevant area x,y,height,width */
int rel_x,rel_y,rel_h,rel_w;

/* do we want to show the sizes of the calculated area needed ? */
Bool show_sizes=SHOWSIZE_DEFVAL;

/* no more global vars */

/* calculate the location of the text */
static void CalcXY(void)
{
  int fx,fy;

  fx=final_x; fy=final_y;
  
  /* first make preliminary guess */
  rel_h = 32 + fasc + fdsc + (Underlined?4*ULthickness:0);
  rel_w = 32 + bwid + (lbear<0?-lbear:0) + (Underlined?20+Thickness:0);
  final_y=fasc+1;
  final_x=0;		/* important do not remove! */

  /* now each effect has different needs */
  switch(effect)
  {
    case SHAKE:
      rel_h   += 18;
      rel_w   += 32;
      break;
    case STANDOUT:
    case STANDIN:
    case POPART:
    case COIN:
    case BACKLIGHT:
      rel_h   += 2*Thickness+2;
      rel_w   += 7*Thickness+7;
      final_x += 3*Thickness+3;
      final_y += Thickness+1;
      break;
    case NONE:
    case STDOUT2:
    case STDIN2:
    case FUNNYOUTL:
      rel_w   += 6*Thickness+6;
      final_x += 2*Thickness+2;
      break;
    case FADE:
    case THICK:
      rel_h   += 2*Thickness+2;
      rel_w   += 3*Thickness+3;
      final_x += Thickness+1;
      break;
    case SHADOUTL:
      rel_h   += 2*(SUR_MAX-SUR_MIN)+2 + 2*SHD_Y_OFFSET+2;
      rel_w   += 4*(SUR_MAX-SUR_MIN)+4 + 4*SHD_X_OFFSET+4;
      final_x += 2*(SUR_MAX-SUR_MIN) + 2*SHD_X_OFFSET;
      final_y += 2*(SUR_MAX-SUR_MIN) + 2*SHD_Y_OFFSET;
      break;
    case OUTLINE:
      rel_h   += 2*(SUR_MAX-SUR_MIN);
      rel_w   += 2*(SUR_MAX-SUR_MIN);
      final_x += (SUR_MAX-SUR_MIN);
      final_y += (SUR_MAX-SUR_MIN);
      break;
    case FGGRAD:
    case FGPLASMA:
    case SHADOW:
    case FATTEXT:
      rel_w   += 6*SHD_X_OFFSET+6;
      rel_h   += 2*SHD_Y_OFFSET+2;
      final_x += 3*SHD_X_OFFSET+3;
      break;
    case SHD3D:
      rel_h   += ( 2*(SUR_MAX-SUR_MIN) + SHD3D_SHADOWS*SHD_Y_OFFSET );
      rel_w   += ( 4*(SUR_MAX-SUR_MIN) + SHD3D_SHADOWS*SHD_X_OFFSET );
      final_x += 2*(SUR_MAX-SUR_MIN);
      final_y += (SUR_MAX-SUR_MIN);
      break;
    default:		/* default is sc_w*sc_h starting at 0,0 */
      final_x=sc_w/2; final_y=sc_h/2;
      rel_w = sc_w;
      rel_h = sc_h;
  } /* switch(effect) */

  /* now find rel_x and rel_y according to placement */
  switch(placement)
  {
    case TOPLEFT:
      rel_y=0;
      rel_x=0;
      break;
    case TOPRIGHT:
      rel_y=0;
      rel_x=sc_w-rel_w;
      break;
    case BOTLEFT:
      rel_y=sc_h-rel_h;
      rel_x=0;
      break;
    case BOTRIGHT:
      rel_y=sc_h-rel_h;
      rel_x=sc_w-rel_w;
      break;
    case TOPCENT:
      rel_y=0;
      rel_x=(sc_w/2) - (rel_w/2);
      break;
    case BOTCENT:
      rel_y=sc_h-rel_h;
      rel_x=(sc_w/2) - (rel_w/2);
      break;
    case CENTER:
      rel_y=(sc_h/2) - (rel_h/2);
      rel_x=(sc_w/2) - (rel_w/2);
      break;
    case CTRONY:
      rel_x=(sc_w/2) - (rel_w/2);
      rel_y=fy;
      break;
    case XY:	/* XY placement */
      rel_x=fx;
      rel_y=fy;
      break;
  } /* switch(placement) */

  /* let's show the sizes */
  if(show_sizes==True)
  {
    fprintf(stderr,"Calculated area for the text will be:\n");
    fprintf(stderr,"( X , Y ) - (Width,Height):\n");
    fprintf(stderr,"(%3d,%3d) - (%4d,%4d)\n",rel_x,rel_y,rel_w,rel_h);
  }
} /* CalcXY() */

int main(int argc, char *argv[])
{
  XFontStruct  *xfont;
  Screen       *scrn;
  int		scrn_no;
  XCharStruct	xchrs;
  time_t	now;
  int           i;

  /* randomize in case we need any random numbers */
  now = time(NULL);
  srand((unsigned int)now);     /* randomize */

  /* make sure we don't think any of the grad[]s are in use */
  for(i=0;i<NUM_GRADS;i++)
    grad[i].Used=False;

  /* possible cycle things */
  for(i=0;i<MAX_CYCLES;i++)
  {
    cyc_inf[i].grad_num	  = -1;
    cyc_inf[i].dir	  = CYC_FORWARD;
    cyc_inf[i].only_first = False;

    /* the following are not used in v1.3 but will be used in the future */

    cyc_inf[i].redc	  = 1;
    cyc_inf[i].greenc	  = 1;
    cyc_inf[i].bluec	  = 1;
    cyc_inf[i].dir_count  = 0;
    cyc_inf[i].dir_steps  = 0;
  }

  /* do the entire X resource databases thing - this also opens the display */
  DoResources(&argc,argv);

  /* get some of the information about the screen */
  scrn    = XDefaultScreenOfDisplay(disp);  /* get the default screen	*/
  scrn_no = XDefaultScreen(disp);	    /* get the default screen #	*/
  root    = RootWindow(disp,scrn_no);	    /* the root window		*/
  mgc     = DefaultGC(disp,scrn_no);	    /* get the root window GC	*/
  cmap    = XDefaultColormapOfScreen(scrn); /* get the colormap		*/
  sc_w    = DisplayWidth(disp,scrn_no);     /* get pixel width of screen*/
  sc_h    = DisplayHeight(disp,scrn_no);    /* get pixel height of screen*/

  /* some active things for the X server to do about the font... */
  xfont = XLoadQueryFont(disp,FONTN);		/* ask for the font */
  if(xfont == NULL)
  {
    error(disp,mgc,"Could not get the font, using 'fixed'",False);
    xfont = XLoadQueryFont(disp,"fixed");
  }
  XSetFont(disp,mgc,xfont->fid);		/* make the GC use this font */

  /* get text extent information */
  XQueryTextExtents(disp,xfont->fid,BANNER,strlen(BANNER),
  			&dir,&fasc,&fdsc,&xchrs);
  lbear = (xfont->per_char[0]).lbearing;      /* important global varibles */
  rbear = (xfont->per_char[strlen(BANNER)]).rbearing;
  bwid  = XTextWidth(xfont,BANNER,strlen(BANNER));

  /* calc relevant area */
  CalcXY();					/* calc the X/Y pos first */

  /* now let's do some bound and sanity checking */
  if(rel_y<0)
    error(disp,mgc,"REPORT: rel_y < 0",True);
  if(rel_y>sc_h)
    error(disp,mgc,"REPORT: rel_y > sc_h",True);
  if(rel_x>sc_w)
    error(disp,mgc,"REPORT: rel_x > sc_w",True);
  if(rel_w > sc_w)
    error(disp,mgc,"Your text is probably too wide",True);
  if(rel_h > sc_h)	/* do you think this can happen??? */
    error(disp,mgc,"Your text is too tall. Use smaller font",False);
  if(rel_x<0)
    error(disp,mgc,"REPORT: rel_x < 0",True);

  AllocColors();		/* ask for the main colors */

  /************************************************************************

  The algorythm is simple: I make a pixmap the size of the entire screen,
  copy the root window into the pixmap, calculate where the text will be
  placed according to the placement, then I draw the text with the effect
  into the pixmap. Some effects use some more resources. After that, we
  copy the pixmap back onto the root window.

  ************************************************************************/
  
  rootpix = XCreatePixmap(disp,root,			/* create the pixmap */
  			  rel_w,rel_h,
  			  DefaultDepth(disp,scrn_no));

  /* do the window background... */
  if(bgstyle != NOBG || do_fill)
    DoBackground();

  /* I copy, render and copy back so you don't see it drawing */
  XCopyArea(disp,root,rootpix,mgc,			/* XRoot->pixmap     */
  	rel_x,rel_y,rel_w,rel_h,0,0);	
  DrawIt();						/* draw the effect   */
  /* copy the effect to the root window */
  XCopyArea(disp,rootpix,root,mgc,0,0,rel_w,rel_h,rel_x,rel_y);

#ifdef HAS_XPM
  /* All do the pixmap thingy */
  DoPastePixmap();
#endif

  /* All done. Free the resources and close the connection */
  XFreeFont(disp,xfont);			/* done with this font       */
  XFreePixmap(disp,rootpix);			/* done with the pixmap      */

  /* do we wanna see them warnings and non-fatal errors??? */
  if(show_errors)
    display_errors(disp,mgc);

  /* do we want to linger?? */
  if(XB_Linger == True)
#ifdef vms
  {
    fprintf(stderr,"%s: No linger support on OpenVMS, hence color-cycling and glint are also\nnot supported on this platform.\n",PRGCLASS);
    exit(1);
  }
#else
    DoLinger();
#endif

  XSetCloseDownMode(disp,RetainTemporary);	/* I want the colors to stay */
  XCloseDisplay(disp);				/* close the display	     */
#ifdef vms
  return SS$_NORMAL;
#else
  return 0;
#endif
}
