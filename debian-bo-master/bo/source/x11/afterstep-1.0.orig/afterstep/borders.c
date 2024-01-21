/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 *
 * modified later for BowMan
 * by Bo Yang
 *
 * modified again for AfterStep
 * by Frank Fejes
 *
****************************************************************************/
/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/


/***********************************************************************
 *
 * afterstep window border drawing code
 *
 ***********************************************************************/

#include "../configure.h"

#include <stdio.h>
#include <signal.h>
#include <string.h>

#include "afterstep.h"
#include "menus.h"
#include "misc.h"
#include "parse.h"
#include "screen.h"
#include "module.h"
#ifdef ENABLE_TEXTURE
#include "stepgfx.h"
#endif

#ifdef SHAPE
#include <X11/extensions/shape.h>
#endif

/* macro to change window background color/pixmap */
#define ChangeWindowColor(window) {\
        if(NewColor)\
           {\
             XChangeWindowAttributes(dpy,window,valuemask, &attributes);\
             XClearWindow(dpy,window);\
           }\
         }

extern Window PressedW;



/**************************************************************
 * 
 * Sets the window background to the correct texture 
 * 
 **************************************************************/
#define STATE_FOCUSED	0
#define STATE_UNFOCUSED 1
#define STATE_STICKY	2
void SetBackgroundTexture(ASWindow *t, Window win, int type, int state)
{
    Pixmap grad=None, tile=None;
    unsigned long pixel=0;

    switch(state) {
     case STATE_FOCUSED:
	grad = t->backPixmap;
	tile = Scr.ForeTitle;
	pixel = Scr.HiColors.back;
	break;
     case STATE_UNFOCUSED:
	grad = t->backPixmap2;
	tile = Scr.BackTitle;
	pixel = Scr.StdColors.back;	
	break;
     case STATE_STICKY:
	grad = t->backPixmap3;
	tile = Scr.StickyTitle;
	pixel = Scr.StickyColors.back;	
	break;
    }
    switch(type) {
     case 0:
	XSetWindowBackground(dpy, win, pixel);
	break;
     case TEXTURE_PIXMAP:
	XSetWindowBackgroundPixmap(dpy, win, tile);
	break;
     default: /* gradients */
	XSetWindowBackgroundPixmap(dpy, win, grad);
	break;
    }
    XClearWindow(dpy, win);
    XFlush(dpy);
}

/************************************************************************
 * 
 * Sets the window borders to the correct background
 * 
 **********************************************************************/
int SetTitleBackground(ASWindow *t, Bool focused)
{
    int state, type;
    /* the title background */
    if (focused) {
	state = STATE_FOCUSED;
	type = Textures.Ttype;
    } else {
	if (t->flags & STICKY) {
	    state = STATE_STICKY;
	    type = Textures.Stype;
	} else {
	    state = STATE_UNFOCUSED;
	    type = Textures.Utype;
	}
    }

    SetBackgroundTexture(t, t->title_w, type, state);
    return type;
}

void SetBottomBackground(ASWindow *t, Bool focused)
{
    int state, type;

    /* the title background */
    if (focused) {
	state = STATE_FOCUSED;
	type = Textures.Ttype;
    } else {
	if (t->flags & STICKY) {
	    state = STATE_STICKY;
	    type = Textures.Stype;
	} else {
	    state = STATE_UNFOCUSED;
	    type = Textures.Utype;
	}
    }
    if (!(Textures.flags & TexturedHandle)) {
	/* if the user wants the NeXT style... */
	state = STATE_UNFOCUSED;
	type = 0;
    }
    /* middle bottom frame */
    SetBackgroundTexture(t, t->side, type, state);
    /* side bottom frames */
    SetBackgroundTexture(t, t->corners[0], type, state);
    SetBackgroundTexture(t, t->corners[1], type, state);
}

/****************************************************************************
 *
 * Redraws the windows borders
 *
 ****************************************************************************/
void SetBorder (ASWindow *t, Bool onoroff,Bool force,Bool Mapped, 
		Window expose_win)
{
  Window w=None;
  int y, i, x;
  GC ReliefGC,ShadowGC;
  GC HReliefGC,HShadowGC;
  Pixel BorderColor,BackColor;
  Pixmap BackPixmap,TextColor;
  Bool NewColor = False;
  XSetWindowAttributes attributes;
  unsigned long valuemask;
  static unsigned int corners[2];

  corners[0] = BOTTOM_HILITE | LEFT_HILITE;
  corners[1] = BOTTOM_HILITE | RIGHT_HILITE;
  
  if(!t)
    return;
  
  if (onoroff) 
    {
      /* don't re-draw just for kicks */
      if((!force)&&(Scr.Hilite == t))
	return;
      
      
      if(Scr.Hilite != t)
	NewColor = True;

      /* make sure that the previously highlighted window got unhighlighted */
      if((Scr.Hilite != t)&&(Scr.Hilite != NULL))
	SetBorder(Scr.Hilite,False,False,True,None);

      /* set the keyboard focus */
      if((Mapped)&&(t->flags&MAPPED)&&(Scr.Hilite != t))
	  w = t->w;
      Scr.Hilite = t;

      TextColor = Scr.HiColors.fore;
      BackPixmap= Scr.gray_pixmap;
      BackColor = Scr.HiColors.back;
	ReliefGC = Scr.StdReliefGC;
	ShadowGC = Scr.StdShadowGC;

#ifdef ENABLE_TEXTURE
	if (Textures.flags & TitlebarNoPush && 
	    Textures.flags & TexturedHandle && Scr.BevelReliefGC!=None) {
	    HReliefGC = Scr.BevelReliefGC;
	    HShadowGC = Scr.BevelShadowGC;		  
	} else {
	    if (t->flags & STICKY) {
		HReliefGC = Scr.StickyReliefGC;
		HShadowGC = Scr.StickyShadowGC;		
	    } else {	      
		HReliefGC = ReliefGC;
		HShadowGC = ShadowGC;
	    }	      
	}
#else
	HReliefGC = ReliefGC;
	HShadowGC = ShadowGC;
#endif
      BorderColor = Scr.HiRelief.back;
    }
  else
    {
      /* don't re-draw just for kicks */
	if((!force)&&(Scr.Hilite != t))	return;
	
	if(Scr.Hilite == t) {
	    Scr.Hilite = NULL;
	    NewColor = True;
	}
		
	TextColor =t->TextPixel;
	BackPixmap = Scr.light_gray_pixmap;
	if(t->flags & STICKY)
	  BackPixmap = Scr.sticky_gray_pixmap;
	BackColor = t->BackPixel;
	if(t->BackPixel != Scr.StdColors.back)
	{
	    Globalgcv.foreground = t->ReliefPixel;
	    Globalgcm = GCForeground;
	    XChangeGC(dpy,Scr.ScratchGC1,Globalgcm,&Globalgcv); 
	    ReliefGC = Scr.ScratchGC1;
	    
	    Globalgcv.foreground = t->ShadowPixel;
	    XChangeGC(dpy,Scr.ScratchGC2,Globalgcm,&Globalgcv); 
	    ShadowGC = Scr.ScratchGC2;
	}
	else
	{
	    ReliefGC = Scr.StdReliefGC;
	    ShadowGC = Scr.StdShadowGC;
	}
	BorderColor = t->ShadowPixel;
	HReliefGC = ReliefGC;
	HShadowGC = ShadowGC;
    }
#ifndef NO_PAGER
  if((Scr.Pager_w) && !(t->flags & STICKY))
    {
      if(NewColor)
	{
	  if(Scr.d_depth < 2)
	    XSetWindowBackgroundPixmap(dpy,t->pager_view,BackPixmap);
	  else
	    XSetWindowBackground(dpy,t->pager_view,BackColor);
	  XClearWindow(dpy,t->pager_view);
	}
      if((t->icon_name != NULL)&&(Scr.PagerFont.height > 0))
	{
	  NewFontAndColor(Scr.PagerFont.font->fid,TextColor,BackColor);
	  XDrawImageString(dpy, t->pager_view, Scr.FontGC, 2,Scr.PagerFont.y+2,
			   t->icon_name, strlen(t->icon_name));
	}
    }
#endif
  
  if(t->flags & ICONIFIED)
    {
      DrawIconWindow(t);
      return;
    }
  
  valuemask = CWBorderPixel;
  attributes.border_pixel = BorderColor;
  if(Scr.d_depth < 2)
    {
      attributes.background_pixmap = BackPixmap;
      valuemask |= CWBackPixmap;
    }
  else
    {
      attributes.background_pixel = BackColor;
      valuemask |= CWBackPixel;
    }
  
  if(t->flags & (TITLE|BORDER))
    {
       XSetWindowBorder(dpy,t->Parent,BlackPixel(dpy, DefaultScreen(dpy)));
       XSetWindowBorder(dpy,t->frame,BlackPixel(dpy, DefaultScreen(dpy)));
    }
  if(t->flags & TITLE) {
    ChangeWindowColor(t->title_w);
    for(i=0;i<Scr.nr_left_buttons;i++) {
      if(t->left_w[i] != None) {
	if(flush_expose(t->left_w[i]) ||
	   (expose_win == t->left_w[i]) ||
	   (expose_win == None)) {
	  /* fsf first sighting */
	  RelieveWindow(t,t->left_w[i],0,0,t->button_height,
			t->button_height,
			(PressedW==t->left_w[i]?ShadowGC:ReliefGC), 
			(PressedW==t->left_w[i]?ReliefGC:ShadowGC), 
			BOTTOM_HILITE|RIGHT_HILITE);
	  switch(Scr.button_style[i*2+1]) {
	  case XPM_BUTTON_STYLE:
	    XCopyArea(dpy, Scr.button_pixmap[i*2+1], t->left_w[i],
		      Scr.LineGC, 0, 0, Scr.button_width[i*2+1],
		      Scr.button_height[i*2+1], 2, 2);
	    break;
	  default:
	    afterstep_err( "old button styles should not be used\n", NULL,NULL,NULL);
	    exit(1);
	  }
	}
      }
    }

    for(i=0;i<Scr.nr_right_buttons;i++)	{
      if(t->right_w[i] != None) {
	if(flush_expose(t->right_w[i]) ||
	   (expose_win==t->right_w[i]) ||
	   (expose_win == None)) {

	  RelieveWindow(t,t->right_w[i],0,0,t->button_height,
			t->button_height,
			(PressedW==t->right_w[i]?ShadowGC:ReliefGC), 
			(PressedW==t->right_w[i]?ReliefGC:ShadowGC), 
			BOTTOM_HILITE|RIGHT_HILITE);
	  switch (Scr.button_style[(i*2+2)%10]) {
	  case XPM_BUTTON_STYLE:
	    XCopyArea(dpy, Scr.button_pixmap[(i*2+2)%10], t->right_w[i],
		      Scr.LineGC, 0, 0, Scr.button_width[(i*2+2)%10],
		      Scr.button_height[(i*2+2)%10], 2, 2);
	    break;
	  default:
	    afterstep_err( "old button styles should not be used\n",NULL,NULL,NULL);
	    exit(1);
	  }
	}
      }
    }
    SetTitleBar(t,onoroff, False);
  }
      
  if(t->flags & BORDER )
    {
      /* draw relief lines */

        y = t->frame_height - 2*t->corner_width;
        x = t->frame_width -  2*t->corner_width +t->bw;
	SetBottomBackground(t, onoroff);
	/*
      if((flush_expose (t->side))||(expose_win == t->side)||
	     (expose_win == None)) {*/

	  RelieveWindow(t,t->side,0,0,x,t->boundary_height,
			HReliefGC, HShadowGC, 0x0004);
	/* } */
      for(i=0;i<2;i++)
	{
	    /*
	  if((flush_expose(t->corners[i]))||(expose_win==t->corners[i])||
	     (expose_win == None))
	    {*/
	      GC rgc,sgc;
	      
	      rgc = HReliefGC;
	      sgc = HShadowGC;

		RelieveWindow(t,t->corners[i],0,0,t->corner_width,
			      t->corner_width+t->bw,
			      rgc,sgc, corners[i]);
		 
	      if(t->boundary_height > 1)
		RelieveParts(t,i,rgc,i?rgc:sgc);
	      else
		RelieveParts(t,i,sgc,i?sgc:sgc);
	   /* } */
	}
    }
  else      /* no decorative border */
    {
      /* for mono - put a black border on 
       * for color, make it the color of the decoration background */
      if(t->boundary_height < 2)
	{
          t->boundary_height = 0;
	  flush_expose (t->frame);
      	  if(Scr.d_depth <2)
	    {
              XSetWindowBorder(dpy,t->frame,BlackPixel(dpy, DefaultScreen(dpy)));
              XSetWindowBorder(dpy,t->Parent,BlackPixel(dpy, DefaultScreen(dpy)));
	      XSetWindowBackgroundPixmap(dpy,t->frame,BackPixmap);
	      XClearWindow(dpy,t->frame);
	      XSetWindowBackgroundPixmap(dpy,t->Parent,BackPixmap);
	      XClearWindow(dpy,t->Parent);
	    }
	  else
	    {
	      XSetWindowBackground(dpy,t->frame,BorderColor);
/*	     XSetWindowBorder(dpy,t->frame,BorderColor);
*/
	      XClearWindow(dpy,t->frame);
	      XSetWindowBackground(dpy,t->Parent,BorderColor);
/*	      XSetWindowBorder(dpy,t->Parent,BorderColor);
*/
	      XClearWindow(dpy,t->Parent);
/*	      XSetWindowBorder(dpy,t->w,BorderColor);	      
*/
	    }
	}
      else
	{
	  GC rgc,sgc;

/*	  XSetWindowBorder(dpy,t->Parent,BorderColor);
	  XSetWindowBorder(dpy,t->frame,BorderColor);	  
*/

	  rgc=ReliefGC;
	  sgc=ShadowGC;
	  ChangeWindowColor(t->frame);
	    
	  if((flush_expose(t->frame))||(expose_win == t->frame)||
	     (expose_win == None))
	    {

	      if(t->boundary_height > 2)
		{
		  RelieveWindow(t,t->frame,t->boundary_width-1 - t->bw,
				t->boundary_width-1-t->bw,
				t->frame_width-
				(t->boundary_width<<1)+2+3*t->bw,
				t->frame_height-
				(t->boundary_width<<1)+2+3*t->bw,
				sgc,rgc,
				TOP_HILITE|LEFT_HILITE|RIGHT_HILITE|
				BOTTOM_HILITE);
		  RelieveWindow(t,t->frame,0,0,t->frame_width+t->bw,
				t->frame_height+t->bw,rgc,sgc,
				TOP_HILITE|LEFT_HILITE|RIGHT_HILITE|
				BOTTOM_HILITE);
		}
	      else
		{
		  RelieveWindow(t,t->frame,0,0,t->frame_width+t->bw,
				t->frame_height+t->bw,rgc,rgc,
				TOP_HILITE|LEFT_HILITE|RIGHT_HILITE|
				BOTTOM_HILITE);	      
		}
	    }
	  else
	    {
	      XSetWindowBackground(dpy,t->Parent,BorderColor);	      
	    }
	}
    }
}


/****************************************************************************
 *
 *  Redraws just the title bar
 *
 ****************************************************************************/
void SetTitleBar (ASWindow *t,Bool onoroff, Bool NewTitle)
{
  int hor_off, w;
  GC ReliefGC,ShadowGC,tGC;
  Pixel Forecolor, BackColor;

  if(!t)
    return;
  if(!(t->flags & TITLE))
    return;

  if (onoroff) 
    {
      Forecolor = Scr.HiColors.fore;
      BackColor = Scr.HiColors.back;
#ifdef ENABLE_TEXTURE	
      if (Textures.flags & TitlebarNoPush) {
	  ReliefGC = (onoroff ? Scr.HiReliefGC : Scr.StdReliefGC);
	  ShadowGC = (onoroff ? Scr.HiShadowGC : Scr.StdShadowGC);
      } else
#endif
      {
	  if (onoroff || !(Textures.flags & TexturedHandle)) {
	      ReliefGC = (PressedW==t->title_w?Scr.HiShadowGC:Scr.HiReliefGC);
	      ShadowGC = (PressedW==t->title_w?Scr.HiReliefGC:Scr.HiShadowGC);
	  } else {
	      ReliefGC = (PressedW==t->title_w?Scr.StdShadowGC:Scr.StdReliefGC);
	      ShadowGC = (PressedW==t->title_w?Scr.StdReliefGC:Scr.StdShadowGC);
	  }		  
      }	
    }
  else
    {
      Forecolor =t->TextPixel;
      BackColor = t->BackPixel;
      if(t->BackPixel != Scr.StdColors.back)
	{
	  Globalgcv.foreground = t->ReliefPixel;
	  Globalgcm = GCForeground;
	  XChangeGC(dpy,Scr.ScratchGC1,Globalgcm,&Globalgcv); 
	  ReliefGC = Scr.ScratchGC1;

	  Globalgcv.foreground = t->ShadowPixel;
	  XChangeGC(dpy,Scr.ScratchGC2,Globalgcm,&Globalgcv); 
	  ShadowGC = Scr.ScratchGC2;
	}
      else
	{
	  ReliefGC = Scr.StdReliefGC;
	  ShadowGC = Scr.StdShadowGC;
	}
      if(PressedW==t->title_w)
	{
	  tGC = ShadowGC;
	  ShadowGC = ReliefGC;
	  ReliefGC = tGC;
	}
    }
  flush_expose(t->title_w);
  
  if(t->name != (char *)NULL)
    {
      w=XTextWidth(Scr.WindowFont.font,t->name,strlen(t->name));
      if(w > t->title_width-12)
	w = t->title_width-4;
      if(w < 0)
	w = 0;
    }
  else
    w = 0;

#ifdef ENABLE_TEXTURE
  if (Scr.TitleStyle == TITLE_NEXT4) 
      hor_off = t->nr_left_buttons>0 ? t->title_height+10 : 5;
  else
#endif
  switch (Scr.TitleTextAlign) {
  case JUSTIFY_LEFT:
    hor_off = t->nr_left_buttons*t->title_height+6;
    break;
  case JUSTIFY_RIGHT:
    hor_off = t->title_width - w - t->nr_right_buttons*t->title_height-6;
    break;
  case JUSTIFY_CENTER:
  default:
    hor_off = (t->title_width - w)/2;
  }
  
  NewFontAndColor(Scr.WindowFont.font->fid,Forecolor, BackColor);
  			
      
  if(NewTitle)
    XClearWindow(dpy,t->title_w);
  
  /* for mono, we clear an area in the title bar where the window
   * title goes, so that its more legible. For color, no need */
  if(Scr.d_depth<2)
    {
      RelieveWindow(t,t->title_w,0,0,hor_off-2,t->title_height,
		    ReliefGC, ShadowGC, BOTTOM_HILITE);
      RelieveWindow(t,t->title_w,hor_off+w+2,0,
		    t->title_width - w - hor_off-2,t->title_height,
		    ReliefGC, ShadowGC, BOTTOM_HILITE);
      XFillRectangle(dpy,t->title_w,
		     (PressedW==t->title_w?ShadowGC:ReliefGC),
		     hor_off - 2, 0, w+4,t->title_height);
      
      XDrawLine(dpy,t->title_w,ShadowGC,hor_off+w+1,0,hor_off+w+1,
		t->title_height);
      if(t->name != (char *)NULL)
	XDrawString (dpy, t->title_w,Scr.FontGC,hor_off, 
		Scr.WindowFont.y + 4,
		     t->name, strlen(t->name));
    }
  else
    {
#ifdef ENABLE_TEXTURE
	int type;
	
	type=SetTitleBackground(t,onoroff);

	if (!(Textures.flags & TitlebarNoPush) || type==0 
	    || type==TEXTURE_PIXMAP) {
	    RelieveWindow(t,t->title_w,0,0,t->bp_width,t->bp_height,
			ReliefGC, ShadowGC, BOTTOM_HILITE| RIGHT_HILITE);
	    XFlush(dpy);
	}
	
	if(t->name != (char *)NULL) {
	    if (onoroff && (Textures.flags & GradientText)) {
		DrawTexturedText(dpy,t->title_w,Scr.WindowFont.font,hor_off,
				 4,Scr.TitleGradient, t->name, strlen(t->name));
	    } else {
		XDrawString (dpy, t->title_w,Scr.FontGC,hor_off,
			     Scr.WindowFont.y+ 4,
			     t->name, strlen(t->name));
	    }
	}	
#else
      if(t->name != (char *)NULL)
	  XDrawString (dpy, t->title_w,Scr.FontGC,hor_off, 
		       Scr.WindowFont.y+ 4,
		       t->name, strlen(t->name));
	RelieveWindow(t,t->title_w,0,0,t->title_width,t->title_height,
		      ReliefGC, ShadowGC, BOTTOM_HILITE| RIGHT_HILITE);
#endif
    }
  XFlush(dpy);
}




/****************************************************************************
 *
 *  Draws the relief pattern around a window
 *
 ****************************************************************************/
AFTER_INLINE void RelieveWindow(ASWindow *t,Window win,
			       int x,int y,int w,int h,
			       GC ReliefGC,GC ShadowGC, int hilite)
{
  XSegment seg[4];
  int i;
  int edge;

  if( ! win)
    return;

  edge = 0; 
  if(win == t->side)
   edge = -1;
  if(win == t->corners[0])
    edge = 1;
  if(win == t->corners[1])
    edge = 2;

  i=0;
  seg[i].x1 = x;        seg[i].y1   = y;
  seg[i].x2 = w+x-1;    seg[i++].y2 = y;

  seg[i].x1 = x;        seg[i].y1   = y;
  seg[i].x2 = x;        seg[i++].y2 = h+y-1;

  if(((t->boundary_height > 2)||(edge == 0))&&
     ((t->boundary_height > 3)||(edge < 1))&&
      (((edge==0)||(t->boundary_height > 3))&&(hilite & TOP_HILITE)))
    {
      seg[i].x1 = x+1;      seg[i].y1   = y+1;
      seg[i].x2 = x+w-2;    seg[i++].y2 = y+1;
    }

  if(((t->boundary_height > 2)||(edge == 0))&&
     ((t->boundary_height > 3)||(edge < 1))&&
      (((edge==0)||(t->boundary_height > 3))&&(hilite & LEFT_HILITE)))
    {
      seg[i].x1 = x+1;      seg[i].y1   = y+1;
      seg[i].x2 = x+1;      seg[i++].y2 = y+h-2;
    }
  XDrawSegments(dpy, win, ReliefGC, seg, i);
  i=0;
  seg[i].x1 = x;        seg[i].y1   = y+h-1;
  seg[i].x2 = w+x-1;    seg[i++].y2 = y+h-1;
    
  if(((t->boundary_height > 2)||(edge == 0))&&
      (((edge==0)||(t->boundary_height > 3))&&(hilite & BOTTOM_HILITE)))
    {
      seg[i].x1 = x+1;      seg[i].y1   = y+h-2;
      seg[i].x2 = x+w-2;    seg[i++].y2 = y+h-2;
    }

  seg[i].x1 = x+w-1;    seg[i].y1   = y;
  seg[i].x2 = x+w-1;    seg[i++].y2 = y+h-1;

  if(((t->boundary_height > 2)||(edge == 0))&&
      (((edge==0)||(t->boundary_height > 3))&&(hilite & RIGHT_HILITE)))
    {
      seg[i].x1 = x+w-2;    seg[i].y1   = y+1;
      seg[i].x2 = x+w-2;    seg[i++].y2 = y+h-2;
    }
  XDrawSegments(dpy, win, ShadowGC, seg, i);

}

void RelieveParts(ASWindow *t,int i,GC hor, GC vert)
{
  XSegment seg[2];

      if( !t->corners[i])
	   return;
      switch(i)
	{
	case 0:
	  seg[0].x1 = t->boundary_width-1;
	  seg[0].x2 = t->corner_width-2;
	  seg[0].y1 = t->corner_width - t->boundary_height+t->bw;
	  seg[0].y2 = t->corner_width - t->boundary_height+t->bw;
	  break;
	case 1:
	  seg[0].x1 = 0;
	  seg[0].x2 = t->corner_width - t->boundary_width;
	  seg[0].y1 = t->corner_width - t->boundary_height+t->bw;
	  seg[0].y2 = t->corner_width - t->boundary_height+t->bw;
	  break;
	}
      XDrawSegments(dpy, t->corners[i], hor, seg, 1);
      switch(i)
	{
	case 0:
	  seg[0].y1 = 0;
	  seg[0].y2 = t->corner_width - t->boundary_height;
	  seg[0].x1 = t->boundary_width-1;
	  seg[0].x2 = t->boundary_width-1;
	  break;
	case 1:
	  seg[0].y1 = 0;
	  seg[0].y2 = t->corner_width - t->boundary_height + t->bw;
	  seg[0].x1 = t->corner_width - t->boundary_width;
	  seg[0].x2 = t->corner_width - t->boundary_width;
	  break;
	}
      XDrawSegments(dpy, t->corners[i], vert, seg, 1);
}


Pixmap MakeGradient(int type, int From[3], int To[3], 
		    int w, int h, int maxcols, Pixmap cache)
{
    Pixmap pixmap;

    pixmap=XCreatePixmap(dpy, Scr.Root, w-1, h-1, Scr.d_depth);
    
    switch (type) {	 
     case 1:/* NW to SE gradient */
	if (!DrawDegradeRelief(dpy, pixmap, 0, 0, w-1, h-1,From, To, 1, 
			       maxcols)) {
	    XFreePixmap(dpy, pixmap);
	    pixmap = None;
	}
	break;
     case 2: /* horizontal gradient */
     case 3: /* horizn. cilyndrical */
	/* use cached thing */
	XCopyArea(dpy, cache, pixmap, Scr.ScratchGC1,
		  0, 0, w-1, h-1, 0,0);
	XCopyArea(dpy, cache, pixmap, Scr.ScratchGC1,
		  Scr.MyDisplayWidth-2, 0, 1, h-1,w-2,0);
	break;
     case 4: /* vertical gradient */
     case 5: /* vert. cilyndrical */
	if (!DrawVGradient(dpy, pixmap, 0, 0, 
			   w-1, h-1, From,To, 1, maxcols, type-4)) {
	    XFreePixmap(dpy, pixmap);
	    pixmap = None;
	}
	break;
     default:
	XFreePixmap(dpy, pixmap);
	pixmap = None;
    }
    return pixmap;
}


/***********************************************************************
 *
 *  Procedure:
 *      Setupframe - set window sizes, this was called from either
 *              AddWindow, EndResize, or HandleConfigureNotify.
 *
 *  Inputs:
 *      tmp_win - the ASWindow pointer
 *      x       - the x coordinate of the upper-left outer corner of the frame
 *      y       - the y coordinate of the upper-left outer corner of the frame
 *      w       - the width of the frame window w/o border
 *      h       - the height of the frame window w/o border
 *
 *  Special Considerations:
 *      This routine will check to make sure the window is not completely
 *      off the display, if it is, it'll bring some of it back on.
 *
 *      The tmp_win->frame_XXX variables should NOT be updated with the
 *      values of x,y,w,h prior to calling this routine, since the new
 *      values are compared against the old to see whether a synthetic
 *      ConfigureNotify event should be sent.  (It should be sent if the
 *      window was moved but not resized.)
 *
 ************************************************************************/

void SetupFrame(ASWindow *tmp_win,int x,int y,int w,int h,Bool sendEvent)
{
  XEvent client_event;
  XWindowChanges frame_wc, xwc;
  unsigned long frame_mask, xwcm;
  int cx,cy,i;
  Bool Resized = False;
#ifndef NO_PAGER
  ASWindow *t;
#endif
  int xwidth,ywidth,left,right;

  if (tmp_win->flags & SHADED) return;  
  /* if windows is not being maximized, save size in case of maximization */
  if (!(tmp_win->flags & MAXIMIZED))
    {
      tmp_win->orig_x = x;
      tmp_win->orig_y = y;
      tmp_win->orig_wd = w;
      tmp_win->orig_ht = h;
    }
  if(Scr.flags & DontMoveOff)
    {
      if (x + Scr.Vx + w < 16)
	x = 16 - Scr.Vx - w;
      if (y + Scr.Vy + h < 16)
	y = 16 - Scr.Vy - h;
    }
  if (x >= Scr.MyDisplayWidth + Scr.VxMax - Scr.Vx-16)
    x = Scr.MyDisplayWidth + Scr.VxMax -Scr.Vx - 16;
  if (y >= Scr.MyDisplayHeight+Scr.VyMax - Scr.Vy -16)
    y = Scr.MyDisplayHeight + Scr.VyMax - Scr.Vy - 16;

  /*
   * According to the July 27, 1988 ICCCM draft, we should send a
   * "synthetic" ConfigureNotify event to the client if the window
   * was moved but not resized.
   */
  if ((x != tmp_win->frame_x || y != tmp_win->frame_y) &&
      (w == tmp_win->frame_width && h == tmp_win->frame_height))
    sendEvent = TRUE;

  if((w != tmp_win->frame_width) || (h != tmp_win->frame_height))
    Resized = True;

  if(Resized)
    {
      left = tmp_win->nr_left_buttons;
      right = tmp_win->nr_right_buttons;

      tmp_win->title_width= w- 2*tmp_win->boundary_width+tmp_win->bw ;


      if(tmp_win->title_width < 1) 
	tmp_win->title_width = 1;

      if (tmp_win->flags & TITLE) 
	{
	  xwcm = CWWidth | CWX | CWY;
	  tmp_win->title_x =  tmp_win->boundary_width;
	  if(tmp_win->title_x >=  w - tmp_win->boundary_width)
	    tmp_win->title_x = -10;
          tmp_win->title_y = 0;
	  xwc.width = tmp_win->title_width;
	  xwc.x = tmp_win->title_x;
	  xwc.y = tmp_win->title_y;
	  XConfigureWindow(dpy, tmp_win->title_w, xwcm, &xwc);


	  xwcm = CWX | CWY;
	  xwc.y = 3;
	  
	  xwc.x = 3;
	  for(i=0;i<Scr.nr_left_buttons;i++)
	    {
	      if(tmp_win->left_w[i] != None)
		{
		  if(xwc.x + tmp_win->title_height < w - tmp_win->boundary_width)
		    XConfigureWindow(dpy, tmp_win->left_w[i], xwcm, &xwc);
		  else
		    {
		      xwc.x = -tmp_win->title_height;
		      XConfigureWindow(dpy, tmp_win->left_w[i], xwcm, &xwc);
		    }
		  xwc.x += tmp_win->title_height;
		}
	    }
	  
	  xwc.x=w-tmp_win->boundary_width-
		(tmp_win->button_height-tmp_win->title_height)/2 +tmp_win->bw;
	  for(i=0;i<Scr.nr_right_buttons;i++)
	    {
	      if(tmp_win->right_w[i] != None)
		{
		  xwc.x -=tmp_win->title_height;
		  if(xwc.x > tmp_win->boundary_width)
		    XConfigureWindow(dpy, tmp_win->right_w[i], xwcm, &xwc);
		  else
		    {
		      xwc.x = -tmp_win->title_height;
		      XConfigureWindow(dpy, tmp_win->right_w[i], xwcm, &xwc);
		    }
		}
	    }
	}

      if((tmp_win->flags & BORDER) 
	 && (((tmp_win->wmhints)&&(!(tmp_win->wmhints->initial_state == ZoomState)))
	 || (!tmp_win->wmhints && !(tmp_win->flags & MAXIMIZED))))
	{
	  xwidth = w - 2*tmp_win->corner_width+tmp_win->bw;
	  ywidth = h - 2*tmp_win->corner_width;
	  xwcm = CWWidth | CWHeight | CWX | CWY;
	  if(xwidth<2)
	    xwidth = 2;
	  if(ywidth<2)
	    ywidth = 2;

	  xwc.x = tmp_win->corner_width;
	  xwc.y = h - tmp_win->boundary_height+tmp_win->bw;
	  xwc.height = tmp_win->boundary_height+tmp_win->bw;
	  xwc.width = xwidth;
	  XConfigureWindow(dpy, tmp_win->side, xwcm, &xwc);

	  xwcm = CWX|CWY;
	  for(i=0;i<2;i++)
	    {
	      if(i)
		xwc.x = w - tmp_win->corner_width+tmp_win->bw;
	      else
		xwc.x = 0;
	      
	      xwc.y = h - tmp_win->corner_width;
	      XConfigureWindow(dpy, tmp_win->corners[i], xwcm, &xwc);
	    }
	}
    }
  tmp_win->attr.width = w - 2*tmp_win->boundary_width;
  tmp_win->attr.height = h - tmp_win->title_height - tmp_win->boundary_height;
  /* may need to omit the -1 for shaped windows, next two lines*/
  cx = tmp_win->boundary_width-tmp_win->bw;
  cy = tmp_win->title_height -tmp_win->bw;

/*
 change 09/25/96
  if (tmp_win->attr.height<0) tmp_win->attr.height=1;
*/
  if (tmp_win->attr.height<=0) tmp_win->attr.height=tmp_win->hints.height_inc;

  XResizeWindow(dpy, tmp_win->w, tmp_win->attr.width,
		tmp_win->attr.height);
  XMoveResizeWindow(dpy, tmp_win->Parent, cx,cy,
		    tmp_win->attr.width, tmp_win->attr.height);

  /* 
   * fix up frame and assign size/location values in tmp_win
   */
  frame_wc.x = tmp_win->frame_x = x;
  frame_wc.y = tmp_win->frame_y = y;
  frame_wc.width = tmp_win->frame_width = w;
  frame_wc.height = tmp_win->frame_height = h;
  frame_mask = (CWX | CWY | CWWidth | CWHeight);
  XConfigureWindow (dpy, tmp_win->frame, frame_mask, &frame_wc);

#ifdef ENABLE_TEXTURE
    /* 
     * rebuild titlebar background pixmap 
     */    
    tmp_win->bp_height = tmp_win->title_height;				    
    /* Titlebar */
    if (tmp_win->flags&TITLE) {
	if ((Textures.Ttype>0) && (Textures.Ttype<TEXTURE_PIXMAP)
	    && (tmp_win->title_width > 2) && ((tmp_win->backPixmap==None) 
	     || (tmp_win->bp_width!=tmp_win->title_width))) {
	    
	    if (tmp_win->backPixmap!=None) {
		XFreePixmap(dpy, tmp_win->backPixmap);
	    }	
	    tmp_win->backPixmap = MakeGradient(Textures.Ttype, 
					       Textures.Tfrom, Textures.Tto,
					       tmp_win->title_width,
					       tmp_win->title_height,
					       Textures.Tmaxcols,
					       Scr.ForeTitle);
	    if (tmp_win->backPixmap==None) {
		afterstep_err("Invalid TextureType %i specified for titlebar\n",
			      (char *)Textures.Ttype,NULL,NULL);
		Textures.Ttype = 0;
	    }
	} else if (Textures.Ttype==0 || Textures.Ttype==TEXTURE_PIXMAP) {
	    tmp_win->backPixmap=None;
	}    
    } else
      tmp_win->backPixmap=None;
    
    /* Unfocused titlebar */
    if (tmp_win->flags&TITLE) {
	if ((Textures.Utype>0) && (Textures.Utype<TEXTURE_PIXMAP)
	    && (tmp_win->title_width > 2) && ((tmp_win->backPixmap2==None) 
	     || (tmp_win->bp_width!=tmp_win->title_width))) {
	    
	    if (tmp_win->backPixmap2!=None) {
		XFreePixmap(dpy, tmp_win->backPixmap2);
	    }	
	    tmp_win->backPixmap2 = MakeGradient(Textures.Utype, 
					       Textures.Ufrom, Textures.Uto,
					       tmp_win->title_width,
					       tmp_win->title_height,
					       Textures.Umaxcols,
					       Scr.BackTitle);
	    if (tmp_win->backPixmap2==None) {
		afterstep_err("Invalid UTextureType %i specified for titlebar\n",
			      (char *)Textures.Utype,NULL,NULL);
		Textures.Utype = 0;
	    }
	} else if (Textures.Utype==0 || Textures.Utype==TEXTURE_PIXMAP) {
	    tmp_win->backPixmap2=None;
	}
    } else
      tmp_win->backPixmap2=None;
    
    /* Unfocused sticky titlebar */
    if (tmp_win->flags&TITLE) {
	if ((Textures.Stype>0) && (Textures.Stype<TEXTURE_PIXMAP)
	    && (tmp_win->title_width > 2) && ((tmp_win->backPixmap3==None) 
	     || (tmp_win->bp_width!=tmp_win->title_width))) {
	    
	    if (tmp_win->backPixmap3!=None) {
		XFreePixmap(dpy, tmp_win->backPixmap3);
	    }	
	    tmp_win->backPixmap3 = MakeGradient(Textures.Stype, 
					       Textures.Sfrom, Textures.Sto,
					       tmp_win->title_width,
					       tmp_win->title_height,
					       Textures.Smaxcols,
					       Scr.StickyTitle);
	    if (tmp_win->backPixmap3==None) {
		afterstep_err("Invalid STextureType %i specified for titlebar\n",
			      (char *)Textures.Stype,NULL,NULL);
		Textures.Stype = 0;
	    }
	} else if (Textures.Stype==0 || Textures.Stype==TEXTURE_PIXMAP) {
	    tmp_win->backPixmap3=None;
	}
    } else
      tmp_win->backPixmap3=None;
    tmp_win->bp_width= tmp_win->title_width;

#endif /* ENABLE_TEXTURE */

#ifdef SHAPE
  if ((Resized)&&(tmp_win->wShaped))
    {
      SetShape(tmp_win,w);
    }
#endif /* SHAPE */
  XSync(dpy,0);
  if (sendEvent)
    {
      client_event.type = ConfigureNotify;
      client_event.xconfigure.display = dpy;
      client_event.xconfigure.event = tmp_win->w;
      client_event.xconfigure.window = tmp_win->w;

      client_event.xconfigure.x = x + tmp_win->boundary_width;
      client_event.xconfigure.width = w-2*tmp_win->boundary_width;
      client_event.xconfigure.y = y + tmp_win->title_height;
      client_event.xconfigure.height =h-tmp_win->boundary_height -
           tmp_win->title_height;
/*
  change 09/25/96
      if (client_event.xconfigure.height<0) 
	  client_event.xconfigure.height=1;
*/

      if (client_event.xconfigure.height<=0) 
        client_event.xconfigure.height=tmp_win->hints.height_inc;


      client_event.xconfigure.border_width =tmp_win->bw;
      /* Real ConfigureNotify events say we're above title window, so ... */
      /* what if we don't have a title ????? */
      client_event.xconfigure.above = tmp_win->frame;
      client_event.xconfigure.override_redirect = False;
      XSendEvent(dpy, tmp_win->w, False, StructureNotifyMask, &client_event);
    }
#ifndef NO_PAGER
  if(tmp_win == Scr.ASPager)
    {
      MoveResizeViewPortIndicator();
      for (t = Scr.ASRoot.next; t != NULL; t = t->next)
	{
	  MoveResizePagerView(t);
	}
    }
  else
    MoveResizePagerView(tmp_win);
#endif
  BroadcastConfig(M_CONFIGURE_WINDOW,tmp_win);
}


/****************************************************************************
 *
 * Sets up the shaped window borders 
 * 
 ****************************************************************************/
void SetShape(ASWindow *tmp_win, int w)
{
#ifdef SHAPE
  XRectangle rect;

  XShapeCombineShape (dpy, tmp_win->frame, ShapeBounding,
		      tmp_win->boundary_width,
		      tmp_win->title_height+tmp_win->boundary_width,
		      tmp_win->w,
		      ShapeBounding, ShapeSet);
  if (tmp_win->title_w) 
    {
      /* windows w/ titles */
      rect.x = tmp_win->boundary_width;
      rect.y = tmp_win->title_y;
      rect.width = w - 2*tmp_win->boundary_width+tmp_win->bw;
      rect.height = tmp_win->title_height;
      
      
      XShapeCombineRectangles(dpy,tmp_win->frame,ShapeBounding,
			      0,0,&rect,1,ShapeUnion,Unsorted);
    }
#endif
}

/********************************************************************
 *
 * Sets the input focus to the indicated window.
 *
 **********************************************************************/

Bool SetFocus(Window w, ASWindow *Fw, Bool circulated)
{
  int i;
  extern Time lastTimestamp;
  Bool focusAccepted = True;

  if (!circulated && Fw)
      SetCirculateSequence(Fw, 1);

  /* ClickToFocus focus queue manipulation */
  if (Fw && Fw != Scr.Focus && Fw != &Scr.ASRoot)
      Fw->focus_sequence = Scr.next_focus_sequence++;

  if(Scr.NumberOfScreens > 1)
    {
      XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		    &JunkX, &JunkY, &JunkX, &JunkY, &JunkMask);
      if(JunkRoot != Scr.Root)
	{
	  if((Scr.flags & ClickToFocus) && (Scr.Ungrabbed != NULL))
	    {
	      /* Need to grab buttons for focus window */
	      XSync(dpy,0);
	      for(i = 0; i < MAX_BUTTONS; i++)
		if(Scr.buttons2grab & (1<<i))
		    MyXGrabButton(dpy, i+1, 0, Scr.Ungrabbed->frame,
				  True, ButtonPressMask, GrabModeSync,
				  GrabModeAsync, None, Scr.ASCursors[SYS]);
	      Scr.Focus = NULL;
	      Scr.Ungrabbed = NULL;
	      XSetInputFocus(dpy, Scr.NoFocusWin,RevertToParent,lastTimestamp);
	    }
	  return False;
	}
    }

  if (Fw && Fw->focus_var)
      return False;

  if(Fw && (Fw->Desk != Scr.CurrentDesk))
    {
      Fw = NULL;
      w = Scr.NoFocusWin;
      focusAccepted = False;
    }

  if((Scr.flags & ClickToFocus) && (Scr.Ungrabbed != Fw))
    {
      /* need to grab all buttons for window that we are about to
       * unfocus */
      if(Scr.Ungrabbed != NULL)
	{
	  XSync(dpy,0);
	  for(i = 0; i < MAX_BUTTONS; i++)
	    if(Scr.buttons2grab & (1<<i))
	      {
		MyXGrabButton(dpy, i+1, 0, Scr.Ungrabbed->frame,
			      True, ButtonPressMask, GrabModeSync,
			      GrabModeAsync, None, Scr.ASCursors[SYS]);
	      }
	  Scr.Ungrabbed = NULL;
	}
      /* if we do click to focus, remove the grab on mouse events that
       * was made to detect the focus change */
      if((Scr.flags & ClickToFocus)&&(Fw != NULL))
	{
	  for(i = 0; i < MAX_BUTTONS; i++)
	    if(Scr.buttons2grab & (1<<i))
	      {
		MyXUngrabButton(dpy, i+1, 0, Fw->frame);
	      }
	  Scr.Ungrabbed = Fw;
	}
    }

    /* do not set focus to shaded windows */
    if (Fw && (Fw->flags& SHADED))
      return False;
    
  if(!((Fw)&&(Fw->wmhints)&&(Fw->wmhints->flags & InputHint)&&
	    (Fw->wmhints->input == False)))
    {
      /* Window will accept input focus */
      XSetInputFocus (dpy, w, RevertToParent, lastTimestamp);
      Scr.Focus = Fw;
    }
  else if ((Scr.Focus)&&(Scr.Focus->Desk == Scr.CurrentDesk))
    {
      /* Window doesn't want focus. Leave focus alone */
      /* XSetInputFocus (dpy,Scr.Hilite->w , RevertToParent, lastTimestamp);*/
      focusAccepted = False;
    }
  else
    {
      XSetInputFocus (dpy, Scr.NoFocusWin, RevertToParent, lastTimestamp);
      Scr.Focus = NULL;
      focusAccepted = False;
    }
  if ((Fw)&&(Fw->flags & DoesWmTakeFocus))
    send_clientmessage (w,_XA_WM_TAKE_FOCUS, lastTimestamp);

  XSync(dpy,0);

  return focusAccepted;
}
