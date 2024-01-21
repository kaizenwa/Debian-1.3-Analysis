/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 *
 * later modified for BowMan
 * by Bo Yang
 * 
 * modified again for AfterStep
 * by Frank Fejes
 * 
 * yet more modifications 
 * by Alfredo Kojima
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
 * afterstep menu code
 *
 ***********************************************************************/
#include "../configure.h"

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include <X11/keysym.h>
#include <sys/types.h>
#include <sys/time.h>

#include "afterstep.h"
#include "menus.h"
#include "misc.h"
#include "parse.h"
#include "screen.h"
#ifdef ENABLE_TEXTURE
#include "stepgfx.h"
#endif

short menu_on=0;
Bool IgnoreRelease = 1;
int BottomOfPage=0;
enum {MWAIT, MCLICK, MHOLD} MenuMode;
Time MenuModeCommitTime;

MenuRoot *ActiveMenu = NULL;		/* the active menu */
MenuItem *ActiveItem = NULL;		/* the active menu item */

int menuFromFrameOrWindowOrTitlebar = FALSE;

extern int Context,Button;
extern ASWindow *ButtonWindow, *Tmp_win;
extern XEvent Event;
int Stashed_X, Stashed_Y,MenuY=0;

void DrawTrianglePattern(Window,GC,GC,GC,int,int,int,int);
void DrawSeparator(Window, GC,GC,int, int,int,int,int);
void DrawUnderline(Window w, GC gc, int x, int y, char *txt, int off);
int UpdateMenu(void); 

extern XContext MenuContext;
/****************************************************************************
 *
 * Initiates a menu pop-up
 *
 ***************************************************************************/
int do_menu (MenuRoot *menu)
{
  int prevStashedX=0,prevStashedY=0;
  MenuRoot *PrevActiveMenu=0;
  MenuItem *PrevActiveItem=0;
  int retval=MENU_NOP;
  int x,y,offset;

  /* this condition could get ugly */
  if(menu->in_use)
    return MENU_ERROR;

    
  /* In case we wind up with a move from a menu which is
   * from a window border, we'll return to here to start
   * the move */
  XQueryPointer( dpy, Scr.Root, &JunkRoot, &JunkChild,
		&x, &y, &JunkX, &JunkY, &JunkMask);    

  if(menu_on)
    {
      prevStashedX = Stashed_X;
      prevStashedY = Stashed_Y;

      PrevActiveMenu = ActiveMenu;
      PrevActiveItem = ActiveItem;
      if(ActiveMenu) {
          /* Offset the new menu depending on the width (odd/even) 
             of its parent menu */
          offset = ((ActiveMenu->width % 2) ? 1 : 0);
	  x = (Stashed_X < (ActiveMenu->width >> 1)? ActiveMenu->width-1 :
		Stashed_X + (ActiveMenu->width >> 1)) + (menu->width >> 1)
                + offset;
	}
      if(ActiveItem) {
	  if (!(Scr.flags & MenusHigh))
            y = MenuY - 10 + ((ActiveItem->item_num)*(ActiveItem->y_height));
	  else
            y = MenuY + (Scr.EntryHeight >>1);

	}
    }
  else
    {
      if(!GrabEm(MENU))
	{
	  XBell(dpy,Scr.screen);
	  return MENU_DONE;
	}
      MenuMode = MWAIT;
      MenuModeCommitTime = lastTimestamp + Scr.ClickTime;
    }
#ifdef ENABLE_TEXTURE
    if (Scr.d_depth >= 8) {
	if ((menu->titlebg==None) && (Textures.Mtype>0)) {
	    menu->titlebg =  XCreatePixmap(dpy, menu->w, menu->width-2,
					   menu->first->y_height-2,
					   Scr.d_depth);
	    switch (Textures.Mtype) {
	     case 1:/* NW to SE gradient */
		if (!DrawDegradeRelief(dpy, menu->titlebg, 0, 0, menu->width-2,
				   menu->first->y_height-2,
				   Textures.Mfrom, Textures.Mto, 1,
				   Textures.Mmaxcols)) {
		    XFreePixmap(dpy, menu->titlebg);
		    menu->titlebg = None;
		}
    		break;
	     case 2: /* horizontal gradient */
	     case 3: /* horiz. cylindrical */	
		if (!DrawHGradient(dpy, menu->titlebg, 0, 0, menu->width-2,
				   menu->first->y_height-2,
				   Textures.Mfrom, Textures.Mto, 1,
				   Textures.Mmaxcols,Textures.Mtype-2)) {
		    XFreePixmap(dpy, menu->titlebg);
		    menu->titlebg = None;
		}
		break;
	     case 4: /* vertical gradient */
	     case 5: /* vert. cylindrical */	
		if (!DrawVGradient(dpy, menu->titlebg, 0, 0, menu->width-2,
				   menu->first->y_height-2,
				   Textures.Mfrom, Textures.Mto, 1,
				   Textures.Mmaxcols,Textures.Mtype-4)) {
		    XFreePixmap(dpy, menu->titlebg);
		    menu->titlebg = None;
		}
		break;
	     default:
		XFreePixmap(dpy, menu->titlebg);
		menu->titlebg = None;
		afterstep_err("Invalid TextureType %i specified for menu title\n",
			      (char *)Textures.Mtype, NULL,NULL);
		Textures.Mtype=0;
	    }
	}
	if ((menu->itembg==None) && (Textures.Itype>0) && (menu->last!=NULL)) {
	    menu->itembg =  XCreatePixmap(dpy, menu->w, menu->width-2,
					  menu->last->y_height-3,
					  Scr.d_depth);
	    switch (Textures.Itype) {
	     case 1:/* NW to SE gradient */
		if (!DrawDegradeRelief(dpy, menu->itembg, 0, 0, 
				       menu->width-2,menu->last->y_height-3,
				       Textures.Ifrom, Textures.Ito, 0,
				       Textures.Imaxcols)) {
		    XFreePixmap(dpy, menu->itembg);
		    menu->itembg = None;
		}
		break;
	     case 2: /* horizontal gradient */
	     case 3: /* horiz. cylindrical */
		if (!DrawHGradient(dpy, menu->itembg, 0, 0, menu->width-2,
				   menu->last->y_height-3, 
				   Textures.Ifrom, Textures.Ito, 0,
				   Textures.Imaxcols,Textures.Itype-2)) {
		    XFreePixmap(dpy, menu->itembg);
		    menu->itembg = None;
		}
		break;
	     case 4: /* vertical gradient */
	     case 5: /* vert. cylindrical */
		if (!DrawVGradient(dpy, menu->itembg, 0, 0, menu->width-2,
				   menu->last->y_height-3, 
				   Textures.Ifrom, Textures.Ito, 0,
				   Textures.Imaxcols,Textures.Itype-4)) {
		    XFreePixmap(dpy, menu->itembg);
		    menu->itembg = None;
		}
		break;		
	     default:
		XFreePixmap(dpy, menu->itembg);
		menu->itembg = None;		
		afterstep_err("Invalid TextureType %i specified for menu item\n",
			      (char *)Textures.Itype,NULL,NULL);
		Textures.Itype = 0;
	    }
	}
    }
#endif /* ENABLE_TEXTURE */
  if (PopUpMenu (menu, x, y))
    {
      retval = UpdateMenu();
    }
  else
    XBell (dpy, Scr.screen);

  ActiveMenu = PrevActiveMenu;
  ActiveItem = PrevActiveItem;
  if((ActiveItem)&&(menu_on))
    ActiveItem->state = 1;
  Stashed_X = prevStashedX;
  Stashed_Y = prevStashedY;


  if(!menu_on)
    {
      UngrabEm();
      WaitForButtonsUp();
    }
  return retval;
}

/***********************************************************************
 *
 *  Procedure:
 *	RelieveRectangle - add relief lines to a rectangular window
 *
 ***********************************************************************/
void RelieveRectangle(Window win,int x,int y,int w, int h,GC Hilite,GC Shadow)
{
  XDrawLine(dpy, win, Hilite, x, y, w+x-1, y);
  XDrawLine(dpy, win, Hilite, x, y, x, h+y-1);

  XDrawLine(dpy, win, Shadow, x, h+y-1, w+x-1, h+y-1);
  XDrawLine(dpy, win, Shadow, w+x-1, y, w+x-1, h+y-1);
}

/***********************************************************************
 *
 *  Procedure:
 *	RelieveHalfRectangle - add relief lines to the sides only of a
 *      rectangular window
 *
 ***********************************************************************/
void RelieveHalfRectangle(Window win,int x,int y,int w,int h,
			  GC Hilite,GC Shadow)
{
  XDrawLine(dpy, win, Hilite, x, y-1, x, h+y);
  XDrawLine(dpy, win, Hilite, x+1, y, x+1, h+y-1);

  XDrawLine(dpy, win, Shadow, w+x-1, y-1, w+x-1, h+y);
  XDrawLine(dpy, win, Shadow, w+x-2, y, w+x-2, h+y-1);
}


/***********************************************************************
 *
 *  Procedure:
 *      PaintEntry - draws a single entry in a poped up menu
 *
 ***********************************************************************/
void PaintEntry(MenuRoot *mr, MenuItem *mi)
{
  int y_offset,text_y,d, y_height;
  GC ShadowGC, ReliefGC, currentGC;
  char hk[2];

  hk[1] = 0;
  y_offset = mi->y_offset;
  y_height = mi->y_height;
  text_y = y_offset + Scr.StdFont.y;

  ShadowGC = Scr.MenuShadowGC;
  if(Scr.d_depth<2)
    ReliefGC = Scr.MenuShadowGC;
  else
    ReliefGC = Scr.MenuReliefGC;

/* stuff */
  if((mi->state)&&(mi->func != F_TITLE)&&(mi->func != F_NOP)&&*mi->item) {
      Globalgcv.foreground = Scr.HiColors.fore;
      XChangeGC(dpy,Scr.ScratchGC1,Globalgcm,&Globalgcv);
      XFillRectangle(dpy, mr->w, Scr.ScratchGC1, 2, y_offset,
		     mr->width-2, y_height-2);
  } else if( mi->func == F_TITLE) {
#ifdef ENABLE_TEXTURE      
      	  if (mr->titlebg != None) {
	     Globalgcv.foreground = Scr.HiColors.back;
	     XChangeGC(dpy,Scr.ScratchGC1,GCForeground,&Globalgcv);
	     XCopyArea(dpy, mr->titlebg, mr->w, DefaultGC(dpy,Scr.screen),
		       0, y_offset, mr->width-1, mr->first->y_height-2, 1,
		       y_offset+1);
	     XDrawRectangle(dpy, mr->w, Scr.ScratchGC1, 0, y_offset,
		mr->width-1, y_height-2);
	      text_y += 4;
          }
          else
#endif	
              {
	      Globalgcv.foreground = Scr.HiColors.back;
	      XChangeGC(dpy,Scr.ScratchGC1,Globalgcm,&Globalgcv);
	      XFillRectangle(dpy, mr->w, Scr.ScratchGC1, 0, y_offset, mr->width-1,
		   y_height-2);
	      text_y += 4;
	      }
          }
   else {
#ifdef ENABLE_TEXTURE
       if (mr->itembg!=None) {
	   XCopyArea(dpy, mr->itembg, mr->w, DefaultGC(dpy,Scr.screen), 0, 0, 
		     mr->width-2, mr->last->y_height-3, 1, y_offset+1);
       } else
#endif       
         XClearArea(dpy, mr->w, 0,y_offset,mr->width,y_height,0);
   }
#ifdef ENABLE_TEXTURE
    if (mr->titlebg == None || mi->func != F_TITLE) {
	RelieveHalfRectangle(mr->w, 0, y_offset, mr->width,
			     y_height, ReliefGC, ShadowGC); 
	RelieveRectangle(mr->w, 0, y_offset, mr->width, y_height-1, 
			 ReliefGC, ShadowGC);
/* hack 
	XDrawLine( dpy, mr->w, Scr.LineGC, 0, 22, 0, 1280);
*/
/* end hack */
	if(mi->func != F_TITLE)
	  text_y += HEIGHT_EXTRA>>1;
	
	XDrawLine( dpy, mr->w, Scr.LineGC, 0, y_height+y_offset-1,
		  mr->width, y_height+y_offset-1);
    }
#else
   RelieveHalfRectangle(mr->w, 0, y_offset, mr->width,
			   y_height, ReliefGC, ShadowGC); 
   RelieveRectangle(mr->w, 0, y_offset, mr->width, y_height-1, 
                       ReliefGC, ShadowGC);
/* kludge 
   XDrawLine( dpy, mr->w, Scr.LineGC, 0, 0, 0, 1280);
*/
  if(mi->func != F_TITLE)
    text_y += HEIGHT_EXTRA>>1;

  XDrawLine( dpy, mr->w, Scr.LineGC, 0, y_height+y_offset-1,
		mr->width, y_height+y_offset-1);
/*
  XDrawLine( dpy, mr->w, Scr.LineGC, 0, y_offset,
		0, y_offset+y_height-1);
*/
#endif /* ENABLE_TEXTURE */
  if( mi->func == F_TITLE) {
    Globalgcv.foreground = Scr.HiColors.fore;
    XChangeGC(dpy,Scr.ScratchGC1,Globalgcm,&Globalgcv);
    currentGC = Scr.ScratchGC1;
    }
  else if(check_allowed_function(mi))
    currentGC = Scr.MenuGC;
  else
    /* should be a shaded out word, no just re-colored. */
    currentGC = Scr.MenuStippleGC;
  if (Scr.d_depth<2 && mi->state) {
      Globalgcv.foreground = WhitePixel(dpy,Scr.screen);  /* ??? */
      XChangeGC(dpy,Scr.ScratchGC1,Globalgcm,&Globalgcv);
      currentGC = Scr.ScratchGC1;      
  }
  if(*mi->item)
    XDrawString(dpy, mr->w, currentGC,mi->x,text_y, mi->item, mi->strlen);
  if(mi->strlen2>0)
    XDrawString(dpy, mr->w, currentGC,mi->x2,text_y, mi->item2,mi->strlen2);

  d=(Scr.EntryHeight-7)/2;
  if(mi->func != F_POPUP && mi->hotkey != 0) {
	hk[0]= mi->hotkey;
	XDrawString(dpy, mr->w, currentGC,
		mr->width-d-4- XTextWidth(Scr.StdFont.font, hk, 1)/2,
		text_y, hk, 1);
  }
  d=(Scr.EntryHeight-7)/2;
  if(mi->func == F_POPUP)
      DrawTrianglePattern(mr->w, ShadowGC, ReliefGC, ShadowGC,mr->width-d-8,
			  y_offset+d-1, mr->width-d-1, y_offset+d+7);
/* major hackage going on here */
  if (y_offset > 30)
    XDrawLine( dpy, mr->w, Scr.LineGC, 0, y_offset-1, 1600, y_offset-1);
  else
    XDrawLine( dpy, mr->w, Scr.MenuStippleGC, 0, y_offset-1, 1600, y_offset-1);	
  return;
}

/****************************************************************************
 * Procedure:
 *	DrawUnderline() - Underline a character in a string (pete@tecc.co.uk)
 *
 * Calculate the pixel offsets to the start of the character position we
 * want to underline and to the next character in the string.  Shrink by
 * one pixel from each end and the draw a line that long two pixels below
 * the character...
 *
 ****************************************************************************/
void  DrawUnderline(Window w, GC gc, int x, int y, char *txt, int posn) 
{
  int off1 = XTextWidth(Scr.StdFont.font, txt, posn);
  int off2 = XTextWidth(Scr.StdFont.font, txt, posn + 1) - 1;
  
  XDrawLine(dpy, w, gc, x + off1, y + 2, x + off2, y + 2);
}
/****************************************************************************
 *
 *  Draws two horizontal lines to form a separator
 *
 ****************************************************************************/
void DrawSeparator(Window w, GC TopGC, GC BottomGC,int x1,int y1,int x2,int y2,
		   int extra_off)
{
  XDrawLine(dpy, w, TopGC   , x1,           y1,  x2,          y2);
  XDrawLine(dpy, w, BottomGC, x1-extra_off, y1+1,x2+extra_off,y2+1);
}
    
/****************************************************************************
 *
 *  Draws a little Triangle pattern within a window
 *
 ****************************************************************************/
void DrawTrianglePattern(Window w,GC GC1,GC GC2,GC GC3,int l,int u,int r,int b)
{
  int m;

  m = (u + b)/2;

  XDrawLine(dpy,w,GC1,l,u,l,b);

  XDrawLine(dpy,w,GC2,l,b,r,m);
  XDrawLine(dpy,w,GC3,r,m,l,u);
}

/***********************************************************************
 *
 *  Procedure:
 *	PaintMenu - draws the entire menu
 *
 ***********************************************************************/
void PaintMenu(MenuRoot *mr, XEvent *e)
{
  MenuItem *mi;
  
  for (mi = mr->first; mi != NULL; mi = mi->next)
    {
      /* be smart about handling the expose, redraw only the entries
       * that we need to
       */
      if (e->xexpose.y < (mi->y_offset + mi->y_height) &&
	  (e->xexpose.y + e->xexpose.height) > mi->y_offset)
	{
	  PaintEntry(mr, mi);
	}
    }
  XSync(dpy, 0);
  return;
}

MenuRoot *PrevMenu = NULL;
MenuItem *PrevItem = NULL;
int PrevY=0;


/***********************************************************************
 *
 *  Procedure:
 *	Updates menu display to reflect the highlighted item
 *
 ***********************************************************************/
int FindEntry(void)
{
  MenuItem *mi;
  MenuRoot *actual_mr;
  int retval = MENU_NOP;
  MenuRoot *PrevPrevMenu;
  MenuItem *PrevPrevItem;
  int PrevPrevY;
  int x, y, ChildY;
  Window Child;

  XQueryPointer( dpy, Scr.Root, &JunkRoot, &Child,
		&JunkX,&ChildY, &x, &y, &JunkMask);
  XQueryPointer( dpy, ActiveMenu->w, &JunkRoot, &JunkChild,
		&JunkX, &ChildY, &x, &y, &JunkMask);

  /* look for the entry that the mouse is in */
  for(mi=ActiveMenu->first; mi; mi=mi->next)
    if(y>=mi->y_offset && y<mi->y_offset+mi->y_height)
      break;
  if(x<0 || x>ActiveMenu->width)
    mi = NULL;


  /* if we weren't on the active entry, let's turn the old active one off */
  if ((ActiveItem)&&(mi!=ActiveItem))
    {
      ActiveItem->state = 0;
      PaintEntry(ActiveMenu, ActiveItem);
    }

  /* if we weren't on the active item, change the active item and turn it on */
  if ((mi!=ActiveItem)&&(mi != NULL))
    {
      mi->state = 1;
/* blah */
      if (mi->func != F_TITLE)
        PaintEntry(ActiveMenu, mi);
    }
  ActiveItem = mi;

  if(ActiveItem)
    {
      /* create a new sub-menu */
      if((ActiveItem->func == F_POPUP)&& (x>(3*ActiveMenu->width>>2)))
	{
	  PrevPrevMenu = PrevMenu;
	  PrevPrevItem = PrevItem;
	  PrevPrevY = PrevY;
	  PrevY = MenuY;
	  PrevMenu = ActiveMenu;
	  PrevItem = ActiveItem;
	  retval = do_menu(ActiveItem->menu);
	  /* Unfortunately, this is needed (why?) for multi-screen operation */
	  flush_expose(ActiveMenu->w);
	  for (mi = ActiveMenu->first; mi != NULL; mi = mi->next)
	    {
	      PaintEntry(ActiveMenu, mi);
	    }
	  XSync(dpy, 0);
	  MenuY = PrevY;
	  PrevMenu = PrevPrevMenu;
	  PrevItem = PrevPrevItem;
	  PrevY = PrevPrevY;
	}
    }
  /* end a sub-menu */ 
  if (XFindContext (dpy, Child,MenuContext,(caddr_t *)&actual_mr)==XCNOENT)
    {
      return retval;
    }

  if(actual_mr != ActiveMenu)
    {
      if(actual_mr == PrevMenu)
	{
	  if((PrevItem->y_offset + PrevY > ChildY)||
	     ((PrevItem->y_offset+PrevItem->y_height + PrevY) < ChildY))
	    {
              
	      return SUBMENU_DONE;
	    }
	}
      else
      {
	return SUBMENU_DONE;
      }
    }
  return retval;
}

/***********************************************************************
 * Procedure
 * 	menuShortcuts() - Menu keyboard processing (pete@tecc.co.uk)
 *
 * Function called from UpdateMenu instead of Keyboard_Shortcuts()
 * when a KeyPress event is received.  If the key is alphanumeric,
 * then the menu is scanned for a matching hot key.  Otherwise if
 * it was the escape key then the menu processing is aborted.
 * If none of these conditions are true, then the default processing
 * routine is called.
 ***********************************************************************/
void menuShortcuts(XEvent *ev) 
{
  MenuItem *mi;
  KeySym keysym = XLookupKeysym(&ev->xkey,0);
  
  /* Try to match hot keys */
if (menu_on)  {
  if (((keysym >= XK_a) && (keysym <= XK_z)) ||	/* Only consider alphabetic */
      ((keysym >= XK_A) && (keysym <= XK_Z)) ||	/* Only consider alphabetic */
      ((keysym >= XK_0) && (keysym <= XK_9)))	/* ...or numeric keys	*/
    {
      keysym=toupper(keysym);
      /* Search menu for matching hotkey */
      for (mi = ActiveMenu->first; mi; mi = mi->next) 
	{
	  char key;
	  if (mi->hotkey == 0) continue;	/* Item has no hotkey	*/
	  key = mi->hotkey;
	  
	  if (keysym == key)
	    {		/* Are they equal?		*/
	      ActiveItem = mi;		/* Yes: Make this the active item */
              if (MenuMode == MWAIT)  MenuMode = MCLICK;
                  IgnoreRelease = 0;
                  ev->type = ButtonRelease;
	      return;
	    }
	}
    }
}  
  switch(keysym)		/* Other special keyboard handling	*/
    {
    case XK_Escape:		/* Escape key pressed. Abort		*/
      ActiveItem = NULL;	/* No selection				*/
          ev->type = ButtonRelease;
      if (MenuMode == MWAIT)  MenuMode = MCLICK;
      IgnoreRelease = 0;
      break;
      
      /* Nothing special --- Allow other shortcuts (cursor movement)	*/
    default:
      if (MenuMode != MHOLD) {
	  MenuMode = MCLICK;
          IgnoreRelease = 0;
      }
          Keyboard_shortcuts(ev,ButtonRelease);

      break;
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	Updates menu display to reflect the highlighted item
 * 
 *  Returns:
 *      0 on error condition
 *      1 on return from submenu to parent menu
 *      2 on button release return
 *
 ***********************************************************************/
int UpdateMenu(void)
{
  int done,func;
  int retval;
  MenuRoot *actual_mr;

  FindEntry();
  while (TRUE)
    {
      /* block until there is an event */

      XMaskEvent(dpy, ButtonPressMask|ButtonReleaseMask|ExposureMask | 
		 KeyPressMask | ButtonMotionMask, &Event);
      StashEventTime(&Event);
      if ((MenuMode == MWAIT) && (lastTimestamp > MenuModeCommitTime))
	MenuMode = MHOLD;
      done = 0;
      if (Event.type == MotionNotify) 
	{
	  /* discard any extra motion events before a release */
	  while((XCheckMaskEvent(dpy,ButtonMotionMask|ButtonReleaseMask,
				 &Event))&&(Event.type != ButtonRelease));
	}
      /* Handle a limited number of key press events to allow mouseless
       * operation */
      if(Event.type == KeyPress)
	menuShortcuts(&Event);

      switch(Event.type)
	{
        
	case ButtonPress:
            IgnoreRelease = 0;
            done=1;
            break;

        case ButtonRelease:
	  if (MenuMode == MWAIT) {
	    MenuMode = MCLICK;
                  IgnoreRelease = 0;
                  break;
              }
	  if ((MenuMode == MCLICK) && IgnoreRelease) {
	    IgnoreRelease = 0;
	    break;
          } 
          IgnoreRelease = 1;
          PopDownMenu();
          BottomOfPage = 0;
          if(ActiveItem)
            {
              func = ActiveItem->func;
              done = 1;
              if(ButtonWindow)
                {
                  ExecuteFunction(func, ActiveItem->action,
                                  ButtonWindow->frame,
                                  ButtonWindow, &Event, Context,
                                  ActiveItem->val1,ActiveItem->val2,
                                  ActiveItem->val1_unit,ActiveItem->val2_unit,
                                  ActiveItem->menu,-1);
                }
              else
                {
                  ExecuteFunction(func,ActiveItem->action,
                                  None,None, &Event,
                                  Context,ActiveItem->val1,
                        ActiveItem->val2,
                                  ActiveItem->val1_unit,ActiveItem->val2_unit,
                                  ActiveItem->menu,-1);

                }

            }
          ActiveItem = NULL;
          ActiveMenu = NULL;
          menuFromFrameOrWindowOrTitlebar = FALSE;
          return MENU_DONE;
        
        case KeyPress:
	case VisibilityNotify:
	  done=1;
	  break;

	case MotionNotify:
	  done = 1;

	  retval = FindEntry();
	  if((retval == MENU_DONE)||(retval == SUBMENU_DONE))
	    {
	      PopDownMenu();	      
	      ActiveItem = NULL;
	      ActiveMenu = NULL;
	      menuFromFrameOrWindowOrTitlebar = FALSE;
	    }

	  if(retval == MENU_DONE)
	    return MENU_DONE;
	  else if (retval == SUBMENU_DONE)
	    return MENU_NOP;

	  break;

	case Expose:
	  /* grab our expose events, let the rest go through */
	  if((XFindContext(dpy, Event.xany.window,MenuContext,
			   (caddr_t *)&actual_mr)!=XCNOENT))
	    {
	      PaintMenu(actual_mr,&Event);
	      done = 1;
	    }
	  break;

	default:
	  break;
	}
      
      if(!done)DispatchEvent();
      XFlush(dpy);
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	PopUpMenu - pop up a pull down menu
 *
 *  Inputs:
 *	menu	- the root pointer of the menu to pop up
 *	x, y	- location of upper left of menu
 *      center	- whether or not to center horizontally over position
 *
 ***********************************************************************/
Bool PopUpMenu (MenuRoot *menu, int x, int y)
{

  if ((!menu)||(menu->w == None)||(menu->items == 0)||(menu->in_use))
    return False;

  menu_on++;
  InstallRootColormap();

  Stashed_X = x;
  Stashed_Y = y;
  
  /* pop up the menu */
  ActiveMenu = menu;
  ActiveItem = NULL;
  
  x -= (menu->width >> 1);
  y -= (Scr.EntryHeight >> 1);
  
if ( BottomOfPage != 1 )
{
  if((Tmp_win)&&(menu_on == 1)&&(Context&C_LALL))
    {
      y = Tmp_win->frame_y+Tmp_win->boundary_width+Tmp_win->title_height+1;
      x = Tmp_win->frame_x + Tmp_win->boundary_width + 
	Button*Tmp_win->title_height+1;
    }
  if((Tmp_win)&&(menu_on==1)&&(Context&C_RALL))
    {
      y = Tmp_win->frame_y+Tmp_win->boundary_width+Tmp_win->title_height+1;
      x = Tmp_win->frame_x +Tmp_win->frame_width - Tmp_win->boundary_width-
	Button*Tmp_win->title_height - menu->width+1;
    }
  if((Tmp_win)&&(menu_on==1)&&(Context&C_TITLE))
    {
      y = Tmp_win->frame_y+Tmp_win->boundary_width+Tmp_win->title_height+1;
      if(x < Tmp_win->frame_x + Tmp_win->title_x)
	x = Tmp_win->frame_x + Tmp_win->title_x;
      if((x + menu->width) >
	 (Tmp_win->frame_x + Tmp_win->title_x +Tmp_win->title_width))
	x = Tmp_win->frame_x + Tmp_win->title_x +Tmp_win->title_width-
	  menu->width +1;
    }

  /* clip to screen */
  if (x + menu->width > Scr.MyDisplayWidth-2) 
    x = Scr.MyDisplayWidth - menu->width-2;
  if (x < 0) x = 0;

  if (y + menu->height > Scr.MyDisplayHeight-2) 
    {
      if (menu_on == 1)
      BottomOfPage = 1;
      y = Scr.MyDisplayHeight - menu->height-2;
      /* Warp pointer to middle of top line */
/*      XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.MyDisplayWidth, 
		     Scr.MyDisplayHeight, 
		     x + (menu->width>>1), (y + (Scr.EntryHeight >> 1)));
*/
    }
  if (y < 0) y = 0;
}
else 
{
    y = (Scr.MyDisplayHeight - (menu->height+2));
    if (y < 0) y = 0;

    if (x + menu->width > Scr.MyDisplayWidth-2)
        x = Scr.MyDisplayWidth - menu->width-2;
    if (x < 0) x = 0;

}

  MenuY = y;
  XMoveWindow(dpy, menu->w, x, y);
  XMapRaised(dpy, menu->w);
  menu->in_use = True;
  return True;
}


/***********************************************************************
 *
 *  Procedure:
 *	PopDownMenu - unhighlight the current menu selection and
 *		take down the menus
 *
 ***********************************************************************/
void PopDownMenu()
{
  if (ActiveMenu == NULL)
    return;
  
  menu_on--;
  if (ActiveItem)
    ActiveItem->state = 0;
  
  XUnmapWindow(dpy, ActiveMenu->w);

  UninstallRootColormap();
  XFlush(dpy);
  if (Context & (C_WINDOW | C_FRAME | C_TITLE | C_SIDEBAR))
    menuFromFrameOrWindowOrTitlebar = TRUE;
  else
    menuFromFrameOrWindowOrTitlebar = FALSE;
  ActiveMenu->in_use = FALSE;
}

/***************************************************************************
 * 
 * Wait for all mouse buttons to be released 
 * This can ease some confusion on the part of the user sometimes 
 * 
 * Discard superflous button events during this wait period.
 *
 ***************************************************************************/
void WaitForButtonsUp()
{
  Bool AllUp = False;
  XEvent JunkEvent;
  unsigned int mask;

  while(!AllUp)
    {
      XAllowEvents(dpy,ReplayPointer,CurrentTime);
      XQueryPointer( dpy, Scr.Root, &JunkRoot, &JunkChild,
		    &JunkX, &JunkY, &JunkX, &JunkY, &mask);    
      
      if((mask&
	  (Button1Mask|Button2Mask|Button3Mask|Button4Mask|Button5Mask))==0)
	AllUp = True;
    }
  XSync(dpy,0);
  while(XCheckMaskEvent(dpy,
			ButtonPressMask|ButtonReleaseMask|ButtonMotionMask,
			&JunkEvent))
    {
      StashEventTime (&JunkEvent);
      XAllowEvents(dpy,ReplayPointer,CurrentTime);
    }

}



