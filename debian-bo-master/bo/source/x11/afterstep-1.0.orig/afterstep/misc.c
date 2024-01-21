/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 *
 * slightly modified by Bo Yang
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


/**************************************************************************
 *
 * Assorted odds and ends
 *
 **************************************************************************/


#include "../configure.h"

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>
#include <limits.h>

#include "afterstep.h"
#include <X11/Xatom.h>
#include "menus.h"
#include "misc.h"
#include "parse.h"
#include "screen.h"
#include "module.h"

XGCValues Globalgcv;
unsigned long Globalgcm;


/**************************************************************************
 * 
 * Releases dynamically allocated space used to store window/icon names
 *
 **************************************************************************/
void free_window_names (ASWindow *tmp, Bool nukename, Bool nukeicon)
{
  if (!tmp)
    return;
  
  if (nukename && nukeicon) 
    {
      if (tmp->name == tmp->icon_name) 
	{
	  if (tmp->name != NoName)
	    XFree (tmp->name);
	  tmp->name = NULL;
	  tmp->icon_name = NULL;
	}
      else
	{
	  if (tmp->name != NoName)
	    XFree (tmp->name);
	  tmp->name = NULL;
	  if (tmp->icon_name != NoName)
	    XFree (tmp->icon_name);
	  tmp->icon_name = NULL;
	}
    }   
  else if (nukename) 
    {
      if (tmp->name != tmp->icon_name && tmp->name != NoName)
	XFree (tmp->name);
      tmp->name = NULL;
    }
  else
    { /* if (nukeicon) */
      if (tmp->icon_name != tmp->name && tmp->icon_name != NoName)
	XFree (tmp->icon_name);
      tmp->icon_name = NULL;
    }
  
  return;
}

/***************************************************************************
 *
 * Handles destruction of a window 
 *
 ****************************************************************************/
void Destroy(ASWindow *Tmp_win)
{ 
  int i;
  extern ASWindow *ButtonWindow;
  extern ASWindow *colormap_win;
  /*
   * Warning, this is also called by HandleUnmapNotify; if it ever needs to
   * look at the event, HandleUnmapNotify will have to mash the UnmapNotify
   * into a DestroyNotify.
   */
  if(!Tmp_win)
    return;

  XUnmapWindow(dpy, Tmp_win->frame);
  XSync(dpy,0);
  
  if(Tmp_win == Scr.Hilite)
    Scr.Hilite = NULL;
  
  Broadcast(M_DESTROY_WINDOW,3,Tmp_win->w,Tmp_win->frame,
	    (unsigned long)Tmp_win,0,0,0,0);

  if(Scr.PreviousFocus == Tmp_win)
    Scr.PreviousFocus = NULL;

  if(ButtonWindow == Tmp_win)
    ButtonWindow = NULL;

  if((Tmp_win == Scr.Focus)&&(Scr.flags & ClickToFocus))
    {
      ASWindow *t, *tn = NULL;
      long best = LONG_MIN;
      for (t = Scr.ASRoot.next; t != NULL; t = t->next)
        {
	  if ((t->focus_sequence > best) && (t != Tmp_win))
	    {
	      best = t->focus_sequence;
	      tn = t;
	    }
	}
      if (tn)
	SetFocus(tn->w, tn, False);
      else
	SetFocus(Scr.NoFocusWin, NULL, False);
    }

  if(Scr.Focus == Tmp_win)
    SetFocus(Scr.NoFocusWin, NULL, False);

  if(Tmp_win == Scr.pushed_window)
    Scr.pushed_window = NULL;

  if(Tmp_win == colormap_win)
    colormap_win = NULL;

  XDestroyWindow(dpy, Tmp_win->frame);
  XDeleteContext(dpy, Tmp_win->frame, ASContext);
  
  XDestroyWindow(dpy, Tmp_win->Parent);

  XDeleteContext(dpy, Tmp_win->Parent, ASContext);

  XDeleteContext(dpy, Tmp_win->w, ASContext);
  

#ifndef NO_PAGER
  if ((Scr.Pager_w)&& !(Tmp_win->flags & STICKY))
    XDestroyWindow(dpy, Tmp_win->pager_view);
#endif
    
  if((Tmp_win->flags &ICON_OURS)&&(Tmp_win->icon_pixmap_w != None))
    XDestroyWindow(dpy, Tmp_win->icon_pixmap_w);
  if(Tmp_win->icon_pixmap_w != None)
    XDeleteContext(dpy, Tmp_win->icon_pixmap_w, ASContext);

  if (Tmp_win->flags & TITLE)
    {
      XDeleteContext(dpy, Tmp_win->title_w, ASContext);
      for(i=0;i<Scr.nr_left_buttons;i++)
	XDeleteContext(dpy, Tmp_win->left_w[i], ASContext);
      for(i=0;i<Scr.nr_right_buttons;i++)
	if(Tmp_win->right_w[i] != None)
	  XDeleteContext(dpy, Tmp_win->right_w[i], ASContext);
    }
  if (Tmp_win->flags & BORDER)
    {
      XDeleteContext(dpy, Tmp_win->side, ASContext);
      for(i=0;i<2;i++)
	XDeleteContext(dpy, Tmp_win->corners[i], ASContext);
    }
  
  Tmp_win->prev->next = Tmp_win->next;
  if (Tmp_win->next != NULL)
    Tmp_win->next->prev = Tmp_win->prev;
  free_window_names (Tmp_win, True, True);		
  if (Tmp_win->wmhints)					
    XFree ((char *)Tmp_win->wmhints);
  if (Tmp_win->class.res_name && Tmp_win->class.res_name != NoName)  
    XFree ((char *)Tmp_win->class.res_name);
  if (Tmp_win->class.res_class && Tmp_win->class.res_class != NoName) 
    XFree ((char *)Tmp_win->class.res_class);
  if(Tmp_win->mwm_hints)
    XFree((char *)Tmp_win->mwm_hints);

  if(Tmp_win->cmap_windows != (Window *)NULL)
    XFree((void *)Tmp_win->cmap_windows);
#ifdef ENABLE_TEXTURE
  if (Tmp_win->backPixmap != None)
    XFreePixmap(dpy, Tmp_win->backPixmap);
  if (Tmp_win->backPixmap2 != None)
    XFreePixmap(dpy, Tmp_win->backPixmap2);
#endif
    
  free((char *)Tmp_win);

#ifndef NO_PAGER
  RedrawPager();
#endif
  XSync(dpy,0);
  return;
}



/**************************************************************************
 *
 * Removes expose events for a specific window from the queue 
 *
 *************************************************************************/
int flush_expose (Window w)
{
  XEvent dummy;
  int i=0;
  
  while (XCheckTypedWindowEvent (dpy, w, Expose, &dummy))i++;
  return i;
}



/***********************************************************************
 *
 *  Procedure:
 *	RestoreWithdrawnLocation
 * 
 *  Puts windows back where they were before afterstep took over 
 *
 ************************************************************************/
void RestoreWithdrawnLocation (ASWindow *tmp,Bool restart)
{
  int a,b,w2,h2;
  unsigned int bw,mask;
  XWindowChanges xwc;
  
  if(!tmp)
    return;
  
  if (XGetGeometry (dpy, tmp->w, &JunkRoot, &xwc.x, &xwc.y, 
		    &JunkWidth, &JunkHeight, &bw, &JunkDepth)) 
    {
      XTranslateCoordinates(dpy,tmp->frame,Scr.Root,xwc.x,xwc.y,
			    &a,&b,&JunkChild);
      xwc.x = a + tmp->xdiff;
      xwc.y = b + tmp->ydiff;
      xwc.border_width = tmp->old_bw;
      mask = (CWX | CWY|CWBorderWidth);
      
      /* We can not assume that the window is currently on the screen.
       * Although this is normally the case, it is not always true.  The
       * most common example is when the user does something in an
       * application which will, after some amount of computational delay,
       * cause the window to be unmapped, but then switches screens before
       * this happens.  The XTranslateCoordinates call above will set the
       * window coordinates to either be larger than the screen, or negative.
       * This will result in the window being placed in odd, or even
       * unviewable locations when the window is remapped.  The followin code
       * forces the "relative" location to be within the bounds of the display.
       *
       * gpw -- 11/11/93
       *
       * Unfortunately, this does horrendous things during re-starts, 
       * hence the "if(restart) clause (RN) 
       *
       * Also, fixed so that it only does this stuff if a window is more than
       * half off the screen. (RN)
       */
      
      if(!restart)
	{
	  /* Don't mess with it if its partially on the screen now */
	  if((tmp->frame_x < 0)||(tmp->frame_y<0)||
	     (tmp->frame_x >= Scr.MyDisplayWidth)||
	     (tmp->frame_y >= Scr.MyDisplayHeight))
	    {
	      w2 = (tmp->frame_width>>1);
	      h2 = (tmp->frame_height>>1);
	      if (( xwc.x < -w2) || (xwc.x > (Scr.MyDisplayWidth-w2 )))
		{
		  xwc.x = xwc.x % Scr.MyDisplayWidth;
		  if ( xwc.x < -w2 )
		    xwc.x += Scr.MyDisplayWidth;
		}
	      if ((xwc.y < -h2) || (xwc.y > (Scr.MyDisplayHeight-h2 )))
		{
		  xwc.y = xwc.y % Scr.MyDisplayHeight;
		  if ( xwc.y < -h2 )
		    xwc.y += Scr.MyDisplayHeight;
		}
	    }
	}
      XReparentWindow (dpy, tmp->w,Scr.Root,xwc.x,xwc.y);
      
      if((tmp->flags & ICONIFIED)&&(!(tmp->flags & SUPPRESSICON)))
	{
	  if (tmp->icon_pixmap_w) 
	    XUnmapWindow(dpy, tmp->icon_pixmap_w);	  
	}
      
      XConfigureWindow (dpy, tmp->w, mask, &xwc);
      XSync(dpy,0);
    }
}


/***************************************************************************
 *
 * Start/Stops the auto-raise timer
 *
 ****************************************************************************/
void SetTimer(int delay)
{
#ifdef HAVE_SETITIMER
  struct itimerval value;
  
  value.it_value.tv_usec = 1000*(delay%1000);
  value.it_value.tv_sec = delay/1000;
  value.it_interval.tv_usec = 0;
  value.it_interval.tv_sec = 0;
  setitimer(ITIMER_REAL,&value,NULL);
#endif
}

/***************************************************************************
 *
 * ICCCM Client Messages - Section 4.2.8 of the ICCCM dictates that all
 * client messages will have the following form:
 *
 *     event type	ClientMessage
 *     message type	_XA_WM_PROTOCOLS
 *     window		tmp->w
 *     format		32
 *     data[0]		message atom
 *     data[1]		time stamp
 *
 ****************************************************************************/
void send_clientmessage (Window w, Atom a, Time timestamp)
{
  XClientMessageEvent ev;
  
  ev.type = ClientMessage;
  ev.window = w;
  ev.message_type = _XA_WM_PROTOCOLS;
  ev.format = 32;
  ev.data.l[0] = a;
  ev.data.l[1] = timestamp;
  XSendEvent (dpy, w, False, 0L, (XEvent *) &ev);
}





/****************************************************************************
 *
 * Records the time of the last processed event. Used in XSetInputFocus
 *
 ****************************************************************************/
Time lastTimestamp = CurrentTime;	/* until Xlib does this for us */

Bool StashEventTime (XEvent *ev)
{
  Time NewTimestamp = CurrentTime;

  switch (ev->type) 
    {
    case KeyPress:
    case KeyRelease:
      NewTimestamp = ev->xkey.time;
      break;
    case ButtonPress:
    case ButtonRelease:
      NewTimestamp = ev->xbutton.time;
      break;
    case MotionNotify:
      NewTimestamp = ev->xmotion.time;
      break;
    case EnterNotify:
    case LeaveNotify:
      NewTimestamp = ev->xcrossing.time;
      break;
    case PropertyNotify:
      NewTimestamp = ev->xproperty.time;
      break;
    case SelectionClear:
      NewTimestamp = ev->xselectionclear.time;
      break;
    case SelectionRequest:
      NewTimestamp = ev->xselectionrequest.time;
      break;
    case SelectionNotify:
      NewTimestamp = ev->xselection.time;
      break;
    default:
      return False;
    }
  if(NewTimestamp > lastTimestamp)
    lastTimestamp = NewTimestamp;
  return True;
}


/******************************************************************************
 *
 * Move a window to the top (dir 1) or bottom (dir -1) of the circulate seq.
 * 
 *****************************************************************************/

void SetCirculateSequence(ASWindow* tw, int dir)
{
  ASWindow *t;
  long best = (dir == -1) ? LONG_MAX : LONG_MIN;

  t = Scr.ASRoot.next;
  if (t)
    {
      do
        {
	  if ((dir == -1) ? (t->circulate_sequence < best)
	      : (t->circulate_sequence > best))
	      best = t->circulate_sequence;
	} while ((t = t->next));
    }
  else
      best = 0;

  tw->circulate_sequence = best + dir;
}



/******************************************************************************
 *
 * Versions of grab primitives that circumvent modifier problems
 * 
 *****************************************************************************/

unsigned mygrabs_no_mods[] = { 0 };

void MyXGrabButton(Display* display, unsigned button, unsigned modifiers,
		   Window grab_window, Bool owner_events, unsigned event_mask,
		   int pointer_mode, int keyboard_mode, Window confine_to,
		   Cursor cursor)
{
    unsigned mod, *mods;
    mods = (modifiers != AnyModifier) ? Scr.lock_mods : mygrabs_no_mods;
    do {
	mod = *mods++;
	XGrabButton(display, button, modifiers | mod, grab_window,
		    owner_events, event_mask, pointer_mode, keyboard_mode,
		    confine_to, cursor);
    } while (mod);
}

void MyXUngrabButton(Display* display, unsigned button, unsigned modifiers,
		     Window grab_window)
{
    unsigned mod, *mods;
    mods = (modifiers != AnyModifier) ? Scr.lock_mods : mygrabs_no_mods;
    do {
	mod = *mods++;
	XUngrabButton(display, button, modifiers | mod, grab_window);
    } while (mod);
}

void MyXGrabKey(Display* display, int keycode, unsigned modifiers,
		Window grab_window, Bool owner_events, int pointer_mode,
		int keyboard_mode)
{
    unsigned mod, *mods;
    mods = (modifiers != AnyModifier) ? Scr.lock_mods : mygrabs_no_mods;
    do {
	mod = *mods++;
	XGrabKey(display, keycode, modifiers | mod, grab_window, owner_events,
		 pointer_mode, keyboard_mode);
    } while (mod);
}

/******************************************************************************
 *
 * Grab ClickToRaise button press events for a window
 * 
 *****************************************************************************/
void GrabRaiseClick(ASWindow* t)
{
  int b;
  for (b = 1; b <= MAX_BUTTONS; b++)
    {
      if (Scr.RaiseButtons & (1<<b))
	  MyXGrabButton(dpy, b, 0, t->w, True, ButtonPressMask, GrabModeSync,
		      GrabModeAsync, None, Scr.ASCursors[TITLE_CURSOR]);
    }
}

/******************************************************************************
 *
 * Ungrab ClickToRaise button press events to allow their use in applications
 * 
 *****************************************************************************/
void UngrabRaiseClick(ASWindow* t)
{
  int b;
  for (b = 1; b <= MAX_BUTTONS; b++)
    {
	if (Scr.RaiseButtons & (1<<b))
	  MyXUngrabButton(dpy, b, 0, t->w);
    }
}

/******************************************************************************
 *
 * Recalculate the visibility flags
 * 
 *****************************************************************************/

void UpdateVisibility(void)
{
    ASWindow *t, *s, *tbase;
    int tx1, ty1, tx2, ty2;
    int ontop = 1, visible;

    tbase = Scr.ASRoot.next;
    for (t = Scr.ASRoot.next; t != NULL; t = t->next)
      {
	if (t->flags & MAPPED)
	  {
	    tx1 = t->frame_x;
	    ty1 = t->frame_y;
	    tx2 = t->frame_x + t->frame_width;
	    ty2 = t->frame_y + t->frame_height;
	  }
	else if (t->flags & ICONIFIED)
	  {
	    tx1 = t->icon_x_loc;
	    ty1 = t->icon_y_loc;
	    tx2 = t->icon_x_loc + t->icon_p_width;
	    ty2 = t->icon_y_loc + t->icon_p_height;
	  }
        else if (t->flags & SHADED)
          {
            tx1 = t->frame_x;
            ty1 = t->frame_y;
            tx2 = t->frame_x + t->frame_width;
            ty2 = t->frame_y + NS_TITLE_HEIGHT;
          }
	else continue;

	if (ontop && !(t->flags & ONTOP))
	  {
	    ontop = 0;
	    tbase = t;
	  }
	if ((tx2 > 0) && (tx1 < Scr.MyDisplayWidth) &&
	    (ty2 > 0) && (ty1 < Scr.MyDisplayHeight))
	  {
	    visible = VISIBLE;
	    for (s = tbase; s != t; s = s->next)
	      {
		if ((s->flags & TRANSIENT) && (s->transientfor == t->w))
		    continue;
		if (s->flags & MAPPED)
		  {
		    if ((tx2 > s->frame_x)&&(tx1 < s->frame_x+s->frame_width)&&
			(ty2 > s->frame_y)&&(ty1 < s->frame_y+s->frame_height))
		      {
			visible = 0;
			break;
		      }
		  }
		else if (s->flags & ICONIFIED)
		  {
		    if ((tx2 > s->icon_x_loc)&&(tx1 < s->icon_x_loc+s->icon_p_width) &&
			(ty2 > s->icon_y_loc)&&(ty1 < s->icon_y_loc+s->icon_p_height))
                      {
                        visible = 0;
                        break;
                      }
                  }
                else if (s->flags & SHADED)
                  {
                    if ((tx2 > s->frame_x)&&(tx1 < s->frame_x+s->frame_width)&&
                        (ty2 > s->frame_y)&&(ty1 < s->frame_y+NS_TITLE_HEIGHT))

		      {
			visible = 0;
			break;
		      }
		  }

	      }
	  }
	else
	  visible = 0;
	if ((t->flags & VISIBLE) != visible)
	  {
	    t->flags ^= VISIBLE;
	    if ((Scr.flags & ClickToRaise) && ! (Scr.flags & ClickToFocus)
		&& (t->flags & MAPPED))
	      {
		if (visible)
		  UngrabRaiseClick(t);
		else
		  GrabRaiseClick(t);
	      }
	  }
      }
}

/******************************************************************************
 *
 * Get the correct window stacking order from the X server and
 * make sure everything that depends on the order is fine and dandy
 * 
 *****************************************************************************/

void CorrectStackOrder(void)
{
    Window root, parent, *children, *cp;
    unsigned nchildren;
    ASWindow *t;

    if (XQueryTree(dpy, Scr.ASRoot.w, &root, &parent, &children,
		   &nchildren))
      {
	for (cp = children; nchildren-- > 0; cp++)
	  {
	    if ((XFindContext(dpy, *cp, ASContext, (caddr_t*) &t) == XCSUCCESS)
		&& (t->frame == *cp))
	      {
		if (t != Scr.ASRoot.next)
		  {
		    t->prev->next = t->next;
		    if (t->next)  t->next->prev = t->prev;
		    t->next = Scr.ASRoot.next;
		    t->prev = &Scr.ASRoot;
		    Scr.ASRoot.next->prev = t;
		    Scr.ASRoot.next = t;
		  }
	      }
	  }
	XFree(children);
      }
    else
      {
	fprintf(stderr, "CorrectStackOrder(): XQueryTree failed!\n");
      }

    t = Scr.ASRoot.next;
    if (t)
      {
        if (t->flags & ONTOP)
          {
	    do { t = t->next; } while (t && (t->flags & ONTOP));
	    if (t)
	      {
	        do { t = t->next; } while (t && ! (t->flags & ONTOP));
	        if (t)  RaiseWindow(Scr.ASRoot.next);
	      }
	  }
        else
          {
	    do { t = t->next; } while (t && ! (t->flags & ONTOP));
	    if (t)  RaiseWindow(t);
	  }
      }

    UpdateVisibility();
}
