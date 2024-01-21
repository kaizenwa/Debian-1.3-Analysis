/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 *
 * later modified for BowMan
 * by Bo Yang
 *
 * again modified for AfterStep
 * by Frank Fejes
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
 * afterstep event handling
 *
 ***********************************************************************/

#include "../configure.h"

#ifdef ISC
#include <sys/bsdtypes.h>
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>
#include <limits.h>
/* Some people say that AIX and AIXV3 need 3 preceding underscores, other say
 * no. I'll do both */
#if defined ___AIX || defined _AIX || defined __QNX__ || defined ___AIXV3 || defined AIXV3 || defined _SEQUENT_
#include <sys/select.h>
#endif

#include "afterstep.h"
#include <X11/Xatom.h>
#include "menus.h"
#include "misc.h"
#include "parse.h"
#include "screen.h"
#ifdef SHAPE
#include <X11/extensions/shape.h>
#endif /* SHAPE */
#include "module.h"

#ifndef NO_PAGER
extern ASWindow *FindCounterpart(Window target);
#endif /* NO_PAGER */

extern int menuFromFrameOrWindowOrTitlebar;

static void ChangeIcon();

int Context = C_NO_CONTEXT;	/* current button press context */
int Button = 0;
ASWindow *ButtonWindow;	/* button press window structure */
XEvent Event;			/* the current event */
ASWindow *Tmp_win;		/* the current afterstep window */

int last_event_type=0;
Window last_event_window=0;

#ifdef SHAPE
extern int ShapeEventBase;
void HandleShapeNotify(void);
#endif /* SHAPE */

Window PressedW;

/***********************************************************************
 *
 *  Procedure:
 *	DispatchEvent - handle a single X event stored in global var Event
 *
 ************************************************************************/
void DispatchEvent()
{
  Window w = Event.xany.window;

  StashEventTime(&Event);

  if (XFindContext (dpy, w, ASContext, (caddr_t *) &Tmp_win) == XCNOENT)
    Tmp_win = NULL;
  last_event_type = Event.type;
  last_event_window = w;

  switch(Event.type)
    {
    case Expose:
      HandleExpose();
      break;
    case DestroyNotify:
      HandleDestroyNotify();
      break;
    case MapRequest:
      HandleMapRequest();
      break;
    case MapNotify:
      HandleMapNotify();
      break;
    case UnmapNotify:
      HandleUnmapNotify();
      break;
    case MotionNotify:
      HandleMotionNotify();
      break;
    case ButtonPress:
      HandleButtonPress();
      break;
    case ButtonRelease:
      HandleButtonRelease();
      break;
    case EnterNotify:
      HandleEnterNotify();
      break;
    case LeaveNotify:
      HandleLeaveNotify();
      break;
    case FocusIn:
      HandleFocusIn();
      break;
    case ConfigureRequest:
      HandleConfigureRequest();
      break;
    case ClientMessage:
      HandleClientMessage();
      break;
    case PropertyNotify:
      HandlePropertyNotify();
      break;
    case KeyPress:
      HandleKeyPress();
      break;
    case ColormapNotify:
      HandleColormapNotify();
      break;
    default:
#ifdef SHAPE
      if(Event.type == (ShapeEventBase + ShapeNotify)) 
	HandleShapeNotify();
#endif /* SHAPE */
      
      break;
    }
  return;
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleEvents - handle X events
 *
 ************************************************************************/
void HandleEvents()
{
  while (TRUE)
    {
      last_event_type = 0;
      if(My_XNextEvent(dpy, &Event))
	{
	  DispatchEvent ();
	}
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	Find the AS context for the Event.
 *
 ************************************************************************/
int GetContext(ASWindow *t, XEvent *e, Window *w)
{
  int i;

  if (t == NULL)
      return C_ROOT; 
  
  *w= e->xany.window;
  
  if ((*w == Scr.NoFocusWin) || (*w == Scr.Root))
      return C_ROOT;
  
  /* Since key presses and button presses are grabbed in the frame
   * when we have re-parented windows, we need to find out the real
   * window where the event occured */
  if (! (t->flags & ICONIFIED))
    {
      if (e->type == KeyPress)
        {
	  if (e->xkey.subwindow != None)
	      *w = e->xany.window = e->xkey.subwindow;
	}
      else if (e->type == ButtonPress)
        {
	  if (e->xbutton.subwindow != None)
	      *w = e->xany.window = e->xbutton.subwindow;
	  else if ((*w == t->frame) && (e->xbutton.y < NS_TITLE_HEIGHT+2))
	      return C_TITLE;
	}
    }

  if (*w == t->title_w)  return C_TITLE;
  if ((*w == t->icon_pixmap_w) || (t->flags & ICONIFIED))  return C_ICON;
  if ((*w == t->frame) || (*w == t->side))  return C_SIDEBAR;

  for(i = 0; i < 2; i++)
    {
      if(*w == t->corners[i])
      {
	  Button = i;
	  return C_FRAME;
      }
    }
  for(i = 0; i < Scr.nr_left_buttons; i++)
    {
      if(*w == t->left_w[i])
        {
	  Button = i;
	  return (1<<i)*C_L1;
	}
    }
  for(i = 0; i < Scr.nr_right_buttons; i++)
  {
      if(*w == t->right_w[i])
      {
	  Button = i;
	  return (1<<i)*C_R1;
      }
  }

  *w = t->w;
  return C_WINDOW;
}

/***********************************************************************
 *
 *  Procedure:
 *	HandleFocusIn - handles focus in events
 *
 ************************************************************************/
void HandleFocusIn()
{
  XEvent d;
  Window w;
  
  w= Event.xany.window;
  while(XCheckTypedEvent(dpy,FocusIn,&d))
    {
      w = d.xany.window;
    }
  if (XFindContext (dpy, w, ASContext, (caddr_t *) &Tmp_win) == XCNOENT)
    {
      Tmp_win = NULL;
    }
  
  if(!Tmp_win)
    {
      SetBorder(Scr.Hilite,False,True,True,None);
      Broadcast(M_FOCUS_CHANGE,3,0,0,0,0,0,0,0);
    }
  else if(Tmp_win != Scr.Hilite)
    {
      SetBorder(Tmp_win,True,True,True,None);
      Broadcast(M_FOCUS_CHANGE,3,Tmp_win->w,
		Tmp_win->frame,(unsigned long)Tmp_win,0,0,0,0);
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	HandleKeyPress - key press event handler
 *
 ************************************************************************/
void HandleKeyPress()
{
  FuncKey *key;
  unsigned int modifier;
  Window dummy;
  
  Context = GetContext(Tmp_win,&Event,&dummy);

  modifier = (Event.xkey.state & Scr.nonlock_mods);
  for (key = Scr.FuncKeyRoot.next; key != NULL; key = key->next)
    {
      ButtonWindow = Tmp_win;
      /* Here's a real hack - some systems have two keys with the
       * same keysym and different keycodes. This converts all
       * the cases to one keycode. */
      Event.xkey.keycode = 
	XKeysymToKeycode(dpy,XKeycodeToKeysym(dpy,Event.xkey.keycode,0));
      if ((key->keycode == Event.xkey.keycode) &&
	  ((key->mods == (modifier&(~LockMask)))||
	   (key->mods == AnyModifier)) &&
	  (key->cont & Context))
	{
	  ExecuteFunction(key->func, key->action, Event.xany.window,Tmp_win,
			  &Event,Context,key->val1,key->val2,
			  key->val1_unit,key->val2_unit,
			  key->menu,-1);
	  return;
	}
    }
  
  /* if we get here, no function key was bound to the key.  Send it
   * to the client if it was in a window we know about.
   */
  if (Tmp_win)
    {
      if(Event.xkey.window != Tmp_win->w)
	{
	  Event.xkey.window = Tmp_win->w;
	  XSendEvent(dpy, Tmp_win->w, False, KeyPressMask, &Event);
	}
    }
  ButtonWindow = NULL;
}


/***********************************************************************
 *
 *  Procedure:
 *	HandlePropertyNotify - property notify event handler
 *
 ***********************************************************************/
#define MAX_NAME_LEN 200L		/* truncate to this many */
#define MAX_ICON_NAME_LEN 200L		/* ditto */

void HandlePropertyNotify()
{
  char *prop = NULL;
  Atom actual = None;
  int actual_format;
  unsigned long nitems, bytesafter;
  
  if ((!Tmp_win)||(XGetGeometry(dpy, Tmp_win->w, &JunkRoot, &JunkX, &JunkY,
				&JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0))
    return;
  
  switch (Event.xproperty.atom) 
    {
    case XA_WM_NAME:
      if (XGetWindowProperty (dpy, Tmp_win->w, Event.xproperty.atom, 0L, 
			      MAX_NAME_LEN, False, XA_STRING, &actual,
			      &actual_format, &nitems, &bytesafter,
			      (unsigned char **) &prop) != Success ||
	  actual == None)
	return;
      if (!prop) prop = NoName;
      free_window_names (Tmp_win, True, False);
      
      Tmp_win->name = prop;
      BroadcastName(M_WINDOW_NAME,Tmp_win->w,Tmp_win->frame,
		    (unsigned long)Tmp_win,Tmp_win->name);
      
      /* fix the name in the title bar */
      if(!(Tmp_win->flags & ICONIFIED))
	SetTitleBar(Tmp_win,(Scr.Hilite==Tmp_win),True);
	else
	  ChangeIcon();

/*
* if the icon name is NoName, set the name of the icon to be
* the same as the window 
       */
      if (Tmp_win->icon_name == NoName) 
	{
	  Tmp_win->icon_name = Tmp_win->name;
	  BroadcastName(M_ICON_NAME,Tmp_win->w,Tmp_win->frame,
			(unsigned long)Tmp_win,Tmp_win->icon_name);
	}
      break;
      
    case XA_WM_ICON_NAME:
      if (XGetWindowProperty (dpy, Tmp_win->w, Event.xproperty.atom, 0, 
			      MAX_ICON_NAME_LEN, False, XA_STRING, &actual,
			      &actual_format, &nitems, &bytesafter,
			      (unsigned char **) &prop) != Success ||
	  actual == None)
	return;
      if (!prop) prop = NoName;
      free_window_names (Tmp_win, False, True);
      Tmp_win->icon_name = prop;
      BroadcastName(M_ICON_NAME,Tmp_win->w,Tmp_win->frame,
		    (unsigned long)Tmp_win,Tmp_win->icon_name);
	  if((Tmp_win->flags & ICONIFIED))
		  ChangeIcon();
      break;
      
    case XA_WM_HINTS:
      if (Tmp_win->wmhints) 
	XFree ((char *) Tmp_win->wmhints);
      Tmp_win->wmhints = XGetWMHints(dpy, Event.xany.window);

      if(Tmp_win->wmhints == NULL)
	return;

      if((Tmp_win->wmhints->flags & IconPixmapHint)||
	 (Tmp_win->wmhints->flags & IconWindowHint))
	if(Tmp_win->icon_bitmap_file == Scr.DefaultIcon)
	  Tmp_win->icon_bitmap_file = (char *)0;
      
      if((Tmp_win->wmhints->flags & IconPixmapHint)||
         (Tmp_win->wmhints->flags & IconWindowHint))
	{
	  if (!(Tmp_win->flags & SUPPRESSICON))
	    {
	      if(Tmp_win->flags & ICON_OURS)
		{
		  if(Tmp_win->icon_pixmap_w != None)
		    {
		      XDestroyWindow(dpy,Tmp_win->icon_pixmap_w);
		      XDeleteContext(dpy, Tmp_win->icon_pixmap_w, ASContext);
		    }
		}
	      else 
		XUnmapWindow(dpy,Tmp_win->icon_pixmap_w);
	    }
          Tmp_win->icon_pixmap_w = None;
	  Tmp_win->iconPixmap = (Window)NULL;
	  if(Tmp_win->flags & ICONIFIED)
	    {
	      Tmp_win->flags &= ~ICONIFIED;
	      Tmp_win->flags &= ~ICON_UNMAPPED;
	      CreateIconWindow(Tmp_win,Tmp_win->icon_x_loc,Tmp_win->icon_y_loc);
	      Broadcast(M_ICONIFY,7,Tmp_win->w,Tmp_win->frame,
			(unsigned long)Tmp_win,
			Tmp_win->icon_x_loc,
			Tmp_win->icon_y_loc,
			Tmp_win->icon_p_width,
			Tmp_win->icon_p_height);
	      BroadcastConfig(M_CONFIGURE_WINDOW,Tmp_win);
	      
	      if (!(Tmp_win->flags & SUPPRESSICON))
		{
		  LowerWindow(Tmp_win);
		  AutoPlace(Tmp_win);
		  if(Tmp_win->Desk == Scr.CurrentDesk)
		    {
		      if(Tmp_win->icon_pixmap_w != None)
			XMapWindow(dpy, Tmp_win->icon_pixmap_w);
		    }
		}
	      Tmp_win->flags |= ICONIFIED;
	      DrawIconWindow(Tmp_win);
	    }
	}
      break;
      
    case XA_WM_NORMAL_HINTS:
      GetWindowSizeHints (Tmp_win);
      BroadcastConfig(M_CONFIGURE_WINDOW,Tmp_win);
      break;
      
    default:
      if(Event.xproperty.atom == _XA_WM_PROTOCOLS)
	FetchWmProtocols (Tmp_win);
      else if (Event.xproperty.atom == _XA_WM_COLORMAP_WINDOWS)
	{
	  FetchWmColormapWindows (Tmp_win);	/* frees old data */
	  ReInstallActiveColormap();
	}
      else if(Event.xproperty.atom == _XA_WM_STATE)
	{
	  if((Scr.flags & ClickToFocus)&&(Tmp_win == Scr.Focus)&&
	     (Tmp_win != NULL))
	    {
	      Scr.Focus = NULL;
	      SetFocus(Tmp_win->w,Tmp_win,False);
	    }
	}
      break;
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleClientMessage - client message event handler
 *
 ************************************************************************/
void HandleClientMessage()
{
  XEvent button;
  
  if ((Event.xclient.message_type == _XA_WM_CHANGE_STATE)&&
      (Tmp_win)&&(Event.xclient.data.l[0]==IconicState)&&
      !(Tmp_win->flags & ICONIFIED))
    {
      XQueryPointer( dpy, Scr.Root, &JunkRoot, &JunkChild,
		    &(button.xmotion.x_root),
		    &(button.xmotion.y_root),
		    &JunkX, &JunkY, &JunkMask);
      button.type = 0;
      ExecuteFunction(F_ICONIFY, NULLSTR, Event.xany.window,
		      Tmp_win, &button, C_FRAME,0,0, 0,0,
		      (MenuRoot *)0,-1);
#ifdef ENABLE_DND	
	/* Pass the event to the client window */
	if (Event.xclient.window != Tmp_win->w)
	{	
	    Event.xclient.window = Tmp_win->w;
	    XSendEvent(dpy,Tmp_win->w,True,NoEventMask,&Event);
	}
#endif 
    }    
}

/***********************************************************************
 *
 *  Procedure:
 *	HandleExpose - expose event handler
 *
 ***********************************************************************/
void HandleExpose()
{
  if (Event.xexpose.count != 0)
    return;
  
  if (Tmp_win)
    {
#ifndef NO_PAGER
      if((Tmp_win->w == Scr.Pager_w)||
	 (Tmp_win->w == Scr.CPagerWin))
	{
	  ReallyRedrawPager();
	}
#endif
      if ((Event.xany.window == Tmp_win->title_w))
	{
	  SetTitleBar(Tmp_win,(Scr.Hilite == Tmp_win),False);
	}
      else
	{
	  SetBorder(Tmp_win,(Scr.Hilite == Tmp_win),True,True,Event.xany.window);
	}
    }
#ifndef NO_PAGER
  else
    {
      if(FindCounterpart(Event.xany.window))
	ReallyRedrawPager();
    }
#endif
  return;
}



/***********************************************************************
 *
 *  Procedure:
 *	HandleDestroyNotify - DestroyNotify event handler
 *
 ***********************************************************************/
void HandleDestroyNotify()
{
  if (Tmp_win) {
    Destroy(Tmp_win);
    UpdateVisibility();
  }
}




/***********************************************************************
 *
 *  Procedure:
 *	HandleMapRequest - MapRequest event handler
 *
 ************************************************************************/
void HandleMapRequest()
{
  extern long isIconicState;
  
  Event.xany.window = Event.xmaprequest.window;
  
  if(XFindContext(dpy, Event.xany.window, ASContext, 
		  (caddr_t *)&Tmp_win)==XCNOENT)
    Tmp_win = NULL;
  
  XFlush(dpy);
  
  /* If the window has never been mapped before ... */
  if(!Tmp_win)
    {
      /* Add decorations. */
      Tmp_win = AddWindow(Event.xany.window);
      if (Tmp_win == NULL)
	return;
    }

  /* If it's not merely iconified, and we have hints, use them. */
  if (!(Tmp_win->flags & ICONIFIED))
    {
      int state;
      
      if(Tmp_win->wmhints && (Tmp_win->wmhints->flags & StateHint))
	state = Tmp_win->wmhints->initial_state;
      else
	state = NormalState;
      
      if(Tmp_win->flags & STARTICONIC)
	state = IconicState;

      if(isIconicState != DontCareState) 
	state = isIconicState;

      XGrabServer (dpy);
      switch (state) 
	{
	case DontCareState:
	case NormalState:
	case InactiveState:
	default:
	  if (Tmp_win->Desk == Scr.CurrentDesk)
	    {
	      XMapWindow(dpy, Tmp_win->w);
	      XMapWindow(dpy, Tmp_win->frame);
	      Tmp_win->flags |= MAP_PENDING;
	      SetMapStateProp(Tmp_win, NormalState);
	      if(Scr.flags & ClickToFocus)
		{
		  SetFocus(Tmp_win->w,Tmp_win,False);
		}
	    }
	  else
	    {
	      XMapWindow(dpy, Tmp_win->w);
	      SetMapStateProp(Tmp_win, NormalState);
	    }
	  break;
	  
	case IconicState:
	  Iconify(Tmp_win, 0, 0);
	  break;

	case ZoomState:
	  Shade(Tmp_win);
	  break;
	}
      XSync(dpy,0);
      XUngrabServer(dpy);
    }
  /* If no hints, or currently an icon, just "deiconify" */
  else
    {
      DeIconify(Tmp_win);
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleMapNotify - MapNotify event handler
 *
 ***********************************************************************/
void HandleMapNotify()
{
  if (!Tmp_win || (Event.xmap.event == Scr.ASRoot.w))
    {
      return;
    }

  /*
   * Need to do the grab to avoid race condition of having server send
   * MapNotify to client before the frame gets mapped; this is bad because
   * the client would think that the window has a chance of being viewable
   * when it really isn't.
   */
  XGrabServer (dpy);
  if(Tmp_win->icon_pixmap_w != None)
    XUnmapWindow(dpy, Tmp_win->icon_pixmap_w);
  XMapSubwindows(dpy, Tmp_win->frame);

  if(Tmp_win->Desk == Scr.CurrentDesk)
    {
      XMapWindow(dpy, Tmp_win->frame);
    }

  if(Tmp_win->flags & ICONIFIED)
    Broadcast(M_DEICONIFY,3,Tmp_win->w,Tmp_win->frame,
	      (unsigned long)Tmp_win,0,0,0,0);
  else
    {
      Broadcast(M_MAP,3,Tmp_win->w,Tmp_win->frame,
		(unsigned long)Tmp_win,0,0,0,0);
    }
  
  if(Scr.flags & ClickToFocus)
      SetFocus(Tmp_win->w,Tmp_win,False);

  if((!(Tmp_win->flags &(BORDER|TITLE)))&&(Tmp_win->boundary_width <2))
    {
      SetBorder(Tmp_win,False,True,True,Tmp_win->frame);
    }
  XSync(dpy,0);
  XUngrabServer (dpy);
  XFlush (dpy);
  Tmp_win->flags |= MAPPED;
  Tmp_win->flags &= ~MAP_PENDING;
  Tmp_win->flags &= ~ICONIFIED;
  Tmp_win->flags &= ~ICON_UNMAPPED;
  UpdateVisibility();
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleUnmapNotify - UnmapNotify event handler
 *
 ************************************************************************/
void HandleUnmapNotify()
{
  int dstx, dsty;
  Window dumwin;
  XEvent dummy;
  extern ASWindow *colormap_win;

  /*
   * The July 27, 1988 ICCCM spec states that a client wishing to switch
   * to WithdrawnState should send a synthetic UnmapNotify with the
   * event field set to (pseudo-)root, in case the window is already
   * unmapped (which is the case for afterstep for IconicState).  Unfortunately,
   * we looked for the ASContext using that field, so try the window
   * field also.
   */
  if (!Tmp_win)
    {
      Event.xany.window = Event.xunmap.window;
      if (XFindContext(dpy, Event.xany.window,
		       ASContext, (caddr_t *)&Tmp_win) == XCNOENT)
	Tmp_win = NULL;
    }
  
  if (Event.xunmap.event == Scr.ASRoot.w) return;

  if(!Tmp_win)
    return;

  if(Tmp_win ==  Scr.Hilite)
    Scr.Hilite = NULL;

  if(Scr.PreviousFocus == Tmp_win)
    Scr.PreviousFocus = NULL;

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

  if ((!(Tmp_win->flags & MAPPED)&&!(Tmp_win->flags&ICONIFIED)))
    {
      return;
    }
  
  XGrabServer(dpy);
  
  if(XCheckTypedWindowEvent (dpy, Event.xunmap.window, DestroyNotify,&dummy)) 
    {
      Destroy(Tmp_win);
      XUngrabServer (dpy);
      UpdateVisibility();
      return;
    } 

  /*
   * The program may have unmapped the client window, from either
   * NormalState or IconicState.  Handle the transition to WithdrawnState.
   *
   * We need to reparent the window back to the root (so that afterstep exiting 
   * won't cause it to get mapped) and then throw away all state (pretend 
   * that we've received a DestroyNotify).
   */
  if (XTranslateCoordinates (dpy, Event.xunmap.window, Scr.Root,
			     0, 0, &dstx, &dsty, &dumwin)) 
    {
      XEvent ev;
      Bool reparented;
      
      reparented = XCheckTypedWindowEvent (dpy, Event.xunmap.window, 
					   ReparentNotify, &ev);
      SetMapStateProp (Tmp_win, WithdrawnState);
      if (reparented) 
	{
	  if (Tmp_win->old_bw)
	    XSetWindowBorderWidth (dpy, Event.xunmap.window, Tmp_win->old_bw);
	  if((!(Tmp_win->flags & SUPPRESSICON))&&
	     (Tmp_win->wmhints && (Tmp_win->wmhints->flags & IconWindowHint)))
	    XUnmapWindow (dpy, Tmp_win->wmhints->icon_window);
	} 
      else
	{
	  RestoreWithdrawnLocation (Tmp_win,False);
	}
      XRemoveFromSaveSet (dpy, Event.xunmap.window);
      XSelectInput (dpy, Event.xunmap.window, NoEventMask);
      Destroy(Tmp_win);		/* do not need to mash event before */
      /*
       * Flush any pending events for the window.
       */
      /* Bzzt! it could be about to re-map */
/*      while(XCheckWindowEvent(dpy, Event.xunmap.window,
			      StructureNotifyMask | PropertyChangeMask |
			      ColormapChangeMask |
			      EnterWindowMask | LeaveWindowMask, &dummy));
      */
    } /* else window no longer exists and we'll get a destroy notify */
  XUngrabServer (dpy);
  UpdateVisibility();
  XFlush (dpy);
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleMotionNotify - MotionNotify event handler
 *
 **********************************************************************/
void HandleMotionNotify()
{
#ifndef NO_PAGER
  extern Bool EnablePagerRedraw;
  
  /* here is the code for dragging the viewport around within the pager.*/
  if((Tmp_win)&&(Tmp_win->w == Scr.Pager_w)&&
     (!(Tmp_win->flags & ICONIFIED)))
    {
      if(Event.xmotion.state == Button3MotionMask)
	{
	  EnablePagerRedraw = False;	  
	  SwitchPages(FALSE,FALSE);
	}
    }
#endif
}

/***********************************************************************
 *
 *  Procedure:
 *	HandleButtonPress - ButtonPress event handler
 *
 ***********************************************************************/
void HandleButtonPress()
{
#ifndef NO_PAGER
  extern Bool EnablePagerRedraw;
#endif
  unsigned int modifier;
  MouseButton *MouseEntry;
  char *Action;
  Window xPressedW;
  int LocalContext;
  
  Context = GetContext(Tmp_win,&Event, &PressedW);
  LocalContext = Context;
  xPressedW= PressedW;

  /* click to focus stuff goes here */
  if (Scr.flags & ClickToFocus)
    {
      if (Tmp_win && (Tmp_win != Scr.Ungrabbed)
	  && ((Event.xbutton.state & Scr.nonlock_mods) == 0))
        {
	  Bool focusAccepted = SetFocus(Tmp_win->w,Tmp_win,False);

	  if (! (Tmp_win->flags & VISIBLE))
	    {
	      if ((Scr.flags & ClickToRaise) && (Context == C_WINDOW)
		  && (Scr.RaiseButtons & (1<<Event.xbutton.button)))
		 RaiseWindow(Tmp_win);
	      else
	        {
		  if(Scr.AutoRaiseDelay > 0)
		    {
		      SetTimer(Scr.AutoRaiseDelay);
		    }
		  else 
		    {
#ifdef CLICKY_MODE_1
		      if((Event.xany.window != Tmp_win->w)&&
			 (Event.xbutton.subwindow != Tmp_win->w)&&
			 (Event.xany.window != Tmp_win->Parent)&&
			 (Event.xbutton.subwindow != Tmp_win->Parent))
#endif
		        {
			  if(Scr.AutoRaiseDelay == 0)
			      RaiseWindow(Tmp_win);
			}
		    }
		}
	    }
	  if(!(Tmp_win->flags & ICONIFIED) && focusAccepted)
	    {
	      XSync(dpy,0);
	      if ((Scr.flags & EatFocusClick) && (Context == C_WINDOW))
		  XAllowEvents(dpy, AsyncPointer, CurrentTime);
	      else
		  XAllowEvents(dpy,ReplayPointer,CurrentTime);

	      XSync(dpy,0);
	      return;
	    }
	}
    }
  else if((Scr.flags & ClickToRaise) && Tmp_win && (Context == C_WINDOW)
          && (Scr.RaiseButtons & (1<<Event.xbutton.button))
          && ((Event.xbutton.state & Scr.nonlock_mods) == 0)
          && ((Tmp_win->flags & VISIBLE) == 0))
   {
         RaiseWindow(Tmp_win);
         if(!(Tmp_win->flags & ICONIFIED))
           {
             XSync(dpy,0);
             XAllowEvents(dpy,AsyncPointer,CurrentTime);
             XSync(dpy,0);
             return;
           }
   }
  XSync(dpy,0);
  XAllowEvents(dpy, (Context == C_WINDOW) ? ReplayPointer : AsyncPointer,
	       CurrentTime);
  XSync(dpy,0);
  
  /* here is the code for moving windows in the pager.*/
#ifndef NO_PAGER
  if((Tmp_win)&&(Tmp_win->w == Event.xbutton.window)&&
     (Tmp_win->w == Scr.Pager_w)&&(Event.xbutton.button == Button2))
    {
      PagerMoveWindow();
      return;
    }
  if((Tmp_win)&&(Tmp_win->w == Scr.Pager_w)&&
     (!(Tmp_win->flags & ICONIFIED))&&
     (Event.xbutton.button == Button3)&&
     (Event.xany.window == Scr.Pager_w))
    {
#ifndef NO_PAGER
      EnablePagerRedraw = False;	  
#endif
      SwitchPages(FALSE,FALSE);
    }
#endif

  if(Context == C_TITLE)
    SetTitleBar(Tmp_win,(Scr.Hilite == Tmp_win),False);
  else
    SetBorder(Tmp_win,(Scr.Hilite == Tmp_win),True,True,PressedW);	

  ButtonWindow = Tmp_win;
  
  /* we have to execute a function or pop up a menu
   */
  
  modifier = (Event.xbutton.state & Scr.nonlock_mods);
  /* need to search for an appropriate mouse binding */
  MouseEntry = Scr.MouseButtonRoot;
  while(MouseEntry != (MouseButton *)0)
    {
      if(((MouseEntry->Button == Event.xbutton.button)||(MouseEntry->Button == 0))&&
	 (MouseEntry->Context & Context)&&
	 ((MouseEntry->Modifier == AnyModifier)||
	  (MouseEntry->Modifier == modifier)))
	{
	  /* got a match, now process it */
	  if (MouseEntry->func != (int)NULL)
 	    {
	      Action = MouseEntry->item ? MouseEntry->item->action : NULL;
	      ExecuteFunction(MouseEntry->func, Action, Event.xany.window, 
			      Tmp_win, &Event, Context,MouseEntry->val1,
			      MouseEntry->val2,
			      MouseEntry->val1_unit,MouseEntry->val2_unit,
			      MouseEntry->menu,-1);
	      break;
	    }
	}
      MouseEntry = MouseEntry->NextButton;
    }
  PressedW = None;

  if(LocalContext!=C_TITLE)
    SetBorder(ButtonWindow,(Scr.Hilite == ButtonWindow),True,True,xPressedW);
  else
    SetTitleBar(ButtonWindow,(Scr.Hilite==ButtonWindow),False);

  ButtonWindow = NULL;
}

/***********************************************************************
 *
 *  Procedure:
 *	HandleButtonRelease - ButtonRelease event handler
 *
 ***********************************************************************/
void HandleButtonRelease()
{
#ifndef NO_PAGER
  extern Bool EnablePagerRedraw;
  if((Tmp_win)&&(Event.xany.window == Scr.Pager_w)&&
     (!(Tmp_win->flags & ICONIFIED)))
    {
      switch(Event.xbutton.button)
	{
	default:
	case Button1:
	  SwitchPages(TRUE,TRUE);
	  break;
	case Button3:
	  EnablePagerRedraw = True;
	  SwitchPages(FALSE,FALSE);
	  RedrawPager();
	  break;
	}
    }
#endif
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleEnterNotify - EnterNotify event handler
 *
 ************************************************************************/
void HandleEnterNotify()
{
  XEnterWindowEvent *ewp = &Event.xcrossing;
  XEvent d;
  
  /* look for a matching leaveNotify which would nullify this enterNotify */
  if(XCheckTypedWindowEvent (dpy, ewp->window, LeaveNotify, &d))
    {
      StashEventTime(&d);
      if((d.xcrossing.mode==NotifyNormal)&&
	 (d.xcrossing.detail!=NotifyInferior))
	return;
    }

/* an EnterEvent in one of the PanFrameWindows activates the Paging */
#ifndef NON_VIRTUAL
  if (ewp->window==Scr.PanFrameTop.win 
      || ewp->window==Scr.PanFrameLeft.win
      || ewp->window==Scr.PanFrameRight.win 
      || ewp->window==Scr.PanFrameBottom.win ) 
    {
      int delta_x=0, delta_y=0;
      /* this was in the HandleMotionNotify before, HEDU */
      HandlePaging(Scr.EdgeScrollX,Scr.EdgeScrollY,
                   &Event.xcrossing.x_root,&Event.xcrossing.y_root,
                   &delta_x,&delta_y,True);
      return;
    }
#endif /* NON_VIRTUAL */
 
  if(Event.xany.window == Scr.Root)
    {
      if((!(Scr.flags & ClickToFocus))&&(!(Scr.flags & SloppyFocus)))
	{
	  SetFocus(Scr.NoFocusWin,NULL,False);
	}
      InstallWindowColormaps(NULL);
      return;
    }
  
  /* make sure its for one of our windows */
  if (!Tmp_win) 
    return;
  
if (Tmp_win->focus_var == 0)
{
  if(!(Scr.flags & ClickToFocus))
    {
      if(Scr.Focus != Tmp_win)
	{
	  if((Scr.AutoRaiseDelay > 0)&&(!(Tmp_win->flags & VISIBLE)))
	    SetTimer(Scr.AutoRaiseDelay);
	  SetFocus(Tmp_win->w,Tmp_win,False);
	}
      else
	SetFocus(Tmp_win->w,Tmp_win, True);  /* don't affect the circ.seq. */
    }
  if((!(Tmp_win->flags & ICONIFIED))&&(Event.xany.window == Tmp_win->w))
    InstallWindowColormaps(Tmp_win);
  else
    InstallWindowColormaps(NULL);
}
  return;
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleLeaveNotify - LeaveNotify event handler
 *
 ************************************************************************/
void HandleLeaveNotify()
{
  /* If we leave the root window, then we're really moving
   * another screen on a multiple screen display, and we
   * need to de-focus and unhighlight to make sure that we
   * don't end up with more than one highlighted window at a time */
  if(Event.xcrossing.window == Scr.Root)
    {
      if(Event.xcrossing.mode == NotifyNormal)
	{
	  if (Event.xcrossing.detail != NotifyInferior) 
	    {
	      if(Scr.Focus != NULL)
		{
		  SetFocus(Scr.NoFocusWin,NULL,False);
		}
	      if(Scr.Hilite != NULL)
		SetBorder(Scr.Hilite,False,True,True,None);
	    }
	}
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	HandleConfigureRequest - ConfigureRequest event handler
 *
 ************************************************************************/
void HandleConfigureRequest()
{
  XWindowChanges xwc;
  unsigned long xwcm;
  int x, y, width, height;
  XConfigureRequestEvent *cre = &Event.xconfigurerequest;
  
  /*
   * Event.xany.window is Event.xconfigurerequest.parent, so Tmp_win will
   * be wrong
   */
  Event.xany.window = cre->window;	/* mash parent field */
  if (XFindContext (dpy, cre->window, ASContext, (caddr_t *) &Tmp_win) ==
      XCNOENT)
    Tmp_win = NULL;
  
  /*
   * According to the July 27, 1988 ICCCM draft, we should ignore size and
   * position fields in the WM_NORMAL_HINTS property when we map a window.
   * Instead, we'll read the current geometry.  Therefore, we should respond
   * to configuration requests for windows which have never been mapped.
   */
  if (!Tmp_win )
    {
      xwcm = cre->value_mask & 
	(CWX | CWY | CWWidth | CWHeight | CWBorderWidth);
      xwc.x = cre->x;
      xwc.y = cre->y;
      xwc.width = cre->width;
      xwc.height = cre->height;
      xwc.border_width = cre->border_width;
      XConfigureWindow(dpy, Event.xany.window, xwcm, &xwc);
      
      if(Tmp_win)
	{
	  xwc.x = Tmp_win->icon_x_loc;
	  xwc.y = Tmp_win->icon_y_loc - Tmp_win->icon_p_height;
	  xwcm = cre->value_mask & (CWX | CWY);
	  if(Tmp_win->icon_pixmap_w != None)
	    XConfigureWindow(dpy, Tmp_win->icon_pixmap_w, xwcm, &xwc);
	  xwc.x = Tmp_win->icon_x_loc;
	  xwc.y = Tmp_win->icon_y_loc;
	  xwcm = cre->value_mask & (CWX | CWY);
	}
      return;
    }
  
  if (cre->value_mask & CWStackMode) 
    {
      ASWindow *otherwin;
      
      xwc.sibling = (((cre->value_mask & CWSibling) &&
		      (XFindContext (dpy, cre->above, ASContext,
				     (caddr_t *) &otherwin) == XCSUCCESS))
		     ? otherwin->frame : cre->above);
      xwc.stack_mode = cre->detail;
      XConfigureWindow (dpy, Tmp_win->frame,
			cre->value_mask & (CWSibling | CWStackMode), &xwc);
      XSync(dpy, False);
      CorrectStackOrder();
    }
  
#ifdef SHAPE
  {
    int xws, yws, xbs, ybs;
    unsigned wws, hws, wbs, hbs;
    int boundingShaped, clipShaped;
    
    XShapeQueryExtents (dpy, Tmp_win->w,&boundingShaped, &xws, &yws, &wws,
			&hws,&clipShaped, &xbs, &ybs, &wbs, &hbs);
    Tmp_win->wShaped = boundingShaped;
  }
#endif /* SHAPE */
  
  /* Don't modify frame_XXX fields before calling SetupWindow! */
  x = Tmp_win->frame_x;
  y = Tmp_win->frame_y;
  width = Tmp_win->frame_width;
  height = Tmp_win->frame_height;
  
  /* for restoring */  
  if (cre->value_mask & CWBorderWidth) 
    {
      Tmp_win->old_bw = cre->border_width; 
    }
  /* override even if border change */
  
  if (cre->value_mask & CWX)
    x = cre->x - Tmp_win->boundary_width - Tmp_win->bw;
  if (cre->value_mask & CWY) 
    y = cre->y - Tmp_win->title_height - Tmp_win->bw;
  if (cre->value_mask & CWWidth)
    width = cre->width + 2*Tmp_win->boundary_width;
  if (cre->value_mask & CWHeight) 
    height = cre->height+Tmp_win->title_height+Tmp_win->boundary_height;
  
  /*
   * SetupWindow (x,y) are the location of the upper-left outer corner and
   * are passed directly to XMoveResizeWindow (frame).  The (width,height)
   * are the inner size of the frame.  The inner width is the same as the 
   * requested client window width; the inner height is the same as the
   * requested client window height plus any title bar slop.
   */
  SetupFrame (Tmp_win, x, y, width, height,FALSE);
  UpdateVisibility();
  
#ifndef NO_PAGER
  RedrawPager();
#endif
}

/***********************************************************************
 *
 *  Procedure:
 *      HandleShapeNotify - shape notification event handler
 *
 ***********************************************************************/
#ifdef SHAPE
void HandleShapeNotify (void)
{
  XShapeEvent *sev = (XShapeEvent *) &Event;
  
  if (!Tmp_win)
    return;
  if (sev->kind != ShapeBounding)
    return;
  Tmp_win->wShaped = sev->shaped;
  SetShape(Tmp_win,Tmp_win->frame_width);
}
#endif  /* SHAPE*/

/**************************************************************************
 * 
 * For auto-raising windows, this routine is called
 *
 *************************************************************************/
volatile int alarmed;
void enterAlarm(int nonsense)
{
  alarmed = True;
  signal(SIGALRM, enterAlarm);
}


/***************************************************************************
 *
 * Waits for next X event, or for an auto-raise timeout.
 *
 ****************************************************************************/
int My_XNextEvent(Display *dpy, XEvent *event)
{
  extern int fd_width, x_fd;
  struct itimerval value;
  fd_set in_fdset,out_fdset;
  Window child;
  Window targetWindow;
  int i,count;
  int retval;

  /* Do this prior to the select() call, in case the timer already expired,
   * in which case the select would never return. */
  if(alarmed)
    {
      alarmed = False;
      XQueryPointer(dpy,Scr.Root,&JunkRoot,&child,&JunkX,&JunkY,&JunkX, 
		    &JunkY,&JunkMask );
      if((Scr.Focus != NULL)&&(child == Scr.Focus->frame))
	{
	  if(!(Scr.Focus->flags & VISIBLE))
	    {
	      RaiseWindow(Scr.Focus);
#ifndef NO_PAGER
	      RedrawPager();
#endif
	    }
	}
      return 0;
    }
  
#ifndef HAVE_GETITIMER
  value.it_value.tv_usec = 0;
  value.it_value.tv_sec = 0;
#else
  getitimer(ITIMER_REAL,&value);
#endif
  
  FD_ZERO(&in_fdset);
  FD_SET(x_fd,&in_fdset);
  FD_ZERO(&out_fdset);
  for(i=0; i<npipes; i++)
    {
      if(readPipes[i]>=0)
	{
	  FD_SET(readPipes[i], &in_fdset);
	}
    }

  for(i=0; i<npipes; i++)
    {
      if(pipeQueue[i]!= NULL)
	{
	  FD_SET(writePipes[i], &out_fdset);
	}
    }
  

  /* Do this IMMEDIATELY prior to select, to prevent any nasty
   * queued up X events from just hanging around waiting to be
   * flushed */
  XFlush(dpy);
  if(XPending(dpy))
    {
      XNextEvent(dpy,event);
      StashEventTime(event);
      return 1;
    }

  /* Zap all those zombies! */
  /* If we get to here, then there are no X events waiting to be processed.
   * Just take a moment to check for dead children. */
  ReapChildren();

  XFlush(dpy);
  if((value.it_value.tv_usec != 0)||
     (value.it_value.tv_sec != 0))
#ifdef __hpux
    retval=select(fd_width,(int *)&in_fdset, 0, 0, &value.it_value);
#else
    retval=select(fd_width,&in_fdset, 0, 0, &value.it_value);
#endif
  else
#ifdef __hpux
    retval=select(fd_width,(int *)&in_fdset, (int *)&out_fdset,
		  0, NULL);
#else
    retval=select(fd_width,&in_fdset, &out_fdset, 0, NULL);
#endif
  
  /* Check for module input. */
  for(i=0;i<npipes;i++)
    {
      if(readPipes[i] >= 0)
	{
	  if((retval>0)&&(FD_ISSET(readPipes[i], &in_fdset)))
	    {
	      if((count = 
		  read(readPipes[i],&targetWindow, sizeof(Window))) >0)
		{
		  HandleModuleInput(targetWindow,i);
		}
	      if(count <= 0)
		{
		  KillModule(i,10);
		}
	    }
	}
      if(writePipes[i] >= 0)
	{
	  if((retval>0)&&(FD_ISSET(writePipes[i], &out_fdset)))
	    {
	      FlushQueue(i);
	    }
	}
    }
  return 0;
}

	/* Change the icon, bugs for that to wiz@iol.it */
void ChangeIcon()
{
	char *cp;
	if (!(Tmp_win->flags & SUPPRESSICON) && (Tmp_win->flags & ICON_OURS)) {
		SearchIcon(Tmp_win,&cp);
		if(cp && !( Tmp_win->icon_bitmap_file && cp == Tmp_win->icon_bitmap_file ))
			Tmp_win->icon_bitmap_file=cp;
		else
			return;
	}
	GetIcon(Tmp_win);
	DrawIconWindow(Tmp_win);
}

void SearchIcon(ASWindow *tmp_win, char**value)
{
	name_list *nptr=Scr.TheList;
	*value = NULL;
		/* Start from the last element, because we are looking for
		   an icon name and it is the last match that should win,
		   I think better to start from the end of the list...;)
		   */

	if(!nptr)
		return;
	nptr=nptr->prev;
	do{
		if(nptr->value){
			if (matchWildcards(nptr->name,tmp_win->icon_name) == TRUE){
				*value = nptr->value;			
				return;
			}
			if (matchWildcards(nptr->name,tmp_win->name) == TRUE){
				*value = nptr->value;			
				return;
			}
			if (matchWildcards(nptr->name,tmp_win->class.res_name) == TRUE ){
				*value = nptr->value;			
				return;
			}
			if (matchWildcards(nptr->name,tmp_win->class.res_class) == TRUE){
				*value = nptr->value;			
				return;
			}
				
		}
		nptr=nptr->prev;
	} while(nptr != Scr.TheList);
		
}
