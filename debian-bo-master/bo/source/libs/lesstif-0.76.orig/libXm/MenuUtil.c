/**
 *
 * $Id: MenuUtil.c,v 1.7 1996/11/28 09:21:33 u27113 Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static char rcsid[] = "$Id: MenuUtil.c,v 1.7 1996/11/28 09:21:33 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/RowColumnP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/Screen.h>
#include <Xm/ScreenP.h>
#include <Xm/XmosP.h>

#include <XmI/DebugUtil.h>

Boolean
_XmGetInPMMode(Widget w)
{
    XmMenuState state = _XmGetMenuState(w);

    return state->MU_InPMMode;
}

void
_XmSetInPMMode(Widget w, Boolean flag)
{
    XmMenuState state = _XmGetMenuState(w);

    state->MU_InPMMode = flag;
}

Boolean 
_XmGetInDragMode(Widget w)
{
    XmMenuState state = _XmGetMenuState(w);

    return state->MU_InDragMode;
}

void
_XmSetInDragMode(Widget w, Boolean flag)
{
    XmMenuState state = _XmGetMenuState(w);

    state->MU_InDragMode = flag;
}

extern Widget 
_XmGetRC_PopupPosted(Widget rc)
{
    if (!XmIsRowColumn(rc))
	return NULL;

    return RC_PopupPosted(rc);
}

void
_XmRecordEvent(XEvent *event)
{
}

Boolean
_XmIsEventUnique(XEvent *event)
{
    return False; /* FIX ME */
}

/*
 * A wrapper around the Xt Intrinsic's pointer grabbing. This
 * one retries a failing grab for several times and if all fails
 * spits out a warning message.
 */
int 
_XmGrabPointer(Widget w,
	       int owner_events,
	       unsigned int event_mask,
	       int pointer_mode,
	       int keyboard_mode,
	       Window confine_to,
	       Cursor cursor,
	       Time time)
{
    int result, retries;

    XdbDebug(__FILE__, w, "_XmGrabPointer()\n");

    for ( retries = 4; retries >= 0; retries-- ) {
        result = XtGrabPointer(XmIsGadget(w) ? XtParent(w) : w,
			       owner_events, event_mask,
			       pointer_mode, keyboard_mode,
			       confine_to, cursor, time);
	if ( result == GrabSuccess )
	    return result;

	XdbDebug(__FILE__, w, "_XmGrabPointer => %s, trying again\n",
		(result == AlreadyGrabbed) ? "AlreadyGrabbed" :
		(result == GrabInvalidTime) ? "GrabInvalidTime" :
		(result == GrabNotViewable) ? "GrabNotViewable" :
		(result == GrabFrozen) ? "GrabFrozen" : "??" );

	if ( retries ) _XmMicroSleep(1);
    }
    _XmWarning(w, "Can't grab the pointer.");
    return result;
}

void
_XmUngrabPointer(Widget w, Time t)
{
  XdbDebug(__FILE__, w, "_XmUngrabPointer\n");
  
  XtUngrabPointer(XmIsGadget(w) ? XtParent(w) : w, t);
}

/*
 * Same as the _XmGrabPointer wrapper above, but this time for grabbing
 * the keyboard.
 */
int
_XmGrabKeyboard(Widget widget,
		int owner_events,
		int pointer_mode,
		int keyboard_mode,
		Time time)
{
   int result, retries;

    XdbDebug(__FILE__, widget, "_XmGrabKeyboard()\n");
    
    for ( retries = 4; retries >= 0; retries-- ) {
        result = XtGrabKeyboard(XmIsGadget(widget) ? XtParent(widget) : widget,
			        owner_events,
			        pointer_mode, keyboard_mode,
			        time);
	if ( result == GrabSuccess )
	    return result;

	XdbDebug(__FILE__, widget, "_XmGrabKeyboard : trying again\n");

	if ( retries ) _XmMicroSleep(1);
    }
    _XmWarning(widget, "Can't grab the keyboard.");
    return result;
}

void
_XmUngrabKeyboard(Widget w, Time t)
{
  XdbDebug(__FILE__, w, "_XmUngrabKeyboard\n");
  
  XtUngrabKeyboard(XmIsGadget(w) ? XtParent(w) : w, t);
}

void 
_XmMenuEscape(Widget w, 
	      XEvent *ev, 
	      String *params, 
	      Cardinal *num_params)
{
  Widget	cb;
  XmRowColumnWidget	rc;
  extern void CleanupMenuBar(Widget w, XEvent *event, String *params, Cardinal *num_params);
  
  if (XmIsRowColumn(w))
    rc = (XmRowColumnWidget)w;
  else
    rc = (XmRowColumnWidget) XtParent(w);
  
/* First guess */
  cb = RC_MemWidget(rc);

/* Second guess */
  if (! cb) {
      Widget  p = RC_PopupPosted(rc); /* Pane */
      if (p)
              cb = RC_CascadeBtn(p);
      else
              cb = RC_CascadeBtn(rc);
  }
  
  if (cb == 0) {
    _XmError(w, "_XmMenuEscape() cannot locate cascade button\n");
  }

  XdbDebug2(__FILE__, w, cb, "_XmMenuEscape()\n");
  /* Cannot use this as an action; needs to work with gadgets also */
  /* XtCallActionProc(cb, "CleanupMenuBar", ev, params, *num_params); */
  CleanupMenuBar(cb, ev, params, num_params);
}

void 
_XmMenuTraverseLeft(Widget w, 
		    XEvent *ev, 
		    String *params, 
		    Cardinal *num_params)
{
  XdbDebug(__FILE__, w, "_XmMenuTraverseLeft()\n");
}

void 
_XmMenuTraverseRight(Widget w, 
		     XEvent *ev, 
		     String *params, 
		     Cardinal *num_params)
{
  XdbDebug(__FILE__, w, "_XmMenuTraverseTight()\n");
}

void 
_XmMenuTraverseUp(Widget w, 
		  XEvent *ev, 
		  String *params, 
		  Cardinal *num_params)
{
  XdbDebug(__FILE__, w, "_XmMenuTraverseUp()\n");
}

void 
_XmMenuTraverseDown(Widget w, 
		    XEvent *ev, 
		    String *params, 
		    Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmMenuTraverseDown()\n");
}

void 
_XmRC_GadgetTraverseDown(Widget w, 
			 XEvent *ev, 
			 String *params, 
			 Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmRC_GadgetTraverseRight()\n");
}

void 
_XmRC_GadgetTraverseUp(Widget w, 
		       XEvent *ev, 
		       String *params, 
		       Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmRC_GadgetTraverseUp()\n");
}

void 
_XmRC_GadgetTraverseLeft(Widget w, 
			 XEvent *ev, 
			 String *params, 
			 Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmRC_GadgetTraverseLeft()\n");
}

void 
_XmRC_GadgetTraverseRight(Widget w, 
			  XEvent *ev, 
			  String *params, 
			  Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmRC_GadgetTraverseRight()\n");
}

/*
 * I don't know, but I'm willing to bet on XContexts...
 */
XtPointer
_XmGetMenuProcContext(void)
{
    return NULL;
}

int num_saved = 0;
static String saved_trans[32];

void
_XmSaveCoreClassTranslations(Widget widget)
{
    saved_trans[num_saved] = widget->core.widget_class->core_class.tm_table;
    num_saved++;
}

void
_XmRestoreCoreClassTranslations(Widget widget)
{
    num_saved--;
    widget->core.widget_class->core_class.tm_table = saved_trans[num_saved];
}

void
XmSetMenuCursor(Display *display, Cursor cursorId)
{
    XmScreen scr = (XmScreen)XmGetXmScreen(DefaultScreenOfDisplay(display));
    Screen_MenuCursor(scr) = cursorId;
}

Cursor
XmGetMenuCursor(Display *display)
{
	return _XmGetMenuCursorByScreen(DefaultScreenOfDisplay(display));
}

XmMenuState 
_XmGetMenuState(Widget widget)
{
  XmScreenInfo *info = _XmGetScreenInfo(XmGetXmScreen(XtScreen(widget)));
  
  return (XmMenuState)info->menu_state;
}

void
_XmFakeExpose(Widget menu_shell)
{
  /* we use the menu shell since it's guaranteed to enclose
     the the row column, as well as all its children. */

  /* this code assumes that the menu system is no more
     complex than a row column with a number of primitive
     widgets and gadgets.  Therefore, we don't recurse
     down the tree -- we just call the row column's
     childrens' expose methods after call the row column's */

  int i;
  Widget rc = MGR_Children(menu_shell)[0];

  /* here we assume that the row column child is managed. */
  (*XtClass(rc)->core_class.expose)(rc, NULL, NULL);

  for (i = 0 ; i < MGR_NumChildren(rc) ; i ++)
  {
      Widget kid = MGR_Children(rc)[i];

      if (!XtIsManaged(kid))
	continue;

      (*XtClass(kid)->core_class.expose)(kid, NULL, NULL);
  }
}
