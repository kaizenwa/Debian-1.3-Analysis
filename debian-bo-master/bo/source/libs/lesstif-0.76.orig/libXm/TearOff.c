/**
 *
 * $Id: TearOff.c,v 1.4 1996/11/28 09:22:04 u27113 Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
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

static char rcsid[] = "$Id: TearOff.c,v 1.4 1996/11/28 09:22:04 u27113 Exp $";

#include <stdio.h>

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/AtomMgr.h>
#include <Xm/MwmUtil.h>
#include <Xm/Protocols.h>
#include <Xm/TearOffP.h>
#include <Xm/TearOffBP.h>
#include <Xm/RowColumnP.h>
#include <Xm/MenuShellP.h>
#include <Xm/PushB.h>

#include <XmI/DebugUtil.h>

#define TEAR_OFF_SHELL_NAME " Tear-off"

XmExcludedParentPaneRec _XmExcludedParentPane = { 0 };

void 
_XmTearOffBtnDownEventHandler(Widget reportingWidget,
			      XtPointer data,
			      XEvent *event,
			      Boolean *cont)
{
    XdbDebug(__FILE__, reportingWidget, "_XmTearOffBtnDownEventHandler()\n");
}

void 
_XmTearOffBtnUpEventHandler(Widget reportingWidget,
			    XtPointer data,
			    XEvent *event,
			    Boolean *cont)
{
    XdbDebug(__FILE__, reportingWidget, "_XmTearOffBtnUpEventHandler()\n");
}

void 
_XmDestroyTearOffShell(Widget w)
{
    XdbDebug(__FILE__, w, "_XmDestroyTearOffShell()\n");
}
   
void 
_XmDismissTearOff(Widget shell,
		  XtPointer closure,
		  XtPointer call_data)
{
    Widget		menu;
    ShellWidget	sw = (ShellWidget) shell;
  
    XdbDebug(__FILE__, shell, "_XmDismissTearOff()\n");
  
    menu = sw->composite.children[1];	/* FIX ME - 0 is the vendor ext */
    _XmRestoreTearOffToMenuShell(menu, NULL);

    XtPopdown(shell);
}

void 
_XmTearOffInitiate(Widget w,
		   XEvent *event)
{
	Widget	menu;

	XdbDebug(__FILE__, w, "_XmTearOffInitiate()\n");

	menu = XtParent(w);

	if (! XmIsRowColumn(menu)) {
	  XdbDebug2(__FILE__, menu, w, "Parent is not a menu\n");
	  return;
	}

	if (RC_ParentShell(menu))
	  RC_SetFromInit(menu, 0);
	else
	  RC_SetFromInit(menu, 1);

	_XmRestoreTearOffToToplevelShell(menu, event);

/*
 * The call below seems to be necessary, I must confess I don't know why...
 * Maybe Xt doesn't want us to reparent widgets after all :-)
 */
/*	XMapWindow(XtDisplay(menu), XtWindow(menu));*/
}

void 
_XmAddTearOffEventHandlers(Widget w)
{
    XdbDebug(__FILE__, w, "_XmAddTearOffEventHandlers()\n");
}

Boolean 
_XmIsTearOffShellDescendant(Widget w)
{
    XdbDebug(__FILE__, w, "_XmIsTearOffShellDescendant()\n");

    return False;
}

void 
_XmLowerTearOffObscuringPoppingDownPanes(Widget ancestor,
					 Widget tearOff)
{
    XdbDebug2(__FILE__, ancestor, tearOff, "_XmLowerTearOffObscuringPoppingDownPanes()\n");
}

void
_XmRestoreExcludedTearOffToToplevelShell(Widget w,
					 XEvent *event)
{
  XdbDebug(__FILE__, w, "_XmRestoreExcludedTearOffToToplevelShell()\n");
}

void 
_XmRestoreTearOffToToplevelShell(Widget w,
				 XEvent *event)
{

  /* w represents the row column */
  Widget parent_shell = RC_ParentShell(w);
  Widget menu_shell = XtParent(w);
  int i;

  XdbDebug(__FILE__, w, "_XmRestoreTearOffToToplevelShell()\n");

  /* Tear off buttons should not appear in torn off shells */
  if (RC_TearOffControl(w))
  {
    XtDestroyWidget(RC_TearOffControl(w));
    RC_TearOffControl(w) = NULL;
  }

  /* if we already torn off */
  if (RC_TornOff(w))
    return;

  /* should we create the shell? */
  if (RC_FromInit(w))
  {
    Widget shell;
    int	nargs;
    Arg	args[8];
    char *to_shell_name;
    Atom delete_atom;

    /* create the tear off shell's name */
    to_shell_name = XtMalloc(strlen(TEAR_OFF_SHELL_NAME) 
			     + (RC_CascadeBtn(w) 
				? strlen(XtName(RC_CascadeBtn(w))) + 2 
				: 2));

    sprintf (to_shell_name, "%s%s", 
	     RC_CascadeBtn(w) ? XtName(RC_CascadeBtn(w)) : "", 
	     TEAR_OFF_SHELL_NAME);

    /* find the toplevel shell in this heirarchy and make 
       that the parent of the transient shell */
    for (shell=w;
	 !XtIsSubclass(shell, vendorShellWidgetClass) && XtParent(shell);
	 shell = XtParent(shell))
      ;

    nargs = 0;
    XtSetArg(args[nargs], XmNdeleteResponse, XmDO_NOTHING); nargs++;
    XtSetArg(args[nargs], XmNallowShellResize, True); nargs++;
    XtSetArg(args[nargs], XmNtransientFor, shell); nargs++;
    XtSetArg(args[nargs], XmNtitle, to_shell_name); nargs++;
    
    XtSetArg(args[nargs], XmNmwmFunctions, 
	     MWM_FUNC_ALL & ~(MWM_FUNC_RESIZE | MWM_FUNC_MINIMIZE | MWM_FUNC_MAXIMIZE)); nargs++;
    XtSetArg(args[nargs], XmNmwmDecorations, MWM_DECOR_BORDER | MWM_DECOR_MENU | MWM_DECOR_TITLE); nargs++; 

    parent_shell = XtCreatePopupShell("", transientShellWidgetClass,
				      shell, args, nargs);

    RC_ParentShell(w) = parent_shell;

    XtFree(to_shell_name);

    /*
     * Get a popdown handler too
     */
    delete_atom = XmInternAtom(XtDisplay(parent_shell), "WM_DELETE_WINDOW", FALSE);
    XmAddWMProtocolCallback(parent_shell, delete_atom, _XmDismissTearOff, NULL);
  }

  /* Cleanup Menu stuff */
  if (RC_CascadeBtn(w))
    XtCallActionProc(RC_CascadeBtn(w), 
		     "CleanupMenuBar", event, NULL, 0);
  

  /* Make the buttons behave like outside a menu system */
  for (i=0; i<MGR_NumChildren(w); i++)
    if (XtIsSubclass(MGR_Children(w)[i], xmPushButtonWidgetClass)) {
      XdbDebug2(__FILE__, w, MGR_Children(w)[i],
		"_XmTearOffInitiate: set normal translations on button\n");
#if 0 /* gotta exist, first */
      _XmPushButtonSetTranslation(MGR_Children(w)[i], 0);
#endif
    }

  /* This is a simple case : works only if one level of menu .. FIX ME */
  XtUngrabPointer(w, CurrentTime);
  XtUngrabKeyboard(w, CurrentTime);

  XtUnmanageChild(w);

  /* set our parent to the created transient shell */
  XtParent(w) = RC_ParentShell(w);

  (*((TransientShellWidgetClass)transientShellWidgetClass)
       ->composite_class.insert_child)(w);

  XtManageChild(w);

  _XmMoveObject(parent_shell, event->xbutton.x_root, event->xbutton.y_root);
    
  (*((TransientShellWidgetClass)transientShellWidgetClass)
       ->composite_class.change_managed)(parent_shell);

  XtRealizeWidget(parent_shell);
    
  /* reparent the window */
#if 0
  if (XtIsRealized(parent_shell) & XtIsRealized(w))
    XReparentWindow(XtDisplay(parent_shell), 
		    XtWindow(w), 
		    XtWindow(parent_shell), 
		    0, 0);      
#endif

  RC_SetTornOff(w, 1);
  RC_SetFromInit(w, 0);
  RC_ParentShell(w) = menu_shell; /* save off the menu shell */
  XtPopup(parent_shell, XtGrabNone);
}

void 
_XmRestoreTearOffToMenuShell(Widget w,
			     XEvent *event)
{
    /* w represents the row column */
    Widget parent_shell = RC_ParentShell(w);
    Widget transient = XtParent(w);

    XdbDebug(__FILE__, w, "_XmRestoreTearOffToMenuShell()\n");

    /* Tear off buttons should appear in menu shells */
    if (!RC_TearOffControl(w)) 
    {
      XdbDebug(__FILE__, w, "_XmRestoreTearOffToMenuShell creating tear off control\n");
      RC_TearOffControl(w) = XtVaCreateManagedWidget("TearOffControl",
						     xmTearOffButtonWidgetClass,
						     w,
						     XmNpositionIndex, 0,
						     NULL);
    }

    /* if we're not really in a torn off menu. */
    if (!RC_TornOff(w))
      return;

    /* set our parent back to the menu shell */
    XtParent(w) = RC_ParentShell(w);

    (*((XmMenuShellWidgetClass)xmMenuShellWidgetClass)
         ->composite_class.insert_child)(w);
    
    XtManageChild(w);
    
    /* reparent the window */
#if 0
    if (XtIsRealized(parent_shell) & XtIsRealized(w))
	XReparentWindow(XtDisplay(parent_shell), 
			XtWindow(w), 
			XtWindow(parent_shell), 
			0, 0);      
#endif

    RC_SetTornOff(w, 0);
    RC_ParentShell(w) = transient; /* save off the transient */

}
