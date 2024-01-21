/**
 *
 * $Id: CascadeB.c,v 1.16 1996/12/18 21:45:13 helden Exp $
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

static char rcsid[] = "$Id: CascadeB.c,v 1.16 1996/12/18 21:45:13 helden Exp $";

#define	DO_PIXMAP
#define	DO_SANITY
#define RESTORE_TEAR_OFFS

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/RowColumnP.h>
#include <Xm/TearOffP.h>
#include <Xm/TransltnsP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/ScreenP.h>

#include <XmI/DebugUtil.h>

/* FIX ME:
 *   - XtManageChild(submenu)
 *   - popup/popdown
 */

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes);
static void expose(Widget w, XEvent *event, Region region);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
void CleanupMenuBar(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void _XmCBRecomputeSize(Widget w);


/*
 * Resources for the cascade button class
 */
#define Offset(field) XtOffsetOf(XmCascadeButtonRec, cascade_button.field)
static XtResource resources[] = {
    {
	XmNactivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(activate_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNcascadingCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(cascade_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNsubMenuId, XmCMenuWidget, XmRMenuWidget,
	sizeof(Widget), Offset(submenu),
	XmRMenuWidget, (XtPointer)NULL
    },
    {
	XmNcascadePixmap, XmCPixmap, XmRPrimForegroundPixmap,
	sizeof(Pixmap), Offset(cascade_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNmappingDelay, XmCMappingDelay, XmRInt,
	sizeof(int), Offset(map_delay),
	XmRImmediate, (XtPointer)180
    },
    /* resources we override from XmLabel/XmPrimitive */
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmCascadeButtonRec, primitive.shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmCascadeButtonRec, primitive.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmCascadeButtonRec, primitive.highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmCascadeButtonRec, label.margin_width),
	XmRImmediate, (XtPointer)0x0000FFFF	/* FIX ME */
    }
};

#define STATIC_ACTION(fn) static void (fn)(Widget, XEvent*, String*, Cardinal*)
#define ACTION(fn) void (fn)(Widget, XEvent*, String*, Cardinal*)

STATIC_ACTION(StartDrag);
STATIC_ACTION(Help);
STATIC_ACTION(KeySelect);
STATIC_ACTION(_XmCBMenuBarSelect);
STATIC_ACTION(_XmCBMenuBarDoSelect);
STATIC_ACTION(MenuBarEnter);
STATIC_ACTION(EnterWindow);
STATIC_ACTION(LeaveWindow);
STATIC_ACTION(DelayedArm);
STATIC_ACTION(CheckDisarm);
STATIC_ACTION(MenuFocusIn);
STATIC_ACTION(MenuFocusOut);

char _XmCascadeB_menubar_events[] = 
   "<EnterWindow>Normal:     MenuBarEnter()\n\
    <LeaveWindow>Normal:     MenuBarLeave()\n\
    <BtnDown>:               MenuBarSelect()\n\
    <BtnUp>:                 DoSelect()\n\
    <Key>osfSelect:          KeySelect()\n\
    <Key>osfActivate:        KeySelect()\n\
    <Key>osfHelp:            Help()\n\
    <Key>osfCancel:          CleanupMenuBar()\n\
    ~s <Key>Return:          KeySelect()\n\
    ~s <Key>space:           KeySelect()";

char _XmCascadeB_p_events[] =
   "<EnterWindow>:           DelayedArm()\n\
    <LeaveWindow>:           CheckDisarm()\n\
    <BtnDown>:               StartDrag()\n\
    <BtnUp>:                 DoSelect()\n\
    <Key>osfSelect:          KeySelect()\n\
    <Key>osfActivate:        KeySelect()\n\
    <Key>osfHelp:            Help()\n\
    <Key>osfCancel:          CleanupMenuBar()\n\
    ~s <Key>Return:          KeySelect()\n\
    ~s <Key>space:           KeySelect()";

static XtActionsRec actions[] = {
    {"MenuBarSelect", _XmCBMenuBarSelect},
    {"MenuBarEnter", MenuBarEnter},
    {"StartDrag", StartDrag},
    {"DoSelect", _XmCBMenuBarDoSelect},
    {"KeySelect", KeySelect},
    {"Help", Help},
    {"CleanupMenuBar", CleanupMenuBar},
    {"EnterWindow", EnterWindow},
    {"LeaveWindow", LeaveWindow},
    {"DelayedArm", DelayedArm},
    {"CheckDisarm", CheckDisarm},
    {"MenuFocusIn", MenuFocusIn},
    {"MenuFocusOut", MenuFocusOut},
};

static XmBaseClassExtRec _XmCascadeBCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL, /* FIXME */
    /* set_values_prehook        */ NULL, /* FIXME */
    /* initialize_posthook       */ NULL, /* FIXME */
    /* set_values_posthook       */ NULL, /* FIXME */
    /* secondary_object_class    */ NULL, /* FIXME */
    /* secondary_object_create   */ NULL, /* FIXME */
    /* get_secondary_resources   */ NULL, /* FIXME */
    /* fast_subclass             */ { 0 }, /* FIXME */
    /* get_values_prehook        */ NULL, /* FIXME */
    /* get_values_posthook       */ NULL, /* FIXME */
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ FALSE,
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

XmPrimitiveClassExtRec _XmCascadeBPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL, /* FIX ME */
    /* widget_display_rect */ NULL, /* FIX ME */
    /* widget_margins      */ NULL  /* FIX ME */
};

XmCascadeButtonClassRec xmCascadeButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelClassRec,
        /* class_name            */ "XmCascadeButton",
	/* widget_size           */ sizeof(XmCascadeButtonRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ XtInheritResize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmCascadeBCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight,
        /* border_unhighlight    */ XmInheritBorderUnhighlight,
        /* translations          */ XtInheritTranslations,
        /* arm_and_activate_proc */ XmInheritArmAndActivate,
        /* synthetic resources   */ NULL,
	/* num syn res           */ 0,
        /* extension             */ (XtPointer)&_XmCascadeBPrimClassExtRec,
    },
    /* Label Class part */
    {
        /* setOverrideCallback */ XmInheritSetOverrideCallback,
        /* menuProcs           */ XmInheritMenuProc,
        /* translations        */ XtInheritTranslations,
	/* extension           */ NULL
    },
    /* CascadeButton Class part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmCascadeButtonWidgetClass = (WidgetClass)&xmCascadeButtonClassRec;

static void
class_initialize()
{
    _XmCascadeBCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmCASCADE_BUTTON_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    CB_SetArmed(new_w, False);

    if (!XtIsSubclass(XtParent(new_w), xmRowColumnWidgetClass))
	_XmError(new_w, "Cascade parent must be a RowColumn.");

    if (Lab_MenuType(new_w) != XmMENU_BAR &&
        Lab_MenuType(new_w) != XmMENU_POPUP &&
	Lab_MenuType(new_w) != XmMENU_PULLDOWN &&
	Lab_MenuType(new_w) != XmMENU_OPTION)
	_XmError(new_w, "Cascade parent is incorrect type.");
    else
    {
	Lab_Highlight(new_w) = 0;
    }

    if (Lab_MenuType(new_w) == XmMENU_BAR) {
      XdbDebug(__FILE__, new_w, "Initialize: using translation table for use in MenuBar\n");
	XtOverrideTranslations(new_w, XtParseTranslationTable(_XmCascadeB_menubar_events));
    } else if (Lab_MenuType(new_w) == XmMENU_POPUP || Lab_MenuType(new_w) == XmMENU_PULLDOWN)
    {
      XdbDebug(__FILE__, new_w, "Initialize: using normal translation table\n");
	XtOverrideTranslations(new_w, XtParseTranslationTable(_XmCascadeB_p_events));
    } else {
      XdbDebug(__FILE__, new_w, "Initialize: using default translation table\n");
    }

    if (Lab_MarginWidth(new_w) == 0x0000FFFF)
    {
	/* do something funky for the pixmap, but for now do nothing -- FIX ME! */
	Lab_MarginWidth(new_w) = 0;
    }

    if (RC_Type(XtParent(new_w)) == XmMENU_PULLDOWN)
	_XmCreateArrowPixmaps(new_w);

    _XmCBRecomputeSize(new_w);
    CB_Timer(new_w) = 0;
}

static void
_XmCBRecomputeSize(Widget w)
{
	_XmCalcLabelDimensions(w);

	(*xmLabelClassRec.core_class.resize)(w);

	if (CB_CascadePixmap(w) != XmUNSPECIFIED_PIXMAP) {
		/* FIX ME */
		XtWidth(w) += 2 * XtHeight(w) / 3;
	}
}

static void
destroy(Widget w)
{
    _XmFreeScratchPixmap((XmScreen)XmGetXmScreen(XtScreen(w)), CB_ArmedPixmap(w));
    _XmFreeScratchPixmap((XmScreen)XmGetXmScreen(XtScreen(w)), CB_CascadePixmap(w));
}

static void 
realize(Widget w, 
	XtValueMask *value_mask, 
	XSetWindowAttributes *attributes)
{
#define superclass (&xmLabelClassRec)
    (*superclass->core_class.realize)(w, value_mask, attributes);
#undef superclass

/* Make sure the size is right to include pixmap */
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean refresh_needed = False;

    if (CB_CascadePixmap(old) != CB_CascadePixmap(new_w))
    {
#if 0
	XFreePixmap(XtDisplay(old), CB_CascadePixmap(old));
	XFreePixmap(XtDisplay(new_w), CB_ArmedPixmap(new_w));
#endif
	_XmCreateArrowPixmaps(new_w);
	refresh_needed = True;
    }

    if (XtSensitive(new_w) != XtSensitive(old)) {
	XdbDebug(__FILE__, new_w, "SetValues: sensitive changes to %d\n", XtSensitive(new_w));
	refresh_needed = True;
    }

    return refresh_needed;
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    XdbDebug(__FILE__, w, "expose() Armed(%d)\n", CB_IsArmed(w));

  /* The label's expose routine should be called after the cascade button's
     expose routine. */

    if (CB_IsArmed(w) || Lab_MenuType(w) == XmMENU_OPTION)
	XmCascadeButtonHighlight(w, True);
    else
	XmCascadeButtonHighlight(w, False);

#define superclass (&xmLabelClassRec)
    (*superclass->core_class.expose)(w, event, region);
#undef superclass
}

/*
 * CleanupMenuBar
 *    w       is a cascadeButton
 *
 * CleanupMenuBar is no longer static because it needs to be called on gadgets
 *    as well as widgets. Gadgets have no translation table...
 *    Danny 11/9/1996
 */
void
CleanupMenuBar(Widget w,
	       XEvent *event,
	       String *params,
	       Cardinal *num_params)
{
    Widget top_menu;
    Cardinal numparams = 0;

    XdbDebug(__FILE__, w, "CleanupMenuBar()\n");

    if (w == NULL)
	return;		/* Try not to crash */

    top_menu = _XmGetRC_PopupPosted(XtParent(w));

    /* Make sure a valid menu was found */
    if (top_menu && XmIsRowColumn(top_menu) &&
	(RC_Type(top_menu) != XmWORK_AREA) &&
	(RC_Type(top_menu) != XmMENU_OPTION))
    {
        /* Release the keyboard and pointer grabs */
      _XmUngrabKeyboard(w, CurrentTime);
      _XmUngrabPointer(w, CurrentTime);
	
	XdbDebug(__FILE__, w, "   Doing MenuShellPopdownDone(%s)\n",
		 XtName(XtParent(top_menu)));
	
	
	(*((XmMenuShellWidgetClass)XtClass(XtParent(top_menu)))
	   ->menu_shell_class.popdownDone)(XtParent(top_menu), event, NULL, &numparams);

	 /* Release any grab associated with the top level menu widget */
	 /* Release any grab associated with the menu bar, and mark it as
	    disarmed. */

	XdbDebug(__FILE__, XtParent(top_menu), "XtRemoveGrab\n");
	XtRemoveGrab(XtParent(top_menu));
    }
    else 
    {
	_XmWarning(w, "CascadeButton's parent is not a valid menu widget");
    }
}

static void
KeySelect(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "KeySelect\n");
    /* If we're in keyboard traversal mode, and if we have a submenu 
       attached, call the cascading callback, post the submenu and 
       allow keyboard traversal of it. */

    /* else call the activate callback, and unpost all active menus
       in the cascade */
}

static void 
Help(Widget w,
     XEvent *event,
     String *params,
     Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "Help\n");

    /* clean up things */

    /* invoke help callbacks */
    XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
}

static void
EnterWindow(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "EnterWindow\n");
}

static void
MenuBarEnter(Widget w,
	     XEvent *event,
	     String *params,
	     Cardinal *num_params)
{
    /* if we're not dragging, do nothing. */
    if (!_XmGetInDragMode(w))
      return;

    /* if we're already armed, do nothing */
    if (CB_IsArmed(w))
      return;

    /* treat this like a menu bar select event */
    XtCallActionProc(w, "MenuBarSelect", event, params, *num_params);
}

static void
LeaveWindow(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    if (CB_IsArmed(w))
	XmCascadeButtonHighlight(w, False);
}

void
_XmCBMenuBarDoSelect(Widget w,
		     XEvent *event,
		     String *params,
		     Cardinal *num_params)
{
    Widget submenu;

    submenu = (XmIsGadget(w) ? CBG_Submenu(w) : CB_Submenu(w));

    XdbDebug(__FILE__, w, "_XmCBMenuBarDoSelect()\n");

    /* quene events until the next button event. */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    /* if we have a submenu attached, call the cascading callback, 
       post the submenu and allow keyboard traversal of it. */

    if (submenu) 
    {
	/* Turn on keyboard traversal */
	_XmSetInDragMode(w, False);

	XdbDebug0(__FILE__, submenu, "RC_CascadeBtn(%s)-> %s\n", 
		XtName(submenu), XtName(w));

	RC_CascadeBtn(submenu) = w;
	RC_PopupPosted(XtParent(w)) = submenu;

	XdbDebug0(__FILE__, w, "RC_PopupPosted(%s) set to %s\n",
		XtName(XtParent(w)),
		submenu ? XtName(submenu) : "(null)");
    }

    RC_SetArmed(XtParent(w), 0);
}

void
_XmCBMenuBarSelect(Widget w,
		   XEvent *event,
		   String *params,
		   Cardinal *num_params)
{
    Widget	menu, shell, mb;
    Display	*dpy;
    Position my_x, my_y;

    /* first thing we do is free up the next button event. */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    dpy = XtDisplay(w);

    mb = XtParent(w);
    menu = XmIsGadget(w) ? CBG_Submenu(w) : CB_Submenu(w);
    shell = menu ? XtParent(menu) : NULL;

    if (!menu)
      return;

    /*
     * If this button is already armed, remove only any cascaded sub-menus and then
     * simply enable drag mode.
     */

    /*
     * Check for torn-off menu
     */
#ifdef RESTORE_TEAR_OFFS
    if (RC_TearOffModel(menu) == XmTEAR_OFF_ENABLED)
    {
        XdbDebug(__FILE__, w, 
		 "_XmCBMenuBarSelect: restoring tear off menu to menu shell\n");
        _XmRestoreTearOffToMenuShell(menu, event);

        shell = XtParent(menu);
    }
#else
    if (RC_TornOff(menu)) 
    {
        XdbDebug(__FILE__, w, 
		 "_XmCBMenuBarSelect: menu was torn off; can't cope with that.\n");
        return;
    }
#endif


    /*
     * Check if our menu is already up
     */
    if (((ShellWidget)shell)->shell.popped_up)
    {
        XdbDebug(__FILE__, w, 
		 "MenuBarSelect: Menu is already up (Shell %s, Menu Pane %s)\n",
		 shell ? XtName(shell) : "(null)",
		 menu ? XtName(menu) : "(null)");

	XdbDebug(__FILE__, w, "MenuBarSelect: shell popped up %d, armed CB %d\n",
		 ((ShellWidget)shell)->shell.popped_up,
		 XmIsGadget(w)?CBG_IsArmed(w):CB_IsArmed(w));


	/* Popdown any sub menu levels */

	XtCallActionProc(shell, "MenuShellPopdownDone", event, NULL, 0);
    }
    else 
    {
        XmAnyCallbackStruct cbs;
        ShellWidget popupShell;
	Widget popupPosted = _XmGetRC_PopupPosted(XtParent(w));

	/* popup any submenus (besides ours) that are already popped up. */
	if (popupPosted)
        {
	  popupShell = (ShellWidget)XtParent(popupPosted);

	  (*xmLabelClassRec.label_class.menuProcs)(XmMENU_SHELL_POPDOWN, 
						   (Widget)popupShell, 
						   NULL, 
						   event, 
						   NULL);
	  /* This is an educated guess at best, but is seems to solve
	   * the problem seen in xephem: popups refusing to take focus sometimes.
	   * PvH 10/22/96
	   */
	  XtRemoveGrab((Widget)popupShell);
          XtRemoveGrab(mb);
	}

        XdbDebug(__FILE__, w, 
		 "MenuBarSelect : need to pop up a menu (Shell %s, Menu Pane %s)\n",
		 shell ? XtName(shell) : "(null)",
		 menu ? XtName(menu) : "(null)");

	/* grab the keyboard and freeze everything so that we can be sure that
	   the next event goes to the right place. */
	XdbDebug(__FILE__, w, "    Doing _XmGrabKeyboard\n");
	
	_XmGrabKeyboard(mb,
			/* owner_events */	True,
			/* Pointer mode */	GrabModeSync,
			/* Keyboard mode */	GrabModeSync,
			/* Timestamp */		CurrentTime);

	/* get the window that previously had the focus
	   before popping up the menu so we can revert 
	   to it after we're done. */
	_XmMenuFocus(w, XmMENU_FOCUS_SAVE, CurrentTime);
	
	_XmMenuFocus(mb, XmMENU_FOCUS_SET, CurrentTime);

	/* We can unfreeze the keyboard events, since the input focus has been set */
	XdbDebug(__FILE__, w, "    Doing XAllowEvents(AsyncKeyboard)\n");
	XAllowEvents(dpy,
		     AsyncKeyboard,
		     CurrentTime);

	/* Set drag mode */
	_XmSetInDragMode(w, True);

	if (XmIsGadget(w))
	    CBG_SetArmed(w, True);
	else
	    CB_SetArmed(w, True);

	/* highlight the cascade button. */
	XmCascadeButtonHighlight(w, True);

	    
	/* Register inside the RC that this cascadebutton is the one that 
	   triggered the menu */
	RC_CascadeBtn(menu) = w;
      XdbDebug0(__FILE__, menu, "RC_CascadeBtn(%s)-> %s\n", XtName(menu), XtName(w));
	
	XtUnmapWidget(menu);
	
	/* position the row column inside the menushell*/
	_XmMoveObject(menu, 0,0);
	
	/* now move the menushell */
      XtTranslateCoords(w, 0, 0, &my_x, &my_y);

	_XmMoveObject(shell, my_x, my_y + XtHeight(w));
	XdbDebug(__FILE__, shell, "_XmMoveObject -> %d %d\n", my_x, my_y + XtHeight(w));
	
        cbs.reason=XmCR_CASCADING;
        cbs.event=event;
	XtCallCallbackList(w, 
			   (XmIsGadget(w) 
			    ? CBG_CascadeCall(w) 
			    : CB_CascadeCall(w)), 
			   &cbs);
	
	RC_PopupPosted(mb) = menu;
	XdbDebug0(__FILE__, mb, "RC_PopupMenuPosted(%s) set to %s\n",
		  mb ? XtName(mb) : "(null)",
		  shell ? XtName(shell) : "(null)");
	
	RC_LastSelectToplevel(menu) = menu;
	
	/*
	 * Add the menu bar to the TOP of the modal cascade, as an exclusive grab.
	 * This causes all pointer events which occur outside the menu
	 * cascade to be delivered to the menu bar widget.  It is 
	 * essential to have at least one widget in the cascade own an 
	 * exclusive grab, because otherwise external events would be
	 * suppressed by the Intrinsics.  This would prevent us from
	 * releasing the next pointer event in the server, and hence the
	 * system would lock.
	 */
	XdbDebug(__FILE__, mb, "    Doing XtAddGrab()\n");
	XtAddGrab(mb, True, True);
	
	/*
	 * Popup the menu shell, and append it to the modal cascade.
	 * Must be done after adding grab for menu bar, so that the menu
	 * bar is the first widget in the modal cascade.
	 */
	XdbDebug(__FILE__, menu, "XtMapWidget\n");
	XtMapWidget(menu);
	XdbDebug(__FILE__, shell, "XMapRaised(0x%X) geo %d %d %dx%d\n",	
		XtWindow(shell), XtX(shell), XtY(shell), XtWidth(shell), XtHeight(shell));
	XMapRaised(XtDisplay(shell), XtWindow(shell));

	/*
	 * The XtMapWidget and XMapRaised above don't always seem to work.
	 * The "Other" menu in testXm/cascadebutton/test1 is an example.
	 * Danny 7/8/96.
	 */
	XtManageChild(menu);
	
	XdbDebug(__FILE__, shell, "XtAddGrab()\n");
	XtAddGrab(shell, False, False);
	
	/* in xscope, after the call to MapWindow and before the
	   call to _XmGrabPointer, it seems like all the children
	   of the menu shell are being exposed -- you see all these
	   things like PolyText8 and CopyArea and PolySegment
	   flying around.  The server hasn't generated an Expose
	   event yet, so the client side code must be walking through 
	   the widget tree calling the widgets' expose methods
	   itself -- faking an expose event. */
	/*	_XmFakeExpose(shell);*/
	
	/* Initiate a new pointer grab for the menu bar */
	/* Changing owner_events to False is a bad idea - Danny 12/5 */
	/* Pointer mode will change to Sync anyway with the 
	   XAllowEvents. It's also set here, according to xscope - Chris 6/23 */
	
	XdbDebug(__FILE__, menu, "_XmGrabPointer()\n");

	_XmGrabPointer(mb,
		       /* owner_events */	True,
		       /* Event Mask */	(ButtonPressMask 
					 | ButtonReleaseMask 
					 | EnterWindowMask 
					 | LeaveWindowMask),
		       /* Pointer Mode */	GrabModeSync,
		       /* Keyboard Mode */	GrabModeAsync,
		       /* Confine_to */	None,
		       /* Cursor */		XmGetMenuCursor(dpy),
		       /* Timestamp */		CurrentTime);

	_XmMenuFocus(shell, XmMENU_FOCUS_SET, CurrentTime);

	/* queue events up until the next button event. */
	XAllowEvents(dpy, SyncPointer, CurrentTime);
    }
    
    /* arm the menu bar */
    RC_SetArmed(mb, 1);
}

static void
StartDrag(Widget w,
	  XEvent *event,
	  String *params,
	  Cardinal *num_params)
{
    Boolean validButton;

    XdbDebug(__FILE__, w, "StartDrag()\n");

    /* queue events up until the next button event. */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    /* Is it even the right kind of event? */
    if (!event
	|| event->type != ButtonPress)
      return;

    /* Was it the right button? */
    (* xmLabelClassRec.label_class.menuProcs) (XmMENU_BUTTON, 
					       XtParent(w), 
					       NULL, 
					       event, 
					       &validButton);
    if (!validButton)
      return;
    
    /* If the submenu is already active, disable keyboard traversal
       and set it to mouse traversal */

    if (CB_Submenu(w) 
	&& XmIsMenuShell(XtParent(CB_Submenu(w))))
    {
        XmAnyCallbackStruct cbs;
	Widget sub_menu_pane = CB_Submenu(w);
	Position my_x, my_y;

	XtUnmapWidget(CB_Submenu(w));
	
	/* FIX ME, check for identity, somewhere else ? */
	if (RC_PopupPosted(XtParent(w)) != NULL)
	{
	    /* FIX ME: cb might be a gadget */
	    Widget cb = RC_CascadeBtn(RC_PopupPosted(XtParent(w)));

	    XtPopdown(XtParent(RC_PopupPosted(XtParent(w))));

	    CB_SetArmed(cb, False);

	    XmCascadeButtonHighlight(cb, False);
	
	}

	/* position the row column inside the menushell*/
	_XmMoveObject(sub_menu_pane, 0,0);

	XtTranslateCoords(w, 0, 0, &my_x, &my_y);

	_XmMoveObject(XtParent(sub_menu_pane),
		      my_x + XtWidth(w) - Lab_Shadow(w),
		      my_y);

        cbs.reason=XmCR_CASCADING;
        cbs.event=event;
	XtCallCallbackList(w,
			   CB_CascadeCall(w),
			   &cbs);

	XdbDebug0(__FILE__, sub_menu_pane, "RC_CascadeBtn(%s)-> %s\n", 
		XtName(sub_menu_pane), XtName(w));

	XtManageChild(sub_menu_pane);

	RC_CascadeBtn(sub_menu_pane) = w;
	RC_PopupPosted(XtParent(w)) = sub_menu_pane;

	XdbDebug0(__FILE__, w, "RC_PopupMenuPosted(%s) set to %s\n",
		XtParent(w) ? XtName(XtParent(w)) : "(null)",
		sub_menu_pane ? XtName(sub_menu_pane) : "(null)");

	RC_LastSelectToplevel(sub_menu_pane) = RC_LastSelectToplevel(XtParent(w));

	XdbDebug(__FILE__, w, "    Top level menu is %s\n",
		 XtName(RC_LastSelectToplevel(sub_menu_pane)));

	XtMapWidget(CB_Submenu(w));

	XtPopup(XtParent(CB_Submenu(w)), XtGrabNonexclusive);

	/* Arm the cascade button, post the submenu, and enable mouse
	 * traversal on it.
	 */

	CB_SetArmed(w, True);

	XmCascadeButtonHighlight(w, True);

    }
    else if (CB_Submenu(w))
    {
        _XmWarning(w,
                   "CascadeButton's popup must be a subclass of XmMenuShell\n");
        return;
    }

    _XmSetInDragMode(w, True);
}

static void 
CascadePopupHandler(XtPointer clientData, XtIntervalId *id)
{
    Widget w = (Widget)clientData;

    XdbDebug(__FILE__, w, "CascadePopupHandler\n");

    CB_Timer(w) = 0;

    if (CB_Submenu(w) && XmIsMenuShell(XtParent(CB_Submenu(w))))
    {
        XmAnyCallbackStruct cbs;
	Position my_x, my_y;
	
	/* make sure the menu is popped down */

	XtUnmapWidget(CB_Submenu(w));
	
	/* position the row column inside the menushell*/
	_XmMoveObject(CB_Submenu(w), 0,0);

	/* now move the menushell */
	XtTranslateCoords(w, 0, 0, &my_x, &my_y);
	_XmMoveObject(XtParent(CB_Submenu(w)),
		      my_x + XtWidth(w) - Lab_Shadow(w),
		      my_y);

	cbs.reason=XmCR_CASCADING;
	cbs.event=NULL; /* I dunno...? what's it supposed to be? */
	XtCallCallbackList(w,
			   CB_CascadeCall(w),
			   &cbs);

	RC_CascadeBtn(CB_Submenu(w)) = w;
	XdbDebug0(__FILE__, CB_Submenu(w), "RC_CascadeBtn(%s)-> %s\n", 
		XtName(CB_Submenu(w)), XtName(w));

	RC_PopupPosted(XtParent(w)) = CB_Submenu(w);

	XdbDebug0(__FILE__, w, "RC_PopupMenuPosted(%s) set to %s\n",
		XtParent(w) ? XtName(XtParent(w)) : "(null)",
		CB_Submenu(w) ? XtName(CB_Submenu(w)) : "(null)");

	RC_LastSelectToplevel(CB_Submenu(w)) = RC_LastSelectToplevel(XtParent(w));

	XdbDebug(__FILE__, w, "    Top level menu is %s\n",
		 XtName(RC_LastSelectToplevel(CB_Submenu(w))));

	XtManageChild(CB_Submenu(w));

	/* FIX ME: map done by XtPopup */
	XtMapWidget(CB_Submenu(w));

	XtPopup(XtParent(CB_Submenu(w)), XtGrabNonexclusive);
    }
    else if (CB_Submenu(w))
    {
	_XmWarning(w,
                   "CascadeButton's popup must be a subclass of XmMenuShell\n");
        return;
    }
}

static void 
DelayedArm(Widget w, 
           XEvent *event, 
           String *params, 
           Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "DelayedArm()\n");

    /* Signal server to release next pointer event */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    if (_XmGetInDragMode(w)) 
    {
	CB_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				      CB_MapDelay(w),
				      CascadePopupHandler,
				      (XtPointer)w);
	
	CB_SetArmed(w, True);

	XmCascadeButtonHighlight(w, True);
    }
}

static void 
CheckDisarm(Widget w, 
	    XEvent *event, 
	    String *params, 
	    Cardinal *num_params)
{
    int x = ((XLeaveWindowEvent*)event)->x;
    int y = ((XLeaveWindowEvent*)event)->y;

    XdbDebug(__FILE__, w, "CheckDisarm()\n");

    if (_XmGetInDragMode(w)) 
    {
	if (CB_Timer(w))
	{
	    XtRemoveTimeOut(CB_Timer(w));
	    CB_Timer(w) = 0;
	}

	/*
	 * If a submenu is already popped up, pop it down unless
	 * we've left on the right edge.
	 * The test is a bit elaborate because the right edge is 
	 * not a straight line (at least, in M*tif: FIX ME).
	 */
	if ((x <= 0 || y <= 0 || y >= XtHeight(w)) &&
	    CB_Submenu(w) != NULL)
	{
	    XtPopdown(XtParent(CB_Submenu(w)));
	    /* FIX ME: more? */
	}

	CB_SetArmed(w, False);

	XmCascadeButtonHighlight(w, False);
    }
}

static void 
MenuFocusIn(Widget w, 
	    XEvent *event, 
	    String *params, 
	    Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "MenuFocusIn()\n");
}

static void 
MenuFocusOut(Widget w, 
	     XEvent *event, 
	     String *params, 
	     Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "MenuFocusOut()\n");
}

void
_XmCBHelp(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
}

void
_XmCascadingPopup(Widget cb, XEvent *event, Boolean doCascade)
{
}

void
XmCascadeButtonHighlight(Widget cb, 
			 Boolean highlight)
{
    XdbDebug(__FILE__, cb, "XmCascadeButtonHighlight(hl %d, armed %d, apm 0x%X, cpm 0x%X)\n",
	highlight,
	XmIsPrimitive(cb) ? CB_IsArmed(cb) : CBG_IsArmed(cb),
	XmIsPrimitive(cb) ? CB_ArmedPixmap(cb) : CBG_ArmedPixmap(cb),
	XmIsPrimitive(cb) ? CB_CascadePixmap(cb) : CBG_CascadePixmap(cb));

    if (! XtIsRealized(cb))
	return;

    if (XmIsPrimitive(cb)) {
#ifdef	DO_SANITY
	if (CB_CascadePixmap(cb) == 0) {
		XdbDebug(__FILE__, cb, "CascadePixmap has NULL value\n");
#if 0
		_XmWarning(cb, "CascasdePixmap has NULL value\n");
#endif
	}
	if (CB_ArmedPixmap(cb) == 0) {
		XdbDebug(__FILE__, cb, "ArmedPixmap has NULL value\n");
#if 0
		_XmWarning(cb, "ArmedPixmap has NULL value\n");
#endif
	}
#endif
	_XmDrawShadows(XtDisplayOfObject(cb),
		       XtWindowOfObject(cb),
		       Prim_TopShadowGC(cb),
		       Prim_BottomShadowGC(cb),
		       0,0,
		       XtWidth(cb),
		       XtHeight(cb),
		       Lab_Shadow(cb),
		       highlight ? (int)XmSHADOW_OUT : (int)XmNO_LINE);

/* now draw the pixmap 
   -- this pixmap stuff is (at least until we get the other pixmap)
   only for cascade buttons inside pulldowns.  Don't do it for
   option menus. */
	if (Lab_MenuType(cb) == XmMENU_PULLDOWN)
	  if (CB_IsArmed(cb)) {
	    if (CB_ArmedPixmap(cb) != XmUNSPECIFIED_PIXMAP 
		&& CB_ArmedPixmap(cb) != 0) {
	      XdbDebug(__FILE__, cb, "XCopyArea 0x%X -> 0x%X %d %d %dx%d\n",
		       CB_ArmedPixmap(cb), XtWindow(cb),
		       2 * XtHeight(cb) / 3, XtHeight(cb) / 2,
		       XtWidth(cb) - 2 * XtHeight(cb) / 3, XtHeight(cb) / 4);

	      XCopyArea(XtDisplay(cb), CB_ArmedPixmap(cb), XtWindow(cb),
			Lab_NormalGC(cb),
			0, 0,
			2 * XtHeight(cb) / 3, XtHeight(cb) / 2,
			XtWidth(cb) - 2 * XtHeight(cb) / 3, XtHeight(cb) / 4);
	    }
	  } 
	  else if (CB_CascadePixmap(cb) != XmUNSPECIFIED_PIXMAP 
		   && CB_CascadePixmap(cb) != 0) {
	    XdbDebug(__FILE__, cb, "XCopyArea 0x%X -> 0x%X geo %d %d %dx%d\n",
		     CB_CascadePixmap(cb), XtWindow(cb),
		     2 * XtHeight(cb) / 3, XtHeight(cb) / 2,
		     XtWidth(cb) - 2 * XtHeight(cb) / 3, XtHeight(cb) / 4);
	    XCopyArea(XtDisplay(cb), CB_CascadePixmap(cb), XtWindow(cb),
		      Lab_NormalGC(cb),
		      0, 0,
		      2 * XtHeight(cb) / 3, XtHeight(cb) / 2,
		      XtWidth(cb) - 2 * XtHeight(cb) / 3, XtHeight(cb) / 4);
	}
    } 
    else if (XmIsGadget(cb))
	XmCascadeButtonGadgetHighlight(cb, highlight);
    else
      _XmError(cb, "XmCascadeButtonHighlight called with non-cascade button widget");
}

Widget
XmCreateCascadeButton(Widget parent,
		      char *name,
		      Arg *arglist,
		      Cardinal argcount)
{
    return XtCreateWidget(name, 
			  xmCascadeButtonWidgetClass,
			  parent,
			  arglist, argcount);
}
