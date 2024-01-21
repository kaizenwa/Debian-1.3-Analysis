/**
 * 
 * $Id: MenuShell.c,v 1.9 1996/11/28 09:21:31 u27113 Exp $
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

static char rcsid[] = "$Id: MenuShell.c,v 1.9 1996/11/28 09:21:31 u27113 Exp $";

#define	FAKE_NONZERO

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/RowColumnP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <assert.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */
static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes);
static void expose(Widget w, XEvent *event, Region region);
static void resize(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static XtGeometryResult GeometryManager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply);
static void change_managed(Widget w);
static void insert_child(Widget w);

#define Offset(field) XtOffsetOf(XmMenuShellRec, menu_shell.field)

/* Resources for the MenuShell class */
static XtResource resources[] = {
    {
	XmNdefaultFontList, XmCDefaultFontList, XmRFontList,
	sizeof(XmFontList), Offset(default_font_list),
	XmRString, (XtPointer)NULL
    },
    {
	XmNlabelFontList, XmCLabelFontList, XmRFontList,
	sizeof(XmFontList), Offset(label_font_list),
	XmRFontList, (XtPointer)NULL
    },
    {
	XmNbuttonFontList, XmCButtonFontList, XmRFontList,
	sizeof(XmFontList), Offset(button_font_list),
	XmRFontList, (XtPointer)NULL
    }
};

static void MenuShellPopdownDone(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void MenuShellPopdownOne(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void MenuShellPopdownEveryone(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void MenuShellPopupSharedMenuPane(Widget w, Widget w2, XEvent *event);

char _XmMenuShell_translations[] = 
   "<BtnDown>:            ClearTraversal()\n\
    <BtnUp>:              MenuShellPopdownDone()";

static XtActionsRec actions[] = {
    {"ClearTraversal", _XmClearTraversal},
    {"MenuShellPopdownOne", MenuShellPopdownOne},
    {"MenuShellPopdownDone", MenuShellPopdownDone},
};

static XmBaseClassExtRec _XmMenuSCoreClassExtRec = {
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
    /* class_part_init_prehook   */ NULL, /* FIXME */
    /* class_part_init_posthook  */ NULL, /* FIXME */
    /* ext_resources             */ NULL, /* FIXME */
    /* compiled_ext_resources    */ NULL, /* FIXME */
    /* num_ext_resources         */ 0, /* FIXME */
    /* use_sub_resources         */ FALSE, /* FIXME */
    /* widget_navigable          */ NULL, /* FIXME */
    /* focus_change              */ NULL, /* FIXME */
    /* wrapper_data              */ NULL
};

XmMenuShellClassRec xmMenuShellClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &overrideShellClassRec,
        /* class_name            */ "XmMenuShell",
	/* widget_size           */ sizeof(XmMenuShellRec),
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
	/* compress_exposure     */ XtExposeCompressMultiple,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmMenuShell_translations,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmMenuSCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ GeometryManager,
        /* change_managed   */ change_managed,
        /* insert_child     */ insert_child,
        /* delete_child     */ NULL,
        /* extension        */ NULL,	
    },
    /* Shell class part */
    {
	/* extension        */ NULL,
    },
    /* Override class part */
    {
	/* extension        */ NULL,
    },
    /* XmMenuShell class part */
    {
        /* popdownOne          */ MenuShellPopdownOne, 
        /* popdownEveryone     */ MenuShellPopdownEveryone,
        /* popdownDone         */ MenuShellPopdownDone, 
        /* popupSharedMenupane */ MenuShellPopupSharedMenuPane,
	/* extension           */ NULL,
    },
};

WidgetClass xmMenuShellWidgetClass = (WidgetClass)&xmMenuShellClassRec;

static void
class_initialize()
{
    _XmMenuSCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmMENU_SHELL_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmMenuShellWidget mw = (XmMenuShellWidget)new_w;

    if (mw->menu_shell.button_font_list == NULL)
	if (mw->menu_shell.default_font_list != NULL)
	    mw->menu_shell.button_font_list = mw->menu_shell.default_font_list;
	else
	    mw->menu_shell.button_font_list = _XmGetDefaultFontList(new_w,
								    XmBUTTON_FONTLIST);

    if (mw->menu_shell.label_font_list == NULL)
	if (mw->menu_shell.default_font_list != NULL)
	    mw->menu_shell.label_font_list = mw->menu_shell.default_font_list;
	else
	    mw->menu_shell.label_font_list = _XmGetDefaultFontList(new_w,
								   XmLABEL_FONTLIST);

    XtBorderWidth(new_w) = 0;

    /* menu shells must be given non-zero width's and height's when
       they are initialized, as we create the window (realize the widget) here. */

#ifdef	FAKE_NONZERO
    if (XtWidth(new_w) == 0 || XtHeight(new_w) == 0) {
	XdbDebug(__FILE__, new_w, "Initialize: dimensions %d %d changed to 1x1",
	    XtWidth(new_w), XtHeight(new_w));

	XtWidth(new_w) = XtHeight(new_w) = 1;
    }
#endif

    if (XtWidth(new_w) == 0 || XtHeight(new_w) == 0)
      _XmError(new_w, "Shell widget %s has zero width and/or height\n", XtName(new_w));

    XtRealizeWidget(new_w);
}

static void
destroy(Widget w)
{
}

static void
resize(Widget w)
{
    XdbDebug(__FILE__, w, "Resize -- (%d, %d)\n", XtWidth(w), XtHeight(w));
}

static void 
realize(Widget w, 
	XtValueMask *value_mask, 
	XSetWindowAttributes *attributes)
{
    *value_mask = CWBackPixmap | CWBorderPixel | CWBitGravity | CWOverrideRedirect | CWEventMask | CWSaveUnder | CWColormap;

    attributes->background_pixmap = None;
    attributes->save_under = True;
    attributes->bit_gravity = NorthWestGravity;
    attributes->override_redirect = True;
    attributes->event_mask = ButtonPressMask | ButtonReleaseMask | StructureNotifyMask;

    if (XtWidth(w) == 0)
	XtWidth(w) = 1;
    if (XtHeight(w) == 0)
	XtHeight(w) = 1;

#define superclass (&overrideShellClassRec)    
    (*superclass->core_class.realize)(w, value_mask, attributes);
#undef superclass

    XdbDebug(__FILE__, w, "Realize (size %dx%d)\n", XtWidth(w), XtHeight(w));
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
	XdbDebug(__FILE__, w, "Expose\n");
}

static Boolean 
set_values(Widget current,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "SetValues\n");

    return True;
}

static XtGeometryResult 
GeometryManager(Widget w,
		XtWidgetGeometry *request,
		XtWidgetGeometry *reply)
{
    XtGeometryResult res;
    XmMenuShellWidget ms = (XmMenuShellWidget)XtParent(w);

    /* if we're not listening to our children, there's not
       much to do. */
    if ((request->request_mode & (CWWidth|CWHeight)) && ms->shell.allow_shell_resize == False) {
	XdbDebug(__FILE__, XtParent(w), "GeometryManager: request %s => No\n",
	     XdbWidgetGeometry2String(request));
	return XtGeometryNo;
    }
    if ((request->request_mode & (CWWidth|CWHeight)) == 0)
	return XtGeometryYes;

    XdbDebug(__FILE__, XtParent(w), "GeometryManager: %s\n", XdbWidgetGeometry2String(request));

    do {
	res = XtMakeResizeRequest(XtParent(w),
				  request->width, request->height,
				  &request->width, &request->height);
    } while (res == XtGeometryAlmost);

    if (res == XtGeometryNo)
      XdbDebug(__FILE__, w, "XtGeometryNo returned... THIS SHOULD NOT HAPPEN\n");

    reply = request;

    _XmResizeObject(w, request->width, request->height, 0);

    XdbDebug(__FILE__, w, "GeometryManager: size %dx%d => Yes\n", request->width, request->height);

    return XtGeometryYes;
}

static void
change_managed(Widget w)
{
    XtWidgetGeometry	geo;
    Widget		child;
    int			i;

    XdbDebug(__FILE__, w, "ChangeManaged: trying to find child to manage\n");

    child = NULL;
    for (i=0; i<MGR_NumChildren(w); i++) {
	XdbDebug2(__FILE__, w, 
		  MGR_Children(w)[i], "ChangeManaged [%d] %s\n",
		  i, 
		  XtIsManaged(MGR_Children(w)[i]) ? "Managed" : "Not Managed");
	if (XtIsManaged(MGR_Children(w)[i])) {
		child = MGR_Children(w)[i];
		break;
	}
    }

    if (! child) {
	XdbDebug(__FILE__, w, 
		 "change_managed: no managed children so we must be popping down\n");
	XtPopdown(w);
	return;
    }

#if 0
/* allow_shell_resize shouldn't be looked at here - only in set_values() */
/* I think :-) Danny */
    if (((XmMenuShellRec *)w)->shell.allow_shell_resize) 
#endif
    {
	/* MenuShell's take their height/width from the child */
	XtQueryGeometry(MGR_Children(w)[0], NULL, &geo);

	/* FIX ME: should look at the flags */
	_XmResizeObject(w, geo.width, geo.height, 0);
    }

    XdbDebug2(__FILE__, w, child, "ChangeManaged width %d height %d\n",
	XtWidth(w), XtHeight(w));

/* MenuShell children should be at 0,0 positions !
 * Danny 17/11/96 */
    _XmMoveObject(child, 0, 0);

    if (RC_Type(child) == XmMENU_POPUP) 
    {
	XdbDebug2(__FILE__, w, child, "Popping up\n");
	
	/* FIX ME */
	_XmPostPopupMenu(child, NULL); 
    }
}

static void
insert_child(Widget w)
{
    if (!XmIsRowColumn(w)) 
    {
        _XmWarning(w, 
		   "MenuShell widgets must have a xmRowColumnWidgetClass child.");
	return;
    } 
    else 
    {
#define superclass (&overrideShellClassRec)    
        (*superclass->composite_class.insert_child)(w);
#undef superclass

	/* this might not need to get done.
	   Does the composite class's insert child realize
	   a widget if the composite is realized? */
	XtRealizeWidget(w);
    }
}

static void
MenuShellPopdownDone(Widget w,
		     XEvent *event,
		     String *params,
		     Cardinal *num_params)
{
    Cardinal numparams = 0;
    Widget rc, toplevelrc;
    Widget menu_shell;
    int		i;

    XdbDebug(__FILE__, w, "MenuShellPopdownDone()\n");

    /* must be a menu shell here. */
    assert(XmIsMenuShell(w));

    if (((CompositeWidget)w)->composite.num_children == 0)
      return;

/* Find a child that is managed */
    rc = NULL;
    for (i=0; i<MGR_NumChildren(w); i++) {
	rc = MGR_Children(w)[i];
	if (XtIsManaged(rc))
	    break;
    }

    if (!rc)
      return;

    XdbDebug2(__FILE__, w, rc, "MenuShellPopdownDone - found RC\n");

    if (_XmGetInPMMode(w))
    {
      /* if we're in popup menu mode, the popup menu
	 is popped up spring loaded, which means if we're
	 here, this was the menu that was popped up initially.
	 
	 This may or may not be a good assumption to make here :)
	 but we'll roll with it.
	 */
        menu_shell = w;
    }
    else
    {
        toplevelrc = RC_LastSelectToplevel(rc);

/* Without the statement below, the "assert" fails when you use an accelerator
 * when you haven't popped up the menu yet.
 */
	if (! toplevelrc)
		toplevelrc = rc;

	assert(toplevelrc);

	menu_shell = XtParent(toplevelrc);
    }

    XdbDebug(__FILE__, w, "  calling popdownEveryone on toplevel menushell\n");
    (*((XmMenuShellWidgetClass)XtClass(menu_shell))
	   ->menu_shell_class.popdownEveryone)(menu_shell,
					       event, 
					       NULL, 
					       &numparams);
    
    _XmUngrabKeyboard(w, CurrentTime);
    
    _XmUngrabPointer(w, CurrentTime);

    if (_XmGetInPMMode(w))
      XtUnmanageChild(rc);

    if (!_XmGetInPMMode(w))
      /* now we restore the focus to the window that had it before we went
	 carousing into the menu code. */	
      _XmMenuFocus(w, XmMENU_FOCUS_RESTORE, CurrentTime);

    _XmSetInPMMode(w, False);
}

static void
MenuShellPopdownOne(Widget w,
		    XEvent *event,
		    String *params,
		    Cardinal *num_params)
{
    Widget rc = NULL;

    XdbDebug(__FILE__, w, "MenuShellPopdownOne()\n");

    /* must be a menu shell here. */
    assert(XmIsMenuShell(w));

    if (((CompositeWidget)w)->composite.num_children == 0)
      return;

    rc = ((CompositeWidget)w)->composite.children[0];

    if (!rc || !XmIsRowColumn(rc))
      return;

    XdbDebug(__FILE__, w, "  Child menu pane is %s\n", XtName(rc));
    
    /* unhighlight the cascade button that popped us up. */
    if (RC_CascadeBtn(rc))
    {
        XdbDebug(__FILE__, w, "  Removing myself from the cascade\n");

	/* disarm the cascade button that pulled us down. */
	XmCascadeButtonHighlight(RC_CascadeBtn(rc), False);
	
	if (XmIsPrimitive(RC_CascadeBtn(rc)))
	    CB_SetArmed(RC_CascadeBtn(rc), False);
	else
  	    CBG_SetArmed(RC_CascadeBtn(rc), False);
	    
	/* disarm the menu bar, if we're a toplevel pulldown. */
	if (RC_Type(XtParent(RC_CascadeBtn(rc))) == XmMENU_BAR)
	    RC_SetArmed(XtParent(RC_CascadeBtn(rc)), 0);
	
	/* clean up everything so noone thinks we're
	   popped up. */
	RC_PopupPosted(XtParent(RC_CascadeBtn(rc))) = NULL;
	RC_LastSelectToplevel(rc) = NULL;
	RC_CascadeBtn(rc) = NULL;
    } 

    XdbDebug(__FILE__, w, "  Popping down\n");
    XtUnmapWidget(w);
    XtPopdown(w);
}

static void 
MenuShellPopdownEveryone(Widget w, 
			 XEvent *event, 
			 String *params, 
			 Cardinal *num_params)
{
  Cardinal numparams = 0;
  Widget rc;

  /* must be a menu shell here. */
  assert(XmIsMenuShell(w));

  XdbDebug(__FILE__, w, "Popping down everyone\n");

  if (((CompositeWidget)w)->composite.num_children == 0)
    return;

  rc = ((CompositeWidget)w)->composite.children[0];

  if (!rc)
    return;

  if (RC_PopupPosted(rc))
  {
      /* pop down our own sub tree, then ourselves. */
      Widget shell = XtParent(RC_PopupPosted(rc));
      
      if (!shell)
	  return;

      XdbDebug(__FILE__, shell, "  recursing in popdownEveryone.\n");
      (*((XmMenuShellWidgetClass)XtClass(shell))->menu_shell_class.popdownEveryone)(shell, event, NULL, &numparams);
  }

  XdbDebug(__FILE__, w, "  calling popdownOne.\n");
  (*((XmMenuShellWidgetClass)XtClass(XtParent(rc)))->menu_shell_class.popdownOne)(XtParent(rc), 
										  event, 
										  NULL, &numparams);
}

static void 
MenuShellPopupSharedMenuPane(Widget w, 
			     Widget w2, 
			     XEvent *event)
{
  /* in this code I assume that w is the menu shell and
     w2 is the cascade button (gadget). */

    /* must be a menu shell here. */
    assert(XmIsMenuShell(w));

}

void
_XmEnterRowColumn(Widget widget,
		  XtPointer closure,
		  XEvent *event,
		  Boolean *cont)
{
}

void
_XmClearTraversal(Widget wid,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
	XdbDebug(__FILE__, wid, "_XmClearTraversal(not implemented)\n");
}

void
_XmSetLastManagedMenuTime(Widget wid, 
			  Time newTime)
{
  XmMenuState state = _XmGetMenuState(wid);

  state->MS_LastManagedMenuTime = newTime;
}


Widget
XmCreateMenuShell( Widget parent, char *name, ArgList arglist, Cardinal argcount )
{
    while (parent && !XtIsComposite(parent))
	parent = XtParent(parent);

    return XtCreatePopupShell(name, xmMenuShellWidgetClass, parent,
			      arglist, argcount);
}

