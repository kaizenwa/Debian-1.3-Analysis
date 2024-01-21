/**
 *
 * $Id: DialogS.c,v 1.21 1997/01/11 02:19:44 miers Exp $
 *
 * derived from Xt Vendor class.c 
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright 1989 Massachusetts Institute of Technology
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

static char rcsid[] = "$Id: DialogS.c,v 1.21 1997/01/11 02:19:44 miers Exp $";

#include <stdio.h>

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/DialogSP.h>
#include <Xm/DialogSEP.h>
#include <X11/StringDefs.h>
#include <X11/ShellP.h>
#include <X11/Xfuncs.h>
#include <Xm/VendorSP.h>
#include <Xm/VendorSEP.h>
#include <Xm/BulletinBP.h>
#include <Xm/DisplayP.h>

#include <XmI/DebugUtil.h>

static void class_initialize();
static void class_part_initialize(WidgetClass wclass);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void change_managed(Widget w);
static void InsertChild(Widget w);
static void DeleteChild(Widget w);
static void StructureNotifyHandler(Widget w, XtPointer closure, XEvent *event, Boolean *cont);
static void WmProtocolHandler(Widget w, XtPointer client, XtPointer call);

static XmBaseClassExtRec _XmDialogSCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ XmInheritInitializePrehook,
    /* set_values_prehook        */ XmInheritSetValuesPrehook,
    /* initialize_posthook       */ XmInheritInitializePosthook,
    /* set_values_posthook       */ XmInheritSetValuesPosthook,
    /* secondary_object_class    */ (WidgetClass)&xmDialogShellExtClassRec,
    /* secondary_object_create   */ XmInheritSecObjectCreate,
    /* get_secondary_resources   */ NULL,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ NULL,
    /* get_values_posthook       */ NULL,
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ FALSE,
    /* widget_navigable          */ NULL,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

XmDialogShellClassRec xmDialogShellClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &transientShellClassRec,
        /* class_name            */ "XmDialogShell",
        /* size                  */ sizeof(XmDialogShellRec),
        /* class_initialize      */ class_initialize,
        /* class_part_initialize */ class_part_initialize,
        /* class_inited          */ FALSE,
        /* initialize            */ initialize,
      	/* initialize_hook       */ NULL,		
      	/* realize               */ XtInheritRealize,
      	/* actions               */ NULL,
      	/* num_actions           */ 0,
      	/* resources             */ NULL,
      	/* resource_count        */ 0,
      	/* xrm_class             */ NULLQUARK,
      	/* compress_motion       */ FALSE,
      	/* compress_exposure     */ TRUE,
      	/* compress_enterleave   */ FALSE,
      	/* visible_interest      */ FALSE,
      	/* destroy               */ NULL,
      	/* resize                */ XtInheritResize,
      	/* expose                */ NULL,
      	/* set_values            */ NULL,
      	/* set_values_hook       */ NULL,			
      	/* set_values_almost     */ XtInheritSetValuesAlmost,  
      	/* get_values_hook       */ NULL,
      	/* accept_focus          */ NULL,
      	/* intrinsics version    */ XtVersion,
      	/* callback offsets      */ NULL,
      	/* tm_table              */ XtInheritTranslations,
      	/* query_geometry        */ XtInheritQueryGeometry,
      	/* display_accelerator   */ NULL,
      	/* extension             */ (XtPointer)&_XmDialogSCoreClassExtRec
    },
    /* Composite Class Part */
    {
	/* geometry_manager */ XtInheritGeometryManager,
        /* change_managed   */ change_managed,
        /* insert_child     */ InsertChild,
        /* delete_child     */ DeleteChild,
        /* extension        */ NULL
    },
    /* Shell Class Part */
    {
	/* extension */ NULL
    },
    /* WMShell Class Part*/
    {
	/* extension */ NULL
    },
    /* Vendor Shell Class */
    {
	/* extension */	NULL
    },
    /* TransientShell Class Part */
    {
	/* extension */ NULL
    },
    /* XmDialogShell Class Part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmDialogShellWidgetClass = (WidgetClass) (&xmDialogShellClassRec);

/*
 * resources 
 */
static XtResource ext_resources[] = {
    {
        XmNdeleteResponse, XmCDeleteResponse, XmRDeleteResponse,
        sizeof(unsigned char), XtOffsetOf(XmDialogShellExtRec, vendor.delete_response),
        XmRImmediate, (XtPointer)XmUNMAP
    },
};

XmDialogShellExtClassRec xmDialogShellExtClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmVendorShellExtClassRec,
        /* class_name            */ "XmDialogShellExtClass",
	/* widget_size           */ sizeof(XmDialogShellExtRec),
	/* class_initialize      */ NULL,
	/* class_part_initialize */ NULL,
	/* class_inited          */ FALSE,
	/* initialize            */ NULL,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ ext_resources,
	/* num_resources         */ XtNumber(ext_resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ 0,
	/* compress_exposure     */ 0,
	/* compress_enterleave   */ 0,
	/* visible_interest      */ 0,
	/* destroy               */ NULL,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ NULL,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
        /* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* XmExtObject part */
    {
        /* syn_resources      */ NULL,
        /* num_syn_resources  */ 0,
        /* extension          */ NULL
    },
    /* Desktop Class part */
    {
        /* child_class           */ NULL,
        /* insert_child          */ XmInheritWidgetProc,
        /* delete_child          */ XmInheritWidgetProc,
        /* extension             */ NULL
    },
    /* ShellExt Class part */
    {
	/* structure_notify   */ StructureNotifyHandler,
	/* extension          */ NULL
    },
    /* VendorClassExt Part */
    {
	/* delete_window_handler */ WmProtocolHandler,
	/* offset_handler        */ NULL,
	/* extension             */ NULL
    },
    /* DialogShellExt Part */
    {
	/* extension          */ NULL
    }
};

WidgetClass xmDialogShellExtObjectClass = (WidgetClass)&xmDialogShellExtClassRec;

static void class_initialize()
{
    _XmDialogSCoreClassExtRec.record_type = XmQmotif;
}

static void class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmDIALOG_SHELL_BIT);
}

static void 
initialize(Widget request, 
	   Widget new_w, 
	   ArgList args, 
	   Cardinal *num_args)
{
    Widget par;

    par = _XmFindTopMostShell(XtParent(new_w));
    if (XtIsRealized(par)) {
	Arg args[2];
	int argc = 0;

	XtSetArg(args[argc], XmNtransientFor, par); argc++;
	XtSetArg(args[argc], XmNwindowGroup, XtWindow(par)); argc++;
	XtSetValues(new_w, args, argc);
    }
    if (XtWidth(new_w) == 0)
	XtWidth(new_w) = 1;
    if (XtHeight(new_w) == 0)
	XtHeight(new_w) = 1;
}

static Widget
GetChild(Widget w)
{
	CompositeWidget		cw = (CompositeWidget)w;
	int			i;
	extern WidgetClass	xmVendorShellExtObjectClass;	/* FIX ME */
/*
 * LessTif implementation dependency : vendor shell extension objects must
 * be ignored
 */
	for (i=0; i<cw->composite.num_children; i++)
		if ((! XtIsSubclass(cw->composite.children[i], xmVendorShellExtObjectClass))
		 && (! XtIsSubclass(cw->composite.children[i], xmDisplayClass)))
			return cw->composite.children[i];
	return NULL;
}

static void 
change_managed(Widget w)
{
    XmDialogShellWidget	sw = (XmDialogShellWidget)w;
    Widget child;
    XmBulletinBoardWidget bb;

    child = GetChild(w);
    bb = (XmBulletinBoardWidget)child;

    XdbDebug2(__FILE__, w, child, "ChangeManaged\n");

    /* not doing this was why popup dialogs wouldn't take focus */
    XtSetKeyboardFocus(w, child);

    if (XtIsManaged(child)) {

	XdbDebug2(__FILE__, w, child, "... was not managed\n");
	if (! XtIsRealized(child)) {
	    XdbDebug(__FILE__, child, "... Realizing\n");

	    XtRealizeWidget(child);
	}

	/* Pick up child size */
	(void) XtMakeResizeRequest(w, XtWidth(child), XtHeight(child),
				   NULL, NULL);
	XdbDebug2(__FILE__, w, child, "Shell size %d %d\n",
		  XtWidth(child), XtHeight(child));

	if (XmIsBulletinBoard(child) && BB_DefaultPosition(child))
	{
	    Position	px, py;
	    Widget	p;

	    p = XtParent(w);

	    px = XtX(p) + (XtWidth(p) - XtWidth(child)) / 2;
	    py = XtY(p) + (XtHeight(p) - XtHeight(child)) / 2;

	    XdbDebug(__FILE__, w, "def. pos. %d %d\n", px, py);

	    if (px < 0)
		px = 0;
	    if (py < 0)
		py = 0;

	    XtMoveWidget(w, px, py);
	}
	if (sw->core.mapped_when_managed) {
	    XdbDebug(__FILE__, (Widget)sw, "XtPopup\n");
	    XtPopup((Widget)sw, XtGrabNone);
	}

	XdbDebug2(__FILE__, w, child, "Mapping shell, just to be sure\n");
	XtMapWidget(w);
    }
    else {
#if 1
	XdbDebug(__FILE__, (Widget)sw, "XtUnmapWidget\n");
	XtUnmapWidget((Widget)sw);
#else
	XdbDebug(__FILE__, (Widget)sw, "XtPopDown\n");
	XtPopdown((Widget)sw);
#endif
    }
}

static void
InsertChild(Widget w)
{
    CompositeWidget	p = (CompositeWidget) XtParent(w);

#define superclass (&transientShellClassRec)    
    (*superclass->composite_class.insert_child)(w);
#undef superclass

    XdbDebug2(__FILE__, (Widget)p, w, "InsertChild\n");

    /* Avoid nasty side effects with the shell extension object */
    if (!XtIsRectObj(w))
	return;

    if (!XtIsRealized((Widget)p)) {
	/*
	 * Avoid Xt errors on zero width/height here by
	 * temporarily setting p's width/height to 1 and
	 * restoring them after realize.
	 */
	Dimension	ww, hh;

	ww = XtWidth(p);
	hh = XtHeight(p);

	XtWidth(p) = XtHeight(p) = 1;

	XtRealizeWidget((Widget)p);

	XtWidth(p) = ww;
	XtHeight(p) = hh;
    }
}

static void
DeleteChild(Widget w)
{
    Widget	s = XtParent(w);

    XdbDebug2(__FILE__, s, w, "DeleteChild\n");

    if (!XtIsRectObj(w))
	return;

    /*
     * The XtIsManaged part is probably never true. Should check Xt manuals.
     * When we have two children, this means only one will be left. We know
     * that's the extension object, so unmap ourselves.
     */
    if (XtIsManaged(w) || MGR_NumChildren(s) == 1) {
#if 1
	XdbDebug(__FILE__, s, "XtUnmapWidget\n");
	XtUnmapWidget(s);
#else
	XdbDebug(__FILE__, s, "XtPopdown\n");
	XtPopdown(s);
#endif
    }

#define superclass (&transientShellClassRec)
    (*superclass->composite_class.delete_child)(w);
#undef superclass
}

static void
WmProtocolHandler(Widget w, XtPointer client, XtPointer call)
{
    XmVendorShellExtObject	ve = (XmVendorShellExtObject)client;
    int				i;

    XdbDebug(__FILE__, w, "Dialog's WmProtocolHandler\n");

    switch (VSEP_DeleteResponse(ve)) {
	case XmDESTROY:
	    XtDestroyWidget(w);
	    XdbDebug(__FILE__, w,
		     "WmProtocolHandler(DeleteResponse XmDESTROY)\n");
	    break;
	case XmUNMAP:
	    /* The word says UNMAP but we really have to unMANAGE */
	    for (i=0; i<MGR_NumChildren(w); i++)
		if (XtIsManaged(MGR_Children(w)[i])) {
		    XdbDebug2(__FILE__, w, MGR_Children(w)[i],
			      "XtUnmanageChild(child)\n");
		    XtUnmanageChild(MGR_Children(w)[i]);
		    return;
		}
	    break;
	case XmDO_NOTHING:
	    XdbDebug(__FILE__, w,
		     "WmProtocolHandler(DeleteResponse XmNO_NOTHING)\n");
	    return ;
    }
}

String
_XmMakeDialogName(String name)
{
    String s;

    s = XtMalloc((name ? strlen(name) : 0) + strlen(XmDIALOG_SUFFIX) + 1);
    if (name)
        strcpy(s, name);
    else
        s[0] = '\0';
    strcat(s, XmDIALOG_SUFFIX);

    return s;
}

Widget
XmCreateDialogShell(Widget parent,
		    char *name,
		    Arg *arglist,
		    Cardinal argcount)
{
    Widget composite_parent;

    /*
     * First we find the first widget (starting at the parent argument)
     * that is a composite subclass. We use this as the parent when
     * creating the shell
     * Correction: find shell parent, so that we can correctly set
     * transientFor. -- MLM
     */
    composite_parent = parent;
    while (!XtIsVendorShell(composite_parent))
	composite_parent = XtParent(composite_parent);

    return XtCreatePopupShell(name,
			      xmDialogShellWidgetClass,
			      composite_parent,
			      arglist,
			      argcount);
}

static void
StructureNotifyHandler(Widget w, XtPointer closure,
			 XEvent *event, Boolean *cont)
{
    if (!XtIsSubclass(w, xmDialogShellWidgetClass))
	return;

    XdbDebug(__FILE__, w, "STRUCTURE NOTIFY: %d <%s>\n",
	event->type,
	(event->type == 22) ? "ConfigureNotify" :
	(event->type == 21) ? "ReparentNotify" :
	(event->type == 19) ? "MapNotify" : "??");
}
