/**
 *
 * $Id: Display.c,v 1.7 1996/11/28 09:21:00 u27113 Exp $
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

static char rcsid[] = "$Id: Display.c,v 1.7 1996/11/28 09:21:00 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>
#include <XmI/AtomMgrI.h>

#include <Xm/XmP.h>
#include <Xm/DisplayP.h>

#include <XmI/DebugUtil.h>

#define Offset(field) XtOffsetOf(XmDisplayRec, display.field)

static XtResource resources[] = {
    {
	XmNdropSiteManagerClass, XmCDropSiteManagerClass, XmRWidgetClass,
	sizeof(WidgetClass), Offset(dropSiteManagerClass),
	XmRImmediate, (XtPointer)NULL
	/* Motif has XmRImmediate, (XtPointer)wacky */
    },
    {
	XmNdropTransferClass, XmCDropTransferClass, XmRWidgetClass,
	sizeof(WidgetClass), Offset(dropTransferClass),
	XmRImmediate, (XtPointer)NULL
	/* Motif has XmRImmediate, (XtPointer)wacky */
    },
    {
	XmNdragContextClass, XmCDragContextClass, XmRWidgetClass,
	sizeof(WidgetClass), Offset(dragContextClass),
	XmRImmediate, (XtPointer)NULL
	/* Motif has XmRImmediate, (XtPointer)wacky */
    },
    {
	XmNdragInitiatorProtocolStyle, XmCDragInitiatorProtocolStyle, XmRDragInitiatorProtocolStyle,
	sizeof(unsigned char), Offset(dragInitiatorProtocolStyle),
	XmRImmediate, (XtPointer)XmDRAG_PREFER_RECEIVER
    },
    {
	XmNdragReceiverProtocolStyle, XmCDragReceiverProtocolStyle, XmRDragReceiverProtocolStyle,
	sizeof(unsigned char), Offset(dragReceiverProtocolStyle),
	XmRImmediate, (XtPointer)XmDRAG_PREFER_PREREGISTER
    },
    {
	XmNdefaultVirtualBindings, XmCString, XmRString,
	sizeof(String), Offset(bindingsString),
	XmRImmediate, (XtPointer)NULL /* adhere to compatibility! */
    }
};

static void DisplayClassInitialize();
static void DisplayClassPartInitialize(WidgetClass w_class);
static void DisplayInitialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void DisplayDestroy(Widget w);
static Boolean DisplaySetValues(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);

static Widget GetXmDisplay(Display *Dsp);

/*
 * This one is needed for the automagically refreshing of the modifier
 * mappings.
 */
char _XmDisplay_Translations[] =
    "<Mapping>: RefreshMapping()";

static void RefreshMapping(Widget w, XEvent *event, String *Params, Cardinal *NumParams);

static XtActionsRec actions[] = {
    { "RefreshMapping", RefreshMapping },
};

static XmBaseClassExtRec _XmDisplayCoreClassExtRec = {
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
    /* widget_navigable          */ NULL,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

XmDisplayClassRec xmDisplayClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &applicationShellClassRec,
        /* class_name            */ "XmDisplay",
	/* widget_size           */ sizeof(XmDisplayRec),
	/* class_initialize      */ DisplayClassInitialize,
	/* class_part_initialize */ DisplayClassPartInitialize,
	/* class_inited          */ FALSE,
	/* initialize            */ DisplayInitialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMultiple,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ DisplayDestroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ DisplaySetValues,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmDisplayCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ NULL,
        /* change_managed   */ NULL,
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ NULL,	
    },
    /* Shell class part */
    {
	/* extension        */ NULL,
    },
    /* WMShell class part */
    {
	/* extension        */ NULL,
    },
    /* vendor shell class part */
    {
	/* extension        */ NULL,
    },
    /* toplevel shell class part */
    {
	/* extension        */ NULL,
    },
    /* application shell class part */    
    {
	/* extension        */ NULL,
    },
    /* display class part */    
    {
        /* GetDisplay       */ GetXmDisplay,
	/* extension        */ NULL,
    }
};

WidgetClass xmDisplayClass = (WidgetClass) &xmDisplayClassRec;

/*
 * Following is all that stuff (variables) that is needed in order to put
 * the management of XmDisplay widgets per display to live.
 */
#define PDWC_None               ((XContext) 0)
#define PDWC_RID_DisplayWidget  ((XID) 0)

static XContext PerDisplayWidgetContext = PDWC_None;

/*
 * This one is needed due to the internal functions _XmGetXmDisplayClass()
 * and _XmSetXmDisplayClass(). This is one of the reasons why I'm regarding
 * ISO 9000 for software as a bad joke. --aldi
 */
static WidgetClass __XmDisplayClass = (WidgetClass) &xmDisplayClassRec;

String _Xm_MOTIF_DRAG_AND_DROP_MESSAGE = "_MOTIF_DRAG_AND_DROP_MESSAGE";

/*
 * This is a hook for clean-up whenever our display connection gets closed.
 * In reaction to a close we clean-up ourself and all our children.
 * WARNING: The widget ID of the XmDisplay widget is contained in the
 * ClientData parameter and not in the parameter "w"!
 */
static void
DisplaySuicide(Widget w, XtPointer ClientData, XtPointer CallData)
{
    Display *Dsp;

    XdbDebug(__FILE__, (Widget) ClientData,
	     "XmDisplay Connection Close-Down\n");
    Dsp = XtDisplay((Widget) ClientData);
    XtDestroyWidget((Widget) ClientData);
    _XmFlushAtomsForDisplay(Dsp); /* do this as last operation */
} /* DisplaySuicide */

/*
 * _XmAddCloseConnectionCallback registers a callback on the widget w
 * that is triggered by a close-down on the connection the widget is
 * belonging to.
 */
static void
_XmAddCloseConnectionCallback(Widget w, XtCallbackProc callback)
{
#if XtSpecificationRelease >= 6
    XtAddCallback(XtHooksOfDisplay(XtDisplay(w)), XmNdestroyCallback,
                  callback, (XtPointer) w);
#endif
} /* _XmAddCloseConnectionCallback */


/*
 * The widget class methods...
 */
static void
DisplayClassInitialize()
{
    _XmDisplayCoreClassExtRec.record_type = XmQmotif;
}

/*
 * Whenever the XmDisplay class or one of it's subclasses gets initialized,
 * we setup the default proc for getting a display's XmDisplay widget. This
 * way, subclasses can set another default proc within their class init
 * method (or create a chain).
 */
static void
DisplayClassPartInitialize(WidgetClass widget_class)
{
    if (((XmDisplayClassRec *)widget_class)->display_class.GetDisplay == NULL) {
        ((XmDisplayClassRec *)widget_class)->display_class.GetDisplay = 
            GetXmDisplay;
    }

    _XmFastSubclassInit(widget_class, XmDISPLAY_BIT);
}

/*
 * Warn the user about double XmDisplay instances for the same display. This
 * can otherwise cause trouble and many tears... Yes, programmers do have
 * feelings.
 */
static void
DisplayInitialize(Widget request,
	          Widget new_w,
	          ArgList args,
	          Cardinal *num_args)
{
    XPointer FirstDisplayWidget;
    XmDisplay dpy = (XmDisplay)new_w;

    XdbDebug(__FILE__, new_w, "XmDisplay Initialize\n");

    dpy->display.shellCount = 0;
    dpy->display.numModals  = 0;
    dpy->display.maxModals  = 0;
    dpy->display.modals     = NULL;

    /*
     * If haven't yet allocated the context with all kind of information
     * about LessTif goodies we'll do it right now. This context contains
     * LessTif goodies on a per display basis.
     */
    if ( PerDisplayWidgetContext == PDWC_None )
        PerDisplayWidgetContext = XUniqueContext();
    /*
     * Make sure, that there hasn't already allocated another XmDisplay
     * widget. Then register this widget as the XmDisplay widget for the
     * appropiate display.
     */
    if ( XFindContext(XtDisplay(new_w), PDWC_RID_DisplayWidget,
                      PerDisplayWidgetContext, &FirstDisplayWidget)
         == XCSUCCESS ) {
        _XmWarning(new_w, "Don't create multiple XmDisplays for one Display");
    } else {
        XSaveContext(XtDisplay(new_w), PDWC_RID_DisplayWidget,
                     PerDisplayWidgetContext, (XPointer) new_w);
    }
    /*
     * ...
     */
    _XmMessageBoxInstallImages(new_w);
    _XmInstallStippleImages(new_w);

    /*
     * Allocate space for the virtual key bindings and then fetch the
     * translations for the convertion from keys to csf keys. This is all
     * done in the following proc call.
     */
    ((XmDisplayRec *) new_w)->display.bindings = NULL;
    _XmVirtKeysInitialize(new_w);
    /*
     * Register ourself with the current connection so we can kill ourself
     * when the connection is closed.
     */
    _XmAddCloseConnectionCallback(new_w, DisplaySuicide);
}

/*
 * Don't waste your environment -- clean up everthing and recycle it.
 */
static void
DisplayDestroy(Widget w)
{
    if ( ((XmDisplayRec *) w)->display.bindings )
        XtFree((char *) ((XmDisplayRec *) w)->display.bindings);
    /* ... and get rid of old atom id's! */
    _XmFlushAtomsForDisplay(XtDisplay(w));
}

/*
 * There is almost nothing dto do -- yet.
 */
static Boolean
DisplaySetValues(Widget old,
	         Widget request,
	         Widget new_w,
	         ArgList args,
	         Cardinal *num_args)
{
	return	False;
}

/*
 * This is the real place, where we try to find the XmDisplay widget of
 * a display. If we can't get our hands on the widget, we create one.
 *
 * NOTICE:
 * We now support for user-defined XmDisplay subclasses. Such user-defined
 * replacements can be activated by calling _XmSetXmDisplayClass with a
 * pointer to an appropiate class record.
 */
static Widget GetXmDisplay(Display *Dsp)
{
    Widget   DisplayWidget;
    Arg      Args[5];
    Cardinal ArgCount;
    String   AppName, AppClass;
    
    /*
     * If we haven't yet a XmDisplay widget for that particular display,
     * we're creating it on the fly.
     */
    if ( (PerDisplayWidgetContext == PDWC_None) ||
         (XFindContext(Dsp, PDWC_RID_DisplayWidget, PerDisplayWidgetContext, 
                       (XPointer *) &DisplayWidget)
          != XCSUCCESS) ) {
        /*
         * Because we need to realize that display widget in the next step,
         * we have to set the widget's initial size -- otherwise we will
         * crash with that nasty error message saying, that the shell has
         * zero with/height...
         */
        ArgCount = 0;
        XtSetArg(Args[ArgCount], XmNwidth, 1); ArgCount++;
        XtSetArg(Args[ArgCount], XmNheight, 1); ArgCount++;
        XtSetArg(Args[ArgCount], XmNmappedWhenManaged, False); ArgCount++;
        XtGetApplicationNameAndClass(Dsp, &AppName, &AppClass);
        DisplayWidget = XtAppCreateShell(AppName, AppClass, __XmDisplayClass, 
                                         Dsp, Args, ArgCount);
        /*
         * The XmDisplay widget is now already registered as the display
         * object (this is done in DisplayInitialize()).
         */
    }
    /*
     * Make sure we have a window.
     */
    if ( !XtIsRealized(DisplayWidget) ) {
        XtRealizeWidget(DisplayWidget);
    }
    return DisplayWidget;
}

XmDropSiteManagerObject
_XmGetDropSiteManagerObject(XmDisplay xmDisplay)
{
    return NULL;
}

unsigned char
_XmGetDragProtocolStyle(Widget w)
{
    return 0;
}

unsigned char
_XmGetDragTrackingMode(Widget w)
{
    return 0;
}

Widget
_XmGetDragContextFromHandle(Widget w, Atom iccHandle)
{
    return NULL;
}

/*
 * This is just there, so one can install her/his own new XmDisplay widget 
 * subclass. It does not make the design clean, but we've to obey the
 * compatibility.
 */
WidgetClass _XmGetXmDisplayClass(void)
{
    if ( __XmDisplayClass == NULL )
        __XmDisplayClass = (WidgetClass) &xmDisplayClassRec;
    return __XmDisplayClass;
}

/*
 * Install a new XmDisplay subclass, so application developers can replace
 * the default XmDisplay widget class with their own one. The function
 * checks, that the specified widget class is indeed a subclass of the
 * XmDisplay widget class.
 */
WidgetClass _XmSetXmDisplayClass(WidgetClass wc)
{
    WidgetClass OldDisplayClass, SuperClass;
    
    OldDisplayClass = __XmDisplayClass;
    SuperClass      = wc;
    while ( (SuperClass != NULL) && 
            (SuperClass != (WidgetClass) &xmDisplayClassRec) )
        SuperClass = SuperClass->core_class.superclass;
    if ( SuperClass == NULL ) {
        _XmWarning(NULL, "\
_XmSetXmDisplayClass(): Thou shall not set the XmDisplay class to\n\
non-subclass of XmDisplay.");
    } else {
        __XmDisplayClass = wc;
    }
    return OldDisplayClass;
}

Widget
XmGetDragContext(Widget w, Time time)
{
    return NULL;
}

/*
 * This thing should create the XmDisplay widget...
 *
 * It is called from lots of places, but certainly from the initialize
 * method of VendorShell. Therefore, we always have a XmDisplay widget
 * created for every display.
 *
 */
Widget XmGetXmDisplay(Display *Dsp)
{
    return ((XmDisplayClassRec *)_XmGetXmDisplayClass())->
               display_class.GetDisplay(Dsp);
}

/*
 * This action is triggered whenever a MappingNotify event happens on our
 * display's wire. Then we will invalidate the cached modifier mappings
 * and reload the virtual keysym stuff. This way LessTif applications --
 * contrary to some software foundations' stuff -- can be reconfigured
 * during run-time.
 */
static void RefreshMapping(Widget w, XEvent *event, 
                           String *Params, Cardinal *NumParams)
{
    /*
     * NOTE: w is always NULL with mapping notifications! So we just
     * look up the offending display widget and refresh the virtual
     * key bindings. Happy refreshing...
     */
    XdbDebug(__FILE__, w, "Refreshing virtual key bindings.\n");
    _XmRefreshVirtKeys(XmGetXmDisplay(event->xmapping.display));
} /* RefreshMapping */
