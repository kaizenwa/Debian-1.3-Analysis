/**
 *
 * $Id: Screen.c,v 1.8 1996/12/09 18:01:14 miers Exp $
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

static char rcsid[] = "$Id: Screen.c,v 1.8 1996/12/09 18:01:14 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuUtilP.h>
#include <Xm/DisplayP.h>
#include <Xm/ScreenP.h>

#include <XmI/DebugUtil.h>

#ifndef XmUNSPECIFIED
#define XmUNSPECIFIED (~0)
#endif

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes);
static void expose(Widget w, XEvent *event, Region region);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);

static void ScreenInsertChild(Widget w);
static void ScreenDeleteChild(Widget w);

/*
 * Resources for the primitive class
 */
#define Offset(field) XtOffsetOf(XmScreenRec, screen.field)
static XtResource resources[] = {
    {
	XmNdarkThreshold, XmCDarkThreshold, XmRInt,
	sizeof(int), Offset(darkThreshold),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNlightThreshold, XmCLightThreshold, XmRInt,
	sizeof(int), Offset(lightThreshold),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNforegroundThreshold, XmCForegroundThreshold, XmRInt,
	sizeof(int), Offset(foregroundThreshold),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNdefaultNoneCursorIcon, XmCDefaultNoneCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultNoneCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultValidCursorIcon, XmCDefaultValidCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultValidCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultInvalidCursorIcon, XmCDefaultInvalidCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultInvalidCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultMoveCursorIcon, XmCDefaultMoveCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultMoveCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultLinkCursorIcon, XmCDefaultLinkCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultLinkCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultCopyCursorIcon, XmCDefaultCopyCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultCopyCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdefaultSourceCursorIcon, XmCDefaultSourceCursorIcon, XmRWidget,
	sizeof(XmDragIconObject), Offset(defaultSourceCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNmenuCursor, XmCCursor, XmRCursor,
	sizeof(Cursor), Offset(menuCursor),
	XmRString, (XtPointer)"arrow"
    },
    {
	XmNunpostBehavior, XmCUnpostBehavior, XmRUnpostBehavior,
	sizeof(unsigned char), Offset(unpostBehavior),
	XmRImmediate, (XtPointer)XmUNPOST_AND_REPLAY
    },
    {
	XmNfont, XmCFont, XmRFontStruct,
	sizeof(XFontStruct *), Offset(font_struct),
	XmRString, (XtPointer)"Fixed"
    },
    {
	XmNhorizontalFontUnit, XmCHorizontalFontUnit, XmRInt,
	sizeof(int), Offset(h_unit),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNverticalFontUnit, XmCVerticalFontUnit, XmRInt,
	sizeof(int), Offset(v_unit),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNmoveOpaque, XmCMoveOpaque, XmRBoolean,
	sizeof(Boolean), Offset(moveOpaque),
	XmRImmediate, (XtPointer)False
    },
};

static XmBaseClassExtRec _XmScreenCoreClassExtRec = {
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

XmScreenClassRec xmScreenClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &coreClassRec,
        /* class_name            */ "XmScreen",
	/* widget_size           */ sizeof(XmScreenRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmScreenCoreClassExtRec
    },
    /* Desktop Class part */
    {
        /* child_class           */ NULL,
        /* insert_child          */ ScreenInsertChild,
        /* delete_child          */ ScreenDeleteChild,
        /* extension             */ NULL
    },
    /* Screen Class part */
    {
        /* extension             */ NULL
    }
};

WidgetClass xmScreenClass = (WidgetClass)&xmScreenClassRec;

/*
 * helper function for font_unit stuff
 */
static void
_XmComputeFontUnits(Widget w, XFontStruct *font, int *h_unit, int *v_unit)
{
    XFontProp *props;
    int i, nprops;
    Boolean got1;
    Atom atom;

    if (font == NULL) {
       *h_unit = 10;
       *v_unit = 10;
       return;
    }
    props = font->properties;
    nprops = font->n_properties;

    /* we calculate the value in reverse order as specified the programmer's
     * reference; we let the higher precedence rules to override the lower
     * ones. */
    /* HUNITS */
    *h_unit = (int)((font->min_bounds.width + font->max_bounds.width) / 2.3);
    for (i = 0; i < nprops; i++) {
       if (props[i].name == XA_QUAD_WIDTH) {
           *h_unit = props[i].card32;
           break;
       }
    }
    atom = XInternAtom(XtDisplay(w), "AVERAGE_WIDTH", False);
    for (i = 0; i < nprops; i++) {
       if (props[i].name == atom) {
           *h_unit = props[i].card32 / 10;
           break;
       }
    }

    /* VUNITS */
    *v_unit = (int)((font->ascent + font->descent) / 2.2);
    got1 = False;
    for (i = 0; i < nprops; i++) {
       if (props[i].name == XA_POINT_SIZE) {
           got1 = True;
           *v_unit = props[i].card32;
           break;
       }
    }
    if (got1) {
       atom = XInternAtom(XtDisplay(w), "RESOLUTION_Y", False);
       for (i = 0; i < nprops; i++) {
           if (props[i].name == atom) {
               *v_unit = (*v_unit * props[i].card32) / 1400;
               break;
           }
       }
    }
    atom = XInternAtom(XtDisplay(w), "PIXEL_SIZE", False);
    for (i = 0; i < nprops; i++) {
       if (props[i].name == atom) {
           *v_unit = (int)(props[i].card32 / 1.8);
           break;
       }
    }
}

/*
 * MLM - pretty much ripped off from Display.c
 * Following is all that stuff (variables) that is needed in order to put
 * the management of XmScreen widgets per screen to live.
 */
#define PSWC_None               ((XContext) 0)
static XContext PerScreenWidgetContext = PSWC_None;

static void
class_initialize()
{
    _XmScreenCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmSCREEN_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XtPointer FirstScreenWidget;

    /*
     * This is due to the %&"% children management mechanism needed for the
     * shell "shadow hierarchy". See Desktop.c for more details on this.
     */
    Screen_NumChildren(new_w) = 0;
    Screen_NumSlots(new_w)    = 0;
    Screen_Children(new_w)    = NULL;

    /*
     * If haven't yet allocated the context with all kind of information
     * about LessTif goodies we'll do it right now. This context contains
     * LessTif goodies on a per display basis.
     */
    if (PerScreenWidgetContext == PSWC_None)
        PerScreenWidgetContext = XUniqueContext();

    /*
     * Make sure that there hasn't already allocated another XmScreen
     * widget. Then register this widget as the XmScreen widget for the
     * appropiate screen.
     */
    if (XFindContext(XtDisplay(new_w), RootWindowOfScreen(XtScreen(new_w)),
		     PerScreenWidgetContext, (XtPointer)&FirstScreenWidget)
         == XCSUCCESS ) {
        _XmError(new_w, "Attempt to create a second XmScreen widget.");
    }
    else {
        XSaveContext(XtDisplay(new_w), RootWindowOfScreen(XtScreen(new_w)),
                     PerScreenWidgetContext, (XPointer)new_w);
    }

    Screen_MwmPresent(new_w) = XmIsMotifWMRunning(new_w);

    _XmComputeFontUnits(new_w, Screen_FontStruct(new_w),
			&Screen_HorizUnit(new_w), &Screen_VertUnit(new_w));

    Screen_ScratchPixmaps(new_w) = NULL;
    Screen_ScreenInfo(new_w) = NULL;
}

static void
destroy(Widget w)
{
    XmScratchPixmap pix, tmp;

    XtFree((char *) Screen_Children(w));
    for (pix = Screen_ScratchPixmaps(w); pix != NULL; pix = tmp) {
	tmp = pix->next;
	XFreePixmap(XtDisplay(w), pix->pixmap);
	XtFree((char *)pix);
    }
}

static void
ScreenInsertChild(Widget w)
{
    Widget MeTheParent;

    /*
     * The next access is really FINE. The child to be inserted must always
     * be of class xmDesktopClass.
     */
    MeTheParent = Desktop_Parent(w);
    /*
     * Make free room for the new child, if necessary.
     */
    if ( Screen_NumChildren(MeTheParent) == Screen_NumSlots(MeTheParent) ) {
        Screen_NumSlots(MeTheParent) += Screen_NumSlots(MeTheParent) / 2 + 2;
	Screen_Children(MeTheParent) = (WidgetList)
	    XtRealloc((char *) Screen_Children(MeTheParent),
                      sizeof(Widget) * Screen_NumSlots(MeTheParent));
    }
    Screen_Children(MeTheParent)[Screen_NumChildren(MeTheParent)] = w;
    Screen_NumChildren(MeTheParent)++;
} /* DesktopInsertChild */

static void
ScreenDeleteChild(Widget w)
{
    Widget     MeTheParent;
    WidgetList Children;
    int        NumChildren, i;

    MeTheParent = Desktop_Parent(w); /* We're working on a xmDesktop desc. */
    Children    = Screen_Children(MeTheParent);
    NumChildren = Screen_NumChildren(MeTheParent);
    for ( i = 0; i < NumChildren; i++ ) {
        if ( *Children == w ) {
	    for ( i++; i < NumChildren; i++ ) {
	        Children[0] = Children[1];
		Children++;
	    }
	    Screen_NumChildren(MeTheParent)--;
	    break;
	}
	Children++;
    }
} /* ScreenDeleteChild */


static void 
expose(Widget w, 
       XEvent *event, 
       Region region)
{
}

static void 
realize(Widget w, 
	XtValueMask *value_mask, 
	XSetWindowAttributes *attributes)
{
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean need_refresh = False;

    return need_refresh;
}

XmDragIconObject
_XmScreenGetOperationIcon(Widget w, unsigned char operation)
{
    return NULL;
}

XmDragIconObject
_XmScreenGetStateIcon(Widget w, unsigned char state)
{
    return NULL;
}

XmDragIconObject
_XmScreenGetSourceIcon(Widget w)
{
    return NULL;
}

Pixmap
_XmAllocScratchPixmap(XmScreen xmScreen,
		      Cardinal depth,
		      Dimension width,
		      Dimension height)
{
    XmScratchPixmap pix = NULL;

    for (pix = Screen_ScratchPixmaps(xmScreen); pix != NULL; pix = pix->next) {
	if (!pix->inUse && pix->depth == depth &&
	    pix->width == width && pix->height == height) {
	    pix->inUse = True;
	    return pix->pixmap;
	}
    }

    pix = (XmScratchPixmap)XtMalloc(sizeof(XmScratchPixmapRec));
    pix->inUse = True;
    pix->depth = depth;
    pix->width = width;
    pix->height = height;
    pix->pixmap = XCreatePixmap(XtDisplay((Widget)xmScreen),
				RootWindowOfScreen(XtScreen(xmScreen)),
				width, height, depth);    
    pix->next = Screen_ScratchPixmaps(xmScreen);
    Screen_ScratchPixmaps(xmScreen) = pix;
    return pix->pixmap;
}

void
_XmFreeScratchPixmap(XmScreen xmScreen, Pixmap pixmap)
{
    XmScratchPixmap pix;

    for (pix = Screen_ScratchPixmaps(xmScreen); pix != NULL; pix = pix->next) {
	if (pix->pixmap == pixmap)
	    pix->inUse = False;
    }
}

XmDragCursorCache *
_XmGetDragCursorCachePtr(XmScreen xmScreen)
{
    return &Screen_CursorCache(xmScreen);
}

void
_XmGetMaxCursorSize(Widget w,
		    Dimension *width,
		    Dimension *height)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(XtScreen(w));
    *width = Screen_MaxCursorWidth(scr);
    *height = Screen_MaxCursorHeight(scr);
}

Cursor
_XmGetNullCursor(Widget w)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(XtScreen(w));
    return Screen_NullCursor(scr);
}

/* Motif 2.* version of the above */
Cursor
XmeGetNullCursor(Widget w)
{
	return _XmGetNullCursor(w);
}

Cursor
_XmGetMenuCursorByScreen(Screen *screen)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(screen);
    return Screen_MenuCursor(scr);
}

Boolean
_XmGetMoveOpaqueByScreen(Screen *screen)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(screen);
    return Screen_MoveOpaque(screen);
}

unsigned char
_XmGetUnpostBehavior(Widget wid)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(XtScreen(wid));
    return Screen_UnpostBehavior(scr);
}

int
_XmGetFontUnit(Screen *screen, int dimension)
{
    XmScreen scr;

    scr = (XmScreen)XmGetXmScreen(screen);
    
    if (dimension == XmVERTICAL)
	return Screen_VertUnit(scr);
    return Screen_HorizUnit(scr);
}

void
_XmScreenRemoveFromCursorCache(XmDragIconObject icon)
{
}

XmScreenInfo *
_XmGetScreenInfo(Widget scr)
{
    if (!Screen_ScreenInfo(scr)) 
    {
	XmScreenInfo *info = (XmScreenInfo*)XtMalloc(sizeof(XmScreenInfo));
	XmMenuState state = (XmMenuState)XtMalloc(sizeof(XmMenuStateRec));

	state->MU_InDragMode = False;
	
	info->menu_state = (XtPointer)state;
	info->destroyCallbackAdded = False;

	Screen_ScreenInfo(scr) = info;
    }

    return Screen_ScreenInfo(scr);
}

Widget
XmGetXmScreen(Screen *scr)
{
    Widget disp;
    Widget screen;
    int argc;
    Arg args[5];
    char name[128];
    int  screen_no, max_screens;

    disp = XmGetXmDisplay(DisplayOfScreen(scr));

    if ((PerScreenWidgetContext == PSWC_None) ||
        (XFindContext(DisplayOfScreen(scr),
		      RootWindowOfScreen(scr),
		      PerScreenWidgetContext, 
                      (XPointer *)&screen) != XCSUCCESS) ) {
        argc = 0;
        XtSetArg(args[argc], XmNwidth, 1); argc++;
        XtSetArg(args[argc], XmNheight, 1); argc++;
        XtSetArg(args[argc], XmNmappedWhenManaged, False); argc++;

	max_screens = ScreenCount(DisplayOfScreen(scr));
	for ( screen_no = 0; screen_no < max_screens; screen_no++ ) {
	    if ( ScreenOfDisplay(DisplayOfScreen(scr), screen_no) ==
		   scr ) {
	        break;
	    }
	}
	sprintf(name, "screen%d", screen_no);

	screen = XtCreateManagedWidget(name, xmScreenClass, disp,
				       args, argc);
    }

    if (!XtIsRealized(screen))
        XtRealizeWidget(screen);

    return screen;
}

void
_XmSetDefaultBackgroundColorSpec(Screen *screen, String string)
{
	XdbDebug(__FILE__, NULL, "_XmSetDefaultBackgroundColorSpec(%s) is not implemented\n", string);
}
