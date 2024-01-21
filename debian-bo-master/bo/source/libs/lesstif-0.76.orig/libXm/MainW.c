/**
 *
 * $Id: MainW.c,v 1.10 1997/01/13 07:08:54 u27113 Exp $
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

static char rcsid[] = "$Id: MainW.c,v 1.10 1997/01/13 07:08:54 u27113 Exp $";

/*
 * Be optimistic in GeometryManager
 */
#define	OPTIMISTIC

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/BaseClassP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/RowColumnP.h>
#include <Xm/CommandP.h>
#include <Xm/MainWP.h>
#include <Xm/SeparatoG.h>
#include <Xm/SeparatoGP.h>
#include <X11/Xfuncs.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void resize(Widget w);
static void realize(Widget w, Mask *value_mask, XSetWindowAttributes *attributes);
static XtGeometryResult QueryGeometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static XtGeometryResult GeometryManager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply);
static void change_managed(Widget w);
static void InsertChild(Widget w);

static void _XmMainWindowLayout(Widget w, Boolean ParentResize, Widget child, Boolean TestMode, XtWidgetGeometry *childgeom);


/*
 * Resources for the MainWindow class
 */
#define Offset(field) XtOffsetOf(XmMainWindowRec, mwindow.field)
static XtResource resources[] = {
    {
	XmNcommandWindow, XmCCommandWindow, XmRWidget,
	sizeof(Widget), Offset(CommandWindow),
	XmRImmediate, NULL
    },
    {
	XmNcommandWindowLocation, XmCCommandWindowLocation, XmRCommandWindowLocation,
	sizeof(unsigned char), Offset(CommandLoc),
	XmRImmediate, (XtPointer)XmCOMMAND_ABOVE_WORKSPACE
    },
    {
	XmNmenuBar, XmCMenuBar, XmRWidget,
	sizeof(Widget), Offset(MenuBar),
	XmRImmediate, NULL
    },
    {
	XmNmessageWindow, XmCMessageWindow, XmRWidget,
	sizeof(Widget), Offset(Message),
	XmRImmediate, NULL
    },
    {
	XmNmainWindowMarginWidth, XmCMainWindowMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmainWindowMarginHeight, XmCMainWindowMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNshowSeparator, XmCShowSeparator, XmRBoolean,
	sizeof(Boolean), Offset(ShowSep),
	XtRImmediate, (XtPointer)False
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNmainWindowMarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmainWindowMarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    }
};

/* Add Actions and Translations -- FIX ME */

static XmBaseClassExtRec _XmMainWindowCoreClassExtRec = {
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
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

static XmManagerClassExtRec _XmMainWMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmMainWindowClassRec xmMainWindowClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmScrolledWindowClassRec,
        /* class_name            */ "XmMainWindow",
	/* widget_size           */ sizeof(XmMainWindowRec),
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
	/* compress_exposure     */ XtExposeCompressMultiple,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ XtInheritExpose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ 	QueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmMainWindowCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ GeometryManager, 
        /* change_managed   */ change_managed, 
        /* insert_child     */ InsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ NULL,
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,  /* FIX ME */
        /* subresource_count */ 0,     /* FIX ME */
        /* constraint_size   */ 0,     /* FIX ME */
        /* initialize        */ NULL,  /* FIX ME */
        /* destroy           */ NULL,  /* FIX ME */
        /* set_values        */ NULL,  /* FIX ME */
        /* extension         */ NULL,   /* FIX ME */
    },
    /* XmManager class part */
    {
	/* translations                 */ XmInheritTranslations,
	/* syn_resources                */ syn_resources,
	/* num_syn_resources            */ XtNumber(syn_resources),
	/* syn_constraint_resources     */ NULL,
	/* num_syn_constraint_resources */ 0,
	/* parent_process               */ XmInheritParentProcess,
	/* extension                    */ (XtPointer)&_XmMainWMClassExtRec
    },
    /* XmScrolledWindow part */
    {
	/* extension */ NULL,
    },
    /* XmMainWindow part */
    {
	/* extension */ NULL,	
    },
};

WidgetClass xmMainWindowWidgetClass = (WidgetClass)&xmMainWindowClassRec;

static void 
class_initialize()
{
    _XmMainWindowCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmMAIN_WINDOW_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
/* Revert setting from XmScrolledWindow's initialize */
    if (XtWidth(request) == 0)
	XtWidth(new_w) = 0;
    if (XtHeight(request) == 0)
	XtHeight(new_w) = 0;

    SW_MarginWidth(new_w) = MW_MarginWidth(new_w);
    SW_MarginHeight(new_w) = MW_MarginHeight(new_w);

    MW_Sep1(new_w) = (XmSeparatorGadget)XmCreateSeparatorGadget(new_w, "Separator1", args, *num_args);
    MW_Sep2(new_w) = (XmSeparatorGadget)XmCreateSeparatorGadget(new_w, "Separator2", args, *num_args);
    MW_Sep3(new_w) = (XmSeparatorGadget)XmCreateSeparatorGadget(new_w, "Separator3", args, *num_args);
    if (MW_ShowSep(new_w) == True) {
	XtManageChild((Widget)MW_Sep1(new_w));
	XtManageChild((Widget)MW_Sep2(new_w));
	XtManageChild((Widget)MW_Sep3(new_w));
    }

    _XmMainWindowLayout(new_w, True, NULL, False, NULL);
}


static void
destroy(Widget w)
{
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean refresh = False;
#define NE(x)	(x(old) != x(new_w))

    if (NE(MW_CommandWindow) ||
	NE(MW_CommandLoc) ||
	NE(MW_MenuBar) ||
	NE(MW_MessageWindow))
	refresh = True;


    if (NE(MW_ShowSep)) {
	if (MW_ShowSep(new_w)) {
	    XtManageChild((Widget)MW_Sep1(new_w));
	    XtManageChild((Widget)MW_Sep2(new_w));
	    XtManageChild((Widget)MW_Sep3(new_w));
	}
	else {
	    XtUnmanageChild((Widget)MW_Sep1(new_w));
	    XtUnmanageChild((Widget)MW_Sep2(new_w));
	    XtUnmanageChild((Widget)MW_Sep3(new_w));
	}

	refresh = True;
    }

    if (NE(MW_MarginHeight)) {
	SW_MarginHeight(new_w) = MW_MarginHeight(new_w);
	refresh = True;
    }

    if (NE(MW_MarginWidth)) {
	SW_MarginWidth(new_w) = MW_MarginWidth(new_w);
	refresh = True;
    }

    _XmMainWindowLayout(new_w, True, NULL, False, NULL);

    return refresh;
}

static void
resize(Widget w)
{
    XdbDebug(__FILE__, w, "Resize\n");

    _XmMainWindowLayout(w, False, NULL, False, NULL);
}

static void
realize(Widget w, Mask *value_mask, XSetWindowAttributes *attributes)
{
    XdbDebug(__FILE__, w, "Realize ...\n");

#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize)(w, value_mask, attributes);
#undef superclass

    _XmMainWindowLayout(w, True, NULL, False, NULL);

    XdbDebug(__FILE__, w, "Realize => size %d %d\n", XtWidth(w), XtHeight(w));
}

static XtGeometryResult 
QueryGeometry(Widget w, 
	       XtWidgetGeometry *preferred, 
	       XtWidgetGeometry *intended)
{
    XtWidgetGeometry            dbug;
    XtGeometryResult		r = XtGeometryYes;

    dbug = *preferred;

    _XmMainWindowLayout(w, False, w, True, preferred);

    if (intended)
	*intended = *preferred;

/* FIX ME - this is all wrong */
    if (intended == NULL) {
	if (XtWidth(w) != preferred->width || XtHeight(w) != preferred->height)
	    r =  XtGeometryAlmost;		/* Something's different */
	r =  XtGeometryNo;			/* Everything is ok */
    }
    else if (((intended->request_mode & CWWidth) && preferred->width >= intended->width)
     || ((intended->request_mode & CWHeight) && preferred->height >= intended->height)) {
	return	XtGeometryYes;
    }

    if ((intended->request_mode & CWWidth) && preferred->width != intended->width)
	r = XtGeometryAlmost;		/* Something's different */
    if ((intended->request_mode & CWHeight) && preferred->height != intended->height)
	r = XtGeometryAlmost;		/* Something's different */
    return r;
}

static XtGeometryResult
GeometryManager(Widget w,
		 XtWidgetGeometry *desired,
		 XtWidgetGeometry *allowed)
{
    XtWidgetGeometry wants;

    XdbDebug2(__FILE__, XtParent(w), w, "GeometryManager request %s\n",
	XdbWidgetGeometry2String(desired));

#define Wants(flag)     (desired->request_mode & flag)

    if (desired != NULL && allowed != NULL && desired != allowed)
	wants = *desired;
    else
	bzero((void *)&wants, sizeof(XtWidgetGeometry));

    if (Wants(XtCWQueryOnly)) {
	XdbDebug(__FILE__, w, "Geometry Mgr Query Only Unimplemented\n");
	return XtGeometryYes;
    }

    /*
     * We control the XY of all children.  Width/Height is all I care about
     */
    wants.request_mode = CWWidth | CWHeight;

#ifdef	OPTIMISTIC
    if (Wants(CWWidth))
	XtWidth(w) = desired->width;
    if (Wants(CWHeight))
	XtHeight(w) = desired->height;
#endif

#if 1
    /* This sucks - we're not supposed to be in test mode. */
    _XmMainWindowLayout(XtParent(w), False, w, True, &wants);
#else
    _XmMainWindowLayout(XtParent(w), True, w, False, &wants);
#endif

    if (allowed)
	*allowed = wants;

    if (Wants(CWX) && Wants(CWY) && !(Wants(CWWidth) && Wants(CWHeight)))
	return XtGeometryNo;

    if (Wants(CWWidth) && wants.width == desired->width &&
	Wants(CWHeight) && wants.height == desired->height)
	return XtGeometryYes;

    /*
     * Try this : if we have a difference, just report "Almost"
     */
    return XtGeometryAlmost;
#undef	Wants
}

static void
change_managed(Widget w)
{
    if (SW_HSB(w) && XtIsManaged(SW_HSB(w)))
	SW_HasHSB(w) = True;
    else
	SW_HasHSB(w) = False;
    if (SW_VSB(w) && XtIsManaged(SW_VSB(w)))
	SW_HasVSB(w) = True;
    else
	SW_HasVSB(w) = False;

    _XmMainWindowLayout(w, True, NULL, False, NULL);
}

/*
 * Note : we should NEVER resize the widget 'child' from here, if we're
 *	called from geometry_manager. That's why the child parameter is
 *	there !!
 */
#define	VALID(w)	(w != NULL && XtIsManaged(w))

static void
_XmMainWLayoutAbove(Widget w, Dimension curw, Dimension curh)
{
    Position starty, endy;
    Position	xx, yy;
    Dimension	ww, hh;

    XdbDebug(__FILE__, w, "LayoutAbove\n");
    /*
     * FIXME -- this layout algorithm is probably wrong.  I don't know which
     * child should win if the window is smaller than the asked for geometry
     */
    starty = MW_MarginHeight(w);
    endy = curh - MW_MarginHeight(w);
    if (VALID(MW_MenuBar(w))) {
	XdbDebug2(__FILE__, w, MW_MenuBar(w),
		  "MenuBar geometry X %d Y %d W %d H %d\n",
			   MW_MarginWidth(w), starty,
			   curw - 2 * MW_MarginWidth(w),
			   XtHeight(MW_MenuBar(w)));
	_XmConfigureObject(MW_MenuBar(w),
			   MW_MarginWidth(w), starty,
			   curw - 2 * MW_MarginWidth(w),
			   XtHeight(MW_MenuBar(w)),
			   XtBorderWidth(MW_MenuBar(w)));
	starty += XtHeight(MW_MenuBar(w));
    }

    if (VALID(MW_CommandWindow(w))) {
	if (MW_ShowSep(w)) {
	    if (MW_Sep1(w) && XtIsManaged((Widget)MW_Sep1(w))) {
		XdbDebug2(__FILE__, w, (Widget)MW_Sep1(w),
			  "Sep1 geometry X %d Y %d W %d Y %d\n",
					MW_MarginWidth(w), starty,
					curw - 2 * MW_MarginWidth(w),
					XtHeight(MW_Sep1(w)));

		_XmConfigureObject((Widget)MW_Sep1(w),
				   MW_MarginWidth(w), starty,
				   curw - 2 * MW_MarginWidth(w),
				   XtHeight(MW_Sep1(w)),
				   XtBorderWidth((Widget)MW_Sep1(w)));
		starty += XtHeight(MW_Sep1(w));
	    }
	}
	XdbDebug2(__FILE__, w, MW_CommandWindow(w),
		  "CommandWindow geometry X %d Y %d W %d Y %d\n",
			MW_MarginWidth(w), starty,
			curw - 2 * MW_MarginWidth(w),
			XtHeight(MW_CommandWindow(w)));

	_XmConfigureObject(MW_CommandWindow(w),
			   MW_MarginWidth(w), starty,
			   curw - 2 * MW_MarginWidth(w),
			   XtHeight(MW_CommandWindow(w)),
			   XtBorderWidth(MW_CommandWindow(w)));
	starty += XtHeight(MW_CommandWindow(w));
    }

    if (VALID(MW_MessageWindow(w))) {
	endy -= XtHeight(MW_MessageWindow(w));

	XdbDebug2(__FILE__, w, MW_MessageWindow(w),
		  "MessageWindow geometry X %d Y %d W %d Y %d\n",
			MW_MarginWidth(w), endy,
			curw - 2 * MW_MarginWidth(w),
			endy);

	_XmConfigureObject(MW_MessageWindow(w),
			   MW_MarginWidth(w), endy,
			   curw - 2 * MW_MarginWidth(w),
			   XtHeight(MW_MessageWindow(w)),
			   XtBorderWidth(MW_MessageWindow(w)));
	if (MW_ShowSep(w)) {
	    if (VALID(MW_Sep3(w))) {
		endy -= XtHeight((Widget)MW_Sep3(w));

		XdbDebug2(__FILE__, w, (Widget)MW_Sep3(w),
			  "Sep3 geometry X %d Y %d W %d Y %d\n",
			MW_MarginWidth(w),
			MW_MarginHeight(w),
			curw - 2 * MW_MarginWidth(w),
			XtHeight((Widget)MW_Sep3(w)));

		_XmConfigureObject((Widget)MW_Sep3(w),
				   MW_MarginWidth(w), endy,
				   curw - 2 * MW_MarginWidth(w),
				   XtHeight((Widget)MW_Sep3(w)),
				   XtBorderWidth((Widget)MW_Sep3(w)));
	    }
	}
    }

    if (VALID(SW_WorkWindow(w))) {
	if (MW_ShowSep(w)) {
	    if (VALID(MW_Sep2(w))) {
		XdbDebug2(__FILE__, w, (Widget)MW_Sep2(w),
			  "Sep2 geometry X %d Y %d W %d Y %d\n",
				MW_MarginWidth(w),
				MW_MarginHeight(w),
				curw - 2 * MW_MarginWidth(w),
				XtHeight((Widget)MW_Sep2(w)));

		_XmConfigureObject((Widget)MW_Sep2(w),
				   MW_MarginWidth(w), starty,
				   curw - 2 * MW_MarginWidth(w),
				   XtHeight((Widget)MW_Sep2(w)),
				   XtBorderWidth((Widget)MW_Sep2(w)));
		starty += XtHeight(MW_Sep2(w));
	    }
	}

	/* if we're treating this as a -ConfigureObject call, this
	 * should always be True. */
	SW_FromResize(w) = True;

	xx = MW_MarginWidth(w);
	yy = starty;
	ww = curw - 2 * MW_MarginWidth(w);
	hh = endy - starty;

	if (curw < 2 * MW_MarginWidth(w)) {
		XdbDebug2(__FILE__, w, SW_WorkWindow(w),
			"curw %d < 2 * MarginWidth = %d\n",
			curw, 2 * MW_MarginWidth(w));
	}
	if (endy < starty) {
		XdbDebug2(__FILE__, w, SW_WorkWindow(w),
			"endy %d < starty %d\n", endy, starty);
	}

	XdbDebug2(__FILE__, w, SW_WorkWindow(w),
		  "WorkWindow geometry X %d Y %d W %d H %d\n",
		  xx, yy, ww, hh);

/* For lack of better treatment, don't call this with negative args */
/* FIX ME */
	if (curw >= 2 * MW_MarginWidth(w) && endy >= starty)
		_XmScrolledWindowLayout(w, False, NULL, False, NULL,
				xx, yy, ww, hh);

	SW_FromResize(w) = False;
    }
}

static void
_XmMainWLayoutBelow(Widget w, Dimension curw, Dimension curh)
{
    Position	starty, endy;
    Position	xx, yy;
    Dimension	ww, hh;

    XdbDebug(__FILE__, w, "LayoutBelow\n");
    /*
     * FIXME -- this layout algorithm is probably wrong.  I don't know which
     * child should win if the window is smaller than the asked for geometry
     */
    starty = MW_MarginHeight(w);
    endy = curh - MW_MarginHeight(w);
    if (VALID(MW_MenuBar(w))) {
	XdbDebug2(__FILE__, w, MW_MenuBar(w),
		  "MenuBar geometry X %d Y %d W %d Y %d\n",
			   MW_MarginWidth(w), MW_MarginHeight(w),
			   curw - 2 * MW_MarginWidth(w),
			   XtHeight(MW_MenuBar(w)));

	_XmConfigureObject(MW_MenuBar(w),
			   MW_MarginWidth(w), starty,
			   curw - 2 * MW_MarginWidth(w),
			   XtHeight(MW_MenuBar(w)),
			   XtBorderWidth(MW_MenuBar(w)));
	starty += XtHeight(MW_MenuBar(w));
    }


    if (VALID(MW_MessageWindow(w))) {
	endy -= XtHeight(MW_MessageWindow(w));
	XdbDebug2(__FILE__, w, MW_MessageWindow(w),
		  "MessageWindow geometry X %d Y %d W %d Y %d\n",
			MW_MarginWidth(w),
			endy,
			curw - 2 * MW_MarginWidth(w),
			XtHeight(MW_MessageWindow(w)));

	_XmConfigureObject(MW_MessageWindow(w),
			   MW_MarginWidth(w), endy,
			   curw - 2 * MW_MarginWidth(w),
			   XtHeight(MW_MessageWindow(w)),
			   XtBorderWidth(MW_MessageWindow(w)));
	if (MW_ShowSep(w)) {
	    if (VALID(MW_Sep3(w))) {
		endy -= XtHeight((Widget)MW_Sep3(w));

		XdbDebug2(__FILE__, w, (Widget)MW_Sep3(w),
			  "Sep3 geometry X %d Y %d W %d Y %d\n",
				MW_MarginWidth(w),
				endy,
				curw - 2 * MW_MarginWidth(w),
				XtHeight((Widget)MW_Sep3(w)));

		_XmConfigureObject((Widget)MW_Sep3(w),
				   MW_MarginWidth(w), endy,
				   curw - 2 * MW_MarginWidth(w),
				   XtHeight((Widget)MW_Sep3(w)),
				   XtBorderWidth((Widget)MW_Sep3(w)));
	    }
	}
    }

    if (VALID(MW_CommandWindow(w))) {
	endy -= XtHeight(MW_CommandWindow(w));
	XdbDebug2(__FILE__, w, MW_CommandWindow(w),
		  "CommandWindow geometry X %d Y %d W %d Y %d\n",
			MW_MarginWidth(w),
			endy,
			curw - 2 * MW_MarginWidth(w),
			XtHeight(MW_CommandWindow(w)));

	_XmConfigureObject(MW_CommandWindow(w),
			   MW_MarginWidth(w), endy,
			   curw - 2 * MW_MarginWidth(w),
			   XtHeight(MW_CommandWindow(w)),
			   XtBorderWidth(MW_CommandWindow(w)));
	if (MW_ShowSep(w)) {
	    if (VALID(MW_Sep2(w))) {
		endy -= XtHeight(MW_Sep2(w));
		XdbDebug2(__FILE__, w, (Widget)MW_Sep2(w),
			  "Sep2 geometry X %d Y %d W %d Y %d\n",
				       MW_MarginWidth(w),
				       endy,
				       curw - 2 * MW_MarginWidth(w),
				       XtHeight(MW_Sep2(w)));

		_XmConfigureObject((Widget)MW_Sep2(w),
				   MW_MarginWidth(w), endy,
				   curw - 2 * MW_MarginWidth(w),
				   XtHeight(MW_Sep2(w)),
				   XtBorderWidth(MW_Sep2(w)));
	    }
	}
    }

    if (VALID(SW_WorkWindow(w))) {
	if (MW_ShowSep(w)) {
	    if (VALID(MW_Sep1(w))) {
		XdbDebug2(__FILE__, w, (Widget)MW_Sep1(w),
			  "Sep1 geometry X %d Y %d W %d Y %d\n",
			  MW_MarginWidth(w),
			  starty,
			  curw - 2 * MW_MarginWidth(w),
			  XtHeight((Widget)MW_Sep1(w)));

		_XmConfigureObject((Widget)MW_Sep1(w),
				   MW_MarginWidth(w), starty,
				   curw - 2 * MW_MarginWidth(w),
				   XtHeight(MW_Sep1(w)),
				   XtBorderWidth(MW_Sep1(w)));
		starty += XtHeight(MW_Sep2(w));
	    }
	}

	/* if we're treating this as a -ConfigureObject call, this
	 * should always be True. */
	SW_FromResize(w) = True;

	xx = MW_MarginWidth(w);
	yy = starty;
	ww = curw - 2 * MW_MarginWidth(w);
	hh = endy - starty;

	if (curw < 2 * MW_MarginWidth(w)) {
		XdbDebug2(__FILE__, w, SW_WorkWindow(w),
			"curw %d < 2 * MarginWidth = %d\n",
			curw, 2 * MW_MarginWidth(w));
	}
	if (endy < starty) {
		XdbDebug2(__FILE__, w, SW_WorkWindow(w),
			"endy %d < starty %d\n", endy, starty);
	}

	XdbDebug2(__FILE__, w, SW_WorkWindow(w),
		  "WorkWindow geometry X %d Y %d W %d H %d\n",
		  xx, yy, ww, hh);

/* For lack of better treatment, don't call this with negative args */
/* FIX ME */
	if (curw >= 2 * MW_MarginWidth(w) && endy >= starty)
	    _XmScrolledWindowLayout(w, False, NULL, False, NULL,
				xx, yy, ww, hh);

	SW_FromResize(w) = False;
    }
}

static void
_XmMainWLayoutChildAnswer(Widget w, Widget child, XtWidgetGeometry *childgeom,
			  Dimension curw, Dimension curh)
{
    Position starty, endy;

    childgeom->request_mode &= (CWWidth|CWHeight);

    XdbDebug(__FILE__, w, "Anwer child's geom request\n");

    /*
     * FIXME -- this layout algorithm is probably wrong.  I don't know which
     * child should win if the window is smaller than the asked for geometry
     */
    starty = MW_MarginHeight(w);
    endy = curh - MW_MarginHeight(w);
    if (VALID(MW_MenuBar(w))) {
	if (child == MW_MenuBar(w)) {
	    childgeom->width = curw - 2 * MW_MarginWidth(w);
	    childgeom->height = XtHeight(child);
	    return;
	}
	starty += XtHeight(MW_MenuBar(w));
    }

    if (VALID(MW_CommandWindow(w))) {
	if (MW_ShowSep(w) && MW_Sep1(w) && XtIsManaged(MW_Sep1(w))) {
	    if (child == (Widget)MW_Sep1(w)) {
		childgeom->width = curw - 2 * MW_MarginWidth(w);
		childgeom->height = XtHeight(child);
		return;
	    }
	    starty += XtHeight(MW_Sep1(w));
	}
	if (child == MW_CommandWindow(w)) {
	    childgeom->width = curw - 2 * MW_MarginWidth(w);
	    childgeom->height = XtHeight(child);
	    return;
	}
	starty += XtHeight(MW_CommandWindow(w));
    }

    if (VALID(MW_MessageWindow(w))) {
	endy -= XtHeight(MW_MessageWindow(w));
	if (child == MW_MessageWindow(w)) {
	    childgeom->width = curw - 2 * MW_MarginWidth(w);
	    childgeom->height = XtHeight(child);
	    return;
	}
	if (MW_ShowSep(w) && VALID(MW_Sep3(w))) {
	    endy -= XtHeight(MW_Sep3(w));

	    if (child == (Widget)MW_Sep3(w)) {
		childgeom->width = curw - 2 * MW_MarginWidth(w);
		childgeom->height = XtHeight(child);
		return;
	    }
	}
    }

    if (VALID(SW_WorkWindow(w))) {
	if (MW_ShowSep(w) && VALID(MW_Sep2(w))) {
	    if (child == (Widget)MW_Sep2(w)) {
		childgeom->width = curw - 2 * MW_MarginWidth(w);
		childgeom->height = XtHeight(child);
		return;
	    }
	    starty += XtHeight(MW_Sep2(w));
	}
	if (child == SW_WorkWindow(w)) {
	    childgeom->width = curw - 2 * MW_MarginWidth(w);
	    childgeom->height = endy - starty;
	    return;
	}
    }
}

static void
_XmMainWindowLayout(Widget w, Boolean ParentResize,
		    Widget child, Boolean TestMode, XtWidgetGeometry *childgeom)
{
    Dimension curw, curh, fixedh;
    XtWidgetGeometry wants;
    XtGeometryResult result;
    XtWidgetGeometry request;

    curw = 2 * MW_MarginWidth(w);
    curh = 2 * MW_MarginHeight(w);
    fixedh = 2 * MW_MarginHeight(w);

    if (TestMode && (child == NULL || childgeom == NULL)) {
	XdbDebug(__FILE__, w,
		 "_XmMainWindowLayout TestMode with invalid parameters\n");
	return;
    }

    if (TestMode)
	ParentResize = False;

    XdbDebug(__FILE__, w, "_XmMainWindowLayout(%s%s) Child %s geo %s\n",
	ParentResize ? "ParentResize" : "",
	TestMode ? (ParentResize ? " TestMode" : "TestMode") : "",
	child ? XtName(child) : "(none)",
	XdbWidgetGeometry2String(childgeom));

#define	Wants(x)	((childgeom->request_mode & (x)) == (x))

    /* first, find out how our scrolledWindow portion wants to look,
     * as it will be the controlling factor for our width (if it's managed) */
    if (VALID(SW_WorkWindow(w))) {
	wants.request_mode = 0;
	if (child == SW_WorkWindow(w) && Wants(CWWidth) && Wants(CWHeight)) {
	    wants = *childgeom;
	} else {
	    _XmScrolledWindowLayout(w, False, w, True, &wants,
				XtX(w), XtY(w), 0, 0);
	}
	if (child == SW_WorkWindow(w) && Wants(CWWidth))
	    wants.width = childgeom->width;
	if (child == SW_WorkWindow(w) && Wants(CWHeight))
	    wants.height = childgeom->height;

	if (wants.height == 0)
	    wants.height = 1;

	curw = wants.width + 2 * MW_MarginWidth(w);
	curh = wants.height + 2 * MW_MarginHeight(w);

	XdbDebug2(__FILE__, w, SW_WorkWindow(w),
		  "_XmMainWindowLayout: work window wants %d %d\n",
		  curw, curh);
	if (MW_ShowSep(w) && VALID(MW_Sep2(w)))
	    curh += XtHeight(MW_Sep2(w));
    }

    /* add the menubar.  FIXME -- should this affect our width? */
    if (VALID(MW_MenuBar(w))) {
	if ((XtWidth(MW_MenuBar(w)) + 2 * MW_MarginWidth(w)) > curw)
		curw = XtWidth(MW_MenuBar(w)) + 2 * MW_MarginWidth(w);
	curh += XtHeight(MW_MenuBar(w));
	fixedh += XtHeight(MW_MenuBar(w));
    }

    /* command window.  FIXME -- should this affect our width? */
    if (VALID(MW_CommandWindow(w))) {
	if ((XtWidth(MW_CommandWindow(w)) + 2 * MW_MarginWidth(w)) > curw)
		curw = XtWidth(MW_CommandWindow(w)) + 2 * MW_MarginWidth(w);
	curh += XtHeight(MW_CommandWindow(w));
	fixedh += XtHeight(MW_CommandWindow(w));
	if (MW_ShowSep(w) && VALID(MW_Sep1(w))) {
	    curh += XtHeight(MW_Sep1(w));
	    fixedh += XtHeight(MW_Sep1(w));
	}
    }

    /* message box.  FIXME -- should this affect our width? */
    if (VALID(MW_MessageWindow(w))) {
	if ((XtWidth(MW_MessageWindow(w)) + 2 * MW_MarginWidth(w)) > curw)
		curw = XtWidth(MW_MessageWindow(w)) + 2 * MW_MarginWidth(w);
	curh += XtHeight(MW_MessageWindow(w));
	fixedh += XtHeight(MW_MessageWindow(w));
	if (MW_ShowSep(w) && VALID(MW_Sep3(w))) {
	    curh += XtHeight(MW_Sep3(w));
	    fixedh += XtHeight(MW_Sep3(w));
	}
    }

    /*
     * if we're coming from query_geometry
     */
    if (TestMode && w == child) {
	childgeom->width = curw;
	childgeom->height = curh;
	return;
    }

    /*
     * OK, we know how big we want to be.  See if our parent will let us.
     */
    if (ParentResize && SW_VisualPolicy(w) != XmCONSTANT) {
	if (curw < XtWidth(w))
	    curw = XtWidth(w);
	if (curh < XtHeight(w))
	    curh = XtHeight(w);
	if (curw == 0)
	    curw = 1;
	if (curh == 0)
	    curh = 1;

	request.request_mode = (CWWidth|CWHeight);
	request.width = curw;
	request.height = curh;
	result = _XmMakeGeometryRequest(w, &request);
	curw = request.width;
	curh = request.height;

	if (result == XtGeometryYes) {
	    XtWidth(w) = curw;
	    XtHeight(w) = curh;
	}
	if (result == XtGeometryNo) {
	    curw = XtWidth(w);
	    curh = XtHeight(w);
	}
	XdbDebug(__FILE__, w, "_XmMainWindowLayout (%d %d) => %s\n",
		curw, curh, XdbGeometryResult2String(result));
    }
    else {
	curw = XtWidth(w);
	curh = XtHeight(w);
    }

    /* FIXME -- this is the most broken part.  I gotta do some
     * M*tif testing to fix this */
    if (curh < (fixedh + 4 + MW_MarginHeight(w) * 2))
	curh = fixedh + 4 + MW_MarginHeight(w) * 2;

    if (TestMode) {
	_XmMainWLayoutChildAnswer(w, child, childgeom, curw, curh);
	return;
    }

    if (MW_CommandLoc(w) == XmCOMMAND_ABOVE_WORKSPACE)
	_XmMainWLayoutAbove(w, curw, curh);
    else
	_XmMainWLayoutBelow(w, curw, curh);
}

Widget
XmCreateMainWindow(Widget parent, 
		   char *name,
		   Arg *argList,
		   Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmMainWindowWidgetClass,
			  parent,
			  argList, argcount);

}

Widget 
XmMainWindowSep1(Widget widget)
{
    return (Widget)MW_Sep1(widget);
}

Widget 
XmMainWindowSep2(Widget widget)
{
    return (Widget)MW_Sep2(widget);
}

Widget 
XmMainWindowSep3(Widget widget)
{
    return (Widget)MW_Sep3(widget);
}

void 
XmMainWindowSetAreas(Widget widget, 
		     Widget menu_bar,
		     Widget command_window,
		     Widget horizontal_scrollbar,
		     Widget vertical_scrollbar,
		     Widget work_region)
{
    XdbDebug(__FILE__, widget, "XmMainWindowSetAreas [");
#define	P(cw, t)						\
    if (cw)							\
	XdbDebug0(__FILE__, widget, t, XtName(cw));		\
    else							\
	XdbDebug0(__FILE__, widget, t, ": NULL");

    P(menu_bar, " MenuBar %s");
    P(command_window, " CommandWindow %s");
    P(horizontal_scrollbar, " Hor.Scrollbar %s");
    P(vertical_scrollbar, " Vert.Scrollbar %s");
    P(work_region, " WorkRegion %s");
    XdbDebug0(__FILE__, widget, "\n");

    MW_MenuBar(widget) = menu_bar;
    MW_CommandWindow(widget) = command_window;
    SW_WorkWindow(widget) = work_region;
    SW_HSB(widget) = (XmScrollBarWidget)horizontal_scrollbar;
    SW_VSB(widget) = (XmScrollBarWidget)vertical_scrollbar;

    _XmMainWindowLayout(widget, True, NULL, False, NULL);
}

static void
InsertChild(Widget w)
{
    Widget p = XtParent(w);
    
    if ((XmIsRowColumn(w) && RC_Type(w) == XmMENU_BAR) ||
        XmIsCommand(w) || XmIsSeparator(w) || XmIsSeparatorGadget(w)) {
#define	superclass	(&xmManagerClassRec)
	(*superclass->composite_class.insert_child)(w);
#undef	superclass
	if (XmIsRowColumn(w) && RC_Type(w) == XmMENU_BAR) {
	    XdbDebug2(__FILE__, p, w, "InsertChild : this is the menu bar\n");
	    XtVaSetValues(p, XmNmenuBar, w, NULL);
	}
	else if (XmIsCommand(w)) {
	    XdbDebug2(__FILE__, p, w, "InsertChild : this is the command window\n");
	    XtVaSetValues(p, XmNcommandWindow, w, NULL);
	}
	else if (XmIsSeparator(w) || XmIsSeparatorGadget(w)) {
	    XdbDebug2(__FILE__, p, w, "InsertChild : this is the seperator\n");
	}
    }
    else {
/* 
 * Now we have a scrollbar or a workwindow - let scrolledwindow handle this
 */
#define	superclass	(&xmScrolledWindowClassRec)
	(*superclass->composite_class.insert_child)(w);
#undef	superclass
    } 
}

