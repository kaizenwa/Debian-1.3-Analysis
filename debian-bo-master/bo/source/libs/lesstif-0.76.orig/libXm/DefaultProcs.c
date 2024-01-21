/**
 *
 * $Id: DefaultProcs.c,v 1.7 1996/11/28 09:20:55 u27113 Exp $
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

static char rcsid[] = "$Id: DefaultProcs.c,v 1.7 1996/11/28 09:20:55 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/BulletinBP.h>
#include <Xm/DialogSP.h>
#include <Xm/RowColumnP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/ScaleP.h>
#include <Xm/CascadeBP.h>
#include <Xm/FrameP.h>
#include <Xm/MenuShellP.h>
#include <Xm/PushBP.h>
#include <Xm/PushBGP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/ToggleBP.h>
#include <Xm/ToggleBGP.h>
#include <Xm/VendorSEP.h>
#include <Xm/VendorSP.h>
#include <stdlib.h>

#include <XmI/DebugUtil.h>

void 
_XmDefaultButtonShadowThickness(Widget w,
				int offset,
				XrmValue *val)
{
    static Dimension shadowThickness = 1;

    XdbDebug(__FILE__, w, "_XmDefaultButtonShadowThickness(%s), orig is %d",
	XtName(w), XmIsPushButton(w) ? PB_DefaultButtonShadow(w) :
		XmIsPushButtonGadget(w) ? PBG_DefaultButtonShadow(w) : 0);

    if (!XmIsPushButton(w) && !XmIsPushButtonGadget(w))
	shadowThickness = 0;
    else {
	if (XmIsPushButton(w)) {
	    if (PB_DefaultButtonShadow(w) > 0)
		shadowThickness = PB_DefaultButtonShadow(w);
	    else
		shadowThickness = PB_ShowAsDefault(w);
	}
	else {
	    if (PBG_DefaultButtonShadow(w) > 0)
		shadowThickness = PBG_DefaultButtonShadow(w);
	    else
		shadowThickness = PBG_ShowAsDefault(w);
	}
    }

    val->addr = (XtPointer)&shadowThickness;
}

void 
_XmDefaultMultiClick(Widget w,
		     int offset,
		     XrmValue *val)
{
    /* FIX ME */
    static unsigned char click = XmMULTICLICK_KEEP;

    val->addr = (XtPointer)&click;
}

/* BulletinBoard defaults */

void _XmBulletinBoardDialogStyleDefault(Widget w,
					int offset,
					XrmValue *val)
{
    static unsigned char style;

    if (XmIsDialogShell(XtParent(w)))
	style = XmDIALOG_MODELESS;
    else
	style = XmDIALOG_WORK_AREA;

    val->addr = (XtPointer)&style;
}

/* RowColumn Defaults */

void _XmRowColumnPackingDefault(Widget w,
				int offset,
				XrmValue *val)
{
    static unsigned char packing;

    if (XdbInDebug(__FILE__, w)) {
	XdbDebug(__FILE__, w, "_XmRowColumnPackingDefault: RadioBehavior %s, RC_Type %s => Packing %s\n",
	    XdbBoolean2String(RC_RadioBehavior(w)), XdbRcType2String(RC_Type(w)),
	    XdbPacking2String(packing));
    }

    if (RC_RadioBehavior(w) && RC_Type(w) == XmWORK_AREA)
	packing = XmPACK_COLUMN;
    else
	packing = XmPACK_TIGHT;

    val->addr = (XtPointer)&packing;
}

void 
_XmRowColumnEntryClassDefault(Widget w,
			      int offset,
			      XrmValue *val)
{
    static WidgetClass wclass;
/*    XmRowColumnWidget rc = (XmRowColumnWidget)w;

    if (rc->rowcolumn.radioBehavior && rc->rowcolumn.rowColumnType == XmWORK_AREA)
	wclass = xmToggleButtonGadgetClass;
    else if (rc->rowcolumn.rowColumnType == XmMENU_BAR)
	wclass = xmCascadeButtonClass;
    else *//* hmmm.... */;

    val->addr = (XtPointer)&wclass;
}

void 
_XmRowColumnIsHomogeneousDefault(Widget w,
				 int offset,
				 XrmValue *val)
{
    static Boolean homo;

    if ((RC_RadioBehavior(w) && RC_Type(w) == XmWORK_AREA)
	|| RC_Type(w) == XmMENU_BAR)
	homo = True;
    else
	homo = False;

    val->addr = (XtPointer)&homo;
}

void 
_XmRowColumnMarginDefault(Widget w,
			  int offset,
			  XrmValue *val)
{
    static Dimension margin;

    if (RC_Type(w) == XmMENU_POPUP
	|| RC_Type(w) == XmMENU_PULLDOWN)
	margin = 0;
    else
	margin = 3;

    val->addr = (XtPointer)&margin;
}

void 
_XmRowColumnMenuAcceleratorDefault(Widget w,
				   int offset,
				   XrmValue *val)
{
    static String foo;

    if (!foo)
	foo = XtNewString("foo");

    val->addr = (XtPointer)&foo;
}

void 
_XmRowColumnSpacingDefault(Widget w,
			   int offset,
			   XrmValue *val)
{
    static Dimension dim;

    if (RC_Type(w) == XmMENU_OPTION
	|| RC_Type(w) == XmWORK_AREA)
	dim = 3;
    else
	dim = 0;

    val->addr = (XtPointer)&dim;
}

/* Cascade Button */

void 
_XmCascadePixmapDefault(Widget w,
			int offset,
			XrmValue *value)
{
    Widget parent = XtParent(w);

    if (XmIsRowColumn(parent))
    {
	if (RC_Type(w) == XmMENU_POPUP
	    || RC_Type(w) == XmMENU_PULLDOWN)
	{
#if 0
	    /* make the widget's marginWidth wider to accomodate the pixmap */
	    LabG_MarginRight(w) = 20;
#endif
	}
    }
}

/* Scale */

void
_XmScaleProcessingDirectionDefault(Widget w,
				   int offset,
				   XrmValue *val)
{
    static unsigned char direction;
    XmScaleWidget sw = (XmScaleWidget) w;
    
    if (sw->scale.orientation == XmVERTICAL)
	direction = XmMAX_ON_TOP;
    else
	direction = XmMAX_ON_RIGHT; /* FIX ME */
    
    val->addr = (XtPointer)&direction;
}

/* Scrollbar */

void 
_XmScrollBarTroughColorDefault(Widget w,
			       int offset,
			       XrmValue *val)
{    
    XColor _widgetBackground;
    static XColor troughColor;

    _widgetBackground.pixel = XtBackground(w);

    XQueryColor(XtDisplay(w),
		DefaultColormapOfScreen(XtScreen(w)),
		&_widgetBackground);

    troughColor.blue = _widgetBackground.blue * .80;
    troughColor.green = _widgetBackground.green * .80;
    troughColor.red = _widgetBackground.red * .80;

    if (!XAllocColor(XtDisplay(w),
		     DefaultColormapOfScreen(XtScreen(w)),
		     &troughColor))
	troughColor.pixel = WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplay(w)));


    val->addr = (XtPointer)&troughColor.pixel;
}

void
_XmScrollBarProcessingDirectionDefault(Widget w,
				       int offset,
				       XrmValue *val)
{
    static unsigned char direction;
    XmScrollBarWidget sw = (XmScrollBarWidget) w;

    if (sw->scrollBar.orientation == XmVERTICAL)
	direction = XmMAX_ON_BOTTOM;
    else
	direction = XmMAX_ON_RIGHT; /* FIX ME */

    val->addr = (XtPointer)&direction;
}

void
_XmScrollBarTraversalOnDefault(Widget w,
			       int offset,
			       XrmValue *val)
{
    static Boolean traversalOn;

    traversalOn = False; /* FIX ME */

    val->addr = (XtPointer)&traversalOn;
}

void
_XmFrameHorizSpaceDefault(Widget w,
			  int offset,
			  XrmValue *val)
{
    static Dimension space;
    XmFrameWidget fw = (XmFrameWidget)XtParent(w);

    space = Frame_MarginWidth(fw);

    val->addr = (XtPointer)&space;
}

void
_XmFrameShadowTypeDefault(Widget w,
			  int offset,
			  XrmValue *val)
{
    static unsigned char type;

    if (XtIsShell(XtParent(w)))
	type = XmSHADOW_OUT;
    else
	type = XmSHADOW_ETCHED_IN;

    val->addr = (XtPointer)&type;
}

void
_XmFrameShadowThicknessDefault(Widget w,
			       int offset,
			       XrmValue *val)
{
    static Dimension thickness;

    if (XtIsShell(XtParent(w)))
	thickness = 1;
    else
	thickness = 2;

    val->addr = (XtPointer)&thickness;
}

void
_XmToggleButtonFillOnSelectDefault(Widget w, 
				    int offset, 
				    XrmValue *val)
{
    static Boolean fill;

    if (XmIsToggleButton(w))
	fill = TB_IndOn(w);
    else /* assume toggle button gadget */
	fill = TBG_IndOn(w);

    val->addr = (XtPointer)&fill;
}

void
_XmToggleButtonIndicatorTypeDefault(Widget w, 
				    int offset, 
				    XrmValue *val)
{
    static unsigned char type;
    Widget parent;

    parent = XtParent(w);

    if (XmIsRowColumn(parent))
    {
	XmRowColumnWidget rc = (XmRowColumnWidget)parent;

	if (RC_RadioBehavior(rc) == True)
	    type = XmONE_OF_MANY;
	else
	    type = XmN_OF_MANY;
    }
    else
	type = XmN_OF_MANY;

    val->addr = (XtPointer)&type;
}

void
_XmVendorShellVirtualBindingsDefault(Widget w, 
				     int offset, 
				     XrmValue *val)
{
    extern char _XmVirtKeys_fallbackBindingString[];

    val->addr = (XtPointer)_XmVirtKeys_fallbackBindingString;
}

