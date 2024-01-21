/**
 *
 * $Id: DragIcon.c,v 1.5 1996/11/28 09:21:04 u27113 Exp $
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

static char rcsid[] = "$Id: DragIcon.c,v 1.5 1996/11/28 09:21:04 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/DragDrop.h>
#include <Xm/DragIconP.h>

#include <XmI/DebugUtil.h>

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);

#define Offset(field) XtOffsetOf(XmDragIconRec, drag.field)

/* Resources for the DragIcon class */
static XtResource resources[] = {
    {
	XmNdepth, XmCDepth, XmRInt,
	sizeof(Cardinal), Offset(depth),
	XmRImmediate, (XtPointer)1
    },
    {
	XmNwidth, XmCWidth, XmRDimension,
	sizeof(Dimension), Offset(width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNheight, XmCHeight, XmRDimension,
	sizeof(Dimension), Offset(height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNhotX, XmCHot, XmRPosition,
	sizeof(Position), Offset(hot_x),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNhotY, XmCHot, XmRPosition,
	sizeof(Position), Offset(hot_y),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmask, XmCPixmap, XmRBitmap,
	sizeof(Pixmap), Offset(mask),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNpixmap, XmCPixmap, XmRBitmap,
	sizeof(Pixmap), Offset(pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNoffsetX, XmCOffset, XmRPosition,
	sizeof(Position), Offset(offset_x),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNoffsetY, XmCOffset, XmRPosition,
	sizeof(Position), Offset(offset_y),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNattachment, XmCAttachment, XmRIconAttachment,
	sizeof(unsigned char), Offset(attachment),
	XmRImmediate, (XtPointer)XmATTACH_NORTH_WEST
    }
};

static XmBaseClassExtRec _XmDragIconRectClassExtRec = {
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

XmDragIconClassRec xmDragIconClassRec = {
    /* RectObj class part */
    {
       /* superclass            */ (WidgetClass) &rectObjClassRec,
       /* class_name            */ "XmDragIcon",
       /* widget_size           */ sizeof(XmDragIconRec),
       /* class_initialize      */ class_initialize,
       /* class_part_initialize */ class_part_initialize,
       /* class_inited          */ FALSE,
       /* initialize            */ initialize,
       /* initialize_hook       */ NULL,
       /* realize               */ NULL,
       /* actions               */ NULL,
       /* num_actions           */ 0,
       /* resources             */ resources,
       /* num_resources         */ XtNumber(resources),
       /* xrm_class             */ NULLQUARK,
       /* compress_motion       */ FALSE,
       /* compress_exposure     */ XtExposeNoCompress,
       /* compress_enterleave   */ FALSE,
       /* visible_interest      */ FALSE,
       /* destroy               */ destroy,
       /* resize                */ NULL,
       /* expose                */ NULL,
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
       /* extension             */ (XtPointer)&_XmDragIconRectClassExtRec
    },
    /* XmDragIcon part */
    {
        /* extension          */ NULL
    }
};

WidgetClass xmDragIconObjectClass = (WidgetClass)&xmDragIconClassRec;

static void
class_initialize()
{
    _XmDragIconRectClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmDRAG_ICON_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
}

static void
destroy(Widget w)
{
}

static Boolean
set_values(Widget current,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    return True; /* FIX ME */
}

Widget
_XmGetTextualDragIcon(Widget w)
{
    XdbDebug(__FILE__, w, "_XmGetTextualDragIcon is not implemented yet.\n");
    return NULL;
}

/* Motif 2.* version of the above */
Widget
XmeGetTextualDragIcon(Widget w)
{
	return _XmGetTextualDragIcon(w);
}

void
_XmDestroyDefaultDragIcon(XmDragIconObject icon)
{
}

Boolean
_XmDragIconIsDirty(XmDragIconObject icon)
{
    return False;
}

void
_XmDragIconClean(XmDragIconObject icon1,
		 XmDragIconObject icon2,
		 XmDragIconObject icon3)
{
}

Widget 
XmCreateDragIcon(Widget widget, 
		 String name, 
		 ArgList arglist,
		 Cardinal argcount)
{
    return NULL;
}
