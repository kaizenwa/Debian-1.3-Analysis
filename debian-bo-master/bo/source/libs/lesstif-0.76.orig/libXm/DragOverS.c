/**
 *
 * $Id: DragOverS.c,v 1.2 1996/11/28 09:21:06 u27113 Exp $
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

static char rcsid[] = "$Id: DragOverS.c,v 1.2 1996/11/28 09:21:06 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/VendorSP.h>
#include <Xm/VendorSEP.h>
#include <Xm/DragOverSP.h>

#include <XmI/DebugUtil.h>

static void class_initialize();
static void class_part_initialize(WidgetClass wclass);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void change_managed(Widget w);

static XmBaseClassExtRec _XmDragOverSCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL,
    /* set_values_prehook        */ NULL,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ NULL,
    /* secondary_object_class    */ (WidgetClass)NULL,
    /* secondary_object_create   */ NULL,
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

XmDragOverShellClassRec xmDragOverShellClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &vendorShellClassRec,
        /* class_name            */ "XmDragOverShell",
        /* size                  */ sizeof(XmDragOverShellRec),
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
      	/* tm_table              */ NULL,
      	/* query_geometry        */ NULL,
      	/* display_accelerator   */ NULL,
      	/* extension             */ (XtPointer)&_XmDragOverSCoreClassExtRec
    },
    /* Composite Class Part */
    {
	/* geometry_manager */ XtInheritGeometryManager,
        /* change_managed   */ change_managed,
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
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
    /* XmDragOverShell Class Part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmDragOverShellWidgetClass = (WidgetClass)(&xmDragOverShellClassRec);

static void class_initialize()
{
    _XmDragOverSCoreClassExtRec.record_type = XmQmotif;
}

static void class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmDRAG_OVER_SHELL_BIT);
}


static void 
initialize_posthook(Widget request, 
	   Widget new_w, 
	   ArgList args, 
	   Cardinal *num_args)
{
}

static void 
initialize(Widget request, 
	   Widget new_w, 
	   ArgList args, 
	   Cardinal *num_args)
{
}

static void 
change_managed(Widget w)
{
}
