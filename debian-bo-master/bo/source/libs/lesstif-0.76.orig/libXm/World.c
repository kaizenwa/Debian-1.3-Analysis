/**
 *
 * $Id: World.c,v 1.5 1996/11/28 09:22:25 u27113 Exp $
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

static char rcsid[] = "$Id: World.c,v 1.5 1996/11/28 09:22:25 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/WorldP.h>
#include <string.h>

#include <XmI/DebugUtil.h>

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);

XmWorldClassRec xmWorldClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmDesktopClassRec,
        /* class_name            */ "XmWorldClass",
	/* widget_size           */ sizeof(XmWorldRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ NULL,
	/* num_resources         */ 0,
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ 0,
	/* compress_exposure     */ 0,
	/* compress_enterleave   */ 0,
	/* visible_interest      */ 0,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL,
	/* get_values_hook       */ _XmExtGetValuesHook,
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
        /* insert_child          */ NULL,
        /* delete_child          */ NULL,
        /* extension             */ NULL
    },
    /* World Class part */
    {
        /* extension             */ NULL
    }
};

WidgetClass xmWorldClass = (WidgetClass)&xmWorldClassRec;

static void
class_initialize()
{
    XdbDebug(__FILE__, NULL, "WorldObject class initialize\n");
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XdbDebug(__FILE__, NULL, "WorldObject class part initialize\n");
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "WorldObject initialize\n");
}

static void
destroy(Widget w)
{
    XdbDebug(__FILE__, w, "WorldObject destroy\n");
}

static Boolean
set_values(Widget current,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "WorldObject set_values\n");

    return True; /* FIX ME */
}

XmWorldObject
_XmGetWorldObject(Widget shell,
		  ArgList args,
		  Cardinal *num_args)
{
    return (XmWorldObject)XtCreateManagedWidget("world", xmWorldClass,
						shell, args, *num_args);
}
