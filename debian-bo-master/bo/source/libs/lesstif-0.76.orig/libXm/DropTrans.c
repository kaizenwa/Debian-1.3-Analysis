/**
 *
 * $Id: DropTrans.c,v 1.5 1996/11/28 09:21:10 u27113 Exp $
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

static char rcsid[] = "$Id: DropTrans.c,v 1.5 1996/11/28 09:21:10 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/DropTransP.h>
#include <string.h>

#include <XmI/DebugUtil.h>

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);

#define Offset(field) XtOffsetOf(XmDropTransferRec, dropTransfer.field)

static XtResource resources[] = {
    {
	XmNdropTransfers, XmCDropTransfers, XmRDropTransfers,
	sizeof(XmDropTransferEntry), Offset(drop_transfers),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNnumDropTransfers, XmCNumDropTransfers, XmRCardinal,
	sizeof(Cardinal), Offset(num_drop_transfers),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNincremental, XmCIncremental, XmRBoolean,
	sizeof(Boolean), Offset(incremental),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNtransferProc, XmCTransferProc, XmRCallbackProc,
	sizeof(XtSelectionCallbackProc), Offset(transfer_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNtransferStatus, XmCTransferStatus, XmRTransferStatus,
	sizeof(unsigned char), Offset(transfer_status),
	XmRImmediate, (XtPointer)XmTRANSFER_SUCCESS
    }
};

static XmBaseClassExtRec _XmDropTransferObjectClassExtRec = {
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

XmDropTransferClassRec xmDropTransferClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &objectClassRec,
        /* class_name            */ "XmDropTransferObject",
	/* widget_size           */ sizeof(XmDropTransferRec),
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
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
        /* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmDropTransferObjectClassExtRec
    },
    /* XmDropTranferObject part */
    {
        /* start_drop_transfer*/ NULL,
        /* add_drop_transfer  */ NULL,
        /* extension          */ NULL
    }

};

WidgetClass xmDropTransferObjectClass = (WidgetClass)&xmDropTransferClassRec;

static void
class_initialize()
{
    XdbDebug(__FILE__, NULL, "DropTransfer class initialize\n");

    _XmDropTransferObjectClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XdbDebug(__FILE__, NULL, "DropTransfer class part initialize\n");

    _XmFastSubclassInit(widget_class, XmDROP_TRANSFER_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "DropTransfer initialize\n");
}

static void
destroy(Widget w)
{
    XdbDebug(__FILE__, w, "DropTransfer destroy\n");
}

static Boolean
set_values(Widget current,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "DropTransfer set_values\n");

    return True; /* FIX ME */
}
