/**
 *
 * $Id: DropSMgr.c,v 1.5 1997/01/14 04:16:57 miers Exp $
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

static char rcsid[] = "$Id: DropSMgr.c,v 1.5 1997/01/14 04:16:57 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/DropSMgrP.h>
#include <Xm/XmP.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void __XmDSMCreateInfo(XmDropSiteManagerObject dsm,
			      Widget widget,
			      ArgList args,
			      Cardinal num_args);
static void __XmDSMDestroyInfo(XmDropSiteManagerObject dsm,
			       Widget widget);
static void __XmDSMStartUpdate(XmDropSiteManagerObject dsm,
			       Widget widget);
static void __XmDSMRetrieveInfo(XmDropSiteManagerObject dsm,
				Widget widget,
				ArgList args,
				Cardinal num_args);
static void __XmDSMUpdateInfo(XmDropSiteManagerObject dsm,
			      Widget widget,
			      ArgList args,
			      Cardinal num_args);
static void __XmDSMEndUpdate(XmDropSiteManagerObject dsm,
			     Widget widget);
static void __XmDSMUpdateDSM(XmDropSiteManagerObject dsm,
			     XtPointer clientData,
			     XtPointer callData);
static void __XmDSMProcessMotion(XmDropSiteManagerObject dsm,
				 XtPointer clientData,
				 XtPointer callData);
static void __XmDSMProcessDrop(XmDropSiteManagerObject dsm,
			       XtPointer clientData,
			       XtPointer callData);
static void __XmDSMOperationChanged(XmDropSiteManagerObject dsm,
				    XtPointer clientData,
				    XtPointer callData);
static void __XmDSMChangeRoot(XmDropSiteManagerObject dsm,
			      XtPointer clientData,
			      XtPointer callData);
static void __XmDSMInsertInfo(XmDropSiteManagerObject dsm,
			      XtPointer clientData,
			      XtPointer callData);
static void __XmDSMRemoveInfo(XmDropSiteManagerObject dsm,
			      XtPointer info);
static void __XmDSMSyncTree(XmDropSiteManagerObject dsm,
			    Widget shell);
static int __XmDSMGetTree(XmDropSiteManagerObject dsm,
			  Widget shell,
			  XtPointer dataPtr);
static void __XmDSMCreateDSInfoTable(XmDropSiteManagerObject dsm);
static void __XmDSMDestroyDSInfoTable(XmDropSiteManagerObject dsm);
static void __XmDSMRegisterInfo(XmDropSiteManagerObject dsm,
				Widget widget,
				XtPointer info);
static XtPointer __XmDSMWidgetToInfo(XmDropSiteManagerObject dsm,
				     Widget widget);
static void __XmDSMUnregisterInfo(XmDropSiteManagerObject dsm,
				  XtPointer info);

static void class_initialize(void);
static void class_part_initialize(WidgetClass widget_class);

static XmBaseClassExtRec _XmDropSMgrObjectClassExtRec = {
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

XmDropSiteManagerClassRec xmDropSiteManagerClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &objectClassRec,
        /* class_name            */ "XmDropSiteManager",
	/* widget_size           */ sizeof(XmDropSiteManagerRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ NULL,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ NULL,
	/* num_resources         */ 0,
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMultiple,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
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
	/* extension             */ (XtPointer)&_XmDropSMgrObjectClassExtRec
    },
    /* Drop site manager class part */
    {
	/* createInfo       */  __XmDSMCreateInfo,
	/* destroyInfo      */  __XmDSMDestroyInfo,
	/* startUpdate      */  __XmDSMStartUpdate,
	/* retrieveInfo     */  __XmDSMRetrieveInfo,
	/* updateInfo       */  __XmDSMUpdateInfo,
	/* endUpdate        */  __XmDSMEndUpdate,
	/* updateDSM        */  __XmDSMUpdateDSM,
	/* processMotion    */  __XmDSMProcessMotion,
	/* processDrop      */  __XmDSMProcessDrop,
	/* operationChanged */  __XmDSMOperationChanged,
	/* changeRoot       */  __XmDSMChangeRoot,
	/* insertInfo       */  __XmDSMInsertInfo,
	/* removeInfo       */  __XmDSMRemoveInfo,
	/* syncTree         */  __XmDSMSyncTree,
	/* getTreeFromDSM   */  __XmDSMGetTree,
	/* createTable      */  __XmDSMCreateDSInfoTable,
	/* destroyTable     */  __XmDSMDestroyDSInfoTable,
	/* registerInfo     */  __XmDSMRegisterInfo,
	/* widgetToInfo     */  __XmDSMWidgetToInfo,
	/* unregisterInfo   */  __XmDSMUnregisterInfo,
	/* extension        */  NULL
    }
};

WidgetClass xmDropSiteManagerObjectClass = (WidgetClass)(&xmDropSiteManagerClassRec);

/*
 * Creates a drop site for the associated widget (perhaps)
 */

static void
class_initialize(void)
{
    _XmDropSMgrObjectClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmDROP_SITE_MANAGER_BIT);
}

static void 
__XmDSMCreateInfo(XmDropSiteManagerObject object,
		  Widget widget,
		  ArgList args,
		  Cardinal num_args)
{
    /* Fix Me */
}

static void 
__XmDSMDestroyInfo(XmDropSiteManagerObject dsm,
		   Widget widget)
{
}

static void 
__XmDSMStartUpdate(XmDropSiteManagerObject dsm,
		   Widget widget)
{
}

static void 
__XmDSMRetrieveInfo(XmDropSiteManagerObject dsm,
		    Widget widget,
		    ArgList args,
		    Cardinal num_args)
{
}

static void 
__XmDSMUpdateInfo(XmDropSiteManagerObject dsm,
		  Widget widget,
		  ArgList args,
		  Cardinal num_args)
{
}

static void 
__XmDSMEndUpdate(XmDropSiteManagerObject dsm,
		 Widget widget)
{
}

static void 
__XmDSMUpdateDSM(XmDropSiteManagerObject dsm,
		 XtPointer clientData,
		 XtPointer callData)
{
}

/*
 * When a drag motion event is sent to this client, we handle 
 * the notification of the drop site (if it has drag under effects)
 */
static void 
__XmDSMProcessMotion(XmDropSiteManagerObject dsm,
		     XtPointer clientData,
		     XtPointer callData)
{
}

/*
 * When the drop is started (BDrag is released), we let the drop site know that
 * it's being dropped in
 */
static void 
__XmDSMProcessDrop(XmDropSiteManagerObject dsm,
		   XtPointer clientData,
		   XtPointer callData)
{
}

/*
 * We notify the current drop site (if any) that the operation of the drag
 * has changed
 */
static void 
__XmDSMOperationChanged(XmDropSiteManagerObject dsm,
			XtPointer clientData,
			XtPointer callData)
{
}

static void 
__XmDSMChangeRoot(XmDropSiteManagerObject dsm,
		  XtPointer clientData,
		  XtPointer callData)
{
}

static void 
__XmDSMInsertInfo(XmDropSiteManagerObject dsm,
		  XtPointer clientData,
		  XtPointer callData)
{
}

static void 
__XmDSMRemoveInfo(XmDropSiteManagerObject dsm,
		  XtPointer info)
{
}

static void 
__XmDSMSyncTree(XmDropSiteManagerObject dsm,
		Widget shell)
{
}

/*
 * This function creates (or replaces) the property on the shell widget
 * to contain the entire drop site database.
 */
static int 
__XmDSMGetTree(XmDropSiteManagerObject dsm,
	       Widget shell,
	       XtPointer dataPtr)
{
    return 0;
}

/*
 * Creates a hash table that goes from widgets to drop site information
 */
static void 
__XmDSMCreateDSInfoTable(XmDropSiteManagerObject dsm)
{
}

/*
 * destroys the hash table
 */
static void 
__XmDSMDestroyDSInfoTable(XmDropSiteManagerObject dsm)
{
}

/*
 * adds a widget and its associated info to the hash table
 */
static void 
__XmDSMRegisterInfo(XmDropSiteManagerObject dsm,
		    Widget widget,
		    XtPointer info)
{
}

/*
 * retrieves the info for a given widget from the hash table
 */
static XtPointer 
__XmDSMWidgetToInfo(XmDropSiteManagerObject dsm,
		    Widget widget)
{
    return NULL;
}

/*
 * removes the info from the hash table, along with the widget that
 * hashes to it.
 */
static void 
__XmDSMUnregisterInfo(XmDropSiteManagerObject dsm,
		      XtPointer info)
{
}

void
_XmDSMUpdate(XmDropSiteManagerObject dsm,
	     XtPointer clientData,
	     XtPointer callData)
{
}

int
_XmDSMGetTreeFromDSM(XmDropSiteManagerObject dsm,
		     Widget shell,
		     XtPointer dataPtr)
{
    return 0;
}

Boolean
_XmDropSiteShell(Widget widget)
{
    return False;
}

Boolean
_XmDropSiteWrapperCandidate(Widget widget)
{
    return False;
}

Widget
_XmGetActiveDropSite(Widget widget)
{
    return NULL;
}

void
_XmSyncDropSiteTree(Widget shell)
{
}

void
_XmIEndUpdate(XtPointer client_data, XtIntervalId *interval_id)
{
}

