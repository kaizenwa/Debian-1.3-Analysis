/**
 *
 * $Id: BulletinBoard.c,v 1.18 1997/01/11 02:19:41 miers Exp $
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

static char rcsid[] = "$Id";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/BulletinBP.h>
#include <Xm/TransltnsP.h>
#include <Xm/DialogS.h>
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/LabelP.h>
#include <Xm/LabelGP.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/MwmUtil.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult geometry_manager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply);
static void change_managed(Widget w);
static void ConstraintInitialize(Widget request, Widget new_w, Arg *args, Cardinal *nargs);
static Boolean ConstraintSetValues(Widget old, Widget request, Widget new_w, Arg *args, Cardinal *nargs);
static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes);
static void resize(Widget w);
static void InsertChild(Widget w);
static void DeleteChild(Widget w);
static Boolean _XmBBParentProcess(Widget widget, XmParentProcessData data);

/*
 * Resources for the Bulletin board class
 */
#define Offset(field) XtOffsetOf(XmBulletinBoardRec, bulletin_board.field)
#define MGR_Offset(field) XtOffsetOf(XmBulletinBoardRec, manager.field)
static XtResource resources[] = {
    {
	XmNshadowType, XmCShadowType, XmRShadowType,
	sizeof(unsigned char), Offset(shadow_type),
	XmRImmediate, (XtPointer)XmSHADOW_OUT
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), MGR_Offset(shadow_thickness),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)10
    },    
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNdefaultButton, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(default_button),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNcancelButton, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(cancel_button),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNfocusCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(focus_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNmapCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(map_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNunmapCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(unmap_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNbuttonFontList, XmCButtonFontList, XmRFontList,
	sizeof(XmFontList), Offset(button_font_list),
	XmRFontList, (XtPointer)NULL
    },
    {
	XmNlabelFontList, XmCLabelFontList, XmRFontList,
	sizeof(XmFontList), Offset(label_font_list),
	XmRFontList, (XtPointer)NULL
    },
    {
	XmNtextFontList, XmCTextFontList, XmRFontList,
	sizeof(XmFontList), Offset(text_font_list),
	XmRFontList, (XtPointer)NULL
    }, 
    {
	XmNtextTranslations, XmCTranslations, XmRTranslationTable,
	sizeof(XtTranslations), Offset(text_translations),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNallowOverlap, XmCAllowOverlap, XmRBoolean,
	sizeof(Boolean), Offset(allow_overlap),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNautoUnmanage, XmCAutoUnmanage, XmRBoolean,
	sizeof(Boolean), Offset(auto_unmanage),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNdefaultPosition, XmCDefaultPosition, XmRBoolean,
	sizeof(Boolean), Offset(default_position),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNresizePolicy, XmCResizePolicy, XmRResizePolicy,
	sizeof(unsigned char), Offset(resize_policy),
	XmRImmediate, (XtPointer)XmRESIZE_ANY
    },
    {
	XmNnoResize, XmCNoResize, XmRBoolean,
	sizeof(Boolean), Offset(no_resize),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNdialogStyle, XmCDialogStyle, XmRDialogStyle,
	sizeof(unsigned char), Offset(dialog_style),
	XmRCallProc, (XtPointer)_XmBulletinBoardDialogStyleDefault
    },
    {
	XmNdialogTitle, XmCDialogTitle, XmRXmString,
	sizeof(XmString), Offset(dialog_title),
	XmRString, (XtPointer)NULL
    },
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNdialogTitle,
	sizeof(XmString), Offset(dialog_title),
	_XmExportXmString, NULL
    },
    {
	XmNmarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    }
};

char _XmBulletinB_defaultTranslations[] = 
    "<BtnMotion>:               ManagerGadgetButtonMotion()\n\
     <Btn1Down>:                ManagerGadgetArm()\n\
     <Btn1Down>,<Btn1Up>:       ManagerGadgetActivate()\n\
     <Btn1Up>:                  ManagerGadgetActivate()\n\
     <Btn1Down>(2+):            ManagerGadgetMultiArm()\n\
     <Btn1Up>(2+):              ManagerGadgetMultiActivate()\n\
     <Btn2Down>:                ManagerGadgetDrag()\n\
     <Key>osfHelp:              ManagerGadgetHelp()\n\
     <Key>osfActivate:          ManagerParentActivate()\n\
     <Key>osfCancel:            ManagerParentCancel()\n\
     <Key>osfSelect:            ManagerGadgetSelect()\n\
     <Key>space:                ManagerGadgetSelect()\n\
     <Key>Return:               ManagerParentActivate()\n\
     <Key>:                     ManagerGadgetKeyInput()";

static char _XmBulletinB_mapTranslations[] =
    "<Map>:           BulletinBoardMap()\n\
     <Unmap>:         BulletinBoardMap()";

static XtActionsRec actions[] = {
    { "BulletinBoardMap", _XmBulletinBoardMap}
};

static XtTranslations mapTrans;

static XmBaseClassExtRec _XmBulletinBCoreClassExtRec = {
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

static CompositeClassExtensionRec bbCompositeExt = 
{
    /* next_extension */  NULL,
    /* record_type    */  NULLQUARK,
    /* version        */  XtCompositeExtensionVersion,
    /* record_size    */  sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ True
#endif
};

static XmManagerClassExtRec _XmBulletinBMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmBulletinBoardClassRec xmBulletinBoardClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
        /* class_name            */ "XmBulletinBoard",
	/* widget_size           */ sizeof(XmBulletinBoardRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmBulletinB_defaultTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmBulletinBCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager, 
        /* change_managed   */ change_managed, 
        /* insert_child     */	InsertChild,
        /* delete_child     */	DeleteChild,
        /* extension        */ (XtPointer) &bbCompositeExt,
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,  /* FIX ME */
        /* subresource_count */ 0,     /* FIX ME */
        /* constraint_size   */ 0,     /* FIX ME */
        /* initialize        */ ConstraintInitialize,
        /* destroy           */ NULL,  /* FIX ME */
        /* set_values        */ ConstraintSetValues,
        /* extension         */ NULL,   /* FIX ME */
    },
    /* XmManager class part */
    {
	/* translations                 */ XmInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ _XmBBParentProcess,
	/* extension                    */ (XtPointer)&_XmBulletinBMClassExtRec
    },
    /* XmBulletinBoard class part */
    {
	/* always_install_accelerators	*/ False,
	/* geo_matrix_create		*/ NULL,
	/* focus_moved_proc		*/ NULL,
	/* extension			*/ NULL,
    },
};

WidgetClass xmBulletinBoardWidgetClass = (WidgetClass)&xmBulletinBoardClassRec;

static void 
class_initialize()
{
    _XmBulletinBCoreClassExtRec.record_type = XmQmotif;

    mapTrans = XtParseTranslationTable(_XmBulletinB_mapTranslations);
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmBulletinBoardWidgetClass bbclass = (XmBulletinBoardWidgetClass) widget_class;
    XmBulletinBoardWidgetClass sclass =
	(XmBulletinBoardWidgetClass) (widget_class->core_class.superclass);
    CompositeClassExtension ext, *extptr;

    extptr = (CompositeClassExtension*)_XmGetClassExtensionPtr(
		(XmGenericClassExt*)&(bbclass->composite_class.extension),
		NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension) XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = bbclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    bbclass->composite_class.extension = (XtPointer) ext;
	}
    }    
    /* MLM: This seems to be necessary in XFree86/R6 on linux */
    else if (!(*extptr)->accepts_objects)
	(*extptr)->accepts_objects = True;

    if (bbclass->bulletin_board_class.geo_matrix_create ==
	XmInheritGeoMatrixCreate && widget_class != xmBulletinBoardWidgetClass)
	bbclass->bulletin_board_class.geo_matrix_create = 
		sclass->bulletin_board_class.geo_matrix_create;

    if (bbclass->bulletin_board_class.focus_moved_proc ==
	XmInheritFocusMovedProc)
    {
	if (sclass->bulletin_board_class.focus_moved_proc)
	    bbclass->bulletin_board_class.focus_moved_proc =
		sclass->bulletin_board_class.focus_moved_proc;
	else
	    bbclass->bulletin_board_class.focus_moved_proc =
		_XmBulletinBoardFocusMoved;
    }

    _XmFastSubclassInit(widget_class, XmBULLETIN_BOARD_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmBulletinBoardWidget bb = (XmBulletinBoardWidget)new_w;
    XmBulletinBoardWidgetClass bbclass;

    if (!BB_LabelFontList(bb))
	BB_LabelFontList(bb) = _XmGetDefaultFontList(new_w, XmLABEL_FONTLIST);

    if (!BB_ButtonFontList(bb))
	BB_ButtonFontList(bb) = _XmGetDefaultFontList(new_w, XmBUTTON_FONTLIST);

    if (!BB_TextFontList(bb))
	BB_TextFontList(bb) = _XmGetDefaultFontList(new_w, XmTEXT_FONTLIST);

    XtAugmentTranslations(new_w, mapTrans);

    if (XmIsDialogShell(XtParent(new_w))) {

	switch (BB_DialogStyle(new_w)) {
	case XmDIALOG_MODELESS:
	    XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			  MWM_INPUT_MODELESS, NULL);
	    break;
	case XmDIALOG_PRIMARY_APPLICATION_MODAL:
	    XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			  MWM_INPUT_PRIMARY_APPLICATION_MODAL, NULL);
	    break;
	case XmDIALOG_FULL_APPLICATION_MODAL:
	    XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			  MWM_INPUT_FULL_APPLICATION_MODAL, NULL);
	    break;
	case XmDIALOG_SYSTEM_MODAL:
	    XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			  MWM_INPUT_SYSTEM_MODAL, NULL);
	    break;
	}
	/*
	 * --aldi: Better do the realize call of our parent -- which is a
	 * shell -- first here. Otherwise we have *real trouble* with
	 * the grab mechanism. This replaces the call to XtRealizeWidget()
	 * within the initialize_posthook() method of the BulletinBoard
	 * class. And it does get us rid off that %"&!$ double installed
	 * grabs when using a dialog shell.
	 */
	XtRealizeWidget(XtParent(new_w));

	/*
	 * Shouldn't this be in synthetic resource handlers?
	 *
	 * MLM: No.  The bottom line is that the resource converter can
	 * bypass the arglist passed to initialize; this means that it
	 * makes just as much sense to catch this here as it does in a
	 * Synth resource handler, and duplicating the work here.  This
	 * little jewel of knowledge came from Danny and I working out the
	 * rules for internal strings, as applicable for Labels.
	 */
	if (BB_DialogTitle(new_w))
	{
	    char *p;

	    if (XmStringGetLtoR(BB_DialogTitle(new_w),
				XmFONTLIST_DEFAULT_TAG, &p)) {
		XtVaSetValues(XtParent(new_w), XtNtitle, p, NULL);
		XtFree(p);
	    }
	    BB_DialogTitle(new_w) = XmStringCopy(BB_DialogTitle(new_w));
	}
    }

    if (XtIsSubclass(XtParent(new_w), xmDialogShellWidgetClass) ||
	XtIsSubclass(XtParent(new_w), vendorShellWidgetClass)) {
	if (MGR_ShadowThickness(new_w) == 0)
	    MGR_ShadowThickness(new_w) = 1;
    }

    BB_DynamicDefaultButton(new_w) = NULL;
    BB_DynamicCancelButton(new_w) = NULL;

    bbclass = (XmBulletinBoardWidgetClass)XtClass(new_w);
    if (bbclass->bulletin_board_class.focus_moved_proc) {
	Widget ve;

	if ((ve = _LtFindVendorExt(new_w)) != NULL) {
	    XtAddCallback(ve, XmNfocusMovedCallback,
			  bbclass->bulletin_board_class.focus_moved_proc, new_w);
	}
    }

    /* initialize these to values that aren't possible */
    BB_OldWidth(new_w) = -1;
    BB_OldHeight(new_w) = -1;

    BB_GeoCache(new_w) = NULL;
    BB_InSetValues(new_w) = False;
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
    XmBulletinBoardWidget ow = (XmBulletinBoardWidget) old;
    XmBulletinBoardWidget nw = (XmBulletinBoardWidget) new_w;
    Boolean need_refresh = False;

    BB_InSetValues(new_w) = True;

    XdbDebug(__FILE__, new_w, "SetValues\n");
    if (BB_DefaultButton(ow) != BB_DefaultButton(nw))
    {
	/* the default button has changed, so set the old one's showAsDefault
	   to 0, and make the new one display as the default. */

	_XmBBUpdateDynDefaultButton(new_w);

	need_refresh = True; /* do we need to ?*/
    }

    if (XmIsDialogShell(XtParent(new_w))) {
	/*
	 * Shouldn't this be in synthetic resource handlers ?
	 * MLM 960524 not according to a dump of the synthetics.  Only an
	 * export proc is given, to copy the existing title.
	 */
	if (!XmStringCompare(BB_DialogTitle(old), BB_DialogTitle(new_w))) {
	    char	*p;

	    if (XmStringGetLtoR(BB_DialogTitle(new_w),
				XmFONTLIST_DEFAULT_TAG,
				&p)) {
		XtVaSetValues(XtParent(new_w), XtNtitle, p, NULL);
		XtFree(p);
		BB_DialogTitle(new_w) = XmStringCopy(BB_DialogTitle(new_w));
	    }
	    XmStringFree(BB_DialogTitle(old));
	}

	/*
	 * If we're a dialog, refuse to be placed away from 0,0
	 */
	if (XtX(new_w) != 0) {
	    XtX(new_w) = 0;
	    need_refresh = True;
	}

	if (XtY(new_w) != 0) {
	    XtY(new_w) = 0;
	    need_refresh = True;
	}
    }
    if (XmIsDialogShell(XtParent(new_w))) {
	if (BB_DialogStyle(new_w) != BB_DialogStyle(old)) {
	    switch (BB_DialogStyle(new_w)) {
	    case XmDIALOG_MODELESS:
		XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			      MWM_INPUT_MODELESS, NULL);
		break;
	    case XmDIALOG_PRIMARY_APPLICATION_MODAL:
		XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			      MWM_INPUT_PRIMARY_APPLICATION_MODAL, NULL);
		break;
	    case XmDIALOG_FULL_APPLICATION_MODAL:
		XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			      MWM_INPUT_FULL_APPLICATION_MODAL, NULL);
		break;
	    case XmDIALOG_SYSTEM_MODAL:
		XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			      MWM_INPUT_SYSTEM_MODAL, NULL);
		break;
	    }
	}
    }

    BB_InSetValues(new_w) = False;

    if (XtWidth(new_w) != XtWidth(old) || XtHeight(new_w) != XtHeight(old)) {
	need_refresh = True;
    }

    if (need_refresh == True && XtClass(new_w) == xmBulletinBoardWidgetClass)
    {
	_XmBulletinBoardSizeUpdate(new_w);
	return False;
    }
    return need_refresh;
}

static XtGeometryResult 
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    XmBulletinBoardWidgetClass bbc = (XmBulletinBoardWidgetClass)XtClass(w);
    XtGeometryResult res;

    XdbDebug(__FILE__, w, "QueryGeometry\n");

    if (bbc->bulletin_board_class.geo_matrix_create) {
	return _XmHandleQueryGeometry(w, proposed, answer, BB_ResizePolicy(w),
				      bbc->bulletin_board_class.geo_matrix_create);
    }

    res = _XmGMHandleQueryGeometry(w, proposed, answer,
				   BB_MarginWidth(w), BB_MarginHeight(w),
				   BB_ResizePolicy(w));
    XdbDebug(__FILE__, w, "BB wants %d %d\n", answer->width, answer->height);

    return res;
}

static void
handle_resize(Widget w, XmGeoCreateProc mat_make)
{
    Dimension wd, ht;
    XmGeoMatrix geo;

    wd = XtWidth(w);
    ht = XtHeight(w);

    geo = mat_make(w, NULL, NULL);

    _XmGeoMatrixGet(geo, XmGET_PREFERRED_SIZE);

    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);

    _XmGeoMatrixSet(geo);

    if (XtIsRealized(w)) {
	_XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
			   BB_OldShadowThickness(w), 0);

	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    _XmGeoMatrixFree(geo);

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);
}

static void
resize(Widget w)
{
    XmBulletinBoardClassRec *bb = (XmBulletinBoardClassRec *)XtClass(w);

    XdbDebug(__FILE__, NULL, "resize\n");

    if (bb->bulletin_board_class.geo_matrix_create) {
	handle_resize(w, bb->bulletin_board_class.geo_matrix_create);
	return;
    }

    _XmGMEnforceMargin(w,
		       BB_MarginWidth(w), BB_MarginHeight(w),
		       False);

    _XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
		       BB_OldShadowThickness(w), 0);

    BB_OldShadowThickness(w) = 0;

    if (XtIsRealized(w) || XtWidth(w) == 0 || XtHeight(w) == 0) {
	_XmGMDoLayout(w, BB_MarginWidth(w), BB_MarginHeight(w),
		      BB_ResizePolicy(w), True);
    }

    if ((XtWidth(w) < BB_OldWidth(w) || XtHeight(w) < BB_OldHeight(w)) &&
	XtIsRealized(w)) {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);
}

/*
 * handle subclasses that use the GeoCache
 */
static XtGeometryResult
handle_geometry_manager(Widget w,
			XtWidgetGeometry *desired, XtWidgetGeometry *allowed,
			XmGeoCreateProc mat_make)
{
    Widget bb = XtParent(w);
    XtGeometryResult res;

    XdbDebug2(__FILE__, bb, w,
	      "handle_geometry_manager\n");

    if (!(desired->request_mode & (CWWidth|CWHeight)))
	return XtGeometryYes;

    if (BB_OldShadowThickness(bb) != 0 ||
	BB_ResizePolicy(bb) != XmRESIZE_NONE) {
	_XmClearShadowType(bb, BB_OldWidth(bb), BB_OldHeight(bb),
			   BB_OldShadowThickness(bb), 0);
	BB_OldShadowThickness(bb) = 0;
    }

    res = _XmHandleGeometryManager(bb, w, desired, allowed,
				   BB_ResizePolicy(bb), &BB_GeoCache(bb),
				   mat_make);

    if (!BB_InSetValues(bb) ||
	XtWidth(bb) > BB_OldWidth(bb) || XtHeight(bb) > BB_OldHeight(bb)) {
	if (XtIsRealized(bb)) {
	    _XmDrawShadows(XtDisplay(bb), XtWindow(bb),
			 MGR_TopShadowGC(bb), MGR_BottomShadowGC(bb),
			 0, 0, XtWidth(bb), XtHeight(bb),
			 MGR_ShadowThickness(bb), BB_ShadowType(bb));
	}
    }
    BB_OldWidth(bb) = XtWidth(bb);
    BB_OldHeight(bb) = XtHeight(bb);

    return res;
}
			
/*
 * Geometry Manager is always called by a child of BB.
 *	It asks to get a different geometry. We may allow this.
 *	Also we may have to resize ourself because of this.
 */
static XtGeometryResult
geometry_manager(Widget w, XtWidgetGeometry *desired, XtWidgetGeometry *allowed)
{
    Widget bb = XtParent(w);
    XmBulletinBoardWidgetClass bbc = (XmBulletinBoardWidgetClass)XtClass(bb);

    if (bbc->bulletin_board_class.geo_matrix_create) {
	return handle_geometry_manager(w, desired, allowed,
				       bbc->bulletin_board_class.geo_matrix_create);
    }
    return _XmGMHandleGeometryManager(bb, w, desired, allowed,
				      BB_MarginWidth(bb), BB_MarginHeight(bb),
				      BB_ResizePolicy(bb), BB_AllowOverlap(bb));
}

static void
handle_change_managed(Widget w, XmGeoCreateProc mat_make)
{
    Dimension wd, ht;
    XmGeoMatrix geo;
    XtWidgetGeometry request;

    if (!XtIsRealized(w))
	wd = ht = 0;

    else if (BB_ResizePolicy(w) != XmNONE)
	wd = ht = 0;

    else {
	wd = XtWidth(w);
	ht = XtHeight(w);
    }

    geo = mat_make(w, NULL, NULL);

    _XmGeoMatrixGet(geo, XmGET_PREFERRED_SIZE);

    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);

    if (BB_ResizePolicy(w) == XmRESIZE_GROW) {
	/* check the return against the original.  If the procedure would
	 * like the BB to shrink, call again */
	if (wd < XtWidth(w) || ht < XtHeight(w)) {
	    wd = XtWidth(w);
	    ht = XtHeight(w);
	    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);
	}
    }

    if (wd == XtWidth(w) && ht == XtHeight(w)) {
	_XmGeoMatrixFree(geo);
	_XmNavigChangeManaged(w);
	return;
    }

    request.request_mode = (CWWidth|CWHeight);
    request.width = wd;
    request.height = ht;
    _XmMakeGeometryRequest(w, &request);

    if (request.width != wd || request.height != ht)
	_XmGeoArrangeBoxes(geo, 0, 0, &request.width, &request.height);

    _XmGeoMatrixSet(geo);

    if (XtIsRealized(w)) {
	_XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
			   BB_OldShadowThickness(w), 0);

	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    _XmGeoMatrixFree(geo);

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);

    _XmNavigChangeManaged(w);
}

static void
change_managed(Widget w)
{
    XmBulletinBoardClassRec *bb = (XmBulletinBoardClassRec *)XtClass(w);

    XdbDebug(__FILE__, w, "ChangeManaged\n");

    if (bb->bulletin_board_class.geo_matrix_create) {
	handle_change_managed(w, bb->bulletin_board_class.geo_matrix_create);
	return;
    }

    _XmGMEnforceMargin(w,
		       BB_MarginWidth(w), BB_MarginHeight(w),
		       False);

    _XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
		       BB_OldShadowThickness(w), 0);

    BB_OldShadowThickness(w) = 0;

    if (XtIsRealized(w) || XtWidth(w) == 0 || XtHeight(w) == 0) {
	_XmGMDoLayout(w, BB_MarginWidth(w), BB_MarginHeight(w),
		      BB_ResizePolicy(w), False);
    }

    if ((XtWidth(w) < BB_OldWidth(w) || XtHeight(w) < BB_OldHeight(w)) &&
	XtIsRealized(w)) {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);

    _XmNavigChangeManaged(w);
}

Widget 
XmCreateBulletinBoard(Widget parent,
		      char *name,
		      Arg *arglist,
		      Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmBulletinBoardWidgetClass,
			  parent,
			  arglist,
			  argcount);
}

Widget 
XmCreateBulletinBoardDialog(Widget parent,
			    char *name,
			    Arg *arglist,
			    Cardinal argcount)
{
    Widget  d;
    char    *s;

    s = _XmMakeDialogName(name);

    d = XtCreateWidget(s, xmDialogShellWidgetClass, parent, arglist, argcount);
    XtFree(s);

    return XtCreateWidget(name, xmBulletinBoardWidgetClass, d, arglist, argcount);
}

static void
ConstraintInitialize(Widget request, Widget new_w, Arg *args, Cardinal *nargs)
{
        XdbDebug(__FILE__, new_w, "ConstraintInitialize\n");
}

static Boolean
ConstraintSetValues(Widget old, Widget request, Widget new_w, Arg *args, Cardinal *nargs)
{
        XmBulletinBoardWidget   bb = (XmBulletinBoardWidget) XtParent(new_w);

        XdbDebug2(__FILE__, (Widget)bb, new_w, "ConstraintSetValues\n");

        return False;
}

static void
handle_realize(Widget w, XmGeoCreateProc mat_make)
{
    Dimension wd, ht;
    XmGeoMatrix geo;
    XtWidgetGeometry request;

    wd = XtWidth(w);
    ht = XtHeight(w);

    geo = mat_make(w, NULL, NULL);

    _XmGeoMatrixGet(geo, XmGET_PREFERRED_SIZE);

    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);

    if (BB_ResizePolicy(w) == XmRESIZE_GROW) {
	/* check the return against the original.  If the procedure would
	 * like the BB to shrink, call again */
	if (wd < XtWidth(w) || ht < XtHeight(w)) {
	    wd = XtWidth(w);
	    ht = XtHeight(w);
	    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);
	}
    }

    if (wd == XtWidth(w) && ht == XtHeight(w)) {
#if 0
/*
 * this SHOULDN'T be necessary, but somebody isn't playing by the rules
 * (RowColumn, judging by smartmb/test1, selectionbox/test4)
 */
	_XmGeoMatrixSet(geo);
#endif
	_XmGeoMatrixFree(geo);
	return;
    }

    request.request_mode = (CWWidth|CWHeight);
    request.width = wd;
    request.height = ht;
    _XmMakeGeometryRequest(w, &request);

    if (request.width != wd || request.height != ht)
	_XmGeoArrangeBoxes(geo, 0, 0, &request.width, &request.height);

    _XmGeoMatrixSet(geo);

    if (XtIsRealized(w)) {

	_XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
			   BB_OldShadowThickness(w), 0);

	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    _XmGeoMatrixFree(geo);

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);
}

static void 
realize(Widget w,
        XtValueMask *value_mask, 
        XSetWindowAttributes *attributes)
{
    XmBulletinBoardClassRec *bb = (XmBulletinBoardClassRec *)XtClass(w);

    XdbDebug(__FILE__, w, "Realize\n");

#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize)(w, value_mask, attributes);
#undef superclass

    if (bb->bulletin_board_class.geo_matrix_create) {
	handle_realize(w, bb->bulletin_board_class.geo_matrix_create);
	return;
    }

    _XmGMEnforceMargin(w,
		       BB_MarginWidth(w), BB_MarginHeight(w),
		       False);

    _XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
		       BB_OldShadowThickness(w), 0);

    BB_OldShadowThickness(w) = 0;

    _XmGMDoLayout(w, BB_MarginWidth(w), BB_MarginHeight(w),
		  BB_ResizePolicy(w), False);

    if (XtWidth(w) < BB_OldWidth(w) || XtHeight(w) < BB_OldHeight(w)) {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);
}

static void
expose(Widget w, XEvent *event, Region region)
{
    _XmRedisplayGadgets(w, event, region);

    if (MGR_ShadowThickness(w)) {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }
}

static void
_XmBbButton(Widget w, XtPointer client, XtPointer call)
{
    XmBulletinBoardWidget bb = (XmBulletinBoardWidget) XtParent(w);

    XdbDebug2(__FILE__, (Widget)bb, w, "XmBbButton\n");

    /*
     * How do we make sure the APPLY or HELP buttons don't
     * trigger unmanaging ??  
     *
     * Somebody already caught this.  See comments in SelectionBox.c (look
     * for auto_unmanage).
     */
    if (BB_AutoUnmanage(bb)) {
	Widget	s = XtParent((Widget)bb);

	if (XtIsSubclass(s, xmDialogShellWidgetClass)) {
	    XtUnmanageChild((Widget)bb);
	    XdbDebug2(__FILE__, (Widget)bb, w, "AutoUnmanage\n");

	    /* XtNpopdownCallback */
	    XtCallCallbackList(s, Shell_PopdownCallback(s), NULL);
	}
    }
}

/*
 * Keep track of button children
 */
static void
InsertChild(Widget w)
{
#define	superclass	(&xmManagerClassRec)
    (*superclass->composite_class.insert_child)(w);
#undef	superclass

    if (_XmIsFastSubclass(XtClass(w), XmPUSH_BUTTON_GADGET_BIT) ||
	_XmIsFastSubclass(XtClass(w), XmPUSH_BUTTON_BIT))
    {
	XtAddCallback(w, XmNactivateCallback, _XmBbButton, NULL);
    }
}

static void
DeleteChild(Widget w)
{
    Widget	bb = XtParent(w);

    XdbDebug2(__FILE__, bb, w, "DeleteChild\n");

#define	superclass	(&xmManagerClassRec)
    (*superclass->composite_class.delete_child)(w);
#undef superclass

    if (w == BB_CancelButton(bb))
	BB_CancelButton(bb) = NULL;
}

static Boolean
_XmBBParentProcess(Widget widget, XmParentProcessData data)
{
    if (data->input_action.process_type == XmINPUT_ACTION)
    {
	if (data->input_action.action == XmPARENT_ACTIVATE)
	{
	    _XmBulletinBoardReturn(widget,
				   data->input_action.event,
				   data->input_action.params,
				   data->input_action.num_params);
	    return True;
	}
	else if (data->input_action.action == XmPARENT_CANCEL)
	{
	    _XmBulletinBoardCancel(widget,
				   data->input_action.event,
				   data->input_action.params,
				   data->input_action.num_params);
	    return True;
	}
    }
    return False;
}

Widget
_XmBB_CreateButtonG(Widget bb, XmString l_string, char *name)
{
    Widget button;
    Arg args[2];

    XtSetArg(args[0], XmNlabelString, l_string);
#ifdef USE_WIDGETS
    button = XmCreatePushButton(bb, name, args, 1);
#else
    button = XmCreatePushButtonGadget(bb, name, args, 1);
#endif
    _XmBulletinBoardSetDefaultShadow(button);
    return button;
}

Widget
_XmBB_CreateLabelG(Widget bb, XmString l_string, char *name)
{
    Widget label;
    Arg args[2];

    XtSetArg(args[0], XmNlabelString, l_string);

#ifdef USE_WIDGETS
    label = XmCreateLabel(bb, name, args, 1);
#else
    label = XmCreateLabelGadget(bb, name, args, 1);
#endif
    return label;
}

void
_XmBulletinBoardSizeUpdate(Widget w)
{
    XmBulletinBoardWidgetClass bbc = (XmBulletinBoardWidgetClass)XtClass(w);

    if (!XtIsRealized(w))
	return;

    if (bbc->bulletin_board_class.geo_matrix_create == NULL) {
	BB_OldWidth(w) = XtWidth(w);
	BB_OldHeight(w) = XtHeight(w);
	return;
    }
    if (!BB_OldShadowThickness(w) && BB_ResizePolicy(w) != XmRESIZE_NONE) {
	_XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
			   BB_OldShadowThickness(w), 0);
	BB_OldShadowThickness(w) = 0;
    }

    _XmHandleSizeUpdate(w, BB_ResizePolicy(w),
			bbc->bulletin_board_class.geo_matrix_create);

    if ((XtWidth(w) < BB_OldWidth(w) || XtHeight(w) < BB_OldHeight(w)) &&
	XtIsRealized(w)) {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);
}

void
_XmBulletinBoardFocusMoved(Widget wid, XtPointer client_data, XtPointer data)
{
    XmFocusMovedCallback cbs = (XmFocusMovedCallback)data;
    Widget bb = (Widget)client_data, par;
    Boolean call = False;
    XmAnyCallbackStruct cb;

    if (cbs->new_focus == wid)
	call = True;

    else if (cbs->new_focus) {
	par = XtParent(cbs->new_focus);
	for (;;) {
	    if (par == bb) {
		call = True;
		if (!XmIsForm(bb) && XtParent(cbs->new_focus) == bb &&
		    XtIsShell(XtParent(bb)))
		    _XmBulletinBoardSetDynDefaultButton(bb, cbs->new_focus);
		break;
	    }
	    if (XtIsShell(par))
		break;
	    if (!par)
		break;
	    par = XtParent(par);
	}
    }

    if (call && BB_FocusCallback(bb)) {
	cb.reason = XmCR_FOCUS;
	cb.event = cbs->event;
	XtCallCallbackList(bb, BB_FocusCallback(bb), data);
    }

    cbs->cont = True;
}

void
_XmBulletinBoardMap(Widget w, XEvent *event,
		    String *params, Cardinal *numParams)
{
    XMapEvent *mev = (XMapEvent *)event;
    XUnmapEvent *uev = (XUnmapEvent *)event;
    XmAnyCallbackStruct cb;

    cb.event = event;
    if (mev->type == MapNotify)
    {
	XdbDebug(__FILE__, w, "Map\n");
	cb.reason = XmCR_MAP;
	XtCallCallbackList(w, BB_MapCallback(w), (XtPointer)&cb);
    }
    else if (uev->type == UnmapNotify)
    {
	XdbDebug(__FILE__, w, "Unmap\n");
	cb.reason = XmCR_UNMAP;
	XtCallCallbackList(w, BB_UnmapCallback(w), (XtPointer)&cb);
    }
}

void
_XmBulletinBoardReturn(Widget wid, XEvent *event,
		       String *params, Cardinal *numParams)
{
    /* FIXME -- this needs filling in */
}

void
_XmBulletinBoardCancel(Widget wid, XEvent *event,
		       String *params, Cardinal *numParams)
{
    XmPushButtonCallbackStruct cbs;

    cbs.reason = XmCR_ACTIVATE;
    cbs.event = event;
    if (BB_CancelButton(wid) && XtIsSensitive(BB_CancelButton(wid)) &&
	XtIsManaged(BB_CancelButton(wid)) && XtIsRealized(BB_CancelButton(wid)))
    {
	XtCallCallbacks(BB_CancelButton(wid), XmNactivateCallback, &cbs);
    }
    else if (BB_DynamicCancelButton(wid) &&
	     XtIsSensitive(BB_DynamicCancelButton(wid)) &&
	     XtIsManaged(BB_DynamicCancelButton(wid)) &&
	     XtIsRealized(BB_DynamicCancelButton(wid)))
    {
	XtCallCallbacks(BB_DynamicCancelButton(wid), XmNactivateCallback, &cbs);
    }
   
}

void
_XmBulletinBoardSetDefaultShadow(Widget button)
{
    Dimension st, dbst;
    Arg args[2];

    if (_XmIsFastSubclass(XtClass(button), XmPUSH_BUTTON_GADGET_BIT) ||
	_XmIsFastSubclass(XtClass(button), XmPUSH_BUTTON_BIT)) {

	XtSetArg(args[0], XmNdefaultButtonShadowThickness, &dbst);
	XtSetArg(args[1], XmNshadowThickness, &st);
	XtGetValues(button, args, 2);
	if (st > 1)
	    st >>= 1;
	XtSetArg(args[0], XmNdefaultButtonShadowThickness, st);
	XtSetValues(button, args, 1);
    }
}

void
_XmBulletinBoardSetDynDefaultButton(Widget wid, Widget newDefaultButton)
{
    Arg argl[2];

    if (BB_DynamicDefaultButton(wid) != NULL) {
	if (!BB_DynamicDefaultButton(wid)->core.being_destroyed) {
	    XtSetArg(argl[0], XmNshowAsDefault, 0);
	    XtSetValues(BB_DynamicDefaultButton(wid), argl, 1);
	}
    }

    if (newDefaultButton == NULL) {
	BB_DynamicDefaultButton(wid) = NULL;
	return;
    }

    if (_XmIsFastSubclass(XtClass(newDefaultButton), XmPUSH_BUTTON_GADGET_BIT) ||
	_XmIsFastSubclass(XtClass(newDefaultButton), XmPUSH_BUTTON_BIT)) {
	BB_DynamicDefaultButton(wid) = newDefaultButton;
	XtSetArg(argl[0], XmNshowAsDefault, 1);
	XtSetValues(BB_DynamicDefaultButton(wid), argl, 1);
    }
}

void
_XmBBUpdateDynDefaultButton(Widget bb)
{
    if (BB_DynamicDefaultButton(bb))
	_XmBulletinBoardSetDynDefaultButton(bb, BB_DynamicDefaultButton(bb));
    else
	_XmBulletinBoardSetDynDefaultButton(bb, BB_DefaultButton(bb));
}

