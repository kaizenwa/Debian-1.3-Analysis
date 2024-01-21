/**
 *
 * $Id: RowColumn.c,v 1.35 1997/01/13 07:08:55 u27113 Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
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

static char rcsid[] = "$Id: RowColumn.c,v 1.35 1997/01/13 07:08:55 u27113 Exp $";

/*
 * This doesn't work (yet) .
 */
#undef	GEO_TIGHT_STRICT
#undef	VT_DO_LAST

/*
 * These are absolutely necessary for some apps, don't bother some others.
 */
#define	VC_HAS_EQUAL_WIDTH_TOO
#define	DO_SANITY

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <X11/Xfuncs.h>
#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/DrawnBP.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/ManagerP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/PushBP.h>
#include <Xm/PushBGP.h>
#include <Xm/RepType.h>
#include <Xm/RowColumnP.h>
#include <Xm/ScreenP.h>
#include <Xm/Separator.h>
#include <Xm/SeparatoG.h>
#include <Xm/TearOffBP.h>
#include <Xm/ToggleBP.h>
#include <Xm/ToggleBGP.h>
#include <Xm/TransltnsP.h>

#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <XmI/DebugUtil.h>

/*
 * builtin widgets for option menus
 */
#define RC_OPTION_LABEL		"OptionLabel"
#define RC_OPTION_CBG		"OptionButton"

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, 
		       Cardinal * num_args);
static void initialize_prehook(Widget request, Widget new_w, ArgList args, 
			       Cardinal * num_args);
static void destroy(Widget w);
static void resize(Widget w);
static void expose(Widget w, XEvent * event, Region region);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry * proposed, 
				       XtWidgetGeometry * answer);
static Boolean set_values(Widget current, Widget request, Widget new_w, 
			  ArgList args, Cardinal * num_args);
static XtGeometryResult RcGeometryManager(Widget w, XtWidgetGeometry * request,
					 XtWidgetGeometry * reply);
static void change_managed(Widget w);
static void insert_child(Widget w);
static void DeleteChild(Widget w);
static void XmRowColumnLayout(Widget rc, int mode, Widget cw, XtWidgetGeometry *cg, XtWidgetGeometry *rcgp);
static void DoLayoutHT(Widget rc, int mode, Widget cw, XtWidgetGeometry *cg, XtWidgetGeometry *rcg);
static void DoLayoutVT(Widget rc, int mode, Widget cw, XtWidgetGeometry *cg, XtWidgetGeometry *rcg);
static void DoLayoutVC(Widget rc, int mode, Widget cw, XtWidgetGeometry *cg, XtWidgetGeometry *rcg);
static void DoLayoutHC(Widget rc, int mode, Widget cw, XtWidgetGeometry *cg, XtWidgetGeometry *rcg);
static void _XmRadioCallback(Widget w, XtPointer cd, XtPointer cbs);
static void _XmEntryCallback(Widget w, XtPointer cd, XtPointer cbs);
static void initialize_boxes(Widget w, Widget instig, XtWidgetGeometry *instig_request);
static Boolean RcConstraintSetValues(Widget current, Widget request, Widget new, ArgList args, Cardinal *num_args);

static void _XmFromMenuPost(Widget widget, int offset, XtArgVal *value);
static XmImportOperator _XmToMenuPost(Widget widget, int offset, 
				     XtArgVal *value);

static Cardinal _XmRowColumnOrderProc(Widget widget);

#define Offset(field) XtOffsetOf(XmRowColumnRec, row_column.field)
#define MGR_Offset(field) XtOffsetOf(XmRowColumnRec, manager.field)
#define COMP_Offset(field) XtOffsetOf(XmRowColumnRec, composite.field)

/* Resources for the RowColumn class */
static XtResource resources[] =
{
    {
	XmNresizeWidth, XmCResizeWidth, XmRBoolean,
	sizeof(Boolean), Offset(resize_width),
	XmRImmediate, (XtPointer) True
    },
    {
	XmNresizeHeight, XmCResizeHeight, XmRBoolean,
	sizeof(Boolean), Offset(resize_height),
	XmRImmediate, (XtPointer) True
    },
    {
	XmNwhichButton, XmCWhichButton, XmRWhichButton,
	sizeof(unsigned int), Offset(postButton),
	XmRImmediate, (XtPointer) XmUNSPECIFIED
	/* add support for this!!! */
    },
    {
	XmNmenuPost, XmCMenuPost, XmRString,
	sizeof(String), Offset(menuPost),
	XmRString, (XtPointer) NULL
    },
    {
	XmNadjustLast, XmCAdjustLast, XmRBoolean,
	sizeof(Boolean), Offset(adjust_last),
	XmRImmediate, (XtPointer) True
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XtRCallProc, (XtPointer) _XmRowColumnMarginDefault
	/*	XmRImmediate, (XtPointer)XmINVALID_DIMENSION*/
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XtRCallProc, (XtPointer) _XmRowColumnMarginDefault
	/*XmRImmediate, (XtPointer)XmINVALID_DIMENSION*/
    },
    {
	XmNentryCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(entry_callback),
	XmRCallback, (XtPointer) NULL
    },
    {
	XmNmapCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(map_callback),
	XmRCallback, (XtPointer) NULL
    },
    {
	XmNunmapCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(unmap_callback),
	XmRCallback, (XtPointer) NULL
    },
    {
	XmNorientation, XmCOrientation, XmROrientation,
	sizeof(unsigned char), Offset(orientation),
	XmRImmediate, (XtPointer) XmVERTICAL
	/*FIXME: Should be XmROrientation, (XtPointer)some whacko thing here */
    },
    {
	XmNspacing, XmCSpacing, XmRHorizontalDimension,
	sizeof(Dimension), Offset(spacing),
	XtRCallProc, (XtPointer) _XmRowColumnSpacingDefault
	/*XmRImmediate, (XtPointer)XmINVALID_DIMENSION*/
    },
    {
	XmNentryBorder, XmCEntryBorder, XmRHorizontalDimension,
	sizeof(Dimension), Offset(entry_border),
	XmRImmediate, (XtPointer) 0
    },
    {
	XmNisAligned, XmCIsAligned, XmRBoolean,
	sizeof(Boolean), Offset(do_alignment),
	XmRImmediate, (XtPointer) True
	/* FIXME: Motif has XmRBoolean, (XtPointer)whacko value */
    },
    {
	XmNentryAlignment, XmCAlignment, XmRAlignment,
	sizeof(unsigned char), Offset(entry_alignment),
	XtRImmediate, (XtPointer) XmALIGNMENT_BEGINNING
	/* FIXME: Motif has XmRAlignment, (XtPointer)whacko value */
    },
    {
	XmNadjustMargin, XmCAdjustMargin, XmRBoolean,
	sizeof(Boolean), Offset(adjust_margin),
	XtRImmediate, (XtPointer) True
	/* FIXME: Motif has XmRBoolean, (XtPointer)whacko value */
    },
    {
	XmNnumColumns, XmCNumColumns, XmRShort,
	sizeof(short), Offset(num_columns),
	XtRImmediate, (XtPointer) 1
	/* FIXME: Motif has XmRShort, (XtPointer)whacko value */
    },
    {
	XmNradioBehavior, XmCRadioBehavior, XmRBoolean,
	sizeof(Boolean), Offset(radio),
	XmRImmediate, (XtPointer) False
	/* FIXME: Motif has XmRBoolean, (XtPointer)whacko value */
    },
    {
	XmNradioAlwaysOne, XmCRadioAlwaysOne, XmRBoolean,
	sizeof(Boolean), Offset(radio_one),
	XmRImmediate, (XtPointer) True
	/* FIXME: Motif has XmRBoolean, (XtPointer)whacko value */
    },
    {
	XmNisHomogeneous, XmCIsHomogeneous, XmRBoolean,
	sizeof(Boolean), Offset(homogeneous),
	XtRCallProc, (XtPointer) _XmRowColumnIsHomogeneousDefault
	/* FIXME: Motif has XmRBoolean, (XtPointer)whacko value */
    },
    {
	XmNentryClass, XmCEntryClass, XmRWidgetClass,
	sizeof(WidgetClass), Offset(entry_class),
	XmRCallProc, (XtPointer) _XmRowColumnEntryClassDefault
	/* Motif has XmRWidgetClass, (XtPointer)NULL */
    },
    {
	XmNrowColumnType, XmCRowColumnType, XmRRowColumnType,
	sizeof(unsigned char), Offset(type),
	XtRImmediate, (XtPointer) XmWORK_AREA
	/* FIXME: Motif has XmRRowColumnType, (XtPointer)whacko value */
    },
    {
	XmNmenuHelpWidget, XmCMenuWidget, XmRMenuWidget,
	sizeof(Widget), Offset(help_pushbutton),
	XmRImmediate, (XtPointer) NULL
	/* FIXME: Motif has XmRMenuWidget, (XtPointer)whacko value */
    },
    {
	XmNlabelString, XmCXmString, XmRXmString,
	sizeof(XmString), Offset(option_label),
	XmRXmString, (XtPointer) NULL
    },
    {
	XmNsubMenuId, XmCMenuWidget, XmRMenuWidget,
	sizeof(Widget), Offset(option_submenu),
	XmRImmediate, (XtPointer) NULL
	/* FIXME: Motif has XmRMenuWidget, (XtPointer)whacko value */
    },
    {
	XmNmenuHistory, XmCMenuWidget, XmRMenuWidget,
	sizeof(Widget), Offset(memory_subwidget),
	XmRImmediate, (XtPointer) NULL
	/* FIXME: Motif has XmRMenuWidget, (XtPointer)whacko value */
    },
    {
	XmNpopupEnabled, XmCPopupEnabled, XmRBoolean,
	sizeof(Boolean), Offset(popup_enabled),
	XmRImmediate, (XtPointer) True
	/* FIXME: Motif has XmRBoolean, (XtPointer)whacko value */
    },
    {
	XmNmenuAccelerator, XmCAccelerators, XmRString,
	sizeof(String), Offset(menu_accelerator),
	XtRCallProc, (XtPointer) _XmRowColumnMenuAcceleratorDefault
	/* FIXME: Motif has XmRString, (XtPointer)whacko values */
    },
    {
	XmNmnemonic, XmCMnemonic, XmRKeySym,
	sizeof(KeySym), Offset(mnemonic),
	XmRImmediate, (XtPointer) NULL
    },
    {
	XmNmnemonicCharSet, XmCMnemonicCharSet, XmRString,
	sizeof(String), Offset(mnemonicCharSet),
	XtRImmediate, (XtPointer) XmFONTLIST_DEFAULT_TAG
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), MGR_Offset(shadow_thickness),
	XmRImmediate, (XtPointer) 0
	/* FIXME: Motif has XmRImmediate, (XtPointer)XmINVALID_DIMENSION */
    },
    {
	XmNpostFromList, XmCPostFromList, XmRWidgetList,
	sizeof(WidgetList), Offset(postFromList),
	XmRWidgetList, (XtPointer) NULL
    },
    {
	XmNpostFromCount, XmCPostFromCount, XmRInt,
	sizeof(int), Offset(postFromCount),
	XmRImmediate, (XtPointer) XmUNSPECIFIED
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), MGR_Offset(navigation_type),
	XmRImmediate, (XtPointer) ((XmNavigationType) XmUNSPECIFIED)
    },
    {
      XmNentryVerticalAlignment, XmCVerticalAlignment, XmRVerticalAlignment,
	sizeof(unsigned char), Offset(entry_vertical_alignment),
	XtRImmediate, (XtPointer) XmALIGNMENT_BEGINNING
	/* FIXME: Motif has XmRVerticalAlignment, (XtPointer)whacko values */
    },
    {
	XmNtearOffModel, XmCTearOffModel, XmRTearOffModel,
	sizeof(unsigned char), Offset(TearOffModel),
	XtRImmediate, (XtPointer) XmTEAR_OFF_DISABLED
	/* FIXME: Motif has XmRTearOffModel, (XtPointer)whacko values */
    },
    {
	XmNtearOffMenuActivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(tear_off_activated_callback),
	XmRCallback, (XtPointer) NULL
    },
    {
	XmNtearOffMenuDeactivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(tear_off_deactivated_callback),
	XmRCallback, (XtPointer) NULL
    },
    {
	XmNinsertPosition, XmCInsertPosition, XmRFunction,
	sizeof(XtOrderProc), COMP_Offset(insert_position),
	XmRImmediate, (XtPointer) _XmRowColumnOrderProc,
    },
    {
	XmNpacking, XmCPacking, XmRPacking,
	sizeof(unsigned char), Offset(packing),
	XmRImmediate, (XtPointer) XmUNSPECIFIED		/* To be overruled in initialize */
    }
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNmnemonicCharSet,
	sizeof(String), Offset(mnemonicCharSet),
	NULL /* FIXME */ , NULL
    },
    {
	XmNmenuAccelerator,
	sizeof(String), Offset(menu_accelerator),
	NULL /* FIXME */ , NULL
    },
    {
	XmNmenuPost,
	sizeof(String), Offset(menuPost),
	_XmFromMenuPost, _XmToMenuPost,
    },
    {
	XmNlabelString,
	sizeof(XmString), Offset(option_label),
	NULL /* FIXME */ , NULL
    },
    {
	XmNspacing,
	sizeof(Dimension), Offset(spacing),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNmarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNentryBorder,
	sizeof(Dimension), Offset(entry_border),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

#undef Offset

#define Offset(field) XtOffsetOf(XmRowColumnConstraintRec, row_column.field)

static XtResource rowColumnConstraintResources[] =
{
    {
	XmNpositionIndex, XmCPositionIndex, XmRShort,
	sizeof(short), Offset(position_index),
	XmRImmediate, (XtPointer) XmLAST_POSITION
    }
};

/* not all actually written yet... */
#define STATIC_ACTION(fn) static void (fn)(Widget, XEvent*, String*, Cardinal*)
#define ACTION(fn) void (fn)(Widget, XEvent*, String*, Cardinal*)

STATIC_ACTION(MenuBarEnter);
STATIC_ACTION(MenuBarLeave);
STATIC_ACTION(MenuBarGadgetSelect);
STATIC_ACTION(MenuBtnDown);
STATIC_ACTION(MenuBtnUp);
ACTION(_XmMenuHelp);
STATIC_ACTION(MenuEnter);
STATIC_ACTION(MenuUnmap);
STATIC_ACTION(MenuFocusIn);
STATIC_ACTION(MenuFocusOut);
STATIC_ACTION(DoBtnEventCleanupReplay);

static Boolean ExternalBtnEvent(Widget w, XEvent * event);
static void _XmPopupButtonPressHandler(Widget w, XtPointer client_data,
				       XEvent *event, Boolean *cont);
#undef STATIC_ACTION
#undef ACTION

char _XmRowColumn_menu_traversal_table[] =
"<Unmap>:              MenuUnmap()\n\
 <EnterWindow>Normal:  MenuEnter()\n\
 <FocusIn>:            MenuFocusIn()\n\
 <FocusOut>:           MenuFocusOut()\n\
 <Key>osfHelp:         MenuHelp()\n\
 <Key>osfLeft:         MenuGadgetTraverseLeft()\n\
 <Key>osfRight:        MenuGadgetTraverseRight()\n\
 <Key>osfUp:           MenuGadgetTraverseUp()\n\
 <Key>osfDown:         MenuGadgetTraverseDown()";

char _XmRowColumn_bar_table[] =
"<BtnDown>:            MenuBtnDown()\n\
 <BtnUp>:              MenuBtnUp()\n\
 <Key>osfSelect:       MenuBarGadgetSelect()\n\
 <Key>osfActivate:     MenuBarGadgetSelect()\n\
 <Key>osfHelp:         MenuHelp()\n\
 <Key>osfCancel:       MenuGadgetEscape()\n\
 ~s ~m ~a <Key>Return: MenuBarGadgetSelect()\n\
 ~s ~m ~a <Key>space:  MenuBarGadgetSelect()";

char _XmRowColumn_option_table[] =
"<BtnDown>:             MenuBtnDown()\n\
 <BtnUp>:               MenuBtnUp()\n\
 <Key>osfActivate:      ManagerParentActivate()\n\
 <Key>osfCancel:        ManagerParentCancel()\n\
 <Key>osfSelect:        ManagerGadgetSelect()\n\
 <Key>osfHelp:          MenuHelp()\n\
 ~s ~m ~a <Key>Return:  ManagerParentActivate()\n\
 ~s ~m ~a <Key>space:   ManagerGadgetSelect()";

char _XmRowColumn_menu_table[] =
"<BtnDown>:             MenuBtnDown()\n\
 <BtnUp>:               MenuBtnUp()\n\
 <Key>osfActivate:      ManagerGadgetSelect()\n\
 <Key>osfSelect:        ManagerGadgetSelect()\n\
 <Key>osfCancel:        MenuGadgetEscape()\n\
 <Key>osfHelp:          MenuHelp()\n\
 ~s ~m ~a <Key>Return:  ManagerGadgetSelect()\n\
 ~s ~m ~a <Key>space:   ManagerGadgetSelect()";

static XtActionsRec actions[] =
{
    {"MenuBarEnter", MenuBarEnter},
    {"MenuBarLeave", MenuBarLeave},
    {"MenuBarGadgetSelect", MenuBarGadgetSelect},
    {"MenuBtnDown", MenuBtnDown},
    {"MenuBtnUp", MenuBtnUp},
    {"MenuFocusIn", MenuFocusIn},
    {"MenuFocusOut", MenuFocusOut},
    {"MenuHelp", _XmMenuHelp},
    {"MenuUnmap", MenuUnmap},
    {"MenuEnter", MenuEnter},
    {"MenuGadgetEscape", _XmMenuEscape},
    {"MenuGadgetTraverseLeft", _XmRC_GadgetTraverseLeft},
    {"MenuGadgetTraverseRight", _XmRC_GadgetTraverseRight},
    {"MenuGadgetTraverseUp", _XmRC_GadgetTraverseUp},
    {"MenuGadgetTraverseDown", _XmRC_GadgetTraverseDown},
};

static XmBaseClassExtRec _XmRowColumnCoreClassExtRec =
{
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ NULL, /* FIXME */
    /* initialize_posthook       */ NULL, /* FIXME */
    /* set_values_posthook       */ NULL, /* FIXME */
    /* secondary_object_class    */ NULL, /* FIXME */
    /* secondary_object_create   */ NULL, /* FIXME */
    /* get_secondary_resources   */ NULL, /* FIXME */
    /* fast_subclass             */ {0},  /* FIXME */
    /* get_values_prehook        */ NULL, /* FIXME */
    /* get_values_posthook       */ NULL, /* FIXME */
    /* class_part_init_prehook   */ NULL, /* FIXME */
    /* class_part_init_posthook  */ NULL, /* FIXME */
    /* ext_resources             */ NULL, /* FIXME */
    /* compiled_ext_resources    */ NULL, /* FIXME */
    /* num_ext_resources         */ 0,    /* FIXME */
    /* use_sub_resources         */ FALSE,/* FIXME */
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

static CompositeClassExtensionRec rcCompositeExt =
{
    /* next_extension */ NULL,
    /* record_type    */ NULLQUARK,
    /* version        */ XtCompositeExtensionVersion,
    /* record_size    */ sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ True
#endif
};

static XmManagerClassExtRec _XmRowColumnMClassExtRec =
{
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmRowColumnClassRec xmRowColumnClassRec =
{
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) & xmManagerClassRec,
	/* class_name            */ "XmRowColumn",
	/* widget_size           */ sizeof(XmRowColumnRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
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
	/* visible_interest      */ TRUE,
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
	/* tm_table              */ XtInheritTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer) & _XmRowColumnCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */	RcGeometryManager,
	/* change_managed   */	change_managed,
	/* insert_child     */	insert_child,
	/* delete_child     */	DeleteChild,
	/* extension        */	(XtPointer) & rcCompositeExt,
    },
    /* Constraint class part */
    {
	/* subresources      */ rowColumnConstraintResources,
	/* subresource_count */ XtNumber(rowColumnConstraintResources),
	/* constraint_size   */ sizeof(XmRowColumnConstraintRec),
	/* initialize        */ NULL,
	/* destroy           */ NULL,
	/* set_values        */ RcConstraintSetValues,
	/* extension         */ NULL,
    },
    /* XmManager class part */
    {
	/* translations           */ XmInheritTranslations,
	/* syn_resources          */ syn_resources,
	/* num_syn_resources      */ XtNumber(syn_resources),
	/* syn_constraint_res     */ NULL,
	/* num_syn_constraint_res */ 0,
	/* parent_process         */ XmInheritParentProcess,
	/* extension              */ (XtPointer) & _XmRowColumnMClassExtRec
    },
    /* XmRowColumn Area part */
    {
	/* menuProcedures   */ NULL, /* FIXME */
	/* armAndActivate   */ NULL, /* FIXME */
	/* traversalHandler */ NULL, /* FIXME */
	/* extension        */ NULL, /* FIXME */
    },
};

WidgetClass xmRowColumnWidgetClass = (WidgetClass) & xmRowColumnClassRec;

static void
class_initialize()
{
    _XmRowColumnCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    CompositeClassExtension ext, *extptr;
    XmRowColumnWidgetClass rc_class = (XmRowColumnWidgetClass) widget_class;

    extptr = (CompositeClassExtension *) 
      _XmGetClassExtensionPtr((XmGenericClassExt *) 
			         &(rc_class->composite_class.extension),
			      NULLQUARK);

    if (extptr == NULL || *extptr == NULL) {
	ext = (CompositeClassExtension) XtNew(CompositeClassExtensionRec);
	if (ext != NULL) {
	    ext->next_extension = rc_class->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    rc_class->composite_class.extension = (XtPointer) ext;
	}
    }

    _XmFastSubclassInit(widget_class, XmROW_COLUMN_BIT);
}

static void
initialize_prehook(Widget request,
		   Widget new_w,
		   ArgList args,
		   Cardinal * num_args)
{
    if (RC_Type(new_w) == XmWORK_AREA)
	MGR_NavigationType(new_w) = XmTAB_GROUP;
    else
	MGR_NavigationType(new_w) = XmNONE;

#if 1
/*
 * I think all cases of RC_Type turn out to be traverable...
 * Danny 23/6/96
 */
    MGR_TraversalOn(new_w) = True;
#else
    if (RC_Type(new_w) == XmWORK_AREA || RC_Type(new_w) == XmMENU_OPTION)
	MGR_TraversalOn(new_w) = True;
    else
	MGR_TraversalOn(new_w) = False;
#endif
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal * num_args)
{
    RC_Boxes(new_w) = NULL;	/* no initial children */

    if (RC_Type(new_w) == XmMENU_BAR || RC_Type(new_w) == XmMENU_POPUP
	|| RC_Type(new_w) == XmMENU_PULLDOWN) {
	MGR_ShadowThickness(new_w) = 2;
    }

    /* now, we install our translations, 
     * depending on the row column type */

    if (RC_Type(new_w) != XmWORK_AREA) {
	XtOverrideTranslations(new_w,
			  XtParseTranslationTable(_XmRowColumn_menu_table));
	XtOverrideTranslations(new_w,
		XtParseTranslationTable(_XmRowColumn_menu_traversal_table));
    }

    if (RC_Type(new_w) == XmMENU_OPTION)
	XtOverrideTranslations(new_w,
			XtParseTranslationTable(_XmRowColumn_option_table));
    else if (RC_Type(new_w) == XmMENU_BAR)
	XtOverrideTranslations(new_w,
			   XtParseTranslationTable(_XmRowColumn_bar_table));

    /* Internal widgets */

    RC_CascadeBtn(new_w) = NULL;
    RC_PopupPosted(new_w) = NULL;
    RC_LastSelectToplevel(new_w) = NULL;

    /* Tear off stuff */
    RC_SetTornOff(new_w, 0);
    RC_SetFromInit(new_w, 0);
    RC_SetTearOffDirty(new_w, 0);
    RC_SetTearOffActive(new_w, 0);
    RC_ParentShell(new_w) = NULL;
    RC_TearOffControl(new_w) = NULL;

    if (RC_Type(new_w) == XmMENU_OPTION) {
	Widget	cb;

	XtVaCreateManagedWidget(RC_OPTION_LABEL,
				xmLabelGadgetClass,
				new_w,
				XmNlabelString, RC_OptionLabel(new_w),
				NULL);

	cb = XtVaCreateManagedWidget(RC_OPTION_CBG,
				xmCascadeButtonGadgetClass,
				new_w,
				XmNsubMenuId, RC_OptionSubMenu(new_w),
				NULL);

	/* If we have XmNmenuHistory, copy it */
	if (RC_MemWidget(new_w)) {
	    XmString	xms = NULL;
	    Arg		a;
	    XtSetArg(a, XmNlabelString, &xms);
	    XtGetValues(RC_MemWidget(new_w), &a, 1);
	    XtSetArg(a, XmNlabelString, xms);
	    XtSetValues(cb, &a, 1);
	    XmStringFree(xms);
	}
    }

    /* just to initialize it. */
    RC_SetFromResize(new_w, 0);

    /* menuPost stuff. */
    if (RC_MenuPost(new_w) == NULL) 
      if (RC_Type(new_w) == XmMENU_OPTION
	  || RC_Type(new_w) == XmWORK_AREA
	  || RC_Type(new_w) == XmMENU_BAR)
	RC_MenuPost(new_w) =  "<Button1Down>";

    if (RC_Packing(new_w) == (unsigned char) XmUNSPECIFIED) {
/*
 * This is what the XmRowColumn manual page says
 *	(end of the text describing XmNpacking).
 */
	if (RC_RadioBehavior(new_w) && RC_Type(new_w) == XmWORK_AREA)
	    RC_Packing(new_w) = XmPACK_COLUMN;
	else if (RC_Type(new_w) == XmMENU_OPTION)
	    RC_Packing(new_w) = XmPACK_TIGHT;
	else
	    RC_Packing(new_w) = XmPACK_TIGHT;
    }

    /* Be verbose about it ... */
    if (XdbInDebug(__FILE__, new_w)) 
    {
	XdbDebug(__FILE__, 
		 new_w, 
		 "Initialize: RadioBehavior %s, RC_Type %s => Packing %s\n",
		 XdbBoolean2String(RC_RadioBehavior(new_w)), 
		 XdbRcType2String(RC_Type(new_w)),
		 XdbPacking2String(RC_Packing(new_w)));
    }

/* Initialize sizes if not already done */
    if (RC_Type(new_w) == XmMENU_BAR) {
	if (XtWidth(new_w) == 0)
		XtWidth(new_w) = 1;	/* FIX ME */
	if (XtHeight(new_w) == 0)
		XtHeight(new_w) = 20;	/* FIX ME */
    }
}

static void
destroy(Widget w)
{
    if (RC_Boxes(w))
	XtFree((XtPointer)RC_Boxes(w));
}

static void
_XmRcCreateTearOffControl(Widget rc)
{
    RC_TearOffControl(rc) = XtVaCreateManagedWidget("TearOffControl",
	xmTearOffButtonWidgetClass, rc,
		XmNpositionIndex, 0, 
	NULL);
}

static void
_XmRcDestroyTearOffControl(Widget rc)
{
    if (RC_TearOffControl(rc))
	XtDestroyWidget(RC_TearOffControl(rc));
    RC_TearOffControl(rc) = NULL;
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal * num_args)
{
    Boolean	need_refresh = False,
		need_relayout = False;

    XdbDebug(__FILE__, new_w, "RowColumn set_values()\n");

    if (RC_EntryAlignment(old) != RC_EntryAlignment(new_w)) {
	int	i;
	Arg	a;

	XtSetArg(a, XmNalignment, RC_EntryAlignment(new_w));
	XdbDebug(__FILE__, new_w, "Setting Alignment for children to %s\n",
		XdbAlignment2String(RC_EntryAlignment(new_w)));

	for (i=0; i<MGR_NumChildren(new_w); i++)
	    if (XmIsLabel(MGR_Children(new_w)[i])
		|| XmIsLabelGadget(MGR_Children(new_w)[i])) {
		    XdbDebug2(__FILE__, new_w, MGR_Children(new_w)[i],
			"Set Alignment to %s\n",
			XdbAlignment2String(RC_EntryAlignment(new_w)));
		    XtSetValues(MGR_Children(new_w)[i], &a, 1);
		}
    }

    if ((RC_Orientation(old) != RC_Orientation(new_w))
		|| (RC_Packing(old) != RC_Packing(new_w))) {
	need_relayout = True;
	need_refresh = True;
    }

    if ((RC_TearOffModel(new_w) != RC_TearOffModel(old))
	&& (RC_Type(new_w) == XmMENU_POPUP || RC_Type(new_w) == XmMENU_PULLDOWN))
    {
	if (RC_TearOffModel(new_w) == XmTEAR_OFF_ENABLED)
	    _XmRcCreateTearOffControl(new_w);
	else
	    _XmRcDestroyTearOffControl(new_w);

	need_relayout = True;
    }

    if (RC_Type(new_w) == XmMENU_OPTION 
	&& RC_OptionSubMenu(new_w) != RC_OptionSubMenu(old)) 
    {
	int	i;

	/* Oh yes this is very efficient :-( */

	for (i=0; i<MGR_NumChildren(new_w); i++)
	    if (XtIsSubclass(MGR_Children(new_w)[i], 
			     xmCascadeButtonGadgetClass)) 
            {
		XtVaSetValues(MGR_Children(new_w)[i], 
			      XmNsubMenuId, 
			      RC_OptionSubMenu(new_w), 
			      NULL);
		XdbDebug2(__FILE__, 
			  new_w, 
			  MGR_Children(new_w)[i], 
			  "Assign SubMenuId (%s)\n",
			  XtName(RC_OptionSubMenu(new_w)));
		break;
	    }
    }

    if (XtSensitive(new_w) != XtSensitive(old)) {
	XdbDebug(__FILE__, new_w, "SetValues: sensitive changes to %d\n", XtSensitive(new_w));
#if 0
/* Doesn't seem to be necessary */
	if (XtSensitive(new_w)) {
	    int i;
	    for (i=0; i<MGR_NumChildren(new_w); i++)
		XtSetSensitive(MGR_Children(new_w)[i], True);
	}
#endif
	need_refresh = True;
    }

    if (need_relayout) {
#if 0
	if (XtIsRealized(new_w))
#endif
	    XmRowColumnLayout(new_w, Mode_Normal, NULL, NULL, NULL);
    }

    return need_refresh;
}

static void
expose(Widget w,
       XEvent * event,
       Region region)
{
    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   MGR_TopShadowGC(w),
		   MGR_BottomShadowGC(w),
		   0, 0,
		   XtWidth(w),
		   XtHeight(w),
		   MGR_ShadowThickness(w),
		   XmSHADOW_OUT);

    /* display the gadgets, if there are any */
    _XmRedisplayGadgets(w, event, region);
}

static XtGeometryResult
query_geometry(Widget w,
	       XtWidgetGeometry * request,
	       XtWidgetGeometry * reply)
{
    XtGeometryResult result = XtGeometryYes;
    Dimension width, height;
    XtWidgetGeometry	rcg;

    XdbDebug(__FILE__, w, "RC QueryGeometry, request %s\n", XdbWidgetGeometry2String(request));

    XmRowColumnLayout(w, Mode_Test, NULL, NULL, &rcg);
    width = (rcg.request_mode & CWWidth) ? rcg.width : XtWidth(w);
    height = (rcg.request_mode & CWHeight) ? rcg.height : XtHeight(w);

    XdbDebug(__FILE__, w, "preferred size %s\n", XdbWidgetGeometry2String(&rcg));

    reply->width = width;
    reply->height = height;

    /* Not interested in more flags */
    request->request_mode &= CWWidth | CWHeight;	

    if (request->request_mode == 0) {
	reply->request_mode = CWWidth | CWHeight;
	XdbDebug(__FILE__, w, 
		 "RC QueryGeometry(NULL) => XtGeometryYes, %d %d\n", 
		 width, height);
	return result;
    }

    if (request->request_mode & CWWidth)
	if (request->width < width) 
        {
	    result = XtGeometryAlmost;
	    reply->width = width;
	    reply->request_mode |= CWWidth;
	}
    if (request->request_mode & CWHeight)
	if (request->height < height) 
        {
	    result = XtGeometryAlmost;
	    reply->height = height;
	    reply->request_mode |= CWHeight;
	}

    reply->width = width;
    reply->height = height;
    reply->request_mode = CWWidth | CWHeight;

    XdbDebug(__FILE__, w, 
	     "RC QueryGeometry() => %s, %d %d\n",
	     XdbGeometryResult2String(result), width, height);

    return result;
}

/*
 * This function was a major inefficiency provider of the RC widget -
 * it gets called by setvalues, insert_child, and change_managed;
 * and every time it does a XtQueryGeometry on all its children,
 * forgetting all the info from the previous calls ...
 *
 * In the process of fixing this... Danny 23/8/1996
 *
 * Moved all insert_child and delete_child interaction with this sucker either
 *	to delete_child or to a separate function. 25/8/96.
 *
 * Much better now :-)
 */
static void
initialize_boxes(Widget w, Widget instig, XtWidgetGeometry *instig_request)
{
    int i;
    XmRCKidGeometry kid_geometry;

    XdbDebug(__FILE__, w, "initialize_boxes\n");

    for (i = 0; i < MGR_NumChildren(w); i++) {
	Dimension *baselines;
	int number_of_baselines;

	kid_geometry = &(RC_Boxes(w)[i]);
	kid_geometry->kid = MGR_Children(w)[i];

	if (kid_geometry->kid == NULL)
	  continue;

	if (instig != NULL && kid_geometry->kid == instig)
	    if (instig_request->request_mode == (CWWidth|CWHeight|CWX|CWY))
		RC_Boxes(w)[i].box = *instig_request;
	    else {
		XtQueryGeometry(kid_geometry->kid, NULL, &(RC_Boxes(w)[i].box));
		if (instig_request->request_mode & CWX)
		    RC_Boxes(w)[i].box.x = instig_request->x;
		if (instig_request->request_mode & CWY)
		    RC_Boxes(w)[i].box.y = instig_request->y;
		if (instig_request->request_mode & CWWidth)
		    RC_Boxes(w)[i].box.width = instig_request->width;
		if (instig_request->request_mode & CWHeight)
		    RC_Boxes(w)[i].box.height = instig_request->height;
	    }
	else
	    XtQueryGeometry(kid_geometry->kid, NULL, &(RC_Boxes(w)[i].box));

	kid_geometry->margin_top = kid_geometry->margin_bottom = 0;

	XtVaGetValues(kid_geometry->kid,
		      XmNmarginTop, &kid_geometry->margin_top,
		      XmNmarginBottom, &kid_geometry->margin_bottom,
		      NULL);

	if (XmWidgetGetBaselines(kid_geometry->kid, 
				 &baselines, 
				 &number_of_baselines)) 
        {
	    /* we set the baseline of the child to the lowest baseline of
	     * the widget */
	    kid_geometry->baseline = baselines[number_of_baselines - 1];

	    XtFree((char *)baselines);
	}
	else
	    kid_geometry->baseline = 0;		/* is this right ? */
    }
}

/*
 * Initialize one box
 */
static void
InitializeBox(Widget w, Widget child, XtWidgetGeometry *wg)
{
    XmRCKidGeometry kid_geometry;
    Dimension *baselines;
    int number_of_baselines, i;

    for (i=0; i<MGR_NumChildren(w); i++)
	if (child == MGR_Children(w)[i])
	    break;

    kid_geometry = &(RC_Boxes(w)[i]);
    kid_geometry->kid = MGR_Children(w)[i];

    if (kid_geometry->kid == NULL)
	return;

    assert(wg != NULL);

    XdbDebug2(__FILE__, w, child, "InitializeBox %s\n",
      XdbWidgetGeometry2String(wg));

    RC_Boxes(w)[i].box = *wg;

    kid_geometry->margin_top = kid_geometry->margin_bottom = 0;

    XtVaGetValues(kid_geometry->kid,
	XmNmarginTop, &kid_geometry->margin_top,
	XmNmarginBottom, &kid_geometry->margin_bottom,
	NULL);

    if (XmWidgetGetBaselines(kid_geometry->kid, &baselines, &number_of_baselines)) {
    /* we set the baseline of the child to the lowest baseline of the widget */
	kid_geometry->baseline = baselines[number_of_baselines - 1];
	XtFree((char *)baselines);
    } else
	kid_geometry->baseline = 0;		/* is this right ? */
}

/*
 * Special version of the above to be called from insert_child
 */
static void
InitializeBoxesInsert(Widget w, Widget child)
{
    XmRCKidGeometry	kid_geometry;
    int			i, j;
    XtWidgetGeometry	geo;

    for (i=0; i<MGR_NumChildren(w); i++)
	if (child == MGR_Children(w)[i])
	    break;

    RC_Boxes(w) = (XmRCKidGeometry) XtRealloc((XtPointer)RC_Boxes(w),
			MGR_NumChildren(w) * sizeof(XmRCKidGeometryRec));
/*
 * Move existing entries out of the way if this new child was not added at the
 * end. (TearOffControls are always added at 0.)
 */
    for (j=i+1; j<MGR_NumChildren(w); j++)
	RC_Boxes(w)[j] = RC_Boxes(w)[j-1];

    kid_geometry = &(RC_Boxes(w)[i]);
    kid_geometry->kid = MGR_Children(w)[i];

    if (kid_geometry->kid == NULL)
	return;

    XtQueryGeometry(kid_geometry->kid, NULL, &geo);

    InitializeBox(w, child, &geo);
}

/*
 * If CW is non-null, make sure not to resize it.
 *
 * The last parameter returns the RC size, to be used in GeometryManager.
 *	Why ? Because GeometryManager needs to call XmRowColumnLayout in test mode,
 *	and figure out from that whether the geometry change is acceptable. If it is,
 *	then it might need to resize the RC itself as a consequence.
 *	And *that* geometry change is a whole different issue from the one we can do
 *	inside XmRowColumnLayout.
 *
 * The meaning of "mode" :
 *	- mode & Mode_Test == 1		means this is a query; nothing should be changed
 *	- mode & Mode_Resize == 1	means (we're being called from the resize method)
 *					  we cannot resize ourselves, but must fit in the
 *					  size given in XtWidth/XtHeight
 *	- mode == Mode_Normal		means both Test and Resize are off
 */
static void
XmRowColumnLayout(Widget rc, int mode, Widget cw, XtWidgetGeometry *cg, XtWidgetGeometry *rcgp)
{
    XtWidgetGeometry	rcg;	/* in case rcgp is NULL */
    int i;

    /* No need to do anything else if we don't have any children */
    if (MGR_NumChildren(rc) == 0) {
	if (rcgp)
	    rcgp->request_mode = 0;
	return;
    }

    XdbDebug(__FILE__, rc, "XmRowColumnLayout (%s) %s %s\n",
	XdbRcType2String(RC_Type(rc)),
	(mode & Mode_Resize) ? "" : "ParentResize",
	(mode & Mode_Test) ? "TestMode" : "");

    rcg.request_mode = 0;
    rcg.width = rcg.height = 0;

    if (RC_Boxes(rc) == 0) {
	abort();	/* This never happens because of InitializeBoxesInsert() */
	initialize_boxes(rc, cw, cg);
    }

/* As you can see the last parameter of the DoLayout*() functions is never NULL */
    switch (RC_Type(rc)) {
    case XmWORK_AREA:
	switch (RC_Orientation(rc)) {
	case XmHORIZONTAL:
	    if (RC_Packing(rc) == XmPACK_TIGHT)
		DoLayoutHT(rc, mode, cw, cg, &rcg);
	    else if (RC_Packing(rc) == XmPACK_COLUMN)
		DoLayoutHC(rc, mode, cw, cg, &rcg);
	    else
		DoLayoutHC(rc, mode, cw, cg, &rcg);	/* Right ? FIX ME */
	    break;
	case XmVERTICAL:
	    if (RC_Packing(rc) == XmPACK_TIGHT)
		DoLayoutVT(rc, mode, cw, cg, &rcg);
	    else if (RC_Packing(rc) == XmPACK_COLUMN)
		DoLayoutVC(rc, mode, cw, cg, &rcg);
	    else
		DoLayoutVC(rc, mode, cw, cg, &rcg);	/* Right ? FIX ME */
	    break;
	}
	break;
    case XmMENU_BAR:
	DoLayoutHT(rc, mode, cw, cg, &rcg);
	break;
    case XmMENU_PULLDOWN:
    case XmMENU_POPUP:
	DoLayoutVT(rc, mode, cw, cg, &rcg);	/* Menus should be tight */
	break;
    case XmMENU_OPTION:
	DoLayoutHT(rc, mode, cw, cg, &rcg);
	break;
    }

/*
 * Resize ourselves, if we're allowed to...
 *	Only if ~Mode_Test and ~Mode_Resize...
 */
    if (mode == Mode_Normal) {
	XtGeometryResult	result;

	if (((rcg.request_mode & CWWidth) && (rcg.width != XtWidth(rc)))
	 || ((rcg.request_mode & CWHeight) && (rcg.height != XtHeight(rc)))) {

	    XdbDebug(__FILE__, rc, "Need to resize to %s\n", XdbWidgetGeometry2String(&rcg));

	    rcg.request_mode &= (CWWidth|CWHeight);		/* One can never be sure enough */
	    result = _XmMakeGeometryRequest(rc, &rcg);

	    if (result == XtGeometryYes) {
		XtWidth(rc) = rcg.width;
		XtHeight(rc) = rcg.height;
	    }
	}
    }

/* Provide a result */
    if (rcgp)
	*rcgp = rcg;

/* Stop if we're in test mode */
    if (mode & Mode_Test)
    	return;

/* Now we actually lay out our children.  */
    for (i = 0; i < MGR_NumChildren(rc); i++) {
	XmRCKidGeometry kid_geometry = &(RC_Boxes(rc)[i]);

	if (MGR_Children(rc)[i] == cw)		/* Don't touch the instigator */
		continue;
	if (! XtIsManaged(MGR_Children(rc)[i]))
		continue;

	_XmConfigureObject(MGR_Children(rc)[i],
			   kid_geometry->box.x,
			   kid_geometry->box.y,
			   kid_geometry->box.width,
			   kid_geometry->box.height,
			   XtBorderWidth(MGR_Children(rc)[i]));
    }
}

/*
 * This function treats the three following cases :
 *	XmWORK_AREA & XmHORIZONTAL & XmPACK_TIGHT
 *	XmMENU_BAR
 *	XmMENU_OPTION
 * ... which kind of explains what MENU_BAR and MENU_OPTION mean.
 *
 * Finally, it also covers the case where DoLayoutHC is called but no number
 *	of columns is specified.
 *
 * According to the manual page, XmNnumColumns doesn't matter in XmPACK_COLUMN.
 * I guess this means this function shouldn't think about XmNnumColumns.
 */
static void
DoLayoutHT(Widget rc, int mode, Widget cw, XtWidgetGeometry *cg, XtWidgetGeometry *rcg)
{
    int			i, j;
    int			first_in_row = 0, row;
    Dimension		current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
    Dimension		current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
    Dimension		max_height = 0, max_width = 0;
    XtWidgetGeometry	geo;
    XmRCKidGeometry	help_kid_geometry = NULL;

    if (XdbInDebug(__FILE__, rc)) {
	XdbDebug(__FILE__, rc, "DoLayoutHT(");
	XdbDebug0(__FILE__, rc, "%s", (mode & Mode_Test) ? "Test " : "");
	XdbDebug0(__FILE__, rc, "%s", (mode & Mode_Resize) ? "" : "ParentResize ");
	if (cw) XdbDebug0(__FILE__, rc, "Child %s ", XtName(cw));
	XdbDebug0(__FILE__, rc, ")\n");
    }

/* Calculate the max_height */
    for (i = 0; i < MGR_NumChildren(rc); i++) {
	XmRCKidGeometry kid_geometry = &(RC_Boxes(rc)[i]);
	Widget kid = kid_geometry->kid;

	if (!kid || !XtIsManaged(kid))
	    continue;

	XdbDebug2(__FILE__, rc, kid, "DoLayoutHT: kid geo %d %d %dx%d\n",
		kid_geometry->box.x,
		kid_geometry->box.y,
		kid_geometry->box.width,
		kid_geometry->box.height);
	/*
	 * If we don't have enough space in this row, start another one.
	 * We may not want to do this in menu bars.
	 */
	if (XtIsRealized(rc) && RC_Type(rc) == XmWORK_AREA
		&& current_x + kid_geometry->box.width > XtWidth(rc)) {
	    current_y += max_height + RC_Spacing(rc);
	    current_x += RC_MarginW(rc) + MGR_ShadowThickness(rc);
	}

	/* Don't add spacing after last child */
	current_x += kid_geometry->box.width;
	if (i != MGR_NumChildren(rc) - 1)
	    current_x += RC_Spacing(rc);

	if (current_x > max_width)
	    max_width = current_x;

	if (kid == cw && (cg->request_mode & CWHeight)) {
	  if (cg->height + current_y > max_height)
	    max_height = cg->height + current_y;
	} else {
	  if (kid_geometry->box.height + current_y > max_height)
	    max_height = kid_geometry->box.height + current_y;
	}
    }

    geo.request_mode = CWWidth | CWHeight;
    geo.width = max_width + RC_MarginW(rc) + MGR_ShadowThickness(rc);
    geo.height = max_height + RC_MarginH(rc) + MGR_ShadowThickness(rc);

/* FIX ME - is this good enough ??
   May want to test for 0,0 condition in one of the other cases */
    rcg->width = geo.width;
    rcg->height = geo.height;
    rcg->request_mode = CWWidth | CWHeight;

    if (mode & Mode_Test) {
	XdbDebug(__FILE__, rc, "DoLayoutHT => %s\n",
		XdbWidgetGeometry2String(&geo));
	return;
    }

    if ((mode & Mode_Resize) == 0) {
	XdbDebug(__FILE__, rc, "DoLayoutHT: requesting %s\n",
		XdbWidgetGeometry2String(&geo));

	if (_XmMakeGeometryRequest(rc, &geo) == XtGeometryYes) {
	    XtWidth(rc) = geo.width;
	    XtHeight(rc) = geo.height;
	}
    } else {
	XdbDebug(__FILE__, rc, "DoLayoutHT: RC size is %d x %d\n",
		XtWidth(rc), XtHeight(rc));
    }

/*
 * Adapt max_height to actual size of RC.
 * This is important both in case we didn't get what we asked, and
 *	in case we were not allowed to change our geometry.
 */
    max_height = XtHeight(rc) - RC_MarginH(rc) - MGR_ShadowThickness(rc);

/* Start layout */
    row = 0;
    current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
    current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);

    for (i = 0; i < MGR_NumChildren(rc); i++) {
	XmRCKidGeometry kid_geometry = &(RC_Boxes(rc)[i]);
	Widget kid = kid_geometry->kid;

	if (!kid || !XtIsManaged(kid))
	    continue;

	/* don't lay the help button out until the end. */
	if (kid == RC_HelpPb(rc)) {
	    help_kid_geometry = kid_geometry;
	    if (kid == cw) {
		if (cg->request_mode & CWWidth)
		    help_kid_geometry->box.width = cg->width;
		if (cg->request_mode & CWHeight)
		    help_kid_geometry->box.height = cg->height;
	    }
	    continue;
	}

	/* Height treatment */
	kid_geometry->box.height = max_height;

	if (((mode & Mode_Resize) == 0) &&
		current_x + kid_geometry->box.width > XtWidth(rc)) 
        {
	    row++;
	    current_y = row * max_height + RC_Spacing(rc);
	    current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
	    first_in_row = i;
	}

	kid_geometry->box.x = current_x;
	kid_geometry->box.y = current_y;

	current_x += RC_Spacing(rc) + kid_geometry->box.width;
    }

    /* now we fill in the last row, so it takes up the remaining height, 
       if XmNadjustLast is True. */
    if (RC_AdjLast(rc)) {
	for (j = first_in_row; j < MGR_NumChildren(rc); j++) 
        {
	    XmRCKidGeometry kid_geometry = &(RC_Boxes(rc)[j]);
	    Widget kid = kid_geometry->kid;

	    if (!kid || !XtIsManaged(kid))
		continue;

	    if ((XtHeight(rc) <= current_y + RC_MarginH(rc))
		|| (max_height + RC_MarginH(rc) > XtHeight(rc))) {
		    kid_geometry->box.height = max_height;
	    } else
		kid_geometry->box.height = (XtHeight(rc) 
		    - (current_y + RC_MarginH(rc) + MGR_ShadowThickness(rc)));
	}
    }

    if (RC_HelpPb(rc) && XtIsManaged(RC_HelpPb(rc))) {
/* In some cases we don't have a help widget here ...
   like when it's not managed */
	help_kid_geometry->box.x = (XtWidth(rc)
				    - MGR_ShadowThickness(rc)
				    - help_kid_geometry->box.width);
	help_kid_geometry->box.y = current_y;
    }

    RC_SetFromResize(rc, 0);
}

/* 
 * Horizontal Column
 *	XmNnumColumns does determine geometry here (indicating number of rows !!).
 *	All children (of certain class) get to have the same width.
 */
static void
DoLayoutHC(Widget rc, int mode, Widget cw, XtWidgetGeometry *cg, XtWidgetGeometry *rcg)
{
    int			i;
    int			number_per_row;
    Dimension		current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
    Dimension		current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
    Dimension		width_of_widgets = 0;
    Dimension		*height_of_row;
    int			*widgets_per_row;
    int			wpr;
    XmRCKidGeometry	kid_geometry;
    Widget		kid;
    Dimension		rcw, rch;
    XtWidgetGeometry	geo;

    XdbDebug(__FILE__, rc, "DoLayoutHC()\n");

    if (RC_NCol(rc) == 0) {
	/* what the hell.  punt and go to .. */
	DoLayoutHT(rc, mode, NULL, NULL, NULL);
	return;
    }

#define ROW_OF_THIS_WIDGET(i) (i / number_per_row)
    number_per_row = (MGR_NumChildren(rc) - 1) / RC_NCol(rc) + 1;

    height_of_row = (Dimension *) XtCalloc(RC_NCol(rc), sizeof(Dimension));
    widgets_per_row = (int *) XtCalloc(RC_NCol(rc), sizeof(int));

    for (i = 0; i < RC_NCol(rc); i++)
	widgets_per_row[i] = height_of_row[i] = 0;

    /* first, figure out the width of each column and the height of each row */
    for (i = 0; i < MGR_NumChildren(rc); i++) {
	kid_geometry = &(RC_Boxes(rc)[i]);
	kid = kid_geometry->kid;

	if (!kid || !XtIsManaged(kid))
	    continue;

	if (! (XmIsSeparator(kid) || XmIsSeparatorGadget(kid)))
	    widgets_per_row[ROW_OF_THIS_WIDGET(i)] ++;

	if (width_of_widgets < kid_geometry->box.width)
	    width_of_widgets = kid_geometry->box.width;

	if (height_of_row[ROW_OF_THIS_WIDGET(i)] < kid_geometry->box.height)
	    height_of_row[ROW_OF_THIS_WIDGET(i)] = kid_geometry->box.height;
    }

/* Determine the RC's size */
    rcw = 2 * (RC_MarginW(rc) + MGR_ShadowThickness(rc))
	+ width_of_widgets * number_per_row
	+ RC_Spacing(rc) * (number_per_row - 1);
    rch = 2 * (RC_MarginH(rc) + MGR_ShadowThickness(rc))
	+ RC_Spacing(rc) * (RC_NCol(rc) - 1);
    for (i=0; i<RC_NCol(rc); i++)
	rch += height_of_row[i];

    rcg->width = rcw;
    rcg->height = rch;
    rcg->request_mode = CWWidth | CWHeight;

    if (mode & Mode_Test)
        return;

/* Now try to resize to that size */
    geo.request_mode = CWWidth | CWHeight;
    geo.width = rcw;
    geo.height = rch;

/* FIX ME */
    if (mode != Mode_Test) {
	if (_XmMakeGeometryRequest(rc, &geo) == XtGeometryYes) {
	    rcw = XtWidth(rc) = geo.width;
	    rch = XtHeight(rc) = geo.height;
	}
    }

    rcg->request_mode = CWWidth|CWHeight;
    rcg->width = rcw;
    rcg->height = rch;

    XdbDebug(__FILE__, rc, "DoLayoutHC: Size returned %s\n", XdbWidgetGeometry2String(rcg));

/*
 * We might have a problem now if the geometry that we have is smaller than
 * the desired one. Especially the height should be taken care of.
 * If we got a height smaller than the height that we need for one of the columns,
 * then height_of_widgets should be decreased.
 * Decrease by getting the amount of widgets in that column ...
 */
    wpr = 0;
    for (i=0; i<RC_NCol(rc); i++)
	if (widgets_per_row[i] > wpr)
	    wpr = widgets_per_row[i];

    if (2 * (RC_MarginW(rc) + MGR_ShadowThickness(rc)) + wpr * width_of_widgets
		+ RC_Spacing(rc) * (wpr - 1) > geo.width) {

	XdbDebug(__FILE__, rc, "DoLayoutHC: Width needed %d, got %d; width of widgets %d -> %d\n",
		rcw, geo.width, width_of_widgets, width_of_widgets - (rcw - geo.width) / wpr);

	if (width_of_widgets - (rcw - geo.width) / wpr <= 0) {
		XdbDebug(__FILE__, rc, "DoLayoutHC: width_of_widgets untouched\n");
	} else
		width_of_widgets -= (rcw - geo.width) / wpr;
    }

/* now we lay out the children */
    for (i = 0; i < MGR_NumChildren(rc); i++) {
	if (i != 0 && (i % number_per_row) == 0) 
        {
	    current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
	    current_y += height_of_row[ROW_OF_THIS_WIDGET(i)] + RC_Spacing(rc);
	}

	kid_geometry = &(RC_Boxes(rc)[i]);
	kid = kid_geometry->kid;

	if (!kid || !XtIsManaged(kid))
	    continue;

	kid_geometry->box.x = current_x;
	kid_geometry->box.y = current_y;

	kid_geometry->box.width = width_of_widgets;
	kid_geometry->box.height = height_of_row[ROW_OF_THIS_WIDGET(i)];

	current_x += RC_Spacing(rc) + width_of_widgets;
    }

    /* now we fill in the last row, so it takes up the remaining height, 
       if XmNadjustLast is True. */
    if (RC_AdjLast(rc))
	for (i = MGR_NumChildren(rc) - number_per_row; 
	     i < MGR_NumChildren(rc); 
	     i++) 
        {
	    kid_geometry = &(RC_Boxes(rc)[i]);
	    kid = kid_geometry->kid;

	    if (!kid || !XtIsManaged(kid))
		continue;

	    kid_geometry->box.height = (XtHeight(rc) 
					- (current_y 
					   + RC_MarginH(rc) 
					   + MGR_ShadowThickness(rc)));
	}

	XtFree((XtPointer)height_of_row);
	XtFree((XtPointer)widgets_per_row);
#undef ROW_OF_THIS_WIDGET
}

/*
 * Called for cases :
 *	XmVERTICAL & XmPACK_TIGHT
 *	XmMENU_PULLDOWN
 *
 * According to the manual pages, XmNnumColumns should not make a difference
 * in this case.
 *
 * An extra complication is added by TearOffControl. This button is always
 * child 0, and when it's there, RC_TearOffControl is non-NULL.
 */
static void
DoLayoutVT(Widget rc, int mode, Widget cw,
	XtWidgetGeometry *cg, XtWidgetGeometry *rcg)
{
    int i, j, col;
    int first_in_column = 0;
    Dimension current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
    Dimension current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
    Dimension max_width = 0;
    XtGeometryResult	result;
    XtWidgetGeometry	geo;
    XmRCKidGeometry	last = NULL;

/* Calculate the max_width */
    for (i = 0; i < MGR_NumChildren(rc); i++) {
	XmRCKidGeometry kid_geometry = &(RC_Boxes(rc)[i]);
	Widget kid = kid_geometry->kid;

	if (!kid || !XtIsManaged(kid))
	    continue;

	if (kid == cw && (cg->request_mode & CWWidth)) {
	    XdbDebug2(__FILE__, rc, kid_geometry->kid, "DoLayoutVT: kid %s\n",
		XdbWidgetGeometry2String(cg));

	    if (cg->width > max_width)
		max_width = cg->width;
	} else {
	    XdbDebug2(__FILE__, rc, kid_geometry->kid, "DoLayoutVT: kid %s\n",
		XdbWidgetGeometry2String(&kid_geometry->box));

	    if (kid_geometry->box.width > max_width)
		max_width = kid_geometry->box.width;
	}

    }
    XdbDebug(__FILE__, rc, "DoLayoutVT: MaxWidth %d\n", max_width);

/* After figuring out how much space we need, try to get it */
    if ((mode & Mode_Resize) == 0) {	/* We can try to resize ourselves */
	Dimension	ww, hh;
	hh = 0;
	for (i = 0; i < MGR_NumChildren(rc); i++) {
	    XmRCKidGeometry kid_geometry = &(RC_Boxes(rc)[i]);
	    Widget kid = kid_geometry->kid;

	    if (!kid || !XtIsManaged(kid))
		continue;

#ifdef	DO_SANITY
/* Something breaks xephem on this */
	    if (kid == cw && (cg->request_mode & CWHeight))
		if (cg->height > 10000) {
		    _XmWarning(rc, "Child %s had specified geometry %s\n",
			XtName(cw), XdbWidgetGeometry2String(cg));
		    cg->height = kid_geometry->box.height;
		}
#endif
	    if (kid == cw && (cg->request_mode & CWHeight))
		hh += RC_Spacing(rc) + cg->height;
	    else
		hh += RC_Spacing(rc) + kid_geometry->box.height;
	}
	if (hh)
	    hh -= RC_Spacing(rc);

	hh += 2 * RC_MarginH(rc) + 2 * MGR_ShadowThickness(rc);
/* Calculate suggested width from max_width */
	ww = max_width + 2 * RC_MarginW(rc) + 2 * MGR_ShadowThickness(rc);

	XdbDebug(__FILE__, rc, "Requesting geometry %d %d\n", ww, hh);

	if ((mode & Mode_Test) == 0) {
	    geo.width = ww;
	    geo.height = hh;
	    geo.request_mode = CWWidth | CWHeight;
	    result = _XmMakeGeometryRequest(rc, &geo);
	    ww = geo.width;
	    hh = geo.height;

	    XdbDebug(__FILE__, rc, "==> got geometry %d %d (%s)\n",
		ww, hh, XdbGeometryResult2String(result));

	    if (result == XtGeometryYes) {
		XtWidth(rc) = ww;
		XtHeight(rc) = hh;
	    }
	}

/* Give a reply */
	rcg->width = ww;
	rcg->height = hh;
	rcg->request_mode = CWWidth | CWHeight;

/* Terminate if we're in test mode */
	if (mode & Mode_Test)
	    return;

/* Change max_width back with the changes in geometry */
	assert(ww >= 2 * (RC_MarginW(rc) + MGR_ShadowThickness(rc)));

	max_width = ww - 2 * (RC_MarginW(rc) + MGR_ShadowThickness(rc));
	XdbDebug(__FILE__, rc, "DoLayoutVT: MaxWidth %d\n", max_width);
    } else {
	/* Just report our size */
	XdbDebug(__FILE__, rc, "DoLayoutVT: size %dx%d\n",
		XtWidth(rc), XtHeight(rc));
    }

/* Start layout */
    col = 0;
    for (i = 0; i < MGR_NumChildren(rc); i++) {
	XmRCKidGeometry kid_geometry = &(RC_Boxes(rc)[i]);
	Widget kid = kid_geometry->kid;

	if (!kid || !XtIsManaged(kid))
	    continue;

	kid_geometry->box.width = max_width;

	/*
	 * See if we need to move to the next column.
	 *
	 * This is what the first condition does; the second insures that
	 * we don't do that if this is the only child in the column.
	 *
	 * The third condition makes sure we don't do this for the last
	 * column.
	 */
	if (current_y + kid_geometry->box.height > XtHeight(rc)
	 && current_y > RC_MarginH(rc) + MGR_ShadowThickness(rc)
	 && current_x + max_width + RC_Spacing(rc) < XtWidth(rc)) {
	    /* Move the child to the next column. */

	    col++;
	    current_x = col * max_width + RC_Spacing(rc);
	    current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
	    first_in_column = i;

#ifdef	VT_DO_LAST
	    if (last && XtIsRealized(rc)) {
		Dimension hh = XtHeight(rc) - last->box.y - RC_MarginH(rc)
			- MGR_ShadowThickness(rc); 

		if (last->box.height < hh) {
		    XdbDebug2(__FILE__, rc, last->kid,
			"DoLayoutVT: Last child's height set to %d, was %d\n",
			hh, last->box.height);
		    last->box.height = hh;
		}
	    }
#else
	    XdbDebug2(__FILE__, rc, last->kid,
		"DoLayoutVT: Last child's height SHOULD BE set to %d (it's %d)\n",
		XtHeight(rc) - last->box.y - RC_MarginH(rc)
			- MGR_ShadowThickness(rc), last->box.height);
#endif
	}

	kid_geometry->box.x = current_x;
	kid_geometry->box.y = current_y;

	current_y += RC_Spacing(rc) + kid_geometry->box.height;
#if 0	/* This cannot happen */
	if (kid_geometry->box.width > max_width)
	    max_width = kid_geometry->box.width;
#endif

	/* Remember the child before this one */
	last = kid_geometry;
    }

#ifdef	VT_DO_LAST
    if (last && XtIsRealized(rc)) {
	Dimension hh = XtHeight(rc) - last->box.y - RC_MarginH(rc)
			- MGR_ShadowThickness(rc); 

	if (last->box.height < hh) {
	    XdbDebug2(__FILE__, rc, last->kid,
		"DoLayoutVT: Last child's height set to %d, was %d\n",
		hh, last->box.height);
	    last->box.height = hh;
	}
    }
#else
    XdbDebug2(__FILE__, rc, last->kid,
	"DoLayoutVT: Last child's height SHOULD BE set to %d (it's %d)\n",
	XtHeight(rc) - last->box.y - RC_MarginH(rc) - MGR_ShadowThickness(rc),
	last->box.height);
#endif
    /* now we fill in the last column, so it takes up the remaining width, 
       if XmNadjustLast is True. */
    if (RC_AdjLast(rc)) 
    {
	for (j = first_in_column; j < MGR_NumChildren(rc); j++) 
        {
	    XmRCKidGeometry kid_geometry = &(RC_Boxes(rc)[j]);
	    Widget kid = kid_geometry->kid;

	    if (!kid || !XtIsManaged(kid))
		continue;

	    if (XtWidth(rc) <= current_x + RC_MarginW(rc) + MGR_ShadowThickness(rc)) {
		/* It may actually be wiser not to modify the width here.... FIX ME ?? */
		kid_geometry->box.width = 1;
	    } else {
		kid_geometry->box.width = (XtWidth(rc)
			- (current_x + RC_MarginW(rc) + MGR_ShadowThickness(rc)));
	    }
	}
    }
}

/*
 * Vertical Column
 *	XmNnumColumns does determine geometry here (and it does refer
 *	to number of columns).
 *	All children (of certain class) get to have the same height.
 *	It looks like separators and tearOffs don't get treated this way
 *	in Motif ...
 *
 * Called when :
 *	XmVERTICAL & XmPACK_COLUMN
 *	XmMENU_POPUP
 */
static void
DoLayoutVC(Widget rc, int mode, Widget cw,
	XtWidgetGeometry *cg, XtWidgetGeometry *rcg)
{
    int i, j, wpc;
    int starting;
    int number_per_column;
    Dimension current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
    Dimension current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
    Dimension height_of_widgets = 0, toch = 0;
    Dimension *width_of_column, ww;
    Dimension		rcw, rch;
    XtWidgetGeometry	geo;
    Widget kid;
    XmRCKidGeometry kid_geometry;
    int toc;			/* 1 if there is a TearOffControl */
    int *widgets_per_column;	/* Amount of equally sized widgets per column */

    XdbDebug(__FILE__, rc, "DoLayoutVC(ncols %d)\n", RC_NCol(rc));

    toc = XmIsTearOffButton(MGR_Children(rc)[0]) ? 1 : 0;
    if (RC_Type(rc) != XmMENU_PULLDOWN && RC_Type(rc) != XmMENU_POPUP)
	toc = 0;
    if (RC_TearOffModel(rc) == XmTEAR_OFF_DISABLED)
	toc = 0;
    if (! XtIsManaged(MGR_Children(rc)[0]))
	toc = 0;

#define	COLUMN_OF_THIS_WIDGET(i)	((i - toc) / number_per_column)

    if (RC_NCol(rc) == 0) {
	/* what the hell.  punt and go to .. */
	DoLayoutVT(rc, mode, NULL, NULL, NULL);
	return;
    }

    if (toc)
	number_per_column = (MGR_NumChildren(rc) - 2) / RC_NCol(rc) + 1;
    else
	number_per_column = (MGR_NumChildren(rc) - 1) / RC_NCol(rc) + 1;

    width_of_column = (Dimension *) XtCalloc(RC_NCol(rc), sizeof(Dimension));
    widgets_per_column = (int *) XtCalloc(RC_NCol(rc), sizeof(int));

    for (i = 0; i < RC_NCol(rc); i++)
	widgets_per_column[i] = width_of_column[i] = 0;

    /* first, figure out the height of each row and the width of each column */
    for (i = 0; i < MGR_NumChildren(rc); i++) {
	kid_geometry = &(RC_Boxes(rc)[i]);
	kid = kid_geometry->kid;

	if (!kid || !XtIsManaged(kid) || XmIsTearOffButton(kid))
	    continue;

	if (! (XmIsSeparator(kid) || XmIsSeparatorGadget(kid)))
	    widgets_per_column[COLUMN_OF_THIS_WIDGET(i)] ++;

	if (kid == cw && cg->request_mode & CWHeight) {
	  if (height_of_widgets < cg->height)
	    height_of_widgets = cg->height;
	} else {
	  if (height_of_widgets < kid_geometry->box.height)
	    height_of_widgets = kid_geometry->box.height;
	}

	if (kid == cw && cg->request_mode & CWWidth) {
	  if (width_of_column[COLUMN_OF_THIS_WIDGET(i)] < cg->width)
	    width_of_column[COLUMN_OF_THIS_WIDGET(i)] = cg->width;
	} else {
	  if (width_of_column[COLUMN_OF_THIS_WIDGET(i)] 
		< kid_geometry->box.width)
	    width_of_column[COLUMN_OF_THIS_WIDGET(i)] = kid_geometry->box.width;
	}
    }

/* Figure out how big we need to be */
    rcw = 2 * (RC_MarginW(rc) + MGR_ShadowThickness(rc))
		+ RC_Spacing(rc) * (RC_NCol(rc) - 1);
    rch = 2 * (RC_MarginH(rc) + MGR_ShadowThickness(rc))
		+ height_of_widgets * number_per_column
		+ RC_Spacing(rc) * (number_per_column - 1);
    if (toc)
	rch += XtHeight(MGR_Children(rc)[0]);

#ifdef VC_HAS_EQUAL_WIDTH_TOO
/* It looks like the children need to have both width and height the same */
    ww = 0;
    for (i=0; i<RC_NCol(rc); i++)
	if (ww < width_of_column[i])
	    ww = width_of_column[i];

    rcw += ww * RC_NCol(rc);
#else
/*
 * Old implementation (dates from 1996, yuck - it's a full week into 1997 now)
 * only gave all children the same height.
 */
    for (i=0; i<RC_NCol(rc); i++)
	rcw += width_of_column[i];
#endif

    rcg->width = rcw;
    rcg->height = rch;
    rcg->request_mode = CWWidth | CWHeight;

    if (mode & Mode_Test)
        return;

    geo.request_mode = CWWidth | CWHeight;
    geo.width = rcw;
    geo.height = rch;

    if (mode != Mode_Test) {
	if (_XmMakeGeometryRequest(rc, &geo) == XtGeometryYes) {
	    XtWidth(rc) = geo.width;
	    XtHeight(rc) = geo.height;
	}
    }

/*
 * We might have a problem now if the geometry that we have is smaller than
 * the desired one. Especially the height should be taken care of.
 * If we got a height smaller than the height that we need for one of the columns,
 * then height_of_widgets should be decreased.
 * Decrease by getting the amount of widgets in that column ...
 */
    wpc = 0;
    for (i=0; i<RC_NCol(rc); i++)
	if (widgets_per_column[i] > wpc)
	    wpc = widgets_per_column[i];

    if (2 * (RC_MarginH(rc) + MGR_ShadowThickness(rc)) + wpc * height_of_widgets
		+ RC_Spacing(rc) * (wpc - 1) > geo.height) {

	XdbDebug(__FILE__, rc, "DoLayoutVC: Height needed %d, got %d; height of widgets %d -> %d\n",
		rch, geo.height, height_of_widgets, height_of_widgets - (rch - geo.height) / wpc);

	if (height_of_widgets - (rch - geo.height) / wpc <= 0) {
		XdbDebug(__FILE__, rc, "DoLayoutVC: height_of_widgets untouched\n");
	} else
		height_of_widgets -= (rch - geo.height) / wpc;
    }

    /*
     * Now we lay out the children
     * First, we do our tear off control, if we have one
     *
     * (That's because the TearOffControl should span all columns)
     */
    if ((RC_Type(rc) == XmMENU_POPUP || RC_Type(rc) == XmMENU_PULLDOWN)
	&& RC_TearOffModel(rc) == XmTEAR_OFF_ENABLED && RC_TearOffControl(rc) 
	&& XtIsManaged(RC_TearOffControl(rc))) {
	kid_geometry = &(RC_Boxes(rc)[0]);	/* the tear off control is here */
	kid = kid_geometry->kid;

	kid_geometry->box.x = current_x;
	kid_geometry->box.y = current_y;
	kid_geometry->box.width = XtWidth(rc) - RC_MarginW(rc) - MGR_ShadowThickness(rc);
#ifdef VC_HAS_EQUAL_WIDTH_TOO
	toch = kid_geometry->box.height = ww;
#else
	toch = kid_geometry->box.height = XtHeight(kid);
#endif

	current_y += RC_Spacing(rc) + kid_geometry->box.height;
	starting = 1;
    }
    else
	starting = 0;

    j = 0;			/* Count the #children already had in this column */
    for (i = starting; i < MGR_NumChildren(rc); i++) {

#if 0
	if (i != 0 && (i % number_per_column) == 0)
#endif
	    if (j == number_per_column) {
		j = 0;
		/*
		 * Current_y should be incremented with the height of TearOffControl if
		 * there is one.
		 */
		current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
		if (starting)	/* There is a TearOffControl */
		    current_y += toch;

#ifdef VC_HAS_EQUAL_WIDTH_TOO
		current_x += ww + RC_Spacing(rc);
#else
		current_x += width_of_column[COLUMN_OF_THIS_WIDGET(i)] + RC_Spacing(rc);
#endif
	    }

	kid_geometry = &(RC_Boxes(rc)[i]);
	kid = kid_geometry->kid;

	if (!kid || !XtIsManaged(kid) || XmIsTearOffButton(kid))
	    continue;

	kid_geometry->box.x = current_x;
	kid_geometry->box.y = current_y;

#ifdef VC_HAS_EQUAL_WIDTH_TOO
	kid_geometry->box.width = ww;
#else
	kid_geometry->box.width = width_of_column[COLUMN_OF_THIS_WIDGET(i)];
#endif
	kid_geometry->box.height = height_of_widgets;

	current_y += RC_Spacing(rc) + height_of_widgets;

	j++;
    }

    /*
     * Now we fill in the last column 
     * so it takes up the remaining width if XmNadjustLast is True.
     *
     * (The elements in the last column were also processed above, hence no
     *      x, y, height corrections here.)
     */
    if (RC_AdjLast(rc))
	for (j = MGR_NumChildren(rc) - number_per_column;
	     j < MGR_NumChildren(rc);
	     j++) {
	    kid_geometry = &(RC_Boxes(rc)[j]);
	    kid = kid_geometry->kid;

	    if (!kid || !XtIsManaged(kid))
		continue;

	    if (current_x + RC_MarginW(rc) + MGR_ShadowThickness(rc) < XtWidth(rc))
		kid_geometry->box.width = (XtWidth(rc)
					   - current_x
					   - RC_MarginW(rc)
					   - MGR_ShadowThickness(rc));
	}

    /* Cleanup */
    XtFree((XtPointer)width_of_column);
    XtFree((XtPointer)widgets_per_column);
}

static void
resize(Widget w)
{
    RC_SetFromResize(w, 1);
    if (XtWidth(w) == 0 || XtHeight(w) == 0)
	XmRowColumnLayout(w, Mode_Normal, NULL, NULL, NULL);
    else
	XmRowColumnLayout(w, Mode_Resize, NULL, NULL, NULL);
}

/*
 * FIX ME
 *
 * This sucker badly needs work.
 *
 * Note (as indicated in Form.c) that we are called from the widget "w" here,
 * and that the processing of RcGeometryManager should NEVER result in geometry
 * changes to that widget.
 *
 * FIX ME : Every time we return XtGeometryYes, we have a situation where a child
 *	widget is granted permission to change its geometry. We'll have to assume
 *	that it does this, and change our "boxes".
 *	An alternative would be to indicate in boxes that its contents is invalid,
 *	and have the next user of boxes look up the new geometry info.
 * Partially applied 25/8/1996 (Danny).
 */
static XtGeometryResult
RcGeometryManager(Widget w,
		 XtWidgetGeometry * request,
		 XtWidgetGeometry * reply)
{
    Widget		rc = XtParent(w);

    if (XmIsMenuShell(XtParent(rc)))
	/* we're the direct child of a menu shell, so allow any resizing -- FIX ME */
    {
	Widget msw = XtParent(rc);
	XtWidgetGeometry        r = *request;
  
	XdbDebug2(__FILE__, rc, w, "RC in MenuShell GeometryManager - REQ %s RC %d %d Child %d %d\n",
		XdbWidgetGeometry2String(request),
                XtWidth(rc), XtHeight(rc),
                XtWidth(w), XtHeight(w));

	if ( !(request->request_mode & CWWidth))
		r.width = XtWidth(w);
	if ( !(request->request_mode & CWHeight))
		r.height = XtHeight(w);
  
	XtWidth(rc) = r.width + 2 * RC_MarginW(rc);
	XtHeight(rc) = r.height + 2 * RC_MarginH(rc);

	XtWidth(msw) = r.width + 2 * RC_MarginW(rc);
	XtHeight(msw) = r.height + 2 * RC_MarginH(rc);

	XdbDebug2(__FILE__, rc, w, "RC GeometryManager => YES, RC %d %d MS %d %d\n",
		XtWidth(rc), XtHeight(rc),
		XtWidth(msw), XtHeight(msw));

	/* FIX ME : boxes */

	return XtGeometryYes;
    }

#ifdef	GEO_TIGHT_STRICT
/* If we're PACK_TIGHT then chances of getting what you ask are pretty slim */
   if (RC_Packing(rc) == XmPACK_TIGHT) {
	if (((request->request_mode & CWHeight) && request->height != XtHeight(w))
	 || ((request->request_mode & CWWidth) && request->width != XtWidth(w))) {
	    XdbDebug2(__FILE__, rc, w, "RcGeometryManager: request %s => No (PACK_TIGHT)\n",
		XdbWidgetGeometry2String(request));
	    return XtGeometryNo;
	}
   }
#endif

/* We're not a menu */
    *reply = *request;
    reply->request_mode = CWWidth | CWHeight;

    XdbDebug2(__FILE__, rc, w, "RC GeometryManager: XmRowColumnLayout\n");
    RC_SetFromResize(w, 0);
    XmRowColumnLayout(rc, Mode_Normal, w, reply, NULL);

/* FIX ME : boxes */

    if ((reply->request_mode & CWWidth) && (reply->width == request->width)
     && (reply->request_mode & CWHeight) && (reply->height == request->height))
	return XtGeometryYes;

    XdbDebug2(__FILE__, rc, w, "RcGeometryManager request [%s] reply [%s] => Almost\n",
	XdbWidgetGeometry2String(request), XdbWidgetGeometry2String(reply));

    return XtGeometryAlmost;
}

static void
change_managed(Widget w)
{
    XdbDebug(__FILE__, w, "change_managed()\n");

    initialize_boxes(w, NULL, NULL);

    XmRowColumnLayout(w, Mode_Normal, NULL, NULL, NULL);

    _XmNavigChangeManaged(w);
}

static void
_XmRadioCallback(Widget w, 
                 XtPointer cd,
                 XtPointer cs)
{
   Widget tgl;
   Widget rc = (Widget) cd;
   XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) cs;
   int i;

   if (cbs == NULL || !RC_RadioBehavior(rc))
        return;

   if (RC_RadioAlwaysOne(rc) && cbs->set == False) {
	int     cnt = 0;
	for (i=0; i<MGR_NumChildren(rc); i++) {
	    tgl = MGR_Children(rc)[i];
	    if (XmIsToggleButton(tgl) && XmToggleButtonGetState(tgl))
		cnt++;
	    else if (XmIsToggleButtonGadget(tgl) && XmToggleButtonGadgetGetState(tgl))
		cnt++;
	}
     if (cnt == 0) {
	/* Whow. You're resetting the radio box without written permission.
	 * Stop right there ! */
	if (XmIsToggleButton(w))
	    XmToggleButtonSetState(w, True, False);
	else if (XmIsToggleButtonGadget(w))
            XmToggleButtonGadgetSetState(w, True, False);

	return;
     }
   }

   for (i = 0; i < MGR_NumChildren(rc); i++) {
	tgl = MGR_Children(rc)[i];
	    if (XmIsToggleButton(tgl) && XmToggleButtonGetState(tgl)) {
		if (w != tgl && cbs && cbs->set) {
		    XmToggleButtonSetState(tgl, False, True);
		} else if (w == tgl && cbs && cbs->set)
		    XmToggleButtonSetState(tgl, True, False);
	    } else if (XmIsToggleButtonGadget(tgl) && XmToggleButtonGadgetGetState(w)) {
		if (w != tgl && cbs && cbs->set)
		    XmToggleButtonGadgetSetState(tgl, False, True);
		else if (w == tgl && cbs && cbs->set)
		    XmToggleButtonGadgetSetState(tgl, True, False);
            }
    }

}

static void
_XmEntryCallback(Widget w, 
                 XtPointer cd,
                 XtPointer cs)
{
    XmRowColumnCallbackStruct rcb;
    XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)cs;
    Widget rc = (Widget)cd;

#if 0
    if (XmIsToggleButton(w) || XmIsToggleButtonGadget(w)) {
	if (cbs->set == 0)
	    return;
    }
#endif
    rcb.reason = XmCR_ACTIVATE;
    rcb.event = cbs->event;
    rcb.widget = w;
    rcb.data = (char *)cd;
    rcb.callbackstruct = (char *)cs;
    XtCallCallbackList(rc, RC_Entry_cb(rc), (XtPointer)&rcb);
}

static Cardinal 
_XmRowColumnOrderProc(Widget widget)
{
  if (RC_constraint_IndexPosition(widget) == XmLAST_POSITION
      || RC_constraint_IndexPosition(widget) >= MGR_NumChildren(XtParent(widget)))
  {
      return MGR_NumChildren(XtParent(widget));
  }
  else 
  {
      return RC_constraint_IndexPosition(widget);
  }
}

static void
insert_child(Widget w)
{
    Widget rc = (Widget) XtParent(w);

/*
  This may be correct, but it keeps me from adding CascadeButtonGadgets
  to a menu bar...   Chris

    if (rc->row_column.is_homogeneous
        && (rc->row_column.entry_class != XtClass(w))) 
    {
       _XmWarning(w, "Child of homogeneous rowcolumn is of wrong class!");
       return;
    }
*/
    if ((RC_RadioBehavior(rc) || RC_RadioAlwaysOne(rc)) &&
	((XtClass(w) == xmToggleButtonWidgetClass)
	 || (XtClass(w) == xmToggleButtonGadgetClass)))
	XtAddCallback(w, XmNvalueChangedCallback, _XmRadioCallback, rc);

#define superclass (&xmManagerClassRec)
    (*superclass->composite_class.insert_child) (w);
#undef superclass

    XdbDebug2(__FILE__, rc, w, "InsertChild\n");

    /*
     * if it's a label subclass, set its alignment to our
     * RC_EntryAlignment resource
     */
    if (RC_IsAligned(rc) && (! XmIsDialogShell(XtParent(rc)))
		&& (XmIsLabel(w) || XmIsLabelGadget(w))) {
	Arg	arg;
	XtSetArg(arg, XmNalignment, RC_EntryAlignment(rc));
	XtSetValues(w, &arg, 1);
	XdbDebug2(__FILE__, rc, w, "InsertChild: Set Alignment to %s\n",
		XdbAlignment2String(RC_EntryAlignment(rc)));
    }


    if (RC_Entry_cb(rc)) 
    {
	if (XmIsDrawnButton(w)) 
        {
	    if (DB_ActivateCallback(w))
		XtRemoveCallbacks(w, XmNactivateCallback, DB_ActivateCallback(w));
	    XtAddCallback(w, XmNactivateCallback, _XmEntryCallback, rc);
	}
	else if (XmIsCascadeButton(w)) 
        {
	    if (CB_ActivateCall(w))
		XtRemoveCallbacks(w, XmNactivateCallback, CB_ActivateCall(w));
	    XtAddCallback(w, XmNactivateCallback, _XmEntryCallback, rc);
	}
	else if (XmIsCascadeButtonGadget(w)) 
        {
	    if (CBG_ActivateCall(w))
		XtRemoveCallbacks(w, XmNactivateCallback, CBG_ActivateCall(w));
	    XtAddCallback(w, XmNactivateCallback, _XmEntryCallback, rc);
	}
	else if (XmIsPushButton(w)) 
        {
	    if (PB_ActivateCallback(w))
		XtRemoveCallbacks(w, XmNactivateCallback, PB_ActivateCallback(w));
	    XtAddCallback(w, XmNactivateCallback, _XmEntryCallback, rc);
	}
	else if (XmIsPushButtonGadget(w)) 
        {
	    if (PBG_ActivateCallback(w))
		XtRemoveCallbacks(w, XmNactivateCallback, PBG_ActivateCallback(w));
	    XtAddCallback(w, XmNactivateCallback, _XmEntryCallback, rc);
	}
	else if (XmIsToggleButton(w)) 
        {
	    if (TB_ValueChangedCallback(w))
		XtRemoveCallbacks(w, XmNvalueChangedCallback, TB_ValueChangedCallback(w));
	    XtAddCallback(w, XmNvalueChangedCallback, _XmEntryCallback, rc);
	}
	else if (XmIsToggleButtonGadget(w)) 
        {
	    if (TBG_ValueChangedCallback(w))
		XtRemoveCallbacks(w, XmNvalueChangedCallback, TBG_ValueChangedCallback(w));
	    XtAddCallback(w, XmNvalueChangedCallback, _XmEntryCallback, rc);
	}
    }

    if (XmIsTearOffButton(w)) {
	RC_TearOffControl(rc) = w;
	XdbDebug2(__FILE__, rc, w, "InsertChild: this is the TearOff control\n");
    }

    InitializeBoxesInsert(rc, w);
}

/* Determine if widget w is a top level menu pane/bar, and that the given event
 * (assumed to be a button event !) occured outside widget w's child widgets */
static Boolean
ExternalBtnEvent(Widget w,
		 XEvent * event)
{
    Widget event_widget;
    Boolean is_child = False;

    /* Determine the top-level widget for 'w' */
    Widget w_top_level = RC_LastSelectToplevel(w);

    XdbDebug(__FILE__, w, 
	     "  Determining if the button event was external to the menu system\n");

    /* Determine if w is a top level menu pane/bar */
    if ((w_top_level == w) || !w_top_level) {
	/* Determine the event window */
	Window event_window = (event->xbutton.subwindow) ?
	event->xbutton.subwindow : event->xbutton.window;

	/* Convert window to a widget ID */
	event_widget = XtWindowToWidget(XtDisplay(w), event_window);

	/* Now determine if the event widget is a child (direct of indirect)
	 * of widget w. */
	if (event_widget && (event_widget != w)) {
	    Widget parent = XtParent(event_widget);

	    /* Search event widgets parents, to see if 'w' is an ancestor */
	    while (parent && (parent != w))
		parent = XtParent(parent);

	    is_child = (parent == w);
	}			/* if event widget is not 'w' */
    }				/* If event widget is a top level menu pane/bar */

    return (!is_child);
}

/* action routines */

static void
MenuBtnDown(Widget w,
	    XEvent * event,
	    String * params,
	    Cardinal * num_params)
{
    Widget gadget;

    /* If event was in a child gadget, then arm it, otherwise determine
     * whether the event occured outside the menu system, and do a cleanup
     * if it did. */
    gadget = (Widget) _XmInputInGadget(w,
				       event->xkey.x,
				       event->xkey.y);

    if (gadget) {
	/* Check whether event is, because of springloaded grab, delivered
	 * for the second time.
	 * FIX ME: XAllowEvents(SyncPointer) in else clause?
	 * FIX ME: This is a workaround, needs a consistent solution.
	 */
	if (XtWindow(w) == event->xbutton.window) {
	    /* Event was in a gadget */
	    XdbDebug2(__FILE__, w, gadget, "MenuBtnDown() in gadget\n");

	    MGR_HighlightedWidget(w) = gadget;

	    /* turn on mouse traversal */
	    _XmSetInDragMode(w, True);

	    /* Now we arm the widget */

	    /* FIX ME: this used to be _XmGadgetArm(), it seems to be a good
	    * thing to bypass logic, 'cause we get a XAllowEvents this way.
	    */
	
	    _XmDispatchGadgetInput((Widget)gadget, event, XmARM_EVENT);
	}
    }
    else if (ExternalBtnEvent(w, event)) {
	XdbDebug(__FILE__, w, "MenuBtnDown(event external to menu system)\n");
	/*
	 * Event was external to the menu system;
	 * Do a cleanup, and replay the event to the outside world.
	 */
	DoBtnEventCleanupReplay(w, event, params, num_params);
    }
    else {
	XdbDebug(__FILE__, w, "MenuBtnDown(not in gadget)\n");
    }
}

static void
DoBtnEventCleanupReplay(Widget w,
			XEvent *event,
			String *params,
			Cardinal *num_params)
{
    Widget	menu_shell = NULL, mb = NULL;

    XdbDebug(__FILE__, w, "DoBtnEventCleanupReplay\n");

    if (RC_PopupPosted(w))
      menu_shell = XtParent(RC_PopupPosted(w));

    if (menu_shell == NULL) {	/* Think this is always happening - Danny 9/5/1996 */
	mb = w;

	while (mb && !XmIsMenuBar(mb))
	    mb = XtParent(mb);

	if (RC_PopupPosted(mb))
	  menu_shell = XtParent(RC_PopupPosted(mb));

	XdbDebug(__FILE__, w, "DoBtnEventCleanupReplay: menu bar is '%s'\n",
		 mb ? XtName(mb) : "(null)");

	/* disarm the menu bar */
	RC_SetArmed(mb, 1);
    }

    if (menu_shell) {

	if (menu_shell) {
	    XdbDebug(__FILE__, w, 
		     "DoBtnEventCleanupReplay: calling MenuShellPopdownDone\n");
	    XtCallActionProc(menu_shell, "MenuShellPopdownDone",
			     event, params, *num_params);
	}

	if (XmIsMenuBar(w)) {
	    XtRemoveGrab(w);
	    XdbDebug(__FILE__, w, "XtRemoveGrab()\n");
	} else {
	    _XmWarning(w, "FIX ME - no menu bar at %s %d\n", __FILE__, __LINE__);
	}

	_XmMenuFocus(w, XmMENU_FOCUS_RESTORE, CurrentTime);

	_XmUngrabKeyboard(w, CurrentTime);

	/* FIX ME: ungran done by ReplayPointer, or not? */
/*	_XmUngrabPointer(w, CurrentTime);*/

	_XmSetInDragMode(w, False);

	/* Replay the event */
	XdbDebug(__FILE__, w, "DoBtnEventCleanupReplay: XAllowEvents (replay)\n");
	XAllowEvents(XtDisplay(w), ReplayPointer, CurrentTime);

    }
    else {
	XdbDebug(__FILE__, w, "DoBtnEventCleanupReplay: RC_PopupPosted was False\n");
    }
}

static void
MenuBtnUp(Widget w,
	  XEvent * event,
	  String * params,
	  Cardinal * num_params)
{
    Widget gadget;

    /*
     * If event was in a child gadget, then arm it, otherwise determine
     * whether the event occured outside the menu system, and do a cleanup if it did.
     */
    gadget = (Widget) _XmInputInGadget(w,
				       event->xkey.x,
				       event->xkey.y);

    if (gadget) {

	XdbDebug2(__FILE__, w, gadget, "MenuBtnUp(in gadget)\n");

	_XmSetInDragMode(w, False);

	/* Now we activate the gadget */
	/* FIX ME: this used to be _XmGadgetActivate(), it seems to be a good
	 * thing to bypass logic, 'cause we get a XAllowEvents this way.
         */
	
	_XmDispatchGadgetInput((Widget)gadget, event, XmACTIVATE_EVENT);
    }
    else if (ExternalBtnEvent(w, event)) {

	XdbDebug(__FILE__, w, "MenuBtnUp(event external to menu system)\n");

	/*
	 * Event was external to the menu system;
	 * Do a cleanup, and replay the event to the outside world.
	 */
	DoBtnEventCleanupReplay(w, event, params, num_params);
    }
    else {
	XdbDebug(__FILE__, w, "MenuBtnUp(not in gadget)\n");
    }
}

static void
MenuEnter(Widget w,
	  XEvent * event,
	  String * params,
	  Cardinal * num_params)
{
    XdbDebug(__FILE__, w, "MenuEnter()\n");
}

static void
MenuUnmap(Widget w,
	  XEvent * event,
	  String * params,
	  Cardinal * num_params)
{
    XdbDebug(__FILE__, w, "MenuUnmap()\n");
    _XmCallRowColumnUnmapCallback(w, event);
}

static void
MenuFocusIn(Widget w,
	    XEvent * event,
	    String * params,
	    Cardinal * num_params)
{
    XdbDebug(__FILE__, w, "MenuFocusIn\n");
}

static void
MenuFocusOut(Widget w,
	     XEvent * event,
	     String * params,
	     Cardinal * num_params)
{
    XdbDebug(__FILE__, w, "MenuFocusOut\n");
    /* restore the focus to the widget that had it before moving to the menu bar */
}

static void
MenuBarEnter(Widget w,
	     XEvent * event,
	     String * params,
	     Cardinal * num_params)
{
    XdbDebug(__FILE__, w, "MenuBarEnter()\n");
}

static void
MenuBarLeave(Widget w,
	     XEvent * event,
	     String * params,
	     Cardinal * num_params)
{
    XdbDebug(__FILE__, w, "MenuBarLeave()\n");
}

static void
MenuBarGadgetSelect(Widget w,
		    XEvent * event,
		    String * params,
		    Cardinal * num_params)
{
    XdbDebug(__FILE__, w, "MenuBarGadgetSelect()\n");
}

/* convenience functions */

Widget
XmCreateMenuBar(Widget parent,
		char *name,
		Arg * arglist,
		Cardinal argcount)
{
    /* menu bar's have the their rowColumnType set to XmMENU_BAR and
       are homogeneous (they only accept CascadeButtons, and 
       CascadeButtonGadgets */

    Widget w;
    Arg myArgList[4];
    int n = 0;

    ArgList combined;

    XtSetArg(myArgList[n], XmNrowColumnType, XmMENU_BAR); n++;
    XtSetArg(myArgList[n], XmNorientation, XmHORIZONTAL); n++;
    XtSetArg(myArgList[n], XmNisHomogeneous, True); n++;
    XtSetArg(myArgList[n], XmNentryClass, xmCascadeButtonWidgetClass); n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    w = XtCreateWidget(name,
		       xmRowColumnWidgetClass,
		       parent,
		       combined, n + argcount);

    XtFree((char *)combined);

    return w;
}

Widget
XmCreateOptionMenu(Widget parent,
		   char *name,
		   Arg * arglist,
		   Cardinal argcount)
{
    /* option menus have the their rowColumnType set to XmMENU_OPTION */
    Widget w;
    Arg myArgList[1];
    int n = 0;

    ArgList combined;

    XtSetArg(myArgList[n], XmNrowColumnType, XmMENU_OPTION); n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    w = XtCreateWidget(name,
		       xmRowColumnWidgetClass,
		       parent,
		       combined, n + argcount);

    XtFree((char *)combined);

    return w;
}

Widget
XmCreatePopupMenu(Widget parent,
		  char *name,
		  Arg * arglist,
		  Cardinal argcount)
{
    /* popup menus have the their rowColumnType set to XmMENU_POPUP */

    Widget w, ms;
    Arg myArgList[1];
    Arg shell_args[3];
    int shell_ac;
    int n=0;
    char *popup_name = XtMalloc(strlen("popup_") + strlen(name) + 1);

    ArgList combined;

    strcpy(popup_name, "popup_");
    strcat(popup_name, name);


    XtSetArg(myArgList[n], XmNrowColumnType, XmMENU_POPUP); n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    shell_ac = 0;
    XtSetArg(shell_args[shell_ac], XmNwidth, 10); shell_ac ++; /* FIX ME */
    XtSetArg(shell_args[shell_ac], XmNheight, 10); shell_ac ++; /* FIX ME */
    XtSetArg(shell_args[shell_ac], XmNallowShellResize, True); shell_ac ++;

    ms = XtCreatePopupShell(popup_name,
			    xmMenuShellWidgetClass,
			    parent, shell_args, shell_ac);

    XtFree(popup_name);

    w = XtCreateWidget(name, 
		       xmRowColumnWidgetClass,
		       ms,
		       combined, n+argcount);


    /* since we're not managed -- we leave that up to the application
       to get us to pop up, we realize ourselves here. */
    /*XtRealizeWidget(w);*/

    XtAddEventHandler(parent, ButtonPressMask, False,
		      _XmPopupButtonPressHandler, (XtPointer) w);

    XtFree((char *)combined);

    return w;
}

Widget
XmCreatePulldownMenu(Widget parent,
		     char *name,
		     Arg * arglist,
		     Cardinal argcount)
{
/* pulldown menus have their rowColumnType set to XmMENU_PULLDOWN */
    Widget w, ms;
    Arg myArgList[5];
    int n = 0;
    char *popup_name = XtMalloc(strlen("popup_") + strlen(name) + 1);
    ArgList combined;
    Arg shell_args[3];
    int shell_ac;

    strcpy(popup_name, "popup_");
    strcat(popup_name, name);

    XtSetArg(myArgList[n], XmNrowColumnType, XmMENU_PULLDOWN); n++;
    XtSetArg(myArgList[n], XmNorientation, XmVERTICAL); n++;
    XtSetArg(myArgList[n], XmNpacking, XmPACK_COLUMN); n++;
    XtSetArg(myArgList[n], XmNnumColumns, 1); n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    shell_ac = 0;
    XtSetArg(shell_args[shell_ac], XmNwidth, 10); shell_ac ++; /* FIX ME */
    XtSetArg(shell_args[shell_ac], XmNheight, 10); shell_ac ++; /* FIX ME */
    XtSetArg(shell_args[shell_ac], XmNallowShellResize, True); shell_ac ++;

    ms = XtCreatePopupShell(popup_name,
			    xmMenuShellWidgetClass,
			    parent, shell_args, shell_ac);

    XtFree(popup_name);

    w = XtCreateManagedWidget(name, 
			      xmRowColumnWidgetClass,
			      ms,
			      combined, n + argcount);

    XtFree((char *)combined);

    return w;
}

Widget
XmCreateRadioBox(Widget parent,
		 char *name,
		 Arg * arglist,
		 Cardinal argcount)
{
    /* radio boxes have their rowColumnType set to XmWORK_AREA, and their 
       radioBehavior set to true.  The also are homogeneous and accept 
       ToggleButtons, and ToggleButtonGadgets */

    Widget w;
    Arg myArgList[2];
    int n = 0;

    ArgList combined;

    XtSetArg(myArgList[n], XmNrowColumnType, XmWORK_AREA); n++;
    XtSetArg(myArgList[n], XmNradioBehavior, True); n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    w = XtCreateWidget(name,
		       xmRowColumnWidgetClass,
		       parent,
		       combined, n + argcount);

    XtFree((char *)combined);

    return w;
}

Widget
XmCreateRowColumn(Widget parent,
		  char *name,
		  Arg * arglist,
		  Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmRowColumnWidgetClass,
			  parent,
			  arglist,
			  argcount);
}

Widget
XmCreateWorkArea(Widget parent,
		 char *name,
		 Arg * arglist,
		 Cardinal argcount)
{
    Widget w;
    Arg myArgList[2];
    int n = 0;

    ArgList combined;

    XtSetArg(myArgList[n], XmNrowColumnType, XmWORK_AREA);
    n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    w = XtCreateWidget(name,
		       xmRowColumnWidgetClass,
		       parent,
		       combined, n + argcount);

    XtFree((char *)combined);

    return w;
}

Widget
XmOptionButtonGadget(Widget option_menu)
{
    return XtNameToWidget(option_menu, RC_OPTION_CBG);
}

Widget
XmOptionLabelGadget(Widget option_menu)
{
    return XtNameToWidget(option_menu, RC_OPTION_LABEL);
}

void
XmMenuPosition(Widget menu,
	       XButtonPressedEvent * event)
{
    if (!XmIsMenuShell(XtParent(menu))
	|| RC_Type(menu) != XmMENU_POPUP)
    {
        _XmWarning(menu, "XmMenuPosition called with a non popup menu.");
	return;
    }

    /* should be all that's needed */
    _XmMoveObject(XtParent(menu),
		  event->x_root,
		  event->y_root);
}

Widget
XmGetTearOffControl(Widget menu)
{
    if (XmIsRowColumn(menu))
	return RC_TearOffControl(menu);
    return NULL;
}

void
XmAddToPostFromList(Widget menu_wid, 
		    Widget widget)
{
  /* well, we could either be adding menu_wid to widget's post
     from list or adding widget to menu_wid's post from list.
     I'll opt for the latter. */
  if (RC_PostFromList(menu_wid) == NULL)
  {
    RC_PostFromListSize(menu_wid) = 5;

    RC_PostFromList(menu_wid) = (Widget*)XtMalloc(sizeof(Widget)
						  * RC_PostFromListSize(menu_wid));
    RC_PostFromCount(menu_wid) = 0;
  }

  RC_PostFromList(menu_wid) [ RC_PostFromCount(menu_wid)++ ] = widget;

  if (RC_PostFromCount(menu_wid) == RC_PostFromListSize(menu_wid))
  {
    RC_PostFromListSize(menu_wid) *= 2;

    RC_PostFromList(menu_wid) = (Widget*)XtRealloc((char*) RC_PostFromList(menu_wid),
						   sizeof(Widget)
						   * RC_PostFromListSize(menu_wid));
  }
}

void
XmRemoveFromPostFromList(Widget menu_wid, 
			 Widget widget)
{
  int i;

  if (RC_PostFromList(menu_wid) == NULL)
    return; /* should we post an error/warning? */

  for (i = 0; i < RC_PostFromCount(menu_wid); i++ )
  {
    if (RC_PostFromList(menu_wid)[i] == widget)
    {
        int j;

	/* found it, now slide everything down. */
        for (j = i ; j < RC_PostFromCount(menu_wid) - 1 ; j ++)
	  RC_PostFromList(menu_wid)[j] = RC_PostFromList(menu_wid)[j + 1];

	RC_PostFromCount(menu_wid) --;

	return;
    }
  }

  /* should getting to this point also be an error? */
}

/*
 * The result of XmGetPostedFromWidget when the callback is activated by
 * an accelerator is not defined in the manual.
 */
Widget
XmGetPostedFromWidget(Widget menu)
{
    if (XmIsRowColumn(menu)) 
    {
	if (RC_LastSelectToplevel(menu))
	    return RC_LastSelectToplevel(menu);
	/* return something sensible */
	return menu;
    }
    return NULL;
}

/*
 * most of these are already written under different names
 */
void
_XmPostPopupMenu(Widget wid,
		 XEvent * event)
{
    if (!XmIsRowColumn(wid)
	|| RC_Type(wid) != XmMENU_POPUP)
    {
      _XmWarning(wid,
		 "_XmPostPopupMenu sent non rowcolumn widget\n");
      return;
    }

    _XmSetInDragMode(wid, True);
    _XmSetInPMMode(wid, True);

    XtPopupSpringLoaded(XtParent(wid));
}

void
_XmSetPopupMenuClick(Widget wid, 
		     Boolean popupMenuClick)
{
}

Boolean
_XmGetPopupMenuClick(Widget wid)
{
    return False;
}

void
_XmAllowAcceleratedInsensitiveUnmanagedMenuItems(Widget wid,
						 Boolean allowed)
{
  XmMenuState state = _XmGetMenuState(wid);

  state->RC_allowAcceleratedInsensitiveUnmanagedMenuItems = allowed;
}

void
_XmSetSwallowEventHandler(Widget widget, 
			  Boolean add_handler)
{
}

void
_XmMenuFocus(Widget w, 
	     int operation, 
	     Time _time)
{
    XmMenuState state = _XmGetMenuState(w);
    Window	window_return;
    int		revert_to_return;
    /* nothing but a guess here.  There are three operations that seem
       to take place with respect to focus and menus.
       
       1 - saving the keyboard focus (done at the start of the menu stuff.)
       2 - setting the keyboard focus (done at several points.)
       3 - restoring the keyboard focus (done at the end of the menu stuff.)
       
       So, those are the three "operations" that are valid to this function.
       */
    switch (operation) 
    {
    case XmMENU_FOCUS_SAVE:
      XGetInputFocus(XtDisplay(w),
		     &state->RC_menuFocus.oldFocus,
		     &state->RC_menuFocus.oldRevert);

      state->RC_menuFocus.oldWidget = XtWindowToWidget(XtDisplay(w),
   				               state->RC_menuFocus.oldFocus);
      break;
    case XmMENU_FOCUS_RESTORE:
	XSetInputFocus(XtDisplay(w),
		       state->RC_menuFocus.oldFocus,
		       state->RC_menuFocus.oldRevert,
		       _time);

	XGetInputFocus(XtDisplay(w),
		       &window_return,
		       &revert_to_return);

	if (state->RC_menuFocus.oldFocus != window_return
	    || state->RC_menuFocus.oldRevert != revert_to_return)
	  XdbDebug(__FILE__, w, "  SetInputFocus call failed.\n");
      break;
    case XmMENU_FOCUS_SET:
      XSetInputFocus(XtDisplay(w),
		     XtWindow(w),
		     RevertToPointerRoot,
		     _time);

      XGetInputFocus(XtDisplay(w),
		     &window_return,
		     &revert_to_return);

      if (window_return != XtWindow(w)
	  || revert_to_return != RevertToPointerRoot)
      {
	XdbDebug(__FILE__, w, "  setting input focus failed\n");
	_XmUngrabKeyboard(w, _time);
	return;
      }
      break;
    }
}

void
_XmGetActiveTopLevelMenu(Widget wid, 
			 Widget * rwid)
{
}

Boolean
_XmMatchBSelectEvent(Widget wid, 
		     XEvent * event)
{
    return False;
}

Boolean
_XmMatchBDragEvent(Widget wid, 
		   XEvent * event)
{
    return False;
}

void
_XmHandleMenuButtonPress(Widget wid, 
			 XEvent * event)
{
}

void
_XmMenuBtnDown(Widget wid, 
	       XEvent * event,
	       String * params, 
	       Cardinal * num_params)
{
}

void
_XmMenuBtnUp(Widget wid, 
	     XEvent * event,
	     String * params, 
	     Cardinal * num_params)
{
}

void
_XmCallRowColumnMapCallback(Widget wid, XEvent * event)
{
    XmRowColumnCallbackStruct cbs;

    cbs.reason = XmCR_MAP;
    cbs.event = event;
    cbs.widget = NULL;
    cbs.data = NULL;
    cbs.callbackstruct = NULL;

    XtCallCallbackList(wid, RC_Map_cb(wid), (XtPointer) & cbs);
}

void
_XmCallRowColumnUnmapCallback(Widget wid, XEvent * event)
{
    XmRowColumnCallbackStruct cbs;

    cbs.reason = XmCR_UNMAP;
    cbs.event = event;
    cbs.widget = NULL;
    cbs.data = NULL;
    cbs.callbackstruct = NULL;

    XtCallCallbackList(wid, RC_Unmap_cb(wid), (XtPointer) & cbs);
}

void
_XmMenuPopDown(Widget w,
	       XEvent * event,
	       Boolean * popped_up)
{
}

Boolean
_XmIsActiveTearOff(Widget w)
{
    return RC_TearOffActive(w);
}

void
_XmMenuHelp(Widget w, 
	    XEvent *event, 
	    String *params, 
	    Cardinal *num_params)
{
    XtCallCallbacks(w,
		    XmNhelpCallback,
		    NULL);
}

/* synthetic resource converters */
static XmImportOperator
_XmToMenuPost(Widget widget,
	      int offset,
	      XtArgVal *value)
{
    String menuPost = (String)*value; /* the string we're checking/parsing. */
    Boolean valid_expression = True; /* we're optimistic :) */
    int numberIndex = 0; /* the index of the button number */
    int button = 0;
    
    XdbDebug(__FILE__, widget, "In _XmToMenuPost(%s)\n", menuPost);
    
    if (menuPost == NULL)
    {
        XdbDebug(__FILE__, widget, "  Default case -- menupost == null\n");
	return XmSYNTHETIC_NONE;
    }

    /* first we check the basic syntax of the expression. 
       for now we only allow things of the type <(Btn|Button)[0-5](Down)?> */

    if (valid_expression)
      if (!strncmp(menuPost, "<Btn", 4))
	numberIndex = 4;
      else if (!strncmp(menuPost, "<Button", 7))
	numberIndex = 7;
      else
      {
  	  XdbDebug(__FILE__, widget, "  Invalid beginning of string.\n");
	  valid_expression = False;
      }

    if (valid_expression)
      if (numberIndex + 1 >= strlen(menuPost))
      {
	  XdbDebug(__FILE__, widget, "  Invalid ending of string.\n");
	  valid_expression = False;
      }
      else if (!strncmp(&(menuPost[numberIndex+1]), "Down>", 5))
	valid_expression = True;
      else if (menuPost[numberIndex+1] == '>')
	valid_expression = True;
      else 
      {
	  XdbDebug(__FILE__, widget, "  Invalid ending of string.\n");
	  valid_expression = False;
      }
  
    if (valid_expression)
    {
        button = menuPost[numberIndex] - '0';
	
	if (button < 1 || button > 5)
        {
	    XdbDebug(__FILE__, widget, "  Invalid button in string.\n");
	    valid_expression = False;
	}
    }

    if (valid_expression)
    {
        XdbDebug(__FILE__, widget, "  Valid expression.  Button is %d\n", button);
	
	*value = (XtArgVal)menuPost;
	/* does this one ever change? */
	RC_PostEventType(widget) = ButtonPressMask;
	RC_PostButton(widget) = button;
	RC_PostModifiers(widget) = 0; /* FIX ME */
	
	return XmSYNTHETIC_LOAD;
    }
    else
        return XmSYNTHETIC_NONE;
}

static void 
_XmFromMenuPost(Widget widget, 
		int offset, 
		XtArgVal *value)
{
    /* This function assumes that the menuPost string is a button event.
       Not sure if this is in general the case.. FIX ME -- Chris */
    String return_value = (String)XtMalloc(sizeof(char)
					   * (strlen("<Btn>")
					      + 2  /* for the <>'s */
					      + 1  /* the the NULL */));

    XdbDebug(__FILE__, widget, "In _XmFromMenuPost()\n");

    sprintf(return_value, "<Btn%d>", RC_PostButton(widget));
    
    *value = (XtArgVal)return_value;
}

static void 
_XmPopupButtonPressHandler(Widget w, 
			   XtPointer client_data,
			   XEvent *event, 
			   Boolean *cont)
{
    Widget rc = (Widget) client_data;
    XmMenuState state = _XmGetMenuState(w);

    /* first we check if the event is valid with respect to our menupost resource*/
    if (event->xany.type != ButtonPress
	|| event->xbutton.button != RC_PostButton(rc)
	/* should also check the modifiers, but since we don't set 
	   them to a useful value... */)
        return;

    /* if we're already waiting to be managed, do nothing. */
    if (state->RC_ButtonEventStatus.waiting_to_be_managed)
        return;

    XdbDebug(__FILE__, rc, "  Filling in ButtonEventStatusRec\n");

    state->RC_ButtonEventStatus.waiting_to_be_managed = True;
    state->RC_ButtonEventStatus.event = event->xbutton;
    state->RC_ButtonEventStatus.time = event->xbutton.time;
    state->RC_ButtonEventStatus.verified = True; /* Is it really? FIX ME */
#if 0
    RC_PopupTimer(rc) = XtAppAddTimeOut(XtWidgetToApplicationContext(rc),
					3000 /* 3 seconds */,
					foo,
					(XtPointer) rc);
#endif
}

static void
DeleteChild(Widget w)
{
	Widget	rc = XtParent(w);
	int	i, j;

	XdbDebug2(__FILE__, rc, w, "DeleteChild\n");

	if (! XtIsRectObj(w))
	    return;	/* Why is this here ?? */

/* Boxes */
	for (i=0; i<MGR_NumChildren(rc); i++)
	     if (w == MGR_Children(rc)[i])
		break;

	if (i < MGR_NumChildren(rc)) {
	    for (j=i; j<MGR_NumChildren(rc)-1; j++)
		RC_Boxes(rc)[j] = RC_Boxes(rc)[j+1];
	}
	RC_Boxes(rc) = (XmRCKidGeometry) XtRealloc((XtPointer)RC_Boxes(rc),
			(MGR_NumChildren(rc) - 1) * sizeof(XmRCKidGeometryRec));

#define	superclass	(&xmManagerClassRec)
    (*superclass->composite_class.delete_child)(w);
#undef superclass
}

static Boolean
RcConstraintSetValues(Widget current, Widget request, Widget new, ArgList args, Cardinal *num_args)
{
	XdbDebug(__FILE__, new, "RcConstraintSetValues\n");

	return False;
}

#if 0
/* This isn't actually used anywhere. */
static void
ManagerGadgetSelect(Widget w,
		    XEvent * event,
		    String * params,
		    Cardinal * num_params)
{
    XdbDebug(__FILE__, w, "ManagerGadgetSelect()\n");
}
#endif

#if 0
/* This isn't actually used anywhere. */
static void
MenuGadgetEscape(Widget w,
		 XEvent * event,
		 String * params,
		 Cardinal * num_params)
{
    XdbDebug(__FILE__, w, "MenuGadgetEscape()\n");
}
#endif
