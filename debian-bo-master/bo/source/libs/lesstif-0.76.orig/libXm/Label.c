/**
 *
 * $Id: Label.c,v 1.21 1996/12/18 00:45:19 miers Exp $
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

static char rcsid[] = "$Id: Label.c,v 1.21 1996/12/18 00:45:19 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/XmosP.h>
#include <Xm/AtomMgr.h>
#include <Xm/DragC.h>
#include <Xm/LabelP.h>
#include <Xm/LabelGP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/RowColumnP.h>
#include <Xm/TransltnsP.h>
#include <Xm/XmosP.h>	
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <assert.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_prehook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void resize(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_prehook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);

static void label_menu_procs(int function, Widget menushell_parent, ...);
Boolean _XmLabelShowsAccelerators(Widget w);
Boolean _XmLabelShowsMnemonic(Widget w);

static Boolean XmLabelGetBaselines(Widget w,  Dimension **baselines, int *nbaselines);
static Boolean XmLabelGetDisplayRect(Widget w, XRectangle *rect);

/* prototypes for drag-drop */

static Boolean drag_convert_proc(Widget w, Atom *selection, Atom *target, Atom *type_return, XtPointer *value_return, unsigned long *length_return, int *format_return);
static void drag_drop_finish(Widget w, XtPointer client_data, XtPointer call_data);

/*
 * Resources for the label class
 */
#define Offset(field) XtOffsetOf(XmLabelRec, label.field)
static XtResource resources[] = {
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmLabelRec, primitive.shadow_thickness),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNalignment, XmCAlignment, XmRAlignment,
	sizeof(unsigned char), Offset(alignment),
	XmRImmediate, (XtPointer)XmALIGNMENT_CENTER
    },
    {
	XmNlabelType, XmCLabelType, XmRLabelType,
	sizeof(unsigned char), Offset(label_type),
	XmRImmediate, (XtPointer)XmSTRING
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNmarginLeft, XmCMarginLeft, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_left),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginRight, XmCMarginRight, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_right),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginTop, XmCMarginTop, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_top),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginBottom, XmCMarginBottom, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_bottom),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(font),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNlabelPixmap, XmCLabelPixmap, XmRPrimForegroundPixmap,
	sizeof(Pixmap), Offset(pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNlabelInsensitivePixmap, XmCLabelInsensitivePixmap, XmRPrimForegroundPixmap,
	sizeof(Pixmap), Offset(pixmap_insen),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNlabelString, XmCXmString, XmRXmString,
	sizeof(XmString), Offset(_label),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNmnemonic, XmCMnemonic, XmRKeySym,
	sizeof(KeySym), Offset(mnemonic),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNmnemonicCharSet, XmCMnemonicCharSet, XmRString,
	sizeof(String), Offset(mnemonicCharset),
	XmRImmediate, (XtPointer)XmFONTLIST_DEFAULT_TAG
    },
    {
	XmNaccelerator, XmCAccelerator, XmRString,
	sizeof(String), Offset(accelerator),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNacceleratorText, XmCAcceleratorText, XmRXmString,
	sizeof(XmString), Offset(_acc_text),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNrecomputeSize, XmCRecomputeSize, XmRBoolean,
	sizeof(Boolean), Offset(recompute_size),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNstringDirection, XmCStringDirection, XmRStringDirection,
	sizeof(XmStringDirection), Offset(string_direction),
	XmRImmediate, (XtPointer)((XmStringDirection)XmUNSPECIFIED)
    },
    /* resources we override from XmPrimitive */
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmLabelRec, primitive.traversal_on),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmLabelRec, primitive.highlight_thickness),
	XmRImmediate, (XtPointer)0
    }
};

/* Synthetic Resources for the Label Widget */

static XmSyntheticResource syn_resources[] = {
    {
	XmNmarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNmarginLeft,
	sizeof(Dimension), Offset(margin_left),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginRight,
	sizeof(Dimension), Offset(margin_right),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginTop,
	sizeof(Dimension), Offset(margin_top),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNmarginBottom,
	sizeof(Dimension), Offset(margin_bottom),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNlabelString,
	sizeof(XmString), Offset(_label),
	_XmExportLabelString, NULL
    },
    {
	XmNaccelerator,
	sizeof(String), Offset(accelerator),
	_XmExportString, NULL
    },
    {
	XmNacceleratorText,
	sizeof(XmString), Offset(_acc_text),
	_XmExportLabelString, NULL
    },
    {
	XmNmnemonicCharSet,
	sizeof(String), Offset(mnemonicCharset),
	_XmExportString, NULL
    }
};

static void ProcessDrag(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Help(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void LabelFocusIn(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void LabelFocusOut(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void LabelUnmap(Widget w, XEvent *event, String *params, Cardinal *num_params);

char _XmLabel_defaultTranslations[] = 
   "<EnterWindow>:        Enter()\n\
    <LeaveWindow>:        Leave()\n\
    <Btn2Down>:           ProcessDrag()\n\
    <Key>osfActivate:     PrimitiveParentActivate()\n\
    <Key>osfCancel:       PrimitiveParentCancel()\n\
    <Key>osfHelp:         Help()\n\
    ~s ~m ~a <Key>Return: PrimitiveParentActivate()";

char _XmLabel_menuTranslations[] =
    "<EnterWindow>: Enter() \n\
     <LeaveWindow>: Leave() \n\
     :<Key>osfHelp: Help()";

static XtTranslations default_trans = NULL;
static XtTranslations menu_trans = NULL;

char _XmLabel_menu_traversal_events[] =  /* fix me -- add these translations to something -- chris*/
   "<Unmap>:              Unmap()\n\
    <FocusOut>:           FocusOut()\n\
    <FocusIn>:            FocusIn()\n\
    <Key>osfCancel:       MenuEscape()\n\
    <Key>osfLeft:         MenuTraverseLeft()\n\
    <Key>osfRight:        MenuTraverseRight()\n\
    <Key>osfUp:           MenuTraverseUp()\n\
    <Key>osfDown:         MenuTraverseDown()";

static XtActionsRec actions[] = {
    {"Enter", _XmPrimitiveEnter}, /* are these two right ? */
    {"Leave", _XmPrimitiveLeave}, 
    {"ProcessDrag", ProcessDrag},
    {"Help", Help},
    {"MenuTraverseLeft", _XmMenuTraverseLeft},
    {"MenuTraverseRight", _XmMenuTraverseRight},
    {"MenuTraverseUp", _XmMenuTraverseUp},
    {"MenuTraverseDown", _XmMenuTraverseDown},
    {"MenuEscape", _XmMenuEscape},
    {"FocusIn", LabelFocusIn},
    {"FocusOut", LabelFocusOut},
    {"Unmap", LabelUnmap},
};

static XmBaseClassExtRec _XmLabelCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ NULL, /* FIXME */
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ NULL, /* FIXME */
    /* secondary_object_class    */ NULL, /* FIXME */
    /* secondary_object_create   */ NULL, /* FIXME */
    /* get_secondary_resources   */ NULL, /* FIXME */
    /* fast_subclass             */ { 0 }, /* FIXME */
    /* get_values_prehook        */ NULL, /* FIXME */
    /* get_values_posthook       */ NULL, /* FIXME */
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ FALSE,
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

XmPrimitiveClassExtRec _XmLabelPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ XmLabelGetBaselines,
    /* widget_display_rect */ XmLabelGetDisplayRect,
    /* widget_margins      */ NULL
};

XmLabelClassRec xmLabelClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPrimitiveClassRec,
        /* class_name            */ "XmLabel",
	/* widget_size           */ sizeof(XmLabelRec),
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
	/* tm_table              */ NULL,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmLabelCoreClassExtRec
    },
    /* Primitive Class part */
    {
        /* border_highlight      */ XmInheritBorderHighlight,
        /* border_unhighlight    */ XmInheritBorderUnhighlight,
        /* translations          */ XtInheritTranslations,
        /* arm_and_activate_proc */ XmInheritArmAndActivate,
        /* Synthetic Resources   */ syn_resources,
        /* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)&_XmLabelPrimClassExtRec
    },
    /* Label Class part */
    {
        /* setOverrideCallback */ NULL,
        /* menuProcs           */ label_menu_procs,
        /* translations        */ _XmLabel_menu_traversal_events,
	/* extension           */ NULL
    }
};

WidgetClass xmLabelWidgetClass = (WidgetClass)&xmLabelClassRec;


extern XmFontList _XmFontListCreateDefault(Display *);

static void
class_initialize()
{
    menu_trans = XtParseTranslationTable(_XmLabel_menuTranslations);
    default_trans = XtParseTranslationTable(_XmLabel_defaultTranslations);

    _XmLabelCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmLabelWidgetClass lwc = (XmLabelWidgetClass)widget_class;
    XmLabelWidgetClass swc = (XmLabelWidgetClass)(widget_class->core_class.superclass);
    XmPrimitiveClassExt ext, *extptr, *sextptr;

    /* Handle label class part inheritance */

    if (lwc->label_class.menuProcs == XmInheritMenuProc)
	lwc->label_class.menuProcs =
		swc->label_class.menuProcs;
    if (lwc->label_class.setOverrideCallback == XmInheritSetOverrideCallback)
	lwc->label_class.setOverrideCallback = False; /* FIX ME */
    if (lwc->label_class.translations == XtInheritTranslations)
	lwc->label_class.translations =
		swc->label_class.translations;

    extptr = (XmPrimitiveClassExt*)_XmGetClassExtensionPtr(
		(XmGenericClassExt*)&(lwc->primitive_class.extension),
		NULLQUARK);
    sextptr = (XmPrimitiveClassExt*)_XmGetClassExtensionPtr(
		(XmGenericClassExt*)&(swc->primitive_class.extension),
		NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (XmPrimitiveClassExt) XtNew(XmPrimitiveClassExtRec);
	if (ext != NULL)
	{
	    ext->next_extension = lwc->primitive_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XmPrimitiveClassExtVersion;
	    ext->record_size = sizeof(XmPrimitiveClassExtRec);
	    lwc->primitive_class.extension = (XtPointer) ext;
	}
    }
    else
	ext = *extptr;

    if (sextptr && *sextptr) {
	if (ext->widget_baseline == XmInheritBaselineProc)
	    ext->widget_baseline = (*sextptr)->widget_baseline;
	if (ext->widget_display_rect == XmInheritDisplayRectProc)
	    ext->widget_display_rect = (*sextptr)->widget_display_rect;
	if (ext->widget_margins == XmInheritMarginsProc)
	    ext->widget_margins = (*sextptr)->widget_margins;
    }

    _XmFastSubclassInit(widget_class, XmLABEL_BIT);
}

static void
CreateNormalGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
		GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillSolid;

    Lab_NormalGC(w) = XtGetGC(w, mask, &values);
}

static void 
CreateInsensitiveGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction | GCStipple |
		GCPlaneMask | GCSubwindowMode | GCGraphicsExposures |
		GCTileStipXOrigin | GCTileStipYOrigin;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillStippled;
    values.ts_x_origin = values.ts_y_origin = 0;
    if ((Lab_TextRect_x(w) & 1) ^ (Lab_TextRect_y(w) & 1))
	values.stipple = XmGetPixmapByDepth(XtScreen(w),
					    XmODD_STIPPLE_IMAGE,
					    WhitePixelOfScreen(XtScreen(w)),
					    BlackPixelOfScreen(XtScreen(w)),
					    1);
    else
	values.stipple = XmGetPixmapByDepth(XtScreen(w),
					    XmEVEN_STIPPLE_IMAGE,
					    WhitePixelOfScreen(XtScreen(w)),
					    BlackPixelOfScreen(XtScreen(w)),
					    1);

    Lab_InsensitiveGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize_prehook(Widget request,
		   Widget new_w,
		   ArgList args,
		   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "Initialize Prehook\n");

    _XmSaveCoreClassTranslations(new_w);

    if (XmIsRowColumn(XtParent(new_w)) &&
	(RC_Type(XtParent(new_w)) == XmMENU_PULLDOWN ||
	 RC_Type(XtParent(new_w)) == XmMENU_POPUP))
	new_w->core.widget_class->core_class.tm_table = (String)menu_trans;
    else 
	new_w->core.widget_class->core_class.tm_table = (String)default_trans;
}

static void
initialize_posthook(Widget request,
		    Widget new_w,
		    ArgList args,
		    Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "Initialize Posthook\n");

    _XmRestoreCoreClassTranslations(new_w);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "Initialize\n");

    /* get the default fontlist if the label was created without one. */
    if (Lab_Font(new_w) == (XmFontList)XmUNSPECIFIED || Lab_Font(new_w) == NULL)
	Lab_Font(new_w) = _XmGetDefaultFontList(new_w, XmLABEL_FONTLIST);

    /* If the label was not initialized with the resource labelString set,
       use its name -- the following _XmString code comes from MegaButton */
    if (Lab_Label(new_w) == (_XmString)XmUNSPECIFIED 
	|| Lab_Label(new_w) == (_XmString)0) { /* Shouldn't be necessary but is */
	XmString xmstring;

	xmstring = _XmOSGetLocalizedString((char *) NULL,
					   (Widget)new_w,
					   XmNlabelString,
					   XtName(new_w));

	Lab_Label(new_w) = _XmStringCreate(xmstring);
    }

    if (_XmStringIsXmString((XmString)Lab_Label(new_w)))
	Lab_Label(new_w) = _XmStringCreate((XmString)Lab_Label(new_w));

    if (Lab_AcceleratorText(new_w) != NULL)
	Lab_AcceleratorText(new_w) = _XmStringCreate((XmString)Lab_AcceleratorText(new_w));
    else
	Lab_AcceleratorText(new_w) = _XmStringCreate(XmStringCreateSimple(""));

    /*
     * have to check request since new may have been polluted by a
     * superclass 
     */
    if (XtWidth(request) == (Dimension)0 || XtHeight(request) == (Dimension)0) {

	XtWidth(new_w) = 0;
	XtHeight(new_w) = 0;
	_XmCalcLabelDimensions(new_w);

	resize(new_w);
    }

    /* allocate the normal and insensitive GC's */
    CreateNormalGC(new_w);
    CreateInsensitiveGC(new_w);

    /* if the parent is a row column, set the menu_type to
       it's type.  Otherwise, XmNONE  (Is this right?) FIX ME */

    if (XmIsRowColumn(XtParent(new_w)))
	Lab_MenuType(new_w) = RC_Type(XtParent(new_w));
    else
	Lab_MenuType(new_w) = XmNONE;

    /* Force the traversal and highlight on enter resources if
       in an popup, pulldown, and option menus. */

    if (Lab_MenuType(new_w) == XmMENU_POPUP 
	|| Lab_MenuType(new_w) == XmMENU_PULLDOWN
	|| Lab_MenuType(new_w) == XmMENU_OPTION)
    {
	Prim_TraversalOn(new_w) = False;
	Prim_HighlightOnEnter(new_w) = False;
    }
    Lab_SkipCallback(new_w) = False;

    if (Lab_MnemonicCharset(new_w) != NULL)
	Lab_MnemonicCharset(new_w) = XtNewString(Lab_MnemonicCharset(new_w));
    else
	Lab_MnemonicCharset(new_w) = XtNewString("");

    if (_XmStringIsXmString((XmString)Lab_Accelerator(new_w)))
	XmStringGetLtoR((XmString)Lab_Accelerator(new_w),
			XmFONTLIST_DEFAULT_TAG,
			&Lab_Accelerator(new_w));

    if (Lab_Accelerator(new_w)) {
	Lab_Accelerator(new_w) = XtNewString(Lab_Accelerator(new_w));
	_XmManagerInstallAccelerator(XtParent(new_w), new_w, Lab_Accelerator(new_w));
    }
    if (Lab_Mnemonic(new_w))
      _XmManagerInstallMnemonic(XtParent(new_w), new_w, Lab_Mnemonic(new_w));
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, Lab_NormalGC(w));
    XtReleaseGC(w, Lab_InsensitiveGC(w));
    _XmManagerUninstallAccelerator(XtParent(w), w);
    _XmManagerUninstallMnemonic(XtParent(w), w);
}


/* 
 * it is bad to call _XmCalcLabelDimensions inside SetValues
 * since it is possible the parent will reject the size
 * changes and _XmLabelRecomputeSize will have changed widget 
 * internals based on the size it thinks it will get.
 * If the changes are rejected, we are left with the changed
 * internals and the unchanged Width and Height.
 *
 * We could change the label widget to use set_values_almost, but
 * I don't see the need to do so.  In fact, I can't think of any
 * simple widgets that use set_values_almost anyway.  The internal 
 * positions are always calcuated in resize.
 *
 * Lab_RecomputeSize seems to be checked in OSF/Motif when either
 *	o the pixmap changes
 *	o the labelString changes
 * it is possible to change into the other...
 *
 * What happens if we are changing the label (possibly
 * to a pixmap) and the size change is rejected or the new size evaluates 
 * to the same value as the old size?  Resize won't be called.  We need to 
 * do something so expose doesn't use the old internal values.  
 *
 * Just recompute the label internals in expose, by calling resize()
 * directly. :)
 *
 * Notes:
 *
 *    if Lab_RecomputeSize is set, the widget wants to resize to fit
 *    changes to the labelString or pixmap
 * 051496 -- There are surely missing cases in here. MLM
 */
static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean refresh_needed = False,
	    relayout_needed = False;

    XdbDebug(__FILE__, new_w, "set_values()\n");

    /* This is a Primitive resource but we have the GC's for it... */
    if (Prim_Foreground(new_w) != Prim_Foreground(old) ||
	XtBackground(new_w) != XtBackground(old)) {
	XtReleaseGC(new_w, Lab_NormalGC(new_w));
	XtReleaseGC(new_w, Lab_InsensitiveGC(new_w));
	CreateNormalGC(new_w);
	CreateInsensitiveGC(new_w);
	refresh_needed = True;
    }

    if (Lab_AcceleratorText(new_w) != Lab_AcceleratorText(old)) {
	Lab_AcceleratorText(new_w) = _XmStringCreate((XmString)Lab_AcceleratorText(new_w));
	_XmStringFree(Lab_AcceleratorText(old));
	refresh_needed = True;
    }
    if (Lab_MnemonicCharset(new_w) != Lab_MnemonicCharset(old)) {
	Lab_MnemonicCharset(new_w) = XtNewString(Lab_MnemonicCharset(new_w));
	XtFree(Lab_MnemonicCharset(old));
	refresh_needed = True;
    }

    if (Lab_Label(new_w) == NULL) {
	Lab_Label(new_w) = _XmStringCreate(XmStringCreateSimple(XtName(new_w)));
	relayout_needed = True;
    }
    else if (Lab_Label(new_w) != Lab_Label(old)) {
	if (_XmStringIsXmString((XmString)Lab_Label(new_w)))
	    Lab_Label(new_w) = _XmStringCreate((XmString)Lab_Label(new_w));

	if (Lab_Label(old))
	    _XmStringFree(Lab_Label(old));

	relayout_needed = True;
    }

    if (Lab_Alignment(new_w) != Lab_Alignment(old))
	refresh_needed = True;

    if (Lab_Font(new_w) != Lab_Font(old)
	|| Lab_MarginTop(new_w) != Lab_MarginTop(old)
	|| Lab_MarginBottom(new_w) != Lab_MarginBottom(old)
	|| Lab_MarginLeft(new_w) != Lab_MarginLeft(old)
	|| Lab_MarginRight(new_w) != Lab_MarginRight(old)
	|| Lab_MarginWidth(new_w) != Lab_MarginWidth(old)
	|| Lab_MarginHeight(new_w) != Lab_MarginHeight(old)
	|| Lab_Mnemonic(new_w) != Lab_Mnemonic(old)
	|| Lab_StringDirection(new_w) != Lab_StringDirection(old))
    {
	relayout_needed = True;
    }

    /* check for change in insensitive pixmap */
    if ((Lab_PixmapInsensitive(new_w) != Lab_PixmapInsensitive(old)) 
	&& !XtSensitive(new_w) && Lab_IsPixmap(new_w))
    {
	relayout_needed = True;    
    }

    /* check for change in pixmap */
    if (Lab_Pixmap(new_w) != Lab_Pixmap(old)) 
    {
	/* if changed pixmap to UNSPECIFIED, automatically configure to a
	 * string
	 */
	if (Lab_IsPixmap(new_w)
	    && Lab_Pixmap(new_w) == (Pixmap)XmUNSPECIFIED_PIXMAP)
	{
	    Lab_LabelType(new_w) = XmSTRING;
	}

	relayout_needed = True;
    }

    /* did the label change types? */
    if (Lab_LabelType(new_w) != Lab_LabelType(old)) 
    {
	relayout_needed = True;
    }

    if (Lab_Accelerator(new_w) != Lab_Accelerator(old)) {

	if (_XmStringIsXmString((XmString)Lab_Accelerator(new_w)))
	    XmStringGetLtoR((XmString)Lab_Accelerator(new_w),
			     XmFONTLIST_DEFAULT_TAG,
			     &Lab_Accelerator(new_w));
	else if (Lab_Accelerator(new_w))
	    Lab_Accelerator(new_w) = XtNewString(Lab_Accelerator(new_w));
	_XmManagerUninstallAccelerator(XtParent(new_w), new_w);
	_XmManagerInstallAccelerator(XtParent(new_w), new_w, Lab_Accelerator(new_w));
        refresh_needed = True;
    }
    if (Lab_Mnemonic(new_w) != Lab_Mnemonic(old)) {
	_XmManagerUninstallMnemonic(XtParent(new_w), new_w);
	_XmManagerInstallMnemonic(XtParent(new_w), new_w, Lab_Mnemonic(new_w));
        refresh_needed = True;
    }

    if (XtWidth(new_w) == 0 || XtHeight(new_w) == 0)
	relayout_needed = True;

    if (relayout_needed) {

	if (Lab_RecomputeSize(new_w) && XtWidth(new_w) == XtWidth(old))
	    XtWidth(new_w) = 0;
	if (Lab_RecomputeSize(new_w) && XtHeight(new_w) == XtHeight(old))
	    XtHeight(new_w) = 0;

	_XmCalcLabelDimensions(new_w);

	resize(new_w);

	refresh_needed = True;
    }

    return refresh_needed;
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    XRectangle cliprect;
    int width, height, showAcc;
    GC myGC;

    XdbDebug(__FILE__, w, "Expose\n");
/*
 * I'm becoming paranoid - Danny - see testXm/filesb/test3
 */
    if (! XtIsRealized(w))
	return;
    /* 
     * recompute the labels internals... they could be off if resize
     * hadn't been called after a change (see set_values)
     */
    resize(w);

    /* use the right GC */
    if (XtSensitive(w))
        myGC = Lab_NormalGC(w);
    else
        myGC = Lab_InsensitiveGC(w);

    /* Set a clip rectangle for the GC - ensure we don't overwrite shadows */
    width = XtWidth(w);
    height = XtHeight(w);

    if(width <= 0)
	width = 1;
    if(height <= 0)
	height = 1;
    cliprect.x = Lab_MarginLeft(w) + Prim_HighlightThickness(w) +
			Prim_ShadowThickness(w);
    cliprect.y = Lab_MarginTop(w) + Prim_HighlightThickness(w) +
			Prim_ShadowThickness(w);
    cliprect.width = XtWidth(w) - (Lab_MarginRight(w) + cliprect.x +
			Prim_ShadowThickness(w) + Prim_HighlightThickness(w));
    cliprect.height = XtHeight(w) - (Lab_MarginBottom(w) + cliprect.y +
			Prim_ShadowThickness(w) + Prim_HighlightThickness(w));

    showAcc = _XmLabelShowsAccelerators(w);
    if (showAcc) {
	cliprect.width = XtWidth(w) - (Lab_MarginRight(w) + cliprect.x +
			Prim_ShadowThickness(w) + Prim_HighlightThickness(w));
	cliprect.height = XtHeight(w) - (Lab_MarginBottom(w) + cliprect.y +
			Prim_ShadowThickness(w) + Prim_HighlightThickness(w));
    }

    XdbDebug(__FILE__, w, "Expose: cliprect %d %d %dx%d\n",
		cliprect.x, cliprect.y, cliprect.width, cliprect.height);

    XSetClipRectangles(XtDisplay(w), myGC, 0, 0, &cliprect, 1, Unsorted);

    if (Lab_IsText(w))
    { /* LabelString */
	if (_XmLabelShowsMnemonic(w) && Lab_Mnemonic(w)) {
	    char	m[2];

	    m[0] = Lab_Mnemonic(w);
	    m[1] = '\0';

	    _XmStringDrawMnemonic(XtDisplay(w), XtWindow(w),
		Lab_Font(w), Lab_Label(w), myGC,
		Lab_TextRect_x(w), Lab_TextRect_y(w), Lab_TextRect_width(w),
		Lab_Alignment(w),
		0,
		NULL,
		m, Lab_MnemonicCharset(w));
	}
	else {
	    _XmStringDraw(XtDisplay(w),
		     XtWindow(w),
		     Lab_Font(w),
		     Lab_Label(w),
		     myGC,
		     Lab_TextRect_x(w),
		     Lab_TextRect_y(w),
		     Lab_TextRect_width(w),
		     Lab_Alignment(w),
		     0,
		     NULL);
	}
	/* AcceleratorText */
	if (showAcc) {
		_XmStringDraw(XtDisplay(w),
		     XtWindow(w),
		     Lab_Font(w),
		     Lab_AcceleratorText(w),
		     myGC,
		     Lab_AccTextRect(w).x,
		     Lab_AccTextRect(w).y,
		     Lab_AccTextRect(w).width,
		     XmALIGNMENT_BEGINNING,
		     0,
		     NULL);
	}
    } 
    else if (XtSensitive(w) &&
	     Lab_Pixmap(w) != XmUNSPECIFIED_PIXMAP)
    {
	XCopyArea(XtDisplay(w),
		  Lab_Pixmap(w),
		  XtWindow(w),
		  myGC,
		  0,
		  0,
		  Lab_TextRect_width(w),
		  Lab_TextRect_height(w), 
		  Lab_TextRect_x(w), Lab_TextRect_y(w));
    }
    else if (!XtSensitive(w) &&
	     Lab_PixmapInsensitive(w) != XmUNSPECIFIED_PIXMAP)
    {
	XCopyArea(XtDisplay(w),
		  Lab_PixmapInsensitive(w),
		  XtWindow(w),
		  myGC,
		  0,
		  0,
		  Lab_TextRect_width(w),
		  Lab_TextRect_height(w), 
		  Lab_TextRect_x(w), Lab_TextRect_y(w));
    }

    XSetClipMask(XtDisplay(w), myGC, None);
}

static void
resize(Widget w)
{
    Dimension width, height;
    Dimension pix_width, pix_height;
    Boolean	showAcc;
    unsigned char beforex, beforey, afterx, aftery;

    if (!XmIsLabel(w))
	return;

    beforex = Lab_TextRect_x(w) & 1;
    beforey = Lab_TextRect_y(w) & 1;

    showAcc = _XmLabelShowsAccelerators(w);

    /* set the label's size so the pixmap/string fits */
    if (Lab_IsText(w))
	_XmStringExtent(Lab_Font(w),
		        Lab_Label(w),
		        &width,
		        &height);
    else /* Lab_IsPixmap(w) */
    {
	_XmLabelGetPixmapSize(w, Lab_Pixmap(w), &pix_width, &pix_height);	
	width = pix_width;
	height = pix_height;

	XdbDebug(__FILE__, w, "_XmLabelGetPixmapSize: w = %d, h = %d\n", width, height);
    }
	
    Lab_TextRect_width(w) = width;
    Lab_TextRect_height(w) = height;

	    
    /* The alignments only modify x values if a size was set */
    _XmLabelAccTextSize(w);

    switch(Lab_Alignment(w))
    {
    case XmALIGNMENT_END:
	Lab_TextRect_x(w) = (XtWidth(w)
				 - Lab_Highlight(w)
				 - Lab_Shadow(w)
				 - Lab_MarginWidth(w)
				 - Lab_MarginRight(w)
				 - Lab_TextRect_width(w));
	break;
    case XmALIGNMENT_BEGINNING:
	/* Default X Position of TextRect */
	Lab_TextRect_x(w) = (Lab_Highlight(w)
				 + Lab_Shadow(w)
				 + Lab_MarginWidth(w)
				 + Lab_MarginLeft(w));
	break;
    case XmALIGNMENT_CENTER:
    default:
	Lab_TextRect_x(w) = (XtWidth(w) -
				(Lab_MarginLeft(w) +
				 Lab_MarginRight(w)) -
				width) / 2 + Lab_MarginLeft(w);
	break;
    }
    Lab_TextRect_y(w) = (XtHeight(w) -
				(Lab_MarginTop(w) +
				 Lab_MarginBottom(w)) -
				height) / 2 +
				Lab_MarginTop(w);

    if (showAcc) {
	Lab_AccTextRect(w).x = XtWidth(w)
				- Lab_Shadow(w)
				- Lab_Highlight(w)
				- Lab_AccTextRect(w).width;
	Lab_AccTextRect(w).y = Lab_TextRect_y(w);

	XdbDebug(__FILE__, w, "Accelerator @ %d %d (x = %d - %d - %d - %d)\n",
		Lab_AccTextRect(w).x, Lab_AccTextRect(w).y,
		XtWidth(w), Lab_Shadow(w), Lab_Highlight(w),
		Lab_AccTextRect(w).width);
    }

    afterx = Lab_TextRect_x(w) & 1;
    aftery = Lab_TextRect_y(w) & 1;
    if (beforex ^ afterx || beforey ^ aftery) {
	XtReleaseGC(w, Lab_InsensitiveGC(w));
	CreateInsensitiveGC(w);
    }
}

/*
 * ask how we want to look
 */
#define	Wants(x)	(proposed->request_mode & x)

static XtGeometryResult 
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    Dimension ww, hh;

    XdbDebug(__FILE__, w, "query_geometry\n");

    answer->request_mode = CWWidth | CWHeight;

    ww = XtWidth(w);
    hh = XtHeight(w);
    XtWidth(w) = 0;
    XtHeight(w) = 0;

    _XmCalcLabelDimensions(w);

    answer->width = XtWidth(w);
    answer->height = XtHeight(w);

    XtWidth(w) = ww;
    XtHeight(w) = hh;

    if ((proposed->request_mode & (CWWidth | CWHeight)) == (CWWidth | CWHeight) &&
	proposed->width == answer->width && proposed->height == answer->height) 
	return XtGeometryYes;
    else if (answer->width == XtWidth(w) && answer->height == XtHeight(w))
	return XtGeometryNo;
    else 
	return XtGeometryAlmost;
}

static void
LabelFocusIn(Widget w,
	     XEvent *event,
	     String *params,
	     Cardinal *num_params) 
{
    XdbDebug(__FILE__, w, "LabelFocusIn()\n");

    if (Lab_MenuType(w) == XmMENU_BAR
	|| Lab_MenuType(w) == XmMENU_PULLDOWN
	|| Lab_MenuType(w) == XmMENU_POPUP) {

	XtCallActionProc(w, "MenuFocusIn", event, params, *num_params);
    }
}

static void 
LabelFocusOut(Widget w,
	      XEvent *event,
	      String *params,
	      Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "LabelFocusOut()\n");
}

static void 
LabelUnmap(Widget w,
	      XEvent *event,
	      String *params,
	      Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "LabelUnmap()\n");
}

static Boolean
drag_convert_proc(Widget w, 
		  Atom *selection, 
		  Atom *target, 
		  Atom *type_return, 
		  XtPointer *value_return, 
		  unsigned long *length_return, 
		  int *format_return)
{
    Atom COMPOUND_TEXT;
    Atom MOTIF_DROP;

    COMPOUND_TEXT = XmInternAtom(XtDisplay(w), "COMPOUND_TEXT", False);
    MOTIF_DROP = XmInternAtom(XtDisplay(w), "_MOTIF_DROP", False);

    if (*selection != MOTIF_DROP)
	return False;

    XdbDebug(__FILE__, w, "We're dealing with a motif drop\n");
    return False;
}

static void 
drag_drop_finish(Widget w, 
		 XtPointer client_data, 
		 XtPointer call_data)
{
    Widget source_icon = NULL;

    XtVaGetValues(w, XmNsourceCursorIcon, &source_icon, NULL);

    if (source_icon)
	XtDestroyWidget(source_icon);
}

static void 
ProcessDrag(Widget w, 
	    XEvent *event, 
	    String *params, 
	    Cardinal *num_params)
{
    Atom export_target;
    Arg args[10];
    int n = 0;
    Widget dc;

    XdbDebug(__FILE__, w, "Processing a drag-drop request\n");

    if (Lab_IsPixmap(w))
        export_target = XmInternAtom(XtDisplay(w),
                                     "PIXMAP",
                                     False);
    else 
        export_target = XmInternAtom(XtDisplay(w),
                                     "COMPOUND_TEXT", 
                                     False);

    XtSetArg(args[n], XmNexportTargets, &export_target); n++;
    XtSetArg(args[n], XmNnumExportTargets, 1); n++;
    XtSetArg(args[n], XmNdragOperations, XmDROP_COPY); n++;
    XtSetArg(args[n], XmNconvertProc, drag_convert_proc); n++;
    XtSetArg(args[n], XmNclientData, w); n++;

    dc = XmDragStart(w, event, args, n);
    XtAddCallback (dc, XmNdragDropFinishCallback, drag_drop_finish, NULL);
}

static void 
Help(Widget w, 
     XEvent *event, 
     String *params, 
     Cardinal *num_params)
{
    XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
}

void
_XmExportLabelString(Widget w, int offset, XtArgVal *value)
{
    _XmString str;
    XmString ret;

    str = *(_XmString *)(((char *)w)+offset);
    if (str)
	ret = _XmStringCreateExternal(Lab_Font(w), str);
    else
	ret = NULL;
    *value = (XtArgVal)ret;
}

void 
_XmLabelGetPixmapSize(Widget w,
		      Pixmap Pix,
		      Dimension *width,
		      Dimension *height)
{
    unsigned int tmp, tmpw, tmph;
    int tmpx, tmpy;
    unsigned Depth;
    Window tmpwin;

    if (Pix == XmUNSPECIFIED_PIXMAP) {
       *width = *height = 10;
       return;
    }

    XGetGeometry(XtDisplayOfObject(w),
		 Pix,
		 &tmpwin,
                 &tmpx, &tmpy,
                 &tmpw, &tmph,
		 &tmp,&Depth);

    *width = (Dimension)tmpw;
    *height = (Dimension)tmph;
}

static Boolean
XmLabelGetBaselines(Widget w, Dimension **baseelines, int *nbaselines)
{
    return False;
}

static Boolean
XmLabelGetDisplayRect(Widget w, XRectangle *rect)
{
    rect->x = Lab_TextRect_x(w);
    rect->y = Lab_TextRect_y(w);
    rect->width = Lab_TextRect_width(w);
    rect->height = Lab_TextRect_height(w);
    return True;
}

Boolean
_XmLabelShowsMnemonic(Widget w)
{
	if (! XtIsSubclass(XtParent(w), xmRowColumnWidgetClass))
		return False;
	if (RC_Type(XtParent(w)) != XmMENU_PULLDOWN && RC_Type(XtParent(w)) != XmMENU_BAR)
		return False;
	return True;
}

Boolean
_XmLabelShowsAccelerators(Widget w)
{
	if (! XtIsSubclass(XtParent(w), xmRowColumnWidgetClass))
		return False;
	if (RC_Type(XtParent(w)) != XmMENU_PULLDOWN)
		return False;
	if (XmIsLabel(w)) {
		if (Lab_IsPixmap(w) || Lab_AcceleratorText(w) == NULL)
			return False;
	} else if (XmIsLabelGadget(w)) {
		if (LabG_IsPixmap(w) || LabG_AcceleratorText(w) == NULL)
			return False;
	} else
		return False;
	return True;
}

void
_XmLabelAccTextSize(Widget w)
{
	Dimension	width, height;

	if (XmIsLabel(w)) {
		Lab_AccTextRect(w).width = Lab_AccTextRect(w).height = 0;

		if (! _XmLabelShowsAccelerators(w))
			return;

		_XmStringExtent(Lab_Font(w), Lab_AcceleratorText(w), &width, &height);

		Lab_AccTextRect(w).height = height;
		Lab_AccTextRect(w).width = width + LABEL_ACC_PAD;
#if 0
		if (Lab_MarginRight(w) < Lab_AccTextRect(w).width) {
			Lab_MarginRight(w) = Lab_AccTextRect(w).width;
			XdbDebug(__FILE__, w, "_XmLabelAccTextSize: set right margin to %d\n",
				Lab_MarginRight(w));
		}
#endif
	} else if (XmIsLabelGadget(w)) {
		LabG_AccTextRect(w).width = LabG_AccTextRect(w).height = 0;

		if (! _XmLabelShowsAccelerators(w))
			return;

		_XmStringExtent(LabG_Font(w), LabG_AcceleratorText(w), &width, &height);

		LabG_AccTextRect(w).height = height;
		LabG_AccTextRect(w).width = width + LABELG_ACC_PAD;
#if 0
		if (LabG_MarginRight(w) < LabG_AccTextRect(w).width) {
			LabG_MarginRight(w) = LabG_AccTextRect(w).width;
			XdbDebug(__FILE__, w, "_XmLabelAccTextSize: set right margin to %d\n",
				LabG_MarginRight(w));
		}
#endif
	}
	return;
}

/*
 * private label functions 
 * converted to work for both widgets and gadgets -- CT
 *
 * --- JRA
 * this should NEVER be called with XtWidth or XtHeight == 0
 * 
 * this touches the internal dimensions of the widget, it should
 * NOT be called from inside a SetValues call as a result of a 
 * size change since the size may be REJECTED by the PARENT
 *
 * MLM: Hmmm.  The Motif version of this function appears to be
 * always called with XtWidth/XtHeight = 0.  Also, I don't think
 * the statement about the size rejection is entirely true (I've
 * run into a situation with XmPushB where it is essential that
 * the size be changed in the set_values method).  I believe if
 * the parent doesn't like the change, it will reject it -- AND
 * call the child's resize procedure -- FIXME.
 *
 * FIX ME
 *
 * This thing is to be changed for PushButtons (including gadgets)
 * when they're in a menu, in the case that XmNacceleratorText
 * is non-NULL.
 */
void 
_XmCalcLabelDimensions(Widget w)
{
    Dimension width, height;

    if (Lab_IsText(w))
    {
	_XmStringExtent(Lab_Font(w),
		        Lab_Label(w),
		        &width,
		        &height);
    }
    else	/* pixmap */
    {
	_XmLabelGetPixmapSize(w, Lab_Pixmap(w), &width, &height);
    }

    width = (Lab_Highlight(w)
	   + Lab_Shadow(w)
	   + Lab_MarginLeft(w)
	   + Lab_MarginWidth(w)
	   + width
	   + Lab_MarginWidth(w)
	   + Lab_MarginRight(w)
	   + Lab_Shadow(w)
	   + Lab_Highlight(w));

    height = (Lab_Highlight(w)
	   + Lab_Shadow(w)
	   + Lab_MarginTop(w)
	   + Lab_MarginHeight(w)
	   + height
	   + Lab_MarginHeight(w)
	   + Lab_MarginBottom(w)
	   + Lab_Shadow(w)
	   + Lab_Highlight(w));

/* Do something about menus... */
    if (_XmLabelShowsAccelerators(w)) {
	Dimension	ww, hh;

	_XmStringExtent(Lab_Font(w), Lab_AcceleratorText(w), &ww, &hh);

	width += ww + LABEL_ACC_PAD;
	if (hh > height)
	   height = hh;
    }

    if (XtWidth(w) == 0)
	XtWidth(w) = width;

    if (XtHeight(w) == 0)
	XtHeight(w) = height;
}

static void 
#ifdef __STDC__
label_menu_procs(int function, Widget widget, ...)
{
    va_list arg_list;
    XtPointer foo;
    XEvent *event; 
    XtPointer returnData;

    va_start(arg_list, widget);

#else
label_menu_procs(function, widget, va_alist)
    int function;
    Widget widget;
    va_dcl
{
    va_list arglist;
    XtPointer foo;
    XEvent *event; 
    XtPointer returnData;

    va_start(arg_list);

#endif
    foo = va_arg(arg_list, XtPointer);
    event = va_arg(arg_list, XEvent *);
    returnData = va_arg(arg_list, XtPointer);

    XdbDebug(__FILE__, widget, "Label_Menu_Procs(%s, ...)\n", XdbMenuEnum2String(function));

    switch (function)
    {
    case XmMENU_BUTTON: 
	{
	    /* There was a button press in the menu.  
               Check to see if it was the valid button
	       for that type of menu */
	    XButtonEvent *xbe = (XButtonEvent*)event;

	    if (!XmIsRowColumn(widget)) {
		va_end(arg_list);
		return;
	    }

	    switch (RC_Type(widget))
	    {
	    case XmMENU_BAR:
	    case XmMENU_PULLDOWN:
	    case XmMENU_OPTION:
		*(Boolean*)returnData = (xbe->button == 1);
		break;
	    case XmMENU_POPUP:
		/* the third mouse button also works in popups */
		*(Boolean*)returnData = ((xbe->button == 1) || (xbe->button == 3));		
		break;
	    default:
		*(Boolean*)returnData = False; /* ? */
		break;
	    }
	    break;
	}
    case XmMENU_POPDOWN:
	break;
    case XmMENU_BUTTON_POPDOWN:
	{
	    Widget mb, shell = XtParent(XtParent(widget));

	    if (Lab_MenuType(widget) == XmMENU_POPUP)
		*(Boolean*)returnData = True;
	    else
		*(Boolean*)returnData = False;

	    XdbDebug2(__FILE__, shell, widget, "Label_Menu_Procs: => %s\n",
		(Lab_MenuType(widget) == XmMENU_POPUP) ? "True" : "False");

	    XdbDebug(__FILE__, widget, "Calling MenuShellPopdownDone\n");
	    XtCallActionProc(shell, "MenuShellPopdownDone", event, NULL, 0);

	    XdbDebug(__FILE__, widget, "XtUngrabPointer\n");
	    XtUngrabPointer(widget, CurrentTime);

	    for (mb = widget; mb; mb = XtParent(mb))
		if (XmIsRowColumn(mb) && RC_Type(mb) == XmMENU_BAR)
		    break;
	    if (mb) {
		XdbDebug(__FILE__, mb, "XtRemoveGrab\n");
		XtRemoveGrab(mb);
	    }
	    break;
	}
    case XmMENU_SHELL_POPDOWN:
	{
  	    Cardinal numparams = 0;

  	    /* widget must be a menu shell here. */
  	    assert(XmIsMenuShell(widget));

	    (*((XmMenuShellWidgetClass)XtClass(widget))->menu_shell_class.popdownEveryone)(widget,
											   event, 
											   NULL, 
											   &numparams);
	    break;
	}
	break;
	/*
	 * Uncaught cases
	 */
    case XmMENU_CALLBACK:
	_XmWarning(widget, "label_menu_procs: XmMENU_CALLBACK is not implemented !!\n");
	break;
    default:
	_XmWarning(widget, "label_menu_procs: function %d is not implemented !!\n", function);
	XdbDebug(__FILE__, widget, "label_menu_procs(%d) : uncaught case in switch\n", function);
	break;
    }
    va_end(arg_list);
}

Boolean
XmWidgetGetBaselines(Widget w,
		     Dimension **baselines,
		     int *line_count)
{
    if (XmIsLabel(w)) {
	XmPrimitiveClassExt *extptr;

	extptr = _XmGetPrimitiveClassExtPtr(XtClass(w), NULLQUARK);

	if (extptr && *extptr && (*extptr)->widget_baseline)
	    return ((*extptr)->widget_baseline)(w, baselines, line_count);
    }
    else if (XmIsLabelGadget(w)) {
	XmGadgetClassExt *extptr;

	extptr = _XmGetGadgetClassExtPtr(XtClass(w), NULLQUARK);

	if (extptr && *extptr && (*extptr)->widget_baseline)
	    return ((*extptr)->widget_baseline)(w, baselines, line_count);
    }

    return False;
}

Boolean
XmWidgetGetDisplayRect(Widget w,
		       XRectangle *displayrect)
{
    if (XmIsLabel(w)) {
	XmPrimitiveClassExt *extptr;

	extptr = _XmGetPrimitiveClassExtPtr(XtClass(w), NULLQUARK);

	if (extptr && *extptr && (*extptr)->widget_display_rect)
	    return ((*extptr)->widget_display_rect)(w, displayrect);
    }
    else if (XmIsLabelGadget(w)) {
	XmGadgetClassExt *extptr;

	extptr = _XmGetGadgetClassExtPtr(XtClass(w), NULLQUARK);

	if (extptr && *extptr && (*extptr)->widget_display_rect)
	    return ((*extptr)->widget_display_rect)(w, displayrect);
    }

    return False;
}

Widget
XmCreateLabel(Widget parent,
	      char *name,
	      Arg *arglist,
	      Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmLabelWidgetClass,
			  parent,
			  arglist,
			  argcount);
}
