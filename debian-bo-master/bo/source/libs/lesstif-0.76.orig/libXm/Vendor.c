/**
 *
 * $Id: Vendor.c,v 1.29 1997/01/07 02:35:44 miers Exp $
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

static char rcsid[] = "$Id: Vendor.c,v 1.29 1997/01/07 02:35:44 miers Exp $";

#include <stdio.h>
#include <stdlib.h>

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <X11/Vendor.h>
#include <X11/VendorP.h>
#include <X11/Xfuncs.h>
#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/BulletinBP.h>
#include <Xm/MenuShellP.h>
#include <Xm/VendorSEP.h>
#include <Xm/VendorSP.h>
#include <Xm/DisplayP.h>
#include <Xm/ScreenP.h>
#include <Xm/BaseClassP.h>
#include <Xm/Protocols.h>
#include <Xm/ProtocolsP.h>
#include <Xm/DialogSEP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/VendorS.h>

#include <XmI/DebugUtil.h>

#ifndef XmUNSPECIFIED
#define XmUNSPECIFIED (~0)
#endif

extern void _LesstifEditResCheckMessages(Widget w, XtPointer data, XEvent *event, Boolean *cont);

/***************************************************************************
 *
 * Vendor shell class record
 *
 ***************************************************************************/

static CompositeClassExtensionRec vendorCompositeExt = {
    /* next_extension */  NULL,
			  /* record_type    */  NULLQUARK,
			  /* version        */  XtCompositeExtensionVersion,
			  /* record_size    */  sizeof(CompositeClassExtensionRec),
			  /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
			  /* allows_change_managed_set */ False
#endif
};

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values(Widget old, Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes);
static void change_managed(Widget w);
static void resize(Widget w);
static void get_values_prehook(Widget widget, ArgList args, Cardinal *num_args);
static void get_values_posthook(Widget widget, ArgList args, Cardinal *num_args);
static void initialize_prehook(Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_prehook(Widget old, Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_posthook(Widget old, Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static void WmProtocolHandler(Widget w, XtPointer client, XtPointer call);
static Cardinal get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data);
static void insert_child(Widget w);
static void delete_child(Widget w);
static void secondary_object_create(Widget req, Widget new_w, ArgList args,
				    Cardinal *num_args);
/*
 * "forwards" needed within the vendor class implementation
 */
static void LTAddGrab(Widget wid, Boolean exclusive, Boolean spring_loaded, XmVendorShellExtObject ve, XmVendorShellExtObject grabber);
static void LTRemoveGrab(Widget wid, XmVendorShellExtObject ve, Boolean remove_grab_physically);
static void LTRemoveGrabCallback(Widget wid, XtPointer client_data, XtPointer callback_data);
static void LTShellPopupCallback(Widget w, XtPointer ClientData, XtPointer CallbackData);
static void LTShellPopdownCallback(Widget w, XtPointer ClientData, XtPointer CallbackData);

/*
 * the following is for the extension record for LessTif Vendor Shells
 * this is needed before the shell record as the shell's base class extension
 * needs some of the data defined here.
 */
static void _XmVendorExtInitialize(Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean _XmVendorExtSetValues(Widget cw, Widget rw, Widget nw, ArgList args, Cardinal *nargs);

#define Offset(field) XtOffsetOf(XmVendorShellExtRec, vendor.field)
static XtResource ext_resources[] = {
    {
	XmNextensionType, XmCExtensionType, XmRExtensionType,
	sizeof(unsigned char), XtOffsetOf(XmVendorShellExtRec, ext.extensionType),
	XtRImmediate, (XtPointer)XmSHELL_EXTENSION
    },
    {
	XmNdefaultFontList, XmCDefaultFontList, XmRFontList,
	sizeof(XmFontList), Offset(default_font_list),
	XmRImmediate, (XtPointer)NULL
    }, 
    {
	XmNbuttonFontList, XmCButtonFontList, XmRFontList,
	sizeof(XmFontList), Offset(button_font_list),
	XmRImmediate, (XtPointer)NULL
    }, 
    {
	XmNlabelFontList, XmCLabelFontList, XmRFontList,
	sizeof(XmFontList), Offset(label_font_list),
	XmRImmediate, (XtPointer)NULL
    }, 
    {
	XmNtextFontList, XmCTextFontList, XmRFontList,
	sizeof(XmFontList), Offset(text_font_list),
	XmRImmediate, NULL
    },
    {
	XmNaudibleWarning, XmCAudibleWarning, XmRAudibleWarning,
	sizeof(unsigned char), Offset(audible_warning),
	XmRImmediate, (XtPointer)XmBELL
    },
    {
	XmNshellUnitType, XmCShellUnitType, XmRUnitType,
	sizeof(unsigned char), Offset(unit_type),
	XmRImmediate, (XtPointer)XmPIXELS /* CHECK THIS */
    },
    {
	XmNdeleteResponse, XmCDeleteResponse, XmRDeleteResponse,
	sizeof(unsigned char), Offset(delete_response),
	XmRImmediate, (XtPointer)XmDESTROY
    },
    {
	XmNkeyboardFocusPolicy, XmCKeyboardFocusPolicy, XmRKeyboardFocusPolicy,
	sizeof(unsigned char), Offset(focus_policy),
	XmRImmediate, (XtPointer)XmEXPLICIT
    },
    {
	XmNmwmDecorations, XmCMwmDecorations, XmRInt,
	sizeof(int), Offset(mwm_hints.decorations),
	XmRImmediate, (XtPointer)-1
    },
    {
	XmNmwmFunctions, XmCMwmFunctions, XmRInt,
	sizeof(int), Offset(mwm_hints.functions),
	XmRImmediate, (XtPointer)-1
    },
    {
	XmNmwmInputMode, XmCMwmInputMode, XmRInt,
	sizeof(int), Offset(mwm_hints.input_mode),
	XmRImmediate, (XtPointer)-1
    },
    {
	XmNmwmMenu, XmCMwmMenu, XmRString,
	sizeof(String), Offset(mwm_menu),
	XmRImmediate, NULL
    },
    {
	XmNfocusMovedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(focus_moved_callback),
	XmRImmediate, NULL
    },
    {
	XmNrealizeCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(realize_callback),
	XmRImmediate, NULL
    },
    {
	XmNinputMethod, XmCInputMethod, XmRString,
	sizeof(String), Offset(input_method_string),
	XtRImmediate, (XtPointer)NULL /* FIXME */
    },
    {
	XmNpreeditType, XmCPreeditType, XmRString,
	sizeof(String), Offset(preedit_type_string),
	XtRImmediate, (XtPointer) "OffTheSpot,OverTheSpot,Root"
    },
    {
	XmNlightThreshold, XmCLightThreshold, XmRInt,
	sizeof(int), Offset(light_threshold),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNdarkThreshold, XmCDarkThreshold, XmRInt,
	sizeof(int), Offset(dark_threshold),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNforegroundThreshold, XmCForegroundThreshold, XmRInt,
	sizeof(int), Offset(foreground_threshold),
	XtRImmediate, (XtPointer)0
    },
};

static XmSyntheticResource ext_syn_resources[] = {
    {
	XmNx,
	sizeof(Position), XtOffsetOf(VendorShellRec, core.x),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNy,
	sizeof(Position), XtOffsetOf(VendorShellRec, core.y),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNwidth,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.width),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNheight,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.height),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNborderWidth,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.border_width),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNminWidth,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_width),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNminHeight,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_height),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNmaxWidth,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_width),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNmaxHeight,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_height),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNiconX,
	sizeof(Position), XtOffsetOf(VendorShellRec, wm.wm_hints.icon_x),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNiconY,
	sizeof(Position), XtOffsetOf(VendorShellRec, wm.wm_hints.icon_y),
	NULL /* FIXME */, NULL /* FIXME */
    },
#if 0
    {
	XmNmwmFunctions,
	sizeof(), XtOffsetOf(),
	NULL /* FIXME */, NULL /* FIXME */
    },
#endif
};

XmVendorShellExtClassRec xmVendorShellExtClassRec = {
    /* Object Class Part */
    {
 	/* superclass         */    (WidgetClass)&xmShellExtClassRec,
	/* class_name         */    "VendorShell",
	/* size               */    sizeof(XmVendorShellExtRec),
	/* class_initialize   */    NULL,
	/* class_part_initialize*/  NULL,
	/* Class init'ed ?    */    FALSE,
	/* initialize         */    NULL,
	/* initialize_hook    */    NULL,
	/* pad                */    NULL,
	/* pad                */    NULL,
	/* pad                */    0,
	/* resources          */    ext_resources,
	/* resource_count     */    XtNumber(ext_resources),
	/* xrm_class          */    NULLQUARK,
	/* pad                */    FALSE,
	/* pad                */    FALSE,
	/* pad                */    FALSE,
	/* pad                */    FALSE,
	/* destroy            */    NULL,
	/* pad                */    NULL,
	/* pad                */    NULL,
	/* set_values         */    NULL,
	/* set_values_hook    */    NULL,
	/* pad                */    NULL,
	/* get_values_hook    */    NULL,
	/* pad                */    NULL,
	/* version            */    XtVersion,
	/* callback_offsets   */    NULL,
	/* pad                */    NULL,
	/* pad                */    NULL,
	/* pad                */    NULL,
	/* extension          */    NULL
    },
    /* XmExtObject part */
    {
        /* syn_resources      */ ext_syn_resources,
        /* num_syn_resources  */ XtNumber(ext_syn_resources),
        /* extension          */ NULL
    },
    /* Desktop Class part */
    {
        /* child_class           */ NULL,
        /* insert_child          */ XmInheritWidgetProc,
        /* delete_child          */ XmInheritWidgetProc,
        /* extension             */ NULL
    },
    /* ShellExt Class part */
    {
	/* structure_notify_handler */ XmInheritEventHandler,
	/* extension                */ NULL
    },
    /* VendorClass Part */
    {
	/* delete_window_handler */ WmProtocolHandler,
	/* offset_handler        */ NULL, /* FIXME */
	/* extension             */ NULL
    }
};

WidgetClass xmVendorShellExtObjectClass = (WidgetClass) &xmVendorShellExtClassRec;

static XtResource vendor_resources[] = {
    {
	XmNx, XmCPosition, XmRShellHorizPos,
	sizeof(Position), XtOffsetOf(VendorShellRec, core.x),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNy, XmCPosition, XmRShellVertPos,
	sizeof(Position), XtOffsetOf(VendorShellRec, core.y),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNwidth, XmCDimension, XmRShellHorizDim,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNheight, XmCDimension, XmRShellVertDim,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNborderWidth, XmCBorderWidth, XmRShellHorizDim,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.border_width),
	XmRImmediate, (XtPointer)0
    },
#if 0
    {
	XmNbaseWidth, XmCBaseWidth, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.base_width),
        XmRImmediate, (XtPointer)0
	/* Motif has XmRHorizontalInt, (XtPointer)wacky */
    },
    {
	XmNbaseHeight, XmCBaseHeight, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.base_height),
        XmRImmediate, (XtPointer)0
	/* Motif has XmRVerticalInt, (XtPointer)wacky */
    },
    {
	XmNminWidth, XmCMinWidth, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_width),
        XmRImmediate, (XtPointer)0
	/* Motif has XmRHorizontalInt, (XtPointer)wacky */
    },
    {
	XmNminHeight, XmCMinHeight, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_height),
        XmRImmediate, (XtPointer)0
	/* Motif has XmRVerticalInt, (XtPointer)wacky */
    },
    {
	XmNmaxWidth, XmCMaxWidth, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_width),
        XmRImmediate, (XtPointer)XmUNSPECIFIED
	/* Motif has XmRHorizontalInt, (XtPointer)wacky */
    },
    {
	XmNmaxHeight, XmCMaxHeight, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_height),
        XmRImmediate, (XtPointer)XmUNSPECIFIED
	/* Motif has XmRVerticalInt, (XtPointer)wacky */
    },
    {
	XmNwidthInc, XmCWidthInc, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.width_inc),
        XmRImmediate, (XtPointer)1
	/* Motif has XmRHorizontalInt, (XtPointer)wacky */
    },
    {
	XmNheightInc, XmCHeightInc, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.height_inc),
        XmRImmediate, (XtPointer)1
	/* Motif has XmRVerticalInt, (XtPointer)wacky */
    },
    {
	XmNminAspectX, XmCMinAspectX, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_aspect.x),
        XmRImmediate, (XtPointer)0
	/* Motif has XmRHorizontalInt, (XtPointer)wacky */
    },
    {
	XmNminAspectY, XmCMinAspectY, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_aspect.y),
        XmRImmediate, (XtPointer)0
	/* Motif has XmRVerticalInt, (XtPointer)wacky */
    },
    {
	XmNmaxAspectX, XmCMaxAspectX, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_aspect.x),
        XmRImmediate, (XtPointer)0
	/* Motif has XmRHorizontalInt, (XtPointer)wacky */
    },
    {
	XmNmaxAspectY, XmCMaxAspectY, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_aspect.y),
        XmRImmediate, (XtPointer)0
	/* Motif has XmRVerticalInt, (XtPointer)wacky */
    },
#endif
    {
	XmNiconPixmap, XmCIconPixmap, XmRPixmap,
	sizeof(Pixmap), XtOffsetOf(VendorShellRec, wm.wm_hints.icon_pixmap),
        XmRPixmap, (XtPointer)NULL
    },
#if 0
    {
	XmNiconX, XmCIconX, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.wm_hints.icon_x),
        XmRImmediate, (XtPointer)0
	/* Motif has XmRHorizontalInt, (XtPointer)wacky */
    },
    {
	XmNiconY, XmCIconY, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.wm_hints.icon_y),
        XmRImmediate, (XtPointer)0
	/* Motif has XmRVerticalInt, (XtPointer)wacky */
    },
#endif
    {
	XtNinput, XtCInput, XmRBool,
	sizeof(Bool), XtOffsetOf(WMShellRec, wm.wm_hints.input),
	XtRImmediate, (XtPointer)True
    },
    {
	XmNwindowGroup, XmCWindowGroup, XmRWindow,
	sizeof(Window), XtOffsetOf(WMShellRec, wm.wm_hints.window_group),
	XmRImmediate, (XtPointer)00000003
	/* Motif has XmRImmediate, (XtPointer)00000003; what's this mean? */
    }
};

static XmBaseClassExtRec _XmVendorSCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (WidgetClass)&xmVendorShellExtClassRec,
    /* secondary_object_create   */ secondary_object_create,
    /* get_secondary_resources   */ get_sec_res_data,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ get_values_prehook,
    /* get_values_posthook       */ get_values_posthook,
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ FALSE,
    /* widget_navigable          */ NULL,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

VendorShellClassRec vendorShellClassRec = {
    /* Core Class Part */
    {
	/* superclass            */ (WidgetClass)&wmShellClassRec,
	/* class_name	         */ "VendorShell",
	/* size                  */ sizeof(VendorShellRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* Class init'ed ?       */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,		
	/* realize               */ realize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ vendor_resources,
	/* resource_count        */ XtNumber(vendor_resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ FALSE,
	/* compress_exposure     */ TRUE,
	/* compress_enterleave   */ FALSE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,			
	/* set_values_almost     */ XtInheritSetValuesAlmost,  
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* intrinsics version    */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmVendorSCoreClassExtRec,
    },
    /* Composite Class Part */
    {
	/* geometry_manager */ _XmRootGeometryManager,
        /* change_managed   */ change_managed,
        /* insert_child     */ insert_child,
        /* delete_child     */ delete_child,
        /* extension        */ (XtPointer) &vendorCompositeExt,
    },
    /* Shell Class Part */
    {
        /* extension */ NULL
    },
    /* WMShell Class Part */
    {
        /* extension */ NULL
    },
    /* Vendor Shell Class Part */
    {
        /* extension */ NULL
    }
};

WidgetClass vendorShellWidgetClass = (WidgetClass) (&vendorShellClassRec);

static Display *default_display = NULL;

#define background_width 16
#define background_height 16
static unsigned char background_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

#define f25_width 16
#define f25_height 16
static unsigned char f25_bits[] = {
   0x55, 0x55, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00,
   0x55, 0x55, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00,
   0x55, 0x55, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00};

#define f50_width 16
#define f50_height 16
static unsigned char f50_bits[] = {
   0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa,
   0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa,
   0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa};

#define f75_width 16
#define f75_height 16
static unsigned char f75_bits[] = {
   0xbb, 0xbb, 0xee, 0xee, 0xdd, 0xdd, 0x77, 0x77, 0xbb, 0xbb, 0xee, 0xee,
   0xdd, 0xdd, 0x77, 0x77, 0xbb, 0xbb, 0xee, 0xee, 0xdd, 0xdd, 0x77, 0x77,
   0xbb, 0xbb, 0xee, 0xee, 0xdd, 0xdd, 0x77, 0x77};

#define horizontal_width 16
#define horizontal_height 16
static unsigned char horizontal_bits[] = {
   0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00,
   0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00,
   0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00};

#define vertical_width 16
#define vertical_height 16
static unsigned char vertical_bits[] = {
   0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
   0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
   0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55};

#define slant_right_width 16
#define slant_right_height 16
static unsigned char slant_right_bits[] = {
   0x11, 0x11, 0x88, 0x88, 0x44, 0x44, 0x22, 0x22, 0x11, 0x11, 0x88, 0x88,
   0x44, 0x44, 0x22, 0x22, 0x11, 0x11, 0x88, 0x88, 0x44, 0x44, 0x22, 0x22,
   0x11, 0x11, 0x88, 0x88, 0x44, 0x44, 0x22, 0x22};

#define slant_left_width 16
#define slant_left_height 16
static unsigned char slant_left_bits[] = {
   0x88, 0x88, 0x11, 0x11, 0x22, 0x22, 0x44, 0x44, 0x88, 0x88, 0x11, 0x11,
   0x22, 0x22, 0x44, 0x44, 0x88, 0x88, 0x11, 0x11, 0x22, 0x22, 0x44, 0x44,
   0x88, 0x88, 0x11, 0x11, 0x22, 0x22, 0x44, 0x44};

static void 
class_initialize()
{
    /* we only do this here so that people can override the default reptypes,
       based on information from Harald Albrecht */
    _XmInitializeExtensions();
    _XmVendorSCoreClassExtRec.record_type = XmQmotif;

    XmSetColorCalculation(NULL);
    _XmRegisterConverters();
    _XmSetupImageCache();
}

static void 
class_part_initialize(WidgetClass widget_class)
{
    CompositeClassExtension ext, *extptr;
    VendorShellWidgetClass vsclass = (VendorShellWidgetClass) widget_class;

    XdbDebug(__FILE__, NULL, "Vendor Shell's class_part_initialize()\n");
    extptr = (CompositeClassExtension*)_XmGetClassExtensionPtr((XmGenericClassExt*)&(vsclass->composite_class.extension),
							       NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension) XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = vsclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = False;
#endif
	    vsclass->composite_class.extension = (XtPointer) ext;
	}
    }

    _XmBaseClassPartInitialize(widget_class);
    _XmFastSubclassInit(widget_class, XmVENDOR_SHELL_BIT);
}

/*
 * Helper function: checks whether a shell is a (real) popup shell or some
 * other kind of shell. We need this one lateron when we have do deal with
 * top level shells.
 * The check is done by searching the list of popups which is maintained by
 * our parent widget. As the widget we're looking for has probably just been
 * added a few machine cycles ago, we're browsing the list backwards.
 */
static Boolean
LTIsARealPopupShell(Widget wid)
{
    Widget     dad = XtParent(wid);
    WidgetList popups;
    int        i;

    if ( dad ) { /* Oops, we might got an application shell with no parent */
        popups = dad->core.popup_list;
        for ( i = dad->core.num_popups ; --i >= 0; ) {
            if ( popups[i] == wid ) {
                return True;
            }
        }
    }
    return False;
}

/*
 * Helper function: returns the next parental shell for any given shell
 * widget. If there isn't any parent shell (as we're might already be at the
 * top of the hill) then the function returns NULL.
 */
static Widget
LTGetParentShell(Widget w)
{
    while (((w = XtParent(w)) != NULL) && !XtIsVendorShell(w))
	;

    return w;
}

/*
 * When given the current shell widget this function returns the desktop
 * extension object belonging to the parent shell of the shell given.
 * Well -- if we're already at the top level of the shell hierarchy then
 * the function returns the widget id of the screen widget.
 */
static Widget
LTGetDesktopLogicalParentForShell(Widget w)
{
    Widget          logParent = NULL;
    XmWidgetExtData extData;

    if (!XtIsWMShell(w)) {
        _XmError(w, "LTGetDesktopLogicalParentForShell: "
		    "need a WM shell or a subclass of.");
    }

    if (((WMShellRec *)w)->wm.transient) {
        if ( XtIsTransientShell(w) ) {
	    /*
	     * If the current shell (in "w") is transient and is indeed of
	     * the transientShell widget class (some kind of dialog), then
	     * we may already may have a shell we're transient for. If we
	     * don't have such a shell, we search for our next parental shell
	     * and use that as the shell we're transient for.
	     */
	    logParent = ((TransientShellRec *)w)->transient.transient_for;

	    if ( logParent == NULL ) {
	        logParent = LTGetParentShell(w);
		((TransientShellRec *) w)->transient.transient_for = logParent;
	    }
	}
	else {
	    /*
	     * We're transient, but we are something other than a real
	     * transient shell. So look out for the nearest parental shell
	     * and use that.
	     */
	    logParent = LTGetParentShell(w);
	}
    }

    /*
     * Now if we have found a suitable shell then we're returning the
     * extension object associated with that shell.
     */
    if (logParent && XtIsVendorShell(logParent)) {
	extData = _XmGetWidgetExtData(logParent, XmSHELL_EXTENSION);
	if (extData == NULL) {
	   _XmError(logParent, "vendor shell has no shell extension data");
	}

	return extData->widget;
    }

    /*
     * In case there is no parental shell left -- or -- there is something
     * wrong with the parental shell (that is it is not a vendor shell or
     * a subclass of) then we return the screen widget. This way the
     * vendor extension object is added to the screen's widget child list.
     */
    return XmGetXmScreen(XtScreenOfObject(w));

}

static void
secondary_object_create(Widget req, Widget new_w,
			ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData data;
    ArgList         AllArgs;
    Arg             NeededArgs[3];
    Widget          desktopParent;

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);

    /*
     * The shell extension objects have their XmNdesktopParent set pointing
     * to the next higher level within the shells' hierarchy. The other
     * "parent" resource -- XmNlogicalParent -- indicates the widget the
     * extension object is associated with.
     * --aldi 97/01/03: someone "optimized" here so that setting the
     * logicalParent and the extensionType is done after the XtCreateWidget().
     * But the initialize() method of the Desktop object relies on the
     * desktopParent resource being set already when it is called. So either
     * switch over to way we create secondary objects for gadgets (urgh) or
     * we must set XmNdesktopParent before XtCreateWidget().
     */
    data = (XmWidgetExtData)XtCalloc(1, sizeof(XmWidgetExtDataRec));
    desktopParent = LTGetDesktopLogicalParentForShell(new_w);
    if ( *num_args ) {
        AllArgs = (ArgList) XtCalloc(3 + *num_args, sizeof(Arg));
        bcopy(args, &(AllArgs[3]), *num_args * sizeof(Arg));
        XtSetArg(AllArgs[0], XmNlogicalParent, new_w);
        XtSetArg(AllArgs[1], XmNextensionType, XmSHELL_EXTENSION);
        XtSetArg(AllArgs[2], XmNdesktopParent, desktopParent);
        data->widget = XtCreateWidget("VendorShellExt",
				      (*bce)->secondaryObjectClass,
				      new_w,
				      AllArgs, 3 + *num_args);
	XtFree((char *)AllArgs);
    } else {
        XtSetArg(NeededArgs[0], XmNlogicalParent, new_w);
        XtSetArg(NeededArgs[1], XmNextensionType, XmSHELL_EXTENSION);
        XtSetArg(NeededArgs[2], XmNdesktopParent, desktopParent);
        data->widget = XtCreateWidget("VendorShellExt",
				      (*bce)->secondaryObjectClass,
				      new_w,
				      NeededArgs, 3);
    }

/*    ExtObj_LogicalParent(data->widget) = new_w; */
/*    Desktop_Parent(data->widget) = LTGetDesktopLogicalParentForShell(new_w); */
/*    ExtObj_ExtensionType(data->widget) = XmSHELL_EXTENSION; */

    data->reqWidget = NULL;

    _XmPushWidgetExtData(new_w, data, XmSHELL_EXTENSION);

    XtAddCallback(data->widget, XmNrealizeCallback,
		  _XmVendorExtRealize, NULL);

    /*
     * Now install some callbacks for use with the grab mechanism...
     */
    XtAddCallback(new_w, XmNpopupCallback, LTShellPopupCallback, data->widget);
    XtAddCallback(new_w, XmNpopdownCallback, LTShellPopdownCallback, data->widget);
}

static void 
initialize_prehook(Widget req, 
	   Widget new_w, 
	   ArgList args, 
	   Cardinal *num_args)
{
    XmDisplay	d;
    XmScreen    s;
    XmBaseClassExt bce;

    /*
     * I had to move this here to avoid recursion problems with XmDisplay
     * 051696 -- seems those problems may have been related to the bugs
     * in BaseClass.  -- MLM
     */
    if (!XmIsDisplay(new_w)) {
	d = (XmDisplay)XmGetXmDisplay(XtDisplay(new_w));

	s = (XmScreen)XmGetXmScreen(XtScreen(new_w));
 
	d->display.shellCount++;

	bce = *(XmBaseClassExt *)_XmGetBaseClassExtPtr(XtClass(new_w),
						       XmQmotif);

	if (bce && bce->secondaryObjectClass) {
	    if (bce->secondaryObjectCreate)
		(bce->secondaryObjectCreate)(req, new_w, args, num_args);
	}
    }
}

static void 
initialize_posthook(Widget req, 
	   Widget new_w, 
	   ArgList args, 
	   Cardinal *num_args)
{
    XImage *background_image;
    XImage *f25_image;
    XImage *f50_image;
    XImage *f75_image;
    XImage *horizontal_image;
    XImage *vertical_image;
    XImage *slant_right_image;
    XImage *slant_left_image;

    if (XmIsDisplay(new_w)) {
	_XmCreateImage(background_image, XtDisplay(new_w), (char *)background_bits,
		       background_width, background_height, LSBFirst);
	XmInstallImage(background_image, "background");

	_XmCreateImage(f25_image, XtDisplay(new_w), (char *)f25_bits,
		       f25_width, f25_height, LSBFirst);
	XmInstallImage(f25_image, "foreground_25");

	_XmCreateImage(f50_image, XtDisplay(new_w), (char *)f50_bits,
		       f50_width, f50_height, LSBFirst);
	XmInstallImage(f50_image, "foreground_50");

	_XmCreateImage(f75_image, XtDisplay(new_w), (char *)f75_bits,
		       f75_width, f75_height, LSBFirst);
	XmInstallImage(f75_image, "foreground_75");

	_XmCreateImage(horizontal_image, XtDisplay(new_w), (char *)horizontal_bits,
		       horizontal_width, horizontal_height, LSBFirst);
	XmInstallImage(horizontal_image, "horizontal");

	_XmCreateImage(vertical_image, XtDisplay(new_w), (char *)vertical_bits,
		       vertical_width, vertical_height, LSBFirst);
	XmInstallImage(vertical_image, "vertical");

	_XmCreateImage(slant_right_image, XtDisplay(new_w), (char *)slant_right_bits,
		       slant_right_width, slant_right_height, LSBFirst);
	XmInstallImage(slant_right_image, "slant_right");

	_XmCreateImage(slant_left_image, XtDisplay(new_w), (char *)slant_left_bits,
		       slant_left_width, slant_left_height, LSBFirst);
	XmInstallImage(slant_left_image, "slant_left");
    }
}

#define VSEPC_DeleteWindowHandler(w) \
    (((XmVendorShellExtClassRec *)XtClass(w))-> \
					vendor_class.delete_window_handler)

static void 
initialize(Widget req, 
	   Widget new_w, 
	   ArgList args, 
	   Cardinal *num_args)
{
    XmWidgetExtData data;
    XtEventHandler str_not;
    XmShellExtClassRec *shellc;

    XdbDebug(__FILE__, new_w,
	     "##########VendorShell %s initialize\n", XtName(new_w));

    data = _XmGetWidgetExtData(new_w, XmSHELL_EXTENSION);
    if (data)
	_XmVendorExtInitialize(data->reqWidget, data->widget, args, num_args);

    if (!default_display)
	default_display = XtDisplay(new_w);

    if (!XmIsDisplay(new_w)) {

	/* add the handler for editres messages */
	XtAddEventHandler(new_w, (EventMask)0, True, 
                          (XtEventHandler)_LesstifEditResCheckMessages, NULL);

	XdbDebug(__FILE__, new_w, "Setting up virtual key bindings\n");

	XtSetKeyTranslator(XtDisplay(new_w), (XtKeyProc)XmTranslateKey);

	shellc = (XmShellExtClassRec *)XtClass(data->widget);
	str_not = shellc->shell_class.structureNotifyHandler;

	if (data) {
	    XtAddEventHandler(new_w,
			      (EventMask)FocusChangeMask|
				    EnterWindowMask|LeaveWindowMask,
			      True, 
			      (XtEventHandler)_XmTrackShellFocus,
			      (XtPointer)data->widget);

	    XtAddEventHandler(new_w,
			      (EventMask)StructureNotifyMask,
			      True, 
			      (XtEventHandler)str_not,
			      (XtPointer)data->widget);
	}
    }
}

/*
 * Don't forget to clean up: the shell extension data as well as the
 * extension object for example...
 */
static void
destroy(Widget w)
{
    XmWidgetExtData data;

    _XmPopWidgetExtData(w, &data, XmSHELL_EXTENSION);
    XtDestroyWidget(data->widget);
    XtFree((char *) data);
}


static void 
realize(Widget w,
	XtValueMask *value_mask,
	XSetWindowAttributes *attributes)
{
    XdbDebug(__FILE__, w, "Realize\n");

#define superclass (&wmShellClassRec)
    (*superclass->core_class.realize)(w, value_mask, attributes);
#undef superclass
}

static void
WmProtocolHandler(Widget w, XtPointer client, XtPointer call)
{
    XmVendorShellExtObject	ve = (XmVendorShellExtObject)client;
    int				i;

    XdbDebug(__FILE__, w, "WmProtocolHandler\n");

    switch (VSEP_DeleteResponse(ve)) {
	case XmDESTROY:
	    XtDestroyWidget(w);
	    if (XtIsApplicationShell(w)) {
		XdbDebug(__FILE__, w, "WmProtocolHandler(DeleteResponse XmDESTROY) - Exiting (WM_DELETE_WINDOW)\n");
		XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
		exit(0);
	    }
	    XdbDebug(__FILE__, w, "WmProtocolHandler(DeleteResponse XmDESTROY)\n");
	    break;
	case XmUNMAP:
	    /* The word says UNMAP but we really have to unMANAGE */
	    for (i=0; i<MGR_NumChildren(w); i++)
		if (XtIsManaged(MGR_Children(w)[i])) {
		    XdbDebug2(__FILE__, w, MGR_Children(w)[i], "XtUnmanageChild(child)\n");
		    XtUnmanageChild(MGR_Children(w)[i]);
		    return;	/* Shells have only one managed child */
		}
	    break;
	case XmDO_NOTHING:
	    XdbDebug(__FILE__, w, "WmProtocolHandler(DeleteResponse XmNO_NOTHING)\n");
	    return ;
    }
}

static void
resize(Widget w)
{
    /* Chain up for now -- needed due to the resize mechanism */
    wmShellClassRec.core_class.resize(w);
}

/*
 * set_values_prehook
 */
static Boolean
set_values_prehook(Widget old, Widget req, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData data;
    int size;
    XtPointer nsec, rsec;
    Widget ve;
 
    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;
 
    nsec = _XmExtObjAlloc(size);
    rsec = _XmExtObjAlloc(size);

    ve = _LtFindVendorExt(new_w);

    data = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));

    bcopy(ve, rsec, size);
    bcopy(ve, nsec, size);

    data->widget = (Widget)ve;
    data->oldWidget = nsec;
    data->reqWidget = rsec;

    _XmPushWidgetExtData(new_w, data, XmSHELL_EXTENSION);

    _XmExtImportArgs(data->widget, args, num_args);

    XtSetValues(data->widget, args, *num_args);

    return False;
}

static Boolean
set_values_posthook(Widget old, Widget req, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    XmWidgetExtData data;

    _XmPopWidgetExtData(new_w, &data, XmSHELL_EXTENSION);

    _XmExtObjFree(data->oldWidget);
    _XmExtObjFree(data->reqWidget);

    XtFree((char *)data);

    return False;
}

static Boolean
set_values(Widget old, Widget req, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    XmWidgetExtData data;
    Boolean refresh = False;

    data = _XmGetWidgetExtData(new_w, XmSHELL_EXTENSION);
    if (data)
	refresh = _XmVendorExtSetValues(data->oldWidget, data->reqWidget,
					data->widget, args, num_args);

    return refresh;
}

static void
get_values_prehook(Widget widget, 
		ArgList args,
		Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData data;
    int size;
    XtPointer nsec;
    Widget ve;
 
    bce = _XmGetBaseClassExtPtr(XtClass(widget), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;
 
    nsec = _XmExtObjAlloc(size);

    ve = _LtFindVendorExt(widget);

    bcopy(ve, nsec, size);

    data = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    data->widget = nsec;

    _XmPushWidgetExtData(widget, data, XmSHELL_EXTENSION);

    XtGetValues(data->widget, args, *num_args);
 
    _XmExtGetValuesHook(data->widget, args, num_args);
}

static void
get_values_posthook(Widget widget, 
		ArgList args,
		Cardinal *num_args)
{
     XmWidgetExtData ext;
 
     _XmPopWidgetExtData(widget, &ext, XmSHELL_EXTENSION);
 
     _XmExtObjFree((XtPointer)ext->widget);
 
     XtFree((char *)ext);
}

static void 
change_managed(Widget wid)
{
    ShellWidget w = (ShellWidget) wid;
    Widget* childP;
    int i;

#define superclass (&wmShellClassRec)
    (*superclass->composite_class.change_managed)(wid);
#undef superclass

    for (i = w->composite.num_children, childP = w->composite.children;
	 i; i--, childP++) {
	if (XtIsRectObj(*childP) && XtIsManaged(*childP)) {
	    XtSetKeyboardFocus(wid, *childP);
	    /* replace this with the lesstif focus handling stuff when it's
	     * in..  MLM: It's in.  Don't remove this. */
	}
    }
}

static void
insert_child(Widget w)
{
    /* Keep those pesky objects OUT of the child list */
    if (!XtIsRectObj(w))
	return;

#define superclass (&wmShellClassRec)
    (*superclass->composite_class.insert_child)(w);
#undef superclass
}

static void
delete_child(Widget w)
{
    /* Keep those pesky objects OUT of the child list */
    if (!XtIsRectObj(w))
	return;

#define superclass (&wmShellClassRec)
    (*superclass->composite_class.delete_child)(w);
#undef superclass
}

Cardinal
_XmFilterResources(XtResource *resources,
		   Cardinal numResources,
		   WidgetClass filterClass,
		   XtResource **filteredResourcesRtn)
{
    *filteredResourcesRtn = NULL;
    return 0;
}

#ifdef DEBUG
static void
dump_grab_list(Widget w)
{
    int i;

    for (i = 0; i < Display_NumModals(w); i++) {
	printf(" Wid: %p ve: %p grabber: %p exclusive: %d sprung: %d\n",
	       Display_Modals(w)[i].wid, Display_Modals(w)[i].ve,
	       Display_Modals(w)[i].grabber, Display_Modals(w)[i].exclusive,
	       Display_Modals(w)[i].springLoaded);
    }
}
#endif

/*
 * Put a widget on LessTif's grab list and issue an Intrinsics' grab.
 * The correct use of the function's parameters is a little bit tricky:
 *   - In the simplest case, leave "ve" and "grabber" set to NULL and use
 *     "wid", "exclusive", and "spring_loaded" exactly the same way as you
 *     would using XtAddGrab(). _XmAddGrab() does it just that way.
 *   - You can set "wid" to NULL, but then you *must* specify a valid
 *     "ve" (= vendor shell extension object). In this case the grab is
 *     set to the logical parent to which the "ve" object is bound to.
 */
static void 
LTAddGrab(Widget wid, Boolean exclusive, Boolean spring_loaded,
          XmVendorShellExtObject ve, XmVendorShellExtObject grabber)
{
    Widget   d;
    XmModalData modal;

    /*
     * If there's no widget wid specified, we'll take the logical parent
     * of the vendor shell extension object as the grab destination instead.
     */    
    if (wid == NULL)
        wid = ExtObj_LogicalParent(ve);

    d = XmGetXmDisplay(XtDisplayOfObject(wid));

    /* grow the modal list, if necessary */
    if (Display_NumModals(d) >= Display_MaxModals(d)) {
        Display_MaxModals(d) += 8;
        Display_Modals(d) = (XmModalData)XtRealloc((char *)Display_Modals(d),
						   sizeof(XmModalDataRec) * 
							Display_MaxModals(d));
    }

    /*
     * Now occupy a free entry and pollute it with information about the
     * grab. Then introduce the grab to the Intrinsics and make sure the
     * grab gets removed if the grab widget should ever be destroyed
     * before releasing the grab. This is necessary so LessTif's grab list
     * stays in sync with the Intrinsics's list.
     */
    modal               = Display_Modals(d) + Display_NumModals(d);
    modal->wid          = wid;
    modal->ve           = ve;
    modal->grabber      = grabber;
    modal->exclusive    = exclusive;
    modal->springLoaded = spring_loaded;

    Display_NumModals(d)++;

    XtAddGrab(wid, exclusive, spring_loaded);

    /*
     * Never, NEVER, add this callback before you'd set up the grab. See
     * below ( RemoveLessTifGrab() ) for some explanation.
     */
    XtAddCallback(wid, XmNdestroyCallback,
                  (XtCallbackProc) LTRemoveGrabCallback,
                  (XtPointer) ve);
}

/*
 * The "public" grab interface. Put a widget on LessTif's grab list. This
 * will also set an Intrinsic's grab on that widget. The tricky thing comes
 * in whenever you remove the grab from such a widget... but see the comments
 * below for more information. Please note that you should always use
 * _XmAddGrab() instead of XtAddGrab() within LessTif.
 */
void
_XmAddGrab(Widget wid, Boolean exclusive, Boolean spring_loaded)
{
    LTAddGrab(wid, exclusive, spring_loaded,
              (XmVendorShellExtObject) NULL, (XmVendorShellExtObject) NULL);
}

/*
 * The third parameter "remove_grab" is just a small optimization, so we
 * don't need to remove callbacks and grabs if the widget to be removed
 * from the grab list is already in the phase of being destroyed.
 * One last reflection about what we're doing here:
 *   Especially during destruction of a widget we must restore right at
 *   this place all those grabs that have been issued after the grab on
 *   that widget in destruction. We therefore reissue the appropriate
 *   grabs. We *do rely* on the fact that during destruction of a widget
 *   the grab on that widget has *already been removed* by a callback.
 *   That grab in turn had been set up by the Intrinsics. So we *rely* on
 *   the calling order of callbacks. Sigh. This is the reason why you
 *   *must never* do the XtAddCallback() before the XtAddGrab() in
 *   LTAddGrab() -- or you're dead: in this case the cascade will break.
 *   See: LessTif and it's alter ego M*tif is an excellent example of
 *   clean design. Bang, bang, bang!
 */
static void
LTRemoveGrab(Widget wid, XmVendorShellExtObject ve,
	     Boolean remove_grab_physically)
{
    Widget d;
    XmModalData pd, ps;
    int i, skipped;
    
    if (wid == NULL)
        wid = ExtObj_LogicalParent(ve);

    /*
     * If we got called during the destruction phase of a widget/object, we
     * don't mind about removing the callback at all -- the callback will
     * blast of into ethernity as soon as the widget fades away.
     */
    if (remove_grab_physically) {
        XtRemoveCallback(wid, XmNdestroyCallback,
                         (XtCallbackProc) LTRemoveGrabCallback,
                         (XtPointer) ve);
    }

    /*
     * Now walk through the grab list and reissue all grabs that have
     * been issued after the grab(s) we just about to kill. Due to the
     * Intrinsics' cascade concept, we first must remove the grabs and
     * only then we can regrab the remaining widgets. Otherwise we can
     * put our list out of sync with the grabs set by the Intrinsics'
     */
    d = XmGetXmDisplay(XtDisplayOfObject(wid));
    pd = ps = Display_Modals(d);
    skipped = 0;

    for (i = Display_NumModals(d); i > 0; i--, ps++) {
        if ( (ps->wid == wid) && remove_grab_physically ) {
	    XtRemoveGrab(wid);
	}
    }

    ps = pd;
    for (i = Display_NumModals(d); i > 0; i--, ps++, pd++) {
        do {
            if (ps->wid == wid) {
                ps++;
		i--;
		skipped++; /* skip this entry */
	    }
	    else if ((ps->grabber == ve) && ve) {

	        /* Get rid off all primary application modal grabs too. */
	        ps++;
		i--;
		skipped++;
            }
	    else
                break; /* nothing more to skip at the moment */

        } while ( i > 0 );

        /*
         * See if we've already reached the end of the list and leave
         * the loop then. Otherwise check if we've skipped one or more
         * entries. We know then that we must reissue all grabs coming
         * after the first entry skipped.
         */
        if (i <= 0)
            break;

        if ( pd != ps ) {
            *pd = *ps;
            XtAddGrab(pd->wid, pd->exclusive, pd->springLoaded);
        }
    }

    Display_NumModals(d) -= skipped;
}

/*
 * This is called whenever a widget issued a grab and is now being
 * destroyed. We then must remove the grab from LessTif's grab list
 * to this list in sync with the list from the Intrinsics. Note that the
 * grab has already been removed when we come to this function as calling
 * LTAddGrab() installs another callback located inside the Xt lib and that
 * callback removed the grab.
 */
static void
LTRemoveGrabCallback(Widget wid, XtPointer client_data, 
                          XtPointer callback_data)
{
    if (!XtIsSubclass(wid, vendorShellWidgetClass))
        LTRemoveGrab(wid, (XmVendorShellExtObject) client_data, False);
}

/*
 * Simply remove a grab from a widget (identified by "wid"). This results
 * in: a) removal of the Intrinsics' grab,
 *     b) removal of the widget from LessTif's grab list.
 * All widgets that did add a grab after our widget wid had added a grab
 * will be put back on their grab. So LessTif's grab list isn't a cascade
 * like the Intrinsics' grab list but rather a modality list.
 */
void
_XmRemoveGrab(Widget wid)
{
    LTRemoveGrab(wid, NULL, True);
}

/*
 * Whenever a primary application modal dialog shows up we must add grabs
 * to all those shells which are not a parental shell of that dialog. This
 * function as well as the next one -- LTGrabRelatives() -- are responsible
 * for this task. LTGrabRelatives() ascends the shell shadow hierarchy
 * starting with the primary application modal dialog and calls LTGrabKids()
 * for every side-branch. LTGrabKids() then recursively put all shells
 * located within the branch on the grab list again.
 */
static void
LTGrabKids(XmVendorShellExtObject ve,
	   XmVendorShellExtObject skip_branch,
	   XmVendorShellExtObject grabber)
{
    int        num_kids;
    WidgetList kids;
    Widget     logParent;

    /*
     * Depending on the class of object within the shadow shell hierarchy
     * we've to choose one out of three different ways to get the object's
     * child list.
     */
    if ( _XmIsFastSubclass(XtClass(ve), XmDISPLAY_BIT) ) {
        /*
	 * We do use the short path and dive into the next deeper level
	 * of the shadow hierarchy. There we recursively repeat our task.
	 */
        kids     = MGR_Children((Widget) ve);
	num_kids = MGR_NumChildren((Widget) ve);
	for ( ; --num_kids >= 0; kids++ ) {
	    if ( ((XmVendorShellExtObject) *kids != skip_branch) &&
		 _XmIsFastSubclass(XtClass(*kids), XmSCREEN_BIT) ) {
	        LTGrabKids((XmVendorShellExtObject) *kids,
			   skip_branch, grabber);
	    }
	}
	return;
    } else if ( _XmIsFastSubclass(XtClass(ve), XmSCREEN_BIT) ) {
        kids     = ((XmScreen) ve)->desktop.children;
	num_kids = ((XmScreen) ve)->desktop.num_children;
    } else {
        kids     = Desktop_Children(ve);
	num_kids = Desktop_NumChildren(ve);
    }
    /*
     * We're either working on the children of a screen widget or a
     * vendor shell extension object. So we're sure in every case that
     * these kids are vendor shell extension objects.
     */
    for ( ; --num_kids >= 0; kids++ ) {
        if ( (XmVendorShellExtObject) *kids != skip_branch ) {
	    logParent = ExtObj_LogicalParent(*kids);
	    /*
	     * If the shell (those vendor shell extension object we're just
	     * observing) has been popped up we must set a non-exclusive
	     * grab on it or otherwise it will get no user input events.
	     * In case this shell is not a popup shell but rather a top
	     * level shell and it has been realized then we must add
	     * a non-exclusive grab, too. Otherwise we would cut it off
	     * from user input.
	     */
	    if ( ((VendorShellWidget) logParent)->shell.popped_up ) {
	        LTAddGrab(NULL, False, False,
			  (XmVendorShellExtObject) *kids, grabber);
	    } else if ( XtIsRealized(logParent) &&
			!LTIsARealPopupShell(logParent) ) {
	        LTAddGrab(NULL, False, False,
			  (XmVendorShellExtObject) *kids, grabber);
	    }
	    /*
	     * Dive into next level of the shadow shell hierarchy and
	     * repeat your task there...
	     */
	    LTGrabKids((XmVendorShellExtObject) *kids,
		       skip_branch, grabber);
	}
    }
} /* LTGrabKids */

/*
 * Within this function we start at those vendor shell extension object
 * which has belongs to the dialog just about to pop up. Then we ascend
 * the shadow shell hierarchy, and at each level we descend into those
 * branches which we haven't visited so far. This way we only add a grab
 * to such dialogs which are not parents of the current pop up dialog.
 */
static void
LTGrabRelatives(XmVendorShellExtObject grabber)
{
    XmVendorShellExtObject eo, skip_branch;

    eo = (XmVendorShellExtObject) Desktop_Parent(grabber);
    skip_branch = grabber;
    for ( ;; ) {
        /*
	 * Descend into side-branches not visited so far...
	 */
        LTGrabKids(eo, skip_branch, grabber);
	skip_branch = eo;
	if ( _XmIsFastSubclass(XtClass(eo), XmDISPLAY_BIT) ) {
	    /*
	     * We've reached the top of the shadow shell hierarchy. So
	     * let us make a break. We've now visited all important
	     * relatives.
	     */
	    break;
	} else if ( _XmIsFastSubclass(XtClass(eo), XmSCREEN_BIT) ) {
	    eo = (XmVendorShellExtObject) XtParent(eo);
	} else {
	    eo = (XmVendorShellExtObject) Desktop_Parent(eo);
	}
    }
} /* LTGrabRelatives */

/*
 * Whenever a shell widget pops up on the display of a surprised user this
 * callback handler makes sure the appropiate grabs are installed according
 * to the modality mode of the shell. Ouch, what a sentence.
 */
static void
LTShellPopupCallback(Widget w, XtPointer ClientData, XtPointer CallbackData)
{
    XmVendorShellExtObject ve = (XmVendorShellExtObject) ClientData;
    Widget                 ws;
    XtGrabKind             GrabKind = XtGrabNone;
    Boolean                GrabRelatives = False;

    XdbDebug(__FILE__, w, "ShellPopup callback\n");

    ws = XmGetXmScreen(XtScreenOfObject(w));

    VSEP_XAtMap(ve) = XtX(w);
    VSEP_YAtMap(ve) = XtY(w);
    if (!XtIsRealized(w) == None)
        XtRealizeWidget(w);

    /* FIXME! Is the next one right?? */
    VSEP_LastMapRequest(ve) = LastKnownRequestProcessed(XtDisplayOfObject(w));

    switch (VSEP_MwmHints(ve).input_mode ) {
    case MWM_INPUT_PRIMARY_APPLICATION_MODAL:
	/*
	 * Input to the ancestors of this window is prohibited. That is, no
	 * parental shell will receive input, whereas our relatives (cousines)
	 * will still receive input as will all dialogs of other top level
	 * shells within our application.
	 */
	GrabKind = XtGrabExclusive;
	if ( Screen_MwmPresent(ws) ) {
	    /*
	     * This is at least what M*tif does: if can't find mwm it just
	     * disables ALL other dialogs after popping up a primary
	     * application modal dialog. SOOOORRRRYYY. But I don't know
	     * what support from mwm here would be necessary.
	     */
	    GrabRelatives = True;
	}
        break;

    case MWM_INPUT_SYSTEM_MODAL:
        /*
	 * Input only goes to this window, no other window from any other
	 * application or ourself can receive input. This needs help from
	 * the window manager.
	 *
	 * Fall through.
	 */

    case MWM_INPUT_FULL_APPLICATION_MODAL:
        /*
	 * Input only goes to this window within this application. Other
	 * applications receive input as normal.
	 */
        GrabKind = XtGrabExclusive; /* Only dispatch all incoming events
				     * to us.
				     */
        break;

    case MWM_INPUT_MODELESS:
    default:
        /*
	 * The input goes to any window as usual.
	 */
        GrabKind = XtGrabNonexclusive;
	break;
    }

    if (GrabKind != XtGrabNone) {
        LTAddGrab(NULL, 
		  GrabKind == XtGrabExclusive ? True : False, False, 
		  ve, ve);
    }
    VSEP_GrabKind(ve) = GrabKind;
    if ( GrabRelatives ) {
        LTGrabRelatives(ve);
    }
}


static void
LTShellPopdownCallback(Widget w, XtPointer ClientData, XtPointer CallbackData)
{
    XmVendorShellExtObject ve = (XmVendorShellExtObject) ClientData;

    XdbDebug(__FILE__, w, "ShellPopdown callback\n");

    if (VSEP_GrabKind(ve) != XtGrabNone) {
	XdbDebug(__FILE__, w, "Remove grab\n");
        LTRemoveGrab(NULL, ve, True);
    }
}

/*
 * if this doesn't exist, the BaseClass geometry wrapper won't work.  I assume
 * this is customized for Motif, but for now, is verbatim from Xt.
 */
XtGeometryResult
_XmRootGeometryManager(Widget wid,
		       XtWidgetGeometry *request,
		       XtWidgetGeometry *reply)
{
    ShellWidget shell = (ShellWidget)(wid->core.parent);
    XtWidgetGeometry req;

    if (shell->shell.allow_shell_resize == FALSE && XtIsRealized(wid))
	return XtGeometryNo;

    if (request->request_mode & (CWX | CWY))
	return(XtGeometryNo);

    /*
     * This is a slight change from the Xt implementation.  For some reason,
     * even though we were passing in XtCWQueryOnly in the request call below,
     * the shell was still getting resized.  For now, if we pass the above
     * tests, and are only querying, return Yes.
     */
    if (request->request_mode & XtCWQueryOnly)
	return XtGeometryYes;

    req.request_mode = (request->request_mode & XtCWQueryOnly);
    if (request->request_mode & CWWidth) {
	req.width = request->width;
	req.request_mode |= CWWidth;
    }
    if (request->request_mode & CWHeight) {
	req.height = request->height;
	req.request_mode |= CWHeight;
    }
    if (request->request_mode & CWBorderWidth) {
	req.border_width = request->border_width;
	req.request_mode |= CWBorderWidth;
    }
    if (XtMakeGeometryRequest((Widget)shell, &req, NULL) == XtGeometryYes) {
        if (!(request->request_mode & XtCWQueryOnly)) {
	    wid->core.width = shell->core.width;
	    wid->core.height = shell->core.height;
	    if (request->request_mode & CWBorderWidth) {
		wid->core.x = wid->core.y = -request->border_width;
	    }
	}
	return XtGeometryYes;
    }

    return XtGeometryNo;
}

static void 
_XmVendorExtInitialize(Widget req, 
		       Widget new_w, 
		       ArgList args, 
		       Cardinal *num_args)
{
    XmVendorShellExtObject ve = (XmVendorShellExtObject)new_w;
    Atom wm_delete_window;

    XdbDebug(__FILE__, new_w, "VendorShellExt %s initialize\n", XtName(new_w));

    /* initialize the button font list */
    if (VSEP_ButtonFontList(ve) == NULL) {
	if (VSEP_DefaultFontList(ve))
	    VSEP_ButtonFontList(ve) = VSEP_DefaultFontList(ve);
	else 
	    VSEP_ButtonFontList(ve) =
		_XmGetDefaultFontList(ExtObj_LogicalParent(new_w),
				      XmBUTTON_FONTLIST);
    }

    /* initialize the label font list */
    if (VSEP_LabelFontList(ve) == NULL) {
	if (VSEP_DefaultFontList(ve))
	    VSEP_LabelFontList(ve) = VSEP_DefaultFontList(ve);
	else 
	    VSEP_LabelFontList(ve) =
		_XmGetDefaultFontList(ExtObj_LogicalParent(new_w),
				      XmLABEL_FONTLIST);
    }

    /* initialize the text font list */
    if (VSEP_TextFontList(ve) == NULL) {
	if (VSEP_DefaultFontList(ve))
	    VSEP_TextFontList(ve) = VSEP_DefaultFontList(ve);
	else 
	    VSEP_TextFontList(ve) =
		_XmGetDefaultFontList(ExtObj_LogicalParent(new_w),
				      XmTEXT_FONTLIST);
    }

    VSEP_MwmHints(ve).flags = 0;

    if (VSEP_MwmMenu(ve) != NULL)
	VSEP_MwmMenu(ve) = XtNewString(VSEP_MwmMenu(ve));

    if (VSEP_MwmHints(ve).functions != -1)
	VSEP_MwmHints(ve).flags |= MWM_HINTS_FUNCTIONS;
    if (VSEP_MwmHints(ve).decorations != -1)
	VSEP_MwmHints(ve).flags |= MWM_HINTS_DECORATIONS;
    if (VSEP_MwmHints(ve).input_mode != -1)
	VSEP_MwmHints(ve).flags |= MWM_HINTS_INPUT_MODE;

    VSEP_ImInfo(ve) = NULL;

    VSEP_FocusData(ve) = _XmCreateFocusData();

    wm_delete_window = XmInternAtom(XtDisplay(ExtObj_LogicalParent(new_w)),
				    "WM_DELETE_WINDOW", False);

    _XmInitProtocols(ExtObj_LogicalParent(new_w));

    XmAddWMProtocols(ExtObj_LogicalParent(new_w), &wm_delete_window, 1);
    XmSetWMProtocolHooks(ExtObj_LogicalParent(new_w), wm_delete_window,
			 NULL, NULL,
			 VSEPC_DeleteWindowHandler(new_w), (XtPointer)new_w);
}

static Boolean 
_XmVendorExtSetValues(Widget cw, Widget rw, Widget nw,
		      ArgList args, Cardinal *nargs)
{
    Atom at;

    XdbDebug(__FILE__, nw, "VendorShellExt %s SetValues\n", XtName(nw));

    VSEP_MwmHints(nw).flags = 0;

    if (VSEP_MwmHints(nw).functions != -1)
	VSEP_MwmHints(nw).flags |= MWM_HINTS_FUNCTIONS;
    if (VSEP_MwmHints(nw).decorations != -1)
	VSEP_MwmHints(nw).flags |= MWM_HINTS_DECORATIONS;
    if (VSEP_MwmHints(nw).input_mode != -1)
	VSEP_MwmHints(nw).flags |= MWM_HINTS_INPUT_MODE;

    if (XtIsRealized(ExtObj_LogicalParent(nw)) &&
	bcmp(&VSEP_MwmHints(nw), &VSEP_MwmHints(cw), sizeof(MwmHints)) != 0)
    {
	at = XmInternAtom(XtDisplay(nw), _XA_MWM_HINTS, False);
	/* note the 32 format */
	XChangeProperty(XtDisplay(nw), XtWindow(nw), at, at, 32,
			PropModeReplace,
			(unsigned char*)&VSEP_MwmHints(nw),
			PROP_MOTIF_WM_HINTS_ELEMENTS);
    }

    if (XtIsRealized(ExtObj_LogicalParent(nw)) &&
	((!VSEP_MwmMenu(nw) && VSEP_MwmMenu(cw)) ||
	 (VSEP_MwmMenu(nw) && !VSEP_MwmMenu(cw)) ||
	 (VSEP_MwmMenu(nw) && VSEP_MwmMenu(cw) &&
	  strcmp(VSEP_MwmMenu(nw), VSEP_MwmMenu(cw)) != 0)))
    {
	if (VSEP_MwmMenu(cw))
	    XtFree(VSEP_MwmMenu(cw));

	if (VSEP_MwmMenu(nw))
	    VSEP_MwmMenu(nw) = XtNewString(VSEP_MwmMenu(nw));

	at = XmInternAtom(XtDisplay(nw), _XA_MWM_MENU, False);
	/* note the 8 format */
	XChangeProperty(XtDisplay(nw), XtWindow(nw), at, at, 8,
			PropModeReplace,
			(unsigned char *)VSEP_MwmMenu(nw),
			VSEP_MwmMenu(nw) == NULL
				? 0 : strlen(VSEP_MwmMenu(nw)));
    }

    return False;
}

void
_XmVendorExtRealize(Widget w,
		    XtPointer closure,
		    XtPointer call_data)
{
    Atom at;
    Widget par;
    int i;

    XdbDebug(__FILE__, w, "XmVendorExtRealize\n");

    par = ExtObj_LogicalParent(w);
    if (!XmIsDisplay(par))
	_XmInstallProtocols(par);

    XdbDebug(__FILE__, w,
	     "_XmVendorExtRealize[flags %d, functions %d, decorations %d, "
	     "input_mode %d, status %d\n",
    	     VSEP_MwmHints(w).flags,
    	     VSEP_MwmHints(w).functions,
    	     VSEP_MwmHints(w).decorations,
    	     VSEP_MwmHints(w).input_mode,
    	     VSEP_MwmHints(w).status);

    /*
     * Note all the fields must be in the right order here !
     * flags, functions, decorations, input_mode, status
     * The XChangeProperty will set them all in one go.
     * note the 32 format for HINTS, the 8 format for MENU
     */
    at = XmInternAtom(XtDisplay(par), _XA_MWM_HINTS, False);
    XChangeProperty(XtDisplay(par), XtWindow(par), at, at, 32,
		    PropModeReplace,
		    (unsigned char*)&VSEP_MwmHints(w),
		    PROP_MOTIF_WM_HINTS_ELEMENTS);

    at = XmInternAtom(XtDisplay(par), _XA_MWM_MENU, False);
    XChangeProperty(XtDisplay(par), XtWindow(par), at, at, 8,
		    PropModeReplace,
		    (unsigned char *)VSEP_MwmMenu(w),
		    VSEP_MwmMenu(w) == NULL
			? 0 : strlen(VSEP_MwmMenu(w)));

    for (i = 0; i < par->core.num_popups; i++) {
	if (XtIsTransientShell(par->core.popup_list[i])) {
	    Arg args[2];
	    int argc = 0;

	    XtSetArg(args[argc], XmNtransientFor, par); argc++;
	    XtSetArg(args[argc], XmNwindowGroup, XtWindow(par)); argc++;
	    XtSetValues(par->core.popup_list[i], args, argc);
	}
    }

    /*
     * In case this is something like a top level shell and not a real
     * pop up shell we have to do a explicit grab here or we do lose the
     * ability to get user input whenever a non-modal dialog shows up.
     */
    if (!LTIsARealPopupShell(par)) {
        LTAddGrab(NULL, False, False, 
		  (XmVendorShellExtObject) w, (XmVendorShellExtObject) w);
    }
}

static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
    /* FIXME */

    return _XmSecondaryResourceData(&_XmVendorSCoreClassExtRec,
                                    data, NULL, NULL, NULL, NULL);
}

Display *
_XmGetDefaultDisplay(void)
{
    return default_display;
}

/* Motif 2.* version of the above */
Display *
XmeGetDefaultDisplay(void)
{
    return _XmGetDefaultDisplay();
}

unsigned char
_XmGetAudibleWarning(Widget w)
{
    return 0;
}

char *
_XmGetIconPixmapName(void)
{
    return NULL;
}

void
_XmClearIconPixmapName(void)
{
}

/* To be used elsewhere ... */
Widget
_LtFindVendorExt(Widget w)
{
    Widget p;
    XmWidgetExtData data;

    if (w == (Widget)0)
	return NULL;

    for (p=w;
	 !XtIsSubclass(p, vendorShellWidgetClass) && XtParent(p);
	 p = XtParent(p))
	;

    data = _XmGetWidgetExtData(p, XmSHELL_EXTENSION);
    if (data)
	return data->widget;
    else
	return NULL;
}

/* This fixes different shared library mechanism in OS/2 
   compared to the standard Unix OS's.
   The routine should be called before any other function,
   done by the _DLL_InitTerm-function under OS/2.
 */
#ifdef __EMX__
void _XmFixupVendorShell()
{
    transientShellWidgetClass->core_class.superclass =
        (WidgetClass) &vendorShellClassRec;
    topLevelShellWidgetClass->core_class.superclass =
        (WidgetClass) &vendorShellClassRec;
}

unsigned long _DLL_InitTerm (unsigned long mod_handle, unsigned long flag)
{
    if (flag==0)
	_XmFixupVendorShell();
    return 1; /* sucess */
}
#endif
