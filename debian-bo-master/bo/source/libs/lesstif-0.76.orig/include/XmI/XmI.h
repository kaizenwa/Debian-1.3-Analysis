/**
 *
 * $Id: XmI.h,v 1.10 1996/11/07 07:15:51 u27113 Exp $
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

#ifndef _XM_I_H
#define _XM_I_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * LessTif-specific functions/variables.  Use at the cost of incompatibility
 * with Motif 1.2
 * YOU SHOULD NOT CALL CALL THESE FUNCTIONS IF YOU DON'T KNOW WHAT YOU'RE
 * DOING!!
 * Correction: Some of these functions are totally undocumented Motif 1.2
 * calls.  WE don't know if we got them right, so you'd better not bank
 * on them.
 */

#include <Xm/XmP.h>
#include <Xm/ScreenP.h>
#include <Xm/ManagerP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/GadgetP.h>
#include <Xm/DrawP.h>
#include <XmI/MacrosI.h>

/*
 * extra resources
 */
#define XmNdefaultVirtualBindings	"defaultVirtualBindings"

#define _XA_MOTIF_DEFAULT_BINDINGS	"_MOTIF_DEFAULT_BINDINGS"

/*
 * STRING AND FONTLIST INTERNALS
 */
struct _XmFontListRec {
    char *tag;
    XmFontType type;
    XtPointer font;
};

struct _XmFontListContextRec {
    XmFontList fontlist;
    int current_entry;
};

struct __XmStringExtRec {
    unsigned char tag;
    unsigned char len;
    unsigned char data[1];
};

struct __XmStringComponentRec {
    XmStringComponentType type;
    int length;
    char *data;
    short font;
};
typedef struct __XmStringComponentRec _XmStringComponentRec, *_XmStringComponent;

struct __XmStringRec {
    struct __XmStringComponentRec **components;
    int number_of_components;
};

struct __XmStringContextRec {
    struct __XmStringRec *string;
    int current_component;
};

/*
 * NOTE: The first two fields in this structure MUST match those
 * in struct __XmStringContextRec!!!
 */
struct _XmtStringContextRec {
    struct __XmStringRec *string;
    int current_component;
    char *text;
    short textlen;
    char *charset;
    XmStringDirection direction;
    Boolean separator;
};

/* ADDED FOR EXTERNAL FORM */
#define XmSTRING_COMPONENT_XMSTRING     (XmSTRING_COMPONENT_LOCALE_TEXT + 1)
#define XmSTRING_TAG                    0xDFU
#define XmSTRING_LENGTH                 0x80U

/*
 * XmIm stuff
 */
typedef unsigned char XmInputPolicy;

#define	XmINHERIT_POLICY	0
#define	XmPER_WIDGET		1
#define	XmPER_SHELL		2

/*
 * GENERIC PROTOTYPES
 */
XmScreenInfo *_XmGetScreenInfo(Widget w);

String _XmMakeDialogName(String name);
void _XmError(Widget w, char *message, ...);

extern void _XmScrolledWindowLayout(Widget w, Boolean ParentResize,
				    Widget child, Boolean TestMode,
				    XtWidgetGeometry *childgeom,
				    Position x, Position y,
				    Dimension fw, Dimension fh);

/* default procs */
void _XmDefaultButtonShadowThickness(Widget w, int offset, XrmValue *val);
void _XmDefaultMultiClick(Widget w, int offset, XrmValue *val);
void _XmBulletinBoardDialogStyleDefault(Widget w, int offset, XrmValue *val);
void _XmRowColumnEntryClassDefault(Widget w, int offset, XrmValue *val);
void _XmRowColumnIsHomogeneousDefault(Widget w, int offset, XrmValue *val);
void _XmRowColumnMarginDefault(Widget w, int offset, XrmValue *val);
void _XmRowColumnMenuAcceleratorDefault(Widget w, int offset, XrmValue *val);
void _XmRowColumnPackingDefault(Widget w, int offset, XrmValue *val);
void _XmRowColumnSpacingDefault(Widget w, int offset, XrmValue *val);
void _XmCascadePixmapDefault(Widget w, int offset, XrmValue *val);
void _XmScaleProcessingDirectionDefault(Widget w, int offset, XrmValue *val);
void _XmScrollBarTroughColorDefault(Widget w, int offset, XrmValue *val);
void _XmScrollBarProcessingDirectionDefault(Widget w, int offset, XrmValue *val);
void _XmScrollBarTraversalOnDefault(Widget w, int offset, XrmValue *val);
void _XmFrameHorizSpaceDefault(Widget w, int offset, XrmValue *val);
void _XmFrameShadowTypeDefault(Widget w, int offset, XrmValue *val);
void _XmFrameShadowThicknessDefault(Widget w, int offset, XrmValue *val);
void _XmToggleButtonFillOnSelectDefault(Widget w, int offset, XrmValue *val);
void _XmToggleButtonIndicatorTypeDefault(Widget w, int offset, XrmValue *val);
void _XmVendorShellVirtualBindingsDefault(Widget w, int offset, XrmValue *val);

/* resource converters */
extern Boolean _XmCvtStringToXmString(Display *, XrmValue *, Cardinal *,
				      XrmValue *, XrmValue *, XtPointer *);
extern Boolean _XmCvtStringToFontlist(Display *, XrmValue *, Cardinal *,
				      XrmValue *, XrmValue *, XtPointer *);
extern Boolean _XmCvtStringToDimension(Display *, XrmValue *, Cardinal *,
				       XrmValue *, XrmValue *, XtPointer *);
extern Boolean _XmCvtStringToPosition(Display *, XrmValue *, Cardinal *,
				      XrmValue *, XrmValue *, XtPointer *);
extern Boolean _XmCvtStringToKeySym(Display *, XrmValue *, Cardinal *,
				      XrmValue *, XrmValue *, XtPointer *);
extern Boolean _XmCvtStringToStringTable (Display*, XrmValue*, Cardinal*,
					  XrmValue*, XrmValue*, XtPointer*);

/* Find the VendorShell Extension Object */
extern Widget _LtFindVendorExt(Widget);

/* resource destroyers */
void _XmDestroyStringTable (XtAppContext, XrmValue*, XtPointer, XrmValue*,
			    Cardinal*);

/*
 * fontlist prototypes
 */
XmFontList _XmFontListCreateDefault(Display *);
XmFontListEntry _XmFontListEntryFromTag(XmFontList fontlist, char *tag);

/* for vendor */
extern void _XmInitProtocols(Widget w);

/* Things for Label/LabelG */

extern void _XmLabelGetPixmapSize(Widget w, Pixmap Pix, Dimension *width, Dimension *height);
extern void _XmLabelAccTextSize(Widget w);
extern Boolean _XmLabelShowsAccelerators(Widget w);
extern Boolean _XmLabelShowsMnemonic(Widget w);
extern void _XmExportLabelString(Widget w, int offset, XtArgVal *value);
extern XmImportOperator _XmImportLabelString(Widget w, int offset, XtArgVal *value);

/* For buttons */
#define ACTIVATE_DELAY	100

/* GeomUtils : A few of these I'm not sure of*/
extern XtGeometryResult _XmGMReplyToQueryGeometry(Widget w,
						XtWidgetGeometry *request,
						XtWidgetGeometry *reply);
extern XtGeometryResult _XmGMHandleQueryGeometry(Widget w,
						 XtWidgetGeometry *proposed,
						 XtWidgetGeometry *answer,
				    		 Dimension margin_width,
						 Dimension margin_height,
						 unsigned char resize_policy);
extern void _XmGMEnforceMargin(Widget w,
			       Dimension margin_width,
			       Dimension margin_height,
			       Boolean useSetValues);
extern void _XmGMCalcSize(Widget w,
			  Dimension margin_w, Dimension margin_h,
			  Dimension *retw, Dimension *reth);
extern void _XmGMDoLayout(Widget w,
			  Dimension margin_w, Dimension margin_h,
			  unsigned char resize_policy, short adjust);
extern XtGeometryResult _XmGMHandleGeometryManager(Widget w, Widget instigator,
						   XtWidgetGeometry *desired,
						   XtWidgetGeometry *allowed,
						   Dimension margin_width,
						   Dimension margin_height,
						   unsigned char resize_policy,
						   Boolean allow_overlap);
extern Boolean _XmGMOverlap(Widget w, Widget instigator,
			    Position x, Position y,
			    Dimension width, Dimension height);

/* for DialogS.c */
extern void _XmBbMap(Widget w);
extern void _XmBbUnmap(Widget w);

/* for ImageCache */
extern void _XmSetupImageCache(void);

/* from MenuUtil.c */
extern void _XmFakeExpose(Widget menu_shell);
extern void _XmSetInPMMode(Widget w, Boolean flag);
extern Boolean _XmGetInPMMode(Widget w);

/* from RowColumn.c */
/* used as the operation parameter for _XmMenuFocus. */
enum {
  XmMENU_FOCUS_SAVE=0,
  XmMENU_FOCUS_RESTORE,
  XmMENU_FOCUS_SET
};

/* from MessageB.c */
extern void _XmMessageBoxInstallImages(Widget w);

/* from misc (and for primitives and gadgets) */
extern void _XmInstallStippleImages(Widget w);

#define XmEVEN_STIPPLE_IMAGE	"xm_even_stipple"
#define XmODD_STIPPLE_IMAGE	"xm_odd_stipple"

/* from ResInd */
extern void _XmExportXmString(Widget w, int offset, XtArgVal *value);
extern void _XmExportString(Widget w, int offset, XtArgVal *value);

/* from Manager.c */
#ifndef MCEPTR
#define MCEPTR(cl) \
    ((XmManagerClassExt *)(&(((XmManagerWidgetClass)(cl))->manager_class.extension)))
#endif
#ifndef _XmGetManagerClassExtPtr
#define _XmGetManagerClassExtPtr(cl, o) \
    ((*MCEPTR(cl) && (((*MCEPTR(cl))->record_type) == (o))) \
        ? MCEPTR(cl) \
        : ((XmManagerClassExt *)_XmGetClassExtensionPtr(((XmGenericClassExt *)MCEPTR(cl)), (o))))
#endif

extern void _XmManagerInstallAccelerator(Widget m, Widget w, String s);
extern void _XmManagerInstallMnemonic(Widget m, Widget w, KeySym mn);
extern void _XmManagerUninstallAccelerator(Widget m, Widget w);
extern void _XmManagerUninstallMnemonic(Widget m, Widget w);

/* for ScrollBar */
extern Position _XmScrollBarValueToPos(Widget sw, int value);
extern Dimension _XmScrollBarSliderPixSize(Widget sw);

/* for TearOff */
extern void _XmPushButtonSetTranslation(Widget, int);

/* TearOff */
extern void _XmTearOffInitiate(Widget w, XEvent *event);

/* Traversal */
extern void _XmSetFocusResetFlag(Widget w, Boolean value);
extern Boolean _XmGetFocusResetFlag(Widget w);
extern Widget _XmGetActiveTabGroup(Widget widget);
extern Widget _XmFindTopMostShell(Widget widget);

/* VirtKeys */
/*
 * This is for handling of ALT, META, and other modifier keys when parsing
 * the virtual binding(s). In contrast to the closed software foundation,
 * LessTif is able to configure the current setting of modifiers and to
 * adjust itself to the right modifier masks.
 * USING THIS STUFF WILL MAKE YOUR APPLICATION MORE USEFUL BUT WILL BREAK
 * THE COMPATIBILITY. THIS IS PRIMARILY FOR INTERNAL USE.
 */
typedef enum _XmModifierLevels {
    ALTModifier = 0,
    METAModifier,
    SUPERModifier,
    HYPERModifier,
    /* This one must be the last! */ MAX_MODIFIERS
} XmModifierLevels;

typedef Modifiers XmModifierMaskSet[MAX_MODIFIERS];
typedef Modifiers *XmModifierMaskSetReference;

extern XmModifierMaskSetReference _XmGetModifierMappingsForDisplay(Display *Dsp);
extern void _XmInvalidateModifierMappingsForDisplay(Display *Dsp);
extern void _XmRefreshVirtKeys(Widget w);

/*
 * Behaviour control for XmFormLayout, XmRowColumnLayout.
 * This is their second parameter.
 */
#define Mode_Normal     0x00    
#define Mode_Test       0x01    
#define Mode_Resize     0x02

#ifdef __cplusplus
}
#endif



#endif /* _XM_I_H */
