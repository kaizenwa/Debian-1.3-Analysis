/* $Id: SmartMB.c,v 1.2 1996/08/25 01:35:11 miers Exp $ */
/*
 * Copyright 1994 John L. Cwikla
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appears in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of John L. Cwikla or
 * Wolfram Research, Inc not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.	John L. Cwikla and Wolfram Research, Inc make no
 * representations about the suitability of this software for any
 * purpose. It is provided "as is" without express or implied warranty.
 *
 * John L. Cwikla and Wolfram Research, Inc disclaim all warranties with
 * regard to this software, including all implied warranties of
 * merchantability and fitness, in no event shall John L. Cwikla or
 * Wolfram Research, Inc be liable for any special, indirect or
 * consequential damages or any damages whatsoever resulting from loss of
 * use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 *
 * Author:
 *  John L. Cwikla
 *  X Programmer
 *  Wolfram Research Inc.
 *
 *  cwikla@wri.com
*/

/*
** SmartMessageB.c
*/

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>

#if XPM
#include <X11/xpm.h>
#endif /* XPM */

#include <Xm/Xm.h>
#include <Xm/VendorSP.h>
#include <Xm/BulletinBP.h>
#include <Xm/LabelG.h>
#include <stdio.h>
#include <ctype.h>
#include "SmartMBP.h"

#define managerTranslations _XmBulletinB_defaultTranslations
extern void _XmBulletinBoardSetDefaultShadow(); /* Why rewrite? */

#define CORE(a) (((XmSmartMessageBoxWidget)(a))->core)
#define WIDTH(a) (((XmSmartMessageBoxWidget)(a))->core.width)
#define HEIGHT(a) (((XmSmartMessageBoxWidget)(a))->core.height)
#define COMP(a) (((XmSmartMessageBoxWidget)(a))->composite)
#define MANAGER(a) (((XmSmartMessageBoxWidget)(a))->manager)
#define SMB(a) (((XmSmartMessageBoxWidget)(a))->smart_message_box)
#define LABEL(a) (((XmSmartMessageBoxWidget)(a))->smart_message_box.labelW)
#define CONTROL(a) (((XmSmartMessageBoxWidget)(a))->smart_message_box.controlW)
#define SEP(a) (((XmSmartMessageBoxWidget)(a))->smart_message_box.separatorW)
#define DTYPE(a) (((XmSmartMessageBoxWidget)(a))->smart_message_box.dialogType)
#define PIX(a) (((XmSmartMessageBoxWidget)(a))->smart_message_box.pixmap)
#define DPOS(a) (((XmSmartMessageBoxWidget)(a))->smart_message_box.dialogPositioning)

#define MCLASS(a) ((XmSmartMessageBoxWidget)(a)->smart_message_box_class)

#define GOOD(a) ((a) && XtIsManaged(a))

#define CH_TYPE(a) (((XmSmartMessageBoxConstraint)((a)->core.constraints))->smart_message_box.childType)

#define IS_ACTION(a) (CH_TYPE(a) == XmCHILD_ACTION)
#define IS_CONTROL(a) (CH_TYPE(a) == XmCHILD_CONTROL)
#define IS_SEPARATOR(a) (CH_TYPE(a) == XmCHILD_SEPARATOR)
#define IS_LABEL(a) (CH_TYPE(a) == XmCHILD_LABEL)


#ifdef _NO_PROTO
static XtProc classInit();
static XtInitProc initialize();
static Boolean setValues();
static Boolean constraintSetValues();
static XtWidgetProc insertChild();
static XtWidgetProc deleteChild();
static XtWidgetProc destroy();

static Boolean noGeoRequest();
static XmGeoMatrix geoMatrixCreate();
static Boolean cvtStringToSMBChildType();
static Boolean cvtStringToDialogPositioning();
static void addLabel();
static void removeLabel();
#if XPM
static String findFile();
#endif
static void addPositioningToShell();
static void removePositioningFromShell();

#else
static XtProc classInit();
static XtInitProc initialize(Widget _request, Widget _new, String *_args, Cardinal *_numArgs);
static Boolean setValues(Widget _current, Widget _request, Widget _new, ArgList args, Cardinal *num_args);
static Boolean constraintSetValues(Widget _current, Widget _request, Widget _new);
static XtWidgetProc insertChild(Widget _w);
static XtWidgetProc deleteChild(Widget _w);
static XtWidgetProc destroy(Widget _smbw);

static Boolean noGeoRequest(XmGeoMatrix _geoSpec);
static XmGeoMatrix geoMatrixCreate(Widget _w, Widget _from, XtWidgetGeometry *_pref);
static Boolean cvtStringToSMBChildType(Display *_display, XrmValuePtr _args,
	Cardinal *_numArgs, XrmValuePtr _from, XrmValuePtr _to, XtPointer *_data);
static Boolean cvtStringToDialogPositioning(Display *_display, XrmValuePtr _args,
	Cardinal *_numArgs, XrmValuePtr _from, XrmValuePtr _to, XtPointer *_data);
static void addLabel(Widget _smbw);
static void removeLabel(Widget _smbw);
#if XPM
static String findFile(Widget, char *_name);
#endif
static void addPositioningToShell(Widget _smb);
static void removePositioningFromShell(Widget _smb);

#endif /* _NO_PROTO */

#define LOCALCONSTRAINTREC XmSmartMessageBoxConstraintRec

#define TheOffset(field) XtOffset(XmSmartMessageBoxWidget, smart_message_box.field)
static XtResource resources[] =
{
	{XmNdata, XmCData, XmRPointer, sizeof(XtPointer),
		TheOffset(data), XmRImmediate, (XtPointer)NULL},
	{XmNminimizeButtons, XmCMinimizeButtons, XmRBoolean, sizeof(Boolean),
		TheOffset(minimizeButtons), XmRImmediate, (XtPointer)False},
	{XmNdialogType, XmCDialogType, XmRDialogType, sizeof(unsigned char),
		TheOffset(dialogType), XmRImmediate, (XtPointer)XmDIALOG_MESSAGE},
	{XmNdialogPositioning, XmCDialogPositioning, XmRDialogPositioning, sizeof(DialogPositioning),
		TheOffset(dialogPositioning), XmRImmediate, (XtPointer)XmDIALOG_POSITIONING_LEAVE_ALONE},
};

#undef TheOffset

#define TheOffset(field) XtOffset(XmSmartMessageBoxConstraint, smart_message_box.field)
static XtResource constraintResources[] =
{
	{XmNchildType, XmCChildType, XmRSMBChildType, sizeof(unsigned char),
		TheOffset(childType), XmRImmediate, (XtPointer)XmCHILD_ACTION},
};
#undef TheOffset


static CompositeClassExtensionRec smCompositeExt = 
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

XmSmartMessageBoxClassRec xmSmartMessageBoxClassRec =
{
	{
		(WidgetClass)&xmBulletinBoardClassRec,				/* superclass */
		"XmSmartMessageBox",								 /* class_name */
		(Cardinal)sizeof(XmSmartMessageBoxRec),						/* widget size */
		(XtProc)classInit,										/* class_init */
		(XtWidgetClassProc)NULL,										/* class_part_init */
		(XtEnum)FALSE,										 /* class_inited */
		(XtInitProc)initialize,									/* initialize */
		(XtArgsProc)NULL,										/* init_hook */
		XtInheritRealize,							/* realize */
		(XtActionList)NULL, 										/* actions */
		(Cardinal)0, 										 /* num_actions */
		(XtResourceList)resources,									 /* resources */
		(Cardinal)XtNumber(resources),						 /* num_resources */
		NULLQUARK,									 /* xrm_class */
		FALSE,										 /* compress_motion */
		(XtEnum)FALSE,										 /* compress_exposur */
		FALSE,										 /* compress enterleave */
		FALSE,										/* visibility_interest */
		(XtWidgetProc)destroy,										/* destroy */
		(XtWidgetProc)XtInheritResize,
		XtInheritExpose,
		(XtSetValuesFunc)setValues,									 /* set_values */
		(XtArgsFunc)NULL,										/* set_values_hook */
		XtInheritSetValuesAlmost,					/* set_values_almost */
		(XtArgsProc)NULL,										/* get_values_hook */
		XtInheritAcceptFocus,						/* accept_focus */
		XtVersion,									 /* version */
		(XtPointer)NULL,										/* callback_private */
		XtInheritTranslations,
		(XtGeometryHandler)XtInheritQueryGeometry,			/* query_geometry */
		XtInheritDisplayAccelerator,				 /* display_accelerator */
		(XtPointer)NULL,										/* extension */
	},
	{ /* Composite Part */
		(XtGeometryHandler)XtInheritGeometryManager,							 /* geometry_manager */
		(XtWidgetProc)XtInheritChangeManaged,								 /* change_managed */
		(XtWidgetProc)insertChild,						/* inherit_child */
		(XtWidgetProc)deleteChild,						/* delete_child */
		(XtPointer)&smCompositeExt,										/* extension */
	},
	{	/* Constraint Part */
		(XtResourceList)constraintResources,
		XtNumber(constraintResources),
		(Cardinal)sizeof(XmSmartMessageBoxConstraintRec),
		(XtInitProc)NULL,
		(XtWidgetProc)NULL,
		(XtSetValuesFunc)constraintSetValues,
		(XtPointer)NULL,
	},
	{	/* XmManager Part */
		XmInheritTranslations,
		NULL,
		0,	
		NULL,
		0,	
		XmInheritParentProcess,
		NULL,
	},
	{ /* XmBulletinBoard Part */
		TRUE,
		geoMatrixCreate,
		XmInheritFocusMovedProc,	
		NULL,
	},
	{ /* XmSmartMessageBox Part */
		0,
	},
};

WidgetClass xmSmartMessageBoxWidgetClass = (WidgetClass)&xmSmartMessageBoxClassRec;


#include "error.xbm"
#include "warning.xbm"
#include "info.xbm"
#include "question.xbm"
#include "working.xbm"

#if XPM
#include "error.xpm"
#include "warning.xpm"
#include "info.xpm"
#include "question.xpm"
#include "working.xpm"
#endif /* XPM */


static XtProc classInit()
{
	Display *dpy;
	XImage *image;
#if XPM
	XImage *shape;
	XpmAttributes attr;
#endif /* XPM */

	XtSetTypeConverter(XtRString, XmRSMBChildType, cvtStringToSMBChildType,
		(XtConvertArgList)NULL, 0,
			XtCacheAll, (XtDestructor)NULL);

	XtSetTypeConverter(XtRString, XmRDialogPositioning, cvtStringToDialogPositioning,
		(XtConvertArgList)NULL, 0,
			XtCacheAll, (XtDestructor)NULL);


	dpy = _XmGetDefaultDisplay();

/*
** Install default images.  
** These are purposely named the same as the Motif ones.
** If XmInstallImage returns false then other Motif
** stuff must have already installed these, so we don't need to keep them around.
*/

#if XPM
	attr.valuemask = 0;
	if (XpmCreateImageFromData(dpy, error_xpm, &image, &shape, &attr) != XpmSuccess)
#endif /* XPM */
		_XmCreateImage(image, dpy, (char *)error_bits, error_width, error_height, LSBFirst);

	if (XmInstallImage(image, "default_xm_error") == FALSE)
		XDestroyImage(image);

#if XPM
	attr.valuemask = 0;
	if (XpmCreateImageFromData(dpy, warning_xpm, &image, &shape, &attr) != XpmSuccess)
#endif /* XPM */
		_XmCreateImage(image, dpy, (char *)warning_bits, warning_width, warning_height, LSBFirst);

	if (XmInstallImage(image, "default_xm_warning") == FALSE)
		XDestroyImage(image);

#if XPM
	attr.valuemask = 0;
	if (XpmCreateImageFromData(dpy, info_xpm, &image, &shape, &attr) != XpmSuccess)
#endif /* XPM */
		_XmCreateImage(image, dpy, (char *)info_bits, info_width, info_height, LSBFirst);

	if (XmInstallImage(image, "default_xm_information") == FALSE)
		XDestroyImage(image);

#if XPM
	attr.valuemask = 0;
	if (XpmCreateImageFromData(dpy, question_xpm, &image, &shape, &attr) != XpmSuccess)
#endif /* XPM */
		_XmCreateImage(image, dpy, (char *)question_bits, question_width, question_height, LSBFirst);

	if (XmInstallImage(image, "default_xm_question") == FALSE)
		XDestroyImage(image);

#if XPM
	attr.valuemask = 0;
	if (XpmCreateImageFromData(dpy, working_xpm, &image, &shape, &attr) != XpmSuccess)
#endif /* XPM */
		_XmCreateImage(image, dpy, (char *)working_bits, working_width, working_height, LSBFirst);

	if (XmInstallImage(image, "default_xm_working") == FALSE)
		XDestroyImage(image);

	return (XtProc)NULL;
}

#ifdef _NO_PROTO
static XtInitProc initialize(_request, _new, _args, _numArgs)
Widget _request;
Widget _new;
String *_args;
Cardinal *_numArgs;
#else
static XtInitProc initialize(Widget _request, Widget _new, String *_args, Cardinal *_numArgs)
#endif /* _NO_PROTO */
{
	LABEL(_new) = (Widget)NULL;
	SEP(_new) = (Widget)NULL;
	CONTROL(_new) = (Widget)NULL;
	PIX(_new) = XmUNSPECIFIED_PIXMAP;

	if (DTYPE(_new) != XmDIALOG_MESSAGE)
		addLabel(_new);

	if (DPOS(_new) != XmDIALOG_POSITIONING_LEAVE_ALONE)
		addPositioningToShell(_new);

	return (XtInitProc)NULL;
}

#ifdef _NO_PROTO
static XtWidgetProc insertChild(_w)
Widget _w;
#else
static XtWidgetProc insertChild(Widget _w)
#endif /* _NO_PROTO */
{
	XmSmartMessageBoxWidget mb;
	char buffer[255];
	Cardinal numParms;

	mb = (XmSmartMessageBoxWidget)XtParent(_w);

	(*((CompositeWidgetClass)(xmSmartMessageBoxWidgetClass->core_class.superclass))->composite_class.insert_child)(_w);

	if (IS_CONTROL(_w))
	{
		if (CONTROL(mb) == (Widget)NULL)
			CONTROL(mb) = _w;
		else
		{
			numParms = 1;
			sprintf(buffer, "Trying to add %s as another control child widget!\n",
				XtName(_w));

			XtAppWarningMsg(XtWidgetToApplicationContext((Widget)_w),
				"childType", "CHILD_CONTROL", "TooManyControlWidgets",
				buffer, (String *)NULL, &numParms);

			CH_TYPE(_w) = XmCHILD_ACTION;
		}
	}
	else
	if (IS_SEPARATOR(_w))
	{
		if (SEP(mb) == (Widget)NULL)
			SEP(mb) = _w;
		else
		{
			numParms = 1;
			sprintf(buffer, "Trying to add %s as another separator child widget!\n",
				XtName(_w));

			XtAppWarningMsg(XtWidgetToApplicationContext((Widget)_w),
				"childType", "CHILD_SEPARATOR", "TooManySeparatorWidgets",
				buffer, (String *)NULL, &numParms);

			CH_TYPE(_w) = XmCHILD_ACTION;
		}
	}
	else
	if (IS_LABEL(_w))
	{
		if (LABEL(mb) == (Widget)NULL)
			LABEL(mb) = _w;
		else
		{
			numParms = 1;
			sprintf(buffer, "Trying to add %s as another label child widget!\n",
				XtName(_w));

			XtAppWarningMsg(XtWidgetToApplicationContext((Widget)_w),
				"childType", "CHILD_LABEL", "TooManyLabelWidgets",
				buffer, (String *)NULL, &numParms);

			CH_TYPE(_w) = XmCHILD_ACTION;
		}
	}

	return (XtWidgetProc)NULL;
}

#ifdef _NO_PROTO
static XtWidgetProc destroy(_smbw)
Widget _smbw;
#else
static XtWidgetProc destroy(Widget _smbw)
#endif /* _NO_PROTO */
{
	if (PIX(_smbw) != XmUNSPECIFIED_PIXMAP)
		XmDestroyPixmap(CORE(_smbw).screen, PIX(_smbw));

	return (XtWidgetProc)NULL;
}

#ifdef _NO_PROTO
static XtWidgetProc deleteChild(_w)
Widget _w;
#else
static XtWidgetProc deleteChild(Widget _w)
#endif /* _NO_PROTO */
{
	XmSmartMessageBoxWidget mb;

	mb = (XmSmartMessageBoxWidget)XtParent(_w);

	if (CONTROL(mb) == _w)
		CONTROL(mb) = (Widget)NULL;
	else
	if (LABEL(mb) == _w)
		LABEL(mb) = (Widget)NULL;
	else
	if (SEP(mb) == _w)
		SEP(mb) = (Widget)NULL;

	(*((CompositeWidgetClass)(xmSmartMessageBoxWidgetClass->core_class.superclass))->composite_class.delete_child)(_w);

	return (XtWidgetProc)NULL;
}

#ifdef _NO_PROTO
static Boolean setValues(_current, _request, _new, args, num_args)
Widget _current;
Widget _request;
Widget _new;
ArgList args;
Cardinal *num_args;
#else
static Boolean setValues(Widget _current, Widget _request, Widget _new, ArgList args, Cardinal *num_args)
#endif /* NO_PROTO */
{
	Boolean redo = FALSE;

	BB_InSetValues(_current) = TRUE;

	if (DTYPE(_new) != DTYPE(_current))
	{
		removeLabel(_current);
		addLabel(_new);
		redo = TRUE;
	}
	else
	if (SMB(_new).minimizeButtons != SMB(_current).minimizeButtons)
		redo = TRUE;
	else
	if (CONTROL(_new) != CONTROL(_current))
		redo = TRUE;
	else
	if (LABEL(_new) != LABEL(_current))
		redo = TRUE;
	else
	if (SEP(_new) != SEP(_current))
		redo = TRUE;

	if (DPOS(_new) != DPOS(_current))
	{
		removePositioningFromShell(_current);
		addPositioningToShell(_new);
	}

	BB_InSetValues(_current) = FALSE;

	if(redo && (XtClass(_new) == xmSmartMessageBoxWidgetClass))
	{
		_XmBulletinBoardSizeUpdate( (Widget) _new);
		return FALSE;
	}

	return redo;
}

#ifdef _NO_PROTO
static Boolean constraintSetValues(_current, _request, _new)
Widget _current;
Widget _request;
Widget _new;
#else
static Boolean constraintSetValues(Widget _current, Widget _request, Widget _new)
#endif /* _NO_PROTO */
{
	XmSmartMessageBoxWidget smbw;
	Boolean redo = FALSE;

	smbw = (XmSmartMessageBoxWidget)XtParent(_new);

	BB_InSetValues(smbw) = TRUE;

	if (CH_TYPE(_new) != CH_TYPE(_current))
	{
		redo = TRUE;
		if (IS_CONTROL(_new))
			CONTROL(smbw) = _new;
		else
		if (IS_LABEL(_new))
			LABEL(smbw) = _new;
		else
		if (IS_SEPARATOR(_new))
			SEP(smbw) = _new;
	}

	BB_InSetValues(smbw) = FALSE;

	if(redo && (XtClass(smbw) == xmSmartMessageBoxWidgetClass))
	{
		_XmBulletinBoardSizeUpdate( (Widget)smbw);
		return FALSE;
	}

	return redo;
}

#ifdef _NO_PROTO
static XmGeoMatrix geoMatrixCreate(_w, _from, _pref)
Widget _w;
Widget _from;
XtWidgetGeometry *_pref;
#else
static XmGeoMatrix geoMatrixCreate(Widget _w, Widget _from, XtWidgetGeometry *_pref)
#endif /* _NO_PROTO */
{
	XmSmartMessageBoxWidget smbw = (XmSmartMessageBoxWidget)_w;
	XmGeoMatrix geoSpec;
	register XmGeoRowLayout	layoutPtr;
	register XmKidGeometry boxPtr;
	XmKidGeometry tempBox;
	Cardinal actionKids;
	Widget controlKid, curChild;
	Cardinal i, numKids;
	Boolean newRow;
	int nrows;

	nrows = 0;
	actionKids = 0;
	controlKid = (Widget)NULL;

	numKids = COMP(smbw).num_children;

	for(i=0;i<numKids;i++)
	{
		curChild = COMP(smbw).children[i];

		if (IS_ACTION(curChild) && GOOD(curChild))
			actionKids++;
		else
		if (IS_CONTROL(curChild) && GOOD(curChild))
			controlKid = curChild;
	}

	if (actionKids != 0)
		nrows++;

	if (GOOD(LABEL(smbw)) || (controlKid != (Widget)NULL))
		nrows++;

	if (GOOD(SEP(smbw)))
		nrows++;

	geoSpec = _XmGeoMatrixAlloc(nrows, numKids, 0);
	geoSpec->composite = (Widget)smbw;
	geoSpec->instigator = (Widget)_from;
	if (_pref)
		geoSpec->instig_request = *_pref;

	geoSpec->margin_w = BB_MarginWidth(smbw) + MANAGER(smbw).shadow_thickness;
	geoSpec->margin_h = BB_MarginHeight(smbw) + MANAGER(smbw).shadow_thickness;
	geoSpec->no_geo_request = noGeoRequest;

	layoutPtr = &(geoSpec->layouts->row);
	boxPtr = geoSpec->boxes;

	tempBox = boxPtr;
	newRow = FALSE;

	if ( GOOD(LABEL(smbw)) && _XmGeoSetupKid(boxPtr, LABEL(smbw)))
	{
		newRow = TRUE;
		boxPtr++;
	}

	if (controlKid && _XmGeoSetupKid(boxPtr, controlKid))
	{
		newRow = TRUE;
		boxPtr++;
	}

	if ( (boxPtr != tempBox) && controlKid)
	{
		layoutPtr->fill_mode = XmGEO_EXPAND;
		layoutPtr->even_width = 0;
		layoutPtr->stretch_height = 1;
		layoutPtr->space_above = 0; /* BB_MarginHeight(smbw); */
		layoutPtr->space_between = BB_MarginWidth(smbw);
		newRow = TRUE;
	}

	if (newRow)
	{
		boxPtr++;
		layoutPtr++;
	}


	if(	GOOD(SEP(smbw)) && _XmGeoSetupKid( boxPtr, SEP(smbw)))
	{	 
		layoutPtr->fix_up = _XmSeparatorFix;
		layoutPtr->space_above = BB_MarginHeight(smbw)/2;
		boxPtr += 2;
		layoutPtr++;
	}

	if (actionKids)
	{
		for(i=0;i<numKids;i++)
		{
			curChild = COMP(smbw).children[i];

			if (IS_ACTION(curChild) && GOOD(curChild))
				_XmGeoSetupKid( boxPtr++, curChild);
		}

		layoutPtr->fill_mode = XmGEO_CENTER;
		layoutPtr->fit_mode = XmGEO_AVERAGING;
		layoutPtr->even_width = SMB(smbw).minimizeButtons ? 0 : 1;
		layoutPtr->even_height = 1;
		layoutPtr->space_above = BB_MarginHeight(smbw);
		layoutPtr++;
	}

	layoutPtr->space_above = BB_MarginHeight(smbw);
	layoutPtr->end = TRUE;
	return(geoSpec);
}

#ifdef _NO_PROTO
static Boolean noGeoRequest(_geoSpec)
XmGeoMatrix _geoSpec;
#else
static Boolean noGeoRequest(XmGeoMatrix _geoSpec)
#endif /* _NO_PROTO */
{
	if(BB_InSetValues(_geoSpec->composite) && 
		(XtClass(_geoSpec->composite) == xmSmartMessageBoxWidgetClass))
			return(TRUE);

	return( FALSE);
}

#ifdef _NO_PROTO
static void addLabel(_smbw)
Widget _smbw;
#else
static void addLabel(Widget _smbw)
#endif /* _NO_PROTO */
{
	Arg warg[2];
	int n;
	char *name, *def;
	Pixmap pixmap;
#if XPM
	char *xpm;
	XpmAttributes attr;
	XImage *image, *shape;
#endif /* XPM */

	if (LABEL(_smbw) == NULL)
	{
printf("LABEL IS NULL\n");
		n = 0;
		XtSetArg(warg[n], XmNchildType, XmCHILD_LABEL); n++;
		LABEL(_smbw) = XtCreateWidget("_smb_label", xmLabelGadgetClass, (Widget)_smbw, warg, n);
	}

	name = NULL;
	def = NULL;

	switch(DTYPE(_smbw))
	{
		case XmDIALOG_ERROR:
			name = "xm_error";
			def = "default_xm_error";
#if XPM 
			xpm = "xpm_error";
#endif /* XPM */
			break;
		case XmDIALOG_WARNING:
			name = "xm_warning";
			def = "default_xm_warning";
#if XPM 
			xpm = "xpm_warning";
#endif /* XPM */
			break;
		case XmDIALOG_INFORMATION:
			name = "xm_information";
			def = "default_xm_information";
#if XPM
			xpm = "xpm_information";
#endif /* XPM */
			break;
		case XmDIALOG_QUESTION:
			name = "xm_question";
			def = "default_xm_question";
#if XPM 
			xpm = "xpm_question";
#endif /* XPM */
			break;
		case XmDIALOG_WORKING:
			name = "xm_working";
			def = "default_xm_working";
#if XPM
			xpm = "xpm_working";
#endif /* XPM */
			break;
	}

#if XPM
	pixmap = XmUNSPECIFIED_PIXMAP;
	if ((pixmap = XmGetPixmapByDepth(CORE(_smbw).screen, xpm, MANAGER(_smbw).foreground,
		CORE(_smbw).background_pixel, CORE(_smbw).depth)) == XmUNSPECIFIED_PIXMAP)
	{
		String xpmFileName;
		xpmFileName = findFile(_smbw, xpm);
		if (xpmFileName != NULL)
		{
			attr.valuemask = 0;
			if (XpmReadFileToImage(XtDisplay(_smbw), xpmFileName, &image, &shape, &attr) == XpmSuccess)
			{
				XmInstallImage(image, xpm);
				pixmap = XmGetPixmapByDepth(CORE(_smbw).screen, xpm, MANAGER(_smbw).foreground,
					CORE(_smbw).background_pixel, CORE(_smbw).depth);
			}
			XtFree(xpmFileName);
		}
	}

	if (pixmap == XmUNSPECIFIED_PIXMAP)
#endif /* XPM */

	if ((pixmap = XmGetPixmapByDepth(CORE(_smbw).screen, name, MANAGER(_smbw).foreground, 
		CORE(_smbw).background_pixel, CORE(_smbw).depth)) == XmUNSPECIFIED_PIXMAP)
		pixmap = XmGetPixmapByDepth(CORE(_smbw).screen, def, MANAGER(_smbw).foreground,
			CORE(_smbw).background_pixel, CORE(_smbw).depth);

	n = 0;
	XtSetArg(warg[n], XmNlabelPixmap, pixmap); n++;
	XtSetArg(warg[n], XmNlabelType, XmPIXMAP); n++;
	XtSetValues(LABEL(_smbw), warg, n);

	PIX(_smbw) = pixmap; /* save for later destroy */

	XtManageChild(LABEL(_smbw));
}

#ifdef _NO_PROTO
static void removeLabel(_smbw)
Widget _smbw;
#else
static void removeLabel(Widget _smbw)
#endif /* _NO_PROTO */
{
	if (GOOD(LABEL(_smbw)))
		XtUnmanageChild((Widget)_smbw);

	if (PIX(_smbw) != XmUNSPECIFIED_PIXMAP)
	{
		XmDestroyPixmap(CORE(_smbw).screen, PIX(_smbw));
		PIX(_smbw) = XmUNSPECIFIED_PIXMAP;
	}
}

#if XPM
#ifdef _NO_PROTO
static String findFile(_smbw, _name)
Widget _smbw;
char *_name;
#else
static String findFile(Widget _smbw, char *_name)
#endif /* _NO_PROTO */
{
	char *retFile;
	Display *dpy = XtDisplay(_smbw);

/*
** Ok, ok, I should read up on XtResolvePathname some more.
*/

	if ((retFile = XtResolvePathname(dpy, "xpm", _name, NULL, NULL, NULL, 0, NULL)) == NULL)
		retFile = XtResolvePathname(dpy, "bitmaps", _name, NULL, NULL, NULL, 0, NULL);

	return retFile;
} 
#endif
	

#ifdef _NO_PROTO
static void toLower(_str1, _str2, _length)
char *_str1;
char *_str2;
int _length;
#else
static void toLower(char *_str1, char *_str2, int _length)
#endif /* _NO_PROTO */
{
	int i;
	char *ptr;

	for(ptr=_str1,i=0;(ptr!=NULL) && (i<_length);ptr++,i++)
		*(_str2+i) = tolower(*ptr);
}

#ifdef _NO_PROTO
static Boolean cvtStringToSMBChildType(_display, _args, _numArgs, _from, _to, _data)
Display *_display;
XrmValuePtr _args;
Cardinal *_numArgs;
XrmValuePtr _from;
XrmValuePtr _to;
XtPointer *_data;
#else
static Boolean cvtStringToSMBChildType(Display *_display, XrmValuePtr _args,
	Cardinal *_numArgs, XrmValuePtr _from, XrmValuePtr _to, XtPointer *_data)
#endif /* _NO_PROTO */
{
	char *lower;
	static unsigned char childType;
	Boolean badConversion = FALSE;

	if (*_numArgs != 0)
	{
		XtAppWarningMsg(XtDisplayToApplicationContext(_display), "cvtStringToSMBChildType", "wrongParamaters",
		"ResourceError",
		"cvtStringToSMBChildType needs no arguments.",
		(String *)NULL, (Cardinal *)NULL);
	}

	lower = XtNewString(_from->addr);
	toLower(_from->addr, lower, strlen(_from->addr));

	childType = XmCHILD_ACTION;

	if (!strncmp(lower, "control", 7))
		childType = XmCHILD_CONTROL;
	else
	if (!strncmp(lower, "separator", 9))
		childType = XmCHILD_SEPARATOR;
	else
	if (!strncmp(lower, "label", 5))
		childType = XmCHILD_LABEL;
	else
	if (!strncmp(lower, "action", 6))
		childType = XmCHILD_ACTION;
	else
	if (!strncmp(lower, "child_control", 13))
		childType = XmCHILD_CONTROL;
	else
	if (!strncmp(lower, "child_separator", 15))
		childType = XmCHILD_SEPARATOR;
	else
	if (!strncmp(lower, "child_label", 11))
		childType = XmCHILD_LABEL;
	else
	if (!strncmp(lower, "child_action", 12))
		childType = XmCHILD_ACTION;
	else
		badConversion = TRUE;

	XtFree(lower);

	if (badConversion)
		XtDisplayStringConversionWarning(_display, _from->addr, XmRSMBChildType);
	else
	{
		if (_to->addr == NULL)
			_to->addr = (XtPointer)&childType;
		else
		if (_to->size < sizeof(unsigned char))
			badConversion = TRUE;
		else
			*(unsigned char *)_to->addr = childType;
			_to->size = sizeof(unsigned char);
	}

	return !badConversion;
}

#ifdef _NO_PROTO
static Boolean cvtStringToDialogPositioning(_display, _args, _numArgs, _from, _to, _data)
Display *_display;
XrmValuePtr _args;
Cardinal *_numArgs;
XrmValuePtr _from;
XrmValuePtr _to;
XtPointer *_data;
#else
static Boolean cvtStringToDialogPositioning(Display *_display, XrmValuePtr _args,
	Cardinal *_numArgs, XrmValuePtr _from, XrmValuePtr _to, XtPointer *_data)
#endif /* _NO_PROTO_ */
{
	char *lower;
	static DialogPositioning dp;
	Boolean badConversion = FALSE;

	if (*_numArgs != 0)
	{
		XtAppWarningMsg(XtDisplayToApplicationContext(_display), "CvtStringToDialogPositioning",
			"wrongParamaters",
		"ResourceError",
		"CvtStringToDialogPositioning needs no arguments.",
		(String *)NULL, (Cardinal *)NULL);
	}

	lower = XtNewString(_from->addr);
	toLower(_from->addr, lower, strlen(_from->addr));

	dp = XmDIALOG_POSITIONING_LEAVE_ALONE;

	if (!strcmp(lower, "dialog_positioning_leave_alone"))
		dp = XmDIALOG_POSITIONING_LEAVE_ALONE;
	else
	if (!strcmp(lower, "dialog_positioning_initial_center"))
		dp = XmDIALOG_POSITIONING_INITIAL_CENTER;
	else
	if (!strcmp(lower, "dialog_positioning_always_center"))
		dp = XmDIALOG_POSITIONING_ALWAYS_CENTER;
	else
	if (!strcmp(lower, "dialog_positioning_default_at_pointer"))
		dp = XmDIALOG_POSITIONING_DEFAULT_AT_POINTER;
	else
	if (!strcmp(lower, "dialog_positioning_center_at_pointer"))
		dp = XmDIALOG_POSITIONING_CENTER_AT_POINTER;
	else
	if (!strcmp(lower, "leave_alone"))
		dp = XmDIALOG_POSITIONING_LEAVE_ALONE;
	else
	if (!strcmp(lower, "initial_center"))
		dp = XmDIALOG_POSITIONING_INITIAL_CENTER;
	else
	if (!strcmp(lower, "always_center"))
		dp = XmDIALOG_POSITIONING_ALWAYS_CENTER;
	else
	if (!strcmp(lower, "default_at_pointer"))
		dp = XmDIALOG_POSITIONING_DEFAULT_AT_POINTER;
	else
	if (!strcmp(lower, "center_at_pointer"))
		dp = XmDIALOG_POSITIONING_CENTER_AT_POINTER;
	else
	badConversion = TRUE;

	XtFree(lower);

	if (badConversion)
		XtDisplayStringConversionWarning(_display, _from->addr, XmRDialogPositioning);
	else
	{
		if (_to->addr == NULL)
			_to->addr = (XtPointer)&dp;
		else
		if (_to->size < sizeof(DialogPositioning))
			badConversion = TRUE;
		else
			*(DialogPositioning *)_to->addr = dp;
			_to->size = sizeof(DialogPositioning);
	}

	return !badConversion;
}


#ifdef _NO_PROTO
static Widget getShellChild(_w)
Widget _w;
#else
static Widget getShellChild(Widget _w)
#endif /* _NO_PROTO */
{
	Cardinal numKids;
	WidgetList kids;

	if (_w == (Widget)NULL)
		return (Widget)NULL;

	if (!XtIsSubclass(_w, shellWidgetClass))
		return NULL;

	XtVaGetValues(_w, XtNnumChildren, &numKids, XtNchildren, &kids, NULL);
	if (numKids == 0)
		return (Widget)NULL;
	else
		return kids[numKids-1];
}

#ifdef _NO_PROTO
static XtCallbackProc shellCenter(_shell, _remove, _nothing)
Widget _shell;
XtPointer _remove;
XtPointer _nothing;
#else
static XtCallbackProc shellCenter(Widget _shell, XtPointer _remove, XtPointer _nothing)
#endif /* _NO_PROTO */
{
	int remove;
	Position x, y;
	Widget child;
	Arg warg[2];
	int n;

	remove = (int)_remove;

	child = getShellChild(_shell);

	if (child == (Widget)NULL)
		child = _shell;

	x = DisplayWidth(XtDisplay(_shell), DefaultScreen(XtDisplay(_shell)))/2 - child->core.width/2;
	y = DisplayHeight(XtDisplay(_shell), DefaultScreen(XtDisplay(_shell)))/2 - child->core.height/2;

	n = 0;
	XtSetArg(warg[n], XtNx, x); n++;
	XtSetArg(warg[n], XtNy, y); n++;
	XtSetValues(_shell, warg, n);

	if (remove)
		XtRemoveCallback(_shell, XtNpopupCallback, (XtCallbackProc)shellCenter, (XtPointer)_remove);

	return (XtCallbackProc)NULL;
}

#ifdef _NO_PROTO
static XtCallbackProc shellMoveToPointer(_shell, _moveToDefaultButton, _nothing)
Widget _shell;
XtPointer _moveToDefaultButton;
XtPointer _nothing;
#else
static XtCallbackProc shellMoveToPointer(Widget _shell, XtPointer _moveToDefaultButton, XtPointer _nothing)
#endif /* _NO_PROTO */
{
	Widget smb, db;
	int rx, ry, wx, wy;
	Window root, child;
	int moveToDefaultButton = (int)_moveToDefaultButton;
	Position x, y;
	Arg warg[2];
	unsigned int mask;
	int n;

	if (!XQueryPointer(XtDisplay(_shell), RootWindow(XtDisplay(_shell), DefaultScreen(XtDisplay(_shell))),
		&root, &child, &rx, &ry, &wx, &wy, &mask))
		return (XtCallbackProc)NULL;

	smb = getShellChild(_shell);
	db = BB_DefaultButton((XmBulletinBoardWidget)smb);
	if (moveToDefaultButton && db)
	{
		x = rx - (CORE(db).x + WIDTH(db)/2);
		y = ry - (CORE(db).y + HEIGHT(db)/2);
	}
	else
	{
		x = rx - WIDTH(smb)/2;
		y = ry - HEIGHT(smb)/2;
	}

	n = 0;
	XtSetArg(warg[n], XtNx, x); n++;
	XtSetArg(warg[n], XtNy, y); n++;
	XtSetValues(_shell, warg, n);

	return (XtCallbackProc)NULL;
}

#ifdef _NO_PROTO
static void addPositioningToShell(_smb)
Widget _smb;
#else
static void addPositioningToShell(Widget _smb)
#endif /* _NO_PROTO */
{
	Widget parent;

	parent = XtParent(_smb);

	while(parent && !XtIsSubclass(parent, shellWidgetClass))
		parent = XtParent(parent);

	if (parent == NULL)
		return;

	switch(DPOS(_smb))
	{
		case XmDIALOG_POSITIONING_LEAVE_ALONE:
			break;
		case XmDIALOG_POSITIONING_INITIAL_CENTER:
			XtAddCallback(parent, XtNpopupCallback, (XtCallbackProc)shellCenter, (XtPointer)TRUE);
			break;
		case XmDIALOG_POSITIONING_ALWAYS_CENTER:
			XtAddCallback(parent, XtNpopupCallback, (XtCallbackProc)shellCenter, (XtPointer)FALSE);
			break;
		case XmDIALOG_POSITIONING_DEFAULT_AT_POINTER:
			XtAddCallback(parent, XtNpopupCallback, (XtCallbackProc)shellMoveToPointer, (XtPointer)TRUE);
			break;
		case XmDIALOG_POSITIONING_CENTER_AT_POINTER:
			XtAddCallback(parent, XtNpopupCallback, (XtCallbackProc)shellMoveToPointer, (XtPointer)FALSE);
			break;
	}
}

#ifdef _NO_PROTO
static void removePositioningFromShell(_smb)
Widget _smb;
#else
static void removePositioningFromShell(Widget _smb)
#endif /* _NO_PROTO */
{
    Widget parent;

    parent = XtParent(_smb);

    while(parent && !XtIsSubclass(parent, shellWidgetClass))
        parent = XtParent(parent);

    if (parent == NULL)
        return;

    switch(DPOS(_smb))
    {
        case XmDIALOG_POSITIONING_LEAVE_ALONE:
            break;
        case XmDIALOG_POSITIONING_INITIAL_CENTER:
            XtRemoveCallback(parent, XtNpopupCallback, (XtCallbackProc)shellCenter, (XtPointer)TRUE);
            break;
        case XmDIALOG_POSITIONING_ALWAYS_CENTER:
            XtRemoveCallback(parent, XtNpopupCallback, (XtCallbackProc)shellCenter, (XtPointer)FALSE);
            break;
        case XmDIALOG_POSITIONING_DEFAULT_AT_POINTER:
            XtRemoveCallback(parent, XtNpopupCallback, (XtCallbackProc)shellMoveToPointer, (XtPointer)TRUE);
            break;
        case XmDIALOG_POSITIONING_CENTER_AT_POINTER:
            XtRemoveCallback(parent, XtNpopupCallback, (XtCallbackProc)shellMoveToPointer, (XtPointer)FALSE);
            break;
    }
}


#ifdef _NO_PROTO
Widget XmCreateSmartMessageBox(_parent, _name, _warg, _numWarg)
Widget _parent;
char *_name;
ArgList _warg;
Cardinal _numWarg;
#else
Widget XmCreateSmartMessageBox(Widget _parent, char *_name, ArgList _warg, Cardinal _numWarg)
#endif /* _NO_PROTO */
{
	return (XtCreateWidget(_name, xmSmartMessageBoxWidgetClass, _parent, _warg, _numWarg));
}

