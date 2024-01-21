/**
 *
 * $Id: Form.c,v 1.22 1997/01/06 06:50:14 u27113 Exp $
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

static char rcsid[] = "$Id: Form.c,v 1.22 1997/01/06 06:50:14 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/FormP.h>
#include <Xm/DialogS.h>

#include <XmI/DebugUtil.h>

/* Don't touch these !! Danny */
#define	NO_WIDGET_BECOMES_FORM
/* End touch stuff */

#undef	DEBUG
#define	PRINT_ATTACHMENT_REPORT
#define	PRINT_REPORT
#define	PRINT_ASSIGNMENTS
#undef	PRINT_PREFERRED
#define	PRINT_LINENO

/*
 * Whether to produce sensible warnings that we've never seen Motif generate
 */
#define	LESSTIF_VERBOSE

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void resize(Widget w);
static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static XtGeometryResult geometry_manager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply);
static void ChangeManaged(Widget w);
static void XmFormLayout(XmFormWidget f, int mode, Widget cw, XtWidgetGeometry *cg, XtWidgetGeometry *fg);
static void XmFormFindPreferred(XmFormWidget f);

static void _XmFormConstraintInitialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean _XmFormConstraintSetValues(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void FormInsertChild(Widget w);

void XmFormPrintAttachmentReport(XmFormWidget f);

/*
 * Resources for the Form class
 */
#define Offset(field) XtOffsetOf(XmFormRec, form.field)
static XtResource resources[] = {
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmFormRec, bulletin_board.margin_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), XtOffsetOf(XmFormRec, bulletin_board.margin_height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNhorizontalSpacing, XmCSpacing, XmRHorizontalDimension,
	sizeof(Dimension), Offset(horizontal_spacing),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNverticalSpacing, XmCSpacing, XmRVerticalDimension,
	sizeof(Dimension), Offset(vertical_spacing),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNfractionBase, XmCMaxValue, XmRInt,
	sizeof(int), Offset(fraction_base),
	XmRImmediate, (XtPointer)100
    },
    {
	XmNrubberPositioning, XmCRubberPositioning, XmRBoolean,
	sizeof(Boolean), Offset(rubber_positioning),
	XmRImmediate, (XtPointer)False
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNmarginWidth,
	sizeof(Dimension), XtOffsetOf(XmFormRec, bulletin_board.margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), XtOffsetOf(XmFormRec, bulletin_board.margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNhorizontalSpacing,
	sizeof(Dimension), Offset(horizontal_spacing),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNverticalSpacing,
	sizeof(Dimension), Offset(vertical_spacing),
	_XmFromVerticalPixels, _XmToVerticalPixels
    }
};

#undef Offset

#define Offset(field) XtOffsetOf(XmFormConstraintRec, form.field)

#define LEFT 0
#define RIGHT 1
#define TOP 2
#define BOTTOM 3

/*
 * The opposite direction
 */
#define	OPPOSITE_DIRECTION(x)	((x & 2) | (1 - (x & 1)))

char _XmForm_defaultTranslations[] = 
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

static XtResource formConstraintResources[] = {
    {
	XmNtopAttachment, XmCAttachment, XmRAttachment,
	sizeof(unsigned char), Offset(atta[TOP].type),
	XmRImmediate, (XtPointer)XmATTACH_NONE
    },
    {
	XmNbottomAttachment, XmCAttachment, XmRAttachment,
	sizeof(unsigned char), Offset(atta[BOTTOM].type),
	XmRImmediate, (XtPointer)XmATTACH_NONE
    },
    {
	XmNleftAttachment, XmCAttachment, XmRAttachment,
	sizeof(unsigned char), Offset(atta[LEFT].type),
	XmRImmediate, (XtPointer)XmATTACH_NONE
    },
    {
	XmNrightAttachment, XmCAttachment, XmRAttachment,
	sizeof(unsigned char), Offset(atta[RIGHT].type),
	XmRImmediate, (XtPointer)XmATTACH_NONE
    },
    {
	XmNtopWidget, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(atta[TOP].w),
	XtRImmediate, (XtPointer)NULL
    },
    {
	XmNbottomWidget, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(atta[BOTTOM].w),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNleftWidget, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(atta[LEFT].w),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNrightWidget, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(atta[RIGHT].w),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNtopPosition, XmCPosition, XmRInt,
	sizeof(int), Offset(atta[TOP].value),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNbottomPosition, XmCPosition, XmRInt,
	sizeof(int), Offset(atta[BOTTOM].value),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNleftPosition, XmCPosition, XmRInt,
	sizeof(int), Offset(atta[LEFT].value),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNrightPosition, XmCPosition, XmRInt,
	sizeof(int), Offset(atta[RIGHT].value),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNtopOffset, XmCOffset, XmRVerticalInt,
	sizeof(int), Offset(atta[TOP].offset),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNbottomOffset, XmCOffset, XmRVerticalInt,
	sizeof(int), Offset(atta[BOTTOM].offset),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNleftOffset, XmCOffset, XmRHorizontalInt,
	sizeof(int), Offset(atta[LEFT].offset),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNrightOffset, XmCOffset, XmRHorizontalInt,
	sizeof(int), Offset(atta[RIGHT].offset),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNresizable, XmCBoolean, XmRBoolean,
	sizeof(Boolean), Offset(resizable),
	XmRImmediate, (XtPointer)True
    }
};

static XmSyntheticResource constraint_syn_resources[] = {
    {
	XmNtopOffset,
	sizeof(int), Offset(atta[TOP].offset),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNbottomOffset,
	sizeof(int), Offset(atta[BOTTOM].offset),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNleftOffset,
	sizeof(int), Offset(atta[LEFT].offset),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNrightOffset,
	sizeof(int), Offset(atta[RIGHT].offset),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

static XmBaseClassExtRec _XmFormCoreClassExtRec = {
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

static XmManagerClassExtRec _XmFormMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmFormClassRec xmFormClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmBulletinBoardClassRec,
        /* class_name            */ "XmForm",
	/* widget_size           */ sizeof(XmFormRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMultiple,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ XtInheritExpose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmForm_defaultTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmFormCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager, 
        /* change_managed   */ ChangeManaged, 
        /* insert_child     */ FormInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ NULL,
    },
    /* Constraint class part */
    {
	/* subresources      */ formConstraintResources,
        /* subresource_count */ XtNumber(formConstraintResources),
        /* constraint_size   */ sizeof(XmFormConstraintRec),
        /* initialize        */ _XmFormConstraintInitialize,
        /* destroy           */ NULL,  /* FIX ME */
        /* set_values        */ _XmFormConstraintSetValues,
        /* extension         */ NULL,   /* FIX ME */
    },
    /* XmManager class part */
    {
	/* translations                 */ XmInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ constraint_syn_resources,
        /* num_syn_constraint_resources */ XtNumber(constraint_syn_resources),
        /* parent_process               */ XmInheritParentProcess,
	/* extension                    */ (XtPointer)&_XmFormMClassExtRec
    },
    /* XmBulletinBoard part */
    {
	/* always_install_accelerators  */	FALSE,
	/* geo_matrix_create            */	NULL,
	/* focus_moved_proc             */	XmInheritFocusMovedProc,
	/* extension */				NULL
    },
    /* XmForm part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmFormWidgetClass = (WidgetClass)&xmFormClassRec;

static void 
class_initialize()
{
    _XmFormCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmFORM_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
	XmFormWidget	nw = (XmFormWidget) new_w;

	nw->form.bogus_border = -1;
}

/*
 * These are just to save typing and make things readable
 * Note: most of these will only work in a loop over form's children,
 * in which a couple of variables are set correctly :
 *
 */

#define	FFB	f->form.fraction_base

#define	CON	constraints->form
#define	C_TOP		CON.atta[TOP]
#define	C_BOTTOM	CON.atta[BOTTOM]
#define	C_LEFT		CON.atta[LEFT]
#define	C_RIGHT		CON.atta[RIGHT]

/* Set the geometry fields of the current child widget */
#if	defined(DEBUG) || defined(PRINT_ASSIGNMENTS)
#define	SETX(p)		{ SetX(f, child, p, __LINE__); changed = True; }
#define	SETY(p)		{ SetY(f, child, p, __LINE__); changed = True; }
#define	SETW(p)		{ SetW(f, child, p, __LINE__); changed = True; }
#define	SETH(p)		{ SetH(f, child, p, __LINE__); changed = True; }

/* Set geometry of a widget referred to with XmATTACH_WIDGET or so */
/* x is something like C_BOTTOM, p is the value to be set */
#define	SETWX(x,p)	{ SetX(f, (x).w, p, __LINE__); changed = True; }
#define	SETWY(x,p)	{ SetY(f, (x).w, p, __LINE__); changed = True; }
#define	SETWW(x,p)	{ SetW(f, (x).w, p, __LINE__); changed = True; }
#define	SETWH(x,p)	{ SetH(f, (x).w, p, __LINE__); changed = True; }

#define	SETFW(v)	{ if (ParentChangeMode) { SetFW(f, child, v, __LINE__); fw = v; changed = True; } }
#define	SETFH(v)	{ if (ParentChangeMode) { SetFH(f, child, v, __LINE__); fh = v; changed = True; } }
#else
#define	SETX(p)		{ constraints->form.x = p; changed = True; }
#define	SETY(p)		{ constraints->form.y = p; changed = True; }
#define	SETW(p)		{ constraints->form.w = p; changed = True; }
#define	SETH(p)		{ constraints->form.h = p; changed = True; }

/* Set geometry of a widget referred to with XmATTACH_WIDGET or so */
/* q is something like C_BOTTOM, p is the value to be set */
#define	SETWX(q,p)	{ ((XmFormConstraints)((q).w->core.constraints))->form.x = p; changed = True; }
#define	SETWY(q,p)	{ ((XmFormConstraints)((q).w->core.constraints))->form.y = p; changed = True; }
#define	SETWW(q,p)	{ ((XmFormConstraints)((q).w->core.constraints))->form.w = p; changed = True; }
#define	SETWH(q,p)	{ ((XmFormConstraints)((q).w->core.constraints))->form.h = p; changed = True; }

#define	SETFW(v)	{ if (ParentChangeMode) { fw = v; changed = True; } }
#define	SETFH(v)	{ if (ParentChangeMode) { fh = v; changed = True; } }
#endif

/* Lookup the geometry fields of the current child widget */
#define	CX		constraints->form.x
#define	CY		constraints->form.y
#define	CW		constraints->form.w
#define	CH		constraints->form.h

/* For widget attachments : same stuff but not for current widget */
#define	W_X(p)		((XmFormConstraints)p->core.constraints)->form.x
#define	W_Y(p)		((XmFormConstraints)p->core.constraints)->form.y
#define	W_W(p)		((XmFormConstraints)p->core.constraints)->form.w
#define	W_H(p)		((XmFormConstraints)p->core.constraints)->form.h

#define	W_ATT(p,a)	(((XmFormConstraints)p->core.constraints)->form.atta[a].type)
#define	W_OFFSET(p,a)	(((XmFormConstraints)p->core.constraints)->form.atta[a].offset)
#define	W_WIDGET(p,a)	(((XmFormConstraints)p->core.constraints)->form.atta[a].w)

/* Form's current size */
#define	FW		fw
#define	FH		fh

#define	FMW		BB_MarginWidth(f)
#define	FMH		BB_MarginHeight(f)

/*
 *
 */
#if	defined(DEBUG) || defined(PRINT_ASSIGNMENTS)
void SetW(XmFormWidget f, Widget child, int p, int line)
{
	XmFormConstraints	constraints = (XmFormConstraints) child->core.constraints;

#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
	XdbDebug2(__FILE__, (Widget)f, child, "set width to %d (line %d)\n", p, line);
#else
	XdbDebug2(__FILE__, (Widget)f, child, "set width to %d\n", p);
#endif
#endif
	constraints->form.w = p;
}

void SetH(XmFormWidget f, Widget child, int p, int line)
{
	XmFormConstraints	constraints = (XmFormConstraints) child->core.constraints;

#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
	XdbDebug2(__FILE__, (Widget)f, child, "set height to %d (line %d)\n", p, line);
#else
	XdbDebug2(__FILE__, (Widget)f, child, "set height to %d\n", p);
#endif
#endif
	constraints->form.h = p;
}

void SetX(XmFormWidget f, Widget child, int p, int line)
{
	XmFormConstraints	constraints = (XmFormConstraints) child->core.constraints;

#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
	XdbDebug2(__FILE__, (Widget)f, child, "set x to %d (line %d)\n", p, line);
#else
	XdbDebug2(__FILE__, (Widget)f, child, "set x to %d\n", p);
#endif
#endif
	constraints->form.x = p;
}

void SetY(XmFormWidget f, Widget child, int p, int line)
{
	XmFormConstraints	constraints = (XmFormConstraints) child->core.constraints;

#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
	XdbDebug2(__FILE__, (Widget)f, child, "set y to %d (line %d)\n", p, line);
#else
	XdbDebug2(__FILE__, (Widget)f, child, "set y to %d\n", p);
#endif
#endif
	constraints->form.y = p;
}

void SetFW(XmFormWidget f, Widget child, int p, int line)
{
#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
	XdbDebug2(__FILE__, (Widget)f, child, "set form width to %d (line %d)\n", p, line);
#else
	XdbDebug2(__FILE__, (Widget)f, child, "set form width to %d\n", p);
#endif
#endif
}

void SetFH(XmFormWidget f, Widget child, int p, int line)
{
#ifdef	PRINT_ASSIGNMENTS
#ifdef	PRINT_LINENO
	XdbDebug2(__FILE__, (Widget)f, child, "set form height to %d (line %d)\n", p, line);
#else
	XdbDebug2(__FILE__, (Widget)f, child, "set form height to %d\n", p);
#endif
#endif
}

#endif

/*
 * Called in the process of adding a child
 */
static void 
_XmFormConstraintInitialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    XmFormConstraints constraints = (XmFormConstraints)new_w->core.constraints;
    XmFormWidget f = (XmFormWidget)XtParent(new_w);
    int i;
    XtGeometryResult	res;
    XtWidgetGeometry	geo;
    Widget		w;

    XdbDebug2(__FILE__, (Widget)f, new_w, "_XmFormConstraintInitialize\n");

    for (i=0; i<4; i++)  /* do this for all four contraints (TOP, BOTTOM, LEFT, RIGHT) */
    {
	switch (constraints->form.atta[i].type)
	{
	case XmATTACH_WIDGET:
		/*
		 * Check if the widget attached to is a child of this Form.
		 * If not, see if one of its ancestors is. If yes, replace the
		 * widget by its ancestor that is our child.
		 */
		w = constraints->form.atta[i].w;
		if (w && XtParent(w) != (Widget)f)
		    while (w) {
			if (XtParent(w) == (Widget)f) {
				XdbDebug2(__FILE__, (Widget)f, constraints->form.atta[i].w,
					"Replace child by %s\n", XtName(w));
#ifdef	LESSTIF_VERBOSE
	/*
	 * Not sure if we should do this. I've never seen Motif issue a warning
	 * for this (nor an error). It seems better to indicate that something
	 * is wrong though.
	 */
			    {
				String	pp[3];
				Cardinal np = 3;

				pp[0] = XtName(f);
				pp[1] = XtName(constraints->form.atta[i].w);
				pp[2] = XtName(w);

				XtAppWarningMsg(XtWidgetToApplicationContext((Widget)f),
					"formGeometry", "formIllegalAttachment",
					"LessTifError",
					"XmForm %s : attachment to %s which is not a child,\n"
					"\treplaced with %s",
					pp, &np);
			    }
#endif

				constraints->form.atta[i].w = w;
				w = NULL;
			} else
				w = XtParent(w);
		}
		/* Fall tru */
	case XmATTACH_FORM:
	case XmATTACH_NONE:
	    _XmMoveObject(new_w, 0, 0);
	    CON.atta[i].percent = 0;
	    break;
	case XmATTACH_POSITION:
	    CON.atta[i].percent = (int)(((float)CON.atta[i].value 
					/ (float)f->form.fraction_base) * 100.0);
	    break;
	}
    }

/* Is this just a silly special case ??? */
    if (C_TOP.type == XmATTACH_FORM && C_BOTTOM.type == XmATTACH_FORM && XtHeight(f) == 0) {
	res = XtQueryGeometry(new_w, NULL, &geo);
	if (res != XtGeometryYes) {
	    XtHeight(f) = geo.height;
	}
    }
    if (C_LEFT.type == XmATTACH_FORM && C_RIGHT.type == XmATTACH_FORM && XtWidth(f) == 0) {
	res = XtQueryGeometry(new_w, NULL, &geo);
	if (res != XtGeometryYes) {
	    XtWidth(f) = geo.width;
        }
    }

    /* XmATTACH_SELF */
    /* Should probably also query preferred geometry and then set it */
    /* FIX ME */
}


static Boolean 
_XmFormConstraintSetValues(Widget current, 
			   Widget request, 
			   Widget new_w, 
			   ArgList args, 
			   Cardinal *num_args)
{
    XmFormConstraints new_constraints = (XmFormConstraints)new_w->core.constraints;
    XmFormConstraints old = (XmFormConstraints)current->core.constraints;
    XmFormWidget f = (XmFormWidget)XtParent(new_w);
    Boolean changed = False,
	DirtyTrick = False;
    int i;
    
    if (! new_constraints->form.resizable) {
	XtWidth(new_w) = XtWidth(current);	/* refuse request */
	XtHeight(new_w) = XtHeight(current);

	XdbDebug2(__FILE__, (Widget)f, new_w, "_XmFormConstraintSetValues - refused resize\n");
    }

    XdbDebug2(__FILE__, (Widget)f, new_w, "_XmFormConstraintSetValues\n");

    for (i=0; i<4; i++) {
	if (new_constraints->form.atta[i].value != old->form.atta[i].value) {
	    if (i == TOP || i == BOTTOM) {
		if (XtHeight(f) != 0)
		    new_constraints->form.atta[i].percent =
			(new_constraints->form.atta[i].value / XtHeight(f));
		else
		    new_constraints->form.atta[i].percent = 100;
	    }
	    else {
		if (XtWidth(f) != 0)
		    new_constraints->form.atta[i].percent =
			(new_constraints->form.atta[i].value / XtWidth(f));
		else
		    new_constraints->form.atta[i].percent = 100;
	    }

	    DirtyTrick = True;
	    changed = True;
	}

	if (new_constraints->form.atta[i].type != old->form.atta[i].type
	 || new_constraints->form.atta[i].w != old->form.atta[i].w
	 || new_constraints->form.atta[i].offset != old->form.atta[i].offset)
	{
		Widget	w;

		DirtyTrick = True;
		changed = True;

		/*
		* Check if the widget attached to is a child of this Form.
		* If not, see if one of its ancestors is. If yes, replace the
		* widget by its ancestor that is our child.
		*/
		w = new_constraints->form.atta[i].w;
		if (w && XtParent(w) != (Widget)f)
		    while (w) {
		    if (XtParent(w) == (Widget)f) {
			XdbDebug2(__FILE__, (Widget)f, new_constraints->form.atta[i].w,
			    "Replace child by %s\n", XtName(w));

#ifdef	LESSTIF_VERBOSE
	/*
	 * Not sure if we should do this. I've never seen Motif issue a warning
	 * for this (nor an error). It seems better to indicate that something
	 * is wrong though.
	 */
			    {
				String	pp[3];
				Cardinal np = 3;

				pp[0] = XtName(f);
				pp[1] = XtName(new_constraints->form.atta[i].w);
				pp[2] = XtName(w);

				XtAppWarningMsg(XtWidgetToApplicationContext((Widget)f),
					"formGeometry", "formIllegalAttachment",
					"LessTifError",
					"XmForm %s : attachment to %s which is not a child,\n"
					"\treplaced with %s",
					pp, &np);
			    }
#endif
			new_constraints->form.atta[i].w = w;
			w = NULL;
		    } else
			w = XtParent(w);
		}
	}
    }

    if (DirtyTrick) {
	/* Not alltogether sure that this is the right thing to do */
	resize((Widget)f);
    }

    return changed;
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
	Boolean		r = False,
			relayout = False;
	XmFormWidget	ow = (XmFormWidget) old,
			nw = (XmFormWidget) new_w;

	BB_InSetValues(new_w) = True;
/* FIX ME */
	relayout = r = True;

	if (XtWidth(nw) != XtWidth(ow) || XtHeight(nw) != XtHeight(ow)) {
		XdbDebug(__FILE__, new_w, "SetValues: changed size to %d %d\n", XtWidth(nw), XtHeight(nw));

		r = True;
		relayout = True;
	} else {
		XdbDebug(__FILE__, new_w, "SetValues\n");
	}


	if (relayout)
		XmFormLayout(nw, Mode_Normal, NULL, NULL, NULL);

	BB_InSetValues(new_w) = False;

	return	r;
}

static void
resize(Widget w)
{
	XdbDebug(__FILE__, w, "Resize\n");
	XmFormLayout((XmFormWidget)w, Mode_Resize, NULL, NULL, NULL);
} 

static void 
realize(Widget w, 
	XtValueMask *value_mask, 
	XSetWindowAttributes *attributes)
{
#define superclass (&xmBulletinBoardClassRec)
    (*superclass->core_class.realize)(w, value_mask, attributes);
#undef superclass

	XdbDebug(__FILE__, w, "Realize\n");
	XmFormLayout((XmFormWidget)w, Mode_Normal, NULL, NULL, NULL);
}

/*
 * QueryGeometry
 *
 * The purpose of this method is for other widgets to ask what the preferred
 *	geometry of this one is.
 * The other widget (the parent, usually) can either propose a geometry, to
 * 	which we answer, or call XtQueryGeometry(w, NULL, ___). In that case,
 *	we'll get a proposed which has no entries filled out. (Xt does some
 *	magic so we'll never see the NULL).
 * It is possible, and even common practice, to call this with the second and
 *	third parameter being identical. We must be prepared for this.
 *	That's why we copy the contents of *proposed.
 */
static XtGeometryResult 
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *desired)
{
	XtGeometryResult	r = XtGeometryYes;
	XmFormWidget		f = (XmFormWidget)w;
	XtWidgetGeometry	pp, fg;

	pp = *proposed;		/* A statement, not an initializer, due to
				   suspected bug in GCC_BOUNDS_CHECKING */

	if (proposed != desired)
		*desired = *proposed;

	XmFormLayout(f, Mode_Test, (Widget)f, desired, &fg);

	if (desired->width != XtWidth(f) || desired->height != XtHeight(f))
		r = XtGeometryAlmost;

	desired->request_mode = CWWidth | CWHeight;

	if ((pp.request_mode & (CWWidth | CWHeight)) == 0) {
	    r = XtGeometryAlmost;
	    XdbDebug(__FILE__, (Widget)f, "QueryGeometry NULL => %s, answer %s\n",
		XdbWidgetGeometry2String(&fg),
		XdbGeometryResult2String(r));
	} else
	    XdbDebug(__FILE__, (Widget)f, "QueryGeometry %s => %s, answer %s\n",
		XdbWidgetGeometry2String(&pp),
		XdbWidgetGeometry2String(&fg),
		XdbGeometryResult2String(r));

	return r;
}

/*
 * GeometryManager Method
 *
 * If request is unacceptable and we have no compromise, return XtGeometryNo.
 * If request is acceptable, return XtGeometryYes or XtGeometryDone (depending on
 *    whether LessTif wants widgets to do this by themselves).
 * If we propose a compromise, return XtGeometryAlmost.
 * If XtCWQueryOnly is set, don't change anything.
 *
 * Note that if you're not in test mode, and the result of the query will be YES,
 * and the geometry change implies that the form needs to be resized, then
 * resizing the form is one of the things this function needs to do !
 * Note also that doing so doesn't imply that we would have to reply with XtGeometryDone.
 * Done implies that we changed the child widget's geometry.
 *
 * According to Asente & Swick, do not resize the calling widget from here (p. 734).
 */
static XtGeometryResult
geometry_manager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
	XmFormWidget		f = (XmFormWidget) XtParent(w);
	XtWidgetGeometry	mr, a, fg;
	int			good = 0, ask = 0;

#define       Wants(x)        (request->request_mode & x)

	if (XdbInDebug(__FILE__, (Widget)f)) {
		XdbDebug2(__FILE__, (Widget)f, w, "GeometryManager : request ");
		if (Wants(CWX))		XdbDebug0(__FILE__, (Widget)f,  "X %d ", request->x);
		if (Wants(CWY))		XdbDebug0(__FILE__, (Widget)f,  "Y %d ", request->y);
		if (Wants(CWWidth))	XdbDebug0(__FILE__, (Widget)f,  "W %d ", request->width);
		if (Wants(CWHeight))	XdbDebug0(__FILE__, (Widget)f,  "H %d ", request->height);
		XdbDebug0(__FILE__, (Widget)f,  "\n");
	}

	mr = *request;

/* Ask form's layout algorithm what it thinks about this */
	XmFormLayout(f, Mode_Test, w, &mr, &fg);

	XdbDebug2(__FILE__, (Widget)f, w, "GeometryManager: Form Layout makes child geo %dx%d\n",
		mr.width, mr.height);

/* Now that form has looked at the geometry, tell our caller what happened */
	if (Wants(CWX)) {
	    ask++;
	    if ((mr.request_mode & CWX) && mr.x == request->x) {
		a.request_mode |= CWX;
		a.x = request->x;
		good++;
	    }
	}
	if (Wants(CWY)) {
	    ask++;
	    if ((mr.request_mode & CWY) && mr.y == request->y) {
		a.request_mode |= CWY;
		a.y = request->y;
		good++;
	    }
	}
	if (Wants(CWWidth)) {
	    ask++;
	    if ((mr.request_mode & CWWidth) && mr.width == request->width) {
		a.request_mode |= CWWidth;
		a.width = request->width;
		good++;
	    }
	}
	if (Wants(CWHeight)) {
	    ask++;
	    if ((mr.request_mode & CWHeight) && mr.height == request->height) {
		a.request_mode |= CWHeight;
		a.height = request->height;
		good++;
	    }
	}

	if (reply != NULL)
		*reply = mr;

	if (good == ask) {
		XdbDebug2(__FILE__, (Widget)f, w, "GeometryManager => YES\n");
	/* In the case where the request is granted, we may have to resize form itself */
		if (fg.request_mode & (CWWidth | CWHeight)) {
			/* Must resize form */
			XtGeometryResult r;
			r = XtMakeGeometryRequest((Widget)f, &fg, &fg);
			if (r == XtGeometryYes || r == XtGeometryDone) {
				XtWidth(f) = fg.width;
				XtHeight(f) = fg.height;

				XmFormLayout(f, Mode_Normal, w, &mr, NULL);

				return XtGeometryYes;
			}
			return XtGeometryNo;	/* Form couldn't be resized */
		}
		return XtGeometryYes;		/* Didn't need to resize Form */
	}
	if (good == 0) {
		XdbDebug2(__FILE__, (Widget)f, w, "GeometryManager => NO\n");
		return XtGeometryNo;
	}

	XdbDebug2(__FILE__, (Widget)f, w, "GeometryManager => Almost (Req %d %d, Reply %d %d)\n",
		request->width, request->height, reply->width, reply->height);

	return XtGeometryAlmost;
#undef        Wants
}


/* define the maximum iterations of the layout routine to
   be the same as Motif's */

#if 0
/* Motif says 10000 here */
#define MAX_ITERATIONS 10000
#else
#define	MAX_ITERATIONS	200
#endif

/*
 * Find Preferred Geometry for the form's children
 *
 * Note: currently we're not called efficiently. We should find out when exactly
 *	we need to update this information !
 * Doing all this over and over again is useless and sloooow.
 *
 * FIX ME
 */
static void
XmFormFindPreferred(XmFormWidget f)
{
    int		i;
    Boolean	changed;	/* is needed because of SETX macros */

    for (i=0; i<f->composite.num_children; i++) {
	XtWidgetGeometry        pref;
	Widget			child = f->composite.children[i];
	XmFormConstraints	constraints = (XmFormConstraints)child->core.constraints;

	SETX(XtX(child));
	SETY(XtY(child));
	SETW(XtWidth(child));
	SETH(XtHeight(child));

/* Sanity check */
#if	1
	if (XtWidth(child) > 32767)
		SETW(100);
	if (XtHeight(child) > 32767)
		SETH(100);
#endif

	if (! XtIsManaged(f->composite.children[i]))
	    continue;

	if (XtQueryGeometry(f->composite.children[i], NULL, &pref) != XtGeometryYes) {
#ifdef	PRINT_PREFERRED
	    XdbDebug2(__FILE__, f, child, "Preferred w %d h %d\n", pref.width, pref.height);
#endif
	    if (pref.request_mode & CWWidth) {
		SETW(pref.width);
	    }
	    if (pref.request_mode & CWHeight) {
		SETH(pref.height);
	    }
	    if (pref.request_mode & CWX)	SETX(pref.x);
	    if (pref.request_mode & CWY)	SETY(pref.y);
	} else {
#ifdef	PRINT_PREFERRED
	    XdbDebug2(__FILE__, f, child, "XmFormFindPreferred : no preferences\n");
#endif
	}
    }
}

void
XmFormPrintAttachmentReport(XmFormWidget f)
{
    int	i;

    XdbDebug(__FILE__, (Widget)f, "Attachment Report : (Top,Bottom,Left,Right)\n");

    for (i=0; i<f->composite.num_children; i++) {
	Widget			child = f->composite.children[i];
	XmFormConstraints	constraints = (XmFormConstraints)child->core.constraints;

	XdbDebug0(__FILE__, (Widget)f,  "child %s\t\t", XtName(child));

#define	PRINTIT(x)								\
	if (CON.atta[x].type == XmATTACH_WIDGET ||				\
			CON.atta[x].type == XmATTACH_OPPOSITE_WIDGET) {		\
		if (CON.atta[x].w == NULL)					\
		    XdbDebug0(__FILE__, (Widget)f,  "%s(%s)\t",			\
			XdbAttachment2String(CON.atta[x].type),			\
			"(null)");						\
		else								\
		    XdbDebug0(__FILE__, (Widget)f,  "%s(%s,%d)\t",		\
			XdbAttachment2String(CON.atta[x].type),			\
			XtName(CON.atta[x].w),					\
			CON.atta[x].offset);					\
	} else if (CON.atta[x].type == XmATTACH_POSITION)			\
		XdbDebug0(__FILE__, (Widget)f, "%s(%d/%d)\t",			\
			XdbAttachment2String(CON.atta[x].type),			\
			CON.atta[x].value, FFB);				\
	else									\
		XdbDebug0(__FILE__, (Widget)f,  "%s(%d)\t",			\
			XdbAttachment2String(CON.atta[x].type),			\
			CON.atta[x].offset);

	PRINTIT(TOP);
	PRINTIT(BOTTOM);
	XdbDebug0(__FILE__, (Widget)f,  "\n\t\t\t\t");
	PRINTIT(LEFT);
	PRINTIT(RIGHT);

	XdbDebug0(__FILE__, (Widget)f,  "\n");
#undef	PRINTIT
    }
}

/*
 * XmFormPath
 *
 * Try to find children whose ATTACH_WIDGET properties give you a path from
 * one side of the Form to the other.
 *
 * Return the size of the Form in that direction.
 */
static Dimension
XmFormPath(XmFormWidget f, Widget c, int dir)
{
    int	i;
    Dimension	mx, x;
    int		other = OPPOSITE_DIRECTION(dir);

    if (dir == LEFT || dir == RIGHT)
	mx = XtWidth(c) + BB_MarginWidth(f);
    else
	mx = XtHeight(c) + BB_MarginHeight(f);

    for (i=0; i<f->composite.num_children; i++) {
	Widget			child = f->composite.children[i];
	XmFormConstraints	constraints = (XmFormConstraints)child->core.constraints;

	if (constraints->form.atta[other].type == XmATTACH_WIDGET &&
		constraints->form.atta[other].w == c)
	    {
		x = XmFormPath(f, child, dir)
			+ constraints->form.atta[other].offset;
		if (dir == LEFT || dir == RIGHT)
			x += XtWidth(c);
		else
			x += XtHeight(c);

		if (x > mx)
			mx = x;
	    }
    }

    return mx;
}

static void
XmFormAllPaths(XmFormWidget f, Dimension *ww, Dimension *hh)
{
	int	i;
	Dimension	x, mx;

	*ww = *hh = 0;

	mx = 0;
	for (i=0; i<MGR_NumChildren(f); i++) {
	    x = XmFormPath(f, MGR_Children(f)[i], LEFT);
	    if (x > mx)
		mx = x;
	}
	for (i=0; i<MGR_NumChildren(f); i++) {
	    x = XmFormPath(f, MGR_Children(f)[i], RIGHT);
	    if (x > mx)
		mx = x;
	}

	mx += BB_MarginWidth(f);
	*ww = mx;

	mx = 0;
	for (i=0; i<MGR_NumChildren(f); i++) {
	    x = XmFormPath(f, MGR_Children(f)[i], TOP);
	    if (x > mx)
		mx = x;
	}
	for (i=0; i<MGR_NumChildren(f); i++) {
	    x = XmFormPath(f, MGR_Children(f)[i], BOTTOM);
	    if (x > mx)
		mx = x;
	}

	mx += BB_MarginHeight(f);
	*hh = mx;
}

/*
 * Find out if the widget is attached to Form, more than one
 * step away in a given direction.
 *
 * Simplistic case from which this started :
 *	W_ATT(C_RIGHT.w, RIGHT) == XmATTACH_FORM
 * The loop in this function makes the difference ...
 */
static Boolean
AttachedToForm(Widget f, Widget w, int dir)
{
	while (w && W_ATT(w, dir) == XmATTACH_WIDGET)
		w = W_WIDGET(w, dir);

	if (!w) {
		_XmWarning(f, "AttachedToForm: this should never happen\n");
		return False;	/* Should never happen */
	}

	if (W_ATT(w, dir) == XmATTACH_FORM)
		return True;
#if 1
	/* Not sure about these */
	if (W_ATT(w, dir) == XmATTACH_POSITION)
		return True;
	if (W_ATT(w, dir) == XmATTACH_SELF)
		return True;
#endif
	return False;
}

/*
 * XmFormLayout() - run the XmForm layout algorithm
 *
 * The MODE parameter is a bit set composed from the following bits :
 *	#define	Mode_Normal	0x00	(no bits set)
 *	#define	Mode_Test	0x01	(don't actually change anything)
 *	#define	Mode_Resize	0x02	(can resize ourselves if == 0)
 *
 * If Mode_Test is set, then the cw and cg parameters must be valid, and the
 *	cw widget must be a managed child of form.
 *
 * Mode_Test is used by GeometryManager to see whether a geometry change is ok.
 *	In this case, a widget and its proposed geometry are passed.
 *	XmFormLayout will apply the changes temporarily, and after the layout
 *	algorithm it'll return the resulting geometry of that widget.
 *	GeometryManager will draw its own conclusions.
 *
 * Mode_Test can also be used with the form itself as widget being observed.
 *
 * ParentChangeMode (mode & Mode_Resize) :
 *	Allow XmFormLayout to change the form's geometry.
 *	Is set depending on the method from which this function is called.
 *	(Mode_Resize is set when called from the widget's resize method.)
 *
 * The FG parameter is used to indicate form geometry changes when called from
 *	GeometryManageer.
 *
 * For this routine, the fields x, y, w, h were added to the constraint record, in FormP.h.
 *	Why ? In Mode_Test, the full algorithm must work (making changes to children
 *	widgets and their geometry); however nothing can be actually changed.
 *	The working variables for XmFormLayout are in the constraint record.
 *
 * 31/7/1996 addition : There are circumstances in which the geometry of the CW widget 
 *	should not be changed. Allowing all changes would create potential infinite loop
 *	situations. This needs to be added. Without this change, the use of e.g. editres
 *	to decrease the size of a widget can induce an infinite loop. (If we didn't have
 *	MAX_ITERATIONS, of course).
 * FIX ME.
 *
 * 1/8/1996 addition : cw and cg should be taken into account even when not in test mode.
 */
static void
XmFormLayout(XmFormWidget f, int mode, Widget cw, XtWidgetGeometry *cg, XtWidgetGeometry *fg)
{
    int		number_of_iterations = 0;
    Boolean	changed;
    int		i, this_child = 0;
    Dimension	fw, fh;				/* Form dimensions */
    Boolean	ParentChangeMode = ((mode & Mode_Resize) == 0),
		TestMode = ((mode & Mode_Test) == Mode_Test);

#define	Wants(x)	(cg->request_mode & x)

    XdbDebug(__FILE__, (Widget)f, "XmFormLayout - width %d height %d\n",
		XtWidth(f), XtHeight(f));

/* Special case ... should this be a special case (maybe our code below is wrong !) */
/* What if there are no children ? */
    if (MGR_NumChildren(f) == 0 && XtIsRealized((Widget)f)) {
	if (! TestMode) {
		XtWidgetGeometry	geo;
		geo.request_mode = CWWidth|CWHeight;
		geo.width = geo.height = 1;
		_XmMakeGeometryRequest((Widget)f, &geo);
	}
	if (fg) {
		fg->request_mode = CWWidth|CWHeight;
		fg->width = fg->height = 1;
	}
	return;
    }
/* End special case */

    fw = XtWidth(f);
    fh = XtHeight(f);

/* Find all preferred sizes */
    XmFormFindPreferred(f);

/* Check whether we should test geometry change for form itself ! */
    if (TestMode && cw == (Widget)f) {
	if (Wants(CWWidth))	fw = cg->width;
	if (Wants(CWHeight))	fh = cg->height;

	if (XdbInDebug(__FILE__, (Widget)f)) {
	    XdbDebug(__FILE__, (Widget)f, "XmFormLayout TestMode ");
	    if (Wants(CWWidth)) XdbDebug0(__FILE__, (Widget)f, "width %d ", fw);
	    if (Wants(CWHeight)) XdbDebug0(__FILE__, (Widget)f, "height %d ", fh);
	    XdbDebug0(__FILE__, (Widget)f, "\n");
	}
    }

#ifdef	PRINT_ATTACHMENT_REPORT
    XmFormPrintAttachmentReport(f);
#endif

#if 1
/* Hack */
    if (mode != Mode_Resize)
#endif
      for (i=0; i<f->composite.num_children; i++) {
	/* These two variables are needed to make the macros work ! */
	Widget			child = f->composite.children[i];
	XmFormConstraints	constraints = (XmFormConstraints)child->core.constraints;

	if (! XtIsManaged(f->composite.children[i]))
	    continue;

/* Initialize */
	constraints->form.width_from_side = False;
	constraints->form.height_from_side = False;

	if (cw == f->composite.children[i]) {
	    this_child = i;
	    if (TestMode)
		XdbDebug2(__FILE__, (Widget)f, cw, "Test Geometry : ");
	    else
		XdbDebug2(__FILE__, (Widget)f, cw, "Require Geometry : ");

	    if (cg->request_mode & CWX) {
		SETX(cg->x);
		XdbDebug0(__FILE__, (Widget)f,  "x %d ", cg->x);
	    }
	    if (cg->request_mode & CWY) {
		SETY(cg->y);
		XdbDebug0(__FILE__, (Widget)f,  "y %d ", cg->y);
	    }
	    if (cg->request_mode & CWWidth) {
		SETW(cg->width);
		XdbDebug0(__FILE__, (Widget)f,  "w %d ", cg->width);
	    }
	    if (cg->request_mode & CWHeight) {
		SETH(cg->height);
		XdbDebug0(__FILE__, (Widget)f,  "h %d ", cg->height);
	    }
	    XdbDebug0(__FILE__, (Widget)f,  "\n");
	}
/*
 * Every child of a form needs to have a left or a right attachment.
 * If they have neither, fix it based on XmNrubberPositioning.
 */

    if (C_LEFT.type == XmATTACH_NONE && C_RIGHT.type == XmATTACH_NONE) {
	XdbDebug2(__FILE__, (Widget)f, child, "left & right were NONE\n");
	if (f->form.rubber_positioning == True) {
	    C_LEFT.type = XmATTACH_POSITION;
	    C_LEFT.value = XtX(child);
	    C_LEFT.percent = (C_LEFT.value / f->form.fraction_base);
	    XdbDebug2(__FILE__, (Widget)f, child, "Left set to ATTACH_POSITION %d\n", C_LEFT.value);
	} else {
	    C_LEFT.type = XmATTACH_FORM;
	    C_LEFT.offset = XtX(child);
	    XdbDebug2(__FILE__, (Widget)f, child, "Left set to ATTACH_FORM %d\n", C_LEFT.offset);
	    XdbDebug2(__FILE__, (Widget)f, child, "Mode was %d\n", mode);
	}
    }

/*
 * Every child of a form needs to have a top or a bottom attachment.
 * If they have neither, fix it based on XmNrubberPositioning.
 */

    if (C_TOP.type == XmATTACH_NONE && C_BOTTOM.type == XmATTACH_NONE) {
	XdbDebug2(__FILE__, (Widget)f, child, "ConstraintInitialize : top & bottom were NONE\n");
	if (f->form.rubber_positioning == True) {
	    C_TOP.type = XmATTACH_POSITION;
	    C_TOP.value = XtY(child);
	    C_TOP.percent = (C_TOP.value / f->form.fraction_base);
	    XdbDebug2(__FILE__, (Widget)f, child, "Top set to ATTACH_POSITION %d\n", C_TOP.value);
	} else {
	    C_TOP.type = XmATTACH_FORM;
	    C_TOP.offset = XtY(child);
	    XdbDebug2(__FILE__, (Widget)f, child, "Top set to ATTACH_FORM %d\n", C_TOP.offset);
	    XdbDebug2(__FILE__, (Widget)f, child, "Mode was %d\n", mode);
	}
    }
    }

#ifdef PRINT_REPORT
    XdbDebug(__FILE__, (Widget)f, "Initial geometry : %d %d\n", FW, FH);
    XdbDebug0(__FILE__, (Widget)f,  "Initial child geometry :\n");
	for (i=0; i<f->composite.num_children; i++) {
		Widget child = f->composite.children[i];
		XmFormConstraints constraints = (XmFormConstraints)child->core.constraints;

		XdbDebug0(__FILE__, (Widget)f,  "Child #%d (%s)\tx %d y %d w %d h %d\n",
			i, XtName(child), CX, CY, CW, CH);
	}
#endif

/*
 * In these two nested loops we repeatedly try to fix all geometries for all
 * children, and for the form itself.
 *
 * The outer loop runs until either nothing has changed, or MAX_ITERATIONS times.
 *
 * The inner loop goes over all (managed) children.
 */
    do {
	changed = False;
/*
 * Inner loop
 */

/* Tryout */
	if (ParentChangeMode) {
		Dimension	ww, hh;

		XmFormAllPaths(f, &ww, &hh);

		if (FW < ww) {
			fw = ww;
			changed = True;
		}
		if (FH < hh) {
			fh = hh;
			changed = True;
		}
	}

/* End tryout */

	for (i=0; i<f->composite.num_children; i++) {
	    Widget		child = f->composite.children[i];
	    XmFormConstraints	constraints = (XmFormConstraints)child->core.constraints;

	    if (!XtIsManaged(child))
		continue;

/*
 * First use a couple of rules to try to fix form's size.
 *	Some of these are special cases which should be implemented in the rest
 *	of the geometry layout algorithm. FIX ME !
 */
	    if (ParentChangeMode &&
		(BB_ResizePolicy(f) == XmRESIZE_GROW
		 || BB_ResizePolicy(f) == XmRESIZE_ANY)) {
/* Form width */
		if (! constraints->form.width_from_side) {
		    if (C_RIGHT.type == XmATTACH_FORM
			&& FW < CX + CW + C_RIGHT.offset + FMW) {
			SETFW(CX + CW + C_RIGHT.offset + FMW);
		    } else if (FW < CX + CW + FMW) {
			SETFW(CX + CW + FMW);
		    }
		}
/* Form height */
		if (FH < CY + CH + FMH) {
		    if (! constraints->form.height_from_side)
			SETFH(CY + CH + FMH);
		}
/* Two POSITION attachments */	/* FIX ME */
		if (C_TOP.type == XmATTACH_POSITION && C_BOTTOM.type == XmATTACH_POSITION
			&& C_TOP.value != C_BOTTOM.value) {
		    if (FH < (int)(CH * FFB / (C_BOTTOM.value - C_TOP.value)))
			SETFH(CH * FFB / (C_BOTTOM.value - C_TOP.value));
		}
/* Two POSITION attachments */	/* FIX ME */
		if (C_LEFT.type == XmATTACH_POSITION && C_RIGHT.type == XmATTACH_POSITION
			&& C_LEFT.value != C_RIGHT.value) {
		    if (FW < (int)(CW * FFB / (C_RIGHT.value - C_LEFT.value)))
			SETFW(CW * FFB / (C_RIGHT.value - C_LEFT.value));
		}
	    }

/*
 * Now treat the attachments
 *
 * We see more or less the same code four times (TOP, BOTTOM, LEFT, RIGHT) here.
 *	Not sure whether it is at all possible to generalise the code
 *	such that we could write this in a shorter way though.
 */
	    switch (C_TOP.type)
	    {
/* TOP */    case XmATTACH_FORM:
		if (CY != C_TOP.offset + FMH) {
		    SETY(C_TOP.offset + FMH);
		}
		break;
/* TOP */    case XmATTACH_OPPOSITE_FORM:
		if (CY != FH + C_TOP.offset - FMH) {
		    SETY(FH + C_TOP.offset - FMH);
		}
		break;
/* TOP */    case XmATTACH_WIDGET:
		if (! (C_TOP.w)) {
		    _XmWarning(child, "top: XmATTACH_WIDGET without widget");
#ifdef	NO_WIDGET_BECOMES_FORM
		    C_TOP.type = XmATTACH_FORM;
#else
		    C_TOP.type = XmATTACH_NONE;
#endif
		    changed = True;
		    break;
		}
		if (CY != (W_Y(C_TOP.w) + W_H(C_TOP.w) + C_TOP.offset)) {
		    int yy = W_Y(C_TOP.w) + W_H(C_TOP.w) + C_TOP.offset;
		    if (yy < 0) {
			yy = CY - W_H(C_TOP.w) - C_TOP.offset;
			if (yy < 0) {
			    SETWH(C_TOP, CY - W_Y(C_TOP.w) - C_TOP.offset);
			} else {
			    SETWY(C_TOP, yy);
			}
		    } else {
			SETY(yy);
		    }
		}
		break;
/* TOP */    case XmATTACH_OPPOSITE_WIDGET:
		if (! (C_TOP.w)) {
		    _XmWarning(child, "top: XmATTACH_OPPOSITE_WIDGET without widget");
#ifdef	NO_WIDGET_BECOMES_FORM
		    C_TOP.type = XmATTACH_FORM;
#else
		    C_TOP.type = XmATTACH_NONE;
#endif
		    changed = True;
		    break;
		}
		if (CY != (W_Y(C_TOP.w) + C_TOP.offset)) {
		    int	yy = W_Y(C_TOP.w) + C_TOP.offset;

		    if (yy < 0) {
			SETWY(C_TOP, CY - C_TOP.offset);
		    } else
			SETY(W_Y(C_TOP.w) + C_TOP.offset);
		}
		break;
/* TOP */    case XmATTACH_POSITION:
		if (CY != C_TOP.value * FH / FFB) {
		    SETY(C_TOP.value * FH / FFB);
		}
		break;
/* TOP */   case XmATTACH_NONE:
		break;
/* TOP */   case XmATTACH_SELF:
		if (C_TOP.offset != CY) {
			SETY(C_TOP.offset);
		}
		break;
/* TOP */   default:
		_XmWarning(child, "Illegal top attachment");
	    }

	    switch (C_BOTTOM.type)
	    {
/* BOTTOM */    case XmATTACH_FORM:
		if (FH == 0) {
		    FH = CH + CY + C_BOTTOM.offset + FMH;
		} else if (CH != FH - CY - C_BOTTOM.offset - FMH) {
		    if (C_TOP.type == XmATTACH_NONE) {
			if (FH < C_BOTTOM.offset + CH + FMH) {
				/* ??? FIX ME */
			} else {
			    SETY(FH - C_BOTTOM.offset - CH - FMH);
			}
		    } else {
			SETH(FH - CY - C_BOTTOM.offset - FMH);
			constraints->form.height_from_side = True;
		    }
		}
		break;
/* BOTTOM */    case XmATTACH_OPPOSITE_FORM:
		if (CH + CY != FMH - C_BOTTOM.offset) {
		    if (C_TOP.type == XmATTACH_NONE) {
			SETY(FMH - C_BOTTOM.offset - CH);
		    } else {
			SETH(FMH - CY - C_BOTTOM.offset);
			constraints->form.height_from_side = True;
		    }
		}
		break;
/* BOTTOM */    case XmATTACH_WIDGET:
		if (! (C_BOTTOM.w)) {
		    _XmWarning(child, "bottom: XmATTACH_WIDGET without widget");

#ifdef	NO_WIDGET_BECOMES_FORM
		    C_BOTTOM.type = XmATTACH_FORM;
#else
		    C_BOTTOM.type = XmATTACH_NONE;
#endif
		    changed = True;
		    break;
		}
		if (CY != (W_Y(C_BOTTOM.w) - CH - C_BOTTOM.offset)) {
		    if (C_TOP.type == XmATTACH_NONE) {
			if (W_Y(C_BOTTOM.w) < CH + C_BOTTOM.offset) {
				/* FIX ME */
			} else {
			    SETY(W_Y(C_BOTTOM.w) - CH - C_BOTTOM.offset);
			}
		    } else {
			int hh = W_Y(C_BOTTOM.w) - CY - C_BOTTOM.offset;

			if (hh < 0) {
				/* Force the other widget away */
#ifdef	notdef
				W_Y(C_BOTTOM.w) = CY + CH + C_BOTTOM.offset;
				changed = True;
				XdbDebug(__FILE__, (Widget)f, "Child %s forced down to %d by child %s\n",
					XtName(C_BOTTOM.w), W_Y(C_BOTTOM.w), XtName(child));
#else
				SETWY(C_BOTTOM, CY + CH + C_BOTTOM.offset);
#endif
			} else
			    SETH(hh);
			constraints->form.height_from_side = True;
		    }
		}
		break;
/* BOTTOM */    case XmATTACH_POSITION:		/* FIX ME FMH */
		if (CH + CY != C_BOTTOM.value * FH / FFB) {
		    if (C_TOP.type == XmATTACH_NONE) {
			SETY(C_BOTTOM.value * FH / FFB - CH);
		    } else {
			SETH(C_BOTTOM.value * FH / FFB - CY);
			constraints->form.height_from_side = True;
		    }
		}
		break;
/* BOTTOM */    case XmATTACH_NONE:
		break;
/* BOTTOM */    case XmATTACH_OPPOSITE_WIDGET:
		if (! (C_BOTTOM.w)) {
		    _XmWarning(child, "bottom: XmATTACH_OPPOSITE_WIDGET without widget");

#ifdef	NO_WIDGET_BECOMES_FORM
		    C_BOTTOM.type = XmATTACH_FORM;
#else
		    C_BOTTOM.type = XmATTACH_NONE;
#endif
		    changed = True;
		    break;
		}
		if (CY + CH != (W_Y(C_BOTTOM.w) + W_H(C_BOTTOM.w) - C_BOTTOM.offset)) {
		    if (C_TOP.type == XmATTACH_NONE) {
			SETY(W_Y(C_BOTTOM.w) + W_H(C_BOTTOM.w) - CH - C_BOTTOM.offset);
		    } else {
			SETH(W_Y(C_BOTTOM.w) + W_H(C_BOTTOM.w) - CY - C_BOTTOM.offset);
			constraints->form.height_from_side = True;
		    }
		}
		break;
/* BOTTOM */    case XmATTACH_SELF:
		if (FH != C_BOTTOM.offset + CH)
		    SETH(FH - C_BOTTOM.offset - CH);
		break;
/* Now for the illegal cases */
	    default:
		_XmWarning(child, "Illegal bottom attachment");
	    }

	    switch (C_LEFT.type)
	    {
/* LEFT */    case XmATTACH_FORM:
		if (CX != C_LEFT.offset + FMW) {
		    SETX(C_LEFT.offset + FMW);
		}
		break;
/* LEFT */    case XmATTACH_POSITION:
		if (CX != C_LEFT.value * FW / FFB) {
		    SETX(C_LEFT.value * FW / FFB);
		}
		break;
/* LEFT */    case XmATTACH_WIDGET:
		if (! (C_LEFT.w)) {
		    _XmWarning(child, "left: XmATTACH_WIDGET without widget");

#ifdef	NO_WIDGET_BECOMES_FORM
		    C_LEFT.type = XmATTACH_FORM;
#else
		    C_LEFT.type = XmATTACH_NONE;
#endif
		    changed = True;
		    break;
		}
		if (CX != (W_X(C_LEFT.w) + W_W(C_LEFT.w) + C_LEFT.offset)) {
		    SETX(W_X(C_LEFT.w) + W_W(C_LEFT.w) + C_LEFT.offset);
		}
		break;
/* LEFT */    case XmATTACH_OPPOSITE_WIDGET:
		if (! (C_LEFT.w)) {
		    _XmWarning(child, "left: XmATTACH_OPPOSITE_WIDGET without widget");

#ifdef	NO_WIDGET_BECOMES_FORM
		    C_LEFT.type = XmATTACH_FORM;
#else
		    C_LEFT.type = XmATTACH_NONE;
#endif
		    changed = True;
		    break;
		}
		if (XtParent(C_LEFT.w) != (Widget)f) {
		    XdbDebug2(__FILE__, (Widget)f, child, "Attachments should be to children of Form.\n");
		    C_LEFT.type = XmATTACH_NONE;
		    changed = True;
		    break;
		}
		if (CX != C_LEFT.offset + W_X(C_LEFT.w)) {
		    SETX(C_LEFT.offset + W_X(C_LEFT.w));
		} 
		break;
/* LEFT */    case XmATTACH_NONE:
		break;
/* LEFT */    case XmATTACH_OPPOSITE_FORM:
		if (CX + C_LEFT.offset + FMW != FW) {
		    SETX(FW - C_LEFT.offset - FMW);
		}
		break;
/* LEFT */    case XmATTACH_SELF:
		if (CX != C_LEFT.offset)
			SETX(C_LEFT.offset);
		break;
/* Now for the illegal cases */
	    default:
		_XmWarning(child, "Illegal left attachment");
	    }

	    switch (C_RIGHT.type)
	    {
/* RIGHT */    case XmATTACH_FORM:
		if (FW == 0) {
		    FW = CW + CX + C_RIGHT.offset + FMW;
		} else if (CW != FW - CX - C_RIGHT.offset - FMW) {
		    if (C_LEFT.type == XmATTACH_NONE) {
			SETX(FW - CW - C_RIGHT.offset - FMW);
		    } else {
			SETW(FW - CX - C_RIGHT.offset - FMW);
			constraints->form.width_from_side = True;
		    }
		}
		break;
/* RIGHT */    case XmATTACH_WIDGET:
		if (! (C_RIGHT.w)) {
		    _XmWarning(child, "right: XmATTACH_WIDGET without widget");

#ifdef	NO_WIDGET_BECOMES_FORM
		    C_RIGHT.type = XmATTACH_FORM;
#else
		    C_RIGHT.type = XmATTACH_NONE;
#endif
		    changed = True;
		    break;
		}
		if (CW != (W_X(C_RIGHT.w) - CX - C_RIGHT.offset)) {
		    if (C_LEFT.type == XmATTACH_NONE) {
			SETX(W_X(C_RIGHT.w) - CW - C_RIGHT.offset);
		    } else if (CW < (W_X(C_RIGHT.w) - CX - C_RIGHT.offset)) {
/* Ad hoc : we'll try to grow but not shrink - FIX ME  - Danny 28/12/96 */
			SETW(W_X(C_RIGHT.w) - CX - C_RIGHT.offset);
			constraints->form.width_from_side = True;
/* Danny 27/12/1996: not sure about condition below */
		    } else if (child == cw || cw == NULL) {
			if (AttachedToForm((Widget)f, C_RIGHT.w, RIGHT)) {
			    if (FW < CX + CW + C_RIGHT.offset
				+ W_W(C_RIGHT.w) + W_OFFSET(C_RIGHT.w, RIGHT)) {
				SETFW(CX + CW + C_RIGHT.offset
				    + W_W(C_RIGHT.w) + W_OFFSET(C_RIGHT.w, RIGHT));
			    } else {
				/* Do nothing here ? FIX ME */
			    }
			} else {
			    SETWX(C_RIGHT, CW + CX + C_RIGHT.offset);
			}
		    } else {
			/* Who knows ? FIX ME */
			/* Shrink anyway :-( */
			SETW(W_X(C_RIGHT.w) - CX - C_RIGHT.offset);
			constraints->form.width_from_side = True;
		    }
		}
		break;
/* RIGHT */    case XmATTACH_OPPOSITE_WIDGET:
		if (! (C_RIGHT.w)) {
		    _XmWarning(child, "right : XmATTACH_OPPOSITE_WIDGET without widget");

#ifdef	NO_WIDGET_BECOMES_FORM
		    C_RIGHT.type = XmATTACH_FORM;
#else
		    C_RIGHT.type = XmATTACH_NONE;
#endif
		    changed = True;
		    break;
		}
		if (W_X(C_RIGHT.w) + W_W(C_RIGHT.w) !=
				CW + CX + C_RIGHT.offset) {
		    if (C_LEFT.type == XmATTACH_NONE) {
			SETX(W_X(C_RIGHT.w) + W_W(C_RIGHT.w)
				- CW - C_RIGHT.offset);
		    } else {
			SETW(W_X(C_RIGHT.w) + W_W(C_RIGHT.w)
				- CX - C_RIGHT.offset);
			constraints->form.width_from_side = True;
		    }
		}
		break;
/* RIGHT */    case XmATTACH_POSITION:
		if (CX + CW != C_RIGHT.value * FW / FFB) {
		    if (C_LEFT.type == XmATTACH_NONE) {
			SETX(C_RIGHT.value * FW / FFB - CW);
		    } else {
			SETW(C_RIGHT.value * FW / FFB - CX);
			constraints->form.width_from_side = True;
		    }
		}
		break;
	    case XmATTACH_NONE:
		break;
/* RIGHT */ case XmATTACH_OPPOSITE_FORM:			/* FMW FIX ME */
		if (CX + CW != - C_RIGHT.offset) {
		    if (C_LEFT.type == XmATTACH_NONE) {
			SETX(0 - C_RIGHT.offset - CW);
		    } else {
			SETW(0 - C_RIGHT.offset - CX);
			constraints->form.width_from_side = True;
		    }
		}
		break;
/* RIGHT */ case XmATTACH_SELF:
		if (FW != CX + CW + C_RIGHT.offset)
			SETW(FW - CW - C_RIGHT.offset - CX);	/* FIX ME */
		break;
/* Now for the illegal cases */
	    default:
		_XmWarning(child, "Illegal right attachment\n");
	    }

/*
 * End of the two nested loops
 */
	}	/* End of the loop over all children */
	number_of_iterations ++;
    } while (changed && number_of_iterations < MAX_ITERATIONS);

/*
 * After loops ...
 *
 * Print a warning if necessary.
 *
 * Then depending on our parameters either copy back information,
 * or try to modify the widgets involved.
 */
    if (number_of_iterations == MAX_ITERATIONS) {
	_XmWarning((Widget)f,
		"Layout algorithm bailing out after %d iterations",
		MAX_ITERATIONS);
    }

#ifdef	PRINT_REPORT
    XdbDebug(__FILE__, (Widget)f, "XmFormLayout results : form size %d %d\n", FW, FH);
    for (i=0; i<f->composite.num_children; i++) {
	Widget child = f->composite.children[i];
	XmFormConstraints constraints = (XmFormConstraints)child->core.constraints;

	XdbDebug0(__FILE__, (Widget)f,  "\tChild #%d (%s)\tx %d y %d w %d h %d\n",
		i, XtName(child), CX, CY, CW, CH);
    }
#endif

/* Report Form geometry */
    if (fg) {
	fg->request_mode = CWWidth | CWHeight;
	fg->width = FW;
	fg->height = FH;
    }

/* If in test mode for a child widget, copy back the answer */
    if (TestMode) {
	if (cw != (Widget)f) {
	    XmFormConstraints constraints =
		(XmFormConstraints)f->composite.children[this_child]->core.constraints;

	    cg->x = constraints->form.x;
	    cg->y = constraints->form.y;
	    cg->width = constraints->form.w;
	    cg->height = constraints->form.h;
	    cg->request_mode = CWX | CWY | CWWidth | CWHeight;
	} else { /* TestMode for form itself */
	    cg->width = fw;
	    cg->height = fh;
	    cg->request_mode = CWWidth | CWHeight;
	} 
    } else {	/* If not in test mode, change all widget geometries */
	XtWidgetGeometry	request;
	XtGeometryResult	result;

	/* Children first */
	for (i=0; i<f->composite.num_children; i++) {
	    Widget child = f->composite.children[i];
	    XmFormConstraints constraints =
			(XmFormConstraints)child->core.constraints;

	    _XmConfigureObject(child,
		constraints->form.x,
		constraints->form.y,
		constraints->form.w <= 0 ? 1 : constraints->form.w,
		constraints->form.h <= 0 ? 1 : constraints->form.h,
		child->core.border_width);
	}

	/* Myself */
	request.request_mode = CWWidth | CWHeight;
	request.width = FW;
	request.height = FH;

	if (BB_ResizePolicy(f) == XmRESIZE_ANY) {
	    if (FW != XtWidth(f) || FH != XtHeight(f)) {
		request.width = FW;
		request.height = FH;
	    }
	} else if (BB_ResizePolicy(f) == XmRESIZE_GROW) {
	    if (FW != XtWidth(f))
		request.width = FW;
	    if (FH != XtHeight(f))
		request.height = FH;
	}


/*
 * MLM: 960403 the if statement below keeps the form from making resize
 * requests before it is realized, when that path is followed: the
 * second test below is the reason why.  This may have been necessary
 * when objects were capricious about how and when they set their
 * size requirements -- that's mostly no longer valid (see form/test4
 * for an example of one - my ScrolledWindow:().
 */
#if 0
	if (! XtIsRealized((Widget)f)) {
		if (XtWidth(f) != 0) request.width = XtWidth(f);
		if (XtHeight(f) != 0) request.height = XtHeight(f);
	}
#endif
	if (XtWidth(f) != request.width || XtHeight(f) != request.height) {
	    do {
		result = XtMakeGeometryRequest((Widget)f, &request, &request);

		XdbDebug(__FILE__, (Widget)f, "XtMakeGeometryRequest (w %d h %d) => %s\n",
			request.width, request.height,
			XdbGeometryResult2String(result));
	    } while (result == XtGeometryAlmost);
	}
    }
#undef	Wants
}

/*
 * ChangeManaged method
 *
 * Called when adding a managed child, or by managing/unmanaging an
 *	existing child.
 *
 * Before Realize, ChangeManaged is called only once (just before realize).
 *	At that time, we should do the layout algorithm to determine the
 *	form's initial size depending on the children.
 */
static void
ChangeManaged(Widget w)
{
	XmFormWidget	f = (XmFormWidget) w;

/*
 * Must also redo layout when not realized to make up initial
 * sizing calculations
 */
#ifdef notdef
        if (! XtIsRealized(w)) {
                XdbDebug(__FILE__, (Widget)f, "ChangeManaged: not realized\n");
                return;
        }
#endif

        XdbDebug(__FILE__, (Widget)f, "ChangeManaged\n");
        XmFormLayout(f, Mode_Normal, NULL, NULL, NULL);

/* FIX ME */
	_XmNavigChangeManaged(w);
}

static void
FormInsertChild(Widget w)
{
    Widget f = XtParent(w);

    XdbDebug2(__FILE__, (Widget)f, w, "InsertChild\n");

#define superclass (&xmBulletinBoardClassRec)
    (*superclass->composite_class.insert_child)(w);
#undef superclass
}

Widget 
XmCreateForm(Widget parent,
	     char *name,
	     Arg *arglist,
	     Cardinal argCount)
{
    return XtCreateWidget(name, xmFormWidgetClass, parent, arglist, argCount);
}

Widget 
XmCreateFormDialog(Widget parent,
		   char *name,
		   Arg *arglist,
		   Cardinal argcount)
{
    Widget	shell, r;
    char	*shell_name;
    int		ac, i;
    Arg		*al = (Arg *)XtCalloc(argcount + 1, sizeof(Arg));

    ac=0;
    XtSetArg(al[ac], XmNallowShellResize, True); ac++;
    for (i=0; i<argcount; i++) {
	XtSetArg(al[ac], arglist[i].name, arglist[i].value); ac++;
    }

    shell_name = _XmMakeDialogName(name);
    shell = XmCreateDialogShell(parent, shell_name, al, ac);
    XtFree(shell_name);

    r =  XmCreateForm(shell, name, al, ac);
    XtFree((XtPointer)al);

    return r;
}
