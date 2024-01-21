/**
 *
 * $Id: ResInd.c,v 1.9 1996/11/28 09:21:45 u27113 Exp $
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
/**
 *
 * This file contains functions relating to resolution independent
 * dimension control in Motif/LessTif
 *
 **/

static char rcsid[] = "$Id: ResInd.c,v 1.9 1996/11/28 09:21:45 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <X11/Xfuncs.h>
#include <Xm/ManagerP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/GadgetP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/VendorSEP.h>

#include <XmI/DebugUtil.h>

#define MMsPerInch		(25.4)
#define InchesPerMM		(1.0 / MMsPerInch)
#define MMsPerPoint		(MMsPerInch / 72.0)
#define PointsPerMM		(72.0 / MMsPerInch)
#define InchesPerPoint		(1.0 / 72.0)
#define PointsPerInch		(72.0)

#ifdef __cplusplus
extern "C" {
#endif

#if XtSpecificationRelease >= 6
extern void _XtCopyToArg(char *, XtArgVal **, int);
#else
extern void _XtCopyToArg(char *, XtArgVal *, int);
#endif
extern void _XtCopyFromArg(XtArgVal, char *, int);

#ifdef __cplusplus
}
#endif

void 
_XmFromHorizontalPixels(Widget widget, 
			int offset, 
			XtArgVal *value)
{
    unsigned char unitType = _XmGetUnitType(widget);
    int converted_value;

    converted_value = XmConvertUnits(widget,
				     XmHORIZONTAL,
				     XmPIXELS,
				     *value,
				     unitType);

    *value = converted_value;
}

/* Motif 2.* version of the above */
void 
XmeFromHorizontalPixels(Widget widget, 
			int offset, 
			XtArgVal *value)
{
	_XmFromHorizontalPixels(widget, offset, value);
}

void 
_XmFromVerticalPixels(Widget widget, 
		      int offset, 
		      XtArgVal *value)
{
    unsigned char unitType = _XmGetUnitType(widget);
    int converted_value;

    converted_value = XmConvertUnits(widget,
				     XmVERTICAL,
				     XmPIXELS,
				     *value,
				     unitType);

    *value = converted_value;
}

/* Motif 2.* version of the above */
void 
XmeFromVerticalPixels(Widget widget, 
		      int offset, 
		      XtArgVal *value)
{
	_XmFromVerticalPixels(widget, offset, value);
}

XmImportOperator
_XmToHorizontalPixels(Widget widget, 
		      int offset, 
		      XtArgVal *value)
{
    unsigned char unitType = _XmGetUnitType(widget);
    int converted_value;

    converted_value = XmConvertUnits(widget,
				     XmHORIZONTAL,
				     unitType,
				     *value,
				     XmPIXELS);

    *value = converted_value;
    return XmSYNTHETIC_LOAD;
}

/* Motif 2.* version of the above */
XmImportOperator
XmeToHorizontalPixels(Widget widget, 
		      int offset, 
		      XtArgVal *value)
{
	return _XmToHorizontalPixels(widget, offset, value);
}

XmImportOperator
_XmToVerticalPixels(Widget widget, 
		    int offset, 
		    XtArgVal *value)
{
    unsigned char unitType = _XmGetUnitType(widget);
    int converted_value;

    converted_value = XmConvertUnits(widget,
				     XmVERTICAL,
				     unitType,
				     *value,
				     XmPIXELS);

    *value = converted_value;
    return XmSYNTHETIC_LOAD;
}

/* Motif 2.* version of the above */
XmImportOperator
XmeToVerticalPixels(Widget widget, 
		    int offset, 
		    XtArgVal *value)
{
	return _XmToVerticalPixels(widget, offset, value);
}

/*
 * this function _does_ appear to combine all the synthetic resources an
 * object has with the synthetic resources the parent classes have.  Check
 * out testXm/misc/test6.c and testXm/misc/mot.compsyn.  Closer examinination
 * of mot.compsyn will show that duplicate synthetics are not found, even
 * when a subclass has the same synthetic as a superclass (e.g., Form
 * and BulletinBoard.  My assumption is that the subclass synthetic takes
 * precedence over the superclass, as that's the class initialization order,
 * and that's what Xt does.  An interesting experiment is to subclass from
 * a widget that has synthetic resources, and add no new ones.  You'll
 * discover that after class part initialization (check in initialization;
 * you can call _XmBuildResources once), you'll have some.
 */
void
_XmBuildResources(XmSyntheticResource **wc_resources_ptr,
		  int *wc_num_resources_ptr,
		  XmSyntheticResource *sc_resources,
		  int sc_num_resources)
{
    XmSyntheticResource *cmb;
    int ncmb, i, j, wb;

    XdbDebug(__FILE__, NULL, "BuildResources\n");

    if (*wc_num_resources_ptr == 0) {
	*wc_resources_ptr = sc_resources;
	*wc_num_resources_ptr = sc_num_resources;
	return;
    }

    ncmb = *wc_num_resources_ptr + sc_num_resources;
    cmb = (XmSyntheticResource *)XtMalloc(ncmb * sizeof(XmSyntheticResource));
    bcopy((void *)sc_resources,
	  (void *)cmb,
	  sc_num_resources * sizeof(XmSyntheticResource));
    bcopy((void *)*wc_resources_ptr,
	  (void *)&cmb[sc_num_resources],
	  *wc_num_resources_ptr * sizeof(XmSyntheticResource));

    for (i = 0; i < sc_num_resources; i++) {
	wb = sc_num_resources;
	for (j = 0; j < (ncmb - wb); ) {
	    if (cmb[i].resource_name == cmb[wb+j].resource_name &&
		cmb[i].resource_size == cmb[wb+j].resource_size &&
		cmb[i].resource_offset == cmb[wb+j].resource_offset) {
		cmb[i] = cmb[wb+j];
		if ((ncmb - (wb + j + 1) > 0)) {
		    bcopy((void *)&cmb[wb+j+1],
			  (void *)&cmb[wb+j],
			  (ncmb - (wb + j + 1)) * sizeof(XmSyntheticResource));
		}
		ncmb--;
	    }
	    else
		j++;
	}
    }
    *wc_resources_ptr = cmb;
    *wc_num_resources_ptr = ncmb;
}

/*
 * the only thing I know that Motif does is to "quarkify" the strings.
 */
void
_XmInitializeSyntheticResources(XmSyntheticResource *resources,
				int num_resources)
{
    int i;

    XdbDebug(__FILE__, NULL, "InitializeSyntheticResources\n");

    if (!resources || !num_resources)
	return;
    for (i = 0; i < num_resources; i++) {
	resources[i].resource_name = 
		(char *)XrmStringToQuark(resources[i].resource_name);
    }
}

/*
 * _Xm*GetValueHook:
 *
 * Ugh.  Another doubly nested loop.  There has GOT to be a better way to
 * do this.
 * There is another bit of obscene trickery here.  After I dumped the
 * synthetics table (textXm/misc/test3.c, mot.syn), I could not for the
 * life of me figure out why Gadget were specifying their object.parent
 * fields for some of the synthetics.  Then it occurred to me that we
 * rarely pay attention to our offset values in DefaultProcs.c, so ExportProcs
 * were free to do the same, or to use them in nefarious ways.  Look in
 * Gadget.c for more details.
 * If you don't do GetValues right, forget it.  What works in R5 won't in R6,
 * by default.  See lib/Xt/Resources.c (GETVALUES_BUG).  Using CopyToArg
 * and CopyFromArg will work with R5, and give a toolkit error (by default)
 * in R6.
 */
void
_XmPrimitiveGetValuesHook(Widget w,
			  ArgList args,
			  Cardinal *num_args)
{
    int i, j;
    XrmQuark tq;
    XmPrimitiveWidgetClass pwc = (XmPrimitiveWidgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));

    XdbDebug(__FILE__, w, "PrimitiveGetValuesHook\n");

    if (!XmIsPrimitive(w))
	return;

    for (i = 0; i < *num_args; i++) {
	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < pwc->primitive_class.num_syn_resources; j++) {
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &pwc->primitive_class.syn_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (rq == tq && res->export_proc != NULL) {
		XtArgVal hold = 0, *val = &hold;

/* this is so bletcherously ugly I have to be doing something wrong. FIXME */
#if XtSpecificationRelease >= 6
		_XtCopyToArg((char *)w + res->resource_offset,
			     &val,
			     res->resource_size);
#else
		_XtCopyToArg((char *)w + res->resource_offset,
			     val,
			     res->resource_size);
#endif
		(res->export_proc)(w,
				   res->resource_offset,
				   val);
		_XtCopyFromArg(hold,
			       (char *)args[i].value,
			       res->resource_size);
	    }
	}

	/*
	 * check for constraint synthetics
	 */
	if (!XmIsManager(XtParent(w)))
	    continue;
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	    continue;
	if (!w->core.constraints)
	    continue;
	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++) {
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (rq == tq && res->export_proc != NULL) {
		XtArgVal hold = 0, *val = &hold;

#if XtSpecificationRelease >= 6
		_XtCopyToArg((char *)w->core.constraints +
				 res->resource_offset,
			     &val,
			     res->resource_size);
#else
		_XtCopyToArg((char *)w->core.constraints +
				 res->resource_offset,
			     val,
			     res->resource_size);
#endif
		(res->export_proc)(w,
				   res->resource_offset,
				   val);
		_XtCopyFromArg(hold,
			       (char *)args[i].value,
			       res->resource_size);
	    }
	}
    }
}

void
_XmGadgetGetValuesHook(Widget w,
		       ArgList args,
		       Cardinal *num_args)
{
    int i, j;
    XrmQuark tq;
    XmGadgetClass gwc = (XmGadgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));

    XdbDebug(__FILE__, w, "GadgetGetValuesHook\n");

    if (!XmIsGadget(w))
	return;

    for (i = 0; i < *num_args; i++) {
	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < gwc->gadget_class.num_syn_resources; j++) {
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &gwc->gadget_class.syn_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (rq == tq && res->export_proc != NULL) {
		XtArgVal hold = 0, *val = &hold;

#if XtSpecificationRelease >= 6
		_XtCopyToArg((char *)w + res->resource_offset,
			     &val,
			     res->resource_size);
#else
		_XtCopyToArg((char *)w + res->resource_offset,
			     val,
			     res->resource_size);
#endif
		(res->export_proc)(w,
				   res->resource_offset,
				   val);
		_XtCopyFromArg(hold,
			       (char *)args[i].value,
			       res->resource_size);
	    }
	}

	/*
	 * check for constraint synthetics
	 */
	if (!XmIsManager(XtParent(w)))
	    continue;
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	    continue;
	if (!w->core.constraints)
	    continue;
	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++) {
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (rq == tq && res->export_proc != NULL) {
		XtArgVal hold = 0, *val = &hold;

#if XtSpecificationRelease >= 6
		_XtCopyToArg((char *)w->core.constraints +
				 res->resource_offset,
			     &val,
			     res->resource_size);
#else
		_XtCopyToArg((char *)w->core.constraints +
				 res->resource_offset,
			     val,
			     res->resource_size);
#endif
		(res->export_proc)(w,
				   res->resource_offset,
				   val);
		_XtCopyFromArg(hold,
			       (char *)args[i].value,
			       res->resource_size);
	    }
	}
    }
}

void
_XmManagerGetValuesHook(Widget w,
		        ArgList args,
		        Cardinal *num_args)
{
    int i, j;
    XrmQuark tq;
    XmManagerWidgetClass pwc = (XmManagerWidgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));

    XdbDebug(__FILE__, w, "ManagerGetValuesHook\n");

    if (!XmIsManager(w))
	return;

    for (i = 0; i < *num_args; i++) {
	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < pwc->manager_class.num_syn_resources; j++) {
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &pwc->manager_class.syn_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (rq == tq && res->export_proc != NULL) {
		XtArgVal hold = 0, *val = &hold;

#if XtSpecificationRelease >= 6
		_XtCopyToArg((char *)w + res->resource_offset,
			     &val,
			     res->resource_size);
#else
		_XtCopyToArg((char *)w + res->resource_offset,
			     val,
			     res->resource_size);
#endif
		(res->export_proc)(w,
				   res->resource_offset,
				   val);
		_XtCopyFromArg(hold,
			       (char *)args[i].value,
			       res->resource_size);
	    }
	}

	/*
	 * check for constraint synthetics
	 */
	if (!XmIsManager(XtParent(w)))
	    continue;
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	    continue;
	if (!w->core.constraints)
	    continue;
	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++) {
	    XmSyntheticResource *res;
	    XrmQuark rq;

	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (rq == tq && res->export_proc != NULL) {
		XtArgVal hold = 0, *val = &hold;

#if XtSpecificationRelease >= 6
		_XtCopyToArg((char *)w->core.constraints +
				 res->resource_offset,
			     &val,
			     res->resource_size);
#else
		_XtCopyToArg((char *)w->core.constraints +
				 res->resource_offset,
			     val,
			     res->resource_size);
#endif
		(res->export_proc)(w,
				   res->resource_offset,
				   val);
		_XtCopyFromArg(hold,
			       (char *)args[i].value,
			       res->resource_size);
	    }
	}
    }
}

void
_XmExtGetValuesHook(Widget w,
		    ArgList args,
		    Cardinal *num_args)
{
    int i, j;
    XrmQuark tq;
    XmExtObjectClass ewc = (XmExtObjectClass)XtClass(w);
    XmSyntheticResource *res;
    XrmQuark rq;


    XdbDebug(__FILE__, w, "ExtObjectGetValuesHook\n");

    if (!XmIsExtObject(w))
	return;

    for (i = 0; i < *num_args; i++) {
	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < ewc->ext_class.num_syn_resources; j++) {
	    res = &ewc->ext_class.syn_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (rq == tq && res->export_proc != NULL) {
		XtArgVal hold = 0, *val = &hold;
#if XtSpecificationRelease >= 6
		_XtCopyToArg((char *)w + res->resource_offset,
			     &val,
			     res->resource_size);
#else
		_XtCopyToArg((char *)w + res->resource_offset,
			     val,
			     res->resource_size);
#endif

		(res->export_proc)(w,
				   res->resource_offset,
				   val);
		_XtCopyFromArg(hold,
			       (char *)args[i].value,
			       res->resource_size);
	    }
	}
    }
}

void
_XmExtImportArgs(Widget w,
		 ArgList args,
		 Cardinal *num_args)
{
    XmExtObjectClass ewc = (XmExtObjectClass)XtClass(w);
    int i;
    XrmQuark tq;

    XdbDebug(__FILE__, w, "ExtImportArgs\n");

    if (!XmIsExtObject(w))
	return;
    /*
     * An ExtObject will never have a real parent, and will never have
     * constraints.
     */
    for (i = 0; i < *num_args; i++) {
	int j;
	XmSyntheticResource *res;
	XrmQuark rq;
	XtArgVal value;

	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < ewc->ext_class.num_syn_resources; j++) {
	    res = &ewc->ext_class.syn_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (tq == rq && res->import_proc != NULL) {
		/*
		 * Well, I'm going to take the lessons I learned doing the
		 * mwm resource conversion and apply them here.  We'll see
		 * if this works.
		 */
		value = args[i].value;
		if ((res->import_proc)(w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD) {
	    	    _XtCopyFromArg(value,
				   (char *)w + res->resource_offset,
				   res->resource_size);
		}
		else
		    args[i].value = value;
	    }
	}
    }
}

void
_XmPrimitiveImportArgs(Widget w,
		       ArgList args,
		       Cardinal *num_args)
{
    XmPrimitiveWidgetClass pwc = (XmPrimitiveWidgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));
    int i;
    XrmQuark tq;

    XdbDebug(__FILE__, w, "PrimitiveImportArgs\n");

    if (!XmIsPrimitive(w))
	return;

    /*
     * This was commented, 'cause there should be a better way than a doubly
     * nested loop.
     * 012396 - MLM - As bad as this is, it seems to be what Motif does.  This
     * will be what I do for now, but this should be revisited.
     */
    for (i = 0; i < *num_args; i++) {
	int j;
	XmSyntheticResource *res;
	XrmQuark rq;
	XtArgVal value;

	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < pwc->primitive_class.num_syn_resources; j++) {
	    res = &pwc->primitive_class.syn_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (tq == rq && res->import_proc != NULL) {
		/*
		 * Well, I'm going to take the lessons I learned doing the
		 * mwm resource conversion and apply them here.  We'll see
		 * if this works.
		 */
		value = args[i].value;
		if ((res->import_proc)(w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD) {
	    	    _XtCopyFromArg(value,
				   (char *)w + res->resource_offset,
				   res->resource_size);
		}
		else
		    args[i].value = value;
	    }
	}
	/*
	 * Form (at least) has synthetic constraint resources.  Do them, too.
	 */
	if (!XmIsManager(XtParent(w)))
	    continue;
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	    continue;
	if (!w->core.constraints)
	    continue;
	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++) {
	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (tq == rq && res->import_proc) {
		value = args[i].value;
		if ((res->import_proc)(w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD) {
	    	    _XtCopyFromArg(value,
				   (char *)w->core.constraints +
					    res->resource_offset,
				   res->resource_size);
		}
		else
		    args[i].value = value;
	    }
	}
    }
}

void
_XmGadgetImportArgs(Widget w,
		    ArgList args,
		    Cardinal *num_args)
{
    XmGadgetClass gwc = (XmGadgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));
    int i;
    XrmQuark tq;

    XdbDebug(__FILE__, w, "GadgetImportArgs\n");

    if (!XmIsGadget(w))
	return;
    /*
     * see comments above.
     */
    for (i = 0; i < *num_args; i++) {
	int j;
	XmSyntheticResource *res;
	XrmQuark rq;
	XtArgVal value;

	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < gwc->gadget_class.num_syn_resources; j++) {
	    res = &gwc->gadget_class.syn_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (tq == rq && res->import_proc != NULL) {
		/*
		 * Well, I'm going to take the lessons I learned doing the
		 * mwm resource conversion and apply them here.  We'll see
		 * if this works.
		 */
		value = args[i].value;
		if ((res->import_proc)(w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD) {
	    	    _XtCopyFromArg(value,
				   (char *)w + res->resource_offset,
				   res->resource_size);
		}
		else
		    args[i].value = value;
	    }
	}
	/*
	 * Form (at least) has synthetic constraint resources.  Do them, too.
	 */
	if (!XmIsManager(XtParent(w)))
	    continue;
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	    continue;
	if (!w->core.constraints)
	    continue;
	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++) {
	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (tq == rq && res->import_proc) {
		value = args[i].value;
		if ((res->import_proc)(w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD) {
	    	    _XtCopyFromArg(value,
				   (char *)w->core.constraints +
					   res->resource_offset,
				   res->resource_size);
		}
		else
		    args[i].value = value;
	    }
	}
    }
}

/*
 * Got the same problems here that I have in BuildGadgetResources -- how to
 * get the GCache part of a gadget, and gadgets with no cache part.
 * 012796 -- Solved by the BaseClass extension
 */
void
_XmGadgetImportSecondaryArgs(Widget w,
			     ArgList args,
			     Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmExtClassRec *subpclass;
    int i;
    XrmQuark tq;

    XdbDebug(__FILE__, w, "GadgetImportSecondaryArgs\n");

    bce = (XmBaseClassExt *)_XmGetBaseClassExtPtr(XtClass(w), XmQmotif);

    if (!XmIsGadget(w) || !*bce ||
	!(subpclass = (XmExtObjectClass)(*bce)->secondaryObjectClass))
	return;

    /*
     * see comments above.
     */
    for (i = 0; i < *num_args; i++) {
	int j;
	XmSyntheticResource *res;
	XrmQuark rq;
	XtArgVal value;

	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < subpclass->ext_class.num_syn_resources; j++) {
	    res = &subpclass->ext_class.syn_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (tq == rq && res->import_proc != NULL) {
		/*
		 * Well, I'm going to take the lessons I learned doing the
		 * mwm resource conversion and apply them here.  We'll see
		 * if this works.
		 */
		value = args[i].value;
		(res->import_proc)(w, res->resource_offset, &value);
		args[i].value = value;
	    }
	}
    }
}

void
_XmManagerImportArgs(Widget w,
		     ArgList args,
		     Cardinal *num_args)
{
    XmManagerWidgetClass pwc = (XmManagerWidgetClass)XtClass(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)XtClass(XtParent(w));
    int i;
    XrmQuark tq;

    XdbDebug(__FILE__, w, "ManagerImportArgs\n");

    if (!XmIsManager(w))
	return;
    /*
     * A little trickier this time.  When you read this, keep in mind that
     * managers often have Manager parents, and thus have constraint resources.
     */
    for (i = 0; i < *num_args; i++) {
	int j;
	XmSyntheticResource *res;
	XrmQuark rq;
	XtArgVal value;

	tq = XrmStringToQuark(args[i].name);

	for (j = 0; j < pwc->manager_class.num_syn_resources; j++) {
	    res = &pwc->manager_class.syn_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (tq == rq && res->import_proc != NULL) {
		/*
		 * Well, I'm going to take the lessons I learned doing the
		 * mwm resource conversion and apply them here.  We'll see
		 * if this works.
		 */
		value = args[i].value;
		if ((res->import_proc)(w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD) {
	    	    _XtCopyFromArg(value,
				   (char *)w + res->resource_offset,
				   res->resource_size);
		}
		else
		    args[i].value = value;
	    }
	}
	/*
	 * Form (at least) has synthetic constraint resources.  Do them, too.
	 */
	if (!XmIsManager(XtParent(w)))
	    continue;
	if (mwc->manager_class.num_syn_constraint_resources == 0)
	    continue;
	if (!w->core.constraints)
	    continue;
	for (j = 0; j < mwc->manager_class.num_syn_constraint_resources; j++) {
	    res = &mwc->manager_class.syn_constraint_resources[j];
	    rq = (XrmQuark)res->resource_name;
	    if (tq == rq && res->import_proc) {
		value = args[i].value;
		if ((res->import_proc)(w, res->resource_offset, &value) ==
		    XmSYNTHETIC_LOAD) {
	    	    _XtCopyFromArg(value,
				   (char *)w->core.constraints +
					   res->resource_offset,
				   res->resource_size);
		}
		else
		    args[i].value = value;
	    }
	}
    }
}

/*
 * WANTED: more accurate algorithms
 */
int
_XmConvertUnits(Screen *screen,
		int orientation,
		int from_type,
		int from_val,
		int to_type)
{
    double factor = 1.0, sfact = 1.0;

    /*
     * factor is the number of pixels of the screen over the number of
     * millimeters of the screen, i.e., the number of pixels per millimeter.
     * 1 point is 1/72nd of an inch.
     * font units are specified by the XmScreen resources XmNhorizontalFontUnit
     * and XmNverticalFontUnit.
     */
    switch (orientation)
    {
    case XmHORIZONTAL:
	factor = (double)WidthOfScreen(screen) /
		 (double)WidthMMOfScreen(screen);
	sfact = (double)_XmGetFontUnit(screen, XmHORIZONTAL);
	break;
    case XmVERTICAL:
	factor = (double)HeightOfScreen(screen) /
		 (double)HeightMMOfScreen(screen);
	sfact = (double)_XmGetFontUnit(screen, XmVERTICAL);
	break;
    }

    switch (from_type)
    {
    case XmPIXELS:
	switch (to_type)
	{
	case XmPIXELS:
	    return from_val;
	    break;
	case Xm100TH_MILLIMETERS:
	    return (int)((from_val / factor) / 100.0 + 0.5);
	    break;
	case Xm1000TH_INCHES:
	    return (int)((from_val / factor) * (InchesPerMM * 1000.0) + 0.5);
	    break;
	case Xm100TH_POINTS:
	    return (int)((from_val / factor) * (PointsPerMM * 100.0) + 0.5);
	    break;
	case Xm100TH_FONT_UNITS:
	    break;
	}
	break;
    case Xm100TH_MILLIMETERS:
	switch (to_type)
	{
	case XmPIXELS:
	    return (int)((from_val * 100.0) * factor + 0.5);
	    break;
	case Xm100TH_MILLIMETERS:
	    return from_val;
	    break;
	case Xm1000TH_INCHES:
	    return (int)((from_val / 100.0) * (InchesPerMM * 1000.0) + 0.5);
	    break;
	case Xm100TH_POINTS:
	    return (int)((from_val / 100.0) * (PointsPerMM * 100.0) + 0.5);
	    break;
	case Xm100TH_FONT_UNITS:
	    break;
	}
	break;
    case Xm1000TH_INCHES:
	switch (to_type)
	{
	case XmPIXELS:
	    return (int)((from_val / 1000.0) * MMsPerInch * factor + 0.5);
	    break;
	case Xm100TH_MILLIMETERS:
	    return (int)((from_val / 1000.0) * (MMsPerInch * 100.0) + 0.5);
	    break;
	case Xm1000TH_INCHES:
	    return from_val;
	    break;
	case Xm100TH_POINTS:
	    return (int)((from_val / 1000.0) * (PointsPerInch * 100.0) + 0.5);
	    break;
	case Xm100TH_FONT_UNITS:
	    break;
	}
	break;
    case Xm100TH_POINTS:
	switch (to_type)
	{
	case XmPIXELS:
	    return (int)((from_val / 100.0) * MMsPerPoint * factor);
	    break;
	case Xm100TH_MILLIMETERS:
	    return (int)((from_val / 100.0) * (MMsPerPoint * 100.0) + 0.5);
	    break;
	case Xm1000TH_INCHES:
	    return (int)((from_val / 100.0) * (InchesPerPoint * 1000.0) + 0.5);
	    break;
	case Xm100TH_POINTS:
	    return from_val;
	    break;
	case Xm100TH_FONT_UNITS:
	    break;
	}
	break;
    case Xm100TH_FONT_UNITS:
	switch (to_type)
	{
	case XmPIXELS:
	    break;
	case Xm100TH_MILLIMETERS:
	    break;
	case Xm1000TH_INCHES:
	    break;
	case Xm100TH_POINTS:
	    break;
	case Xm100TH_FONT_UNITS:
	    return from_val;
	    break;
	}
	break;
    }
    return 0;
}

/*
 * MLM: In Motif, if this function is called with an uncompiled resource list,
 * it explodes messily.  However, if you look at the output of misc/test5.c
 * you will notice an interesting thing;  the XmNunitType resource, floats
 * to the top (check testXm/misc/test5.c + testXm/misc/mot.compres).
 * I can't find any other candidate for doing this other than
 * _XmSortResourceList; after passing in a LessTif widget (suitably
 * renamed) resource list into this function, I can verify that that does
 * indeed happen.  Note the XrmResource argument:  it must be a compiled
 * resource list.
 * Addendum: this actually makes a weird sort of sense:  the Synthetic resource
 * ResInd stuff wants the UnitType to be correct when called.  That doesn't
 * mean it's pretty, though.
 */
void
_XmSortResourceList(XrmResource *list[],
		    Cardinal len)
{
    int i;
    XrmQuark unit;
    XrmResource *tmp;

    unit = XrmStringToQuark(XmNunitType);

    for (i = 0; i < len; i++) {
	if (list[i]->xrm_name == unit)
	    break;
    }

    if (i == len || i == 0) /* Nobody home, or already done */
	return;

    tmp = list[i];
    bcopy((void *)&list[0], (void *)&list[1], i * sizeof(XrmResource *));
    list[0] = tmp;
}

unsigned char
_XmGetUnitType(Widget widget)
{
    Widget ve;

    /*
     * There used to be a call to XtGetValues here.  Unfortunately, with the
     * BaseClass stuff, that won't work, as that looks like a recursive call
     * to GetValues (i.e., the call to here from _XmGadgetImportArgs).  That
     * implied that the GetValuesRootWrapper was called twice, with the
     * result that GetValuesLeafWrapper was called infinitely recursive.
     * Bad thing, that.
     */
    if (XmIsVendorShell(widget)) {
	if ((ve = _LtFindVendorExt(widget)) != NULL)
	    return VSEP_UnitType(ve);
	else
	    return XmPIXELS;
    }
    if (XmIsManager(widget))
	return MGR_UnitType(widget);

    if (XmIsPrimitive(widget))
        return Prim_UnitType(widget);

    if (XmIsGadget(widget))
	return G_UnitType(widget);

    if (XmIsExtObject(widget))
	return G_UnitType(ExtObj_LogicalParent(widget));

    return XmPIXELS;
}

/*
 * XmP.h says this is here
 */
void
_XmUnitTypeDefault(Widget w,
		   int offset,
		   XrmValue *val)
{
    static unsigned char unit_type;
    Widget ve;

    unit_type = XmPIXELS;

    if ((XmIsPrimitive(w) || XmIsGadget(w))) {
	if (XmIsManager(XtParent(w)))
	    unit_type = MGR_UnitType(XtParent(w));
    }
    else if (XmIsManager(w)) {
	if (XmIsManager(XtParent(w)))
	    unit_type = MGR_UnitType(XtParent(w));
	else if (XmIsVendorShell(XtParent(w))) {
	    if ((ve = _LtFindVendorExt(XtParent(w))) != NULL)
		unit_type = VSEP_UnitType(ve);
	}
    }

    val->addr = (XtPointer)&unit_type;
}

void
_XmExportXmString(Widget w, int offset, XtArgVal *value)
{
    XmString str;

    str = *(XmString *)(((char *)w)+offset);
    if (str)
	str = XmStringCopy(str);
    *value = (XtArgVal)str;
}

void
_XmExportString(Widget w, int offset, XtArgVal *value)
{
    String str;

    str = *(String *)(((char *)w)+offset);
    if (str)
	str = XtNewString(str);
    *value = (XtArgVal)str;
}

int
XmConvertUnits(Widget widget,
	       int orientation,
	       int from_unit_type,
	       int from_value,
	       int to_unit_type)
{
    if (widget == NULL)
	return 0;
    if (orientation != XmHORIZONTAL && orientation != XmVERTICAL)
	return 0;
    if (from_unit_type < 0 || from_unit_type > Xm100TH_FONT_UNITS)
	return 0;
    if (to_unit_type < 0 || to_unit_type > Xm100TH_FONT_UNITS)
	return 0;
    return _XmConvertUnits(XtScreenOfObject(widget), orientation,
			   from_unit_type, from_value, to_unit_type);
}

int
XmCvtToHorizontalPixels(Screen *screen, int from_val, int from_type)
{
    if (!screen)
	return 0;
    if (from_type < 0 || from_type > Xm100TH_FONT_UNITS)
	return 0;
    return _XmConvertUnits(screen, XmHORIZONTAL, from_type, from_val, XmPIXELS);
}

int
XmCvtToVerticalPixels(Screen *screen, int from_val, int from_type)
{
    if (!screen)
	return 0;
    if (from_type < 0 || from_type > Xm100TH_FONT_UNITS)
	return 0;
    return _XmConvertUnits(screen, XmVERTICAL, from_type, from_val, XmPIXELS);
}

int
XmCvtFromHorizontalPixels(Screen *screen, int from_val, int to_type)
{
    if (!screen)
	return 0;
    if (to_type < 0 || to_type > Xm100TH_FONT_UNITS)
	return 0;
    return _XmConvertUnits(screen, XmHORIZONTAL, XmPIXELS, from_val, to_type);
}

int
XmCvtFromVerticalPixels(Screen *screen, int from_val, int to_type)
{
    if (!screen)
	return 0;
    if (to_type < 0 || to_type > Xm100TH_FONT_UNITS)
	return 0;
    return _XmConvertUnits(screen, XmVERTICAL, XmPIXELS, from_val, to_type);
}

void
XmSetFontUnits(Display *display, int h_value, int v_value)
{
}

void
XmSetFontUnit(Display *display, int value)
{
}
