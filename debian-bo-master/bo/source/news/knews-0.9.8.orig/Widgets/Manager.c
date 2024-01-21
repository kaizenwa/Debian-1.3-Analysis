/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include "Compat.h"
#include "ManagerP.h"

static XtResource resources[] = {
    {XtNborderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(ManagerRec, core.border_width), XtRImmediate, (XtPointer)0},
#define offset(field) XtOffsetOf(ManagerRec, manager.field)
    {XtNresizeCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(resize_callback), XtRCallback, (XtPointer)NULL},
    {XtNpreferredWidth, XtCPreferredWidth, XtRDimension, sizeof(Dimension),
     offset(pref_width), XtRImmediate, (XtPointer)0},
    {XtNpreferredHeight, XtCPreferredHeight, XtRDimension, sizeof(Dimension),
     offset(pref_height), XtRImmediate, (XtPointer)0},
#undef offset
};

static XtResource sub_resources[] = {
#define offset(field) XtOffsetOf(ManagerConstraintsRec, manager.field)
    {XtNcontainHoriz, XtCContain, XtRBoolean, sizeof(Boolean),
     offset(contain_horiz), XtRImmediate, (XtPointer)False},
    {XtNcontainVert, XtCContain, XtRBoolean, sizeof(Boolean),
     offset(contain_vert), XtRImmediate, (XtPointer)False},
#undef offset
};

static void		Initialize(Widget, Widget, ArgList, Cardinal*);
static void		Resize(Widget);
static XtGeometryResult	GeometryManager(Widget, XtWidgetGeometry*,
					XtWidgetGeometry*);
static void		ChangeManaged(Widget);
static XtGeometryResult	QueryGeometry(Widget, XtWidgetGeometry*,
				      XtWidgetGeometry*);
static Boolean		ConstraintSetValues(Widget, Widget, Widget,
					    ArgList, Cardinal*);

ManagerClassRec managerClassRec = {
    { /* core fields */
    	(WidgetClass) &constraintClassRec,	/* superclass		*/
    	"Manager",				/* class_name		*/
    	sizeof(ManagerRec),			/* widget_size		*/
    	NULL,					/* class_initialize	*/
    	NULL,					/* class_part_initialize*/
    	FALSE,					/* class_inited		*/
    	Initialize,				/* initialize		*/
    	NULL,					/* initialize_hook	*/
    	XtInheritRealize,			/* realize		*/
    	NULL,					/* actions		*/
    	0,					/* num_actions		*/
    	resources,				/* resources		*/
    	XtNumber(resources),			/* resource_count	*/
    	NULLQUARK,				/* xrm_class		*/
    	TRUE,					/* compress_motion	*/
    	TRUE,					/* compress_exposure	*/
    	TRUE,					/* compress_enterleave	*/
    	FALSE,					/* visible_interest	*/
    	NULL,					/* destroy		*/
    	Resize,					/* resize		*/
    	NULL,					/* expose		*/
    	NULL,					/* set_values		*/
    	NULL,					/* set_values_hook	*/
    	XtInheritSetValuesAlmost,		/* set_values_almost	*/
    	NULL,					/* get_values_hook	*/
    	NULL,					/* accept_focus		*/
    	XtVersion,				/* version		*/
    	NULL,					/* callback_private	*/
    	NULL,					/* tm_table		*/
    	QueryGeometry,				/* query_geometry       */
    	XtInheritDisplayAccelerator,		/* display_accelerator  */
    	NULL					/* extension            */
    },
    { /* composite fields */
	GeometryManager,			/* geometry manager     */
	ChangeManaged,				/* change_managed       */
	XtInheritInsertChild,			/* insert child         */
	XtInheritDeleteChild,			/* delete child         */
	NULL					/* extension            */
    },
    { /* constraint fields */
	sub_resources,				/* resources		*/
	XtNumber(sub_resources),		/* num_resources	*/
	sizeof(ManagerConstraintsRec),		/* constraint_size	*/
	NULL,					/* initialize		*/
	NULL,					/* destroy		*/
	ConstraintSetValues,			/* set_values		*/
	NULL,					/* extension		*/
    },
    { /* manager fields */
	NULL					/* extension            */
    }
};

WidgetClass managerWidgetClass = (WidgetClass) &managerClassRec;

/*************************************************************************/

static int fit_coords(Dimension	parent_size,
		      Dimension	*child_size,
		      Position	*child_pos,
		      int	contain)
{
    if (contain) {
	if (*child_pos == 0 && *child_size == parent_size)
	    return False;
	*child_pos = 0;
	*child_size = parent_size;
	return True;
    }

    if (*child_pos > 0) {
	*child_pos = 0;
	if (*child_size < parent_size)
	    *child_size = parent_size;
	return True;
    }

    if (*child_pos < (int)parent_size - (int)*child_size) {
	*child_pos = (int)parent_size - (int)*child_size;
	if (*child_pos > 0) {
	    *child_pos = 0;
	    *child_size = parent_size;
	}
	return True;
    }

    return False;
}

static Widget find_child(ManagerWidget w)
{
    Widget	*loop = w->composite.children;
    int		n = w->composite.num_children;

    while (n-- > 0)
	if (XtIsManaged(*loop))
	    return *loop;
	else
	    loop++;

    return NULL;
}

static void fit_child(ManagerWidget w, Widget child)
{
    XtWidgetGeometry	intended, preferred;
    ManagerCons		cons   = (ManagerCons)child->core.constraints;
    Dimension		width  = child->core.width;
    Dimension		height = child->core.height;
    Position		x      = child->core.x;
    Position		y      = child->core.y;

    fit_coords(w->core.width, &width, &x, cons->manager.contain_horiz);
    fit_coords(w->core.height, &height, &y, cons->manager.contain_vert);
    
    intended.request_mode = CWX | CWY | CWWidth | CWHeight;
    intended.x      = x;
    intended.y      = y;
    intended.width  = width;
    intended.height = height;

    if (XtQueryGeometry(child, &intended, &preferred) == XtGeometryAlmost) {
	if (preferred.request_mode & CWX)
	    x = preferred.x;
	if (preferred.request_mode & CWY)
	    y = preferred.y;
	if (preferred.request_mode & CWWidth)
	    width = preferred.width;
	if (preferred.request_mode & CWHeight)
	    height = preferred.height;

	fit_coords(w->core.width, &width, &x, cons->manager.contain_horiz);
	fit_coords(w->core.height, &height, &y, cons->manager.contain_vert);
    }

    XtConfigureWidget(child, x, y, width, height, 0);
}

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *num_args)
{
    ManagerWidget	new = (ManagerWidget)gnew;

    if (new->core.width  == 0)
	if (new->manager.pref_width == 0)
	    new->core.width  = 1;
	else
	    new->core.width = new->manager.pref_width;
    if (new->core.height == 0)
	if (new->manager.pref_height == 0)
	    new->core.height = 1;
	else
	    new->core.height = new->manager.pref_height;
}

static void Resize(Widget gw)
{
    ManagerWidget	w = (ManagerWidget)gw;
    Widget		child = find_child(w);
    XtCallbackList	c_list = w->manager.resize_callback;

    if (child)
	fit_child(w, child);
    if (c_list)
	XtCallCallbackList((Widget)w, c_list, NULL);
}

static void ChangeManaged(Widget gw)
{
    ManagerWidget	w = (ManagerWidget)gw;
    Widget		child = find_child(w);

    if (child)
	fit_child(w, child);
}

static XtGeometryResult QueryGeometry(Widget gw,
				      XtWidgetGeometry *intended,
				      XtWidgetGeometry *preferred)
{
    ManagerWidget	w = (ManagerWidget)gw;
    Widget		child = find_child(w);
    Dimension		pref_width = w->manager.pref_width;
    Dimension		pref_height = w->manager.pref_height;
    Dimension		intended_width, intended_height;

    if (intended->request_mode & CWWidth)
	intended_width = intended->width;
    else
	intended_width = w->core.width;

    if (intended->request_mode & CWHeight)
	intended_height = intended->height;
    else
	intended_height = w->core.height;

    if (pref_width == 0 || pref_height == 0) {
	XtWidgetGeometry	child_pref;
	XtGeometryResult	result;

	if (!child)
	    return XtGeometryNo;

	result = XtQueryGeometry(child, NULL, &child_pref);
	if (result == XtGeometryAlmost) {
	    if ((child_pref.request_mode & CWWidth) && pref_width == 0)
		pref_width = child_pref.width;
	    if ((child_pref.request_mode & CWHeight) && pref_height == 0)
		pref_height = child_pref.height;
	}
	if (pref_width == 0)
	    pref_width = child->core.width;
	if (pref_height == 0)
	    pref_height = child->core.height;
    }

    preferred->request_mode = CWWidth | CWHeight;
    preferred->width = pref_width;
    preferred->height = pref_height;

    if (pref_width == w->core.width && pref_height == w->core.height)
	return XtGeometryNo;
    if (pref_width == intended_width && pref_height == intended_height)
	return XtGeometryYes;

    return XtGeometryAlmost;
}

static XtGeometryResult GeometryManager(Widget child,
					XtWidgetGeometry *req,
					XtWidgetGeometry *reply)
{
    ManagerWidget	w = (ManagerWidget)XtParent(child);
    ManagerCons		cons = (ManagerCons)child->core.constraints;
    Dimension		width, height;
    Position		x, y;
    Boolean		ok = True;

    if (child != find_child(w))
	return XtGeometryNo;

    if (req->request_mode & CWX)
	x = req->x;
    else
	x = child->core.x;

    if (req->request_mode & CWY)
	y = req->y;
    else
	y = child->core.y;
    if (req->request_mode & CWWidth)
	width = req->width;
    else
	width = child->core.width;

    if (req->request_mode & CWHeight)
	height = req->height;
    else
	height = child->core.height;

    if (!fit_coords(w->core.width, &width, &x, cons->manager.contain_horiz))
	ok = False;
    if (!fit_coords(w->core.height, &height, &y, cons->manager.contain_vert))
	ok = False;

    if (req->request_mode & XtCWQueryOnly) {
	reply->request_mode = CWX | CWY | CWWidth | CWHeight;
	reply->x = x;
	reply->y = y;
	reply->width = width;
	reply->height = height;
	return XtGeometryAlmost;
    }

    XtConfigureWidget(child, x, y, width, height, 0);

    return XtGeometryDone;
}

static Boolean ConstraintSetValues(Widget current,
				   Widget request,
				   Widget new,
				   ArgList args,
				   Cardinal *num_args)
{
    ManagerWidget	w = (ManagerWidget)XtParent(new);
    ManagerCons		new_cons = (ManagerCons)new->core.constraints;
    ManagerCons		curr_cons = (ManagerCons)current->core.constraints;

    if (new == find_child(w) &&
	(curr_cons->manager.contain_horiz != new_cons->manager.contain_horiz ||
	 curr_cons->manager.contain_vert  != new_cons->manager.contain_vert))
	fit_child(w, new);

    return False;
}

/***************************************************************/

void Remanage(Widget w)
{
    ChangeManaged(w);
}
