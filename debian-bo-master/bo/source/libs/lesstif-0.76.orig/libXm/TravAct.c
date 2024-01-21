/**
 *
 * $Id: TravAct.c,v 1.6 1997/01/11 02:19:55 miers Exp $
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

static char rcsid[] = "$Id: TravAct.c,v 1.6 1997/01/11 02:19:55 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>
#include <XmI/TraversalI.h>

#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/ManagerP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/GadgetP.h>
#include <Xm/VendorSEP.h>
#include <X11/X.h>

#include <XmI/DebugUtil.h>


typedef struct _XmFocusFlag {
    struct _XDisplay	*display;
    unsigned short	flags;
} XmFocusFlagRec, *XmFocusFlag;

XmFocusFlag flag_list = NULL;
int flag_list_size = 0;

/*
 * apparently a flag is kept on a per- display basis.  Why, I don't
 * know, but I'm trying to find out.  It seems to be related somehow
 * to the help system and focus...  FIXME
 */
void
_XmSetFocusFlag(Widget w, unsigned mask, Boolean value)
{
    int i = 0;

    for (i = 0; i < flag_list_size; i++) {
	if (XtDisplay(w) == flag_list[i].display) {
	    if (value) {
		flag_list[i].flags |= mask;
		return;
	    }
	    else {
		flag_list[i].flags &= ~mask;
		return;
	    }
	}
    }

    i = flag_list_size;
    flag_list_size++;
    flag_list = (XmFocusFlag)XtRealloc((char *)flag_list,
				       flag_list_size * sizeof(XmFocusFlagRec));

    flag_list[i].display = XtDisplay(w);
    if (!value)
	flag_list[i].flags = 0;
    else
	flag_list[i].flags = mask;
}

/*
 * this is how we get the flags
 */
unsigned short
_XmGetFocusFlag(Widget w, unsigned int mask)
{
    int i;

    for (i = 0; i < flag_list_size; i++) {
	if (flag_list[i].display == XtDisplay(w))
	    return flag_list[i].flags & mask;
    }

    return 0;
}

void
_XmSetFocusResetFlag(Widget w, Boolean value)
{
    _XmSetFocusFlag(w, XmFOCUS_RESET, value);
}

Boolean
_XmGetFocusResetFlag(Widget w)
{
    return _XmGetFocusFlag(w, XmFOCUS_RESET);
}

void
_XmTrackShellFocus(Widget widget,
		   XtPointer client_data,
		   XEvent *event,
		   Boolean *dontSwallow)
{
    XFocusChangeEvent *fev = (XFocusChangeEvent *)event;
    XCrossingEvent *cev = (XCrossingEvent *)event;
    XmVendorShellExtRec *ve = (XmVendorShellExtRec *)client_data;
    XmRelations focal_point;
    XmFocusData fd;

    if (event->type == FocusIn || event->type == FocusOut) {
	XdbDebug(__FILE__, widget,
	     "FOCUS EVENT: Shell: 0x%08x %s\n", widget, XtName(widget));
	XdbDebug(__FILE__, widget,
	     " type %d mode %d detail %d\n", fev->type, fev->mode, fev->detail);
    }
    else {
	XdbDebug(__FILE__, widget,
	     "ENTER/LEAVE EVENT: Shell: 0x%08x %s\n", widget, XtName(widget));
	XdbDebug(__FILE__, widget,
	     " type %d mode %d detail %d\n", cev->type, cev->mode, cev->detail);
    }

    if (CoreBeingDestroyed(widget)) {
	*dontSwallow = False;
	return;
    }

    if (ve == NULL)
	return;

    if (VSEP_FocusData(ve) == NULL)
	return;

    fd = VSEP_FocusData(ve);
    focal_point = fd->focal_point;

    switch (event->type) {
    case EnterNotify:
    case LeaveNotify:
	if (cev->detail == NotifyInferior)
	    break;
	if (cev->focus) {
	    switch (focal_point) {
	    case XmNO_RELATION:
		if (cev->type == EnterNotify)
		    focal_point = XmUNCLE;
		break;

	    case XmUNCLE:
		if (cev->type == LeaveNotify)
		    focal_point = XmNO_RELATION;
		break;

	    default:
		break;
	    }
	}

	break;

    case FocusIn:
	switch (fev->detail) {
	case NotifyAncestor:
	case NotifyInferior:
	case NotifyNonlinear:
	    focal_point = XmME;
	    break;

	case NotifyVirtual:
	case NotifyNonlinearVirtual:
	    focal_point = XmNEPHEW;
	    break;

	case NotifyPointer:
	    focal_point = XmUNCLE;
	    break;

	case NotifyPointerRoot:
	case NotifyDetailNone:
	default:
	    break;
	}
	break;

    case FocusOut:
	switch (fev->detail) {
	case NotifyAncestor:
	case NotifyVirtual:
	case NotifyNonlinear:
	case NotifyNonlinearVirtual:
	case NotifyPointer:
	    focal_point = XmNO_RELATION;
	    break;

	case NotifyInferior:
	case NotifyPointerRoot:
	case NotifyDetailNone:
	default:
	    break;
	}
	break;

    default:
	break;
    }

    if (focal_point == XmNO_RELATION) {
	fd->old_focus_item = NULL;
	if (fd->tree.num_entries != 0)
	    _XmFreeTravGraph(&fd->tree);
    }

    if (fd->focus_policy == XmEXPLICIT && focal_point != fd->focal_point &&
	fd->focus_item != NULL) {
	if (fd->focal_point == XmNO_RELATION || focal_point == XmNO_RELATION) {
	    if (fd->focal_point == XmNO_RELATION)
		_XmCallFocusMoved(NULL, fd->focus_item, event);
	    else
		_XmCallFocusMoved(fd->focus_item, NULL, event);
	}
    }

    fd->focal_point = focal_point;
}

Boolean
set_pointer_item(Widget w, XEvent *event)
{
    XCrossingEvent *ev = (XCrossingEvent *)event;
    XmFocusData fd = _XmGetFocusData(w);

    if (!fd)
	return False;

    fd->flush = True;

    /* if we've already seen it once, skip it */
    if (ev->type == fd->last_enter_leave.type &&
	ev->serial == fd->last_enter_leave.serial &&
	ev->time == fd->last_enter_leave.time &&
	ev->x == fd->last_enter_leave.x &&
	ev->y == fd->last_enter_leave.y) {
	return False;
    }

    fd->old_pointer_item = fd->pointer_item;
    fd->pointer_item = w;
    fd->last_enter_leave = *ev;

    return True;
}

void
flush_pointer_item(Widget w, XEvent *event)
{
    XmFocusData fd = _XmGetFocusData(w);
    XCrossingEvent *ev = (XCrossingEvent *)event;
    XCrossingEvent sev;

    if (!fd)
	return;

    if (!fd->flush)
	return;

    sev = fd->last_enter_leave;
    fd->flush = False;

    sev.serial = ev->serial;
    sev.time = ev->time;
    sev.focus = True;

    XtDispatchEvent((XEvent *)&sev);
}

void 
_XmPrimitiveEnter(Widget w, 
		  XEvent *event, 
		  String *params, 
		  Cardinal *num_params)
{
    XCrossingEvent *ev = (XCrossingEvent *)event;

    XdbDebug(__FILE__, w, "_XmPrimitiveEnter()\n");

    if (_XmGetFocusPolicy(w) != XmPOINTER)
	return;

    if (ev->focus) {
	_XmCallFocusMoved(XtParent(w), w, event);
	_XmWidgetFocusChange(w, XmENTER);
    }

    set_pointer_item(w, event);
}

void 
_XmPrimitiveLeave(Widget w, 
		  XEvent *event, 
		  String *params, 
		  Cardinal *num_params)
{
    XCrossingEvent *ev = (XCrossingEvent *)event;

    XdbDebug(__FILE__, w, "_XmPrimitiveLeave()\n");

    if (_XmGetFocusPolicy(w) != XmPOINTER)
	return;

    if (ev->focus) {
	_XmCallFocusMoved(w, XtParent(w), event);
	_XmWidgetFocusChange(w, XmLEAVE);
    }
}

void
_XmPrimitiveFocusInInternal(Widget w,
			    XEvent *event,
			    String *params,
			    Cardinal *num_params)
{
    XFocusChangeEvent *ev = (XFocusChangeEvent *)event;

    XdbDebug(__FILE__, w, "_XmPrimitiveFocusInInternal()\n");

    if (!ev->send_event) {
	XdbDebug(__FILE__, w, "_XmPrimitiveFocusInInternal: !send_event\n");
	return;
    }

    if (_XmGetFocusFlag(w, XmFOCUS_IGNORE)) {
	XdbDebug(__FILE__, w, "_XmPrimitiveFocusInInternal: FOCUS_IGNORE\n");
	return;
    }

    if (_XmGetFocusPolicy(w) != XmEXPLICIT) {
	if (!XtIsShell(XtParent(w)))
	    return;
	flush_pointer_item(w, event);
	return;
    }

    if (_XmGetActiveTabGroup(w) == NULL) {
	_XmMgrTraversal(_XmFindTopMostShell(w), XmTRAVERSE_NEXT_TAB_GROUP);
	return;
    }

    _XmWidgetFocusChange(w, XmFOCUS_IN);
}

void 
_XmPrimitiveFocusOut(Widget w, 
		    XEvent *event, 
		    String *params, 
		    Cardinal *num_params)
{
    XFocusChangeEvent *ev = (XFocusChangeEvent *)event;

    XdbDebug(__FILE__, w, "_XmPrimitiveFocusOut(): send_event: %d\n",
	     ev->send_event);

    if (!ev->send_event)
	return;

    if (CoreBeingDestroyed(w))
	return;

    if (_XmGetFocusPolicy(w) != XmEXPLICIT)
	return;

    _XmWidgetFocusChange(w, XmFOCUS_OUT);
}

void 
_XmPrimitiveFocusIn(Widget w, 
		    XEvent *event, 
		    String *params, 
		    Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmPrimitiveFocusIn()\n");

    _XmPrimitiveFocusInInternal(w, event, params, num_params);
}

void
_XmPrimitiveUnmap(Widget w,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmPrimitiveUnmap()\n");

    _XmValidateFocus(w);
}

void
_XmEnterGadget(Widget w,
	       XEvent *event,
	       String *params,
	       Cardinal *num_params)
{
    XmFocusData fd;

    XdbDebug(__FILE__, w, "_XmGadgetEnter()\n");

    if (_XmGetFocusPolicy(w) != XmPOINTER)
        return;

    if ((fd = _XmGetFocusData(w)) == NULL)
	return;

    if (fd->focal_point == XmNO_RELATION)
	return;

    _XmCallFocusMoved(fd->old_focus_item, w, event);

    _XmWidgetFocusChange(w, XmENTER);
}

void
_XmLeaveGadget(Widget w,
	       XEvent *event,
	       String *params,
	       Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmGadgetLeave()\n");

    if (_XmGetFocusPolicy(w) != XmPOINTER)
        return;

    _XmCallFocusMoved(w, XtParent(w), event);
    _XmWidgetFocusChange(w, XmLEAVE);
}

void
_XmFocusInGadget(Widget w,
		 XEvent *event,
		 String *params,
		 Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmGadgetFocusIn()\n");

    if (_XmGetFocusPolicy(w) != XmEXPLICIT)
        return;

    _XmWidgetFocusChange(w, XmFOCUS_IN);
}

void
_XmFocusOutGadget(Widget w,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmGadgetFocusOut()\n");

    if (_XmGetFocusPolicy(w) != XmEXPLICIT)
        return;

    _XmWidgetFocusChange(w, XmFOCUS_OUT);
}

void
_XmManagerEnter(Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params)
{
    XCrossingEvent *ev = (XCrossingEvent *)event;
    Widget pw;

    XdbDebug(__FILE__, w, "_XmManagerEnter()\n");

    if (_XmGetFocusPolicy(w) != XmPOINTER)
	return;

    if (!set_pointer_item(w, event))
	return;

    if (!ev->focus)
	return;

    if (ev->detail == NotifyInferior)
	pw = XtWindowToWidget(ev->display, ev->subwindow);
    else
	pw = XtParent(w);

    _XmCallFocusMoved(pw, w, event);

    _XmWidgetFocusChange(w, XmENTER);
}

void
_XmManagerLeave(Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params)
{
    XCrossingEvent *ev = (XCrossingEvent *)event;
    Widget pw;

    XdbDebug(__FILE__, w, "_XmManagerLeave()\n");

    if (ev->type != LeaveNotify)
	return;

    if (_XmGetFocusPolicy(w) != XmPOINTER)
	return;

    if (ev->detail == NotifyInferior)
	pw = XtWindowToWidget(ev->display, ev->subwindow);
    else
	pw = XtParent(w);

    if (!set_pointer_item(w, event))
	return;

    if (!ev->focus)
	return;

    _XmCallFocusMoved(w, pw, event);

    _XmWidgetFocusChange(w, XmLEAVE);
}

void
_XmManagerFocusInInternal(Widget w,
			  XEvent *event,
			  String *params,
			  Cardinal *num_params)
{
    XFocusChangeEvent *ev = (XFocusChangeEvent *)event;
    Widget tg;

    XdbDebug(__FILE__, w, "_XmManagerFocusInInternal()\n");

    if (!ev->send_event)
	return;

    if (_XmGetFocusFlag(w, XmFOCUS_RESET|XmFOCUS_IGNORE))
	return;

    if (_XmGetFocusPolicy(w) == XmPOINTER) {
	flush_pointer_item(w, event);
	return;
    }

    if ((tg = _XmGetActiveTabGroup(w)) == NULL) {
	w = _XmFindTopMostShell(w);
	_XmMgrTraversal(w, XmTRAVERSE_NEXT_TAB_GROUP);
	return;
    }

    if (MGR_ActiveChild(w) != NULL && XmIsGadget(MGR_ActiveChild(w)))
	_XmDispatchGadgetInput(MGR_ActiveChild(w), event, XmFOCUS_IN_EVENT);

    _XmWidgetFocusChange(w, XmFOCUS_IN);
}

void
_XmManagerFocusIn(Widget w,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmManagerFocusIn()\n");

    _XmManagerFocusInInternal(w, event, params, num_params);
}

void
_XmManagerFocusOut(Widget w,
		   XEvent *event,
		   String *params,
		   Cardinal *num_params)
{
    XFocusChangeEvent *ev = (XFocusChangeEvent *)event;

    XdbDebug(__FILE__, w, "_XmManagerFocusOut()\n");

    if (!ev->send_event)
	return;

    if (_XmGetFocusFlag(w, XmFOCUS_IGNORE))
	return;

    if (_XmGetFocusPolicy(w) != XmEXPLICIT)
	return;

    if (MGR_ActiveChild(w) != NULL && XmIsGadget(MGR_ActiveChild(w)))
	_XmDispatchGadgetInput(MGR_ActiveChild(w), event, XmFOCUS_OUT_EVENT);

    _XmWidgetFocusChange(w, XmFOCUS_OUT);
}

void
_XmManagerUnmap(Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmManagerUnmap()\n");

    _XmValidateFocus(w);
}

