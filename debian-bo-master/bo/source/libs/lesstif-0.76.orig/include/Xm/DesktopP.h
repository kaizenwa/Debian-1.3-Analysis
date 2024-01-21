/**
 *
 * $Id: DesktopP.h,v 1.1.1.1 1996/08/06 05:51:22 toshok Exp $
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
#ifndef _XM_DESKTOP_P_H
#define _XM_DESKTOP_P_H

#include <Xm/ExtObjectP.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef XmIsDesktopObject
#define XmIsDesktopObject(w)	XtIsSubclass(w, xmDesktopClass)
#endif

typedef struct _XmDesktopRec *XmDesktopObject;
typedef struct _XmDesktopClassRec *XmDesktopObjectClass;
extern WidgetClass xmDesktopClass;

typedef struct _XmDesktopClassPart {
    WidgetClass		child_class;
    XtWidgetProc	insert_child;
    XtWidgetProc      	delete_child;
    XtPointer		extension;
} XmDesktopClassPart, *XmDesktopClassPartPtr;

typedef struct _XmDesktopClassRec {
    ObjectClassPart		object_class;
    XmExtClassPart		ext_class;
    XmDesktopClassPart 		desktop_class;
} XmDesktopClassRec;

typedef struct {
    Widget		parent;
    Widget		*children;
    Cardinal		num_children;
    Cardinal		num_slots;
} XmDesktopPart, *XmDesktopPartPtr;

extern XmDesktopClassRec xmDesktopClassRec;

typedef struct _XmDesktopRec {
    ObjectPart			object;
    XmExtPart			ext;
    XmDesktopPart		desktop;
} XmDesktopRec;

/*
 * access macros
 */
#define Desktop_Parent(w) \
    (((XmDesktopObject)w)->desktop.parent)

#define Desktop_Children(w) \
    (((XmDesktopObject)w)->desktop.children)

#define Desktop_NumChildren(w) \
    (((XmDesktopObject)w)->desktop.num_children)

#define Desktop_NumSlots(w) \
    (((XmDesktopObject)w)->desktop.num_slots)

/*
 * protos
 */
extern WidgetClass _XmGetActualClass(Display *display,
				     WidgetClass w_class);
extern void _XmSetActualClass(Display *display,
			      WidgetClass w_class,
			      WidgetClass actualClass);

#ifdef __cplusplus
}
#endif

#endif
