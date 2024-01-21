/**
 *
 * $Id: DragDrop.c,v 1.7 1997/01/14 04:16:56 miers Exp $
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

static char rcsid[] = "$Id: DragDrop.c,v 1.7 1997/01/14 04:16:56 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/DragCP.h>
#include <Xm/DragDrop.h>
#include <Xm/DropTransP.h>

#include <XmI/DebugUtil.h>

Widget 
XmDragStart(Widget widget, 
	    XEvent *event, 
	    ArgList arglist, 
	    Cardinal argcount)
{
    Widget dc;
    XmDisplay disp = (XmDisplay)XmGetXmDisplay(XtDisplay(widget));

    XdbDebug(__FILE__, widget, "XmDragStart()\n");

    dc = XtCreateWidget("drag_context", xmDragContextClass, (Widget)disp,
			arglist, argcount);
    
    _XmDragStart((XmDragContext)dc, widget, event);

    return dc;
}

void 
XmDragCancel(Widget dragcontext)
{
    _XmDragCancel((XmDragContext)dragcontext);

    XtDestroyWidget(dragcontext);
}

void 
XmDropSiteConfigureStackingOrder(Widget widget, 
				 Widget Sibling, 
				 Cardinal stack_mode)
{
}

void 
XmDropSiteEndUpdate(Widget widget)
{
}

Status 
XmDropSiteQueryStackingOrder(Widget widget, 
			     Widget *parent_return, 
			     Widget **child_returns, 
			     Cardinal *num_child_returns)
{
    return 0;
}

void 
XmDropSiteRegister(Widget widget, 
		   ArgList arglist, 
		   Cardinal argcount)
{
}

void 
XmDropSiteRetrieve(Widget widget, 
		   ArgList arglist, 
		   Cardinal argcount)
{
}

void 
XmDropSiteStartUpdate(Widget widget)
{
}

void 
XmDropSiteUnregister(Widget widget)
{
}

void 
XmDropSiteUpdate(Widget widget, 
		 ArgList arglist, 
		 Cardinal argcount)
{
}

void 
XmDropTransferAdd(Widget drop_transfer, 
		  XmDropTransferEntryRec *transfers, 
		  Cardinal num_transfers)
{
}

Widget 
XmDropTransferStart(Widget widget, 
		    ArgList arglist, 
		    Cardinal argcount)
{
    Widget dt;
    
    dt = XtCreateManagedWidget("drop_transfer",
			       xmDropTransferObjectClass,
			       widget,
			       arglist,
			       argcount);
    
    return dt;
}

