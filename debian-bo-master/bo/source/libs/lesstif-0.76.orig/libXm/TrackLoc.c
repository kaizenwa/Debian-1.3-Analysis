/**
 *
 * $Id: TrackLoc.c,v 1.3 1996/11/28 09:22:16 u27113 Exp $
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

static char rcsid[] = "$Id: TrackLoc.c,v 1.3 1996/11/28 09:22:16 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>
#include <Xm/XmP.h>

#include <XmI/DebugUtil.h>

Widget
XmTrackingEvent(Widget widget,
                Cursor cursor,
                Boolean confine_to,
                XEvent *event_return)
{
    Window confine_to_this;
    XEvent ev;

    if (confine_to)
	confine_to_this = XtWindow(widget);
    else
	confine_to_this = None;

    XdbDebug(__FILE__, widget, "XmTrackingEvent: XtGrabPointer()\n");
    XtGrabPointer(widget,
		 True,
		 ButtonReleaseMask | ButtonPressMask,
		 GrabModeAsync,
		 GrabModeAsync,
		 confine_to_this,
		 cursor,
		 CurrentTime);

    while (True)
    {
	XtAppNextEvent(XtWidgetToApplicationContext(widget), &ev);

	if (ev.xbutton.type == KeyRelease
	    || (ev.xbutton.type == ButtonRelease
		&& ev.xbutton.button == 1))
	{
	    XdbDebug(__FILE__, widget, "XmTrackingEvent: XtUngrabPointer()\n");
	    XtUngrabPointer(widget, CurrentTime);

	    /* If the button was clicked outside of this programs windows, the widget
	       that grabbed the pointer will get the event.  So, we check the bounds of
	       the widget against the coordinates of the event.  If they're outside, we
	       return NULL.  Otherwise we return the widget in which the event occured. */
	    
	    if (ev.xbutton.window == XtWindow(widget)
		&& (ev.xbutton.x < XtX(widget) 
		    || ev.xbutton.y < XtY(widget) 
		    || ev.xbutton.x > XtX(widget) + XtWidth(widget)
		    || ev.xbutton.y > XtY(widget) + XtHeight(widget)))
		return NULL;
	    else
		return XtWindowToWidget(XtDisplay(widget),
					ev.xbutton.window);
	}
    }
}

Widget 
XmTrackingLocate(Widget widget,
		 Cursor cursor,
		 Boolean confine_to)
{
    XEvent ev;

    return XmTrackingEvent(widget,
			   cursor,
			   confine_to,
			   &ev);
}
