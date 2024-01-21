/**
 *
 * $Id: GadgetUtil.c,v 1.8 1997/01/11 02:19:46 miers Exp $
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

static char rcsid[] = "$Id: GadgetUtil.c,v 1.8 1997/01/11 02:19:46 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <X11/RectObjP.h>

#include <XmI/DebugUtil.h>

XmGadget 
_XmInputInGadget(Widget cw,
		 int x,
		 int y)
{
    int i;
    XmManagerWidget mw = (XmManagerWidget)cw;

    XdbDebug(__FILE__, cw, "_XmInputInGadget(%d,%d)\n", x, y);

    for (i=0; i<mw->composite.num_children; i++)
    {
	Widget g = MGR_Children(mw)[i];

	if (!XmIsGadget(g) || !XtIsManaged(g))
	    continue;

	if ((x > XtX(g) && x < XtX(g) + XtWidth(g))
	    && (y > XtY(g) && y < XtY(g) + XtHeight(g)))
	{
	    return (XmGadget)g;
	}
    }
    
    return NULL;
}

/* this function seems to have the same purpose as the one above...
   maybe they really _do_ do the same thing... */
/* MLM: Oho, those CSF boys must think they're extremely clever.  A little
   birdy that sometimes sends me email said that one of LessTif's blind spots
   was the XtNsensitive resource, and suggested that I might find calling
   InputFor and InputIn on an insensitive gadget "enlightening", when button
   events occur.  Now all I gotta do is find out if all events are treated
   this way -- see testXm/pushbg/test7.c for details.  Gotta check this
   with KeyPresses too. */
XmGadget 
_XmInputForGadget(Widget cw,
		  int x,
		  int y)
{
    XmGadget g = _XmInputInGadget(cw, x, y);

    if (!g || !XtIsSensitive((Widget)g))
	return NULL;
    else
	return g;
}

void 
_XmConfigureObject(Widget g,
		   Position x,
		   Position y,
		   Dimension width,
		   Dimension height,
		   Dimension border_width)
{
    XdbDebug(__FILE__, g, "_XmConfigureObject X %d Y %d W %d H %d BW %d\n",
	x, y, width, height, border_width);

    if (XmIsGadget(g))
    {
	XmGadget gadget = (XmGadget)g;
	Dimension old_width = XtWidth((Widget)gadget);
	Dimension old_height = XtHeight((Widget)gadget);
	Dimension old_border = gadget->rectangle.border_width;

	XtX((Widget)gadget) = x;
	XtY((Widget)gadget) = y;
	XtWidth((Widget)gadget) = width;
	XtHeight((Widget)gadget) = height;
	gadget->rectangle.border_width = border_width;

	/* only call the resize if the resize method is there
	   and something relevant has changed */
	if ((XtWidth((Widget)gadget) != old_width
	     || XtHeight((Widget)gadget) != old_height
	     || gadget->rectangle.border_width != old_border)
	    && xmGadgetClass->core_class.resize)
	    (*xmGadgetClass->core_class.resize)(g);
    }
    else
    {
	if (XtIsRealized(g)) {
	    if (width == 0)
		width = 1;
	    if (height == 0)
		height = 1;
	}
	XtConfigureWidget(g,
			  x, y,
			  width, height,
			  border_width);
    }
}

/* Motif 2.* version of the above */
void
XmeConfigureObject(Widget g,
		   Position x,
		   Position y,
		   Dimension width,
		   Dimension height,
		   Dimension border_width)
{
	_XmConfigureObject(g, x, y, width, height, border_width);
}

void 
_XmResizeObject(Widget g,
		Dimension width,
		Dimension height,
		Dimension border_width)
{
    XdbDebug(__FILE__, g, "_XmResizeObject W %d H %d\n", width, height);

    if (XmIsGadget(g))
    {
	XmGadget gadget = (XmGadget)g;

	XtWidth((Widget)gadget) = width;
	XtHeight((Widget)gadget) = height;
	gadget->rectangle.border_width = border_width;

	if (xmGadgetClass->core_class.resize)
	    (*xmGadgetClass->core_class.resize)(g);
    }
    else
    {
	XtResizeWidget(g,
		       width, height,
		       border_width);
    }
}

void 
_XmMoveObject(Widget g,
	      Position x,
	      Position y)
{
    XdbDebug(__FILE__, g, "_XmMoveObject X %d Y %d\n", x, y);

    if (XmIsGadget(g))
    {
	XmGadget gadget = (XmGadget)g;

	XtX((Widget)gadget) = x;
	XtY((Widget)gadget) = y;
    }
    else
    {
	XtMoveWidget(g,
		     x,y);
    }    
}

void 
_XmRedisplayGadgets(Widget w,
		    XEvent *event,
		    Region region)
{
    XmManagerWidget mw = (XmManagerWidget)w;
    XExposeEvent *ev = (XExposeEvent *)event;
    int i;

    for (i=0; i<mw->composite.num_children; i++) 
    {
	Widget child = mw->composite.children[i];

	if (XtIsManaged(child) && XmIsGadget(child))
	{
	    if (region && XRectInRegion(region, XtX(child), XtY(child),
					XtWidth(child), XtHeight(child)))
		(*XtClass(child)->core_class.expose)(child, event, region);
	    else if (ev &&
		     XtX(child) < ev->x + ev->width &&
		     XtX(child) + XtWidth(child) > ev->x &&
		     XtY(child) < ev->y + ev->height &&
		     XtY(child) + XtHeight(child) > ev->y)
		(*XtClass(child)->core_class.expose)(child, event, region);
	    else
		(*XtClass(child)->core_class.expose)(child, event, region);
	}
    }
}

/* Motif 2.* version of the above */
void 
XmeRedisplayGadgets(Widget w,
		    XEvent *event,
		    Region region)
{
    _XmRedisplayGadgets(w, event, region);
}

extern void 
_XmDispatchGadgetInput(Widget w,
		       XEvent *event,
		       Mask mask)
{
    XmGadget g = (XmGadget)w;
    XmGadgetClass gc = (XmGadgetClass)w->core.widget_class;

    XdbDebug(__FILE__, w, "_XmDispatchGadgetInput: mask 0x%X, FocusIn %s, Parent %s Manager\n",
	mask,
	(XmFOCUS_IN_EVENT & mask) ? "True" : "False",
	XmIsManager(XtParent(w)) ? "is" : "is no" );

    if (XmIsGadget(w) 
	&& (g->gadget.event_mask & mask))
    {
	XdbDebug(__FILE__, w, "_XmDispatchGadgetInput() dispatching..\n");

	(*gc->gadget_class.input_dispatch)(w, event, mask);
    }
    if ((mask & XmFOCUS_IN_EVENT) && XmIsManager(XtParent(w)))
	MGR_HighlightedWidget(XtParent(w)) = w;

    if ((mask & XmFOCUS_OUT_EVENT) && XmIsManager(XtParent(w)))
	MGR_HighlightedWidget(XtParent(w)) = NULL;
}

extern Time 
__XmGetDefaultTime(Widget w,
		   XEvent *event)
{
    return 0;
}

