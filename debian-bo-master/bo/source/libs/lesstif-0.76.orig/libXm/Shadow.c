/**
 *
 * $Id: Shadow.c,v 1.5 1996/12/11 04:18:34 miers Exp $
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

static char rcsid[] = "$Id: Shadow.c,v 1.5 1996/12/11 04:18:34 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/ManagerP.h>

#include <XmI/DebugUtil.h>

void
_XmClearBorder(Display *display,
	       Window win,
	       Position x,
	       Position y,
	       Dimension width,
	       Dimension height,
	       Dimension shadow_thick)
{
    XClearArea(display, win,
	       x, y, 
	       shadow_thick, height,
	       False);

    XClearArea(display, win,
	       x, y, 
	       width, shadow_thick,
	       False);

    XClearArea(display, win,
	       x + width - shadow_thick, y,
	       shadow_thick, height,
	       False);

    XClearArea(display, win,
	       x, y + height - shadow_thick,
	       width, shadow_thick,
	       False);
}

/* Motif 2.* version of the above */
void
XmeClearBorder(Display *display,
	       Window win,
	       Position x,
	       Position y,
	       Dimension width,
	       Dimension height,
	       Dimension shadow_thick)
{
	_XmClearBorder(display, win, x, y, width, height, shadow_thick);
}

void
_XmDrawShadows(Display *display,
	       Window win,
	       GC topShadowGC,
	       GC bottomShadowGC,
	       Position x, Position y, 
	       Dimension width, Dimension height,
	       Dimension shadowThickness,
	       unsigned int shadowType)
{
    GC topGC = 0, bottomGC = 0;
    int i;
    Cardinal n;

    if (shadowThickness > width || shadowThickness > height)
	shadowThickness = 2;

    switch (shadowType)
    {
    case XmSHADOW_ETCHED_IN:
    case XmSHADOW_IN:
	    topGC = bottomShadowGC;
	    bottomGC = topShadowGC;
	    break;
    case XmSHADOW_ETCHED_OUT:
    case XmSHADOW_OUT:
	    bottomGC = bottomShadowGC;
	    topGC = topShadowGC;
	    break;
    case XmNO_LINE:
	break;
    default:
	n = 0;
        XtAppWarningMsg(XtDisplayToApplicationContext(display),
		        "shadowType", "shadowType",
			"LessTifError",
			"Illegal shadow type sent to _XmDrawShadows",
			NULL, &n);

	break;
    }

    XdbDebug(__FILE__, NULL, "XmDrawShadows(x = %d, y = %d, w = %d, h = %d, s = %d)\n",
	x,y,width, height,shadowThickness);

    if (shadowType == XmNO_LINE) {
	XClearArea(display,
		   win,
		   x, y,
		   shadowThickness ? shadowThickness : 1, height,
		   False);
	XClearArea(display,
		   win,
		   x + width - shadowThickness, y,
		   shadowThickness ? shadowThickness: 1, height,
		   False);
	XClearArea(display,
		   win,
		   x, y,
		   width, shadowThickness ? shadowThickness : 1, 
		   False);
	XClearArea(display,
		   win,
		   x, y+height-shadowThickness,
		   width, shadowThickness ? shadowThickness : 1,
		   False);
	return;
    }

    if (shadowType == XmSHADOW_IN || shadowType == XmSHADOW_OUT) {

       for (i=0; i<shadowThickness; i++) {
           /*
            * draw the vertical lines
            */
           XFillRectangle(display,
                          win,
                          topGC,
                          x + i ,y,
                          1, height - i);

           XFillRectangle(display,
                          win,
                          bottomGC,
                          x+width - i - 1, y + i + 1,
                          1, height - i - 1);

           /*
            * draw the horizontal lines
            */
           XFillRectangle(display,
                          win,
                          topGC,
                          x, y + i,
                          width - i, 1);

           XFillRectangle(display,
                          win,
                          bottomGC,
                          x + i + 1, y+height - i - 1,
                          width - i - 1, 1);
       }
    }
    else { /* SHADOW_ETCHED_IN and SHADOW_ETCHED_OUT */

       for (i = 0; i<shadowThickness/2; i++) {
           /*
            * draw the vertical lines
            */
           XFillRectangle(display,
                          win,
                          topGC,
                          x + i ,y,
                          1, height - i);

           XFillRectangle(display,
                          win,
                          bottomGC,
                          x+width - i - 1, y + i + 1,
                          1, height - i - 1);

           /*
            * draw the horizontal lines
            */
           XFillRectangle(display,
                          win,
                          topGC,
                          x, y + i,
                          width - i, 1);

           XFillRectangle(display,
                          win,
                          bottomGC,
                          x + i + 1, y+height - i - 1,
                          width - i - 1, 1);
       }

       for (; i<shadowThickness; i++) {
           /*
            * draw the vertical lines
            */
           XFillRectangle(display,
                          win,
                          bottomGC,
                          x + i ,y + shadowThickness/2,
                          1, height - i - shadowThickness/2);

           XFillRectangle(display,
                          win,
                          topGC,
                          x + width - i - 1, y + i + 1,
                          1, height - i - 1 - shadowThickness/2);

           /*
            * draw the horizontal lines
            */
           XFillRectangle(display,
                          win,
                          bottomGC,
                          x + shadowThickness/2, y + i,
                          width - i - shadowThickness/2, 1);

           XFillRectangle(display,
                          win,
                          topGC,
                          x + i + 1, y+height - i - 1,
                          width - i - 1 - shadowThickness/2, 1);
       }
    }
}

/* Motif 2.* version of the above */
void
XmeDrawShadows(Display *display,
	       Window win,
	       GC topShadowGC,
	       GC bottomShadowGC,
	       Position x, Position y, 
	       Dimension width, Dimension height,
	       Dimension shadowThickness,
	       unsigned int shadowType)
{
	_XmDrawShadows(display, win, topShadowGC, bottomShadowGC, x, y,
		width, height, shadowThickness, shadowType);
}

void
_XmDrawShadow(Display *display,
		   Drawable d,
		   GC top_gc,
		   GC bottom_gc,
		   Dimension shad_thick,
		   Position x,
		   Position y,
		   Dimension width,
		   Dimension height)
{
    _XmDrawShadows(display, d,
		   top_gc, bottom_gc,
		   x, y,
		   width, height,
		   shad_thick, XmSHADOW_OUT);
}

#define _XmMin(a,b)  ((a) < (b)) ? (a) : (b)

/*
 * this function is icky, nasty, and a couple of other things that aren't
 * decent to put in comments.  The values for the x/y coords of the vertices
 * of the arrow were arrived at strictly by trial and error to get symmetrical
 * shapes.  The values for the shadow drawing are the same way (if not worse).
 * CHANGE THESE AT YOUR OWN RISK!  However, if your server doesn't draw nice
 * symmetric arrows, I want to know (miers@packet.net)
 */
void
_XmDrawArrow(Display *display,
	     Window win,
	     GC bottomGC,
	     GC topGC,
	     GC fillGC,
	     Position x, Position y,
	     Dimension width, Dimension height,
	     Dimension shadowThickness,
	     unsigned char direction)
{
    int x1 = 0,y1 = 0,x2 = 0,y2 = 0,x3 = 0,y3 = 0,x4 = 0,y4 = 0;
    XPoint pts[8];
    int Midx;
    int Midy;
    int half, min;
    XRectangle cliprect;
 
    Midx = x + width / 2;
    Midy = y + height / 2;
 
    min = _XmMin(width,height);
 
    half = min/2;
 
    if (shadowThickness > 2)
 	shadowThickness = 2;
 
    switch(direction){
    case XmARROW_UP:
 	x1 = Midx - half - 1;
 	y1 = Midy - half + min;
 	if (min & 1) {
 	    x2 = Midx;
 	    y2 = Midy - half - 1;
 	    y3 = Midy - half;
 	}
 	else {
 	    x2 = Midx - 1;
 	    y2 = Midy - half;
 	    y3 = Midy - half - 1;
 	}
 	x3 = x2 + 1;
 	x4 = x3 + half;
 	y4 = Midy - half + min;
 	break;
 
    case XmARROW_DOWN:
 	x1 = Midx - half - 1;
 	y1 = Midy - half;
 	if (min & 1) {
 	    x2 = Midx;
 	    y2 = Midy - half + min;
 	    y3 = Midy - half + min - 1;
 	}
 	else {
 	    x2 = Midx - 1;
 	    y2 = Midy - half + min - 1;
 	    y3 = Midy - half + min;
 	}
 	x3 = x2 + 1;
 	x4 = x3 + half;
 	y4 = Midy - half;
 	break;  
    case XmARROW_RIGHT:
 	x1 = Midx - half;
 	y1 = Midy - half - 1;
	if (min & 1) {
 	    x2 = Midx - half + min;
 	    y2 = Midy;
 	    x3 = Midx - half + min - 1;
	}
	else {
 	    x2 = Midx - half + min;
 	    y2 = Midy - 1;
 	    x3 = Midx - half + min;
	}
 	y3 = y2 + 1;
	x4 = Midx - half;
	y4 = Midy - half + min;
 	break;  
    case XmARROW_LEFT:
 	x1 = Midx - half + min;
 	y1 = Midy - half - 1;
	if (width & 1) {
 	    x2 = Midx - half - 1;
 	    y2 = Midy;
 	    x3 = Midx - half + 1;
	}
	else {
 	    x2 = Midx - half;
 	    y2 = Midy - 1;
 	    x3 = Midx - half;
	}
 	y3 = y2 + 1;
	x4 = Midx - half + min;
	y4 = Midy - half + min;
 	break;
    }
 
    pts[0].x = (short)x1;
    pts[0].y = (short)y1;
    pts[1].x = (short)x2;
    pts[1].y = (short)y2;
    pts[2].x = (short)x3;
    pts[2].y = (short)y3;
    pts[3].x = (short)x4;
    pts[3].y = (short)y4;
 
    /* Ensure we don't draw beyond our boundaries */
    cliprect.x = Midx - half; cliprect.y = Midy - half;
    cliprect.width = min; cliprect.height = min;
 
    if (fillGC)
 	XSetClipRectangles(display,fillGC,0,0,&cliprect,1,Unsorted);
    XSetClipRectangles(display,topGC,0,0,&cliprect,1,Unsorted);
    XSetClipRectangles(display,bottomGC,0,0,&cliprect,1,Unsorted);
    if (fillGC)
	XFillPolygon(display, win, fillGC, pts, 4, CoordModeOrigin, Complex);
 
    if (shadowThickness == 0) {
 	/* Remove the clip masks */
 	if (fillGC)
 	    XSetClipMask(display,fillGC,None);
 	XSetClipMask(display,topGC,None);
 	XSetClipMask(display,bottomGC,None);
 	return;
    }
 
    /* normally dark */
    switch(direction){
    case XmARROW_UP:
 	XDrawLine(display, win, bottomGC, x4,y4-1,x1-1,y1-1);
 	if (shadowThickness == 2)
 	    XDrawLine(display, win, bottomGC, x4,y4-2,x1,y1-2);
 
 	XDrawLine(display, win, bottomGC, x3-1, y3,  x4,y4+1);
 	if (shadowThickness == 2)
 	    XDrawLine(display, win, bottomGC, x3, y3+3,  x4-1,y4);
 
 	XDrawLine(display, win, topGC, x1+1,y1-1,x2+1,y2-1);
 	XDrawLine(display, win, topGC, x2+1,y2,x3+1,y3);
 	if (shadowThickness == 2)
 	    XDrawLine(display, win, topGC, x1+3,y1-3,x2,y2+2);
 
 	break;
    case XmARROW_DOWN:
 	XDrawLine(display, win, topGC, x1, y1-2, x2+1, y2+1);
 	if (shadowThickness == 2)
 	    XDrawLine(display, win, topGC, x1+2, y1, x2, y2-2);
 
 	if (width & 1)
 	    XDrawLine(display, win, bottomGC, x2-1,y2+1,x3,y3-2);
 	else
 	    XDrawLine(display, win, bottomGC, x2,y2,x3+1,y3);
 
 	XDrawLine(display, win, bottomGC, x3,y3-2,x4,y4-2);
 	if (shadowThickness == 2)
 	    XDrawLine(display, win, bottomGC, x3-1,y3-1,x4-2,y4+1);
 
 	XDrawLine(display, win, topGC, x4,y4,x1,y1);
 	if (shadowThickness == 2)
 	    XDrawLine(display, win, topGC, x4-2,y4+1,x1+1,y1+1);
 
 	break;
    case XmARROW_RIGHT:
	if (min & 1)
 	    XDrawLine(display, win, topGC, x1-2,y1,x2-1,y2);
	else
 	    XDrawLine(display, win, topGC, x1,y1+1,x2,y2+1);

 	XDrawLine(display, win, bottomGC, x2,y2-1,x3,y3-1);
 	XDrawLine(display, win, bottomGC, x4,y4-1, x3,y3-1);

 	XDrawLine(display, win, topGC, x4,y4,x1,y1);

 	break;
    case XmARROW_LEFT:
 	XDrawLine(display, win, bottomGC, x3-1,y3-1,x4-1,y4-1);

 	XDrawLine(display, win, topGC, x2+1,y2,x1+1,y1);
	XDrawLine(display, win, topGC, x2, y2, x3, y3-1);

 	XDrawLine(display, win, bottomGC, x1-1,y1+2,x4-1,y4);
 	break;
    }
 
    /* Remove the clip masks */
    if (fillGC)
	XSetClipMask(display,fillGC,None);
    XSetClipMask(display,topGC,None);
    XSetClipMask(display,bottomGC,None);
}

/* Motif 2.* version of the above */
void
XmeDrawArrow(Display *display,
	     Window win,
	     GC bottomGC,
	     GC topGC,
	     GC fillGC,
	     Position x, Position y,
	     Dimension width, Dimension height,
	     Dimension shadowThickness,
	     unsigned char direction)
{
	_XmDrawArrow(display, win, bottomGC, topGC, fillGC,
		x, y, width, height, shadowThickness, direction);
}

void
_XmDrawDiamond(Display *display,
	       Window win,
	       GC bottomGC,
	       GC topGC,
	       GC selectGC,
	       Position x, Position y,
	       Dimension width, Dimension height,
	       Dimension shadowThickness,
	       Dimension fill)
{
    int i;
    int x_edge, y_edge;
    int ox,oy;
    XPoint pts[5];

    x_edge = width/2;
    y_edge = height/2;

    ox = x + x_edge;
    oy = y + y_edge;

    pts[0].x = (short)ox;
    pts[0].y = (short)oy-y_edge;
    pts[1].x = (short)ox+x_edge;
    pts[1].y = (short)oy;
    pts[2].x = (short)ox;
    pts[2].y = (short)oy+y_edge;
    pts[3].x = (short)ox-x_edge;
    pts[3].y = (short)oy;
    pts[4].x = (short)ox;
    pts[4].y = (short)oy-y_edge;
   
    if (fill)
	XFillPolygon(display, win, selectGC ,pts,4,CoordModeOrigin, Complex);

    for (i=0; i<shadowThickness; i++)
    {
        XDrawLine(display, win, topGC, ox, oy - y_edge - i, ox + x_edge + i, oy);
        XDrawLine(display, win, topGC, ox - x_edge - i, oy, ox, oy - y_edge - i);
    }

    for (i=0; i<shadowThickness; i++)
    {
        XDrawLine(display, win, bottomGC, ox, oy + y_edge + i, ox - x_edge - i, oy);
        XDrawLine(display, win, bottomGC, ox + x_edge + i, oy, ox, oy + y_edge + i);
    }
}

/* Motif 2.* version of the above */
void
XmeDrawDiamond(Display *display,
	       Window win,
	       GC bottomGC,
	       GC topGC,
	       GC selectGC,
	       Position x, Position y,
	       Dimension width, Dimension height,
	       Dimension shadowThickness,
	       Dimension fill)
{
	_XmDrawDiamond(display, win, bottomGC, topGC, selectGC, x, y, width, height,
	       shadowThickness, fill);
}

void 
_XmDrawSeparator(Display *display, 
		 Drawable win, 
		 GC top_gc, 
		 GC bottom_gc,	
		 GC separator_GC, 
		 Position wx, 
		 Position wy, 
		 Dimension wwidth,	
		 Dimension wheight, 
		 Dimension shadowThickness, 
		 Dimension margin,	
		 unsigned char orientation, 
		 unsigned char separator_type)
{
    int x,y;
    int x1 = 0,y1 = 0,x2 = 0,y2 = 0,x3 = 0,y3 = 0,x4 = 0,y4 = 0;
    int width, height, i;

#ifdef	DO_FLUSH
    XFlush(display);
#endif

    if (orientation == XmHORIZONTAL)
    {
	x = wx + margin;
	y = wy + wheight/2;
	width = wwidth - 2 * margin;
	if (separator_type  == XmDOUBLE_LINE ||
	    separator_type  == XmDOUBLE_DASHED_LINE) {
	    y2 = y1 = y;
	    y3 = y4 = y+1;
	    x3 = x1 = x;
	    x4 = x2 = x+width;
	}
	else if (separator_type  == XmSHADOW_ETCHED_OUT ||
		 separator_type  == XmSHADOW_ETCHED_IN ||
		 separator_type  == XmSHADOW_ETCHED_IN_DASH ||
		 separator_type  == XmSHADOW_ETCHED_OUT_DASH) {
	    y2 = y1 = y - shadowThickness/2;
	    y3 = y4 = y;
	    x3 = x1 = x;
	    x4 = x2 = x+width;
	}
	else
	{
	    x1 = x;
	    y1 = y;
	    x2 = x+width;
	    y2 = y;
        }
    }
    else
    {
	x = wx + wwidth/2;
	y = wy + margin;
	height = wheight - 2*margin;
	y1 = y+height;
	x1 = x;
	if (separator_type  == XmDOUBLE_LINE ||
	    separator_type  == XmDOUBLE_DASHED_LINE) {
	    y3 = y1 = y;
	    y2 = y4 = y+height;
	    x2 = x1 = x;
	    x4 = x3 = x+1;
        }
	else if (separator_type  == XmSHADOW_ETCHED_OUT ||
		 separator_type  == XmSHADOW_ETCHED_IN ||
		 separator_type  == XmSHADOW_ETCHED_IN_DASH ||
		 separator_type  == XmSHADOW_ETCHED_OUT_DASH){
	    y3 = y1 = y;
	    y2 = y4 = y+height;
	    x2 = x1 = x - shadowThickness/2;
	    x4 = x3 = x;
        }
	else
	{
	    x1 = x;
	    y1 = y;
	    x2 = x;
	    y2 = y+height;
        }
    }

    switch(separator_type)
    {
    case XmSINGLE_LINE:
    	XDrawLine(display, win, separator_GC, x1,y1,x2,y2);
        break;
    case XmSINGLE_DASHED_LINE:
    	XDrawLine(display, win, separator_GC, x1,y1,x2,y2);
	break;
    case XmDOUBLE_LINE:
    	XDrawLine(display, win, separator_GC, x1,y1,x2,y2);
    	XDrawLine(display, win, separator_GC, x3,y3,x4,y4);
	break;
    case XmDOUBLE_DASHED_LINE:
    	XDrawLine(display, win, separator_GC, x1,y1,x2,y2);
    	XDrawLine(display, win, separator_GC, x3,y3,x4,y4);
	break;
    case XmSHADOW_ETCHED_IN:
        if (orientation == XmVERTICAL) {
	    for (i = 0; i < shadowThickness/2; i++) {
		XDrawLine(display, win, bottom_gc,
			  x1 + i, y1 + (shadowThickness/2 - i - 1),
			  x2 + i, y2 - (shadowThickness/2 - i - 1));
	    }
	    for (i = 0; i < shadowThickness/2; i++) {
		XDrawLine(display, win, top_gc,
			  x3 + i, y3 + i,
			  x4 + i, y4 - i);
	    }
	}
	else {
	    for (i = 0; i < shadowThickness/2; i++) {
		XDrawLine(display, win, bottom_gc,
			  x1 + (shadowThickness/2 - i - 1), y1 + i,
			  x2 - (shadowThickness/2 - i - 1), y2 + i);
	    }
	    for (i = 0; i < shadowThickness/2; i++) {
		XDrawLine(display, win, top_gc,
			  x3 + i, y3 + i,
			  x4 - i, y4 + i);
	    }
	}
        break;
    case XmSHADOW_ETCHED_OUT:
	if (orientation == XmVERTICAL) {
	    for (i = 0; i < shadowThickness/2; i++)
		XDrawLine(display, win, top_gc,
			  x1 + i, y1 + (shadowThickness/2 - i - 1),
			  x2 + i, y2 - (shadowThickness/2 - i - 1));
	    for (i = 0; i < shadowThickness/2; i++)
		XDrawLine(display, win, bottom_gc,
			  x3 + i, y3 + i,
			  x4 + i, y4 - i);
	}
	else {
	    for (i = 0; i < shadowThickness/2; i++)
		XDrawLine(display, win, top_gc,
			  x1 + (shadowThickness/2 - i - 1), y1 + i,
			  x2 - (shadowThickness/2 - i - 1), y2 + i);
	    for (i = 0; i < shadowThickness/2; i++)
		XDrawLine(display, win, bottom_gc,
			  x3 + i, y3 + i,
			  x4 - i, y4 + i);
	}
	break;
    case XmSHADOW_ETCHED_IN_DASH:
	XSetLineAttributes(display, top_gc, 0, LineDoubleDash,
			   CapButt,JoinMiter);
	XSetLineAttributes(display, bottom_gc, 0, LineDoubleDash,
			   CapButt,JoinMiter);
        if (orientation == XmVERTICAL) {
	    for (i = 0; i < shadowThickness/2; i++) {
		XDrawLine(display, win, bottom_gc,
			  x1 + i, y1 + (shadowThickness/2 - i - 1),
			  x2 + i, y2 - (shadowThickness/2 - i - 1));
	    }
	    for (i = 0; i < shadowThickness/2; i++) {
		XDrawLine(display, win, top_gc,
			  x3 + i, y3 + i,
			  x4 + i, y4 - i);
	    }
	}
	else {
	    for (i = 0; i < shadowThickness/2; i++) {
		XDrawLine(display, win, bottom_gc,
			  x1 + (shadowThickness/2 - i - 1), y1 + i,
			  x2 - (shadowThickness/2 - i - 1), y2 + i);
	    }
	    for (i = 0; i < shadowThickness/2; i++) {
		XDrawLine(display, win, top_gc,
			  x3 + i, y3 + i,
			  x4 - i, y4 + i);
	    }
	}
	break;
    case XmSHADOW_ETCHED_OUT_DASH:
	if (orientation == XmVERTICAL) {
	    for (i = 0; i < shadowThickness/2; i++)
		XDrawLine(display, win, top_gc,
			  x1 + i, y1 + (shadowThickness/2 - i - 1),
			  x2 + i, y2 - (shadowThickness/2 - i - 1));
	    for (i = 0; i < shadowThickness/2; i++)
		XDrawLine(display, win, bottom_gc,
			  x3 + i, y3 + i,
			  x4 + i, y4 - i);
	}
	else {
	    for (i = 0; i < shadowThickness/2; i++)
		XDrawLine(display, win, top_gc,
			  x1 + (shadowThickness/2 - i - 1), y1 + i,
			  x2 - (shadowThickness/2 - i - 1), y2 + i);
	    for (i = 0; i < shadowThickness/2; i++)
		XDrawLine(display, win, bottom_gc,
			  x3 + i, y3 + i,
			  x4 - i, y4 + i);
	}
	break;
    case XmNO_LINE:
	break;
    default:
    	XDrawLine(display, win, separator_GC, x,y,x1,y1);
	break;
    }
}

/* Motif 2.* version of the above */
void 
XmeDrawSeparator(Display *display, 
		 Drawable win, 
		 GC top_gc, 
		 GC bottom_gc,	
		 GC separator_GC, 
		 Position wx, 
		 Position wy, 
		 Dimension wwidth,	
		 Dimension wheight, 
		 Dimension shadowThickness, 
		 Dimension margin,	
		 unsigned char orientation, 
		 unsigned char separator_type)
{
	_XmDrawSeparator(display, win, top_gc, bottom_gc, separator_GC, 
		 wx, wy, wwidth, wheight, shadowThickness, margin, orientation, separator_type);
}

void
_XmDrawSimpleHighlight(Display *display,
                       Drawable d,
                       GC gc,
                       Position x,
                       Position y,
                       Dimension width,
                       Dimension height,
                       Dimension highlight_thick)
{
}

void
_XmDrawHighlight(Display *display,
                 Drawable d,
                 GC gc,
                 Position x,
                 Position y,
                 Dimension width,
                 Dimension height,
                 Dimension highlight_thick,
                 int line_style)
{
    int i;
    static      char dash_list[] = { 8, 8 };

    /* this is dangerous, but the same as the Manger and Primitive */
    XSetLineAttributes(display, gc, 0, line_style, CapButt, JoinMiter);

    switch (line_style) {
    case LineOnOffDash:
    case LineDoubleDash:
        XSetDashes(display, gc, 0, dash_list, 2);
        for (i = 0; i < highlight_thick; i++) {
            XDrawLine(display, (Window)d, gc, 
                  x, y+i, x+width-1, y+i);
        
            XDrawLine(display, (Window)d, gc, 
                  x, y+height-1-i, x+width-1, y+height-1-i);
        
            XDrawLine(display, (Window)d, gc, 
                  x+i, y, x+i, y+height-1);
        
            XDrawLine(display, (Window)d, gc, 
                  x+width-1-i, y, x+width-1-i, y+height-1);
        }
	break;

    case LineSolid:
    default:
        for (i = 0; i < highlight_thick; i++) {
            XDrawRectangle(display, (Window)d, gc,
                       x + i, y + i,
                       width - (2*i) - 1, height - (2*i) - 1);
        }
        break;
    }
}

/* Motif 2.* version of the above */
void
XmeDrawHighlight(Display *display,
                 Drawable d,
                 GC gc,
                 Position x,
                 Position y,
                 Dimension width,
                 Dimension height,
                 Dimension highlight_thick,
                 int line_style)
{
	_XmDrawHighlight(display, d, gc, x, y, width, height, highlight_thick, line_style);
}
