/**
 *
 * $Id: Visual.c,v 1.3 1996/11/28 09:22:24 u27113 Exp $
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

static char rcsid[] = "$Id: Visual.c,v 1.3 1996/11/28 09:22:24 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/ManagerP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/GadgetP.h>

#include <XmI/DebugUtil.h>

static XmColorProc _color_proc = NULL;

static void _XmColorProcDefaultProc(XColor *bg_color, XColor *fg_color,
				    XColor *sel_color, XColor *ts_color,
				    XColor *bs_color);

/*
 * forward declarations
 */
void __XmRGB2HSV(unsigned short r, 
		 unsigned short g, 
		 unsigned short b, 
		 double *hr, 
		 double *sr, 
		 double *vr);

XmColorProc 
XmSetColorCalculation(XmColorProc proc)
{
    XmColorProc ret_val;

    ret_val = _color_proc;

    if (proc == NULL)
	_color_proc = _XmColorProcDefaultProc;
    else
	_color_proc = proc;

    return ret_val;
}

XmColorProc
XmGetColorCalculation()
{
    return _color_proc;
}

void
XmGetColors(Screen *screen,
	    Colormap color_map,
	    Pixel background,
	    Pixel *foreground_ret,
	    Pixel *top_shadow_ret,
	    Pixel *bottom_shadow_ret,
	    Pixel *select_ret)
{
    XColor background_xcolor;
    XColor foreground_xcolor;
    XColor top_shadow_xcolor;
    XColor bottom_shadow_xcolor;
    XColor select_xcolor;

    background_xcolor.pixel = background;

    XQueryColor(XDisplayOfScreen(screen),
		color_map,
		&background_xcolor);

    _color_proc(&background_xcolor,
		&foreground_xcolor,
		&select_xcolor,
		&top_shadow_xcolor,
		&bottom_shadow_xcolor);

    if (foreground_ret) {
	XAllocColor(XDisplayOfScreen(screen),
		    color_map,
		    &foreground_xcolor);

	*foreground_ret = foreground_xcolor.pixel;
    }
    if (select_ret) {
	XAllocColor(XDisplayOfScreen(screen),
		    color_map,
		    &select_xcolor);

	*select_ret = select_xcolor.pixel;
    }
    if (top_shadow_ret) {
	XAllocColor(XDisplayOfScreen(screen),
		    color_map,
		    &top_shadow_xcolor);

	*top_shadow_ret = top_shadow_xcolor.pixel;
    }
    if (bottom_shadow_ret) {
	XAllocColor(XDisplayOfScreen(screen),
		    color_map,
		    &bottom_shadow_xcolor);

	*bottom_shadow_ret = bottom_shadow_xcolor.pixel;
    }
}

void
XmChangeColor(Widget widget,
	      Pixel background)
{
    Pixel foreground;
    Pixel top_shadow;
    Pixel bottom_shadow;
    Pixel select;
    Colormap color_map;

    XtVaGetValues(widget,
		  XmNcolormap, &color_map,
		  NULL);

    XmGetColors(XtScreenOfObject(widget),
		color_map,
		background,
		&foreground,
		&top_shadow,
		&bottom_shadow,
		&select);

    XtVaSetValues(widget,
		  XmNbackground, background,
		  XmNforeground, foreground,
		  XmNtopShadowColor, top_shadow,
		  XmNbottomShadowColor, bottom_shadow,
		  XmNarmColor, select,
		  NULL);
}

/* the default color calculation */
static void 
_XmColorProcDefaultProc(XColor *bg_color, 
			XColor *fg_color,
			XColor *sel_color, 
			XColor *ts_color,
			XColor *bs_color)
{
    double h,s,v;
    XColor _widgetBackground;

    _widgetBackground = *bg_color;

    __XmRGB2HSV(_widgetBackground.red,
		_widgetBackground.green,
		_widgetBackground.blue,
		&h,&s,&v);

    if (v<0.5) {
	fg_color->blue = 0xFFFF;
	fg_color->green = 0xFFFF;
	fg_color->red = 0xFFFF;
    }
    else {
	fg_color->blue = 0x0;
	fg_color->green = 0x0;
	fg_color->red = 0x0;
    }

    /* FIX THESE -- they're the same as the bottom shadow color */
    sel_color->blue = _widgetBackground.blue * .7;
    sel_color->green = _widgetBackground.green * .7;
    sel_color->red = _widgetBackground.red * .7;

    if (_widgetBackground.red > 65535/1.5)
        ts_color->red = 65535;
    else
        ts_color->red = _widgetBackground.red * 1.5;

    if (_widgetBackground.green > 65535/1.5)
        ts_color->green = 65535;
    else
        ts_color->green = _widgetBackground.green * 1.5;

    if (_widgetBackground.blue > 65535/1.5)
	ts_color->blue = 65535;
    else
        ts_color->blue = _widgetBackground.blue * 1.5;

    bs_color->blue = _widgetBackground.blue * .5;
    bs_color->green = _widgetBackground.green * .5;
    bs_color->red = _widgetBackground.red * .5;
}

/****************************************/
/* private functions for default colors */
/****************************************/

#define FLOOR(x) ((int)((x) - 0.5) > (x) ? (x) : (x)-1)

/* Convert between RGB and HSV */
void 
__XmRGB2HSV(unsigned short r, 
	    unsigned short g, 
	    unsigned short b, 
	    double *hr, 
	    double *sr, 
	    double *vr)
{
    double rd, gd, bd, max, min, del;
    double rc, gc, bc;
    double v, h, s;

    /* convert RGB to HSV */
    rd = r / 65535.0;            /* rd,gd,bd range 0-1 instead of 0-65535 */
    gd = g / 65535.0;
    bd = b / 65535.0;
    
    /* compute maximum of rd,gd,bd */
    if (rd>=gd) { if (rd>=bd) max = rd;  else max = bd; }
    else { if (gd>=bd) max = gd;  else max = bd; }

    /* compute minimum of rd,gd,bd */
    if (rd<=gd) { if (rd<=bd) min = rd;  else min = bd; }
    else { if (gd<=bd) min = gd;  else min = bd; }


    del = max - min;
    v = max;
    if (max != 0.0) s = (del) / max;
    else s = 0.0;

    h = -1;
    if (s != 0.0) {
	rc = (max - rd) / del;
	gc = (max - gd) / del;
	bc = (max - bd) / del;

	if      (rd==max) h = bc - gc;
	else if (gd==max) h = 2 + rc - bc;
	else if (bd==max) h = 4 + gc - rc;

	h = h * 60;
	if (h<0) h += 360;
    }

    *hr = h;  *sr = s;  *vr = v;
}

/* Convert between HSV and RGB */
void 
_XmHSV2RGB(double h, 
	   double s, 
	   double v,
	   unsigned short *rr, 
	   unsigned short *gr, 
	   unsigned short *br)
{
    int    j;
    double rd, gd, bd;
    double f, p, q, t;

    /* convert HSV back to RGB */
    if (h==-1 || s==0.0) { rd = v;  gd = v;  bd = v; }
    else {
	if (h==360.0) h = 0.0;
	h = h / 60.0;
	j = (int) FLOOR(h);
	if (j<0) j=0;          /* either h or floor seem to go neg on some sys */
	f = h - j;
	p = v * (1-s);
	q = v * (1 - (s*f));
	t = v * (1 - (s*(1 - f)));

	switch (j) {
	case 0:  rd = v;  gd = t;  bd = p;  break;
	case 1:  rd = q;  gd = v;  bd = p;  break;
	case 2:  rd = p;  gd = v;  bd = t;  break;
	case 3:  rd = p;  gd = q;  bd = v;  break;
	case 4:  rd = t;  gd = p;  bd = v;  break;
	case 5:  rd = v;  gd = p;  bd = q;  break;
	default: rd = v;  gd = t;  bd = p;  break;  /* never happen */
	}
    }


    *rr = (unsigned short) FLOOR((rd * 65535.0) + 0.5);
    *gr = (unsigned short) FLOOR((gd * 65535.0) + 0.5);
    *br = (unsigned short) FLOOR((bd * 65535.0) + 0.5);
}


void 
_XmForegroundColorDefault(Widget w,
			  int offset,
			  XrmValue *val)
{
    double h,s,v;
    XColor _widgetBackground;
    static XColor foregroundColor;

    if (XmIsGadget(w))
	_widgetBackground.pixel = XmParentBackground(w);
    else
	_widgetBackground.pixel = XtBackground(w);

    XQueryColor(XtDisplayOfObject(w),
		DefaultColormapOfScreen(XtScreenOfObject(w)),
		&_widgetBackground);

    __XmRGB2HSV(_widgetBackground.red,
		_widgetBackground.green,
		_widgetBackground.blue,
		&h,&s,&v);

    if (v<0.5)
	foregroundColor.pixel = WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplayOfObject(w)));
    else
	foregroundColor.pixel = BlackPixelOfScreen(DefaultScreenOfDisplay(XtDisplayOfObject(w)));

    XQueryColor(XtDisplayOfObject(w),
		DefaultColormapOfScreen(XtScreenOfObject(w)),
		&foregroundColor);

    val->addr = (XtPointer)&foregroundColor.pixel;
}

void
_XmHighlightColorDefault(Widget w,
			 int offset,
			 XrmValue *val)
{
    double h,s,v;
    XColor _widgetBackground;
    static XColor highlightColor;

    if (XmIsGadget(w))
	_widgetBackground.pixel = XtBackground(XtParent(w));
    else
	_widgetBackground.pixel = XtBackground(w);

    XQueryColor(XtDisplayOfObject(w),
		DefaultColormapOfScreen(XtScreenOfObject(w)),
		&_widgetBackground);

    __XmRGB2HSV(_widgetBackground.red,
		_widgetBackground.green,
		_widgetBackground.blue,
		&h,&s,&v);

    if (v<0.5)
	highlightColor.pixel = WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplayOfObject(w)));
    else
	highlightColor.pixel = BlackPixelOfScreen(DefaultScreenOfDisplay(XtDisplayOfObject(w)));

    XQueryColor(XtDisplayOfObject(w),
		DefaultColormapOfScreen(XtScreenOfObject(w)),
		&highlightColor);

    val->addr = (XtPointer)&highlightColor.pixel;
}

void 
_XmBackgroundColorDefault(Widget w,
			  int offset,
			  XrmValue *val)
{
    static XColor backgroundColor;

    if (!XParseColor(XtDisplayOfObject(w),
		     DefaultColormapOfScreen(XtScreenOfObject(w)),
		     "rgb:72/9f/ff",
		     &backgroundColor))
	backgroundColor.pixel = WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplayOfObject(w)));
    else
    {
	XAllocColor(XtDisplayOfObject(w),
		    DefaultColormapOfScreen(XtScreenOfObject(w)),
		    &backgroundColor);
    }

    val->addr = (XtPointer)&backgroundColor.pixel;
}


void 
_XmTopShadowColorDefault(Widget w,
			 int offset,
			 XrmValue *val) 
{
    XColor _widgetBackground;
    static XColor topShadowColor;

    if (XmIsGadget(w))
	_widgetBackground.pixel = XtBackground(XtParent(w));
    else
	_widgetBackground.pixel = XtBackground(w);

    XQueryColor(XtDisplayOfObject(w),
		DefaultColormapOfScreen(XtScreenOfObject(w)),
		&_widgetBackground);

    if (_widgetBackground.red > 65535/1.5)
        topShadowColor.red = 65535;
    else
        topShadowColor.red = _widgetBackground.red * 1.5;

    if (_widgetBackground.green > 65535/1.5)
        topShadowColor.green = 65535;
    else
        topShadowColor.green = _widgetBackground.green * 1.5;

    if (_widgetBackground.blue > 65535/1.5)
	topShadowColor.blue = 65535;
    else
        topShadowColor.blue = _widgetBackground.blue * 1.5;


    if (!XAllocColor(XtDisplayOfObject(w),
		     DefaultColormapOfScreen(XtScreenOfObject(w)),
		     &topShadowColor))
	topShadowColor.pixel = WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplayOfObject(w)));

    val->addr = (XtPointer)&topShadowColor.pixel;    
}


void 
_XmBottomShadowColorDefault(Widget w,
			    int offset,
			    XrmValue *val)
{
    XColor _widgetBackground;
    static XColor bottomShadowColor;

    if (XmIsGadget(w))
	_widgetBackground.pixel = XtBackground(XtParent(w));
    else
	_widgetBackground.pixel = XtBackground(w);

    XQueryColor(XtDisplayOfObject(w),
		DefaultColormapOfScreen(XtScreenOfObject(w)),
		&_widgetBackground);

    bottomShadowColor.blue = _widgetBackground.blue * .5;
    bottomShadowColor.green = _widgetBackground.green * .5;
    bottomShadowColor.red = _widgetBackground.red * .5;

    if (!XAllocColor(XtDisplayOfObject(w),
		     DefaultColormapOfScreen(XtScreenOfObject(w)),
		     &bottomShadowColor))
	bottomShadowColor.pixel = WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplayOfObject(w)));

    val->addr = (XtPointer)&bottomShadowColor.pixel;    
}

void
_XmRegisterPixmapConverters()
{
    XdbDebug(__FILE__, NULL, "_XmRegisterPixmapConverters\n");
}

/*
 * XmP.h says this is here
 */
void 
_XmSelectColorDefault(Widget w,
		      int offset,
		      XrmValue *val)
{
    XColor _widgetBackground;
    static XColor selectColor;

    if (XmIsGadget(w))
	_widgetBackground.pixel = XmParentBackground(w);
    else if (XmIsExtObject(w))
	_widgetBackground.pixel = XmParentBackground(w);
    else
	_widgetBackground.pixel = XtBackground(w);

    XQueryColor(XtDisplayOfObject(w),
		DefaultColormapOfScreen(XtScreenOfObject(w)),
		&_widgetBackground);

    selectColor.blue = _widgetBackground.blue * .7;  /* FIX THESE -- they're the same as the bottom shadow color */
    selectColor.green = _widgetBackground.green * .7;
    selectColor.red = _widgetBackground.red * .7;

    if (!XAllocColor(XtDisplayOfObject(w),
		     DefaultColormapOfScreen(XtScreenOfObject(w)),
		     &selectColor))
	selectColor.pixel = WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplayOfObject(w)));


    val->addr = (XtPointer)&selectColor.pixel;
}


