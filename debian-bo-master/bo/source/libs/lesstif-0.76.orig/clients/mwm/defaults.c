/**
 *
 * $Id: defaults.c,v 1.9 1996/11/11 03:36:52 miers Exp $
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
#include "mwm.h"
#include "mwmstrings.h"

/*
 * This stuff will get used as I write stuff
 *
 Icons
 "Pack Icons" _P  Shift Alt<Key>F7 f.pack_icons
 XBMLANGPATH
 * function names came from running strings against mwm on Solaris
 */
void *working_base = NULL;
ScreenInfo *rscr = NULL;

#if 1
extern void __XmRGB2HSV();
#else
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
#endif

void
_WmMultiClickTimeDefault(Widget widget, int offset, XrmValue * val)
{
    static int time;

    time = XtGetMultiClickTime(dpy);
    val->addr = (XtPointer)&time;
}

void
_WmDefaultBorderWidth(Widget widget, int offset, XrmValue * val)
{
    static Dimension border_width;

    border_width = 5;
    val->addr = (XtPointer)&border_width;
}

void
_WmDefaultResizeBorderWidth(Widget widget, int offset, XrmValue * val)
{
    static Dimension border_width;

    border_width = Mwm.frame_border_width + 3;
    val->addr = (XtPointer)&border_width;
}

void
_WmFocusAutoRaiseDefault(Widget widget, int offset, XrmValue * val)
{
    static Boolean raise;

    if (Mwm.keyboard_focus_policy == XmEXPLICIT)
	raise = True;
    else if (Mwm.keyboard_focus_policy == XmPOINTER)
	raise = False;
    else {
	_XmWarning(toplevel, "Keyboard Focus Policy is unknown.\n");
	raise = True;
    }
    val->addr = (XtPointer)&raise;
}

void
_WmIconImageBDefault(Widget widget, int offset, XrmValue * val)
{
    val->addr = (XtPointer)&rscr->components[MWM_ICON].background;
}

void
_WmIconImageBSCDefault(Widget widget, int offset, XrmValue * val)
{
    val->addr = (XtPointer)&rscr->components[MWM_ICON].bottom_shadow_color;
}

void
_WmIconImageBSPDefault(Widget widget, int offset, XrmValue * val)
{
    val->addr = (XtPointer)&rscr->components[MWM_ICON].bottom_shadow_pixmap;
}

void
_WmIconImageFDefault(Widget widget, int offset, XrmValue * val)
{
    val->addr = (XtPointer)&rscr->components[MWM_ICON].foreground;
}

void
_WmIconImageTSCDefault(Widget widget, int offset, XrmValue * val)
{
    val->addr = (XtPointer)&rscr->components[MWM_ICON].top_shadow_color;
}

void
_WmIconImageTSPDefault(Widget widget, int offset, XrmValue * val)
{
    val->addr = (XtPointer)&rscr->components[MWM_ICON].top_shadow_pixmap;
}

void
_WmMatteBDefault(Widget widget, int offset, XrmValue * val)
{
    static XColor backgroundColor;

    if (!XParseColor(XtDisplay(toplevel),
                     DefaultColormap(dpy, rscr->screen),
		     "LightGrey",
                     &backgroundColor)) {
	if (!XParseColor(XtDisplay(toplevel),
			 DefaultColormap(dpy, rscr->screen),
			 "#A8A8A8A8A8A8",
			 &backgroundColor)) {
	    backgroundColor.pixel = WhitePixel(dpy, rscr->screen);
	}
	else {
	    XAllocColor(XtDisplay(toplevel),
			DefaultColormap(dpy, rscr->screen),
			&backgroundColor);
	}
    }
    else {
	XAllocColor(XtDisplay(toplevel),
		    DefaultColormap(dpy, rscr->screen),
		    &backgroundColor);
    }

    val->addr = (XtPointer)&backgroundColor.pixel;
}

void
_WmMatteBSCDefault(Widget widget, int offset, XrmValue * val)
{
    XColor _widgetBackground;
    static XColor bottomShadowColor;
    void *w = working_base;

    _widgetBackground.pixel = ((MwmWindow *)w)->matte_background;

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
                &_widgetBackground);

    bottomShadowColor.blue = _widgetBackground.blue * .5;
    bottomShadowColor.green = _widgetBackground.green * .5;
    bottomShadowColor.red = _widgetBackground.red * .5;

    if (!XAllocColor(XtDisplay(toplevel),
                     DefaultColormap(dpy, rscr->screen),
                     &bottomShadowColor))
        bottomShadowColor.pixel = WhitePixel(dpy, rscr->screen);

    val->addr = (XtPointer)&bottomShadowColor.pixel;
}

void
_WmMatteBSPDefault(Widget widget, int offset, XrmValue * val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XtPointer)&pix;
}

void
_WmMatteFDefault(Widget widget, int offset, XrmValue * val)
{
    double h,s,v;
    XColor _widgetBackground;
    static XColor foregroundColor;
    void *w = working_base;

    _widgetBackground.pixel = ((MwmWindow *)w)->matte_background;

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
                &_widgetBackground);

    __XmRGB2HSV(_widgetBackground.red,
                _widgetBackground.green,
                _widgetBackground.blue,
                &h,&s,&v);

    if (v<0.5)
        foregroundColor.pixel = WhitePixel(dpy, rscr->screen);
    else
        foregroundColor.pixel = BlackPixel(dpy, rscr->screen);

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
                &foregroundColor);

    val->addr = (XtPointer)&foregroundColor.pixel;
}

void
_WmMatteTSCDefault(Widget widget, int offset, XrmValue * val)
{
    XColor _widgetBackground;
    static XColor topShadowColor;
    void *w = working_base;

    _widgetBackground.pixel = ((MwmWindow *)w)->matte_background;

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
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


    if (!XAllocColor(XtDisplay(toplevel),
                     DefaultColormap(dpy, rscr->screen),
                     &topShadowColor))
        topShadowColor.pixel = WhitePixel(dpy, rscr->screen);

    val->addr = (XtPointer)&topShadowColor.pixel;
}

void
_WmMatteTSPDefault(Widget widget, int offset, XrmValue * val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XtPointer)&pix;
}

void
_WmBackgroundDefault(Widget widget, int offset, XrmValue * val)
{
    static XColor backgroundColor;
    ComponentInfo *comp = (ComponentInfo *)working_base;

    if (comp->type == MWM_MENU) {
	if (!XParseColor(XtDisplay(toplevel),
			 DefaultColormap(dpy, rscr->screen),
			 "#72729f9fffff",
			 &backgroundColor)) {
	    backgroundColor.pixel = WhitePixel(dpy, rscr->screen);
	}
	else {
	    XAllocColor(XtDisplay(toplevel),
			DefaultColormap(dpy, rscr->screen),
			&backgroundColor);
	}

	val->addr = (XtPointer)&backgroundColor.pixel;
    }
    else if (comp->type == MWM_FEEDBACK) {
	if (!XParseColor(XtDisplay(toplevel),
                         DefaultColormap(dpy, rscr->screen),
		         "Cadet Blue",
                         &backgroundColor)) {
	    if (!XParseColor(XtDisplay(toplevel),
			     DefaultColormap(dpy, rscr->screen),
			     "#5F5F92929E9E",
			     &backgroundColor)) {
		backgroundColor.pixel = WhitePixel(dpy, rscr->screen);
	    }
	    else {
		XAllocColor(XtDisplay(toplevel),
			    DefaultColormap(dpy, rscr->screen),
			    &backgroundColor);
	    }
	}
	else {
	    XAllocColor(XtDisplay(toplevel),
		        DefaultColormap(dpy, rscr->screen),
		        &backgroundColor);
	}

	val->addr = (XtPointer)&backgroundColor.pixel;
    }
    else {
	if (!XParseColor(XtDisplay(toplevel),
                         DefaultColormap(dpy, rscr->screen),
		         "LightGrey",
                         &backgroundColor)) {
	    if (!XParseColor(XtDisplay(toplevel),
			     DefaultColormap(dpy, rscr->screen),
			     "#A8A8A8A8A8A8",
			     &backgroundColor)) {
		backgroundColor.pixel = WhitePixel(dpy, rscr->screen);
	    }
	    else {
		XAllocColor(XtDisplay(toplevel),
			    DefaultColormap(dpy, rscr->screen),
			    &backgroundColor);
	    }
	}
	else {
	    XAllocColor(XtDisplay(toplevel),
		        DefaultColormap(dpy, rscr->screen),
		        &backgroundColor);
	}
	val->addr = (XtPointer)&backgroundColor.pixel;
    }
}

void
_WmBackgroundPixmapDefault(Widget widget, int offset, XrmValue * val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XtPointer)&pix;
}

void
_WmBottomShadowColorDefault(Widget widget, int offset, XrmValue * val)
{
    XColor _widgetBackground;
    static XColor bottomShadowColor;
    void *w = working_base;

    _widgetBackground.pixel = ((ComponentInfo *)w)->background;

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
                &_widgetBackground);

    bottomShadowColor.blue = _widgetBackground.blue * .5;
    bottomShadowColor.green = _widgetBackground.green * .5;
    bottomShadowColor.red = _widgetBackground.red * .5;
    if (!XAllocColor(XtDisplay(toplevel),
                     DefaultColormap(dpy, rscr->screen),
                     &bottomShadowColor))
        bottomShadowColor.pixel = WhitePixel(dpy, rscr->screen);

    val->addr = (XtPointer)&bottomShadowColor.pixel;
}

void
_WmBottomShadowPixmapDefault(Widget widget, int offset, XrmValue * val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XtPointer)&pix;
}

void
_WmForegroundDefault(Widget widget, int offset, XrmValue * val)
{
    double h,s,v;
    XColor _widgetBackground;
    static XColor foregroundColor;
    void *w = working_base;

    _widgetBackground.pixel = ((ComponentInfo *)w)->background;

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
                &_widgetBackground);

    __XmRGB2HSV(_widgetBackground.red,
                _widgetBackground.green,
                _widgetBackground.blue,
                &h,&s,&v);

    if (v<0.5)
        foregroundColor.pixel = WhitePixel(dpy, rscr->screen);
    else
        foregroundColor.pixel = BlackPixel(dpy, rscr->screen);

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
                &foregroundColor);

    val->addr = (XtPointer)&foregroundColor.pixel;
}

void
_WmTopShadowColorDefault(Widget widget, int offset, XrmValue * val)
{
    XColor _widgetBackground;
    static XColor topShadowColor;
    void *w = working_base;

    _widgetBackground.pixel = ((ComponentInfo *)w)->background;

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
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


    if (!XAllocColor(XtDisplay(toplevel),
                     DefaultColormap(dpy, rscr->screen),
                     &topShadowColor))
        topShadowColor.pixel = WhitePixel(dpy, rscr->screen);

    val->addr = (XtPointer)&topShadowColor.pixel;
}

void
_WmTopShadowPixmapDefault(Widget widget, int offset, XrmValue * val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XtPointer)&pix;
}

void
_WmABackgroundDefault(Widget widget, int offset, XrmValue * val)
{
    static XColor backgroundColor;

    if (!XParseColor(XtDisplay(toplevel),
                     DefaultColormap(dpy, rscr->screen),
		     "Cadet Blue",
                     &backgroundColor)) {
	if (!XParseColor(XtDisplay(toplevel),
			 DefaultColormap(dpy, rscr->screen),
			 "#5F5F92929E9E",
			 &backgroundColor)) {
	    backgroundColor.pixel = WhitePixel(dpy, rscr->screen);
	}
	else {
	    XAllocColor(XtDisplay(toplevel),
			DefaultColormap(dpy, rscr->screen),
			&backgroundColor);
	}
    }
    else {
	XAllocColor(XtDisplay(toplevel),
		    DefaultColormap(dpy, rscr->screen),
		    &backgroundColor);
    }

    val->addr = (XtPointer)&backgroundColor.pixel;
}

void
_WmAForegroundDefault(Widget widget, int offset, XrmValue * val)
{
    double h,s,v;
    XColor _widgetBackground;
    static XColor foregroundColor;
    void *w = working_base;

    _widgetBackground.pixel = ((ComponentInfo *)w)->active_background;

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
                &_widgetBackground);

    __XmRGB2HSV(_widgetBackground.red,
                _widgetBackground.green,
                _widgetBackground.blue,
                &h,&s,&v);

    if (v<0.5)
        foregroundColor.pixel = WhitePixel(dpy, rscr->screen);
    else
        foregroundColor.pixel = BlackPixel(dpy, rscr->screen);

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
                &foregroundColor);

    val->addr = (XtPointer)&foregroundColor.pixel;
}

void
_WmABottomShadowColorDefault(Widget widget, int offset, XrmValue * val)
{
    XColor _widgetBackground;
    static XColor bottomShadowColor;
    void *w = working_base;

    _widgetBackground.pixel = ((ComponentInfo *)w)->active_background;

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
                &_widgetBackground);

    bottomShadowColor.blue = _widgetBackground.blue * .5;
    bottomShadowColor.green = _widgetBackground.green * .5;
    bottomShadowColor.red = _widgetBackground.red * .5;

    if (!XAllocColor(XtDisplay(toplevel),
                     DefaultColormap(dpy, rscr->screen),
                     &bottomShadowColor))
        bottomShadowColor.pixel = WhitePixel(dpy, rscr->screen);

    val->addr = (XtPointer)&bottomShadowColor.pixel;
}

void
_WmATopShadowColorDefault(Widget widget, int offset, XrmValue * val)
{
    XColor _widgetBackground;
    static XColor topShadowColor;
    void *w = working_base;

    _widgetBackground.pixel = ((ComponentInfo *)w)->active_background;

    XQueryColor(XtDisplay(toplevel),
                DefaultColormap(dpy, rscr->screen),
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


    if (!XAllocColor(XtDisplay(toplevel),
                     DefaultColormap(dpy, rscr->screen),
                     &topShadowColor))
        topShadowColor.pixel = WhitePixel(dpy, rscr->screen);

    val->addr = (XtPointer)&topShadowColor.pixel;
}

void
_WmABackgroundPixmapDefault(Widget widget, int offset, XrmValue * val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XtPointer)&pix;
}

void
_WmATopShadowPixmapDefault(Widget widget, int offset, XrmValue * val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XtPointer)&pix;
}
