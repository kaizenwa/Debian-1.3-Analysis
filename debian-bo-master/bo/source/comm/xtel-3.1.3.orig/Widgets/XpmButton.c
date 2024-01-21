/*	
 *   Copyright (C) 1995  Pierre Ficheux	(pierre@rd.lectra.fr)
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
static char rcsid[] = "$Id: XpmButton.c,v 1.4 1997/02/21 17:12:20 pierre Exp $";

/*
 * XpmButton, a simple push/toggle button widget with a nice 
 * XPM Pixmap inside...
 */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Misc.h>
#ifndef USE_MOTIF
#include <X11/Xaw/XawInit.h>
#endif
#include <X11/Xmu/Converters.h>

#include "XpmButtonP.h"

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */
static char defaultTranslations[] =
     "<Btn1Down>:	set() \n\
      <Btn1Up>:		notify() unset()	\n\
      <EnterWindow>:	enter() \n\
      <LeaveWindow>:	leave()";
    

#define offset(field) XtOffsetOf(XpmButtonRec, field)
static XtResource resources[] = { 
   {XtNcallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
      offset(xpmbutton.callback), XtRCallback, (XtPointer)NULL},
   {XtNpixmap1, XtCPixmap, XtRPixmap, sizeof(Pixmap),
	offset(xpmbutton.pixmap1), XtRImmediate, (XtPointer)None},
   {XtNpixmap2, XtCPixmap, XtRPixmap, sizeof(Pixmap),
	offset(xpmbutton.pixmap2), XtRImmediate, (XtPointer)None},
   {XtNshape1, XtCPixshape, XtRPixshape, sizeof(Pixshape),
	offset(xpmbutton.shape1), XtRImmediate, (XtPointer)None},
   {XtNshape2, XtCPixshape, XtRPixshape, sizeof(Pixshape),
	offset(xpmbutton.shape2), XtRImmediate, (XtPointer)None},
   {XtNunsensitiveBitmap, XtCPixmap, XtRPixmap, sizeof(Pixmap),
	offset(xpmbutton.unsensitive_bitmap), XtRImmediate, (XtPointer)None},
   {XtNhelpFont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	offset(xpmbutton.help_font),XtRString, XtDefaultFont},
   {XtNhelpString, XtCHelpString, XtRString, sizeof(String),
	offset(xpmbutton.help_string), XtRString, NULL},
   {XtNhelpBackground, XtCHelpBackground, XtRPixel, sizeof(Pixel),
	offset(xpmbutton.help_background), XtRString, XtDefaultBackground},
   {XtNhelpForeground, XtCHelpForeground, XtRPixel, sizeof(Pixel),
	offset(xpmbutton.help_foreground), XtRString, XtDefaultForeground},
   {XtNshadowColor1, XtCShadowColor, XtRPixel, sizeof(Pixel),
	offset(xpmbutton.shadow_color1), XtRString, XtDefaultForeground},
   {XtNshadowColor2, XtCShadowColor, XtRPixel, sizeof(Pixel),
	offset(xpmbutton.shadow_color2), XtRString, XtDefaultForeground},
   {XtNshadowThickness, XtCShadowThickness, XtRDimension, sizeof(Dimension),
	offset(xpmbutton.shadow_thickness), XtRImmediate, (XtPointer)0},
};
#undef offset

static Boolean SetValues();
static void Initialize(), Redisplay(), Set(), Unset();
static void Enter(), Leave(), Notify();
static void Destroy();
static void ClassInitialize();
static void Realize(), Resize();
static void MyCvtStringToPixmap();
static void MyCvtStringToShapemask();

static XtActionsRec actionsList[] = {
  {"set",		Set},
  {"unset",		Unset},
  {"notify",		Notify},
  {"enter",		Enter},
  {"leave",		Leave},
};

static XtConvertArgRec myScreenConvertArg[] = {
    {XtBaseOffset, (XtPointer) XtOffset(Widget, core.screen), sizeof(Screen *)}
};	


XpmButtonClassRec xpmButtonClassRec = {
  {
/* core_class fields */	
#ifdef USE_MOTIF
#define superclass			(&xmPrimitiveClassRec)
#else
#define superclass			(&simpleClassRec)
#endif
    (WidgetClass)superclass,		/* superclass		  */	
    "XpmButton",			/* class_name		  */
    sizeof(XpmButtonRec),		/* size			  */
    ClassInitialize,			/* class_initialize	  */
    NULL,				/* class_part_initialize  */
    FALSE,				/* class_inited		  */
    Initialize,				/* initialize		  */
    NULL,				/* initialize_hook	  */
    Realize,				/* realize		  */
    actionsList,			/* actions		  */
    XtNumber(actionsList),		/* num_actions		  */
    resources,				/* resources		  */
    XtNumber(resources),		/* resource_count	  */
    NULLQUARK,				/* xrm_class		  */
    FALSE,				/* compress_motion	  */
    TRUE,				/* compress_exposure	  */
    TRUE,				/* compress_enterleave    */
    FALSE,				/* visible_interest	  */
    Destroy,				/* destroy		  */
    Resize,				/* resize		  */
    Redisplay,				/* expose		  */
    SetValues,				/* set_values		  */
    NULL,				/* set_values_hook	  */
    XtInheritSetValuesAlmost,		/* set_values_almost	  */
    NULL,				/* get_values_hook	  */
    NULL,				/* accept_focus		  */
    XtVersion,				/* version		  */
    NULL,				/* callback_private	  */
    defaultTranslations,		/* tm_table		  */
    XtInheritQueryGeometry,		/* query_geometry	  */
    XtInheritDisplayAccelerator,	/* display_accelerator	  */
    NULL				/* extension		  */
  },  /* CoreClass fields initialization */
#ifdef USE_MOTIF
  { /* primitive_class record       */
    /* Primitive border_highlight   */	NULL,
    /* Primitive border_unhighlight */	NULL,
    /* translations		    */  XtInheritTranslations,
    /* arm_and_activate		    */  NULL,
    /* get resources		    */  NULL,
    /* num get_resources	    */  0,
    /* extension		    */  NULL,
  },
#else
  {
    XtInheritChangeSensitive		/* change_sensitive	  */	
  },  /* XpmButtonClass fields initialization */
#endif /* USE_MOTIF */
};

  /* for public consumption */
WidgetClass xpmButtonWidgetClass = (WidgetClass) &xpmButtonClassRec;

/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

/* ARGSUSED */
static void 
Initialize(request, new, args, num_args)
Widget request, new;
ArgList args;			/* unused */
Cardinal *num_args;		/* unused */
{
  XpmButtonWidget xbw = (XpmButtonWidget) new;
  int rab, width, height;
  Window root;
  XGCValues	values;

  /* If no Pixmap defined, use the default ;-) */
  if (xbw->xpmbutton.pixmap1 == None) {
      fprintf (stderr, "xpmButtonWidget: Warning, no Pixmap defined, using default.\n");
      XpmCreatePixmapFromData (XtDisplay(new), RootWindowOfScreen(XtScreen(new)), default_xpm, &xbw->xpmbutton.pixmap1, NULL, NULL);
      XpmCreatePixmapFromData (XtDisplay(new), RootWindowOfScreen(XtScreen(new)), default_on_xpm, &xbw->xpmbutton.pixmap2, NULL, NULL);

  }

  if (xbw->xpmbutton.pixmap2 == None)
      xbw->xpmbutton.pixmap2 = xbw->xpmbutton.pixmap1;

  if (xbw->xpmbutton.shape2 == None)
      xbw->xpmbutton.shape2 = xbw->xpmbutton.shape1;

  xbw->xpmbutton.set = FALSE;
  xbw->xpmbutton.help_armed = FALSE;

  XGetGeometry (XtDisplay(xbw), xbw->xpmbutton.pixmap1, &root, &rab, &rab, (unsigned int *)&width, (unsigned int *)&height, (unsigned int *)&rab, (unsigned int *)&rab);

  xbw->core.width = width + 2 * xbw->xpmbutton.shadow_thickness;
  xbw->core.height = height + 2 * xbw->xpmbutton.shadow_thickness;

  xbw->xpmbutton.normal_GC = XCreateGC (XtDisplay(new), RootWindowOfScreen(XtScreen(new)), 0, NULL);
  values.foreground = xbw->xpmbutton.shadow_color1;
  xbw->xpmbutton.color1_GC = XtGetGC(new, (unsigned) GCForeground, &values);
  values.foreground = xbw->xpmbutton.shadow_color2;
  xbw->xpmbutton.color2_GC = XtGetGC(new, (unsigned) GCForeground, &values);

  /* Create the Help window ? */
  if (xbw->xpmbutton.help_string) {
      int ws, hs;
      XSetWindowAttributes attr;
      XGCValues	values;

      attr.override_redirect = True;
      attr.background_pixel = xbw->xpmbutton.help_background;

      ws = XTextWidth (xbw->xpmbutton.help_font, xbw->xpmbutton.help_string, strlen (xbw->xpmbutton.help_string))+4;
      hs = xbw->xpmbutton.help_font->ascent+xbw->xpmbutton.help_font->descent+4;
      xbw->xpmbutton.help_window = XCreateWindow (XtDisplay(new), RootWindowOfScreen(XtScreen(new)), 0, 0, ws, hs, 1, DisplayPlanes(XtDisplay(new), DefaultScreen(XtDisplay(new))), CopyFromParent, CopyFromParent, CWOverrideRedirect|CWBackPixel, &attr);

      values.foreground	= xbw->xpmbutton.help_foreground;
      values.background	= xbw->xpmbutton.help_background;
      values.font = xbw->xpmbutton.help_font->fid;
      xbw->xpmbutton.help_GC = XtGetGC((Widget)xbw, (unsigned) GCForeground | GCBackground | GCFont, &values);
  }
}

/***************************
*
*  Action Procedures
*
***************************/

static void DrawPixmap (w, pixmap, shape)
Widget w;
Pixmap pixmap, shape;
{
    XpmButtonWidget xbw = (XpmButtonWidget)w;
    GC gc = xbw->xpmbutton.normal_GC;
    register int st = xbw->xpmbutton.shadow_thickness;
    register int ww = w->core.width-2*st;
    register int hh = w->core.height-2*st;

    if (XtIsRealized(w) && pixmap != None) {

	XClearWindow(XtDisplay(w), XtWindow(w));
	
	if (!XtIsSensitive (w)) {
	    if (xbw->xpmbutton.unsensitive_bitmap != None)
		XSetClipMask(XtDisplay(w), gc, xbw->xpmbutton.unsensitive_bitmap);
	}
	else {
	    if (shape != None) {
		XSetClipMask(XtDisplay(w), gc, shape);
	    } else {
		XSetClipMask(XtDisplay(w), gc, (Pixmap)None);
	    }
	}
	
	/* Draws the pixmap */
	XCopyArea(XtDisplay(w), pixmap, XtWindow(w), gc, 0, 0, ww, hh, st, st);

	/* Draws the shadows */
	if (st) {
	    XPoint poly1[6];
	    XPoint poly2[6];

	    poly1[0].x = 0; poly1[0].y = 0;
	    poly1[1].x = ww+2*st; poly1[1].y = 0;
	    poly1[2].x = ww+st; poly1[2].y = st;
	    poly1[3].x = st; poly1[3].y = st;
	    poly1[4].x = st; poly1[4].y = hh+st;
	    poly1[5].x = 0; poly1[5].y = hh+2*st;

	    poly2[0].x = 0; poly2[0].y = hh+2*st;
	    poly2[1].x = ww+2*st; poly2[1].y = hh+2*st;
	    poly2[2].x = ww+2*st; poly2[2].y = 0;
	    poly2[3].x = ww+st; poly2[3].y = st;
	    poly2[4].x = ww+st; poly2[4].y = hh+st;
	    poly2[5].x = st; poly2[5].y = hh+st;

	    if (xbw->xpmbutton.set) {
		XFillPolygon (XtDisplay(w), XtWindow(w), xbw->xpmbutton.color2_GC, poly1, 6, Convex, CoordModeOrigin);
		XFillPolygon (XtDisplay(w), XtWindow(w), xbw->xpmbutton.color1_GC, poly2, 6, Convex, CoordModeOrigin);
	    }
	    else {
		XFillPolygon (XtDisplay(w), XtWindow(w), xbw->xpmbutton.color1_GC, poly1, 6, Convex, CoordModeOrigin);
		XFillPolygon (XtDisplay(w), XtWindow(w), xbw->xpmbutton.color2_GC, poly2, 6, Convex, CoordModeOrigin);
	    }
	}
    }
}

static XtTimerCallbackProc hide_help (w, id)
Widget w;
XtIntervalId *id;
{
    XpmButtonWidget xbw = (XpmButtonWidget)w;

    if (xbw->xpmbutton.help_string)
	XUnmapWindow (XtDisplay(w), xbw->xpmbutton.help_window);
}

static XtTimerCallbackProc show_help (w, id)
Widget w;
XtIntervalId *id;
{
    XpmButtonWidget xbw = (XpmButtonWidget)w;
    Window dummy_w;
    int dummy, x_root, y_root;

    XQueryPointer (XtDisplay(w), XtWindow(w), &dummy_w, &dummy_w, &x_root, &y_root, &dummy, &dummy, (unsigned int *)&dummy);
    XMoveWindow (XtDisplay(w), xbw->xpmbutton.help_window, x_root, y_root+10);
    XRaiseWindow (XtDisplay(w), xbw->xpmbutton.help_window);
    XMapWindow (XtDisplay(w), xbw->xpmbutton.help_window);
    XDrawImageString (XtDisplay(w), xbw->xpmbutton.help_window, xbw->xpmbutton.help_GC, 2, xbw->xpmbutton.help_font->max_bounds.ascent+2, xbw->xpmbutton.help_string, strlen(xbw->xpmbutton.help_string));
}

static void arm_help (xbw)
XpmButtonWidget xbw;
{
    if (xbw->xpmbutton.help_string && !xbw->xpmbutton.help_armed) {
	xbw->xpmbutton.id = XtAppAddTimeOut (XtWidgetToApplicationContext((Widget)xbw), (unsigned long)1000, (XtTimerCallbackProc)show_help, (XtPointer)xbw);
	xbw->xpmbutton.help_armed = TRUE;
    }
}

static void disarm_help (xbw)
XpmButtonWidget xbw;
{
    if (xbw->xpmbutton.help_string && xbw->xpmbutton.help_armed) {
	XtRemoveTimeOut (xbw->xpmbutton.id);
	XUnmapWindow (XtDisplay(xbw), xbw->xpmbutton.help_window);
	xbw->xpmbutton.help_armed = FALSE;
    }
}


/* ARGSUSED */
static void 
Enter(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;		/* unused */
Cardinal *num_params;	/* unused */
{
      arm_help ((XpmButtonWidget)w);
}

/* ARGSUSED */
static void 
Leave(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;		/* unused */
Cardinal *num_params;	/* unused */
{
      disarm_help ((XpmButtonWidget)w);
      Unset (w, event, params, num_params);
}

/* ARGSUSED */
static void 
Set(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;		/* unused */
Cardinal *num_params;	/* unused */
{
  XpmButtonWidget xbw = (XpmButtonWidget)w;

  if (xbw->xpmbutton.set)
      return;

  disarm_help (xbw);
  
  xbw->xpmbutton.set = TRUE;
  if (XtIsRealized(w))
      DrawPixmap (w, xbw->xpmbutton.pixmap2,  xbw->xpmbutton.shape2);
}

/* ARGSUSED */
static void
Unset(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;		/* unused */
Cardinal *num_params;
{
  XpmButtonWidget xbw = (XpmButtonWidget)w;

  if (!xbw->xpmbutton.set)
      return;

  xbw->xpmbutton.set = FALSE;
  if (XtIsRealized(w))
      DrawPixmap (w, xbw->xpmbutton.pixmap1, xbw->xpmbutton.shape1);
}

/* ARGSUSED */
static void 
Notify(w,event,params,num_params)
Widget w;
XEvent *event;
String *params;		/* unused */
Cardinal *num_params;	/* unused */
{
    XpmButtonWidget xbw = (XpmButtonWidget)w;

    if (xbw->xpmbutton.set)
	XtCallCallbackList(w, xbw->xpmbutton.callback, (XtPointer) NULL);
}

/*
 * Repaint the widget window
 */

/************************
*
*  REDISPLAY (DRAW)
*
************************/

/* ARGSUSED */
static void 
Redisplay(w, event, region)
Widget w;
XEvent *event;
Region region;
{
    XpmButtonWidget xbw = (XpmButtonWidget)w;

    if (xbw->xpmbutton.set)
	DrawPixmap (w, xbw->xpmbutton.pixmap2,  xbw->xpmbutton.shape2);
    else
	DrawPixmap (w, xbw->xpmbutton.pixmap1,  xbw->xpmbutton.shape1);
}


static void 
Destroy(w)
Widget w;
{
  XpmButtonWidget xbw = (XpmButtonWidget) w;
  XFreeGC (XtDisplay(w), xbw->xpmbutton.normal_GC);
  XtReleaseGC (w, xbw->xpmbutton.color1_GC);
  XtReleaseGC (w, xbw->xpmbutton.color2_GC);
  XtReleaseGC (w, xbw->xpmbutton.help_GC);
  XDestroyWindow (XtDisplay(w), xbw->xpmbutton.help_window);
}

/*
 * Set specified arguments into widget
 */

/* ARGSUSED */
static Boolean 
SetValues (current, request, new)
Widget current, request, new;
{
    XpmButtonWidget curxbw = (XpmButtonWidget) current;
    XpmButtonWidget newxbw = (XpmButtonWidget) new;
    Boolean redisplay = False;
    Window root;	
    int rab, width, height;

    if (curxbw->xpmbutton.pixmap1 != newxbw->xpmbutton.pixmap1) {
	XGetGeometry (XtDisplay(newxbw), newxbw->xpmbutton.pixmap1, &root, &rab, &rab, (unsigned int *)&width, (unsigned int *)&height, (unsigned int *)&rab, (unsigned int *)&rab);
	newxbw->core.width = width + 2 * newxbw->xpmbutton.shadow_thickness;
	newxbw->core.height = height + 2 * newxbw->xpmbutton.shadow_thickness;

	redisplay = True;
    }

    return (redisplay || (XtIsSensitive(current) != XtIsSensitive(new)));
}

static void ClassInitialize()
{
    XtAddConverter (XtRString, XtRPixmap, MyCvtStringToPixmap, myScreenConvertArg, XtNumber(myScreenConvertArg));
    XtAddConverter (XtRString, XtRPixshape, MyCvtStringToShapemask, myScreenConvertArg, XtNumber(myScreenConvertArg));
}


static void Realize(w, valueMask, attributes)
    Widget w;
    Mask *valueMask;
    XSetWindowAttributes *attributes;
{
    XtCreateWindow(w, (unsigned) InputOutput, (Visual *) CopyFromParent, *valueMask, attributes);
}

static void Resize(w)
    Widget w;
{}

/*
 * Converter function (XPM filename to Pixmap)
 */
#define	done(address, type) \
	{ (*toVal).size = sizeof(type); (*toVal).addr = (caddr_t) address; }


static void MyCvtStringToPixmap (args, num_args, fromVal, toVal)
XrmValuePtr args;
Cardinal    *num_args;
XrmValuePtr	fromVal;
XrmValuePtr	toVal;
{
    int res;
    static Pixmap pixm, shapem;
    char *name = (char *)fromVal->addr;
    Screen *screen;
    Display *dpy;
    
    unsigned int width, height;
    int xhot, yhot;

    if (*num_args != 1)
	XtErrorMsg("wrongParameters","MyCvtStringToPixmap","XtToolkitError",
		   "String to pixmap conversion needs screen argument",
		   (String *)NULL, (Cardinal *)NULL);

    if (strcmp(name, "None") == 0) {
	pixm = None;
	done(&pixm, Pixmap);
	return;
    }

    if (strcmp(name, "ParentRelative") == 0) {
	pixm = ParentRelative;
	done(&pixm, Pixmap);
	return;
    }

    screen = *((Screen **) args[0].addr);
    dpy = DisplayOfScreen(screen);


    if (strstr (name, ".xpm"))
	res = XpmReadFileToPixmap(DisplayOfScreen(screen), RootWindowOfScreen(screen), name, &pixm, &shapem, NULL);
    else
	res = XReadBitmapFile (DisplayOfScreen(screen), RootWindowOfScreen(screen), name, &width, &height, &pixm, &xhot, &yhot);

    if (res) {
	XtStringConversionWarning (name, "Pixmap");
	return;
    } else {
	done (&pixm, Pixmap);
    }
}


/*ARGSUSED*/
static void MyCvtStringToShapemask (args, num_args, fromVal, toVal)
XrmValuePtr args;
Cardinal    *num_args;
XrmValuePtr	fromVal;
XrmValuePtr	toVal;
{
    int res;
    static Pixmap pixm, shapem;
    char *name = (char *)fromVal->addr;
    Screen *screen;
    Display *dpy;

    if (*num_args != 1)
     XtErrorMsg("wrongParameters","MyCvtStringToShapemask","XtToolkitError",
             "String to Shapemask conversion needs screen argument",
              (String *)NULL, (Cardinal *)NULL);

    if (strcmp(name, "None") == 0) {
	shapem = None;
	done(&shapem, Pixmap);
	return;
    }

    if (strcmp(name, "ParentRelative") == 0) {
	shapem = ParentRelative;
	done(&shapem, Pixmap);
	return;
    }

    screen = *((Screen **) args[0].addr);
    dpy = DisplayOfScreen(screen);

    res = XpmReadFileToPixmap(DisplayOfScreen(screen),
			      RootWindowOfScreen(screen),
			      name, &pixm, &shapem, NULL );

    if (res) {
	XtStringConversionWarning (name, "Pixmap");
	return;
    } else {
	done (&shapem, Pixmap);
    }
}
