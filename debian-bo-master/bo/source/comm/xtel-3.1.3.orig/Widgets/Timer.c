/*	
 *   Copyright (C) 1995  Pierre Ficheux (pierre@rd.lectra.fr)
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
static char rcsid[] = "$Id: Timer.c,v 1.6 1997/02/21 17:03:36 pierre Exp $";

/*
 * Timer, a digital clock/timer widget
 */

#include <stdio.h>
#include <time.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Misc.h>
#include <X11/Xaw/XawInit.h>

#include "TimerP.h"

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */
static char defaultTranslations[] = "";

#define offset(field) XtOffsetOf(TimerRec, field)
static XtResource resources[] = { 
    {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	offset(timer.font),XtRString, XtDefaultFont},
    {XtNclockForeground, XtCClockForeground, XtRPixel, sizeof(Pixel),
	offset(timer.clock_foreground), XtRString, XtDefaultForeground},
    {XtNtimerForeground, XtCTimerForeground, XtRPixel, sizeof(Pixel),
	offset(timer.timer_foreground), XtRString, XtDefaultForeground},
    {XtNmargin, XtCBorderWidth, XtRDimension, sizeof(Dimension),
	offset(timer.margin), XtRImmediate, (XtPointer)2},
    {XtNtimerMode, XtCTimerMode, XtRBoolean, sizeof(Boolean),
	offset(timer.timer_mode), XtRImmediate, (XtPointer)False},
    {XtNcountDown, XtCCountDown, XtRBoolean, sizeof(Boolean),
	offset(timer.count_down), XtRImmediate, (XtPointer)False},
    {XtNmin, XtCMin, XtRInt, sizeof (int),
	offset(timer.min), XtRImmediate, (XtPointer) 0},
    {XtNsec, XtCSec, XtRInt, sizeof (int),
	offset(timer.sec), XtRImmediate, (XtPointer) 0},
};
#undef offset

static void SetTimer(), DrawValue();
static XtTimerCallbackProc RefreshTimer();

static Boolean SetValues();
static void Initialize(), Redisplay();
static void Destroy();
static void ClassInitialize();
static void Realize(), Resize();

TimerClassRec timerClassRec = {
  {
#define superclass			(&widgetClassRec)
    (WidgetClass)superclass,		/* superclass		  */	
    "Timer",				/* class_name		  */
    sizeof(TimerRec),			/* size			  */
    ClassInitialize,			/* class_initialize	  */
    NULL,				/* class_part_initialize  */
    FALSE,				/* class_inited		  */
    Initialize,				/* initialize		  */
    NULL,				/* initialize_hook	  */
    Realize,				/* realize		  */
    NULL,				/* actions		  */
    0,					/* num_actions		  */
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
};

  /* for public consumption */
WidgetClass timerWidgetClass = (WidgetClass) &timerClassRec;

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
  TimerWidget tw = (TimerWidget) new;
  XGCValues	values;

  values.foreground	= tw->timer.clock_foreground;
  values.background	= tw->core.background_pixel;
  values.font		= tw->timer.font->fid;
  tw->timer.clock_GC = XtGetGC((Widget)tw, (unsigned) GCForeground | GCBackground | GCFont, &values);
  values.foreground	= tw->timer.timer_foreground;
  tw->timer.timer_GC = XtGetGC((Widget)tw, (unsigned) GCForeground | GCBackground | GCFont, &values);

  tw->timer.state = TimerOff;
  tw->timer.min_init = tw->timer.min;
  tw->timer.sec_init = tw->timer.sec;
  SetTimer (tw, True);

  tw->core.width = 2 * tw->timer.margin + 5 * tw->timer.font->max_bounds.width;
  tw->core.height = 2 * tw->timer.margin + tw->timer.font->max_bounds.ascent + tw->timer.font->max_bounds.descent;
}

/***************************
*
*  Action Procedures
*
***************************/

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
    TimerWidget tw = (TimerWidget) w;

    /* If clock mode, start now */
    if (!tw->timer.timer_mode && tw->timer.state != TimerOn) {
	tw->timer.id = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)tw), (unsigned long) 1000, (XtTimerCallbackProc) RefreshTimer, (XtPointer)tw);
	tw->timer.state = TimerOn;
    }

    DrawValue (tw);
}


static void 
Destroy(w)
Widget w;
{
  TimerWidget tw = (TimerWidget) w;
  XtReleaseGC (w, tw->timer.timer_GC);
  XtReleaseGC (w, tw->timer.clock_GC);
}

/*
 * Set specified arguments into widget
 */

/* ARGSUSED */
static Boolean 
SetValues (current, request, new)
Widget current, request, new;
{
    TimerWidget curtw = (TimerWidget) current;
    TimerWidget newtw = (TimerWidget) new;
    Boolean redisplay = False;

    if (curtw->timer.min != newtw->timer.min)
      newtw->timer.min_init = newtw->timer.min;

    if (curtw->timer.sec != newtw->timer.sec)
      newtw->timer.sec_init = newtw->timer.sec;

    /* From timer to clock, force re-init */
    if (curtw->timer.timer_mode && !newtw->timer.timer_mode)
      SetTimer (newtw, True);

    if (curtw->timer.font != newtw->timer.font || 
	curtw->timer.clock_foreground != newtw->timer.clock_foreground || 
	curtw->timer.timer_foreground != newtw->timer.timer_foreground || 
	curtw->timer.margin != newtw->timer.margin ||
	curtw->timer.min != newtw->timer.min ||
	curtw->timer.sec != newtw->timer.sec ||
	curtw->timer.count_down != newtw->timer.count_down ||
	curtw->timer.timer_mode != newtw->timer.timer_mode) {

	if (newtw->timer.timer_mode == False)
	    timerResetTimer (curtw);
	
	redisplay = True;
    }

    return (redisplay);
}

static void ClassInitialize()
{
}

static void Realize(w, valueMask, attributes)
    Widget w;
    Mask *valueMask;
    XSetWindowAttributes *attributes;
{
    XtCreateWindow(w, (unsigned) InputOutput, (Visual *) CopyFromParent,
		   *valueMask, attributes);

}

static void Resize(w)
    Widget w;
{
}

static void DrawValue(tw)
TimerWidget tw;
{
    XDrawImageString(XtDisplay((Widget)tw), XtWindow((Widget)tw), (tw->timer.timer_mode ? tw->timer.timer_GC : tw->timer.clock_GC), tw->timer.margin, tw->timer.margin + tw->timer.font->max_bounds.ascent, tw->timer.value, strlen(tw->timer.value));
}

static void SetTimer(tw, init)
TimerWidget tw;
Boolean init;
{
    time_t t = time(0);
    struct tm *tm;

    /* Timer mode */
    if (tw->timer.timer_mode) {
	sprintf (tw->timer.value, "%02d:%02d", tw->timer.min, tw->timer.sec);
	if (tw->timer.count_down) {
	  if (tw->timer.sec == 0) {
	    if (tw->timer.min > 0) {
	      tw->timer.min--;
	      tw->timer.sec = 60;
	    }
	    else {
	      timerStopTimer ((Widget)tw);
	      tw->timer.sec++;
	    }
	  }

	  tw->timer.sec--;
	}
	else {
	  tw->timer.sec++;
	  if (tw->timer.sec == 60) {
	    tw->timer.sec = 0;
	    tw->timer.min++;
	  }
	}
    }
    /* Clock mode */
    else {
	tm = localtime (&t);
	if (tm->tm_sec == 0 || init)
	    sprintf (tw->timer.value, "%02d:%02d", tm->tm_hour, tm->tm_min);
    }
}


static XtTimerCallbackProc 
RefreshTimer(tw, id)
TimerWidget tw;
XtIntervalId *id;
{
    if (tw->timer.state == TimerOn) {
	tw->timer.id = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)tw), (unsigned long) 1000, (XtTimerCallbackProc) RefreshTimer, (XtPointer)tw);
	SetTimer (tw, False);	
	DrawValue (tw);
    }
}

/*
 * Public functions
 */

void timerStartTimer (w)
Widget w;
{
    TimerWidget tw = (TimerWidget)w;

    if (tw->timer.timer_mode && tw->timer.state == TimerOff) {
	tw->timer.id = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)tw), (unsigned long) 1000, (XtTimerCallbackProc) RefreshTimer, (XtPointer)tw);
	tw->timer.state = TimerOn;
    }
}

void timerStopTimer (w)
Widget w;
{
    TimerWidget tw = (TimerWidget)w;

    if (tw->timer.timer_mode && tw->timer.state == TimerOn) {
	tw->timer.state = TimerOff;
	XtRemoveTimeOut (tw->timer.id);
    }
}

void timerResetTimer (w)
Widget w;
{
    TimerWidget tw = (TimerWidget)w;

    if (tw->timer.timer_mode) {
	if (tw->timer.state == TimerOn)
	   timerStopTimer (w);   

	tw->timer.min = tw->timer.min_init;
	tw->timer.sec = tw->timer.sec_init;
	SetTimer (tw, False);
	DrawValue (tw);
    }
}
