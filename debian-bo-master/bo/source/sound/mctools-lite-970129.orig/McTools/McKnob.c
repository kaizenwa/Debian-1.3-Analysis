/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; see the file COPYING.LIB.  If
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "McApp.h"
#include "McGadget.h"
#include "McFocus.h"
#include "McKnob.h"

#include <X11/keysym.h>

/****************************************************************************/

static void KnobUpdate(McGadget *gadget, int busy, int all);
static int KnobEvent(McGadget *gadget, XEvent *event);
static void KnobCleanup(McGadget *gadget);
static int KnobKey(McGadget *gadget, XKeyEvent *event);

/****************************************************************************/

McSpecialInfo *McCreateKnob(unsigned int flags, int max) {
  McKnob *knob = (McKnob *) calloc(sizeof(McKnob), 1);
  knob->flags = flags;
  knob->max=max;
  knob->value=knob->lastvalue=max>>1;
  
  knob->specialInfo.updateProc   = KnobUpdate;
  knob->specialInfo.eventProc    = KnobEvent;
  knob->specialInfo.keyboardProc = KnobKey;
  knob->specialInfo.cleanupProc  = KnobCleanup;

  return (McSpecialInfo *)knob;
}

/****************************************************************************/

static void KnobCleanup(McGadget *gadget) {
  free(gadget->specialInfo);
}

/****************************************************************************/

/*
 * Whats better? Wasting memory for a table of sines,
 * or wasting cpu cycles by doing it with floats?
 * Dunno, but coding was easier this way (:
 */
static void KnobUpdate(McGadget *gadget, int busy,int all) {
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  Display *display=app->display;
  McKnob *knob=KNOB;
  double foo;
  int x, y, w_2, h_2;
  int width=gadget->width-4;
  int height=gadget->height-4;

  if (knob->value!=knob->lastvalue) {
    h_2=height>>1; w_2=width>>1;
    foo=(knob->max-knob->lastvalue) * (3*M_PI_2) / knob->max - M_PI_4;
    y=sin(foo)*h_2; x=cos(foo)*w_2;
    XDrawLine(display, gadget->win, app->gc[GC_CLEAR],
	      w_2+1, h_2+1, w_2+x+1, h_2-y+1);
  }

  h_2=height>>1; w_2=width>>1;
  foo=(knob->max-knob->value) * (3*M_PI_2) / knob->max - M_PI_4;
  y=sin(foo)*h_2; x=cos(foo)*w_2;
  XDrawLine(display, gadget->win, app->gc[GC_NORMAL],
	    w_2+1, h_2+1, w_2+x+1, h_2-y+1);

  knob->lastvalue=knob->value;
  XDrawArc(display, gadget->win, app->gc[GC_DARK], 1, 1,
	   width, height,  225*64, 180*64);
  XDrawArc(display, gadget->win, app->gc[GC_NORMAL], 1, 1,
	   width, height,  270*64, 90*64);
  XDrawArc(display, gadget->win, app->gc[GC_BRIGHT], 1, 1,
	   width, height, 45*64,  180*64);

  McDrawFocusMark(gadget, COL_BACKGROUND);
}

/****************************************************************************/

static void CalcValue(McGadget *gadget, McKnob *knob, int x, int y, int set) {

  double d;

  x=x-(gadget->width>>1);
  y=(gadget->height>>1)-y;

  if (y>0) {
    d=acos(y/sqrt(x*x + y*y));
    if (x>0) d=M_PI_2+M_PI_4+d; else d=M_PI_2+M_PI_4-d;
  } else if (y<0) {
    d=acos(-y/sqrt(x*x + y*y));
    if (x>0) {
      d=M_PI+M_PI_2+M_PI_4-d;
      if (d>M_PI+M_PI_2) d=M_PI+M_PI_2;
    } else {
      d=d-M_PI_4;
      if (d<0) d=0;
    }
  } else {
    if (x>0) d=M_PI+M_PI_4; else d=M_PI_4;
  }

  if (set) {
    knob->value = knob->max * d / (M_PI+M_PI_2);
  } else {
    int now = knob->max * d / (M_PI+M_PI_2);
    if (now>knob->value)
      knob->value++;
    else
      if (now<knob->value)
	knob->value--;
  }
}

/****************************************************************************/

static int KnobEvent(McGadget *gadget, XEvent *event) {
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  McKnob *knob=KNOB;

  if (event->type==Expose) return 0;

  if (gadget->flags&GAD_PRESSED) {
    switch (event->type) {
    case ButtonRelease:
      gadget->flags&=~GAD_PRESSED;
      if (gadget->callbackUp) {
	(*gadget->callbackUp)(gadget);
      }
      return 1;

    case MotionNotify:
      CalcValue(gadget, knob, event->xmotion.x, event->xmotion.y, 1);
      KnobUpdate(gadget, 0, 0);
      if (gadget->callbackDown) {
	XFlush(mcw->app->display);
	(*gadget->callbackDown)(gadget);
      }
      return 1;
    case ButtonPress:
      return 1;
    default:
      return 0;
    }
  } else { /* not currently pressed */
    switch (event->type) {
    case ButtonPress:
      if (McInRectangle(event->xbutton.x, event->xbutton.y,
			0, 0, gadget->width, gadget->height)) {
	switch (event->xbutton.button) {
	case 1:
	  CalcValue(gadget, knob, event->xmotion.x, event->xmotion.y, 0);
	  KnobUpdate(gadget, 0, 0);
	  McSetFocus(gadget);
	  if (gadget->callbackUp) {
	    XFlush(mcw->app->display);
	    (*gadget->callbackUp)(gadget);
	  }
	  gadget->flags&=~GAD_PRESSED;
	  McMoveGadgetToStart(gadget);
	  return 1;
	case 2:
	  CalcValue(gadget, knob, event->xmotion.x, event->xmotion.y, 1);
	  KnobUpdate(gadget, 0, 0);
	  McSetFocus(gadget);
	  if (gadget->callbackDown) {
	    XFlush(app->display);
	    (*gadget->callbackDown)(gadget);
	  }
	  gadget->flags|=GAD_PRESSED;
	  McMoveGadgetToStart(gadget);
	  return 1;
	}
      } else {
	return 0;
      }
    default:
      return 0;
    }
  }
  return 0;
}

/****************************************************************************/

static int KnobKey(McGadget *gadget, XKeyEvent *event) {
  int up=0;
  McWindow *mcw=gadget->mcw;
  McKnob *knob=KNOB;

  switch((int)mcw->keysym) {
  case XK_Home:
    if (knob->value) {
      knob->value=0;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_End:
    if (knob->value!=knob->max) {
      knob->value=knob->max;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Up:
  case XK_Right:
    if (knob->value<knob->max) {
      knob->value++;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Left:
  case XK_Down:
    if (knob->value>0) {
      knob->value--;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Prior:
    if (knob->value<knob->max) {
      knob->value+=knob->max/20;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Next:
    if (knob->value>0) {
      knob->value-=knob->max/20;
      up=2;
    } else {
      up=1;
    }
    break;
  }

  if (up&2) {
    if (knob->value<0) knob->value=0;
    if (knob->value>knob->max) knob->value=knob->max;
    KnobUpdate(gadget, 0, 1);
    if (gadget->callbackUp) {
      XFlush(mcw->app->display);
      (*gadget->callbackUp)(gadget);
    }
  }

  return !!up;
}

/***************************************************************************/

McGadget *MakeKnob(McWindow *mcw, int x, int y, int d, int max) {
  McGadget *gad = McCreateGadget(mcw, GAD_ACTIVE, KNOBGADGET, x, y, d, d);
  gad->specialInfo=McCreateKnob(0, max);
  return gad;
}
