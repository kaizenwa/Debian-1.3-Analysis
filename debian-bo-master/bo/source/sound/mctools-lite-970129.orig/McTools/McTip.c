/* Copyright (C) 1996 
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


#include "McApp.h"
#include <X11/cursorfont.h>
#include <stdio.h>
#include <string.h>
#include "McGadget.h"
#include "McTip.h"

static const string_t emptytip="";

/****************************************************************************/

static const string_t GetTip(McGadget *gadget, int *x, int *y) {
  if (!gadget) return NULL;

  if ((gadget->specialInfo) && (gadget->specialInfo->tipProc)) {
    Window nonsense;
    const string_t tip;

    McApp *app=gadget->mcw->app;
    XTranslateCoordinates(app->display, RootWindow(app->display, app->screen),
			  gadget->win, *x, *y, x, y, &nonsense);
    tip=(*(gadget->specialInfo->tipProc))(gadget, x, y);
    if (tip) return tip;
    if (app->tip) return app->tip;
    return emptytip;
  } else {
    *x=gadget->width>>1;
    *y=gadget->height-BW;
    return gadget->tip;
  }
}

/****************************************************************************/

int McTipPopup(McApp *app) {
  int		direction, ascent, descent;
  XCharStruct	overall;
  XSetWindowAttributes attr;
  McGadget *gadget;
  McWindow *mcw;
  int ww, wh, wx, wy;
  Window win;

  gadget=app->tipgad;
  if (!gadget) return 0;
  wx=app->tipx; wy=app->tipy;
  app->tip=GetTip(gadget, &wx, &wy);
  if (!app->tip) return 0;

  mcw=gadget->mcw;
  XTextExtents(app->defaultFont, app->tip, strlen(app->tip),
	       &direction, &ascent, &descent, &overall);
  app->tipascent=ascent;

  app->tipw=ww=overall.width+9; app->tiph=wh=ascent+descent+9;

  wx-=ww>>1;
  XTranslateCoordinates(app->display, gadget->win,
			RootWindow(app->display, app->screen),
			wx, wy, &wx, &wy, &win);

  attr.override_redirect = True;
  attr.save_under = True;
  attr.background_pixel=app->colors[COL_MENU];
  if (wx<0) wx=0; if (wy<0) wy=0;
  win=XCreateWindow(app->display, RootWindow(app->display, app->screen),
		    wx, wy, ww, wh, 0, CopyFromParent, InputOutput,
		    CopyFromParent,
		    CWSaveUnder|CWOverrideRedirect, &attr);

  if (app->tip!=emptytip) XMapRaised(app->display, win);
  app->tipwin=win;

  McTipRedraw(app);

  return 1;
}

/****************************************************************************/

void McTipRedraw(McApp *app) {
  Window win;
  int wx, wy;

  if (!(win=app->tipwin)) return;

  if (app->tip==emptytip) return;

  wx=app->tipx; wy=app->tipy;
  if (!(app->tip=GetTip(app->tipgad, &wx, &wy))) return;

  XSetStipple(app->display, app->gc[GC_BUSY], app->stipple1);
  XFillRectangle(app->display, win, app->gc[GC_BUSY],
		 3, 3, app->tipw-3, app->tiph-3);
  XSetStipple(app->display, app->gc[GC_BUSY], app->stipple3);

  XSetForeground(app->display, app->gc[GC_NORMAL],
		 app->colors[COL_TIP]);
  XFillRectangle(app->display, win, app->gc[GC_NORMAL],
		 1, 1, app->tipw-5, app->tiph-5);
  XSetForeground(app->display, app->gc[GC_NORMAL],
		 app->colors[COL_FOREGROUND]);

  XSetBackground(app->display, app->gc[GC_NORMAL],
		 app->colors[COL_TIP]);
  XDrawRectangle(app->display, win, app->gc[GC_NORMAL],
		 0, 0, app->tipw-4, app->tiph-4);
  XDrawImageString(app->display, win, app->gc[GC_NORMAL],
		   BW+1, BW+app->tipascent+1, app->tip, strlen(app->tip));
  XSetBackground(app->display, app->gc[GC_NORMAL],
		 app->colors[COL_BACKGROUND]);
}

/****************************************************************************/

static void McDestroyTipwin(McApp *app) {
  if (app->tipwin) {
    XDestroyWindow(app->display, app->tipwin);
    app->tipwin=0;
  }
}

/****************************************************************************/

int McTipMotion(McApp *app, int x, int y) {
  Window win;
  const string_t tip;
  int wx, wy;

  if (!(win=app->tipwin)) return 0;

  app->tipx=x;
  app->tipy=y;

  wx=app->tipx; wy=app->tipy;
  if (!(tip=GetTip(app->tipgad, &wx, &wy))) return 0;
  if (tip==app->tip) return 0;

  app->tip=tip;

  McDestroyTipwin(app);
  McTipPopup(app);

  return 1;
}

/****************************************************************************/


void McTipPopdown(McApp *app) {
  McDestroyTipwin(app);
  if (app->tipgad) {
    XSelectInput(app->display, app->tipgad->win, MCGADGET_EVENT_MASK);
    app->tipgad=0;
  }

  app->tipx=0;
  app->tipy=0;
  app->tipto=0;
}

/****************************************************************************/

void McTipEnter(McGadget *gadget, int x, int y) {
  if (((gadget->specialInfo) && (gadget->specialInfo->tipProc)) ||
      (gadget->tip)) {
    McApp *app=gadget->mcw->app;
    app->tipto=800000;
    app->tipgad=gadget;
    app->tipx=x;
    app->tipy=y;
    app->tipmotions=100;
    XSelectInput(app->display, app->tipgad->win,
		 MCGADGET_EVENT_MASK | PointerMotionMask);
  } else {
    gadget->mcw->app->tipto=0;
  }
}
