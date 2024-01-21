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
#include "McApp.h"
#include "McGadget.h"
#include "McMeter.h"

#define METER ((McMeter *)(gadget->specialInfo))
#define LED_WIDTH  10
#define LED_HEIGHT  3

/****************************************************************************/

static void MeterUpdate(McGadget *gadget, int busy, int all);
static int MeterEvent(McGadget *gadget, XEvent *event);
static void MeterCleanup(McGadget *gadget);

/****************************************************************************/

McSpecialInfo *McCreateMeter(unsigned int flags, int nleds,
				    int oyellow, int ored) {
  McMeter *meter = (McMeter *) calloc(sizeof(McMeter), 1);
  meter->flags = flags;
  meter->nleds=nleds;
  meter->oyellow=oyellow;
  meter->ored=ored;
  meter->value=nleds>>1;
  
  meter->specialInfo.updateProc  = MeterUpdate;
  meter->specialInfo.eventProc   = MeterEvent;
  meter->specialInfo.cleanupProc = MeterCleanup;

  return (McSpecialInfo *)meter;
}

/****************************************************************************/

static void MeterCleanup(McGadget *gadget) {
  free(gadget->specialInfo);
}

/****************************************************************************/

#define METERCOLOR(clr) \
    if (col!=clr) XSetForeground(app->display, gc, app->colors[(col=clr)]);

static void MeterUpdate(McGadget *gadget, int busy, int all) {
  McMeter *meter=METER;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  Display *display=app->display;
  int i,y,col,big;
  GC gc;
  XPoint points[3];

  gc=app->gc[GC_NORMAL];
  col=-1;
  y=(LED_HEIGHT+4)*(meter->nleds-1);
  for (i=0; i<meter->nleds; i++) {

    if (all) {
      points[0].x=LED_WIDTH+1;
      points[0].y=y;
      points[1].x=0;
      points[1].y=y;
      points[2].x=0;
      points[2].y=y+LED_HEIGHT+1;
      XDrawLines(display, gadget->win, app->gc[GC_BRIGHT],
		 points, 3, CoordModeOrigin);

      points[0].x=LED_WIDTH+1;
      points[0].y=y+1;
      points[1].x=LED_WIDTH+1;
      points[1].y=y+LED_HEIGHT+1;
      points[2].x=1;
      points[2].y=y+LED_HEIGHT+1;
      XDrawLines(display, gadget->win, app->gc[GC_DARK],
		 points, 3, CoordModeOrigin);
    }

    if (app->flags&MCAPP_COLOR) {
      if (i<meter->value) {
	if (i>=meter->ored) {
	  METERCOLOR(COL_RED);
	} else if (i>=meter->oyellow) {
	  METERCOLOR(COL_YELLOW);
	} else {
	  METERCOLOR(COL_GREEN);
	}
	big=1;
      } else {
	METERCOLOR(COL_WHITE);
	big=0;
      }
    } else {
      if (i<meter->value) {
	big=1;
	gc=app->gc[GC_SELECTED];
      } else {
	big=0;
	gc=app->gc[GC_NORMAL];
      }
    }
    
    if (big) {
      XFillRectangle(display, gadget->win, gc,
		     1, y+1, LED_WIDTH, 3);
    } else {
      XDrawRectangle(display, gadget->win, app->gc[GC_CLEAR],
	       1, y+1, LED_WIDTH-1, 2);
      XDrawLine(display, gadget->win, gc,
		2, y+2, LED_WIDTH-1, y+2);
    }

    y-=LED_HEIGHT+4;
  }

  if (app->flags&MCAPP_COLOR) {
    METERCOLOR(COL_FOREGROUND);
  }
  
}

/****************************************************************************/

static int MeterEvent(McGadget *gadget, XEvent *event) {
  return 0;
}

/****************************************************************************/

void McSetMeter(McGadget *gadget, int val) {
  if (val<0) val=0;
  if (val>(METER->nleds)) val=METER->nleds;
  METER->value=val;
  MeterUpdate(gadget, 0, 0);
}

/****************************************************************************/

McGadget *MakeMeter(McWindow *mcw, int x, int y,
		    int nleds, int oyellow, int ored) {
  int w = LED_WIDTH+2;
  int h = (LED_HEIGHT+4)*nleds-2;
  McGadget *gad = McCreateGadget(mcw, GAD_HNONE, METERGADGET, x, y, w, h);

  gad->specialInfo=McCreateMeter(0, nleds, oyellow, ored);

  return gad;
}
