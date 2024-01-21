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
#include "McLoad.h"

#define LOAD ((McLoad *)(gadget->specialInfo))

static void McLoadUpdate(McGadget *gadget, int busy, int all);
static int McLoadEvent(McGadget *gadget, XEvent *event);
static void McLoadCleanup(McGadget *gadget);

McSpecialInfo *McCreateLoad(unsigned int flags, int depth,
			    int min, int max, int base) {
  McLoad *load = (McLoad *) calloc(sizeof(McLoad), 1);
  load->flags = flags;
  load->depth=depth;
  load->ptr=0;
  load->min=min;
  load->max=max;
  load->base=base;

  load->data = (signed short *)calloc(depth, sizeof(signed short));

  load->specialInfo.updateProc = McLoadUpdate;
  load->specialInfo.eventProc = McLoadEvent;
  load->specialInfo.cleanupProc = McLoadCleanup;

  return (McSpecialInfo *)load;
}

static void McLoadCleanup(McGadget *gadget) {
  free(LOAD->data);
  free(gadget->specialInfo);
}

static void McLoadUpdate(McGadget *gadget, int busy,int all) {

  int ptr,py,px,w=gadget->width,h=gadget->height;
  int ye = h-1;
  McWindow *mcw=gadget->mcw;
  McApp *app = mcw->app;
  Display *dsp = app->display;
  GC gc1,gc2;

  if (gadget->flags & GAD_SELECTED) {
    gc1=app->gc[GC_SELECTED];
    gc2=app->gc[GC_NORMAL];
  } else {
    gc1=app->gc[GC_CLEAR];
    gc2=app->gc[GC_NORMAL];
  }

  ptr=LOAD->ptr;
  if (LOAD->flags & LOAD_RIGHT_ALIGNED) {
    px=w-1;
  } else {
    if (ptr>=w) {
      px=w;
    } else {
      px=ptr;
      if (px<=(w-1))
	XFillRectangle(dsp, gadget->win, gc1, px, 0, w-px, h);
    }
    px--;
  }

  if (LOAD->flags & LOAD_BAR) {
    while ((px>=0) && ptr) {
      py = h - ((LOAD->data[--ptr]-LOAD->min) * h / LOAD->max);
      if (py)   XDrawLine(dsp, gadget->win, gc1, px,  0, px, py-1);
      if (py<h) XDrawLine(dsp, gadget->win, gc2, px, py, px, h-1);
      px--;
    }
  } else {
    while ((px>=0) && ptr) {
      py = h - ((LOAD->data[--ptr]-LOAD->min) * h / LOAD->max);
      XDrawLine (dsp, gadget->win, gc1, px,  0, px, ye);
      XDrawPoint(dsp, gadget->win, gc2, px, py-1);
      px--;
    }
  }
  if (px>=0) {
    XFillRectangle(dsp, gadget->win, gc1, 0, 0, px+1, h);
  }
}

static int McLoadEvent(McGadget *gadget, XEvent *event) {
  return 0;
}

void McLoadShiftIn(McGadget *gadget, signed short val) {
  if (LOAD->ptr >= LOAD->depth) {
    memmove(LOAD->data,&LOAD->data[1],(LOAD->depth-1)*sizeof(signed short));
    LOAD->ptr=LOAD->depth-1;
    LOAD->data[LOAD->ptr++]=val;
  } else {
    LOAD->data[LOAD->ptr++]=val;
  }
  McGadgetUpdate(gadget);
}
