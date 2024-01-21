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


#include <X11/keysym.h>
#include <X11/cursorfont.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xatom.h>
#include "McApp.h"
#include "McGadget.h"
#include "McFocus.h"
#include "McDebug.h"
#include "McSlider.h"
#include "McViewBitmap.h"
#include "McSelection.h"
#include "McBuffer.h"

/**************************************************************************/

#define VIEWBITMAP ((McViewBitmap *)(gadget->specialInfo))

static void ViewBitmapCleanup(McGadget *gadget);
static int  ViewBitmapEvent(McGadget *gadget, XEvent *event);
static void ViewBitmap_slider_update(McGadget *gadget,
				     McViewBitmap *ViewBitmap);
static int ViewBitmapKey(McGadget *gadget, XKeyEvent *event);

/**************************************************************************/

McSpecialInfo *McCreateViewBitmap(unsigned int flags) {
  McViewBitmap *ViewBitmap = (McViewBitmap *) calloc(sizeof(McViewBitmap), 1);
  ViewBitmap->flags = flags;
  ViewBitmap->specialInfo.updateProc = McViewBitmapUpdate;
  ViewBitmap->specialInfo.eventProc = ViewBitmapEvent;
  ViewBitmap->specialInfo.keyboardProc = ViewBitmapKey;
  ViewBitmap->specialInfo.cleanupProc = ViewBitmapCleanup;
  ViewBitmap->size_x = 1;
  ViewBitmap->size_y = 1;
  ViewBitmap->home_x = 0;
  ViewBitmap->home_y = 0;
  ViewBitmap->pixmap = 0;
  ViewBitmap->depth  = 0;

  return (McSpecialInfo *)ViewBitmap;
}

static void ViewBitmapCleanup(McGadget *gadget) {
  free(gadget->specialInfo);
}

/**************************************************************************/

void McViewBitmapUpdate(McGadget *gadget, int busy, int all) {
  McViewBitmap *ViewBitmap = (McViewBitmap *)gadget->specialInfo;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;

  if (ViewBitmap->pixmap) {
    if (ViewBitmap->depth>1) {
      XCopyArea(app->display, ViewBitmap->pixmap, gadget->win,
		app->gc[GC_NORMAL],
		ViewBitmap->home_x, ViewBitmap->home_y,
		gadget->width, gadget->height, 0, 0);
    } else {
      XCopyPlane(app->display, ViewBitmap->pixmap, gadget->win,
		 app->gc[GC_NORMAL],
		 ViewBitmap->home_x, ViewBitmap->home_y,
		 gadget->width, gadget->height, 0, 0, 1);
    }
  }

  ViewBitmap_slider_update(gadget, ViewBitmap);
  McDrawFocusMark(gadget, COL_BACKGROUND);
}

/**************************************************************************/

static int ViewBitmapEvent(McGadget *gadget, XEvent *event) {

  switch (event->type) {
  case Expose:
    return 0;

  case ButtonPress:
    if (event->xbutton.button!=1) return 0;
    if (McInRectangle(event->xbutton.x, event->xbutton.y,
		      0, 0, gadget->width, gadget->height)) {
      McMoveGadgetToStart(gadget);
      McSetFocus(gadget);
      return 1;
    }
    return 0;
  }

  return 0;
}

/**************************************************************************/

static int ViewBitmapKey(McGadget *gadget, XKeyEvent *event) {
  int up=0, maxh, maxw, stepx, stepy;
  McWindow *mcw=gadget->mcw;
  McViewBitmap *viewbitmap = (McViewBitmap *)gadget->specialInfo;

  maxh=viewbitmap->size_y-(gadget->height-BW)-1;
  maxw=viewbitmap->size_x-(gadget->width-BW)-1;
  if ((stepx=maxw/20)<4) if ((stepx=maxw/4)<1) stepx=1;
  if ((stepy=maxh/20)<4) if ((stepy=maxh/4)<1) stepy=1;

  switch((int)mcw->keysym) {
  case XK_Home:
    if (viewbitmap->home_x || viewbitmap->home_y) {
      viewbitmap->home_x=0;
      viewbitmap->home_y=0;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_End:
    if ((viewbitmap->home_y!=maxh) || (viewbitmap->home_x!=maxw)) {
      viewbitmap->home_x=maxw;
      viewbitmap->home_y=maxh;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Prior:
  case XK_Up:
    if (viewbitmap->home_y>0) {
      viewbitmap->home_y-=stepy;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Next:
  case XK_Down:
    if (viewbitmap->home_y<maxh) {
      viewbitmap->home_y+=stepy;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Left:
    if (viewbitmap->home_x>0) {
      viewbitmap->home_x-=stepx;
      up=2;
    } else {
      up=1;
    }
    break;

  case XK_Right:
    if (viewbitmap->home_x<maxw) {
      viewbitmap->home_x+=stepx;
      up=2;
    } else {
      up=1;
    }
    break;

  }

  if (up&2) {
    if (viewbitmap->home_y<0) viewbitmap->home_y=0;
    if (viewbitmap->home_y>maxh) viewbitmap->home_y=maxh;
    if (viewbitmap->home_x<0) viewbitmap->home_x=0;
    if (viewbitmap->home_x>maxw) viewbitmap->home_x=maxw;
    McViewBitmapUpdate(gadget, 0, 1);
  }

  return !!up;
}

/***************************************************************************/

McGadget *MakeViewBitmap(McWindow *mcw, int x, int y, int w, int h) {
  McGadget *gadget;

  if (w<=0) w=mcw->w-(BW<<2);
  if (h<=0) h=mcw->h-(BW<<2);
  if (x==0) x=(mcw->w>>1)-(w>>1);	/* Center */
  if (y==0) y=(mcw->h>>1)-(h>>1);
  if (x<0) x=mcw->w+x-w-BW-BW-BW;	/* Right justify */
  if (y<0) y=mcw->h+y-h-BW-BW-BW;

  gadget = McCreateGadget(mcw, GAD_3D|GAD_H3D|GAD_ACTIVE,
			  VIEWBITMAPGADGET, x, y, w, h);
  gadget->specialInfo = McCreateViewBitmap(0);
  return gadget;
}

/**************************************************************************/

static void ValidateHome(McGadget *gadget) {
  McViewBitmap *ViewBitmap = (McViewBitmap *)gadget->specialInfo;

  if (ViewBitmap->home_x >= ViewBitmap->size_x - gadget->width)
    ViewBitmap->home_x = ViewBitmap->size_x - gadget->width;

  if (ViewBitmap->home_y >= ViewBitmap->size_y - gadget->height)
    ViewBitmap->home_y = ViewBitmap->size_y - gadget->height;

  if (ViewBitmap->home_x < 0) ViewBitmap->home_x = 0;

  if (ViewBitmap->home_y < 0) ViewBitmap->home_y = 0;
}

void McViewBitmapSetPixmap(McGadget *gadget, Pixmap pix, int depth,
			   int size_x, int size_y) {
  McViewBitmap *ViewBitmap = (McViewBitmap *)gadget->specialInfo;

  ViewBitmap->home_x = ViewBitmap->home_x * size_x / ViewBitmap->size_x;
  ViewBitmap->home_y = ViewBitmap->home_y * size_y / ViewBitmap->size_y;

  ViewBitmap->pixmap=pix;
  ViewBitmap->depth=depth;
  ViewBitmap->size_x=size_x;
  ViewBitmap->size_y=size_y;

  ValidateHome(gadget);

  ViewBitmap_slider_update(gadget, ViewBitmap);

#if 0
  fprintf(stderr, "home=%d/%d size=%d/%d gadget=%d/%d\n",
	  ViewBitmap->home_x, ViewBitmap->home_y,
	  ViewBitmap->size_x, ViewBitmap->size_y,
	  gadget->width, gadget->height);	  
#endif

}

/**************************************************************************/

void McViewBitmapSetHome(McGadget *gadget, int home_x, int home_y) {
  McViewBitmap *ViewBitmap = (McViewBitmap *)gadget->specialInfo;

  ViewBitmap->home_x = home_x;
  ViewBitmap->home_y = home_y;
  ValidateHome(gadget);
}

/**************************************************************************/

static void slider_ver_proc(McGadget *gadget);
static void slider_hor_proc(McGadget *gadget);

void McViewBitmapBindSliders(McGadget *bitgad,
			   McGadget *sliderhor, McGadget *sliderver) {
  McViewBitmap *ViewBitmap = (McViewBitmap *)bitgad->specialInfo;
  ViewBitmap->sliderhor = sliderhor;
  ViewBitmap->sliderver = sliderver;
  if (sliderhor) {
    sliderhor->callbackUp = sliderhor->callbackDown = slider_hor_proc;
    sliderhor->customData = bitgad;
  }
  if (sliderver) {
    sliderver->callbackUp = sliderver->callbackDown = slider_ver_proc;
    sliderver->customData = bitgad;
  }
}

static void ViewBitmap_slider_update(McGadget *gadget,
				     McViewBitmap *ViewBitmap) {
  McGadget *slidergad;

  if ((slidergad=ViewBitmap->sliderhor)) {
    McSlider *slider = (McSlider *)slidergad->specialInfo;
    int max = ViewBitmap->size_x - gadget->width;
    if (max<0) max=0;
    slider->leftValue=slider->rightValue=ViewBitmap->home_x;
    McSliderSetProps(slidergad, max, ViewBitmap->size_x);
    McGadgetUpdate(slidergad);
  }

  if ((slidergad=ViewBitmap->sliderver)) {
    McSlider *slider = (McSlider *)slidergad->specialInfo;
    int max = ViewBitmap->size_y - gadget->height;
    if (max<0) max=0;
    slider->leftValue=slider->rightValue=max-ViewBitmap->home_y;
    McSliderSetProps(slidergad, max, ViewBitmap->size_y);
    McGadgetUpdate(slidergad);
  }
}

static void slider_hor_proc(McGadget *gadget) {
  McSlider *slider = (McSlider *)gadget->specialInfo;
  McGadget *bitgad = (McGadget *)gadget->customData;
  McViewBitmap *ViewBitmap = (McViewBitmap *)bitgad->specialInfo;
  int col = slider->leftValue;

  if (ViewBitmap->home_x!=col) {
    ViewBitmap->home_x = col;
    McViewBitmapUpdate(bitgad, 0, 1);
    McSetFocus(bitgad);
  }
}

static void slider_ver_proc(McGadget *gadget) {
  McSlider *slider = (McSlider *)gadget->specialInfo;
  McGadget *bitgad = (McGadget *)gadget->customData;
  McViewBitmap *ViewBitmap = (McViewBitmap *)bitgad->specialInfo;
  int row = slider->maxValue-slider->leftValue;

  if (ViewBitmap->home_y!=row) {
    ViewBitmap->home_y = row;
    McViewBitmapUpdate(bitgad, 0, 1);
    McSetFocus(bitgad);
  }
}





