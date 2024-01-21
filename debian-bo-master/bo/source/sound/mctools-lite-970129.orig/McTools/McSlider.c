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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "McApp.h"
#include "McGadget.h"
#include "McFocus.h"
#include "McSlider.h"

#define SLIDER ((McSlider *)(gadget->specialInfo))
static void McSliderUpdate(McGadget *gadget, int busy, int all);
static int McSliderEvent(McGadget *gadget, XEvent *event);
static void McSliderCleanup(McGadget *gadget);
static int McSliderKey(McGadget *gadget, XKeyEvent *event);

McSpecialInfo *McCreateSlider(unsigned int flags, unsigned int maxValue) {
  McSlider *slider = (McSlider *) calloc(sizeof(McSlider), 1);
  slider->flags = flags;
  slider->leftValue = slider->rightValue = maxValue>>1;
  slider->maxValue = maxValue;
  slider->specialInfo.updateProc = McSliderUpdate;
  slider->specialInfo.eventProc = McSliderEvent;
  slider->specialInfo.keyboardProc = McSliderKey;
  slider->specialInfo.cleanupProc = McSliderCleanup;
  slider->step = 0;
  slider->width = slider->pixelWidth = 0;
  return (McSpecialInfo *)slider;
}

static void McSliderCleanup(McGadget *gadget) {
  free(gadget->specialInfo);
}

static int CheckBounds(McGadget *gadget) {
  int max;
  if ((max=SLIDER->maxValue)<=0) max=SLIDER->maxValue=1;
  if (SLIDER->leftValue>max) SLIDER->leftValue=max;
  if (SLIDER->leftValue<0) SLIDER->leftValue=0;
  if (SLIDER->rightValue>max) SLIDER->rightValue=max;
  if (SLIDER->rightValue<0) SLIDER->rightValue=0;
  return max;
}

static void RecalcSliderPixels(McGadget *gadget) {
  int width, max, l, r;

  max=CheckBounds(gadget);

  if ((SLIDER->flags) & SLIDER_VERTICAL) {
    width=gadget->height;
    l = max-SLIDER->leftValue;
    r = max-SLIDER->rightValue;
  } else {
    width=gadget->width;
    l = SLIDER->leftValue;
    r = SLIDER->rightValue;
  }

  SLIDER->leftPixels  = (l*(SLIDER->maxPixels)/max) + BW + BW;

  SLIDER->rightPixels = (r*(SLIDER->maxPixels)/max) + BW + BW;

}

static void ReCalcProps(McGadget *gadget) {

  CheckBounds(gadget);

  if (SLIDER->width) {
    int max=SLIDER->maxValue;

    if (SLIDER->flags & SLIDER_VERTICAL) {
      if (SLIDER->flags & SLIDER_VOLUME) {
	SLIDER->pixelWidth = 24;
      } else {
	if (SLIDER->width!=max) {
	  SLIDER->pixelWidth=
	    (SLIDER->width-max)*(gadget->height-(BW<<2))/SLIDER->width;
	} else {
	  SLIDER->pixelWidth = (gadget->height-(BW<<2)) / max;
	}
      }
    } else {
      if (SLIDER->width!=max) {
	SLIDER->pixelWidth=
	  (SLIDER->width-max)*(gadget->width-(BW<<2))/SLIDER->width;
      } else {
	SLIDER->pixelWidth = (gadget->width-(BW<<2)) / max;
      }
    }

    if (SLIDER->pixelWidth<5)
      SLIDER->pixelWidth=5;
  } else {
    SLIDER->pixelWidth = 0;
  }

  if (SLIDER->flags & SLIDER_VERTICAL) {
    SLIDER->maxPixels=(gadget->height)-(BW<<2)-(SLIDER->pixelWidth);
  } else {
    SLIDER->maxPixels=(gadget->width)-(BW<<2)-(SLIDER->pixelWidth);
  }

  RecalcSliderPixels(gadget);
}

void McSliderSetProps(McGadget *gadget, int max, int width) {
  if (max>0) { if (width<max) width=max; } else { max=1; width=1; }
  SLIDER->maxValue = max;
  SLIDER->width = width;
  ReCalcProps(gadget);
}

static void DrawVertical(McWindow *mcw, McGadget *gadget,
			 int pixels, int offs, int gwi) {
  int width = SLIDER->pixelWidth;
  McApp *app=mcw->app;

  if (width) {
    XFillRectangle(app->display, gadget->win, app->gc[GC_SELECTED],
		   offs, 0, gwi, pixels-BW);
    XFillRectangle(app->display, gadget->win, app->gc[GC_CLEAR],
		   offs+BW, pixels, gwi-BW-BW, width);
    McAppDrawbox(mcw, gadget->win, offs+BW, pixels, gwi-BW-BW, width, _3D_OUT);
    XFillRectangle(app->display, gadget->win, app->gc[GC_SELECTED],
		   offs, pixels+width+BW, gwi, gadget->height-pixels-width-BW);
  } else {

    McAppDrawbox(mcw, gadget->win,
		 offs+BW, pixels, gwi-BW-BW, gadget->height-pixels-BW-BW,
		 _3D_OUT);

    XFillRectangle(app->display, gadget->win, app->gc[GC_CLEAR],
		   offs+BW, pixels, gwi-BW-BW, gadget->height-pixels-BW-BW);

    XFillRectangle(app->display, gadget->win, app->gc[GC_SELECTED],
		   offs, BW, gwi, pixels-BW-BW);

  }

  if (SLIDER->flags & SLIDER_VOLUME) {
    int i, y;
    XSegment segments[5];

    y=pixels+(width>>1)-1;

    segments[0].x1=offs+BW+1;
    segments[0].x2=offs+gwi-BW-2;
    segments[0].y1=segments[0].y2=y;

    for (i=1; i<5; i++) {
      segments[i].x1=segments[0].x1+1;
      segments[i].x2=segments[0].x2-1;
    }

    segments[1].y1=segments[1].y2=segments[0].y1+4;
    segments[2].y1=segments[2].y2=segments[0].y1+8;
    segments[3].y1=segments[3].y2=segments[0].y1-4;
    segments[4].y1=segments[4].y2=segments[0].y1-8;

    XDrawSegments(app->display, gadget->win, app->gc[GC_DARK],
		  segments, 5);

    for (i=0; i<5; i++) { segments[i].y1++; segments[i].y2++; }
    XDrawSegments(app->display, gadget->win, app->gc[GC_BRIGHT],
		  segments, 5);

    segments[0].x1=segments[0].x2=segments[1].x1=segments[1].x2=
      offs+(gwi>>1)-1;
    segments[0].y1=1;
    segments[0].y2=pixels-BW-1;
    segments[1].y1=pixels+width+BW;
    segments[1].y2=gadget->height-2;

    XDrawSegments(app->display, gadget->win, app->gc[GC_DARK],
		  segments, 2);
    for (i=0; i<2; i++) { segments[i].x1++; segments[i].x2++; }
    XDrawSegments(app->display, gadget->win, app->gc[GC_BRIGHT],
		  segments, 2);
  }
}


static void DrawHorizontal(McWindow *mcw, McGadget *gadget,
			   int pixels, int offs, int gwi) {
  int width = SLIDER->pixelWidth;
  McApp *app=mcw->app;

  if (width) {
    XFillRectangle(app->display, gadget->win, app->gc[GC_SELECTED],
		   0, offs, pixels-BW, gwi);
    XFillRectangle(app->display, gadget->win, app->gc[GC_CLEAR],
		   pixels, offs+BW, width, gwi-BW-BW);
    McAppDrawbox(mcw, gadget->win, pixels, offs+BW, width, gwi-BW-BW, _3D_OUT);
    XFillRectangle(app->display, gadget->win, app->gc[GC_SELECTED],
		   pixels+width+BW, offs, gadget->width-pixels-width-BW, gwi);
  } else {
    XFillRectangle(app->display, gadget->win, app->gc[GC_CLEAR],
		   BW+BW, offs+BW, pixels-BW-BW, gwi-BW-BW);
    McAppDrawbox(mcw, gadget->win, BW+BW, offs+BW, pixels-BW-BW, gwi-BW-BW,
		 _3D_OUT);
    XFillRectangle(app->display, gadget->win, app->gc[GC_SELECTED],
		   pixels+BW, offs, gadget->width-pixels-BW, gwi);
  }
}


static void McSliderUpdate(McGadget *gadget, int busy,int all) {
  McWindow *mcw=gadget->mcw;

  if (all)
    ReCalcProps(gadget);
  else
    RecalcSliderPixels(gadget);

  if (SLIDER->flags & SLIDER_VERTICAL) {
    /************************/
    /* Update it vertically */
    /************************/

    if (SLIDER->flags & SLIDER_STEREO) {
      DrawVertical(mcw, gadget, SLIDER->leftPixels, BW, (gadget->width>>1)-BW);
      DrawVertical(mcw, gadget,SLIDER->rightPixels,
		   1 + gadget->width/2, (gadget->width>>1) - BW);
    } else {
      DrawVertical(mcw, gadget, SLIDER->leftPixels, BW, gadget->width-BW-BW);
    }
  } else {
    /**************************/
    /* Update it horizontally */
    /**************************/

    if (SLIDER->flags & SLIDER_STEREO) {
      DrawHorizontal(mcw, gadget,  SLIDER->leftPixels, BW,
		     (gadget->height>>1) - BW);
      DrawHorizontal(mcw, gadget, SLIDER->rightPixels, 1 + gadget->height/2,
		   (gadget->height>>1) - BW);
    } else {
      DrawHorizontal(mcw, gadget, SLIDER->leftPixels, BW,
		     gadget->height - BW - BW);
    }
  }
  McDrawFocusMark(gadget,
		  (gadget->flags&GAD_SELECTED)?COL_SELECTED:COL_BACKGROUND);
}


/*
 * Set new slider position, return true if it's really different
 */
static int NewSliderPos(McGadget *gadget, int nl, int nr) {
  int delta=0;
  int max=SLIDER->maxValue;

  if (nl>max) nl=max; else if (nl<0) nl=0;
  if (nr>max) nr=max; else if (nr<0) nr=0;

  if (SLIDER->flags & SLIDER_SET_LEFT) {
    if (nl!=SLIDER->leftValue) {
      SLIDER->leftValue=nl;
      delta=1;
    }
  }
  if (SLIDER->flags & SLIDER_SET_RIGHT) {
    if (nr!=SLIDER->rightValue) {
      SLIDER->rightValue=nr;
      delta=1;
    }
  }

#if 0
  printf("pixelwidth=%d pl/pr=%d/%d l/r=%d/%d(%d)\n",
	 SLIDER->pixelWidth, SLIDER->leftPixels, SLIDER->rightPixels,
	 SLIDER->leftValue,SLIDER->rightValue,SLIDER->maxValue);
#endif

  return delta;
}

/*
 * Move a grabbed slider according to new mouse position
 */
static int GrabMoveSlider(McGadget *gadget, int x, int y) {
  int delta;

  if (!SLIDER->maxPixels) return 0;

  if (SLIDER->flags & SLIDER_VERTICAL) {
    delta=-(y - SLIDER->snapCrsr) * (SLIDER->maxValue) / (SLIDER->maxPixels);
  } else {
    delta=(x - SLIDER->snapCrsr) * (SLIDER->maxValue) / (SLIDER->maxPixels);
  }
  return NewSliderPos(gadget,
		      (SLIDER->snapLeft)+delta, (SLIDER->snapRight)+delta);
}


/*
 * Return False if x/y hits slider bar, -1 if it's above/left and +1
 * when it's below/right.
 */
static int HitSlider(McGadget *gadget, int x, int y) {
  int c, pxl;
  if (SLIDER->flags & SLIDER_VERTICAL) {
    c=y;
    if (x<(gadget->width>>1))
      pxl=SLIDER->leftPixels;
    else
      pxl=SLIDER->rightPixels;
  } else {
    c=x;
    if (y<(gadget->height>>1))
      pxl=SLIDER->leftPixels;
    else
      pxl=SLIDER->rightPixels;
  }

  if (SLIDER->pixelWidth) {
    if (c<pxl-BW) return -1;
    if (c>=(pxl+(SLIDER->pixelWidth)+BW)) return 1;
    return 0;
  } else {
    if (c<(pxl+BW)) return -1;
    return 1;
  }
}

/*
 * Snap the current slider position
 */
static void SnapSlider(McGadget *gadget, int x, int y) {
  SLIDER->snapLeft=SLIDER->leftValue;
  SLIDER->snapRight=SLIDER->rightValue;
  SLIDER->snapCrsr=(SLIDER->flags & SLIDER_VERTICAL)?y:x;
}

/*
 * Move the slider 5% up when dir is negative and 5% down when dir is positive
 */
static int ShiftSlider(McGadget *gadget, int dir) {
  int nl, nr;

  if (SLIDER->flags & SLIDER_VERTICAL) dir=-dir;

  if (dir<0) {
    nl=SLIDER->leftValue - ((SLIDER->maxValue) * 5 / 100);
    if (nl<0) nl=0;
    nr=SLIDER->rightValue - ((SLIDER->maxValue) * 5 / 100);
    if (nr<0) nr=0;
  } else if (dir>0) {
    nl=SLIDER->leftValue + ((SLIDER->maxValue) * 5 / 100);
    if (nl<0) nl=0;
    nr=SLIDER->rightValue + ((SLIDER->maxValue) * 5 / 100);
    if (nr<0) nr=0;
  } else return 0;

  return NewSliderPos(gadget, nl, nr);
}

int McShiftSlider(McGadget *gadget, int dir) {
  int result;
  SLIDER->flags|=SLIDER_SET_LEFT|SLIDER_SET_RIGHT;
  if ((result=ShiftSlider(gadget, dir))) {
    McSliderUpdate(gadget, 0, 0);
    if ((gadget->callbackDown) || (gadget->callbackUp))
      XFlush(gadget->mcw->app->display);
    if (gadget->callbackDown) (*gadget->callbackDown)(gadget);
    if (gadget->callbackUp) (*gadget->callbackUp)(gadget);
  }
  return result;
}

/*
 * Center Slider under Mouseposition
 */
static int CenterSlider(McGadget *gadget, int x, int y) {
  int n;

  if (!SLIDER->maxPixels) return 0;

  if (SLIDER->flags & SLIDER_VERTICAL) {
    n=SLIDER->maxValue -
      ((y-BW-BW-(SLIDER->pixelWidth)/2)*
       (SLIDER->maxValue)/(SLIDER->maxPixels));
  } else {
    n=(x-BW-BW-(SLIDER->pixelWidth)/2)*(SLIDER->maxValue)/(SLIDER->maxPixels);
  }

  return NewSliderPos(gadget, n, n);
}

/*
 * Handle events
 */
static int McSliderEvent(McGadget *gadget, XEvent *event) {
  int i,j,dir;

  if (event->type==Expose) return 0;

  if (gadget->flags&GAD_PRESSED) {
    switch (event->type) {
    case ButtonRelease:
      gadget->flags&=~GAD_PRESSED;
      SLIDER->flags&=~SLIDER_SET_MASK;
      if (McInRectangle(event->xbutton.x, event->xbutton.y,
			0, 0, gadget->width, gadget->height)) {
	if (gadget->callbackUp) {
	  (*gadget->callbackUp)(gadget);
	}
      }
      return 1;

    case MotionNotify:
      if (GrabMoveSlider(gadget, event->xmotion.x, event->xmotion.y)) {
	McSliderUpdate(gadget, 0, 0);
	if (gadget->callbackDown) {
	  (*gadget->callbackDown)(gadget);
	}
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
	if (SLIDER->flags & SLIDER_STEREO) {
	  SLIDER->flags&=~SLIDER_SET_MASK;
	  if (SLIDER->flags & SLIDER_VERTICAL) {
	    i = event->xbutton.x;
	    j = gadget->width;
	  } else {
	    i = event->xbutton.y;
	    j = gadget->height;
	  }
	  if (i < (j/3))
	    SLIDER->flags|=SLIDER_SET_LEFT;
	  else
	    if (i > (j/3*2))
	      SLIDER->flags|=SLIDER_SET_RIGHT;
	    else
	      SLIDER->flags|=SLIDER_SET_LEFT | SLIDER_SET_RIGHT;
	} else {
	  SLIDER->flags|=SLIDER_SET_LEFT | SLIDER_SET_RIGHT;
	}
	switch (event->xbutton.button) {
	case 2:
	  if (CenterSlider(gadget, event->xbutton.x, event->xbutton.y)) {
	    McSliderUpdate(gadget, 0, 0);
	    McSetFocus(gadget);
	    if (gadget->callbackDown) {
	      XFlush(gadget->mcw->app->display);
	      (*gadget->callbackDown)(gadget);
	    }
	  }
	  SnapSlider(gadget, event->xbutton.x, event->xbutton.y);
	  gadget->flags|=GAD_PRESSED;
	  McMoveGadgetToStart(gadget);
	  return 1;
	case 1:
	  if (!(dir=HitSlider(gadget, event->xbutton.x,event->xbutton.y))) {
	    McSetFocus(gadget);
	    SnapSlider(gadget, event->xbutton.x, event->xbutton.y);
	    gadget->flags|=GAD_PRESSED;
	  } else {
	    if (ShiftSlider(gadget, dir)) {
	      McSliderUpdate(gadget, 0, 0);
	      McSetFocus(gadget);
	      if ((gadget->callbackDown) || (gadget->callbackUp))
		XFlush(gadget->mcw->app->display);
	      if (gadget->callbackDown) (*gadget->callbackDown)(gadget);
	      if (gadget->callbackUp) (*gadget->callbackUp)(gadget);
	    }
	    gadget->flags&=~GAD_PRESSED;
	  }
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
}

/***************************************************************************/

static int McSliderKey(McGadget *gadget, XKeyEvent *event) {
  int dir=0;
  McWindow *mcw=gadget->mcw;

  switch((int)mcw->keysym) {
  case XK_Up:
    if (SLIDER->flags&SLIDER_VERTICAL) dir=-1;
    break;

  case XK_Down:
    if (SLIDER->flags&SLIDER_VERTICAL) dir=1;
    break;

  case XK_Left:
    if (!(SLIDER->flags&SLIDER_VERTICAL)) dir=-1;
    break;

  case XK_Right:
    if (!(SLIDER->flags&SLIDER_VERTICAL)) dir=1;
    break;
  }

  if (dir) {
    McShiftSlider(gadget, dir);
    return 1;
  }

  return 0;
}

/***************************************************************************/

McGadget *MakeProp(McWindow *mcw, int x, int y, int w, int h, int id,
		   void (*callback)(struct McGadget *)) {
  McGadget *gadget;

  if (x<0) x=mcw->w+x-w;
  if (y<0) x=mcw->h+y-h;

  gadget = McCreateGadget(mcw, GAD_H3D|GAD_3D|GAD_SELECTED|GAD_ACTIVE,
			  SLIDERGADGET, x, y, w, h);
  gadget->specialInfo = McCreateSlider(SLIDER_MONO|SLIDER_VERTICAL, 1);
  McSliderSetProps(gadget, 1, 0);
  gadget->callbackUp = gadget->callbackDown = callback;
  gadget->id = id;
  return gadget;
}

