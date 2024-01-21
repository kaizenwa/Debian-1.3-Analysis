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
#include <X11/Xatom.h>
#include "McApp.h"
#include "McGadget.h"
#include "McFocus.h"
#include "McSelector.h"
#include "McSlider.h"
#include "McDebug.h"

#define SELECTOR ((McSelector *)(gadget->specialInfo))
#define SELECTORHEIGHT(gadget,selector) \
  ((gadget->height-BW-BW) / (selector->height))


static int McSelectorEvent(McGadget *gadget, XEvent *event);
static void McSelectorCleanup(McGadget *gadget);
static int McSelectorKey(McGadget *gadget, XKeyEvent *event);

McSpecialInfo *McCreateSelector(unsigned int flags, XFontStruct *font,
				const unsigned char *const *const data,
				int size) {

  McSelector *selector = (McSelector *) calloc(sizeof(McSelector), 1);
  selector->flags = flags;
  selector->specialInfo.updateProc = McSelectorUpdate;
  selector->specialInfo.eventProc = McSelectorEvent;
  selector->specialInfo.keyboardProc = McSelectorKey;
  selector->specialInfo.cleanupProc = McSelectorCleanup;
  selector->specialInfo.flags = GSP_FOCUSUPDATE;
  selector->font = font;
  selector->data = data;
  selector->size = size;
  selector->first = 0;
  selector->oldselection = selector->selection = -1;
  selector->height=font->ascent+font->descent;
  return (McSpecialInfo *)selector;
}

static void McSelectorCleanup(McGadget *gadget) {
  free(gadget->specialInfo);
}

static __inline__ void selector_slider_update(McGadget *gadget,
				   McSelector *selector) {
  McGadget *slidergad;
  if ((slidergad=selector->slidergad)) {
    McSlider *slider = (McSlider *)slidergad->specialInfo;
    int max = selector->size-McSelectorHeight(gadget);
    slider->leftValue=slider->rightValue=max-selector->first;
    McSliderSetProps(slidergad, (max<1)?1:max, selector->size);
    McGadgetUpdate(slidergad);
  }
}

int McSelectorWidth(McGadget *gadget) {
  int		direction, ascent, descent;
  XCharStruct	overall;
  McSelector *selector = (McSelector *)gadget->specialInfo;
  XTextExtents(selector->font," ",1, &direction, &ascent, &descent, &overall);
  return (gadget->width-BW-BW) / overall.width;
}

int McSelectorHeight(McGadget *gadget) {
  McSelector *selector = (McSelector *)gadget->specialInfo;
  return SELECTORHEIGHT(gadget, selector);
}

void McSelectorUpdate(McGadget *gadget,int busy,int all) {
  int x, y, i, imax, bgcol;
  GC gc1, gc3, gc;
  McSelector *selector = (McSelector *)gadget->specialInfo;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  int height=SELECTORHEIGHT(gadget,selector);

  /* At first, validate the positions */

  if (selector->selection<selector->first) {
    selector->first=selector->selection;
    all=1;
  } else {
    if (selector->selection>=selector->first+height+1) {
      selector->first=selector->selection-height+1;
      all=1;
    }
  }

  if ((selector->first+height>=selector->size)
      && (height<selector->size)) {
    selector->first=selector->size-height;
    all=1;
  }

  if (selector->first<0) {
    selector->first=0;
    all=1;
  }

  if (gadget->flags & GAD_SELECTED) {
    gc1=app->gc[GC_SELECTED];
    gc3=app->gc[GC_SELECTED_BITMAP];
    bgcol=COL_SELECTED;
  } else {
    gc1=app->gc[GC_SELECTED];
    gc3=app->gc[GC_NORMAL];
    bgcol=COL_BACKGROUND;
  }

  gc=XCreateGC(app->display, gadget->win, 0, NULL);
  XCopyGC(app->display, gc1, ~0, gc);
  XSetFont(app->display, gc, selector->font->fid);
  gc1=gc;
  gc=XCreateGC(app->display, gadget->win, 0, NULL);
  XCopyGC(app->display, gc3, ~0, gc);
  XSetFont(app->display, gc, selector->font->fid);
  gc3=gc;

  if (app->style) {
    XSetForeground(app->display, gc1, app->colors[COL_BRIGHT]);
    XSetBackground(app->display, gc1, app->colors[COL_FOREGROUND]);
    XSetBackground(app->display, gc3, app->colors[COL_BRIGHT]);
    bgcol=COL_BRIGHT;
  }

  if (all) {
    x = BW;
    y = BW;

    XFillRectangle(app->display, gadget->win, gc1, 0, 0, -1, y);
    XFillRectangle(app->display, gadget->win, gc1, 0, gadget->height-BW,-1,-1);

    imax = (gadget->height-BW-BW)/selector->height + selector->first;
    if (imax>=selector->size) imax=selector->size-1;

    if (selector->data) {
      for(i=selector->first;i<=imax;i++) {
	XFillRectangle(app->display, gadget->win, gc1, 0, y,
		       gadget->width,selector->height);
	XDrawImageString(app->display, gadget->win, gc3,
			 x, y + selector->font->ascent,
			 selector->data[i], strlen(selector->data[i]));
	y+=selector->height;
      }
    }
    if (y<(gadget->height-BW))
      XFillRectangle(app->display, gadget->win, gc1,
		     0, y, gadget->width, gadget->height-y);
    selector->oldselection=0;
    selector_slider_update(gadget,selector);
  } else {
    if (selector->data) {
      if ((i=selector->oldselection)>=0) {
	XFillRectangle(app->display, gadget->win, gc1,
		       0, BW+(i-selector->first)*selector->height,
		       gadget->width,selector->height);
	XDrawImageString(app->display, gadget->win, gc3,
			 BW, BW+(i-selector->first)*selector->height+
			 selector->font->ascent,
			 selector->data[i], strlen(selector->data[i]));
      }
    }
  }

  if ((mcw->keyboardFocus==gadget) && (selector->selection<0) &&
      (selector->size>0) && (selector->first<selector->size))
    selector->selection=selector->first;

  McDrawFocusMark(gadget, bgcol);

  if (selector->data) {
    if (((i=selector->selection)>=0)) {
      int yy2=BW+(i-selector->first)*selector->height;
      int yy=yy2+selector->font->ascent;

/* focus:
 * win
 *  0     box
 *  1     inverse
 */
      if (mcw->window_has_focus) {
	XFillRectangle(app->display, gadget->win, gc3,
		       0, yy2, gadget->width,selector->height);
	XDrawImageString(app->display, gadget->win, gc1,
			 BW, yy,
			 selector->data[i], strlen(selector->data[i]));
      } else {
	XFillRectangle(app->display, gadget->win, gc1,
		       0, yy2, gadget->width,selector->height);
	XDrawImageString(app->display, gadget->win, gc3,
			 BW, yy,
			 selector->data[i], strlen(selector->data[i]));
	XDrawRectangle(app->display, gadget->win, gc3,
		       0, yy2, gadget->width-1,selector->height-1);
      }
      selector->oldselection=-1;
    }
  }

  XFreeGC(app->display, gc1);
  XFreeGC(app->display, gc3);
}

/*
 * check if line is currently displayed, and if so, redraw selector
 * This could one day just redraw the single line.
 */
void McSelectorRefreshLine(McGadget *gadget, int line) {
  McSelector *selector = (McSelector *)gadget->specialInfo;
  int height=SELECTORHEIGHT(gadget, selector);
  if ((line>=selector->first) && (line<selector->first+height)) {
    McSelectorUpdate(gadget, 0, 1);
  }
}


/* This will always redraw the current selection, as the
 * string may have changed.
 */
int McSelectorSelect(McGadget *gadget, int selection) {
  McSelector *selector = (McSelector *)gadget->specialInfo;
  if ((selection<selector->size) && (selection != selector->selection)) {
    selector->oldselection=selector->selection;
    selector->selection=selection;
    McSelectorUpdate(gadget, 0, 0);
    selector_slider_update(gadget,selector);
    return 1;
  }
  return 0;
}

void McSelectorSetList(McGadget *gadget,
		       const unsigned char *const *const data,
		       int size, int selection) {
  McSelector *selector = (McSelector *) gadget->specialInfo;

  selector->data = data;
  selector->size = size;
  selector->first = 0;
  selector->selection = selection;
  selector->oldselection = -1;
  McGadgetRedraw(gadget);
  selector_slider_update(gadget,selector);
}

static int McSelectorEvent(McGadget *gadget, XEvent *event) {
  McSelector *selector = (McSelector *)gadget->specialInfo;
  int selection, diff;

  if (event->type==Expose) return 0;

  if (gadget->flags & GAD_PRESSED) {
    switch (event->type) {
    case ButtonPress:
      if (event->xbutton.button!=1) return 0;
      return 1;
    case ButtonRelease:
      if (event->xbutton.button!=1) return 0;
      gadget->flags&=~GAD_PRESSED;
      if (McInRectangle(event->xbutton.x, event->xbutton.y,
			0, 0, gadget->width, gadget->height)) {
	McSelectorSelect(gadget,
			 (event->xbutton.y-BW)/selector->height+
			  selector->first);
      }
      if (gadget->callbackUp) {
	(*gadget->callbackUp)(gadget);
      }
      return 1;
    case MotionNotify:
      if (McInRectangle(event->xbutton.x, event->xbutton.y,
			0, 0, gadget->width, gadget->height)) {
	if(McSelectorSelect(gadget,
			    (event->xbutton.y-BW)/selector->height+
			    selector->first)) {
	  if (gadget->callbackDown) {
	    (*gadget->callbackDown)(gadget);
	  }
	}
      }
      return 1;
    }
  } else {
    switch (event->type) {
    case ButtonPress:
      if (event->xbutton.button!=1) return 0;
      if (McInRectangle(event->xbutton.x, event->xbutton.y,
			0, 0, gadget->width, gadget->height)) {
	selection = (event->xbutton.y-BW)/selector->height+
	  selector->first;
	if (selection==selector->selection) {
	  diff=event->xbutton.time-selector->time;
	  if (diff>0 && diff<500) {
	    selector->flags|=SEL_DOUBLE_CLICK;
	  }
	}
	selector->time=event->xbutton.time;
	McSetFocus(gadget);
	McSelectorSelect(gadget, selection);
	if (gadget->callbackDown) {
	  XFlush(gadget->mcw->app->display);
	  (*gadget->callbackDown)(gadget);
	}
	if (selector->flags & SEL_DOUBLE_CLICK) {
	  selector->flags&=~SEL_DOUBLE_CLICK;
	  selector->time=0;
	} else {
	  gadget->flags|=GAD_PRESSED;
	  McMoveGadgetToStart(gadget);
	}
	return 1;
      }
    }
  }

  return 0;
}


/**************************************************************************/

static int McSelectorKey(McGadget *gadget, XKeyEvent *event) {
  McSelector *selector = (McSelector *)gadget->specialInfo;
  int height=SELECTORHEIGHT(gadget,selector);
  int result=0;
  int up=0; /* avoid flickering */
  McWindow *mcw=gadget->mcw;

  switch((int)mcw->keysym) {
  case XK_Linefeed:
  case XK_KP_Enter:
  case XK_Return:
    if (gadget->callbackDown) {
      selector->flags|=SEL_DOUBLE_CLICK;
      XFlush(mcw->app->display);
      (*gadget->callbackDown)(gadget);
      selector->flags&=~SEL_DOUBLE_CLICK;
      result=1;
    }
    break;

  case XK_End:
    up=selector->size+9;
    result=1;
    break;

  case XK_Home:
    if (selector->size>0) {
      up=10;
    }
    result=1;
    break;

  case XK_Up:
    if (selector->selection>0) {
      up=selector->selection+9;
    }
    result=1;
    break;

  case XK_Prior:
    if (selector->selection-(height-1)>=0) {
      up=selector->selection-height+11;
    } else {
      up=10;
    }
    result=1;
    break;

  case XK_Next:
    if (selector->selection+(height-1)<selector->size-1) {
      up=selector->selection+height+9;
    } else {
      up=selector->size+9;
    }
    result=1;
    break;

  case XK_Down:
    if (selector->selection<(selector->size-1)) {
      up=selector->selection+11;
    }
    result=1;
    break;
  }

  if (up) {
    McSelectorSelect(gadget,up-10);
    if (gadget->callbackDown) {
      XFlush(gadget->mcw->app->display);
      (*gadget->callbackDown)(gadget);
    }
  }

  return result;
}

/**************************************************************************/

/*
 * This stuff binds a selector with a slider.
 * The slider can be used to scroll the selector's contents and
 * the slider will be updated on every selector change
 */

static void slider_proc(McGadget *gadget);

void McSelectorBindSlider(McGadget *selgad, McGadget *slidergad) {
  McSelector *selector = (McSelector *)selgad->specialInfo;
  selector->slidergad = slidergad;
  if (!slidergad) return; /* Used to unbind */
  slidergad->callbackUp = slidergad->callbackDown = slider_proc;
  slidergad->customData = selgad;
}

static void slider_proc(McGadget *gadget) {
  McSlider *slider = (McSlider *)gadget->specialInfo;
  McGadget *selgad = (McGadget *)gadget->customData;
  McSelector *selector = (McSelector *)selgad->specialInfo;
  int first = (slider->maxValue-slider->leftValue);

  if (selector->first!=first) {
    int kick=0, fish;

    selector->first = (slider->maxValue-slider->leftValue);

    if (selector->selection<first) {
      selector->selection=first;
      kick=1;
    }

    if (selector->selection >=
	(first+(fish=SELECTORHEIGHT(selgad,selector)))) {
      selector->selection=first+fish-1;
      kick=1;
    }

    McSelectorUpdate(selgad, 0, 1);

    if (kick && selgad->callbackDown) {
      (*selgad->callbackDown)(selgad);
    }
  }
}

const unsigned char *McSelectorSelectionString(McGadget *gadget) {
  McSelector *selector = (McSelector *) gadget->specialInfo;
  if ((selector->data) && (selector->selection>=0))
    return selector->data[selector->selection];
  else
    return NULL;
}

int McSelectorSelection(McGadget *gadget) {
  McSelector *selector = (McSelector *) gadget->specialInfo;
  if ((selector->data) && (selector->selection>=0))
    return selector->selection;
  else
    return -1;
}

/****************************************************************************/

McGadget *MakeSelector(McWindow *mcw, int x, int y, int w, int h, int id,
		       XFontStruct *font,
		       void (*callbackUp)(struct McGadget *),
		       void (*callbackDown)(struct McGadget *)) {
  McGadget *gadget;

  if (x<0) x=mcw->w+x-w;
  if (y<0) x=mcw->h+y-h;

  gadget = McCreateGadget(mcw, GAD_H3D|GAD_3D|GAD_SELECTED|GAD_ACTIVE,
			  SELECTORGADGET, x, y, w, h);
  gadget->specialInfo = McCreateSelector(0, font, NULL, 0);
  gadget->callbackUp = callbackUp;
  gadget->callbackDown = callbackDown;
  gadget->id = id;
  return gadget;
}

