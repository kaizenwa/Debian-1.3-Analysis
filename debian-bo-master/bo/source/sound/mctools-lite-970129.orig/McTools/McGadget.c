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


#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "McApp.h"
#include "McGadget.h"
#include "McSlider.h"
#include "McDebug.h"
#include "McFocus.h"

static void GadgetUpdate(McGadget *gadget, int busy, int all);
static void GadgetBorderRedraw(McGadget *gadget);
static void set_state(McGadget *gadget);

McGadget *McCreateGadget(McWindow *mcw, unsigned int flags, unsigned int type,
			 int x, int y, int w, int h) {
  McGadget *gadget = (McGadget *) calloc(sizeof(McGadget), 1);
  McApp *app = mcw->app;
  XSetWindowAttributes attr;
  int attron;
  Window bwin=0;
  int WB;

  WB=0;
  if ((app->flags&MCAPP_GADGET_BORDER) && (!(flags&GAD_NOBORDER))) WB=1;

  /* Thou shalt not set GAD_BORDERONLY and GAD_NOBORDER together
   * or thy program shalt die horribly!
   */
  assert((flags&(GAD_BORDERONLY|GAD_NOBORDER))!=(GAD_BORDERONLY|GAD_NOBORDER));

  gadget->type = type;
  gadget->flags = flags | ((flags & GAD_SELECTED)?GAD_STATE:0);

  gadget->normalBitmap = gadget->selectBitmap = NULL;
  gadget->normalLabel = gadget->selectLabel = NULL;
  gadget->callbackDown = gadget->callbackUp = NULL;
  gadget->topLeftGravity = NorthWestGravity;
  gadget->bottomRightGravity = ForgetGravity;
  gadget->x = x;
  gadget->y = y;
  gadget->width = w;
  gadget->height = h;
  gadget->id = 0;
  gadget->mcw = mcw;
  gadget->customData = gadget->specialInfo = NULL;
  gadget->win=gadget->bwin=0;

  attron=CWBitGravity|CWBackPixel;
  attr.bit_gravity = NorthWestGravity;
  attr.background_pixel=app->colors[COL_BACKGROUND];

  if (!(flags&GAD_NOBORDER)) {
    int dbr=(flags&GAD_DEFBORDER)?1:0;
    if ((flags & GAD_HMASK) != GAD_HNONE) {
      bwin=gadget->bwin=XCreateWindow(app->display, mcw->clientwin,
				      x-BW-WB-1-dbr, y-BW-WB-1-dbr,
				      w+BW+BW+2+dbr+dbr, h+BW+BW+2+dbr+dbr,
				      WB, CopyFromParent, CopyFromParent,
				      CopyFromParent, attron, &attr);
      XSelectInput(app->display, gadget->bwin, ExposureMask);
      x=y=BW+1+dbr;
    } else {
      bwin=gadget->bwin=XCreateWindow(app->display, mcw->clientwin,
				      x-dbr-1, y-dbr-1,
				      w+dbr+dbr+2, h+dbr+dbr+2,
				      WB, CopyFromParent, CopyFromParent,
				      CopyFromParent, attron, &attr);
      XSelectInput(app->display, gadget->bwin, ExposureMask);
      x=y=dbr+1;
    }
  }

  if (!(flags & GAD_BORDERONLY)) {
    gadget->win=XCreateWindow(app->display, bwin?bwin:mcw->clientwin,
			      x, y, w, h, 0,
			      CopyFromParent, CopyFromParent, CopyFromParent,
			      attron, &attr);

    XSelectInput(app->display, gadget->win, MCGADGET_EVENT_MASK);

    if (gadget->bwin) XMapRaised(app->display, gadget->win);
  }
  McAddGadgetToList(mcw, gadget);
  return gadget;
}

int McMoveGadget(McGadget *gadget, int x, int y) {
  int WB=0;
  int changed=0;

  if ((gadget->mcw->app->flags&MCAPP_GADGET_BORDER) &&
      (!(gadget->flags&GAD_NOBORDER))) WB=1;

  if ((x>=0) && (x!=gadget->x)) { gadget->x=x; changed=1; }
  if ((y>=0) && (y!=gadget->y)) { gadget->y=y; changed=1; }

  if (gadget->bwin) {
    int ofs;
    int dbr=(gadget->flags&GAD_DEFBORDER)?1:0;
    if ((gadget->flags & GAD_HMASK)==GAD_HNONE)
      ofs=dbr;
    else
      ofs=BW+WB+dbr+dbr;

    XMoveWindow(gadget->mcw->app->display, gadget->bwin,
		gadget->x-ofs, gadget->y-ofs);
  } else {
    XMoveWindow(gadget->mcw->app->display, gadget->win,
		gadget->x, gadget->y);
  }

  return changed;
}

int McResizeGadget(McGadget *gadget, int w, int h) {
  int changed=0;
  if ((w>0) && (w!=gadget->width)) { gadget->width=w; changed=1; }
  if ((h>0) && (h!=gadget->height)) { gadget->height=h; changed=1; }

  if (gadget->bwin) {
    int ofs;
    int dbr=(gadget->flags&GAD_DEFBORDER)?1:0;
    if ((gadget->flags & GAD_HMASK)==GAD_HNONE)
      ofs=dbr+dbr;
    else
      ofs=BW+BW+2+dbr+dbr;
    XResizeWindow(gadget->mcw->app->display, gadget->bwin,
		  gadget->width+ofs, gadget->height+ofs);
  }

  if (gadget->win) {
    XResizeWindow(gadget->mcw->app->display, gadget->win,
		  gadget->width, gadget->height);
  }

  return changed;
}

void McInitGadgets(McWindow *mcw) {
  XMapSubwindows(mcw->app->display, mcw->clientwin);
}

static void GadgetCheckClear(McGadget *gadget) {
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;

  if (gadget->flags & GAD_INVIS) return;
  if ((gadget->flags & GAD_STATE) &&
      ((gadget->flags & GAD_HMASK) != GAD_HNONE)) {  /* Highlighted */
    if ((!(gadget->flags & GAD_NOFILL)) && (gadget->win))
      XFillRectangle(app->display, gadget->win, app->gc[GC_SELECTED],
		     0, 0, gadget->width, gadget->height);
  } else {
    if ((!(gadget->flags & GAD_NOFILL)) && (gadget->win))
      XFillRectangle(mcw->app->display, gadget->win, mcw->app->gc[GC_CLEAR],
		     0, 0, gadget->width, gadget->height);
  }
}


static void GadgetBorderRedraw(McGadget *gadget) {
  McWindow *mcw=gadget->mcw;
  int dbr;

  set_state(gadget);

  if (gadget->flags & GAD_INVIS) return;

  if (!gadget->bwin) return;

#if 0 /* DEBUG: Fill background with a screaming color */
  XSetForeground(mcw->app->display, mcw->app->gc[GC_NORMAL],
		 mcw->app->colors[COL_MENU]);
  XFillRectangle(mcw->app->display, gadget->bwin,
		 mcw->app->gc[GC_NORMAL], 0, 0, -1, -1);
  XSetForeground(mcw->app->display, mcw->app->gc[GC_NORMAL],
		 mcw->app->colors[COL_FOREGROUND]);
#endif

  if ((gadget->flags & GAD_HMASK) == GAD_HNONE) return;

  dbr=(gadget->flags&GAD_DEFBORDER)?1:0;

  if (gadget->flags & GAD_STATE) {  /* Highlighted */
    if (gadget->flags & GAD_3D)
      McAppDrawbox(mcw, gadget->bwin,
		   BW+dbr, BW+dbr,
		   gadget->width+2, gadget->height+2, _3D_IN);
  } else { /* Normal */
    if (gadget->flags & GAD_3D)
      McAppDrawbox(mcw, gadget->bwin,
		   BW+dbr, BW+dbr,
		   gadget->width+2, gadget->height+2, _3D_OUT);
  }
}

static void GadgetRedraw(McGadget *gadget, int busy) {
  McWindow *mcw=gadget->mcw;

  if (gadget->flags & GAD_NOSTIPPLE) busy=0;
  GadgetBorderRedraw(gadget);
  GadgetCheckClear(gadget);
  if (gadget->win) {
    GadgetUpdate(gadget, busy, 1);
    if (busy || (gadget->flags & GAD_DISABLE)) {
      XFillRectangle(mcw->app->display, gadget->win, mcw->app->gc[GC_BUSY],
		     0, 0, gadget->width, gadget->height);
    }
  }
}

static void GadgetUpdate(McGadget *gadget, int busy, int all) {
  McText *txt;
  McBitmap *bmp;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;

  if (gadget->flags & GAD_NOSTIPPLE) busy=0;

  if (gadget->flags & GAD_INVIS) return;
  if (gadget->flags & GAD_STATE) {
    if (!(txt=gadget->selectLabel)) txt=gadget->normalLabel;
    if (!(bmp=gadget->selectBitmap)) bmp=gadget->normalBitmap;
  } else {
    txt=gadget->normalLabel;
    bmp=gadget->normalBitmap;
  }
      
  if ((gadget->flags & GAD_STATE) &&
      ((gadget->flags & GAD_HMASK) != GAD_HNONE)) {
    /* Highlighted */
    if (gadget->specialInfo && gadget->specialInfo->updateProc)
      (*(gadget->specialInfo->updateProc))(gadget, busy, all);
    if (bmp)
      McPutBitmap(mcw, gadget->win, bmp, app->gc[GC_SELECTED_BITMAP], 0, 0);
    if (txt)
      McWriteText(mcw, gadget->win, txt, app->gc[GC_SELECTED_BITMAP], 0, 0);

  } else {
    if (gadget->specialInfo && gadget->specialInfo->updateProc)
      (*(gadget->specialInfo->updateProc))(gadget, busy, all);

    if (bmp)
      McPutBitmap(mcw, gadget->win, bmp, app->gc[GC_NORMAL], 0, 0);
    if (txt)
      McWriteText(mcw, gadget->win, txt, app->gc[GC_NORMAL], 0, 0);
  }

  if (!(gadget->specialInfo && gadget->specialInfo->updateProc)) {
    McDrawFocusMark(gadget,
		    (gadget->flags&GAD_SELECTED)?COL_SELECTED:COL_BACKGROUND);
  }
}

void McDrawFocusMark(McGadget *gadget, int background) {
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  XGCValues values;
  GC gc;
  int ofs,dbr;
  int defbtn;

  if (!gadget->bwin) return;

  dbr=(gadget->flags&GAD_DEFBORDER)?1:0;
  if ((gadget->flags&GAD_HMASK)==GAD_HNONE) ofs=0; else ofs=BW+dbr;

  if (background>=0) {
    if (gadget==mcw->keyboardFocus) {
      values.line_style=LineDoubleDash;
      values.foreground=app->colors[COL_FOCUS];
      values.background=app->colors[background];
      values.dashes=2;
      XChangeGC(app->display, app->gc[GC_NORMAL],
		GCForeground|GCBackground|GCLineStyle|GCDashList, &values);
    } else {
      values.foreground=app->colors[background];
      XChangeGC(app->display, app->gc[GC_NORMAL], GCForeground, &values);
    }
    XDrawRectangle(app->display, gadget->bwin, app->gc[GC_NORMAL], ofs, ofs,
		   gadget->width+1, gadget->height+1);
    values.background=app->colors[COL_BACKGROUND];
    values.foreground=app->colors[COL_FOREGROUND];
    values.line_style=LineSolid;
    XChangeGC(app->display, app->gc[GC_NORMAL],
	      GCForeground|GCBackground|GCLineStyle, &values);
  }

  if ((gadget->flags&GAD_HMASK)==GAD_HNONE) return;

  defbtn=((gadget==mcw->mainButton) && (gadget!=mcw->keyboardFocus) &&
	  ((!mcw->keyboardFocus) || (mcw->keyboardFocus->type!=BOOLGADGET)));

  if (defbtn) {
    /* Check if someone made it the default button without 
     * setting GAD_DEFBORDER. If so, enlarge the border window
     */
    if (!(gadget->flags&GAD_DEFBORDER)) {
      gadget->flags|=GAD_DEFBORDER;
      XResizeWindow(gadget->mcw->app->display, gadget->bwin,
		    gadget->width+ofs+ofs+4, gadget->height+ofs+ofs+4);
      XMoveWindow(gadget->mcw->app->display, gadget->bwin,
		  gadget->x-ofs-2, gadget->y-ofs-2);
      if (gadget->win)
	XMoveWindow(gadget->mcw->app->display, gadget->win, ofs+2, ofs+2);
    }
    gc=app->gc[GC_NORMAL];
  } else {
    if (gadget->flags&GAD_DEFBORDER) gc=app->gc[GC_CLEAR]; else gc=0;
  }

  if (gc)
    XDrawRectangle(app->display, gadget->bwin, gc, 0, 0,
		   gadget->width+BW+BW+3, gadget->height+BW+BW+3);
}

void McSetMainButton(McGadget *gadget) {
  McGadget *old;

  old=gadget->mcw->mainButton;

  if (gadget==old) return;

  gadget->mcw->mainButton=gadget;

  if (old) McDrawFocusMark(old, -1);
  if (gadget) McDrawFocusMark(gadget, -1);
}


static void set_state(McGadget *gadget) {
  if (gadget->flags & GAD_SELECTED)
    gadget->flags|=GAD_STATE;
  else
    gadget->flags&=~GAD_STATE;
}

void McGadgetRedraw(McGadget *gadget) {
  set_state(gadget);
  GadgetRedraw(gadget, 0);
}

void McGadgetBorderRedraw(McGadget *gadget, int busy) {
  McWindow *mcw=gadget->mcw;
  if (gadget->flags & GAD_NOSTIPPLE) busy=0;
  GadgetBorderRedraw(gadget);
  if ((busy || (gadget->flags & GAD_DISABLE)) && (gadget->win)) {
    XFillRectangle(mcw->app->display, gadget->win, mcw->app->gc[GC_BUSY],
		   0, 0, gadget->width, gadget->height);
  }
}

void McGadgetUpdate(McGadget *gadget) {
  set_state(gadget);
  GadgetUpdate(gadget, 0, 0);
}

void McGadgetBusy(McGadget *gadget) {
  set_state(gadget);
  GadgetRedraw(gadget, 1);
}

void McPressGadget(McGadget *gadget) {
  unsigned long excl;
  McWindow *mcw=gadget->mcw;

  gadget->flags|=GAD_PRESSED;
  if (!(gadget->flags&GAD_NOTOGGLE)) {
    if ((excl=gadget->mutualExclude)) {
      McGadget *next;
      gadget->flags|=GAD_SELECTED;
      next = mcw->firstGadget;
      while(next) {
	if ((next!=gadget) && (next->mutualExclude & excl)) {
	  if (next->flags&GAD_SELECTED) {
	    next->flags^=GAD_SELECTED;
	    set_state(next);
	    GadgetRedraw(next, 0);
	  }
	}
	next=next->next;
      }
    } else {
      gadget->flags^=GAD_SELECTED;
    }
  }
  if (gadget->callbackDown) {
    set_state(gadget);
    GadgetRedraw(gadget, 1);
    XFlush(mcw->app->display);
    (*gadget->callbackDown)(gadget);
  }
  set_state(gadget);
  GadgetRedraw(gadget, 0);
  McMoveGadgetToStart(gadget);
}

void McReleaseGadget(McGadget *gadget, int inside) {
  McWindow *mcw=gadget->mcw;
  gadget->flags&=~GAD_PRESSED;
  if (inside || (gadget->flags & GAD_NORELVERIFY)) {
    if ((!gadget->mutualExclude) &&
	(!(gadget->flags&GAD_TOGGLE)) && (!(gadget->flags&GAD_NOTOGGLE))) {
      gadget->flags^=GAD_SELECTED;
    }
    if (gadget->callbackUp) {
      set_state(gadget);
      GadgetRedraw(gadget, 1);
      XFlush(mcw->app->display);
      (*gadget->callbackUp)(gadget);
    }
  } else {
    if (!(gadget->flags&GAD_NOTOGGLE)) gadget->flags^=GAD_SELECTED;
  }

  set_state(gadget);
  GadgetRedraw(gadget, 0);
}

void McGadgetUpdateClicks(McGadget *gadget, int tme) {
  int diff=tme-gadget->time;
  if (diff>0 && diff<250) {
    gadget->clicks++;
  } else {
    gadget->clicks=1;
  }
  gadget->time=tme;
}

int McGadgetEvent(McGadget *gadget, XEvent *event) {
  int in;

  if (!(gadget->flags&GAD_ACTIVE)) return 0;

  if (gadget->specialInfo && gadget->specialInfo->eventProc)
    return (*(gadget->specialInfo->eventProc))(gadget, event);

  if (event->type==Expose) return 0;

  /* Standard bool gadget */
  if (gadget->flags&GAD_PRESSED) {
    switch (event->type) {
    case ButtonRelease:
      if (event->xbutton.button==1) {
	McReleaseGadget(gadget,
			McInRectangle(event->xbutton.x, event->xbutton.y,
				      0, 0, gadget->width, gadget->height));
	return 1;
      }
      return 0;
    case MotionNotify:
      if (gadget->flags & GAD_NORELVERIFY) return 1;
      in = McInRectangle(event->xmotion.x, event->xmotion.y,
			 0, 0, gadget->width, gadget->height);
      
      if ((in && (gadget->flags&GAD_SELECTED)) ||
	  ((!in) && (!(gadget->flags&GAD_SELECTED)))) {
	if (!(gadget->flags&GAD_STATE)) {
	  gadget->flags|=GAD_STATE;
	  GadgetRedraw(gadget, 0);
	}
      } else {
	if (gadget->flags&GAD_STATE) {
	  gadget->flags&=~GAD_STATE;
	  GadgetRedraw(gadget, 0);
	}
      }
      return 1;
    case ButtonPress:
      if (event->xbutton.button==1) return 1; else return 0;
    default:
      return 0;
    }
  } else { /* not currently pressed */
    switch (event->type) {
    case ButtonPress:
      if (event->xbutton.button==1) {
	if (McInRectangle(event->xbutton.x, event->xbutton.y,
			  0, 0, gadget->width, gadget->height)) {
	  McGadgetUpdateClicks(gadget, event->xbutton.time);
	  McSetFocus(gadget);
	  McPressGadget(gadget);
	  return 1;
	}
	return 0;
      } else {
	return 0;
      }
    default:
      return 0;
    }
    }
  }

#ifdef DEBUG_CODE
void McGadgetInfo(McGadget *gadget) {
  fprintf(stderr,
	  "        x:%d\n"
	  "        y:%d\n"
	  "    width:%d\n"
	  "   height:%d\n"
	  "    flags:$%04x\n"
	  "     type:%d\n",
	  gadget->x, gadget->y, gadget->width, gadget->height,
	  gadget->flags, gadget->type);
}
#endif

void McFreeGadget(McGadget *gadget) {
  McWindow *mcw=gadget->mcw;

  McRemoveFocusGadget(gadget);

  if (mcw->keyboardFocus==gadget) mcw->keyboardFocus=gadget->focusNext;
  if (mcw->selectionOwner==gadget) mcw->selectionOwner=NULL;

  McWipeGadget(gadget);

  if (gadget->normalBitmap) McFreeBitmap(mcw->app, gadget->normalBitmap);
  if (gadget->selectBitmap) McFreeBitmap(mcw->app, gadget->selectBitmap);
  if (gadget->normalLabel) McFreeText(gadget->normalLabel);
  if (gadget->selectLabel) McFreeText(gadget->selectLabel);

  if (gadget->specialInfo) {
    if (gadget->specialInfo->cleanupProc)
      (*gadget->specialInfo->cleanupProc)(gadget);
    else
      free(gadget->specialInfo);
  }
  McRemoveGadgetFromList(gadget);
  if (gadget->region) XDestroyRegion(gadget->region);
  if (gadget->win) XDestroyWindow(gadget->mcw->app->display, gadget->win);
  if (gadget->bwin) XDestroyWindow(gadget->mcw->app->display, gadget->bwin);
  free(gadget);
}

/****************************************************************************/

void McWipeGadget(McGadget *gadget) {
  McWindow *mcw=gadget->mcw;
  if (gadget->flags & (GAD_3D | GAD_H3D)) {
    XClearArea(mcw->app->display, mcw->clientwin,
	       gadget->x-BW, gadget->y-BW,
	       gadget->width+BW+BW, gadget->height+BW+BW, True);
  }
  if (gadget->win) XClearWindow(mcw->app->display, gadget->win);
}

/****************************************************************************/

int McGadgetKeyPress(McGadget *gadget, XKeyEvent *event) {
  if ((gadget->specialInfo) && (gadget->specialInfo->keyboardProc))
    return (*(gadget->specialInfo->keyboardProc))(gadget, event);

  switch(gadget->mcw->keysym) {
  case XK_space:
    if (gadget!=gadget->mcw->keyboardFocus) break;
  case XK_Linefeed:
  case XK_KP_Enter:
  case XK_Return:
    McPressGadget(gadget);
    McReleaseGadget(gadget, 1);
    return 1;
  }
  return 0;
}
