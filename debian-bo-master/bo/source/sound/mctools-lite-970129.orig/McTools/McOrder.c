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
#include "McOrder.h"
#include "McDigits.h"

static void McOrderUpdate(McGadget *gadget, int busy, int all);
static int McOrderEvent(McGadget *gadget, XEvent *event);
static void McOrderCleanup(McGadget *gadget);
static void UpdateScrollers(McGadget *gadget);
static void McDrawOrderItem(McWindow *mcw, Window win,
			    int id, int x, int y, int iw, int ih, int out);
static void ValidateOffset(McGadget *gadget, int *all);
static const string_t McOrderTip(McGadget *gadget, int *x, int *y);
static int WhichItem(McGadget *gadget, int x, int y, McOrderItem **which);

McSpecialInfo *McCreateOrder(McWindow *mcw, unsigned int flags) {
  McOrder *order = (McOrder *) calloc(sizeof(McOrder), 1);
  order->flags = flags;
  order->specialInfo.updateProc = McOrderUpdate;
  order->specialInfo.eventProc = McOrderEvent;
  order->specialInfo.cleanupProc = McOrderCleanup;
  order->specialInfo.tipProc = NULL;
  return (McSpecialInfo *)order;
}

McOrderItem *McCreateOrderItem(McOrderItem *prev) {
  McOrderItem *it = (McOrderItem *) calloc(sizeof(McOrderItem), 1);
  if (prev) prev->next=it;
  it->next=NULL;
  it->prev=prev;
  return it;
}

static void McOrderCleanup(McGadget *gadget) {
  McOrder *order = (McOrder *)gadget->specialInfo;
  McOrderItem *next, *it   = order->first;
  while(it) {
    next=it->next;
    free(it);
    it=next;
  }
  free(order);
}

static const string_t McOrderTip(McGadget *gadget, int *x, int *y) {
  McOrderItem *it;

  WhichItem(gadget, *x, *y, &it);

  if (it) {
    *y=gadget->height-BW;
    return it->tip;
  }

  return NULL;
}

static void McOrderUpdate(McGadget *gadget, int busy, int all) {
  McWindow *mcw=gadget->mcw;
  McOrderItem *it   = ORDER(gadget)->first;
  McOrderItem *sel  = ORDER(gadget)->grip;
  McOrderItem *last = ORDER(gadget)->lastgrip;
  int x=BW+BW;
  int y=BW+BW;
  int iw = ORDER(gadget)->ItemWidth;
  int ih = ORDER(gadget)->ItemHeight;
  int xm = gadget->width-BW-BW-iw;
  int ym = gadget->height-BW-BW-ih;
  int numb=(ORDER(gadget)->flags & ORD_PRINTID);
  int i;
  McApp *app=mcw->app;

  ValidateOffset(gadget, &all);
  
  if (all) {
    McOrderItem *tipit = ORDER(gadget)->first;
    gadget->specialInfo->tipProc = NULL;
    while(tipit) { 
      if (tipit->tip) {
	gadget->specialInfo->tipProc = McOrderTip;
	break;
      }
      tipit=tipit->next;
    }
  }

  if (all==2) McGadgetBorderRedraw(gadget, busy);

  if (ORDER(gadget)->flags & ORD_SCROLLIES) {
    if (all) {
      GC gc1, gc2;
      int xx=x+iw+(BW<<1);
      int ya=y-(BW<<1);
      int ye=ya+ih+(BW<<2)-1;
      if (gadget->flags & GAD_SELECTED) {
	gc1=app->gc[GC_BRIGHT]; gc2=app->gc[GC_DARK];
      } else {
	gc1=app->gc[GC_DARK]; gc2=app->gc[GC_BRIGHT];
      }

      XDrawLine(app->display, gadget->win, gc1, xx  , ya, xx  , ye);
      XDrawLine(app->display, gadget->win, gc2, xx+1, ya, xx+1, ye);
      xx=xm-iw+(BW<<1);
      XDrawLine(app->display, gadget->win, gc1, xx  , ya, xx  , ye);
      XDrawLine(app->display, gadget->win, gc2, xx+1, ya, xx+1, ye);
      UpdateScrollers(gadget);
    }
    x+=(i=(iw + 5 * BW));
    xm-=iw<<1;
  }

  ORDER(gadget)->lastgrip=sel;

  for (i=ORDER(gadget)->offset; (i>0) && (it); i--) it=it->next;

  while(it) {
    if (it==sel) {
      ORDER(gadget)->xgrip = x;
      ORDER(gadget)->ygrip = y;
      McDrawOrderItem(mcw, gadget->win, numb?it->id:-1, x, y, iw, ih, 0);
    } else {
      if ((it==last) || all) {
	McDrawOrderItem(mcw, gadget->win, numb?it->id:-1, x, y, iw, ih, 1);
      }
    }
    it=it->next;
    x=x+iw+BW+BW+BW;
    if (x>xm) {
      x=BW+BW+BW;
      y=y+ih+BW+BW+BW;
      if (y>ym) break;
    }
  }

  if ((x>iw) && (x<gadget->width)) {
    XFillRectangle(app->display, gadget->win, app->gc[GC_SELECTED],
		   x-BW, 0, gadget->width-x+BW, gadget->height);
  }
  McDrawFocusMark(gadget, COL_SELECTED);
}

static void McDrawOrderItem(McWindow *mcw, Window win,
			    int id, int x, int y, int iw, int ih, int out) {
  McApp *app=mcw->app;

  if (out) {
    McAppDrawbox(mcw, win, x, y, iw, ih, _3D_OUT);
    XFillRectangle(app->display, win, app->gc[GC_CLEAR], x, y, iw, ih);
    if (id>=0)
      McPutNumber(mcw, win, app->gc[GC_SET_NORMAL], id, x, y+1);
  } else {
    McAppDrawbox(mcw, win, x, y, iw, ih, _3D_IN);
    XFillRectangle(app->display, win, app->gc[GC_SELECTED],
		   x, y, iw, ih);
    if (id>=0) McPutNumber(mcw, win,
			   app->gc[GC_SET_SELECTED_BITMAP], id, x, y+1);
  }
}


static void UpdateScrollers(McGadget *gadget) {
  McWindow *mcw=gadget->mcw;
  int iw = ORDER(gadget)->ItemWidth;
  int ih = ORDER(gadget)->ItemHeight;
  int y=BW+BW;

  McDrawOrderItem(mcw, gadget->win, DIG_LEFT_FILLED_ARROW,
		  BW+BW, y, iw, ih,
		  !(ORDER(gadget)->flags&ORD_LEFTDOWN));

  McDrawOrderItem(mcw, gadget->win, DIG_RIGHT_FILLED_ARROW,
		  gadget->width-BW-BW-iw, y, iw, ih,
		  !(ORDER(gadget)->flags&ORD_RIGHTDOWN));
}


static int McOrderLocate(McWindow *mcw, McGadget *gadget) {
  McOrderItem *it = ORDER(gadget)->first;
  McOrderItem *sel = ORDER(gadget)->grip;
  int x  = BW+BW;
  int y  = BW+BW;
  int iw = ORDER(gadget)->ItemWidth;
  int ih = ORDER(gadget)->ItemHeight;
  int xm = gadget->width-BW-BW-iw;
  int ym = gadget->height-BW-BW-ih;
  int i;

  if (ORDER(gadget)->flags & ORD_SCROLLIES) {
    x+=(i=(iw + 5 * BW));
    xm-=iw;
  }

  for (i=ORDER(gadget)->offset; (i>0) && (it); i--) it=it->next;

  while(it) {
    if (it==sel) {
      ORDER(gadget)->xgrip = x;
      ORDER(gadget)->ygrip = y;
      return 1;
    }
    it=it->next;
    x=x+iw+BW+BW+BW;
    if (x>xm) {
      x=BW+BW+BW;
      y=y+ih+BW+BW+BW;
      if (y>ym) break;
    }
  }
  ORDER(gadget)->xgrip = -1;
  ORDER(gadget)->ygrip = -1;
  return 0;
}

static int InScroller(McGadget *gadget, int x, int y) {
  int iw,ih,xl,yl;

  if (ORDER(gadget)->flags & ORD_SCROLLIES) {
    iw = ORDER(gadget)->ItemWidth + (BW<<1);
    ih = ORDER(gadget)->ItemHeight + (BW<<1);
    xl =BW;
    yl =BW;
    if ((x>=xl) && (x<(xl+iw)) && (y>=yl) && (y<(yl+ih))) return ORD_LEFTDOWN;
    xl = gadget->width-BW-BW-iw;
    if ((x>=xl) && (x<(xl+iw)) && (y>=yl) && (y<(yl+ih))) return ORD_RIGHTDOWN;
  }
  return 0;
}

static int WhichItem(McGadget *gadget, int x, int y, McOrderItem **which) {
  McOrderItem *it = ORDER(gadget)->first;
  int lx = BW;
  int ly = BW;
  int iw = ORDER(gadget)->ItemWidth+BW+BW+BW;
  int ih = ORDER(gadget)->ItemHeight+BW+BW+BW;
  int xm = gadget->width-BW-BW-BW-BW;
  int ym = gadget->height-BW-BW;
  int i;

  if (ORDER(gadget)->flags & ORD_SCROLLIES) {
    lx+=(i=(iw + 2 * BW));
    xm-=iw<<1;
  }

  /* Chapter 1: Fun things to do with C ... */
  if ((it) && ((x=x-lx)>=0) && ((y=y-ly)>=0) &&
      (x<xm) && (y<ym) && (((x%iw)+BW)<iw) && (((y%ih)+BW)<ih)) {
    x/=iw;
    y/=ih;
    if (y) x+=y*((xm/iw)+1);
    for (i=ORDER(gadget)->offset; (i>0) && (it); i--) it=it->next;
    while (it && x) { it=it->next; --x; }
    if (it) x=0; else x=1;
  } else {
    it=NULL;
    x=-1;
  }
#ifdef DEBUG_ORDER
  printf("item %d\n",it?it->id:-1);
#endif
  if (which) *which=it;

  return x;
}

static int InItem(McGadget *gadget, int x, int y) {
  McOrderItem *it;
  int res;
  res=WhichItem(gadget, x, y, &it);
  ORDER(gadget)->selection=it;
  return res;
}

static void RedrawHandle(McGadget *gadget) {
  int iw, ih;
  Window win;
  McWindow *mcw=gadget->mcw;

  if ((win = ORDER(gadget)->handle)) {
    iw = ORDER(gadget)->ItemWidth;
    ih = ORDER(gadget)->ItemHeight;
    McDrawOrderItem(mcw, win,
		    (ORDER(gadget)->flags&ORD_PRINTID)?
		    (ORDER(gadget)->grip->id):-1,
		    BW, BW, iw, ih, 1);
  }
}

static void MakeOrderHandle(McGadget *gadget, int cx, int cy) {
  XSetWindowAttributes attr;
  int x,y;
  Window win;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  int iw = ORDER(gadget)->ItemWidth+BW+BW;
  int ih = ORDER(gadget)->ItemHeight+BW+BW;

  if (!McOrderLocate(mcw,gadget)) {
    ORDER(gadget)->handle=0;
    return;
  }
  x=ORDER(gadget)->xgrip;
  y=ORDER(gadget)->ygrip;

  attr.save_under=True;
  attr.background_pixel=app->colors[COL_BACKGROUND];
  win=XCreateWindow(app->display, mcw->framewin,
		    x-BW+gadget->x, y-BW+gadget->y, iw, ih, 0,
		    CopyFromParent, CopyFromParent, CopyFromParent,
		    CWBackPixel|CWSaveUnder, &attr);

  XSelectInput(app->display, win, ExposureMask);

  XMapRaised(app->display,win);

  ORDER(gadget)->handle=win;
}

static int McOrderEvent(McGadget *gadget, XEvent *event) {
  int res, id;
  McOrderItem *it1,*it2,*it3,*it;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;

  if (event->type == Expose) {
    if ((ORDER(gadget)->handle) &&
	(event->xexpose.window==ORDER(gadget)->handle)) {
      RedrawHandle(gadget);
      return 1;
    }
    return 0;
  }

  if (gadget->flags&GAD_PRESSED) {
    switch (event->type) {
    case ButtonRelease:
      if ((event->xbutton.button!=1) && (event->xbutton.button!=2)) return 0;
      gadget->flags&=~GAD_PRESSED;
      if (ORDER(gadget)->flags & (ORD_LEFTDOWN|ORD_RIGHTDOWN)) {
	if ((res=InScroller(gadget, event->xbutton.x,event->xbutton.y))==
	    (ORDER(gadget)->flags & (ORD_LEFTDOWN|ORD_RIGHTDOWN))) {
	  if (res&ORD_RIGHTDOWN) {
	    ORDER(gadget)->offset++;
	  } else {
	    if (res&ORD_LEFTDOWN) {
	      ORDER(gadget)->offset--;
	      if (ORDER(gadget)->offset<0) ORDER(gadget)->offset = 0;
	    }
	  }
	}
	ORDER(gadget)->flags &= ~(ORD_LEFTDOWN|ORD_RIGHTDOWN);
	McOrderUpdate(gadget, 0, 1);
      } else {
	if ((InItem(gadget, event->xbutton.x,event->xbutton.y))==0) {
	  if (!ORDER(gadget)->handle) {
	    if (event->xbutton.button==1) {
	      if (gadget->callbackDown) {
		(*gadget->callbackDown)(gadget);
	      }
	    } else {
	      if ((id=((it2=ORDER(gadget)->grip)->id))<DIG_START) {
		/* Delete an item, but be sure to leave */
		/* at least one item in the list.       */
		for (it=ORDER(gadget)->first; it; it=it->next) {
		  if ((it!=it2) && (id==it->id)) {
		    if (it2->prev) {
		      it2->prev->next=it2->next;
		    } else {
		      ORDER(gadget)->first=it2->next;
		    }
		    if (it2->next)
		      it2->next->prev=it2->prev;
		    McOrderUpdate(gadget, 0, 2);
		    it2=NULL;
		    break;
		  }
		}
		if (it2) XBell(app->display, 0);
	      } else {
		/* Keep markers there... */
		XBell(app->display, 0);
	      }
	    }
	  } else {
	    it1 = ORDER(gadget)->selection;  /* destination */
	    it2 = ORDER(gadget)->grip;       /* source      */
	    it=ORDER(gadget)->first;
	     /* Chapter 2: More fun things to do with C */
	    if ((it1!=it2) || (event->xbutton.button==2)) {
	      res=0;
	      while(it) {
		if (it==it1) { res=1; break; }
		if (it==it2) { res=2; break; }
		it=it->next;
	      }
	      if (res) {
		if (event->xbutton.button!=2) {
		  /* Remove grip from list */
		  if (it2->prev) {
		    it2->prev->next=it2->next;
		  } else {
		    ORDER(gadget)->first=it2->next;
		  }
		  if (it2->next)
		    it2->next->prev=it2->prev;
		} else {
		  /* Duplicate grip */
		  if (it2->id >= DIG_START) {
		    XBell(app->display, 0);
		    goto nonsense;			/* goto, ARGH! */
		  }
		  it3 = McCreateOrderItem(NULL);
		  memcpy(it3, it2, sizeof(McOrderItem));
		  it3->next = it3->prev = NULL;
		  it2 = it3;
		}
		if (res==2) {
		  /* insert grip after selection */
		  it2->prev=it1;
		  if ((it2->next=it1->next)) {
		    it1->next->prev=it2;
		  }
		  it1->next=it2;
		} else {
		  /* insert grip before selection */
		  it2->next=it1;
		  if ((it2->prev=it1->prev)) {
		    it1->prev->next=it2;
		  } else {
		    ORDER(gadget)->first=it2;
		  }
		  it1->prev=it2;
		}
		if ((gadget->callbackUp)) {
		  (*gadget->callbackUp)(gadget);
		}
	      }
	    }
nonsense:
	    XDestroyWindow(app->display, ORDER(gadget)->handle);
	    ORDER(gadget)->handle=0;
	    ORDER(gadget)->selection=ORDER(gadget)->grip=NULL;
	    McOrderUpdate(gadget, 0, 1);
	    return 1;
	  }
	}
	if (ORDER(gadget)->handle) {
	  XDestroyWindow(app->display, ORDER(gadget)->handle);
	  ORDER(gadget)->handle=0;
	}
      }
      ORDER(gadget)->selection=ORDER(gadget)->grip=NULL;
      McGadgetUpdate(gadget);
      return 1;

    case MotionNotify:
      if (!(ORDER(gadget)->flags & (ORD_LEFTDOWN|ORD_RIGHTDOWN))) {
	InItem(gadget, event->xmotion.x,event->xmotion.y);
	if ((!ORDER(gadget)->handle) &&
	    (ORDER(gadget)->grip!=ORDER(gadget)->selection)) {
	  MakeOrderHandle(gadget, event->xmotion.x,
			  event->xmotion.y);
	  return 1;
	}
	if (ORDER(gadget)->handle) {
	  XMoveWindow(app->display,ORDER(gadget)->handle,
		      event->xmotion.x-(ORDER(gadget)->ItemWidth)+gadget->x,
		      event->xmotion.y-(ORDER(gadget)->ItemHeight)+gadget->y);
	}
      }
      return 1;
    case ButtonPress:
      if (event->xbutton.button!=1) return 0;
      if (!(ORDER(gadget)->flags & (ORD_LEFTDOWN|ORD_RIGHTDOWN))) {
	InItem(gadget, event->xbutton.x,event->xbutton.y);
      }
      return 1;
    default:
      return 0;
    }
  } else { /* not currently pressed */
    switch (event->type) {
    case ButtonPress:
      if ((event->xbutton.button!=1) && (event->xbutton.button!=2)) return 0;
      if (McInRectangle(event->xbutton.x, event->xbutton.y,
			0, 0, gadget->width, gadget->height)) {
	gadget->flags|=GAD_PRESSED;
	McMoveGadgetToStart(gadget);
	if ((res=InScroller(gadget, event->xbutton.x,event->xbutton.y))) {
	  ORDER(gadget)->flags |= res;
	  UpdateScrollers(gadget);
	} else {
	  InItem(gadget, event->xbutton.x,event->xbutton.y);
	  ORDER(gadget)->grip=ORDER(gadget)->selection;
	}
	McGadgetUpdate(gadget);
	return 1;
      } else {
	return 0;
      }
    default:
      return 0;
    }
  }
}

static void ValidateOffset(McGadget *gadget, int *all) {
  McOrderItem *it;
  int i, max;

  if (ORDER(gadget)->flags & ORD_SCROLL) {
    max=(gadget->width-11*BW-((ORDER(gadget)->ItemWidth)<<1)) /
      (ORDER(gadget)->ItemWidth+3*BW);
    it=ORDER(gadget)->first;
    for (i=0; it; i++) it=it->next;
    if (i<=(max+2)) {
      if (ORDER(gadget)->flags & ORD_SCROLLIES) *all=2;
      ORDER(gadget)->flags &= ~ORD_SCROLLIES;
      ORDER(gadget)->offset = 0;
    } else {
      i-=max;
      if (i<0) i=0;
      if (ORDER(gadget)->offset>i) ORDER(gadget)->offset = i;
      if (ORDER(gadget)->offset<0) ORDER(gadget)->offset = 0;
      if (!(ORDER(gadget)->flags & ORD_SCROLLIES)) *all=2;
      ORDER(gadget)->flags |= ORD_SCROLLIES;
    }
  }
}
