/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

  McMenu.c

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

#include "../config.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "X11/cursorfont.h"

#include "McApp.h"
#include "McGadget.h"
#include "McSlider.h"
#include "McText.h"
#include "McBitmap.h"
#include "McString.h"
#include "McSelector.h"
#include "McResource.h"
#include "McMenu.h"
#include "bitmaps/hook.xbm"
#ifdef USE_META
#include "bitmaps/meta.xbm"
#define hotkey_width  meta_width
#define hotkey_height meta_height
#define hotkey_bits   meta_bits
#else
#include "bitmaps/alt.xbm"
#define hotkey_width  alt_width
#define hotkey_height alt_height
#define hotkey_bits   alt_bits
#endif


static McSpecialInfo *McCreateMenuGadget(McMenu *mnu);
static void McMenuUpdate(McGadget *gadget, int busy, int all);
static int McMenuEvent(McGadget *gadget, XEvent *event);
static void McMenuCleanup(McGadget *gadget);
static McMenuItem *InItem(McMenu *mnu, int x, int y);
static int TryToPropagate(McMenu *menu, XEvent *event);
static int HotkeyHandler(McHotkeyHandler *handler, XKeyEvent *event);

McMenuList *McCreateMenu(McWindow *mcw,
			 void (*callback)(int, struct McMenuItem *),
			 McMenuInit *items, int nitems, int flags) {
  int i,ix,iy,w,h,spc;
  McMenuList *list = (McMenuList *)calloc(sizeof(McMenuList), 1);
  McMenuItem *item, *prev;
  McApp *app=mcw->app;

  list->mcw=mcw;
  list->flags=flags;
  prev=NULL;
  ix=0; iy=0;
  w=0; h=0;
  for (i=0;i<nitems;i++) {
    item = (McMenuItem *)calloc(sizeof(McMenuItem), 1);
    if (!list->first) list->first=item;
    if (prev) prev->next=item;
    item->prev=prev;
    item->callback=items[i].callback;
    item->hotkey=items[i].hotkey;
    item->next=NULL;
    item->flags=items[i].flags;
    item->id=items[i].id;
    item->x=ix+BW; item->y=iy+BW;
    if (items[i].sub)
      item->subMenu=McCreateMenu(mcw, callback,
				 items[i].sub, items[i].nsub, flags);

    if (item->flags & ITEM_CHECKIT) spc=14; else spc=0;
    if (items[i].text) {
      item->normalLabel=McCreateText(mcw, _(items[i].text),
				     app->gc[GC_NORMAL],
				     app->defaultFont, spc+BW, 0);
      item->w=item->normalLabel->width+spc+BW+BW;
      item->h=item->normalLabel->height;
      if (items[i].sub) item->w+=10+BW;
    } else {
      if (item->flags & ITEM_LINE) {
	item->w=w;
	item->h=2;
	item->flags&=~(ITEM_ENABLED | ITEM_H3D | ITEM_HBOX | ITEM_DOUBLE_LINE);
      }
      if (item->flags & ITEM_DOUBLE_LINE) {
	item->w=w;
	item->h=3;
	item->flags&=~(ITEM_ENABLED | ITEM_H3D | ITEM_HBOX | ITEM_LINE);
      }
    }

    if (item->hotkey)
      item->w+=hotkey_width+app->defaultFont->max_bounds.width+8;

    if (w<item->w) w=item->w;
    h+=item->h+BW+BW;
    iy+=item->h+BW+BW;
    prev=item;
  }
  list->w=w+BW+BW; list->h=h+((app->style)?1:0);
  list->callback=callback;

  if (flags & MENU_HOTKEY) {
    if (!mcw->app->hotkeyHandler) {
      mcw->app->hotkeyHandler=&list->hotkeyHandler;
      list->hotkeyHandler.callback=HotkeyHandler;
      list->hotkeyHandler.data.list=list;
    }
  }

  return list;
}

static McMenu *McShowSubMenu(McApp *app, McGadget *starter, McMenuList *list,
			     int x, int y, McMenu *parent) {
  McMenu *mnu;
  int dummy;
  XSetWindowAttributes values;

  if (list->flags & MENU_ISDRAWN) return NULL;

  mnu = (McMenu *)calloc(sizeof(McMenu), 1);

  mnu->parent=parent;
  mnu->starter=starter;
  mnu->list = list;
  list->flags|=MENU_ISDRAWN;

  /*
   * Create the window
   */


  if (!x && !y) {
    XQueryPointer(app->display, RootWindow(app->display, app->screen),
		  (Window *)&dummy, (Window *)&dummy,
		  &x, &y, &dummy, &dummy, &dummy);

    if (list->flags & MENU_CENTER) {
      x-=((list->w+BW+BW)>>1);
      y-=BW+BW+BW;
    }
  }

  mnu->x=x; mnu->y=y;

  mnu->mcw=McCreateAppWindow(app, x-1, y-1,
			     list->w+BW+BW+2,
			     list->h+BW+BW+2,NULL,NULL);
  mnu->mcw->customData=mnu;

  values.save_under = values.override_redirect = True;
  XChangeWindowAttributes(app->display, mnu->mcw->clientwin,
			  CWSaveUnder | CWOverrideRedirect, &values);
  McMapRaised(mnu->mcw);
  mnu->gadget = McCreateGadget(mnu->mcw, GAD_3D | GAD_H3D | GAD_ACTIVE,
			       MENUGADGET, BW+1, BW+1, list->w, list->h);
  mnu->gadget->specialInfo = McCreateMenuGadget(mnu);
  McInitGadgets(mnu->mcw);
#ifndef DBG_NOGRAB
  if (!parent) {
    Cursor cursor;
    cursor=XCreateFontCursor(app->display, XC_left_ptr);
    XGrabPointer(app->display, mnu->gadget->win, False,
		 ButtonPressMask | ButtonReleaseMask | ButtonMotionMask |
		 PointerMotionMask, GrabModeAsync, GrabModeAsync, None, cursor,
		 CurrentTime);
    XFreeCursor(app->display, cursor);
  }
#endif
  InItem(mnu, -1, -1); 
  return mnu;
}


McMenu *McShowMenu(McApp *app, McGadget *starter, McMenuList *list,
		   int x, int y) {
  return McShowSubMenu(app, starter, list, x, y, NULL);
}

void McFreeMenu(McMenu *mnu) {
  McGadget *starter=mnu->starter;

  mnu->list->flags&=~MENU_ISDRAWN;

#ifndef DBG_NOGRAB
  if (!mnu->parent)
    XUngrabPointer(mnu->mcw->app->display, CurrentTime);
#endif

  if (mnu->mcw) {
    McUnmapWindow(mnu->mcw);
    McFreeWindow(mnu->mcw);
  }
  if ((!mnu->parent) && starter &&
      ((!(starter->specialInfo)) || (!(starter->specialInfo->releaseProc)))) {
    McReleaseGadget(starter, 1);
  }
  free(mnu);
}

extern void McFreeMenuList(McMenuList *mls) {
  McMenuItem *next, *mnu;
  if (mls) {
    mnu=mls->first;
    while(mnu) {
      next=mnu->next;
      McFreeMenuItem(mnu);
      mnu=next;
    }
    free(mls);
  }
}

extern void McFreeMenuItem(McMenuItem *item) {
  if (item->normalLabel) McFreeText(item->normalLabel);
  if (item->selectLabel) McFreeText(item->selectLabel);
  if (item->subMenu) McFreeMenuList(item->subMenu);
  free(item);
}

static McSpecialInfo *McCreateMenuGadget(McMenu *mnu) {
  McMenuGadget *mga = (McMenuGadget *) calloc(sizeof(McMenuGadget), 1);
  mga->specialInfo.updateProc = McMenuUpdate;
  mga->specialInfo.eventProc = McMenuEvent;
  mga->specialInfo.cleanupProc = McMenuCleanup;
  mga->menu = mnu;
  return (McSpecialInfo *)mga;
}

static void McMenuCleanup(McGadget *gadget) {
  free(gadget->specialInfo);
}

static void McMenuItemUpdate(McGadget *gadget, McMenuItem *item, int w) {
  McApp *app = gadget->mcw->app;
  McWindow *mcw = gadget->mcw;
  McText *text=NULL;
  GC gc=0, gcf=0, gcr=0;

  item->flags &= ~ITEM_UPDATE;

  if (item->flags & ITEM_HIGH) {
    if (!(text=item->selectLabel)) text=item->normalLabel;
    if (!app->style) {
      if (item->flags & ITEM_H3D) {
	McAppDrawbox(mcw, gadget->win, item->x, item->y, w, item->h, _3D_IN);
	XFillRectangle(app->display, gadget->win, app->gc[GC_SELECTED],
		       item->x, item->y, w, item->h);
	gc=app->gc[GC_SELECTED_BITMAP];
      }
      if (item->flags & ITEM_HBOX) {
	McAppDrawbox(mcw, gadget->win, item->x, item->y, w, item->h, _3D_OUT);
	XFillRectangle(app->display, gadget->win, app->gc[GC_CLEAR],
		       item->x, item->y, w, item->h);
	gc=app->gc[GC_NORMAL];
      }
    } else {
      XSetForeground(app->display, app->gc[GC_NORMAL],
		     app->colors[COL_MENU]);
      XFillRectangle(app->display, gadget->win, app->gc[GC_NORMAL],
		     item->x-BW, item->y-BW, w+BW+BW-1, item->h+BW+BW);
      XSetForeground(app->display, app->gc[GC_NORMAL],
		       app->colors[COL_FOREGROUND]);
      XSetBackground(app->display, app->gc[GC_NORMAL],
		     app->colors[COL_MENU]);
      if (item->flags & ITEM_HBOX) {
	XDrawRectangle(app->display, gadget->win, app->gc[GC_NORMAL],
		 item->x-BW, item->y-BW, w+BW, item->h+BW+1);
      }
      gcr=gc=app->gc[GC_NORMAL];
    }
  } else {
    XFillRectangle(app->display, gadget->win, app->gc[GC_CLEAR],
		   item->x-BW, item->y-BW, w+BW+BW, item->h+BW+BW);
    text=item->normalLabel;
    gc=app->gc[GC_NORMAL];
  }

  if (text)
    McWriteText(mcw, gadget->win, text, gc,
	       item->x+((item->flags&ITEM_CENTER)?(((w-text->width)>>1)-BW):0),
		item->y);
  if (item->flags & ITEM_LINE) {
    XDrawLine(app->display, gadget->win, app->gc[GC_DARK],
	      item->x-BW, item->y,item->x+w+BW-1,item->y);
    XDrawLine(app->display, gadget->win, app->gc[GC_BRIGHT],
	      item->x-BW, item->y+1,item->x+w+BW-1,item->y+1);
  }
  if (item->flags & ITEM_DOUBLE_LINE) {
    XDrawLine(app->display, gadget->win, app->gc[GC_DARK],
	      item->x-BW, item->y,item->x+w+BW-1,item->y);
    XDrawLine(app->display, gadget->win, app->gc[GC_DARK],
	      item->x-BW, item->y+2,item->x+w+BW-1,item->y+2);
  }
  if (item->flags & ITEM_CHECKED) {
    if (!app->checkmark)
      app->checkmark=McCreateBitmap(mcw,2,2,hook_bits,hook_width,hook_height);
    McPutBitmap(mcw, gadget->win, app->checkmark, gc, item->x, item->y);
  }

  if (item->hotkey) {
    int hx=item->x+w-(hotkey_width+app->defaultFont->max_bounds.width+3);
    if (!app->altmark)
      app->altmark=McCreateBitmap(mcw, 2, 2,
				  hotkey_bits, hotkey_width, hotkey_height);
    McPutBitmap(mcw, gadget->win, app->altmark,gc,hx,item->y-hotkey_height/2+
		(app->defaultFont->ascent+app->defaultFont->descent)/2-1);
    XDrawImageString(app->display, gadget->win, gc, hx+hotkey_width+3,
		     item->y+app->defaultFont->ascent,
		     &item->hotkey, 1);
  }

  if (item->subMenu) {
    int x,y;
    x=item->x+w-10;
    y=item->y+(item->h>>1)-4;
    if (!app->style) {
      XDrawLine(app->display, gadget->win, app->gc[GC_DARK], x, y+8, x+7, y+4);
      XDrawLine(app->display, gadget->win, app->gc[GC_BRIGHT], x, y, x+7, y+4);
      XDrawLine(app->display, gadget->win, app->gc[GC_BRIGHT], x, y, x, y+7);
    } else {
      XPoint points[4];
      points[0].x=x; points[0].y=y+9;
      points[1].x=x+7; points[1].y=y+4;
      points[2].x=x; points[2].y=y;
      points[3].x=x; points[3].y=y+9;
      XFillPolygon(app->display, gadget->win, app->gc[GC_NORMAL],
		   points, 4, Convex, CoordModeOrigin);
    }
  }

  if (item->flags & ITEM_GRAY) {
    XGCValues values;
    values.foreground=app->colors[COL_BACKGROUND];
    values.stipple=app->stipple1;
    values.fill_style=FillStippled;
    gcf = XCreateGC(app->display, gadget->win,
		    GCStipple|GCFillStyle|GCForeground, &values);
    XFillRectangle(app->display, gadget->win, gcf,
		   item->x-BW, item->y-BW, w+BW+BW, item->h+BW+BW);
    XFreeGC(app->display, gcf);
  }

  if (gcr) XSetBackground(app->display, app->gc[GC_NORMAL],
			  app->colors[COL_BACKGROUND]);
}

static void McMenuUpdate(McGadget *gadget, int busy, int all) {
  McMenuItem *item;
  McMenuList *list;
  int w;

  list=((McMenuGadget *)(gadget->specialInfo))->menu->list; /* wow! */
  item=list->first;
  w=list->w-BW-BW;

  while(item) {
    McMenuItemUpdate(gadget, item, w);
    item=item->next;
  }
}

static McMenuItem *InItem(McMenu *mnu, int x, int y) {
  McMenuItem *item, *found=NULL;
  int inside, hit=0;
  x-=BW; y-=BW;
  inside = ((x>=0) && (x<mnu->list->w) &&
	    (y>=0) && (y<mnu->list->h));

  item=mnu->list->first;
  while(item) {
    if (inside && (!hit) && (y<item->y+item->h)) {
      hit=1;
      if (item->flags&ITEM_ENABLED) {
	found=item;
	if (!(item->flags & ITEM_HIGH)) {
	  item->flags|=ITEM_HIGH | ITEM_UPDATE;
	}
      }
    } else {
      if (item->flags & ITEM_HIGH) {
	item->flags&=~ITEM_HIGH;
	item->flags|=ITEM_UPDATE;
      }
    }
    item=item->next;
  }

  if (found) {
    if (mnu->child) {
      if (found!=mnu->subMenuItem) {
	McMenu *child = mnu->child, *next;
	while(child) {
	  next=child->child;
	  McFreeMenu(child);
	  child=next;
	}
	mnu->child=NULL; mnu->subMenuItem=NULL;
      }
    }
  } else {
    if (mnu->child) {
      mnu->subMenuItem->flags|=ITEM_HIGH;
      mnu->subMenuItem->flags^=ITEM_UPDATE;
    }
  }

  item=mnu->list->first;
  while(item) {
    if (item->flags & ITEM_UPDATE)
      McMenuItemUpdate(mnu->gadget, item, mnu->list->w-BW-BW);
    item=item->next;
  }

  return found;
}

void McZapMenus(McMenu *first) {
  McMenu *next;
  while((next=first->parent)) first=next;
  while(first) {
    next=first->child;
    McFreeMenu(first);
    first=next;
  }
}

static int McMenuEvent(McGadget *gadget, XEvent *event) {
  McMenuGadget *mga = (McMenuGadget *)(gadget->specialInfo);
  McWindow *mcw=gadget->mcw;
  McApp *app = mcw->app;
  McMenuItem *item;
  McGadget *starter;
  McMenu *mnu=mga->menu;

  switch(event->type) {
  case Expose:
    return 0;

  case ButtonPress:
    if (!InItem(mnu, event->xbutton.x, event->xbutton.y))
      return TryToPropagate(mnu, event);
    return 1;

  case MotionNotify:
    if ((item=InItem(mnu, event->xmotion.x, event->xmotion.y))) {
      if (item->subMenu && (!mnu->child)) {
	mnu->child = McShowSubMenu(app, mnu->starter, item->subMenu,
				   mnu->x+mnu->list->w-BW, mnu->y+item->y,
				   mga->menu);
	mnu->subMenuItem=item;
      }
      return 1;
    } else {
      if (TryToPropagate(mnu, event)) return 1;
      if ((starter=mnu->starter)) {
	if (starter->specialInfo && starter->specialInfo->releaseProc) {
	  int xr, yr;
	  Window nonsense;
	  XTranslateCoordinates(app->display, gadget->win, starter->win,
				event->xmotion.x, event->xmotion.y,
				&xr, &yr, &nonsense);
	  (*(starter->specialInfo->releaseProc))(starter, xr,yr);
	  return 1;
	}
      }
      return 0;
    }

  case ButtonRelease:
    if ((item=InItem(mnu, event->xbutton.x, event->xbutton.y))) {
      void (*callback)(int, struct McMenuItem *);
      int id;
      if (!item->subMenu) {
	if (item->flags & ITEM_TOGGLE) item->flags^=ITEM_CHECKED;
	if (!(callback=item->callback)) callback=mnu->list->callback;
	id=item->id;
      } else {
	 /* ButtonRelease in an item having subitems does nothing
	  * but zaps the menu away
	  */
	callback=NULL;
	id=0;
      }
      if ((starter=mnu->starter) && starter->specialInfo &&
	  starter->specialInfo->releaseProc) {
	(*(starter->specialInfo->releaseProc))(starter, 0, 0);
      } else {
	McZapMenus(mnu);
      }
      XFlush(app->display);
      if (callback) (*callback)(id, item);
      return 1;
    } else {
      if (!TryToPropagate(mnu, event)) {
	if ((starter=mnu->starter) && starter->specialInfo &&
	    starter->specialInfo->releaseProc) {
	  (*(starter->specialInfo->releaseProc))(starter, 0, 0);
	} else {
	  McZapMenus(mnu);
	}
      }
      return 1;
    }
  }     
  return 0;
}

/*
 * If a menu can't handle an event, there is a chance that the event is for
 * any of the child menu(s). We recompute the coordinates according to the
 * child's window, and pass the event along to it.
 * The child itself may decide to propagate the event further on.
 * So the event passes down the hirarchy until no more childs are left or
 * it reaches a child which knows what to do with it.
 */
static int TryToPropagate(McMenu *menu, XEvent *event) {
  McMenu *child=menu->child;

  if (child) {
    switch(event->type) {
    case ButtonPress:
    case ButtonRelease:
      event->xbutton.x=event->xbutton.x+menu->mcw->x-child->mcw->x;
      event->xbutton.y=event->xbutton.y+menu->mcw->y-child->mcw->y;
      return McMenuEvent(child->gadget, event);
    case MotionNotify:
      event->xmotion.x=event->xmotion.x+menu->mcw->x-child->mcw->x;
      event->xmotion.y=event->xmotion.y+menu->mcw->y-child->mcw->y;
      return McMenuEvent(child->gadget, event);
    }
  }
  return 0;
}

/**************************************************************************/

static int HotkeyHandler(McHotkeyHandler *handler, XKeyEvent *event) {
  McMenuList *list;
  int ch;

  list=handler->data.list;

  if (list->mcw->keycnt!=1) return 0;
  ch=tolower(*(list->mcw->keys));

  return _McHandleHotkeyInItems(list, list->first, ch);
}

int _McHandleHotkeyInItems(McMenuList *menu, McMenuItem *item,
			   unsigned char key) {
  while(item) {
    if (item->subMenu) {
      if (_McHandleHotkeyInItems(menu, item->subMenu->first, key)) return 1;
    } else {
      if (tolower(item->hotkey)==key) {
	if (item->flags & ITEM_TOGGLE) item->flags^=ITEM_CHECKED;
	if (item->callback) (*(item->callback))(item->id, item);
	else if (menu->callback) (*(menu->callback))(item->id, item);
	return 1;
      }
    }
    item=item->next;
  }
  return 0;
}

/**************************************************************************/

McMenuItem *McFindMenuItem(McMenuList *parent, int num) {
  McMenuItem *result;
  result=parent->first;
  while(--num>=0) { result=result->next; if (!result) break; }
  return result;
}

McMenuList *McFindMenuList(McMenuList *parent, int num) {
  return McFindMenuItem(parent, num)->subMenu;
}

