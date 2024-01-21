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

#include "../config.h"
#include <X11/keysym.h>
#include <X11/cursorfont.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <X11/Xatom.h>
#include "McApp.h"
#include "McGadget.h"
#include "McDebug.h"
#include "McMenubar.h"

/**************************************************************************/

#define MENUBAR ((McMenubar *)(gadget->specialInfo))
#define DISTANCE (8*BW)

/**************************************************************************/

static int  MenubarEvent(McGadget *gadget, XEvent *event);
static void MenubarCleanup(McGadget *gadget);
static void MenubarCreateMenus(McMenubar *menubar,
			       void (*callback)(int, struct McMenuItem *));
static void MenubarCreateSizes(McMenubar *menubar);
static int  MenubarRelease(McGadget *gadget, int x, int y);
static int  HotkeyHandler(McHotkeyHandler *handler, XKeyEvent *event);

/**************************************************************************/

McSpecialInfo *McCreateMenubar(McWindow *mcw,
			       unsigned int flags, XFontStruct *font,
			       McMenubarInit *items, int size,
			       void (*callback)(int, struct McMenuItem *)) {

  McMenubar *menubar = (McMenubar *) calloc(sizeof(McMenubar), 1);

  menubar->mcw = mcw;
  menubar->flags = flags;
  menubar->specialInfo.updateProc = McMenubarUpdate;
  menubar->specialInfo.eventProc = MenubarEvent;
  menubar->specialInfo.cleanupProc = MenubarCleanup;
  menubar->specialInfo.releaseProc = MenubarRelease;
  menubar->size = size;
  menubar->items = items;
  menubar->font = font;
  menubar->widths = NULL;
  menubar->current = -1;
  menubar->lastcurrent = -1;

  MenubarCreateSizes(menubar);

  MenubarCreateMenus(menubar, callback);

  return (McSpecialInfo *)menubar;
}

void McInstallMenubar(McGadget *gadget) {
  McWindow *mcw=gadget->mcw;
  McMenubar *menubar=(McMenubar *)gadget->specialInfo;

  if (!mcw->app->hotkeyHandler) {
    mcw->app->hotkeyHandler=&menubar->hotkeyHandler;
    menubar->hotkeyHandler.callback=HotkeyHandler;
    menubar->hotkeyHandler.data.gadget=gadget;
  }

  menubar->mcw = mcw;
}

/**************************************************************************/

static void MenubarCreateMenus(McMenubar *menubar,
			       void (*callback)(int, struct McMenuItem *)) {
  int i;
  menubar->menu=(McMenuList **)calloc(menubar->size, sizeof(McMenu));
  for (i=0; i<menubar->size; i++) {
    menubar->menu[i]=McCreateMenu(menubar->mcw, callback,
				  menubar->items[i].items,
				  menubar->items[i].count, 0);
  }
}

/**************************************************************************/

static void MenubarCleanup(McGadget *gadget) {
  McWindow *mcw;

  McMenubar *menubar = (McMenubar *)gadget->specialInfo;
  if (menubar->menu) {
    int i;
    for (i=0; i<menubar->size; i++) {
      if (menubar->menu[i]) McFreeMenuList(menubar->menu[i]);
    }
    free(menubar->menu);
  }
  if (menubar->widths) free(menubar->widths);
  if (menubar->titles) free(menubar->titles);

  mcw=gadget->mcw;
  if (mcw->app->hotkeyHandler==&menubar->hotkeyHandler)
    mcw->app->hotkeyHandler=NULL;

  free(gadget->specialInfo);
}

/**************************************************************************/

void McMenubarUpdate(McGadget *gadget,int busy,int all) {
  McMenubar *menubar = (McMenubar *) gadget->specialInfo;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  int i,x,y,h;
  unsigned char *data;
  GC gc1 = XCreateGC(app->display, gadget->win, 0, NULL);
  GC gc2 = XCreateGC(app->display, gadget->win, 0, NULL);
  GC gc3, gc4;

  XCopyGC(app->display, app->gc[GC_NORMAL], ~0, gc1);
  XCopyGC(app->display, app->gc[GC_SELECTED_BITMAP], ~0, gc2);
  gc3=app->gc[GC_SELECTED];
  gc4=app->gc[GC_CLEAR];

  XSetFont(app->display, gc1, menubar->font->fid);
  XSetFont(app->display, gc2, menubar->font->fid);

#if 0
  if (all) XFillRectangle(app->display, gadget->win, app->gc[GC_DARK],
			  0, 0, gadget->width, gadget->height);
#endif

  h=menubar->height+BW+BW;
  x=(DISTANCE>>1)+BW+BW;
  y=(((gadget->height-BW-2)-h)>>1) + BW;

#if 0
  printf("gadget=%d menubar=%d ofs=%d\n",
	 gadget->height, menubar->height, 
	 ((gadget->height-BW-2)-menubar->height)>>1);
#endif

  for (i=0; i<menubar->size; i++) {
    data=menubar->titles[i];
    if (i==menubar->current) {
      if (!app->style) {
	XFillRectangle(app->display, gadget->win, gc3,
		       x-(DISTANCE>>1)+BW, y,
		       menubar->widths[i]+DISTANCE-BW-BW, h-BW-BW);
	McAppDrawbox(mcw, gadget->win,
		     x-(DISTANCE>>1)+BW, y,
		     menubar->widths[i]+DISTANCE-BW-BW, h-BW-BW, _3D_IN);
      } else {
	XSetForeground(app->display, gc3, app->colors[COL_MENU]);
	XFillRectangle(app->display,gadget->win,gc3,
		       x-(DISTANCE>>1), y-BW,
		       menubar->widths[i]+DISTANCE, h);
	XSetForeground(app->display, gc3, app->colors[COL_SELECTED]);
	XSetBackground(app->display, gc2, app->colors[COL_MENU]);
      }
      XDrawImageString(app->display, gadget->win, gc2,
		       x, menubar->font->ascent+y, data, strlen(data));
    } else if (all || (i==menubar->lastcurrent)) {
      XFillRectangle(app->display, gadget->win, gc4,
		     x-(DISTANCE>>1), y-BW,
		     menubar->widths[i]+DISTANCE, h);
      XDrawImageString(app->display, gadget->win, gc1,
		       x, menubar->font->ascent+y, data, strlen(data));
    }
    
    x+=menubar->widths[i]+DISTANCE;
  }
  menubar->lastcurrent=-1;

  if (all) {
    h=gadget->height-2;
    XDrawLine(app->display, gadget->win, app->gc[GC_DARK],
	      0, h, gadget->width+1, h);
    XDrawLine(app->display, gadget->win, app->gc[GC_BRIGHT],
	      0, h+1, gadget->width+1, h+1);
  }

  XFreeGC(app->display, gc1);
  XFreeGC(app->display, gc2);
}

/**************************************************************************/

static void MenubarCreateSizes(McMenubar *menubar) {
  int i;
  int ascent, descent, dummy;
  XCharStruct overall;

  menubar->widths=calloc(menubar->size, sizeof(int));
  menubar->titles=calloc(menubar->size, sizeof(int));
  menubar->width=0;

  for(i=0; i<menubar->size; i++) {
    menubar->titles[i]=_(menubar->items[i].title);
    XTextExtents(menubar->font, menubar->titles[i], strlen(menubar->titles[i]),
		 &dummy, &ascent, &descent, &overall);
    
    menubar->widths[i]=overall.width;
    menubar->width+=overall.width+DISTANCE;
  }

  menubar->height=ascent+descent;
}

/**************************************************************************
 *
 * Check in which menu the coordinate x is in.
 *
 * Return the number of the menu found, -1 if none found and -2 if the
 * menu found is already current.
 *
 */
static int IsInMenu(McGadget *gadget, int x) {
  McMenubar *menubar = (McMenubar *)gadget->specialInfo;
  int prev, next, i;

  if (x>=0) {
    next=BW;
    for (i=0; i<menubar->size; i++) {
      prev=next;
      next+=menubar->widths[i]+DISTANCE;
      if ((x>=(prev+BW)) && (x<=(next-BW))) {
	if (i!=menubar->current) return i; else return -2;
      }
    }
  }

  return -1;
}

static void MenubarOff(McGadget *gadget) {
  McMenubar *menubar = (McMenubar *)gadget->specialInfo;
  if (menubar->current!=-1) {
    menubar->lastcurrent=menubar->current;
    menubar->current=-1;
    McMenubarUpdate(gadget, 0, 0);
  }
  gadget->flags&=~GAD_PRESSED;
}

/**************************************************************************/

static void ZapMenubar(McMenubar *menubar) {
  if (menubar->currentMenu) {
    McZapMenus(menubar->currentMenu);
    menubar->currentMenu=NULL;
  }
}

static void PopdownMenu(McGadget *gadget, int this) {
  McMenubar *menubar = (McMenubar *)gadget->specialInfo;
  McApp *app=gadget->mcw->app;
  int x, y, xr, yr, i;
  Window nonsense;

  ZapMenubar(menubar);

  if (this>=0) {
    x=BW;
    for (i=0; i<this; i++) x+=menubar->widths[i]+DISTANCE;
    y=(((gadget->height-BW-2)-(menubar->height+BW+BW))>>1)+BW+menubar->height;

    XTranslateCoordinates(app->display, gadget->win,
			  RootWindow(app->display, app->screen),
			  x, y, &xr, &yr, &nonsense);

    menubar->currentMenu=McShowMenu(app, gadget, menubar->menu[this], xr, yr);
  }
}

/**************************************************************************/

static int MenubarRelease(McGadget *gadget, int x, int y) {
  int i;
  McMenubar *menubar = (McMenubar *)gadget->specialInfo;

  if (gadget->flags&GAD_PRESSED) {
    if (!x && !y) {
      ZapMenubar(menubar);
      MenubarOff(gadget);
      return 1;
    }

    if (McInRectangle(x, y, 0, 0, gadget->width, gadget->height)) {
      i=IsInMenu(gadget, x);
      if (i<-1) return 0;
      menubar->lastcurrent=menubar->current;
      menubar->current=i;
      McMenubarUpdate(gadget, 0, 0);
      PopdownMenu(gadget, i);
      return 1;
    }
    return 0;
  }

  ZapMenubar(menubar);
  return 1;
}

/**************************************************************************/

static int HotkeyHandler(McHotkeyHandler *handler, XKeyEvent *event) {
  McMenubar *menubar;
  McGadget *gadget;
  int i, ch;

  gadget=handler->data.gadget;

  if (gadget->mcw->keycnt!=1) return 0;
  ch=tolower(*(gadget->mcw->keys));

  menubar = (McMenubar *)gadget->specialInfo;
  for (i=0; i<menubar->size; i++) {
    if (_McHandleHotkeyInItems(menubar->menu[i],
			       menubar->menu[i]->first, ch)) return 1;
  }
  return 0;
}

/**************************************************************************/

static int MenubarEvent(McGadget *gadget, XEvent *event) {
  McMenubar *menubar = (McMenubar *)gadget->specialInfo;
  int this;

  switch(event->type) {
  case MotionNotify:
  case ButtonPress:
    if (McInRectangle(event->xbutton.x, event->xbutton.y,
		      0, 0, gadget->width, gadget->height)) {
      gadget->flags|=GAD_PRESSED;
      this=IsInMenu(gadget, event->xbutton.x);
      if (this>-1) {
	menubar->lastcurrent=menubar->current;
	menubar->current=this;
	McMenubarUpdate(gadget, 0, 0);
	PopdownMenu(gadget, this);
      }
      return 1;
    }

  case ButtonRelease:
    if (gadget->flags & GAD_PRESSED) {
      MenubarOff(gadget);
      return 1;
    }
  }

  return 0;
}

/**************************************************************************/

struct McMenuItem *McMenubarMenu(McGadget *gadget, int num) {
  McMenubar *menubar = (McMenubar *)gadget->specialInfo;
  return menubar->menu[num]->first;
}

struct McMenuList *McMenubarList(McGadget *gadget, int num) {
  McMenubar *menubar = (McMenubar *)gadget->specialInfo;
  return menubar->menu[num];
}

/**************************************************************************/

McGadget *MakeMenubar(McWindow *mcw, McMenubarInit *items, int size,
		      void (*callback)(int, struct McMenuItem *), int width) {
  McGadget *gadget;
  McMenubar *bar;

  if (!width) width=mcw->w;

  bar = (McMenubar *)McCreateMenubar(mcw, 0, mcw->app->defaultFont,
				     items, size, callback);

  if (width<0) width=bar->width+(BW<<2);

  gadget = McCreateGadget(mcw, GAD_ACTIVE|GAD_HNONE|GAD_NOBORDER,
			  MENUBARGADGET, 0, BW, width,
			  mcw->app->defaultFont->ascent+
			    mcw->app->defaultFont->descent+BW+BW+BW);

  gadget->specialInfo = (McSpecialInfo *)bar;
  gadget->bottomRightGravity=NorthEastGravity;

  McInstallMenubar(gadget);

  return gadget;
}

/**************************************************************************/

