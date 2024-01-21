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
#include "../config.h"
#include "McApp.h"
#include "McGadget.h"
#include "McFocus.h"
#include "McDebug.h"
#include "McBitmap.h"
#include "McToolbar.h"
#include "McSelection.h"
#include "McBuffer.h"

/**************************************************************************/

#define TOOLBAR ((McToolbar *)(gadget->specialInfo))

static void ToolbarCleanup(McGadget *gadget);
static int  ToolbarEvent(McGadget *gadget, XEvent *event);
static const string_t ToolbarTip(McGadget *gadget, int *x, int *y);

/**************************************************************************/

extern McSpecialInfo *McCreateToolbar(unsigned int flags,
				      const McBitmap *bmp,
				      McToolbarItem *buttons, int count,
				      McToolbarCallback *callback) {
  McToolbar *toolbar = (McToolbar *) calloc(sizeof(McToolbar), 1);
  toolbar->flags    = flags;
  toolbar->bitmap   = bmp;
  toolbar->callback = callback;
  toolbar->buttons  = buttons;
  toolbar->count    = count;
  toolbar->item     = -1;
  toolbar->specialInfo.updateProc = McToolbarUpdate;
  toolbar->specialInfo.eventProc = ToolbarEvent;
  toolbar->specialInfo.cleanupProc = ToolbarCleanup;
  toolbar->specialInfo.tipProc = ToolbarTip;

  return (McSpecialInfo *)toolbar;
}

static void ToolbarCleanup(McGadget *gadget) {
  free(gadget->specialInfo);
}

/**************************************************************************/

#define TOOLBAR_ITEM_WIDTH (BW+BW+2)
#define TOOLBAR_OFFSET TOOLBAR_ITEM_WIDTH

void McToolbarUpdate(McGadget *gadget, int busy, int all) {
  McToolbar *toolbar = (McToolbar *)gadget->specialInfo;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  int indx, max, x, btn;

  max=toolbar->bitmap->width/toolbar->bitmap->wpi;
  x=BW;
  for(btn=0; btn<toolbar->count; btn++) {
    if (toolbar->buttons[btn].id<0) {
      x-=toolbar->buttons[btn].id;
      continue;
    }

    if (all || (btn==toolbar->item)) {
      int gcid, threed;

      if ((btn==toolbar->item) && (gadget->flags&GAD_SELECTED)) {
	gcid=GC_SELECTED;
	threed=_3D_IN;
      } else {
	gcid=GC_CLEAR;
	threed=_3D_OUT;
      }

      XFillRectangle(mcw->app->display, gadget->win,
		     mcw->app->gc[gcid], x, BW,
		     toolbar->bitmap->wpi+2, toolbar->bitmap->height+2);

      indx=toolbar->buttons[btn].image;
      if (indx<max) {
	McPutIndexedBitmap(gadget->mcw, gadget->win, toolbar->bitmap,
			   app->gc[GC_NORMAL], x+1, BW+1, indx);
      }
      McAppDrawbox(mcw, gadget->win, x, BW,
		   toolbar->bitmap->wpi+2, toolbar->bitmap->height+2,
		   threed);
    }

    x+=toolbar->bitmap->wpi+TOOLBAR_OFFSET;
  }

  if (all) {
    int h=gadget->height-2;
    XDrawLine(app->display, gadget->win, app->gc[GC_DARK],
	      0, h, gadget->width+1, h);
    XDrawLine(app->display, gadget->win, app->gc[GC_BRIGHT],
	      0, h+1, gadget->width+1, h+1);
  }

}

/**************************************************************************/

static int ToolbarFindButton(McGadget *gadget, int hit) {
  McToolbar *toolbar = (McToolbar *)gadget->specialInfo;
  int x, btn;

  x=0;
  for(btn=0; btn<toolbar->count; btn++) {
    if (toolbar->buttons[btn].id<0) {
      x-=toolbar->buttons[btn].id;
      continue;
    }

    if ((hit>=x) && (hit<(x+BW+BW+toolbar->bitmap->wpi)))
      return btn;

    x+=toolbar->bitmap->wpi+TOOLBAR_OFFSET;
  }

  return -1;
}

static void ToolbarPress(McGadget *gadget, int x) {
  int item;
  if ((item=ToolbarFindButton(gadget, x))>=0) {
    gadget->flags|=GAD_PRESSED|GAD_SELECTED;
    ((McToolbar *)gadget->specialInfo)->item=item;
    McToolbarUpdate(gadget, 0, 0);
  }
}

static void ToolbarRelease(McGadget *gadget, int x, int in) {
  int item;
  McToolbar *toolbar = (McToolbar *)gadget->specialInfo;

  if (in) {
    item=ToolbarFindButton(gadget, x);
    if ((item==toolbar->item) && toolbar->callback)
      (toolbar->callback)(gadget, &toolbar->buttons[item]);
  }

  gadget->flags&=~(GAD_PRESSED|GAD_SELECTED);
  McToolbarUpdate(gadget, 0, 0);
  toolbar->item=-1;
}

static void ToolbarMotion(McGadget *gadget, int x, int in) {
  int item;
  McToolbar *toolbar = (McToolbar *)gadget->specialInfo;

  if (in) {
    item=ToolbarFindButton(gadget, x);
    if (item!=toolbar->item) in=0;
  }

  if (in) {
    if (gadget->flags&GAD_SELECTED) return;
    gadget->flags|=GAD_SELECTED;
  } else {
    if (!(gadget->flags&GAD_SELECTED)) return;
    gadget->flags&=~GAD_SELECTED;
  }

  McToolbarUpdate(gadget, 0, 0);
}

static int ToolbarEvent(McGadget *gadget, XEvent *event) {
  switch (event->type) {
  case Expose:
    return 0;

  case ButtonPress:
    if (event->xbutton.button!=1) return 0;
    if (McInRectangle(event->xbutton.x, event->xbutton.y,
		      0, 0, gadget->width, gadget->height)) {
      McMoveGadgetToStart(gadget);
      ToolbarPress(gadget, event->xbutton.x);
      return 1;
    }
    return 0;

  case ButtonRelease:
    if (event->xbutton.button!=1) return 0;
    if (gadget->flags|=GAD_PRESSED) {
      ToolbarRelease(gadget, event->xbutton.x,
		     McInRectangle(event->xbutton.x, event->xbutton.y,
				   0, 0, gadget->width, gadget->height));
      return 1;
    }
    return 0;

  case MotionNotify:
    if (gadget->flags|=GAD_PRESSED) {
      ToolbarMotion(gadget, event->xbutton.x,
		    McInRectangle(event->xbutton.x, event->xbutton.y,
				  0, 0, gadget->width, gadget->height));
      return 1;
    }
    return 0;

  }

  return 0;
}

/**************************************************************************/

static const string_t ToolbarTip(McGadget *gadget, int *x, int *y) {
  int i=ToolbarFindButton(gadget, *x);
  if (i>=0) {
    McToolbar *toolbar=(McToolbar *)(gadget->specialInfo);
    McToolbarItem *item=&toolbar->buttons[i];
    
    *y=gadget->height-BW;
    *x=item->x+((toolbar->bitmap->wpi+TOOLBAR_ITEM_WIDTH)>>1);

#if HAVE_GETTEXT
    /* Only search the translation when called for the first time */
    if (item->xtip) return item->xtip;
    if (!item->tip) return NULL;
    item->xtip=_(item->tip);
    return item->xtip;
#else
    return item->tip;
#endif
  }
  return NULL;
}

/**************************************************************************/

int McToolbarHeight(McGadget *gad) {
  if ((((McToolbar *)(gad->specialInfo))->flags) & MCTBF_LINE)
    return gad->height-BW-BW-2-BW-2;
  else
    return gad->height-BW-BW-2;
}

/**************************************************************************/

McGadget *MakeToolbar(McWindow *mcw, McGadget *menu, int *ty,
		      const McBitmap *bmp, McToolbarItem *buttons,
		      int count, McToolbarCallback *callback) {
  McGadget *gadget;
  int w;
  int h=bmp->height+BW+BW+2;
  int i, x, y, flags, image;

  w=0;
  image=0;
  for (i=0; i<count; i++) {
    buttons[i].x=w+BW+1;

    if (buttons[i].id<0) {
      w-=buttons[i].id;
    } else {
      w+=bmp->wpi+TOOLBAR_OFFSET;
      if (buttons[i].image<0)
	buttons[i].image=image++;
      else
	image=buttons[i].image+1;
    }

  }

  if (menu) {
    int diff=mcw->w - (menu->x+menu->width+w+BW+BW+BW+BW);
    if (diff>=0) {
      /* Yep, the toolbar fits right next to the menu on the left hand side */

      w+=BW+BW;
      x=mcw->w-w;
      y=BW;
      /* Resize the menu so it takes up all room not used by the toolbar */
      McResizeGadget(menu, x-menu->x, menu->height);
      flags=MCTBF_LINE;
      h+=BW+2;

      /* Align the bottom of menu and toolbar */
      if (menu->y+menu->height<y+h) {
	McResizeGadget(menu, -1, h);
      } else if (menu->y+menu->height>y+h) {
	y=(menu->y+menu->height)-h;
      }

      /* Don't bother resizing it, we gonna resize the toolbar instead */
      menu->bottomRightGravity=NorthWestGravity;
    } else {
      /* Nope, no more room, put the toolbar below */
      x=BW;
      y=menu->height;
      /* Make sure the menu uses the whole window width */
      McResizeGadget(menu, mcw->w-BW-BW-menu->x, menu->height);
      flags=0;
      h+=BW+2;
    }
  } else {
    x=BW;
    y=BW;
    flags=MCTBF_LINE;
  }

  gadget = McCreateGadget(mcw, GAD_ACTIVE|GAD_HNONE|GAD_NOBORDER,
			  TOOLBARGADGET, x, y, w, h);
  gadget->specialInfo = McCreateToolbar(flags, bmp, buttons, count, callback);
  gadget->bottomRightGravity=NorthEastGravity;

  if (ty) *ty=y+gadget->height;

  return gadget;
}

/**************************************************************************/
