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
#include "McChoice.h"

#define CHOICE ((McChoice *)(gadget->specialInfo))

static int  ChoiceEvent(McGadget *gadget, XEvent *event);
static void ChoiceCleanup(McGadget *gadget);
static void CreateChoiceWindow(McGadget *gadget);
static void FreeChoiceWindow(McGadget *gadget);
static void ChoiceUpdateWidthAndHeight(McChoice *choice);
static void RedrawChoiceWindow(McChoice *choice);
static int ChoiceWindowSelect(McChoice *choice, int selection);
static int ChoiceKey(McGadget *gadget, XKeyEvent *event);

McSpecialInfo *McCreateChoice(unsigned int flags, XFontStruct *font,
			      const unsigned char *const *const data,
			      int size) {

  McChoice *choice = (McChoice *) calloc(sizeof(McChoice), 1);
  choice->flags = flags;
  choice->specialInfo.updateProc = McChoiceUpdate;
  choice->specialInfo.eventProc = ChoiceEvent;
  choice->specialInfo.keyboardProc = ChoiceKey;
  choice->specialInfo.cleanupProc = ChoiceCleanup;
  choice->data = data;
  choice->size = size;
  choice->font = font;
  choice->selection = -1;
  choice->oldtxtwidth = 0;
  choice->center=-1;
  ChoiceUpdateWidthAndHeight(choice);
  return (McSpecialInfo *)choice;
}

static void ChoiceCleanup(McGadget *gadget) {
  free(gadget->specialInfo);
}

void McChoiceUpdate(McGadget *gadget,int busy,int all) {
  McChoice *choice = (McChoice *) gadget->specialInfo;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  const unsigned char *data;
  int i,x,y;
  GC gc1;

  if (gadget->flags&GAD_SELECTED) i=_3D_IN; else i=_3D_OUT;

  McAppDrawbox(mcw, gadget->win, BW+BW+BW, (gadget->height>>1)-(BW/2),
	       3*BW, BW, i);

  if (choice->data) {
    gc1 = XCreateGC(app->display, gadget->win, 0, NULL);
    if (gadget->flags & GAD_SELECTED) {
      XCopyGC(app->display, app->gc[GC_SELECTED_BITMAP], ~0, gc1);
    } else {
      XCopyGC(app->display, app->gc[GC_NORMAL], ~0, gc1);
    }
    XSetFont(app->display, gc1, choice->font->fid);
    x=9*BW;
    y=1;

    data=choice->data[choice->selection];
    XDrawImageString(app->display, gadget->win, gc1,
		     x, y + choice->font->ascent + choice->fonty,
		     data, strlen(data));

    XFreeGC(app->display, gc1);
    McDrawFocusMark(gadget,
		    (gadget->flags&GAD_SELECTED)?COL_SELECTED:COL_BACKGROUND);
  }
}

int McChoiceSelect(McGadget *gadget, int selection) {
  McChoice *choice = (McChoice *)gadget->specialInfo;

  if ((selection != choice->selection) && (selection<choice->size)) {

    if (choice->flags & CH_AUTORESIZE) {
      int ascent, descent, dummy, oldtxtwidth, newtxtwidth, newwidth, newx;
      XCharStruct overall;

      oldtxtwidth=choice->oldtxtwidth;

      if (selection>=0) {
	XTextExtents(choice->font,
		     choice->data[selection],
		     strlen(choice->data[selection]),
		     &dummy, &ascent, &descent, &overall);
	newtxtwidth=overall.width;
      } else {
	newtxtwidth=0;
      }

      choice->oldtxtwidth=newtxtwidth;

      newwidth = gadget->width - oldtxtwidth + newtxtwidth;
      if (choice->flags & CH_AUTORECENTER) {
	int center = gadget->x + (gadget->width>>1);
	if ((center>CHOICE->center+1) || (center<CHOICE->center-1)) {
	  CHOICE->center=center;
	} else {
	  center=CHOICE->center;
	}
	newx=center-(newwidth>>1);
      } else {
	newx = gadget->x;
      }

      gadget->width=newwidth;
      if (gadget->x>newx) {
	gadget->x=newx;
	McMoveGadget(gadget, -1, -1);
	McResizeGadget(gadget, -1, -1);
      } else {
	gadget->x=newx;
      	McResizeGadget(gadget, -1, -1);
	McMoveGadget(gadget, -1, -1);
      }
    }

    choice->selection=selection;
    if (selection>=0) McGadgetRedraw(gadget);
    return 1;
  }
  return 0;
}

void McChoiceSetList(McGadget *gadget, const unsigned char *const *const data,
		     int size) {
  McChoice *choice = (McChoice *) gadget->specialInfo;

  McChoiceSelect(gadget, -1);
  choice->data = data;
  choice->size = size;
  if (data) {
    ChoiceUpdateWidthAndHeight(choice);
    McChoiceSelect(gadget, 0);
  } else {
    McChoiceUpdate(gadget, 0, 1);
  }
}

static void ChoiceUpdateWidthAndHeight(McChoice *choice) {
  int i, dummy, ascent, descent, width;
  XCharStruct overall;

  if (choice->data) {
    width=0;
    for (i=0; i<choice->size; i++) {
      XTextExtents(choice->font, choice->data[i], strlen(choice->data[i]),
		   &dummy, &ascent, &descent, &overall);
      if (overall.width>width) width=overall.width;
    }
    choice->width=width+BW;
    choice->height=ascent+descent+BW+BW;
  }
}

static int ChoiceWindowEvent(McWindow *mcw, XEvent *event) {
  McGadget *gadget = (McGadget *)mcw->customData;
  McChoice *choice = (McChoice *)gadget->specialInfo;
  int n;

  switch(event->type) {
  case Expose:
    RedrawChoiceWindow(choice);
    return 1;

  case ButtonRelease:
  case ButtonPress:
  case MotionNotify:
    n=((event->xbutton.y)-BW) / (choice->height);
    if (n<0) n=0;
    if (n>=choice->size) n=choice->size-1;
    if (event->type==ButtonRelease) {
      ChoiceWindowSelect(choice, n);
      gadget->flags&=~GAD_PRESSED;
      gadget->flags^=GAD_SELECTED;
      if (!McChoiceSelect(gadget, choice->win_selection))
	McGadgetRedraw(gadget);
      FreeChoiceWindow(gadget);
      if (gadget->callbackUp) (*gadget->callbackUp)(gadget);
    } else {
      if (ChoiceWindowSelect(choice, n))
	if (gadget->callbackDown) (*gadget->callbackDown)(gadget);
    }
    return 1;
  }

  return 0;
}

static int ChoiceEvent(McGadget *gadget, XEvent *event) {
/*  McChoice *choice = (McChoice *)gadget->specialInfo; */

  if (event->type==Expose) return 0;

  if (McInRectangle(event->xbutton.x, event->xbutton.y,
		    0, 0, gadget->width, gadget->height)) {
    if (!(gadget->flags & GAD_PRESSED)) {
      switch(event->type) {
      case ButtonPress:
	gadget->flags|=GAD_PRESSED;
	gadget->flags^=GAD_SELECTED;
	McGadgetRedraw(gadget);
	CreateChoiceWindow(gadget);
	McMoveGadgetToStart(gadget);
	McSetFocus(gadget);
	XFlush(gadget->mcw->app->display);
	if (gadget->callbackDown) (*gadget->callbackDown)(gadget);
	return 1;
      }
    }
  }
  return 0;
}

static void CreateChoiceWindow(McGadget *gadget) {
  McChoice *choice = (McChoice *) gadget->specialInfo;
  XSetWindowAttributes values;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  int px, py, dummy;

  if (choice->data) {
    XQueryPointer(app->display, RootWindow(app->display, app->screen),
		  (Window *)&dummy, (Window *)&dummy,
		  &px, &py, &dummy, &dummy, &dummy);

    choice->win_w=choice->width+(BW<<2);
    choice->win_h=choice->height*choice->size+(BW<<1)+((app->style)?1:0);
    choice->win_x=px-((choice->win_w)>>1);
    choice->win_y=py+BW+BW-((choice->selection+1)*choice->height);

    /* Avoid popping up off screen */
    if (choice->win_y>=0) {
      choice->win_selection=choice->selection;
    } else {
      choice->win_selection=(py-BW) / choice->height;
      if (choice->win_selection<0) choice->win_selection=0; /*should't happen*/
      choice->win_y=0;
    }

    choice->mcw=McCreateAppWindow(mcw->app, choice->win_x, choice->win_y,
				  choice->win_w, choice->win_h, NULL, 
				  ChoiceWindowEvent);
    choice->mcw->customData = gadget;
  
    values.save_under = values.override_redirect = True;
    XChangeWindowAttributes(app->display, choice->mcw->clientwin,
			    CWSaveUnder | CWOverrideRedirect, &values);
    XMapRaised(app->display, choice->mcw->clientwin);
#ifndef DBG_NOGRAB
    { Cursor cursor;
      cursor=XCreateFontCursor(app->display, XC_left_ptr);
      XGrabPointer(app->display, choice->mcw->clientwin, False,
		   ButtonPressMask | ButtonReleaseMask | ButtonMotionMask |
		   PointerMotionMask, GrabModeAsync, GrabModeAsync, None,
		   cursor, CurrentTime);
    }
#endif
    McInitGadgets(choice->mcw);
    RedrawChoiceWindow(choice);
  }
}

static void RedrawChoiceWindow(McChoice *choice) {
  McWindow *mcw = choice->mcw;
  McApp *app = mcw->app;
  McAppDrawbox(mcw, mcw->clientwin,
	       BW, BW, choice->win_w-BW-BW, choice->win_h-BW-BW, _3D_OUT);
  if (choice->data) {
    int i,x,y,w,h;
    GC gc1 = XCreateGC(app->display, mcw->clientwin, 0, NULL);
    GC gc2 = XCreateGC(app->display, mcw->clientwin, 0, NULL);
    XCopyGC(app->display, app->gc[GC_NORMAL], ~0, gc1);
    XSetFont(app->display, gc1, choice->font->fid);
    XCopyGC(app->display, app->gc[GC_SELECTED_BITMAP], ~0, gc2);
    XSetFont(app->display, gc2, choice->font->fid);

    for (i=0; i<choice->size; i++) {
      const unsigned char *data=choice->data[i];
      x=BW+BW;
      y=BW+BW+choice->height*i;
      w=choice->width;
      h=choice->height-BW-BW;
      if (i==choice->win_selection) {
	if (!app->style) {
	  XFillRectangle(app->display, mcw->clientwin, app->gc[GC_SELECTED],
			 x, y, w, h);
	  McAppDrawbox(mcw, mcw->clientwin,
		       x, y, w, h, _3D_IN);
	} else {
	  XSetForeground(app->display, gc2, app->colors[COL_MENU]);
	  XFillRectangle(app->display, mcw->clientwin, gc2,
			 x-BW, y-BW, w+BW+BW-1, h+BW+BW);
	  XSetForeground(app->display, gc2, app->colors[COL_FOREGROUND]);
	  XSetBackground(app->display, gc2, app->colors[COL_MENU]);
	}
	XDrawImageString(app->display, mcw->clientwin, gc2,
			 x, choice->font->ascent+y, data, strlen(data));
      } else {
	XFillRectangle(app->display, mcw->clientwin, app->gc[GC_CLEAR],
		       x-BW, y-BW, w+BW+BW, h+BW+BW);
	XDrawImageString(app->display, mcw->clientwin, gc1,
			 x, choice->font->ascent+y, data, strlen(data));
      }
    }     
    XFreeGC(app->display, gc1);
    XFreeGC(app->display, gc2);
  }
}

static int ChoiceWindowSelect(McChoice *choice, int selection) {
  int x,y,w,h;
  GC gc1, gc2;
  const unsigned char *data;
  McWindow *mcw;
  McApp *app;
  
  if ((!choice->data) || (selection==choice->win_selection)) return 0;

  mcw = choice->mcw;
  app = mcw->app;
  gc1 = XCreateGC(app->display, mcw->clientwin, 0, NULL);
  gc2 = XCreateGC(app->display, mcw->clientwin, 0, NULL);
  XCopyGC(app->display, app->gc[GC_NORMAL], ~0, gc1);
  XSetFont(app->display, gc1, choice->font->fid);
  XCopyGC(app->display, app->gc[GC_SELECTED_BITMAP], ~0, gc2);
  XSetFont(app->display, gc2, choice->font->fid);

  w=choice->width;
  h=choice->height-BW-BW;
  x=BW+BW; 

  data=choice->data[choice->win_selection];
  y=BW+BW+choice->height*choice->win_selection;
  XFillRectangle(app->display, mcw->clientwin, app->gc[GC_CLEAR],
		 x-BW, y-BW, w+BW+BW, h+BW+BW);
  XDrawImageString(app->display, mcw->clientwin, gc1,
		   x, choice->font->ascent+y, data, strlen(data));

  y=BW+BW+choice->height*selection;
  data=choice->data[selection];
  if (!app->style) {
    XFillRectangle(app->display, mcw->clientwin, app->gc[GC_SELECTED],
		   x, y, w, h);
    McAppDrawbox(mcw, mcw->clientwin,
		 x, y, w, h, _3D_IN);
  } else {
    XSetForeground(app->display, gc2, app->colors[COL_MENU]);
    XFillRectangle(app->display, mcw->clientwin, gc2,
		   x-BW,y-BW,w+BW+BW-1,h+BW+BW);
    XSetForeground(app->display, gc2, app->colors[COL_FOREGROUND]);
    XSetBackground(app->display, gc2, app->colors[COL_MENU]);
  }
  XDrawImageString(app->display, mcw->clientwin, gc2,
		   x, choice->font->ascent+y, data, strlen(data));

  choice->win_selection=selection;

  XFreeGC(app->display, gc1);
  XFreeGC(app->display, gc2);

  return 1;
}

static void FreeChoiceWindow(McGadget *gadget) {
  McChoice *choice = (McChoice *) gadget->specialInfo;
  McWindow *mcw=gadget->mcw;

#ifndef DBG_NOGRAB
  XUngrabPointer(mcw->app->display, CurrentTime);
  XFlush(mcw->app->display); /* Be sure the pointer is already free in case */
			     /* the application crashes right after this.   */
#endif

  if (choice->mcw) {
    McFreeWindow(choice->mcw);
    choice->mcw=NULL;
  }
}

const unsigned char *McChoiceSelectionString(McGadget *gadget) {
  McChoice *choice = (McChoice *) gadget->specialInfo;
  if ((choice->data) && (choice->selection>=0))
    return choice->data[choice->selection];
  else
    return NULL;
}

unsigned int McChoiceSelection(McGadget *gadget) {
  McChoice *choice = (McChoice *) gadget->specialInfo;
  if ((choice->data) && (choice->selection>=0))
    return choice->selection;
  else
    return -1;
}

/***************************************************************************/

static int ChoiceKey(McGadget *gadget, XKeyEvent *event) {
  int dir=0;
  McWindow *mcw=gadget->mcw;

  switch((int)mcw->keysym) {
  case XK_Up:
    dir=-1;
    break;

  case XK_Down:
    dir=1;
    break;
  }

  if (dir) {
    int new=CHOICE->selection+dir;
    if (new>=CHOICE->size) new=0; else if (new<0) new=CHOICE->size-1;
    McChoiceSelect(gadget, new);
    if (gadget->callbackUp) (*gadget->callbackUp)(gadget);
    return 1;
  }

  return 0;
}

/***************************************************************************/

McGadget *MakeChoice(McWindow *mcw, int x, int y, int w, int h, int id,
		     void (*callbackUp)(struct McGadget *),
		     void (*callbackDown)(struct McGadget *)) {
  McGadget *gadget;

  if (h<=0) h=mcw->app->defaultFont->ascent+mcw->app->defaultFont->descent+3;
  if (w<=0) w=11*BW;
  if (x==0) x=(mcw->w>>1)-(w>>1);	/* Center */
  if (y==0) x=(mcw->h>>1)-(h>>1);
  if (x<0) x=mcw->w+x-w;		/* Right justify */
  if (y<0) x=mcw->h+y-h;

  gadget = McCreateGadget(mcw, GAD_H3D|GAD_3D|GAD_ACTIVE, CHOICEGADGET,
			  x, y, w, h);
  gadget->specialInfo=McCreateChoice(CH_AUTORESIZE|CH_AUTORECENTER,
				     mcw->app->defaultFont, NULL, 0);

  ((McChoice *)gadget->specialInfo)->fonty =
    (h-BW-(mcw->app->defaultFont->ascent+mcw->app->defaultFont->descent))
      >>1;

  gadget->callbackUp = callbackUp;
  gadget->callbackDown = callbackDown;
  gadget->id = id;
  return gadget;
}

