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
#include "McApp.h"
#include "McGadget.h"
#include "McCheckBox.h"

/**************************************************************************/

static unsigned char checkbox_bits[] = {
  0x63, 0x77, 0x3e, 0x1c, 0x3e, 0x77, 0x63,
};

#define MAKEPIX(x) \
  x=XCreateBitmapFromData(app->display, RootWindow(app->display, app->screen),\
			  x##_bits, 12, 12)

static void MakePixmaps(McApp *app) {

  if (app->checkbox_on) return;

  app->checkbox_on=
    XCreateBitmapFromData(app->display, RootWindow(app->display, app->screen),
			  checkbox_bits, 7, 7);
}

/**************************************************************************/

McSpecialInfo *McCreateCheckBox(McApp *app) {
  McCheckBox *CheckBox = (McCheckBox *) calloc(sizeof(McCheckBox), 1);
  CheckBox->specialInfo.updateProc = McCheckBoxUpdate;
  MakePixmaps(app);
  return (McSpecialInfo *)CheckBox;
}

/**************************************************************************/

void McCheckBoxUpdate(McGadget *gadget, int busy, int all) {
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  int y=(gadget->height-12)>>1;

  if (gadget->flags & GAD_3D)
    McAppDrawbox(mcw, gadget->win, BW+2, BW+y, 9, 9, _3D_IN);
  else
    XDrawRectangle(app->display, gadget->win, app->gc[GC_NORMAL],
		   3, y+1, 10, 10);

  XFillRectangle(app->display, gadget->win, app->gc[GC_BRIGHT],
		 BW+2, y+BW, 9, 9);

  if (gadget->flags&GAD_SELECTED) {
    XSetBackground(app->display, app->gc[GC_NORMAL],
		   app->colors[COL_BRIGHT]);
    XCopyPlane(app->display, app->checkbox_on, gadget->win, app->gc[GC_NORMAL],
	      0, 0, 7, 7, BW+3, y+BW+1, 1);
    XSetBackground(app->display, app->gc[GC_NORMAL],
		   app->colors[COL_BACKGROUND]);
  }
  McDrawFocusMark(gadget, COL_BACKGROUND);
}

/**************************************************************************/

McGadget *MakeCheckBox(McWindow *mcw, int x, int y, int w, int h,
		    int id,
		    const char *str, void (*callback)(struct McGadget *)) {

  McGadget *gadget;
  McText *txt;

  if (!str) str="CheckBox";
  txt = McCreateText(mcw, (unsigned char *)str, mcw->app->gc[GC_NORMAL],
		     mcw->app->defaultFont, 16, 0);
  if (w<=0) w=16+txt->width+(BW<<2);
  if (h<=0) h=txt->height+BW+BW;

  if (x==0) x=(mcw->w>>1)-(w>>1);	/* Center */
  if (y==0) y=(mcw->h>>1)-(h>>1);
  if (x<0) x=mcw->w+x-w-BW-BW-BW;	/* Right justify */
  if (y<0) y=mcw->h+y-h-BW-BW-BW;

  gadget = McCreateGadget(mcw,
			  GAD_3D | GAD_HNONE | GAD_ACTIVE | GAD_NOFILL|
			  GAD_NOSTIPPLE | GAD_TOGGLE,
			  CHECKBOXGADGET, x, y, w, h);
  txt->y = ((gadget->height - txt->height)>>1);
  gadget->normalLabel = txt;
  gadget->specialInfo = McCreateCheckBox(mcw->app);
  gadget->callbackUp = callback;
  return gadget;
}


