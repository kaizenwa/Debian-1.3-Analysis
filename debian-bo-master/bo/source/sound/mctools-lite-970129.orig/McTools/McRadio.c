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
#include "McRadio.h"

/**************************************************************************/

static unsigned char radio_black_off_bits[] = {
  0x00, 0x00, 0xf0, 0x00, 0x0c, 0x03, 0x04, 0x00, 0x02, 0x00, 0x02, 0x00,
  0x02, 0x00, 0x02, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
};

static unsigned char radio_black_on_bits[] = {
  0x00, 0x00, 0xf0, 0x00, 0x0c, 0x03, 0x04, 0x00, 0x62, 0x00, 0xf2, 0x00,
  0xf2, 0x00, 0x62, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
};

static unsigned char radio_dark_bits[] = {
  0xf0, 0x00, 0x0c, 0x03, 0x02, 0x00, 0x02, 0x00, 0x01, 0x00, 0x01, 0x00,
  0x01, 0x00, 0x01, 0x00, 0x02, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00,
};

static unsigned char radio_bright_on_bits[] = {
  0x00, 0x00, 0x00, 0x00, 0xf0, 0x04, 0xf8, 0x05, 0x9c, 0x0b, 0x0c, 0x0b,
  0x0c, 0x0b, 0x9c, 0x0b, 0xf8, 0x05, 0xf0, 0x04, 0x0c, 0x03, 0xf0, 0x00,
};

static unsigned char radio_bright_off_bits[] = {
  0x00, 0x00, 0x00, 0x00, 0xf0, 0x04, 0xf8, 0x05, 0xfc, 0x0b, 0xfc, 0x0b,
  0xfc, 0x0b, 0xfc, 0x0b, 0xf8, 0x05, 0xf0, 0x04, 0x0c, 0x03, 0xf0, 0x00,
};

/**************************************************************************/

#define MAKEPIX(x) \
  x=XCreateBitmapFromData(app->display, RootWindow(app->display, app->screen),\
			  x##_bits, 12, 12)
#define FREEPIX(x) XFreePixmap(app->display, x);

static void MakePixmaps(McApp *app) {
  XGCValues values;
  GC gc;
  Pixmap radio_black_on,  radio_black_off;
  Pixmap radio_bright_on, radio_bright_off;
  Pixmap radio_dark, on, off;

  if (app->radio_on) return;

  on=XCreatePixmap(app->display, RootWindow(app->display, app->screen),
		   12, 12, DefaultDepth(app->display, app->screen));
  off=XCreatePixmap(app->display, RootWindow(app->display, app->screen),
		    12, 12, DefaultDepth(app->display, app->screen));
  XFillRectangle(app->display, on, app->gc[GC_CLEAR], 0, 0, 12, 12);
  XFillRectangle(app->display, off, app->gc[GC_CLEAR], 0, 0, 12, 12);
  app->radio_on = on;
  app->radio_off = off;

  MAKEPIX(radio_black_on);
  MAKEPIX(radio_black_off);
  MAKEPIX(radio_bright_on);
  MAKEPIX(radio_bright_off);
  MAKEPIX(radio_dark);

  values.stipple=radio_black_on;
  values.foreground=app->colors[COL_FOREGROUND];
  values.fill_style=FillStippled;
  gc=XCreateGC(app->display, RootWindow(app->display, app->screen),
	       GCForeground|GCStipple|GCFillStyle,
	       &values);
  XFillRectangle(app->display, on, gc, 0, 0, 12, 12);

  values.stipple=radio_black_off;
  XChangeGC(app->display, gc, GCStipple, &values);
  XFillRectangle(app->display, off, gc, 0, 0, 12, 12);

  values.stipple=radio_bright_on;
  values.foreground=app->colors[COL_BRIGHT];
  XChangeGC(app->display, gc, GCForeground|GCStipple, &values);
  XFillRectangle(app->display, on, gc, 0, 0, 12, 12);

  values.stipple=radio_bright_off;
  XChangeGC(app->display, gc, GCStipple, &values);
  XFillRectangle(app->display, off, gc, 0, 0, 12, 12);

  values.stipple=radio_dark;
  values.foreground=app->colors[COL_DARK];
  XChangeGC(app->display, gc, GCForeground|GCStipple, &values);
  XFillRectangle(app->display, on, gc, 0, 0, 12, 12);
  XFillRectangle(app->display, off, gc, 0, 0, 12, 12);

  XFreeGC(app->display, gc);

  FREEPIX(radio_black_on);
  FREEPIX(radio_black_off);
  FREEPIX(radio_bright_on);
  FREEPIX(radio_bright_off);
  FREEPIX(radio_dark);
}

/**************************************************************************/

McSpecialInfo *McCreateRadio(McApp *app) {
  McRadio *Radio = (McRadio *) calloc(sizeof(McRadio), 1);
  Radio->specialInfo.updateProc = McRadioUpdate;
  MakePixmaps(app);
  return (McSpecialInfo *)Radio;
}

/**************************************************************************/

void McRadioUpdate(McGadget *gadget, int busy, int all) {
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  Pixmap pix;

  if (gadget->flags&GAD_SELECTED) pix=app->radio_on; else pix=app->radio_off;

  XCopyArea(app->display, pix, gadget->win, app->gc[GC_NORMAL],
	    0, 0, 12, 12, 2, (gadget->height-12)>>1);

  McDrawFocusMark(gadget, COL_BACKGROUND);
}

/**************************************************************************/

McGadget *MakeRadio(McWindow *mcw, int x, int y, int w, int h,
		    int id, unsigned long mutual_exclude,
		    const char *str, void (*callback)(struct McGadget *)) {

  McGadget *gadget;
  McText *txt=NULL;

  if (str)
    txt = McCreateText(mcw, (unsigned char *)str, mcw->app->gc[GC_NORMAL],
		       mcw->app->defaultFont, 16, 0);
  if (w<=0) w=16+(txt?txt->width:5)+(BW<<2);
  if (h<=0) h=(txt?txt->height:5)+BW+BW;

  if (x==0) x=(mcw->w>>1)-(w>>1);	/* Center */
  if (y==0) y=(mcw->h>>1)-(h>>1);
  if (x<0) x=mcw->w+x-w-BW-BW-BW;	/* Right justify */
  if (y<0) y=mcw->h+y-h-BW-BW-BW;

  gadget = McCreateGadget(mcw,
			  GAD_3D|GAD_HNONE|GAD_ACTIVE|GAD_NOFILL|GAD_NOSTIPPLE,
			  RADIOGADGET, x, y, w, h);
  if (txt) txt->y = ((gadget->height - txt->height)>>1);
  gadget->mutualExclude=mutual_exclude;
  gadget->normalLabel = txt;
  gadget->specialInfo = McCreateRadio(mcw->app);
  gadget->callbackUp = callback;
  return gadget;
}


