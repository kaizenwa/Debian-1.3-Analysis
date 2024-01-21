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
#include <string.h>

#include "McApp.h"
#include "McGadget.h"
#include "McSlider.h"
#include "McText.h"
#include "McBitmap.h"
#include "McString.h"
#include "McSelector.h"
#include "McResource.h"

#include "McUtils.h"

McGadget *MakeButton(McWindow *mcw, int x, int y, int w, int h,
		     int id, string_t label,
		     void (*callback)(struct McGadget *)) {
  McGadget *gadget;
  McText *txt;

  txt = McCreateText(mcw,label, mcw->app->gc[GC_NORMAL],
		     mcw->app->defaultFont, 0, 0);
  if (!w) w=txt->width+(BW<<2);
  if (!h) h=txt->height+BW+BW;
  if (x<0) x=mcw->w+x-w;
  if (y<0) y=mcw->h+y-h;

  if (x==0) x=(mcw->w>>1) - (w>>1); /* Center */
  if (y==0) y=(mcw->h>>1) - (h>>1); /* Center */

  gadget = McCreateGadget(mcw, GAD_H3D | GAD_3D | GAD_ACTIVE,BOOLGADGET,
			  x, y, w, h);
  gadget->normalLabel = txt;
  txt->x = (gadget->width  - txt->width )>>1;
  txt->y = (gadget->height - txt->height)>>1;
  gadget->callbackUp = callback;
  gadget->id = id;
  return gadget;
}

McGadget *MakeMessage(McWindow *mcw, int x, int y, int w, int h,
		      int id, string_t label) {
  McGadget *gadget;
  McText *txt;

  txt = McCreateText(mcw,label, mcw->app->gc[GC_NORMAL],
		     mcw->app->defaultFont, 0, 0);
  if (!w) w=txt->width+(BW<<2);
  if (!h) h=txt->height+BW+BW;
  if (x<0) x=mcw->w+x-w;
  if (y<0) x=mcw->h+y-h;
  gadget = McCreateGadget(mcw, GAD_H3D | GAD_3D | GAD_SELECTED, BOOLGADGET,
			  x, y, w, h);
  gadget->normalLabel = txt;
  txt->x = (gadget->width  - txt->width )>>1;
  txt->y = (gadget->height - txt->height)>>1;
  gadget->id = id;
  return gadget;
}

McGadget *MakeRText(McWindow *mcw, int x, int y, int id, string_t label) {
  McGadget *gadget;
  McText *txt;

  txt = McCreateText(mcw,label, mcw->app->gc[GC_NORMAL],
		     mcw->app->defaultFont,
		     0, mcw->app->defaultFont->descent-3);
  if (x<0) x=mcw->w+x-txt->width;
  if (y<0) x=mcw->h+y-txt->height;
  x = x - txt->width;
  gadget = McCreateGadget(mcw, GAD_HNONE, BOOLGADGET,
			  x, y, txt->width, txt->height);
  gadget->normalLabel = txt;
  gadget->id = id;
  return gadget;
}

void MakeTexts(McWindow *mcw, int x, int y, McLine *line, int lines) {
  McGadget *gad;
  int i;
  for(i=0; i<lines; i++) {
    if (line[i].data && (*(line[i].data))) {
      switch(line[i].orientation) {
      case MCTXT_CENTERED:
	gad=MakeText(mcw, 0, y, 0, line[i].data);
	gad->x=x+(((mcw->w)>>1) - ((line[i].width)>>1));
	break;
      case MCTXT_FLUSHLEFT:
	MakeText(mcw, x-1+line[i].offset, y, 0, line[i].data);
	break;
      case MCTXT_FLUSHRIGHT:
	MakeText(mcw, mcw->w-line[i].width-x, y, 0, line[i].data);
	break;
      }
    }
    y+=line[i].height;
  }
}

McGadget *MakeText(McWindow *mcw, int x, int y, int id, string_t label) {
  McGadget *gadget;
  McText *txt;

  txt = McCreateText(mcw,label, mcw->app->gc[GC_NORMAL],
		     mcw->app->defaultFont,
		     0, mcw->app->defaultFont->descent-3);
  if (x<0) x=mcw->w+x-txt->width;
  if (y<0) x=mcw->h+y-txt->height;
  if (x==0) x=(mcw->w>>1)-(txt->width>>1);	/* Center */
  if (y==0) x=(mcw->h>>1)-(txt->height>>1);

  gadget = McCreateGadget(mcw, GAD_HNONE, BOOLGADGET,
			  x, y, txt->width, txt->height);
  gadget->normalLabel = txt;
  gadget->id = id;
  return gadget;
}








