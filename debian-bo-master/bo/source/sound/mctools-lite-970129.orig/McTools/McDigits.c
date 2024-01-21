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

#include "McApp.h"
#include "McDigits.h"
#include "McBitmap.h"
#include "MicroDigits.h"

McBitmap dig[] = {
    MCBITMAP(dig0_bits, dig_width, dig_height, 0, 0),
    MCBITMAP(dig1_bits, dig_width, dig_height, 0, 0),
    MCBITMAP(dig2_bits, dig_width, dig_height, 0, 0),
    MCBITMAP(dig3_bits, dig_width, dig_height, 0, 0),
    MCBITMAP(dig4_bits, dig_width, dig_height, 0, 0),
    MCBITMAP(dig5_bits, dig_width, dig_height, 0, 0),
    MCBITMAP(dig6_bits, dig_width, dig_height, 0, 0),
    MCBITMAP(dig7_bits, dig_width, dig_height, 0, 0),
    MCBITMAP(dig8_bits, dig_width, dig_height, 0, 0),
    MCBITMAP(dig9_bits, dig_width, dig_height, 0, 0),

    MCBITMAP(icn0_bits, icn_width, icn_height, 1, 0),
    MCBITMAP(icn1_bits, icn_width, icn_height, 0, 0),
    MCBITMAP(icn2_bits, icn_width, icn_height, 1, 0),
    MCBITMAP(icn3_bits, icn_width, icn_height, 1, 0),
  };



void McSetupDigits(McWindow *mcw) {
  int i;
  if (dig[0].pixmap<=0)
    for (i=14; --i>=0;)
      McCreateBitmapFromData(mcw, &dig[i]);
}

void McPutNumber(McWindow *mcw, Window win, GC gc, int n, int x, int y) {
  int z,e;

  McSetupDigits(mcw);

  if (n>=DIG_START) {
    n-=DIG_START-10;
    XCopyPlane(mcw->app->display, dig[n].pixmap, win, gc, 0, 0,
	       dig[n].width, dig[n].height, x+dig[n].x, y+dig[n].y, 1);
  } else {
    z=n/10;
    e=n%10;
    if (z>9) { z=9; e=9; }
    if (z) {
      XCopyPlane(mcw->app->display, dig[z].pixmap, win, gc, 0, 0,
		 dig[z].width, dig[z].height, x+dig[z].x, y+dig[z].y, 1);
      XCopyPlane(mcw->app->display, dig[e].pixmap, win, gc, 0, 0,
		 dig[e].width, dig[e].height, x+dig[e].x+4, y+dig[e].y, 1);
    } else {
      XCopyPlane(mcw->app->display, dig[e].pixmap, win, gc, 0, 0,
		 dig[e].width, dig[e].height, x+dig[e].x+3, y+dig[e].y, 1);
    }
  }
}
