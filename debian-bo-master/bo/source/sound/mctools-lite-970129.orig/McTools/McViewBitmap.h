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

#ifndef _McViewBitmap_h_
#define _McViewBitmap_h_

typedef struct McViewBitmap {
  McSpecialInfo specialInfo;
  McGadget *sliderhor, *sliderver;
  unsigned int flags;
  int home_x, home_y, size_x, size_y;
  Pixmap pixmap;
  int depth;
} McViewBitmap;

extern McSpecialInfo *McCreateViewBitmap(unsigned int flags);
extern void McViewBitmapUpdate(McGadget *gadget, int busy, int all);
extern McGadget *MakeViewBitmap(McWindow *mcw, int x, int y, int w, int h);
extern void McViewBitmapBindSliders(McGadget *timgad,
				    McGadget *sliderhor, McGadget *sliderver);
extern void McViewBitmapSetPixmap(McGadget *gadget, Pixmap pix, int depth,
				  int size_x, int size_y);
extern void McViewBitmapSetHome(McGadget *gadget, int home_x, int home_y);

#endif /* _McViewBitmap_h_ */


