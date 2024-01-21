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

#ifndef _McSlider_h_
#define _McSlider_h_

/* Slider flags */
#define SLIDER_MONO		0
#define SLIDER_STEREO		1
#define SLIDER_SET_LEFT		2
#define SLIDER_SET_RIGHT	4
#define SLIDER_SET_MASK		6
#define SLIDER_HORIZONTAL	0
#define SLIDER_VERTICAL		8
#define SLIDER_VOLUME		16

#define SLIDER ((McSlider *)(gadget->specialInfo))

typedef struct McSlider {
  McSpecialInfo specialInfo;
  unsigned int flags;
  int leftValue, rightValue;
  int maxValue;
  int snapLeft, snapRight;
  int step, width, pixelWidth, snapCrsr;
  int leftPixels, rightPixels, maxPixels;
} McSlider;

extern McSpecialInfo *McCreateSlider(unsigned int flags,unsigned int maxValue);
void McSliderSetProps(McGadget *gadget, int max, int width);

extern McGadget *MakeProp(McWindow *mcw, int x, int y, int w, int h, int id,
			  void (*callback)(struct McGadget *));

extern int McShiftSlider(McGadget *gadget, int dir);

#endif /* _McSlider_h_ */
