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

#ifndef _McViewText_h_
#define _McViewText_h_

typedef struct McViewLine {
  unsigned char *text;
  int len;
  short width, height;
} McViewLine;

typedef struct McViewText {
  McSpecialInfo specialInfo;
  McGadget *sliderhor, *sliderver;
  XFontStruct *font;
  unsigned int flags;
  unsigned char *text;
  McViewLine *lines;
  int linecnt, linemax;
  int maxwidth, maxheight;
  int firstrow, firstcol;
  struct {
    int rowstart, rowend, rowclick;
    int colstart, colend, colclick;
  } last;
  struct {
    int rowstart, rowend, rowclick;
    int colstart, colend, colclick;
  } now;
} McViewText;

extern McSpecialInfo *McCreateViewText(unsigned int flags, XFontStruct *font,
				       unsigned char *txt);
extern void McViewTextUpdate(McGadget *gadget,int busy,int all);
extern McGadget *MakeViewText(McWindow *mcw, int x, int y, int w, int h,
			      XFontStruct *font, char *text);
extern void McViewTextBindSliders(McGadget *timgad,
				  McGadget *sliderhor, McGadget *sliderver);
extern void McViewTextSetText(McGadget *gadget, unsigned char *text);

#endif /* _McViewText_h_ */
