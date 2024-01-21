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

#ifndef _McText_h_
#define _McText_h_

typedef struct McText {
  struct McText *next;
  int x,y,width,height;
  XFontStruct *font;
  GC gc;
  string_t text;
} McText;

enum { MCTXT_CENTERED, MCTXT_FLUSHLEFT, MCTXT_FLUSHRIGHT, };

typedef struct McLine {
  int width, height;
  int len;
  int orientation;
  int offset;
  unsigned char *data;
} McLine;

extern int McCalculateText(XFontStruct *font, unsigned char *text,
			   int *lines, int *maxwidth, struct McLine *line);

extern McText *McCreateText(McWindow *mcw, const string_t text, GC gc,
			    XFontStruct *font, int x, int y);
extern void McChangeText(McWindow *mcw, McText *txt, const string_t text,
			 GC gc);
extern void McFreeText(McText *txt);
void McWriteText(McWindow *mcw, Window win, McText *txt, GC gc, int x, int y);

#endif /* _McText_h_ */
