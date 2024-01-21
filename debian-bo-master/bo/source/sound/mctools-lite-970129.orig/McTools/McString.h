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

#ifndef _McString_h_
#define _McString_h_

#define CURSOR_VISIBLE 1

typedef struct McString {
  McSpecialInfo specialInfo;
  unsigned int flags;
  XFontStruct *font;
  unsigned char *buf;
  int bufsize, len, oldlen, cursor, oldcursor, offset, cursorWidth;
  int cursor_end, oldcursor_end, cursor_click;
  void	(*callbackChange)(struct McGadget *);
} McString;

extern McSpecialInfo *McCreateString(unsigned int flags, XFontStruct *font,
				     unsigned char *text, int bufsize);
extern void McStringSetText(McGadget *gadget, unsigned char *text);

extern McGadget *MakeString(McWindow *mcw, int x, int y, int w, int h,
			    int id,string_t label,
			    void (*callbackUp)(struct McGadget *),
			    void (*callbackDown)(struct McGadget *));

#define McStringString(gad) (((McString *)((gad)->specialInfo))->buf)

#endif /* _McString_h_ */

