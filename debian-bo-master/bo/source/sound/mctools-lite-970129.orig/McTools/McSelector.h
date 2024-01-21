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

#ifndef _McSelector_h_
#define _McSelector_h_

#define SEL_DOUBLE_CLICK 1

typedef struct McSelector {
  McSpecialInfo specialInfo;
  unsigned int flags;
  XFontStruct *font;
  const unsigned char *const *data;
  int height, selection, first, oldselection, size;
  Time time; /* To detect double clicks */
  McGadget *slidergad;
} McSelector;

extern McSpecialInfo *McCreateSelector(unsigned int flags, XFontStruct *font,
				       const unsigned char *const *const data,
				       int size);
extern int McSelectorSelect(McGadget *gadget, int selection);
extern void McSelectorSetList(McGadget *gadget,
			      const unsigned char *const *const data,
			      int size, int selection);
extern int McSelectorWidth(McGadget *gadget);
extern int McSelectorHeight(McGadget *gadget);
extern void McSelectorUpdate(McGadget *gadget,int busy,int all);
extern const unsigned char *McSelectorSelectionString(McGadget *gadget);
extern int McSelectorSelection(McGadget *gadget);
extern void McSelectorRefreshLine(McGadget *gadget, int line);
extern void McSelectorBindSlider(McGadget *selgad, McGadget *slidergad);
#define McSelectUnbindSlider(selgad) McSelectorBindSlider(selgad, NULL);

extern McGadget *MakeSelector(McWindow *mcw, int x, int y, int w, int h,int id,
			      XFontStruct *font,
			      void (*callbackUp)(struct McGadget *),
			      void (*callbackDown)(struct McGadget *));

#endif /* _McSelector_h_ */
