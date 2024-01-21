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

#ifndef _McChoice_h_
#define _McChoice_h_

typedef struct McChoice {
  McSpecialInfo specialInfo;
  unsigned int flags;
  const unsigned char *const *data;
  XFontStruct *font;
  short selection, size, height, width, oldtxtwidth, fonty;
  McWindow *mcw;
  short win_w, win_h, win_x, win_y, win_selection;
  short center;
} McChoice;

#define CH_AUTORESIZE	    1	/* Gadget will resize itself to its text     */
#define CH_AUTORECENTER	    2	/* Gadget will recenter itself to its text   */

extern McSpecialInfo *McCreateChoice(unsigned int flags, XFontStruct *font,
				     const unsigned char *const *const data,
				     int size);
extern int McChoiceSelect(McGadget *gadget, int selection);
extern void McChoiceSetList(McGadget *gadget,
			    const unsigned char *const *const data, int size);
extern void McChoiceUpdate(McGadget *gadget, int busy,int all);
extern const unsigned char *McChoiceSelectionString(McGadget *gadget);
extern unsigned int McChoiceSelection(McGadget *gadget);
McGadget *MakeChoice(McWindow *mcw, int x, int y, int w, int h, int id,
		     void (*callbackUp)(struct McGadget *),
		     void (*callbackDown)(struct McGadget *));

#endif /* _McChoice_h_ */
