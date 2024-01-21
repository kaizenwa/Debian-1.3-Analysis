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

#ifndef _McMenubar_h_
#define _McMenubar_h_

#include "./McMenu.h"

struct McMenubarInit;
typedef struct McMenubar {
  McSpecialInfo specialInfo;
  McHotkeyHandler hotkeyHandler;
  McWindow *mcw;
  unsigned int flags;
  struct McMenubarInit *items;
  McMenuList **menu;
  McMenu *currentMenu;
  XFontStruct *font;
  int size, current, lastcurrent;
  int *widths;
  int width; /* width of all menus */
  int height; /* height of text only, without etched line below */
  unsigned char **titles;
} McMenubar;

typedef struct McMenubarInit {
  unsigned char *title;
  McMenuInit *items;
  int count;
} McMenubarInit;

#define MENUBARSIZE(x) (sizeof(x)/sizeof(struct McMenubarInit))

McSpecialInfo *McCreateMenubar(McWindow *, unsigned int flags,
			       XFontStruct *font,
			       McMenubarInit *items, int size,
			       void (*callback)(int, struct McMenuItem *));
extern void McInstallMenubar(McGadget *gadget);

extern void McMenubarUpdate(McGadget *gadget,int busy,int all);
extern McGadget *MakeMenubar(McWindow *mcw, McMenubarInit *items, int size,
			     void (*callback)(int, struct McMenuItem *),
			     int width);
extern struct McMenuItem *McMenubarMenu(McGadget *gadget, int num);
extern struct McMenuList *McMenubarList(McGadget *gadget, int num);

#endif /* _McMenubar_h_ */

