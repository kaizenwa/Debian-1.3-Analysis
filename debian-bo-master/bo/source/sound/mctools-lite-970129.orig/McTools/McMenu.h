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

#ifndef _McMenu_h_
#define _McMenu_h_

#include <X11/Xlib.h>
#include "McApp.h"

#define MENU_CENTER		   1
#define MENU_ISDRAWN		   2
#define MENU_HOTKEY		   4

#define ITEM_ENABLED		   1
#define ITEM_TOGGLE		   2  /* Toggle status on each selection */
#define ITEM_CHECKED		   4
#define ITEM_HIGH		   8
#define ITEM_HNONE		   0
#define ITEM_H3D		  16
#define ITEM_HBOX		  32
#define ITEM_LINE		  64
#define ITEM_DOUBLE_LINE	 128
#define ITEM_CENTER		 256
#define ITEM_CHECKIT		 512
#define ITEM_UPDATE		1024  /* Item needs to be redrawn */
#define ITEM_GRAY		2048  /* Item is drawn stippled gray */
#define ITEM_GREY		ITEM_GRAY /* Which is which? */

struct McMenuItem;
struct McMenuList;

typedef struct McMenu {
  int x,y;
  struct McMenuList *list;
  McWindow *mcw;
  McGadget *gadget;
  McGadget *starter;
  struct McMenu *child;
  struct McMenu *parent;
  struct McMenuItem *subMenuItem;
} McMenu;

typedef struct McMenuGadget {
  McSpecialInfo specialInfo;
  McMenu *menu;
} McMenuGadget;


typedef struct McMenuList {
  int flags;
  int w,h;
  McWindow *mcw;
  struct McMenuItem *first;
  void	(*callback)(int id, struct McMenuItem *item);
  McHotkeyHandler hotkeyHandler;
} McMenuList;

typedef struct McMenuItem {
  struct McMenuItem *next;
  struct McMenuItem *prev;
  int flags;
  int x,y,w,h;
  int id;
  unsigned long mutual_exclude;
  void *customData;
  struct McBitmap *normalBitmap;
  struct McBitmap *selectBitmap;
  struct McText *normalLabel;
  struct McText *selectLabel;
  struct McBitmap *checkMark;
  struct McMenuList *subMenu;
  void	(*callback)(int id, struct McMenuItem *);
  unsigned char hotkey;
} McMenuItem;

typedef struct McMenuInit {
  unsigned char *text;
  int flags;
  int id;
  void	(*callback)(int id, struct McMenuItem *);
  struct McMenuInit *sub;
  int nsub;
  unsigned char hotkey;
} McMenuInit;

#define MENUTITLE(text) { text, ITEM_CENTER, 0, NULL, NULL, 0 }
#define MENUITEM(text,hk,flags,id) { text, flags, id, NULL, NULL, 0, hk }
#define MENULINE { NULL, ITEM_LINE, 0, NULL, NULL, 0 }
#define MENUDOUBLELINE { NULL, ITEM_DOUBLE_LINE, 0, NULL, NULL, 0 }
#define SUBMENUITEM(text,flags,id,sm) \
				 { text, flags, id, NULL, sm,MENU_ITEMS(sm),0 }
#define MENUITEMCB(text,hk,flags,id,cb) { text, flags, id, cb, NULL, 0, hk }
#define SUBMENUITEMCB(text,flags,id,sm,cb) \
				 { text, flags, id, cb, sm, MENU_ITEMS(sm), 0 }
#define MENU_ITEMS(i) (sizeof(i)/sizeof(McMenuInit))

extern McMenuList *McCreateMenu(McWindow *mcw,
				void (*callback)(int, struct McMenuItem *),
				McMenuInit *items, int nitems, int flags);
extern McMenu *McShowMenu(McApp *, McGadget *starter, McMenuList *list,
			  int x, int y);
extern void McZapMenus(McMenu *first);
extern void McFreeMenu(McMenu *mnu);
extern void McFreeMenuList(McMenuList *mls);
extern void McFreeMenuItem(McMenuItem *item);

extern McMenuList *McFindMenuList(McMenuList *parent, int num);
extern McMenuItem *McFindMenuItem(McMenuList *parent, int num);

extern int _McHandleHotkeyInItems(McMenuList *menu, McMenuItem *item,
				  unsigned char key);

#endif /* _McMenu_h_ */

