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

#ifndef _McToolbar_h_
#define _McToolbar_h_

#define MCTBF_LINE		1 /* Draw an etched line below toolbar */

typedef struct McToolbarItem {
  int id;		/* generic id */
  int image;		/* Index in Bitmap */
  int x;		/* Receives this X position relative to gadget->x */
  const string_t tip;	/* infamous tool tip */
  const string_t xtip;	/* translated tip */
} McToolbarItem;

#define MCTOOLBAR(id,tip)		{ id, -1,  -1, tip, NULL }
#define MCTOOLBARIDX(id,tip,img)	{ id, img, -1, tip, NULL }
#define MCTB_SEPARATOR			{ -8, -1,  -1, NULL, NULL }
#define MCTB_SPACE(x)			{ -(x+BW+BW+2), -1,  -1, NULL, NULL}

#define McToolbarY(gad)		(gad->y-BW+5)
extern int McToolbarHeight(McGadget *gad);

typedef void (McToolbarCallback)(McGadget *, McToolbarItem *);

typedef struct McToolbar {
  McSpecialInfo specialInfo;
  unsigned int flags;
  const McBitmap *bitmap;  /* Indexed Bitmap with graphics for all buttons */
  McToolbarItem *buttons;		/* Array holding id of each button */
  int count;					/* Number of array entries */
  McToolbarCallback *callback;	 	    /* Callback for button presses */
  int item;					/* Currently selected item */
} McToolbar;

extern McSpecialInfo *McCreateToolbar(unsigned int flags,
				      const McBitmap *bmp,
				      McToolbarItem *buttons, int count,
				      McToolbarCallback *callback);
extern void McToolbarUpdate(McGadget *gadget, int busy, int all);
extern McGadget *MakeToolbar(McWindow *mcw, McGadget *menu, int *ty, 
			     const McBitmap *bmp, McToolbarItem *buttons,
			     int count, McToolbarCallback *callback);

#endif /* _McToolbar_h_ */


