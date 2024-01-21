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

#ifndef _McFocus_h_
#define _McFocus_h_

#define FIRST_FOCUS_GADGET(w) ((w)->firstFocus)

extern void McSetFocus(McGadget *gadget);

extern void McInsertFocusGadget(McGadget *prev, McGadget *gadget,
				McGadget *next, int group);
#define McAddFocusGadget(gad, grp) McInsertFocusGadget(NULL, gad, NULL, grp)
extern void McRemoveFocusGadget(McGadget *gadget);

extern void McNextFocus(McWindow *mcw, int group);
extern void McPrevFocus(McWindow *mcw, int group);

#endif /* _McFocus_h_ */

