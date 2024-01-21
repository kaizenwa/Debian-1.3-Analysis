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

#ifndef _McOrder_h_
#define _McOrder_h_

/* Order flags */
#define ORD_PRINTID	1	/* Print order ID inside order buttons */
#define ORD_SCROLL	2	/* Add scroll buttons if necessary */
#define ORD_SCROLLIES	4	/* Add always scroll buttons */
#define ORD_LEFTDOWN	8	/* private */
#define ORD_RIGHTDOWN	16	/* private */


struct McOrderItem;

typedef struct McOrder {
  McSpecialInfo specialInfo;
  unsigned int flags;
  int offset;
  int ItemWidth, ItemHeight;
  struct McOrderItem *first;
  struct McOrderItem *selection;
  struct McOrderItem *grip;
  struct McOrderItem *lastgrip;
  int xgrip, ygrip;
  Window handle;
} McOrder;

typedef struct McOrderItem {
  struct McOrderItem *next;
  struct McOrderItem *prev;
  int id;
  int customData;
  McBitmap *normalBitmap;
  McBitmap *selectBitmap;
  McText *normalText;
  McText *selectText;
  const string_t tip;
} McOrderItem;

extern McSpecialInfo *McCreateOrder(McWindow *mcw, unsigned int flags);
extern McOrderItem *McCreateOrderItem(McOrderItem *prev);

#define ORDER(gadget) ((McOrder *)((gadget)->specialInfo))

#endif /* _McOrder_h_ */
