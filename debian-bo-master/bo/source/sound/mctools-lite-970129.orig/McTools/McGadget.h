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

#ifndef _McGadget_h_
#define _McGadget_h_

#ifndef _McText_h_
#include "McText.h"
#endif
#ifndef _McBitmap_h_
#include "McBitmap.h"
#endif

/* Gadget flags */
#define GAD_H3D		0x00000
#define GAD_HCOMP	0x00001
#define GAD_HBOX	0x00002
#define GAD_HNONE	0x00003
#define GAD_HMASK	0x00003
#define GAD_ACTIVE	0x00004	/* This gadget wants events */
#define GAD_SELECTED	0x00008 
#define GAD_PRESSED	0x00010
#define GAD_TOGGLE	0x00020
#define GAD_INVIS	0x00040
#define GAD_STATE	0x00080	/* private */
#define GAD_3D		0x00100
#define GAD_DISABLE	0x00200
#define GAD_NOFILL	0x00400 /* Don't ClearArea this gadget before redraw,*
				 * since the Bitmap will cover the whole     *
				 * gadget anyway.			     */
#define GAD_NOTOGGLE	0x00800	/* Don't toggle GAD_SELECTED flag on press   */
#define GAD_CHANGED	0x01000	/* This gadget's contents have been changed  */
#define GAD_NOBORDER	0x02000 /* Don't create a border window		     */
#define GAD_NOSTIPPLE	0x04000	/* Don't stipple the gadget during callback  */
#define GAD_NORELVERIFY	0x08000	/* ButtonRelease doesn't need 2b over gadget */
#define GAD_BORDERONLY  0x10000	/* This one has only a border, no window     */
#define GAD_GROUP	0x20000 /* tab moves to next group bit set	     */
#define GAD_WANTFOCUS	0x40000 /* Gadget wants focus                        */
#define GAD_DEFBORDER	0x80000	/* Leave room to draw defaultButton border   */

/* Gadget types */
enum {
  CUSTOMGADGET,
  BOOLGADGET,
  SLIDERGADGET,
  ORDERGADGET,
  STRINGGADGET,
  SELECTORGADGET,
  MENUGADGET,		/* PRIVATE! */
  RESERVEDGADGET,
  LOADGADGET,
  CHOICEGADGET,
  MENUBARGADGET,
  VIEWTEXTGADGET,
  METERGADGET,
  KNOBGADGET,
  VIEWBITMAPGADGET,
  RADIOGADGET,
  CHECKBOXGADGET,
  TOOLBARGADGET,
};

#define MCGADGET_EVENT_MASK	(EnterWindowMask | LeaveWindowMask | \
				 ExposureMask | ButtonPressMask | \
				 ButtonReleaseMask | ButtonMotionMask)

#define GSP_FOCUSUPDATE 0x00001 /* private: Needs update on focus change     */

typedef struct McSpecialInfo {
  void (*updateProc)(struct McGadget *, int, int);
  int  (*eventProc)(struct McGadget *, XEvent *);
  int  (*keyboardProc)(struct McGadget *, XKeyEvent *);
  void (*cleanupProc)(struct McGadget *);
  int  (*releaseProc)(struct McGadget *, int, int);
  const string_t (*tipProc)(struct McGadget *, int *x, int *y);
  int flags;
} McSpecialInfo;

typedef struct McGadget {
  struct McGadget *next;
  struct McGadget *prev;
  struct McGadget *focusNext;
  struct McGadget *focusPrev; /* For keyboard focus */
  unsigned int type;
  unsigned int flags;
  int topLeftGravity;
  int bottomRightGravity;
  struct McBitmap *normalBitmap;
  struct McBitmap *selectBitmap;
  struct McText *normalLabel;
  struct McText *selectLabel;
  void	(*callbackDown)(struct McGadget *);
  void	(*callbackUp)(struct McGadget *);
  int x, y;
  int width, height;
  int id;
  unsigned long mutualExclude;
  void *customData;
  McWindow *mcw;
  Time time; /* To detect double clicks */
  int clicks;
  struct McSpecialInfo *specialInfo;
  const string_t tip;
  Window win,bwin;
  Region region;
} McGadget;

extern McGadget *McCreateGadget(McWindow *mcw, unsigned int flags,
				unsigned int type, int x, int y, int w, int h);
extern void McInitGadgets(McWindow *mcw);
extern void McFreeGadget(McGadget *gadget);
extern void McWipeGadget(McGadget *gadget);
extern int  McMoveGadget(McGadget *gadget, int x, int y);
extern int  McResizeGadget(McGadget *gadget, int w, int h);
extern void McGadgetRedraw(McGadget *gadget);
extern void McGadgetBorderRedraw(McGadget *gadget, int busy);
extern void McGadgetBusy(McGadget *gadget);
extern void McGadgetUpdate(McGadget *gadget);
extern void McDrawFocusMark(McGadget *gadget, int background);
extern void McSetMainButton(McGadget *gadget);
extern int  McGadgetEvent(McGadget *gadget, XEvent *event);
extern int  McGadgetKeyPress(McGadget *gadget, XKeyEvent *event);
extern void McGadgetInfo(McGadget *gadget);
extern void McPressGadget(McGadget *gadget);
extern void McReleaseGadget(McGadget *gadget, int inside);
extern void McGadgetUpdateClicks(McGadget *gadget, int tme);
#endif /* _McGadget_h_ */

