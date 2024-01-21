/* Copyright (C) 1996 
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


#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "McApp.h"
#include "McGadget.h"
#include "McFocus.h"
#include "McDebug.h"

/****************************************************************************/

void McSetFocus(McGadget *gadget) {
  McWindow *mcw;
  McGadget *old;

  if (!gadget) return;
  if (!(gadget->flags&GAD_WANTFOCUS)) return;

  mcw=gadget->mcw;
  old=mcw->keyboardFocus;
  if (old!=gadget) {
    mcw->keyboardFocus=gadget;
    if (old) {
      McGadgetUpdate(old);
      if (old->flags & GAD_CHANGED) {
	old->flags&=~GAD_CHANGED;
	if (old->callbackUp)
	  (*old->callbackUp)(old);
      }
    }
    McGadgetUpdate(gadget);
    if (mcw->focusCallback) (*(mcw->focusCallback))(gadget);
  }
  if (mcw->mainButton && mcw->mainButton!=gadget && mcw->mainButton!=old)
    McDrawFocusMark(mcw->mainButton, -1);
}

/****************************************************************************/

void McInsertFocusGadget(McGadget *prev, McGadget *gadget, McGadget *next,
			 int group) {
  McWindow *mcw=gadget->mcw;

  assert(!(prev && next));

  if (!group) gadget->flags&=~GAD_GROUP; else gadget->flags|=GAD_GROUP;
  gadget->flags|=GAD_WANTFOCUS;

  if (prev) {
    gadget->focusPrev=prev;
    gadget->focusNext=prev->focusNext;
    if (prev->focusNext) prev->focusNext->focusPrev=gadget;
    prev->focusNext=gadget;

    if ((!mcw->lastFocus) || (mcw->lastFocus==prev))
      mcw->lastFocus=gadget;

    if (!mcw->firstFocus) mcw->firstFocus=prev;

  } else if (next) {
    gadget->focusNext=next;
    gadget->focusPrev=next->focusPrev;
    if (next->focusPrev) next->focusPrev->focusNext=gadget;
    next->focusPrev=gadget;

    if ((!mcw->firstFocus) || (mcw->firstFocus==next))
      mcw->firstFocus=gadget;

    if (!mcw->lastFocus) mcw->lastFocus=next;

  } else { /* prev==NULL && next==NULL */
    if (!mcw->firstFocus) mcw->firstFocus=gadget;
    gadget->focusPrev=mcw->lastFocus;
    gadget->focusNext=NULL;
    if (mcw->lastFocus) mcw->lastFocus->focusNext=gadget;
    mcw->lastFocus=gadget;
  }

}

/****************************************************************************/

void McRemoveFocusGadget(McGadget *gadget) {
  McWindow *mcw=gadget->mcw;
  if (gadget->focusPrev) gadget->focusPrev->focusNext=gadget->focusNext;
  if (gadget->focusNext) gadget->focusNext->focusPrev=gadget->focusPrev;

  if (mcw->firstFocus==gadget) mcw->firstFocus = gadget->focusNext;
  if (mcw->lastFocus==gadget)  mcw->lastFocus  = gadget->focusPrev;

  gadget->flags&=~GAD_WANTFOCUS;
}

/****************************************************************************/

void McNextFocus(McWindow *mcw, int group) {
  McGadget *gadget;
  if ((gadget=mcw->keyboardFocus)) {

    if (group) {
      McGadget *next=gadget;
      while(1) {
	next=next->focusNext;
	if (!next) next=mcw->firstFocus;
	if (next==gadget) break;
	if (!(next->flags&GAD_ACTIVE)) continue;
	if (next->flags&GAD_GROUP) break;
      }
      McSetFocus(next);
    } else {
      McGadget *next=gadget->focusNext;
      if (!next) next=mcw->firstFocus;
      McSetFocus(next);
    }
  } else {
    McSetFocus(mcw->firstFocus);
  }
}

/****************************************************************************/

void McPrevFocus(McWindow *mcw, int group) {
  McGadget *gadget;
  if ((gadget=mcw->keyboardFocus)) {

    if (group) {
      McGadget *prev=gadget;
      int i;
      if (gadget->flags&GAD_GROUP) i=1; else i=2;
      while(--i>=0) {
	while(1) {
	  prev=prev->focusPrev;
	  if (!prev) prev=mcw->lastFocus;
	  if (prev==gadget) break;
	  if (!(prev->flags&GAD_ACTIVE)) continue;
	  if (prev->flags&GAD_GROUP) break;
	}
      }
      McSetFocus(prev);
    } else {
      McGadget *prev=gadget->focusPrev;
      if (!prev) prev=mcw->lastFocus;
      McSetFocus(prev);
    }
  } else {
    McSetFocus(mcw->lastFocus);
  }
}

/****************************************************************************/
