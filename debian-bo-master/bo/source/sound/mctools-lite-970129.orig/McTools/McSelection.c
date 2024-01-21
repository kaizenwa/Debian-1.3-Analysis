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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xatom.h>
#include "McApp.h"
#include "McGadget.h"
#include "McResource.h"
#include "McSelection.h"

/* How to make lead from gold... :) */
static Atom aurum(Display *display) {
  static Atom plumbum=0;
  if (!plumbum)
    plumbum=XInternAtom(display, "CLIPBOARD", False);
  return plumbum;
}

/**************************************************************************
 *
 * Tries to make gadget the selection owner
 *
 */
int McSetNewSelection(McGadget *gadget, unsigned char *new, int len) {
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;

  XSetSelectionOwner(app->display, XA_PRIMARY, mcw->framewin, CurrentTime);
  if (XGetSelectionOwner(app->display, XA_PRIMARY)!=mcw->framewin) {
    XBell(app->display, 0);
    return 0;
  }

  if (mcw->selection) {
    free(mcw->selection);
    mcw->selection=NULL;
    mcw->selectionOwner=NULL;
  }

  if (len>0) {
    mcw->selection=(unsigned char *)calloc(len+1, 1);
    mcw->selectionOwner=gadget;
    strncpy(mcw->selection, new, len);
    XChangeProperty(app->display, DefaultRootWindow(app->display),
		    XA_CUT_BUFFER0, XA_STRING, 8,
		    PropModeReplace, mcw->selection, len);
  }
  return 1;
}

/*
 * Tells the current selection owner that we want the current selection.
 * If there's no owner, return the contents of CUT_BUFFER0 instead.
 *
 * The returned string must be freed with XFree() !
 */
unsigned char *McQuerySelection(McGadget *gadget, Time tme, int *max_wanted) {
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  if (XGetSelectionOwner(app->display,XA_PRIMARY) != None) {
    XConvertSelection(app->display, XA_PRIMARY, XA_STRING,
		      aurum(app->display), mcw->framewin, tme);
    return NULL; /* Not yet available */
    *max_wanted=0;
  } else {
    return XFetchBytes(app->display, max_wanted);
  }
}

/*
 * We actually got the selection, so read it.
 *
 * Subtitle: Fun things to do with lead and gold...
 *
 * The returned string must be freed with XFree() !
 */
unsigned char *McReadSelection(McGadget *gadget, int *max_wanted) {
  Atom plumbum;
  McWindow *mcw=gadget->mcw;
  McApp *app=mcw->app;
  int how;
  unsigned long bytes, len;
  unsigned char *data;

  XGetWindowProperty(app->display, mcw->framewin, aurum(app->display),
		     0, (*max_wanted)>>2, False,
		     XA_STRING, &plumbum, &how, &len, &bytes, &data);
  if ((plumbum!=XA_STRING) || (how!=8)) {
    XBell(app->display, 0);
    if (data) XFree(data);
    *max_wanted=0;
    return NULL;
  } else {
    *max_wanted=len;
    return data;
  }
}

/*
 * Someone requests us to send him the current selection
 */
void McSendSelection(McWindow *mcw, XSelectionRequestEvent *event) {
  XSelectionEvent sel;

  sel.type=SelectionNotify;
  sel.send_event=True;
  sel.display=event->display;
  sel.requestor=event->requestor;
  sel.selection=event->selection;
  sel.target=event->target;
  sel.property=event->property;
  sel.time=event->time;
  if ((!mcw->selection) || (sel.selection!=XA_PRIMARY) ||
      (sel.target!=XA_STRING)) {
    XBell(mcw->app->display, 0);
    sel.property=None;
  } else {
    XChangeProperty(sel.display, sel.requestor,
		    sel.property, sel.target,
		    8, PropModeReplace, mcw->selection,
		    strlen(mcw->selection));
  }
  XSendEvent(sel.display, sel.requestor, True, 0,(XEvent *)&sel);
}

/*
 * We lost the selectionownership
 */
void McClearSelection(McWindow *mcw) {
  mcw->selectionOwner=NULL;
  if (mcw->selection) {
    free(mcw->selection);
    mcw->selection=NULL;
  }
}
