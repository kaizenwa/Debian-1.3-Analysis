/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

  McInfoRequest.c

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
#include "../config.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

#include "McApp.h"
#include "McGadget.h"
#include "McFocus.h"
#include "McSlider.h"
#include "McText.h"
#include "McBitmap.h"
#include "McString.h"
#include "McSelector.h"
#include "McResource.h"
#include "McUtils.h"

#include "McInfoRequest.h"

#define STRSTACKDUP(var,x) 

static void gadget_proc(McGadget *gadget);
static int textwidth(McInfoRequest *req, const unsigned char *text);
static  int event_proc(McWindow *window, XEvent *event);

McInfoRequest *McCreateInfoRequest(McApp *app,
				   const unsigned char *title,
				   const unsigned char *text,
				   const unsigned char *yes,
				   const unsigned char *middle,
				   const unsigned char *no,
				   int winwidth,
				   void (*callback)(int, void *)) {
  McInfoRequest *req;
  int i, gadwidth;
  int width, height;
  McGadget *gad;
  unsigned char *message;
  struct McLine line[25];
  int lines, maxwidth=0;

  if (!text) return NULL;

  /* Make it writable if it isn't */
  message=alloca(strlen(text)+1); strcpy(message, text);

  if (!(req=malloc(sizeof(McInfoRequest)))) return NULL;

  req->app=app;
  req->callback=callback;

  gadwidth=0;
  if (yes)    gadwidth+=textwidth(req,yes)+30;
  if (middle) gadwidth+=textwidth(req,middle)+30;
  if (no)     gadwidth+=textwidth(req,no)+30;

  if (winwidth) {
    if (winwidth<gadwidth) winwidth=gadwidth;
    maxwidth=winwidth-20;
    if (maxwidth<20) maxwidth=0;
  } else {
    maxwidth=0;
  }
  height=McCalculateText(app->defaultFont, message,
			 &lines, &maxwidth, line)+60;

  width=maxwidth+20;
  if (width<gadwidth) width=gadwidth;

  /*
   * Create the window
   */
  if (no) i=width-20-textwidth(req,no); else i=width>>1;
  req->mcw=McCreateSimpleWindow(app, title, width, height, width, height,
				i, height-18, NULL, event_proc);
  req->mcw->mainButton=NULL;

  if (yes) {
    gad= MakeButton(req->mcw, 20, -8, 20+textwidth(req,yes), 0, 1,
		    (unsigned char *)yes, gadget_proc);
    gad->topLeftGravity = SouthWestGravity;
    gad->customData = req;
    if (!req->mcw->mainButton) req->mcw->mainButton=gad;
    McAddFocusGadget(gad, 1);
  }

  if (middle) {
    gad= MakeButton(req->mcw, 0, -8, 20+textwidth(req,middle), 0, -1,
		    (unsigned char *)middle, gadget_proc);
    gad->topLeftGravity = SouthWestGravity;
    gad->customData = req;
    if (!req->mcw->mainButton) req->mcw->mainButton=gad;
    McAddFocusGadget(gad, 1);
  }

  if (no) {
    gad= MakeButton(req->mcw, -20, -8, 20+textwidth(req,no), 0, 0,
		    (unsigned char *)no, gadget_proc);
    gad->topLeftGravity = SouthWestGravity;
    gad->customData = req;
    if (!req->mcw->mainButton) req->mcw->mainButton=gad;
    McAddFocusGadget(gad, 1);
  }

  req->mcw->customData = req;

  MakeTexts(req->mcw, 15, 15, line, lines);

  /*
   * Display the window
   */
  McSetFocus(FIRST_FOCUS_GADGET(req->mcw));
  McInitGadgets(req->mcw);
  McMapWindow(req->mcw);

  /* Setup complete. Let the event-loop do the rest. */
  return req;
}

void McRemoveInfoRequest(McInfoRequest **req) {
  if (*req) {
    McFreeWindow((*req)->mcw);
    free(*req);
    *req=NULL;
  }
}

void McInfoRequestNewText(McInfoRequest *req, unsigned char *text) {
  McApp *app=req->mcw->app;
  int lines, maxwidth=req->mcw->w - 20;
  McGadget *gad, *next;
  struct McLine line[25];

  gad=req->mcw->firstGadget;
  while(gad) {
    next=gad->next;
    if ((gad->normalLabel) && (!(gad->flags&GAD_ACTIVE))) McFreeGadget(gad);
    gad=next;
  }

  McCalculateText(app->defaultFont, text, &lines, &maxwidth, line);
  MakeTexts(req->mcw, 15, 15, line, lines);
  McInitGadgets(req->mcw);
}

/**************************************************************************/

static void gadget_proc(McGadget *gadget) {
  McInfoRequest *req=(McInfoRequest *)gadget->customData;
  McFreeWindow(gadget->mcw);
  if (req->callback) (*(req->callback))(gadget->id, req->customData);
  free(req);
}

/**************************************************************************/

static int textwidth(McInfoRequest *req, const unsigned char *text) {
  int direction, ascent, descent;
  XCharStruct overall;
  XTextExtents(req->app->defaultFont, text, strlen(text), &direction,
	       &ascent, &descent, &overall);
  return overall.width;
}

/**************************************************************************
 * static int event_proc(McWindow *mcw, XEvent *event)
 *
 */
static int event_proc(McWindow *mcw, XEvent *event) {
  McInfoRequest *req=(McInfoRequest *)mcw->customData;

  if(event->type==ClientMessage) {
    if ((event->xclient.format == 32) &&
	(event->xclient.data.l[0] == req->app->wmDelWin)) {
      McFreeWindow(mcw);
      if (req->callback) (*(req->callback))(0, req->customData);
      return 1;
    }
  }
  return 0;
}

/***************************************************************************/

/* height not used yet, pass as zero! */
McInfoRequest *McSizedError(McApp *app, int width, int height,
			    const char *fmt, ...) {
  const char *msg;
  va_list args;
  unsigned char buf[1024];
  va_start(args, fmt);
  vsprintf(buf, fmt, args);
  va_end(args);

#ifndef NO_FUNNY_MSG
  {
    int num;
    static const char *funny_msg[]={
      __M("So what?"),
      __M("Doh!"),
      __M("Oh no!"),
      __M("Shut up!"),
      __M("Never mind!"),
      __M("Oh really?"),
      __M("Wheee..."),
    };
    do {
      num=((random()>>8)*(sizeof(funny_msg)/sizeof(char *)))/(RAND_MAX>>8);
    } while(num>=(sizeof(funny_msg)/sizeof(char *)));
    msg = funny_msg[num];
  }
#else
  msg = __M("Ok");
#endif

  return McCreateInfoRequest(app, "Error", buf, NULL, _M(msg),
			     NULL, width, NULL);
} 

/***************************************************************************/

McInfoRequest *McSizedInfo(McApp *app, int width, int height,
			   const char *fmt, ...) {
  const char *msg;
  va_list args;
  unsigned char buf[1024];
  va_start(args, fmt);
  vsprintf(buf, fmt, args);
  va_end(args);

#ifndef NO_FUNNY_MSG
  {
    int num;
    static const char *funny_msg[]={
      __M("Very nice!"),
      __M("So?"),
      __M("Wow!"),
      __M("Sure!"),
      __M("I don't care!"),
      __M("Cool!"),
    };
    do {
      num=((random()>>8)*(sizeof(funny_msg)/sizeof(char *)))/(RAND_MAX>>8);
    } while(num>=(sizeof(funny_msg)/sizeof(char *)));
    msg = funny_msg[num];
  }
#else
  msg = __M("Ok");
#endif

  return McCreateInfoRequest(app, "Info", buf, NULL, _M(msg),
			     NULL, width, NULL);
} 

/**************************************************************************/

void McSync(McApp *app) {
  XSync(app->display, 0);
}

