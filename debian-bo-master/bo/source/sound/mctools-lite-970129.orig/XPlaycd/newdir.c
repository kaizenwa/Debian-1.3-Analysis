/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include "../config.h"

#include <stdio.h>
#include <errno.h>
#include <sys/time.h>
#include <strings.h>
#include <stdlib.h>

#include <X11/Xlib.h>
#include <McTools/McApp.h>
#include <McTools/McGadget.h>
#include <McTools/McFocus.h>
#include <McTools/McSlider.h>
#include <McTools/McText.h>
#include <McTools/McBitmap.h>
#include <McTools/McString.h>
#include <McTools/McSelector.h>
#include <McTools/McResource.h>
#include <McTools/McUtils.h>
#include <McTools/McChoice.h>
#include <McTools/McInfoRequest.h>

#include "xmdb.h"
#include "newdir.h"

/*****************************************************************************
 *
 * Private definitons
 *
 */

#define WIDTH  320
#define HEIGHT 100

typedef struct newdir_t {
  unsigned char *root;
  McGadget *strgad;
} newdir_t;

/*****************************************************************************
 *
 * Private variables and procedures
 *
 */
static int event_proc(McWindow *mcw, XEvent *event);
static void cancel_proc(McGadget *gadget);
static void create_proc(McGadget *gadget);

/*****************************************************************************
 *
 * Pop up the newdir window
 *
 */
void create_newdir_window(McApp *app,
			  unsigned char *root, unsigned char *path) {
  McGadget *gad, *strgad;
  McWindow *mcw;
  newdir_t *newdir;


  newdir = malloc(sizeof(newdir_t));
  newdir->root=root;

  /*
   * Create the window
   */
  mcw=McCreateSimpleWindow(app, _("New Cathegory"),
			   WIDTH, HEIGHT, WIDTH, HEIGHT,
			   WIDTH>>1, HEIGHT-20,
			   NULL, event_proc);

  MakeText(mcw, 0, 10, 0, _("Please enter name of new cathegory"));

  strgad=MakeString(mcw, 20, 35, WIDTH-40, 17, 0, path, NULL, create_proc);
  newdir->strgad=strgad;
  strgad->customData=newdir;
  McAddFocusGadget(strgad, 1);

  gad=MakeButton(mcw,  12, -8, 60, 16, 0, _("Create"), create_proc);
  gad->topLeftGravity = SouthWestGravity;
  gad->customData=newdir;
  McAddFocusGadget(gad, 1);
  mcw->mainButton=gad;

  gad=MakeButton(mcw, -12, -8, 60, 16, 0, _("Cancel"), cancel_proc);
  gad->topLeftGravity = SouthEastGravity;
  gad->customData=newdir;
  McAddFocusGadget(gad, 1);

  /*
   * Display the window
   */
  McInitGadgets(mcw);
  McMapWindow(mcw);
  McSetFocus(strgad);

  return;
}

/**************************************************************************/

static int event_proc(McWindow *mcw, XEvent *event) {
  switch(event->type) {
  case ClientMessage:
    if ((event->xclient.format == 32) &&
	(event->xclient.data.l[0] == mcw->app->wmDelWin)) {
      if (mcw) {
	newdir_t *newdir = (newdir_t *)mcw->customData;
	McFreeWindow(mcw);
	free(newdir);
      }
      return 1;
    }
  }
  return 0;
}

/**************************************************************************/

static void cancel_proc(McGadget *gadget) {
  newdir_t *newdir = (newdir_t *)gadget->customData;
  free(newdir);
  McFreeWindow(gadget->mcw);
}

/**************************************************************************/

static void create_proc(McGadget *gadget) {
  newdir_t *newdir = (newdir_t *)gadget->customData;
  unsigned char *buf, *in;
  int len;

  in = ((McString *)(newdir->strgad->specialInfo))->buf;
  while((*in==32) || (*in==9)) in++;
  len=strlen(in);
  while((len>0) && ((in[len-1]==32) || (in[len-1]==9))) len--;
  in[len]=0;
  if (len) {
    buf = alloca(strlen(newdir->root)+strlen(in)+10);
    strcpy(buf, newdir->root);
    if (buf[(len=strlen(buf))-1]!='/') { buf[len++]='/'; buf[len]=0; }
    strcat(buf, in);
    new_cathegory(buf);
    McFreeWindow(gadget->mcw);
    free(newdir);
  }
}
