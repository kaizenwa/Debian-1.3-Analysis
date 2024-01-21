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
#include <stdlib.h>
#include <errno.h>
#include <sys/time.h>
#include <strings.h>

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
#include "mover.h"
#include "xmedit.h"
#include "getdir.h"

/*****************************************************************************
 *
 * Private definitons
 *
 */

#define WIDTH  320
#define HEIGHT 120

typedef struct move_t {
  struct move_t *prev, *next;
  McApp *app;
  McWindow *mcw;
  McGadget *chcgad;
  unsigned char *path;
  unsigned char *root;
  unsigned char **dirs;
  int numdirs;
  int current;
} move_t;

/*****************************************************************************
 *
 * Private variables and procedures
 *
 */

static move_t *first_mover = NULL;

static int event_proc(McWindow *mcw, XEvent *event);
static void cancel_proc(McGadget *gadget);
static void move_proc(McGadget *gadget);
static void free_mover(move_t *mover);
static void free_mover_data(move_t *mover);

/*****************************************************************************
 *
 * Pop up the movewin window
 *
 */
void create_move_window(McApp *app, unsigned char *path, int current,
			unsigned char *root,
			unsigned char **dirs, int numdirs) {
  McGadget *gad;
  McWindow *movewin;
  struct move_t *mover=NULL;
  int i;
  unsigned char *foo;

  /* Avoid popping up a mover for the same path twice, and
   * avoid moving a path which is currently being edited
   */
  if (is_being_moved(path) || is_being_edited(path)) {
    XBell(app->display, 0);
    return;
  }

  if (!(mover = (struct move_t *)calloc(1, sizeof(struct move_t)))) goto bell;

  /*
   * have to dup everything since the user may chose to fiddle
   * with the main window while the mover is open. Doh!
   */

  if (!(mover->path=strdup(path))) goto bell;
  if (!(mover->dirs=calloc(numdirs,sizeof(unsigned char *)))) goto bell;
  for (i=0; i<numdirs; i++) if (!(mover->dirs[i]=strdup(dirs[i]))) goto bell;
  mover->root=root; /* no need to dup, won't change (normally...) */
  mover->numdirs=numdirs;
  mover->current=current;

  /*
   * Create the window
   */
  movewin=mover->mcw=McCreateSimpleWindow(app, _("Move Media"),
					  WIDTH, HEIGHT, WIDTH, HEIGHT,
					  WIDTH>>1, HEIGHT-20,
					  NULL, event_proc);
  movewin->customData=mover;

  MakeText(movewin, 0, 10, 0, _("Please select new cathegory for"));
  MakeText(movewin, 0, 30, 0, (foo=read_media_name(path))); free(foo);

  gad=MakeChoice(movewin, 0, 60, 0, 0, 0, NULL, NULL);
  mover->chcgad=gad;
  McChoiceSetList(mover->chcgad,
		  (const unsigned char **)mover->dirs, mover->numdirs);
  McChoiceSelect(mover->chcgad, current);
  McAddFocusGadget(gad, 1);

  gad=MakeButton(movewin,  12, -8, 80, 16, 0, _("Move"), move_proc);
  gad->topLeftGravity = SouthWestGravity;
  gad->customData=mover;
  McAddFocusGadget(gad, 1);
  movewin->mainButton=gad;

  gad=MakeButton(movewin, -12, -8, 80, 16, 0, _("Cancel"), cancel_proc);
  gad->topLeftGravity = SouthEastGravity;
  gad->customData=mover;
  McAddFocusGadget(gad, 1);

  mover->app=app;

  /*
   * Display the window
   */
  McInitGadgets(movewin);
  McMapWindow(movewin);
  McSetFocus(mover->chcgad);

  if (first_mover) first_mover->prev=mover;
  mover->prev=NULL;
  mover->next=first_mover;
  first_mover=mover;
  return;

bell:
  if (mover) {
    free_mover_data(mover);
    free(mover);
  }
  XBell(app->display, 0);
  return;
}

/**************************************************************************/

int is_being_moved(unsigned char *path) {
  if (first_mover) {
    move_t *next=first_mover;
    while(next) {
      if (!strcmp(next->path, path)) return 1;
      next=next->next;
    }
  }
  return 0;
}

/**************************************************************************/

static int event_proc(McWindow *mcw, XEvent *event) {
  move_t *mover = (move_t *)mcw->customData;

  switch(event->type) {
  case ClientMessage:
    if ((event->xclient.format == 32) &&
	(event->xclient.data.l[0] == mcw->app->wmDelWin)) {
      if (mover && (mcw==mover->mcw)) {
	free_mover(mover);
	return 1;
      }
    }
  }
  return 0;
}

/**************************************************************************/

static void cancel_proc(McGadget *gadget) {
  move_t *mover = (move_t *)gadget->customData;
  free_mover(mover);
}

static void move_proc(McGadget *gadget) {
  move_t *mover = (move_t *)gadget->customData;
  int sel=McChoiceSelection(mover->chcgad);
  if ((sel>=0) && (sel!=mover->current))
    move_line(mover->path, McChoiceSelectionString(mover->chcgad));
     free_mover(mover);
}

/**************************************************************************/

static void free_mover(move_t *mover) {
  McFreeWindow(mover->mcw);

  if (first_mover==mover) first_mover=mover->next;
  if (mover->prev) mover->prev->next=mover->next;
  if (mover->next) mover->next->prev=mover->prev;

  free_mover_data(mover);
  free(mover);
}

static void free_mover_data(move_t *mover) {
  if (mover->path) { free(mover->path); mover->path=NULL; }
  if (mover->dirs) {
    int i;
    for(i=0; i<mover->numdirs; i++)
      if (mover->dirs[i]) free(mover->dirs[i]);
    free(mover->dirs);
    mover->dirs=NULL;
  }
}

/**************************************************************************/




