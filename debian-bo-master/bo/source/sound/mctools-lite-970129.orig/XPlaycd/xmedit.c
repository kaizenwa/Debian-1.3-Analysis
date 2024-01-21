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
#include <sys/stat.h>
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
#include "xmedit.h"
#include "mover.h"
#include "getdir.h"
#include "cathegory.h"
#include "newdir.h"

/*****************************************************************************
 *
 * Private definitons
 *
 */

#define WIDTH  376
#define HEIGHT 320

typedef struct edit_t {
  struct edit_t *prev, *next;
  McApp *app;
  McWindow *mcw;
  McGadget *selgad, *titgad, *trkgad;
  unsigned char **titles, **before, **after;
  unsigned char *path, *title;
  int ntitles, nbefore, nafter, count;
  int finish;
  McWindow *cwin;
  McGadget *catgad;
  unsigned char *root;
  unsigned char **dirs;
  int numdirs;

} edit_t;

/*****************************************************************************
 *
 * Private variables and procedures
 *
 */

static edit_t *first_editor = NULL;

static int event_proc(McWindow *mcw, XEvent *event);
static void select_up(McGadget *gadget);
static void select_down(McGadget *gadget);
static void close_proc(McGadget *gadget);
static void undo_proc(McGadget *gadget);
static void cancel_proc(McGadget *gadget);
static void apply_proc(McGadget *gadget);
static void song_proc(McGadget *gadget);
static void set_selection(edit_t *editor, int sel);
static void create_titlelist(edit_t *editor);
static int apply_list(edit_t *editor);
static void free_editor(edit_t *editor);
static void free_editor_data(edit_t *editor, int all);
static int query_cathegory(edit_t *editor);

/*****************************************************************************
 *
 * Pop up the editwin window
 *
 */
void create_edit_window(McApp *app, unsigned char *path, unsigned char *root,
			int count, unsigned char **dirs, int numdirs) {
  McGadget *gad, *selgad, *trkgad, *titgad;
  McWindow *editwin;
  struct edit_t *editor=NULL;
  struct stat st;

  if (!path) goto bell;

  /*
   * If count is non zero, pop up an empty editor with count empty
   * entries. On close or apply, let the user chose a cathegory.
   * count empty entries. But if count is not set, do nothing but beep.
   */
  if (count<=0) {
    if (stat(path, &st)) goto bell;
    if ((!(S_ISREG(st.st_mode)))  && (!(S_ISLNK(st.st_mode)))) goto bell;
  }

  /* Avoid popping up an editor for the same path twice, and
   * avoid editing a path which is currently being moved.
   */
  if (is_being_edited(path) || is_being_moved(path)) goto bell;

  /* No way to pop up an error message if allocating these few
   * bytes fails.
   */
  if (!(editor = (struct edit_t *)calloc(1, sizeof(struct edit_t)))) goto bell;


  /*
   * have to dup everything since the user may chose to fiddle
   * with the main window while the editor is open. Doh!
   */
  if ((editor->count=count)>0) {
    int i;
    if (!(editor->dirs=calloc(numdirs,sizeof(unsigned char *)))) goto bell;
    for (i=0; i<numdirs; i++)
      if (!(editor->dirs[i]=strdup(dirs[i]))) goto bell;
    editor->numdirs=numdirs;
    editor->root=root;
  }

  /*
   * Create the window
   */
  editwin=editor->mcw=McCreateSimpleWindow(app, _("Media editor"),
					   WIDTH, HEIGHT, WIDTH, 150,
					   0, 0, NULL, event_proc);
  editwin->customData=editor;

  gad=MakeButton(editwin,  12, 294, 77, 16, 0, _("Ok"), close_proc);
  gad->topLeftGravity = SouthWestGravity;
  gad->customData=editor;
  editwin->mainButton=gad;
  McAddFocusGadget(gad, 1);

  gad=MakeButton(editwin, 103, 294, 78, 16, 0, _("Apply"), apply_proc);
  gad->topLeftGravity = SouthWestGravity;
  gad->customData=editor;
  McAddFocusGadget(gad, 1);

  gad=MakeButton(editwin, 195, 294, 78, 16, 0, _("Undo"), undo_proc);
  gad->topLeftGravity = SouthWestGravity;
  gad->customData=editor;
  McAddFocusGadget(gad, 1);

  gad=MakeButton(editwin, 287, 294, 77, 16, 0, _("Cancel"), cancel_proc);
  gad->topLeftGravity = SouthWestGravity;
  gad->customData=editor;
  McAddFocusGadget(gad, 1);

  MakeRText(editwin, 40, 10, 0, _("Title:"));
  titgad=MakeString(editwin, 46, 10, 318, 17, 0, "", NULL, NULL);
  titgad->topLeftGravity = NorthWestGravity;
  titgad->bottomRightGravity = NorthEastGravity;
  titgad->customData=editor;
  McAddFocusGadget(titgad, 1);

  selgad=MakeSelector(editwin, 12, 37, 331, 220, 0,
		      app->defaultFont, select_up, select_down);
  selgad->bottomRightGravity = SouthEastGravity;
  selgad->customData=editor;
  McAddFocusGadget(selgad, 1);

  gad=MakeProp(editwin, 352, 37, 12, 220, 0, NULL);
  gad->topLeftGravity = NorthEastGravity;
  gad->bottomRightGravity = SouthWestGravity;
  gad->customData=editor;
  McSelectorBindSlider(selgad, gad);

  trkgad=MakeString(editwin, 12, 267, 352, 17, 0, "", song_proc, song_proc);
  trkgad->topLeftGravity = SouthWestGravity;
  trkgad->bottomRightGravity = NorthEastGravity;
  trkgad->customData=editor;
  McAddFocusGadget(trkgad, 1);

  editor->path=strdup(path);
  editor->titgad=titgad;
  editor->selgad=selgad;
  editor->trkgad=trkgad;
  editor->app=app;

  /*
   * Display the window
   */
  McInitGadgets(editwin);
  McMapWindow(editwin);

  McSetFocus(titgad);

  create_titlelist(editor);

  if (first_editor) first_editor->prev=editor;
  editor->prev=NULL;
  editor->next=first_editor;
  first_editor=editor;
  return;

bell:
  if (editor) free_editor(editor);
  XBell(app->display, 0);
  return;
}

/**************************************************************************/

int is_being_edited(unsigned char *path) {
  if (first_editor) {
    edit_t *next=first_editor;
    while(next) {
      if (!strcmp(next->path, path)) return 1;
      next=next->next;
    }
  }
  return 0;
}

int is_editing(void) {
  return !!first_editor;
}

/**************************************************************************/

static int event_proc(McWindow *mcw, XEvent *event) {
  edit_t *editor = (edit_t *)mcw->customData;

  switch(event->type) {
  case ClientMessage:
    if ((event->xclient.format == 32) &&
	(event->xclient.data.l[0] == mcw->app->wmDelWin) && (editor)) {
      if (mcw==editor->mcw) {
	McFreeWindow(mcw);
	editor->mcw=NULL;
	if (!editor->cwin) free_editor(editor);
	return 1;
      }
      if (mcw==editor->cwin) {
	McFreeWindow(mcw);
	editor->cwin=NULL;
	if (!editor->mcw) free_editor(editor);
	return 1;
      }
    }
  }
  return 0;
}

/**************************************************************************/

static void free_editor(edit_t *editor) {
  McFreeWindow(editor->mcw);
  if (editor->cwin) McFreeWindow(editor->cwin);

  if (first_editor==editor) first_editor=editor->next;
  if (editor->prev) editor->prev->next=editor->next;
  if (editor->next) editor->next->prev=editor->prev;

  free_editor_data(editor, 1);
  if (editor->path) free(editor->path);
  free(editor);
}

/**************************************************************************/

static void select_up(McGadget *gadget) {
  edit_t *editor = (edit_t *)gadget->customData;

  McSetFocus(editor->trkgad);
  McGadgetUpdate(editor->trkgad);
}

/**************************************************************************/

static void select_down(McGadget *gadget) {
  edit_t *editor = (edit_t *)gadget->customData;
  McSelector *selector = (McSelector *)gadget->specialInfo;

  set_selection(editor, selector->selection);
  if (selector->flags & SEL_DOUBLE_CLICK) {
    McSetFocus(editor->trkgad);
    McGadgetUpdate(editor->trkgad);
  }
}

/**************************************************************************/

static void set_selection(edit_t *editor, int sel) {
  unsigned char *c;
  if (editor->titles) {
    if (!(c=strchr(editor->titles[sel], '-')))
      c=editor->titles[sel]; else c+=2;
    McStringSetText(editor->trkgad, c);
  }
}

/**************************************************************************/

static void close_proc(McGadget *gadget) {
  edit_t *editor = (edit_t *)gadget->customData;
  if (apply_list(editor)) {
    if (editor->cwin) editor->finish=1; else free_editor(editor);
  }
}

static void undo_proc(McGadget *gadget) {
  edit_t *editor = (edit_t *)gadget->customData;
  create_titlelist(editor);
}

static void cancel_proc(McGadget *gadget) {
  edit_t *editor = (edit_t *)gadget->customData;
  free_editor(editor);
}

static void apply_proc(McGadget *gadget) {
  edit_t *editor = (edit_t *)gadget->customData;
  apply_list(editor);
}

/**************************************************************************/

static void song_proc(McGadget *gadget) {
  edit_t *editor = (edit_t *)gadget->customData;
  unsigned char *c;
  int sel;

  sel=((McSelector *)editor->selgad->specialInfo)->selection;
  if (!(c=strchr(editor->titles[sel], '-')))
    c=editor->titles[sel]; else c+=2;
  
  if (strcmp(c, ((McString *)gadget->specialInfo)->buf)) {
    unsigned char *p;
    free(editor->titles[sel]);
    c=((McString *)gadget->specialInfo)->buf;
    p=editor->titles[sel]=malloc(strlen(c)+8);
    if (sel<9) { *p++=' '; *p++=' '; } else { *p++=((sel+1)/10)+'0'; }
    *p++=((sel+1)%10)+'0'; *p++=' '; *p++='-'; *p++=' ';
    strcpy(p, c);
    McGadgetUpdate(editor->trkgad);
  }
  if (++sel>=editor->ntitles) sel=0;
  McSelectorSelect(editor->selgad, sel);
  set_selection(editor, sel);
}

/**************************************************************************/

static void create_empty_editor(edit_t *editor) {
  int i;

  editor->titles=calloc(editor->count, sizeof(unsigned char *));
  if (!editor->titles) goto error;
  for (i=0; i<editor->count; i++) {
    unsigned char *p;
    if (!(p=malloc(8))) goto error;
    editor->titles[i]=p;
    if (i<9) { *p++=' '; *p++=' '; } else { *p++=((i+1)/10)+'0'; }
    *p++=((i+1)%10)+'0'; *p++=' '; *p++='-'; *p++=' '; *p++=0;
  }
  editor->ntitles=editor->count;

  McStringSetText(editor->titgad, "");
  McSelectorSetList(editor->selgad, (const unsigned char **)editor->titles,
		    editor->ntitles, 0);
  set_selection(editor, 0);
  return;

error:
  XBell(editor->app->display, 0);
  free_editor(editor);
}

static void create_titlelist(edit_t *editor) {
  int len, flag;
  FILE *fp;
  size_t ttlmax=0, ttlinc=0, commax=0, cominc=0, mscmax=0, mscinc=0;
  unsigned char buf[512];

  free_editor_data(editor, 0);

  if (editor->count>0) {
    create_empty_editor(editor);
    return;
  }

  if (editor->path) {
    fp=fopen(editor->path, "r");
    if (!fp) {
      McError(editor->app, _("Can't open database file `%s'\nReason: %s\n"),
	      editor->path, strerror(errno));
      free_editor(editor);
      return;
    }

    flag=0;
    while (fgets(buf,512,fp)) {
      if (buf[(len=strlen(buf)-1)]=='\n') buf[len]=0;

      if (!strncmp(buf, "DTITLE=", 7)) {
	flag=1;
	if (!editor->title) if (!(editor->title=strdup(buf+7))) goto error;
	continue;
      }

      if (!strncmp(buf, "TTITLE", 6)) {
	int tr,aa=0;
	unsigned char *p;

	flag=1;
	sscanf(buf+6,"%d=%n",&tr,&aa);
	if (tr>=0) {
	  if (!make_room_for_more(tr, &ttlmax, &ttlinc, &editor->titles))
	    goto error;
	  if (editor->titles[tr]) free(editor->titles[tr]);
	  if (!(p=malloc(strlen(&buf[aa+6])+8))) goto error;
	  editor->titles[tr]=p;
	  tr++;
	  if (tr<10) { *p++=' '; *p++=' '; } else { *p++=(tr/10)+'0'; }
	  *p++=(tr%10)+'0'; *p++=' '; *p++='-'; *p++=' ';
	  strcpy(p, &buf[aa+6]);
	  if (tr>editor->ntitles) editor->ntitles=tr;
	}
	continue;
      }

      /* We save everything else and write it back unchanged into the file */
      if (flag) {
	if (!make_room_for_more(editor->nafter, &mscmax, &mscinc,
				&editor->after)) goto error;
	if (!(editor->after[editor->nafter++]=strdup(buf))) goto error;
      } else {
	if (!make_room_for_more(editor->nbefore, &commax, &cominc,
				&editor->before)) goto error;
	if (!(editor->before[editor->nbefore++]=strdup(buf))) goto error;
      }
    }
    fclose(fp);
  } else {
    McError(editor->app, _("Huh? No path for database file?\n"));
  }

  if (editor->title)
    McStringSetText(editor->titgad, editor->title);
  else
    McStringSetText(editor->titgad, "");
  McSelectorSetList(editor->selgad, (const unsigned char **)editor->titles,
		    editor->ntitles, 0);
  set_selection(editor, 0);
  return;

error:
  free_editor_data(editor, 0);
  McSelectorSetList(editor->selgad, NULL, 0, 0);
  fclose(fp);
  McError(editor->app, 
	  _("Error reading database file into buffer.\nReason: %s\n"),
	  strerror(errno));
}

static void free_editor_data(edit_t *editor, int all) {
  int i;

  if (editor->titles) {
    for (i=0; i<editor->ntitles; i++) {
      if(editor->titles[i]) free(editor->titles[i]);
    }
    free(editor->titles);
  }

  if (all) {
    if (editor->dirs) {
      for(i=0; i<editor->numdirs; i++)
	if (editor->dirs[i]) free(editor->dirs[i]);
      free(editor->dirs);
      editor->dirs=NULL;
    }
  }

  if (editor->before) {
    for (i=0; i<editor->nbefore; i++) {
      if (editor->before[i]) free(editor->before[i]);
    }
    free(editor->before);
  }

  if (editor->after) {
    for (i=0; i<editor->nafter; i++) {
      if (editor->after[i]) free(editor->after[i]);
    }
    free(editor->after);
  }

  if (editor->title) free(editor->title);

  editor->title=NULL;
  editor->titles=NULL;
  editor->ntitles=0;
  editor->before=NULL;
  editor->nbefore=0;
  editor->after=NULL;
  editor->nafter=0;
}

/**************************************************************************/

static int apply_list(edit_t *editor) {
  int i;
  FILE *fp;
  unsigned char *p;

  /* Double Mopple... (: */
  p=((McString *)editor->titgad->specialInfo)->buf;
  if ((!editor->title) || (strcmp(editor->title, p))) {
    if (editor->title) free(editor->title);
    editor->title=NULL;
    if (strlen(p)) editor->title=strdup(p);
  }

  if (editor->count) {
    return query_cathegory(editor);
  }

  if (!editor->path) {
    McError(editor->app, _("Huh? No path for database file?\n"));
    return 0;
  }

  if (!(fp=fopen(editor->path, "w"))) goto error;

  if (editor->before) {
    for (i=0; i<editor->nbefore; i++)
      if (!fprintf(fp, "%s\n", editor->before[i])) goto error;
  } else {
    time_t fish;
    fputs("# xplaycd, xmdb and xmcd database file\n", fp);
    fputs("# Copyright: xmcd & file layout: (C) 1994 Ti Kan\n", fp);
fputs("#            xplaycd, xmdb:      (C) 1994/1995 Olav Wölfelschneider\n",
	  fp);
    fputs("#\n",fp);
    fputs("# File created by " APP_VERSION " on ",fp);
    time(&fish);
    fputs(ctime(&fish), fp);
    fputs("#\n",fp);
  }

  if (editor->title)
    if (!fprintf(fp, "DTITLE=%s\n", editor->title)) goto error;

  if (editor->titles) {
    for (i=0; i<editor->ntitles; i++)
      if (editor->titles[i]) {
	unsigned char *fish=strchr(editor->titles[i],'-');
	if (fish) fish+=2; else fish=editor->titles[i];
	if (!fprintf(fp, "TTITLE%d=%s\n", i, fish)) goto error;
      }
  }

  if (editor->after) {
    for (i=0; i<editor->nafter; i++)
      if (!fprintf(fp, "%s\n", editor->after[i])) goto error;
  }

  fclose(fp);
  update_line(editor->path);
  return 1;

error:
  if (fp) fclose(fp);
  McError(editor->app, _("Unable to write database file `%s'\nReason: %s\n"),
	  editor->path, strerror(errno));
  return 0;
}

/**************************************************************************
 *
 * Code for querying the user into which cathegory he wants to place
 * the new media.
 *
 */

#define CWIDTH  320
#define CHEIGHT 120

static void cath_cancel_proc(McGadget *gadget);
static void cath_save_proc(McGadget *gadget);
static void cath_new_proc(McGadget *gadget);

static int query_cathegory(edit_t *editor) {
  McWindow *mcw;
  McGadget *gad;

  if (editor->cwin) {
    XBell(editor->app->display, 0);
    return 0;
  }

  if (!editor->title) {
    McError(editor->app, _("You can't save a Media with no title.\n"));
    return 0;
  }

  /*
   * Create the window
   */
  mcw=editor->cwin=McCreateSimpleWindow(editor->app, _("New Media"),
					CWIDTH, CHEIGHT, CWIDTH, CHEIGHT,
					CWIDTH>>1, CHEIGHT-20,
					NULL, event_proc);
  mcw->customData=editor;

  MakeText(mcw, 0, 10, 0, _("Please select cathegory for"));
  MakeText(mcw, 0, 30, 0, editor->title);

  gad=MakeChoice(mcw, 0, 60, 0, 0, 0, NULL, NULL);
  editor->catgad=gad;
  McChoiceSetList(gad, (const unsigned char **)editor->dirs, editor->numdirs);

  gad=MakeButton(mcw, -12, -8, 60, 16, 0, _("Cancel"), cath_cancel_proc);
  gad->topLeftGravity = SouthEastGravity;
  gad->customData=editor;

  gad=MakeButton(mcw,   0, -8, 60, 16, 0, _("New"), cath_new_proc);
  gad->topLeftGravity = SouthEastGravity;
  gad->customData=editor;

  gad=MakeButton(mcw,  12, -8, 60, 16, 0, _("Save"), cath_save_proc);
  gad->topLeftGravity = SouthWestGravity;
  gad->customData=editor;

  /*
   * Display the window
   */
  McInitGadgets(mcw);
  McMapWindow(mcw);

  return 1;
}

/**************************************************************************/

static void cath_cancel_proc(McGadget *gadget) {
  edit_t *editor = (edit_t *)gadget->customData;
  McFreeWindow(editor->cwin);
  editor->cwin=NULL;
  if (!editor->mcw) free_editor(editor);
}

/**************************************************************************/

static void cath_save_proc(McGadget *gadget) {
  edit_t *editor = (edit_t *)gadget->customData;
  unsigned char *buf;
  const unsigned char *cath;
  int len;

  McFreeWindow(editor->cwin);
  editor->cwin=NULL;

  cath=McChoiceSelectionString(editor->catgad);
  if (!strcmp(cath, NO_CATHEGORY)) cath=NULL;

  len=strlen(editor->root)+16; if (cath) len+=strlen(cath);
  buf = malloc(len);
  if (!buf) return;

  strcpy(buf, editor->root);
  if (buf[(len=strlen(buf))-1]!='/') { buf[len++]='/'; buf[len]=0; }
  if (cath) {
    strcat(buf, cath);
    if (buf[(len=strlen(buf))-1]!='/') { buf[len++]='/'; buf[len]=0; }
  }

  strcat(buf, editor->path);

  free(editor->path);
  editor->path=buf;
  editor->count=0;

  apply_list(editor);

  if (editor->finish) free_editor(editor);
}

/**************************************************************************/

static void cath_new_proc(McGadget *gadget) {
  edit_t *editor = (edit_t *)gadget->customData;
  unsigned char *path, *foo;
  int len;

  foo=editor->dirs[McChoiceSelection(editor->catgad)];
  if (strcmp(foo, NO_CATHEGORY)) {
    path=alloca((len=strlen(foo))+2);
    strcpy(path, foo);
    path[len]='/';
  } else {
    path=NULL;
  }

  create_newdir_window(editor->app, editor->root, path);
}

/**************************************************************************
 *
 * When the user makes a new cathegory, there may be some editors
 * which have to be updated.
 *
 */
static void update_edit_dir(edit_t *editor, unsigned char **dirs, int numdirs);

void update_editor_dirs(unsigned char **dirs, int numdirs) {
  if (first_editor) {
    edit_t *next=first_editor;
    while(next) {
      update_edit_dir(next, dirs, numdirs);
      next=next->next;
    }
  }
}

static void update_edit_dir(edit_t *editor,unsigned char **dirs,int numdirs) {
  if (editor->dirs) {
    int i;
    unsigned char *last=NULL;

    if (editor->cwin)
      last=strdup(McChoiceSelectionString(editor->catgad));

    for (i=0; i<editor->numdirs; i++)
      if (editor->dirs[i]) free(editor->dirs[i]);
    free(editor->dirs);

    editor->dirs=calloc(numdirs, sizeof(unsigned char *));
    editor->numdirs=numdirs;
    for (i=0; i<numdirs; i++)
      editor->dirs[i]=strdup(dirs[i]);


    if (editor->cwin) {
      McChoiceSetList(editor->catgad,
		      (const unsigned char **)editor->dirs, editor->numdirs);
      if (last) {
	for (i=0; i<numdirs; i++) {
	  if (!strcmp(last, dirs[i])) {
	    McChoiceSelect(editor->catgad, i);
	    break;
	  }
	}
	free(last);
      }
    }
  }
}
