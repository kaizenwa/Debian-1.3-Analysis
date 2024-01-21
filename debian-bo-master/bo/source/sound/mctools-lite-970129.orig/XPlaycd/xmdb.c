/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2, or (at your option)
     any later version.

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
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <errno.h>
#include <dirent.h>

#include <McTools/McApp.h>
#include <X11/Xatom.h>
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

#include "getdir.h"
#include "cathegory.h"
#include "xmdb.h"
#include "xmedit.h"
#include "mover.h"
#include "newdir.h"
#include "xmdbmessage.h"

/**************************************************************************/

McApp *app;
McWindow *mainWindow;
McGadget *mediagad, *cathgad;
Window parent; /* Send ClientMessage event whenever the editor writes a file */

static XrmOptionDescRec userTable[] = {
  { "-edit",    "*directEdit",     XrmoptionSepArg, (caddr_t)NULL },
  { "-new",     "*newMedia",       XrmoptionSepArg, (caddr_t)NULL },
  { "-count",   "*newCount",       XrmoptionSepArg, (caddr_t)NULL },
  { "-notify",  "*notifyWindow",   XrmoptionSepArg, (caddr_t)NULL },
  { "-root",    "*cddb",           XrmoptionSepArg, (caddr_t)NULL },
};

/**************************************************************************/

void cleanup(int r);
static void usage(void);
static  int event_proc(McWindow *window, XEvent *event);
static void make_buttons(McWindow *mcw);

static void edit_proc(McGadget *gadget);
static void exit_proc(McGadget *gadget);
static void delete_proc(McGadget *gadget);
static void move_proc(McGadget *gadget);
static void new_proc(McGadget *gadget);
static void select_down(McGadget *gadget);
static void choice_up(McGadget *gadget);
static unsigned char *make_path(const unsigned char *cath);
static unsigned char *current_path(void);
static void term_proc(int arg);
static void tell_parent(int what);

static unsigned char **filep=NULL;
static unsigned char **dirp =NULL;
static unsigned char **namep=NULL;
static int files=0, dirs=0;
static unsigned char *cathegory=NULL;
static unsigned char *root;
static int current;

/**************************************************************************/

void main(int argc, char **argv) {
  XWMHints	 wm_hints;
  XSizeHints	 sizehints;
  unsigned char *direct;
  unsigned char *foo;
  int main_is_mapped;

  if ((myname = strrchr(argv[0],'/'))) myname++; else myname=argv[0];

  /*
   * Build connection with X server
   */
  app = McCreateApp(&argc, argv, APP_VERSION, APP_CLASS,
		    userTable, sizeof(userTable)/sizeof(userTable[0]));

  if ((argc==2) && ((!strcmp(argv[1],"-?")) || (!strcmp(argv[1],"-h"))))
    usage();

  if (argc>1) {
    fprintf(stderr, _("%s: Too many arguments\n"), myname);
    cleanup(1);
  }

  foo=McGetResource(app, "notifyWindow");
  if (foo) parent=(Window)atoi(foo); else parent=(Window)0;

  /*
   * Create the window
   */
  if (!(app->h || app->w)) {
    app->h=300; app->w=438;
  }
  mainWindow=McCreateAppWindow(app,-1,-1,-1,-1, NULL, event_proc);
/*  McAddInput(mainWindow, PropertyChangeMask); */

  memset(&sizehints, 0, sizeof(XSizeHints));
  memset(&wm_hints, 0, sizeof(XWMHints));
  sizehints.flags = PMinSize;
  sizehints.min_width   = 438; sizehints.min_height  = 100;
  McSetHints(mainWindow, NULL, argc, argv, &sizehints, &wm_hints);

  make_buttons(mainWindow);
  McInitGadgets(mainWindow);

  root=McGetResource(app, "cddb");
  if (!root) {
    McError(app, _("cddb resource is not set.\nUsing current directory.\n"));
    root=".";
  }

  scandb(1);

  main_is_mapped=0;
  if ((direct=McGetResource(app, "directEdit"))) {
    create_edit_window(app, direct, root, 0, dirp, dirs);
  } else if ((direct=McGetResource(app, "newMedia"))) {
    int count=0;
    foo=McGetResource(app, "newCount");
    if (foo) count=(Window)atoi(foo); else cleanup(1); /* Shouldn't happen */
    create_edit_window(app, direct, root, count, dirp, dirs);
  } else {
    McMapWindow(mainWindow);
    main_is_mapped=1;
  }

  signal(SIGTERM, term_proc);

  do {
    McAppSelect(app, 0, NULL, NULL, NULL, NULL);
  } while (main_is_mapped || is_editing());

  cleanup(0);
}

/**************************************************************************/

/* No funny message if killed by SIGTERM from XPlaycd */
static void term_proc(int arg) {
  cleanup(0);
}

void cleanup(int r) {
  McFreeApp(app);
  exit(r);
}

/**************************************************************************/

static void usage(void) {
  printf(_("Usage: %s\n"), myname);
  cleanup(1);
}

/**************************************************************************/

static int event_proc(McWindow *mcw, XEvent *event) {

  switch(event->type) {

#if 0
  case ButtonPress:
    if (event->xbutton.button==3) {
      McShowMenu(app, NULL, mainMenu,
		 event->xbutton.x, event->xbutton.y);
      return 1;
    } else {
      return 0;
    }
#endif
  
  case ClientMessage:
    if ((event->xclient.format == 32) &&
	(event->xclient.data.l[0] == app->wmDelWin)) {
      if (mcw==mainWindow) cleanup(0);
      return 1;
    }

/*
  case PropertyNotify:
    printf("got PropertyNotify  state: %d atom: %s\n",
	   event->xproperty.state,
	   XGetAtomName(app->display, event->xproperty.atom));
    return 1;
*/
    
  }

  return 0;
}

/**************************************************************************/

void scandb(int changed) {
  unsigned char *oldpath;
  int sel;

  if (changed) {
    oldpath=NULL;
  } else {
    oldpath=current_path();
  }

  if (!read_dir_in_buffer(root, cathegory,
			  &filep, &dirp, &namep, &files, &dirs, &current)) {
    McError(app, _("Cannot read directory '%s/%s'\nReason: %s\n"),
	    root, cathegory, strerror(errno));
    if (oldpath) free(oldpath);
  }


  /* Check if the previously highlighted media still exists in the
   * current list of files and if so, highlight it.
   */
  sel=0;
  if (oldpath) {
    unsigned char *bar, *foo=current_path();
    if (foo && (bar=strrchr(foo, '/'))) {
      int i;
      bar++;
      for(i=0; i<files; i++) {
	strcpy(bar, filep[i]);
	if (!strcmp(oldpath, foo)) {
	  sel=i;
	  break;
	}
      }
    }
    free(foo);
    free(oldpath);
  }

  McChoiceSetList(cathgad, dirs?(const unsigned char **)dirp:NULL, dirs);
  McChoiceSelect(cathgad, current);
  McSelectorSetList(mediagad, files?(const unsigned char **)namep:NULL,
		    files, sel);
}

/* The editor calls this whenever someone presses apply or close
 * this function checks if the edited media is currently beeing
 * displayed, and if so, it re-reads the file and updates the
 * display because the title may have changed.
 */
void update_line(unsigned char *path) {
  int i;
  int len;
  unsigned char *buf, *file;

  /* There might be a parent process lurking for
   * changes in the edited file. Tell him!
   */
  tell_parent(XMDB_FILE_WRITTEN);

  len=strlen(path);
  if ((len<9) || (path[len-9]!='/')) {
    McError(app, _("Invalid path for update:\n`%s'\n"), path);
    return;
  }

  if (!files) return;

  len=strlen(root)+16; if (cathegory) len+=strlen(cathegory);
  buf = alloca(len);

  strcpy(buf, root);
  if (buf[(len=strlen(buf))-1]!='/') { buf[len++]='/'; buf[len]=0; }
  if (cathegory) {
    strcat(buf, cathegory);
    if (buf[(len=strlen(buf))-1]!='/') { buf[len++]='/'; buf[len]=0; }
  }
  file=&buf[len];

  for (i=0; i<files; i++) {
    strcpy(file, filep[i]);
    if (!strcmp(buf, path)) {
      if (namep[i]) free(namep[i]);
      namep[i]=read_media_name(path);
      McSelectorRefreshLine(mediagad, i);
      break;
    }
  }
}

/* This function checks if the media pointed to by path is currently
 * displayed, and if so, it removes it from the display.
 */
static void remove_line(unsigned char *path) {
  int i;
  int len;
  unsigned char *buf, *file;
  int oldselection;

  len=strlen(path);
  if ((len<9) || (path[len-9]!='/')) {
    McError(app, _("Invalid path for update:\n`%s'\n"), path);
    return;
  }

  if (!files) return;

  len=strlen(root)+16; if (cathegory) len+=strlen(cathegory);
  buf = alloca(len);

  strcpy(buf, root);
  if (buf[(len=strlen(buf))-1]!='/') { buf[len++]='/'; buf[len]=0; }
  if (cathegory) {
    strcat(buf, cathegory);
    if (buf[(len=strlen(buf))-1]!='/') { buf[len++]='/'; buf[len]=0; }
  }
  file=&buf[len];

  for (i=0; i<files; i++) {
    strcpy(file, filep[i]);
    if (!strcmp(buf, path)) {
      oldselection=McSelectorSelection(mediagad);
      if (namep[i]) free(namep[i]);
      if (filep[i]) free(filep[i]);
      if (i+1<files) {
	memmove(&namep[i], &namep[i+1], (files-i-1)*sizeof(unsigned char *));
	memmove(&filep[i], &filep[i+1], (files-i-1)*sizeof(unsigned char *));
      }
      files--;
      if (i<oldselection) oldselection--;
      if (oldselection<0) oldselection=0;
      if (oldselection>=files) oldselection=files-1;
      McSelectorSetList(mediagad, files?(const unsigned char **)namep:NULL,
			files, oldselection);
      break;
    }
  }
}

/* This function moves a file into the new cathegory
 * If this succeeds, it the checks if the file disappeared or newly
 * appearead in the current displayed cathegory
 * The display is then updated accordingly.
 */
void move_line(unsigned char *oldpath, const unsigned char *cath) {
  unsigned char *newpath, *base, *curpath;
  int rescan=0;
  struct stat st;

  if (!strcmp(cath, NO_CATHEGORY)) cath=NULL;
  newpath=make_path(cath);

  /* Check if the file will have to appear in the current selection */
  if ((curpath=current_path())) {
    if (!strncmp(curpath, newpath, strlen(newpath))) rescan=1;
    free(curpath);
  }

  if ((base=strrchr(oldpath, '/'))) base++; else base=oldpath;
  strcat(newpath, base);

  if (!stat(newpath, &st)) {
    /* Some day, we could add a "Try it anyway?" button */
    McError(app,_("Ooops!\n"
		  "There is already a file for this\n"
		  "media in the destination cathegory.\n"
		  "You should clean up that mess in your database!\n"));
    free(newpath);
    return;
  }
	    
  if (rename(oldpath, newpath)) {
    McError(app,
	    _("Couldn't rename\n\"%s\"\ninto\n\"%s\"\nReason:%s\n"),
	    oldpath, newpath, strerror(errno));
    free(newpath);
    return;
  }

  free(newpath);

  /* If the new path has to show up
   * in the current selection gadget, rescan...
   */

  if (rescan) {
    scandb(0);
    return;
  }

  /* If the new path doesn't have to show up in the current selection,
   * perhaps the old one has to disappear now?
   */
  remove_line(oldpath);
}


/**************************************************************************/

static void tell_parent(int what) {
  if (parent) {
    XClientMessageEvent event;
    event.type=ClientMessage;
    event.send_event=True;
    event.display=app->display;
    event.window=parent;
    event.message_type=XA_INTEGER;
    event.format=32;
    event.data.l[0]=XA_NOTICE;
    event.data.l[1]=getpid();
    event.data.l[2]=what;
    XSendEvent(app->display, parent, True, 0, (XEvent *)(&event));
  }
}

/**************************************************************************/

/*
 * Create a new directory, be sure it makes its way into the
 * current list.
 *
 */
void new_cathegory(unsigned char *path) {
  int umsk;

  umsk = umask(077); umask(umsk);
  if (mkdir(path, 0755 & (~umsk))) {
    McError(app, _("Unable to create \"%s\"\nReason: %s\n"),
	    path, strerror(errno));
    return;
  }

  scandb(0);
  update_editor_dirs(dirp, dirs);
}

/**************************************************************************/

static void make_buttons(McWindow *mcw) {
  McGadget *gad;

  gad=MakeButton(mcw, 12, 278, 72, 16, 0, _("Edit"), edit_proc);
  gad->topLeftGravity = SouthWestGravity;
  McAddFocusGadget(gad, 1);
  mcw->mainButton=gad;

  gad=MakeButton(mcw, 97, 278, 72, 16, 0, _("Move"), move_proc);
  gad->topLeftGravity = SouthWestGravity;
  McAddFocusGadget(gad, 1);

  gad=MakeButton(mcw, 182, 278, 72, 16, 0, _("New"), new_proc);
  gad->topLeftGravity = SouthWestGravity;
  McAddFocusGadget(gad, 1);

  gad=MakeButton(mcw, 268, 278, 72, 16, 0, _("Delete"), delete_proc);
  gad->topLeftGravity = SouthWestGravity;
  McAddFocusGadget(gad, 1);

  gad=MakeButton(mcw, 354, 278, 72, 16, 0, _("Exit"), exit_proc);
  gad->topLeftGravity = SouthWestGravity;
  McAddFocusGadget(gad, 1);

  cathgad=MakeChoice(mcw, 176, 10, 0, 0, 0, choice_up, NULL);
  McAddFocusGadget(cathgad, 1);
  McSetFocus(cathgad);

  mediagad=MakeSelector(mcw, 12, 40, 393, 226, 0,
		      app->defaultFont, NULL, select_down);
  mediagad->bottomRightGravity = SouthEastGravity;
  McAddFocusGadget(mediagad, 1);

  gad=MakeProp(mcw, 414, 40, 12, 226, 0, NULL);
  gad->topLeftGravity = NorthEastGravity;
  gad->bottomRightGravity = SouthWestGravity;
  McSelectorBindSlider(mediagad, gad);

}

/**************************************************************************/

/*
 * concatenates root and cath into a path, leaving room for the
 * base filename at the end.
 * Returns a malloced string.
 */
static unsigned char *make_path(const unsigned char *cath) {
  unsigned char *buf;
  int len;

  len=strlen(root)+16; if (cath) len+=strlen(cath);
  buf = malloc(len);
  if (!buf) return NULL;

  strcpy(buf, root);
  if (buf[(len=strlen(buf))-1]!='/') { buf[len++]='/'; buf[len]=0; }
  if (cath) {
    strcat(buf, cath);
    if (buf[(len=strlen(buf))-1]!='/') { buf[len++]='/'; buf[len]=0; }
  }
  return buf;
}

static unsigned char *current_path(void) {
  unsigned char *buf;
  int sel;

  if ((sel=McSelectorSelection(mediagad))<0) {
    XBell(app->display, 0);
    return NULL;
  }

  if ((sel>=files) || (!filep[sel])) return NULL;

  if (!(buf=make_path(cathegory))) return NULL;
  strcat(buf, filep[sel]);
  return buf;
}


/**************************************************************************/

static void edit_proc(McGadget *gadget) {
  unsigned char *buf;
  if ((files<=0) || (!(buf=current_path()))) { /* Nothing to edit */
    XBell(app->display, 0);
    return;
  }
  create_edit_window(app, buf, root, 0, dirp, dirs);
  free(buf);
}

/**************************************************************************/

static void exit_proc(McGadget *gadget) {
  cleanup(0);
}

/**************************************************************************/

static void move_proc(McGadget *gadget) {
  unsigned char *buf;
  if ((files<=0) || (!(buf=current_path()))) { /* Nothing to do */
    XBell(app->display, 0);
    return;
  }
  create_move_window(app, buf, current, root, dirp, dirs);
  free(buf);
}

/**************************************************************************/

static void delete_request(int choice, void *nonsense);

static unsigned char *delpath=NULL;

static void delete_proc(McGadget *gadget) {
  unsigned char *info;
  const unsigned char *sel;
  if (delpath || (!(delpath=current_path())) || (files<=0)) {
    XBell(app->display, 0);
    return;
  }

  sel=McSelectorSelectionString(mediagad);
  info=alloca(128+strlen(sel));
  sprintf(info, _("You are going to delete\n\n%s\n\nAre you sure?\n"), sel);
  McCreateInfoRequest(app, _("Delete"), info, _("Yes"), NULL, _("No"), 0,
		      delete_request);
}

static void delete_request(int choice, void *nonsense) {
  if (choice>0) {
    if (delpath) {
      if (unlink(delpath))
	McError(app, _("Failed to delete `%s'\nReason: %s\n"), delpath,
		strerror(errno));
      else
	remove_line(delpath);
    }
  }

  if (delpath) {
    free(delpath);
    delpath=NULL;
  }
}

/**************************************************************************/

static void new_proc(McGadget *gadget) {
  unsigned char *path;

  if (cathegory) {
    path=alloca(strlen(cathegory)+2);
    strcpy(path, cathegory);
    strcat(path, "/");
  } else {
    path="";
  }
  create_newdir_window(app, root, path);
}

/**************************************************************************/

static void select_down(McGadget *gadget) {
  McSelector *selector = (McSelector *)gadget->specialInfo;
  if (selector->flags & SEL_DOUBLE_CLICK) {
    edit_proc(NULL);
  }
}

/**************************************************************************/

static void choice_up(McGadget *gadget) {
  const unsigned char *new;

  new=McChoiceSelectionString(gadget);
  if (!strcmp(new, NO_CATHEGORY)) new=NULL;

  if ((cathegory&&new&&strcmp(cathegory, new)) ||
      (cathegory && (!new)) || ((!cathegory) && new)) {
    if (cathegory) free(cathegory);
    if (new) cathegory=strdup(new); else cathegory=NULL;
    scandb(1);
  } else {
    scandb(0);
  }
}





