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
#include <sys/types.h>
#include <sys/time.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <strings.h>
#include <malloc.h>
#include <errno.h>
#include <sys/soundcard.h>
#include <stdlib.h>

#include "volume.h"
#include "xplaycd.h"

#include <McTools/McApp.h>
#include <X11/Xatom.h>
#include <McTools/McResource.h>
#include <McTools/McGadget.h>
#include <McTools/McFocus.h>
#include <McTools/McSlider.h>
#include <McTools/McText.h>
#include <McTools/McBitmap.h>
#include <McTools/McOrder.h>
#include <McTools/McDigits.h>
#include <McTools/McMenu.h>
#include <McTools/McInfoRequest.h>
#ifdef DEBUG_CODE
#include <McTools/McDebug.h> /* Geesh, blows up code... */
#endif

#include "icons.h"
#include "cd.h"
#include "struct.h"
#include "hardware.h"
#include "discid.h"
#include "xpedit.h"
#include "xmdbmessage.h"

#ifdef USE_RC5
#include <rcon.h>
#include "remotekeys.h"
int remotefd = -1, use_remote = -1;
rc5_keysym keysym[NUM_KEYSYMS];
char *knames[] = RKEYS;
static int remote_cue = 0;
static void remote_action(remote_data *rd);
static void remote_toggle(int id);
static void remote_press(int id);
static void remote_fast(int id, int idoff);
static void remote_nofast(void);
static void remote_change_volume(int dl, int dr);
#endif

/* Private routines
 * ================
 */
static void mainloop(void);
static void button_down_proc(McGadget *gadget);
static void button_up_proc(McGadget *gadget);
/*static void menu_proc(McGadget *gadget);*/
static void menu_item_proc(int id, McMenuItem *item);
static void goto_proc(McGadget *gadget);
static void replay_proc(McGadget *gadget);
static void slider_proc(McGadget *gadget);
static  int event_proc(McWindow *window, XEvent *event);
static void AddButton(int indx, const string_t tip);
static void AddMessage(int x, int y, int width, int txt_x, char *title);
static void AddBitmap(int indx, int x, int y);
static void CreateMyWindow(void);

/*  Public routines
 * ===============
 */
static int update_volume(int left, int right);

/* Public variables
 * ================
 */

int use_mixer = -1;    /* probe for mixer first */
int mixer_id  = SOUND_MIXER_CD; /* id of volume control used */
int emptypoll = 0;     /* An empty drive gets polled for a new cd when set */
int active_search = 0; /* Search actively with CDROMPLAYMSF commands */
int poll_out  = 0;     /* After eject, go on polling for a few seconds */
int poll_rate=333333;

/* Don't change this, please use the resource database
 * /usr/lib/X11/app-defaults/XPlaycd
 */
char *mixer_name = "/dev/mixer";
char *cdrom_name = "/dev/cdrom";

/*
 * The names of each mixer volume control, so the user can pick his favorite.
 */
const unsigned char *sources[]=SOUND_DEVICE_NAMES;


McGadget	*Gad[NUM_GADGETS];
McApp		*app = NULL;
McWindow	*mainWindow;
McMenuList	*mainMenu = NULL;

int             NeedToPoll = 0;
int             GadCnt;
char		*myname;
int		my_argc;
char 		**my_argv;

McMenuInit mainItems[] = {
  MENUTITLE(__("Main Menu")),
  MENUDOUBLELINE,
  MENUITEM(__("Edit"),       'E',  ITEM_ENABLED | ITEM_H3D, MAIN_EDIT),
  MENUITEM(__("Reload"),     'R',  ITEM_ENABLED | ITEM_H3D, MAIN_REREAD),
  MENUITEM(__("Save"),       'W',  ITEM_ENABLED | ITEM_H3D, MAIN_WRITE),
#ifdef DEBUG_CODE
  MENULINE,
  MENUITEM(__("Diskinfo"),     0,  ITEM_ENABLED | ITEM_H3D, MAIN_PRINT),
  MENUITEM(__("Trackinfo"),    0,  ITEM_ENABLED | ITEM_H3D, MAIN_TRACK),
  MENUITEM(__("Memory usage"), 'M',  ITEM_ENABLED | ITEM_H3D, MAIN_DEBUG),
#endif
  MENULINE,
  MENUITEM(__("Exit"),       'X',  ITEM_ENABLED | ITEM_H3D, MAIN_EXIT),
};

static XrmOptionDescRec userTable[] = {
  { "-usemixer",  "*useMixer",     XrmoptionSepArg , (caddr_t)NULL  },
  { "-mixer",     "*useMixer",     XrmoptionNoArg ,  (caddr_t)"cd"  },
  { "-nomixer",   "*useMixer",     XrmoptionNoArg ,  (caddr_t)"off" },
  { "-emptypoll", "*emptyPoll",    XrmoptionNoArg ,  (caddr_t)"off" },
  { "-rate",      "*pollRate",     XrmoptionSepArg,  (caddr_t)NULL  },
  { "-start",     "*autostart",    XrmoptionNoArg,   (caddr_t)"on"  },
#ifdef USE_RC5
  { "-rc5",       "*useRc5",       XrmoptionNoArg ,  (caddr_t)"on"  },
  { "-norc5",     "*useRc5",       XrmoptionNoArg ,  (caddr_t)"off" },
#endif
};

static struct timeval tout = { 0L, 1L }; /* provocate first timeout */

/**************************************************************************
 *  main(int ac, char **argv)
 *
 */
void main(int argc, char **argv) {
  int	left, right;
  char *str;

  myname = argv[0];
  my_argc = argc; my_argv = argv;

  if ((argc==2) && (!strcmp(argv[1], "-v"))) {
    printf("%s\n", APP_VERSION);
    cleanup(0);
  }

  /*
   * Build connection with X server
   */
  app = McCreateApp(&argc, argv, APP_VERSION, APP_CLASS,
		    userTable, sizeof(userTable)/sizeof(userTable[0]));
  if (argc>1) {
    fprintf(stderr, _("%s: Too many arguments\n"), myname);
    cleanup(1);
  }

  if ((str=McGetResource(app, "mixerDevice"))) {
    mixer_name=str;
  }

  emptypoll=McGetSwitch(app, "emptyPoll");

  active_search=McGetSwitch(app, "activeSearch");

  if ((str=McGetResource(app, "pollRate"))) {
    poll_rate=atoi(str);
    if (poll_rate<10000) {
      fprintf(stderr,_("%s: Poll rate too low, using 1/3s.\n"),myname);
      poll_rate=333333;
    }
  }

  if ((str=McGetResource(app, "useMixer"))) {
    if (!strcasecmp(str,"off")) {
      use_mixer=0;
    } else if (!strcasecmp(str,"on")) { /* For backward compatibility */
      use_mixer=1;
      mixer_id=SOUND_MIXER_CD;
    } else if (!strcasecmp(str,"auto")) {
      use_mixer=-1;
    } else {
      int i;

      use_mixer=0;
      for(i=0; sources[i]; i++) {
	if (!strcasecmp(sources[i], str)) {
	  mixer_id=i;
	  use_mixer=1;
	  break;
	}
      }

      if (!use_mixer) {
	fprintf(stderr,
		_("%s: Mixer id `%s' not supported. Supported ids are:\n"),
		myname, str);
	for(i=0; sources[i]; i++) fprintf(stderr, "%s ", sources[i]);
	fputc('\n', stderr);
	exit(1);
      }
    }
  }

  if ((str=McGetResource(app, "cdromDevice"))) {
    cdrom_name=str;
  }

  /*
   * Connect to the cdrom
   */
  cd_init();

#ifdef USE_RC5
  /*
   * Connect to the remote control daemon
   */

  if ((str=McGetResource(app, "useRc5"))) {
    if (!strcasecmp(str,"auto"))
      use_remote=-1;
    else
      use_remote=McTestSwitch(str);
  }


  if (use_remote) {
    if ((remotefd=rc5_connect())<0) {
      if (use_remote==1) {
	fprintf(stderr, "%s: ", myname);
	perror("Can't connect to rc5d");
	cleanup(1);
      }
    }
  } else {
    remotefd=-1;
  }

  /* Get all those keysyms */
  if (remotefd>=0) {
    int i;
    for (i=0; i<NUM_KEYSYMS; i++) {
      keysym[i]=rc5_query_keysym(remotefd, knames[i]);
    }
  }
#endif

  CreateMyWindow();

  if (use_mixer>0) {
    if (mix_open(mixer_id)<0) {
      fprintf(stderr,_("%s: Can't open %s: "),myname, mixer_name);
      perror("");
      cleanup(1);
    }
  } else if (use_mixer<0) {
    if (mix_open(mixer_id)>=0) {
      use_mixer=mixer_id;
    } else {
      use_mixer=0;
    }
  }

  if (use_mixer) {
    left=right=-1;
    mix_set_volume(mixer_id, &left, &right);
  } else {
    left=right=50;
    cd_volume(left, right);
  }
  update_volume(left, right);

  if (McGetSwitch(app, "autostart") && (cur_cdmode!=CDPLAY)) start_cd();

  /*
   * Now do the main loop
   */
  mainloop();
}

/**************************************************************************
 *
 * Create the window
 */
static void CreateMyWindow(void) {
  XWMHints	 wm_hints;
  XSizeHints	 sizehints;
  int i;

  if (mainWindow) McFreeWindow(mainWindow);

  if (!(app->h || app->w)) {
    app->h=162; app->w=366;
  }
  mainWindow=McCreateAppWindow(app,-1,-1,-1,-1,NULL, event_proc);
  McAddInput(mainWindow, ButtonPressMask);

  /*
   * Create all bitmaps
   */
  for (i=NUM_BITMAPS; --i>=0;) McCreateBitmapFromData(mainWindow, &pmap[i]);

  memset(&sizehints, 0, sizeof(XSizeHints));
  memset(&wm_hints, 0, sizeof(XWMHints));
  wm_hints.icon_pixmap = pmap[PIX_ICON].pixmap;
  wm_hints.icon_mask = pmap[PIX_ICONMASK].pixmap;
  wm_hints.flags = IconPixmapHint | IconMaskHint;
  sizehints.flags = PMinSize | PMaxSize;
  sizehints.min_width   = 57; sizehints.max_width  = app->w;
  sizehints.min_height  = 37; sizehints.max_height = app->h;
  McSetHints(mainWindow, NULL, my_argc, my_argv, &sizehints, &wm_hints);

  GadCnt=1;
  Gad[0] = McCreateGadget(mainWindow,GAD_H3D|GAD_3D|GAD_ACTIVE, BOOLGADGET,
			  10, 11, 39, 20);
  Gad[0]->normalLabel = McCreateText(mainWindow, _("Exit"), app->gc[GC_NORMAL],
				     app->defaultFont, 0, 4);
  Gad[0]->normalLabel->x=1+((39-Gad[0]->normalLabel->width)>>1);
  Gad[0]->callbackUp = button_up_proc;
  Gad[0]->id = 0;
  Gad[0]->tip = _("Guess what?");
  McAddFocusGadget(Gad[0], 1);

  AddButton(PIX_EJECT, _("Load/Eject"));
  AddButton(PIX_REWIND, _("Search backward"));
  Gad[GadCnt-1]->callbackUp = button_up_proc;
  McRemoveFocusGadget(Gad[GadCnt-1]); /* FIXME */
  AddButton(PIX_PLAY, _("Play/Stop"));
  mainWindow->mainButton=Gad[GadCnt-1];
  McResizeGadget(Gad[GadCnt-1], Gad[GadCnt-1]->width+13, -1);
  AddButton(PIX_FF, _("Search forward"));
  Gad[GadCnt-1]->callbackUp = button_up_proc;
  McRemoveFocusGadget(Gad[GadCnt-1]); /* FIXME */
  AddButton(PIX_PAUSE, _("Pause"));
  Gad[GadCnt-1]->flags &= ~GAD_HMASK;
  Gad[GadCnt-1]->flags |= GAD_NOTOGGLE;
  AddButton(PIX_SHUFFLE, _("Shuffle tracks"));
  AddButton(PIX_REPEAT, _("Repeat mode"));
  Gad[GadCnt-1]->flags |= GAD_TOGGLE;
  AddButton(PIX_PREV, _("Previous track"));
  McMoveGadget(Gad[GadCnt-1], Gad[BTN_REWIND]->x, 40);
  AddButton(PIX_NEXT, _("Next track"));
  McMoveGadget(Gad[GadCnt-1], Gad[BTN_FF]->x, 40);

  AddMessage( 34, 40, 57, 12, "00:00");
  Gad[GadCnt-1]->flags |=
    GAD_ACTIVE | GAD_NOTOGGLE | GAD_3D | GAD_SELECTED | GAD_NOSTIPPLE;
  Gad[GadCnt-1]->callbackDown = button_down_proc;
  Gad[GadCnt-1]->normalBitmap = &pmap[PIX_T];
  McInsertFocusGadget(NULL, Gad[GadCnt-1], Gad[GadCnt-3] , 1);

  AddMessage(143, 40, 45,  8, "");
  AddMessage(240, 40, 56,  8, "");

  AddBitmap(PIX_CDLOGO, 300, 48);
  AddBitmap(PIX_VOLUME,   9, 73);
  AddBitmap(PIX_CLOCK,    9, 43);

  Gad[GadCnt] = McCreateGadget(mainWindow,
			       GAD_3D | GAD_H3D | GAD_ACTIVE | GAD_SELECTED,
			       SLIDERGADGET, 34, 69, 262, 25);
  Gad[GadCnt]->specialInfo = McCreateSlider(SLIDER_STEREO, 100);
  ((McSlider *)(Gad[GadCnt]->specialInfo))->step = 3;
  Gad[GadCnt]->callbackDown = slider_proc;
  McAddFocusGadget(Gad[GadCnt], 1);
  Gad[GadCnt]->id = GadCnt++;

  Gad[GadCnt] = McCreateGadget(mainWindow,
			       GAD_3D | GAD_H3D | GAD_ACTIVE | GAD_SELECTED,
			       ORDERGADGET, 10, 103, 346, 18);
  Gad[GadCnt]->specialInfo = McCreateOrder(mainWindow,
					   ORD_PRINTID | ORD_SCROLL);
  Gad[GadCnt]->callbackDown = goto_proc;
  Gad[GadCnt]->callbackUp = replay_proc;
  ORDER(Gad[GadCnt])->ItemHeight = ORDER(Gad[GadCnt])->ItemWidth = 10;
  Gad[GadCnt]->id = GadCnt++;

  AddMessage(10, 130, 346,  12, "");

  /*
   * Menu init
   */
  mainMenu=McCreateMenu(mainWindow, menu_item_proc, mainItems,
			MENU_ITEMS(mainItems),MENU_CENTER|MENU_HOTKEY);
  /*
   * Display the window
   */
  McInitGadgets(mainWindow);
  McMapWindow(mainWindow);
  McSetFocus(mainWindow->mainButton);
  cd_update(-1);
}

/**************************************************************************
 * static void button_XXX_proc(McGadget *gadget)
 *
 */
static void button_down_proc(McGadget *gadget) {
  if (gadget->id==BTN_EXIT) cleanup(0);
  cd_update(cd_action(gadget->id, 1));
}

static void button_up_proc(McGadget *gadget) {
  if (gadget->id==BTN_EXIT) cleanup(0);
  if (cur_cdmode==CDERROR) return;
  cd_update(cd_action(gadget->id, 0));
}

/**************************************************************************
 * static void menu_item_proc(int id, McMenuItem *item)
 *
 */
static void menu_item_proc(int id, McMenuItem *item) {
  unsigned char *tmp;

  switch(id) {
  case MAIN_EXIT:
    cleanup(0);
#ifdef DEBUG_CODE
  case MAIN_DEBUG:
    McMstats(app);
    break;
  case MAIN_TRACK:
    pr_trackinfo(app, cur_track);
    break;
  case MAIN_PRINT:
    if (!(tmp=currentFile)) tmp="<none>";
    McInfo(app, "&L:ID of current disc: 0x%08x\n"
	   "&L:Current file name: %s\n",
	   get_discid(), tmp);
#endif
  case MAIN_REREAD:
    update_db(1);
    break;

  case MAIN_EDIT:
    create_edit_window();
    break;

  case MAIN_WRITE:
    write_db(currentFile);
    break;

  }
}

/**************************************************************************
 * static void goto_proc(McGadget *gadget)
 *
 */
static void goto_proc(McGadget *gadget) {
  cd_update(cd_action(gadget->id, (ORDER(gadget)->grip)->id));
}

static void replay_proc(McGadget *gadget) {

  /* Be sure that the stop mark is behind the start mark. If not, swap them */
  McOrderItem *st, *it = ORDER(Gad[ORD_SONGS])->first;
  while(it && (it->id != DIG_RIGHT_ARROW) && (it->id !=DIG_LEFT_ARROW))
    it=it->next;
  if (!it) return; /* Empty list? */
  if (it->id==DIG_LEFT_ARROW) {
    st=it;
    while(it && (it->id != DIG_RIGHT_ARROW)) it=it->next;
    if (!it) return; /* Geesh, bug! */
    st->id=DIG_RIGHT_ARROW;
    it->id=DIG_LEFT_ARROW;
    McGadgetUpdate(Gad[ORD_SONGS]);
    XFlush(app->display);
  }
  cd_update(cd_action(gadget->id, -1));
}

/**************************************************************************
 * VOLUME VOLUME VOLUME VOLUME VOLUME VOLUME VOLUME VOLUME VOLUME VOLUME */

#define _SLIDER(gad) ((McSlider *)((gad)->specialInfo))

static void slider_proc(McGadget *gadget) {
  int left, right;
  left = _SLIDER(Gad[SLD_VOLUME])->leftValue;
  right = _SLIDER(Gad[SLD_VOLUME])->rightValue;
  if (use_mixer) {
    mix_set_volume(mixer_id, &left, &right);
    update_volume(left, right);
  } else {
    cd_volume(left, right);
  }
}

static int update_volume(int left, int right) {
  if ((left>=0) && (right>=0) &&
      ((left != (_SLIDER(Gad[SLD_VOLUME])->leftValue)) ||
       (right != (_SLIDER(Gad[SLD_VOLUME])->rightValue)))) {
    _SLIDER(Gad[SLD_VOLUME])->leftValue = left;
    _SLIDER(Gad[SLD_VOLUME])->rightValue = right;
    McGadgetUpdate(Gad[SLD_VOLUME]);
    return 1;
  }
  return 0;
}

/**************************************************************************
 * static int event_proc(McWindow *mcw, XEvent *event)
 *
 */
static int event_proc(McWindow *mcw, XEvent *event) {

  switch(event->type) {

  case ButtonPress:
    if (event->xbutton.button==3) {
      McShowMenu(app, NULL, mainMenu, 0, 0);
      return 1;
    } else {
      return 0;
    }
  
  case ClientMessage:
    if (mcw==mainWindow) {
      if ((event->xclient.format == 32) &&
	  (event->xclient.data.l[0] == app->wmDelWin)) cleanup(0);

      /* Check if media editor has something to say... */
      if ((event->xclient.format == 32) &&
	  (event->xclient.data.l[0] == XA_NOTICE) &&
	  (event->xclient.data.l[1] == editor_pid)) {
	switch(event->xclient.data.l[2]) {
	case XMDB_FILE_WRITTEN:
	  update_db(1);
	  break;
	default:
	  printf("Client message from %ld saying: %ld\n",
		 event->xclient.data.l[1], event->xclient.data.l[2]);
	  break;
	}
      }

      return 1;
    }
  }
  return 0;
}

/**************************************************************************
 * static void mainloop()
 *
 */
static void mainloop(void) {
  fd_set rdfds, usefds;
  int width;

  FD_ZERO(&rdfds);
  width=0;

#ifdef USE_RC5
  if (remotefd>=0) {
    FD_SET(remotefd, &rdfds);
    if (remotefd>width) width=remotefd;
  }
#endif

  while(1) {
    if ((!tout.tv_usec) && (!tout.tv_sec)) {
      if (cue)
	tout.tv_usec = 50000;  /* Poll faster while searching */
      else
	if ((mainWindow->window_visible) && (cur_cdmode==CDPLAY))
	  tout.tv_usec =  poll_rate; /* three polls per second should be
				      * enough to have a smooth display */
	else
	  tout.tv_usec = 1000000; /* When the window is iconified, we only
				     have to poll to ensure repeat and
				     playlist functions. Therefore, once per
				     second is enough then. */
      tout.tv_sec  = 0;
    }
    usefds=rdfds;
    if (poll_out || (((mainWindow->window_visible) || NeedToPoll) &&
		     (cur_cdmode!=CDERROR) &&
		     (emptypoll || ((cur_cdmode!=CDEJECT) &&
				    (cur_cdmode!=CDSTOP))))) {
      McAppSelect(app, width+1, &usefds, NULL, NULL, &tout);
      if (poll_out) poll_out--;
    } else {
      McAppSelect(app, width+1, &usefds, NULL, NULL, NULL);
    }    

    if ((!tout.tv_sec) && (!tout.tv_usec)) {
      int cds;
      cds=cd_action(REQ_POLL, 0);
      if (use_mixer) {
	int l,r;
	mix_get_volume(mixer_id, &l, &r);
	update_volume(l,r);
      }
      cd_update(cds);
    }

#ifdef USE_RC5
    if (remotefd>=0) {
      if (FD_ISSET(remotefd, &usefds)) {
	remote_data rd;
	if (rc5_read_socket(remotefd, &rd)<0) {
	  McError(app, "Can't read from rc5d socket\nReason: %s\n",
		  strerror(errno));
	  close(remotefd);
	  remotefd=-1;
	} else {
	  remote_action(&rd);
	}
      }
    }
#endif

  }
}

/**************************************************************************
 * PRIVATE PRIVATE PRIVATE PRIVATE PRIVATE PRIVATE PRIVATE PRIVATE PRIVATE
 */

#ifdef USE_RC5
static void remote_action(remote_data *rd) {
  int key, i;

  key = -1;
  for (i=0; i<NUM_KEYSYMS; i++) {
    if (keysym[i]==rd->keysym) { key=i; break; }
  }

  switch(key) {
  case RK_CUE:
    remote_fast(BTN_FF, BTN_REWIND);
    return;
  case RK_REW:
    remote_fast(BTN_REWIND, BTN_FF);
    return;
  }
  
  if (rd->usec>200000) {
    switch(key) {
    case RK_PLAY:
      remote_nofast();
      if ((cur_cdmode==CDSTOP) || (cur_cdmode==CDPAUSE)) {
	remote_press(BTN_PLAYSTOP);
      }
      return;
    case RK_STOP:
      remote_nofast();
      if ((cur_cdmode==CDPLAY) || (cur_cdmode==CDPAUSE))
	remote_press(BTN_PLAYSTOP);
      return;
    case RK_PAUSE:
      remote_nofast();
      remote_press(BTN_PAUSE);
      return;
    case RK_PREV:
      remote_nofast();
      remote_press(BTN_PREV);
      return;
    case RK_NEXT:
      remote_nofast();
      remote_press(BTN_NEXT);
      return;
    case RK_EJECT:
      remote_nofast();
      remote_press(BTN_EJECT);
      return;
    case RK_DISPLAY:
      cd_update(cd_action(MSG_TIME, 1));
      return;
    case RK_REPEAT:
      remote_toggle(BTN_REPEAT);
      return;
    }
  }

  switch(key) {
  case RK_VOLUP:
    remote_change_volume( 1,  1);
    return;
  case RK_VOLDOWN:
    remote_change_volume(-1, -1);
    return;
  }
}

static void remote_fast(int id, int idoff) {
  int f=0;

  if (Gad[idoff]->flags&GAD_SELECTED) {
    Gad[idoff]->flags&=~GAD_SELECTED;
    McGadgetRedraw(Gad[idoff]);
    f=1;
  }

  if (!(Gad[id]->flags&GAD_SELECTED)) {
    Gad[id]->flags|=GAD_SELECTED;
    McGadgetRedraw(Gad[id]);
    XFlush(app->display);
    cd_action(id, 1);
  } else if (f) XFlush(app->display);

  remote_cue = 1;
}

static void remote_nofast(void) {
  if (remote_cue) {
    remote_cue = 0;
    cue=0;
    if (Gad[BTN_FF]->flags&GAD_SELECTED) {
      Gad[BTN_FF]->flags&=~GAD_SELECTED;
      McGadgetRedraw(Gad[BTN_FF]);
    }
    if (Gad[BTN_REWIND]->flags&GAD_SELECTED) {
      Gad[BTN_REWIND]->flags&=~GAD_SELECTED;
      McGadgetRedraw(Gad[BTN_REWIND]);
    }
  }
}

static void remote_press(int id) {
  Gad[id]->flags|=GAD_SELECTED;
  McGadgetBusy(mainWindow, Gad[id]);
  XFlush(app->display);
  cd_action(id, 1);
  Gad[id]->flags&=~GAD_SELECTED;
  McGadgetRedraw(Gad[id]);
  XFlush(app->display);
}

static void remote_toggle(int id) {
  Gad[id]->flags^=GAD_SELECTED;
  McGadgetBusy(mainWindow, Gad[id]);
  XFlush(app->display);
  cd_action(id, Gad[id]->flags&GAD_SELECTED);
  McGadgetRedraw(Gad[id]);
  XFlush(app->display);
}

static void remote_change_volume(int dl, int dr) {
  int left = _SLIDER(Gad[SLD_VOLUME])->leftValue + dl;
  int right = _SLIDER(Gad[SLD_VOLUME])->rightValue + dr;
 
  if ( left  > 100 ) left  = 100;
  if ( right > 100 ) right = 100;
  if ( left  <   0 ) left  = 0;
  if ( right <   0 ) right = 0;

  if (use_mixer)
    mix_set_volume(mixer_id, &left, &right);
  else
    cd_volume(left, right);
  update_volume(left, right);
}

#endif



/**************************************************************************
 * GADGETS  GADGETS  GADGETS  GADGETS  GADGETS  GADGETS  GADGETS  GADGETS */

static void AddButton(int indx, const string_t tip) {
  Gad[GadCnt] = McCreateGadget(mainWindow, GAD_3D | GAD_H3D | GAD_ACTIVE, BOOLGADGET,
			       Gad[GadCnt-1]->x + Gad[GadCnt-1]->width + 10,
			       Gad[GadCnt-1]->y, 32, 20);
  Gad[GadCnt]->callbackDown = button_down_proc;
  Gad[GadCnt]->normalBitmap = &pmap[indx];
  Gad[GadCnt]->tip = tip;
  McAddFocusGadget(Gad[GadCnt], 1);
  Gad[GadCnt]->id = GadCnt++;
}


static void AddMessage(int x, int y, int width, int txt_x, char *title) {
  Gad[GadCnt] = McCreateGadget(mainWindow, GAD_3D | GAD_H3D | GAD_SELECTED,
			       BOOLGADGET, x, y, width, 20);
  Gad[GadCnt]->normalLabel = McCreateText(mainWindow, title, app->gc[GC_NORMAL],
					  app->defaultFont, txt_x, 3);
  Gad[GadCnt]->id = GadCnt++;
}


static void AddBitmap(int indx, int x, int y) {
  Gad[GadCnt] = McCreateGadget(mainWindow, GAD_HNONE, BOOLGADGET, x, y,
			       pmap[indx].width, pmap[indx].height);
  Gad[GadCnt]->normalBitmap = &pmap[indx];
  Gad[GadCnt]->id = GadCnt++;
}

/**************************************************************************
 * BE CLEAN  BE CLEAN  BE CLEAN  BE CLEAN  BE CLEAN  BE CLEAN  BE CLEAN   */

void cleanup(int r) {
  close_editor();
  McFreeApp(app);
  mix_close();
  if (currentFile) free(currentFile);
#ifdef USE_RC5
  if (remotefd>=0) close(remotefd);
#endif
  exit(r);
}

