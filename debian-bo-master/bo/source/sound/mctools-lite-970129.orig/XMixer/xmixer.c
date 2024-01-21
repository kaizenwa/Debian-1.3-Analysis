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

/**********************************************************************/
/*                                                                    */
/*  File:          xmixer.c                                           */
/*                                                                    */
/**********************************************************************/

#include <stdlib.h>

#include "../config.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <sys/soundcard.h>
#include <signal.h>

#include <McTools/McApp.h>
#include <McTools/McResource.h>
#include <McTools/McGadget.h>
#include <McTools/McFocus.h>
#include <McTools/McSlider.h>
#include <McTools/McText.h>
#include <McTools/McBitmap.h>
#include "icons.h"
#include "xmixer.h"

#include <X11/xpm.h>

/**************************************************************************/

/*
 * Some hacks to ensure compatibility between kernels before 2.0.3
 * and after 2.0.3
 *
 * This is done so the program runs with both kernel versions (hopefully)
 * without any need to recompile.
 *
 * These settings are, according to hannu, only supported by PAS16 soundcards.
 * The executable tries to detect the PAS16 at runtime and provides the
 * necessary buttons only if necessary.
 *
 * kernels before 2.0.3 use the symbols below, while later kernels use
 * SOUND_MIXER_PRIVATEx commands.
 *
 * As the symbols below are obsolete anyway, are provided only for backwards
 * compatibility and surely won't change in future anymore, I hardcode them
 * here.
 * Thus I avoid any attempt made by hannu to break software depending on
 * those old values when tried to compile under recent versions.
 * I do this on purpose, I wan't to provide backward compatibility for some
 * time until the 1.2.x & 1.3.x kernel series become ancient relicts.
 *
 * In future, support for those symbols will be removed.
 *
 */

#ifdef SOUND_MIXER_MUTE
#undef SOUND_MIXER_MUTE
#endif

#ifdef SOUND_MIXER_LOUD
#undef SOUND_MIXER_LOUD
#endif

#ifdef SOUND_MIXER_ENHANCE
#undef SOUND_MIXER_ENHANCE
#endif

#define SOUND_MIXER_MUTE        28      /* 0 or 1 */
#define SOUND_MIXER_ENHANCE     29      /* Enhanced stereo (0, 40, 60 or 80) */
#define SOUND_MIXER_LOUD        30      /* 0 or 1 */

/*
 * Symbols used internally and mapped to one of the above or to PRIVATEx
 */
#define XMIXER_MUTE	1001
#define XMIXER_ENHANCE	1002
#define XMIXER_LOUD	1003

/**************************************************************************/

#define SLIDER_DISTANCE        45
#define SLIDER_OFFSET_MONO     6
#define SLIDER_SIZE_STEREO     25
#define SLIDER_SIZE_MONO       14

/**************************************************************************
 * Private routines
 * ================
 */
static void alarm_handler(int dummy);
static void slider_proc(McGadget *gadget);
static void button_proc(McGadget *gadget);
static void record_proc(McGadget *gadget);
static void exit_proc(McGadget *gadget);
static int event_proc(McWindow *window, XEvent *event);
static int get_volume(McGadget *gadget);
static int set_volume(McGadget *gadget);
static void slider_update(void);
static int set_get_source(int nr, int onoff);
static int set_flag(McGadget *gadget);
static int get_flag(McGadget *gadget);
static McGadget *AddSwitch(char *title, McGadget *prev,
			   McBitmap *bm1, McBitmap *bm2, int request);
static McGadget *AddSlider(int x,int y,int request,int stereo);
static McGadget *AddIcon(int x,int y, int w, int h, int request);
static McGadget *AddBorder(int x,int y, int w, int h);
static McGadget *AddSelector(int x, int y, int request);
static void mainloop(void);

/**************************************************************************
 *
 * Public routines
 * ===============
 */

void do_message(char *string);

/**************************************************************************
 *
 * Public variables
 * ================
 */
char            *mixer_name="/dev/mixer";
McApp		*app;
McWindow	*mainWindow;

unsigned char   *sources[]=SOUND_DEVICE_NAMES;
int             mixer;
int		poll_rate=333333;
int		ClearMessage = 0;
int             control[SOUND_MIXER_NRDEVICES];
int		source; /* contains a bit for each supported recording source*/
int		stereo; /* Contains a bit for each stereo mixer device */
int		mixers; /* Contains a bit for each supported mixer device */
int		show_pas_buttons; /* Is it a PAS16? */
int		show_exit_button; /* Show the exit button */

#if 0
int		mixcaps; /* Mixer capabilities */
#endif

int		debug      = 0;
int		poll_mixer = 0;

McGadget *SliderGads[SOUND_MIXER_NRDEVICES];
McGadget *SelectorGads[SOUND_MIXER_NRDEVICES]; /* For recording source */
McGadget *MessageGad;
McGadget *LoudnessGad;
McGadget *WideGad;
McGadget *MuteGad;

/**************************************************************************/

static XrmOptionDescRec userTable[] = {
  { "-1",      "*showAll",		XrmoptionNoArg,  (caddr_t)"on"	  },
  { "-a",      "*showAll",		XrmoptionNoArg,  (caddr_t)"on"	  },
  { "-0",      "*mixerMode",		XrmoptionNoArg,  (caddr_t)"small" },
  { "-p",      "*pollMixer",		XrmoptionNoArg,  (caddr_t)"on"	  },
  { "-np",     "*pollMixer",		XrmoptionNoArg,  (caddr_t)"off"	  },
  { "-d",      "*debug",		XrmoptionNoArg,  (caddr_t)"on"	  },
  { "-debug",  "*debug",		XrmoptionNoArg,  (caddr_t)"on"	  },
  { "-device", "*mixerDevice",		XrmoptionSepArg, (caddr_t)NULL	  },
  { "-all",    "*showAll",		XrmoptionNoArg,  (caddr_t)"on"	  },
  { "-poll",   "*pollMixer",		XrmoptionNoArg,  (caddr_t)"on"	  },
  { "-nopoll", "*pollMixer",		XrmoptionNoArg,  (caddr_t)"off"	  },
  { "-rate",   "*pollRate",		XrmoptionSepArg, (caddr_t)NULL	  },
  { "-hide-exit","*hideExitButton",	XrmoptionNoArg,  (caddr_t)"on"	  },
};

/**************************************************************************
 * main(int ac, char **argv)
 *
 */
void main(int argc, char **argv) {
  int	i,j,y;
  int   flag;
  char	*str;
  McGadget *gad;
  XWMHints	 wm_hints;
  XSizeHints	 sizehints;
  struct sigaction alarm_sigaction= { alarm_handler, 0, 0, 0 };
  mixer_info info;

  myname = argv[0];

  if ((argc==2) && (!strcmp(argv[1], "-v"))) {
    printf("%s\n", APP_VERSION);
    cleanup(0);
  }

  /*
   * Build connection with X server
   */
  app = McCreateApp(&argc, argv, APP_VERSION, APP_CLASS,
		    userTable, sizeof(userTable)/sizeof(userTable[0]));

  if ((argc==2) && ((!strcmp(argv[1], "-h")) || (!strcmp(argv[1], "-?")))) {
    printf(_("Usage:\n"
	     "   %s [-all] [-debug] [-device <device>] "
	     "[-nopoll] [-poll] [-rate <µsec>]\n"),
	   myname);
    cleanup(0);
  }

  if (argc>1) {
    fprintf(stderr, _("%s: Too many arguments\n"), myname);
    cleanup(1);
  }

  sigaction(SIGALRM,&alarm_sigaction,0);

  /*
   * Read settings.
   */

  debug=McGetSwitch(app, "debug");

  if ((str=McGetResource(app, "mixerDevice"))) {
    mixer_name=str;
  }

  if ((str=McGetResource(app, "mixerMode")))
    fprintf(stderr,
	  _("%s: mixerMode Resource and -0, -1 switch no longer supported.\n"),
	    myname);

  if ((str=McGetResource(app, "pollMixer"))) {
    poll_mixer=McTestSwitch(str);
  }

  if ((str=McGetResource(app, "pollRate"))) {
    poll_rate=atoi(str);
    if (poll_rate<10000) {
      fprintf(stderr,_("%s: Poll rate too low, using 1/3s.\n"),myname);
      poll_rate=333333;
    }
  }

  /*
   * Find out which sliders should be visible
   */
  flag=0;
  if (!McGetSwitch(app, "showAll")) {
    for (i=0; i<SOUND_MIXER_NRDEVICES; i++) {
      if ((str=McGetResource(app, sources[i]))) {
	control[i]=McTestSwitch(str);
	flag=1;
      } else {
	control[i]=0;
      }
    }
  }

  /*
   * If no resources are found, use default
   */
  if (!flag) {
    if (debug) fprintf(stderr, _("%s: Enabling all sliders.\n"), myname);
    for (i=0; i<SOUND_MIXER_NRDEVICES; i++) control[i]=1;
  }

  /*
   * Wanna see the exit button?
   */
  show_exit_button=!McGetSwitch(app, "hideExitButton");

  /*
   * Open the mixer device
   */
  if ((mixer = open(mixer_name,O_RDWR))<0) {
    fprintf(stderr,_("%s: Can't open %s: "),argv[0],mixer_name);
    perror("");
    cleanup(1);
  }

  /*
   * Determine the available devices
   */
  if (ioctl(mixer, SOUND_MIXER_READ_DEVMASK, &mixers)<0) {
    fprintf(stderr,
	    _("%s: Warning: Can't get a list of available mixer devices.\n"),
	    myname);
    mixers=0xFFF; /* Be optimistic... */
  } else {
    if (debug) {
      fprintf(stderr, _("%s: SOUND_MIXER_READ_DEVMASK returned 0x%08X\n"),
	      myname, mixers);
    }
  }

  /*
   * Determine the available recording sources
   */
  if (ioctl(mixer, SOUND_MIXER_READ_RECMASK, &source)<0) {
    fprintf(stderr,
	  _("%s: Warning: Can't get a list of available recording sources.\n"),
	    myname);
    source=0;
  } else {
    if (debug) {
      fprintf(stderr, _("%s: SOUND_MIXER_READ_RECMASK returned 0x%08X\n"),
	      myname, source);
    }
  }

  /*
   * Find out which devices are stereo
   */
  if (ioctl(mixer, SOUND_MIXER_READ_STEREODEVS, &stereo)<0) {
    fprintf(stderr,
	    _("%s: Warning: Can't get a list of stereo mixer devices.\n"),
	    myname);
    stereo=0xFFF9; /* Be optimistic... */
  } else {
    if (debug) {
      fprintf(stderr, _("%s: SOUND_MIXER_READ_STEREODEVS returned 0x%08X\n"),
	      myname, stereo);
    }
  }

#if 0
  /*
   * Read flags (currently not used)
   */
  if (ioctl(mixer, SOUND_MIXER_READ_CAPS, &mixcaps)<0) {
    fprintf(stderr,
	    _("%s: Warning: Can't read mixer capabilities.\n"),
	    myname);
    mixcaps=0;
  } else {
    if (debug) {
      fprintf(stderr, _("%s: SOUND_MIXER_READ_CAPS returned 0x%08X\n"),
	      myname, mixcaps);
    }
  }
#endif

  /*
   * Read mixer info
   */
  if (ioctl(mixer, SOUND_MIXER_INFO, &info)<0) {
    fprintf(stderr, _("%s: Warning: Can't read mixer info.\n"), myname);
    show_pas_buttons=1; /* sane(?) default */
  } else {
    show_pas_buttons=!strcmp(info.id, "PAS16");
  }
  
  /*
   * Remove unsupported devices from list
   */
  for (i=0; i<SOUND_MIXER_NRDEVICES; i++) {
    if (control[i]) {
      if (!(mixers & (1<<i))) {
	control[i]=0;
	if (debug) {
	  fprintf(stderr,
		 _("%s: Slider `%s' requested but NOT supported by driver.\n"),
		  myname, sources[i]);
	}
      } else if (debug) {
	fprintf(stderr,
		_("%s: Slider `%s' requested and supported by driver.\n"),
		myname, sources[i]);
      }
    }
  }

  /*
   * Calculate window size
   */
  if (show_exit_button || show_pas_buttons) y=42; else y=11;
  if (!(app->w || app->h)) {
    app->w=12;
    for (i=0;i<SOUND_MIXER_NRDEVICES;i++) {
      if (control[i])
	app->w+=SLIDER_DISTANCE;
    }
    if (app->w<278) app->w=278;
    app->h=188+y;
  }

  /*
   * Create the window
   */
  mainWindow=McCreateAppWindow(app,-1,-1,-1,-1,NULL, event_proc);

  /*
   * Create all bitmaps
   */

  for (i=NUM_BITMAPS; --i>=0;) McCreateBitmapFromData(mainWindow, &pmap[i]);

  /*
   * Set WM Hints
   */
  memset(&sizehints, 0, sizeof(XSizeHints));
  memset(&wm_hints, 0, sizeof(XWMHints));
  wm_hints.icon_pixmap = pmap[PIX_ICON].pixmap;
  wm_hints.icon_mask = pmap[PIX_ICONMASK].pixmap;
  wm_hints.flags =IconPixmapHint | IconMaskHint;
  sizehints.flags = PMinSize | PMaxSize;
  sizehints.min_width = 57; sizehints.max_width = app->w;
  sizehints.min_height= 27; sizehints.max_height = app->h;
  McSetHints(mainWindow, NULL, argc, argv, &sizehints, &wm_hints);


  if (show_exit_button || show_pas_buttons) {
    gad = McCreateGadget(mainWindow, GAD_H3D | GAD_3D | GAD_ACTIVE, BOOLGADGET,
			 10, 11, 39, 20);
    gad->normalLabel = McCreateText(mainWindow, _("Exit"), app->gc[GC_NORMAL],
				    app->defaultFont, 0, 4);
    gad->normalLabel->x=1+((39-gad->normalLabel->width)>>1);
    gad->callbackUp = exit_proc;
    gad->tip = _("Guess what?");
    McAddFocusGadget(gad, 1);
    McSetFocus(gad);
  } else {
    gad=NULL;
  }

  if (show_pas_buttons) {
    LoudnessGad=AddSwitch("Loudness", gad, NULL, NULL, XMIXER_LOUD);
    LoudnessGad->tip=_("Loudness");
    McAddFocusGadget(LoudnessGad, 1);
    WideGad=AddSwitch(NULL, LoudnessGad, &pmap[PIX_NOWIDE], &pmap[PIX_WIDE],
		      XMIXER_ENHANCE);
    WideGad->tip=_("Stereo-wide");
    McAddFocusGadget(WideGad, 1);
    MuteGad=AddSwitch(NULL, WideGad, &pmap[PIX_NOMUTE], &pmap[PIX_MUTE],
		      XMIXER_MUTE);
    MuteGad->tip=_("Mute");
    McAddFocusGadget(MuteGad, 1);
  } else {
    LoudnessGad=NULL;
    WideGad=NULL;
    MuteGad=NULL;
  }

  MessageGad = McCreateGadget(mainWindow, GAD_HNONE, BOOLGADGET,
			      10, app->h - 20, app->w-20, 16);
  MessageGad->normalLabel = McCreateText(mainWindow, "", app->gc[GC_NORMAL],
					 app->defaultFont, 4, 3);


  /* Sets up a control for each requested input
   */
  for (i=0,j=11;i<SOUND_MIXER_NRDEVICES;i++) {
    SliderGads[i]=NULL;
    SelectorGads[i]=NULL;
    if (control[i]) {
      AddBorder(j, y, 37, 166);
      AddIcon(j+4, y, 32, 32, i);
      SliderGads[i]=AddSlider(j + 7, y+35, i, stereo & (1<<i));
      McAddFocusGadget(SliderGads[i], 1);
      if (source&(1<<i)) {
	SelectorGads[i]=AddSelector(j + 7, 143+y, i);
      }
      j+=SLIDER_DISTANCE;
    }
  }

  for (i=0;i<SOUND_MIXER_NRDEVICES;i++)
    if (SelectorGads[i]) McAddFocusGadget(SelectorGads[i], 1);

  /* Current mixer settings */
  slider_update();

  /* Current button settings */
  if (show_pas_buttons) {
    get_flag(LoudnessGad);
    get_flag(WideGad);
    get_flag(MuteGad);
  }

  /* Read the current source */
  set_get_source(-1, 0);

  /* Hi folx! */
  do_message(info.name);

  /*
   * Display the window
   */
  McInitGadgets(mainWindow);
  McMapWindow(mainWindow);

  /*
   * Now do the main loop
   */

  mainloop();
}

/**************************************************************************
 * PRIVATE void slider_proc(Slider *slider)
 */

#define SLIDE ((McSlider *)(gadget->specialInfo))
static void slider_proc(McGadget *gadget) {
  set_volume(gadget);
  get_volume(gadget);
  McGadgetUpdate(gadget);
}

/**************************************************************************
 * static void exit_proc(McGadget *gadget)
 *
 */
static void exit_proc(McGadget *gadget) {
  cleanup(0);
}

void cleanup(int r) {
  if (mixer>=0)
    close(mixer);
  exit(r);
}

/**************************************************************************
 * static void button_proc(Button *button)
 *
 */
static void button_proc(McGadget *gadget) {
  set_flag(gadget);
  get_flag(gadget);
  McGadgetRedraw(gadget);
}

/**************************************************************************
 * static void button_proc(Button *button)
 *
 */
static void record_proc(McGadget *gadget) {
  set_get_source(gadget->id, gadget->flags & GAD_SELECTED);
}

static void slider_update(void) {
  int i,f;
  f=0;
  for (i=0; i<SOUND_MIXER_NRDEVICES; i++) {
    if (SliderGads[i]) {
      if (get_volume(SliderGads[i])>0) {
	McGadgetRedraw(SliderGads[i]);
	f=1;
      }      
    }
  }

  if (show_pas_buttons) {
    if (get_flag(LoudnessGad)>0) {
      McGadgetRedraw(LoudnessGad);
      f=1;
    }

    if (get_flag(WideGad)>0) {
      McGadgetRedraw(WideGad);
      f=1;
    }

    if (get_flag(MuteGad)>0) {
      McGadgetRedraw(MuteGad);
      f=1;
    }
  }

}

/***************************************************************************/

static void mainloop(void) {
  struct timeval to;

  while(1) {
    to.tv_sec  = 0;
    to.tv_usec =  poll_rate;
    if ((mainWindow->window_visible) && poll_mixer) {
      McAppSelect(app, 0, NULL, NULL, NULL, &to);
    } else {
      McAppSelect(app, 0, NULL, NULL, NULL, NULL);
    }

    slider_update();
    if (ClearMessage) {
      ClearMessage=0;
      do_message(0);
    }
  }

}

/*************************************************************************
 * STATUS STATUS STATUS STATUS STATUS STATUS STATUS STATUS STATUS STATUS */

void do_message(char *string) {
  if (string) alarm(3); else string="";
  McChangeText(mainWindow, MessageGad->normalLabel, string,
	       MessageGad->normalLabel->gc);
  McGadgetRedraw(MessageGad);
}

/* We must not call Xlib from within a signal handler */
static void alarm_handler(int dummy) {
  ClearMessage = 1;
}


/**************************************************************************/

static McGadget *AddSlider(int x,int y,int request,int str) {
  McGadget *gad;
  gad = McCreateGadget(mainWindow,
		       GAD_3D | GAD_H3D | GAD_ACTIVE | GAD_SELECTED,
		       SLIDERGADGET, x+(str?0:SLIDER_OFFSET_MONO),
		       y, str?SLIDER_SIZE_STEREO:SLIDER_SIZE_MONO,
		       100);
  gad->specialInfo = McCreateSlider((str?SLIDER_STEREO:SLIDER_MONO)|
				    SLIDER_VERTICAL, 100);

  ((McSlider *)(gad->specialInfo))->step = 3;
  gad->callbackDown = slider_proc;
  gad->id = request;
  return gad;
}

static McGadget *AddIcon(int x,int y, int w, int h, int request) {
  McGadget *gad;
  McBitmap *bm;

  gad = McCreateGadget(mainWindow, GAD_HNONE, BOOLGADGET,
		       x , y, w, h);

  if (request<NUM_ICONS) {
    bm = &pmap[PIX_VOL + request];
  } else {
    bm = &pmap[PIX_UNKNOWN];
  }

  gad->normalBitmap=bm;
  gad->tip=sources[request];
  return gad;
}

static McGadget *AddBorder(int x,int y, int w, int h) {
  return McCreateGadget(mainWindow, GAD_3D | GAD_NOFILL | GAD_BORDERONLY,
			BOOLGADGET, x , y, w, h);
}



static McGadget *AddSelector(int x,int y, int request) {
  McGadget *gad;

  gad = McCreateGadget(mainWindow,GAD_3D | GAD_H3D | GAD_ACTIVE | GAD_TOGGLE,
		       BOOLGADGET, x , y, 25, 16);
  gad->id = request;
  gad->normalBitmap = &pmap[PIX_MIX_PLAY];
  gad->selectBitmap = &pmap[PIX_MIX_REC];
  gad->callbackDown = record_proc;
  return gad;
}

static McGadget *AddSwitch(char *title, McGadget *prev,
			   McBitmap *bm1, McBitmap *bm2, int request) {
  McGadget *gad;
  gad = McCreateGadget(mainWindow, GAD_3D | GAD_H3D | GAD_ACTIVE | GAD_TOGGLE,
		       BOOLGADGET, prev->x+prev->width+10, prev->y, 32, 20);
  gad->callbackDown = button_proc;
  if (title) {
    gad->normalLabel=McCreateText(mainWindow,title,app->gc[GC_NORMAL],
				  app->defaultFont, 8, 4);
    McResizeGadget(gad, gad->normalLabel->width + 16, -1);
  }
  if (bm1) {
    gad->normalBitmap=bm1;
    McResizeGadget(gad, bm1->width + (bm1->x<<1), -1);
  }
  if (bm2) {
    gad->selectBitmap=bm2;
    McResizeGadget(gad, bm2->width + (bm2->x<<1), -1);
  }
  gad->id = request;
  return gad;
}

/**************************************************************************
 * Device access
 *
 */

static int set_volume(McGadget *gadget) {
  unsigned int n;
  n = ((McSlider *)(gadget->specialInfo))->leftValue +
     (((McSlider *)(gadget->specialInfo))->rightValue<<8);
  if ((mixer<0) || ioctl(mixer,MIXER_WRITE(gadget->id),&n)<0) {
    do_message(_("Can't set volume."));
    return -1;
  }
  return 0;
}


static int get_volume(McGadget *gadget) {
  unsigned int n;

  if ((mixer<0) || ioctl(mixer,MIXER_READ(gadget->id),&n)<0) {
    do_message(_("Can't read volume."));
    return -1;
  }
  if ((((McSlider *)(gadget->specialInfo))->leftValue) == (n & 0x7F))
    if ((((McSlider *)(gadget->specialInfo))->rightValue) == ((n>>8) & 0x7F)) {
      return 0;
    }
  ((McSlider *)(gadget->specialInfo))->leftValue = n & 0x7F;
  ((McSlider *)(gadget->specialInfo))->rightValue = (n>>8) & 0x7F;
  return 1;
}

static void decode_ioctl(int id, int *old, int *new) {

  *old=0;
  *new=0;

  switch(id) {
  case XMIXER_MUTE:
#ifdef SOUND_MIXER_PRIVATE3
    *new=SOUND_MIXER_PRIVATE3;
#endif
    *old=SOUND_MIXER_MUTE;
    break;

  case XMIXER_LOUD:
#ifdef SOUND_MIXER_PRIVATE1
    *new=SOUND_MIXER_PRIVATE1;
#endif
    *old=SOUND_MIXER_LOUD;
    break;

  case XMIXER_ENHANCE:
#ifdef SOUND_MIXER_PRIVATE2
    *new=SOUND_MIXER_PRIVATE2;
#endif
    *old=SOUND_MIXER_ENHANCE;
    break;
  }
}

static int set_flag(McGadget *gadget) {
  int n, old, new;

  if (mixer<0) return -1; /* Huh? */

  decode_ioctl(gadget->id, &old, &new);

  if (gadget->flags&GAD_SELECTED) n=1; else n=0;
  if ((gadget->id==SOUND_MIXER_ENHANCE) && n) n=80;

  if (new && (ioctl(mixer, new, &n)>=0)) return 0;
  if (old && (ioctl(mixer, MIXER_WRITE(gadget->id), &n)>=0)) return 0;

  do_message(_("Can't write flag."));
  return -1;
}

static int get_flag(McGadget *gadget) {
  int n, old, new;

  if (mixer<0) return -1; /* Huh? */

  decode_ioctl(gadget->id, &old, &new);

  n=-1;
  if (new && (ioctl(mixer, new, &n)>=0)) goto good;
  if (old && (ioctl(mixer, MIXER_READ(old), &n)>=0)) goto good;

  do_message(_("Can't read flag."));
  return -1;

good:
  if (n) {
    if (gadget->flags&GAD_SELECTED) return 0;
    gadget->flags|=GAD_SELECTED;
  } else {
    if (!(gadget->flags&GAD_SELECTED)) return 0;
    gadget->flags&=~GAD_SELECTED;
  }
  return 1;
}

static int set_get_source(int nr, int onoff) {
  int n,i;

  if (mixer<0) goto bad;

  if (nr>=0) {
    if (ioctl(mixer, SOUND_MIXER_READ_RECSRC, &n)<0) goto bad;
    if (onoff) n |= 1<<nr; else n &= ~(1<<nr);
    if (ioctl(mixer, SOUND_MIXER_WRITE_RECSRC, &n)<0) goto bad;
  }

  if (ioctl(mixer, SOUND_MIXER_READ_RECSRC, &n)<0) goto bad;

  for (i=0;i<SOUND_MIXER_NRDEVICES;i++) {
    if (n & 1) {
      if (SelectorGads[i] && (!(SelectorGads[i]->flags & GAD_SELECTED))) {
	SelectorGads[i]->flags |= GAD_SELECTED;
	McGadgetRedraw(SelectorGads[i]);
      }
      if (SelectorGads[i]) SelectorGads[i]->tip=_("Recording source");
    } else {
      if (SelectorGads[i] && (SelectorGads[i]->flags & GAD_SELECTED)) {
	SelectorGads[i]->flags &= ~GAD_SELECTED;
	McGadgetRedraw(SelectorGads[i]);
      }
      if (SelectorGads[i]) SelectorGads[i]->tip=_("Not recorded");
    }
    n=n>>1;
  }
  return 0;

bad:
  do_message(_("Can't access recording source."));
  return -1;
}

/**************************************************************************/

static int event_proc(McWindow *mcw, XEvent *event) {
  switch(event->type) {
  case ClientMessage:
    if ((event->xclient.format == 32) &&
	(event->xclient.data.l[0] == app->wmDelWin)) {
      if (mcw==mainWindow) cleanup(0);
      return 1;
    }
  }
  return 0;
}

