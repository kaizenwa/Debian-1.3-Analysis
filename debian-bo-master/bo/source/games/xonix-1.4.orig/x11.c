/*
 * xonix game
 *
 * X11 interface functions
 *
 * x11.c,v 1.53 1995/09/14 11:55:55 j Exp
 */

/*
 * Copyright (c) 1995
 *   Joerg Wunsch <joerg_wunsch@uriah.heep.sax.de>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE DEVELOPERS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE DEVELOPERS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifdef USE_X11

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <X11/Shell.h>

#include <X11/xpm.h>

#define POINTS_DIGITS 6		/* number of digits in score display */

#if !defined(__unix) && (defined(__unix__) || defined(unix))
# define __unix			/* not all compilers do this */
#endif

#ifdef __unix

#define MAXSCORES 10		/* number of entries in high score table */
#define PATH_HIGHSCORE XONIXDIR "/scores"
#define PATH_TEMPSCORE XONIXDIR "/score_tmp"

#if defined(__unix) && !defined PATH_RMAIL
#define PATH_RMAIL "rmail"	/* rely on the $PATH */
#endif

#include <sys/types.h>
#include <sys/time.h>
#include <pwd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#endif

#include "xonix.xbm"
#include "xonix_mask.xbm"

#include "d0.xbm"
#include "d1.xbm"
#include "d2.xbm"
#include "d3.xbm"
#include "d4.xbm"
#include "d5.xbm"
#include "d6.xbm"
#include "d7.xbm"
#include "d8.xbm"
#include "d9.xbm"
#include "colon.xbm"

#include "flyer.xpm"
#include "runner.xpm"
#include "eater.xpm"
#include "empty.xpm"
#include "way.xpm"
#include "filled.xpm"

#include <X11/Core.h>
#if USE_XAW_3D
#include <X11/Xaw3d/Form.h>
#include <X11/Xaw3d/Label.h>
#include <X11/Xaw3d/Box.h>
#include <X11/Xaw3d/Command.h>
#include <X11/Xaw3d/MenuButton.h>
#include <X11/Xaw3d/SimpleMenu.h>
#include <X11/Xaw3d/SmeLine.h>
#include <X11/Xaw3d/SmeBSB.h>
#else /* standard Xaw */
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/SmeBSB.h>
#endif /* USE_XAW_3D */

#include "patchlevel.h"

#include "xonix.h"

#define ApplicationClassName "Xonix"

#define trans_table \
"<Message>WM_PROTOCOLS:	GlobalProtoHandler()\n" \
"<Key>Left:		Left()\n" \
"<Key>Right:		Right()\n" \
"<Key>Up:		Up()\n" \
"<Key>Down:		Down()\n" \
"<Key>Q:		Quit()\n" \
"<Key>Escape:		Quit()\n" \
"<Unmap>:		Pause()\n" \
"<Key>P:		Pause()\n" \
"<Map>:			Continue()\n"

#define about_msg \
"                  The game of xonix\n" \
"\n" \
"                 Copyright \251 1995 by\n" \
"Torsten Sch\366nitz, J\366rg Wunsch, Alfredo Herrera Hern\341ndez\n" \
"\n" \
"                     version info:\n" \
VERSION


typedef struct AppResources {
  int time_step;
} AppResources;

typedef struct CallbackData {
  Widget w;
  int button_number;
} CallbackData;

typedef struct Dims {
  Dimension width, height;
} Dims;


#define Offset(field) (XtOffsetOf(AppResources, field))

static XtResource xonix_resources[] = {
  {"timeStep", "TimeStep", XtRInt, sizeof(int),
     Offset(time_step), XtRImmediate, (XtPointer) STEP_TIME},
};

static Window win;
static Visual *vis;
static int screenno;
static int depth;
static Screen *screen;
static Colormap cmap;
static int winx, winy;
static AppResources resources;
static int exiting = 0;

/* exported */
Display      *dpy;
Window       canv;
Dimension    sizex, sizey;

static GC default_gc, empty_gc;
static Pixmap runner_pm, flyer_pm, eater_pm, filled_pm, empty_pm, way_pm;
static Dims empty_dims, filled_dims, way_dims;
static const EventMask evmask = ExposureMask;

static Atom delwin;		/* the delete window atom */
static Widget toplevel;		/* the top level widget */
static Widget container;	/* form widget containing everything */
static Widget drawarea;		/* our Canvas */
static Widget statusarea;	/* form for status display */
/* percentage display */
static Widget st_percentage, st_perc_label, st_perc_d10, st_perc_d1;
/* level display */
static Widget st_level, st_lev_label, st_lev_d10, st_lev_d1;
/* runner display */
static Widget st_runner, st_run_label, st_run_d10, st_run_d1;
/* time display */
static Widget st_score, st_score_label;
static Widget st_points[POINTS_DIGITS];
/* game menu */
static Widget game_button, game_menu, game_about, game_l1, game_quit;
/* game over shell */
static Widget gameover_shell = (Widget)0;
static int gameover_pending = 0;
/* score widget */
static Widget score_shell = (Widget)0;

static void timeout(XtPointer client_data, XtIntervalId *timer);
static void DoAbout(void);
static void QuitCallback(Widget, XtPointer, XtPointer);
static void AboutCallback(Widget, XtPointer, XtPointer);

static XtAppContext app;
static XtIntervalId gTimer = (XtIntervalId) 0;

/* action handling */
static void GlobalProtoHandler(Widget, XEvent *, String *, Cardinal *);
static void Left(Widget, XEvent *, String *, Cardinal *);
static void Right(Widget, XEvent *, String *, Cardinal *);
static void Up(Widget, XEvent *, String *, Cardinal *);
static void Down(Widget, XEvent *, String *, Cardinal *);
static void Quit(Widget, XEvent *, String *, Cardinal *);
static void Pause(Widget, XEvent *, String *, Cardinal *);
static void Continue(Widget, XEvent *, String *, Cardinal *);

static void DrawWayElementToGWorld (int xPos, int yPos);

static XtActionsRec actions[] = {
  {"GlobalProtoHandler", GlobalProtoHandler},
  {"Left", Left},
  {"Right", Right},
  {"Up", Up},
  {"Down", Down},
  {"Quit", Quit},
  {"Pause", Pause},
  {"Continue", Continue}
};

/* our 7-segment display */
static unsigned char *digit_bits[] = {
  d0_bits, d1_bits, d2_bits, d3_bits, d4_bits,
  d5_bits, d6_bits, d7_bits, d8_bits, d9_bits
};
static Pixmap digit_pms[10];

static void
GlobalProtoHandler(Widget w, XEvent *xev, String *p, Cardinal *n)
{
  if(xev->xclient.data.l[0] == delwin) {
    if(w == toplevel) ExitXonix(0); else XtPopdown(w);
  }
}


static void
Left(Widget w, XEvent *xev, String *p, Cardinal *n)
{
  gMyRunner.dx = - RATIO;
  gMyRunner.dy = 0;
}


static void
Right(Widget w, XEvent *xev, String *p, Cardinal *n)
{
  gMyRunner.dx = RATIO;
  gMyRunner.dy = 0;
}


static void
Up(Widget w, XEvent *xev, String *p, Cardinal *n)
{
  gMyRunner.dy = - RATIO;
  gMyRunner.dx = 0;
}


static void
Down(Widget w, XEvent *xev, String *p, Cardinal *n)
{
  gMyRunner.dy = RATIO;
  gMyRunner.dx = 0;
}


static void
Quit(Widget w, XEvent *xev, String *p, Cardinal *n)
{
  exiting++;			/* do not ask back */
  ExitXonix(0);
}


static void
Pause(Widget w, XEvent *xev, String *p, Cardinal *n)
{
  if(xev->type == UnmapNotify) {
    gPause = True;
    if(gRun) {
      XtRemoveTimeOut(gTimer);
      gTimer = (XtIntervalId) 0;
    }
  } else {
    (void)XIconifyWindow (dpy, XtWindow(toplevel), screenno);
    /* will come here soon again (UnmapNotify) */
  }
}


static void
Continue(Widget w, XEvent *xev, String *p, Cardinal *n)
{
  if(gPause) {
    gPause = False;
    if(gTimer == 0)
      gTimer = XtAppAddTimeOut(app, resources.time_step, timeout, 0);
  }
}


static void 
XonixEventHandler(Widget w,      /* canvas */
		  XtPointer cd,
		  XEvent *event, 
		  Boolean *cont)
{
  XExposeEvent *exp = (XExposeEvent *)event;
  int x0, y0, x1, y1, i, j;
  int exposed = 0;
  
  /* first, redraw the requested large areas */
  do {
    switch(event->type) {
    case Expose:
      exposed++;
      x0 = (((exp->x + EATER_SIZE - 1) / EATER_SIZE) - 1) & ~1;
      y0 = (((exp->y + EATER_SIZE - 1) / EATER_SIZE) - 1) & ~1;
      if(x0 < 0) x0 = 0;
      if(y0 < 0) y0 = 0;
      x1 = ((exp->x + exp->width + EATER_SIZE - 1) / EATER_SIZE) | 1;
      y1 = ((exp->y + exp->height + EATER_SIZE - 1) / EATER_SIZE) | 1;
      for(j = y0; j < y1; j += 2)
	for(i = x0; i < x1; i += 2) {
	  unsigned char c = *(gMyStatusArea + (j * H_STEPS) + i);
	  if((c & FILLED) || (c & BORDER) || (c & EATER))
	    DrawFilledToGWorld(i, j);
	  else if(c & FLYER) DrawFlyerToGWorld(i, j);
	  else if(c & WAY) DrawWayElementToGWorld(i, j);
	  else if(c & RUNNER) DrawRunnerToGWorld();
	  else DrawEmptyToGWorld(i, j);
	}
      break;
      
    default: /* nothing else notify */
      break;
    }
  } while(True == XCheckWindowEvent(dpy, canv, evmask, event));

  /*
   * finally, if we have been exposed, redraw all flyers; this
   * is way easier than to care for their sub-micros in the loop
   * above
   */
  if(exposed)
    for(i = 0; i < gEaterCount; i ++)
      DrawEaterToGWorld (gEater[i].x, gEater[i].y);

  *cont = True; /* the event handler will not be removed */
}


void
x11_init(int argc, char **argv)
{
  XGCValues v;
  XColor c, c1;
  Cursor cursor;
  Pixmap cursor_pm, cursor_mask_pm, colon;
  XColor cursor_fg, cursor_bg;
  Arg wargs[4];
  XtTranslations ttab;
  int i;
  Dimension width, height;
  XpmAttributes xpm_attr;
  
  toplevel = 
    XtVaAppInitialize(&app,			/* application context */
		      ApplicationClassName,	/* class */
		      NULL, 0,			/* options */
		      &argc, argv,
		      NULL,			/* fallback resources */
		      NULL);

  XtAppAddActions(app, actions, XtNumber(actions));
  XtGetApplicationResources(toplevel, (XtPointer) &resources, 
			    xonix_resources, XtNumber(xonix_resources),
			    NULL, (Cardinal) 0);

  container = XtVaCreateManagedWidget("container", formWidgetClass,
				      toplevel,
				      NULL);
  
  game_menu= XtVaCreatePopupShell("game_menu", simpleMenuWidgetClass, 
				  container,
				  NULL);
  game_button = XtVaCreateManagedWidget("game_button", menuButtonWidgetClass, 
					container,
					XtNmenuName, "game_menu",
					NULL);
  game_about = XtVaCreateManagedWidget("about", smeBSBObjectClass,
				       game_menu,
				       NULL);
  game_l1 = XtVaCreateManagedWidget("game_l1", smeLineObjectClass,
				    game_menu,
				    NULL);
  game_quit = XtVaCreateManagedWidget("quit", smeBSBObjectClass,
				      game_menu,
				      NULL);
  XtAddCallback(game_quit, XtNcallback, QuitCallback, NULL);
  XtAddCallback(game_about, XtNcallback, AboutCallback, NULL);

  drawarea = XtVaCreateManagedWidget("canvas", coreWidgetClass,
				     container,
				     XtNfromVert, (XtPointer)game_button,
				     XtNwidth, (sizex = FIELD_WIDTH),
				     XtNheight, (sizey = FIELD_HEIGHT),
				     NULL);

  statusarea = XtVaCreateManagedWidget("status", formWidgetClass,
				       container,
				       XtNfromVert, (XtPointer)drawarea,
				       NULL);

  st_level = XtVaCreateManagedWidget("level", formWidgetClass,
				     statusarea,
				     NULL);
  st_lev_label = XtVaCreateManagedWidget("lev_label", labelWidgetClass,
					 st_level,
					 NULL);
  st_lev_d10 = XtVaCreateManagedWidget("lev_d10", labelWidgetClass,
				       st_level,
				       XtNfromHoriz, (XtPointer)st_lev_label,
				       XtNwidth, (XtPointer)d0_width,
				       XtNheight, (XtPointer)d0_height,
				       NULL);
  st_lev_d1 = XtVaCreateManagedWidget("lev_d1", labelWidgetClass,
				      st_level,
				      XtNfromHoriz, (XtPointer)st_lev_d10,
				      XtNwidth, (XtPointer)d0_width,
				      XtNheight, (XtPointer)d0_height,
				      NULL);

  st_percentage = XtVaCreateManagedWidget("percentage", formWidgetClass,
					  statusarea,
					  XtNfromHoriz, (XtPointer)st_level,
					  NULL);
  st_perc_label = XtVaCreateManagedWidget("perc_label", labelWidgetClass,
					  st_percentage,
					  NULL);
  st_perc_d10 = XtVaCreateManagedWidget("perc_d10", labelWidgetClass,
					st_percentage,
					XtNfromHoriz, (XtPointer)st_perc_label,
					XtNwidth, (XtPointer)d0_width,
					XtNheight, (XtPointer)d0_height,
					NULL);
  st_perc_d1 = XtVaCreateManagedWidget("perc_d1", labelWidgetClass,
				       st_percentage,
				       XtNfromHoriz, (XtPointer)st_perc_d10,
				       XtNwidth, (XtPointer)d0_width,
				       XtNheight, (XtPointer)d0_height,
				       NULL);

  st_runner = XtVaCreateManagedWidget("runner", formWidgetClass,
				      statusarea,
				      XtNfromHoriz, (XtPointer)st_percentage,
				      NULL);
  st_run_label = XtVaCreateManagedWidget("run_label", labelWidgetClass,
					 st_runner,
					 NULL);
  st_run_d10 = XtVaCreateManagedWidget("run_d10", labelWidgetClass,
				       st_runner,
				       XtNfromHoriz, (XtPointer)st_run_label,
				       XtNwidth, (XtPointer)d0_width,
				       XtNheight, (XtPointer)d0_height,
				       NULL);
  st_run_d1 = XtVaCreateManagedWidget("run_d1", labelWidgetClass,
				      st_runner,
				      XtNfromHoriz, (XtPointer)st_run_d10,
				      XtNwidth, (XtPointer)d0_width,
				      XtNheight, (XtPointer)d0_height,
				      NULL);
  st_score = XtVaCreateManagedWidget("time", formWidgetClass,
				    statusarea,
				    XtNfromHoriz, (XtPointer)st_runner,
				    NULL);
  st_score_label = XtVaCreateManagedWidget("time_label", labelWidgetClass,
					  st_score,
					  NULL);
  for(i = POINTS_DIGITS - 1; i >= 0; i--) {
    char WName[20];
      sprintf(WName, "st_points%d", i + 1);
      st_points[i] =
	XtVaCreateManagedWidget(WName, labelWidgetClass,
				st_score,
				XtNfromHoriz,
				(XtPointer)(i == POINTS_DIGITS-1?
					    st_score_label:
					    st_points[i + 1]),
				XtNwidth, (XtPointer)d0_width,
				XtNheight, (XtPointer)d0_height,
				NULL);
    }
  
  XtRealizeWidget(toplevel);
  dpy = XtDisplay(toplevel);
  screenno = DefaultScreen(dpy);
  screen = DefaultScreenOfDisplay(dpy);
  vis = DefaultVisual(dpy, screenno);
  depth = DefaultDepth(dpy, screenno);
  cmap = DefaultColormap(dpy, screenno);
  
  if(depth < 4)
    XtAppError(app, "Depth too small, need at least 4");
  win = XtWindow(toplevel);
  delwin = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
  XSetWMProtocols(dpy, win, &delwin, 1);

  i = 0;
  XtSetArg(wargs[i], XtNwidth, (XtPointer)&width); i++;
  XtSetArg(wargs[i], XtNheight, (XtPointer)&height); i++;
  XtGetValues(toplevel, wargs, i);

  i = 0;
  XtSetArg(wargs[i], XtNminWidth, (XtPointer)(unsigned)width); i++;
  XtSetArg(wargs[i], XtNmaxWidth, (XtPointer)(unsigned)width); i++;
  XtSetArg(wargs[i], XtNminHeight, (XtPointer)(unsigned)height); i++;
  XtSetArg(wargs[i], XtNmaxHeight, (XtPointer)(unsigned)height); i++;
  XtSetValues(toplevel, wargs, i);
  
  ttab = XtParseTranslationTable(trans_table);
  XtOverrideTranslations(toplevel, ttab);
  XtOverrideTranslations(container, ttab);

  canv = XtWindow(drawarea);
  XtAddEventHandler(drawarea, evmask, True, 
		    XonixEventHandler,
		    0 /* client data or data transfer */);

  XAllocNamedColor (dpy, cmap, "cadet blue", &c1, &cursor_fg);
  XAllocNamedColor (dpy, cmap, "white", &c1, &cursor_bg);
  cursor_pm = XCreateBitmapFromData(dpy, canv, xonix_bits,
				    xonix_width, xonix_height);
  cursor_mask_pm = XCreateBitmapFromData(dpy, canv, xonix_mask_bits,
				    xonix_mask_width, xonix_mask_height);
  
  cursor = XCreatePixmapCursor(dpy, cursor_pm, cursor_mask_pm,
			       &cursor_fg, &cursor_bg,
			       xonix_mask_width/2, xonix_mask_height/2);
  XDefineCursor(dpy, canv, cursor);
  XtSetArg(wargs[0], XtNiconPixmap, (XtPointer)cursor_pm);
  XtSetValues(toplevel, wargs, 1);

  for(i = 0; i < 10; i++) {
    digit_pms[i] = XCreateBitmapFromData(dpy, XtWindow(statusarea),
					 digit_bits[i],
					 /* assume all digits are same size */
					 d0_width, d0_height);
  }
  colon = XCreateBitmapFromData(dpy, XtWindow(statusarea), colon_bits,
				colon_width, colon_height);
  XtSetArg(wargs[0], XtNbitmap, (XtPointer)colon);
  
  default_gc = DefaultGC(dpy, screenno);

#define InitPM(item) \
    (void)XpmCreatePixmapFromData(dpy, canv, item##_xpm, \
				  &item##_pm, NULL, &xpm_attr);
  
  xpm_attr.valuemask = XpmSize;
  
  InitPM(filled);
  filled_dims.width = xpm_attr.width; filled_dims.height = xpm_attr.height;
  InitPM(empty);
  empty_dims.width = xpm_attr.width; empty_dims.height = xpm_attr.height;
  InitPM(way);
  way_dims.width = xpm_attr.width; way_dims.height = xpm_attr.height;
  InitPM(runner);
  InitPM(flyer);
  InitPM(eater);
  
  v.function = GXcopy;
  v.fill_style = FillTiled;
  v.tile = empty_pm;
  empty_gc = XCreateGC(dpy, canv, GCTile|GCFillStyle, &v);

  gMyStatusArea = (char *)malloc(H_STEPS * V_STEPS);
  if(gMyStatusArea == 0)
    XtAppError(app, "No space for status area");

  Do_New();

  if(gTimer == 0)
    gTimer = XtAppAddTimeOut(app, resources.time_step, timeout, 0);
}


void
x11_eventloop(void)
{
  XtAppMainLoop(app);
}

void
DrawRunnerToGWorld (void)
{
  XCopyArea(dpy, runner_pm, canv, default_gc,
	    0, 0,
	    gMyRunner.sx, gMyRunner.sy,
	    gMyRunner.x * EATER_SIZE, gMyRunner.y * EATER_SIZE);
}

void
DrawWayToGWorld (void)
{
  XCopyArea(dpy, way_pm, canv, default_gc,
	    (gMyRunner.x * EATER_SIZE) % way_dims.width,
	    (gMyRunner.y * EATER_SIZE) % way_dims.height,
	    gMyRunner.sx, gMyRunner.sy,
	    gMyRunner.x * EATER_SIZE, gMyRunner.y * EATER_SIZE);
}

static void
DrawWayElementToGWorld (int xPos, int yPos)
{
  XCopyArea(dpy, way_pm, canv, default_gc,
	    (xPos * EATER_SIZE) % way_dims.width,
	    (yPos * EATER_SIZE) % way_dims.height,
	    RUNNER_SIZE, RUNNER_SIZE,
	    xPos * EATER_SIZE, yPos * EATER_SIZE);
}

void
DrawEmptyToGWorld (int xPos, int yPos)
{
  XCopyArea(dpy, empty_pm, canv, default_gc,
	    (xPos * EATER_SIZE) % empty_dims.width,
	    (yPos * EATER_SIZE) % empty_dims.height,
	    RUNNER_SIZE, RUNNER_SIZE,
	    xPos * EATER_SIZE, yPos * EATER_SIZE);
}

void
DrawFlyerToGWorld (int xPos, int yPos)
{
  XCopyArea(dpy, flyer_pm, canv, default_gc,
	    0, 0,
	    RUNNER_SIZE, RUNNER_SIZE,
	    xPos * EATER_SIZE, yPos * EATER_SIZE);
}

void
DrawEaterToGWorld (int xPos, int yPos)
{
  XCopyArea(dpy, eater_pm, canv, default_gc,
	    0, 0,
	    EATER_SIZE, EATER_SIZE,
	    xPos * EATER_SIZE, yPos * EATER_SIZE);
}

void
DrawFilledToGWorld (int xPos, int yPos)
{
  XCopyArea(dpy, filled_pm, canv, default_gc,
	    (xPos * EATER_SIZE) % filled_dims.width,
	    (yPos * EATER_SIZE) % filled_dims.height,
	    RUNNER_SIZE, RUNNER_SIZE,
	    xPos * EATER_SIZE, yPos * EATER_SIZE);
}

void
DrawSmallFilledToGWorld (int xPos, int yPos)
{
  XCopyArea(dpy, filled_pm, canv, default_gc,
	    (xPos * EATER_SIZE) % way_dims.width,
	    (yPos * EATER_SIZE) % way_dims.height,
	    EATER_SIZE, EATER_SIZE,
	    xPos * EATER_SIZE, yPos * EATER_SIZE);
}

void
DrawCompleteBorder (void)
{
  int i;
	
  for (i = 0; i < H_STEPS; i += RATIO)
    {
      DrawFilledToGWorld (i, 0);
      DrawFilledToGWorld (i, V_STEPS - RATIO);
    }
	
  for (i = RATIO; i < V_STEPS - RATIO; i += RATIO)
    {
      DrawFilledToGWorld (0, i);
      DrawFilledToGWorld (H_STEPS - RATIO, i);
    }
}


void
DrawCompleteInside (void)
{  
  XFillRectangle(dpy, canv, empty_gc,
		 RATIO * EATER_SIZE, RATIO * EATER_SIZE,
		 (H_STEPS - 2 * RATIO) * EATER_SIZE, 
		 (V_STEPS - 2 * RATIO) * EATER_SIZE);
}  


static void
timeout(XtPointer client_data, XtIntervalId *timer)
{
  if(gQuit)
    ExitXonix(0);

  if(gRun && !gPause) {
    Animate();
    XSync(dpy, False);
    gTimer = XtAppAddTimeOut(app, resources.time_step, timeout, 0);
  }
}


static void
DrawDigit(Widget w, int num)
{
  Arg wargs[1];

  if(num < 0 || num > 9)
    XtAppError(app, "Unreasonable value in DrawDigit()");
  XtSetArg(wargs[0], XtNbitmap, (XtPointer)digit_pms[num]);
  XtSetValues(w, wargs, 1);
}


void
ScorePercentage(int num)
{
  if(num > 99) num = 99;	/* sanity */
  DrawDigit(st_perc_d10, num / 10);
  DrawDigit(st_perc_d1, num % 10);
}


void
ScoreLevel(int num)
{
  if(num > 99) num = 99;	/* sanity */
  DrawDigit(st_lev_d10, num / 10);
  DrawDigit(st_lev_d1, num % 10);
}


void
ScoreRunner(int num)
{
  if(num > 99) num = 99;	/* sanity */
  DrawDigit(st_run_d10, num / 10);
  DrawDigit(st_run_d1, num % 10);
}


static void
PopdownGameOver(Widget w, XtPointer client_data, XtPointer call_data)
{
  CallbackData *cb = (CallbackData *)client_data;
  XtPopdown(cb->w);
  
  switch(cb->button_number) {
  case 0:			/* quit "i know" */
    gQuit = True;
    break;
    
  case 1:			/* goon "once more" */
    exiting = 0;
    gQuit = False;
    gRun = False;
    gPause = False;
    Do_New();
    break;
  }
  
  /* make sure, the quit will be notified */
  XtRemoveTimeOut(gTimer);
  gTimer = XtAppAddTimeOut(app, resources.time_step, timeout, 0);
}


static void
PopdownGeneric(Widget w, XtPointer client_data, XtPointer call_data)
{
  XtPopdown((Widget)client_data);
  XtDestroyWidget((Widget)client_data);
  if(gameover_pending > 1)
    XtPopupSpringLoaded(gameover_shell);
  gameover_pending = 0;
}


static void
QuitCallback(Widget w, XtPointer client_data, XtPointer call_data)
{
  ExitXonix(0);
}


static void
AboutCallback(Widget w, XtPointer client_data, XtPointer call_data)
{
  DoAbout();
}


static void
DoAbout(void)
{
  static Widget about = (Widget)0;
  Widget box, msg, done;
  Arg wargs[2];
  int i;
  Cardinal x, y;

  about = XtVaCreatePopupShell("about_shell", transientShellWidgetClass,
			       toplevel,
			       NULL);
  box = XtVaCreateManagedWidget("about_box", boxWidgetClass, about,
				XtNorientation,
				(XtPointer)XtorientVertical,
				NULL);
  msg = XtVaCreateManagedWidget("about_msg", labelWidgetClass, box,
				XtNlabel, about_msg,
				NULL);
  done = XtVaCreateManagedWidget("about_done", commandWidgetClass,
				 box,
				 NULL);
  
  XtAddCallback(done, XtNcallback, PopdownGeneric, (XtPointer)about);

  i = 0;
  XtSetArg(wargs[i], XtNx, (XtPointer)&x);  i++;
  XtSetArg(wargs[i], XtNy, (XtPointer)&y);  i++;
  XtGetValues(toplevel, wargs, i);
    
  i = 0;
  XtSetArg(wargs[i], XtNx, (XtPointer)(x + 10));  i++;
  XtSetArg(wargs[i], XtNy, (XtPointer)(y + 10));  i++;
  XtSetValues(about, wargs, i);

  gameover_pending = 1;
  XtPopupSpringLoaded(about);
}

void ScorePoints (int points)
{
  int i;

  for(i = 0; i < POINTS_DIGITS; i++) {
    DrawDigit(st_points[i], points % 10);
    points /= 10;
  }
}

#ifdef __unix

struct score_rec {
  unsigned score, level;
  char login[11], full[65];
  time_t tstamp;
};

static
int compare(const void *a, const void *b)
{
  const struct score_rec *ra = (struct score_rec *)a;
  const struct score_rec *rb = (struct score_rec *)b;
  return ra->score - rb->score;
}

#endif /* __unix */

void
DisplayHighScore(void)
{

#ifdef __unix
  /*
   * global high score file handling, non-unix (but X11) systems will
   * have to provide their own hooks here
   */
  FILE *high;
  struct passwd *pw;
  char *fullname = 0, *cp;
  struct score_rec score_rec[MAXSCORES];
  int i, numentries = 0;
  char tempname[sizeof(PATH_TEMPSCORE) + 15];
  char hugestring[MAXSCORES * 100];
  
  Widget box1, box2, msg, headl, done, area;
  Arg wargs[2];
  Cardinal x, y;

  if((pw = getpwuid(getuid())) == 0) {
    fprintf(stderr, "xonix: Who are you?\n");
    endpwent();
    return;
  }
  endpwent();
  if(strlen(pw->pw_gecos)) {
    /* replace & by login ID */
    if((cp = strchr(pw->pw_gecos, '&'))) {
      fullname =
	(char *)malloc(strlen(pw->pw_gecos) + strlen(pw->pw_name) + 1);
      memset(fullname, 0, strlen(pw->pw_gecos) + strlen(pw->pw_name) + 1);
      strncpy(fullname, pw->pw_gecos, cp - pw->pw_gecos);
      strcat(fullname, pw->pw_name);
      strcat(fullname, cp + 1);
      /* see if first char of login ID needs to be capitalized */
      cp = &fullname[cp - pw->pw_gecos];
      *cp = toupper(*cp);
    }
    else
      fullname = strdup(pw->pw_gecos);
    if((cp = strchr(fullname, ','))) *cp = 0; /* remove trailing garbage */
  }
  else
    fullname = strdup("(No name)"); /* the strdup() allows to free() it */
  
  /* try opening high score file, and read it */
  if((high = fopen(PATH_HIGHSCORE, "r"))) {
    for(i = 0; i < 10; i++) {
      char line[100];

      if(fgets(line, 100, high) == NULL)
	break;

      if(sscanf(line, " %u %u%*[\t]%10[^\t]%*[\t]%64[^\t] %ld",
		&score_rec[i].score, &score_rec[i].level,
		score_rec[i].login, score_rec[i].full,
		&score_rec[i].tstamp)
	 != 5) break;		/* mangled entry */
    }
    numentries = i;
    fclose(high);
  }
  if(numentries)
    qsort(score_rec, numentries, sizeof(struct score_rec), compare);

  /* make sure the new list will be world-readable */
  (void)umask(umask(0) & ~0644);
  sprintf(tempname, "%s.%d", PATH_TEMPSCORE, (int)getpid());
  if((high = fopen(tempname, "w")) == NULL) {
    fprintf(stderr, "xonix: cannot rewrite high score file\n");
    free(fullname);
    gameover_pending = 0;
    return;
  }
  
  if(numentries >= MAXSCORES && gHighScore < score_rec[0].score) {
    /* sorry, not among top ten */
    fclose(high);
    (void)unlink(tempname);
    free(fullname);
    gameover_pending = 0;
    return;
  }
  
  for(i = 0; i < numentries; i++) {
    /* look where to put entry */
    if(score_rec[i].score > gHighScore) break;
  }

#ifdef SEND_MAIL
  if(numentries > 0 && gHighScore > score_rec[numentries - 1].score &&
     strcmp(pw->pw_name, score_rec[numentries - 1].login) != 0) {
    /* we have got a new champion, warn the old one (:-) */
    FILE *mail;
    char cmd[200];
    char tbuf[20];
    struct score_rec *sp = &score_rec[numentries - 1];
    struct tm *tm;

    strncpy(cmd, PATH_RMAIL, 200);
    strncat(cmd, " \"", 200 - 2 - strlen(cmd));
    strncat(cmd, sp->login, 200 - strlen(sp->login) - strlen(cmd));
    strncat(cmd, "\"", 200 - 1 - strlen(cmd));
    
    tm = localtime(&sp->tstamp);
    strftime(tbuf, 20, "%d-%b-%y", tm);

    if((mail = popen(cmd, "w")) != NULL) {
      fprintf(mail,
	      "To: %s (%s)\n"
	      "Subject: Lost xonix championship\n\n"
	      "Your previously held first rank in the local xonix score\n"
	      "table (%u points, level %u, dated %s) has been\n"
	      "vanished today by me with %u points.\n\n"
	      "\t\tpitying you\t%s (%s)\n",
	      sp->login, sp->full,
	      sp->score, sp->level, tbuf,
	      gHighScore,
	      pw->pw_name, fullname);
      (void)pclose(mail);
    }
  }
  

#endif /* SEND_MAIL */

  if(numentries >= MAXSCORES) {
    i--;
    if(i > 0)
      /* we must shift something */
      memmove(&score_rec[0], &score_rec[1],
	      i * sizeof(struct score_rec));
  } else {
    if(numentries > i)
      /* we must shift something */
      memmove(&score_rec[i + 1], &score_rec[i],
	      (numentries - i) * sizeof(struct score_rec));
    numentries++;
  }
  
  score_rec[i].score = gHighScore;
  score_rec[i].level = gLevel;
  
  memset(score_rec[i].login, 0, 11);
  strncpy(score_rec[i].login, pw->pw_name, 10);
  memset(score_rec[i].full, 0, 65);
  strncpy(score_rec[i].full, fullname, 64);
  score_rec[i].tstamp = time(NULL);
  free(fullname);

  for(i = 0; i < numentries; i++)
    (void)fprintf(high, "%u\t%u\t%s\t%s\t%ld\n",
		  score_rec[i].score, score_rec[i].level,
		  score_rec[i].login, score_rec[i].full,
		  score_rec[i].tstamp);
  fclose(high);
  
  if(rename(tempname, PATH_HIGHSCORE))
    fprintf(stderr, "xonix: cannot install new highscore file\n");

  /* create hugestring for highscore label */
  hugestring[0] = 0;
  for(i = 0; i < numentries; i++) {
    struct score_rec *sp = &score_rec[numentries - i - 1];
    struct tm *tm;
    char tbuf[20];
    char line[100];
    
    tm = localtime(&sp->tstamp);
    strftime(tbuf, 20, "%d-%b-%y", tm);
    sprintf(line, "%5u %5u  %s %-10.10s %-.64s\n",
	    sp->score, sp->level, tbuf, sp->login, sp->full);
    strcat(hugestring, line);
  }
  
  score_shell = XtVaCreatePopupShell("score_shell",
				     transientShellWidgetClass,
				     toplevel,
				     NULL);
  box1 = XtVaCreateManagedWidget("score_box", boxWidgetClass, score_shell,
				 XtNorientation,
				 (XtPointer)XtorientVertical,
				 NULL);
  box2 = XtVaCreateManagedWidget("score_box", boxWidgetClass, box1,
				 XtNorientation,
				 (XtPointer)XtorientVertical,
				 NULL);
  msg = XtVaCreateManagedWidget("score_msg", labelWidgetClass, box2,
				NULL);
  headl = XtVaCreateManagedWidget("score_headl", labelWidgetClass, box2,
				  NULL);
  area = XtVaCreateManagedWidget("score_area", labelWidgetClass, box2,
				 XtNlabel, (XtPointer)hugestring,
				 NULL);
  done = XtVaCreateManagedWidget("score_done", commandWidgetClass,
				 box1,
				 NULL);
  XtAddCallback(done, XtNcallback, PopdownGeneric, (XtPointer)score_shell);

  i = 0;
  XtSetArg(wargs[i], XtNx, (XtPointer)&x);  i++;
  XtSetArg(wargs[i], XtNy, (XtPointer)&y);  i++;
  XtGetValues(toplevel, wargs, i);

  i = 0;
  XtSetArg(wargs[i], XtNx, (XtPointer)(x + 10));  i++;
  XtSetArg(wargs[i], XtNy, (XtPointer)(y + 100));  i++;
  XtSetValues(score_shell, wargs, i);

  gameover_pending = 1;
  XtPopupSpringLoaded(score_shell);
#else /* ! __unix */
  gameover_pending = 0;
#endif /* __unix */
}


void
ExitXonix(int status)
{
  static CallbackData cb_iknow, cb_goon;
  Arg wargs[2];
  int i;
  Cardinal x, y;

  /* game over dialog */
  Widget box, msg, buttonbox, iknow, goon;

  if(status == 0 && !exiting) {
    if(gameover_shell == 0) {
      gameover_shell = XtVaCreatePopupShell("gameover_shell",
					    transientShellWidgetClass,
					    toplevel,
					    NULL);
      box = XtVaCreateManagedWidget("gameover_box", boxWidgetClass,
				    gameover_shell,
				    XtNorientation,
				    (XtPointer)XtorientVertical,
				    NULL);
      msg = XtVaCreateManagedWidget("gameover_msg", labelWidgetClass, box,
				    NULL);
      buttonbox = XtVaCreateManagedWidget("gameover_buttonbox",
					  boxWidgetClass, box,
					  NULL);
      iknow = XtVaCreateManagedWidget("gameover_iknow", commandWidgetClass,
				      buttonbox,
				      NULL);
      goon = XtVaCreateManagedWidget("gameover_goon", commandWidgetClass,
				     buttonbox,
				     NULL);
      XtInstallAllAccelerators(gameover_shell, toplevel);
      XtInstallAllAccelerators(gameover_shell, gameover_shell);
      XtInstallAllAccelerators(toplevel, toplevel);
      XtInstallAllAccelerators(toplevel, gameover_shell);
      cb_iknow.w = cb_goon.w = gameover_shell;
      cb_iknow.button_number = 0;
      cb_goon.button_number = 1;
    
      XtAddCallback(iknow, XtNcallback, PopdownGameOver, (XtPointer)&cb_iknow);
      XtAddCallback(goon, XtNcallback, PopdownGameOver, (XtPointer)&cb_goon);
    }
    exiting++;
    i = 0;
    XtSetArg(wargs[i], XtNx, (XtPointer)&x);  i++;
    XtSetArg(wargs[i], XtNy, (XtPointer)&y);  i++;
    XtGetValues(toplevel, wargs, i);
    
    i = 0;
    XtSetArg(wargs[i], XtNx, (XtPointer)(x + 10));  i++;
    XtSetArg(wargs[i], XtNy, (XtPointer)(y + 10));  i++;
    XtSetValues(gameover_shell, wargs, i);

    if(!gameover_pending++)
      XtPopupSpringLoaded(gameover_shell);
  }
  else
    exit(status);
}


int
main(int argc, char **argv)
{
  x11_init(argc, argv);
  x11_eventloop();
  /* NOTREACHED */
  return 1;
}


#endif /* USE_X11 */
