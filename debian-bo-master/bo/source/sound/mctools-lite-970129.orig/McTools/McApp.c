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

#include "../config.h"

#include <X11/bitmaps/gray1>
#include <X11/bitmaps/gray3>
#include <X11/keysym.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <string.h>
#include <varargs.h>
#include <unistd.h>

#include "McColors.h"
#include "McApp.h"
#include "McGadget.h"
#include "McFocus.h"
#include "McResource.h"
#include "McTip.h"
#include "McSelection.h"
#ifdef DEBUG_CODE
#include "McDebug.h"
#endif

#undef SHOW_EXPOSE
#undef DBG_EVENTS
#undef DBG_UNUSED_EVENTS
#undef DBG_RECTANGLE

static int HandleExposeEvent(McApp *app, XEvent *event);
static int McIOErrorHandler(Display *dsp);
static void ForceFreeWindow (McWindow *mcw);
static int McAppDoEvent(McApp *app, XEvent *event);
static int DefaultEventQuitProc(McWindow *mcw, XEvent *event);
static int DefaultEventCloseProc(McWindow *mcw, XEvent *event);
static int PropagateEvent(McApp *app, McWindow *mcw, XEvent *event);

char *default_colors[] = COLORS;
char *myname;     /* This is set in McResource.c */

/* STARTUP STARTUP STARTUP STARTUP STARTUP STARTUP STARTUP STARTUP STARTUP */

McApp *McAllocApp(int *ac, char *av[], char *title, char *class,
		  XrmOptionDescRec *DescTable, int DescSize) {
  McApp *app;
  int i;

  app = (McApp *)calloc(sizeof(McApp), 1);

  app->default_font_name = DEFAULT_FONT;
  app->fixed_font_name = FIXED_FONT;

  for (i=0;i<MAX_COLORS;i++) app->color_names[i]=strdup(default_colors[i]);

  McParseOpenDisp(app,ac,av,class,DescTable,DescSize);

#if HAVE_GETTEXT
  {
    unsigned char *ldir = McGetResource(app, "localedir");
    unsigned char *pack = McGetResource(app, "package");
    if (!pack) pack=class;
    setlocale (LC_ALL, "");
    if (ldir) {
      bindtextdomain (pack, ldir);
      bindtextdomain ("McTools", ldir);
    }
#ifdef LOCALEDIR
    else {
      bindtextdomain (pack, LOCALEDIR);
      bindtextdomain ("McTools", LOCALEDIR);
    }
#endif

    textdomain (pack);
  }
#endif

  XSetIOErrorHandler(McIOErrorHandler);

  if (title)
    app->window_title=title;
  else
    app->window_title=class;
  app->class=class;

  McReadStandards(app);

  if (app->flags & MCAPP_SYNCED) {
    fprintf(stderr,_M("Synchronizing server...\n"));
    XSynchronize(app->display,1);
  }

  app->screen = DefaultScreen(app->display);

  return app;
}

McApp *McCreateApp(int *ac, char *av[], char *title, char *class,
		   XrmOptionDescRec *DescTable, int DescSize) {
  McApp *app;
  srandom(time(NULL));

  app=McAllocApp(ac, av, title, class, DescTable, DescSize);
  return McInitApp(app);
}

McApp *McInitApp(McApp *app) {
  int i;
  XGCValues values;
  XColor color, rcolor;
  Display *display = app->display;

  /*
   * Free everything
   */
  McFreeAppPrefs(app);

  app->defaultFont = McLoadFont(display,app->default_font_name);
  if (app->fixed_font_name)
    app->fixedFont = McLoadFont(display,app->fixed_font_name);
  else
    app->fixedFont = app->defaultFont;

  app->firstWindow = app->lastWindow = NULL;

  app->stipple1 = XCreateBitmapFromData(display,
					RootWindow(display, app->screen),
					gray1_bits, gray1_width, gray1_height);

  app->stipple3 = XCreateBitmapFromData(display,
					RootWindow(display, app->screen),
					gray3_bits, gray3_width, gray3_height);

  values.function = GXand;
  values.font = app->defaultFont->fid;
  values.stipple = app->stipple3;
  values.graphics_exposures = True;
  values.fill_style = FillStippled;

  if (DefaultDepth(display, app->screen) > 1) {

    app->flags |= MCAPP_COLOR;

    app->colormap = DefaultColormap(app->display, app->screen);

    for (i = 0; i < MAX_COLORS; i++) {
      if (!XLookupColor(app->display, app->colormap, app->color_names[i],
			&color, &rcolor)) {
	fprintf(stderr,_M("%s: No color named '%s', using default '%s'.\n"),
		myname,app->color_names[i],default_colors[i]);
	if (!XLookupColor(app->display, app->colormap, default_colors[i],
			    &color, &rcolor)) {
	  fprintf(stderr,
		  _M("%s: Ayieee, Can't allocate default color '%s',"
		    "commiting suicide.\n"), myname,default_colors[i]);
	  cleanup(1);
	} else {
	  app->color_names[i]=default_colors[i];
	}
      }
      if (app->flags & MCAPP_REVERSE) {
	rcolor.red=~rcolor.red;
	rcolor.green=~rcolor.green;
	rcolor.blue=~rcolor.blue;
      }

      if(!XAllocColor(app->display, app->colormap, &rcolor)) {
	fprintf(stderr,_M("%s: Cannot allocate colormap entry for '%s'.\n"),
		myname, app->color_names[i]);
	/* Hmm, lets use black for green<0x8000, white else */
	if (rcolor.green & 0x8000)
	  rcolor.pixel = WhitePixel(app->display, app->screen);
	else
	  rcolor.pixel = BlackPixel(app->display, app->screen);
      }

      app->colors[i] = rcolor.pixel;
      continue;

    }

    values.foreground = app->colors[COL_BACKGROUND];
    values.background = app->colors[COL_FOREGROUND];

    app->gc[GC_CLEAR] = XCreateGC(app->display,
				  RootWindow(app->display, app->screen),
				  GCForeground | GCBackground | GCFont |
				  GCGraphicsExposures, &values);

    values.foreground = app->colors[COL_FOREGROUND];

    values.background = app->colors[COL_BACKGROUND];

    app->gc[GC_NORMAL] = XCreateGC(app->display,
				   RootWindow(app->display, app->screen),
				   GCForeground | GCBackground | GCFont |
				   GCGraphicsExposures, &values);

    app->gc[GC_BUSY] = XCreateGC(app->display,
				 RootWindow(app->display, app->screen),
				 GCForeground | GCBackground | GCFont |
				 GCFillStyle | GCStipple | GCGraphicsExposures,
				 &values);
    
    for (i=GC_SELECTED;i<=GC_DARK;i++) {
      app->gc[i] = XCreateGC(app->display,
			     RootWindow(app->display, app->screen),
			     GCForeground | GCBackground |
			     GCFont | GCGraphicsExposures, &values);
    }

    XSetForeground(app->display, app->gc[GC_BRIGHT],
		   app->colors[COL_BRIGHT]);
    XSetForeground(app->display, app->gc[GC_DARK],
		   app->colors[COL_DARK]);

    XSetForeground(app->display, app->gc[GC_SELECTED],
		   app->colors[COL_SELECTED]);
    XSetBackground(app->display, app->gc[GC_SELECTED],
		   app->colors[COL_FOREGROUND]);

    XSetForeground(app->display, app->gc[GC_SELECTED_BITMAP],
		   app->colors[COL_FOREGROUND]);
    XSetBackground(app->display, app->gc[GC_SELECTED_BITMAP],
		   app->colors[COL_SELECTED]);

    XSetForeground(app->display, app->gc[GC_SET_SELECTED_BITMAP],
		   app->colors[COL_FOREGROUND]);
    XSetBackground(app->display, app->gc[GC_SET_SELECTED_BITMAP],
		   app->colors[COL_SELECTED]);
    XSetFunction(app->display, app->gc[GC_SET_SELECTED_BITMAP], GXcopy);

    values.function   = GXcopy;
    app->gc[GC_SET_NORMAL] = XCreateGC(app->display,
				   RootWindow(app->display, app->screen),
				   GCForeground | GCBackground | GCFont |
				   GCGraphicsExposures |
				   GCFunction, &values);
  } else { /* mono */
    app->flags &= ~MCAPP_COLOR;
    if (app->flags & MCAPP_REVERSE) {
      values.foreground = WhitePixel(app->display, app->screen);
      values.background = BlackPixel(app->display, app->screen);
    } else {
      values.foreground = BlackPixel(app->display, app->screen);
      values.background = WhitePixel(app->display, app->screen);
    }

    app->gc[GC_NORMAL] = XCreateGC(app->display,
				   RootWindow(app->display, app->screen),
				   GCForeground | GCBackground | GCFont |
				   GCGraphicsExposures, &values);

    app->gc[GC_SET_NORMAL] = XCreateGC(app->display,
				  RootWindow(app->display, app->screen),
				  GCForeground | GCBackground | GCFont |
				  GCFunction | GCStipple | GCGraphicsExposures,
				  &values);

    values.fill_style = FillStippled;
    app->gc[GC_BUSY] = XCreateGC(app->display,
				RootWindow(app->display, app->screen),
				GCForeground | GCBackground | GCFont |
				GCFillStyle | GCStipple | GCGraphicsExposures,
				&values);
    values.stipple = app->stipple1;
    values.fill_style = FillOpaqueStippled;
    app->gc[GC_BRIGHT] = XCreateGC(app->display,
				 RootWindow(app->display, app->screen),
				 GCForeground | GCBackground | GCFont |
				 GCFillStyle | GCStipple | GCGraphicsExposures,
				 &values);
    app->gc[GC_SELECTED_BITMAP]=app->gc[GC_DARK] = XCreateGC(app->display,
			RootWindow(app->display, app->screen),
			GCForeground | GCBackground | GCFont |
			GCGraphicsExposures,&values);
    app->gc[GC_SET_SELECTED_BITMAP]=app->gc[GC_DARK] = XCreateGC(app->display,
			RootWindow(app->display, app->screen),
			GCForeground | GCBackground | GCFont | GCFunction |
			GCGraphicsExposures,&values);

    app->gc[GC_SELECTED]=XCreateGC(app->display,
			    RootWindow(app->display, app->screen),
			    GCForeground | GCBackground | GCFont |
			    GCGraphicsExposures, &values);
    XSetForeground(app->display, app->gc[GC_SELECTED], values.background);
    XSetBackground(app->display, app->gc[GC_SELECTED], values.foreground);

    app->gc[GC_CLEAR] = XCreateGC(app->display,
				   RootWindow(app->display, app->screen),
				   GCForeground | GCBackground | GCFont |
				   GCGraphicsExposures, &values);
    XSetForeground(app->display, app->gc[GC_CLEAR], values.background);
    XSetBackground(app->display, app->gc[GC_CLEAR], values.foreground);
  }

  /* both, color & mono */
  return app;
}

XFontStruct *McLoadFont(Display *display, char *name) {
  XFontStruct *font;
  
  if (!(font = XLoadQueryFont(display, name))) {
    fprintf(stderr,_M("%s: Can't open font '%s', using 'fixed'.\n"),
	    myname, name);
    if (!(font = XLoadQueryFont(display, "fixed"))) {
      fprintf(stderr,_M("%s: Can't open font 'fixed' either!\n"), myname);
      exit(1);
    }
  }
  return font;
}

McWindow *McCreateAppWindow(McApp *app, int x, int y, int w, int h,
		      void (*configureCallback)(struct McWindow *),
		      int (*eventCallback)(struct McWindow *, XEvent *event)) {
  Window win;
  McWindow *mcw;
  XSetWindowAttributes attr;
  int attron;

  if (w<0) w=app->w;
  if (h<0) h=app->h;

  if (x<0) {
    x=app->x;
    if (app->geometry_flags & XNegative)
      x+=DisplayWidth(app->display,DefaultScreen(app->display))-w;
  }

  if (y<0) {
    y=app->y;
    if (app->geometry_flags & YNegative)
      y+=DisplayHeight(app->display,DefaultScreen(app->display))-h;
  }
  
  mcw=(McWindow *)calloc(sizeof(McWindow), 1);
  mcw->app=app;
  mcw->configureCallback = configureCallback;
  if (eventCallback==MCAPP_DEFAULT_EVENT_HANDLER)
    mcw->eventCallback = DefaultEventCloseProc;
  else if (eventCallback==MCAPP_DEFAULT_QUIT_HANDLER)
    mcw->eventCallback = DefaultEventQuitProc;
  else
    mcw->eventCallback = eventCallback;
  mcw->window_visible=0;
  mcw->wm_border_width=0;
  mcw->wm_border_height=0;
  mcw->firstGadget=mcw->lastGadget=NULL;
  mcw->x=x;  mcw->y=y;  mcw->w=w;  mcw->h=h;

  if (app->flags & MCAPP_COLOR) {
    win = XCreateSimpleWindow(app->display,
			      RootWindow(app->display, app->screen),
			      mcw->x, mcw->y, mcw->w, mcw->h, 0,
			      app->colors[COL_FOREGROUND],
			      app->colors[COL_BACKGROUND]);
  } else {
    win = XCreateSimpleWindow(app->display,
			      RootWindow(app->display, app->screen),
			      mcw->x, mcw->y, mcw->w, mcw->h, 0,
			      BlackPixel(app->display, app->screen),
			      WhitePixel(app->display, app->screen));
  }

#ifdef DBG_EVENTS
  printf("New app window 0x%08X\n", (int)win);
#endif

  attron=CWBitGravity;
  attr.bit_gravity = NorthWestGravity;

  if (app->flags & MCAPP_BSTORE) { /* If you really like... */
    attron|=CWBackingStore;
    attr.backing_store=WhenMapped;
  }
  XChangeWindowAttributes(app->display, win, attron, &attr);


  /*
   * Select the input events we will be wanting to deal with
   */
  mcw->event_mask=0;
  mcw->clientwin=win;
  mcw->framewin=win;
  mcw->flags = 0;

  McAddInput(mcw, 
	     FocusChangeMask |
	     KeyPressMask |
	     VisibilityChangeMask |
	     StructureNotifyMask);

  McAddWindowToList(app, mcw);
  return mcw;
}

void McAddInput(McWindow *mcw, long mask) {
  mcw->event_mask |= mask;
  XSelectInput(mcw->app->display, mcw->framewin, mcw->event_mask);
}

void McRemoveInput(McWindow *mcw, long mask) {
  mcw->event_mask &= ~mask;
  XSelectInput(mcw->app->display, mcw->framewin, mcw->event_mask);
}

/* 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D */

void McAppDrawbox(McWindow *mcw, Window win,
		  int x, int y, int width, int height, int mode) {
  GC g1, g2, g3, g4;
  McApp *app=mcw->app;
  XPoint points[6];
  if (!win) win=mcw->clientwin;

  if (app->style==0) {
    switch (mode) {
    case _3D_OUT:
      g1=app->gc[GC_BRIGHT];
      g2=app->gc[GC_DARK];
      break;
    case _3D_IN:
      g2=app->gc[GC_BRIGHT];
      g1=app->gc[GC_DARK];
      break;
    default: /* _3D_NONE */
      g1=g2=app->gc[GC_NORMAL];
      break;
    }

    points[0].x = x - BW;		points[0].y = y - BW;
    points[1].x = width + 2 * BW;	points[1].y = 0;
    points[2].x = -BW;			points[2].y = BW;
    points[3].x = -width;		points[3].y = 0;
    points[4].x = 0;			points[4].y = height;
    points[5].x = -BW;			points[5].y = BW;
    XFillPolygon(app->display, win, g1,
		 points,6, Nonconvex, CoordModePrevious);
    points[0].y = y + height + BW;
    points[1].x = BW;			points[1].y = -BW;
    points[2].x = width;		points[2].y = 0;
    points[3].x = 0;			points[3].y = -height;
    points[4].x = BW;			points[4].y = -BW;
    points[5].x = 0;			points[5].y = height + 2 * BW;
    XFillPolygon(app->display, win, g2,
		 points, 6, Nonconvex, CoordModePrevious);
    return;
  }

  switch(mode) {
  case _3D_OUT:
    g1=app->gc[GC_BRIGHT];
    g2=app->gc[GC_CLEAR];
    g3=app->gc[GC_DARK];
    g4=app->gc[GC_NORMAL];
    break;

  case _3D_IN:
    g1=app->gc[GC_DARK];
    g2=app->gc[GC_NORMAL];
    g3=app->gc[GC_CLEAR];
    g4=app->gc[GC_BRIGHT];
    break;

  default: /* _3D_NONE */
    g1=app->gc[GC_NORMAL];
    g2=app->gc[GC_CLEAR];
    g3=app->gc[GC_CLEAR];
    g4=app->gc[GC_NORMAL];
    break;
  }

  points[0].x=x-2;		points[0].y=y+height;
  points[1].x=x-2;		points[1].y=y-2;
  points[2].x=x+width;		points[2].y=y-2;
  XDrawLines(app->display, win, g1, points, 3, CoordModeOrigin);

  points[0].x=x-1;		points[0].y=y+height-1;
  points[1].x=x-1;		points[1].y=y-1;
  points[2].x=x+width-1;	points[2].y=y-1;
  XDrawLines(app->display, win, g2, points, 3, CoordModeOrigin);

  points[0].x=x-1;		points[0].y=y+height;
  points[1].x=x+width;		points[1].y=y+height;
  points[2].x=x+width;		points[2].y=y-1;
  XDrawLines(app->display, win, g3, points, 3, CoordModeOrigin);

  points[0].x=x-2;		points[0].y=y+height+1;
  points[1].x=x+width+1;	points[1].y=y+height+1;
  points[2].x=x+width+1;	points[2].y=y-2;
  XDrawLines(app->display, win, g4, points, 3, CoordModeOrigin);
}

int McInRectangle(int ex, int ey, int x, int y, int width, int height) {
#ifdef DBG_RECTANGLE
  int in;
  in= (ex >= x && ex < x + width && ey >= y && ey < y + height);

  printf("%d,%d is %s %d-%d,%d-%d\n",ex,ey,in?"inside":"outside",
	 x,x+width-1,y,y+height-1);

  return in;

#else
  if (ex >= x && ex < x + width &&
      ey >= y && ey < y + height)
    return 1;
  else
    return 0;
#endif
}

/* LISTS LISTS LISTS LISTS LISTS LISTS LISTS LISTS LISTS LISTS LISTS LISTS */

void McAddGadgetToList(McWindow *mcw, McGadget *gadget) {
  if (!(mcw->firstGadget))
    mcw->firstGadget = gadget;

  if (!(mcw->lastGadget)) {
    mcw->lastGadget = gadget;
    gadget->prev = gadget->next = NULL;
  } else {
    gadget->prev = mcw->lastGadget;
    gadget->next = NULL;
    mcw->lastGadget->next = gadget;
    mcw->lastGadget = gadget;
  }
}

void McRemoveGadgetFromList(McGadget *gadget) {
  McWindow *mcw=gadget->mcw;

  if (gadget->prev)
    gadget->prev->next=gadget->next;
  if (gadget->next)
    gadget->next->prev=gadget->prev;

  if (mcw->firstGadget == gadget)
    mcw->firstGadget = gadget->next;
  if (mcw->lastGadget == gadget)
    mcw->lastGadget = gadget->prev;

  gadget->prev = gadget->next = NULL;
}

void McMoveGadgetToStart(McGadget *gadget) {
  McWindow *mcw=gadget->mcw;

  McRemoveGadgetFromList(gadget);
  if (!(mcw->lastGadget))
    mcw->lastGadget = gadget;

  if (!(mcw->firstGadget)) {
    mcw->firstGadget = gadget;
    gadget->prev = gadget->next = NULL;
  } else {
    gadget->next = mcw->firstGadget;
    gadget->prev = NULL;
    mcw->firstGadget->prev = gadget;
    mcw->firstGadget = gadget;
  }

  McMoveWindowToStart(mcw->app, mcw);
}

void McAddWindowToList(McApp *app, McWindow *window) {
  if (!(app->firstWindow))
    app->firstWindow = window;

  if (!(app->lastWindow)) {
    app->lastWindow = window;
    window->prev = window->next = NULL;
  } else {
    window->prev = app->lastWindow;
    window->next = NULL;
    app->lastWindow->next = window;
    app->lastWindow = window;
  }
}

void McRemoveWindowFromList(McApp *app, McWindow *window) {
  if (window->prev)
    window->prev->next=window->next;
  if (window->next)
    window->next->prev=window->prev;

  if (app->firstWindow == window)
    app->firstWindow = window->next;
  if (app->lastWindow == window)
    app->lastWindow = window->prev;

  window->prev = window->next = NULL;

}    

void McMoveWindowToStart(McApp *app, McWindow *window) {

  McRemoveWindowFromList(app, window);
  if (!(app->lastWindow))
    app->lastWindow = window;

  if (!(app->firstWindow)) {
    app->firstWindow = window;
    window->prev = window->next = NULL;
  } else {
    window->next = app->firstWindow;
    window->prev = NULL;
    app->firstWindow->prev = window;
    app->firstWindow = window;
  }
}

/*#######################################################################*/

/* EVENTS EVENTS EVENTS EVENTS EVENTS EVENTS EVENTS EVENTS EVENTS EVENTS */

static int set_window_visible(McApp *app, Window win, int flag) {
  McWindow *mcw=app->firstWindow;
  while(mcw) {
    if (win==mcw->framewin) {
      mcw->window_visible = flag;
      return 1;
    }
    mcw=mcw->next;
  }
  return 0;
}

/*************************************************************************/
    
static int set_window_focus(McApp *app, Window win, int flag) {
  McWindow *mcw=app->firstWindow;
  while(mcw) {
    if (win==mcw->framewin) {
      McGadget *next;
      mcw->window_has_focus = flag;
      next=mcw->firstGadget;
      while(next) {
	if ((next==mcw->keyboardFocus) ||
	    (next->specialInfo && next->specialInfo->flags & GSP_FOCUSUPDATE))
	  McGadgetUpdate(next);
	next=next->next;
      }
      return 1;
    }
    mcw=mcw->next;
  }
  return 0;
}

/*************************************************************************/

static int HandleKeypress(McWindow *mcw, XKeyEvent *event) {
  McGadget *gad;
  int res;

  if (event->type==KeyRelease) return 1;

  mcw->keycnt=XLookupString(event, mcw->keys, sizeof(mcw->keys)-1,
			    &mcw->keysym, &mcw->compose);
  mcw->keys[mcw->keycnt]=0;

#if 0
  {
    char *type;
    if (event->type==KeyPress) type="KeyPress";
    else if (event->type==KeyRelease) type="KeyRelease";
    else type="???";
    printf("%s: keysym %d", type, (int)mcw->keysym);
    if (event->state & ShiftMask) printf(" SHIFT");
    if (event->state & LockMask) printf(" LOCK");
    if (event->state & ControlMask) printf(" CTRL");
    if (event->state & Mod1Mask) printf(" MOD1");
    if (event->state & Mod2Mask) printf(" MOD2");
    if (event->state & Mod3Mask) printf(" MOD3");
    if (event->state & Mod4Mask) printf(" MOD4");
    if (event->state & Mod5Mask) printf(" MOD5");
    printf("\n");
  }
#endif

  if ((event->state&Mod1Mask) && (mcw->app->hotkeyHandler)) {
    res=(*(mcw->app->hotkeyHandler->callback))(mcw->app->hotkeyHandler, event);
    if (res) return res;
  }

  if ((gad=mcw->keyboardFocus))
    if ((res=McGadgetKeyPress(gad, event))) return res;

  if ((gad=mcw->mainButton))
    if ((res=McGadgetKeyPress(gad, event))) return res;
 
  switch(mcw->keysym) {
    /*
     * So, you sit down, code your stuff, relying on the fact that you
     * get XK_Tab all the time with ShiftMask specifying the Shift state,
     * just for this one blessed day when someone goes ahead and silently
     * changes the keysym for Shift-Tab to XK_ISO_Left_Tab... DOH!
     * And I was first out searching for a bug in my focus handler... @#@$%!!!
     */
#ifdef XK_ISO_Left_Tab
  case XK_ISO_Left_Tab:
#endif
  case XK_KP_Tab:
  case XK_Tab:
    if (event->state&LockMask)
      McPrevFocus(mcw, 1);
    else
      McNextFocus(mcw, 1);
    return 1;

  case XK_Down:
  case XK_Right:
    McNextFocus(mcw, 0);
    return 1;

  case XK_Up:
  case XK_Left:
    McPrevFocus(mcw, 0);
    return 1;
  }

  return 0;
}

/*************************************************************************/

static INLINE int check_close(McWindow *mcw) {
  if (mcw->flags&MCW_CLOSEREQUEST) { ForceFreeWindow(mcw); return 1; }
  return 0;
}

/*************************************************************************
 *
 * Scan the gadgets and recompute their positions according to their
 * gravity settings. A nice example is the file requester...
 */
static void RecomputePositions(McWindow *mcw, int neww, int newh) {
  McGadget *next = mcw->firstGadget;
  Display *dsp = mcw->app->display;
  int oldw, oldh;

  oldw=mcw->w; oldh=mcw->h;
  mcw->w=neww; mcw->h=newh;

#ifdef DBG_CONFIGURE_NOTIFY
  printf("x=%d y=%d w=%d h=%d\n", newx, newy, neww, newh);
#endif

  if ((oldw!=neww) || (oldh!=newh)) {
    while(next) {
      switch(next->topLeftGravity) {
      case ForgetGravity:
      case NorthGravity:
      case NorthWestGravity:
      case WestGravity:
	break;
      case SouthGravity:
      case SouthWestGravity:
	McMoveGadget(next, -1, newh - (oldh - next->y));
	break;
      case NorthEastGravity:
      case EastGravity:
	McMoveGadget(next, neww - (oldw - next->x), -1);
	break;
      case SouthEastGravity:
	McMoveGadget(next, neww - (oldw - next->x), newh - (oldh - next->y));
	break;
#ifdef DEBUG_CODE
      default:
	fprintf(stderr,"%s: Funny, you used a weird gravity.\n", myname);
	break;
#endif
      }

      switch(next->bottomRightGravity) {
      case ForgetGravity:
      case NorthGravity:
      case NorthWestGravity:
      case WestGravity:
	break;
      case SouthGravity:
      case SouthWestGravity:
	if (McResizeGadget(next, -1, next->height+newh-oldh))
	  if(next->win)
	    XClearArea(dsp, next->win, 0, 0, 0, 0, True);
	break;
      case NorthEastGravity:
      case EastGravity:
	if (McResizeGadget(next, next->width + neww - oldw, -1))
	  if(next->win)
	    XClearArea(dsp, next->win, 0, 0, 0, 0, True);
	break;
      case SouthEastGravity:
	McResizeGadget(next, -1, -1);
	if (McResizeGadget(next, next->width + neww - oldw,
			   next->height+newh-oldh))
	  if(next->win)
	    XClearArea(dsp, next->win, 0, 0, 0, 0, True);
	break;
#ifdef DEBUG_CODE
    default:
	fprintf(stderr,"%s: Funny, you used a weird gravity.\n", myname);
	break;
#endif
      }

#if 0
      if (next->type == SLIDERGADGET) {
	printf("x=%d y=%d w=%d h=%d\n",
	       next->x, next->y, next->width, next->height);
      }
#endif

      next=next->next;
    }
  }

  if (mcw->configureCallback) (*mcw->configureCallback)(mcw);
}

/*****************************************************************************
 *
 * returns 1 if event was eaten
 *
 */
static int McAppDoEvent(McApp *app, XEvent *event) {
  McWindow *mcw, *nextwin;
  McGadget *next;
  int result;

#ifdef DBG_EVENTS
  {
    static Window last=0;
    if (event->type!=Expose) {
      if (last!=event->xany.window) {
	last=event->xany.window;
	printf("\n");
      }
      printf("window=0x%08X event=%s\n", (int)(event->xany.window),
	     McEventName(event->type));
    }
  }
#endif

  if ((event->xany.send_event==True) &&
      (!(app->flags & MCAPP_ALLOW_SENDEVENT)) &&
      (event->type != ClientMessage) &&
      (event->type != ConfigureRequest) &&
      (event->type != ConfigureNotify) &&
      (event->type != SelectionNotify)) {
#ifdef DBG_UNUSED_EVENTS
    if (event->type!=Expose)
      printf("rejected event: %s for window 0x%08X\n",
	     McEventName(event->type), (unsigned int)event->xany.window);
#endif
    return 0;
  }


  if (app->tipwin || app->tipto) {
    switch (event->type) {
    case ButtonRelease:
    case ButtonPress:
    case KeyPress:
    case KeyRelease:
      McTipPopdown(app);
      break;
    case MotionNotify:
      if (app->tipwin) {
	if (!McTipMotion(app, event->xmotion.x_root, event->xmotion.y_root))
	  if ((event->xmotion.x_root<app->tipx-16) ||
	      (event->xmotion.x_root>app->tipx+16) ||
	      (event->xmotion.y_root<app->tipy-16) ||
	      (event->xmotion.y_root>app->tipy+16)) McTipPopdown(app);
      } else {
	if (--(app->tipmotions)<0) {
	  McTipPopdown(app);
	} else {
	  app->tipto=800000;
	  app->tipx=event->xmotion.x_root;
	  app->tipy=event->xmotion.y_root;
	}
      }
      break;
    }
    if ((app->tipwin) && (event->xany.window==app->tipwin)) return 1;
  }

  switch (event->type) {
  case ConfigureNotify: /* Window has been resized */
    mcw=app->firstWindow;
    while(mcw) {
      nextwin=mcw->next;
      if (!(mcw->flags&MCW_CLOSEREQUEST)) {
	if (event->xconfigure.window==mcw->framewin) {
	  RecomputePositions(mcw, event->xconfigure.width,
			     event->xconfigure.height);
	  break;
	}
      }
      if (mcw->flags&MCW_CLOSEREQUEST) ForceFreeWindow(mcw);
      mcw=nextwin;
    }
    return 1;
    
  case ReparentNotify:
    /*
     * Find out about the border width & height the window manager
     * gives us. This helps popping up a window with a certain
     * gadget right under the pointer ready for click.
     * Shooting yourself in the foot using libX11...
     *   You create a window to draw the bullet, but forget some
     *   Exposure events and don't see it, so suddenly it hits your head.
     */
    mcw=app->firstWindow;
    while(mcw) {
      if (event->xany.window==mcw->framewin) {
	if (event->xreparent.parent==RootWindow(app->display, app->screen)) {
	  mcw->wm_border_width=mcw->wm_border_height=0;
	} else {
	  int x,y,foo;
	  XGetGeometry(app->display, event->xreparent.parent,
		       (Window *)&foo, &x, &y, &foo, &foo, &foo, &foo);
	  if ((x>40) || (x>40)) {
	    mcw->wm_border_width=0;
	    mcw->wm_border_height=0;
	  } else {
	    mcw->wm_border_width=x;
	    mcw->wm_border_height=y;
	  }
	}
	return 1;
      }
      mcw=mcw->next;
    }
    break;
    
  case KeyPress:
  case KeyRelease:
    mcw=app->firstWindow;
    while(mcw) {
      nextwin=mcw->next;
      if (!(mcw->flags&MCW_CLOSEREQUEST)) {
	if (event->xkey.window==mcw->framewin) {
	  int a=HandleKeypress(mcw, (XKeyEvent *)event);
	  check_close(mcw);
	  return a;
	}
      }
      mcw=nextwin;
    }
    break;

  case SelectionRequest:
    mcw=app->firstWindow;
    while(mcw) {
      if (mcw->framewin==event->xany.window) {
	McSendSelection(mcw, (XSelectionRequestEvent *)event);
	return 1;
      }
      mcw=mcw->next;
    }
    return 0;

  case SelectionClear:
  case SelectionNotify:
    mcw=app->firstWindow;
    while(mcw) {
      if (mcw->framewin==event->xany.window) {
	if (mcw->keyboardFocus) {
	  int a=McGadgetEvent(mcw->keyboardFocus, event);
	  check_close(mcw);
	  return a;
	}
	return 0;
      }
      mcw=mcw->next;
    }
    return 0;
    
  case EnterNotify:
  case LeaveNotify:
    mcw=app->firstWindow;
    while(mcw) {
      next=mcw->firstGadget;
      while(next) {
	if (next->win==event->xany.window) {
	  if (event->type==EnterNotify)
	    McTipEnter(next, event->xcrossing.x_root, event->xcrossing.y_root);
	  else if (event->type==LeaveNotify) McTipPopdown(app);
	  return 1;
	}
	next=next->next;
      }
      mcw=mcw->next;
    }
    break;
    
  case MappingNotify:
    if (event->xmapping.request==MappingKeyboard)
      XRefreshKeyboardMapping((XMappingEvent *)event);
    return 1;
    
  case UnmapNotify:
    if (set_window_visible(app, event->xunmap.window, 0)) return 1;
    return 0;
    
  case MapNotify:
    if (set_window_visible(app, event->xmap.window, 1)) return 1;
    return 0;
    
  case FocusIn:
    result=set_window_focus(app, event->xfocus.window, 1);
    if ((event->xfocus.mode==NotifyGrab) || (event->xfocus.mode==NotifyUngrab))
      return result|PropagateEvent(app, NULL, event);
    return result;
    
  case FocusOut:
    result=set_window_focus(app, event->xfocus.window, 0);
    if ((event->xfocus.mode==NotifyGrab) || (event->xfocus.mode==NotifyUngrab))
      return result|PropagateEvent(app, NULL, event);
    return result;
    
  case VisibilityNotify:
    if (set_window_visible(app, event->xvisibility.window,
			   event->xvisibility.state!=VisibilityFullyObscured))
      return 1;
    return 0;
    
  case Expose:
  case GraphicsExpose:
  case NoExpose:
    if (HandleExposeEvent(app, event)) return 1;
    break; /* Perhaps a gadget wants it? */
  }
  
  mcw=app->firstWindow;
  while(mcw) {
    nextwin=mcw->next;
    if (!(mcw->flags&MCW_CLOSEREQUEST)) {
      
      if (event->xany.window == mcw->framewin) {
#ifdef DBG_EVENTS
	printf("Eventcallback to window 0x%08X\n", (int)mcw->framewin);
#endif
	if ((mcw->eventCallback) && (*mcw->eventCallback)(mcw, event)) {
	  check_close(mcw);
	  return 1;
	}
	goto unused;
      }
      
      /*
       * First round... does this event belong to any gadget?
       */
      next = mcw->firstGadget;
      while(next) {
	if (event->xany.window == next->win) {
	  int a,b;
	  a=McGadgetEvent(next, event);
	  b=check_close(mcw);
	  if (a) return 1;
	  if (b) return 0;
	  if ((mcw->eventCallback) && (*mcw->eventCallback)(mcw, event)) {
	    check_close(mcw);
	    return 1;
	  }
	  goto unused;
	}
	next = next->next;
      }
      
      /*
       * Second round for exposures... the window is not registered, but
       * perhaps some gadget hides a private one which needs to be redrawn?
       */
      if ((event->type==Expose) ||
	  (event->type==GraphicsExpose) || (event->type==NoExpose))
	if (PropagateEvent(app, mcw, event)) return 1;
    }
    
    check_close(mcw);
    mcw=nextwin;
  }
  
 unused:
#ifdef DBG_UNUSED_EVENTS
  if (event->type!=Expose)
    printf("unused event: %s for window 0x%08X\n",
	   McEventName(event->type), (unsigned int)event->xany.window);
#endif
  
  return 0;
}

/*
 * We dunno what to do with this event, offer it to each gadget.
 */
static int PropagateEvent(McApp *app, McWindow *mcw, XEvent *event) {
  McGadget *next;

  if (!mcw) {
    mcw=app->firstWindow;
    while(mcw) {
      if (event->xany.window==mcw->framewin) break;
      mcw=mcw->next;
    }
    if (!mcw) return 0;
  }

  next = mcw->firstGadget;
  while(next) {
    if (McGadgetEvent(next, event)) {
      check_close(mcw);
      return 1;
    }
    next = next->next;
  }
  return 0;
}

/*****************************************************************************/

void McFreeWindow (McWindow *mcw) {
  if (!mcw) return;
  if (mcw->app->flags & MCAPP_IN_EVENT_HANDLER) {
    mcw->flags|=MCW_CLOSEREQUEST;
  } else {
    ForceFreeWindow(mcw);
  }
}

static void ForceFreeWindow (McWindow *mcw) {
  McGadget *gadget, *next;
  McApp *app=mcw->app;

  gadget=mcw->firstGadget;
  while(gadget) {
    next=gadget->next;
    McFreeGadget(gadget);
    gadget=next;
  }
  if (mcw->selection) free(mcw->selection);
  if (mcw->region) XDestroyRegion(mcw->region);

  if (mcw->clientwin && mcw->clientwin!=mcw->framewin)
    XDestroyWindow(app->display, mcw->clientwin);
  if (mcw->framewin) XDestroyWindow(app->display, mcw->framewin);

  McRemoveWindowFromList(app, mcw);
  free(mcw);
}

void McFreeApp(McApp *app) {
  McWindow *mcw, *nxtmcw;

  if (app) {
    McWriteResources(app);
    McFreeAppPrefs(app);
    mcw=app->firstWindow;
    while(mcw) {
      nxtmcw=mcw->next;
      ForceFreeWindow(mcw);
      mcw=nxtmcw;
    }
    if (app->checkmark)
      McFreeBitmap(app, app->checkmark);
    if (app->altmark)
      McFreeBitmap(app, app->altmark);
    XCloseDisplay(app->display);
    free(app);
  }
}

void McFreeAppPrefs(McApp *app) {
  Display *dp=app->display;
  int i;

  if (app->defaultFont) {
    XFreeFont(dp,app->defaultFont);
    app->defaultFont=NULL;
  }

  if (app->fixedFont) {
    if (app->fixed_font_name)
      XFreeFont(dp,app->fixedFont);
    app->fixedFont=NULL;
  }

#define FREEPIX(x) if (app->x) { XFreePixmap(dp, app->x); app->x=0; }
  FREEPIX(stipple1);
  FREEPIX(stipple3);
  FREEPIX(radio_on);
  FREEPIX(radio_off);
  FREEPIX(checkbox_on);

  if (app->colormap) {
    XFreeColors(dp, app->colormap, app->colors, MAX_COLORS, 0);
    app->colormap = 0;
  }

  for (i=0; i<NUM_GC; i++)
    if (app->gc[i]) { XFreeGC(dp, app->gc[i]); app->gc[i]=0; }
}

unsigned char *McMakePathAbsolute(unsigned char *pa) {
  unsigned char *name;
  unsigned char *prefix = NULL;
  unsigned char *path = pa;

  int pflen;

  if (!path) return 0;

  if (strlen(path)) {
    if (*path != '~') {
      return strdup(path);
    }

    path++;
    if (*path== '/') path++;
    prefix = getenv("HOME");
    if (!prefix) prefix="/";
  }
 
  if (!prefix) prefix = "";

  name = (unsigned char *)calloc(strlen(path) + (pflen=strlen(prefix)) + 2, 1);
  strcpy(name, prefix);
  if (name[pflen-1]!='/') name[pflen++]='/';
  strcpy(name+pflen, path);
  return name;
}

/*
 * Create a window ready for mapping, set all usual hints.
 * If x & y are non zero, make the window a transient and let it pop with
 * its x/y position right under the pointer.
 *
 */
McWindow *McCreateSimpleWindow(McApp *app, const unsigned char *title,
			       int width, int height,
			       int minwidth, int minheight,
			       int x, int y,
			       void (*configureCallback)(struct McWindow *),
			       int (*eventCallback)
					(struct McWindow *, XEvent *event)) {
  McWindow *mcw;
  XWMHints wm_hints;
  XSizeHints sizehints;
  int px, py, dummy;

  bzero(&sizehints, sizeof(XSizeHints));
  bzero(&wm_hints, sizeof(XWMHints));

  if (x && y) {
    XQueryPointer(app->display, RootWindow(app->display, app->screen),
		  (Window *)&dummy, (Window *)&dummy,
		  &px, &py, &dummy, &dummy, &dummy);
    if (app->firstWindow) {
      px-=x+app->firstWindow->wm_border_width;  
      py-=y+app->firstWindow->wm_border_height;
    } else {
      px-=x-5;
      py-=y+20; /* FIXME: A rough guess of the WM's border width */
    }
    if (px<0) px=0;
    if (py<0) py=0;
    sizehints.flags = PMinSize | USPosition;
    sizehints.x = px;
    sizehints.y = py;
  } else {
    sizehints.flags = PMinSize;
    px=-1; py=-1;
  }

  sizehints.min_width  = minwidth;
  sizehints.min_height = minheight;

  mcw=McCreateAppWindow(app, px, py, width, height,
			configureCallback, eventCallback);
  McSetHints(mcw, (char *)title, 0, NULL,  &sizehints, &wm_hints);

  if (x || y) XSetTransientForHint(app->display, app->mainWindow,
				   mcw->framewin);

  return mcw;
}

void McMapWindow(McWindow *mcw) {
  XMapWindow(mcw->app->display, mcw->framewin);
  XMapSubwindows(mcw->app->display, mcw->framewin);
}

void McMapRaised(McWindow *mcw) {
  XMapRaised(mcw->app->display, mcw->framewin);
  XMapSubwindows(mcw->app->display, mcw->framewin);
}

void McUnmapWindow(McWindow *mcw) {
  XUnmapWindow(mcw->app->display, mcw->framewin);
}

void McRaiseWindow(McWindow *mcw) {
  XRaiseWindow(mcw->app->display, mcw->framewin);
}

void McLowerWindow(McWindow *mcw) {
  XRaiseWindow(mcw->app->display, mcw->framewin);
}

/**************************************************************************/

void McResizeWindow(McWindow *mcw, int w, int h) {
  XSizeHints	 sizehints;

  if (w<0) w=mcw->w;
  if (h<0) h=mcw->h;

  bzero(&sizehints, sizeof(XSizeHints));
  sizehints.flags = USSize;
  sizehints.width=w; sizehints.height=h; 
  XSetWMProperties(mcw->app->display, mcw->framewin, NULL, NULL,
		   NULL, 0, &sizehints, NULL, NULL);
  XResizeWindow(mcw->app->display, mcw->framewin, w, h);
  /* TODO: Resize childs */

  RecomputePositions(mcw, w, h);
}

/**************************************************************************/

/*
 * A gadget wants to clip a gc down.
 */
void McSetClipRectangle(McWindow *mcw, GC gc, XRectangle *rect) {
  XSetClipRectangles(mcw->app->display, gc, 0, 0, rect, 1, Unsorted);
}

/*
 * The clipping is no longer needed
 */
void McClearClipRectangle(McWindow *mcw, GC gc) {
  XSetClipMask(mcw->app->display, gc, None);
}

/**************************************************************************/

/*
 * Collects exposure events for a single window and triggers the
 * redraw mechanism when all exposures in a row are collected.
 *
 * FIXME: This code is very ugly. Split it up in subfunctions.
 */
static int HandleExposeEvent(McApp *app, XEvent *event) {
  McWindow *mcw, *nextwin;
  McGadget *gadget;
  XRectangle rect;
  int i, c;
  Window w;

  switch(event->type) {
  case Expose:
#ifdef SHOW_EXPOSE
    printf("Expose %d: win=0x%08X  x=%d y=%d w=%d h=%d\n",
	   event->xexpose.count,
	   (int)event->xexpose.window,event->xexpose.x,event->xexpose.y,
	   event->xexpose.width,event->xexpose.height);
#endif
    rect.x=event->xexpose.x;
    rect.y=event->xexpose.y;
    rect.width=event->xexpose.width;
    rect.height=event->xexpose.height;
    c=event->xexpose.count;
    w=event->xexpose.window;
    break;

  case GraphicsExpose:
#ifdef SHOW_EXPOSE
    printf("GraphicsExpose %d: win=0x%08X  x=%d y=%d w=%d h=%d\n",
	   event->xgraphicsexpose.count, (int)event->xgraphicsexpose.drawable,
	   event->xgraphicsexpose.x,event->xgraphicsexpose.y,
	   event->xgraphicsexpose.width,event->xgraphicsexpose.height);
#endif
    rect.x=event->xgraphicsexpose.x;
    rect.y=event->xgraphicsexpose.y;
    rect.width=event->xgraphicsexpose.width;
    rect.height=event->xgraphicsexpose.height;
    c=event->xgraphicsexpose.count;
    w=event->xgraphicsexpose.drawable;
    break;

  case NoExpose:
#ifdef SHOW_EXPOSE
    printf("NoExpose: win=0x%08X\n", (int)event->xexpose.window);
#endif
    return 1;
    break;

  default:
#ifdef DEBUG_CODE
    fprintf(stderr, "Warning -- McAppDoExpose called with non expose event\n");
#endif
    return 0;
  }

  mcw=app->firstWindow;
  while(mcw) {
    nextwin=mcw->next;

    if (!mcw->firstGadget) {
      mcw=nextwin;
      continue;
    }

    if (mcw->flags&MCW_CLOSEREQUEST) {
      ForceFreeWindow(mcw);
    } else {
      if (mcw->framewin == w) return 0;
      if (mcw->clientwin == w) return 0;

      gadget=mcw->firstGadget;
      while(gadget) {
	if (gadget->win == w) {

	  /*
	   * So we found a gadget which wants the exposures.
	   * Now lets clip down the window to the area of the
	   * actual exposures to avoid redrawing of unneeded stuff.
	   * However, since this is really costly, do it only for
	   * gadgets covering more than 5000 pixels and just redraw
	   * smaller gadgets completely.
	   */

	  if ((gadget->region) || ((gadget->width*gadget->height)>5000)) {
	    if (!gadget->region) gadget->region=XCreateRegion();
	    XUnionRectWithRegion(&rect, gadget->region, gadget->region);
	    if (c>0) return 1;

#ifdef SHOW_EXPOSE
	    printf("Redrawing gadget in window 0x%08X using a Region.\n",
		   (int)(gadget->win));
#endif

	    for (i=0; i<NUM_GC; i++)
	      if (app->gc[i])
		XSetRegion(mcw->app->display, app->gc[i], gadget->region);

	    McGadgetRedraw(gadget);

	    for (i=0; i<NUM_GC; i++)
	      if (app->gc[i])
		XSetClipMask(mcw->app->display, app->gc[i], None);

	    XDestroyRegion(gadget->region);
	    gadget->region=NULL;
	  } else {
	    if (c>0) return 1;
	    McGadgetRedraw(gadget);
#ifdef SHOW_EXPOSE
	    printf("Redrawing complete gadget in window 0x%08X.\n",
		   (int)(gadget->win));
#endif
	    return 1;
	  }
	} else if (gadget->bwin == w) {
	  if (c>0) return 1;
	  McGadgetBorderRedraw(gadget, 0);
	  McGadgetUpdate(gadget);
	  return 1;
	}
	gadget=gadget->next;
      }
    }
    mcw=nextwin;
  }

  return 0;
}

/****************************************************************************/

/*
 * Be sure to call cleanup() so the application can save it's state
 */
static int McIOErrorHandler(Display *dsp) {
  fprintf(stderr, _M("%s: X connection to %s broken, cleaning up.\n"),
	  myname, DisplayString(dsp));
  cleanup(1);
  return 1;
}

/****************************************************************************/

static INLINE void SubtractTime(struct timeval *r,
				struct timeval *a, struct timeval *b) {
  struct timeval tmp;

  tmp.tv_sec=a->tv_sec-b->tv_sec;
  tmp.tv_usec=a->tv_usec-b->tv_usec;;

  while (tmp.tv_usec<0) {
    tmp.tv_usec+=1000000;
    tmp.tv_sec--;
  }

  if (tmp.tv_sec<0) { tmp.tv_sec=0; tmp.tv_usec=0; }

  r->tv_sec=tmp.tv_sec;
  r->tv_usec=tmp.tv_usec;
}

int McAppSelect(McApp *app, int n, fd_set *readfds, fd_set *writefds,
		fd_set *exceptfds, struct timeval *timeout) {
#ifndef SELECT_MODIFIES_TIMEOUT  
  struct timeval before={0,0}, after={0,0};
  struct timezone nonsense;
#endif

  struct timeval *tipto, *useto, to={0,0}, selto={0,0};
  fd_set appfds;
  int result;
  int dfd=XConnectionNumber(app->display);

  if (!readfds) {
    FD_ZERO(&appfds);
    readfds=&appfds;
  }

  if ((n-1)<dfd)
    n=dfd+1;

  /*
   * A tip-window will appear after app->tipto ms
   */
  if (app->tipto) {
    to.tv_usec=app->tipto;
    to.tv_sec=0;
    tipto=&to;
  } else {
    tipto=NULL;
  }

  /*
   * Be sure to use the shortest timeout value, if any, for the select call
   */
  if (tipto && timeout) {
    if ((tipto->tv_sec<timeout->tv_sec) ||
	((tipto->tv_sec==timeout->tv_sec)&&(tipto->tv_usec<timeout->tv_usec)))
      useto=tipto;
    else
      useto=timeout;
  } else {
    if (tipto) useto=tipto; else useto=timeout;
  }

  FD_SET(dfd, readfds);
  XFlush(app->display);

  if (useto) {
#ifndef SELECT_MODIFIES_TIMEOUT
    gettimeofday(&before, &nonsense);
#endif
    selto.tv_usec=useto->tv_usec;
    selto.tv_sec=useto->tv_sec;
    result=select(n, readfds, writefds, exceptfds, &selto);
  } else {
    result=select(n, readfds, writefds, exceptfds, NULL);
  }
  if (result<0) return result;

  if (useto) {
#ifndef SELECT_MODIFIES_TIMEOUT
    /*
     * selto now becomes `elapsed time'
     */
    gettimeofday(&after, &nonsense);
    SubtractTime(&selto, &after, &before);
#else
    /*
     * selto now becomes `elapsed time' instead of `remaining time'
     */
    SubtractTime(&selto, useto, &selto);
#endif
  }

  /*
   * Now subtract `elapsed time' from any present timeout to get
   * `remaining time' again
   */
  if (timeout) SubtractTime(timeout, timeout, &selto);

  if (tipto) {
    SubtractTime(tipto, tipto, &selto);
    app->tipto=tipto->tv_usec;
    if (app->tipto<=0) {
      app->tipto=0;
      McTipPopup(app);
    }
  }

  /*
   * Check if there are events
   */
  if (FD_ISSET(dfd, readfds)) {
    FD_CLR(dfd, readfds);
    McDrain(app);
  }

  return result;
}

void McDrain(McApp *app) {
  while(XPending(app->display)) {
    XEvent event;
    XNextEvent(app->display, &event);
    app->flags |= MCAPP_IN_EVENT_HANDLER;
    McAppDoEvent(app, &event);
    app->flags &= ~MCAPP_IN_EVENT_HANDLER;
  }
}

void McDrainTypedEvent(McApp *app, int type) {
  XEvent event;
  while(XCheckTypedEvent(app->display, type, &event)) {
    printf("foo\n");
    app->flags |= MCAPP_IN_EVENT_HANDLER;
    McAppDoEvent(app, &event);
    app->flags &= ~MCAPP_IN_EVENT_HANDLER;
  }
}

/****************************************************************************/

void NORETURN McAppMainLoop(McApp *app) {
  while(1) {
    McAppSelect(app, 0, NULL, NULL, NULL, NULL);
  }
}

/****************************************************************************/

/*
 * Default handler: Closes the window on request
 *
 */
static int DefaultEventCloseProc(McWindow *mcw, XEvent *event) {
  switch(event->type) {
  case ClientMessage:
    if ((event->xclient.format == 32) &&
	(event->xclient.data.l[0] == mcw->app->wmDelWin)) {
      mcw->flags|=MCW_CLOSEREQUEST;
      return 1;
    }
  }
  return 0;
}

static int DefaultEventQuitProc(McWindow *mcw, XEvent *event) {
  switch(event->type) {
  case ClientMessage:
    if ((event->xclient.format == 32) &&
	(event->xclient.data.l[0] == mcw->app->wmDelWin)) {
      if (mcw==mcw->app->firstWindow) cleanup(0);
      return 1;
    }
  }

  return 0;
}

