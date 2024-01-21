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

#ifndef _McApp_h_
#define _McApp_h_

#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/Xfuncs.h>

/* FIXME: Add detection code to see if used version of gcc supports inline */
#ifdef CONFIG_INLINE
#define INLINE __inline__
#define NORETURN __attribute__ ((noreturn))
#define HAVE_INLINE
#else
#define INLINE
#define NORETURN
#undef HAVE_INLINE
#endif

#ifdef HAVE_GETTEXT
# define _M(String) dgettext("McTools", String)
# define __M(String) (String) /* noop */
#else
# define _M(String) (String)
# define __M(String) (String) /* noop */
#endif

#include "McColors.h"

#define PROFILE_DIR	".McTools"	/* Subdir in user's home where the
					 * app saves its last state before
					 * it exits. Why don't you just
					 * leave it alone, eh? (:
					 */

#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <X11/Xutil.h>

/* For convenience */
#define string_t unsigned char *

/* The various drawing modes for the 3d effect */

enum {
  _3D_OUT,
  _3D_IN,
  _3D_NONE,
};

/* The GC's are stored in an array inside the McApp structure */
enum {
  GC_NORMAL,
  GC_CLEAR,
  GC_BUSY,
  GC_SELECTED,
  GC_SELECTED_BITMAP,
  GC_SET_SELECTED_BITMAP,
  GC_BRIGHT,
  GC_DARK,
  GC_SMALL,
  GC_SET_NORMAL,
  NUM_GC,
};

#ifndef DEFAULT_FONT
#define	DEFAULT_FONT	"-b&h-lucida-medium-r-normal-sans-*-120-*-*-p-*-*-*"
#endif

#define FIXED_FONT	"fixed"

struct McApp;
struct McBitmap;
struct McText;
struct McGadget;
struct McWindow;
struct McHotkeyHandler;
struct McProfile;
struct McMenuList;

#define MCAPP_COLOR		1
#define MCAPP_ICONIC		2
#define MCAPP_REVERSE		4
#define MCAPP_SYNCED		8
#define MCAPP_RESDEBUG		16
#define MCAPP_BSTORE		32
#define MCAPP_REINIT		64
#define MCAPP_ALLOW_SENDEVENT	128
#define MCAPP_IN_EVENT_HANDLER	256
#define MCAPP_GADGET_BORDER	512

#define MCAPP_DEFAULT_EVENT_HANDLER (int (*)(struct McWindow *, XEvent *))(-42)
#define MCAPP_DEFAULT_QUIT_HANDLER (int (*)(struct McWindow *, XEvent *))(-43)

typedef struct McApp {
  XrmDatabase stateDB, resDB;
  char *color_names[MAX_COLORS];
  char *default_font_name;
  char *fixed_font_name;
  char *class;
  Display *display;
  int screen;
  Atom wmDelWin, McToolName;
  int ico_x,ico_y,ico_w,ico_h,icon_geometry_flags;
  int x,y,w,h,geometry_flags;
  int argc_save;
  char **argv_save;
  char *window_title;
  GC gc[NUM_GC];
  XFontStruct *defaultFont;
  XFontStruct *fixedFont;
  Pixmap stipple1, stipple3;
  Pixmap radio_on, radio_off, checkbox_on;
  int style:2;
  int flags:30;
  Colormap colormap;
  unsigned long colors[MAX_COLORS];
  struct McBitmap *checkmark;
  struct McBitmap *altmark;
  Window mainWindow;
  struct McWindow *firstWindow;
  struct McWindow *lastWindow;
  struct McHotkeyHandler *hotkeyHandler;
  struct McGadget *tipgad;
  Window tipwin;
  short tipx,tipy,tipw,tiph,tipmotions,tipascent;
  int tipto;
  const string_t tip;
  struct McProfile *prof;
} McApp;

typedef struct McHotkeyHandler {
  int	(*callback)(struct McHotkeyHandler *, XKeyEvent *);
  union {
    struct McGadget *gadget;
    struct McMenuList *list;
  } data;
} McHotkeyHandler;

#define MCW_CLOSEREQUEST	1   /* Mainloop should close the window ASAP */

typedef struct McWindow {
  struct McWindow *prev;
  struct McWindow *next;
  int flags;
  McApp *app;
  short window_visible, window_has_focus;
  short x,y,w,h, wm_border_width, wm_border_height;
  Window framewin, clientwin;
  Region region;
  struct McGadget *firstGadget;
  struct McGadget *lastGadget;
  struct McGadget *keyboardFocus, *firstFocus, *lastFocus;
  struct McGadget *mainButton;
  KeySym keysym;
  XComposeStatus compose;
  unsigned char keys[32];
  int keycnt;
  int	(*eventCallback)(struct McWindow *, XEvent *);
  void	(*configureCallback)(struct McWindow *);
  void  (*focusCallback)(struct McGadget *gadget);
  void *customData;
  long event_mask;
  unsigned char *selection;
  struct McGadget *selectionOwner;
} McWindow;

#define	BW	2

struct timeval;

extern McApp *McAllocApp(int *ac, char *av[], char *title, char *class,
			 XrmOptionDescRec *DescTable, int DescSize);
extern McApp *McInitApp(McApp *);
extern McApp *McCreateApp(int *ac, char *av[], char *title, char *class,
			  XrmOptionDescRec *DescTable, int DescSize);
extern void McAppDrawbox(McWindow *mcw, Window win,
			 int x,int y,int width,int height,int mode);
extern int McInRectangle(int ex, int ey, int x, int y, int width, int height);
extern McWindow *McCreateAppWindow(McApp *, int x, int y, int w, int h,
		      void (*configureCallback)(struct McWindow *),
		      int (*eventCallback)(struct McWindow *, XEvent *event));
extern XFontStruct *McLoadFont(Display *display, char *name);
extern void McAddInput(McWindow *mcw, long mask);
extern void McRemoveInput(McWindow *mcw, long mask);

extern void McAddGadgetToList(McWindow *mcw, struct McGadget *gadget);
extern void McRemoveGadgetFromList(struct McGadget *gadget);
extern void McMoveGadgetToStart(struct McGadget *gadget);

extern void McAddWindowToList(McApp *, struct McWindow *window);
extern void McRemoveWindowFromList(McApp *, struct McWindow *window);
extern void McMoveWindowToStart(McApp *, struct McWindow *window);

extern int McAppSelect(McApp *, int n, fd_set *readfds, fd_set *writefds,
		       fd_set *exceptfds, struct timeval *timeout);
extern void McDrain(McApp *);
extern void McDrainTypedEvent(McApp *, int);
extern void NORETURN McAppMainLoop(McApp *);
extern void McFreeAppPrefs(McApp *);
extern void McFreeApp(McApp *);
extern void McFreeWindow(McWindow *mcw);
extern void cleanup(int r);

/* Yeah, it's simple... (: */
extern McWindow *McCreateSimpleWindow(McApp *, const unsigned char *title,
				      int width, int height,
				      int minwidth, int minheight,
				      int x, int y,
				      void (*configureCallback)
					    (struct McWindow *),
				      int (*eventCallback)
					   (struct McWindow *, XEvent *event));

extern void McMapWindow(McWindow *mcw);
extern void McMapRaised(McWindow *mcw);

extern INLINE void McUnmapWindow(McWindow *mcw);
extern INLINE void McRaiseWindow(McWindow *mcw);
extern INLINE void McLowerWindow(McWindow *mcw);

#ifdef HAVE_INLINE
extern INLINE void McUnmapWindow(McWindow *mcw) {
  XUnmapWindow(mcw->app->display, mcw->framewin);
}
#else
extern void McUnmapWindow(McWindow *mcw);
#endif


#ifdef HAVE_INLINE
extern INLINE void McLowerWindow(McWindow *mcw) {
  XRaiseWindow(mcw->app->display, mcw->framewin);
}
#else
extern void McRaiseWindow(McWindow *mcw);
#endif

#ifdef HAVE_INLINE
extern INLINE void McRaiseWindow(McWindow *mcw) {
  XRaiseWindow(mcw->app->display, mcw->framewin);
}
#else
extern void McRaiseWindow(McWindow *mcw);
#endif

extern unsigned char *McMakePathAbsolute(unsigned char *pa);

extern void McResizeWindow(McWindow *mcw, int w, int h);

extern void McSetClipRectangle(McWindow *mcw, GC gc, XRectangle *rect);
extern void McClearClipRectangle(McWindow *mcw, GC gc);

extern char *myname, myclass[64];

#endif /* _McApp_h_ */
