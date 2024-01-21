/*
 * Author:      William Chia-Wei Cheng (william@cs.ucla.edu)
 *
 * Copyright (C) 1990-1996, William Chia-Wei Cheng.
 *
 * Permission limited to the use, copy, display, distribute without
 * charging for a fee, and produce derivative works of "tgif" and
 * its documentation for not-for-profit purpose is hereby granted by
 * the Author, provided that the above copyright notice appears in
 * all copies made of "tgif" and that both the copyright notice
 * and this permission notice appear in supporting documentation,
 * and that the name of the Author not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  The Author makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.  All other rights (including, but not limited to, the
 * right to sell "tgif", the right to sell derivative works of
 * "tgif", and the right to distribute "tgif" for a fee) are
 * reserved by the Author.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
 * OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#ifndef lint
static char RCSid[] =
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/menu.c,v 3.0 1996/05/06 16:06:00 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "const.h"
#include "patchlvl.h"
#include "types.h"

#include "align.e"
#include "box.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "edit.e"
#include "exec.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "imgproc.e"
#include "mainloop.e"
#include "mainmenu.e"
#ifndef _NO_EXTERN
#include "menu.e"
#endif
#include "move.e"
#include "msg.e"
#include "names.e"
#include "navigate.e"
#include "obj.e"
#include "page.e"
#include "pattern.e"
#include "raster.e"
#include "rect.e"
#include "remote.e"
#include "select.e"
#include "setup.e"
#include "shape.e"
#include "special.e"
#include "stk.e"
#include "text.e"
#include "version.e"

typedef int (*MenuProc)ARGS_DECL((int X, int Y, int TrackMenubar));

int	iconWindowCreated=FALSE;
int	iconWindowShown=FALSE;
int	importingIconFile=FALSE;

int	showVersion=FALSE;
int	activeMenu=INVALID;

char	* mainMenuStr[MAXMENUS+1] = {
      "Mode", "File", "Edit", "Layout", "MoveMode", "Arrange",
      "Page", "PageLayout", "HoriAlign", "VertAlign",
      "Font", "TextStyle", "TextSize", "Shape", "StretchText",
      "LineDash", "LineStyle", "LineType", "LineWidth",
      "Fill", "Pen", "Color", "ImageProc", "Navigate", "Special",
      "Help", NULL
};
static char * mainMenuDescription[] = {
      "Mode Menu",
      "File Menu",
      "Edit Menu",
      "Layout Menu",
      "MoveMode Menu",
      "Arrange Menu",
      "Page Menu",
      "PageLayout Menu",
      "HoriAlign Menu",
      "VertAlign Menu",
      "Font Menu",
      "TextStyle Menu",
      "TextSize Menu",
      "Shape Menu",
      "StretchText Menu",
      "LineDash Menu",
      "LineStyle Menu",
      "LineType Menu",
      "LineWidth Menu",
      "Fill Menu",
      "Pen Menu",
      "Color Menu",
      "ImageProc Menu",
      "Navigate Menu",
      "Special Menu",
      "Help Menu",
      NULL
};
int	mainMenuStrWidth[MAXMENUS];

GC	textMenuGC;
GC	rvPixmapMenuGC;
int	multiMenu=FALSE;

extern char	* getenv ARGS_DECL((char *));

static int	savedZoomScale=0, savedDrawOrigX=0, savedDrawOrigY=0;
static int	savedZoomedIn=FALSE;
static int	savedDrawWinW=0, savedDrawWinH=0, savedFileModified=FALSE;

static int	menuIsMainMenu=FALSE;

static int	minimalMenubar=(-1);
static MenuProc	* curMultiMenuCallback=NULL;

static struct BBRec	excludeMenubarWinBBox;
static struct BBRec	multiMenuWinBBox; /* not include border */
static struct BBRec	excludeMultiMenuWinBBox;
static Window		multiMenuWindow=None;
static int		specialEscapeForGotoPageUnderViewMultiMenu=INVALID;

#define MENUBARMENU_MODE 0
#define MENUBARMENU_FILE 1
#define MENUBARMENU_EDIT 2
#define MENUBARMENU_LAYOUT 3
#define MENUBARMENU_ARRANGE 4
#define MENUBARMENU_VIEW 5
#define MENUBARMENU_TEXT 6
#define MENUBARMENU_GRAPHICS 7
#define MENUBARMENU_COLOR 8
#define MENUBARMENU_NAVIGATE 9
#define MENUBARMENU_SPECIAL 10
#define MENUBARMENU_HELP 11

#define MAXMENUBARMENUS 12

static char *mainMenubarMenuStr[MAXMENUBARMENUS+1] = {
   "Mode",
   "File",
   "Edit",
   "Layout",
   "Arrange",
   "View",
   "Text",
   "Graphics",
   "Color",
   "Navigate",
   "Special",
   "Help",
   NULL
};

static
int WhichMenu (X, Y, MenuX, MenuY, TextBBox)
   int		X, Y, * MenuX, * MenuY;
   struct BBRec	* TextBBox;
{
   register int	i;
   int		x=2, h=initialMenubarWindowH;
   int		gap=defaultFontWidth+(defaultFontWidth>>1);
   int		num=(minimalMenubar ? MAXMENUBARMENUS : MAXMENUS);

   for (i=0; i < num; i++)
   {
      int	w;

      w = defaultFontWidth*strlen(minimalMenubar ? mainMenubarMenuStr[i] :
            mainMenuStr[i]);
      if (x+w >= menubarWindowW)
      {
         x = 2;
         h += initialMenubarWindowH;
      }
      if (Y < h && X < x+w+gap)
      {
         if (MenuX != NULL || MenuY != NULL)
         {
            int			win_x, win_y, main_win_x, main_win_y;
            unsigned int	win_w, win_h, win_brdr_w, win_d;
            Window		root_win;

            ComputeMainWinXY (&main_win_x, &main_win_y);
            XGetGeometry (mainDisplay, menubarWindow, &root_win, &win_x, &win_y,
                  &win_w, &win_h, &win_brdr_w, &win_d);
            if (MenuX != NULL) *MenuX = main_win_x+win_x+x;
            if (MenuY != NULL)
               *MenuY = main_win_y+brdrW+win_y+h+(win_brdr_w<<1)-2;
         }
         if (TextBBox != NULL)
         {
            TextBBox->ltx = x;
            TextBBox->rbx = x+defaultFontWidth*(strlen(
                  minimalMenubar ? mainMenubarMenuStr[i] : mainMenuStr[i]));
            TextBBox->lty = h-initialMenubarWindowH;
            TextBBox->rby = h;
         }
         return i;
      }
      x += w+gap;
   }
   return (INVALID);
}

int TextMenuLoop (OrigX, OrigY, Strings, Entries, ForeColors, Valid, InitRV,
      StatusStr, MultiColor, TrackMenubar)
   int	OrigX, OrigY, Entries, MultiColor;
   int	* ForeColors, * Valid, * InitRV, TrackMenubar;
   char	* Strings[], * StatusStr[];
{
   register int	i, j, max_len, menuing=TRUE;
   Window	window, root_win, child_win;
   int		x, y, rc = INVALID, old_selected, saved_root_x, saved_root_y;
   int		* len, dsp_w, dsp_h, menu_w, menu_h, pixel;
   unsigned int	status;
   char		* left_msg=NULL, * mid_msg=NULL, * right_msg=NULL;
   XEvent	input;
   XWMHints	wmhints;
   XSetWindowAttributes	win_attrs;
   struct BBRec	menubar_win_bbox;
   XPoint	arrow_vs[4], tmp_vs[4];
   int		min_pin, max_pin, suspended=FALSE, release_suspend=FALSE;
   int		pinned_x=0, pinned_y=0, menu_pinned=FALSE;

   if (multiMenu) multiMenuWindow = None;
   if (StatusStr != NULL && Entries > 0 &&
         (mid_msg=strchr(StatusStr[0],'\n')) != NULL &&
         (right_msg=strchr(++mid_msg,'\n')) != NULL) {
      left_msg = (char*)malloc((strlen(StatusStr[0])+1)*sizeof(char));
      if (left_msg == NULL) FailAllocMessage();
      strcpy (left_msg, StatusStr[0]);
      mid_msg = strchr (left_msg, '\n');
      *mid_msg++ = '\0';
      right_msg = strchr (mid_msg, '\n');
      *right_msg++ = '\0';
      SetMouseStatus (left_msg, mid_msg, right_msg);
   } else {
      SetStringStatus ("");
   }
   if (TrackMenubar && menubarWindow != None) {
      unsigned int	win_w, win_h, win_brdr_w, win_d;
      int		win_x, win_y, main_win_x, main_win_y;

      ComputeMainWinXY (&main_win_x, &main_win_y);
      XGetGeometry (mainDisplay, menubarWindow, &root_win, &win_x, &win_y,
            &win_w, &win_h, &win_brdr_w, &win_d);
      menubar_win_bbox.ltx = main_win_x + win_x;
      menubar_win_bbox.lty = main_win_y + win_y;
      menubar_win_bbox.rbx = menubar_win_bbox.ltx + win_w;
      menubar_win_bbox.rby = menubar_win_bbox.lty + win_h;
   }

   dsp_w = DisplayWidth (mainDisplay, mainScreen);
   dsp_h = DisplayHeight (mainDisplay, mainScreen);

   len = (int*)malloc(Entries*sizeof(int));
   if (len == NULL) FailAllocMessage();
   for(i = 0, max_len = 0; i < Entries; i++) {
      len[i] = strlen (Strings[i]);
      if (len[i] > max_len) max_len = len[i];
   }
   if (multiMenu) {
      menu_w = defaultFontWidth * (max_len+1) + (defaultFontWidth>>1) + 1;
   } else {
      menu_w = defaultFontWidth * max_len + 1;
   }
   menu_h = defaultFontHeight * Entries;

   if (multiMenu) {
      arrow_vs[0].x = arrow_vs[3].x = menu_w-2;
      arrow_vs[0].y = arrow_vs[3].y = defaultFontHeight>>1;
      arrow_vs[1].x = menu_w-defaultFontWidth;
      arrow_vs[1].y = 3;
      arrow_vs[2].x = menu_w-defaultFontWidth;
      arrow_vs[2].y = (((defaultFontHeight-6)>>1)<<1)+3;
      for (j=0; j < 4; j++) tmp_vs[j].x = arrow_vs[j].x;
   }
   if (OrigX+menu_w >= dsp_w-1-2*brdrW) {
      OrigX = dsp_w - 1 - (brdrW<<1) - menu_w;
      if (multiMenuWindow != None) {
         OrigX = multiMenuWinBBox.ltx - 1 - (brdrW<<2) - menu_w;
      }
   }
   if (OrigY+menu_h >= dsp_h-1-2*brdrW) {
      OrigY = dsp_h - 1 - (brdrW<<2) - menu_h;
   }
   if (multiMenuWindow != None) {
      if (OrigX < multiMenuWinBBox.ltx-(brdrW<<1)) {
         min_pin = OrigX - mainMenuPinDistance;
      } else {
         min_pin = multiMenuWinBBox.ltx - (brdrW<<1) - mainMenuPinDistance;
      }
      if (OrigX+menu_w+(brdrW<<2) >= multiMenuWinBBox.rbx+(brdrW<<1)) {
         max_pin = OrigX + menu_w + (brdrW<<2) + mainMenuPinDistance;
      } else {
         max_pin = multiMenuWinBBox.rbx + (brdrW<<1) + mainMenuPinDistance;
      }
   } else {
      min_pin = OrigX - mainMenuPinDistance;
      max_pin = OrigX + menu_w + (brdrW<<2) + mainMenuPinDistance;
   }
   if ((window = XCreateSimpleWindow (mainDisplay, rootWindow, OrigX, OrigY,
         menu_w, menu_h, 2*brdrW, myBorderPixel, myBgPixel)) == 0) {
      Error ("TextMenuLoop()", "Can not XCreateSimpleWindow()");
   }
   if (multiMenuWindow == None) {
      multiMenuWinBBox.ltx = OrigX+(brdrW<<1);
      multiMenuWinBBox.lty = OrigY+(brdrW<<1);
      multiMenuWinBBox.rbx = OrigX+(brdrW<<1)+menu_w;
      multiMenuWinBBox.rby = OrigY+(brdrW<<1)+menu_h;
   }

   win_attrs.save_under = True;
   win_attrs.override_redirect = True;
   win_attrs.colormap = mainColormap;
   XChangeWindowAttributes (mainDisplay, window,
         CWSaveUnder | CWOverrideRedirect | CWColormap, &win_attrs);

   wmhints.flags = InputHint | StateHint;
   wmhints.input = True;
   wmhints.initial_state = NormalState;
   XSetWMHints (mainDisplay, window, &wmhints);

   old_selected = INVALID;

   XSetTransientForHint (mainDisplay, window, mainWindow);

#ifdef MAPBEFORESELECT
   XMapWindow(mainDisplay, window);
   XSelectInput(mainDisplay, window, StructureNotifyMask |
         ExposureMask | KeyPressMask | ButtonPressMask | ButtonReleaseMask |
         EnterWindowMask | LeaveWindowMask);
#else
   XSelectInput(mainDisplay, window, StructureNotifyMask |
         ExposureMask | KeyPressMask | ButtonPressMask | ButtonReleaseMask |
         EnterWindowMask | LeaveWindowMask);
   XMapWindow(mainDisplay, window);
#endif

   if (!(TrackMenubar && menubarWindow != None)) {
      XWarpPointer (mainDisplay,None,rootWindow,0,0,0,0,OrigX-2,OrigY-2);
   }
   XFlush (mainDisplay);
   XSync (mainDisplay, False);

   XQueryPointer (mainDisplay, window, &root_win, &child_win, &saved_root_x,
         &saved_root_y, &x, &y, &status);

   while (XCheckWindowEvent(mainDisplay, window, ExposureMask, &input)) ;
   while (XCheckWindowEvent(mainDisplay, window, StructureNotifyMask, &input)) ;
 
   y = defaultFontAsc;
   if (MultiColor) {
      for (i = 0; i < Entries; i++, y += defaultFontHeight) {
         if (InitRV[i]) {
            XSetForeground (mainDisplay, textMenuGC, ForeColors[i]);
            XFillRectangle (mainDisplay, window, textMenuGC, 0,
                  i*defaultFontHeight, menu_w, defaultFontHeight);
            XSetForeground (mainDisplay, textMenuGC, myBgPixel);
            XDrawString (mainDisplay, window, textMenuGC, 0,
                  defaultFontAsc+i*defaultFontHeight, Strings[i], len[i]);
         } else {
            XSetForeground (mainDisplay, textMenuGC, ForeColors[i]);
            XDrawString (mainDisplay, window, textMenuGC, 0, y,
                  Strings[i], len[i]);
         }
      }
   } else {
      XSetForeground (mainDisplay, textMenuGC, ForeColors[0]);
      pixel = ForeColors[0];
      for (i = 0; i < Entries; i++, y += defaultFontHeight) {
         if (InitRV[i]) {
            XSetForeground (mainDisplay, textMenuGC, ForeColors[i]);
            XFillRectangle (mainDisplay, window, textMenuGC, 0,
                  i*defaultFontHeight, menu_w, defaultFontHeight);
            XSetForeground (mainDisplay, textMenuGC, myBgPixel);
            pixel = myBgPixel;
            XDrawString (mainDisplay, window, textMenuGC, 0,
                  defaultFontAsc+i*defaultFontHeight, Strings[i], len[i]);
         } else {
            if (pixel != ForeColors[i]) {
               XSetForeground (mainDisplay, textMenuGC, ForeColors[i]);
               pixel = ForeColors[i];
            }
            XDrawString (mainDisplay, window, textMenuGC, 0, y, Strings[i],
                  len[i]);
            if (multiMenu) {
               for (j=0; j < 4; j++) {
                  tmp_vs[j].y = arrow_vs[j].y+defaultFontHeight*i;
               }
               XFillPolygon (mainDisplay, window, textMenuGC, tmp_vs, 4,
                     Convex, CoordModeOrigin);
            }
         }
      }
   }
   if (multiMenuWindow == None) {
      XGrabPointer (mainDisplay, window, FALSE, None,
            GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   }

   while (menuing) {
      int root_x, root_y, any_button_down;

      XQueryPointer (mainDisplay, window, &root_win, &child_win, &root_x,
            &root_y, &x, &y, &status);
      any_button_down = ((status & BUTTONSMASK) != 0);
      if (!any_button_down)
      {
         if (release_suspend && suspended) {
            suspended = FALSE;
         }
         if (!release_suspend && !menu_pinned &&
               abs(root_x-saved_root_x)<=2 && abs(root_y-saved_root_y)<=2) {
            suspended = TRUE;
            continue;
         } else if (!menu_pinned && suspended) {
            continue;
         }
         XUngrabPointer (mainDisplay, CurrentTime);
         if (multiMenuWindow != None) multiMenu = TRUE;
         menuing = FALSE;
         if (menu_pinned) {
            XDrawRectangle (mainDisplay, rootWindow, revDefaultGC,
                  pinned_x, pinned_y, menu_w+2*brdrW, menu_h+2*brdrW);
            XSetSubwindowMode (mainDisplay, revDefaultGC, ClipByChildren);
            pinned_x = root_x;
            pinned_y = root_y;
            if (menuIsMainMenu) {
               XMoveWindow (mainDisplay, mainMenuWindow, pinned_x, pinned_y);
               XMapRaised (mainDisplay, mainMenuWindow);
               XSetWindowColormap (mainDisplay, mainMenuWindow, mainColormap);
               XDefineCursor (mainDisplay, mainMenuWindow, defaultCursor);
               pinnedMainMenu = TRUE;
               mainMenuX = pinned_x;
               mainMenuY = pinned_y;
            } else {
               RealizeSubMenuWindow (pinned_x, pinned_y,
                     menu_w+2*brdrW, menu_h+2*brdrW);
            }
            rc = INVALID;
         } else if (x >= 0 && x < menu_w && y >= 0 && y < menu_h) {
            rc = (int)(y / defaultFontHeight);
            if (!Valid[rc]) rc = INVALID;
         } else {
            rc = INVALID;
         }
      } else if (menu_pinned) {
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC,
               pinned_x, pinned_y, menu_w+2*brdrW, menu_h+2*brdrW);
         pinned_x = root_x;
         pinned_y = root_y;
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC,
               pinned_x, pinned_y, menu_w+2*brdrW, menu_h+2*brdrW);
      } else if (x >= 0 && x < menu_w && y >=0 && y < menu_h) {
         int	new_selected=(int)(y / defaultFontHeight);

         release_suspend = TRUE;
         if (old_selected != new_selected ) {
            if (old_selected != INVALID && Valid[old_selected]) {
               XSetForeground (mainDisplay, textMenuGC, myBgPixel);
               XFillRectangle (mainDisplay, window, textMenuGC, 0,
                     old_selected*defaultFontHeight, menu_w, defaultFontHeight);
               XSetForeground (mainDisplay, textMenuGC,
                     ForeColors[old_selected]);
               XDrawString (mainDisplay, window, textMenuGC, 0,
                     defaultFontAsc+old_selected*defaultFontHeight,
                     Strings[old_selected], len[old_selected]);
               if (multiMenu) {
                  for (j=0; j < 4; j++) {
                     tmp_vs[j].y = arrow_vs[j].y+defaultFontHeight*old_selected;
                  }
                  XFillPolygon (mainDisplay, window, textMenuGC, tmp_vs, 4,
                        Convex, CoordModeOrigin);
               }
            }
            if (Valid[new_selected]) {
               XSetForeground (mainDisplay, textMenuGC,
                     ForeColors[new_selected]);
               XFillRectangle (mainDisplay, window, textMenuGC, 0,
                     new_selected*defaultFontHeight, menu_w, defaultFontHeight);
               XSetForeground (mainDisplay, textMenuGC, myBgPixel);
               XDrawString (mainDisplay, window, textMenuGC, 0,
                     defaultFontAsc+new_selected*defaultFontHeight,
                     Strings[new_selected], len[new_selected]);
               if (left_msg == NULL && StatusStr != NULL &&
                     StatusStr[new_selected] != NULL) {
                  SetStringStatus (StatusStr[new_selected]);
               }
               if (multiMenu) {
                  for (j=0; j < 4; j++) {
                     tmp_vs[j].y = arrow_vs[j].y+defaultFontHeight*new_selected;
                  }
                  XFillPolygon (mainDisplay, window, textMenuGC, tmp_vs, 4,
                        Convex, CoordModeOrigin);

                  excludeMultiMenuWinBBox.ltx = 0;
                  excludeMultiMenuWinBBox.lty = defaultFontHeight*new_selected;
                  excludeMultiMenuWinBBox.rbx = menu_w;
                  excludeMultiMenuWinBBox.rby =
                        defaultFontHeight*(new_selected+1);

                  multiMenuWindow = window;
                  multiMenu = FALSE;
                  rc = (*(curMultiMenuCallback[new_selected]))(
                        menu_w+(brdrW<<2)+OrigX-1,
                        (defaultFontHeight*new_selected)+OrigY, TrackMenubar);
                  if (rc >= 0 &&
                        (curMultiMenuCallback[new_selected] == PageMenu ||
                        curMultiMenuCallback[new_selected] == PageLayoutMenu)) {
                     specialEscapeForGotoPageUnderViewMultiMenu = new_selected;
                  } else {
                     specialEscapeForGotoPageUnderViewMultiMenu = INVALID;
                  }
                  multiMenu = TRUE;
                  multiMenuWindow = None;
                  if (rc != INVALID && rc != (-3)) {
                     menuing = FALSE;
                  }
               }
            } else if (left_msg == NULL) {
               SetStringStatus ("");
            }
            old_selected = new_selected;
         }
      } else if (TrackMenubar && menubarWindow != None &&
            PointInBBox (root_x, root_y, menubar_win_bbox) &&
            WhichMenu (root_x-menubar_win_bbox.ltx, root_y-menubar_win_bbox.lty,
                  NULL, NULL, NULL) != INVALID &&
            !PointInBBox (root_x-menubar_win_bbox.ltx,
            root_y-menubar_win_bbox.lty, excludeMenubarWinBBox)) {
         XUngrabPointer (mainDisplay, CurrentTime);
         menuing = FALSE;
         rc = BAD;
      } else if (multiMenuWindow != None &&
            PointInBBox (root_x, root_y, multiMenuWinBBox) &&
            !PointInBBox (root_x-multiMenuWinBBox.ltx,
            root_y-multiMenuWinBBox.lty, excludeMultiMenuWinBBox)) {
         /* we are in the multimenu window */
         menuing = FALSE;
         rc = (-3);
      } else if (!suspended && !menu_pinned &&
            (activeMenu!=INVALID || menuIsMainMenu) &&
            (root_x < min_pin || root_x > max_pin)) {
         pinned_x = root_x;
         pinned_y = root_y;
         XSetSubwindowMode (mainDisplay, revDefaultGC, IncludeInferiors);
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC,
               pinned_x, pinned_y, menu_w+2*brdrW, menu_h+2*brdrW);
         SetStringStatus ("Release the mouse to pin down the menu");
         menu_pinned = TRUE;
      } else if (old_selected != INVALID) {
         release_suspend = TRUE;
         if (Valid[old_selected]) {
            XSetForeground (mainDisplay, textMenuGC, myBgPixel);
            XFillRectangle (mainDisplay, window, textMenuGC, 0,
                  old_selected*defaultFontHeight, menu_w, defaultFontHeight);
            XSetForeground (mainDisplay, textMenuGC, ForeColors[old_selected]);
            XDrawString (mainDisplay, window, textMenuGC, 0,
                  defaultFontAsc+old_selected*defaultFontHeight,
                  Strings[old_selected], len[old_selected]);
            if (multiMenu) {
               for (j=0; j < 4; j++) {
                  tmp_vs[j].y = arrow_vs[j].y+defaultFontHeight*old_selected;
               }
               XFillPolygon (mainDisplay, window, textMenuGC, tmp_vs, 4,
                     Convex, CoordModeOrigin);
            }
            if (left_msg == NULL) SetStringStatus ("");
         }
         old_selected = INVALID;
      } else if (suspended) {
         release_suspend = TRUE;
      }
   }
   free(len);
   free(ForeColors);
   free(Valid);
   free(InitRV);
   XDestroyWindow(mainDisplay, window);

   XFlush(mainDisplay);
   XSync(mainDisplay, False);
   if (left_msg != NULL) {
      free(left_msg);
      SetMouseStatus("", "", "");
   } else {
      SetStringStatus("");
   }
   return rc;
}

int PxMpMenuLoop (OrigX, OrigY, W, H, Rows, Cols, Entries, ForeColors, PxMp,
      InitRV, StatusStr, MultiColor, TrackMenubar)
   int		OrigX, OrigY, W, H, Rows, Cols, Entries;
   int		* ForeColors, * InitRV, MultiColor, TrackMenubar;
   Pixmap	PxMp[];
   char		* StatusStr[];
{
   register int	i, j, menuing=TRUE;
   Window	window, root_win, child_win;
   int		x, y, old_selected, menu_w, menu_h, saved_root_x, saved_root_y;
   int		rc = INVALID, old_i = 0, old_j = 0, k, dsp_w, dsp_h;
   unsigned int	status;
   char		* left_msg=NULL, * mid_msg=NULL, * right_msg=NULL;
   XGCValues	values;
   XEvent	input;
   struct BBRec	menubar_win_bbox;
   unsigned long	toggle = xorOne;
   XWMHints	wmhints;
   XSetWindowAttributes	win_attrs;
   int		min_pin, max_pin, suspended=FALSE, release_suspend=FALSE;
   int		pinned_x=0, pinned_y=0, menu_pinned=FALSE;

   if (StatusStr != NULL && Entries > 0 &&
         (mid_msg=strchr(StatusStr[0],'\n')) != NULL &&
         (right_msg=strchr(++mid_msg,'\n')) != NULL) {
      left_msg = (char*)malloc((strlen(StatusStr[0])+1)*sizeof(char));
      if (left_msg == NULL) FailAllocMessage();
      strcpy (left_msg, StatusStr[0]);
      mid_msg = strchr (left_msg, '\n');
      *mid_msg++ = '\0';
      right_msg = strchr (mid_msg, '\n');
      *right_msg++ = '\0';
      SetMouseStatus (left_msg, mid_msg, right_msg);
   } else {
      SetStringStatus ("");
   }
   if (TrackMenubar && menubarWindow != None) {
      unsigned int	win_w, win_h, win_brdr_w, win_d;
      int		win_x, win_y, main_win_x, main_win_y;

      ComputeMainWinXY (&main_win_x, &main_win_y);
      XGetGeometry (mainDisplay, menubarWindow, &root_win, &win_x, &win_y,
            &win_w, &win_h, &win_brdr_w, &win_d);
      menubar_win_bbox.ltx = main_win_x + win_x;
      menubar_win_bbox.lty = main_win_y + win_y;
      menubar_win_bbox.rbx = menubar_win_bbox.ltx + win_w;
      menubar_win_bbox.rby = menubar_win_bbox.lty + win_h;
   }

   dsp_w = DisplayWidth (mainDisplay, mainScreen);
   dsp_h = DisplayHeight (mainDisplay, mainScreen);

   menu_w = W * Cols;
   menu_h = H * Rows;

   if (OrigX+menu_w >= dsp_w-1-2*brdrW) {
      OrigX = dsp_w - 1 - 2*brdrW - menu_w;
      if (multiMenuWindow != None) {
         OrigX = multiMenuWinBBox.ltx - 1 - (brdrW<<2) - menu_w;
      }
   }
   if (OrigY+menu_h >= dsp_h-1-2*brdrW) {
      OrigY = dsp_h - 1 - (brdrW<<2) - menu_h;
   }
   if (multiMenuWindow != None) {
      if (OrigX < multiMenuWinBBox.ltx-(brdrW<<1)) {
         min_pin = OrigX - mainMenuPinDistance;
      } else {
         min_pin = multiMenuWinBBox.ltx - (brdrW<<1) - mainMenuPinDistance;
      }
      if (OrigX+menu_w+(brdrW<<2) >= multiMenuWinBBox.rbx+(brdrW<<1)) {
         max_pin = OrigX + menu_w + (brdrW<<2) + mainMenuPinDistance;
      } else {
         max_pin = multiMenuWinBBox.rbx + (brdrW<<1) + mainMenuPinDistance;
      }
   } else {
      min_pin = OrigX - mainMenuPinDistance;
      max_pin = OrigX + menu_w + (brdrW<<2) + mainMenuPinDistance;
   }

   if ((window = XCreateSimpleWindow (mainDisplay, rootWindow, OrigX, OrigY,
         menu_w, menu_h, 2*brdrW, myBorderPixel, myBgPixel)) == 0) {
      Error ("PxMpMenuLoop()", "Can not XCreateSimpleWindow()");
   }
   win_attrs.save_under = True;
   win_attrs.override_redirect = True;
   win_attrs.colormap = mainColormap;
   XChangeWindowAttributes (mainDisplay, window,
         CWSaveUnder | CWOverrideRedirect | CWColormap, &win_attrs);

   wmhints.flags = InputHint | StateHint;
   wmhints.input = True;
   wmhints.initial_state = NormalState;
   XSetWMHints (mainDisplay, window, &wmhints);

   old_selected = INVALID;

   XSetTransientForHint (mainDisplay, window, mainWindow);

#ifdef MAPBEFORESELECT
   XMapWindow(mainDisplay, window);
   XSelectInput(mainDisplay, window, StructureNotifyMask |
         ExposureMask | KeyPressMask | ButtonPressMask | ButtonReleaseMask |
         EnterWindowMask | LeaveWindowMask);
#else
   XSelectInput(mainDisplay, window, StructureNotifyMask |
         ExposureMask | KeyPressMask | ButtonPressMask | ButtonReleaseMask |
         EnterWindowMask | LeaveWindowMask);
   XMapWindow(mainDisplay, window);
#endif

   if (!(TrackMenubar && menubarWindow != None)) {
      XWarpPointer (mainDisplay,None,rootWindow,0,0,0,0,OrigX-2,OrigY-2);
   }
   XFlush (mainDisplay);
   XSync (mainDisplay, False);

   XQueryPointer (mainDisplay, window, &root_win, &child_win, &saved_root_x,
         &saved_root_y, &x, &y, &status);

   while (XCheckWindowEvent(mainDisplay, window, ExposureMask, &input)) ;
   while (XCheckWindowEvent(mainDisplay, window, StructureNotifyMask, &input)) ;

   for (i = 0; i < Rows; i++) {
      for (j = 0; j < Cols; j++) {
         k = i + j * Rows;
         if (k >= Entries) break;
         if (MultiColor) {
            values.foreground = ForeColors[k];
            values.stipple = patPixmap[SOLIDPAT];
            XChangeGC (mainDisplay, rasterGC,
                  GCForeground | GCStipple, &values);
            XFillRectangle (mainDisplay, window, rasterGC, j*W, i*H, W, H);
         } else {
            if (InitRV[k]) {
               XSetForeground (mainDisplay, textMenuGC, myFgPixel);
               XFillRectangle (mainDisplay, window, textMenuGC, j*W, i*H, W, H);
               XSetStipple (mainDisplay, rvPixmapMenuGC, PxMp[k]);
               XFillRectangle (mainDisplay, window, rvPixmapMenuGC, j*W, i*H,
                  W, H);
            } else {
               XSetStipple (mainDisplay, rasterGC, PxMp[k]);
               XFillRectangle (mainDisplay, window, rasterGC, j*W, i*H, W, H);
            }
         }
      }
   }
   if (multiMenuWindow == None) {
      XGrabPointer (mainDisplay, window, FALSE, None,
            GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   }

   while (menuing) {
      int root_x, root_y, any_button_down;

      XQueryPointer (mainDisplay, window, &root_win, &child_win, &root_x,
            &root_y, &x, &y, &status);
      any_button_down = ((status & BUTTONSMASK) != 0);
      if (!any_button_down) {
         if (release_suspend && suspended) {
            suspended = FALSE;
         }
         if (!release_suspend && !menu_pinned &&
               abs(root_x-saved_root_x)<=2 && abs(root_y-saved_root_y)<=2) {
            suspended = TRUE;
            continue;
         } else if (!menu_pinned && suspended) {
            continue;
         }
         XUngrabPointer (mainDisplay, CurrentTime);
         if (multiMenuWindow != None) multiMenu = TRUE;
         menuing = FALSE;
         if (menu_pinned) {
            XDrawRectangle (mainDisplay, rootWindow, revDefaultGC,
                  pinned_x, pinned_y, menu_w+2*brdrW, menu_h+2*brdrW);
            XSetSubwindowMode (mainDisplay, revDefaultGC, ClipByChildren);
            pinned_x = root_x;
            pinned_y = root_y;
            RealizeSubMenuWindow (pinned_x, pinned_y,
                  menu_w+2*brdrW, menu_h+2*brdrW);
            rc = INVALID;
         } else if (x >= 0 && x < menu_w && y >= 0 && y < menu_h) {
            i = (int)(y / H);
            j = (int)(x / W);
            rc = i + j * Rows;
            if (rc >= Entries) rc = INVALID;
         } else {
            rc = INVALID;
         }
      } else if (menu_pinned) {
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC,
               pinned_x, pinned_y, menu_w+2*brdrW, menu_h+2*brdrW);
         pinned_x = root_x;
         pinned_y = root_y;
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC,
               pinned_x, pinned_y, menu_w+2*brdrW, menu_h+2*brdrW);
      } else if (x >= 0 && x < menu_w && y >=0 && y < menu_h) {
         int	new_selected;

         release_suspend = TRUE;
         i = (int)(y / H);
         j = (int)(x / W);
         new_selected = i + j * Rows;
         if (old_selected != new_selected) {
            if (old_selected != INVALID) {
               if (MultiColor) {
                  if (old_selected >= Entries) {
                     values.foreground = myBgPixel;
                  } else {
                     values.foreground = ForeColors[old_selected];
                  }
                  values.stipple = patPixmap[SOLIDPAT];
                  XChangeGC (mainDisplay, rasterGC,
                        GCForeground | GCStipple, &values);
                  XFillRectangle (mainDisplay, window, rasterGC,
                        old_j*W, old_i*H, W, H);
               } else if (old_selected < Entries) {
                  values.foreground = myFgPixel;
                  values.stipple = PxMp[old_selected];
                  XChangeGC (mainDisplay, rasterGC,
                        GCForeground | GCStipple, &values);
                  XFillRectangle (mainDisplay, window, rasterGC,
                        old_j*W, old_i*H, W, H);
               }
            }
            if (!MultiColor && new_selected < Entries) {
               XSetForeground (mainDisplay, textMenuGC, myFgPixel);
               XFillRectangle (mainDisplay, window, textMenuGC, j*W, i*H, W, H);
               XSetStipple (mainDisplay, rvPixmapMenuGC, PxMp[new_selected]);
               XFillRectangle (mainDisplay, window, rvPixmapMenuGC, j*W, i*H,
                  W, H);
            }
            if (left_msg == NULL) {
               if (StatusStr != NULL && new_selected < Entries &&
                     StatusStr[new_selected] != NULL) {
                  SetStringStatus (StatusStr[new_selected]);
               } else {
                  SetStringStatus ("");
               }
            }
            toggle = xorOne;
            old_selected = new_selected;
            old_i = i;
            old_j = j;
         } else if (MultiColor) {
            toggle = (toggle == xorOne) ? xorZero : xorOne;
            values.foreground = toggle;
            values.stipple = patPixmap[SOLIDPAT];
            XChangeGC (mainDisplay, rasterGC,
                  GCForeground | GCStipple, &values);
            MyBox (window, rasterGC, j*W, i*H, (j+1)*W-1, (i+1)*H-1);
         }
      } else if (TrackMenubar && menubarWindow != None &&
            PointInBBox (root_x, root_y, menubar_win_bbox) &&
            WhichMenu (root_x-menubar_win_bbox.ltx, root_y-menubar_win_bbox.lty,
                  NULL, NULL, NULL) != INVALID &&
            !PointInBBox (root_x-menubar_win_bbox.ltx,
            root_y-menubar_win_bbox.lty, excludeMenubarWinBBox)) {
         XUngrabPointer (mainDisplay, CurrentTime);
         menuing = FALSE;
         rc = BAD;
      } else if (multiMenuWindow != None &&
            PointInBBox (root_x, root_y, multiMenuWinBBox) &&
            !PointInBBox (root_x-multiMenuWinBBox.ltx,
            root_y-multiMenuWinBBox.lty, excludeMultiMenuWinBBox)) {
         /* we are in the multimenu window */
         menuing = FALSE;
         rc = (-3);
      } else if (!suspended && !menu_pinned && activeMenu!=INVALID &&
            (root_x < min_pin || root_x > max_pin)) {
         pinned_x = root_x;
         pinned_y = root_y;
         XSetSubwindowMode (mainDisplay, revDefaultGC, IncludeInferiors);
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC,
               pinned_x, pinned_y, menu_w+2*brdrW, menu_h+2*brdrW);
         SetStringStatus ("Release the mouse to pin down the menu");
         menu_pinned = TRUE;
      } else if (old_selected != INVALID) {
         release_suspend = TRUE;
         if (MultiColor) {
            if (old_selected >= Entries) {
               values.foreground = myBgPixel;
            } else {
               values.foreground = ForeColors[old_selected];
            }
            values.stipple = patPixmap[SOLIDPAT];
            XChangeGC (mainDisplay, rasterGC,
                  GCForeground | GCStipple, &values);
            XFillRectangle (mainDisplay, window, rasterGC,
                  old_j*W, old_i*H, W, H);
         } else if (old_selected < Entries) {
            values.foreground = myFgPixel;
            values.stipple = PxMp[old_selected];
            XChangeGC (mainDisplay, rasterGC,
                  GCForeground | GCStipple, &values);
            XFillRectangle (mainDisplay, window, rasterGC,
                  old_j*W, old_i*H, W, H);
         }
         if (left_msg == NULL) SetStringStatus ("");
         old_selected = INVALID;
      } else if (suspended) {
         release_suspend = TRUE;
      }
   }
   XSetForeground (mainDisplay, rasterGC, myFgPixel);

   free(ForeColors);
   free(InitRV);
   XDestroyWindow(mainDisplay, window);

   XFlush(mainDisplay);
   XSync(mainDisplay, False);
   if (left_msg != NULL) {
      free(left_msg);
      SetMouseStatus("", "", "");
   } else {
      SetStringStatus("");
   }
   return rc;
}

/*
 * static char * opStr[] =
 * {
 *    "more", "update", "vi", "init", "quit", "mainmenu", "animate",
 *    "upd_attr_val", "green", "yellow"
 * };
 * #define MAXOP 10
 * 
 * void Prompt (PromptStr, OpName, FileName)
 *    char	* PromptStr, * OpName, * FileName;
 * {
 *    char	inbuf[80];
 *  
 *    printf (PromptStr);
 *    fgets (inbuf, 80, stdin);
 *    sscanf (inbuf, "%s%s", OpName, FileName);
 *    return;
 * }
 */

int MainMenu ()
{
   int		rc = INVALID, index, * fore_colors, * valid, * init_rv, x, y;
   Window	root_win, child_win;
   unsigned int	status;
   int		root_x, root_y;

   Msg ("");
   XQueryPointer (mainDisplay, rootWindow, &root_win, &child_win, &root_x,
         &root_y, &x, &y, &status);

   menuIsMainMenu = TRUE;
   DefaultColorArrays (MAXMENUS, &fore_colors, &valid, &init_rv, NULL);
   index = TextMenuLoop (x, y, mainMenuStr, MAXMENUS, fore_colors, valid,
         init_rv, mainMenuDescription, SINGLECOLOR, FALSE);
   menuIsMainMenu = FALSE;

   if (index == INVALID) return (INVALID);

   if (index == MENU_COLOR && !colorDisplay)
   {
      Msg ("No color menu available for non-color displays.");
      return (INVALID);
   }

   CornerLoop (&x, &y);
   switch (index)
   {
      case MENU_MODE: ModeMenu (x, y, FALSE); break;
      case MENU_FILE: rc = FileMenu (x, y, FALSE); break;
      case MENU_EDIT: EditMenu (x, y, FALSE); break;
      case MENU_LAYOUT: LayoutMenu (x, y, FALSE); break;
      case MENU_MOVEMODE: MoveModeMenu (x, y, FALSE); break;
      case MENU_ARRANGE: ArrangeMenu (x, y, FALSE); break;
      case MENU_HORIALIGN: HoriAlignMenu (x, y, FALSE); break;
      case MENU_VERTALIGN: VertAlignMenu (x, y, FALSE); break;
      case MENU_FONT: FontMenu (x, y, FALSE); break;
      case MENU_STYLE: StyleMenu (x, y, FALSE); break;
      case MENU_SIZE: SizeMenu (x, y, FALSE); break;
      case MENU_SHAPE: ShapeMenu (x, y, FALSE); break;
      case MENU_STRETCHTEXT: StretchableTextModeMenu (x, y, FALSE); break;
      case MENU_LINEDASH: LineDashMenu (x, y, FALSE); break;
      case MENU_LINESTYLE: LineStyleMenu (x, y, FALSE); break;
      case MENU_LINETYPE: LineTypeMenu (x, y, FALSE); break;
      case MENU_LINEWIDTH: LineWidthMenu (x, y, FALSE); break;
      case MENU_FILL: FillMenu (x, y, FALSE); break;
      case MENU_PEN: PenMenu (x, y, FALSE); break;
      case MENU_COLOR: ColorMenu (x, y, FALSE); break;
      case MENU_IMAGEPROC: ImageProcMenu (x, y, FALSE); break;
      case MENU_NAVIGATE: NavigateMenu (x, y, FALSE); break;
      case MENU_SPECIAL: SpecialMenu (x, y, FALSE); break;
      case MENU_PAGE: PageMenu (x, y, FALSE); break;
      case MENU_PAGELAYOUT: PageLayoutMenu (x, y, FALSE); break;
      case MENU_HELP: HelpMenu (x, y, FALSE); break;
   }
   return (rc);
}

int IsPrefix (Prefix, Str, Rest)
   char	* Prefix, * Str, * * Rest;
{
   register char	* c_ptr = Prefix;

   for (*Rest=Str; *c_ptr!='\0' && **Rest!='\0'; (*Rest)++, c_ptr++)
      if (**Rest != *c_ptr)
         return (FALSE);
   return (*c_ptr == '\0' && **Rest == '/');
}

void RedrawTitleWindow()
{
   int y, len, amount, left;
   char s[MAXPATHLENGTH], name[MAXPATHLENGTH], * c_ptr, * rest;

   s[0] = '\0';
   if (curFileDefined) {
      if (*curSymDir == '\0') {
         sprintf(name, "%s/%s", curDir, curFileName);
      } else {
         sprintf(name, "%s/%s", curSymDir, curFileName);
      }
      if (IsPrefix(bootDir, name, &rest)) {
         c_ptr = ++rest;
      } else {
         c_ptr = name;
      }
      sprintf(s, "%s:%s (%1d%%)", curDomainName, c_ptr, printMag);
   } else {
      sprintf(s, "%s:[Unnamed] (%1d%%)", curDomainName, printMag);
   }
   if (pageLayoutMode==PAGE_STACK && curPage!=NULL) {
      sprintf(&s[strlen(s)], " \"%s\"",
            (curPage->name==NULL) ? "" : curPage->name);
   }
   if (fileModified) strcat(s, " [Modified]");

   if (s[0] != '\0') {
      if (showVersion) {
         XClearArea(mainDisplay, titleWindow, 0, titleWindowH/2, titleWindowW,
               titleWindowH/2, FALSE);
         XDrawString(mainDisplay, titleWindow, defaultGC, 1,
               titleWindowH/2+defaultFontAsc+1, s, strlen(s));
      } else {
         XClearArea(mainDisplay, titleWindow, 0, 0, titleWindowW, titleWindowH,
               FALSE);
         XDrawString(mainDisplay, titleWindow, defaultGC, 1, defaultFontAsc+1,
               s, strlen(s));
      }
   }
   if (showVersion) {
      if (TGIF_PATCHLEVEL == 0) {
         sprintf(s, "%s-%s", TOOL_NAME, versionString);
      } else {
         sprintf(s, "%s-%s-p%1d", TOOL_NAME, versionString, TGIF_PATCHLEVEL);
      }

      len = strlen(s);
      amount = defaultFontWidth * len;
      left = (titleWindowW - amount) / 2;

      XDrawString(mainDisplay, titleWindow, defaultGC, left, defaultFontAsc+2,
         s, len);

      for (y = 4; y < titleWindowH/2-4; y += 2) {
         XDrawLine(mainDisplay, titleWindow, defaultGC, 2, y,
               left-defaultFontWidth, y);
         XDrawLine(mainDisplay, titleWindow, defaultGC,
               left+amount+defaultFontWidth, y, titleWindowW-3, y);
      }
   }
}

static struct ObjRec	* iconTopObj=NULL, * iconBotObj=NULL;
static struct ObjRec	* iconTgifObj=NULL;
static int		justIconified=FALSE;

void RedrawIconWindow ()
{
   register struct ObjRec	* obj_ptr;

   numRedrawBBox = 0;
   for (obj_ptr = iconBotObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
      obj_ptr->tmp_parent = NULL;
      DrawObj (iconWindow, obj_ptr);
   }
   if (justIconified && iconTgifObj != NULL && iconTgifObj->fattr != NULL)
   {
      struct ObjRec	* obj_ptr;
      struct AttrRec	* attr_ptr;

      justIconified = FALSE;
      if ((attr_ptr=FindAttrWithName(iconTgifObj,"icon_init_exec_obj=",NULL)) !=
            NULL && (obj_ptr=FindObjWithName(iconBotObj, iconTgifObj,
            attr_ptr->attr_value.s,FALSE,FALSE,NULL,NULL)) != NULL &&
            (attr_ptr=FindAttrWithName(obj_ptr,EXEC_ATTR,NULL)) != NULL)
      {
         int	saved_intr_check_interval=intrCheckInterval;
         int	saved_history_depth=historyDepth;

         intrCheckInterval = 1;
         historyDepth = 0;
         ShowInterrupt (1);

         DoExec (attr_ptr, obj_ptr);

         HideInterrupt();
         intrCheckInterval = saved_intr_check_interval;
         historyDepth = saved_history_depth;
      }
   }
   justIconified = FALSE;
}

static char iconFileName[] = "tgificon";

static
void InitIcon ()
{
   struct ObjRec	* obj_ptr, * saved_tgif_obj;
   char			s[MAXPATHLENGTH], * c_ptr, msg[MAXSTRING];
   char			ext_str[MAXPATHLENGTH];
   FILE			* fp;
   int			ltx = 0, lty = 0, rbx = 0, rby = 0, seen_obj = FALSE;
   int			dx, dy, w, h, len, ext_len;
   int			x, y, read_status;
   unsigned int		icon_w, icon_h;
   XSizeHints		sizehints;
   int			tmp_linenum;
   char			tmp_filename[MAXPATHLENGTH];

   DelAllPages ();
   lastPageNum = 1;
   InitPage ();

   iconWindowCreated = FALSE;
   if (((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"NoTgifIcon")) != NULL)
         && (strcmp(c_ptr,"True") == 0 || strcmp(c_ptr,"true") == 0))
      return;
   if (((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"UseWMIconPixmap")) != NULL)
         && (strcmp(c_ptr,"True") == 0 || strcmp(c_ptr,"true") == 0))
      return;

   strcpy (s, drawPath);
   strcat (s, "/");
   if ((c_ptr = getenv ("TGIFICON")) == NULL)
   {
      if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"TGIFICON")) != NULL)
      {
         if (*c_ptr == '/')
            strcpy (s, c_ptr);
         else
            strcat (s, c_ptr);
      }
      else
         strcat (s, iconFileName);
   }
   else if (((int) strlen (c_ptr)) >= 200)
      /* too long, must be an error */
      strcat (s, iconFileName);
   else if (*c_ptr == '/')
      strcpy (s, c_ptr);
   else
      strcat (s, c_ptr);

   sprintf (ext_str, ".%s", OBJ_FILE_EXT);
   ext_len = strlen (ext_str);
   len = strlen(s);

   if (len<ext_len || strcmp(&s[len-ext_len],ext_str) != 0)
      sprintf (&(s[len]), ".%s", OBJ_FILE_EXT);

   if ((fp = fopen (s, "r")) == NULL)
   {
      fprintf (stderr, "Warning:  Can not open the tgif icon file '%s'\n.", s);
      return;
   }

   strcpy (tmp_filename, scanFileName);
   tmp_linenum = scanLineNum;
   strcpy (scanFileName, s);
   scanLineNum = 0;

   saved_tgif_obj = tgifObj;
   InitTgifObj ();

   importingFile = TRUE; /* ignore the 'state' predicate */
   importingIconFile = TRUE; /* read the 'file_attr' predicate */
   readingPageNum = 0;
   while ((read_status = ReadObj (fp, &obj_ptr)) == TRUE)
      if (obj_ptr != NULL)
      {
         AddObj (NULL, topObj, obj_ptr);
         if (!seen_obj)
         {
            seen_obj = TRUE;
            ltx = obj_ptr->bbox.ltx; lty = obj_ptr->bbox.lty;
            rbx = obj_ptr->bbox.rbx; rby = obj_ptr->bbox.rby;
         }
         else
         {
            if (obj_ptr->bbox.ltx < ltx) ltx = obj_ptr->bbox.ltx;
            if (obj_ptr->bbox.lty < lty) lty = obj_ptr->bbox.lty;
            if (obj_ptr->bbox.rbx > rbx) rbx = obj_ptr->bbox.rbx;
            if (obj_ptr->bbox.rby > rby) rby = obj_ptr->bbox.rby;
         }
      }
   strcpy (scanFileName, tmp_filename);
   scanLineNum = tmp_linenum;

   importingFile = FALSE;
   importingIconFile = FALSE;

   fclose (fp);

   if (read_status == INVALID)
   {
      sprintf (msg, "Icon file error:  file version too large (=%1d).",
            fileVersion);
      Msg (s);
      CleanUpTgifObj ();
      tgifObj = saved_tgif_obj;
      return;
   }

   w = rbx - ltx;
   h = rby - lty;
   if (w > iconWindowW)
   {
      dx = -ltx;
      iconWindowW = w;
   }
   else
      dx = -ltx+(iconWindowW-w)/2;

   if (h > iconWindowH)
   {
      dy = -lty;
      iconWindowH = h;
   }
   else
      dy = -lty+(iconWindowH-h)/2;

   for (obj_ptr = topObj; obj_ptr != NULL; obj_ptr = obj_ptr->next)
      MoveObj (obj_ptr, dx, dy);

   iconTgifObj = tgifObj;
   tgifObj = saved_tgif_obj;

   iconTopObj = topObj;
   iconBotObj = botObj;
   curPage->top = curPage->bot = topObj = botObj = NULL;

   CleanUpPage ();

   sizehints.x = 0;
   sizehints.y = 0;

   if ((c_ptr = XGetDefault (mainDisplay,TOOL_NAME,"IconGeometry")) != NULL)
   {
      int	bitmask = XParseGeometry (c_ptr, &x, &y, &icon_w, &icon_h);

      if ((bitmask & XValue) && (bitmask & YValue))
      {
         if (bitmask & XValue) sizehints.x = x;
         if (bitmask & YValue) sizehints.y = y;
         if (bitmask & XNegative) sizehints.x += DisplayWidth (mainDisplay,
               mainScreen) - iconWindowW - 2*brdrW - 1;
         if (bitmask & YNegative) sizehints.y += DisplayHeight (mainDisplay,
               mainScreen) - iconWindowH - 2*brdrW - 1;
     }
   }

   if ((iconBaseWindow = XCreateSimpleWindow (mainDisplay, rootWindow,
         sizehints.x, sizehints.y, iconWindowW+2*brdrW, iconWindowH+2*brdrW,
         brdrW, myBorderPixel, myBgPixel)) == 0)
   { fprintf (stderr, "Can not create icon base window!\n"); exit(1); }

   if ((iconWindow = XCreateSimpleWindow (mainDisplay, iconBaseWindow, 0, 0,
         iconWindowW, iconWindowH, brdrW, myBorderPixel, myBgPixel)) == 0)
   { fprintf (stderr, "Can not create icon window!\n"); exit(1); }

   XStoreName (mainDisplay, iconBaseWindow, TOOL_NAME);

   XSelectInput (mainDisplay, iconBaseWindow, StructureNotifyMask |
         VisibilityChangeMask);
   if (((c_ptr = 
         XGetDefault(mainDisplay,TOOL_NAME,"DoubleClickUnIconify")) != NULL)
         && (strcmp(c_ptr,"True") == 0 || strcmp(c_ptr,"true") == 0))
      XSelectInput (mainDisplay, iconWindow,
            ButtonPressMask | KeyPressMask | ExposureMask);
   else
      XSelectInput (mainDisplay, iconWindow, KeyPressMask | ExposureMask);

   iconWindowCreated = TRUE;
}

void InitTitle ()
{
   InitIcon ();
}

void InitMenu ()
{
   char		* c_ptr;
   XGCValues	values;

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.fill_style = FillSolid;
   values.font = defaultFontPtr->fid;
   textMenuGC = XCreateGC (mainDisplay, rootWindow,
         GCForeground | GCBackground | GCFillStyle | GCFont, &values);

   values.foreground = myBgPixel;
   values.background = myFgPixel;
   values.fill_style = FillStippled;
   rvPixmapMenuGC = XCreateGC (mainDisplay, rootWindow,
         GCForeground | GCBackground | GCFillStyle | GCFont, &values);

   InitMainMenu ();
   multiMenu = FALSE;
   curMultiMenuCallback = NULL;
   multiMenuWindow = None;

   if (minimalMenubar == (-1))
   {
      minimalMenubar = TRUE;
      if (((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"MinimalMenubar")) !=
            NULL) && (strcmp(c_ptr,"False") == 0 || strcmp(c_ptr,"false") == 0))
         minimalMenubar = FALSE;
   }
}

void CleanUpMenu ()
{
   struct ObjRec *saved_top_obj, *saved_bot_obj, *saved_tgif_obj;

   XFreeGC(mainDisplay, textMenuGC);
   XFreeGC(mainDisplay, rvPixmapMenuGC);
   CleanUpMainMenu();
   if (stackingWins != NULL) {
      free(stackingWins);
      stackingWins = NULL;
   }
   if (iconTgifObj != NULL) {
      saved_tgif_obj = tgifObj;
      tgifObj = iconTgifObj;
      CleanUpTgifObj();
      tgifObj = saved_tgif_obj;
   }
   if (iconTopObj != NULL) {
      saved_top_obj = topObj;
      saved_bot_obj = botObj;
      topObj = iconTopObj;
      botObj = iconBotObj;
      DelAllObj();
      topObj = saved_top_obj;
      botObj = saved_bot_obj;
   }
}

void SaveDrawWinInfo()
{
   savedZoomScale = zoomScale;
   savedZoomedIn = zoomedIn;
   savedDrawOrigX = drawOrigX;
   savedDrawOrigY = drawOrigY;
   savedDrawWinW = drawWinW;
   savedDrawWinH = drawWinH;
   savedFileModified = fileModified;
}

void UnIconify()
{
   register int	j, i;

   if (!iconWindowShown) return;

   iconWindowShown = FALSE;

   zoomScale = savedZoomScale;
   zoomedIn = savedZoomedIn;
   drawOrigX = savedDrawOrigX;
   drawOrigY = savedDrawOrigY;
   drawWinW = savedDrawWinW;
   drawWinH = savedDrawWinH;
   fileModified = savedFileModified;
   UpdDrawWinBBox();
   SetDefaultDrawWinClipRecs();

#ifdef notdef
   XUnmapWindow(mainDisplay, iconWindow);
#endif
   if (iconWindowCreated) XUnmapWindow(mainDisplay, iconBaseWindow);
   XMapWindow(mainDisplay, mainWindow);

   for (i = 0; i < numStacking; i++) {
      for (j = 0; j < numExtraWins; j++) {
         if (extraWinInfo[j].raise && extraWinInfo[j].window==stackingWins[i]) {
            extraWinInfo[j].mapped = TRUE;
            break;
         }
      }
      XMapRaised(mainDisplay, stackingWins[i]);
   }

   XFlush(mainDisplay);
   XSync(mainDisplay, False);
}

void Iconify()
{
   register int i;

   if (iconWindowShown) return;

   iconWindowShown = TRUE;

   SaveDrawWinInfo();
   zoomScale = 0;
   zoomedIn = FALSE;
   drawOrigX = 0;
   drawOrigY = 0;
   drawWinW = iconWindowW;
   drawWinH = iconWindowH;
   UpdDrawWinBBox();
   SetDefaultIconWinClipRecs();

   justIconified = TRUE;

#ifdef notdef
   XUnmapWindow(mainDisplay, mainWindow);
#endif

   SaveStackingOrder();

   if (pinnedMainMenu) XUnmapWindow(mainDisplay, mainMenuWindow);
   for (i = 0; i < numExtraWins; i++) {
      if (extraWinInfo[i].raise && extraWinInfo[i].mapped &&
            extraWinInfo[i].window != None) {
         XUnmapWindow(mainDisplay, extraWinInfo[i].window);
         extraWinInfo[i].mapped = FALSE;
      }
   }
   if (iconWindowCreated) {
      XMapWindow(mainDisplay, iconBaseWindow);
      XMapWindow(mainDisplay, iconWindow);
   }
}

static int iconJustClicked=FALSE;
static Time iconClickTime;

void IconEventHandler(input)
   XEvent *input;
{
   XEvent ev;
   Time click_time;

   if (input->xany.window == iconWindow && input->type == ButtonPress) {
      XButtonEvent *button_ev=(&(input->xbutton));

      if (iconWindowShown && !justIconified && button_ev->button == Button2 &&
            (button_ev->state & (ShiftMask | ControlMask))) {
         justIconified = TRUE;
         RedrawIconWindow();
      } else {
         click_time = input->xbutton.time;
         if (iconJustClicked &&
               (click_time-iconClickTime)<doubleClickInterval) {
            iconJustClicked = FALSE;
            UnIconify();
         } else {
            iconJustClicked = TRUE;
            iconClickTime = click_time;
         }
      }
   } else if (input->xany.window==iconBaseWindow && input->type==UnmapNotify) {
      UnIconify();
   } else if (input->xany.window==iconBaseWindow && input->type==MapNotify) {
      Iconify();
/*
   } else if (input->xany.window==iconBaseWindow && (input->type==MapNotify ||
         input->type == VisibilityNotify && input->xvisibility.state ==
         VisibilityUnobscured)) {
      Iconify();
 */
   } else if (input->xany.window == iconWindow && input->type == Expose) {
      if (!iconWindowShown) return;

      while (XCheckWindowEvent(mainDisplay, iconWindow, ExposureMask, &ev)) ;
      while (XCheckWindowEvent(mainDisplay, iconBaseWindow,
            StructureNotifyMask, &ev)) ;
      RedrawIconWindow();
   }
}

void TitleEventHandler(input)
   XEvent *input;
{
   XEvent ev;

   if (input->type == Expose) {
      XSync(mainDisplay, False);
      while (XCheckWindowEvent(mainDisplay, titleWindow, ExposureMask, &ev)) ;
      RedrawTitleWindow();
   } else if (input->type == EnterNotify) {
      SetMouseStatus("(none)", "(none)", "(none)");
   }
}

void CalcMenubarWindowHeight()
   /* Given menubarWindowW, set manubarWindowH to fit everything */
{
   register int i;
   int x=2, h=initialMenubarWindowH;
   int gap=defaultFontWidth+(defaultFontWidth>>1), num;
   char *c_ptr;

   if (minimalMenubar == (-1)) {
      minimalMenubar = TRUE;
      if (((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"MinimalMenubar")) !=
            NULL) && UtilStrICmp(c_ptr,"false") == 0) {
         minimalMenubar = FALSE;
      }
   }
   num = (minimalMenubar ? MAXMENUBARMENUS : MAXMENUS);
   for (i=0; i < num; i++) {
      int w;

      w = defaultFontWidth*strlen(minimalMenubar ? mainMenubarMenuStr[i]:
            mainMenuStr[i]);
      x += w;
      if (x >= menubarWindowW) {
         x = 2+w+gap;
         h += initialMenubarWindowH;
      } else {
         x += gap;
      }
   }
   menubarWindowH = h;
}

void RedrawMenubarWindow()
{
   register int i;
   int x=2, y=defaultFontAsc, num=(minimalMenubar ? MAXMENUBARMENUS : MAXMENUS);
   int gap=defaultFontWidth+(defaultFontWidth>>1);

   XClearWindow(mainDisplay, menubarWindow);
   for (i=0; i < num; i++) {
      int len, w;

      len = strlen(minimalMenubar ? mainMenubarMenuStr[i] : mainMenuStr[i]);
      w = defaultFontWidth*len;
      if (x+w >= menubarWindowW) {
         x = 2;
         y += initialMenubarWindowH;
      }
      XDrawString(mainDisplay, menubarWindow, defaultGC, x, y,
            minimalMenubar ? mainMenubarMenuStr[i] : mainMenuStr[i], len);
      x += w+gap;
   }
}

#define MAXVIEWMULTIMENUS 5

static MenuProc viewMenuCallback[MAXVIEWMULTIMENUS] =
{
   PageMenu,
   PageLayoutMenu,
   HoriAlignMenu,
   VertAlignMenu,
   MoveModeMenu
};
static char * viewMultiMenuStr[MAXVIEWMULTIMENUS] =
{
   "Page",
   "PageLayout",
   "HoriAlign",
   "VertAlign",
   "MoveMode"
};

static
int ViewMultiMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   specialEscapeForGotoPageUnderViewMultiMenu = INVALID;

   DefaultColorArrays(MAXVIEWMULTIMENUS, &fore_colors, &valid, &init_rv, NULL);
   activeMenu = INVALID;
   multiMenu = TRUE;
   curMultiMenuCallback = viewMenuCallback;
   index = TextMenuLoop(X, Y, viewMultiMenuStr, MAXVIEWMULTIMENUS, fore_colors,
         valid, init_rv, NULL, SINGLECOLOR, TrackMenubar);
   curMultiMenuCallback = NULL;
   multiMenu = FALSE;

   if (specialEscapeForGotoPageUnderViewMultiMenu != INVALID) {
      switch (specialEscapeForGotoPageUnderViewMultiMenu) {
      case 0: PageSubMenu(index); break;
      case 1: PageLayoutSubMenu(index); break;
      }
      specialEscapeForGotoPageUnderViewMultiMenu = INVALID;
   }
   return index;
}

#define MAXTEXTMULTIMENUS 4

static MenuProc textMenuCallback[MAXTEXTMULTIMENUS] = {
   FontMenu,
   StyleMenu,
   SizeMenu,
   StretchableTextModeMenu
};
static char * textMultiMenuStr[MAXTEXTMULTIMENUS] = {
   "Font",
   "TextStyle",
   "TextSize",
   "StretchText"
};

static
int TextMultiMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   DefaultColorArrays(MAXTEXTMULTIMENUS, &fore_colors, &valid, &init_rv, NULL);
   activeMenu = INVALID;
   multiMenu = TRUE;
   curMultiMenuCallback = textMenuCallback;
   index = TextMenuLoop(X, Y, textMultiMenuStr, MAXTEXTMULTIMENUS, fore_colors,
         valid, init_rv, NULL, SINGLECOLOR, TrackMenubar);
   curMultiMenuCallback = NULL;
   multiMenu = FALSE;

   return index;
}

#define MAXGRAPHICSMULTIMENUS 8

static MenuProc graphicsMenuCallback[MAXGRAPHICSMULTIMENUS+1] = {
   ShapeMenu,
   LineDashMenu,
   LineStyleMenu,
   LineTypeMenu,
   LineWidthMenu,
   FillMenu,
   PenMenu,
   ImageProcMenu,
   NULL
};
static char * graphicsMultiMenuStr[MAXGRAPHICSMULTIMENUS+1] = {
   "Shape",
   "LineDash",
   "LineStyle",
   "LineType",
   "LineWidth",
   "Fill",
   "Pen",
   "ImageProc",
   NULL
};

static
int GraphicsMultiMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   DefaultColorArrays(MAXGRAPHICSMULTIMENUS, &fore_colors, &valid, &init_rv,
         NULL);
   activeMenu = INVALID;
   multiMenu = TRUE;
   curMultiMenuCallback = graphicsMenuCallback;
   index = TextMenuLoop(X, Y, graphicsMultiMenuStr, MAXGRAPHICSMULTIMENUS,
         fore_colors, valid, init_rv, NULL, SINGLECOLOR, TrackMenubar);
   curMultiMenuCallback = NULL;
   multiMenu = FALSE;

   return index;
}

static
int PullDownFromMenubar(index, x, y, text_bbox)
   int index, x, y;
   struct BBRec *text_bbox;
{
   int rc=BAD;

   while (rc == BAD) {
      if (index != INVALID) {
         XSetForeground(mainDisplay, textMenuGC, myFgPixel);
         XFillRectangle(mainDisplay, menubarWindow, textMenuGC,
               text_bbox->ltx-2, text_bbox->lty,
               text_bbox->rbx-text_bbox->ltx+4,
               text_bbox->rby-text_bbox->lty);
         XSetForeground(mainDisplay, textMenuGC, myBgPixel);
         XDrawString(mainDisplay, menubarWindow, textMenuGC, text_bbox->ltx,
               defaultFontAsc+text_bbox->lty,
               minimalMenubar ? mainMenubarMenuStr[index] : mainMenuStr[index],
               strlen(minimalMenubar ? mainMenubarMenuStr[index] :
               mainMenuStr[index]));
         excludeMenubarWinBBox.ltx = text_bbox->ltx-2;
         excludeMenubarWinBBox.lty = text_bbox->lty;
         excludeMenubarWinBBox.rbx = text_bbox->rbx+2*defaultFontWidth+2;
         excludeMenubarWinBBox.rby = text_bbox->rby+2;
      }
      if (minimalMenubar) {
         switch (index) {
         case MENUBARMENU_MODE: rc = ModeMenu(x, y, TRUE); break;
         case MENUBARMENU_FILE: rc = FileMenu(x, y, TRUE); break;
         case MENUBARMENU_EDIT: rc = EditMenu(x, y, TRUE); break;
         case MENUBARMENU_LAYOUT: rc = LayoutMenu(x, y, TRUE); break;
         case MENUBARMENU_ARRANGE: rc = ArrangeMenu(x, y, TRUE); break;
         case MENUBARMENU_VIEW: rc = ViewMultiMenu(x, y, TRUE); break;
         case MENUBARMENU_TEXT: rc = TextMultiMenu(x, y, TRUE); break;
         case MENUBARMENU_GRAPHICS: rc = GraphicsMultiMenu(x, y, TRUE); break;
         case MENUBARMENU_COLOR: rc = ColorMenu(x, y, TRUE); break;
         case MENUBARMENU_NAVIGATE: rc = NavigateMenu(x, y, TRUE); break;
         case MENUBARMENU_SPECIAL: rc = SpecialMenu(x, y, TRUE); break;
         case MENUBARMENU_HELP: rc = HelpMenu(x, y, TRUE); break;
         }
      } else {
         switch (index) {
         case MENU_MODE: rc=ModeMenu(x, y, TRUE); break;
         case MENU_FILE: rc=FileMenu(x, y, TRUE); break;
         case MENU_EDIT: rc=EditMenu(x, y, TRUE); break;
         case MENU_LAYOUT: rc=LayoutMenu(x, y, TRUE); break;
         case MENU_MOVEMODE: rc=MoveModeMenu(x, y, TRUE); break;
         case MENU_ARRANGE: rc=ArrangeMenu(x, y, TRUE); break;
         case MENU_HORIALIGN: rc=HoriAlignMenu(x, y, TRUE); break;
         case MENU_VERTALIGN: rc=VertAlignMenu(x, y, TRUE); break;
         case MENU_FONT: rc=FontMenu(x, y, TRUE); break;
         case MENU_STYLE: rc=StyleMenu(x, y, TRUE); break;
         case MENU_SIZE: rc=SizeMenu(x, y, TRUE); break;
         case MENU_SHAPE: rc=ShapeMenu(x, y, TRUE); break;
         case MENU_STRETCHTEXT: rc=StretchableTextModeMenu(x, y, TRUE); break;
         case MENU_LINEDASH: rc=LineDashMenu(x, y, TRUE); break;
         case MENU_LINESTYLE: rc=LineStyleMenu(x, y, TRUE); break;
         case MENU_LINETYPE: rc=LineTypeMenu(x, y, TRUE); break;
         case MENU_LINEWIDTH: rc=LineWidthMenu(x, y, TRUE); break;
         case MENU_FILL: rc=FillMenu(x, y, TRUE); break;
         case MENU_PEN: rc=PenMenu(x, y, TRUE); break;
         case MENU_COLOR: rc=ColorMenu(x, y, TRUE); break;
         case MENU_IMAGEPROC: rc=ImageProcMenu(x, y, TRUE); break;
         case MENU_NAVIGATE: rc=NavigateMenu(x, y, TRUE); break;
         case MENU_SPECIAL: rc=SpecialMenu(x, y, TRUE); break;
         case MENU_PAGE: rc=PageMenu(x, y, TRUE); break;
         case MENU_PAGELAYOUT: rc=PageLayoutMenu(x, y, TRUE); break;
         case MENU_HELP: rc=HelpMenu(x, y, TRUE); break;
         }
      }
      if (index != INVALID) {
         XSetForeground(mainDisplay, textMenuGC, myBgPixel);
         XFillRectangle(mainDisplay, menubarWindow, textMenuGC,
               text_bbox->ltx-2, text_bbox->lty,
               text_bbox->rbx-text_bbox->ltx+4,
               text_bbox->rby-text_bbox->lty);
         XSetForeground(mainDisplay, textMenuGC, myFgPixel);
         XDrawString(mainDisplay, menubarWindow, textMenuGC, text_bbox->ltx,
               defaultFontAsc+text_bbox->lty,
               minimalMenubar ? mainMenubarMenuStr[index] : mainMenuStr[index],
               strlen(minimalMenubar ? mainMenubarMenuStr[index] :
               mainMenuStr[index]));
      }
      if (rc == BAD) {
         Window root_win, child_win;
         int mouse_x, mouse_y, root_x, root_y;
         unsigned int status;

         XQueryPointer(mainDisplay, menubarWindow, &root_win, &child_win,
               &root_x, &root_y, &mouse_x, &mouse_y, &status);
         index = WhichMenu(mouse_x, mouse_y, &x, &y, text_bbox);
         if (!(status & BUTTONSMASK) && index == (-1)) return INVALID;
      } else if (index != MENU_FILE) {
         return INVALID;
      }
   }
   return rc;
}

static struct MouseStatusStrRec menubarMouseStatus[] = {
   { "", "Mode Menu", "" },
   { "", "File Menu", "" },
   { "", "Edit Menu", "" },
   { "", "Layout Menu", "" },
   { "", "MoveMode Menu", "" },
   { "", "Arrange Menu", "" },
   { "", "Page Menu", "" },
   { "", "PageLayout Menu", "" },
   { "", "HoriAlign Menu", "" },
   { "", "VertAlign Menu", "" },
   { "", "Font Menu", "" },
   { "", "TextStyle Menu", "" },
   { "", "TextSize Menu", "" },
   { "", "Shape Menu", "" },
   { "", "StretchText Menu", "" },
   { "", "LineDash Menu", "" },
   { "", "LineStyle Menu", "" },
   { "", "LineType Menu", "" },
   { "", "LineWidth Menu", "" },
   { "", "Fill Menu", "" },
   { "", "Pen Menu", "" },
   { "", "Color Menu", "" },
   { "", "ImageProc Menu", "" },
   { "", "Navigate Menu", "" },
   { "", "Special Menu", "" },
   { "", "Help Menu", "" },
   { NULL, NULL, NULL }
};
static struct MouseStatusStrRec shortMenubarMouseStatus[] = {
   { "", "Mode Menu", "" },
   { "", "File Menu", "" },
   { "", "Edit Menu", "" },
   { "", "Layout Menu", "" },
   { "", "Arrange Menu", "" },
   { "", "View MultiMenu", "" },
   { "", "Text MultiMenu", "" },
   { "", "Graphics MultiMenu", "" },
   { "", "Color Menu", "" },
   { "", "Navigate Menu", "" },
   { "", "Special Menu", "" },
   { "", "Help Menu", "" },
   { NULL, NULL, NULL }
};

int MenubarEventHandler(input)
   XEvent *input;
{
   XEvent ev;
   int rc=INVALID;

   if (input->type == Expose) {
      XSync(mainDisplay, False);
      while (XCheckWindowEvent(mainDisplay,menubarWindow,ExposureMask,&ev)) ;
      RedrawMenubarWindow();
   } else if (input->type == EnterNotify) {
      SetMouseStatus("", "", "");
   } else if (input->type == MotionNotify) {
      int index;

      index = WhichMenu(input->xmotion.x, input->xmotion.y, NULL, NULL, NULL);
      if (index == (-1)) {
         SetMouseStatus("(none)", "(none)", "(none)");
      } else if (minimalMenubar) {
         SetMouseStatus(shortMenubarMouseStatus[index].l,
               shortMenubarMouseStatus[index].m,
               shortMenubarMouseStatus[index].r);
      } else {
         SetMouseStatus(menubarMouseStatus[index].l,
               menubarMouseStatus[index].m, menubarMouseStatus[index].r);
      }
      XSync(mainDisplay, False);
      while (XCheckWindowEvent(mainDisplay, menubarWindow, PointerMotionMask,
            &ev)) ;
   } else if (input->type == ButtonPress) {
      int win_x, win_y, index;
      struct BBRec text_bbox;

      index = WhichMenu(input->xbutton.x, input->xbutton.y, &win_x, &win_y,
            &text_bbox);
      if (index == (-1)) {
         SetMouseStatus("(none)", "(none)", "(none)");
      } else {
         SaveStatusStrings();
         rc = PullDownFromMenubar(index, win_x, win_y, &text_bbox);
         RestoreStatusStrings();
         SetMouseStatus(NULL, NULL, NULL);
      }
   }
   return rc;
}
