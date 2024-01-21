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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/mainmenu.c,v 3.0 1996/05/06 16:05:53 william Exp $";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "const.h"
#include "types.h"

#include "align.e"
#include "choice.e"
#include "color.e"
#include "cursor.e"
#include "edit.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "help.e"
#include "imgproc.e"
#include "mainloop.e"
#ifndef _NO_EXTERN
#include "mainmenu.e"
#endif
#include "menu.e"
#include "msg.e"
#include "navigate.e"
#include "page.e"
#include "pattern.e"
#include "raster.e"
#include "setup.e"
#include "shape.e"
#include "special.e"
#include "stretch.e"
#include "text.e"

extern int	atoi ARGS_DECL((char *));

#define DEF_MAINMENUPINDIST 80

int	pinnedMainMenu = FALSE;
int	mainMenuPinDistance = DEF_MAINMENUPINDIST;
int	mainMenuX;
int	mainMenuY;
int	mainMenuW;
int	mainMenuH;
Window	mainMenuWindow;

int	numStacking = 0;
Window	* stackingWins = NULL;

typedef struct SubMenuRec {
   Window	win;
   int		x, y, w, h, extra_index;
} * SubMenuRecPtr;

static struct SubMenuRec	subMenuInfo[MAXMENUS+1];

static int 	savedMainWinX, savedMainWinY;

static
void InitSubMenus ()
{
   register int	i;

   for (i = 0; i < MAXMENUS+1; i++) subMenuInfo[i].win = None;
}

void InitMainMenu ()
{
   register int	i;
   int		menu_w, menu_h, len[MAXMENUS], max_len;
   char		* c_ptr;
   XWMHints	wmhints;
   XSizeHints	sizehints;
   XSetWindowAttributes	win_attrs;

   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"MainMenuPinDistance")) !=
         NULL)
      mainMenuPinDistance = atoi (c_ptr);
   else
      mainMenuPinDistance = DEF_MAINMENUPINDIST;
   pinnedMainMenu = FALSE;

   for (i = 0, max_len = 0; i < MAXMENUS; i++)
   {
      len[i] = strlen (mainMenuStr[i]);
      if (len[i] > max_len) max_len = len[i];
   }
   menu_w = defaultFontWidth * max_len + 1;
   menu_h = defaultFontHeight * MAXMENUS;

   mainMenuX = 0;
   mainMenuY = 0;
   mainMenuW = menu_w+2*brdrW;
   mainMenuH = menu_h+2*brdrW;

   if ((mainMenuWindow = XCreateSimpleWindow (mainDisplay, rootWindow, 0, 0,
         menu_w, menu_h, brdrW, myBorderPixel, myBgPixel)) == 0)
   { fprintf (stderr, "Can not create main menu window!\n"); exit(1); }

   win_attrs.save_under = True;
   win_attrs.override_redirect = True;
   XChangeWindowAttributes (mainDisplay, mainMenuWindow,
         CWSaveUnder | CWOverrideRedirect, &win_attrs);

   XSetTransientForHint (mainDisplay, mainMenuWindow, mainWindow);

   XSelectInput (mainDisplay, mainMenuWindow, StructureNotifyMask |
         ExposureMask | ButtonPressMask | ButtonReleaseMask | KeyPressMask |
         EnterWindowMask | LeaveWindowMask);

   sizehints.flags = PSize | PMinSize | PMaxSize;
   sizehints.width = sizehints.min_width = sizehints.max_width =
         menu_w+2*brdrW;
   sizehints.height = sizehints.min_height = sizehints.max_height =
         menu_h+2*brdrW;

   sizehints.flags |= USPosition | PPosition;
#ifdef NOTR4MODE
   XSetNormalHints (mainDisplay, mainMenuWindow, &sizehints);
#else
   XSetWMNormalHints (mainDisplay, mainMenuWindow, &sizehints);
#endif

   wmhints.flags = InputHint;
   wmhints.input = True;
   XSetWMHints (mainDisplay, mainMenuWindow, &wmhints);

   InitSubMenus ();
}

void CleanUpMainMenu ()
{
   register int	i;

   XDestroyWindow (mainDisplay, mainMenuWindow);

   for (i = 0; i < MAXMENUS+1; i++)
   {
      if (subMenuInfo[i].win != None)
         XDestroyWindow (mainDisplay, subMenuInfo[i].win);
      subMenuInfo[i].win = None;
   }
}

static int	mainMenuMoveDX = 0, mainMenuMoveDY = 0;

void SaveMainWinPosition (x, y)
   unsigned int	x, y;
{
   mainMenuMoveDX = x - savedMainWinX;
   mainMenuMoveDY = y - savedMainWinY;
    
   savedMainWinX = x;
   savedMainWinY = y;
}

void MoveMainMenuWindow (x, y)
   unsigned int	x, y;
{
   mainMenuMoveDX = x - savedMainWinX;
   mainMenuMoveDY = y - savedMainWinY;
    
   if (x == savedMainWinX && y == savedMainWinX) return;

   mainMenuX += mainMenuMoveDX;
   mainMenuY += mainMenuMoveDY;

   XMoveWindow (mainDisplay, mainMenuWindow, mainMenuX, mainMenuY);

   savedMainWinX = x;
   savedMainWinY = y;
}
    
static
void RepositionMainMenuWindow (OrigCursorX, OrigCursorY)
   int	OrigCursorX, OrigCursorY;
{
   int		moving = TRUE, x, y, w, h;
   XEvent	input;

   x = mainMenuX;
   y = mainMenuY;
   w = mainMenuW;
   h = mainMenuH;

   XGrabPointer (mainDisplay, mainMenuWindow, FALSE,
         PointerMotionMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);

   XSetSubwindowMode (mainDisplay, revDefaultGC, IncludeInferiors);
   XDrawRectangle (mainDisplay, rootWindow, revDefaultGC, x, y, w, h);
    
   while (moving)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == ButtonRelease)
      {
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC, x, y, w, h);
         XSetSubwindowMode (mainDisplay, revDefaultGC, ClipByChildren);
         XMoveWindow (mainDisplay, mainMenuWindow, x, y);
         XRaiseWindow (mainDisplay, mainMenuWindow);
         mainMenuX = x;
         mainMenuY = y;

         moving = FALSE;
         XUngrabPointer (mainDisplay, CurrentTime);
         XDefineCursor (mainDisplay, mainMenuWindow, defaultCursor);
      }
      else if (input.type == MotionNotify)
      {
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC, x, y, w, h);
         x = input.xbutton.x_root-OrigCursorX+mainMenuX;
         y = input.xbutton.y_root-OrigCursorY+mainMenuY;
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC, x, y, w, h);
      }
   }
}
    
void RedrawMainMenuWindow ()
{
   register int	i, y;

   XClearWindow (mainDisplay, mainMenuWindow);

   y = defaultFontAsc;
   XSetForeground (mainDisplay, textMenuGC, myFgPixel);
   for (i = 0; i < MAXMENUS; i++, y += defaultFontHeight)
      XDrawString (mainDisplay, mainMenuWindow, textMenuGC, 0, y,
            mainMenuStr[i], strlen(mainMenuStr[i]));
}

int MainMenuEventHandler (input)
   XEvent	* input;
{
   int		x, y, index, rc = INVALID;
   XEvent	ev;
   XButtonEvent	* button_ev;
   
   if (!pinnedMainMenu) return (INVALID);

   if (input->type==MapNotify || input->type==Expose)
   {
      while (XCheckWindowEvent (mainDisplay,mainMenuWindow,ExposureMask,&ev)) ;
      while (XCheckWindowEvent (mainDisplay,mainMenuWindow,StructureNotifyMask,
            &ev)) ;

      RedrawMainMenuWindow ();
   }
   else if (input->type == EnterNotify)
   {
      SetMouseStatus ("Move pinned menu", "Execute Menu Command",
            "Close pinned menu");
   }
   else if (input->type == LeaveNotify)
   {
   }
   else if (input->type == ConfigureNotify)
   {
      mainMenuX = input->xconfigure.x;
      mainMenuY = input->xconfigure.y;
   }
   else if (input->type == ButtonPress)
   {
      button_ev = &(input->xbutton);
      switch (button_ev->button)
      {
         case Button1:
            RepositionMainMenuWindow (button_ev->x_root, button_ev->y_root);
            break;
         case Button2:
            index = button_ev->y/defaultFontHeight;
            x = button_ev->x_root;
            y = button_ev->y_root;
            switch (index) {
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
            break;
      }
   }
   else if (input->type == ButtonRelease)
   {
      button_ev = &(input->xbutton);
      if (button_ev->button == Button3 &&
            button_ev->x >=0 && button_ev->x <=mainMenuW &&
            button_ev->y >=0 && button_ev->y <=mainMenuH)
      {
         XUnmapWindow (mainDisplay, mainMenuWindow);
         pinnedMainMenu = FALSE;
      }
   }
   return (rc);
}

static char	checkExtra[33];

void SaveStackingOrder ()
{
   register int			i, j;
   register unsigned char	hash_value;
   Window			root_win, parent_win, * child_wins;
   unsigned int			num_child;

   for (i=0; i < 33; i++) checkExtra[i] = '\0';
   if (pinnedMainMenu)
   {
      hash_value = mainMenuWindow & 0xff;
      checkExtra[hash_value>>3] |= (1<<(hash_value&0x7));
   }
   for (i = 0; i < numExtraWins; i++) {
      if (extraWinInfo[i].raise && extraWinInfo[i].mapped &&
            extraWinInfo[i].window != None) {
         hash_value = extraWinInfo[i].window & 0xff;
         checkExtra[hash_value>>3] |= (1<<(hash_value&0x7));
      }
   }

   XQueryTree(mainDisplay, rootWindow, &root_win, &parent_win, &child_wins,
         &num_child);

   numStacking = 0;
   if (stackingWins != NULL) free(stackingWins);
   stackingWins = (Window*)malloc((numExtraWins+1)*sizeof(Window));
   if (stackingWins == NULL) FailAllocMessage();

   for (i = 0; i < num_child; i++) {
      hash_value = child_wins[i] & 0xff;
      if (checkExtra[hash_value>>3] & (1<<(hash_value&0x7))) {
         if (child_wins[i] == mainMenuWindow) {
            stackingWins[numStacking++] = mainMenuWindow;
         } else {
            for (j = 0; j < numExtraWins; j++) {
               if (extraWinInfo[j].raise && extraWinInfo[j].mapped &&
                     extraWinInfo[j].window == child_wins[i]) {
                  stackingWins[numStacking++] = child_wins[i];
                  break;
               }
            }
         }
      }
   }
   if (child_wins != NULL) XFree ((void *)child_wins);
}

void MoveSubMenuWindow (win)
   Window	win;
{
   register int	i;

   for (i = 0; i < MAXMENUS; i++)
      if (subMenuInfo[i].win == win)
         break;
   if (i == MAXMENUS) return;
    
   subMenuInfo[i].x += mainMenuMoveDX;
   subMenuInfo[i].y += mainMenuMoveDY;

   XMoveWindow (mainDisplay, win, subMenuInfo[i].x, subMenuInfo[i].y);
}
    
static
void RepositionSubMenuWindow (win, menu_index, OrigCursorX, OrigCursorY)
   Window	win;
   int		menu_index, OrigCursorX, OrigCursorY;
{
   int		moving = TRUE, x, y, w, h;
   XEvent	input;

   x = subMenuInfo[menu_index].x;
   y = subMenuInfo[menu_index].y;
   w = subMenuInfo[menu_index].w;
   h = subMenuInfo[menu_index].h;

   XGrabPointer (mainDisplay, win, FALSE, PointerMotionMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);

   XSetSubwindowMode (mainDisplay, revDefaultGC, IncludeInferiors);
   XDrawRectangle (mainDisplay, rootWindow, revDefaultGC, x, y, w, h);
    
   while (moving)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == ButtonRelease)
      {
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC, x, y, w, h);
         XSetSubwindowMode (mainDisplay, revDefaultGC, ClipByChildren);
         XMoveWindow (mainDisplay, win, x, y);
         XRaiseWindow (mainDisplay, win);
         subMenuInfo[menu_index].x = x;
         subMenuInfo[menu_index].y = y;

         moving = FALSE;
         XUngrabPointer (mainDisplay, CurrentTime);
         XDefineCursor (mainDisplay, win, defaultCursor);
      }
      else if (input.type == MotionNotify)
      {
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC, x, y, w, h);
         x = input.xbutton.x_root-OrigCursorX+subMenuInfo[menu_index].x;
         y = input.xbutton.y_root-OrigCursorY+subMenuInfo[menu_index].y;
         XDrawRectangle (mainDisplay, rootWindow, revDefaultGC, x, y, w, h);
      }
   }
}

static
void PaintTextSubMenu (window,win_w,Strings,Entries,ForeColors,Valid,InitRV)
   Window	window;
   int		win_w, Entries, * ForeColors, * Valid, * InitRV;
   char		* Strings[];
{
   register int	i, y;
   int		pixel;

   XClearWindow (mainDisplay, window);
   y = defaultFontAsc;
   XSetForeground (mainDisplay, textMenuGC, ForeColors[0]);
   pixel = ForeColors[0];
   for (i = 0; i < Entries; i++, y += defaultFontHeight)
   {
      if (InitRV[i])
      {
         XSetForeground (mainDisplay, textMenuGC, ForeColors[i]);
         XFillRectangle (mainDisplay, window, textMenuGC, 0,
               i*defaultFontHeight, win_w, defaultFontHeight);
         XSetForeground (mainDisplay, textMenuGC, myBgPixel);
         pixel = myBgPixel;
         XDrawString (mainDisplay, window, textMenuGC, 0,
               defaultFontAsc+i*defaultFontHeight, Strings[i],
               strlen(Strings[i]));
      }
      else
      {
         if (pixel != ForeColors[i])
         {
            XSetForeground (mainDisplay, textMenuGC, ForeColors[i]);
            pixel = ForeColors[i];
         }
         XDrawString (mainDisplay, window, textMenuGC, 0, y, Strings[i],
               strlen(Strings[i]));
      }
   }
   free(ForeColors);
   free(Valid);
   free(InitRV);
}

static
void PaintPxMpSubMenu (window, W, H, Rows, Cols, Entries, ForeColors, PxMp,
      InitRV, MultiColor)
   Window	window;
   int		W, H, Rows, Cols, Entries, * ForeColors, * InitRV, MultiColor;
   Pixmap	PxMp[];
{
   register int	i, j;
   int		k;
   XGCValues	values;

   XClearWindow (mainDisplay, window);
   for (i = 0; i < Rows; i++)
      for (j = 0; j < Cols; j++)
      {
         k = i + j * Rows;
         if (k >= Entries) break;
         if (MultiColor)
         {
            values.foreground = ForeColors[k];
            values.stipple = patPixmap[SOLIDPAT];
            XChangeGC (mainDisplay, rasterGC,
                  GCForeground | GCStipple, &values);
            XFillRectangle (mainDisplay, window, rasterGC, j*W, i*H, W, H);
         }
         else
         {
            if (InitRV[k])
            {
               XSetForeground (mainDisplay, textMenuGC, myFgPixel);
               XFillRectangle (mainDisplay, window, textMenuGC, j*W, i*H, W, H);
               XSetStipple (mainDisplay, rvPixmapMenuGC, PxMp[k]);
               XFillRectangle (mainDisplay, window, rvPixmapMenuGC, j*W, i*H,
                  W, H);
            }
            else
            {
               XSetStipple (mainDisplay, rasterGC, PxMp[k]);
               XFillRectangle (mainDisplay, window, rasterGC, j*W, i*H, W, H);
            }
         }
      }
   XSetForeground (mainDisplay, rasterGC, myFgPixel);

   free(ForeColors);
   free(InitRV);
}

static
void RedrawSubMenu (menu_index)
   int	menu_index;
{
   char		* layout_menu_strings[MAXLAYOUTMENUS];
   char		* layout_menu_description[MAXLAYOUTMENUS];
   int		* fore_colors, * valid, * init_rv, rows, cols, index;
   Pixmap	*pixmap;
   Window	win=subMenuInfo[menu_index].win;

   switch (menu_index)
   {
      case MENU_MODE:
         DefaultColorArrays (MAXCHOICES, &fore_colors, &valid, &init_rv, NULL);
         free(valid);
         init_rv[curChoice] = TRUE;
         PaintPxMpSubMenu (win, choiceImageW, choiceImageH, MAXCHOICES, 1,
               MAXCHOICES, fore_colors, choicePixmap, init_rv, SINGLECOLOR);
         break;
      case MENU_FILE:
         DefaultColorArrays (FILEMENUENTRIES, &fore_colors, &valid, &init_rv,
               NULL);
         PaintTextSubMenu (win, subMenuInfo[menu_index].w, fileMenuStr,
               FILEMENUENTRIES, fore_colors, valid, init_rv);
         break;
      case MENU_EDIT:
         DefaultColorArrays (MAXEDITMENUS, &fore_colors, &valid, &init_rv,
               NULL);
         PaintTextSubMenu (win, subMenuInfo[menu_index].w, editMenuStr,
               MAXEDITMENUS, fore_colors, valid, init_rv);
         break;
      case MENU_STYLE:
         DefaultColorArrays (MAXFONTSTYLES+MAXJUSTS+1, &fore_colors, &valid,
               &init_rv, NULL);
         init_rv[curStyle] = TRUE;
         init_rv[MAXFONTSTYLES+1+textJust] = TRUE;
         PaintTextSubMenu (win, subMenuInfo[menu_index].w, styleMenuStr,
               MAXFONTSTYLES+MAXJUSTS+1, fore_colors, valid, init_rv);
         break;
      case MENU_SIZE:
         DefaultColorArrays (numFontSizes, &fore_colors, &valid, &init_rv,
               NULL);
         index = GetSizeMenuIndex ();
         if (index != INVALID) init_rv[index] = TRUE;
         PaintTextSubMenu (win, subMenuInfo[menu_index].w, sizeMenuStr,
               numFontSizes, fore_colors, valid, init_rv);
         break;
      case MENU_FONT:
         DefaultColorArrays (numFonts, &fore_colors, &valid, &init_rv, NULL);
         init_rv[curFont] = TRUE;
         PaintTextSubMenu (win, subMenuInfo[menu_index].w, fontMenuStr,
               numFonts, fore_colors, valid, init_rv);
         break;
      case MENU_LAYOUT:
         SetUpLayoutMenuStr (layout_menu_strings, layout_menu_description);
         DefaultColorArrays (MAXLAYOUTMENUS, &fore_colors, &valid, &init_rv,
               NULL);
         switch (pageStyle)
         {
            case PORTRAIT: init_rv[LAYOUT_PORT] = TRUE; break;
            case LANDSCAPE: init_rv[LAYOUT_LAND] = TRUE; break;
         }
         PaintTextSubMenu (win, subMenuInfo[menu_index].w, layout_menu_strings,
               MAXLAYOUTMENUS, fore_colors, valid, init_rv);
         free(layout_menu_strings[0]);
         free(layout_menu_description[0]);
         break;
      case MENU_ARRANGE:
         DefaultColorArrays (MAXARRANGEMENUS, &fore_colors, &valid, &init_rv,
               NULL);
         PaintTextSubMenu (win, subMenuInfo[menu_index].w, arrangeMenuStr,
               MAXARRANGEMENUS, fore_colors, valid, init_rv);
         break;
      case MENU_MOVEMODE:
         DefaultColorArrays (MAXMOVEMODES, &fore_colors, &valid, &init_rv,
               NULL);
         free(valid);
         init_rv[moveMode] = TRUE;
         PaintPxMpSubMenu (win, choiceImageW, choiceImageH, MAXMOVEMODES, 1,
               MAXMOVEMODES, fore_colors, moveModePixmap, init_rv, SINGLECOLOR);
         break;
      case MENU_HORIALIGN:
         DefaultColorArrays (MAXALIGNS, &fore_colors, &valid, &init_rv, NULL);
         free(valid);
         init_rv[horiAlign] = TRUE;
         PaintPxMpSubMenu (win, choiceImageW, choiceImageH, MAXALIGNS, 1,
               MAXALIGNS, fore_colors, alignHoriPixmap, init_rv, SINGLECOLOR);
         break;
      case MENU_VERTALIGN:
         DefaultColorArrays (MAXALIGNS, &fore_colors, &valid, &init_rv, NULL);
         free(valid);
         init_rv[vertAlign] = TRUE;
         PaintPxMpSubMenu (win, choiceImageW, choiceImageH, MAXALIGNS, 1,
               MAXALIGNS, fore_colors, alignVertPixmap, init_rv, SINGLECOLOR);
         break;
      case MENU_FILL:
         DefaultColorArrays (MAXPATTERNS, &fore_colors, &valid, &init_rv, NULL);
         free(valid);
         PaintPxMpSubMenu (win, choiceImageW, choiceImageH, 8, 4,
               MAXPATTERNS, fore_colors, patPixmap, init_rv, SINGLECOLOR);
         break;
      case MENU_SHAPE:
         DefaultColorArrays (numShapes, &fore_colors, &valid, &init_rv, NULL);
         free(valid);
         PaintPxMpSubMenu (win, choiceImageW, choiceImageH, 8, 3,
               numShapes, fore_colors, shapePixmap, init_rv, SINGLECOLOR);
         break;
      case MENU_STRETCHTEXT:
         DefaultColorArrays (MAXSTRETCHABLEMODES, &fore_colors, &valid,
               &init_rv, NULL);
         free(valid);
         init_rv[stretchableText] = TRUE;
         PaintPxMpSubMenu (win, choiceImageW, choiceImageH,
               MAXSTRETCHABLEMODES, 1, MAXSTRETCHABLEMODES, fore_colors,
               stretchableModePixmap, init_rv, SINGLECOLOR);
         break;
      case MENU_LINEWIDTH:
         DefaultColorArrays (maxLineWidths, &fore_colors, &valid, &init_rv,
               NULL);
         free(valid);
         init_rv[lineWidth] = TRUE;
         PaintPxMpSubMenu (win, menuImageW, menuImageH, maxLineWidths, 1,
               maxLineWidths, fore_colors, lineWidthPixmap, init_rv,
               SINGLECOLOR);
         break;
      case MENU_LINESTYLE:
         DefaultColorArrays (MAXLINESTYLES, &fore_colors, &valid, &init_rv,
               NULL);
         free(valid);
         init_rv[lineStyle] = TRUE;
         PaintPxMpSubMenu (win, menuImageW, menuImageH, MAXLINESTYLES, 1,
               MAXLINESTYLES, fore_colors, lineStylePixmap, init_rv,
               SINGLECOLOR);
         break;
      case MENU_LINETYPE:
         DefaultColorArrays (MAXLINETYPES, &fore_colors, &valid, &init_rv,
               NULL);
         free(valid);
         init_rv[curSpline] = TRUE;
         PaintPxMpSubMenu (win, menuImageW, menuImageH, MAXLINETYPES, 1,
               MAXLINETYPES, fore_colors, lineTypePixmap, init_rv,
               SINGLECOLOR);
         break;
      case MENU_LINEDASH:
         DefaultColorArrays (MAXDASHES, &fore_colors, &valid, &init_rv, NULL);
         free(valid);
         init_rv[curDash] = TRUE;
         PaintPxMpSubMenu (win, menuImageW, menuImageH, MAXDASHES, 1,
               MAXDASHES, fore_colors, dashPixmap, init_rv, SINGLECOLOR);
         break;
      case MENU_PEN:
         DefaultColorArrays (MAXPATTERNS, &fore_colors, &valid, &init_rv, NULL);
         free(valid);
         PaintPxMpSubMenu (win, choiceImageW, choiceImageH, 8, 4,
               MAXPATTERNS, fore_colors, patPixmap, init_rv, SINGLECOLOR);
         break;
      case MENU_PAGE:
         switch (pageLayoutMode)
         {
            case PAGE_STACK:
               DefaultColorArrays (MAXPAGESTACKMENUS, &fore_colors, &valid,
                     &init_rv, NULL);
               PaintTextSubMenu (win, subMenuInfo[menu_index].w,
                     pageStackMenuStr, MAXPAGESTACKMENUS, fore_colors, valid,
                     init_rv);
               break;
            case PAGE_TILE:
               DefaultColorArrays (MAXPAGETILEMENUS, &fore_colors, &valid,
                     &init_rv, NULL);
               PaintTextSubMenu (win, subMenuInfo[menu_index].w,
                     pageTileMenuStr, MAXPAGETILEMENUS, fore_colors, valid,
                     init_rv);
               break;
         }
         break;
      case MENU_PAGELAYOUT:
         DefaultColorArrays (MAXPAGELAYOUTMODES, &fore_colors, &valid, &init_rv,
               NULL);
         free(valid);
         init_rv[pageLayoutMode] = TRUE;
         PaintPxMpSubMenu (win, choiceImageW, choiceImageH, MAXPAGELAYOUTMODES,
               1, MAXPAGELAYOUTMODES, fore_colors, pageLayoutPixmap, init_rv,
               SINGLECOLOR);
         break;
      case MENU_COLOR:
         SetUpColorMenuPixmap (&fore_colors, &init_rv, &pixmap, &rows, &cols);
         PaintPxMpSubMenu (win, choiceImageW, choiceImageH, rows, cols,
               maxColors, fore_colors, pixmap, init_rv, MULTICOLOR);
         free(pixmap);
         break;
      case MENU_IMAGEPROC:
         DefaultColorArrays (numImageProc, &fore_colors, &valid, &init_rv,
               NULL);
         PaintTextSubMenu (win, subMenuInfo[menu_index].w, imageProcMenuStr,
               numImageProc, fore_colors, valid, init_rv);
         break;
      case MENU_NAVIGATE:
         DefaultColorArrays (MAXNAVIGATEMENUS, &fore_colors, &valid, &init_rv,
               NULL);
         PaintTextSubMenu (win, subMenuInfo[menu_index].w,
               inHyperSpace ? navigateMenuStrInHyperSpace : navigateMenuStr,
               MAXNAVIGATEMENUS, fore_colors, valid, init_rv);
         break;
      case MENU_SPECIAL:
         DefaultColorArrays (MAXSPECIALMENUS, &fore_colors, &valid, &init_rv,
               NULL);
         PaintTextSubMenu (win, subMenuInfo[menu_index].w, specialMenuStr,
               MAXSPECIALMENUS, fore_colors, valid, init_rv);
         break;
      case MENU_HELP:
         DefaultColorArrays (numHelp, &fore_colors, &valid, &init_rv, NULL);
         PaintTextSubMenu (win, subMenuInfo[menu_index].w, helpMenuStr,
               numHelp, fore_colors, valid, init_rv);
         break;
   }
}
    
static
void SubMenuExposeHandler (input)
   XEvent	* input;
{
   register int	menu_index;
   Window	win;
   XEvent	ev;

   win = input->xany.window;
   while (XCheckWindowEvent (mainDisplay, win, ExposureMask, &ev));
   while (XCheckWindowEvent (mainDisplay, win, StructureNotifyMask, &ev));

   for (menu_index = 0; menu_index < MAXMENUS; menu_index++)
      if (subMenuInfo[menu_index].win == win && win != None)
         break;

   if (menu_index != INVALID) RedrawSubMenu (menu_index);
}

static
void GetRowCol (menu_index, x, y, row, col, unit_w, unit_h, num_rows)
   int	menu_index, x, y, * row, * col, * unit_w, * unit_h, * num_rows;
{
   switch (menu_index) {
   case MENU_FILE:
   case MENU_EDIT:
   case MENU_STYLE:
   case MENU_SIZE:
   case MENU_FONT:
   case MENU_LAYOUT:
   case MENU_ARRANGE:
   case MENU_IMAGEPROC:
   case MENU_NAVIGATE:
   case MENU_SPECIAL:
   case MENU_PAGE:
   case MENU_HELP:
      *unit_w = subMenuInfo[menu_index].w;
      *unit_h = defaultFontHeight;
      break;
   case MENU_LINEWIDTH:
   case MENU_LINESTYLE:
   case MENU_LINETYPE:
   case MENU_LINEDASH:
      *unit_w = subMenuInfo[menu_index].w;
      *unit_h = menuImageH;
      break;
   case MENU_MODE:
   case MENU_MOVEMODE:
   case MENU_HORIALIGN:
   case MENU_VERTALIGN:
   case MENU_SHAPE:
   case MENU_STRETCHTEXT:
   case MENU_COLOR:
   case MENU_FILL:
   case MENU_PEN:
   case MENU_PAGELAYOUT:
      *unit_w = choiceImageW;
      *unit_h = choiceImageH;
      break;
   }
   *row = (y>=0 && y<=subMenuInfo[menu_index].h) ? y/(*unit_h) : INVALID;
   *col = (x>=0 && x<=subMenuInfo[menu_index].w) ? x/(*unit_w) : INVALID;
   *num_rows = subMenuInfo[menu_index].h / (*unit_h);
}

static
void ComputeSubMenuWinXY (win, x, y)
   Window	win;
   int		* x, * y;
{
   int		win_x, win_y, done=FALSE;
   unsigned int	win_w, win_h, win_brdr_w, win_d, num_child;
   Window	root_win, parent_win, * child_wins;

   *x = *y = 0;
   while (!done)
   {
      XGetGeometry (mainDisplay, win, &root_win, &win_x, &win_y, &win_w,
            &win_h, &win_brdr_w, &win_d);
      *x += win_x;
      *y += win_y;
      if (XQueryTree (mainDisplay, win, &root_win, &parent_win, &child_wins,
            &num_child) == 0)
         return;
      if (child_wins != NULL) XFree ((void *)child_wins);
      if (parent_win == rootWindow)
         done = TRUE;
      else
         win = parent_win;
   }
}

static
int SubMenuEventHandler(input)
   XEvent *input;
{
   register int menu_index;
   int done, down_row, down_col, up_row, up_col, unit_w, unit_h, num_rows;
   XButtonEvent *button_ev;
   Window win;

   if (input->type==MapNotify || input->type==Expose) {
      SubMenuExposeHandler(input);
      return INVALID;
   }
   win = input->xany.window;
   for (menu_index = 0; menu_index < MAXMENUS; menu_index++) {
      if (subMenuInfo[menu_index].win == win && win != None) {
         break;
      }
   }
   if (menu_index == MAXMENUS) return INVALID;

   if (input->type == ConfigureNotify) {
      ComputeSubMenuWinXY(win, &(subMenuInfo[menu_index].x),
            &(subMenuInfo[menu_index].y));
   } else if (input->type == EnterNotify) {
      SetMouseStatus("Move pinned menu", "Execute Menu Command",
            "Close pinned menu");
   } else if (input->type == LeaveNotify) {
   } else if (input->type == ButtonPress) {
      button_ev = &(input->xbutton);
      switch (button_ev->button) {
      case Button1:
         RepositionSubMenuWindow(win, menu_index, button_ev->x_root,
               button_ev->y_root);
         break;
      case Button2:
         GetRowCol(menu_index, button_ev->x, button_ev->y, &down_row,
               &down_col, &unit_w, &unit_h, &num_rows);
         XRaiseWindow(mainDisplay, win);
         XFillRectangle(mainDisplay, win, revDefaultGC, down_col*unit_w,
               down_row*unit_h, unit_w, unit_h);

         done = FALSE;
         while (!done) {
            XEvent ev;

            XNextEvent(mainDisplay, &ev);

            if (ev.type == Expose || ev.type == VisibilityNotify) {
               ExposeEventHandler(&ev, TRUE);
            } else if (ev.type == ButtonRelease) {
               done = TRUE;
               XFillRectangle(mainDisplay, win, revDefaultGC,
                     down_col*unit_w, down_row*unit_h, unit_w, unit_h);

               GetRowCol(menu_index, ev.xbutton.x, ev.xbutton.y,
                     &up_row, &up_col, &unit_w, &unit_h, &num_rows);

               if (up_row!=INVALID && up_col!=INVALID &&
                     up_row==down_row && up_col==down_col) {
                  switch (menu_index) {
                  case MENU_MODE: SetCurChoice(up_row); break;
                  case MENU_FILE: return FileSubMenu(up_row);
                  case MENU_EDIT: EditSubMenu(up_row); break;
                  case MENU_STYLE: StyleSubMenu(up_row); break;
                  case MENU_SIZE: ChangeFontSize(up_row); break;
                  case MENU_FONT: ChangeFont(up_row); break;
                  case MENU_LAYOUT: LayoutSubMenu(up_row); break;
                  case MENU_ARRANGE: ArrangeSubMenu(up_row); break;
                  case MENU_MOVEMODE: MoveModeSubMenu(up_row); break;
                  case MENU_HORIALIGN: HoriAlignSubMenu(up_row); break;
                  case MENU_VERTALIGN: VertAlignSubMenu(up_row); break;
                  case MENU_SHAPE: ShapeSubMenu(up_col*num_rows+up_row); break;
                  case MENU_STRETCHTEXT:
                     StretchableTextModeSubMenu(up_row);
                     break;
                  case MENU_FILL:
                     ChangeAllSelFill(up_col*num_rows+up_row, TRUE);
                     break;
                  case MENU_LINEWIDTH:
                     ChangeAllSelLineWidth(up_row, TRUE); break;
                  case MENU_LINESTYLE:
                     ChangeAllSelLineStyle(up_row, TRUE); break;
                  case MENU_LINETYPE:
                     ChangeAllSelLineType(up_row, TRUE); break;
                  case MENU_LINEDASH:
                     ChangeAllSelDashes(up_row, TRUE); break;
                  case MENU_PEN:
                     ChangeAllSelPen(up_col*num_rows+up_row, TRUE);
                     break;
                  case MENU_COLOR:
                     ChangeAllSelColor(up_col*num_rows+up_row, TRUE);
                     break;
                  case MENU_IMAGEPROC: ImageProcSubMenu(up_row); break;
                  case MENU_NAVIGATE: NavigateSubMenu(up_row); break;
                  case MENU_SPECIAL: SpecialSubMenu(up_row); break;
                  case MENU_PAGE: PageSubMenu(up_row); break;
                  case MENU_PAGELAYOUT: PageLayoutSubMenu(up_row); break;
                  case MENU_HELP: HelpSubMenu(up_row); break;
                  }
               }
            }
         }
         break;
      }
   } else if (input->type == ButtonRelease) {
      button_ev = &(input->xbutton);
      if (button_ev->button == Button3 &&
            button_ev->x>=0 && button_ev->x<=subMenuInfo[menu_index].w &&
            button_ev->y>=0 && button_ev->y<=subMenuInfo[menu_index].h) {
         XDestroyWindow(mainDisplay, subMenuInfo[menu_index].win);
         subMenuInfo[menu_index].win = None;
         extraWinInfo[subMenuInfo[menu_index].extra_index].window = None;
      }
   }
   return INVALID;
}

static
void SubMenuCleanUp ()
{
}

void RealizeSubMenuWindow (win_x, win_y, win_w, win_h)
   int		win_x, win_y, win_w, win_h;
{
   Window	win;
   XWMHints	wmhints;
   XSizeHints	sizehints;
   XSetWindowAttributes	win_attrs;

   if ((win = XCreateSimpleWindow (mainDisplay, rootWindow, win_x, win_y,
         win_w-2*brdrW, win_h-2*brdrW, brdrW, myBorderPixel, myBgPixel)) == 0)
      Error ("InitSubMenus()", "Can not XCreateSimpleWindow()");

   XDefineCursor (mainDisplay, win, defaultCursor);

   win_attrs.save_under = True;
   win_attrs.override_redirect = True;
   win_attrs.colormap = mainColormap;
   XChangeWindowAttributes (mainDisplay, win,
         CWSaveUnder | CWOverrideRedirect | CWColormap, &win_attrs);

   XSetTransientForHint (mainDisplay, win, mainWindow);

   sizehints.flags = PSize | PMinSize | PMaxSize;
   sizehints.width = sizehints.min_width = sizehints.max_width = win_w;
   sizehints.height = sizehints.min_height = sizehints.max_height = win_h;

   sizehints.flags |= USPosition | PPosition;
#ifdef NOTR4MODE
   XSetNormalHints (mainDisplay, win, &sizehints);
#else
   XSetWMNormalHints (mainDisplay, win, &sizehints);
#endif

   wmhints.flags = InputHint;
   wmhints.input = True;
   XSetWMHints (mainDisplay, win, &wmhints);

#ifdef MAPBEFORESELECT
   XMapWindow (mainDisplay, win);
   XSelectInput (mainDisplay, win, StructureNotifyMask |
         ExposureMask | ButtonPressMask | ButtonReleaseMask | KeyPressMask |
         EnterWindowMask | LeaveWindowMask);
#else
   XSelectInput (mainDisplay, win, StructureNotifyMask |
         ExposureMask | ButtonPressMask | ButtonReleaseMask | KeyPressMask |
         EnterWindowMask | LeaveWindowMask);
   XMapWindow (mainDisplay, win);
#endif

   if (subMenuInfo[activeMenu].win != None)
   {
      XDestroyWindow (mainDisplay, subMenuInfo[activeMenu].win);
      extraWinInfo[subMenuInfo[activeMenu].extra_index].window = None;
   }

   subMenuInfo[activeMenu].extra_index =
         AddExtraWinInfo (win, TRUE, TRUE, SubMenuExposeHandler,
         SubMenuEventHandler, SubMenuCleanUp);

   subMenuInfo[activeMenu].x = win_x;
   subMenuInfo[activeMenu].y = win_y;
   subMenuInfo[activeMenu].w = win_w;
   subMenuInfo[activeMenu].h = win_h;
   subMenuInfo[activeMenu].win = win;
}

int HandleSubMenuEvent(win, menu_index, input)
   Window win;
   int menu_index;
   XEvent *input;
{
   if (subMenuInfo[menu_index].win != None &&
         subMenuInfo[menu_index].win == win) {
      return (*(extraWinInfo[subMenuInfo[menu_index].extra_index].ev_handler))(
            input);
   }
   return INVALID;
}

int WindowIsSubMenu(win, menu_index)
   Window win;
   int menu_index;
{
   return (subMenuInfo[menu_index].win != None &&
         subMenuInfo[menu_index].win == win);
}

void UpdateSubMenu(menu_index)
   int menu_index;
{
   if (subMenuInfo[menu_index].win != None) RedrawSubMenu(menu_index);
}

void UpdateAllSubMenus()
{
   UpdateSubMenu(MENU_HORIALIGN);
   UpdateSubMenu(MENU_VERTALIGN);
   UpdateSubMenu(MENU_STYLE);
   UpdateSubMenu(MENU_FONT);
   UpdateSubMenu(MENU_SIZE);
   UpdateSubMenu(MENU_LINEWIDTH);
   UpdateSubMenu(MENU_LINESTYLE);
   UpdateSubMenu(MENU_LINETYPE);
   UpdateSubMenu(MENU_LINEDASH);
   UpdateSubMenu(MENU_LAYOUT);
   UpdateSubMenu(MENU_MOVEMODE);
   UpdateSubMenu(MENU_STRETCHTEXT);
   UpdateSubMenu(MENU_PAGE);
   UpdateSubMenu(MENU_PAGELAYOUT);
}

void DestroySubMenu(menu_index)
   int menu_index;
{
   if (subMenuInfo[menu_index].win != None) {
      XDestroyWindow(mainDisplay, subMenuInfo[menu_index].win);
      subMenuInfo[menu_index].win = None;
      extraWinInfo[subMenuInfo[menu_index].extra_index].window = None;
   }
}
