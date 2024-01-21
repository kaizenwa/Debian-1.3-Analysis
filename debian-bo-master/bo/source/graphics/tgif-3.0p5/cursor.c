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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/cursor.c,v 3.0 1996/05/06 16:04:20 william Exp $";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include "const.h"
#include "types.h"

#include "choice.e"
#include "color.e"
#ifndef _NO_EXTERN
#include "cursor.e"
#endif
#include "setup.e"
#include "xbitmap.e"

#include "xbm/null.xbm"
#include "xbm/nullmask.xbm"
#include "xbm/text_cur.xbm"
#include "xbm/helphand.xbm"
#include "xbm/hhand_mk.xbm"
#include "xbm/mag.xbm"
#include "xbm/mag_mask.xbm"
#include "xbm/flood.xbm"
#include "xbm/flood_mk.xbm"
#include "xbm/drip.xbm"
#include "xbm/drip_msk.xbm"

Cursor nullCursor=(Cursor)0;
Cursor cornerCursor=(Cursor)0;
Cursor handCursor=(Cursor)0;
Cursor defaultCursor=(Cursor)0;
Cursor watchCursor=(Cursor)0;
Cursor drawCursor=(Cursor)0;
Cursor vertexCursor=(Cursor)0;
Cursor rotateCursor=(Cursor)0;
Cursor rotatingCursor=(Cursor)0;
Cursor horiShearCursor=(Cursor)0;
Cursor vertShearCursor=(Cursor)0;
Cursor moveCursor=(Cursor)0;
Cursor textCursor=(Cursor)0;
Cursor hyperSpaceCursor=(Cursor)0;
Cursor magCursor=(Cursor)0;
Cursor floodCursor=(Cursor)0;
Cursor dripCursor=(Cursor)0;

int watchCursorOnMainWindow=FALSE;

static GC textCursorGC=NULL;
static Pixmap textPixmap=None;
static Pixmap nullPixmap=None;
static Pixmap nullMaskPixmap=None;
static Pixmap hyperSpacePixmap=None;
static Pixmap hyperSpaceMaskPixmap=None;
static Pixmap magPixmap=None;
static Pixmap magMaskPixmap=None;
static Pixmap floodPixmap=None;
static Pixmap floodMaskPixmap=None;
static Pixmap dripPixmap=None;
static Pixmap dripMaskPixmap=None;

static char *cursorName[] = {
   "X_cursor",
   "arrow",
   "based_arrow_down",
   "based_arrow_up",
   "boat",
   "bogosity",
   "bottom_left_corner",
   "bottom_right_corner",
   "bottom_side",
   "bottom_tee",
   "box_spiral",
   "center_ptr",
   "circle",
   "clock",
   "coffee_mug",
   "cross",
   "cross_reverse",
   "crosshair",
   "diamond_cross",
   "dot",
   "dotbox",
   "double_arrow",
   "draft_large",
   "draft_small",
   "draped_box",
   "exchange",
   "fleur",
   "gobbler",
   "gumby",
   "hand1",
   "hand2",
   "heart",
   "icon",
   "iron_cross",
   "left_ptr",
   "left_side",
   "left_tee",
   "leftbutton",
   "ll_angle",
   "lr_angle",
   "man",
   "middlebutton",
   "mouse",
   "pencil",
   "pirate",
   "plus",
   "question_arrow",
   "right_ptr",
   "right_side",
   "right_tee",
   "rightbutton",
   "rtl_logo",
   "sailboat",
   "sb_down_arrow",
   "sb_h_double_arrow",
   "sb_left_arrow",
   "sb_right_arrow",
   "sb_up_arrow",
   "sb_v_double_arrow",
   "shuttle",
   "sizing",
   "spider",
   "spraycan",
   "star",
   "target",
   "tcross",
   "top_left_arrow",
   "top_left_corner",
   "top_right_corner",
   "top_side",
   "top_tee",
   "trek",
   "ul_angle",
   "umbrella",
   "ur_angle",
   "watch",
   "xterm",
   ""
};

static unsigned int cursorID[] = {
   XC_X_cursor,
   XC_arrow,
   XC_based_arrow_down,
   XC_based_arrow_up,
   XC_boat,
   XC_bogosity,
   XC_bottom_left_corner,
   XC_bottom_right_corner,
   XC_bottom_side,
   XC_bottom_tee,
   XC_box_spiral,
   XC_center_ptr,
   XC_circle,
   XC_clock,
   XC_coffee_mug,
   XC_cross,
   XC_cross_reverse,
   XC_crosshair,
   XC_diamond_cross,
   XC_dot,
   XC_dotbox,
   XC_double_arrow,
   XC_draft_large,
   XC_draft_small,
   XC_draped_box,
   XC_exchange,
   XC_fleur,
   XC_gobbler,
   XC_gumby,
   XC_hand1,
   XC_hand2,
   XC_heart,
   XC_icon,
   XC_iron_cross,
   XC_left_ptr,
   XC_left_side,
   XC_left_tee,
   XC_leftbutton,
   XC_ll_angle,
   XC_lr_angle,
   XC_man,
   XC_middlebutton,
   XC_mouse,
   XC_pencil,
   XC_pirate,
   XC_plus,
   XC_question_arrow,
   XC_right_ptr,
   XC_right_side,
   XC_right_tee,
   XC_rightbutton,
   XC_rtl_logo,
   XC_sailboat,
   XC_sb_down_arrow,
   XC_sb_h_double_arrow,
   XC_sb_left_arrow,
   XC_sb_right_arrow,
   XC_sb_up_arrow,
   XC_sb_v_double_arrow,
   XC_shuttle,
   XC_sizing,
   XC_spider,
   XC_spraycan,
   XC_star,
   XC_target,
   XC_tcross,
   XC_top_left_arrow,
   XC_top_left_corner,
   XC_top_right_corner,
   XC_top_side,
   XC_top_tee,
   XC_trek,
   XC_ul_angle,
   XC_umbrella,
   XC_ur_angle,
   XC_watch,
   XC_xterm
};

void SetTextCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, textCursor);
}

void SetNullCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, nullCursor);
}

void SetWatchCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, watchCursor);
   if (window == mainWindow) watchCursorOnMainWindow = TRUE;
   XSync(mainDisplay, False);
}

void SetDrawCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, drawCursor);
}

void SetVertexCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, vertexCursor);
}

void SetRotateCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, rotateCursor);
}

void SetRotatingCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, rotatingCursor);
}

void SetHoriShearCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, horiShearCursor);
}

void SetVertShearCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, vertShearCursor);
}

void SetMoveCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, moveCursor);
}

void SetHyperSpaceCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, hyperSpaceCursor);
}

void SetFloodFillCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, floodCursor);
}

void SetDripCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, dripCursor);
}

void SetDefaultCursor(window)
   Window window;
{
   XDefineCursor(mainDisplay, window, defaultCursor);
   if (window == mainWindow && watchCursorOnMainWindow) {
      watchCursorOnMainWindow = FALSE;
   }
}

void ShowCursor()
{
   if (curChoice == DRAWTEXT) {
      SetTextCursor(drawWindow);
   } else if (curChoice == NOTHING) {
      SetDefaultCursor(drawWindow);
   } else if (curChoice == VERTEXMODE) {
      SetVertexCursor(drawWindow);
   } else if (curChoice == ROTATEMODE) {
      SetRotateCursor(drawWindow);
   } else {
      SetDrawCursor(drawWindow);
   }
}

void CreateCursor()
{
   int i, j;
   XGCValues values;
   XColor color, fg_color, bg_color;
   char *c_ptr;
   int default_cursor_id=XC_arrow, rotate_shear_cursor_id;
   int draw_cursor_id, drag_cursor_id, vertex_cursor_id;

   textPixmap = XCreateBitmapFromData(mainDisplay, mainWindow,
         text_cur_bits, text_cur_width, text_cur_height);

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.fill_style = FillOpaqueStippled;
   values.stipple = textPixmap;
   textCursorGC = XCreateGC(mainDisplay, mainWindow,
         GCForeground | GCBackground | GCFillStyle | GCStipple, &values);

   XParseColor(mainDisplay, mainColormap, myFgColorStr, &fg_color);
   XAllocColor(mainDisplay, mainColormap, &fg_color);
   XParseColor(mainDisplay, mainColormap, myBgColorStr, &bg_color);
   XAllocColor(mainDisplay, mainColormap, &bg_color);

   textCursor = XCreateFontCursor(mainDisplay, XC_xterm);
   cornerCursor = XCreateFontCursor(mainDisplay, XC_ul_angle);
   watchCursor = XCreateFontCursor(mainDisplay, XC_watch);

   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"DefaultCursor")) != NULL) {
      for (i = 0; *cursorName[i] != '\0'; i++) {
         if (strcmp(c_ptr, cursorName[i]) == 0) {
            default_cursor_id = cursorID[i];
            break;
         }
      }
      if (*cursorName[i] == '\0') {
         fprintf(stderr, "Can not find DefaultCursor %s, %s used instead.\n",
               c_ptr, cursorName[default_cursor_id>>1]);
      }
   }
   defaultCursor = XCreateFontCursor(mainDisplay, default_cursor_id);

   draw_cursor_id = default_cursor_id;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"DrawCursor")) != NULL) {
      for (i = 0; *cursorName[i] != '\0'; i++) {
         if (strcmp(c_ptr, cursorName[i]) == 0) {
            draw_cursor_id = cursorID[i];
            break;
         }
      }
      if (*cursorName[i] == '\0') {
         fprintf(stderr, "Can not find DrawCursor %s, %s used instead.\n",
               c_ptr, cursorName[draw_cursor_id>>1]);
      }
   }
   drawCursor = XCreateFontCursor(mainDisplay, draw_cursor_id);

   drag_cursor_id = XC_hand2;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "DragCursor")) != NULL) {
      for (i = 0; *cursorName[i] != '\0'; i++) {
         if (strcmp(c_ptr, cursorName[i]) == 0) {
            drag_cursor_id = cursorID[i];
            break;
         }
      }
      if (*cursorName[i] == '\0') {
         fprintf(stderr, "Can not find DragCursor %s, %s used instead.\n",
               c_ptr, cursorName[drag_cursor_id>>1]);
      }
   }
   handCursor = XCreateFontCursor(mainDisplay, drag_cursor_id);

   vertex_cursor_id = XC_plus;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"VertexCursor")) != NULL) {
      for (i = 0; *cursorName[i] != '\0'; i++) {
         if (strcmp(c_ptr, cursorName[i]) == 0) {
            vertex_cursor_id = cursorID[i];
            break;
         }
      }
      if (*cursorName[i] == '\0') {
         fprintf(stderr, "Can not find VertexCursor %s, %s used instead.\n",
               c_ptr, cursorName[vertex_cursor_id>>1]);
      }
   }
   vertexCursor = XCreateFontCursor(mainDisplay, vertex_cursor_id);

   rotate_shear_cursor_id = XC_crosshair;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"RotateCursor")) != NULL) {
      for (i = 0; *cursorName[i] != '\0'; i++) {
         if (strcmp(c_ptr, cursorName[i]) == 0) {
            rotate_shear_cursor_id = cursorID[i];
            break;
         }
      }
      if (*cursorName[i] == '\0') {
         fprintf(stderr, "Can not find RotateCursor %s, %s %s.\n",
               c_ptr, cursorName[rotate_shear_cursor_id>>1], "used instead");
      }
   }
   rotateCursor = XCreateFontCursor(mainDisplay, rotate_shear_cursor_id);

   rotatingCursor = XCreateFontCursor(mainDisplay, XC_exchange);
   horiShearCursor = XCreateFontCursor(mainDisplay, XC_sb_h_double_arrow);
   vertShearCursor = XCreateFontCursor(mainDisplay, XC_sb_v_double_arrow);
   moveCursor = XCreateFontCursor(mainDisplay, XC_fleur);

   XRecolorCursor(mainDisplay, textCursor, &fg_color, &bg_color);
   XRecolorCursor(mainDisplay, cornerCursor, &fg_color, &bg_color);
   XRecolorCursor(mainDisplay, handCursor, &fg_color, &bg_color);
   XRecolorCursor(mainDisplay, watchCursor, &fg_color, &bg_color);
   XRecolorCursor(mainDisplay, defaultCursor, &fg_color, &bg_color);
   XRecolorCursor(mainDisplay, drawCursor, &fg_color, &bg_color);
   XRecolorCursor(mainDisplay, vertexCursor, &fg_color, &bg_color);
   XRecolorCursor(mainDisplay, rotateCursor, &fg_color, &bg_color);
   XRecolorCursor(mainDisplay, rotatingCursor, &fg_color, &bg_color);
   XRecolorCursor(mainDisplay, horiShearCursor, &fg_color, &bg_color);
   XRecolorCursor(mainDisplay, vertShearCursor, &fg_color, &bg_color);
   XRecolorCursor(mainDisplay, moveCursor, &fg_color, &bg_color);

   nullPixmap = XCreateBitmapFromData(mainDisplay, mainWindow,
         null_bits, null_width, null_height);

   nullMaskPixmap = XCreateBitmapFromData(mainDisplay, mainWindow,
         nullmask_bits, nullmask_width, nullmask_height);

   nullCursor = XCreatePixmapCursor(mainDisplay, nullPixmap, nullMaskPixmap,
         &color, &color, 7, 0);

   hyperSpacePixmap = XCreateBitmapFromData(mainDisplay, mainWindow,
         helphand_bits, helphand_width, helphand_height);

   hyperSpaceMaskPixmap = XCreateBitmapFromData(mainDisplay, mainWindow,
         hhand_mk_bits, hhand_mk_width, hhand_mk_height);

   hyperSpaceCursor = XCreatePixmapCursor(mainDisplay, hyperSpacePixmap,
         hyperSpaceMaskPixmap, &fg_color, &bg_color, 5, 1);

   XRecolorCursor(mainDisplay, hyperSpaceCursor, &fg_color, &bg_color);

   magPixmap = XCreateBitmapFromData(mainDisplay, mainWindow,
         mag_bits, mag_width, mag_height);

   magMaskPixmap = XCreateBitmapFromData(mainDisplay, mainWindow,
         mag_mask_bits, mag_mask_width, mag_mask_height);

   magCursor = XCreatePixmapCursor(mainDisplay, magPixmap,
         magMaskPixmap, &fg_color, &bg_color, 8, 8);

   floodPixmap = XCreateBitmapFromData(mainDisplay, mainWindow,
         flood_bits, flood_width, flood_height);

   floodMaskPixmap = XCreateBitmapFromData(mainDisplay, mainWindow,
         flood_mk_bits, flood_mk_width, flood_mk_height);

   floodCursor = XCreatePixmapCursor(mainDisplay, floodPixmap,
         floodMaskPixmap, &fg_color, &bg_color, 9, 18);

   dripPixmap = XCreateBitmapFromData(mainDisplay, mainWindow,
         drip_bits, drip_width, drip_height);

   dripMaskPixmap = XCreateBitmapFromData(mainDisplay, mainWindow,
         drip_msk_bits, drip_msk_width, drip_msk_height);

   dripCursor = XCreatePixmapCursor(mainDisplay, dripPixmap,
         dripMaskPixmap, &fg_color, &bg_color, 10, 18);

   XRecolorCursor(mainDisplay, hyperSpaceCursor, &fg_color, &bg_color);
}

void PutCursor(window, x, y, foreground)
   Window window;
   int x, y, foreground;
{
   XGCValues values;

   values.foreground = foreground;
   values.ts_x_origin = x;
   values.ts_y_origin = y;
   XChangeGC(mainDisplay, textCursorGC,
         GCForeground | GCTileStipXOrigin | GCTileStipYOrigin, &values);
   XFillRectangle(mainDisplay, window, textCursorGC, x, y,
         text_cur_width, text_cur_height);
}

void CleanUpCursors()
{
   XFreePixmap(mainDisplay, textPixmap);
   XFreeGC(mainDisplay, textCursorGC);

   XFreeCursor(mainDisplay, textCursor);
   XFreeCursor(mainDisplay, cornerCursor);
   XFreeCursor(mainDisplay, handCursor);
   XFreeCursor(mainDisplay, defaultCursor);
   XFreeCursor(mainDisplay, watchCursor);
   XFreeCursor(mainDisplay, drawCursor);
   XFreeCursor(mainDisplay, vertexCursor);
   XFreeCursor(mainDisplay, rotateCursor);
   XFreeCursor(mainDisplay, rotatingCursor);
   XFreeCursor(mainDisplay, horiShearCursor);
   XFreeCursor(mainDisplay, vertShearCursor);
   XFreeCursor(mainDisplay, moveCursor);
   XFreeCursor(mainDisplay, hyperSpaceCursor);
   XFreeCursor(mainDisplay, magCursor);
   XFreeCursor(mainDisplay, floodCursor);
   XFreeCursor(mainDisplay, dripCursor);

   XFreePixmap(mainDisplay, nullPixmap);
   XFreePixmap(mainDisplay, nullMaskPixmap);
   XFreeCursor(mainDisplay, nullCursor);
   XFreePixmap(mainDisplay, hyperSpacePixmap);
   XFreePixmap(mainDisplay, hyperSpaceMaskPixmap);
   XFreePixmap(mainDisplay, magPixmap);
   XFreePixmap(mainDisplay, magMaskPixmap);
   XFreePixmap(mainDisplay, floodPixmap);
   XFreePixmap(mainDisplay, floodMaskPixmap);
   XFreePixmap(mainDisplay, dripPixmap);
   XFreePixmap(mainDisplay, dripMaskPixmap);
}

Cursor NewFontCursor(cursor_name)
   char *cursor_name;
{
   register int i;

   for (i = 0; *cursorName[i] != '\0'; i++) {
      if (strcmp(cursor_name, cursorName[i]) == 0) {
         return XCreateFontCursor(mainDisplay, cursorID[i]);
      }
   }
   return (Cursor)0;
}

void SetWindowCursor(window, cursor)
   Window window;
   Cursor cursor;
{
   XDefineCursor(mainDisplay, window, cursor);
}

void DeleteFontCursor(cursor)
   Cursor cursor;
{
   if (handCursor != (Cursor)0) XFreeCursor(mainDisplay, cursor);
}

