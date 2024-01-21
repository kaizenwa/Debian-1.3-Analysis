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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/animate.c,v 3.0 1996/05/06 16:03:36 william Exp $";
#endif

#include <stdio.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#ifndef _NO_EXTERN
#include "animate.e"
#endif
#include "color.e"
#include "dialog.e"
#include "msg.e"
#include "poly.e"
#include "raster.e"
#include "select.e"
#include "setup.e"

#define SEND_SPEED 8

#define TOKEN_R 8

static XPoint	savedToken[5];

void AnimateSend(PolyPtr, Speed, Pixel)
   struct PolyRec *PolyPtr;
   int Speed, Pixel;
{
   register int delta, i;
   register XPoint *token;
   int x, y, num_pts, j, x_dist, y_dist;
   IntPoint *v;
   struct BBRec bbox;
   double slope, delta_x, delta_y;
   XGCValues values;

   values.foreground = Pixel;
   values.function = GXxor;
   values.line_style = FillSolid;
#ifdef NO_THIN_LINE
   values.line_width = 1;
#else
   values.line_width = 0;
#endif
   XChangeGC(mainDisplay, drawGC,
         GCForeground | GCFunction | GCLineStyle | GCLineWidth, &values);

   bbox.ltx = 0; bbox.lty = 0; bbox.rbx = 2*TOKEN_R; bbox.rby = 2*TOKEN_R;

   num_pts = PolyPtr->n;
   v = PolyPtr->vlist;
   token = (XPoint *)malloc(5*sizeof(XPoint));
   if (token == NULL) FailAllocMessage();

   for (j = 0; j < num_pts-1; j++) {
      x = OFFSET_X(v[j].x);
      y = OFFSET_Y(v[j].y);
      token[0].x = (short)(x - TOKEN_R); token[0].y = (short)(y - TOKEN_R);
      token[1].x = (short)(x + TOKEN_R); token[1].y = (short)(y - TOKEN_R);
      token[2].x = (short)(x + TOKEN_R); token[2].y = (short)(y + TOKEN_R);
      token[3].x = (short)(x - TOKEN_R); token[3].y = (short)(y + TOKEN_R);
      token[4].x = (short)(x - TOKEN_R); token[4].y = (short)(y - TOKEN_R);
      XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5, Convex,
            CoordModeOrigin);
      if (v[j].x == v[j+1].x) {
         /* moving vertical */
         if ((y_dist = ZOOMED_SIZE(v[j+1].y-v[j].y)) > 0) {
            /* moving down */
            for (delta = 0; delta < y_dist; delta += Speed) {
               XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5, Convex,
                     CoordModeOrigin);
               for (i = 0; i < 5; i++) token[i].y += Speed;
               XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5, Convex,
                     CoordModeOrigin);
            }
         } else {
            /* moving up */
            for (delta = y_dist; delta < 0; delta += Speed) {
               XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5, Convex,
                     CoordModeOrigin);
               for (i = 0; i < 5; i++) token[i].y -= Speed;
               XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5, Convex,
                     CoordModeOrigin);
            }
         }
      } else if (v[j].y == v[j+1].y) {
         /* moving horizontal */
         if ((x_dist = ZOOMED_SIZE(v[j+1].x-v[j].x)) > 0) {
            /* moving right */
            for (delta = 0; delta < x_dist; delta += Speed) {
               XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5, Convex,
                     CoordModeOrigin);
               for (i = 0; i < 5; i++) token[i].x += Speed;
               XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5, Convex,
                     CoordModeOrigin);
            }
         } else {
            /* moving left */
            for (delta = x_dist; delta < 0; delta += Speed) {
               XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5, Convex,
                     CoordModeOrigin);
               for (i = 0; i < 5; i++) token[i].x -= Speed;
               XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5, Convex,
                     CoordModeOrigin);
            }
         }
      } else {
         /* moving diagonally */
         x_dist = ZOOMED_SIZE(v[j+1].x-v[j].x);
         y_dist = ZOOMED_SIZE(v[j+1].y-v[j].y);
         for (i = 0; i < 5; i++) {
            savedToken[i].x = token[i].x;
            savedToken[i].y = token[i].y;
         }
         if (abs (x_dist) > abs (y_dist)) {
            /* moving in the x direction */
            slope = ((double)y_dist) / ((double)x_dist);
            if (x_dist > 0) {
               /* moving right */
               for (delta = 0; delta < x_dist; delta += Speed) {
                  XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5,
                        Convex, CoordModeOrigin);
                  delta_y = slope * ((double)delta);
                  for (i = 0; i < 5; i++) {
                     token[i].x = savedToken[i].x + delta;
                     token[i].y = savedToken[i].y + delta_y;
                  }
                  XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5,
                        Convex, CoordModeOrigin);
               }
            } else {
               /* moving left */
               for (delta = 0; delta > x_dist; delta -= Speed) {
                  XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5,
                        Convex, CoordModeOrigin);
                  delta_y = slope * ((double)delta);
                  for (i = 0; i < 5; i++) {
                     token[i].x = savedToken[i].x + delta;
                     token[i].y = savedToken[i].y + delta_y;
                  }
                  XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5,
                        Convex, CoordModeOrigin);
               }
            }
         } else {
            /* moving in the y direction */
            slope = ((double)x_dist) / ((double)y_dist);
            if (y_dist > 0) {
               /* moving down */
               for (delta = 0; delta < y_dist; delta += Speed) {
                  XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5,
                        Convex, CoordModeOrigin);
                  delta_x = slope * ((double)delta);
                  for (i = 0; i < 5; i++) {
                     token[i].x = savedToken[i].x + delta_x;
                     token[i].y = savedToken[i].y + delta;
                  }
                  XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5,
                        Convex, CoordModeOrigin);
               }
            } else {
               /* moving up */
               for (delta = 0; delta > y_dist; delta -= Speed) {
                  XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5,
                        Convex, CoordModeOrigin);
                  delta_x = slope * ((double)delta);
                  for (i = 0; i < 5; i++) {
                     token[i].x = savedToken[i].x + delta_x;
                     token[i].y = savedToken[i].y + delta;
                  }
                  XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5,
                        Convex, CoordModeOrigin);
               }
            }
         }
      }
      XFillPolygon(mainDisplay, drawWindow, drawGC, token, 5, Convex,
            CoordModeOrigin);
   }
   free(token);
}

void AnimateSel()
{
   if (topSel != botSel || topSel == NULL || topSel->obj->type != OBJ_POLY) {
      MsgBox("Please select only one POLY object.", TOOL_NAME, INFO_MB);
      return;
   }
   AnimateSend(topSel->obj->detail.p, SEND_SPEED,
         xorColorPixels[topSel->obj->color]);
}

void AnimateFlashColor(ObjPtr, ColorIndex)
   struct ObjRec *ObjPtr;
   int ColorIndex;
{
   int saved_color_index = ObjPtr->color;

   ObjPtr->color = ColorIndex;
   DrawPolyObj(drawWindow, drawOrigX, drawOrigY, ObjPtr);
   ObjPtr->color = saved_color_index;
   DrawPolyObj(drawWindow, drawOrigX, drawOrigY, ObjPtr);
}

void FlashSelColor()
{
   register int i;

   if (topSel != botSel || topSel == NULL || topSel->obj->type != OBJ_POLY) {
      MsgBox("Please select only one POLY object.", TOOL_NAME, INFO_MB);
      return;
   }
   for (i = 0; i < maxColors; i++) {
      if (strcmp("white", colorMenuItems[i]) == 0) {
         break;
      }
   }
   AnimateFlashColor(topSel->obj, i);
}
