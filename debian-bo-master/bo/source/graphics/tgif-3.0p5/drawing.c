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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/drawing.c,v 3.2 1996/05/15 17:33:08 william Exp $";
#endif

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#ifdef _NO_GETTIMEOFDAY
#include <sys/timeb.h>
#endif /* _NO_GETTIMEOFDAY */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#include "align.e"
#include "animate.e"
#include "arc.e"
#include "attr.e"
#include "auxtext.e"
#include "box.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cutpaste.e"
#include "cursor.e"
#include "dialog.e"
#ifndef _NO_EXTERN
#include "drawing.e"
#endif
#include "dup.e"
#include "edit.e"
#include "eps.e"
#include "exec.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "group.e"
#include "imgproc.e"
#include "import.e"
#include "mark.e"
#include "mainloop.e"
#include "menu.e"
#include "msg.e"
#include "navigate.e"
#include "names.e"
#include "obj.e"
#include "oval.e"
#include "page.e"
#include "pattern.e"
#include "poly.e"
#include "polygon.e"
#include "raster.e"
#include "rcbox.e"
#include "rect.e"
#include "remote.e"
#include "ruler.e"
#include "scroll.e"
#include "select.e"
#include "setup.e"
#include "shortcut.e"
#include "special.e"
#include "stk.e"
#include "stretch.e"
#include "text.e"
#ifdef _TGIF_WB
#include "wb1.e"
#endif /* _TGIF_WB */
#include "xbitmap.e"
#include "xpixmap.e"

#define O_VIS 4
#define O_INVIS 4
#define O_GRID (O_VIS+O_INVIS)

#define DEF_CHECK_INTERVAL 1

int		intrCheckInterval=DEF_CHECK_INTERVAL;
int		pasteInDrawTextMode=FALSE;
int		pasteFromFileInDrawTextMode=FALSE;
char		pasteFromFileName[MAXPATHLENGTH+1];
int		copyInDrawTextMode=FALSE;
int		numRedrawBBox=INVALID;
int		numClipRecs=0;
int		clipOrdering=Unsorted;
XRectangle	clipRecs[4];
int		checkBBox=TRUE;

static struct BBRec	smallArea[2];

static Pixmap	execAnimatePixmap=None;
static int	execAnimatePixmapW=0, execAnimatePixmapH=0;

static int skipCrossHair=FALSE;

void SetDefaultDrawWinClipRecs ()
{
   SetRecVals (clipRecs[0], 0, 0, ZOOMED_SIZE(drawWinW), ZOOMED_SIZE(drawWinH));
   numClipRecs = 1;
   clipOrdering = YXBanded;
   XSetClipRectangles (mainDisplay, drawGC, 0, 0, clipRecs, numClipRecs,
         clipOrdering);
}

void SetDefaultIconWinClipRecs ()
{
   SetRecVals (clipRecs[0], 0, 0, iconWindowW, iconWindowH);
   numClipRecs = 1;
   clipOrdering = YXBanded;
   XSetClipRectangles (mainDisplay, drawGC, 0, 0, clipRecs, numClipRecs,
         clipOrdering);
}

static
void DrawHorizOutline(Win, Y, X1, X2, XStart, XEnd)
   Window Win;
   int Y, X1, X2, XStart, XEnd;
   /* XStart and XEnd are the real place, X1 and X2 are on outline grid */
{
   register int i;

   if (XStart-X1 < O_VIS)
      XDrawLine (mainDisplay, Win, defaultGC, XStart, Y, X1+O_VIS-1, Y);
   for (i = X1+O_GRID; i < X2-O_GRID; i+= O_GRID)
      XDrawLine (mainDisplay, Win, defaultGC, i, Y, i+O_VIS-1, Y);
   if (X2-XEnd < O_VIS)
      XDrawLine (mainDisplay, Win, defaultGC, X2-O_GRID, Y, XEnd, Y);
   else
      XDrawLine (mainDisplay, Win, defaultGC, X2-O_GRID, Y, X2-O_INVIS-1, Y);
}

static
void DrawVertOutline(Win, X, Y1, Y2, YStart, YEnd)
   Window Win;
   int X, Y1, Y2, YStart, YEnd;
   /* YStart and YEnd are the real place, Y1 and Y2 are on outline grid */
{
   register int i;

   if (YStart-Y1 < O_VIS)
      XDrawLine (mainDisplay, Win, defaultGC, X, YStart, X, Y1+O_VIS-1);
   for (i = Y1+O_GRID; i < Y2-O_GRID; i+= O_GRID)
      XDrawLine (mainDisplay, Win, defaultGC, X, i, X, i+O_VIS-1);
   if (Y2-YEnd < O_VIS)
      XDrawLine (mainDisplay, Win, defaultGC, X, Y2-O_GRID, X, YEnd);
   else
      XDrawLine (mainDisplay, Win, defaultGC, X, Y2-O_GRID, X, Y2-O_INVIS-1);
}

static
void DrawSymOutline(Win, XOff, YOff, ObjPtr)
   Window Win;
   int XOff, YOff;
   struct ObjRec *ObjPtr;
{
   int ltx, lty, rbx, rby, x_start, x_end, y_start, y_end;

   ltx = ZOOMED_SIZE(ObjPtr->obbox.ltx - XOff - QUARTER_INCH) + 1;
   lty = ZOOMED_SIZE(ObjPtr->obbox.lty - YOff - QUARTER_INCH) + 1;
   rbx = ZOOMED_SIZE(ObjPtr->obbox.rbx - XOff + QUARTER_INCH) - 1;
   rby = ZOOMED_SIZE(ObjPtr->obbox.rby - YOff + QUARTER_INCH) - 1;

   x_start = (ltx % O_GRID == 0) ? ltx : (int)(ltx / O_GRID) * O_GRID;
   x_end = (rbx % O_GRID == 0) ? rbx : ((int)(rbx / O_GRID) + 1) * O_GRID;
   DrawHorizOutline (Win, lty, x_start, x_end, ltx, rbx);
   DrawHorizOutline (Win, rby, x_start, x_end, ltx, rbx);
   y_start = (lty % O_GRID == 0) ? lty : (int)(lty / O_GRID) * O_GRID;
   y_end = (rby % O_GRID == 0) ? rby : ((int)(rby / O_GRID) + 1) * O_GRID;
   DrawVertOutline (Win, ltx, y_start, y_end, lty, rby);
   DrawVertOutline (Win, rbx, y_start, y_end, lty, rby);
}

static
int NeedToDraw(ObjBBox)
   struct BBRec ObjBBox;
{
   switch (numRedrawBBox) {
   case 0: return (BBoxIntersect(ObjBBox, drawWinBBox));
   case 1: return (BBoxIntersect(ObjBBox, drawWinBBox) &&
         BBoxIntersect(ObjBBox, smallArea[0]));
   case 2: return (BBoxIntersect(ObjBBox, drawWinBBox) &&
         (BBoxIntersect(ObjBBox, smallArea[0]) ||
         BBoxIntersect(ObjBBox, smallArea[1])));
   default: fprintf(stderr, "Warning:  Invalid numRedrawBBox.\n"); break;
   }
   return TRUE;
}

#include "xbm/intr.xbm"
#include "xbm/trek.xbm"

static int intrShown=FALSE;
static int checkCount=0;
static int savedCheckInterval=(-1);
static int intrIndex=(-1);

static long intrTick=0L;

static
void RedrawInterrupt()
{
   GC gc;
   XGCValues values;
   int x, y;
#ifdef _NO_GETTIMEOFDAY
   struct timeb now;
#else /* ~_NO_GETTIMEOFDAY */
   struct timeval now;
   struct timezone zone;
#endif /* _NO_GETTIMEOFDAY */
   long cur_tick;

   if (!intrShown) return;

#ifdef _NO_GETTIMEOFDAY
   ftime(&now);
   cur_tick = ((long)(((long)now.millitm) / 200)) +
         ((long)(((long)now.time) * 5));
#else /* ~_NO_GETTIMEOFDAY */
   gettimeofday(&now, &zone);
   cur_tick = ((long)(now.tv_usec / 200000)) + ((long)(now.tv_sec * 5));
#endif /* _NO_GETTIMEOFDAY */
   if (intrIndex != (-1) && intrTick == cur_tick) return;

   intrTick = cur_tick;
   if (++intrIndex == MAXINTRS) intrIndex = 0;

   x = ((rulerW-intr_width)>>1);
   y = ((rulerW-intr_height)>>1);
   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.fill_style = FillOpaqueStippled;
   values.stipple = intrPixmap[intrIndex];
   values.ts_x_origin = x;
   values.ts_y_origin = y;
   gc = XCreateGC (mainDisplay, dummyWindow1,
         GCForeground | GCBackground | GCFillStyle | GCStipple |
         GCTileStipXOrigin | GCTileStipYOrigin, &values);
   XFillRectangle (mainDisplay, dummyWindow1, gc, x, y, intr_width,
         intr_height);
   XFreeGC(mainDisplay, gc);

   XSync (mainDisplay, False);
}

static
void ShowHyperSpace()
{
   GC gc;
   XGCValues values;
   int x, y;

   x = ((rulerW-trek_width)>>1);
   y = ((rulerW-trek_height)>>1);
   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.fill_style = FillOpaqueStippled;
   values.stipple = trekPixmap;
   values.ts_x_origin = x;
   values.ts_y_origin = y;
   gc = XCreateGC (mainDisplay, dummyWindow1,
         GCForeground | GCBackground | GCFillStyle | GCStipple |
         GCTileStipXOrigin | GCTileStipYOrigin, &values);
   XFillRectangle (mainDisplay, dummyWindow1, gc, x, y, trek_width,
         trek_height);
   XFreeGC(mainDisplay, gc);
}

static int interruptLevel=0;

void ShowInterrupt(CheckInterval)
   int CheckInterval;
{
   if (interruptLevel++ > 0) return;

   if (CheckInterval > 0)
   {
      savedCheckInterval = intrCheckInterval;
      intrCheckInterval = CheckInterval;
   }
   if (intrCheckInterval <= 0) return;

   intrShown = TRUE;
   intrIndex = (-1);
   RedrawInterrupt ();
}

int HideInterrupt ()
{
   if (--interruptLevel > 0) return interruptLevel;
   interruptLevel = 0;
   if (execAnimatePixmap == None) {
      XEvent ev;

      while (XCheckWindowEvent(mainDisplay,dummyWindow1,ButtonPressMask,&ev)) ;
   }
   XClearWindow (mainDisplay, dummyWindow1);
   intrShown = FALSE;
   checkCount = 0;
   if (savedCheckInterval > 0)
   {
      intrCheckInterval = savedCheckInterval;
      savedCheckInterval = (-1);
   }
   if (inHyperSpace) ShowHyperSpace ();
   XSync(mainDisplay, False);
   return 0;
}

void RedrawDummyWindow1()
{
   XEvent ev;

   while (XCheckWindowEvent(mainDisplay, dummyWindow1, ExposureMask, &ev)) ;
   while (XCheckWindowEvent(mainDisplay, dummyWindow1, ButtonPressMask, &ev)) ;
   if (intrShown) {
      RedrawInterrupt();
   } else if (inHyperSpace) {
      ShowHyperSpace();
   } else {
      HideInterrupt();
   }
}

void DummiesEventHandler(input)
   XEvent *input;
{
   if (input->xany.window == dummyWindow1)
   {
      if (input->type == Expose)
         RedrawDummyWindow1 ();
      else if (input->type == EnterNotify)
      {
         if (intrShown)
            SetMouseStatus ("interrupt", "interrupt", "interrupt");
         else
            SetMouseStatus ("(none)", "(none)", "(none)");
      }
      else if (input->type == ButtonPress)
      {
         if (!intrShown && execAnimatePixmap == None)
            ToggleHyperSpace (FALSE);
         else if (intrShown)
            HideInterrupt ();
      }
   }
   else if (input->xany.window == dummyWindow2)
      SetMouseStatus ("(none)", "(none)", "(none)");
}

static XComposeStatus c_stat;

static
Bool CheckESC(p_display, p_ev, psz_arg)
   Display *p_display;
   XEvent *p_ev;
   char *psz_arg;
{
   if (p_ev->type == KeyPress) {
      XKeyEvent *key_ev=(&(p_ev->xkey));
      KeySym key_sym;
      char s[80];
      int has_ch=XLookupString(key_ev, s, sizeof(s)-1, &key_sym, &c_stat);

      TranslateKeys(s, &key_sym);
      if (key_sym== XK_Escape || (has_ch && s[0]=='\033')) {
         return True;
      }
   }
   return False;
}

int ESCPressed()
{
   XEvent ev;

   if (XCheckIfEvent(mainDisplay, &ev, CheckESC, NULL)) {
      return TRUE;
   }
   return FALSE;
}

int CheckInterrupt()
{
   if (execAnimatePixmap == None && intrCheckInterval <= 0) return (FALSE);
   if (++checkCount >= intrCheckInterval) {
      XEvent ev;

      RedrawInterrupt();
      checkCount = 0;
      if (XCheckWindowEvent(mainDisplay, dummyWindow1, ButtonPressMask, &ev)) {
         while (XCheckWindowEvent(mainDisplay, dummyWindow1, ButtonPressMask,
               &ev)) ;
         return TRUE;
      }
      if (ESCPressed()) return TRUE;
   }
   return FALSE;
}

void DrawClippedPixmap(pixmap, win, gc, pixmap_w, pixmap_h, ltx, lty)
   Pixmap pixmap;
   Window win;
   GC gc;
   int pixmap_w, pixmap_h, ltx, lty;
{
   if (numClipRecs <= 0) {
      XCopyArea(mainDisplay, pixmap, win, gc, 0, 0, pixmap_w, pixmap_h,
            ltx, lty);
   } else {
      int i;
      struct BBRec pixmap_bbox;

      pixmap_bbox.ltx = ltx;
      pixmap_bbox.lty = lty;
      pixmap_bbox.rbx = ltx+pixmap_w;
      pixmap_bbox.rby = lty+pixmap_h;
      for (i=0; i < numClipRecs; i++) {
         struct BBRec bbox;

         bbox.ltx = (int)clipRecs[i].x;
         bbox.lty = (int)clipRecs[i].y;
         bbox.rbx = bbox.ltx + ((int)clipRecs[i].width);
         bbox.rby = bbox.lty + ((int)clipRecs[i].height);
         if (BBoxIntersect(pixmap_bbox, bbox)) {
            int x, y, w, h;

            bbox.ltx = max(bbox.ltx, pixmap_bbox.ltx);
            bbox.lty = max(bbox.lty, pixmap_bbox.lty);
            bbox.rbx = min(bbox.rbx, pixmap_bbox.rbx);
            bbox.rby = min(bbox.rby, pixmap_bbox.rby);
            x = bbox.ltx - pixmap_bbox.ltx;
            y = bbox.lty - pixmap_bbox.lty;
            w = bbox.rbx - bbox.ltx;
            h = bbox.rby - bbox.lty;
            XCopyArea(mainDisplay, pixmap, win, gc, x, y, w, h,
                  ltx+x, lty+y);
         }
      }
   }
}

void FillClippedRectangle(win, gc, ltx, lty, orig_w, orig_h)
   Window win;
   GC gc;
   int ltx, lty, orig_w, orig_h;
{
   if (numClipRecs <= 0) {
      XFillRectangle(mainDisplay, win, gc, ltx, lty, orig_w, orig_h);
   } else {
      int i;
      struct BBRec obj_bbox;

      obj_bbox.ltx = ltx;
      obj_bbox.lty = lty;
      obj_bbox.rbx = ltx+orig_w;
      obj_bbox.rby = lty+orig_h;
      for (i=0; i < numClipRecs; i++) {
         struct BBRec bbox;

         bbox.ltx = (int)clipRecs[i].x;
         bbox.lty = (int)clipRecs[i].y;
         bbox.rbx = bbox.ltx + ((int)clipRecs[i].width);
         bbox.rby = bbox.lty + ((int)clipRecs[i].height);
         if (BBoxIntersect(obj_bbox, bbox)) {
            int x, y, w, h;

            bbox.ltx = max(bbox.ltx, obj_bbox.ltx);
            bbox.lty = max(bbox.lty, obj_bbox.lty);
            bbox.rbx = min(bbox.rbx, obj_bbox.rbx);
            bbox.rby = min(bbox.rby, obj_bbox.rby);
            x = bbox.ltx - obj_bbox.ltx;
            y = bbox.lty - obj_bbox.lty;
            w = bbox.rbx - bbox.ltx;
            h = bbox.rby - bbox.lty;
            XFillRectangle(mainDisplay, win, gc, ltx+x, lty+y, w, h);
         }
      }
   }
}

int ObjInVisibleLayer(ObjPtr)
   struct ObjRec *ObjPtr;
{
   struct ObjRec *obj_ptr;
   struct AttrRec *attr_ptr;

   switch (ObjPtr->type) {
   case OBJ_POLY:
   case OBJ_BOX:
   case OBJ_OVAL:
   case OBJ_TEXT:
   case OBJ_POLYGON:
   case OBJ_ARC:
   case OBJ_RCBOX:
   case OBJ_XBM:
      if (colorLayerOn[ObjPtr->color]) {
         return TRUE;
      }
      break;

   case OBJ_XPM: return TRUE;

   case OBJ_GROUP:
   case OBJ_ICON:
   case OBJ_SYM:
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         obj_ptr->tmp_parent = ObjPtr;
         if (ObjInVisibleLayer(obj_ptr)) {
            return TRUE;
         }
      }
      break;
   }
   for (attr_ptr=ObjPtr->fattr; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
      if (attr_ptr->shown && ObjInVisibleLayer(attr_ptr->obj)) {
         return TRUE;
      }
   }
   return FALSE;
}

int DrawObj(Win, ObjPtr)
   Window Win;
   register struct ObjRec *ObjPtr;
   /* returns TRUE if all objects are drawn */
   /* returns FALSE if interrupted by the user */
{
   if (placingTopObj && ObjPtr==topObj) {
      return TRUE;
   }
   switch (ObjPtr->type) {
   case OBJ_POLY:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DrawPolyObj(Win, drawOrigX, drawOrigY, ObjPtr); 
         DrawAttrs(Win, drawOrigX, drawOrigY, ObjPtr->fattr);
      }
      break;
   case OBJ_BOX:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DrawBoxObj(Win, drawOrigX, drawOrigY, ObjPtr);
         DrawAttrs(Win, drawOrigX, drawOrigY, ObjPtr->fattr);
      }
      break;
   case OBJ_OVAL:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DrawOvalObj(Win, drawOrigX, drawOrigY, ObjPtr);
         DrawAttrs(Win, drawOrigX, drawOrigY, ObjPtr->fattr);
      }
      break;
   case OBJ_TEXT:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DrawTextObj(Win, drawOrigX, drawOrigY, ObjPtr);
      }
      break;
   case OBJ_POLYGON:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DrawPolygonObj(Win, drawOrigX, drawOrigY, ObjPtr);
         DrawAttrs(Win, drawOrigX, drawOrigY, ObjPtr->fattr);
      }
      break;
   case OBJ_ARC:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DrawArcObj(Win, drawOrigX, drawOrigY, ObjPtr);
         DrawAttrs(Win, drawOrigX, drawOrigY, ObjPtr->fattr);
      }
      break;
   case OBJ_RCBOX:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DrawRCBoxObj(Win, drawOrigX, drawOrigY, ObjPtr);
         DrawAttrs(Win, drawOrigX, drawOrigY, ObjPtr->fattr);
      }
      break;
   case OBJ_XBM:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DrawXBmObj(Win, drawOrigX, drawOrigY, ObjPtr);
         DrawAttrs(Win, drawOrigX, drawOrigY, ObjPtr->fattr);
      }
      break;
   case OBJ_XPM:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DrawXPmObj(Win, drawOrigX, drawOrigY, ObjPtr);
         DrawAttrs(Win, drawOrigX, drawOrigY, ObjPtr->fattr);
      }
      break;

   case OBJ_SYM:
   case OBJ_ICON:
   case OBJ_GROUP:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         struct ObjRec *obj_ptr=ObjPtr->detail.r->last;

         for ( ; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
            if (!checkBBox || NeedToDraw(obj_ptr->bbox)) {
               obj_ptr->tmp_parent = ObjPtr;
               if (!DrawObj(Win, obj_ptr)) return FALSE;
               if (execAnimatePixmap == None && CheckInterrupt()) {
                  Msg("User interrupt.  Repaint aborted.");
                  return FALSE;
               }
            }
         }
         if (ObjPtr->type == OBJ_ICON && ObjPtr->dirty) {
            struct AttrRec *attr_ptr=ObjPtr->fattr;

            for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
               UpdTextBBox(attr_ptr->obj);
            }
            AdjObjBBox(ObjPtr);
            UpdSelBBox();
            ObjPtr->dirty = FALSE;
         }
         DrawAttrs(Win, drawOrigX, drawOrigY, ObjPtr->fattr);
         if (ObjPtr->type == OBJ_SYM) {
            DrawSymOutline(Win, drawOrigX, drawOrigY, ObjPtr);
         }
      }
      break;
   }
   return TRUE;
}

void DrawPaperBoundary(Win)
   Window Win;
{
   register int x_end, y_end;

   if (drawOrigX+drawWinW > paperWidth)
   {
      x_end = OFFSET_X(paperWidth);
      if (drawOrigY+drawWinH > paperHeight)
      {
         y_end = OFFSET_Y(paperHeight);
         XDrawLine (mainDisplay, Win, defaultGC, x_end, 0, x_end, y_end);
         XDrawLine (mainDisplay, Win, defaultGC, 0, y_end, x_end, y_end);
      }
      else
         XDrawLine (mainDisplay, Win, defaultGC, x_end, 0, x_end,
               ZOOMED_SIZE(drawWinH));
   }
   else if (drawOrigY+drawWinH > paperHeight)
   {
      y_end = OFFSET_Y(paperHeight);
      XDrawLine (mainDisplay, Win, defaultGC, 0, y_end,
            ZOOMED_SIZE(drawWinW), y_end);
   }
}

void RedrawAnArea(BotObj, LtX, LtY, RbX, RbY)
   struct ObjRec *BotObj;
   int LtX, LtY, RbX, RbY;
   /* LtX, LtY, RbX, RbY are absolute coordinates */
{
   register struct ObjRec *obj_ptr;
   int x=OFFSET_X(LtX), y=OFFSET_Y(LtY), redraw_cross_hair=FALSE;
   int w=ZOOMED_SIZE(RbX-LtX)+1, h=ZOOMED_SIZE(RbY-LtY)+1;

   smallArea[0].ltx = LtX; smallArea[0].lty = LtY;
   smallArea[0].rbx = RbX; smallArea[0].rby = RbY;
   if (!BBoxIntersect(smallArea[0], drawWinBBox)) {
      return;
   }
   SetRecVals(clipRecs[0], OFFSET_X(LtX), OFFSET_Y(LtY),
         ZOOMED_SIZE(RbX-LtX)+1, ZOOMED_SIZE(RbY-LtY)+1);
   numClipRecs = 1;
   clipOrdering = YXBanded;
   XSetClipRectangles(mainDisplay, drawGC, 0, 0, clipRecs, numClipRecs,
         clipOrdering);

   if (execAnimatePixmap != None) {
      XGCValues values;
      int real_w=(x+w >= execAnimatePixmapW ? execAnimatePixmapW-x : w);
      int real_h=(y+h >= execAnimatePixmapH ? execAnimatePixmapH-y : h);

      if (!skipCrossHair && showCrossHair) {
         int cx, cy;

         GetCrossHairPosition(&cx, &cy, NULL);
         if (cx >= x && cx < x+real_w && cy >= y && cy < y+real_h) {
            RedrawCrossHair();
            redraw_cross_hair = TRUE;
         }
      }
      values.foreground = myBgPixel;
      values.function = GXcopy;
      values.fill_style = FillSolid;
      XChangeGC(mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle, &values);
      XFillRectangle(mainDisplay, execAnimatePixmap, drawGC,
            x, y, real_w, real_h);
   } else {
      if (!skipCrossHair && showCrossHair) {
         int cx, cy;

         GetCrossHairPosition(&cx, &cy, NULL);
         if (cx >= x && cx < x+w && cy >= y && cy < y+h) {
            RedrawCrossHair();
            redraw_cross_hair = TRUE;
         }
      }
      XClearArea(mainDisplay, drawWindow, x, y, w, h, FALSE);
   }

   if ((paperWidth >= LtX && paperWidth <= RbX) ||
         (paperHeight >= LtY && paperHeight <= RbY)) {
      DrawPaperBoundary(execAnimatePixmap==None ? drawWindow :
            execAnimatePixmap);
   }
   if (execAnimatePixmap != None) {
      DrawGridLines(execAnimatePixmap, x, y, w, h);
      DrawPageLines(execAnimatePixmap, x, y, w, h);
   } else {
      DrawGridLines(drawWindow, x, y, w, h);
      DrawPageLines(drawWindow, x, y, w, h);
   }

   numRedrawBBox = 1;
   smallArea[0].ltx = LtX; smallArea[0].lty = LtY;
   smallArea[0].rbx = RbX; smallArea[0].rby = RbY;
   ShowInterrupt(DEF_CHECK_INTERVAL);
   for (obj_ptr = BotObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
      obj_ptr->tmp_parent = NULL;
      if (BBoxIntersect(obj_ptr->bbox, drawWinBBox) &&
            BBoxIntersect(obj_ptr->bbox, smallArea[0])) {
         if (!DrawObj(execAnimatePixmap==None ? drawWindow : execAnimatePixmap,
               obj_ptr)) {
            break;
         }
         if (execAnimatePixmap == None && CheckInterrupt()) {
            Msg("User interrupt.  Repaint aborted.");
            break;
         }
      }
   }
   HideInterrupt();
   SetDefaultDrawWinClipRecs();
   if (execAnimatePixmap != None && execAnimateRedraw &&
         x < execAnimatePixmapW && y < execAnimatePixmapH) {
      int real_w=(x+w >= execAnimatePixmapW ? execAnimatePixmapW-x : w);
      int real_h=(y+h >= execAnimatePixmapH ? execAnimatePixmapH-y : h);

      XSetFunction(mainDisplay, drawGC, GXcopy);
      XCopyArea(mainDisplay, execAnimatePixmap, drawWindow, drawGC,
            x, y, real_w, real_h, x, y);
   }
   if (redraw_cross_hair) RedrawCrossHair();
}

void RedrawAreas(BotObj, LtX1, LtY1, RbX1, RbY1, LtX2, LtY2, RbX2, RbY2)
   struct ObjRec *BotObj;
   int LtX1, LtY1, RbX1, RbY1, LtX2, LtY2, RbX2, RbY2;
   /* note:  these coordinates are absolute */
{
   register struct ObjRec *obj_ptr;
   struct BBRec bbox1, bbox2;
   int rec1_slot, redraw_cross_hair=FALSE;
   int x1=OFFSET_X(LtX1), y1=OFFSET_Y(LtY1);
   int w1=ZOOMED_SIZE(RbX1-LtX1)+1, h1=ZOOMED_SIZE(RbY1-LtY1)+1;
   int x2=OFFSET_X(LtX2), y2=OFFSET_Y(LtY2);
   int w2=ZOOMED_SIZE(RbX2-LtX2)+1, h2=ZOOMED_SIZE(RbY2-LtY2)+1;

   bbox1.ltx = LtX1; bbox1.lty = LtY1;
   bbox1.rbx = RbX1; bbox1.rby = RbY1;
   bbox2.ltx = LtX2; bbox2.lty = LtY2;
   bbox2.rbx = RbX2; bbox2.rby = RbY2;

   if (Inside(bbox1, bbox2)) {
      RedrawAnArea(BotObj, LtX2, LtY2, RbX2, RbY2);
      return;
   } else if (Inside(bbox2, bbox1)) {
      RedrawAnArea(BotObj, LtX1, LtY1, RbX1, RbY1);
      return;
   }
   if (!BBoxIntersect(bbox1, drawWinBBox) &&
         !BBoxIntersect(bbox2, drawWinBBox)) {
      return;
   }

   if (execAnimatePixmap != None) {
      XGCValues values;
      int real_w=(x1+w1 >= execAnimatePixmapW ? execAnimatePixmapW-x1 : w1);
      int real_h=(y1+h1 >= execAnimatePixmapH ? execAnimatePixmapH-y1 : h1);

      if (!skipCrossHair && showCrossHair) {
         int cx, cy;

         GetCrossHairPosition(&cx, &cy, NULL);
         if (cx >= x1 && cx < x1+real_w && cy >= y1 && cy < y1+real_h) {
            RedrawCrossHair();
            redraw_cross_hair = TRUE;
         }
      }
      values.foreground = myBgPixel;
      values.function = GXcopy;
      values.fill_style = FillSolid;
      XChangeGC(mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle, &values);
      XFillRectangle(mainDisplay, execAnimatePixmap, drawGC,
            x1, y1, real_w, real_h);
   } else {
      if (!skipCrossHair && showCrossHair) {
         int cx, cy;

         GetCrossHairPosition(&cx, &cy, NULL);
         if (cx >= x1 && cx < x1+w1 && cy >= y1 && cy < y1+h1) {
            RedrawCrossHair();
            redraw_cross_hair = TRUE;
         }
      }
      XClearArea(mainDisplay, drawWindow, x1, y1, w1, h1, FALSE);
   }

   if (BBoxIntersect(bbox1, bbox2)) {
      int union_ltx, union_lty, union_rbx, union_rby;

      union_ltx = min(LtX1,LtX2); union_lty = min(LtY1,LtY2);
      union_rbx = max(RbX1,RbX2); union_rby = max(RbY1,RbY2);
      skipCrossHair = TRUE;
      RedrawAnArea(BotObj, union_ltx, union_lty, union_rbx, union_rby);
      skipCrossHair = FALSE;
      if (redraw_cross_hair) RedrawCrossHair();
      return;
   }
   if (LtY1 == LtY2) {
      rec1_slot = (LtX1 <= LtX2) ? 0 : 1;
      SetRecVals(clipRecs[rec1_slot], OFFSET_X(LtX1), OFFSET_Y(LtY1),
            ZOOMED_SIZE(RbX1-LtX1)+1, ZOOMED_SIZE(RbY1-LtY1)+1);
      SetRecVals(clipRecs[!rec1_slot], OFFSET_X(LtX2), OFFSET_Y(LtY2),
            ZOOMED_SIZE(RbX2-LtX2)+1, ZOOMED_SIZE(RbY2-LtY2)+1);
      numClipRecs = 2;
   } else {
      if (LtY1 < LtY2) {
         if (RbY1 <= LtY2) {  /* y-extents do not intersect */
            SetRecVals(clipRecs[0], OFFSET_X(LtX1), OFFSET_Y(LtY1),
                  ZOOMED_SIZE(RbX1-LtX1)+1, ZOOMED_SIZE(RbY1-LtY1)+1);
            SetRecVals(clipRecs[1], OFFSET_X(LtX2), OFFSET_Y(LtY2),
                  ZOOMED_SIZE(RbX2-LtX2)+1, ZOOMED_SIZE(RbY2-LtY2)+1);
            numClipRecs = 2;
         } else if (RbY1 >= RbY2) {
            /* box 2's y-extents is inside box 1's y-extents */
            rec1_slot = (LtX1 < LtX2) ? 0 : 1;  
            SetRecVals(clipRecs[rec1_slot], OFFSET_X(LtX1), OFFSET_Y(LtY1),
                  ZOOMED_SIZE(RbX1-LtX1)+1, ZOOMED_SIZE(RbY1-LtY1)+1);
            SetRecVals(clipRecs[!rec1_slot], OFFSET_X(LtX2), OFFSET_Y(LtY2),
                  ZOOMED_SIZE(RbX2-LtX2)+1, ZOOMED_SIZE(RbY2-LtY2)+1);
            numClipRecs = 2;
         } else {  
            SetRecVals(clipRecs[0], OFFSET_X(LtX1), OFFSET_Y(LtY1),
                  ZOOMED_SIZE(RbX1-LtX1)+1, ZOOMED_SIZE(LtY2-LtY1)+1);
            if (LtX1 < LtX2) {
               SetRecVals(clipRecs[1], OFFSET_X(LtX1), OFFSET_Y(LtY2),
                     ZOOMED_SIZE(RbX1-LtX1)+1, ZOOMED_SIZE(RbY1-LtY2)+1);
               SetRecVals(clipRecs[2], OFFSET_X(LtX2), OFFSET_Y(LtY2),
                     ZOOMED_SIZE(RbX2-LtX2)+1, ZOOMED_SIZE(RbY1-LtY2)+1);
            } else {
               SetRecVals(clipRecs[1], OFFSET_X(LtX2), OFFSET_Y(LtY2),
                     ZOOMED_SIZE(RbX2-LtX2)+1, ZOOMED_SIZE(RbY1-LtY2)+1);
               SetRecVals(clipRecs[2], OFFSET_X(LtX1), OFFSET_Y(LtY2),
                     ZOOMED_SIZE(RbX1-LtX1)+1, ZOOMED_SIZE(RbY1-LtY2)+1);
            }
            SetRecVals(clipRecs[3], OFFSET_X(LtX2), OFFSET_Y(RbY1),
                  ZOOMED_SIZE(RbX2-LtX2)+1, ZOOMED_SIZE(RbY2-RbY1)+1);
            numClipRecs = 4;
         }
      } else {
         if (RbY2 <= LtY1) {  /* y-extents do not intersect */
            SetRecVals(clipRecs[0], OFFSET_X(LtX2), OFFSET_Y(LtY2),
                  ZOOMED_SIZE(RbX2-LtX2)+1, ZOOMED_SIZE(RbY2-LtY2)+1);
            SetRecVals(clipRecs[1], OFFSET_X(LtX1), OFFSET_Y(LtY1),
                  ZOOMED_SIZE(RbX1-LtX1)+1, ZOOMED_SIZE(RbY1-LtY1)+1);
            numClipRecs = 2;
         } else if (RbY2 >= RbY1) {
            /* box 1's y-extents is inside box 2's y-extents */
            rec1_slot = (LtX1 < LtX2) ? 0 : 1;  
            SetRecVals(clipRecs[rec1_slot], OFFSET_X(LtX1), OFFSET_Y(LtY1),
                  ZOOMED_SIZE(RbX1-LtX1)+1, ZOOMED_SIZE(RbY1-LtY1)+1);
            SetRecVals(clipRecs[!rec1_slot], OFFSET_X(LtX2), OFFSET_Y(LtY2),
                  ZOOMED_SIZE(RbX2-LtX2)+1, ZOOMED_SIZE(RbY2-LtY2)+1);
            numClipRecs = 2;
         } else {  
            SetRecVals(clipRecs[0], OFFSET_X(LtX2), OFFSET_Y(LtY2),
                  ZOOMED_SIZE(RbX2-LtX2)+1, ZOOMED_SIZE(LtY1-LtY2)+1);
            if (LtX1 < LtX2) {
               SetRecVals(clipRecs[1], OFFSET_X(LtX1), OFFSET_Y(LtY1),
                     ZOOMED_SIZE(RbX1-LtX1)+1, ZOOMED_SIZE(RbY2-LtY1)+1);
               SetRecVals(clipRecs[2], OFFSET_X(LtX2), OFFSET_Y(LtY1),
                     ZOOMED_SIZE(RbX2-LtX2)+1, ZOOMED_SIZE(RbY2-LtY1)+1);
            } else {
               SetRecVals(clipRecs[1], OFFSET_X(LtX2), OFFSET_Y(LtY1),
                     ZOOMED_SIZE(RbX2-LtX2)+1, ZOOMED_SIZE(RbY2-LtY1)+1);
               SetRecVals(clipRecs[2], OFFSET_X(LtX1), OFFSET_Y(LtY1),
                     ZOOMED_SIZE(RbX1-LtX1)+1, ZOOMED_SIZE(RbY2-LtY1)+1);
            }
            SetRecVals(clipRecs[3], OFFSET_X(LtX1), OFFSET_Y(RbY2),
                  ZOOMED_SIZE(RbX1-LtX1)+1, ZOOMED_SIZE(RbY1-RbY2)+1);
            numClipRecs = 4;
         }
      }
   }
   clipOrdering = YXSorted;
   XSetClipRectangles(mainDisplay, drawGC, 0, 0, clipRecs, numClipRecs,
         clipOrdering);

   if ((paperWidth >= LtX1 && paperWidth <= RbX1) ||
         (paperHeight >= LtY1 && paperHeight <= RbY1) ||
         (paperWidth >= LtX2 && paperWidth <= RbX2) ||
         (paperHeight >= LtY2 && paperHeight <= RbY2)) {
      DrawPaperBoundary(execAnimatePixmap==None ? drawWindow :
            execAnimatePixmap);
   }
   if (execAnimatePixmap != None) {
      DrawGridLines(execAnimatePixmap, x1, y1, w1, h1);
      DrawPageLines(execAnimatePixmap, x1, y1, w1, h1);
   } else {
      DrawGridLines(drawWindow, x1, y1, w1, h1);
      DrawPageLines(drawWindow, x1, y1, w1, h1);
   }

   numRedrawBBox = 2;
   smallArea[0].ltx = LtX1; smallArea[0].lty = LtY1;
   smallArea[0].rbx = RbX1; smallArea[0].rby = RbY1;
   smallArea[1].ltx = LtX2; smallArea[1].lty = LtY2;
   smallArea[1].rbx = RbX2; smallArea[1].rby = RbY2;
   ShowInterrupt(DEF_CHECK_INTERVAL);
   for (obj_ptr = BotObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
      obj_ptr->tmp_parent = NULL;
      if (BBoxIntersect(obj_ptr->bbox, drawWinBBox) &&
            (BBoxIntersect(obj_ptr->bbox, bbox1) ||
            BBoxIntersect(obj_ptr->bbox, bbox2))) {
         if (!DrawObj(execAnimatePixmap==None ? drawWindow : execAnimatePixmap,
               obj_ptr)) {
            break;
         }
         if (execAnimatePixmap == None && CheckInterrupt()) {
            Msg("User interrupt.  Repaint aborted.");
            break;
         }
      }
   }
   HideInterrupt();
   SetDefaultDrawWinClipRecs();
   if (execAnimatePixmap != None && execAnimateRedraw) {
      XSetFunction(mainDisplay, drawGC, GXcopy);
      if (x1 < execAnimatePixmapW && y1 < execAnimatePixmapH) {
         int real_w=(x1+w1 >= execAnimatePixmapW ? execAnimatePixmapW-x1 : w1);
         int real_h=(y1+h1 >= execAnimatePixmapH ? execAnimatePixmapH-y1 : h1);

         XCopyArea(mainDisplay, execAnimatePixmap, drawWindow, drawGC,
               x1, y1, real_w, real_h, x1, y1);
      }
      if (x2 < execAnimatePixmapW && y2 < execAnimatePixmapH) {
         int real_w=(x2+w2 >= execAnimatePixmapW ? execAnimatePixmapW-x2 : w2);
         int real_h=(y2+h2 >= execAnimatePixmapH ? execAnimatePixmapH-y2 : h2);

         XCopyArea(mainDisplay, execAnimatePixmap, drawWindow, drawGC,
               x2, y2, real_w, real_h, x2, y2);
      }
   }
   if (redraw_cross_hair) RedrawCrossHair();
}

static
void GetBetterBBox(ObjPtr, LtX, LtY, RbX, RbY, AlreadyFound)
   struct ObjRec *ObjPtr;
   int *LtX, *LtY, *RbX, *RbY, *AlreadyFound;
{
   int found, style=INVALID, w=0, ltx, lty, rbx, rby;
   struct ObjRec *obj_ptr;
   struct AttrRec *attr_ptr;

   if (colorLayers &&
            ObjPtr->tmp_parent==NULL && !ObjInVisibleLayer(ObjPtr)) {
      return;
   }
   if (*AlreadyFound && ObjPtr->bbox.ltx >= *LtX && ObjPtr->bbox.lty >= *LtY &&
         ObjPtr->bbox.rbx <= *RbX && ObjPtr->bbox.rby <= *RbY) {
      return;
   }
   switch (ObjPtr->type) {
   case OBJ_POLY:
   case OBJ_ARC:
      switch (ObjPtr->type) {
      case OBJ_POLY:
         w = ObjPtr->detail.p->width;
         style = ObjPtr->detail.p->style;
         break;
      case OBJ_ARC:
         w = ObjPtr->detail.a->width;
         style = ObjPtr->detail.a->style;
         break;
      }
      if (style==LS_PLAIN && (w & 0x1)) {
         w = (w-1)>>1;
         ltx = ObjPtr->obbox.ltx-w; lty = ObjPtr->obbox.lty-w;
         rbx = ObjPtr->obbox.rbx+w; rby = ObjPtr->obbox.rby+w;
      } else {
         ltx = ObjPtr->bbox.ltx; lty = ObjPtr->bbox.lty;
         rbx = ObjPtr->bbox.rbx; rby = ObjPtr->bbox.rby;
      }
      break;
   case OBJ_BOX:
   case OBJ_OVAL:
   case OBJ_POLYGON:
   case OBJ_RCBOX:
      switch (ObjPtr->type) {
      case OBJ_BOX: w = ObjPtr->detail.b->width; break;
      case OBJ_OVAL: w = ObjPtr->detail.o->width; break;
      case OBJ_POLYGON: w = ObjPtr->detail.g->width; break;
      case OBJ_RCBOX: w = ObjPtr->detail.rcb->width; break;
      }
      if (w & 0x1) {
         w = (w-1)>>1;
         ltx = ObjPtr->obbox.ltx-w; lty = ObjPtr->obbox.lty-w;
         rbx = ObjPtr->obbox.rbx+w; rby = ObjPtr->obbox.rby+w;
      } else {
         ltx = ObjPtr->bbox.ltx; lty = ObjPtr->bbox.lty;
         rbx = ObjPtr->bbox.rbx; rby = ObjPtr->bbox.rby;
      }
      break;

   case OBJ_TEXT:
      ltx = ObjPtr->bbox.ltx; lty = ObjPtr->bbox.lty;
      rbx = ObjPtr->bbox.rbx; rby = ObjPtr->bbox.rby;
      break;

   case OBJ_XBM:
   case OBJ_XPM:
      ltx = ObjPtr->bbox.ltx;   lty = ObjPtr->bbox.lty;
      rbx = ObjPtr->bbox.rbx-1; rby = ObjPtr->bbox.rby-1;
      break;

   case OBJ_GROUP:
   case OBJ_ICON:
   case OBJ_SYM:
      found = FALSE;
      obj_ptr = ObjPtr->detail.r->last;
      for ( ; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
         obj_ptr->tmp_parent = ObjPtr;
         GetBetterBBox(obj_ptr, &ltx, &lty, &rbx, &rby, &found);
      }
      if (!found) {
         return;
      }
      break;
   }
   for (attr_ptr=ObjPtr->lattr; attr_ptr!=NULL; attr_ptr=attr_ptr->prev) {
      if (attr_ptr->shown) {
         if (attr_ptr->obj->bbox.ltx < ltx) ltx = attr_ptr->obj->bbox.ltx;
         if (attr_ptr->obj->bbox.lty < lty) lty = attr_ptr->obj->bbox.lty;
         if (attr_ptr->obj->bbox.rbx > rbx) rbx = attr_ptr->obj->bbox.rbx;
         if (attr_ptr->obj->bbox.rby > rby) rby = attr_ptr->obj->bbox.rby;
      }
   }
   if (ObjPtr->type == OBJ_SYM) {
      if (ObjPtr->obbox.ltx-QUARTER_INCH < ltx) {
         ltx = ObjPtr->obbox.ltx - QUARTER_INCH;
      }
      if (ObjPtr->obbox.lty-QUARTER_INCH < lty) {
         lty = ObjPtr->obbox.lty - QUARTER_INCH;
      }
      if (ObjPtr->obbox.rbx+QUARTER_INCH > rbx) {
         rbx = ObjPtr->obbox.rbx + QUARTER_INCH;
      }
      if (ObjPtr->obbox.rby+QUARTER_INCH > rby) {
         rby = ObjPtr->obbox.rby + QUARTER_INCH;
      }
   }
   if (*AlreadyFound) {
      if (ltx < *LtX) *LtX = ltx; if (lty < *LtY) *LtY = lty;
      if (rbx > *RbX) *RbX = rbx; if (rby > *RbY) *RbY = rby;
   } else {
      *LtX = ltx; *LtY = lty; *RbX = rbx; *RbY = rby;
   }
   *AlreadyFound = TRUE;
}

void RedrawDrawWindow(BotObj)
   struct ObjRec *BotObj;
{
   register struct ObjRec *obj_ptr;

   if (!skipCrossHair) {
      RedrawCrossHair();
   }
   if (execAnimating) {
      int already_animating=(execAnimatePixmap!=None);

      if (execAnimatePixmap != None) {
         XFreePixmap(mainDisplay, execAnimatePixmap);
         execAnimatePixmap = None;
      }
      execAnimatePixmapW = ZOOMED_SIZE(drawWinW);
      execAnimatePixmapH = ZOOMED_SIZE(drawWinH);
      execAnimatePixmap = XCreatePixmap(mainDisplay, mainWindow,
            execAnimatePixmapW, execAnimatePixmapH, mainDepth);
      if (execAnimatePixmap == None) {
         if (already_animating) {
            sprintf(gszMsgBox,
                  "Fail to create pixmap of size [%1dx%1d].\n\n%s.",
                  execAnimatePixmapW, execAnimatePixmapH,
                  "Can not cache animation");
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         }
         execAnimatePixmapW = execAnimatePixmapH = 0;
      } else {
         XGCValues values;

         values.foreground = myBgPixel;
         values.function = GXcopy;
         values.fill_style = FillSolid;
         XChangeGC(mainDisplay, drawGC,
               GCForeground | GCFunction | GCFillStyle, &values);
         XFillRectangle(mainDisplay, execAnimatePixmap, drawGC,
               0, 0, execAnimatePixmapW, execAnimatePixmapH);
      }
   }
   DrawPaperBoundary(execAnimatePixmap==None ? drawWindow : execAnimatePixmap);
   RedrawGridLines(execAnimatePixmap==None ? drawWindow : execAnimatePixmap);
   RedrawPageLines(execAnimatePixmap==None ? drawWindow : execAnimatePixmap);

   numRedrawBBox = 0;
   ShowInterrupt(DEF_CHECK_INTERVAL);
   for (obj_ptr = BotObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
      obj_ptr->tmp_parent = NULL;
      if (BBoxIntersect(obj_ptr->bbox, drawWinBBox)) {
         if (!DrawObj(execAnimatePixmap==None ? drawWindow : execAnimatePixmap,
               obj_ptr)) {
            break;
         }
         if (execAnimatePixmap == None && CheckInterrupt()) {
            Msg("User interrupt.  Repaint aborted.");
            break;
         }
      }
   }
   HideInterrupt();
   if (execAnimatePixmap != None) {
      XSetFunction(mainDisplay, drawGC, GXcopy);
      XCopyArea(mainDisplay, execAnimatePixmap, drawWindow, drawGC,
            0, 0, execAnimatePixmapW, execAnimatePixmapH, 0, 0);
   }
   if (!skipCrossHair) {
      RedrawCrossHair();
   }
}

Pixmap DrawAllOnPixmap(LtX, LtY, W, H)
   int *LtX, *LtY, *W, *H;
{
   register struct ObjRec *obj_ptr;
   int ltx, lty, rbx, rby, w, h, saved_zoom_scale, saved_zoomed_in, found=FALSE;
   int saved_draw_orig_x, saved_draw_orig_y, saved_draw_win_w, saved_draw_win_h;
   Pixmap pixmap;
   XGCValues values;

   ltx = lty = rbx = rby = 0;
   for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
      obj_ptr->tmp_parent = NULL;
      GetBetterBBox(obj_ptr, &ltx, &lty, &rbx, &rby, &found);
   }
   if (!found) {
      *LtX = *LtY = *W = *H = 0;
      sprintf(gszMsgBox, "No objects to print!");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return None;
   }
   *W = w = rbx - ltx + 1;
   *H = h = rby - lty + 1;
   *LtX = ltx;
   *LtY = lty;

   saved_draw_orig_x = drawOrigX; saved_draw_orig_y = drawOrigY;
   saved_draw_win_w = drawWinW; saved_draw_win_h = drawWinH;
   saved_zoom_scale = zoomScale;
   saved_zoomed_in = zoomedIn;

   drawOrigX = ltx; drawOrigY = lty;
   drawWinW = w; drawWinH = h;
   zoomScale = 0;
   zoomedIn = FALSE;

   SetDefaultDrawWinClipRecs();

   pixmap = XCreatePixmap(mainDisplay, mainWindow, w, h, mainDepth);

   if (pixmap == None) {
      sprintf(gszMsgBox, "Can not allocate pixmap of size %1dx%1d.", w, h);
      Msg(gszMsgBox);
      return (None);
   }

   values.foreground = myBgPixel;
   values.function = GXcopy;
   values.fill_style = FillSolid;
   XChangeGC (mainDisplay, drawGC,
         GCForeground | GCFunction | GCFillStyle, &values);
   XFillRectangle (mainDisplay, pixmap, drawGC, 0, 0, w, h);

   AdjCaches ();
   AdjSplineVs ();

   checkBBox = FALSE;
   ShowInterrupt (DEF_CHECK_INTERVAL);
   for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev)
   {
      obj_ptr->tmp_parent = NULL;
      if (!DrawObj (pixmap, obj_ptr))
      {
         XFreePixmap (mainDisplay, pixmap);
         pixmap = None;
         break;
      }
      if (execAnimatePixmap == None && CheckInterrupt ())
      {
         Msg ("User interrupt.  Repaint aborted.");
         XFreePixmap (mainDisplay, pixmap);
         pixmap = None;
         break;
      }
   }
   HideInterrupt();
   checkBBox = TRUE;

   drawOrigX = saved_draw_orig_x; drawOrigY = saved_draw_orig_y;
   drawWinW = saved_draw_win_w; drawWinH = saved_draw_win_h;
   zoomedIn = saved_zoomed_in;
   zoomScale = saved_zoom_scale;

   AdjSplineVs ();
   AdjCaches ();
   SetDefaultDrawWinClipRecs ();

   skipCrossHair = TRUE;
   RedrawDrawWindow(botObj);
   skipCrossHair = FALSE;
   return (pixmap);
}

void ClearAndRedrawDrawWindow()
{
   XClearWindow(mainDisplay, drawWindow);
   if (execAnimatePixmap != None) {
      XGCValues values;

      values.foreground = myBgPixel;
      values.function = GXcopy;
      values.fill_style = FillSolid;
      XChangeGC(mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle, &values);
      XFillRectangle(mainDisplay, execAnimatePixmap, drawGC,
            0, 0, execAnimatePixmapW, execAnimatePixmapH);
   }
   somethingHighLighted = FALSE;
   skipCrossHair = TRUE;
   RedrawDrawWindow(botObj);
   skipCrossHair = FALSE;
   RedrawCurText();
   if (!execAnimating) HighLightForward();
   RedrawCrossHair();
}

void ClearAndRedrawDrawWindowNoCurT()
   /* use to be ClearAndRedrawDrawWindowDontDrawCurText() */
{
   XClearWindow(mainDisplay, drawWindow);
   if (execAnimatePixmap != None) {
      XGCValues values;

      values.foreground = myBgPixel;
      values.function = GXcopy;
      values.fill_style = FillSolid;
      XChangeGC(mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle, &values);
      XFillRectangle(mainDisplay, execAnimatePixmap, drawGC,
            0, 0, execAnimatePixmapW, execAnimatePixmapH);
   }
   somethingHighLighted = FALSE;
   skipCrossHair = TRUE;
   RedrawDrawWindow(botObj);
   skipCrossHair = FALSE;
   HighLightForward();
   RedrawCrossHair();
}

int BeginExecAnimate()
{
   execAnimating = TRUE;
   execAnimateRedraw = TRUE;
   RedrawDrawWindow(botObj);

   if (execAnimatePixmap == None) {
      sprintf(gszMsgBox, "Fail to create pixmap of size [%1dx%1d].\n\n%s.",
            execAnimatePixmapW, execAnimatePixmapH, "Can not begin animation");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      execAnimatePixmapW = execAnimatePixmapH = 0;
      execAnimating = execAnimateRedraw = FALSE;
      return FALSE;
   }
   return TRUE;
}

void EndExecAnimate()
{
   if (execAnimatePixmap != None) {
      XFreePixmap(mainDisplay, execAnimatePixmap);
      execAnimatePixmap = None;
   }
   execAnimatePixmapW = execAnimatePixmapH = 0;
   execAnimating = execAnimateRedraw = FALSE;
}

void CleanUpDrawingWindow ()
{
   if (execAnimatePixmap != None) {
      Msg("Forcing an end_animate().");
      EndExecAnimate();
   }
   SetCurChoice (NOTHING);
   if (topSel != NULL) { HighLightReverse (); RemoveAllSel (); }
   if (tgifObj != NULL && tgifObj->fattr != NULL)
   {
      DelAllAttrs (tgifObj->fattr);
      tgifObj->fattr = tgifObj->lattr = NULL;
   }
   DelAllPages ();
}

static
int DoShortCut(name, key_sym, state, args)
   char *name, *args;
   KeySym key_sym;
   unsigned int state;
{
   if ((state & ControlMask) && (!(state & Mod1Mask))) {
      switch (key_sym&0xff) {
      case 'a': /*^a*/ SelAllObj(TRUE); break;
      case 'b': /*^b*/ BackProc(); break;
      case 'c': /*^c*/ ChangeDomain(); break;
      case 'd': /*^d*/ DupSelObj(); break;
      case 'e': /*^e*/ PushCurChoice(); break;
      case 'f': /*^f*/ FrontProc(); break;
      case 'g': /*^g*/ GroupSelObj(); break;
      case 'h': /*^h*/ return(INVALID);
      case 'i': /*^i*/ Instantiate(); break;
      case 'j': /*^j*/ return(INVALID);
      case 'k': /*^k*/ PopIcon(); break;
      case 'l': /*^l*/ AlignSelObjs(); break;
      case 'm': /*^m*/ return(INVALID);
      case 'n': /*^n*/ NewProc(); break;
      case 'o': /*^o*/ OpenProc(); break;
      case 'p': /*^p*/ Dump(""); break;
      case 'q': /*^q*/ return QuitProc();
      case 'r': /*^r*/ ClearAndRedrawDrawWindow(); break;
      case 's': /*^s*/ SaveFile(); break;
      case 't': /*^t*/ AlignSelToGrid(); break;
      case 'u': /*^u*/ UngroupSelObj(); break;
      case 'v': /*^v*/ PushIcon(); break;
      case 'w': /*^w*/ SetCurChoice(DRAWTEXT); break;
      case 'x': /*^x*/ DelAllSelObj(); break;
      case 'y': /*^y*/ if (!CopyToCutBuffer()) return BAD; break;
      case 'z': /*^z*/ return AnimateProc();
      case ',': /*^,*/ ScrollLeft(NULL); break;
      case '.': /*^.*/ ScrollRight(NULL); break;
      case '-': /*^-*/ PrintWithCommand(""); break;
      }
   } else if ((state & Mod1Mask) && (!(state & ControlMask))) {
      switch (key_sym&0xff) {
      case 'a': /*#a*/ AddAttrs(); break;
      case 'b': /*#b*/ return ProbeProc();
      case 'c': /*#c*/ RotateCounter(); break;
      case 'd': /*#d*/ DecGrid(); break;
      case 'e': /*#e*/ AnimateSel(); break;
      case 'f': /*#f*/ FlashSelColor(); break;
      case 'g': /*#f*/ ToggleGridShown(); break;
      case 'h': /*#h*/ FlipHorizontal(); break;
      case 'i': /*#i*/ IncGrid(); break;
      case 'j': /*#j*/ HideAllAttrNames(); break;
      case 'k': /*#k*/ SetCurChoice(NOTHING); break;
      case 'l': /*#l*/ DistrSelObjs(); break;
      case 'm': /*#m*/ MoveAttr(); break;
      case 'n': /*#n*/ ShowAllAttrNames(); break;
      case 'o': /*#o*/ ZoomOut(); break;
      case 'p': /*#p*/ ImportFile(); break;
      case 'q': /*#q*/ SetCurChoice(DRAWPOLY); break;
      case 'r': /*#r*/ SetCurChoice(DRAWBOX); break;
      case 's': /*#s*/ return SolveProc();
      case 't': /*#t*/ DetachAttrs(); break;
      case 'u': /*#u*/ UndoCmd(); break;
      case 'v': /*#v*/ FlipVertical(); break;
      case 'w': /*#w*/ RotateClockWise(); break;
      case 'x': /*#x*/ return EscapeProc();
      case 'y': /*#y*/ return SimulateProc();
      case 'z': /*#z*/ ZoomIn(); break;
      case '9': /*#9*/ MakePreciseArc(); break;
      case '0': /*#0*/ UpdateSelObjs(); break;
      case ',': /*#,*/ ScrollUp(NULL); break;
      case '.': /*#.*/ ScrollDown(NULL); break;
      case '-': /*#-*/ ShowAllAttrs(); break;
      case '{': /*#{*/ AlignObjsTop(); break;
      case '+': /*#+*/ AlignObjsMiddle(); break;
      case '}': /*#}*/ AlignObjsBottom(); break;
      case '[': /*#[*/ AlignObjsLeft(); break;
      case '=': /*#=*/ AlignObjsCenter(); break;
      case ']': /*#]*/ AlignObjsRight(); break;
      case '"': /*#"*/ MakeRegularPolygon(); break;
      case '%': /*#%*/ SetPrintReduction(); break;
      case ':': /*#:*/ DefaultZoom(); break;
      case '`': /*#`*/ ZoomWayOut(); break;
      case '~': /*#~*/ SaveNewFile(TRUE); break;
      case ';': /*#;*/ CutMaps(); break;
      case '_': /*#_*/ AbutHorizontal(); break;
      case '|': /*#|*/ AbutVertical(); break;
      case '#': /*##*/ BreakUpText(); break;
      case '^': /*#^*/ ScrollToOrigin(); break;
      case '@': /*#@*/ ToggleMoveMode(); break;
      case '$': /*#$*/ SetCurChoice(VERTEXMODE); break;
      case '&': /*#&*/ AlignSelToPage(); break;
      case '*': /*#**/ RedoCmd(); break;
      case '(': /*#(*/ ImportEPSFile(FALSE); break;
      case ')': /*#)*/ ScaleAllSelObj(); break;
      case '<': /*#<*/ LockSelObj(); break;
      case '>': /*#>*/ UnlockSelObj(); break;
      }
   } else if ((state & Mod1Mask) && (state & ControlMask)) {
      switch (key_sym&0xff) {
      case 'a': /*^#a*/ AddPoint(); break;
      case 'b': /*^#b*/ ChangeFontStyle(STYLE_BR); break;
      case 'c': /*^#c*/ ChangeFontJust(JUST_C); break;
      case 'd': /*^#d*/ DeletePoint(); break;
      case 'e': /*^#e*/ SetCurChoice(DRAWRCBOX); break;
      case 'f': /*^#f*/ InvertXBitmaps(); break;
      case 'g': /*^#g*/ ToggleSnapOn(); break;
      case 'h': /*^#h*/ HideAllAttrs(); break;
      case 'i': /*^#i*/ MakeIconic(); break;
      case 'j': /*^#j*/ UnMakeIconic(); break;
      case 'k': /*^#k*/ ToggleColorPostScript(); break;
      case 'l': /*^#l*/ ChangeFontJust(JUST_L); break;
      case 'm': /*^#m*/ MakeSymbolic(); break;
      case 'n': /*^#n*/ UnMakeSymbolic(); break;
      case 'o': /*^#o*/ ChangeFontStyle(STYLE_NR); break;
      case 'p': /*^#p*/ ChangeFontStyle(STYLE_BI); break;
      case 'q': /*^#q*/ SetCurChoice(DRAWPOLYGON); break;
      case 'r': /*^#r*/ ChangeFontJust(JUST_R); break;
      case 's': /*^#s*/ SaveNewFile(FALSE); break;
      case 't': /*^#t*/ ChangeFontStyle(STYLE_NI); break;
      case 'u': /*^#u*/ UpdateSymbols(); break;
      case 'v': /*^#v*/ SetCurChoice(DRAWCIRCLE); break;
      case 'w': /*^#w*/ ToggleAllSelLineType(); break;
      case 'x': /*^#x*/ ToggleWhereToPrint(); break;
      case 'y': /*^#y*/ if (!PasteFromCutBuffer()) return(BAD); break;
      case 'z': /*^#z*/ SetCurChoice(DRAWARC); break;
      case '.': /*^#.*/ ImportXBitmapFile(); break;
      case ',': /*^#,*/ ImportXPixmapFile(); break;
      case '-': /*^#-*/ ToggleGridSystem(); break;
      }
   } else if (name != NULL && key_sym == '\0' && state == 0) {
      XButtonEvent button_ev;

      button_ev.state = ShiftMask;
      if (strcmp(name, "ScrollPageUp()") == 0) {
         ScrollUp (&button_ev);
      } else if (strcmp(name, "ScrollPageDown()") == 0) {
         ScrollDown (&button_ev);
      } else if (strcmp(name, "ScrollPageRight()") == 0) {
         ScrollRight (&button_ev);
      } else if (strcmp(name, "ScrollPageLeft()") == 0) {
         ScrollLeft (&button_ev);
      } else if (strcmp(name, "FlushUndoBuffer()") == 0) {
         FlushUndoBuffer();
      } else if (strcmp(name, "PrintMsgBuffer()") == 0) {
         PrintMsgBuffer();
      } else if (strcmp(name, "SaveOrigin()") == 0) {
         SaveOrigin();
      } else if (strcmp(name, "RestoreImageWH()") == 0) {
         RestoreImageWH();
      } else if (strcmp(name, "UpdateEPS()") == 0) {
         UpdateEPS();
      } else if (strcmp(name, "ToggleMapShown()") == 0) {
         ToggleMapShown();
      } else if (strcmp(name, "ToggleUseGrayScale()") == 0) {
         ToggleUseGray();
      } else if (strcmp(name, "FreeHandMode()") == 0) {
         SetCurChoice (FREEHAND);
      } else if (strcmp(name, "SaveSymInLibrary()") == 0) {
         SaveSymInLibrary();
      } else if (strcmp(name, "CenterAnEndPoint()") == 0 ||
            strcmp(name, "CenterAVertex()") == 0) {
         CenterAnEndPoint();
      } else if (strcmp(name, "NextPage()") == 0) {
         NextPage();
      } else if (strcmp(name, "PrevPage()") == 0) {
         PrevPage();
      } else if (strcmp(name, "NamePages()") == 0) {
         NamePages();
      } else if (strcmp(name, "GotoPage()") == 0) {
         GotoPage();
      } else if (strcmp(name, "AddPageBefore()") == 0) {
         AddPageBefore();
      } else if (strcmp(name, "AddPageAfter()") == 0) {
         AddPageAfter();
      } else if (strcmp(name, "DeleteCurPage()") == 0) {
         DeleteCurPage();
      } else if (strcmp(name, "TogglePageLineShown()") == 0) {
         TogglePageLineShown();
      } else if (strcmp(name, "SpecifyDrawingSize()") == 0) {
         SpecifyDrawingSize();
      } else if (strcmp(name, "PrintOnePage()") == 0) {
         PrintOnePage();
      } else if (strcmp(name, "ToggleNamedAttrShown()") == 0) {
         if (args != NULL) {
            ToggleNamedAttrShown(args);
         }
      } else if (strcmp(name, "AttachFileAttrs()") == 0) {
         AddFileAttrs();
      } else if (strcmp(name, "DetachFileAttrs()") == 0) {
         DetachFileAttrs();
      } else if (strcmp(name, "EditFileAttrs()") == 0) {
         EditFileAttrs();
      } else if (strcmp(name, "PrintSelectedObjs()") == 0) {
         PrintSelectedObjs();
      } else if (strcmp(name, "InputPolyPts()") == 0) {
         InputPolyPts();
      } else if (strcmp(name, "InputPolygonPts()") == 0) {
         InputPolygonPts();
      } else if (strcmp(name, "EditAttrs()") == 0) {
         EditAttrs();
      } else if (strcmp(name, "ConvertIntSpline()") == 0) {
         ConvertIntSpline();
      } else if (strcmp(name, "PasteFromFile()") == 0) {
         if (!PasteFromFile()) return (INVALID);
      } else if (strcmp(name, "ToggleShowMeasurement()") == 0) {
         ToggleShowMeasurement();
      } else if (strcmp(name, "SetMeasureUnit()") == 0) {
         SetMeasureUnit();
      } else if (strcmp(name, "Cut()") == 0) {
         CutToCutBuffer();
      } else if (strcmp(name, "ToggleSmoothHinge()") == 0) {
         ToggleSmoothHinge();
      } else if (strcmp(name, "ToggleShowMenubar()") == 0) {
         ToggleShowMenubar();
      } else if (strcmp(name, "ToggleShowStatus()") == 0) {
         ToggleShowStatus();
      } else if (strcmp(name, "BrowseXBitmap()") == 0) {
         BrowseXBitmap();
      } else if (strcmp(name, "BrowseXPixmap()") == 0) {
         BrowseXPixmap();
      } else if (strcmp(name, "SpecifyPaperSize()") == 0) {
         SpecifyPaperSize();
      } else if (strcmp(name, "ToggleOneMotionSelMove()") == 0) {
         ToggleOneMotionSelectMove();
      } else if (strcmp(name, "GoBack()") == 0) {
         NavigateBack();
      } else if (strcmp(name, "GoForward()") == 0) {
         NavigateForward();
      } else if (strcmp(name, "RefreshCurrent()") == 0) {
         NavigateRefresh();
      } else if (strcmp(name, "HotList()") == 0) {
         NavigateHotList();
      } else if (strcmp(name, "AddCurrentToHotList()") == 0) {
         NavigateAddToHotList();
      } else if (strcmp(name, "SessionHistory()") == 0) {
         NavigateSessionHistory();
      } else if (strcmp(name, "ToggleHyperSpace()") == 0) {
         ToggleHyperSpace (FALSE);
      } else if (strcmp(name, "EmbedEPSFile()") == 0) {
         ImportEPSFile (TRUE);
      } else if (strcmp(name, "SetSelLineWidth()") == 0) {
         SetSelectedLineWidth();
      } else if (strcmp(name, "AddColor()") == 0) {
         AddColor();
      } else if (strcmp(name, "ImportAttrs()") == 0) {
         ImportAttrs();
      } else if (strcmp(name, "ExportAttrs()") == 0) {
         ExportAttrs();
      } else if (strcmp(name, "MergeWithTable()") == 0) {
         MergeWithTable();
      } else if (strcmp(name, "ExportToTable()") == 0) {
         ExportToTable();
      } else if (strcmp(name, "DeletePages()") == 0) {
         DeletePages();
      } else if (strcmp(name, "PrintOneFilePerPage()") == 0) {
         PrintOneFilePerPage();
      } else if (strcmp(name, "ImportGIFFile()") == 0) {
         ImportGIFFile();
      } else if (strcmp(name, "SetExportPixelTrim()") == 0) {
         SetExportPixelTrim (NULL);
#ifdef _TGIF_WB
      } else if (strcmp(name, "WhiteBoard()") == 0) {
         WhiteBoard();
#endif /* _TGIF_WB */
      } else if (strcmp(name, "ToggleColorLayers()") == 0) {
         ToggleColorLayers();
      } else if (strcmp(name, "ToggleStretchableText()") == 0) {
         ToggleStretchableText();
      } else if (strcmp(name, "BreakUpBit/Pixmap()") == 0) {
         BreakUpMaps();
      } else if (strcmp(name, "LayoutOnArc()") == 0) {
         LayoutOnArc();
      } else if (strcmp(name, "PreciseRotate()") == 0) {
         PreciseRotate();
      } else if (strcmp(name, "JoinPoly()") == 0) {
         JoinPoly();
      } else if (strcmp(name, "CutPoly()") == 0) {
         CutPoly();
      } else if (strcmp(name, "GetBoundingBox()") == 0) {
         GetBoundingBox();
      } else if (strcmp(name, "SetTemplate()") == 0) {
         SetTemplate();
      } else if (strcmp(name, "MakeGray()") == 0) {
         MakeGray();
      } else if (strcmp(name, "InvertColor()") == 0) {
         InvertColor();
      } else if (strcmp(name, "InterpolateColor()") == 0) {
         InterpolateColor();
      } else if (strcmp(name, "BrightenDarken()") == 0) {
         BrightenDarken();
      } else if (strcmp(name, "ChangeSaturation()") == 0) {
         ChangeSaturation();
      } else if (strcmp(name, "ChangeHue()") == 0) {
         ChangeHue();
      } else if (strcmp(name, "ContrastEnhance()") == 0) {
         ContrastEnhance();
      } else if (strcmp(name, "ColorBalance()") == 0) {
         ColorBalance();
      } else if (strcmp(name, "Gamma()") == 0) {
         Gamma();
      } else if (strcmp(name, "EdgeDetect()") == 0) {
         EdgeDetect();
      } else if (strcmp(name, "Emboss()") == 0) {
         Emboss();
      } else if (strcmp(name, "ReduceColors()") == 0) {
         ReduceColors();
      } else if (strcmp(name, "ReduceToPixmapColors()") == 0) {
         ReduceToPixmapColors();
      } else if (strcmp(name, "SetDefaultColorLevels()") == 0) {
         SetDefaultColorLevels();
      } else if (strcmp(name, "ReduceToDefaultColors()") == 0) {
         ReduceToDefaultColors();
      } else if (strcmp(name, "DefaultErrorDiffuse()") == 0) {
         DefaultErrorDiffuse();
      } else if (strcmp(name, "Spread()") == 0) {
         Spread();
      } else if (strcmp(name, "Sharpen()") == 0) {
         Sharpen();
      } else if (strcmp(name, "Blur3()") == 0) {
         Blur3();
      } else if (strcmp(name, "Blur5()") == 0) {
         Blur5();
      } else if (strcmp(name, "Blur7()") == 0) {
         Blur7();
      } else if (strcmp(name, "RunBggen()") == 0) {
         RunBggen();
      } else if (strcmp(name, "CircularBggen()") == 0) {
         CircularBggen();
      } else if (strcmp(name, "RegenerateImage()") == 0) {
         RegenerateImage();
      } else if (strcmp(name, "CropImage()") == 0) {
         CropImage();
      } else if (strcmp(name, "GetColor()") == 0) {
         GetColor();
      } else if (strcmp(name, "ReplaceColor()") == 0) {
         ReplaceColor();
      } else if (strcmp(name, "FloodFill()") == 0) {
         FloodFill();
      } else if (strcmp(name, "CreateContour()") == 0) {
         CreateContour();
      } else if (strcmp(name, "AlphaCombine()") == 0) {
         AlphaCombine();
      } else if (strcmp(name, "ImportOtherFile()") == 0) {
         ImportOtherFile();
      } else if (strcmp(name, "ImportOtherFileType()") == 0) {
         if (args != NULL) {
            ImportOtherFileType(args);
         }
      } else if (strcmp(name, "BrowseOther()") == 0) {
         BrowseOther();
      } else if (strcmp(name, "BrowseOtherType()") == 0) {
         if (args != NULL) {
            BrowseOtherType(args);
         }
      } else if (strcmp(name, "ToggleShowCrossHair()") == 0) {
         ToggleShowCrossHair();
      }
   }
   return BAD;
}

int ShortHand(input)
   XEvent *input;
   /* returns BAD if the character is a <CONTROL> or a <META> character */
   /* returns INVALID if the character is a normal character */
   /* otherwise, returns the value of sub-functions, such as QuitProc () */
{
   register int i;
   char buf[80], *name=NULL, args[MAXSTRING+1];
   int valid_shortcut=FALSE, have_ch;
   KeySym key_sym=(KeySym)0;
   XKeyEvent *key_ev;

   key_ev = &(input->xkey);
   have_ch = XLookupString (key_ev, buf, 80-1, &key_sym, &c_stat);
   TranslateKeys (buf, &key_sym);

   *args = '\0';

   if (key_sym>='\040' && key_sym<='\177' &&
         (key_ev->state & (ControlMask | Mod1Mask)))
      valid_shortcut = TRUE;
   else if (((key_sym>'\040' && key_sym<='\177') ||
         (key_sym>0xa0 && key_sym<=0xff)) &&
         !(key_ev->state & (ControlMask | Mod1Mask)) &&
         curChoice != DRAWTEXT)
   {
      char code;
      unsigned int state;

      for (i = 0; i < numExtraWins; i++) {
         if (key_ev->window == extraWinInfo[i].window &&
               extraWinInfo[i].window != None) {
            break;
         }
      }
      if (i == numExtraWins) {
         valid_shortcut = FetchShortCut((int)(key_sym&0xff),
               &code, &state, &name, args);
         if (valid_shortcut) {
            key_sym = code;
            key_ev->state = state;
         }
      }
   }

   if (valid_shortcut)
   {
      Msg ("");
      return DoShortCut(name, key_sym, key_ev->state, args);
   }
   return (INVALID);
}

int CallShortCut(name, argc, argv, code, state)
   char *name, *argv[], *code;
   int argc;
   unsigned int state;
{
   DoShortCut(name, (KeySym)(*code), state, (argc<=1 ? NULL : argv[1]));
   return TRUE;
}

int SomethingDirty()
{
   register struct ObjRec *obj_ptr=topObj;

   for ( ; obj_ptr != NULL; obj_ptr = obj_ptr->next) {
      if (obj_ptr->dirty) {
         return TRUE;
      }
   }
   return FALSE;
}

int DrawingEventHandler(input)
   XEvent *input;
{
   int mouse_x, mouse_y, grid_x, grid_y;
   XEvent ev;
   XButtonEvent *button_ev;

   if (input->type == Expose)
   {
      XSync (mainDisplay, False);
      while (XCheckWindowEvent (mainDisplay, drawWindow, ExposureMask, &ev)) ;

      if (topSel != NULL || curChoice == VERTEXMODE || SomethingDirty ())
         ClearAndRedrawDrawWindow ();
      else
      {
         RedrawDrawWindow (botObj);
         RedrawCurText ();
      }
      return (INVALID);
   }
   else if (input->type == EnterNotify)
   {
      switch (curChoice)
      {
         case NOTHING:
            if (inHyperSpace)
               SetMouseStatus ("(none)", "Main Menu", "Mode Menu");
            else
               SetMouseStatus ("select/move/resize objects", "Main Menu",
                     "Mode Menu");
            break;
         case DRAWTEXT:
            SetMouseStatus ("enter text", "Main Menu", "Mode Menu");
            break;
         case DRAWBOX:
            SetMouseStatus ("draw rectangles", "Main Menu", "Mode Menu");
            break;
         case DRAWCIRCLE:
            SetMouseStatus ("draw ovals", "Main Menu", "Mode Menu");
            break;
         case DRAWPOLY:
            SetMouseStatus ("draw poly/open splines", "Main Menu", "Mode Menu");
            break;
         case DRAWPOLYGON:
            SetMouseStatus ("draw polygon/closed splines", "Main Menu",
                  "Mode Menu");
            break;
         case DRAWARC:
            SetMouseStatus ("draw arcs", "Main Menu", "Mode Menu");
            break;
         case DRAWRCBOX:
            SetMouseStatus ("draw rcboxes", "Main Menu", "Mode Menu");
            break;
         case FREEHAND:
            SetMouseStatus ("freehand poly/open splines", "Main Menu",
                 "Mode Menu");
            break;
         case VERTEXMODE:
            SetMouseStatus ("select/move vertices", "Main Menu", "Mode Menu");
            break;
      }
   }
   else if (input->type == MotionNotify)
   {
      while (XCheckWindowEvent (mainDisplay,drawWindow,PointerMotionMask,&ev)) ;

      mouse_x = (input->xmotion).x;
      mouse_y = (input->xmotion).y;
      GridXY (mouse_x, mouse_y, &grid_x, &grid_y);
      MarkRulers (grid_x, grid_y);
      if (inHyperSpace) {
         struct ObjRec *obj_ptr, *owner_obj;
         struct AttrRec *attr_ptr;

         if ((obj_ptr=FindAnObj(mouse_x, mouse_y, &owner_obj, NULL, NULL)) !=
               NULL) {
            if (owner_obj != NULL) obj_ptr = owner_obj;
            if ((attr_ptr=FindAttrWithName(obj_ptr, TELEPORT_ATTR, NULL)) !=
                  NULL || (attr_ptr=FindAttrWithName(obj_ptr, "href=",
                  NULL)) != NULL)
            {
               char fname[MAXPATHLENGTH+1];

               SetHyperSpaceCursor (drawWindow);
               if (FormNewFileName (curDir, attr_ptr->attr_value.s,
                     (strcmp(attr_ptr->attr_name.s,TELEPORT_ATTR)==0 ?
                     OBJ_FILE_EXT : NULL), fname, NULL))
                  SetStringStatus (fname);
            }
            else if ((allowLaunchInHyperSpace &&
                  (attr_ptr=FindAttrWithName(obj_ptr, LAUNCH_ATTR, NULL)) !=
                  NULL) || (attr_ptr=FindAttrWithName(obj_ptr, EXEC_ATTR,
                  NULL)) != NULL)
            {
               SetHyperSpaceCursor (drawWindow);
               sprintf (gszMsgBox, "%s%s", attr_ptr->attr_name.s,
                     (*attr_ptr->attr_value.s=='\0' ? "..." :
                     attr_ptr->attr_value.s));
               SetStringStatus (gszMsgBox);
            }
            else
            {
               ShowCursor ();
               SetMouseStatus ("(none)", "Main Menu", "Mode Menu");
            }
         }
         else
         {
            ShowCursor ();
            SetMouseStatus ("(none)", "Main Menu", "Mode Menu");
         }
      } else if (curChoice == DRAWPOLY && drawPolyToEndInANode) {
         int need_to_highlight=FALSE, something_changed=FALSE;
         struct ObjRec *owner_obj=NULL, *obj_ptr, *obj_under_cursor=NULL;
         struct AttrRec *attr_ptr;

         obj_ptr = FindAnObj(mouse_x, mouse_y, &owner_obj, &obj_under_cursor,
               drawPolyFirstNodeName);
         if (drawPolyHighlightedNode != NULL) {
            if (obj_under_cursor != drawPolyHighlightedNode) {
               /* un-highlight */
               SelBox(drawWindow, revGrayGC,
                     OFFSET_X(drawPolyHighlightedNode->bbox.ltx)-2,
                     OFFSET_Y(drawPolyHighlightedNode->bbox.lty)-2,
                     OFFSET_X(drawPolyHighlightedNode->bbox.rbx)+2,
                     OFFSET_Y(drawPolyHighlightedNode->bbox.rby)+2);
               if (obj_under_cursor != NULL &&
                     (attr_ptr=FindAttrWithName(obj_under_cursor, "type=",
                     NULL)) != NULL && strcmp(attr_ptr->attr_value.s,
                     "port")==0) {
                  drawPolyHighlightedNode = obj_under_cursor;
               } else {
                  drawPolyHighlightedNode = NULL;
               }
               if (drawPolyHighlightedNode != NULL) {
                  need_to_highlight = TRUE;
               }
               something_changed = TRUE;
            }
         } else {
            if (obj_under_cursor != NULL) {
               if ((attr_ptr=FindAttrWithName(obj_under_cursor, "type=",
                     NULL)) != NULL && strcmp(attr_ptr->attr_value.s,
                     "port")==0) {
                  drawPolyHighlightedNode = obj_under_cursor;
               } else {
                  drawPolyHighlightedNode = NULL;
               }
               if (drawPolyHighlightedNode != NULL) {
                  need_to_highlight = TRUE;
                  something_changed = TRUE;
               }
            }
         }
         if (need_to_highlight) {
            SelBox(drawWindow, revGrayGC,
                  OFFSET_X(drawPolyHighlightedNode->bbox.ltx)-2,
                  OFFSET_Y(drawPolyHighlightedNode->bbox.lty)-2,
                  OFFSET_X(drawPolyHighlightedNode->bbox.rbx)+2,
                  OFFSET_Y(drawPolyHighlightedNode->bbox.rby)+2);
         }
         if (something_changed) {
            if (*drawPolyFirstNodeName != '\0') {
               SetStringStatus(drawPolyFirstNodeName);
            } else {
               SetStringStatus("");
            }
         }
      }
      return (INVALID);
   }

   if (input->type == ButtonPress)
   {
      button_ev = &(input->xbutton);
      if ((button_ev->state & ShiftMask) && (button_ev->state & ControlMask))
      {
         if (button_ev->button == Button1) {
            int abs_x=ABS_X(button_ev->x);
            int abs_y=ABS_Y(button_ev->y);

            ZoomInAtCursor(abs_x, abs_y);
         } else if (button_ev->button == Button3) {
            ZoomOut();
         }
         return INVALID;
      }
      else if ((button_ev->button == Button3) &&
            (button_ev->state & (ShiftMask | ControlMask)))
      {
         SetCurChoice (NOTHING);
         return (INVALID);
      }
      else if (((button_ev->button == Button2) && curChoice == NOTHING &&
            (button_ev->state & (ShiftMask | ControlMask))) ||
            (inHyperSpace && button_ev->button == Button1))
      {
         Teleport (button_ev);
         return (INVALID);
      }
      else if (button_ev->button == Button2)
         return (MainMenu ());
      else if (button_ev->button == Button3)
      {
         ModeMenu (button_ev->x_root, button_ev->y_root, FALSE);
         return (INVALID);
      }
      Msg ("");
   }

   switch(curChoice)
   {
      case NOTHING: Select (input); break;
      case DRAWTEXT: DrawText (input); break;
      case DRAWBOX: DrawBox (input); break;
      case DRAWCIRCLE: DrawOval (input); break;
      case DRAWPOLY: DrawPoly (input); break;
      case DRAWPOLYGON: DrawPolygon (input); break;
      case DRAWARC: DrawArc (input); break;
      case DRAWRCBOX: DrawRCBox (input); break;
      case FREEHAND: DrawPoly (input); break;
      case VERTEXMODE: Select (input); break;
      case ROTATEMODE: Select (input); break;
   }
   return (INVALID);
}

