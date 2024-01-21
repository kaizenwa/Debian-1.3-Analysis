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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/choice.c,v 3.1 1996/05/14 12:55:55 william Exp $";
#endif

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "align.e"
#include "arc.e"
#include "box.e"
#ifndef _NO_EXTERN
#include "choice.e"
#endif
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "edit.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "mainloop.e"
#include "mainmenu.e"
#include "mark.e"
#include "msg.e"
#include "navigate.e"
#include "oval.e"
#include "page.e"
#include "pattern.e"
#include "poly.e"
#include "polygon.e"
#include "raster.e"
#include "rcbox.e"
#include "select.e"
#include "setup.e"
#include "shape.e"
#include "special.e"
#include "stretch.e"
#include "text.e"
#include "xbitmap.e"

#define MODE_ROW 0
#define EDIT_ROW 0
#define ZOOM_ROW 0
#define RADIUS_ROW 0
#define PAGE_ROW 0
#define HORI_ALIGN_ROW 0
#define FONT_ROW 0
#define VSPACE_ROW 0
#define SHAPE_ROW 0
#define DASH_ROW 0
#define LINE_TYPE_ROW 0
#define FILL_ROW 0
#define COLOR_ROW 0

#define FILE_ROW 1
#define PRINT_ROW 1
#define MOVE_MODE_ROW 1
#define ROTATE_ROW 1
#define PAGELAYOUT_ROW 1
#define VERT_ALIGN_ROW 1
#define TEXT_SIZE_ROW 1
#define JUST_ROW 1
#define STRETCHTEXT_ROW 1
#define LINE_STYLE_ROW 1
#define LINE_WIDTH_ROW 1
#define PEN_ROW 1
#define SPECIAL_ROW 1

#define MODE_COL 0
#define EDIT_COL 1
#define ZOOM_COL 2
#define RADIUS_COL 3
#define PAGE_COL 4
#define HORI_ALIGN_COL 5
#define FONT_COL 6
#define VSPACE_COL 7
#define SHAPE_COL 8
#define DASH_COL 9
#define LINE_TYPE_COL 10
#define FILL_COL 11
#define COLOR_COL 12

#define FILE_COL 0
#define PRINT_COL 1
#define MOVE_MODE_COL 2
#define ROTATE_COL 3
#define PAGELAYOUT_COL 4
#define VERT_ALIGN_COL 5
#define TEXT_SIZE_COL 6
#define JUST_COL 7
#define STRETCHTEXT_COL 8
#define LINE_STYLE_COL 9
#define LINE_WIDTH_COL 10
#define PEN_COL 11
#define SPECIAL_COL 12

#define CHOICE_SHIFT 4

#define CHOICE_MODE ((MODE_COL<<CHOICE_SHIFT)|MODE_ROW)
#define CHOICE_COLOR ((COLOR_COL<<CHOICE_SHIFT)|COLOR_ROW)
#define CHOICE_FILE ((FILE_COL<<CHOICE_SHIFT)|FILE_ROW)
#define CHOICE_EDIT ((EDIT_COL<<CHOICE_SHIFT)|EDIT_ROW)
#define CHOICE_PRINT ((PRINT_COL<<CHOICE_SHIFT)|PRINT_ROW)
#define CHOICE_SPECIAL ((SPECIAL_COL<<CHOICE_SHIFT)|SPECIAL_ROW)
#define CHOICE_HORI_ALIGN ((HORI_ALIGN_COL<<CHOICE_SHIFT)|HORI_ALIGN_ROW)
#define CHOICE_VERT_ALIGN ((VERT_ALIGN_COL<<CHOICE_SHIFT)|VERT_ALIGN_ROW)
#define CHOICE_ROTATE ((ROTATE_COL<<CHOICE_SHIFT)|ROTATE_ROW)
#define CHOICE_MOVE_MODE ((MOVE_MODE_COL<<CHOICE_SHIFT)|MOVE_MODE_ROW)
#define CHOICE_RADIUS ((RADIUS_COL<<CHOICE_SHIFT)|RADIUS_ROW)
#define CHOICE_ZOOM ((ZOOM_COL<<CHOICE_SHIFT)|ZOOM_ROW)
#define CHOICE_FONT ((FONT_COL<<CHOICE_SHIFT)|FONT_ROW)
#define CHOICE_JUST ((JUST_COL<<CHOICE_SHIFT)|JUST_ROW)
#define CHOICE_TEXT_SIZE ((TEXT_SIZE_COL<<CHOICE_SHIFT)|TEXT_SIZE_ROW)
#define CHOICE_VSPACE ((VSPACE_COL<<CHOICE_SHIFT)|VSPACE_ROW)
#define CHOICE_SHAPE ((SHAPE_COL<<CHOICE_SHIFT)|SHAPE_ROW)
#define CHOICE_STRETCHTEXT ((STRETCHTEXT_COL<<CHOICE_SHIFT)|STRETCHTEXT_ROW)
#define CHOICE_LINE_WIDTH ((LINE_WIDTH_COL<<CHOICE_SHIFT)|LINE_WIDTH_ROW)
#define CHOICE_LINE_STYLE ((LINE_STYLE_COL<<CHOICE_SHIFT)|LINE_STYLE_ROW)
#define CHOICE_LINE_TYPE ((LINE_TYPE_COL<<CHOICE_SHIFT)|LINE_TYPE_ROW)
#define CHOICE_DASH ((DASH_COL<<CHOICE_SHIFT)|DASH_ROW)
#define CHOICE_FILL ((FILL_COL<<CHOICE_SHIFT)|FILL_ROW)
#define CHOICE_PEN ((PEN_COL<<CHOICE_SHIFT)|PEN_ROW)
#define CHOICE_PAGE ((PAGE_COL<<CHOICE_SHIFT)|PAGE_ROW)
#define CHOICE_PAGELAYOUT ((PAGELAYOUT_COL<<CHOICE_SHIFT)|PAGELAYOUT_ROW)

int	curChoice=NOTHING;
int	cycleThroughChoice=FALSE;

static GC	choiceGC=(GC)0;
static Pixmap	abcBitmap=None, rotatedAbcBitmap=None;
static XImage	*abcImage=NULL;
static int	abcRotation=0, abcRotatedBitmapSize=0;
static struct BBRec rotatedAbcBBox;

#include "xbm/abc.xbm"

void InitChoice()
{
   XGCValues values;

   values.foreground = xorOne;
   values.background = xorZero;
   values.fill_style = FillSolid;
   values.font = defaultFontPtr->fid;
   choiceGC = XCreateGC(mainDisplay, choiceWindow,
         GCForeground | GCBackground | GCFillStyle | GCFont, &values);

   memset(&rotatedAbcBBox, 0, sizeof(struct BBRec));
   abcBitmap = XCreateBitmapFromData(mainDisplay, choiceWindow,
         abc_bits, abc_width, abc_height);
   if (abcBitmap == None) fprintf(stderr, "Fail to allocate pixmap.\n");

   abcImage = XGetImage(mainDisplay, abcBitmap, 0, 0, abc_width, abc_height, 1,
         ZPixmap);
   if (abcImage == NULL) fprintf(stderr, "Fail to get image.\n");
}

void CleanUpChoices()
{
   XFreeGC(mainDisplay, choiceGC);
   choiceGC = (GC)0;
   XFreePixmap(mainDisplay, abcBitmap);
   XDestroyImage(abcImage);
   if (rotatedAbcBitmap != None) XFreePixmap(mainDisplay, rotatedAbcBitmap);
   abcBitmap = rotatedAbcBitmap = None;
}

static
void UpdateAbcBitmap()
{
   XImage *image;
   int r, ltx, lty, rbx, rby, half_sz;
   struct XfrmMtrxRec ctm;
   double radian, sin_val, cos_val;

   if (textRotation == 0) return;
   if (textRotation == abcRotation && rotatedAbcBitmap != None) return;
   if (rotatedAbcBitmap == None) {
      double dval=(double)sqrt(
            (double)(abc_width*abc_width+abc_height*abc_height));

      abcRotatedBitmapSize = ((round(dval)+2)<<1);
      if ((rotatedAbcBitmap=XCreatePixmap(mainDisplay, dummyBitmap,
            abcRotatedBitmapSize, abcRotatedBitmapSize, 1)) == None) {
         fprintf(stderr, "Fail to allocate pixmap.\n");
         return;
      }
   }
   XFillRectangle(mainDisplay, rotatedAbcBitmap, xbmGC, 0, 0,
         abcRotatedBitmapSize, abcRotatedBitmapSize);
   image = XGetImage(mainDisplay, rotatedAbcBitmap, 0, 0, abcRotatedBitmapSize,
         abcRotatedBitmapSize, 1, ZPixmap);
   if (image == NULL) {
      fprintf(stderr, "Fail to get image.\n");
      return;
   }
   radian = (((double)textRotation)*M_PI/180.0/64.0);
   sin_val = sin(radian);
   cos_val = cos(radian);

   memset(&ctm, 0, sizeof(struct XfrmMtrxRec));
   ctm.m[CTM_SX] = ctm.m[CTM_SY] = round(((double)1000.0)*cos_val);
   ctm.m[CTM_SIN] = round(((double)1000.0)*sin_val);
   ctm.m[CTM_MSIN] = (-ctm.m[CTM_SIN]);
   ctm.m[CTM_TX] = ctm.m[CTM_TY] = 0;
   ltx = lty = abcRotatedBitmapSize+1;
   rbx = rby = (-1);
   half_sz = (abcRotatedBitmapSize>>1);
   for (r=0; r < abcRotatedBitmapSize; r++) {
      int c, y=r-half_sz;

      for (c=0; c < abcRotatedBitmapSize; c++) {
         int x=c-half_sz, new_x, new_y;

         ReverseTransformPointThroughCTM(x, y, &ctm, &new_x, &new_y);
         if (new_x >= 0 && new_x < abc_width &&
               new_y >= 0 && new_y < abc_height) {
            if (XGetPixel(abcImage, new_x, new_y) == 1) {
               XPutPixel(image, c, r, 1);
               if (c < ltx) ltx = c; if (r < lty) lty = r;
               if (c > rbx) rbx = c; if (r > rby) rby = r;
            }
         }
      }
   }
   XPutImage(mainDisplay, rotatedAbcBitmap, xbmGC, image, 0, 0, 0, 0,
         abcRotatedBitmapSize, abcRotatedBitmapSize);
   XDestroyImage(image);
   rotatedAbcBBox.ltx = ltx; rotatedAbcBBox.lty = lty;
   rotatedAbcBBox.rbx = rbx; rotatedAbcBBox.rby = rby;
}

void ShowWhereToPrint ()
{
   if (colorDump)
      XSetForeground (mainDisplay, rasterGC, colorPixels[colorIndex]);
   XSetStipple (mainDisplay, rasterGC, whereToPrintPixmap[whereToPrint]);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC, PRINT_COL*choiceImageW,
         PRINT_ROW*choiceImageH, choiceImageW, choiceImageH);
   if (colorDump) XSetForeground (mainDisplay, rasterGC, myFgPixel);
}

void ShowMode ()
{
   XSetStipple (mainDisplay, rasterGC, choicePixmap[curChoice]);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC, MODE_COL*choiceImageW,
         MODE_ROW*choiceImageH, choiceImageW, choiceImageH);
}

void ShowColor (PropagateWhereToPrint)
   int PropagateWhereToPrint;
{
   XGCValues values;

   if (colorDisplay)
   {
      values.foreground = colorPixels[colorIndex];
      values.function = GXcopy;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[1];
      XChangeGC (mainDisplay, patGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple, &values);

      XFillRectangle (mainDisplay, choiceWindow, patGC,
            COLOR_COL*choiceImageW, COLOR_ROW*choiceImageH,
            choiceImageW, choiceImageH);
   }
   if (PropagateWhereToPrint) ShowWhereToPrint ();
}

void ShowHoriAlign ()
{
   XSetStipple (mainDisplay, rasterGC, alignHoriPixmap[horiAlign]);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC,
         HORI_ALIGN_COL*choiceImageW, HORI_ALIGN_ROW*choiceImageH,
         choiceImageW, choiceImageH);
}

void ShowVertAlign ()
{
   XSetStipple (mainDisplay, rasterGC, alignVertPixmap[vertAlign]);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC,
         VERT_ALIGN_COL*choiceImageW, VERT_ALIGN_ROW*choiceImageH,
         choiceImageW, choiceImageH);
}

void ShowJust ()
{
   XSetStipple (mainDisplay, rasterGC, justPixmap[textJust]);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC,
         JUST_COL*choiceImageW, JUST_ROW*choiceImageH,
         choiceImageW, choiceImageH);
}

void ShowCurFont ()
{
   int x, y, w, h;
   XRectangle recs[1];
   XGCValues values;

   recs[0].x = FONT_COL*choiceImageW;
   recs[0].y = FONT_ROW*choiceImageH;
   recs[0].width = choiceImageW;
   recs[0].height = choiceImageH;

   w = XTextWidth (canvasFontPtr, "W", 1);
   h = canvasFontHeight;

   x = (w > choiceImageW) ?
         FONT_COL*choiceImageW - (w-choiceImageW)/2 :
         FONT_COL*choiceImageW + (choiceImageW-w)/2;
   y = (h > choiceImageH) ?
         FONT_ROW*choiceImageH + (canvasFontAsc-(h-choiceImageH)/2) :
         FONT_ROW*choiceImageH + (canvasFontAsc+(choiceImageH-h)/2);

   XClearArea (mainDisplay, choiceWindow, FONT_COL*choiceImageW,
         FONT_ROW*choiceImageH, choiceImageW, choiceImageH, FALSE);

   XSetFont (mainDisplay, choiceGC, canvasFontPtr->fid);
   XSetClipRectangles (mainDisplay, choiceGC, 0, 0, recs, 1, YXBanded);

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground, &values);

   XDrawString (mainDisplay, choiceWindow, choiceGC, x, y, "W", 1);

   values.foreground = xorOne;
   values.background = xorZero;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground, &values);

   recs[0].x = 0;
   recs[0].y = 0;
   recs[0].width = choiceWindowW;
   recs[0].height = choiceWindowH;
   XSetClipRectangles (mainDisplay, choiceGC, 0, 0, recs, 1, YXBanded);
}

void ShowTextVSpace ()
{
   int len, x, y;
   char s[80];
   XGCValues values;

   XSetStipple (mainDisplay, rasterGC, vspacePixmap);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC, VSPACE_COL*choiceImageW,
         VSPACE_ROW*choiceImageH, choiceImageW, choiceImageH);
   sprintf (s, "%1d", textVSpace);
   len = strlen (s);
   x = (int)((VSPACE_COL+0.5)*choiceImageW-2);
   y = VSPACE_ROW*choiceImageH+((choiceImageH-rulerFontAsc)>>1)+rulerFontAsc;

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.font = rulerFontPtr->fid;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground | GCFont,
         &values);

   XDrawString (mainDisplay, choiceWindow, choiceGC, x, y, s, len);

   values.foreground = xorOne;
   values.background = xorZero;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground, &values);
}

void ShowZoom ()
{
   int len, x, y, w, x_w, one_w;
   char s[80];
   XGCValues values;

   XClearArea (mainDisplay, choiceWindow, ZOOM_COL*choiceImageW,
      ZOOM_ROW*choiceImageH, choiceImageW, choiceImageH, FALSE);

   sprintf (s, "x%1d", 1<<zoomScale);
   len = strlen (s);

   w = XTextWidth (rulerFontPtr, s, len)+1;
   x_w = XTextWidth (rulerFontPtr, "x", 1)+1;
   one_w = XTextWidth (rulerFontPtr, "1", 1);
   x = ZOOM_COL*choiceImageW+((choiceImageW-w)>>1);
   y = ZOOM_ROW*choiceImageH+((choiceImageH-rulerFontAsc)>>1)+rulerFontAsc;

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.font = rulerFontPtr->fid;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground | GCFont,
         &values);

   XDrawString (mainDisplay, choiceWindow, choiceGC, x, y, "x", 1);

   x += x_w;
   sprintf (s, "%1d", 1<<zoomScale);
   len--;
   if (zoomedIn || zoomScale==0)
      XDrawString (mainDisplay, choiceWindow, choiceGC, x, y, s, len);
   else
   {
      y = ZOOM_ROW*choiceImageH + (choiceImageH>>1);
      XDrawString (mainDisplay, choiceWindow, choiceGC, x+((w-x_w-one_w)>>1),
            y-2, "1", 1);
      XDrawLine (mainDisplay, choiceWindow, choiceGC, x, y, x+(w-x_w-1), y);
      XDrawString (mainDisplay, choiceWindow, choiceGC, x, y+rulerFontAsc,
            s, len);
   }

   values.foreground = xorOne;
   values.background = xorZero;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground, &values);
}

void ShowTextSize ()
{
   int len, x, y, w;
   char s[80];
   XGCValues values;

   XClearArea (mainDisplay, choiceWindow, TEXT_SIZE_COL*choiceImageW,
         TEXT_SIZE_ROW*choiceImageH, choiceImageW, choiceImageH, FALSE);
   sprintf (s, "%1d", curSize);
   len = strlen (s);
   w = XTextWidth (defaultFontPtr, s, len);
   x = TEXT_SIZE_COL*choiceImageW + ((choiceImageW-w)>>1);
   y = TEXT_SIZE_ROW*choiceImageH + ((choiceImageH-defaultFontAsc)>>1) + 
         defaultFontAsc;

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.font = defaultFontPtr->fid;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground | GCFont,
         &values);

   XDrawString (mainDisplay, choiceWindow, choiceGC, x, y, s, len);

   values.foreground = xorOne;
   values.background = xorZero;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground, &values);
}

void ShowRotate()
{
   int ltx, lty, w, h, rbx, rby, x, y;
   XGCValues values;
   Pixmap bitmap=None;

   UpdateAbcBitmap();
   XClearArea(mainDisplay, choiceWindow, ROTATE_COL*choiceImageW,
         ROTATE_ROW*choiceImageH, choiceImageW, choiceImageH, FALSE);
   if (textRotation == 0) {
      bitmap = abcBitmap;
      x = 0;
      y = 0;
      w = abc_width;
      h = abc_height;
   } else {
      bitmap = rotatedAbcBitmap;
      x = rotatedAbcBBox.ltx;
      y = rotatedAbcBBox.lty;
      w = rotatedAbcBBox.rbx-x;
      h = rotatedAbcBBox.rby-y;
   }
   ltx = ((choiceImageW-w)>>1);
   lty = ((choiceImageH-h)>>1);
   rbx = ltx + w;
   rby = lty + h;
   if (ltx < 0) {
      x = (-ltx);
      w -= x;
      ltx = 0;
   }
   if (lty < 0) {
      y = (-lty);
      h -= y;
      lty = 0;
   }
   if (rbx > choiceImageW) {
      w -= (rbx-choiceImageW);
      rbx = choiceImageW;
   }
   if (rby > choiceImageH) {
      h -= (rbx-choiceImageH);
      rby = choiceImageH;
   }
   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.function = GXcopy;
   values.fill_style = FillStippled;
   values.ts_x_origin = ROTATE_COL*choiceImageW+ltx-x;
   values.ts_y_origin = ROTATE_ROW*choiceImageH+lty-y;
   values.stipple = bitmap;
   XChangeGC (mainDisplay, patGC,
         GCForeground | GCBackground | GCFunction | GCFillStyle | GCStipple |
         GCTileStipXOrigin | GCTileStipYOrigin, &values);
   XFillRectangle(mainDisplay, choiceWindow, patGC,
         ROTATE_COL*choiceImageW+ltx, ROTATE_ROW*choiceImageH+lty,
         rbx-ltx, rby-lty);
   XSetTSOrigin(mainDisplay, patGC, 0, 0);
}

void ShowSpecial ()
{
   XSetStipple (mainDisplay, rasterGC, specialPixmap);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC,
         SPECIAL_COL*choiceImageW, SPECIAL_ROW*choiceImageH,
         choiceImageW, choiceImageH);
}

void ShowLineWidth()
{
   char s[40];
   int x, y, len, w;
   XGCValues values;

   XSetStipple(mainDisplay, rasterGC, shortLineWidthPixmap[lineWidth]);
   XFillRectangle(mainDisplay, choiceWindow, rasterGC,
         LINE_WIDTH_COL*choiceImageW, LINE_WIDTH_ROW*choiceImageH,
         choiceImageW, choiceImageH);

   UtilStrCpy(s, sizeof(s), curWidthOfLineSpec[lineWidth]);
   len = strlen(s);
   w = rulerFontWidth * len;
   x = LINE_WIDTH_COL*choiceImageW+((choiceImageW-w)>>1);
   y = LINE_WIDTH_ROW*choiceImageH+((choiceImageH-rulerFontAsc)>>1) +
         rulerFontAsc;
   values.foreground = myBgPixel;
   values.background = myBgPixel;
   values.font = rulerFontPtr->fid;
   XChangeGC(mainDisplay, choiceGC, GCForeground | GCBackground | GCFont,
         &values);
   XFillRectangle(mainDisplay, choiceWindow, choiceGC,
         x-2, y-rulerFontAsc-2, w+4, rulerFontAsc+4);
   XSetForeground(mainDisplay, choiceGC, myFgPixel);
   XDrawString(mainDisplay, choiceWindow, choiceGC, x, y, s, len);
   values.foreground = xorOne;
   values.background = xorZero;
   XChangeGC(mainDisplay, choiceGC, GCForeground | GCBackground, &values);
}

void ShowLineStyle ()
{
   XSetStipple (mainDisplay, rasterGC, shortLineStylePixmap[lineStyle]);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC,
         LINE_STYLE_COL*choiceImageW, LINE_STYLE_ROW*choiceImageH,
         choiceImageW, choiceImageH);
}

void ShowLineType ()
{
   XSetStipple (mainDisplay, rasterGC, shortLineTypePixmap[curSpline]);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC,
         LINE_TYPE_COL*choiceImageW, LINE_TYPE_ROW*choiceImageH,
         choiceImageW, choiceImageH);
}

void ShowDash ()
{
   XSetStipple (mainDisplay, rasterGC, shortDashPixmap[curDash]);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC, DASH_COL*choiceImageW,
         DASH_ROW*choiceImageH, choiceImageW, choiceImageH);
}

void ShowFile ()
{
   switch (pageStyle)
   {
      case PORTRAIT: XSetStipple(mainDisplay,rasterGC,filePixmap); break;
      case LANDSCAPE: XSetStipple(mainDisplay,rasterGC,landscapePixmap); break;
   }
   XFillRectangle (mainDisplay, choiceWindow, rasterGC, FILE_COL*choiceImageW,
         FILE_ROW*choiceImageH, choiceImageW, choiceImageH);
}

void ShowEdit ()
{
   XSetStipple (mainDisplay, rasterGC, editPixmap);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC, EDIT_COL*choiceImageW,
         EDIT_ROW*choiceImageH, choiceImageW, choiceImageH);
}

void ShowRCBRadius ()
{
   int len, x, y;
   char s[80];
   XGCValues values;

   XSetStipple (mainDisplay, rasterGC, rcbRadiusPixmap);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC, RADIUS_COL*choiceImageW,
         RADIUS_ROW*choiceImageH, choiceImageW, choiceImageH);
   sprintf (s, "%1d", rcbRadius);
   len = strlen (s);
   x = (int)((RADIUS_COL+0.5)*choiceImageW-2);
   y = (((RADIUS_ROW+1)*choiceImageH-rulerFontAsc)>>1)+rulerFontAsc;

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.font = rulerFontPtr->fid;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground | GCFont,
         &values);

   XDrawString (mainDisplay, choiceWindow, choiceGC, x, y, s, len);

   values.foreground = xorOne;
   values.background = xorZero;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground, &values);
}

void ShowMoveMode ()
{
   XSetStipple (mainDisplay, rasterGC, moveModePixmap[moveMode]);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC,
         MOVE_MODE_COL*choiceImageW, MOVE_MODE_ROW*choiceImageH,
         choiceImageW, choiceImageH);
}

void ShowShape ()
{
   XSetStipple(mainDisplay, rasterGC, shapePixmap[STAR_SHAPE]);
   XFillRectangle(mainDisplay, choiceWindow, rasterGC, SHAPE_COL*choiceImageW,
         SHAPE_ROW*choiceImageH, choiceImageW, choiceImageH);
}

void ShowStretchableTextMode ()
{
   XSetStipple (mainDisplay, rasterGC, stretchableModePixmap[stretchableText]);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC,
         STRETCHTEXT_COL*choiceImageW, STRETCHTEXT_ROW*choiceImageH,
         choiceImageW, choiceImageH);
}

void ShowFill ()
{
   XGCValues values;

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.function = GXcopy;
   values.fill_style = FillOpaqueStippled;
   values.stipple = patPixmap[objFill];
   XChangeGC (mainDisplay, patGC,
         GCForeground | GCBackground | GCFunction | GCFillStyle | GCStipple,
         &values);

   XFillRectangle (mainDisplay, choiceWindow, patGC, FILL_COL*choiceImageW,
         FILL_ROW*choiceImageH, choiceImageW, choiceImageH);
}

void ShowPen ()
{
   XGCValues values;

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.function = GXcopy;
   values.fill_style = FillOpaqueStippled;
   values.stipple = patPixmap[penPat];
   XChangeGC (mainDisplay, patGC,
         GCForeground | GCBackground | GCFunction | GCFillStyle | GCStipple,
         &values);

   XFillRectangle (mainDisplay, choiceWindow, patGC, PEN_COL*choiceImageW,
         PEN_ROW*choiceImageH, choiceImageW, choiceImageH);
   if (penPat != NONEPAT)
      XClearArea (mainDisplay, choiceWindow,
            PEN_COL*choiceImageW+(choiceImageW>>2),
            PEN_ROW*choiceImageH+(choiceImageH>>2),
            (choiceImageW>>1), (choiceImageH>>1), FALSE);
}

void ShowPage ()
{
   int                len, x, y, w;
   char               s[80];
   XGCValues  values;

   XClearArea (mainDisplay, choiceWindow, PAGE_COL*choiceImageW,
      PAGE_ROW*choiceImageH, choiceImageW, choiceImageH, FALSE);

   switch (pageLayoutMode)
   {
      case PAGE_STACK: sprintf (s, "%1d/%1d", curPageNum, lastPageNum); break;
      case PAGE_TILE: sprintf (s, "%1dx%1d", paperCol, paperRow); break;
   }
   len = strlen (s);
   w = XTextWidth (rulerFontPtr, s, len);
   x = PAGE_COL*choiceImageW+((choiceImageW-w)>>1);
   y = PAGE_ROW*choiceImageH+((choiceImageH-rulerFontAsc)>>1) +
         rulerFontAsc;

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.font = rulerFontPtr->fid;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground | GCFont,
         &values);

   XDrawString (mainDisplay, choiceWindow, choiceGC, x, y, s, len);

   values.foreground = xorOne;
   values.background = xorZero;
   XChangeGC (mainDisplay, choiceGC, GCForeground | GCBackground,
         &values);

   RedrawPageWindow();
}

void ShowPageLayout ()
{
   XSetStipple (mainDisplay, rasterGC, pageLayoutPixmap[pageLayoutMode]);
   XFillRectangle (mainDisplay, choiceWindow, rasterGC,
         PAGELAYOUT_COL*choiceImageW, PAGELAYOUT_ROW*choiceImageH,
         choiceImageW, choiceImageH);
}

struct MouseStatusStrRec choiceMouseStatus[] = {
   { "select/move/resize objects", "Main Menu", "Mode Menu" },
   { "enter text", "Main Menu", "Mode Menu" },
   { "draw rectangles", "Main Menu", "Mode Menu" },
   { "draw ovals", "Main Menu", "Mode Menu" },
   { "draw poly/open splines", "Main Menu", "Mode Menu" },
   { "draw polygon/closed splines", "Main Menu", "Mode Menu" },
   { "draw arcs", "Main Menu", "Mode Menu" },
   { "draw rcboxes", "Main Menu", "Mode Menu" },
   { "freehand poly/open splines", "Main Menu", "Mode Menu" },
   { "select/move vertices", "Main Menu", "Mode Menu" },
   { "rotate/shear objects", "Main Menu", "Mode Menu" },
   { NULL, NULL, NULL }
};

static
int NeedSelectTopObject(Choice, VertexTypeObj)
   int Choice, VertexTypeObj;
{
   if (VertexTypeObj) {
      return (Choice==NOTHING || Choice==VERTEXMODE || Choice==ROTATEMODE);
   } else {
      return (Choice==NOTHING || Choice==ROTATEMODE);
   }
}

void SetCurChoice(NewChoice)
   int NewChoice;
{
   if (curChoice == NewChoice) return;

   switch (curChoice) {
   case NOTHING:
      if (topSel != NULL) {
         if (NewChoice == VERTEXMODE) {
            UnSelNonVertexObjs(TRUE); /* with highlight */
            UpdSelBBox();
         } else if (NewChoice != ROTATEMODE) {
            HighLightReverse();
            RemoveAllSel();
         }
      }
      break;
   case DRAWTEXT:
      CreateTextObj();
      if (NeedSelectTopObject(NewChoice, FALSE) && textDrawn) {
         HighLightJustDrawnText();
      }
      textDrawn = FALSE;
      textCursorShown = FALSE;
      break;
   case DRAWBOX:
      if (NeedSelectTopObject(NewChoice, FALSE) && boxDrawn) {
         SelectTopObj();
      }
      boxDrawn = FALSE;
      break;
   case DRAWCIRCLE:
      if (NeedSelectTopObject(NewChoice, FALSE) && ovalDrawn) {
         SelectTopObj();
      }
      ovalDrawn = FALSE;
      break;
   case DRAWPOLY:
      if (NeedSelectTopObject(NewChoice, TRUE) && polyDrawn) {
         SelectTopObj();
      }
      polyDrawn = FALSE;
      break;
   case DRAWPOLYGON:
      if (NeedSelectTopObject(NewChoice, TRUE) && polygonDrawn) {
         SelectTopObj();
      }
      polygonDrawn = FALSE;
      break;
   case DRAWARC:
      if (NeedSelectTopObject(NewChoice, FALSE) && arcDrawn) {
         SelectTopObj();
      }
      arcDrawn = FALSE;
      break;
   case DRAWRCBOX:
      if (NeedSelectTopObject(NewChoice, FALSE) && rcBoxDrawn) {
         SelectTopObj();
      }
      rcBoxDrawn = FALSE;
      break;
   case FREEHAND:
      if (NeedSelectTopObject(NewChoice, TRUE) && polyDrawn) {
         SelectTopObj();
      }
      polyDrawn = FALSE;
      break;
   case VERTEXMODE:
      if (NewChoice == NOTHING || NewChoice == ROTATEMODE) {
         HighLightReverse();
         JustRemoveAllVSel();
         HighLightForward();
      } else {
         HighLightReverse();
         RemoveAllSel();
      }
      break;
   case ROTATEMODE:
      if (topSel != NULL) {
         if (NewChoice == VERTEXMODE) {
            UnSelNonVertexObjs(TRUE); /* with highlight */
            UpdSelBBox();
         } else if (NewChoice != NOTHING) {
            HighLightReverse();
            RemoveAllSel();
         }
      }
      break;
   }

   curChoice = NewChoice;

   switch (curChoice) {
   case DRAWTEXT:
   case DRAWBOX:
   case DRAWCIRCLE:
   case DRAWPOLY:
   case DRAWPOLYGON:
   case DRAWARC:
   case DRAWRCBOX:
   case FREEHAND:
      if (colorLayers && !colorLayerOn[colorIndex]) {
         sprintf(gszMsgBox,
               "Invisible color %1d (%s) is selected for drawing.",
               colorIndex, colorMenuItems[colorIndex]);
         Msg(gszMsgBox);
      }
      break;
   case NOTHING: break;
   case VERTEXMODE: break;
   case ROTATEMODE: break;
   }
   ShowCursor();
   textCursorShown = FALSE;

   ShowMode();
   UpdateSubMenu(MENU_MODE);

   if (curChoice == NOTHING && inHyperSpace) {
      SetMouseStatus("(none)",
            choiceMouseStatus[curChoice].m, choiceMouseStatus[curChoice].r);
   } else {
      SetMouseStatus(choiceMouseStatus[curChoice].l,
            choiceMouseStatus[curChoice].m, choiceMouseStatus[curChoice].r);
   }
   if (inHyperSpace && curChoice != NOTHING) ToggleHyperSpace(FALSE);
}

static int prevChoice=NOTHING;

void PushCurChoice ()
   /* Kouichi Matsuda's modification */
{
   if (curChoice == NOTHING)
      SetCurChoice (prevChoice);
   else
   {
      prevChoice = curChoice;
      SetCurChoice (NOTHING);
   }
}

static
void TextVSpaceLoop(button_ev)
   XButtonEvent *button_ev;
{
   int saved_text_vspace=textVSpace, res=8, done=FALSE, need_to_restore=FALSE;
   int orig_x=button_ev->x, orig_y=button_ev->y, saved_change=0;
   XEvent ev;

   XGrabPointer(mainDisplay, choiceWindow, False,
         PointerMotionMask | ButtonReleaseMask, GrabModeAsync,
         GrabModeAsync, None, handCursor, CurrentTime);

   while (!done) {
      XNextEvent(mainDisplay, &ev);

      if (ev.type == Expose || ev.type == VisibilityNotify) {
         ExposeEventHandler(&ev, TRUE);
      } else if (ev.type == ButtonRelease) {
         XUngrabPointer(mainDisplay, CurrentTime);
         done = TRUE;
      } else if (ev.type == MotionNotify) {
         int dx=ev.xmotion.x-orig_x, dy=ev.xmotion.y-orig_y;
         int change=((abs(dx)>abs(dy)) ? (int)(dx/res) : (int)(dy/res));

         if (change != saved_change) {
            textVSpace = saved_text_vspace+change;
            ShowTextVSpace();
            saved_change = change;
         }
         while (XCheckMaskEvent(mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   ShowTextVSpace();
   if ((topSel == NULL || stickyMenuSelection) &&
         (textCursorH+textVSpace <= 0)) {
      need_to_restore = TRUE;
   }
   ChangeVSpace(textVSpace);
   if (need_to_restore) {
      textVSpace = saved_text_vspace;
      ShowTextVSpace();
   }
}

static
void RCBRadiusLoop (button_ev)
   XButtonEvent *button_ev;
{
   int saved_rcb_radius = rcbRadius, res=8, done=FALSE;
   int orig_x=button_ev->x, orig_y=button_ev->y, saved_change=0;
   XEvent ev;

   XGrabPointer (mainDisplay, choiceWindow, False,
         PointerMotionMask | ButtonReleaseMask, GrabModeAsync,
         GrabModeAsync, None, handCursor, CurrentTime);

   while (!done)
   {
      XNextEvent (mainDisplay, &ev);

      if (ev.type == Expose || ev.type == VisibilityNotify)
         ExposeEventHandler (&ev, TRUE);
      else if (ev.type == ButtonRelease)
      {
         XUngrabPointer (mainDisplay, CurrentTime);
         done = TRUE;
      }
      else if (ev.type == MotionNotify)
      {
         int dx=ev.xmotion.x-orig_x, dy=ev.xmotion.y-orig_y;
         int change=((abs(dx)>abs(dy)) ? (int)(dx/res) : (int)(dy/res));

         if (change != saved_change)
         {
            rcbRadius = max(MIN_RCB_RADIUS, saved_rcb_radius+change);
            ShowRCBRadius ();
            saved_change = change;
         }
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   if (topSel != NULL) ChangeAllSelRCBRadius (rcbRadius);
}

static
void ZoomLoop (button_ev)
   XButtonEvent *button_ev;
{
   int saved_zoomed_in=zoomedIn, saved_zoom_scale=zoomScale, res=8;
   int new_zoomed_in, new_zoom_scale, done=FALSE, saved_change=0;
   int initial_zoom, zoom, max_zoom, win_w, win_h;
   int orig_x=button_ev->x, orig_y=button_ev->y;
   XEvent ev;

   initial_zoom = (zoomedIn) ? (MAX_ZOOMED_IN-zoomScale) :
         (MAX_ZOOMED_IN+zoomScale);

   win_w = drawWinW;
   win_h = drawWinH;
   new_zoomed_in = zoomedIn;
   new_zoom_scale = zoomScale;
   while ((win_w>>1) >= paperWidth && (win_h>>1) >= paperHeight)
   {
      if (new_zoomed_in)
         new_zoom_scale++;
      else if (new_zoom_scale == 0)
      {
         new_zoomed_in = TRUE;
         new_zoom_scale++;
      }
      else
         new_zoom_scale--;
      win_w >>= 1;
      win_h >>= 1;
   }
   while (win_w < paperWidth || win_h < paperHeight)
   {
      if (!new_zoomed_in)
         new_zoom_scale++;
      else if (new_zoom_scale == 1)
      {
         new_zoomed_in = FALSE;
         new_zoom_scale--;
      }
      else
         new_zoom_scale--;
      win_w <<= 1;
      win_h <<= 1;
   }
   max_zoom = (new_zoomed_in) ? (MAX_ZOOMED_IN-new_zoom_scale) :
         (MAX_ZOOMED_IN+new_zoom_scale);

   XGrabPointer (mainDisplay, choiceWindow, False,
         PointerMotionMask | ButtonReleaseMask, GrabModeAsync,
         GrabModeAsync, None, handCursor, CurrentTime);

   while (!done)
   {
      XNextEvent (mainDisplay, &ev);

      if (ev.type == Expose || ev.type == VisibilityNotify)
         ExposeEventHandler (&ev, TRUE);
      else if (ev.type == ButtonRelease)
      {
         XUngrabPointer (mainDisplay, CurrentTime);
         done = TRUE;
      }
      else if (ev.type == MotionNotify)
      {
         int dx=ev.xmotion.x-orig_x, dy=ev.xmotion.y-orig_y;
         int change=((abs(dx)>abs(dy)) ? (int)(dx/res) : (int)(dy/res));

         if (change != saved_change)
         {
            zoom = initial_zoom+change;
            if (zoom < 0) zoom = 0;
            if (zoom > max_zoom) zoom = max_zoom;
            zoomedIn = (zoom < MAX_ZOOMED_IN);
            zoomScale = (zoomedIn) ? (MAX_ZOOMED_IN-zoom) :
                  (zoom-MAX_ZOOMED_IN);
            ShowZoom ();
            saved_change = change;
         }
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   if (saved_zoomed_in==zoomedIn && saved_zoom_scale==zoomScale) return;
   new_zoomed_in = zoomedIn;
   new_zoom_scale = zoomScale;
   zoomedIn = saved_zoomed_in;
   zoomScale = saved_zoom_scale;
   PreciseZoom (new_zoomed_in, new_zoom_scale, FALSE);
}

struct MouseStatusStrRec modeMouseStatus[] = {
   { "text mode", "Mode Menu", "vertex mode" },
   { "rectangle mode", "Mode Menu", "select mode" },
   { "oval mode", "Mode Menu", "text mode" },
   { "poly mode", "Mode Menu", "rectangle mode" },
   { "polygon mode", "Mode Menu", "oval mode" },
   { "arc mode", "Mode Menu", "poly mode" },
   { "rcbox mode", "Mode Menu", "polygon mode" },
   { "freehand mode", "Mode Menu", "arc mode" },
   { "vertex mode", "Mode Menu", "rcbox mode" },
   { "select mode", "Mode Menu", "freehand mode" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec colorMouseStatus[] = {
   { "next color", "Color Menu", "prev color" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec hAlignMouseStatus[] = {
   { "next horizontal align", "HoriAlign Menu", "prev horizontal align" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec vAlignMouseStatus[] = {
   { "next vertical align", "VertAlign Menu", "prev vertical align" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec pageMouseStatus[] = {
   { "next page", "Page Menu", "prev page" },
   { "drawing size", "Page Menu", "drawing size" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec pageLayoutMouseStatus[] = {
   { "tiled page mode", "PageLayout Menu", "tiled page mode" },
   { "stacked page mode", "PageLayout Menu", "stacked page mode" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec justMouseStatus[] = {
   { "center justified", "TextStyle Menu", "right justified" },
   { "right justified", "TextStyle Menu", "left justified" },
   { "left justified", "TextStyle Menu", "center justified" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec fontMouseStatus[] = {
   { "next font", "Font Menu", "prev font" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec vspaceMouseStatus[] = {
   { "inc text vspace", "modify text vspace", "dec text vspace" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec textSizeMouseStatus[] = {
   { "next text size", "TextSize Menu", "prev text size" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec rotateMouseStatus[] = {
   { "text rotate = 90", "Arrange Menu", "text rotate = 270" },
   { "text rotate = 180", "Arrange Menu", "no text rotate" },
   { "text rotate = 270", "Arrange Menu", "text rotate = 90" },
   { "no text rotate", "Arrange Menu", "text rotate = 180" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec editMouseStatus[] = {
   { "(none)", "Edit Menu", "(none)" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec specialMouseStatus[] = {
   { "(none)", "Special Menu", "(none)" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec lineWidthMouseStatus[] = {
   { "next line width", "LineWidth Menu", "prev line width" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec lineStyleMouseStatus[] = {
   { "next line style", "LineStyle Menu", "prev line style" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec lineTypeMouseStatus[] = {
   { "next line type", "LineType Menu", "prev line type" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec lineDashMouseStatus[] = {
   { "next line dash", "LineDash Menu", "prev line dash" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec rcbRadiusMouseStatus[] = {
   { "inc rcbox radius", "modify rcbox radius", "dec rcbox radius" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec zoomMouseStatus[] = {
   { "zoom in", "modify zoom", "zoom out" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec moveModeMouseStatus[] = {
   { "unconstrained move mode", "MoveMode Menu", "unconstrained move mode" },
   { "constrained move mode", "MoveMode Menu", "constrained move mode" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec shapeMouseStatus[] = {
   { "(none)", "Shape Menu", "(none)" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec stretchableTextModeMouseStatus[] = {
   { "non-stretchable text mode", "StretchText Menu", "stretchable text mode" },
   { "stretchable text mode", "StretchText Menu",
         "non-stretchable text mode" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec bwPrintMouseStatus[] = {
   { "EPS print mode", "Layout Menu", "HTML print mode" },     /* printer */
   { "PS print mode", "Layout Menu", "printer print mode" },   /* EPS */
   { "XBM print mode", "Layout Menu", "EPS print mode" },      /* PS */
   { "ASCII print mode", "Layout Menu", "PS print mode" },     /* XBM */
   { "EPSI print mode", "Layout Menu", "XBM print mode" },     /* ASCII */
   { "GIF print mode", "Layout Menu", "ASCII print mode" },    /* EPSI */
   { "HTML print mode", "Layout Menu", "EPSI print mode" },    /* GIF */
   { "printer print mode", "Layout Menu", "GIF print mode" },  /* HTML */
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec colorPrintMouseStatus[] = {
   { "EPS print mode", "Layout Menu", "HTML print mode" },     /* printer */
   { "PS print mode", "Layout Menu", "printer print mode" },   /* EPS */
   { "XPM print mode", "Layout Menu", "EPS print mode" },      /* PS */
   { "ASCII print mode", "Layout Menu", "PS print mode" },     /* XPM */
   { "EPSI print mode", "Layout Menu", "XPM print mode" },     /* ASCII */
   { "GIF print mode", "Layout Menu", "ASCII print mode" },    /* EPSI */
   { "HTML print mode", "Layout Menu", "EPSI print mode" },    /* GIF */
   { "printer print mode", "Layout Menu", "GIF print mode" },  /* HTML */
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec fileMouseStatus[] = {
   { "landscape", "File Menu", "landscape" },
   { "portrait", "File Menu", "portrait" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec fillMouseStatus[] = {
   { "next fill pattern", "Fill Menu", "prev fill pattern" },
   { NULL, NULL, NULL }
};
struct MouseStatusStrRec penMouseStatus[] = {
   { "next pen pattern", "Pen Menu", "prev pen pattern" },
   { NULL, NULL, NULL }
};

static
void QuickSetMouseStatus(pmssr)
   struct MouseStatusStrRec *pmssr;
{
   SetMouseStatus(pmssr->l, pmssr->m, pmssr->r);
}

void FormatAngle(angle, buf)
   int angle; /* degrees*64 */
   char *buf;
{
   char int_buf[80], frac_buf[4];
   float fval=(((float)rotationIncrement)/64.0*1000.0);
   int ival=round(fval), len;

   *buf = '\0';
   if (ival == 0) {
      strcpy(buf, "0"); 
      return;
   }
   sprintf(int_buf, "%04d", ival);
   len = strlen(int_buf);
   if (strcmp(&int_buf[len-3], "000") == 0) {
      *frac_buf = '\0';
   } else if (strcmp(&int_buf[len-2], "00") == 0) {
      int_buf[len-2] = '\0';
      strcpy(frac_buf, &int_buf[len-3]);
   } else if (strcmp(&int_buf[len-1], "0") == 0) {
      int_buf[len-1] = '\0';
      strcpy(frac_buf, &int_buf[len-3]);
   } else {
      strcpy(frac_buf, &int_buf[len-3]);
   }
   int_buf[len-3] = '\0';
   if (*frac_buf == '\0') {
      strcpy(buf, int_buf);
   } else {
      sprintf(buf, "%s.%s", int_buf, frac_buf);
   }
}

static
void ShowRotateMouseStatus()
{
   char left_buf[80], right_buf[80], tmp_buf[80];

   if (rotationIncrement == 0) {
      SetMouseStatus("(none)", "Arrange Menu", "(none)");
      return;
   }
   FormatAngle(rotationIncrement, tmp_buf);
   sprintf(left_buf, "Rotate %s degrees clock-wise", tmp_buf);
   sprintf(right_buf, "Rotate %s degrees counter-clock-wise", tmp_buf);
   SetMouseStatus(left_buf, "Arrange Menu", right_buf);
}

int ChoiceEventHandler(input)
   XEvent *input;
{
   XEvent ev;
   int xindex, yindex, x=0, y=0, choice;

   if (input->type == Expose) {
      XSync(mainDisplay, False);
      while (XCheckWindowEvent(mainDisplay, choiceWindow, ExposureMask, &ev)) ;
      RedrawChoiceWindow();
   } else if (input->type == EnterNotify) {
      SetMouseStatus("", "", "");
   } else if (input->type == MotionNotify) {
      xindex = (int)(input->xmotion.x / choiceImageW);
      yindex = (int)(input->xmotion.y / choiceImageH);
      choice = (xindex<<CHOICE_SHIFT)|yindex;
      switch (choice) {
      case CHOICE_MODE: QuickSetMouseStatus(&modeMouseStatus[curChoice]); break;
      case CHOICE_COLOR: QuickSetMouseStatus(&colorMouseStatus[0]); break;
      case CHOICE_HORI_ALIGN: QuickSetMouseStatus(&hAlignMouseStatus[0]); break;
      case CHOICE_VERT_ALIGN: QuickSetMouseStatus(&vAlignMouseStatus[0]); break;
      case CHOICE_PAGE:
         QuickSetMouseStatus(&pageMouseStatus[pageLayoutMode]);
         break;
      case CHOICE_PAGELAYOUT:
         QuickSetMouseStatus(&pageLayoutMouseStatus[pageLayoutMode]);
         break;
      case CHOICE_JUST: QuickSetMouseStatus(&justMouseStatus[textJust]); break;
      case CHOICE_FONT: QuickSetMouseStatus(&fontMouseStatus[0]); break;
      case CHOICE_VSPACE: QuickSetMouseStatus(&vspaceMouseStatus[0]); break;
      case CHOICE_TEXT_SIZE:
         QuickSetMouseStatus(&textSizeMouseStatus[0]);
         break;
      case CHOICE_ROTATE: ShowRotateMouseStatus(); break;
      case CHOICE_EDIT: QuickSetMouseStatus(&editMouseStatus[0]); break;
      case CHOICE_SPECIAL: QuickSetMouseStatus(&specialMouseStatus[0]); break;
      case CHOICE_LINE_WIDTH:
         QuickSetMouseStatus(&lineWidthMouseStatus[0]);
         break;
      case CHOICE_LINE_STYLE:
         QuickSetMouseStatus(&lineStyleMouseStatus[0]);
         break;
      case CHOICE_LINE_TYPE:
         QuickSetMouseStatus(&lineTypeMouseStatus[0]);
         break;
      case CHOICE_DASH: QuickSetMouseStatus(&lineDashMouseStatus[0]); break;
      case CHOICE_RADIUS: QuickSetMouseStatus(&rcbRadiusMouseStatus[0]); break;
      case CHOICE_ZOOM: QuickSetMouseStatus(&zoomMouseStatus[0]); break;
      case CHOICE_MOVE_MODE:
         QuickSetMouseStatus(&moveModeMouseStatus[moveMode]);
         break;
      case CHOICE_SHAPE: QuickSetMouseStatus(&shapeMouseStatus[0]); break;
      case CHOICE_STRETCHTEXT:
         QuickSetMouseStatus(&stretchableTextModeMouseStatus[stretchableText]);
         break;
      case CHOICE_PRINT:
         if (colorDump) {
            QuickSetMouseStatus(&colorPrintMouseStatus[whereToPrint]);
         } else {
            QuickSetMouseStatus(&bwPrintMouseStatus[whereToPrint]);
         }
         break;
      case CHOICE_FILE: QuickSetMouseStatus(&fileMouseStatus[pageStyle]); break;
      case CHOICE_FILL: QuickSetMouseStatus(&fillMouseStatus[0]); break;
      case CHOICE_PEN: QuickSetMouseStatus(&penMouseStatus[0]); break;
      }
   } else if (input->type == ButtonPress) {
      XButtonEvent *button_ev=(&(input->xbutton));
      int delta=0, root_x, root_y;
      Window root_win, child_win;
      unsigned int status;

      Msg("");
      switch (button_ev->button) {
      case Button1: delta = 1; break;
      case Button2:
         XQueryPointer(mainDisplay, rootWindow, &root_win, &child_win,
               &root_x, &root_y, &x, &y, &status);
         break;
      case Button3: delta = -1; break;
      default: return INVALID;
      }
      xindex = (int)(button_ev->x / choiceImageW);
      yindex = (int)(button_ev->y / choiceImageH);
      choice = (xindex<<CHOICE_SHIFT)|yindex;
      if (delta != 0) {
         cycleThroughChoice = TRUE;
      }
      switch (choice) {
      case CHOICE_MODE:
         if (delta == 0) {
            ModeMenu(x, y, FALSE);
         } else {
            SetCurChoice((curChoice+MAXCHOICES+delta) % MAXCHOICES);
         }
         QuickSetMouseStatus(&modeMouseStatus[curChoice]);
         break;
      case CHOICE_COLOR:
         if (colorDisplay) {
            if (delta == 0) {
               ColorMenu(x, y, FALSE);
            } else {
               if (curChoice != NOTHING &&
                     !(curChoice == DRAWTEXT && textCursorShown)) {
                  SetCurChoice(NOTHING);
               }
               colorIndex = (colorIndex+maxColors+delta) % maxColors;
               if (colorLayers && !colorLayerOn[colorIndex]) {
                  sprintf(gszMsgBox,
                        "Color %1d (%s) is currently invisible.",
                        colorIndex, colorMenuItems[colorIndex]);
                  Msg(gszMsgBox);
               }
               ShowColor(TRUE);
               if (topSel != NULL) {
                  ChangeAllSelColor(colorIndex, TRUE);
               } else if (curChoice == DRAWTEXT && textCursorShown) {
                  ChangeAllSelColor(colorIndex, FALSE);
               } else {
                  sprintf(gszMsgBox, "Color set to '%s'.",
                        colorMenuItems[colorIndex]);
                  Msg(gszMsgBox);
               }
            }
         }
         QuickSetMouseStatus(&colorMouseStatus[0]);
         break;
      case CHOICE_HORI_ALIGN:
         if (delta == 0) {
            HoriAlignMenu(x, y, FALSE);
         } else {
            HoriAlignSubMenu((horiAlign+MAXALIGNS+delta) % MAXALIGNS);
         }
         QuickSetMouseStatus(&hAlignMouseStatus[0]);
         break;
      case CHOICE_VERT_ALIGN:
         if (delta == 0) {
            VertAlignMenu(x, y, FALSE);
         } else {
            VertAlignSubMenu((vertAlign+MAXALIGNS+delta) % MAXALIGNS);
         }
         QuickSetMouseStatus(&vAlignMouseStatus[0]);
         break;
      case CHOICE_PAGE:
         if (delta == 0) {
            PageMenu(x, y, FALSE);
         } else if (pageLayoutMode == PAGE_STACK) {
            if (delta == 1) {
               NextPage();
            } else {
               PrevPage();
            }
         } else {
            SpecifyDrawingSize();
         }
         QuickSetMouseStatus(&pageMouseStatus[pageLayoutMode]);
         break;
      case CHOICE_PAGELAYOUT:
         if (delta == 0) {
            PageLayoutMenu(x, y, FALSE);
         } else {
            PageLayoutSubMenu(!pageLayoutMode);
         }
         QuickSetMouseStatus(&pageLayoutMouseStatus[pageLayoutMode]);
         break;
      case CHOICE_JUST:
         if (delta == 0) {
            StyleMenu(x, y, FALSE);
         } else {
            if (curChoice == DRAWTEXT && !textCursorShown) {
               if (TieLooseEnds()) {
                  SetCurChoice(NOTHING);
                  HighLightReverse();
               } else {
                  SetCurChoice(NOTHING);
               }
               RemoveAllSel();
            }
            textJust = (textJust+MAXJUSTS+delta) % MAXJUSTS;
            ShowJust();
            UpdateSubMenu(MENU_STYLE);
            if (topSel != NULL) {
               ChangeFontJust(textJust);
            } else if (curChoice == DRAWTEXT && textCursorShown) {
               ChangeFontJust(textJust);
            }
         }
         QuickSetMouseStatus(&justMouseStatus[textJust]);
         break;
      case CHOICE_FONT:
         if (delta == 0) {
            FontMenu(x, y, FALSE);
         } else {
            int saved_font=curFont;

            if (curChoice == DRAWTEXT && !textCursorShown) {
               if (TieLooseEnds()) {
                  SetCurChoice(NOTHING);
                  HighLightReverse();
               } else {
                  SetCurChoice(NOTHING);
               }
               RemoveAllSel();
            }
            curFont = (curFont+numFonts+delta) % numFonts;
            attemptingToSetFontProperty = TRUE;
            SetCanvasFont();
            attemptingToSetFontProperty = FALSE;
            if (canvasFontSize == INVALID) {
               sprintf(gszMsgBox, "%s-%1d not available.",
                     fontMenuStr[curFont], curSize);
               MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
               curFont = saved_font;
               SetCanvasFont();
            }
            ShowCurFont();
            ShowTextSize();
            UpdateSubMenu(MENU_FONT);
            if (topSel != NULL) {
               ChangeFont(curFont);
            } else if (curChoice == DRAWTEXT && textCursorShown) {
               ChangeFont(curFont);
            }
         }
         QuickSetMouseStatus(&fontMouseStatus[0]);
         break;
      case CHOICE_VSPACE:
         if (delta == 0) {
            TextVSpaceLoop(button_ev);
         } else {
            if (curChoice == DRAWTEXT && !textCursorShown) {
               if (TieLooseEnds()) {
                  SetCurChoice(NOTHING);
                  HighLightReverse();
               } else {
                  SetCurChoice(NOTHING);
               }
               RemoveAllSel();
            }
            if (topSel == NULL && delta < 0 &&
                  textCursorH+textVSpace+delta <= 0) {
               Msg("Text vertical spacing too small.  No change.");
            } else {
               textVSpace += delta;
               ShowTextVSpace();
               if (topSel != NULL) {
                  ChangeVSpace(textVSpace);
               } else if (curChoice == DRAWTEXT && textCursorShown) {
                  ChangeVSpace(textVSpace);
               }
            }
         }
         QuickSetMouseStatus(&vspaceMouseStatus[0]);
         break;
      case CHOICE_TEXT_SIZE:
         if (delta == 0) {
            SizeMenu(x, y, FALSE);
         } else {
            int cur_index, saved_index;

            if (curChoice == DRAWTEXT && !textCursorShown) {
               if (TieLooseEnds()) {
                  SetCurChoice(NOTHING);
                  HighLightReverse();
               } else {
                  SetCurChoice(NOTHING);
               }
               RemoveAllSel();
            }
            cur_index = saved_index = GetSizeMenuIndex();
            if (cur_index == INVALID) {
               cur_index = 0;
            } else {
               cur_index = (cur_index+numFontSizes+delta) % numFontSizes;
            }
            curSize = fontSizes[cur_index];
            attemptingToSetFontProperty = TRUE;
            SetCanvasFont();
            attemptingToSetFontProperty = FALSE;
            if (canvasFontSize == INVALID) {
               sprintf(gszMsgBox, "%s-%1d not available.",
                     fontMenuStr[curFont], curSize);
               MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
               cur_index = saved_index;
               curSize = fontSizes[cur_index];
               SetCanvasFont();
            }
            ShowCurFont();
            ShowTextSize();
            UpdateSubMenu(MENU_SIZE);
            if (topSel != NULL) {
               ChangeFontSize(cur_index);
            } else if (curChoice == DRAWTEXT && textCursorShown) {
               ChangeFontSize(cur_index);
            }
         }
         QuickSetMouseStatus(&textSizeMouseStatus[0]);
         break;
      case CHOICE_ROTATE:
         if (delta == 0) {
            ArrangeMenu(x, y, FALSE);
         } else {
            if (curChoice == DRAWTEXT) {
               if (TieLooseEnds()) {
                  SetCurChoice(NOTHING);
                  HighLightReverse();
               } else {
                  SetCurChoice(NOTHING);
               }
               RemoveAllSel();
            }
            textRotation += delta*rotationIncrement;
            while (textRotation < 0) textRotation += (360<<6);
            while (textRotation >= (360<<6)) textRotation -= (360<<6);
            ShowRotate();
            if (topSel != NULL) {
               if (delta == 1) {
                  RotateClockWise();
               } else {
                  RotateCounter();
               }
            }
         }
         ShowRotateMouseStatus();
         break;
      case CHOICE_EDIT:
         if (delta == 0) EditMenu(x, y, FALSE);
         QuickSetMouseStatus(&editMouseStatus[0]);
         break;
      case CHOICE_SPECIAL:
         if (delta == 0) SpecialMenu(x, y, FALSE);
         QuickSetMouseStatus(&specialMouseStatus[0]);
         break;
      case CHOICE_LINE_WIDTH:
         if (delta == 0) {
            LineWidthMenu(x, y, FALSE);
         } else {
            lineWidth = (lineWidth+maxLineWidths+delta) % maxLineWidths;
            ShowLineWidth();
            UpdateSubMenu(MENU_LINEWIDTH);
            if (topSel != NULL) {
               ChangeAllSelLineWidth(lineWidth, TRUE);
            } else {
               sprintf(gszMsgBox, "Line width set to %1d.",
                     curWidthOfLine[lineWidth]);
               Msg(gszMsgBox);
            }
         }
         QuickSetMouseStatus(&lineWidthMouseStatus[0]);
         break;
      case CHOICE_LINE_STYLE:
         if (delta == 0) {
            LineStyleMenu(x, y, FALSE);
         } else {
            lineStyle = (lineStyle+MAXLINESTYLES+delta) % MAXLINESTYLES;
            ShowLineStyle();
            UpdateSubMenu(MENU_LINESTYLE);
            if (topSel != NULL) ChangeAllSelLineStyle(lineStyle, TRUE);
         }
         QuickSetMouseStatus(&lineStyleMouseStatus[0]);
         break;
      case CHOICE_LINE_TYPE:
         if (delta == 0) {
            LineTypeMenu(x, y, FALSE);
         } else {
            curSpline = (curSpline+MAXLINETYPES+delta) % MAXLINETYPES;
            ShowLineType();
            UpdateSubMenu(MENU_LINETYPE);
            if (topSel != NULL) {
               ChangeAllSelLineType(curSpline, TRUE);
            } else {
               *gszMsgBox = '\0';
               switch (curSpline) {
               case LT_STRAIGHT:
                  sprintf(gszMsgBox, "Line type is 'straight'.");
                  break;
               case LT_SPLINE:
                  sprintf(gszMsgBox, "Line type is 'spline'.");
                  break;
               case LT_INTSPLINE:
                  sprintf(gszMsgBox, "Line type is 'interpolated spline'.");
                  break;
               }
               Msg(gszMsgBox);
            }
         }
         QuickSetMouseStatus(&lineTypeMouseStatus[0]);
         break;
      case CHOICE_DASH:
         if (delta == 0) {
            LineDashMenu(x, y, FALSE);
         } else {
            curDash = (curDash+MAXDASHES+delta) % MAXDASHES;
            ShowDash();
            UpdateSubMenu(MENU_LINEDASH);
            if (topSel != NULL) ChangeAllSelDashes(curDash, TRUE);
         }
         QuickSetMouseStatus(&lineDashMouseStatus[0]);
         break;
      case CHOICE_RADIUS:
         if (delta == 0) {
            RCBRadiusLoop(button_ev);
         } else {
            if (rcbRadius+delta >= MIN_RCB_RADIUS) {
               rcbRadius += delta;
               ShowRCBRadius();
               if (topSel != NULL) ChangeAllSelRCBRadius(rcbRadius);
            }
         }
         QuickSetMouseStatus(&rcbRadiusMouseStatus[0]);
         break;
      case CHOICE_ZOOM:
         switch (delta) {
         case -1: ZoomOut(); break;
         case 0: ZoomLoop(button_ev); break;
         case 1: ZoomIn(); break;
         }
         QuickSetMouseStatus(&zoomMouseStatus[0]);
         break;
      case CHOICE_MOVE_MODE:
         if (delta == 0) {
            MoveModeMenu(x, y, FALSE);
         } else {
            ToggleMoveMode();
         }
         QuickSetMouseStatus(&moveModeMouseStatus[moveMode]);
         break;
      case CHOICE_SHAPE:
         if (delta == 0) ShapeMenu(x, y, FALSE);
         QuickSetMouseStatus(&shapeMouseStatus[0]);
         break;
      case CHOICE_STRETCHTEXT:
         if (delta == 0) {
            StretchableTextModeMenu (x, y, FALSE);
         } else {
            ToggleStretchableText();
         }
         QuickSetMouseStatus(&stretchableTextModeMouseStatus[stretchableText]);
         break;
      case CHOICE_PRINT:
         if (delta == 0) {
            LayoutMenu(x, y, FALSE);
         } else {
            whereToPrint = (whereToPrint+MAXWHERETOPRINT+delta) %
                  MAXWHERETOPRINT-1;
            ToggleWhereToPrint();
         }
         break;
      case CHOICE_FILE:
         if (delta == 0) {
            return FileMenu(x, y, FALSE);
         } else {
            switch (pageStyle) {
            case PORTRAIT: ChangePageStyle(LANDSCAPE, "LandScape"); break;
            case LANDSCAPE: ChangePageStyle(PORTRAIT, "Portrait"); break;
            }
         }
         QuickSetMouseStatus(&fileMouseStatus[pageStyle]);
         break;
      case CHOICE_FILL:
         if (delta == 0) {
            FillMenu(x, y, FALSE);
         } else {
            if (curChoice == DRAWTEXT && !textCursorShown) {
               if (TieLooseEnds()) {
                  SetCurChoice(NOTHING);
                  HighLightReverse();
               } else {
                  SetCurChoice(NOTHING);
               }
               RemoveAllSel();
            }
            objFill = (objFill+MAXPATTERNS+delta) % MAXPATTERNS;
            ShowFill();
            if (topSel != NULL) {
               ChangeAllSelFill(objFill, TRUE);
            } else if (curChoice == DRAWTEXT && textCursorShown) {
               ChangeAllSelFill(objFill, FALSE);
            }
         }
         QuickSetMouseStatus(&fillMouseStatus[0]);
         break;
      case CHOICE_PEN:
         if (delta == 0) {
            PenMenu(x, y, FALSE);
         } else {
            if (curChoice == DRAWTEXT && !textCursorShown) {
               if (TieLooseEnds()) {
                  SetCurChoice(NOTHING);
                  HighLightReverse();
               } else {
                  SetCurChoice(NOTHING);
               }
               RemoveAllSel();
            }
            penPat = (penPat+MAXPATTERNS+delta) % MAXPATTERNS;
            ShowPen();
            if (topSel != NULL) {
               ChangeAllSelPen(penPat, TRUE);
            } else if (curChoice == DRAWTEXT && textCursorShown) {
               ChangeAllSelPen(penPat, FALSE);
            }
         }
         QuickSetMouseStatus(&penMouseStatus[0]);
         break;
      }
      cycleThroughChoice = FALSE;
   }
   return INVALID;
}
