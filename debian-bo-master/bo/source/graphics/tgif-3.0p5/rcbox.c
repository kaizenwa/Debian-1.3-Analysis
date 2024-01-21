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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/rcbox.c,v 3.0 1996/05/06 16:07:05 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#include "attr.e"
#include "auxtext.e"
#include "box.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "file.e"
#include "grid.e"
#include "mainloop.e"
#include "msg.e"
#include "obj.e"
#include "pattern.e"
#include "poly.e"
#include "ps.e"
#include "raster.e"
#ifndef _NO_EXTERN
#include "rcbox.e"
#endif
#include "ruler.e"
#include "select.e"
#include "setup.e"
#include "spline.e"
#include "util.e"
#include "xpixmap.e"

int	rcBoxDrawn = FALSE;

static XSegment	rcbSegs[4];
static XArc	rcbArcs[4];
static int	rcbArcsInitialized = FALSE;

void SetRCBoxVertex (x1, y1, x2, y2, r)
   int	x1, y1, x2, y2, r;
{
   register int	inc_x, inc_y, d=2*r;

   inc_x = (x2 > x1);
   inc_y = (y2 > y1);
   rcbSegs[0].x1 = (inc_x) ? (x1+r) : (x1-r); rcbSegs[0].y1 = y1;
   rcbSegs[0].x2 = (inc_x) ? (x2-r) : (x2+r); rcbSegs[0].y2 = y1;
   rcbSegs[1].x1 = x2; rcbSegs[1].y1 = (inc_y) ? (y1+r) : (y1-r);
   rcbSegs[1].x2 = x2; rcbSegs[1].y2 = (inc_y) ? (y2-r) : (y2+r);
   rcbSegs[2].x1 = (inc_x) ? (x1+r) : (x1-r); rcbSegs[2].y1 = y2;
   rcbSegs[2].x2 = (inc_x) ? (x2-r) : (x2+r); rcbSegs[2].y2 = y2;
   rcbSegs[3].x1 = x1; rcbSegs[3].y1 = (inc_y) ? (y1+r) : (y1-r);
   rcbSegs[3].x2 = x1; rcbSegs[3].y2 = (inc_y) ? (y2-r) : (y2+r);

   if (!rcbArcsInitialized)
   {
      rcbArcsInitialized = TRUE;

      rcbArcs[0].angle1 = 90*64;   rcbArcs[0].angle2 = 90*64;
      rcbArcs[1].angle1 = 0;       rcbArcs[1].angle2 = 90*64;
      rcbArcs[2].angle1 = -90*64;  rcbArcs[2].angle2 = 90*64;
      rcbArcs[3].angle1 = -180*64; rcbArcs[3].angle2 = 90*64;
   }
   rcbArcs[0].width=rcbArcs[1].width=rcbArcs[2].width=rcbArcs[3].width=d;
   rcbArcs[0].height=rcbArcs[1].height=rcbArcs[2].height=rcbArcs[3].height=d;

   if (inc_x)
   {
      if (inc_y)
      {
         rcbArcs[0].x=x1; rcbArcs[0].y=y1;
         rcbArcs[1].x=x2-d; rcbArcs[1].y=y1;
         rcbArcs[2].x=x2-d; rcbArcs[2].y=y2-d;
         rcbArcs[3].x=x1; rcbArcs[3].y=y2-d;
      }
      else
      {
         rcbArcs[0].x=x1; rcbArcs[0].y=y2;
         rcbArcs[1].x=x2-d; rcbArcs[1].y=y2;
         rcbArcs[2].x=x2-d; rcbArcs[2].y=y1-d;
         rcbArcs[3].x=x1; rcbArcs[3].y=y1-d;
      }
   }
   else
   {
      if (inc_y)
      {
         rcbArcs[0].x=x2; rcbArcs[0].y=y1;
         rcbArcs[1].x=x1-d; rcbArcs[1].y=y1;
         rcbArcs[2].x=x1-d; rcbArcs[2].y=y2-d;
         rcbArcs[3].x=x2; rcbArcs[3].y=y2-d;
      }
      else
      {
         rcbArcs[0].x=x2; rcbArcs[0].y=y2;
         rcbArcs[1].x=x1-d; rcbArcs[1].y=y2;
         rcbArcs[2].x=x1-d; rcbArcs[2].y=y1-d;
         rcbArcs[3].x=x2; rcbArcs[3].y=y1-d;
      }
   }
}

void MyRCBox (window, gc, x1, y1, x2, y2, r)
   Window	window;
   GC		gc;
   int		x1, y1, x2, y2, r;
{
   if (abs(x1-x2) < 2*r || abs(y1-y2) < 2*r)
      MyBox (window, gc, x1, y1, x2, y2);
   else
   {
      XDrawSegments (mainDisplay, window, gc, rcbSegs, 4);
      XDrawArcs (mainDisplay, window, gc, rcbArcs, 4);
   }
}
 
static
void MyFillRCBox (window, gc, x1, y1, x2, y2, r)
   Window	window;
   GC		gc;
   int		x1, y1, x2, y2, r;
{
   if (abs(x1-x2) < 2*r || abs(y1-y2) < 2*r)
      XFillRectangle (mainDisplay, window, gc, x1, y1, x2-x1, y2-y1);
   else
   {
      XFillRectangle (mainDisplay, window, gc, x1+r, y1, x2-x1-2*r, y2-y1);
      XFillRectangle (mainDisplay, window, gc, x1, y1+r, x2-x1, y2-y1-2*r);
      XFillArcs (mainDisplay, window, gc, rcbArcs, 4);
   }
}

static
void DumpRCBoxPSPath (FP, ltx, lty, rbx, rby, r, blank1, blank2)
   FILE	* FP;
   int	ltx, lty, rbx, rby, r;
   char	* blank1, * blank2;
{
   if (abs(ltx-rbx) < 2*r || abs(lty-rby) < 2*r)
   {
      fprintf (FP, "%snewpath\n%s%1d %1d moveto ", blank1, blank2, rbx, lty);
      fprintf (FP, "%1d %1d lineto ", rbx, rby);
      fprintf (FP, "%1d %1d lineto ", ltx, rby);
      fprintf (FP, "%1d %1d lineto\n", ltx, lty);
   }
   else
   {
      fprintf (FP, "%snewpath\n%s%1d %1d moveto\n", blank1, blank2, rbx-r, lty);
      fprintf (FP, "%s%1d %1d %1d %1d %1d arcto 4 {pop} repeat\n", blank2,
            rbx, lty, rbx, rby, r);
      fprintf (FP, "%s%1d %1d lineto\n", blank2, rbx, rby-r);
      fprintf (FP, "%s%1d %1d %1d %1d %1d arcto 4 {pop} repeat\n", blank2,
            rbx, rby, ltx, rby, r);
      fprintf (FP, "%s%1d %1d lineto\n", blank2, ltx+r, rby);
      fprintf (FP, "%s%1d %1d %1d %1d %1d arcto 4 {pop} repeat\n", blank2,
            ltx, rby, ltx, lty, r);
      fprintf (FP, "%s%1d %1d lineto\n", blank2, ltx, lty+r);
      fprintf (FP, "%s%1d %1d %1d %1d %1d arcto 4 {pop} repeat\n", blank2,
            ltx, lty, rbx, lty, r);
   }
}

static
void DumpRCBoxPath (FP, ObjPtr, ltx, lty, rbx, rby, r, width, pen, dash)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
   int			ltx, lty, rbx, rby, r, width, pen, dash;
{
   register int	i;

   fprintf(FP, "   gsave\n");
   if (!colorDump && useGray && pen > BACKPAT)
   {
      GrayCheck(pen);
      fprintf(FP, "      %s setgray\n", GrayStr(pen));
   }
   DumpRCBoxPSPath(FP, ltx, lty, rbx, rby, r, "      ", "         ");
   fprintf(FP, "      closepath\n");

   if (ObjPtr->ctm != NULL) {
      fprintf(FP, "      tgiforigctm setmatrix\n");
   }
   if (dash != 0)
   {
      fprintf(FP, "      [");
      for (i = 0; i < dashListLength[dash]-1; i++) {
         fprintf(FP, "%1d ", (int)(dashList[dash][i]));
      }
      fprintf(FP, "%1d] 0 setdash\n",
            (int)(dashList[dash][dashListLength[dash]-1]));
   }
   if (width != 1) fprintf(FP, "      %1d setlinewidth\n", width);
   switch (pen) {
   case SOLIDPAT: fprintf(FP, "      stroke\n"); break;
   case BACKPAT: fprintf(FP, "      1 setgray stroke 0 setgray\n"); break;
   default:
      if (colorDump || !useGray) {
         if (preDumpSetup) PSUseColorPattern();
         fprintf(FP, "      flattenpath strokepath clip newpath\n");
         DumpPatFill(FP, pen, 8, ObjPtr->bbox, "      ");
      } else {
         fprintf(FP, "      stroke\n");
      }
      break;
   }
   fprintf(FP, "   grestore\n");
}
 
void DumpRCBoxObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{
   register int	ltx, lty, rbx, rby;
   int		fill, width, pen, dash, color_index, r;

   if (ObjPtr->ctm == NULL) {
      ltx = ObjPtr->obbox.ltx; lty = ObjPtr->obbox.lty;
      rbx = ObjPtr->obbox.rbx; rby = ObjPtr->obbox.rby;
   } else {
      ltx = ObjPtr->orig_obbox.ltx; lty = ObjPtr->orig_obbox.lty;
      rbx = ObjPtr->orig_obbox.rbx; rby = ObjPtr->orig_obbox.rby;
   }

   fill = ObjPtr->detail.rcb->fill;
   pen = ObjPtr->detail.rcb->pen;
   width = ObjPtr->detail.rcb->width;
   dash = ObjPtr->detail.rcb->dash;
   r = ObjPtr->detail.rcb->radius;

   if (fill == NONEPAT && pen == NONEPAT) return;

   fprintf (FP, "%% RCBOX\n");
   if (ObjPtr->ctm != NULL) {
      float m[6];

      fprintf(FP, "gsave\n");
      m[CTM_SX] = ((float)ObjPtr->ctm->m[CTM_SX])/((float)1000.0);
      m[CTM_SY] = ((float)ObjPtr->ctm->m[CTM_SY])/((float)1000.0);
      m[CTM_SIN] = ((float)ObjPtr->ctm->m[CTM_SIN])/((float)1000.0);
      m[CTM_MSIN] = ((float)ObjPtr->ctm->m[CTM_MSIN])/((float)1000.0);
      fprintf (FP, "   %1d %1d translate\n", ObjPtr->x, ObjPtr->y);
      fprintf (FP, "   [%.3f %.3f %.3f %.3f %1d %1d] concat\n",
            m[CTM_SX], m[CTM_SIN], m[CTM_MSIN], m[CTM_SY],
            ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY]);
      fprintf (FP, "   %1d neg %1d neg translate\n", ObjPtr->x, ObjPtr->y);
   }
   color_index = ObjPtr->color;
   DumpRGBColorLine(FP, color_index, 0, TRUE);

   switch (fill)
   {
      case NONEPAT: break;
      case SOLIDPAT:
         DumpRCBoxPSPath (FP, ltx, lty, rbx, rby, r, "", "   ");
         fprintf (FP, "closepath fill\n");
         break;
      case BACKPAT:
         DumpRCBoxPSPath (FP, ltx, lty, rbx, rby, r, "", "   ");
         fprintf (FP, "closepath 1 setgray fill\n");
         DumpRGBColorLine(FP, color_index, 3, TRUE);
         break;
      default:
         fprintf (FP, "gsave\n");
         if (colorDump || !useGray)
         {
            DumpRCBoxPSPath (FP, ltx, lty, rbx, rby, r, "   ", "      ");
            fprintf (FP, "   closepath 1 setgray fill\n");
            DumpRGBColorLine(FP, color_index, 3, TRUE);
         }
         else
         {
            GrayCheck (fill);
            fprintf (FP, "   %s setgray\n", GrayStr(fill));
         }
         DumpRCBoxPSPath (FP, ltx, lty, rbx, rby, r, "   ", "      ");
         if (colorDump || !useGray)
         {
            if (preDumpSetup) PSUseColorPattern();
            fprintf (FP, "   closepath eoclip newpath\n");
            DumpPatFill (FP, fill, 8, ObjPtr->bbox, "   ");
         }
         else
            fprintf (FP, "   closepath fill\n");
         fprintf (FP, "grestore\n");
         break;
   }
   if (pen == NONEPAT) {
      if (ObjPtr->ctm != NULL) fprintf(FP, "grestore\n");
      fprintf (FP, "\n");
      return;
   }
   fprintf (FP, "gsave\n");

   if ((colorDump || !useGray) && pen > BACKPAT)
   {
      DumpRCBoxPath (FP, ObjPtr, ltx, lty, rbx, rby, r, width, BACKPAT, 0);
      DumpRGBColorLine(FP, color_index, 3, TRUE);
   }
   DumpRCBoxPath (FP, ObjPtr, ltx, lty, rbx, rby, r, width, pen, dash);

   fprintf (FP, "grestore\n");
   if (ObjPtr->ctm != NULL) fprintf(FP, "grestore\n");
   fprintf (FP, "\n");
}

int NeedsToCacheRCBoxObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   return (ObjPtr->ctm != NULL);
}

static
void MakeCachedRCBox(ObjPtr)
   struct ObjRec *ObjPtr;
{
   struct RCBoxRec *rcbox_ptr=ObjPtr->detail.rcb;
   XPoint *sv=NULL;
   IntPoint *pv;
   char *smooth=NULL;
   struct BBRec obbox;
   int i, w, h, sn;
   int r=rcbox_ptr->radius, num_vs;

   if (ObjPtr->ctm == NULL) return;

   obbox.ltx = ObjPtr->orig_obbox.ltx - ObjPtr->x;
   obbox.lty = ObjPtr->orig_obbox.lty - ObjPtr->y;
   obbox.rbx = ObjPtr->orig_obbox.rbx - ObjPtr->x;
   obbox.rby = ObjPtr->orig_obbox.rby - ObjPtr->y;
   w = obbox.rbx - obbox.ltx;
   h = obbox.rby - obbox.lty;
   num_vs = (w < (r<<1) || h < (r<<1)) ? 5 : 13;

   if (rcbox_ptr->rotated_vlist != NULL) free(rcbox_ptr->rotated_vlist);
   rcbox_ptr->rotated_n = 0;
   rcbox_ptr->rotated_vlist = (XPoint*)malloc((num_vs+1)*sizeof(XPoint));
   pv = (IntPoint*)malloc((num_vs+1)*sizeof(IntPoint));
   if (rcbox_ptr->rotated_vlist == NULL || pv == NULL) FailAllocMessage();
   if (num_vs == 13) {
      smooth = (char*)malloc((num_vs+1)*sizeof(char));
      if (smooth == NULL) FailAllocMessage();
   }
   if (num_vs == 5) {
      pv[0].x = obbox.ltx; pv[0].y = obbox.lty;
      pv[1].x = obbox.rbx; pv[1].y = obbox.lty;
      pv[2].x = obbox.rbx; pv[2].y = obbox.rby;
      pv[3].x = obbox.ltx; pv[3].y = obbox.rby;
      pv[4].x = pv[0].x; pv[4].y = pv[0].y;
   } else {
      pv[0].x  = obbox.ltx;   pv[0].y  = obbox.lty;
      pv[1].x  = obbox.ltx+r; pv[1].y  = obbox.lty;
      pv[2].x  = obbox.rbx-r; pv[2].y  = obbox.lty;
      pv[3].x  = obbox.rbx;   pv[3].y  = obbox.lty;
      pv[4].x  = obbox.rbx;   pv[4].y  = obbox.lty+r;
      pv[5].x  = obbox.rbx;   pv[5].y  = obbox.rby-r;
      pv[6].x  = obbox.rbx;   pv[6].y  = obbox.rby;
      pv[7].x  = obbox.rbx-r; pv[7].y  = obbox.rby;
      pv[8].x  = obbox.ltx+r; pv[8].y  = obbox.rby;
      pv[9].x  = obbox.ltx;   pv[9].y  = obbox.rby;
      pv[10].x = obbox.ltx;   pv[10].y = obbox.rby-r;
      pv[11].x = obbox.ltx;   pv[11].y = obbox.lty+r;
      pv[12].x = pv[0].x; pv[12].y = pv[0].y;
      for (i=0; i < num_vs; i++) smooth[i] = FALSE;
      smooth[0] = smooth[3] = smooth[6] = smooth[9] = smooth[12] = TRUE;
   }
   for (i=0; i < num_vs; i++) {
      int x, y;

      TransformPointThroughCTM(pv[i].x, pv[i].y, ObjPtr->ctm, &x, &y);
      pv[i].x = x + ObjPtr->x;
      pv[i].y = y + ObjPtr->y;
      rcbox_ptr->rotated_vlist[i].x = (short)OFFSET_X(pv[i].x);
      rcbox_ptr->rotated_vlist[i].y = (short)OFFSET_Y(pv[i].y);
   }
   if (num_vs == 13) {
      sv = MakeMultiSplinePolygonVertex(&sn, smooth, drawOrigX, drawOrigY,
            num_vs, pv);
      if (sv == NULL) FailAllocMessage();
      free(rcbox_ptr->rotated_vlist);
      rcbox_ptr->rotated_n = sn;
      rcbox_ptr->rotated_vlist = sv;
   } else {
      rcbox_ptr->rotated_n = num_vs;
   }
   free(pv);
   if (smooth != NULL) free(smooth);
}

void DrawRCBoxObj (win, XOff, YOff, ObjPtr)
   Window		win;
   int			XOff, YOff;
   struct ObjRec	* ObjPtr;
{
   struct RCBoxRec	* rcbox_ptr = ObjPtr->detail.rcb;
   int			fill, pen, pixel, ltx, lty, rbx, rby, width, dash;
   int			real_x_off, real_y_off, radius;
   XGCValues		values;

   pen = rcbox_ptr->pen;
   fill = rcbox_ptr->fill;
   width = rcbox_ptr->width;
   dash = rcbox_ptr->dash;
   radius = ZOOMED_SIZE(rcbox_ptr->radius);
   pixel = colorPixels[ObjPtr->color];

   if (NeedsToCacheRCBoxObj(ObjPtr) && rcbox_ptr->rotated_vlist==NULL) {
      MakeCachedRCBox(ObjPtr);
   }
   if (fill == 0 && pen == 0) return;

   real_x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   real_y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);
   ltx = ZOOMED_SIZE(ObjPtr->obbox.ltx - real_x_off);
   lty = ZOOMED_SIZE(ObjPtr->obbox.lty - real_y_off);
   rbx = ZOOMED_SIZE(ObjPtr->obbox.rbx - real_x_off);
   rby = ZOOMED_SIZE(ObjPtr->obbox.rby - real_y_off);

   SetRCBoxVertex(ltx, lty, rbx, rby, radius);

   if (fill != 0) {
      values.foreground = (fill == 2) ? myBgPixel : pixel;
      values.function = GXcopy;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[fill];
      XChangeGC(mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple, &values);
      if (ObjPtr->ctm != NULL) {
         XFillPolygon(mainDisplay, win, drawGC, rcbox_ptr->rotated_vlist,
               rcbox_ptr->rotated_n, Convex, CoordModeOrigin);
      } else {
         MyFillRCBox(win, drawGC, ltx, lty, rbx, rby, radius);
      }
   }
   if (pen != 0) {
      values.foreground = (pen == 2) ? myBgPixel : pixel;
      values.function = GXcopy;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[pen];
      values.line_width = ZOOMED_SIZE(width);
#ifdef NO_THIN_LINE
      if (values.line_width < 1) values.line_width = 1;
#else
#ifdef THIN_OVAL_AND_ARC
      if (values.line_width <= 1) values.line_width = 0;
#endif
#endif
      if (dash != 0) {
         XSetDashes(mainDisplay, drawGC, 0, dashList[dash],
               dashListLength[dash]);
         values.line_style = LineOnOffDash;
      } else {
         values.line_style = LineSolid;
      }
      XChangeGC(mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple | GCLineWidth |
            GCLineStyle, &values);
      if (ObjPtr->ctm != NULL) {
         XDrawLines(mainDisplay, win, drawGC, rcbox_ptr->rotated_vlist,
               rcbox_ptr->rotated_n, CoordModeOrigin);
      } else {
         MyRCBox(win, drawGC, ltx, lty, rbx, rby, radius);
      }
   }
}

static
void CreateRCBoxObj (X1, Y1, X2, Y2)
   int	X1, Y1, X2, Y2;
{
   struct RCBoxRec	* rcbox_ptr;
   struct ObjRec	* obj_ptr;
   int			width, w, ltx, lty, rbx, rby;

   rcbox_ptr = (struct RCBoxRec *)malloc(sizeof(struct RCBoxRec));
   if (rcbox_ptr == NULL) FailAllocMessage();
   memset(rcbox_ptr, 0, sizeof(struct RCBoxRec));
   rcbox_ptr->fill = objFill;
   rcbox_ptr->width = width = curWidthOfLine[lineWidth];
   UtilStrCpy(rcbox_ptr->width_spec, sizeof(rcbox_ptr->width_spec),
         curWidthOfLineSpec[lineWidth]);
   rcbox_ptr->pen = penPat;
   rcbox_ptr->dash = curDash;
   rcbox_ptr->radius = rcbRadius;

   rcbox_ptr->rotated_n = 0;
   rcbox_ptr->rotated_vlist = NULL;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));

   if (X1 < X2)
      if (Y1 < Y2)
      {
         ltx = X1; lty = Y1; rbx = X2; rby = Y2;
      }
      else
      {
         ltx = X1; lty = Y2; rbx = X2; rby = Y1;
      }
   else
      if (Y1 < Y2)
      {
         ltx = X2; lty = Y1; rbx = X1; rby = Y2;
      }
      else
      {
         ltx = X2; lty = Y2; rbx = X1; rby = Y1;
      }

   obj_ptr->bbox.ltx = obj_ptr->obbox.ltx = obj_ptr->x = ABS_X(ltx);
   obj_ptr->bbox.lty = obj_ptr->obbox.lty = obj_ptr->y = ABS_Y(lty);
   obj_ptr->bbox.rbx = obj_ptr->obbox.rbx = ABS_X(rbx);
   obj_ptr->bbox.rby = obj_ptr->obbox.rby = ABS_Y(rby);
   w = HALF_W(width);
   obj_ptr->bbox.ltx -= w;
   obj_ptr->bbox.lty -= w;
   obj_ptr->bbox.rbx += w;
   obj_ptr->bbox.rby += w;
   obj_ptr->type = OBJ_RCBOX;
   obj_ptr->color = colorIndex;
   obj_ptr->id = objId++;
   obj_ptr->dirty = FALSE;
   obj_ptr->rotation = 0;
   obj_ptr->locked = FALSE;
   obj_ptr->detail.rcb = rcbox_ptr;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;
   obj_ptr->invisible = FALSE;
   AddObj (NULL, topObj, obj_ptr);
}

static XComposeStatus	c_stat;
 
static
void ContinueRCBox (OrigX, OrigY)
   int 	OrigX, OrigY;
{
   int 			end_x, end_y, grid_x, grid_y, saved_x, saved_y;
   int 			done=FALSE, abort=FALSE;
   int			xor_pixel, radius=ZOOMED_SIZE(rcbRadius);
   char			buf[80], w_buf[80], h_buf[80], x_buf[80], y_buf[80];
   XGCValues		values;
   XEvent		input, ev;
   XMotionEvent		* motion_ev;

   xor_pixel = xorColorPixels[colorIndex];

   values.foreground = xor_pixel;
   values.function = GXxor;
   values.fill_style = FillSolid;
#ifdef NO_THIN_LINE
   values.line_width = 1;
#else
   values.line_width = 0;
#endif
   values.line_style = LineSolid;

   XChangeGC (mainDisplay, drawGC,
         GCForeground | GCFunction | GCFillStyle | GCLineWidth | GCLineStyle,
         &values);

   saved_x = grid_x = OrigX;
   saved_y = grid_y = OrigY; 
   PixelToMeasurementUnit(w_buf, 0);
   PixelToMeasurementUnit(h_buf, 0);
   PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
   PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
   sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
   StartShowMeasureCursor (grid_x, grid_y, buf, TRUE);
   BeginIntervalRulers (grid_x, grid_y, grid_x, grid_y);
   XGrabPointer (mainDisplay, drawWindow, FALSE,
         PointerMotionMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   
   SetRCBoxVertex (OrigX, OrigY, saved_x, saved_y, radius);
   while (!done)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == ButtonRelease)
      {
         XUngrabPointer (mainDisplay, CurrentTime);
         MyRCBox (drawWindow,drawGC,OrigX,OrigY,saved_x,saved_y,radius);
         EndIntervalRulers (grid_x, grid_y);
         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(saved_x-OrigX)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(saved_y-OrigY)));
         PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
         sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         EndShowMeasureCursor (saved_x, saved_y, buf, TRUE);
         done = TRUE;
      }
      else if (input.type == MotionNotify)
      {
         motion_ev = &(input.xmotion);
         end_x = motion_ev->x;
         end_y = motion_ev->y;
         GridXY (end_x, end_y, &grid_x, &grid_y);
         if (motion_ev->state & (ShiftMask | ControlMask))
         {
            int w, h, pos_w=TRUE, pos_h=TRUE;

            w = grid_x - OrigX;
            h = grid_y - OrigY;
            if (w < 0)
            {
               w = (-w);
               pos_w = FALSE;
            }
            if (h < 0)
            {
               h = (-h);
               pos_h = FALSE;
            }
            if (w > h)
               grid_x = (pos_w ? (OrigX+h) : (OrigX-h));
            else
               grid_y = (pos_h ? (OrigY+w) : (OrigY-w));
         }
         if (grid_x != saved_x || grid_y != saved_y)
         {
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(saved_x-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(saved_y-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            ShowMeasureCursor (saved_x, saved_y, buf, TRUE);
            MyRCBox (drawWindow,drawGC,OrigX,OrigY,saved_x,saved_y,radius);
            saved_x = grid_x;
            saved_y = grid_y;
            SetRCBoxVertex (OrigX, OrigY, saved_x, saved_y, radius);
            MyRCBox (drawWindow,drawGC,OrigX,OrigY,saved_x,saved_y,radius);
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(saved_x-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(saved_y-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            ShowMeasureCursor (saved_x, saved_y, buf, TRUE);
         }
         DrawIntervalRulers (OrigX, OrigY, grid_x, grid_y);
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
      else if (input.type == KeyPress)
      {
         KeySym	key_sym;
         char	s[80];

         XLookupString (&(input.xkey), s, 80-1, &key_sym, &c_stat);
         TranslateKeys (s, &key_sym);
         if (s[0] == '\033' && (key_sym & 0xff) == '\033')
         {
            XUngrabPointer (mainDisplay, CurrentTime);
            MyRCBox (drawWindow,drawGC,OrigX,OrigY,saved_x,saved_y,radius);
            EndIntervalRulers (grid_x, grid_y);
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(saved_x-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(saved_y-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            EndShowMeasureCursor (saved_x, saved_y, buf, TRUE);
            abort = TRUE;
            done = TRUE;
         }
      }
   }
   if (!abort && OrigX != grid_x && OrigY != grid_y)
   {
      CreateRCBoxObj (OrigX, OrigY, grid_x, grid_y);
      RecordNewObjCmd ();
      DrawRCBoxObj (drawWindow, drawOrigX, drawOrigY, topObj);
      rcBoxDrawn = TRUE;
      SetFileModified (TRUE);
   }
}

void DrawRCBox (input)
   XEvent	* input;
{
   XButtonEvent	* button_ev;
   int		mouse_x, mouse_y, grid_x, grid_y;

   if (input->type != ButtonPress) return;

   button_ev = &(input->xbutton);
   if (button_ev->button == Button1)
   {
      mouse_x = button_ev->x;
      mouse_y = button_ev->y;
      GridXY (mouse_x, mouse_y, &grid_x, &grid_y);
      ContinueRCBox (grid_x, grid_y);
   }
}

void SaveRCBoxObj(FP, ObjPtr)
   FILE *FP;
   struct ObjRec *ObjPtr;
{
   struct RCBoxRec *rcbox_ptr=ObjPtr->detail.rcb;

   if (fprintf(FP, "rcbox('%s',", colorMenuItems[ObjPtr->color]) == EOF) {
      writeFileFailed = TRUE;
   }
   if (fprintf(FP,
         "%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,'%s',",
         ObjPtr->obbox.ltx, ObjPtr->obbox.lty, ObjPtr->obbox.rbx,
         ObjPtr->obbox.rby, rcbox_ptr->fill, rcbox_ptr->width, rcbox_ptr->pen,
         rcbox_ptr->dash, rcbox_ptr->radius, ObjPtr->id, ObjPtr->rotation,
         ObjPtr->locked, ObjPtr->ctm!=NULL, ObjPtr->invisible,
         rcbox_ptr->width_spec) == EOF) {
      writeFileFailed = TRUE;
   }
   if (ObjPtr->ctm != NULL && fprintf(FP,
         "[\n    %1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d],",
         ObjPtr->x, ObjPtr->y,
         ObjPtr->orig_obbox.ltx, ObjPtr->orig_obbox.lty,
         ObjPtr->orig_obbox.rbx, ObjPtr->orig_obbox.rby,
         ObjPtr->ctm->m[CTM_SX], ObjPtr->ctm->m[CTM_SIN],
         ObjPtr->ctm->m[CTM_MSIN], ObjPtr->ctm->m[CTM_SY],
         ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY]) == EOF) {
      writeFileFailed = TRUE;
   }
   SaveAttrs(FP, ObjPtr->lattr);
   if (fprintf(FP, ")") == EOF) writeFileFailed = TRUE;
}

#define GETVALUE(val,name) ScanValue("%d", &(val), name, "rcbox")
#define GETSTRNG(val,name) ScanValue("%s", (val), name, "rcbox")

void ReadRCBoxObj (FP, Inbuf, ObjPtr)
   FILE			* FP;
   char			* Inbuf;
   struct ObjRec	* * ObjPtr;
{
   struct RCBoxRec	* rcbox_ptr;
   char			color_str[40], * s, msg[MAXSTRING], width_spec[40];
   int			ltx, lty, rbx, rby, fill, width, pen, dash, radius;
   int			rotation, new_alloc, id=0, w, locked=FALSE;
   int			transformed=FALSE, invisible=FALSE;

   *ObjPtr = NULL;

   s = FindChar ((int)'(', Inbuf);
   s = ParseStr (s, (int)',', color_str, sizeof(color_str));

   InitScan (s, ", \t\n");

   rotation = 0;
   *width_spec = '\0';
   if (fileVersion <= 8)
   {
      sprintf (msg, "Invalid rcbox version (%1d).", fileVersion);
      if (PRTGIF)
         fprintf (stderr, "%s\n", msg);
      else
         Msg (msg);
      return;
   }
   else if (fileVersion <= 13)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (radius,   "radius") == INVALID ||
          GETVALUE (id,       "id") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 25)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (radius,   "radius") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 32)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (radius,   "radius") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID ||
          GETVALUE (locked,   "locked") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else
   {
      if (GETVALUE (ltx,         "ltx") == INVALID ||
          GETVALUE (lty,         "lty") == INVALID ||
          GETVALUE (rbx,         "rbx") == INVALID ||
          GETVALUE (rby,         "rby") == INVALID ||
          GETVALUE (fill,        "fill") == INVALID ||
          GETVALUE (width,       "width") == INVALID ||
          GETVALUE (pen,         "pen") == INVALID ||
          GETVALUE (dash,        "dash") == INVALID ||
          GETVALUE (radius,      "radius") == INVALID ||
          GETVALUE (id,          "id") == INVALID ||
          GETVALUE (rotation,    "rotation") == INVALID ||
          GETVALUE (locked,      "locked") == INVALID ||
          GETVALUE (transformed, "transformed") == INVALID ||
          GETVALUE (invisible,   "invisible") == INVALID ||
          GETSTRNG (width_spec,  "width_spec") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
      UtilRemoveQuotes(width_spec);
   }

   if (ltx > rbx || lty > rby)
   {
      int	tmp_ltx, tmp_lty, tmp_rbx, tmp_rby;

      if (!PRTGIF) Msg ("Bad rcbox bounding box.  Adjusted.");
      CalcBBox (ltx, lty, rbx, rby, &tmp_ltx, &tmp_lty, &tmp_rbx, &tmp_rby);
      ltx = tmp_ltx; lty = tmp_lty; rbx = tmp_rbx; rby = tmp_rby;
   }

   if (fileVersion <= 16 && width <= 6) width = origWidthOfLine[width];
   if (fileVersion <= 32) {
      sprintf(width_spec, "%1d", width);
   }
   fill = UpgradePenFill (fill);
   pen = UpgradePenFill (pen);

   *ObjPtr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));
   rcbox_ptr = (struct RCBoxRec *)malloc(sizeof(struct RCBoxRec));
   if (rcbox_ptr == NULL) FailAllocMessage();
   memset(rcbox_ptr, 0, sizeof(struct RCBoxRec));

   rcbox_ptr->fill = fill;
   rcbox_ptr->width = width;
   UtilStrCpy(rcbox_ptr->width_spec, sizeof(rcbox_ptr->width_spec), width_spec);
   rcbox_ptr->pen = pen;
   rcbox_ptr->dash = dash;
   rcbox_ptr->radius = radius;

   rcbox_ptr->rotated_n = 0;
   rcbox_ptr->rotated_vlist = NULL;

   (*ObjPtr)->x = ltx;
   (*ObjPtr)->y = lty;
   (*ObjPtr)->color = QuickFindColorIndex(*ObjPtr, color_str, &new_alloc, TRUE);
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->type = OBJ_RCBOX;
   (*ObjPtr)->obbox.ltx = ltx;
   (*ObjPtr)->obbox.lty = lty;
   (*ObjPtr)->obbox.rbx = rbx;
   (*ObjPtr)->obbox.rby = rby;
   w = HALF_W(width);
   (*ObjPtr)->bbox.ltx = ltx - w;
   (*ObjPtr)->bbox.lty = lty - w;
   (*ObjPtr)->bbox.rbx = rbx + w;
   (*ObjPtr)->bbox.rby = rby + w;
   (*ObjPtr)->detail.rcb = rcbox_ptr;
   (*ObjPtr)->ctm = NULL;
   (*ObjPtr)->invisible = invisible;
   if (fileVersion >= 33 && transformed)
   {
      int real_x=0, real_y=0;
      struct BBRec orig_obbox;
      char inbuf[MAXSTRING+1];
      struct XfrmMtrxRec *ctm;

      fgets(inbuf, MAXSTRING, FP);
      scanLineNum++;
      InitScan(inbuf, "\t\n, ");

      ctm = (struct XfrmMtrxRec *)malloc(sizeof(struct XfrmMtrxRec));
      if (ctm == NULL) FailAllocMessage();
      if (GETVALUE(real_x,           "real_x") == INVALID ||
          GETVALUE(real_y,           "real_y") == INVALID ||
          GETVALUE(orig_obbox.ltx,   "orig_obbox.ltx") == INVALID ||
          GETVALUE(orig_obbox.lty,   "orig_obbox.lty") == INVALID ||
          GETVALUE(orig_obbox.rbx,   "orig_obbox.rbx") == INVALID ||
          GETVALUE(orig_obbox.rby,   "orig_obbox.rby") == INVALID ||
          GETVALUE(ctm->m[CTM_SX],   "CTM_SX") == INVALID ||
          GETVALUE(ctm->m[CTM_SIN],  "CTM_SIN") == INVALID ||
          GETVALUE(ctm->m[CTM_MSIN], "CTM_MSIN") == INVALID ||
          GETVALUE(ctm->m[CTM_SY],   "CTM_SY") == INVALID ||
          GETVALUE(ctm->m[CTM_TX],   "CTM_TX") == INVALID ||
          GETVALUE(ctm->m[CTM_TY],   "CTM_TY") == INVALID) {
         return;
      }
      (*ObjPtr)->ctm = ctm;
      if (ctm != NULL) {
         memcpy(&(*ObjPtr)->orig_obbox, &orig_obbox, sizeof(struct BBRec));
         (*ObjPtr)->x = real_x;
         (*ObjPtr)->y = real_y;
         GetTransformedOBBoxOffsetVs(*ObjPtr, (*ObjPtr)->rotated_obbox);
      }
   }
}

void FreeRCBoxObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   if (ObjPtr->detail.rcb->rotated_vlist != NULL) {
      free(ObjPtr->detail.rcb->rotated_vlist);
   }
   free(ObjPtr->detail.rcb);
   free(ObjPtr);
}
