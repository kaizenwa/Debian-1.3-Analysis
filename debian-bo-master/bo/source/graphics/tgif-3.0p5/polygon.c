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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/polygon.c,v 3.0 1996/05/06 16:06:49 william Exp $";
#endif

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#include "attr.e"
#include "box.e"
#include "cmd.e"
#include "color.e"
#include "choice.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "grid.e"
#include "file.e"
#include "mainloop.e"
#include "mark.e"
#include "msg.e"
#include "obj.e"
#include "pattern.e"
#include "poly.e"
#ifndef _NO_EXTERN
#include "polygon.e"
#endif
#include "ps.e"
#include "raster.e"
#include "rect.e"
#include "ruler.e"
#include "select.e"
#include "setup.e"
#include "spline.e"
#include "util.e"
#include "xpixmap.e"

int	polygonDrawn = FALSE;

static struct PtRec	* lastPtPtr = NULL;
static int		startPolygonX, startPolygonY;

XPoint	* MakePolygonVertex (XOff, YOff, NumVs, Vs)
   int			XOff, YOff, NumVs;
   register IntPoint	* Vs;
{
   register XPoint	* v;
   register int		i;
   int			real_x_off, real_y_off;

   real_x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   real_y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);

   v = (XPoint*)malloc((NumVs+1)*sizeof(XPoint));
   if (v == NULL) FailAllocMessage();
   for (i = 0; i < NumVs; i++) {
      v[i].x = (short)ZOOMED_SIZE(Vs[i].x-real_x_off);
      v[i].y = (short)ZOOMED_SIZE(Vs[i].y-real_y_off);
   }
   return (v);
}

void DumpPoints (FP, NumPts, V, Indent)
   FILE		* FP;
   int		NumPts, Indent;
   IntPoint	* V;
{
   register int	i, j;

   for (i = 1; i < NumPts; i++)
   {
      for (j = 0; j < Indent; j++) fprintf (FP, " ");
      fprintf (FP, "%1d %1d lineto\n", V[i].x, V[i].y);
   }
}

static
void DumpJustPolygonPath(FP, Vs, NumPts, Smooth, Curved)
   FILE *FP;
   IntPoint *Vs;
   char *Smooth;
   int NumPts, Curved;
{
   fprintf (FP, "newpath\n");
   if (Curved == LT_INTSPLINE)
      DumpCurvedPolygonPoints (FP, NumPts, Vs, 3);
   else
      DumpMultiCurvedPolygonPoints (FP, Smooth, Curved, NumPts, Vs, 3);
   fprintf (FP, "closepath\n");
}

static
void DumpPolygonPath (FP, ObjPtr, Vs, NumPts, Width, Pen, Fill, Dash)
   FILE *FP;
   struct ObjRec *ObjPtr;
   IntPoint *Vs;
   int NumPts, Width, Pen, Fill, Dash;
{
   register int	i;

   if (Fill != (-1) && Pen == (-1)) {
      /* dumping the fill */
      switch (Fill) {
      case SOLIDPAT: fprintf(FP, "   eofill\n"); break;
      case BACKPAT: fprintf(FP, "   1 setgray eofill\n"); break;
      default:
         if (colorDump || !useGray) {
            if (preDumpSetup) PSUseColorPattern();
            fprintf(FP, "   eoclip newpath\n");
            for (i=0; i < 3; i++) fprintf(FP, " ");
            DumpPatFill(FP, Fill, 8, ObjPtr->bbox, "");
         } else {
            GrayCheck(Fill);
            for (i=0; i < 3; i++) fprintf(FP, " ");
            fprintf(FP, "%s setgray\n", GrayStr(Fill));
            fprintf(FP, "   eofill\n");
         }
         break;
      }
   } else if (Fill == (-1) && Pen != (-1)) {
      /* dumping the pen */
      if (ObjPtr->ctm != NULL) {
         fprintf(FP, "   tgiforigctm setmatrix\n");
      }
      if (Width != 1) {
         fprintf(FP, "   %1d setlinewidth\n", Width);
      }
      if (Dash != 0) {
         fprintf(FP, "   [");
         for (i = 0; i < dashListLength[Dash]-1; i++) {
            fprintf(FP, "%1d ", (int)(dashList[Dash][i]));
         }
         fprintf(FP, "%1d] 0 setdash\n",
               (int)(dashList[Dash][dashListLength[Dash]-1]));
      }
      switch (Pen) {
      case SOLIDPAT: fprintf(FP, "   stroke\n"); break;
      case BACKPAT: fprintf(FP, "   1 setgray stroke 0 setgray\n"); break;
      default:
         if (colorDump || !useGray) {
            if (preDumpSetup) PSUseColorPattern();
            fprintf(FP, "   flattenpath strokepath clip newpath\n");
            for (i=0; i < 3; i++) fprintf(FP, " ");
            DumpPatFill(FP, Pen, 8, ObjPtr->bbox, "");
         } else {
            GrayCheck(Pen);
            fprintf(FP, "   %s setgray\n", GrayStr(Pen));
            fprintf(FP, "   stroke\n");
         }
         break;
      }
   }
}

void DumpPolygonObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{
   IntPoint	*v=ObjPtr->detail.g->vlist, *intv;
   int		num_pts=ObjPtr->detail.g->n;
   int		fill, width, pen, curved, dash, color_index, intn;
   char		*smooth;

   fill = ObjPtr->detail.g->fill;
   width = ObjPtr->detail.g->width;
   pen = ObjPtr->detail.g->pen;
   curved = ObjPtr->detail.g->curved;
   dash = ObjPtr->detail.g->dash;
   intv = ObjPtr->detail.g->intvlist;
   intn = ObjPtr->detail.g->intn;
   smooth = ObjPtr->detail.g->smooth;

   if (fill == NONEPAT && pen == NONEPAT) return;

   fprintf(FP, "%% POLYGON/CLOSED-SPLINE\n");
   color_index = ObjPtr->color;
   DumpRGBColorLine(FP, color_index, 0, TRUE);

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
   if (curved != LT_INTSPLINE) {
      DumpJustPolygonPath(FP, v, num_pts, smooth, curved);
   } else {
      DumpJustPolygonPath(FP, intv, intn, smooth, curved);
   }
   fprintf(FP, "gsave\n");

   if (fill != NONEPAT) {
      if (curved != LT_INTSPLINE) {
         if ((colorDump || !useGray) && fill > BACKPAT) {
            DumpPolygonPath(FP, ObjPtr, v, num_pts, width, (-1), BACKPAT, 0);
            fprintf(FP, "grestore\n");
            fprintf(FP, "gsave\n");
         }
         DumpPolygonPath(FP, ObjPtr, v, num_pts, width, (-1), fill, 0);
      } else {
         if ((colorDump || !useGray) && fill > BACKPAT) {
            DumpPolygonPath(FP, ObjPtr, intv, intn, width, (-1), BACKPAT, 0);
            fprintf(FP, "grestore\n");
            fprintf(FP, "gsave\n");
         }
         DumpPolygonPath(FP, ObjPtr, intv, intn, width, (-1), fill, 0);
      }
   }
   if (pen != NONEPAT) {
      fprintf(FP, "grestore\n");
      fprintf(FP, "gsave\n");
      if (curved != LT_INTSPLINE) {
         if ((colorDump || !useGray) && pen > BACKPAT) {
            DumpPolygonPath(FP, ObjPtr, v, num_pts, width, BACKPAT, (-1), 0);
            fprintf(FP, "grestore\n");
            fprintf(FP, "gsave\n");
         }
         DumpPolygonPath(FP, ObjPtr, v, num_pts, width, pen, (-1), dash);
      } else {
         if ((colorDump || !useGray) && pen > BACKPAT) {
            DumpPolygonPath(FP, ObjPtr, intv, intn, width, BACKPAT, (-1), 0);
            fprintf(FP, "grestore\n");
            fprintf(FP, "gsave\n");
         }
         DumpPolygonPath(FP, ObjPtr, intv, intn, width, pen, (-1), dash);
      }
   }
   fprintf(FP, "grestore\n");
   if (ObjPtr->ctm != NULL) fprintf(FP, "grestore\n");
   fprintf(FP, "\n");
}

int NeedsToCachePolygonObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   return (ObjPtr->ctm != NULL);
}

static
void MakeCachedPolygon (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register int		i;
   struct PolygonRec	* polygon_ptr=ObjPtr->detail.g;
   IntPoint		*v=polygon_ptr->vlist, *pv=NULL, *pv1=NULL;
   int			num_pts=polygon_ptr->n, num_pts1=0;

   if (ObjPtr->ctm == NULL) return;
   if (polygon_ptr->rotated_vlist != NULL) free(polygon_ptr->rotated_vlist);
   polygon_ptr->rotated_n = 0;
   polygon_ptr->rotated_vlist = NULL;
   pv = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
   if (pv == NULL) { FailAllocMessage(); return; }
   for (i=0; i < num_pts; i++) {
      int x, y;

      TransformPointThroughCTM (v[i].x-ObjPtr->x, v[i].y-ObjPtr->y,
            ObjPtr->ctm, &x, &y);
      pv[i].x = x+ObjPtr->x;
      pv[i].y = y+ObjPtr->y;
   }
   if (polygon_ptr->curved != LT_INTSPLINE) {
      polygon_ptr->rotated_vlist = MakeMultiSplinePolygonVertex (
            &(polygon_ptr->rotated_n), ObjPtr->detail.g->smooth,
            drawOrigX, drawOrigY, num_pts, pv);
   } else {
      polygon_ptr->rotated_vlist = MakeIntSplinePolygonVertex (
            &(polygon_ptr->rotated_n), &(num_pts1), &(pv1),
            drawOrigX, drawOrigY, num_pts, pv);
      free(pv1);
   }
   free(pv);
}

void DrawPolygonObj (Win, XOff, YOff, ObjPtr)
   Window		Win;
   int			XOff, YOff;
   struct ObjRec	* ObjPtr;
{
   struct PolygonRec	* polygon_ptr = ObjPtr->detail.g;
   XPoint		* v;
   int			fill, width, pen, dash, pixel;
   int			num_pts;
   XGCValues		values;

   fill = polygon_ptr->fill;
   width = polygon_ptr->width;
   pen = polygon_ptr->pen;
   dash = polygon_ptr->dash;
   pixel = colorPixels[ObjPtr->color];

   if (fill == NONEPAT && pen == NONEPAT) return;

   if (NeedsToCachePolygonObj(ObjPtr) && polygon_ptr->rotated_vlist==NULL) {
      MakeCachedPolygon(ObjPtr);
   }
   v = polygon_ptr->svlist;
   num_pts = polygon_ptr->sn;

   if (fill != 0)
   {
      values.foreground = (fill == BACKPAT) ? myBgPixel : pixel;
      values.function = GXcopy;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[fill];
      XChangeGC (mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple, &values);
      if (ObjPtr->ctm == NULL)
         XFillPolygon (mainDisplay, Win, drawGC, v, num_pts, Complex,
               CoordModeOrigin);
      else
         XFillPolygon (mainDisplay, Win, drawGC, polygon_ptr->rotated_vlist,
               polygon_ptr->rotated_n, Complex, CoordModeOrigin);
   }

   if (pen == NONEPAT) return;

   values.foreground = (pen == BACKPAT) ? myBgPixel : pixel;
   values.function = GXcopy;
   values.fill_style = FillOpaqueStippled;
   values.stipple = patPixmap[pen];
   values.line_width = ZOOMED_SIZE(width);
#ifdef NO_THIN_LINE
   if (values.line_width < 1) values.line_width = 1;
#endif
   values.join_style = JoinBevel;
   if (dash != 0)
   {
      XSetDashes (mainDisplay, drawGC, 0, dashList[dash],
            dashListLength[dash]);
      values.line_style = LineOnOffDash;
   }
   else
      values.line_style = LineSolid;
   XChangeGC (mainDisplay, drawGC,
         GCForeground | GCFunction | GCFillStyle | GCStipple |
         GCLineWidth | GCLineStyle | GCJoinStyle, &values);

   if (ObjPtr->ctm == NULL)
      XDrawLines (mainDisplay, Win, drawGC, v, num_pts, CoordModeOrigin);
   else
      XDrawLines (mainDisplay, Win, drawGC, polygon_ptr->rotated_vlist,
            polygon_ptr->rotated_n, CoordModeOrigin);
}

#define CREATE_RELATIVE (FALSE)
#define CREATE_ABSOLUTE (TRUE)

void CreatePolygonObj (NumPts, CreateAbsolute)
   int	NumPts;
   int	CreateAbsolute;
{
   register int		i;
   struct PtRec		* pt_ptr, * next_pt;
   struct PolygonRec	* polygon_ptr;
   struct ObjRec	* obj_ptr;
   IntPoint		* v;
   int			width, w, ltx, lty, rbx, rby;
   char			* smooth=NULL;

   polygon_ptr = (struct PolygonRec *)malloc(sizeof(struct PolygonRec));
   if (polygon_ptr == NULL) FailAllocMessage();
   memset(polygon_ptr, 0, sizeof(struct PolygonRec));
   polygon_ptr->n = NumPts;
   v = (IntPoint*)malloc((NumPts+1)*sizeof(IntPoint));
   if (v == NULL) FailAllocMessage();
   if (curSpline != LT_INTSPLINE) {
      smooth = (char*)malloc((NumPts+1)*sizeof(char));
      if (smooth == NULL) FailAllocMessage();
   }
   pt_ptr = lastPtPtr;
   ltx = rbx = pt_ptr->x;
   lty = rby = pt_ptr->y;
   for (i = NumPts-1; i >= 0; i--, lastPtPtr = next_pt)
   {
      next_pt = lastPtPtr->next;
      v[i].x = CreateAbsolute ? lastPtPtr->x : ABS_X(lastPtPtr->x);
      v[i].y = CreateAbsolute ? lastPtPtr->y : ABS_Y(lastPtPtr->y);
      if (curSpline != LT_INTSPLINE)
      {
         if (lastPtPtr->x < ltx) ltx = lastPtPtr->x;
         if (lastPtPtr->y < lty) lty = lastPtPtr->y;
         if (lastPtPtr->x > rbx) rbx = lastPtPtr->x;
         if (lastPtPtr->y > rby) rby = lastPtPtr->y;
         smooth[i] = (curSpline != LT_STRAIGHT);
      }
      free(lastPtPtr);
   }

   polygon_ptr->vlist = v;
   polygon_ptr->smooth = smooth;
   polygon_ptr->svlist = NULL;
   polygon_ptr->intvlist = NULL;
   polygon_ptr->fill = objFill;
   polygon_ptr->width = width = curWidthOfLine[lineWidth];
   UtilStrCpy(polygon_ptr->width_spec, sizeof(polygon_ptr->width_spec),
         curWidthOfLineSpec[lineWidth]);
   polygon_ptr->pen = penPat;
   polygon_ptr->curved = curSpline;
   polygon_ptr->dash = curDash;

   polygon_ptr->rotated_n = 0;
   polygon_ptr->rotated_vlist = NULL;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));
   obj_ptr->color = colorIndex;
   obj_ptr->type = OBJ_POLYGON;
   if (CreateAbsolute)
   {
      obj_ptr->bbox.ltx = obj_ptr->obbox.ltx = obj_ptr->x = ltx;
      obj_ptr->bbox.lty = obj_ptr->obbox.lty = obj_ptr->y = lty;
      obj_ptr->bbox.rbx = obj_ptr->obbox.rbx = rbx;
      obj_ptr->bbox.rby = obj_ptr->obbox.rby = rby;
   }
   else
   {
      obj_ptr->bbox.ltx = obj_ptr->obbox.ltx = obj_ptr->x = ABS_X(ltx);
      obj_ptr->bbox.lty = obj_ptr->obbox.lty = obj_ptr->y = ABS_Y(lty);
      obj_ptr->bbox.rbx = obj_ptr->obbox.rbx = ABS_X(rbx);
      obj_ptr->bbox.rby = obj_ptr->obbox.rby = ABS_Y(rby);
   }
   w = HALF_W(width);
   obj_ptr->bbox.ltx -= w;
   obj_ptr->bbox.lty -= w;
   obj_ptr->bbox.rbx += w;
   obj_ptr->bbox.rby += w;
   obj_ptr->id = objId++;
   obj_ptr->dirty = FALSE;
   obj_ptr->rotation = 0;
   obj_ptr->locked = FALSE;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;
   obj_ptr->invisible = FALSE;
   obj_ptr->detail.g = polygon_ptr;
   AdjObjSplineVs (obj_ptr);
   if (curSpline != LT_INTSPLINE)
      UpdPolyBBox (obj_ptr, polygon_ptr->n, polygon_ptr->vlist);
   else
      UpdPolyBBox (obj_ptr, polygon_ptr->intn, polygon_ptr->intvlist);
   AdjObjBBox (obj_ptr);
   AddObj (NULL, topObj, obj_ptr);
}

void ResetCreatePolygon()
{
   lastPtPtr = NULL;
}

void AddPtToCreatePolygon(AbsX, AbsY)
{
   struct PtRec *pt_ptr=(struct PtRec *)malloc(sizeof(struct PtRec));

   if (pt_ptr == NULL) FailAllocMessage();
   memset(pt_ptr, 0, sizeof(struct PtRec));
   pt_ptr->next = lastPtPtr;
   lastPtPtr = pt_ptr;
   pt_ptr->x = AbsX;
   pt_ptr->y = AbsY;
}

static XComposeStatus	c_stat;

static
void ContinuePolygon (OrigX, OrigY)
   int 	OrigX, OrigY;
   /* OrigX and OrigY are screen coordinates (scaled and translated). */
{
   register int		i;
   XGCValues		values;
   XEvent		input, ev;
   XButtonEvent		* button_ev;
   XMotionEvent		* motion_ev;
   KeySym		key_sym;
   char			s[80];
   char			buf[80], w_buf[80], h_buf[80], x_buf[80], y_buf[80];
   int 			end_x, end_y, grid_x, grid_y, done=FALSE;
   int 			saved_x, saved_y, closed=FALSE, abort=FALSE;
   int			xor_pixel, num_pts=1, n=2, sn=0, max_n=40, intn=0;
   int			ltx=OrigX, lty=OrigY, rbx=OrigX, rby=OrigY;
   struct PtRec		* pt_ptr;
   XPoint		*sv=NULL;
   IntPoint		*pv=NULL, *cntrlv=NULL;

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

   grid_x = end_x = saved_x = OrigX;
   grid_y = end_y = saved_y = OrigY;
   if (curSpline != LT_STRAIGHT && splineRubberband)
   {
      pv = (IntPoint*)malloc((max_n+1)*sizeof(IntPoint));
      if (pv == NULL) FailAllocMessage();
      pv[0].x = pv[1].x = pv[2].x = ABS_X(OrigX);
      pv[0].y = pv[1].y = pv[2].y = ABS_Y(OrigY);
      switch (curSpline)
      {
         case LT_SPLINE:
            sv = MakeSplinePolygonVertex (&sn, drawOrigX, drawOrigY, n+1, pv);
            break;
         case LT_INTSPLINE:
            sv = MakeIntSplinePolygonVertex (&sn, &intn, &cntrlv,
                  drawOrigX, drawOrigY, n+1, pv);
            for (i=0; i < sn; i++)
            {
               if (sv[i].x < ltx) ltx = sv[i].x;
               if (sv[i].y < lty) lty = sv[i].y;
               if (sv[i].x > rbx) rbx = sv[i].x;
               if (sv[i].y > rby) rby = sv[i].y;
            }
            break;
      }
   }
   SaveStatusStrings ();
   SetMouseStatus ("Add a vertex", "Close the polygon/spline",
         "Close the polygon/spline");
   Msg ("Click middle or right button to end polygon.");
   PixelToMeasurementUnit(w_buf, 0);
   PixelToMeasurementUnit(h_buf, 0);
   PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
   PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
   sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
   StartShowMeasureCursor (grid_x, grid_y, buf, TRUE);
   XGrabPointer (mainDisplay, drawWindow, FALSE,
         PointerMotionMask | ButtonPressMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   
   while (!done)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == KeyPress || input.type == KeyRelease)
      {
         XLookupString (&(input.xkey), s, 80-1, &key_sym, &c_stat);
         if (num_pts != 1 && !(curSpline != LT_STRAIGHT && splineRubberband) &&
               (key_sym == XK_Shift_L || key_sym == XK_Shift_R ||
               key_sym == XK_Control_L || key_sym == XK_Control_R))
         {
            XDrawLine (mainDisplay, drawWindow, drawGC, saved_x, saved_y,
                  grid_x, grid_y);
            closed = !closed;
         }
         else if (input.type == KeyPress && s[0] == '\033' &&
               (key_sym & 0xff) == '\033')
         {
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(grid_x-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(grid_y-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            EndShowMeasureCursor (grid_x, grid_y, buf, TRUE);
            abort = TRUE;
            done = TRUE;
         }
      }
      else if (input.type == MotionNotify)
      {
         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(grid_x-OrigX)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(grid_y-OrigY)));
         PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
         sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         if (curSpline != LT_STRAIGHT && splineRubberband)
            XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                  CoordModeOrigin);
         else
         {
            XDrawLine (mainDisplay, drawWindow, drawGC, OrigX, OrigY, grid_x,
                  grid_y);
            if (num_pts != 1 && closed)
               XDrawLine (mainDisplay, drawWindow, drawGC, saved_x, saved_y,
                     grid_x, grid_y);
         }
         motion_ev = &(input.xmotion);
         end_x = motion_ev->x;
         end_y = motion_ev->y;
         GridXY (end_x, end_y, &grid_x, &grid_y);
         MarkRulers (grid_x, grid_y);
         if (curSpline != LT_STRAIGHT && splineRubberband)
         {
            if (sv != NULL) free(sv);
            pv[n-1].x = ABS_X(grid_x);
            pv[n-1].y = ABS_Y(grid_y);
            switch (curSpline)
            {
               case LT_SPLINE:
                  sv = MakeSplinePolygonVertex (&sn,drawOrigX,drawOrigY,n+1,pv);
                  break;
               case LT_INTSPLINE:
                  if (cntrlv != NULL) free(cntrlv);
                  sv = MakeIntSplinePolygonVertex (&sn, &intn, &cntrlv,
                        drawOrigX, drawOrigY, n+1, pv);
                  for (i=0; i < sn; i++)
                  {
                     if (sv[i].x < ltx) ltx = sv[i].x;
                     if (sv[i].y < lty) lty = sv[i].y;
                     if (sv[i].x > rbx) rbx = sv[i].x;
                     if (sv[i].y > rby) rby = sv[i].y;
                  }
                  break;
            }
            XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                  CoordModeOrigin);
         }
         else
         {
            if (num_pts != 1)
            {
               if (motion_ev->state & (ShiftMask | ControlMask))
               {
                  XDrawLine (mainDisplay, drawWindow, drawGC, saved_x, saved_y,
                        grid_x, grid_y);
                  closed = TRUE;
               }
               else
                  closed = FALSE;
            }
            XDrawLine (mainDisplay, drawWindow, drawGC, OrigX, OrigY, grid_x,
                  grid_y);
         }
         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(grid_x-OrigX)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(grid_y-OrigY)));
         PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
         sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
      else if (input.type == ButtonPress)
      {
         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(grid_x-OrigX)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(grid_y-OrigY)));
         PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
         sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         EndShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         button_ev = &(input.xbutton);

         end_x = button_ev->x;
         end_y = button_ev->y;
         GridXY (end_x, end_y, &grid_x, &grid_y);

         if ((grid_x != OrigX || grid_y != OrigY) &&
               (button_ev->button == Button1 || num_pts != 1)) {
            num_pts++;
            pt_ptr = (struct PtRec *)malloc(sizeof(struct PtRec));
            if (pt_ptr == NULL) FailAllocMessage();
            pt_ptr->next = lastPtPtr;
            lastPtPtr = pt_ptr;
            pt_ptr->x = grid_x;
            pt_ptr->y = grid_y;
            if (curSpline != LT_STRAIGHT && splineRubberband) {
               if (n >= max_n-3) {
                  max_n += 40;
                  pv = (IntPoint *) realloc (pv, sizeof(IntPoint)*max_n+1);
                  if (pv == NULL) FailAllocMessage ();
               }
               XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                     CoordModeOrigin);
               free(sv);
               pv[n].x = ABS_X(grid_x);
               pv[n].y = ABS_Y(grid_y);
               n++;
               pv[n].x = pv[0].x;
               pv[n].y = pv[0].y;
               switch (curSpline)
               {
                  case LT_SPLINE:
                     sv = MakeSplinePolygonVertex (&sn, drawOrigX, drawOrigY,
                           n+1, pv);
                     break;
                  case LT_INTSPLINE:
                     if (cntrlv != NULL) free(cntrlv);
                     sv = MakeIntSplinePolygonVertex (&sn, &intn, &cntrlv,
                           drawOrigX, drawOrigY, n+1, pv);
                     for (i=0; i < sn; i++)
                     {
                        if (sv[i].x < ltx) ltx = sv[i].x;
                        if (sv[i].y < lty) lty = sv[i].y;
                        if (sv[i].x > rbx) rbx = sv[i].x;
                        if (sv[i].y > rby) rby = sv[i].y;
                     }
                     break;
               }
               XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                     CoordModeOrigin);
            }
         }

         if (num_pts == 2 && closed && button_ev->button == Button1 &&
               !(curSpline != LT_STRAIGHT && splineRubberband))
            XDrawLine (mainDisplay, drawWindow, drawGC, OrigX, OrigY, grid_x,
                  grid_y);

         if (grid_x == startPolygonX && grid_y == startPolygonY)
         {
            if (curSpline != LT_STRAIGHT && splineRubberband)
               XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                     CoordModeOrigin);
            else
            {
               XDrawLine (mainDisplay, drawWindow, drawGC, OrigX, OrigY,
                     grid_x, grid_y);
               if (num_pts != 1 && closed)
                  XDrawLine (mainDisplay, drawWindow, drawGC, saved_x,
                        saved_y, grid_x, grid_y);
               if (num_pts < 4)
                  XDrawLine (mainDisplay, drawWindow, drawGC, saved_x,
                        saved_y, OrigX, OrigY);
            }
            done = TRUE;
         }
         else
         {
            switch(button_ev->button)
            {
               case Button1:
                  OrigX = grid_x; OrigY = grid_y;
                  PixelToMeasurementUnit(w_buf, 0);
                  PixelToMeasurementUnit(h_buf, 0);
                  PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
                  PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
                  sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
                  StartShowMeasureCursor (grid_x, grid_y, buf, TRUE);
                  break;
               case Button2:
               case Button3:
                  if (curSpline != LT_STRAIGHT && splineRubberband)
                     XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                           CoordModeOrigin);
                  else
                  {
                     XDrawLine (mainDisplay, drawWindow, drawGC, OrigX, OrigY,
                           grid_x, grid_y);
                     if (num_pts != 1 && closed) {
                        XDrawLine (mainDisplay, drawWindow, drawGC, saved_x,
                              saved_y, grid_x, grid_y);
                     }
                     if (num_pts < 3) {
                        XDrawLine (mainDisplay, drawWindow, drawGC, saved_x,
                              saved_y, OrigX, OrigY);
                     }
                  }
                  if (num_pts == 1) {
                     done = TRUE;
                     break;
                  }
                  num_pts++;
                  pt_ptr = (struct PtRec *)malloc(sizeof(struct PtRec));
                  if (pt_ptr == NULL) FailAllocMessage();
                  pt_ptr->next = lastPtPtr;
                  lastPtPtr = pt_ptr;
                  pt_ptr->x = startPolygonX;
                  pt_ptr->y = startPolygonY;
                  done = TRUE;
                  break;
            }
         }
      }
   }
   XUngrabPointer (mainDisplay, CurrentTime);
   RestoreStatusStrings ();
   SetMouseStatus (NULL, NULL, NULL);
   Msg ("");

   if (curSpline != LT_STRAIGHT && splineRubberband) {
      free(pv); free(sv);
      if (curSpline == LT_INTSPLINE && cntrlv != NULL) free(cntrlv);
   }
   if (!abort && num_pts > 3)
   {
      CreatePolygonObj (num_pts, CREATE_RELATIVE);
      RecordNewObjCmd ();
      RedrawAnArea (botObj, topObj->bbox.ltx-GRID_ABS_SIZE(1),
            topObj->bbox.lty-GRID_ABS_SIZE(1),
            topObj->bbox.rbx+GRID_ABS_SIZE(1),
            topObj->bbox.rby+GRID_ABS_SIZE(1));
      polygonDrawn = TRUE;
      SetFileModified (TRUE);
   }
   else
   {
      struct PtRec	* pt_ptr, * next_pt;

      if (curSpline != LT_INTSPLINE)
      {
         ltx = rbx = grid_x;
         lty = rby = grid_y;
      }
      for (pt_ptr=lastPtPtr; pt_ptr != NULL; pt_ptr=next_pt)
      {
         next_pt = pt_ptr->next;
         if (curSpline != LT_INTSPLINE)
         {
            if (pt_ptr->x < ltx) ltx = pt_ptr->x;
            if (pt_ptr->y < lty) lty = pt_ptr->y;
            if (pt_ptr->x > rbx) rbx = pt_ptr->x;
            if (pt_ptr->y > rby) rby = pt_ptr->y;
         }
         free(pt_ptr);
      }
      RedrawAnArea (botObj, ABS_X(ltx)-GRID_ABS_SIZE(1),
            ABS_Y(lty)-GRID_ABS_SIZE(1), ABS_X(rbx)+GRID_ABS_SIZE(1),
            ABS_Y(rby)+GRID_ABS_SIZE(1));
      lastPtPtr = NULL;
      polygonDrawn = FALSE;
   }
}

void DrawPolygon (input)
   XEvent	* input;
{
   XButtonEvent	* button_ev;
   int		mouse_x, mouse_y, grid_x, grid_y;

   if (input->type != ButtonPress) return;

   button_ev = &(input->xbutton);
   if (button_ev->button == Button1) {
      mouse_x = button_ev->x;
      mouse_y = button_ev->y;
      GridXY (mouse_x, mouse_y, &grid_x, &grid_y);
      lastPtPtr = (struct PtRec *)malloc(sizeof(struct PtRec));
      if (lastPtPtr == NULL) FailAllocMessage();
      lastPtPtr->x = startPolygonX = grid_x;
      lastPtPtr->y = startPolygonY = grid_y;
      lastPtPtr->next = NULL;
      ContinuePolygon (grid_x, grid_y);
   } 
}

void SelectAndHighLightNewObjects (PrevTopObj)
   struct ObjRec	* PrevTopObj;
{
   register struct ObjRec	* obj_ptr;
   register struct SelRec	* sel_ptr;

   if (topSel != NULL) { HighLightReverse (); RemoveAllSel(); }
   for (obj_ptr = topObj; obj_ptr != PrevTopObj; obj_ptr = obj_ptr->next)
   {
      sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (sel_ptr == NULL) FailAllocMessage();
      sel_ptr->obj = obj_ptr;
      sel_ptr->prev = sel_ptr->next = NULL;
      AddSel (botSel, NULL, sel_ptr);
   }
   UpdSelBBox ();
   HighLightForward ();
}

void InputPolygonPts ()
{
   char			inbuf[MAXSTRING+1];
   int			more_polygon=FALSE, num_polygons=0;
   int			started_composite=FALSE;
   struct ObjRec	* saved_top_obj=topObj;

   MakeQuiescent ();
   XSync (mainDisplay, False);
   do
   {
      int		len, ok=TRUE, num_pts=0;
      int		first_x=0, first_y=0, eof=TRUE;
      struct PtRec	* pt_ptr;

      more_polygon = FALSE;
      lastPtPtr = NULL;
      printf ("Please input pairs of points: ");
      printf ("(terminate with \"<Cntrl>d<CR>\" or \".<CR>\", ");
      printf ("continue to next polygon with \";<CR>\")\n");
      printf ("> ");
      fflush (stdout);
      while (ok && fgets (inbuf, MAXSTRING, stdin) != NULL)
      {
         if (strcmp (inbuf, ";\n") == 0)
         {
            eof = FALSE;
            more_polygon = TRUE;
            break;
         }
         if (strcmp (inbuf, ".\n") == 0) { eof = FALSE; break; }
         len = strlen (inbuf);
         if (len > 0)
         {
            char * c_ptr=strtok(inbuf," ,\t\n"), * c_ptr1=NULL;
   
            if (c_ptr != NULL)
               c_ptr1 = strtok(NULL," ,\t\n");
            if (c_ptr1 != NULL)
               while (strchr (" ,\t\n", *c_ptr1)) c_ptr1++;
            while (c_ptr != NULL && c_ptr1 != NULL)
            {
               num_pts++;
               pt_ptr = (struct PtRec *)malloc(sizeof(struct PtRec));
               if (pt_ptr == NULL) FailAllocMessage();
               pt_ptr->next = lastPtPtr;
               if (sscanf (c_ptr, "%d", &pt_ptr->x) != 1 ||
                     sscanf (c_ptr1, "%d", &pt_ptr->y) != 1) {
                  ok = FALSE;
                  MsgBox ("Error reading integer for poly points.",
                        TOOL_NAME, INFO_MB);
                  XSync (mainDisplay, False);
                  break;
               }
               if (lastPtPtr == NULL)
               {
                  first_x = pt_ptr->x;
                  first_y = pt_ptr->y;
               }
               lastPtPtr = pt_ptr;
               c_ptr = strtok(NULL," ,\t\n");
               if (c_ptr != NULL)
                  c_ptr1 = strtok(NULL," ,\t\n");
               if (c_ptr1 != NULL)
                  while (strchr (" ,\t\n", *c_ptr1)) c_ptr1++;
            }
            if (c_ptr != NULL)
            {
               ok = FALSE;
               MsgBox ("Error reading integer for poly points.",
                     TOOL_NAME, INFO_MB);
               XSync (mainDisplay, False);
            }
         }
         printf ("> ");
         fflush (stdout);
      }
      printf ("\n");
      if (eof) rewind (stdin);
      if (ok && num_pts > 2)
      {
         num_polygons++;
         if (lastPtPtr->x != first_x || lastPtPtr->y != first_y) {
            num_pts++;
            pt_ptr = (struct PtRec *)malloc(sizeof(struct PtRec));
            if (pt_ptr == NULL) FailAllocMessage();
            pt_ptr->next = lastPtPtr;
            pt_ptr->x = first_x;
            pt_ptr->y = first_y;
            lastPtPtr = pt_ptr;
         }
         CreatePolygonObj (num_pts, CREATE_ABSOLUTE);
         if (more_polygon || num_polygons > 1)
         {
            if (num_polygons <= 1)
            {
               StartCompositeCmd ();
               started_composite = TRUE;
            }
            RecordNewObjCmd ();
            numRedrawBBox = 0;
            topObj->tmp_parent = NULL;
            DrawObj (drawWindow, topObj);
         }
         else
         {
            RecordNewObjCmd ();
            RedrawAnArea (botObj, topObj->bbox.ltx-GRID_ABS_SIZE(1),
                  topObj->bbox.lty-GRID_ABS_SIZE(1),
                  topObj->bbox.rbx+GRID_ABS_SIZE(1),
                  topObj->bbox.rby+GRID_ABS_SIZE(1));
            SelectTopObj ();
            SetFileModified (TRUE);
            justDupped = FALSE;
         }
      }
      if (ok && num_pts <= 2)
      {
         MsgBox ("Too few points.", TOOL_NAME, INFO_MB);
         XSync (mainDisplay, False);
      }
      for ( ; lastPtPtr != NULL; lastPtPtr=pt_ptr) {
         pt_ptr = lastPtPtr->next;
         free(pt_ptr);
      }
   } while (more_polygon);
   if (num_polygons > 1 || started_composite)
   {
      SelectAndHighLightNewObjects (saved_top_obj);
      GroupSelObj ();
      EndCompositeCmd ();

      SetFileModified (TRUE);
      justDupped = FALSE;
   }
}

static
void AddToLastPoint(XOff, YOff)
   int XOff, YOff;
{
   struct PtRec *pt_ptr=(struct PtRec *)malloc(sizeof(struct PtRec));

   if (pt_ptr == NULL) FailAllocMessage();
   pt_ptr->next = lastPtPtr;
   lastPtPtr = pt_ptr;
   pt_ptr->x = XOff;
   pt_ptr->y = YOff;
}

void GetBoundingBox()
{
   struct SelRec *sel_ptr, *tmp_top_sel=NULL, *tmp_bot_sel=NULL;
   int num_created=0;

   if (topSel == NULL) return;
   if (curChoice == VERTEXMODE) SetCurChoice(NOTHING);

   tmp_top_sel = tmp_bot_sel = NULL;

   HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      struct ObjRec *obj_ptr=sel_ptr->obj;
      struct SelRec *new_sel_ptr;

      if (obj_ptr->ctm == NULL) {
         struct BBRec *p_obbox=(&obj_ptr->obbox);

         AddToLastPoint(p_obbox->ltx, p_obbox->lty);
         AddToLastPoint(p_obbox->ltx, p_obbox->rby);
         AddToLastPoint(p_obbox->rbx, p_obbox->rby);
         AddToLastPoint(p_obbox->rbx, p_obbox->lty);
         AddToLastPoint(p_obbox->ltx, p_obbox->lty);
      } else {
         int i;

         lastPtPtr = NULL;
         for (i=0; i < 5; i++) {
            AddToLastPoint(obj_ptr->rotated_obbox[i].x,
                  obj_ptr->rotated_obbox[i].y);
         }
      }
      CreatePolygonObj(5, CREATE_RELATIVE);

      new_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (new_sel_ptr == NULL) FailAllocMessage();
      new_sel_ptr->obj = topObj;
      new_sel_ptr->next = NULL;
      new_sel_ptr->prev = NULL;
      PrepareToRecord(CMD_NEW, NULL, NULL, 0);
      RecordCmd(CMD_NEW, NULL, new_sel_ptr, new_sel_ptr, 1);

      new_sel_ptr->next = tmp_top_sel;
      if (tmp_top_sel == NULL) {
         tmp_bot_sel = new_sel_ptr;
      } else {
         tmp_top_sel->prev = new_sel_ptr;
      }
      tmp_top_sel = new_sel_ptr;
      num_created++;
   }
   EndCompositeCmd();

   RemoveAllSel();
   topSel = tmp_top_sel;
   botSel = tmp_bot_sel;
   UpdSelBBox();
   RedrawAnArea(botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward();
   justDupped = FALSE;
   SetFileModified(TRUE);
}

void SavePolygonObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{
   register int		i, n;
   int			count;
   struct PolygonRec	* polygon_ptr = ObjPtr->detail.g;

   n = polygon_ptr->n;
   if (fprintf (FP, "polygon('%s',%1d,[\n\t",
         colorMenuItems[ObjPtr->color], polygon_ptr->n) == EOF) {
      writeFileFailed = TRUE;
   }
   for (i=0, count = 0; i < n-1; i++) {
      if (fprintf (FP, "%1d,%1d,", polygon_ptr->vlist[i].x,
            polygon_ptr->vlist[i].y) == EOF) {
         writeFileFailed = TRUE;
      }
      if (++count == 8) {
         count = 0;
         if (fprintf (FP, "\n\t") == EOF) writeFileFailed = TRUE;
      }
   }
   if (fprintf (FP, "%1d,%1d],",
         polygon_ptr->vlist[n-1].x, polygon_ptr->vlist[n-1].y) == EOF)
      writeFileFailed = TRUE;

   if (fprintf (FP,
         "%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,'%s',",
         polygon_ptr->fill, polygon_ptr->width, polygon_ptr->pen,
         polygon_ptr->curved, ObjPtr->id, polygon_ptr->dash, ObjPtr->rotation,
         ObjPtr->locked, ObjPtr->ctm!=NULL, ObjPtr->invisible,
         polygon_ptr->width_spec) == EOF) {
      writeFileFailed = TRUE;
   }
   if (fprintf (FP, "\n    \"") == EOF) writeFileFailed = TRUE;
   SaveSmoothHinge (FP, polygon_ptr->curved, polygon_ptr->n,
         polygon_ptr->smooth);
   if (fprintf (FP, "\",") == EOF) writeFileFailed = TRUE;
   if (ObjPtr->ctm != NULL && fprintf(FP,
         "[\n\t%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d],",
         ObjPtr->x, ObjPtr->y,
         ObjPtr->orig_obbox.ltx, ObjPtr->orig_obbox.lty,
         ObjPtr->orig_obbox.rbx, ObjPtr->orig_obbox.rby,
         ObjPtr->ctm->m[CTM_SX], ObjPtr->ctm->m[CTM_SIN],
         ObjPtr->ctm->m[CTM_MSIN], ObjPtr->ctm->m[CTM_SY],
         ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY]) == EOF) {
      writeFileFailed = TRUE;
   }
   SaveAttrs (FP, ObjPtr->lattr);
   if (fprintf (FP, ")") == EOF) writeFileFailed = TRUE;
}

#define GETVALUE(val,name) ScanValue("%d", &(val), name, "polygon")
#define GETSTRNG(val,name) ScanValue("%s", (val), name, "polygon")

void ReadPolygonObj (FP, Inbuf, ObjPtr)
   FILE			* FP;
   char			* Inbuf;
   struct ObjRec	* * ObjPtr;
{
   register int		i;
   struct PolygonRec	* polygon_ptr;
   IntPoint		* v;
   char			color_str[40], * s, inbuf[MAXSTRING], msg[MAXSTRING];
   int			num_pts, ltx=0, lty=0, rbx=0, rby=0, x, y, id=0;
   int			fill, width, pen, w, new_alloc, locked=FALSE;
   int			curved, dash, initialized, rotation, count;
   int			real_x=0, real_y=0, transformed=FALSE, invisible=FALSE;
   char			* smooth=NULL, width_spec[40];
   struct XfrmMtrxRec	*ctm=NULL;
   struct BBRec		orig_obbox;

   *ObjPtr = NULL;

   s = FindChar ((int)'(', Inbuf);
   s = ParseStr (s, (int)',', color_str, sizeof(color_str));

   InitScan (s, "\t\n, []");

   if (GETVALUE (num_pts, "number of points") == INVALID)
      return;

   if (num_pts <= 0)
   {
      (void) sprintf (msg, "%s, %d:  Invalid number of points in polygon",
            scanFileName, scanLineNum);
      if (PRTGIF)
         fprintf (stderr, "%s\n", msg);
      else
         Msg (msg);
      return;
   }

   *ObjPtr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));
   polygon_ptr = (struct PolygonRec *)malloc(sizeof(struct PolygonRec));
   if (polygon_ptr == NULL) FailAllocMessage();
   memset(polygon_ptr, 0, sizeof(struct PolygonRec));

   if (num_pts < 3)
   {
      v = (IntPoint*)malloc(5*sizeof(IntPoint));
      if (v == NULL) FailAllocMessage();
      smooth = (char*)malloc(5*sizeof(char));
      if (smooth == NULL) FailAllocMessage();
   }
   else
   {
      v = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
      if (v == NULL) FailAllocMessage();
      smooth = (char*)malloc((num_pts+1)*sizeof(char));
      if (smooth == NULL) FailAllocMessage();
   }

   initialized = FALSE;

   *width_spec = '\0';
   if (fileVersion <= 13)
   {
      for (i = 0; i < num_pts; i++)
      {
         if (GETVALUE (x, "x") == INVALID || GETVALUE (y, "y") == INVALID)
         {
            free(*ObjPtr);
            free(polygon_ptr);
            free(v);
            *ObjPtr = NULL;
            return;
         }
         v[i].x = x; v[i].y = y;
         if (!initialized)
         {
            initialized = TRUE;
            ltx = rbx = x; lty = rby = y;
         }
         else
         {
            if (x < ltx) ltx = x; if (y < lty) lty = y;
            if (x > rbx) rbx = x; if (y > rby) rby = y;
         }
      }
   }
   else
   {
      fgets (inbuf, MAXSTRING, FP);
      scanLineNum++;
      s = inbuf;
      InitScan (s, "\t\n, []");
      for (i = 0, count = 0; i < num_pts; i++)
      {
         if (GETVALUE (x, "x") == INVALID || GETVALUE (y, "y") == INVALID)
         {
            free(*ObjPtr);
            free(polygon_ptr);
            free(v);
            *ObjPtr = NULL;
            return;
         }
         v[i].x = x; v[i].y = y;
         if (!initialized)
         {
            initialized = TRUE;
            ltx = rbx = x; lty = rby = y;
         }
         else
         {
            if (x < ltx) ltx = x; if (y < lty) lty = y;
            if (x > rbx) rbx = x; if (y > rby) rby = y;
         }
         if (++count == 8 && i != num_pts-1)
         {
            count = 0;
            fgets (inbuf, MAXSTRING, FP);
            scanLineNum++;
            s = inbuf;
            InitScan (s, "\t\n, []");
         }
      }
   }

   switch (num_pts)
   {
      case 1:
         sprintf (msg, "%s (%1d,%1d) converted to 3-point polygon.",
               "1-point polygon", v[0].x, v[0].y);
         if (PRTGIF)
            fprintf (stderr, "%s\n", msg);
         else
            Msg (msg);
         v[3].x = v[2].x = v[1].x = v[0].x;
         v[3].y = v[2].y = v[1].y = v[0].y;
         num_pts = 4;
         break;
      case 2:
         sprintf (msg, "%s [%1d,%1d,%1d,%1d] converted to 3-point polygon.",
               "2-point polygon", v[0].x, v[0].y, v[1].x, v[1].y);
         if (PRTGIF)
            fprintf (stderr, "%s\n", msg);
         else
            Msg (msg);
         v[3].x = v[2].x = v[0].x;
         v[3].y = v[2].y = v[0].y;
         num_pts = 4;
         break;
      case 3:
         sprintf (msg, "%s [%1d,%1d,%1d,%1d] converted to 3-point polygon.",
               "2-point polygon", v[0].x, v[0].y, v[1].x, v[1].y);
         if (PRTGIF)
            fprintf (stderr, "%s\n", msg);
         else
            Msg (msg);
         v[3].x = v[2].x = v[0].x;
         v[3].y = v[2].y = v[0].y;
         num_pts = 4;
         break;
   }

   polygon_ptr->n = num_pts;

   dash = 0;
   rotation = 0;
   if (fileVersion <= 3)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (width == LINE_CURVED)
      {
         width = 0;
         curved = TRUE;
      }
      else
         curved = FALSE;
      switch (width)
      {
         case 1: width = 3; break;
         case 2: width = 6; break;
      }
      id = objId++;
   }
   else if (fileVersion <= 5)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      switch (width)
      {
         case 1: width = 3; break;
         case 2: width = 6; break;
      }
      id = objId++;
   }
   else if (fileVersion <= 7)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      id = objId++;
   }
   else if (fileVersion <= 8)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (id,       "id") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 13)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 25)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 32)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID ||
          GETVALUE (locked,   "locked") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else
   {
#ifdef _TGIF_DBG
      if (GETVALUE (fill,        "fill") == INVALID ||
          GETVALUE (width,       "width") == INVALID ||
          GETVALUE (pen,         "pen") == INVALID ||
          GETVALUE (curved,      "curved") == INVALID ||
          GETVALUE (id,          "id") == INVALID ||
          GETVALUE (dash,        "dash") == INVALID ||
          GETVALUE (rotation,    "rotation") == INVALID ||
          GETVALUE (locked,      "locked") == INVALID ||
          GETVALUE (transformed, "transformed") == INVALID ||
          GETVALUE (invisible,   "invisible") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
      if (GETSTRNG(width_spec,  "width_spec") != INVALID) {
         UtilRemoveQuotes(width_spec);
      } else {
         sprintf(width_spec, "%1d", width);
         fprintf(stderr, "Bad width_spec in polygon... Fix this file!\n");
      }
#else /* ~_TGIF_DBG */
      if (GETVALUE (fill,        "fill") == INVALID ||
          GETVALUE (width,       "width") == INVALID ||
          GETVALUE (pen,         "pen") == INVALID ||
          GETVALUE (curved,      "curved") == INVALID ||
          GETVALUE (id,          "id") == INVALID ||
          GETVALUE (dash,        "dash") == INVALID ||
          GETVALUE (rotation,    "rotation") == INVALID ||
          GETVALUE (locked,      "locked") == INVALID ||
          GETVALUE (transformed, "transformed") == INVALID ||
          GETVALUE (invisible,   "invisible") == INVALID ||
          GETSTRNG (width_spec,  "width_spec") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
      UtilRemoveQuotes(width_spec);
#endif /* _TGIF_DBG */
   }

   if (fileVersion <= 16 && width <= 6) width = origWidthOfLine[width];

   if (fileVersion <= 25 && curved > 1) curved = 0;
   if (curved == LT_INTSPLINE && smooth != NULL)
   {
      free(smooth);
      smooth = NULL;
   }
   if (fileVersion <= 30)
   {
      switch (curved)
      {
         case LT_STRAIGHT:
            for (i=0; i < num_pts; i++) smooth[i] = FALSE;
            break;
         case LT_SPLINE:
            for (i=0; i < num_pts; i++) smooth[i] = TRUE;
            break;
      }
   }
   else if (!ReadSmoothHinge (FP, curved, num_pts, smooth))
      return;

   if (fileVersion >= 33 && transformed) {
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
   }
   if (fileVersion <= 32) {
      sprintf(width_spec, "%1d", width);
   }

   fill = UpgradePenFill (fill);
   pen = UpgradePenFill (pen);

   polygon_ptr->fill = fill;
   polygon_ptr->width = width;
   UtilStrCpy(polygon_ptr->width_spec, sizeof(polygon_ptr->width_spec),
         width_spec);
   polygon_ptr->pen = pen;
   polygon_ptr->curved = curved;
   polygon_ptr->dash = dash;

   polygon_ptr->vlist = v;
   polygon_ptr->smooth = smooth;
   polygon_ptr->svlist = NULL;
   polygon_ptr->intvlist = NULL;

   polygon_ptr->rotated_n = 0;
   polygon_ptr->rotated_vlist = NULL;

   (*ObjPtr)->x = ltx;
   (*ObjPtr)->y = lty;
   (*ObjPtr)->color = QuickFindColorIndex(*ObjPtr, color_str, &new_alloc, TRUE);
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->type = OBJ_POLYGON;
   (*ObjPtr)->obbox.ltx = ltx;
   (*ObjPtr)->obbox.lty = lty;
   (*ObjPtr)->obbox.rbx = rbx;
   (*ObjPtr)->obbox.rby = rby;
   w = HALF_W(width);
   (*ObjPtr)->bbox.ltx = ltx - w;
   (*ObjPtr)->bbox.lty = lty - w;
   (*ObjPtr)->bbox.rbx = rbx + w;
   (*ObjPtr)->bbox.rby = rby + w;
   (*ObjPtr)->detail.g = polygon_ptr;
   (*ObjPtr)->ctm = ctm;
   (*ObjPtr)->invisible = invisible;
   if (ctm != NULL) {
      memcpy(&(*ObjPtr)->orig_obbox, &orig_obbox, sizeof(struct BBRec));
      (*ObjPtr)->x = real_x;
      (*ObjPtr)->y = real_y;
      GetTransformedOBBoxOffsetVs(*ObjPtr, (*ObjPtr)->rotated_obbox);
   }
   AdjObjCache (*ObjPtr);
   AdjObjSplineVs (*ObjPtr);
   if (polygon_ptr->curved != LT_INTSPLINE)
      UpdPolyBBox (*ObjPtr, polygon_ptr->n, polygon_ptr->vlist);
   else
      UpdPolyBBox (*ObjPtr, polygon_ptr->intn, polygon_ptr->intvlist);
}

void FreePolygonObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   if (ObjPtr->detail.g->intvlist != NULL) free(ObjPtr->detail.g->intvlist);
   if (ObjPtr->detail.g->svlist != NULL) free(ObjPtr->detail.g->svlist);
   if (ObjPtr->detail.g->rotated_vlist != NULL) {
      free(ObjPtr->detail.g->rotated_vlist);
   }
   free(ObjPtr->detail.g->vlist);
   if (ObjPtr->detail.g->smooth != NULL) free(ObjPtr->detail.g->smooth);
   free(ObjPtr->detail.g);
   free(ObjPtr);
}
