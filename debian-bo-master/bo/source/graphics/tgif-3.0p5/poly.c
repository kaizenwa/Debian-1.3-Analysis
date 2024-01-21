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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/poly.c,v 3.0 1996/05/06 16:06:45 william Exp $";
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
#include "auxtext.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "file.e"
#include "grid.e"
#include "mainloop.e"
#include "msg.e"
#include "obj.e"
#include "pattern.e"
#ifndef _NO_EXTERN
#include "poly.e"
#endif
#include "polygon.e"
#include "ps.e"
#include "raster.e"
#include "rect.e"
#include "ruler.e"
#include "select.e"
#include "setup.e"
#include "spline.e"
#include "stretch.e"
#include "util.e"
#include "xpixmap.e"

#define RETREAT (0.8)

int	polyDrawn = FALSE;

/* short	widthOfLine[] = { 0, 3,  6,  0 }; */
/* short	arrowHeadH[]  = { 3, 5,  10, 3 }; */
/* short	arrowHeadW[]  = { 8, 12, 22, 8 }; */

short	origWidthOfLine[] = { 1, 2,  3,  4,  5,  6,  7 };
short	origArrowHeadH[]  = { 3, 4,  5,  6,  7,  8,  9 };
short	origArrowHeadW[]  = { 8, 10, 12, 14, 18, 20, 22 };

short	*curWidthOfLine=NULL;
short	*curArrowHeadH=NULL;
short	*curArrowHeadW=NULL;

char	**curWidthOfLineSpec=NULL;
char	**curArrowHeadHSpec=NULL;
char	**curArrowHeadWSpec=NULL;

int	drawPolyToEndInANode=0;
char	drawPolyFirstNodeName[MAXSTRING+1];
char	drawPolyLastNodeName[MAXSTRING+1];

struct ObjRec *drawPolyHighlightedNode=NULL;

static struct PtRec	* lastPtPtr=NULL;

XPoint	* MakePolyVertex (XOff, YOff, NumVs, Vs)
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

void CalcPolyBBox (ObjPtr)
   struct ObjRec *ObjPtr;
{
   register int x, y;
   struct PolyRec *poly_ptr=ObjPtr->detail.p;
   int style=poly_ptr->style, width=poly_ptr->width;
   int aw=poly_ptr->aw, ah=poly_ptr->ah;
   int ltx=ObjPtr->obbox.ltx, lty=ObjPtr->obbox.lty;
   int rbx=ObjPtr->obbox.rbx, rby=ObjPtr->obbox.rby;
   int dx, dy, tmp_x, tmp_y, num_pts;
   double len, sin, cos, w, h;
   IntPoint *v;
   int retracted_arrow=(RetractedArrowAttr(ObjPtr) ||
         AutoRetractedArrowAttr(ObjPtr, TRUE));

   num_pts = (poly_ptr->curved==LT_INTSPLINE ? poly_ptr->intn : poly_ptr->n);
   v = (poly_ptr->curved==LT_INTSPLINE ? poly_ptr->intvlist : poly_ptr->vlist);
   dx = v[1].x - v[0].x;
   dy = v[1].y - v[0].y;
   if ((style & LS_LEFT) && (dx != 0 || dy != 0))
   {
      len = (double)sqrt((double)(((double)dx)*((double)dx) +
            ((double)dy)*((double)dy)));
      sin = ((double)dy) / len;
      cos = ((double)dx) / len;

      w = (double)aw; h = (double)max(ah,(width>>1));

      x = round(v[0].x + w*cos - h*sin);
      y = round(v[0].y + w*sin + h*cos);
      if (ObjPtr->ctm != NULL) {
         TransformPointThroughCTM(x-ObjPtr->x, y-ObjPtr->y, ObjPtr->ctm,
               &tmp_x, &tmp_y);
         x = tmp_x+ObjPtr->x;
         y = tmp_y+ObjPtr->y;
      }
      if (x < ltx) ltx = x; if (y < lty) lty = y;
      if (x > rbx) rbx = x; if (y > rby) rby = y;

      x = round(v[0].x + w*cos + h*sin);
      y = round(v[0].y + w*sin - h*cos);
      if (ObjPtr->ctm != NULL) {
         TransformPointThroughCTM(x-ObjPtr->x, y-ObjPtr->y, ObjPtr->ctm,
               &tmp_x, &tmp_y);
         x = tmp_x+ObjPtr->x;
         y = tmp_y+ObjPtr->y;
      }
      if (x < ltx) ltx = x; if (y < lty) lty = y;
      if (x > rbx) rbx = x; if (y > rby) rby = y;
   }

   dx = v[num_pts-1].x - v[num_pts-2].x;
   dy = v[num_pts-1].y - v[num_pts-2].y;
   if ((style & LS_RIGHT) && (dx != 0 || dy != 0))
   {
      len = (double)sqrt((double)(((double)dx)*((double)dx) +
            ((double)dy)*((double)dy)));
      sin = ((double)dy) / len;
      cos = ((double)dx) / len;

      w = (double)aw; h = (double)max(ah,(width>>1));

      x = round(v[num_pts-1].x - w*cos + h*sin);
      y = round(v[num_pts-1].y - w*sin - h*cos);
      if (ObjPtr->ctm != NULL) {
         TransformPointThroughCTM(x-ObjPtr->x, y-ObjPtr->y, ObjPtr->ctm,
               &tmp_x, &tmp_y);
         x = tmp_x+ObjPtr->x;
         y = tmp_y+ObjPtr->y;
      }
      if (x < ltx) ltx = x; if (y < lty) lty = y;
      if (x > rbx) rbx = x; if (y > rby) rby = y;

      x = round(v[num_pts-1].x - w*cos - h*sin);
      y = round(v[num_pts-1].y - w*sin + h*cos);
      if (ObjPtr->ctm != NULL) {
         TransformPointThroughCTM(x-ObjPtr->x, y-ObjPtr->y, ObjPtr->ctm,
               &tmp_x, &tmp_y);
         x = tmp_x+ObjPtr->x;
         y = tmp_y+ObjPtr->y;
      }
      if (x < ltx) ltx = x; if (y < lty) lty = y;
      if (x > rbx) rbx = x; if (y > rby) rby = y;
   }

   if (retracted_arrow) {
      int i;

      for (i=1; i < num_pts; i++) {
         x = v[i].x; y = v[i].y;
         if (x-ah < ltx) ltx = x-ah; if (y-ah < lty) lty = y-ah;
         if (x+ah > rbx) rbx = x+ah; if (y+ah > rby) rby = y+ah;
      }
   }
   ObjPtr->bbox.ltx = min(ltx, ObjPtr->obbox.ltx-(width>>1));
   ObjPtr->bbox.lty = min(lty, ObjPtr->obbox.lty-(width>>1));
   ObjPtr->bbox.rbx = max(rbx, ObjPtr->obbox.rbx+(width>>1));
   ObjPtr->bbox.rby = max(rby, ObjPtr->obbox.rby+(width>>1));
}

void UpdPolyBBox (ObjPtr, NumPts, V)
   struct ObjRec	* ObjPtr;
   int			NumPts;
   IntPoint		* V;
{
   register int	i;
   int		ltx, lty, rbx, rby;

   ltx = rbx = V[0].x;
   lty = rby = V[0].y;

   for (i = 1; i < NumPts; i++)
   {
      if (V[i].x < ltx) ltx = V[i].x; if (V[i].y < lty) lty = V[i].y;
      if (V[i].x > rbx) rbx = V[i].x; if (V[i].y > rby) rby = V[i].y;
   }
   if (ObjPtr->ctm == NULL) {
      ObjPtr->x = ltx;
      ObjPtr->y = lty;
      ObjPtr->obbox.ltx = ltx;
      ObjPtr->obbox.lty = lty;
      ObjPtr->obbox.rbx = rbx;
      ObjPtr->obbox.rby = rby;
   } else {
      IntPoint abs_obj_obbox_vs[5];

      ObjPtr->orig_obbox.ltx = ltx;
      ObjPtr->orig_obbox.lty = lty;
      ObjPtr->orig_obbox.rbx = rbx;
      ObjPtr->orig_obbox.rby = rby;
      GetTransformedOBBoxAbsVs(ObjPtr, abs_obj_obbox_vs);
      ObjPtr->obbox.ltx = min(min(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
            min(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
      ObjPtr->obbox.rbx = max(max(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
            max(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
      ObjPtr->obbox.lty = min(min(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
            min(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));
      ObjPtr->obbox.rby = max(max(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
            max(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));
   }
   AdjObjBBox (ObjPtr);
}

#define CREATE_RELATIVE (FALSE)
#define CREATE_ABSOLUTE (TRUE)

void CreatePolyObj (NumPts, CreateAbsolute)
   int	NumPts;
   int	CreateAbsolute;
{
   struct PtRec		* pt_ptr, * next_pt;
   struct PolyRec	* poly_ptr;
   struct ObjRec	* obj_ptr;
   register int		i;
   IntPoint		* v;
   int			ltx, lty, rbx, rby;
   char			* smooth=NULL;

   poly_ptr = (struct PolyRec *)malloc(sizeof(struct PolyRec));
   if (poly_ptr == NULL) FailAllocMessage();
   memset(poly_ptr, 0, sizeof(struct PolyRec));
   poly_ptr->n = NumPts;
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
         if (curSpline == LT_STRAIGHT)
            smooth[i] = FALSE;
         else
            smooth[i] = (i != 0 && i != NumPts-1);
      }
      free(lastPtPtr);
   }

   poly_ptr->vlist = v;
   poly_ptr->smooth = smooth;
   poly_ptr->svlist = poly_ptr->asvlist = NULL;
   poly_ptr->intvlist = NULL;
   poly_ptr->style = lineStyle;
   poly_ptr->width = curWidthOfLine[lineWidth];
   poly_ptr->aw = curArrowHeadW[lineWidth];
   poly_ptr->ah = curArrowHeadH[lineWidth];
   UtilStrCpy(poly_ptr->width_spec, sizeof(poly_ptr->width_spec),
         curWidthOfLineSpec[lineWidth]);
   UtilStrCpy(poly_ptr->aw_spec, sizeof(poly_ptr->aw_spec),
         curArrowHeadWSpec[lineWidth]);
   UtilStrCpy(poly_ptr->ah_spec, sizeof(poly_ptr->ah_spec),
         curArrowHeadHSpec[lineWidth]);
   poly_ptr->pen = penPat;
   poly_ptr->curved = curSpline;
   poly_ptr->fill = objFill;
   poly_ptr->dash = curDash;

   poly_ptr->rotated_n = 0;
   poly_ptr->rotated_vlist = NULL;
   poly_ptr->rotated_asn = 0;
   poly_ptr->rotated_asvlist = NULL;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));
   obj_ptr->color = colorIndex;
   obj_ptr->type = OBJ_POLY;
   if (CreateAbsolute)
   {
      obj_ptr->obbox.ltx = obj_ptr->x = ltx;
      obj_ptr->obbox.lty = obj_ptr->y = lty;
      obj_ptr->obbox.rbx = rbx;
      obj_ptr->obbox.rby = rby;
   }
   else
   {
      obj_ptr->obbox.ltx = obj_ptr->x = ABS_X(ltx);
      obj_ptr->obbox.lty = obj_ptr->y = ABS_Y(lty);
      obj_ptr->obbox.rbx = ABS_X(rbx);
      obj_ptr->obbox.rby = ABS_Y(rby);
   }
   obj_ptr->id = objId++;
   obj_ptr->dirty = FALSE;
   obj_ptr->rotation = 0;
   obj_ptr->locked = FALSE;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;
   obj_ptr->invisible = FALSE;
   obj_ptr->detail.p = poly_ptr;
   AdjObjSplineVs (obj_ptr);
   if (curSpline != LT_INTSPLINE)
      UpdPolyBBox (obj_ptr, poly_ptr->n, poly_ptr->vlist);
   else
      UpdPolyBBox (obj_ptr, poly_ptr->intn, poly_ptr->intvlist);
   AdjObjBBox (obj_ptr);
   AddObj (NULL, topObj, obj_ptr);
}

void ResetCreatePoly()
{
   lastPtPtr = NULL;
}

void AddPtToCreatePoly(AbsX, AbsY)
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
void ContinuePoly (OrigX, OrigY)
   int 	OrigX, OrigY;
   /* OrigX and OrigY are screen coordinates (scaled and translated). */
   /* OrigX and OrigY are also on grid. */
{
   register int		i;
   XGCValues		values;
   XEvent		input, ev;
   XButtonEvent		* button_ev;
   XMotionEvent		* motion_ev;
   int			xor_pixel, abort=FALSE;
   int 			end_x, end_y, grid_x, grid_y, done=FALSE, num_pts=1;
   int			last_x=OrigX, last_y=OrigY, n=2, sn=0, max_n=40, intn=0;
   int			ltx=OrigX, lty=OrigY, rbx=OrigX, rby=OrigY;
   int			one_line_status=FALSE;
   char			status_buf[MAX_STATUS_BTNS+1][MAXSTRING+1];
   char			buf[80], w_buf[80], h_buf[80], x_buf[80], y_buf[80];
   struct PtRec		* pt_ptr;
   XPoint		*sv=NULL;
   IntPoint		*v=NULL, *cntrlv=NULL;

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

   grid_x = end_x = OrigX;
   grid_y = end_y = OrigY;
   if (curSpline != LT_STRAIGHT && splineRubberband)
   {
      v = (IntPoint*)malloc((max_n+1)*sizeof(IntPoint));
      if (v == NULL) FailAllocMessage();
      v[0].x = v[1].x = v[2].x = ABS_X(OrigX);
      v[0].y = v[1].y = v[2].y = ABS_Y(OrigY);
      switch (curSpline)
      {
         case LT_SPLINE:
            sv = MakeSplinePolyVertex (&sn, drawOrigX, drawOrigY, n, v);
            break;
         case LT_INTSPLINE:
            sv = MakeIntSplinePolyVertex (&sn, &intn, &cntrlv,
                  drawOrigX, drawOrigY, n, v);
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
   if (curChoice == FREEHAND)
   {
      Msg ("Release button to terminate freehand.");
      PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
      PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
      sprintf (buf, "%s%s", x_buf, y_buf);
      StartShowMeasureCursor (grid_x, grid_y, buf, TRUE);
      XGrabPointer (mainDisplay, drawWindow, FALSE,
            PointerMotionMask | ButtonReleaseMask,
            GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   }
   else
   {
      SetMouseStatus ("Add a vertex", "Add last vertex", "Add last vertex");
      Msg ("Click middle or right button to end poly.");
      PixelToMeasurementUnit(w_buf, 0);
      PixelToMeasurementUnit(h_buf, 0);
      PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
      PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
      sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
      StartShowMeasureCursor (grid_x, grid_y, buf, TRUE);
      XGrabPointer (mainDisplay, drawWindow, FALSE,
            PointerMotionMask | ButtonPressMask,
            GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   }
   if (drawPolyToEndInANode > 0) {
      *drawPolyLastNodeName = '\0';
      drawPolyHighlightedNode = NULL;
      SaveStatusStringsIntoBuf(status_buf, &one_line_status);
      values.line_width = 3;
      XChangeGC(mainDisplay, revGrayGC, GCLineWidth, &values);
   }
   while (!done)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == MotionNotify)
      {
         if (curChoice == FREEHAND)
         {
            PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
            sprintf (buf, "%s%s", x_buf, y_buf);
         }
         else
         {
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(grid_x-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(grid_y-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         }
         ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         if (curSpline != LT_STRAIGHT && splineRubberband)
         {
#ifdef HP_LINE_BUG
            if (sn == 2)
            {
               XPoint hp_sv[3];

               hp_sv[0].x = sv[0].x; hp_sv[0].y = sv[0].y;
               hp_sv[1].x = sv[0].x; hp_sv[1].y = sv[0].y;
               hp_sv[2].x = sv[1].x; hp_sv[2].y = sv[1].y;
               XDrawLines (mainDisplay, drawWindow, drawGC, hp_sv, 3,
                     CoordModeOrigin);
            }
            else
               XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                     CoordModeOrigin);
#else
            XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                  CoordModeOrigin);
#endif
         }
         else
            XDrawLine (mainDisplay, drawWindow, drawGC, OrigX, OrigY, grid_x,
                  grid_y);
         motion_ev = &(input.xmotion);
         end_x = motion_ev->x;
         end_y = motion_ev->y;
         if (curChoice == FREEHAND)
         {
            grid_x = end_x;
            grid_y = end_y;
            MarkRulers (grid_x, grid_y);
            if (curSpline != LT_STRAIGHT && splineRubberband)
            {
               if (sv != NULL) free(sv);
               v[n-1].x = v[n].x = ABS_X(grid_x);
               v[n-1].y = v[n].y = ABS_Y(grid_y);
               switch (curSpline)
               {
                  case LT_SPLINE:
                     sv = MakeSplinePolyVertex (&sn,drawOrigX,drawOrigY,n,v);
                     break;
                  case LT_INTSPLINE:
                     if (cntrlv != NULL) free(cntrlv);
                     sv = MakeIntSplinePolyVertex (&sn, &intn, &cntrlv,
                           drawOrigX, drawOrigY, n, v);
                     for (i=0; i < sn; i++)
                     {
                        if (sv[i].x < ltx) ltx = sv[i].x;
                        if (sv[i].y < lty) lty = sv[i].y;
                        if (sv[i].x > rbx) rbx = sv[i].x;
                        if (sv[i].y > rby) rby = sv[i].y;
                     }
                     break;
               }
#ifdef HP_LINE_BUG
               if (sn == 2)
               {
                  XPoint hp_sv[3];

                  hp_sv[0].x = sv[0].x; hp_sv[0].y = sv[0].y;
                  hp_sv[1].x = sv[0].x; hp_sv[1].y = sv[0].y;
                  hp_sv[2].x = sv[1].x; hp_sv[2].y = sv[1].y;
                  XDrawLines (mainDisplay, drawWindow, drawGC, hp_sv, 3,
                        CoordModeOrigin);
               }
               else
                  XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                        CoordModeOrigin);
#else
               XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                     CoordModeOrigin);
#endif
            }
            else
               XDrawLine (mainDisplay, drawWindow, drawGC, OrigX, OrigY, grid_x,
                     grid_y);
            while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;

            if (grid_x != last_x || grid_y != last_y)
            {
               num_pts++;
               pt_ptr = (struct PtRec *)malloc(sizeof(struct PtRec));
               if (pt_ptr == NULL) FailAllocMessage();
               pt_ptr->next = lastPtPtr;
               lastPtPtr = pt_ptr;
               pt_ptr->x = last_x = grid_x;
               pt_ptr->y = last_y = grid_y;
               if (curSpline != LT_STRAIGHT && splineRubberband)
               {
                  if (n >= max_n-2)
                  {
                     max_n += 40;
                     v = (IntPoint *) realloc (v, sizeof(IntPoint)*max_n+1);
                     if (v == NULL) FailAllocMessage ();
                  }
#ifdef HP_LINE_BUG
                  if (sn == 2)
                  {
                     XPoint hp_sv[3];

                     hp_sv[0].x = sv[0].x; hp_sv[0].y = sv[0].y;
                     hp_sv[1].x = sv[0].x; hp_sv[1].y = sv[0].y;
                     hp_sv[2].x = sv[1].x; hp_sv[2].y = sv[1].y;
                     XDrawLines (mainDisplay, drawWindow, drawGC, hp_sv, 3,
                           CoordModeOrigin);
                  }
                  else
                     XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                           CoordModeOrigin);
#else
                  XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                        CoordModeOrigin);
#endif
                  if (sv != NULL) free(sv);
                  v[n].x = v[n+1].x = ABS_X(grid_x);
                  v[n].y = v[n+1].y = ABS_Y(grid_y);
                  n++;
                  switch (curSpline)
                  {
                     case LT_SPLINE:
                        sv = MakeSplinePolyVertex (&sn,drawOrigX,drawOrigY,n,v);
                        break;
                     case LT_INTSPLINE:
                        if (cntrlv != NULL) free(cntrlv);
                        sv = MakeIntSplinePolyVertex (&sn, &intn, &cntrlv,
                              drawOrigX, drawOrigY, n, v);
                        for (i=0; i < sn; i++)
                        {
                           if (sv[i].x < ltx) ltx = sv[i].x;
                           if (sv[i].y < lty) lty = sv[i].y;
                           if (sv[i].x > rbx) rbx = sv[i].x;
                           if (sv[i].y > rby) rby = sv[i].y;
                        }
                        break;
                  }
#ifdef HP_LINE_BUG
                  if (sn == 2)
                  {
                     XPoint hp_sv[3];

                     hp_sv[0].x = sv[0].x; hp_sv[0].y = sv[0].y;
                     hp_sv[1].x = sv[0].x; hp_sv[1].y = sv[0].y;
                     hp_sv[2].x = sv[1].x; hp_sv[2].y = sv[1].y;
                     XDrawLines (mainDisplay, drawWindow, drawGC, hp_sv, 3,
                           CoordModeOrigin);
                  }
                  else
                     XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                           CoordModeOrigin);
#else
                  XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                        CoordModeOrigin);
#endif
               }
            }
            OrigX = grid_x; OrigY = grid_y;
            PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
            sprintf (buf, "%s%s", x_buf, y_buf);
            ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         }
         else
         {
            if (drawPolyToEndInANode > 0) {
               int need_to_highlight=FALSE, something_changed=FALSE;
               struct ObjRec *owner_obj=NULL, *obj_ptr, *obj_under_cursor=NULL;
               struct AttrRec *attr_ptr;

               *drawPolyLastNodeName = '\0';
               obj_ptr = FindAnObj(end_x, end_y, &owner_obj, &obj_under_cursor,
                     drawPolyLastNodeName);
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
                  if (*drawPolyLastNodeName != '\0') {
                     SetStringStatus(drawPolyLastNodeName);
                  } else {
                     RestoreStatusStringsFromBuf (status_buf, one_line_status);
                  }
               }
               if (drawPolyHighlightedNode != NULL) {
                  grid_x = OFFSET_X((drawPolyHighlightedNode->obbox.ltx +
                        drawPolyHighlightedNode->obbox.rbx)>>1);
                  grid_y = OFFSET_Y((drawPolyHighlightedNode->obbox.lty +
                        drawPolyHighlightedNode->obbox.rby)>>1);
               } else {
                  GridXY (end_x, end_y, &grid_x, &grid_y);
               }
            } else {
               GridXY (end_x, end_y, &grid_x, &grid_y);
            }
            MarkRulers (grid_x, grid_y);
            if (curSpline != LT_STRAIGHT && splineRubberband)
            {
               if (sv != NULL) free(sv);
               v[n-1].x = v[n].x = ABS_X(grid_x);
               v[n-1].y = v[n].y = ABS_Y(grid_y);
               switch (curSpline)
               {
                  case LT_SPLINE:
                     sv = MakeSplinePolyVertex (&sn,drawOrigX,drawOrigY,n,v);
                     break;
                  case LT_INTSPLINE:
                     free(cntrlv);
                     sv = MakeIntSplinePolyVertex (&sn, &intn, &cntrlv,
                           drawOrigX, drawOrigY, n, v);
                     for (i=0; i < sn; i++)
                     {
                        if (sv[i].x < ltx) ltx = sv[i].x;
                        if (sv[i].y < lty) lty = sv[i].y;
                        if (sv[i].x > rbx) rbx = sv[i].x;
                        if (sv[i].y > rby) rby = sv[i].y;
                     }
                     break;
               }
#ifdef HP_LINE_BUG
               if (sn == 2)
               {
                  XPoint hp_sv[3];

                  hp_sv[0].x = sv[0].x; hp_sv[0].y = sv[0].y;
                  hp_sv[1].x = sv[0].x; hp_sv[1].y = sv[0].y;
                  hp_sv[2].x = sv[1].x; hp_sv[2].y = sv[1].y;
                  XDrawLines (mainDisplay, drawWindow, drawGC, hp_sv, 3,
                        CoordModeOrigin);
               }
               else
                  XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                        CoordModeOrigin);
#else
               XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                     CoordModeOrigin);
#endif
            }
            else
               XDrawLine (mainDisplay, drawWindow, drawGC, OrigX, OrigY, grid_x,
                     grid_y);
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(grid_x-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(grid_y-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
            while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
         }
      }
      else if (input.type == ButtonPress && curChoice != FREEHAND)
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
         if (drawPolyHighlightedNode != NULL) {
            grid_x = OFFSET_X((drawPolyHighlightedNode->obbox.ltx +
                  drawPolyHighlightedNode->obbox.rbx)>>1);
            grid_y = OFFSET_Y((drawPolyHighlightedNode->obbox.lty +
                  drawPolyHighlightedNode->obbox.rby)>>1);
         } else {
            GridXY (end_x, end_y, &grid_x, &grid_y);
         }
         if (grid_x != last_x || grid_y != last_y)
         {
            num_pts++;
            pt_ptr = (struct PtRec *)malloc(sizeof(struct PtRec));
            if (pt_ptr == NULL) FailAllocMessage();
            pt_ptr->next = lastPtPtr;
            lastPtPtr = pt_ptr;
            pt_ptr->x = last_x = grid_x;
            pt_ptr->y = last_y = grid_y;
            if (curSpline != LT_STRAIGHT && splineRubberband)
            {
               if (n >= max_n-2)
               {
                  max_n += 40;
                  v = (IntPoint *) realloc (v, sizeof(IntPoint)*max_n+1);
                  if (v == NULL) FailAllocMessage ();
               }
#ifdef HP_LINE_BUG
               if (sn == 2)
               {
                  XPoint hp_sv[3];

                  hp_sv[0].x = sv[0].x; hp_sv[0].y = sv[0].y;
                  hp_sv[1].x = sv[0].x; hp_sv[1].y = sv[0].y;
                  hp_sv[2].x = sv[1].x; hp_sv[2].y = sv[1].y;
                  XDrawLines (mainDisplay, drawWindow, drawGC, hp_sv, 3,
                        CoordModeOrigin);
               }
               else
                  XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                        CoordModeOrigin);
#else
               XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                     CoordModeOrigin);
#endif
               if (sv != NULL) free(sv);
               v[n].x = v[n+1].x = ABS_X(grid_x);
               v[n].y = v[n+1].y = ABS_Y(grid_y);
               n++;
               switch (curSpline)
               {
                  case LT_SPLINE:
                     sv = MakeSplinePolyVertex (&sn,drawOrigX,drawOrigY,n,v);
                     break;
                  case LT_INTSPLINE:
                     if (cntrlv != NULL) free(cntrlv);
                     sv = MakeIntSplinePolyVertex (&sn, &intn, &cntrlv,
                           drawOrigX, drawOrigY, n, v);
                     for (i=0; i < sn; i++)
                     {
                        if (sv[i].x < ltx) ltx = sv[i].x;
                        if (sv[i].y < lty) lty = sv[i].y;
                        if (sv[i].x > rbx) rbx = sv[i].x;
                        if (sv[i].y > rby) rby = sv[i].y;
                     }
                     break;
               }
#ifdef HP_LINE_BUG
               if (sn == 2)
               {
                  XPoint hp_sv[3];

                  hp_sv[0].x = sv[0].x; hp_sv[0].y = sv[0].y;
                  hp_sv[1].x = sv[0].x; hp_sv[1].y = sv[0].y;
                  hp_sv[2].x = sv[1].x; hp_sv[2].y = sv[1].y;
                  XDrawLines (mainDisplay, drawWindow, drawGC, hp_sv, 3,
                        CoordModeOrigin);
               }
               else
                  XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                        CoordModeOrigin);
#else
               XDrawLines (mainDisplay, drawWindow, drawGC, sv, sn,
                     CoordModeOrigin);
#endif
            }
         }

         switch (button_ev->button)
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
            case Button2: done = TRUE; break;
            case Button3: done = TRUE; break;
         }
      }
      else if (input.type == ButtonRelease && curChoice == FREEHAND)
      {
         PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
         sprintf (buf, "%s%s", x_buf, y_buf);
         EndShowMeasureCursor (grid_x, grid_y, buf, TRUE);

         button_ev = &(input.xbutton);

         if (grid_x != last_x || grid_y != last_y)
         {
            num_pts++;
            pt_ptr = (struct PtRec *)malloc(sizeof(struct PtRec));
            if (pt_ptr == NULL) FailAllocMessage();
            pt_ptr->next = lastPtPtr;
            lastPtPtr = pt_ptr;
            pt_ptr->x = last_x = grid_x;
            pt_ptr->y = last_y = grid_y;
         }
         done = TRUE;
      }
      else if (input.type == KeyPress)
      {
         KeySym	key_sym;
         char	s[80];

         XLookupString (&(input.xkey), s, 80-1, &key_sym, &c_stat);
         TranslateKeys (s, &key_sym);
         if (s[0] == '\033' && (key_sym & 0xff) == '\033')
         {
            if (curChoice == FREEHAND)
            {
               PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
               PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
               sprintf (buf, "%s%s", x_buf, y_buf);
            }
            else
            {
               PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(grid_x-OrigX)));
               PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(grid_y-OrigY)));
               PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
               PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
               sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            }
            EndShowMeasureCursor (grid_x, grid_y, buf, TRUE);
            abort = TRUE;
            done = TRUE;
         }
      }
   }
   if (drawPolyHighlightedNode != NULL) {
      SelBox(drawWindow, revGrayGC,
            OFFSET_X(drawPolyHighlightedNode->bbox.ltx)-2,
            OFFSET_Y(drawPolyHighlightedNode->bbox.lty)-2,
            OFFSET_X(drawPolyHighlightedNode->bbox.rbx)+2,
            OFFSET_Y(drawPolyHighlightedNode->bbox.rby)+2);
      drawPolyHighlightedNode = NULL;
   }
   XUngrabPointer (mainDisplay, CurrentTime);
   if (drawPolyToEndInANode > 0) {
      values.line_width = 1;
      XChangeGC(mainDisplay, revGrayGC, GCLineWidth, &values);
      RestoreStatusStringsFromBuf(status_buf, one_line_status);
   }
   RestoreStatusStrings ();
   SetMouseStatus (NULL, NULL, NULL);
   Msg ("");

   if (curSpline != LT_STRAIGHT && splineRubberband) {
      free(v); free(sv);
      if (curSpline == LT_INTSPLINE && cntrlv != NULL) free(cntrlv);
   }

   if (!abort && num_pts > 1)
   {
      CreatePolyObj (num_pts, CREATE_RELATIVE);
      RecordNewObjCmd ();
      RedrawAnArea (botObj, topObj->bbox.ltx-GRID_ABS_SIZE(1),
            topObj->bbox.lty-GRID_ABS_SIZE(1),
            topObj->bbox.rbx+GRID_ABS_SIZE(1),
            topObj->bbox.rby+GRID_ABS_SIZE(1));
      polyDrawn = TRUE;
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
      polyDrawn = FALSE;
   }
   if (drawPolyToEndInANode == 2 && !polyDrawn) {
      drawPolyToEndInANode = (-1);
   }
}

void DrawPoly (input)
   XEvent	* input;
{
   int		mouse_x, mouse_y, grid_x, grid_y;
   XButtonEvent	* button_ev;

   if (input->type != ButtonPress) return;

   button_ev = &(input->xbutton);
   if (button_ev->button == Button1)
   {
      mouse_x = input->xbutton.x;
      mouse_y = input->xbutton.y;
      if (drawPolyToEndInANode > 0 || curChoice == FREEHAND) {
         if (drawPolyHighlightedNode != NULL) {
            grid_x = OFFSET_X((drawPolyHighlightedNode->obbox.ltx +
                  drawPolyHighlightedNode->obbox.rbx)>>1);
            grid_y = OFFSET_Y((drawPolyHighlightedNode->obbox.lty +
                  drawPolyHighlightedNode->obbox.rby)>>1);
            SelBox(drawWindow, revGrayGC,
                  OFFSET_X(drawPolyHighlightedNode->bbox.ltx)-2,
                  OFFSET_Y(drawPolyHighlightedNode->bbox.lty)-2,
                  OFFSET_X(drawPolyHighlightedNode->bbox.rbx)+2,
                  OFFSET_Y(drawPolyHighlightedNode->bbox.rby)+2);
            drawPolyHighlightedNode = NULL;
         } else {
            grid_x = mouse_x;
            grid_y = mouse_y;
         }
      } else {
         GridXY (mouse_x, mouse_y, &grid_x, &grid_y);
      }
      lastPtPtr = (struct PtRec *)malloc(sizeof(struct PtRec));
      if (lastPtPtr == NULL) FailAllocMessage();
      lastPtPtr->x = grid_x;
      lastPtPtr->y = grid_y;
      lastPtPtr->next = NULL;
      ContinuePoly (grid_x, grid_y);
   } 
}

void InputPolyPts ()
{
   char			inbuf[MAXSTRING+1];
   int			more_poly=FALSE, num_polys=0;
   int			started_composite=FALSE;
   struct ObjRec	* saved_top_obj=topObj;

   MakeQuiescent ();
   XSync (mainDisplay, False);
   do
   {
      int		len, ok=TRUE, num_pts=0, eof=TRUE;
      struct PtRec	* pt_ptr;

      more_poly = FALSE;
      lastPtPtr = NULL;
      printf ("Please input pairs of points: ");
      printf ("(terminate with \"<Cntrl>d<CR>\" or \".<CR>\", ");
      printf ("continue to next poly with \";<CR>\")\n");
      printf ("> ");
      fflush (stdout);
      while (ok && fgets (inbuf, MAXSTRING, stdin) != NULL)
      {
         if (strcmp (inbuf, ";\n") == 0)
         {
            more_poly = TRUE;
            eof = FALSE;
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
      if (ok && num_pts > 1)
      {
         num_polys++;
         CreatePolyObj (num_pts, CREATE_ABSOLUTE);
         if (more_poly || num_polys > 1)
         {
            if (num_polys <= 1)
            {
               StartCompositeCmd ();
               started_composite = TRUE;
            }
            RecordNewObjCmd ();
            numRedrawBBox = 0;
            topObj->tmp_parent = NULL;
            DrawObj(drawWindow, topObj);
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
      if (ok && num_pts <= 1) {
         MsgBox("Too few points.", TOOL_NAME, INFO_MB);
         XSync(mainDisplay, False);
      }
      for ( ; lastPtPtr != NULL; lastPtPtr=pt_ptr) {
         pt_ptr = lastPtPtr->next;
         free(pt_ptr);
      }
   } while (more_poly);
   if (num_polys > 1 || started_composite)
   {
      SelectAndHighLightNewObjects (saved_top_obj);
      GroupSelObj ();
      EndCompositeCmd ();

      SetFileModified (TRUE);
      justDupped = FALSE;
   }
}

static
void FinishJoinPoly(obj_ptr1, obj_ptr2, poly_ptr1, vs, smooth, num_pts)
   struct ObjRec *obj_ptr1, *obj_ptr2;
   struct PolyRec *poly_ptr1;
   IntPoint *vs;
   char *smooth;
   int num_pts;
{
   struct SelRec *sel_ptr, *top_sel=NULL, *bot_sel=NULL;
   int x0=0, y0=0, index0=0;

   if (curChoice == VERTEXMODE) {
      if (topVSel->obj == obj_ptr1) {
         index0 = topVSel->v_index[0];
         x0 = topVSel->x[0];
         y0 = topVSel->y[0];
      } else {
         index0 = botVSel->v_index[0];
         x0 = botVSel->x[0];
         y0 = botVSel->y[0];
      }
   }
   RemoveAllSel();
   if (obj_ptr2->fattr != NULL) {
      int count=0;
      struct SelRec *next_sel;

      PrepareToReplaceAnObj(obj_ptr2);
      DetachAllObjAttrs(obj_ptr2, &top_sel, &bot_sel);
      /* obj_ptr2 is pointed to by bot_sel now */
      AdjObjBBox(obj_ptr2);
      for (sel_ptr=top_sel; sel_ptr != NULL; sel_ptr=sel_ptr->next) count++;
      RecordCmd(CMD_ONE_TO_MANY, NULL, top_sel, bot_sel, count);
      sel_ptr = bot_sel;
      bot_sel = bot_sel->prev;
      bot_sel->next = NULL;
      free(sel_ptr);
      if (curChoice == VERTEXMODE) {
         for (sel_ptr=top_sel; sel_ptr != NULL; sel_ptr=next_sel) {
            next_sel = sel_ptr->next;
            free(sel_ptr);
         }
         top_sel = bot_sel = NULL;
      }
   }
   sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (sel_ptr == NULL) FailAllocMessage();
   sel_ptr->next = sel_ptr->prev = NULL;
   sel_ptr->obj = obj_ptr2;
   PrepareToRecord(CMD_DELETE, sel_ptr, sel_ptr, 1);
   UnlinkObj(obj_ptr2);
   FreeObj(obj_ptr2);
   RecordCmd(CMD_DELETE, NULL, NULL, NULL, 0);
   free(sel_ptr);

   PrepareToReplaceAnObj(obj_ptr1);

   if (poly_ptr1->vlist != NULL) free(poly_ptr1->vlist);
   if (poly_ptr1->svlist != NULL) free(poly_ptr1->svlist);
   if (poly_ptr1->asvlist != NULL) free(poly_ptr1->asvlist);
   if (poly_ptr1->smooth != NULL) free(poly_ptr1->smooth);
   if (poly_ptr1->intvlist != NULL) free(poly_ptr1->intvlist);
   if (poly_ptr1->rotated_vlist != NULL) free(poly_ptr1->rotated_vlist);
   if (poly_ptr1->rotated_asvlist != NULL) free(poly_ptr1->rotated_asvlist);
   poly_ptr1->smooth = smooth;
   poly_ptr1->vlist = poly_ptr1->intvlist = NULL;
   poly_ptr1->n = poly_ptr1->intn = 0;
   poly_ptr1->svlist = poly_ptr1->asvlist = poly_ptr1->rotated_vlist =
         poly_ptr1->rotated_asvlist = NULL;
   poly_ptr1->sn = poly_ptr1->asn = poly_ptr1->rotated_n =
         poly_ptr1->rotated_asn = 0;
   if (obj_ptr1->ctm != NULL) free(obj_ptr1->ctm);
   obj_ptr1->ctm = NULL;

   poly_ptr1->vlist = vs;
   poly_ptr1->n = num_pts;

   AdjObjSplineVs(obj_ptr1);
   if (poly_ptr1->curved != LT_INTSPLINE) {
      UpdPolyBBox(obj_ptr1, poly_ptr1->n, poly_ptr1->vlist);
   } else {
      UpdPolyBBox(obj_ptr1, poly_ptr1->intn, poly_ptr1->intvlist);
   }
   AdjObjBBox(obj_ptr1);
   RecordReplaceAnObj(obj_ptr1);

   topSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (topSel == NULL) FailAllocMessage();
   topSel->obj = obj_ptr1;
   topSel->prev = NULL;
   topSel->next = top_sel;
   if (top_sel != NULL) {
      top_sel->prev = topSel;
      botSel = bot_sel;
   } else {
      botSel = topSel;
   }
   if (curChoice == VERTEXMODE) {
      topVSel = botVSel = (struct VSelRec *)malloc(sizeof(struct VSelRec));
      if (topVSel == NULL) FailAllocMessage();
      topVSel->obj = obj_ptr1;
      topVSel->max_v = 10;
      topVSel->v_index = (int*)malloc(10*sizeof(int));
      topVSel->x = (int*)malloc(10*sizeof(int));
      topVSel->y = (int*)malloc(10*sizeof(int));
      if (topVSel->v_index==NULL || topVSel->x==NULL || topVSel->y==NULL) {
         FailAllocMessage();
      }
      topVSel->v_index[0] = index0;
      topVSel->x[0] = x0;
      topVSel->y[0] = y0;
      topVSel->n = 1;
      topVSel->next = topVSel->prev = NULL;
   }
}

static
void DoJoinPoly(obj_ptr1, obj_ptr2, poly_ptr1, poly_ptr2, min_index)
   struct ObjRec *obj_ptr1, *obj_ptr2;
   struct PolyRec *poly_ptr1, *poly_ptr2;
   int min_index;
{
   int i, j, n, n1=poly_ptr1->n, n2=poly_ptr2->n, num_pts, ltx, lty, rbx, rby;
   int min_val, max_val, inc;
   IntPoint *vs, tmp_p;
   char *smooth=NULL;

   num_pts = n1+n2-1;
   vs = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
   if (vs == NULL) FailAllocMessage();
   if (poly_ptr1->curved != LT_INTSPLINE) {
      smooth = (char*)malloc((num_pts+1)*sizeof(char));
      if (smooth == NULL) FailAllocMessage();
   }

   switch (min_index) {
   case 0: min_val=n1-1; max_val=0; inc=(-1); break;
   case 1: min_val=n1-1; max_val=0; inc=(-1); break;
   case 2: min_val=0; max_val=n1; inc=1; break;
   case 3: min_val=0; max_val=n1; inc=1; break;
   }
   n = 0;
   i = min_val;
   while (inc > 0 ? i < max_val : i >= max_val) {
      if (obj_ptr1->ctm == NULL) {
         memcpy(&vs[n], &poly_ptr1->vlist[i], sizeof(IntPoint));
      } else {
         TransformObjectV(obj_ptr1, &poly_ptr1->vlist[i], &tmp_p);
         memcpy(&vs[n], &tmp_p, sizeof(IntPoint));
      }
      if (smooth != NULL) {
         smooth[n] = poly_ptr1->smooth[i];
      }
      n++;
      i += inc;
   }
   switch (min_index) {
   case 0: min_val=1; max_val=n2; inc=1; break;
   case 1: min_val=n2-2; max_val=0; inc=(-1); break;
   case 2: min_val=1; max_val=n2; inc=1; break;
   case 3: min_val=n2-2; max_val=0; inc=(-1); break;
   }
   i = min_val;
   while (inc > 0 ? i < max_val : i >= max_val) {
      if (obj_ptr2->ctm == NULL) {
         memcpy(&vs[n], &poly_ptr2->vlist[i], sizeof(IntPoint));
      } else {
         TransformObjectV(obj_ptr2, &poly_ptr2->vlist[i], &tmp_p);
         memcpy(&vs[n], &tmp_p, sizeof(IntPoint));
      }
      if (smooth != NULL) {
         smooth[n] = poly_ptr2->smooth[i];
      }
      n++;
      i += inc;
   }
   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
   HighLightReverse();
   StartCompositeCmd();
   FinishJoinPoly(obj_ptr1, obj_ptr2, poly_ptr1, vs, smooth, num_pts);
   EndCompositeCmd();
   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);
   justDupped = FALSE;
   HighLightForward();
}

static
void FinishCloseOnePoly(obj_ptr, poly_ptr, vs, smooth, num_pts)
   struct ObjRec *obj_ptr;
   struct PolyRec *poly_ptr;
   IntPoint *vs;
   char *smooth;
   int num_pts;
{
   struct PolygonRec *polygon_ptr;

   polygon_ptr = (struct PolygonRec *)malloc(sizeof(struct PolygonRec));
   if (polygon_ptr == NULL) FailAllocMessage();

   memset(polygon_ptr, 0, sizeof(struct PolygonRec));
   polygon_ptr->n = num_pts;
   polygon_ptr->vlist = vs;
   polygon_ptr->smooth = smooth;
   polygon_ptr->sn = 0;
   polygon_ptr->svlist = NULL;
   polygon_ptr->intn = 0;
   polygon_ptr->intvlist = NULL;
   polygon_ptr->fill = poly_ptr->fill;
   polygon_ptr->width = poly_ptr->width;
   UtilStrCpy(polygon_ptr->width_spec, sizeof(polygon_ptr->width_spec),
         poly_ptr->width_spec);
   polygon_ptr->pen = poly_ptr->pen;
   polygon_ptr->curved = poly_ptr->curved;
   polygon_ptr->dash = poly_ptr->dash;
   polygon_ptr->rotated_n = 0;
   polygon_ptr->rotated_vlist = NULL;

   PrepareToReplaceAnObj(obj_ptr);

   if (poly_ptr->vlist != NULL) free(poly_ptr->vlist);
   if (poly_ptr->svlist != NULL) free(poly_ptr->svlist);
   if (poly_ptr->asvlist != NULL) free(poly_ptr->asvlist);
   if (poly_ptr->smooth != NULL) free(poly_ptr->smooth);
   if (poly_ptr->intvlist != NULL) free(poly_ptr->intvlist);
   if (poly_ptr->rotated_vlist != NULL) free(poly_ptr->rotated_vlist);
   if (poly_ptr->rotated_asvlist != NULL) free(poly_ptr->rotated_asvlist);
   free(poly_ptr);

   obj_ptr->type = OBJ_POLYGON;
   obj_ptr->detail.g = polygon_ptr;

   AdjObjSplineVs(obj_ptr);
   if (polygon_ptr->curved != LT_INTSPLINE) {
      UpdPolyBBox(obj_ptr, polygon_ptr->n, polygon_ptr->vlist);
   } else {
      UpdPolyBBox(obj_ptr, polygon_ptr->intn, polygon_ptr->intvlist);
   }
   AdjObjBBox(obj_ptr);
   RecordReplaceAnObj(obj_ptr);
}

static
void CloseOnePoly(obj_ptr)
   struct ObjRec *obj_ptr;
{
   struct PolyRec *poly_ptr=obj_ptr->detail.p;
   int i, j, n=poly_ptr->n, num_pts, ltx, lty, rbx, rby, coincide=FALSE;
   IntPoint *vs, *pv=poly_ptr->vlist;
   char *smooth=NULL, *ps=poly_ptr->smooth;

   if (n <= 2) {
      MsgBox("Too few vertices to close a polyline/open-spline.",
            TOOL_NAME, INFO_MB);
      return;
   }
   if (pv[0].x == pv[n-1].x && pv[0].y == pv[n-1].y) {
      num_pts = n;
      coincide = TRUE;
   } else {
      num_pts = n+1;
   }
   vs = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
   if (vs == NULL) FailAllocMessage();
   for (i=0; i < n; i++) memcpy(&vs[i], &pv[i], sizeof(IntPoint));
   memcpy(&vs[n], &pv[0], sizeof(IntPoint));
   if (poly_ptr->curved != LT_INTSPLINE) {
      smooth = (char*)malloc((num_pts+1)*sizeof(char));
      if (smooth == NULL) FailAllocMessage();
      for (i=0; i < n; i++) smooth[i] = ps[i];
      smooth[0] = smooth[n] = FALSE;
   }
   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
   HighLightReverse();
   FinishCloseOnePoly(obj_ptr, poly_ptr, vs, smooth, num_pts);
   if (curChoice == VERTEXMODE) {
      if (topVSel == botVSel) {
         int x0=0, y0=0, first_index=(-1), last_index=(-1);

         for (i=0; i < topVSel->n; i++) {
            if (topVSel->v_index[i] == 0) {
               x0 = topVSel->x[i];
               y0 = topVSel->y[i];
               first_index = i;
            } else if (topVSel->v_index[i] ==
                  (coincide ? num_pts-1 : num_pts-2)) {
               if (!coincide) {
                  topVSel->v_index[i]++;
               }
               last_index = i;
            }
         }
         if (last_index != (-1) && first_index != (-1)) {
            topVSel->n = 2;
            topVSel->x[first_index] = topVSel->x[last_index] = x0;
            topVSel->y[first_index] = topVSel->y[last_index] = y0;
         } else {
            fprintf(stderr, "Huh?  Where are the vertices?\n");
         }
      } else {
         fprintf(stderr, "Huh?  How can topVSel != botVSel?\n");
      }
   }
   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);
   justDupped = FALSE;
   HighLightForward();
}

void JoinPoly()
{
   int i;

   if (curChoice != VERTEXMODE && curChoice != NOTHING) {
      MsgBox("This command is only available in vertex or select mode.",
            TOOL_NAME, INFO_MB);
      return;
   }
   if (curChoice == VERTEXMODE) {
      struct ObjRec *obj_ptr1, *obj_ptr2;
      struct PolyRec *poly_ptr1, *poly_ptr2;
      int min_index;

      if (CountSelectedVertices() != 2) {
         MsgBox("Please select 2 end-point vertices to join.",
               TOOL_NAME, INFO_MB);
         return;
      }
      obj_ptr1 = topVSel->obj;
      obj_ptr2 = botVSel->obj;
      if (obj_ptr1->type != OBJ_POLY || obj_ptr2->type != OBJ_POLY) {
         MsgBox("Please select vertices from polylines/open-splines only.",
               TOOL_NAME, INFO_MB);
         return;
      }
      poly_ptr1 = obj_ptr1->detail.p;
      for (i=0; i < topVSel->n; i++) {
         int index=topVSel->v_index[i];

         if (index != 0 && index != poly_ptr1->n-1) {
            MsgBox("Please select 2 end-point vertices to join.",
                  TOOL_NAME, INFO_MB);
            return;
         }
      }
      if (obj_ptr1 == obj_ptr2) {
         CloseOnePoly(obj_ptr1);
         return;
      }
      poly_ptr2 = obj_ptr2->detail.p;
      for (i=0; i < botVSel->n; i++) {
         int index=botVSel->v_index[i];

         if (index != 0 && index != poly_ptr2->n-1) {
            MsgBox("Please select 2 end-point vertices to join.",
                  TOOL_NAME, INFO_MB);
            return;
         }
      }
      if (topVSel->v_index == 0) {
         if (botVSel->v_index == 0) {
            min_index = 0;
         } else {
            min_index = 1;
         }
      } else {
         if (botVSel->v_index == 0) {
            min_index = 2;
         } else {
            min_index = 3;
         }
      }
      DoJoinPoly(obj_ptr1, obj_ptr2, poly_ptr1, poly_ptr2, min_index);
   } else {
      struct ObjRec *obj_ptr1, *obj_ptr2;
      struct PolyRec *poly_ptr1, *poly_ptr2;
      IntPoint p[4], tmp_p;
      long dx, dy, d[4], min_lval;
      int min_index;

      if (numObjSelected == 1 && topSel->obj->type == OBJ_POLY) {
         CloseOnePoly(topSel->obj);
         return;
      } else if (numObjSelected != 2 || topSel == NULL ||
            topSel->obj->type != OBJ_POLY || botSel->obj->type != OBJ_POLY) {
         MsgBox("Please select 2 polylines/open-splines.", TOOL_NAME, INFO_MB);
         return;
      }
      obj_ptr1 = topSel->obj;
      obj_ptr2 = botSel->obj;
      poly_ptr1 = obj_ptr1->detail.p;
      poly_ptr2 = obj_ptr2->detail.p;
      if ((poly_ptr1->curved == LT_INTSPLINE &&
            poly_ptr2->curved != LT_INTSPLINE) ||
            (poly_ptr1->curved != LT_INTSPLINE &&
            poly_ptr2->curved == LT_INTSPLINE)) {
         sprintf(gszMsgBox, "%s %s.",
               "Cannot join a interpolated spline with a",
               "non-interpolated polyline/open-spline");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return;
      }
      memcpy(&p[0], &poly_ptr1->vlist[0], sizeof(IntPoint));
      memcpy(&p[1], &poly_ptr1->vlist[poly_ptr1->n-1], sizeof(IntPoint));
      memcpy(&p[2], &poly_ptr2->vlist[0], sizeof(IntPoint));
      memcpy(&p[3], &poly_ptr2->vlist[poly_ptr2->n-1], sizeof(IntPoint));
      if (obj_ptr1->ctm != NULL) {
         TransformObjectV(obj_ptr1, &p[0], &tmp_p);
         memcpy(&p[0], &tmp_p, sizeof(IntPoint));
         TransformObjectV(obj_ptr1, &p[1], &tmp_p);
         memcpy(&p[1], &tmp_p, sizeof(IntPoint));
      }
      if (obj_ptr2->ctm != NULL) {
         TransformObjectV(obj_ptr2, &p[2], &tmp_p);
         memcpy(&p[2], &tmp_p, sizeof(IntPoint));
         TransformObjectV(obj_ptr2, &p[3], &tmp_p);
         memcpy(&p[3], &tmp_p, sizeof(IntPoint));
      }
      dx=(long)(p[0].x-p[2].x); dy=(long)(p[0].y-p[2].y); d[0]=dx*dx+dy*dy;
      dx=(long)(p[0].x-p[3].x); dy=(long)(p[0].y-p[3].y); d[1]=dx*dx+dy*dy;
      dx=(long)(p[1].x-p[2].x); dy=(long)(p[1].y-p[2].y); d[2]=dx*dx+dy*dy;
      dx=(long)(p[1].x-p[3].x); dy=(long)(p[1].y-p[3].y); d[3]=dx*dx+dy*dy;
      min_index = 0;
      min_lval = d[0];
      for (i=1; i < 4; i++) {
         if (d[i] < min_lval) {
            min_index = i;
            min_lval = d[i];
         }
      }
      DoJoinPoly(obj_ptr1, obj_ptr2, poly_ptr1, poly_ptr2, min_index);
   }
}

static
void FinishCutPoly(obj_ptr1, poly_ptr1, vs1, smooth1, vs2, smooth2,
      num_pts1, num_pts2)
   struct ObjRec *obj_ptr1;
   struct PolyRec *poly_ptr1;
   IntPoint *vs1, *vs2;
   char *smooth1, *smooth2;
   int num_pts1, num_pts2;
{
   struct ObjRec *obj_ptr2;
   struct PolyRec *poly_ptr2;
   struct AttrRec *attr_ptr, *next_attr;
   int x0, y0;

   x0 = topVSel->x[0];
   y0 = topVSel->y[0];

   RemoveAllSel();
   PrepareToReplaceAnObj(obj_ptr1);

   obj_ptr2 = DupObj(obj_ptr1);
   poly_ptr2 = obj_ptr2->detail.p;
   DelAllAttrs(obj_ptr2->fattr);
   obj_ptr2->fattr = obj_ptr2->lattr = NULL;

   if (poly_ptr1->vlist != NULL) free(poly_ptr1->vlist);
   if (poly_ptr1->svlist != NULL) free(poly_ptr1->svlist);
   if (poly_ptr1->asvlist != NULL) free(poly_ptr1->asvlist);
   if (poly_ptr1->smooth != NULL) free(poly_ptr1->smooth);
   if (poly_ptr1->intvlist != NULL) free(poly_ptr1->intvlist);
   if (poly_ptr1->rotated_vlist != NULL) free(poly_ptr1->rotated_vlist);
   if (poly_ptr1->rotated_asvlist != NULL) free(poly_ptr1->rotated_asvlist);
   poly_ptr1->smooth = smooth1;
   poly_ptr1->vlist = poly_ptr1->intvlist = NULL;
   poly_ptr1->n = poly_ptr1->intn = 0;
   poly_ptr1->svlist = poly_ptr1->asvlist = poly_ptr1->rotated_vlist =
         poly_ptr1->rotated_asvlist = NULL;
   poly_ptr1->sn = poly_ptr1->asn = poly_ptr1->rotated_n =
         poly_ptr1->rotated_asn = 0;
   if (obj_ptr1->ctm != NULL) free(obj_ptr1->ctm);
   obj_ptr1->ctm = NULL;

   poly_ptr1->vlist = vs1;
   poly_ptr1->n = num_pts1;

   if (poly_ptr2->vlist != NULL) free(poly_ptr2->vlist);
   if (poly_ptr2->svlist != NULL) free(poly_ptr2->svlist);
   if (poly_ptr2->asvlist != NULL) free(poly_ptr2->asvlist);
   if (poly_ptr2->smooth != NULL) free(poly_ptr2->smooth);
   if (poly_ptr2->intvlist != NULL) free(poly_ptr2->intvlist);
   if (poly_ptr2->rotated_vlist != NULL) free(poly_ptr2->rotated_vlist);
   if (poly_ptr2->rotated_asvlist != NULL) free(poly_ptr2->rotated_asvlist);
   poly_ptr2->smooth = smooth2;
   poly_ptr2->vlist = poly_ptr2->intvlist = NULL;
   poly_ptr2->n = poly_ptr2->intn = 0;
   poly_ptr2->svlist = poly_ptr2->asvlist = poly_ptr2->rotated_vlist =
         poly_ptr2->rotated_asvlist = NULL;
   poly_ptr2->sn = poly_ptr2->asn = poly_ptr2->rotated_n =
         poly_ptr2->rotated_asn = 0;
   if (obj_ptr2->ctm != NULL) free(obj_ptr2->ctm);
   obj_ptr2->ctm = NULL;

   poly_ptr2->vlist = vs2;
   poly_ptr2->n = num_pts2;

   AdjObjSplineVs(obj_ptr1);
   if (poly_ptr1->curved != LT_INTSPLINE) {
      UpdPolyBBox(obj_ptr1, poly_ptr1->n, poly_ptr1->vlist);
   } else {
      UpdPolyBBox(obj_ptr1, poly_ptr1->intn, poly_ptr1->intvlist);
   }
   AdjObjBBox(obj_ptr1);

   AdjObjSplineVs(obj_ptr2);
   if (poly_ptr2->curved != LT_INTSPLINE) {
      UpdPolyBBox(obj_ptr2, poly_ptr2->n, poly_ptr2->vlist);
   } else {
      UpdPolyBBox(obj_ptr2, poly_ptr2->intn, poly_ptr2->intvlist);
   }
   AdjObjBBox(obj_ptr2);
   AddObj(obj_ptr1->prev, obj_ptr1, obj_ptr2);

   topSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   botSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (topSel==NULL || botSel==NULL) FailAllocMessage();
   topSel->obj = obj_ptr2;
   botSel->obj = obj_ptr1;
   topSel->prev = botSel->next = NULL;
   topSel->next = botSel;
   botSel->prev = topSel;

   RecordCmd(CMD_ONE_TO_MANY, NULL, topSel, botSel, 2);

   topVSel = botVSel = (struct VSelRec *)malloc(sizeof(struct VSelRec));
   if (topVSel==NULL) FailAllocMessage();
   topVSel->obj = obj_ptr2;
   topVSel->max_v = 10;
   topVSel->v_index = (int*)malloc(10*sizeof(int));
   topVSel->x = (int*)malloc(10*sizeof(int));
   topVSel->y = (int*)malloc(10*sizeof(int));
   if (topVSel->v_index==NULL || topVSel->x==NULL || topVSel->y==NULL) {
      FailAllocMessage();
   }
   topVSel->v_index[0] = 0;
   topVSel->x[0] = x0;
   topVSel->y[0] = y0;
   topVSel->n = 1;
   topVSel->next = topVSel->prev = NULL;
}

static
void DoCutPoly(obj_ptr, index, poly_ptr)
   struct ObjRec *obj_ptr;
   int index;
   struct PolyRec *poly_ptr;
{
   int i, n=poly_ptr->n, num_pts1, num_pts2, ltx, lty, rbx, rby;
   IntPoint *vs1=NULL, *vs2=NULL;
   char *smooth1=NULL, *smooth2=NULL;

   num_pts1 = index+1;
   num_pts2 = n-index;
   vs1 = (IntPoint*)malloc((num_pts1+1)*sizeof(IntPoint));
   vs2 = (IntPoint*)malloc((num_pts2+1)*sizeof(IntPoint));
   if (vs1==NULL || vs2==NULL) FailAllocMessage();
   if (poly_ptr->curved != LT_INTSPLINE) {
      smooth1 = (char*)malloc((num_pts1+1)*sizeof(char));
      smooth2 = (char*)malloc((num_pts2+1)*sizeof(char));
      if (smooth1==NULL || smooth2==NULL) FailAllocMessage();
   }
   for (i=0; i <= index; i++) {
      if (obj_ptr->ctm == NULL) {
         memcpy(&vs1[i], &poly_ptr->vlist[i], sizeof(IntPoint));
      } else {
         IntPoint tmp_p;

         TransformObjectV(obj_ptr, &poly_ptr->vlist[i], &tmp_p);
         memcpy(&vs1[i], &tmp_p, sizeof(IntPoint));
      }
      if (smooth1 != NULL) {
         smooth1[i] = poly_ptr->smooth[i];
      }
   }
   if (smooth1 != NULL) smooth1[0] = smooth1[index] = FALSE;

   for (i=index; i < n; i++) {
      if (obj_ptr->ctm == NULL) {
         memcpy(&vs2[i-index], &poly_ptr->vlist[i], sizeof(IntPoint));
      } else {
         IntPoint tmp_p;

         TransformObjectV(obj_ptr, &poly_ptr->vlist[i], &tmp_p);
         memcpy(&vs2[i-index], &tmp_p, sizeof(IntPoint));
      }
      if (smooth2 != NULL) {
         smooth2[i-index] = poly_ptr->smooth[i];
      }
   }
   if (smooth2 != NULL) smooth2[0] = smooth2[n-1-index] = FALSE;

   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
   HighLightReverse();
   FinishCutPoly(obj_ptr, poly_ptr, vs1, smooth1, vs2, smooth2,
         num_pts1, num_pts2);
   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);
   justDupped = FALSE;
   HighLightForward();
}

static
void FinishCutPolygon(obj_ptr1, polygon_ptr1, vs, smooth, num_pts)
   struct ObjRec *obj_ptr1;
   struct PolygonRec *polygon_ptr1;
   IntPoint *vs;
   char *smooth;
   int num_pts;
{
   struct ObjRec *obj_ptr2;
   struct PolyRec *poly_ptr;
   struct PolygonRec *polygon_ptr2;
   int x0, y0;

   x0 = topVSel->x[0];
   y0 = topVSel->y[0];

   poly_ptr = (struct PolyRec *)malloc(sizeof(struct PolyRec));
   if (poly_ptr == NULL) FailAllocMessage();

   memset(poly_ptr, 0, sizeof(struct PolyRec));
   poly_ptr->n = num_pts;
   poly_ptr->vlist = vs;
   poly_ptr->smooth = smooth;
   poly_ptr->asn = 0;
   poly_ptr->asvlist = NULL;
   poly_ptr->sn = 0;
   poly_ptr->svlist = NULL;
   poly_ptr->intn = 0;
   poly_ptr->intvlist = NULL;
   poly_ptr->style = LS_PLAIN;
   poly_ptr->width = polygon_ptr1->width;
   UtilStrCpy(poly_ptr->width_spec, sizeof(poly_ptr->width_spec),
         polygon_ptr1->width_spec);
   poly_ptr->pen = polygon_ptr1->pen;
   poly_ptr->curved = polygon_ptr1->curved;
   poly_ptr->fill = polygon_ptr1->fill;
   poly_ptr->dash = polygon_ptr1->dash;
   if (poly_ptr->width == curWidthOfLine[lineWidth]) {
      poly_ptr->aw = curArrowHeadW[lineWidth];
      poly_ptr->ah = curArrowHeadH[lineWidth];
      UtilStrCpy(poly_ptr->aw_spec, sizeof(poly_ptr->aw_spec),
            curArrowHeadWSpec[lineWidth]);
      UtilStrCpy(poly_ptr->ah_spec, sizeof(poly_ptr->ah_spec),
            curArrowHeadHSpec[lineWidth]);
   } else if (maxLineWidths > 0) {
      int i, width=poly_ptr->width;
      int min_diff=abs(curWidthOfLine[0]-width), min_index=0;

      for (i=1; min_diff > 0 && i < maxLineWidths; i++) {
         int diff=abs(curWidthOfLine[i]-width);

         if (diff < min_diff) {
            min_diff = diff;
            min_index = i;
         }
      }
      poly_ptr->aw = curArrowHeadW[min_index];
      poly_ptr->ah = curArrowHeadH[min_index];
      UtilStrCpy(poly_ptr->aw_spec, sizeof(poly_ptr->aw_spec),
            curArrowHeadWSpec[min_index]);
      UtilStrCpy(poly_ptr->ah_spec, sizeof(poly_ptr->ah_spec),
            curArrowHeadHSpec[min_index]);
   }
   poly_ptr->rotated_n = poly_ptr->rotated_asn = 0;
   poly_ptr->rotated_vlist = poly_ptr->rotated_asvlist = NULL;

   JustRemoveAllVSel();
   PrepareToReplaceAnObj(obj_ptr1);

   obj_ptr2 = DupObj(obj_ptr1);
   obj_ptr2->fattr = obj_ptr1->fattr;
   obj_ptr2->lattr = obj_ptr1->lattr;
   obj_ptr1->fattr = obj_ptr1->lattr = NULL;
   polygon_ptr2 = obj_ptr2->detail.g;

   if (polygon_ptr2->vlist != NULL) free(polygon_ptr2->vlist);
   if (polygon_ptr2->svlist != NULL) free(polygon_ptr2->svlist);
   if (polygon_ptr2->smooth != NULL) free(polygon_ptr2->smooth);
   if (polygon_ptr2->intvlist != NULL) free(polygon_ptr2->intvlist);
   if (polygon_ptr2->rotated_vlist != NULL) free(polygon_ptr2->rotated_vlist);
   free(polygon_ptr2);
   if (obj_ptr2->ctm != NULL) free(obj_ptr2->ctm);
   obj_ptr2->ctm = NULL;

   obj_ptr2->type = OBJ_POLY;
   obj_ptr2->detail.p = poly_ptr;

   AdjObjSplineVs(obj_ptr2);
   if (poly_ptr->curved != LT_INTSPLINE) {
      UpdPolyBBox(obj_ptr2, poly_ptr->n, poly_ptr->vlist);
   } else {
      UpdPolyBBox(obj_ptr2, poly_ptr->intn, poly_ptr->intvlist);
   }
   AdjObjBBox(obj_ptr2);

   AddObj(obj_ptr1->prev, obj_ptr1, obj_ptr2);
   UnlinkObj(obj_ptr1);
   FreeObj(obj_ptr1);

   topSel->obj = obj_ptr2;
   topSel->prev = topSel->next = NULL;
   botSel = topSel;

   RecordReplaceAnObj(obj_ptr2);

   topVSel = botVSel = (struct VSelRec *)malloc(sizeof(struct VSelRec));
   if (topVSel == NULL) FailAllocMessage();
   topVSel->obj = obj_ptr2;
   topVSel->max_v = 10;
   topVSel->v_index = (int*)malloc(10*sizeof(int));
   topVSel->x = (int*)malloc(10*sizeof(int));
   topVSel->y = (int*)malloc(10*sizeof(int));
   if (topVSel->v_index==NULL || topVSel->x==NULL || topVSel->y==NULL) {
      FailAllocMessage();
   }
   topVSel->v_index[0] = 0;
   topVSel->x[0] = x0;
   topVSel->y[0] = y0;
   topVSel->n = 1;
   topVSel->next = topVSel->prev = NULL;
}

static
void DoCutPolygon(obj_ptr, index, polygon_ptr)
   struct ObjRec *obj_ptr;
   int index;
   struct PolygonRec *polygon_ptr;
{
   int i, n=polygon_ptr->n, num_pts, ltx, lty, rbx, rby;
   IntPoint *vs=NULL;
   char *smooth=NULL;

   num_pts = n;
   vs = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
   if (vs == NULL) FailAllocMessage();
   if (polygon_ptr->curved != LT_INTSPLINE) {
      smooth = (char*)malloc((num_pts+1)*sizeof(char));
      if (smooth == NULL) FailAllocMessage();
   }
   for (i=index; i < num_pts; i++) {
      if (obj_ptr->ctm == NULL) {
         memcpy(&vs[i-index], &polygon_ptr->vlist[i], sizeof(IntPoint));
      } else {
         IntPoint tmp_p;

         TransformObjectV(obj_ptr, &polygon_ptr->vlist[i], &tmp_p);
         memcpy(&vs[i-index], &tmp_p, sizeof(IntPoint));
      }
      if (smooth != NULL) {
         smooth[i-index] = polygon_ptr->smooth[i];
      }
   }
   for (i=1; i <= index; i++) {
      if (obj_ptr->ctm == NULL) {
         memcpy(&vs[i+num_pts-index-1], &polygon_ptr->vlist[i],
               sizeof(IntPoint));
      } else {
         IntPoint tmp_p;

         TransformObjectV(obj_ptr, &polygon_ptr->vlist[i], &tmp_p);
         memcpy(&vs[i+num_pts-index-1], &tmp_p, sizeof(IntPoint));
      }
      if (smooth != NULL) {
         smooth[i+num_pts-index-1] = polygon_ptr->smooth[i];
      }
   }
   if (smooth != NULL) smooth[0] = smooth[num_pts-1] = FALSE;

   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
   HighLightReverse();
   FinishCutPolygon(obj_ptr, polygon_ptr, vs, smooth, num_pts);
   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);
   justDupped = FALSE;
   HighLightForward();
}

void CutPoly()
{
   struct ObjRec *obj_ptr;
   struct PolyRec *poly_ptr=NULL;
   struct PolygonRec *polygon_ptr=NULL;
   int index;

   if (curChoice != VERTEXMODE) {
      MsgBox("This command is only available in vertex mode.",
            TOOL_NAME, INFO_MB);
      return;
   } else if (CountSelectedVertices() != 1) {
      MsgBox("Please select 1 vertex to cut.", TOOL_NAME, INFO_MB);
      return;
   }
   obj_ptr = topVSel->obj;
   index = topVSel->v_index[0];
   switch (obj_ptr->type) {
   case OBJ_POLY:
      poly_ptr = obj_ptr->detail.p;
      if (index==0 || index==poly_ptr->n-1) return;
      DoCutPoly(obj_ptr, index, poly_ptr);
      break;
   case OBJ_POLYGON:
      polygon_ptr = obj_ptr->detail.g;
      DoCutPolygon(obj_ptr, index, polygon_ptr);
      break;
   default:
      MsgBox("Please select a vertex from a polyline or polygon object.",
            TOOL_NAME, INFO_MB);
      return;
   }
}

#define WIDTH_VAL_TOL (1.0e-4)

static
double GetWidthInDouble(nVal, pszSpec, pnIsInt)
   int nVal, *pnIsInt;
   char *pszSpec;
{
   float fval;

   if (pnIsInt != NULL) *pnIsInt = TRUE;
   if (pszSpec == NULL || *pszSpec == '\0') return (double)nVal;
   if (sscanf(pszSpec, "%f", &fval) != 1) return (double)nVal;
   if (pnIsInt != NULL) {
      if (fabs(((float)nVal)-fval) > WIDTH_VAL_TOL) {
         *pnIsInt = FALSE;
      }
   }
   return ((double)fval);
}

void DumpArrow(FP, TailV, HeadV, ArrowW, ArrowH, aw_spec, ah_spec, Pen,
      ColorIndex)
   FILE *FP;
   IntPoint *TailV, *HeadV;
   int ArrowW, ArrowH, Pen, ColorIndex;
   char *aw_spec, *ah_spec;
   /* HeadV is where the arrow tip is */
{
   int i, dx, dy;
   struct BBRec bbox;
   IntPoint v[2];
   double len, sin, cos, daw, dah;

   dx = HeadV->x - TailV->x;
   dy = HeadV->y - TailV->y;

   if (dx == 0 && dy == 0) return;

   fprintf(FP, "gsave\n");
   fprintf(FP, "   tgiforigctm setmatrix\n");

   daw = GetWidthInDouble(ArrowW, aw_spec, NULL);
   dah = GetWidthInDouble(ArrowH, ah_spec, NULL);
   if (colorDump || !useGray) {
      len = (double)sqrt((double)(((double)dx)*((double)dx) +
            ((double)dy)*((double)dy)));
      sin = ((double)dy) / len;
      cos = ((double)dx) / len;

      v[0].x = round(HeadV->x - daw*cos + dah*sin);
      v[0].y = round(HeadV->y - daw*sin - dah*cos);
      v[1].x = round(HeadV->x - daw*cos - dah*sin);
      v[1].y = round(HeadV->y - daw*sin + dah*cos);

      bbox.ltx = bbox.rbx = HeadV->x;
      bbox.lty = bbox.rby = HeadV->y;

      for (i = 0; i < 2; i++) {
         if (v[i].x < bbox.ltx) bbox.ltx = v[i].x;
         if (v[i].y < bbox.lty) bbox.lty = v[i].y;
         if (v[i].x > bbox.rbx) bbox.rbx = v[i].x;
         if (v[i].y > bbox.rby) bbox.rby = v[i].y;
      }
      if (preDumpSetup) PSUseArrow();
      fprintf(FP, "   newpath\n");
      fprintf(FP, "      %1d %1d %.3f %.3f %1d %1d tgifarrowtip\n",
            HeadV->x, HeadV->y, daw, dah, dx, dy);
      fprintf(FP, "   1 setgray closepath fill\n");
      DumpRGBColorLine(FP, ColorIndex, 3, TRUE);
   } else {
      switch (Pen) {
      case SOLIDPAT: break;
      case BACKPAT: break;
      default:
         GrayCheck (Pen);
         fprintf (FP, "   %s setgray\n", GrayStr(Pen));
         break;
      }
   }

   if (!((colorDump || !useGray) && Pen==BACKPAT)) {
      if (preDumpSetup) PSUseArrow();
      fprintf(FP, "   newpath\n");
      fprintf(FP, "      %1d %1d %.3f %.3f %1d %1d tgifarrowtip\n",
            HeadV->x, HeadV->y, daw, dah, dx, dy);
   }
   if (colorDump || !useGray) {
      switch (Pen) {
      case SOLIDPAT: fprintf(FP, "   closepath fill\n"); break;
      case BACKPAT: break;
      default:
         if (preDumpSetup) PSUseColorPattern();
         fprintf(FP, "   closepath eoclip newpath\n");
         DumpPatFill(FP, Pen, 8, bbox, "   ");
         break;
      }
   } else {
      switch (Pen) {
      case SOLIDPAT: fprintf(FP, "   closepath fill\n"); break;
      case BACKPAT: fprintf(FP, "   closepath 1 setgray fill\n"); break;
      default: fprintf(FP, "   closepath fill\n"); break;
      }
   }
   fprintf(FP, "grestore\n");
}

static
int CalcArrowRetreatPoint(TailV, HeadV, ArrowW, aw_spec, ReturnV)
   IntPoint *TailV, *HeadV, *ReturnV;
   int ArrowW;
   char *aw_spec;
   /* HeadV is where the arrow tip is */
{
   int i, dx, dy;
   struct BBRec bbox;
   IntPoint v[2];
   double len, sin, cos, daw;

   dx = HeadV->x - TailV->x;
   dy = HeadV->y - TailV->y;

   if (dx == 0 && dy == 0) return FALSE;

   len = (double)sqrt((double)(((double)dx)*((double)dx) +
         ((double)dy)*((double)dy)));
   sin = ((double)dy) / len;
   cos = ((double)dx) / len;

   daw = GetWidthInDouble(ArrowW, aw_spec, NULL);
   ReturnV->x = round(HeadV->x-RETREAT*daw*cos);
   ReturnV->y = round(HeadV->y-RETREAT*daw*sin);
   return TRUE;
}

static
void DumpPolyPath(FP, ObjPtr, Vs, NumPts, Smooth, Style, Width, ArrowW, ArrowH,
      width_spec, aw_spec, ah_spec, Pen, Fill, Curved, Dash, Indent)
   FILE				*FP;
   register struct ObjRec	*ObjPtr;
   IntPoint			*Vs;
   int				NumPts, Style, Width, ArrowW, ArrowH;
   char				*width_spec, *aw_spec, *ah_spec;
   int				Pen, Fill, Curved, Dash, Indent;
   char				*Smooth;
{
   register int	i, dx, dy;
   int		retracted_arrow, color_index=ObjPtr->color, w_is_int;
   double	daw, dw;

   retracted_arrow = (RetractedArrowAttr(ObjPtr) ||
         AutoRetractedArrowAttr(ObjPtr, TRUE));

   daw = GetWidthInDouble(ArrowW, aw_spec, NULL);
   dw = GetWidthInDouble(Width, width_spec, &w_is_int);
   if (Fill == (-1) && Pen != (-1))
   {  /* dumping the pen */
      if (Dash != 0)
      {
         for (i=0; i < Indent+3; i++) fprintf (FP, " ");
         fprintf (FP, "[");
         for (i = 0; i < dashListLength[Dash]-1; i++)
            fprintf (FP, "%1d ", (int)(dashList[Dash][i]));
         fprintf (FP, "%1d] 0 setdash\n",
               (int)(dashList[Dash][dashListLength[Dash]-1]));
      }
   }
   else if (Fill != (-1) && Pen == (-1))
   {  /* dumping the fill */
      if (Fill > BACKPAT)
      {
         for (i=0; i < Indent; i++) fprintf (FP, " ");
         fprintf (FP, "gsave\n");
         if (colorDump || !useGray)
         {
            if (preDumpSetup) PSUseColorPattern();
            DumpPolyPath (FP, ObjPtr, Vs, NumPts, Smooth, LS_PLAIN, Width,
                  ArrowW, ArrowH, width_spec, aw_spec, ah_spec,
                  (-1), BACKPAT, Curved, Dash, Indent);
         }
         else
         {
            GrayCheck (Fill);
            for (i=0; i < Indent+3; i++) fprintf (FP, " ");
            fprintf(FP, "%s setgray\n", GrayStr(Fill));
         }
      }
   }
   for (i=0; i < Indent+3; i++) fprintf (FP, " ");
   fprintf (FP, "newpath\n");
   for (i=0; i < Indent+3; i++) fprintf (FP, " ");
   fprintf (FP, "   %1d %1d moveto\n", Vs[0].x, Vs[0].y);
   if (Style & LS_LEFT)
   {
      dx = Vs[1].x - Vs[0].x;
      dy = Vs[1].y - Vs[0].y;
      if (dx != 0 || dy != 0)
      {
         if (!retracted_arrow) {
            if (ObjPtr->ctm == NULL) {
               for (i=0; i < Indent+6; i++) fprintf (FP, " ");
               fprintf (FP, "%1d %1d atan dup cos %.3f mul exch sin %.3f %s\n",
                     dy, dx, daw, daw, "mul rmoveto");
            } else {
               IntPoint ip0, ip1, ip2, ip3;

               TransformObjectV(ObjPtr, &Vs[1], &ip0);
               TransformObjectV(ObjPtr, &Vs[0], &ip1);
               if (CalcArrowRetreatPoint(&ip0, &ip1, ArrowW, aw_spec, &ip2)) {
                  ReversedTransformObjectV(ObjPtr, &ip2, &ip3);
                  for (i=0; i < Indent+6; i++) fprintf (FP, " ");
                  fprintf (FP, "%1d %1d moveto\n", ip3.x, ip3.y);
               }
            }
         }
      }
   }
   if (Style & LS_RIGHT)
   {
      if (Curved == LT_INTSPLINE)
         DumpCurvedPolyPoints (FP, NumPts, Vs, Indent+6);
      else
         DumpMultiCurvedPolyPoints (FP, Smooth, Style, Curved, NumPts, Vs,
               Indent+6);

      dx = Vs[NumPts-1].x - Vs[NumPts-2].x;
      dy = Vs[NumPts-1].y - Vs[NumPts-2].y;
      if (dx != 0 || dy != 0)
      {
         if (retracted_arrow) {
            for (i=0; i < Indent+6; i++) fprintf (FP, " ");
            fprintf (FP, "%1d %1d", Vs[NumPts-1].x, Vs[NumPts-1].y);
         } else {
            if (ObjPtr->ctm == NULL) {
               for (i=0; i < Indent+6; i++) fprintf (FP, " ");
               fprintf (FP, "%1d %1d atan dup cos %.3f mul %1d exch sub\n",
                     dy, dx, daw, Vs[NumPts-1].x);
               for (i=0; i < Indent+6; i++) fprintf (FP, " ");
               fprintf (FP, "exch sin %.3f mul %1d exch sub",
                     daw, Vs[NumPts-1].y);
            } else {
               IntPoint ip0, ip1, ip2, ip3;

               TransformObjectV(ObjPtr, &Vs[NumPts-2], &ip0);
               TransformObjectV(ObjPtr, &Vs[NumPts-1], &ip1);
               if (CalcArrowRetreatPoint(&ip0, &ip1, ArrowW, aw_spec, &ip2)) {
                  ReversedTransformObjectV(ObjPtr, &ip2, &ip3);
                  for (i=0; i < Indent+6; i++) fprintf (FP, " ");
                  fprintf (FP, "%1d %1d", ip3.x, ip3.y);
               }
            }
         }
         switch (Curved)
         {
            case LT_STRAIGHT:
            case LT_SPLINE:
               if (NumPts <= 2 || (Smooth != NULL && !Smooth[NumPts-2]))
                  fprintf (FP, " lineto");
               else
                  fprintf (FP, " curveto");
               break;
            case LT_INTSPLINE:
               if (NumPts <= 2)
                  fprintf (FP, " lineto");
               else
                  fprintf (FP, " curveto");
               break;
         }
      }
      fprintf (FP, "\n");
   }
   else if (Curved == LT_INTSPLINE)
   {
      DumpCurvedPolyPoints (FP, NumPts, Vs, Indent+6);
      if (NumPts <= 2)
      {
         for (i=0; i < Indent+6; i++) fprintf (FP, " ");
         fprintf (FP, "%1d %1d lineto\n",Vs[NumPts-1].x,Vs[NumPts-1].y);
      }
      else
      {
         for (i=0; i < Indent+6; i++) fprintf (FP, " ");
         fprintf (FP, "%1d %1d curveto\n",Vs[NumPts-1].x,Vs[NumPts-1].y);
      }
   }
   else
      DumpMultiCurvedPolyPoints (FP, Smooth, Style, Curved, NumPts, Vs,
            Indent+6);

   if (Fill == (-1) && Pen != (-1))
   {  /* dumping the pen */
      for (i=0; i < Indent+3; i++) fprintf (FP, " ");
      fprintf (FP, "tgiforigctm setmatrix\n");
      for (i=0; i < Indent+3; i++) fprintf (FP, " ");
      if (w_is_int) {
         fprintf (FP, "%1d setlinewidth\n", Width);
      } else {
         fprintf (FP, "%.3f setlinewidth\n", dw);
      }
      for (i=0; i < Indent+3; i++) fprintf (FP, " ");
      switch (Pen)
      {
         case SOLIDPAT: fprintf (FP, "stroke\n"); break;
         case BACKPAT: fprintf (FP, "1 setgray stroke\n"); break;
         default:
            if (colorDump || !useGray)
            {
               if (preDumpSetup) PSUseColorPattern();
               fprintf (FP, "flattenpath strokepath clip newpath\n");
               for (i=0; i < Indent+3; i++) fprintf (FP, " ");
               DumpPatFill (FP, Pen, 8, ObjPtr->bbox, "");
            }
            else
            {
               GrayCheck (Pen);
               fprintf (FP, "%s setgray\n", GrayStr(Pen));
               for (i=0; i < Indent+3; i++) fprintf (FP, " ");
               fprintf (FP, "stroke\n");
            }
            break;
      }
      if (Dash != 0)
      {
         for (i=0; i < Indent+3; i++) fprintf (FP, " ");
         fprintf (FP, "[] 0 setdash\n");
      }
      if (Width != 1)
      {
         for (i=0; i < Indent+3; i++) fprintf (FP, " ");
         fprintf (FP, "1 setlinewidth\n");
      }
   }
   else if (Fill != (-1) && Pen == (-1))
   {  /* dumping the fill */
      for (i=0; i < Indent+3; i++) fprintf (FP, " ");
      switch (Fill)
      {
         case SOLIDPAT:
            fprintf (FP, "closepath eofill\n");
            break;
         case BACKPAT:
            fprintf (FP, "closepath 1 setgray eofill\n");
            for (i=0; i < Indent+3; i++) fprintf (FP, " ");
            DumpRGBColorLine(FP, color_index, 0, TRUE);
            break;
         default:
            if (colorDump || !useGray)
            {
               if (preDumpSetup) PSUseColorPattern();
               fprintf (FP, "closepath eoclip newpath\n");
               for (i=0; i < Indent+3; i++) fprintf (FP, " ");
               DumpPatFill (FP, Fill, 8, ObjPtr->bbox, "");
            }
            else
               fprintf (FP, "closepath eofill\n");
            for (i=0; i < Indent; i++) fprintf (FP, " ");
            fprintf (FP, "grestore\n");
            break;
      }
   }
}

void DumpPolyObj (FP, ObjPtr)
   FILE				* FP;
   register struct ObjRec	* ObjPtr;
{
   IntPoint	*intv, *v;
   int		num_pts, fill, pen, width, curved, dash, color_index, style;
   int		aw, ah, rotation, intn, retracted_arrow;
   char		* smooth, *width_spec, *aw_spec, *ah_spec;

   fill = ObjPtr->detail.p->fill;
   width = ObjPtr->detail.p->width;
   aw = ObjPtr->detail.p->aw;
   ah = ObjPtr->detail.p->ah;
   width_spec = ObjPtr->detail.p->width_spec;
   aw_spec = ObjPtr->detail.p->aw_spec;
   ah_spec = ObjPtr->detail.p->ah_spec;
   pen = ObjPtr->detail.p->pen;
   style = ObjPtr->detail.p->style;
   curved = ObjPtr->detail.p->curved;
   dash = ObjPtr->detail.p->dash;
   rotation = ObjPtr->rotation;
   v = ObjPtr->detail.p->vlist;
   num_pts = ObjPtr->detail.p->n;
   smooth = ObjPtr->detail.p->smooth;
   intv = ObjPtr->detail.p->intvlist;
   intn = ObjPtr->detail.p->intn;

   if (fill == NONEPAT && pen == NONEPAT) return;

   fprintf (FP, "%% POLY/OPEN-SPLINE\n");
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
   if (fill != NONEPAT && num_pts > 2)
   {
      if (curved != LT_INTSPLINE)
         DumpPolyPath (FP, ObjPtr, v, num_pts, smooth, LS_PLAIN, width, aw, ah,
               width_spec, aw_spec, ah_spec, (-1), fill, curved, dash,
               (fill > BACKPAT ? 0 : (-3)));
      else
         DumpPolyPath (FP, ObjPtr, intv, intn, smooth, LS_PLAIN, width, aw, ah,
               width_spec, aw_spec, ah_spec, (-1), fill, curved, dash,
               (fill > BACKPAT ? 0 : (-3)));
   }
   if (pen == NONEPAT) {
      if (ObjPtr->ctm != NULL) fprintf(FP, "grestore\n");
      fprintf (FP, "\n");
      return;
   }

   fprintf (FP, "gsave\n");

   if ((colorDump || !useGray) && pen > BACKPAT)
   {
      fprintf(FP, "   gsave\n");
      if (curved != LT_INTSPLINE)
         DumpPolyPath (FP, ObjPtr, v, num_pts, smooth, style, width, aw, ah,
               width_spec, aw_spec, ah_spec, BACKPAT, (-1), curved, 0, 3);
      else
         DumpPolyPath (FP, ObjPtr, intv, intn, smooth, style, width, aw, ah,
               width_spec, aw_spec, ah_spec, BACKPAT, (-1), curved, 0, 3);
      fprintf(FP, "   grestore\n");
   }
   if (curved != LT_INTSPLINE)
      DumpPolyPath (FP, ObjPtr, v, num_pts, smooth, style, width, aw, ah,
            width_spec, aw_spec, ah_spec, pen, (-1), curved, dash, 0);
   else
      DumpPolyPath (FP, ObjPtr, intv, intn, smooth, style, width, aw, ah,
            width_spec, aw_spec, ah_spec, pen, (-1), curved, dash, 0);

   fprintf (FP, "grestore\n");

   retracted_arrow = (RetractedArrowAttr(ObjPtr) ||
         AutoRetractedArrowAttr(ObjPtr, TRUE));

   if (ObjPtr->ctm == NULL) {
      if (curved != LT_INTSPLINE) {
         switch (style) {
         case LS_PLAIN: break;
         case LS_LEFT:
            if (retracted_arrow) {
               DumpArrow(FP, &v[2], &v[1], aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            } else {
               DumpArrow(FP, &v[1], &v[0], aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            }
            break;
         case LS_RIGHT:
            if (retracted_arrow) {
               DumpArrow(FP, &v[num_pts-3], &v[num_pts-2], aw, ah,
                     aw_spec, ah_spec, pen, color_index);
            } else {
               DumpArrow(FP, &v[num_pts-2], &v[num_pts-1], aw, ah,
                     aw_spec, ah_spec, pen, color_index);
            }
            break;
         case LS_DOUBLE:
            if (retracted_arrow) {
               DumpArrow(FP, &v[2], &v[1], aw, ah, aw_spec, ah_spec,
                     pen, color_index);
               DumpArrow(FP, &v[num_pts-3], &v[num_pts-2], aw, ah,
                     aw_spec, ah_spec, pen, color_index);
            } else {
               DumpArrow(FP, &v[1], &v[0], aw, ah, aw_spec, ah_spec,
                     pen, color_index);
               DumpArrow(FP, &v[num_pts-2], &v[num_pts-1], aw, ah,
                     aw_spec, ah_spec, pen, color_index);
            }
            break;
         }
      } else {
         switch (style) {
         case LS_PLAIN: break;
         case LS_LEFT:
            if (retracted_arrow) {
               DumpArrow(FP, &intv[2], &v[1], aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            } else {
               DumpArrow(FP, &intv[1], &intv[0], aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            }
            break;
         case LS_RIGHT:
            if (retracted_arrow) {
               DumpArrow(FP, &intv[intn-3], &v[num_pts-2], aw, ah,
                     aw_spec, ah_spec, pen, color_index);
            } else {
               DumpArrow(FP, &intv[intn-2], &intv[intn-1], aw, ah,
                     aw_spec, ah_spec, pen, color_index);
            }
            break;
         case LS_DOUBLE:
            if (retracted_arrow) {
               DumpArrow(FP, &intv[2], &v[1], aw, ah, aw_spec, ah_spec,
                     pen, color_index);
               DumpArrow(FP, &intv[intn-3], &v[num_pts-2], aw, ah,
                     aw_spec, ah_spec, pen, color_index);
            } else {
               DumpArrow(FP, &intv[1], &intv[0], aw, ah, aw_spec, ah_spec,
                     pen, color_index);
               DumpArrow(FP, &intv[intn-2], &intv[intn-1], aw, ah,
                     aw_spec, ah_spec, pen, color_index);
            }
            break;
         }
      }
   } else {
      IntPoint ip0, ip1;

      if (curved != LT_INTSPLINE) {
         switch (style) {
         case LS_PLAIN: break;
         case LS_LEFT:
            if (retracted_arrow) {
               TransformObjectV(ObjPtr, &v[2], &ip0);
               TransformObjectV(ObjPtr, &v[1], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            } else {
               TransformObjectV(ObjPtr, &v[1], &ip0);
               TransformObjectV(ObjPtr, &v[0], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            }
            break;
         case LS_RIGHT:
            if (retracted_arrow) {
               TransformObjectV(ObjPtr, &v[num_pts-3], &ip0);
               TransformObjectV(ObjPtr, &v[num_pts-2], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            } else {
               TransformObjectV(ObjPtr, &v[num_pts-2], &ip0);
               TransformObjectV(ObjPtr, &v[num_pts-1], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            }
            break;
         case LS_DOUBLE:
            if (retracted_arrow) {
               TransformObjectV(ObjPtr, &v[2], &ip0);
               TransformObjectV(ObjPtr, &v[1], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
               TransformObjectV(ObjPtr, &v[num_pts-3], &ip0);
               TransformObjectV(ObjPtr, &v[num_pts-2], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            } else {
               TransformObjectV(ObjPtr, &v[1], &ip0);
               TransformObjectV(ObjPtr, &v[0], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
               TransformObjectV(ObjPtr, &v[num_pts-2], &ip0);
               TransformObjectV(ObjPtr, &v[num_pts-1], &ip1);
               DumpArrow(FP, &ip0, &ip1,aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            }
            break;
         }
      } else {
         switch (style) {
         case LS_PLAIN: break;
         case LS_LEFT:
            if (retracted_arrow) {
               TransformObjectV(ObjPtr, &intv[2], &ip0);
               TransformObjectV(ObjPtr, &v[1], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            } else {
               TransformObjectV(ObjPtr, &intv[1], &ip0);
               TransformObjectV(ObjPtr, &intv[0], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            }
            break;
         case LS_RIGHT:
            if (retracted_arrow) {
               TransformObjectV(ObjPtr, &intv[intn-3], &ip0);
               TransformObjectV(ObjPtr, &v[num_pts-2], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            } else {
               TransformObjectV(ObjPtr, &intv[intn-2], &ip0);
               TransformObjectV(ObjPtr, &intv[intn-1], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            }
            break;
         case LS_DOUBLE:
            if (retracted_arrow) {
               TransformObjectV(ObjPtr, &intv[2], &ip0);
               TransformObjectV(ObjPtr, &v[1], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
               TransformObjectV(ObjPtr, &intv[intn-3], &ip0);
               TransformObjectV(ObjPtr, &v[num_pts-2], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            } else {
               TransformObjectV(ObjPtr, &intv[1], &ip0);
               TransformObjectV(ObjPtr, &intv[0], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
               TransformObjectV(ObjPtr, &intv[intn-2], &ip0);
               TransformObjectV(ObjPtr, &intv[intn-1], &ip1);
               DumpArrow(FP, &ip0, &ip1, aw, ah, aw_spec, ah_spec,
                     pen, color_index);
            }
            break;
         }
      }
   }
   if (ObjPtr->ctm != NULL) fprintf(FP, "grestore\n");
   fprintf (FP, "\n");
}

int NeedsToCachePolyObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   return (ObjPtr->ctm != NULL);
}

static
void MakeCachedPoly (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register int		i;
   struct PolyRec	* poly_ptr=ObjPtr->detail.p;
   IntPoint		*pv=poly_ptr->vlist, *intvs, *vs;
   int			n=poly_ptr->n, intn;

   if (ObjPtr->ctm == NULL) return;
   if (poly_ptr->rotated_vlist != NULL) free(poly_ptr->rotated_vlist);
   if (poly_ptr->rotated_asvlist != NULL) free(poly_ptr->rotated_asvlist);
   poly_ptr->rotated_n = poly_ptr->rotated_asn = 0;
   poly_ptr->rotated_vlist = poly_ptr->rotated_asvlist = NULL;

   vs = (IntPoint*)malloc((n+1)*sizeof(IntPoint));
   if (vs == NULL) FailAllocMessage();
   for (i=0; i < n; i++) {
      int x, y;

      TransformPointThroughCTM(pv[i].x-ObjPtr->x, pv[i].y-ObjPtr->y,
            ObjPtr->ctm, &x, &y);
      vs[i].x = x+ObjPtr->x;
      vs[i].y = y+ObjPtr->y;
   }
   vs[n].x = vs[0].x;
   vs[n].y = vs[0].y;

   switch (poly_ptr->curved) {
   case LT_STRAIGHT:
   case LT_SPLINE:
      poly_ptr->rotated_vlist = MakeMultiSplinePolyVertex(
            &(poly_ptr->rotated_n), poly_ptr->smooth,
            drawOrigX, drawOrigY, n, vs);
      break;
   case LT_INTSPLINE:
      poly_ptr->rotated_vlist = MakeIntSplinePolyVertex(
            &(poly_ptr->rotated_n), &(intn), &(intvs),
            drawOrigX, drawOrigY, n, vs);
      free(intvs);
      break;
   }
   poly_ptr->rotated_vlist[poly_ptr->rotated_n].x =
         poly_ptr->rotated_vlist[0].x;
   poly_ptr->rotated_vlist[poly_ptr->rotated_n].y =
         poly_ptr->rotated_vlist[0].y;

   if (poly_ptr->style != LS_PLAIN && !RetractedArrowAttr(ObjPtr) &&
         !AutoRetractedArrowAttr(ObjPtr,True)) {
      int aw=poly_ptr->aw, dx, dy;
      double len, sin, cos;

      if (aw == 0) aw = 1;

      dx = vs[1].x - vs[0].x;
      dy = vs[1].y - vs[0].y;
      if ((poly_ptr->style & LS_LEFT) && (dx != 0 || dy != 0)) {
         len = (double)sqrt((double)(((double)dx)*((double)dx) +
               ((double)dy)*((double)dy)));
         sin = ((double)dy)/len;
         cos = ((double)dx)/len;
         vs[0].x = round(vs[0].x+RETREAT*aw*cos);
         vs[0].y = round(vs[0].y+RETREAT*aw*sin);
      }
      dx = vs[n-1].x - vs[n-2].x;
      dy = vs[n-1].y - vs[n-2].y;
      if ((poly_ptr->style & LS_RIGHT) && (dx != 0 || dy != 0)) {
         len = (double)sqrt((double)(((double)dx)*((double)dx) +
               ((double)dy)*((double)dy)));
         sin = ((double)dy)/len;
         cos = ((double)dx)/len;
         vs[n-1].x = round(vs[n-1].x-RETREAT*aw*cos);
         vs[n-1].y = round(vs[n-1].y-RETREAT*aw*sin);
      }
      switch (poly_ptr->curved) {
      case LT_STRAIGHT:
      case LT_SPLINE:
         poly_ptr->rotated_asvlist = MakeMultiSplinePolyVertex(
               &(poly_ptr->rotated_asn), poly_ptr->smooth,
               drawOrigX, drawOrigY, n, vs);
         break;
      case LT_INTSPLINE:
         poly_ptr->rotated_asvlist = MakeIntSplinePolyVertex(
               &(poly_ptr->rotated_asn), &(intn), &(intvs),
               drawOrigX, drawOrigY, n, vs);
         free(intvs);
         break;
      }
   }
   free(vs);
}

static
void SetArrowVsForDraw(obj_ptr, retracted_arrow, real_x_off, real_y_off,
      v0, v1, vnminus2, vnminus1)
   struct ObjRec *obj_ptr;
   int retracted_arrow, real_x_off, real_y_off;
   XPoint *v0, *v1, *vnminus2, *vnminus1;
{
   struct PolyRec *poly_ptr=obj_ptr->detail.p;
   int n=poly_ptr->n;

   if (obj_ptr->ctm == NULL) {
      if (poly_ptr->curved != LT_INTSPLINE) {
         IntPoint *vs=poly_ptr->vlist;
   
         if (retracted_arrow) {
            v0->x = ZOOMED_SIZE(vs[1].x-real_x_off);
            v0->y = ZOOMED_SIZE(vs[1].y-real_y_off);
            v1->x = ZOOMED_SIZE(vs[2].x-real_x_off);
            v1->y = ZOOMED_SIZE(vs[2].y-real_y_off);
            vnminus2->x = ZOOMED_SIZE(vs[n-3].x-real_x_off);
            vnminus2->y = ZOOMED_SIZE(vs[n-3].y-real_y_off);
            vnminus1->x = ZOOMED_SIZE(vs[n-2].x-real_x_off);
            vnminus1->y = ZOOMED_SIZE(vs[n-2].y-real_y_off);
         } else {
            v0->x = ZOOMED_SIZE(vs[0].x-real_x_off);
            v0->y = ZOOMED_SIZE(vs[0].y-real_y_off);
            v1->x = ZOOMED_SIZE(vs[1].x-real_x_off);
            v1->y = ZOOMED_SIZE(vs[1].y-real_y_off);
            vnminus2->x = ZOOMED_SIZE(vs[n-2].x-real_x_off);
            vnminus2->y = ZOOMED_SIZE(vs[n-2].y-real_y_off);
            vnminus1->x = ZOOMED_SIZE(vs[n-1].x-real_x_off);
            vnminus1->y = ZOOMED_SIZE(vs[n-1].y-real_y_off);
         }
      } else {
         int intn=poly_ptr->intn;
         IntPoint *intvlist=poly_ptr->intvlist;
   
         if (retracted_arrow) {
            int n=poly_ptr->n;
            IntPoint *vs=poly_ptr->vlist;
   
            v0->x = ZOOMED_SIZE(vs[1].x-real_x_off);
            v0->y = ZOOMED_SIZE(vs[1].y-real_y_off);
            v1->x = ZOOMED_SIZE(intvlist[2].x-real_x_off);
            v1->y = ZOOMED_SIZE(intvlist[2].y-real_y_off);
            vnminus2->x = ZOOMED_SIZE(intvlist[intn-3].x-real_x_off);
            vnminus2->y = ZOOMED_SIZE(intvlist[intn-3].y-real_y_off);
            vnminus1->x = ZOOMED_SIZE(vs[n-2].x-real_x_off);
            vnminus1->y = ZOOMED_SIZE(vs[n-2].y-real_y_off);
         } else {
            v0->x = ZOOMED_SIZE(intvlist[0].x-real_x_off);
            v0->y = ZOOMED_SIZE(intvlist[0].y-real_y_off);
            v1->x = ZOOMED_SIZE(intvlist[1].x-real_x_off);
            v1->y = ZOOMED_SIZE(intvlist[1].y-real_y_off);
            vnminus2->x = ZOOMED_SIZE(intvlist[intn-2].x-real_x_off);
            vnminus2->y = ZOOMED_SIZE(intvlist[intn-2].y-real_y_off);
            vnminus1->x = ZOOMED_SIZE(intvlist[intn-1].x-real_x_off);
            vnminus1->y = ZOOMED_SIZE(intvlist[intn-1].y-real_y_off);
         }
      }
   } else {
      int x, y;

      if (poly_ptr->curved != LT_INTSPLINE) {
         IntPoint *vs=poly_ptr->vlist;
   
         if (retracted_arrow) {
            TransformPointThroughCTM(vs[1].x-obj_ptr->x, vs[1].y-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            v0->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            v0->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(vs[2].x-obj_ptr->x, vs[2].y-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            v1->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            v1->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(vs[n-3].x-obj_ptr->x, vs[n-3].y-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            vnminus2->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            vnminus2->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(vs[n-2].x-obj_ptr->x, vs[n-2].y-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            vnminus1->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            vnminus1->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
         } else {
            TransformPointThroughCTM(vs[0].x-obj_ptr->x, vs[0].y-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            v0->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            v0->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(vs[1].x-obj_ptr->x, vs[1].y-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            v1->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            v1->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(vs[n-2].x-obj_ptr->x, vs[n-2].y-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            vnminus2->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            vnminus2->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(vs[n-1].x-obj_ptr->x, vs[n-1].y-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            vnminus1->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            vnminus1->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
         }
      } else {
         int intn=poly_ptr->intn;
         IntPoint *intvlist=poly_ptr->intvlist;
   
         if (retracted_arrow) {
            int n=poly_ptr->n;
            IntPoint *vs=poly_ptr->vlist;

            TransformPointThroughCTM(vs[1].x-obj_ptr->x, vs[1].y-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            v0->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            v0->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(intvlist[2].x-obj_ptr->x,
                  intvlist[2].y-obj_ptr->y, obj_ptr->ctm, &x, &y);
            v1->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            v1->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(intvlist[n-3].x-obj_ptr->x,
                  intvlist[n-3].y-obj_ptr->y, obj_ptr->ctm, &x, &y);
            vnminus2->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            vnminus2->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(vs[n-2].x-obj_ptr->x, vs[n-2].y-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            vnminus1->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            vnminus1->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
         } else {
            TransformPointThroughCTM(intvlist[0].x-obj_ptr->x,
                  intvlist[0].y-obj_ptr->y, obj_ptr->ctm, &x, &y);
            v0->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            v0->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(intvlist[1].x-obj_ptr->x,
                  intvlist[1].y-obj_ptr->y, obj_ptr->ctm, &x, &y);
            v1->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            v1->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(intvlist[n-2].x-obj_ptr->x,
                  intvlist[n-2].y-obj_ptr->y, obj_ptr->ctm, &x, &y);
            vnminus2->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            vnminus2->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);
            TransformPointThroughCTM(intvlist[n-1].x-obj_ptr->x,
                  intvlist[n-1].y-obj_ptr->y, obj_ptr->ctm, &x, &y);
            vnminus1->x = ZOOMED_SIZE(x+obj_ptr->x-real_x_off);
            vnminus1->y = ZOOMED_SIZE(y+obj_ptr->y-real_y_off);

            v0->x = ZOOMED_SIZE(intvlist[0].x-real_x_off);
            v0->y = ZOOMED_SIZE(intvlist[0].y-real_y_off);
            v1->x = ZOOMED_SIZE(intvlist[1].x-real_x_off);
            v1->y = ZOOMED_SIZE(intvlist[1].y-real_y_off);
            vnminus2->x = ZOOMED_SIZE(intvlist[intn-2].x-real_x_off);
            vnminus2->y = ZOOMED_SIZE(intvlist[intn-2].y-real_y_off);
            vnminus1->x = ZOOMED_SIZE(intvlist[intn-1].x-real_x_off);
            vnminus1->y = ZOOMED_SIZE(intvlist[intn-1].y-real_y_off);
         }
      }
   }
}

void DrawPolyObj (Win, XOff, YOff, ObjPtr)
   Window		Win;
   int			XOff, YOff;
   struct ObjRec	* ObjPtr;
{
   register struct PolyRec	* poly_ptr = ObjPtr->detail.p;
   XPoint			* v, tmp_v[4];
   XPoint			v0, v1, vnminus2, vnminus1;
   int				pen, width, pixel, fill, n, dash;
   int				real_x_off, real_y_off;
   int				style, aw, ah, num_pts;
   int				left_dx, left_dy, right_dx, right_dy;
   int				retracted_arrow=FALSE;
   double			len, sin, cos;
   XGCValues			values;

   n = poly_ptr->n;
   fill = poly_ptr->fill;
   width = poly_ptr->width;
   aw = poly_ptr->aw;
   ah = poly_ptr->ah;
   pen = poly_ptr->pen;
   style = poly_ptr->style;
   dash = poly_ptr->dash;
   pixel = colorPixels[ObjPtr->color];

   if (fill == NONEPAT && pen == NONEPAT) return;

   if (NeedsToCachePolyObj(ObjPtr) && poly_ptr->rotated_vlist==NULL) {
      MakeCachedPoly(ObjPtr);
   }
   real_x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   real_y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);

   v = poly_ptr->svlist;
   num_pts = poly_ptr->sn;

   v[num_pts].x = v[0].x; v[num_pts].y = v[0].y;

   if (fill != NONEPAT)
   {
      values.foreground = (fill == BACKPAT) ? myBgPixel : pixel;
      values.function = GXcopy;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[fill];
      XChangeGC (mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple, &values);
      if (ObjPtr->ctm == NULL)
         XFillPolygon (mainDisplay, Win, drawGC, v, num_pts+1, Complex,
               CoordModeOrigin);
      else
         XFillPolygon (mainDisplay, Win, drawGC, poly_ptr->rotated_vlist,
               poly_ptr->rotated_n+1, Complex, CoordModeOrigin);
   }

   if (pen == NONEPAT) return;

   retracted_arrow = (RetractedArrowAttr(ObjPtr) ||
         AutoRetractedArrowAttr(ObjPtr, TRUE));

   SetArrowVsForDraw(ObjPtr, retracted_arrow, real_x_off, real_y_off,
         &v0, &v1, &vnminus2, &vnminus1);

   aw = ZOOMED_SIZE(aw); if (aw == 0) aw = 1;
   ah = ZOOMED_SIZE(ah); if (ah == 0) ah = 1;

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
         GCForeground | GCFunction | GCFillStyle | GCStipple | GCLineWidth |
         GCLineStyle | GCJoinStyle, &values);

   left_dx = v1.x - v0.x;
   left_dy = v1.y - v0.y;

   if ((style & LS_LEFT) && (left_dx != 0 || left_dy != 0))
   {  /* adjust the first point */
      len = (double)sqrt((double)(((double)left_dx)*((double)left_dx) +
            ((double)left_dy)*((double)left_dy)));
      sin = ((double)left_dy)/len;
      cos = ((double)left_dx)/len;

      tmp_v[0].x = tmp_v[3].x = v0.x;
      tmp_v[0].y = tmp_v[3].y = v0.y;
      tmp_v[1].x = round(v0.x + aw*cos - ah*sin);
      tmp_v[1].y = round(v0.y + aw*sin + ah*cos);
      tmp_v[2].x = round(v0.x + aw*cos + ah*sin);
      tmp_v[2].y = round(v0.y + aw*sin - ah*cos);

      XFillPolygon (mainDisplay, Win, drawGC, tmp_v, 4, Convex,
            CoordModeOrigin);
   }

   right_dx = vnminus1.x - vnminus2.x;
   right_dy = vnminus1.y - vnminus2.y;

   if ((style & LS_RIGHT) && (right_dx != 0 || right_dy != 0))
   {  /* adjust the last point */
      len = (double)sqrt((double)(((double)right_dx)*((double)right_dx) +
            ((double)right_dy)*((double)right_dy)));
      sin = ((double)right_dy)/len;
      cos = ((double)right_dx)/len;

      tmp_v[0].x = tmp_v[3].x = vnminus1.x;
      tmp_v[0].y = tmp_v[3].y = vnminus1.y;
      tmp_v[1].x = round(vnminus1.x - aw*cos + ah*sin);
      tmp_v[1].y = round(vnminus1.y - aw*sin - ah*cos);
      tmp_v[2].x = round(vnminus1.x - aw*cos - ah*sin);
      tmp_v[2].y = round(vnminus1.y - aw*sin + ah*cos);

      XFillPolygon (mainDisplay, Win, drawGC, tmp_v, 4, Convex,
            CoordModeOrigin);
   }

   if (style != LS_PLAIN)
   {
#ifdef HP_LINE_BUG
      if (poly_ptr->asn == 2)
      {
         XPoint hp_sv[3];

         hp_sv[0].x=poly_ptr->asvlist[0].x; hp_sv[0].y=poly_ptr->asvlist[0].y;
         hp_sv[1].x=poly_ptr->asvlist[0].x; hp_sv[1].y=poly_ptr->asvlist[0].y;
         hp_sv[2].x=poly_ptr->asvlist[1].x; hp_sv[2].y=poly_ptr->asvlist[1].y;
         XDrawLines (mainDisplay, Win, drawGC, hp_sv, 3, CoordModeOrigin);
      }
      else
         XDrawLines (mainDisplay, Win, drawGC, poly_ptr->asvlist, poly_ptr->asn,
               CoordModeOrigin);
#else
      if (ObjPtr->ctm == NULL) {
         XDrawLines (mainDisplay, Win, drawGC, poly_ptr->asvlist, poly_ptr->asn,
               CoordModeOrigin);
      } else {
         XDrawLines (mainDisplay, Win, drawGC, poly_ptr->rotated_asvlist,
               poly_ptr->rotated_asn, CoordModeOrigin);
      }
#endif
   }
   else
   {
#ifdef HP_LINE_BUG
      if (num_pts == 2)
      {
         XPoint hp_sv[3];

         hp_sv[0].x = v[0].x; hp_sv[0].y = v[0].y;
         hp_sv[1].x = v[0].x; hp_sv[1].y = v[0].y;
         hp_sv[2].x = v[1].x; hp_sv[2].y = v[1].y;
         XDrawLines (mainDisplay, Win, drawGC, hp_sv, 3, CoordModeOrigin);
      }
      else
         XDrawLines (mainDisplay, Win, drawGC, v, num_pts, CoordModeOrigin);
#else
      if (ObjPtr->ctm == NULL) {
         XDrawLines (mainDisplay, Win, drawGC, v, num_pts, CoordModeOrigin);
      } else {
         XDrawLines (mainDisplay, Win, drawGC, poly_ptr->rotated_vlist,
               poly_ptr->rotated_n, CoordModeOrigin);
      }
#endif
   }
}

static char	hexValue[] = "0123456789abcdef";

void SaveSmoothHinge (FP, Curved, NumPts, Smooth)
   FILE	* FP;
   int	Curved, NumPts;
   char	* Smooth;
{
   register int	nibble_count=0, bit_count=0, data=0, j;

   if (Curved == LT_INTSPLINE || Smooth == NULL) return;

   for (j = 0; j < NumPts; j++)
   {
      data = (Smooth[j] ? (data<<1) | 1 : (data<<1));

      if (++bit_count == 4)
      {
         if (nibble_count++ == 64)
         {
            nibble_count = 1;
            if (fprintf (FP, "\n     ") == EOF) writeFileFailed = TRUE;
         }
         if (fprintf (FP, "%c", hexValue[data]) == EOF)
            writeFileFailed = TRUE;
         bit_count = 0;
         data = 0;
      }
   }
   if ((NumPts & 0x3) != 0)
   {
      data <<= (4 - (NumPts & 0x3));
      if (nibble_count++ == 64)
      {
         nibble_count = 1;
         if (fprintf (FP, "\n     ") == EOF) writeFileFailed = TRUE;
      }
      if (fprintf (FP, "%c", hexValue[data]) == EOF) writeFileFailed = TRUE;
   }
}

void SavePolyObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{
   register int		i, n;
   int			count;
   struct PolyRec	* poly_ptr = ObjPtr->detail.p;

   n = poly_ptr->n;
   if (fprintf (FP, "poly('%s',%1d,[\n\t",
         colorMenuItems[ObjPtr->color], poly_ptr->n) == EOF)
      writeFileFailed = TRUE;
   for (i = 0, count = 0; i < n-1; i++)
   {
      if (fprintf (FP, "%1d,%1d,", poly_ptr->vlist[i].x,
            poly_ptr->vlist[i].y) == EOF)
         writeFileFailed = TRUE;
      if (++count == 8)
      {
         count = 0;
         if (fprintf (FP, "\n\t") == EOF) writeFileFailed = TRUE;
      }
   }
   if (fprintf (FP, "%1d,%1d],", poly_ptr->vlist[n-1].x,
         poly_ptr->vlist[n-1].y) == EOF)
      writeFileFailed = TRUE;

   if (fprintf (FP,
         "%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,'%s','%s','%s',",
         poly_ptr->style, poly_ptr->width, poly_ptr->pen, ObjPtr->id,
         poly_ptr->curved, poly_ptr->fill, poly_ptr->dash, ObjPtr->rotation,
         poly_ptr->aw, poly_ptr->ah, ObjPtr->locked, ObjPtr->ctm!=NULL,
         ObjPtr->invisible, poly_ptr->width_spec, poly_ptr->aw_spec,
         poly_ptr->ah_spec) == EOF) {
      writeFileFailed = TRUE;
   }
   if (fprintf (FP, "\n    \"") == EOF) writeFileFailed = TRUE;
   SaveSmoothHinge (FP, poly_ptr->curved, poly_ptr->n, poly_ptr->smooth);
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

int ReadSmoothHinge (FP, Curved, NumPts, Smooth)
   FILE	* FP;
   int	Curved, NumPts;
   char	* Smooth;
{
   int	num_nibbles=NumPts>>2, nibble_count=0, bit_count=0, j, k;
   char	* c_ptr, inbuf[MAXSTRING+1], msg[MAXSTRING];

   if ((NumPts & 0x3) != 0) num_nibbles++;
   fgets (inbuf, MAXSTRING, FP);
   scanLineNum++;
   if (Curved == LT_INTSPLINE || Smooth == NULL) return (TRUE);
   if ((c_ptr = strchr (inbuf, '"')) == NULL)
   {
      (void) sprintf (msg, "%s, %d:  Invalid smooth/hinge spec for a poly.",
            scanFileName, scanLineNum);
      if (PRTGIF)
         fprintf (stderr, "%s\n", msg);
      else
         Msg (msg);
      return (FALSE);
   }
   c_ptr++;
   for (j = 0; j < num_nibbles; j++)
   {
      int	data=0;

      if (nibble_count++ == 64)
      {
         fgets (inbuf, MAXSTRING, FP);
         scanLineNum++;
         for (c_ptr=inbuf; *c_ptr == ' '; c_ptr++) ;
         nibble_count = 1;
      }
      if (*c_ptr >= '0' && *c_ptr <= '9')
         data = (int)(*c_ptr++) - (int)('0');
      else if (*c_ptr >= 'a' && *c_ptr <= 'f')
         data = (int)(*c_ptr++) - (int)('a') + 10;
      for (k = 0; k < 4; k++)
      {
         if (bit_count++ == NumPts) break;

         Smooth[(j<<2)+k] = (data & (1<<(3-k)) ? TRUE : FALSE);
      }
   }
   return (TRUE);
}

#define GETVALUE(val,name) ScanValue("%d", &(val), name, "poly")
#define GETSTRNG(val,name) ScanValue("%s", (val), name, "poly")

void ReadPolyObj (FP, Inbuf, ObjPtr)
   FILE			* FP;
   char			* Inbuf;
   struct ObjRec	* * ObjPtr;
{
   register int		i;
   struct PolyRec	* poly_ptr;
   IntPoint		*v;
   char			color_str[40], * s, inbuf[MAXSTRING+1], msg[MAXSTRING];
   char			width_spec[40], aw_spec[40], ah_spec[40];
   int			num_pts, ltx=0, lty=0, rbx=0, rby=0, x, y, id=0;
   int			initialized, rotation, count, new_alloc;
   int			style, width=0, pen, curved, fill, dash, locked=FALSE;
   int			aw=origArrowHeadW[6], ah=origArrowHeadH[6];
   char			* smooth=NULL;
   int			real_x=0, real_y=0, transformed=FALSE, invisible=FALSE;
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
      (void) sprintf (msg, "%s, %d:  Invalid number of points in poly",
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
   poly_ptr = (struct PolyRec *)malloc(sizeof(struct PolyRec));
   if (poly_ptr == NULL) FailAllocMessage();
   memset(poly_ptr, 0, sizeof(struct PolyRec));

   if (num_pts == 1) {
      v = (IntPoint*)malloc(4*sizeof(IntPoint));
      if (v == NULL) FailAllocMessage();
      smooth = (char*)malloc(4*sizeof(char));
      if (smooth == NULL) FailAllocMessage();
   } else {
      v = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
      if (v == NULL) FailAllocMessage();
      smooth = (char*)malloc((num_pts+1)*sizeof(char));
      if (smooth == NULL) FailAllocMessage();
   }

   initialized = FALSE;

   *width_spec = *aw_spec = *ah_spec = '\0';
   if (fileVersion <= 13)
   {
      for (i = 0; i < num_pts; i++)
      {
         if (GETVALUE (x, "x") == INVALID || GETVALUE (y, "y") == INVALID)
         {
            free(*ObjPtr);
            free(poly_ptr);
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
            free(poly_ptr);
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

   if (num_pts == 1)
   {
      sprintf (msg, "%s (%1d,%1d) converted to double point poly.",
            "Single point poly", v[0].x, v[0].y);
      if (PRTGIF)
         fprintf (stderr, "%s\n", msg);
      else
         Msg (msg);
      v[1].x = v[0].x;
      v[1].y = v[0].y;
      num_pts = 2;
   }

   poly_ptr->n = num_pts;

   dash = 0;
   rotation = 0;
   if (fileVersion == INVALID)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      id = objId++;
      fill = NONEPAT;
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
   }
   else if (fileVersion <= 3)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
      fill = NONEPAT;
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
   }
   else if (fileVersion <= 4)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
      fill = NONEPAT;
      switch (width)
      {
         case 1: width = 3; break;
         case 2: width = 6; break;
      }
   }
   else if (fileVersion <= 5)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
      switch (width)
      {
         case 1: width = 3; break;
         case 2: width = 6; break;
      }
   }
   else if (fileVersion <= 8)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 13)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 16)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 25)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID ||
          GETVALUE (aw,       "arrow head width") == INVALID ||
          GETVALUE (ah,       "arrow head height") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 32)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID ||
          GETVALUE (aw,       "arrow head width") == INVALID ||
          GETVALUE (ah,       "arrow head height") == INVALID ||
          GETVALUE (locked,   "locked") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else
   {
#ifdef _TGIF_DBG
      if (GETVALUE (style,       "style") == INVALID ||
          GETVALUE (width,       "width") == INVALID ||
          GETVALUE (pen,         "pen") == INVALID ||
          GETVALUE (id,          "id") == INVALID ||
          GETVALUE (curved,      "curved") == INVALID ||
          GETVALUE (fill,        "fill") == INVALID ||
          GETVALUE (dash,        "dash") == INVALID ||
          GETVALUE (rotation,    "rotation") == INVALID ||
          GETVALUE (aw,          "arrow head width") == INVALID ||
          GETVALUE (ah,          "arrow head height") == INVALID ||
          GETVALUE (locked,      "locked") == INVALID ||
          GETVALUE (transformed, "transformed") == INVALID ||
          GETVALUE (invisible,   "invisible") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
      if (GETSTRNG(width_spec,  "width_spec") != INVALID) {
         UtilRemoveQuotes(width_spec);
      } else {
         sprintf(width_spec, "%1d", width);
         fprintf(stderr, "Bad width_spec in poly... Fix this file!\n");
      }
      if (GETSTRNG(aw_spec,     "aw_spec") != INVALID) {
         UtilRemoveQuotes(aw_spec);
      } else {
         sprintf(aw_spec, "%1d", aw);
         fprintf(stderr, "Bad width_spec in poly... Fix this file!\n");
      }
      if (GETSTRNG(ah_spec,     "ah_spec") != INVALID) {
         UtilRemoveQuotes(ah_spec);
      } else {
         sprintf(ah_spec, "%1d", ah);
         fprintf(stderr, "Bad width_spec in poly... Fix this file!\n");
      }
#else /* ~_TGIF_DBG */
      if (GETVALUE (style,       "style") == INVALID ||
          GETVALUE (width,       "width") == INVALID ||
          GETVALUE (pen,         "pen") == INVALID ||
          GETVALUE (id,          "id") == INVALID ||
          GETVALUE (curved,      "curved") == INVALID ||
          GETVALUE (fill,        "fill") == INVALID ||
          GETVALUE (dash,        "dash") == INVALID ||
          GETVALUE (rotation,    "rotation") == INVALID ||
          GETVALUE (aw,          "arrow head width") == INVALID ||
          GETVALUE (ah,          "arrow head height") == INVALID ||
          GETVALUE (locked,      "locked") == INVALID ||
          GETVALUE (transformed, "transformed") == INVALID ||
          GETVALUE (invisible,   "invisible") == INVALID ||
          GETSTRNG (width_spec,  "width_spec") == INVALID ||
          GETSTRNG (aw_spec,     "aw_spec") == INVALID ||
          GETSTRNG (ah_spec,     "ah_spec") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
      UtilRemoveQuotes(width_spec);
      UtilRemoveQuotes(aw_spec);
      UtilRemoveQuotes(ah_spec);
#endif /* _TGIF_DBG */
   }

   if (fileVersion <= 16 && width <= 6)
   {
      aw = origArrowHeadW[width];
      ah = origArrowHeadH[width];
      width = origWidthOfLine[width];
   }
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
            smooth[0] = smooth[num_pts-1] = FALSE;
            for (i=1; i < num_pts-1; i++) smooth[i] = TRUE;
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
      sprintf(aw_spec, "%1d", aw);
      sprintf(ah_spec, "%1d", ah);
   }

   fill = UpgradePenFill (fill);
   pen = UpgradePenFill (pen);

   poly_ptr->style = style;
   poly_ptr->width = width;
   poly_ptr->aw = aw;
   poly_ptr->ah = ah;
   UtilStrCpy(poly_ptr->width_spec, sizeof(poly_ptr->width_spec), width_spec);
   UtilStrCpy(poly_ptr->aw_spec, sizeof(poly_ptr->aw_spec), aw_spec);
   UtilStrCpy(poly_ptr->ah_spec, sizeof(poly_ptr->ah_spec), ah_spec);
   poly_ptr->pen = pen;
   poly_ptr->curved = curved;
   poly_ptr->fill = fill;
   poly_ptr->dash = dash;

   poly_ptr->vlist = v;
   poly_ptr->smooth = smooth;
   poly_ptr->svlist = poly_ptr->asvlist = NULL;
   poly_ptr->intvlist = NULL;

   poly_ptr->rotated_n = poly_ptr->rotated_asn = 0;
   poly_ptr->rotated_vlist = poly_ptr->rotated_asvlist = NULL;

   (*ObjPtr)->x = ltx;
   (*ObjPtr)->y = lty;
   (*ObjPtr)->color = QuickFindColorIndex(*ObjPtr, color_str, &new_alloc, TRUE);
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->type = OBJ_POLY;
   (*ObjPtr)->obbox.ltx = ltx;
   (*ObjPtr)->obbox.lty = lty;
   (*ObjPtr)->obbox.rbx = rbx;
   (*ObjPtr)->obbox.rby = rby;
   (*ObjPtr)->detail.p = poly_ptr;
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
   if (poly_ptr->curved != LT_INTSPLINE)
      UpdPolyBBox (*ObjPtr, poly_ptr->n, poly_ptr->vlist);
   else
      UpdPolyBBox (*ObjPtr, poly_ptr->intn, poly_ptr->intvlist);
}

void FreePolyObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   if (ObjPtr->detail.p->svlist != NULL) free(ObjPtr->detail.p->svlist);
   if (ObjPtr->detail.p->asvlist != NULL) free(ObjPtr->detail.p->asvlist);
   if (ObjPtr->detail.p->intvlist != NULL) free(ObjPtr->detail.p->intvlist);
   if (ObjPtr->detail.p->rotated_vlist != NULL) {
      free(ObjPtr->detail.p->rotated_vlist);
   }
   if (ObjPtr->detail.p->rotated_asvlist != NULL) {
      free(ObjPtr->detail.p->rotated_asvlist);
   }
   free(ObjPtr->detail.p->vlist);
   if (ObjPtr->detail.p->smooth != NULL) free(ObjPtr->detail.p->smooth);
   free(ObjPtr->detail.p);
   free(ObjPtr);
}
