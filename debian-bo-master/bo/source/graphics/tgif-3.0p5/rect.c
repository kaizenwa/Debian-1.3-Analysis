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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/rect.c,v 3.0 1996/05/06 16:07:09 william Exp $";
#endif

#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "arc.e"
#include "color.e"
#include "dialog.e"
#include "poly.e"
#ifndef _NO_EXTERN
#include "rect.e"
#endif
#include "setup.e"
#include "spline.e"

void SetRotateVs (vs, ltx, lty, rbx, rby)
   XPoint	* vs; /* array of 5 points */
   int		ltx, lty, rbx, rby;
{
   vs[0].x = vs[4].x = ltx; vs[0].y = vs[4].y = lty;
   vs[1].x = rbx; vs[1].y = lty;
   vs[2].x = rbx; vs[2].y = rby;
   vs[3].x = ltx; vs[3].y = rby;
}

void SetBBRec (bbox, ltx, lty, rbx, rby)
   struct BBRec	* bbox;
   int		ltx, lty, rbx, rby;
{
   bbox->ltx = ltx; bbox->lty = lty; bbox->rbx = rbx; bbox->rby = rby;
}

void ConcatCTM (ctm, orig_ctm, new_ctm)
   struct XfrmMtrxRec	* ctm, * orig_ctm, * new_ctm;
   /* Note: CTM_SX, CTM_SIN, CTM_MSIN, and CTM_SY are scaled 1000 times */
   /*       while CTM_TX and CTM_TY are not scaled */
{
   new_ctm->m[0] = round(((float)(ctm->m[0]*orig_ctm->m[0] +
         ctm->m[1]*orig_ctm->m[2]))/1000.0);
   new_ctm->m[1] = round(((float)(ctm->m[0]*orig_ctm->m[1] +
         ctm->m[1]*orig_ctm->m[3]))/1000.0);
   new_ctm->m[2] = round(((float)(ctm->m[2]*orig_ctm->m[0] +
         ctm->m[3]*orig_ctm->m[2]))/1000.0);
   new_ctm->m[3] = round(((float)(ctm->m[2]*orig_ctm->m[1] +
         ctm->m[3]*orig_ctm->m[3]))/1000.0);
   new_ctm->m[4] = round(((float)(ctm->m[4]*orig_ctm->m[0] +
         ctm->m[5]*orig_ctm->m[2]))/1000.0) + orig_ctm->m[4];
   new_ctm->m[5] = round(((float)(ctm->m[4]*orig_ctm->m[1] +
         ctm->m[5]*orig_ctm->m[3]))/1000.0) + orig_ctm->m[5];
}

void TransformPointThroughCTM (X, Y, ctm, NewX, NewY)
   int			X, Y, * NewX, * NewY;
   struct XfrmMtrxRec	* ctm;
{
   *NewX = round(((float)(X*ctm->m[0] + Y*ctm->m[2]))/1000.0) + ctm->m[CTM_TX];
   *NewY = round(((float)(X*ctm->m[1] + Y*ctm->m[3]))/1000.0) + ctm->m[CTM_TY];
}

void ReverseTransformPointThroughCTM (X, Y, ctm, NewX, NewY)
   int			X, Y, * NewX, * NewY;
   struct XfrmMtrxRec	* ctm;
   /*
    *           [ a b 0 ]
    * Let ctm = [ c d 0 ]
    *           [ e f 1 ]
    *
    *          d(X-e) - c(Y-f)
    * *NewX = -----------------
    *              ad - bc
    *
    *          b(X-e) - a(Y-f)
    * *NewY = -----------------
    *              bc - ad
    */
{
   int		ad_bc=ctm->m[0]*ctm->m[3]-ctm->m[1]*ctm->m[2];
   int		term_1=X-ctm->m[CTM_TX], term_2=Y-ctm->m[CTM_TY];
   float	x, y;

   x = ((float)1000.0) *
         ((float)(ctm->m[3]*term_1 - ctm->m[2]*term_2)) / ((float)ad_bc);
   y = ((float)1000.0) *
         ((float)(ctm->m[0]*term_2 - ctm->m[1]*term_1)) / ((float)(ad_bc));
   *NewX = round(x);
   *NewY = round(y);
}

void TransformObjectV(ObjPtr, VIn, VOut)
   struct ObjRec *ObjPtr;
   IntPoint *VIn, *VOut;
{
   int x, y;

   TransformPointThroughCTM(VIn->x-ObjPtr->x, VIn->y-ObjPtr->y, ObjPtr->ctm,
         &x, &y);
   VOut->x = x + ObjPtr->x;
   VOut->y = y + ObjPtr->y;
}

void ReversedTransformObjectV(ObjPtr, VIn, VOut)
   struct ObjRec *ObjPtr;
   IntPoint *VIn, *VOut;
{
   int x, y;

   ReverseTransformPointThroughCTM(VIn->x-ObjPtr->x, VIn->y-ObjPtr->y,
         ObjPtr->ctm, &x, &y);
   VOut->x = x + ObjPtr->x;
   VOut->y = y + ObjPtr->y;
}

void TransformOffsetBBoxThroughCTM (bbox, ctm, vs)
   struct BBRec		* bbox;
   struct XfrmMtrxRec	* ctm;
   IntPoint		* vs; /* array of 5 points */
{
   int	x, y;

   TransformPointThroughCTM (bbox->ltx, bbox->lty, ctm, &x, &y);
   vs[0].x = vs[4].x = x; vs[0].y = vs[4].y = y;
   TransformPointThroughCTM (bbox->rbx, bbox->lty, ctm, &x, &y);
   vs[1].x = x; vs[1].y = y;
   TransformPointThroughCTM (bbox->rbx, bbox->rby, ctm, &x, &y);
   vs[2].x = x; vs[2].y = y;
   TransformPointThroughCTM (bbox->ltx, bbox->rby, ctm, &x, &y);
   vs[3].x = x; vs[3].y = y;
}

void GetTransformedOBBoxOffsetVs (ObjPtr, Vs)
   struct ObjRec	* ObjPtr;
   XPoint		* Vs; /* array of 5 points */
{
   int		x, y;
   struct BBRec	obbox;
   IntPoint	vs[5];

   if (ObjPtr->ctm == NULL) return;

   obbox.ltx = ObjPtr->orig_obbox.ltx - ObjPtr->x;
   obbox.lty = ObjPtr->orig_obbox.lty - ObjPtr->y;
   obbox.rbx = ObjPtr->orig_obbox.rbx - ObjPtr->x;
   obbox.rby = ObjPtr->orig_obbox.rby - ObjPtr->y;
   TransformOffsetBBoxThroughCTM (&obbox, ObjPtr->ctm, vs);
   x = OFFSET_X(vs[0].x + ObjPtr->x);
   y = OFFSET_Y(vs[0].y + ObjPtr->y);
   Vs[0].x = Vs[4].x = x; Vs[0].y = Vs[4].y = y;
   x = OFFSET_X(vs[1].x + ObjPtr->x);
   y = OFFSET_Y(vs[1].y + ObjPtr->y);
   Vs[1].x = x; Vs[1].y = y;
   x = OFFSET_X(vs[2].x + ObjPtr->x);
   y = OFFSET_Y(vs[2].y + ObjPtr->y);
   Vs[2].x = x; Vs[2].y = y;
   x = OFFSET_X(vs[3].x + ObjPtr->x);
   y = OFFSET_Y(vs[3].y + ObjPtr->y);
   Vs[3].x = x; Vs[3].y = y;
}

void GetTransformedOBBoxAbsVs (ObjPtr, Vs)
   struct ObjRec	* ObjPtr;
   IntPoint		* Vs; /* array of 5 points */
{
   int		x, y;
   struct BBRec	obbox;
   IntPoint	vs[5];

   if (ObjPtr->ctm == NULL) return;

   obbox.ltx = ObjPtr->orig_obbox.ltx - ObjPtr->x;
   obbox.lty = ObjPtr->orig_obbox.lty - ObjPtr->y;
   obbox.rbx = ObjPtr->orig_obbox.rbx - ObjPtr->x;
   obbox.rby = ObjPtr->orig_obbox.rby - ObjPtr->y;
   TransformOffsetBBoxThroughCTM (&obbox, ObjPtr->ctm, vs);
   x = vs[0].x + ObjPtr->x; y = vs[0].y + ObjPtr->y;
   Vs[0].x = Vs[4].x = x; Vs[0].y = Vs[4].y = y;
   x = vs[1].x + ObjPtr->x; y = vs[1].y + ObjPtr->y;
   Vs[1].x = x; Vs[1].y = y;
   x = vs[2].x + ObjPtr->x; y = vs[2].y + ObjPtr->y;
   Vs[2].x = x; Vs[2].y = y;
   x = vs[3].x + ObjPtr->x; y = vs[3].y + ObjPtr->y;
   Vs[3].x = x; Vs[3].y = y;
}

void SetCTM(ObjPtr, ctm)
   struct ObjRec *ObjPtr;
   struct XfrmMtrxRec *ctm;
   /* ObjPtr->ctm must be NULL */
{
   IntPoint abs_obj_obbox_vs[5];
   struct XfrmMtrxRec new_ctm;
   int ltx, lty, rbx, rby;

   if (ObjPtr->ctm != NULL) {
      sprintf(gszMsgBox, "Error: SetCTM() is called with ObjPtr->ctm != NULL");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      free(ObjPtr->ctm);
   }
   memcpy(&ObjPtr->orig_obbox, &ObjPtr->obbox, sizeof(struct BBRec));
   if (ObjPtr->type == OBJ_TEXT) {
      memcpy(&ObjPtr->detail.t->orig_bbox, &ObjPtr->bbox,
            sizeof(struct BBRec));
   }
   ObjPtr->ctm = (struct XfrmMtrxRec *)malloc(sizeof(struct XfrmMtrxRec));
   if (ObjPtr->ctm == NULL) FailAllocMessage();
   ObjPtr->ctm->m[CTM_SX] = ObjPtr->ctm->m[CTM_SY] = 1000;
   ObjPtr->ctm->m[CTM_SIN] = ObjPtr->ctm->m[CTM_MSIN] = 0;
   ObjPtr->ctm->m[CTM_TX] = ObjPtr->ctm->m[CTM_TY] = 0;
   ConcatCTM(ObjPtr->ctm, ctm, &new_ctm);
   memcpy(ObjPtr->ctm, &new_ctm, sizeof(struct XfrmMtrxRec));

   GetTransformedOBBoxAbsVs(ObjPtr, abs_obj_obbox_vs);

   ltx = min(min(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
         min(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
   rbx = max(max(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
         max(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
   lty = min(min(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
         min(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));
   rby = max(max(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
         max(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));

   ObjPtr->obbox.ltx = ltx; ObjPtr->obbox.lty = lty;
   ObjPtr->obbox.rbx = rbx; ObjPtr->obbox.rby = rby;
   GetTransformedOBBoxOffsetVs(ObjPtr, ObjPtr->rotated_obbox);
}

int IntersectRect(BBox1, BBox2, BBox3)
   struct BBRec	BBox1, BBox2, *BBox3;
   /* returns the intersection of BBox1 and BBox2 in BBox3 */
{
   if (BBoxIntersect (BBox1, BBox2))
   {
      BBox3->ltx = max(BBox1.ltx,BBox2.ltx);
      BBox3->lty = max(BBox1.lty,BBox2.lty);
      BBox3->rbx = min(BBox1.rbx,BBox2.rbx);
      BBox3->rby = min(BBox1.rby,BBox2.rby);
      return (TRUE);
   }
   return (FALSE);
}

int Inside (BBox1, BBox2)
   struct BBRec	BBox1, BBox2;
   /* returns TRUE if BBox1 is inside BBox2 */
{
   return (BBox1.ltx >= BBox2.ltx && BBox1.lty >= BBox2.lty &&
         BBox1.rbx <= BBox2.rbx && BBox1.rby <= BBox2.rby);
}

int BBoxIntersect (Rect1, Rect2)
   struct BBRec	Rect1, Rect2;
{
   if (Rect1.ltx < Rect2.ltx)
      if (Rect1.lty < Rect2.lty)
         return (Rect1.rbx >= Rect2.ltx && Rect1.rby >= Rect2.lty);
      else
         return (Rect1.rbx >= Rect2.ltx && Rect1.lty <= Rect2.rby);
   else
      if (Rect1.lty < Rect2.lty)
         return (Rect1.ltx <= Rect2.rbx && Rect1.rby >= Rect2.lty);
      else
         return (Rect1.ltx <= Rect2.rbx && Rect1.lty <= Rect2.rby);
}

int PointInBBox (X, Y, Rect)
   int	X, Y;
   struct BBRec	Rect;
{
   return (X >= Rect.ltx && Y >= Rect.lty && X <= Rect.rbx && Y <= Rect.rby);
}

#define POLYGON_TOL (1.0e-5)

int PointInPolygon (X, Y, NumPts, V)
   int		X, Y, NumPts;
   XPoint	* V;
{
   register double	x1, x2, y1, y2;
   double		m, y_int;
   int			n, count = 0;

   x2 = (double)V[0].x;
   y2 = (double)V[0].y;
   for (n = 0; n < NumPts-1; n++)
   {
      x1 = x2;
      y1 = y2;
      x2 = (double)V[n+1].x;
      y2 = (double)V[n+1].y;
      if (x2 == x1)
      {
         if (X == x1 && Y >= min(y1,y2) && Y <= max(y1,y2)) count++;
         continue;
      }
      if (x2 > x1)
      {
         if (X >= x2 || X < x1) continue;
      }
      else
      {
         if (X > x1 || X <= x2) continue;
      }

      m = (y1 - y2) / (x1 - x2);
      y_int = m * X + (y1 - m * x1);
      if (Y <= y_int)
      {
         if (fabs(X-x1) < POLYGON_TOL)
         {
            double	x3 = (double)((n == 0) ? V[NumPts-2].x : V[n-1].x);

            if ((x2 > X && X > x3) || (x3 > X && X > x2)) count++;
         }
         else
            count++;
      }
   }
   return (count & 0x1);
}

int PointOnPoly (X, Y, NumPts, V, W)
   int		X, Y, NumPts, W;
   XPoint	* V;
{
   register double	x1, x2, y1, y2;
   double		x_int, y_int, dx, dy, abs_dx, abs_dy, real_w;
   int			n, horizontal;

   x2 = (double)V[0].x;
   y2 = (double)V[0].y;
   for (n = 0; n < NumPts-1; n++)
   {
      x1 = x2;
      y1 = y2;
      x2 = (double)V[n+1].x;
      y2 = (double)V[n+1].y;

      if (V[n].x==V[n+1].x && V[n].y==V[n+1].y) continue;

      dx = x2 - x1; abs_dx = fabs(dx);
      dy = y2 - y1; abs_dy = fabs(dy);
      horizontal = (abs_dx >= abs_dy);

      if (horizontal)
      {
         if (x2 >= x1)
         {
            if (X < x1 || X > x2) continue;
         }
         else
         {
            if (X < x2 || X > x1) continue;
         }
         real_w = (double)sqrt(abs_dx*abs_dx+abs_dy*abs_dy)*((double)W)/abs_dx;
      }
      else
      {
         if (y2 >= y1)
         {
            if (Y < y1 || Y > y2) continue;
         }
         else
         {
            if (Y < y2 || Y > y1) continue;
         }
         real_w = (double)sqrt(abs_dx*abs_dx+abs_dy*abs_dy)*((double)W)/abs_dy;
      }

      if (abs_dx >= abs_dy)
      {  /* kind of a horizontal segment */
         y_int = y1+(((double)X)-x1)*dy/dx;
         if ((double)fabs((double)Y-y_int) <= (real_w+4)) return (TRUE);
      }
      else
      {  /* kind of a vertical segment */
         x_int = x1+(((double)Y)-y1)*dx/dy;
         if ((double)fabs((double)X-x_int) <= (real_w+4)) return (TRUE);
      }
   }
   return (FALSE);
}

int FindGoodText (XOff, YOff, TextObj)
   int			XOff, YOff;
   struct ObjRec	* TextObj;
   /* XOff and YOff are screen offsets */
{
   if (colorLayers && TextObj->tmp_parent == NULL &&
         !ObjInVisibleLayer(TextObj)) {
      return FALSE;
   }
   if (TextObj->ctm == NULL)
      return TRUE;
   else
      return (PointInPolygon (XOff, YOff, 5, TextObj->rotated_obbox));
}

int FindGoodXBm (XOff, YOff, XBmObj)
   int			XOff, YOff;
   struct ObjRec	* XBmObj;
   /* XOff and YOff are screen offsets */
{
   struct BBRec	bbox;

   if (colorLayers && XBmObj->tmp_parent == NULL &&
         !ObjInVisibleLayer(XBmObj)) {
      return FALSE;
   }
   bbox.ltx = OFFSET_X(XBmObj->obbox.ltx)-3;
   bbox.lty = OFFSET_Y(XBmObj->obbox.lty)-3;
   bbox.rbx = OFFSET_X(XBmObj->obbox.rbx)+3;
   bbox.rby = OFFSET_Y(XBmObj->obbox.rby)+3;
   if (XBmObj->ctm == NULL)
      return (PointInBBox (XOff, YOff, bbox));
   else
      return (PointInPolygon (XOff, YOff, 5, XBmObj->rotated_obbox));
}

int FindGoodXPm (XOff, YOff, XPmObj)
   int			XOff, YOff;
   struct ObjRec	* XPmObj;
   /* XOff and YOff are screen offsets */
{
   struct BBRec	bbox;

   if (colorLayers && XPmObj->tmp_parent == NULL &&
         !ObjInVisibleLayer(XPmObj)) {
      return FALSE;
   }
   bbox.ltx = OFFSET_X(XPmObj->obbox.ltx)-3;
   bbox.lty = OFFSET_Y(XPmObj->obbox.lty)-3;
   bbox.rbx = OFFSET_X(XPmObj->obbox.rbx)+3;
   bbox.rby = OFFSET_Y(XPmObj->obbox.rby)+3;
   if (XPmObj->ctm == NULL)
      return (PointInBBox (XOff, YOff, bbox));
   else
      return (PointInPolygon (XOff, YOff, 5, XPmObj->rotated_obbox));
}

int FindGoodBox (XOff, YOff, BoxObj)
   int			XOff, YOff;
   struct ObjRec	* BoxObj;
   /* XOff and YOff are screen offsets */
{
   struct BBRec	bbox;
   int		w;

   if (colorLayers && BoxObj->tmp_parent == NULL &&
         !ObjInVisibleLayer(BoxObj)) {
      return FALSE;
   }
   bbox.ltx = OFFSET_X(BoxObj->obbox.ltx)-3;
   bbox.lty = OFFSET_Y(BoxObj->obbox.lty)-3;
   bbox.rbx = OFFSET_X(BoxObj->obbox.rbx)+3;
   bbox.rby = OFFSET_Y(BoxObj->obbox.rby)+3;
   if (!PointInBBox (XOff, YOff, bbox)) return (FALSE);

   if (BoxObj->ctm == NULL)
   {
      if (BoxObj->detail.b->fill != NONEPAT) return (TRUE);

      w = HALF_W(BoxObj->detail.b->width);
      bbox.ltx = OFFSET_X(BoxObj->obbox.ltx+w)+3;
      bbox.lty = OFFSET_Y(BoxObj->obbox.lty+w)+3;
      bbox.rbx = OFFSET_X(BoxObj->obbox.rbx-w)-3;
      bbox.rby = OFFSET_Y(BoxObj->obbox.rby-w)-3;
      return (!PointInBBox (XOff, YOff, bbox));
   }
   else
   {
      if (BoxObj->detail.b->fill != NONEPAT)
         if (PointInPolygon (XOff, YOff, 5, BoxObj->rotated_obbox))
            return (TRUE);
      return (PointOnPoly (XOff, YOff, 5, BoxObj->rotated_obbox,
            ZOOMED_HALF_W(BoxObj->detail.b->width)));
   }
}

int FindGoodRCBox (XOff, YOff, RCBoxObj)
   int			XOff, YOff;
   struct ObjRec	* RCBoxObj;
   /* XOff and YOff are screen offsets */
{
   struct RCBoxRec *rcbox_ptr=RCBoxObj->detail.rcb;
   int fill=rcbox_ptr->fill;

   if (colorLayers && RCBoxObj->tmp_parent == NULL &&
         !ObjInVisibleLayer(RCBoxObj)) {
      return FALSE;
   }
   if (RCBoxObj->ctm == NULL) {
      register struct BBRec *obbox;
      register struct BBRec bbox;
      int w, r;

      obbox = &(RCBoxObj->obbox);

      bbox.ltx = OFFSET_X(obbox->ltx)-3;
      bbox.lty = OFFSET_Y(obbox->lty)-3;
      bbox.rbx = OFFSET_X(obbox->rbx)+3;
      bbox.rby = OFFSET_Y(obbox->rby)+3;
      if (!PointInBBox(XOff, YOff, bbox)) return (FALSE);

      if (fill != NONEPAT) return (TRUE);

      r = rcbox_ptr->radius;
      w = HALF_W(rcbox_ptr->width);

      bbox.ltx = OFFSET_X(obbox->ltx+w)+3; bbox.lty = OFFSET_Y(obbox->lty+r)+3;
      bbox.rbx = OFFSET_X(obbox->rbx-w)-3; bbox.rby = OFFSET_Y(obbox->rby-r)-3;

      if (PointInBBox(XOff, YOff, bbox)) {
         return (FALSE);
      } else {
         bbox.ltx = OFFSET_X(obbox->ltx+r)+3;
         bbox.lty = OFFSET_Y(obbox->lty+w)+3;
         bbox.rbx = OFFSET_X(obbox->rbx-r)-3;
         bbox.rby = OFFSET_Y(obbox->rby-w)-3;
         return (!PointInBBox(XOff, YOff, bbox));
      }
   } else {
      if (fill != NONEPAT) {
         if (PointInPolygon(XOff, YOff, rcbox_ptr->rotated_n,
               rcbox_ptr->rotated_vlist)) {
            return TRUE;
         }
      }
      return (PointOnPoly(XOff, YOff, rcbox_ptr->rotated_n,
            rcbox_ptr->rotated_vlist, ZOOMED_HALF_W(rcbox_ptr->width)));
   }
}

int FindGoodOval (XOff, YOff, OvalObj)
   int			XOff, YOff;
   struct ObjRec	* OvalObj;
   /* XOff and YOff are screen offsets */
{
   struct OvalRec	* oval_ptr=OvalObj->detail.o;
   int			w, ltx, lty, rbx, rby;
   int			fill=oval_ptr->fill;

   if (colorLayers && OvalObj->tmp_parent == NULL &&
         !ObjInVisibleLayer(OvalObj)) {
      return FALSE;
   }
   ltx = OFFSET_X(OvalObj->obbox.ltx); lty = OFFSET_Y(OvalObj->obbox.lty);
   rbx = OFFSET_X(OvalObj->obbox.rbx); rby = OFFSET_Y(OvalObj->obbox.rby);

   if (ltx==rbx && lty==rby) return (FALSE);

   if (OvalObj->ctm == NULL)
   {
      double	cx, cy, rx, ry, tmp_x, tmp_y;
      double	x1=0.0, x2=0.0, y1=0.0, y2=0.0;

      cx = ((double)(ltx+rbx)/2.0); cy = ((double)(lty+rby)/2.0);
      rx = ((double)(rbx-ltx)/2.0); ry = ((double)(rby-lty)/2.0);

      if (rx >= ry)
      {  /* flat oval */
         tmp_y = (double)sqrt(fabs((double)(ry*ry*(1 -
               (((double)XOff)-cx)*(((double)XOff)-cx)/rx/rx))));
         y1 = cy - tmp_y;
         y2 = cy + tmp_y;
      }
      else
      {  /* tall oval */
         tmp_x = (double)sqrt(fabs((double)(rx*rx*(1 -
               (((double)YOff)-cy)*(((double)YOff)-cy)/ry/ry))));
         x1 = cx - tmp_x;
         x2 = cx + tmp_x;
      }

      if (fill != NONEPAT)
      {
         if (rx >= ry)
         {  /* flat oval */
            if (YOff >= y1-4 && y2+4 >= YOff) return (TRUE);
         }
         else
         {  /* tall oval */
            if (XOff >= x1-4 && x2+4 >= XOff) return (TRUE);
         }
      }
      w = ZOOMED_HALF_W(oval_ptr->width)+4;
      if (rx >= ry)
         return ((fabs((double)(YOff-y1))<=w) || (fabs((double)(YOff-y2))<=w));
      else
         return ((fabs((double)(XOff-x1))<=w) || (fabs((double)(XOff-x2))<=w));
   }
   else
   {
      if (fill != NONEPAT)
         if (PointInPolygon (XOff, YOff, oval_ptr->rotated_n,
               oval_ptr->rotated_vlist))
            return TRUE;
      return (PointOnPoly (XOff, YOff, oval_ptr->rotated_n,
            oval_ptr->rotated_vlist, ZOOMED_HALF_W(oval_ptr->width)));
   }
}

int FindGoodPoly (XOff, YOff, PolyObj)
   int			XOff, YOff;
   struct ObjRec	* PolyObj;
   /* XOff and YOff are screen offsets */
{
   int			sn=0, dx, dy;
   double		len, sin, cos, aw, ah;
   XPoint		tmp_v[4], *sv=NULL;
   struct PolyRec	* poly_ptr = PolyObj->detail.p;

   if (colorLayers && PolyObj->tmp_parent == NULL &&
         !ObjInVisibleLayer(PolyObj)) {
      return FALSE;
   }
   if (poly_ptr->fill != NONEPAT) {
      if (PolyObj->ctm == NULL) {
         sn = poly_ptr->sn;
         sv = poly_ptr->svlist;
         sv[sn].x = sv[0].x; sv[sn].y = sv[0].y;
         if (PointInPolygon(XOff, YOff, sn+1, sv)) return (TRUE);
      } else {
         sn = poly_ptr->rotated_n;
         sv = poly_ptr->rotated_vlist;
         sv[sn].x = sv[0].x; sv[sn].y = sv[0].y;
         if (PointInPolygon(XOff, YOff, sn+1, sv)) return (TRUE);
      }
   }
   if (poly_ptr->style == LS_PLAIN) {
      if (PolyObj->ctm == NULL) {
         if (PointOnPoly(XOff, YOff, poly_ptr->sn, poly_ptr->svlist,
               ZOOMED_HALF_W(poly_ptr->width)))
            return (TRUE);
      } else {
         if (PointOnPoly(XOff, YOff, poly_ptr->rotated_n,
               poly_ptr->rotated_vlist, ZOOMED_HALF_W(poly_ptr->width)))
            return (TRUE);
      }
   } else {
      if (PolyObj->ctm == NULL) {
         if (PointOnPoly(XOff, YOff, poly_ptr->asn, poly_ptr->asvlist,
               ZOOMED_HALF_W(poly_ptr->width)))
            return (TRUE);
      } else {
         if (PointOnPoly(XOff, YOff, poly_ptr->rotated_n,
               poly_ptr->rotated_vlist, ZOOMED_HALF_W(poly_ptr->width)))
            return (TRUE);
      }
   }
   if (PolyObj->ctm == NULL)
   {
      sv = poly_ptr->svlist;
      sn = poly_ptr->sn;
   }
   else
   {
      sv = poly_ptr->rotated_vlist;
      sn = poly_ptr->rotated_n;
   }
   aw = poly_ptr->aw;
   ah = poly_ptr->ah;

   dx = (int)(sv[1].x - sv[0].x);
   dy = (int)(sv[1].y - sv[0].y);

   if ((poly_ptr->style & LS_LEFT) && (dx != 0 || dy != 0))
   {
      len = (double)sqrt((double)(((double)dx)*((double)dx) +
            ((double)dy)*((double)dy)));
      sin = ((double)dy) / len;
      cos = ((double)dx) / len;

      tmp_v[0].x = tmp_v[3].x = OFFSET_X(sv[0].x);
      tmp_v[0].y = tmp_v[3].y = OFFSET_Y(sv[0].y);
      tmp_v[1].x = OFFSET_X(round(sv[0].x+aw*cos-ah*sin));
      tmp_v[1].y = OFFSET_Y(round(sv[0].y+aw*sin+ah*cos));
      tmp_v[2].x = OFFSET_X(round(sv[0].x+aw*cos+ah*sin));
      tmp_v[2].y = OFFSET_Y(round(sv[0].y+aw*sin-ah*cos));

      if (PointInPolygon (XOff, YOff, 4, tmp_v)) return (TRUE);
   }
   dx = (int)(sv[sn-1].x - sv[sn-2].x);
   dy = (int)(sv[sn-1].y - sv[sn-2].y);
   if ((poly_ptr->style & LS_RIGHT) && (dx != 0 || dy != 0))
   {
      len = (double)sqrt((double)(((double)dx)*((double)dx) +
            ((double)dy)*((double)dy)));
      sin = ((double)dy) / len;
      cos = ((double)dx) / len;

      tmp_v[0].x = tmp_v[3].x = OFFSET_X(sv[sn-1].x);
      tmp_v[0].y = tmp_v[3].y = OFFSET_Y(sv[sn-1].y);
      tmp_v[1].x = OFFSET_X(round(sv[sn-1].x-aw*cos+ah*sin));
      tmp_v[1].y = OFFSET_Y(round(sv[sn-1].y-aw*sin-ah*cos));
      tmp_v[2].x = OFFSET_X(round(sv[sn-1].x-aw*cos-ah*sin));
      tmp_v[2].y = OFFSET_Y(round(sv[sn-1].y-aw*sin+ah*cos));

      if (PointInPolygon (XOff, YOff, 4, tmp_v)) return (TRUE);
   }
   return (FALSE);
}

int FindGoodPolygon (XOff, YOff, PolygonObj)
   int			XOff, YOff;
   struct ObjRec	* PolygonObj;
   /* XOff and YOff are screen offsets */
{
   struct PolygonRec	* polygon_ptr = PolygonObj->detail.g;

   if (colorLayers && PolygonObj->tmp_parent == NULL &&
         !ObjInVisibleLayer(PolygonObj)) {
      return FALSE;
   }
   if (PolygonObj->ctm == NULL)
   {
      if (polygon_ptr->fill != NONEPAT)
         if (PointInPolygon (XOff, YOff, polygon_ptr->sn, polygon_ptr->svlist))
            return (TRUE);

      return (PointOnPoly (XOff, YOff, polygon_ptr->sn, polygon_ptr->svlist,
            ZOOMED_HALF_W(polygon_ptr->width)));
   }
   else
   {
      if (polygon_ptr->fill != NONEPAT)
         if (PointInPolygon (XOff, YOff, polygon_ptr->rotated_n,
               polygon_ptr->rotated_vlist))
            return (TRUE);

      return (PointOnPoly (XOff, YOff, polygon_ptr->rotated_n,
            polygon_ptr->rotated_vlist, ZOOMED_HALF_W(polygon_ptr->width)));
   }
}

static
int PointInFlatPie (Y,dir,angle,ov_int_y1,ov_int_y2,rad_int_y1,rad_int_y2)
   int		Y, dir, angle;
   double	ov_int_y1, ov_int_y2, rad_int_y1, rad_int_y2;
{
   switch (dir)
   {
      case ARC_CCW:
         switch ((angle+360)%360)
         {
            case 0:
               if (ov_int_y2 > rad_int_y1)
               {
                  if (Y>=rad_int_y2 && rad_int_y1>=Y) return (TRUE);
               }
               else if (ov_int_y2 > rad_int_y2)
               {
                  if (Y>=rad_int_y2 && ov_int_y2>=Y) return (TRUE);
               }
               break;
            case 90:
               if (ov_int_y1 < rad_int_y2)
               {
                  if (Y>=rad_int_y2 && rad_int_y1>=Y) return (TRUE);
               }
               else if (ov_int_y1 < rad_int_y1)
               {
                  if (Y>=ov_int_y1 && rad_int_y1>=Y) return (TRUE);
               }
               break;
            case 180:
               if (ov_int_y1 < rad_int_y1)
               {
                  if (Y>=rad_int_y1 && rad_int_y2>=Y) return (TRUE);
               }
               else if (ov_int_y1 < rad_int_y2)
               {
                  if (Y>=ov_int_y1 && rad_int_y2>=Y) return (TRUE);
               }
               break;
            case 270:
               if (ov_int_y2 > rad_int_y2)
               {
                  if (Y>=rad_int_y1 && rad_int_y2>=Y) return (TRUE);
               }
               else if (ov_int_y2 > rad_int_y1)
               {
                  if (Y>=rad_int_y1 && ov_int_y2>=Y) return (TRUE);
               }
               break;
         }
         break;
      case ARC_CW:
         switch ((angle+360)%360)
         {
            case 0:
               if (ov_int_y1 < rad_int_y1)
               {
                  if (Y>=rad_int_y1 && rad_int_y2>=Y) return (TRUE);
               }
               else if (ov_int_y1 < rad_int_y2)
               {
                  if (Y>=ov_int_y1 && rad_int_y2>=Y) return (TRUE);
               }
               break;
            case 90:
               if (ov_int_y1 < rad_int_y2)
               {
                  if (Y>=rad_int_y2 && rad_int_y1>=Y) return (TRUE);
               }
               else if (ov_int_y1 < rad_int_y1)
               {
                  if (Y>=ov_int_y1 && rad_int_y1>=Y) return (TRUE);
               }
               break;
            case 180:
               if (ov_int_y2 > rad_int_y1)
               {
                  if (Y>=rad_int_y2 && rad_int_y1>=Y) return (TRUE);
               }
               else if (ov_int_y2 > rad_int_y2)
               {
                  if (Y>=rad_int_y2 && ov_int_y2>=Y) return (TRUE);
               }
               break;
            case 270:
               if (ov_int_y2 > rad_int_y2)
               {
                  if (Y>=rad_int_y1 && rad_int_y2>=Y) return (TRUE);
               }
               else if (ov_int_y2 > rad_int_y1)
               {
                  if (Y>=rad_int_y1 && ov_int_y2>=Y) return (TRUE);
               }
               break;
         }
         break;
   }
   return (FALSE);
}

static
int PointInTallPie (X,dir,angle,ov_int_x1,ov_int_x2,rad_int_x1,rad_int_x2)
   int		X, dir, angle;
   double	ov_int_x1, ov_int_x2, rad_int_x1, rad_int_x2;
{
   switch (dir)
   {
      case ARC_CCW:
         switch ((angle+360)%360)
         {
            case 0:
               if (ov_int_x2 > rad_int_x2)
               {
                  if (X>=rad_int_x1 && rad_int_x2>=X) return (TRUE);
               }
               else if (ov_int_x2 > rad_int_x1)
               {
                  if (X>=rad_int_x1 && ov_int_x2>=X) return (TRUE);
               }
               break;
            case 90:
               if (ov_int_x2 > rad_int_x1)
               {
                  if (X>=rad_int_x2 && rad_int_x1>=X) return (TRUE);
               }
               else if (ov_int_x2 > rad_int_x2)
               {
                  if (X>=rad_int_x2 && ov_int_x2>=X) return (TRUE);
               }
               break;
            case 180:
               if (ov_int_x1 < rad_int_x2)
               {
                  if (X>=rad_int_x2 && rad_int_x1>=X) return (TRUE);
               }
               else if (ov_int_x1 < rad_int_x1)
               {
                  if (X>=ov_int_x1 && rad_int_x1>=X) return (TRUE);
               }
               break;
            case 270:
               if (ov_int_x1 < rad_int_x1)
               {
                  if (X>=rad_int_x1 && rad_int_x2>=X) return (TRUE);
               }
               else if (ov_int_x1 < rad_int_x2)
               {
                  if (X>=ov_int_x1 && rad_int_x2>=X) return (TRUE);
               }
               break;
         }
         break;
      case ARC_CW:
         switch ((angle+360)%360)
         {
            case 0:
               if (ov_int_x2 > rad_int_x2)
               {
                  if (X>=rad_int_x1 && rad_int_x2>=X) return (TRUE);
               }
               else if (ov_int_x2 > rad_int_x1)
               {
                  if (X>=rad_int_x1 && ov_int_x2>=X) return (TRUE);
               }
               break;
            case 90:
               if (ov_int_x1 < rad_int_x1)
               {
                  if (X>=rad_int_x1 && rad_int_x2>=X) return (TRUE);
               }
               else if (ov_int_x1 < rad_int_x2)
               {
                  if (X>=ov_int_x1 && rad_int_x2>=X) return (TRUE);
               }
               break;
            case 180:
               if (ov_int_x1 < rad_int_x2)
               {
                  if (X>=rad_int_x2 && rad_int_x1>=X) return (TRUE);
               }
               else if (ov_int_x1 < rad_int_x1)
               {
                  if (X>=ov_int_x1 && rad_int_x1>=X) return (TRUE);
               }
               break;
            case 270:
               if (ov_int_x2 > rad_int_x1)
               {
                  if (X>=rad_int_x2 && rad_int_x1>=X) return (TRUE);
               }
               else if (ov_int_x2 > rad_int_x2)
               {
                  if (X>=rad_int_x2 && ov_int_x2>=X) return (TRUE);
               }
               break;
         }
         break;
   }
   return (FALSE);
}

static
int XInPieRange (X, dir, angle, cx, rx)
   int		X, dir, angle;
   double	cx, rx;
{
   switch (dir)
   {
      case ARC_CCW:
         switch ((angle+360)%360)
         {
            case 0:
            case 90: return (X>=cx && cx+rx>=X);

            case 180:
            case 270: return (X>=cx-rx && cx>=X);
         }
         break;
      case ARC_CW:
         switch ((angle+360)%360)
         {
            case 0:
            case 270: return (X>=cx && cx+rx>=X);

            case 90:
            case 180: return (X>=cx-rx && cx>=X);
         }
         break;
   }
   return (FALSE);
}

static
int YInPieRange (Y, dir, angle, cy, ry)
   int		Y, dir, angle;
   double	cy, ry;
{
   switch (dir)
   {
      case ARC_CCW:
         switch ((angle+360)%360)
         {
            case 0:
            case 270: return (Y>=cy && cy+ry>=Y);

            case 90:
            case 180: return (Y>=cy-ry && cy>=Y);
         }
         break;
      case ARC_CW:
         switch ((angle+360)%360)
         {
            case 0:
            case 90: return (Y>=cy-ry && cy>=Y);

            case 180:
            case 270: return (Y>=cy && cy+ry>=Y);
         }
         break;
   }
   return (FALSE);
}

#define ARC_TOL (1.0e-5)

int FindGoodArc (XOff, YOff, ArcObj)
   int			XOff, YOff;
   struct ObjRec	* ArcObj;
   /* XOff and YOff are screen offsets */
{
   struct ArcRec	* arc_ptr = ArcObj->detail.a;
   int			w, h, ltx, lty, rbx, rby, aw, ah, dx, dy, theta=0;
   double		cx, cy, rx, ry, tmp_x, tmp_y, x = 0.0, y = 0.0;
   double		ov_int_x1 = 0.0, ov_int_x2 = 0.0;
   double		ov_int_y1 = 0.0, ov_int_y2 = 0.0;
   double		rad_int_x1 = 0.0, rad_int_x2 = 0.0;
   double		rad_int_y1 = 0.0, rad_int_y2 = 0.0;
   double		len, sine, cosine;
   int			fill = arc_ptr->fill, angle1, angle2;
   int			arc_x1, arc_y1, arc_x2, arc_y2, theta1, theta2;
   int			pass_theta1, just_pass_theta1, angle, dir, full_circle;
   XPoint		tmp_v[4];

   if (colorLayers && ArcObj->tmp_parent == NULL &&
         !ObjInVisibleLayer(ArcObj)) {
      return FALSE;
   }
   if (ArcObj->ctm != NULL) {
      if (fill != NONEPAT) {
         if (PointInPolygon(XOff, YOff, arc_ptr->rotated_n+2,
               arc_ptr->rotated_vlist)) {
            return TRUE;
         }
      }
      return (PointOnPoly(XOff, YOff, arc_ptr->rotated_n,
            arc_ptr->rotated_vlist, ZOOMED_HALF_W(arc_ptr->width)));
   }

   ltx = OFFSET_X(arc_ptr->ltx);
   lty = OFFSET_Y(arc_ptr->lty);
   rbx = OFFSET_X(arc_ptr->ltx+arc_ptr->w);
   rby = OFFSET_Y(arc_ptr->lty+arc_ptr->h);

   if (ltx==rbx && lty==rby) return (FALSE);

   rx = (rbx-ltx)/2; ry = (rby-lty)/2;
   cx = (double)(OFFSET_X(arc_ptr->xc)); cy = (double)(OFFSET_Y(arc_ptr->yc));
   arc_x1 = OFFSET_X(arc_ptr->x1); arc_y1 = OFFSET_Y(arc_ptr->y1);
   dir = arc_ptr->dir;

   theta1 = (int)(arc_ptr->angle1)/64;
   theta2 = theta1 + (int)(arc_ptr->angle2)/64;

   ArcRealX2Y2 (arc_ptr, &arc_x2, &arc_y2);
   arc_x2 = OFFSET_X(arc_x2); arc_y2 = OFFSET_Y(arc_y2);

   if (theta2 < -180) theta2 += 360;
   if (theta2 > 180) theta2 -= 360;

   if (theta1 < 0) theta1 += 360;
   if (theta2 <= 0) theta2 += 360;

   angle1 = arc_ptr->angle1;
   angle2 = arc_ptr->angle2;

   full_circle = (abs(angle2) == 64*360);

   if (rx >= ry)
   {  /* flat oval */
      tmp_y = (double)sqrt(fabs((double)(ry*ry*(1 -
            (((double)XOff)-cx)*(((double)XOff)-cx)/rx/rx))));
      ov_int_y1 = cy - tmp_y;
      ov_int_y2 = cy + tmp_y;
   }
   else
   {  /* tall oval */
      tmp_x = (double)sqrt(fabs((double)(rx*rx*(1 -
            (((double)YOff)-cy)*(((double)YOff)-cy)/ry/ry))));
      ov_int_x1 = cx - tmp_x;
      ov_int_x2 = cx + tmp_x;
   }

   w = ZOOMED_HALF_W(arc_ptr->width)+4;
   if (rx >= ry)
   {  /* flat oval */
      if (fabs (arc_x1-cx) < ARC_TOL)
      {
         switch (theta1)
         {
            case 90: rad_int_y1 = cy - 1/ARC_TOL; break;
            case 270: rad_int_y1 = cy + 1/ARC_TOL; break;
            default: fprintf (stderr, "theta1 = %1d (flat)\n", theta1); break;
         }
      }
      else
         rad_int_y1 = cy + (XOff-cx)*(arc_y1-cy)/(arc_x1-cx);

      if (fabs (arc_x2-cx) < ARC_TOL)
      {
         switch (theta2)
         {
            case 90: rad_int_y2 = cy - 1/ARC_TOL; break;
            case 270: rad_int_y2 = cy + 1/ARC_TOL; break;
            default: fprintf (stderr, "theta2 = %1d (flat)\n", theta2); break;
         }
      }
      else
         rad_int_y2 = cy + (XOff-cx)*(arc_y2-cy)/(arc_x2-cx);
   }
   else
   {  /* tall oval */
      if (fabs (arc_y1-cy) < ARC_TOL)
      {
         switch (theta1)
         {
            case 0:
            case 360: rad_int_x1 = cx + 1/ARC_TOL; break;
            case 180: rad_int_x1 = cx - 1/ARC_TOL; break;
            default: fprintf (stderr, "theta1 = %1d (tall)\n", theta1); break;
         }
      }
      else
         rad_int_x1 = cx + (YOff-cy)*(arc_x1-cx)/(arc_y1-cy);

      if (fabs (arc_y2-cy) < ARC_TOL)
      {
         switch (theta2)
         {
            case 0:
            case 360: rad_int_x2 = cx + 1/ARC_TOL; break;
            case 180: rad_int_x2 = cx - 1/ARC_TOL; break;
            default: fprintf (stderr, "theta2 = %1d (tall)\n", theta2); break;
         }
      }
      else
         rad_int_x2 = cx + (YOff-cy)*(arc_x2-cx)/(arc_y2-cy);
   }
   if (dir == ARC_CCW)
   {
      angle = 0;
      pass_theta1 = FALSE;
      just_pass_theta1 = FALSE;
      while (angle < theta2 || !pass_theta1)
      {
         if (angle >= theta1 && !pass_theta1)
         {
            pass_theta1 = TRUE;
            just_pass_theta1 = TRUE;
            if (theta2 > theta1 && angle >= theta2 && !full_circle)
            {  /* theta1 and theta2 are in the same quadrant */
               if (fill != NONEPAT)
               {
                  if (rx >= ry)
                  {  /* flat oval */
                     if (PointInFlatPie (YOff, dir, angle, ov_int_y1,
                           ov_int_y2, rad_int_y1, rad_int_y2))
                        return (TRUE);
                  }
                  else
                  {  /* tall oval */
                     if (PointInTallPie (XOff, dir, angle, ov_int_x1,
                           ov_int_x2, rad_int_x1, rad_int_x2))
                        return (TRUE);
                  }
               }
               if (rx >= ry)
               {  /* flat oval */
                  switch ((angle+360)%360)
                  {
                     case 0:
                     case 270:
                        return (XOff>=arc_x1 && arc_x2>=XOff &&
                              (fabs ((double)(YOff-ov_int_y2)) <= w));
                     case 90:
                     case 180:
                        return (XOff>=arc_x2 && arc_x1>=XOff &&
                              (fabs ((double)(YOff-ov_int_y1)) <= w));
                  }
               }
               else
               {  /* tall oval */
                  switch ((angle+360)%360)
                  {
                     case 0:
                     case 90:
                        return (YOff>=arc_y2 && arc_y1>=YOff &&
                              (fabs ((double)(XOff-ov_int_x2)) <= w));
                     case 180:
                     case 270:
                        return (YOff>=arc_y1 && arc_y2>=YOff &&
                              (fabs ((double)(XOff-ov_int_x1)) <= w));
                  }
               }
            }
            if (theta2 <= theta1) angle -= 360;
            if (angle > theta2) angle -= 360;
         }
         if (just_pass_theta1)
         {
            just_pass_theta1 = FALSE;
            if (rx >= ry)
            {  /* flat oval */
               switch ((angle+360)%360)
               {
                  case 0: y = cy; break;
                  case 90: y = cy-ry; break;
                  case 180: y = cy; break;
                  case 270: y = cy+ry; break;
               }
               if (fill != NONEPAT)
               {
                  if (XInPieRange (XOff, dir, angle, cx, rx) &&
                        PointInFlatPie (YOff, dir, angle, ov_int_y1,
                        ov_int_y2, rad_int_y1, y))
                     return (TRUE);
               }
               switch ((angle+360)%360)
               {
                  case 0:
                     if (XOff>=arc_x1 && cx+rx>=XOff &&
                           (fabs ((double)(YOff-ov_int_y2)) <= w))
                        return (TRUE);
                     break;
                  case 90:
                     if (XOff>=cx && arc_x1>=XOff &&
                           (fabs ((double)(YOff-ov_int_y1)) <= w))
                        return (TRUE);
                     break;
                  case 180:
                     if (XOff>=cx-rx && arc_x1>=XOff &&
                           (fabs ((double)(YOff-ov_int_y1)) <= w))
                        return (TRUE);
                     break;
                  case 270:
                     if (XOff>=arc_x1 && cx>=XOff &&
                           (fabs ((double)(YOff-ov_int_y2)) <= w))
                        return (TRUE);
                     break;
               }
            }
            else
            {  /* tall oval */
               switch ((angle+360)%360)
               {
                  case 0: x = cx+rx; break;
                  case 90: x = cx; break;
                  case 180: x = cx-rx; break;
                  case 270: x = cx; break;
               }
               if (fill != NONEPAT)
               {
                  if (YInPieRange (YOff, dir, angle, cy, ry) &&
                        PointInTallPie (XOff, dir, angle, ov_int_x1,
                        ov_int_x2, rad_int_x1, x))
                     return (TRUE);
               }
               switch ((angle+360)%360)
               {
                  case 0:
                     if (YOff>=cy && arc_y1>=YOff &&
                           (fabs ((double)(XOff-ov_int_x2)) <= w))
                        return (TRUE);
                     break;
                  case 90:
                     if (YOff>=cy-ry && arc_y1>=YOff &&
                           (fabs ((double)(XOff-ov_int_x2)) <= w))
                        return (TRUE);
                     break;
                  case 180:
                     if (YOff>=arc_y1 && cy>=YOff &&
                           (fabs ((double)(XOff-ov_int_x1)) <= w))
                        return (TRUE);
                     break;
                  case 270:
                     if (YOff>=arc_y1 && cy+ry>=YOff &&
                           (fabs ((double)(XOff-ov_int_x1)) <= w))
                        return (TRUE);
                     break;
               }
            }
         }
         else if (pass_theta1)
         {  /* see if point is in the quadrant */
            if (rx >= ry)
            {  /* flat oval */
               if (XInPieRange (XOff, dir, angle, cx, rx))
               {
                  if (fill != NONEPAT)
                  {
                     switch ((angle+360)%360)
                     {
                        case 90:
                        case 180:
                           if (YOff>=ov_int_y1 && cy>=YOff) return (TRUE);
                           break;

                        case 0:
                        case 270:
                           if (YOff>=cy && ov_int_y2>=YOff) return (TRUE);
                           break;
                     }
                  }
                  switch ((angle+360)%360)
                  {
                     case 0:
                     case 270:
                        if (fabs ((double)(YOff-ov_int_y2)) <= w) return (TRUE);
                        break;
                     case 90:
                     case 180:
                        if (fabs ((double)(YOff-ov_int_y1)) <= w) return (TRUE);
                        break;
                  }
               }
            }
            else
            {  /* tall oval */
               if (YInPieRange (YOff, dir, angle, cy, ry))
               {
                  if (fill != NONEPAT)
                  {
                     switch ((angle+360)%360)
                     {
                        case 0:
                        case 90:
                           if (XOff>=cx && ov_int_x2>=XOff) return (TRUE);
                           break;

                        case 180:
                        case 270:
                           if (XOff>=ov_int_x1 && cx>=XOff) return (TRUE);
                           break;
                     }
                  }
                  switch ((angle+360)%360)
                  {
                     case 0:
                     case 90:
                        if (fabs ((double)(XOff-ov_int_x2)) <= w) return (TRUE);
                        break;

                     case 180:
                     case 270:
                        if (fabs ((double)(XOff-ov_int_x1)) <= w) return (TRUE);
                        break;
                  }
               }
            }
         }
         angle = (angle == 360) ? 0 : (angle+90);
      }
      if (rx >= ry)
      {  /* flat oval */
         switch ((angle+360)%360)
         {
            case 0: y = cy+ry; break;
            case 180: y = cy-ry; break;

            case 90:
            case 270: y = cy; break;
         }
         if (fill != NONEPAT)
         {
            if (XInPieRange (XOff, dir, angle, cx, rx) &&
                  PointInFlatPie (YOff, dir, angle, ov_int_y1, ov_int_y2,
                  y, rad_int_y2))
               return (TRUE);
         }
         switch ((angle+360)%360)
         {
            case 0:
               if (XOff>=cx && arc_x2>=XOff &&
                     (fabs ((double)(YOff-ov_int_y2)) <= w))
                  return (TRUE);
               break;
            case 90:
               if (XOff>=arc_x2 && cx+rx>=XOff &&
                     (fabs ((double)(YOff-ov_int_y1)) <= w))
                  return (TRUE);
               break;
            case 180:
               if (XOff>=arc_x2 && cx>=XOff &&
                     (fabs ((double)(YOff-ov_int_y1)) <= w))
                  return (TRUE);
               break;
            case 270:
               if (XOff>=cx-rx && arc_x2>=XOff &&
                     (fabs ((double)(YOff-ov_int_y2)) <= w))
                  return (TRUE);
               break;
         }
      }
      else
      {  /* tall oval */
         switch ((angle+360)%360)
         {
            case 0:
            case 180: x = cx; break;

            case 90: x = cx+rx; break;
            case 270: x = cx-rx; break;
         }
         if (fill != NONEPAT)
         {
            if (YInPieRange (YOff, dir, angle, cy, ry) &&
                  PointInTallPie (XOff, dir, angle, ov_int_x1, ov_int_x2,
                  x, rad_int_x2))
               return (TRUE);
         }
         switch ((angle+360)%360)
         {
            case 0:
               if (YOff>=arc_y2 && cy+ry>=YOff &&
                     (fabs ((double)(XOff-ov_int_x2)) <= w))
                  return (TRUE);
               break;
            case 90:
               if (YOff>=arc_y2 && cy>=YOff &&
                     (fabs ((double)(XOff-ov_int_x2)) <= w))
                  return (TRUE);
               break;
            case 180:
               if (YOff>=cy-ry && arc_y2>=YOff &&
                     (fabs ((double)(XOff-ov_int_x1)) <= w))
                  return (TRUE);
               break;
            case 270:
               if (YOff>=cy && arc_y2>=YOff &&
                     (fabs ((double)(XOff-ov_int_x1)) <= w))
                  return (TRUE);
               break;
         }
      }
   }
   else
   {
      angle = 360;
      pass_theta1 = FALSE;
      just_pass_theta1 = FALSE;
      while (angle > theta2 || !pass_theta1)
      {
         if (angle <= theta1 && !pass_theta1)
         {
            pass_theta1 = TRUE;
            just_pass_theta1 = TRUE;
            if (theta2 < theta1 && angle <= theta2 && !full_circle)
            {  /* theta1 and theta2 are in the same quadrant */
               if (fill != NONEPAT)
               {
                  if (rx >= ry)
                  {
                     if (PointInFlatPie (YOff, dir, angle, ov_int_y1,
                           ov_int_y2, rad_int_y1, rad_int_y2))
                        return (TRUE);
                  }
                  else
                  {
                     if (PointInTallPie (XOff, dir, angle, ov_int_x1,
                           ov_int_x2, rad_int_x1, rad_int_x2))
                        return (TRUE);
                  }
               }
               if (rx >= ry)
               {  /* flat oval */
                  switch ((angle+360)%360)
                  {
                     case 0:
                     case 90:
                        return (XOff>=arc_x1 && arc_x2>=XOff &&
                              (fabs ((double)(YOff-ov_int_y1)) <= w));
                     case 180:
                     case 270:
                        return (XOff>=arc_x2 && arc_x1>=XOff &&
                              (fabs ((double)(YOff-ov_int_y2)) <= w));
                  }
               }
               else
               {  /* tall oval */
                  switch ((angle+360)%360)
                  {
                     case 0:
                     case 270:
                        return (YOff>=arc_y1 && arc_y2>=YOff &&
                              (fabs ((double)(XOff-ov_int_x2)) <= w));
                     case 90:
                     case 180:
                        return (YOff>=arc_y2 && arc_y1>=YOff &&
                              (fabs ((double)(XOff-ov_int_x1)) <= w));
                  }
               }
            }
            if (theta2 >= theta1) angle += 360;
            if (angle <= theta2) angle += 360;
         }
         if (just_pass_theta1)
         {
            just_pass_theta1 = FALSE;
            if (rx >= ry)
            {  /* flat oval */
               switch ((angle+360)%360)
               {
                  case 0: y = cy; break;
                  case 90: y = cy-ry; break;
                  case 180: y = cy; break;
                  case 270: y = cy+ry; break;
               }
               if (fill != NONEPAT)
               {
                  if (XInPieRange (XOff, dir, angle, cx, rx) &&
                        PointInFlatPie (YOff, dir, angle, ov_int_y1,
                        ov_int_y2, rad_int_y1, y))
                     return (TRUE);
               }
               switch ((angle+360)%360)
               {
                  case 0:
                     if (XOff>=arc_x1 && cx+rx>=XOff &&
                           (fabs ((double)(YOff-ov_int_y1)) <= w))
                        return (TRUE);
                     break;
                  case 90:
                     if (XOff>=arc_x1 && cx>=XOff &&
                           (fabs ((double)(YOff-ov_int_y1)) <= w))
                        return (TRUE);
                     break;
                  case 180:
                     if (XOff>=cx-rx && arc_x1>=XOff &&
                           (fabs ((double)(YOff-ov_int_y2)) <= w))
                        return (TRUE);
                     break;
                  case 270:
                     if (XOff>=cx && arc_x1>=XOff &&
                           (fabs ((double)(YOff-ov_int_y2)) <= w))
                        return (TRUE);
                     break;
               }
            }
            else
            {  /* tall oval */
               switch ((angle+360)%360)
               {
                  case 0: x = cx+rx; break;
                  case 90: x = cx; break;
                  case 180: x = cx-rx; break;
                  case 270: x = cx; break;
               }
               if (fill != NONEPAT)
               {
                  if (YInPieRange (YOff, dir, angle, cy, ry) &&
                        PointInTallPie (XOff, dir, angle, ov_int_x1,
                        ov_int_x2, rad_int_x1, x))
                     return (TRUE);
               }
               switch ((angle+360)%360)
               {
                  case 0:
                     if (YOff>=arc_y1 && cy>=YOff &&
                           (fabs ((double)(XOff-ov_int_x2)) <= w))
                        return (TRUE);
                     break;
                  case 90:
                     if (YOff>=cy-ry && arc_y1>=YOff &&
                           (fabs ((double)(XOff-ov_int_x1)) <= w))
                        return (TRUE);
                     break;
                  case 180:
                     if (YOff>=cy && arc_y1>=YOff &&
                           (fabs ((double)(XOff-ov_int_x1)) <= w))
                        return (TRUE);
                     break;
                  case 270:
                     if (YOff>=arc_y1 && cy+ry>=YOff &&
                           (fabs ((double)(XOff-ov_int_x2)) <= w))
                        return (TRUE);
                     break;
               }
            }
         }
         else if (pass_theta1)
         {  /* see if point is in the quadrant */
            if (rx >= ry)
            {  /* flat oval */
               if (XInPieRange (XOff, dir, angle, cx, rx))
               {
                  if (fill != NONEPAT)
                  {
                     switch ((angle+360)%360)
                     {
                        case 0:
                        case 90:
                           if (YOff>=ov_int_y1 && cy>=YOff) return (TRUE);
                           break;
      
                        case 180:
                        case 270:
                           if (YOff>=cy && ov_int_y2>=YOff) return (TRUE);
                           break;
                     }
                  }
                  switch ((angle+360)%360)
                  {
                     case 0:
                     case 90:
                        if (fabs ((double)(YOff-ov_int_y1)) <= w) return (TRUE);
                        break;
                     case 180:
                     case 270:
                        if (fabs ((double)(YOff-ov_int_y2)) <= w) return (TRUE);
                        break;
                  }
               }
            }
            else
            {  /* tall oval */
               if (YInPieRange (YOff, dir, angle, cy, ry))
               {
                  if (fill != NONEPAT)
                  {
                     switch ((angle+360)%360)
                     {
                        case 0:
                        case 270:
                           if (XOff>=cx && ov_int_x2>=XOff) return (TRUE);
                           break;
                        case 90:
                        case 180: 
                           if (XOff>=ov_int_x1 && cx>=XOff) return (TRUE);
                           break;
                     }
                  }
                  switch ((angle+360)%360)
                  {
                     case 0:
                     case 270:
                        if (fabs ((double)(XOff-ov_int_x2)) <= w) return (TRUE);
                        break;
                     case 90:
                     case 180:
                        if (fabs ((double)(XOff-ov_int_x1)) <= w) return (TRUE);
                        break;
                  }
               }
            }
         }
         angle = (angle == 0) ? 360 : (angle-90);
      }
      if (rx >= ry)
      {  /* flat oval */
         switch ((angle+360)%360)
         {
            case 0: y = cy-ry; break;
            case 180: y = cy+ry; break;

            case 90:
            case 270: y = cy; break;
         }
         if (fill != NONEPAT)
         {
            if (XInPieRange (XOff, dir, angle, cx, rx) &&
                  PointInFlatPie (YOff, dir, angle, ov_int_y1, ov_int_y2,
                  y, rad_int_y2))
               return (TRUE);
         }
         switch ((angle+360)%360)
         {
            case 0:
               if (XOff>=cx && arc_x2>=XOff &&
                     (fabs ((double)(YOff-ov_int_y1)) <= w))
                  return (TRUE);
               break;
            case 90:
               if (XOff>=cx-rx && arc_x2>=XOff &&
                     (fabs ((double)(YOff-ov_int_y1)) <= w))
                  return (TRUE);
               break;
            case 180:
               if (XOff>=arc_x2 && cx>=XOff &&
                     (fabs ((double)(YOff-ov_int_y2)) <= w))
                  return (TRUE);
               break;
            case 270:
               if (XOff>=arc_x2 && cx+rx>=XOff &&
                     (fabs ((double)(YOff-ov_int_y2)) <= w))
                  return (TRUE);
               break;
         }
      }
      else
      {  /* tall oval */
         switch ((angle+360)%360)
         {
            case 0:
            case 180: x = cx; break;

            case 90: x = cx-rx; break;
            case 270: x = cx+rx; break;
         }
         if (fill != NONEPAT)
         {
            if (YInPieRange (YOff, dir, angle, cy, ry) &&
                  PointInTallPie (XOff, dir, angle, ov_int_x1, ov_int_x2,
                  x, rad_int_x2))
               return (TRUE);
         }
         switch ((angle+360)%360)
         {
            case 0:
               if (YOff>=cy-ry && arc_y2>=YOff &&
                     (fabs ((double)(XOff-ov_int_x2)) <= w))
                  return (TRUE);
               break;
            case 90:
               if (YOff>=arc_y2 && cy>=YOff &&
                     (fabs ((double)(XOff-ov_int_x1)) <= w))
                  return (TRUE);
               break;
            case 180:
               if (YOff>=arc_y2 && cy+ry>=YOff &&
                     (fabs ((double)(XOff-ov_int_x1)) <= w))
                  return (TRUE);
               break;
            case 270:
               if (YOff>=cy && arc_y2>=YOff &&
                     (fabs ((double)(XOff-ov_int_x2)) <= w))
                  return (TRUE);
               break;
         }
      }
   }

   w = ZOOMED_SIZE(arc_ptr->w);
   h = ZOOMED_SIZE(arc_ptr->h);
   aw = ZOOMED_SIZE(arc_ptr->aw); if (aw == 0) aw = 1;
   ah = ZOOMED_SIZE(arc_ptr->ah); if (ah == 0) ah = 1;

   if ((arc_ptr->style & LS_LEFT) && (angle2%(360*64) != 0))
   {  /* the arrow should appear at angle1 */
      switch (dir)
      {
         case ARC_CCW: theta = (int)(angle1/64)-90; break;
         case ARC_CW: theta = (int)(angle1/64)+90; break;
      }
      dx = -round(w*cos(((double)theta)*M_PI/180.0));
      dy = round(h*sin(((double)theta)*M_PI/180.0));
      if (dx == 0 && dy == 0)
      {
         sine = cosine = ((double)0.0);

         tmp_v[0].x = tmp_v[1].x = tmp_v[2].x = tmp_v[3].x = arc_x1;
         tmp_v[0].y = tmp_v[1].y = tmp_v[2].y = tmp_v[3].y = arc_y1;
      }
      else
      {
         len = (double)sqrt((double)(((double)dx)*((double)dx) +
               ((double)dy)*((double)dy)));
         sine = dy/len;
         cosine = dx/len;

         tmp_v[0].x = tmp_v[3].x = arc_x1;
         tmp_v[0].y = tmp_v[3].y = arc_y1;
         tmp_v[1].x = round(arc_x1 + aw*cosine - ah*sine);
         tmp_v[1].y = round(arc_y1 + aw*sine + ah*cosine);
         tmp_v[2].x = round(arc_x1 + aw*cosine + ah*sine);
         tmp_v[2].y = round(arc_y1 + aw*sine - ah*cosine);
      }

      if (PointInPolygon (XOff, YOff, 4, tmp_v)) return (TRUE);
   }
   if ((arc_ptr->style & LS_RIGHT) && (angle2%(360*64) != 0))
   {
      switch (dir)
      {
         case ARC_CCW: theta = (int)((angle1+angle2)/64)-90; break;
         case ARC_CW: theta = (int)((angle1+angle2)/64)+90; break;
      }
      dx = -round(w*cos(((double)theta)*M_PI/180.0));
      dy = round(h*sin(((double)theta)*M_PI/180.0));
      if (dx == 0 && dy == 0)
      {
         sine = cosine = ((double)0.0);

         tmp_v[0].x = tmp_v[1].x = tmp_v[2].x = tmp_v[3].x = arc_x2;
         tmp_v[0].y = tmp_v[1].y = tmp_v[2].y = tmp_v[3].y = arc_y2;
      }
      else
      {
         len = (double)sqrt((double)(((double)dx)*((double)dx) +
               ((double)dy)*((double)dy)));
         sine = dy/len;
         cosine = dx/len;

         tmp_v[0].x = tmp_v[3].x = arc_x2;
         tmp_v[0].y = tmp_v[3].y = arc_y2;
         tmp_v[1].x = round(arc_x2 - aw*cosine + ah*sine);
         tmp_v[1].y = round(arc_y2 - aw*sine - ah*cosine);
         tmp_v[2].x = round(arc_x2 - aw*cosine - ah*sine);
         tmp_v[2].y = round(arc_y2 - aw*sine + ah*cosine);
      }

      if (PointInPolygon (XOff, YOff, 4, tmp_v)) return (TRUE);
   }
   return (FALSE);
}

int FindGoodObj (XOff, YOff, FirstObj, SubObj, ImmediateChildObj)
   int			XOff, YOff;
   struct ObjRec	* FirstObj, * * SubObj, * * ImmediateChildObj;
   /* XOff and YOff are screen offsets */
{
   register struct ObjRec	* obj_ptr;
   register struct AttrRec	* attr_ptr;

   *SubObj = NULL;

   for (obj_ptr = FirstObj; obj_ptr != NULL; obj_ptr = obj_ptr->next)
   {
      obj_ptr->tmp_child = NULL;
      obj_ptr->tmp_parent = NULL;
      if (ImmediateChildObj != NULL) *ImmediateChildObj = obj_ptr;
      if (colorLayers && !ObjInVisibleLayer(obj_ptr)) {
         continue;
      }
      for (attr_ptr=obj_ptr->fattr; attr_ptr!=NULL; attr_ptr=attr_ptr->next)
         if (attr_ptr->shown &&
               XOff >= OFFSET_X(attr_ptr->obj->bbox.ltx)-3 &&
               YOff >= OFFSET_Y(attr_ptr->obj->bbox.lty)-3 &&
               XOff <= OFFSET_X(attr_ptr->obj->bbox.rbx)+3 &&
               YOff <= OFFSET_Y(attr_ptr->obj->bbox.rby)+3)
         {
            *SubObj = attr_ptr->obj;
            return (TRUE);
         }

      if (XOff >= OFFSET_X(obj_ptr->bbox.ltx)-3 &&
            YOff >= OFFSET_Y(obj_ptr->bbox.lty)-3 &&
            XOff <= OFFSET_X(obj_ptr->bbox.rbx)+3 &&
            YOff <= OFFSET_Y(obj_ptr->bbox.rby)+3)
      {
         struct ObjRec	* next_level_child=NULL;

         switch (obj_ptr->type)
         {
            case OBJ_TEXT:
               if (FindGoodText (XOff, YOff, obj_ptr)) return (TRUE);
               break;
            case OBJ_XBM:
               if (FindGoodXBm (XOff, YOff, obj_ptr)) return (TRUE);
               break;
            case OBJ_XPM:
               if (FindGoodXPm (XOff, YOff, obj_ptr)) return (TRUE);
               break;
            case OBJ_BOX:
               if (FindGoodBox (XOff, YOff, obj_ptr)) return (TRUE);
               break;
            case OBJ_RCBOX:
               if (FindGoodRCBox (XOff, YOff, obj_ptr)) return (TRUE);
               break;
            case OBJ_OVAL:
               if (FindGoodOval (XOff, YOff, obj_ptr)) return (TRUE);
               break;
            case OBJ_POLY:
               if (FindGoodPoly (XOff, YOff, obj_ptr)) return (TRUE);
               break;
            case OBJ_POLYGON:
               if (FindGoodPolygon (XOff, YOff, obj_ptr)) return (TRUE);
               break;
            case OBJ_ARC:
               if (FindGoodArc (XOff, YOff, obj_ptr)) return (TRUE);
               break;

            case OBJ_GROUP:
            case OBJ_SYM:
            case OBJ_ICON:
               if (colorLayers) {
                  struct ObjRec *tmp_obj;

                  for (tmp_obj=obj_ptr->detail.r->first; tmp_obj != NULL;
                        tmp_obj=tmp_obj->next) {
                     tmp_obj->tmp_parent = obj_ptr;
                  }
               }
               if (FindGoodObj (XOff, YOff, obj_ptr->detail.r->first, SubObj,
                     &next_level_child))
               {
                  obj_ptr->tmp_child = next_level_child;
                  return (TRUE);
               }
               break;
         }
      }
   }
   if (ImmediateChildObj != NULL) *ImmediateChildObj = NULL;
   return (FALSE);
}
