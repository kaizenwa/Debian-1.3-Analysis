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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/arc.c,v 3.0 1996/05/06 16:03:39 william Exp $";
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

#ifndef _NO_EXTERN
#include "arc.e"
#endif
#include "attr.e"
#include "auxtext.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "dup.e"
#include "file.e"
#include "grid.e"
#include "mainloop.e"
#include "mark.e"
#include "msg.e"
#include "obj.e"
#include "pattern.e"
#include "poly.e"
#include "ps.e"
#include "raster.e"
#include "ruler.e"
#include "select.e"
#include "setup.e"
#include "special.e"
#include "spline.e"
#include "util.e"
#include "xpixmap.e"

extern int	atoi ARGS_DECL((char *));

#define EXPAND_BBOX(bbox,x,y) \
   if ((x)<(bbox)->ltx) (bbox)->ltx=(x); if ((y)<(bbox)->lty) (bbox)->lty=(y); \
   if ((x)>(bbox)->rbx) (bbox)->rbx=(x); if ((y)>(bbox)->rby) (bbox)->rby=(y)

int	arcDrawn=FALSE;

/*
 * 0 degree is horizontal in the direction of the X axis.
 * Positive angles measures counter-clockwise from 0 degree.
 * Negative angles measures clockwise from 0 degree.
 * Angle1 means the start angle.
 * Angle2 means the amount between angle1 and the real angle2.
 */

static
int ArcDirection(xc, yc, x1, y1, x2, y2)
   int xc, yc, x1, y1, x2, y2;
{
   register int dx, dy;
   register double theta1, theta2;

   dx = x1-xc; dy = y1-yc;
   theta1 = (dx==0) ? ((dy>=0) ? M_PI/2.0 : -M_PI/2.0) :
         atan2 ((double)(dy), (double)(dx));
   theta2 = (x2==xc) ? ((y2>=yc) ? M_PI/2.0 : -M_PI/2.0) :
         atan2 ((double)(y2-yc), (double)(x2-xc));
   if (theta1 < 0) theta1 += 2*M_PI;
   if (theta2 < 0) theta2 += 2*M_PI;

   if (theta2 > theta1) {
      if (theta2-theta1 >= 2*M_PI-theta2+theta1) {
         return ARC_CCW;
      } else {
         return ARC_CW;
      }
   } else if (theta1 > theta2) {
      if (theta1-theta2 >= 2*M_PI-theta1+theta2) {
         return ARC_CW;
      } else {
         return ARC_CCW;
      }
   } else {
      return ARC_CCW;
   }
}

#define ARC_TOL (1.0e-5)

void PointsToShearScale(Corner, x_pivot, y_pivot, x_move, y_move,
      x_current, y_current, x_shear, y_shear, x_scale, y_scale)
   int Corner, x_pivot, y_pivot, x_move, y_move, x_current, y_current;
   int *x_shear, *y_shear, *x_scale, *y_scale;
   /* the returned shear value is 1000*arctan() */
   /* the returned scale value is 1000*scaling */
{
   int dx=x_current-x_move, dy=y_current-y_move;

   switch (Corner) {
   case CORNER_TOP:
   case CORNER_BOTTOM:
      if (x_scale != NULL) *x_scale = 1000;
      if (y_scale != NULL) {
         *y_scale = (dy == 0 ? 1000 :
               (int)(((float)(y_current-y_pivot)) /
               ((float)(y_move-y_pivot))*1000.0));
      }
      if (x_shear != NULL) {
         *x_shear = (dx == 0 ? 0 :
               (int)(atan2((double)dx,(double)y_current-y_pivot)*1000.0));
      }
      if (y_shear != NULL) *y_shear = 0;
      break;
   case CORNER_RIGHT:
   case CORNER_LEFT:
      if (x_scale != NULL) {
         *x_scale = (dx == 0 ? 1000 :
               (int)(((float)(x_current-x_pivot)) /
               ((float)(x_move-x_pivot))*1000.0));
      }
      if (y_scale != NULL) *y_scale = 1000;
      if (x_shear != NULL) *x_shear = 0;
      if (y_shear != NULL) {
         *y_shear = (dy == 0 ? 0 :
               (int)(atan2((double)dy,(double)x_current-x_pivot)*1000.0));
      }
      break;
   default: /* scaling only, no shearing */
      if (x_scale != NULL) {
         *x_scale = (dx == 0 ? 1000 :
               (int)(((float)(x_current-x_pivot)) /
               ((float)(x_move-x_pivot))*1000.0));
      }
      if (y_scale != NULL) {
         *y_scale = (dy == 0 ? 1000 :
               (int)(((float)(y_current-y_pivot)) /
               ((float)(y_move-y_pivot))*1000.0));
      }
      if (x_shear != NULL) *x_shear = 0;
      if (y_shear != NULL) *y_shear = 0;
      break;
   }
}

void PointsToArc(xc, yc, x1, y1, x2, y2, dir, int_degree, ltx, lty, w, h,
      angle1, angle2)
   int xc, yc, x1, y1, x2, y2, dir, int_degree;
   int *ltx, *lty, *w, *h, *angle1, *angle2;
   /* Only good if the points are part of a circle, not an oval */
{
   register int dx, dy, radius;
   double tmp_theta;

   dx = x1-xc; dy = y1-yc;
   radius = (int)sqrt((double)(((double)dx)*((double)dx) +
         ((double)dy)*((double)dy)));
   if (ltx != NULL) *ltx = xc-radius;
   if (lty != NULL) *lty = yc-radius;
   if (w != NULL) *w = (radius<<1);
   if (h != NULL) *h = (radius<<1);
   if (int_degree) {
      int theta1, theta2, d_theta;

      tmp_theta = (dx==0) ? ((dy>=0) ? M_PI/2.0 : -M_PI/2.0) :
            atan2 ((double)(dy),(double)(dx));
      theta1 = (int)(tmp_theta/M_PI*(-180.0));
      tmp_theta = (x2==xc) ? ((y2>=yc) ? M_PI/2.0 : -M_PI/2.0) :
            atan2 ((double)(y2-yc),(double)(x2-xc));
      theta2 = (int)(tmp_theta/M_PI*(-180.0));
      /* NOTE:  *angle1 must be between -180 degrees and +180 degrees */
      if (angle1 != NULL) *angle1 = theta1*64;
      d_theta = theta2-theta1;
      switch (dir) {
      case ARC_CCW: if (d_theta < 0) d_theta = 360 + d_theta; break;
      case ARC_CW:  if (d_theta > 0) d_theta = d_theta - 360; break;
      }
      if (d_theta == 0) d_theta = 360;
      if (angle2 != NULL) *angle2 = (d_theta<<6);
   } else {
      double theta1, theta2, d_theta;

      tmp_theta = (dx==0) ? ((dy>=0) ? M_PI/2.0 : -M_PI/2.0) :
            atan2 ((double)(dy),(double)(dx));
      theta1 = tmp_theta/M_PI*(-180.0);
      tmp_theta = (x2==xc) ? ((y2>=yc) ? M_PI/2.0 : -M_PI/2.0) :
            atan2 ((double)(y2-yc),(double)(x2-xc));
      theta2 = tmp_theta/M_PI*(-180.0);
      /* NOTE:  *angle1 must be between -180 degrees and +180 degrees */
      if (angle1 != NULL) *angle1 = (int)(theta1*64.0);
      d_theta = theta2-theta1;
      switch (dir) {
      case ARC_CCW: if (d_theta < 0) d_theta = 360.0 + d_theta; break;
      case ARC_CW:  if (d_theta > 0) d_theta = d_theta - 360.0; break;
      }
      if (fabs(d_theta) < ARC_TOL) d_theta = (double)360.0;
      if (angle2 != NULL) *angle2 = (int)(d_theta*64.0);
   }
}

void ArcRealX2Y2(ArcPtr, RealX2, RealY2)
   register struct ArcRec *ArcPtr;
   int *RealX2, *RealY2;
{
   register double angle_in_radian;
   int w=ArcPtr->w, h=ArcPtr->h;

   angle_in_radian = (ArcPtr->angle1+ArcPtr->angle2)*M_PI/180/64;
   *RealX2 = ArcPtr->xc + round((w/2)*cos(angle_in_radian));
   *RealY2 = ArcPtr->yc - round((h/2)*sin(angle_in_radian));
}

static
double GetRadianInArc(arc_ptr, x, y)
   struct ArcRec *arc_ptr;
   int x, y; /* x and y are abs */
{
   return ((double)atan2((double)((arc_ptr->yc-y)*arc_ptr->w),
         (double)((x-arc_ptr->xc)*arc_ptr->h)));
}

#define RETREAT (0.8)

void GetArcArrowInfo(obj_ptr, ptip_vs1, ptail_vs1, vs1, pa_angle1,
      ptip_vs2, ptail_vs2, vs2, pa_angle2)
   struct ObjRec *obj_ptr;
   IntPoint *ptip_vs1, *ptail_vs1, *vs1, *ptip_vs2, *ptail_vs2, *vs2;
   int *pa_angle1, *pa_angle2;
   /*
    * if want to get a_angle1 and a_angle2, this function must be
    * called in the following fashion (cannot omit the second call):
    *
    *     a_angle1 = arc_ptr->angle1;
    *     a_angle2 = arc_ptr->angle2;
    *     GetArcArrowInfo(*, *, *, *, &a_angle1, *, *, *, &a_angle2);
    *     arc_ptr->a_angle1 = a_angle1;
    *     arc_ptr->a_angle2 = a_angle2;
    */
{
   struct ArcRec *arc_ptr=obj_ptr->detail.a;
   int i, style=arc_ptr->style, a_angle1, a_angle2, angle90, angle360;
   int x1, y1, x2, y2, int_dx, int_dy, x, y, angle1, angle2, dir, w, h, aw, ah;
   double dx, dy, angle_in_radian, theta1, theta2;
   IntPoint tip_vs1, tail_vs1, tip_vs2, tail_vs2;

   if (style == LS_PLAIN) return;

   angle90 = (90<<6);
   angle360 = (360<<6);
   dir = arc_ptr->dir;
   x1 = arc_ptr->x1; y1 = arc_ptr->y1;
   angle1 = a_angle1 = arc_ptr->angle1; angle2 = a_angle2 = arc_ptr->angle2;
   w = arc_ptr->w; h = arc_ptr->h;
   aw = arc_ptr->aw; ah = arc_ptr->ah;

   /* the arrow should appear at angle1 */
   theta1 = 0.0;
   tip_vs1.x = x1;
   tip_vs1.y = y1;
   if ((style & LS_LEFT) && obj_ptr->ctm != NULL) {
      TransformPointThroughCTM(x1-obj_ptr->x, y1-obj_ptr->y,
            obj_ptr->ctm, &x, &y);
      tip_vs1.x = x+obj_ptr->x;
      tip_vs1.y = y+obj_ptr->y;
   }
   switch (dir) {
   case ARC_CCW: theta1=(double)(angle1-(angle90)); break;
   case ARC_CW: theta1=(double)(angle1+(angle90)); break;
   }
   dx = -((double)w)*cos(theta1*M_PI/180.0/64.0);
   dy = ((double)h)*sin(theta1*M_PI/180.0/64.0);
   int_dx = round(dx);
   int_dy = round(dy);
   tail_vs1.x = x1+int_dx;
   tail_vs1.y = y1+int_dy;
   if ((style & LS_LEFT) && obj_ptr->ctm != NULL) {
      TransformPointThroughCTM(x1+int_dx-obj_ptr->x, y1+int_dy-obj_ptr->y,
            obj_ptr->ctm, &x, &y);
      tail_vs1.x = x+obj_ptr->x;
      tail_vs1.y = y+obj_ptr->y;
   }
   if (int_dx==0 && int_dy==0) {
      if ((style & LS_LEFT) && vs1 != NULL) {
         if (obj_ptr->ctm == NULL) {
            vs1[0].x = vs1[1].x = vs1[2].x = vs1[3].x = x1;
            vs1[0].y = vs1[1].y = vs1[2].y = vs1[3].y = y1;
         } else {
            TransformPointThroughCTM(x1-obj_ptr->x, y1-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            vs1[0].x = vs1[1].x = vs1[2].x = vs1[3].x = x+obj_ptr->x;
            vs1[0].y = vs1[1].y = vs1[2].y = vs1[3].y = y+obj_ptr->y;
         }
      }
   } else {
      double sin_val, cos_val, len;

      dx = (double)(tail_vs1.x - tip_vs1.x);
      dy = (double)(tail_vs1.y - tip_vs1.y);
      len = (double)sqrt(dx*dx+dy*dy);
      sin_val = dy/len;
      cos_val = dx/len;
      if ((style & LS_LEFT) && vs1 != NULL) {
         vs1[0].x = tip_vs1.x;
         vs1[0].y = tip_vs1.y;
         vs1[1].x = round(tip_vs1.x + aw*cos_val - ah*sin_val);
         vs1[1].y = round(tip_vs1.y + aw*sin_val + ah*cos_val);
         vs1[2].x = round(tip_vs1.x + aw*cos_val + ah*sin_val);
         vs1[2].y = round(tip_vs1.y + aw*sin_val - ah*cos_val);
         vs1[3].x = vs1[0].x;
         vs1[3].y = vs1[0].y;
      }
      if ((style & LS_LEFT) && (pa_angle1 != NULL || pa_angle2 != NULL)) {
         x = round(tip_vs1.x + aw*cos_val*RETREAT);
         y = round(tip_vs1.y + aw*sin_val*RETREAT);
         if (obj_ptr->ctm == NULL) {
            angle_in_radian = GetRadianInArc(arc_ptr, x, y);
         } else {
            int tmp_x, tmp_y;

            ReverseTransformPointThroughCTM(x-obj_ptr->x, y-obj_ptr->y,
                  obj_ptr->ctm, &tmp_x, &tmp_y);
            x = tmp_x+obj_ptr->x;
            y = tmp_y+obj_ptr->y;
            angle_in_radian = GetRadianInArc(arc_ptr, x, y);
         }
         angle_in_radian = angle_in_radian*180.0/M_PI*64.0;
         a_angle1 = round(angle_in_radian);
      }
   }
   theta2 = 0.0;
   ArcRealX2Y2(arc_ptr, &x2, &y2);
   tip_vs2.x = x2;
   tip_vs2.y = y2;
   if ((style & LS_RIGHT) && obj_ptr->ctm != NULL) {
      TransformPointThroughCTM(x2-obj_ptr->x, y2-obj_ptr->y,
            obj_ptr->ctm, &x, &y);
      tip_vs2.x = x+obj_ptr->x;
      tip_vs2.y = y+obj_ptr->y;
   }
   switch (dir) {
   case ARC_CCW: theta2=(double)((angle1+angle2)+(angle90)); break;
   case ARC_CW: theta2=(double)((angle1+angle2)-(angle90)); break;
   }
   dx = -((double)w)*cos(theta2*M_PI/180.0/64.0);
   dy = ((double)h)*sin(theta2*M_PI/180.0/64.0);
   int_dx = round(dx);
   int_dy = round(dy);
   tail_vs2.x = x2+int_dx;
   tail_vs2.y = y2+int_dy;
   if ((style & LS_RIGHT) && obj_ptr->ctm != NULL) {
      TransformPointThroughCTM(x2+int_dx-obj_ptr->x, y2+int_dy-obj_ptr->y,
            obj_ptr->ctm, &x, &y);
      tail_vs2.x = x+obj_ptr->x;
      tail_vs2.y = y+obj_ptr->y;
   }
   if (int_dx==0 && int_dy==0) {
      if ((style & LS_RIGHT) && vs2 != NULL) {
         if (obj_ptr->ctm != NULL) {
            vs2[0].x = vs2[1].x = vs2[2].x = vs2[3].x = x2;
            vs2[0].y = vs2[1].y = vs2[2].y = vs2[3].y = y2;
         } else {
            TransformPointThroughCTM(x2-obj_ptr->x, y2-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            vs2[0].x = vs2[1].x = vs2[2].x = vs2[3].x = x+obj_ptr->x;
            vs2[0].y = vs2[1].y = vs2[2].y = vs2[3].y = y+obj_ptr->y;
         }
      }
   } else {
      double sin_val, cos_val, len;

      dx = tail_vs2.x - tip_vs2.x;
      dy = tail_vs2.y - tip_vs2.y;
      len = (double)sqrt(dx*dx+dy*dy);
      sin_val = dy/len;
      cos_val = dx/len;
      if ((style & LS_RIGHT) && vs2 != NULL) {
         vs2[0].x = tip_vs2.x;
         vs2[0].y = tip_vs2.y;
         vs2[1].x = round(tip_vs2.x + aw*cos_val - ah*sin_val);
         vs2[1].y = round(tip_vs2.y + aw*sin_val + ah*cos_val);
         vs2[2].x = round(tip_vs2.x + aw*cos_val + ah*sin_val);
         vs2[2].y = round(tip_vs2.y + aw*sin_val - ah*cos_val);
         vs2[3].x = vs2[0].x;
         vs2[3].y = vs2[0].y;
      }
      if (pa_angle1 != NULL || pa_angle2 != NULL) {
         int delta64;

         if (style & LS_RIGHT) {
            x = round(tip_vs2.x + aw*cos_val*RETREAT);
            y = round(tip_vs2.y + aw*sin_val*RETREAT);
            if (obj_ptr->ctm == NULL) {
               angle_in_radian = GetRadianInArc(arc_ptr, x, y);
            } else {
               int tmp_x, tmp_y;

               ReverseTransformPointThroughCTM(x-obj_ptr->x, y-obj_ptr->y,
                     obj_ptr->ctm, &tmp_x, &tmp_y);
               x = tmp_x+obj_ptr->x;
               y = tmp_y+obj_ptr->y;
               angle_in_radian = GetRadianInArc(arc_ptr, x, y);
            }
            angle_in_radian = angle_in_radian*180.0/M_PI*64.0;
         } else {
            angle_in_radian = angle1+angle2;
            while (angle_in_radian > angle360) angle_in_radian -= angle360;
            while (angle_in_radian < (-angle360)) angle_in_radian += angle360;
         }
         delta64 = round(angle_in_radian)-(a_angle1);
         switch(dir) {
         case ARC_CCW: if (delta64 < 0) delta64 += (angle360); break;
         case ARC_CW: if (delta64 > 0) delta64 -= (angle360); break;
         }
         while (delta64 > angle360) delta64 -= angle360;
         while (delta64 < (-angle360)) delta64 += angle360;
         if (delta64 == 0 && angle2 != 0) delta64 = (angle360);
         a_angle2 = delta64;
      }
   }
   if (pa_angle1 != NULL) *pa_angle1 = a_angle1;
   if (pa_angle2 != NULL) *pa_angle2 = a_angle2;
   if (ptip_vs1 != NULL) memcpy(ptip_vs1, &tip_vs1, sizeof(IntPoint));
   if (ptail_vs1 != NULL) memcpy(ptail_vs1, &tail_vs1, sizeof(IntPoint));
   if (ptip_vs2 != NULL) memcpy(ptip_vs2, &tip_vs2, sizeof(IntPoint));
   if (ptail_vs2 != NULL) memcpy(ptail_vs2, &tail_vs2, sizeof(IntPoint));
}

void CalcArcOBBox(ObjPtr)
   struct ObjRec *ObjPtr;
{
   struct ArcRec *arc_ptr=ObjPtr->detail.a;

   if (ObjPtr->ctm == NULL) {
      int theta1, theta2, real_x2, real_y2;
      int dir=arc_ptr->dir, ltx=arc_ptr->ltx, lty=arc_ptr->lty;
      int w=arc_ptr->w, h=arc_ptr->h, pass_theta1=FALSE, coverage=0, angle;
      struct BBRec obbox;

      memcpy(&obbox, &ObjPtr->obbox, sizeof(struct BBRec));

      ObjPtr->x = arc_ptr->xc;
      ObjPtr->y = arc_ptr->yc;

      theta1 = (arc_ptr->angle1)/64;
      theta2 = theta1 + (arc_ptr->angle2)/64;

      ArcRealX2Y2(arc_ptr, &real_x2, &real_y2);

      if (arc_ptr->fill == NONEPAT) {
         /* don't counter the center of the arc */
         obbox.ltx = min(arc_ptr->x1,real_x2);
         obbox.lty = min(arc_ptr->y1,real_y2);
         obbox.rbx = max(arc_ptr->x1,real_x2);
         obbox.rby = max(arc_ptr->y1,real_y2);
      } else {
         obbox.ltx = min(arc_ptr->xc,min(arc_ptr->x1,real_x2));
         obbox.lty = min(arc_ptr->yc,min(arc_ptr->y1,real_y2));
         obbox.rbx = max(arc_ptr->xc,max(arc_ptr->x1,real_x2));
         obbox.rby = max(arc_ptr->yc,max(arc_ptr->y1,real_y2));
      }

      if (theta2 < -180) theta2 += 360;
      if (theta2 > 180) theta2 -= 360;

      if (theta1 < 0) theta1 += 360;
      if (theta2 < 0) theta2 += 360;

      if (theta1 == theta2) {
         coverage = 0xf;
      } else if (dir == ARC_CCW) {
         angle = 0;
         while (angle < theta2 || !pass_theta1) {
            if (angle >= theta1 && !pass_theta1) {
               pass_theta1 = TRUE;
               if (theta2 > theta1 && angle >= theta2) break;
               if (theta2 < theta1) angle -= 360;
            }
            if (pass_theta1) coverage |= 1 << (((angle+360)/90) % 4);
            angle = (angle == 360) ? 0 : (angle+90);
         }
      } else {
         angle = 360;
         while (angle > theta2 || !pass_theta1) {
            if (angle <= theta1 && !pass_theta1) {
               pass_theta1 = TRUE;
               if (theta2 < theta1 && angle <= theta2) break;
               if (theta2 > theta1) angle += 360;
            }
            if (pass_theta1) coverage |= 1 << ((angle/90) % 4);
            angle = (angle == 0) ? 360 : (angle-90);
         }
      }
      if (coverage & 0x1) { EXPAND_BBOX(&obbox,(int)(ltx+w),(int)(lty+h/2)); }
      if (coverage & 0x2) { EXPAND_BBOX(&obbox,(int)(ltx+w/2),lty); }
      if (coverage & 0x4) { EXPAND_BBOX(&obbox,ltx,(int)(lty+h/2)); }
      if (coverage & 0x8) { EXPAND_BBOX(&obbox,(int)(ltx+w/2),(int)(lty+h)); }
      memcpy(&ObjPtr->obbox, &obbox, sizeof(struct BBRec));
   } else {
      IntPoint abs_obj_obbox_vs[5];

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
}

void CalcArcBBox(obj_ptr, obbox, bbox)
   struct ObjRec *obj_ptr;
   struct BBRec obbox, *bbox;
   /* given good obbox, figure out bbox */
{
   struct ArcRec *arc_ptr=obj_ptr->detail.a;
   int i, half_w=(arc_ptr->width>>1);
   int ltx=obbox.ltx-half_w, lty=obbox.lty-half_w;
   int rbx=obbox.rbx+half_w, rby=obbox.rby+half_w;
   IntPoint vs1[4], vs2[4];

   GetArcArrowInfo(obj_ptr, NULL, NULL, vs1, NULL, NULL, NULL, vs2, NULL);
   if (arc_ptr->style & LS_LEFT) {
      for (i=0; i < 4; i++) {
         if (vs1[i].x < ltx) ltx = vs1[i].x;
         if (vs1[i].y < lty) lty = vs1[i].y;
         if (vs1[i].x > rbx) rbx = vs1[i].x;
         if (vs1[i].y > rby) rby = vs1[i].y;
      }
   }
   if (arc_ptr->style & LS_RIGHT) {
      for (i=0; i < 4; i++) {
         if (vs2[i].x < ltx) ltx = vs2[i].x;
         if (vs2[i].y < lty) lty = vs2[i].y;
         if (vs2[i].x > rbx) rbx = vs2[i].x;
         if (vs2[i].y > rby) rby = vs2[i].y;
      }
   }
   bbox->ltx = min(ltx, obbox.ltx-(arc_ptr->width>>1));
   bbox->lty = min(lty, obbox.lty-(arc_ptr->width>>1));
   bbox->rbx = max(rbx, obbox.rbx+(arc_ptr->width>>1));
   bbox->rby = max(rby, obbox.rby+(arc_ptr->width>>1));
}

static
void DumpArcPSPath(FP, xc, yc, xr, yr, dir, a1, a2, outline, blank1, blank2)
   FILE *FP;
   int xc, yc, xr, yr, dir, a1, a2, outline;
   char *blank1, *blank2;
{
#ifdef INVERT_CTM_BUG
   if (preDumpSetup) PSUseMinRadius();
#endif /* INVERT_CTM_BUG */

   if (preDumpSetup) PSUseArc();
   fprintf(FP, "%snewpath\n", blank1);
   if (outline) fprintf(FP, "%s%1d %1d moveto\n", blank2, xc, yc);
#ifdef INVERT_CTM_BUG
   switch (dir) {
   case ARC_CCW:
      fprintf(FP, "%s%1d %1d %1d %s %1d %s %1d %1d tgifarc\n", blank2,
            xc, yc, xr, "tgif_min_radius", yr, "tgif_min_radius", a1, a2);
      break;
   case ARC_CW:
      fprintf(FP, "%s%1d %1d %1d %s %1d %s %1d %1d tgifarcn\n", blank2,
            xc, yc, xr, "tgif_min_radius", yr, "tgif_min_radius", a1, a2);
      break;
   }
#else
   switch (dir) {
   case ARC_CCW:
      fprintf(FP, "%s%1d %1d %1d %1d %1d %1d tgifarc\n", blank2,
            xc, yc, xr, yr, a1, a2);
      break;
   case ARC_CW:
      fprintf(FP, "%s%1d %1d %1d %1d %1d %1d tgifarcn\n", blank2,
            xc, yc, xr, yr, a1, a2);
      break;
   }
#endif
   if (outline) fprintf(FP, "%s%1d %1d lineto\n", blank2, xc, yc);
}

static
void DumpArcArrows(FP, obj_ptr)
   FILE *FP;
   struct ObjRec *obj_ptr;
{
   struct ArcRec *arc_ptr=obj_ptr->detail.a;
   int style=arc_ptr->style, aw=arc_ptr->aw, ah=arc_ptr->ah, pen=arc_ptr->pen;
   int color_index=obj_ptr->color;
   IntPoint tip_vs1, tail_vs1, tip_vs2, tail_vs2;
   char *aw_spec=arc_ptr->aw_spec, *ah_spec=arc_ptr->ah_spec;

   GetArcArrowInfo(obj_ptr, &tip_vs1, &tail_vs1, NULL, NULL,
         &tip_vs2, &tail_vs2, NULL, NULL);
   if (obj_ptr->ctm == NULL) {
      if (style & LS_LEFT) {
         DumpArrow(FP, &tail_vs1, &tip_vs1, aw, ah, aw_spec, ah_spec,
               pen, color_index);
      }
      if (style & LS_RIGHT) {
         DumpArrow(FP, &tail_vs2, &tip_vs2, aw, ah, aw_spec, ah_spec,
               pen, color_index);
      }
   } else {
      if (style & LS_LEFT) {
         DumpArrow(FP, &tail_vs1, &tip_vs1, aw, ah, aw_spec, ah_spec,
               pen, color_index);
      }
      if (style & LS_RIGHT) {
         DumpArrow(FP, &tail_vs2, &tip_vs2, aw, ah, aw_spec, ah_spec,
               pen, color_index);
      }
   }
}

static
void DumpArcPath(FP,ObjPtr,xc,yc,xr,yr,dir,angle1,angle2,width,pen,dash)
   FILE *FP;
   struct ObjRec *ObjPtr;
   int xc, yc, xr, yr, dir, angle1, angle2, width, pen, dash;
{
   register int i;

   fprintf(FP, "   gsave\n");
   if (!colorDump && useGray && pen > BACKPAT) {
      GrayCheck(pen);
      fprintf(FP, "      %s setgray\n", GrayStr(pen));
   }

   DumpArcPSPath(FP,xc,yc,xr,yr,dir,angle1,angle2,FALSE,"      ","         ");

   if (ObjPtr->ctm != NULL) {
      fprintf(FP, "      tgiforigctm setmatrix\n");
   }
   if (width != 1) fprintf(FP, "      %1d setlinewidth\n", width);
   if (dash != 0) {
      fprintf(FP, "      [");
      for (i = 0; i < dashListLength[dash]-1; i++) {
         fprintf(FP, "%1d ", (int)(dashList[dash][i]));
      }
      fprintf(FP, "%1d] 0 setdash\n",
            (int)(dashList[dash][dashListLength[dash]-1]));
   }
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
   }
   fprintf(FP, "   grestore\n");
}

void DumpArcObj(FP, ObjPtr)
   FILE *FP;
   struct ObjRec *ObjPtr;
{
   register struct ArcRec *arc_ptr=ObjPtr->detail.a;
   int fill, width, pen, dash, color_index;
   int xc, yc, xr, yr, dir, angle1, angle2, style, a_angle1, a_angle2;
   int x1, y1, aw, ah;

   fill = arc_ptr->fill;
   width = arc_ptr->width;
   aw = arc_ptr->aw;
   ah = arc_ptr->ah;
   pen = arc_ptr->pen;
   dash = arc_ptr->dash;
   style = arc_ptr->style;
   xc = arc_ptr->xc; yc = arc_ptr->yc;
   x1 = arc_ptr->x1; y1 = arc_ptr->y1;
   xr = (int)(arc_ptr->w/2); yr = (int)(arc_ptr->h/2);
   dir = arc_ptr->dir;
   angle1 = -round(((double)arc_ptr->angle1)/64.0);
   angle2 = -round(((double)arc_ptr->angle2)/64.0) + angle1;
   a_angle1 = -round(((double)arc_ptr->a_angle1)/64.0);
   a_angle2 = -round(((double)arc_ptr->a_angle2)/64.0) + a_angle1;

   if (fill == NONEPAT && pen == NONEPAT) return;

   fprintf(FP, "%% ARC\n");
   if (ObjPtr->ctm != NULL) {
      float m[6];

      fprintf(FP, "gsave\n");
      m[CTM_SX] = ((float)ObjPtr->ctm->m[CTM_SX])/((float)1000.0);
      m[CTM_SY] = ((float)ObjPtr->ctm->m[CTM_SY])/((float)1000.0);
      m[CTM_SIN] = ((float)ObjPtr->ctm->m[CTM_SIN])/((float)1000.0);
      m[CTM_MSIN] = ((float)ObjPtr->ctm->m[CTM_MSIN])/((float)1000.0);
      fprintf(FP, "   %1d %1d translate\n", ObjPtr->x, ObjPtr->y);
      fprintf(FP, "   [%.3f %.3f %.3f %.3f %1d %1d] concat\n",
            m[CTM_SX], m[CTM_SIN], m[CTM_MSIN], m[CTM_SY],
            ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY]);
      fprintf(FP, "   %1d neg %1d neg translate\n", ObjPtr->x, ObjPtr->y);
   }
   color_index = ObjPtr->color;
   DumpRGBColorLine(FP, color_index, 0, TRUE);

   switch (fill) {
   case NONEPAT: break;
   case SOLIDPAT:
      DumpArcPSPath(FP,xc,yc,xr,yr,dir,angle1,angle2,TRUE,"","   ");
      fprintf(FP, "   fill\n");
      break;
   case BACKPAT:
      DumpArcPSPath(FP,xc,yc,xr,yr,dir,angle1,angle2,TRUE,"","   ");
      fprintf(FP, "   closepath 1 setgray fill\n");
      DumpRGBColorLine(FP, color_index, 3, TRUE);
      break;
   default:
      fprintf(FP, "gsave\n");
      if (colorDump || !useGray) {
         if (preDumpSetup) PSUseColorPattern();
         DumpArcPSPath(FP, xc, yc, xr, yr, dir, angle1, angle2, TRUE,
               "   ","      ");
         fprintf(FP, "   closepath 1 setgray fill\n");
         DumpRGBColorLine(FP, color_index, 3, TRUE);
         DumpArcPSPath(FP,xc,yc,xr,yr,dir,angle1,angle2,TRUE,"   ","      ");
         fprintf(FP, "   closepath clip newpath\n");
         DumpPatFill(FP, fill, 8, ObjPtr->bbox, "   ");
      } else {
         GrayCheck(fill);
         fprintf(FP, "   %s setgray\n", GrayStr(fill));
         DumpArcPSPath(FP,xc,yc,xr,yr,dir,angle1,angle2,TRUE,"   ","      ");
         fprintf(FP, "   fill\n");
      }
      fprintf(FP, "grestore\n");
      break;
   }

   if (pen == NONEPAT) {
      if (ObjPtr->ctm != NULL) fprintf(FP, "grestore\n");
      fprintf(FP, "\n");
      return;
   }

   fprintf (FP, "gsave\n");

   if ((colorDump || !useGray) && pen > BACKPAT) {
      if (style == LS_PLAIN) {
         DumpArcPath(FP,ObjPtr,xc,yc,xr,yr,dir,angle1,angle2,width,BACKPAT,0);
      } else {
         DumpArcPath(FP,ObjPtr,xc,yc,xr,yr,dir,a_angle1,a_angle2,width,
               BACKPAT,0);
      }
      DumpRGBColorLine(FP, color_index, 3, TRUE);
   }
   if (style == LS_PLAIN) {
      DumpArcPath(FP,ObjPtr,xc,yc,xr,yr,dir,angle1,angle2,width,pen,dash);
   } else {
      DumpArcPath(FP,ObjPtr,xc,yc,xr,yr,dir,a_angle1,a_angle2,width,pen,dash);
   }
   fprintf (FP, "grestore\n");

   if (style != LS_PLAIN) {
      DumpArcArrows(FP,ObjPtr);
   }
   if (ObjPtr->ctm != NULL) fprintf(FP, "grestore\n");
   fprintf (FP, "\n");
}

int NeedsToCacheArcObj(ObjPtr)
   struct ObjRec *ObjPtr;
{
   return (ObjPtr->ctm != NULL);
}

static int arcXYMagInitialized=FALSE;
static double arcXMag[6], arcYMag[6];

static
void MakeArcRotatedVs(obj_ptr, oval_vs, angle1, angle2,
      rotated_n, rotated_vlist)
   struct ObjRec *obj_ptr;
   IntPoint *oval_vs;
   int angle1, angle2, *rotated_n;
   XPoint **rotated_vlist;
{
   struct ArcRec *arc_ptr=obj_ptr->detail.a;
   XPoint *sv=NULL;
   IntPoint *pv, *cntrlv=NULL;
   int i, sn, cntrln, v_index, cur_index, angle;
   int num_vs=0, angle90=(90<<6), angle360=(360<<6), seen_angle1;
   int cx=arc_ptr->xc, cy=arc_ptr->yc;
   double hw=(double)(arc_ptr->w>>1), hh=(double)(arc_ptr->h>>1);
   int start_angle=angle90-angle360, end_angle=angle360;

   seen_angle1 = FALSE;
   if (angle1 > angle2) {
      angle = angle2;
      angle2 = angle1;
      angle1 = angle;
   }
   while (start_angle > angle1) start_angle -= angle360;
   while (end_angle < angle2) end_angle += angle360;
   for (angle=start_angle; angle <= end_angle; angle+=angle90) {
      /* the quadrant being considered is angle-angle90 to angle */
      double angle_in_quadrant; /* 0 < angle_in_quadrant <= 90.0 degrees */

      if (seen_angle1) {
         if (angle >= angle2) {
            angle_in_quadrant = ((double)(angle2-(angle-angle90)))/64.0;
            if (angle_in_quadrant < 20.0) {
               num_vs++;
            } else if (angle_in_quadrant < 35.0) {
               num_vs += 2;
            } else if (angle_in_quadrant < 50.0) {
               num_vs += 3;
            } else if (angle_in_quadrant < 65.0) {
               num_vs += 4;
            } else if (angle_in_quadrant < 80.0) {
               num_vs += 5;
            } else {
               num_vs += 6;
            }
            break;
         } else {
            num_vs += 6;
         }
      } else {
         if (angle >= angle1) {
            angle_in_quadrant = ((double)(angle1-(angle-angle90)))/64.0;
            if (angle_in_quadrant < 10.0) {
               num_vs += 7;
            } else if (angle_in_quadrant < 25.0) {
               num_vs += 6;
            } else if (angle_in_quadrant < 40.0) {
               num_vs += 5;
            } else if (angle_in_quadrant < 55.0) {
               num_vs += 4;
            } else if (angle_in_quadrant < 70.0) {
               num_vs += 3;
            } else if (angle_in_quadrant < 80.0) {
               num_vs += 2;
            } else {
               num_vs++;
            }
            if (angle >= angle2) {
               angle_in_quadrant = ((double)(angle2-(angle-angle90)))/64.0;
               if (angle_in_quadrant < 20.0) {
                  num_vs -= 5;
               } else if (angle_in_quadrant < 35.0) {
                  num_vs -= 4;
               } else if (angle_in_quadrant < 50.0) {
                  num_vs -= 3;
               } else if (angle_in_quadrant < 65.0) {
                  num_vs -= 2;
               } else if (angle_in_quadrant < 80.0) {
                  num_vs--;
               }
               break;
            }
            seen_angle1 = TRUE;
         }
      }
   }
   *rotated_n = 0;
   *rotated_vlist = (XPoint*)malloc((num_vs+3)*sizeof(XPoint));
   pv = (IntPoint*)malloc((num_vs+3)*sizeof(IntPoint));
   if (*rotated_vlist == NULL || pv == NULL) FailAllocMessage();

   cur_index = 0;
   seen_angle1 = FALSE;
   for (angle=start_angle, v_index=0; angle <= end_angle;
         angle+=angle90, v_index+=6) {
      /* the quadrant being considered is angle-angle90 to angle */
      double angle_in_quadrant; /* 0 < angle_in_quadrant <= 90.0 degrees */
      int count;
      double sin_val, cos_val;

      if (v_index >= 24) v_index = 0;
      if (seen_angle1) {
         if (angle >= angle2) {
            angle_in_quadrant = ((double)(angle2-(angle-angle90)))/64.0;
            if (angle_in_quadrant < 20.0) {
               count = 1;
            } else if (angle_in_quadrant < 35.0) {
               count = 2;
            } else if (angle_in_quadrant < 50.0) {
               count = 3;
            } else if (angle_in_quadrant < 65.0) {
               count = 4;
            } else if (angle_in_quadrant < 80.0) {
               count = 5;
            } else {
               count = 6;
            }
            for (i=1; i < count; i++) {
               pv[cur_index].x = oval_vs[v_index+i].x;
               pv[cur_index].y = oval_vs[v_index+i].y;
               cur_index++;
            }
            sin_val = cos((double)(angle2*M_PI/180.0/64.0));
            cos_val = sin((double)(angle2*M_PI/180.0/64.0));
            pv[cur_index].x = cx + round(hw*sin_val);
            pv[cur_index].y = cy - round(hh*cos_val);
            cur_index++;
            break;
         } else {
            for (i=1; i < 7; i++) {
               pv[cur_index].x = oval_vs[v_index+i].x;
               pv[cur_index].y = oval_vs[v_index+i].y;
               cur_index++;
            }
         }
      } else {
         if (angle >= angle1) {
            angle_in_quadrant = ((double)(angle1-(angle-angle90)))/64.0;
            if (angle_in_quadrant < 10.0) {
               count = 7;
            } else if (angle_in_quadrant < 25.0) {
               count = 6;
            } else if (angle_in_quadrant < 40.0) {
               count = 5;
            } else if (angle_in_quadrant < 55.0) {
               count = 4;
            } else if (angle_in_quadrant < 70.0) {
               count = 3;
            } else if (angle_in_quadrant < 80.0) {
               count = 2;
            } else {
               count = 1;
            }
            sin_val = cos((double)(angle1*M_PI/180.0/64.0));
            cos_val = sin((double)(angle1*M_PI/180.0/64.0));
            pv[0].x = cx + round(hw*sin_val);
            pv[0].y = cy - round(hh*cos_val);
            for (i=1; i < count; i++) {
               pv[i].x = oval_vs[(v_index+7-(count-i)) % 24].x;
               pv[i].y = oval_vs[(v_index+7-(count-i)) % 24].y;
            }
            cur_index += count;
            if (angle >= angle2) {
               angle_in_quadrant = ((double)(angle2-(angle-angle90)))/64.0;
               if (angle_in_quadrant < 20.0) {
                  cur_index -= 6;
               } else if (angle_in_quadrant < 35.0) {
                  cur_index -= 5;
               } else if (angle_in_quadrant < 50.0) {
                  cur_index -= 4;
               } else if (angle_in_quadrant < 65.0) {
                  cur_index -= 3;
               } else if (angle_in_quadrant < 80.0) {
                  cur_index -= 2;
               } else {
                  cur_index--;
               }
               sin_val = cos((double)(angle2*M_PI/180.0/64.0));
               cos_val = sin((double)(angle2*M_PI/180.0/64.0));
               pv[cur_index].x = cx + round(hw*sin_val);
               pv[cur_index].y = cy - round(hh*cos_val);
               cur_index++;
               break;
            }
            seen_angle1 = TRUE;
         }
      }
   }
   if (num_vs != cur_index) {
      fprintf(stderr, "num_vs (%1d) != cur_index (%1d)\n", num_vs, cur_index);
   }
   pv[num_vs].x = cx;
   pv[num_vs].y = cy;
   for (i=0; i < num_vs+1; i++) {
      int x, y;

      TransformPointThroughCTM(pv[i].x-obj_ptr->x, pv[i].y-obj_ptr->y,
            obj_ptr->ctm, &x, &y);
      pv[i].x = x + obj_ptr->x;
      pv[i].y = y + obj_ptr->y;
      (*rotated_vlist)[i].x = (short)OFFSET_X(pv[i].x);
      (*rotated_vlist)[i].y = (short)OFFSET_Y(pv[i].y);
   }
   sv = MakeIntSplinePolyVertex(&sn, &cntrln, &cntrlv,
         drawOrigX, drawOrigY, num_vs, pv);
   if (sv == NULL) {
      FailAllocMessage();
   } else {
      XPoint *new_sv=(XPoint*)malloc((sn+3)*sizeof(XPoint));

      if (new_sv == NULL) FailAllocMessage();
      for (i=0; i < sn; i++) {
         new_sv[i].x = sv[i].x;
         new_sv[i].y = sv[i].y;
      }
      new_sv[sn].x = (*rotated_vlist)[num_vs].x;
      new_sv[sn].y = (*rotated_vlist)[num_vs].y;
      new_sv[sn+1].x = new_sv[0].x;
      new_sv[sn+1].y = new_sv[0].y;
      free(sv);
      sv = new_sv;
   }
   free(*rotated_vlist);
   *rotated_n = sn;
   *rotated_vlist = sv;

   free(pv);
   if (cntrlv != NULL) free(cntrlv);
}

static
void MakeCachedArc(ObjPtr)
   struct ObjRec *ObjPtr;
{
   struct ArcRec *arc_ptr=ObjPtr->detail.a;
   XPoint *sv=NULL;
   IntPoint *pv, *cntrlv=NULL, tmp_vs[25];
   struct BBRec obbox;
   int i, cx, cy, v_index, cur_index, angle, a_angle1, a_angle2;
   double hw, hh; /* half w and half h */

   if (!arcXYMagInitialized) {
      int j;

      for (i=0, j=0; i < 90; i+=15, j++) {
         arcXMag[j] = cos((double)(((double)i)*M_PI/180.0));
         arcYMag[j] = sin((double)(((double)i)*M_PI/180.0));
      }
      arcXYMagInitialized = TRUE;
   }
   if (ObjPtr->ctm == NULL) return;

   a_angle1 = arc_ptr->angle1;
   a_angle2 = arc_ptr->angle2;
   if (arc_ptr->style != LS_PLAIN) {
      GetArcArrowInfo(ObjPtr, NULL, NULL, NULL, &a_angle1,
            NULL, NULL, NULL, &a_angle2);
   }
   arc_ptr->a_angle1 = a_angle1;
   arc_ptr->a_angle2 = a_angle2;
   cx = arc_ptr->xc;
   cy = arc_ptr->yc;
   obbox.ltx = cx-(arc_ptr->w>>1); obbox.lty = cy-(arc_ptr->h>>1);
   obbox.rbx = cx+(arc_ptr->w>>1); obbox.rby = cy+(arc_ptr->h>>1);
   hw = (double)(arc_ptr->w>>1);
   hh = (double)(arc_ptr->h>>1);

   for (i=0; i < 24; i++) {
      double dx, dy;

      switch (i) {
      case 0: tmp_vs[0].x=obbox.rbx; tmp_vs[0].y=cy; break;
      case 6: tmp_vs[6].x=cx; tmp_vs[6].y=obbox.lty; break;
      case 12: tmp_vs[12].x=obbox.ltx; tmp_vs[12].y=cy; break;
      case 18: tmp_vs[18].x=cx; tmp_vs[18].y=obbox.rby; break;
      default:
         if (i < 6) {
            dx = (hw*arcXMag[i % 6]); dy = (hh*arcYMag[i % 6]);
            tmp_vs[i].x=cx+round(dx); tmp_vs[i].y=cy-round(dy);
         } else if (i < 12) {
            dx = (hw*arcXMag[(24-i) % 6]); dy = (hh*arcYMag[(24-i) % 6]);
            tmp_vs[i].x=cx-round(dx); tmp_vs[i].y=cy-round(dy);
         } else if (i < 18) {
            dx = (hw*arcXMag[i % 6]); dy = (hh*arcYMag[i % 6]);
            tmp_vs[i].x=cx-round(dx); tmp_vs[i].y=cy+round(dy);
         } else {
            dx = (hw*arcXMag[(24-i) % 6]); dy = (hh*arcYMag[(24-i) % 6]);
            tmp_vs[i].x=cx+round(dx); tmp_vs[i].y=cy+round(dy);
         }
         break;
      }
   }
   tmp_vs[24].x=tmp_vs[0].x; tmp_vs[24].y=tmp_vs[0].y;

   if (arc_ptr->rotated_vlist != NULL) free(arc_ptr->rotated_vlist);
   if (arc_ptr->rotated_asvlist != NULL) free(arc_ptr->rotated_asvlist);
   arc_ptr->rotated_vlist = arc_ptr->rotated_asvlist = NULL;
   arc_ptr->rotated_n = arc_ptr->rotated_asn = 0;

   MakeArcRotatedVs(ObjPtr, tmp_vs, arc_ptr->angle1,
         arc_ptr->angle1+arc_ptr->angle2, &arc_ptr->rotated_n,
         &arc_ptr->rotated_vlist);
   if (arc_ptr->style != LS_PLAIN) {
      MakeArcRotatedVs(ObjPtr, tmp_vs, arc_ptr->a_angle1,
            arc_ptr->a_angle1+arc_ptr->a_angle2, &arc_ptr->rotated_asn,
            &arc_ptr->rotated_asvlist);
   }
}

void DrawArcObj(window, XOff, YOff, ObjPtr)
   Window window;
   int XOff, YOff;
   struct ObjRec *ObjPtr;
{
   struct ArcRec *arc_ptr=ObjPtr->detail.a;
   int i, ltx, lty, w, h, angle1, angle2;
   int fill, width, pen, dash, pixel, real_x_off, real_y_off;
   XPoint tmp_v[4];
   IntPoint vs1[4], vs2[4];
   XGCValues values;

   if (NeedsToCacheArcObj(ObjPtr) && arc_ptr->rotated_vlist==NULL) {
      MakeCachedArc(ObjPtr);
   }
   real_x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   real_y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);

   ltx = ZOOMED_SIZE(arc_ptr->ltx-real_x_off);
   lty = ZOOMED_SIZE(arc_ptr->lty-real_y_off);
   w = ZOOMED_SIZE(arc_ptr->ltx+arc_ptr->w-real_x_off)-ltx;
   h = ZOOMED_SIZE(arc_ptr->lty+arc_ptr->h-real_y_off)-lty;
   angle1 = arc_ptr->angle1;
   angle2 = arc_ptr->angle2;

   fill = arc_ptr->fill;
   width = arc_ptr->width;
   pen = arc_ptr->pen;
   dash = arc_ptr->dash;
   pixel = colorPixels[ObjPtr->color];

   if (fill != NONEPAT) {
      values.foreground = (fill == NONEPAT) ? myBgPixel : pixel;
      values.function = GXcopy;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[fill];
      XChangeGC(mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple, &values);
      if (ObjPtr->ctm != NULL) {
         XFillPolygon(mainDisplay, window, drawGC, arc_ptr->rotated_vlist,
               arc_ptr->rotated_n+2, Complex, CoordModeOrigin);
      } else {
         XFillArc(mainDisplay, window, drawGC, ltx, lty, w, h, angle1, angle2);
      }
   }

   if (pen == NONEPAT) return;

   values.foreground = (pen == NONEPAT) ? myBgPixel : pixel;
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

   GetArcArrowInfo(ObjPtr, NULL, NULL, vs1, NULL, NULL, NULL, vs2, NULL);
   if (arc_ptr->style & LS_LEFT) {
      for (i=0; i < 3; i++) {
         tmp_v[i].x = (short)ZOOMED_SIZE(vs1[i].x-real_x_off);
         tmp_v[i].y = (short)ZOOMED_SIZE(vs1[i].y-real_y_off);
      }
      tmp_v[3].x = tmp_v[0].x;
      tmp_v[3].y = tmp_v[0].y;
      XFillPolygon(mainDisplay, window, drawGC, tmp_v, 4, Convex,
            CoordModeOrigin);
   }
   if (arc_ptr->style & LS_RIGHT) {
      for (i=0; i < 3; i++) {
         tmp_v[i].x = (short)ZOOMED_SIZE(vs2[i].x-real_x_off);
         tmp_v[i].y = (short)ZOOMED_SIZE(vs2[i].y-real_y_off);
      }
      tmp_v[3].x = tmp_v[0].x;
      tmp_v[3].y = tmp_v[0].y;
      XFillPolygon(mainDisplay, window, drawGC, tmp_v, 4, Convex,
            CoordModeOrigin);
   }
   if (ObjPtr->ctm != NULL) {
      if (arc_ptr->style == LS_PLAIN) {
         XDrawLines(mainDisplay, window, drawGC, arc_ptr->rotated_vlist,
               arc_ptr->rotated_n, CoordModeOrigin);
      } else {
         XDrawLines(mainDisplay, window, drawGC, arc_ptr->rotated_asvlist,
               arc_ptr->rotated_asn, CoordModeOrigin);
      }
   } else {
      if (arc_ptr->style == LS_PLAIN) {
         XDrawArc(mainDisplay, window, drawGC, ltx, lty, w, h, angle1, angle2);
      } else {
         XDrawArc(mainDisplay, window, drawGC, ltx, lty, w, h,
               arc_ptr->a_angle1, arc_ptr->a_angle2);
      }
   }
}

struct ObjRec *CreateArcObj(xc,yc,x1,y1,x2,y2,dir,ltx,lty,w,h,angle1,angle2)
   int xc, yc, x1, y1, x2, y2, dir, ltx, lty, w, h, angle1, angle2;
{
   struct ArcRec *arc_ptr;
   struct ObjRec *obj_ptr;

   arc_ptr = (struct ArcRec *)malloc(sizeof(struct ArcRec));
   if (arc_ptr == NULL) FailAllocMessage();
   memset(arc_ptr, 0, sizeof(struct ArcRec));
   arc_ptr->fill = objFill;
   arc_ptr->width = curWidthOfLine[lineWidth];
   arc_ptr->aw = curArrowHeadW[lineWidth];
   arc_ptr->ah = curArrowHeadH[lineWidth];
   UtilStrCpy(arc_ptr->width_spec, sizeof(arc_ptr->width_spec),
         curWidthOfLineSpec[lineWidth]);
   UtilStrCpy(arc_ptr->aw_spec, sizeof(arc_ptr->aw_spec),
         curArrowHeadWSpec[lineWidth]);
   UtilStrCpy(arc_ptr->ah_spec, sizeof(arc_ptr->ah_spec),
         curArrowHeadHSpec[lineWidth]);
   arc_ptr->pen = penPat;
   arc_ptr->dash = curDash;
   arc_ptr->style = lineStyle;

   arc_ptr->xc = ABS_X(xc);
   arc_ptr->yc = ABS_Y(yc);
   arc_ptr->x1 = ABS_X(x1);
   arc_ptr->y1 = ABS_Y(y1);
   arc_ptr->x2 = ABS_X(x2);
   arc_ptr->y2 = ABS_Y(y2);
   arc_ptr->dir = dir;
   arc_ptr->ltx = ABS_X(xc-w/2);
   arc_ptr->lty = ABS_Y(yc-h/2);
   arc_ptr->w = (arc_ptr->xc-arc_ptr->ltx)<<1;
   arc_ptr->h = (arc_ptr->yc-arc_ptr->lty)<<1;
   arc_ptr->angle1 = arc_ptr->a_angle1 = angle1;
   arc_ptr->angle2 = arc_ptr->a_angle2 = angle2;
   arc_ptr->rotated_n = 0;
   arc_ptr->rotated_vlist = NULL;
   arc_ptr->rotated_asn = 0;
   arc_ptr->rotated_asvlist = NULL;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));
   obj_ptr->detail.a = arc_ptr;

   obj_ptr->type = OBJ_ARC;
   obj_ptr->color = colorIndex;
   obj_ptr->id = objId++;
   obj_ptr->dirty = FALSE;
   obj_ptr->rotation = 0;
   obj_ptr->locked = FALSE;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;
   obj_ptr->invisible = FALSE;

   AdjObjSplineVs(obj_ptr);
   AdjObjBBox(obj_ptr);
   AddObj(NULL, topObj, obj_ptr);

   return obj_ptr;
}

static XComposeStatus c_stat;
 
static
void ContinueArc(OrigX, OrigY)
   int OrigX, OrigY;
{
   int grid_x, grid_y, first_x=0, first_y=0;
   int end_x, end_y, saved_x, saved_y, dx, dy, radius;
   int done=FALSE, drawing_arc=FALSE;
   int dir=INVALID, ltx, lty, w, h, angle1, angle2=0;
   char buf[80], r_buf[80], x_buf[80], y_buf[80];
   struct ObjRec *obj_ptr;
   XGCValues values;
   XEvent input, ev;
   XMotionEvent *motion_ev;

   values.foreground = xorColorPixels[colorIndex];
   values.function = GXxor;
   values.fill_style = FillSolid;
#ifdef NO_THIN_LINE
   values.line_width = 1;
#else
   values.line_width = 0;
#endif
   values.line_style = LineSolid;
   XChangeGC(mainDisplay, drawGC,
         GCForeground | GCFunction | GCFillStyle | GCLineWidth | GCLineStyle,
         &values);

   grid_x = saved_x = OrigX;
   grid_y = saved_y = OrigY; 
   XDrawLine(mainDisplay, drawWindow, drawGC, OrigX, OrigY, saved_x, saved_y);

   PixelToMeasurementUnit(r_buf, 0);
   PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
   PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
   sprintf(buf, "r=%s,[%s,%s]", r_buf, x_buf, y_buf);
   StartShowMeasureCursor(grid_x, grid_y, buf, TRUE);
   XGrabPointer(mainDisplay, drawWindow, FALSE,
         PointerMotionMask | ButtonPressMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   
   Msg("Please specify the start of an arc.");
   SetMouseStatus("Set start of arc", "Cancel", "Cancel");
   while (!done) {
      XNextEvent(mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.type == ButtonPress) {
         if (input.xbutton.button != Button1) {
            XUngrabPointer(mainDisplay, CurrentTime);
            if (drawing_arc) {
               XDrawArc(mainDisplay, drawWindow, drawGC, ltx, lty, w, h,
                     angle1, angle2);
               PixelToMeasurementUnit(r_buf, w>>1);
               sprintf(buf, "r=%s,degree=%1d", r_buf, abs(angle2>>6));
            } else {
               XDrawLine(mainDisplay, drawWindow, drawGC, OrigX, OrigY,
                     saved_x, saved_y);
               dx = saved_x - OrigX;
               dy = saved_y - OrigY;
               radius = (int)(sqrt((double)(((double)dx)*((double)dx) +
                     ((double)dy)*((double)dy))));
               w = h = (radius<<1);
               PixelToMeasurementUnit(r_buf, radius);
               PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
               PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
               sprintf(buf, "r=%s,[%s,%s]", r_buf, x_buf, y_buf);
            }
            EndShowMeasureCursor(saved_x, saved_y, buf, TRUE);
            done = TRUE;
            angle2 = 0;
            Msg("");
         } else {
            if (drawing_arc) {
               XUngrabPointer(mainDisplay, CurrentTime);
               XDrawArc(mainDisplay, drawWindow, drawGC, ltx, lty, w, h,
                     angle1, angle2);
               PixelToMeasurementUnit(r_buf, w>>1);
               sprintf(buf, "r=%s,degree=%1d", r_buf, abs(angle2>>6));
               EndShowMeasureCursor(saved_x, saved_y, buf, TRUE);
               done = TRUE;
               Msg("");
            } else {
               dx = saved_x - OrigX;
               dy = saved_y - OrigY;
               radius = (int)(sqrt((double)(((double)dx)*((double)dx) +
                     ((double)dy)*((double)dy))));
               w = h = (radius<<1);
               PixelToMeasurementUnit(r_buf, radius);
               PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
               PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
               sprintf(buf, "r=%s,[%s,%s]", r_buf, x_buf, y_buf);
               ShowMeasureCursor(saved_x, saved_y, buf, TRUE);
               XDrawLine(mainDisplay, drawWindow, drawGC, OrigX, OrigY,
                     saved_x, saved_y);
               first_x = saved_x;
               first_y = saved_y;
               drawing_arc = TRUE;
               if (OrigX == grid_x && OrigY == grid_y) {
                  /* fake it as if the 1st point is ok but not the 2nd point */
                  XUngrabPointer(mainDisplay, CurrentTime);
                  grid_x = first_x;
                  grid_y = first_y;
                  done = TRUE;
               }
               SetMouseStatus("Set end of arc", "Cancel", "Cancel");
               Msg("Please specify the end of the arc.");
               PixelToMeasurementUnit(r_buf, radius);
               sprintf(buf, "r=%s,degree=0", r_buf);
               ShowMeasureCursor(saved_x, saved_y, buf, TRUE);
               if (done) {
                  PixelToMeasurementUnit(r_buf, radius);
                  sprintf(buf, "r=%s,degree=0", r_buf);
                  EndShowMeasureCursor(saved_x, saved_y, buf, TRUE);
               }
            }
         }
      } else if (input.type == MotionNotify) {
         if (drawing_arc) {
            PixelToMeasurementUnit(r_buf, w>>1);
            sprintf(buf, "r=%s,degree=%1d", r_buf, abs(angle2>>6));
            ShowMeasureCursor(saved_x, saved_y, buf, TRUE);
         } else {
            dx = saved_x - OrigX;
            dy = saved_y - OrigY;
            radius = (int)(sqrt((double)(((double)dx)*((double)dx) +
                  ((double)dy)*((double)dy))));
            PixelToMeasurementUnit(r_buf, radius);
            PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
            sprintf(buf, "r=%s,[%s,%s]", r_buf, x_buf, y_buf);
            ShowMeasureCursor(saved_x, saved_y, buf, TRUE);
         }
         motion_ev = &(input.xmotion);
         end_x = motion_ev->x;
         end_y = motion_ev->y;
         GridXY (end_x, end_y, &grid_x, &grid_y);
         if (grid_x != saved_x || grid_y != saved_y) {
            if (drawing_arc) {
               /* finished with the center and the first point on the arc */
               if (dir == INVALID) {
                  dir = ArcDirection(OrigX, OrigY, first_x, first_y,
                        grid_x, grid_y);
                  ltx = OrigX; lty = OrigY; w = 0; h = 0; angle1 = angle2 = 0;
                  if (dir == ARC_CW) {
                     sprintf(gszMsgBox,
                           "Please specify the end of the arc.  (%s).",
                           "clockwise");
                  } else {
                     sprintf(gszMsgBox,
                           "Please specify the end of the arc.  (%s).",
                           "counter-clockwise");
                  }
                  Msg(gszMsgBox);
               }
               XDrawArc(mainDisplay, drawWindow, drawGC, ltx, lty, w, h,
                     angle1, angle2);
               saved_x = grid_x;
               saved_y = grid_y;
               PointsToArc(OrigX, OrigY, first_x, first_y, saved_x, saved_y,
                     dir, TRUE, &ltx, &lty, &w, &h, &angle1, &angle2);
               XDrawArc(mainDisplay, drawWindow, drawGC, ltx, lty, w, h,
                     angle1, angle2);
            } else {
               /* looking for the first point on the arc */
               XDrawLine(mainDisplay, drawWindow, drawGC, OrigX, OrigY,
                     saved_x, saved_y);
               saved_x = grid_x;
               saved_y = grid_y;
               XDrawLine(mainDisplay, drawWindow, drawGC, OrigX, OrigY,
                     saved_x, saved_y);
            }
         }
         if (drawing_arc) {
            PixelToMeasurementUnit(r_buf, w>>1);
            sprintf(buf, "r=%s,degree=%1d", r_buf, abs(angle2>>6));
            ShowMeasureCursor(saved_x, saved_y, buf, TRUE);
         } else {
            dx = saved_x - OrigX;
            dy = saved_y - OrigY;
            radius = (int)(sqrt((double)(((double)dx)*((double)dx) +
                  ((double)dy)*((double)dy))));
            PixelToMeasurementUnit(r_buf, radius);
            PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
            sprintf(buf, "r=%s,[%s,%s]", r_buf, x_buf, y_buf);
            ShowMeasureCursor(saved_x, saved_y, buf, TRUE);
         }
         MarkRulers(grid_x, grid_y);
         while (XCheckMaskEvent(mainDisplay, PointerMotionMask, &ev)) ;
      } else if (input.type == KeyPress) {
         KeySym key_sym;
         char s[80];

         XLookupString(&(input.xkey), s, 80-1, &key_sym, &c_stat);
         TranslateKeys(s, &key_sym);
         if (s[0] == '\033' && (key_sym & 0xff) == '\033') {
            XUngrabPointer(mainDisplay, CurrentTime);
            if (drawing_arc) {
               XDrawArc(mainDisplay, drawWindow, drawGC, ltx, lty, w, h,
                     angle1, angle2);
               PixelToMeasurementUnit(r_buf, w>>1);
               sprintf(buf, "r=%s,degree=%1d", r_buf, abs(angle2>>6));
            } else {
               XDrawLine(mainDisplay, drawWindow, drawGC, OrigX, OrigY,
                     saved_x, saved_y);
               dx = saved_x - OrigX;
               dy = saved_y - OrigY;
               radius = (int)(sqrt((double)(((double)dx)*((double)dx) +
                     ((double)dy)*((double)dy))));
               w = h = (radius<<1);
               PixelToMeasurementUnit(r_buf, radius);
               PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
               PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
               sprintf(buf, "r=%s,[%s,%s]", r_buf, x_buf, y_buf);
            }
            EndShowMeasureCursor(saved_x, saved_y, buf, TRUE);
            done = TRUE;
            angle2 = 0;
            Msg("");
         }
      }
   }
   SetMouseStatus(NULL, NULL, NULL);
   if (angle2 == 0) {
      Msg("No arc created.");
   } else {
      obj_ptr = CreateArcObj(OrigX, OrigY, first_x, first_y, saved_x,
            saved_y, dir, ltx, lty, w, h, angle1, angle2);
      RecordNewObjCmd();
      DrawArcObj(drawWindow, drawOrigX, drawOrigY, topObj);
      arcDrawn = TRUE;
      SetFileModified(TRUE);
   }
}

void DrawArc(input)
   XEvent *input;
{
   XButtonEvent *button_ev;
   int mouse_x, mouse_y, grid_x, grid_y;

   if (input->type != ButtonPress) return;

   button_ev = &(input->xbutton);
   if (button_ev->button == Button1) {
      mouse_x = button_ev->x;
      mouse_y = button_ev->y;
      GridXY(mouse_x, mouse_y, &grid_x, &grid_y);
      SaveStatusStrings();
      ContinueArc(grid_x, grid_y);
      RestoreStatusStrings();
   }
}

void SaveArcObj(FP, ObjPtr)
   FILE *FP;
   register struct ObjRec *ObjPtr;
{
   register struct ArcRec *arc_ptr=ObjPtr->detail.a;

   if (fprintf(FP, "arc('%s',", colorMenuItems[ObjPtr->color]) == EOF)
      writeFileFailed = TRUE;
   if (fprintf(FP, "%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,\
%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,'%s','%s','%s',",
         arc_ptr->fill, arc_ptr->width, arc_ptr->pen, arc_ptr->dash,
         arc_ptr->ltx, arc_ptr->lty, arc_ptr->xc, arc_ptr->yc,
         arc_ptr->x1, arc_ptr->y1, arc_ptr->x2, arc_ptr->y2,
         arc_ptr->dir, arc_ptr->w, arc_ptr->h, arc_ptr->angle1, arc_ptr->angle2,
         ObjPtr->id, ObjPtr->rotation, arc_ptr->style, arc_ptr->aw,
         arc_ptr->ah, ObjPtr->locked, ObjPtr->ctm!=NULL,
         ObjPtr->invisible, arc_ptr->width_spec, arc_ptr->aw_spec,
         arc_ptr->ah_spec) == EOF) {
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

#define GETVALUE(val,name) ScanValue("%d", &(val), name, "arc")
#define GETSTRNG(val,name) ScanValue("%s", (val), name, "arc")

void ReadArcObj(FP, Inbuf, ObjPtr)
   FILE *FP;
   char *Inbuf;
   struct ObjRec **ObjPtr;
{
   register struct ArcRec *arc_ptr;
   char color_str[40], * s;
   char width_spec[40], aw_spec[40], ah_spec[40];
   int fill, width=0, pen, dash, ltx, lty, w, h, id=0;
   int rotation, new_alloc, style, locked=FALSE;
   int aw=origArrowHeadW[0], ah=origArrowHeadH[0];
   int xc, yc, x1, y1, x2, y2, dir, angle1, angle2;
   int invisible=FALSE, transformed=FALSE;

   *ObjPtr = NULL;

   s = FindChar((int)'(', Inbuf);
   s = ParseStr(s, (int)',', color_str, sizeof(color_str));

   InitScan(s, ", \t\n");

   style = LS_PLAIN;
   rotation = 0;
   *width_spec = *aw_spec = *ah_spec = '\0';
   if (fileVersion <= 8) {
      *ObjPtr = NULL;
      sprintf(gszMsgBox, "Invalid arc version (%1d).", fileVersion);
      if (PRTGIF) {
         fprintf(stderr, "%s\n", gszMsgBox);
      } else {
         Msg(gszMsgBox);
      }
      return;
   } else if (fileVersion <= 13) {
      if (GETVALUE(fill,   "fill") == INVALID ||
          GETVALUE(width,  "width") == INVALID ||
          GETVALUE(pen,    "pen") == INVALID ||
          GETVALUE(dash,   "dash") == INVALID ||
          GETVALUE(ltx,    "ltx") == INVALID ||
          GETVALUE(lty,    "lty") == INVALID ||
          GETVALUE(xc,     "xc") == INVALID ||
          GETVALUE(yc,     "yc") == INVALID ||
          GETVALUE(x1,     "x1") == INVALID ||
          GETVALUE(y1,     "y1") == INVALID ||
          GETVALUE(x2,     "x2") == INVALID ||
          GETVALUE(y2,     "y2") == INVALID ||
          GETVALUE(dir,    "direction") == INVALID ||
          GETVALUE(w,      "width") == INVALID ||
          GETVALUE(h,      "height") == INVALID ||
          GETVALUE(angle1, "angle1") == INVALID ||
          GETVALUE(angle2, "angle2") == INVALID ||
          GETVALUE(id,     "id") == INVALID) {
         return;
      }
      if (id >= objId) objId = id+1;
   } else if (fileVersion <= 15) {
      if (GETVALUE(fill,     "fill") == INVALID ||
          GETVALUE(width,    "width") == INVALID ||
          GETVALUE(pen,      "pen") == INVALID ||
          GETVALUE(dash,     "dash") == INVALID ||
          GETVALUE(ltx,      "ltx") == INVALID ||
          GETVALUE(lty,      "lty") == INVALID ||
          GETVALUE(xc,       "xc") == INVALID ||
          GETVALUE(yc,       "yc") == INVALID ||
          GETVALUE(x1,       "x1") == INVALID ||
          GETVALUE(y1,       "y1") == INVALID ||
          GETVALUE(x2,       "x2") == INVALID ||
          GETVALUE(y2,       "y2") == INVALID ||
          GETVALUE(dir,      "direction") == INVALID ||
          GETVALUE(w,        "width") == INVALID ||
          GETVALUE(h,        "height") == INVALID ||
          GETVALUE(angle1,   "angle1") == INVALID ||
          GETVALUE(angle2,   "angle2") == INVALID ||
          GETVALUE(id,       "id") == INVALID ||
          GETVALUE(rotation, "rotation") == INVALID) {
         return;
      }
      if (id >= objId) objId = id+1;
   } else if (fileVersion <= 16) {
      if (GETVALUE(fill,     "fill") == INVALID ||
          GETVALUE(width,    "width") == INVALID ||
          GETVALUE(pen,      "pen") == INVALID ||
          GETVALUE(dash,     "dash") == INVALID ||
          GETVALUE(ltx,      "ltx") == INVALID ||
          GETVALUE(lty,      "lty") == INVALID ||
          GETVALUE(xc,       "xc") == INVALID ||
          GETVALUE(yc,       "yc") == INVALID ||
          GETVALUE(x1,       "x1") == INVALID ||
          GETVALUE(y1,       "y1") == INVALID ||
          GETVALUE(x2,       "x2") == INVALID ||
          GETVALUE(y2,       "y2") == INVALID ||
          GETVALUE(dir,      "direction") == INVALID ||
          GETVALUE(w,        "width") == INVALID ||
          GETVALUE(h,        "height") == INVALID ||
          GETVALUE(angle1,   "angle1") == INVALID ||
          GETVALUE(angle2,   "angle2") == INVALID ||
          GETVALUE(id,       "id") == INVALID ||
          GETVALUE(rotation, "rotation") == INVALID ||
          GETVALUE(style,    "style") == INVALID) {
         return;
      }
      if (id >= objId) objId = id+1;
   } else if (fileVersion <= 25) {
      if (GETVALUE(fill,     "fill") == INVALID ||
          GETVALUE(width,    "width") == INVALID ||
          GETVALUE(pen,      "pen") == INVALID ||
          GETVALUE(dash,     "dash") == INVALID ||
          GETVALUE(ltx,      "ltx") == INVALID ||
          GETVALUE(lty,      "lty") == INVALID ||
          GETVALUE(xc,       "xc") == INVALID ||
          GETVALUE(yc,       "yc") == INVALID ||
          GETVALUE(x1,       "x1") == INVALID ||
          GETVALUE(y1,       "y1") == INVALID ||
          GETVALUE(x2,       "x2") == INVALID ||
          GETVALUE(y2,       "y2") == INVALID ||
          GETVALUE(dir,      "direction") == INVALID ||
          GETVALUE(w,        "width") == INVALID ||
          GETVALUE(h,        "height") == INVALID ||
          GETVALUE(angle1,   "angle1") == INVALID ||
          GETVALUE(angle2,   "angle2") == INVALID ||
          GETVALUE(id,       "id") == INVALID ||
          GETVALUE(rotation, "rotation") == INVALID ||
          GETVALUE(style,    "style") == INVALID ||
          GETVALUE(aw,       "arrow head w") == INVALID ||
          GETVALUE(ah,       "arrow head h") == INVALID) {
         return;
      }
      if (id >= objId) objId = id+1;
   } else if (fileVersion <= 32) {
      if (GETVALUE(fill,     "fill") == INVALID ||
          GETVALUE(width,    "width") == INVALID ||
          GETVALUE(pen,      "pen") == INVALID ||
          GETVALUE(dash,     "dash") == INVALID ||
          GETVALUE(ltx,      "ltx") == INVALID ||
          GETVALUE(lty,      "lty") == INVALID ||
          GETVALUE(xc,       "xc") == INVALID ||
          GETVALUE(yc,       "yc") == INVALID ||
          GETVALUE(x1,       "x1") == INVALID ||
          GETVALUE(y1,       "y1") == INVALID ||
          GETVALUE(x2,       "x2") == INVALID ||
          GETVALUE(y2,       "y2") == INVALID ||
          GETVALUE(dir,      "direction") == INVALID ||
          GETVALUE(w,        "width") == INVALID ||
          GETVALUE(h,        "height") == INVALID ||
          GETVALUE(angle1,   "angle1") == INVALID ||
          GETVALUE(angle2,   "angle2") == INVALID ||
          GETVALUE(id,       "id") == INVALID ||
          GETVALUE(rotation, "rotation") == INVALID ||
          GETVALUE(style,    "style") == INVALID ||
          GETVALUE(aw,       "arrow head w") == INVALID ||
          GETVALUE(ah,       "arrow head h") == INVALID ||
          GETVALUE(locked,   "locked") == INVALID) {
         return;
      }
      if (id >= objId) objId = id+1;
   } else {
#ifdef _TGIF_DBG
      if (GETVALUE(fill,        "fill") == INVALID ||
          GETVALUE(width,       "width") == INVALID ||
          GETVALUE(pen,         "pen") == INVALID ||
          GETVALUE(dash,        "dash") == INVALID ||
          GETVALUE(ltx,         "ltx") == INVALID ||
          GETVALUE(lty,         "lty") == INVALID ||
          GETVALUE(xc,          "xc") == INVALID ||
          GETVALUE(yc,          "yc") == INVALID ||
          GETVALUE(x1,          "x1") == INVALID ||
          GETVALUE(y1,          "y1") == INVALID ||
          GETVALUE(x2,          "x2") == INVALID ||
          GETVALUE(y2,          "y2") == INVALID ||
          GETVALUE(dir,         "direction") == INVALID ||
          GETVALUE(w,           "width") == INVALID ||
          GETVALUE(h,           "height") == INVALID ||
          GETVALUE(angle1,      "angle1") == INVALID ||
          GETVALUE(angle2,      "angle2") == INVALID ||
          GETVALUE(id,          "id") == INVALID ||
          GETVALUE(rotation,    "rotation") == INVALID ||
          GETVALUE(style,       "style") == INVALID ||
          GETVALUE(aw,          "arrow head w") == INVALID ||
          GETVALUE(ah,          "arrow head h") == INVALID ||
          GETVALUE(locked,      "locked") == INVALID ||
          GETVALUE(transformed, "transformed") == INVALID ||
          GETVALUE(invisible,   "invisible") == INVALID) {
         return;
      }
      if (id >= objId) objId = id+1;
      if (GETSTRNG(width_spec,  "width_spec") != INVALID) {
         UtilRemoveQuotes(width_spec);
      } else {
         sprintf(width_spec, "%1d", width);
         fprintf(stderr, "Bad width_spec in arc... Fix this file!\n");
      }
      if (GETSTRNG(aw_spec,     "aw_spec") != INVALID) {
         UtilRemoveQuotes(aw_spec);
      } else {
         sprintf(aw_spec, "%1d", aw);
         fprintf(stderr, "Bad width_spec in arc... Fix this file!\n");
      }
      if (GETSTRNG(ah_spec,     "ah_spec") != INVALID) {
         UtilRemoveQuotes(ah_spec);
      } else {
         sprintf(ah_spec, "%1d", ah);
         fprintf(stderr, "Bad width_spec in arc... Fix this file!\n");
      }
#else /* ~_TGIF_DBG */
      if (GETVALUE(fill,        "fill") == INVALID ||
          GETVALUE(width,       "width") == INVALID ||
          GETVALUE(pen,         "pen") == INVALID ||
          GETVALUE(dash,        "dash") == INVALID ||
          GETVALUE(ltx,         "ltx") == INVALID ||
          GETVALUE(lty,         "lty") == INVALID ||
          GETVALUE(xc,          "xc") == INVALID ||
          GETVALUE(yc,          "yc") == INVALID ||
          GETVALUE(x1,          "x1") == INVALID ||
          GETVALUE(y1,          "y1") == INVALID ||
          GETVALUE(x2,          "x2") == INVALID ||
          GETVALUE(y2,          "y2") == INVALID ||
          GETVALUE(dir,         "direction") == INVALID ||
          GETVALUE(w,           "width") == INVALID ||
          GETVALUE(h,           "height") == INVALID ||
          GETVALUE(angle1,      "angle1") == INVALID ||
          GETVALUE(angle2,      "angle2") == INVALID ||
          GETVALUE(id,          "id") == INVALID ||
          GETVALUE(rotation,    "rotation") == INVALID ||
          GETVALUE(style,       "style") == INVALID ||
          GETVALUE(aw,          "arrow head w") == INVALID ||
          GETVALUE(ah,          "arrow head h") == INVALID ||
          GETVALUE(locked,      "locked") == INVALID ||
          GETVALUE(transformed, "transformed") == INVALID ||
          GETVALUE(invisible,   "invisible") == INVALID ||
          GETSTRNG(width_spec,  "width_spec") == INVALID ||
          GETSTRNG(aw_spec,     "aw_spec") == INVALID ||
          GETSTRNG(ah_spec,     "ah_spec") == INVALID) {
         return;
      }
      if (id >= objId) objId = id+1;
      UtilRemoveQuotes(width_spec);
      UtilRemoveQuotes(aw_spec);
      UtilRemoveQuotes(ah_spec);
#endif /* _TGIF_DBG */
   }

   if (dir == ARC_CCW && angle2 < 0) {
      sprintf(gszMsgBox, "Warning:  Inconsistent arc direction.  Corrected.");
      if (PRTGIF) {
         fprintf(stderr, "%s\n", gszMsgBox);
      } else {
         Msg(gszMsgBox);
      }
      SetFileModified(TRUE);
      dir = ARC_CW;
   } else if (dir == ARC_CW && angle2 > 0) {
      sprintf(gszMsgBox, "Warning:  Inconsistent arc direction.  Corrected.");
      if (PRTGIF) {
         fprintf(stderr, "%s\n", gszMsgBox);
      } else {
         Msg(gszMsgBox);
      }
      SetFileModified(TRUE);
      dir = ARC_CCW;
   }

   if (fileVersion <= 16 && width <= 6) {
      aw = origArrowHeadW[width];
      ah = origArrowHeadH[width];
      width = origWidthOfLine[width];
   }
   if (fileVersion <= 32) {
      sprintf(width_spec, "%1d", width);
      sprintf(aw_spec, "%1d", aw);
      sprintf(ah_spec, "%1d", ah);
   }
   fill = UpgradePenFill(fill);
   pen = UpgradePenFill(pen);

   *ObjPtr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));
   arc_ptr = (struct ArcRec *)malloc(sizeof(struct ArcRec));
   if (arc_ptr == NULL) FailAllocMessage();
   memset(arc_ptr, 0, sizeof(struct ArcRec));

   arc_ptr->fill = fill;
   arc_ptr->width = width;
   arc_ptr->aw = aw;
   arc_ptr->ah = ah;
   UtilStrCpy(arc_ptr->width_spec, sizeof(arc_ptr->width_spec), width_spec);
   UtilStrCpy(arc_ptr->aw_spec, sizeof(arc_ptr->aw_spec), aw_spec);
   UtilStrCpy(arc_ptr->ah_spec, sizeof(arc_ptr->ah_spec), ah_spec);
   arc_ptr->pen = pen;
   arc_ptr->dash = dash;
   arc_ptr->style = style;

   arc_ptr->xc = xc;         arc_ptr->yc = yc;
   arc_ptr->x1 = x1;         arc_ptr->y1 = y1;
   arc_ptr->x2 = x2;         arc_ptr->y2 = y2;
   arc_ptr->dir = dir;
   arc_ptr->ltx = ltx;       arc_ptr->lty = lty;
   arc_ptr->w = w;           arc_ptr->h = h;
   arc_ptr->angle1 = arc_ptr->a_angle1 = angle1;
   arc_ptr->angle2 = arc_ptr->a_angle2 = angle2;

   arc_ptr->rotated_n = 0;
   arc_ptr->rotated_vlist = NULL;
   arc_ptr->rotated_asn = 0;
   arc_ptr->rotated_asvlist = NULL;

   (*ObjPtr)->detail.a = arc_ptr;

   (*ObjPtr)->type = OBJ_ARC;
   (*ObjPtr)->color = QuickFindColorIndex(*ObjPtr, color_str, &new_alloc, TRUE);
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->ctm = NULL;
   (*ObjPtr)->invisible = invisible;
   if (fileVersion >= 33 && transformed) {
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
   AdjObjSplineVs(*ObjPtr);
   AdjObjBBox(*ObjPtr);
}

void FreeArcObj(ObjPtr)
   struct ObjRec *ObjPtr;
{
   if (ObjPtr->detail.a->rotated_vlist != NULL) {
      free(ObjPtr->detail.a->rotated_vlist);
   }
   if (ObjPtr->detail.a->rotated_asvlist != NULL) {
      free(ObjPtr->detail.a->rotated_asvlist);
   }
   free(ObjPtr->detail.a);
   free(ObjPtr);
}

static
int ParseArcSpec(spec, seperator, radius, dir, theta1, theta2, error_str)
   char *spec, *error_str;
   int seperator, *radius, *dir, *theta1, *theta2;
{
   char *s, buf[MAXSTRING], tmp_buf[MAXSTRING];

   strcpy(tmp_buf, spec);

   s = ParseStr(tmp_buf, seperator, buf, sizeof(buf));
   if (*s != '\0' && (*radius = atoi(buf)) <= 0) *s = '\0';
   if (*s == '\0') {
      strcpy(error_str, "radius");
      return FALSE;
   }
   s = ParseStr(s, seperator, buf, sizeof(buf));
   switch (*buf) {
   case '+': *dir = ARC_CW; break;
   case '-': *dir = ARC_CCW; break;
   default: *s = '\0'; break;
   }
   if (*s == '\0') {
      strcpy(error_str, "dir");
      return FALSE;
   }
   s = ParseStr(s, seperator, buf, sizeof(buf));
   if (*s == '\0') {
      strcpy(error_str, "theta1");
      return FALSE;
   }
   *theta1 = atoi(buf);
   *theta2 = atoi(s);

   return TRUE;
}

void MakePreciseArc()
{
   int r = 0, dir = 0, x1, y1, x2, y2, theta1, theta2, angle2=0;
   char spec[MAXSTRING], error_str[MAXSTRING];
   double angle_in_radian;
   struct ObjRec *obj_ptr;

   Dialog("Arc specification: [radius,dir(+/-),theta1,theta2]", NULL, spec);
   UtilTrimBlanks(spec);
   if (*spec == '\0') return;

   TieLooseEnds();
   SetCurChoice(NOTHING);
   if (topSel!=NULL) { HighLightReverse(); RemoveAllSel(); }

   if (!ParseArcSpec(spec, (int)',', &r, &dir, &theta1, &theta2, error_str) &&
         !ParseArcSpec(spec, (int)' ', &r, &dir, &theta1, &theta2, error_str)) {
      sprintf(gszMsgBox, "Invalid %s in arc specification '%s'.",
            error_str, spec);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   switch (dir) {
   case ARC_CCW: theta1 += 90;        theta2 += 90;        break;
   case ARC_CW:  theta1 = -theta1+90; theta2 = -theta2+90; break;
   }

   angle_in_radian = theta1 * M_PI / 180;
   x1 = round(r*cos(angle_in_radian));
   y1 = -round(r*sin(angle_in_radian));
   angle_in_radian = theta2 * M_PI / 180;
   x2 = round(r*cos(angle_in_radian));
   y2 = -round(r*sin(angle_in_radian));

   while (theta1 < 0) theta1 += 360;
   while (theta2 > theta1) theta2 -= 360;
   while (theta2 < theta1) theta2 += 360;

   switch (dir) {
   case ARC_CCW:
      angle2 = theta2-theta1;
      if (angle2 == 0) angle2 = 360;
      break;
   case ARC_CW:
      angle2 = theta2-theta1-360;
      break;
   }
   obj_ptr = CreateArcObj(0, 0, x1, y1, x2, y2, dir, -r, -r, r*2, r*2,
         theta1*64, angle2*64);

   PlaceTopObj(obj_ptr);
   SelectTopObj();
   RecordNewObjCmd();
   SetFileModified(TRUE);
   justDupped = FALSE;
}

void PreciseRotateAnArc()
{
}
