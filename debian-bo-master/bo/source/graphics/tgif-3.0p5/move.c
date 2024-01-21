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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/move.c,v 3.0 1996/05/06 16:06:05 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "arc.e"
#include "attr.e"
#include "cmd.e"
#include "cursor.e"
#include "drawing.e"
#include "dup.e"
#include "grid.e"
#include "mainloop.e"
#include "mark.e"
#ifndef _NO_EXTERN
#include "move.e"
#endif
#include "msg.e"
#include "names.e"
#include "obj.e"
#include "oval.e"
#include "poly.e"
#include "raster.e"
#include "rcbox.e"
#include "rect.e"
#include "ruler.e"
#include "select.e"
#include "setup.e"
#include "spline.e"
#include "stretch.e"

int oneMotionTimeout=200;
int minMoveInterval=0;

static
void MovePoly (ObjPtr, Dx, Dy)
   register struct ObjRec	* ObjPtr;
   register int			Dx, Dy;
{
   register int	i;

   for (i = 0; i < ObjPtr->detail.p->n; i++)
   {
      ObjPtr->detail.p->vlist[i].x += Dx;
      ObjPtr->detail.p->vlist[i].y += Dy;
   }
}

static
void MovePolygon (ObjPtr, Dx, Dy)
   register struct ObjRec	* ObjPtr;
   register int			Dx, Dy;
{
   register int	i;

   for (i = 0; i < ObjPtr->detail.g->n; i++)
   {
      ObjPtr->detail.g->vlist[i].x += Dx;
      ObjPtr->detail.g->vlist[i].y += Dy;
   }
}

static
void MoveArc (ObjPtr, Dx, Dy)
   struct ObjRec	* ObjPtr;
   register int		Dx, Dy;
{
   register struct ArcRec	* arc_ptr = ObjPtr->detail.a;

   arc_ptr->xc += Dx;  arc_ptr->yc += Dy;
   arc_ptr->x1 += Dx;  arc_ptr->y1 += Dy;
   arc_ptr->x2 += Dx;  arc_ptr->y2 += Dy;
   arc_ptr->ltx += Dx; arc_ptr->lty += Dy;
}

void MoveObj (ObjPtr, Dx, Dy)
   struct ObjRec	* ObjPtr;
   int			Dx, Dy;
{
   struct ObjRec	* ptr;
   int			saved_undoing_or_redoing=undoingOrRedoing;

   ObjPtr->x += Dx;
   ObjPtr->y += Dy;
   ObjPtr->bbox.ltx += Dx;
   ObjPtr->bbox.lty += Dy;
   ObjPtr->bbox.rbx += Dx;
   ObjPtr->bbox.rby += Dy;
   ObjPtr->obbox.ltx += Dx;
   ObjPtr->obbox.lty += Dy;
   ObjPtr->obbox.rbx += Dx;
   ObjPtr->obbox.rby += Dy;
   MoveRotatedObjCache (ObjPtr, Dx, Dy);
   switch (ObjPtr->type)
   {
      case OBJ_POLY:
         MoveAttrs(ObjPtr->fattr, Dx, Dy);
         MovePoly (ObjPtr, Dx, Dy); 
         /* fake the undoingOrRedoing so that no */
         /*		actual auto-adjusting is done */
         undoingOrRedoing = TRUE;
         AdjObjSplineVs (ObjPtr);
         undoingOrRedoing = saved_undoing_or_redoing;
         break;
      case OBJ_BOX:
         MoveAttrs(ObjPtr->fattr, Dx, Dy);
         break;
      case OBJ_OVAL:
         MoveAttrs(ObjPtr->fattr, Dx, Dy);
         break;
      case OBJ_TEXT:
         break;
      case OBJ_POLYGON:
         MoveAttrs(ObjPtr->fattr, Dx, Dy);
         MovePolygon (ObjPtr, Dx, Dy);
         AdjObjSplineVs (ObjPtr);
         break;
      case OBJ_ARC:
         MoveAttrs(ObjPtr->fattr, Dx, Dy);
         MoveArc(ObjPtr, Dx, Dy);
         AdjObjSplineVs(ObjPtr);
         break;
      case OBJ_RCBOX:
         MoveAttrs(ObjPtr->fattr, Dx, Dy);
         break;
      case OBJ_XBM:
         MoveAttrs(ObjPtr->fattr, Dx, Dy);
         break;
      case OBJ_XPM:
         MoveAttrs(ObjPtr->fattr, Dx, Dy);
         break;
      case OBJ_SYM:
      case OBJ_ICON:
      case OBJ_GROUP:
         MoveAttrs(ObjPtr->fattr, Dx, Dy);
         for (ptr = ObjPtr->detail.r->first; ptr != NULL; ptr = ptr->next)
            MoveObj (ptr, Dx, Dy);
         break;
   }
}

void MoveAllSelObjects (Dx, Dy)
   register int	Dx, Dy;
{
   register struct SelRec	* sel_ptr;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
      if (!sel_ptr->obj->locked)
         MoveObj (sel_ptr->obj, Dx, Dy);
   if (numObjLocked != 0) Msg ("Locked objects are not moved.");
}

static
void MarkObjectsForMove ()
{
   register struct ObjRec	* obj_ptr;
   register struct SelRec	* sel_ptr;

   for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev)
      obj_ptr->marked = FALSE;

   for (sel_ptr = botSel; sel_ptr != NULL; sel_ptr = sel_ptr->prev)
      sel_ptr->obj->marked = TRUE;
}

static
int EndPtInObjList (XOff, YOff, FirstObjPtr)
   int			XOff, YOff;
   struct ObjRec	* FirstObjPtr;
   /* XOff and YOff are screen offsets */
{
   register struct ObjRec	* obj_ptr;
   register struct AttrRec	* attr_ptr;
   int				found, saved_fill;

   for (obj_ptr = FirstObjPtr; obj_ptr != NULL; obj_ptr = obj_ptr->next)
   {
      for (attr_ptr=obj_ptr->fattr; attr_ptr!=NULL; attr_ptr=attr_ptr->next)
         if (attr_ptr->shown &&
               XOff >= OFFSET_X(attr_ptr->obj->bbox.ltx)-3 &&
               YOff >= OFFSET_Y(attr_ptr->obj->bbox.lty)-3 &&
               XOff <= OFFSET_X(attr_ptr->obj->bbox.rbx)+3 &&
               YOff <= OFFSET_Y(attr_ptr->obj->bbox.rby)+3)
            return (TRUE);

      if (XOff >= OFFSET_X(obj_ptr->bbox.ltx)-3 &&
            YOff >= OFFSET_Y(obj_ptr->bbox.lty)-3 &&
            XOff <= OFFSET_X(obj_ptr->bbox.rbx)+3 &&
            YOff <= OFFSET_Y(obj_ptr->bbox.rby)+3)
      {
         switch (obj_ptr->type)
         {
            case OBJ_TEXT:
               if (FindGoodText (XOff,YOff,obj_ptr)) return (TRUE); break;
            case OBJ_XBM:
               if (FindGoodXBm (XOff,YOff,obj_ptr)) return (TRUE); break;
            case OBJ_XPM:
               if (FindGoodXPm (XOff,YOff,obj_ptr)) return (TRUE); break;
            case OBJ_BOX:
               saved_fill = obj_ptr->detail.b->fill;
               obj_ptr->detail.b->fill = SOLIDPAT;
               found = FindGoodBox (XOff,YOff,obj_ptr);
               obj_ptr->detail.b->fill = saved_fill;
               if (found) return (TRUE);
               break;
            case OBJ_OVAL:
               saved_fill = obj_ptr->detail.o->fill;
               obj_ptr->detail.o->fill = SOLIDPAT;
               found = FindGoodOval (XOff,YOff,obj_ptr);
               obj_ptr->detail.o->fill = saved_fill;
               if (found) return (TRUE);
               break;
            case OBJ_POLY:
               if (FindGoodPoly (XOff,YOff,obj_ptr)) return (TRUE);
               break;
            case OBJ_POLYGON:
               saved_fill = obj_ptr->detail.g->fill;
               obj_ptr->detail.g->fill = SOLIDPAT;
               found = FindGoodPolygon (XOff,YOff,obj_ptr);
               obj_ptr->detail.g->fill = saved_fill;
               if (found) return (TRUE);
               break;
            case OBJ_ARC:
               if (FindGoodArc (XOff,YOff,obj_ptr)) return (TRUE);
               break;
            case OBJ_RCBOX:
               saved_fill = obj_ptr->detail.rcb->fill;
               obj_ptr->detail.rcb->fill = SOLIDPAT;
               found = FindGoodRCBox (XOff,YOff,obj_ptr);
               obj_ptr->detail.rcb->fill = saved_fill;
               if (found) return (TRUE);
               break;

            case OBJ_GROUP:
            case OBJ_SYM:
            case OBJ_ICON:
               if (EndPtInObjList (XOff,YOff,obj_ptr->detail.r->first))
                  return (TRUE);
               break;
         }
      }
   }
   return (FALSE);
}

int EndPtInSelected (XOff, YOff)
   int	XOff, YOff;
   /* XOff and YOff are screen offsets */
{
   register struct SelRec	* sel_ptr;
   register struct ObjRec	* obj_ptr;
   register struct AttrRec	* attr_ptr;
   int				found, saved_fill;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      obj_ptr = sel_ptr->obj;
      if (obj_ptr->locked) continue;

      for (attr_ptr=obj_ptr->fattr; attr_ptr!=NULL; attr_ptr=attr_ptr->next)
         if (attr_ptr->shown &&
               XOff >= OFFSET_X(attr_ptr->obj->bbox.ltx)-3 &&
               YOff >= OFFSET_Y(attr_ptr->obj->bbox.lty)-3 &&
               XOff <= OFFSET_X(attr_ptr->obj->bbox.rbx)+3 &&
               YOff <= OFFSET_Y(attr_ptr->obj->bbox.rby)+3)
            return (TRUE);

      if (XOff >= OFFSET_X(obj_ptr->bbox.ltx)-3 &&
            YOff >= OFFSET_Y(obj_ptr->bbox.lty)-3 &&
            XOff <= OFFSET_X(obj_ptr->bbox.rbx)+3 &&
            YOff <= OFFSET_Y(obj_ptr->bbox.rby)+3)
      {
         switch (obj_ptr->type)
         {
            case OBJ_TEXT:
               if (FindGoodText (XOff,YOff,obj_ptr)) return (TRUE); break;
            case OBJ_XBM:
               if (FindGoodXBm (XOff,YOff,obj_ptr)) return (TRUE); break;
            case OBJ_XPM:
               if (FindGoodXPm (XOff,YOff,obj_ptr)) return (TRUE); break;
            case OBJ_BOX:
               saved_fill = obj_ptr->detail.b->fill;
               obj_ptr->detail.b->fill = SOLIDPAT;
               found = FindGoodBox (XOff,YOff,obj_ptr);
               obj_ptr->detail.b->fill = saved_fill;
               if (found) return (TRUE);
               break;
            case OBJ_OVAL:
               saved_fill = obj_ptr->detail.o->fill;
               obj_ptr->detail.o->fill = SOLIDPAT;
               found = FindGoodOval (XOff,YOff,obj_ptr);
               obj_ptr->detail.o->fill = saved_fill;
               if (found) return (TRUE);
               break;
            case OBJ_POLY:
               if (FindGoodPoly (XOff,YOff,obj_ptr)) return (TRUE);
               break;
            case OBJ_POLYGON:
               saved_fill = obj_ptr->detail.g->fill;
               obj_ptr->detail.g->fill = SOLIDPAT;
               found = FindGoodPolygon (XOff,YOff,obj_ptr);
               obj_ptr->detail.g->fill = saved_fill;
               if (found) return (TRUE);
               break;
            case OBJ_ARC:
               if (FindGoodArc (XOff,YOff,obj_ptr)) return (TRUE);
               break;
            case OBJ_RCBOX:
               saved_fill = obj_ptr->detail.rcb->fill;
               obj_ptr->detail.rcb->fill = SOLIDPAT;
               found = FindGoodRCBox (XOff,YOff,obj_ptr);
               obj_ptr->detail.rcb->fill = saved_fill;
               if (found) return (TRUE);
               break;

            case OBJ_GROUP:
            case OBJ_SYM:
            case OBJ_ICON:
               if (EndPtInObjList (XOff,YOff,obj_ptr->detail.r->first))
                  return (TRUE);
               break;
         }
      }
   }
   return (FALSE);
}

static
int ConstrainedMoveAllSel (Dx, Dy, ltx, lty, rbx, rby)
   register int	Dx, Dy;
   int		* ltx, * lty, * rbx, * rby;
{
   register struct ObjRec	* obj_ptr;
   int				something_stretched = FALSE, num_pts;
   int				x_off, y_off, move_first, move_last;
   IntPoint			* v;

   for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev)
   {
      if (!obj_ptr->marked && obj_ptr->type==OBJ_POLY && !obj_ptr->locked)
      {
         num_pts = obj_ptr->detail.p->n;
         v = obj_ptr->detail.p->vlist;

         x_off = OFFSET_X(v[0].x); y_off = OFFSET_Y(v[0].y);
         move_first = EndPtInSelected (x_off, y_off);
         x_off = OFFSET_X(v[num_pts-1].x); y_off = OFFSET_Y(v[num_pts-1].y);
         move_last = EndPtInSelected (x_off, y_off);

         if (move_first || move_last)
         {
            PrepareToReplaceAnObj (obj_ptr);
            if (something_stretched)
            {
               if (obj_ptr->bbox.ltx < *ltx) *ltx = obj_ptr->bbox.ltx;
               if (obj_ptr->bbox.lty < *lty) *lty = obj_ptr->bbox.lty;
               if (obj_ptr->bbox.rbx > *rbx) *rbx = obj_ptr->bbox.rbx;
               if (obj_ptr->bbox.rby > *rby) *rby = obj_ptr->bbox.rby;
            }
            else
            {
               *ltx = obj_ptr->bbox.ltx; *lty = obj_ptr->bbox.lty;
               *rbx = obj_ptr->bbox.rbx; *rby = obj_ptr->bbox.rby;
            }
            something_stretched = TRUE;
            if (move_first && move_last)
               MoveObj (obj_ptr, Dx, Dy);
            else
            {
               int	index=INVALID, seg_dx=0, seg_dy=0;
               int	cur_seg_dx=0, cur_seg_dy=0;

               if (move_first)
               {
                  if (num_pts > 2)
                  {
                     index = 1;
                     cur_seg_dx = v[index-1].x - v[index].x;
                     cur_seg_dy = v[index-1].y - v[index].y;
                     seg_dx = v[index].x - v[index+1].x;
                     seg_dy = v[index].y - v[index+1].y;
                  }
                  v[0].x += Dx; v[0].y += Dy;
               }
               else
               {
                  if (num_pts > 2)
                  {
                     index = num_pts-2;
                     cur_seg_dx = v[index+1].x - v[index].x;
                     cur_seg_dy = v[index+1].y - v[index].y;
                     seg_dx = v[index].x - v[index-1].x;
                     seg_dy = v[index].y - v[index-1].y;
                  }
                  v[num_pts-1].x += Dx; v[num_pts-1].y += Dy;
               }
               if (num_pts>2 && cur_seg_dy==0 && cur_seg_dx!=0 &&
                     (seg_dy!=0 || (seg_dy==0 && Dx==0)))
                  v[index].y += Dy;
               else if (num_pts>2 && cur_seg_dx==0 && cur_seg_dy!=0 &&
                     (seg_dx!=0 || (seg_dx==0 && Dy==0)))
                  v[index].x += Dx;
            }
            AdjObjSplineVs (obj_ptr);
            if (obj_ptr->detail.p->curved != LT_INTSPLINE)
               UpdPolyBBox (obj_ptr, num_pts, v);
            else
               UpdPolyBBox (obj_ptr, obj_ptr->detail.p->intn,
                     obj_ptr->detail.p->intvlist);
            if (AutoCenterAttr (obj_ptr))
            {
               struct AttrRec	* attr_ptr=obj_ptr->fattr;
               int		modified=FALSE;

               for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
                  if (attr_ptr->shown)
                  {
                     struct BBRec	bbox;

                     CenterObjInOBBox (attr_ptr->obj, obj_ptr->obbox,
                           &bbox);
                     if (bbox.ltx < *ltx) *ltx = bbox.ltx;
                     if (bbox.lty < *lty) *lty = bbox.lty;
                     if (bbox.rbx > *rbx) *rbx = bbox.rbx;
                     if (bbox.rby > *rby) *rby = bbox.rby;
                     modified = TRUE;
                  }
               if (modified) AdjObjBBox (obj_ptr);
            }
            if (obj_ptr->bbox.ltx < *ltx) *ltx = obj_ptr->bbox.ltx;
            if (obj_ptr->bbox.lty < *lty) *lty = obj_ptr->bbox.lty;
            if (obj_ptr->bbox.rbx > *rbx) *rbx = obj_ptr->bbox.rbx;
            if (obj_ptr->bbox.rby > *rby) *rby = obj_ptr->bbox.rby;
            RecordReplaceAnObj (obj_ptr);
         }
      }
   }
   MoveAllSelObjects (Dx, Dy);
   return (something_stretched);
}

void MoveAllSel (Dx, Dy)
   register int	Dx, Dy;
{
   int			ltx, lty, rbx, rby;
   struct MoveSubCmdRec	* move_cmd;
   struct SubCmdRec	* sub_cmd;

   move_cmd = (struct MoveSubCmdRec *)malloc(sizeof(struct MoveSubCmdRec));
   sub_cmd = (struct SubCmdRec *)malloc(sizeof(struct SubCmdRec));
   if (move_cmd == NULL || sub_cmd == NULL) FailAllocMessage();
   memset(move_cmd, 0, sizeof(struct MoveSubCmdRec));
   memset(sub_cmd, 0, sizeof(struct SubCmdRec));
   sub_cmd->detail.mv = move_cmd;
   move_cmd->dx = Dx;
   move_cmd->dy = Dy;

   if (moveMode==CONST_MOVE && !justDupped)
   {
      MarkObjectsForMove ();

      StartCompositeCmd ();
      PrepareToRecord (CMD_MOVE, NULL, NULL, 0);
      RecordCmd (CMD_MOVE, sub_cmd, topSel, botSel, numObjSelected);
      if (ConstrainedMoveAllSel (Dx, Dy, &ltx, &lty, &rbx, &rby))
      {
         ltx = min(ltx,min(selLtX,selLtX+Dx));
         lty = min(lty,min(selLtY,selLtY+Dy));
         rbx = max(rbx,max(selRbX,selRbX+Dx));
         rby = max(rby,max(selRbY,selRbY+Dy));
         RedrawAnArea (botObj, ltx-GRID_ABS_SIZE(1),
               lty-GRID_ABS_SIZE(1),
               rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1));
      }
      else
         RedrawAreas (botObj, selLtX-GRID_ABS_SIZE(1),
               selLtY-GRID_ABS_SIZE(1),
               selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1),
               selLtX-GRID_ABS_SIZE(1)+Dx, selLtY-GRID_ABS_SIZE(1)+Dy,
               selRbX+GRID_ABS_SIZE(1)+Dx, selRbY+GRID_ABS_SIZE(1)+Dy);
      EndCompositeCmd ();
   }
   else
   {
      MoveAllSelObjects (Dx, Dy);
      PrepareToRecord (CMD_MOVE, NULL, NULL, 0);
      RecordCmd (CMD_MOVE, sub_cmd, topSel, botSel, numObjSelected);
      RedrawAreas (botObj, selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1),
            selLtX-GRID_ABS_SIZE(1)+Dx, selLtY-GRID_ABS_SIZE(1)+Dy,
            selRbX+GRID_ABS_SIZE(1)+Dx, selRbY+GRID_ABS_SIZE(1)+Dy);
   }
   free(move_cmd);
   free(sub_cmd);
}

void MoveSel (OrigX, OrigY, ObjPtr, down_button_ev)
   int 			OrigX, OrigY;
   struct ObjRec	* ObjPtr;
   XButtonEvent		* down_button_ev;
{
   register int		i, num_pts = 0;
   struct PolyRec	* poly_ptr = NULL;
   struct PolygonRec	* polygon_ptr = NULL;
   struct ArcRec	* arc_ptr = NULL;
   struct ObjRec	* arc_obj_ptr = NULL;
   struct BBRec		bbox, o_bbox;
   IntPoint		*cntrlv=NULL, *polyv=NULL;
   XPoint		obj_obbox_vs[5], *v=NULL, *sv=NULL, *pv=NULL;
   int 			ltx = 0, lty = 0, rbx = 0, rby = 0, sn = 0;
   int			sel_ltx, sel_lty, sel_rbx, sel_rby, curved = FALSE;
   int			ruler_ltx, ruler_lty, ruler_rbx, ruler_rby;
   int			x, y, moving = TRUE, dx, dy;
   int			w = 0, h = 0, angle1 = 0, angle2 = 0;
   int			xc = 0, yc = 0, x1 = 0, y1 = 0, x2 = 0, y2 = 0;
   int			grid_x = OrigX, grid_y = OrigY, radius = 0;
   int			saved_xc = 0, saved_yc = 0, saved_x1 = 0, saved_y1 = 0;
   int			saved_x2 = 0, saved_y2 = 0;
   int			saved_ltx = 0, saved_lty = 0, intn=0;
   char			buf[80], x_buf[80], y_buf[80], * smooth=NULL;
   XEvent		input, ev;
   Time			down_click_time=(Time)0;

   if (down_button_ev != NULL) {
      down_click_time = down_button_ev->time;
   }
   if (numObjSelected == numObjLocked)
   {
      Msg ("Locked object(s) can not be moved.");
      return;
   }

   XFlush (mainDisplay);
   XSync (mainDisplay, False);

   if (XCheckMaskEvent (mainDisplay, ExposureMask, &ev) ||
         XCheckMaskEvent (mainDisplay, VisibilityChangeMask, &ev))
      ExposeEventHandler (&ev, TRUE);

   sel_ltx = OFFSET_X(selLtX) - 1; sel_lty = OFFSET_Y(selLtY) - 1;
   sel_rbx = OFFSET_X(selRbX) + 1; sel_rby = OFFSET_Y(selRbY) + 1;

   ruler_ltx = OFFSET_X(selObjLtX); ruler_lty = OFFSET_Y(selObjLtY);
   ruler_rbx = OFFSET_X(selObjRbX); ruler_rby = OFFSET_Y(selObjRbY);

   SelBox (drawWindow, revDefaultGC, sel_ltx, sel_lty, sel_rbx, sel_rby);
   PixelToMeasurementUnit(x_buf, 0);
   PixelToMeasurementUnit(y_buf, 0);
   sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
   StartShowMeasureCursor (OrigX, OrigY, buf, TRUE);
   BeginIntervalRulers (ruler_ltx, ruler_lty, ruler_rbx, ruler_rby);

   switch (ObjPtr->type)
   {
      case OBJ_BOX:
      case OBJ_XBM:
      case OBJ_XPM:
      case OBJ_TEXT:
         if (ObjPtr->ctm == NULL)
         {
            ltx = OFFSET_X(ObjPtr->obbox.ltx);
            lty = OFFSET_Y(ObjPtr->obbox.lty);
            rbx = OFFSET_X(ObjPtr->obbox.rbx);
            rby = OFFSET_Y(ObjPtr->obbox.rby);
            SelBox (drawWindow, revDefaultGC, ltx, lty, rbx, rby);
         }
         else
         {
            memcpy (obj_obbox_vs, ObjPtr->rotated_obbox, 5*sizeof(XPoint));
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, obj_obbox_vs, 5,
                  CoordModeOrigin);
         }
         break;
      case OBJ_RCBOX:
         if (ObjPtr->ctm == NULL) {
            ltx = OFFSET_X(ObjPtr->obbox.ltx);
            lty = OFFSET_Y(ObjPtr->obbox.lty);
            rbx = OFFSET_X(ObjPtr->obbox.rbx);
            rby = OFFSET_Y(ObjPtr->obbox.rby);
            radius = ObjPtr->detail.rcb->radius;
            SetRCBoxVertex (ltx, lty, rbx, rby, radius);
            MyRCBox (drawWindow, revDefaultGC, ltx, lty, rbx, rby, radius);
         } else {
            sn = ObjPtr->detail.rcb->rotated_n;
            sv = (XPoint*)malloc(sn*sizeof(XPoint));
            pv = (XPoint*)malloc(sn*sizeof(XPoint));
            if (sv == NULL || pv == NULL) FailAllocMessage();
            for (i=0; i < sn; i++) {
               pv[i].x = sv[i].x = ObjPtr->detail.rcb->rotated_vlist[i].x;
               pv[i].y = sv[i].y = ObjPtr->detail.rcb->rotated_vlist[i].y;
            }
            XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                  CoordModeOrigin);
         }
         break;
      case OBJ_ARC:
         if (ObjPtr->ctm == NULL) {
            arc_obj_ptr = DupObj(ObjPtr);
            arc_ptr = arc_obj_ptr->detail.a;
            ltx = OFFSET_X(arc_ptr->ltx); lty = OFFSET_Y(arc_ptr->lty);
            w = OFFSET_X(arc_ptr->ltx+arc_ptr->w)-ltx;
            h = OFFSET_Y(arc_ptr->lty+arc_ptr->h)-lty;
            angle1 = arc_ptr->angle1; angle2 = arc_ptr->angle2;
            xc = OFFSET_X(arc_ptr->xc); yc = OFFSET_Y(arc_ptr->yc);
            x1 = OFFSET_X(arc_ptr->x1); y1 = OFFSET_Y(arc_ptr->y1);
            ArcRealX2Y2(arc_ptr, &x2, &y2);
            x2 = OFFSET_X(x2); y2 = OFFSET_Y(y2);
            saved_xc = xc; saved_yc = yc;
            saved_x1 = x1; saved_y1 = y1;
            saved_x2 = x2; saved_y2 = y2;
            saved_ltx = ltx; saved_lty = lty;
            if (arc_ptr->fill != NONEPAT) {
               XDrawLine(mainDisplay, drawWindow, revDefaultGC, xc, yc, x1, y1);
               XDrawLine(mainDisplay, drawWindow, revDefaultGC, xc, yc, x2, y2);
            }
            XDrawArc(mainDisplay, drawWindow, revDefaultGC, ltx, lty, w, h,
                  angle1, angle2);
         } else {
            arc_ptr = ObjPtr->detail.a;
            sn = ObjPtr->detail.a->rotated_n;
            sv = (XPoint*)malloc((sn+2)*sizeof(XPoint));
            pv = (XPoint*)malloc((sn+2)*sizeof(XPoint));
            if (sv == NULL || pv == NULL) FailAllocMessage();
            for (i=0; i < sn+2; i++) {
               pv[i].x = sv[i].x = ObjPtr->detail.a->rotated_vlist[i].x;
               pv[i].y = sv[i].y = ObjPtr->detail.a->rotated_vlist[i].y;
            }
            if (arc_ptr->fill != NONEPAT) {
               XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn+2,
                     CoordModeOrigin);
            } else {
               XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                     CoordModeOrigin);
            }
         }
         break;
      case OBJ_OVAL:
         if (ObjPtr->ctm == NULL) {
            num_pts = 13;
            bbox.ltx = OFFSET_X(ObjPtr->obbox.ltx);
            bbox.lty = OFFSET_Y(ObjPtr->obbox.lty);
            bbox.rbx = OFFSET_X(ObjPtr->obbox.rbx);
            bbox.rby = OFFSET_Y(ObjPtr->obbox.rby);
            MyOval (drawWindow, revDefaultGC, bbox);
         } else {
            sn = ObjPtr->detail.o->rotated_n;
            sv = (XPoint*)malloc(sn*sizeof(XPoint));
            pv = (XPoint*)malloc(sn*sizeof(XPoint));
            if (sv == NULL || pv == NULL) FailAllocMessage();
            for (i=0; i < sn; i++) {
               pv[i].x = sv[i].x = ObjPtr->detail.o->rotated_vlist[i].x;
               pv[i].y = sv[i].y = ObjPtr->detail.o->rotated_vlist[i].y;
            }
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, sv, sn,
                  CoordModeOrigin);
         }
         break;
      case OBJ_POLY:
         poly_ptr = ObjPtr->detail.p;
         curved = poly_ptr->curved;
         num_pts = poly_ptr->n;
         if (splineRubberband) {
            polyv = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
            if (polyv == NULL) FailAllocMessage();
            if (curved != LT_INTSPLINE && poly_ptr->smooth != NULL) {
               smooth = (char*)malloc((num_pts+1)*sizeof(char));
               if (smooth == NULL) FailAllocMessage();
            }
            if (ObjPtr->ctm == NULL) {
               for (i = 0; i < num_pts; i++) {
                  polyv[i].x = poly_ptr->vlist[i].x;
                  polyv[i].y = poly_ptr->vlist[i].y;
                  if (smooth != NULL) smooth[i] = poly_ptr->smooth[i];
               }
            } else {
               for (i = 0; i < num_pts; i++) {
                  int x, y;

                  TransformPointThroughCTM(poly_ptr->vlist[i].x-ObjPtr->x,
                        poly_ptr->vlist[i].y-ObjPtr->y, ObjPtr->ctm, &x, &y);
                  polyv[i].x = x+ObjPtr->x;
                  polyv[i].y = y+ObjPtr->y;
                  if (smooth != NULL) smooth[i] = poly_ptr->smooth[i];
               }
            }
            if (curved != LT_INTSPLINE) {
               sv = MakeMultiSplinePolyVertex (&sn, smooth,
                     drawOrigX, drawOrigY, num_pts, polyv);
            } else {
               sv = MakeIntSplinePolyVertex (&sn, &intn, &cntrlv, drawOrigX,
                     drawOrigY, num_pts, polyv);
            }
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, sv, sn,
                  CoordModeOrigin);
         } else {
            v = (XPoint*)malloc((num_pts+1)*sizeof(XPoint));
            if (v == NULL) FailAllocMessage();
            for (i = 0; i < num_pts; i++) {
               v[i].x = OFFSET_X(poly_ptr->vlist[i].x);
               v[i].y = OFFSET_Y(poly_ptr->vlist[i].y);
            }
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, v, num_pts,
                  CoordModeOrigin);
         }
         break;
      case OBJ_POLYGON:
         polygon_ptr = ObjPtr->detail.g;
         curved = polygon_ptr->curved;
         num_pts = polygon_ptr->n;
         if (splineRubberband) {
            polyv = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
            if (polyv == NULL) FailAllocMessage();
            if (curved != LT_INTSPLINE && polygon_ptr->smooth != NULL) {
               smooth = (char*)malloc((num_pts+1)*sizeof(char));
               if (smooth == NULL) FailAllocMessage();
            }
            if (ObjPtr->ctm == NULL) {
               for (i = 0; i < num_pts; i++) {
                  polyv[i].x = polygon_ptr->vlist[i].x;
                  polyv[i].y = polygon_ptr->vlist[i].y;
                  if (smooth != NULL) smooth[i] = polygon_ptr->smooth[i];
               }
            } else {
               for (i = 0; i < num_pts; i++) {
                  int x, y;

                  TransformPointThroughCTM(polygon_ptr->vlist[i].x-ObjPtr->x,
                        polygon_ptr->vlist[i].y-ObjPtr->y, ObjPtr->ctm, &x, &y);
                  polyv[i].x = x+ObjPtr->x;
                  polyv[i].y = y+ObjPtr->y;
                  if (smooth != NULL) smooth[i] = polygon_ptr->smooth[i];
               }
            }
            if (curved != LT_INTSPLINE) {
               sv = MakeMultiSplinePolygonVertex (&sn, smooth,
                     drawOrigX, drawOrigY, num_pts, polyv);
            } else {
               sv = MakeIntSplinePolygonVertex (&sn, &intn, &cntrlv,
                     drawOrigX, drawOrigY, num_pts, polyv);
            }
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, sv, sn,
                  CoordModeOrigin);
         } else {
            v = (XPoint*)malloc((num_pts+1)*sizeof(XPoint));
            if (v == NULL) FailAllocMessage();
            for (i = 0; i < num_pts; i++) {
               v[i].x = OFFSET_X(polygon_ptr->vlist[i].x);
               v[i].y = OFFSET_Y(polygon_ptr->vlist[i].y);
            }
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, v, num_pts,
                  CoordModeOrigin);
         }
         break;
      case OBJ_GROUP:
      case OBJ_SYM:
      case OBJ_ICON:
         ltx = OFFSET_X(ObjPtr->obbox.ltx); lty = OFFSET_Y(ObjPtr->obbox.lty);
         rbx = OFFSET_X(ObjPtr->obbox.rbx); rby = OFFSET_Y(ObjPtr->obbox.rby);
         SelBox (drawWindow, revDefaultGC, ltx, lty, rbx, rby);
         break;
   }

   XGrabPointer (mainDisplay, drawWindow, FALSE,
         PointerMotionMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, moveCursor, CurrentTime);

   dx = dy = 0;

   while (moving)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == ButtonRelease)
      {
         Time release_time=input.xbutton.time;

         XUngrabPointer (mainDisplay, CurrentTime);
         XSync (mainDisplay, False);
         moving = FALSE;

         switch (ObjPtr->type)
         {
            case OBJ_BOX:
            case OBJ_XBM:
            case OBJ_XPM:
            case OBJ_TEXT:
               if (ObjPtr->ctm == NULL)
                  SelBox (drawWindow,revDefaultGC,ltx+dx,lty+dy,rbx+dx,rby+dy);
               else
               {
                  for (i=0; i < 5; i++)
                  {
                     obj_obbox_vs[i].x = ObjPtr->rotated_obbox[i].x+dx;
                     obj_obbox_vs[i].y = ObjPtr->rotated_obbox[i].y+dy;
                  }
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC,
                        obj_obbox_vs, 5, CoordModeOrigin);
               }
               break;
            case OBJ_RCBOX:
               if (ObjPtr->ctm == NULL) {
                  MyRCBox (drawWindow,revDefaultGC,ltx+dx,lty+dy,rbx+dx,rby+dy,
                        radius);
               } else {
                  for (i=0; i < sn; i++) {
                     sv[i].x = pv[i].x + dx;
                     sv[i].y = pv[i].y + dy;
                  }
                  XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                        CoordModeOrigin);
               }
               break;
            case OBJ_OVAL:
               if (ObjPtr->ctm == NULL)
               {
                  o_bbox.ltx = bbox.ltx + dx; o_bbox.lty = bbox.lty + dy;
                  o_bbox.rbx = bbox.rbx + dx; o_bbox.rby = bbox.rby + dy;
                  MyOval (drawWindow, revDefaultGC, o_bbox);
               }
               else
               {
                  for (i=0; i < sn; i++)
                  {
                     sv[i].x = pv[i].x + dx;
                     sv[i].y = pv[i].y + dy;
                  }
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC, sv, sn,
                        CoordModeOrigin);
               }
               break;
            case OBJ_ARC:
               if (ObjPtr->ctm == NULL) {
                  XDrawArc(mainDisplay, drawWindow, revDefaultGC, ltx, lty,
                        w, h, angle1, angle2);
                  if (arc_ptr->fill != NONEPAT) {
                     XDrawLine(mainDisplay,drawWindow,revDefaultGC,xc,yc,x1,y1);
                     XDrawLine(mainDisplay,drawWindow,revDefaultGC,xc,yc,x2,y2);
                  }
               } else {
                  for (i=0; i < sn+2; i++) {
                     sv[i].x = pv[i].x + dx;
                     sv[i].y = pv[i].y + dy;
                  }
                  if (arc_ptr->fill != NONEPAT) {
                     XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn+2,
                           CoordModeOrigin);
                  } else {
                     XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                           CoordModeOrigin);
                  }
               }
               break;
            case OBJ_POLY:
            case OBJ_POLYGON:
               if (splineRubberband)
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC, sv, sn,
                        CoordModeOrigin);
               else
               {
                  for (i = 0; i < num_pts; i++)
                  {
                     v[i].x += dx;
                     v[i].y += dy;
                  }
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC, v, num_pts,
                        CoordModeOrigin);
                  for (i = 0; i < num_pts; i++)
                  {
                     v[i].x -= dx;
                     v[i].y -= dy;
                  }
               }
               break;
            case OBJ_GROUP:
            case OBJ_SYM:
            case OBJ_ICON:
               SelBox (drawWindow,revDefaultGC,ltx+dx,lty+dy,rbx+dx,rby+dy);
               break;
         }
         EndIntervalRulers (grid_x, grid_y);
         PixelToMeasurementUnit(x_buf, ABS_SIZE(dx));
         PixelToMeasurementUnit(y_buf, ABS_SIZE(dy));
         sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
         EndShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         SelBox (drawWindow, revDefaultGC, sel_ltx+dx, sel_lty+dy, sel_rbx+dx,
               sel_rby+dy);

         dx = grid_x - OrigX;
         dy = grid_y - OrigY;

         switch (ObjPtr->type)
         {
            case OBJ_ARC:
               if (ObjPtr->ctm != NULL) {
                  free(sv);
                  free(pv);
               } else {
                  FreeArcObj(arc_obj_ptr);
               }
               break;

            case OBJ_RCBOX:
            case OBJ_OVAL:
               if (ObjPtr->ctm != NULL) {
                  free(sv);
                  free(pv);
               }
               break;

            case OBJ_POLY:
            case OBJ_POLYGON:
               if (splineRubberband) {
                  if (sv != NULL) free(sv);
                  if (polyv != NULL) free(polyv);
                  if (smooth != NULL) free(smooth);
                  if (curved == LT_INTSPLINE && cntrlv != NULL) free(cntrlv);
               } else {
                  free(v);
               }
               break;
         }
         if (oneMotionSelectMove && down_button_ev != NULL &&
               (release_time-down_click_time) < oneMotionTimeout) {
            dx = dy = 0;
         } else if (!oneMotionSelectMove && down_button_ev != NULL &&
               (release_time-down_click_time) < minMoveInterval) {
            dx = dy = 0;
         }
         if (dx != 0 || dy != 0)
         {
            HighLightReverse ();
            dx = ABS_SIZE(dx);
            dy = ABS_SIZE(dy);
            if (numObjSelected == numObjLocked)
            {
               HighLightForward ();
               return;
            }
            MoveAllSel (dx, dy);
            HighLightForward ();
            UpdSelBBox ();
            if (justDupped)
            {
               dupDx += dx;
               dupDy += dy;
            }
            SetFileModified (TRUE);
         }
      }
      else if (input.type == MotionNotify)
      {
         PixelToMeasurementUnit(x_buf, ABS_SIZE(dx));
         PixelToMeasurementUnit(y_buf, ABS_SIZE(dy));
         sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
         ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         x = input.xmotion.x;
         y = input.xmotion.y;
         GridXY (x, y, &grid_x, &grid_y);

         switch (ObjPtr->type)
         {
            case OBJ_BOX:
            case OBJ_XBM:
            case OBJ_XPM:
            case OBJ_TEXT:
               if (ObjPtr->ctm == NULL)
                  SelBox (drawWindow,revDefaultGC,ltx+dx,lty+dy,rbx+dx,rby+dy);
               else
               {
                  for (i=0; i < 5; i++)
                  {
                     obj_obbox_vs[i].x = ObjPtr->rotated_obbox[i].x+dx;
                     obj_obbox_vs[i].y = ObjPtr->rotated_obbox[i].y+dy;
                  }
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC,
                        obj_obbox_vs, 5, CoordModeOrigin);
               }
               break;
            case OBJ_RCBOX:
               if (ObjPtr->ctm == NULL) {
                  MyRCBox (drawWindow,revDefaultGC,ltx+dx,lty+dy,rbx+dx,rby+dy,
                        radius);
               } else {
                  for (i=0; i < sn; i++) {
                     sv[i].x = pv[i].x + dx;
                     sv[i].y = pv[i].y + dy;
                  }
                  XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                        CoordModeOrigin);
               }
               break;
            case OBJ_ARC:
               if (ObjPtr->ctm == NULL) {
                  XDrawArc(mainDisplay, drawWindow, revDefaultGC, ltx, lty,
                        w, h, angle1, angle2);
                  if (arc_ptr->fill != NONEPAT) {
                     XDrawLine(mainDisplay,drawWindow,revDefaultGC,xc,yc,x1,y1);
                     XDrawLine(mainDisplay,drawWindow,revDefaultGC,xc,yc,x2,y2);
                  }
               } else {
                  for (i=0; i < sn+2; i++) {
                     sv[i].x = pv[i].x + dx;
                     sv[i].y = pv[i].y + dy;
                  }
                  if (arc_ptr->fill != NONEPAT) {
                     XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn+2,
                           CoordModeOrigin);
                  } else {
                     XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                           CoordModeOrigin);
                  }
               }
               break;
            case OBJ_OVAL:
               if (ObjPtr->ctm == NULL)
               {
                  o_bbox.ltx = bbox.ltx + dx; o_bbox.lty = bbox.lty + dy;
                  o_bbox.rbx = bbox.rbx + dx; o_bbox.rby = bbox.rby + dy;
                  MyOval (drawWindow, revDefaultGC, o_bbox);
               }
               else
               {
                  for (i=0; i < sn; i++)
                  {
                     sv[i].x = pv[i].x + dx;
                     sv[i].y = pv[i].y + dy;
                  }
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC, sv, sn,
                        CoordModeOrigin);
               }
               break;
            case OBJ_POLY:
            case OBJ_POLYGON:
               if (splineRubberband)
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC, sv, sn,
                        CoordModeOrigin);
               else
               {
                  for (i = 0; i < num_pts; i++)
                  {
                     v[i].x += dx;
                     v[i].y += dy;
                  }
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC, v, num_pts,
                        CoordModeOrigin);
                  for (i = 0; i < num_pts; i++)
                  {
                     v[i].x -= dx;
                     v[i].y -= dy;
                  }
               }
               break;
            case OBJ_GROUP:
            case OBJ_SYM:
            case OBJ_ICON:
               SelBox (drawWindow,revDefaultGC,ltx+dx,lty+dy,rbx+dx,rby+dy);
               break;
         }
         SelBox (drawWindow, revDefaultGC, sel_ltx+dx, sel_lty+dy, sel_rbx+dx,
               sel_rby+dy);

         dx = grid_x - OrigX;
         dy = grid_y - OrigY;

         DrawIntervalRulers (ruler_ltx+dx, ruler_lty+dy, ruler_rbx+dx,
               ruler_rby+dy);
         SelBox (drawWindow, revDefaultGC, sel_ltx+dx, sel_lty+dy, sel_rbx+dx,
               sel_rby+dy);
         PixelToMeasurementUnit(x_buf, ABS_SIZE(dx));
         PixelToMeasurementUnit(y_buf, ABS_SIZE(dy));
         sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
         ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         switch (ObjPtr->type)
         {
            case OBJ_BOX:
            case OBJ_XBM:
            case OBJ_XPM:
            case OBJ_TEXT:
               if (ObjPtr->ctm == NULL)
                  SelBox (drawWindow,revDefaultGC,ltx+dx,lty+dy,rbx+dx,rby+dy);
               else
               {
                  for (i=0; i < 5; i++)
                  {
                     obj_obbox_vs[i].x = ObjPtr->rotated_obbox[i].x+dx;
                     obj_obbox_vs[i].y = ObjPtr->rotated_obbox[i].y+dy;
                  }
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC,
                        obj_obbox_vs, 5, CoordModeOrigin);
               }
               break;
            case OBJ_RCBOX:
               if (ObjPtr->ctm == NULL) {
                  SetRCBoxVertex (ltx+dx, lty+dy, rbx+dx, rby+dy, radius);
                  MyRCBox (drawWindow,revDefaultGC,ltx+dx,lty+dy,rbx+dx,rby+dy,
                        radius);
               } else {
                  for (i=0; i < sn; i++) {
                     sv[i].x = pv[i].x + dx;
                     sv[i].y = pv[i].y + dy;
                  }
                  XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                        CoordModeOrigin);
               }
               break;
            case OBJ_OVAL:
               if (ObjPtr->ctm == NULL)
               {
                  o_bbox.ltx = bbox.ltx + dx; o_bbox.lty = bbox.lty + dy;
                  o_bbox.rbx = bbox.rbx + dx; o_bbox.rby = bbox.rby + dy;
                  MyOval (drawWindow, revDefaultGC, o_bbox);
               }
               else
               {
                  for (i=0; i < sn; i++)
                  {
                     sv[i].x = pv[i].x + dx;
                     sv[i].y = pv[i].y + dy;
                  }
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC, sv, sn,
                        CoordModeOrigin);
               }
               break;
            case OBJ_ARC:
               if (ObjPtr->ctm == NULL) {
                  xc = saved_xc+dx; yc = saved_yc+dy;
                  x1 = saved_x1+dx; y1 = saved_y1+dy;
                  x2 = saved_x2+dx; y2 = saved_y2+dy;
                  ltx = saved_ltx+dx; lty = saved_lty+dy;
                  if (arc_ptr->fill != NONEPAT) {
                     XDrawLine(mainDisplay,drawWindow,revDefaultGC,xc,yc,x1,y1);
                     XDrawLine(mainDisplay,drawWindow,revDefaultGC,xc,yc,x2,y2);
                  }
                  XDrawArc(mainDisplay, drawWindow, revDefaultGC, ltx, lty,
                        w, h, angle1, angle2);
               } else {
                  for (i=0; i < sn+2; i++) {
                     sv[i].x = pv[i].x + dx;
                     sv[i].y = pv[i].y + dy;
                  }
                  if (arc_ptr->fill != NONEPAT) {
                     XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn+2,
                           CoordModeOrigin);
                  } else {
                     XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                           CoordModeOrigin);
                  }
               }
               break;
            case OBJ_POLY:
            case OBJ_POLYGON:
               if (splineRubberband)
               {
                  free(sv);
                  switch (ObjPtr->type)
                  {
                     case OBJ_POLY:
                        if (ObjPtr->ctm == NULL) {
                           for (i = 0; i < num_pts; i++) {
                              polyv[i].x = poly_ptr->vlist[i].x+ABS_SIZE(dx);
                              polyv[i].y = poly_ptr->vlist[i].y+ABS_SIZE(dy);
                           }
                        } else {
                           for (i = 0; i < num_pts; i++) {
                              int x, y;

                              TransformPointThroughCTM(
                                    poly_ptr->vlist[i].x-ObjPtr->x,
                                    poly_ptr->vlist[i].y-ObjPtr->y,
                                    ObjPtr->ctm, &x, &y);
                              polyv[i].x = x+ObjPtr->x+ABS_SIZE(dx);
                              polyv[i].y = y+ObjPtr->y+ABS_SIZE(dy);
                           }
                        }
                        if (curved != LT_INTSPLINE) {
                           sv = MakeMultiSplinePolyVertex (&sn, smooth,
                                 drawOrigX, drawOrigY, num_pts, polyv);
                        } else {
                           free(cntrlv);
                           sv = MakeIntSplinePolyVertex (&sn, &intn, &cntrlv,
                                 drawOrigX, drawOrigY, num_pts, polyv);
                        }
                        break;
                     case OBJ_POLYGON:
                        if (ObjPtr->ctm == NULL) {
                           for (i = 0; i < num_pts; i++) {
                              polyv[i].x = polygon_ptr->vlist[i].x+ABS_SIZE(dx);
                              polyv[i].y = polygon_ptr->vlist[i].y+ABS_SIZE(dy);
                           }
                        } else {
                           for (i = 0; i < num_pts; i++) {
                              int x, y;

                              TransformPointThroughCTM(
                                    polygon_ptr->vlist[i].x-ObjPtr->x,
                                    polygon_ptr->vlist[i].y-ObjPtr->y,
                                    ObjPtr->ctm, &x, &y);
                              polyv[i].x = x+ObjPtr->x+ABS_SIZE(dx);
                              polyv[i].y = y+ObjPtr->y+ABS_SIZE(dy);
                           }
                        }
                        if (curved != LT_INTSPLINE) {
                           sv = MakeMultiSplinePolygonVertex (&sn, smooth,
                                 drawOrigX, drawOrigY, num_pts, polyv);
                        } else {
                           free(cntrlv);
                           sv = MakeIntSplinePolygonVertex (&sn, &intn,
                                 &cntrlv, drawOrigX, drawOrigY, num_pts,
                                 polyv);
                        }
                        break;
                  }
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC,
                        sv, sn, CoordModeOrigin);
               }
               else
               {
                  for (i = 0; i < num_pts; i++)
                  {
                     v[i].x += dx;
                     v[i].y += dy;
                  }
                  XDrawLines (mainDisplay, drawWindow, revDefaultGC, v, num_pts,
                        CoordModeOrigin);
                  for (i = 0; i < num_pts; i++)
                  {
                     v[i].x -= dx;
                     v[i].y -= dy;
                  }
               }
               break;
            case OBJ_GROUP:
            case OBJ_SYM:
            case OBJ_ICON:
               SelBox (drawWindow,revDefaultGC,ltx+dx,lty+dy,rbx+dx,rby+dy);
               break;
         }
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
}

void MoveAllSelVs (Dx, Dy)
   register int Dx, Dy;
   /* Dx and Dy are absolute size */
{
   register int i;
   register IntPoint *v=NULL;
   struct ObjRec *obj_ptr;
   struct VSelRec *vsel_ptr;
   int n=0, ltx=selLtX, lty=selLtY, rbx=selRbX, rby=selRbY;

   StartCompositeCmd();
   for (vsel_ptr = botVSel; vsel_ptr != NULL; vsel_ptr = vsel_ptr->prev) {
      int auto_retracted_arrow=FALSE;

      obj_ptr = vsel_ptr->obj;
      switch (obj_ptr->type) {
      case OBJ_POLY:
         v = obj_ptr->detail.p->vlist;
         n = obj_ptr->detail.p->n;
         auto_retracted_arrow = AutoRetractedArrowAttr(obj_ptr, TRUE);
         break;
      case OBJ_POLYGON:
         v = obj_ptr->detail.g->vlist;
         n = obj_ptr->detail.g->n;
         break;
      }
      PrepareToReplaceAnObj(obj_ptr);
      if (obj_ptr->ctm == NULL) {
         for (i = 0; i < vsel_ptr->n; i++) {
            vsel_ptr->x[i] += Dx;
            vsel_ptr->y[i] += Dy;
            v[vsel_ptr->v_index[i]].x += Dx;
            v[vsel_ptr->v_index[i]].y += Dy;
         }
      } else {
         for (i = 0; i < vsel_ptr->n; i++) {
            int x, y;

            vsel_ptr->x[i] += Dx;
            vsel_ptr->y[i] += Dy;
            ReverseTransformPointThroughCTM(vsel_ptr->x[i]-obj_ptr->x,
                  vsel_ptr->y[i]-obj_ptr->y, obj_ptr->ctm, &x, &y);
            v[vsel_ptr->v_index[i]].x = x+obj_ptr->x;
            v[vsel_ptr->v_index[i]].y = y+obj_ptr->y;
         }
      }
      AdjObjSplineVs(obj_ptr);
      if (auto_retracted_arrow) {
         for (i=0; i < vsel_ptr->n; i++) {
            if (vsel_ptr->v_index[i] == 1) {
               vsel_ptr->x[i] = v[1].x;
               vsel_ptr->y[i] = v[1].y;
            }
         }
      }
      switch (obj_ptr->type) {
      case OBJ_POLY:
         if (obj_ptr->detail.p->curved != LT_INTSPLINE) {
            UpdPolyBBox(obj_ptr, n, v);
         } else {
            UpdPolyBBox(obj_ptr, obj_ptr->detail.p->intn,
                  obj_ptr->detail.p->intvlist);
         }
         break;
      case OBJ_POLYGON:
         if (obj_ptr->detail.g->curved != LT_INTSPLINE) {
            UpdPolyBBox(obj_ptr, n, v);
         } else {
            UpdPolyBBox(obj_ptr, obj_ptr->detail.g->intn,
                  obj_ptr->detail.g->intvlist);
         }
         break;
      }
      RecordReplaceAnObj(obj_ptr);
   }
   EndCompositeCmd();
   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
}

static struct ObjRec	* tmpTopObj, * tmpBotObj;

#define FORWARD 0
#define REVERSE 1

static
void EndMoveVs ()
{
   register struct ObjRec	* obj_ptr, * next_obj;

   for (obj_ptr=tmpTopObj; obj_ptr != NULL; obj_ptr = next_obj) {
      next_obj = obj_ptr->next;
      switch (obj_ptr->type) {
      case OBJ_POLY:
         free(obj_ptr->detail.p->vlist);
         if (splineRubberband) {
            free(obj_ptr->detail.p->svlist);
            if (obj_ptr->detail.p->curved == LT_INTSPLINE &&
                  obj_ptr->detail.p->intvlist != NULL) {
               free(obj_ptr->detail.p->intvlist);
            }
         }
         free(obj_ptr->detail.p);
         break;
      case OBJ_POLYGON:
         free(obj_ptr->detail.g->vlist);
         if (splineRubberband) {
            free(obj_ptr->detail.g->svlist);
            if (obj_ptr->detail.g->curved == LT_INTSPLINE &&
                  obj_ptr->detail.g->intvlist != NULL) {
               free(obj_ptr->detail.g->intvlist);
            }
         }
         free(obj_ptr->detail.g);
         break;
      }
      free(obj_ptr);
   }
}

static
void PrepareToMoveVs ()
{
   register int i;
   struct VSelRec *vsel_ptr;
   struct ObjRec *obj_ptr;
   struct PolyRec *poly_ptr, *poly_copy;
   struct PolygonRec *polygon_ptr, *polygon_copy;
   XPoint *sv=NULL;
   IntPoint *polyv, *cntrlv=NULL;
   int num_pts, sn, curved, intn=0;
   char *smooth=NULL;

   tmpTopObj = tmpBotObj = NULL;
   for (vsel_ptr = botVSel; vsel_ptr != NULL; vsel_ptr = vsel_ptr->prev) {
      obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
      if (obj_ptr == NULL) FailAllocMessage();
      memset(obj_ptr, 0, sizeof(struct ObjRec));
      obj_ptr->prev = NULL;
      obj_ptr->next = tmpTopObj;
      obj_ptr->ctm = NULL;
      if (tmpTopObj == NULL) {
         tmpBotObj = obj_ptr;
      } else {
         tmpTopObj->prev = obj_ptr;
      }
      tmpTopObj = obj_ptr;
      obj_ptr->type = vsel_ptr->obj->type;

      switch (vsel_ptr->obj->type) {
      case OBJ_POLY:
         poly_copy = (struct PolyRec *)malloc(sizeof(struct PolyRec));
         if (poly_copy == NULL) FailAllocMessage();
         memset(poly_copy, 0, sizeof(struct PolyRec));
         obj_ptr->detail.p = poly_copy;

         poly_ptr = vsel_ptr->obj->detail.p;
         curved = poly_copy->curved = poly_ptr->curved;
         num_pts = poly_copy->n = poly_ptr->n;
         if (splineRubberband) {
            polyv = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
            if (polyv == NULL) FailAllocMessage();
            if (curved != LT_INTSPLINE && poly_ptr->smooth != NULL) {
               smooth = (char*)malloc((num_pts+1)*sizeof(char));
               if (smooth == NULL) FailAllocMessage();
            }
            if (vsel_ptr->obj->ctm == NULL) {
               for (i = 0; i < num_pts; i++) {
                  polyv[i].x = poly_ptr->vlist[i].x;
                  polyv[i].y = poly_ptr->vlist[i].y;
                  if (smooth != NULL) smooth[i] = poly_ptr->smooth[i];
               }
            } else {
               for (i = 0; i < num_pts; i++) {
                  int x, y;

                  TransformPointThroughCTM(
                        poly_ptr->vlist[i].x-vsel_ptr->obj->x,
                        poly_ptr->vlist[i].y-vsel_ptr->obj->y,
                        vsel_ptr->obj->ctm, &x, &y);
                  polyv[i].x = x+vsel_ptr->obj->x;
                  polyv[i].y = y+vsel_ptr->obj->y;
                  if (smooth != NULL) smooth[i] = poly_ptr->smooth[i];
               }
            }
            if (curved != LT_INTSPLINE) {
               sv = MakeMultiSplinePolyVertex(&sn, smooth,
                     drawOrigX, drawOrigY, num_pts, polyv);
            } else {
               sv = MakeIntSplinePolyVertex(&sn, &intn, &cntrlv,
                     drawOrigX, drawOrigY, num_pts, polyv);
            }
            poly_copy->vlist = polyv;
            poly_copy->smooth = smooth;
            poly_copy->svlist = sv;
            poly_copy->sn = sn;
         } else {
            polyv = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
            if (polyv == NULL) FailAllocMessage();
            for (i = 0; i < num_pts; i++) {
               polyv[i].x = OFFSET_X(poly_ptr->vlist[i].x);
               polyv[i].y = OFFSET_Y(poly_ptr->vlist[i].y);
            }
            poly_copy->vlist = polyv;
            poly_copy->smooth = NULL;
         }
         break;
      case OBJ_POLYGON:
         polygon_copy = (struct PolygonRec *)malloc(sizeof(struct PolygonRec));
         if (polygon_copy == NULL) FailAllocMessage();
         memset(polygon_copy, 0, sizeof(struct PolygonRec));
         obj_ptr->detail.g = polygon_copy;

         polygon_ptr = vsel_ptr->obj->detail.g;
         curved = polygon_copy->curved = polygon_ptr->curved;
         num_pts = polygon_copy->n = polygon_ptr->n;
         if (splineRubberband) {
            polyv = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
            if (polyv == NULL) FailAllocMessage();
            if (curved != LT_INTSPLINE && polygon_ptr->smooth != NULL) {
               smooth = (char*)malloc((num_pts+1)*sizeof(char));
               if (smooth == NULL) FailAllocMessage();
            }
            if (vsel_ptr->obj->ctm == NULL) {
               for (i = 0; i < num_pts; i++) {
                  polyv[i].x = polygon_ptr->vlist[i].x;
                  polyv[i].y = polygon_ptr->vlist[i].y;
                  if (smooth != NULL) smooth[i] = polygon_ptr->smooth[i];
               }
            } else {
               for (i = 0; i < num_pts; i++) {
                  int x, y;

                  TransformPointThroughCTM(
                        polygon_ptr->vlist[i].x-vsel_ptr->obj->x,
                        polygon_ptr->vlist[i].y-vsel_ptr->obj->y,
                        vsel_ptr->obj->ctm, &x, &y);
                  polyv[i].x = x+vsel_ptr->obj->x;
                  polyv[i].y = y+vsel_ptr->obj->y;
                  if (smooth != NULL) smooth[i] = polygon_ptr->smooth[i];
               }
            }
            if (curved != LT_INTSPLINE) {
               sv = MakeMultiSplinePolygonVertex(&sn, smooth,
                     drawOrigX, drawOrigY, num_pts,polyv);
            } else {
               sv = MakeIntSplinePolygonVertex(&sn, &intn, &cntrlv,
                     drawOrigX, drawOrigY, num_pts,polyv);
            }
            polygon_copy->vlist = polyv;
            polygon_copy->smooth = smooth;
            polygon_copy->svlist = sv;
            polygon_copy->sn = sn;
         } else {
            polyv = (IntPoint*)malloc((num_pts+1)*sizeof(IntPoint));
            if (polyv == NULL) FailAllocMessage();
            for (i = 0; i < num_pts; i++) {
               polyv[i].x = OFFSET_X(polygon_ptr->vlist[i].x);
               polyv[i].y = OFFSET_Y(polygon_ptr->vlist[i].y);
            }
            polygon_copy->vlist = polyv;
            polygon_copy->smooth = NULL;
         }
         break;
      }
   }
}

static
void HighLightVs (Dir)
   int	Dir;
{
   register int			i;
   register struct ObjRec	* obj_ptr=NULL;
   register IntPoint		* v;

   switch (Dir)
   {
      case FORWARD: obj_ptr = tmpBotObj; break;
      case REVERSE: obj_ptr = tmpTopObj; break;
   }
   while (obj_ptr != NULL)
   {
      switch (obj_ptr->type)
      {
         case OBJ_POLY:
            v = obj_ptr->detail.p->vlist;
            if (splineRubberband)
            {
               if (obj_ptr->detail.p->curved != LT_INTSPLINE &&
                     obj_ptr->detail.p->smooth != NULL)
               {
                  char	* smooth=obj_ptr->detail.p->smooth;

                  for (i = 0; i < obj_ptr->detail.p->n; i++)
                     if (smooth[i])
                        MARKO(drawWindow, revDefaultGC, OFFSET_X(v[i].x),
                              OFFSET_Y(v[i].y));
                     else
                        MARK(drawWindow, revDefaultGC, OFFSET_X(v[i].x),
                              OFFSET_Y(v[i].y));
               }
               else
                  for (i = 0; i < obj_ptr->detail.p->n; i++)
                     MARK(drawWindow, revDefaultGC, OFFSET_X(v[i].x),
                           OFFSET_Y(v[i].y));
               XDrawLines (mainDisplay, drawWindow, revDefaultGC,
                     obj_ptr->detail.p->svlist, obj_ptr->detail.p->sn,
                     CoordModeOrigin);
            }
            else
            {
               int sn=obj_ptr->detail.p->n;
               XPoint *sv=(XPoint*)malloc(sn*sizeof(XPoint));

               if (sv == NULL) FailAllocMessage();
               for (i = 0; i < obj_ptr->detail.p->n; i++) {
                  MARK(drawWindow, revDefaultGC, v[i].x, v[i].y);
                  sv[i].x = (short)v[i].x;
                  sv[i].y = (short)v[i].y;
               }
               XDrawLines (mainDisplay, drawWindow, revDefaultGC,
                     sv, sn, CoordModeOrigin);
               if (sv != NULL) free(sv);
            }
            break;
         case OBJ_POLYGON:
            v = obj_ptr->detail.p->vlist;
            if (splineRubberband)
            {
               if (obj_ptr->detail.g->curved != LT_INTSPLINE &&
                     obj_ptr->detail.g->smooth != NULL)
               {
                  char	* smooth=obj_ptr->detail.g->smooth;

                  for (i = 0; i < obj_ptr->detail.g->n-1; i++)
                     if (smooth[i])
                        MARKO(drawWindow, revDefaultGC, OFFSET_X(v[i].x),
                              OFFSET_Y(v[i].y));
                     else
                        MARK(drawWindow, revDefaultGC, OFFSET_X(v[i].x),
                              OFFSET_Y(v[i].y));
               }
               else
                  for (i = 0; i < obj_ptr->detail.g->n-1; i++)
                     MARK(drawWindow, revDefaultGC, OFFSET_X(v[i].x),
                           OFFSET_Y(v[i].y));
               XDrawLines (mainDisplay, drawWindow, revDefaultGC,
                     obj_ptr->detail.g->svlist, obj_ptr->detail.g->sn,
                     CoordModeOrigin);
            }
            else
            {
               int sn=obj_ptr->detail.g->n;
               XPoint *sv=(XPoint*)malloc(sn*sizeof(XPoint));

               if (sv == NULL) FailAllocMessage();
               for (i = 0; i < obj_ptr->detail.g->n-1; i++) {
                  MARK(drawWindow, revDefaultGC, v[i].x, v[i].y);
                  sv[i].x = (short)v[i].x;
                  sv[i].y = (short)v[i].y;
               }
               XDrawLines (mainDisplay, drawWindow, revDefaultGC,
                     sv, sn, CoordModeOrigin);
               if (sv != NULL) free(sv);
            }
            break;
      }
      switch (Dir)
      {
         case FORWARD: obj_ptr = obj_ptr->prev; break;
         case REVERSE: obj_ptr = obj_ptr->next; break;
      }
   }
}

static
void MarkVs (Dir, Dx, Dy)
   int	Dir, Dx, Dy;
{
   register int			i, x, y;
   register struct VSelRec	* vsel_ptr=NULL;

   switch (Dir)
   {
      case FORWARD: vsel_ptr = botVSel; break;
      case REVERSE: vsel_ptr = topVSel; break;
   }
   while (vsel_ptr != NULL)
   {
      char	* smooth=NULL;
      int	curved=(-1);

      switch (vsel_ptr->obj->type)
      {
         case OBJ_POLY:
            smooth = vsel_ptr->obj->detail.p->smooth;
            curved = vsel_ptr->obj->detail.p->curved;
            break;
         case OBJ_POLYGON:
            smooth = vsel_ptr->obj->detail.g->smooth;
            curved = vsel_ptr->obj->detail.g->curved;
            break;
      }
      for (i = 0; i < vsel_ptr->n; i++)
      {
         if (!(vsel_ptr->obj->type==OBJ_POLYGON &&
               vsel_ptr->obj->detail.g->n-1==vsel_ptr->v_index[i]))
         {
            x = OFFSET_X(vsel_ptr->x[i])+Dx;
            y = OFFSET_Y(vsel_ptr->y[i])+Dy;
            if (curved != LT_INTSPLINE && curved != (-1) && smooth != NULL)
            {
               if (smooth[vsel_ptr->v_index[i]])
                  MARKO(drawWindow, revDefaultGC, x, y);
               else
                  MARK(drawWindow, revDefaultGC, x, y);
            }
            else
               MARK(drawWindow, revDefaultGC, x, y);
            MARKV(drawWindow, revDefaultGC, x, y);
         }
      }
      switch (Dir)
      {
         case FORWARD: vsel_ptr = vsel_ptr->prev; break;
         case REVERSE: vsel_ptr = vsel_ptr->next; break;
      }
   }
}

void MoveSelVs (OrigX, OrigY)
   int 	OrigX, OrigY;
{
   register int x, y, i;
   struct ObjRec *obj_ptr;
   struct VSelRec *vsel_ptr;
   struct PolyRec *poly_ptr;
   struct PolygonRec *polygon_ptr;
   IntPoint *pv;
   int moving = TRUE, dx, dy, num_pts, curved;
   int grid_x=OrigX, grid_y=OrigY;
   int saved_grid_x=OrigX, saved_grid_y=OrigY;
   char buf[80], x_buf[80], y_buf[80];
   XEvent ev;

   XFlush(mainDisplay);
   XSync(mainDisplay, False);

   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev) ||
         XCheckMaskEvent(mainDisplay, VisibilityChangeMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
   }
   HighLightReverse();

   PrepareToMoveVs();
   HighLightVs(FORWARD);
   MarkVs(FORWARD, 0, 0);

   PixelToMeasurementUnit(x_buf, 0);
   PixelToMeasurementUnit(y_buf, 0);
   sprintf(buf, "dx=%s,dy=%s", x_buf, y_buf);
   StartShowMeasureCursor(grid_x, grid_y, buf, TRUE);

   XGrabPointer(mainDisplay, drawWindow, FALSE,
         PointerMotionMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, moveCursor, CurrentTime);

   while(moving) {
      XEvent input;

      XNextEvent(mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.type == ButtonRelease) {
         XUngrabPointer(mainDisplay, CurrentTime);
         XSync(mainDisplay, False);
         PixelToMeasurementUnit(x_buf, ABS_SIZE(grid_x-OrigX));
         PixelToMeasurementUnit(y_buf, ABS_SIZE(grid_y-OrigY));
         sprintf(buf, "dx=%s,dy=%s", x_buf, y_buf);
         EndShowMeasureCursor(grid_x, grid_y, buf, TRUE);
         MarkRulers(grid_x, grid_y);
         moving = FALSE;

         MarkVs(REVERSE, grid_x-OrigX, grid_y-OrigY);
         HighLightVs(REVERSE);
         EndMoveVs();

         dx = grid_x - OrigX;
         dy = grid_y - OrigY;

         if (dx != 0 || dy != 0) {
            MoveAllSelVs(ABS_SIZE(dx), ABS_SIZE(dy));
            HighLightForward();
            SetFileModified(TRUE);
         } else {
            HighLightForward();
         }
      } else if (input.type == MotionNotify) {
         PixelToMeasurementUnit(x_buf, ABS_SIZE(grid_x-OrigX));
         PixelToMeasurementUnit(y_buf, ABS_SIZE(grid_y-OrigY));
         sprintf(buf, "dx=%s,dy=%s", x_buf, y_buf);
         ShowMeasureCursor(grid_x, grid_y, buf, TRUE);
         MarkVs(REVERSE, grid_x-OrigX, grid_y-OrigY);
         HighLightVs(REVERSE);

         x = input.xmotion.x;
         y = input.xmotion.y;
         GridXY(x, y, &grid_x, &grid_y);

         dx = grid_x - saved_grid_x;
         dy = grid_y - saved_grid_y;

         saved_grid_x = grid_x;
         saved_grid_y = grid_y;

         MarkRulers(grid_x, grid_y);
         for (vsel_ptr = botVSel, obj_ptr = tmpBotObj; vsel_ptr != NULL;
               vsel_ptr = vsel_ptr->prev, obj_ptr = obj_ptr->prev) {
            switch (obj_ptr->type) {
            case OBJ_POLY:
               poly_ptr = obj_ptr->detail.p;
               curved = poly_ptr->curved;
               num_pts = poly_ptr->n;
               pv = poly_ptr->vlist;
               if (splineRubberband) {
                  for (i = 0; i < vsel_ptr->n; i++) {
                     pv[vsel_ptr->v_index[i]].x += ABS_SIZE(dx);
                     pv[vsel_ptr->v_index[i]].y += ABS_SIZE(dy);
                  }
                  free(poly_ptr->svlist);
                  switch (curved) {
                  case LT_STRAIGHT:
                  case LT_SPLINE:
                     poly_ptr->svlist = MakeMultiSplinePolyVertex(
                           &(poly_ptr->sn), poly_ptr->smooth,
                           drawOrigX, drawOrigY, num_pts, pv);
                     break;
                  case LT_INTSPLINE:
                     free(poly_ptr->intvlist);
                     poly_ptr->svlist = MakeIntSplinePolyVertex(
                           &(poly_ptr->sn), &(poly_ptr->intn),
                           &(poly_ptr->intvlist), drawOrigX, drawOrigY,
                           num_pts, pv);
                     break;
                  }
               } else {
                  for (i = 0; i < vsel_ptr->n; i++) {
                     pv[vsel_ptr->v_index[i]].x += dx;
                     pv[vsel_ptr->v_index[i]].y += dy;
                  }
               }
               break;
            case OBJ_POLYGON:
               polygon_ptr = obj_ptr->detail.g;
               curved = polygon_ptr->curved;
               num_pts = polygon_ptr->n;
               pv = polygon_ptr->vlist;
               if (splineRubberband) {
                  for (i = 0; i < vsel_ptr->n; i++) {
                     pv[vsel_ptr->v_index[i]].x += ABS_SIZE(dx);
                     pv[vsel_ptr->v_index[i]].y += ABS_SIZE(dy);
                  }
                  free(polygon_ptr->svlist);
                  switch (curved) {
                  case LT_STRAIGHT:
                  case LT_SPLINE:
                     polygon_ptr->svlist =
                           MakeMultiSplinePolygonVertex(
                           &(polygon_ptr->sn), polygon_ptr->smooth,
                           drawOrigX, drawOrigY, num_pts, pv);
                     break;
                  case LT_INTSPLINE:
                     free(polygon_ptr->intvlist);
                     polygon_ptr->svlist =
                           MakeIntSplinePolygonVertex(&(polygon_ptr->sn),
                           &(polygon_ptr->intn), &(polygon_ptr->intvlist),
                           drawOrigX, drawOrigY, num_pts, pv);
                     break;
                  }
               } else {
                  for (i = 0; i < vsel_ptr->n; i++) {
                     pv[vsel_ptr->v_index[i]].x += dx;
                     pv[vsel_ptr->v_index[i]].y += dy;
                  }
               }
               break;
            }
         }
         HighLightVs(FORWARD);
         MarkVs(FORWARD, grid_x-OrigX, grid_y-OrigY);
         PixelToMeasurementUnit(x_buf, ABS_SIZE(grid_x-OrigX));
         PixelToMeasurementUnit(y_buf, ABS_SIZE(grid_y-OrigY));
         sprintf(buf, "dx=%s,dy=%s", x_buf, y_buf);
         ShowMeasureCursor(grid_x, grid_y, buf, TRUE);
         while (XCheckMaskEvent(mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
}
