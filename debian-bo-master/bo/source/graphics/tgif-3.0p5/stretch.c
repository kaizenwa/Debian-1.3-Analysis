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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/stretch.c,v 3.1 1996/05/12 08:08:26 william Exp $";
#endif

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#include "align.e"
#include "arc.e"
#include "auxtext.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "exec.e"
#include "font.e"
#include "grid.e"
#include "mainloop.e"
#include "mark.e"
#include "move.e"
#include "msg.e"
#include "obj.e"
#include "poly.e"
#include "raster.e"
#include "rect.e"
#include "ruler.e"
#include "select.e"
#include "setup.e"
#include "spline.e"
#ifndef _NO_EXTERN
#include "stretch.e"
#endif
#include "text.e"
#include "xbitmap.e"
#include "xpixmap.e"

int stretchableText=FALSE;
int rotationIncrement=(45<<6); /* degrees*64 */

static
int PtIn4Corners (XOff, YOff, BBox, Corner)
   int		XOff, YOff, * Corner;
   struct BBRec	BBox;
{
   if (PtInMark (XOff, YOff, OFFSET_X(BBox.ltx), OFFSET_Y(BBox.lty)))
   {
      *Corner = 1;
      return(TRUE);
   }
   if (PtInMark (XOff, YOff, OFFSET_X(BBox.ltx), OFFSET_Y(BBox.rby)))
   {
      *Corner = 7;
      return(TRUE);
   }
   if (PtInMark (XOff, YOff, OFFSET_X(BBox.rbx), OFFSET_Y(BBox.lty)))
   {
      *Corner = 3;
      return(TRUE);
   }
   if (PtInMark (XOff, YOff, OFFSET_X(BBox.rbx), OFFSET_Y(BBox.rby)))
   {
      *Corner = 5;
      return(TRUE);
   }
   return (FALSE);
}

static
int PtIn8Places (XOff, YOff, BBox, Corner)
   int		XOff, YOff, * Corner;
   struct BBRec	BBox;
{
   register int	xmid, ymid;

   if (PtIn4Corners (XOff, YOff, BBox, Corner)) return (TRUE);

   xmid = ((BBox.ltx+BBox.rbx)>>1);
   if (PtInMark (XOff, YOff, OFFSET_X(xmid), OFFSET_Y(BBox.lty)))
   {
      *Corner = 2;
      return(TRUE);
   }
   if (PtInMark (XOff, YOff, OFFSET_X(xmid), OFFSET_Y(BBox.rby)))
   {
      *Corner = 6;
      return(TRUE);
   }
   ymid = ((BBox.lty+BBox.rby)>>1);
   if (PtInMark (XOff, YOff, OFFSET_X(BBox.ltx), OFFSET_Y(ymid)))
   {
      *Corner = 8;
      return(TRUE);
   }
   if (PtInMark (XOff, YOff, OFFSET_X(BBox.rbx), OFFSET_Y(ymid)))
   {
      *Corner = 4;
      return(TRUE);
   }
   return (FALSE);
}

int PtInPolyMark(ObjPtr, XOff, YOff, NumPts, V, Index)
   struct ObjRec *ObjPtr;
   int XOff, YOff, NumPts, * Index;
   IntPoint *V;
{
   int i;

   if (ObjPtr->ctm == NULL) {
      for (i = 0; i < NumPts; i++) {
         if (PtInMark(XOff, YOff, OFFSET_X(V[i].x), OFFSET_Y(V[i].y))) {
            *Index = i;
            return TRUE;
         }
      }
   } else {
      for (i = 0; i < NumPts; i++) {
         int x, y;

         TransformPointThroughCTM(V[i].x-ObjPtr->x, V[i].y-ObjPtr->y,
               ObjPtr->ctm, &x, &y);
         if (PtInMark(XOff, YOff, OFFSET_X(x+ObjPtr->x),
               OFFSET_Y(y+ObjPtr->y))) {
            *Index = i;
            return TRUE;
         }
      }
   }
   return FALSE;
}

int RetractedArrowAttr(obj_ptr)
   struct ObjRec *obj_ptr;
{
   register struct AttrRec *attr_ptr;

   if (obj_ptr->type != OBJ_POLY) return FALSE;

   if (obj_ptr->detail.p->n <= 2) return FALSE;
   for (attr_ptr=obj_ptr->lattr; attr_ptr!=NULL; attr_ptr=attr_ptr->prev) {
      if (*attr_ptr->attr_name.s=='\0' && strcmp(attr_ptr->attr_value.s,
            "retracted_arrows")==0) {
         return TRUE;
      }
   }
   return FALSE;
}

int AutoRetractedArrowAttr(obj_ptr, check_v_count)
   /* if check_v_count == TRUE:  return FALSE if poly_ptr->n != 3 */
   /* if check_v_count == FALSE: skip the poly_ptr->n check */
   struct ObjRec *obj_ptr;
   int check_v_count;
{
   register struct AttrRec *attr_ptr;

   if (obj_ptr->type == OBJ_POLY) {
      struct PolyRec *poly_ptr=obj_ptr->detail.p;

      if (poly_ptr->style == LS_PLAIN || poly_ptr->style == LS_DOUBLE ||
            (check_v_count && poly_ptr->n != 3)) {
         return FALSE;
      }
   } else {
      return FALSE;
   }
   for (attr_ptr=obj_ptr->lattr; attr_ptr!=NULL; attr_ptr=attr_ptr->prev) {
      if (*attr_ptr->attr_name.s=='\0' &&
            strcmp(attr_ptr->attr_value.s, "auto_retracted_arrows")==0) {
         return TRUE;
      }
   }
   return FALSE;
}

struct AttrRec *NotifyResizeAttr(obj_ptr)
   struct ObjRec *obj_ptr;
{
   register struct AttrRec *attr_ptr;

   for (attr_ptr=obj_ptr->lattr; attr_ptr!=NULL; attr_ptr=attr_ptr->prev) {
      if (strcmp(attr_ptr->attr_name.s, "on_reize_event")==0 &&
            *attr_ptr->attr_value.s != '\0') {
         return attr_ptr;
      }
   }
   return NULL;
}

int NotifyResize(obj_ptr, attr_ptr)
   struct ObjRec *obj_ptr;
   struct AttrRec *attr_ptr;
{
   struct AttrRec *exec_attr_ptr;

   if (*attr_ptr->attr_value.s == '\0') return FALSE;

   if ((exec_attr_ptr=FindAttrWithName(obj_ptr,attr_ptr->attr_value.s,NULL)) !=
         NULL) {
      return DoExec(exec_attr_ptr, obj_ptr);
   }
   return FALSE;
}

int AutoCenterAttr (obj_ptr)
   struct ObjRec	* obj_ptr;
{
   register struct AttrRec	* attr_ptr;

   for (attr_ptr=obj_ptr->lattr; attr_ptr!=NULL; attr_ptr=attr_ptr->prev)
      if (*attr_ptr->attr_name.s=='\0' && strcmp (attr_ptr->attr_value.s,
            "auto_center_attr")==0)
         return (TRUE);
   return (FALSE);
}

void CenterObjInOBBox (TextObjPtr, OBBox, BBoxReturn)
   struct ObjRec	* TextObjPtr;
   struct BBRec		OBBox, * BBoxReturn;
{
   int text_w, text_h, bbox_w, bbox_h, dx, dy;

   if (BBoxReturn != NULL)
   {
      BBoxReturn->ltx = TextObjPtr->bbox.ltx;
      BBoxReturn->lty = TextObjPtr->bbox.lty;
      BBoxReturn->rbx = TextObjPtr->bbox.rbx;
      BBoxReturn->rby = TextObjPtr->bbox.rby;
   }
   text_w = TextObjPtr->obbox.rbx-TextObjPtr->obbox.ltx;
   text_h = TextObjPtr->obbox.rby-TextObjPtr->obbox.lty;
   bbox_w = OBBox.rbx - OBBox.ltx;
   bbox_h = OBBox.rby - OBBox.lty;
   if (text_w > bbox_w)
      dx = OBBox.ltx-((text_w-bbox_w)>>1)-TextObjPtr->obbox.ltx;
   else
      dx = OBBox.ltx+((bbox_w-text_w)>>1)-TextObjPtr->obbox.ltx;
   if (text_h > bbox_h)
      dy = OBBox.lty-((text_h-bbox_h)>>1)-TextObjPtr->obbox.lty;
   else
      dy = OBBox.lty+((bbox_h-text_h)>>1)-TextObjPtr->obbox.lty;
   MoveObj (TextObjPtr, dx, dy);
   if (BBoxReturn != NULL)
   {
      if (TextObjPtr->bbox.ltx < BBoxReturn->ltx)
         BBoxReturn->ltx = TextObjPtr->bbox.ltx;
      if (TextObjPtr->bbox.lty < BBoxReturn->lty)
         BBoxReturn->lty = TextObjPtr->bbox.lty;
      if (TextObjPtr->bbox.rbx > BBoxReturn->rbx)
         BBoxReturn->rbx = TextObjPtr->bbox.rbx;
      if (TextObjPtr->bbox.rby > BBoxReturn->rby)
         BBoxReturn->rby = TextObjPtr->bbox.rby;
   }
}

struct SelRec *PtInSelMark(XOff, YOff, Corner)
   int XOff, YOff, *Corner;
   /* XOff and YOff are screen offsets */
   /* 1 2 3 */
   /* 8   4 */
   /* 7 6 5 */
{
   register struct SelRec *sel_ptr;
   register struct ObjRec *obj_ptr;

   for (sel_ptr=topSel; sel_ptr != NULL; sel_ptr=sel_ptr->next) {
      obj_ptr = sel_ptr->obj;

      switch (obj_ptr->type) {
      case OBJ_POLY:
         if (PtInPolyMark(obj_ptr, XOff, YOff, obj_ptr->detail.p->n,
               obj_ptr->detail.p->vlist, Corner)) {
            return sel_ptr;
         }
         break;
      case OBJ_POLYGON:
         if (PtInPolyMark(obj_ptr, XOff, YOff, obj_ptr->detail.g->n-1,
               obj_ptr->detail.g->vlist, Corner)) {
            return sel_ptr;
         }
         break;
      case OBJ_BOX:
      case OBJ_GROUP:
      case OBJ_SYM:
      case OBJ_ICON:
      case OBJ_OVAL:
      case OBJ_ARC:
      case OBJ_RCBOX:
      case OBJ_XBM:
      case OBJ_XPM:
         if (PtIn8Places(XOff, YOff, obj_ptr->obbox, Corner)) {
            return sel_ptr;
         }
         break;
      case OBJ_TEXT:
         if (curChoice == ROTATEMODE) {
            if (PtIn8Places(XOff, YOff, obj_ptr->obbox, Corner)) {
               return sel_ptr;
            }
         } else if (curChoice == NOTHING) {
            if (stretchableText &&
                  PtIn8Places(XOff, YOff, obj_ptr->obbox, Corner)) {
               return sel_ptr;
            }
         }
         break;
      }
   }
   return NULL;
}

static XPoint v[5];

static
void StretchPoly (XGridOff, YGridOff, ObjPtr, NumPts, V, Index)
   int			XGridOff, YGridOff, NumPts, Index;
   IntPoint		* V;
   struct ObjRec	* ObjPtr;
{
   register int	i;
   int		x, y, dx, dy, num=0, stretching=TRUE;
   int		ltx, lty, rbx, rby, curved=LT_STRAIGHT;
   int		grid_x=XGridOff, grid_y=YGridOff, sn, intn;
   int		auto_center_attr=AutoCenterAttr (ObjPtr);
   char		* smooth=NULL;
   char		buf[80], x_buf[80], y_buf[80];
   XEvent	input, ev;
   XPoint	*sv=NULL;
   IntPoint	*pv=NULL, *cntrlv=NULL;

   if (ObjPtr->locked)
   {
      Msg ("Locked object can not be stretched.");
      return;
   }

   switch (ObjPtr->type)
   {
      case OBJ_POLY:
         curved = ObjPtr->detail.p->curved;
         if (splineRubberband)
         {
            pv = (IntPoint*)malloc((NumPts+1)*sizeof(IntPoint));
            if (pv == NULL) FailAllocMessage();
            if (curved != LT_INTSPLINE && ObjPtr->detail.p->smooth != NULL) {
               smooth = (char*)malloc((NumPts+1)*sizeof(char));
               if (smooth == NULL) FailAllocMessage();
            }
            if (ObjPtr->ctm == NULL) {
               for (i = 0; i < NumPts; i++) {
                  pv[i].x = V[i].x;
                  pv[i].y = V[i].y;
                  if (smooth != NULL) smooth[i] = ObjPtr->detail.p->smooth[i];
               }
            } else {
               for (i = 0; i < NumPts; i++) {
                  int x, y;

                  TransformPointThroughCTM(V[i].x-ObjPtr->x, V[i].y-ObjPtr->y,
                        ObjPtr->ctm, &x, &y);
                  pv[i].x = x+ObjPtr->x;
                  pv[i].y = y+ObjPtr->y;
                  if (smooth != NULL) smooth[i] = ObjPtr->detail.p->smooth[i];
               }
            }
            switch (curved)
            {
               case LT_STRAIGHT:
               case LT_SPLINE:
                  sv = MakeMultiSplinePolyVertex (&sn, smooth,
                        drawOrigX, drawOrigY, NumPts, pv);
                  break;
               case LT_INTSPLINE:
                  sv = MakeIntSplinePolyVertex (&sn, &intn, &cntrlv, drawOrigX,
                        drawOrigY, NumPts, pv);
                  break;
            }
         }
         else if (Index == 0 || Index == NumPts-1)
         {
            num = 2;
            if (Index == 0)
            {
               v[0].x = OFFSET_X(V[1].x); v[0].y = OFFSET_Y(V[1].y);
               v[1].x = OFFSET_X(V[0].x); v[1].y = OFFSET_Y(V[0].y);
            }
            else
            {
               v[0].x = OFFSET_X(V[NumPts-2].x);
               v[0].y = OFFSET_Y(V[NumPts-2].y);
               v[1].x = OFFSET_X(V[NumPts-1].x);
               v[1].y = OFFSET_Y(V[NumPts-1].y);
            }
         }
         else
         {
            num = 3;
            v[0].x = OFFSET_X(V[Index-1].x); v[0].y = OFFSET_Y(V[Index-1].y);
            v[1].x = OFFSET_X(V[Index].x);   v[1].y = OFFSET_Y(V[Index].y);
            v[2].x = OFFSET_X(V[Index+1].x); v[2].y = OFFSET_Y(V[Index+1].y);
         }
         break;
      case OBJ_POLYGON:
         curved = ObjPtr->detail.g->curved;
         if (splineRubberband)
         {
            pv = (IntPoint*)malloc((NumPts+1)*sizeof(IntPoint));
            if (pv == NULL) FailAllocMessage();
            if (curved != LT_INTSPLINE && ObjPtr->detail.g->smooth != NULL) {
               smooth = (char*)malloc((NumPts+1)*sizeof(char));
               if (smooth == NULL) FailAllocMessage();
            }
            if (ObjPtr->ctm == NULL) {
               for (i = 0; i < NumPts; i++) {
                  pv[i].x = V[i].x;
                  pv[i].y = V[i].y;
                  if (smooth != NULL) smooth[i] = ObjPtr->detail.g->smooth[i];
               }
            } else {
               for (i = 0; i < NumPts; i++) {
                  int x, y;

                  TransformPointThroughCTM(V[i].x-ObjPtr->x, V[i].y-ObjPtr->y,
                        ObjPtr->ctm, &x, &y);
                  pv[i].x = x+ObjPtr->x;
                  pv[i].y = y+ObjPtr->y;
                  if (smooth != NULL) smooth[i] = ObjPtr->detail.g->smooth[i];
               }
            }
            switch (curved)
            {
               case LT_STRAIGHT:
               case LT_SPLINE:
                  sv = MakeMultiSplinePolygonVertex (&sn, smooth,
                        drawOrigX, drawOrigY, NumPts, pv);
                  break;
               case LT_INTSPLINE:
                  sv = MakeIntSplinePolygonVertex (&sn, &intn, &cntrlv,
                        drawOrigX, drawOrigY, NumPts, pv);
                  break;
            }
         }
         else if (Index == 0 || Index == NumPts-1)
         {
            num = 3;
            v[0].x = OFFSET_X(V[1].x); v[0].y = OFFSET_Y(V[1].y);
            v[1].x = OFFSET_X(V[0].x); v[1].y = OFFSET_Y(V[0].y);
            v[2].x = OFFSET_X(V[NumPts-2].x); v[2].y = OFFSET_Y(V[NumPts-2].y);
         }
         else
         {
            num = 3;
            v[0].x = OFFSET_X(V[Index-1].x); v[0].y = OFFSET_Y(V[Index-1].y);
            v[1].x = OFFSET_X(V[Index].x);   v[1].y = OFFSET_Y(V[Index].y);
            v[2].x = OFFSET_X(V[Index+1].x); v[2].y = OFFSET_Y(V[Index+1].y);
         }
         break;
   }

   ltx = ObjPtr->bbox.ltx;
   lty = ObjPtr->bbox.lty;
   rbx = ObjPtr->bbox.rbx;
   rby = ObjPtr->bbox.rby;

   XFlush (mainDisplay);
   XSync (mainDisplay, False);

   if (XCheckMaskEvent (mainDisplay, ExposureMask, &ev) ||
         XCheckMaskEvent (mainDisplay, VisibilityChangeMask, &ev))
      ExposeEventHandler (&ev, TRUE);

   PixelToMeasurementUnit(x_buf, 0);
   PixelToMeasurementUnit(y_buf, 0);
   sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
   StartShowMeasureCursor (grid_x, grid_y, buf, TRUE);
   XGrabPointer (mainDisplay, drawWindow, False,
         PointerMotionMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);

   dx = dy = 0;
   while (stretching)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == ButtonRelease)
      {
         XUngrabPointer (mainDisplay, CurrentTime);
         XSync (mainDisplay, False);
         stretching = FALSE;
      }
      else if (input.type == MotionNotify)
      {
         PixelToMeasurementUnit(x_buf, ABS_SIZE(grid_x-XGridOff));
         PixelToMeasurementUnit(y_buf, ABS_SIZE(grid_y-YGridOff));
         sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
         ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         x = input.xmotion.x;
         y = input.xmotion.y;
         GridXY (x, y, &grid_x, &grid_y);

         if (splineRubberband)
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, sv, sn,
                  CoordModeOrigin);
         else
            MyDashedLine (drawWindow, revDefaultGC, v, num);

         dx = grid_x - XGridOff;
         dy = grid_y - YGridOff;
         v[1].x = OFFSET_X(V[Index].x) + dx;
         v[1].y = OFFSET_Y(V[Index].y) + dy;
         MarkRulers (v[1].x, v[1].y);

         if (splineRubberband)
         {
            free(sv);
            if (ObjPtr->type==OBJ_POLYGON && (Index==0 || Index==NumPts-1))
            {
               if (ObjPtr->ctm == NULL) {
                  pv[0].x = pv[NumPts-1].x = V[0].x + ABS_SIZE(dx);
                  pv[0].y = pv[NumPts-1].y = V[0].y + ABS_SIZE(dy);
               } else {
                  int x, y;

                  TransformPointThroughCTM(V[0].x-ObjPtr->x, V[0].y-ObjPtr->y,
                        ObjPtr->ctm, &x, &y);
                  pv[0].x = pv[NumPts-1].x = x + ObjPtr->x + ABS_SIZE(dx);
                  pv[0].y = pv[NumPts-1].y = y + ObjPtr->y + ABS_SIZE(dy);
               }
            }
            else
            {
               if (ObjPtr->ctm == NULL) {
                  pv[Index].x = V[Index].x + ABS_SIZE(dx);
                  pv[Index].y = V[Index].y + ABS_SIZE(dy);
               } else {
                  int x, y;

                  TransformPointThroughCTM(V[Index].x-ObjPtr->x,
                        V[Index].y-ObjPtr->y, ObjPtr->ctm, &x, &y);
                  pv[Index].x = x + ObjPtr->x + ABS_SIZE(dx);
                  pv[Index].y = y + ObjPtr->y + ABS_SIZE(dy);
               }
            }
            switch (ObjPtr->type)
            {
               case OBJ_POLY:
                  switch (curved)
                  {
                     case LT_STRAIGHT:
                     case LT_SPLINE:
                        sv = MakeMultiSplinePolyVertex (&sn, smooth,
                              drawOrigX, drawOrigY, NumPts, pv);
                        break;
                     case LT_INTSPLINE:
                        free(cntrlv);
                        sv = MakeIntSplinePolyVertex (&sn, &intn, &cntrlv,
                              drawOrigX, drawOrigY, NumPts, pv);
                        break;
                  }
                  break;
               case OBJ_POLYGON:
                  switch (curved)
                  {
                     case LT_STRAIGHT:
                     case LT_SPLINE:
                        sv = MakeMultiSplinePolygonVertex (&sn, smooth,
                              drawOrigX, drawOrigY, NumPts, pv);
                        break;
                     case LT_INTSPLINE:
                        free(cntrlv);
                        sv = MakeIntSplinePolygonVertex (&sn, &intn, &cntrlv,
                              drawOrigX, drawOrigY, NumPts, pv);
                        break;
                  }
                  break;
            }
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, sv, sn,
                  CoordModeOrigin);
         }
         else
            MyDashedLine (drawWindow, revDefaultGC, v, num);
         PixelToMeasurementUnit(x_buf, ABS_SIZE(grid_x-XGridOff));
         PixelToMeasurementUnit(y_buf, ABS_SIZE(grid_y-YGridOff));
         sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
         ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   PixelToMeasurementUnit(x_buf, ABS_SIZE(grid_x-XGridOff));
   PixelToMeasurementUnit(y_buf, ABS_SIZE(grid_y-YGridOff));
   sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
   EndShowMeasureCursor (grid_x, grid_y, buf, TRUE);
   if (dx != 0 || dy != 0)
   {
      if (splineRubberband)
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, sv, sn,
               CoordModeOrigin);
      else
         MyDashedLine (drawWindow, revDefaultGC, v, num);

      HighLightReverse ();

      PrepareToReplaceAnObj (ObjPtr);

      dx = ABS_SIZE(dx);
      dy = ABS_SIZE(dy);
      switch (ObjPtr->type)
      {
         case OBJ_POLY:
            if (ObjPtr->ctm == NULL) {
               V[Index].x += dx; V[Index].y += dy;
            } else {
               int x, y, new_x, new_y;

               TransformPointThroughCTM(V[Index].x-ObjPtr->x,
                     V[Index].y-ObjPtr->y, ObjPtr->ctm, &x, &y);
               x += ObjPtr->x + dx;
               y += ObjPtr->y + dy;
               ReverseTransformPointThroughCTM(x-ObjPtr->x, y-ObjPtr->y,
                     ObjPtr->ctm, &new_x, &new_y);
               V[Index].x = new_x + ObjPtr->x;
               V[Index].y = new_y + ObjPtr->y;
            }
            AdjObjSplineVs (ObjPtr);
            if (ObjPtr->detail.p->curved != LT_INTSPLINE)
               UpdPolyBBox (ObjPtr, NumPts, V);
            else
               UpdPolyBBox (ObjPtr, ObjPtr->detail.p->intn,
                     ObjPtr->detail.p->intvlist);
            break;
         case OBJ_POLYGON:
            if (ObjPtr->ctm == NULL) {
               V[Index].x += dx; V[Index].y += dy;
            } else {
               int x, y, new_x, new_y;

               TransformPointThroughCTM(V[Index].x-ObjPtr->x,
                     V[Index].y-ObjPtr->y, ObjPtr->ctm, &x, &y);
               x += ObjPtr->x + dx;
               y += ObjPtr->y + dy;
               ReverseTransformPointThroughCTM(x-ObjPtr->x, y-ObjPtr->y,
                     ObjPtr->ctm, &new_x, &new_y);
               V[Index].x = new_x + ObjPtr->x;
               V[Index].y = new_y + ObjPtr->y;
            }
            if (Index == 0)
            {
               V[NumPts-1].x = V[Index].x; V[NumPts-1].y = V[Index].y;
            }
            else if (Index == NumPts-1)
            {
               V[0].x = V[Index].x; V[0].y = V[Index].y;
            }
            AdjObjSplineVs (ObjPtr);
            if (ObjPtr->detail.g->curved != LT_INTSPLINE)
               UpdPolyBBox (ObjPtr, NumPts, V);
            else
               UpdPolyBBox (ObjPtr, ObjPtr->detail.g->intn,
                     ObjPtr->detail.g->intvlist);
            break;
      }
      if (auto_center_attr)
      {
         struct AttrRec	* attr_ptr=ObjPtr->fattr;
         int		modified=FALSE;

         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
            if (attr_ptr->shown)
            {
               struct BBRec	bbox;

               CenterObjInOBBox (attr_ptr->obj, ObjPtr->obbox, &bbox);
               if (bbox.ltx < ltx) ltx = bbox.ltx;
               if (bbox.lty < lty) lty = bbox.lty;
               if (bbox.rbx > rbx) rbx = bbox.rbx;
               if (bbox.rby > rby) rby = bbox.rby;
               modified = TRUE;
            }
         if (modified) AdjObjBBox (ObjPtr);
      }
      RecordReplaceAnObj (ObjPtr);

      UpdSelBBox ();
      RedrawAreas (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
            ObjPtr->bbox.ltx-GRID_ABS_SIZE(1),
            ObjPtr->bbox.lty-GRID_ABS_SIZE(1),
            ObjPtr->bbox.rbx+GRID_ABS_SIZE(1),
            ObjPtr->bbox.rby+GRID_ABS_SIZE(1));
      SetFileModified (TRUE);
      justDupped = FALSE;

      HighLightForward ();
   }
   if (splineRubberband) {
      free(sv);
      free(pv);
      if (smooth != NULL) free(smooth);
      if (curved == LT_INTSPLINE && cntrlv != NULL) free(cntrlv);
   }
}

static double	multX=(double)0.0, multY=(double)0.0;
static int	pivotX=0, pivotY=0, changeX=0, changeY=0, moveX=0, moveY=0;
static int	absPivotX=0, absPivotY=0;

static
void StretchedXY(X, Y, NewX, NewY)
   int X, Y, * NewX, * NewY; /* screen offsets */
{
   register int dx, dy;

   dx = round((double)((double)(X - pivotX) * multX));
   dy = round((double)((double)(Y - pivotY) * multY));
   *NewX = pivotX + dx;
   *NewY = pivotY + dy;
}

static
void StretchedAbsXY(X, Y, NewX, NewY)
   int X, Y, * NewX, * NewY; /* screen offsets */
{
   register int dx, dy;

   dx = round((double)((double)(X - absPivotX) * multX));
   dy = round((double)((double)(Y - absPivotY) * multY));
   *NewX = absPivotX + dx;
   *NewY = absPivotY + dy;
}

static
void ShearedXY(Corner, x, y, x_shear, y_shear, x_scale, y_scale, new_x, new_y)
   int Corner, x, y, x_shear, y_shear, x_scale, y_scale, * new_x, * new_y;
{
   double val, dx, dy;

   if (Corner != CORNER_NONE && Corner != CORNER_RIGHT &&
         Corner != CORNER_LEFT) {
      if (y == pivotY) {
         *new_x = x;
         *new_y = y;
      } else {
         dy = ((double)(y-pivotY))*((double)y_scale)/1000.0;
         val = tan(((double)x_shear)/1000.0)*dy;
         *new_x = round(val + x);
         *new_y = round(dy + pivotY);
      }
   }
   if (Corner != CORNER_NONE && Corner != CORNER_TOP &&
         Corner != CORNER_BOTTOM) {
      if (x == pivotX) {
         *new_x = x;
         *new_y = y;
      } else {
         dx = ((double)(x-pivotX))*((double)x_scale)/1000.0;
         val = tan(((double)y_shear)/1000.0)*dx;
         *new_x = round(dx + pivotX);
         *new_y = round(val + y);
      }
   }
}

static
void ShearedAbsXY(Corner, x, y, x_shear, y_shear, x_scale, y_scale, new_x,
      new_y)
   int Corner, x, y, x_shear, y_shear, x_scale, y_scale, * new_x, * new_y;
{
   double val, dx, dy;

   if (Corner != CORNER_NONE && Corner != CORNER_RIGHT &&
         Corner != CORNER_LEFT) {
      if (y == pivotY) {
         *new_x = x;
         *new_y = y;
      } else {
         dy = ((double)(y-absPivotY))*((double)y_scale)/1000.0;
         val = tan(((double)x_shear)/1000.0)*dy;
         *new_x = round(val + x);
         *new_y = round(dy + absPivotY);
      }
   }
   if (Corner != CORNER_NONE && Corner != CORNER_TOP &&
         Corner != CORNER_BOTTOM) {
      if (x == pivotX) {
         *new_x = x;
         *new_y = y;
      } else {
         dx = ((double)(x-absPivotX))*((double)x_scale)/1000.0;
         val = tan(((double)y_shear)/1000.0)*dx;
         *new_x = round(dx + absPivotX);
         *new_y = round(val + y);
      }
   }
}

static
void SetPivot(Corner, OBBox)
   int Corner;
   struct BBRec OBBox;
   /* pivotX, pivotY, moveX, moveY will be set to screen offsets */
{
   switch (Corner) {
   case CORNER_NONE: /* same as CORNER_CC */
      pivotX = moveX = ((OBBox.ltx+OBBox.rbx)>>1);
      pivotY = moveY = ((OBBox.lty+OBBox.rby)>>1);
      changeX = TRUE; changeY = TRUE;
      break;
   case CORNER_LT:
      pivotX = OBBox.rbx; pivotY = OBBox.rby;
      moveX = OBBox.ltx; moveY = OBBox.lty;
      changeX = changeY = TRUE;
      break;
   case CORNER_TOP:
      pivotX = moveX = ((OBBox.ltx+OBBox.rbx)>>1); pivotY = OBBox.rby;
      moveY = OBBox.lty;
      changeX = FALSE; changeY = TRUE;
      break;
   case CORNER_RT:
      pivotX = OBBox.ltx; pivotY = OBBox.rby;
      moveX = OBBox.rbx; moveY = OBBox.lty;
      changeX = changeY = TRUE;
      break;
   case CORNER_RIGHT:
      pivotX = OBBox.ltx; pivotY = moveY = ((OBBox.lty+OBBox.rby)>>1);
      moveX = OBBox.rbx;
      changeX = TRUE; changeY = FALSE;
      break;
   case CORNER_RB:
      pivotX = OBBox.ltx; pivotY = OBBox.lty;
      moveX = OBBox.rbx; moveY = OBBox.rby;
      changeX = changeY = TRUE;
      break;
   case CORNER_BOTTOM:
      pivotX = moveX = ((OBBox.ltx+OBBox.rbx)>>1); pivotY = OBBox.lty;
      moveY = OBBox.rby;
      changeX = FALSE; changeY = TRUE;
      break;
   case CORNER_LB:
      pivotX = OBBox.rbx; pivotY = OBBox.lty;
      moveX = OBBox.ltx; moveY = OBBox.rby;
      changeX = changeY = TRUE;
      break;
   case CORNER_LEFT:
      pivotX = OBBox.rbx; pivotY = moveY = ((OBBox.lty+OBBox.rby)>>1);
      moveX = OBBox.ltx;
      changeX = TRUE; changeY = FALSE;
      break;
   }
   multX = 1.0;
   multY = 1.0;
   absPivotX = pivotX;
   absPivotY = pivotY;
   pivotX = OFFSET_X(absPivotX);
   pivotY = OFFSET_Y(absPivotY);
   moveX = OFFSET_X(moveX);
   moveY = OFFSET_Y(moveY);
}

void ShearObj(ObjPtr, Corner, XShear, YShear, XScale, YScale, RealLtX, RealLtY)
   struct ObjRec *ObjPtr;
   int Corner;
   int XShear, YShear, XScale, YScale; /* scaled by 1000 */
   int *RealLtX, *RealLtY;
{
   IntPoint abs_obj_obbox_vs[5];
   int x, y, new_ltx, new_lty, new_rbx, new_rby;
   double tan_val;
   struct XfrmMtrxRec ctm, new_ctm;
   struct ObjRec *obj_ptr;
   struct AttrRec *attr_ptr;
   int auto_center_attr=AutoCenterAttr(ObjPtr);

   switch (ObjPtr->type) {
   case OBJ_GROUP:
   case OBJ_SYM:
   case OBJ_ICON:
      for (obj_ptr=ObjPtr->detail.r->first; obj_ptr != NULL;
            obj_ptr=obj_ptr->next) {
         ShearObj(obj_ptr, Corner, XShear, YShear, XScale, YScale,
               RealLtX, RealLtY);
      }
      break;

   default:
      if (ObjPtr->ctm == NULL) {
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
      }
      ShearedAbsXY(Corner, ObjPtr->x+ObjPtr->ctm->m[CTM_TX],
            ObjPtr->y+ObjPtr->ctm->m[CTM_TY], XShear, YShear, XScale, YScale,
            &x, &y);
      switch (Corner) {
      case CORNER_TOP:
      case CORNER_BOTTOM:
         tan_val = tan(((double)XShear)/1000.0);
         ctm.m[CTM_SX] = 1000;
         ctm.m[CTM_SY] = YScale;
         ctm.m[CTM_SIN] = 0;
         ctm.m[CTM_MSIN] = round(((double)YScale)*tan_val);
         break;
      case CORNER_RIGHT:
      case CORNER_LEFT:
         tan_val = tan(((double)YShear)/1000.0);
         ctm.m[CTM_SX] = XScale;
         ctm.m[CTM_SY] = 1000;
         ctm.m[CTM_SIN] = round(((double)XScale)*tan_val);
         ctm.m[CTM_MSIN] = 0;
         break;
      default:
         ctm.m[CTM_SX] = XScale;
         ctm.m[CTM_SY] = YScale;
         ctm.m[CTM_SIN] = 0;
         ctm.m[CTM_MSIN] = 0;
         break;
      }
      ctm.m[CTM_TX] = 0;
      ctm.m[CTM_TY] = 0;
      ConcatCTM(ObjPtr->ctm, &ctm, &new_ctm);
      new_ctm.m[CTM_TX] = x-ObjPtr->x;
      new_ctm.m[CTM_TY] = y-ObjPtr->y;
      memcpy(ObjPtr->ctm, &new_ctm, sizeof(struct XfrmMtrxRec));

      GetTransformedOBBoxAbsVs(ObjPtr, abs_obj_obbox_vs);

      new_ltx = min(min(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
            min(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
      new_rbx = max(max(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
            max(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
      new_lty = min(min(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
            min(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));
      new_rby = max(max(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
            max(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));

      ObjPtr->obbox.ltx = new_ltx; ObjPtr->obbox.lty = new_lty;
      ObjPtr->obbox.rbx = new_rbx; ObjPtr->obbox.rby = new_rby;
      if (RealLtX != NULL && RealLtY != NULL) {
         MoveObj(ObjPtr, (*RealLtX)-new_ltx, (*RealLtY)-new_lty);
      }
      break;
   }
   AdjObjOBBox(ObjPtr);
   if (auto_center_attr) {
      for (attr_ptr=ObjPtr->fattr; attr_ptr != NULL; attr_ptr=attr_ptr->next) {
         ShearObj(attr_ptr->obj, Corner, XShear, YShear, XScale, YScale,
               NULL, NULL);
         if (attr_ptr->shown) {
            CenterObjInOBBox(attr_ptr->obj, ObjPtr->obbox, NULL);
         }
      }
   } else {
      for (attr_ptr=ObjPtr->fattr; attr_ptr != NULL; attr_ptr=attr_ptr->next) {
         ShearObj(attr_ptr->obj, Corner, XShear, YShear, XScale, YScale,
               NULL, NULL);
      }
   }
   AdjObjSplineVs(ObjPtr);
   AdjObjCache(ObjPtr);
   AdjObjBBox(ObjPtr);
}

static
void StretchSimpleText(ObjPtr, Corner, XScale, YScale)
   struct ObjRec *ObjPtr;
   int Corner, XScale, YScale;
{
   /* !stratchableText */
   if (ObjPtr->ctm == NULL) {
      int new_x, new_y, h=ABS_SIZE(ObjPtr->obbox.rby-ObjPtr->obbox.lty);

      StretchedAbsXY(ObjPtr->x, ObjPtr->y, &new_x, &new_y);
      ObjPtr->x = new_x;
      ObjPtr->y = new_y;
      if (multX < 0) {
         ObjPtr->detail.t->just = MAXJUSTS - 1 - ObjPtr->detail.t->just;
      }
      if (multY < 0) ObjPtr->y -= h;
   } else {
      int abs_x, abs_y, new_x, new_y;

      abs_x = ObjPtr->x+ObjPtr->ctm->m[CTM_TX];
      abs_y = ObjPtr->y+ObjPtr->ctm->m[CTM_TY];
      StretchedAbsXY(abs_x, abs_y, &new_x, &new_y);
      if (multX < 0.0 || multY < 0.0) {
         int new_ltx, new_lty, new_rbx, new_rby;
         IntPoint abs_obj_obbox_vs[5];
         struct XfrmMtrxRec ctm, new_ctm;

         ctm.m[CTM_SX] = ctm.m[CTM_SY] = 1000;
         ctm.m[CTM_SIN] = ctm.m[CTM_MSIN] = 0;
         ctm.m[CTM_TX] = ctm.m[CTM_TY] = 0;
         ctm.m[CTM_SX] = (multX < 0.0) ? (-1000) : 1000;
         ctm.m[CTM_SY] = (multY < 0.0) ? (-1000) : 1000;
         ConcatCTM(ObjPtr->ctm, &ctm, &new_ctm);
         new_ctm.m[CTM_TX] = new_x-ObjPtr->x;
         new_ctm.m[CTM_TY] = new_y-ObjPtr->y;
         memcpy(ObjPtr->ctm, &new_ctm, sizeof(struct XfrmMtrxRec));

         GetTransformedOBBoxAbsVs(ObjPtr, abs_obj_obbox_vs);

         new_ltx = min(min(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
               min(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
         new_rbx = max(max(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
               max(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
         new_lty = min(min(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
               min(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));
         new_rby = max(max(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
               max(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));

         ObjPtr->obbox.ltx = new_ltx; ObjPtr->obbox.lty = new_lty;
         ObjPtr->obbox.rbx = new_rbx; ObjPtr->obbox.rby = new_rby;
         abs_x = ObjPtr->x+ObjPtr->ctm->m[CTM_TX];
         abs_y = ObjPtr->y+ObjPtr->ctm->m[CTM_TY];
      }
      MoveObj(ObjPtr, new_x-abs_x, new_y-abs_y);
   }
   UpdTextBBox(ObjPtr);
   AdjObjSplineVs(ObjPtr);
   AdjObjBBox(ObjPtr);
}

static void StretchObj ARGS_DECL((struct ObjRec *, int Corner, int XScale,
      int YScale));

static
void StretchAttr(ObjPtr, Corner, XScale, YScale, AutoCenterAttr)
   struct ObjRec *ObjPtr;
   int Corner, XScale, YScale, AutoCenterAttr;
{
   struct AttrRec *attr_ptr=ObjPtr->fattr;

   if (AutoCenterAttr) {
      for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
         if (attr_ptr->shown) {
            CenterObjInOBBox(attr_ptr->obj, ObjPtr->obbox, NULL);
         } else {
            StretchSimpleText(attr_ptr->obj, Corner, XScale, YScale);
         }
      }
   } else {
      for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
         StretchSimpleText(attr_ptr->obj, Corner, XScale, YScale);
      }
   }
}

static
void StretchSimpleArc(ObjPtr)
   struct ObjRec *ObjPtr;
{
   struct ArcRec *arc_ptr=ObjPtr->detail.a;
   int x, y;

   StretchedAbsXY(arc_ptr->xc, arc_ptr->yc, &x, &y);
   arc_ptr->xc = ObjPtr->x = x;
   arc_ptr->yc = ObjPtr->y = y;
   StretchedAbsXY(arc_ptr->x1, arc_ptr->y1, &x, &y);
   arc_ptr->x1 = x;
   arc_ptr->y1 = y;
   StretchedAbsXY(arc_ptr->x2, arc_ptr->y2, &x, &y);
   arc_ptr->x2 = x;
   arc_ptr->y2 = y;
   StretchedAbsXY(arc_ptr->ltx, arc_ptr->lty, &x, &y);
   arc_ptr->ltx = arc_ptr->xc-abs(x-arc_ptr->xc);
   arc_ptr->lty = arc_ptr->yc-abs(y-arc_ptr->yc);
   arc_ptr->w = (arc_ptr->xc-arc_ptr->ltx)<<1;
   arc_ptr->h = (arc_ptr->yc-arc_ptr->lty)<<1;

   if (multX < 0) {
      arc_ptr->dir = !(arc_ptr->dir);
      arc_ptr->angle2 = -(arc_ptr->angle2);
      if (arc_ptr->angle1 > 0) {
         arc_ptr->angle1 = (180*64) - arc_ptr->angle1;
      } else {
         arc_ptr->angle1 = (-180*64) - arc_ptr->angle1;
      }
   }
   if (multY < 0) {
      arc_ptr->dir = !(arc_ptr->dir);
      arc_ptr->angle1 = -(arc_ptr->angle1);
      arc_ptr->angle2 = -(arc_ptr->angle2);
   }
   AdjObjSplineVs(ObjPtr);
   AdjObjBBox(ObjPtr);
}

static
void StretchSimplePoly(ObjPtr)
   struct ObjRec *ObjPtr;
{
   int i, ltx=0, lty=0, rbx=0, rby=0;
   struct PolyRec *poly_ptr= ObjPtr->detail.p;
   IntPoint *vs=poly_ptr->vlist;

   for (i = 0; i < poly_ptr->n; i++) {
      int x, y;

      StretchedAbsXY(vs[i].x, vs[i].y, &x, &y);
      vs[i].x = x;
      vs[i].y = y;
      if (i == 0) {
         ltx = rbx = x;
         lty = rby = y;
      } else {
         if (x < ltx) ltx = x; if (y < lty) lty = y;
         if (x > rbx) rbx = x; if (y > rby) rby = y;
      }
   }
   ObjPtr->obbox.ltx = ObjPtr->x = ltx;
   ObjPtr->obbox.lty = ObjPtr->y = lty;
   ObjPtr->obbox.rbx = rbx;
   ObjPtr->obbox.rby = rby;
   AdjObjSplineVs(ObjPtr);
   if (poly_ptr->curved == LT_INTSPLINE) {
      UpdPolyBBox(ObjPtr, poly_ptr->intn, poly_ptr->intvlist);
   }
}

static
void StretchSimplePolygon(ObjPtr)
   struct ObjRec *ObjPtr;
{
   int i, ltx=0, lty=0, rbx=0, rby=0;
   struct PolygonRec *polygon_ptr= ObjPtr->detail.g;
   IntPoint *vs=polygon_ptr->vlist;

   for (i = 0; i < polygon_ptr->n; i++) {
      int x, y;

      StretchedAbsXY(vs[i].x, vs[i].y, &x, &y);
      vs[i].x = x;
      vs[i].y = y;
      if (i == 0) {
         ltx = rbx = x;
         lty = rby = y;
      } else {
         if (x < ltx) ltx = x; if (y < lty) lty = y;
         if (x > rbx) rbx = x; if (y > rby) rby = y;
      }
   }
   ObjPtr->obbox.ltx = ObjPtr->x = ltx;
   ObjPtr->obbox.lty = ObjPtr->y = lty;
   ObjPtr->obbox.rbx = rbx;
   ObjPtr->obbox.rby = rby;
   AdjObjSplineVs(ObjPtr);
   if (polygon_ptr->curved == LT_INTSPLINE) {
      UpdPolyBBox(ObjPtr, polygon_ptr->intn, polygon_ptr->intvlist);
   }
}

static
void StretchSimpleObj(ObjPtr, Corner, XScale, YScale, FinalOBBox,
      auto_center_attr)
   struct ObjRec *ObjPtr;
   int Corner, XScale, YScale, auto_center_attr;
   struct BBRec *FinalOBBox;
{
   ObjPtr->obbox.ltx = ObjPtr->x = FinalOBBox->ltx;
   ObjPtr->obbox.lty = ObjPtr->y = FinalOBBox->lty;
   ObjPtr->obbox.rbx = FinalOBBox->rbx;
   ObjPtr->obbox.rby = FinalOBBox->rby;

   switch (ObjPtr->type) {
   case OBJ_ARC: StretchSimpleArc(ObjPtr); break;
   case OBJ_POLY: StretchSimplePoly(ObjPtr); break;
   case OBJ_POLYGON: StretchSimplePolygon(ObjPtr); break;
   case OBJ_RCBOX: AdjObjSplineVs(ObjPtr); break;
   case OBJ_BOX: AdjObjSplineVs(ObjPtr); break;
   case OBJ_OVAL: AdjObjSplineVs(ObjPtr); break;
   }
   AdjObjOBBox(ObjPtr);
   StretchAttr(ObjPtr, Corner, XScale, YScale, auto_center_attr);
   AdjObjBBox(ObjPtr);
}

static
void StretchObj(ObjPtr, Corner, XScale, YScale)
   struct ObjRec *ObjPtr;
   int Corner, XScale, YScale;
{
   int ltx, lty, rbx, rby;
   int auto_center_attr=AutoCenterAttr(ObjPtr);
   struct BBRec final_obbox;
   struct ObjRec *obj_ptr;
   struct AttrRec *saved_fattr, *saved_lattr;

   StretchedAbsXY(ObjPtr->obbox.ltx, ObjPtr->obbox.lty, &ltx, &lty);
   StretchedAbsXY(ObjPtr->obbox.rbx, ObjPtr->obbox.rby, &rbx, &rby);
   CalcBBox(ltx, lty, rbx, rby, &final_obbox.ltx, &final_obbox.lty,
         &final_obbox.rbx, &final_obbox.rby);

   switch (ObjPtr->type) {
   case OBJ_POLY:
   case OBJ_POLYGON:
   case OBJ_BOX:
   case OBJ_OVAL:
   case OBJ_ARC:
   case OBJ_RCBOX:
      if (ObjPtr->ctm == NULL) {
         StretchSimpleObj(ObjPtr, Corner, XScale, YScale, &final_obbox,
               auto_center_attr);
      } else {
         ShearObj(ObjPtr, Corner, 0, 0, XScale, YScale,
               &final_obbox.ltx, &final_obbox.lty);
      }
      break;
   case OBJ_TEXT:
      if (stretchableText) {
         ShearObj(ObjPtr, Corner, 0, 0, XScale, YScale,
               &final_obbox.ltx, &final_obbox.lty);
      } else {
         StretchSimpleText(ObjPtr, Corner, XScale, YScale);
      }
      break;
   case OBJ_GROUP:
   case OBJ_SYM:
   case OBJ_ICON:
      for (obj_ptr=ObjPtr->detail.r->first; obj_ptr != NULL;
            obj_ptr=obj_ptr->next) {
         StretchObj(obj_ptr, Corner, XScale, YScale);
      }
      AdjObjOBBox(ObjPtr);
      StretchAttr(ObjPtr, Corner, XScale, YScale, auto_center_attr);
      AdjObjSplineVs(ObjPtr);
      AdjObjBBox(ObjPtr);
      break;
   case OBJ_XBM:
   case OBJ_XPM:
      saved_fattr = ObjPtr->fattr;
      saved_lattr = ObjPtr->lattr;
      ObjPtr->fattr = ObjPtr->lattr = NULL;
      ShearObj(ObjPtr, Corner, 0, 0, XScale, YScale,
            &final_obbox.ltx, &final_obbox.lty);
      ObjPtr->fattr = saved_fattr;
      ObjPtr->lattr = saved_lattr;
      StretchAttr(ObjPtr, Corner, XScale, YScale, auto_center_attr);
      AdjObjBBox(ObjPtr);
      break;
   }
}

static
void StretchAllSelObjects (Corner, XScale, YScale)
   int	Corner, XScale, YScale;
{
   struct SelRec	* sel_ptr;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
      if (!sel_ptr->obj->locked)
         StretchObj (sel_ptr->obj, Corner, XScale, YScale);
   if (numObjLocked != 0) Msg ("Locked objects are not stretched.");
}

static
void MarkObjectsForStretch ()
{
   register struct ObjRec	* obj_ptr;
   register struct SelRec	* sel_ptr;

   for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev)
      obj_ptr->marked = FALSE;

   for (sel_ptr = botSel; sel_ptr != NULL; sel_ptr = sel_ptr->prev)
      sel_ptr->obj->marked = TRUE;
}

static
int ConstrainedStretchAllSel (Corner, XScale, YScale, ltx, lty, rbx, rby)
   int	Corner, XScale, YScale;
   int	* ltx, * lty, * rbx, * rby;
{
   register struct ObjRec	* obj_ptr;
   int				something_stretched=FALSE, num_pts;
   int				x_off, y_off, move_first, move_last, x, y;
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
            int	index=INVALID, seg_dx, seg_dy, dx, dy;
            int	cur_seg_dx, cur_seg_dy;

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
            if (move_first && move_last && num_pts==3)
            {
               StretchedAbsXY (v[0].x, v[0].y, &x, &y);
               dx = x-v[0].x; dy = y-v[0].y;
               index = 1;
               cur_seg_dx = v[index-1].x - v[index].x;
               cur_seg_dy = v[index-1].y - v[index].y;
               seg_dx = v[index].x - v[index+1].x;
               seg_dy = v[index].y - v[index+1].y;

               if (cur_seg_dy==0 && seg_dx==0 &&
                     (seg_dy!=0 || (seg_dy==0 && dx==0)))
                  v[index].y += dy;
               else if (cur_seg_dx==0 && seg_dy==0 &&
                     (seg_dx!=0 || (seg_dx==0 && dy==0)))
                  v[index].x += dx;
            }
            else
            {
               if (move_first && num_pts>2)
               {
                  StretchedAbsXY (v[0].x, v[0].y, &x, &y);
                  dx = x-v[0].x; dy = y-v[0].y;
                  index = 1;
                  cur_seg_dx = v[index-1].x - v[index].x;
                  cur_seg_dy = v[index-1].y - v[index].y;
                  seg_dx = v[index].x - v[index+1].x;
                  seg_dy = v[index].y - v[index+1].y;

                  if (cur_seg_dy==0 && cur_seg_dx!=0 &&
                        (seg_dy!=0 || (seg_dy==0 && dx==0)))
                     v[index].y += dy;
                  else if (cur_seg_dx==0 && cur_seg_dy!=0 &&
                        (seg_dx!=0 || (seg_dx==0 && dy==0)))
                     v[index].x += dx;
               }
               if (move_last && num_pts>2)
               {
                  StretchedAbsXY (v[num_pts-1].x, v[num_pts-1].y, &x, &y);
                  dx = x-v[num_pts-1].x; dy = y-v[num_pts-1].y;
                  index = num_pts-2;
                  cur_seg_dx = v[index+1].x - v[index].x;
                  cur_seg_dy = v[index+1].y - v[index].y;
                  seg_dx = v[index].x - v[index-1].x;
                  seg_dy = v[index].y - v[index-1].y;

                  if (cur_seg_dy==0 && cur_seg_dx!=0 &&
                        (seg_dy!=0 || (seg_dy==0 && dx==0)))
                     v[index].y += dy;
                  else if (cur_seg_dx==0 && cur_seg_dy!=0 &&
                        (seg_dx!=0 || (seg_dx==0 && dy==0)))
                     v[index].x += dx;
               }
            }
            if (move_first)
            {
               StretchedAbsXY (v[0].x, v[0].y, &x, &y);
               v[0].x = x; v[0].y = y;
            }
            if (move_last)
            {
               StretchedAbsXY (v[num_pts-1].x, v[num_pts-1].y, &x, &y);
               v[num_pts-1].x = x; v[num_pts-1].y = y;
            }
            AdjObjSplineVs (obj_ptr);
            switch (obj_ptr->type)
            {
               case OBJ_POLY:
                  if (obj_ptr->detail.p->curved != LT_INTSPLINE)
                     UpdPolyBBox (obj_ptr, num_pts, v);
                  else
                     UpdPolyBBox (obj_ptr, obj_ptr->detail.p->intn,
                           obj_ptr->detail.p->intvlist);
                  break;
               case OBJ_POLYGON:
                  if (obj_ptr->detail.g->curved != LT_INTSPLINE)
                     UpdPolyBBox (obj_ptr, num_pts, v);
                  else
                     UpdPolyBBox (obj_ptr, obj_ptr->detail.g->intn,
                           obj_ptr->detail.g->intvlist);
                  break;
            }
            AdjObjBBox (obj_ptr);
            if (AutoCenterAttr (obj_ptr))
            {
               struct AttrRec	* attr_ptr=obj_ptr->fattr;
               int		modified=FALSE;

               for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
                  if (attr_ptr->shown)
                  {
                     struct BBRec	bbox;

                     CenterObjInOBBox (attr_ptr->obj, obj_ptr->obbox, &bbox);
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
   return (something_stretched);
}

static
void StretchAllSel (Corner, XScale, YScale)
   int	Corner, XScale, YScale; /* XScale and YScale are scaled by 1000 */
{
   int	ltx, lty, rbx, rby, saved_ltx, saved_lty, saved_rbx, saved_rby;
   int	poly_stretched;

   saved_ltx = selLtX; saved_lty = selLtY;
   saved_rbx = selRbX; saved_rby = selRbY;

   if (moveMode==CONST_MOVE)
   {
      MarkObjectsForStretch ();

      StartCompositeCmd ();
      PrepareToRecord (CMD_STRETCH, topSel, botSel, numObjSelected);
      RecordCmd (CMD_STRETCH, NULL, topSel, botSel, numObjSelected);

      poly_stretched = ConstrainedStretchAllSel(Corner, XScale, YScale,
            &ltx, &lty, &rbx, &rby);
      StretchAllSelObjects (Corner, XScale, YScale);
      UpdSelBBox ();
      if (poly_stretched)
      {
         ltx = min(ltx,min(selLtX,saved_ltx));
         lty = min(lty,min(selLtY,saved_lty));
         rbx = max(rbx,max(selRbX,saved_rbx));
         rby = max(rby,max(selRbY,saved_rby));
         RedrawAnArea (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
               rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1));
      }
      else
      {
         RedrawAreas (botObj, saved_ltx-GRID_ABS_SIZE(1),
               saved_lty-GRID_ABS_SIZE(1),
               saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
               selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
               selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      }
      EndCompositeCmd ();
   }
   else
   {
      PrepareToRecord (CMD_STRETCH, topSel, botSel, numObjSelected);
      StretchAllSelObjects (Corner, XScale, YScale);
      RecordCmd (CMD_STRETCH, NULL, topSel, botSel, numObjSelected);
      UpdSelBBox ();
      RedrawAreas (botObj, saved_ltx-GRID_ABS_SIZE(1),
            saved_lty-GRID_ABS_SIZE(1),
            saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
            selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   }
}

static
void GetMeasurement (ObjPtr, buf)
   struct ObjRec	* ObjPtr;
   char			* buf;
{
   int	ltx, lty, rbx, rby, real_ltx, real_lty, real_rbx, real_rby;
   char	x_buf[80], y_buf[80];

   StretchedAbsXY (ObjPtr->obbox.ltx, ObjPtr->obbox.lty, &ltx, &lty);
   StretchedAbsXY (ObjPtr->obbox.rbx, ObjPtr->obbox.rby, &rbx, &rby);
   CalcBBox (ltx, lty, rbx, rby, &real_ltx, &real_lty, &real_rbx, &real_rby);
   PixelToMeasurementUnit(x_buf, abs(real_rbx-real_ltx));
   PixelToMeasurementUnit(y_buf, abs(real_rby-real_lty));
   sprintf (buf, "%sx%s", x_buf, y_buf);
}

static
void StretchBox (XGridOff, YGridOff, ObjPtr, Corner)
   int			XGridOff, YGridOff, Corner;
   struct ObjRec	* ObjPtr;
{
   int		x, y, stretching=TRUE;
   int		ltx, lty, rbx, rby, sel_ltx, sel_lty, sel_rbx, sel_rby;
   int		stretched_ltx, stretched_lty, stretched_rbx, stretched_rby;
   int		stretched_sel_ltx, stretched_sel_lty, stretched_sel_rbx;
   int		stretched_sel_rby;
   int		ruler_ltx, ruler_lty, ruler_rbx, ruler_rby;
   int		sel_obj_ltx, sel_obj_lty, sel_obj_rbx, sel_obj_rby;
   int		grid_x = XGridOff, grid_y = YGridOff, proportional=FALSE;
   char		buf[80], x_buf[80], y_buf[80];
   double	obj_w, obj_h;
   XEvent	input, ev;

   if (numObjSelected == numObjLocked)
   {
      Msg ("Locked object(s) can not be stretched.");
      return;
   }

   XFlush (mainDisplay);
   XSync (mainDisplay, False);

   if (XCheckMaskEvent (mainDisplay, ExposureMask, &ev) ||
         XCheckMaskEvent (mainDisplay, VisibilityChangeMask, &ev))
      ExposeEventHandler (&ev, TRUE);

   SetPivot (Corner, ObjPtr->obbox);

   stretched_sel_ltx = sel_ltx = OFFSET_X(selLtX);
   stretched_sel_lty = sel_lty = OFFSET_Y(selLtY);
   stretched_sel_rbx = sel_rbx = OFFSET_X(selRbX);
   stretched_sel_rby = sel_rby = OFFSET_Y(selRbY);
   SelBox (drawWindow, revDefaultGC, stretched_sel_ltx-2, stretched_sel_lty-2,
         stretched_sel_rbx+2, stretched_sel_rby+2);

   ruler_ltx = sel_obj_ltx = OFFSET_X(selObjLtX);
   ruler_lty = sel_obj_lty = OFFSET_Y(selObjLtY);
   ruler_rbx = sel_obj_rbx = OFFSET_X(selObjRbX);
   ruler_rby = sel_obj_rby = OFFSET_Y(selObjRbY);

   stretched_ltx = ltx = OFFSET_X(ObjPtr->obbox.ltx);
   stretched_lty = lty = OFFSET_Y(ObjPtr->obbox.lty);
   stretched_rbx = rbx = OFFSET_X(ObjPtr->obbox.rbx);
   stretched_rby = rby = OFFSET_Y(ObjPtr->obbox.rby);
   SelBox (drawWindow, revDefaultGC, stretched_ltx, stretched_lty,
         stretched_rbx, stretched_rby);

   if (ltx == rbx)
   {
      Msg ("Can not stretch!  Object has ZERO width!");
      SelBox (drawWindow, revDefaultGC, stretched_ltx, stretched_lty,
            stretched_rbx, stretched_rby);
      SelBox (drawWindow, revDefaultGC, stretched_sel_ltx-2,
            stretched_sel_lty-2, stretched_sel_rbx+2, stretched_sel_rby+2);
      return;
   }
   else if (lty == rby)
   {
      Msg ("Can not stretch!  Object has ZERO height!");
      SelBox (drawWindow, revDefaultGC, stretched_ltx, stretched_lty,
            stretched_rbx, stretched_rby);
      SelBox (drawWindow, revDefaultGC, stretched_sel_ltx-2,
            stretched_sel_lty-2, stretched_sel_rbx+2, stretched_sel_rby+2);
      return;
   }

   obj_w = (double)(moveX - pivotX);
   obj_h = (double)(moveY - pivotY);

   PixelToMeasurementUnit(x_buf, ObjPtr->obbox.rbx-ObjPtr->obbox.ltx);
   PixelToMeasurementUnit(y_buf, ObjPtr->obbox.rby-ObjPtr->obbox.lty);
   sprintf (buf, "%sx%s", x_buf, y_buf);
   StartShowMeasureCursor (grid_x, grid_y, buf, TRUE);
   BeginIntervalRulers (ruler_ltx, ruler_lty, ruler_rbx, ruler_rby);
   XGrabPointer (mainDisplay, drawWindow, False,
         PointerMotionMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);

   while (stretching)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == ButtonRelease)
      {
         proportional = input.xbutton.state & (ShiftMask|ControlMask);
         XUngrabPointer (mainDisplay, CurrentTime);
         XSync (mainDisplay, False);
         stretching = FALSE;
      }
      else if (input.type == MotionNotify)
      {
         proportional = input.xmotion.state & (ShiftMask|ControlMask);

         GetMeasurement (ObjPtr, buf);
         EndShowMeasureCursor (grid_x, grid_y, buf, TRUE);

         x = input.xmotion.x;
         y = input.xmotion.y;
         GridXY (x, y, &grid_x, &grid_y);

         SelBox (drawWindow, revDefaultGC, stretched_ltx, stretched_lty,
               stretched_rbx, stretched_rby);
         SelBox (drawWindow, revDefaultGC, stretched_sel_ltx-2,
               stretched_sel_lty-2, stretched_sel_rbx+2, stretched_sel_rby+2);

         if (proportional)
         {
            int		new_w, new_h;
            double	w_ratio, h_ratio;

            new_w = moveX + grid_x - XGridOff - pivotX;
            new_h = moveY + grid_y - YGridOff - pivotY;
            w_ratio = (moveX!=pivotX) ? fabs(((double)new_w)/obj_w) : 0.0;
            h_ratio = (moveY!=pivotY) ? fabs(((double)new_h)/obj_h) : 0.0;
            if (changeX && changeY)
            {
               if (w_ratio >= h_ratio)
               {
                  multX = (moveX!=pivotX) ? ((double)new_w)/obj_w : 1.0;
                  multY = fabs(multX) * ((new_h*obj_h>=0) ? 1.0 : -1.0);
               }
               else
               {
                  multX = fabs(multY) * ((new_w*obj_w>=0) ? 1.0 : -1.0);
                  multY = (moveY!=pivotY) ? ((double)new_h)/obj_h : 1.0;
               }
            }
            else if (changeX)
            {
               multX = (moveX!=pivotX) ? ((double)new_w)/obj_w : 1.0;
               multY = fabs(multX);
            }
            else if (changeY)
            {
               multX = fabs(multY);
               multY = (moveY!=pivotY) ? ((double)new_h)/obj_h : 1.0;
            }
         }
         else
         {
            if (changeX)
               multX = (moveX!=pivotX) ?
                     (double)(moveX+grid_x-XGridOff-pivotX)/obj_w : 1.0;
            else
               multX = (double)1.0;
            if (changeY)
               multY = (moveY!=pivotY) ?
                     (double)(moveY+grid_y-YGridOff-pivotY)/obj_h : 1.0;
            else
               multY = (double)1.0;
         }

         StretchedXY (sel_ltx, sel_lty, &stretched_sel_ltx, &stretched_sel_lty);
         StretchedXY (sel_rbx, sel_rby, &stretched_sel_rbx, &stretched_sel_rby);
         StretchedXY (ltx, lty, &stretched_ltx, &stretched_lty);
         StretchedXY (rbx, rby, &stretched_rbx, &stretched_rby);
         StretchedXY (sel_obj_ltx, sel_obj_lty, &ruler_ltx, &ruler_lty);
         StretchedXY (sel_obj_rbx, sel_obj_rby, &ruler_rbx, &ruler_rby);

         DrawIntervalRulers (ruler_ltx, ruler_lty, ruler_rbx, ruler_rby);
         SelBox (drawWindow, revDefaultGC, stretched_sel_ltx-2,
               stretched_sel_lty-2, stretched_sel_rbx+2, stretched_sel_rby+2);
         SelBox (drawWindow, revDefaultGC, stretched_ltx, stretched_lty,
               stretched_rbx, stretched_rby);
         GetMeasurement (ObjPtr, buf);
         EndShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   EndIntervalRulers (grid_x, grid_y);
   GetMeasurement (ObjPtr, buf);
   EndShowMeasureCursor (grid_x, grid_y, buf, TRUE);
   SelBox (drawWindow, revDefaultGC, stretched_ltx, stretched_lty,
         stretched_rbx, stretched_rby);
   SelBox (drawWindow, revDefaultGC, stretched_sel_ltx-2,
         stretched_sel_lty-2, stretched_sel_rbx+2, stretched_sel_rby+2);
   if (multX != (double)1.0 || multY != (double)1.0)
   {
      int	x_scale=1000, y_scale=1000;

      PointsToShearScale (Corner, pivotX, pivotY, moveX, moveY,
            moveX+grid_x-XGridOff, moveY+grid_y-YGridOff,
            NULL, NULL, &x_scale, &y_scale);
      if (proportional) {
         int abs_x_scale=abs(x_scale), abs_y_scale=abs(y_scale);

         if (abs_x_scale > abs_y_scale) {
            y_scale = x_scale;
         } else if (abs_x_scale < abs_y_scale) {
            x_scale = y_scale;
         }
      }
      HighLightReverse ();
      StretchAllSel (Corner, x_scale, y_scale);
      HighLightForward ();
      SetFileModified (TRUE);
      justDupped = FALSE;
   }
}

void StretchSel (XGridOff, YGridOff, ObjPtr, Corner)
   int			XGridOff, YGridOff, Corner;
   struct ObjRec	* ObjPtr;
{
   switch (ObjPtr->type)
   {
      case OBJ_BOX:
      case OBJ_OVAL:
      case OBJ_GROUP:
      case OBJ_ICON:
      case OBJ_ARC:
      case OBJ_RCBOX:
      case OBJ_SYM:
      case OBJ_XBM:
      case OBJ_XPM:
         StretchBox (XGridOff, YGridOff, ObjPtr, Corner);
         break;
      case OBJ_POLY:
         StretchPoly (XGridOff, YGridOff, ObjPtr, ObjPtr->detail.p->n,
               ObjPtr->detail.p->vlist, Corner);
         break;
      case OBJ_POLYGON:
         StretchPoly (XGridOff, YGridOff, ObjPtr, ObjPtr->detail.g->n,
               ObjPtr->detail.g->vlist, Corner);
         break;
      case OBJ_TEXT:
         if (stretchableText) {
            StretchBox (XGridOff, YGridOff, ObjPtr, Corner);
         }
         break;
   }
}

void ScaleAnEPSObj (ObjPtr, ScalingFactor)
   struct ObjRec	* ObjPtr;
   float		* ScalingFactor;
{
   struct BBRec	* obbox = &(ObjPtr->obbox);

   multX = multY = (double) (*ScalingFactor);
   changeX = changeY = (fabs(multX-1.0) > 1.0e-6);
   if (!changeX && !changeY) return;;

   absPivotX = obbox->ltx;
   absPivotY = obbox->lty;
   moveX = obbox->rbx;
   moveY = obbox->rby;
   StretchObj (ObjPtr, CORNER_RB, (int)(multX * 1000.0), (int)(multY * 1000.0));
}

static
char * FindColon(s)
   register char	* s;
{
   while (*s!=':' && *s!='x' && *s!='X' && *s!=' ' && *s!='\0') s++;
   return ((*s==':' || *s=='x' || *s=='X' || *s==' ') ? (s) : (char *)NULL);
}

static
void ScaleAllSelObjects (Corner, XScale, YScale)
   int	Corner, XScale, YScale;
{
   register struct SelRec	* sel_ptr;
   register struct BBRec	* obbox;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
      if (!sel_ptr->obj->locked)
      {
         obbox = &(sel_ptr->obj->obbox);
         switch (Corner)
         {
            case CORNER_NONE:
               moveX = absPivotX = (obbox->ltx+obbox->rbx)>>1;
               moveY = absPivotY = (obbox->lty+obbox->rby)>>1;
               break;
            case CORNER_LT:
               absPivotX = obbox->rbx; absPivotY = obbox->rby;
               moveX = obbox->ltx; moveY = obbox->lty;
               break;
            case CORNER_TOP:
               moveX = absPivotX = (obbox->ltx+obbox->rbx)>>1;
               absPivotY = obbox->rby;
               moveY = obbox->lty;
               break;
            case CORNER_RT:
               absPivotX = obbox->ltx; absPivotY = obbox->rby;
               moveX = obbox->rbx; moveY = obbox->lty;
               break;
            case CORNER_RIGHT:
               absPivotX = obbox->ltx;
               moveX = obbox->rbx;
               moveY = absPivotY = (obbox->lty+obbox->rby)>>1;
               break;
            case CORNER_RB:
               absPivotX = obbox->ltx; absPivotY = obbox->lty;
               moveX = obbox->rbx; moveY = obbox->rby;
               break;
            case CORNER_BOTTOM:
               moveX = absPivotX = (obbox->ltx+obbox->rbx)>>1;
               absPivotY = obbox->lty;
               moveY = obbox->rby;
               break;
            case CORNER_LB:
               absPivotX = obbox->rbx; absPivotY = obbox->lty;
               moveX = obbox->ltx; moveY = obbox->rby;
               break;
            case CORNER_LEFT:
               absPivotX = obbox->rbx;
               moveX = obbox->ltx;
               moveY = absPivotY = (obbox->lty+obbox->rby)>>1;
               break;
         }
         StretchObj (sel_ptr->obj, Corner, XScale, YScale);
      }
   if (numObjLocked != 0) Msg ("Locked objects are not scaled.");
}

static
void ScaleAllSel (Corner, XScale, YScale)
   int	Corner, XScale, YScale;
{
   int	ltx, lty, rbx, rby, saved_ltx, saved_lty, saved_rbx, saved_rby;
   int	poly_stretched;

   saved_ltx = selLtX; saved_lty = selLtY;
   saved_rbx = selRbX; saved_rby = selRbY;

   if (moveMode==CONST_MOVE)
   {
      MarkObjectsForStretch ();

      StartCompositeCmd ();
      PrepareToRecord (CMD_STRETCH, topSel, botSel, numObjSelected);
      RecordCmd (CMD_STRETCH, NULL, topSel, botSel, numObjSelected);

      poly_stretched = ConstrainedStretchAllSel(Corner, XScale, YScale,
            &ltx, &lty, &rbx, &rby);
      ScaleAllSelObjects (Corner, XScale, YScale);
      UpdSelBBox ();
      if (poly_stretched)
      {
         ltx = min(ltx,min(selLtX,saved_ltx));
         lty = min(lty,min(selLtY,saved_lty));
         rbx = max(rbx,max(selRbX,saved_rbx));
         rby = max(rby,max(selRbY,saved_rby));
         RedrawAnArea (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
               rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1));
      }
      else
      {
         RedrawAreas (botObj, saved_ltx-GRID_ABS_SIZE(1),
               saved_lty-GRID_ABS_SIZE(1),
               saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
               selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
               selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      }
      EndCompositeCmd ();
   }
   else
   {
      PrepareToRecord (CMD_STRETCH, topSel, botSel, numObjSelected);
      ScaleAllSelObjects (Corner, XScale, YScale);
      RecordCmd (CMD_STRETCH, NULL, topSel, botSel, numObjSelected);
      UpdSelBBox ();
      RedrawAreas (botObj, saved_ltx-GRID_ABS_SIZE(1),
            saved_lty-GRID_ABS_SIZE(1),
            saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
            selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   }
}

void ScaleAllSelObj ()
{
   char		spec[MAXSTRING], * y_spec;
   int		corner=INVALID;
   struct BBRec	obbox;

   if (topSel == NULL) return;
   if (numObjSelected == numObjLocked)
   {
      Msg ("Locked objects can not be scaled.");
      return;
   }

   Dialog ("Please enter scaling factor: [X:Y] or [Scale]",
         "( <CR>: accept, <ESC>: cancel )", spec);
   if (*spec == '\0') return;

   switch (horiAlign)
   {
      case ALIGN_N:
      case ALIGN_S:
         switch (vertAlign)
         {
            case ALIGN_N: corner = CORNER_NONE; break;
            case ALIGN_S: corner = CORNER_NONE; break;
            case ALIGN_T: corner = CORNER_BOTTOM; break;
            case ALIGN_M: corner = CORNER_NONE; break;
            case ALIGN_B: corner = CORNER_TOP; break;
         }
         break;
      case ALIGN_L:
         switch (vertAlign)
         {
            case ALIGN_N: corner = CORNER_RIGHT; break;
            case ALIGN_S: corner = CORNER_RIGHT; break;
            case ALIGN_T: corner = CORNER_RB; break;
            case ALIGN_M: corner = CORNER_RIGHT; break;
            case ALIGN_B: corner = CORNER_RT; break;
         }
         break;
      case ALIGN_C:
         switch (vertAlign)
         {
            case ALIGN_N: corner = CORNER_NONE; break;
            case ALIGN_S: corner = CORNER_NONE; break;
            case ALIGN_T: corner = CORNER_BOTTOM; break;
            case ALIGN_M: corner = CORNER_NONE; break;
            case ALIGN_B: corner = CORNER_TOP; break;
         }
         break;
      case ALIGN_R:
         switch (vertAlign)
         {
            case ALIGN_N: corner = CORNER_LEFT; break;
            case ALIGN_S: corner = CORNER_LEFT; break;
            case ALIGN_T: corner = CORNER_LB; break;
            case ALIGN_M: corner = CORNER_LEFT; break;
            case ALIGN_B: corner = CORNER_LT; break;
         }
         break;
   }
   obbox.ltx = selObjLtX; obbox.lty = selObjLtY;
   obbox.rbx = selObjRbX; obbox.rby = selObjRbY;
   SetPivot(corner, obbox);

   if ((y_spec = FindColon (spec)) == NULL)
   {
      sscanf (spec, "%lf", &multX);
      if (multX <= 0.0)
      {
         Msg ("Invalid scaling specification.");
         return;
      }
      multY = multX;
   }
   else
   {
      *y_spec++ = '\0';
      sscanf (spec, "%lf", &multX);
      sscanf (y_spec, "%lf", &multY);
      if (multX <= 0.0 || multY <= 0.0)
      {
         Msg ("Invalid scaling specification.");
         return;
      }
   }
   changeX = (fabs(multX-1.0) > 1.0e-6);
   changeY = (fabs(multY-1.0) > 1.0e-6);
   if (!changeX && !changeY) return;

   HighLightReverse ();
   ScaleAllSel (corner, (int)(multX * 1000.0), (int)(multY * 1000.0));
   HighLightForward ();
   SetFileModified (TRUE);
   justDupped = FALSE;
}

void SizeAllSelObj(AbsW, AbsH)
   int AbsW, AbsH;
{
   int saved_h_align=horiAlign, saved_v_align=vertAlign;
   struct BBRec obbox;

   if (topSel == NULL) return;
   if (AbsW == selObjRbX-selObjLtX && AbsH == selObjRbY-selObjLtY) return;

   obbox.ltx = selObjLtX; obbox.lty = selObjLtY;
   obbox.rbx = selObjRbX; obbox.rby = selObjRbY;
   SetPivot(CORNER_RB, obbox);

   horiAlign = ALIGN_L;
   vertAlign = ALIGN_T;
   multX = (selObjRbX==selObjLtX ? ((double)1.0) :
         ((double)AbsW) / ((double)selObjRbX-selObjLtX));
   multY = (selObjRbY==selObjLtY ? ((double)1.0) :
         ((double)AbsH) / ((double)selObjRbY-selObjLtY));
   changeX = (fabs(multX-1.0) > 1.0e-6);
   changeY = (fabs(multY-1.0) > 1.0e-6);
   ScaleAllSel(CORNER_RB);
   horiAlign = saved_h_align;
   vertAlign = saved_v_align;

   UpdSelBBox();
   SetFileModified(TRUE);
   justDupped = FALSE;
}

void FlipObjHorizontal (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   int two_x_pivot=selObjLtX+selObjRbX;
   int new_obj_ltx=two_x_pivot-ObjPtr->obbox.rbx;
   int new_obj_rbx=two_x_pivot-ObjPtr->obbox.ltx;
   int new_obj_lty=ObjPtr->obbox.lty;

   if (ObjPtr->ctm == NULL && ObjPtr->type != OBJ_XBM &&
         ObjPtr->type != OBJ_XPM) {
      register IntPoint *v;
      register int i;
      int num_pts;
      struct ObjRec *obj_ptr;
      struct AttrRec *attr_ptr;
      struct ArcRec *arc_ptr;

      switch (ObjPtr->type) {
      case OBJ_TEXT:
         switch (ObjPtr->detail.t->rotate) {
         case ROTATE0:
         case ROTATE180:
            ObjPtr->x = two_x_pivot - ObjPtr->x;
            if (ObjPtr->detail.t->just != JUST_C) {
               ObjPtr->detail.t->just = MAXJUSTS-1-ObjPtr->detail.t->just;
               if (ObjPtr->detail.t->cached_bitmap != None) {
                  XFreePixmap(mainDisplay, ObjPtr->detail.t->cached_bitmap);
               }
               ObjPtr->detail.t->cached_bitmap = None;

               if (zoomScale != 0) {
                  ObjPtr->detail.t->cached_zoom = 0;
               } else {
                  ObjPtr->detail.t->cached_rotate = INVALID;
               }
            }
            break;

         case ROTATE90: ObjPtr->x = new_obj_rbx; break;
         case ROTATE270: ObjPtr->x = new_obj_ltx; break;
         }
         UpdTextBBox(ObjPtr);
         break;

      default:
         switch (ObjPtr->type) {
         case OBJ_XBM:
            switch (ObjPtr->detail.xbm->rotate) {
            case ROTATE0:
            case ROTATE180:
               ObjPtr->detail.xbm->flip ^= HORI_EVEN;
               break;
            case ROTATE90:
            case ROTATE270:
               ObjPtr->detail.xbm->flip ^= HORI_ODD;
               break;
            }
            if (ObjPtr->detail.xbm->cached_bitmap != None) {
               XFreePixmap(mainDisplay, ObjPtr->detail.xbm->cached_bitmap);
            }
            ObjPtr->detail.xbm->cached_bitmap = None;

            if (zoomScale != 0) {
               ObjPtr->detail.xbm->cached_zoom = 0;
            } else {
               ObjPtr->detail.xbm->cached_rotate = INVALID;
            }
            break;
         case OBJ_XPM:
            switch (ObjPtr->detail.xpm->rotate) {
            case ROTATE0:
            case ROTATE180:
               ObjPtr->detail.xpm->flip ^= HORI_EVEN;
               break;
            case ROTATE90:
            case ROTATE270:
               ObjPtr->detail.xpm->flip ^= HORI_ODD;
               break;
            }
            if (ObjPtr->detail.xpm->cached_pixmap != None) {
               XFreePixmap(mainDisplay, ObjPtr->detail.xpm->cached_pixmap);
            }
            ObjPtr->detail.xpm->cached_pixmap = None;
            if (ObjPtr->detail.xpm->cached_bitmap != None) {
               XFreePixmap(mainDisplay, ObjPtr->detail.xpm->cached_bitmap);
            }
            ObjPtr->detail.xpm->cached_bitmap = None;
            ObjPtr->detail.xpm->cached_color = (-1);

            if (zoomScale != 0) {
               ObjPtr->detail.xpm->cached_zoom = 0;
            } else {
               ObjPtr->detail.xpm->cached_rotate = INVALID;
            }
            break;
         case OBJ_ICON:
            switch (ObjPtr->detail.r->rotate) {
            case ROTATE0:
            case ROTATE180: ObjPtr->detail.r->flip ^= HORI_EVEN; break;

            case ROTATE90:
            case ROTATE270: ObjPtr->detail.r->flip ^= HORI_ODD; break;
            }
            break;
         }
         ObjPtr->obbox.ltx = ObjPtr->x = new_obj_ltx;
         ObjPtr->obbox.rbx = new_obj_rbx;
         break;
      }

      switch (ObjPtr->type) {
      case OBJ_POLY:
         num_pts = ObjPtr->detail.p->n;
         v = ObjPtr->detail.p->vlist;
         for (i = 0; i < num_pts; i++, v++) (*v).x = two_x_pivot - (*v).x;
         AdjObjSplineVs(ObjPtr);
         attr_ptr = ObjPtr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
            FlipObjHorizontal(attr_ptr->obj);
         }
         break;
      case OBJ_POLYGON:
         num_pts = ObjPtr->detail.g->n;
         v = ObjPtr->detail.g->vlist;
         for (i = 0; i < num_pts; i++, v++) (*v).x = two_x_pivot - (*v).x;
         AdjObjSplineVs(ObjPtr);
         attr_ptr = ObjPtr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
            FlipObjHorizontal(attr_ptr->obj);
         }
         break;
      case OBJ_BOX:
      case OBJ_OVAL:
      case OBJ_RCBOX:
      case OBJ_XBM:
      case OBJ_XPM:
         attr_ptr = ObjPtr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
            FlipObjHorizontal(attr_ptr->obj);
         }
         AdjObjSplineVs(ObjPtr);
         break;
      case OBJ_TEXT:
         AdjObjSplineVs (ObjPtr);
         break;
      case OBJ_ARC:
         arc_ptr = ObjPtr->detail.a;
         arc_ptr->xc = two_x_pivot - arc_ptr->xc;
         arc_ptr->x1 = two_x_pivot - arc_ptr->x1;
         arc_ptr->x2 = two_x_pivot - arc_ptr->x2;
         arc_ptr->dir = !(arc_ptr->dir);
         arc_ptr->ltx = two_x_pivot - arc_ptr->ltx - arc_ptr->w;
         if (arc_ptr->angle1 > 0) {
            arc_ptr->angle1 = (180*64) - arc_ptr->angle1;
         } else {
            arc_ptr->angle1 = (-180)*64 - arc_ptr->angle1;
         }
         arc_ptr->angle2 = -(arc_ptr->angle2);
         AdjObjBBox(ObjPtr);
         attr_ptr = ObjPtr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
            FlipObjHorizontal(attr_ptr->obj);
         }
         AdjObjSplineVs(ObjPtr);
         break;
      case OBJ_GROUP:
      case OBJ_SYM:
      case OBJ_ICON:
         obj_ptr = ObjPtr->detail.r->first;
         for ( ; obj_ptr != NULL; obj_ptr = obj_ptr->next) {
            FlipObjHorizontal(obj_ptr);
         }
         attr_ptr = ObjPtr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
            FlipObjHorizontal(attr_ptr->obj);
         }
         AdjObjSplineVs(ObjPtr);
         break;
      }
      AdjObjBBox(ObjPtr);
   } else {
      ShearObj(ObjPtr, CORNER_LEFT, 0, 0, -1000, 1000, NULL, NULL);
      MoveObj(ObjPtr, new_obj_ltx-ObjPtr->obbox.ltx,
            new_obj_lty-ObjPtr->obbox.lty);
   }
   SetFileModified(TRUE);
}

void FlipIconHorizontal (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register int		two_x_pivot;
   int			new_obj_ltx, new_obj_rbx;
   struct ObjRec	* obj_ptr;
   struct AttrRec	* attr_ptr;

   two_x_pivot = selObjLtX + selObjRbX;
   new_obj_ltx = two_x_pivot - ObjPtr->obbox.rbx;
   new_obj_rbx = two_x_pivot - ObjPtr->obbox.ltx;

   switch (ObjPtr->detail.r->rotate)
   {
      case ROTATE0:
      case ROTATE180: ObjPtr->detail.r->flip ^= HORI_EVEN; break;

      case ROTATE90:
      case ROTATE270: ObjPtr->detail.r->flip ^= HORI_ODD; break;
   }

   ObjPtr->obbox.ltx = ObjPtr->x = new_obj_ltx;
   ObjPtr->obbox.rbx = new_obj_rbx;

   obj_ptr = ObjPtr->detail.r->first;
   for ( ; obj_ptr != NULL; obj_ptr = obj_ptr->next)
      FlipObjHorizontal (obj_ptr);
   attr_ptr = ObjPtr->fattr;
   for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
      FlipObjHorizontal (attr_ptr->obj);

   AdjObjBBox (ObjPtr);
}

void FlipObjVertical (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   int two_x_pivot=selObjLtY+selObjRbY;
   int new_obj_lty=two_x_pivot-ObjPtr->obbox.rby;
   int new_obj_rby=two_x_pivot-ObjPtr->obbox.lty;
   int new_obj_ltx=ObjPtr->obbox.ltx;

   if (ObjPtr->ctm == NULL && ObjPtr->type != OBJ_XBM &&
         ObjPtr->type != OBJ_XPM) {
      register IntPoint *v;
      register int i;
      int num_pts;
      struct ObjRec *obj_ptr;
      struct AttrRec *attr_ptr;
      struct ArcRec *arc_ptr;

      switch (ObjPtr->type) {
      case OBJ_TEXT:
         switch (ObjPtr->detail.t->rotate) {
         case ROTATE0: ObjPtr->y = new_obj_lty; break;
         case ROTATE180: ObjPtr->y = new_obj_rby; break;

         case ROTATE90:
         case ROTATE270:
            ObjPtr->y = two_x_pivot - ObjPtr->y;
            if (ObjPtr->detail.t->just != JUST_C) {
               ObjPtr->detail.t->just = MAXJUSTS-1-ObjPtr->detail.t->just;
               if (ObjPtr->detail.t->cached_bitmap != None) {
                  XFreePixmap (mainDisplay, ObjPtr->detail.t->cached_bitmap);
               }
               ObjPtr->detail.t->cached_bitmap = None;

               if (zoomScale != 0) {
                  ObjPtr->detail.t->cached_zoom = 0;
               } else {
                  ObjPtr->detail.t->cached_rotate = INVALID;
               }
            }
            break;
         }
         UpdTextBBox (ObjPtr);
         break;

      default:
         switch (ObjPtr->type) {
         case OBJ_XBM:
            switch (ObjPtr->detail.xbm->rotate) {
            case ROTATE0:
            case ROTATE180: ObjPtr->detail.xbm->flip ^= VERT_EVEN; break;

            case ROTATE90:
            case ROTATE270: ObjPtr->detail.xbm->flip ^= VERT_ODD; break;
            }
            if (ObjPtr->detail.xbm->cached_bitmap != None) {
               XFreePixmap (mainDisplay, ObjPtr->detail.xbm->cached_bitmap);
            }
            ObjPtr->detail.xbm->cached_bitmap = None;

            if (zoomScale != 0) {
               ObjPtr->detail.xbm->cached_zoom = 0;
            } else {
               ObjPtr->detail.xbm->cached_rotate = INVALID;
            }
            break;
         case OBJ_XPM:
            switch (ObjPtr->detail.xpm->rotate) {
            case ROTATE0:
            case ROTATE180: ObjPtr->detail.xpm->flip ^= VERT_EVEN; break;

            case ROTATE90:
            case ROTATE270: ObjPtr->detail.xpm->flip ^= VERT_ODD; break;
            }
            if (ObjPtr->detail.xpm->cached_pixmap != None) {
               XFreePixmap (mainDisplay, ObjPtr->detail.xpm->cached_pixmap);
            }
            ObjPtr->detail.xpm->cached_pixmap = None;
            if (ObjPtr->detail.xpm->cached_bitmap != None) {
               XFreePixmap (mainDisplay, ObjPtr->detail.xpm->cached_bitmap);
            }
            ObjPtr->detail.xpm->cached_bitmap = None;
            ObjPtr->detail.xpm->cached_color = (-1);

            if (zoomScale != 0) {
               ObjPtr->detail.xpm->cached_zoom = 0;
            } else {
               ObjPtr->detail.xpm->cached_rotate = INVALID;
            }
            break;
         case OBJ_ICON:
            switch (ObjPtr->detail.r->rotate) {
            case ROTATE0:
            case ROTATE180: ObjPtr->detail.r->flip ^= VERT_EVEN; break;

            case ROTATE90:
            case ROTATE270: ObjPtr->detail.r->flip ^= VERT_ODD; break;
            }
            break;
         }
         ObjPtr->obbox.lty = ObjPtr->y = new_obj_lty;
         ObjPtr->obbox.rby = new_obj_rby;
         break;
      }

      switch (ObjPtr->type) {
      case OBJ_POLY:
         num_pts = ObjPtr->detail.p->n;
         v = ObjPtr->detail.p->vlist;
         for (i = 0; i < num_pts; i++, v++) (*v).y = two_x_pivot - (*v).y;
         AdjObjSplineVs (ObjPtr);
         attr_ptr = ObjPtr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
            FlipObjVertical (attr_ptr->obj);
         }
         break;
      case OBJ_POLYGON:
         num_pts = ObjPtr->detail.g->n;
         v = ObjPtr->detail.g->vlist;
         for (i = 0; i < num_pts; i++, v++) (*v).y = two_x_pivot - (*v).y;
         AdjObjSplineVs (ObjPtr);
         attr_ptr = ObjPtr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
            FlipObjVertical (attr_ptr->obj);
         }
         break;
      case OBJ_BOX:
      case OBJ_OVAL:
      case OBJ_RCBOX:
      case OBJ_XBM:
      case OBJ_XPM:
         attr_ptr = ObjPtr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
            FlipObjVertical (attr_ptr->obj);
         }
         AdjObjSplineVs (ObjPtr);
         break;
      case OBJ_TEXT:
         AdjObjSplineVs (ObjPtr);
         break;
      case OBJ_ARC:
         arc_ptr = ObjPtr->detail.a;
         arc_ptr->yc = two_x_pivot - arc_ptr->yc;
         arc_ptr->y1 = two_x_pivot - arc_ptr->y1;
         arc_ptr->y2 = two_x_pivot - arc_ptr->y2;
         arc_ptr->dir = !(arc_ptr->dir);
         arc_ptr->lty = two_x_pivot - arc_ptr->lty - arc_ptr->h;
         arc_ptr->angle1 = -(arc_ptr->angle1);
         arc_ptr->angle2 = -(arc_ptr->angle2);
         AdjObjBBox(ObjPtr);
         attr_ptr = ObjPtr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
            FlipObjVertical (attr_ptr->obj);
         }
         AdjObjSplineVs (ObjPtr);
         break;
      case OBJ_GROUP:
      case OBJ_SYM:
      case OBJ_ICON:
         obj_ptr = ObjPtr->detail.r->first;
         for ( ; obj_ptr != NULL; obj_ptr = obj_ptr->next) {
            FlipObjVertical (obj_ptr);
         }
         attr_ptr = ObjPtr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
            FlipObjVertical (attr_ptr->obj);
         }
         AdjObjSplineVs (ObjPtr);
         break;
      }
      AdjObjBBox (ObjPtr);
   } else {
      ShearObj(ObjPtr, CORNER_TOP, 0, 0, 1000, -1000, NULL, NULL);
      MoveObj(ObjPtr, new_obj_ltx-ObjPtr->obbox.ltx,
            new_obj_lty-ObjPtr->obbox.lty);
   }
   SetFileModified (TRUE);
}

void FlipIconVertical (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register int		two_x_pivot;
   int			new_obj_lty, new_obj_rby;
   struct ObjRec	* obj_ptr;
   struct AttrRec	* attr_ptr;

   two_x_pivot = selObjLtY + selObjRbY;
   new_obj_lty = two_x_pivot - ObjPtr->obbox.rby;
   new_obj_rby = two_x_pivot - ObjPtr->obbox.lty;

   switch (ObjPtr->detail.r->rotate)
   {
      case ROTATE0:
      case ROTATE180: ObjPtr->detail.r->flip ^= VERT_EVEN; break;

      case ROTATE90:
      case ROTATE270: ObjPtr->detail.r->flip ^= VERT_ODD; break;
   }

   ObjPtr->obbox.lty = ObjPtr->y = new_obj_lty;
   ObjPtr->obbox.rby = new_obj_rby;

   obj_ptr = ObjPtr->detail.r->first;
   for ( ; obj_ptr != NULL; obj_ptr = obj_ptr->next)
      FlipObjVertical (obj_ptr);
   attr_ptr = ObjPtr->fattr;
   for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
      FlipObjVertical (attr_ptr->obj);

   AdjObjBBox (ObjPtr);
}

/* --------------------- Rotation --------------------- */

static int	rotatePivotX=0;
static int	rotatePivotY=0;

void SetRotatePivot()
{
   rotatePivotX = ((selObjLtX + selObjRbX)>>1);
   rotatePivotY = ((selObjLtY + selObjRbY)>>1);
}

void SetRotatePivotByObject (ObjPtr)
   struct ObjRec *ObjPtr;
{
   rotatePivotX = ((ObjPtr->obbox.ltx + ObjPtr->obbox.rbx)>>1);
   rotatePivotY = ((ObjPtr->obbox.lty + ObjPtr->obbox.rby)>>1);
}

static
void RotatePtClockWise (X, Y, NewX, NewY)
   int	X, Y, * NewX, * NewY;
{
   *NewX = rotatePivotX + rotatePivotY - Y;
   *NewY = rotatePivotY - rotatePivotX + X;
}

static
void RotatedXY (X, Y, AngleDelta, NewX, NewY)
   int	X, Y, AngleDelta, * NewX, * NewY; /* AngleDelta is degree*64 */
{
   register double	radian, sin_val, cos_val;
   int			dx=X-pivotX, dy=Y-pivotY;

   if (dx == 0 && dy == 0)
   {
      *NewX = pivotX;
      *NewY = pivotY;
   }
   else
   {
      radian = (((double)AngleDelta)*M_PI/180.0/64.0);
      sin_val = sin(radian);
      cos_val = cos(radian);
      *NewX = pivotX + round(dx*cos_val - dy*sin_val);
      *NewY = pivotY + round(dx*sin_val + dy*cos_val);
   }
}

static
void RotatedAbsXY (X, Y, AngleDelta, NewX, NewY)
   int	X, Y, AngleDelta, * NewX, * NewY; /* AngleDelta is degree*64 */
{
   register double	radian, sin_val, cos_val;
   int			dx=X-absPivotX, dy=Y-absPivotY;

   if (dx == 0 && dy == 0)
   {
      *NewX = absPivotX;
      *NewY = absPivotY;
   }
   else
   {
      radian = (((double)AngleDelta)*M_PI/180.0/64.0);
      sin_val = sin(radian);
      cos_val = cos(radian);
      *NewX = absPivotX + round(dx*cos_val - dy*sin_val);
      *NewY = absPivotY + round(dx*sin_val + dy*cos_val);
   }
}

void RotateObj (ObjPtr, Corner, AngleDelta, RealLtX, RealLtY)
   struct ObjRec	* ObjPtr;
   int			Corner, AngleDelta; /* AngleDelta is degree*64 */
   int			*RealLtX, *RealLtY;
{
   IntPoint		abs_obj_obbox_vs[5];
   int			x, y, new_ltx, new_lty, new_rbx, new_rby;
   double		radian=(((double)AngleDelta)*M_PI/180.0/64.0);
   double		sin_val=sin(radian), cos_val=cos(radian);
   struct XfrmMtrxRec	ctm, new_ctm;
   struct ObjRec	*obj_ptr;
   struct AttrRec	*attr_ptr;

   switch (ObjPtr->type) {
   case OBJ_GROUP:
   case OBJ_SYM:
   case OBJ_ICON:
      for (obj_ptr=ObjPtr->detail.r->first; obj_ptr != NULL;
            obj_ptr=obj_ptr->next) {
         RotateObj(obj_ptr, Corner, AngleDelta, RealLtX, RealLtY);
      }
      break;

   default:
      if (ObjPtr->ctm == NULL) {
         memcpy (&ObjPtr->orig_obbox, &ObjPtr->obbox, sizeof(struct BBRec));
         if (ObjPtr->type == OBJ_TEXT) {
            memcpy(&ObjPtr->detail.t->orig_bbox, &ObjPtr->bbox,
                  sizeof(struct BBRec));
         }
         ObjPtr->ctm = (struct XfrmMtrxRec *)malloc(sizeof(struct XfrmMtrxRec));
         if (ObjPtr->ctm == NULL) FailAllocMessage ();
         ObjPtr->ctm->m[CTM_SX] = ObjPtr->ctm->m[CTM_SY] = 1000;
         ObjPtr->ctm->m[CTM_SIN] = ObjPtr->ctm->m[CTM_MSIN] = 0;
         ObjPtr->ctm->m[CTM_TX] = ObjPtr->ctm->m[CTM_TY] = 0;
      }
      RotatedAbsXY (ObjPtr->x+ObjPtr->ctm->m[CTM_TX],
            ObjPtr->y+ObjPtr->ctm->m[CTM_TY], AngleDelta, &x, &y);
      ctm.m[CTM_SX] = ctm.m[CTM_SY] = round(1000.0*cos_val);
      ctm.m[CTM_SIN] = round(1000.0*sin_val);
      ctm.m[CTM_MSIN] = (-ctm.m[CTM_SIN]);
      ctm.m[CTM_TX] = 0;
      ctm.m[CTM_TY] = 0;
      ConcatCTM (ObjPtr->ctm, &ctm, &new_ctm);
      new_ctm.m[CTM_TX] = x-ObjPtr->x;
      new_ctm.m[CTM_TY] = y-ObjPtr->y;
      memcpy (ObjPtr->ctm, &new_ctm, sizeof(struct XfrmMtrxRec));

      GetTransformedOBBoxAbsVs (ObjPtr, abs_obj_obbox_vs);

      new_ltx = min(min(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
            min(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
      new_rbx = max(max(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
            max(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
      new_lty = min(min(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
            min(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));
      new_rby = max(max(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
            max(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));

      ObjPtr->obbox.ltx = new_ltx; ObjPtr->obbox.lty = new_lty;
      ObjPtr->obbox.rbx = new_rbx; ObjPtr->obbox.rby = new_rby;
      if (RealLtX != NULL && RealLtY != NULL) {
         int dx=(*RealLtX)-new_ltx, dy=(*RealLtY)-new_lty;

         ObjPtr->x += dx; ObjPtr->y += dy;
         ObjPtr->bbox.ltx += dx; ObjPtr->bbox.lty += dy;
         ObjPtr->bbox.rbx += dx; ObjPtr->bbox.rby += dy;
         ObjPtr->obbox.ltx += dx; ObjPtr->obbox.lty += dy;
         ObjPtr->obbox.rbx += dx; ObjPtr->obbox.rby += dy;
         MoveRotatedObjCache(ObjPtr, dx, dy);
      }
      break;
   }
   for (attr_ptr=ObjPtr->fattr; attr_ptr != NULL; attr_ptr=attr_ptr->next) {
      RotateObj(attr_ptr->obj, Corner, AngleDelta, RealLtX, RealLtY);
   }
   AdjObjCache (ObjPtr);
   AdjObjBBox (ObjPtr);
}

void RotateObjForLayout(ObjPtr, AngleInRadian, Corner)
   struct ObjRec *ObjPtr;
   double AngleInRadian;
{
   double angle=AngleInRadian*64.0*180.0/M_PI;

   SetPivot(Corner, ObjPtr->obbox);
   RotateObj(ObjPtr, Corner, round(angle), NULL, NULL);
}

void RotateObjClockWise(ObjPtr)
   struct ObjRec *ObjPtr;
{
   double angle_in_radian=((double)(rotationIncrement))*M_PI/180.0/64.0;
   double sin_val=sin(angle_in_radian);
   double cos_val=cos(angle_in_radian);
   int orig_x=((ObjPtr->obbox.ltx+ObjPtr->obbox.rbx)>>1);
   int orig_y=ObjPtr->obbox.lty;
   int x=0, y=0, dx=orig_x-rotatePivotX, dy=orig_y-rotatePivotY;

   if (dx != 0 || dy != 0) {
      x = (short)round(dx*cos_val - dy*sin_val);
      y = (short)round(dx*sin_val + dy*cos_val);
   }
   x += rotatePivotX;
   y += rotatePivotY;
   /* RotateObjForLayout() rotates about center-top */
   RotateObjForLayout(ObjPtr, angle_in_radian, CORNER_BOTTOM);
   MoveObj(ObjPtr, x-orig_x, y-orig_y);
   SetFileModified(TRUE);
}

void RotateIconClockWise(ObjPtr)
   struct ObjRec *ObjPtr;
{
   int ltx, lty, rbx, rby;
   struct ObjRec *obj_ptr;
   struct AttrRec *attr_ptr;

   SetRotatePivot();
/* ObjPtr->detail.r->rotate = (ObjPtr->detail.r->rotate+4+1) % 4; */
   RotatePtClockWise(ObjPtr->obbox.ltx, ObjPtr->obbox.rby, &ltx, &lty);
   RotatePtClockWise(ObjPtr->obbox.rbx, ObjPtr->obbox.lty, &rbx, &rby);
   ObjPtr->obbox.ltx = ObjPtr->x = ltx;
   ObjPtr->obbox.lty = ObjPtr->y = lty;
   ObjPtr->obbox.rbx = rbx;
   ObjPtr->obbox.rby = rby;

   obj_ptr = ObjPtr->detail.r->first;
   for ( ; obj_ptr != NULL; obj_ptr = obj_ptr->next) {
      RotateObjClockWise(obj_ptr);
   }
   attr_ptr = ObjPtr->fattr;
   for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
      RotateObjClockWise(attr_ptr->obj);
   }
   AdjObjBBox(ObjPtr);
}

static
void RotatePtCounter(X, Y, NewX, NewY)
   int X, Y, *NewX, *NewY;
{
   *NewX = rotatePivotX - rotatePivotY + Y;
   *NewY = rotatePivotY + rotatePivotX - X;
}

void RotateObjCounter(ObjPtr)
   struct ObjRec *ObjPtr;
{
   double angle_in_radian=((double)(-rotationIncrement))*M_PI/180.0/64.0;
   double sin_val=sin(angle_in_radian);
   double cos_val=cos(angle_in_radian);
   int orig_x=((ObjPtr->obbox.ltx+ObjPtr->obbox.rbx)>>1);
   int orig_y=ObjPtr->obbox.lty;
   int x=0, y=0, dx=orig_x-rotatePivotX, dy=orig_y-rotatePivotY;

   if (dx != 0 || dy != 0) {
      x = (short)round(dx*cos_val - dy*sin_val);
      y = (short)round(dx*sin_val + dy*cos_val);
   }
   x += rotatePivotX;
   y += rotatePivotY;
   /* RotateObjForLayout() rotates about center-top */
   RotateObjForLayout(ObjPtr, angle_in_radian, CORNER_BOTTOM);
   MoveObj(ObjPtr, x-orig_x, y-orig_y);
   SetFileModified(TRUE);
}

void RotateIconCounter(ObjPtr)
   struct ObjRec *ObjPtr;
{
   int ltx, lty, rbx, rby;
   struct ObjRec *obj_ptr;
   struct AttrRec *attr_ptr;

   SetRotatePivot();
/* ObjPtr->detail.r->rotate = (ObjPtr->detail.r->rotate+4-1) % 4; */
   RotatePtCounter(ObjPtr->obbox.rbx, ObjPtr->obbox.lty, &ltx, &lty);
   RotatePtCounter(ObjPtr->obbox.ltx, ObjPtr->obbox.rby, &rbx, &rby);
   ObjPtr->obbox.ltx = ObjPtr->x = ltx;
   ObjPtr->obbox.lty = ObjPtr->y = lty;
   ObjPtr->obbox.rbx = rbx;
   ObjPtr->obbox.rby = rby;

   obj_ptr = ObjPtr->detail.r->first;
   for ( ; obj_ptr != NULL; obj_ptr = obj_ptr->next) {
      RotateObjCounter(obj_ptr);
   }
   attr_ptr = ObjPtr->fattr;
   for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
      RotateObjCounter(attr_ptr->obj);
   }
   AdjObjBBox(ObjPtr);
}

void FlipHorizontal ()
{
   register struct SelRec *sel_ptr;
   int saved_ltx, saved_lty, saved_rbx, saved_rby;
   struct BBRec sel_obbox;

   if (topSel == NULL) return;
   if (numObjSelected == numObjLocked)
   {
      Msg ("Locked objects can not be flipped.");
      return;
   }
   sel_obbox.ltx = selObjLtX; sel_obbox.lty = selObjLtY;
   sel_obbox.rbx = selObjRbX; sel_obbox.rby = selObjRbY;
   SetPivot(CORNER_LEFT, sel_obbox);

   saved_ltx = selLtX; saved_lty = selLtY;
   saved_rbx = selRbX; saved_rby = selRbY;
   HighLightReverse ();
   PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);
   JustRemoveAllVSel ();
   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
      if (!sel_ptr->obj->locked)
         FlipObjHorizontal (sel_ptr->obj);
   UpdSelBBox ();
   RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
   RedrawAreas (botObj, saved_ltx-GRID_ABS_SIZE(1), saved_lty-GRID_ABS_SIZE(1),
         saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward ();
   justDupped = FALSE;
   if (numObjLocked != 0)
      Msg ("Locked objects are not flipped.");
   else
      Msg ("Flipped horizontally.");
}

void FlipVertical ()
{
   register struct SelRec *sel_ptr;
   int saved_ltx, saved_lty, saved_rbx, saved_rby;
   struct BBRec sel_obbox;

   if (topSel == NULL) return;
   if (numObjSelected == numObjLocked)
   {
      Msg ("Locked objects can not be flipped.");
      return;
   }
   sel_obbox.ltx = selObjLtX; sel_obbox.lty = selObjLtY;
   sel_obbox.rbx = selObjRbX; sel_obbox.rby = selObjRbY;
   SetPivot(CORNER_TOP, sel_obbox);

   saved_ltx = selLtX; saved_lty = selLtY;
   saved_rbx = selRbX; saved_rby = selRbY;
   HighLightReverse ();
   PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);
   JustRemoveAllVSel ();
   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
      if (!sel_ptr->obj->locked)
         FlipObjVertical (sel_ptr->obj);
   UpdSelBBox ();
   RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
   RedrawAreas (botObj, saved_ltx-GRID_ABS_SIZE(1), saved_lty-GRID_ABS_SIZE(1),
         saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward ();
   justDupped = FALSE;
   if (numObjLocked != 0)
      Msg ("Locked objects are not flipped.");
   else
      Msg ("Flipped vertically.");
}

/* --------------------- Rotate --------------------- */

void RotateClockWise()
{
   register struct SelRec *sel_ptr;
   int saved_ltx, saved_lty, saved_rbx, saved_rby;
   int text_obj_created, text_cursor_shown;

   if (topSel == NULL) {
      text_cursor_shown = textCursorShown;
      text_obj_created = TieLooseEnds();
      curRotate = (curRotate+1) & 0x3;
      ShowRotate();
      if (!text_obj_created && curChoice == DRAWTEXT && text_cursor_shown) {
         NewCurText();
         RedrawCurText();
      } else {
         textCursorShown = FALSE;
      }
      return;
   }
   if (numObjSelected == numObjLocked) {
      MsgBox("Locked objects can not be rotated.", TOOL_NAME, INFO_MB);
      return;
   }
   saved_ltx = selLtX; saved_lty = selLtY;
   saved_rbx = selRbX; saved_rby = selRbY;
   HighLightReverse();
   PrepareToRecord(CMD_REPLACE, topSel, botSel, numObjSelected);
   JustRemoveAllVSel();
   SetRotatePivot();
   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next) {
      if (!sel_ptr->obj->locked) {
         RotateObjClockWise(sel_ptr->obj);
      }
   }
   UpdSelBBox();
   RecordCmd(CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
   RedrawAreas(botObj, saved_ltx-GRID_ABS_SIZE(1), saved_lty-GRID_ABS_SIZE(1),
         saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward();
   justDupped = FALSE;
   if (numObjLocked != 0) {
      Msg("Locked objects are not rotated.");
   } else {
      Msg("Rotated clockwise.");
   }
}

void RotateCounter()
{
   register struct SelRec *sel_ptr;
   int saved_ltx, saved_lty, saved_rbx, saved_rby;
   int text_obj_created, text_cursor_shown;

   if (topSel == NULL) {
      text_cursor_shown = textCursorShown;
      text_obj_created = TieLooseEnds();
      curRotate = (curRotate+3) & 0x3;
      ShowRotate();
      if (!text_obj_created && curChoice == DRAWTEXT && text_cursor_shown) {
         NewCurText();
         RedrawCurText();
      } else {
         textCursorShown = FALSE;
      }
      return;
   }
   if (numObjSelected == numObjLocked) {
      MsgBox("Locked objects can not be rotated.", TOOL_NAME, INFO_MB);
      return;
   }
   saved_ltx = selLtX; saved_lty = selLtY;
   saved_rbx = selRbX; saved_rby = selRbY;
   HighLightReverse();
   PrepareToRecord(CMD_REPLACE, topSel, botSel, numObjSelected);
   JustRemoveAllVSel();
   SetRotatePivot();
   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next) {
      if (!sel_ptr->obj->locked) {
         RotateObjCounter(sel_ptr->obj);
      }
   }
   UpdSelBBox();
   RecordCmd(CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
   RedrawAreas(botObj, saved_ltx-GRID_ABS_SIZE(1), saved_lty-GRID_ABS_SIZE(1),
         saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward();
   justDupped = FALSE;
   if (numObjLocked != 0) {
      Msg("Locked objects are not rotated.");
   } else {
      Msg("Rotated counter-clockwise.");
   }
}

void SetTextRotation()
{
   char spec[80], buf[80];
   float fval;
   int ival;

   FormatAngle(textRotation, buf);
   sprintf(gszMsgBox, "%s: (current value is %s)",
         "Please enter text rotation in degrees", buf);
   if (Dialog(gszMsgBox, NULL, spec) == INVALID) return;
   UtilTrimBlanks(spec);
   if (sscanf(spec, "%f", &fval) != 1) {
      sprintf(gszMsgBox, "Cannot parse '%s'.\n\nPlease enter a numeric value!",
            spec);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   fval *= (float)64.0;
   ival = round(fval);
   if (ival < 0 || ival >= (360<<6)) {
      sprintf(gszMsgBox, "Please enter a value >= 0 and < 360!");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   textRotation = ival;
   ShowRotate();
   FormatAngle(textRotation, buf);
   sprintf(gszMsgBox, "Text rotation set to %s.", buf);
   Msg(gszMsgBox);
}

void SetRotationIncrement()
{
   char spec[80], buf[80];
   float fval;
   int ival;

   FormatAngle(rotationIncrement, buf);
   sprintf(gszMsgBox, "%s: (current value is %s)",
         "Please enter rotation increment in degrees", buf);
   if (Dialog(gszMsgBox, NULL, spec) == INVALID) return;
   UtilTrimBlanks(spec);
   if (sscanf(spec, "%f", &fval) != 1) {
      sprintf(gszMsgBox, "Cannot parse '%s'.\n\nPlease enter a numeric value!",
            spec);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   fval *= (float)64.0;
   ival = round(fval);
   if (ival <= 0 || ival >= (360<<6)) {
      sprintf(gszMsgBox, "Please enter a value > 0 and < 360!");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   rotationIncrement = ival;
   FormatAngle(rotationIncrement, buf);
   sprintf(gszMsgBox, "Rotation increment set to %s.", buf);
   Msg(gszMsgBox);
}

/* --------------------- Rotate Any Angle --------------------- */

static
void RotateBBoxByAnAngle (bbox, d_angle, vs)
   struct BBRec	* bbox; /* the original bounding box */
   int		d_angle; /* d_angle is degree*64 */
   XPoint	* vs; /* array of 5 points */
{
   int	x, y;

   RotatedXY (bbox->ltx, bbox->lty, d_angle, &x, &y);
   vs[0].x = vs[4].x = x; vs[0].y = vs[4].y = y;
   RotatedXY (bbox->rbx, bbox->lty, d_angle, &x, &y);
   vs[1].x = x; vs[1].y = y;
   RotatedXY (bbox->rbx, bbox->rby, d_angle, &x, &y);
   vs[2].x = x; vs[2].y = y;
   RotatedXY (bbox->ltx, bbox->rby, d_angle, &x, &y);
   vs[3].x = x; vs[3].y = y;
}

static
void RotateVsByAnAngle (InVs, NumPts, d_angle, OutVs)
   XPoint	* InVs, * OutVs;
   int		NumPts, d_angle;
{
   register int	i;

   for (i=0; i < NumPts; i++)
   {
      int	x, y;

      RotatedXY (InVs[i].x, InVs[i].y, d_angle, &x, &y);
      OutVs[i].x = x;
      OutVs[i].y = y;
   }
}

static
void RotateAllSelObjects (Corner, AngleDelta)
   int	Corner, AngleDelta; /* AngleDelta is degree*64 */
{
   register struct SelRec	* sel_ptr;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next) {
      if (!sel_ptr->obj->locked) {
         RotateObj (sel_ptr->obj, Corner, AngleDelta, NULL, NULL);
      }
   }
   if (numObjLocked != 0) Msg ("Locked objects are not rotated.");
}

static
int ConstrainedRotateAllSel (Corner, AngleDelta, ltx, lty, rbx, rby)
   int	Corner, AngleDelta, * ltx, * lty, * rbx, * rby;
{
   register struct ObjRec	* obj_ptr;
   int				something_stretched=FALSE, num_pts;
   int				x_off, y_off, move_first, move_last, x, y;
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
            int	index=INVALID, seg_dx, seg_dy, dx, dy;
            int	cur_seg_dx, cur_seg_dy;

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
            if (move_first && move_last && num_pts==3)
            {
               RotatedAbsXY(v[0].x, v[0].y, AngleDelta, &x, &y);
               dx = x-v[0].x; dy = y-v[0].y;
               index = 1;
               cur_seg_dx = v[index-1].x - v[index].x;
               cur_seg_dy = v[index-1].y - v[index].y;
               seg_dx = v[index].x - v[index+1].x;
               seg_dy = v[index].y - v[index+1].y;

               if (cur_seg_dy==0 && seg_dx==0 &&
                     (seg_dy!=0 || (seg_dy==0 && dx==0)))
                  v[index].y += dy;
               else if (cur_seg_dx==0 && seg_dy==0 &&
                     (seg_dx!=0 || (seg_dx==0 && dy==0)))
                  v[index].x += dx;
            }
            else
            {
               if (move_first && num_pts>2)
               {
                  RotatedAbsXY(v[0].x, v[0].y, AngleDelta, &x, &y);
                  dx = x-v[0].x; dy = y-v[0].y;
                  index = 1;
                  cur_seg_dx = v[index-1].x - v[index].x;
                  cur_seg_dy = v[index-1].y - v[index].y;
                  seg_dx = v[index].x - v[index+1].x;
                  seg_dy = v[index].y - v[index+1].y;

                  if (cur_seg_dy==0 && cur_seg_dx!=0 &&
                        (seg_dy!=0 || (seg_dy==0 && dx==0)))
                     v[index].y += dy;
                  else if (cur_seg_dx==0 && cur_seg_dy!=0 &&
                        (seg_dx!=0 || (seg_dx==0 && dy==0)))
                     v[index].x += dx;
               }
               if (move_last && num_pts>2)
               {
                  RotatedAbsXY(v[num_pts-1].x, v[num_pts-1].y, AngleDelta,
                        &x, &y);
                  dx = x-v[num_pts-1].x; dy = y-v[num_pts-1].y;
                  index = num_pts-2;
                  cur_seg_dx = v[index+1].x - v[index].x;
                  cur_seg_dy = v[index+1].y - v[index].y;
                  seg_dx = v[index].x - v[index-1].x;
                  seg_dy = v[index].y - v[index-1].y;

                  if (cur_seg_dy==0 && cur_seg_dx!=0 &&
                        (seg_dy!=0 || (seg_dy==0 && dx==0)))
                     v[index].y += dy;
                  else if (cur_seg_dx==0 && cur_seg_dy!=0 &&
                        (seg_dx!=0 || (seg_dx==0 && dy==0)))
                     v[index].x += dx;
               }
            }
            if (move_first)
            {
               RotatedAbsXY(v[0].x, v[0].y, AngleDelta, &x, &y);
               v[0].x = x; v[0].y = y;
            }
            if (move_last)
            {
               RotatedAbsXY(v[num_pts-1].x, v[num_pts-1].y, AngleDelta,
                     &x, &y);
               v[num_pts-1].x = x; v[num_pts-1].y = y;
            }
            AdjObjSplineVs (obj_ptr);
            switch (obj_ptr->type)
            {
               case OBJ_POLY:
                  if (obj_ptr->detail.p->curved != LT_INTSPLINE)
                     UpdPolyBBox (obj_ptr, num_pts, v);
                  else
                     UpdPolyBBox (obj_ptr, obj_ptr->detail.p->intn,
                           obj_ptr->detail.p->intvlist);
                  break;
               case OBJ_POLYGON:
                  if (obj_ptr->detail.g->curved != LT_INTSPLINE)
                     UpdPolyBBox (obj_ptr, num_pts, v);
                  else
                     UpdPolyBBox (obj_ptr, obj_ptr->detail.g->intn,
                           obj_ptr->detail.g->intvlist);
                  break;
            }
            AdjObjBBox (obj_ptr);
            if (AutoCenterAttr (obj_ptr))
            {
               struct AttrRec	* attr_ptr=obj_ptr->fattr;
               int		modified=FALSE;

               for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
                  if (attr_ptr->shown)
                  {
                     struct BBRec	bbox;

                     CenterObjInOBBox (attr_ptr->obj, obj_ptr->obbox, &bbox);
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
   return (something_stretched);
}

static
void RotateAllSel (Corner, AngleDelta)
   int		Corner, AngleDelta; /* AngleDelta is degree*64 */
{
   int	ltx, lty, rbx, rby, saved_ltx, saved_lty, saved_rbx, saved_rby;
   int	poly_stretched;

   saved_ltx = selLtX; saved_lty = selLtY;
   saved_rbx = selRbX; saved_rby = selRbY;

   if (moveMode==CONST_MOVE)
   {
      MarkObjectsForStretch ();

      StartCompositeCmd ();
      PrepareToRecord (CMD_STRETCH, topSel, botSel, numObjSelected);
      RecordCmd (CMD_STRETCH, NULL, topSel, botSel, numObjSelected);

      poly_stretched = ConstrainedRotateAllSel (Corner, AngleDelta,
            &ltx, &lty, &rbx, &rby);
      RotateAllSelObjects (Corner, AngleDelta);
      UpdSelBBox ();
      if (poly_stretched)
      {
         ltx = min(ltx,min(selLtX,saved_ltx));
         lty = min(lty,min(selLtY,saved_lty));
         rbx = max(rbx,max(selRbX,saved_rbx));
         rby = max(rby,max(selRbY,saved_rby));
         RedrawAnArea (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
               rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1));
      }
      else
      {
         RedrawAreas (botObj, saved_ltx-GRID_ABS_SIZE(1),
               saved_lty-GRID_ABS_SIZE(1),
               saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
               selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
               selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      }
      EndCompositeCmd ();
   }
   else
   {
      PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);
      RotateAllSelObjects (Corner, AngleDelta);
      RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
      UpdSelBBox ();
      RedrawAreas (botObj, saved_ltx-GRID_ABS_SIZE(1),
            saved_lty-GRID_ABS_SIZE(1),
            saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
            selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   }
}

static
void FormatAngleForRotate(buf, angle)
   char *buf;
   int angle;
{
   float fval=(float)(((float)angle)/((float)64.0));

   sprintf(buf, "degree=%.2f", fval);
}

static
void RotateSel (XGridOff, YGridOff, ObjPtr, Corner)
   int			XGridOff, YGridOff, Corner;
   struct ObjRec	* ObjPtr;
{
   register int	i;
   XEvent	ev;
   XPoint	all_bbox_vs[5], obj_obbox_vs[5], *vs=NULL, *orig_vs=NULL;
   int		grid_x=XGridOff, grid_y=YGridOff, dx, dy, d_angle=0;
   int		saved_x=XGridOff, saved_y=YGridOff;
   int		rotating=TRUE, deg360=(360<<6), n=0;
   char		buf[80], msg[MAXSTRING+1];
   struct BBRec	orig_all_bbox, orig_obj_obbox;

   if (numObjSelected == numObjLocked)
   {
      Msg ("Locked object(s) can not be rotated.");
      return;
   }
   XFlush (mainDisplay);
   XSync (mainDisplay, False);
   if (XCheckMaskEvent (mainDisplay, ExposureMask, &ev) ||
         XCheckMaskEvent (mainDisplay, VisibilityChangeMask, &ev))
      ExposeEventHandler (&ev, TRUE);

   if (ObjPtr->type==OBJ_POLY || ObjPtr->type==OBJ_POLYGON)
   {
      IntPoint	*pv;
      int	px, py;

      if (ObjPtr->ctm == NULL)
      {
         pv = (ObjPtr->type==OBJ_POLY ? &ObjPtr->detail.p->vlist[Corner] :
               &ObjPtr->detail.g->vlist[Corner]);
         px = pv->x;
         py = pv->y;
      }
      else
      {
         pv = (ObjPtr->type==OBJ_POLY ? &ObjPtr->detail.p->vlist[Corner] :
               &ObjPtr->detail.g->vlist[Corner]);
         TransformPointThroughCTM (pv->x-ObjPtr->x, pv->y-ObjPtr->y,
               ObjPtr->ctm, &px, &py);
         px += ObjPtr->x;
         py += ObjPtr->y;
      }
      absPivotX = (ObjPtr->obbox.ltx+ObjPtr->obbox.rbx)>>1;
      absPivotY = (ObjPtr->obbox.lty+ObjPtr->obbox.rby)>>1;
      moveX = OFFSET_X(px);
      moveY = OFFSET_Y(py);
      changeX = changeY = TRUE;
      multX = multY = 1.0;
      pivotX = OFFSET_X(absPivotX);
      pivotY = OFFSET_Y(absPivotY);
   }
   else
      SetPivot (Corner, ObjPtr->obbox);

   SetBBRec (&orig_all_bbox, OFFSET_X(selLtX)-2, OFFSET_Y(selLtY)-2,
         OFFSET_X(selRbX)+2, OFFSET_Y(selRbY)+2);
   SetRotateVs (all_bbox_vs, orig_all_bbox.ltx, orig_all_bbox.lty,
         orig_all_bbox.rbx, orig_all_bbox.rby);
   XDrawLines (mainDisplay, drawWindow, revDefaultGC, all_bbox_vs, 5,
         CoordModeOrigin);

   if (ObjPtr->type==OBJ_POLY || ObjPtr->type==OBJ_POLYGON) {
      if (ObjPtr->type == OBJ_POLY) {
         if (ObjPtr->ctm == NULL) {
            n = ObjPtr->detail.p->sn;
            orig_vs = (XPoint*)malloc(n*sizeof(XPoint));
            vs = (XPoint*)malloc(n*sizeof(XPoint));
            if (orig_vs == NULL || vs == NULL) FailAllocMessage();
            for (i=0; i < n; i++) {
               vs[i].x = orig_vs[i].x = ObjPtr->detail.p->svlist[i].x;
               vs[i].y = orig_vs[i].y = ObjPtr->detail.p->svlist[i].y;
            }
         } else {
            n = ObjPtr->detail.p->rotated_n;
            orig_vs = (XPoint*)malloc(n*sizeof(XPoint));
            vs = (XPoint*)malloc(n*sizeof(XPoint));
            if (orig_vs == NULL || vs == NULL) FailAllocMessage();
            for (i=0; i < n; i++) {
               vs[i].x = orig_vs[i].x = ObjPtr->detail.p->rotated_vlist[i].x;
               vs[i].y = orig_vs[i].y = ObjPtr->detail.p->rotated_vlist[i].y;
            }
         }
      } else {
         if (ObjPtr->ctm == NULL) {
            n = ObjPtr->detail.g->sn;
            orig_vs = (XPoint*)malloc(n*sizeof(XPoint));
            vs = (XPoint*)malloc(n*sizeof(XPoint));
            if (orig_vs == NULL || vs == NULL) FailAllocMessage();
            for (i=0; i < n; i++) {
               vs[i].x = orig_vs[i].x = ObjPtr->detail.g->svlist[i].x;
               vs[i].y = orig_vs[i].y = ObjPtr->detail.g->svlist[i].y;
            }
         } else {
            n = ObjPtr->detail.g->rotated_n;
            orig_vs = (XPoint*)malloc(n*sizeof(XPoint));
            vs = (XPoint*)malloc(n*sizeof(XPoint));
            if (orig_vs == NULL || vs == NULL) FailAllocMessage();
            for (i=0; i < n; i++) {
               vs[i].x = orig_vs[i].x = ObjPtr->detail.g->rotated_vlist[i].x;
               vs[i].y = orig_vs[i].y = ObjPtr->detail.g->rotated_vlist[i].y;
            }
         }
      }
      XDrawLines (mainDisplay, drawWindow, revDefaultGC, vs, n,
            CoordModeOrigin);
   } else {
      if (ObjPtr->ctm == NULL) {
         SetBBRec (&orig_obj_obbox, OFFSET_X(ObjPtr->obbox.ltx),
               OFFSET_Y(ObjPtr->obbox.lty), OFFSET_X(ObjPtr->obbox.rbx),
               OFFSET_Y(ObjPtr->obbox.rby));
         SetRotateVs (obj_obbox_vs, orig_obj_obbox.ltx, orig_obj_obbox.lty,
               orig_obj_obbox.rbx, orig_obj_obbox.rby);
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, obj_obbox_vs, 5,
               CoordModeOrigin);
      } else {
         memcpy (obj_obbox_vs, ObjPtr->rotated_obbox, 5*sizeof(XPoint));
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, obj_obbox_vs, 5,
               CoordModeOrigin);
      }
   }

   dx = OFFSET_X(ObjPtr->obbox.rbx) - OFFSET_X(ObjPtr->obbox.ltx);
   dy = OFFSET_Y(ObjPtr->obbox.rby) - OFFSET_Y(ObjPtr->obbox.lty);
   if (dx == 0 && dy == 0)
   {
      sprintf (msg, "%s.\n\n%s.",
            "This object is too small",
            "Please select another object to rotate");
      MsgBox (msg, TOOL_NAME, INFO_MB);
      return;
   }

   grid_x = moveX;
   grid_y = moveY;
   FormatAngleForRotate(buf, 0);
   StartShowMeasureCursor (grid_x, grid_y, buf, TRUE);
   XGrabPointer (mainDisplay, drawWindow, False,
         PointerMotionMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, rotatingCursor, CurrentTime);

   while (rotating)
   {
      XEvent	input;

      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == ButtonRelease)
      {
         XUngrabPointer (mainDisplay, CurrentTime);
         XSync (mainDisplay, False);
         rotating = FALSE;
         FormatAngleForRotate(buf, d_angle);
         EndShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         if (ObjPtr->type==OBJ_POLY || ObjPtr->type==OBJ_POLYGON)
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, vs, n,
                  CoordModeOrigin);
         else
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, obj_obbox_vs, 5,
                  CoordModeOrigin);
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, all_bbox_vs, 5,
               CoordModeOrigin);
      }
      else if (input.type == MotionNotify)
      {
         FormatAngleForRotate(buf, d_angle);
         ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         if (ObjPtr->type==OBJ_POLY || ObjPtr->type==OBJ_POLYGON)
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, vs, n,
                  CoordModeOrigin);
         else
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, obj_obbox_vs, 5,
                  CoordModeOrigin);
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, all_bbox_vs, 5,
               CoordModeOrigin);

         GridXY (input.xmotion.x, input.xmotion.y, &grid_x, &grid_y);
         dx = grid_x - saved_x;
         dy = grid_y - saved_y;
         grid_x = moveX + dx;
         grid_y = moveY + dy;
         MarkRulers (grid_x, grid_y);
         PointsToArc (pivotX, pivotY, moveX, moveY, grid_x, grid_y,
               ARC_CW, FALSE, NULL, NULL, NULL, NULL, NULL, &d_angle);
         if (d_angle == deg360) d_angle = 0;
         d_angle = (-d_angle);
         while (d_angle >= (deg360)) d_angle -= (deg360);

         RotateBBoxByAnAngle (&orig_all_bbox, d_angle, all_bbox_vs);
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, all_bbox_vs, 5,
               CoordModeOrigin);
         if (ObjPtr->type==OBJ_POLY || ObjPtr->type==OBJ_POLYGON)
         {
            RotateVsByAnAngle (orig_vs, n, d_angle, vs);
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, vs, n,
                  CoordModeOrigin);
         }
         else
         {
            if (ObjPtr->ctm == NULL)
               RotateBBoxByAnAngle (&orig_obj_obbox, d_angle, obj_obbox_vs);
            else
               RotateVsByAnAngle (ObjPtr->rotated_obbox, 5, d_angle,
                     obj_obbox_vs);
            XDrawLines (mainDisplay, drawWindow, revDefaultGC, obj_obbox_vs, 5,
                 CoordModeOrigin);
         }
         FormatAngleForRotate(buf, d_angle);
         ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   if (d_angle != 0)
   {
      HighLightReverse ();
      RotateAllSel (Corner, d_angle);
      HighLightForward ();
      SetFileModified (TRUE);
      justDupped = FALSE;
   }
   if (vs != NULL) free(vs);
   if (orig_vs != NULL) free(orig_vs);
}

/* --------------------- Shearing --------------------- */

static
void ShearBBox (Corner, bbox, x_shear, y_shear, x_scale, y_scale, vs)
   int		Corner, x_shear, y_shear, x_scale, y_scale;
   struct BBRec	* bbox; /* the original bounding box */
   XPoint	* vs; /* array of 5 points */
{
   int	x, y;

   switch (Corner)
   {
      case CORNER_TOP:
      case CORNER_BOTTOM:
         if (bbox->lty == pivotY)
         {
            vs[0].x = vs[4].x = bbox->ltx;
            vs[0].y = vs[4].y = bbox->lty;
            vs[1].x = bbox->rbx; vs[1].y = bbox->lty;
         }
         else
         {
            ShearedXY (Corner, bbox->ltx, bbox->lty, x_shear, y_shear,
                  x_scale, y_scale, &x, &y);
            vs[0].x = vs[4].x = x; vs[0].y = vs[4].y = y;
            ShearedXY (Corner, bbox->rbx, bbox->lty, x_shear, y_shear,
                  x_scale, y_scale, &x, &y);
            vs[1].x = x; vs[1].y = y;
         }
         if (bbox->rby == pivotY)
         {
            vs[2].x = bbox->rbx; vs[2].y = bbox->rby;
            vs[3].x = bbox->ltx; vs[3].y = bbox->rby;
         }
         else
         {
            ShearedXY (Corner, bbox->rbx, bbox->rby, x_shear, y_shear,
                  x_scale, y_scale, &x, &y);
            vs[2].x = x; vs[2].y = y;
            ShearedXY (Corner, bbox->ltx, bbox->rby, x_shear, y_shear,
                  x_scale, y_scale, &x, &y);
            vs[3].x = x; vs[3].y = y;
         }
         break;
      case CORNER_RIGHT:
      case CORNER_LEFT:
         if (bbox->ltx == pivotX)
         {
            vs[0].x = vs[4].x = bbox->ltx;
            vs[0].y = vs[4].y = bbox->lty;
            vs[3].x = bbox->ltx; vs[3].y = bbox->rby;
         }
         else
         {
            ShearedXY (Corner, bbox->ltx, bbox->lty, x_shear, y_shear,
                  x_scale, y_scale, &x, &y);
            vs[0].x = vs[4].x = x; vs[0].y = vs[4].y = y;
            ShearedXY (Corner, bbox->ltx, bbox->rby, x_shear, y_shear,
                  x_scale, y_scale, &x, &y);
            vs[3].x = x; vs[3].y = y;
         }
         if (bbox->rbx == pivotX)
         {
            vs[1].x = bbox->rbx; vs[1].y = bbox->lty;
            vs[2].x = bbox->rbx; vs[2].y = bbox->rby;
         }
         else
         {
            ShearedXY (Corner, bbox->rbx, bbox->lty, x_shear, y_shear,
                  x_scale, y_scale, &x, &y);
            vs[1].x = x; vs[1].y = y;
            ShearedXY (Corner, bbox->rbx, bbox->rby, x_shear, y_shear,
                  x_scale, y_scale, &x, &y);
            vs[2].x = x; vs[2].y = y;
         }
         break;
   }
}

static
void ShearVs (Corner, InVs, NumPts, x_shear, y_shear, x_scale, y_scale, OutVs)
   int		Corner, NumPts, x_shear, y_shear, x_scale, y_scale;
   XPoint	* InVs, * OutVs; /* array of 5 points */
{
   register int	i;
   int		x, y;

   switch (Corner)
   {
      case CORNER_TOP:
      case CORNER_BOTTOM:
         for (i=0; i < NumPts; i++)
         {
            if (InVs[i].y == pivotY)
            {
               OutVs[i].x = InVs[i].x;
               OutVs[i].y = InVs[i].y;
            }
            else
            {
               ShearedXY (Corner, InVs[i].x, InVs[i].y, x_shear, y_shear,
                     x_scale, y_scale, &x, &y);
               OutVs[i].x = x;
               OutVs[i].y = y;
            }
         }
         break;
      case CORNER_RIGHT:
      case CORNER_LEFT:
         for (i=0; i < NumPts; i++)
         {
            if (InVs[i].x == pivotX)
            {
               OutVs[i].x = InVs[i].x;
               OutVs[i].y = InVs[i].y;
            }
            else
            {
               ShearedXY (Corner, InVs[i].x, InVs[i].y, x_shear, y_shear,
                     x_scale, y_scale, &x, &y);
               OutVs[i].x = x;
               OutVs[i].y = y;
            }
         }
         break;
   }
}

static
void ShearAllSelObjects (Corner, XShear, YShear, XScale, YScale)
   int	Corner, XShear, YShear, XScale, YScale; /* everything scaled by 1000 */
{
   register struct SelRec	* sel_ptr;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next) {
      if (!sel_ptr->obj->locked) {
         ShearObj(sel_ptr->obj, Corner, XShear, YShear, XScale, YScale,
               NULL, NULL);
      }
   }
   if (numObjLocked != 0) Msg ("Locked objects are not rotated.");
}

static
int ConstrainedShearAllSel (Corner, XShear, YShear, XScale, YScale,
      ltx, lty, rbx, rby)
   int	Corner, XShear, YShear, XScale, YScale, * ltx, * lty, * rbx, * rby;
{
   register struct ObjRec	* obj_ptr;
   int				something_stretched=FALSE, num_pts;
   int				x_off, y_off, move_first, move_last, x, y;
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
            int	index=INVALID, seg_dx, seg_dy, dx, dy;
            int	cur_seg_dx, cur_seg_dy;

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
            if (move_first && move_last && num_pts==3)
            {
               ShearedAbsXY(Corner, v[0].x, v[0].y, XShear, YShear,
                     XScale, YScale, &x, &y);
               dx = x-v[0].x; dy = y-v[0].y;
               index = 1;
               cur_seg_dx = v[index-1].x - v[index].x;
               cur_seg_dy = v[index-1].y - v[index].y;
               seg_dx = v[index].x - v[index+1].x;
               seg_dy = v[index].y - v[index+1].y;

               if (cur_seg_dy==0 && seg_dx==0 &&
                     (seg_dy!=0 || (seg_dy==0 && dx==0)))
                  v[index].y += dy;
               else if (cur_seg_dx==0 && seg_dy==0 &&
                     (seg_dx!=0 || (seg_dx==0 && dy==0)))
                  v[index].x += dx;
            }
            else
            {
               if (move_first && num_pts>2)
               {
                  ShearedAbsXY(Corner, v[0].x, v[0].y, XShear, YShear,
                        XScale, YScale, &x, &y);
                  dx = x-v[0].x; dy = y-v[0].y;
                  index = 1;
                  cur_seg_dx = v[index-1].x - v[index].x;
                  cur_seg_dy = v[index-1].y - v[index].y;
                  seg_dx = v[index].x - v[index+1].x;
                  seg_dy = v[index].y - v[index+1].y;

                  if (cur_seg_dy==0 && cur_seg_dx!=0 &&
                        (seg_dy!=0 || (seg_dy==0 && dx==0)))
                     v[index].y += dy;
                  else if (cur_seg_dx==0 && cur_seg_dy!=0 &&
                        (seg_dx!=0 || (seg_dx==0 && dy==0)))
                     v[index].x += dx;
               }
               if (move_last && num_pts>2)
               {
                  ShearedAbsXY(Corner, v[num_pts-1].x, v[num_pts-1].y,
                        XShear, YShear, XScale, YScale, &x, &y);
                  dx = x-v[num_pts-1].x; dy = y-v[num_pts-1].y;
                  index = num_pts-2;
                  cur_seg_dx = v[index+1].x - v[index].x;
                  cur_seg_dy = v[index+1].y - v[index].y;
                  seg_dx = v[index].x - v[index-1].x;
                  seg_dy = v[index].y - v[index-1].y;

                  if (cur_seg_dy==0 && cur_seg_dx!=0 &&
                        (seg_dy!=0 || (seg_dy==0 && dx==0)))
                     v[index].y += dy;
                  else if (cur_seg_dx==0 && cur_seg_dy!=0 &&
                        (seg_dx!=0 || (seg_dx==0 && dy==0)))
                     v[index].x += dx;
               }
            }
            if (move_first)
            {
               ShearedAbsXY(Corner, v[0].x, v[0].y, XShear, YShear,
                     XScale, YScale, &x, &y);
               v[0].x = x; v[0].y = y;
            }
            if (move_last)
            {
               ShearedAbsXY(Corner, v[num_pts-1].x, v[num_pts-1].y,
                     XShear, YShear, XScale, YScale, &x, &y);
               v[num_pts-1].x = x; v[num_pts-1].y = y;
            }
            AdjObjSplineVs (obj_ptr);
            switch (obj_ptr->type)
            {
               case OBJ_POLY:
                  if (obj_ptr->detail.p->curved != LT_INTSPLINE)
                     UpdPolyBBox (obj_ptr, num_pts, v);
                  else
                     UpdPolyBBox (obj_ptr, obj_ptr->detail.p->intn,
                           obj_ptr->detail.p->intvlist);
                  break;
               case OBJ_POLYGON:
                  if (obj_ptr->detail.g->curved != LT_INTSPLINE)
                     UpdPolyBBox (obj_ptr, num_pts, v);
                  else
                     UpdPolyBBox (obj_ptr, obj_ptr->detail.g->intn,
                           obj_ptr->detail.g->intvlist);
                  break;
            }
            AdjObjBBox (obj_ptr);
            if (AutoCenterAttr (obj_ptr))
            {
               struct AttrRec	* attr_ptr=obj_ptr->fattr;
               int		modified=FALSE;

               for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
                  if (attr_ptr->shown)
                  {
                     struct BBRec	bbox;

                     CenterObjInOBBox (attr_ptr->obj, obj_ptr->obbox, &bbox);
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
   return (something_stretched);
}

static
void ShearAllSel (Corner, XShear, YShear, XScale, YScale)
   int	Corner, XShear, YShear, XScale, YScale; /* everything scaled by 1000 */
{
   int	ltx, lty, rbx, rby, saved_ltx, saved_lty, saved_rbx, saved_rby;
   int	poly_stretched;

   saved_ltx = selLtX; saved_lty = selLtY;
   saved_rbx = selRbX; saved_rby = selRbY;

   if (moveMode==CONST_MOVE)
   {
      MarkObjectsForStretch ();

      StartCompositeCmd ();
      PrepareToRecord (CMD_STRETCH, topSel, botSel, numObjSelected);
      RecordCmd (CMD_STRETCH, NULL, topSel, botSel, numObjSelected);

      poly_stretched = ConstrainedShearAllSel (Corner, XShear, YShear, XScale,
            YScale, &ltx, &lty, &rbx, &rby);
      ShearAllSelObjects (Corner, XShear, YShear, XScale, YScale);
      UpdSelBBox ();
      if (poly_stretched)
      {
         ltx = min(ltx,min(selLtX,saved_ltx));
         lty = min(lty,min(selLtY,saved_lty));
         rbx = max(rbx,max(selRbX,saved_rbx));
         rby = max(rby,max(selRbY,saved_rby));
         RedrawAnArea (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
               rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1));
      }
      else
      {
         RedrawAreas (botObj, saved_ltx-GRID_ABS_SIZE(1),
               saved_lty-GRID_ABS_SIZE(1),
               saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
               selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
               selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      }
      EndCompositeCmd ();
   }
   else
   {
      PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);
      ShearAllSelObjects (Corner, XShear, YShear, XScale, YScale);
      RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
      UpdSelBBox ();
      RedrawAreas (botObj, saved_ltx-GRID_ABS_SIZE(1),
            saved_lty-GRID_ABS_SIZE(1),
            saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1),
            selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   }
}

static
void ShearSel (XGridOff, YGridOff, ObjPtr, Corner)
   int			XGridOff, YGridOff, Corner;
   struct ObjRec	* ObjPtr;
{
   XEvent	ev;
   XPoint	all_bbox_vs[5], obj_obbox_vs[5];
   int		grid_x=XGridOff, grid_y=YGridOff, dx, dy;
   int		saved_x=XGridOff, saved_y=YGridOff;
   int		shearing=TRUE, shear_hori=FALSE;
   int		x_scale=1000, y_scale=1000, x_shear=0, y_shear=0;
   char		buf[80], msg[MAXSTRING+1];
   struct BBRec	orig_all_bbox, orig_obj_obbox;

   if (numObjSelected == numObjLocked)
   {
      Msg ("Locked object(s) can not be rotated.");
      return;
   }
   XFlush (mainDisplay);
   XSync (mainDisplay, False);
   if (XCheckMaskEvent (mainDisplay, ExposureMask, &ev) ||
         XCheckMaskEvent (mainDisplay, VisibilityChangeMask, &ev))
      ExposeEventHandler (&ev, TRUE);

   SetPivot (Corner, ObjPtr->obbox);

   SetBBRec (&orig_all_bbox, OFFSET_X(selLtX)-2, OFFSET_Y(selLtY)-2,
         OFFSET_X(selRbX)+2, OFFSET_Y(selRbY)+2);
   SetRotateVs (all_bbox_vs, orig_all_bbox.ltx, orig_all_bbox.lty,
         orig_all_bbox.rbx, orig_all_bbox.rby);
   XDrawLines (mainDisplay, drawWindow, revDefaultGC, all_bbox_vs, 5,
         CoordModeOrigin);

   if (ObjPtr->ctm == NULL)
   {
      SetBBRec (&orig_obj_obbox, OFFSET_X(ObjPtr->obbox.ltx),
            OFFSET_Y(ObjPtr->obbox.lty), OFFSET_X(ObjPtr->obbox.rbx),
            OFFSET_Y(ObjPtr->obbox.rby));
      SetRotateVs (obj_obbox_vs, orig_obj_obbox.ltx, orig_obj_obbox.lty,
            orig_obj_obbox.rbx, orig_obj_obbox.rby);
      XDrawLines (mainDisplay, drawWindow, revDefaultGC, obj_obbox_vs, 5,
            CoordModeOrigin);
   }
   else
   {
      memcpy (obj_obbox_vs, ObjPtr->rotated_obbox, 5*sizeof(XPoint));
      XDrawLines (mainDisplay, drawWindow, revDefaultGC, obj_obbox_vs, 5,
            CoordModeOrigin);
   }

   dx = OFFSET_X(ObjPtr->obbox.rbx) - OFFSET_X(ObjPtr->obbox.ltx);
   dy = OFFSET_Y(ObjPtr->obbox.rby) - OFFSET_Y(ObjPtr->obbox.lty);
   if (dx == 0 || dy == 0)
   {
      sprintf (msg, "%s.\n\n%s.",
            "This object is too small",
            "Please select another object to shear");
      MsgBox (msg, TOOL_NAME, INFO_MB);
      return;
   }
   if (Corner == CORNER_TOP || Corner == CORNER_BOTTOM)
   {
      shear_hori = TRUE;
      multX = 0.0;
      multY = (Corner == CORNER_BOTTOM ? 1.0 : (-1.0));
   }
   else
   {
      shear_hori = FALSE;
      multX = (Corner == CORNER_RIGHT ? 1.0 : (-1.0));
      multY = 0.0;
   }
   dx = dy = 0;
   grid_x = moveX;
   grid_y = moveY;
   sprintf (buf, "dx=0,dy=0");
   StartShowMeasureCursor (grid_x, grid_y, buf, TRUE);
   XGrabPointer (mainDisplay, drawWindow, False,
         PointerMotionMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None,
         ((Corner==CORNER_TOP || Corner==CORNER_BOTTOM) ? horiShearCursor :
         vertShearCursor), CurrentTime);

   while (shearing)
   {
      XEvent	input;

      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == ButtonRelease)
      {
         XUngrabPointer (mainDisplay, CurrentTime);
         XSync (mainDisplay, False);
         shearing = FALSE;
         sprintf (buf, "dx=%1d,dy=%1d", dx, dy);
         EndShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, obj_obbox_vs, 5,
               CoordModeOrigin);
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, all_bbox_vs, 5,
               CoordModeOrigin);
      }
      else if (input.type == MotionNotify)
      {
         sprintf (buf, "dx=%1d,dy=%1d", dx, dy);
         ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, obj_obbox_vs, 5,
               CoordModeOrigin);
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, all_bbox_vs, 5,
               CoordModeOrigin);

         GridXY (input.xmotion.x, input.xmotion.y, &grid_x, &grid_y);
         dx = grid_x - saved_x;
         dy = grid_y - saved_y;
         grid_x = moveX + dx;
         grid_y = moveY + dy;
         MarkRulers (grid_x, grid_y);
         PointsToShearScale (Corner, pivotX, pivotY, moveX, moveY,
               grid_x, grid_y, &x_shear, &y_shear, &x_scale, &y_scale);

         ShearBBox (Corner, &orig_all_bbox, x_shear, y_shear, x_scale, y_scale,
               all_bbox_vs);
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, all_bbox_vs, 5,
               CoordModeOrigin);
         if (ObjPtr->ctm == NULL)
            ShearBBox (Corner, &orig_obj_obbox, x_shear, y_shear, x_scale,
                  y_scale, obj_obbox_vs);
         else
            ShearVs (Corner, ObjPtr->rotated_obbox, 5, x_shear, y_shear,
                  x_scale, y_scale, obj_obbox_vs);
         XDrawLines (mainDisplay, drawWindow, revDefaultGC, obj_obbox_vs, 5,
               CoordModeOrigin);
         sprintf (buf, "dx=%1d,dy=%1d", dx, dy);
         ShowMeasureCursor (grid_x, grid_y, buf, TRUE);
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   ShowCursor ();
   if (dx != 0 || dy != 0)
   {
      PointsToShearScale (Corner, pivotX, pivotY, moveX, moveY,
            moveX+dx, moveY+dy, &x_shear, &y_shear, &x_scale, &y_scale);
      HighLightReverse ();
      ShearAllSel (Corner, x_shear, y_shear, x_scale, y_scale);
      HighLightForward ();
      SetFileModified (TRUE);
      justDupped = FALSE;
   }
}

void RotateShearSel (XGridOff, YGridOff, ObjPtr, Corner)
   int			XGridOff, YGridOff, Corner;
   struct ObjRec	* ObjPtr;
   /* 1 2 3 */
   /* 8   4 */
   /* 7 6 5 */
{
   if (ObjPtr->type==OBJ_POLY || ObjPtr->type==OBJ_POLYGON || (Corner & 0x1))
      RotateSel (XGridOff, YGridOff, ObjPtr, Corner);
   else
      ShearSel (XGridOff, YGridOff, ObjPtr, Corner);
}

