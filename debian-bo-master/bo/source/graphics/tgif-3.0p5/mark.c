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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/mark.c,v 3.0 1996/05/06 16:05:57 william Exp $";
#endif

#include <stdio.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "choice.e"
#include "exec.e"
#ifndef _NO_EXTERN
#include "mark.e"
#endif
#include "obj.e"
#include "raster.e"
#include "rect.e"
#include "setup.e"
#include "select.e"

#define FORWARD 0
#define REVERSE 1

int	somethingHighLighted=FALSE;

static
void MarkPoly(ObjPtr, NumPts, V, Smooth, Curved, Locked)
   struct ObjRec *ObjPtr;
   int NumPts, Curved, Locked;
   char *Smooth;
   IntPoint *V;
{
   register int i;

   if (ObjPtr->ctm == NULL) {
      if (Curved != LT_INTSPLINE && Smooth != NULL) {
         for (i = 0; i < NumPts; i++) {
            if (Smooth[i]) {
               MARKO(drawWindow, Locked ? revGrayGC : revDefaultGC,
                     OFFSET_X(V[i].x), OFFSET_Y(V[i].y));
            } else {
               MARK(drawWindow, Locked ? revGrayGC : revDefaultGC,
                     OFFSET_X(V[i].x), OFFSET_Y(V[i].y));
            }
         }
      } else {
         for (i = 0; i < NumPts; i++) {
            MARK(drawWindow, Locked ? revGrayGC : revDefaultGC,
                  OFFSET_X(V[i].x), OFFSET_Y(V[i].y));
         }
      }
   } else {
      for (i=0; i < NumPts; i++) {
         int x, y;

         TransformPointThroughCTM(V[i].x-ObjPtr->x, V[i].y-ObjPtr->y,
               ObjPtr->ctm, &x, &y);
         if (Curved != LT_INTSPLINE && Smooth != NULL && Smooth[i]) {
            MARKO(drawWindow, Locked ? revGrayGC : revDefaultGC,
                  OFFSET_X(x+ObjPtr->x), OFFSET_Y(y+ObjPtr->y));
         } else {
            MARK(drawWindow, Locked ? revGrayGC : revDefaultGC,
                  OFFSET_X(x+ObjPtr->x), OFFSET_Y(y+ObjPtr->y));
         }
      }
   }
}

static
void Mark4Corners(BBox, Locked)
   struct BBRec BBox;
   int Locked;
{
   if (Locked) {
      MARK(drawWindow, revGrayGC, OFFSET_X(BBox.ltx), OFFSET_Y(BBox.lty));
      MARK(drawWindow, revGrayGC, OFFSET_X(BBox.ltx), OFFSET_Y(BBox.rby));
      MARK(drawWindow, revGrayGC, OFFSET_X(BBox.rbx), OFFSET_Y(BBox.lty));
      MARK(drawWindow, revGrayGC, OFFSET_X(BBox.rbx), OFFSET_Y(BBox.rby));
   } else {
      MARK(drawWindow, revDefaultGC, OFFSET_X(BBox.ltx), OFFSET_Y(BBox.lty));
      MARK(drawWindow, revDefaultGC, OFFSET_X(BBox.ltx), OFFSET_Y(BBox.rby));
      MARK(drawWindow, revDefaultGC, OFFSET_X(BBox.rbx), OFFSET_Y(BBox.lty));
      MARK(drawWindow, revDefaultGC, OFFSET_X(BBox.rbx), OFFSET_Y(BBox.rby));
   }
}

static
void Mark8Places(BBox, Locked)
   struct BBRec BBox;
   int Locked;
{
   register int xmid, ymid;

   if (ZOOMED_SIZE(BBox.rbx - BBox.ltx) >= 10) {
      xmid = (BBox.ltx+BBox.rbx) / 2;
      if (Locked) {
         MARK(drawWindow, revGrayGC, OFFSET_X(xmid), OFFSET_Y(BBox.lty));
         MARK(drawWindow, revGrayGC, OFFSET_X(xmid), OFFSET_Y(BBox.rby));
      } else {
         MARK(drawWindow, revDefaultGC, OFFSET_X(xmid), OFFSET_Y(BBox.lty));
         MARK(drawWindow, revDefaultGC, OFFSET_X(xmid), OFFSET_Y(BBox.rby));
      }
   }
   if (ZOOMED_SIZE(BBox.rby - BBox.lty) >= 10) {
      ymid = ((BBox.lty+BBox.rby) >> 1);
      if (Locked) {
         MARK(drawWindow, revGrayGC, OFFSET_X(BBox.ltx), OFFSET_Y(ymid));
         MARK(drawWindow, revGrayGC, OFFSET_X(BBox.rbx), OFFSET_Y(ymid));
      } else {
         MARK(drawWindow, revDefaultGC, OFFSET_X(BBox.ltx), OFFSET_Y(ymid));
         MARK(drawWindow, revDefaultGC, OFFSET_X(BBox.rbx), OFFSET_Y(ymid));
      }
   }
   Mark4Corners(BBox, Locked);
}

void HighLightAnObj(ObjPtr)
   register struct ObjRec *ObjPtr;
{
   if (execCurDepth > 0) return;
   switch (ObjPtr->type) {
   case OBJ_POLY:
      MarkPoly(ObjPtr, ObjPtr->detail.p->n, ObjPtr->detail.p->vlist,
            ObjPtr->detail.p->smooth, ObjPtr->detail.p->curved,
            ObjPtr->locked);
      break;
   case OBJ_POLYGON:
      MarkPoly(ObjPtr, ObjPtr->detail.g->n-1, ObjPtr->detail.g->vlist,
            ObjPtr->detail.g->smooth, ObjPtr->detail.g->curved,
            ObjPtr->locked);
      break;

   case OBJ_BOX:
   case OBJ_OVAL:
   case OBJ_TEXT:
   case OBJ_ARC:
   case OBJ_RCBOX:
   case OBJ_GROUP:
   case OBJ_XBM:
   case OBJ_XPM:
   case OBJ_SYM:
   case OBJ_ICON:
      Mark8Places(ObjPtr->obbox, ObjPtr->locked);
      break;
   }
}

static
void HighLightVertices(Dir)
   int Dir;
{
   register struct VSelRec *vsel_ptr=NULL;
   register struct ObjRec *obj_ptr;
   register int i;
   struct SelRec *sel_ptr=NULL;

   if (execCurDepth > 0) return;
   switch (Dir) {
   case FORWARD: vsel_ptr=botVSel; sel_ptr=botSel; break;
   case REVERSE: vsel_ptr=topVSel; sel_ptr=topSel; break;
   }
   while (sel_ptr != NULL) {
      obj_ptr = sel_ptr->obj;

      switch (obj_ptr->type) {
      case OBJ_POLY:
         MarkPoly(obj_ptr, obj_ptr->detail.p->n, obj_ptr->detail.p->vlist,
               obj_ptr->detail.p->smooth, obj_ptr->detail.p->curved,
               obj_ptr->locked);
         break;
      case OBJ_POLYGON:
         MarkPoly(obj_ptr, obj_ptr->detail.g->n-1, obj_ptr->detail.g->vlist,
               obj_ptr->detail.g->smooth, obj_ptr->detail.g->curved,
               obj_ptr->locked);
         break;
      }
      switch (Dir) {
      case FORWARD: sel_ptr = sel_ptr->prev; break;
      case REVERSE: sel_ptr = sel_ptr->next; break;
      }
   }
   while (vsel_ptr != NULL) {
      char *smooth=NULL;
      int curved=(-1);

      switch (vsel_ptr->obj->type) {
      case OBJ_POLY:
         smooth = vsel_ptr->obj->detail.p->smooth;
         curved = vsel_ptr->obj->detail.p->curved;
         break;
      case OBJ_POLYGON:
         smooth = vsel_ptr->obj->detail.g->smooth;
         curved = vsel_ptr->obj->detail.g->curved;
         break;
      }
      for (i = 0; i < vsel_ptr->n; i++) {
         if (!(vsel_ptr->obj->type==OBJ_POLYGON &&
               vsel_ptr->obj->detail.g->n-1==vsel_ptr->v_index[i])) {
            if (curved != LT_INTSPLINE && curved != (-1) && smooth != NULL) {
               if (smooth[vsel_ptr->v_index[i]]) {
                  MARKO(drawWindow, revDefaultGC, OFFSET_X(vsel_ptr->x[i]),
                        OFFSET_Y(vsel_ptr->y[i]));
               } else {
                  MARK(drawWindow, revDefaultGC, OFFSET_X(vsel_ptr->x[i]),
                        OFFSET_Y(vsel_ptr->y[i]));
               }
            } else {
               MARK(drawWindow, revDefaultGC, OFFSET_X(vsel_ptr->x[i]),
                     OFFSET_Y(vsel_ptr->y[i]));
            }
            MARKV(drawWindow, revDefaultGC, OFFSET_X(vsel_ptr->x[i]),
                  OFFSET_Y(vsel_ptr->y[i]));
         }
      }
      switch (Dir)
      {
         case FORWARD: vsel_ptr = vsel_ptr->prev; break;
         case REVERSE: vsel_ptr = vsel_ptr->next; break;
      }
   }
}

static
void HighLight(Dir)
   int Dir;
{
   register struct SelRec *sel_ptr=NULL;
   register struct ObjRec *obj_ptr;

   if (execCurDepth > 0) return;
   switch (Dir) {
   case FORWARD: sel_ptr = botSel; break;
   case REVERSE: sel_ptr = topSel; break;
   }
   while (sel_ptr != NULL) {
      obj_ptr = sel_ptr->obj;
      switch (obj_ptr->type) {
      case OBJ_POLY:
         MarkPoly(obj_ptr, obj_ptr->detail.p->n, obj_ptr->detail.p->vlist,
               obj_ptr->detail.p->smooth, obj_ptr->detail.p->curved,
               obj_ptr->locked);
         break;
      case OBJ_POLYGON:
         MarkPoly(obj_ptr, obj_ptr->detail.g->n-1, obj_ptr->detail.g->vlist,
               obj_ptr->detail.g->smooth, obj_ptr->detail.g->curved,
               obj_ptr->locked);
         break;

      case OBJ_BOX:
      case OBJ_OVAL:
      case OBJ_TEXT:
      case OBJ_GROUP:
      case OBJ_ARC:
      case OBJ_RCBOX:
      case OBJ_XBM:
      case OBJ_XPM:
      case OBJ_SYM:
      case OBJ_ICON:
         Mark8Places(obj_ptr->obbox, obj_ptr->locked);
         break;
      }
      switch (Dir) {
      case FORWARD: sel_ptr = sel_ptr->prev; break;
      case REVERSE: sel_ptr = sel_ptr->next; break;
      }
   }
}

void HighLightForward()
{
   if (curChoice == VERTEXMODE) {
      HighLightVertices(FORWARD);
   } else {
      HighLight(FORWARD);
   }
   somethingHighLighted = TRUE;
}
 
void HighLightReverse()
{
   if (curChoice == VERTEXMODE) {
      HighLightVertices(REVERSE);
   } else {
      HighLight(REVERSE);
   }
   somethingHighLighted = FALSE;
}
