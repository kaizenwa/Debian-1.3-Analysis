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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/align.c,v 3.1 1996/05/12 05:17:25 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#ifndef _NO_EXTERN
#include "align.e"
#endif
#include "auxtext.e"
#include "button.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "grid.e"
#include "mark.e"
#include "mainmenu.e"
#include "menu.e"
#include "move.e"
#include "msg.e"
#include "obj.e"
#include "poly.e"
#include "raster.e"
#include "select.e"
#include "setup.e"

typedef struct DistrVRec {
   struct ObjRec *obj;
   struct VSelRec *vsel;
   int index, vindex, x, y;
   struct DistrVRec *next, *prev;
} * DistrVRecPtr;

int	horiAlign=ALIGN_L;
int	vertAlign=ALIGN_T;

void DistrSelObjs()
{
   register int i;
   struct SelRec *sel_ptr, *next_sel;
   struct ObjRec *obj_ptr;
   struct VSelRec *vsel_ptr;
   int dx=0, dy=0, ltx, lty, rbx, rby, count, w, h;
   double x=0.0, y=0.0, h_dist=0.0, v_dist=0.0;

   if ((topSel==NULL && topVSel==NULL) ||
         (horiAlign==ALIGN_N && vertAlign==ALIGN_N)) {
      return;
   }

   if (numObjLocked != 0) {
      MsgBox("Can not distribute with locked objects.", TOOL_NAME, INFO_MB);
      return;
   }

   if (curChoice == VERTEXMODE) {
      int start, num_pts;
      struct DistrVRec *dv_ptr, *left_dv, *right_dv, *top_dv, *bottom_dv;
      struct DistrVRec *ptr, *hori_dv, *vert_dv;
      struct SelRec *tmp_top_sel, *tmp_bot_sel, *tmp_sel_ptr;
      IntPoint *v=NULL;

      if ((num_pts=CountSelectedVertices()) <= 2) return;

      HighLightReverse();

      dv_ptr = (struct DistrVRec *)malloc(2*num_pts*sizeof(struct DistrVRec));
      if (dv_ptr == NULL) FailAllocMessage();

      start = 1;

      ptr = dv_ptr;
      ptr->obj = topVSel->obj;
      ptr->vsel = topVSel;
      ptr->index = topVSel->v_index[0];
      ptr->vindex = 0;
      ptr->x = topVSel->x[0];
      ptr->y = topVSel->y[0];
      ptr->next = ptr->prev = NULL;
      left_dv = right_dv = ptr;

      ptr++;
      ptr->obj = topVSel->obj;
      ptr->vsel = topVSel;
      ptr->index = topVSel->v_index[0];
      ptr->vindex = 0;
      ptr->x = topVSel->x[0];
      ptr->y = topVSel->y[0];
      ptr->next = ptr->prev = NULL;
      top_dv = bottom_dv = ptr;

      ptr++;
      for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next) {
         sel_ptr->obj->marked = FALSE;
      }
      for (vsel_ptr = topVSel; vsel_ptr != NULL; vsel_ptr = vsel_ptr->next) {
         int obj_type = vsel_ptr->obj->type, last_index=0, n;

         if (obj_type == OBJ_POLYGON) last_index = vsel_ptr->obj->detail.g->n-1;
         obj_ptr = vsel_ptr->obj;
         n = vsel_ptr->n;
         for (i = start; i < n; i++) {
            if (!(obj_type==OBJ_POLYGON && vsel_ptr->v_index[i]==last_index)) {
               for (hori_dv=left_dv; hori_dv!=NULL; hori_dv=hori_dv->next) {
                  if (hori_dv->x > vsel_ptr->x[i] ||
                        (hori_dv->x == vsel_ptr->x[i] &&
                        hori_dv->y > vsel_ptr->y[i])) {
                     break;
                  }
               }
               ptr->obj = obj_ptr;
               ptr->vsel = vsel_ptr;
               ptr->index = vsel_ptr->v_index[i];
               ptr->vindex = i;
               ptr->x = vsel_ptr->x[i];
               ptr->y = vsel_ptr->y[i];

               ptr->next = hori_dv;
               if (hori_dv == NULL) {
                  ptr->prev = right_dv;
                  right_dv->next = ptr;
                  right_dv = ptr;
               } else {
                  ptr->prev = hori_dv->prev;
                  if (hori_dv->prev == NULL) {
                     left_dv = ptr;
                  } else {
                     hori_dv->prev->next = ptr;
                  }
                  hori_dv->prev = ptr;
               }
               ptr++;

               for (vert_dv=top_dv; vert_dv!=NULL; vert_dv=vert_dv->next) {
                  if (vert_dv->y > vsel_ptr->y[i] ||
                        (vert_dv->y == vsel_ptr->y[i] &&
                        vert_dv->x > vsel_ptr->x[i])) {
                     break;
                  }
               }
               ptr->obj = obj_ptr;
               ptr->vsel = vsel_ptr;
               ptr->index = vsel_ptr->v_index[i];
               ptr->vindex = i;
               ptr->x = vsel_ptr->x[i];
               ptr->y = vsel_ptr->y[i];

               ptr->next = vert_dv;
               if (vert_dv == NULL) {
                  ptr->prev = bottom_dv;
                  bottom_dv->next = ptr;
                  bottom_dv = ptr;
               } else {
                  ptr->prev = vert_dv->prev;
                  if (vert_dv->prev == NULL) {
                     top_dv = ptr;
                  } else {
                     vert_dv->prev->next = ptr;
                  }
                  vert_dv->prev = ptr;
               }
               ptr++;
            }
         }
         start = 0;
      }

      if (horiAlign != ALIGN_N) {
         for (ptr=left_dv; ptr->next!=right_dv; ptr=ptr->next) {
            obj_ptr = ptr->next->obj;
            if (!(obj_ptr->type==OBJ_POLYGON &&
                  ptr->next->index==obj_ptr->detail.g->n-1)) {
               obj_ptr->marked = TRUE;
            }
         }
      }
      if (vertAlign != ALIGN_N) {
         for (ptr=top_dv; ptr->next!=bottom_dv; ptr=ptr->next) {
            obj_ptr = ptr->next->obj;
            if (!(obj_ptr->type==OBJ_POLYGON &&
                  ptr->next->index==obj_ptr->detail.g->n-1)) {
               obj_ptr->marked = TRUE;
            }
         }
      }

      tmp_top_sel = tmp_bot_sel = NULL;
      count = 0;
      for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next) {
         obj_ptr = sel_ptr->obj;
         if (obj_ptr->marked) {
            count++;
            tmp_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
            if (tmp_sel_ptr == NULL) FailAllocMessage();
            tmp_sel_ptr->obj = obj_ptr;
            tmp_sel_ptr->prev = tmp_bot_sel;
            tmp_sel_ptr->next = NULL;
            if (tmp_bot_sel == NULL) {
               tmp_top_sel = tmp_sel_ptr;
            } else {
               tmp_bot_sel->next = tmp_sel_ptr;
            }
            tmp_bot_sel = tmp_sel_ptr;
         }
      }
      if (count != 0) {
         PrepareToRecord(CMD_REPLACE, tmp_top_sel, tmp_bot_sel, count);
      }
      if (horiAlign != ALIGN_N) {
         x = (double)(left_dv->x);
         h_dist = (right_dv->x-x)/(num_pts-1);
         for (ptr=left_dv; ptr->next!=right_dv; ptr=ptr->next) {
            obj_ptr = ptr->next->obj;
            vsel_ptr = ptr->next->vsel;

            if (obj_ptr->type==OBJ_POLYGON &&
                  ptr->next->index==obj_ptr->detail.g->n-1) {
               continue;
            }
            switch (obj_ptr->type) {
            case OBJ_POLY: v = obj_ptr->detail.p->vlist; break;
            case OBJ_POLYGON: v = obj_ptr->detail.g->vlist; break;
            }
            obj_ptr->marked = TRUE;
            v[ptr->next->index].x = vsel_ptr->x[ptr->next->vindex] =
                  round(x+h_dist);

            x += h_dist;
         }
      }
      if (vertAlign != ALIGN_N) {
         y = (double)(top_dv->y);
         v_dist = (bottom_dv->y-y)/(num_pts-1);
         for (ptr=top_dv; ptr->next!=bottom_dv; ptr=ptr->next) {
            obj_ptr = ptr->next->obj;
            vsel_ptr = ptr->next->vsel;

            if (obj_ptr->type==OBJ_POLYGON &&
                  ptr->next->index==obj_ptr->detail.g->n-1) {
               continue;
            }
            switch (obj_ptr->type) {
            case OBJ_POLY: v = obj_ptr->detail.p->vlist; break;
            case OBJ_POLYGON: v = obj_ptr->detail.g->vlist; break;
            }
            obj_ptr->marked = TRUE;
            v[ptr->next->index].y = vsel_ptr->y[ptr->next->vindex] =
                  round(y+v_dist);

            y += v_dist;
         }
      }
      for (vsel_ptr = topVSel; vsel_ptr != NULL; vsel_ptr = vsel_ptr->next) {
         obj_ptr = vsel_ptr->obj;
         if (obj_ptr->marked) {
            if (obj_ptr->type==OBJ_POLYGON) {
               struct PolygonRec *polygon_ptr=obj_ptr->detail.g;

               polygon_ptr->vlist[polygon_ptr->n-1].x = polygon_ptr->vlist[0].x;
               polygon_ptr->vlist[polygon_ptr->n-1].y = polygon_ptr->vlist[0].y;
            }
            AdjObjSplineVs(obj_ptr);
            switch (obj_ptr->type) {
            case OBJ_POLY:
               if (obj_ptr->detail.p->curved != LT_INTSPLINE) {
                  UpdPolyBBox(obj_ptr, obj_ptr->detail.p->n,
                        obj_ptr->detail.p->vlist);
               } else {
                  UpdPolyBBox(obj_ptr, obj_ptr->detail.p->intn,
                        obj_ptr->detail.p->intvlist);
               }
               break;
            case OBJ_POLYGON:
               if (obj_ptr->detail.g->curved != LT_INTSPLINE) {
                  UpdPolyBBox(obj_ptr, obj_ptr->detail.g->n,
                        obj_ptr->detail.g->vlist);
               } else {
                  UpdPolyBBox(obj_ptr, obj_ptr->detail.g->intn,
                        obj_ptr->detail.g->intvlist);
               }
               break;
            }
         }
      }
      if (count != 0) {
         RecordCmd(CMD_REPLACE, NULL, tmp_top_sel, tmp_bot_sel, count);
      }
      for (tmp_sel_ptr=tmp_top_sel; tmp_sel_ptr!=NULL; tmp_sel_ptr=next_sel) {
         next_sel = tmp_sel_ptr->next;
         free(tmp_sel_ptr);
      }
      free(dv_ptr);

      if (horiAlign != ALIGN_N) {
         sprintf(gszMsgBox, "Vertices are %1d pixels apart horizontally.",
               round(h_dist));
         Msg(gszMsgBox);
      }
      if (vertAlign != ALIGN_N) {
         sprintf(gszMsgBox, "Vertices are %1d pixels apart vertically.",
               round(v_dist));
         Msg(gszMsgBox);
      }
   } else {
      struct SelRec *left_sel, *right_sel;
      struct SelRec *top_sel, *bottom_sel;
      struct SelRec *vert_sel=NULL, *hori_sel=NULL, *new_sel;
      struct MoveSubCmdRec *move_cmd;
      struct SubCmdRec *sub_cmd;
      struct SelRec *tmp_sel_ptr;

      if (topSel==botSel || topSel->next==botSel) return;

      tmp_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (tmp_sel_ptr == NULL) FailAllocMessage();
      tmp_sel_ptr->next = tmp_sel_ptr->prev = NULL;

      move_cmd = (struct MoveSubCmdRec *)malloc(sizeof(struct MoveSubCmdRec));
      if (move_cmd == NULL) FailAllocMessage();
      memset(move_cmd, 0, sizeof(struct MoveSubCmdRec));
      sub_cmd = (struct SubCmdRec *)malloc(sizeof(struct SubCmdRec));
      if (sub_cmd == NULL) FailAllocMessage();
      memset(sub_cmd, 0, sizeof(struct SubCmdRec));
      sub_cmd->detail.mv = move_cmd;

      StartCompositeCmd();
      HighLightReverse();

      left_sel = right_sel = (struct SelRec *)malloc(sizeof(struct SelRec));
      top_sel = bottom_sel = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (left_sel == NULL || right_sel == NULL ||
            top_sel == NULL || bottom_sel == NULL) {
         FailAllocMessage();
      }
      left_sel->obj = right_sel->obj = botSel->obj;
      top_sel->obj = bottom_sel->obj = botSel->obj;
      left_sel->prev = right_sel->next = NULL;
      top_sel->prev = bottom_sel->next = NULL;

      count = 1;
      w = left_sel->obj->obbox.rbx - left_sel->obj->obbox.ltx;
      h = left_sel->obj->obbox.rby - left_sel->obj->obbox.lty;
      for (sel_ptr = botSel->prev; sel_ptr != NULL; sel_ptr = sel_ptr->prev) {
         count++;
         obj_ptr = sel_ptr->obj;
         w += obj_ptr->obbox.rbx - obj_ptr->obbox.ltx;
         h += obj_ptr->obbox.rby - obj_ptr->obbox.lty;
         switch (horiAlign) {
         case ALIGN_N:
         case ALIGN_L:
         case ALIGN_S:
            for (hori_sel=left_sel; hori_sel!=NULL; hori_sel=hori_sel->next) {
               if (hori_sel->obj->obbox.ltx > obj_ptr->obbox.ltx ||
                     (hori_sel->obj->obbox.ltx == obj_ptr->obbox.ltx &&
                     hori_sel->obj->obbox.lty > obj_ptr->obbox.lty)) {
                  break;
               }
            }
            break;
         case ALIGN_C:
            for (hori_sel=left_sel; hori_sel!=NULL; hori_sel=hori_sel->next) {
               if (hori_sel->obj->obbox.ltx+hori_sel->obj->obbox.rbx >
                     obj_ptr->obbox.ltx+obj_ptr->obbox.rbx ||
                     (hori_sel->obj->obbox.ltx+hori_sel->obj->obbox.rbx ==
                     obj_ptr->obbox.ltx+obj_ptr->obbox.rbx &&
                     hori_sel->obj->obbox.lty+hori_sel->obj->obbox.rby >
                     obj_ptr->obbox.lty+obj_ptr->obbox.rby)) {
                  break;
               }
            }
            break;
         case ALIGN_R:
            for (hori_sel=left_sel; hori_sel!=NULL; hori_sel=hori_sel->next) {
               if (hori_sel->obj->obbox.rbx > obj_ptr->obbox.rbx ||
                     (hori_sel->obj->obbox.rbx == obj_ptr->obbox.rbx &&
                     hori_sel->obj->obbox.rby > obj_ptr->obbox.rby)) {
                  break;
               }
            }
            break;
         }
         new_sel = (struct SelRec *)malloc(sizeof(struct SelRec));
         if (new_sel == NULL) FailAllocMessage();
         new_sel->obj = obj_ptr;
         new_sel->next = hori_sel;
         if (hori_sel == NULL) {
            new_sel->prev = right_sel;
            right_sel->next = new_sel;
            right_sel = new_sel;
         } else {
            new_sel->prev = hori_sel->prev;
            if (hori_sel->prev == NULL) {
               left_sel = new_sel;
            } else {
               hori_sel->prev->next = new_sel;
            }
            hori_sel->prev = new_sel;
         }
         switch (vertAlign) {
         case ALIGN_N:
         case ALIGN_T:
         case ALIGN_S:
            for (vert_sel=top_sel; vert_sel!=NULL; vert_sel=vert_sel->next) {
               if (vert_sel->obj->obbox.lty > obj_ptr->obbox.lty ||
                     (vert_sel->obj->obbox.lty == obj_ptr->obbox.lty &&
                     vert_sel->obj->obbox.ltx > obj_ptr->obbox.ltx)) {
                  break;
               }
            }
            break;
         case ALIGN_M:
            for (vert_sel=top_sel; vert_sel!=NULL; vert_sel=vert_sel->next) {
               if (vert_sel->obj->obbox.lty+vert_sel->obj->obbox.rby >
                     obj_ptr->obbox.lty+obj_ptr->obbox.rby ||
                     (vert_sel->obj->obbox.lty+vert_sel->obj->obbox.rby ==
                     obj_ptr->obbox.lty+obj_ptr->obbox.rby &&
                     vert_sel->obj->obbox.ltx+vert_sel->obj->obbox.rbx >
                     obj_ptr->obbox.ltx+obj_ptr->obbox.rbx)) {
                  break;
               }
            }
            break;
         case ALIGN_B:
            for (vert_sel=top_sel; vert_sel!=NULL; vert_sel=vert_sel->next) {
               if (vert_sel->obj->obbox.rby > obj_ptr->obbox.rby ||
                     (vert_sel->obj->obbox.rby == obj_ptr->obbox.rby &&
                     vert_sel->obj->obbox.rbx > obj_ptr->obbox.rbx)) {
                  break;
               }
            }
            break;
         }
         new_sel = (struct SelRec *)malloc(sizeof(struct SelRec));
         if (new_sel == NULL) FailAllocMessage();
         new_sel->obj = obj_ptr;
         new_sel->next = vert_sel;
         if (vert_sel == NULL)
         {
            new_sel->prev = bottom_sel;
            bottom_sel->next = new_sel;
            bottom_sel = new_sel;
         } else {
            new_sel->prev = vert_sel->prev;
            if (vert_sel->prev == NULL) {
               top_sel = new_sel;
            } else {
               vert_sel->prev->next = new_sel;
            }
            vert_sel->prev = new_sel;
         }
      }
      switch (horiAlign) {
      case ALIGN_N:
      case ALIGN_L:
         x = (double)(left_sel->obj->obbox.ltx);
         h_dist = (right_sel->obj->obbox.ltx-x)/(count-1);
         break;
      case ALIGN_C:
         x = (double)(left_sel->obj->obbox.rbx+left_sel->obj->obbox.ltx);
         h_dist = (right_sel->obj->obbox.rbx+right_sel->obj->obbox.ltx-x) /
               (count-1);
         break;
      case ALIGN_R:
         x = (double)(left_sel->obj->obbox.rbx);
         h_dist = (right_sel->obj->obbox.rbx-x)/(count-1);
         break;
      case ALIGN_S:
         x = (double)(left_sel->obj->obbox.rbx);
         h_dist = (right_sel->obj->obbox.rbx-left_sel->obj->obbox.ltx-w) /
               (count-1);
         break;
      }
      switch (vertAlign) {
      case ALIGN_N:
      case ALIGN_T:
         y = (double)(top_sel->obj->obbox.lty);
         v_dist = (bottom_sel->obj->obbox.lty-y)/(count-1);
         break;
      case ALIGN_M:
         y = (double)(top_sel->obj->obbox.rby+top_sel->obj->obbox.lty);
         v_dist = (bottom_sel->obj->obbox.rby +
               bottom_sel->obj->obbox.lty-y) / (count-1);
         break;
      case ALIGN_B:
         y = (double)(top_sel->obj->obbox.rby);
         v_dist = (bottom_sel->obj->obbox.rby-y)/(count-1);
         break;
      case ALIGN_S:
         y = (double)(top_sel->obj->obbox.rby);
         v_dist = (bottom_sel->obj->obbox.rby-top_sel->obj->obbox.lty-h) /
               (count-1);
         break;
      }
      for (sel_ptr=left_sel; sel_ptr->next!=right_sel; sel_ptr=next_sel) {
         switch (horiAlign) {
         case ALIGN_N: dx = 0; break;
         case ALIGN_L:
            dx = round(x+h_dist-sel_ptr->next->obj->obbox.ltx);
            break;
         case ALIGN_C:
            dx = round((x + h_dist - sel_ptr->next->obj->obbox.rbx -
                  sel_ptr->next->obj->obbox.ltx) / 2.0);
            break;
         case ALIGN_R:
            dx = round(x+h_dist-sel_ptr->next->obj->obbox.rbx);
            break;
         case ALIGN_S:
            dx = round(x+h_dist-sel_ptr->next->obj->obbox.ltx);
            break;
         }
         if (dx != 0) {
            move_cmd->dx = dx;
            move_cmd->dy = 0;
            tmp_sel_ptr->obj = sel_ptr->next->obj;
            PrepareToRecord(CMD_MOVE, NULL, NULL, 0);
            RecordCmd(CMD_MOVE, sub_cmd, tmp_sel_ptr, tmp_sel_ptr, 1);

            MoveObj(sel_ptr->next->obj, dx, 0);
         }

         if (horiAlign == ALIGN_S) {
            x = sel_ptr->next->obj->obbox.rbx;
         } else {
            x += h_dist;
         }
         next_sel = sel_ptr->next;
         free(sel_ptr);
      }
      free(sel_ptr);
      free(right_sel);

      for (sel_ptr=top_sel; sel_ptr->next!=bottom_sel; sel_ptr=next_sel) {
         switch (vertAlign) {
         case ALIGN_N: dy = 0; break;
         case ALIGN_T:
            dy = round(y+v_dist-sel_ptr->next->obj->obbox.lty);
            break;
         case ALIGN_M:
            dy = round((y + v_dist - sel_ptr->next->obj->obbox.rby -
                  sel_ptr->next->obj->obbox.lty) / 2.0);
            break;
         case ALIGN_B:
            dy = round(y+v_dist-sel_ptr->next->obj->obbox.rby);
            break;
         case ALIGN_S:
            dy = round(y+v_dist-sel_ptr->next->obj->obbox.lty);
            break;
         }
         if (dy != 0) {
            move_cmd->dx = 0;
            move_cmd->dy = dy;
            tmp_sel_ptr->obj = sel_ptr->next->obj;
            PrepareToRecord(CMD_MOVE, NULL, NULL, 0);
            RecordCmd(CMD_MOVE, sub_cmd, tmp_sel_ptr, tmp_sel_ptr, 1);

            MoveObj(sel_ptr->next->obj, 0, dy);
         }

         if (vertAlign == ALIGN_S) {
            y = sel_ptr->next->obj->obbox.rby;
         } else {
            y += v_dist;
         }
         next_sel = sel_ptr->next;
         free(sel_ptr);
      }
      free(sel_ptr);
      free(bottom_sel);

      EndCompositeCmd();
      free(move_cmd);
      free(sub_cmd);
      free(tmp_sel_ptr);

      switch (horiAlign) {
      case ALIGN_L:
         sprintf(gszMsgBox, "Left sides of objects are %1d pixels apart.",
               round(h_dist));
         Msg(gszMsgBox);
         break;
      case ALIGN_C:
         sprintf(gszMsgBox, "Center of objects are %1d pixels apart.",
               round(h_dist/2.0));
         Msg(gszMsgBox);
         break;
      case ALIGN_R:
         sprintf(gszMsgBox, "Right sides of objects are %1d pixels apart.",
               round(h_dist));
         Msg(gszMsgBox);
         break;
      case ALIGN_S:
         sprintf(gszMsgBox, "Objects are spaced %1d pixels apart horizontally.",
               round(h_dist));
         Msg(gszMsgBox);
         break;
      }
      switch (vertAlign) {
      case ALIGN_T:
         sprintf(gszMsgBox, "Top sides of objects are %1d pixels apart.",
               round(v_dist));
         Msg(gszMsgBox);
         break;
      case ALIGN_M:
         sprintf(gszMsgBox, "Middle of objects are %1d pixels apart.",
               round(v_dist/2.0));
         Msg(gszMsgBox);
         break;
      case ALIGN_B:
         sprintf(gszMsgBox, "Bottom sides of objects are %1d pixels apart.",
               round(v_dist));
         Msg(gszMsgBox);
         break;
      case ALIGN_S:
         sprintf(gszMsgBox, "Objects are spaced %1d pixels apart vertically.",
               round(v_dist));
         Msg(gszMsgBox);
         break;
      }
   }
   ltx = selLtX; lty = selLtY; rbx = selRbX, rby = selRbY;
   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward();
   SetFileModified(TRUE);
   justDupped = FALSE;
}

static
int AutoJustifiable(obj_ptr)
   struct ObjRec *obj_ptr;
{
   struct TextRec *text_ptr;
   int auto_justifiable=TRUE, rotate, just;

   if (obj_ptr->type!=OBJ_TEXT || obj_ptr->detail.t->lines!=1) return FALSE;

   text_ptr=obj_ptr->detail.t;
   rotate=text_ptr->rotate;
   just=text_ptr->just;

   switch (rotate) {
   case ROTATE0:
      if ((horiAlign==ALIGN_L && just==JUST_L) ||
            (horiAlign==ALIGN_C && just==JUST_C) ||
            (horiAlign==ALIGN_R && just==JUST_R)) {
         auto_justifiable = FALSE;
      }
      break;
   case ROTATE90:
      if ((vertAlign==ALIGN_T && just==JUST_L) ||
            (vertAlign==ALIGN_M && just==JUST_C) ||
            (vertAlign==ALIGN_B && just==JUST_R)) {
         auto_justifiable = FALSE;
      }
      break;
   case ROTATE180:
      if ((horiAlign==ALIGN_L && just==JUST_R) ||
            (horiAlign==ALIGN_C && just==JUST_C) ||
            (horiAlign==ALIGN_R && just==JUST_L)) {
         auto_justifiable = FALSE;
      }
      break;
   case ROTATE270:
      if ((vertAlign==ALIGN_T && just==JUST_R) ||
            (vertAlign==ALIGN_M && just==JUST_C) ||
            (vertAlign==ALIGN_B && just==JUST_L)) {
         auto_justifiable = FALSE;
      }
      break;
   }
   return auto_justifiable;
}

static
void AutoJustify(obj_ptr)
   struct ObjRec *obj_ptr;
{
   struct TextRec *text_ptr;
   int new_ltx, new_lty, dx, dy, ltx, lty, rotate;

   text_ptr=obj_ptr->detail.t;
   rotate=text_ptr->rotate;

   ltx = obj_ptr->obbox.ltx; lty = obj_ptr->obbox.lty;
   switch (rotate) {
   case ROTATE0:
      switch (horiAlign) {
      case ALIGN_L: text_ptr->just = JUST_L; break;
      case ALIGN_C: text_ptr->just = JUST_C; break;
      case ALIGN_R: text_ptr->just = JUST_R; break;
      }
      break;
   case ROTATE90:
      switch (vertAlign) {
      case ALIGN_T: text_ptr->just = JUST_L; break;
      case ALIGN_M: text_ptr->just = JUST_C; break;
      case ALIGN_B: text_ptr->just = JUST_R; break;
      }
      break;
   case ROTATE180:
      switch (horiAlign) {
      case ALIGN_L: text_ptr->just = JUST_R; break;
      case ALIGN_C: text_ptr->just = JUST_C; break;
      case ALIGN_R: text_ptr->just = JUST_L; break;
      }
      break;
   case ROTATE270:
      switch (vertAlign) {
      case ALIGN_T: text_ptr->just = JUST_R; break;
      case ALIGN_M: text_ptr->just = JUST_C; break;
      case ALIGN_B: text_ptr->just = JUST_L; break;
      }
      break;
   }
   UpdTextBBox(obj_ptr);
   dx = dy = 0;
   new_ltx = obj_ptr->obbox.ltx; new_lty = obj_ptr->obbox.lty;
   switch (rotate) {
   case ROTATE0: dx = ltx-new_ltx; break;
   case ROTATE180: dx = ltx-new_ltx; break;
   case ROTATE90: dy = lty-new_lty; break;
   case ROTATE270: dy = lty-new_lty; break;
   }
   if (text_ptr->cached_bitmap != None) {
      XFreePixmap(mainDisplay, text_ptr->cached_bitmap);
   }
   text_ptr->cached_zoom = 0;
   text_ptr->cached_bitmap = None;

   MoveObj(obj_ptr, dx, dy);
}

void AlignSelObjs()
{
   register int x=0, y=0, i;
   struct SelRec *sel_ptr;
   struct ObjRec *obj_ptr;
   struct VSelRec *vsel_ptr;
   int pivot_x = 0, pivot_y = 0, auto_justifiable;
   int dx, dy, ltx=0, lty=0, rbx=0, rby=0;

   if (topSel == NULL && topVSel == NULL) return;

   if (curChoice == VERTEXMODE) {
      int changed=FALSE, num_pts=0, first_time=TRUE, start;
      IntPoint *v=NULL;

      if (topVSel!=NULL && (topVSel->next!=NULL || topVSel->n>=3  ||
            (topVSel->n==2 &&
            !(topVSel->obj->type==OBJ_POLYGON && topVSel->v_index[0]==0))) &&
            horiAlign!=ALIGN_N && horiAlign!=ALIGN_S &&
            vertAlign!=ALIGN_N && vertAlign!=ALIGN_S) {
         if (MsgBox("Okay to move all vertices to one point? [ync](y)",
               TOOL_NAME, YNC_MB) != MB_ID_YES) {
            return;
         }
      }
      StartCompositeCmd();
      HighLightReverse();
      for (vsel_ptr = topVSel; vsel_ptr != NULL; vsel_ptr = vsel_ptr->next) {
         if (first_time) {
            first_time = FALSE;
            ltx = vsel_ptr->x[0]; lty = vsel_ptr->y[0];
            rbx = vsel_ptr->x[0]; rby = vsel_ptr->y[0];
            start = 1;
         } else {
            start = 0;
         }
         for (i = start; i < vsel_ptr->n; i++) {
            if (vsel_ptr->x[i] < ltx) ltx = vsel_ptr->x[i];
            if (vsel_ptr->y[i] < lty) lty = vsel_ptr->y[i];
            if (vsel_ptr->x[i] > rbx) rbx = vsel_ptr->x[i];
            if (vsel_ptr->y[i] > rby) rby = vsel_ptr->y[i];
         }
      }
      switch (horiAlign) {
      case ALIGN_L: pivot_x = ltx; break;
      case ALIGN_C: pivot_x = (ltx + rbx) / 2; break;
      case ALIGN_R: pivot_x = rbx; break;
      }
      switch (vertAlign) {
      case ALIGN_T: pivot_y = lty; break;
      case ALIGN_M: pivot_y = (lty + rby) / 2; break;
      case ALIGN_B: pivot_y = rby; break;
      }

      for (vsel_ptr = topVSel; vsel_ptr != NULL; vsel_ptr = vsel_ptr->next) {
         obj_ptr = vsel_ptr->obj;
         switch (obj_ptr->type) {
         case OBJ_POLY:
            num_pts = obj_ptr->detail.p->n;
            v = obj_ptr->detail.p->vlist;
            break;
         case OBJ_POLYGON:
            num_pts = obj_ptr->detail.g->n;
            v = obj_ptr->detail.g->vlist;
            break;
         }
         PrepareToReplaceAnObj(obj_ptr);
         for (i = 0; i < vsel_ptr->n; i++) {
            if (horiAlign!=ALIGN_N && horiAlign!=ALIGN_S &&
                  vsel_ptr->x[i]!=pivot_x) {
               changed = TRUE;
               vsel_ptr->x[i] = pivot_x;
               v[vsel_ptr->v_index[i]].x = pivot_x;
            }
            if (vertAlign!=ALIGN_N && vertAlign!=ALIGN_S &&
                  vsel_ptr->y[i]!=pivot_y) {
               changed = TRUE;
               vsel_ptr->y[i] = pivot_y;
               v[vsel_ptr->v_index[i]].y = pivot_y;
            }
         }
         if (changed) {
            AdjObjSplineVs(obj_ptr);
            switch (obj_ptr->type) {
            case OBJ_POLY:
               if (obj_ptr->detail.p->curved != LT_INTSPLINE) {
                  UpdPolyBBox(obj_ptr, num_pts, v);
               } else {
                  UpdPolyBBox(obj_ptr, obj_ptr->detail.p->intn,
                        obj_ptr->detail.p->intvlist);
               }
               break;
            case OBJ_POLYGON:
               if (obj_ptr->detail.g->curved != LT_INTSPLINE) {
                  UpdPolyBBox(obj_ptr, num_pts, v);
               } else {
                  UpdPolyBBox(obj_ptr, obj_ptr->detail.g->intn,
                        obj_ptr->detail.g->intvlist);
               }
               break;
            }
            RecordReplaceAnObj(obj_ptr);
         }
         else
            AbortPrepareCmd(CMD_REPLACE);
      }
      EndCompositeCmd();
   } else {
      struct MoveSubCmdRec *move_cmd;
      struct SubCmdRec *sub_cmd;
      struct SelRec *tmp_sel_ptr;
      struct ObjRec *locked_obj=NULL;

      if (numObjLocked > 1) {
         MsgBox("Can not align objects.  Too many objects locked.", TOOL_NAME,
               INFO_MB);
         return;
      } else if (numObjLocked == 1) {
         for (sel_ptr=topSel; sel_ptr!=NULL; sel_ptr=sel_ptr->next) {
            if (sel_ptr->obj->locked) {
               locked_obj = sel_ptr->obj;
               break;
            }
         }
      }
      tmp_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (tmp_sel_ptr == NULL) FailAllocMessage();
      tmp_sel_ptr->next = tmp_sel_ptr->prev = NULL;

      move_cmd = (struct MoveSubCmdRec *)malloc(sizeof(struct MoveSubCmdRec));
      if (move_cmd == NULL) FailAllocMessage();
      memset(move_cmd, 0, sizeof(struct MoveSubCmdRec));
      sub_cmd = (struct SubCmdRec *)malloc(sizeof(struct SubCmdRec));
      if (sub_cmd == NULL) FailAllocMessage();
      memset(sub_cmd, 0, sizeof(struct SubCmdRec));
      sub_cmd->detail.mv = move_cmd;

      StartCompositeCmd();
      HighLightReverse();
      switch (horiAlign) {
      case ALIGN_L:
         pivot_x = (locked_obj==NULL) ? selObjLtX : locked_obj->obbox.ltx;
         break;
      case ALIGN_C:
         pivot_x = (locked_obj==NULL) ? ((selObjLtX+selObjRbX)>>1) :
               ((locked_obj->obbox.ltx+locked_obj->obbox.rbx)>>1);
         break;
      case ALIGN_R:
         pivot_x = (locked_obj==NULL) ? selObjRbX : locked_obj->obbox.rbx;
         break;
      }
      switch (vertAlign) {
      case ALIGN_T:
         pivot_y = (locked_obj==NULL) ? selObjLtY : locked_obj->obbox.lty;
         break;
      case ALIGN_M:
         pivot_y = (locked_obj==NULL) ? ((selObjLtY+selObjRbY)>>1) :
               ((locked_obj->obbox.lty+locked_obj->obbox.rby)>>1);
         break;
      case ALIGN_B:
         pivot_y = (locked_obj==NULL) ? selObjRbY : locked_obj->obbox.rby;
         break;
      }

      for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next) {
         obj_ptr = sel_ptr->obj;
         if (obj_ptr->locked) continue;

         if ((auto_justifiable = AutoJustifiable(obj_ptr))) {
            PrepareToReplaceAnObj(obj_ptr);
            AutoJustify(obj_ptr);
         }
         switch (horiAlign) {
         case ALIGN_L: x = obj_ptr->obbox.ltx; break;
         case ALIGN_C: x = (obj_ptr->obbox.ltx+obj_ptr->obbox.rbx)/2; break;
         case ALIGN_R: x = obj_ptr->obbox.rbx; break;
         }
         switch (vertAlign) {
         case ALIGN_T: y = obj_ptr->obbox.lty; break;
         case ALIGN_M: y = (obj_ptr->obbox.lty+obj_ptr->obbox.rby)/2; break;
         case ALIGN_B: y = obj_ptr->obbox.rby; break;
         }
         if (horiAlign==ALIGN_N || horiAlign==ALIGN_S) x = pivot_x;
         if (vertAlign==ALIGN_N || vertAlign==ALIGN_S) y = pivot_y;

         dx = pivot_x - x;
         dy = pivot_y - y;
         if (dx != 0 || dy != 0) {
            if (auto_justifiable) {
               MoveObj(obj_ptr, dx, dy);
               RecordReplaceAnObj(obj_ptr);
            } else {
               move_cmd->dx = dx;
               move_cmd->dy = dy;
               tmp_sel_ptr->obj = obj_ptr;
               PrepareToRecord(CMD_MOVE, NULL, NULL, 0);
               RecordCmd(CMD_MOVE, sub_cmd, tmp_sel_ptr, tmp_sel_ptr, 1);

               MoveObj(obj_ptr, dx, dy);
            }
         } else if (auto_justifiable) {
            RecordReplaceAnObj(obj_ptr);
         }
      }
      EndCompositeCmd();

      free(move_cmd);
      free(sub_cmd);
      free(tmp_sel_ptr);

      switch ((horiAlign<<ALIGN_SHIFT)|vertAlign) {
      case ALIGN_NN: break;
      case ALIGN_NT: Msg("top sides are aligned."); break;
      case ALIGN_NM: Msg("vertical centers are aligned."); break;
      case ALIGN_NB: Msg("bottom sides are aligned."); break;
      case ALIGN_NS: break;
      case ALIGN_LN: Msg("left sides are aligned."); break;
      case ALIGN_LT: Msg("left and top sides are aligned."); break;
      case ALIGN_LM: Msg("left side and middle are aligned."); break;
      case ALIGN_LB: Msg("left and bottom sides are aligned."); break;
      case ALIGN_LS: Msg("left sides are aligned."); break;
      case ALIGN_CN: Msg("horizontal centers are aligned."); break;
      case ALIGN_CT: Msg("centers and top sides are aligned."); break;
      case ALIGN_CM: Msg("centers and middle are aligned."); break;
      case ALIGN_CB: Msg("centers and bottom sides are aligned."); break;
      case ALIGN_CS: Msg("horizontal centers are aligned."); break;
      case ALIGN_RN: Msg("right sides are aligned."); break;
      case ALIGN_RT: Msg("right and top sides are aligned."); break;
      case ALIGN_RM: Msg("right and middle are aligned."); break;
      case ALIGN_RB: Msg("right and bottom sides are aligned."); break;
      case ALIGN_RS: Msg("right sides are aligned."); break;
      case ALIGN_SN: break;
      case ALIGN_ST: Msg("top sides are aligned."); break;
      case ALIGN_SM: Msg("vertical centers are aligned."); break;
      case ALIGN_SB: Msg("bottom sides are aligned."); break;
      case ALIGN_SS: break;
      }
   }
   ltx = selLtX; lty = selLtY; rbx = selRbX, rby = selRbY;
   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward();
   SetFileModified(TRUE);
   justDupped = FALSE;
}

void AlignSelToPage()
{
   register int i;
   struct SelRec *sel_ptr;
   struct ObjRec *obj_ptr;
   struct VSelRec *vsel_ptr;
   int pivot_x=0, pivot_y=0, dx, dy, ltx, lty, rbx, rby;

   if (topSel == NULL && topVSel == NULL) return;

   if (curChoice == VERTEXMODE) {
      int changed=FALSE, num_pts=0;
      IntPoint *v=NULL;

      if (topVSel!=NULL && (topVSel->next!=NULL || topVSel->n>=3  ||
            (topVSel->n==2 &&
            !(topVSel->obj->type==OBJ_POLYGON && topVSel->v_index[0]==0))) &&
            horiAlign!=ALIGN_N && horiAlign!=ALIGN_S &&
            vertAlign!=ALIGN_N && vertAlign!=ALIGN_S) {
         if (MsgBox("Okay to move all vertices to one point? [ync](y)",
               TOOL_NAME, YNC_MB) != MB_ID_YES) {
            return;
         }
      }
      StartCompositeCmd();
      HighLightReverse();
      switch (horiAlign) {
      case ALIGN_L: pivot_x = 0; break;
      case ALIGN_C: pivot_x = paperWidth / 2; break;
      case ALIGN_R: pivot_x = paperWidth; break;
      }
      switch (vertAlign) {
      case ALIGN_T: pivot_y = 0; break;
      case ALIGN_M: pivot_y = paperHeight / 2; break;
      case ALIGN_B: pivot_y = paperHeight; break;
      }

      for (vsel_ptr = topVSel; vsel_ptr != NULL; vsel_ptr = vsel_ptr->next) {
         obj_ptr = vsel_ptr->obj;
         switch (obj_ptr->type) {
         case OBJ_POLY:
            num_pts = obj_ptr->detail.p->n;
            v = obj_ptr->detail.p->vlist;
            break;
         case OBJ_POLYGON:
            num_pts = obj_ptr->detail.g->n;
            v = obj_ptr->detail.g->vlist;
            break;
         }
         PrepareToReplaceAnObj(obj_ptr);
         for (i = 0; i < vsel_ptr->n; i++) {
            if (horiAlign!=ALIGN_N && horiAlign!=ALIGN_S &&
                  vsel_ptr->x[i]!=pivot_x) {
               changed = TRUE;
               vsel_ptr->x[i] = pivot_x;
               v[vsel_ptr->v_index[i]].x = pivot_x;
            }
            if (vertAlign!=ALIGN_N && vertAlign!=ALIGN_S &&
                  vsel_ptr->y[i]!=pivot_y) {
               changed = TRUE;
               vsel_ptr->y[i] = pivot_y;
               v[vsel_ptr->v_index[i]].y = pivot_y;
            }
         }
         if (changed) {
            AdjObjSplineVs(obj_ptr);
            switch (obj_ptr->type) {
            case OBJ_POLY:
               if (obj_ptr->detail.p->curved != LT_INTSPLINE) {
                  UpdPolyBBox(obj_ptr, num_pts, v);
               } else {
                  UpdPolyBBox(obj_ptr, obj_ptr->detail.p->intn,
                        obj_ptr->detail.p->intvlist);
               }
               break;
            case OBJ_POLYGON:
               if (obj_ptr->detail.g->curved != LT_INTSPLINE) {
                  UpdPolyBBox(obj_ptr, num_pts, v);
               } else {
                  UpdPolyBBox(obj_ptr, obj_ptr->detail.g->intn,
                        obj_ptr->detail.g->intvlist);
               }
               break;
            }
            RecordReplaceAnObj(obj_ptr);
         }
         else
            AbortPrepareCmd(CMD_REPLACE);
      }
      EndCompositeCmd();
   } else {
      HighLightReverse();
      dx = dy = 0;
      switch (horiAlign) {
      case ALIGN_L: dx = 0 - selLtX; break;
      case ALIGN_C: dx = (paperWidth>>1) - ((selRbX+selLtX)>>1); break;
      case ALIGN_R: dx = paperWidth - selRbX; break;
      }
      switch (vertAlign) {
      case ALIGN_T: dy = 0 - selLtY; break;
      case ALIGN_M: dy = (paperHeight>>1) - ((selRbY+selLtY)>>1); break;
      case ALIGN_B: dy = paperHeight - selRbY; break;
      }
      if (dx != 0 || dy != 0) {
         struct MoveSubCmdRec *move_cmd;
         struct SubCmdRec *sub_cmd;

         move_cmd = (struct MoveSubCmdRec*)malloc(sizeof(struct MoveSubCmdRec));
         if (move_cmd == NULL) FailAllocMessage();
         memset(move_cmd, 0, sizeof(struct MoveSubCmdRec));
         sub_cmd = (struct SubCmdRec *)malloc(sizeof(struct SubCmdRec));
         if (sub_cmd == NULL) FailAllocMessage();
         memset(sub_cmd, 0, sizeof(struct SubCmdRec));
         sub_cmd->detail.mv = move_cmd;

         move_cmd->dx = dx;
         move_cmd->dy = dy;

         PrepareToRecord(CMD_MOVE, NULL, NULL, 0);
         RecordCmd(CMD_MOVE, sub_cmd, topSel, botSel, numObjSelected);
         for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next) {
            if (!sel_ptr->obj->locked) {
               MoveObj(sel_ptr->obj, dx, dy);
            }
         }
         free(move_cmd);
         free(sub_cmd);
      }
   }
   ltx = selLtX; lty = selLtY; rbx = selRbX, rby = selRbY;
   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward();
   SetFileModified(TRUE);
   justDupped = FALSE;
}

void AlignSelToGrid()
{
   register int x=0, y=0, i;
   struct ObjRec *obj_ptr;
   struct SelRec *sel_ptr;
   struct VSelRec *vsel_ptr;
   int grid_x, grid_y, dx, dy, ltx, lty, rbx, rby, auto_justifiable;

   if (topSel == NULL && topVSel == NULL) return;

   StartCompositeCmd();
   HighLightReverse();
   if (curChoice == VERTEXMODE) {
      for (vsel_ptr = topVSel; vsel_ptr != NULL; vsel_ptr = vsel_ptr->next) {
         int changed=FALSE, num_pts=0;
         IntPoint *v=NULL;

         obj_ptr = vsel_ptr->obj;
         switch (obj_ptr->type) {
         case OBJ_POLY:
            num_pts = obj_ptr->detail.p->n;
            v = obj_ptr->detail.p->vlist;
            break;
         case OBJ_POLYGON:
            num_pts = obj_ptr->detail.g->n;
            v = obj_ptr->detail.g->vlist;
            break;
         }
         PrepareToReplaceAnObj(obj_ptr);
         for (i = 0; i < vsel_ptr->n; i++) {
            if (horiAlign!=ALIGN_N && horiAlign!=ALIGN_S) x = vsel_ptr->x[i];
            if (vertAlign!=ALIGN_N && vertAlign!=ALIGN_S) y = vsel_ptr->y[i];
            if (zoomedIn) {
               GridXY(ZOOMED_SIZE(x), ZOOMED_SIZE(y), &grid_x, &grid_y);
               if (horiAlign==ALIGN_N || horiAlign==ALIGN_S) {
                  x = ABS_SIZE(grid_x);
               }
               if (vertAlign==ALIGN_N || vertAlign==ALIGN_S) {
                  y = ABS_SIZE(grid_y);
               }
               dx = ABS_SIZE(grid_x) - x;
               dy = ABS_SIZE(grid_y) - y;
            } else {
               GridXY(ZOOMED_SIZE(x), ZOOMED_SIZE(y), &grid_x, &grid_y);
               if (horiAlign==ALIGN_N || horiAlign==ALIGN_S) x = grid_x;
               if (vertAlign==ALIGN_N || vertAlign==ALIGN_S) y = grid_y;

               dx = ABS_SIZE(grid_x) - x;
               dy = ABS_SIZE(grid_y) - y;
            }
            if (dx != 0 || dy != 0) {
               changed = TRUE;
               vsel_ptr->x[i] += dx;
               vsel_ptr->y[i] += dy;
               v[vsel_ptr->v_index[i]].x += dx;
               v[vsel_ptr->v_index[i]].y += dy;
            }
         }
         if (changed) {
            AdjObjSplineVs(obj_ptr);
            switch (obj_ptr->type) {
            case OBJ_POLY:
               if (obj_ptr->detail.p->curved != LT_INTSPLINE) {
                  UpdPolyBBox(obj_ptr, num_pts, v);
               } else {
                  UpdPolyBBox(obj_ptr, obj_ptr->detail.p->intn,
                        obj_ptr->detail.p->intvlist);
               }
               break;
            case OBJ_POLYGON:
               if (obj_ptr->detail.g->curved != LT_INTSPLINE) {
                  UpdPolyBBox(obj_ptr, num_pts, v);
               } else {
                  UpdPolyBBox(obj_ptr, obj_ptr->detail.g->intn,
                        obj_ptr->detail.g->intvlist);
               }
               break;
            }
            RecordReplaceAnObj(obj_ptr);
         } else {
            AbortPrepareCmd(CMD_REPLACE);
         }
      }
   } else {
      struct MoveSubCmdRec *move_cmd;
      struct SubCmdRec *sub_cmd;
      struct SelRec *tmp_sel_ptr;

      tmp_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (tmp_sel_ptr == NULL) FailAllocMessage();
      tmp_sel_ptr->next = tmp_sel_ptr->prev = NULL;

      move_cmd = (struct MoveSubCmdRec *)malloc(sizeof(struct MoveSubCmdRec));
      if (move_cmd == NULL) FailAllocMessage();
      memset(move_cmd, 0, sizeof(struct MoveSubCmdRec));
      sub_cmd = (struct SubCmdRec *)malloc(sizeof(struct SubCmdRec));
      if (sub_cmd == NULL) FailAllocMessage();
      memset(sub_cmd, 0, sizeof(struct SubCmdRec));
      sub_cmd->detail.mv = move_cmd;

      for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next) {
         obj_ptr = sel_ptr->obj;
         if (obj_ptr->locked) continue;

         if ((auto_justifiable=AutoJustifiable(obj_ptr))) {
            PrepareToReplaceAnObj(obj_ptr);
            AutoJustify(obj_ptr);
         }
         switch (horiAlign) {
         case ALIGN_L: x = obj_ptr->obbox.ltx; break;
         case ALIGN_C: x = (obj_ptr->obbox.ltx+obj_ptr->obbox.rbx)/2; break;
         case ALIGN_R: x = obj_ptr->obbox.rbx; break;
         }
         switch (vertAlign) {
         case ALIGN_T: y = obj_ptr->obbox.lty; break;
         case ALIGN_M: y = (obj_ptr->obbox.lty+obj_ptr->obbox.rby)/2; break;
         case ALIGN_B: y = obj_ptr->obbox.rby; break;
         }
         if (zoomedIn) {
            GridXY(ZOOMED_SIZE(x), ZOOMED_SIZE(y), &grid_x, &grid_y);
            if (horiAlign==ALIGN_N || horiAlign==ALIGN_S) x = ABS_SIZE(grid_x);
            if (vertAlign==ALIGN_N || vertAlign==ALIGN_S) y = ABS_SIZE(grid_y);

            dx = ABS_SIZE(grid_x) - x;
            dy = ABS_SIZE(grid_y) - y;
         } else {
            GridXY(ZOOMED_SIZE(x), ZOOMED_SIZE(y), &grid_x, &grid_y);
            if (horiAlign==ALIGN_N || horiAlign==ALIGN_S) x = grid_x;
            if (vertAlign==ALIGN_N || vertAlign==ALIGN_S) y = grid_y;

            dx = ABS_SIZE(grid_x) - x;
            dy = ABS_SIZE(grid_y) - y;
         }
         if (dx != 0 || dy != 0) {
            if (auto_justifiable) {
               MoveObj(obj_ptr, dx, dy);
               RecordReplaceAnObj(obj_ptr);
            } else {
               move_cmd->dx = dx;
               move_cmd->dy = dy;
               tmp_sel_ptr->obj = obj_ptr;
               PrepareToRecord(CMD_MOVE, NULL, NULL, 0);
               RecordCmd(CMD_MOVE, sub_cmd, tmp_sel_ptr, tmp_sel_ptr, 1);

               MoveObj(obj_ptr, dx, dy);
            }
         } else if (auto_justifiable) {
            RecordReplaceAnObj(obj_ptr);
         }
      }
      free(move_cmd);
      free(sub_cmd);
      free(tmp_sel_ptr);
   }
   EndCompositeCmd();

   ltx = selLtX; lty = selLtY; rbx = selRbX, rby = selRbY;
   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward();
   SetFileModified(TRUE);
   justDupped = FALSE;
}

void HoriAlignSubMenu(index)
   int index;
{
   horiAlign = index;
   switch (horiAlign) {
   case ALIGN_N: Msg("Horizontal alignment set to NONE."); break;
   case ALIGN_L: Msg("Will align on the LEFT."); break;
   case ALIGN_C: Msg("Will align objects at the horizontal CENTER."); break;
   case ALIGN_R: Msg("Will align on the RIGHT."); break;
   case ALIGN_S: Msg("Will equally SPACE objects horizontally."); break;
   }
   ShowHoriAlign();
   UpdateSubMenu(MENU_HORIALIGN);
}

static char * alignHoriDescription[] =
{
   "Set horizontal alignment to none",
   "Set horizontal alignment to left aligned",
   "Set horizontal alignment to center aligned",
   "Set horizontal alignment to right aligned",
   "Set horizontal distribution to equal-spacing",
   NULL
};

int HoriAlignMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, * fore_colors, * valid, * init_rv;

   DefaultColorArrays(MAXALIGNS, &fore_colors, &valid, &init_rv, NULL);
   free(valid);
   init_rv[horiAlign] = TRUE;
   activeMenu = MENU_HORIALIGN;
   index = PxMpMenuLoop(X, Y, choiceImageW, choiceImageH, MAXALIGNS, 1,
         MAXALIGNS, fore_colors, alignHoriPixmap, init_rv,
         alignHoriDescription, SINGLECOLOR, TrackMenubar);

   if (index >= 0) HoriAlignSubMenu(index);
   return index;
}

void VertAlignSubMenu(index)
   int index;
{
   vertAlign = index;
   switch (vertAlign) {
   case ALIGN_N: Msg("Vertical alignment set to NONE."); break;
   case ALIGN_T: Msg("Will align at the TOP."); break;
   case ALIGN_M: Msg("Will align objects in the MIDDLE vertically."); break;
   case ALIGN_B: Msg("Will align at the BOTTOM."); break;
   case ALIGN_S: Msg("Will equally SPACE objects vertically."); break;
   }
   ShowVertAlign();
   UpdateSubMenu(MENU_VERTALIGN);
}

static char * alignVertDescription[] =
{
   "Set vertical alignment to none",
   "Set vertical alignment to top aligned",
   "Set vertical alignment to middel aligned",
   "Set vertical alignment to bottom aligned",
   "Set vertical distribution to equal-spacing",
   NULL
};

int VertAlignMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, * fore_colors, * valid, * init_rv;

   DefaultColorArrays(MAXALIGNS, &fore_colors, &valid, &init_rv, NULL);
   free(valid);
   init_rv[vertAlign] = TRUE;
   activeMenu = MENU_VERTALIGN;
   index = PxMpMenuLoop(X, Y, choiceImageW, choiceImageH, MAXALIGNS, 1,
         MAXALIGNS, fore_colors, alignVertPixmap, init_rv,
         alignVertDescription, SINGLECOLOR, TrackMenubar);

   if (index >= 0) VertAlignSubMenu(index);
   return index;
}

void CenterAnEndPoint()
{
   struct ObjRec *other_obj, *poly_obj;
   IntPoint *v;
   int cx, cy, x1, y1, xn, yn, d1, dn, num_pts, index, x, y, tmp_x, tmp_y;

   if (curChoice != NOTHING) return;
   if (topSel == NULL) return;
   if (numObjSelected != 2 ||
         (!((topSel->obj->type == OBJ_POLY && botSel->obj->type != OBJ_POLY) ||
         (topSel->obj->type != OBJ_POLY && botSel->obj->type == OBJ_POLY)))) {
      MsgBox("Please select one polyline and one non-polyline object.",
            TOOL_NAME, INFO_MB);
      return;
   }
   if (topSel->obj->type == OBJ_POLY) {
      poly_obj = topSel->obj;
      other_obj = botSel->obj;
   } else {
      poly_obj = botSel->obj;
      other_obj = topSel->obj;
   }
   if (poly_obj->locked) {
      MsgBox("Can not move a vertex for a locked polyline object.",
            TOOL_NAME, INFO_MB);
      return;
   }
   cx = (other_obj->obbox.ltx+other_obj->obbox.rbx)>>1;
   cy = (other_obj->obbox.lty+other_obj->obbox.rby)>>1;
   num_pts = poly_obj->detail.p->n;
   v = poly_obj->detail.p->vlist;
   if (poly_obj->ctm == NULL) {
      x1 = v[0].x;
      y1 = v[0].y;
      xn = v[num_pts-1].x;
      yn = v[num_pts-1].y;
   } else {
      TransformPointThroughCTM(v[0].x-poly_obj->x, v[0].y-poly_obj->y,
            poly_obj->ctm, &tmp_x, &tmp_y);
      x1 = tmp_x+poly_obj->x;
      y1 = tmp_y+poly_obj->y;
      TransformPointThroughCTM(v[num_pts-1].x-poly_obj->x,
            v[num_pts-1].y-poly_obj->y, poly_obj->ctm, &tmp_x, &tmp_y);
      xn = tmp_x+poly_obj->x;
      yn = tmp_y+poly_obj->y;
   }
   d1 = (x1-cx)*(x1-cx)+(y1-cy)*(y1-cy);
   dn = (xn-cx)*(xn-cx)+(yn-cy)*(yn-cy);
   if (d1 <= dn) {
      index = 0;
      x = x1;
      y = y1;
   } else {
      index = num_pts-1;
      x = xn;
      y = yn;
   }
   if (cx != x || cy != y) {
      int ltx=0, lty=0, rbx=0, rby=0;

      HighLightReverse();
      PrepareToReplaceAnObj(poly_obj);
      if (poly_obj->ctm == NULL) {
         v[index].x = cx;
         v[index].y = cy;
      } else {
         ReverseTransformPointThroughCTM(cx-poly_obj->x, cy-poly_obj->y,
               poly_obj->ctm, &tmp_x, &tmp_y);
         v[index].x = tmp_x+poly_obj->x;
         v[index].y = tmp_y+poly_obj->y;
      }
      AdjObjSplineVs(poly_obj);
      if (poly_obj->detail.p->curved != LT_INTSPLINE) {
         UpdPolyBBox(poly_obj, num_pts, v);
      } else {
         UpdPolyBBox(poly_obj, poly_obj->detail.p->intn,
               poly_obj->detail.p->intvlist);
      }
      RecordReplaceAnObj(poly_obj);
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
            selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      HighLightForward();
      SetFileModified(TRUE);
      justDupped = FALSE;
   }
}
