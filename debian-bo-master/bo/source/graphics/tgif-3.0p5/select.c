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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/select.c,v 3.0 1996/05/06 16:07:26 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#include "button.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "exec.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "group.e"
#include "mainloop.e"
#include "mark.e"
#include "menu.e"
#include "move.e"
#include "msg.e"
#include "names.e"
#include "obj.e"
#include "page.e"
#include "poly.e"
#include "raster.e"
#include "rect.e"
#include "remote.e"
#include "ruler.e"
#include "scroll.e"
#ifndef _NO_EXTERN
#include "select.e"
#endif
#include "setup.e"
#include "stk.e"
#include "stretch.e"

#ifndef XK_KP_Left
#define XK_KP_Left	0xFF96
#define XK_KP_Up	0xFF97
#define XK_KP_Right	0xFF98
#define XK_KP_Down	0xFF99
#endif /* ~XK_KP_LEFT */

extern int	atoi ARGS_DECL((char *));

#define FORWARD 0
#define REVERSE 1

int		selLtX, selLtY, selRbX, selRbY;
int		selObjLtX, selObjLtY, selObjRbX, selObjRbY;
int		numObjSelected=0;
int		numObjLocked=0;
struct SelRec	* topSel = NULL, * botSel = NULL;
struct VSelRec	* topVSel = NULL, * botVSel = NULL;

int CountSelectedVertices()
{
   struct VSelRec *vsel_ptr;
   int count = 0;

   for (vsel_ptr=topVSel; vsel_ptr != NULL; vsel_ptr=vsel_ptr->next) {
      int i, n=vsel_ptr->n;

      count += n;
      if (vsel_ptr->obj->type == OBJ_POLYGON) {
         for (i = 0; i < n; i++) {
            if (vsel_ptr->v_index[i] == 0) {
               count--;
               break;
            }
         }
      }
   }
   return count;
}

void CalcBBox (X1, Y1, X2, Y2, LtX, LtY, RbX, RbY)
   int X1, Y1, X2, Y2, * LtX, * LtY, * RbX, * RbY;
{
   if (X1 < X2)
      if (Y1 < Y2)
      {
         *LtX = X1; *LtY = Y1; *RbX = X2; *RbY = Y2;
      }
      else
      {
         *LtX = X1; *LtY = Y2; *RbX = X2; *RbY = Y1;
      }
   else
      if (Y1 < Y2)
      {
         *LtX = X2; *LtY = Y1; *RbX = X1; *RbY = Y2;
      }
      else
      {
         *LtX = X2; *LtY = Y2; *RbX = X1; *RbY = Y1;
      }
}

void CalcVertexBBox (LtX, LtY, RbX, RbY)
   int	* LtX, * LtY, * RbX, * RbY;
{
   register int		i, *x_ptr, *y_ptr;
   struct VSelRec	* vsel_ptr;

   *LtX = selRbX; *LtY = selRbY; *RbX = selLtX; *RbY = selLtY;

   for (vsel_ptr=topVSel; vsel_ptr!=NULL; vsel_ptr=vsel_ptr->next)
      for (i=0, x_ptr=vsel_ptr->x, y_ptr=vsel_ptr->y; i < vsel_ptr->n;
            i++, x_ptr++, y_ptr++)
      {
         if (*x_ptr < *LtX) *LtX = *x_ptr;
         if (*y_ptr < *LtY) *LtY = *y_ptr;
         if (*x_ptr > *RbX) *RbX = *x_ptr;
         if (*y_ptr > *RbY) *RbY = *y_ptr;
      }
}

void UnSelNonVertexObjs (highlight)
   int	highlight;
{
   register struct ObjRec	* obj_ptr;
   register struct SelRec	* sel_ptr, * prev_sel;

   for (sel_ptr = botSel; sel_ptr != NULL; sel_ptr = prev_sel)
   {
      prev_sel = sel_ptr->prev;
      obj_ptr = sel_ptr->obj;

      if ((obj_ptr->type==OBJ_POLY || obj_ptr->type==OBJ_POLYGON) &&
            !obj_ptr->locked)
         continue;

      if (highlight) HighLightAnObj (obj_ptr);

      if (sel_ptr->prev == NULL)
         topSel = sel_ptr->next;
      else
         sel_ptr->prev->next = sel_ptr->next;

      if (sel_ptr->next == NULL)
         botSel = sel_ptr->prev;
      else
         sel_ptr->next->prev = sel_ptr->prev;

      free(sel_ptr);
   }
}

void JustRemoveAllVSel ()
{
   register struct VSelRec	* next_vsel;

   while (topVSel != NULL)
   {
      next_vsel = topVSel->next;
      free(topVSel->v_index);
      free(topVSel->x);
      free(topVSel->y);
      free(topVSel);
      topVSel = next_vsel;
   }
   botVSel = NULL;
}

void RemoveAllSel ()
{
   register struct SelRec	* next_sel;
   register struct VSelRec	* next_vsel;

   while (topSel != NULL)
   {
      next_sel = topSel->next;
      free(topSel);
      topSel = next_sel;
   }
   botSel = NULL;

   while (topVSel != NULL)
   {
      next_vsel = topVSel->next;
      free(topVSel->v_index);
      free(topVSel->x);
      free(topVSel->y);
      free(topVSel);
      topVSel = next_vsel;
   }
   botVSel = NULL;
   numObjSelected = 0;
}

static
struct AttrRec *FindObjAttrWithName(ObjPtr, AttrName)
   struct ObjRec *ObjPtr;
   char *AttrName;
   /* AttrName here must not contain '.' */
{
   register struct AttrRec *attr_ptr;
   struct AttrRec *found_attr=NULL;
   int count=1, compare_name=(strchr(AttrName,'=') != NULL);

   if (ObjPtr == NULL) return NULL;

   for (attr_ptr=ObjPtr->fattr; attr_ptr!=NULL; attr_ptr=attr_ptr->next) {
      if (compare_name) {
         if (strcmp(attr_ptr->attr_name.s, AttrName) == 0) {
            found_attr = attr_ptr;
            break;
         }
      } else {
         if (strcmp(attr_ptr->attr_value.s, AttrName) == 0) {
            found_attr = attr_ptr;
            break;
         }
      }
   }
   if (attr_ptr == NULL) return NULL;

   if (found_attr->obj->color == colorIndex) return found_attr;
   for (attr_ptr=found_attr->next; attr_ptr!=NULL; attr_ptr=attr_ptr->next) {
      if (compare_name) {
         if (strcmp(attr_ptr->attr_name.s, AttrName) == 0) {
            if (attr_ptr->obj->color == colorIndex) {
               break;
            } else {
               count++;
            }
         }
      } else {
         if (strcmp(attr_ptr->attr_value.s, AttrName) == 0) {
            if (attr_ptr->obj->color == colorIndex) {
               break;
            } else {
               count++;
            }
         }
      }
   }
   if (attr_ptr != NULL) {
      found_attr = attr_ptr;
   } else if (count != 1) {
      sprintf(gszMsgBox, "Can not find '%s' attribute with color %s.",
            AttrName, colorMenuItems[colorIndex]);
      Msg(gszMsgBox);
      return NULL;
   }
   return found_attr;
}

static
struct ObjRec * FindAVertex (XOff, YOff, VIndex, AbsX, AbsY)
   int	XOff, YOff, * VIndex, * AbsX, * AbsY;
   /* XOff and YOff are screen offsets */
{
   register struct ObjRec	* obj_ptr;
   struct PolyRec		* poly_ptr;
   struct PolygonRec		* polygon_ptr;
   struct SelRec		* sel_ptr;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      obj_ptr = sel_ptr->obj;

      if (obj_ptr->type != OBJ_POLY && obj_ptr->type != OBJ_POLYGON) continue;
      if (!(XOff >= OFFSET_X(obj_ptr->bbox.ltx)-3 &&
            YOff >= OFFSET_Y(obj_ptr->bbox.lty)-3 &&
            XOff <= OFFSET_X(obj_ptr->bbox.rbx)+3 &&
            YOff <= OFFSET_Y(obj_ptr->bbox.rby)+3)) continue;

      switch (obj_ptr->type)
      {
         case OBJ_POLY:
            poly_ptr = obj_ptr->detail.p;
            if (PtInPolyMark (obj_ptr, XOff, YOff, poly_ptr->n,
                  poly_ptr->vlist, VIndex))
            {
               if (obj_ptr->ctm == NULL) {
                  *AbsX = poly_ptr->vlist[*VIndex].x;
                  *AbsY = poly_ptr->vlist[*VIndex].y;
               } else {
                  int x, y;

                  TransformPointThroughCTM(
                        poly_ptr->vlist[*VIndex].x-obj_ptr->x,
                        poly_ptr->vlist[*VIndex].y-obj_ptr->y,
                        obj_ptr->ctm, &x, &y);
                  *AbsX = x+obj_ptr->x;
                  *AbsY = y+obj_ptr->y;
               }
               return (obj_ptr);
            }
            break;
         case OBJ_POLYGON:
            polygon_ptr = obj_ptr->detail.g;
            if (PtInPolyMark (obj_ptr, XOff, YOff, polygon_ptr->n,
                  polygon_ptr->vlist, VIndex))
            {
               if (obj_ptr->ctm == NULL) {
                  *AbsX = polygon_ptr->vlist[*VIndex].x;
                  *AbsY = polygon_ptr->vlist[*VIndex].y;
               } else {
                  int x, y;

                  TransformPointThroughCTM(
                        polygon_ptr->vlist[*VIndex].x-obj_ptr->x,
                        polygon_ptr->vlist[*VIndex].y-obj_ptr->y,
                        obj_ptr->ctm, &x, &y);
                  *AbsX = x+obj_ptr->x;
                  *AbsY = y+obj_ptr->y;
               }
               return (obj_ptr);
            }
            break;
      }
   }
   return (NULL);
}

struct ObjRec * FindAnObj (XOff, YOff, OwnerObj, ConnectObj, ReturnedObjName)
   int			XOff, YOff;
   struct ObjRec	* * OwnerObj, * * ConnectObj;
   char			* ReturnedObjName;
   /* XOff and YOff are screen offsets */
{
   register struct ObjRec	* obj_ptr;
   register struct AttrRec	* attr_ptr;
   struct ObjRec		*sub_obj, *returned_obj=NULL, *actual_obj=NULL;
   struct ObjRec		*owner_obj=NULL;
   int				found_attr=FALSE;

   if (OwnerObj != NULL) *OwnerObj = NULL;
   if (ConnectObj != NULL) *ConnectObj = NULL;
   if (ReturnedObjName != NULL) *ReturnedObjName = '\0';

   for (obj_ptr=topObj; returned_obj==NULL && obj_ptr!=NULL;
         obj_ptr=obj_ptr->next) {
      obj_ptr->tmp_child = NULL;
      obj_ptr->tmp_parent = NULL;
      if (colorLayers && !ObjInVisibleLayer(obj_ptr)) {
         continue;
      }
      actual_obj = obj_ptr;
      for (attr_ptr=obj_ptr->fattr; returned_obj==NULL && attr_ptr!=NULL;
            attr_ptr=attr_ptr->next) {
         if (attr_ptr->shown &&
               XOff >= OFFSET_X(attr_ptr->obj->bbox.ltx)-3 &&
               YOff >= OFFSET_Y(attr_ptr->obj->bbox.lty)-3 &&
               XOff <= OFFSET_X(attr_ptr->obj->bbox.rbx)+3 &&
               YOff <= OFFSET_Y(attr_ptr->obj->bbox.rby)+3)
         {
            owner_obj = obj_ptr;
            returned_obj = attr_ptr->obj;
            found_attr = TRUE;
            break;
         }
      }
      if (returned_obj != NULL) break;

      if (XOff >= OFFSET_X(obj_ptr->bbox.ltx)-3 &&
            YOff >= OFFSET_Y(obj_ptr->bbox.lty)-3 &&
            XOff <= OFFSET_X(obj_ptr->bbox.rbx)+3 &&
            YOff <= OFFSET_Y(obj_ptr->bbox.rby)+3) {
         struct ObjRec  * next_level_child=NULL;

         switch (obj_ptr->type) {
         case OBJ_TEXT:
            if (FindGoodText (XOff,YOff,obj_ptr)) returned_obj = obj_ptr;
            break;
         case OBJ_XBM:
            if (FindGoodXBm (XOff,YOff,obj_ptr)) returned_obj = obj_ptr;
            break;
         case OBJ_XPM:
            if (FindGoodXPm (XOff,YOff,obj_ptr)) returned_obj = obj_ptr;
            break;
         case OBJ_BOX:
            if (FindGoodBox (XOff,YOff,obj_ptr)) returned_obj = obj_ptr;
            break;
         case OBJ_OVAL:
            if (FindGoodOval (XOff,YOff,obj_ptr)) returned_obj = obj_ptr;
            break;
         case OBJ_POLY:
            if (FindGoodPoly (XOff,YOff,obj_ptr)) returned_obj = obj_ptr;
            break;
         case OBJ_POLYGON:
            if (FindGoodPolygon (XOff,YOff,obj_ptr)) returned_obj = obj_ptr;
            break;
         case OBJ_ARC:
            if (FindGoodArc (XOff,YOff,obj_ptr)) returned_obj = obj_ptr;
            break;
         case OBJ_RCBOX:
            if (FindGoodRCBox (XOff,YOff,obj_ptr)) returned_obj = obj_ptr;
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
            if (FindGoodObj (XOff, YOff, obj_ptr->detail.r->first, &sub_obj,
                  &next_level_child)) {
               obj_ptr->tmp_child = next_level_child;
               if (sub_obj == NULL) {
                  owner_obj = NULL;
                  returned_obj = obj_ptr;
               } else {
                  owner_obj = obj_ptr;
                  returned_obj = sub_obj;
               }
            }
            break;
         }
      }
   }
   if (ReturnedObjName != NULL) {
      char *c_ptr;

      strcpy(ReturnedObjName, "NodeName: ");
      c_ptr = &ReturnedObjName[10];
      if (returned_obj != NULL) {
         struct ObjRec *prev_obj=NULL;

         for (obj_ptr=actual_obj; obj_ptr != NULL; obj_ptr=obj_ptr->tmp_child) {
            if ((attr_ptr=FindObjAttrWithName(obj_ptr, "name=")) != NULL) {
               *c_ptr++ = '!';
               strcpy(c_ptr, attr_ptr->attr_value.s);
               c_ptr = &c_ptr[strlen(c_ptr)];
            } else {
               if (prev_obj != NULL) prev_obj->tmp_child = NULL;
               break;
            }
            prev_obj = obj_ptr;
         }
         if (ConnectObj != NULL) *ConnectObj = prev_obj;
      }
      if (c_ptr == &ReturnedObjName[10]) {
         *ReturnedObjName = '\0';
      } else {
         *c_ptr = '\0';
      }
   }
   if (OwnerObj != NULL) *OwnerObj = owner_obj;
   return returned_obj;
}

static
int VertexAlreadySelected (ObjPtr, VIndex, VSelPtr)
   register struct ObjRec	* ObjPtr;
   int				VIndex;
   register struct VSelRec	* * VSelPtr;
{
   register int	i;

   for (*VSelPtr = topVSel; *VSelPtr != NULL; *VSelPtr = (*VSelPtr)->next)
      if ((*VSelPtr)->obj == ObjPtr)
      {
         for (i = 0; i < (*VSelPtr)->n; i++)
            if ((*VSelPtr)->v_index[i] == VIndex)
               return (TRUE);
         return (FALSE);
      }
   return (FALSE);
}

static
struct SelRec * AlreadySelected (ObjPtr)
   register struct ObjRec	* ObjPtr;
{
   register struct SelRec	* sel_ptr;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
      if (sel_ptr->obj == ObjPtr)
         return (sel_ptr);
   return (NULL);
}

void AddSel (PrevPtr, NextPtr, SelPtr)
   struct SelRec	* PrevPtr, * NextPtr, * SelPtr;
   /* add SelPtr between PrevPtr and NextPtr */
{
   SelPtr->prev = PrevPtr;
   SelPtr->next = NextPtr;

   if (PrevPtr == NULL)
      topSel = SelPtr;
   else
      PrevPtr->next = SelPtr;

   if (NextPtr == NULL)
      botSel = SelPtr;
   else
      NextPtr->prev = SelPtr;
   numObjSelected++;
}

void AddNewSelObj (ObjPtr)
   register struct ObjRec	* ObjPtr;
{
   register struct ObjRec	* obj_ptr = topObj;
   register struct SelRec	* sel_ptr = topSel, * new_sel_ptr;

   new_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (new_sel_ptr == NULL) FailAllocMessage();
   new_sel_ptr->obj = ObjPtr;

   for ( ; sel_ptr != NULL && obj_ptr != ObjPtr; obj_ptr = obj_ptr->next) {
      if (obj_ptr == sel_ptr->obj) {
         sel_ptr = sel_ptr->next;
      }
   }
   if (sel_ptr == NULL)
   {  /* the object is below the last selected object */
      if (botSel == NULL)
         topSel = new_sel_ptr;
      else
         botSel->next = new_sel_ptr;

      new_sel_ptr->prev = botSel;
      new_sel_ptr->next = NULL;
      botSel = new_sel_ptr;
   }
   else
   {  /* the object is between sel_ptr and sel_ptr->prev */
      if (sel_ptr->prev == NULL)
         topSel = new_sel_ptr;
      else
         sel_ptr->prev->next = new_sel_ptr;

      new_sel_ptr->next = sel_ptr;
      new_sel_ptr->prev = sel_ptr->prev;
      sel_ptr->prev = new_sel_ptr;
   }
   numObjSelected++;
}

void UpdSelBBox ()
   /* update selLtX, selLtY, selRbX, selRbY */
{
   register struct ObjRec	* obj_ptr;
   register struct SelRec	* sel_ptr;

   numObjSelected = 0;
   numObjLocked = 0;
   if ((sel_ptr = topSel) == NULL) return;

   numObjSelected++;
   obj_ptr = sel_ptr->obj;
   if (obj_ptr->locked) numObjLocked++;
   selLtX = obj_ptr->bbox.ltx;
   selLtY = obj_ptr->bbox.lty;
   selRbX = obj_ptr->bbox.rbx;
   selRbY = obj_ptr->bbox.rby;
   selObjLtX = obj_ptr->obbox.ltx;
   selObjLtY = obj_ptr->obbox.lty;
   selObjRbX = obj_ptr->obbox.rbx;
   selObjRbY = obj_ptr->obbox.rby;

   for (sel_ptr = topSel->next; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      numObjSelected++;
      obj_ptr = sel_ptr->obj;
      if (obj_ptr->locked) numObjLocked++;
      if (obj_ptr->bbox.ltx < selLtX) selLtX = obj_ptr->bbox.ltx;
      if (obj_ptr->bbox.lty < selLtY) selLtY = obj_ptr->bbox.lty;
      if (obj_ptr->bbox.rbx > selRbX) selRbX = obj_ptr->bbox.rbx;
      if (obj_ptr->bbox.rby > selRbY) selRbY = obj_ptr->bbox.rby;
      if (obj_ptr->obbox.ltx < selObjLtX) selObjLtX = obj_ptr->obbox.ltx;
      if (obj_ptr->obbox.lty < selObjLtY) selObjLtY = obj_ptr->obbox.lty;
      if (obj_ptr->obbox.rbx > selObjRbX) selObjRbX = obj_ptr->obbox.rbx;
      if (obj_ptr->obbox.rby > selObjRbY) selObjRbY = obj_ptr->obbox.rby;
   }
}
 
static
struct VSelRec * SelectOneVertex (XOff, YOff)
   int	XOff, YOff;
   /* XOff and YOff are screen offsets */
{
   register struct ObjRec	* obj_ptr;
   int				v_index, x, y;

   JustRemoveAllVSel ();
   if ((obj_ptr = FindAVertex (XOff, YOff, &v_index, &x, &y)) == NULL)
      return (NULL);

   topVSel = (struct VSelRec *)malloc(sizeof(struct VSelRec));
   if (topVSel == NULL) FailAllocMessage();
   topVSel->obj = obj_ptr;
   topVSel->max_v = 10;
   topVSel->v_index = (int*)malloc(10*sizeof(int));
   if (topVSel->v_index == NULL) FailAllocMessage();
   topVSel->x = (int*)malloc(10*sizeof(int));
   topVSel->y = (int*)malloc(10*sizeof(int));
   if (topVSel->x == NULL || topVSel->y == NULL) FailAllocMessage();
   topVSel->v_index[0] = v_index;
   topVSel->x[0] = x;
   topVSel->y[0] = y;
   if (obj_ptr->type==OBJ_POLYGON && v_index==0)
   {
      topVSel->n = 2;
      topVSel->v_index[1] = obj_ptr->detail.g->n-1;
      topVSel->x[1] = x;
      topVSel->y[1] = y;
   }
   else
      topVSel->n = 1;
   topVSel->next = NULL;
   topVSel->prev = NULL;
   botVSel = topVSel;
   UpdSelBBox ();

   return (topVSel);
}

static
struct SelRec * SelectOneObj (XOff, YOff)
   int	XOff, YOff;
   /* XOff and YOff are screen offsets */
{
   register struct ObjRec	* obj_ptr;
   struct ObjRec		* owner_obj;

   RemoveAllSel ();
   if ((obj_ptr = FindAnObj (XOff,YOff,&owner_obj,NULL,NULL)) == NULL) {
      return (NULL);
   }
   if (owner_obj != NULL) obj_ptr = owner_obj;

   topSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (topSel == NULL) FailAllocMessage();
   topSel->next = NULL;
   topSel->obj = obj_ptr;
   topSel->prev = NULL;
   botSel = topSel;
   UpdSelBBox ();

   return (topSel);
}

static
int FindVertices (X1, Y1, X2, Y2, TopVSel, BotVSel)
   int			X1, Y1, X2, Y2;
   struct VSelRec	* * TopVSel, * * BotVSel;
   /* X1, Y1, X2, Y2 are absolute coordinates */
{
   register struct ObjRec	* obj_ptr;
   register struct VSelRec	* vsel_ptr;
   register int			i;
   struct SelRec		* sel_ptr;
   struct BBRec			bbox;
   IntPoint			* v;
   int				n, count, max_count, j;

   *TopVSel = *BotVSel = NULL;

   bbox.ltx = X1; bbox.lty = Y1;
   bbox.rbx = X2; bbox.rby = Y2;
   for (sel_ptr = botSel; sel_ptr != NULL; sel_ptr = sel_ptr->prev)
   {
      obj_ptr = sel_ptr->obj;

      if (obj_ptr->type != OBJ_POLY && obj_ptr->type != OBJ_POLYGON) continue;
      if (!BBoxIntersect (bbox, obj_ptr->bbox)) continue;

      v = (obj_ptr->type==OBJ_POLY) ? obj_ptr->detail.p->vlist :
            obj_ptr->detail.g->vlist;
      n = (obj_ptr->type==OBJ_POLY) ? obj_ptr->detail.p->n :
            obj_ptr->detail.g->n;
      if (obj_ptr->ctm == NULL) {
         for (i = 0, count = 0; i < n; i++) {
            if (v[i].x >= X1 && v[i].x <= X2 && v[i].y >= Y1 && v[i].y <= Y2) {
               count++;
            }
         }
      } else {
         for (i = 0, count = 0; i < n; i++) {
            int x, y;

            TransformPointThroughCTM(v[i].x-obj_ptr->x, v[i].y-obj_ptr->y,
                  obj_ptr->ctm, &x, &y);
            if (x+obj_ptr->x >= X1 && x+obj_ptr->x <= X2 &&
                  y+obj_ptr->y >= Y1 && y+obj_ptr->y <= Y2) {
               count++;
            }
         }
      }

      if (count != 0)
      {
         vsel_ptr = (struct VSelRec *)malloc(sizeof(struct VSelRec));
         if (vsel_ptr == NULL) FailAllocMessage();
         vsel_ptr->obj = obj_ptr;
         vsel_ptr->next = *TopVSel;
         vsel_ptr->prev = NULL;
         if (*TopVSel == NULL)
            *BotVSel = vsel_ptr;
         else
            (*TopVSel)->prev = vsel_ptr;
         *TopVSel = vsel_ptr;
         vsel_ptr->n = count;
         max_count = ((count%10) == 0) ? 10*((int)(count/10)) :
               10*((int)(count/10)+1);
         vsel_ptr->max_v = max_count;
         vsel_ptr->v_index = (int*)malloc(max_count*sizeof(int));
         if (vsel_ptr->v_index == NULL) FailAllocMessage();
         vsel_ptr->x = (int*)malloc(max_count*sizeof(int));
         vsel_ptr->y = (int*)malloc(max_count*sizeof(int));
         if (vsel_ptr->x == NULL || vsel_ptr->y == NULL) FailAllocMessage();

         if (obj_ptr->ctm == NULL) {
            for (i = 0, j = 0; i < n; i++) {
               if (v[i].x >= X1 && v[i].x <= X2 &&
                     v[i].y >= Y1 && v[i].y <= Y2) {
                  vsel_ptr->v_index[j] = i;
                  vsel_ptr->x[j] = v[i].x;
                  vsel_ptr->y[j] = v[i].y;
                  j++;
               }
            }
         } else {
            for (i = 0, j = 0; i < n; i++) {
               int x, y;

               TransformPointThroughCTM(v[i].x-obj_ptr->x, v[i].y-obj_ptr->y,
                     obj_ptr->ctm, &x, &y);
               if (x+obj_ptr->x >= X1 && x+obj_ptr->x <= X2 &&
                     y+obj_ptr->y >= Y1 && y+obj_ptr->y <= Y2) {
                  vsel_ptr->v_index[j] = i;
                  vsel_ptr->x[j] = x+obj_ptr->x;
                  vsel_ptr->y[j] = y+obj_ptr->y;
                  j++;
               }
            }
         }
      }
   }
   return (*TopVSel != NULL);
}
 
static
struct SelRec * FindObjects (X1, Y1, X2, Y2, TopSel, BotSel)
   int			X1, Y1, X2, Y2;
   struct SelRec	* * TopSel, * * BotSel;
   /* X1, Y1, X2, Y2 are absolute coordinates */
{
   register struct ObjRec	* obj_ptr;
   register struct SelRec	* sel_ptr;

   *TopSel = *BotSel = NULL;

   for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
      obj_ptr->tmp_parent = NULL;
      if (colorLayers && !ObjInVisibleLayer(obj_ptr)) {
         continue;
      }
      if (X1 <= obj_ptr->bbox.ltx && X2 >= obj_ptr->bbox.rbx &&
            Y1 <= obj_ptr->bbox.lty && Y2 >= obj_ptr->bbox.rby) {
         sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
         if (sel_ptr == NULL) FailAllocMessage();
         sel_ptr->next = *TopSel;
         sel_ptr->obj = obj_ptr;
         sel_ptr->prev = NULL;
         if (*TopSel == NULL) {
            *BotSel = sel_ptr;
         } else {
            (*TopSel)->prev = sel_ptr;
         }
         *TopSel = sel_ptr;
      }
   }

   return (*TopSel);
}
 
void SelBox (window, gc, x1, y1, x2, y2)
   Window	window;
   GC		gc;
   int		x1, y1, x2, y2;
{
   XPoint	sv[5];

   if (x1 == x2 || y1 == y2)
      XDrawLine (mainDisplay, window, gc, x1, y1, x2, y2);
   else
   {
      sv[0].x = (short)x1; sv[0].y = (short)y1;
      sv[1].x = (short)x1; sv[1].y = (short)y2;
      sv[2].x = (short)x2; sv[2].y = (short)y2;
      sv[3].x = (short)x2; sv[3].y = (short)y1;
      sv[4].x = (short)x1; sv[4].y = (short)y1;
      XDrawLines (mainDisplay, window, gc, sv, 5, CoordModeOrigin);
   }
}

static
struct ObjRec * PtInObjList (XOff, YOff, FirstObjPtr)
   int			XOff, YOff;
   struct ObjRec	* FirstObjPtr;
   /* XOff and YOff are screen offsets */
{
   register struct ObjRec	* obj_ptr, * obj_ptr1;
   register struct AttrRec	* attr_ptr;

   for (obj_ptr = FirstObjPtr; obj_ptr != NULL; obj_ptr = obj_ptr->next)
   {
      for (attr_ptr=obj_ptr->fattr; attr_ptr!=NULL; attr_ptr=attr_ptr->next)
         if (attr_ptr->shown &&
               XOff >= OFFSET_X(attr_ptr->obj->bbox.ltx)-3 &&
               YOff >= OFFSET_Y(attr_ptr->obj->bbox.lty)-3 &&
               XOff <= OFFSET_X(attr_ptr->obj->bbox.rbx)+3 &&
               YOff <= OFFSET_Y(attr_ptr->obj->bbox.rby)+3)
            return (attr_ptr->obj);

      if (XOff >= OFFSET_X(obj_ptr->bbox.ltx)-3 &&
            YOff >= OFFSET_Y(obj_ptr->bbox.lty)-3 &&
            XOff <= OFFSET_X(obj_ptr->bbox.rbx)+3 &&
            YOff <= OFFSET_Y(obj_ptr->bbox.rby)+3)
      {
         switch (obj_ptr->type)
         {
            case OBJ_TEXT:
               if (FindGoodText (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_XBM:
               if (FindGoodXBm (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_XPM:
               if (FindGoodXPm (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_BOX:
               if (FindGoodBox (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_OVAL:
               if (FindGoodOval (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_POLY:
               if (FindGoodPoly (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_POLYGON:
               if (FindGoodPolygon (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_ARC:
               if (FindGoodArc (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_RCBOX:
               if (FindGoodRCBox (XOff,YOff,obj_ptr)) return (obj_ptr); break;

            case OBJ_GROUP:
            case OBJ_SYM:
            case OBJ_ICON:
               obj_ptr1 = PtInObjList (XOff,YOff,obj_ptr->detail.r->first);
               if (obj_ptr1 != NULL) return (obj_ptr1);
               break;
         }
      }
   }
   return (NULL);
}

static
struct ObjRec * PtInSelected (XOff, YOff)
   int	XOff, YOff;
   /* XOff and YOff are screen offsets */
{
   register struct SelRec	* sel_ptr;
   register struct ObjRec	* obj_ptr, * obj_ptr1;
   register struct AttrRec	* attr_ptr;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      obj_ptr = sel_ptr->obj;

      for (attr_ptr=obj_ptr->fattr; attr_ptr!=NULL; attr_ptr=attr_ptr->next)
         if (attr_ptr->shown &&
               XOff >= OFFSET_X(attr_ptr->obj->bbox.ltx)-3 &&
               YOff >= OFFSET_Y(attr_ptr->obj->bbox.lty)-3 &&
               XOff <= OFFSET_X(attr_ptr->obj->bbox.rbx)+3 &&
               YOff <= OFFSET_Y(attr_ptr->obj->bbox.rby)+3)
            return (attr_ptr->obj);

      if (XOff >= OFFSET_X(obj_ptr->bbox.ltx)-3 &&
            YOff >= OFFSET_Y(obj_ptr->bbox.lty)-3 &&
            XOff <= OFFSET_X(obj_ptr->bbox.rbx)+3 &&
            YOff <= OFFSET_Y(obj_ptr->bbox.rby)+3)
      {
         switch (obj_ptr->type)
         {
            case OBJ_TEXT:
               if (FindGoodText (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_XBM:
               if (FindGoodXBm (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_XPM:
               if (FindGoodXPm (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_BOX:
               if (FindGoodBox (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_OVAL:
               if (FindGoodOval (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_POLY:
               if (FindGoodPoly (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_POLYGON:
               if (FindGoodPolygon (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_ARC:
               if (FindGoodArc (XOff,YOff,obj_ptr)) return (obj_ptr); break;
            case OBJ_RCBOX:
               if (FindGoodRCBox (XOff,YOff,obj_ptr)) return (obj_ptr); break;

            case OBJ_GROUP:
            case OBJ_SYM:
            case OBJ_ICON:
               obj_ptr1 = PtInObjList (XOff,YOff,obj_ptr->detail.r->first);
               if (obj_ptr1 != NULL) return (obj_ptr1);
               break;
         }
      }
   }
   return (NULL);
}

static
void ToggleVertexSelection (ObjPtr, VIndex, AbsX, AbsY)
   struct ObjRec	* ObjPtr;
   int			VIndex, AbsX, AbsY;
{
   int			i, j, n;
   struct VSelRec	* vsel_ptr;

   if (!(ObjPtr->type==OBJ_POLYGON && ObjPtr->detail.g->n-1==VIndex))
   {
      char	* smooth=NULL;
      int	curved=(-1);

      switch (ObjPtr->type)
      {
         case OBJ_POLY:
            smooth = ObjPtr->detail.p->smooth;
            curved = ObjPtr->detail.p->curved;
            break;
         case OBJ_POLYGON:
            smooth = ObjPtr->detail.g->smooth;
            curved = ObjPtr->detail.g->curved;
            break;
      }
      if (curved != LT_INTSPLINE && curved != (-1) && smooth != NULL)
      {
         if (smooth[VIndex])
            MARKO(drawWindow, revDefaultGC, OFFSET_X(AbsX), OFFSET_Y(AbsY));
         else
            MARK(drawWindow, revDefaultGC, OFFSET_X(AbsX), OFFSET_Y(AbsY));
      }
      else
         MARK(drawWindow, revDefaultGC, OFFSET_X(AbsX), OFFSET_Y(AbsY));
      MARKV(drawWindow, revDefaultGC, OFFSET_X(AbsX), OFFSET_Y(AbsY));
   }
   if (VertexAlreadySelected (ObjPtr, VIndex, &vsel_ptr))
   {  /* de-select a vertex */
      if (vsel_ptr->n == 1)
      {
         if (vsel_ptr->prev == NULL)
            topVSel = vsel_ptr->next;
         else
            vsel_ptr->prev->next = vsel_ptr->next;

         if (vsel_ptr->next == NULL)
            botVSel = vsel_ptr->prev;
         else
            vsel_ptr->next->prev = vsel_ptr->prev;

         free(vsel_ptr->v_index);
         free(vsel_ptr->x);
         free(vsel_ptr->y);
         free(vsel_ptr);
      }
      else
      {
         for (j = 0; j < vsel_ptr->n; j++)
            if (vsel_ptr->v_index[j] == VIndex)
               break;
         if (j > vsel_ptr->n)
            fprintf (stderr, "%s.  Please send bug report.\n",
                  "Inconsistent vertex selection");
         for (i = j; i < vsel_ptr->n-1; i++)
         {
            vsel_ptr->v_index[i] = vsel_ptr->v_index[i+1];
            vsel_ptr->x[i] = vsel_ptr->x[i+1];
            vsel_ptr->y[i] = vsel_ptr->y[i+1];
         }
         vsel_ptr->n--;
      }
   }
   else
   {
      if (vsel_ptr == NULL)
      {
         vsel_ptr = (struct VSelRec *)malloc(sizeof(struct VSelRec));
         if (vsel_ptr == NULL) FailAllocMessage();
         vsel_ptr->obj = ObjPtr;
         n = vsel_ptr->n = 1;
         vsel_ptr->max_v = 10;
         vsel_ptr->v_index = (int*)malloc(10*sizeof(int));
         if (vsel_ptr->v_index == NULL) FailAllocMessage();
         vsel_ptr->x = (int*)malloc(10*sizeof(int));
         vsel_ptr->y = (int*)malloc(10*sizeof(int));
         if (vsel_ptr->x == NULL || vsel_ptr->y == NULL) FailAllocMessage();

         vsel_ptr->prev = NULL;
         vsel_ptr->next = topVSel;
         if (topVSel == NULL) {
            botVSel = vsel_ptr;
         } else {
            topVSel->prev = vsel_ptr;
         }
         topVSel = vsel_ptr;
      }
      else
      {
         n = ++(vsel_ptr->n);
         if (n > vsel_ptr->max_v)
         {
            int	max_v;

            vsel_ptr->max_v += 10;
            max_v = vsel_ptr->max_v;
            vsel_ptr->v_index = (int *) realloc (vsel_ptr->v_index,
                  sizeof(int)*max_v);
            vsel_ptr->x = (int *) realloc (vsel_ptr->x, sizeof(int)*max_v);
            vsel_ptr->y = (int *) realloc (vsel_ptr->y, sizeof(int)*max_v);
         }
      }
      vsel_ptr->v_index[n-1] = VIndex;
      vsel_ptr->x[n-1] = AbsX;
      vsel_ptr->y[n-1] = AbsY;
   }
}

static
void ToggleSelectedObjIfSelectedAlready (ObjPtr)
   register struct ObjRec	* ObjPtr;
{
   register struct SelRec	* sel_ptr;

   if ((sel_ptr = AlreadySelected (ObjPtr)) != NULL)
   {  /* de-select an object */
      HighLightAnObj (ObjPtr);

      if (sel_ptr->prev == NULL)
         topSel = sel_ptr->next;
      else
         sel_ptr->prev->next = sel_ptr->next;

      if (sel_ptr->next == NULL)
         botSel = sel_ptr->prev;
      else
         sel_ptr->next->prev = sel_ptr->prev;

      free(sel_ptr);
      numObjSelected--;
   }
   else
   {  /* add a newly selected object */
      AddNewSelObj (ObjPtr);
      HighLightAnObj (ObjPtr);
   }
}

static
void ContinueSel (XOff, YOff, ShiftKeyDown)
   int 	XOff, YOff, ShiftKeyDown;
   /* XOff and YOff are screen offsets, and they are not on grid */
{
   register int		i;
   int 			end_x, end_y, v_index;
   int 			done = FALSE, ltx, lty, rbx, rby, dx, dy, x, y;
   int 			new_end_x, new_end_y;
   char 		buf[80], w_buf[80], h_buf[80], x_buf[80], y_buf[80];
   XEvent		input, ev;
   XMotionEvent		* motion_ev;
   struct SelRec	* sel_ptr, * top_sel_ptr, * bot_sel_ptr;
   struct VSelRec	* vsel_ptr, * top_vsel_ptr, * bot_vsel_ptr;
   struct ObjRec	* obj_ptr, * owner_obj;

   end_x = XOff;
   end_y = YOff; 

   SelBox (drawWindow, revDefaultGC, XOff, YOff, end_x, end_y);
   PixelToMeasurementUnit(w_buf, 0);
   PixelToMeasurementUnit(h_buf, 0);
   PixelToMeasurementUnit(x_buf, ABS_X(end_x));
   PixelToMeasurementUnit(y_buf, ABS_Y(end_y));
   sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
   StartShowMeasureCursor (end_x, end_y, buf, TRUE);
   XGrabPointer (mainDisplay, drawWindow, False,
         PointerMotionMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   
   while (!done)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == ButtonRelease)
      {
         XUngrabPointer (mainDisplay, CurrentTime);
         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(end_x-XOff)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(end_y-YOff)));
         PixelToMeasurementUnit(x_buf, ABS_X(end_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(end_y));
         sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         EndShowMeasureCursor (end_x, end_y, buf, TRUE);
         SelBox (drawWindow, revDefaultGC, XOff, YOff, end_x, end_y);
         done = TRUE;
      }
      else if (input.type == MotionNotify)
      {
         motion_ev = &(input.xmotion);
         new_end_x = motion_ev->x;
         new_end_y = motion_ev->y;

         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(end_x-XOff)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(end_y-YOff)));
         PixelToMeasurementUnit(x_buf, ABS_X(end_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(end_y));
         sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         ShowMeasureCursor (end_x, end_y, buf, TRUE);
         SelBox (drawWindow, revDefaultGC, XOff, YOff, end_x, end_y);
         end_x = new_end_x; end_y = new_end_y;
         SelBox (drawWindow, revDefaultGC, XOff, YOff, end_x, end_y);
         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(end_x-XOff)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(end_y-YOff)));
         PixelToMeasurementUnit(x_buf, ABS_X(end_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(end_y));
         sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         ShowMeasureCursor (end_x, end_y, buf, TRUE);

         MarkRulers (end_x, end_y);
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
   }

   dx = abs (XOff - end_x);
   dy = abs (YOff - end_y);
   if (curChoice == VERTEXMODE)
   {
      if (dx <= 2 && dy <= 2)
      {
         if (topSel == NULL)
         {
            if (SelectOneObj (XOff, YOff) != NULL)
            {
               if ((topSel->obj->type == OBJ_POLY ||
                     topSel->obj->type == OBJ_POLYGON) && !topSel->obj->locked)
                  HighLightForward ();
               else
                  RemoveAllSel ();
            }
         }
         else if (ShiftKeyDown)
         {
            obj_ptr = FindAVertex (XOff, YOff, &v_index, &x, &y);
            if (obj_ptr != NULL)
            {
               ToggleVertexSelection (obj_ptr, v_index, x, y);
               if (obj_ptr->type==OBJ_POLYGON && v_index==0)
                  ToggleVertexSelection (obj_ptr,obj_ptr->detail.g->n-1,x,y);
               UpdSelBBox ();
            }
         }
         else
         {
            HighLightReverse ();
            SelectOneVertex (XOff, YOff);
            HighLightForward ();
         }
      }
      else
      {
         CalcBBox (XOff, YOff, end_x, end_y, &ltx, &lty, &rbx, &rby);
         if (topSel == NULL)
         {
            if (FindObjects (ABS_X(ltx), ABS_Y(lty), ABS_X(rbx), ABS_Y(rby),
                  &top_sel_ptr, &bot_sel_ptr) != NULL)
            {
               topSel = top_sel_ptr;
               botSel = bot_sel_ptr;
               UnSelNonVertexObjs (FALSE); /* do not highlight */
               UpdSelBBox ();
               HighLightForward ();
            }
         }
         else if (ShiftKeyDown)
         {
            if (FindVertices (ABS_X(ltx), ABS_Y(lty), ABS_X(rbx), ABS_Y(rby),
                  &top_vsel_ptr, &bot_vsel_ptr))
            {
               struct VSelRec	* next_vsel;

               for (vsel_ptr=top_vsel_ptr; vsel_ptr!=NULL; vsel_ptr=next_vsel)
               {
                  obj_ptr = vsel_ptr->obj;
                  for (i = 0; i < vsel_ptr->n; i++)
                     ToggleVertexSelection (obj_ptr, vsel_ptr->v_index[i],
                           vsel_ptr->x[i], vsel_ptr->y[i]);
                  next_vsel = vsel_ptr->next;
                  free(vsel_ptr->v_index);
                  free(vsel_ptr->x);
                  free(vsel_ptr->y);
                  free(vsel_ptr);
               }
               UpdSelBBox ();
            }
         }
         else
         {
            HighLightReverse ();
            JustRemoveAllVSel ();
            if (FindVertices (ABS_X(ltx), ABS_Y(lty), ABS_X(rbx), ABS_Y(rby),
                  &top_vsel_ptr, &bot_vsel_ptr))
            {
               topVSel = top_vsel_ptr;
               botVSel = bot_vsel_ptr;
               UpdSelBBox ();
            }
            HighLightForward ();
         }
      }
   }
   else
   {
      if (dx <= 2 && dy <= 2)
      {
         if (ShiftKeyDown)
         {
            obj_ptr = FindAnObj (XOff, YOff, &owner_obj, NULL, NULL);
            if (obj_ptr != NULL)
            {
               if (owner_obj != NULL) obj_ptr = owner_obj;

               ToggleSelectedObjIfSelectedAlready (obj_ptr);
               UpdSelBBox ();
            }
         }
         else
         {
            if (topSel != NULL) HighLightReverse ();

            if (SelectOneObj (XOff, YOff) != NULL)
               HighLightForward ();
         }
      }
      else
      {
         CalcBBox (XOff, YOff, end_x, end_y, &ltx, &lty, &rbx, &rby);
         if (ShiftKeyDown)
         {
            if (FindObjects (ABS_X(ltx), ABS_Y(lty), ABS_X(rbx), ABS_Y(rby),
                  &top_sel_ptr, &bot_sel_ptr) != NULL)
            {
               struct SelRec	* next_sel;

               for (sel_ptr=top_sel_ptr; sel_ptr!=NULL; sel_ptr=next_sel)
               {
                  next_sel = sel_ptr->next;
                  obj_ptr = sel_ptr->obj;
                  ToggleSelectedObjIfSelectedAlready (obj_ptr);
                  free(sel_ptr);
               }
               UpdSelBBox ();
            }
         }
         else
         {
            if (topSel != NULL) HighLightReverse ();
            RemoveAllSel ();
            if (FindObjects (ABS_X(ltx), ABS_Y(lty), ABS_X(rbx), ABS_Y(rby),
                  &top_sel_ptr, &bot_sel_ptr) != NULL)
            {
               topSel = top_sel_ptr;
               botSel = bot_sel_ptr;
               UpdSelBBox ();
               HighLightForward ();
            }
         }
      }
   }
}

static Time	selectLastClickTime;
static int	selectJustClicked = FALSE;

static XComposeStatus	c_stat;

void Select (input)
   XEvent	* input;
{
   register int		i;
   XButtonEvent		* button_ev;
   struct SelRec	* sel_ptr;
   struct VSelRec	* vsel_ptr;
   struct ObjRec	* obj_ptr;
   char			s[80];
   KeySym		key_sym;
   Time			click_time;

   if (input->type == KeyPress)
   {
      int	delta, dx=0, dy=0;

      XLookupString (&(input->xkey), s, 80-1, &key_sym, &c_stat);
      if (topSel==NULL && topVSel==NULL)
      {
         XKeyEvent	* key_ev = &(input->xkey);

         if (key_ev->state & ControlMask)
         {
            XButtonEvent	button_ev;

            button_ev.state = ShiftMask;
            switch (key_sym)
            {
               case XK_Left: ScrollLeft (&button_ev); break;
               case XK_KP_Left: ScrollLeft (&button_ev); break;
               case XK_Up: ScrollUp (&button_ev); break;
               case XK_KP_Up: ScrollUp (&button_ev); break;
               case XK_Right: ScrollRight (&button_ev); break;
               case XK_KP_Right: ScrollRight (&button_ev); break;
               case XK_Down: ScrollDown (&button_ev); break;
               case XK_KP_Down: ScrollDown (&button_ev); break;
            }
         }
         else
         {
            switch (key_sym)
            {
               case XK_Left: ScrollLeft (NULL); break;
               case XK_KP_Left: ScrollLeft (NULL); break;
               case XK_Up: ScrollUp (NULL); break;
               case XK_KP_Up: ScrollUp (NULL); break;
               case XK_Right: ScrollRight (NULL); break;
               case XK_KP_Right: ScrollRight (NULL); break;
               case XK_Down: ScrollDown (NULL); break;
               case XK_KP_Down: ScrollDown (NULL); break;
            }
         }
         return;
      }
      if (key_sym!=XK_Left && key_sym!=XK_Up && key_sym!=XK_Right &&
            key_sym!=XK_Down && key_sym!=XK_KP_Left && key_sym!=XK_KP_Up &&
            key_sym!=XK_KP_Right && key_sym!=XK_KP_Down)
         return;

      if (gridOn)
         delta = (gridSystem==ENGLISH_GRID) ? GRID_ABS_SIZE(xyEnglishGrid) :
               GRID_ABS_SIZE(xyMetricGrid);
      else
         delta = GRID_ABS_SIZE(1);
      HighLightReverse ();
      switch (key_sym)
      {
         case XK_Left:     dx = -delta; dy = 0; break;
         case XK_KP_Left:  dx = -delta; dy = 0; break;
         case XK_Up:       dx = 0;      dy = -delta; break;
         case XK_KP_Up:    dx = 0;      dy = -delta; break;
         case XK_Right:    dx = delta;  dy = 0; break;
         case XK_KP_Right: dx = delta;  dy = 0; break;
         case XK_Down:     dx = 0;      dy = delta; break;
         case XK_KP_Down:  dx = 0;      dy = delta; break;
      }
      if (curChoice == VERTEXMODE)
         MoveAllSelVs (dx, dy);
      else if (numObjSelected == numObjLocked)
      {
         HighLightForward ();
         return;
      }
      else
         MoveAllSel (dx, dy);
      HighLightForward ();
      UpdSelBBox ();
      if (justDupped)
      {
         dupDx += dx;
         dupDy += dy;
      }
      SetFileModified (TRUE);
      return;
   }
   else if (input->type != ButtonPress)
      return;

   button_ev = &(input->xbutton);
   if (button_ev->button == Button1)
   {
      int	mouse_x, mouse_y, grid_x, grid_y, corner;

      mouse_x = button_ev->x;
      mouse_y = button_ev->y;
      GridXY (mouse_x, mouse_y, &grid_x, &grid_y);

      click_time = button_ev->time;
      if (curChoice==VERTEXMODE && topSel!=NULL && selectJustClicked &&
            (click_time-selectLastClickTime) < doubleClickInterval)
      {
         selectJustClicked = FALSE;
         HighLightReverse ();
         RemoveAllSel ();
         return;
      }
      selectJustClicked = TRUE;
      selectLastClickTime = click_time;

      if (button_ev->state & (ShiftMask | ControlMask))
      {
         ContinueSel (mouse_x, mouse_y, TRUE);
         justDupped = FALSE;
         return;
      }
      else if (curChoice == VERTEXMODE && topVSel != NULL)
      {
         int	found = FALSE;

         for (vsel_ptr=topVSel; vsel_ptr!=NULL && !found;
               vsel_ptr=vsel_ptr->next)
            for (i = 0; i < vsel_ptr->n; i++)
               if (PtInMark (mouse_x, mouse_y, OFFSET_X(vsel_ptr->x[i]),
                     OFFSET_Y(vsel_ptr->y[i])))
               {
                  found = TRUE;
                  break;
               }
         if (found)
         {
            MoveSelVs (grid_x, grid_y);
            return;
         }
      }
      else if (curChoice == NOTHING)
      {
         if (topSel != NULL)
         {
            if (oneMotionSelectMove &&
                  PtInSelMark (mouse_x, mouse_y, &corner) == NULL &&
                  PtInSelected (mouse_x, mouse_y) == NULL)
            {  /* may be pointing in not already selected object */
               HighLightReverse ();
               RemoveAllSel ();
               if (SelectOneObj (mouse_x, mouse_y) != NULL)
               {
                  HighLightForward ();
                  MoveSel (grid_x, grid_y, topSel->obj, button_ev);
                  return;
               }
            }
            else if ((sel_ptr=PtInSelMark(mouse_x,mouse_y,&corner)) != NULL)
            {
               StretchSel (grid_x, grid_y, sel_ptr->obj, corner);
               return;
            }
            else if ((obj_ptr = PtInSelected (mouse_x, mouse_y)) != NULL)
            {
               MoveSel (grid_x, grid_y, obj_ptr, button_ev);
               return;
            }
         }
         else if (oneMotionSelectMove && SelectOneObj(mouse_x,mouse_y) != NULL)
         {
            HighLightForward ();
            MoveSel (grid_x, grid_y, topSel->obj, button_ev);
            return;
         }
      }
      else if (curChoice == ROTATEMODE && topSel != NULL)
      {
         if ((sel_ptr = PtInSelMark (mouse_x, mouse_y, &corner)) != NULL)
         {
            RotateShearSel (grid_x, grid_y, sel_ptr->obj, corner);
            return;
         }
      }
      ContinueSel (mouse_x, mouse_y, FALSE);
      justDupped = FALSE;
   }
}

struct AttrRec *FindFileAttrWithName(AttrName)
   char *AttrName;
{
   struct AttrRec *attr_ptr, *found_attr=NULL;
   int count=1, compare_name=(strchr(AttrName,'=') != NULL);

   if (tgifObj == NULL) return NULL;

   for (attr_ptr=tgifObj->fattr; attr_ptr != NULL; attr_ptr=attr_ptr->next) {
      if (compare_name) {
         if (strcmp(attr_ptr->attr_name.s, AttrName) == 0) {
            found_attr = attr_ptr;
            break;
         }
      } else {
         if (strcmp(attr_ptr->attr_value.s, AttrName) == 0) {
            found_attr = attr_ptr;
            break;
         }
      }
   }
   if (attr_ptr == NULL) return NULL;

   if (found_attr->obj->color == colorIndex) return found_attr;
   for (attr_ptr=found_attr->next; attr_ptr!=NULL; attr_ptr=attr_ptr->next) {
      if (compare_name) {
         if (strcmp (attr_ptr->attr_name.s, AttrName) == 0) {
            if (attr_ptr->obj->color == colorIndex) {
               break;
            } else {
               count++;
            }
         }
      } else {
         if (strcmp (attr_ptr->attr_value.s, AttrName) == 0) {
            if (attr_ptr->obj->color == colorIndex) {
               break;
            } else {
               count++;
            }
         }
      }
   }
   if (attr_ptr != NULL) {
      found_attr = attr_ptr;
   } else if (count != 1) {
      sprintf(gszMsgBox, "Can not find '%s' file attribute with color %s.",
            AttrName, colorMenuItems[colorIndex]);
      Msg(gszMsgBox);
      return NULL;
   }
   return found_attr;
}

struct AttrRec *FindAttrWithName(ObjPtr, AttrName, pp_top_owner)
   struct ObjRec *ObjPtr, **pp_top_owner;
   char *AttrName;
{
   struct ObjRec *obj_ptr;
   char *dot_ptr;

   if ((dot_ptr=strchr(AttrName, '.')) == NULL) {
      if (pp_top_owner != NULL) *pp_top_owner = ObjPtr;
      return FindObjAttrWithName(ObjPtr, AttrName);
   }
   *dot_ptr = '\0';
   if (strcmp(AttrName, "!") == 0) {
      *dot_ptr++ = '.';
      if (pp_top_owner != NULL) *pp_top_owner = tgifObj;
      return FindFileAttrWithName(dot_ptr);
   }
   if ((obj_ptr=FindObjWithName(botObj, ObjPtr, AttrName, FALSE,
         FALSE, NULL, pp_top_owner)) == NULL) {
      *dot_ptr++ = '.';
      return NULL;
   }
   *dot_ptr++ = '.';
   return FindObjAttrWithName(obj_ptr, dot_ptr);
}

struct AttrRec * ValidAttrArg (c_ptr, obj_ptr, new_c_ptr)
   char			* c_ptr, * * new_c_ptr;
   struct ObjRec	* obj_ptr;
{
   char	name[MAXSTRING+1], * name_ptr;
   struct AttrRec	* attr_ptr;

   name_ptr = name;
   if (c_ptr[0] == '$' && c_ptr[1] == '(')
   {
      for (c_ptr = &c_ptr[2]; *c_ptr != '\0'; c_ptr++)
         switch (*c_ptr)
         {
            case '\\':
               c_ptr++;
               *name_ptr++ = *c_ptr;
               break;
            case ')':
               *name_ptr++ = '=';
               *name_ptr = '\0';
               *new_c_ptr = c_ptr;
               attr_ptr = FindAttrWithName (obj_ptr, name, NULL);
               if (attr_ptr == NULL)
               {
                  char	msg[MAXSTRING+1];

                  sprintf (msg, "Can not find '%s' attr.", name);
                  Msg (msg);
               }
               return (attr_ptr);
            default:
               *name_ptr++ = *c_ptr;
               break;
         }
   }
   return (NULL);
}

int DoTeleport(teleport_attr)
   struct AttrRec *teleport_attr;
{
   char file_name[MAXPATHLENGTH+1], msg[MAXSTRING+1], *page_spec=NULL;
   int do_not_save=FALSE, rc=TRUE, just_goto_page=FALSE;

   while (!DirIsRemote(curDir) && fileModified) {
      switch (MsgBox("File modified.\n\nSave file before open? [ync](y)",
            TOOL_NAME, YNC_MB)) {
      case MB_ID_YES: SaveFile(); break;
      case MB_ID_NO: do_not_save = TRUE; SetFileModified(FALSE); break;
      case MB_ID_CANCEL: return FALSE;
      }
   }
   if (!FormNewFileName(curDir, teleport_attr->attr_value.s,
         (strcmp(teleport_attr->attr_name.s,TELEPORT_ATTR)==0 ? OBJ_FILE_EXT :
         NULL), file_name, &page_spec)) {
      sprintf(msg, "Invalid teleport destination '%s'.",
            teleport_attr->attr_value.s);
      MsgBox(msg, TOOL_NAME, INFO_MB);
      if (do_not_save) SetFileModified(TRUE);
      rc = FALSE;
   }
   if (*teleport_attr->attr_value.s == '#') just_goto_page = TRUE;

   if (rc && page_spec != NULL && just_goto_page) {
      int new_page_num=(-1);

      if (!GetPageNumFromPageSpec(page_spec, &new_page_num)) {
         sprintf(msg, "Invalid teleport destination '%s'.",
               teleport_attr->attr_value.s);
         MsgBox(msg, TOOL_NAME, INFO_MB);
         if (do_not_save) SetFileModified(TRUE);
         rc = FALSE;
      } else if (new_page_num != curPageNum) {
         /* PrepareToRecord(CMD_GOTO_PAGE, NULL, NULL, curPageNum); */
         GotoPageNum(new_page_num);
         /* RecordCmd(CMD_GOTO_PAGE, NULL, NULL, NULL, curPageNum); */
         ShowPage();
         ClearAndRedrawDrawWindow();
         RedrawTitleWindow();
         RedrawRulers();
         RedrawScrollBars();
         CleanUpCmds();
         CommitNavigate();
         justDupped = FALSE;
      }
      if (page_spec != NULL) free(page_spec);
      return rc;
   }
   if (!rc) {
      if (page_spec != NULL) free(page_spec);
      return rc;
   }
   if (FileIsRemote(file_name)) {
      char *buf=NULL, *content_type=NULL;
      int buf_sz=0, is_html=FALSE;

      SetWatchCursor(drawWindow);
      SetWatchCursor(mainWindow);
      SaveStatusStrings();
      rc = LoadRemoteFileInMem(file_name, &buf, &content_type, &buf_sz,
            &is_html, FALSE);
      RestoreStatusStrings();
      SetDefaultCursor(mainWindow);
      ShowCursor();
      if (rc && buf != NULL) {
         LoadRemoteFileFromMem(file_name, buf, content_type, buf_sz, is_html);
      } else {
         if (do_not_save) SetFileModified(TRUE);
         rc = FALSE;
      }
      if (content_type != NULL) FreeRemoteBuf(content_type);
      if (buf != NULL) FreeRemoteBuf(buf);
   } else {
      if (!LoadFile(file_name, TRUE)) {
         if (do_not_save) SetFileModified(TRUE);
         rc = FALSE;
      }
   }
   if (rc && page_spec != NULL && !just_goto_page) {
      int new_page_num=(-1);

      if (!GetPageNumFromPageSpec(page_spec, &new_page_num)) {
         sprintf(msg, "Invalid teleport destination '%s'.",
               teleport_attr->attr_value.s);
         MsgBox(msg, TOOL_NAME, INFO_MB);
         if (do_not_save) SetFileModified(TRUE);
         rc = FALSE;
      } else if (new_page_num != curPageNum) {
         GotoPageNum(new_page_num);
         ShowPage();
         ClearAndRedrawDrawWindow();
         RedrawTitleWindow();
         RedrawRulers();
         RedrawScrollBars();
         justDupped = FALSE;
      }
      if (page_spec != NULL) free(page_spec);
      return rc;
   }
   if (page_spec != NULL) free(page_spec);
   return rc;
}

#define DO_PAGE_BY_NUM 0
#define DO_PAGE_BY_NAME 1

int DoPageTeleport(teleport_attr, do_by_page_name)
   struct AttrRec *teleport_attr;
   int do_by_page_name;
{
   int i, rc=TRUE;
   char msg[MAXSTRING+1];

   if (do_by_page_name) {
      struct PageRec *page_ptr;

      for (i=1, page_ptr=firstPage; page_ptr!=NULL;
            page_ptr=page_ptr->next, i++) {
         if (page_ptr->name != NULL && strcmp(page_ptr->name,
               teleport_attr->attr_value.s) == 0) {
            if (curPageNum != i) SetCurPage(i);
            return TRUE;
         }
      }
      sprintf(msg, "Can not find page \"%s\" to warp to.",
            teleport_attr->attr_value.s);
      MsgBox(msg, TOOL_NAME, INFO_MB);
      rc = FALSE;
   } else {
      i = atoi(teleport_attr->attr_value.s);
      if (i >= 1 && i <= lastPageNum) {
         if (curPageNum != i) SetCurPage(i);
      } else {
         sprintf(msg, "Can not find page %1d to warp to.", i);
         MsgBox(msg, TOOL_NAME, INFO_MB);
         rc = FALSE;
      }
   }
   return rc;
}

static
void ResetDeckIndices()
{
   register struct ObjRec *obj_ptr;

   for (obj_ptr=botObj; obj_ptr != NULL; obj_ptr=obj_ptr->prev) {
      if (obj_ptr->type == OBJ_GROUP || obj_ptr->type == OBJ_SYM ||
            obj_ptr->type == OBJ_ICON) {
         obj_ptr->detail.r->deck_index = (-1);
      }
   }
}

void DoExecLoop(obj_ptr, exec_attr)
   struct ObjRec *obj_ptr;
   struct AttrRec *exec_attr;
{
   while (exec_attr != NULL) {
      int saved_intr_check_interval=intrCheckInterval;
      int one_line_status=FALSE, exec_rc;
      int saved_history_depth=historyDepth;
      char status_buf[MAX_STATUS_BTNS+1][MAXSTRING+1];

      MakeQuiescent();
      intrCheckInterval = 1;
      ShowInterrupt(1);

      ResetDeckIndices();
      SaveStatusStringsIntoBuf(status_buf, &one_line_status);
      if (cmdToExecAfterHyperJump != NULL) {
         free(cmdToExecAfterHyperJump);
         cmdToExecAfterHyperJump = NULL;
      }
      warpToAttr = NULL;
      execNavigateBack = FALSE;
      exec_rc = DoExec(exec_attr, obj_ptr);
      exec_attr = NULL;
      RemoveAllSel();
      EndExecAnimate();
      if (saved_history_depth != historyDepth) RestoreDefaultHistoryDepth();
      if (exec_rc == TRUE && warpToAttr != NULL) DoTeleport(warpToAttr);
      RestoreStatusStringsFromBuf(status_buf, one_line_status);

      while (HideInterrupt() > 0) ;
      intrCheckInterval = saved_intr_check_interval;

      if (exec_rc==TRUE && warpToAttr!=NULL) {
         if (cmdToExecAfterHyperJump == NULL) {
            if ((exec_attr=FindFileAttrWithName("auto_exec=")) == NULL) {
               return;
            }
            obj_ptr = NULL;
            continue;
         } else {
            exec_attr = FindAttrWithName(NULL, cmdToExecAfterHyperJump,
                  &obj_ptr);
            if (exec_attr == NULL) {
               sprintf(gszMsgBox, "Can not find the '%s' %s '%s' command.",
                     cmdToExecAfterHyperJump, "attribute while executing the",
                     "hyperjump_then_exec");
               MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
               return;
            }
            continue;
         }
      }
      if (exec_rc==TRUE && execNavigateBack) {
         NavigateBack();
      }
      return;
   }
   sprintf(gszMsgBox, "Can not find interpretable attribute.");
   TwoLineMsg(gszMsgBox, "No action taken.");
}

void Teleport (button_ev)
   XButtonEvent	* button_ev;
{
   struct AttrRec	* teleport_attr, * launch_attr, * exec_attr;
   struct ObjRec	* obj_ptr, * owner_obj;
   char			buf[MAXSTRING+1];
   int			len;

   if ((obj_ptr=FindAnObj(button_ev->x,button_ev->y,&owner_obj,NULL,NULL)) ==
         NULL) {
      return;
   }
   if (owner_obj != NULL) obj_ptr = owner_obj;

   teleport_attr = FindAttrWithName (obj_ptr, TELEPORT_ATTR, NULL);
   if (teleport_attr != NULL) {
      DoTeleport (teleport_attr);
      if ((exec_attr=FindFileAttrWithName("auto_exec=")) != NULL) {
         DoExecLoop(NULL, exec_attr);
      }
      return;
   }
   teleport_attr = FindAttrWithName (obj_ptr, "href=", NULL);
   if (teleport_attr != NULL) {
      DoTeleport (teleport_attr);
      if ((exec_attr=FindFileAttrWithName("auto_exec=")) != NULL) {
         DoExecLoop(NULL, exec_attr);
      }
      return;
   }
   strcpy (buf, TELEPORT_ATTR);
   len = strlen (buf);
   if (buf[len-1] == '=')
   {
      sprintf (&buf[len-1], "_page#=");
      teleport_attr = FindAttrWithName (obj_ptr, buf, NULL);
      if (teleport_attr != NULL)
      {
         DoPageTeleport (teleport_attr, DO_PAGE_BY_NUM);
         return;
      }

      sprintf (&buf[len-1], "_page=");
      teleport_attr = FindAttrWithName (obj_ptr, buf, NULL);
      if (teleport_attr != NULL)
      {
         DoPageTeleport (teleport_attr, DO_PAGE_BY_NAME);
         return;
      }
   }

   launch_attr = FindAttrWithName (obj_ptr, LAUNCH_ATTR, NULL);
   if (launch_attr != NULL) { DoLaunch (launch_attr, obj_ptr); return; }

   exec_attr = FindAttrWithName (obj_ptr, EXEC_ATTR, NULL);
   DoExecLoop(obj_ptr, exec_attr);
}

void SelAllObj (high_light)
   int	high_light;
{
   register struct SelRec	* sel_ptr;
   register struct ObjRec	* obj_ptr;

   TieLooseEnds ();
   SetCurChoice (NOTHING);
   if (topSel!=NULL) { HighLightReverse (); RemoveAllSel (); }

   for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
      obj_ptr->tmp_parent = NULL;
      if (colorLayers && !ObjInVisibleLayer(obj_ptr)) {
         continue;
      }
      sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (sel_ptr == NULL) FailAllocMessage();
      sel_ptr->next = topSel;
      sel_ptr->obj = obj_ptr;
      sel_ptr->prev = NULL;
      if (topSel == NULL) {
         botSel = sel_ptr;
      } else {
         topSel->prev = sel_ptr;
      }
      topSel = sel_ptr;
   }
   UpdSelBBox ();
   if (high_light) HighLightForward ();
   justDupped = FALSE;
}

static struct ObjRec	* tmpTopObj, * tmpBotObj;

static
void PushTmpObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   ObjPtr->next = tmpTopObj;
   ObjPtr->prev = NULL;

   if (tmpBotObj == NULL)
      tmpBotObj = ObjPtr;
   else
      tmpTopObj->prev = ObjPtr;

   tmpTopObj = ObjPtr;
}

static
void BreakSel ()
   /* break off selected objects from the main stream objects           */
   /* when returns, tmpTopObj points to the top of the selected objects */
   /*    and tmpBotObj points to the bottom of the selected objects     */
{
   register struct SelRec	* sel_ptr;

   for (sel_ptr = botSel; sel_ptr != NULL; sel_ptr = sel_ptr->prev)
   {
      UnlinkObj (sel_ptr->obj);
      PushTmpObj (sel_ptr->obj);
   }
}

void JustMoveSelToTop ()
{
   if (topSel == NULL) return;

   tmpTopObj = tmpBotObj = NULL;

   BreakSel ();
   tmpBotObj->next = topObj;
   if (topObj == NULL)
      curPage->bot = botObj = tmpBotObj;
   else
      topObj->prev = tmpBotObj;
   curPage->top = topObj = tmpTopObj;
}

void MoveSelToTop ()
{
   if (topSel == NULL) return;

   PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);
   JustMoveSelToTop ();
   RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
}

void MoveSelToBot ()
{
   if (topSel == NULL) return;

   PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);
   tmpTopObj = tmpBotObj = NULL;

   BreakSel ();
   tmpTopObj->prev = botObj;
   if (topObj == NULL)
      curPage->top = topObj = tmpTopObj;
   else
      botObj->next = tmpTopObj;
   curPage->bot = botObj = tmpBotObj;
   RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
}

void DelAllSelObj ()
{
   register struct ObjRec	* obj_ptr;
   register struct SelRec	* sel_ptr;
   register int			j, i;
   struct VSelRec		* vsel_ptr;
   IntPoint			* v=NULL;
   struct PolyRec		* poly_ptr=NULL;
   struct PolygonRec		* polygon_ptr=NULL;
   int				n=0;
   short			* mark;

   if (topSel==NULL && topVSel==NULL) { Msg ("No object selected!"); return; }

   HighLightReverse ();
   if (curChoice == VERTEXMODE)
   {
      StartCompositeCmd ();
      for (vsel_ptr=botVSel; vsel_ptr!=NULL; vsel_ptr=vsel_ptr->prev)
      {
         int	delete_it=FALSE, extra_vertex=FALSE;

         obj_ptr = vsel_ptr->obj;

         PrepareToReplaceAnObj (obj_ptr);
         switch (obj_ptr->type)
         {
            case OBJ_POLY:
               poly_ptr = obj_ptr->detail.p;
               v = poly_ptr->vlist;
               n = poly_ptr->n;

               if (vsel_ptr->n >= n-1) delete_it = TRUE;
               break;
            case OBJ_POLYGON:
               polygon_ptr = obj_ptr->detail.g;
               v = polygon_ptr->vlist;
               n = polygon_ptr->n;

               for (j=0; j < vsel_ptr->n; j++)
                  if (vsel_ptr->v_index[j] == 0)
                  {
                     extra_vertex = TRUE;
                     break;
                  }

               if ((!extra_vertex && n-vsel_ptr->n <= 3) ||
                     (extra_vertex && n-vsel_ptr->n <= 2))
                  delete_it = TRUE;
               break;
         }
         if (delete_it)
         {
            struct SelRec	* saved_top_sel, * saved_bot_sel;

            for (sel_ptr=botSel; sel_ptr!=NULL; sel_ptr=sel_ptr->prev)
               if (sel_ptr->obj == obj_ptr)
                  break;

            if (sel_ptr->prev == NULL)
               topSel = sel_ptr->next;
            else
               sel_ptr->prev->next = sel_ptr->next;

            if (sel_ptr->next == NULL)
               botSel = sel_ptr->prev;
            else
               sel_ptr->next->prev = sel_ptr->prev;

            saved_top_sel = topSel;
            saved_bot_sel = botSel;
            topSel = botSel = sel_ptr;
            sel_ptr->next = sel_ptr->prev = NULL;
            DelObj (obj_ptr);
            topSel = saved_top_sel;
            botSel = saved_bot_sel;

            free(sel_ptr);
            ChangeReplaceOneCmdToDeleteCmd ();
         }
         else
         {
            mark = (short *)malloc(n*sizeof(short));
            if (mark == NULL) FailAllocMessage();
            for (j=0; j < n; j++) mark[j] = FALSE;
            for (j=0; j < vsel_ptr->n; j++) mark[vsel_ptr->v_index[j]] = TRUE;

            switch (obj_ptr->type)
            {
               case OBJ_POLY:
                  for (i=n-1; i >= 0; i--)
                  {
                     if (mark[i])
                     {
                        for (j=i+1; j < n; j++)
                           v[j-1] = v[j];
                        n--;
                     }
                  }
                  poly_ptr->n -= vsel_ptr->n;
                  AdjObjSplineVs (obj_ptr);
                  if (poly_ptr->curved != LT_INTSPLINE)
                     UpdPolyBBox (obj_ptr, poly_ptr->n, poly_ptr->vlist);
                  else
                     UpdPolyBBox (obj_ptr, poly_ptr->intn, poly_ptr->intvlist);
                  break;
               case OBJ_POLYGON:
                  for (i=n-1; i >= 0; i--)
                  {
                     if (mark[i])
                     {
                        for (j=i+1; j < n; j++) v[j-1] = v[j];
                        n--;
                     }
                  }
                  polygon_ptr->n -= vsel_ptr->n;
                  if (extra_vertex) v[polygon_ptr->n++] = v[0];
                  AdjObjSplineVs (obj_ptr);
                  if (poly_ptr->curved != LT_INTSPLINE)
                     UpdPolyBBox (obj_ptr, polygon_ptr->n, polygon_ptr->vlist);
                  else
                     UpdPolyBBox (obj_ptr, polygon_ptr->intn,
                           polygon_ptr->intvlist);
                  break;
            }
            free(mark);
            AdjObjBBox (obj_ptr);
            RecordReplaceAnObj (obj_ptr);
         }
      }
      EndCompositeCmd ();
      JustRemoveAllVSel ();
   }
   else
   {
      PrepareToRecord (CMD_DELETE, topSel, botSel, numObjSelected);
      for (sel_ptr = botSel; sel_ptr != NULL; sel_ptr = sel_ptr->prev)
      {
         UnlinkObj (sel_ptr->obj);
         FreeObj (sel_ptr->obj);
      }
      RemoveAllSel ();
      RecordCmd (CMD_DELETE, NULL, NULL, NULL, 0);
   }
   RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward ();
   SetFileModified (TRUE);
   justDupped = FALSE;
}

void GroupSingleObj ()
{
   tmpTopObj = tmpBotObj = NULL;

   BreakSel ();
   CreateGroupObj (tmpTopObj, tmpBotObj);
   RemoveAllSel ();
   if (tmpTopObj == tmpBotObj && tmpTopObj->fattr != NULL &&
         !(tmpTopObj->type == OBJ_GROUP || tmpTopObj->type == OBJ_SYM ||
         tmpTopObj->type == OBJ_ICON))
   {
      struct AttrRec *attr_ptr;

      topObj->fattr = tmpTopObj->fattr;
      topObj->lattr = tmpTopObj->lattr;
      tmpTopObj->fattr = tmpTopObj->lattr = NULL;
      for (attr_ptr=topObj->fattr; attr_ptr != NULL; attr_ptr=attr_ptr->next) {
         attr_ptr->owner = topObj;
      }
   }

   topSel = botSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (topSel == NULL) FailAllocMessage();
   topSel->obj = topObj;
   topSel->next = topSel->prev = NULL;
   numObjSelected = 1;
}

void GroupSelObj ()
{
   if (topSel == NULL) { Msg ("No object to group!"); return; }
   if (curChoice==VERTEXMODE && topSel!=NULL)
   {
      Msg ("Can not group in vertex mode!");
      return;
   }
   if (topSel == botSel && topSel->obj->type != OBJ_POLY &&
         topSel->obj->type != OBJ_POLYGON)
   {
      Msg ("Can not group a single object!  Group aborted!");
      return;
   }

   tmpTopObj = tmpBotObj = NULL;

   HighLightReverse ();
   PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);
   BreakSel ();

   CreateGroupObj (tmpTopObj, tmpBotObj);

   RemoveAllSel ();
   topSel = botSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (topSel == NULL) FailAllocMessage();
   topSel->obj = topObj;
   topSel->next = topSel->prev = NULL;
   UpdSelBBox ();

   RecordCmd (CMD_MANY_TO_ONE, NULL, topSel, botSel, 1);
   RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward ();
   SetFileModified (TRUE);
   justDupped = FALSE;
}

void SelectTopObj ()
{
   if (topObj == NULL) return;

   topSel = botSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (topSel == NULL) FailAllocMessage();
   topSel->obj = topObj;
   topSel->next = topSel->prev = NULL;
   UpdSelBBox ();

   HighLightForward ();
   justDupped = FALSE;
}
