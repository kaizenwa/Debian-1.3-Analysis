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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/group.c,v 3.0 1996/05/06 16:05:24 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "attr.e"
#include "choice.e"
#include "cmd.e"
#include "drawing.e"
#include "dup.e"
#include "file.e"
#ifndef _NO_EXTERN
#include "group.e"
#endif
#include "mark.e"
#include "msg.e"
#include "obj.e"
#include "page.e"
#include "select.e"
#include "setup.e"

static struct SelRec	* tmpTopSel, * tmpBotSel;

void CreateGroupObj (TopObjPtr, BotObjPtr)
   struct ObjRec	* TopObjPtr, * BotObjPtr;
{
   struct GroupRec	* group_ptr;
   struct ObjRec	* obj_ptr;

   group_ptr = (struct GroupRec *)malloc(sizeof(struct GroupRec));
   if (group_ptr == NULL) FailAllocMessage();
   memset(group_ptr, 0, sizeof(struct GroupRec));
   group_ptr->first = TopObjPtr;
   group_ptr->last = BotObjPtr;
   group_ptr->rotate = ROTATE0;
   group_ptr->flip = NO_FLIP;
   group_ptr->deck_index = (-1);
   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));
   obj_ptr->x = selObjLtX; /* note:  selLtX, selLtY are absolute */
   obj_ptr->y = selObjLtY;
   obj_ptr->id = objId++;
   obj_ptr->locked = FALSE;
   obj_ptr->type = OBJ_GROUP;
   obj_ptr->bbox.ltx = selLtX;
   obj_ptr->bbox.lty = selLtY;
   obj_ptr->bbox.rbx = selRbX;
   obj_ptr->bbox.rby = selRbY;
   obj_ptr->obbox.ltx = selObjLtX;
   obj_ptr->obbox.lty = selObjLtY;
   obj_ptr->obbox.rbx = selObjRbX;
   obj_ptr->obbox.rby = selObjRbY;
   obj_ptr->detail.r = group_ptr;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;
   obj_ptr->invisible = FALSE;
   if (numObjLocked > 0) obj_ptr->locked = TRUE;
   AddObj (NULL, topObj, obj_ptr);
}

void SaveGroupObj (FP, ObjPtr, Level)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
   int			Level;
{
   if (fprintf (FP, "group([\n") == EOF) writeFileFailed = TRUE;
   Save (FP, ObjPtr->detail.r->last, Level+1, INVALID);
   if (fprintf (FP, "],\n") == EOF) writeFileFailed = TRUE;
   if (fprintf (FP, "%1d,%1d,%1d,", ObjPtr->id, ObjPtr->locked,
         ObjPtr->invisible) == EOF) {
      writeFileFailed = TRUE;
   }
   SaveAttrs (FP, ObjPtr->lattr);
   if (fprintf (FP, ")") == EOF) writeFileFailed = TRUE;
}

void SaveCompObj (FP, ObjPtr, Level)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
   int			Level;
{
   if (fprintf (FP, "sym([\n") == EOF) writeFileFailed = TRUE;
   Save (FP, ObjPtr->detail.r->last, Level+1, INVALID);
   if (fprintf (FP, "],\n") == EOF) writeFileFailed = TRUE;
   if (fprintf (FP, "%1d,%1d,%1d,", ObjPtr->id, ObjPtr->locked,
         ObjPtr->invisible) == EOF) {
      writeFileFailed = TRUE;
   }
   SaveAttrs (FP, ObjPtr->lattr);
   if (fprintf (FP, ")") == EOF) writeFileFailed = TRUE;
}

void SaveIconObj (FP, ObjPtr, Level)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
   int			Level;
{
   if (fprintf (FP, "icon([\n") == EOF) writeFileFailed = TRUE;
   Save (FP, ObjPtr->detail.r->last, Level+1, INVALID);
   if (fprintf (FP, "],\n") == EOF) writeFileFailed = TRUE;
   if (fprintf (FP, "\"%s\",%1d,%1d,%1d,%1d,%1d,",
         ObjPtr->detail.r->s, ObjPtr->id, ObjPtr->detail.r->rotate,
         ObjPtr->detail.r->flip, ObjPtr->locked, ObjPtr->invisible) == EOF) {
      writeFileFailed = TRUE;
   }
   SaveAttrs (FP, ObjPtr->lattr);
   if (fprintf (FP, ")") == EOF) writeFileFailed = TRUE;
}

void ReadGroupObj (FP, ObjType, ObjPtr)
   FILE			* FP;
   int			ObjType;
   struct ObjRec	* * ObjPtr;
{
   struct GroupRec	* group_ptr;
   struct ObjRec	* top_obj = NULL, * bot_obj = NULL, * obj_ptr;
   int			ltx, lty, rbx, rby, id=0, locked=FALSE;
   int			obj_ltx, obj_lty, obj_rbx, obj_rby, rotate=0, flip=0;
   int			invisible=FALSE;
   char			line[MAXSTRING+1], * s, * s1, tmp_str[MAXSTRING+1];

   *ObjPtr = NULL;
   while (ReadObj(FP, &obj_ptr)) {
      if (obj_ptr == NULL) return;

      obj_ptr->next = top_obj;
      if (top_obj == NULL) {
         bot_obj = obj_ptr;
      } else {
         top_obj->prev = obj_ptr;
      }
      top_obj = obj_ptr;
   }
   if (top_obj == NULL) return;

   if (fileVersion <= 20 && (ObjType==OBJ_GROUP || ObjType==OBJ_SYM)) {
      id = objId++;
   } else {
      if (fgets(line, MAXSTRING, FP) == NULL) return;
      scanLineNum++;

      switch (ObjType) {
      case OBJ_GROUP:
         InitScan(line, "\t\n, []");
         if (fileVersion <= 25) {
            if (ScanValue("%d", &id, "id", "group") == INVALID) return;
         } else if (fileVersion <= 32) {
            if (ScanValue("%d", &id, "id", "group") == INVALID ||
                ScanValue("%d", &locked, "locked", "group") == INVALID) {
               return;
            }
         } else {
            if (ScanValue("%d", &id, "id", "group") == INVALID ||
                ScanValue("%d", &locked, "locked", "group") == INVALID ||
                ScanValue("%d", &invisible, "invisible", "group") == INVALID) {
               return;
            }
         }
         if (id >= objId) objId = id+1;
         break;
      case OBJ_SYM:
         InitScan(line, "\t\n, []");
         if (fileVersion <= 25) {
            if (ScanValue("%d", &id, "id", "sym") == INVALID) return;
         } else if (fileVersion <= 32) {
            if (ScanValue("%d", &id, "id", "sym") == INVALID ||
                ScanValue("%d", &locked, "locked", "sym") == INVALID) {
               return;
            }
         } else {
            if (ScanValue("%d", &id, "id", "sym") == INVALID ||
                ScanValue("%d", &locked, "locked", "sym") == INVALID ||
                ScanValue("%d", &invisible, "invisible", "group") == INVALID) {
               return;
            }
         }
         if (id >= objId) objId = id+1;
         break;
      case OBJ_ICON:
         strcpy(tmp_str, FindChar((int)'"', line));
         s = FindChar((int)'"', tmp_str);
         if (fileVersion == INVALID) return;

         if (fileVersion <= 12) {
            s1 = FindChar((int)',', s);
            InitScan(s1, "\t\n, ");
            if (ScanValue("%d", &id, "id", "icon") == INVALID) return;
         } else if (fileVersion <= 25) {
            s1 = FindChar((int)',', s);
            InitScan(s1, "\t\n, ");
            if (ScanValue("%d", &id, "id", "icon") == INVALID ||
                ScanValue("%d", &rotate, "rotation", "icon") == INVALID ||
                ScanValue("%d", &flip, "flip", "icon") == INVALID) {
               return;
            }
         } else if (fileVersion <= 32) {
            s1 = FindChar((int)',', s);
            InitScan(s1, "\t\n, ");
            if (ScanValue("%d", &id, "id", "icon") == INVALID ||
                ScanValue("%d", &rotate, "rotation", "icon") == INVALID ||
                ScanValue("%d", &flip, "flip", "icon") == INVALID ||
                ScanValue("%d", &locked, "locked", "icon") == INVALID) {
               return;
            }
         } else {
            s1 = FindChar((int)',', s);
            InitScan(s1, "\t\n, ");
            if (ScanValue("%d", &id, "id", "icon") == INVALID ||
                ScanValue("%d", &rotate, "rotation", "icon") == INVALID ||
                ScanValue("%d", &flip, "flip", "icon") == INVALID ||
                ScanValue("%d", &locked, "locked", "icon") == INVALID ||
                ScanValue("%d", &invisible, "invisible", "icon") == INVALID) {
               return;
            }
         }
         if (id >= objId) objId = id+1;
         *(--s) = '\0';
         break;
      }
   }

   *ObjPtr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));

   top_obj->prev = NULL;

   group_ptr = (struct GroupRec *)malloc(sizeof(struct GroupRec));
   if (group_ptr == NULL) FailAllocMessage();
   memset(group_ptr, 0, sizeof(struct GroupRec));
   group_ptr->first = top_obj;
   group_ptr->last = bot_obj;
   group_ptr->rotate = rotate;
   group_ptr->flip = flip;
   group_ptr->deck_index = (-1);
   if (ObjType == OBJ_ICON) strcpy(group_ptr->s, tmp_str);

   ltx = top_obj->bbox.ltx;
   lty = top_obj->bbox.lty;
   rbx = top_obj->bbox.rbx;
   rby = top_obj->bbox.rby;
   obj_ltx = top_obj->obbox.ltx;
   obj_lty = top_obj->obbox.lty;
   obj_rbx = top_obj->obbox.rbx;
   obj_rby = top_obj->obbox.rby;
   for (obj_ptr = top_obj->next; obj_ptr != NULL; obj_ptr = obj_ptr->next) {
      if (obj_ptr->bbox.ltx < ltx) ltx = obj_ptr->bbox.ltx;
      if (obj_ptr->bbox.lty < lty) lty = obj_ptr->bbox.lty;
      if (obj_ptr->bbox.rbx > rbx) rbx = obj_ptr->bbox.rbx;
      if (obj_ptr->bbox.rby > rby) rby = obj_ptr->bbox.rby;
      if (obj_ptr->obbox.ltx < obj_ltx) obj_ltx = obj_ptr->obbox.ltx;
      if (obj_ptr->obbox.lty < obj_lty) obj_lty = obj_ptr->obbox.lty;
      if (obj_ptr->obbox.rbx > obj_rbx) obj_rbx = obj_ptr->obbox.rbx;
      if (obj_ptr->obbox.rby > obj_rby) obj_rby = obj_ptr->obbox.rby;
   }
   
   (*ObjPtr)->x = obj_ltx;
   (*ObjPtr)->y = obj_lty;
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->type = ObjType;
   (*ObjPtr)->bbox.ltx = ltx;
   (*ObjPtr)->bbox.lty = lty;
   (*ObjPtr)->bbox.rbx = rbx;
   (*ObjPtr)->bbox.rby = rby;
   (*ObjPtr)->obbox.ltx = obj_ltx;
   (*ObjPtr)->obbox.lty = obj_lty;
   (*ObjPtr)->obbox.rbx = obj_rbx;
   (*ObjPtr)->obbox.rby = obj_rby;
   (*ObjPtr)->detail.r = group_ptr;
   (*ObjPtr)->ctm = NULL;
   (*ObjPtr)->invisible = invisible;
}

void FreeGroupObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register struct ObjRec	* ptr, * next_obj;

   for (ptr = ObjPtr->detail.r->first; ptr != NULL; ptr = next_obj) {
      next_obj = ptr->next;
      FreeObj(ptr);
   }
   free(ObjPtr->detail.r);
   free(ObjPtr);
}

static
void UngroupObj (ObjPtr)
   struct ObjRec	* ObjPtr;
   /* ungroup the grouped object ObjPtr to a list of objects   */
   /* when returns, a list of select pointers will be created, */
   /*    tmpTopSel will point to the top of the list, and      */
   /*    tmpBotSel will point to the bottom of the list.       */
{
   register struct SelRec	* sel_ptr;
   register struct ObjRec	* obj_ptr = ObjPtr->detail.r->last;

   tmpTopSel = tmpBotSel = NULL;

   for ( ; obj_ptr != NULL; obj_ptr=obj_ptr->prev) {
      sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (sel_ptr == NULL) FailAllocMessage();
      sel_ptr->obj = obj_ptr;
      sel_ptr->next = tmpTopSel;
      if (tmpTopSel == NULL) {
         tmpBotSel = sel_ptr;
      } else {
         tmpTopSel->prev = sel_ptr;
      }
      tmpTopSel = sel_ptr;
   }
   tmpTopSel->prev = NULL;
}

static
int NoObjToUngroup ()
{
   register struct SelRec	* sel_ptr;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
      if (sel_ptr->obj->type == OBJ_GROUP)
         return (FALSE);
   return (TRUE);
}

void UngroupSelObj ()
{
   register struct SelRec	* sel_ptr, * next_sel;
   register struct ObjRec	* obj_ptr;
   int				sel_ltx, sel_lty, sel_rbx, sel_rby;
   int				changed=FALSE;

   if (topSel==NULL || NoObjToUngroup ()) return;

   sel_ltx = selLtX; sel_lty = selLtY;
   sel_rbx = selRbX; sel_rby = selRbY;

   HighLightReverse ();
   StartCompositeCmd ();
   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = next_sel)
   {
      next_sel = sel_ptr->next;
      obj_ptr = sel_ptr->obj;
      if (obj_ptr->type == OBJ_GROUP)
      {
         int		count;
         struct SelRec	* tmp_sel_ptr;

         changed = TRUE;
         PrepareToReplaceAnObj (obj_ptr);

         UngroupObj (obj_ptr);
         DetachGroupAttrs (obj_ptr, &tmpTopSel, &tmpBotSel);

         obj_ptr->detail.r->first->prev = obj_ptr->prev;
         if (obj_ptr->prev == NULL)
            curPage->top = topObj = obj_ptr->detail.r->first;
         else
            obj_ptr->prev->next = obj_ptr->detail.r->first;
         obj_ptr->detail.r->last->next = obj_ptr->next;
         if (obj_ptr->next == NULL)
            curPage->bot = botObj = obj_ptr->detail.r->last;
         else
            obj_ptr->next->prev = obj_ptr->detail.r->last;

         count = 0;
         for (tmp_sel_ptr=tmpTopSel; tmp_sel_ptr!=NULL;
               tmp_sel_ptr=tmp_sel_ptr->next)
            count++;
         RecordCmd (CMD_ONE_TO_MANY, NULL, tmpTopSel, tmpBotSel, count);

         tmpTopSel->prev = sel_ptr->prev;
         if (sel_ptr->prev == NULL)
            topSel = tmpTopSel;
         else
            sel_ptr->prev->next = tmpTopSel;
         tmpBotSel->next = sel_ptr->next;
         if (sel_ptr->next == NULL)
            botSel = tmpBotSel;
         else
            sel_ptr->next->prev = tmpBotSel;

         free(sel_ptr);
         free(obj_ptr);
      }
   }
   EndCompositeCmd ();
   if (changed)
   {
      UpdSelBBox ();
      RedrawAreas (botObj,
            sel_ltx-GRID_ABS_SIZE(1), sel_lty-GRID_ABS_SIZE(1),
            sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1),
            selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      SetFileModified (TRUE);
      justDupped = FALSE;
      Msg ("Selected objects are ungrouped.");
   }
   HighLightForward ();
}

void LockSelObj ()
{
   register struct SelRec	* sel_ptr;
   register struct ObjRec	* obj_ptr;
   int				changed=FALSE;

   if (topSel==NULL) { Msg ("No object to lock!"); return; }
   if (curChoice==VERTEXMODE) { Msg ("Can not lock in vertex mode!"); return; }

   HighLightReverse ();
   StartCompositeCmd ();
   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      obj_ptr = sel_ptr->obj;
      if (!obj_ptr->locked)
      {
         changed = TRUE;
         PrepareToReplaceAnObj (obj_ptr);
         obj_ptr->locked = TRUE;
         RecordReplaceAnObj (obj_ptr);
      }
   }
   EndCompositeCmd ();
   HighLightForward ();
   if (changed)
   {
      UpdSelBBox ();
      SetFileModified (TRUE);
      justDupped = FALSE;
      Msg ("Selected objects are locked.");
   }
}

void UnlockSelObj ()
{
   register struct SelRec	* sel_ptr;
   register struct ObjRec	* obj_ptr;
   int				changed=FALSE;

   if (topSel==NULL) { Msg ("No object to unlock!"); return; }
   if (curChoice==VERTEXMODE) { Msg("Can not unlock in vertex mode!"); return; }

   HighLightReverse ();
   StartCompositeCmd ();
   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      obj_ptr = sel_ptr->obj;
      if (obj_ptr->locked)
      {
         changed = TRUE;
         PrepareToReplaceAnObj (obj_ptr);
         obj_ptr->locked = FALSE;
         RecordReplaceAnObj (obj_ptr);
      }
   }
   EndCompositeCmd ();
   HighLightForward ();
   if (changed)
   {
      UpdSelBBox ();
      SetFileModified (TRUE);
      justDupped = FALSE;
      Msg ("Selected objects are unlocked.");
   }
}
