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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/cmd.c,v 3.0 1996/05/06 16:04:09 william Exp $";
#endif

#include <stdio.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "attr.e"
#include "choice.e"
#ifndef _NO_EXTERN
#include "cmd.e"
#endif
#include "color.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "imgproc.e"
#include "mark.e"
#include "move.e"
#include "msg.e"
#include "obj.e"
#include "page.e"
#include "select.e"
#include "setup.e"
#include "stk.e"
#include "xpixmap.e"

#ifdef _TGIF_WB
#include "wb1.e"
#endif /* _TGIF_WB */

int		recordCmdIncludeTgifObj=FALSE;
int		recordCmdUsesNewColormap=FALSE;
int		undoingOrRedoing=FALSE;
int		historyDepth=(-1), historyCount=0, defaultHistoryDepth=(-1);
struct CmdRec	*firstCmd=NULL, *lastCmd=NULL, *curCmd=NULL;

struct SelRec	*topSelBeforeInCmd=NULL, *botSelBeforeInCmd=NULL;
int		*stackingPosition=NULL;
int		stackingCount=0;
int		composingCommand=FALSE;
Colormap	preparedColormap=None;


void InsertCmd(PrevCmd, NextCmd, CmdPtr)
   struct CmdRec *PrevCmd, *NextCmd, *CmdPtr;
   /* add CmdPtr between PrevCmd and NextCmd */
{
   CmdPtr->prev = PrevCmd;
   CmdPtr->next = NextCmd;

   if (PrevCmd == NULL)
      firstCmd = CmdPtr;
   else
      PrevCmd->next = CmdPtr;

   if (NextCmd == NULL)
      lastCmd = CmdPtr;
   else
      NextCmd->prev = CmdPtr;
}

void DeleteARedoRecord(CmdPtr, percent_start, percent_end)
   struct CmdRec *CmdPtr;
   double percent_start, percent_end;
{
   register struct SelRec *sel_ptr, *next_sel;
   struct CmdRec *cmd_ptr, *prev_cmd;
   int num_records=0;
   double inc=0;

   if (CmdPtr->pos_before != NULL) free(CmdPtr->pos_before);
   if (CmdPtr->pos_after != NULL) free(CmdPtr->pos_after);

   if (percent_start >= 0.0) {
      sprintf(gszMsgBox, "Flushing undo buffers... %1d%%",
            round(percent_start));
      SetStringStatus(gszMsgBox);
      CheckInterrupt();
   }
   switch (CmdPtr->type) {
   case CMD_NEW:
      for (sel_ptr=CmdPtr->top_after; sel_ptr!=NULL; sel_ptr=next_sel) {
         next_sel = sel_ptr->next;
         if (CmdPtr->undone) FreeObj(sel_ptr->obj);
         free(sel_ptr);
      }
      break;
   case CMD_DELETE:
      for (sel_ptr=CmdPtr->top_before; sel_ptr!=NULL; sel_ptr=next_sel) {
         next_sel = sel_ptr->next;
         if (!CmdPtr->undone) FreeObj(sel_ptr->obj);
         free(sel_ptr);
      }
      break;
   case CMD_MOVE:
      for (sel_ptr=CmdPtr->top_after; sel_ptr!=NULL; sel_ptr=next_sel) {
         next_sel = sel_ptr->next;
         free(sel_ptr);
      }
      break;
   case CMD_STRETCH:
      for (sel_ptr=CmdPtr->top_before; sel_ptr!=NULL; sel_ptr=next_sel) {
         next_sel = sel_ptr->next;
         FreeObj(sel_ptr->obj);
         free(sel_ptr);
      }
      break;
   case CMD_REPLACE:
      for (sel_ptr=CmdPtr->top_before; sel_ptr!=NULL; sel_ptr=next_sel) {
         next_sel = sel_ptr->next;
         FreeObj(sel_ptr->obj);
         free(sel_ptr);
      }
      break;
   case CMD_ONE_TO_MANY:
      for (sel_ptr=CmdPtr->top_before; sel_ptr!=NULL; sel_ptr=next_sel) {
         next_sel = sel_ptr->next;
         if (!CmdPtr->undone) FreeObj(sel_ptr->obj);
         free(sel_ptr);
      }
      for (sel_ptr=CmdPtr->top_after; sel_ptr!=NULL; sel_ptr=next_sel) {
         next_sel = sel_ptr->next;
         if (CmdPtr->undone) FreeObj(sel_ptr->obj);
         free(sel_ptr);
      }
      break;
   case CMD_MANY_TO_ONE:
      for (sel_ptr=CmdPtr->top_before; sel_ptr!=NULL; sel_ptr=next_sel) {
         next_sel = sel_ptr->next;
         if (!CmdPtr->undone) FreeObj(sel_ptr->obj);
         free(sel_ptr);
      }
      for (sel_ptr=CmdPtr->top_after; sel_ptr!=NULL; sel_ptr=next_sel) {
         next_sel = sel_ptr->next;
         if (CmdPtr->undone) FreeObj(sel_ptr->obj);
         free(sel_ptr);
      }
      break;
   case CMD_COMPOSITE:
      if (percent_start >= 0) {
         for (cmd_ptr=CmdPtr->first; cmd_ptr!=NULL; cmd_ptr=cmd_ptr->next) {
            num_records++;
         }
         if (num_records > 0) {
            inc = (percent_end-percent_start)/((double)num_records);
         }
      }
      for (cmd_ptr=CmdPtr->last; cmd_ptr!=NULL; cmd_ptr=prev_cmd) {
         prev_cmd = cmd_ptr->prev;
         if (percent_start >= 0) {
            DeleteARedoRecord(cmd_ptr, percent_start,
                  min(((double)percent_start+inc),((double)100.0)));
            percent_start += inc;
         } else {
            DeleteARedoRecord(cmd_ptr, percent_start, percent_end);
         }
      }
      break;
   }
   free(CmdPtr);
}

void ClearRedoRecords(CmdPtr)
   struct CmdRec *CmdPtr;
{
   register struct CmdRec *cmd_ptr, *prev_cmd;

   for (cmd_ptr=lastCmd; cmd_ptr != curCmd; cmd_ptr=prev_cmd) {
      prev_cmd = cmd_ptr->prev;
      DeleteARedoRecord(cmd_ptr, (-1.0), (-1.0));
      historyCount--;
   }
   if ((lastCmd=curCmd) == NULL) firstCmd = NULL;
}

void CleanUpCmds ()
{
   register struct CmdRec *cmd_ptr, *prev_cmd;
   int num_records=0;

   for (cmd_ptr=lastCmd; cmd_ptr != NULL; cmd_ptr=cmd_ptr->prev) {
      num_records++;
   }
   if (num_records > 0) {
      double inc=(100.0/((double)num_records)), percent_start=0.0;

      ShowInterrupt(1);
      SaveStatusStrings();
      for (cmd_ptr = lastCmd; cmd_ptr != NULL; cmd_ptr=prev_cmd,
            percent_start += inc) {
         prev_cmd = cmd_ptr->prev;
         DeleteARedoRecord(cmd_ptr, percent_start,
               min(((double)percent_start+inc),((double)100.0)));
      }
      RestoreStatusStrings();
      HideInterrupt();
   }
   firstCmd = lastCmd = curCmd = NULL;
   historyCount = 0;
}

void CopySel(from_top_sel, count, to_top_sel, to_bot_sel)
   struct SelRec *from_top_sel;
   int count;
   struct SelRec **to_top_sel, **to_bot_sel;
{
   register struct SelRec *sel_ptr, *to_sel_ptr;

   *to_top_sel = *to_bot_sel = NULL;

   for (sel_ptr=from_top_sel; sel_ptr!=NULL; sel_ptr=sel_ptr->next, count--) {
      to_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (to_sel_ptr == NULL) FailAllocMessage();
      to_sel_ptr->next = NULL;
      to_sel_ptr->prev = *to_bot_sel;
      to_sel_ptr->obj = sel_ptr->obj;
      if (*to_bot_sel == NULL) {
         *to_top_sel = to_sel_ptr;
      } else {
         (*to_bot_sel)->next = to_sel_ptr;
      }
      *to_bot_sel = to_sel_ptr;
   }
   if (count != 0) {
      Msg("Possible fatal error.  Please read the terminal error output.");
      fprintf(stderr, "Warning:  count!=0 in CopySel().\n");
      fprintf(stderr, "\nUndo()/Redo() may crash %s.\n", TOOL_NAME);
      fprintf(stderr, "Safest thing to do is to save and exit.\n");
      fprintf(stderr, "Please try to reproduce this error and\n");
      fprintf(stderr, "\tsend bug report to william@cs.ucla.edu.\n");
      sprintf(gszMsgBox, "%s.\n%s %s.\n\n%s.\n\n%s %s.",
            "Warning:  count!=0 in CopySel()",
            "Undo()/Redo() may crash", TOOL_NAME,
            "Safest thing to do is to save and exit",
            "Please try to reproduce this error and",
            "send bug report to william@cs.ucla.edu");
      MsgBox(gszMsgBox, TOOL_NAME, STOP_MB);
      XFlush(mainDisplay);
      XSync(mainDisplay, False);
   }
}

static
void LinkJustTheObjects(TopSel, BotSel)
   struct SelRec *TopSel, *BotSel;
{
   register struct SelRec *sel_ptr, *sel_ptr_next;

   sel_ptr=TopSel;
   sel_ptr_next=TopSel->next;
   sel_ptr->obj->prev = NULL;
   for ( ; sel_ptr_next!=NULL; sel_ptr=sel_ptr_next, sel_ptr_next=sel_ptr->next)
   {
      sel_ptr->obj->next = sel_ptr_next->obj;
      sel_ptr_next->obj->prev = sel_ptr->obj;
   }
   sel_ptr->obj->next = NULL;
}

static
void UndoNewCmd(CmdPtr)
   struct CmdRec *CmdPtr;
{
   register struct SelRec *sel_ptr;
   register struct ObjRec *obj_ptr, *next_obj;
   int pos, count;

   topSel = CmdPtr->top_after;
   botSel = CmdPtr->bot_after;

   sel_ptr = topSel;
   pos = count = 0;
   for (obj_ptr=topObj; obj_ptr != NULL; obj_ptr=next_obj, pos++)
   {
      next_obj = obj_ptr->next;
      if (pos == CmdPtr->pos_after[count])
      {
         count++;
         sel_ptr->obj = obj_ptr;
         UnlinkObj (obj_ptr);
         sel_ptr = sel_ptr->next;
         if (count == CmdPtr->count_after) break;
      }
   }
   LinkJustTheObjects (CmdPtr->top_after, CmdPtr->bot_after);

   UpdSelBBox ();
   topSel = botSel = NULL;
   RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   SetFileModified (TRUE);
   justDupped = FALSE;
}

static
void RedoNewCmd(CmdPtr)
   struct CmdRec *CmdPtr;
{
   struct ObjRec *saved_top_obj, *saved_bot_obj;

   CopySel (CmdPtr->top_after, CmdPtr->count_after, &topSel, &botSel);

   saved_top_obj = topObj;
   saved_bot_obj = botObj;

   curPage->top = topObj = CmdPtr->top_after->obj;
   curPage->bot = botObj = CmdPtr->bot_after->obj;

   AdjSplineVs ();
   AdjCaches ();

   curPage->top = topObj = saved_top_obj;
   curPage->bot = botObj = saved_bot_obj;

   if (topObj == NULL)
      curPage->bot = botObj = botSel->obj;
   else
      topObj->prev = botSel->obj;

   botSel->obj->next = topObj;
   curPage->top = topObj = topSel->obj;

   UpdSelBBox ();
   RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward ();
   SetFileModified (TRUE);
   justDupped = FALSE;
}

static
void UndoDeleteCmd(CmdPtr)
   struct CmdRec *CmdPtr;
{
   register struct SelRec *sel_ptr;
   register struct ObjRec *obj_ptr, *next_obj;
   register int pos;
   struct ObjRec *saved_top_obj, *saved_bot_obj;
   int count;

   CopySel (CmdPtr->top_before, CmdPtr->count_before, &topSel, &botSel);

   saved_top_obj = topObj;
   saved_bot_obj = botObj;

   LinkJustTheObjects (CmdPtr->top_before, CmdPtr->bot_before);

   curPage->top = topObj = CmdPtr->top_before->obj;
   curPage->bot = botObj = CmdPtr->bot_before->obj;

   AdjSplineVs ();
   AdjCaches ();

   curPage->top = topObj = saved_top_obj;
   curPage->bot = botObj = saved_bot_obj;

   pos = count = 0;
   sel_ptr = topSel;
   for (obj_ptr=topObj; obj_ptr!=NULL; obj_ptr=next_obj, pos++)
   {
      if (pos == CmdPtr->pos_before[count])
      {
         AddObj (obj_ptr->prev, obj_ptr, sel_ptr->obj);
         next_obj = obj_ptr;
         count++;
         sel_ptr = sel_ptr->next;
         if (count == CmdPtr->count_before) break;
      }
      else
         next_obj = obj_ptr->next;
   }
   for ( ; sel_ptr != NULL; sel_ptr=sel_ptr->next)
      AddObj (botObj, NULL, sel_ptr->obj);

   UpdSelBBox ();
   RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward ();
   SetFileModified (TRUE);
   justDupped = FALSE;
}

static
void RedoDeleteCmd(CmdPtr)
   struct CmdRec *CmdPtr;
{
   register struct SelRec *sel_ptr;
   register struct ObjRec *obj_ptr, *next_obj;
   int pos, count;

   topSel = CmdPtr->top_before;
   botSel = CmdPtr->bot_before;

   sel_ptr = topSel;
   pos = count = 0;
   for (obj_ptr=topObj; obj_ptr!=NULL; obj_ptr=next_obj, pos++)
   {
      next_obj = obj_ptr->next;
      if (pos == CmdPtr->pos_before[count])
      {
         count++;
         sel_ptr->obj = obj_ptr;
         UnlinkObj (obj_ptr);
         sel_ptr = sel_ptr->next;
         if (count == CmdPtr->count_before) break;
      }
   }
   LinkJustTheObjects (CmdPtr->top_before, CmdPtr->bot_before);

   UpdSelBBox ();
   topSel = botSel = NULL;
   RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   SetFileModified (TRUE);
   justDupped = FALSE;
}

static
void UndoOrRedoMoveCmd(CmdPtr)
   struct CmdRec *CmdPtr;
{
   register struct SelRec *sel_ptr;
   register struct ObjRec *obj_ptr, *next_obj;
   int pos, count, dx, dy;

   dx = (CmdPtr->undone) ? CmdPtr->dx : -(CmdPtr->dx);
   dy = (CmdPtr->undone) ? CmdPtr->dy : -(CmdPtr->dy);

   CopySel (CmdPtr->top_after, CmdPtr->count_after, &topSel, &botSel);

   sel_ptr = topSel;
   pos = count = 0;
   for (obj_ptr=topObj; obj_ptr != NULL; obj_ptr=next_obj, pos++)
   {
      next_obj = obj_ptr->next;
      if (pos == CmdPtr->pos_after[count])
      {
         count++;
         sel_ptr->obj = obj_ptr;
         if (!obj_ptr->locked) MoveObj (obj_ptr, dx, dy);
         sel_ptr = sel_ptr->next;
         if (count == CmdPtr->count_after) break;
      }
   }

   UpdSelBBox ();
   RedrawAreas (botObj, selLtX-GRID_ABS_SIZE(1)-dx, selLtY-GRID_ABS_SIZE(1)-dy,
         selRbX+GRID_ABS_SIZE(1)-dx, selRbY+GRID_ABS_SIZE(1)-dy,
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward ();
   SetFileModified (TRUE);
   justDupped = FALSE;
}

static
void UpdateXPmObjectsInACmd(CmdPtr)
   struct CmdRec *CmdPtr;
{
   struct CmdRec *cmd_ptr;
   struct SelRec *sel_ptr;

   switch (CmdPtr->type) {
   case CMD_COMPOSITE:
      for (cmd_ptr=CmdPtr->first; cmd_ptr!=NULL; cmd_ptr=cmd_ptr->next) {
         UpdateXPmObjectsInACmd(cmd_ptr);
      }
      break;
   case CMD_NEW:
      for (sel_ptr=CmdPtr->top_after; sel_ptr!=NULL; sel_ptr=sel_ptr->next) {
         UpdateXPmObjects(sel_ptr->obj);
      }
      break;
   case CMD_DELETE: break;
   case CMD_MOVE: break;
   case CMD_STRETCH: break;
   case CMD_REPLACE:
      for (sel_ptr=CmdPtr->top_after; sel_ptr!=NULL; sel_ptr=sel_ptr->next) {
         UpdateXPmObjects(sel_ptr->obj);
      }
      break;
   case CMD_ONE_TO_MANY: break;
   case CMD_MANY_TO_ONE:
      for (sel_ptr=CmdPtr->top_before; sel_ptr!=NULL; sel_ptr=sel_ptr->next) {
         UpdateXPmObjects(sel_ptr->obj);
      }
      for (sel_ptr=CmdPtr->top_after; sel_ptr!=NULL; sel_ptr=sel_ptr->next) {
         UpdateXPmObjects(sel_ptr->obj);
      }
      break;
   case CMD_GOTO_PAGE: break;
   }
}

static
int UpdatePixelInACmd(CmdPtr, old_color_str)
   struct CmdRec *CmdPtr;
   char **old_color_str;
{
   struct CmdRec *cmd_ptr;
   struct SelRec *sel_ptr;

   switch (CmdPtr->type) {
   case CMD_COMPOSITE:
      for (cmd_ptr=CmdPtr->first; cmd_ptr!=NULL; cmd_ptr=cmd_ptr->next) {
         UpdatePixelInACmd(cmd_ptr, old_color_str);
      }
      break;
   case CMD_NEW:
      for (sel_ptr=CmdPtr->top_after; sel_ptr!=NULL; sel_ptr=sel_ptr->next) {
         UpdatePixel(sel_ptr->obj, old_color_str);
      }
      break;
   case CMD_DELETE: break;
   case CMD_MOVE: break;
   case CMD_STRETCH: break;
   case CMD_REPLACE:
      for (sel_ptr=CmdPtr->top_after; sel_ptr!=NULL; sel_ptr=sel_ptr->next) {
         UpdatePixel(sel_ptr->obj, old_color_str);
      }
      break;
   case CMD_ONE_TO_MANY: break;
   case CMD_MANY_TO_ONE:
      for (sel_ptr=CmdPtr->top_before; sel_ptr!=NULL; sel_ptr=sel_ptr->next) {
         UpdatePixel(sel_ptr->obj, old_color_str);
      }
      for (sel_ptr=CmdPtr->top_after; sel_ptr!=NULL; sel_ptr=sel_ptr->next) {
         UpdatePixel(sel_ptr->obj, old_color_str);
      }
      break;
   case CMD_GOTO_PAGE: break;
   }
}

static
int RefreshColormap(nRedoing, CmdPtr)
   int nRedoing;
   struct CmdRec *CmdPtr;
   /*
    * This function mimics FlushColormap() in color.c.
    */
{
   register int i;
   int old_allocated_colors, changed=FALSE, saved_color_layers;
   char **old_color_str;
   struct ObjRec *obj_ptr;
   struct PageRec *page_ptr;
   Colormap colormap=XCopyColormapAndFree(mainDisplay, mainColormap);

   mainColormap = colormap;
   newColormapUsed = TRUE;
   XSetWindowColormap(mainDisplay, mainWindow, mainColormap);

   if (nRedoing) {
      /* need to scan forward to pick up objects */
      struct CmdRec *cmd_ptr;

      for (page_ptr=firstPage; page_ptr != NULL; page_ptr=page_ptr->next) {
         for (obj_ptr=page_ptr->bot; obj_ptr!=NULL; obj_ptr=obj_ptr->prev) {
            UpdateXPmObjects(obj_ptr);
         }
      }
      for (cmd_ptr=CmdPtr->next; cmd_ptr != NULL; cmd_ptr=cmd_ptr->next) {
         UpdateXPmObjectsInACmd(cmd_ptr);
      }
   } else {
      for (page_ptr=firstPage; page_ptr != NULL; page_ptr=page_ptr->next) {
         for (obj_ptr=page_ptr->bot; obj_ptr!=NULL; obj_ptr=obj_ptr->prev) {
            UpdateXPmObjects(obj_ptr);
         }
      }
   }
   old_color_str = (char**)malloc(maxColors*sizeof(char*));
   if (old_color_str == NULL) FailAllocMessage();
   old_allocated_colors = maxColors;
   for (i = 0; i < maxColors; i++) {
      old_color_str[i] =
            (char*)malloc((strlen(colorMenuItems[i])+1)*sizeof(char));
      if (old_color_str[i] == NULL) FailAllocMessage();
      strcpy(old_color_str[i], colorMenuItems[i]);
   }
   initColorDontReload = TRUE;
   CleanUpColors();
   XFreeColormap(mainDisplay, mainColormap);
   mainColormap = DefaultColormap(mainDisplay, mainScreen);
   XSetWindowColormap(mainDisplay, mainWindow, mainColormap);
   newColormapUsed = FALSE;
   saved_color_layers = colorLayers;
   InitColor();
   initColorDontReload = FALSE;
   colorLayers = saved_color_layers;

   ShowColor(TRUE);

   SaveStatusStrings();
   gnUpdatePixelObjCount = 0;
   if (nRedoing) {
      /* need to scan forward to pick up objects */
      struct CmdRec *cmd_ptr;

      for (page_ptr=firstPage; page_ptr != NULL; page_ptr=page_ptr->next) {
         for (obj_ptr=page_ptr->bot; obj_ptr!=NULL; obj_ptr=obj_ptr->prev) {
            if (UpdatePixel(obj_ptr, old_color_str)) {
               changed = TRUE;
            }
         }
      }
      for (cmd_ptr=CmdPtr->next; cmd_ptr != NULL; cmd_ptr=cmd_ptr->next) {
         if (UpdatePixelInACmd(cmd_ptr, old_color_str)) {
            changed = TRUE;
         }
      }
   } else {
      for (page_ptr=firstPage; page_ptr != NULL; page_ptr=page_ptr->next) {
         for (obj_ptr=page_ptr->bot; obj_ptr!=NULL; obj_ptr=obj_ptr->prev) {
            if (UpdatePixel(obj_ptr, old_color_str)) {
               changed = TRUE;
            }
         }
      }
   }
   RestoreStatusStrings();
   for (i = 0; i < old_allocated_colors; i++) free(old_color_str[i]);
   free(old_color_str);

   DestroySubMenu(MENU_COLOR);
   if (colorLayers) {
      RedrawColorWindow();
   }
   return changed;
}

static
void UndoOrRedoReplaceCmd(CmdPtr, HighLightSingleObj)
   struct CmdRec *CmdPtr;
   int HighLightSingleObj;
{
   register struct ObjRec *obj_ptr, *next_obj;
   register struct SelRec *sel_ptr;
   struct SelRec *saved_top_sel, *saved_bot_sel;
   struct ObjRec *saved_top_obj, *saved_bot_obj;
   int ltx, lty, rbx, rby, pos, count, *pos_table, max_count;
   int need_clear_and_redraw=FALSE;

   LinkJustTheObjects (CmdPtr->top_before, CmdPtr->bot_before);

   CopySel (CmdPtr->top_before, CmdPtr->count_before, &topSel, &botSel);

   if (CmdPtr->include_tgif_obj)
      AddObj (NULL, topObj, tgifObj);

   pos_table = (CmdPtr->undone) ? CmdPtr->pos_before : CmdPtr->pos_after;
   max_count = (CmdPtr->undone) ? CmdPtr->count_before : CmdPtr->count_after;

   sel_ptr = topSel;
   pos = count = 0;
   for (obj_ptr=topObj; obj_ptr != NULL; obj_ptr=next_obj, pos++)
   {
      next_obj = obj_ptr->next;
      if (pos == pos_table[count])
      {
         count++;
         sel_ptr->obj = obj_ptr;
         UnlinkObj (obj_ptr);
         sel_ptr = sel_ptr->next;
         if (count == max_count) break;
      }
   }
   UpdSelBBox ();
   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;

   saved_top_sel = topSel;
   saved_bot_sel = botSel;
   topSel = CmdPtr->top_before;
   botSel = CmdPtr->bot_before;
   CmdPtr->top_before = saved_top_sel;
   CmdPtr->bot_before = saved_bot_sel;

   saved_top_obj = topObj;
   saved_bot_obj = botObj;

   curPage->top = topObj = topSel->obj;
   curPage->bot = botObj = botSel->obj;

   AdjSplineVs ();
   AdjCaches ();

   curPage->top = topObj = saved_top_obj;
   curPage->bot = botObj = saved_bot_obj;

   pos_table = (CmdPtr->undone) ? CmdPtr->pos_after : CmdPtr->pos_before;
   max_count = (CmdPtr->undone) ? CmdPtr->count_after : CmdPtr->count_before;

   pos = count = 0;
   sel_ptr = topSel;
   for (obj_ptr=topObj; obj_ptr!=NULL; obj_ptr=next_obj, pos++)
   {
      if (pos == pos_table[count])
      {
         AddObj (obj_ptr->prev, obj_ptr, sel_ptr->obj);
         next_obj = obj_ptr;
         count++;
         sel_ptr = sel_ptr->next;
         if (count == max_count) break;
      }
      else
         next_obj = obj_ptr->next;
   }
   for ( ; sel_ptr != NULL; sel_ptr=sel_ptr->next)
      AddObj (botObj, NULL, sel_ptr->obj);

   if (CmdPtr->include_tgif_obj)
   {
      tgifObj = topObj;
      UnlinkObj (topObj);

      sel_ptr = topSel;
      topSel = topSel->next;
      if (topSel == NULL)
         botSel = NULL;
      else
         topSel->prev = NULL;
      free(sel_ptr);
   }
   if (CmdPtr->new_colormap) {
      if (RefreshColormap(CmdPtr->undone, CmdPtr)) {
         need_clear_and_redraw = TRUE;
      }
   }
   UpdSelBBox ();
   if (need_clear_and_redraw) {
      ClearAndRedrawDrawWindow();
   } else {
      RedrawAreas (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
            selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   }
   if (HighLightSingleObj && topSel != NULL) {
      if (!need_clear_and_redraw) {
         HighLightForward ();
      }
   } else {
      RemoveAllSel ();
   }
   SetFileModified (TRUE);
   justDupped = FALSE;
}

static
void UndoOrRedoOneToManyCmd(CmdPtr)
   struct CmdRec *CmdPtr;
{
   register struct ObjRec *obj_ptr, *next_obj;
   register struct SelRec *sel_ptr;
   struct ObjRec *saved_top_obj, *saved_bot_obj;
   int ltx, lty, rbx, rby, pos, count, *pos_table, max_count;
   int need_clear_and_redraw=FALSE;

   if (CmdPtr->include_tgif_obj)
   {
      AddObj (NULL, topObj, tgifObj);
      AddNewSelObj (topObj);
   }

   pos_table = (CmdPtr->undone) ? CmdPtr->pos_before : CmdPtr->pos_after;
   max_count = (CmdPtr->undone) ? CmdPtr->count_before : CmdPtr->count_after;

   sel_ptr = (CmdPtr->undone) ? CmdPtr->top_before : CmdPtr->top_after;
   pos = count = 0;
   for (obj_ptr=topObj; obj_ptr != NULL; obj_ptr=next_obj, pos++)
   {
      next_obj = obj_ptr->next;
      if (pos == pos_table[count])
      {
         count++;
         sel_ptr->obj = obj_ptr;
         UnlinkObj (obj_ptr);
         sel_ptr = sel_ptr->next;
         if (count == max_count) break;
      }
   }
   topSel = (CmdPtr->undone) ? CmdPtr->top_before : CmdPtr->top_after;
   botSel = (CmdPtr->undone) ? CmdPtr->bot_before : CmdPtr->bot_after;
   UpdSelBBox ();
   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;

   if (CmdPtr->undone)
      CopySel (CmdPtr->top_after, CmdPtr->count_after, &topSel, &botSel);
   else
      CopySel (CmdPtr->top_before, CmdPtr->count_before, &topSel, &botSel);

   LinkJustTheObjects (topSel, botSel);

   saved_top_obj = topObj;
   saved_bot_obj = botObj;

   curPage->top = topObj = topSel->obj;
   curPage->bot = botObj = botSel->obj;

   AdjSplineVs ();
   AdjCaches ();

   curPage->top = topObj = saved_top_obj;
   curPage->bot = botObj = saved_bot_obj;

   pos_table = (CmdPtr->undone) ? CmdPtr->pos_after : CmdPtr->pos_before;
   max_count = (CmdPtr->undone) ? CmdPtr->count_after : CmdPtr->count_before;

   pos = count = 0;
   sel_ptr = topSel;
   for (obj_ptr=topObj; obj_ptr!=NULL; obj_ptr=next_obj, pos++)
   {
      if (pos == pos_table[count])
      {
         AddObj (obj_ptr->prev, obj_ptr, sel_ptr->obj);
         next_obj = obj_ptr;
         count++;
         sel_ptr = sel_ptr->next;
         if (count == max_count) break;
      }
      else
         next_obj = obj_ptr->next;
   }
   for ( ; sel_ptr != NULL; sel_ptr=sel_ptr->next)
      AddObj (botObj, NULL, sel_ptr->obj);

   if (CmdPtr->include_tgif_obj)
   {
      tgifObj = topObj;
      UnlinkObj (topObj);

      sel_ptr = topSel;
      topSel = topSel->next;
      if (topSel == NULL)
         botSel = NULL;
      else
         topSel->prev = NULL;
      free(sel_ptr);
   }
   if (CmdPtr->new_colormap) {
      if (RefreshColormap(CmdPtr->undone, CmdPtr)) {
         need_clear_and_redraw = TRUE;
      }
   }
   UpdSelBBox ();
   if (need_clear_and_redraw) {
      ClearAndRedrawDrawWindow();
   } else {
      RedrawAreas (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1),
            ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1));
   }
   if (!need_clear_and_redraw) {
      HighLightForward ();
   }
   SetFileModified (TRUE);
   justDupped = FALSE;
}

static
void UndoOrRedoGotoPageCmd(CmdPtr)
   struct CmdRec *CmdPtr;
{
   int new_page_num;

   new_page_num = (CmdPtr->undone) ? CmdPtr->count_after : CmdPtr->count_before;

   GotoPageNum (new_page_num);
   ClearAndRedrawDrawWindow ();
   ShowPage ();
}

void UndoACmd(CmdPtr, HighLight)
   struct CmdRec *CmdPtr;
   int HighLight;
{
   struct CmdRec *cmd_ptr;

   if (topSel!=NULL) { HighLightReverse (); RemoveAllSel (); }
   switch (CmdPtr->type)
   {
      case CMD_COMPOSITE:
         if (CmdPtr->first->type==CMD_MOVE || CmdPtr->first->type==CMD_STRETCH)
            for (cmd_ptr=CmdPtr->last; cmd_ptr!=NULL; cmd_ptr=cmd_ptr->prev)
               UndoACmd (cmd_ptr, FALSE);
         else
            for (cmd_ptr=CmdPtr->last; cmd_ptr!=NULL; cmd_ptr=cmd_ptr->prev)
               UndoACmd (cmd_ptr, TRUE);
         break;
      case CMD_NEW: UndoNewCmd (CmdPtr); break;
      case CMD_DELETE: UndoDeleteCmd (CmdPtr); break;
      case CMD_MOVE: UndoOrRedoMoveCmd (CmdPtr); break;
      case CMD_STRETCH: UndoOrRedoReplaceCmd (CmdPtr, TRUE); break;
      case CMD_REPLACE: UndoOrRedoReplaceCmd (CmdPtr, HighLight); break;
      case CMD_ONE_TO_MANY: UndoOrRedoOneToManyCmd (CmdPtr); break;
      case CMD_MANY_TO_ONE: UndoOrRedoOneToManyCmd (CmdPtr); break;
      case CMD_GOTO_PAGE: UndoOrRedoGotoPageCmd (CmdPtr); break;
   }
   CmdPtr->undone = TRUE;
}

void UndoCmd ()
{
   TieLooseEnds ();
   SetCurChoice (NOTHING);
   if (curCmd == NULL)
   {
      Msg ("No commands to Undo!");
      return;
   }
   undoingOrRedoing = TRUE;
   UndoACmd (curCmd, TRUE);
   curCmd = curCmd->prev;
   undoingOrRedoing = FALSE;
}

void RedoACmd(CmdPtr, HighLight)
   struct CmdRec *CmdPtr;
   int HighLight;
{
   struct CmdRec *cmd_ptr;

   if (topSel!=NULL) { HighLightReverse (); RemoveAllSel (); }
   switch (CmdPtr->type)
   {
      case CMD_COMPOSITE:
         if (CmdPtr->first->type==CMD_MOVE || CmdPtr->first->type==CMD_STRETCH)
         {
            for (cmd_ptr=CmdPtr->first->next; cmd_ptr!=NULL;
                  cmd_ptr=cmd_ptr->next)
               RedoACmd (cmd_ptr, FALSE);
            RedoACmd (CmdPtr->first, TRUE);
         }
         else
            for (cmd_ptr=CmdPtr->first; cmd_ptr!=NULL; cmd_ptr=cmd_ptr->next)
               RedoACmd (cmd_ptr, TRUE);
         break;
      case CMD_NEW: RedoNewCmd (CmdPtr); break;
      case CMD_DELETE: RedoDeleteCmd (CmdPtr); break;
      case CMD_MOVE: UndoOrRedoMoveCmd (CmdPtr); break;
      case CMD_STRETCH: UndoOrRedoReplaceCmd (CmdPtr, TRUE); break;
      case CMD_REPLACE: UndoOrRedoReplaceCmd (CmdPtr, HighLight); break;
      case CMD_ONE_TO_MANY: UndoOrRedoOneToManyCmd (CmdPtr); break;
      case CMD_MANY_TO_ONE: UndoOrRedoOneToManyCmd (CmdPtr); break;
      case CMD_GOTO_PAGE: UndoOrRedoGotoPageCmd (CmdPtr); break;
   }
   CmdPtr->undone = FALSE;
}

void RedoCmd ()
{
   TieLooseEnds ();
   SetCurChoice (NOTHING);
   if (firstCmd==NULL || (curCmd!=NULL && curCmd->next==NULL))
   {
      Msg ("No commands to Redo!");
      return;
   }

   if (curCmd == NULL)
      curCmd = firstCmd;
   else
      curCmd = curCmd->next;

   undoingOrRedoing = TRUE;
   RedoACmd (curCmd, TRUE);
   undoingOrRedoing = FALSE;
}

struct CmdStkRec {
   struct CmdRec *first, *last, *cur;
   int history_count;
   struct CmdStkRec *next;
};

static struct CmdStkRec *topCompositeCmdStk=NULL;

void StartCompositeCmd()
{
   struct CmdStkRec *cmd_stk_ptr;

   if (historyDepth == 0) return;

   cmd_stk_ptr = (struct CmdStkRec *)malloc(sizeof(struct CmdStkRec));
   if (cmd_stk_ptr == NULL) FailAllocMessage();
   memset(cmd_stk_ptr, 0, sizeof(struct CmdStkRec));

   cmd_stk_ptr->next = topCompositeCmdStk;
   cmd_stk_ptr->first = firstCmd;
   cmd_stk_ptr->last = lastCmd;
   cmd_stk_ptr->cur = curCmd;
   cmd_stk_ptr->history_count = historyCount;
   topCompositeCmdStk = cmd_stk_ptr;

   firstCmd = lastCmd = curCmd = NULL;
   historyCount = 0;

   composingCommand = TRUE;
}

void EndCompositeCmd()
{
   struct CmdRec *composite_cmd=NULL;
   int empty=FALSE;

   if (historyDepth == 0) return;

   if (firstCmd == NULL)
      empty = TRUE;
   else
   {
      composite_cmd = (struct CmdRec *)malloc(sizeof(struct CmdRec));
      if (composite_cmd == NULL) FailAllocMessage();
      memset(composite_cmd, 0, sizeof(struct CmdRec));
      composite_cmd->type = CMD_COMPOSITE;
      composite_cmd->include_tgif_obj = FALSE;
      composite_cmd->first = firstCmd;
      composite_cmd->last = lastCmd;
      composite_cmd->top_before = composite_cmd->bot_before = NULL;
      composite_cmd->top_after = composite_cmd->bot_after = NULL;
   }

   if (topCompositeCmdStk != NULL) {
      struct CmdStkRec *cmd_stk_ptr;

      firstCmd = topCompositeCmdStk->first;
      lastCmd = topCompositeCmdStk->last;
      curCmd = topCompositeCmdStk->cur;
      historyCount = topCompositeCmdStk->history_count;

      cmd_stk_ptr = topCompositeCmdStk;
      topCompositeCmdStk = topCompositeCmdStk->next;
      free(cmd_stk_ptr);
   } else {
      firstCmd = lastCmd = curCmd = NULL;
      historyCount = 0;
   }

   if (!empty)
   {
      if (curCmd == NULL)
         ClearRedoRecords (firstCmd);
      else if (curCmd != lastCmd)
         ClearRedoRecords (curCmd);

      if (++historyCount == historyDepth)
      {
         struct CmdRec *new_first_cmd=firstCmd->next;

         new_first_cmd->prev = NULL;
         firstCmd->next = NULL;
         DeleteARedoRecord (firstCmd, (-1.0), (-1.0));
         historyCount--;
         firstCmd = new_first_cmd;
      }
      curCmd = composite_cmd;
      InsertCmd (lastCmd, NULL, curCmd);
   }
   composingCommand = (topCompositeCmdStk != NULL);
}

void RestoreDefaultHistoryDepth()
{
   CleanUpCmds();
   historyDepth = defaultHistoryDepth;
   historyCount = 0;
   firstCmd = lastCmd = curCmd = NULL;
}

void DisableUndo()
{
   CleanUpCmds();
   while (composingCommand) {
      EndCompositeCmd();
      CleanUpCmds();
   }
   historyDepth = 0;
}

void EnableUndo()
{
   RestoreDefaultHistoryDepth();
}

void PrepareStacking(TopSel, BotSel, NumObjs)
   struct SelRec *TopSel, *BotSel;
   int NumObjs;
{
   register int pos;
   register struct SelRec *sel_ptr;
   register struct ObjRec *obj_ptr;

   stackingPosition = (int*)malloc(NumObjs*sizeof(int));
   if (stackingPosition == NULL) FailAllocMessage();
   stackingCount = 0;
   pos = 0;
   sel_ptr = TopSel;
   for (obj_ptr=topObj; obj_ptr!=NULL; obj_ptr=obj_ptr->next, pos++)
      if (obj_ptr == sel_ptr->obj)
      {
         stackingPosition[stackingCount++] = pos;
         sel_ptr = sel_ptr->next;
         if (stackingCount == NumObjs) break;
      }
   if (sel_ptr != NULL || stackingCount != NumObjs)
   {
      Msg ("Possible fatal error.  Please read the terminal error output.");
      fprintf (stderr, "Select list not sorted in PrepareStacking().\n");
      fprintf (stderr, "\nUndo()/Redo() may crash %s.\n", TOOL_NAME);
      fprintf (stderr, "Safest thing to do is to save and exit.\n");
      fprintf (stderr, "Please try to reproduce this error and\n");
      fprintf (stderr, "\tsend bug report to william@cs.ucla.edu.\n");
      sprintf (gszMsgBox, "%s.\n%s %s.\n\n%s.\n\n%s %s.",
            "Select list not sorted in PrepareStacking()",
            "Undo()/Redo() may crash", TOOL_NAME,
            "Safest thing to do is to save and exit",
            "Please try to reproduce this error and",
            "send bug report to william@cs.ucla.edu");
      MsgBox (gszMsgBox, TOOL_NAME, STOP_MB);
      XFlush (mainDisplay);
      XSync (mainDisplay, False);
   }
}

void PrepareToRecord(CmdType, TopSel, BotSel, NumObjs)
   int CmdType, NumObjs;
   struct SelRec *TopSel, *BotSel;
{
   register struct SelRec *sel_ptr, *to_sel_ptr;

   if (historyDepth == 0) return;

   preparedColormap = (gnInImageProc ? mainColormap : None);

   topSelBeforeInCmd = botSelBeforeInCmd = NULL;
   stackingPosition = NULL;
   stackingCount = 0;
   switch (CmdType)
   {
      case CMD_NEW: break;

      case CMD_DELETE:
         PrepareStacking (TopSel, BotSel, NumObjs);
         DupTheseObjects (TopSel, BotSel, &topSelBeforeInCmd, &botSelBeforeInCmd);
         for (sel_ptr = TopSel, to_sel_ptr = topSelBeforeInCmd; to_sel_ptr!=NULL;
               sel_ptr=sel_ptr->next, to_sel_ptr=to_sel_ptr->next)
         {
            CopyObjId (sel_ptr->obj, to_sel_ptr->obj);
            CopyObjLocks (sel_ptr->obj, to_sel_ptr->obj);
         }
         break;

      case CMD_MOVE: break;

      case CMD_STRETCH:
         PrepareStacking (TopSel, BotSel, NumObjs);
         DupTheseObjects (TopSel, BotSel, &topSelBeforeInCmd, &botSelBeforeInCmd);
         for (sel_ptr = TopSel, to_sel_ptr = topSelBeforeInCmd; to_sel_ptr!=NULL;
               sel_ptr=sel_ptr->next, to_sel_ptr=to_sel_ptr->next)
         {
            CopyObjId (sel_ptr->obj, to_sel_ptr->obj);
            CopyObjLocks (sel_ptr->obj, to_sel_ptr->obj);
         }
         break;
      case CMD_REPLACE:
         PrepareStacking (TopSel, BotSel, NumObjs);
         DupTheseObjects (TopSel, BotSel, &topSelBeforeInCmd, &botSelBeforeInCmd);
         for (sel_ptr = TopSel, to_sel_ptr = topSelBeforeInCmd; to_sel_ptr!=NULL;
               sel_ptr=sel_ptr->next, to_sel_ptr=to_sel_ptr->next)
         {
            CopyObjId (sel_ptr->obj, to_sel_ptr->obj);
            CopyObjLocks (sel_ptr->obj, to_sel_ptr->obj);
         }
         break;
      case CMD_GOTO_PAGE:
         stackingCount = NumObjs;
         break;
   }
   return;
}

void FreeAfterSel(CmdPtr)
   struct CmdRec *CmdPtr;
{
   register struct SelRec *sel_ptr, *next_sel;

   for (sel_ptr=CmdPtr->top_after; sel_ptr!=NULL; sel_ptr=next_sel) {
      next_sel = sel_ptr->next;
      free(sel_ptr);
   }
   CmdPtr->top_after = CmdPtr->bot_after = NULL;
}

void RecordCmd(CmdType, SubCmdPtr, TopSel, BotSel, NumObjs)
   int CmdType, NumObjs;
   struct SelRec *TopSel, *BotSel;
   struct SubCmdRec *SubCmdPtr;
{
#ifdef _TGIF_WB
   if (ClientServer != 0)  /* connection is going on */
   {
      CheckConForRecord(CmdType, SubCmdPtr, TopSel, NumObjs, topSelBeforeInCmd);
      if (ClientServer == 1) return;  /* connection ON and I'm client */
   }
#endif /* _TGIF_WB */

   if (historyDepth == 0) return;

   if (curCmd == NULL)
      ClearRedoRecords (firstCmd);
   else if (curCmd != lastCmd)
      ClearRedoRecords (curCmd);

   if (++historyCount == historyDepth && !composingCommand)
   {
      struct CmdRec *new_first_cmd=firstCmd->next;

      new_first_cmd->prev = NULL;
      firstCmd->next = NULL;
      DeleteARedoRecord (firstCmd, (-1.0), (-1.0));
      historyCount--;
      firstCmd = new_first_cmd;
   }

   curCmd = (struct CmdRec *)malloc(sizeof(struct CmdRec));
   if (curCmd == NULL) FailAllocMessage();
   memset(curCmd, 0, sizeof(struct CmdRec));
   curCmd->top_before = topSelBeforeInCmd;
   curCmd->bot_before = botSelBeforeInCmd;
   curCmd->pos_before = stackingPosition;
   curCmd->count_before = stackingCount;
   curCmd->type = CmdType;
   curCmd->undone = FALSE;
   curCmd->include_tgif_obj = recordCmdIncludeTgifObj;
   curCmd->new_colormap = recordCmdUsesNewColormap;

   if (TopSel != NULL)
   {
      CopySel (TopSel, NumObjs, &(curCmd->top_after), &(curCmd->bot_after));
      PrepareStacking (TopSel, BotSel, NumObjs);
      curCmd->pos_after = stackingPosition;
      curCmd->count_after = stackingCount;
   }
   else
   {
      curCmd->top_after = curCmd->bot_after = NULL;
      curCmd->pos_after = NULL;
      curCmd->count_after = 0;
   }

   InsertCmd (lastCmd, NULL, curCmd);

   switch (CmdType)
   {
      case CMD_NEW: break;
      case CMD_DELETE: break;
      case CMD_MOVE:
         curCmd->dx = SubCmdPtr->detail.mv->dx;
         curCmd->dy = SubCmdPtr->detail.mv->dy;
         break;
      case CMD_STRETCH: FreeAfterSel (curCmd); break;
      case CMD_REPLACE: FreeAfterSel (curCmd); break;
      case CMD_ONE_TO_MANY: break;
      case CMD_MANY_TO_ONE: break;
      case CMD_GOTO_PAGE: curCmd->count_after = NumObjs; break;
   }
   curCmd = lastCmd;
}

void AbortPrepareCmd(CmdType)
   int CmdType;
{
   register struct SelRec *sel_ptr, *next_sel;

   if (historyDepth == 0) return;

   for (sel_ptr=topSelBeforeInCmd; sel_ptr!=NULL; sel_ptr=next_sel)
   {
      next_sel = sel_ptr->next;
      switch (CmdType)
      {
         case CMD_REPLACE: FreeObj (sel_ptr->obj); break;
      }
      free(sel_ptr);
   }
   if (stackingPosition != NULL) free(stackingPosition);
   stackingPosition = NULL;
   stackingCount = 0;
   topSelBeforeInCmd = botSelBeforeInCmd = NULL;
}

void RecordNewObjCmd()
{
   if (historyDepth == 0) return;

   if (topSel==NULL)
   {
      struct SelRec *sel_ptr;

      sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (sel_ptr == NULL) FailAllocMessage();
      sel_ptr->next = sel_ptr->prev = NULL;
      sel_ptr->obj = topObj;
      PrepareToRecord (CMD_NEW, NULL, NULL, 0);
      RecordCmd (CMD_NEW, NULL, sel_ptr, sel_ptr, 1);
      free(sel_ptr);
   }
   else
   {
      PrepareToRecord (CMD_NEW, NULL, NULL, 0);
      RecordCmd (CMD_NEW, NULL, topSel, botSel, 1);
   }
}

void PrepareToReplaceAnObj(BeforeObjPtr)
   struct ObjRec *BeforeObjPtr;
{
   struct SelRec *sel_ptr;

   if (historyDepth == 0) return;

   sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (sel_ptr == NULL) FailAllocMessage();
   sel_ptr->next = sel_ptr->prev = NULL;
   sel_ptr->obj = BeforeObjPtr;
   PrepareToRecord (CMD_REPLACE, sel_ptr, sel_ptr, 1);
   free(sel_ptr);
}

void RecordReplaceAnObj(AfterObjPtr)
   struct ObjRec *AfterObjPtr;
{
   struct SelRec *sel_ptr;

   if (historyDepth == 0) return;

   sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (sel_ptr == NULL) FailAllocMessage();
   sel_ptr->next = sel_ptr->prev = NULL;
   sel_ptr->obj = AfterObjPtr;
   RecordCmd (CMD_REPLACE, NULL, sel_ptr, sel_ptr, 1);
   free(sel_ptr);
}

void ChangeReplaceOneCmdToDeleteCmd()
{
   RecordCmd(CMD_DELETE, NULL, NULL, NULL, 0);
}

