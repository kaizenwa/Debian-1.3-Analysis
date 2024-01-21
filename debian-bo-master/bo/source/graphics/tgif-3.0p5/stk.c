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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/stk.c,v 3.1 1996/05/14 12:56:01 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "align.e"
#include "attr.e"
#include "box.e"
#include "button.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "mainmenu.e"
#include "mark.e"
#include "menu.e"
#include "msg.e"
#include "names.e"
#include "navigate.e"
#include "obj.e"
#include "page.e"
#include "pattern.e"
#include "raster.e"
#include "rect.e"
#include "ruler.e"
#include "scroll.e"
#include "select.e"
#include "setup.e"
#ifndef _NO_EXTERN
#include "stk.e"
#endif
#include "text.e"

struct ObjRec	*tgifObj=NULL;
struct StkRec	*topStk=NULL;

int AncesterModified()
{
   struct StkRec *stk_ptr;

   for (stk_ptr = topStk; stk_ptr != NULL; stk_ptr = stk_ptr->next) {
      if (stk_ptr->file_mod) {
         return TRUE;
      }
   }
   return FALSE;
}

void InitTgifObj()
{
   tgifObj = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (tgifObj == NULL) FailAllocMessage();
   memset(tgifObj, 0, sizeof(struct ObjRec));
   tgifObj->type = OBJ_BOX;
   tgifObj->x = tgifObj->y = 0;
   tgifObj->color = 0;
   tgifObj->id = (-1);
   tgifObj->dirty = FALSE;
   tgifObj->rotation = 0;
   tgifObj->marked = tgifObj->locked = FALSE;
   tgifObj->obbox.ltx = tgifObj->obbox.lty = 0;
   tgifObj->obbox.rbx = tgifObj->obbox.rby = 0;
   tgifObj->bbox.ltx = tgifObj->bbox.lty = 0;
   tgifObj->bbox.rbx = tgifObj->bbox.rby = 0;
   tgifObj->next = tgifObj->prev = NULL;
   tgifObj->fattr = tgifObj->lattr = NULL;
   tgifObj->detail.b = (struct BoxRec *)malloc(sizeof(struct BoxRec));
   if (tgifObj->detail.b == NULL) FailAllocMessage();
   memset(tgifObj->detail.b, 0, sizeof(struct BoxRec));
   tgifObj->detail.b->fill = NONEPAT;
   tgifObj->detail.b->pen = NONEPAT;
   tgifObj->detail.b->width = 0;
   tgifObj->detail.b->dash = 0;
}

void InitStk()
{
   curSymDir[0] = '\0';
   InitTgifObj();
}

struct StkRec *SaveFileInfo()
{
   struct StkRec *stk_ptr;

   stk_ptr = (struct StkRec *)malloc(sizeof(struct StkRec));
   if (stk_ptr == NULL) FailAllocMessage();
   memset(stk_ptr, 0, sizeof(struct StkRec));

   stk_ptr->next = topStk;
   stk_ptr->sel = (topSel == NULL ? NULL : topSel->obj);
   stk_ptr->first = topObj;
   stk_ptr->last = botObj;
   stk_ptr->file_mod = fileModified;
   stk_ptr->id = objId;
   stk_ptr->page_style = pageStyle;

   stk_ptr->orig_x = drawOrigX;
   stk_ptr->orig_y = drawOrigY;
   stk_ptr->zoom = zoomScale;
   stk_ptr->zoomed = zoomedIn;
   stk_ptr->grid_system = gridSystem;
   stk_ptr->english_grid = xyEnglishGrid;
   stk_ptr->metric_grid = xyMetricGrid;
   stk_ptr->grid_on = gridOn;
   stk_ptr->color = colorIndex;
   stk_ptr->h_align = horiAlign;
   stk_ptr->v_align = vertAlign;
   stk_ptr->line_w = lineWidth;
   stk_ptr->line_s = lineStyle;
   stk_ptr->fill = objFill;
   stk_ptr->pen = penPat;
   stk_ptr->just = textJust;
   stk_ptr->v_space = textVSpace;
   stk_ptr->font = curFont;
   stk_ptr->f_style = curStyle;
   stk_ptr->f_size = curSize;
   stk_ptr->print_mag = printMag;
   stk_ptr->grid_shown = gridShown;
   stk_ptr->move_mode = moveMode;
   stk_ptr->text_rotate = curRotate;
   stk_ptr->rcb_radius = rcbRadius;
   stk_ptr->underline_on = curUnderlineOn;
   stk_ptr->underline = curUnderline;
   stk_ptr->first_file_attr = tgifObj->fattr;
   stk_ptr->last_file_attr = tgifObj->lattr;

   stk_ptr->first_page = firstPage;
   stk_ptr->last_page = lastPage;
   stk_ptr->cur_page = curPage;
   stk_ptr->cur_page_num = curPageNum;
   stk_ptr->last_page_num = lastPageNum;
   stk_ptr->cols = paperCol;
   stk_ptr->rows = paperRow;
   stk_ptr->page_layout_mode = pageLayoutMode;

   stk_ptr->color_dump = colorDump;
   stk_ptr->one_page_width = onePageWidth;
   stk_ptr->one_page_height = onePageHeight;

   stk_ptr->first_cmd = firstCmd;
   stk_ptr->last_cmd = lastCmd;
   stk_ptr->cur_cmd = curCmd;
   stk_ptr->history_count = historyCount;

   strcpy(stk_ptr->dir, curDir);
   stk_ptr->name_valid = curFileDefined;
   if (stk_ptr->name_valid) strcat(stk_ptr->name, curFileName);
   strcpy(stk_ptr->sym_dir, curSymDir);
   strcpy(stk_ptr->domain, curDomainName);

   stk_ptr->saved_comments = savedComments;
   stk_ptr->saved_comments_len = savedCommentsLen;

   return stk_ptr;
}

void ResetFileInfo()
{
   tgifObj->fattr = NULL;
   tgifObj->lattr = NULL;

   firstCmd = lastCmd = curCmd = NULL;
   historyCount = 0;

   topObj = botObj = NULL;
   firstPage = lastPage = curPage = NULL;
   lastPageNum = 0;
   InitPage();

   savedComments = NULL;
   savedCommentsLen = 0;

   if (usePaperSizeStoredInFile) ResetOnePageSize();
}

int PushIcon()
{
   struct StkRec *stk_ptr;
   struct ObjRec *obj_ptr;
   char sym_name[MAXPATHLENGTH], path_name[MAXPATHLENGTH];
   char file_name[MAXPATHLENGTH], s[MAXPATHLENGTH], * rest;
   int read_status, short_name;
   FILE *fp;
   int tmp_linenum, interrupted;
   char tmp_filename[MAXPATHLENGTH];

   if (topSel == NULL || topSel != botSel || topSel->obj->type != OBJ_ICON) {
      MsgBox("Please select one ICON object to push into.", TOOL_NAME, INFO_MB);
      return FALSE;
   }

   strcpy(sym_name, topSel->obj->detail.r->s);
   if (!GetSymbolPath(sym_name, path_name)) {
      sprintf(gszMsgBox, "Can not find '%s.%s' in %s", sym_name, SYM_FILE_EXT,
            curDomainPath);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }

   strcat(sym_name, ".");
   strcat(sym_name, SYM_FILE_EXT);
   sprintf(file_name, "%s/%s", path_name, sym_name);

   if ((short_name=IsPrefix(bootDir, file_name, &rest))) ++rest;

   if ((fp=fopen(file_name, "r")) == NULL) {
      if (short_name) {
         sprintf(gszMsgBox, "Can not open '%s', icon not pushed into.", rest);
      } else {
         sprintf(gszMsgBox, "Can not open '%s', icon not pushed into.",
               file_name);
      }
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }

   strcpy(tmp_filename, scanFileName);
   tmp_linenum = scanLineNum;
   strcpy(scanFileName, (short_name ? rest : file_name));
   scanLineNum = 0;

   HighLightReverse();

   stk_ptr = SaveFileInfo();
   ResetFileInfo();

   topStk = stk_ptr;

   if (short_name) {
      sprintf(gszMsgBox, "Pushing into '%s' ...", rest);
   } else {
      sprintf(gszMsgBox, "Pushing into '%s' ...", file_name);
   }
   Msg(gszMsgBox);
   CleanUpDrawingWindow();
   XClearWindow(mainDisplay, drawWindow);
   somethingHighLighted = FALSE;
   SetFileModified(FALSE);

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   numRedrawBBox = 0;
   ShowInterrupt(1);
   interrupted = FALSE;
   foundGoodStateObject = FALSE;
   while ((read_status=ReadObj(fp, &obj_ptr)) == TRUE) {
      if (obj_ptr != NULL) {
         obj_ptr->tmp_parent = NULL;
         AdjForOldVersion(obj_ptr);
         AddObj(NULL, topObj, obj_ptr);
         if (!interrupted &&
               (PointInBBox(obj_ptr->x, obj_ptr->y, drawWinBBox) ||
               BBoxIntersect(obj_ptr->bbox, drawWinBBox))) {
            if (!DrawObj(drawWindow, obj_ptr)) interrupted = TRUE;
            if (CheckInterrupt()) interrupted = TRUE;
         }
      }
   }
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   if (interrupted) Msg("User interrupt.  Drawing aborted.");
   HideInterrupt();

   strcpy(scanFileName, tmp_filename);
   scanLineNum = tmp_linenum;

   fclose(fp);
   SetDefaultCursor(mainWindow);
   SetDefaultCursor(drawWindow);

   if (read_status == INVALID) {
      sprintf(gszMsgBox, "File version too large (=%1d).  Push aborted!",
            fileVersion);
      Msg(gszMsgBox);
      DrawPaperBoundary(drawWindow);
      RedrawGridLines(drawWindow);
      return FALSE;
   }

   strcpy(curFileName, sym_name);
   strcpy(curSymDir, path_name);
   curFileDefined = TRUE;
   if (!curDirIsLocal) {
      strcpy(curDir, curLocalDir);
      *curLocalDir = '\0';
      curDirIsLocal = TRUE;
   }

   if (short_name) {
      sprintf(gszMsgBox, "Current file is '%s'.", rest);
   } else {
      sprintf(gszMsgBox, "Current file is '%s'.", file_name);
   }
   Msg(gszMsgBox);

   if (lastPageNum != 1) {
      TwoLineMsg("Error!  Multipage symbol file detected.",
                  "    Please exit as soon as possible.");
   }
   RedrawTitleWindow();
   UpdateAllSubMenus();
   if (foundGoodStateObject && !navigatingBackAndForth) CommitNavigate();
   return foundGoodStateObject;
}

void RestoreFileInfo(StkPtr)
   struct StkRec *StkPtr;
{
   char fname[MAXPATHLENGTH], *rest;

   fileModified = StkPtr->file_mod;
   objId = StkPtr->id;

   CleanUpComments();
   CleanUpDrawingWindow();

   drawOrigX = StkPtr->orig_x;
   drawOrigY = StkPtr->orig_y;
   zoomScale = StkPtr->zoom;
   zoomedIn = StkPtr->zoomed;
   gridSystem = StkPtr->grid_system;
   xyEnglishGrid = StkPtr->english_grid;
   xyMetricGrid = StkPtr->metric_grid;
   gridOn = StkPtr->grid_on;
   colorIndex = StkPtr->color;
   horiAlign = StkPtr->h_align;
   vertAlign = StkPtr->v_align;
   lineWidth = StkPtr->line_w;
   lineStyle = StkPtr->line_s;
   objFill = StkPtr->fill;
   penPat = StkPtr->pen;
   textJust = StkPtr->just;
   textVSpace = StkPtr->v_space;
   curFont = StkPtr->font;
   curStyle = StkPtr->f_style;
   curSize = StkPtr->f_size;
   printMag = StkPtr->print_mag;
   gridShown = StkPtr->grid_shown;
   moveMode = StkPtr->move_mode;
   curRotate = StkPtr->text_rotate;
   rcbRadius = StkPtr->rcb_radius;
   curUnderlineOn = StkPtr->underline_on;
   curUnderline = StkPtr->underline;
   tgifObj->fattr = StkPtr->first_file_attr;
   tgifObj->lattr = StkPtr->last_file_attr;

   CleanUpCmds();
   firstCmd = StkPtr->first_cmd;
   lastCmd = StkPtr->last_cmd;
   curCmd = StkPtr->cur_cmd;
   historyCount = StkPtr->history_count;

   firstPage = StkPtr->first_page;
   lastPage = StkPtr->last_page;
   curPage = StkPtr->cur_page;
   curPageNum = StkPtr->cur_page_num;
   lastPageNum = StkPtr->last_page_num;
   paperCol = StkPtr->cols;
   paperRow = StkPtr->rows;
   pageLayoutMode = StkPtr->page_layout_mode;

   colorDump = StkPtr->color_dump;
   onePageWidth = StkPtr->one_page_width;
   onePageHeight = StkPtr->one_page_height;

   if (usePaperSizeStoredInFile) SetPSPageWidthHeight();

   curPage->top = topObj = StkPtr->first;
   curPage->bot = botObj = StkPtr->last;
   strcpy(curDomainName, StkPtr->domain);
   strcpy(curSymDir, StkPtr->sym_dir);
   curFileDefined = StkPtr->name_valid;
   if (curFileDefined) {
      strcpy(curFileName, StkPtr->name);
      if (*curSymDir == '\0') {
         sprintf(fname, "%s/%s", StkPtr->dir, curFileName);
      } else {
         sprintf(fname, "%s/%s", curSymDir, curFileName);
      }
      if (IsPrefix(bootDir, fname, &rest)) {
         sprintf(gszMsgBox, "Pop back to '%s'.", ++rest);
      } else {
         sprintf(gszMsgBox, "Pop back to '%s'.", fname);
      }
      Msg(gszMsgBox);

      SetCurDir(fname);
   } else {
      sprintf(gszMsgBox,
            "Poping back to parent level.  Current file undefined.");
      Msg(gszMsgBox);
   }

   if (strcmp(curDir, StkPtr->dir) != 0) {
      strcpy(curDir, StkPtr->dir);
      UpdateDirInfo();
   }

   savedComments = StkPtr->saved_comments;
   savedCommentsLen = StkPtr->saved_comments_len;
}

void PopIcon()
{
   if (topStk == NULL) {
      MsgBox("Already at top level.", TOOL_NAME, INFO_MB);
      return;
   }
   while (fileModified) {
      switch (MsgBox("File modified, save file before quit? [ync](y)",
            TOOL_NAME, YNC_MB)) {
      case MB_ID_YES: SaveFile(); break;
      case MB_ID_NO: SetFileModified(FALSE); break;
      case MB_ID_CANCEL: return;
      }
   }
   AdjustNavigate();
   RestoreFileInfo(topStk);
   ResetOnePageSize();

   topSel = botSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (topSel == NULL) FailAllocMessage();
   topSel->next = NULL;
   topSel->prev = NULL;
   topSel->obj = topStk->sel;
   UpdSelBBox();

   if (UpdPageStyle(topStk->page_style)) {
      UpdDrawWinBBox();
      AdjSplineVs();
   }
   UpdDrawWinWH();
   SetCanvasFont();
   RedrawRulers();
   RedrawScrollBars();
   RedrawChoiceWindow();
   RedrawTitleWindow();
   UpdDrawWinBBox();
   UpdateAllSubMenus();

   free(topStk);
   topStk = topStk->next;
   ClearAndRedrawDrawWindow();
   XSync(mainDisplay, True);
   justDupped = FALSE;
}

void CleanUpTgifObj()
{
   if (tgifObj != NULL) {
      DelAllAttrs(tgifObj->fattr);
      FreeBoxObj(tgifObj);
   }
   tgifObj = NULL;
}

void CleanUpStk()
{
   register struct StkRec *next_stk;

   for ( ; topStk != NULL; topStk = next_stk) {
      next_stk = topStk->next;
      firstPage = topStk->first_page;
      lastPage = topStk->last_page;
      CleanUpPage();
      free(topStk);
   }
   curSymDir[0] = '\0';
   CleanUpTgifObj();
}
