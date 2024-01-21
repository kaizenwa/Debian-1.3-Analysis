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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/cutpaste.c,v 3.0 1996/05/06 16:04:23 william Exp $";
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "auxtext.e"
#include "box.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#ifndef _NO_EXTERN
#include "cutpaste.e"
#endif
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "mark.e"
#include "move.e"
#include "msg.e"
#include "names.e"
#include "obj.e"
#include "page.e"
#include "pattern.e"
#include "remote.e"
#include "select.e"
#include "setup.e"
#include "special.e"
#include "text.e"

extern char	* mktemp ARGS_DECL((char *Template));
#ifndef _NO_EXTERN
extern int	write ARGS_DECL((int fd, char *buf, int nbyte));
extern int	unlink ARGS_DECL((char *));
extern int	read ARGS_DECL((int fd, char *buf, int nbyte));
#endif

int	copyingToCutBuffer=FALSE;
int	pastingFile=FALSE;

static char *cutBuffer=NULL;

int CopyToCutBuffer()
   /* returns FALSE if copying in text mode -- this is */
   /*    interpreted as an attempt to copy highlighted text */
{
   FILE *fp;
   register struct SelRec *sel_ptr;
   struct ObjRec *obj_ptr, *top_obj, *bot_obj;
   char tmpfile[MAXSTRING];
   struct stat stat;
   unsigned char header=TGIF_HEADER;
    
   if (curChoice == DRAWTEXT) {
      XEvent ev;

      copyInDrawTextMode = TRUE;
      ev.type = KeyPress;
      DrawText(&ev);
      return FALSE;
   }
   if (topSel == NULL) {
      MsgBox("No object selected for the COPY operation.", TOOL_NAME, INFO_MB);
      return TRUE;
   }
   sprintf(tmpfile, "%sTgifXXXXXX", TMP_DIR);
   mktemp(tmpfile);
   if ((fp=fopen(tmpfile, "w+")) == NULL) {
      sprintf(gszMsgBox, "Can not open %s.", tmpfile);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return TRUE;
   }
   writeFileFailed = FALSE;
   if (write(fileno(fp), (char*)(&header), 1) < 1) writeFileFailed = TRUE;

   top_obj = bot_obj = NULL;
   for (sel_ptr = botSel; sel_ptr != NULL; sel_ptr = sel_ptr->prev) {
      obj_ptr = DupObj(sel_ptr->obj);

      obj_ptr->prev = NULL;
      obj_ptr->next = top_obj;

      if (top_obj == NULL) {
         bot_obj = obj_ptr;
      } else {
         top_obj->prev = obj_ptr;
      }
      top_obj = obj_ptr;
   }
   Save(fp, bot_obj, 0, 1);
   while (top_obj != NULL) {
      obj_ptr = top_obj->next;
      FreeObj(top_obj);
      top_obj = obj_ptr;
   }
   top_obj = bot_obj = NULL;
   if (writeFileFailed) {
      sprintf(gszMsgBox, "Fail to write to '%s'.  File system may be full.",
            tmpfile);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      fclose(fp);
      unlink(tmpfile);
      writeFileFailed = FALSE;
      return TRUE;
   }

   fflush(fp);
   if (fstat(fileno(fp), &stat) < 0) {
      fclose(fp);
      unlink(tmpfile);
      sprintf(gszMsgBox, "FSTAT error in %s.  Copy aborted!", tmpfile);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return TRUE;
   }

   if (cutBuffer != NULL) free(cutBuffer);
   cutBuffer = (char*)malloc((stat.st_size+1)*sizeof(char));
   if (cutBuffer == NULL) FailAllocMessage();

   rewind(fp);
   if (read(fileno(fp), cutBuffer, stat.st_size) < stat.st_size) {
      sprintf(gszMsgBox, "READ error in %s.  Copy aborted!", tmpfile);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   } else {
      int copy_failed = FALSE;

      copyingToCutBuffer = TRUE;
      XStoreBytes(mainDisplay, cutBuffer, stat.st_size);
      XSync(mainDisplay, False);
      if (copyingToCutBuffer == INVALID) {
         sprintf(gszMsgBox, "%s  %s", "Copy to cut buffer fails.",
               "Selected object(s) may be too big.");
         copy_failed = TRUE;
      } else {
         sprintf(gszMsgBox, "Copy buffer updated.");
      }
      copyingToCutBuffer = FALSE;
      Msg(gszMsgBox);
      if (copy_failed) {
         *cutBuffer = '\0';
         XStoreBytes(mainDisplay, cutBuffer, 1);
      }
   }
   fclose(fp);
   unlink(tmpfile);
   return TRUE;
}

void CutToCutBuffer()
{
   if (curChoice == DRAWTEXT && textCursorShown) {
      CopyToCutBuffer();
      DeleteHighLightedText();
   }
   if (curChoice == NOTHING && topSel != NULL) {
      CopyToCutBuffer();
      DelAllSelObj();
   }
}

void PasteString(CutBuffer)
   char *CutBuffer;
{
   register char *c_ptr, *dest_c_ptr;
   int x, y, num_lines, char_count, root_x, root_y, grid_x, grid_y;
   int max_len=0, min_lbearing=0, max_rextra = 0;
   unsigned int status;
   struct StrRec *first_str, *last_str, *str_ptr;
   struct ObjRec *obj_ptr;
   struct TextRec *text_ptr;
   Window root_win, child_win;

   if (*CutBuffer == '\0') {
      MsgBox("Cut buffer is empty", TOOL_NAME, INFO_MB);
      return;
   }

   TieLooseEnds();
   SetCurChoice(NOTHING);
   if (topSel!=NULL) { HighLightReverse(); RemoveAllSel(); }

   XQueryPointer(mainDisplay, drawWindow, &root_win, &child_win,
         &root_x, &root_y, &x, &y, &status);
   GridXY(x, y, &grid_x, &grid_y);

   text_ptr = (struct TextRec *)malloc(sizeof(struct TextRec));
   if (text_ptr == NULL) FailAllocMessage();
   memset(text_ptr, 0, sizeof(struct TextRec));
   CopyCurInfoToTextPtr(text_ptr);

   first_str = last_str = NULL;
   for (c_ptr=CutBuffer, num_lines = 0; *c_ptr != '\0'; num_lines++) {
      char s[MAXSTRING+1];
      int w, lbearing, rextra;

      str_ptr = NewStr();
      if (str_ptr == NULL) FailAllocMessage();

      char_count = 0;
      dest_c_ptr = s;
      while (*c_ptr != '\0' && *c_ptr != '\n' && *c_ptr != '\r') {
         *dest_c_ptr++ = *c_ptr++;
         if (++char_count == MAXSTRING) {
            sprintf(gszMsgBox, "String length exceeds %1d.  %s.",
                  MAXSTRING, "String truncated");
            Msg(gszMsgBox);
            while (*c_ptr != '\0' && *c_ptr != '\n' && *c_ptr != '\r') c_ptr++;
            break;
         }
      }
      *dest_c_ptr = '\0';
      DynStrSet(&str_ptr->dyn_str, s);

      str_ptr->prev = last_str;
      str_ptr->next = NULL;
      if (last_str == NULL) {
         first_str = str_ptr;
      } else {
         last_str->next = str_ptr;
      }
      last_str = str_ptr;

      GetStrSizeInfo(str_ptr, &w, &lbearing, &rextra);
      if (w > max_len) max_len = w;
      if (lbearing < min_lbearing) min_lbearing = lbearing;
      if (rextra > max_rextra) max_rextra = rextra;
      while (*c_ptr == '\n' || *c_ptr == '\r') c_ptr++;
   }

   text_ptr->lines = num_lines;
   text_ptr->first = first_str;
   text_ptr->last = last_str;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));
   obj_ptr->x = grid_x;
   obj_ptr->y = grid_y;
   obj_ptr->type = OBJ_TEXT;
   obj_ptr->color = colorIndex;
   obj_ptr->id = objId++;;
   obj_ptr->dirty = FALSE;
   obj_ptr->rotation = 0;
   obj_ptr->detail.t = text_ptr;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;

   SetTextBBox(obj_ptr, textJust, max_len,
         num_lines*textCursorH+(num_lines-1)*textVSpace,
         min_lbearing, max_rextra, curRotate);
   AdjObjBBox(obj_ptr);
   PlaceTopObj(obj_ptr);

   AddObj(NULL, topObj, obj_ptr);
   SelectTopObj();
   RecordNewObjCmd();
   SetFileModified(TRUE);
   justDupped = FALSE;
}

static
struct ObjRec *CreateTmpBoxObj(LtX, LtY, RbX, RbY)
   int LtX, LtY, RbX, RbY;
{
   register struct BoxRec *box_ptr;
   register struct ObjRec *obj_ptr;

   box_ptr = (struct BoxRec *)malloc(sizeof(struct BoxRec));
   if (box_ptr == NULL) FailAllocMessage();
   memset(box_ptr, 0, sizeof(struct BoxRec));
   box_ptr->fill = NONEPAT;
   box_ptr->width = 0;
   box_ptr->pen = NONEPAT;
   box_ptr->dash = 0;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));

   obj_ptr->bbox.ltx = obj_ptr->obbox.ltx = obj_ptr->x = LtX;
   obj_ptr->bbox.lty = obj_ptr->obbox.lty = obj_ptr->y = LtY;
   obj_ptr->bbox.rbx = obj_ptr->obbox.rbx = RbX;
   obj_ptr->bbox.rby = obj_ptr->obbox.rby = RbY;
   obj_ptr->type = OBJ_BOX;
   obj_ptr->color = colorIndex;
   obj_ptr->id = 0;
   obj_ptr->dirty = FALSE;
   obj_ptr->rotation = 0;
   obj_ptr->detail.b = box_ptr;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;
   return obj_ptr;
}

void AssignNewObjIds(ObjPtr)
   struct ObjRec *ObjPtr;
{
   register struct ObjRec *obj_ptr;
   register struct AttrRec *attr_ptr;

   ObjPtr->id = objId++;
   switch (ObjPtr->type) {
   case OBJ_GROUP:
   case OBJ_SYM:
   case OBJ_ICON:
      for (obj_ptr=ObjPtr->detail.r->first; obj_ptr != NULL;
            obj_ptr=obj_ptr->next) {
         AssignNewObjIds(obj_ptr);
      }
      break;
   default: break;
   }
   for (attr_ptr=ObjPtr->fattr; attr_ptr != NULL; attr_ptr=attr_ptr->next) {
      AssignNewObjIds(attr_ptr->obj);
   }
}

char *FetchCutBuffer(pn_buf_sz)
   int *pn_buf_sz;
{
   char *cut_buffer=(char*)XFetchBytes(mainDisplay, pn_buf_sz);

   return cut_buffer;
}

int PasteFromCutBuffer()
   /* returns FALSE if pasting in text mode and non-tgif bytes are */
   /*    in the cut buffer -- this is interpreted as an attempt to */
   /*    paste into the current text */
{
   FILE *fp;
   int len, ltx, lty, rbx, rby, dx, dy, read_status;
   char tmpfile[MAXSTRING], *cut_buffer, *orig_cut_buffer;
   unsigned char header=TGIF_HEADER;
   struct ObjRec *obj_ptr, *saved_top_obj, *saved_bot_obj, *tmp_obj;
   struct ObjRec *tmp_top_obj, *tmp_bot_obj;

   orig_cut_buffer = cut_buffer = (char*)FetchCutBuffer(&len);
   if (len == 0) {
      MsgBox("Cut buffer is empty", TOOL_NAME, INFO_MB);
      return TRUE;
   }
   if (((unsigned char)(*cut_buffer)) != header) {
      if (curChoice == DRAWTEXT) {
         XEvent ev;

         XFree(cut_buffer);
         pasteInDrawTextMode = TRUE;
         ev.type = KeyPress;
         DrawText(&ev);
         return FALSE;
      }
      Msg("Paste from a non-tgif tool.");
      PasteString(cut_buffer);
      XFree(cut_buffer);
      return TRUE;
   }
   cut_buffer++;
   len--;

   sprintf(tmpfile, "%sTgifXXXXXX", TMP_DIR);
   mktemp(tmpfile);
   if ((fp=fopen(tmpfile, "w+")) == NULL) {
      sprintf(gszMsgBox, "Can not open %s for write.", tmpfile);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      XFree(orig_cut_buffer);
      return TRUE;
   }
   writeFileFailed = FALSE;
   if (write(fileno(fp), cut_buffer, len) < len) {
      fclose(fp);
      unlink(tmpfile);
      sprintf(gszMsgBox, "FWRITE error in writing to '%s'.  Paste aborted!",
            tmpfile);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      XFree(orig_cut_buffer);
      return TRUE;
   }
   fflush(fp);
   rewind(fp);
   XFree(orig_cut_buffer);

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);

   TieLooseEnds();
   SetCurChoice(NOTHING);
   if (topSel!=NULL) { HighLightReverse(); RemoveAllSel(); }

   saved_top_obj = topObj;
   saved_bot_obj = botObj;
   curPage->top = topObj = NULL;
   curPage->bot = botObj = NULL;
    
   importingFile = TRUE;
   pastingFile = TRUE;
   readingPageNum = 0;
   while ((read_status=ReadObj(fp, &obj_ptr)) == TRUE) {
      if (obj_ptr != NULL) {
         AdjForOldVersion(obj_ptr);
         UnlockAnObj(obj_ptr);
         AssignNewObjIds(obj_ptr);
         AddObj(NULL, topObj, obj_ptr);
      }
   }
   fclose(fp);
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   importingFile = FALSE;
   pastingFile = FALSE;
   unlink(tmpfile);
   SetDefaultCursor(mainWindow);
   SetDefaultCursor(drawWindow);

   if (read_status == INVALID) {
      sprintf(gszMsgBox, "File version too large (=%1d).  Paste aborted!",
            fileVersion);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return TRUE;
   }
    
   if (topObj != NULL) SetFileModified(TRUE);

   ltx = topObj->obbox.ltx;
   lty = topObj->obbox.lty;
   rbx = topObj->obbox.rbx;
   rby = topObj->obbox.rby;
   for (obj_ptr = topObj->next; obj_ptr != NULL; obj_ptr = obj_ptr->next) {
      if (obj_ptr->obbox.ltx < ltx) ltx = obj_ptr->obbox.ltx;
      if (obj_ptr->obbox.lty < lty) lty = obj_ptr->obbox.lty;
      if (obj_ptr->obbox.rbx > rbx) rbx = obj_ptr->obbox.rbx;
      if (obj_ptr->obbox.rby > rby) rby = obj_ptr->obbox.rby;
   }
   tmp_obj = CreateTmpBoxObj(ltx, lty, rbx, rby);

   tmp_top_obj = topObj;
   tmp_bot_obj = botObj;
   curPage->top = topObj = NULL;
   curPage->bot = botObj = NULL;
   PlaceTopObj(tmp_obj);
   curPage->top = topObj = tmp_top_obj;
   curPage->bot = botObj = tmp_bot_obj;

   dx = tmp_obj->obbox.ltx - ltx;
   dy = tmp_obj->obbox.lty - lty;
   FreeBoxObj(tmp_obj);

   for (obj_ptr=topObj; obj_ptr != NULL; obj_ptr=obj_ptr->next) {
      MoveObj(obj_ptr, dx, dy);
   }
    
   SelAllObj(FALSE);
    
   if (botObj != NULL) {
      botObj->next = saved_top_obj;
   } else {
      curPage->top = topObj = saved_top_obj;
   }

   if (saved_top_obj != NULL) {
      saved_top_obj->prev = botObj;
      curPage->bot = botObj = saved_bot_obj;
   }
   RedrawDrawWindow(botObj);
   PrepareToRecord(CMD_NEW, NULL, NULL, 0);
   RecordCmd(CMD_NEW, NULL, topSel, botSel, numObjSelected);
   HighLightForward();

   Msg("Objects pasted from tgif.");
   return TRUE;
}        

int PasteFromFile()
   /* returns FALSE if pasting in text mode and non-tgif bytes are */
   /*    in the cut buffer -- this is interpreted as an attempt to */
   /*    paste into the current text */
{
   char file_name[MAXPATHLENGTH+1];
   FILE *fp;
   char inbuf[MAXSTRING+1], * cut_buffer=NULL;
   int size=0, pos=0;

   if (SelectFileNameToPaste("Please select a file to PASTE ...", file_name) ==
         INVALID) {
      return TRUE;
   } else if (FileIsRemote(file_name)) {
      MsgBox("Pasting remote file not supported.", TOOL_NAME, INFO_MB);
      return TRUE;
   }
   if (curChoice == DRAWTEXT) {
      XEvent ev;

      pasteInDrawTextMode = TRUE;
      pasteFromFileInDrawTextMode = TRUE;
      strcpy(pasteFromFileName, file_name);
      ev.type = KeyPress;
      DrawText(&ev);
      return FALSE;
   }
   if ((fp=fopen(file_name, "r")) == NULL) {
      sprintf(gszMsgBox, "Can not open '%s' for read.", file_name);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return TRUE;
   }
   while (fgets(inbuf, MAXSTRING, fp) != NULL) size += strlen(inbuf);
   fclose(fp);
   if (size == 0) {
      sprintf(gszMsgBox, "File '%s' is empty.", file_name);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return TRUE;
   }
   cut_buffer = (char*)malloc((size+2)*sizeof(char));
   if (cut_buffer == NULL) {
      sprintf(gszMsgBox, "Can not malloc %1d bytes.", size+2);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return TRUE;
   }
   if ((fp=fopen(file_name, "r")) == NULL) {
      sprintf(gszMsgBox, "Can not open '%s' for read.", file_name);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return TRUE;
   }
   while (fgets(&cut_buffer[pos], MAXSTRING, fp) != NULL) {
      pos += strlen(&cut_buffer[pos]);
   }
   fclose(fp);
   PasteString(cut_buffer);

   return TRUE;
}

void CleanUpCutBuffer()
{
   if (cutBuffer != NULL) {
      *cutBuffer = '\0';
      free(cutBuffer);
      cutBuffer = NULL;
   }
   copyingToCutBuffer = FALSE;
}
