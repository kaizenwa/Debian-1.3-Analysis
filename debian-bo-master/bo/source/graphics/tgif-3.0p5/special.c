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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/special.c,v 3.0 1996/05/06 16:07:43 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "animate.e"
#include "attr.e"
#include "auxtext.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cutpaste.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "edit.e"
#include "exec.e"
#include "file.e"
#include "grid.e"
#include "group.e"
#include "mainloop.e"
#include "mark.e"
#include "menu.e"
#include "msg.e"
#include "move.e"
#include "names.e"
#include "obj.e"
#include "page.e"
#include "raster.e"
#include "remote.e"
#include "ruler.e"
#include "scroll.e"
#include "select.e"
#include "setup.e"
#ifndef _NO_EXTERN
#include "special.e"
#endif
#include "stk.e"
#include "text.e"
#ifdef _TGIF_WB
#include "wb1.e"
#endif /* _TGIF_WB */
#include "util.e"

int placingTopObj=FALSE;

char * specialMenuStr[] =
   {  "MakeSymbolic   ^#m",
      "UnMakeSymbolic ^#n",
      "Instantiate     ^i",
      "MakeIconic     ^#i",
      "UnMakeIconic   ^#j",
      "Push            ^v",
      "Pop             ^k",
      "AttachAttrs     #a",
      "DetachAttrs     #t",
      "ShowAttr        #-",
      "ShowAttrName    #n",
      "HideAttr       ^#h",
      "HideAttrName    #j",
      "Move/JustfyAttr #m",
      "EditAttrs         ",
      "AnimateSend     #e",
      "AnimateFlash    #f",
      "AttachFileAttrs   ",
      "DetachFileAttrs   ",
      "EditFileAttrs     ",
      "UpdateSymbols  ^#u",
      "ImportAttrs       ",
      "ExportAttrs       ",
      "MergeWithTable    ",
      "ExportToTable     ",
#ifdef _TGIF_WB
      "WhiteBoard        ",
#endif /* _TGIF_WB */
      NULL
   };
static char * specialMenuDescription[] =
   {  "Turn a selected grouped/icon object into a symbol object",
      "Turn a selected symbol object into a grouped object",
      "Instantiate a building-block object from the current domain",
      "Turn a selected grouped/symbol object into an icon object",
      "Turn an icon object into a grouped object",
      "Push into (edit) the symbol file which defines the selected icon object",
      "Pop back to a high lever (reverse of Push)",
      "Attach selected text objects as attributes for the non-text object",
      "Detach all attributes of selected objects",
      "Make all attributes of selected objects visible",
      "Make all attribute names of selected objects visible",
      "Hide all attributes for selected objects",
      "Hide all attribute names for selected objects",
      "Move/justify an attribute for a selected object",
      "Edit attributes of a selected object",
      "Animate a little token on a selected poly (not very useful)",
      "Flash a selected poly (not very useful)",
      "Attach selected text objects as file attributes",
      "Detach all file attributes",
      "Edit file attributes",
      "Refresh selected icon objects from their definition files",
      "Import attributes of a selected object from a text file",
      "Export attributes of a selected object to a text file",
      "Merge selected object with a table file",
      "Export attributes of selected objects to a table file",
#ifdef _TGIF_WB
      "Start/Enable the White Board communication",
#endif /* _TGIF_WB */
      NULL
   };

struct ObjRec * ReadSymbol (FP)
   FILE	* FP;
{
   register struct AttrRec	* attr_ptr;
   struct ObjRec		* obj_ptr;
   int				read_status;

   SetWatchCursor (drawWindow);
   SetWatchCursor (mainWindow);

   importingFile = TRUE; /* ignore 'state' info but set fileVersion */
   readingPageNum = 0;
   while ((read_status=ReadObj(FP, &obj_ptr)) == TRUE) {
      if (obj_ptr != NULL) {
         if (obj_ptr->type == OBJ_SYM) {
            UnlockAnObj(obj_ptr);
            obj_ptr->type = OBJ_ICON;
            attr_ptr = obj_ptr->lattr;
            for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->prev)
               attr_ptr->inherited = TRUE;
            importingFile = FALSE;
            SetDefaultCursor(mainWindow);
            SetDefaultCursor(drawWindow);
            return obj_ptr;
         } else {
            FreeObj(obj_ptr);
         }
      }
   }
   importingFile = FALSE;
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   if (read_status == INVALID)
   {
      sprintf (gszMsgBox,
            "File version too large (=%1d).\n\nRead symbol aborted!",
            fileVersion);
      Msg (gszMsgBox);
   }
   SetDefaultCursor (mainWindow);
   SetDefaultCursor (drawWindow);
   return (NULL);
}

struct ObjRec * GetObjRepresentation (PathName, SymName)
   char	* PathName, * SymName;
{
   char			file_name[MAXPATHLENGTH], * rest;
   int			short_name;
   struct ObjRec	* obj_ptr;
   FILE			* fp;
   int			tmp_linenum;
   char			tmp_filename[MAXPATHLENGTH];

   sprintf (file_name, "%s/%s.%s", PathName, SymName, SYM_FILE_EXT);
   if ((short_name = IsPrefix (bootDir, file_name, &rest))) ++rest;

   if ((fp = fopen (file_name, "r")) == NULL)
   {
      if (short_name)
         sprintf (gszMsgBox, "Can not open '%s'\n", rest);
      else
         sprintf (gszMsgBox, "Can not open '%s'\n", file_name);
      Msg (gszMsgBox);
      return (NULL);
   }

   strcpy (tmp_filename, scanFileName);
   tmp_linenum = scanLineNum;
   strcpy (scanFileName, (short_name ? rest : file_name));
   scanLineNum = 0;

   if ((obj_ptr = ReadSymbol (fp)) != NULL)
   {
      obj_ptr->id = objId++;
      obj_ptr->dirty = FALSE;
      strcpy (obj_ptr->detail.r->s, SymName);
      obj_ptr->detail.r->rotate = ROTATE0;
      obj_ptr->detail.r->flip = NO_FLIP;
      obj_ptr->detail.r->deck_index = (-1);
      AdjObjBBox (obj_ptr);
   }

   strcpy (scanFileName, tmp_filename);
   scanLineNum = tmp_linenum;

   fclose (fp);
   return (obj_ptr);
}

unsigned int PlaceTopObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   int		x, y, grid_x, grid_y, dx, dy, placing = TRUE;
   int		cursor_x, cursor_y, orig_x, orig_y;
   int		obj_ltx, obj_lty, obj_rbx, obj_rby;
   int		grid_obj_ltx, grid_obj_lty, grid_dx=0, grid_dy=0;
   Window	root_win, child_win;
   int		root_x, root_y, first_time=TRUE;
   unsigned int	status, button_pressed=Button1;
   XEvent	input, ev;

   RedrawMsg ();

   XFlush (mainDisplay);
   XSync (mainDisplay, False);

   placingTopObj = TRUE;
   if (XCheckMaskEvent (mainDisplay, ExposureMask, &ev) ||
         XCheckMaskEvent (mainDisplay, VisibilityChangeMask, &ev))
      ExposeEventHandler (&ev, TRUE);

   obj_ltx = OFFSET_X(ObjPtr->obbox.ltx); obj_lty = OFFSET_Y(ObjPtr->obbox.lty);
   obj_rbx = OFFSET_X(ObjPtr->obbox.rbx); obj_rby = OFFSET_Y(ObjPtr->obbox.rby);
   GridXY (obj_ltx, obj_lty, &grid_obj_ltx, &grid_obj_lty);

   dx = dy = 0;
/* XSetInputFocus (mainDisplay, drawWindow, RevertToNone, CurrentTime); */
   XGrabPointer (mainDisplay, drawWindow, False,
         PointerMotionMask | ButtonPressMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);

   if (XCheckMaskEvent (mainDisplay, PointerMotionMask, &input))
   {
      first_time = FALSE;

      cursor_x = input.xmotion.x;
      cursor_y = input.xmotion.y;
      while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &input))
      {
         cursor_x = input.xmotion.x;
         cursor_y = input.xmotion.y;
      }
      GridXY (cursor_x, cursor_y, &orig_x, &orig_y);
      grid_dx = orig_x-grid_obj_ltx; grid_dy = orig_y-grid_obj_lty;
      SelBox (drawWindow, revDefaultGC, obj_ltx+grid_dx, obj_lty+grid_dy,
            obj_rbx+grid_dx, obj_rby+grid_dy);
   }

   while (placing)
   {
      XNextEvent (mainDisplay, &input);

      if (first_time)
      {
         first_time = FALSE;

         XQueryPointer (mainDisplay, drawWindow, &root_win, &child_win,
               &root_x, &root_y, &cursor_x, &cursor_y, &status);

         GridXY (cursor_x, cursor_y, &orig_x, &orig_y);
         grid_dx = orig_x-grid_obj_ltx; grid_dy = orig_y-grid_obj_lty;
         SelBox (drawWindow, revDefaultGC, obj_ltx+grid_dx, obj_lty+grid_dy,
               obj_rbx+grid_dx, obj_rby+grid_dy);
      }

      if (input.type == Expose || input.type == VisibilityNotify)
      {
         SelBox (drawWindow, revDefaultGC, obj_ltx+grid_dx, obj_lty+grid_dy,
               obj_rbx+grid_dx, obj_rby+grid_dy);
         ExposeEventHandler (&input, TRUE);
         SelBox (drawWindow, revDefaultGC, obj_ltx+grid_dx, obj_lty+grid_dy,
               obj_rbx+grid_dx, obj_rby+grid_dy);
      }
      else if (input.type == ButtonPress)
      {
         XUngrabPointer (mainDisplay, CurrentTime);
         button_pressed = input.xbutton.button;
         placing = FALSE;
         SelBox (drawWindow, revDefaultGC, obj_ltx+grid_dx+dx,
               obj_lty+grid_dy+dy, obj_rbx+grid_dx+dx, obj_rby+grid_dy+dy);
         grid_dx = ABS_SIZE(grid_dx+dx);
         grid_dy = ABS_SIZE(grid_dy+dy);
         MoveObj (ObjPtr, grid_dx, grid_dy);
         numRedrawBBox = 0;
         ShowInterrupt (1);
         placingTopObj = FALSE;
         ObjPtr->tmp_parent = NULL;
         DrawObj (drawWindow, ObjPtr);
         HideInterrupt();
      }
      else if (input.type == MotionNotify)
      {
         x = input.xmotion.x;
         y = input.xmotion.y;
         GridXY (x, y, &grid_x, &grid_y);
         SelBox (drawWindow, revDefaultGC, obj_ltx+grid_dx+dx,
               obj_lty+grid_dy+dy, obj_rbx+grid_dx+dx, obj_rby+grid_dy+dy);
         dx = grid_x - orig_x;
         dy = grid_y - orig_y;
         SelBox (drawWindow, revDefaultGC, obj_ltx+grid_dx+dx,
               obj_lty+grid_dy+dy, obj_rbx+grid_dx+dx, obj_rby+grid_dy+dy);
         MarkRulers (grid_x, grid_y);
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   XSync (mainDisplay, True);
   placingTopObj = FALSE;
   return button_pressed;
}

static
int UnMakeIconicOnInstantiate (obj_ptr)
   struct ObjRec	* obj_ptr;
{
   register struct AttrRec	* attr_ptr;

   for (attr_ptr=obj_ptr->lattr; attr_ptr!=NULL; attr_ptr=attr_ptr->prev)
      if (*attr_ptr->attr_name.s=='\0' &&
            strcmp (attr_ptr->attr_value.s, "unmakeiconic_on_instantiate")==0)
         return (TRUE);
   return (FALSE);
}

static
void DeleteUnMakeIconicOnInstantiateText ()
{
   struct SelRec	* sel_ptr;
   struct ObjRec	* text_obj_ptr=NULL;

   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev)
      if (sel_ptr->obj->type == OBJ_TEXT &&
            strcmp (sel_ptr->obj->detail.t->first->dyn_str.s,
            "unmakeiconic_on_instantiate") == 0)
      {
         text_obj_ptr = sel_ptr->obj;
         break;
      }
   if (text_obj_ptr == NULL)
   {
      fprintf (stderr, "Warning:  Can not find the '%s' text object.\n",
            "unmakeiconic_on_instantiate");
      fprintf (stderr, "Please send bug report to william@cs.ucla.edu.\n");
      sprintf (gszMsgBox, "%s '%s' %s.\n\n%s.",
            "Warning:  Can not find the", "unmakeiconic_on_instantiate",
            "text object", "Please send bug report to william@cs.ucla.edu");
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      XFlush (mainDisplay);
      XSync (mainDisplay, False);
      return;
   }
   MakeQuiescent ();
   topSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (topSel == NULL) FailAllocMessage();
   topSel->next = NULL;
   topSel->obj = text_obj_ptr;
   topSel->prev = NULL;
   botSel = topSel;
   UpdSelBBox ();
   HighLightForward ();
   DelAllSelObj ();
}

void UnMakeIconic ();

void Instantiate ()
{
   char			file_name[MAXPATHLENGTH], * rest, msg[MAXSTRING];
   char			sym_name[MAXPATHLENGTH], path_name[MAXPATHLENGTH];
   char			ext_str[MAXPATHLENGTH];
   int			short_name;
   struct ObjRec	* obj_ptr;
   FILE			* fp;
   XEvent		ev;
   int			tmp_linenum, len, ext_len;
   char			tmp_filename[MAXPATHLENGTH];

   TieLooseEnds ();
   SetCurChoice (NOTHING);
   if (topSel!=NULL) { HighLightReverse (); RemoveAllSel (); }

   sprintf (msg, "Please select a symbol to INSTANTIATE in the '%s' domain...",
         curDomainName);
   if (SelectFromLibrary (msg, SYM_FILE_EXT, sym_name, path_name) == INVALID)
      return;

   if (XCheckMaskEvent (mainDisplay, ExposureMask, &ev) ||
         XCheckMaskEvent (mainDisplay, VisibilityChangeMask, &ev))
      ExposeEventHandler (&ev, TRUE);

   sprintf (file_name, "%s/%s", path_name, sym_name);

   len = strlen (sym_name);
   sprintf (ext_str, ".%s", SYM_FILE_EXT);
   ext_len = strlen (ext_str);
   if (len > ext_len && strcmp (ext_str, &sym_name[len-ext_len]) == 0)
      sym_name[len-ext_len] = '\0';

   if ((short_name = IsPrefix (bootDir, file_name, &rest))) ++rest;

   if ((fp = fopen (file_name, "r")) == NULL)
   {
      if (short_name)
         sprintf (msg, "Can not instantiate '%s'.", rest);
      else
         sprintf (msg, "Can not instantiate '%s'.", file_name);
      Msg (msg);
      return;
   }

   strcpy (tmp_filename, scanFileName);
   tmp_linenum = scanLineNum;
   strcpy (scanFileName, (short_name ? rest : file_name));
   scanLineNum = 0;

   SetWatchCursor (drawWindow);
   SetWatchCursor (mainWindow);
   if ((obj_ptr = ReadSymbol (fp)) != NULL)
   {
      if (short_name)
         sprintf (msg, "Instantiating '%s'...", rest);
      else
         sprintf (msg, "Instantiating '%s'...", file_name);
      Msg (msg);
      obj_ptr->id = objId++;
      obj_ptr->dirty = FALSE;
      strcpy (obj_ptr->detail.r->s, sym_name);
      obj_ptr->detail.r->rotate = ROTATE0;
      obj_ptr->detail.r->flip = NO_FLIP;
      obj_ptr->detail.r->deck_index = (-1);
      PlaceTopObj (obj_ptr);
      AssignNewObjIds (obj_ptr);
      AddObj (NULL, topObj, obj_ptr);
      AdjObjBBox (obj_ptr);

      SelectTopObj ();
      if (UnMakeIconicOnInstantiate (obj_ptr))
      {
         StartCompositeCmd ();
         RecordNewObjCmd ();
         UnMakeIconic ();
         UngroupSelObj ();
         DeleteUnMakeIconicOnInstantiateText ();
         EndCompositeCmd ();
      }
      else
         RecordNewObjCmd ();
      SetFileModified (TRUE);
      justDupped = FALSE;
   }
   fclose (fp);

   strcpy (scanFileName, tmp_filename);
   scanLineNum = tmp_linenum;

   SetDefaultCursor (mainWindow);
   SetDefaultCursor (drawWindow);
   Msg ("");
}

void MakeSymbolic ()
{
   if (topSel!=NULL && topSel==botSel && topSel->obj->type!=OBJ_SYM)
   {
      HighLightReverse ();
      PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);

      if (topSel->obj->type != OBJ_GROUP && topSel->obj->type != OBJ_ICON)
      {
         GroupSingleObj ();
         if (topSel->obj->fattr != NULL)
            Msg ("Object's attributes are promoted to the new symbol object.");
      }

      topSel->obj->type = OBJ_SYM;
      Msg ("Selected object is now SYMBOLIC.");
      AdjObjBBox (topSel->obj);
      UpdSelBBox ();
      RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
      RedrawAnArea (botObj,
            botSel->obj->obbox.ltx-QUARTER_INCH-GRID_ABS_SIZE(1),
            botSel->obj->obbox.lty-QUARTER_INCH-GRID_ABS_SIZE(1),
            botSel->obj->obbox.rbx+QUARTER_INCH+GRID_ABS_SIZE(1),
            botSel->obj->obbox.rby+QUARTER_INCH+GRID_ABS_SIZE(1));
      HighLightForward ();
      SetFileModified (TRUE);
      justDupped = FALSE;
   }
   else
      Msg ("Please select one non-symbol object to make it Symbolic.");
}

void UnMakeSymbolic ()
{
   register struct ObjRec	* obj_ptr;
   register int			ltx = 0, lty = 0, rbx = 0, rby = 0;
   struct SelRec		* sel_ptr = topSel;
   int				modified = FALSE;

   StartCompositeCmd ();
   for ( ; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      obj_ptr = sel_ptr->obj;
      if (obj_ptr->type == OBJ_SYM)
      {
         PrepareToReplaceAnObj (obj_ptr);
         obj_ptr->type = OBJ_GROUP;
         AdjObjBBox (obj_ptr);
         RecordReplaceAnObj (obj_ptr);
         if (modified)
         {
            if (obj_ptr->bbox.ltx < ltx) ltx = obj_ptr->bbox.ltx;
            if (obj_ptr->bbox.lty < lty) lty = obj_ptr->bbox.lty;
            if (obj_ptr->bbox.rbx > rbx) rbx = obj_ptr->bbox.rbx;
            if (obj_ptr->bbox.rby > rby) rby = obj_ptr->bbox.rby;
         }
         else
         {
            ltx = obj_ptr->bbox.ltx; lty = obj_ptr->bbox.lty;
            rbx = obj_ptr->bbox.rbx; rby = obj_ptr->bbox.rby;
            modified = TRUE;
         }
      }
   }
   EndCompositeCmd ();
   if (modified)
   {
      HighLightReverse ();
      UpdSelBBox ();
      RedrawAnArea (botObj, ltx-QUARTER_INCH-GRID_ABS_SIZE(1),
            lty-QUARTER_INCH-GRID_ABS_SIZE(1),rbx+QUARTER_INCH+GRID_ABS_SIZE(1),
            rby+QUARTER_INCH+GRID_ABS_SIZE(1));
      HighLightForward ();
      SetFileModified (TRUE);
      justDupped = FALSE;
   }
}

void MakeIconic ()
{
   char 		icon_name[MAXPATHLENGTH], file_name[MAXPATHLENGTH];
   char 		s[MAXPATHLENGTH], icon_full_name[MAXPATHLENGTH], * rest;
   char 		obj_ext_str[MAXSTRING], sym_ext_str[MAXSTRING];
   FILE			* fp;
   int			len, short_name, obj_ext_len, sym_ext_len;
   int			ltx, lty, rbx, rby;

   if (topSel!=NULL && topSel==botSel)
   {
      Dialog ("Please enter name of the icon:",
            "( <CR>: accept, <ESC>: cancel )", icon_name);
      len = strlen (icon_name);
      if (*icon_name == '\0')
      {
         Msg ("Name not specified, icon not created.");
         return;
      }

      sprintf (obj_ext_str, ".%s", OBJ_FILE_EXT);
      sprintf (sym_ext_str, ".%s", SYM_FILE_EXT);
      obj_ext_len = strlen (obj_ext_str);
      sym_ext_len = strlen (sym_ext_str);

      if (len >= obj_ext_len || len >= sym_ext_len)
      {
         if (strcmp (&icon_name[len-obj_ext_len], obj_ext_str) == 0)
         {
            Msg ("Can not save as an object file, icon not created.");
            return;
         }
         else if (strcmp (&icon_name[len-sym_ext_len], sym_ext_str) != 0)
         {
            strcpy (icon_full_name, icon_name);
            strcat (icon_full_name, sym_ext_str);
         }

         if (strlen (icon_full_name) == sym_ext_len)
         {
            Msg ("No file name specified.  File not saved.");
            return;
         }
      }
      else
      {
         strcpy (icon_full_name, icon_name);
         strcat (icon_full_name, sym_ext_str);
      }

      sprintf (file_name, "%s/%s", curDir, icon_full_name);

      if (!OkayToCreateFile (file_name)) return;
      if ((short_name = IsPrefix (bootDir, file_name, &rest))) ++rest;

      if ((fp = fopen (file_name, "w")) == NULL)
      {
         if (short_name)
            sprintf (s, "Can not create '%s', icon not created.", rest);
         else
            sprintf (s, "Can not create '%s', icon not created.", file_name);
         Msg (s);
         return;
      }

      if (!DirInSymPath (curDir)) UpdateSymInfo ();

      if (short_name)
         sprintf (s, "Creating '%s' ...", rest);
      else
         sprintf (s, "Creating '%s' ...", file_name);
      Msg (s);

      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;

      PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);
      if (topSel->obj->type == OBJ_GROUP || topSel->obj->type == OBJ_SYM ||
            topSel->obj->type == OBJ_ICON)
         JustMoveSelToTop ();
      else
      {
         GroupSingleObj ();
         if (topSel->obj->fattr != NULL)
            Msg ("Object's attributes are promoted to the new icon object.");
      }

      topSel->obj->type = OBJ_SYM;
      strcpy (topSel->obj->detail.r->s, icon_name);
      topSel->obj->detail.r->rotate = ROTATE0;
      topSel->obj->detail.r->flip = NO_FLIP;
      topSel->obj->detail.r->deck_index = (-1);

      writeFileFailed = FALSE;
      Save (fp, topSel->obj, 0, 1);

      if (writeFileFailed)
      {
         writeFileFailed = FALSE;
         sprintf (s, "%s '%s'.\n\n%s.\n\n'%s' deleted.",
               "Fail to write to", file_name, "File system may be full",
               file_name);
         MsgBox (s, TOOL_NAME, INFO_MB);
         unlink (file_name);
      }
      else
      {
         if (short_name)
            sprintf (s, "File '%s' created.", rest);
         else
            sprintf (s, "File '%s' created.", file_name);
         Msg (s);
      }
      fclose (fp);

      HighLightReverse ();
      topSel->obj->type = OBJ_ICON;
      topSel->obj->id = objId++;
      Msg ("Selected object is now ICONIC.");
      AdjObjBBox (topSel->obj);
      UpdSelBBox ();
      RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
      ltx = min(ltx,selLtX); lty = min(lty,selLtY);
      rbx = max(rbx,selRbX); rby = max(rby,selRbY);
      RedrawAnArea (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1));
      HighLightForward ();
      SetFileModified (TRUE);
      justDupped = FALSE;
   }
   else
      Msg ("Please select one object to make it ICONIC.");
}

void UnMakeIconic ()
{
   register struct ObjRec	* obj_ptr;
   struct SelRec		* sel_ptr;
   struct AttrRec		* attr_ptr;
   int				modified = FALSE;

   HighLightReverse ();
   StartCompositeCmd ();
   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      obj_ptr = sel_ptr->obj;
      if (obj_ptr->type == OBJ_ICON)
      {
         modified = TRUE;
         PrepareToReplaceAnObj (obj_ptr);
         obj_ptr->type = OBJ_GROUP;
         attr_ptr = obj_ptr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
            attr_ptr->inherited = FALSE;
         AdjObjBBox (obj_ptr);
         RecordReplaceAnObj (obj_ptr);
      }
   }
   EndCompositeCmd ();
   if (modified)
   {
      Msg ("Selected ICONIC objects are GROUP objects now.");
      UpdSelBBox ();
      RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      SetFileModified (TRUE);
      justDupped = FALSE;
   }
   HighLightForward ();
}

typedef struct LineRec {
   char *s;
   struct LineRec *next, *prev;
} *LineRecPtr;

static struct LineRec *firstLine=NULL, *lastLine=NULL;
static int numLines=0, nextX=0, nextY=0;

static
void AddLine(buf)
   char *buf;
{
   struct LineRec *line_ptr;

   line_ptr = (struct LineRec *)malloc(sizeof(struct LineRec));
   if (line_ptr == NULL) FailAllocMessage();
   line_ptr->s = buf;
   line_ptr->next = NULL;
   line_ptr->prev = lastLine;
   if (lastLine == NULL) {
      firstLine = line_ptr;
   } else {
      lastLine->next = line_ptr;
   }
   lastLine = line_ptr;
   numLines++;
}

static
void ImportAnAttr(obj_ptr, attr_line_num, fname)
   struct ObjRec *obj_ptr;
   int attr_line_num;
   char *fname;
{
   struct LineRec *next_line;
   int ok=TRUE;
   char *c_ptr;

   if (firstLine == NULL) return;
   if ((c_ptr=strchr(firstLine->s, '=')) == NULL) {
      sprintf(gszMsgBox, "Line %1d in '%s' %s.", attr_line_num, fname,
            "is skipped because it does not specify an attribute");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      ok = FALSE;
   } else {
      *c_ptr = '\0';
      if (strchr(firstLine->s, '!') != NULL ||
            strchr(firstLine->s, '.') != NULL) {
         sprintf(gszMsgBox, "Line %1d in '%s' %s %s.", attr_line_num, fname,
               "is skipped because attribute name contains",
               "illegal characters such as '!' or '.'");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         ok = FALSE;
      }
      *c_ptr = '=';
   }
   if (ok) {
      char saved_ch=(*(++c_ptr)), *attr_name=NULL;
      struct AttrRec *attr_ptr=NULL;
      struct TextRec *text_ptr=NULL;
      struct StrRec *s_ptr;
      struct LineRec *line_ptr;
      int move_next_y=FALSE;

      *c_ptr = '\0';
      attr_name = UtilStrDup(firstLine->s);
      if (attr_name == NULL) FailAllocMessage();
      *c_ptr = saved_ch;

      if ((attr_ptr=FindAttrWithName(obj_ptr, attr_name, NULL)) == NULL) {
         attr_ptr = AddAttrByNameAndValue(obj_ptr, attr_name, c_ptr);
         attr_ptr->shown = TRUE;
         attr_ptr->obj->color = colorIndex;
         MoveObj(attr_ptr->obj, nextX-attr_ptr->obj->x, nextY-attr_ptr->obj->y);
         text_ptr = attr_ptr->obj->detail.t;
         move_next_y = TRUE;
      } else {
         struct StrRec *next_s_ptr;

         DynStrSet(&attr_ptr->attr_value, c_ptr);
         text_ptr = attr_ptr->obj->detail.t;
         text_ptr->cached_zoom = 0;
         if (text_ptr->cached_bitmap != None) {
            XFreePixmap(mainDisplay, text_ptr->cached_bitmap);
         }
         text_ptr->cached_bitmap = None;
         for (s_ptr=text_ptr->first->next; s_ptr != NULL;
               s_ptr=next_s_ptr) {
            next_s_ptr = s_ptr->next;
            free(s_ptr);
         }
         text_ptr->last = text_ptr->first;
         text_ptr->first->next = text_ptr->first->prev = NULL;
         text_ptr->lines = 1;
         UpdAttr(attr_ptr);
      }
      free(attr_name);
      for (line_ptr=firstLine->next; line_ptr!=NULL; line_ptr=line_ptr->next) {
         s_ptr = (struct StrRec *)malloc(sizeof(struct StrRec));
         if (s_ptr == NULL) FailAllocMessage();
         DynStrSet(&s_ptr->dyn_str, line_ptr->s);
         s_ptr->prev = text_ptr->last;
         s_ptr->next = NULL;
         text_ptr->last->next = s_ptr;
         text_ptr->last = s_ptr;
         text_ptr->lines++;
      }
      UpdTextBBox(attr_ptr->obj);
      if (move_next_y) {
         nextY += attr_ptr->obj->obbox.rby-attr_ptr->obj->obbox.lty;
      }
   }
   for ( ; firstLine != NULL; firstLine=next_line) {
      next_line = firstLine->next;
      if (firstLine->s != NULL) free(firstLine->s);
      free(firstLine);
   }
   lastLine = NULL;
   numLines = 0;
}

void ImportAttrs()
{
   char fname[MAXPATHLENGTH+1], *rest, *buf;
   XEvent ev;
   int short_name, ltx, lty, rbx, rby, line_num=0, attr_line_num=1;
   int empty_line=TRUE;
   FILE *fp;
   struct ObjRec *obj_ptr;

   if (topSel == NULL || topSel != botSel) {
      sprintf(gszMsgBox, "Please select one object for ImportAttrs.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (SelectFileNameToImport("Please select a text file for ImportAttrs...",
         TEXT_FILE_EXT, fname) == INVALID) {
      return;
   } else if (FileIsRemote(fname)) {
      MsgBox("Importing remote text file not supported.", TOOL_NAME, INFO_MB);
      return;
   }
   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
   }
   if ((short_name=IsPrefix(bootDir, fname, &rest))) ++rest;
   if ((fp=fopen(fname, "r")) == NULL) {
      sprintf(gszMsgBox,
            "Can not open '%s' for read, attributes not imported.",
            short_name ? rest : fname);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   obj_ptr = topSel->obj;
   ltx = obj_ptr->bbox.ltx; lty = obj_ptr->bbox.lty;
   rbx = obj_ptr->bbox.rbx; rby = obj_ptr->bbox.rby;

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);

   HighLightReverse();
   PrepareToReplaceAnObj(obj_ptr);

   nextX = obj_ptr->obbox.ltx;
   nextY = obj_ptr->obbox.rby;
   while ((buf=UtilGetALine(fp)) != NULL) {
      line_num++;
      if (empty_line) {
         empty_line = FALSE;
         attr_line_num = line_num;
      }
      if (*buf == '\0') {
         ImportAnAttr(obj_ptr, attr_line_num, fname);
         empty_line = TRUE;
      } else {
         AddLine(buf);
      }
   }
   ImportAnAttr(obj_ptr, attr_line_num, fname);
   fclose(fp);

   AdjObjBBox(obj_ptr);

   RecordReplaceAnObj(obj_ptr);

   SetDefaultCursor(mainWindow);
   ShowCursor();

   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         obj_ptr->bbox.ltx-GRID_ABS_SIZE(1),
         obj_ptr->bbox.lty-GRID_ABS_SIZE(1),
         obj_ptr->bbox.rbx+GRID_ABS_SIZE(1),
         obj_ptr->bbox.rby+GRID_ABS_SIZE(1));
   HighLightForward();
   SetFileModified(TRUE);
   justDupped = FALSE;
}

void ExportAttrs()
{
   char msg[MAXSTRING+1], fname[MAXPATHLENGTH+1], full_fname[MAXPATHLENGTH+1];
   char *c_ptr, *dot_ptr, *rest;
   int short_name;
   FILE *fp;
   struct AttrRec *attr_ptr;

   if (topSel == NULL || topSel != botSel) {
      sprintf(gszMsgBox, "Please select one object for ExportAttrs.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   } else if (topSel->obj->lattr == NULL) {
      sprintf(gszMsgBox, "Select object has no attributes to export.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   sprintf(gszMsgBox, "( working directory: %s )",
         curDirIsLocal ? curDir : curLocalDir);
   sprintf(msg, "%s: %s",
         "Please enter a text file name to export to",
         "( <CR>: accept, <ESC>: cancel )");
   Dialog(msg, gszMsgBox, fname);
   if (*fname == '\0') return;
   if (*fname == '/') {
      strcpy(full_fname, fname);
   } else {
      sprintf(full_fname, "%s/%s", curDirIsLocal ? curDir : curLocalDir, fname);
   }
   if ((c_ptr=UtilStrRChr(full_fname, (int)'/')) == NULL) {
      if ((dot_ptr=UtilStrRChr(full_fname, (int)'.')) == NULL) {
         sprintf(&full_fname[strlen(full_fname)], ".%s", TEXT_FILE_EXT);
      } else {
         if (strcmp(&dot_ptr[1], TEXT_FILE_EXT) != 0) {
            sprintf(&dot_ptr[strlen(dot_ptr)], ".%s", TEXT_FILE_EXT);
         }
      }
   } else {
      if ((dot_ptr=UtilStrRChr(c_ptr, (int)'.')) == NULL) {
         sprintf(&c_ptr[strlen(c_ptr)], ".%s", TEXT_FILE_EXT);
      } else {
         if (strcmp(&dot_ptr[1], TEXT_FILE_EXT) != 0) {
            sprintf(&dot_ptr[strlen(dot_ptr)], ".%s", TEXT_FILE_EXT);
         }
      }
   }
   if (!OkayToCreateFile(full_fname)) return;

   if ((short_name=IsPrefix(bootDir, full_fname, &rest))) ++rest;
   if ((fp=fopen(full_fname, "w")) == NULL) {
      sprintf(gszMsgBox,
            "Can not open '%s' for write, attributes not exported.",
            short_name ? rest : full_fname);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   sprintf(gszMsgBox, "Writing attributes to '%s' ...",
         short_name ? rest : full_fname);
   Msg(gszMsgBox);
   writeFileFailed = FALSE;

   for (attr_ptr=topSel->obj->lattr; !writeFileFailed && attr_ptr != NULL;
         attr_ptr=attr_ptr->prev) {
      struct StrRec *s_ptr;

      if (attr_ptr != topSel->obj->lattr) fprintf(fp, "\n");
      if (fprintf(fp, "%s%s\n", attr_ptr->attr_name.s,
            attr_ptr->attr_value.s) < 0) {
         writeFileFailed = TRUE;
      }
      if (!writeFileFailed && (s_ptr=attr_ptr->obj->detail.t->first) != NULL) {
         for (s_ptr=s_ptr->next; !writeFileFailed && s_ptr != NULL;
               s_ptr=s_ptr->next) {
            if (fprintf(fp, "%s\n", s_ptr->dyn_str.s) < 0) {
               writeFileFailed = TRUE;
            }
         }
      }
   }
   if (writeFileFailed) {
      writeFileFailed = FALSE;
      sprintf(gszMsgBox, "Fail to write to '%s'.\n\nFile system may be full.",
            full_fname);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   } else {
      sprintf(gszMsgBox, "Attributes exported to '%s'.",
            short_name ? rest : full_fname);
      Msg(gszMsgBox);
   }
   fclose(fp);
}

static
int GetMergeSpec(obj_ptr, attr_name, pn_val, distance)
   struct ObjRec *obj_ptr;
   char *attr_name;
   int *pn_val, distance;
{
   struct AttrRec *attr_ptr=FindAttrWithName(obj_ptr, attr_name, NULL);
   char *c_ptr;
   double val;

   if (attr_ptr == NULL) {
      sprintf(gszMsgBox, "Can not find the '%s' attribute for MergeWithTable.",
            attr_name);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   if (distance && ((c_ptr=strstr(attr_ptr->attr_value.s, "in")) != NULL ||
         (c_ptr=strstr(attr_ptr->attr_value.s, "In")) != NULL ||
         (c_ptr=strstr(attr_ptr->attr_value.s, "IN")) != NULL)) {
      char saved_ch=(*c_ptr);

      *c_ptr = '\0';
      if (sscanf(attr_ptr->attr_value.s, "%lf", &val) != 1) {
         *c_ptr = saved_ch;
         sprintf(gszMsgBox, "Malformed '%s' attribute for MergeWithTable.",
               attr_name);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return FALSE;
      }
      *c_ptr = saved_ch;
      val = val * ((double)PIX_PER_INCH);
      *pn_val = round(val);
   } else if (distance &&
         ((c_ptr=strstr(attr_ptr->attr_value.s, "cm")) != NULL ||
         (c_ptr=strstr(attr_ptr->attr_value.s, "Cm")) != NULL ||
         (c_ptr=strstr(attr_ptr->attr_value.s, "CM")) != NULL)) {
      char saved_ch=(*c_ptr);

      *c_ptr = '\0';
      if (sscanf(attr_ptr->attr_value.s, "%lf", &val) != 1) {
         *c_ptr = saved_ch;
         sprintf(gszMsgBox, "Malformed '%s' attribute for MergeWithTable.",
               attr_name);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return FALSE;
      }
      *c_ptr = saved_ch;
      val = val * ((double)ONE_CM);
      *pn_val = round(val);
   } else {
      if (sscanf(attr_ptr->attr_value.s, "%d", pn_val) != 1) {
         sprintf(gszMsgBox, "Malformed '%s' attribute for MergeWithTable.",
               attr_name);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return FALSE;
      }
   }
   return TRUE;
}

void MergeWithTable()
{
   char fname[MAXPATHLENGTH+1], *rest, paper_size_spec[80], *spec, **col_names;
   char *buf, *c_ptr, *val_ptr;
   XEvent ev;
   int i, short_name, columns_in_file, ok=TRUE, line_num, user_placement;
   int left_margin=0, top_margin=0, v_pitch=0, h_pitch=0, strip_double_quotes;
   int num_cols=0, num_rows=0, paper_w=0, paper_h=0, x, y, r, c;
   int tab_separated=TRUE;
   FILE *fp;
   struct ObjRec *template_obj;

   if (topSel == NULL || topSel != botSel) {
      sprintf(gszMsgBox, "Please select one object for MergeWithTable.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   template_obj = topSel->obj;
   user_placement = (FindAttrWithName(template_obj, "USER_PLACEMENT", NULL) !=
         NULL);
   strip_double_quotes = (FindAttrWithName(template_obj, "STRIP_DOUBLE_QUOTES",
         NULL) != NULL);
   if (!user_placement) {
      if (!(GetMergeSpec(template_obj, "LEFT_MARGIN=", &left_margin, TRUE) &&
            GetMergeSpec(template_obj, "TOP_MARGIN=", &top_margin, TRUE) &&
            GetMergeSpec(template_obj, "V_PITCH=", &v_pitch, TRUE) &&
            GetMergeSpec(template_obj, "H_PITCH=", &h_pitch, TRUE) &&
            GetMergeSpec(template_obj, "NUM_COLS=", &num_cols, FALSE) &&
            GetMergeSpec(template_obj, "NUM_ROWS=", &num_rows, FALSE) &&
            GetMergeSpec(template_obj, "PAPER_WIDTH=", &paper_w, TRUE) &&
            GetMergeSpec(template_obj, "PAPER_HEIGHT=", &paper_h, TRUE))) {
         return;
      }
   }
   while (!DirIsRemote(curDir) && fileModified) {
      switch (MsgBox("File modified, save file before merge? [ync](y)",
            TOOL_NAME, YNC_MB)) {
      case MB_ID_YES: SaveFile(); break;
      case MB_ID_NO: SetFileModified(FALSE); break;
      case MB_ID_CANCEL: return;
      }
   }
   if (firstCmd != NULL)
   {
      sprintf(gszMsgBox, "MergeWithTable can not be undone.\n\n%s",
            "Ok to proceed? [ync](y)");
      if (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB) != MB_ID_YES) return;
      CleanUpCmds ();
   }
   if (SelectFileNameToImport("Please select a text file for MergeWithTable...",
         TEXT_FILE_EXT, fname) == INVALID) {
      return;
   } else if (FileIsRemote(fname)) {
      MsgBox("Merging remote text file not supported.", TOOL_NAME, INFO_MB);
      return;
   }
   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
   }
   if ((short_name=IsPrefix(bootDir, fname, &rest))) ++rest;
   if ((fp=fopen(fname, "r")) == NULL) {
      sprintf(gszMsgBox,
            "Can not open '%s' for read.\n\nMerging aborted.",
            short_name ? rest : fname);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if ((spec=UtilGetALine(fp)) == NULL) {
      fclose(fp);
      sprintf(gszMsgBox,
            "Can not find column names in '%s'.\n\nMerging aborted.",
            short_name ? rest : fname);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   columns_in_file = 1;
   val_ptr = spec;
   c_ptr = strchr(val_ptr, '\t');
   if (c_ptr == NULL && (c_ptr=strchr(val_ptr, ';')) != NULL) {
      tab_separated = FALSE;
      Msg("';'s are used as separators.");
   } else {
      Msg("<TAB>s are used as separators.");
   }
   while (val_ptr != NULL) {
      if (c_ptr != NULL) *c_ptr = '\0';
      if (strlen(val_ptr) == 0) {
         free(spec);
         fclose(fp);
         sprintf(gszMsgBox,
               "Malformed column names in '%s'.\n\nMerging aborted.",
               short_name ? rest : fname);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return;
      }
      if (c_ptr == NULL) break;
      *c_ptr++ = (tab_separated ? '\t' : ';');
      val_ptr = c_ptr;
      c_ptr = strchr(val_ptr, tab_separated ? '\t' : ';');

      columns_in_file++;
   }
   col_names = (char**)malloc((columns_in_file+1)*sizeof(char*));
   if (col_names == NULL) {
      free(spec);
      fclose(fp);
      FailAllocMessage();
      return;
   }
   col_names[columns_in_file] = NULL;
   i = 0;
   val_ptr = spec;
   for (c_ptr=strchr(val_ptr, tab_separated ? '\t' : ';'); val_ptr != NULL;
         i++) {
      int len;

      if (c_ptr != NULL) *c_ptr = '\0';
      if (!tab_separated) {
         UtilTrimBlanks(val_ptr);
         if (strip_double_quotes && *val_ptr == '"') {
            len = strlen(val_ptr);

            if (val_ptr[len-1] == '"') {
               val_ptr[len-1] = '\0';
               val_ptr++;
            }
         }
      }
      len = strlen(val_ptr);
      if ((col_names[i]=(char*)malloc((len+2)*sizeof(char))) == NULL) {
         FailAllocMessage();
         columns_in_file = i;
         for (i=0; i < columns_in_file; i++) {
            if (col_names[i] != NULL) {
               free(col_names[i]);
            }
         }
         free(col_names);
         free(spec);
         fclose(fp);
         return;
      }
      sprintf(col_names[i], "%s=", val_ptr);
      if (FindAttrWithName(template_obj, col_names[i], NULL) == NULL) {
         free(col_names[i]);
         col_names[i] = NULL;
      }
      if (c_ptr == NULL) break;
      *c_ptr++ = (tab_separated ? '\t' : ';');
      val_ptr = c_ptr;
      c_ptr = strchr(val_ptr, tab_separated ? '\t' : ';');
   }
   free(spec);

   MakeQuiescent();
   UnlinkObj(template_obj);
   NewProc();
   if (pageLayoutMode == PAGE_TILE) {
      PageLayoutSubMenu(PAGE_STACK);
      while (lastPageNum > 1) {
         DeleteCurPage();
      }
   }
   if (!user_placement) {
      sprintf(paper_size_spec, "%1d x %1d", paper_w, paper_h);
      if (!SetPaperSize(paper_size_spec)) {
         return;
      }
      printMag = 100;
      UpdPageStyle(PORTRAIT);
   }
   UpdDrawWinBBox();
   AdjSplineVs();
   RedrawScrollBars();
   RedrawRulers();
   RedrawTitleWindow();

   SaveStatusStrings();
   if (user_placement) {
      TwoLineMsg("Left button places a merged object,",
            "    other buttons cancel merging.");
   }
   line_num = 1;
   x = left_margin;
   y = top_margin;
   r = c = 0;
   while (ok && (buf=UtilGetALine(fp)) != NULL) {
      struct ObjRec *obj_ptr=DupObj(template_obj);

      if (obj_ptr == NULL) {
         ok = FALSE;
         free(buf);
         break;
      }
      sprintf(gszMsgBox, "Processing object %1d...", line_num);
      SetStringStatus(gszMsgBox);
      XSync(mainDisplay, False);

      AddObj(NULL, topObj, obj_ptr);
      AdjObjBBox(obj_ptr);
      MoveObj(obj_ptr, x-obj_ptr->obbox.ltx, y-obj_ptr->obbox.lty);

      line_num++;
      i = 0;
      val_ptr = buf;
      replaceAttrFirstValueRedraw = FALSE;
      for (c_ptr=strchr(val_ptr, tab_separated ? '\t' : ';'); val_ptr != NULL;
            i++) {
         struct AttrRec *attr_ptr;

         if (c_ptr != NULL) *c_ptr = '\0';

         if (col_names[i] != NULL && (attr_ptr=FindAttrWithName(obj_ptr,
               col_names[i], NULL)) != NULL) {
            if (!tab_separated) UtilTrimBlanks(val_ptr);
            if (strip_double_quotes && *val_ptr == '"') {
               int len=strlen(val_ptr);

               if (val_ptr[len-1] == '"') {
                  val_ptr[len-1] = '\0';
                  ReplaceAttrFirstValue(obj_ptr, attr_ptr, &val_ptr[1]);
                  val_ptr[len-1] = '"';
               } else {
                  ReplaceAttrFirstValue(obj_ptr, attr_ptr, val_ptr);
               }
            } else {
               ReplaceAttrFirstValue(obj_ptr, attr_ptr, val_ptr);
            }
         }
         if (c_ptr == NULL) break;
         *c_ptr++ = (tab_separated ? '\t' : ';');
         val_ptr = c_ptr;
         c_ptr = strchr(val_ptr, tab_separated ? '\t' : ';');
      }
      replaceAttrFirstValueRedraw = TRUE;
      UnlinkObj(obj_ptr);
      if (firstCmd != NULL) CleanUpCmds();
      if (i+1 != columns_in_file) {
         ok = FALSE;
         sprintf(gszMsgBox,
               "Malformed table at line %d in '%s'.\n\nMerging aborted.",
               line_num, short_name ? rest : fname);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         FreeObj(obj_ptr);
         ClearAndRedrawDrawWindow();
      } else {
         struct AttrRec *attr_ptr;
         int saved_history_depth=historyDepth;

         if (user_placement) {
            if ((attr_ptr=FindAttrWithName(obj_ptr, "name=",
                  NULL)) != NULL) {
               sprintf(gszMsgBox, "Placing object '%s'...",
                     attr_ptr->attr_value.s);
               SetStringStatus(gszMsgBox);
               XSync(mainDisplay, False);
            }
            if (PlaceTopObj(obj_ptr) == Button3) {
               Msg("Operation aborted by the user.");
               ok = FALSE;
            }
            AssignNewObjIds(obj_ptr);
         }
         AdjObjBBox(obj_ptr);
         AddObj(NULL, topObj, obj_ptr);
         numRedrawBBox = 0;
         obj_ptr->tmp_parent = NULL;
         DrawObj(drawWindow, obj_ptr);
         if ((attr_ptr=FindAttrWithName(obj_ptr, "EXEC_AFTER_MERGE=",
               NULL)) != NULL) {
            char name[MAXSTRING+1];
            struct AttrRec *next_attr_ptr=NULL;

            if (*attr_ptr->attr_value.s != '\0') {
               sprintf(name, "%s=", attr_ptr->attr_value.s);
               if ((next_attr_ptr=FindAttrWithName(obj_ptr, name, NULL)) !=
                     NULL) {
                  int saved_intr_check_interval=intrCheckInterval;

                  intrCheckInterval = 1;
                  ShowInterrupt(1);
                  if (!DoExec(next_attr_ptr, obj_ptr)) {
                     ok = FALSE;
                  }
                  while (HideInterrupt() > 0) ;
                  intrCheckInterval = saved_intr_check_interval;
               } else {
                  sprintf(gszMsgBox, "%s '%s'.\n\nMerging aborted.",
                        "Can not find attribute named", attr_ptr->attr_value.s);
                  MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
                  ok = FALSE;
               }
            }
            attr_ptr = next_attr_ptr;
         } else if ((attr_ptr=FindAttrWithName(obj_ptr, EXEC_ATTR, NULL)) !=
               NULL) {
            int saved_intr_check_interval=intrCheckInterval;

            intrCheckInterval = 1;
            ShowInterrupt(1);
            if (!DoExec(attr_ptr, obj_ptr)) {
               ok = FALSE;
            }
            while (HideInterrupt() > 0) ;
            intrCheckInterval = saved_intr_check_interval;
         }
         if (saved_history_depth != historyDepth) RestoreDefaultHistoryDepth();
         if (firstCmd != NULL) CleanUpCmds();
         if (ok && !user_placement) {
            c++;
            if (c >= num_cols) {
               c = 0;
               x = left_margin;
               r++;
               if (r >= num_rows) {
                  r = 0;
                  y = top_margin;
                  AddPageAfter();
               } else {
                  y += v_pitch;
               }
            } else {
               x += h_pitch;
            }
         }
      }
      free(buf);
   }
   fclose(fp);

   RestoreStatusStrings();
   sprintf(gszMsgBox, "%1d objects generated.", line_num-1);
   Msg(gszMsgBox);

   FreeObj(template_obj);
   for (i=0; i < columns_in_file; i++) {
      if (col_names[i] != NULL) {
         free(col_names[i]);
      }
   }
   free(col_names);
   if (firstCmd != NULL) CleanUpCmds();
   SetFileModified(TRUE);
   justDupped = FALSE;
}

void ExportToTable()
{
   char msg[MAXSTRING+1], fname[MAXPATHLENGTH+1], full_fname[MAXPATHLENGTH+1];
   char *c_ptr, *dot_ptr, *rest=NULL, *spec, *spec_copy, **col_names=NULL, *buf;
   int short_name, total, i, num_exported, buf_sz, ok=TRUE;
   FILE *fp;
   struct AttrRec *spec_attr;
   struct StrRec *s_ptr;
   struct SelRec *sel_ptr;

   if (topSel == NULL) {
      MsgBox("No object selected.\n\nNothing to export.", TOOL_NAME, INFO_MB);
      return;
   }
   strcpy(msg, "!.TABLE_ATTRS=");
   spec_attr = FindAttrWithName(NULL, msg, NULL);
   if (spec_attr == NULL) {
      sprintf(gszMsgBox, "%s \"!.TABLE_ATTRS\" %s.\n\n%s.",
            "Can not find the", "file attribute",
            "Can not proceed with ExportToTable");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   sprintf(gszMsgBox, "( working directory: %s )",
         curDirIsLocal ? curDir : curLocalDir);
   sprintf(msg, "%s: %s",
         "Please enter a text file name to export to",
         "( <CR>: accept, <ESC>: cancel )");
   Dialog(msg, gszMsgBox, fname);

   if (*fname == '\0') return;

   if (*fname == '/') {
      strcpy(full_fname, fname);
   } else {
      sprintf(full_fname, "%s/%s", curDirIsLocal ? curDir : curLocalDir, fname);
   }
   if ((c_ptr=UtilStrRChr(full_fname, (int)'/')) == NULL) {
      if ((dot_ptr=UtilStrRChr(full_fname, (int)'.')) == NULL) {
         sprintf(&full_fname[strlen(full_fname)], ".%s", TEXT_FILE_EXT);
      } else {
         if (strcmp(&dot_ptr[1], TEXT_FILE_EXT) != 0) {
            sprintf(&dot_ptr[strlen(dot_ptr)], ".%s", TEXT_FILE_EXT);
         }
      }
   } else {
      if ((dot_ptr=UtilStrRChr(c_ptr, (int)'.')) == NULL) {
         sprintf(&c_ptr[strlen(c_ptr)], ".%s", TEXT_FILE_EXT);
      } else {
         if (strcmp(&dot_ptr[1], TEXT_FILE_EXT) != 0) {
            sprintf(&dot_ptr[strlen(dot_ptr)], ".%s", TEXT_FILE_EXT);
         }
      }
   }
   if (!OkayToCreateFile(full_fname)) return;

   if ((short_name=IsPrefix(bootDir, full_fname, &rest))) ++rest;
   if ((fp=fopen(full_fname, "w")) == NULL) {
      sprintf(gszMsgBox,
            "Can not open '%s' for write, attributes not exported.",
            short_name ? rest : full_fname);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   sprintf(gszMsgBox, "Writing attributes in table form to '%s' ...",
         short_name ? rest : full_fname);
   Msg(gszMsgBox);
   writeFileFailed = FALSE;

   total = 0;
   for (s_ptr=spec_attr->obj->detail.t->first; s_ptr != NULL;
         s_ptr=s_ptr->next) {
      total += strlen(s_ptr->dyn_str.s)+1;
   }
   if ((spec=(char*)malloc((total+1)*sizeof(char))) == NULL) {
      FailAllocMessage();
      fclose(fp);
      return;
   }
   c_ptr = spec;
   for (s_ptr=spec_attr->obj->detail.t->first; s_ptr != NULL;
         s_ptr=s_ptr->next) {
      int len=(c_ptr==spec ? strlen(spec_attr->attr_value.s) :
            strlen(s_ptr->dyn_str.s));

      strcpy(c_ptr, (c_ptr==spec ? spec_attr->attr_value.s : s_ptr->dyn_str.s));
      c_ptr += len;
      *c_ptr++ = ',';
   }
   *c_ptr = '\0';
   if ((spec_copy=UtilStrDup(spec)) == NULL) {
      FailAllocMessage();
      free(spec);
      fclose(fp);
      return;
   }
   total = 0;
   for (c_ptr=strtok(spec_copy, " ,;\t\n\r"); c_ptr != NULL;
         c_ptr=strtok(NULL, " ,;\t\n\r")) {
      total++;
   }
   free(spec_copy);

   buf_sz = 0x400;
   spec_copy = UtilStrDup(spec);
   col_names = (char**)malloc((total+1)*sizeof(char*));
   buf = (char*)malloc((buf_sz+2)*sizeof(char));
   if (spec_copy == NULL || col_names == NULL || buf == NULL) {
      FailAllocMessage();
      if (col_names != NULL) free(col_names);
      if (spec_copy != NULL) free(spec_copy);
      if (buf != NULL) free(buf);
      free(spec);
      fclose(fp);
      return;
   }
   col_names[total] = NULL;
   i = 0;
   for (c_ptr=strtok(spec_copy, " ,;\t\n\r"); c_ptr != NULL && !writeFileFailed;
         c_ptr=strtok(NULL, " ,;\t\n\r"), i++) {
      int len=strlen(c_ptr);

      if ((col_names[i]=(char*)malloc((len+2)*sizeof(char))) == NULL) {
         FailAllocMessage();
         for (total=0; total < i; total++) free(col_names[i]);
         free(col_names);
         free(spec_copy);
         free(buf);
         free(spec);
         fclose(fp);
         return;
      }
      if (fprintf(fp, "%s%s", (i==0 ? "" : "\t"), c_ptr) < 0) {
         writeFileFailed = TRUE;
      }
      sprintf(col_names[i], "%s=", c_ptr);
   }
   fprintf(fp, "\n");

   num_exported = 0;
   SaveStatusStrings();
   for (sel_ptr=botSel; ok && sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      int percent=(i*10000/numObjSelected)/100, col;
      int something_exported=FALSE, cur_len=0;

      sprintf(gszMsgBox, "Progress: %1d%%", percent);
      SetStringStatus(gszMsgBox);
      XSync(mainDisplay, False);

      for (col=0; ok && col < total; col++) {
         int len;
         struct AttrRec *attr_ptr=FindAttrWithName(sel_ptr->obj,
               col_names[col], NULL);

         if (attr_ptr != NULL) something_exported = TRUE;

         sprintf(gszMsgBox, "%s%s", (col==0 ? "" : "\t"),
               (attr_ptr==NULL ? "": attr_ptr->attr_value.s));
         len = strlen(gszMsgBox);
         while (len+cur_len >= buf_sz) {
            buf_sz += 0x400;
            buf = (char*)realloc(buf, (buf_sz+2)*sizeof(char));
            if (buf == NULL) {
               FailAllocMessage();
               ok = FALSE;
               break;
            }
         }
         if (ok) {
            sprintf(&buf[cur_len], gszMsgBox);
            cur_len += len;
         }
      }
      buf[cur_len] = '\0';
      if (something_exported) {
         num_exported++;
         if (fprintf(fp, "%s\n", buf) < 0) {
            writeFileFailed = TRUE;
            ok = FALSE;
         }
      }
   }
   RestoreStatusStrings();
   sprintf(gszMsgBox, "%1d objects exported.", num_exported);
   Msg(gszMsgBox);

   for (i=0; i < total; i++) free(col_names[i]);
   free(col_names);
   free(spec_copy);
   if (buf != NULL) free(buf);
   free(spec);
   fclose(fp);

   if (writeFileFailed) {
      writeFileFailed = FALSE;
      sprintf(gszMsgBox, "Fail to write to '%s'.\n\nFile system may be full.",
            full_fname);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   } else {
      sprintf(gszMsgBox, "Attributes exported to table file '%s'.",
            short_name ? rest : full_fname);
      Msg(gszMsgBox);
   }
}

void SpecialSubMenu (index)
   int	index;
{
   switch (index)
   {
      case SPECIAL_SYM: MakeSymbolic (); break;
      case SPECIAL_UNSYM: UnMakeSymbolic (); break;
      case SPECIAL_INST: Instantiate (); break;
      case SPECIAL_ICON: MakeIconic (); break;
      case SPECIAL_UNICON: UnMakeIconic (); break;
      case SPECIAL_PUSH: PushIcon (); break;
      case SPECIAL_POP: PopIcon (); break;
      case SPECIAL_ADDATTR: AddAttrs (); break;
      case SPECIAL_DETACHATTR: DetachAttrs (); break;
      case SPECIAL_SHOWATTR: ShowAllAttrs (); break;
      case SPECIAL_SHOWATTRNAME: ShowAllAttrNames (); break;
      case SPECIAL_HIDEATTR: HideAllAttrs (); break;
      case SPECIAL_HIDEATTRNAME: HideAllAttrNames (); break;
      case SPECIAL_MOVEATTR: MoveAttr (); break;
      case SPECIAL_EDITATTR: EditAttrs (); break;
      case SPECIAL_ANIMATESEND: AnimateSel (); break;
      case SPECIAL_ANIMATEFLASH: FlashSelColor (); break;
      case SPECIAL_ADDFILEATTR: AddFileAttrs (); break;
      case SPECIAL_DETACHFILEATTR: DetachFileAttrs (); break;
      case SPECIAL_EDITFILEATTR: EditFileAttrs (); break;
      case SPECIAL_UPDATESYMS: UpdateSymbols (); break;
      case SPECIAL_IMPORT_ATTRS: ImportAttrs (); break;
      case SPECIAL_EXPORT_ATTRS: ExportAttrs (); break;
      case SPECIAL_MERGE_WITH_TABLE: MergeWithTable (); break;
      case SPECIAL_EXPORT_TO_TABLE: ExportToTable (); break;
#ifdef _TGIF_WB
      case SPECIAL_WHITE_BOARD: WhiteBoard (); break;
#endif /* _TGIF_WB */
   }
}

int SpecialMenu (X, Y, TrackMenubar)
   int	X, Y, TrackMenubar;
{
   register int	index;
   int		* fore_colors, * valid, * init_rv;

   DefaultColorArrays (MAXSPECIALMENUS, &fore_colors, &valid, &init_rv, NULL);
   activeMenu = MENU_SPECIAL;
   index = TextMenuLoop (X, Y, specialMenuStr, MAXSPECIALMENUS, fore_colors,
         valid, init_rv, specialMenuDescription, SINGLECOLOR, TrackMenubar);

   if (index >= 0) SpecialSubMenu (index);
   return (index);
}
