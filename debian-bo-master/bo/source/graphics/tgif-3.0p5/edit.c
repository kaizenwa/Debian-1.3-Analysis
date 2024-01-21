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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/edit.c,v 3.1 1996/05/12 05:17:27 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#include "align.e"
#include "arc.e"
#include "attr.e"
#include "auxtext.e"
#include "button.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cutpaste.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#ifndef _NO_EXTERN
#include "edit.e"
#endif
#include "eps.e"
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
#include "pattern.e"
#include "poly.e"
#include "raster.e"
#include "ruler.e"
#include "select.e"
#include "setup.e"
#include "special.e"
#include "spline.e"
#include "stretch.e"
#include "text.e"
#include "util.e"
#include "xbitmap.e"
#include "xpixmap.e"

char * editMenuStr[] =
      { "Redraw         ^r",
        "Duplicate      ^d",
        "Delete         ^x",
        "SelectAll      ^a",
        "Undo           #u",
        "Redo           #*",
        "DeletePoint   ^#d",
        "AddPoint      ^#a",
        "Copy           ^y",
        "Cut              ",
        "Paste         ^#y",
        "PasteFromFile    ",
        "Update         #0",
        "PreciseScale   #)",
        "FlushUndoBuffer  ",
        "PrintMsgBuffer   ",
        "InvertXBitmap ^#f",
        "PushCurChoice  ^e",
        "SpecifyAnArc   #9",
        "CutBit/Pixmap  #;",
        "RestoreImageWH   ",
        "UpdateEPS        ",
        "ConvertIntSpline ",
        "Smooth <-> Hinge ",
        "MakeRegPolygon #\"",
        "BreakUpText    ##",
        "SetSelLineWidth  ",
        "AddColor         ",
        "BreakUpBit/Pixmap",
        "LayoutOnArc      ",
        "PreciseRotate    ",
        "JoinPoly         ",
        "CutPoly          ",
        "GetBoundingBox   ",
        NULL
      };

static char * editMenuDescription[] =
      { "Redraw canvas window",
        "Duplicate selected objects",
        "Delete selected objects",
        "Select all objects on the current page",
        "Undo the most recent command",
        "Redo the most undone command",
        "Delete vertices from a poly/polygon/spline",
        "Add vertices to a polygon/polygon/spline",
        "Copy selected objects into the cut buffer",
        "Cut selected objects into the cut buffer",
        "Paste from the cut buffer",
        "Select a file and paste the content as text",
        "Update selected objects to have current drawing properties",
        "Scale selected objects",
        "Flush undo buffer and unused colors",
        "Display/save the contents of the message window",
        "Invert pixels for selected bitmap objects",
        "Save/restore current drawing mode",
        "Create and specify an arc",
        "Trim/scale a selected bitmap/pixmap object",
        "Restore a seleted bitmap/pixmap/EPS object to its original dimension",
        "Refresh a selected EPS object",
        "Convert selected interpolated splines to regular splines",
        "Toggle smooth/hinge vertices for a selected poly/polygon/spline",
        "Make a selected polygon/closed spline into a regular one",
        "Break up selected text objects into one-character text objects",
        "Change the line width (and arrow width/height) of selected objects",
        "Add colors to the current palette",
        "Break selected bitmap/pixmap object into smaller objects",
        "Layout objects on an arc",
        "Rotate selected objects by a specified degree",
        "Join polylines or open-splines at endpoints",
        "Cut a poly/polygon at a vertex into two pieces",
        "Get polygonal bounding boxes of selected objects",
        NULL
      };

static struct ObjRec	* tmpTopObj, * tmpBotObj;
static struct SelRec	* tmpTopSel, * tmpBotSel;
static int		tmpCount;

static
int ConvertObjIntSpline (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register struct ObjRec	* obj_ptr;
   register int			i, changed=FALSE;

   switch (ObjPtr->type)
   {
      case OBJ_POLY:
         if (ObjPtr->detail.p->curved == LT_INTSPLINE)
         {
            struct PolyRec	* poly_ptr=ObjPtr->detail.p;
            int			new_n, n, index=0;
            char		* smooth=poly_ptr->smooth;
            IntPoint		* vs;

            changed = TRUE;
            if (smooth != NULL) free(smooth);
            n = poly_ptr->n;
            new_n = (n == 2 ? n : ((n-2)<<1)+n);

            vs = (IntPoint*)malloc((new_n+1)*sizeof(IntPoint));
            if (vs == NULL) FailAllocMessage();
            smooth = (char*)malloc((new_n+1)*sizeof(char));
            if (smooth == NULL) FailAllocMessage();

            smooth[0] = smooth[new_n-1] = FALSE;
            vs[0] = poly_ptr->vlist[0];
            vs[new_n-1] = poly_ptr->vlist[n-1];
            for (i=1; i < n-1; i++)
            {
               index++;
               smooth[index] = TRUE;
               vs[index] = poly_ptr->intvlist[(i<<1)-1];
               index++;
               smooth[index] = FALSE;
               vs[index] = poly_ptr->vlist[i];
               index++;
               smooth[index] = TRUE;
               vs[index] = poly_ptr->intvlist[i<<1];
            }
            poly_ptr->curved = LT_SPLINE;
            free(poly_ptr->vlist);
            free(poly_ptr->intvlist);
            poly_ptr->vlist = vs;
            poly_ptr->n = new_n;
            poly_ptr->smooth = smooth;
            poly_ptr->intvlist = NULL;
            poly_ptr->intn = 0;
            AdjObjSplineVs (ObjPtr);
            UpdPolyBBox (ObjPtr, poly_ptr->n, poly_ptr->vlist);
         }
         break;
      case OBJ_POLYGON:
         if (ObjPtr->detail.g->curved == LT_INTSPLINE)
         {
            struct PolygonRec	* polygon_ptr=ObjPtr->detail.g;
            int			new_n, n, index=0;
            char		* smooth=polygon_ptr->smooth;
            IntPoint		* vs;

            changed = TRUE;
            if (smooth != NULL) free(smooth);
            n = polygon_ptr->n;
            new_n = ((n-1)<<1)+n;

            vs = (IntPoint*)malloc((new_n+1)*sizeof(IntPoint));
            if (vs == NULL) FailAllocMessage();
            smooth = (char*)malloc((new_n+1)*sizeof(char));
            if (smooth == NULL) FailAllocMessage();

            smooth[0] = FALSE;
            vs[0] = polygon_ptr->vlist[0];
            for (i=0; i < n-1; i++)
            {
               index++;
               smooth[index] = TRUE;
               vs[index] = polygon_ptr->intvlist[i<<1];
               index++;
               smooth[index] = TRUE;
               vs[index] = polygon_ptr->intvlist[(i<<1)+1];
               index++;
               smooth[index] = FALSE;
               vs[index] = polygon_ptr->vlist[i+1];
            }
            polygon_ptr->curved = LT_SPLINE;
            free(polygon_ptr->vlist);
            free(polygon_ptr->intvlist);
            polygon_ptr->vlist = vs;
            polygon_ptr->n = new_n;
            polygon_ptr->smooth = smooth;
            polygon_ptr->intvlist = NULL;
            polygon_ptr->intn = 0;
            AdjObjSplineVs (ObjPtr);
            UpdPolyBBox (ObjPtr, polygon_ptr->n, polygon_ptr->vlist);
         }
         break;
         break;
      case OBJ_GROUP:
      case OBJ_SYM:
         for (obj_ptr=ObjPtr->detail.r->last; obj_ptr!=NULL;
               obj_ptr=obj_ptr->prev)
            if (ConvertObjIntSpline (obj_ptr))
               changed = TRUE;
         break;
   }
   if (changed) AdjObjBBox (ObjPtr);
   return (changed);
}

void ConvertIntSpline()
{
   struct SelRec *sel_ptr;
   int changed=FALSE;

   if (topSel == NULL) {
      sprintf(gszMsgBox, "No interpolated spline objects selected.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ConvertObjIntSpline(sel_ptr->obj)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      SetFileModified(TRUE);
      RedrawAnArea(botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      UpdSelBBox();
      justDupped = FALSE;
      Msg("Interpolated spline objects converted to splines.");
   } else {
      HighLightForward();
      sprintf(gszMsgBox, "No interpolated spline objects selected.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   HighLightForward();
}

static
void SelectModeToggleSmoothHinge ()
{
   register struct ObjRec	* obj_ptr;
   struct PolyRec		* poly_ptr=NULL;
   struct PolygonRec		* polygon_ptr=NULL;
   int				index, n, pt_toggled=FALSE, toggling=TRUE;
   int				root_x, root_y, old_x, old_y, curved=(-1);
   unsigned int			status;
   Window			root_win, child_win;
   XEvent			input, ev;
   char				* smooth=NULL;

   if (!(topSel != NULL && topSel == botSel &&
         (topSel->obj->type == OBJ_POLY || topSel->obj->type == OBJ_POLYGON)))
   {
      MsgBox ("Please select only one POLY or POLYGON object.",
            TOOL_NAME, INFO_MB);
      return;
   }
   obj_ptr = topSel->obj;
   switch (obj_ptr->type)
   {
      case OBJ_POLY:
         poly_ptr = obj_ptr->detail.p;
         smooth = poly_ptr->smooth;
         curved = poly_ptr->curved;
         break;
      case OBJ_POLYGON:
         polygon_ptr = obj_ptr->detail.g;
         smooth = polygon_ptr->smooth;
         curved = polygon_ptr->curved;
         break;
   }
   if (curved == LT_INTSPLINE)
   {
      sprintf (gszMsgBox, "%s %s",
            "Cannot toggle Smooth/Hinge points for",
            "interpolated spline objects.");
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (smooth == NULL)
   {
      fprintf (stderr, "Bad poly in SelectModeToggleSmoothHinge().\n");
      fprintf (stderr, "Safest thing to do is to save file and exit.\n");
      fprintf (stderr, "Please try to reproduce this error and\n");
      fprintf (stderr, "\tsend bug report to william@cs.ucla.edu.\n");
      fflush (stderr);
      sprintf (gszMsgBox, "%s.\n\n%s.\n\n%s %s.",
            "Bad poly in SelectModeToggleSmoothHinge()",
            "Safest thing to do is to save file and exit",
            "Please try to reproduce this error and",
            "send bug report to william@cs.ucla.edu");
      MsgBox (gszMsgBox, TOOL_NAME, STOP_MB);
      return;
   }
   PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);
   SaveStatusStrings ();
   SetMouseStatus ("Toggle smooth/hinge points", "Finish", "Finish");
   TwoLineMsg ("Click left mouse button to toggle Smooth/Hinge points.",
         "Click other buttons to quit.");

   XGrabPointer (mainDisplay, drawWindow, False,
         PointerMotionMask | ButtonPressMask,
         GrabModeAsync, GrabModeAsync, None, defaultCursor, CurrentTime);
   XQueryPointer (mainDisplay, drawWindow, &root_win, &child_win,
         &root_x, &root_y, &old_x, &old_y, &status);
   XSetFont (mainDisplay, revDefaultGC, defaultFontPtr->fid);
   XDrawString (mainDisplay, drawWindow, revDefaultGC,
         old_x+4, old_y+defaultFontAsc, "S/H", 3);
   MarkRulers (old_x, old_y);

   while (toggling)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
         ExposeEventHandler (&input, TRUE);
      else if (input.type == ButtonPress)
      {
         if (input.xbutton.button == Button1)
         {
            if ((obj_ptr->type == OBJ_POLY &&
                  PtInPolyMark (obj_ptr, input.xbutton.x, input.xbutton.y,
                  poly_ptr->n, poly_ptr->vlist, &index) &&
                  index != 0 && index != poly_ptr->n-1) ||
                  (obj_ptr->type == OBJ_POLYGON &&
                  PtInPolyMark (obj_ptr, input.xbutton.x, input.xbutton.y,
                  polygon_ptr->n-1, polygon_ptr->vlist, &index)))
            {
               int	sel_ltx=selLtX, sel_lty=selLtY;
               int	sel_rbx=selRbX, sel_rby=selRbY;

               pt_toggled = TRUE;
               HighLightReverse ();
               switch (obj_ptr->type)
               {
                  case OBJ_POLY:
                     n = poly_ptr->n;
                     if (smooth[index])
                        smooth[index] = FALSE;
                     else
                        smooth[index] = TRUE;
                     AdjObjSplineVs (obj_ptr);
                     UpdPolyBBox (obj_ptr, poly_ptr->n, poly_ptr->vlist);
                     break;
                  case OBJ_POLYGON:
                     n = polygon_ptr->n;
                     if (smooth[index])
                        smooth[index] = FALSE;
                     else
                        smooth[index] = TRUE;
                     if (index == 0) smooth[n-1] = smooth[0];
                     AdjObjSplineVs (obj_ptr);
                     UpdPolyBBox (obj_ptr, polygon_ptr->n, polygon_ptr->vlist);
                     break;
               }
               AdjObjBBox (obj_ptr);

               XDrawString (mainDisplay, drawWindow, revDefaultGC, old_x+4,
                     old_y+defaultFontAsc, "S/H", 3);
               old_x = input.xbutton.x;
               old_y = input.xbutton.y;
               UpdSelBBox ();
               RedrawAreas (botObj,
                     sel_ltx-GRID_ABS_SIZE(1), sel_lty-GRID_ABS_SIZE(1),
                     sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1),
                     selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
                     selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
               HighLightForward ();
               if (obj_ptr != NULL)
                  XDrawString (mainDisplay, drawWindow, revDefaultGC,
                        old_x+4, old_y+defaultFontAsc, "S/H", 3);

               SetFileModified (TRUE);
               justDupped = FALSE;
            }
         }
         else
         {
            XUngrabPointer (mainDisplay, CurrentTime);
            Msg ("");
            toggling = FALSE;
            XDrawString (mainDisplay, drawWindow, revDefaultGC,
                  old_x+4, old_y+defaultFontAsc, "S/H", 3);
         }
      }
      else if (input.type == MotionNotify)
      {
         XDrawString (mainDisplay, drawWindow, revDefaultGC,
               old_x+4, old_y+defaultFontAsc, "S/H", 3);
         old_x = input.xmotion.x;
         old_y = input.xmotion.y;
         XDrawString (mainDisplay, drawWindow, revDefaultGC,
               old_x+4, old_y+defaultFontAsc, "S/H", 3);
         MarkRulers (old_x, old_y);
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   RestoreStatusStrings ();
   if (pt_toggled)
      RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
   else
      AbortPrepareCmd (CMD_REPLACE);
}

void ToggleSmoothHinge ()
{
   register int		i;
   int			changed=FALSE;
   int			ltx=selLtX, lty=selLtY, rbx=selRbX, rby=selRbY;
   struct VSelRec	* vsel_ptr;
   struct ObjRec	* obj_ptr;

   if (curChoice == NOTHING)
   {
      SelectModeToggleSmoothHinge ();
      return;
   }
   if (curChoice != VERTEXMODE)
   {
      sprintf (gszMsgBox, "%s %s.",
            "Can only toggle smooth/hinge points in",
            "'vertex' or 'select' mode.");
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (topVSel == NULL)
   {
      if (topSel == NULL) return;
      HighLightReverse ();
      JustRemoveAllVSel ();
      HighLightForward ();
      SelectModeToggleSmoothHinge ();
      return;
   }
   for (vsel_ptr=topVSel; vsel_ptr != NULL; vsel_ptr=vsel_ptr->next)
   {
      if ((vsel_ptr->obj->type == OBJ_POLY &&
            vsel_ptr->obj->detail.p->curved == LT_INTSPLINE) ||
            (vsel_ptr->obj->type == OBJ_POLYGON &&
            vsel_ptr->obj->detail.g->curved == LT_INTSPLINE))
      {
         sprintf (gszMsgBox, "%s.",
               "Cannot toggle smooth/hinge points for an interpolated spline");
         MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
         return;
      }
   }
   HighLightReverse ();
   StartCompositeCmd ();
   for (vsel_ptr=botVSel; vsel_ptr != NULL; vsel_ptr=vsel_ptr->prev)
   {
      int	obj_changed=FALSE, n;
      IntPoint	* v;
      char	* smooth=NULL;

      obj_ptr = vsel_ptr->obj;
      switch (obj_ptr->type)
      {
         case OBJ_POLY:
            v = obj_ptr->detail.p->vlist;
            n = obj_ptr->detail.p->n;
            smooth = obj_ptr->detail.p->smooth;
            for (i=0; i < vsel_ptr->n; i++)
               if (vsel_ptr->v_index[i] != 0 ||
                     vsel_ptr->v_index[i] != n-1)
                  break;
            if (i == vsel_ptr->n) continue;
            break;
         case OBJ_POLYGON:
            v = obj_ptr->detail.g->vlist;
            n = obj_ptr->detail.g->n;
            smooth = obj_ptr->detail.g->smooth;
            break;
         default: continue;
      }
      PrepareToReplaceAnObj (obj_ptr);
      for (i=0; i < vsel_ptr->n; i++)
      {
         int	index=vsel_ptr->v_index[i];

         if (!(obj_ptr->type == OBJ_POLY && (index == 0 || index == n-1)))
         {
            smooth[index] = (smooth[index] ? FALSE : TRUE);
            obj_changed = TRUE;
         }
      }
      AdjObjSplineVs (obj_ptr);
      UpdPolyBBox (obj_ptr, n, v);
      if (obj_changed)
      {
         RecordReplaceAnObj (obj_ptr);
         changed = TRUE;
      }
      else
         AbortPrepareCmd (CMD_REPLACE);
   }
   EndCompositeCmd ();
   if (changed)
   {
      Msg ("Selected vertices' smoothness are toggled.");
      UpdSelBBox ();
      RedrawAreas (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      SetFileModified (TRUE);
      justDupped = FALSE;
   }
   HighLightForward ();
}

#define BREAK_CHAR 0
#define BREAK_WORD 1
#define BREAK_LINE 2

static
void BreakATextObj(ObjPtr, how)
   struct ObjRec *ObjPtr;
   int how;
{
   struct ObjRec *prototype;
   struct StrRec *s_ptr, *prev_str;
   struct TextRec *text_ptr;
   struct SelRec *new_sel_ptr, *sel_ptr;
   int x, y, xinc=0, yinc=0, tx_to_move=0, ty_to_move=0;
   int transformed=(ObjPtr->ctm!=NULL);

   tmpTopSel = tmpBotSel = NULL;
   tmpTopObj = tmpBotObj = NULL;
   tmpCount = 0;

   prototype = DupObj(ObjPtr);

   text_ptr = prototype->detail.t;
   if (text_ptr->cached_bitmap != None) {
      XFreePixmap(mainDisplay, text_ptr->cached_bitmap);
   }
   text_ptr->cached_bitmap = None;

   for (s_ptr=text_ptr->last->prev; s_ptr!=NULL; s_ptr=prev_str) {
      prev_str = s_ptr->prev;
      FreeStr(s_ptr);
   }
   text_ptr->first = text_ptr->last;
   DynStrSet(&text_ptr->first->dyn_str, "X");
   text_ptr->lines = 1;

   text_ptr = ObjPtr->detail.t;

   SaveCurFont();
   curFont = text_ptr->font;
   curStyle = text_ptr->style;
   curSize = text_ptr->size;
   textJust = text_ptr->just;
   textVSpace = text_ptr->v_space;
   curRotate = text_ptr->rotate;
   SetCanvasFont();

   x = ObjPtr->x;
   y = ObjPtr->y;
   switch (curRotate) {
   case ROTATE0: xinc = 0; yinc = textCursorH+textVSpace; break;
   case ROTATE90: xinc = -(textCursorH+textVSpace); yinc = 0; break;
   case ROTATE180: xinc = 0; yinc = -(textCursorH+textVSpace); break;
   case ROTATE270: xinc = textCursorH+textVSpace; yinc = 0; break;
   }
   if (transformed) {
      TransformPointThroughCTM(0, 0, prototype->ctm, &tx_to_move, &ty_to_move);
   }
   for (s_ptr=text_ptr->first; s_ptr!=NULL; s_ptr=s_ptr->next) {
      char *c_ptr=s_ptr->dyn_str.s;
      int w, x_pos, y_pos, len;

      len = strlen(c_ptr);
      w = XTextWidth(canvasFontPtr, c_ptr, len);

      x_pos = x;
      y_pos = y;
      switch (textJust) {
      case JUST_L: break;
      case JUST_C:
         switch (curRotate) {
         case ROTATE0: x_pos -= w/2; break;
         case ROTATE90: y_pos -= w/2; break;
         case ROTATE180: x_pos += w/2; break;
         case ROTATE270: y_pos += w/2; break;
         }
         break;
      case JUST_R:
         switch (curRotate) {
         case ROTATE0: x_pos -= w; break;
         case ROTATE90: y_pos -= w; break;
         case ROTATE180: x_pos += w; break;
         case ROTATE270: y_pos += w; break;
         }
         break;
      }

      for ( ; *c_ptr != '\0'; c_ptr++) {
         int lbearing, rextra, char_w, dx, dy, x_dist, y_dist, new_x=0, new_y=0;
         struct ObjRec *new_obj=DupObj(prototype);
         char *word_start=NULL;

         switch (how) {
         case BREAK_CHAR:
            *new_obj->detail.t->first->dyn_str.s = *c_ptr;
            break;
         case BREAK_WORD:
            word_start = c_ptr;
            while (*c_ptr == ' ' || *c_ptr == '\t') c_ptr++;
            if (*c_ptr == '\0') {
               c_ptr = NULL;
            } else {
               char saved_char, *prev_ptr=NULL;

               while (*c_ptr!=' ' && *c_ptr!='\t' && *c_ptr!='\0') c_ptr++;
               for ( ; *c_ptr == ' ' || *c_ptr == '\t'; c_ptr++) {
                  prev_ptr = c_ptr;
               }
               saved_char = *c_ptr;
               *c_ptr = '\0';
               DynStrSet(&new_obj->detail.t->first->dyn_str, word_start);
               *c_ptr = saved_char;
               if (prev_ptr != NULL) c_ptr = prev_ptr;
            }
            break;
         case BREAK_LINE:
            DynStrSet(&new_obj->detail.t->first->dyn_str, c_ptr);
            c_ptr += strlen(c_ptr);
            break;
         }
         if (c_ptr == NULL) break;

         GetStrSizeInfo(new_obj->detail.t->first, &char_w, &lbearing, &rextra);

         dx = dy = 0;
         switch (textJust) {
         case JUST_L: break;
         case JUST_C:
            switch (curRotate) {
            case ROTATE0: dx = char_w/2; break;
            case ROTATE90: dy = char_w/2; break;
            case ROTATE180: dx = -(char_w/2); break;
            case ROTATE270: dy = -(char_w/2); break;
            }
            break;
         case JUST_R:
            switch (curRotate) {
            case ROTATE0: dx = char_w; break;
            case ROTATE90: dy = char_w; break;
            case ROTATE180: dx = -char_w; break;
            case ROTATE270: dy = -char_w; break;
            }
            break;
         }
         if (new_obj->ctm == NULL) {
            x_dist = x_pos+dx-prototype->x;
            y_dist = y_pos+dy-prototype->y;
            MoveObj(new_obj, x_dist, y_dist);
            SetTextBBox(new_obj, textJust, char_w, textCursorH, lbearing,
                  rextra, curRotate);
         } else {
            TransformPointThroughCTM(x_pos+dx-prototype->x,
                  y_pos+dy-prototype->y, prototype->ctm, &new_x, &new_y);
            new_x += prototype->x;
            new_y += prototype->y;
            free(new_obj->ctm);
            new_obj->ctm = NULL;
            SetTextBBox(new_obj, textJust, char_w, textCursorH, lbearing,
                  rextra, curRotate);
         }
         switch (curRotate) {
         case ROTATE0: x_pos += char_w; break;
         case ROTATE90: y_pos += char_w; break;
         case ROTATE180: x_pos -= char_w; break;
         case ROTATE270: y_pos -= char_w; break;
         }
         if (transformed) {
            SetCTM(new_obj, prototype->ctm);
            MoveObj(new_obj, new_x-new_obj->x-tx_to_move,
                  new_y-new_obj->y-ty_to_move);
         }
         new_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
         if (new_sel_ptr == NULL) FailAllocMessage();
         new_sel_ptr->obj = new_obj;

         new_sel_ptr->next = tmpTopSel;
         new_obj->next = tmpTopObj;
         if (tmpTopSel == NULL) {
            tmpBotSel = new_sel_ptr;
            tmpBotObj = new_obj;
         } else {
            tmpTopSel->prev = new_sel_ptr;
            tmpTopObj->prev = new_obj;
         }
         tmpTopSel = new_sel_ptr;
         tmpTopObj = new_obj;
         tmpTopSel->prev = NULL;
         tmpTopObj->prev = NULL;
         tmpCount++;

         if (*c_ptr == '\0') break;
      }
      x += xinc;
      y += yinc;
   }
   RestoreCurFont();
   FreeTextObj(prototype);
   for (sel_ptr=tmpTopSel; sel_ptr != NULL; sel_ptr=sel_ptr->next) {
      struct ObjRec *obj_ptr=sel_ptr->obj;

      UpdTextBBox(obj_ptr);
      AdjObjSplineVs(obj_ptr);
      AdjObjBBox(obj_ptr);
   }
}

static
int GetBreakSpec()
{
   int how=INVALID, can_do_lines=FALSE, can_do_words=FALSE;
   char spec[MAXSTRING+1];
   struct SelRec *sel_ptr;

   for (sel_ptr=topSel; sel_ptr != NULL; sel_ptr=sel_ptr->next) {
      struct ObjRec *obj_ptr=sel_ptr->obj;

      if (obj_ptr->type == OBJ_TEXT && !obj_ptr->locked) {
         struct StrRec *s_ptr;
         struct TextRec *text_ptr=obj_ptr->detail.t;
         int lines=0;

         if (text_ptr->read_only) continue;
         for (s_ptr=text_ptr->first; s_ptr!=NULL; s_ptr=s_ptr->next) {
            lines++;
            if (strchr(s_ptr->dyn_str.s, ' ') != NULL) {
               can_do_words = TRUE;
            }
         }
         if (lines > 1) can_do_lines = TRUE;
      }
      if (can_do_lines && can_do_words) break;
   }
   how = BREAK_CHAR;
   if (can_do_lines) {
      if (can_do_words) {
         sprintf(gszMsgBox, "%s? [cwl](c)",
               "Would you like to break text at char/word/line boundaries");
      } else {
         sprintf(gszMsgBox, "%s? [cl](c)",
               "Would you like to break text at char/line boundaries");
      }
   } else if (can_do_words) {
      sprintf(gszMsgBox, "%s? [cw](c)",
            "Would you like to break text at char/word boundaries");
   } else {
      return BREAK_CHAR;
   }
   if (Dialog(gszMsgBox, NULL, spec)==INVALID) {
      return INVALID;
   }
   UtilTrimBlanks(spec);
   strcpy(gszMsgBox, spec);
   gszMsgBox[1] = '\0';
   switch (*gszMsgBox) {
   case 'C': *gszMsgBox = 'c'; break;
   case 'W': *gszMsgBox = 'w'; break;
   case 'L': *gszMsgBox = 'l'; break;
   }
   if (can_do_lines) {
      if (can_do_words) {
         switch (*gszMsgBox) {
         case 'c': how = BREAK_CHAR; break;
         case 'w': how = BREAK_WORD; break;
         case 'l': how = BREAK_LINE; break;
         }
      } else {
         switch (*gszMsgBox) {
         case 'c': how = BREAK_CHAR; break;
         case 'l': how = BREAK_LINE; break;
         }
      }
   } else if (can_do_words) {
      switch (*gszMsgBox) {
      case 'c': how = BREAK_CHAR; break;
      case 'w': how = BREAK_WORD; break;
      }
   }
   if (how == INVALID) {
      sprintf(gszMsgBox, "Invalid specification on how to break up text.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
   return how;
}

void BreakUpText()
{
   struct ObjRec *obj_ptr;
   struct SelRec *sel_ptr, *next_sel;
   int sel_ltx, sel_lty, sel_rbx, sel_rby;
   int changed=FALSE, read_only_text_exists=FALSE;
   int how=INVALID;

   if (topSel == NULL) {
      sprintf(gszMsgBox, "No text objects selected to break up.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if ((how=GetBreakSpec()) == INVALID) return;

   HighLightReverse();
   sel_ltx = selLtX; sel_lty = selLtY;
   sel_rbx = selRbX; sel_rby = selRbY;

   StartCompositeCmd();
   for (sel_ptr=topSel; sel_ptr != NULL; sel_ptr=next_sel) {
      next_sel = sel_ptr->next;
      obj_ptr = sel_ptr->obj;
      if (obj_ptr->type == OBJ_TEXT && !obj_ptr->locked) {
         if (obj_ptr->detail.t->read_only) {
            read_only_text_exists = TRUE;
            continue;
         }
         changed = TRUE;

         PrepareToReplaceAnObj(obj_ptr);
         BreakATextObj(obj_ptr, how);

         tmpTopSel->obj->prev = obj_ptr->prev;
         if (obj_ptr->prev == NULL) {
            curPage->top = topObj = tmpTopSel->obj;
         } else {
            obj_ptr->prev->next = tmpTopSel->obj;
         }
         tmpBotSel->obj->next = obj_ptr->next;
         if (obj_ptr->next == NULL) {
            curPage->bot = botObj = tmpBotSel->obj;
         } else {
            obj_ptr->next->prev = tmpBotSel->obj;
         }
         RecordCmd(CMD_ONE_TO_MANY, NULL, tmpTopSel, tmpBotSel, tmpCount);

         tmpTopSel->prev = sel_ptr->prev;
         if (sel_ptr->prev == NULL) {
            topSel = tmpTopSel;
         } else {
            sel_ptr->prev->next = tmpTopSel;
         }
         tmpBotSel->next = sel_ptr->next;
         if (sel_ptr->next == NULL) {
            botSel = tmpBotSel;
         } else {
            sel_ptr->next->prev = tmpBotSel;
         }
         FreeObj(obj_ptr);
         free(sel_ptr);
      }
   }
   EndCompositeCmd();
   if (read_only_text_exists) {
      MsgBox("Some text are not broken up due to unavailable text size.",
            TOOL_NAME, INFO_MB);
   }
   if (changed) {
      UpdSelBBox();
      RedrawAreas(botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1),
            sel_ltx-GRID_ABS_SIZE(1), sel_lty-GRID_ABS_SIZE(1),
            sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1));
      SetFileModified(TRUE);
      justDupped = FALSE;
      Msg("Text string is broken up into characters.");
   } else if (!read_only_text_exists) {
      sprintf(gszMsgBox, "No text objects selected to break up.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
   HighLightForward();
}

void MakeRegularPolygon ()
{
   register int		i;
   int			vertex_at_right, xc, yc;
   int			sel_ltx, sel_lty, sel_rbx, sel_rby, sides, radius;
   int			ltx, lty, rbx, rby;
   double		inc, angle, r;
   struct ObjRec	* obj_ptr, * new_obj_ptr;
   struct PolygonRec	* polygon_ptr;

   if (topSel == NULL) return;
   if (topSel != botSel || topSel->obj->type != OBJ_POLYGON)
   {
      Msg ("Please select one polygon object to make it regular.");
      return;
   }
   if (topSel->obj->locked)
   {
      Msg ("Polygon locked.");
      return;
   }

   PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);

   sel_ltx = selLtX; sel_lty = selLtY;
   sel_rbx = selRbX; sel_rby = selRbY;

   obj_ptr = topSel->obj;

   radius = (min(obj_ptr->obbox.rbx-obj_ptr->obbox.ltx,
         obj_ptr->obbox.rby-obj_ptr->obbox.lty))>>1;
   if (radius < 1)
   {
      Msg ("Selected polygon is too small to make regular.");
      return;
   }

   sprintf (gszMsgBox,
         "Do you want a vertex at the 3 o'clock position?  [ync](y)");
   if ((vertex_at_right = MsgBox (gszMsgBox, TOOL_NAME, YNC_MB)) ==
         MB_ID_CANCEL)
      return;

   tmpTopObj = tmpBotObj = NULL;
   tmpTopSel = tmpBotSel = NULL;

   HighLightReverse ();

   new_obj_ptr = DupObj (obj_ptr);

   UnlinkObj (obj_ptr);

   polygon_ptr = new_obj_ptr->detail.g;
   sides = polygon_ptr->n-1;
   inc = 2*M_PI/sides;
   angle = (vertex_at_right==MB_ID_YES) ? 0 : inc/2;

   xc = obj_ptr->obbox.ltx+radius;
   yc = obj_ptr->obbox.lty+radius;

   if ((sides%4)==0 && vertex_at_right==MB_ID_NO)
      r = ((double)(min(obj_ptr->obbox.rbx-obj_ptr->obbox.ltx,
            obj_ptr->obbox.rby-obj_ptr->obbox.lty)) / cos(angle)) / 2;
   else
      r = radius;

   ltx = obj_ptr->obbox.rbx;
   lty = obj_ptr->obbox.rby;
   rbx = obj_ptr->obbox.ltx;
   rby = obj_ptr->obbox.lty;
   for (i = 0; i < sides; i++, angle+=inc)
   {
      polygon_ptr->vlist[i].x = xc + round(r*cos(angle));
      polygon_ptr->vlist[i].y = yc - round(r*sin(angle));
      if (polygon_ptr->vlist[i].x < ltx) ltx = polygon_ptr->vlist[i].x;
      if (polygon_ptr->vlist[i].y < lty) lty = polygon_ptr->vlist[i].y;
      if (polygon_ptr->vlist[i].x > rbx) rbx = polygon_ptr->vlist[i].x;
      if (polygon_ptr->vlist[i].y > rby) rby = polygon_ptr->vlist[i].y;
   }
   polygon_ptr->vlist[sides].x = polygon_ptr->vlist[0].x;
   polygon_ptr->vlist[sides].y = polygon_ptr->vlist[0].y;
   new_obj_ptr->obbox.ltx = ltx;
   new_obj_ptr->obbox.lty = lty;
   new_obj_ptr->obbox.rbx = rbx;
   new_obj_ptr->obbox.rby = rby;
   AdjObjSplineVs (new_obj_ptr);
   AdjObjBBox (new_obj_ptr);

   topSel->obj = botSel->obj = new_obj_ptr;
   AddObj (NULL, topObj, new_obj_ptr);
   RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
   FreeObj (obj_ptr);

   UpdSelBBox ();
   RedrawAnArea (botObj, sel_ltx-GRID_ABS_SIZE(1), sel_lty-GRID_ABS_SIZE(1),
         sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1));
   SetFileModified (TRUE);
   justDupped = FALSE;
   HighLightForward ();
}

void DeletePoint()
{
   int i, n, pt_deleted=FALSE, deleting=TRUE, root_x, root_y, old_x, old_y;
   struct ObjRec *obj_ptr;
   struct PolyRec *poly_ptr=NULL;
   struct PolygonRec *polygon_ptr=NULL;
   unsigned int status;
   Window root_win, child_win;

   if (!(topSel != NULL && topSel == botSel &&
         (topSel->obj->type == OBJ_POLY || topSel->obj->type == OBJ_POLYGON))) {
      sprintf(gszMsgBox, "Please select only one POLY or POLYGON object.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (topSel->obj->locked) {
      sprintf(gszMsgBox, "Cannot delete points for locked object.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (curChoice == VERTEXMODE) {
      HighLightReverse();
      JustRemoveAllVSel();
      HighLightForward();
   }
   PrepareToRecord(CMD_REPLACE, topSel, botSel, numObjSelected);
   obj_ptr = topSel->obj;
   switch (obj_ptr->type) {
   case OBJ_POLY: poly_ptr = obj_ptr->detail.p; break;
   case OBJ_POLYGON: polygon_ptr = obj_ptr->detail.g; break;
   }
   SaveStatusStrings();
   SetMouseStatus("Delete a vertex", "Finish", "Finish");
   TwoLineMsg("Click left mouse button to DELETE points.",
         "Click other buttons to quit.");

   XGrabPointer(mainDisplay, drawWindow, False,
         PointerMotionMask | ButtonPressMask,
         GrabModeAsync, GrabModeAsync, None, defaultCursor, CurrentTime);
   XQueryPointer(mainDisplay, drawWindow, &root_win, &child_win,
         &root_x, &root_y, &old_x, &old_y, &status);
   XSetFont(mainDisplay, revDefaultGC, defaultFontPtr->fid);
   XDrawString(mainDisplay, drawWindow, revDefaultGC,
         old_x+4, old_y+defaultFontAsc, "DEL", 3);
   MarkRulers(old_x, old_y);

   while (deleting) {
      XEvent input;

      XNextEvent(mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.type == ButtonPress) {
         if (input.xbutton.button == Button1) {
            int index;

            if ((obj_ptr->type == OBJ_POLY &&
                  PtInPolyMark(obj_ptr, input.xbutton.x, input.xbutton.y,
                  poly_ptr->n, poly_ptr->vlist, &index)) ||
                  (obj_ptr->type == OBJ_POLYGON &&
                  PtInPolyMark(obj_ptr, input.xbutton.x, input.xbutton.y,
                  polygon_ptr->n-1, polygon_ptr->vlist, &index))) {
               pt_deleted = TRUE;
               HighLightReverse();
               if ((obj_ptr->type == OBJ_POLY && poly_ptr->n == 2) ||
                     (obj_ptr->type == OBJ_POLYGON && polygon_ptr->n == 4)) {
                  XUngrabPointer(mainDisplay, CurrentTime);
                  Msg("");
                  DelObj(obj_ptr);
                  deleting = FALSE;
                  obj_ptr = NULL;
                  free(topSel);
                  topSel = botSel = NULL;

                  XDrawString(mainDisplay, drawWindow, revDefaultGC, old_x+4,
                        old_y+defaultFontAsc, "DEL", 3);
                  old_x = input.xbutton.x;
                  old_y = input.xbutton.y;
                  RedrawAnArea(botObj,
                        selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
                        selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
                  HighLightForward();
                  UpdSelBBox();
               } else {
                  int sel_ltx=selLtX, sel_lty=selLtY;
                  int sel_rbx=selRbX, sel_rby=selRbY;
                  char *smooth=NULL;

                  switch (obj_ptr->type) {
                  case OBJ_POLY:
                     n = poly_ptr->n;
                     smooth = poly_ptr->smooth;
                     for (i = index+1; i < n; i++) {
                        poly_ptr->vlist[i-1] = poly_ptr->vlist[i];
                        if (smooth != NULL) smooth[i-1] = smooth[i];
                     }
                     if (smooth != NULL) {
                        if (index == 0) {
                           smooth[0] = FALSE;
                        } else if (index == n-1) {
                           smooth[n-2] = FALSE;
                        }
                     }
                     poly_ptr->n--;
                     AdjObjSplineVs(obj_ptr);
                     if (poly_ptr->curved != LT_INTSPLINE) {
                        UpdPolyBBox(obj_ptr, poly_ptr->n, poly_ptr->vlist);
                     } else {
                        UpdPolyBBox(obj_ptr, poly_ptr->intn,
                              poly_ptr->intvlist);
                     }
                     break;
                  case OBJ_POLYGON:
                     n = polygon_ptr->n;
                     smooth = polygon_ptr->smooth;
                     for (i = index+1; i < n; i++) {
                        polygon_ptr->vlist[i-1] = polygon_ptr->vlist[i];
                        if (smooth != NULL) smooth[i-1] = smooth[i];
                     }
                     polygon_ptr->n--;
                     n--;
                     if (index == 0) {
                        polygon_ptr->vlist[n-1] = polygon_ptr->vlist[0];
                        if (smooth != NULL) smooth[n-1] = smooth[0];
                     }
                     AdjObjSplineVs(obj_ptr);
                     if (polygon_ptr->curved != LT_INTSPLINE) {
                        UpdPolyBBox(obj_ptr, polygon_ptr->n,
                              polygon_ptr->vlist);
                     } else {
                        UpdPolyBBox(obj_ptr, polygon_ptr->intn,
                              polygon_ptr->intvlist);
                     }
                     break;
                  }
                  AdjObjBBox(obj_ptr);

                  XDrawString(mainDisplay, drawWindow, revDefaultGC, old_x+4,
                        old_y+defaultFontAsc, "DEL", 3);
                  old_x = input.xbutton.x;
                  old_y = input.xbutton.y;
                  UpdSelBBox();
                  RedrawAreas(botObj,
                        sel_ltx-GRID_ABS_SIZE(1), sel_lty-GRID_ABS_SIZE(1),
                        sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1),
                        selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
                        selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
                  HighLightForward();
                  if (obj_ptr != NULL) {
                     XDrawString (mainDisplay, drawWindow, revDefaultGC,
                           old_x+4, old_y+defaultFontAsc, "DEL", 3);
                  }
               }
               SetFileModified(TRUE);
               justDupped = FALSE;
            }
         } else {
            XUngrabPointer(mainDisplay, CurrentTime);
            Msg("");
            deleting = FALSE;
            XDrawString(mainDisplay, drawWindow, revDefaultGC,
                  old_x+4, old_y+defaultFontAsc, "DEL", 3);
         }
      } else if (input.type == MotionNotify) {
         XEvent ev;

         XDrawString(mainDisplay, drawWindow, revDefaultGC,
               old_x+4, old_y+defaultFontAsc, "DEL", 3);
         old_x = input.xmotion.x;
         old_y = input.xmotion.y;
         XDrawString(mainDisplay, drawWindow, revDefaultGC,
               old_x+4, old_y+defaultFontAsc, "DEL", 3);
         MarkRulers(old_x, old_y);
         while (XCheckMaskEvent(mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   RestoreStatusStrings();
   if (pt_deleted) {
      if (topSel == NULL) {
         ChangeReplaceOneCmdToDeleteCmd();
      } else {
         RecordCmd(CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
      }
   } else {
      AbortPrepareCmd(CMD_REPLACE);
   }
}

static XComposeStatus	c_stat;

static
int ContinueAddPolyPoint(ObjPtr, MouseX, MouseY, Index, PolyPtr,
      LastMouseX, LastMouseY)
   struct ObjRec *ObjPtr;
   int MouseX, MouseY, Index;
   struct PolyRec *PolyPtr;
   int *LastMouseX, *LastMouseY;
   /* (MouseX,MouseY) is the mouse's origin in screen offsets */
{
   int n=PolyPtr->n, sn=0, curved=PolyPtr->curved;
   int already_moved=FALSE, done=FALSE, before=FALSE;
   XPoint v[3], *sv=NULL;
   IntPoint *vs=PolyPtr->vlist, *pv=NULL, *cntrlv=NULL;
   int prev_x, prev_y, prev_tx, prev_ty, x, y, tx, ty, tmp_x, tmp_y;
   int next_x, next_y, next_tx, next_ty;
   int orig_x, orig_y, grid_x, grid_y, new_mouse_x, new_mouse_y;
   int sel_ltx, sel_lty, sel_rbx, sel_rby, num=0, i, intn=0;
   char *smooth=PolyPtr->smooth, *tmp_smooth=NULL;
   char buf[80], w_buf[80], h_buf[80], x_buf[80], y_buf[80];
   double prev_angle, next_angle;

   sel_ltx = selLtX; sel_lty = selLtY;
   sel_rbx = selRbX; sel_rby = selRbY;

   x = tx = vs[Index].x;
   y = ty = vs[Index].y;
   if (ObjPtr->ctm != NULL) {
      TransformPointThroughCTM(x-ObjPtr->x, y-ObjPtr->y, ObjPtr->ctm,
            &tmp_x, &tmp_y);
      tx = ObjPtr->x+tmp_x;
      ty = ObjPtr->y+tmp_y;
   }
   if (curved == LT_INTSPLINE || smooth == NULL || !smooth[Index]) {
      MARK(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
   } else {
      MARKO(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
   }
   if (Index == 0)
   {
      next_x = next_tx = vs[1].x;
      next_y = next_ty = vs[1].y;
      prev_x = prev_tx = 2*x-next_x;
      prev_y = prev_ty = 2*y-next_y;
   }
   else if (Index == n-1)
   {
      prev_x = next_tx = vs[n-2].x;
      prev_y = next_ty = vs[n-2].y;
      next_x = prev_tx = 2*x-prev_x;
      next_y = prev_ty = 2*y-prev_y;
   }
   else
   {
      prev_x = prev_tx = vs[Index-1].x;
      prev_y = prev_ty = vs[Index-1].y;
      next_x = next_tx = vs[Index+1].x;
      next_y = next_ty = vs[Index+1].y;
   }
   if (ObjPtr->ctm != NULL) {
      TransformPointThroughCTM(next_x-ObjPtr->x, next_y-ObjPtr->y,
            ObjPtr->ctm, &tmp_x, &tmp_y);
      next_tx = ObjPtr->x+tmp_x;
      next_ty = ObjPtr->y+tmp_y;
      TransformPointThroughCTM(prev_x-ObjPtr->x, prev_y-ObjPtr->y,
            ObjPtr->ctm, &tmp_x, &tmp_y);
      prev_tx = ObjPtr->x+tmp_x;
      prev_ty = ObjPtr->y+tmp_y;
   }
   prev_angle = (prev_tx==tx) ? ((prev_ty>=ty) ? M_PI/2.0 : -M_PI/2.0) :
         atan2((double)(prev_ty-ty), (double)(prev_tx-tx));
   next_angle = (next_tx==tx) ? ((next_ty>=ty) ? M_PI/2.0 : -M_PI/2.0) :
         atan2((double)(next_ty-ty), (double)(next_tx-tx));
   if (splineRubberband) {
      pv = (IntPoint*)malloc((n+2)*sizeof(IntPoint));
      if (pv == NULL) FailAllocMessage();
      if (curved != LT_INTSPLINE && smooth != NULL) {
         tmp_smooth = (char*)malloc((n+2)*sizeof(char));
         if (tmp_smooth == NULL) FailAllocMessage();
      }
      if (ObjPtr->ctm == NULL) {
         for (i = 0; i <= Index; i++) {
            pv[i].x = vs[i].x;
            pv[i].y = vs[i].y;
            if (tmp_smooth != NULL) tmp_smooth[i] = smooth[i];
         }
         for (i = Index; i < n; i++) {
            pv[i+1].x = vs[i].x;
            pv[i+1].y = vs[i].y;
            if (tmp_smooth != NULL) tmp_smooth[i+1] = smooth[i];
         }
      } else {
         for (i = 0; i <= Index; i++) {
            TransformPointThroughCTM(vs[i].x-ObjPtr->x, vs[i].y-ObjPtr->y,
                  ObjPtr->ctm, &tmp_x, &tmp_y);
            pv[i].x = ObjPtr->x+tmp_x;
            pv[i].y = ObjPtr->y+tmp_y;
            if (tmp_smooth != NULL) tmp_smooth[i] = smooth[i];
         }
         for (i = Index; i < n; i++) {
            TransformPointThroughCTM(vs[i].x-ObjPtr->x, vs[i].y-ObjPtr->y,
                  ObjPtr->ctm, &tmp_x, &tmp_y);
            pv[i+1].x = ObjPtr->x+tmp_x;
            pv[i+1].y = ObjPtr->y+tmp_y;
            if (tmp_smooth != NULL) tmp_smooth[i+1] = smooth[i];
         }
      }
   }
   GridXY(MouseX, MouseY, &orig_x, &orig_y);
   grid_x = orig_x;
   grid_y = orig_y;
   new_mouse_x = MouseX; new_mouse_y = MouseY;
   PixelToMeasurementUnit(w_buf, 0);
   PixelToMeasurementUnit(h_buf, 0);
   PixelToMeasurementUnit(x_buf, tx);
   PixelToMeasurementUnit(y_buf, ty);
   sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
   StartShowMeasureCursor(OFFSET_X(tx), OFFSET_Y(ty), buf, FALSE);

   while (!done) {
      double new_angle, theta_1, theta_2;
      XEvent input, ev;

      XNextEvent(mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler (&input, TRUE);
      } else if (input.type == MotionNotify) {
         int new_x, new_y;

         new_mouse_x = input.xmotion.x;
         new_mouse_y = input.xmotion.y;

         GridXY(new_mouse_x, new_mouse_y, &grid_x, &grid_y);
         new_x = ABS_SIZE(new_mouse_x-MouseX) + tx;
         new_y = ABS_SIZE(new_mouse_y-MouseY) + ty;
         if (!already_moved) {
            already_moved = TRUE;

            new_angle = (new_x==tx) ? ((new_y>=ty) ? M_PI/2.0 : -M_PI/2.0) :
                  atan2((double)(new_y-ty), (double)(new_x-tx));
            theta_1 = fabs(prev_angle - new_angle);
            theta_2 = fabs(next_angle - new_angle);
            if (theta_1 > M_PI) theta_1 = 2*M_PI-theta_1;
            if (theta_2 > M_PI) theta_2 = 2*M_PI-theta_2;
            before = (theta_1 <= theta_2);

            if (before) {
               /* Add a point between the current and the previous point */
               if (Index == 0) {
                  num = 2;
                  v[0].x = OFFSET_X(tx); v[0].y = OFFSET_Y(ty);
                  v[1].x = OFFSET_X(tx); v[1].y = OFFSET_Y(ty);
               } else {
                  num = 3;
                  v[0].x = OFFSET_X(prev_tx); v[0].y = OFFSET_Y(prev_ty);
                  v[1].x = OFFSET_X(tx);      v[1].y = OFFSET_Y(ty);
                  v[2].x = OFFSET_X(tx);      v[2].y = OFFSET_Y(ty);
               }
            } else {
               /* Add a point between the current and the next point */
               if (Index == n-1) {
                  num = 2;
                  v[0].x = OFFSET_X(tx);      v[0].y = OFFSET_Y(ty);
                  v[1].x = OFFSET_X(tx);      v[1].y = OFFSET_Y(ty);
               } else {
                  num = 3;
                  v[0].x = OFFSET_X(tx);      v[0].y = OFFSET_Y(ty);
                  v[1].x = OFFSET_X(tx);      v[1].y = OFFSET_Y(ty);
                  v[2].x = OFFSET_X(next_tx); v[2].y = OFFSET_Y(next_ty);
               }
            }
            if (splineRubberband) {
               switch (curved) {
               case LT_STRAIGHT:
               case LT_SPLINE:
                  sv = MakeMultiSplinePolyVertex(&sn, tmp_smooth,
                        drawOrigX, drawOrigY, n+1, pv);
                  break;
               case LT_INTSPLINE:
                  sv = MakeIntSplinePolyVertex(&sn, &intn, &cntrlv,
                        drawOrigX, drawOrigY, n+1, pv);
                  break;
               }
               XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                     CoordModeOrigin);
            }
         } else {
            PixelToMeasurementUnit(w_buf, abs(ABS_X(v[1].x)-tx));
            PixelToMeasurementUnit(h_buf, abs(ABS_Y(v[1].y)-ty));
            PixelToMeasurementUnit(x_buf, ABS_X(v[1].x));
            PixelToMeasurementUnit(y_buf, ABS_Y(v[1].y));
            sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            ShowMeasureCursor(v[1].x, v[1].y, buf, FALSE);
            if (splineRubberband) {
               XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                     CoordModeOrigin);
            } else {
               MyDashedLine(drawWindow, revDefaultGC, v, num);
            }
            v[1].x = OFFSET_X(tx) + grid_x - orig_x;
            v[1].y = OFFSET_Y(ty) + grid_y - orig_y;
            if (splineRubberband) {
               free(sv);
               if (before) {
                  pv[Index].x = tx + ABS_SIZE(grid_x-orig_x);
                  pv[Index].y = ty + ABS_SIZE(grid_y-orig_y);
               } else {
                  pv[Index+1].x = tx + ABS_SIZE(grid_x-orig_x);
                  pv[Index+1].y = ty + ABS_SIZE(grid_y-orig_y);
               }
               switch (curved) {
               case LT_STRAIGHT:
               case LT_SPLINE:
                  sv = MakeMultiSplinePolyVertex(&sn, tmp_smooth,
                        drawOrigX, drawOrigY, n+1, pv);
                  break;
               case LT_INTSPLINE:
                  free(cntrlv);
                  sv = MakeIntSplinePolyVertex(&sn, &intn, &cntrlv,
                        drawOrigX, drawOrigY, n+1, pv);
                  break;
               }
               XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                     CoordModeOrigin);
            } else {
               MyDashedLine(drawWindow, revDefaultGC, v, num);
            }
            MarkRulers(v[1].x, v[1].y);
            PixelToMeasurementUnit(w_buf, abs(ABS_X(v[1].x)-tx));
            PixelToMeasurementUnit(h_buf, abs(ABS_Y(v[1].y)-ty));
            PixelToMeasurementUnit(x_buf, ABS_X(v[1].x));
            PixelToMeasurementUnit(y_buf, ABS_Y(v[1].y));
            sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            ShowMeasureCursor(v[1].x, v[1].y, buf, FALSE);
         }
         while (XCheckMaskEvent(mainDisplay, PointerMotionMask, &ev)) ;
      } else if (input.type == ButtonRelease) {
         done = TRUE;
         *LastMouseX = new_mouse_x; *LastMouseY = new_mouse_y;
         if (curved == LT_INTSPLINE || smooth == NULL || !smooth[Index]) {
            MARK(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
         } else {
            MARKO(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
         }
         if (already_moved) {
            PixelToMeasurementUnit(w_buf, abs(ABS_X(v[1].x)-tx));
            PixelToMeasurementUnit(h_buf, abs(ABS_Y(v[1].y)-ty));
            PixelToMeasurementUnit(x_buf, ABS_X(v[1].x));
            PixelToMeasurementUnit(y_buf, ABS_Y(v[1].y));
            sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            EndShowMeasureCursor(v[1].x, v[1].y, buf, FALSE);
         } else {
            PixelToMeasurementUnit(w_buf, 0);
            PixelToMeasurementUnit(h_buf, 0);
            PixelToMeasurementUnit(x_buf, tx);
            PixelToMeasurementUnit(y_buf, ty);
            sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            StartShowMeasureCursor(OFFSET_X(tx), OFFSET_Y(ty), buf, FALSE);
         }
         if (!already_moved) {
            return FALSE;
         } else {
            if (splineRubberband) {
               XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                     CoordModeOrigin);
            }
            if (grid_x == orig_x && grid_y == orig_y) {
               return FALSE;
            }
         }
         HighLightReverse();
         vs = (IntPoint*)realloc(vs, (n+2)*sizeof(IntPoint));
         if (vs == NULL) {
            Warning("ContinueAddPolyPoint()", "Cannot realloc()");
            return FALSE;
         }
         if (smooth != NULL) {
            smooth = (char*)realloc(smooth, (n+2)*sizeof(char));
            if (smooth == NULL) {
               Warning("ContinueAddPolyPoint()", "Cannot realloc()");
               return FALSE;
            }
         }
         PolyPtr->vlist = vs;
         PolyPtr->smooth = smooth;
         if (before) {
            for (i = n-1; i >= Index; i--) {
               vs[i+1] = vs[i];
               if (smooth != NULL) smooth[i+1] = smooth[i];
            }
            if (ObjPtr->ctm == NULL) {
               vs[Index].x = x + ABS_SIZE(grid_x-orig_x);
               vs[Index].y = y + ABS_SIZE(grid_y-orig_y);
            } else {
               vs[Index].x = tx + ABS_SIZE(grid_x-orig_x);
               vs[Index].y = ty + ABS_SIZE(grid_y-orig_y);
               ReverseTransformPointThroughCTM(vs[Index].x-ObjPtr->x,
                        vs[Index].y-ObjPtr->y, ObjPtr->ctm, &tmp_x, &tmp_y);
               vs[Index].x = ObjPtr->x+tmp_x;
               vs[Index].y = ObjPtr->y+tmp_y;
            }
            if (smooth != NULL) smooth[Index] = smooth[Index+1];
         } else {
            for (i = n-1; i > Index; i--) {
               vs[i+1] = vs[i];
               if (smooth != NULL) smooth[i+1] = smooth[i];
            }
            if (ObjPtr->ctm == NULL) {
               vs[Index+1].x = x + ABS_SIZE(grid_x-orig_x);
               vs[Index+1].y = y + ABS_SIZE(grid_y-orig_y);
            } else {
               vs[Index+1].x = tx + ABS_SIZE(grid_x-orig_x);
               vs[Index+1].y = ty + ABS_SIZE(grid_y-orig_y);
               ReverseTransformPointThroughCTM(vs[Index+1].x-ObjPtr->x,
                        vs[Index+1].y-ObjPtr->y, ObjPtr->ctm, &tmp_x, &tmp_y);
               vs[Index+1].x = ObjPtr->x+tmp_x;
               vs[Index+1].y = ObjPtr->y+tmp_y;
            }
            if (smooth != NULL) smooth[Index+1] = smooth[Index];
         }
         if (splineRubberband) {
            free(sv);
            free(pv);
            if (tmp_smooth != NULL) free(tmp_smooth);
            if (curved == LT_INTSPLINE && cntrlv != NULL) free(cntrlv);
         }
         PolyPtr->n++;
         n++;
         AdjObjSplineVs(ObjPtr);
         if (curved != LT_INTSPLINE) {
            UpdPolyBBox(ObjPtr, PolyPtr->n, PolyPtr->vlist);
         } else {
            UpdPolyBBox(ObjPtr, PolyPtr->intn, PolyPtr->intvlist);
         }
         AdjObjBBox(ObjPtr);

         UpdSelBBox();
         RedrawAreas(botObj,
               sel_ltx-GRID_ABS_SIZE(1), sel_lty-GRID_ABS_SIZE(1),
               sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1),
               selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
               selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
         HighLightForward();
         SetFileModified(TRUE);
         justDupped = FALSE;
      } else if (input.type == KeyPress) {
         KeySym key_sym;
         char s[80];

         XLookupString(&(input.xkey), s, 80-1, &key_sym, &c_stat);
         TranslateKeys(s, &key_sym);
         if (s[0] == '\033' && (key_sym & 0xff) == '\033') {
            done = TRUE;
            *LastMouseX = new_mouse_x; *LastMouseY = new_mouse_y;
            if (curved == LT_INTSPLINE || smooth == NULL || !smooth[Index]) {
               MARK(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
            } else {
               MARKO(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
            }
            if (already_moved) {
               PixelToMeasurementUnit(w_buf, abs(ABS_X(v[1].x)-tx));
               PixelToMeasurementUnit(h_buf, abs(ABS_Y(v[1].y)-ty));
               PixelToMeasurementUnit(x_buf, ABS_X(v[1].x));
               PixelToMeasurementUnit(y_buf, ABS_Y(v[1].y));
               sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
               EndShowMeasureCursor(v[1].x, v[1].y, buf, FALSE);
            } else {
               PixelToMeasurementUnit(w_buf, 0);
               PixelToMeasurementUnit(h_buf, 0);
               PixelToMeasurementUnit(x_buf, tx);
               PixelToMeasurementUnit(y_buf, ty);
               sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
               StartShowMeasureCursor(OFFSET_X(tx), OFFSET_Y(ty), buf, FALSE);
            }
            if (already_moved) {
               if (splineRubberband) {
                  XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                        CoordModeOrigin);
               } else {
                  MyDashedLine(drawWindow, revDefaultGC, v, 3);
               }
               if (grid_x == orig_x && grid_y == orig_y) {
                  return FALSE;
               }
            }
            return FALSE;
         }
      }
   }
   return TRUE;
}

static
int ContinueAddPolygonPoint(ObjPtr, MouseX, MouseY, Index, PolygonPtr,
      LastMouseX, LastMouseY)
   struct ObjRec *ObjPtr;
   int MouseX, MouseY, Index;
   struct PolygonRec *PolygonPtr;
   int *LastMouseX, *LastMouseY;
   /* (MouseX,MouseY) is the mouse's origin in screen offsets */
{
   int n=PolygonPtr->n, sn=0, curved=PolygonPtr->curved;
   int already_moved=FALSE, done=FALSE, before=FALSE;
   XPoint v[3], *sv=NULL;
   IntPoint *vs=PolygonPtr->vlist, *pv=NULL, *cntrlv=NULL;
   int prev_x, prev_y, prev_tx, prev_ty, x, y, tx, ty, tmp_x, tmp_y;
   int next_x, next_y, next_tx, next_ty;
   int orig_x, orig_y, grid_x, grid_y, new_mouse_x, new_mouse_y;
   int sel_ltx, sel_lty, sel_rbx, sel_rby, i, intn=0;
   char *smooth=PolygonPtr->smooth, *tmp_smooth=NULL;
   char buf[80], w_buf[80], h_buf[80], x_buf[80], y_buf[80];
   double prev_angle, next_angle;

   sel_ltx = selLtX; sel_lty = selLtY;
   sel_rbx = selRbX; sel_rby = selRbY;

   x = tx = vs[Index].x;
   y = ty = vs[Index].y;
   if (ObjPtr->ctm != NULL) {
      TransformPointThroughCTM(x-ObjPtr->x, y-ObjPtr->y, ObjPtr->ctm,
            &tmp_x, &tmp_y);
      tx = ObjPtr->x+tmp_x;
      ty = ObjPtr->y+tmp_y;
   }
   if (curved == LT_INTSPLINE || smooth == NULL || !smooth[Index]) {
      MARK(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
   } else {
      MARKO(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
   }
   if (Index == 0 || Index == n-1) {
      next_x = next_tx = vs[1].x;
      next_y = next_ty = vs[1].y;
      prev_x = prev_tx = vs[n-2].x;
      prev_y = prev_ty = vs[n-2].y;
   } else {
      prev_x = prev_tx = vs[Index-1].x;
      prev_y = prev_ty = vs[Index-1].y;
      next_x = next_tx = vs[Index+1].x;
      next_y = next_ty = vs[Index+1].y;
   }
   if (ObjPtr->ctm != NULL) {
      TransformPointThroughCTM(next_x-ObjPtr->x, next_y-ObjPtr->y,
            ObjPtr->ctm, &tmp_x, &tmp_y);
      next_tx = ObjPtr->x+tmp_x;
      next_ty = ObjPtr->y+tmp_y;
      TransformPointThroughCTM(prev_x-ObjPtr->x, prev_y-ObjPtr->y,
            ObjPtr->ctm, &tmp_x, &tmp_y);
      prev_tx = ObjPtr->x+tmp_x;
      prev_ty = ObjPtr->y+tmp_y;
   }
   prev_angle = (prev_tx==tx) ? ((prev_ty>=ty) ? M_PI/2.0 : -M_PI/2.0) :
         atan2((double)(prev_ty-ty), (double)(prev_tx-tx));
   next_angle = (next_tx==tx) ? ((next_ty>=ty) ? M_PI/2.0 : -M_PI/2.0) :
         atan2((double)(next_ty-ty), (double)(next_tx-tx));
   if (splineRubberband) {
      pv = (IntPoint *)malloc((n+2)*sizeof(IntPoint));
      if (pv == NULL) FailAllocMessage();
      if (curved != LT_INTSPLINE && smooth != NULL) {
         tmp_smooth = (char*)malloc((n+2)*sizeof(char));
         if (tmp_smooth == NULL) FailAllocMessage();
      }
      if (ObjPtr->ctm == NULL) {
         for (i = 0; i <= Index; i++) {
            pv[i].x = vs[i].x;
            pv[i].y = vs[i].y;
            if (tmp_smooth != NULL) tmp_smooth[i] = smooth[i];
         }
         for (i = Index; i < n; i++) {
            pv[i+1].x = vs[i].x;
            pv[i+1].y = vs[i].y;
            if (tmp_smooth != NULL) tmp_smooth[i+1] = smooth[i];
         }
      } else {
         for (i = 0; i <= Index; i++) {
            TransformPointThroughCTM(vs[i].x-ObjPtr->x, vs[i].y-ObjPtr->y,
                  ObjPtr->ctm, &tmp_x, &tmp_y);
            pv[i].x = ObjPtr->x+tmp_x;
            pv[i].y = ObjPtr->y+tmp_y;
            if (tmp_smooth != NULL) tmp_smooth[i] = smooth[i];
         }
         for (i = Index; i < n; i++) {
            TransformPointThroughCTM(vs[i].x-ObjPtr->x, vs[i].y-ObjPtr->y,
                  ObjPtr->ctm, &tmp_x, &tmp_y);
            pv[i+1].x = ObjPtr->x+tmp_x;
            pv[i+1].y = ObjPtr->y+tmp_y;
            if (tmp_smooth != NULL) tmp_smooth[i+1] = smooth[i];
         }
      }
   }
   GridXY(MouseX, MouseY, &orig_x, &orig_y);
   grid_x = orig_x;
   grid_y = orig_y;
   new_mouse_x = MouseX; new_mouse_y = MouseY;
   PixelToMeasurementUnit(w_buf, 0);
   PixelToMeasurementUnit(h_buf, 0);
   PixelToMeasurementUnit(x_buf, tx);
   PixelToMeasurementUnit(y_buf, ty);
   sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
   StartShowMeasureCursor(OFFSET_X(tx), OFFSET_Y(ty), buf, FALSE);

   while (!done) {
      double new_angle, theta_1, theta_2;
      XEvent input, ev;

      XNextEvent(mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.type == MotionNotify) {
         int new_x, new_y;

         new_mouse_x = input.xmotion.x;
         new_mouse_y = input.xmotion.y;

         GridXY(new_mouse_x, new_mouse_y, &grid_x, &grid_y);
         new_x = ABS_SIZE(new_mouse_x-MouseX) + tx;
         new_y = ABS_SIZE(new_mouse_y-MouseY) + ty;
         if (!already_moved) {
            already_moved = TRUE;

            new_angle = (new_x==tx) ? ((new_y>=ty) ? M_PI/2.0 : -M_PI/2.0) :
                  atan2((double)(new_y-ty), (double)(new_x-tx));
            theta_1 = fabs(prev_angle - new_angle);
            theta_2 = fabs(next_angle - new_angle);
            if (theta_1 > M_PI) theta_1 = 2*M_PI-theta_1;
            if (theta_2 > M_PI) theta_2 = 2*M_PI-theta_2;
            before = (theta_1 <= theta_2);

            if (before) {
               /* Add a point between the current and the previous point */
               v[0].x = OFFSET_X(prev_tx); v[0].y = OFFSET_Y(prev_ty);
               v[1].x = OFFSET_X(tx);      v[1].y = OFFSET_Y(ty);
               v[2].x = OFFSET_X(tx);      v[2].y = OFFSET_Y(ty);
            } else {
               /* Add a point between the current and the next point */
               v[0].x = OFFSET_X(tx);      v[0].y = OFFSET_Y(ty);
               v[1].x = OFFSET_X(tx);      v[1].y = OFFSET_Y(ty);
               v[2].x = OFFSET_X(next_tx); v[2].y = OFFSET_Y(next_ty);
            }
            if (splineRubberband) {
               switch (curved) {
               case LT_STRAIGHT:
               case LT_SPLINE:
                  sv = MakeMultiSplinePolygonVertex(&sn, tmp_smooth,
                        drawOrigX, drawOrigY, n+1, pv);
                  break;
               case LT_INTSPLINE:
                  sv = MakeIntSplinePolygonVertex(&sn, &intn, &cntrlv,
                        drawOrigX, drawOrigY, n+1, pv);
                  break;
               }
               XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                     CoordModeOrigin);
            }
         } else {
            PixelToMeasurementUnit(w_buf, abs(ABS_X(v[1].x)-tx));
            PixelToMeasurementUnit(h_buf, abs(ABS_Y(v[1].y)-ty));
            PixelToMeasurementUnit(x_buf, ABS_X(v[1].x));
            PixelToMeasurementUnit(y_buf, ABS_Y(v[1].y));
            sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            ShowMeasureCursor (v[1].x, v[1].y, buf, FALSE);
            if (splineRubberband) {
               XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                     CoordModeOrigin);
            } else {
               MyDashedLine(drawWindow, revDefaultGC, v, 3);
            }
            v[1].x = OFFSET_X(tx) + grid_x - orig_x;
            v[1].y = OFFSET_Y(ty) + grid_y - orig_y;
            if (splineRubberband) {
               free(sv);
               if (before) {
                  pv[Index].x = tx + ABS_SIZE(grid_x-orig_x);
                  pv[Index].y = ty + ABS_SIZE(grid_y-orig_y);
                  if (Index == 0) {
                     pv[n].x = tx + ABS_SIZE(grid_x-orig_x);
                     pv[n].y = ty + ABS_SIZE(grid_y-orig_y);
                  }
               } else {
                  pv[Index+1].x = tx + ABS_SIZE(grid_x-orig_x);
                  pv[Index+1].y = ty + ABS_SIZE(grid_y-orig_y);
                  if (Index == n-1) {
                     pv[0].x = tx + ABS_SIZE(grid_x-orig_x);
                     pv[0].y = ty + ABS_SIZE(grid_y-orig_y);
                  }
               }
               switch (curved) {
               case LT_STRAIGHT:
               case LT_SPLINE:
                  sv = MakeMultiSplinePolygonVertex(&sn, tmp_smooth,
                        drawOrigX, drawOrigY, n+1, pv);
                  break;
               case LT_INTSPLINE:
                  free(cntrlv);
                  sv = MakeIntSplinePolygonVertex(&sn, &intn, &cntrlv,
                        drawOrigX, drawOrigY, n+1, pv);
                  break;
               }
               XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                     CoordModeOrigin);
            } else {
               MyDashedLine(drawWindow, revDefaultGC, v, 3);
            }
            MarkRulers(v[1].x, v[1].y);
            PixelToMeasurementUnit(w_buf, abs(ABS_X(v[1].x)-tx));
            PixelToMeasurementUnit(h_buf, abs(ABS_Y(v[1].y)-ty));
            PixelToMeasurementUnit(x_buf, ABS_X(v[1].x));
            PixelToMeasurementUnit(y_buf, ABS_Y(v[1].y));
            sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            ShowMeasureCursor(v[1].x, v[1].y, buf, FALSE);
         }
         while (XCheckMaskEvent(mainDisplay, PointerMotionMask, &ev)) ;
      } else if (input.type == ButtonRelease) {
         done = TRUE;
         *LastMouseX = new_mouse_x; *LastMouseY = new_mouse_y;
         if (curved == LT_INTSPLINE || smooth == NULL || !smooth[Index]) {
            MARK(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
         } else {
            MARKO(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
         }
         if (already_moved) {
            PixelToMeasurementUnit(w_buf, abs(ABS_X(v[1].x)-tx));
            PixelToMeasurementUnit(h_buf, abs(ABS_Y(v[1].y)-ty));
            PixelToMeasurementUnit(x_buf, ABS_X(v[1].x));
            PixelToMeasurementUnit(y_buf, ABS_Y(v[1].y));
            sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            EndShowMeasureCursor(v[1].x, v[1].y, buf, FALSE);
         } else {
            PixelToMeasurementUnit(w_buf, 0);
            PixelToMeasurementUnit(h_buf, 0);
            PixelToMeasurementUnit(x_buf, tx);
            PixelToMeasurementUnit(y_buf, ty);
            sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            StartShowMeasureCursor(OFFSET_X(tx), OFFSET_Y(ty), buf, FALSE);
         }
         if (!already_moved) {
            return FALSE;
         } else {
            if (splineRubberband) {
               XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                     CoordModeOrigin);
            }
            if (grid_x == orig_x && grid_y == orig_y) {
               return FALSE;
            }
         }
         HighLightReverse();
         vs = (IntPoint*)realloc(vs, (n+2)*sizeof(IntPoint));
         if (vs == NULL) {
            Warning("ContinueAddPolygonPoint()", "Cannot realloc()");
            return FALSE;
         }
         if (smooth != NULL) {
            smooth = (char*)realloc(smooth, (n+2)*sizeof(char));
            if (smooth == NULL) {
               Warning("ContinueAddPolygonPoint()", "Cannot realloc()");
               return FALSE;
            }
         }
         PolygonPtr->vlist = vs;
         PolygonPtr->smooth = smooth;
         if (Index == 0 || Index == n-1) {
            if (before) {
               vs[n].x = vs[n-1].x;
               vs[n].y = vs[n-1].y;
               if (ObjPtr->ctm == NULL) {
                  vs[n-1].x = x + ABS_SIZE(grid_x-orig_x);
                  vs[n-1].y = y + ABS_SIZE(grid_y-orig_y);
               } else {
                  vs[n-1].x = tx + ABS_SIZE(grid_x-orig_x);
                  vs[n-1].y = ty + ABS_SIZE(grid_y-orig_y);
                  ReverseTransformPointThroughCTM(vs[n-1].x-ObjPtr->x,
                        vs[n-1].y-ObjPtr->y, ObjPtr->ctm, &tmp_x, &tmp_y);
                  vs[n-1].x = ObjPtr->x+tmp_x;
                  vs[n-1].y = ObjPtr->y+tmp_y;
               }
               if (smooth != NULL) smooth[n] = smooth[n-1];
            } else {
               for (i = n-1; i > 0; i--) {
                  vs[i+1].x = vs[i].x;
                  vs[i+1].y = vs[i].y;
                  if (smooth != NULL) smooth[i+1] = smooth[i];
               }
               if (ObjPtr->ctm == NULL) {
                  vs[1].x = x + ABS_SIZE(grid_x-orig_x);
                  vs[1].y = y + ABS_SIZE(grid_y-orig_y);
               } else {
                  vs[1].x = tx + ABS_SIZE(grid_x-orig_x);
                  vs[1].y = ty + ABS_SIZE(grid_y-orig_y);
                  ReverseTransformPointThroughCTM(vs[1].x-ObjPtr->x,
                        vs[1].y-ObjPtr->y, ObjPtr->ctm, &tmp_x, &tmp_y);
                  vs[1].x = ObjPtr->x+tmp_x;
                  vs[1].y = ObjPtr->y+tmp_y;
               }
               if (smooth != NULL) smooth[1] = smooth[0];
            }
         } else {
            if (before) {
               for (i = n-1; i >= Index; i--) {
                  vs[i+1].x = vs[i].x;
                  vs[i+1].y = vs[i].y;
                  if (smooth != NULL) smooth[i+1] = smooth[i];
               }
               if (ObjPtr->ctm == NULL) {
                  vs[Index].x = x + ABS_SIZE(grid_x-orig_x);
                  vs[Index].y = y + ABS_SIZE(grid_y-orig_y);
               } else {
                  vs[Index].x = tx + ABS_SIZE(grid_x-orig_x);
                  vs[Index].y = ty + ABS_SIZE(grid_y-orig_y);
                  ReverseTransformPointThroughCTM(vs[Index].x-ObjPtr->x,
                        vs[Index].y-ObjPtr->y, ObjPtr->ctm, &tmp_x, &tmp_y);
                  vs[Index].x = ObjPtr->x+tmp_x;
                  vs[Index].y = ObjPtr->y+tmp_y;
               }
               if (smooth != NULL) smooth[Index] = smooth[Index+1];
            } else {
               for (i = n-1; i > Index; i--) {
                  vs[i+1].x = vs[i].x;
                  vs[i+1].y = vs[i].y;
                  if (smooth != NULL) smooth[i+1] = smooth[i];
               }
               if (ObjPtr->ctm == NULL) {
                  vs[Index+1].x = x + ABS_SIZE(grid_x-orig_x);
                  vs[Index+1].y = y + ABS_SIZE(grid_y-orig_y);
               } else {
                  vs[Index+1].x = tx + ABS_SIZE(grid_x-orig_x);
                  vs[Index+1].y = ty + ABS_SIZE(grid_y-orig_y);
                  ReverseTransformPointThroughCTM(vs[Index+1].x-ObjPtr->x,
                        vs[Index+1].y-ObjPtr->y, ObjPtr->ctm, &tmp_x, &tmp_y);
                  vs[Index+1].x = ObjPtr->x+tmp_x;
                  vs[Index+1].y = ObjPtr->y+tmp_y;
               }
               if (smooth != NULL) smooth[Index+1] = smooth[Index];
            }
         }
         if (splineRubberband) {
            free(sv);
            free(pv);
            if (tmp_smooth != NULL) free(tmp_smooth);
            if (curved == LT_INTSPLINE && cntrlv != NULL) free(cntrlv);
         }
         PolygonPtr->n++;
         n++;
         AdjObjSplineVs(ObjPtr);
         if (curved != LT_INTSPLINE) {
            UpdPolyBBox(ObjPtr, PolygonPtr->n, PolygonPtr->vlist);
         } else {
            UpdPolyBBox(ObjPtr, PolygonPtr->intn, PolygonPtr->intvlist);
         }
         AdjObjBBox(ObjPtr);

         UpdSelBBox();
         RedrawAreas(botObj,
               sel_ltx-GRID_ABS_SIZE(1), sel_lty-GRID_ABS_SIZE(1),
               sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1),
               selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
               selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
         HighLightForward();
         SetFileModified(TRUE);
         justDupped = FALSE;
      } else if (input.type == KeyPress) {
         KeySym key_sym;
         char s[80];

         XLookupString(&(input.xkey), s, 80-1, &key_sym, &c_stat);
         TranslateKeys(s, &key_sym);
         if (s[0] == '\033' && (key_sym & 0xff) == '\033') {
            done = TRUE;
            *LastMouseX = new_mouse_x; *LastMouseY = new_mouse_y;
            if (curved == LT_INTSPLINE || smooth == NULL || !smooth[Index]) {
               MARK(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
            } else {
               MARKO(drawWindow, revDefaultGC, OFFSET_X(tx), OFFSET_Y(ty));
            }
            if (already_moved) {
               PixelToMeasurementUnit(w_buf, abs(ABS_X(v[1].x)-tx));
               PixelToMeasurementUnit(h_buf, abs(ABS_Y(v[1].y)-ty));
               PixelToMeasurementUnit(x_buf, ABS_X(v[1].x));
               PixelToMeasurementUnit(y_buf, ABS_Y(v[1].y));
               sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
               EndShowMeasureCursor(v[1].x, v[1].y, buf, FALSE);
            } else {
               PixelToMeasurementUnit(w_buf, 0);
               PixelToMeasurementUnit(h_buf, 0);
               PixelToMeasurementUnit(x_buf, tx);
               PixelToMeasurementUnit(y_buf, ty);
               sprintf(buf, "ADD: %sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
               StartShowMeasureCursor(OFFSET_X(x), OFFSET_Y(y), buf, FALSE);
            }
            if (already_moved) {
               if (splineRubberband) {
                  XDrawLines(mainDisplay, drawWindow, revDefaultGC, sv, sn,
                        CoordModeOrigin);
               } else {
                  MyDashedLine(drawWindow, revDefaultGC, v, 3);
               }
               if (grid_x == orig_x && grid_y == orig_y) {
                  return FALSE;
               }
            }
            return FALSE;
         }
      }
   }
   return TRUE;
}

void AddPoint()
{
   int adding=TRUE, pt_added=FALSE, root_x, root_y, old_x, old_y;
   struct ObjRec *obj_ptr;
   struct PolyRec *poly_ptr=NULL;
   struct PolygonRec *polygon_ptr=NULL;
   unsigned int status;
   Window root_win, child_win;

   if (!(topSel != NULL && topSel == botSel &&
         (topSel->obj->type == OBJ_POLY || topSel->obj->type == OBJ_POLYGON))) {
      sprintf(gszMsgBox, "Please select only one POLY or POLYGON object.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (topSel->obj->locked) {
      sprintf(gszMsgBox, "Cannot add points for a locked object.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (AutoRetractedArrowAttr(topSel->obj, TRUE)) {
      sprintf(gszMsgBox,
            "Cannot add points for an auto_retracted_arrows object.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (curChoice == VERTEXMODE) {
      HighLightReverse();
      JustRemoveAllVSel();
      HighLightForward();
   }
   PrepareToRecord(CMD_REPLACE, topSel, botSel, numObjSelected);
   obj_ptr = topSel->obj;
   switch (obj_ptr->type) {
   case OBJ_POLY: poly_ptr = obj_ptr->detail.p; break;
   case OBJ_POLYGON: polygon_ptr = obj_ptr->detail.g; break;
   }
   SaveStatusStrings();
   SetMouseStatus("Add a vertex", "Finish", "Finish");
   TwoLineMsg("Drag left mouse button to ADD points.",
         "Click other buttons to quit.");

   XGrabPointer(mainDisplay, drawWindow, False,
         PointerMotionMask | ButtonPressMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, defaultCursor, CurrentTime);
   XQueryPointer(mainDisplay, drawWindow, &root_win, &child_win,
         &root_x, &root_y, &old_x, &old_y, &status);
   XSetFont(mainDisplay, revDefaultGC, defaultFontPtr->fid);
   XDrawString(mainDisplay, drawWindow, revDefaultGC,
         old_x+4, old_y+defaultFontAsc, "ADD", 3);
   MarkRulers(old_x, old_y);

   while (adding) {
      XEvent input;

      XNextEvent(mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.type == ButtonPress) {
         if (input.xbutton.button == Button1) {
            int index;

            XDrawString(mainDisplay, drawWindow, revDefaultGC,
                  old_x+4, old_y+defaultFontAsc, "ADD", 3);
            if (obj_ptr->type == OBJ_POLY &&
                  PtInPolyMark(obj_ptr, input.xbutton.x, input.xbutton.y,
                  poly_ptr->n, poly_ptr->vlist, &index)) {
               if (ContinueAddPolyPoint(obj_ptr, input.xbutton.x,
                     input.xbutton.y, index, poly_ptr, &old_x, &old_y)) {
                  pt_added = TRUE;
               }
            } else if (obj_ptr->type == OBJ_POLYGON &&
                  PtInPolyMark(obj_ptr, input.xbutton.x, input.xbutton.y,
                  polygon_ptr->n-1, polygon_ptr->vlist, &index)) {
               if (ContinueAddPolygonPoint(obj_ptr, input.xbutton.x,
                     input.xbutton.y, index, polygon_ptr, &old_x, &old_y)) {
                  pt_added = TRUE;
               }
            }
            XDrawString(mainDisplay, drawWindow, revDefaultGC,
                  old_x+4, old_y+defaultFontAsc, "ADD", 3);
         } else {
            XUngrabPointer(mainDisplay, CurrentTime);
            Msg("");
            adding = FALSE;
            XDrawString(mainDisplay, drawWindow, revDefaultGC,
                  old_x+4, old_y+defaultFontAsc, "ADD", 3);
         }
      } else if (input.type == MotionNotify) {
         XEvent ev;

         XDrawString(mainDisplay, drawWindow, revDefaultGC,
               old_x+4, old_y+defaultFontAsc, "ADD", 3);
         old_x = input.xmotion.x;
         old_y = input.xmotion.y;
         XDrawString(mainDisplay, drawWindow, revDefaultGC,
               old_x+4, old_y+defaultFontAsc, "ADD", 3);
         MarkRulers(old_x, old_y);
         while (XCheckMaskEvent(mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   RestoreStatusStrings();
   if (pt_added) {
      RecordCmd(CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
   } else {
      AbortPrepareCmd(CMD_REPLACE);
   }
}

void FlushUndoBuffer()
{
   CleanUpMsg();
   CleanUpCmds();
   if (FlushColormap()) {
      Msg("Undo buffer and Colormap flushed.");
      sprintf(gszMsgBox, "%1d color%s allocated.", maxColors,
            (maxColors > 1 ? "s" : ""));
      Msg(gszMsgBox);
   } else {
      Msg("Undo buffer flushed.");
   }
}

void RestoreImageWH ()
{
   struct XBmRec	* xbm_ptr=NULL;
   struct XPmRec	* xpm_ptr=NULL;
   int			w, h, image_w=0, image_h=0, rotate=ROTATE0;
   int			ltx, lty, rbx, rby;

   if (topSel == NULL) return;
   if (topSel != botSel || (!(topSel->obj->type==OBJ_XBM ||
         topSel->obj->type==OBJ_XPM))) {
      sprintf(gszMsgBox,
            "Please select one X Bitmap or X Pixmap object to restore.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (topSel->obj->locked) {
      sprintf(gszMsgBox, "Cannot restore locked object.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   w = topSel->obj->obbox.rbx - topSel->obj->obbox.ltx;
   h = topSel->obj->obbox.rby - topSel->obj->obbox.lty;
   switch (topSel->obj->type)
   {
      case OBJ_XBM:
         xbm_ptr = topSel->obj->detail.xbm;
         if (xbm_ptr->real_type==XBM_EPS && xbm_ptr->bitmap==None)
         {
            image_w = xbm_ptr->eps_w;
            image_h = xbm_ptr->eps_h;
         }
         else
         {
            image_w = xbm_ptr->image_w;
            image_h = xbm_ptr->image_h;
         }
         if (w == image_w && h == image_h) return;
         rotate = xbm_ptr->rotate;
         break;
      case OBJ_XPM:
         xpm_ptr = topSel->obj->detail.xpm;
         image_w = xpm_ptr->image_w; image_h = xpm_ptr->image_h;
         if (w == image_w && h == image_h) return;
         rotate = xpm_ptr->rotate;
         break;
   }
   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
   HighLightReverse ();

   PrepareToReplaceAnObj (topSel->obj);
   if (topSel->obj->ctm == NULL) {
      switch (rotate) {
      case ROTATE0:
      case ROTATE180:
         topSel->obj->obbox.rbx = topSel->obj->obbox.ltx+image_w;
         topSel->obj->obbox.rby = topSel->obj->obbox.lty+image_h;
         break;

      case ROTATE90:
      case ROTATE270:
         topSel->obj->obbox.rbx = topSel->obj->obbox.ltx+image_h;
         topSel->obj->obbox.rby = topSel->obj->obbox.lty+image_w;
         break;
      }
   } else {
      topSel->obj->obbox.rbx = topSel->obj->obbox.ltx+image_w;
      topSel->obj->obbox.rby = topSel->obj->obbox.lty+image_h;
      free(topSel->obj->ctm);
      topSel->obj->ctm = NULL;
   }
   topSel->obj->x = topSel->obj->obbox.ltx;
   topSel->obj->y = topSel->obj->obbox.lty;
   switch (topSel->obj->type)
   {
      case OBJ_XBM:
         if (xbm_ptr->cached_bitmap != None)
            XFreePixmap (mainDisplay, xbm_ptr->cached_bitmap);
         xbm_ptr->cached_bitmap = None;
         xbm_ptr->cached_zoom = 0;
         xbm_ptr->cached_rotate = INVALID;
         break;
      case OBJ_XPM:
         if (xpm_ptr->cached_pixmap != None)
            XFreePixmap (mainDisplay, xpm_ptr->cached_pixmap);
         xpm_ptr->cached_pixmap = None;
         if (xpm_ptr->cached_bitmap != None)
            XFreePixmap (mainDisplay, xpm_ptr->cached_bitmap);
         xpm_ptr->cached_bitmap = None;
         if (xpm_ptr->clip_mask != None)
            XFreePixmap (mainDisplay, xpm_ptr->clip_mask);
         xpm_ptr->clip_mask = None;
         xpm_ptr->cached_zoom = 0;
         xpm_ptr->cached_rotate = INVALID;
         xpm_ptr->cached_color = (-1);
         break;
   }
   AdjObjBBox (topSel->obj);
   RecordReplaceAnObj (topSel->obj);

   UpdSelBBox ();
   RedrawAreas (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward ();
   SetFileModified (TRUE);
   justDupped = FALSE;
}

void CutMaps()
{
   if (topSel == NULL || topSel != botSel) {
      sprintf(gszMsgBox, "Please select a X Bitmap or X Pixmap object to cut.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (topSel->obj->locked) {
      sprintf(gszMsgBox, "Cannot cut locked object.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (topSel->obj->ctm != NULL) {
      sprintf(gszMsgBox, "Cannot cut a stretched/rotated/sheared %s",
            "X Bitmap or X Pixmap object");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   switch (topSel->obj->type) {
   case OBJ_XBM: CutXBitmap(); break;
   case OBJ_XPM: CutXPixmap(NULL, NULL, NULL, NULL); break;
   default:
      sprintf(gszMsgBox, "Please select a X Bitmap or X Pixmap object to cut.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      break;
   }
}

void BreakUpMaps()
{
   int cols, rows, cols_and_rows=TRUE, ok=TRUE, image_w=0, image_h=0;
   char spec[MAXSTRING+1], *dup_spec, *part1=NULL, *part2=NULL;
   struct ObjRec *obj_ptr;

   if (topSel == NULL || topSel != botSel ||
         (topSel->obj->type != OBJ_XBM && topSel->obj->type != OBJ_XPM)) {
      MsgBox("Please select a X Bitmap or X Pixmap object to break up.",
            TOOL_NAME, INFO_MB);
      return;
   }
   obj_ptr = topSel->obj;
   if (obj_ptr->ctm != NULL) {
      sprintf(gszMsgBox, "Cannot break up a stretched/rotated/sheared %s",
            "X Bitmap or X Pixmap object");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (obj_ptr->locked) {
      MsgBox("Cannot break up locked object.", TOOL_NAME, INFO_MB);
      return;
   }
   switch (obj_ptr->type) {
   case OBJ_XBM:
      image_w=obj_ptr->detail.xbm->image_w;
      image_h=obj_ptr->detail.xbm->image_h;
      break;
   case OBJ_XPM:
      image_w=obj_ptr->detail.xpm->image_w;
      image_h=obj_ptr->detail.xpm->image_h;
      break;
   default: return;
   }
   sprintf(gszMsgBox, "%s (original size is %1dx%1d): [Col x Row]",
         "Please enter the number of columns and rows to break into",
         image_w, image_h);
   if (Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", spec)==INVALID) {
      return;
   }
   UtilTrimBlanks(spec);
   if (*spec == '\0') return;
   if ((dup_spec=UtilStrDup(spec)) == NULL) {
      FailAllocMessage();
      return;
   }
   if (*dup_spec == '=') cols_and_rows = FALSE;

   if ((part1=strtok(dup_spec, " ,xX=[]")) != NULL &&
         (part2=strtok(NULL, " ,xX=[]")) != NULL) {
      cols = atoi(part1);
      rows = atoi(part2);
      if (cols > 0 && rows > 0) {
         switch (obj_ptr->type) {
         case OBJ_XBM:
            BreakUpXBitmap(obj_ptr, cols_and_rows, cols, rows);
            break;
         case OBJ_XPM:
            BreakUpXPixmap(obj_ptr, cols_and_rows, cols, rows);
            break;
         }
      } else {
         ok = FALSE;
      }
   } else {
      ok = FALSE;
   }
   if (!ok) {
      sprintf(gszMsgBox, "%s: '%s'.\n\n%s.",
            "Invalid specification", dup_spec,
            "Please enter [Cols x Rows] or [=WxH]");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
   free(dup_spec);
}

/* ----------------------- LayoutOnArc ----------------------- */

typedef struct BoxInfoRec {
   struct ObjRec *obj;
   int w, h;
   int valid_v;
   double half_w, angle_in_radian, angle_to_rotate;
   XPoint v[5];
} *BoxInfoPtr;

#define LAYOUT_DIR_NONE 0
#define LAYOUT_DIR_S 1 /* convex */
#define LAYOUT_DIR_N 2 /* concave */

int gnLayoutDirection=LAYOUT_DIR_NONE;

static
void RotateXY(x, y, angle_in_radian, new_x, new_y)
   int x, y;
   double angle_in_radian;
   short *new_x, *new_y;
{
   if (x == 0 && y == 0) {
      *new_x = 0;
      *new_y = 0;
   } else {
      double sin_val = sin(angle_in_radian);
      double cos_val = cos(angle_in_radian);

      *new_x = (short)round(x*cos_val - y*sin_val);
      *new_y = (short)round(x*sin_val + y*cos_val);
   }
}

static
void RotateBBoxByRadian(bbox, angle_in_radian, v)
   struct BBRec *bbox;
   double angle_in_radian;
   XPoint *v; /* array of 5 points */
{
   RotateXY(bbox->ltx, bbox->lty, angle_in_radian, &(v[0].x), &(v[0].y));
   RotateXY(bbox->rbx, bbox->lty, angle_in_radian, &(v[1].x), &(v[1].y));
   RotateXY(bbox->rbx, bbox->rby, angle_in_radian, &(v[2].x), &(v[2].y));
   RotateXY(bbox->ltx, bbox->rby, angle_in_radian, &(v[3].x), &(v[3].y));
   v[4].x = v[0].x; v[4].y = v[0].y;
}

#define SET_LAYOUT_VS (FALSE)
#define FINALIZE_LAYOUT (TRUE)

static
int HighLightOrFinalLayout(arc_ptr, box_info_ptr, grid_x, grid_y, finalize)
   struct ArcRec *arc_ptr;
   struct BoxInfoRec *box_info_ptr;
   int grid_x, grid_y, finalize;
{
   int i, num_objects=numObjSelected-1;
   int abs_cx, abs_cy, abs_x=ABS_X(grid_x), abs_y=ABS_Y(grid_y), dx, dy;
   double abs_radius, total_arc_radian, total_radian=0, inc_radian, angle;

   abs_cx = arc_ptr->xc;
   abs_cy = arc_ptr->yc;
   dx = abs_x - abs_cx;
   dy = abs_y - abs_cy;
   if (dx == 0 && dy == 0) {
      box_info_ptr[0].valid_v = FALSE;
      return FALSE;
   }
   box_info_ptr[0].valid_v = TRUE;
   total_arc_radian = ((double)arc_ptr->angle2)*M_PI/((double)180.0*64.0);
   abs_radius = (double)sqrt((double)(dx*dx+dy*dy));
   for (i=0; i < num_objects; i++) {
      if (!finalize) {
         box_info_ptr[i].angle_in_radian = atan2(box_info_ptr[i].half_w,
               abs_radius) * ((double)2.0);
      }
      total_radian += fabs(box_info_ptr[i].angle_in_radian);
   }
   switch (gnLayoutDirection) {
   case LAYOUT_DIR_S:
      inc_radian = (total_arc_radian-total_radian)/((double)(num_objects-1));
      break;
   case LAYOUT_DIR_N:
      inc_radian = (total_arc_radian+total_radian)/((double)(num_objects-1));
      break;
   }
   angle = arc_ptr->angle1*M_PI/((double)180.0*64.0);
   for (i=0; i < num_objects; i++, angle+=inc_radian) {
      struct ObjRec *obj_ptr=box_info_ptr[i].obj;
      int x=0, y=0, w=0, h=0, orig_x=0, orig_y=0;
      XPoint v[5];
      struct BBRec bbox;
      double half_way_angle=(double)0.0;

      w = box_info_ptr[i].w;
      h = box_info_ptr[i].h;
      if (finalize) {
         if (obj_ptr->type == OBJ_TEXT && obj_ptr->detail.t->just != JUST_C) {
            ChangeObjTextJust(obj_ptr, JUST_C);
         }
      }
      /*
       * half_way_angle measures angle in the arc sense (see the beginning
       *     of "arc.c").
       * the RoateaBBoxByRadian() rotates things in the drawing sense
       *     (the one that's vertically flipped w.r.t. PostScript)
       */
      switch (gnLayoutDirection) {
      case LAYOUT_DIR_S:
         half_way_angle = angle+(box_info_ptr[i].angle_in_radian/2.0);
         if (finalize) {
            orig_x = ((obj_ptr->obbox.rbx+obj_ptr->obbox.ltx)>>1);
            orig_y = obj_ptr->obbox.lty;
            RotateObjForLayout(obj_ptr, box_info_ptr[i].angle_to_rotate,
                  CORNER_BOTTOM);
            x = abs_cx + (int)((abs_radius-h)*cos(half_way_angle));
            y = abs_cy - (int)((abs_radius-h)*sin(half_way_angle));
         } else {
            SetBBRec(&bbox, -(w>>1), -h, w-(w>>1), 0);
            box_info_ptr[i].angle_to_rotate = (-half_way_angle-(M_PI/2.0));
            RotateBBoxByRadian(&bbox, box_info_ptr[i].angle_to_rotate, v);
            x = abs_cx + (int)(abs_radius*cos(half_way_angle));
            y = abs_cy - (int)(abs_radius*sin(half_way_angle));
         }
         break;
      case LAYOUT_DIR_N:
         half_way_angle = angle-(box_info_ptr[i].angle_in_radian/2.0);
         if (finalize) {
            orig_x = ((obj_ptr->obbox.rbx+obj_ptr->obbox.ltx)>>1);
            orig_y = obj_ptr->obbox.rby;
            RotateObjForLayout(obj_ptr, box_info_ptr[i].angle_to_rotate,
                  CORNER_TOP);
            x = abs_cx + (int)((abs_radius-h)*cos(half_way_angle));
            y = abs_cy - (int)((abs_radius-h)*sin(half_way_angle));
         } else {
            SetBBRec(&bbox, -(w>>1), 0, w-(w>>1), h);
            box_info_ptr[i].angle_to_rotate = (-half_way_angle+(M_PI/2.0));
            RotateBBoxByRadian(&bbox, box_info_ptr[i].angle_to_rotate, v);
            x = abs_cx + (int)(abs_radius*cos(half_way_angle));
            y = abs_cy - (int)(abs_radius*sin(half_way_angle));
         }
         break;
      }
      if (finalize) {
         MoveObj(obj_ptr, x-orig_x, y-orig_y);
      } else {
         box_info_ptr[i].v[0].x = OFFSET_X(x + v[0].x);
         box_info_ptr[i].v[0].y = OFFSET_Y(y + v[0].y);
         box_info_ptr[i].v[1].x = OFFSET_X(x + v[1].x);
         box_info_ptr[i].v[1].y = OFFSET_Y(y + v[1].y);
         box_info_ptr[i].v[2].x = OFFSET_X(x + v[2].x);
         box_info_ptr[i].v[2].y = OFFSET_Y(y + v[2].y);
         box_info_ptr[i].v[3].x = OFFSET_X(x + v[3].x);
         box_info_ptr[i].v[3].y = OFFSET_Y(y + v[3].y);
         box_info_ptr[i].v[4].x = box_info_ptr[i].v[0].x;
         box_info_ptr[i].v[4].y = box_info_ptr[i].v[0].y;
      }
      switch (gnLayoutDirection) {
      case LAYOUT_DIR_S:
         angle += box_info_ptr[i].angle_in_radian;
         break;
      case LAYOUT_DIR_N:
         angle -= box_info_ptr[i].angle_in_radian;
         break;
      }
   }
   return TRUE;
}

#define NO_GENERATE 0
#define GENERATE 1

static
void HighLightLayout(arc_ptr, box_info_ptr, grid_x, grid_y, generate)
   struct ArcRec *arc_ptr;
   struct BoxInfoRec *box_info_ptr;
   int grid_x, grid_y, generate;
{
   int i, num_objects=numObjSelected-1;

   if (generate) {
      HighLightOrFinalLayout(arc_ptr, box_info_ptr, grid_x, grid_y,
            SET_LAYOUT_VS);
   }
   if (box_info_ptr[0].valid_v) {
      for (i=0; i < num_objects; i++) {
         XDrawLines(mainDisplay, drawWindow, revDefaultGC, box_info_ptr[i].v, 5,
               CoordModeOrigin);
      }
   }
}

static
void DoLayoutOnArc(arc_obj, box_info_ptr)
   struct ObjRec *arc_obj;
   struct BoxInfoRec *box_info_ptr;
{
   struct ArcRec *arc_ptr=arc_obj->detail.a;
   int done=FALSE, something_changed=FALSE, grid_x=0, grid_y=0;

   SetMouseStatus("Start layout", "Finish", "Finish");
   XGrabPointer(mainDisplay, drawWindow, FALSE,
         PointerMotionMask | ButtonPressMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   while (!done) {
      XEvent input;

      XNextEvent(mainDisplay, &input);
      switch (input.type) {
      case Expose: ExposeEventHandler(&input, TRUE); break;
      case VisibilityNotify: ExposeEventHandler(&input, TRUE); break;
      case ButtonPress:
         if (input.xbutton.button == Button1) {
            SetMouseStatus("End layout", "", "");
            GridXY(input.xbutton.x, input.xbutton.y, &grid_x, &grid_y);
            HighLightLayout(arc_ptr, box_info_ptr, grid_x, grid_y, GENERATE);
            something_changed = TRUE;
         } else {
            XUngrabPointer(mainDisplay, CurrentTime);
            XSync(mainDisplay, False);
            done = TRUE;
         }
         break;
      case MotionNotify:
         if (something_changed) {
            HighLightLayout(arc_ptr, box_info_ptr, grid_x, grid_y, NO_GENERATE);
            GridXY(input.xmotion.x, input.xmotion.y, &grid_x, &grid_y);
            HighLightLayout(arc_ptr, box_info_ptr, grid_x, grid_y, GENERATE);
         }
         break;
      case ButtonRelease:
         XUngrabPointer(mainDisplay, CurrentTime);
         XSync(mainDisplay, False);
         done = TRUE;
         HighLightLayout(arc_ptr, box_info_ptr, grid_x, grid_y, NO_GENERATE);
      }
   }
   if (something_changed && box_info_ptr[0].valid_v) {
      int ltx=selLtX, lty=selLtY, rbx=selRbX, rby=selRbY;

      HighLightReverse();
      PrepareToRecord(CMD_REPLACE, topSel, botSel, numObjSelected);
      if (HighLightOrFinalLayout(arc_ptr, box_info_ptr, grid_x, grid_y,
            FINALIZE_LAYOUT)) {
         RecordCmd(CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
         UpdSelBBox();
         RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
               rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
               selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
               selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
         SetFileModified(TRUE);
         justDupped = FALSE;
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
      HighLightForward();
   }
}

static
int ObjsAlreadySorted(box_info_ptr, min_index, max_index)
   struct BoxInfoRec *box_info_ptr;
   int min_index, max_index;
{
   int i;

   for (i=min_index; i < max_index; i++) {
      int d=box_info_ptr[i].obj->obbox.ltx-box_info_ptr[i+1].obj->obbox.ltx;

      if (d > 0) {
         return FALSE;
      } else if (d == 0 &&
            box_info_ptr[i].obj->obbox.lty > box_info_ptr[i+1].obj->obbox.lty) {
         return FALSE;
      }
   }
   return TRUE;
}

static
void QuickSortObjs(box_info_ptr, min_index, max_index, level)
   struct BoxInfoRec *box_info_ptr;
   int min_index, max_index, level;
{
   int i, j, pivot_index, pivot_x_value, pivot_y_value, something_swapped;
   struct ObjRec *tmp_obj;

   if (min_index > max_index) return;
   if (ObjsAlreadySorted(box_info_ptr, min_index, max_index)) return;
   pivot_index = max_index;
   pivot_x_value = box_info_ptr[pivot_index].obj->obbox.ltx;
   pivot_y_value = box_info_ptr[pivot_index].obj->obbox.lty;
   i = min_index;
   j = max_index-1;
   something_swapped = FALSE;
   do {
      int d;

      while (TRUE) {
         d = box_info_ptr[i].obj->obbox.ltx-pivot_x_value;
         if (d < 0 || (d == 0 &&
               box_info_ptr[i].obj->obbox.lty < pivot_y_value)) {
            i++;
         } else {
            break;
         }
      }
      while (j > i) {
         d = box_info_ptr[j].obj->obbox.ltx-pivot_x_value;
         if (d > 0 || (d == 0 &&
               box_info_ptr[j].obj->obbox.lty > pivot_y_value)) {
            j--;
         } else {
            break;
         }
      }
      if (j > i) {
         tmp_obj = box_info_ptr[j].obj;
         box_info_ptr[j].obj = box_info_ptr[i].obj;
         box_info_ptr[i].obj = tmp_obj;
         something_swapped = TRUE;
         if (j == i+1) break;
         i++; j--;
      } else {
         break;
      }
   } while (TRUE);
   if (i == max_index) {
      /* pivot corresponds to the largest */
      if (something_swapped) {
         fprintf(stderr, "Huh? min_index=%1d, max_index=%1d, level=%1d\n",
               min_index, max_index, level);
      } else {
         QuickSortObjs(box_info_ptr, min_index, j, level+1);
      }
   } else if (j > i) {
      tmp_obj = box_info_ptr[max_index].obj;
      box_info_ptr[max_index].obj = box_info_ptr[j].obj;
      box_info_ptr[j].obj = tmp_obj;
      QuickSortObjs(box_info_ptr, min_index, j-1, level+1);
      QuickSortObjs(box_info_ptr, j+1, max_index, level+1);
   } else {
      tmp_obj = box_info_ptr[max_index].obj;
      box_info_ptr[max_index].obj = box_info_ptr[i].obj;
      box_info_ptr[i].obj = tmp_obj;
      QuickSortObjs(box_info_ptr, min_index, i-1, level+1);
      QuickSortObjs(box_info_ptr, i+1, max_index, level+1);
   }
}

static
int DecideLayoutDirection(obj_ptr)
   struct ObjRec *obj_ptr;
{
   struct ArcRec *arc_ptr=obj_ptr->detail.a;
   struct BBRec *p_obbox=(&obj_ptr->obbox);
   int cx=arc_ptr->xc, cy=arc_ptr->yc, h_slack, v_slack;
   char spec[MAXSTRING+1];

   h_slack = ((p_obbox->ltx+p_obbox->rbx)>>1) - cx;
   v_slack = ((p_obbox->lty+p_obbox->rby)>>1) - cy;
   if (h_slack == 0) {
      if (v_slack == 0) {
         return LAYOUT_DIR_S;
      } else if (v_slack > 0) {
         return LAYOUT_DIR_S;
      } else {
         return LAYOUT_DIR_N;
      }
   }
   sprintf(gszMsgBox, "%s:",
         "Please specify concave (c) or convex (v)");
   if (Dialog(gszMsgBox, NULL, spec) == INVALID) {
      return LAYOUT_DIR_NONE;
   }
   UtilTrimBlanks(spec);
   if (UtilStrICmp(spec, "concave") == 0) {
      return LAYOUT_DIR_N;
   } else if (UtilStrICmp(spec, "convex") == 0) {
      return LAYOUT_DIR_S;
   } else {
      switch (*spec) {
      case 'c': case 'C': return LAYOUT_DIR_N;
      case 'v': case 'V': return LAYOUT_DIR_S;
      }
   }
   sprintf(gszMsgBox, "Invalid specification: '%s'.", spec);
   MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   return LAYOUT_DIR_NONE;
}

void LayoutOnArc()
{
   struct SelRec *sel_ptr;
   struct ObjRec *obj_ptr, *arc_obj=NULL;
   int arc_obj_count=0, i, something_locked=FALSE;
   struct BoxInfoRec *box_info_ptr;

   if (curChoice != NOTHING) {
      MsgBox("Please select an arc object.", TOOL_NAME, INFO_MB);
      return;
   }
   for (sel_ptr=topSel; sel_ptr != NULL; sel_ptr=sel_ptr->next) {
      struct ObjRec *obj_ptr=sel_ptr->obj;

      if (obj_ptr->type == OBJ_ARC) {
         arc_obj_count++;
         arc_obj = obj_ptr;
      } else if (obj_ptr->locked) {
         something_locked = TRUE;
      }
   }
   if (arc_obj_count == 0) {
      MsgBox("No arc object is selected.", TOOL_NAME, INFO_MB);
      return;
   } else if (arc_obj_count > 1) {
      sprintf(gszMsgBox, "Too many arc objects selected.\n\n%s.",
            "Please select an arc object");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (numObjSelected == 1) {
      sprintf(gszMsgBox, "Nothing to layout on the selected arc.\n\n%s.",
            "Please select other objects in addition to the selected arc");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (something_locked) {
      sprintf(gszMsgBox, "Some objects are locked.\n\n%s.",
            "Please unlock them before retrying");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (arc_obj->ctm != NULL || arc_obj->detail.a->w != arc_obj->detail.a->h) {
      sprintf(gszMsgBox, "%s %s.\n\n%s.",
            "Don't know how to layout objects on an arc object",
            "that has been stretched/sheared/rotated",
            "Please use a simple arc");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   gnLayoutDirection = DecideLayoutDirection(arc_obj);
   if (gnLayoutDirection == LAYOUT_DIR_NONE) {
      return;
   }
   box_info_ptr = (struct BoxInfoRec *)malloc(
         (numObjSelected-1)*sizeof(struct BoxInfoRec));
   if (box_info_ptr == NULL) {
      FailAllocMessage();
      return;
   }
   for (i=0, sel_ptr=topSel; sel_ptr != NULL; sel_ptr=sel_ptr->next) {
      struct ObjRec *obj_ptr=sel_ptr->obj;

      if (obj_ptr->type != OBJ_ARC) {
         box_info_ptr[i].obj = obj_ptr;
         i++;
      }
   }
   QuickSortObjs(box_info_ptr, 0, numObjSelected-2, 0);
   for (i=0; i < numObjSelected-1; i++) {
      struct ObjRec *obj_ptr=box_info_ptr[i].obj;

      box_info_ptr[i].w = obj_ptr->obbox.rbx-obj_ptr->obbox.ltx;
      box_info_ptr[i].h = obj_ptr->obbox.rby-obj_ptr->obbox.lty;
      box_info_ptr[i].half_w = (double)(box_info_ptr[i].w>>1);
      box_info_ptr[i].valid_v = FALSE;
   }
   SaveStatusStrings();
   DoLayoutOnArc(arc_obj, box_info_ptr);
   RestoreStatusStrings();
   free(box_info_ptr);
}

/* ----------------------- PreciseRotate ----------------------- */

static
int FinishPreciseRotate(angle_spec, pivot_x, pivot_y)
   double angle_spec;
   int pivot_x, pivot_y;
{
   struct SelRec *sel_ptr;
   double angle_in_radian=angle_spec*M_PI/180.0;
   double sin_val=sin(angle_in_radian);
   double cos_val=cos(angle_in_radian);

   for (sel_ptr=topSel; sel_ptr != NULL; sel_ptr=sel_ptr->next) {
      struct ObjRec *obj_ptr=sel_ptr->obj;
      int orig_x=((obj_ptr->obbox.ltx+obj_ptr->obbox.rbx)>>1);
      int orig_y=obj_ptr->obbox.lty;
      int x=0, y=0, dx=orig_x-pivot_x, dy=orig_y-pivot_y;

      if (dx != 0 || dy != 0) {
         x = (short)round(dx*cos_val - dy*sin_val);
         y = (short)round(dx*sin_val + dy*cos_val);
      }
      x += pivot_x;
      y += pivot_y;
      /* RotateObjForLayout() rotates about center-top */
      RotateObjForLayout(obj_ptr, angle_in_radian, CORNER_BOTTOM);
      MoveObj(obj_ptr, x-orig_x, y-orig_y);
   }
   return TRUE;
}

void PreciseRotate()
{
   char spec[MAXSTRING+1];
   double angle_spec;
   int arc_count=0, pivot_x=0, pivot_y=0, ltx, lty, rbx, rby;
   struct SelRec *sel_ptr;
   struct ObjRec *arc_obj=NULL;

   if (curChoice == VERTEXMODE) {
      MsgBox("PreciseRotate() is not available in vertex mode.",
            TOOL_NAME, INFO_MB);
      return;
   }
   if (curChoice != NOTHING || topSel == NULL) {
      MsgBox("No objects selected.", TOOL_NAME, INFO_MB);
      return;
   }
   for (sel_ptr=topSel; sel_ptr != NULL; sel_ptr=sel_ptr->next) {
      if (sel_ptr->obj->type == OBJ_ARC) {
         arc_obj = sel_ptr->obj;
         arc_count++;
      }
   }
   if (arc_count == 1) {
      if (arc_obj->ctm == NULL) {
         pivot_x = arc_obj->detail.a->xc;
         pivot_y = arc_obj->detail.a->yc;
      } else {
         struct ArcRec *arc_ptr=arc_obj->detail.a;
         int x, y;

         TransformPointThroughCTM(arc_ptr->xc-arc_obj->x,
               arc_ptr->yc-arc_obj->y, arc_obj->ctm, &x, &y);
         pivot_x = x + arc_obj->x;
         pivot_y = y + arc_obj->y;
      }
   } else {
      pivot_x = (selObjLtX+selObjRbX)>>1;
      pivot_y = (selObjLtY+selObjRbY)>>1;
   }
   sprintf(gszMsgBox, "Please specify an angle in degrees: (%s)",
         "positive angle is counter-clock-wise");
   Dialog(gszMsgBox, NULL, spec);
   UtilTrimBlanks(spec);
   if (*spec == '\0') return;
   if (sscanf(spec, "%lf", &angle_spec) != 1) {
      sprintf(gszMsgBox, "Invalid specification '%s'.\n\n%s.",
            spec, "A numerical value is expected");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (fabs(angle_spec) < (1.0e-5)) return;

   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
   HighLightReverse();
   PrepareToRecord(CMD_REPLACE, topSel, botSel, numObjSelected);
   if (FinishPreciseRotate(angle_spec, pivot_x, pivot_y)) {
      RecordCmd(CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
            selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      SetFileModified(TRUE);
      justDupped = FALSE;
   } else {
      AbortPrepareCmd(CMD_REPLACE);
   }
   HighLightForward();
}

void RotateAllSelObj(angle_spec)
   double angle_spec;
{
   int arc_count=0, pivot_x=0, pivot_y=0, ltx, lty, rbx, rby;
   struct SelRec *sel_ptr;
   struct ObjRec *arc_obj=NULL;

   if (curChoice != NOTHING || topSel == NULL) {
      MsgBox("No objects selected.", TOOL_NAME, INFO_MB);
      return;
   }
   for (sel_ptr=topSel; sel_ptr != NULL; sel_ptr=sel_ptr->next) {
      if (sel_ptr->obj->type == OBJ_ARC) {
         arc_obj = sel_ptr->obj;
         arc_count++;
      }
   }
   if (arc_count == 1) {
      if (arc_obj->ctm == NULL) {
         pivot_x = arc_obj->detail.a->xc;
         pivot_y = arc_obj->detail.a->yc;
      } else {
         struct ArcRec *arc_ptr=arc_obj->detail.a;
         int x, y;

         TransformPointThroughCTM(arc_ptr->xc-arc_obj->x,
               arc_ptr->yc-arc_obj->y, arc_obj->ctm, &x, &y);
         pivot_x = x + arc_obj->x;
         pivot_y = y + arc_obj->y;
      }
   } else {
      pivot_x = (selObjLtX+selObjRbX)>>1;
      pivot_y = (selObjLtY+selObjRbY)>>1;
   }
   if (fabs(angle_spec) < (1.0e-5)) return;

   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
   HighLightReverse();
   PrepareToRecord(CMD_REPLACE, topSel, botSel, numObjSelected);
   FinishPreciseRotate(angle_spec, pivot_x, pivot_y);
   RecordCmd(CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);
   justDupped = FALSE;
}

/* ----------------------- EditMenu ----------------------- */

void EditSubMenu (index)
   int	index;
{
   switch (index)
   {
      case EDIT_REDRAW: ClearAndRedrawDrawWindow (); break;
      case EDIT_DUP: DupSelObj (); break;
      case EDIT_DELETE: DelAllSelObj (); break;
      case EDIT_SELALL: SelAllObj (TRUE); break;
      case EDIT_UNDO: UndoCmd (); break;
      case EDIT_REDO: RedoCmd (); break;
      case EDIT_DEL_POINT: DeletePoint (); break;
      case EDIT_ADD_POINT: AddPoint (); break;
      case EDIT_COPY: CopyToCutBuffer (); break;
      case EDIT_CUT: CutToCutBuffer (); break;
      case EDIT_PASTE: PasteFromCutBuffer (); break;
      case EDIT_PASTE_FROM_FILE: PasteFromFile (); break;
      case EDIT_UPDATE: UpdateSelObjs (); break;
      case EDIT_SCALE: ScaleAllSelObj (); break;
      case EDIT_FLUSH_UNDO: FlushUndoBuffer (); break;
      case EDIT_PRINT_MSG_BUF: PrintMsgBuffer (); break;
      case EDIT_INV_XBM: InvertXBitmaps (); break;
      case EDIT_PUSH_CUR_CHOICE: PushCurChoice (); break;
      case EDIT_MAKE_PREC_ARC: MakePreciseArc (); break;
      case EDIT_CUT_MAPS: CutMaps (); break;
      case EDIT_RESTORE_MAPS: RestoreImageWH (); break;
      case EDIT_UPDATE_EPS: UpdateEPS (); break;
      case EDIT_CV_INTSPLINE: ConvertIntSpline (); break;
      case EDIT_SMOOTH_HINGE: ToggleSmoothHinge (); break;
      case EDIT_MAKE_REGULAR: MakeRegularPolygon (); break;
      case EDIT_BREAK_TEXT: BreakUpText (); break;
      case EDIT_SET_SEL_LINEWIDTH: SetSelectedLineWidth (); break;
      case EDIT_ADD_COLOR: AddColor (); break;
      case EDIT_BREAK_MAPS: BreakUpMaps (); break;
      case EDIT_LAYOUT_ON_ARC: LayoutOnArc (); break;
      case EDIT_PRECISE_ROTATE: PreciseRotate (); break;
      case EDIT_JOIN_POLY: JoinPoly (); break;
      case EDIT_CUT_POLY: CutPoly (); break;
      case EDIT_GET_BBOX: GetBoundingBox (); break;
   }
}

int EditMenu (X, Y, TrackMenubar)
   int	X, Y, TrackMenubar;
{
   int		index, * fore_colors, * valid, * init_rv;

   DefaultColorArrays (MAXEDITMENUS, &fore_colors, &valid, &init_rv, NULL);
   activeMenu = MENU_EDIT;
   index = TextMenuLoop (X, Y, editMenuStr, MAXEDITMENUS, fore_colors, valid,
         init_rv, editMenuDescription, SINGLECOLOR, TrackMenubar);

   if (index >= 0) EditSubMenu (index);
   return (index);
}

/* ======================= ArrangeMenu Related ======================= */

void FrontProc ()
{
   if (topSel != NULL)
   {
      HighLightReverse ();
      MoveSelToTop ();
      RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      HighLightForward ();
      SetFileModified (TRUE);
   }
}

void BackProc ()
{
   if (topSel != NULL)
   {
      HighLightReverse ();
      MoveSelToBot ();
      RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      HighLightForward ();
      SetFileModified (TRUE);
   }
}

void AlignObjsTop ()
{
   register int saved_h_align = horiAlign, saved_v_align = vertAlign;

   horiAlign = ALIGN_N; vertAlign = ALIGN_T;
   AlignSelObjs ();
   horiAlign = saved_h_align; vertAlign = saved_v_align;
}

void AlignObjsMiddle ()
{
   register int saved_h_align = horiAlign, saved_v_align = vertAlign;

   horiAlign = ALIGN_N; vertAlign = ALIGN_M;
   AlignSelObjs ();
   horiAlign = saved_h_align; vertAlign = saved_v_align;
}

void AlignObjsBottom ()
{
   register int saved_h_align = horiAlign, saved_v_align = vertAlign;

   horiAlign = ALIGN_N; vertAlign = ALIGN_B;
   AlignSelObjs ();
   horiAlign = saved_h_align; vertAlign = saved_v_align;
}

void AlignObjsLeft ()
{
   register int saved_h_align = horiAlign, saved_v_align = vertAlign;

   horiAlign = ALIGN_L; vertAlign = ALIGN_N;
   AlignSelObjs ();
   horiAlign = saved_h_align; vertAlign = saved_v_align;
}

void AlignObjsCenter ()
{
   register int saved_h_align = horiAlign, saved_v_align = vertAlign;

   horiAlign = ALIGN_C; vertAlign = ALIGN_N;
   AlignSelObjs ();
   horiAlign = saved_h_align; vertAlign = saved_v_align;
}

void AlignObjsRight ()
{
   register int saved_h_align = horiAlign, saved_v_align = vertAlign;

   horiAlign = ALIGN_R; vertAlign = ALIGN_N;
   AlignSelObjs ();
   horiAlign = saved_h_align; vertAlign = saved_v_align;
}

static
void Abut (Dir)
   int	Dir;
{
   register struct ObjRec	* obj_ptr;
   register struct SelRec	* sel_ptr;
   struct SelRec		* top_sort, * bot_sort, * sort_ptr;
   struct SelRec		* new_sort_ptr, * tmp_sel_ptr, * next_sort;
   struct ObjRec		* sorted_obj, * locked_obj=NULL;
   struct MoveSubCmdRec		* move_cmd;
   struct SubCmdRec		* sub_cmd;
   int				sel_ltx, sel_lty, sel_rbx, sel_rby, rbx, rby;
   int				found, delta, dx=0, dy=0;

   if (topSel == NULL) return;
   if (curChoice == VERTEXMODE)
   {
      Msg ("Cannot abut in vertex mode.");
      return;
   }
   if (numObjLocked > 1)
   {
      Msg ("Cannot abut.  Too many objects locked.");
      return;
   }

   HighLightReverse ();
   StartCompositeCmd ();
   sel_ltx = selLtX; sel_lty = selLtY;
   sel_rbx = selRbX; sel_rby = selRbY;

   top_sort = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (top_sort == NULL) FailAllocMessage();
   top_sort->next = top_sort->prev = NULL;

   top_sort->obj = sorted_obj = botSel->obj;
   if (botSel->obj->locked) locked_obj = botSel->obj;
   for (sel_ptr = botSel->prev; sel_ptr != NULL; sel_ptr = sel_ptr->prev)
   {
      obj_ptr = sel_ptr->obj;
      if (obj_ptr->locked) locked_obj = obj_ptr;
      switch (Dir)
      {
         case ABUT_HORIZONTAL:
            if (obj_ptr->obbox.ltx < sorted_obj->obbox.ltx ||
                  (obj_ptr->obbox.ltx == sorted_obj->obbox.ltx &&
                  obj_ptr->obbox.lty < sorted_obj->obbox.lty))
               top_sort->obj = sorted_obj = sel_ptr->obj;
            break;
         case ABUT_VERTICAL:
            if (obj_ptr->obbox.lty < sorted_obj->obbox.lty ||
                  (obj_ptr->obbox.lty == sorted_obj->obbox.lty &&
                  obj_ptr->obbox.ltx < sorted_obj->obbox.ltx))
               top_sort->obj = sorted_obj = sel_ptr->obj;
            break;
      }
   }
   bot_sort = top_sort;

   for (sel_ptr = botSel; sel_ptr != NULL; sel_ptr = sel_ptr->prev)
   {
      if (sel_ptr->obj == top_sort->obj) continue;

      obj_ptr = sel_ptr->obj;
      new_sort_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (new_sort_ptr == NULL) FailAllocMessage();
      new_sort_ptr->obj = obj_ptr;
      found = FALSE;
      for (sort_ptr=top_sort->next; sort_ptr!=NULL; sort_ptr=sort_ptr->next)
      {
         switch (Dir)
         {
            case ABUT_HORIZONTAL:
               if (sort_ptr->obj->obbox.ltx > obj_ptr->obbox.ltx ||
                     (sort_ptr->obj->obbox.ltx == obj_ptr->obbox.ltx &&
                     sort_ptr->obj->obbox.lty > obj_ptr->obbox.lty))
                  found = TRUE;
               break;
            case ABUT_VERTICAL:
               if (sort_ptr->obj->obbox.lty > obj_ptr->obbox.lty ||
                     (sort_ptr->obj->obbox.lty == obj_ptr->obbox.lty &&
                     sort_ptr->obj->obbox.ltx > obj_ptr->obbox.ltx))
                  found = TRUE;
               break;
         }
         if (found) break;
      }
      new_sort_ptr->next = sort_ptr;
      if (sort_ptr == NULL)
      {
         new_sort_ptr->prev = bot_sort;
         bot_sort->next = new_sort_ptr;
         bot_sort = new_sort_ptr;
      }
      else
      {
         new_sort_ptr->prev = sort_ptr->prev;
         sort_ptr->prev->next = new_sort_ptr;
         sort_ptr->prev = new_sort_ptr;
      }
   }

   tmp_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (tmp_sel_ptr == NULL) FailAllocMessage();
   tmp_sel_ptr->next = tmp_sel_ptr->prev = NULL;

   if (locked_obj != NULL)
   {
      switch (Dir)
      {
         case ABUT_HORIZONTAL:
            rbx = top_sort->obj->obbox.rbx;
            for (sort_ptr=top_sort; sort_ptr->obj!=locked_obj &&
                  sort_ptr->next!=NULL; sort_ptr=next_sort)
            {
               next_sort = sort_ptr->next;
               delta = rbx-next_sort->obj->obbox.ltx;
               dx = (-delta);
               rbx = next_sort->obj->obbox.rbx+delta;
            }
            break;
         case ABUT_VERTICAL:
            rby = top_sort->obj->obbox.rby;
            for (sort_ptr=top_sort; sort_ptr->obj!=locked_obj &&
                  sort_ptr->next!=NULL; sort_ptr=next_sort)
            {
               next_sort = sort_ptr->next;
               delta = rby-next_sort->obj->obbox.lty;
               dy = (-delta);
               rby = next_sort->obj->obbox.rby+delta;
            }
            break;
      }
   }

   move_cmd = (struct MoveSubCmdRec *)malloc(sizeof(struct MoveSubCmdRec));
   sub_cmd = (struct SubCmdRec *)malloc(sizeof(struct SubCmdRec));
   if (move_cmd == NULL || sub_cmd == NULL) FailAllocMessage();
   memset(move_cmd, 0, sizeof(struct MoveSubCmdRec));
   memset(sub_cmd, 0, sizeof(struct SubCmdRec));
   sub_cmd->detail.mv = move_cmd;

   rbx = top_sort->obj->obbox.rbx;
   rby = top_sort->obj->obbox.rby;
   found = (locked_obj == NULL);
   if (!found && locked_obj != top_sort->obj)
   {
      tmp_sel_ptr->obj = top_sort->obj;
      switch (Dir)
      {
         case ABUT_HORIZONTAL:
            move_cmd->dx = dx;
            move_cmd->dy = 0;
            PrepareToRecord (CMD_MOVE, NULL, NULL, 0);
            RecordCmd (CMD_MOVE, sub_cmd, tmp_sel_ptr, tmp_sel_ptr, 1);
            MoveObj (top_sort->obj, dx, 0);
            break;
         case ABUT_VERTICAL:
            move_cmd->dx = 0;
            move_cmd->dy = dy;
            PrepareToRecord (CMD_MOVE, NULL, NULL, 0);
            RecordCmd (CMD_MOVE, sub_cmd, tmp_sel_ptr, tmp_sel_ptr, 1);
            MoveObj (top_sort->obj, 0, dy);
            break;
      }
   }
   for (sort_ptr=top_sort; sort_ptr->next!=NULL; sort_ptr=next_sort)
   {
      next_sort = sort_ptr->next;
      tmp_sel_ptr->obj = next_sort->obj;
      switch (Dir)
      {
         case ABUT_HORIZONTAL:
            delta = rbx-next_sort->obj->obbox.ltx;
            rbx = next_sort->obj->obbox.rbx+delta;
            if (!found) delta += dx;
            move_cmd->dx = delta;
            move_cmd->dy = 0;
            PrepareToRecord (CMD_MOVE, NULL, NULL, 0);
            RecordCmd (CMD_MOVE, sub_cmd, tmp_sel_ptr, tmp_sel_ptr, 1);
            MoveObj (next_sort->obj, delta, 0);
            break;
         case ABUT_VERTICAL:
            delta = rby-next_sort->obj->obbox.lty;
            rby = next_sort->obj->obbox.rby+delta;
            if (!found) delta += dy;
            move_cmd->dx = 0;
            move_cmd->dy = delta;
            PrepareToRecord (CMD_MOVE, NULL, NULL, 0);
            RecordCmd (CMD_MOVE, sub_cmd, tmp_sel_ptr, tmp_sel_ptr, 1);
            MoveObj (next_sort->obj, 0, delta);
            break;
      }
      free(sort_ptr);
   }
   EndCompositeCmd();
   free(sort_ptr);
   free(move_cmd);
   free(sub_cmd);
   free(tmp_sel_ptr);

   UpdSelBBox ();
   RedrawAreas (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1),
         sel_ltx-GRID_ABS_SIZE(1), sel_lty-GRID_ABS_SIZE(1),
         sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1));
   HighLightForward ();
   justDupped = FALSE;
   switch (Dir)
   {
      case ABUT_HORIZONTAL: Msg ("Objects are abutted horizontally."); break;
      case ABUT_VERTICAL: Msg ("Objects are abutted vertically."); break;
   }
   SetFileModified (TRUE);
}

void AbutHorizontal ()
{
   Abut (ABUT_HORIZONTAL);
}

void AbutVertical ()
{
   Abut (ABUT_VERTICAL);
}

char * arrangeMenuStr[] =
      { "Front              ^f",
        "Back               ^b",
        "Group              ^g",
        "UnGroup            ^u",
        "Lock               #<",
        "UnLock             #>",
        "AlignObjs          ^l",
        "AlignToGrid        ^t",
        "AlignToPage        #&",
        "DistributeObjs     #l",
        "FlipHorizontal     #h",
        "FlipVertical       #v",
        "RotateClockWise    #w",
        "RotateCounter      #c",
        "SetTextRotation      ",
        "SetRotationIncrement ",
        "AlignObjsTop       #{",
        "AlignObjsMiddle    #+",
        "AlignObjsBottom    #}",
        "AlignObjsLeft      #[",
        "AlignObjsCenter    #=",
        "AlignObjsRight     #]",
        "AbutHorizontal     #_",
        "AbutVertical       #|",
        "CenterAnEndPoint     ",
        NULL
      };
static char * arrangeMenuDescription[] =
      { "Bring selected objects to the front",
        "Send selected objects to the back",
        "Group selected objects into a compound object",
        "Break up selected grouped objects into its components",
        "Lock the positions of selected objects",
        "UnLock the positions of selected objects",
        "Align selected objects with each other using the current alignment",
        "Align selected objects to grid points using the current alignment",
        "Align selected objects to the page using the current alignment",
        "Distribute selected objects using the current alignment",
        "Flip selected objects horizontally",
        "Flip selected objects vertically",
        "Rotate selected objects clockwise",
        "Rotate selected objects counter-clockwise",
        "Set text rotation in degrees",
        "Set rotation increment in degrees",
        "Align top sides of selected objects",
        "Align vertical centers of selected objects",
        "Align bottom sides of selected objects",
        "Align left sides of selected objects",
        "Align horizontal centers of selected objects",
        "Align right sides of selected objects",
        "Squish selected objects together horizontally",
        "Squish selected objects together vertically",
        "Move an endpoint of a selected poly to the center of another object",
        NULL
      };

void ArrangeSubMenu (index)
   int	index;
{
   switch (index)
   {
      case ARRANGE_FRONT: FrontProc (); break;
      case ARRANGE_BACK: BackProc (); break;
      case ARRANGE_GROUP: GroupSelObj (); break;
      case ARRANGE_UNGROUP: UngroupSelObj (); break;
      case ARRANGE_LOCK: LockSelObj (); break;
      case ARRANGE_UNLOCK: UnlockSelObj (); break;
      case ARRANGE_ALIGNOBJ: AlignSelObjs (); break;
      case ARRANGE_ALIGNGRID: AlignSelToGrid (); break;
      case ARRANGE_ALIGNPAGE: AlignSelToPage (); break;
      case ARRANGE_DISTROBJ: DistrSelObjs (); break;
      case FLIP_HORIZONTAL: FlipHorizontal (); break;
      case FLIP_VERTICAL: FlipVertical (); break;
      case ROTATE_CLOCKWISE: RotateClockWise (); break;
      case ROTATE_COUNTER: RotateCounter (); break;
      case SET_TEXT_ROTATE: SetTextRotation (); break;
      case SET_ROTATE_INC: SetRotationIncrement (); break;
      case ALIGN_OBJ_TOP: AlignObjsTop (); break;
      case ALIGN_OBJ_MIDDLE: AlignObjsMiddle (); break;
      case ALIGN_OBJ_BOTTOM: AlignObjsBottom (); break;
      case ALIGN_OBJ_LEFT: AlignObjsLeft (); break;
      case ALIGN_OBJ_CENTER: AlignObjsCenter (); break;
      case ALIGN_OBJ_RIGHT: AlignObjsRight (); break;
      case ABUT_HORIZONTAL: AbutHorizontal (); break;
      case ABUT_VERTICAL: AbutVertical (); break;
      case ALIGN_CENTERANENDPOINT: CenterAnEndPoint (); break;
   }
}

int ArrangeMenu (X, Y, TrackMenubar)
   int	X, Y, TrackMenubar;
{
   int		index, * fore_colors, * valid, * init_rv;

   DefaultColorArrays (MAXARRANGEMENUS, &fore_colors, &valid, &init_rv, NULL);
   activeMenu = MENU_ARRANGE;
   index = TextMenuLoop (X, Y, arrangeMenuStr, MAXARRANGEMENUS, fore_colors,
         valid, init_rv, arrangeMenuDescription, SINGLECOLOR, TrackMenubar);

   if (index >= 0) ArrangeSubMenu (index);
   return (index);
}

void UpdateSymbols ()
{
   int			dx = 0, dy = 0, changed=FALSE;
   int			sel_ltx, sel_lty, sel_rbx, sel_rby;
   char			path_name[MAXPATHLENGTH], sym_name[MAXPATHLENGTH];
   struct ObjRec	* obj_ptr, * new_obj_ptr;
   struct SelRec	* sel_ptr;
   struct GroupRec	* icon_ptr;

   if (topSel == NULL) return;

   tmpTopObj = tmpBotObj = NULL;
   tmpTopSel = tmpBotSel = NULL;

   sel_ltx = selLtX; sel_lty = selLtY;
   sel_rbx = selRbX; sel_rby = selRbY;

   HighLightReverse ();

   StartCompositeCmd ();
   for (sel_ptr = botSel; sel_ptr != NULL; sel_ptr = sel_ptr->prev)
   {
      obj_ptr = sel_ptr->obj;
      if (obj_ptr->type!=OBJ_ICON || obj_ptr->locked) continue;

      icon_ptr = obj_ptr->detail.r;
      strcpy (sym_name, icon_ptr->s);
      if (GetSymbolPath (icon_ptr->s, path_name))
      {
         if ((new_obj_ptr = GetObjRepresentation (path_name, sym_name)) != NULL)
         {
            PrepareToReplaceAnObj (obj_ptr);
            if (icon_ptr->rotate != ROTATE0 || icon_ptr->flip != NO_FLIP)
            {
               if (icon_ptr->flip & HORI_EVEN) FlipIconHorizontal (new_obj_ptr);
               if (icon_ptr->flip & VERT_EVEN) FlipIconVertical (new_obj_ptr);
               if (icon_ptr->rotate == ROTATE0)
               {
                  if (icon_ptr->flip & (HORI_ODD | VERT_ODD))
                  {
                     RotateIconClockWise (new_obj_ptr);
                     if (icon_ptr->flip & HORI_ODD)
                        FlipIconHorizontal (new_obj_ptr);
                     if (icon_ptr->flip & VERT_ODD)
                        FlipIconVertical (new_obj_ptr);
                     RotateIconCounter (new_obj_ptr);
                  }
               }
               else
               {
                  switch (icon_ptr->rotate)
                  {
                     case ROTATE90:
                        RotateIconClockWise (new_obj_ptr);
                        if (icon_ptr->flip & HORI_ODD)
                           FlipIconHorizontal (new_obj_ptr);
                        if (icon_ptr->flip & VERT_ODD)
                           FlipIconVertical (new_obj_ptr);
                        break;
                     case ROTATE180:
                        RotateIconClockWise (new_obj_ptr);
                        if (icon_ptr->flip & HORI_ODD)
                           FlipIconHorizontal (new_obj_ptr);
                        if (icon_ptr->flip & VERT_ODD)
                           FlipIconVertical (new_obj_ptr);
                        RotateIconClockWise (new_obj_ptr);
                        break;
                     case ROTATE270:
                        RotateIconClockWise (new_obj_ptr);
                        if (icon_ptr->flip & HORI_ODD)
                           FlipIconHorizontal (new_obj_ptr);
                        if (icon_ptr->flip & VERT_ODD)
                           FlipIconVertical (new_obj_ptr);
                        RotateIconClockWise (new_obj_ptr);
                        RotateIconClockWise (new_obj_ptr);
                        break;
                  }
               }
            }
            switch (horiAlign)
            {
               case ALIGN_L:
                  dx = obj_ptr->obbox.ltx - new_obj_ptr->obbox.ltx;
                  break;
               case ALIGN_N:
               case ALIGN_S:
               case ALIGN_C:
                  dx = (int)(((obj_ptr->obbox.ltx+obj_ptr->obbox.rbx) -
                        (new_obj_ptr->obbox.ltx+new_obj_ptr->obbox.rbx))/2);
                  break;
               case ALIGN_R:
                  dx = obj_ptr->obbox.rbx - new_obj_ptr->obbox.rbx;
                  break;
            }
            switch (vertAlign)
            {
               case ALIGN_T:
                  dy = obj_ptr->obbox.lty - new_obj_ptr->obbox.lty;
                  break;
               case ALIGN_N:
               case ALIGN_S:
               case ALIGN_M:
                  dy = (int)(((obj_ptr->obbox.lty+obj_ptr->obbox.rby) -
                        (new_obj_ptr->obbox.lty+new_obj_ptr->obbox.rby))/2);
                  break;
               case ALIGN_B:
                  dy = obj_ptr->obbox.rby - new_obj_ptr->obbox.rby;
                  break;
            }
            MoveObj (new_obj_ptr, dx, dy);

            changed = TRUE;

            UnlinkObj (obj_ptr);
            CopyAndUpdateAttrs (new_obj_ptr, obj_ptr);

            if (new_obj_ptr->bbox.ltx < selLtX) selLtX = new_obj_ptr->bbox.ltx;
            if (new_obj_ptr->bbox.lty < selLtY) selLtY = new_obj_ptr->bbox.lty;
            if (new_obj_ptr->bbox.rbx < selRbX) selRbX = new_obj_ptr->bbox.rbx;
            if (new_obj_ptr->bbox.rby < selRbY) selRbY = new_obj_ptr->bbox.rby;
            if (new_obj_ptr->obbox.ltx < selObjLtX)
                  selObjLtX = new_obj_ptr->obbox.ltx;
            if (new_obj_ptr->obbox.lty < selObjLtY)
                  selObjLtY = new_obj_ptr->obbox.lty;
            if (new_obj_ptr->obbox.rbx < selObjRbX)
                  selObjRbX = new_obj_ptr->obbox.rbx;
            if (new_obj_ptr->obbox.rby < selObjRbY)
                  selObjRbY = new_obj_ptr->obbox.rby;

            sel_ptr->obj = new_obj_ptr;
            AssignNewObjIds (new_obj_ptr);
            AddObj (NULL, topObj, new_obj_ptr);
            RecordReplaceAnObj (new_obj_ptr);
            FreeObj (obj_ptr);
         }
      }
   }
   EndCompositeCmd ();

   if (changed)
   {
      UpdSelBBox ();
      RedrawAreas (botObj, sel_ltx-GRID_ABS_SIZE(1), sel_lty-GRID_ABS_SIZE(1),
            sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1),
            selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      SetFileModified (TRUE);
      justDupped = FALSE;
      Msg ("Selected icons are brought up to date.");
   }
   HighLightForward ();
}
