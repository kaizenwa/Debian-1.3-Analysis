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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/pattern.c,v 3.0 1996/05/06 16:06:39 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "arc.e"
#include "choice.e"
#include "color.e"
#include "cmd.e"
#include "dialog.e"
#include "drawing.e"
#include "file.e"
#include "font.e"
#include "mainmenu.e"
#include "mark.e"
#include "menu.e"
#include "msg.e"
#include "obj.e"
#ifndef _NO_EXTERN
#include "pattern.e"
#endif
#include "poly.e"
#include "raster.e"
#include "select.e"
#include "setup.e"
#include "spline.e"
#include "text.e"
#include "util.e"

extern int	atoi ARGS_DECL((char *));

int     objFill = NONEPAT;
int     lineStyle = LS_RIGHT;
int     lineWidth = 0;
int     penPat = SOLIDPAT;
int     curSpline = LT_STRAIGHT;
int     curDash = 0;
int     rcbRadius = DEF_RCB_RADIUS;
int     useGray = FALSE;
char    patternStr[] = "8 1 0 72 300 32 div div tgifsetpattern";

int	stickyMenuSelection=FALSE;

static int	tileAsGrayDetected=FALSE;
static int	canFakeGray=FALSE;
static char	*grayStr[9] =
{
   "0.995", "0.94", "0.868", "0.779", "0.763", "0.55", "0.41", "0.253", "0.079"
};

void ResetGrayDetection()
{
   tileAsGrayDetected = FALSE;
   canFakeGray = FALSE;
}

char *GrayStr(index)
   int index;
   /* this routine should only be called when useGray == TRUE */
{
   if (index <= 2) {
      fprintf(stderr, "Error:  GrayStr() called with index<=2.\n");
      return("");
   } else if (index >= 12) {
      if (!tileAsGrayDetected) {
         tileAsGrayDetected = TRUE;
         if (PRTGIF) {
            fprintf(stderr, "%s.\n",
                  "Warning:  Gray scales used instead of tiling patterns.");
         } else {
            MsgBox("Warning:  Gray scales used instead of tiling patterns.",
                  TOOL_NAME, INFO_MB);
         }
      }
      switch (index) {
      case 12: index = 5; break;
      case 13: index = 4; break;
      case 14: index = 7; break;
      case 15: index = 6; break;
      case 16: index = 7; break;
      case 17: index = 6; break;
      case 18: index = 6; break;
      case 19: index = 6; break;
      case 20: index = 5; break;
      case 21: index = 6; break;
      case 22: index = 8; break;
      case 23: index = 7; break;
      case 24: index = 9; break;
      case 25: index = 8; break;
      case 26: index = 5; break;
      case 27: index = 6; break;
      case 28: index = 8; break;
      case 29: index = 7; break;
      case 30: index = 9; break;
      case 31: index = 8; break;
      }
   }
   return (grayStr[index-3]);
}

void GrayCheck(index)
   int index;
   /* this routine should only be called when useGray == FALSE */
{
   if (index > BACKPAT) {
      if (index >= 12) {
         tileAsGrayDetected = TRUE;
      } else {
         canFakeGray = TRUE;
      }
   }
}

void EndGrayDetection()
   /* this routine should only be called when useGray == FALSE */
{
   int num_msgs = 1;
   char msg1[MAXSTRING], msg2[MAXSTRING];

   if (colorDump) return;

   if (useGray)
   {
      if (!tileAsGrayDetected && !canFakeGray) return;

      strcpy (msg1, "Gray scale used in printing tiling patterns.");
   }
   else if (tileAsGrayDetected)
      strcpy (msg1, "Note: slow printing due to the use of patterns.");
   else if (canFakeGray)
   {
      num_msgs = 2;
      strcpy (msg1, "Note: slow printing due to the use of patterns.");
      strcpy (msg2, "      May try UseGrayScale() to speed things up.");
   }
   else
      return;

   if (PRTGIF)
   {
      fprintf (stderr, "%s.\n", msg1);
      if (num_msgs==2) fprintf (stderr, "%s.\n", msg2);
   }
   else
   {
      if (num_msgs==1)
         Msg (msg1);
      else
         TwoLineMsg (msg1, msg2);
   }
}

static char * modeMenuDescription[] =
{
   "Enter select/move/resize object mode",
   "Enter draw text mode",
   "Enter draw rectangle mode",
   "Enter draw oval mode",
   "Enter draw poly/open spline mode",
   "Enter draw polygon/close spline mode",
   "Enter draw arc mode",
   "Enter draw rcbox mode",
   "Enter draw freehand poly/open spline mode",
   "Enter select/move vertices mode",
   "Enter rotate/shear mode",
   NULL
};

int ModeMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   DefaultColorArrays(MAXCHOICES, &fore_colors, &valid, &init_rv, NULL);
   free(valid);
   init_rv[curChoice] = TRUE;
   activeMenu = MENU_MODE;
   index = PxMpMenuLoop(X, Y, choiceImageW, choiceImageH, MAXCHOICES, 1,
         MAXCHOICES, fore_colors, choicePixmap, init_rv, modeMenuDescription,
         SINGLECOLOR, TrackMenubar);

   if (index >= 0) SetCurChoice(index);
   return index;
}

static
int ChangeObjFill(ObjPtr, FillIndex)
   struct ObjRec *ObjPtr;
   int FillIndex;
{
   register struct ObjRec *obj_ptr;
   int changed=FALSE;

   switch (ObjPtr->type) {
   case OBJ_TEXT:
      if (ObjPtr->detail.t->fill != FillIndex) {
         ObjPtr->detail.t->fill = FillIndex;
         changed = TRUE;
      }
      break;
   case OBJ_BOX:
      if (ObjPtr->detail.b->fill != FillIndex) {
         ObjPtr->detail.b->fill = FillIndex;
         changed = TRUE;
      }
      break;
   case OBJ_OVAL:
      if (ObjPtr->detail.o->fill != FillIndex) {
         ObjPtr->detail.o->fill = FillIndex;
         changed = TRUE;
      }
      break;
   case OBJ_POLY:
      if (ObjPtr->detail.p->fill != FillIndex) {
         ObjPtr->detail.p->fill = FillIndex;
         changed = TRUE;
      }
      break;
   case OBJ_POLYGON:
      if (ObjPtr->detail.g->fill != FillIndex) {
         ObjPtr->detail.g->fill = FillIndex;
         changed = TRUE;
      }
      break;
   case OBJ_ARC:
      if (ObjPtr->detail.a->fill != FillIndex) {
         if (ObjPtr->detail.a->fill == NONEPAT || FillIndex == NONEPAT) {
            ObjPtr->detail.a->fill = FillIndex;
            AdjObjBBox(ObjPtr);
         } else {
            ObjPtr->detail.a->fill = FillIndex;
         }
         changed = TRUE;
      }
      break;
   case OBJ_RCBOX:
      if (ObjPtr->detail.rcb->fill != FillIndex) {
         ObjPtr->detail.rcb->fill = FillIndex;
         changed = TRUE;
      }
      break;
   case OBJ_XBM:
      if (ObjPtr->detail.xbm->fill != FillIndex) {
         ObjPtr->detail.xbm->fill = FillIndex;
         changed = TRUE;
      }
      break;
   case OBJ_XPM:
      if (ObjPtr->detail.xpm->fill != FillIndex) {
         ObjPtr->detail.xpm->fill = FillIndex;
         changed = TRUE;
      }
      break;

   case OBJ_SYM:
   case OBJ_GROUP:
      for (obj_ptr = ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr = obj_ptr->prev) {
         if (ChangeObjFill(obj_ptr, FillIndex)) {
            changed = TRUE;
         }
      }
      break;
   }
   return changed;
}

void ChangeAllSelFill(FillIndex, HighLight)
   int FillIndex, HighLight;
{
   register struct SelRec *sel_ptr;
   int changed=FALSE, ltx, lty, rbx, rby;

   if (topSel == NULL || stickyMenuSelection) {
      if (!(curChoice == DRAWTEXT && textCursorShown)) {
         TieLooseEnds();
      }
      objFill = FillIndex;
      if (curChoice == DRAWTEXT && textCursorShown) {
         if (ChangeObjFill(curTextObj, FillIndex)) {
            curTextModified = TRUE;
            UpdCurTextBBox();
            RedrawCurText();
            if (cycleThroughChoice) {
               SetPushedFontValue(PUSH_FILL, objFill);
            }
         }
      } else {
         textCursorShown = FALSE;
      }
      ShowFill();
      UpdateSubMenu(MENU_FILL);
      if (topSel == NULL) return;
   }

   if (HighLight) HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjFill(sel_ptr->obj, FillIndex)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      SetFileModified(TRUE);
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
   }
   if (HighLight) HighLightForward();
}

static char * fillMenuDescription[] = { "\nSet fill pattern\n", NULL };

int FillMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   DefaultColorArrays (MAXPATTERNS, &fore_colors, &valid, &init_rv, NULL);
   free(valid);
   activeMenu = MENU_FILL;
   index = PxMpMenuLoop (X, Y, choiceImageW, choiceImageH, 8, 4, MAXPATTERNS,
         fore_colors, patPixmap, init_rv, fillMenuDescription, SINGLECOLOR,
         TrackMenubar);

   if (index >= 0) ChangeAllSelFill (index, TRUE);
   return (index);
}

static
int ChangeObjLineStyle (ObjPtr, StyleIndex)
   struct ObjRec *ObjPtr;
   int StyleIndex;
{
   register struct ObjRec *obj_ptr;
   register int changed=FALSE;

   switch (ObjPtr->type) {
   case OBJ_POLY:
      if (ObjPtr->detail.p->style != StyleIndex) {
         ObjPtr->detail.p->style = StyleIndex;
         changed = TRUE;
         AdjObjSplineVs(ObjPtr);
         if (ObjPtr->detail.p->curved != LT_INTSPLINE) {
            UpdPolyBBox(ObjPtr, ObjPtr->detail.p->n,
                  ObjPtr->detail.p->vlist);
         } else {
            UpdPolyBBox(ObjPtr, ObjPtr->detail.p->intn,
                  ObjPtr->detail.p->intvlist);
         }
      }
      break;
   case OBJ_ARC:
      if (ObjPtr->detail.a->style != StyleIndex) {
         ObjPtr->detail.a->style = StyleIndex;
         changed = TRUE;
         AdjObjSplineVs(ObjPtr);
      }
      break;
   case OBJ_GROUP:
   case OBJ_SYM:
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ChangeObjLineStyle(obj_ptr, StyleIndex)) {
            changed = TRUE;
         }
      }
      break;
   }
   if (changed) AdjObjBBox(ObjPtr);
   return changed;
}

void ChangeAllSelLineStyle(StyleIndex, HighLight)
   int StyleIndex, HighLight;
{
   register struct SelRec *sel_ptr;
   int ltx, lty, rbx, rby, changed=FALSE;

   if (topSel == NULL || stickyMenuSelection) {
      lineStyle = StyleIndex;
      ShowLineStyle();
      UpdateSubMenu(MENU_LINESTYLE);
      if (topSel == NULL) return;
   }

   if (HighLight) HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjLineStyle(sel_ptr->obj, StyleIndex)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      SetFileModified(TRUE);
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
   }
   if (HighLight) HighLightForward();
}

static
int VerifyType(ObjPtr, TypeIndex)
   struct ObjRec *ObjPtr;
   int TypeIndex;
   /* returns TRUE if ObjPtr has the same line type as TypeIndex */
{
   register int i;
   struct PolyRec *poly_ptr;
   struct PolygonRec *polygon_ptr;
   int curved, n;
   char *smooth=NULL;

   switch (ObjPtr->type) {
   case OBJ_POLY:
      poly_ptr = ObjPtr->detail.p;
      smooth = poly_ptr->smooth;
      curved = poly_ptr->curved;
      n = poly_ptr->n;
      break;
   case OBJ_POLYGON:
      polygon_ptr = ObjPtr->detail.g;
      smooth = polygon_ptr->smooth;
      curved = polygon_ptr->curved;
      n = polygon_ptr->n;
      break;
   default: return FALSE;
   }
   switch (TypeIndex) {
   case LT_STRAIGHT:
      if (curved==LT_INTSPLINE) return FALSE;
      if (curved==LT_STRAIGHT && smooth==NULL) return TRUE;
      if (curved==LT_SPLINE && smooth==NULL) return FALSE;
      switch (ObjPtr->type) {
      case OBJ_POLY:
         for (i=1; i < n-1; i++) if (smooth[i]) return FALSE;
         break;
      case OBJ_POLYGON:
         for (i=0; i < n; i++) if (smooth[i]) return FALSE;
         break;
      }
      return TRUE;
   case LT_SPLINE:
      if (curved==LT_INTSPLINE) return FALSE;
      if (curved==LT_STRAIGHT && smooth==NULL) return FALSE;
      if (curved==LT_SPLINE && smooth==NULL) return TRUE;
      switch (ObjPtr->type) {
      case OBJ_POLY:
         for (i=1; i < n-1; i++) if (!smooth[i]) return FALSE;
         break;
      case OBJ_POLYGON:
         for (i=0; i < n; i++) if (!smooth[i]) return FALSE;
         break;
      }
      return TRUE;
   case LT_INTSPLINE:
      return (curved == LT_INTSPLINE);
   }
   return FALSE;
}

static
void SetObjSmooth(ObjPtr, TypeIndex)
   struct ObjRec *ObjPtr;
   int TypeIndex;
{
   register int i;
   int n, curved;
   char *smooth=NULL;
   struct PolyRec *poly_ptr=NULL;
   struct PolygonRec *polygon_ptr=NULL;

   switch (ObjPtr->type) {
   case OBJ_POLY:
      poly_ptr = ObjPtr->detail.p;
      smooth = poly_ptr->smooth;
      curved = poly_ptr->curved;
      n = poly_ptr->n;
      switch (TypeIndex) {
      case LT_STRAIGHT:
         if (smooth == NULL) smooth = (char*)malloc((n+1)*sizeof(char));
         if (smooth == NULL) FailAllocMessage();
         for (i=0; i < n; i++) smooth[i] = FALSE;
         poly_ptr->smooth = smooth;
         return;
      case LT_SPLINE:
         if (smooth == NULL) smooth = (char*)malloc((n+1)*sizeof(char));
         if (smooth == NULL) FailAllocMessage();
         smooth[0] = smooth[n-1] = FALSE;
         for (i=1; i < n-1; i++) smooth[i] = TRUE;
         poly_ptr->smooth = smooth;
         return;
      case LT_INTSPLINE:
         if (smooth != NULL) {
            free(smooth);
            poly_ptr->smooth = NULL;
         }
         return;
      }
      break;
   case OBJ_POLYGON:
      polygon_ptr = ObjPtr->detail.g;
      smooth = polygon_ptr->smooth;
      curved = polygon_ptr->curved;
      n = polygon_ptr->n;
      switch (TypeIndex) {
      case LT_STRAIGHT:
         if (smooth == NULL) smooth = (char*)malloc((n+1)*sizeof(char));
         if (smooth == NULL) FailAllocMessage();
         for (i=0; i < n; i++) smooth[i] = FALSE;
         polygon_ptr->smooth = smooth;
         return;
      case LT_SPLINE:
         if (smooth == NULL) smooth = (char*)malloc((n+1)*sizeof(char));
         if (smooth == NULL) FailAllocMessage();
         for (i=0; i < n; i++) smooth[i] = TRUE;
         polygon_ptr->smooth = smooth;
         return;
      case LT_INTSPLINE:
         if (smooth != NULL) {
            free(smooth);
            polygon_ptr->smooth = NULL;
         }
         return;
      }
      break;
   }
}

static
int ChangeObjLineType(ObjPtr, TypeIndex)
   struct ObjRec *ObjPtr;
   int TypeIndex;
{
   register struct ObjRec *obj_ptr;
   register int changed=FALSE;

   switch (ObjPtr->type) {
   case OBJ_POLY:
      if (!VerifyType(ObjPtr, TypeIndex)) {
         ObjPtr->detail.p->curved = TypeIndex;
         SetObjSmooth(ObjPtr, TypeIndex);
         changed = TRUE;
         AdjObjSplineVs(ObjPtr);
         if (ObjPtr->detail.p->curved != LT_INTSPLINE) {
            UpdPolyBBox(ObjPtr, ObjPtr->detail.p->n,
                  ObjPtr->detail.p->vlist);
         } else {
            UpdPolyBBox(ObjPtr, ObjPtr->detail.p->intn,
                  ObjPtr->detail.p->intvlist);
         }
      }
      break;
   case OBJ_POLYGON:
      if (!VerifyType(ObjPtr, TypeIndex)) {
         ObjPtr->detail.g->curved = TypeIndex;
         SetObjSmooth(ObjPtr, TypeIndex);
         changed = TRUE;
         AdjObjSplineVs(ObjPtr);
         if (ObjPtr->detail.g->curved != LT_INTSPLINE) {
            UpdPolyBBox(ObjPtr, ObjPtr->detail.g->n,
                  ObjPtr->detail.g->vlist);
         } else {
            UpdPolyBBox(ObjPtr, ObjPtr->detail.g->intn,
                  ObjPtr->detail.g->intvlist);
         }
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ChangeObjLineType(obj_ptr, TypeIndex)) {
            changed = TRUE;
         }
      }
      break;
   }
   if (changed) AdjObjBBox(ObjPtr);
   return changed;
}

void ChangeAllSelLineType(TypeIndex, HighLight)
   int TypeIndex, HighLight;
{
   register struct SelRec *sel_ptr;
   int ltx, lty, rbx, rby, changed=FALSE, dont_do_obj=FALSE;

   if (topSel == NULL || stickyMenuSelection) {
      curSpline = TypeIndex;
      ShowLineType();
      UpdateSubMenu(MENU_LINETYPE);
      if (topSel == NULL) dont_do_obj = TRUE;
   }
   *gszMsgBox = '\0';
   switch (curSpline) {
   case LT_STRAIGHT:
      sprintf(gszMsgBox, "Line type is 'straight'.");
      break;
   case LT_SPLINE:
      sprintf(gszMsgBox, "Line type is 'spline'.");
      break;
   case LT_INTSPLINE:
      sprintf(gszMsgBox, "Line type is 'interpolated spline'.");
      break;
   }
   Msg(gszMsgBox);
   if (dont_do_obj) return;

   if (HighLight) HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjLineType(sel_ptr->obj, TypeIndex)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      SetFileModified(TRUE);
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
   }
   if (HighLight) HighLightForward();
}

static
int ChangeObjLineWidth(ObjPtr, W, AW, AH, width_spec, aw_spec, ah_spec)
   struct ObjRec *ObjPtr;
   int W, AW, AH;
   char *width_spec, *aw_spec, *ah_spec;
{
   register struct ObjRec *obj_ptr;
   register int changed=FALSE;

   switch (ObjPtr->type) {
   case OBJ_POLY:
      if (ObjPtr->detail.p->width != W ||
            strcmp(ObjPtr->detail.p->width_spec, width_spec) != 0 ||
            (AW != (-1) && (ObjPtr->detail.p->aw != AW ||
            strcmp(ObjPtr->detail.p->aw_spec, aw_spec) != 0)) ||
            (AH != (-1) && (ObjPtr->detail.p->ah != AH ||
            strcmp(ObjPtr->detail.p->ah_spec, ah_spec) != 0))) {
         ObjPtr->detail.p->width = W;
         UtilStrCpy(ObjPtr->detail.p->width_spec,
               sizeof(ObjPtr->detail.p->width_spec), width_spec);
         if (AW != (-1)) {
            ObjPtr->detail.p->aw = AW;
            UtilStrCpy(ObjPtr->detail.p->aw_spec,
                  sizeof(ObjPtr->detail.p->aw_spec), aw_spec);
         }
         if (AH != (-1)) {
            ObjPtr->detail.p->ah = AH;
            UtilStrCpy(ObjPtr->detail.p->ah_spec,
                  sizeof(ObjPtr->detail.p->ah_spec), ah_spec);
         }
         changed = TRUE;
         AdjObjSplineVs(ObjPtr);
         if (ObjPtr->detail.p->curved != LT_INTSPLINE) {
            UpdPolyBBox(ObjPtr, ObjPtr->detail.p->n,
                  ObjPtr->detail.p->vlist);
         } else {
            UpdPolyBBox(ObjPtr, ObjPtr->detail.p->intn,
                  ObjPtr->detail.p->intvlist);
         }
      }
      break;
   case OBJ_BOX:
      if (ObjPtr->detail.b->width != W ||
            strcmp(ObjPtr->detail.b->width_spec, width_spec) != 0) {
         ObjPtr->detail.b->width = W;
         UtilStrCpy(ObjPtr->detail.b->width_spec,
               sizeof(ObjPtr->detail.b->width_spec), width_spec);
         changed = TRUE;
      }
      break;
   case OBJ_OVAL:
      if (ObjPtr->detail.o->width != W ||
            strcmp(ObjPtr->detail.o->width_spec, width_spec) != 0) {
         ObjPtr->detail.o->width = W;
         UtilStrCpy(ObjPtr->detail.o->width_spec,
               sizeof(ObjPtr->detail.o->width_spec), width_spec);
         changed = TRUE;
      }
      break;
   case OBJ_POLYGON:
      if (ObjPtr->detail.g->width != W ||
            strcmp(ObjPtr->detail.g->width_spec, width_spec) != 0) {
         ObjPtr->detail.g->width = W;
         UtilStrCpy(ObjPtr->detail.g->width_spec,
               sizeof(ObjPtr->detail.g->width_spec), width_spec);
         changed = TRUE;
      }
      break;
   case OBJ_ARC:
      if (ObjPtr->detail.a->width != W ||
            strcmp(ObjPtr->detail.a->width_spec, width_spec) != 0 ||
            (AW != (-1) && (ObjPtr->detail.a->aw != AW ||
            strcmp(ObjPtr->detail.a->aw_spec, aw_spec) != 0)) ||
            (AH != (-1) && (ObjPtr->detail.a->ah != AH ||
            strcmp(ObjPtr->detail.a->ah_spec, ah_spec) != 0))) {
         ObjPtr->detail.a->width = W;
         UtilStrCpy(ObjPtr->detail.a->width_spec,
               sizeof(ObjPtr->detail.a->width_spec), width_spec);
         if (AW != (-1)) {
            ObjPtr->detail.a->aw = AW;
            UtilStrCpy(ObjPtr->detail.a->aw_spec,
                  sizeof(ObjPtr->detail.a->aw_spec), aw_spec);
         }
         if (AH != (-1)) {
            ObjPtr->detail.a->ah = AH;
            UtilStrCpy(ObjPtr->detail.a->ah_spec,
                  sizeof(ObjPtr->detail.a->ah_spec), ah_spec);
         }
         changed = TRUE;
         AdjObjSplineVs(ObjPtr);
      }
      break;
   case OBJ_RCBOX:
      if (ObjPtr->detail.rcb->width != W ||
            strcmp(ObjPtr->detail.rcb->width_spec, width_spec) != 0) {
         ObjPtr->detail.rcb->width = W;
         UtilStrCpy(ObjPtr->detail.rcb->width_spec,
               sizeof(ObjPtr->detail.rcb->width_spec), width_spec);
         changed = TRUE;
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ChangeObjLineWidth(obj_ptr, W, AW, AH, width_spec, aw_spec,
               ah_spec)) {
            changed = TRUE;
         }
      }
      break;
   }
   if (changed) AdjObjBBox(ObjPtr);
   return changed;
}

void ChangeAllSelLineWidth(WidthIndex, HighLight)
   int WidthIndex, HighLight;
{
   struct SelRec *sel_ptr;
   int ltx, lty, rbx, rby, changed=FALSE, w, aw, ah, dont_do_obj=FALSE;
   char *width_spec, *aw_spec, *ah_spec;

   if (topSel == NULL || stickyMenuSelection) {
      lineWidth = WidthIndex;
      ShowLineWidth();
      UpdateSubMenu(MENU_LINEWIDTH);
      if (topSel == NULL) dont_do_obj = TRUE;
   }
   sprintf(gszMsgBox, "Line width set to %1d.", curWidthOfLine[WidthIndex]);
   Msg(gszMsgBox);
   if (dont_do_obj) return;

   w = curWidthOfLine[WidthIndex];
   aw = curArrowHeadW[WidthIndex];
   ah = curArrowHeadH[WidthIndex];
   width_spec = curWidthOfLineSpec[WidthIndex];
   aw_spec = curArrowHeadWSpec[WidthIndex];
   ah_spec = curArrowHeadHSpec[WidthIndex];

   if (HighLight) HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjLineWidth(sel_ptr->obj, w, aw, ah, width_spec, aw_spec,
            ah_spec)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      SetFileModified(TRUE);
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
   }
   if (HighLight) HighLightForward();
}

static
int ChangeObjDashes(ObjPtr, DashIndex)
   struct ObjRec *ObjPtr;
   int DashIndex;
{
   register struct ObjRec *obj_ptr;
   int changed=FALSE;

   switch (ObjPtr->type) {
   case OBJ_POLY:
      if (ObjPtr->detail.p->dash != DashIndex) {
         ObjPtr->detail.p->dash = DashIndex;
         changed = TRUE;
      }
      break;
   case OBJ_BOX:
      if (ObjPtr->detail.b->dash != DashIndex) {
         ObjPtr->detail.b->dash = DashIndex;
         changed = TRUE;
      }
      break;
   case OBJ_OVAL:
      if (ObjPtr->detail.o->dash != DashIndex) {
         ObjPtr->detail.o->dash = DashIndex;
         changed = TRUE;
      }
      break;
   case OBJ_POLYGON:
      if (ObjPtr->detail.g->dash != DashIndex) {
         ObjPtr->detail.g->dash = DashIndex;
         changed = TRUE;
      }
      break;
   case OBJ_ARC:
      if (ObjPtr->detail.a->dash != DashIndex) {
         ObjPtr->detail.a->dash = DashIndex;
         changed = TRUE;
      }
      break;
   case OBJ_RCBOX:
      if (ObjPtr->detail.rcb->dash != DashIndex) {
         ObjPtr->detail.rcb->dash = DashIndex;
         changed = TRUE;
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ChangeObjDashes(obj_ptr, DashIndex)) {
            changed = TRUE;
         }
      }
      break;
   }
   return changed;
}

void ChangeAllSelDashes(DashIndex, HighLight)
   int DashIndex, HighLight;
{
   register struct SelRec *sel_ptr;
   int changed=FALSE;

   if (topSel == NULL || stickyMenuSelection) {
      curDash = DashIndex;
      ShowDash();
      UpdateSubMenu(MENU_LINEDASH);
      if (topSel == NULL) return;
   }

   if (HighLight) HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjDashes(sel_ptr->obj, DashIndex)) {
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
   }
   if (HighLight) HighLightForward();
}

int LineWidthMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   register int i;
   int index, *fore_colors, *valid, *init_rv;
   char **desc=(char**)malloc((maxLineWidths+1)*sizeof(char*));

   if (desc == NULL) FailAllocMessage();
   for (i=0; i < maxLineWidths; i++) {
      desc[i] = (char*)malloc(80*sizeof(char));
      if (desc[i] == NULL) FailAllocMessage();
      sprintf(desc[i], "Set line width to %1d", curWidthOfLine[i]);
   }
   desc[i] = NULL;
   DefaultColorArrays(maxLineWidths, &fore_colors, &valid, &init_rv, NULL);
   free(valid);
   init_rv[lineWidth] = TRUE;
   activeMenu = MENU_LINEWIDTH;
   index = PxMpMenuLoop(X, Y, menuImageW, menuImageH, maxLineWidths, 1,
         maxLineWidths, fore_colors, lineWidthPixmap, init_rv, desc,
         SINGLECOLOR, TrackMenubar);

   if (index >= 0) ChangeAllSelLineWidth(index, TRUE);
   if (desc != NULL) {
      for (i=0; i < maxLineWidths; i++) {
         if (desc[i] != NULL) free(desc[i]);
      }
      free(desc);
   }
   return index;
}

static char *lineStyleMenuDescription[] =
{
   "No arrows for a poly/open spline",
   "An arrow at the end of a poly/open spline",
   "An arrow at the beginning of a poly/open spline",
   "Arrows at both ends of a poly/open spline",
   NULL
};

int LineStyleMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   DefaultColorArrays(MAXLINESTYLES, &fore_colors, &valid, &init_rv, NULL);
   free(valid);
   init_rv[lineStyle] = TRUE;
   activeMenu = MENU_LINESTYLE;
   index = PxMpMenuLoop(X, Y, menuImageW, menuImageH, MAXLINESTYLES, 1,
         MAXLINESTYLES, fore_colors, lineStylePixmap, init_rv,
         lineStyleMenuDescription, SINGLECOLOR, TrackMenubar);

   if (index >= 0) ChangeAllSelLineStyle(index, TRUE);
   return index;
}

static char * lineTypeMenuDescription[] =
{
   "Straight line segments everywhere",
   "Open/closed splines",
   "Interpolated splines",
   NULL
};

int LineTypeMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   DefaultColorArrays(MAXLINETYPES, &fore_colors, &valid, &init_rv, NULL);
   free(valid);
   init_rv[curSpline] = TRUE;
   activeMenu = MENU_LINETYPE;
   index = PxMpMenuLoop(X, Y, menuImageW, menuImageH, MAXLINETYPES, 1,
         MAXLINETYPES, fore_colors, lineTypePixmap, init_rv,
         lineTypeMenuDescription, SINGLECOLOR, TrackMenubar);

   if (index >= 0) ChangeAllSelLineType(index, TRUE);
   return index;
}

static char * lineDashMenuDescription[] =
{
   "\nSet dash pattern for the pen\n", NULL
};

int LineDashMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   DefaultColorArrays(MAXDASHES, &fore_colors, &valid, &init_rv, NULL);
   free(valid);
   init_rv[curDash] = TRUE;
   activeMenu = MENU_LINEDASH;
   index = PxMpMenuLoop(X, Y, menuImageW, menuImageH, MAXDASHES, 1,
         MAXDASHES, fore_colors, dashPixmap, init_rv, lineDashMenuDescription,
         SINGLECOLOR, TrackMenubar);

   if (index >= 0) ChangeAllSelDashes(index, TRUE);
   return index;
}

static
int ChangeObjPen(ObjPtr, PenIndex)
   struct ObjRec *ObjPtr;
   int PenIndex;
{
   register struct ObjRec *obj_ptr;
   int changed=FALSE;

   switch (ObjPtr->type) {
   case OBJ_POLY:
      if (ObjPtr->detail.p->pen != PenIndex) {
         ObjPtr->detail.p->pen = PenIndex;
         changed = TRUE;
      }
      break;
   case OBJ_TEXT:
      if (ObjPtr->detail.t->pen != PenIndex) {
         ObjPtr->detail.t->pen = PenIndex;
         changed = TRUE;
      }
      break;
   case OBJ_BOX:
      if (ObjPtr->detail.b->pen != PenIndex) {
         ObjPtr->detail.b->pen = PenIndex;
         changed = TRUE;
      }
      break;
   case OBJ_OVAL:
      if (ObjPtr->detail.o->pen != PenIndex) {
         ObjPtr->detail.o->pen = PenIndex;
         changed = TRUE;
      }
      break;
   case OBJ_POLYGON:
      if (ObjPtr->detail.g->pen != PenIndex) {
         ObjPtr->detail.g->pen = PenIndex;
         changed = TRUE;
      }
      break;
   case OBJ_ARC:
      if (ObjPtr->detail.a->pen != PenIndex) {
         ObjPtr->detail.a->pen = PenIndex;
         changed = TRUE;
      }
      break;
   case OBJ_RCBOX:
      if (ObjPtr->detail.rcb->pen != PenIndex) {
         ObjPtr->detail.rcb->pen = PenIndex;
         changed = TRUE;
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ChangeObjPen(obj_ptr, PenIndex)) {
            changed = TRUE;
         }
      }
      break;
   }
   return changed;
}

void ChangeAllSelPen(PenIndex, HighLight)
   int PenIndex, HighLight;
{
   register struct SelRec *sel_ptr;
   int changed=FALSE;

   if (topSel == NULL || stickyMenuSelection) {
      if (!(curChoice == DRAWTEXT && textCursorShown)) {
         TieLooseEnds();
      }
      penPat = PenIndex;
      if (curChoice == DRAWTEXT && textCursorShown) {
         if (ChangeObjPen(curTextObj, PenIndex)) {
            curTextModified = TRUE;
            UpdCurTextBBox();
            RedrawCurText();
            if (cycleThroughChoice) {
               SetPushedFontValue(PUSH_PEN, penPat);
            }
         }
      } else {
         textCursorShown = FALSE;
      }
      ShowPen();
      UpdateSubMenu(MENU_PEN);
      if (topSel == NULL) return;
   }

   if (HighLight) HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjPen(sel_ptr->obj, PenIndex)) {
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
   }
   if (HighLight) HighLightForward();
}

static char * penMenuDescription[] = { "\nSet pen pattern\n", NULL };

int PenMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   DefaultColorArrays(MAXPATTERNS, &fore_colors, &valid, &init_rv, NULL);
   free(valid);
   activeMenu = MENU_PEN;
   index = PxMpMenuLoop(X, Y, choiceImageW, choiceImageH, 8, 4, MAXPATTERNS,
         fore_colors, patPixmap, init_rv, penMenuDescription, SINGLECOLOR,
         TrackMenubar);

   if (index >= 0) ChangeAllSelPen(index, TRUE);
   return index;
}

static
int ToggleObjLineType(ObjPtr)
   struct ObjRec *ObjPtr;
{
   register struct ObjRec *obj_ptr;
   register int changed=FALSE;

   switch (ObjPtr->type) {
   case OBJ_POLY:
      ObjPtr->detail.p->curved = (ObjPtr->detail.p->curved+1) % MAXLINETYPES;
      changed = TRUE;
      AdjObjSplineVs(ObjPtr);
      if (ObjPtr->detail.p->curved != LT_INTSPLINE) {
         UpdPolyBBox(ObjPtr, ObjPtr->detail.p->n,
               ObjPtr->detail.p->vlist);
      } else {
         UpdPolyBBox(ObjPtr, ObjPtr->detail.p->intn,
               ObjPtr->detail.p->intvlist);
      }
      break;
   case OBJ_POLYGON:
      ObjPtr->detail.g->curved = (ObjPtr->detail.g->curved+1) % MAXLINETYPES;
      changed = TRUE;
      AdjObjSplineVs(ObjPtr);
      if (ObjPtr->detail.g->curved != LT_INTSPLINE) {
         UpdPolyBBox(ObjPtr, ObjPtr->detail.g->n,
               ObjPtr->detail.g->vlist);
      } else {
         UpdPolyBBox(ObjPtr, ObjPtr->detail.g->intn,
               ObjPtr->detail.g->intvlist);
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ToggleObjLineType(obj_ptr)) {
            changed = TRUE;
         }
      }
      break;
   }
   if (changed) AdjObjBBox(ObjPtr);
   return changed;
}

void ToggleAllSelLineType()
{
   register struct SelRec *sel_ptr;
   register int changed=FALSE;

   if (topSel == NULL) {
      curSpline = (curSpline+1) % MAXLINETYPES;
      ShowLineType();
      UpdateSubMenu(MENU_LINETYPE);
      return;
   }

   HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ToggleObjLineType(sel_ptr->obj)) {
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
   }
   HighLightForward();
}

static
int ChangeObjRCBRadius(ObjPtr, Radius)
   struct ObjRec *ObjPtr;
   int Radius;
{
   register struct ObjRec *obj_ptr;
   int changed=FALSE;

   switch (ObjPtr->type) {
   case OBJ_RCBOX:
      if (ObjPtr->detail.rcb->radius != Radius) {
         ObjPtr->detail.rcb->radius = Radius;
         changed = TRUE;
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ChangeObjRCBRadius(obj_ptr, Radius)) {
            changed = TRUE;
         }
      }
      break;
   }
   return changed;
}

void ChangeAllSelRCBRadius(Radius)
   int Radius;
{
   register struct SelRec *sel_ptr;
   int changed=FALSE;

   if (topSel == NULL) {
      rcbRadius = Radius;
      ShowRCBRadius();
      return;
   }

   HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjRCBRadius(sel_ptr->obj, Radius)) {
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
   }
   HighLightForward();
}

static
int UpdateAnObj(ObjPtr)
   struct ObjRec *ObjPtr;
{
   int w, aw, ah, changed=FALSE;
   char *width_spec, *aw_spec, *ah_spec;

   w = curWidthOfLine[lineWidth];
   aw = curArrowHeadW[lineWidth];
   ah = curArrowHeadH[lineWidth];
   width_spec = curWidthOfLineSpec[lineWidth];
   aw_spec = curArrowHeadWSpec[lineWidth];
   ah_spec = curArrowHeadHSpec[lineWidth];

   if (ChangeObjColor (ObjPtr, colorIndex)) changed = TRUE;

   if (ChangeObjFill (ObjPtr, objFill)) changed = TRUE;
   if (ChangeObjLineStyle (ObjPtr, lineStyle)) changed = TRUE;
   if (ChangeObjLineType (ObjPtr, curSpline)) changed = TRUE;
   if (ChangeObjLineWidth (ObjPtr, w, aw, ah, width_spec, aw_spec, ah_spec)) {
      changed = TRUE;
   }
   if (ChangeObjDashes (ObjPtr, curDash)) changed = TRUE;
   if (ChangeObjPen (ObjPtr, penPat)) changed = TRUE;
   if (ChangeObjRCBRadius (ObjPtr, rcbRadius)) changed = TRUE;

   if (ChangeObjTextStyle (ObjPtr, curStyle)) changed = TRUE;
   if (ChangeObjTextJust (ObjPtr, textJust)) changed = TRUE;
   if (ChangeObjTextSize (ObjPtr, GetSizeMenuIndex ())) changed = TRUE;
   if (ChangeObjTextFont (ObjPtr, curFont)) changed = TRUE;
   if (ChangeObjVSpace (ObjPtr, textVSpace)) changed = TRUE;

   return (changed);
}

void UpdateSelObjs()
{
   register struct SelRec *sel_ptr;
   register struct ObjRec *obj_ptr;
   int ltx, lty, rbx, rby, changed=FALSE;

   if (topSel == NULL) return;

   HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      obj_ptr = sel_ptr->obj;

      PrepareToReplaceAnObj(obj_ptr);
      if (UpdateAnObj(obj_ptr)) {
         changed = TRUE;
         RecordReplaceAnObj(obj_ptr);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      SetFileModified(TRUE);
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
   }
   HighLightForward();
}

void ChangeAllSelRealLineWidth(Width, AW, AH, width_spec, aw_spec, ah_spec,
      HighLight)
   int Width, AW, AH, HighLight;
   char *width_spec, *aw_spec, *ah_spec;
{
   register struct SelRec *sel_ptr;
   int changed=FALSE;

   if (topSel == NULL) return;

   if (HighLight) HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjLineWidth(sel_ptr->obj, Width, AW, AH, width_spec, aw_spec,
            ah_spec)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      int ltx, lty, rbx, rby;

      SetFileModified(TRUE);
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
   }
   if (HighLight) HighLightForward();
}

void SetSelectedLineWidth()
{
   char spec[MAXSTRING+1], *w_str=NULL, *aw_str=NULL, *ah_str=NULL;
   int w=(-1), aw=(-1), ah=(-1);
   char width_spec[40], aw_spec[40], ah_spec[40];
   float fw, faw, fah;

   if (topSel == NULL) {
      MsgBox("No object selected!", TOOL_NAME, INFO_MB);
      return;
   }
   *spec = '\0';
   Dialog("Please enter line width (arrow width and height are optional):",
         "( <CR>: accept, <ESC>: cancel )", spec);
   UtilTrimBlanks(spec);
   if (*spec == '\0') return;
   *width_spec = *aw_spec = *ah_spec = '\0';
   if ((w_str=strtok(spec, ", \t\n\r")) == NULL) return;
   UtilTrimBlanks(w_str);
   if (sscanf(w_str, "%f", &fw) != 1 || fw < (float)0.0) {
      sprintf(gszMsgBox, "Invalid width: '%s'.", w_str);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   w = round(fw);
   UtilStrCpy(width_spec, sizeof(width_spec), w_str);

   if ((aw_str=strtok(NULL, ", \t\n\r")) != NULL &&
         (ah_str=strtok(NULL, ", \t\n\r")) != NULL) {
      UtilTrimBlanks(aw_str);
      UtilTrimBlanks(ah_str);
      if (sscanf(aw_str, "%f", &faw) != 1 || faw < (float)0.0 ||
            sscanf(ah_str, "%f", &fah) != 1 || fah < (float)0.0) {
         sprintf(gszMsgBox, "Invalid arrow width and height: '%s %s'.",
               aw_str, ah_str);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return;
      }
      aw = round(faw);
      UtilStrCpy(aw_spec, sizeof(aw_spec), aw_str);
      ah = round(fah);
      UtilStrCpy(ah_spec, sizeof(ah_spec), ah_str);
   }
   ChangeAllSelRealLineWidth(w, aw, ah, width_spec, aw_spec, ah_spec, TRUE);
}
