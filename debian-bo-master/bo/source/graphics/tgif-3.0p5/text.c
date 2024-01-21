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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/text.c,v 3.1 1996/05/14 12:55:58 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

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
#include "file.e"
#include "font.e"
#include "grid.e"
#include "mainloop.e"
#include "mark.e"
#include "msg.e"
#include "names.e"
#include "obj.e"
#include "pattern.e"
#include "poly.e"
#include "prtgif.e"
#include "raster.e"
#include "rect.e"
#include "ruler.e"
#include "scroll.e"
#include "select.e"
#include "setup.e"
#include "stretch.e"
#ifndef _NO_EXTERN
#include "text.e"
#endif
#include "util.e"
#include "xpixmap.e"

#define PAINT 1
#define FRONT_HIGHLIGHT 2
#define MID_HIGHLIGHT 4
#define BACK_HIGHLIGHT 8

#define ERASE 0
#define PAINT_NORM (PAINT)
#define PAINT_INV (PAINT|FRONT_HIGHLIGHT|MID_HIGHLIGHT|BACK_HIGHLIGHT)
#define PAINT_NORM_INV (PAINT|MID_HIGHLIGHT|BACK_HIGHLIGHT)
#define PAINT_INV_NORM (PAINT|FRONT_HIGHLIGHT)
#define PAINT_NORM_INV_NORM (PAINT|MID_HIGHLIGHT)

#ifndef XK_KP_Left
#define XK_KP_Left	0xFF96
#define XK_KP_Up	0xFF97
#define XK_KP_Right	0xFF98
#define XK_KP_Down	0xFF99
#endif /* ~XK_KP_LEFT */

int		textDrawn=FALSE;
int		curTextModified=FALSE;
int		textVSpace=0;

int		textJust=JUST_L;
int		textCursorShown=FALSE;
int		textCursorH=14; /* UNSCALED height of the text cursor */
int		editingText=FALSE;
struct ObjRec	*curTextObj=NULL;

static struct ObjRec	*justDrawnTextObj=NULL;

static struct StrRec	*firstStr=NULL, *lastStr=NULL, *curStr=NULL;

static int	textOrigX=20, textOrigY=20, textCurX=20, textCurY=20;
		/* textOrigX, textOrigY, textCurX, textCurY */
		/*   are UNSCALED screen offsets */
static int	textAbsMinLBearing=0, textAbsMaxRExtra=0;
static int	textW=0, textH=0; /* absolute for the current text font */
static int	textAbsX=INVALID, textAbsY=INVALID;
		/* textAbsX and textAbsY are absolute coordinates */
static int	textCurIndex=0;
static int	curStrW=0; /* UNSCALED width of the current string */

static int	savedTextLtX, savedTextLtY, savedTextRbX, savedTextRbY;

static int	tmpAdjX=0, tmpAdjY=0;

/* the following static variables are for handling text highlight */
static int		textEndX=0, textEndY=0, textEndIndex=0;
static int		textHighlight=FALSE, endStrW=0;
static struct StrRec	*endStr=NULL;

static int	escPressed=FALSE;

#ifdef _NO_EXTERN
void	DrawTextObj ARGS_DECL((Window, int XOff, int YOff, struct ObjRec *));
void	RedrawCurText ARGS_DECL((void));
#endif /* _NO_EXTERN */

/* --------------------- DynStr Routines --------------------- */

void FreeDynStr (dyn_str)
   struct DynStrRec	* dyn_str;
{
   if (dyn_str == NULL) return;
   if (dyn_str->s != NULL) free(dyn_str->s);
   free(dyn_str);
}

void DynStrCpy (dest_dyn_str, src_dyn_str)
   struct DynStrRec	* dest_dyn_str, * src_dyn_str;
{
   if (dest_dyn_str->s != NULL) free(dest_dyn_str->s);
   if (src_dyn_str->sz == 0) {
      dest_dyn_str->s = NULL;
   } else {
      dest_dyn_str->s = (char *)malloc((src_dyn_str->sz)*sizeof(char));
      if (dest_dyn_str->s == NULL) FailAllocMessage();
      strcpy(dest_dyn_str->s, src_dyn_str->s);
   }
   dest_dyn_str->sz = src_dyn_str->sz;
}

struct DynStrRec * NewDynStr ()
{
   struct DynStrRec	* new_dyn_str;

   new_dyn_str = (struct DynStrRec *)malloc(sizeof(struct DynStrRec));
   if (new_dyn_str == NULL) FailAllocMessage();
   new_dyn_str->s = NULL;
   new_dyn_str->sz = 0;
   return (new_dyn_str);
}

struct DynStrRec * DynStrDup (dyn_str)
   struct DynStrRec	* dyn_str;
{
   struct DynStrRec	* new_dyn_str;

   new_dyn_str = (struct DynStrRec *)malloc(sizeof(struct DynStrRec));
   if (new_dyn_str == NULL) FailAllocMessage();
   new_dyn_str->s = NULL;
   DynStrCpy (new_dyn_str, dyn_str);
   return (new_dyn_str);
}

void DynStrSet (dest_dyn_str, s)
   struct DynStrRec	* dest_dyn_str;
   char			* s;
{
   int	sz=strlen(s)+1;

   if (dest_dyn_str->s != NULL) free(dest_dyn_str->s);
   dest_dyn_str->s = (char*)malloc(sz*sizeof(char));
   if (dest_dyn_str->s == NULL) FailAllocMessage();
   strcpy(dest_dyn_str->s, s);
   dest_dyn_str->sz = sz;
}

/* --------------------- Str Routines --------------------- */

void FreeStr (str_ptr)
   struct StrRec	* str_ptr;
{
   if (str_ptr->dyn_str.s != NULL) free(str_ptr->dyn_str.s);
   free(str_ptr);
}

struct StrRec * NewStr ()
{
   struct StrRec	* new_str_ptr;

   new_str_ptr = (struct StrRec *)malloc(sizeof(struct StrRec));
   if (new_str_ptr == NULL) { FailAllocMessage(); return NULL; }
   new_str_ptr->next = new_str_ptr->prev = NULL;
   new_str_ptr->dyn_str.s = NULL;
   new_str_ptr->dyn_str.sz = 0;
   DynStrSet (&new_str_ptr->dyn_str, "");
   return (new_str_ptr);
}

/* --------------------- TextRec Routines --------------------- */

void CopyCurInfoToTextPtr (text_ptr)
   struct TextRec * text_ptr;
{
   text_ptr->font = curFont;
   text_ptr->style = curStyle;
   text_ptr->size = curSize;
   text_ptr->just = textJust;
   text_ptr->v_space = textVSpace;
   text_ptr->rotate = curRotate;
   text_ptr->pen = penPat;
   text_ptr->fill = objFill;
   text_ptr->underline_on = curUnderlineOn;
   text_ptr->underline = curUnderline;

   text_ptr->asc = canvasFontAsc;
   text_ptr->des = canvasFontDes;
   text_ptr->min_lbearing = 0;
   text_ptr->max_rextra = 0;
   text_ptr->direction = canvasFontDirection;
   text_ptr->double_byte = canvasFontDoubleByte;

   text_ptr->read_only = FALSE;
   text_ptr->orig_w = text_ptr->orig_h = 0;
   text_ptr->font_name = NULL;
   text_ptr->custom_screen_font_name = NULL;

   text_ptr->attr = NULL;
   text_ptr->lines = 0;
   text_ptr->first = text_ptr->last = NULL;

   text_ptr->image = NULL;
   text_ptr->cached_bitmap = None;
   text_ptr->cached_zoom = zoomScale;
   text_ptr->cached_zoomed = zoomedIn;
   text_ptr->cached_rotate = curRotate;
}

#define BLUR 32

void BlurText (Win, gc, XOff, YOff, W, H)
   Window	Win;
   GC		gc;
   int		XOff, YOff, W, H;
   /* XOff and YOff are screen offsets (scaled and translated) */
{
   XPoint	v[5];

   v[0].x = (short)XOff; v[0].y = (short)YOff;
   v[1].x = (short)XOff; v[1].y = (short)YOff+H+1;
   v[2].x = (short)XOff+W+1; v[2].y = (short)YOff+H+1;
   v[3].x = (short)XOff+W+1; v[3].y = (short)YOff;
   v[4].x = (short)XOff; v[4].y = (short)YOff;

   XFillPolygon (mainDisplay, Win, gc, v, 5, Convex, CoordModeOrigin);
}

static
void AddStr (PrevPtr, NextPtr, StrPtr)
   struct StrRec	* PrevPtr, * NextPtr, * StrPtr;
{
   StrPtr->prev = PrevPtr;
   StrPtr->next = NextPtr;

   if (PrevPtr == NULL)
      firstStr = StrPtr;
   else
      PrevPtr->next = StrPtr;

   if (NextPtr == NULL)
      lastStr = StrPtr;
   else
      NextPtr->prev = StrPtr;
}

void InitText ()
{
   XGCValues	values;

   textBackingPixmap = XCreatePixmap (mainDisplay, mainWindow, 10, 10, 1);
   if (textBackingPixmap==None) Error ("InitText()","Can not XCreatePixmap()");
   textBackingPixmapW = 10;
   textBackingPixmapH = 10;

   values.foreground = 1;
   values.background = 0;
   values.fill_style = FillSolid;
   values.function = GXcopy;
   rotateGC = XCreateGC (mainDisplay, textBackingPixmap,
         GCForeground | GCBackground | GCFillStyle | GCFunction,
         &values);
   if (rotateGC==NULL) Error ("InitText()", "Can not XCreateGC()");
}

void CleanUpText ()
{
   XFreePixmap (mainDisplay, textBackingPixmap);
   textBackingPixmap = None;
   XFreeGC (mainDisplay, rotateGC);
   rotateGC = NULL;
}

static
void PaintText (Win,Str,Just,Rotate,XOff,YOff,xfs,ColorIndex,Pen)
   register int	XOff, YOff;
   Window	Win;
   char		* Str;
   int		Just, Rotate, ColorIndex, Pen;
   XFontStruct	* xfs;
   /* XOff and YOff are UNSCALED screen offset */
{
   int		orig_w, orig_h, w, len;
   XGCValues	values;
   struct BBRec	text_bbox;

   if (Pen == NONEPAT) return;

   XOff = ZOOMED_SIZE(XOff);
   YOff = ZOOMED_SIZE(YOff);

   len = strlen (Str);

   orig_w = XTextWidth (xfs, Str, len);
   orig_h = textCursorH;
   w = ZOOMED_SIZE(orig_w);

   switch (Just)
   {
      case JUST_L: break;
      case JUST_C:
         switch (Rotate)
         {
            case ROTATE0: XOff -= w/2; break;
            case ROTATE90: YOff -= w/2; break;
            case ROTATE180: XOff += w/2; break;
            case ROTATE270: YOff += w/2; break;
         }
         break;
      case JUST_R:
         switch (Rotate)
         {
            case ROTATE0: XOff -= w; break;
            case ROTATE90: YOff -= w; break;
            case ROTATE180: XOff += w; break;
            case ROTATE270: YOff += w; break;
         }
         break;
   }
   text_bbox.ltx = ABS_X(XOff);
   text_bbox.lty = ABS_Y(YOff);
   text_bbox.rbx = ABS_X(XOff+w);
   text_bbox.rby = ABS_Y(YOff+ZOOMED_SIZE(orig_h));
   if (checkBBox && !BBoxIntersect (text_bbox, drawWinBBox)) return;

   values.foreground = colorPixels[ColorIndex];
   values.function = GXcopy;
   values.fill_style = FillOpaqueStippled;
   values.stipple = patPixmap[Pen];
   XChangeGC (mainDisplay, drawGC,
         GCForeground | GCFunction | GCFillStyle | GCStipple, &values);
   XDrawString (mainDisplay,Win,drawGC,XOff,YOff+canvasFontAsc,Str,len);
}

static
void PaintCurText (Win, gc, Str, Just, XOff, YOff, xfs, ColorIndex, Pen,
      Mode, FirstIndex, SecondIndex)
   register int	XOff, YOff;
   Window	Win;
   GC		gc;
   char		* Str;
   int		Just, ColorIndex, Pen, Mode, FirstIndex, SecondIndex;
   XFontStruct	* xfs;
   /* XOff and YOff are UNSCALED screen offset */
{
   int		w, h, left, right, len, dir, asc, des, lbearing, rextra;
   XGCValues	values;
   struct BBRec	text_bbox;
   XCharStruct	xcs;

   len = strlen (Str);

   if (xfs->min_byte1 != 0 || xfs->max_byte1 != 0)
      XTextExtents16 (xfs, (XChar2b*)Str, (len>>1), &dir, &asc, &des, &xcs);
   else
      XTextExtents (xfs, Str, len, &dir, &asc, &des, &xcs);
   w = xcs.width;
   h = textCursorH;
   lbearing = (xcs.lbearing < 0 ? xcs.lbearing : 0);
   rextra = (xcs.rbearing-w > 0 ? xcs.rbearing-w : 0);

   switch (Just)
   {
      case JUST_L: break;
      case JUST_C: XOff -= w/2; break;
      case JUST_R: XOff -= w; break;
   }
   text_bbox.ltx = ABS_X(XOff);
   text_bbox.lty = ABS_Y(YOff);
   text_bbox.rbx = ABS_X(XOff+w);
   text_bbox.rby = ABS_Y(YOff+h);
   if (!BBoxIntersect (text_bbox, drawWinBBox)) return;

   if (Mode & PAINT)
   {
      unsigned long	xor_pixel;

      if (colorPixels[ColorIndex] == myBgPixel)
      {
         values.foreground = myFgPixel;
         xor_pixel = xorOne;
      }
      else
      {
         values.foreground = colorPixels[ColorIndex];
         xor_pixel = xorColorPixels[ColorIndex];
      }
      values.function = GXcopy;
      values.fill_style = FillSolid;
      XChangeGC (mainDisplay, gc,
            GCForeground | GCFunction | GCFillStyle, &values);
      XDrawString (mainDisplay, Win, gc, XOff, YOff+canvasFontAsc, Str, len);

      switch (Mode)
      {
         case PAINT_NORM: break;
         case PAINT_INV:
            XSetForeground (mainDisplay, revDefaultGC, xor_pixel);
            XFillRectangle (mainDisplay, Win, revDefaultGC, XOff, YOff,
                  w, h);
            XSetForeground (mainDisplay, revDefaultGC, xorOne);
            break;
         case PAINT_NORM_INV:
            left = XTextWidth (xfs, Str, FirstIndex);
            XSetForeground (mainDisplay, revDefaultGC, xor_pixel);
            XFillRectangle (mainDisplay, Win, revDefaultGC, XOff+left, YOff,
                  w-left, h);
            XSetForeground (mainDisplay, revDefaultGC, xorOne);
            break;
         case PAINT_INV_NORM:
            left = XTextWidth (xfs, Str, FirstIndex);
            XSetForeground (mainDisplay, revDefaultGC, xor_pixel);
            XFillRectangle (mainDisplay, Win, revDefaultGC, XOff, YOff,
                  left, h);
            XSetForeground (mainDisplay, revDefaultGC, xorOne);
            break;
         case PAINT_NORM_INV_NORM:
            left = XTextWidth (xfs, Str, FirstIndex);
            right = XTextWidth (xfs, Str, SecondIndex);
            XSetForeground (mainDisplay, revDefaultGC, xor_pixel);
            XFillRectangle (mainDisplay, Win, revDefaultGC, XOff+left, YOff,
                  right-left, h);
            XSetForeground (mainDisplay, revDefaultGC, xorOne);
            break;
      }
   }
   else
   {
      values.foreground = myBgPixel;
      values.function = GXcopy;
      values.fill_style = FillSolid;
      XChangeGC (mainDisplay, gc,
            GCForeground | GCFunction | GCFillStyle, &values);

      XFillRectangle (mainDisplay, Win, gc, XOff+lbearing-1, YOff,
            w-lbearing+rextra+1, h);
   }
}

void PutTextCursor ()
{
   XDrawLine (mainDisplay, drawWindow, defaultGC, textCurX,
         textCurY, textCurX, textCurY+textCursorH);
}

void EraseTextCursor ()
{
   XSetForeground (mainDisplay, revDefaultGC, myFgPixel^myBgPixel);
   XDrawLine (mainDisplay, drawWindow, revDefaultGC, textCurX,
         textCurY, textCurX, textCurY+textCursorH);
   XSetForeground (mainDisplay, revDefaultGC, xorOne);
}

static int	curTextIsNew=FALSE;

void NewCurText ()
{
   struct TextRec	* text_ptr;

   firstStr = lastStr = curStr = NewStr ();

   if (firstStr == NULL) FailAllocMessage ();
   if (textCursorH+textVSpace <= 0)
   {
      Msg ("Text vertical spacing too small.  Reset to 0.");
      textVSpace = 0;
      ShowTextVSpace ();
   }

   text_ptr = (struct TextRec *)malloc(sizeof(struct TextRec));
   if (text_ptr == NULL) FailAllocMessage();
   memset(text_ptr, 0, sizeof(struct TextRec));
   text_ptr->font = curFont;
   text_ptr->style = curStyle;
   text_ptr->attr = NULL;
   text_ptr->size = curSize;
   text_ptr->just = textJust;
   text_ptr->v_space = textVSpace;
   text_ptr->rotate = curRotate;
   text_ptr->pen = penPat;
   text_ptr->fill = objFill;
   text_ptr->cached_bitmap = None;
   text_ptr->cached_zoom = 0;
   text_ptr->cached_rotate = ROTATE0;
   text_ptr->asc = canvasFontAsc;
   text_ptr->des = canvasFontDes;
   text_ptr->lines = 1;
   text_ptr->first = firstStr;
   text_ptr->last = lastStr;

   text_ptr->read_only = FALSE;
   text_ptr->orig_w = text_ptr->orig_h = 0;
   text_ptr->font_name = NULL;
   text_ptr->custom_screen_font_name = NULL;

   text_ptr->underline_on = curUnderlineOn;
   text_ptr->underline = curUnderline;
   text_ptr->min_lbearing = 0;
   text_ptr->max_rextra = 0;
   text_ptr->direction = canvasFontDirection;
   text_ptr->double_byte = canvasFontDoubleByte;

   curTextObj = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (curTextObj == NULL) FailAllocMessage();
   memset(curTextObj, 0, sizeof(struct ObjRec));
   curTextObj->x = textAbsX;
   curTextObj->y = textAbsY;
   curTextObj->type = OBJ_TEXT;
   curTextObj->color = colorIndex;
   curTextObj->id = objId++;
   curTextObj->dirty = FALSE;
   curTextObj->rotation = 0;
   curTextObj->locked = FALSE;

   curTextObj->detail.t = text_ptr;
   curTextObj->fattr = curTextObj->lattr = NULL;
   curTextObj->ctm = NULL;
   curTextObj->invisible = FALSE;
   AddObj (NULL, topObj, curTextObj);

   textW = 0;
   textH = textCursorH;

   textCursorShown = TRUE;
   textHighlight = FALSE;

   curTextIsNew = TRUE;
}

void FreeTextObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   if (ObjPtr->detail.t != NULL)
   {
      register struct StrRec	* s_ptr, * next_str;

      for (s_ptr = ObjPtr->detail.t->first; s_ptr != NULL; s_ptr = next_str)
      {
         next_str = s_ptr->next;
         FreeStr (s_ptr);
      }

      if (ObjPtr->detail.t->cached_bitmap != None)
         XFreePixmap (mainDisplay, ObjPtr->detail.t->cached_bitmap);

      if (ObjPtr->detail.t->font_name != NULL)
         free(ObjPtr->detail.t->font_name);
      free(ObjPtr->detail.t);
   }
   free(ObjPtr);
}

static ShowTextRelatedInfo()
{
   ShowJust();
   ShowPen();
   ShowFill();
   ShowColor(FALSE);
   ShowCurFont();
   ShowTextVSpace();
   ShowTextSize();
}

int CreateTextObj()
   /* returns TRUE if something got created */
   /* returns FALSE otherwise */
{
   struct AttrRec *attr_ptr;
   int num_lines, max_len=0, min_lbearing=0, max_rextra=0;
   int ltx=0, lty=0, rbx=0, rby=0, scr_ltx=0, scr_lty;

   if (!textCursorShown) return FALSE;
   EraseTextCursor();

   if (firstStr == lastStr && *curStr->dyn_str.s == '\0') {
      /* no text entered or all text erased */
      if ((attr_ptr = curTextObj->detail.t->attr) != NULL) {
         /* the text being edited is an attribute */
         if (attr_ptr->nameshown) {
            UnlinkAttr(attr_ptr);
            FreeTextObj(curTextObj);
            FreeAttr(attr_ptr);
         } else {
            curTextObj->detail.t->lines = 1;
            UpdateAttr(curTextObj->detail.t, attr_ptr);
         }
         AdjObjBBox(attr_ptr->owner);
         if (outerSel != NULL) AdjAncestorsBBox();
         if (curTextModified) {
            if (outerSel != NULL) {
               RecordReplaceAnObj(outerSel->obj);
            } else {
               RecordReplaceAnObj(attr_ptr->owner);
            }
         } else {
            AbortPrepareCmd(CMD_REPLACE);
         }
      } else {
         if (outerSel != NULL) {
            UnlinkCurTextFromInnerSel();
            AdjAncestorsBBox();
         }
         if (!curTextIsNew) {
            if (outerSel != NULL) {
               RecordReplaceAnObj(outerSel->obj);
            } else {
               ChangeReplaceOneCmdToDeleteCmd();
            }
         } else {
            AbortPrepareCmd(CMD_REPLACE);
         }
         if (curTextObj != NULL) {
            if (outerSel != NULL) {
               /* curTextObj already broken off from the main */
               /*	stream of objects, so just free it. */
               FreeObj(curTextObj);
            } else {
               DelObj(curTextObj);
            }
         }
      }
      switch (textJust) {
      case JUST_L:
         scr_ltx = OFFSET_X(textAbsX-2);
         if (zoomedIn) {
            ltx = textAbsX-2-GRID_ABS_SIZE(2);
            rbx = textAbsX+textW+2+GRID_ABS_SIZE(2);
         } else {
            ltx = textAbsX-ABS_SIZE(2)-GRID_ABS_SIZE(2);
            rbx = textAbsX+ABS_SIZE(textW+2)+GRID_ABS_SIZE(2);
         }
         break;
      case JUST_C:
         scr_ltx = OFFSET_X(textAbsX)-textW/2-2;
         if (zoomedIn) {
            ltx = textAbsX-textW/2-2-GRID_ABS_SIZE(2);
            rbx = textAbsX+textW/2+2+GRID_ABS_SIZE(2);
         } else {
            ltx = textAbsX-ABS_SIZE(textW/2+2)-GRID_ABS_SIZE(2);
            rbx = textAbsX+ABS_SIZE(textW/2+2)+GRID_ABS_SIZE(2);
         }
         break;
      case JUST_R:
         scr_ltx = OFFSET_X(textAbsX)-textW-2;
         if (zoomedIn) {
            ltx = textAbsX-textW-2-GRID_ABS_SIZE(2);
            rbx = textAbsX+2+GRID_ABS_SIZE(2);
         } else {
            ltx = textAbsX-ABS_SIZE(textW+2)-GRID_ABS_SIZE(2);
            rbx = textAbsX+ABS_SIZE(2)+GRID_ABS_SIZE(2);
         }
         break;
      }
      scr_lty = OFFSET_Y(textAbsY)-2;
      if (zoomedIn) {
         lty = textAbsY-2-GRID_ABS_SIZE(2);
         rby = textAbsY+textH+2+GRID_ABS_SIZE(2);
      } else {
         lty = textAbsY-ABS_SIZE(2)-GRID_ABS_SIZE(2);
         rby = textAbsY+ABS_SIZE(textH+2)+GRID_ABS_SIZE(2);
      }
      if (editingText) {
         XClearArea(mainDisplay, drawWindow, scr_ltx, scr_lty,
               textW+5, textH+5, FALSE);
         RedrawAreas(botObj, savedTextLtX-GRID_ABS_SIZE(2),
               savedTextLtY-GRID_ABS_SIZE(2), savedTextRbX+GRID_ABS_SIZE(2),
               savedTextRbY+GRID_ABS_SIZE(2),
               ltx, lty, rbx, rby);
      } else {
         RedrawAnArea(botObj, ltx, lty, rbx, rby);
      }
      firstStr = lastStr = curStr = NULL;
      textCursorShown = FALSE;
      curTextObj = NULL;
      textCurIndex = 0;
      curStrW = 0;

      if (editingText) {
         ShowTextRelatedInfo();
         editingText = FALSE;
      }
      textDrawn = FALSE;
      justDrawnTextObj = NULL;
      textHighlight = FALSE;
      curTextIsNew = FALSE;
      return FALSE;
   }
   if (curTextModified) {
      if (curTextObj->detail.t->cached_bitmap != None) {
         XFreePixmap(mainDisplay, curTextObj->detail.t->cached_bitmap);
      }
      curTextObj->detail.t->cached_zoom = 0;
      curTextObj->detail.t->cached_bitmap = None;
   }
   curTextObj->detail.t->last = lastStr;
   GetTextObjSizeInfo(curTextObj->detail.t, &num_lines, &max_len,
         &min_lbearing, &max_rextra);
   curTextObj->detail.t->lines = num_lines;

   curTextObj->x = textAbsX-tmpAdjX;
   curTextObj->y = textAbsY-tmpAdjY;
   if (curTextObj->ctm != NULL) {
      curTextObj->x -= curTextObj->ctm->m[CTM_TX];
      curTextObj->y -= curTextObj->ctm->m[CTM_TY];
   }
   curTextObj->detail.t->min_lbearing = min_lbearing;
   curTextObj->detail.t->max_rextra = max_rextra;

   SetTextOrigBBoxes(curTextObj, textJust, max_len,
         num_lines*textCursorH+(num_lines-1)*textVSpace,
         min_lbearing, max_rextra, curRotate);
   SetTextBBox(curTextObj, textJust, max_len,
         num_lines*textCursorH+(num_lines-1)*textVSpace,
         min_lbearing, max_rextra, curRotate);
   if (curTextObj->ctm != NULL) {
      GetTransformedOBBoxOffsetVs(curTextObj, curTextObj->rotated_obbox);
   }
   switch (textJust) {
   case JUST_L:
      scr_ltx = OFFSET_X(textAbsX-2);
      if (zoomedIn) {
         ltx = textAbsX-2-GRID_ABS_SIZE(2);
         rbx = textAbsX+textW+2+GRID_ABS_SIZE(2);
      } else {
         ltx = textAbsX-ABS_SIZE(2)-GRID_ABS_SIZE(2);
         rbx = textAbsX+ABS_SIZE(textW+2)+GRID_ABS_SIZE(2);
      }
      break;
   case JUST_C:
      scr_ltx = OFFSET_X(textAbsX)-textW/2-2;
      if (zoomedIn) {
         ltx = textAbsX-textW/2-2-GRID_ABS_SIZE(2);
         rbx = textAbsX+textW/2+2+GRID_ABS_SIZE(2);
      } else {
         ltx = textAbsX-ABS_SIZE(textW/2+2)-GRID_ABS_SIZE(2);
         rbx = textAbsX+ABS_SIZE(textW/2+2)+GRID_ABS_SIZE(2);
      }
      break;
   case JUST_R:
      scr_ltx = OFFSET_X(textAbsX)-textW-2;
      if (zoomedIn) {
         ltx = textAbsX-textW-2-GRID_ABS_SIZE(2);
         rbx = textAbsX+2+GRID_ABS_SIZE(2);
      } else {
         ltx = textAbsX-ABS_SIZE(textW+2)-GRID_ABS_SIZE(2);
         rbx = textAbsX+ABS_SIZE(2)+GRID_ABS_SIZE(2);
      }
      break;
   }
   scr_lty = OFFSET_Y(textAbsY);
   if (zoomedIn) {
      lty = textAbsY-2-GRID_ABS_SIZE(2);
      rby = textAbsY+textH+2+GRID_ABS_SIZE(2);
   } else {
      lty = textAbsY-ABS_SIZE(2)-GRID_ABS_SIZE(2);
      rby = textAbsY+ABS_SIZE(textH+2)+GRID_ABS_SIZE(2);
   }

   if ((attr_ptr=curTextObj->detail.t->attr) != NULL) {
      UpdateAttr(curTextObj->detail.t, attr_ptr);
      textDrawn = FALSE;
      justDrawnTextObj = NULL;
      if (curTextModified && AutoCenterAttr(attr_ptr->owner)) {
         struct BBRec	bbox;

         CenterObjInOBBox(attr_ptr->obj, attr_ptr->owner->obbox, &bbox);
         if (bbox.ltx < ltx) ltx = bbox.ltx;
         if (bbox.lty < lty) lty = bbox.lty;
         if (bbox.rbx > rbx) rbx = bbox.rbx;
         if (bbox.rby > rby) rby = bbox.rby;
      }
      AdjObjBBox(attr_ptr->owner);
      if (outerSel != NULL) AdjAncestorsBBox();

      if (curTextModified) {
         if (outerSel != NULL) {
            RecordReplaceAnObj(outerSel->obj);
         } else {
            RecordReplaceAnObj(attr_ptr->owner);
         }
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   } else {
      if (outerSel != NULL) {
         textDrawn = FALSE;
         justDrawnTextObj = NULL;
         AdjAncestorsBBox();
      } else {
         textDrawn = TRUE;
         justDrawnTextObj = curTextObj;
      }
      if (curTextIsNew) {
         AbortPrepareCmd(CMD_REPLACE);
         RecordNewObjCmd();
      } else if (curTextModified) {
         if (outerSel != NULL) {
            RecordReplaceAnObj(outerSel->obj);
         } else {
            RecordReplaceAnObj(curTextObj);
         }
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   if (curTextObj->detail.t->rotate != ROTATE0) {
      struct ObjRec *obj_ptr=curTextObj;
      struct TextRec *text_ptr=obj_ptr->detail.t;
      int rotate=text_ptr->rotate, text_just=text_ptr->just, tmp_ltx, tmp_lty;

      if (rotate == ROTATE90 || rotate == ROTATE270) {
         int h=obj_ptr->obbox.rbx-obj_ptr->obbox.ltx;
         int w=obj_ptr->obbox.rby-obj_ptr->obbox.lty;

         obj_ptr->obbox.rby = obj_ptr->obbox.lty + h;
         obj_ptr->obbox.rbx = obj_ptr->obbox.ltx + w;

         switch (text_just) {
         case JUST_L: obj_ptr->x = obj_ptr->obbox.ltx; break;
         case JUST_C:
            obj_ptr->x = ((obj_ptr->obbox.ltx+obj_ptr->obbox.rbx)>>1);
            break;
         case JUST_R: obj_ptr->x = obj_ptr->obbox.rbx; break;
         }
         obj_ptr->y = obj_ptr->obbox.lty;
      } else {
         switch (text_just) {
         case JUST_L: obj_ptr->x = obj_ptr->obbox.ltx; break;
         case JUST_C: break;
         case JUST_R: obj_ptr->x = obj_ptr->obbox.rbx; break;
         }
         obj_ptr->y = obj_ptr->obbox.lty;
      }
      tmp_ltx = (obj_ptr->obbox.ltx);
      tmp_lty = (obj_ptr->obbox.lty);
      SetRotatePivotByObject(obj_ptr);

      switch (rotate) {
      case ROTATE90:
         RotateObj(obj_ptr, CORNER_LT, CLOCKWISE90, &tmp_ltx, &tmp_lty);
         break;
      case ROTATE180:
         RotateObj(obj_ptr, CORNER_LT, CLOCKWISE90, &tmp_ltx, &tmp_lty);
         RotateObj(obj_ptr, CORNER_LT, CLOCKWISE90, &tmp_ltx, &tmp_lty);
         break;
      case ROTATE270:
         RotateObj(obj_ptr, CORNER_LT, COUNTER90, &tmp_ltx, &tmp_lty);
         break;
      }
      text_ptr->rotate = ROTATE0;
   }
   if (curTextObj->detail.t->rotate != ROTATE0) {
      fprintf(stderr, "CreateTextObj(): text object rotation is not 0!\n");
   }

   firstStr = lastStr = curStr = NULL;
   textCursorShown = FALSE;
   textCurIndex = 0;

   if (editingText) {
      int x;

      switch (textJust) {
      case JUST_L: x = textOrigX+textAbsMinLBearing; break;
      case JUST_C: x = textOrigX-textW/2+textAbsMinLBearing; break;
      case JUST_R: x = textOrigX-textW+textAbsMinLBearing; break;
      }
      XClearArea(mainDisplay, drawWindow, x-2, textOrigY-2,
            textW-textAbsMinLBearing+textAbsMaxRExtra+5, textH+5, FALSE);
      if (ABS_X(x) < ltx) ltx = ABS_X(x);
      if (ABS_Y(textOrigY) < lty) lty = ABS_Y(textOrigY);
      x += textW-textAbsMinLBearing+textAbsMaxRExtra+1;
      if (ABS_X(x) > rbx) rbx = ABS_X(x);
      if (ABS_Y(textOrigY+textH+1) > rby) rby = ABS_Y(textOrigY+textH+1);
      if (curTextObj->ctm != NULL) {
         int i;

         for (i=0; i < 4; i++) {
            int tmp_x=ABS_X(curTextObj->rotated_obbox[i].x);
            int tmp_y=ABS_Y(curTextObj->rotated_obbox[i].y);

            if (tmp_x < ltx) ltx = tmp_x;
            if (tmp_y < lty) lty = tmp_y;
            if (tmp_x > rbx) rbx = tmp_x;
            if (tmp_y > rby) rby = tmp_y;
         }
      }
      RedrawAreas(botObj, savedTextLtX-GRID_ABS_SIZE(2),
            savedTextLtY-GRID_ABS_SIZE(2), savedTextRbX+GRID_ABS_SIZE(2),
            savedTextRbY+GRID_ABS_SIZE(2), ltx-GRID_ABS_SIZE(2),
            lty-GRID_ABS_SIZE(2), rbx+GRID_ABS_SIZE(2), rby+GRID_ABS_SIZE(2));
      if (curRotate != ROTATE0) {
         DrawTextObj(drawWindow, drawOrigX, drawOrigY, curTextObj);
      }
   } else {
      int x;

      switch (textJust) {
      case JUST_L: x = textOrigX+textAbsMinLBearing; break;
      case JUST_C: x = textOrigX-textW/2+textAbsMinLBearing; break;
      case JUST_R: x = textOrigX-textW+textAbsMinLBearing; break;
      }
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(2), lty-GRID_ABS_SIZE(2),
            rbx+GRID_ABS_SIZE(2), rby+GRID_ABS_SIZE(2), ABS_X(x-2),
            ABS_Y(textOrigY-2),
            ABS_X(x+textW-textAbsMinLBearing+textAbsMaxRExtra+3),
            ABS_Y(textOrigY+textH+3));
      if (curRotate != ROTATE0) {
         DrawTextObj(drawWindow, drawOrigX, drawOrigY, curTextObj);
      }
   }
   textOrigX = textOrigY = textCurX = textCurY = ABS_SIZE(20);
   textAbsMinLBearing = textAbsMaxRExtra = 0;
   curStrW = 0;
   textW = textH = 0;
   textAbsX = textOrigX + drawOrigX;
   textAbsY = textOrigY + drawOrigY;
   curTextObj = NULL;

   if (editingText) {
      PopCurFont();
      ShowTextRelatedInfo();
      editingText = FALSE;
   }
   textHighlight = FALSE;
   curTextIsNew = FALSE;

   return TRUE;
}

void HighLightJustDrawnText()
{
   AddNewSelObj(justDrawnTextObj);
   UpdSelBBox();
   HighLightAnObj(justDrawnTextObj);
   justDupped = FALSE;
}

static
void SetTextCurX ()
   /* set textCurX according to textCurIndex */
{
   register int	left = 0, w;
   char		s[MAXSTRING+1];

   strcpy (s, curStr->dyn_str.s);
   s[textCurIndex] = '\0';

   w = XTextWidth (canvasFontPtr, s, strlen (s));

   switch (textJust)
   {
      case JUST_L: left = textOrigX; break;
      case JUST_C: left = textOrigX-curStrW/2; break;
      case JUST_R: left = textOrigX-curStrW; break;
   }
   textCurX = left + w;
}

static struct StrRec	* highlightStartStr=NULL, * highlightEndStr=NULL;
static int		highlightStartIndex=0, highlightEndIndex=0;
static int		highlightStartY=0;

static
void XorText ()
{
   int			x=0, y, w, start_offset, end_offset, len;
   char			buf[MAXSTRING+1];
   unsigned long	xor_pixel;
   struct StrRec	* s_ptr;


   if (highlightStartStr==highlightEndStr &&
         highlightStartIndex==highlightEndIndex)
      return;

   if (colorPixels[colorIndex] == myBgPixel)
      xor_pixel = xorOne;
   else
      xor_pixel = xorColorPixels[colorIndex];

   y = highlightStartY;
   for (s_ptr = highlightStartStr; s_ptr != highlightEndStr->next;
         s_ptr = s_ptr->next)
   {
      len = s_ptr->dyn_str.sz-1;
      w = XTextWidth (canvasFontPtr, s_ptr->dyn_str.s, len);

      switch (textJust)
      {
         case JUST_L: x = textOrigX; break;
         case JUST_C: x = textOrigX-w/2; break;
         case JUST_R: x = textOrigX-w; break;
      }

      if (s_ptr == highlightStartStr)
      {
         strcpy (buf, s_ptr->dyn_str.s);
         buf[highlightStartIndex] = '\0';
         len = strlen (buf);
         start_offset = XTextWidth (canvasFontPtr, buf, len);
      }
      else
         start_offset = 0;

      if (s_ptr == highlightEndStr)
      {
         strcpy (buf, s_ptr->dyn_str.s);
         buf[highlightEndIndex] = '\0';
         len = strlen (buf);
         end_offset = XTextWidth (canvasFontPtr, buf, len);
      }
      else
         end_offset = w;

      XSetForeground (mainDisplay, revDefaultGC, xor_pixel);
      XFillRectangle (mainDisplay, drawWindow, revDefaultGC,
            x+start_offset, y, end_offset-start_offset,
            textCursorH);
      XSetForeground (mainDisplay, revDefaultGC, xorOne);

      y += textCursorH+textVSpace;
   }
}

static
int PrepareEditExistingText(obj_ptr, abs_x, abs_y, x_off, y_off)
   struct ObjRec *obj_ptr;
   int abs_x, abs_y, *x_off, *y_off;
{
   int dx, dy;

   curTextObj = obj_ptr;
   savedTextLtX = obj_ptr->bbox.ltx;
   savedTextLtY = obj_ptr->bbox.lty;
   savedTextRbX = obj_ptr->bbox.rbx;
   savedTextRbY = obj_ptr->bbox.rby;

   PushCurFont();
   editingText = TRUE;

   curFont = obj_ptr->detail.t->font;
   curSize = obj_ptr->detail.t->size;
   curStyle = obj_ptr->detail.t->style;
   textJust = obj_ptr->detail.t->just;
   curRotate = obj_ptr->detail.t->rotate;
   penPat = obj_ptr->detail.t->pen;
   objFill = obj_ptr->detail.t->fill;
   textVSpace = obj_ptr->detail.t->v_space;
   curUnderlineOn = obj_ptr->detail.t->underline_on;
   curUnderline = obj_ptr->detail.t->underline;
   changingFontSizeFromRead = FALSE;
   SetCanvasFont();
   changingFontSizeFromRead = TRUE;
   if (curSize != canvasFontSize) {
      sprintf(gszMsgBox, "Text size=%1d not available.\n\nCan not edit.",
            curSize);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      PopCurFont();
      SetCanvasFont();
      editingText = FALSE;
      curTextModified = FALSE;
      return FALSE;
   }
   colorIndex = obj_ptr->color;
   ShowTextRelatedInfo();
   CurFontMsg();

   firstStr = curStr = obj_ptr->detail.t->first;
   lastStr = obj_ptr->detail.t->last;

   textAbsX = obj_ptr->x;
   textAbsY = obj_ptr->y;
   if (obj_ptr->ctm != NULL) {
      textAbsX += obj_ptr->ctm->m[CTM_TX];
      textAbsY += obj_ptr->ctm->m[CTM_TY];
   }
   textOrigX = OFFSET_X(textAbsX);
   textOrigY = OFFSET_Y(textAbsY);
   textAbsMinLBearing = obj_ptr->detail.t->min_lbearing;
   textAbsMaxRExtra = obj_ptr->detail.t->max_rextra;

   switch (curRotate) {
      case ROTATE0:
         if (obj_ptr->ctm == NULL) {
            textW = obj_ptr->obbox.rbx - obj_ptr->obbox.ltx;
            textH = obj_ptr->obbox.rby - obj_ptr->obbox.lty;
            switch (textJust) {
            case JUST_L: tmpAdjX = ((textW-ABS_SIZE(textW))>>1); break;
            case JUST_C: tmpAdjX = 0; break;
            case JUST_R: tmpAdjX = ((ABS_SIZE(textW)-textW)>>1); break;
            }
            tmpAdjY = ((textH-ABS_SIZE(textH))>>1);
         } else {
            int new_x, new_y;

            textW = obj_ptr->orig_obbox.rbx - obj_ptr->orig_obbox.ltx;
            textH = obj_ptr->orig_obbox.rby - obj_ptr->orig_obbox.lty;
            switch (textJust) {
            case JUST_L:
               tmpAdjX = ((obj_ptr->obbox.ltx + obj_ptr->obbox.rbx -
                     ABS_SIZE(textW))>>1) - textAbsX;
               break;
            case JUST_C:
               tmpAdjX = 0;
               tmpAdjX = ((obj_ptr->obbox.ltx + obj_ptr->obbox.rbx)>>1) -
                     textAbsX;
               break;
            case JUST_R:
               tmpAdjX = ((obj_ptr->obbox.ltx + obj_ptr->obbox.rbx +
                     ABS_SIZE(textW))>>1) - textAbsX;
               break;
            }
            tmpAdjY = ((obj_ptr->obbox.lty + obj_ptr->obbox.rby -
                  ABS_SIZE(textH))>>1) - textAbsY;
         }
         break;
      case ROTATE90:
         dx = textAbsX - abs_x;
         dy = textAbsY - abs_y;
         abs_x = textAbsX - dy;
         abs_y = textAbsY + dx;
         textW = obj_ptr->obbox.rby - obj_ptr->obbox.lty;
         textH = obj_ptr->obbox.rbx - obj_ptr->obbox.ltx;
         switch (textJust) {
         case JUST_L:
            tmpAdjX = -(ABS_SIZE(textW)+textH)/2;
            tmpAdjY = (textW-ABS_SIZE(textH))/2;
            break;
         case JUST_C:
            tmpAdjX = -textH/2;
            tmpAdjY = -ABS_SIZE(textH)/2;
            break;
         case JUST_R:
            tmpAdjX = (ABS_SIZE(textW)-textH)/2;
            tmpAdjY = -(textW+ABS_SIZE(textH))/2;
            break;
         }
         break;
      case ROTATE180:
         abs_x = 2*textAbsX - abs_x;
         abs_y = 2*textAbsY - abs_y;
         textW = obj_ptr->obbox.rbx - obj_ptr->obbox.ltx;
         textH = obj_ptr->obbox.rby - obj_ptr->obbox.lty;
         switch (textJust) {
         case JUST_L: tmpAdjX = -(textW+ABS_SIZE(textW))/2; break;
         case JUST_C: tmpAdjX = 0; break;
         case JUST_R: tmpAdjX = (textW+ABS_SIZE(textW))/2; break;
         }
         tmpAdjY = -(textH+ABS_SIZE(textH))/2;
         break;
      case ROTATE270:
         dx = textAbsX - abs_x;
         dy = textAbsY - abs_y;
         abs_x = textAbsX + dy;
         abs_y = textAbsY - dx;
         textW = obj_ptr->obbox.rby - obj_ptr->obbox.lty;
         textH = obj_ptr->obbox.rbx - obj_ptr->obbox.ltx;
         switch (textJust) {
         case JUST_L:
            tmpAdjX = -(ABS_SIZE(textW)-textH)/2;
            tmpAdjY = -(textW+ABS_SIZE(textH))/2;
            break;
         case JUST_C:
            tmpAdjX = textH/2;
            tmpAdjY = -ABS_SIZE(textH)/2;
            break;
         case JUST_R:
            tmpAdjX = (ABS_SIZE(textW)+textH)/2;
            tmpAdjY = (textW-ABS_SIZE(textH))/2;
            break;
         }
         break;
   }
   textAbsX += tmpAdjX; textOrigX = OFFSET_X(textAbsX);
   textAbsY += tmpAdjY; textOrigY = OFFSET_Y(textAbsY);
   if (obj_ptr->ctm == NULL) {
      abs_x += tmpAdjX; *x_off = OFFSET_X(abs_x);
      abs_y += tmpAdjY; *y_off = OFFSET_Y(abs_y);
   } else {
      int x, y, ltx;

      ReverseTransformPointThroughCTM(abs_x-obj_ptr->x, abs_y-obj_ptr->y,
            obj_ptr->ctm, &x, &y);
      ltx = textOrigX;
      switch (textJust) {
      case JUST_L: break;
      case JUST_C: ltx -= (textW>>1); break;
      case JUST_R: ltx -= textW; break;
      }
      abs_x = ltx + obj_ptr->x + x - obj_ptr->orig_obbox.ltx;
      abs_y = textOrigY + obj_ptr->y + y - obj_ptr->orig_obbox.lty;
      *x_off = OFFSET_X(abs_x);
      *y_off = OFFSET_Y(abs_y);
   }
   if (outerSel != NULL) {
      PrepareToReplaceAnObj(outerSel->obj);
   } else if (obj_ptr->detail.t->attr == NULL) {
      PrepareToReplaceAnObj(obj_ptr);
   } else {
      PrepareToReplaceAnObj(obj_ptr->detail.t->attr->owner);
   }
   return TRUE;
}

static
int PartOfAWord(ch)
   char ch;
{
   return (ch == '_' || (ch >= '0' && ch <= '9') ||
         (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'));
}

static Time	lastClickTime;
static int	textJustClicked=FALSE;

static
void HandleButton (Button_Ev)
   XButtonEvent	* Button_Ev;
{
   int			grid_x, grid_y, x_off, y_off, amount;
   int                  left, pressed_in_same_text = FALSE, x = 0;
   int                  abs_x, abs_y, tmp_x, tmp_y;
   struct ObjRec	* obj_ptr = NULL;
   struct AttrRec       * attr_ptr;
   char			* c_ptr, s[2];
   int			done=FALSE, saved_end_index;
   XEvent		input, ev;
   struct StrRec	* saved_end_str;


   escPressed = FALSE;
   if (Button_Ev->button == Button1 &&
         !(Button_Ev->state & (ShiftMask|ControlMask)))
   {
      x_off = Button_Ev->x; abs_x = ABS_X(x_off);
      y_off = Button_Ev->y; abs_y = ABS_Y(y_off);

      if (textCursorShown)
      {
         switch (textJust)
         {
            case JUST_L: x = textOrigX-2; break;
            case JUST_C: x = textOrigX-textW/2-2; break;
            case JUST_R: x = textOrigX-textW-2; break;
         }
         if (x_off >= x && x_off <= x+textW+4 &&
               y_off >= textOrigY-2 && y_off <= textOrigY+textH+2)
            pressed_in_same_text = TRUE;
         else
         {
            CreateTextObj (); /* end editing on the old text */
            curTextModified = FALSE;
         }
      }
      else
      {
         editingText = FALSE;
         curTextModified = FALSE;
      }

      if (!pressed_in_same_text &&
            (obj_ptr = FindTextObj(x_off, y_off, NULL)) == NULL)
      {  /* cursor not within any existing text object */
         if (curSize != canvasFontSize)
         {
            char	msg[MAXSTRING+1];

            sprintf (msg, "Text size=%1d not available.\n\n%s.", curSize,
                  "Please try a different size");
            MsgBox (msg, TOOL_NAME, INFO_MB);
         }
         else
         {
            GridXY (x_off, y_off, &grid_x, &grid_y);

            textOrigX = textCurX = grid_x;
            textOrigY = textCurY = grid_y;
            textAbsMinLBearing = 0;
            textAbsMaxRExtra = 0;
            textAbsX = ABS_X(grid_x);
            textAbsY = ABS_Y(grid_y);
            tmpAdjX = tmpAdjY = 0;
            NewCurText ();
            RedrawCurText ();
            CurFontMsg ();
            PrepareToReplaceAnObj (curTextObj);
         }
      }
      else
      {  /* cursor inside an existing text object */
         Time	click_time=Button_Ev->time;
         int	double_clicked=FALSE;
         int	inherited_attr_left_index=(-1);

         double_clicked = (pressed_in_same_text && textJustClicked &&
               (click_time-lastClickTime) < doubleClickInterval);
         if (pressed_in_same_text)
         {
            obj_ptr = curTextObj;
            curStr = obj_ptr->detail.t->first;
            if (textJustClicked && (click_time-lastClickTime) <
                  doubleClickInterval)
               double_clicked = TRUE;
         }
         else if (!PrepareEditExistingText (obj_ptr,abs_x,abs_y,&x_off,&y_off))
            return;

         textCurY = textOrigY;
         if (pressed_in_same_text)
         {
            while (y_off >= textCurY+textCursorH+textVSpace &&
                  curStr->next != NULL)
            {
               textCurY += textCursorH+textVSpace;
               curStr = curStr->next;
            }
         }
         else
         {
            tmp_y = textAbsY;
            while (ABS_Y(y_off)>=tmp_y+textCursorH+textVSpace &&
                  curStr->next != NULL)
            {
               textCurY += textCursorH+textVSpace;
               tmp_y += textCursorH+textVSpace;
               curStr = curStr->next;
            }
         }

         curStrW = amount = XTextWidth (canvasFontPtr, curStr->dyn_str.s,
               curStr->dyn_str.sz-1);

         textCurIndex = 0;
         textCurX = textOrigX;

         switch (textJust)
         {
            case JUST_L: break;
            case JUST_C: textCurX -= (amount>>1); break;
            case JUST_R: textCurX -= amount; break;
         }
         left = textCurX;

         s[1] = '\0';
         c_ptr = curStr->dyn_str.s;
         if (pressed_in_same_text)
         {
            for ( ; *c_ptr != '\0'; c_ptr++)
            {
               s[0] = *c_ptr;
               amount = XTextWidth (canvasFontPtr, s, 1);

               if (double_clicked)
               {
                  if (x_off < textCurX+amount)
                     /* make sure it stops on the left side of the character */
                     break;
                  else
                     textCurX += amount;
               }
               else
               {
                  if (x_off < textCurX+(amount>>1))
                     /* stops on the right side of the clicked */
                     /*		character if the cursor is */
                     /*		closer to the right side */
                     break;
                  else
                     textCurX += amount;
               }
               textCurIndex++;
            }
         }
         else
         {
            tmp_x = textAbsX;
            switch (textJust)
            {
               case JUST_L: break;
               case JUST_C: tmp_x -= (amount>>1); break;
               case JUST_R: tmp_x -= amount; break;
            }

            for ( ; *c_ptr != '\0'; c_ptr++)
            {
               s[0] = *c_ptr;
               amount = XTextWidth (canvasFontPtr, s, 1);

               if (ABS_X(x_off)< tmp_x+(amount>>1))
                  break;
               else
               {
                  textCurX += amount;
                  tmp_x += amount;
               }
               textCurIndex++;
            }
         }

         attr_ptr = obj_ptr->detail.t->attr;
         if (attr_ptr != NULL && attr_ptr->inherited && textCurY == textOrigY &&
               attr_ptr->shown && attr_ptr->nameshown)
         {
            inherited_attr_left_index = attr_ptr->attr_name.sz-1;
            if (textCurIndex < inherited_attr_left_index)
            {  /* clicked in the name of an inherited attribute */
               textCurIndex = attr_ptr->attr_name.sz-1;
               textCurX = left + XTextWidth (canvasFontPtr,
                     attr_ptr->attr_name.s, textCurIndex);
            }
         }
         textCursorShown = TRUE;
         textHighlight = FALSE;

         if (double_clicked)
         {
            char	cur_char=curStr->dyn_str.s[textCurIndex];
            int		right_index=curStr->dyn_str.sz-1;

            highlightStartStr = highlightEndStr = endStr = curStr;
            highlightStartY = textEndY = textCurY;
            endStrW = curStrW;
            textEndIndex = textCurIndex;

            if (PartOfAWord (cur_char))
            {
               saved_end_index = textCurIndex;

               if (inherited_attr_left_index != (-1))
               {
                  while (textCurIndex > inherited_attr_left_index)
                     if (PartOfAWord (curStr->dyn_str.s[textCurIndex-1]))
                        textCurIndex--;
                     else
                        break;
               }
               else
               {
                  while (textCurIndex > 0)
                     if (PartOfAWord (curStr->dyn_str.s[textCurIndex-1]))
                        textCurIndex--;
                     else
                        break;
               }
               SetTextCurX ();

               highlightStartIndex = highlightEndIndex = textCurIndex;

               textEndIndex = saved_end_index+1;
               while (textEndIndex < right_index)
                  if (PartOfAWord (curStr->dyn_str.s[textEndIndex]))
                     textEndIndex++;
                  else
                     break;

               highlightEndIndex = textEndIndex;
            }
            else if (cur_char != '\0')
            {
               saved_end_index = textCurIndex;

               if (inherited_attr_left_index != (-1))
               {
                  while (textCurIndex > inherited_attr_left_index)
                     if (curStr->dyn_str.s[textCurIndex-1] == cur_char)
                        textCurIndex--;
                     else
                        break;
               }
               else
               {
                  while (textCurIndex > 0)
                     if (curStr->dyn_str.s[textCurIndex-1] == cur_char)
                        textCurIndex--;
                     else
                        break;
               }
               SetTextCurX ();

               highlightStartIndex = highlightEndIndex = textCurIndex;

               textEndIndex = saved_end_index+1;
               while (textEndIndex < right_index)
                  if (curStr->dyn_str.s[textEndIndex] == cur_char)
                     textEndIndex++;
                  else
                     break;

               highlightEndIndex = textEndIndex;
            }
            textHighlight = !(endStr==curStr && textEndIndex==textCurIndex);
            RedrawCurText ();
            textJustClicked = FALSE;
            return;
         }
         RedrawCurText ();
         textJustClicked = TRUE;
         lastClickTime = click_time;

         XGrabPointer (mainDisplay, drawWindow, FALSE,
               PointerMotionMask | ButtonReleaseMask,
               GrabModeAsync, GrabModeAsync, None, textCursor, CurrentTime);

         saved_end_index = textEndIndex = textCurIndex;
         saved_end_str = endStr = curStr;

         highlightStartStr = highlightEndStr = curStr;
         highlightStartIndex = highlightEndIndex = textCurIndex;
         highlightStartY = textCurY;

         while (!done)
         {
            XNextEvent (mainDisplay, &input);

            if (input.type == Expose || input.type == VisibilityNotify)
               ExposeEventHandler (&input, TRUE);
            else if (input.type == ButtonRelease)
            {
               XUngrabPointer (mainDisplay, CurrentTime);
               done = TRUE;
            }
            else if (input.type == MotionNotify)
            {
               x_off = input.xmotion.x;
               y_off = input.xmotion.y;

               obj_ptr = curTextObj;
               endStr = obj_ptr->detail.t->first;

               textEndY = textOrigY;
               while (y_off >= textEndY+textCursorH+textVSpace &&
                     endStr->next != NULL)
               {
                  textEndY += textCursorH+textVSpace;
                  endStr = endStr->next;
               }

               endStrW = amount = XTextWidth (canvasFontPtr, endStr->dyn_str.s,
                     endStr->dyn_str.sz-1);

               textEndX = textOrigX;
               switch (textJust)
               {
                  case JUST_L: break;
                  case JUST_C: textEndX -= (amount>>1); break;
                  case JUST_R: textEndX -= amount; break;
               }
               textEndIndex = 0;
               left = textEndX;

               if (y_off >= textOrigY-2)
               {
                  if (y_off > textEndY+textCursorH+textVSpace+2 &&
                        endStr->next == NULL)
                  {
                     textEndIndex = endStr->dyn_str.sz-1;
                     textEndX += endStrW;
                  }
                  else
                  {
                     s[1] = '\0';
                     c_ptr = endStr->dyn_str.s;
                     for ( ; *c_ptr != '\0'; c_ptr++)
                     {
                        s[0] = *c_ptr;
                        amount = XTextWidth (canvasFontPtr, s, 1);

                        if (x_off < textEndX+(amount>>1))
                           break;
                        else
                           textEndX += amount;
                        textEndIndex++;
                     }
                  }
               }

               attr_ptr = obj_ptr->detail.t->attr;
               if (attr_ptr != NULL && attr_ptr->inherited &&
                     textEndY == textOrigY &&
                     attr_ptr->shown && attr_ptr->nameshown &&
                     textEndIndex < attr_ptr->attr_name.sz-1)
               {
                  textEndIndex = attr_ptr->attr_name.sz-1;
                  textEndX = left + XTextWidth (canvasFontPtr,
                        attr_ptr->attr_name.s, textEndIndex);
               }
               textHighlight = !(endStr==curStr&&textEndIndex==textCurIndex);

               if (endStr!=saved_end_str || textEndIndex!=saved_end_index)
               {
                  XorText ();

                  if (textEndY == textCurY)
                  {
                     highlightStartStr = highlightEndStr = curStr;
                     highlightStartY = textCurY;
                     if (textEndIndex > textCurIndex)
                     {
                        highlightStartIndex = textCurIndex;
                        highlightEndIndex = textEndIndex;
                     }
                     else
                     {
                        highlightStartIndex = textEndIndex;
                        highlightEndIndex = textCurIndex;
                     }
                  }
                  else if (textEndY > textCurY)
                  {
                     highlightStartStr = curStr;
                     highlightStartIndex = textCurIndex;
                     highlightEndStr = endStr;
                     highlightEndIndex = textEndIndex;
                     highlightStartY = textCurY;
                  }
                  else
                  {
                     highlightStartStr = endStr;
                     highlightStartIndex = textEndIndex;
                     highlightEndStr = curStr;
                     highlightEndIndex = textCurIndex;
                     highlightStartY = textEndY;
                  }

                  saved_end_str = endStr;
                  saved_end_index = textEndIndex;
                  XorText ();
               }

               while (XCheckMaskEvent (mainDisplay,PointerMotionMask,&ev)) ;
            }
         }
      }
   }
   else if (Button_Ev->button == Button1 &&
         (Button_Ev->state & (ShiftMask|ControlMask)))
   {
      if (!textCursorShown) return;

      x_off = Button_Ev->x;
      y_off = Button_Ev->y;

      switch (textJust)
      {
         case JUST_L: x = textOrigX-2; break;
         case JUST_C: x = textOrigX-textW/2-2; break;
         case JUST_R: x = textOrigX-textW-2; break;
      }

      if (!(x_off >= x && x_off <= x+textW+2 &&
            y_off >= textOrigY-2 && y_off <= textOrigY+textH+2))
         return;

      obj_ptr = curTextObj;
      endStr = obj_ptr->detail.t->first;

      textEndY = textOrigY;
      while (y_off >= textEndY+textCursorH+textVSpace && endStr->next != NULL)
      {
         textEndY += textCursorH+textVSpace;
         endStr = endStr->next;
      }

      endStrW = amount = XTextWidth (canvasFontPtr, endStr->dyn_str.s,
            endStr->dyn_str.sz-1);

      textEndX = textOrigX;
      switch (textJust)
      {
         case JUST_L: break;
         case JUST_C: textEndX -= (amount>>1); break;
         case JUST_R: textEndX -= amount; break;
      }
      textEndIndex = 0;
      left = textEndX;

      s[1] = '\0';
      c_ptr = endStr->dyn_str.s;
      for ( ; *c_ptr != '\0'; c_ptr++)
      {
         s[0] = *c_ptr;
         amount = XTextWidth (canvasFontPtr, s, 1);

         if (x_off < textEndX+(amount>>1))
            break;
         else
            textEndX += amount;
         textEndIndex++;
      }

      attr_ptr = obj_ptr->detail.t->attr;
      if (attr_ptr != NULL && attr_ptr->inherited && textEndY == textOrigY &&
            attr_ptr->shown && attr_ptr->nameshown &&
            textEndIndex < attr_ptr->attr_name.sz-1)
      {
         textEndIndex = attr_ptr->attr_name.sz-1;
         textEndX = left + XTextWidth (canvasFontPtr, attr_ptr->attr_name.s,
               textEndIndex);
      }
      textHighlight = !(endStr == curStr && textEndIndex == textCurIndex);
      RedrawCurText ();
   }
}

static
int NumLines ()
{
   register struct StrRec	* str_ptr;
   register int			i = 0;

   for (str_ptr = firstStr; str_ptr != NULL; str_ptr = str_ptr->next, i++) ;
   return (i);
}

static
void JustDeleteHighlightedText (need_to_calc_extra_info)
   int	* need_to_calc_extra_info;
{
   struct StrRec	* s_ptr, * s_ptr1, * new_cur_str = NULL;
   int			highlighting = FALSE, new_cur_index = 0;
   int			second_index, len;
   int			i, y, new_cur_y = 0, len1, len2;

   if (!textHighlight) return;

   y = textOrigY;
   for (s_ptr = firstStr; s_ptr != NULL; y += textCursorH+textVSpace)
   {
      if (highlighting)
      {  /* started deleting already */
         if (s_ptr == curStr || s_ptr == endStr)
         {
            char	buf[MAXSTRING+1], msg[80];

            second_index = (s_ptr == curStr) ? textCurIndex : textEndIndex;
            len1 = strlen (new_cur_str->dyn_str.s);
            len2 = strlen (&(s_ptr->dyn_str.s[second_index]));
            if (len1+len2 >= MAXSTRING)
            {
               sprintf (msg, "String length exceeds %1d.  %s.",
                     MAXSTRING, "String truncated");
               Msg (msg);
               s_ptr->dyn_str.s[MAXSTRING-len1+second_index] = '\0';
               s_ptr->dyn_str.sz = MAXSTRING-len1+second_index+1;
            }
            sprintf (buf, "%s%s", new_cur_str->dyn_str.s,
                  &(s_ptr->dyn_str.s[second_index]));
            DynStrSet (&new_cur_str->dyn_str, buf);
            if (s_ptr == lastStr)
               lastStr = s_ptr->prev;
            else
               s_ptr->next->prev = s_ptr->prev;
            s_ptr->prev->next = s_ptr->next;
            FreeStr (s_ptr);
            break;
         }
         else
         {  /* delete the whole line */
            s_ptr1 = s_ptr->next;
            s_ptr1->prev = s_ptr->prev;
            FreeStr (s_ptr);
            s_ptr = s_ptr1;
         }
      }
      else
      {  /* looking for the beginning ... */
         if (s_ptr == curStr && s_ptr == endStr)
         {  /* the whole string to be deleted is within s_ptr */
            char	* s;

            new_cur_str = s_ptr;
            new_cur_index = min(textCurIndex,textEndIndex);
            new_cur_y = y;
            second_index = max(textCurIndex,textEndIndex);
            if ((new_cur_index==0 || new_cur_index==new_cur_str->dyn_str.sz-1 ||
                  second_index==0 || second_index==new_cur_str->dyn_str.sz-1) &&
                  need_to_calc_extra_info != NULL)
               *need_to_calc_extra_info = TRUE;
            s = new_cur_str->dyn_str.s;
            len = strlen (&(s[second_index]));
            for (i = 0; i <= len; i++) s[new_cur_index+i] = s[second_index+i];
            new_cur_str->dyn_str.sz = strlen(s)+1;
            break;
         }
         else if (s_ptr == curStr || s_ptr == endStr)
         {  /* found the beginning */
            if (need_to_calc_extra_info != NULL)
               *need_to_calc_extra_info = TRUE;
            new_cur_str = s_ptr;
            new_cur_index = (s_ptr == curStr) ? textCurIndex : textEndIndex;
            new_cur_y = y;
            s_ptr->dyn_str.s[new_cur_index] = '\0';
            highlighting = TRUE;
            s_ptr = s_ptr->next;
         }
         else
         {  /* still looking */
            s_ptr = s_ptr->next;
         }
      }
   }
   textHighlight = FALSE;
   curStr = new_cur_str;
   textCurIndex = new_cur_index;
   curStrW = XTextWidth (canvasFontPtr, curStr->dyn_str.s,
         curStr->dyn_str.sz-1);
   if (curStrW > textW) textW = curStrW;
   textCurY = new_cur_y;
   SetTextCurX ();
}

static
void GetCurStrSizeInfo ()
{
   register struct StrRec	* str_ptr;
   int				i, max_len=0, min_lbearing=0, max_rextra=0;

   curTextObj->detail.t->lines = 0;
   for (str_ptr=firstStr; str_ptr!=NULL;
         str_ptr=str_ptr->next, curTextObj->detail.t->lines++)
   {
      int	w=0, lbearing=0, rextra=0;

      GetStrSizeInfo (str_ptr, &w, &lbearing, &rextra);
      if (w > max_len) max_len = w;
      if (lbearing < min_lbearing) min_lbearing = lbearing;
      if (rextra > max_rextra) max_rextra = rextra;
      curTextObj->detail.t->lines++;
   }
   i = curTextObj->detail.t->lines;
   SetTextBBox (curTextObj, textJust, max_len,
         i*textCursorH+(i-1)*textVSpace, min_lbearing, max_rextra, curRotate);
}

static
void SetCurStrExtraInfo ()
{
   register struct StrRec	* str_ptr;

   textAbsMinLBearing = 0;
   textAbsMaxRExtra = 0;
   for (str_ptr=firstStr; str_ptr!=NULL; str_ptr=str_ptr->next)
   {
      int	lbearing=0, rextra=0;

      GetStrSizeInfo (str_ptr, NULL, &lbearing, &rextra);
      if (lbearing < textAbsMinLBearing) textAbsMinLBearing = lbearing;
      if (rextra > textAbsMaxRExtra) textAbsMaxRExtra = rextra;
   }
}

static
void HandleCRLF (ColorIndex)
   int	ColorIndex;
{
   register int		i, y;
   struct StrRec	* str_ptr;
   int			need_redraw = textHighlight;

   escPressed = FALSE;

   curTextModified = TRUE;
   if (textHighlight) JustDeleteHighlightedText (NULL);

   y = textCurY;
   for (str_ptr = curStr; str_ptr != NULL; str_ptr = str_ptr->next)
   {
      PaintCurText (drawWindow, drawGC, str_ptr->dyn_str.s, textJust, textOrigX,
            y, canvasFontPtr, ColorIndex, penPat, ERASE, INVALID, INVALID);
      y += textCursorH+textVSpace;
   }

   str_ptr = NewStr ();
   if (str_ptr == NULL) FailAllocMessage ();
   DynStrSet (&str_ptr->dyn_str, &curStr->dyn_str.s[textCurIndex]);
   curStr->dyn_str.s[textCurIndex] = '\0';
   curStr->dyn_str.sz = textCurIndex+1;
   AddStr (curStr, curStr->next, str_ptr);
   curStr = str_ptr;
   textCurY += textCursorH+textVSpace;
   textCurIndex = 0;
   GetStrSizeInfo (curStr, &curStrW, NULL, NULL);
   SetTextCurX ();
   i = textCursorH*NumLines () + textVSpace*(NumLines ()-1);
   SetCurStrExtraInfo ();

   if (i > textH)
   {
      textH = i;
      RedrawCurText ();
   }
   else if (need_redraw)
      RedrawCurText ();
   else
   {
      y = textCurY - textCursorH - textVSpace;
      for (str_ptr = curStr->prev; str_ptr != NULL; str_ptr = str_ptr->next)
      {
         PaintCurText (drawWindow, drawGC, str_ptr->dyn_str.s, textJust,
               textOrigX, y, canvasFontPtr, ColorIndex, penPat, PAINT_NORM,
               INVALID, INVALID);
         y += textCursorH+textVSpace;
      }
   }

   if (curTextObj->detail.t->cached_bitmap != None)
      XFreePixmap (mainDisplay, curTextObj->detail.t->cached_bitmap);

   curTextObj->detail.t->cached_zoom = 0;
   curTextObj->detail.t->cached_bitmap = None;

   if (zoomScale != 0 || curTextObj->detail.t->rotate != ROTATE0)
      GetCurStrSizeInfo ();

   ScrollTo (textCurX, textCurY);
}

static
void HandleBS (ColorIndex)
   int	ColorIndex;
{
   register int		i, y;
   register char	* s;
   struct StrRec	* str_ptr;
   struct AttrRec       * attr_ptr;
   int			len1, len2, need_to_calc_extra_info=FALSE;
   char			msg[80];

   escPressed = FALSE;

   if (textHighlight)
   {
      curTextModified = TRUE;
      JustDeleteHighlightedText (&need_to_calc_extra_info);
      if (need_to_calc_extra_info) SetCurStrExtraInfo ();
      RedrawCurText ();

      if (curTextObj->detail.t->cached_bitmap != None)
         XFreePixmap (mainDisplay, curTextObj->detail.t->cached_bitmap);

      curTextObj->detail.t->cached_zoom = 0;
      curTextObj->detail.t->cached_bitmap = None;

      if (zoomScale != 0 || curTextObj->detail.t->rotate != ROTATE0)
         GetCurStrSizeInfo ();

      ScrollTo (textCurX, textCurY);
      return;
   }

   attr_ptr = curTextObj->detail.t->attr;
   if (curStr == firstStr && attr_ptr != NULL && attr_ptr->inherited &&
         attr_ptr->nameshown && textCurIndex == attr_ptr->attr_name.sz-1)
      return;

   curTextModified = TRUE;
   if (textCurIndex != 0)
   {
      PaintCurText (drawWindow, drawGC, curStr->dyn_str.s, textJust,
            textOrigX, textCurY, canvasFontPtr, ColorIndex, penPat, ERASE,
            INVALID, INVALID);

      s = curStr->dyn_str.s;
      for (i = textCurIndex; i <= (curStr->dyn_str.sz-1); i++) s[i-1] = s[i];
      textCurIndex--;
      curStr->dyn_str.sz--;

      curStrW = XTextWidth (canvasFontPtr, curStr->dyn_str.s,
            curStr->dyn_str.sz-1);
      PaintCurText (drawWindow, drawGC, curStr->dyn_str.s, textJust, textOrigX,
            textCurY, canvasFontPtr, ColorIndex, penPat, PAINT_NORM,
            INVALID, INVALID);
      SetTextCurX ();
   }
   else
   {
      if (curStr->prev != NULL)
      {
         struct StrRec	* prev_str;
         char		buf[MAXSTRING+1];

         y = textCurY - textCursorH - textVSpace;
         for (str_ptr=curStr->prev; str_ptr!=NULL; str_ptr=str_ptr->next)
         {
            PaintCurText (drawWindow, drawGC, str_ptr->dyn_str.s, textJust,
                  textOrigX, y, canvasFontPtr, ColorIndex, penPat, ERASE,
                  INVALID, INVALID);
            y += textCursorH+textVSpace;
         }

         if (curStr->next == NULL)
            lastStr = curStr->prev;
         else
            curStr->next->prev = curStr->prev;
         curStr->prev->next = curStr->next;
         textCurIndex = curStr->prev->dyn_str.sz-1;
         len1 = curStr->prev->dyn_str.sz-1;
         len2 = curStr->dyn_str.sz-1;
         if (len1+len2 >= MAXSTRING)
         {
            sprintf (msg, "String length exceeds %1d.  %s.",
                  MAXSTRING, "String truncated");
            Msg (msg);
            curStr->dyn_str.s[MAXSTRING-len1] = '\0';
            curStr->dyn_str.sz = MAXSTRING-len1+1;
         }
         sprintf (buf, "%s%s", curStr->prev->dyn_str.s, curStr->dyn_str.s);
         DynStrSet (&curStr->prev->dyn_str, buf);
         prev_str = curStr->prev;
         FreeStr (curStr);
         curStr = prev_str;
         curStrW = XTextWidth (canvasFontPtr, curStr->dyn_str.s,
               curStr->dyn_str.sz-1);
         textCurY -= textCursorH+textVSpace;
         SetTextCurX ();

         y = textCurY;
         for (str_ptr = curStr; str_ptr != NULL; str_ptr = str_ptr->next)
         {
            PaintCurText (drawWindow, drawGC, str_ptr->dyn_str.s, textJust,
                  textOrigX, y, canvasFontPtr, ColorIndex, penPat,
                  PAINT_NORM, INVALID, INVALID);
            y += textCursorH+textVSpace;
         }
         SetCurStrExtraInfo ();
         if (curStrW > textW)
         {
            textW = curStrW;
            RedrawCurText ();
         }
      }
   }

   if (curTextObj->detail.t->cached_bitmap != None)
      XFreePixmap (mainDisplay, curTextObj->detail.t->cached_bitmap);

   curTextObj->detail.t->cached_zoom = 0;
   curTextObj->detail.t->cached_bitmap = None;

   if (zoomScale != 0 || curTextObj->detail.t->rotate != ROTATE0)
      GetCurStrSizeInfo ();

   ScrollTo (textCurX, textCurY);
}

static
void HandleChar (Str, ColorIndex)
   char	* Str;
   int	ColorIndex;
{
   register int		amount;
   int			need_redraw=textHighlight;
   int			need_to_calc_extra_info=FALSE;
   char			msg[80], buf[MAXSTRING+1], tmp_char;
   XEvent		ev;

   if (escPressed)
   {
      Str[0] |= 0x80;
      escPressed = FALSE;
   }

   if (((*Str)&0x80) && curFont != FONT_SYM && !ValidCharCode (Str)) return;

   if (textHighlight)
   {
      curTextModified = TRUE;
      JustDeleteHighlightedText (&need_to_calc_extra_info);
   }

   if (textCurIndex+((int) strlen (&(curStr->dyn_str.s[textCurIndex]))) >=
         MAXSTRING)
   {
      sprintf (msg, "String length exceeds %1d.  Character ignored.",MAXSTRING);
      Msg (msg);
      RedrawCurText ();
      while (XCheckWindowEvent (mainDisplay, drawWindow, KeyPressMask, &ev)) ;
      return;
   }

   curTextModified = TRUE;
   amount = XTextWidth (canvasFontPtr, Str, 1);
 
   if (textJust != JUST_L || textCurIndex != curStr->dyn_str.sz-1)
      PaintCurText (drawWindow, drawGC, curStr->dyn_str.s, textJust, textOrigX,
            textCurY, canvasFontPtr, ColorIndex, penPat, ERASE,
            INVALID, INVALID);

   if (!need_to_calc_extra_info && (textCurIndex==0 ||
         textCurIndex==curStr->dyn_str.sz-1))
      need_to_calc_extra_info = TRUE;
   tmp_char = curStr->dyn_str.s[textCurIndex];
   curStr->dyn_str.s[textCurIndex] = '\0';
   sprintf (buf, "%s%c%c%s", curStr->dyn_str.s, *Str, tmp_char,
         &curStr->dyn_str.s[++textCurIndex]);
   DynStrSet (&curStr->dyn_str, buf);

   curStrW += amount;
   if (!need_redraw)
      PaintCurText (drawWindow, drawGC, curStr->dyn_str.s, textJust, textOrigX,
            textCurY, canvasFontPtr, ColorIndex, penPat, PAINT_NORM,
            INVALID, INVALID);
   SetTextCurX ();
   if (need_to_calc_extra_info) SetCurStrExtraInfo ();
   if (curStrW > textW)
   {
      textW = curStrW;
      RedrawCurText ();
   }
   else if (need_redraw)
      RedrawCurText ();

   if (curTextObj->detail.t->cached_bitmap != None)
      XFreePixmap (mainDisplay, curTextObj->detail.t->cached_bitmap);

   curTextObj->detail.t->cached_zoom = 0;
   curTextObj->detail.t->cached_bitmap = None;

   if (zoomScale != 0 || curTextObj->detail.t->rotate != ROTATE0)
      GetCurStrSizeInfo ();

   ScrollTo (textCurX, textCurY);
}

static
void HandleTAB (key_ev, ColorIndex)
   XKeyEvent	* key_ev;
   int		ColorIndex;
{
   struct AttrRec	* attr_ptr, * new_attr;
   struct ObjRec	* obj_ptr;
   int			abs_x, abs_y, x_off, y_off, left, amount;

   escPressed = FALSE;

   if ((attr_ptr = curTextObj->detail.t->attr) == NULL)
   {
      Msg ("Can not TAB out of a non-attribute text.");
      return;
   }
   CreateTextObj ();
   curTextModified = FALSE;

   if (key_ev->state & Mod1Mask)
   {  /* new_attr will be the next attribute */
      if (attr_ptr->next == NULL)
         new_attr = attr_ptr->owner->fattr;
      else
         new_attr = attr_ptr->next;
      while (!new_attr->shown)
      {
         if (new_attr->next == NULL)
            new_attr = new_attr->owner->fattr;
         else
            new_attr = new_attr->next;
      }
   }
   else
   {  /* new_attr will be the previous attribute */
      if (attr_ptr->prev == NULL)
         new_attr = attr_ptr->owner->lattr;
      else
         new_attr = attr_ptr->prev;
      while (!new_attr->shown)
      {
         if (new_attr->prev == NULL)
            new_attr = new_attr->owner->lattr;
         else
            new_attr = new_attr->prev;
      }
   }
   obj_ptr = new_attr->obj;
   abs_x = obj_ptr->x; x_off = OFFSET_X(abs_x);
   abs_y = obj_ptr->y; y_off = OFFSET_Y(abs_y);

   if (!PrepareEditExistingText (obj_ptr, abs_x, abs_y, &x_off, &y_off))
      return;

   textCurIndex = 0;
   textCurY = textOrigY;
   textCurX = textOrigX;
   curStrW = amount = XTextWidth (canvasFontPtr, curStr->dyn_str.s,
         curStr->dyn_str.sz-1);

   switch (textJust)
   {
      case JUST_L: break;
      case JUST_C: textCurX -= (amount>>1); break;
      case JUST_R: textCurX -= amount; break;
   }
   left = textCurX;

   attr_ptr = obj_ptr->detail.t->attr;
   if (attr_ptr != NULL && attr_ptr->inherited && textCurY == textOrigY &&
         attr_ptr->shown && attr_ptr->nameshown &&
         textCurIndex < attr_ptr->attr_name.sz-1)
   {  /* clicked in the name of an inherited attribute */
      textCurIndex = attr_ptr->attr_name.sz-1;
      textCurX = left + XTextWidth (canvasFontPtr, attr_ptr->attr_name.s,
            textCurIndex);
   }
   textCursorShown = TRUE;
   textHighlight = FALSE;

   textEndIndex = textCurIndex;

   highlightStartStr = endStr;
   highlightStartIndex = textEndIndex;
   highlightEndStr = curStr;
   highlightEndIndex = textCurIndex;
   highlightStartY = textEndY;

   textEndY = textOrigY;
   endStr = obj_ptr->detail.t->first;
   while (endStr->next != NULL)
   {
      textEndY += textCursorH+textVSpace;
      endStr = endStr->next;
   }

   endStrW = amount = XTextWidth (canvasFontPtr, endStr->dyn_str.s,
         endStr->dyn_str.sz-1);
   left = textEndX = textOrigX;
   switch (textJust)
   {
      case JUST_L: break;
      case JUST_C: textEndX -= (amount>>1); break;
      case JUST_R: textEndX -= amount; break;
   }
   textEndIndex = endStr->dyn_str.sz-1;
   textEndX += endStrW;

   attr_ptr = obj_ptr->detail.t->attr;
   if (attr_ptr != NULL && attr_ptr->inherited && textEndY == textOrigY &&
         attr_ptr->shown && attr_ptr->nameshown &&
         textEndIndex < attr_ptr->attr_name.sz-1)
   {
      textEndIndex = attr_ptr->attr_name.sz-1;
      textEndX = left + XTextWidth (canvasFontPtr,
            attr_ptr->attr_name.s, textEndIndex);
   }
   textHighlight = !(endStr==curStr && textEndIndex==textCurIndex);

   RedrawCurText ();
   ScrollTo (textCurX, textCurY);
}

static
void HandleLeft ()
{
   struct AttrRec       * attr_ptr;

   escPressed = FALSE;

   attr_ptr = curTextObj->detail.t->attr;
   if (curStr == firstStr && attr_ptr != NULL && attr_ptr->inherited &&
         attr_ptr->nameshown && textCurIndex == attr_ptr->attr_name.sz-1)
   {
      if (textHighlight)
      {
         textHighlight = FALSE;
         RedrawCurText ();
      }
      return;
   }
   if (textCurIndex != 0)
   {
      textCurIndex--;
      SetTextCurX ();
   }
   else
   {
      if (curStr->prev != NULL)
      {
         textCurIndex = curStr->prev->dyn_str.sz-1;
         curStr = curStr->prev;
         curStrW = XTextWidth (canvasFontPtr, curStr->dyn_str.s,
               curStr->dyn_str.sz-1);
         textCurY -= textCursorH+textVSpace;
         SetTextCurX ();
      }
      else if (!textHighlight)
         return;
   }
   textHighlight = FALSE;
   RedrawCurText ();
   ScrollTo (textCurX, textCurY);
}

static
void HandleUp ()
{
   int			x_off, left, amount;
   char			* c_ptr, s[2];
   struct AttrRec       * attr_ptr;

   escPressed = FALSE;
   if (curStr == firstStr)
   {
      if (!textHighlight) return;
      textHighlight = FALSE;
      RedrawCurText ();
      return;
   }

   x_off = textCurX;
   curStr = curStr->prev;
   curStrW = XTextWidth (canvasFontPtr, curStr->dyn_str.s,
         curStr->dyn_str.sz-1);
   textCurY -= textCursorH+textVSpace;

   textCurX = textOrigX;
   switch (textJust)
   {
      case JUST_L: break;
      case JUST_C: textCurX -= (curStrW/2); break;
      case JUST_R: textCurX -= curStrW; break;
   }
   textCurIndex = 0;
   left = textCurX;

   s[1] = '\0';
   for (c_ptr = curStr->dyn_str.s; *c_ptr != '\0'; c_ptr++)
   {
      s[0] = *c_ptr;
      amount = XTextWidth (canvasFontPtr, s, 1);

      if (x_off < textCurX+(amount>>1))
         break;
      else
         textCurX += amount;
      textCurIndex++;
   }

   attr_ptr = curTextObj->detail.t->attr;
   if (curStr == firstStr && attr_ptr != NULL && attr_ptr->inherited &&
         attr_ptr->nameshown && textCurIndex < attr_ptr->attr_name.sz-1)
   {
      textCurIndex = attr_ptr->attr_name.sz-1;
      textCurX = left + XTextWidth (canvasFontPtr, attr_ptr->attr_name.s,
            textCurIndex);
   }
   textHighlight = FALSE;
   RedrawCurText ();
   ScrollTo (textCurX, textCurY);
}
 
static
void HandleRight ()
{
   int	len;

   escPressed = FALSE;

   len = curStr->dyn_str.sz-1;
   if (textCurIndex != len)
   {
      textCurIndex++;
      SetTextCurX ();
   }
   else
   {
      if (curStr->next != NULL)
      {
         textCurIndex = 0;
         curStr = curStr->next;
         curStrW = XTextWidth (canvasFontPtr, curStr->dyn_str.s,
               curStr->dyn_str.sz-1);
         textCurY += textCursorH+textVSpace;
         SetTextCurX ();
      }
      else if (!textHighlight)
         return;
   }
   textHighlight = FALSE;
   RedrawCurText ();
   ScrollTo (textCurX, textCurY);
}
 
static
void HandleDown ()
{
   int			x_off, amount;
   char			* c_ptr, s[2];

   escPressed = FALSE;
   if (curStr == lastStr)
   {
      if (!textHighlight) return;
      textHighlight = FALSE;
      RedrawCurText ();
      return;
   }

   x_off = textCurX;
   curStr = curStr->next;
   curStrW = XTextWidth (canvasFontPtr, curStr->dyn_str.s,
         curStr->dyn_str.sz-1);
   textCurY += textCursorH+textVSpace;

   textCurX = textOrigX;
   switch (textJust)
   {
      case JUST_L: break;
      case JUST_C: textCurX -= (curStrW/2); break;
      case JUST_R: textCurX -= curStrW; break;
   }
   textCurIndex = 0;

   s[1] = '\0';
   for (c_ptr = curStr->dyn_str.s; *c_ptr != '\0'; c_ptr++)
   {
      s[0] = *c_ptr;
      amount = XTextWidth (canvasFontPtr, s, 1);

      if (x_off < textCurX+(amount>>1))
         break;
      else
         textCurX += amount;
      textCurIndex++;
   }
   textHighlight = FALSE;
   RedrawCurText ();
   ScrollTo (textCurX, textCurY);
}

static XComposeStatus	c_stat;

void DrawText (input)
   XEvent	* input;
{
   register char	* c_ptr;
   char			s[80], * cut_buffer=NULL;
   int			xfree_cut_buffer=FALSE, have_ch;
   XKeyEvent		* key_ev;
   KeySym		key_sym;

   if (input->type == ButtonPress)
      HandleButton (&(input->xbutton));
   else if (input->type == KeyPress)
   {
      if (!textCursorShown) return;

      if (pasteInDrawTextMode)
      {
         int	len;

         pasteInDrawTextMode = FALSE;
         if (pasteFromFileInDrawTextMode)
         {
            FILE	* fp;
            char	msg[MAXSTRING+1], inbuf[MAXSTRING+1];
            int		size=0;

            pasteFromFileInDrawTextMode = FALSE;
            if ((fp = fopen (pasteFromFileName, "r")) == NULL)
            {
               sprintf (msg, "Can not open '%s' for read.", pasteFromFileName);
               MsgBox (msg, TOOL_NAME, INFO_MB);
               return;
            }
            while (fgets (inbuf, MAXSTRING, fp) != NULL) size += strlen (inbuf);
            fclose (fp);
            if (size == 0)
            {
               sprintf (msg, "File '%s' is empty.", pasteFromFileName);
               MsgBox (msg, TOOL_NAME, INFO_MB);
               return;
            }
            cut_buffer = (char*)malloc((size+2)*sizeof(char));
            if (cut_buffer == NULL) {
               FailAllocMessage();
               return;
            }
            if ((fp=fopen(pasteFromFileName, "r")) == NULL) {
               sprintf(msg, "Can not open '%s' for read.", pasteFromFileName);
               MsgBox(msg, TOOL_NAME, INFO_MB);
               free(cut_buffer);
               return;
            }
            len = 0;
            while (fgets (&cut_buffer[len], MAXSTRING, fp) != NULL)
               len += strlen (&cut_buffer[len]);
            fclose (fp);
         }
         else
         {
            cut_buffer = (char *) FetchCutBuffer (&len);
            if (len == 0) { Msg ("Cut buffer is empty."); return; }
            xfree_cut_buffer = TRUE;
         }

         if (escPressed)
         {
            escPressed = FALSE;
            Msg ("An <ESC> key press is ignored.");
         }
         s[1] = '\0';
         EraseTextCursor ();
         for (c_ptr = cut_buffer; *c_ptr != '\0'; c_ptr++)
         {
            s[0] = *c_ptr;
            switch (s[0])
            {
               case '\r':
               case '\n': HandleCRLF (colorIndex); break;

               case '\177': Msg ("Can not paste <DEL>."); break;
               case '\b': Msg ("Can not paste <BS>."); break;
               case '\033': Msg ("Can not paste <ESC>."); break;
               case '\t': Msg ("Can not paste <TAB>."); break;

               default: HandleChar (s, colorIndex); break;
            }
         }
         PutTextCursor ();
         MarkRulers (textCurX, textCurY);
         SetFileModified (TRUE);
         if (xfree_cut_buffer)
            XFree (cut_buffer);
         else
            free(cut_buffer);
         return;
      }
      else if (copyInDrawTextMode)
      {
         int		copy_failed=FALSE, highlighting=FALSE;
         int		first_index=0, second_index;
         int		cut_buffer_size=0;
         struct StrRec	* s_ptr, * tmp_s_ptr, * first_str=NULL;
         char		msg[80];

         copyInDrawTextMode = FALSE;

         if (!textHighlight) return;

         if (escPressed)
         {
            escPressed = FALSE;
            Msg ("An <ESC> key press is ignored.");
         }
         for (s_ptr = firstStr; s_ptr != NULL; )
         {
            if (highlighting)
            {
               if (s_ptr == curStr || s_ptr == endStr)
               {
                  second_index = (s_ptr==curStr) ? textCurIndex : textEndIndex;
                  cut_buffer_size += second_index+1;
                  cut_buffer = (char*)malloc((cut_buffer_size+1)*sizeof(char));
                  if (cut_buffer == NULL) FailAllocMessage();

                  c_ptr = cut_buffer;
                  strcpy (cut_buffer, &first_str->dyn_str.s[first_index]);
                  c_ptr += strlen(&first_str->dyn_str.s[first_index]);
                  *c_ptr++ = '\n';
                  for (tmp_s_ptr = first_str->next; TRUE;
                        tmp_s_ptr = tmp_s_ptr->next)
                  {
                     if (tmp_s_ptr == s_ptr)
                     {
                        strncpy (c_ptr, s_ptr->dyn_str.s, second_index);
                        c_ptr[second_index] = '\0';
                        break;
                     }
                     else
                     {
                        strcpy (c_ptr, tmp_s_ptr->dyn_str.s);
                        c_ptr += tmp_s_ptr->dyn_str.sz-1;
                        *c_ptr++ = '\n';
                     }
                  }
                  break;
               }
               else
               {  /* include the whole line */
                  cut_buffer_size += s_ptr->dyn_str.sz;
                  s_ptr = s_ptr->next;
               }
            }
            else
            {  /* looking for the beginning ... */
               if (s_ptr == curStr && s_ptr == endStr)
               {  /* the whole string to be copied is within s_ptr */
                  if (textCurIndex == textEndIndex) return;

                  first_index = min(textCurIndex,textEndIndex);
                  second_index = max(textCurIndex,textEndIndex);
                  cut_buffer_size = second_index - first_index;
                  cut_buffer = (char*)malloc((cut_buffer_size+1)*sizeof(char));
                  if (cut_buffer == NULL) FailAllocMessage();
                  strncpy (cut_buffer, &s_ptr->dyn_str.s[first_index],
                        cut_buffer_size);
                  cut_buffer[cut_buffer_size] = '\0';
                  break;
               }
               else if (s_ptr == curStr || s_ptr == endStr)
               {  /* found the beginning */
                  first_str = s_ptr;
                  first_index = (s_ptr==curStr) ? textCurIndex : textEndIndex;
                  cut_buffer_size = strlen(&s_ptr->dyn_str.s[first_index]);
                  highlighting = TRUE;
                  s_ptr = s_ptr->next;
               }
               else
               {  /* still looking */
                  s_ptr = s_ptr->next;
               }
            }
         }
         copyingToCutBuffer = TRUE;
         XStoreBytes (mainDisplay, cut_buffer, cut_buffer_size);
         XSync (mainDisplay, False);
         if (copyingToCutBuffer == INVALID)
         {
            sprintf (msg, "%s  %s", "Copy to cut buffer fails.",
                  "Selected string may be too long.");
            copy_failed = TRUE;
         }
         else
            sprintf (msg, "Copy buffer updated.");
         copyingToCutBuffer = FALSE;
         Msg (msg);
         if (copy_failed)
         {
            *cut_buffer = '\0';
            XStoreBytes (mainDisplay, cut_buffer, 1);
         }
         free(cut_buffer);
         return;
      }

      key_ev = &(input->xkey);
      have_ch = XLookupString (key_ev, s, 80-1, &key_sym, &c_stat);
      TranslateKeys (s, &key_sym);

      if (!((s[0]=='\r' && (key_sym & 0xff)=='\r') ||
            (s[0]=='\n' && (key_sym & 0xff)=='\n') ||
            (s[0]=='\b' && (key_sym & 0xff)=='\b') ||
            (s[0]=='\b' && (key_sym & 0xff)=='h' &&
            (key_ev->state & ControlMask)) ||
            key_sym==XK_Left || key_sym==XK_Up ||
            key_sym==XK_Right || key_sym==XK_Down ||
            key_sym==XK_KP_Left || key_sym==XK_KP_Up ||
            key_sym==XK_KP_Right || key_sym==XK_KP_Down ||
            (key_sym==XK_Tab && (key_ev->state & (ShiftMask | Mod1Mask))) ||
            (s[0]=='\033' && (key_sym & 0xff)=='\033') ||
            (s[0]=='\177' && (key_sym & 0x7f)=='\177') ||
            ((s[0]&0xff) && key_sym>='\040' && key_sym<='\177') ||
            ((s[0]&0xff) && key_sym>0xa0 && key_sym<=0xff)))
         return;

      EraseTextCursor ();
      switch (key_sym)
      {
         case XK_Left: HandleLeft (); break;
         case XK_KP_Left: HandleLeft (); break;
         case XK_Up: HandleUp (); break;
         case XK_KP_Up: HandleUp (); break;
         case XK_Right: HandleRight (); break;
         case XK_KP_Right: HandleRight (); break;
         case XK_Down: HandleDown (); break;
         case XK_KP_Down: HandleDown (); break;
         default:
            s[1] = '\0';
            switch (s[0])
            {
               case '\r':
               case '\n': HandleCRLF (colorIndex); break;

               case '\177': /* <DEL> */
                  if (escPressed)
                     HandleChar (s, colorIndex);
                  else
                     HandleBS (colorIndex);
                  break;

               case '\b': HandleBS (colorIndex); break;

               case '\033': /* <ESC> */
                  if (!escPressed) escPressed = TRUE;
                  break;

               case '\t': HandleTAB (key_ev, colorIndex); break;

               default: if (have_ch) HandleChar (s, colorIndex); break;
            }
            break;
      }
      if (textCursorShown)
      {
         PutTextCursor ();
         MarkRulers (textCurX, textCurY);
         SetFileModified (TRUE);
      }
   }
}

void EditTextInAttr(attr_ptr)
   struct AttrRec *attr_ptr;
{
   struct ObjRec *obj_ptr=attr_ptr->obj;
   int abs_x, abs_y, x_off=0, y_off=0, tmp_x, tmp_y, right_index, amount, left;
   int inherited_attr_left_index=0;
   char *c_ptr, buf[80], cur_char;

   SetCurChoice(DRAWTEXT);
   if (obj_ptr->ctm == NULL) {
      abs_x = obj_ptr->obbox.ltx;
      abs_y = obj_ptr->obbox.lty;
   } else {
      abs_x = ((obj_ptr->obbox.ltx+obj_ptr->obbox.rbx)>>1);
      abs_y = ((obj_ptr->obbox.lty+obj_ptr->obbox.rby)>>1);
   }
   if (FindTextObj(OFFSET_X(abs_x), OFFSET_Y(abs_y), obj_ptr) != obj_ptr) {
      return;
   }
   editingText = FALSE;
   curTextModified = FALSE;

   if (!PrepareEditExistingText(obj_ptr, abs_x, abs_y, &x_off, &y_off)) {
      return;
   }
   textCurY = textOrigY;
   curStrW = amount = XTextWidth(canvasFontPtr, curStr->dyn_str.s,
         curStr->dyn_str.sz-1);

   textCurIndex = 0;
   textCurX = textOrigX;

   switch (textJust) {
   case JUST_L: break;
   case JUST_C: textCurX -= (amount>>1); break;
   case JUST_R: textCurX -= amount; break;
   }
   left = textCurX;

   tmp_x = textAbsX;
   switch (textJust) {
   case JUST_L: break;
   case JUST_C: tmp_x -= (amount>>1); break;
   case JUST_R: tmp_x -= amount; break;
   }

   buf[1] = '\0';
   c_ptr = curStr->dyn_str.s;

   for ( ; *c_ptr != '\0'; c_ptr++) {
      buf[0] = *c_ptr;
      amount = XTextWidth(canvasFontPtr, buf, 1);
      if (ABS_X(x_off) < tmp_x+(amount>>1)) {
         break;
      } else {
         textCurX += amount;
         tmp_x += amount;
      }
      textCurIndex++;
   }

   if (attr_ptr->inherited && textCurY == textOrigY && attr_ptr->shown &&
         attr_ptr->nameshown) {
      inherited_attr_left_index = attr_ptr->attr_name.sz-1;
      if (textCurIndex < inherited_attr_left_index) {
         /* clicked in the name of an inherited attribute */
         textCurIndex = attr_ptr->attr_name.sz-1;
         textCurX = left + XTextWidth(canvasFontPtr,
               attr_ptr->attr_name.s, textCurIndex);
      }
   }
   textCursorShown = TRUE;
   textHighlight = FALSE;

   cur_char = curStr->dyn_str.s[textCurIndex];
   right_index = curStr->dyn_str.sz-1;

   highlightStartStr = curStr;
   highlightStartIndex = textCurIndex;
   highlightStartY = textEndY = textCurY;
   endStr = curStr;
   endStrW = curStrW;

   while (endStr->next != NULL) {
      textEndY += textCursorH+textVSpace;
      endStr = endStr->next;
   }
   highlightEndStr = endStr;
   endStrW = amount = XTextWidth(canvasFontPtr, endStr->dyn_str.s,
         endStr->dyn_str.sz-1);

   textEndX = textOrigX;
   switch (textJust) {
   case JUST_L: break;
   case JUST_C: textEndX -= (amount>>1); break;
   case JUST_R: textEndX -= amount; break;
   }
   textEndIndex = endStr->dyn_str.sz-1;;
   textEndX += endStrW;
   left = textEndX;
   highlightEndIndex = textEndIndex;

   textHighlight = !(endStr==curStr && textEndIndex==textCurIndex);
   RedrawCurText();
   textJustClicked = FALSE;
   return;
}

void FindText(spec)
   char *spec;
{
}

void FindTextAgain()
{
}

void DeleteHighLightedText ()
{
   if (!textCursorShown) return;

   EraseTextCursor ();
   HandleBS (colorIndex);
   if (textCursorShown)
   {
      PutTextCursor ();
      MarkRulers (textCurX, textCurY);
      SetFileModified (TRUE);
   }
}

void DumpOneStr (FP, FontIndex, Str)
   register FILE	* FP;
   int			FontIndex;
   register char	* Str;
{
   register char	* c_ptr;

   for ( ; *Str != '\0'; Str++)
   {
      switch (*Str)
      {
         case '(':
         case ')':
         case '\\': fprintf (FP, "\\"); break;
      }
      if ((*Str) & 0x80)
      {
         if (FontIndex != FONT_SYM && (c_ptr = CharCodeTranslate (Str)) != NULL)
         {
            if (*c_ptr == '\\')
               fprintf (FP, "%s", c_ptr);
            else if (*c_ptr == '8')
               fprintf (FP, "\\%c%c%c", c_ptr[2], c_ptr[3], c_ptr[4]);
         }
         else
            fprintf (FP, "\\%o", (*Str)&0xff);
      }
      else
         fprintf (FP, "%c", *Str);
   }
}

void DrawTextObj(Win, XOff, YOff, ObjPtr)
   Window Win;
   int XOff, YOff;
   struct ObjRec *ObjPtr;
{
   struct TextRec *text_ptr=ObjPtr->detail.t;
   struct StrRec *s_ptr;
   int x, y, xinc=0, yinc=0, fill, pen, draw_it=FALSE;
   XGCValues values;

   for (s_ptr=text_ptr->first; !draw_it && s_ptr!=NULL; s_ptr=s_ptr->next) {
      if (*s_ptr->dyn_str.s != '\0') {
         draw_it = TRUE;
      }
   }
   if (!draw_it) return;

   SaveCurFont();
   curFont = text_ptr->font;
   curStyle = text_ptr->style;
   curSize = text_ptr->size;
   textJust = text_ptr->just;
   textVSpace = text_ptr->v_space;
   curRotate = text_ptr->rotate;
   SetCanvasFont();

   pen = text_ptr->pen;

   if (NeedsToCacheTextObj(ObjPtr) && text_ptr->cached_bitmap == None) {
      MakeCachedTextBitmap(ObjPtr);
   }

   x = ObjPtr->x - XOff;
   y = ObjPtr->y - YOff;

   switch (curRotate) {
   case ROTATE0: xinc = 0; yinc = textCursorH+textVSpace; break;
   case ROTATE90: xinc = -(textCursorH+textVSpace); yinc = 0; break;
   case ROTATE180: xinc = 0; yinc = -(textCursorH+textVSpace); break;
   case ROTATE270: xinc = textCursorH+textVSpace; yinc = 0; break;
   }
   if (curRotate != ROTATE0) {
      fprintf(stderr, "DrawTextObj(): text object rotation is not 0!\n");
   }

   fill = text_ptr->fill;
   if (fill != NONEPAT) {
      int real_x_off, real_y_off, ltx, lty, rbx, rby;

      real_x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
      real_y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);
      ltx = ZOOMED_SIZE(ObjPtr->bbox.ltx + 1 - real_x_off);
      lty = ZOOMED_SIZE(ObjPtr->bbox.lty + 1 - real_y_off);
      rbx = ZOOMED_SIZE(ObjPtr->bbox.rbx - 1 - real_x_off);
      rby = ZOOMED_SIZE(ObjPtr->bbox.rby - 1 - real_y_off);

      values.foreground = (fill == BACKPAT) ? myBgPixel :
            colorPixels[ObjPtr->color];
      values.function = GXcopy;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[fill];
      XChangeGC(mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple, &values);
      if (ObjPtr->ctm != NULL) {
         XFillPolygon(mainDisplay, Win, drawGC, ObjPtr->rotated_obbox, 5,
               Convex, CoordModeOrigin);
      } else {
         XFillRectangle(mainDisplay, Win, drawGC, ltx, lty, rbx-ltx, rby-lty);
      }
   }

   if (ObjPtr->ctm!=NULL || zoomScale!=0 || curRotate!=ROTATE0 ||
         text_ptr->read_only) {
      if (ObjPtr->obbox.ltx!=ObjPtr->obbox.rbx &&
            ObjPtr->obbox.lty!=ObjPtr->obbox.rby) {
         int ltx, lty, w, h;

         ltx = OFFSET_X(ObjPtr->bbox.ltx+1);
         lty = OFFSET_Y(ObjPtr->bbox.lty+1);
         w = ZOOMED_SIZE(ObjPtr->bbox.rbx-ObjPtr->bbox.ltx-2);
         h = ZOOMED_SIZE(ObjPtr->bbox.rby-ObjPtr->bbox.lty-2);

         values.foreground = (pen == BACKPAT) ? myBgPixel :
               colorPixels[ObjPtr->color];
         values.function = GXcopy;
         values.fill_style = FillOpaqueStippled;
         values.stipple = patPixmap[pen];
         values.clip_mask = text_ptr->cached_bitmap;
         values.clip_x_origin = ltx;
         values.clip_y_origin = lty;
         XChangeGC(mainDisplay, drawGC,
               GCForeground | GCFunction | GCFillStyle | GCStipple |
               GCClipMask | GCClipXOrigin | GCClipYOrigin, &values);
         XFillRectangle(mainDisplay, Win, drawGC, ltx, lty, w, h);
         FillClippedRectangle(Win, drawGC, ltx, lty, w, h);

         values.clip_mask = None;
         values.clip_x_origin = 0;
         values.clip_y_origin = 0;
         XChangeGC(mainDisplay, drawGC,
               GCClipMask | GCClipXOrigin | GCClipYOrigin, &values);
         if (numClipRecs > 0) {
            XSetClipRectangles(mainDisplay, drawGC, 0, 0, clipRecs,
               numClipRecs, clipOrdering);
         }
      }
   } else {
      for (s_ptr = text_ptr->first; s_ptr != NULL; s_ptr = s_ptr->next) {
         PaintText(Win, s_ptr->dyn_str.s, textJust, curRotate, x, y,
               canvasFontPtr, ObjPtr->color, pen);
         x += xinc;
         y += yinc;
      }
   }
   RestoreCurFont();
}

#define FONTS_PER_DPI (((MAXFONTS-1)*MAXFONTSTYLES+1)*MAXFONTSIZES)

static
int OldFontIndex (dpi_index, font_index, size_index, style_index)
   register int dpi_index, font_index, size_index, style_index;
   /* obsoleted procedure, kept to remain compatible with old versions */
{
   if (font_index == FONT_SYM)
      return (size_index+MAXFONTSIZES*(MAXFONTSTYLES*font_index) +
            dpi_index*FONTS_PER_DPI);
   else
      return (size_index+MAXFONTSIZES*(style_index+MAXFONTSTYLES*font_index) +
            dpi_index*FONTS_PER_DPI);
}

#define GETVALUE(val,name) ScanValue("%d", &(val), name, "text")
#define GETSTRNG(val,name) ScanValue("%s", (val), name, "text")

void ReadTextObj (FP, Inbuf, ObjPtr)
   FILE			* FP;
   char			* Inbuf;
   struct ObjRec	* * ObjPtr;
{
   register int		i, max_len=0, len;
   struct StrRec	* s_ptr;
   struct TextRec	* text_ptr;
   char			color_str[80], * s, * c_ptr, font_str[80];
   char			* tmp_str, inbuf[MAXSTRING+1];
   char			custom_font_name[MAXSTRING+1];
   int			num_lines, x, y, font, style, size, new_alloc, id=0;
   int			text_just, rotate, pen, rotation;
   int			obbox_w, obbox_h, dpi, asc, des, fill, v_space;
   int			locked=FALSE, max_rextra=0;
   int			underline_on=FALSE, underline=2, min_lbearing=0;
   int			double_byte=FALSE, compressed=FALSE;
   int			transformed=FALSE, invisible=FALSE;
   int			direction=FontLeftToRight, real_x=0, real_y=0;
   struct XfrmMtrxRec	*ctm=NULL;
   struct BBRec		orig_obbox, orig_bbox;

   dpi = FONT_DPI_75;
   fill = NONEPAT;
   v_space = 0;
   *custom_font_name = '\0';

   *ObjPtr = NULL;

   s = FindChar ((int)'(', Inbuf);
   s = ParseStr (s, (int)',', color_str, sizeof(color_str));

   InitScan (s, ", \t\n");

   rotate = 0;
   pen = 1;
   rotation = 0;
   if (fileVersion <= 2)
   {
      if (GETVALUE (x,         "x") == INVALID ||
          GETVALUE (y,         "y") == INVALID ||
          GETVALUE (font,      "font") == INVALID ||
          GETVALUE (style,     "style") == INVALID ||
          GETVALUE (size,      "size") == INVALID ||
          GETVALUE (num_lines, "num_lines") == INVALID ||
          GETVALUE (text_just, "text_just") == INVALID)
      {
         return;
      }
      id = objId++;
   }
   else if (fileVersion <= 6)
   {
      if (GETVALUE (x,         "x") == INVALID ||
          GETVALUE (y,         "y") == INVALID ||
          GETVALUE (font,      "font") == INVALID ||
          GETVALUE (style,     "style") == INVALID ||
          GETVALUE (size,      "size") == INVALID ||
          GETVALUE (num_lines, "num_lines") == INVALID ||
          GETVALUE (text_just, "text_just") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (pen,       "pen") == INVALID)
      {
         return;
      }
      id = objId++;
   }
   else if (fileVersion <= 7)
   {
      if (GETVALUE (x,         "x") == INVALID ||
          GETVALUE (y,         "y") == INVALID ||
          GETVALUE (font,      "font") == INVALID ||
          GETVALUE (style,     "style") == INVALID ||
          GETVALUE (size,      "size") == INVALID ||
          GETVALUE (num_lines, "num_lines") == INVALID ||
          GETVALUE (text_just, "text_just") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (pen,       "pen") == INVALID ||
          GETVALUE (obbox_w,   "bbox w") == INVALID ||
          GETVALUE (obbox_h,   "bbox h") == INVALID)
      {
         return;
      }
      id = objId++;
   }
   else if (fileVersion <= 9)
   {
      if (GETVALUE (x,         "x") == INVALID ||
          GETVALUE (y,         "y") == INVALID ||
          GETVALUE (font,      "font") == INVALID ||
          GETVALUE (style,     "style") == INVALID ||
          GETVALUE (size,      "size") == INVALID ||
          GETVALUE (num_lines, "num_lines") == INVALID ||
          GETVALUE (text_just, "text_just") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (pen,       "pen") == INVALID ||
          GETVALUE (obbox_w,   "bbox w") == INVALID ||
          GETVALUE (obbox_h,   "bbox h") == INVALID ||
          GETVALUE (id,        "id") == INVALID ||
          GETVALUE (dpi,       "dpi") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 10)
   {
      if (GETVALUE (x,         "x") == INVALID ||
          GETVALUE (y,         "y") == INVALID ||
          GETVALUE (font,      "font") == INVALID ||
          GETVALUE (style,     "style") == INVALID ||
          GETVALUE (size,      "size") == INVALID ||
          GETVALUE (num_lines, "num_lines") == INVALID ||
          GETVALUE (text_just, "text_just") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (pen,       "pen") == INVALID ||
          GETVALUE (obbox_w,   "bbox w") == INVALID ||
          GETVALUE (obbox_h,   "bbox h") == INVALID ||
          GETVALUE (id,        "id") == INVALID ||
          GETVALUE (dpi,       "dpi") == INVALID ||
          GETVALUE (asc,       "ascent") == INVALID ||
          GETVALUE (des,       "descent") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 12)
   {
      if (GETVALUE (x,         "x") == INVALID ||
          GETVALUE (y,         "y") == INVALID ||
          GETVALUE (font,      "font") == INVALID ||
          GETVALUE (style,     "style") == INVALID ||
          GETVALUE (size,      "size") == INVALID ||
          GETVALUE (num_lines, "num_lines") == INVALID ||
          GETVALUE (text_just, "text_just") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (pen,       "pen") == INVALID ||
          GETVALUE (obbox_w,   "bbox w") == INVALID ||
          GETVALUE (obbox_h,   "bbox h") == INVALID ||
          GETVALUE (id,        "id") == INVALID ||
          GETVALUE (dpi,       "dpi") == INVALID ||
          GETVALUE (asc,       "ascent") == INVALID ||
          GETVALUE (des,       "descent") == INVALID ||
          GETVALUE (fill,      "fill") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 13)
   {
      if (GETVALUE (x,         "x") == INVALID ||
          GETVALUE (y,         "y") == INVALID ||
          GETVALUE (font,      "font") == INVALID ||
          GETVALUE (style,     "style") == INVALID ||
          GETVALUE (size,      "size") == INVALID ||
          GETVALUE (num_lines, "num_lines") == INVALID ||
          GETVALUE (text_just, "text_just") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (pen,       "pen") == INVALID ||
          GETVALUE (obbox_w,   "bbox w") == INVALID ||
          GETVALUE (obbox_h,   "bbox h") == INVALID ||
          GETVALUE (id,        "id") == INVALID ||
          GETVALUE (dpi,       "dpi") == INVALID ||
          GETVALUE (asc,       "ascent") == INVALID ||
          GETVALUE (des,       "descent") == INVALID ||
          GETVALUE (fill,      "fill") == INVALID ||
          GETVALUE (v_space,   "vertical spacing") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 25)
   {
      if (GETVALUE (x,         "x") == INVALID ||
          GETVALUE (y,         "y") == INVALID ||
          GETVALUE (font,      "font") == INVALID ||
          GETVALUE (style,     "style") == INVALID ||
          GETVALUE (size,      "size") == INVALID ||
          GETVALUE (num_lines, "num_lines") == INVALID ||
          GETVALUE (text_just, "text_just") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (pen,       "pen") == INVALID ||
          GETVALUE (obbox_w,   "bbox w") == INVALID ||
          GETVALUE (obbox_h,   "bbox h") == INVALID ||
          GETVALUE (id,        "id") == INVALID ||
          GETVALUE (dpi,       "dpi") == INVALID ||
          GETVALUE (asc,       "ascent") == INVALID ||
          GETVALUE (des,       "descent") == INVALID ||
          GETVALUE (fill,      "fill") == INVALID ||
          GETVALUE (v_space,   "vertical spacing") == INVALID ||
          GETVALUE (rotation,  "rotation") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 29)
   {
      if (GETVALUE (x,         "x") == INVALID ||
          GETVALUE (y,         "y") == INVALID ||
          GETVALUE (font,      "font") == INVALID ||
          GETVALUE (style,     "style") == INVALID ||
          GETVALUE (size,      "size") == INVALID ||
          GETVALUE (num_lines, "num_lines") == INVALID ||
          GETVALUE (text_just, "text_just") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (pen,       "pen") == INVALID ||
          GETVALUE (obbox_w,   "bbox w") == INVALID ||
          GETVALUE (obbox_h,   "bbox h") == INVALID ||
          GETVALUE (id,        "id") == INVALID ||
          GETVALUE (dpi,       "dpi") == INVALID ||
          GETVALUE (asc,       "ascent") == INVALID ||
          GETVALUE (des,       "descent") == INVALID ||
          GETVALUE (fill,      "fill") == INVALID ||
          GETVALUE (v_space,   "vertical spacing") == INVALID ||
          GETVALUE (rotation,  "rotation") == INVALID ||
          GETVALUE (locked,    "locked") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 32)
   {
      if (GETVALUE (x,         "x") == INVALID ||
          GETVALUE (y,         "y") == INVALID ||
          GETSTRNG (font_str,  "font_str") == INVALID ||
          GETVALUE (style,     "style") == INVALID ||
          GETVALUE (size,      "size") == INVALID ||
          GETVALUE (num_lines, "num_lines") == INVALID ||
          GETVALUE (text_just, "text_just") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (pen,       "pen") == INVALID ||
          GETVALUE (obbox_w,   "bbox w") == INVALID ||
          GETVALUE (obbox_h,   "bbox h") == INVALID ||
          GETVALUE (id,        "id") == INVALID ||
          GETVALUE (dpi,       "dpi") == INVALID ||
          GETVALUE (asc,       "ascent") == INVALID ||
          GETVALUE (des,       "descent") == INVALID ||
          GETVALUE (fill,      "fill") == INVALID ||
          GETVALUE (v_space,   "vertical spacing") == INVALID ||
          GETVALUE (rotation,  "rotation") == INVALID ||
          GETVALUE (locked,    "locked") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else
   {
      if (GETVALUE (x,                "x") == INVALID ||
          GETVALUE (y,                "y") == INVALID ||
          GETSTRNG (font_str,         "font_str") == INVALID ||
          GETVALUE (style,            "style") == INVALID ||
          GETVALUE (size,             "size") == INVALID ||
          GETVALUE (num_lines,        "num_lines") == INVALID ||
          GETVALUE (text_just,        "text_just") == INVALID ||
          GETVALUE (rotate,           "rotate") == INVALID ||
          GETVALUE (pen,              "pen") == INVALID ||
          GETVALUE (obbox_w,          "bbox w") == INVALID ||
          GETVALUE (obbox_h,          "bbox h") == INVALID ||
          GETVALUE (id,               "id") == INVALID ||
          GETVALUE (dpi,              "dpi") == INVALID ||
          GETVALUE (asc,              "ascent") == INVALID ||
          GETVALUE (des,              "descent") == INVALID ||
          GETVALUE (fill,             "fill") == INVALID ||
          GETVALUE (v_space,          "vertical spacing") == INVALID ||
          GETVALUE (rotation,         "rotation") == INVALID ||
          GETVALUE (locked,           "locked") == INVALID ||
          GETVALUE (underline_on,     "underline_on") == INVALID ||
          GETVALUE (underline,        "underline") == INVALID ||
          GETVALUE (min_lbearing,     "min_lbearing") == INVALID ||
          GETVALUE (max_rextra,       "max_rextra") == INVALID ||
          GETVALUE (double_byte,      "double_byte") == INVALID ||
          GETVALUE (direction,        "direction") == INVALID ||
          GETSTRNG (custom_font_name, "custom_font_name") == INVALID ||
          GETVALUE (compressed,       "compressed") == INVALID ||
          GETVALUE (transformed,      "transformed") == INVALID ||
          GETVALUE (invisible,        "invisible") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
      len = strlen (custom_font_name);
      if (len >= 2 && *custom_font_name=='"' && custom_font_name[len-1]=='"')
      {
         for (c_ptr=custom_font_name; c_ptr[2] != '\0'; c_ptr++)
            *c_ptr = c_ptr[1];
         *c_ptr = '\0';
      }
   }
   if (fileVersion >= 33 && transformed) {
      fgets(inbuf, MAXSTRING, FP);
      scanLineNum++;
      InitScan(inbuf, "\t\n, ");

      ctm = (struct XfrmMtrxRec *)malloc(sizeof(struct XfrmMtrxRec));
      if (ctm == NULL) FailAllocMessage();
      if (GETVALUE(real_x,           "real_x") == INVALID ||
          GETVALUE(real_y,           "real_y") == INVALID ||
          GETVALUE(orig_obbox.ltx,   "orig_obbox.ltx") == INVALID ||
          GETVALUE(orig_obbox.lty,   "orig_obbox.lty") == INVALID ||
          GETVALUE(orig_obbox.rbx,   "orig_obbox.rbx") == INVALID ||
          GETVALUE(orig_obbox.rby,   "orig_obbox.rby") == INVALID ||
          GETVALUE(ctm->m[CTM_SX],   "CTM_SX") == INVALID ||
          GETVALUE(ctm->m[CTM_SIN],  "CTM_SIN") == INVALID ||
          GETVALUE(ctm->m[CTM_MSIN], "CTM_MSIN") == INVALID ||
          GETVALUE(ctm->m[CTM_SY],   "CTM_SY") == INVALID ||
          GETVALUE(ctm->m[CTM_TX],   "CTM_TX") == INVALID ||
          GETVALUE(ctm->m[CTM_TY],   "CTM_TY") == INVALID ||
          GETVALUE(orig_bbox.ltx,    "orig_bbox.ltx") == INVALID ||
          GETVALUE(orig_bbox.lty,    "orig_bbox.lty") == INVALID ||
          GETVALUE(orig_bbox.rbx,    "orig_bbox.rbx") == INVALID ||
          GETVALUE(orig_bbox.rby,    "orig_bbox.rby") == INVALID) {
         return;
      }
   }
   fill = UpgradePenFill (fill);
   pen = UpgradePenFill (pen);

   *ObjPtr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));

   text_ptr = (struct TextRec *)malloc(sizeof(struct TextRec));
   if (text_ptr == NULL) FailAllocMessage();
   memset(text_ptr, 0, sizeof(struct TextRec));
   text_ptr->lines = num_lines;
   text_ptr->cached_bitmap = None;
   text_ptr->cached_zoom = 0;
   text_ptr->cached_rotate = ROTATE0;

   if (!PRTGIF) SaveCurFont ();

   if (PRTGIF)
   {
      if (fileVersion <= 29)
      {
         curFont = text_ptr->font = font;
         text_ptr->font_name = NULL;
      }
      else
      {
         len = strlen (font_str);
         text_ptr->font_name = (char *)malloc((len+1)*sizeof(char));
         if (text_ptr->font_name == NULL) FailAllocMessage();
         if (*font_str == '\'' && font_str[len-1] == '\'') {
            font_str[len-1] = '\0';
            strcpy(text_ptr->font_name, &font_str[1]);
         } else {
            strcpy(text_ptr->font_name, font_str);
         }
         curFont = text_ptr->font =
               GetFontIndex (text_ptr->font_name, style, FALSE);
      }
   }
   else
   {
      text_ptr->font_name = NULL;
      if (fileVersion <= 29)
         curFont = text_ptr->font = font;
      else
      {
         char	*s;

         len = strlen (font_str);
         if (*font_str == '\'' && font_str[len-1] == '\'')
         {
            font_str[len-1] = '\0';
            s = &font_str[1];
         }
         else
            s = font_str;
         curFont = text_ptr->font = GetFontIndex (s, style, TRUE);
         if (curFont == INVALID)
         {
            char	msg[MAXSTRING];

            sprintf (msg, "Can not find screen font for '%s'.", s);
            TwoLineMsg (msg, "    Use Times instead.");
            SetFileModified (TRUE);
            curFont = text_ptr->font = FONT_TIM;
         }
      }
   }
   curStyle = text_ptr->style = style;
   curSize = text_ptr->size =
         (fileVersion<=29 ? GetCompatibleSize (dpi, size) : size);
   textJust = text_ptr->just = text_just;
   textVSpace = text_ptr->v_space = v_space;
   curRotate = text_ptr->rotate = rotate;
   penPat = text_ptr->pen = pen;
   objFill = text_ptr->fill = fill;
   curUnderlineOn = text_ptr->underline_on = underline_on;
   curUnderline = text_ptr->underline = underline;

   if (PRTGIF)
   {
      if (fileVersion < 10)
      {
         canvasFontAsc =
               pDrawFontAsc[OldFontIndex(dpi,curFont,size,curStyle)];
         canvasFontDes =
               pDrawFontDes[OldFontIndex(dpi,curFont,size,curStyle)];
      }
      else
      {
         canvasFontAsc = asc;
         canvasFontDes = des;
      }
      textCursorH = canvasFontAsc + canvasFontDes;
      text_ptr->read_only = FALSE;
      if (fileVersion > 31)
      {
         canvasFontDoubleByte = double_byte;
         canvasFontDirection = direction;
      }
      else
      {
         canvasFontDoubleByte = FALSE;
         canvasFontDirection = FontLeftToRight;
      }
   }
   else
   {
      SetCanvasFont ();
      text_ptr->read_only = (curSize != canvasFontSize);
   }
   if (text_ptr->read_only)
   {
      text_ptr->asc = asc;
      text_ptr->des = des;
      switch (rotate)
      {
         case ROTATE0:
         case ROTATE180:
            text_ptr->orig_w = obbox_w;
            text_ptr->orig_h = obbox_h;
            break;
         case ROTATE90:
         case ROTATE270:
            text_ptr->orig_w = obbox_h;
            text_ptr->orig_h = obbox_w;
            break;
      }
      text_ptr->min_lbearing = min_lbearing;
      text_ptr->max_rextra = max_rextra;
   }
   else
   {
      text_ptr->asc = canvasFontAsc;
      text_ptr->des = canvasFontDes;
      text_ptr->orig_w = text_ptr->orig_h = 0;
      text_ptr->min_lbearing = text_ptr->max_rextra = 0;
   }
   text_ptr->double_byte = canvasFontDoubleByte;
   text_ptr->direction = canvasFontDirection;
   if (*custom_font_name == '\0')
      text_ptr->custom_screen_font_name = NULL;
   else
      text_ptr->custom_screen_font_name = UtilStrDup (custom_font_name);

   for (i = 0; i < num_lines; i++)
   {
      char *line;

      if ((line=UtilGetALine(FP)) == NULL) {
         sprintf(gszMsgBox, "%s, %d:  EOF in ReadTextObj ().  Read aborted!",
               scanFileName, scanLineNum);
         if (PRTGIF) {
            fprintf (stderr, "%s\n", gszMsgBox);
         } else {
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         }
         free(line);
         return;
      }
      scanLineNum++;

      tmp_str = FindChar ((int)'"', line);
      s = ReadString (tmp_str);
      *(--s) = '\0';
      s_ptr = NewStr ();
      if (s_ptr == NULL) FailAllocMessage ();
      DynStrSet (&s_ptr->dyn_str, tmp_str);
      AddStr (lastStr, (struct StrRec *)NULL, s_ptr);
      if (PRTGIF)
      {
         len = strlen (tmp_str); /* assume string width = 1 pixel per char */
         if (len > max_len) max_len = len;
      }
      else
      {
         int w, lbearing, rextra;

         GetStrSizeInfo (s_ptr, &w, &lbearing, &rextra);
         if (w > max_len) max_len = w;
         if (lbearing < min_lbearing) min_lbearing = lbearing;
         if (rextra > max_rextra) max_rextra = rextra;
      }

      free(line);
   }
   text_ptr->first = firstStr;
   text_ptr->last = lastStr;

   firstStr = lastStr = NULL;
   textCurIndex = 0;

   text_ptr->min_lbearing = min_lbearing;
   text_ptr->max_rextra = max_rextra;
   (*ObjPtr)->type = OBJ_TEXT;
   (*ObjPtr)->color = QuickFindColorIndex(*ObjPtr, color_str, &new_alloc, TRUE);
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->detail.t = text_ptr;
   (*ObjPtr)->x = x;
   (*ObjPtr)->y = y;
   (*ObjPtr)->ctm = ctm;
   (*ObjPtr)->invisible = invisible;

   if (ctm != NULL) {
      memcpy(&(*ObjPtr)->orig_obbox, &orig_obbox, sizeof(struct BBRec));
      memcpy(&text_ptr->orig_bbox, &orig_bbox, sizeof(struct BBRec));
      (*ObjPtr)->x = real_x;
      (*ObjPtr)->y = real_y;
      GetTransformedOBBoxOffsetVs(*ObjPtr, (*ObjPtr)->rotated_obbox);
   }
   if (PRTGIF && fileVersion > 6) {
      switch (rotate) {
      case ROTATE0:
      case ROTATE180:
         SetTextBBox(*ObjPtr, text_just, obbox_w, obbox_h,
               min_lbearing, max_rextra, rotate);
         break;
      case ROTATE90:
      case ROTATE270:
         SetTextBBox(*ObjPtr, text_just, obbox_h, obbox_w,
               min_lbearing, max_rextra, rotate);
         break;
      }
   } else if (!PRTGIF && text_ptr->read_only) {
      SetTextBBox(*ObjPtr, text_just, text_ptr->orig_w, text_ptr->orig_h,
            min_lbearing, max_rextra, rotate);
   } else {
      SetTextBBox(*ObjPtr, text_just, max_len,
            num_lines*textCursorH+(num_lines-1)*textVSpace,
            min_lbearing, max_rextra, rotate);
   }
   if (fileVersion < 33 && rotate != ROTATE0) {
      int ltx, lty;

      if (rotate == ROTATE90 || rotate == ROTATE270) {
         int h=(*ObjPtr)->obbox.rbx-(*ObjPtr)->obbox.ltx;
         int w=(*ObjPtr)->obbox.rby-(*ObjPtr)->obbox.lty;

         (*ObjPtr)->obbox.rby = (*ObjPtr)->obbox.lty + h;
         (*ObjPtr)->obbox.rbx = (*ObjPtr)->obbox.ltx + w;

         switch (text_just) {
         case JUST_L: (*ObjPtr)->x = (*ObjPtr)->obbox.ltx; break;
         case JUST_C:
            (*ObjPtr)->x = (((*ObjPtr)->obbox.ltx+(*ObjPtr)->obbox.rbx)>>1);
            break;
         case JUST_R: (*ObjPtr)->x = (*ObjPtr)->obbox.rbx; break;
         }
         (*ObjPtr)->y = (*ObjPtr)->obbox.lty;
      } else {
         switch (text_just) {
         case JUST_L: (*ObjPtr)->x = (*ObjPtr)->obbox.ltx; break;
         case JUST_C: break;
         case JUST_R: (*ObjPtr)->x = (*ObjPtr)->obbox.rbx; break;
         }
         (*ObjPtr)->y = (*ObjPtr)->obbox.lty;
      }
      ltx = ((*ObjPtr)->obbox.ltx);
      lty = ((*ObjPtr)->obbox.lty);
      SetRotatePivotByObject(*ObjPtr);

      switch (rotate) {
      case ROTATE90:
         RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
         break;
      case ROTATE180:
         RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
         RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
         break;
      case ROTATE270:
         RotateObj(*ObjPtr, CORNER_LT, COUNTER90, &ltx, &lty);
         break;
      }
      text_ptr->rotate = rotate = ROTATE0;
   }
   if (PRTGIF && fileVersion > 6) {
      switch (rotate) {
      case ROTATE0:
      case ROTATE180:
         SetTextBBox(*ObjPtr, text_just, obbox_w, obbox_h,
               min_lbearing, max_rextra, rotate);
         break;
      case ROTATE90:
      case ROTATE270:
         SetTextBBox(*ObjPtr, text_just, obbox_h, obbox_w,
               min_lbearing, max_rextra, rotate);
         break;
      }
   } else if (!PRTGIF && text_ptr->read_only) {
      SetTextBBox(*ObjPtr, text_just, text_ptr->orig_w, text_ptr->orig_h,
            min_lbearing, max_rextra, rotate);
   } else {
      SetTextBBox(*ObjPtr, text_just, max_len,
            num_lines*textCursorH+(num_lines-1)*textVSpace,
            min_lbearing, max_rextra, rotate);
   }
   if (!PRTGIF) RestoreCurFont();

   if (text_ptr->rotate != ROTATE0) {
      fprintf(stderr, "ReadTextObj(): text object rotation is not 0!\n");
   }
}

void RedrawCurText()
{
   register int x=0, y;
   struct StrRec *s_ptr;
   int highlighting=FALSE, mode, first_index=INVALID, second_index=INVALID;

   if (!textCursorShown) return;

   switch (textJust) {
   case JUST_L: x = textOrigX+textAbsMinLBearing; break;
   case JUST_C: x = textOrigX-textW/2+textAbsMinLBearing; break;
   case JUST_R: x = textOrigX-textW+textAbsMinLBearing; break;
   }
   XClearArea(mainDisplay, drawWindow, x-2, textOrigY-2,
         textW-textAbsMinLBearing+textAbsMaxRExtra+5, textH+5, FALSE);
   XDrawRectangle(mainDisplay, drawWindow, defaultGC, x-2, textOrigY-2,
         textW-textAbsMinLBearing+textAbsMaxRExtra+4, textH+4);

   y = textOrigY;

   for (s_ptr=curTextObj->detail.t->first; s_ptr != NULL; s_ptr=s_ptr->next) {
      if (textHighlight) {
         first_index = second_index = INVALID;
         if (highlighting) {
            if (s_ptr == curStr || s_ptr == endStr) {
               mode = PAINT_INV_NORM;
               first_index = (s_ptr == curStr) ? textCurIndex : textEndIndex;
               highlighting = FALSE;
            } else {
               mode = PAINT_INV;
               first_index = (s_ptr == curStr) ? textCurIndex : textEndIndex;
            }
         } else {
            if (s_ptr == curStr && s_ptr == endStr) {
               mode = PAINT_NORM_INV_NORM;
               first_index = min(textCurIndex,textEndIndex);
               second_index = max(textCurIndex,textEndIndex);
            } else if (s_ptr == curStr || s_ptr == endStr) {
               mode = PAINT_NORM_INV;
               first_index = (s_ptr == curStr) ? textCurIndex : textEndIndex;
               highlighting = TRUE;
            } else {
               mode = PAINT_NORM;
            }
         }
      } else {
         mode = PAINT_NORM;
      }
      PaintCurText(drawWindow, drawGC, s_ptr->dyn_str.s, textJust, textOrigX, y,
            canvasFontPtr, colorIndex, penPat, mode, first_index, second_index);
      y += textCursorH+textVSpace;
   }
   PutTextCursor();
}

void UpdCurTextBBox()
{
   struct StrRec *s_ptr;
   int num=0, h, freeze_cur_y=FALSE;

   if (!textCursorShown) return;

   textCurY = textOrigY;
   for (s_ptr=firstStr; s_ptr != NULL; s_ptr=s_ptr->next, num++) {
      int w=XTextWidth(canvasFontPtr, s_ptr->dyn_str.s, s_ptr->dyn_str.sz-1);

      if (w > textW) textW = w;
      if (!freeze_cur_y) {
         if (curStr == s_ptr) {
            curStrW = w;
            freeze_cur_y = TRUE;
         } else {
            textCurY += textCursorH+textVSpace;
         }
      }
   }
   h = textCursorH*num + textVSpace*(num-1);
   if (h > textH) textH = h;
   SetTextCurX();
}

void AdjustCurText(XOff, YOff)
   int XOff, YOff;
{
   textOrigX += XOff;
   textOrigY += YOff;
   textCurX += XOff;
   textCurY += YOff;
}

void PrepareZoomCurText(AbsXc, AbsYc)
   int *AbsXc, *AbsYc;
{
   switch (textJust) {
   case JUST_L: *AbsXc = ABS_X(textOrigX+(textW>>1)); break;
   case JUST_C: *AbsXc = ABS_X(textOrigX); break;
   case JUST_R: *AbsXc = ABS_X(textOrigX-(textW>>1)); break;
   }
   *AbsYc = ABS_Y(textOrigY+(textH>>1));
}

void PostZoomCurText(AbsXc, AbsYc)
   int AbsXc, AbsYc;
{
   int x=0, y;

   switch (textJust) {
   case JUST_L: x = OFFSET_X(AbsXc)-(textW>>1); break;
   case JUST_C: x = OFFSET_X(AbsXc); break;
   case JUST_R: x = OFFSET_X(AbsXc)+(textW>>1); break;
   }
   y = OFFSET_Y(AbsYc)-(textH>>1);
   AdjustCurText(x-textOrigX, y-textOrigY);
   textAbsX -= tmpAdjX;
   textAbsY -= tmpAdjY;
   switch (curRotate) {
   case ROTATE0:
      switch (textJust) {
      case JUST_L: tmpAdjX = (textW-ABS_SIZE(textW))/2; break;
      case JUST_C: tmpAdjX = 0; break;
      case JUST_R: tmpAdjX = (ABS_SIZE(textW)-textW)/2; break;
      }
      tmpAdjY = (textH-ABS_SIZE(textH))/2;
      break;
   case ROTATE90:
      switch (textJust) {
      case JUST_L:
         tmpAdjX = -(ABS_SIZE(textW)+textH)/2;
         tmpAdjY = (textW-ABS_SIZE(textH))/2;
         break;
      case JUST_C:
         tmpAdjX = -textH/2;
         tmpAdjY = -ABS_SIZE(textH)/2;
         break;
      case JUST_R:
         tmpAdjX = (ABS_SIZE(textW)-textH)/2;
         tmpAdjY = -(textW+ABS_SIZE(textH))/2;
         break;
      }
      break;
   case ROTATE180:
      switch (textJust) {
      case JUST_L: tmpAdjX = -(textW+ABS_SIZE(textW))/2; break;
      case JUST_C: tmpAdjX = 0; break;
      case JUST_R: tmpAdjX = (textW+ABS_SIZE(textW))/2; break;
      }
      tmpAdjY = -(textH+ABS_SIZE(textH))/2;
      break;
   case ROTATE270:
      switch (textJust) {
      case JUST_L:
         tmpAdjX = -(ABS_SIZE(textW)-textH)/2;
         tmpAdjY = -(textW+ABS_SIZE(textH))/2;
         break;
      case JUST_C:
         tmpAdjX = textH/2;
         tmpAdjY = -ABS_SIZE(textH)/2;
         break;
      case JUST_R:
         tmpAdjX = (ABS_SIZE(textW)+textH)/2;
         tmpAdjY = (textW-ABS_SIZE(textH))/2;
         break;
      }
      break;
   }
   textAbsX += tmpAdjX;
   textAbsY += tmpAdjY;
   RedrawCurText();
}
