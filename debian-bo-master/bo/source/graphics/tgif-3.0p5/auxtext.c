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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/auxtext.c,v 3.0 1996/05/06 16:03:49 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#ifndef _NO_EXTERN
#include "auxtext.e"
#endif
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "file.e"
#include "font.e"
#include "msg.e"
#include "obj.e"
#include "pattern.e"
#include "prtgif.e"
#include "ps.e"
#include "raster.e"
#include "rect.e"
#include "setup.e"
#include "text.e"
#include "util.e"
#include "xbitmap.e"
#include "xpixmap.e"

#ifndef XK_KP_Left
#define XK_KP_Left	0xFF96
#define XK_KP_Up	0xFF97
#define XK_KP_Right	0xFF98
#define XK_KP_Down	0xFF99
#endif /* ~XK_KP_LEFT */

GC	rotateGC = NULL;
Pixmap	textBackingPixmap = None;
int	textBackingPixmapW = INVALID;
int	textBackingPixmapH = INVALID;
int	doubleQuoteDoubleQuote = FALSE;
int	groupedTextEditable = FALSE;

struct SelRec	* outerSel=NULL, * innerSel=NULL;

void GetStrSizeInfo (StrPtr, w, lbearing, rextra)
   struct StrRec	* StrPtr;
   int			* w, * lbearing, * rextra;
{
   int				dir, asc, des;
   XCharStruct			xcs;

   if (canvasFontDoubleByte)
      XTextExtents16 (canvasFontPtr, (XChar2b*)StrPtr->dyn_str.s,
            (StrPtr->dyn_str.sz-1)>>1, &dir, &asc, &des, &xcs);
   else
      XTextExtents (canvasFontPtr, StrPtr->dyn_str.s, StrPtr->dyn_str.sz-1,
            &dir, &asc, &des, &xcs);

   if (w != NULL) *w = xcs.width;
   if (lbearing != NULL) *lbearing = (xcs.lbearing >= 0 ? 0 : xcs.lbearing);
   if (rextra != NULL) *rextra = xcs.rbearing-xcs.width;
}

void GetTextObjSizeInfo (TextPtr, num_lines, max_w, min_lbearing, max_rextra)
   struct TextRec	* TextPtr;
   int			* num_lines, * max_w, * min_lbearing, * max_rextra;
{
   register struct StrRec	* s_ptr;
   int				dir, asc, des;
   int				w=0, lbearing=0, rextra=0, n=0;
   XCharStruct			xcs;

   for (s_ptr=TextPtr->first; s_ptr != NULL; s_ptr=s_ptr->next, n++)
   {
      if (canvasFontDoubleByte)
         XTextExtents16 (canvasFontPtr, (XChar2b*)s_ptr->dyn_str.s,
               (s_ptr->dyn_str.sz-1)>>1, &dir, &asc, &des, &xcs);
      else
         XTextExtents (canvasFontPtr, s_ptr->dyn_str.s, s_ptr->dyn_str.sz-1,
               &dir, &asc, &des, &xcs);
      if (w < xcs.width) w = xcs.width;
      if (lbearing > xcs.lbearing) lbearing = xcs.lbearing;
      if (rextra < (xcs.rbearing-xcs.width)) rextra = xcs.rbearing-xcs.width;
   }
   if (max_w != NULL) *max_w = w;
   if (min_lbearing != NULL) *min_lbearing = (lbearing >= 0 ? 0 : lbearing);
   if (max_rextra != NULL) *max_rextra = rextra;
   if (num_lines != NULL) *num_lines = n;
}

void TranslateKeys (s, key_sym)
   char		* s;
   KeySym	* key_sym;
{
   switch (*key_sym)
   {
      case XK_KP_Space:     *key_sym=XK_space;    s[0]=' ';  s[1]='\0'; break;
      case XK_KP_Tab:       *key_sym=XK_Tab;      s[0]='\t'; s[1]='\0'; break;
      case XK_KP_Enter:     *key_sym=XK_Return;   s[0]='\r'; s[1]='\0'; break;
      case XK_KP_Equal:     *key_sym=XK_equal;    s[0]='=';  s[1]='\0'; break;
      case XK_KP_Multiply:  *key_sym=XK_multiply; s[0]='*';  s[1]='\0'; break;
      case XK_KP_Add:       *key_sym=XK_plus;     s[0]='+';  s[1]='\0'; break;
      case XK_KP_Separator: *key_sym=XK_comma;    s[0]=',';  s[1]='\0'; break;
      case XK_KP_Subtract:  *key_sym=XK_minus;    s[0]='-';  s[1]='\0'; break;
      case XK_KP_Decimal:   *key_sym=XK_period;   s[0]='.';  s[1]='\0'; break;
      case XK_KP_Divide:    *key_sym=XK_slash;    s[0]='/';  s[1]='\0'; break;

      case XK_KP_0: *key_sym=XK_0; s[0]='0'; s[1]='\0'; break;
      case XK_KP_1: *key_sym=XK_1; s[0]='1'; s[1]='\0'; break;
      case XK_KP_2: *key_sym=XK_2; s[0]='2'; s[1]='\0'; break;
      case XK_KP_3: *key_sym=XK_3; s[0]='3'; s[1]='\0'; break;
      case XK_KP_4: *key_sym=XK_4; s[0]='4'; s[1]='\0'; break;
      case XK_KP_5: *key_sym=XK_5; s[0]='5'; s[1]='\0'; break;
      case XK_KP_6: *key_sym=XK_6; s[0]='6'; s[1]='\0'; break;
      case XK_KP_7: *key_sym=XK_7; s[0]='7'; s[1]='\0'; break;
      case XK_KP_8: *key_sym=XK_8; s[0]='8'; s[1]='\0'; break;
      case XK_KP_9: *key_sym=XK_9; s[0]='9'; s[1]='\0'; break;
   }
}

static
void GetTransformedTextBBoxAbsVs (ObjPtr, LBearing, RightExtra, Vs)
   struct ObjRec	* ObjPtr;
   int			LBearing, RightExtra;
   IntPoint		* Vs; /* array of 5 points */
{
   int		x, y;
   struct BBRec	bbox;

   bbox.ltx = ObjPtr->orig_obbox.ltx - ObjPtr->x + LBearing;
   bbox.lty = ObjPtr->orig_obbox.lty - ObjPtr->y;
   bbox.rbx = ObjPtr->orig_obbox.rbx - ObjPtr->x + RightExtra;
   bbox.rby = ObjPtr->orig_obbox.rby - ObjPtr->y;
   TransformOffsetBBoxThroughCTM (&bbox, ObjPtr->ctm, Vs);
   x = Vs[0].x + ObjPtr->x; y = Vs[0].y + ObjPtr->y;
   Vs[0].x = Vs[4].x = x; Vs[0].y = Vs[4].y = y;
   x = Vs[1].x + ObjPtr->x; y = Vs[1].y + ObjPtr->y;
   Vs[1].x = x; Vs[1].y = y;
   x = Vs[2].x + ObjPtr->x; y = Vs[2].y + ObjPtr->y;
   Vs[2].x = x; Vs[2].y = y;
   x = Vs[3].x + ObjPtr->x; y = Vs[3].y + ObjPtr->y;
   Vs[3].x = x; Vs[3].y = y;
}

void SetTextOBBox(ObjPtr, Just, W, H, LBearing, RightExtra, Rotate)
   struct ObjRec *ObjPtr;
   int Just, W, H, LBearing, RightExtra, Rotate;
{  /* In this procedure, it is assumed that the x and y field */
   /*	of the text object has the correct information.*/
   /* The rotation is clockwise */
   register int	mw2, pw2;

   if (ObjPtr->ctm == NULL) {
      switch (Just) {
      case JUST_L:
         switch (Rotate) {
         case ROTATE0:
            ObjPtr->obbox.ltx = ObjPtr->x; ObjPtr->obbox.rbx = ObjPtr->x+W;
            ObjPtr->obbox.lty = ObjPtr->y; ObjPtr->obbox.rby = ObjPtr->y+H;
            break;
         case ROTATE90:
            ObjPtr->obbox.ltx = ObjPtr->x-H; ObjPtr->obbox.rbx = ObjPtr->x;
            ObjPtr->obbox.lty = ObjPtr->y; ObjPtr->obbox.rby = ObjPtr->y+W;
            break;
         case ROTATE180:
            ObjPtr->obbox.ltx = ObjPtr->x-W; ObjPtr->obbox.rbx = ObjPtr->x;
            ObjPtr->obbox.lty = ObjPtr->y-H; ObjPtr->obbox.rby = ObjPtr->y;
            break;
         case ROTATE270:
            ObjPtr->obbox.ltx = ObjPtr->x; ObjPtr->obbox.rbx = ObjPtr->x+H;
            ObjPtr->obbox.lty = ObjPtr->y-W; ObjPtr->obbox.rby = ObjPtr->y;
            break;
         }
         break;
      case JUST_C:
         mw2 = W/2;
         pw2 = W-W/2;
         switch (Rotate) {
         case ROTATE0:
            ObjPtr->obbox.ltx = ObjPtr->x-mw2;
            ObjPtr->obbox.rbx = ObjPtr->x+pw2;
            ObjPtr->obbox.lty = ObjPtr->y; ObjPtr->obbox.rby = ObjPtr->y+H;
            break;
         case ROTATE90:
            ObjPtr->obbox.ltx = ObjPtr->x-H;
            ObjPtr->obbox.rbx = ObjPtr->x;
            ObjPtr->obbox.lty = ObjPtr->y-mw2;
            ObjPtr->obbox.rby = ObjPtr->y+pw2;
            break;
         case ROTATE180:
            ObjPtr->obbox.ltx = ObjPtr->x-pw2;
            ObjPtr->obbox.rbx = ObjPtr->x+mw2;
            ObjPtr->obbox.lty = ObjPtr->y-H;
            ObjPtr->obbox.rby = ObjPtr->y;
            break;
         case ROTATE270:
            ObjPtr->obbox.ltx = ObjPtr->x;
            ObjPtr->obbox.rbx = ObjPtr->x+H;
            ObjPtr->obbox.lty = ObjPtr->y-pw2;
            ObjPtr->obbox.rby = ObjPtr->y+mw2;
            break;
         }
         break;
      case JUST_R:
         switch (Rotate) {
         case ROTATE0:
            ObjPtr->obbox.ltx = ObjPtr->x-W; ObjPtr->obbox.rbx = ObjPtr->x;
            ObjPtr->obbox.lty = ObjPtr->y; ObjPtr->obbox.rby = ObjPtr->y+H;
            break;
         case ROTATE90:
            ObjPtr->obbox.ltx = ObjPtr->x-H; ObjPtr->obbox.rbx = ObjPtr->x;
            ObjPtr->obbox.lty = ObjPtr->y-W; ObjPtr->obbox.rby = ObjPtr->y;
            break;
         case ROTATE180:
            ObjPtr->obbox.ltx = ObjPtr->x; ObjPtr->obbox.rbx = ObjPtr->x+W;
            ObjPtr->obbox.lty = ObjPtr->y-H; ObjPtr->obbox.rby = ObjPtr->y;
            break;
         case ROTATE270:
            ObjPtr->obbox.ltx = ObjPtr->x; ObjPtr->obbox.rbx = ObjPtr->x+H;
            ObjPtr->obbox.lty = ObjPtr->y; ObjPtr->obbox.rby = ObjPtr->y+W;
            break;
         }
         break;
      }
   } else {
      IntPoint abs_obj_obbox_vs[5];

      GetTransformedOBBoxAbsVs(ObjPtr, abs_obj_obbox_vs);
      ObjPtr->obbox.ltx = min(min(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
            min(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
      ObjPtr->obbox.rbx = max(max(abs_obj_obbox_vs[0].x,abs_obj_obbox_vs[1].x),
            max(abs_obj_obbox_vs[2].x,abs_obj_obbox_vs[3].x));
      ObjPtr->obbox.lty = min(min(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
            min(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));
      ObjPtr->obbox.rby = max(max(abs_obj_obbox_vs[0].y,abs_obj_obbox_vs[1].y),
            max(abs_obj_obbox_vs[2].y,abs_obj_obbox_vs[3].y));
   }
}

void SetTextBBox(ObjPtr, Just, W, H, LBearing, RightExtra, Rotate)
   struct ObjRec *ObjPtr;
   int Just, W, H, LBearing, RightExtra, Rotate;
   /* In this procedure, it is assumed that the x and y field */
   /*	of the text object has the correct information.*/
   /* The rotation is clockwise */
{
   SetTextOBBox(ObjPtr, Just, W, H, LBearing, RightExtra, Rotate);

   if (ObjPtr->ctm == NULL) {
      switch (Rotate) {
      case ROTATE0:
         ObjPtr->bbox.ltx = ObjPtr->obbox.ltx + LBearing;
         ObjPtr->bbox.rbx = ObjPtr->obbox.rbx + RightExtra;
         ObjPtr->bbox.lty = ObjPtr->obbox.lty;
         ObjPtr->bbox.rby = ObjPtr->obbox.rby;
         break;
      case ROTATE90:
         ObjPtr->bbox.ltx = ObjPtr->obbox.ltx;
         ObjPtr->bbox.rbx = ObjPtr->obbox.rbx;
         ObjPtr->bbox.lty = ObjPtr->obbox.lty + LBearing;
         ObjPtr->bbox.rby = ObjPtr->obbox.rby + RightExtra;
         break;
      case ROTATE180:
         ObjPtr->bbox.ltx = ObjPtr->obbox.ltx - RightExtra;
         ObjPtr->bbox.rbx = ObjPtr->obbox.rbx - LBearing;
         ObjPtr->bbox.lty = ObjPtr->obbox.lty;
         ObjPtr->bbox.rby = ObjPtr->obbox.rby;
         break;
      case ROTATE270:
         ObjPtr->bbox.ltx = ObjPtr->obbox.ltx;
         ObjPtr->bbox.rbx = ObjPtr->obbox.rbx;
         ObjPtr->bbox.lty = ObjPtr->obbox.lty - RightExtra;
         ObjPtr->bbox.rby = ObjPtr->obbox.rby - LBearing;
         break;
      }
   } else {
      IntPoint abs_obj_bbox_vs[5];

      GetTransformedTextBBoxAbsVs (ObjPtr, LBearing, RightExtra,
            abs_obj_bbox_vs);
      ObjPtr->bbox.ltx = min(min(abs_obj_bbox_vs[0].x,abs_obj_bbox_vs[1].x),
            min(abs_obj_bbox_vs[2].x,abs_obj_bbox_vs[3].x));
      ObjPtr->bbox.rbx = max(max(abs_obj_bbox_vs[0].x,abs_obj_bbox_vs[1].x),
            max(abs_obj_bbox_vs[2].x,abs_obj_bbox_vs[3].x));
      ObjPtr->bbox.lty = min(min(abs_obj_bbox_vs[0].y,abs_obj_bbox_vs[1].y),
            min(abs_obj_bbox_vs[2].y,abs_obj_bbox_vs[3].y));
      ObjPtr->bbox.rby = max(max(abs_obj_bbox_vs[0].y,abs_obj_bbox_vs[1].y),
            max(abs_obj_bbox_vs[2].y,abs_obj_bbox_vs[3].y));
   }
   ObjPtr->bbox.ltx--;
   ObjPtr->bbox.rbx++;
   ObjPtr->bbox.lty--;
   ObjPtr->bbox.rby++;
}

void SetTextOrigBBoxes(ObjPtr, Just, W, H, LBearing, RightExtra, Rotate)
   struct ObjRec *ObjPtr;
   int Just, W, H, LBearing, RightExtra, Rotate;
   /* In this procedure, it is assumed that the x and y field */
   /*	of the text object has the correct information.*/
   /* The rotation is clockwise */
{
   struct BBRec obbox, bbox;
   struct XfrmMtrxRec *ctm;

   if (ObjPtr->ctm == NULL) return;
   memcpy(&obbox, &ObjPtr->obbox, sizeof(struct BBRec));
   memcpy(&bbox, &ObjPtr->bbox, sizeof(struct BBRec));
   ctm = ObjPtr->ctm;
   ObjPtr->ctm = NULL;

   SetTextBBox(ObjPtr, Just, W, H, LBearing, RightExtra, Rotate);

   memcpy(&ObjPtr->orig_obbox, &ObjPtr->obbox, sizeof(struct BBRec));
   memcpy(&ObjPtr->detail.t->orig_bbox, &ObjPtr->bbox, sizeof(struct BBRec));
   ObjPtr->ctm = ctm;
}

int UpdTextBBox (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   int	num_lines, max_len=0, min_lbearing=0, max_rextra=0;

   if (ObjPtr->detail.t->read_only)
   {
      SetTextBBox (ObjPtr, ObjPtr->detail.t->just, ObjPtr->detail.t->orig_w,
            ObjPtr->detail.t->orig_h, ObjPtr->detail.t->min_lbearing,
            ObjPtr->detail.t->max_rextra, ObjPtr->detail.t->rotate);
      return (TRUE);
   }

   SaveCurFont ();
   curFont = ObjPtr->detail.t->font;
   curStyle = ObjPtr->detail.t->style;
   curSize = ObjPtr->detail.t->size;
   textJust = ObjPtr->detail.t->just;
   textVSpace = ObjPtr->detail.t->v_space;
   curUnderlineOn = ObjPtr->detail.t->underline_on;
   curUnderline = ObjPtr->detail.t->underline;
   attemptingToSetFontProperty = TRUE;
   SetCanvasFont ();
   attemptingToSetFontProperty = FALSE;

   if (canvasFontSize == INVALID || curSize != canvasFontSize)
   {
      RestoreCurFont ();
      return (INVALID);
   }
   if (textCursorH+textVSpace <= 0)
   {
      RestoreCurFont ();
      return (FALSE);
   }

   ObjPtr->detail.t->asc = canvasFontAsc;
   ObjPtr->detail.t->des = canvasFontDes;
   GetTextObjSizeInfo (ObjPtr->detail.t, &num_lines, &max_len, &min_lbearing,
         &max_rextra);
   ObjPtr->detail.t->min_lbearing = min_lbearing;
   ObjPtr->detail.t->max_rextra = max_rextra;

   SetTextBBox (ObjPtr, textJust, max_len,
         num_lines*textCursorH+(num_lines-1)*textVSpace,
         min_lbearing, max_rextra, ObjPtr->detail.t->rotate);

   RestoreCurFont ();
   return (TRUE);
}

int PixelOnOff (image, col_start, row_start, scale)
   XImage	* image;
   int		col_start, row_start, scale;
{
   register int	m, n;
   int		on_count=0, off_count=0;

   if (scale == 2)
   {
      for (m = 0; m < 2; m++)
         for (n = 0; n < 2; n++)
            switch (XGetPixel (image,col_start+n,row_start+m))
            {
               case 0: if (++off_count > 2) return (0); break;
               case 1: if (++on_count >= 2) return (1); break;
            }
   }
   else if (scale > 2)
   {
      int	half_scale=scale>>1;

      for (m = 0; m < 2; m++)
         for (n = 0; n < 2; n++)
            switch (PixelOnOff (image, col_start+n*half_scale,
                  row_start+m*half_scale, half_scale))
            {
               case 0: if (++off_count > 2) return (0); break;
               case 1: if (++on_count >= 2) return (1); break;
            }
   }
   else
      return (1);
   return (0);
}

void MakeCachedTextBitmap (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register int		r, c;
   int			w=0, h=0, num_cols=0, num_rows=0;
   int			yinc, y, rotate, watch_cursor, do_msg;
   int			max_len=0, min_lbearing=0, max_rextra=0;
   int			flat=FALSE, skinny=FALSE, start_row, start_col;
   struct MtrxRec	mtrx;
   Pixmap		dest_bitmap;
   XImage		* src_image, * dest_image;
   struct StrRec	* s_ptr;
   struct TextRec	* text_ptr = ObjPtr->detail.t;
   XGCValues		values;


   if (ObjPtr->ctm==NULL && text_ptr->cached_bitmap!=None &&
         text_ptr->cached_zoomed==zoomedIn &&
         text_ptr->cached_zoom==zoomScale && (text_ptr->rotate==ROTATE0 ||
         text_ptr->cached_rotate==text_ptr->rotate))
      return;

   if (ObjPtr->obbox.ltx==ObjPtr->obbox.rbx ||
         ObjPtr->obbox.lty==ObjPtr->obbox.rby)
      return;

   if (text_ptr->cached_bitmap != None)
   {
      XFreePixmap (mainDisplay, text_ptr->cached_bitmap);
      text_ptr->cached_bitmap = None;
   }
   rotate = text_ptr->rotate;

   if (text_ptr->read_only)
      GetTextObjSizeInfo (text_ptr, NULL, &max_len, &min_lbearing,
            &max_rextra);

   switch (rotate)
   {
      case ROTATE0:
      case ROTATE180:
         if (text_ptr->read_only)
         {
            w = max_len-min_lbearing+max_rextra;
            h = text_ptr->lines*textCursorH+(text_ptr->lines-1)*textVSpace;
         }
         else
         {
            if (ObjPtr->ctm == NULL)
            {
               max_len = ObjPtr->obbox.rbx - ObjPtr->obbox.ltx;
               min_lbearing = ObjPtr->detail.t->min_lbearing;
               max_rextra = ObjPtr->detail.t->max_rextra;
               w = ObjPtr->bbox.rbx - ObjPtr->bbox.ltx - 2;
               h = ObjPtr->bbox.rby - ObjPtr->bbox.lty - 2;
            }
            else
            {
               max_len = ObjPtr->orig_obbox.rbx - ObjPtr->orig_obbox.ltx;
               min_lbearing = ObjPtr->detail.t->min_lbearing;
               max_rextra = ObjPtr->detail.t->max_rextra;
               w = max_len - min_lbearing + max_rextra;
               h = ObjPtr->orig_obbox.rby - ObjPtr->orig_obbox.lty;
            }
         }
         break;
      case ROTATE90:
      case ROTATE270:
         if (text_ptr->read_only)
         {
            w = max_len-min_lbearing+max_rextra;
            h = text_ptr->lines*textCursorH+(text_ptr->lines-1)*textVSpace;
         }
         else
         {
            max_len = ObjPtr->obbox.rby - ObjPtr->obbox.lty;
            min_lbearing = ObjPtr->detail.t->min_lbearing;
            max_rextra = ObjPtr->detail.t->max_rextra;
            w = ObjPtr->bbox.rby - ObjPtr->bbox.lty - 2;
            h = ObjPtr->bbox.rbx - ObjPtr->bbox.ltx - 2;
         }
         break;
   }

   if (w > textBackingPixmapW || h > textBackingPixmapH)
   {
      int	tmp_w, tmp_h;

      if (textBackingPixmap != None)
         XFreePixmap (mainDisplay, textBackingPixmap);
      tmp_w = max(w,textBackingPixmapW);
      tmp_h = max(h,textBackingPixmapH);
      textBackingPixmap = XCreatePixmap (mainDisplay, mainWindow,
            tmp_w, tmp_h, 1);
      if (textBackingPixmap == None)
      {
         sprintf(gszMsgBox, "Can not allocate pixmap of size %1dx%1d.",
               tmp_w, tmp_h);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return;
      }
      textBackingPixmapW = tmp_w;
      textBackingPixmapH = tmp_h;
   }
   watch_cursor = watchCursorOnMainWindow;
   if (!watch_cursor)
   {
      SetWatchCursor (drawWindow);
      SetWatchCursor (mainWindow);
   }

   XSetForeground (mainDisplay, rotateGC, 0);
   XFillRectangle (mainDisplay,textBackingPixmap,rotateGC,0,0,w,h);

   values.foreground = 1;
   values.font = canvasFontPtr->fid;
   XChangeGC (mainDisplay, rotateGC, GCForeground | GCFont, &values);

   y = 0;
   yinc = textCursorH+textVSpace;
   for (s_ptr = text_ptr->first; s_ptr != NULL; s_ptr = s_ptr->next)
   {
      int	x=0, str_w;

      str_w = XTextWidth (canvasFontPtr, s_ptr->dyn_str.s,
            s_ptr->dyn_str.sz-1);
      switch (textJust)
      {
         case JUST_L: x = (-min_lbearing); break;
         case JUST_C:
            x = ((w+min_lbearing-max_rextra-str_w)>>1)-min_lbearing; break;
         case JUST_R: x = w-str_w-max_rextra; break;
      }
      XDrawString (mainDisplay, textBackingPixmap, rotateGC,
            x, y+canvasFontAsc, s_ptr->dyn_str.s, s_ptr->dyn_str.sz-1);
      y += yinc;
   }
   src_image = XGetImage (mainDisplay,textBackingPixmap,0,0,w,h,1,ZPixmap);

   if (ObjPtr->ctm == NULL)
   {
      switch (rotate)
      {
         case ROTATE0:
         case ROTATE180:
            if (text_ptr->read_only)
            {
               num_cols=ObjPtr->bbox.rbx - ObjPtr->bbox.ltx;
               num_rows=ObjPtr->bbox.rby - ObjPtr->bbox.lty;
            }
            else
            {
               num_cols=w; num_rows=h;
            }
            break;

         case ROTATE90:
         case ROTATE270:
            if (text_ptr->read_only)
            {
               num_cols=ObjPtr->bbox.rbx - ObjPtr->bbox.ltx;
               num_rows=ObjPtr->bbox.rby - ObjPtr->bbox.lty;
            }
            else
            {
               num_cols=h; num_rows=w;
            }
            break;
      }
      if (zoomedIn || zoomScale == 0)
      {
         num_cols <<= zoomScale;
         num_rows <<= zoomScale;
      }
      else
      {
         num_cols >>= zoomScale;
         num_rows >>= zoomScale;
      }
   }
   else 
   {
      num_cols = ZOOMED_SIZE(ObjPtr->bbox.rbx - ObjPtr->bbox.ltx - 2);
      num_rows = ZOOMED_SIZE(ObjPtr->bbox.rby - ObjPtr->bbox.lty - 2);
   }
   if (num_cols == 0)
   {
      skinny = TRUE;
      num_cols = 1;
   }
   if (num_rows == 0)
   {
      flat = TRUE;
      num_rows = 1;
   }

   do_msg = ((num_rows*num_cols)>=0x4000);
   if (do_msg) {
      SaveStatusStrings();
      SetStringStatus("Caching text bitmap...");
      XSync(mainDisplay, False);
   }

   dest_bitmap = XCreatePixmap (mainDisplay,mainWindow,num_cols,num_rows,1);
   XSetForeground (mainDisplay, rotateGC, 0);
   XFillRectangle (mainDisplay,dest_bitmap,rotateGC,0,0,num_cols,num_rows);
   dest_image = XGetImage (mainDisplay, dest_bitmap, 0, 0, num_cols,
         num_rows, 1, ZPixmap);

   if (!flat && !skinny)
   {
      if (ObjPtr->ctm == NULL)
      {
         mtrx.image_w = (float)w;
         mtrx.image_h = (float)h;
         mtrx.w = (float)num_cols;
         mtrx.h = (float)num_rows;
         mtrx.rotate = rotate;
         mtrx.flip = NO_FLIP;

         CalcTransform (&mtrx);

         start_col = (mtrx.transformed_w >= 0.0) ? 0 : (-num_cols)+1;
         start_row = (mtrx.transformed_h >= 0.0) ? 0 : (-num_rows)+1;

         for (r = 0; r < num_rows; r++)
         {
            float	part_x, part_y;

            if (do_msg && ((r & 0xf) == 0)) {
               int percent=(r*10000/num_rows)/100;

               sprintf(gszMsgBox, "Progress: %1d%%", percent);
               SetStringStatus(gszMsgBox);
               XSync(mainDisplay, False);
            }
            part_x = (r+start_row)*mtrx.rev_m[1][0];
            part_y = (r+start_row)*mtrx.rev_m[1][1];
            for (c = 0; c < num_cols; c++)
            {
               int	x, y;

               x = (int)((c+start_col)*mtrx.rev_m[0][0]+part_x);
               y = (int)((c+start_col)*mtrx.rev_m[0][1]+part_y);
               if (x>=0 && x<w && y>=0 && y<h && XGetPixel(src_image,x,y)==1)
                  XPutPixel (dest_image, c, r, 1);
            }
         }
      }
      else
      {
         int	abs_offset_x=ObjPtr->bbox.ltx+1-ObjPtr->x;
         int	abs_offset_y=ObjPtr->bbox.lty+1-ObjPtr->y;

         for (r = 0; r < num_rows; r++)
         {
            int	y=abs_offset_y+ABS_SIZE(r);

            if (do_msg && ((r & 0xf) == 0)) {
               int percent=(r*10000/num_rows)/100;

               sprintf(gszMsgBox, "Progress: %1d%%", percent);
               SetStringStatus(gszMsgBox);
               XSync(mainDisplay, False);
            }
            for (c = 0; c < num_cols; c++)
            {
               int	x=abs_offset_x+ABS_SIZE(c);
               int	new_x, new_y;
               
               ReverseTransformPointThroughCTM (x, y, ObjPtr->ctm, &new_x,
                     &new_y);
               new_x += ObjPtr->x-ObjPtr->detail.t->orig_bbox.ltx;
               new_y += ObjPtr->y-ObjPtr->detail.t->orig_bbox.lty;
               if (new_x>=0 && new_x<w && new_y>=0 && new_y<h &&
                     XGetPixel(src_image,new_x,new_y)==1)
                  XPutPixel (dest_image, c, r, 1);
            }
         }
         memcpy (&text_ptr->cached_ctm,ObjPtr->ctm,sizeof(struct XfrmMtrxRec));
      }
   }
   if (do_msg) {
      SetStringStatus("Finishing caching text bitmap...");
      XSync(mainDisplay, False);
   }
   XPutImage (mainDisplay, dest_bitmap, rotateGC, dest_image, 0, 0, 0, 0,
         num_cols, num_rows);
   if (do_msg) RestoreStatusStrings();

   text_ptr->cached_bitmap = dest_bitmap;
   text_ptr->cached_zoomed = zoomedIn;
   text_ptr->cached_zoom = zoomScale;
   text_ptr->cached_rotate = rotate;
   XDestroyImage (src_image);
   XDestroyImage (dest_image);

   if (!watch_cursor)
   {
      SetDefaultCursor (mainWindow);
      ShowCursor ();
   }
}

static
struct ObjRec *SubFindTextObj(ObjPtr, XOff, YOff, obj_to_be_found)
   struct ObjRec *ObjPtr, *obj_to_be_found;
   int XOff, YOff;
{
   register struct ObjRec *obj_ptr;
   register struct AttrRec *attr_ptr;
   struct ObjRec *found_text_obj;
   struct SelRec *sel_ptr;

   for (obj_ptr=ObjPtr->detail.r->first; obj_ptr!=NULL; obj_ptr=obj_ptr->next) {
      obj_ptr->tmp_parent = ObjPtr;
      if (obj_to_be_found == NULL) {
         if (obj_ptr->type == OBJ_TEXT &&
               XOff >= OFFSET_X(obj_ptr->bbox.ltx)-3 &&
               YOff >= OFFSET_Y(obj_ptr->bbox.lty)-3 &&
               XOff <= OFFSET_X(obj_ptr->bbox.rbx)+3 &&
               YOff <= OFFSET_Y(obj_ptr->bbox.rby)+3) {
            return (obj_ptr);
         } else {
            attr_ptr = obj_ptr->fattr;
            for (; attr_ptr != NULL;  attr_ptr = attr_ptr->next) {
               if (XOff>=OFFSET_X(attr_ptr->obj->bbox.ltx)-3 &&
                     YOff>=OFFSET_Y(attr_ptr->obj->bbox.lty)-3 &&
                     XOff<=OFFSET_X(attr_ptr->obj->bbox.rbx)+3 &&
                     YOff<=OFFSET_Y(attr_ptr->obj->bbox.rby)+3 &&
                     attr_ptr->shown == TRUE) {
                  return (attr_ptr->obj);
               }
            }
            switch (obj_ptr->type) {
            case OBJ_GROUP:
            case OBJ_ICON:
            case OBJ_SYM: break;
            default: continue;
            }
            if (XOff >= OFFSET_X(obj_ptr->bbox.ltx)-3 &&
                  YOff >= OFFSET_Y(obj_ptr->bbox.lty)-3 &&
                  XOff <= OFFSET_X(obj_ptr->bbox.rbx)+3 &&
                  YOff <= OFFSET_Y(obj_ptr->bbox.rby)+3) {
               if ((found_text_obj=SubFindTextObj(obj_ptr, XOff, YOff,
                     obj_to_be_found)) != NULL) {
                  sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
                  if (sel_ptr == NULL) FailAllocMessage();
                  sel_ptr->obj = obj_ptr;
                  sel_ptr->prev = NULL;
                  sel_ptr->next = outerSel;
                  if (outerSel == NULL) {
                     innerSel = sel_ptr;
                  } else {
                     outerSel->prev = sel_ptr;
                  }
                  outerSel = sel_ptr;
                  return (found_text_obj);
               }
            }
         }
      } else {
         if (obj_ptr == obj_to_be_found) {
            return (obj_ptr);
         } else {
            attr_ptr = obj_ptr->fattr;
            for (; attr_ptr != NULL;  attr_ptr = attr_ptr->next) {
               if (attr_ptr->obj == obj_to_be_found) {
                  return (attr_ptr->obj);
               }
            }
            switch (obj_ptr->type) {
            case OBJ_GROUP:
            case OBJ_ICON:
            case OBJ_SYM: break;
            default: continue;
            }
            if ((found_text_obj=SubFindTextObj(obj_ptr, XOff, YOff,
                  obj_to_be_found)) != NULL) {
               sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
               if (sel_ptr == NULL) FailAllocMessage();
               sel_ptr->obj = obj_ptr;
               sel_ptr->prev = NULL;
               sel_ptr->next = outerSel;
               if (outerSel == NULL) {
                  innerSel = sel_ptr;
               } else {
                  outerSel->prev = sel_ptr;
               }
               outerSel = sel_ptr;
               return (found_text_obj);
            }
         }
      }
   }
   return NULL;
}

static
void CleanOuterInnerSel()
{
   register struct SelRec *sel_ptr, *next_sel;

   if (outerSel != NULL) {
      for (sel_ptr = outerSel; sel_ptr != NULL; sel_ptr = next_sel) {
         next_sel = sel_ptr->next;
         free(sel_ptr);
      }
      outerSel = innerSel = NULL;
   }
}

struct ObjRec *FindTextObj(XOff, YOff, obj_to_be_found)
   int XOff, YOff;
   struct ObjRec *obj_to_be_found;
   /* XOff and YOff are screen offsets */
{
   register struct ObjRec *obj_ptr;
   register struct AttrRec *attr_ptr;
   struct ObjRec *found_text_obj;
   struct SelRec *sel_ptr;

   CleanOuterInnerSel();

   for (obj_ptr = topObj; obj_ptr != NULL; obj_ptr = obj_ptr->next) {
      obj_ptr->tmp_parent = NULL;
      if (colorLayers && !ObjInVisibleLayer(obj_ptr)) {
         continue;
      }
      if (obj_to_be_found == NULL) {
         if (obj_ptr->type == OBJ_TEXT &&
               XOff >= OFFSET_X(obj_ptr->bbox.ltx)-3 &&
               YOff >= OFFSET_Y(obj_ptr->bbox.lty)-3 &&
               XOff <= OFFSET_X(obj_ptr->bbox.rbx)+3 &&
               YOff <= OFFSET_Y(obj_ptr->bbox.rby)+3) {
            return (obj_ptr);
         } else {
            attr_ptr = obj_ptr->fattr;
            for (; attr_ptr != NULL;  attr_ptr = attr_ptr->next) {
               if (XOff>=OFFSET_X(attr_ptr->obj->bbox.ltx)-3 &&
                     YOff>=OFFSET_Y(attr_ptr->obj->bbox.lty)-3 &&
                     XOff<=OFFSET_X(attr_ptr->obj->bbox.rbx)+3 &&
                     YOff<=OFFSET_Y(attr_ptr->obj->bbox.rby)+3 &&
                     attr_ptr->shown == TRUE) {
                  return (attr_ptr->obj);
               }
            }
            if (!groupedTextEditable) continue;

            switch (obj_ptr->type) {
            case OBJ_GROUP:
            case OBJ_ICON:
            case OBJ_SYM: break;
            default: continue;
            }
            if (XOff >= OFFSET_X(obj_ptr->bbox.ltx)-3 &&
                  YOff >= OFFSET_Y(obj_ptr->bbox.lty)-3 &&
                  XOff <= OFFSET_X(obj_ptr->bbox.rbx)+3 &&
                  YOff <= OFFSET_Y(obj_ptr->bbox.rby)+3) {
               if (colorLayers && !ObjInVisibleLayer(obj_ptr)) {
               } else if ((found_text_obj=SubFindTextObj(obj_ptr, XOff, YOff,
                     obj_to_be_found)) != NULL) {
                  sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
                  if (sel_ptr == NULL) FailAllocMessage();
                  sel_ptr->obj = obj_ptr;
                  sel_ptr->prev = NULL;
                  sel_ptr->next = outerSel;
                  if (outerSel == NULL) {
                     innerSel = sel_ptr;
                  } else {
                     outerSel->prev = sel_ptr;
                  }
                  outerSel = sel_ptr;
                  return (found_text_obj);
               }
            }
         }
      } else {
         if (obj_ptr == obj_to_be_found) {
            return (obj_ptr);
         } else {
            attr_ptr = obj_ptr->fattr;
            for (; attr_ptr != NULL;  attr_ptr = attr_ptr->next) {
               if (attr_ptr->obj == obj_to_be_found) {
                  return (attr_ptr->obj);
               }
            }
            if (!groupedTextEditable) continue;

            switch (obj_ptr->type) {
            case OBJ_GROUP:
            case OBJ_ICON:
            case OBJ_SYM: break;
            default: continue;
            }
            if (colorLayers && !ObjInVisibleLayer(obj_ptr)) {
            } else if ((found_text_obj=SubFindTextObj(obj_ptr, XOff, YOff,
                  obj_to_be_found)) != NULL) {
               sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
               if (sel_ptr == NULL) FailAllocMessage();
               sel_ptr->obj = obj_ptr;
               sel_ptr->prev = NULL;
               sel_ptr->next = outerSel;
               if (outerSel == NULL) {
                  innerSel = sel_ptr;
               } else {
                  outerSel->prev = sel_ptr;
               }
               outerSel = sel_ptr;
               return (found_text_obj);
            }
         }
      }
   }
   return (NULL);
}

void UnlinkCurTextFromInnerSel ()
{
   register struct GroupRec	* group_obj=innerSel->obj->detail.r;

   if (group_obj->first == group_obj->last)
   {
      if (outerSel != innerSel || innerSel->obj->fattr != NULL)
      {
         Msg ("Error!   The text object within a composite object which you");
         Msg ("    have just erased caused all ancestors to be deleted.");
         Msg ("    You may see ghost images now!");
         Msg ("    Please undo and try ungroup the ancestor object first");
         Msg ("    before erasing the text.");
         fprintf (stderr, "%s\n%s\n%s\n%s\n%s\n",
               "Error!   The text object within a composite object which you",
               "    have just erased caused all ancestors to be deleted.",
               "    You may see ghost images now!",
               "    Please undo and try ungroup the ancestor object first",
               "    before erasing the text.");
      }
      DelObj (outerSel->obj);
      CleanOuterInnerSel ();
      curTextObj = NULL;
      return;
   }
   else if (curTextObj == group_obj->first)
   {
      curTextObj->next->prev = NULL;
      group_obj->first = curTextObj->next;
   }
   else if (curTextObj == group_obj->last)
   {
      curTextObj->prev->next = NULL;
      group_obj->last = curTextObj->prev;
   }
   else
   {
      curTextObj->prev->next = curTextObj->next;
      curTextObj->next->prev = curTextObj->prev;
   }
   curTextObj->prev = curTextObj->next = NULL;
}

void AdjAncestorsBBox ()
{
   register struct SelRec	* sel_ptr;

   for (sel_ptr = innerSel; sel_ptr != NULL; sel_ptr = sel_ptr->prev)
      AdjObjBBox (sel_ptr->obj);
}

static
void DumpTextPath (FP, ObjPtr, TextPtr)
   FILE				* FP;
   register struct ObjRec	* ObjPtr;
   register struct TextRec	* TextPtr;
{
   int			x, y, xinc = 0, yinc = 0;
   struct StrRec	* s_ptr;

   x = ObjPtr->x;
   y = ObjPtr->y;

   fprintf (FP, "   gsave\n");
   if (ObjPtr->ctm != NULL) {
      float m[6];

      m[CTM_SX] = ((float)ObjPtr->ctm->m[CTM_SX])/((float)1000.0);
      m[CTM_SY] = ((float)ObjPtr->ctm->m[CTM_SY])/((float)1000.0);
      m[CTM_SIN] = ((float)ObjPtr->ctm->m[CTM_SIN])/((float)1000.0);
      m[CTM_MSIN] = ((float)ObjPtr->ctm->m[CTM_MSIN])/((float)1000.0);
      fprintf (FP, "      %1d %1d translate\n", ObjPtr->x, ObjPtr->y);
      fprintf (FP, "      [%.3f %.3f %.3f %.3f %1d %1d] concat\n",
            m[CTM_SX], m[CTM_SIN], m[CTM_MSIN], m[CTM_SY],
            ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY]);
      x = y = 0;
   }
   switch (penPat)
   {
      case SOLIDPAT: break;
      case BACKPAT: fprintf (FP, "      1 setgray\n"); break;
      default:
         if (!colorDump && useGray)
         {
            GrayCheck (penPat);
            fprintf (FP, "      %s setgray\n", GrayStr(penPat));
         }
         break;
   }

   switch (curRotate)
   {
      case ROTATE0: xinc = 0; yinc = textCursorH+textVSpace; break;
      case ROTATE90: xinc = -(textCursorH+textVSpace); yinc = 0; break;
      case ROTATE180: xinc = 0; yinc = -(textCursorH+textVSpace); break;
      case ROTATE270: xinc = textCursorH+textVSpace; yinc = 0; break;
   }

   for (s_ptr = TextPtr->first; s_ptr != NULL; s_ptr = s_ptr->next)
   {
      switch (curRotate)
      {
         case ROTATE0:
            fprintf (FP, "      %1d %1d moveto (", x, y+canvasFontAsc);
            break;
         case ROTATE90:
            fprintf (FP, "      %1d %1d moveto 90 rotate (",x-canvasFontAsc,y);
            break;
         case ROTATE180:
            fprintf (FP, "      %1d %1d moveto 180 rotate (",x,y-canvasFontAsc);
            break;
         case ROTATE270:
            fprintf (FP, "      %1d %1d moveto 270 rotate (",x+canvasFontAsc,y);
            break;
      }
      DumpOneStr (FP, curFont, s_ptr->dyn_str.s);
      if ((colorDump || !useGray) && penPat > BACKPAT)
      {
         switch (textJust)
         {
            case JUST_L: fprintf(FP,") "); break;
            case JUST_C:
               if (preDumpSetup) PSUseCenterText();
               fprintf(FP,") tgifcentertext ");
               break;
            case JUST_R:
               if (preDumpSetup) PSUseRightText();
               fprintf(FP,") tgifrighttext ");
               break;
         }
         fprintf(FP,"true charpath clip newpath\n");
      }
      else
      {
         switch (textJust)
         {
            case JUST_L: fprintf (FP, ") show\n"); break;
            case JUST_C:
               if (preDumpSetup) PSUseCenterText();
               fprintf (FP, ") tgifcentertext show\n");
               break;
            case JUST_R:
               if (preDumpSetup) PSUseRightText();
               fprintf (FP, ") tgifrighttext show\n");
               break;
         }
      }
      switch (curRotate)
      {
         case ROTATE0: break;
         case ROTATE90: fprintf (FP, "      -90 rotate \n"); break;
         case ROTATE180: fprintf (FP, "      -180 rotate \n"); break;
         case ROTATE270: fprintf (FP, "      -270 rotate \n"); break;
      }
      if ((colorDump || !useGray) && penPat > BACKPAT)
      {
         if (preDumpSetup) PSUseColorPattern();
         DumpPatFill (FP, penPat, 8, ObjPtr->bbox, "      ");
      }
      if (penPat != SOLIDPAT && penPat != BACKPAT && s_ptr->next != NULL &&
            (colorDump || !useGray))
      {
         fprintf (FP, "   grestore\n");
         fprintf (FP, "   gsave\n");
      }
      x += xinc;
      y += yinc;
   }
   fprintf (FP, "   grestore\n");
}

void DumpTextObj (FP, ObjPtr)
   FILE				* FP;
   register struct ObjRec	* ObjPtr;
{
   int			color_index, fill;
   int			ltx, lty, rbx, rby;
   char			font_str[MAXSTRING];
   struct TextRec	* text_ptr = ObjPtr->detail.t;

   if (text_ptr->pen == NONEPAT && text_ptr->fill == NONEPAT) return;

   fprintf (FP, "%% TEXT\n");
   if (!PRTGIF) SaveCurFont ();

   curFont = text_ptr->font;
   curStyle = text_ptr->style;
   curSize = text_ptr->size;
   textJust = text_ptr->just;
   textVSpace = text_ptr->v_space;
   curRotate = text_ptr->rotate;
   penPat = text_ptr->pen;

   fill = text_ptr->fill;

   if (PRTGIF || text_ptr->read_only)
   {
      canvasFontAsc = text_ptr->asc;
      canvasFontDes = text_ptr->des;
      textCursorH = canvasFontAsc + canvasFontDes;
   }
   else
      SetCanvasFont ();

   color_index = ObjPtr->color;
   DumpRGBColorLine(FP, color_index, 0, TRUE);

   ltx = ObjPtr->bbox.ltx;
   lty = ObjPtr->bbox.lty;
   rbx = ObjPtr->bbox.rbx-1;
   rby = ObjPtr->bbox.rby-1;

   switch (fill)
   {
      case NONEPAT: break;
      case SOLIDPAT:
         fprintf (FP, "newpath\n");
         fprintf (FP, "   %1d %1d moveto ", ltx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, rby);
         fprintf (FP, "%1d %1d lineto\n", ltx, rby);
         fprintf (FP, "closepath fill\n");
         break;
      case BACKPAT:
         fprintf (FP, "newpath\n");
         fprintf (FP, "   %1d %1d moveto ", ltx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, rby);
         fprintf (FP, "%1d %1d lineto\n", ltx, rby);
         fprintf (FP, "closepath 1 setgray fill\n");
         DumpRGBColorLine(FP, color_index, 0, TRUE);
         break;
      default:
         /* patterned */
         fprintf (FP, "gsave\n");
         if (colorDump || !useGray)
         {
            fprintf (FP, "   newpath\n");
            fprintf (FP, "      %1d %1d moveto ", ltx, lty);
            fprintf (FP, "%1d %1d lineto ", rbx, lty);
            fprintf (FP, "%1d %1d lineto ", rbx, rby);
            fprintf (FP, "%1d %1d lineto\n", ltx, rby);
            fprintf (FP, "   closepath 1 setgray fill\n");
            DumpRGBColorLine(FP, color_index, 3, TRUE);
         }
         else
         {
            GrayCheck (fill);
            fprintf (FP, "      %s setgray\n", GrayStr(fill));
         }
         fprintf (FP, "   newpath\n");
         fprintf (FP, "      %1d %1d moveto ", ltx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, rby);
         fprintf (FP, "%1d %1d lineto\n", ltx, rby);
         if (colorDump || !useGray)
         {
            if (preDumpSetup) PSUseColorPattern();
            fprintf (FP, "   closepath eoclip newpath\n");
            DumpPatFill (FP, fill, 8, ObjPtr->bbox, "   ");
         }
         else
            fprintf (FP, "   closepath fill\n");
         fprintf (FP, "grestore\n");
         break;
   }
   if (penPat == NONEPAT)
   {
      fprintf (FP, "\n");
      if (!PRTGIF) RestoreCurFont ();
      return;
   }
   if ((colorDump || !useGray) && penPat>BACKPAT && curFont==FONT_COU)
   {
      if (PRTGIF)
         fprintf (stderr, "Warning:  %s  %s\n",
               "Printing Courier fonts with non-solid pen in color.",
               "May cause error!");
      else
      {
         sprintf (gszMsgBox, "Warning:  %s",
               "Printing Courier fonts with non-solid pen in color.");
         TwoLineMsg (gszMsgBox, "    May cause error!");
      }
   }

   if (PRTGIF && text_ptr->font_name != NULL)
      sprintf (font_str, "/%s", text_ptr->font_name);
   else
      GetPSFontStr (curFont, curStyle, font_str);
   fprintf (FP, "%s", font_str); 

   if (NeedEncode (curFont, curStyle))
      fprintf (FP, "-8 "); 
   else
      fprintf (FP, " "); 

   UpdateDocumentFonts (&font_str[1]);

   fprintf (FP, "findfont [%1d 0 0 -%1d 0 0] makefont setfont\n",
         curSize, curSize);

   if ((colorDump || !useGray) && penPat > BACKPAT)
   {
      int tmp_pen = penPat;

      penPat = BACKPAT;
      DumpTextPath (FP, ObjPtr, text_ptr);
      DumpRGBColorLine(FP, color_index, 3, TRUE);
      penPat = tmp_pen;
   }
   DumpTextPath (FP, ObjPtr, text_ptr);

   fprintf (FP, "\n");
   if (!PRTGIF) RestoreCurFont ();
}

int NeedsToCacheTextObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register struct TextRec       * text_ptr = ObjPtr->detail.t;

   return (ObjPtr->ctm != NULL || zoomScale != 0 ||
         text_ptr->rotate != ROTATE0 || text_ptr->read_only
   );
}

void SaveString (FP, S)
   FILE			* FP;
   register char	* S;
{
   for ( ; *S != '\0'; S++)
   {
      if (*S == '\\')
      {
         if (fprintf (FP, "%s", "\\\\") == EOF) writeFileFailed = TRUE;
      }
      else if (*S == '"')
      {
         if (doubleQuoteDoubleQuote)
         {
            if (fprintf (FP, "%s", "\"\"") == EOF) writeFileFailed = TRUE;
         }
         else
         {
            if (fprintf (FP, "%s", "\\\"") == EOF) writeFileFailed = TRUE;
         }
      }
      else if ((*S) & 0x80)
      {
         if (fprintf (FP, "\\%o", (*S)&0xff) == EOF) writeFileFailed = TRUE;
      }
      else
         if (fputc (*S, FP) == EOF) writeFileFailed = TRUE;
   }
}

void SaveTextObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{
   register struct TextRec	* text_ptr = ObjPtr->detail.t;
   register struct StrRec	* s_ptr;
   char				font_str[MAXSTRING];
   int				compressed=FALSE;

   GetPSFontStr (text_ptr->font, text_ptr->style, font_str);
         /* font_str starts with the '/' character */
   if (fprintf (FP, "text('%s',", colorMenuItems[ObjPtr->color]) == EOF)
      writeFileFailed = TRUE;
   if (fprintf (FP,
         "%1d,%1d,'%s',%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,\"%s\",%1d,%1d,%1d,[\n",
         ObjPtr->x, ObjPtr->y, &font_str[1], text_ptr->style, text_ptr->size,
         text_ptr->lines, text_ptr->just, text_ptr->rotate, text_ptr->pen,
         ObjPtr->obbox.rbx-ObjPtr->obbox.ltx,
         ObjPtr->obbox.rby-ObjPtr->obbox.lty, ObjPtr->id, 0, /* dummy dpi */
         text_ptr->asc, text_ptr->des, text_ptr->fill, text_ptr->v_space,
         ObjPtr->rotation, ObjPtr->locked, text_ptr->underline_on,
         text_ptr->underline, text_ptr->min_lbearing, text_ptr->max_rextra,
         text_ptr->double_byte, text_ptr->direction,
         (text_ptr->custom_screen_font_name == NULL ?
         "" : text_ptr->custom_screen_font_name),
         compressed, ObjPtr->ctm!=NULL, ObjPtr->invisible) == EOF) {
      writeFileFailed = TRUE;
   }
   if (ObjPtr->ctm != NULL && fprintf(FP,
         "\t%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d],[\n",
         ObjPtr->x, ObjPtr->y,
         ObjPtr->orig_obbox.ltx, ObjPtr->orig_obbox.lty,
         ObjPtr->orig_obbox.rbx, ObjPtr->orig_obbox.rby,
         ObjPtr->ctm->m[CTM_SX], ObjPtr->ctm->m[CTM_SIN],
         ObjPtr->ctm->m[CTM_MSIN], ObjPtr->ctm->m[CTM_SY],
         ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY],
         text_ptr->orig_bbox.ltx, text_ptr->orig_bbox.lty,
         text_ptr->orig_bbox.rbx, text_ptr->orig_bbox.rby) == EOF) {
      writeFileFailed = TRUE;
   }
   for (s_ptr = text_ptr->first; s_ptr->next != NULL; s_ptr = s_ptr->next)
   {
      if (fprintf (FP, "\t\"") == EOF) writeFileFailed = TRUE;
      SaveString (FP, s_ptr->dyn_str.s);
      if (fprintf (FP, "\",\n") == EOF) writeFileFailed = TRUE;
   }

   if (fprintf (FP, "\t\"") == EOF) writeFileFailed = TRUE;
   SaveString (FP, s_ptr->dyn_str.s);
   if (fprintf (FP, "\"])") == EOF) writeFileFailed = TRUE;
}

char * ReadString (Str)
   char	*	Str;
{
   register char	* s;

   for (s = Str; *s != '\0'; s++)
      if (*s == '"')
      {
         if (s[1] == '"')
            strcpy (s, s+1);
         else
            break;
      }
      else if (*s == '\\')
      {
         if (s[1] >= '0' && s[1] <= '3')
         {
            if (s[2] >= '0' && s[2] <= '7' && s[3] >= '0' && s[3] <= '7')
            {
               *s = (char)(((s[1]-'0')<<6)+((s[2]-'0')<<3)+(s[3]-'0'));
               strcpy (s+1, s+4);
            }
            else
            {
               if (PRTGIF)
                  fprintf (stderr, "Bad octal string \\%c%c%c encountered.\n",
                        s[1], s[2], s[3]);
               else
               {
                  sprintf (gszMsgBox,
                        "Bad octal string \\%c%c%c encountered.\n",
                        s[1], s[2], s[3]);
                  Msg (gszMsgBox);
               }
               strcpy (s, s+1);
            }
         }
         else
            strcpy (s, s+1);
      }

   if (*s == '"') s++;
   return (s);
}

struct ObjRec *FormTextObjectFromFile(FP, AbsX, AbsY)
   FILE *FP;
   int AbsX, AbsY;
{
   struct StrRec *first_str, *last_str, *str_ptr;
   struct ObjRec *obj_ptr;
   struct TextRec *text_ptr;
   int num_lines=0, len, max_len=0, min_lbearing=0, max_rextra=0;

   text_ptr = (struct TextRec *)malloc(sizeof(struct TextRec));
   if (text_ptr == NULL) FailAllocMessage();
   memset(text_ptr, 0, sizeof(struct TextRec));

   CopyCurInfoToTextPtr(text_ptr);

   first_str = last_str = NULL;
   if (FP != NULL) {
      char *buf;

      while ((buf=UtilGetALine(FP)) != NULL) {
         int w, lbearing, rextra;

         num_lines++;
         str_ptr = NewStr();
         if (str_ptr == NULL) {
            FailAllocMessage();
            break;
         }
         DynStrSet(&str_ptr->dyn_str, buf);

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
         free(buf);
      }
   }
   if (num_lines == 0) {
      int w, lbearing, rextra;

      num_lines++;
      str_ptr = NewStr();
      if (str_ptr == NULL) FailAllocMessage();

      str_ptr->prev = last_str;
      str_ptr->next = NULL;
      first_str = last_str = str_ptr;

      GetStrSizeInfo(str_ptr, &w, &lbearing, &rextra);
      if (w > max_len) max_len = w;
      if (lbearing < min_lbearing) min_lbearing = lbearing;
      if (rextra > max_rextra) max_rextra = rextra;
   }
   text_ptr->lines = num_lines;
   text_ptr->first = first_str;
   text_ptr->last = last_str;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));
   obj_ptr->x = AbsX;
   obj_ptr->y = AbsY;
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
   return obj_ptr;
}

static
int PaintLeftText (Str, Just, Rotate, LtX, LtY)
   char	* Str;
   int	Just, Rotate, LtX, LtY;
   /* LtX and LtY are UNSCALED screen offset */
{
   register int	amount;

   if (zoomScale != 0)
   {
      LtX = ZOOMED_SIZE(LtX);
      LtY = ZOOMED_SIZE(LtY);
      amount = XTextWidth (canvasFontPtr, Str, strlen (Str));
      BlurText (drawWindow, drawGC, LtX, LtY,
            (zoomedIn ? ((amount<<zoomScale)+1) : (amount>>zoomScale)+1),
            (zoomedIn ? (textCursorH<<zoomScale)+1 :
                        (textCursorH>>zoomScale)+1));
      return (amount);
   }

   LtY += canvasFontAsc;
   amount = XTextWidth (canvasFontPtr, Str, strlen (Str));
   XDrawString (mainDisplay, drawWindow, drawGC, LtX, LtY, Str, strlen (Str));

   return (amount); /* return the length of the painted string */
}

void RepaintFirstStr (ObjPtr, Str)
   struct ObjRec	* ObjPtr;
   char			* Str;
   /* Replace (graphically) the FIRST string of the text in ObjPtr by Str */
{
   register char	* s = ObjPtr->detail.t->first->dyn_str.s, * s1 = Str;
   char			tmp_str[MAXSTRING+1], * c_ptr;
   int			len;
   struct BBRec		bbox;
   XGCValues		values;

   bbox.ltx = ObjPtr->obbox.ltx; bbox.lty = ObjPtr->obbox.lty;
   bbox.rbx = ObjPtr->obbox.rbx; bbox.rby = ObjPtr->obbox.rby;

   c_ptr = tmp_str;
   for ( ; *s != '\0' && *s1 != '\0' && *s1 == *s; *c_ptr++ = *s++, s1++) ;

   if (*s == *s1) return; /* no updates */
   ObjPtr->detail.t->attr->owner->dirty = TRUE;
   *c_ptr = '\0';

   SaveCurFont ();
   curFont = ObjPtr->detail.t->font;
   curStyle = ObjPtr->detail.t->style;
   curSize = ObjPtr->detail.t->size;
   textJust = ObjPtr->detail.t->just;
   textVSpace = ObjPtr->detail.t->v_space;
   curRotate = ObjPtr->detail.t->rotate;
   SetCanvasFont ();

   if (*s != '\0')
   {
      values.foreground = myBgPixel;
      values.function = GXcopy;
      values.fill_style = FillSolid;
      XChangeGC (mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle, &values);

      len = XTextWidth (canvasFontPtr, tmp_str, strlen (tmp_str));
      XFillRectangle (mainDisplay, drawWindow, drawGC, OFFSET_X(bbox.ltx+len),
            OFFSET_Y(bbox.lty),
            (zoomedIn ? ((bbox.rbx-bbox.ltx-len)<<zoomScale)+1 :
                        ((bbox.rbx-bbox.ltx-len)>>zoomScale)+1),
            (zoomedIn ? (textCursorH<<zoomScale)+1 :
                        (textCursorH>>zoomScale)+1));

      values.foreground = colorPixels[ObjPtr->color];
      XChangeGC (mainDisplay, drawGC, GCForeground, &values);
   }
   else
   {
      values.foreground = colorPixels[ObjPtr->color];
      values.function = GXcopy;
      values.fill_style = FillSolid;
      XChangeGC (mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle, &values);
   }

   ObjPtr->bbox.rbx = ObjPtr->obbox.rbx = bbox.ltx + PaintLeftText (Str,
         textJust, curRotate, bbox.ltx-drawOrigX, bbox.lty-drawOrigY);

   RestoreCurFont ();
}
