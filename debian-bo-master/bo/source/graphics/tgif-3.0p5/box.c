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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/box.c,v 3.0 1996/05/06 16:03:52 william Exp $";
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
#ifndef _NO_EXTERN
#include "box.e"
#endif
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "file.e"
#include "grid.e"
#include "mainloop.e"
#include "msg.e"
#include "obj.e"
#include "pattern.e"
#include "poly.e"
#include "ps.e"
#include "raster.e"
#include "ruler.e"
#include "select.e"
#include "setup.e"
#include "util.e"
#include "xpixmap.e"

int	boxDrawn = FALSE;

static XPoint bv[5];

void MyBox (window, gc, x1, y1, x2, y2)
   /* Hollow box, solid outline with width=1 */
   Window	window;
   GC		gc;
   int		x1, y1, x2, y2;
{
   bv[0].x = (short)x1; bv[0].y = (short)y1;
   bv[1].x = (short)x1; bv[1].y = (short)y2;
   bv[2].x = (short)x2; bv[2].y = (short)y2;
   bv[3].x = (short)x2; bv[3].y = (short)y1;
   bv[4].x = (short)x1; bv[4].y = (short)y1;
   XDrawLines (mainDisplay, window, gc, bv, 5, CoordModeOrigin);
}

static
void DumpBoxPath (FP, ObjPtr, LtX, LtY, RbX, RbY, Width, Pen, Dash)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
   int			LtX, LtY, RbX, RbY, Width, Pen, Dash;
{
   register int	i;

   fprintf(FP, "   gsave\n");
   if (!colorDump && useGray && Pen > BACKPAT) {
      GrayCheck(Pen);
      fprintf(FP, "      %s setgray\n", GrayStr(Pen));
   }
   fprintf(FP, "      newpath\n         %1d %1d moveto ", LtX, LtY);
   fprintf(FP, "%1d %1d lineto ", RbX, LtY);
   fprintf(FP, "%1d %1d lineto ", RbX, RbY);
   fprintf(FP, "%1d %1d lineto\n", LtX, RbY);
   fprintf(FP, "      closepath\n");

   if (ObjPtr->ctm != NULL) {
      fprintf(FP, "      tgiforigctm setmatrix\n");
   }
   if (Width != 1) fprintf(FP, "      %1d setlinewidth\n", Width);
   if (Dash != 0) {
      fprintf(FP, "      [");
      for (i = 0; i < dashListLength[Dash]-1; i++) {
         fprintf(FP, "%1d ", (int)(dashList[Dash][i]));
      }
      fprintf(FP, "%1d] 0 setdash\n",
            (int)(dashList[Dash][dashListLength[Dash]-1]));
   }

   switch (Pen) {
   case SOLIDPAT: fprintf(FP, "      stroke\n"); break;
   case BACKPAT: fprintf(FP, "      1 setgray stroke 0 setgray\n"); break;
   default:
      if (colorDump || !useGray) {
         if (preDumpSetup) PSUseColorPattern();
         fprintf(FP, "      flattenpath strokepath clip newpath\n");
         DumpPatFill(FP, Pen, 8, ObjPtr->bbox, "      ");
      } else {
         fprintf(FP, "      stroke\n");
      }
      break;
   }
   fprintf(FP, "   grestore\n");
}
 
void DumpBoxObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{
   register int	ltx, lty, rbx, rby;
   int		fill, width, pen, dash, color_index;

   if (ObjPtr->ctm == NULL) {
      ltx = ObjPtr->obbox.ltx;
      lty = ObjPtr->obbox.lty;
      rbx = ObjPtr->obbox.rbx;
      rby = ObjPtr->obbox.rby;
   } else {
      ltx = ObjPtr->orig_obbox.ltx;
      lty = ObjPtr->orig_obbox.lty;
      rbx = ObjPtr->orig_obbox.rbx;
      rby = ObjPtr->orig_obbox.rby;
   }
   fill = ObjPtr->detail.b->fill;
   pen = ObjPtr->detail.b->pen;
   width = ObjPtr->detail.b->width;
   dash = ObjPtr->detail.b->dash;

   if (fill == NONEPAT && pen == NONEPAT) return;

   fprintf (FP, "%% BOX\n");
   if (ObjPtr->ctm != NULL) {
      float m[6];

      fprintf(FP, "gsave\n");
      m[CTM_SX] = ((float)ObjPtr->ctm->m[CTM_SX])/((float)1000.0);
      m[CTM_SY] = ((float)ObjPtr->ctm->m[CTM_SY])/((float)1000.0);
      m[CTM_SIN] = ((float)ObjPtr->ctm->m[CTM_SIN])/((float)1000.0);
      m[CTM_MSIN] = ((float)ObjPtr->ctm->m[CTM_MSIN])/((float)1000.0);
      fprintf (FP, "   %1d %1d translate\n", ObjPtr->x, ObjPtr->y);
      fprintf (FP, "   [%.3f %.3f %.3f %.3f %1d %1d] concat\n",
            m[CTM_SX], m[CTM_SIN], m[CTM_MSIN], m[CTM_SY],
            ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY]);
      fprintf (FP, "   %1d neg %1d neg translate\n", ObjPtr->x, ObjPtr->y);
   }
   color_index = ObjPtr->color;
   DumpRGBColorLine(FP, color_index, 0, TRUE);

   switch (fill)
   {
      case NONEPAT: break;
      case SOLIDPAT:
         /* solid black object */
         fprintf (FP, "newpath\n   %1d %1d moveto ", ltx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, rby);
         fprintf (FP, "%1d %1d lineto\n", ltx, rby);
         fprintf (FP, "closepath fill\n");
         break;
      case BACKPAT:
         /* solid white object */
         fprintf (FP, "newpath\n   %1d %1d moveto ", ltx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, rby);
         fprintf (FP, "%1d %1d lineto\n", ltx, rby);
         fprintf (FP, "closepath 1 setgray fill\n");
         DumpRGBColorLine(FP, color_index, 3, TRUE);
         break;
      default:
         /* patterned */
         fprintf (FP, "gsave\n");
         if (colorDump || !useGray) {
            fprintf (FP, "   newpath\n   %1d %1d moveto ", ltx, lty);
            fprintf (FP, "%1d %1d lineto ", rbx, lty);
            fprintf (FP, "%1d %1d lineto ", rbx, rby);
            fprintf (FP, "%1d %1d lineto\n", ltx, rby);
            fprintf (FP, "   closepath 1 setgray fill\n");
            DumpRGBColorLine(FP, color_index, 3, TRUE);
         } else {
            GrayCheck (fill);
            fprintf (FP, "   %s setgray\n", GrayStr(fill));
         }
         fprintf (FP, "   newpath\n      %1d %1d moveto ", ltx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, rby);
         fprintf (FP, "%1d %1d lineto\n", ltx, rby);
         if (colorDump || !useGray) {
            if (preDumpSetup) PSUseColorPattern();
            fprintf (FP, "   closepath eoclip newpath\n");
            DumpPatFill (FP, fill, 8, ObjPtr->bbox, "   ");
         } else {
            fprintf (FP, "   closepath fill\n");
         }
         fprintf (FP, "grestore\n");
         break;
   }

   if (pen == NONEPAT) {
      if (ObjPtr->ctm != NULL) fprintf(FP, "grestore\n");
      fprintf (FP, "\n");
      return;
   }

   fprintf (FP, "gsave\n");
   fprintf (FP, "   10 setmiterlimit\n");

   if ((colorDump || !useGray) && pen > BACKPAT) {
      DumpBoxPath (FP, ObjPtr, ltx, lty, rbx, rby, width, BACKPAT, 0);
      DumpRGBColorLine(FP, color_index, 3, TRUE);
   }
   DumpBoxPath (FP, ObjPtr, ltx, lty, rbx, rby, width, pen, dash);

   fprintf (FP, "grestore\n");
   if (ObjPtr->ctm != NULL) fprintf(FP, "grestore\n");
   fprintf (FP, "\n");
}

void DrawBoxObj (win, XOff, YOff, ObjPtr)
   Window		win;
   int			XOff, YOff;
   struct ObjRec	* ObjPtr;
{
   struct BoxRec	* box_ptr = ObjPtr->detail.b;
   int			fill, pen, pixel, ltx, lty, rbx, rby, width, dash;
   int			real_x_off, real_y_off;
   XGCValues		values;

   pen = box_ptr->pen;
   fill = box_ptr->fill;
   width = box_ptr->width;
   dash = box_ptr->dash;
   pixel = colorPixels[ObjPtr->color];

   if (fill == 0 && pen == 0) return;

   real_x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   real_y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);
   ltx = ZOOMED_SIZE(ObjPtr->obbox.ltx - real_x_off);
   lty = ZOOMED_SIZE(ObjPtr->obbox.lty - real_y_off);
   rbx = ZOOMED_SIZE(ObjPtr->obbox.rbx - real_x_off);
   rby = ZOOMED_SIZE(ObjPtr->obbox.rby - real_y_off);

   if (fill != 0)
   {
      values.foreground = (fill == 2) ? myBgPixel : pixel;
      values.function = GXcopy;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[fill];
      XChangeGC (mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple, &values);
      if (ObjPtr->ctm != NULL)
         XFillPolygon (mainDisplay, win, drawGC, ObjPtr->rotated_obbox, 5,
               Convex, CoordModeOrigin);
      else
         XFillRectangle (mainDisplay, win, drawGC, ltx, lty, rbx-ltx, rby-lty);
   }

   if (pen != 0)
   {
      values.foreground = (pen == 2) ? myBgPixel : pixel;
      values.function = GXcopy;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[pen];
      values.line_width = ZOOMED_SIZE(width);
#ifdef NO_THIN_LINE
      if (values.line_width < 1) values.line_width = 1;
#endif
      values.join_style = JoinMiter;
      if (dash != 0)
      {
         XSetDashes (mainDisplay, drawGC, 0, dashList[dash],
               dashListLength[dash]);
         values.line_style = LineOnOffDash;
      }
      else
         values.line_style = LineSolid;
      XChangeGC (mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple | GCLineWidth |
            GCLineStyle | GCJoinStyle, &values);
      if (ObjPtr->ctm != NULL)
         XDrawLines (mainDisplay, win, drawGC, ObjPtr->rotated_obbox, 5,
               CoordModeOrigin);
      else
         XDrawRectangle (mainDisplay, win, drawGC, ltx, lty, rbx-ltx, rby-lty);
   }
}

int CreateBoxObj(X1, Y1, X2, Y2, CreateAbsolute)
   int X1, Y1, X2, Y2, CreateAbsolute;
{
   struct BoxRec *box_ptr;
   struct ObjRec *obj_ptr;
   int width, w, ltx, lty, rbx, rby;

   if (X1 == X2 && Y1 == Y2) return FALSE;
   box_ptr = (struct BoxRec *)malloc(sizeof(struct BoxRec));
   if (box_ptr == NULL) FailAllocMessage();
   memset(box_ptr, 0, sizeof(struct BoxRec));
   box_ptr->fill = objFill;
   box_ptr->width = width = curWidthOfLine[lineWidth];
   UtilStrCpy(box_ptr->width_spec, sizeof(box_ptr->width_spec),
         curWidthOfLineSpec[lineWidth]);
   box_ptr->pen = penPat;
   box_ptr->dash = curDash;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));

   if (X1 < X2) {
      if (Y1 < Y2) {
         ltx = X1; lty = Y1; rbx = X2; rby = Y2;
      } else {
         ltx = X1; lty = Y2; rbx = X2; rby = Y1;
      }
   } else {
      if (Y1 < Y2) {
         ltx = X2; lty = Y1; rbx = X1; rby = Y2;
      } else {
         ltx = X2; lty = Y2; rbx = X1; rby = Y1;
      }
   }
   if (CreateAbsolute) {
      obj_ptr->bbox.ltx = obj_ptr->obbox.ltx = obj_ptr->x = ltx;
      obj_ptr->bbox.lty = obj_ptr->obbox.lty = obj_ptr->y = lty;
      obj_ptr->bbox.rbx = obj_ptr->obbox.rbx = rbx;
      obj_ptr->bbox.rby = obj_ptr->obbox.rby = rby;
   } else {
      obj_ptr->bbox.ltx = obj_ptr->obbox.ltx = obj_ptr->x = ABS_X(ltx);
      obj_ptr->bbox.lty = obj_ptr->obbox.lty = obj_ptr->y = ABS_Y(lty);
      obj_ptr->bbox.rbx = obj_ptr->obbox.rbx = ABS_X(rbx);
      obj_ptr->bbox.rby = obj_ptr->obbox.rby = ABS_Y(rby);
   }
   w = HALF_W(width);
   obj_ptr->bbox.ltx -= w;
   obj_ptr->bbox.lty -= w;
   obj_ptr->bbox.rbx += w;
   obj_ptr->bbox.rby += w;
   obj_ptr->type = OBJ_BOX;
   obj_ptr->color = colorIndex;
   obj_ptr->id = objId++;
   obj_ptr->dirty = FALSE;
   obj_ptr->rotation = 0;
   obj_ptr->locked = FALSE;
   obj_ptr->detail.b = box_ptr;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;
   obj_ptr->invisible = FALSE;
   AddObj(NULL, topObj, obj_ptr);
   return TRUE;
}

static XComposeStatus	c_stat;
 
static
void ContinueBox (OrigX, OrigY)
   int 	OrigX, OrigY;
{
   int 			end_x, end_y, grid_x, grid_y, saved_x, saved_y;
   int 			done=FALSE, abort=FALSE;
   int			xor_pixel;
   char			buf[80], w_buf[80], h_buf[80], x_buf[80], y_buf[80];
   XGCValues		values;
   XEvent		input, ev;
   XMotionEvent		* motion_ev;

   xor_pixel = xorColorPixels[colorIndex];

   values.foreground = xor_pixel;
   values.function = GXxor;
   values.fill_style = FillSolid;
#ifdef NO_THIN_LINE
   values.line_width = 1;
#else
   values.line_width = 0;
#endif
   values.line_style = LineSolid;

   XChangeGC (mainDisplay, drawGC,
         GCForeground | GCFunction | GCFillStyle | GCLineWidth | GCLineStyle,
         &values);

   saved_x = grid_x = OrigX;
   saved_y = grid_y = OrigY; 
   PixelToMeasurementUnit(w_buf, 0);
   PixelToMeasurementUnit(h_buf, 0);
   PixelToMeasurementUnit(x_buf, ABS_X(grid_x));
   PixelToMeasurementUnit(y_buf, ABS_Y(grid_y));
   sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
   StartShowMeasureCursor (grid_x, grid_y, buf, TRUE);
   BeginIntervalRulers (grid_x, grid_y, grid_x, grid_y);
   XGrabPointer (mainDisplay, drawWindow, FALSE,
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
         MyBox (drawWindow, drawGC, OrigX, OrigY, saved_x, saved_y);
         EndIntervalRulers (grid_x, grid_y);
         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(saved_x-OrigX)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(saved_y-OrigY)));
         PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
         sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         EndShowMeasureCursor (saved_x, saved_y, buf, TRUE);
         done = TRUE;
      }
      else if (input.type == MotionNotify)
      {
         motion_ev = &(input.xmotion);
         end_x = motion_ev->x;
         end_y = motion_ev->y;
         GridXY (end_x, end_y, &grid_x, &grid_y);
         if (motion_ev->state & (ShiftMask | ControlMask))
         {
            int w, h, pos_w=TRUE, pos_h=TRUE;

            w = grid_x - OrigX;
            h = grid_y - OrigY;
            if (w < 0)
            {
               w = (-w);
               pos_w = FALSE;
            }
            if (h < 0)
            {
               h = (-h);
               pos_h = FALSE;
            }
            if (w > h)
               grid_x = (pos_w ? (OrigX+h) : (OrigX-h));
            else
               grid_y = (pos_h ? (OrigY+w) : (OrigY-w));
         }
         if (grid_x != saved_x || grid_y != saved_y)
         {
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(saved_x-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(saved_y-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            ShowMeasureCursor (saved_x, saved_y, buf, TRUE);
            MyBox (drawWindow, drawGC, OrigX, OrigY, saved_x, saved_y);
            saved_x = grid_x;
            saved_y = grid_y;
            MyBox (drawWindow, drawGC, OrigX, OrigY, saved_x, saved_y);
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(saved_x-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(saved_y-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            ShowMeasureCursor (saved_x, saved_y, buf, TRUE);
         }
         DrawIntervalRulers (OrigX, OrigY, grid_x, grid_y);
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
      else if (input.type == KeyPress)
      {
         KeySym	key_sym;
         char	s[80];

         XLookupString (&(input.xkey), s, 80-1, &key_sym, &c_stat);
         TranslateKeys (s, &key_sym);
         if (s[0] == '\033' && (key_sym & 0xff) == '\033')
         {
            XUngrabPointer (mainDisplay, CurrentTime);
            MyBox (drawWindow, drawGC, OrigX, OrigY, saved_x, saved_y);
            EndIntervalRulers (grid_x, grid_y);
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(saved_x-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(saved_y-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(saved_x));
            PixelToMeasurementUnit(y_buf, ABS_Y(saved_y));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            EndShowMeasureCursor (saved_x, saved_y, buf, TRUE);
            abort = TRUE;
            done = TRUE;
         }
      }
   }
   if (!abort && OrigX != grid_x && OrigY != grid_y)
   {
      CreateBoxObj (OrigX, OrigY, grid_x, grid_y, FALSE);
      RecordNewObjCmd ();
      DrawBoxObj (drawWindow, drawOrigX, drawOrigY, topObj);
      boxDrawn = TRUE;
      SetFileModified (TRUE);
   }
}

void DrawBox (input)
   XEvent	* input;
{
   XButtonEvent	* button_ev;
   int		mouse_x, mouse_y, grid_x, grid_y;

   if (input->type != ButtonPress) return;

   button_ev = &(input->xbutton);
   if (button_ev->button == Button1)
   {
      mouse_x = button_ev->x;
      mouse_y = button_ev->y;
      GridXY (mouse_x, mouse_y, &grid_x, &grid_y);
      ContinueBox (grid_x, grid_y);
   }
}

void SaveBoxObj(FP, ObjPtr)
   FILE *FP;
   struct ObjRec *ObjPtr;
{
   struct BoxRec *box_ptr=ObjPtr->detail.b;

   if (fprintf(FP, "box('%s',", colorMenuItems[ObjPtr->color]) == EOF) {
      writeFileFailed = TRUE;
   }
   if (fprintf(FP,
         "%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,'%s',",
         ObjPtr->obbox.ltx, ObjPtr->obbox.lty, ObjPtr->obbox.rbx,
         ObjPtr->obbox.rby, box_ptr->fill, box_ptr->width, box_ptr->pen,
         ObjPtr->id, box_ptr->dash, ObjPtr->rotation, ObjPtr->locked,
         ObjPtr->ctm!=NULL, ObjPtr->invisible, box_ptr->width_spec) == EOF) {
      writeFileFailed = TRUE;
   }
   if (ObjPtr->ctm != NULL && fprintf(FP,
         "[\n    %1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d],",
         ObjPtr->x, ObjPtr->y,
         ObjPtr->orig_obbox.ltx, ObjPtr->orig_obbox.lty,
         ObjPtr->orig_obbox.rbx, ObjPtr->orig_obbox.rby,
         ObjPtr->ctm->m[CTM_SX], ObjPtr->ctm->m[CTM_SIN],
         ObjPtr->ctm->m[CTM_MSIN], ObjPtr->ctm->m[CTM_SY],
         ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY]) == EOF) {
      writeFileFailed = TRUE;
   }
   SaveAttrs(FP, ObjPtr->lattr);
   if (fprintf(FP, ")") == EOF) writeFileFailed = TRUE;
}

#define GETVALUE(val,name) ScanValue("%d", &(val), name, "box")
#define GETSTRNG(val,name) ScanValue("%s", (val), name, "box")

void ReadBoxObj (FP, Inbuf, ObjPtr)
   FILE			* FP;
   char			* Inbuf;
   struct ObjRec	* * ObjPtr;
{
   struct BoxRec	* box_ptr;
   char			color_str[40], * s, width_spec[40];
   int			ltx, lty, rbx, rby, fill, width, pen, dash, w, id=0;
   int			new_alloc, rotation, locked=FALSE;
   int			transformed=FALSE, invisible=FALSE;

   *ObjPtr = NULL;

   s = FindChar ((int)'(', Inbuf);
   s = ParseStr (s, (int)',', color_str, sizeof(color_str));

   InitScan (s, "\t\n, ");

   dash = 0;
   rotation = 0;
   *width_spec = '\0';
   if (fileVersion <= 5)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID)
      {
         return;
      }
      switch (width)
      {
         case 1: width = 3; break;
         case 2: width = 6; break;
      }
      id = objId++;
   }
   else if (fileVersion <= 7)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID)
      {
         return;
      }
      id = objId++;
   }
   else if (fileVersion <= 8)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 13)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 25)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 32)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID ||
          GETVALUE (locked,   "locked") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else
   {
#ifdef _TGIF_DBG
      if (GETVALUE (ltx,         "ltx") == INVALID ||
          GETVALUE (lty,         "lty") == INVALID ||
          GETVALUE (rbx,         "rbx") == INVALID ||
          GETVALUE (rby,         "rby") == INVALID ||
          GETVALUE (fill,        "fill") == INVALID ||
          GETVALUE (width,       "width") == INVALID ||
          GETVALUE (pen,         "pen") == INVALID ||
          GETVALUE (id,          "id") == INVALID ||
          GETVALUE (dash,        "dash") == INVALID ||
          GETVALUE (rotation,    "rotation") == INVALID ||
          GETVALUE (locked,      "locked") == INVALID ||
          GETVALUE (transformed, "transformed") == INVALID ||
          GETVALUE (invisible,   "invisible") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
      if (GETSTRNG(width_spec,  "width_spec") != INVALID) {
         UtilRemoveQuotes(width_spec);
      } else {
         sprintf(width_spec, "%1d", width);
         fprintf(stderr, "Bad width_spec in box... Fix this file!\n");
      }
#else /* ~_TGIF_DBG */
      if (GETVALUE (ltx,         "ltx") == INVALID ||
          GETVALUE (lty,         "lty") == INVALID ||
          GETVALUE (rbx,         "rbx") == INVALID ||
          GETVALUE (rby,         "rby") == INVALID ||
          GETVALUE (fill,        "fill") == INVALID ||
          GETVALUE (width,       "width") == INVALID ||
          GETVALUE (pen,         "pen") == INVALID ||
          GETVALUE (id,          "id") == INVALID ||
          GETVALUE (dash,        "dash") == INVALID ||
          GETVALUE (rotation,    "rotation") == INVALID ||
          GETVALUE (locked,      "locked") == INVALID ||
          GETVALUE (transformed, "transformed") == INVALID ||
          GETVALUE (invisible,   "invisible") == INVALID ||
          GETSTRNG (width_spec,  "width_spec") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
      UtilRemoveQuotes(width_spec);
#endif /* _TGIF_DBG */
   }
   if (fileVersion <= 16 && width <= 6) width = origWidthOfLine[width];
   if (fileVersion <= 32) {
      sprintf(width_spec, "%1d", width);
   }
   fill = UpgradePenFill (fill);
   pen = UpgradePenFill (pen);

   *ObjPtr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));
   box_ptr = (struct BoxRec *)malloc(sizeof(struct BoxRec));
   if (box_ptr == NULL) FailAllocMessage();
   memset(box_ptr, 0, sizeof(struct BoxRec));

   if (ltx > rbx || lty > rby)
   {
      int	tmp_ltx, tmp_lty, tmp_rbx, tmp_rby;

      if (!PRTGIF) Msg ("Bad box bounding box.  Adjusted.");
      CalcBBox (ltx, lty, rbx, rby, &tmp_ltx, &tmp_lty, &tmp_rbx, &tmp_rby);
      ltx = tmp_ltx; lty = tmp_lty; rbx = tmp_rbx; rby = tmp_rby;
   }

   box_ptr->fill = fill;
   box_ptr->width = width;
   UtilStrCpy(box_ptr->width_spec, sizeof(box_ptr->width_spec), width_spec);
   box_ptr->pen = pen;
   box_ptr->dash = dash;
   (*ObjPtr)->x = ltx;
   (*ObjPtr)->y = lty;
   (*ObjPtr)->color = QuickFindColorIndex(*ObjPtr, color_str, &new_alloc, TRUE);
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->type = OBJ_BOX;
   (*ObjPtr)->obbox.ltx = ltx;
   (*ObjPtr)->obbox.lty = lty;
   (*ObjPtr)->obbox.rbx = rbx;
   (*ObjPtr)->obbox.rby = rby;
   w = HALF_W(width);
   (*ObjPtr)->bbox.ltx = ltx - w;
   (*ObjPtr)->bbox.lty = lty - w;
   (*ObjPtr)->bbox.rbx = rbx + w;
   (*ObjPtr)->bbox.rby = rby + w;
   (*ObjPtr)->detail.b = box_ptr;
   (*ObjPtr)->ctm = NULL;
   (*ObjPtr)->invisible = invisible;

   if (fileVersion >= 33 && transformed)
   {
      int real_x=0, real_y=0;
      struct BBRec orig_obbox;
      char inbuf[MAXSTRING+1];
      struct XfrmMtrxRec *ctm;

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
          GETVALUE(ctm->m[CTM_TY],   "CTM_TY") == INVALID) {
         return;
      }
      (*ObjPtr)->ctm = ctm;
      if (ctm != NULL) {
         memcpy(&(*ObjPtr)->orig_obbox, &orig_obbox, sizeof(struct BBRec));
         (*ObjPtr)->x = real_x;
         (*ObjPtr)->y = real_y;
         GetTransformedOBBoxOffsetVs(*ObjPtr, (*ObjPtr)->rotated_obbox);
      }
   }
}

void FreeBoxObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   free(ObjPtr->detail.b);
   free(ObjPtr);
}
