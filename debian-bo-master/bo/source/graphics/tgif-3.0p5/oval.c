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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/oval.c,v 3.0 1996/05/06 16:06:29 william Exp $";
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

#include "attr.e"
#include "auxtext.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "file.e"
#include "grid.e"
#include "mainloop.e"
#include "msg.e"
#include "obj.e"
#ifndef _NO_EXTERN
#include "oval.e"
#endif
#include "pattern.e"
#include "poly.e"
#include "ps.e"
#include "raster.e"
#include "rect.e"
#include "ruler.e"
#include "select.e"
#include "setup.e"
#include "spline.e"
#include "util.e"
#include "xpixmap.e"

int	ovalDrawn = FALSE;

static
void GetPSEllipseStr (xc, yc, a, b, s)
   int	xc, yc, a, b;
   char	* s;
{
   if (preDumpSetup) PSUseEllipse();
#ifdef INVERT_CTM_BUG
   if (preDumpSetup) PSUseMinRadius();
   sprintf (s, "newpath %1d %1d %1d %s %1d %s tgifellipse",
         xc, yc, a, "tgif_min_radius", b, "tgif_min_radius");
#else
   sprintf (s, "newpath %1d %1d %1d %1d tgifellipse", xc, yc, a, b);
#endif
}

void MyOval (window, gc, bbox)
   Window	window;
   GC		gc;
   struct BBRec	bbox;
{
   register int		ltx, lty, w, h;

   if (bbox.ltx > bbox.rbx)
   {
      ltx = bbox.rbx; w = bbox.ltx - ltx;
   }
   else
   {
      ltx = bbox.ltx; w = bbox.rbx - ltx;
   }

   if (bbox.lty > bbox.rby)
   {
      lty = bbox.rby; h = bbox.lty - lty;
   }
   else
   {
      lty = bbox.lty; h = bbox.rby - lty;
   }

   XDrawArc (mainDisplay, window, gc, ltx, lty, w, h, 0, 360*64);
}

static
void DumpOvalPath (FP, ObjPtr, Xc, Yc, A, B, Width, Pen, Dash)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
   int			Xc, Yc, A, B, Width, Pen, Dash;
{
   register int	i;
   char		s[MAXSTRING];

   fprintf(FP, "   gsave\n");
   if (!colorDump && useGray && Pen > BACKPAT) {
      GrayCheck(Pen);
      fprintf(FP, "      %s setgray\n", GrayStr(Pen));
   }
   GetPSEllipseStr(Xc, Yc, A, B, s);
   fprintf(FP, "      %s\n", s);

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
 
void DumpOvalObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{
   int	ltx, lty, rbx, rby, xc, yc, a, b;
   int	fill, width, pen, dash, color_index;
   char	s[MAXSTRING];
   struct OvalRec *oval_ptr=ObjPtr->detail.o;

   if (ObjPtr->ctm == NULL) {
      ltx = ObjPtr->obbox.ltx; lty = ObjPtr->obbox.lty;
      rbx = ObjPtr->obbox.rbx; rby = ObjPtr->obbox.rby;
   } else {
      ltx = ObjPtr->orig_obbox.ltx; lty = ObjPtr->orig_obbox.lty;
      rbx = ObjPtr->orig_obbox.rbx; rby = ObjPtr->orig_obbox.rby;
   }
   a = (rbx - ltx) / 2; xc = ltx + a;
   b = (rby - lty) / 2; yc = lty + b;

   fill = oval_ptr->fill;
   width = oval_ptr->width;
   pen = oval_ptr->pen;
   dash = oval_ptr->dash;

   if (fill == NONEPAT && pen == NONEPAT) return;

   fprintf (FP, "%% OVAL\n");
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

   GetPSEllipseStr (xc, yc, a, b, s);
   switch (fill)
   {
      case NONEPAT: break;
      case SOLIDPAT:
         /* solid black oval */
         fprintf (FP, "%s fill\n", s);
         break;
      case BACKPAT:
         /* solid white oval */
         fprintf (FP, "%s\n", s);
         fprintf (FP, "closepath 1 setgray fill\n");
         DumpRGBColorLine(FP, color_index, 3, TRUE);
         break;
      default:
         /* patterned */
         fprintf (FP, "gsave\n");
         if (colorDump || !useGray)
         {
            if (preDumpSetup) PSUseColorPattern();
            fprintf (FP, "   %s\n", s);
            fprintf (FP, "   closepath 1 setgray fill\n");
            DumpRGBColorLine(FP, color_index, 3, TRUE);
            fprintf (FP, "   %s\n", s);
            fprintf (FP, "   closepath eoclip newpath\n");
            DumpPatFill (FP, fill, 8, ObjPtr->bbox, "   ");
         }
         else
         {
            GrayCheck (fill);
            fprintf (FP, "   %s setgray\n", GrayStr(fill));
            fprintf (FP, "   %s fill\n", s);
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

   if ((colorDump || !useGray) && pen > BACKPAT)
   {
      DumpOvalPath (FP, ObjPtr, xc, yc, a, b, width, BACKPAT, 0);
      DumpRGBColorLine(FP, color_index, 3, TRUE);
   }
   DumpOvalPath (FP, ObjPtr, xc, yc, a, b, width, pen, dash);

   fprintf (FP, "grestore\n");
   if (ObjPtr->ctm != NULL) fprintf(FP, "grestore\n");
   fprintf (FP, "\n");
}

int NeedsToCacheOvalObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   return (ObjPtr->ctm != NULL);
}

static float	ovalXMag1=(float)0.0, ovalYMag1=(float)0.0;
static float	ovalXMag2=(float)0.0, ovalYMag2=(float)0.0;

static
void MakeCachedOval (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register int		i;
   int			sn, cntrln, cx, cy;
   int			saved_zoomedin, saved_zoomscale, saved_x, saved_y;
   struct OvalRec	* oval_ptr=ObjPtr->detail.o;
   XPoint		*sv=NULL, *tmp_sv=NULL;
   IntPoint		*pv=NULL, *cntrlv=NULL;
   struct BBRec		obbox;
   float		w, h;

   if (ovalXMag1 == (float)0.0) {
      ovalXMag1 = (float)cos((double)(28.6*M_PI/180.0));
      ovalYMag1 = (float)sin((double)(28.6*M_PI/180.0));
      ovalXMag2 = (float)cos((double)(61.4*M_PI/180.0));
      ovalYMag2 = (float)sin((double)(61.4*M_PI/180.0));
   }
   if (ObjPtr->ctm == NULL) return;

   if (oval_ptr->rotated_vlist != NULL) free(oval_ptr->rotated_vlist);
   oval_ptr->rotated_n = 0;
   oval_ptr->rotated_vlist = tmp_sv = (XPoint*)malloc((13+1)*sizeof(XPoint));
   pv = (IntPoint*)malloc((13+1)*sizeof(IntPoint));
   if (tmp_sv == NULL || pv == NULL) FailAllocMessage();

   obbox.ltx = ObjPtr->orig_obbox.ltx - ObjPtr->x;
   obbox.lty = ObjPtr->orig_obbox.lty - ObjPtr->y;
   obbox.rbx = ObjPtr->orig_obbox.rbx - ObjPtr->x;
   obbox.rby = ObjPtr->orig_obbox.rby - ObjPtr->y;
   cx = ((obbox.ltx+obbox.rbx)>>1);
   cy = ((obbox.lty+obbox.rby)>>1);
   w = (((float)(obbox.rbx-obbox.ltx))/2.0);
   h = (((float)(obbox.rby-obbox.lty))/2.0);

   tmp_sv[0].x  = obbox.rbx;             tmp_sv[0].y  = cy;
   tmp_sv[1].x  = cx+(int)(w*ovalXMag1); tmp_sv[1].y  = cy-(int)(h*ovalYMag1);
   tmp_sv[2].x  = cx+(int)(w*ovalXMag2); tmp_sv[2].y  = cy-(int)(h*ovalYMag2);
   tmp_sv[3].x  = cx;                    tmp_sv[3].y  = obbox.lty;
   tmp_sv[4].x  = cx-(int)(w*ovalXMag2); tmp_sv[4].y  = cy-(int)(h*ovalYMag2);
   tmp_sv[5].x  = cx-(int)(w*ovalXMag1); tmp_sv[5].y  = cy-(int)(h*ovalYMag1);
   tmp_sv[6].x  = obbox.ltx;             tmp_sv[6].y  = cy;
   tmp_sv[7].x  = cx-(int)(w*ovalXMag1); tmp_sv[7].y  = cy+(int)(h*ovalYMag1);
   tmp_sv[8].x  = cx-(int)(w*ovalXMag2); tmp_sv[8].y  = cy+(int)(h*ovalYMag2);
   tmp_sv[9].x  = cx;                    tmp_sv[9].y  = obbox.rby;
   tmp_sv[10].x = cx+(int)(w*ovalXMag2); tmp_sv[10].y = cy+(int)(h*ovalYMag2);
   tmp_sv[11].x = cx+(int)(w*ovalXMag1); tmp_sv[11].y = cy+(int)(h*ovalYMag1);
   tmp_sv[12].x = tmp_sv[0].x;           tmp_sv[12].y = tmp_sv[0].y;
   for (i=0; i < 13; i++)
   {
      int	x, y;

      TransformPointThroughCTM (tmp_sv[i].x, tmp_sv[i].y, ObjPtr->ctm, &x, &y);
      pv[i].x = OFFSET_X(x + ObjPtr->x);
      pv[i].y = OFFSET_Y(y + ObjPtr->y);
      tmp_sv[i].x = (short)pv[i].x;
      tmp_sv[i].y = (short)pv[i].y;
   }
   saved_zoomedin = zoomedIn;
   saved_zoomscale = zoomScale;
   saved_x = drawOrigX;
   saved_y = drawOrigY;
   zoomedIn = FALSE;
   zoomScale = 0;
   drawOrigX = drawOrigY = 0;
   sv = MakeIntSplinePolygonVertex (&sn, &cntrln, &cntrlv,
         drawOrigX, drawOrigY, 13, pv);
   zoomedIn = saved_zoomedin;
   zoomScale = saved_zoomscale;
   drawOrigX = saved_x;
   drawOrigY = saved_y;
   free(pv);
   if (sv != NULL) {
      free(tmp_sv);
      oval_ptr->rotated_n = sn;
      oval_ptr->rotated_vlist = sv;
   } else {
      oval_ptr->rotated_n = 13;
   }

   if (cntrlv != NULL) free(cntrlv);
}
 
void DrawOvalObj (window, XOff, YOff, ObjPtr)
   Window		window;
   int			XOff, YOff;
   struct ObjRec	* ObjPtr;
{
   int			fill, width, pen, dash, pixel, real_x_off, real_y_off;
   struct BBRec		bbox;
   XGCValues		values;
   struct OvalRec	*oval_ptr=ObjPtr->detail.o;

   real_x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   real_y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);
   bbox.ltx = ZOOMED_SIZE(ObjPtr->obbox.ltx - real_x_off);
   bbox.lty = ZOOMED_SIZE(ObjPtr->obbox.lty - real_y_off);
   bbox.rbx = ZOOMED_SIZE(ObjPtr->obbox.rbx - real_x_off);
   bbox.rby = ZOOMED_SIZE(ObjPtr->obbox.rby - real_y_off);

   fill = oval_ptr->fill;
   width = oval_ptr->width;
   pen = oval_ptr->pen;
   dash = oval_ptr->dash;
   pixel = colorPixels[ObjPtr->color];

   if (NeedsToCacheOvalObj(ObjPtr) && oval_ptr->rotated_vlist==NULL) {
      MakeCachedOval(ObjPtr);
   }
   if (fill != 0)
   {
      values.foreground = (fill == 2) ? myBgPixel : pixel;
      values.function = GXcopy;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[fill];
      XChangeGC (mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple, &values);
      if (ObjPtr->ctm != NULL)
         XFillPolygon (mainDisplay, window, drawGC,
               oval_ptr->rotated_vlist, oval_ptr->rotated_n,
               Convex, CoordModeOrigin);
      else
         XFillArc (mainDisplay, window, drawGC, bbox.ltx, bbox.lty,
               bbox.rbx-bbox.ltx, bbox.rby-bbox.lty, 0, 360*64);
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
#else
#ifdef THIN_OVAL_AND_ARC
      if (values.line_width <= 1) values.line_width = 0;
#endif
#endif
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
            GCLineStyle, &values);
      if (ObjPtr->ctm != NULL)
         XDrawLines (mainDisplay, window, drawGC,
               oval_ptr->rotated_vlist, oval_ptr->rotated_n,
               CoordModeOrigin);
      else
         XDrawArc (mainDisplay, window, drawGC, bbox.ltx, bbox.lty,
               bbox.rbx-bbox.ltx, bbox.rby-bbox.lty, 0, 360*64);
   }
}

void CreateOvalObj(BBox, CreateAbsolute)
   struct BBRec	*BBox;
   int CreateAbsolute;
{
   struct OvalRec	* oval_ptr;
   struct ObjRec	* obj_ptr;
   int			width, w;

   oval_ptr = (struct OvalRec *)malloc(sizeof(struct OvalRec));
   if (oval_ptr == NULL) FailAllocMessage();
   memset(oval_ptr, 0, sizeof(struct OvalRec));
   oval_ptr->fill = objFill;
   oval_ptr->width = width = curWidthOfLine[lineWidth];
   UtilStrCpy(oval_ptr->width_spec, sizeof(oval_ptr->width_spec),
         curWidthOfLineSpec[lineWidth]);
   oval_ptr->pen = penPat;
   oval_ptr->dash = curDash;
   oval_ptr->rotated_n = 0;
   oval_ptr->rotated_vlist = NULL;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));

   if (CreateAbsolute) {
      obj_ptr->bbox.ltx = obj_ptr->obbox.ltx = obj_ptr->x = BBox->ltx;
      obj_ptr->bbox.lty = obj_ptr->obbox.lty = obj_ptr->y = BBox->lty;
      obj_ptr->bbox.rbx = obj_ptr->obbox.rbx = BBox->rbx;
      obj_ptr->bbox.rby = obj_ptr->obbox.rby = BBox->rby;
   } else {
      obj_ptr->bbox.ltx = obj_ptr->obbox.ltx = obj_ptr->x = ABS_X(BBox->ltx);
      obj_ptr->bbox.lty = obj_ptr->obbox.lty = obj_ptr->y = ABS_Y(BBox->lty);
      obj_ptr->bbox.rbx = obj_ptr->obbox.rbx = ABS_X(BBox->rbx);
      obj_ptr->bbox.rby = obj_ptr->obbox.rby = ABS_Y(BBox->rby);
   }
   w = HALF_W(width);
   obj_ptr->bbox.ltx -= w;
   obj_ptr->bbox.lty -= w;
   obj_ptr->bbox.rbx += w;
   obj_ptr->bbox.rby += w;
   obj_ptr->type = OBJ_OVAL;
   obj_ptr->color = colorIndex;
   obj_ptr->id = objId++;
   obj_ptr->dirty = FALSE;
   obj_ptr->rotation = 0;
   obj_ptr->locked = FALSE;
   obj_ptr->detail.o = oval_ptr;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;
   obj_ptr->invisible = FALSE;
   AddObj (NULL, topObj, obj_ptr);
}

static XComposeStatus	c_stat;
 
static
void ContinueOval (OrigX, OrigY)
   int	OrigX, OrigY;
{
   int 			end_x, end_y, grid_x, grid_y, done=FALSE, abort=FALSE;
   int			xor_pixel;
   char			buf[80], w_buf[80], h_buf[80], x_buf[80], y_buf[80];
   struct BBRec		bbox;
   XGCValues		values;
   XEvent		input, ev;
   XMotionEvent		* motion_ev;

   bbox.ltx = bbox.rbx = OrigX;
   bbox.lty = bbox.rby = OrigY;

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

   grid_x = end_x = OrigX;
   grid_y = end_y = OrigY; 
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
         MyOval (drawWindow, drawGC, bbox);
         EndIntervalRulers (grid_x, grid_y);
         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(bbox.rbx-OrigX)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(bbox.rby-OrigY)));
         PixelToMeasurementUnit(x_buf, ABS_X(bbox.rbx));
         PixelToMeasurementUnit(y_buf, ABS_Y(bbox.rby));
         sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         EndShowMeasureCursor (bbox.rbx, bbox.rby, buf, TRUE);
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
            int	w, h, pos_w=TRUE, pos_h=TRUE;

            w = grid_x - bbox.ltx;
            h = grid_y - bbox.lty;
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
               grid_x = (pos_w ? (bbox.ltx+h) : (bbox.ltx-h));
            else
               grid_y = (pos_h ? (bbox.lty+w) : (bbox.lty-w));
         }
         if (grid_x != bbox.rbx || grid_y != bbox.rby)
         {
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(bbox.rbx-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(bbox.rby-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(bbox.rbx));
            PixelToMeasurementUnit(y_buf, ABS_Y(bbox.rby));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            ShowMeasureCursor (bbox.rbx, bbox.rby, buf, TRUE);
            MyOval (drawWindow, drawGC, bbox);
            bbox.rbx = grid_x;
            bbox.rby = grid_y;
            MyOval (drawWindow, drawGC, bbox);
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(bbox.rbx-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(bbox.rby-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(bbox.rbx));
            PixelToMeasurementUnit(y_buf, ABS_Y(bbox.rby));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            ShowMeasureCursor (bbox.rbx, bbox.rby, buf, TRUE);
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
            MyOval (drawWindow, drawGC, bbox);
            EndIntervalRulers (grid_x, grid_y);
            PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(bbox.rbx-OrigX)));
            PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(bbox.rby-OrigY)));
            PixelToMeasurementUnit(x_buf, ABS_X(bbox.rbx));
            PixelToMeasurementUnit(y_buf, ABS_Y(bbox.rby));
            sprintf (buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
            EndShowMeasureCursor (bbox.rbx, bbox.rby, buf, TRUE);
            abort = TRUE;
            done = TRUE;
         }
      }
   }
   if (!abort && OrigX != grid_x && OrigY != grid_y)
   {
      if (bbox.ltx > bbox.rbx)
      {
         end_x = bbox.ltx; bbox.ltx = bbox.rbx; bbox.rbx = end_x;
      }
      if (bbox.lty > bbox.rby)
      {
         end_y = bbox.lty; bbox.lty = bbox.rby; bbox.rby = end_y;
      }
      CreateOvalObj(&bbox, FALSE);
      RecordNewObjCmd ();
      DrawOvalObj (drawWindow, drawOrigX, drawOrigY, topObj);
      ovalDrawn = TRUE;
      SetFileModified (TRUE);
   }
}

void DrawOval (input)
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
      ContinueOval (grid_x, grid_y);
   }
}

void SaveOvalObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{
   struct OvalRec *oval_ptr=ObjPtr->detail.o;

   if (fprintf (FP, "oval('%s',", colorMenuItems[ObjPtr->color]) == EOF) {
      writeFileFailed = TRUE;
   }
   if (fprintf(FP,
         "%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,'%s',",
         ObjPtr->obbox.ltx, ObjPtr->obbox.lty, ObjPtr->obbox.rbx,
         ObjPtr->obbox.rby, oval_ptr->fill, oval_ptr->width,
         oval_ptr->pen, ObjPtr->id, oval_ptr->dash,
         ObjPtr->rotation, ObjPtr->locked, ObjPtr->ctm!=NULL,
         ObjPtr->invisible, oval_ptr->width_spec) == EOF) {
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
   SaveAttrs (FP, ObjPtr->lattr);
   if (fprintf (FP, ")") == EOF) writeFileFailed = TRUE;
}

#define GETVALUE(val,name) ScanValue("%d", &(val), name, "oval")
#define GETSTRNG(val,name) ScanValue("%s", (val), name, "oval")

void ReadOvalObj (FP, Inbuf, ObjPtr)
   FILE			* FP;
   char			* Inbuf;
   struct ObjRec	* * ObjPtr;
{
   struct OvalRec	* oval_ptr;
   char			color_str[40], * s, width_spec[40];
   int			ltx, lty, rbx, rby, fill, width, pen, dash, w, id=0;
   int			new_alloc, rotation, locked=FALSE;
   int			transformed=FALSE, invisible=FALSE;

   *ObjPtr = NULL;

   s = FindChar ((int)'(', Inbuf);
   s = ParseStr (s, (int)',', color_str, sizeof(color_str));

   InitScan (s, "\n\t, ");

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
         fprintf(stderr, "Bad width_spec in oval... Fix this file!\n");
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

   if (ltx > rbx || lty > rby)
   {
      int	tmp_ltx, tmp_lty, tmp_rbx, tmp_rby;

      if (!PRTGIF) Msg ("Bad oval bounding box.  Adjusted.");
      CalcBBox (ltx, lty, rbx, rby, &tmp_ltx, &tmp_lty, &tmp_rbx, &tmp_rby);
      ltx = tmp_ltx; lty = tmp_lty; rbx = tmp_rbx; rby = tmp_rby;
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
   oval_ptr = (struct OvalRec *)malloc(sizeof(struct OvalRec));
   if (oval_ptr == NULL) FailAllocMessage();
   memset(oval_ptr, 0, sizeof(struct OvalRec));

   oval_ptr->fill = fill;
   oval_ptr->width = width;
   UtilStrCpy(oval_ptr->width_spec, sizeof(oval_ptr->width_spec), width_spec);
   oval_ptr->pen = pen;
   oval_ptr->dash = dash;

   oval_ptr->rotated_n = 0;
   oval_ptr->rotated_vlist = NULL;

   (*ObjPtr)->x = ltx;
   (*ObjPtr)->y = lty;
   (*ObjPtr)->color = QuickFindColorIndex(*ObjPtr, color_str, &new_alloc, TRUE);
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->type = OBJ_OVAL;
   (*ObjPtr)->obbox.ltx = ltx;
   (*ObjPtr)->obbox.lty = lty;
   (*ObjPtr)->obbox.rbx = rbx;
   (*ObjPtr)->obbox.rby = rby;
   w = HALF_W(width);
   (*ObjPtr)->bbox.ltx = ltx - w;
   (*ObjPtr)->bbox.lty = lty - w;
   (*ObjPtr)->bbox.rbx = rbx + w;
   (*ObjPtr)->bbox.rby = rby + w;
   (*ObjPtr)->detail.o = oval_ptr;
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

void FreeOvalObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   if (ObjPtr->detail.o->rotated_vlist != NULL) {
      free(ObjPtr->detail.o->rotated_vlist);
   }
   free(ObjPtr->detail.o);
   free(ObjPtr);
}
