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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/xpixmap.c,v 3.0 1996/05/06 16:17:23 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "const.h"
#include "types.h"

#include "attr.e"
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
#include "imgproc.e"
#include "mainmenu.e"
#include "mark.e"
#include "msg.e"
#include "names.e"
#include "obj.e"
#include "pattern.e"
#include "ps.e"
#include "raster.e"
#include "rect.e"
#include "select.e"
#include "setup.e"
#include "util.e"
#include "xbitmap.e"
#ifndef _NO_EXTERN
#include "xpixmap.e"
#endif

struct BucketRec {
   int	pixel, index;
   char	s[10];
};

#define XPM_BUCKETS 67
#define XPM_BUCKET_INC 10

GC	xpmGC=NULL;
int	newColormapUsed=FALSE;
int	allocColorFailed=FALSE;

double	rotatedSine[4] = { 0.0, 1.0, 0.0, -1.0 };
double	rotatedCosine[4] = { 1.0, 0.0, -1.0, 0.0 };

static Pixmap	dummyPixmap;

static char	hexValue[] = "0123456789abcdef";

static int	numColorsToDump = 0;
static int	* pixelValue = NULL;
static int	* colorIndexToDumpIndex = NULL;
static char	* colorChar = NULL;
static char	* * colorStr = NULL;

static int	askForXPmSpec = FALSE;
static int	guessXPmBgColor = FALSE;

static struct BucketRec	* * xpmBucket = NULL;
static int	* xpmBucketSize = NULL;
static int	* xpmBucketMaxSize = NULL;

static int	shownXPmErrorMessage=FALSE;

void ResetXPmErrorMessage ()
{
   shownXPmErrorMessage = FALSE;
}

void InitXPm ()
{
   register int	i;
   XGCValues	values;
   char		* c_ptr;

   dummyPixmap = XCreatePixmap (mainDisplay, mainWindow, 1, 1, mainDepth);

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.function = GXcopy;
   values.fill_style = FillSolid;
   xpmGC = XCreateGC (mainDisplay, dummyPixmap,
         GCForeground | GCBackground | GCFunction | GCFillStyle, &values);

   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"AskForXPmSpec")) != NULL) {
      if (strcmp ("True", c_ptr) == 0 || strcmp ("true", c_ptr) == 0) {
         askForXPmSpec = TRUE;
      } else {
         askForXPmSpec = FALSE;
      }
   }
   guessXPmBgColor = FALSE;
   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"GuessXPmBgColor"))!=NULL) {
      if (strcmp ("True", c_ptr) == 0 || strcmp ("true", c_ptr) == 0) {
         guessXPmBgColor = TRUE;
      }
   }
   newColormapUsed = FALSE;

   xpmBucket =
         (struct BucketRec **)malloc(XPM_BUCKETS*sizeof(struct BucketRec *));
   xpmBucketSize = (int*)malloc((XPM_BUCKETS+1)*sizeof(int));
   xpmBucketMaxSize = (int*)malloc(XPM_BUCKETS*sizeof(int));
   if (xpmBucket == NULL || xpmBucketSize == NULL || xpmBucketMaxSize == NULL) {
      FailAllocMessage();
   }
   for (i=0; i < XPM_BUCKETS; i++) {
      xpmBucket[i] =
            (struct BucketRec *)malloc(XPM_BUCKET_INC*sizeof(struct BucketRec));
      if (xpmBucket[i] == NULL) FailAllocMessage();
      xpmBucketSize[i] = 0;
      xpmBucketMaxSize[i] = XPM_BUCKET_INC;
   }
   xpmBucketSize[XPM_BUCKETS] = INVALID;
}

void CleanUpXPm ()
{
   register int	i;

   if (colorChar != NULL)
   {
      for (i = 0; i < numColorsToDump+2; i++) {
         if (colorStr[i] != NULL) {
            free(colorStr[i]);
         }
      }
      free(colorStr);
      free(colorChar);
      free(pixelValue);
      free(colorIndexToDumpIndex);
   }

   if (xpmGC != NULL) XFreeGC (mainDisplay, xpmGC);
   XFreePixmap (mainDisplay, dummyPixmap);

   askForXPmSpec = FALSE;
   for (i = 0; i < XPM_BUCKETS; i++) free(xpmBucket[i]);
   free(xpmBucket);
   free(xpmBucketSize);
   free(xpmBucketMaxSize);
   xpmBucket = NULL;
   xpmBucketSize = xpmBucketMaxSize = NULL;
}

#define xpmpixelhash(X) (((X)==(-1)) ? (XPM_BUCKETS-1) : ((X)%XPM_BUCKETS))

static
int xpmcharhash (chars_per_pixel, color_char)
   int	chars_per_pixel;
   char	* color_char;
{
   register int	i, val=0;

   for (i = 0; i < chars_per_pixel; i++) val = (val<<1)+(int)(color_char[i]);
   return (xpmpixelhash(val));
}

void BuildXPmBuckets (ncolors, pixels, chars_per_pixel, color_char)
   int	ncolors, * pixels, chars_per_pixel;
   char	* color_char;
{
   register int	* ptr, i;
   int		bucket;

   if (xpmBucketSize == NULL)
   {
      xpmBucket =
            (struct BucketRec **)malloc(XPM_BUCKETS*sizeof(struct BucketRec *));
      xpmBucketSize = (int*)malloc((XPM_BUCKETS+1)*sizeof(int));
      xpmBucketMaxSize = (int*)malloc(XPM_BUCKETS*sizeof(int));
      if (xpmBucket==NULL || xpmBucketSize==NULL || xpmBucketMaxSize==NULL) {
         FailAllocMessage();
      }
      for (i=0; i < XPM_BUCKETS; i++) {
         xpmBucket[i] = (struct BucketRec *)malloc(
               XPM_BUCKET_INC*sizeof(struct BucketRec));
         if (xpmBucket[i] == NULL) FailAllocMessage();
         xpmBucketSize[i] = 0;
         xpmBucketMaxSize[i] = XPM_BUCKET_INC;
      }
      xpmBucketSize[XPM_BUCKETS] = INVALID;
   }

   for (ptr = xpmBucketSize; *ptr != INVALID; ptr++) *ptr = 0;

   if (chars_per_pixel == INVALID)
   {  /* build the hash table according to the pixels */
      for (i = 0; i < ncolors; i++)
      {
         bucket = xpmpixelhash(pixels[i]);
         if (xpmBucketSize[bucket] == xpmBucketMaxSize[bucket])
         {
            xpmBucket[bucket] = (struct BucketRec *) realloc (xpmBucket[bucket],
                  (xpmBucketMaxSize[bucket]+XPM_BUCKET_INC) *
                  sizeof(struct BucketRec));
            xpmBucketMaxSize[bucket] += XPM_BUCKET_INC;
         }
         xpmBucket[bucket][xpmBucketSize[bucket]].index = i;
         xpmBucket[bucket][xpmBucketSize[bucket]].pixel = pixels[i];
         (xpmBucketSize[bucket])++;
      }
   }
   else
   {  /* build the hash table according to the color_char */
      if (chars_per_pixel >= 9)
      {
         sprintf (gszMsgBox, "Chars_per_pixel = %1d.", chars_per_pixel);
         Error ("BuildXPmBuckets()", gszMsgBox);
      }
      for (i = 0; i < ncolors; i++)
      {
         bucket = xpmcharhash(chars_per_pixel, &color_char[i*chars_per_pixel]);
         if (xpmBucketSize[bucket] == xpmBucketMaxSize[bucket])
         {
            xpmBucket[bucket] = (struct BucketRec *) realloc (xpmBucket[bucket],
                  (xpmBucketMaxSize[bucket]+XPM_BUCKET_INC) *
                  sizeof(struct BucketRec));
            xpmBucketMaxSize[bucket] += XPM_BUCKET_INC;
         }
         xpmBucket[bucket][xpmBucketSize[bucket]].index = i;
         strncpy (xpmBucket[bucket][xpmBucketSize[bucket]].s,
               &color_char[i*chars_per_pixel], chars_per_pixel);
         (xpmBucketSize[bucket])++;
      }
   }
}

int XPmLookUp (pixel, chars_per_pixel, color_char)
   int	pixel, chars_per_pixel;
   char	* color_char;
{
   register int			i;
   register struct BucketRec	* ptr;
   int				size, bucket;

   if (chars_per_pixel == INVALID)
   {  /* hash according to the pixels */
      bucket = xpmpixelhash(pixel);
      size = xpmBucketSize[bucket];
      for (i = 0, ptr = xpmBucket[bucket]; i < size; i++, ptr++) {
         if (ptr->pixel == pixel) {
            return (ptr->index);
         }
      }
   }
   else
   {  /* hash according to the color_char */
      bucket = xpmcharhash(chars_per_pixel, color_char);
   
      size = xpmBucketSize[bucket];
      for (i = 0, ptr = xpmBucket[bucket]; i < size; i++, ptr++)
         if (strncmp (color_char, ptr->s, chars_per_pixel) == 0)
            return (ptr->index);
   }
   return (INVALID);
}

void MakeCachedPixmap (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register int		c, r;
   int			w, h, rotate, flip, target_percent;
   int			num_cols, num_rows, image_w, image_h, watch_cursor;
   int			start_col, start_row, do_msg;
   struct XPmRec	* xpm_ptr = ObjPtr->detail.xpm;
   struct MtrxRec	mtrx;
   Pixmap		dest_pixmap=None, dest_bitmap=None;
   XImage		*src_image=NULL, *src_bitmap_image=NULL;
   XImage		*dest_image=NULL, *dest_bitmap_image=NULL;

   w = ObjPtr->obbox.rbx - ObjPtr->obbox.ltx;
   h = ObjPtr->obbox.rby - ObjPtr->obbox.lty;
   num_cols = (zoomedIn) ? (w<<zoomScale) : (w>>zoomScale);
   num_rows = (zoomedIn) ? (h<<zoomScale) : (h>>zoomScale);

   if (ObjPtr->ctm==NULL && xpm_ptr->cached_pixmap!=None &&
         xpm_ptr->cached_color!=(-1) && xpm_ptr->cached_color==ObjPtr->color &&
         xpm_ptr->cached_zoomed==zoomedIn && xpm_ptr->cached_zoom==zoomScale &&
         xpm_ptr->cached_w==num_cols && xpm_ptr->cached_h==num_rows &&
         xpm_ptr->cached_rotate==xpm_ptr->rotate &&
         xpm_ptr->cached_flip==xpm_ptr->flip)
      return;

   if ((w>>zoomScale)==0 || (h>>zoomScale)==0)
   {
      if (xpm_ptr->cached_pixmap != None)
         XFreePixmap (mainDisplay, xpm_ptr->cached_pixmap);
      if (xpm_ptr->cached_bitmap != None)
         XFreePixmap (mainDisplay, xpm_ptr->cached_bitmap);
      xpm_ptr->cached_pixmap = None;
      xpm_ptr->cached_bitmap = None;
      xpm_ptr->cached_color = (-1);
      return;
   }

   watch_cursor = watchCursorOnMainWindow;
   if (!watch_cursor)
   {
      SetWatchCursor (drawWindow);
      SetWatchCursor (mainWindow);
   }

   src_image = xpm_ptr->image;
   src_bitmap_image = xpm_ptr->bitmap_image;
   rotate = xpm_ptr->rotate;
   flip = xpm_ptr->flip;
   image_w = xpm_ptr->image_w;
   image_h = xpm_ptr->image_h;
   if (xpm_ptr->cached_pixmap != None)
      XFreePixmap (mainDisplay, xpm_ptr->cached_pixmap);
   xpm_ptr->cached_pixmap = None;
   if (xpm_ptr->cached_bitmap != None)
      XFreePixmap (mainDisplay, xpm_ptr->cached_bitmap);
   xpm_ptr->cached_bitmap = None;
   xpm_ptr->cached_color = (-1);
   if (xpm_ptr->clip_mask != None)
      XFreePixmap (mainDisplay, xpm_ptr->clip_mask);
   xpm_ptr->clip_mask = None;

   if (src_image == NULL)
      src_image = xpm_ptr->image = XGetImage (mainDisplay, xpm_ptr->pixmap,
            0, 0, image_w, image_h, AllPlanes, ZPixmap);
   if (src_bitmap_image == NULL)
      src_bitmap_image = xpm_ptr->bitmap_image = XGetImage (mainDisplay,
            xpm_ptr->bitmap, 0, 0, image_w, image_h, 1, ZPixmap);

   do_msg = ((num_rows*num_cols)>=0x4000);
   if (do_msg) {
      SaveStatusStrings();
      SetStringStatus("Caching pixmap...");
      XSync(mainDisplay, False);
   }

   dest_pixmap = XCreatePixmap (mainDisplay, dummyPixmap, num_cols, num_rows,
         mainDepth);
   dest_bitmap = XCreatePixmap (mainDisplay, dummyBitmap, num_cols, num_rows,
         1);
   XFillRectangle (mainDisplay,dest_pixmap,xpmGC,0,0,num_cols,num_rows);
   XSetForeground (mainDisplay,xbmGC,1);
   XFillRectangle (mainDisplay,dest_bitmap,xbmGC,0,0,num_cols,num_rows);
   XSetForeground (mainDisplay,xbmGC,0);
   dest_image = XGetImage (mainDisplay, dest_pixmap, 0, 0, num_cols, num_rows,
         AllPlanes, ZPixmap);
   dest_bitmap_image = XGetImage (mainDisplay, dest_bitmap, 0, 0,
         num_cols, num_rows, 1, ZPixmap);

   if (ObjPtr->ctm == NULL)
   {
      mtrx.image_w = (float)image_w; mtrx.image_h = (float)image_h;
      mtrx.w = (float)num_cols; mtrx.h = (float)num_rows;
      mtrx.rotate = rotate; mtrx.flip = flip;

      CalcTransform (&mtrx);

      start_col = (mtrx.transformed_w >= 0.0) ? 0 : (-num_cols)+1;
      start_row = (mtrx.transformed_h >= 0.0) ? 0 : (-num_rows)+1;

      target_percent = 5;
      for (r = 0; r < num_rows; r++)
      {
         float	part_x, part_y;

         if (do_msg && ((r & 0xf) == 0)) {
            int	percent=(r*10000/num_rows)/100;

            if (percent >= target_percent) {
               sprintf(gszMsgBox, "Progress: %1d%%", percent);
               SetStringStatus(gszMsgBox);
               XSync(mainDisplay, False);
               while (target_percent <= percent) target_percent += 5;
            }
         }
         part_x = (r+start_row)*mtrx.rev_m[1][0];
         part_y = (r+start_row)*mtrx.rev_m[1][1];
         for (c = 0; c < num_cols; c++)
         {
            int	x, y;

            x = (int)((c+start_col)*mtrx.rev_m[0][0]+part_x);
            y = (int)((c+start_col)*mtrx.rev_m[0][1]+part_y);
            if (x>=0 && x<image_w && y>=0 && y<image_h) {
               if (XGetPixel(src_bitmap_image,x,y) != 0) {
                  XPutPixel (dest_image, c, r, XGetPixel(src_image,x,y));
               } else {
                  XPutPixel (dest_bitmap_image, c, r, 0);
                  XPutPixel (dest_image, c, r, colorPixels[ObjPtr->color]);
               }
            }
         }
      }
   }
   else
   {
      Pixmap	clip_mask;
      XImage	* clip_mask_image;
      int	abs_offset_x=ObjPtr->obbox.ltx-ObjPtr->x;
      int	abs_offset_y=ObjPtr->obbox.lty-ObjPtr->y;

      clip_mask = XCreatePixmap (mainDisplay,dummyBitmap,num_cols,num_rows,1);
      XSetForeground (mainDisplay, xbmGC, 1);
      XFillRectangle (mainDisplay, clip_mask, xbmGC, 0, 0, num_cols, num_rows);
      XSetForeground (mainDisplay, xbmGC, 0);
      clip_mask_image = XGetImage (mainDisplay, clip_mask, 0, 0,
            num_cols, num_rows, 1, ZPixmap);

      target_percent = 5;
      for (r = 0; r < num_rows; r++)
      {
         int	y=abs_offset_y+ABS_SIZE(r);

         if (do_msg && ((r & 0xf) == 0)) {
            int	percent=(r*10000/num_rows)/100;

            if (percent >= target_percent) {
               sprintf(gszMsgBox, "Progress: %1d%%", percent);
               SetStringStatus(gszMsgBox);
               XSync(mainDisplay, False);
               while (target_percent <= percent) target_percent += 5;
            }
         }
         for (c = 0; c < num_cols; c++)
         {
            int	x=abs_offset_x+ABS_SIZE(c);
            int	new_x, new_y;

            ReverseTransformPointThroughCTM (x, y, ObjPtr->ctm, &new_x, &new_y);
            new_x += ObjPtr->x-ObjPtr->orig_obbox.ltx;
            new_y += ObjPtr->y-ObjPtr->orig_obbox.lty;
            if (new_x>=0 && new_x<image_w && new_y>=0 && new_y<image_h) {
               if (XGetPixel(src_bitmap_image,new_x,new_y) != 0) {
                  XPutPixel(dest_image, c, r, XGetPixel(src_image,new_x,new_y));
               } else {
                  XPutPixel(clip_mask_image, c, r, 0);
                  XPutPixel(dest_image, c, r, myBgPixel);
               }
            } else {
               XPutPixel(clip_mask_image, c, r, 0);
            }
         }
      }
      XPutImage (mainDisplay, clip_mask, xbmGC, clip_mask_image, 0, 0, 0, 0,
            num_cols, num_rows);
      xpm_ptr->clip_mask = clip_mask;
      XDestroyImage (clip_mask_image);
      memcpy (&xpm_ptr->cached_ctm, ObjPtr->ctm, sizeof(struct XfrmMtrxRec));
   }
   if (do_msg) {
      SetStringStatus("Finishing caching pixmap...");
      XSync(mainDisplay, False);
   }
   XPutImage (mainDisplay, dest_pixmap, xpmGC, dest_image, 0, 0, 0, 0,
         num_cols, num_rows);
   XPutImage (mainDisplay, dest_bitmap, xbmGC, dest_bitmap_image, 0, 0, 0, 0,
         num_cols, num_rows);
   if (do_msg) RestoreStatusStrings();

   xpm_ptr->cached_pixmap = dest_pixmap;
   xpm_ptr->cached_bitmap = dest_bitmap;
   xpm_ptr->cached_zoomed = zoomedIn;
   xpm_ptr->cached_zoom = zoomScale;
   xpm_ptr->cached_rotate = xpm_ptr->rotate;
   xpm_ptr->cached_flip = xpm_ptr->flip;
   xpm_ptr->cached_w = num_cols;
   xpm_ptr->cached_h = num_rows;
   xpm_ptr->cached_color = ObjPtr->color;

   if (dest_image != NULL) XDestroyImage (dest_image);
   if (dest_bitmap_image != NULL) XDestroyImage (dest_bitmap_image);

   if (!watch_cursor)
   {
      SetDefaultCursor (mainWindow);
      ShowCursor ();
   }
}

int ExtractPixmap(orig_pixmap, orig_image, orig_bitmap, orig_bitmap_image,
      x, y, w, h, pixmap, image, bitmap, bitmap_image)
   Pixmap orig_pixmap, orig_bitmap, *pixmap, *bitmap;
   XImage *orig_image, *orig_bitmap_image, **image, **bitmap_image;
   int x, y, w, h;
{
   register int j, i;
   XImage *src_image=NULL, *src_bitmap_image=NULL;

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);

   *pixmap = XCreatePixmap(mainDisplay, dummyPixmap, w, h, mainDepth);
   *bitmap = XCreatePixmap(mainDisplay, dummyBitmap, w, h, 1);
   *image = (*pixmap==None ? NULL : XGetImage(mainDisplay, *pixmap, 0, 0, w, h,
         AllPlanes, ZPixmap));
   *bitmap_image = (*bitmap==None ? NULL : XGetImage(mainDisplay, *bitmap, 0, 0,
         w, h, 1, ZPixmap));
   if (orig_image != NULL && x == 0 && y == 0) {
      src_image = orig_image;
   } else {
      src_image = XGetImage(mainDisplay, orig_pixmap, x, y, w, h, AllPlanes,
            ZPixmap);
   }
   if (orig_bitmap_image != NULL && x == 0 && y == 0) {
      src_bitmap_image = orig_bitmap_image;
   } else {
      src_bitmap_image = XGetImage(mainDisplay, orig_bitmap, x, y, w, h, 1,
            ZPixmap);
   }
   if (*pixmap == None || *bitmap == None || *image==NULL ||
         *bitmap_image==NULL || src_image==NULL || src_bitmap_image==NULL) {
      if (*pixmap == None || *bitmap == None) {
         sprintf(gszMsgBox, "Can not allocate %s of size %1dx%1d.",
               (*pixmap==None ? "pixmap" : "bitmap"), w, h);
      } else {
         sprintf(gszMsgBox, "XGetImage() failed!  May have run out of memory!");
      }
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      if (*pixmap != None) XFreePixmap(mainDisplay, *pixmap);
      if (*bitmap != None) XFreePixmap(mainDisplay, *bitmap);
      if (*image != NULL) XDestroyImage(*image);
      if (*bitmap_image != NULL) XDestroyImage(*bitmap_image);
      if (orig_image != NULL) XDestroyImage(src_image);
      if (orig_bitmap_image != NULL) XDestroyImage(src_bitmap_image);
      if (!(orig_image != NULL && x==0 && y==0) && src_image != NULL) {
         XDestroyImage(src_image);
      }
      if (!(orig_bitmap_image != NULL && x==0 && y==0) &&
            src_bitmap_image != NULL) {
         XDestroyImage(src_bitmap_image);
      }
      *pixmap = *bitmap = None;
      *image = *bitmap_image = NULL;
      SetDefaultCursor(mainWindow);
      SetDefaultCursor(drawWindow);
      return FALSE;
   }
   for (i = 0; i < h; i++) {
      for (j = 0; j < w; j++) {
         XPutPixel(*image, j, i, XGetPixel(src_image, j, i));
         XPutPixel(*bitmap_image, j, i, XGetPixel(src_bitmap_image, j, i));
      }
   }
   XPutImage(mainDisplay, *pixmap, xpmGC, *image, 0, 0, 0, 0, w, h);
   XPutImage(mainDisplay, *bitmap, xbmGC, *bitmap_image, 0, 0, 0, 0, w, h);
   SetDefaultCursor(mainWindow);
   SetDefaultCursor(drawWindow);

   if (!(orig_image != NULL && x == 0 && y == 0)) XDestroyImage(src_image);
   if (!(orig_bitmap_image != NULL && x == 0 && y == 0)) {
      XDestroyImage(src_bitmap_image);
   }
   return TRUE;
}

void CutXPixmap(pAbsX, pAbsY, pAbsW, pAbsH)
   int *pAbsX, *pAbsY, *pAbsW, *pAbsH;
{
   register int j, i;
   int w, h, chars_per_pixel, ncolors, * pixels, len;
   int ltx, lty, rbx, rby, new_w, new_h;
   int src_x, src_y, src_w, src_h, image_w, image_h;
   char *color_char, **color_str;
   float h_scale=1.0, v_scale=1.0, mag;
   Pixmap dest_pixmap=None, dest_bitmap=None;
   XImage *dest_image=NULL, *dest_bitmap_image=NULL;
   struct ObjRec *obj_ptr=topSel->obj, *new_obj_ptr;
   struct XPmRec *new_xpm_ptr;

   src_x = 0;
   src_y = 0;
   src_w = image_w = obj_ptr->detail.xpm->image_w;
   src_h = image_h = obj_ptr->detail.xpm->image_h;
   mag = 1.0;

   switch (obj_ptr->detail.xpm->rotate) {
   case ROTATE0:
   case ROTATE180:
      h_scale = ((float)((float)(obj_ptr->obbox.rbx-obj_ptr->obbox.ltx)) /
            ((float)image_w));
      v_scale = ((float)((float)(obj_ptr->obbox.rby-obj_ptr->obbox.lty)) /
            ((float)image_h));
      break;
   case ROTATE90:
   case ROTATE270:
      h_scale = ((float)((float)(obj_ptr->obbox.rby-obj_ptr->obbox.lty)) /
            ((float)image_w));
      v_scale = ((float)((float)(obj_ptr->obbox.rbx-obj_ptr->obbox.ltx)) /
            ((float)image_h));
      break;
   }
   if (pAbsX == NULL && pAbsY == NULL && pAbsW == NULL && pAbsH == NULL) {
      char mag_spec[MAXSTRING];

      sprintf(gszMsgBox, "%s: [[MAG=]WxH+X+Y] (original size is %1dx%1d)",
            "Please enter geometry spec", image_w, image_h);
      Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", mag_spec);
      UtilTrimBlanks(mag_spec);
      if (*mag_spec == '\0') return;

      ParseCutSpec(mag_spec,image_w,image_h,&mag,&src_x,&src_y,&src_w,&src_h);
   } else {
      src_x = *pAbsX;
      src_y = *pAbsY;
      src_w = *pAbsW;
      src_h = *pAbsH;
   }
   if (src_x==0 && src_y==0 && src_w==image_w && src_h==image_h && mag==1.0) {
      return;
   }
   if (src_w==0 || src_h==0) {
      Msg ("Pixmap can not have 0 width or height.");
      return;
   }
   PrepareToRecord(CMD_REPLACE, topSel, botSel, numObjSelected);
   if (!ExtractPixmap(obj_ptr->detail.xpm->pixmap,
         obj_ptr->detail.xpm->image, obj_ptr->detail.xpm->bitmap,
         obj_ptr->detail.xpm->bitmap_image, src_x, src_y, src_w, src_h,
         &dest_pixmap, &dest_image, &dest_bitmap, &dest_bitmap_image)) {
      AbortPrepareCmd(CMD_REPLACE);
      return;
   }

   UnlinkObj(obj_ptr);

   sprintf(gszMsgBox, "New pixmap size is %1dx%1d.", src_w, src_h);
   Msg(gszMsgBox);

   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
   HighLightReverse();

   w = new_w = (int)(((float)src_w) * mag);
   h = new_h = (int)(((float)src_h) * mag);

   new_obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (new_obj_ptr == NULL) FailAllocMessage();
   memset(new_obj_ptr, 0, sizeof(struct ObjRec));
   DupObjBasics(obj_ptr, new_obj_ptr);

   new_xpm_ptr = (struct XPmRec *)malloc(sizeof(struct XPmRec));
   if (new_xpm_ptr == NULL) FailAllocMessage();
   memset(new_xpm_ptr, 0, sizeof(struct XPmRec));
   new_obj_ptr->detail.xpm = new_xpm_ptr;

   new_xpm_ptr->image_w = src_w;
   new_xpm_ptr->image_h = src_h;
   new_xpm_ptr->pixmap = dest_pixmap;
   new_xpm_ptr->image = dest_image;
   new_xpm_ptr->bitmap = dest_bitmap;
   new_xpm_ptr->bitmap_image = dest_bitmap_image;
   new_xpm_ptr->data = NULL;
   new_xpm_ptr->fill = obj_ptr->detail.xpm->fill;
   new_xpm_ptr->rotate = obj_ptr->detail.xpm->rotate;
   new_xpm_ptr->flip = obj_ptr->detail.xpm->flip;
   new_xpm_ptr->cached_zoom = 0;
   new_xpm_ptr->cached_pixmap = None;
   new_xpm_ptr->cached_bitmap = None;
   new_xpm_ptr->cached_rotate = INVALID;
   new_xpm_ptr->cached_flip = 0;
   new_xpm_ptr->cached_w = 0;
   new_xpm_ptr->cached_h = 0;
   new_xpm_ptr->cached_color = (-1);

   chars_per_pixel = new_xpm_ptr->chars_per_pixel =
         obj_ptr->detail.xpm->chars_per_pixel;
   new_xpm_ptr->first_pixel_is_bg = obj_ptr->detail.xpm->first_pixel_is_bg;

   ncolors = new_xpm_ptr->ncolors = obj_ptr->detail.xpm->ncolors;
   color_char = new_xpm_ptr->color_char =
         (char*)malloc((ncolors*chars_per_pixel)*sizeof(char));
   if (color_char == NULL) FailAllocMessage();
   color_str = new_xpm_ptr->color_str = (char**)malloc(ncolors*sizeof(char*));
   if (color_str == NULL) FailAllocMessage();
   pixels = new_xpm_ptr->pixels = (int*)malloc(ncolors*sizeof(int));
   if (pixels == NULL) FailAllocMessage();
   for (i = 0; i < ncolors; i++) {
      pixels[i] = obj_ptr->detail.xpm->pixels[i];

      for (j = 0; j < chars_per_pixel; j++) {
         color_char[i*chars_per_pixel+j] =
               obj_ptr->detail.xpm->color_char[i*chars_per_pixel+j];
      }
      len = strlen(obj_ptr->detail.xpm->color_str[i]);
      color_str[i] = (char*)malloc((len+1)*sizeof(char));
      if (color_str[i] == NULL) FailAllocMessage();
      strcpy(color_str[i], obj_ptr->detail.xpm->color_str[i]);
   }

   switch (obj_ptr->detail.xpm->rotate) {
   case ROTATE0:
   case ROTATE180:
      new_w = round(h_scale * ((float)w));
      new_h = round(v_scale * ((float)h));
      break;
   case ROTATE90:
   case ROTATE270:
      new_h = round(h_scale * ((float)w));
      new_w = round(v_scale * ((float)h));
      break;
   }

   new_obj_ptr->obbox.ltx = obj_ptr->obbox.ltx;
   new_obj_ptr->obbox.lty = obj_ptr->obbox.lty;
   new_obj_ptr->obbox.rbx = new_obj_ptr->bbox.rbx = obj_ptr->obbox.ltx+new_w;
   new_obj_ptr->obbox.rby = new_obj_ptr->bbox.rby = obj_ptr->obbox.lty+new_h;

   AdjObjBBox(new_obj_ptr);

   topSel->obj = botSel->obj = new_obj_ptr;
   AddObj(NULL, topObj, new_obj_ptr);
   if (pAbsX != NULL && pAbsY != NULL) {
      MoveObj(new_obj_ptr, *pAbsX, *pAbsY);
   }
   RecordCmd(CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
   FreeObj(obj_ptr);

   UpdSelBBox();
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward();
   SetFileModified(TRUE);
   justDupped = FALSE;
}

static
void CopyXPmProperties(DestXpmObj, SrcXpmObj)
   struct ObjRec *DestXpmObj, *SrcXpmObj;
{
   struct XPmRec *dest_xpm_ptr=DestXpmObj->detail.xpm;
   struct XPmRec *src_xpm_ptr=SrcXpmObj->detail.xpm;

   dest_xpm_ptr->fill = src_xpm_ptr->fill;
   dest_xpm_ptr->rotate = src_xpm_ptr->rotate;
   DestXpmObj->color = SrcXpmObj->color;
}

static
int FinishBreakUpXPixmap(obj_ptr, cols_and_rows, cols, rows)
   struct ObjRec *obj_ptr;
   int cols_and_rows, cols, rows;
{
   struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;
   int y, image_w=xpm_ptr->image_w, image_h=xpm_ptr->image_h;
   int chunk_w=0, chunk_h=0, num_cols=0, num_rows=0, total_chunks=0;
   int orig_x=obj_ptr->x, orig_y=obj_ptr->y;
   int ncolors, chars_per_pixel, first_pixel_is_bg;

   ncolors = xpm_ptr->ncolors;
   chars_per_pixel = xpm_ptr->chars_per_pixel;
   first_pixel_is_bg = xpm_ptr->first_pixel_is_bg;

   if (cols_and_rows) {
      chunk_w = (int)(image_w / cols);
      chunk_h = (int)(image_h / rows);
   } else {
      chunk_w = cols;
      chunk_h = rows;
   }
   for (y=0; y < image_h; y += chunk_h) {
      int h=min(image_h-y,chunk_h), x;

      for (x=0; x < image_w; x += chunk_w) {
         int w=min(image_w-x,chunk_w), i, *pixels;
         char *color_char, **color_str, *xpm_data=NULL;
         struct ObjRec *new_obj_ptr;
         Pixmap dest_pixmap=None, dest_bitmap=None;
         XImage *dest_image=NULL, *dest_bitmap_image=NULL;

         if (w <= 0 || h <= 0 ||
               !ExtractPixmap(xpm_ptr->pixmap, xpm_ptr->image, xpm_ptr->bitmap,
               xpm_ptr->bitmap_image, x, y, w, h, &dest_pixmap, &dest_image,
               &dest_bitmap, &dest_bitmap_image)) {
            continue;
         }
         pixels = (int*)malloc(ncolors*sizeof(int));
         if (pixels == NULL) FailAllocMessage();
         memcpy(pixels, xpm_ptr->pixels, (size_t)(ncolors*sizeof(int)));

         color_char = (char*)malloc(ncolors*chars_per_pixel*sizeof(char));
         if (color_char == NULL) FailAllocMessage();
         memcpy(color_char, xpm_ptr->color_char,
               (size_t)(ncolors*chars_per_pixel*sizeof(char)));

         color_str = (char**)malloc(ncolors*sizeof(char*));
         if (color_str == NULL) FailAllocMessage();
         for (i=0; i < ncolors; i++) {
            if ((color_str[i]=UtilStrDup(xpm_ptr->color_str[i])) == NULL) {
               FailAllocMessage();
            }
         }
         total_chunks++;
         new_obj_ptr = CreateXPmObj(w, h, w, h, dest_pixmap, dest_image,
               dest_bitmap, dest_bitmap_image, ncolors, chars_per_pixel,
               first_pixel_is_bg, color_char, color_str, pixels, xpm_data);
         CopyXPmProperties(new_obj_ptr, obj_ptr);
         AdjObjBBox(new_obj_ptr);
         MoveObj(new_obj_ptr, orig_x+x, orig_y+y);
         AddObj(NULL, topObj, new_obj_ptr);
      }
   }
   if (total_chunks > 0) {
      int i;

      RemoveAllSel();
      UnlinkObj(obj_ptr);
      FreeObj(obj_ptr);

      for (i=0, obj_ptr=topObj; obj_ptr != NULL && i < total_chunks; i++,
            obj_ptr=obj_ptr->next) {
         struct SelRec *sel_ptr=(struct SelRec *)malloc(sizeof(struct SelRec));

         if (sel_ptr == NULL) {
            FailAllocMessage();
            return FALSE;
         }
         sel_ptr->obj = obj_ptr;
         if (botSel != NULL) {
            botSel->next = sel_ptr;
         } else {
            topSel = sel_ptr;
         }
         sel_ptr->prev = botSel;
         sel_ptr->next = NULL;
         botSel = sel_ptr;
      }
   }
   return (total_chunks > 0);
}

void BreakUpXPixmap(obj_ptr, cols_and_rows, cols, rows)
   struct ObjRec *obj_ptr;
   int cols_and_rows, cols, rows;
{
   struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;

   HighLightReverse ();
   PrepareToReplaceAnObj(obj_ptr);
   if (FinishBreakUpXPixmap(obj_ptr, cols_and_rows, cols, rows)) {
      UpdSelBBox();
      RecordCmd(CMD_ONE_TO_MANY, NULL, topSel, botSel, numObjSelected);
      SetFileModified(TRUE);
      justDupped = FALSE;
   } else {
      AbortPrepareCmd(CMD_REPLACE);
   }
   HighLightForward();
}

static
void SaveXPmColors (FP, def_color_index, xpm_ptr, ncolors, chars_per_pixel,
      color_char, color_str, pixels)
   FILE			* FP;
   int			def_color_index, ncolors, chars_per_pixel, * pixels;
   struct XPmRec	* xpm_ptr;
   char			* color_char, * * color_str;
{
   register int	i, j;
   int		cur_pixel, found_index;

   if (!colorDisplay && xpm_ptr->red != NULL)
   {
      for (i = 0; i < ncolors; i++)
      {
         if (fprintf (FP, "   \"") == EOF) writeFileFailed = TRUE;
         for (j = 0; j < chars_per_pixel; j++)
            if (fprintf (FP, "%c", color_char[i*chars_per_pixel+j]) == EOF)
               writeFileFailed = TRUE;
         if (i == ncolors-1)
         {
            if (fprintf (FP, "\", \"%s\", %1d, %1d, %1d],[\n", color_str[i],
                  (int)(xpm_ptr->red[i]), (int)(xpm_ptr->green[i]),
                  (int)(xpm_ptr->blue[i])) == EOF)
               writeFileFailed = TRUE;
         }
         else
         {
            if (fprintf (FP, "\", \"%s\", %1d, %1d, %1d,\n", color_str[i],
                  (int)(xpm_ptr->red[i]), (int)(xpm_ptr->green[i]),
                  (int)(xpm_ptr->blue[i])) == EOF)
               writeFileFailed = TRUE;
         }
      }
   }
   else
   {
      for (i = 0; i < ncolors; i++)
      {
         found_index = def_color_index;
         cur_pixel = pixels[i];
         if (cur_pixel != (-1)) {
            for (j = 0; j < maxColors; j++) {
               if (colorPixels[j] == cur_pixel) {
                  found_index = j;
                  break;
               }
            }
         }
         if (fprintf (FP, "   \"") == EOF) writeFileFailed = TRUE;
         for (j = 0; j < chars_per_pixel; j++)
            if (fprintf (FP, "%c", color_char[i*chars_per_pixel+j]) == EOF)
               writeFileFailed = TRUE;
         if (i == ncolors-1)
         {
            if (fprintf (FP, "\", \"%s\", %1d, %1d, %1d],[\n", color_str[i],
                  (int)(10000*((int)tgifColors[found_index].red)/maxRGB),
                  (int)(10000*((int)tgifColors[found_index].green)/maxRGB),
                  (int)(10000*((int)tgifColors[found_index].blue)/maxRGB)) ==
                  EOF)
               writeFileFailed = TRUE;
         }
         else
         {
            if (fprintf (FP, "\", \"%s\", %1d, %1d, %1d,\n", color_str[i],
                  (int)(10000*((int)tgifColors[found_index].red)/maxRGB),
                  (int)(10000*((int)tgifColors[found_index].green)/maxRGB),
                  (int)(10000*((int)tgifColors[found_index].blue)/maxRGB)) ==
                  EOF)
               writeFileFailed = TRUE;
         }
      }
   }
}

void DumpXPmObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{
   register int		row, i, index, col;
   int			ltx, lty, rbx, rby, w, h, image_w, image_h;
   int			ncolors, * pixels, rotate, flip, orig_x, orig_y;
   int			cur_pixel, chars_per_pixel;
   int			x, y, found_index=0, pixel_index;
   int			has_transparent_pixel=FALSE;
   char			* xpm_data, * color_char;
   Pixmap		pixmap, bitmap;
   XImage		* image=NULL, * bitmap_image=NULL;
   struct XPmRec	* xpm_ptr;
   struct MtrxRec	mtrx;

   ltx = ObjPtr->obbox.ltx;
   lty = ObjPtr->obbox.lty;
   rbx = ObjPtr->obbox.rbx;
   rby = ObjPtr->obbox.rby;

   xpm_ptr = ObjPtr->detail.xpm;

   pixmap = xpm_ptr->pixmap;
   bitmap = xpm_ptr->bitmap;
   pixels = xpm_ptr->pixels;
   ncolors = xpm_ptr->ncolors;
   rotate = xpm_ptr->rotate;
   flip = xpm_ptr->flip;
   image_w = xpm_ptr->image_w;
   image_h = xpm_ptr->image_h;

   w = rbx - ltx;
   h = rby - lty;

   if (!PRTGIF && (image=xpm_ptr->image) == NULL) {
      if ((image=xpm_ptr->image=XGetImage(mainDisplay, pixmap, 0, 0,
            image_w, image_h, AllPlanes, ZPixmap)) == NULL) {
         Msg("XGetImage() failed!  May have run out of memory!");
         Msg("X pixmap object skipped for printing.");
         return;
      }
   }
   if (!PRTGIF && (bitmap_image=xpm_ptr->bitmap_image) == NULL) {
      if ((bitmap_image=xpm_ptr->bitmap_image=XGetImage(mainDisplay, bitmap,
            0, 0, image_w, image_h, 1, ZPixmap)) == NULL) {
         Msg("XGetImage() failed!  May have run out of memory!");
         Msg("X pixmap object skipped for printing.");
         return;
      }
   }

   mtrx.image_w = (float)image_w; mtrx.image_h = (float)image_h;
   mtrx.w = (float)w; mtrx.h = (float)h;
   mtrx.rotate = rotate; mtrx.flip = flip;

   CalcTransform (&mtrx);

   orig_x = (mtrx.transformed_w >= 0.0) ? ltx : ltx+w;
   orig_y = (mtrx.transformed_h >= 0.0) ? lty : lty+h;

   fprintf (FP, "%% XPM\n");

   xpm_data = xpm_ptr->data;
   chars_per_pixel = xpm_ptr->chars_per_pixel;
   color_char = xpm_ptr->color_char;

   if (PRTGIF && fileVersion < 25)
   {  /* can't print color with PRTGIF, treat it like a bitmap */
      int	j, num_nibbles, nibble_count;
      int	h_blocks, v_blocks, block_w, block_h, bit_count, data;
      char	bg_char[2];

      fprintf (stderr, "Warning (xpm object):  %s\n",
            "All non-background color will be treated as black.");
      switch (chars_per_pixel)
      {
         case 1:
            bg_char[0] = xpm_ptr->color_char[0];
            bg_char[1] = '\0';
            if (*bg_char != '`' && *bg_char != ' ')
               bg_char[0] = bg_char[1] = '\0';
            break;
         case 2:
            bg_char[0] = xpm_ptr->color_char[0];
            bg_char[1] = xpm_ptr->color_char[1];
            if (!(bg_char[0] == '`' && bg_char[1] == '`') &&
                  !(bg_char[0] == ' ' && bg_char[1] == ' '))
               bg_char[0] = '\0';
            break;
      }

      h_blocks = ((image_w&0xff) == 0) ? (image_w>>8) : ((image_w>>8)+1);
      v_blocks = ((image_h&0xff) == 0) ? (image_h>>8) : ((image_h>>8)+1);

      fprintf (FP, "gsave\n");
      fprintf (FP, "   %1d %1d translate %.3f %.3f scale %1d rotate\n\n",
            orig_x, orig_y, mtrx.dump_h_scale, mtrx.dump_v_scale, mtrx.degree);

      for (row = 0; row < v_blocks; row++)
      {
         y = row<<8;
         block_h = (row == v_blocks-1) ? image_h-y : 0x100;

         for (col = 0; col < h_blocks; col++)
         {
            x = col<<8;
            block_w = (col == h_blocks-1) ? image_w-x : 0x100;

            num_nibbles = ((block_w%4) == 0) ? (int)(block_w>>2) :
                  (int)(block_w>>2)+1;

            fprintf (FP, "   gsave\n");
            fprintf (FP, "   %1d %1d translate\n", x, y);
            fprintf (FP, "   %1d %1d true [1 0 0 1 0 0]\n   {<", block_w,
                  block_h);

            nibble_count = 0;
            for (i = 0; i < block_h; i++)
            {
               bit_count = 0;
               data = 0;
               for (j = 0; j < block_w; j++)
               {
                  switch (chars_per_pixel)
                  {
                     case 1:
                        data = (xpm_data[(i+y)*image_w+j+x] != *bg_char) ?
                              (data<<1) | 1 : (data<<1);
                        break;
                     case 2:
                        index = ((i+y)*image_w+j+x)*chars_per_pixel;
                        data = (xpm_data[index] == bg_char[0] &&
                              xpm_data[index+1] == bg_char[1]) ?
                              (data<<1) : (data<<1) | 1;
                        break;
                  }

                  if (++bit_count == 4)
                  {
                     if (nibble_count++ == 64)
                     {
                        nibble_count = 1;
                        fprintf (FP, "\n     ");
                     }
                     fprintf (FP, "%c", hexValue[data]);
                     bit_count = 0;
                     data = 0;
                  }
               }
               if ((block_w % 4) != 0)
               {
                  data <<= (4 - (block_w % 4));
                  if (nibble_count++ == 64)
                  {
                     nibble_count = 1;
                     fprintf (FP, "\n     ");
                  }
                  fprintf (FP, "%c", hexValue[data]);
               }
               if ((num_nibbles & 0x1) == 1)
               {
                  if (nibble_count++ == 64) {
                     nibble_count = 1;
                     fprintf (FP, "\n     ");
                  }
                  fprintf (FP, "0");
               }
            }
            fprintf (FP, ">}\n");
            fprintf (FP, "   imagemask\n");
            fprintf (FP, "   grestore\n");
            if (row!=v_blocks-1 || col!=h_blocks-1) fprintf (FP, "\n");
         }
      }
      fprintf (FP, "grestore\n");
      fprintf (FP, "\n");
      return;
   }

   if (ncolors > 0xff) {
      if (PRTGIF) {
         fprintf (stderr, "Too many colors in an xpm objects -- %s.\n",
               "skipped for printing");
      } else {
         Msg ("Too many colors in an xpm objects -- skipped for printing.");
      }
      return;
   }
   if (PRTGIF || (!colorDisplay && xpm_ptr->red != NULL)) {
      int *red_ptr=xpm_ptr->red;

      for (index = 0; index < ncolors; index++) {
         if (red_ptr[index] == (-1)) {
            has_transparent_pixel = TRUE;
            break;
         }
      }
   } else {
      for (index = 0; index < ncolors; index++) {
         if (pixels[index] == (-1)) {
            has_transparent_pixel = TRUE;
            break;
         }
      }
   }

   x = (mtrx.transformed_w >= 0.0) ? ltx : ltx+w;
   y = (mtrx.transformed_h >= 0.0) ? lty : lty+h;

   fprintf (FP, "gsave\n");
   if (colorDump) {
      fprintf (FP, "   3 %1d mul array tgifsetpixels\n", ncolors);

      if (PRTGIF || (!colorDisplay && xpm_ptr->red != NULL)) {
         BuildXPmBuckets (ncolors, NULL, chars_per_pixel, color_char);
         for (index = 0; index < ncolors; index++) {
            if (xpm_ptr->red[index] == (-1)) {
               fprintf (FP, "%s%3d [ -1.00 -1.00 -1.00 ] tgifsetpix",
                     ((index & 0x1) ? "" : "   "), index*3);
            } else {
               fprintf (FP, "%s%3d [ %.3f %.3f %.3f ] tgifsetpix",
                     ((index & 0x1) ? "" : "   "), index*3,
                     ((float)xpm_ptr->red[index])/((float)10000.0),
                     ((float)xpm_ptr->green[index])/((float)10000.0),
                     ((float)xpm_ptr->blue[index])/((float)10000.0));
            }
            fprintf (FP, "%s", ((index & 0x1) ? "\n" : " "));
         }
      } else {
         BuildXPmBuckets (ncolors, pixels, INVALID, NULL);
         for (index = 0; index < ncolors; index++) {
            cur_pixel = pixels[index];
            if (cur_pixel != (-1)) {
               found_index = ObjPtr->color;
               for (i = 0; i < maxColors; i++) {
                  if (colorPixels[i] == cur_pixel) {
                     found_index = i;
                     break;
                  }
               }
               fprintf (FP, "%s%3d [ %.3f %.3f %.3f ] tgifsetpix",
                     ((index & 0x1) ? "" : "   "), index*3,
                     ((float)tgifColors[found_index].red/maxRGB),
                     ((float)tgifColors[found_index].green/maxRGB),
                     ((float)tgifColors[found_index].blue/maxRGB));
            } else {
               fprintf (FP, "%s%3d [ -1.00 -1.00 -1.00 ] tgifsetpix",
                     ((index & 0x1) ? "" : "   "), index*3);
            }
            fprintf (FP, "%s", ((index & 0x1) ? "\n" : " "));
         }
      }
      if (ncolors & 0x1) fprintf (FP, "\n");
   } else if (PRTGIF || (!colorDisplay && xpm_ptr->red != NULL)) {
      BuildXPmBuckets (ncolors, NULL, chars_per_pixel, color_char);
      if (has_transparent_pixel) {
         fprintf(FP, "   3 %1d mul array tgifsetpixels\n", ncolors);

         for (index = 0; index < ncolors; index++) {
            if (xpm_ptr->red[index] == (-1)) {
               fprintf(FP, "%s%3d [ -1.00 -1.00 -1.00 ] tgifsetpix",
                     ((index & 0x1) ? "" : "   "), index*3);
            } else {
               float gray;

               gray = 0.299*((float)xpm_ptr->red[index]/10000.0) +
                     0.587*((float)xpm_ptr->green[index]/10000.0) +
                     0.114*((float)xpm_ptr->blue[index]/10000.0);
               fprintf(FP, "%s%3d [ %.3f %.3f %.3f ] tgifsetpix",
                     ((index & 0x1) ? "" : "   "), index*3,
                     gray, gray, gray);
            }
            fprintf(FP, "%s", ((index & 0x1) ? "\n" : " "));
         }
      }
   } else if (has_transparent_pixel) {
      BuildXPmBuckets (ncolors, pixels, INVALID, NULL);
      fprintf(FP, "   3 %1d mul array tgifsetpixels\n", ncolors);

      for (index = 0; index < ncolors; index++) {
         cur_pixel = pixels[index];
         if (cur_pixel != (-1)) {
            float gray;

            found_index = ObjPtr->color;
            for (i = 0; i < maxColors; i++) {
               if (colorPixels[i] == cur_pixel) {
                  found_index = i;
                  break;
               }
            }
            gray = 0.299*((float)tgifColors[found_index].red)/((float)maxRGB) +
                  0.587*((float)tgifColors[found_index].green)/((float)maxRGB) +
                  0.114*((float)tgifColors[found_index].blue)/((float)maxRGB);
            fprintf (FP, "%s%3d [ %.3f %.3f %.3f ] tgifsetpix",
                  ((index & 0x1) ? "" : "   "), index*3,
                  gray, gray, gray);
         } else {
            fprintf (FP, "%s%3d [ -1.00 -1.00 -1.00 ] tgifsetpix",
                  ((index & 0x1) ? "" : "   "), index*3);
         }
         fprintf (FP, "%s", ((index & 0x1) ? "\n" : " "));
      }
   }
   if (ObjPtr->ctm == NULL) {
      fprintf (FP, "   %1d %1d translate %.3f %.3f scale %1d rotate\n", x, y,
            mtrx.dump_h_scale, mtrx.dump_v_scale, mtrx.degree);
   } else {
      float m[6];

      m[CTM_SX] = ((float)ObjPtr->ctm->m[CTM_SX])/((float)1000.0);
      m[CTM_SY] = ((float)ObjPtr->ctm->m[CTM_SY])/((float)1000.0);
      m[CTM_SIN] = ((float)ObjPtr->ctm->m[CTM_SIN])/((float)1000.0);
      m[CTM_MSIN] = ((float)ObjPtr->ctm->m[CTM_MSIN])/((float)1000.0);
      fprintf (FP, "   %1d %1d translate\n", ObjPtr->x, ObjPtr->y);
      fprintf (FP, "   [%.3f %.3f %.3f %.3f %1d %1d] concat\n",
            m[CTM_SX], m[CTM_SIN], m[CTM_MSIN], m[CTM_SY],
            ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY]);
   }
   fprintf (FP, "   %1d %1d 8\n   [1 0 0 1 0 0]\n", image_w, image_h);

   if (preDumpSetup) PSUseColorImage();
   if (colorDump) {
      fprintf (FP, "   {currentfile\n");
      fprintf (FP, "    tgifbwpicstr readhexstring pop 0 get tgifcolorspot}\n");
      if (has_transparent_pixel) {
         fprintf (FP, "   false 3 tgiftranscolorimage\n   ");
      } else {
         fprintf (FP, "   false 3 colorimage\n   ");
      }
   } else {
      if (has_transparent_pixel) {
         fprintf(FP, "   {currentfile\n");
         fprintf(FP, "    %s}\n",
               "tgifbwpicstr readhexstring pop 0 get tgifcolorspot");
         fprintf(FP, "   false 3 tgiftranscolorimage\n   ");
      } else {
         fprintf(FP, "   {currentfile\n");
         fprintf(FP, "    tgifbwpicstr readhexstring pop}\n");
         fprintf(FP, "   image\n   ");
      }
   }
   if (PRTGIF)
   {
      for (row = 0; row < image_h; row++) {
         for (col = 0; col < image_w; col++) {
            float gray;
            int value;

            pixel_index = XPmLookUp(INVALID, chars_per_pixel,
                  &(xpm_data[(row*image_w+col)*chars_per_pixel]));

            if (!has_transparent_pixel) {
               if (!colorDump) {
                  gray = 0.299*((float)xpm_ptr->red[pixel_index]/10000.0) +
                        0.587*((float)xpm_ptr->green[pixel_index]/10000.0) +
                        0.114*((float)xpm_ptr->blue[pixel_index]/10000.0);
                  value = gray*256;
                  pixel_index = ((value>255) ? 255 : ((value<0) ? 0 : value));
               }
            }
            fprintf(FP, "%c", hexValue[(pixel_index>>4)&0x0f]);
            fprintf(FP, "%c", hexValue[pixel_index&0x0f]);
            if (col%36 == 35) fprintf(FP, "\n   ");
         }
         if (col%36 != 0) fprintf (FP, "\n");
         if (row != image_h-1) fprintf (FP, "   ");
      }
   }
   else
   {
      for (row = 0; row < image_h; row++)
      {
         for (col = 0; col < image_w; col++)
         {
            if (colorDump || has_transparent_pixel)
            {
               if (!colorDisplay && xpm_ptr->red != NULL) {
                  pixel_index = XPmLookUp (INVALID, chars_per_pixel,
                        &(xpm_data[(row*image_w+col)*chars_per_pixel]));
               } else if (XGetPixel(bitmap_image,col,row) != 0) {
                  pixel_index = XPmLookUp(XGetPixel(image,col,row), INVALID,
                        NULL);
               } else {
                  pixel_index = XPmLookUp((-1), INVALID, NULL);
               }
            }
            else
            {
               float	gray;
               int	value;

               if (!colorDisplay && xpm_ptr->red != NULL)
               {
                  pixel_index = XPmLookUp (INVALID, chars_per_pixel,
                        &(xpm_data[(row*image_w+col)*chars_per_pixel]));
                  gray = 0.299*((float)xpm_ptr->red[pixel_index]/10000.0) +
                        0.587*((float)xpm_ptr->green[pixel_index]/10000.0) +
                        0.114*((float)xpm_ptr->blue[pixel_index]/10000.0);
               }
               else
               {
                  if (XGetPixel(bitmap_image, col, row) != 0) {
                     cur_pixel = XGetPixel (image, col, row);
                  } else {
                     found_index = ObjPtr->color;
                     cur_pixel = (-1);
                  }
                  if (cur_pixel != (-1)) {
                     for (i = 0; i < maxColors; i++) {
                        if (colorPixels[i] == cur_pixel) {
                           found_index = i;
                           break;
                        }
                     }
                  }
                  gray = 0.299*((float)tgifColors[found_index].red/maxRGB) +
                        0.587*((float)tgifColors[found_index].green/maxRGB) +
                        0.114*((float)tgifColors[found_index].blue/maxRGB);
               }
               value = gray*256;
               pixel_index = ((value>255) ? 255 : ((value<0) ? 0 : value));
            }
            fprintf (FP, "%c", hexValue[(pixel_index>>4)&0x0f]);
            fprintf (FP, "%c", hexValue[pixel_index&0x0f]);
            if (col%36 == 35)
               fprintf (FP, "\n%s",
                     (col==image_w-1 && row==image_h-1) ? "" : "   ");
         }
         if (col%36 != 0) fprintf (FP, "\n");
         if (row != image_h-1 && (col%36) != 0) fprintf (FP, "   ");
      }
   }
   fprintf (FP, "grestore\n");
   fprintf (FP, "\n");
}

int NeedsToCacheXPmObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register struct XPmRec	* xpm_ptr=ObjPtr->detail.xpm;
   int				w, h;

   w = ObjPtr->obbox.rbx - ObjPtr->obbox.ltx;
   h = ObjPtr->obbox.rby - ObjPtr->obbox.lty;

   return (ObjPtr->ctm != NULL ||
         zoomScale != 0 || xpm_ptr->image_w != w || xpm_ptr->image_h != h ||
         xpm_ptr->rotate != ROTATE0 || xpm_ptr->flip != 0);
}

static
void DrawHiddenXPm (win, ctm, vs, x, y, w, h, s)
   Window		win;
   struct XfrmMtrxRec	* ctm;
   XPoint		* vs;
   int			x, y, w, h;
   char			* s;
{
   int		str_w, len, sx, sy;
   XGCValues	values;

   values.foreground = myBgPixel;
   values.function = GXcopy;;
   values.fill_style = FillSolid;
#ifdef NO_THIN_LINE
   values.line_width = 1;
#else
   values.line_width = 0;
#endif
   values.font = rulerFontPtr->fid;
   values.line_style = LineSolid;

   XChangeGC (mainDisplay, drawGC,
         GCForeground | GCFunction | GCFillStyle | GCLineWidth | GCFont |
         GCLineStyle, &values);
   if (ctm != NULL)
      XFillPolygon (mainDisplay, win, drawGC, vs, 5, Convex, CoordModeOrigin);
   else
      XFillRectangle (mainDisplay, win, drawGC, x, y, w, h);

   XSetForeground (mainDisplay, drawGC, myFgPixel);
   if (ctm != NULL)
      XDrawLines (mainDisplay, win, drawGC, vs, 5, CoordModeOrigin);
   else
      XDrawRectangle (mainDisplay, win, drawGC, x, y, w, h);

   XSetForeground (mainDisplay, drawGC, colorPixels[colorIndex]);

   len = strlen (s);
   str_w = rulerFontWidth*len;
   if (str_w < w && rulerFontHeight < h)
   {
      sx = x + ((w-str_w)>>1);
      sy = y + ((h-rulerFontHeight)>>1);
      XDrawString (mainDisplay, win, drawGC, sx, sy+rulerFontAsc, s, len);
   }
   XSetFont (mainDisplay, drawGC, canvasFontPtr->fid);
}

void RecolorXPmObj(obj_ptr, color_index)
   struct ObjRec *obj_ptr;
   int color_index;
{
   register int c, r;
   struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;
   int image_w=xpm_ptr->image_w, image_h=xpm_ptr->image_h;
   Pixmap pixmap=None;
   XImage *image=NULL;

   xpm_ptr->cached_color = (-1);
   if (xpm_ptr->bitmap_image == NULL) {
      xpm_ptr->bitmap_image = XGetImage(mainDisplay, xpm_ptr->bitmap,
            0, 0, image_w, image_h, 1, ZPixmap);
      if (xpm_ptr->bitmap_image == NULL) return;
   }
   if (xpm_ptr->image == NULL) {
      xpm_ptr->image = XGetImage(mainDisplay, xpm_ptr->pixmap,
            0, 0, image_w, image_h, AllPlanes, ZPixmap);
      if (xpm_ptr->image == NULL) return;
   }
   pixmap = XCreatePixmap(mainDisplay, dummyPixmap, image_w, image_h,
         mainDepth);
   if (pixmap == None) return;
   XFillRectangle(mainDisplay, pixmap, xpmGC, 0, 0, image_w, image_h);
   image = XGetImage(mainDisplay, pixmap, 0, 0, image_w, image_h,
         AllPlanes, ZPixmap);
   if (image == NULL) {
      XFreePixmap(mainDisplay, pixmap);
      return;
   }
   for (r=0; r < image_h; r++) {
      for (c=0; c < image_w; c++) {
         if (XGetPixel(xpm_ptr->bitmap_image, c, r) != 0) {
            XPutPixel(image, c, r, XGetPixel(xpm_ptr->image, c, r));
         } else {
            XPutPixel(image, c, r, colorPixels[color_index]);
         }
      }
   }
   XPutImage(mainDisplay, pixmap, xpmGC, image, 0, 0, 0, 0, image_w, image_h);
   XFreePixmap(mainDisplay, xpm_ptr->pixmap);
   XDestroyImage(xpm_ptr->image);
   xpm_ptr->pixmap = pixmap;
   xpm_ptr->image = image;
   if (obj_ptr->ctm != NULL) {
      MakeCachedPixmap(obj_ptr);
   }
}

void DrawXPmObj (win, XOff, YOff, ObjPtr)
   Window		win;
   int			XOff, YOff;
   struct ObjRec	* ObjPtr;
{
   int			ltx, lty, rbx, rby, w, h, scr_w, scr_h;
   int			real_x_off, real_y_off;
   char			s[80];
   struct XPmRec	* xpm_ptr = ObjPtr->detail.xpm;
   XGCValues		values;

   if (ObjPtr->prev != NULL && ObjPtr->prev->type == OBJ_XPM &&
         Inside(ObjPtr->obbox, ObjPtr->prev->obbox) &&
         ObjPtr->prev->detail.xpm->bitmap == None) {
      return;
   }

   w = ObjPtr->obbox.rbx - ObjPtr->obbox.ltx;
   h = ObjPtr->obbox.rby - ObjPtr->obbox.lty;

   real_x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   real_y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);
   ltx = ZOOMED_SIZE(ObjPtr->obbox.ltx - real_x_off);
   lty = ZOOMED_SIZE(ObjPtr->obbox.lty - real_y_off);
   rbx = ZOOMED_SIZE(ObjPtr->obbox.rbx - real_x_off);
   rby = ZOOMED_SIZE(ObjPtr->obbox.rby - real_y_off);
   scr_w = rbx - ltx;
   scr_h = rby - lty;

   if (!mapShown)
   {
      sprintf (s, "(%1dx%1d)", xpm_ptr->image_w, xpm_ptr->image_h);
      DrawHiddenXPm (win, ObjPtr->ctm, ObjPtr->rotated_obbox,
            ltx, lty, scr_w, scr_h, s);
      return;
   }

   if (NeedsToCacheXPmObj (ObjPtr) &&
         (ObjPtr->ctm == NULL ||
         (ObjPtr->ctm != NULL && (xpm_ptr->cached_pixmap == None ||
         xpm_ptr->clip_mask == None)))) {
      MakeCachedPixmap (ObjPtr);
   }
   XSetFunction (mainDisplay, drawGC, GXcopy);
   if (ObjPtr->ctm==NULL && zoomScale==0 && xpm_ptr->rotate==ROTATE0 &&
         xpm_ptr->flip==0 && xpm_ptr->image_w==w && xpm_ptr->image_h==h) {
      if (xpm_ptr->bitmap == None) {
         XCopyArea(mainDisplay, xpm_ptr->pixmap, win, drawGC, 0, 0, w, h,
               ltx, lty);
      } else {
         values.function = GXcopy;
         values.clip_x_origin = ltx;
         values.clip_y_origin = lty;
         values.clip_mask = xpm_ptr->bitmap;
         XChangeGC (mainDisplay, drawGC, GCFunction |
               GCClipXOrigin | GCClipYOrigin | GCClipMask, &values);

         DrawClippedPixmap(xpm_ptr->pixmap, win, drawGC, scr_w, scr_h,
               ltx, lty);

         values.clip_x_origin = 0;
         values.clip_y_origin = 0;
         values.clip_mask = None;
         XChangeGC (mainDisplay, drawGC,
               GCClipXOrigin | GCClipYOrigin | GCClipMask, &values);
         if (numClipRecs > 0) {
            XSetClipRectangles(mainDisplay, drawGC, 0, 0, clipRecs,
               numClipRecs, clipOrdering);
         }
      }
   } else {
      if (xpm_ptr->cached_pixmap == None) return;
      if (ObjPtr->ctm == NULL) {
         if (xpm_ptr->cached_bitmap == None) {
            XCopyArea(mainDisplay, xpm_ptr->cached_pixmap, win, drawGC, 0, 0,
                  scr_w, scr_h, ltx, lty);
         } else {
            values.function = GXcopy;
            values.clip_x_origin = ltx;
            values.clip_y_origin = lty;
            values.clip_mask = xpm_ptr->cached_bitmap;
            XChangeGC (mainDisplay, drawGC, GCFunction |
                  GCClipXOrigin | GCClipYOrigin | GCClipMask, &values);

            DrawClippedPixmap(xpm_ptr->cached_pixmap, win, drawGC, scr_w, scr_h,
                  ltx, lty);

            values.clip_x_origin = 0;
            values.clip_y_origin = 0;
            values.clip_mask = None;
            XChangeGC (mainDisplay, drawGC,
                  GCClipXOrigin | GCClipYOrigin | GCClipMask, &values);
            if (numClipRecs > 0) {
               XSetClipRectangles(mainDisplay, drawGC, 0, 0, clipRecs,
                  numClipRecs, clipOrdering);
            }
         }
      } else if (xpm_ptr->clip_mask == None) {
         XCopyArea(mainDisplay, xpm_ptr->cached_pixmap, win, drawGC, 0, 0,
               scr_w, scr_h, ltx, lty);
      } else {
         values.function = GXcopy;
         values.clip_x_origin = ltx;
         values.clip_y_origin = lty;
         values.clip_mask = xpm_ptr->clip_mask;
         XChangeGC (mainDisplay, drawGC, GCFunction |
               GCClipXOrigin | GCClipYOrigin | GCClipMask, &values);

         DrawClippedPixmap(xpm_ptr->cached_pixmap, win, drawGC, scr_w, scr_h,
               ltx, lty);

         values.clip_x_origin = 0;
         values.clip_y_origin = 0;
         values.clip_mask = None;
         XChangeGC (mainDisplay, drawGC,
               GCClipXOrigin | GCClipYOrigin | GCClipMask, &values);
         if (numClipRecs > 0) {
            XSetClipRectangles(mainDisplay, drawGC, 0, 0, clipRecs,
               numClipRecs, clipOrdering);
         }
      }
   }
}

int QuickFindColorIndex(ObjPtr, s, new_alloc, use_default)
   struct ObjRec *ObjPtr;
   char *s;
   int *new_alloc, use_default;
{
   register int i;
   XColor exact_def, screen_def;

   *new_alloc = FALSE;

   if (ObjPtr != NULL) {
      UtilStrCpy(ObjPtr->color_str, sizeof(ObjPtr->color_str), s);
   }
   if (colorMenuItems == NULL) {
      return INVALID;
   }
   if (*s == '#') {
      if (!TgifParseColor(s, &exact_def)) {
         if (use_default) {
            sprintf(gszMsgBox, "Can not allocate color '%s', use '%s' instead.",
                  s, colorMenuItems[colorIndex]);
            if (!PRTGIF) Msg(gszMsgBox);
            allocColorFailed = TRUE;
            if (ObjPtr != NULL) {
               UtilStrCpy(ObjPtr->color_str, sizeof(ObjPtr->color_str),
                     colorMenuItems[colorIndex]);
            }
            return colorIndex;
         }
         return INVALID;
      }
   } else {
      for (i = 0; i < maxColors; i++) {
         int valid=FALSE;
         char *s1=s, *s2=colorMenuItems[i];

         if (*s1 == *s2) {
            valid = TRUE;
         } else if (*s1 >= 'a' && *s1 <= 'z') {
            if (*s1+'A'-'a' == *s2) {
               valid = TRUE;
            }
         } else if (*s >= 'A' && *s <= 'Z') {
            if (*s1+'a'-'A' == *s2) {
               valid = TRUE;
            }
         }
         if (valid && (UtilStrICmp(++s1,++s2) == 0)) {
            if (ObjPtr != NULL) {
               UtilStrCpy(ObjPtr->color_str, sizeof(ObjPtr->color_str),
                     colorMenuItems[i]);
            }
            return i;
         }
      }

      if (!TgifParseColor(s, &exact_def)) {
         if (use_default) {
            sprintf(gszMsgBox, "Can not allocate color '%s', use '%s' instead.",
                  s, colorMenuItems[colorIndex]);
            if (!PRTGIF) Msg(gszMsgBox);
            allocColorFailed = TRUE;
            if (ObjPtr != NULL) {
               UtilStrCpy(ObjPtr->color_str, sizeof(ObjPtr->color_str),
                     colorMenuItems[colorIndex]);
            }
            return colorIndex;
         }
         return INVALID;
      }
   }
   for (i = 0; i < maxColors; i++) {
      if (tgifRequestedColors[i].red == exact_def.red &&
            tgifRequestedColors[i].green == exact_def.green &&
            tgifRequestedColors[i].blue == exact_def.blue) {
         if (ObjPtr != NULL) {
            UtilStrCpy(ObjPtr->color_str, sizeof(ObjPtr->color_str),
                  colorMenuItems[i]);
         }
         return i;
      }
   }

   memcpy(&screen_def, &exact_def, sizeof(XColor));

   if (!XAllocColor(mainDisplay, mainColormap, &screen_def)) {
      Colormap colormap;

      if (newColormapUsed) {
         if (use_default) {
            sprintf(gszMsgBox, "Can not allocate color '%s', use '%s' instead.",
                  s, colorMenuItems[colorIndex]);
            if (!PRTGIF) Msg(gszMsgBox);
            allocColorFailed = TRUE;
            if (ObjPtr != NULL) {
               UtilStrCpy(ObjPtr->color_str, sizeof(ObjPtr->color_str),
                     colorMenuItems[colorIndex]);
            }
            return colorIndex;
         }
         return INVALID;
      }
      colormap = XCopyColormapAndFree(mainDisplay, mainColormap);
      mainColormap = colormap;
      newColormapUsed = TRUE;
      XSetWindowColormap(mainDisplay, mainWindow, mainColormap);
      if (!XAllocColor(mainDisplay, mainColormap, &screen_def)) {
         if (use_default) {
            sprintf(gszMsgBox, "Can not allocate color '%s', use '%s' instead.",
                  s, colorMenuItems[colorIndex]);
            if (!PRTGIF) Msg(gszMsgBox);
            allocColorFailed = TRUE;
            if (ObjPtr != NULL) {
               UtilStrCpy(ObjPtr->color_str, sizeof(ObjPtr->color_str),
                     colorMenuItems[colorIndex]);
            }
            return colorIndex;
         }
         return INVALID;
      }
   }
   colorPixels = (int*)realloc(colorPixels, (maxColors+1)*sizeof(int));
   xorColorPixels = (int*)realloc(xorColorPixels,
         (maxColors+1)*sizeof(int));
   colorLayerOn = (int*)realloc(colorLayerOn, (maxColors+1)*sizeof(int));
   colorMenuItems = (char**)realloc(colorMenuItems,
         (maxColors+1)*sizeof(char*));
   colorMenuItems[maxColors] = (char*)malloc((strlen(s)+1)*sizeof(char));
   if (colorMenuItems[maxColors] == NULL) FailAllocMessage();
   tgifColors = (XColor*)realloc(tgifColors,
         (maxColors+1)*sizeof(XColor));
   tgifRequestedColors = (XColor*)realloc(tgifRequestedColors,
         (maxColors+1)*sizeof(XColor));

   colorPixels[maxColors] = screen_def.pixel;
   xorColorPixels[maxColors] = screen_def.pixel ^ myBgPixel;
   colorLayerOn[maxColors] = TRUE;
   strcpy(colorMenuItems[maxColors], s);
   tgifRequestedColors[maxColors].red = exact_def.red;
   tgifRequestedColors[maxColors].green = exact_def.green;
   tgifRequestedColors[maxColors].blue = exact_def.blue;
   tgifColors[maxColors].red = screen_def.red;
   tgifColors[maxColors].green = screen_def.green;
   tgifColors[maxColors].blue = screen_def.blue;
   maxColors++;
   *new_alloc = TRUE;

   DestroySubMenu(MENU_COLOR);
   if (!PRTGIF && colorLayers) {
      needToRedrawColorWindow = TRUE;
   }
   return (maxColors-1);
}

struct ObjRec * CreateXPmObj (ImageW, ImageH, W, H, pixmap, image, bitmap,
      bitmap_image, ncolors, chars_per_pixel, first_pixel_is_bg, color_char,
      color_str, pixels, xpm_data)
   int		ImageW, ImageH, W, H;
   Pixmap	pixmap, bitmap;
   XImage	* image, * bitmap_image;
   int		ncolors;
   int		chars_per_pixel, first_pixel_is_bg;
   char		* color_char;
   char		* * color_str;
   int		* pixels;
   char		* xpm_data;
{
   struct XPmRec	* xpm_ptr;
   struct ObjRec	* obj_ptr;

   xpm_ptr = (struct XPmRec *)malloc(sizeof(struct XPmRec));
   if (xpm_ptr == NULL) FailAllocMessage();
   memset(xpm_ptr, 0, sizeof(struct XPmRec));

   xpm_ptr->pixmap = pixmap;
   xpm_ptr->image = image;
   xpm_ptr->bitmap = bitmap;
   xpm_ptr->bitmap_image = bitmap_image;
   xpm_ptr->image_w = ImageW;
   xpm_ptr->image_h = ImageH;
   xpm_ptr->data = xpm_data;

   xpm_ptr->fill = objFill;
   xpm_ptr->rotate = xpm_ptr->cached_rotate = ROTATE0;
   xpm_ptr->flip = xpm_ptr->cached_flip = 0;
   xpm_ptr->cached_zoom = 0;
   xpm_ptr->cached_pixmap = None;
   xpm_ptr->cached_bitmap = None;
   xpm_ptr->cached_w = xpm_ptr->cached_h = 0;
   xpm_ptr->cached_color = (-1);

   xpm_ptr->ncolors = ncolors;
   xpm_ptr->chars_per_pixel = chars_per_pixel;
   xpm_ptr->first_pixel_is_bg = first_pixel_is_bg;
   xpm_ptr->color_char = color_char;
   xpm_ptr->color_str = color_str;
   xpm_ptr->pixels = pixels;
   if (colorDisplay)
      xpm_ptr->red = xpm_ptr->green = xpm_ptr->blue = NULL;
   else
   {
      int	i;

      xpm_ptr->red = (int*)malloc(ncolors*sizeof(int));
      xpm_ptr->green = (int*)malloc(ncolors*sizeof(int));
      xpm_ptr->blue = (int*)malloc(ncolors*sizeof(int));
      if (xpm_ptr->red==NULL || xpm_ptr->green==NULL || xpm_ptr->blue==NULL) {
         FailAllocMessage();
      }
      for (i=0; i < ncolors; i++) {
         int new_alloc;
         int index=QuickFindColorIndex(NULL, color_str[i], &new_alloc, TRUE);

         xpm_ptr->red[i] =
               (int)(10000*((int)tgifRequestedColors[index].red)/maxRGB);
         xpm_ptr->green[i] =
               (int)(10000*((int)tgifRequestedColors[index].green)/maxRGB);
         xpm_ptr->blue[i] =
               (int)(10000*((int)tgifRequestedColors[index].blue)/maxRGB);
      }
   }
   xpm_ptr->clip_mask = None;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));

   obj_ptr->bbox.ltx = obj_ptr->obbox.ltx = obj_ptr->x = drawOrigX;
   obj_ptr->bbox.lty = obj_ptr->obbox.lty = obj_ptr->y = drawOrigY;
   obj_ptr->bbox.rbx = obj_ptr->obbox.rbx = W + drawOrigX;
   obj_ptr->bbox.rby = obj_ptr->obbox.rby = H + drawOrigY;
   obj_ptr->type = OBJ_XPM;
   obj_ptr->color = colorIndex;
   obj_ptr->id = objId++;
   obj_ptr->dirty = FALSE;
   obj_ptr->rotation = 0;
   obj_ptr->locked = FALSE;
   obj_ptr->detail.xpm = xpm_ptr;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;
   obj_ptr->invisible = FALSE;
   return (obj_ptr);
}
 
static
void SaveXPmPixels (FP, xpm_ptr, pixmap, image, bitmap, bitmap_image, w, h,
      ncolors, chars_per_pixel, color_char, pixels)
   FILE			* FP;
   struct XPmRec	* xpm_ptr;
   Pixmap		pixmap, bitmap;
   XImage		* * image, * * bitmap_image;
   int			w, h, ncolors, chars_per_pixel, * pixels;
   char			* color_char;
{
   register int	j, data, index, i, k;
   char		* xpm_data=NULL;

   if (*image == NULL) {
      *image = XGetImage(mainDisplay, pixmap, 0, 0, w, h, AllPlanes, ZPixmap);
      if (*image == NULL) {
         sprintf(gszMsgBox, "%s!\n\n%s!\n\n%s.",
               "XGetImage() failed!", "May have run out of memory",
               "Saved file may be corrupted");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return;
      }
   }
   if (*bitmap_image == NULL) {
      *bitmap_image = XGetImage(mainDisplay, bitmap, 0, 0, w, h, 1, ZPixmap);
      if (*bitmap_image == NULL) {
         sprintf(gszMsgBox, "%s!\n\n%s!\n\n%s.",
               "XGetImage() failed!", "May have run out of memory",
               "Saved file may be corrupted");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return;
      }
   }
   xpm_data = xpm_ptr->data;
   for (i = 0; i < h; i++)
   {
      if (fprintf (FP, "   \"") == EOF) writeFileFailed = TRUE;
      for (j = 0; j < w; j++)
      {
         if (!colorDisplay && xpm_data != NULL && xpm_ptr->red != NULL)
         {
            for (k = 0; k < chars_per_pixel; k++)
               if (fprintf(FP, "%c", xpm_data[j*chars_per_pixel+k]) == EOF)
                  writeFileFailed = TRUE;
         }
         else
         {
            if (XGetPixel(*bitmap_image, j, i) != 0) {
               data = XGetPixel (*image,j,i);
            } else {
               data = (-1);
            }
            index = XPmLookUp (data, INVALID, NULL);
            if (index == INVALID)
            {
               sprintf (gszMsgBox,
                     "Unrecognized pixel value %1d!\n\nPrint aborted!", data);
               Msg (gszMsgBox);
               return;
            }
            for (k = 0; k < chars_per_pixel; k++)
               if (fprintf(FP,"%c",color_char[index*chars_per_pixel+k]) == EOF)
                  writeFileFailed = TRUE;
         }
      }
      if (xpm_data != NULL) xpm_data += w*chars_per_pixel;
      if (i == h-1)
      {
         if (fprintf (FP, "\"],") == EOF) writeFileFailed = TRUE;
      }
      else
      {
         if (fprintf (FP, "\",\n") == EOF) writeFileFailed = TRUE;
      }
   }
}

void SaveXPmObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{
   int			ltx, lty, rbx, rby, fill, ncolors, * pixels;
   int			chars_per_pixel, first_pixel_is_bg, image_w, image_h;
   int			compressed=FALSE;
   struct XPmRec	* xpm_ptr = ObjPtr->detail.xpm;
   char			* color_char, * * color_str;

   ltx = ObjPtr->obbox.ltx; lty = ObjPtr->obbox.lty;
   rbx = ObjPtr->obbox.rbx; rby = ObjPtr->obbox.rby;
   image_w = xpm_ptr->image_w;
   image_h = xpm_ptr->image_h;
   fill = xpm_ptr->fill;
   ncolors = xpm_ptr->ncolors;
   chars_per_pixel = xpm_ptr->chars_per_pixel;
   first_pixel_is_bg = xpm_ptr->first_pixel_is_bg;
   color_char = xpm_ptr->color_char;
   color_str = xpm_ptr->color_str;
   pixels = xpm_ptr->pixels;

   if (fprintf (FP, "xpm('%s',", colorMenuItems[ObjPtr->color]) == EOF)
      writeFileFailed = TRUE;
   if (fprintf (FP,
         "%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,[\n",
         ltx, lty, rbx, rby, fill, ncolors, chars_per_pixel, first_pixel_is_bg,
         ObjPtr->id, ObjPtr->rotation, image_w, image_h, xpm_ptr->rotate,
         xpm_ptr->flip, ObjPtr->locked, compressed, ObjPtr->ctm!=NULL,
         ObjPtr->invisible) == EOF) {
      writeFileFailed = TRUE;
   }
   if (ObjPtr->ctm != NULL && fprintf(FP,
         "   %1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d],[\n",
         ObjPtr->x, ObjPtr->y,
         ObjPtr->orig_obbox.ltx, ObjPtr->orig_obbox.lty,
         ObjPtr->orig_obbox.rbx, ObjPtr->orig_obbox.rby,
         ObjPtr->ctm->m[CTM_SX], ObjPtr->ctm->m[CTM_SIN],
         ObjPtr->ctm->m[CTM_MSIN], ObjPtr->ctm->m[CTM_SY],
         ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY]) == EOF) {
      writeFileFailed = TRUE;
   }

   if (!colorDisplay && xpm_ptr->red != NULL)
      BuildXPmBuckets (ncolors, NULL, chars_per_pixel, color_char);
   else
      BuildXPmBuckets (ncolors, pixels, INVALID, NULL);
   SaveXPmColors (FP, ObjPtr->color, xpm_ptr, ncolors, chars_per_pixel,
         color_char, color_str, pixels);
   SaveXPmPixels (FP, xpm_ptr, xpm_ptr->pixmap, &(xpm_ptr->image),
         xpm_ptr->bitmap, &(xpm_ptr->bitmap_image),
         image_w, image_h, ncolors, chars_per_pixel, color_char, pixels);

   SaveAttrs (FP, ObjPtr->lattr);
   if (fprintf (FP, ")") == EOF) writeFileFailed = TRUE;
   if (!colorDisplay && xpm_ptr->red == NULL && !shownXPmErrorMessage)
   {
      sprintf (gszMsgBox, "%s.\n\n%s.",
            "Can not save X11 Pixmap objects on a Black & White display",
            "Colors stored in the file are corrupted");
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      shownXPmErrorMessage = TRUE;
   }
}

static
void FreeAuxData(fp, ncolors, color_char, color_str,
      p_pixels, pixels_used, p_pixmap, p_image, p_bitmap, p_bitmap_image)
   FILE *fp;
   int ncolors;
   char *color_char, **color_str;
   int **p_pixels, *pixels_used;
   Pixmap *p_pixmap, *p_bitmap;
   XImage **p_image, **p_bitmap_image;
{
   register int i;

   for (i = 0; i < ncolors; i++) free(color_str[i]);
   free(color_char);
   free(color_str);
   if (p_pixels != NULL && *p_pixels != NULL) free(*p_pixels);
   if (pixels_used != NULL) free(pixels_used);
   if (p_pixmap != NULL && *p_pixmap != None) {
      XFreePixmap(mainDisplay, *p_pixmap);
   }
   if (p_bitmap != NULL && *p_bitmap != None){
       XFreePixmap(mainDisplay, *p_bitmap);
   }
   if (p_image != NULL && *p_image != NULL) XDestroyImage(*p_image);
   if (p_bitmap_image != NULL && *p_bitmap_image != NULL) {
      XDestroyImage(*p_bitmap_image);
   }
   fclose(fp);
}

int MyReadPixmapFile (file_name, image_w_return, image_h_return, w_return,
      h_return, pixmap_return, image_return, bitmap_return, bitmap_image_return,
      ncolors_return, chars_per_pixel_return, first_pixel_is_bg_return,
      color_char, color_str, pixels, xpm_data)
   char *file_name;
   int *image_w_return, *image_h_return, *w_return, *h_return;
   int *ncolors_return;
   int *chars_per_pixel_return, *first_pixel_is_bg_return;
   Pixmap *pixmap_return, *bitmap_return;
   XImage **image_return, **bitmap_image_return;
   char **color_char, ***color_str;
   int **pixels;
   char **xpm_data;
{
   register int j, k;
   register char *c_ptr;
   FILE *fp;
   char inbuf[MAXSTRING], *line, s[MAXSTRING];
   char mag_spec[MAXSTRING], *xpm_data_ptr=NULL;
   int i, len, format, found=FALSE, index, saved_max_colors, xpm_version=1;
   int x, y, w, h, chars_per_pixel, new_alloc, image_w=0, image_h=0;
   int *pixels_used=NULL;
   float mag;

   *xpm_data = NULL;
   gnNumNewColorsInPixmapFile = 0;

   if ((fp=fopen(file_name, "r")) == NULL) return BitmapOpenFailed;

   saved_max_colors = maxColors;
   while (fgets(inbuf, MAXSTRING, fp) != NULL) {
      if (strncmp(inbuf, "#define ", 8) == 0) {
         xpm_version = 1;
         found = TRUE;
         break;
      } else if (strncmp(inbuf, "/* XPM */", 9) == 0) {
         xpm_version = 3;
         found = TRUE;
         break;
      }
   }
   if (!found) return BitmapFileInvalid;

   if (xpm_version == 1) {
      c_ptr = FindChar((int)' ', inbuf);
      c_ptr = ParseStr(c_ptr, (int)' ', s, sizeof(s));
      len = strlen(s);
      if (len <= 7 || strcmp(&s[len-7], "_format") != 0) {
         return BitmapFileInvalid;
      }
      sscanf(c_ptr, "%d", &format);
      if (format != 1) {
         sprintf(gszMsgBox, "Can not process format %1d.", format);
         Msg(gszMsgBox);
         return BitmapFileInvalid;
      }
      if (fgets(inbuf, MAXSTRING, fp)==NULL) return BitmapFileInvalid;
      c_ptr = FindChar((int)' ', inbuf);
      c_ptr = ParseStr(c_ptr, (int)' ', s, sizeof(s));
      len = strlen(s);
      if (len <= 6 || strcmp(&s[len-6], "_width") != 0) {
         return BitmapFileInvalid;
      }
      sscanf(c_ptr, "%d", &image_w);
      if (image_w_return != NULL) *image_w_return = image_w;

      if (fgets(inbuf, MAXSTRING, fp)==NULL) return BitmapFileInvalid;
      c_ptr = FindChar((int)' ', inbuf);
      c_ptr = ParseStr(c_ptr, (int)' ', s, sizeof(s));
      len = strlen(s);
      if (len <= 7 || strcmp(&s[len-7], "_height") != 0) {
         return (BitmapFileInvalid);
      }
      sscanf(c_ptr, "%d", &image_h);
      if (image_h_return != NULL) *image_h_return = image_h;

      if (fgets(inbuf, MAXSTRING, fp)==NULL) return BitmapFileInvalid;
      c_ptr = FindChar((int)' ', inbuf);
      c_ptr = ParseStr(c_ptr, (int)' ', s, sizeof(s));
      len = strlen(s);
      if (len <= 8 || strcmp(&s[len-8], "_ncolors") != 0) {
         return BitmapFileInvalid;
      }
      sscanf(c_ptr, "%d", ncolors_return);

      if (fgets(inbuf, MAXSTRING, fp)==NULL) return BitmapFileInvalid;
      c_ptr = FindChar((int)' ', inbuf);
      c_ptr = ParseStr(c_ptr, (int)' ', s, sizeof(s));
      len = strlen(s);
      if (len <= 16 || strcmp(&s[len-16], "_chars_per_pixel") != 0) {
         return (BitmapFileInvalid);
      }
      sscanf(c_ptr, "%d", chars_per_pixel_return);
      chars_per_pixel = *chars_per_pixel_return;
      if (chars_per_pixel > 2) {
         sprintf(gszMsgBox, "Can not handle chars_per_pixel=%1d.",
               chars_per_pixel);
         Msg(gszMsgBox);
         return BitmapFileInvalid;
      }

      if (fgets(inbuf, MAXSTRING, fp)==NULL) return BitmapFileInvalid;
      len = strlen(inbuf);
      if (len <= 27 || strncmp(inbuf, "static char *", 13) != 0 ||
            strncmp(&inbuf[len-14], "_colors[] = {\n", 14) != 0) {
         return BitmapFileInvalid;
      }
   } else {
      /* xpm_version is 3 */
      found = FALSE;
      while (fgets(inbuf, MAXSTRING, fp) != NULL) {
         if (*inbuf == '"') {
            found = TRUE;
            break;
         }
      }
      if (!found) return BitmapFileInvalid;
      c_ptr = &inbuf[1];
      if (sscanf(c_ptr, "%d %d %d %d", &image_w, &image_h,
            ncolors_return, chars_per_pixel_return) != 4) {
         return BitmapFileInvalid;
      }
      if (image_w_return != NULL) *image_w_return = image_w;
      if (image_h_return != NULL) *image_h_return = image_h;
      chars_per_pixel = *chars_per_pixel_return;
   }

   *color_char =
         (char*)malloc((*ncolors_return)*(chars_per_pixel)*sizeof(char));
   *color_str = (char**)malloc((*ncolors_return)*sizeof(char*));
   if (color_char == NULL || color_str == NULL) FailAllocMessage();
   if (pixels != NULL) {
      *pixels = (int*)malloc((*ncolors_return)*sizeof(int));
      pixels_used = (int*)malloc((*ncolors_return)*sizeof(int));
      if (*pixels == NULL || pixels_used == NULL) FailAllocMessage();
      memset(pixels_used, 0, (*ncolors_return)*sizeof(int));
   }
   if (pixmap_return != NULL) *pixmap_return = None;
   if (bitmap_return != NULL) *bitmap_return = None;
   if (image_return != NULL) *image_return = NULL;
   if (bitmap_image_return != NULL) *bitmap_image_return = NULL;
   *first_pixel_is_bg_return = FALSE;

   if (xpm_version == 1) {
      for (i = 0; i < *ncolors_return; i++) {
         if (fgets(inbuf, MAXSTRING, fp) == NULL) {
            FreeAuxData(fp, i, *color_char, *color_str, pixels, pixels_used,
                  pixmap_return, image_return, bitmap_return,
                  bitmap_image_return);
            return BitmapFileInvalid;
         }
         c_ptr = FindChar((int)'"', inbuf);
         for (j = 0; j < chars_per_pixel; j++) {
            (*color_char)[i*(chars_per_pixel)+j] = c_ptr[j];
         }
         if (guessXPmBgColor && i == 0 &&
               ((chars_per_pixel == 1 && (*c_ptr=='`' || *c_ptr==' ')) ||
               (chars_per_pixel == 2 && ((c_ptr[0]=='`' && c_ptr[1]=='`') ||
               (c_ptr[0]==' ' && c_ptr[1]==' '))))) {
            strcpy(s, myBgColorStr);
            if (pixels != NULL) (*pixels)[0] = myBgPixel;
            *first_pixel_is_bg_return = TRUE;
         } else {
            c_ptr = FindChar((int)'"', c_ptr);
            c_ptr = FindChar((int)'"', c_ptr);
            ParseStr(c_ptr, (int)'"', s, sizeof(s));
            if (pixels != NULL) {
               if (UtilStrICmp(s, "None") == 0) {
                  (*pixels)[i] = (-1);
               } else if ((index=QuickFindColorIndex(NULL, s, &new_alloc,
                     TRUE)) == INVALID) {
                  sprintf(gszMsgBox,
                        "Can not allocate color '%s', use '%s' instead.",
                        s, colorMenuItems[colorIndex]);
                  Msg(gszMsgBox);
                  strcpy(s, colorMenuItems[colorIndex]);
                  (*pixels)[i] = colorPixels[colorIndex];
                  allocColorFailed = TRUE;
               } else {
                  (*pixels)[i] = colorPixels[index];
               }
            }
         }
         len = strlen(s);
         (*color_str)[i] = (char*)malloc((len+1)*sizeof(char));
         if ((*color_str)[i] == NULL) FailAllocMessage();
         strcpy((*color_str)[i], s);
      }

      if (fgets(inbuf, MAXSTRING, fp) == NULL ||
            inbuf[0] != '}' ||
            fgets(inbuf, MAXSTRING, fp) == NULL) {
         FreeAuxData(fp, *ncolors_return, *color_char, *color_str, pixels,
               pixels_used, pixmap_return, image_return, bitmap_return,
               bitmap_image_return);
         return BitmapFileInvalid;
      }
      len = strlen(inbuf);
      if (len <= 27 || strncmp(inbuf, "static char *", 13) != 0 ||
            strncmp(&inbuf[len-14], "_pixels[] = {\n", 14) != 0) {
         return (BitmapFileInvalid);
      }
   } else {
      /* xpm_version is 3 */
      for (i = 0; i < *ncolors_return; i++) {
         if (i == 0) {
            found = FALSE;
            while (fgets(inbuf, MAXSTRING, fp) != NULL) {
               if (*inbuf == '"') {
                  found = TRUE;
                  break;
               }
            }
            if (!found) {
               FreeAuxData(fp, i, *color_char, *color_str, pixels, pixels_used,
                     pixmap_return, image_return, bitmap_return,
                     bitmap_image_return);
               return BitmapFileInvalid;
            }
         } else if (fgets(inbuf, MAXSTRING, fp) == NULL) {
            FreeAuxData(fp, i, *color_char, *color_str, pixels, pixels_used,
                  pixmap_return, image_return, bitmap_return,
                  bitmap_image_return);
            return BitmapFileInvalid;
         }
         c_ptr = FindChar((int)'"', inbuf);
         for (j = 0; j < chars_per_pixel; j++) {
            (*color_char)[i*(chars_per_pixel)+j] = c_ptr[j];
         }
         if (guessXPmBgColor && i == 0 &&
               ((chars_per_pixel == 1 && (*c_ptr=='`' || *c_ptr==' ')) ||
               (chars_per_pixel == 2 && ((c_ptr[0]=='`' && c_ptr[1]=='`') ||
               (c_ptr[0]==' ' && c_ptr[1]==' '))))) {
            strcpy(s, myBgColorStr);
            if (pixels != NULL) (*pixels)[0] = myBgPixel;
            *first_pixel_is_bg_return = TRUE;
         } else {
            char *ptr;

            c_ptr += chars_per_pixel;
            while (*c_ptr != '\0') {
               while (*c_ptr == ' ' || *c_ptr == '\t') c_ptr++;
               if ((*c_ptr == 'c' || *c_ptr == 'm') &&
                     (c_ptr[1]==' ' || c_ptr[1]=='\t')) {
                  break;
               }
               while (*c_ptr!=' ' && *c_ptr!='\t' && *c_ptr!='\0') c_ptr++;
               while (*c_ptr == ' ' || *c_ptr == '\t') c_ptr++;
               while (*c_ptr!=' ' && *c_ptr!='\t' && *c_ptr!='\0') c_ptr++;
            }
            if (*c_ptr++ == '\0') {
               FreeAuxData(fp, i, *color_char, *color_str, pixels, pixels_used,
                     pixmap_return, image_return, bitmap_return,
                     bitmap_image_return);
               return BitmapFileInvalid;
            }
            while (*c_ptr == ' ' || *c_ptr == '\t') c_ptr++;
            for (ptr=c_ptr; *ptr !=  '"' && *ptr != '\0'; ptr++) {
               if (*ptr == ' ' || *ptr == '\t') {
                  char tmp_ch=ptr[1];

                  if (tmp_ch == 'm' || tmp_ch == 's') {
                     tmp_ch = ptr[2];
                     if (tmp_ch == ' ' || tmp_ch == '\t') {
                        break;
                     }
                  } else if (tmp_ch == 'g') {
                     tmp_ch = ptr[2];
                     if (tmp_ch == ' ' || tmp_ch == '\t') {
                        break;
                     } else if (tmp_ch == '4') {
                        tmp_ch = ptr[4];
                        if (tmp_ch == ' ' || tmp_ch == '\t') {
                           break;
                        }
                     }
                  }
               }
            }
            if (*ptr == '\0') {
               FreeAuxData(fp, i, *color_char, *color_str, pixels, pixels_used,
                     pixmap_return, image_return, bitmap_return,
                     bitmap_image_return);
               return BitmapFileInvalid;
            }
            while (ptr >= c_ptr) {
               if (*ptr == ' ' || *ptr == '\t' || *ptr == '"') {
                  ptr--;
               } else {
                  break;
               }
            }
            if (ptr < c_ptr) {
               FreeAuxData(fp, i, *color_char, *color_str, pixels, pixels_used,
                     pixmap_return, image_return, bitmap_return,
                     bitmap_image_return);
               return BitmapFileInvalid;
            }
            *(++ptr) = '\0';
            strcpy(s, c_ptr);
            if (pixels != NULL) {
               if (UtilStrICmp(s, "None") == 0) {
                  (*pixels)[i] = (-1);
               } else if ((index=QuickFindColorIndex(NULL, s, &new_alloc,
                     TRUE)) == INVALID) {
                  sprintf(gszMsgBox,
                        "Can not allocate color '%s', use '%s' instead.",
                        s, colorMenuItems[colorIndex]);
                  Msg(gszMsgBox);
                  strcpy(s, colorMenuItems[colorIndex]);
                  (*pixels)[i] = colorPixels[colorIndex];
                  allocColorFailed = TRUE;
               } else {
                  (*pixels)[i] = colorPixels[index];
               }
            }
         }
         len = strlen(s);
         (*color_str)[i] = (char*)malloc((len+1)*sizeof(char));
         if ((*color_str)[i] == NULL) FailAllocMessage();
         strcpy((*color_str)[i], s);
      }
   }

   x = 0;
   y = 0;
   w = image_w;
   h = image_h;
   if (pixmap_return != NULL && bitmap_return != NULL &&
         image_return != NULL && bitmap_image_return != NULL) {
      mag = 1.0;
      if (askForXPmSpec) {
         sprintf(gszMsgBox, "%s: [[MAG=]WxH+X+Y] (original size is %1dx%1d)",
            "Please enter geometry spec", image_w, image_h);
         Dialog(gszMsgBox, "( <CR>: accept, <ESC>: continue )", mag_spec);
         if (*mag_spec != '\0') {
            ParseCutSpec(mag_spec, image_w, image_h, &mag, &x, &y, &w, &h);
         }
      }
      *pixmap_return = XCreatePixmap(mainDisplay, dummyPixmap, w, h, mainDepth);
      *bitmap_return = XCreatePixmap(mainDisplay, dummyBitmap, w, h, 1);
      if (*pixmap_return == None || *bitmap_return == None) {
         FreeAuxData(fp, *ncolors_return, *color_char, *color_str, pixels,
               pixels_used, pixmap_return, image_return, bitmap_return,
               bitmap_image_return);
         return BitmapNoMemory;
      }
      XFillRectangle(mainDisplay, *pixmap_return, xpmGC, 0, 0, w, h);
      XSetForeground(mainDisplay, xbmGC, 1);
      XFillRectangle(mainDisplay, *bitmap_return, xbmGC, 0, 0, w, h);
      XSetForeground(mainDisplay, xbmGC, 0);
      *image_return = XGetImage(mainDisplay, *pixmap_return, 0, 0, w, h,
            AllPlanes, ZPixmap);
      *bitmap_image_return = XGetImage(mainDisplay, *bitmap_return, 0, 0, w, h,
            1, ZPixmap);
      if (*image_return == NULL || *bitmap_image_return == NULL) {
         Msg("XGetImage() failed!  May have run out of memory!");
         FreeAuxData(fp, *ncolors_return, *color_char, *color_str, pixels,
               pixels_used, pixmap_return, image_return, bitmap_return,
               bitmap_image_return);
         return BitmapNoMemory;
      }
   }
   BuildXPmBuckets(*ncolors_return, NULL, chars_per_pixel, *color_char);
   if (!colorDisplay)
   {
      xpm_data_ptr = *xpm_data = (char*)malloc(
            image_w*image_h*chars_per_pixel*sizeof(char));
      if (*xpm_data == NULL) FailAllocMessage();
   }
   line = (char*)malloc((image_w*chars_per_pixel+20)*sizeof(char));
   if (line == NULL) FailAllocMessage();
   for (i = 0; i < y+h; i++) {
      if (xpm_version == 3 && i == 0) {
         found = FALSE;
         while (fgets(line, image_w*chars_per_pixel+20, fp) != NULL) {
            if (*line == '"') {
               found = TRUE;
               break;
            }
         }
         if (!found) {
            FreeAuxData(fp, *ncolors_return, *color_char, *color_str, pixels,
                  pixels_used, pixmap_return, image_return, bitmap_return,
                  bitmap_image_return);
            return BitmapFileInvalid;
         }
      } else if (fgets(line, image_w*chars_per_pixel+20, fp)==NULL) {
         FreeAuxData(fp, *ncolors_return, *color_char, *color_str, pixels,
               pixels_used, pixmap_return, image_return, bitmap_return,
               bitmap_image_return);
         free(line);
         return BitmapFileInvalid;
      }
      if (i >= y) {
         c_ptr = FindChar((int)'"', line);
         if (xpm_data_ptr != NULL) {
            strncpy(xpm_data_ptr, c_ptr, image_w*chars_per_pixel);
         }
         for (j = 0; j < x+w; j++, c_ptr+=chars_per_pixel) {
            if (j >= x) {
               k = XPmLookUp(INVALID, chars_per_pixel, c_ptr);
               if (k == INVALID) {
                  FreeAuxData(fp, *ncolors_return, *color_char, *color_str,
                        pixels, pixels_used, pixmap_return, image_return,
                        bitmap_return, bitmap_image_return);
                  free(line);
                  return BitmapFileInvalid;
               }
               if (pixels != NULL) {
                  if (pixels_used != NULL) pixels_used[k] = TRUE;
                  if ((*pixels)[k] == (-1)) {
                     XPutPixel(*bitmap_image_return, j-x, i-y, 0);
                     XPutPixel(*image_return, j-x, i-y,
                           colorPixels[colorIndex]);
                  } else {
                     XPutPixel(*image_return, j-x, i-y, (*pixels)[k]);
                  }
               }
            }
         }
         if (xpm_data_ptr != NULL) {
            xpm_data_ptr += image_w*chars_per_pixel;
         }
      }
   }
   free(line);
   fclose(fp);
   if (pixmap_return != NULL && image_return != NULL) {
      XPutImage(mainDisplay,*pixmap_return,xpmGC,*image_return,0,0,0,0,w,h);
   }
   if (bitmap_return != NULL && bitmap_image_return != NULL) {
      XPutImage(mainDisplay, *bitmap_return, xbmGC, *bitmap_image_return,
            0, 0, 0, 0, w, h);
   }
   if (image_w_return != NULL) *image_w_return = w;
   if (image_h_return != NULL) *image_h_return = h;
   if (w_return != NULL) *w_return = (int)(((float)w) * mag);
   if (h_return != NULL) *h_return = (int)(((float)h) * mag);
   if (pixels != NULL) {
      int new_ncolors=0;

      for (i=0; i < *ncolors_return; i++) {
         if (pixels_used[i]) {
            new_ncolors++;
         }
      }
      if (new_ncolors < *ncolors_return) {
         /* some pixels are not used */
         char *new_color_char=(char*)malloc(
               new_ncolors*chars_per_pixel*sizeof(char));
         char **new_color_str=(char**)malloc(new_ncolors*sizeof(char *));
         int *new_pixels=(int*)malloc(new_ncolors*sizeof(int));

         if (new_color_char == NULL || new_color_str == NULL ||
               new_pixels == NULL) {
            FailAllocMessage();
         } else {
            int new_index=0, ok=TRUE;

            for (i=0; i < *ncolors_return; i++) {
               if (pixels_used[i]) {
                  int j;

                  strncpy(&new_color_char[new_index*chars_per_pixel],
                        &(*color_char)[i*chars_per_pixel],
                        chars_per_pixel*sizeof(char));
                  new_color_str[new_index] = UtilStrDup((*color_str)[i]);
                  if (new_color_str[new_index] == NULL) {
                     for (j=0; j < i; j++) {
                        free(new_color_str[j]);
                     }
                     free(new_color_char);
                     free(new_color_str);
                     free(new_pixels);
                     new_color_char = NULL;
                     new_color_str = NULL;
                     new_pixels = NULL;
                     ok = FALSE;
                     break;
                  }
                  new_pixels[new_index] = (*pixels)[i];
                  new_index++;
               }
            }
            if (ok) {
               char *tmp_char, **tmp_str;
               int *tmp_pixels;

               free(*color_char);
               *color_char = new_color_char;
               for (i=0; i < *ncolors_return; i++) {
                  free((*color_str)[i]);
               }
               free(*color_str);
               *color_str = new_color_str;
               free(*pixels);
               *pixels = new_pixels;
               if (*first_pixel_is_bg_return && pixels_used[0]) {
                  *first_pixel_is_bg_return = FALSE;
               }
               new_color_char = NULL;
               new_color_str = NULL;
               new_pixels = NULL;
               *ncolors_return = new_ncolors;
            }
         }
         if (new_color_char != NULL) free(new_color_char);
         if (new_color_str != NULL) free(new_color_str);
         if (new_pixels != NULL) free(new_pixels);
      }
   }
   if (pixels_used != NULL) free(pixels_used);
   gnNumNewColorsInPixmapFile = maxColors - saved_max_colors;
   if (gnNumNewColorsInPixmapFile != 0) {
      sprintf(gszMsgBox, "%1d additional color allocated.",
            gnNumNewColorsInPixmapFile);
      Msg(gszMsgBox);
   }
   return BitmapSuccess;
}

#define GETVALUE(val,name) ScanValue("%d", &(val), name, "xpixmap")

void ReadXPmObj (FP, Inbuf, ObjPtr)
   FILE			* FP;
   char			* Inbuf;
   struct ObjRec	* * ObjPtr;
{
   struct XPmRec	* xpm_ptr;
   char			color_s[40], * s, inbuf[MAXSTRING], * c_ptr, * line;
   int			ltx, lty, rbx, rby, i, j, k, image_w, image_h;
   int			ncolors, * pixels, len, index, fill, color_index;
   int			* red=NULL, * green=NULL, * blue=NULL;
   int			unrecognized_color = FALSE, rotation, chars_per_pixel;
   int			first_pixel_is_bg, first_pixel_maybe_bg, new_alloc;
   int			id=0, rotate=ROTATE0, flip=NO_FLIP, locked=FALSE;
   int			compressed=FALSE, real_x=0, real_y=0;
   int			transformed=FALSE, invisible=FALSE;
   char			* xpm_data=NULL, * color_char, * * color_str;
   Pixmap		pixmap=None, bitmap=None;
   XImage		*image=NULL, *bitmap_image=NULL;
   struct XfrmMtrxRec	*ctm=NULL;
   struct BBRec		orig_obbox;

   *ObjPtr = NULL;

   s = FindChar ((int)'(', Inbuf);
   s = ParseStr (s, (int)',', color_s, sizeof(color_s));

   InitScan (s, "\t\n, ");

   rotation = 0;
   chars_per_pixel = 1;
   first_pixel_maybe_bg = TRUE;
   first_pixel_is_bg = TRUE;
   if (fileVersion <= 9)
   {
      sprintf (gszMsgBox, "Invalid X Pixmap version (%1d).", fileVersion);
      if (PRTGIF)
         fprintf (stderr, "%s\n", gszMsgBox);
      else
         Msg (gszMsgBox);
      return;
   }
   else if (fileVersion <= 13)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (ncolors,  "ncolors") == INVALID ||
          GETVALUE (id,       "id") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 14)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (ncolors,  "ncolors") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 22)
   {
      if (GETVALUE (ltx,               "ltx") == INVALID ||
          GETVALUE (lty,               "lty") == INVALID ||
          GETVALUE (rbx,               "rbx") == INVALID ||
          GETVALUE (rby,               "rby") == INVALID ||
          GETVALUE (fill,              "fill") == INVALID ||
          GETVALUE (ncolors,           "ncolors") == INVALID ||
          GETVALUE (chars_per_pixel,   "chars_per_pixel") == INVALID ||
          GETVALUE (first_pixel_is_bg, "first_pixel_is_bg") == INVALID ||
          GETVALUE (id,                "id") == INVALID ||
          GETVALUE (rotation,          "rotation") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
      first_pixel_maybe_bg = FALSE;
   }
   else if (fileVersion <= 25)
   {
      if (GETVALUE (ltx,               "ltx") == INVALID ||
          GETVALUE (lty,               "lty") == INVALID ||
          GETVALUE (rbx,               "rbx") == INVALID ||
          GETVALUE (rby,               "rby") == INVALID ||
          GETVALUE (fill,              "fill") == INVALID ||
          GETVALUE (ncolors,           "ncolors") == INVALID ||
          GETVALUE (chars_per_pixel,   "chars_per_pixel") == INVALID ||
          GETVALUE (first_pixel_is_bg, "first_pixel_is_bg") == INVALID ||
          GETVALUE (id,                "id") == INVALID ||
          GETVALUE (rotation,          "rotation") == INVALID ||
          GETVALUE (image_w,           "image_w") == INVALID ||
          GETVALUE (image_h,           "image_h") == INVALID ||
          GETVALUE (rotate,            "rotate") == INVALID ||
          GETVALUE (flip,              "flip") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
      first_pixel_maybe_bg = FALSE;
   }
   else if (fileVersion <= 32)
   {
      if (GETVALUE (ltx,               "ltx") == INVALID ||
          GETVALUE (lty,               "lty") == INVALID ||
          GETVALUE (rbx,               "rbx") == INVALID ||
          GETVALUE (rby,               "rby") == INVALID ||
          GETVALUE (fill,              "fill") == INVALID ||
          GETVALUE (ncolors,           "ncolors") == INVALID ||
          GETVALUE (chars_per_pixel,   "chars_per_pixel") == INVALID ||
          GETVALUE (first_pixel_is_bg, "first_pixel_is_bg") == INVALID ||
          GETVALUE (id,                "id") == INVALID ||
          GETVALUE (rotation,          "rotation") == INVALID ||
          GETVALUE (image_w,           "image_w") == INVALID ||
          GETVALUE (image_h,           "image_h") == INVALID ||
          GETVALUE (rotate,            "rotate") == INVALID ||
          GETVALUE (flip,              "flip") == INVALID ||
          GETVALUE (locked,            "locked") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
      first_pixel_maybe_bg = FALSE;
   }
   else
   {
      if (GETVALUE (ltx,               "ltx") == INVALID ||
          GETVALUE (lty,               "lty") == INVALID ||
          GETVALUE (rbx,               "rbx") == INVALID ||
          GETVALUE (rby,               "rby") == INVALID ||
          GETVALUE (fill,              "fill") == INVALID ||
          GETVALUE (ncolors,           "ncolors") == INVALID ||
          GETVALUE (chars_per_pixel,   "chars_per_pixel") == INVALID ||
          GETVALUE (first_pixel_is_bg, "first_pixel_is_bg") == INVALID ||
          GETVALUE (id,                "id") == INVALID ||
          GETVALUE (rotation,          "rotation") == INVALID ||
          GETVALUE (image_w,           "image_w") == INVALID ||
          GETVALUE (image_h,           "image_h") == INVALID ||
          GETVALUE (rotate,            "rotate") == INVALID ||
          GETVALUE (flip,              "flip") == INVALID ||
          GETVALUE (locked,            "locked") == INVALID ||
          GETVALUE (compressed,        "compressed") == INVALID ||
          GETVALUE (transformed,       "transformed") == INVALID ||
          GETVALUE (invisible,         "invisible") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
      first_pixel_maybe_bg = FALSE;
   }

   if (fileVersion <= 22)
   {
      image_w = rbx-ltx;
      image_h = rby-lty;
      rotate = ROTATE0;
      flip = 0;
   }
   if (fileVersion >= 33 && transformed)
   {
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
   }
   fill = UpgradePenFill (fill);

   *ObjPtr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));
   xpm_ptr = (struct XPmRec *)malloc(sizeof(struct XPmRec));
   if (xpm_ptr == NULL) FailAllocMessage();
   memset(xpm_ptr, 0, sizeof(struct XPmRec));

   color_index = QuickFindColorIndex(*ObjPtr, color_s, &new_alloc, TRUE);

   (*ObjPtr)->color = color_index;
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->type = OBJ_XPM;
   (*ObjPtr)->obbox.ltx = (*ObjPtr)->bbox.ltx = (*ObjPtr)->x = ltx;
   (*ObjPtr)->obbox.lty = (*ObjPtr)->bbox.lty = (*ObjPtr)->y = lty;
   (*ObjPtr)->obbox.rbx = (*ObjPtr)->bbox.rbx = rbx;
   (*ObjPtr)->obbox.rby = (*ObjPtr)->bbox.rby = rby;
   (*ObjPtr)->detail.xpm = xpm_ptr;
   (*ObjPtr)->ctm = ctm;
   (*ObjPtr)->invisible = invisible;

   if (ctm != NULL) {
      memcpy(&(*ObjPtr)->orig_obbox, &orig_obbox, sizeof(struct BBRec));
      (*ObjPtr)->x = real_x;
      (*ObjPtr)->y = real_y;
      GetTransformedOBBoxOffsetVs(*ObjPtr, (*ObjPtr)->rotated_obbox);
   }
   color_char = (char*)malloc(ncolors*chars_per_pixel*sizeof(char));
   color_str = (char**)malloc(ncolors*sizeof(char*));
   pixels = (int*)malloc(ncolors*sizeof(int));
   if (color_char == NULL || color_str == NULL || pixels == NULL) {
      FailAllocMessage();
   }
   if (fileVersion >= 25 && (PRTGIF || !colorDisplay)) {
      red = (int*)malloc(ncolors*sizeof(int));
      green = (int*)malloc(ncolors*sizeof(int));
      blue = (int*)malloc(ncolors*sizeof(int));
      if (red == NULL || green == NULL || blue == NULL) FailAllocMessage();
   }
   xpm_ptr->pixmap = None;
   xpm_ptr->image = NULL;
   xpm_ptr->bitmap = None;
   xpm_ptr->bitmap_image = NULL;
   xpm_ptr->cached_pixmap = None;
   xpm_ptr->cached_bitmap = None;
   xpm_ptr->cached_zoom = 0;
   xpm_ptr->data = NULL;
   xpm_ptr->fill = fill;
   xpm_ptr->rotate = rotate;
   xpm_ptr->flip = flip;
   xpm_ptr->cached_rotate = INVALID;
   xpm_ptr->cached_flip = 0;
   xpm_ptr->cached_w = xpm_ptr->cached_h = 0;
   xpm_ptr->cached_color = (-1);
   xpm_ptr->image_w = image_w;
   xpm_ptr->image_h = image_h;

   xpm_ptr->ncolors = ncolors;
   xpm_ptr->chars_per_pixel = chars_per_pixel;
   xpm_ptr->first_pixel_is_bg = first_pixel_is_bg;
   xpm_ptr->color_char = color_char;
   xpm_ptr->color_str = color_str;
   xpm_ptr->pixels = pixels;
   xpm_ptr->red = red;
   xpm_ptr->green = green;
   xpm_ptr->blue = blue;

   for (i = 0; i < ncolors; i++)
   {
      fgets (inbuf, MAXSTRING, FP);
      scanLineNum++;
      c_ptr = FindChar ((int)'"', inbuf);
      for (j = 0; j < chars_per_pixel; j++)
         color_char[i*chars_per_pixel+j] = c_ptr[j];
      c_ptr = FindChar ((int)'"', c_ptr);
      c_ptr = FindChar ((int)'"', c_ptr);
      c_ptr = ParseStr (c_ptr, (int)'"', color_s, sizeof(color_s));
      if (!PRTGIF)
      {
         if (i == 0 && (first_pixel_is_bg || (first_pixel_maybe_bg &&
               (color_char[i]=='`' || color_char[i]==' ')))) {
            strcpy (color_s, myBgColorStr);
            pixels[i] = myBgPixel;
            xpm_ptr->first_pixel_is_bg = first_pixel_is_bg = TRUE;
         } else if (UtilStrICmp(color_s, "None") == 0) {
            pixels[i] = (-1);
         } else if ((index = QuickFindColorIndex(NULL, color_s, &new_alloc,
               TRUE)) == INVALID) {
            sprintf(gszMsgBox, "Can not allocate color '%s', use '%s' instead.",
                  color_s, colorMenuItems[colorIndex]);
            Msg (gszMsgBox);
            allocColorFailed = TRUE;
            strcpy (color_s, colorMenuItems[colorIndex]);
            pixels[i] = colorPixels[colorIndex];
         } else {
            pixels[i] = colorPixels[index];
         }

         len = strlen (color_s);
         color_str[i] = (char*)malloc((len+1)*sizeof(char));
         if (color_str[i] == NULL) FailAllocMessage();
         strcpy(color_str[i], color_s);
      }
      if (fileVersion >= 25 && (PRTGIF || !colorDisplay))
      {  /* has RGB information for PRTGIF */
         InitScan (c_ptr, "\t\n, ");

         if (GETVALUE (red[i],   "red") == INVALID ||
             GETVALUE (green[i], "green") == INVALID ||
             GETVALUE (blue[i],  "blue") == INVALID)
         {
            return;
         }
         if (UtilStrICmp(color_s, "None") == 0) {
            red[i] = green[i] = blue[i] = (-1);
         }
      }
   }
   xpm_ptr->clip_mask = None;

   if (PRTGIF)
   {
      xpm_data = (char*)malloc(image_w*image_h*chars_per_pixel*sizeof(char));
      if (xpm_data == NULL) FailAllocMessage();
      xpm_ptr->data = xpm_data;

      line = (char*)malloc((image_w*chars_per_pixel+20)*sizeof(char));
      if (line == NULL) FailAllocMessage();
      for (i=0; i < image_h; i++, xpm_data += image_w*chars_per_pixel) {
         fgets (line, image_w*chars_per_pixel+20, FP);
         scanLineNum++;
         c_ptr = &line[4];
         strncpy (xpm_data, c_ptr, image_w*chars_per_pixel);
      }
      free(line);
   }
   else
   {
      if (fileVersion >= 25 && !colorDisplay) {
         xpm_data = (char*)malloc(image_w*image_h*chars_per_pixel*sizeof(char));
         if (xpm_data == NULL) FailAllocMessage();
         xpm_ptr->data = xpm_data;
      }
      pixmap = XCreatePixmap (mainDisplay, dummyPixmap, image_w, image_h,
            mainDepth);
      bitmap = XCreatePixmap (mainDisplay, dummyPixmap, image_w, image_h, 1);
      XFillRectangle (mainDisplay, pixmap, xpmGC, 0, 0, image_w, image_h);
      XSetForeground(mainDisplay, xbmGC, 1);
      XFillRectangle (mainDisplay, bitmap, xbmGC, 0, 0, image_w, image_h);
      XSetForeground(mainDisplay, xbmGC, 0);
      image = XGetImage (mainDisplay, pixmap, 0, 0, image_w, image_h, AllPlanes,
            ZPixmap);
      bitmap_image = XGetImage (mainDisplay, bitmap, 0, 0, image_w, image_h, 1,
            ZPixmap);
      if (image == NULL || bitmap_image == NULL)
      {
         Msg ("XGetImage() failed!  May have run out of memory!");
         XFreePixmap (mainDisplay, pixmap);
         XFreePixmap (mainDisplay, bitmap);
         if (image != NULL) XDestroyImage(image);
         if (bitmap_image != NULL) XDestroyImage(bitmap_image);
         return;
      }

      BuildXPmBuckets (ncolors, NULL, chars_per_pixel, color_char);

      line = (char*)malloc((image_w*chars_per_pixel+20)*sizeof(char));
      if (line == NULL) FailAllocMessage();
      for (i = 0; i < image_h; i++) {
         fgets(line, image_w*chars_per_pixel+20, FP);
         scanLineNum++;
         c_ptr = &line[4];
         if (xpm_data != NULL) {
            strncpy(xpm_data, c_ptr, image_w*chars_per_pixel);
         }
         for (j = 0; j < image_w; j++, c_ptr+=chars_per_pixel) {
            k = XPmLookUp (INVALID, chars_per_pixel, c_ptr);
            if (k == INVALID) {
               XPutPixel(image, j, i, pixels[0]);
               unrecognized_color = TRUE;
            } else if (pixels[k] == (-1)) {
               XPutPixel(bitmap_image, j, i, 0);
               XPutPixel(image, j, i, colorPixels[color_index]);
            } else {
               XPutPixel(image, j, i, pixels[k]);
            }
         }
         if (xpm_data != NULL) xpm_data += image_w*chars_per_pixel;
      }
      free(line);
      XPutImage (mainDisplay,pixmap,xpmGC,image,0,0,0,0,image_w,image_h);
      XPutImage (mainDisplay,bitmap,xbmGC,bitmap_image,0,0,0,0,image_w,image_h);
      xpm_ptr->pixmap = pixmap;
      xpm_ptr->image = image;
      xpm_ptr->bitmap = bitmap;
      xpm_ptr->bitmap_image = bitmap_image;

      if (unrecognized_color)
      {
         sprintf (gszMsgBox, "Unrecognized colors shown as '%s'", color_str[0]);
         Msg (gszMsgBox);
      }
   }
   if (fileVersion < 33 && (rotate != ROTATE0 || flip != NO_FLIP)) {
      if (rotate == ROTATE90 || rotate == ROTATE270) {
         int h=(*ObjPtr)->obbox.rbx-(*ObjPtr)->obbox.ltx;
         int w=(*ObjPtr)->obbox.rby-(*ObjPtr)->obbox.lty;

         (*ObjPtr)->obbox.rby = (*ObjPtr)->obbox.lty + h;
         (*ObjPtr)->obbox.rbx = (*ObjPtr)->obbox.ltx + w;
      }
      ltx = ((*ObjPtr)->obbox.ltx);
      lty = ((*ObjPtr)->obbox.lty);
      SetRotatePivotByObject(*ObjPtr);

      if (flip & HORI_EVEN) {
         ShearObj(*ObjPtr, CORNER_LEFT, 0, 0, -1000, 1000, &ltx, &lty);
      }
      if (flip & VERT_EVEN) {
         ShearObj(*ObjPtr, CORNER_TOP, 0, 0, 1000, -1000, &ltx, &lty);
      }
      if (rotate == ROTATE0) {
         if (flip & (HORI_ODD | VERT_ODD)) {
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            if (flip & HORI_ODD) {
               ShearObj(*ObjPtr, CORNER_LEFT, 0, 0, -1000, 1000, &ltx, &lty);
            }
            if (flip & VERT_ODD) {
               ShearObj(*ObjPtr, CORNER_TOP, 0, 0, 1000, -1000, &ltx, &lty);
            }
            RotateObj(*ObjPtr, CORNER_LT, COUNTER90, &ltx, &lty);
         }
      } else {
         switch (rotate) {
         case ROTATE90:
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            if (flip & HORI_ODD) {
               ShearObj(*ObjPtr, CORNER_LEFT, 0, 0, -1000, 1000, &ltx, &lty);
            }
            if (flip & VERT_ODD) {
               ShearObj(*ObjPtr, CORNER_TOP, 0, 0, 1000, -1000, &ltx, &lty);
            }
            break;
         case ROTATE180:
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            if (flip & HORI_ODD) {
               ShearObj(*ObjPtr, CORNER_LEFT, 0, 0, -1000, 1000, &ltx, &lty);
            }
            if (flip & VERT_ODD) {
               ShearObj(*ObjPtr, CORNER_TOP, 0, 0, 1000, -1000, &ltx, &lty);
            }
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            break;
         case ROTATE270:
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            if (flip & HORI_ODD) {
               ShearObj(*ObjPtr, CORNER_LEFT, 0, 0, -1000, 1000, &ltx, &lty);
            }
            if (flip & VERT_ODD) {
               ShearObj(*ObjPtr, CORNER_TOP, 0, 0, 1000, -1000, &ltx, &lty);
            }
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            break;
         }
      }
      xpm_ptr->rotate = ROTATE0;
      xpm_ptr->flip = NO_FLIP;
   }
   if (xpm_ptr->rotate != ROTATE0) {
      fprintf(stderr, "XPm object rotation is not 0!\n");
   }
}

void FreeXPmObj(ObjPtr)
   struct ObjRec *ObjPtr;
{
   register int i, ncolors;
   struct XPmRec *xpm_ptr=ObjPtr->detail.xpm;

   if (xpm_ptr->pixmap != None) {
      free(xpm_ptr->pixels);
      if (xpm_ptr->red != NULL) free(xpm_ptr->red);
      if (xpm_ptr->green != NULL) free(xpm_ptr->green);
      if (xpm_ptr->blue != NULL) free(xpm_ptr->blue);
      free(xpm_ptr->color_char);
      ncolors = xpm_ptr->ncolors;
      for (i = 0; i < ncolors; i++) free(xpm_ptr->color_str[i]);
      free(xpm_ptr->color_str);
      XFreePixmap(mainDisplay, xpm_ptr->pixmap);
   }
   if (xpm_ptr->bitmap != None) XFreePixmap(mainDisplay, xpm_ptr->bitmap);
   if (xpm_ptr->image != NULL) XDestroyImage(xpm_ptr->image);
   if (xpm_ptr->bitmap_image != NULL) XDestroyImage(xpm_ptr->bitmap_image);
   if (xpm_ptr->cached_pixmap != None) {
      XFreePixmap(mainDisplay, xpm_ptr->cached_pixmap);
   }
   if (xpm_ptr->cached_bitmap != None) {
      XFreePixmap(mainDisplay, xpm_ptr->cached_bitmap);
   }
   if (xpm_ptr->clip_mask != None) {
      XFreePixmap(mainDisplay, xpm_ptr->clip_mask);
   }
   if (xpm_ptr->data != NULL) free(xpm_ptr->data);
   free(xpm_ptr);
   free(ObjPtr);
}
