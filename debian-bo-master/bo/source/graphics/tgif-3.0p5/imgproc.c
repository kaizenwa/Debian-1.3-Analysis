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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/imgproc.c,v 3.0 1996/05/06 16:05:35 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "file.e"
#include "grid.e"
#ifndef _NO_EXTERN
#include "imgproc.e"
#endif
#include "mainloop.e"
#include "mainmenu.e"
#include "menu.e"
#include "msg.e"
#include "names.e"
#include "obj.e"
#include "page.e"
#include "raster.e"
#include "select.e"
#include "setup.e"
#include "util.e"
#include "xbitmap.e"
#include "xpixmap.e"

typedef void (ImageMapColorFunc)ARGS_DECL((int nColorIndex, XColor*));
typedef int (ConvolveFunc)ARGS_DECL((int x, int y));

#define ZERO_TOL (1.0e-5)

#define HISTOGRAMCOUNT(i) (gpHistogram[(i)].pixel)
#define HISTOGRAMRED(i) (gpHistogram[(i)].red)
#define HISTOGRAMGREEN(i) (gpHistogram[(i)].green)
#define HISTOGRAMBLUE(i) (gpHistogram[(i)].blue)

#define IMAGEPROC_MAKEGRAY 0
#define IMAGEPROC_INVERTCOLOR 1
#define IMAGEPROC_INTERPOLATECOLOR 2
#define IMAGEPROC_BRIGHTDARKEN 3
#define IMAGEPROC_CHANGESATURATION 4
#define IMAGEPROC_CHANGEHUE 5
#define IMAGEPROC_CONTRAST 6
#define IMAGEPROC_COLORBALANCE 7
#define IMAGEPROC_GAMMA 8

#define IMAGEPROC_EDGEDETECT 9
#define IMAGEPROC_EMBOSS 10
#define IMAGEPROC_REDUCECOLORS 11
#define IMAGEPROC_REDUCETOPIXMAPCOLORS 12
#define IMAGEPROC_SETDEFAULTCOLORLEVELS 13
#define IMAGEPROC_REDUCETODEFAULTCOLORS 14
#define IMAGEPROC_DEFAULTERRORDIFFUSE 15
#define IMAGEPROC_SPREAD 16
#define IMAGEPROC_SHARPEN 17
#define IMAGEPROC_BLUR3 18
#define IMAGEPROC_BLUR5 19
#define IMAGEPROC_BLUR7 20
#define IMAGEPROC_RUNBGGEN 21
#define IMAGEPROC_CIRCULARBGGEN 22
#define IMAGEPROC_REGENERATEIMAGE 23
#define IMAGEPROC_CROPIMAGE 24
#define IMAGEPROC_GETCOLOR 25
#define IMAGEPROC_REPLACECOLOR 26
#define IMAGEPROC_FLOODFILL 27
#define IMAGEPROC_CREATECONTOUR 28
#define IMAGEPROC_SUBTRACT 29
#define IMAGEPROC_ALPHACOMBINE 30

#define MAXIMAGEPROCS 31

int numImageProc=MAXIMAGEPROCS;
int gnInImageProc=FALSE;
int gnConvolving=FALSE;
int gnNumNewColorsInPixmapFile=0;

char gszImageProcXPmFile[MAXPATHLENGTH+1];

static void *gpImageMapColorFunc=NULL;
static void *gpConvolveFunc=NULL;
static int gnCombining=FALSE;

static int gnCombineW=0, gnCombineH=0;

static char bggenToXpmCmd[MAXSTRING+1];

static XColor gDefErrorDiffuseLevel;

char *imageProcMenuStr[]={
   "MakeGray             ",
   "InvertColor          ",
   "InterpolateColor     ",
   "Brighten/Darken      ",
   "ChangeSaturation     ",
   "ChangeHue            ",
   "ContrastEnhance      ",
   "ColorBalance         ",
   "GammaCorrect         ",
   "EdgeDetect           ",
   "Emboss               ",
   "ReduceColors         ",
   "ReduceToPixmapColors ",
   "SetDefaultColorLevels",
   "ReduceToDefaultColors",
   "DefaultErrorDiffuse  ",
   "Spread               ",
   "Sharpen              ",
   "Blur(3x3)            ",
   "Blur(5x5)            ",
   "Blur(7x7)            ",
   "RunBggen             ",
   "CircularBggen        ",
   "RegenerateImage      ",
   "CropImage            ",
   "GetColor             ",
   "ReplaceColor         ",
   "FloodFill            ",
   "CreateContour        ",
   "Subtract             ",
   "AlphaCombine         ",
   NULL
};
static char *imageProcMenuDesc[]={
   "Convert a selected image into gray-scale",
   "Subtract every pixel of a selected image from White (in RGB-model)",
   "Map the pixel intensities of a selected image between two specified colors",
   "Brighten or darken a selected image",
   "Change saturation of a selected image",
   "Change hue of a selected image",
   "Adjust contrast of a selected image",
   "Balance the RGB components of a selected image",
   "Apply gamma correction a selected image",
   "Perform edge-detection on a selected image",
   "Convert a selected image to gray and then emboss",
   "Reduce the number of colors of a selected image",
   "Reduce the colors of a selected image to a the colors in an XPM file",
   "Set the number of R G B color levels for the ``standard colors''",
   "Reduce the colors of a selected image to standard colors",
   "Error diffuse to reduce the colors of a selected image to standard colors",
   "Spread the pixels of a selected image around",
   "Sharpen a selected image",
   "Blur a selected image using a 3 by 3 filter",
   "Blur a selected image using a 5 by 5 filter",
   "Blur a selected image using a 7 by 7 filter",
   "Run 'bggen' to create an image using the size of a selected image",
   "Create a gray circular image using the size of a selected image",
   "Regenerated a selected image",
   "Crop a selected image",
   "Pick a color from a selected image to be the current color",
   "Replace a color in a selected image with the current color",
   "Flood-filling a selected image with the current color",
   "Generate a contour from a point in a selected image",
   "Subtract one image from another",
   "Combine 2 images using another image as the alpha channel",
   NULL
};

static
int CheckSelectionForImageProc(pszProc)
   char *pszProc;
{
   char szBuf[MAXSTRING+1];

   strcpy(szBuf, pszProc);
   UtilTrimBlanks(szBuf);
   if (curChoice != NOTHING || topSel == NULL || topSel != botSel ||
         topSel->obj->type != OBJ_XPM) {
      sprintf(gszMsgBox, "Please select only one X Pixmap object for %s().",
            szBuf);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   return TRUE;
}

#define LONG_AXIS_IS_RED 0
#define LONG_AXIS_IS_GREEN 1
#define LONG_AXIS_IS_BLUE 2

typedef struct CubeRec { /* not really a ``cube'' */
   int min_index, max_index, level, long_axis;
   unsigned long num_points;
   unsigned short red_length, green_length, blue_length;
} *CubePointer;

typedef struct HGBucketRec {
   int index;
   struct HGBucketRec *next;
} *HGBucketPoiner;

static struct HGBucketRec *gaHGBucket[256];

static XColor *gpHistogram=NULL;
static int *gpnSortedIndex=NULL;
static int **gnOrigImageIndex=NULL, **gnFinalImageIndex=NULL;
static int gnImageW=(-1), gnImageH=(-1);
static int *gpnPixelToIndexMap=NULL;
static int gnHistogramEntries=0, gnHistogramSize=0;
static int gnQuantizingLevels=222, gnUserSpecifiedLevels=(-1);
static struct CubeRec *gpCube=NULL;
static int gnCubeEntries=0;
static int gnTransparentIndex=(-1);

static
int GetXPmImages(xpm_ptr, p_image, p_bitmap_image)
   struct XPmRec *xpm_ptr;
   XImage **p_image, **p_bitmap_image;
{
   Pixmap pixmap=xpm_ptr->pixmap, bitmap=xpm_ptr->bitmap;
   int image_w=xpm_ptr->image_w, image_h=xpm_ptr->image_h;

   *p_image = XGetImage(mainDisplay, pixmap, 0, 0, image_w, image_h, AllPlanes,
         ZPixmap);
   if (bitmap != None) {
      *p_bitmap_image = XGetImage(mainDisplay, bitmap, 0, 0, image_w, image_h,
         1, ZPixmap);
   } else {
      *p_bitmap_image = NULL;
   }
   if ((*p_image) == NULL || (bitmap != None && (*p_bitmap_image) == NULL)) {
      sprintf(gszMsgBox, "%s.\n\n%s.",
            "Can not generate an image", "Image processing aborted");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   return TRUE;
}

void CleanUpConvolution()
{
   register int i;

   if (gpnPixelToIndexMap != NULL) {
      free(gpnPixelToIndexMap);
      gpnPixelToIndexMap = NULL;
   }
   if (gpHistogram != NULL) {
      free(gpHistogram);
      gpHistogram = NULL;
   }
   if (gpnSortedIndex != NULL) {
      free(gpnSortedIndex);
      gpnSortedIndex = NULL;
   }
   gnHistogramEntries = gnHistogramSize = 0;

   if (gpCube != NULL) {
      free(gpCube);
      gpCube = NULL;
   }
   gnCubeEntries = 0;
   if (gnOrigImageIndex != NULL) {
      for (i=0; i < gnImageH; i++) {
         if (gnOrigImageIndex[i] != NULL) {
            free(gnOrigImageIndex[i]);
         } else {
            break;
         }
      }
      free(gnOrigImageIndex);
      gnOrigImageIndex = NULL;
   }
   if (gnFinalImageIndex != NULL) {
      for (i=0; i < gnImageH; i++) {
         if (gnFinalImageIndex[i] != NULL) {
            free(gnFinalImageIndex[i]);
         } else {
            break;
         }
      }
      free(gnFinalImageIndex);
      gnFinalImageIndex = NULL;
   }
   gnImageW = gnImageH = (-1);

   for (i=0; i < 256; i++) {
      if (gaHGBucket[i] != NULL) {
         struct HGBucketRec *bucket_ptr=gaHGBucket[i], *next_bucket;

         for ( ; bucket_ptr != NULL; bucket_ptr=next_bucket) {
            next_bucket = bucket_ptr->next;
            free(bucket_ptr);
         }
         gaHGBucket[i] = NULL;
      }
   }
}

static
int GetOrAllocHistogramIndex(pcolor)
   XColor *pcolor;
{
   int hashvalue=0;

   if (pcolor == NULL) {
      if (gnTransparentIndex != (-1)) return gnTransparentIndex;
   } else {
      struct HGBucketRec *bucket_ptr;

      hashvalue = (int)((pcolor->red ^ pcolor->green ^ pcolor->blue) & 0xff);
      if (gaHGBucket[hashvalue] != NULL) {
         struct HGBucketRec *bucket_ptr=gaHGBucket[hashvalue];

         for ( ; bucket_ptr != NULL; bucket_ptr=bucket_ptr->next) {
            int i=bucket_ptr->index;

            if (gpHistogram[i].red == pcolor->red &&
                  gpHistogram[i].green == pcolor->green &&
                  gpHistogram[i].blue == pcolor->blue) {
               HISTOGRAMCOUNT(i)++;
               return i;
            }
         }
      }
      bucket_ptr = (struct HGBucketRec *)malloc(sizeof(struct HGBucketRec));
      if (bucket_ptr == NULL) {
         FailAllocMessage();
         return (-1);
      }
      bucket_ptr->index = gnHistogramEntries;
      bucket_ptr->next = gaHGBucket[hashvalue];
      gaHGBucket[hashvalue] = bucket_ptr;
   }
   if (gnHistogramEntries >= gnHistogramSize) {
      gnHistogramSize += 256;
      if ((gpHistogram=(XColor*)realloc(gpHistogram,
            gnHistogramSize*sizeof(XColor))) == NULL) {
         FailAllocMessage();
         return (-1);
      }
   }
   memset(&gpHistogram[gnHistogramEntries], 0, sizeof(XColor));
   HISTOGRAMCOUNT(gnHistogramEntries) = 1;
   if (pcolor == NULL) {
      gpHistogram[gnHistogramEntries].red = 0;
      gpHistogram[gnHistogramEntries].green = 0;
      gpHistogram[gnHistogramEntries].blue = 0;
   } else {
      gpHistogram[gnHistogramEntries].red = pcolor->red;
      gpHistogram[gnHistogramEntries].green = pcolor->green;
      gpHistogram[gnHistogramEntries].blue = pcolor->blue;
   }
   return (gnHistogramEntries++);
}

static
int CreatePixelToIndexMapping()
{
   register int i;
   int max_pixel=(-1);

   for (i=0; i < maxColors; i++) {
      if (colorPixels[i] > max_pixel) {
         max_pixel = colorPixels[i];
      }
   }
   if (max_pixel == (-1)) return FALSE;

   gpnPixelToIndexMap = (int*)malloc((max_pixel+1)*sizeof(int));
   if (gpnPixelToIndexMap == NULL) {
      FailAllocMessage();
      CleanUpConvolution();
      return FALSE;
   }
   memset(gpnPixelToIndexMap, (-1), (max_pixel+1)*sizeof(int));
   for (i=0; i < maxColors; i++) {
      gpnPixelToIndexMap[colorPixels[i]] = i;
   }
   return TRUE;
}

static
int CreateObjPixelToIndexMapping(xpm_ptr)
   struct XPmRec *xpm_ptr;
{
   register int i;
   int max_pixel=(-1), start_index, *pixel_to_index_map=NULL;

   gnTransparentIndex = (-1);
   for (i=0; i < maxColors; i++) {
      if (colorPixels[i] > max_pixel) {
         max_pixel = colorPixels[i];
      }
   }
   if (max_pixel == (-1)) return FALSE;

   gpnPixelToIndexMap = (int*)malloc((max_pixel+1)*sizeof(int));
   pixel_to_index_map = (int*)malloc((max_pixel+1)*sizeof(int));
   if (gpnPixelToIndexMap == NULL || pixel_to_index_map == NULL) {
      if (gpnPixelToIndexMap != NULL) free(gpnPixelToIndexMap);
      if (pixel_to_index_map != NULL) free(pixel_to_index_map);
      gpnPixelToIndexMap = NULL;
      FailAllocMessage();
      CleanUpConvolution();
      return FALSE;
   }
   memset(gpnPixelToIndexMap, (-1), (max_pixel+1)*sizeof(int));
   memset(pixel_to_index_map, (-1), (max_pixel+1)*sizeof(int));
   for (i=0; i < maxColors; i++) {
      pixel_to_index_map[colorPixels[i]] = i;
   }
   start_index = (xpm_ptr->first_pixel_is_bg ? 1 : 0);
   for (i=start_index; i < xpm_ptr->ncolors; i++) {
      XColor xcolor;
      int pixel=xpm_ptr->pixels[i];

      memset(&xcolor, 0, sizeof(XColor));
      if (UtilStrICmp(xpm_ptr->color_str[i], "None") == 0) {
         if (gnTransparentIndex == (-1)) {
            gnTransparentIndex = GetOrAllocHistogramIndex(NULL);
         }
      } else {
         ((ImageMapColorFunc*)gpImageMapColorFunc)(pixel_to_index_map[pixel],
               &xcolor);
         gpnPixelToIndexMap[pixel] = GetOrAllocHistogramIndex(&xcolor);
      }
   }
   free(pixel_to_index_map);
   return TRUE;
}

static
int DumpConvolution(fp)
   FILE *fp;
{
   register int j, i;
   int chars_per_pixel=(gnHistogramEntries > 20 ? 2 : 1), target_percent;
   char c0[27], c1[11];

   strcpy(c0, "abcdefghijklmnopqrstuvwxyz");
   strcpy(c1, "0123456789");
   if (fprintf(fp, "#define conv_format 1\n") == EOF ||
         fprintf(fp, "#define conv_width %1d\n", gnImageW) == EOF ||
         fprintf(fp, "#define conv_height %1d\n", gnImageH) == EOF ||
         fprintf(fp, "#define conv_ncolors %1d\n", gnHistogramEntries) == EOF ||
         fprintf(fp, "#define conv_chars_per_pixel %1d\n",
         chars_per_pixel) == EOF ||
         fprintf(fp, "static char *conv_colors[] = {\n") == EOF) {
      writeFileFailed = TRUE;
   }
   for (j=0; j < gnHistogramEntries; j++) {
      int red=(int)gpHistogram[j].red;
      int green=(int)gpHistogram[j].green;
      int blue=(int)gpHistogram[j].blue;

      if (gnTransparentIndex == j) {
         if (chars_per_pixel == 1) {
            if (fprintf(fp, "   \"%c\", \"None\"", c0[j]) == EOF) {
               writeFileFailed = TRUE;
            }
         } else {
            if (fprintf(fp, "   \"%c%c\", \"None\"",
                  c0[(int)(j/10)], c1[j % 10]) == EOF) {
               writeFileFailed = TRUE;
            }
         }
      } else {
         if (chars_per_pixel == 1) {
            if (fprintf(fp, "   \"%c\", \"#%04x%04x%04x\"",
                  c0[j], red&0x0ffff, green&0x0ffff, blue&0x0ffff) == EOF) {
               writeFileFailed = TRUE;
            }
         } else {
            if (fprintf(fp, "   \"%c%c\", \"#%04x%04x%04x\"",
                  c0[(int)(j/10)], c1[j % 10],
                  red&0x0ffff, green&0x0ffff, blue&0x0ffff) == EOF) {
               writeFileFailed = TRUE;
            }
         }
      }
      if (j == gnHistogramEntries-1) {
         fprintf(fp, "\n};\n");
      } else {
         fprintf(fp, ",\n");
      }
   }
   if (fprintf(fp, "static char *conv_pixels[] = {\n") == EOF) {
      writeFileFailed = TRUE;
   }
   target_percent = 5;
   for (i=0; i < gnImageH; i++) {
      int percent=(i*10000/gnImageH)/100;

      if (percent >= target_percent) {
         sprintf(gszMsgBox, "Generating final pixels: %1d%%", percent);
         SetStringStatus(gszMsgBox);
         XSync(mainDisplay, False);
         while (target_percent <= percent) {
            target_percent += 5;
         }
      }
      fprintf(fp, "\"");
      for (j=0; j < gnImageW; j++) {
         int index=gnFinalImageIndex[i][j];

         if (chars_per_pixel == 1) {
            if (fprintf(fp, "%c", c0[index]) == EOF) {
               writeFileFailed = TRUE;
            }
         } else {
            if (fprintf(fp, "%c%c",
                  c0[(int)(index/10)], c1[index % 10]) == EOF) {
               writeFileFailed = TRUE;
            }
         }
      }
      if (i == gnImageH-1) {
         if (fprintf(fp, "\"\n};\n") == EOF) writeFileFailed = TRUE;
      } else {
         if (fprintf(fp, "\",\n") == EOF) writeFileFailed = TRUE;
      }
   }
   return TRUE;
}

static int gnDebugQuantization=FALSE;

static
int DumpQuantizedConvolution(fp)
   FILE *fp;
{
   register int j, i;
   int chars_per_pixel=(gnCubeEntries > 20 ? 2 : 1), target_percent;
   char c0[27], c1[11];

   strcpy(c0, "abcdefghijklmnopqrstuvwxyz");
   strcpy(c1, "0123456789");
   if (fprintf(fp, "#define conv_format 1\n") == EOF ||
         fprintf(fp, "#define conv_width %1d\n", gnImageW) == EOF ||
         fprintf(fp, "#define conv_height %1d\n", gnImageH) == EOF ||
         fprintf(fp, "#define conv_ncolors %1d\n", gnCubeEntries) == EOF ||
         fprintf(fp, "#define conv_chars_per_pixel %1d\n",
         chars_per_pixel) == EOF ||
         fprintf(fp, "static char *conv_colors[] = {\n") == EOF) {
      writeFileFailed = TRUE;
   }
   if (gnDebugQuantization) {
      fprintf(stderr, "Dumping colors...\n");
   }
   for (j=0; j < gnCubeEntries; j++) {
      int min_index=gpCube[j].min_index;
      int max_index=gpCube[j].max_index;
      int idx=gpnSortedIndex[min_index];
      double num_points=(double)HISTOGRAMCOUNT(idx);
      double red=((double)HISTOGRAMRED(idx))*num_points;
      double green=((double)HISTOGRAMGREEN(idx))*num_points;
      double blue=((double)HISTOGRAMBLUE(idx))*num_points;
      long lred, lgreen, lblue;

      for (i=min_index+1; i <= max_index; i++) {
         double n;

         idx = gpnSortedIndex[i];
         n = (double)HISTOGRAMCOUNT(idx);
         num_points += n;
         red += ((long)HISTOGRAMRED(idx))*n;
         green += ((long)HISTOGRAMGREEN(idx))*n;
         blue += ((long)HISTOGRAMBLUE(idx))*n;
      }
      red /= num_points; green /= num_points; blue /= num_points;
      lred = (long)red; lgreen = (long)green; lblue = (long)blue;
      if (gnDebugQuantization) {
         fprintf(stderr, "\t#%02x%02x%02x %6ld\n",
               (int)((lred>>8) & 0xff), (int)((lgreen>>8) & 0xff),
               (int)((lblue>>8) & 0xff), (int)num_points);
      }
      if (chars_per_pixel == 1) {
         if (fprintf(fp, "   \"%c\", \"#%04x%04x%04x\"",
               c0[j], (int)(lred&0x0ffff), (int)(lgreen&0x0ffff),
               (int)(lblue&0x0ffff)) == EOF) {
            writeFileFailed = TRUE;
         }
      } else {
         if (fprintf(fp, "   \"%c%c\", \"#%04x%04x%04x\"",
               c0[(int)(j/10)], c1[j % 10], (int)(lred&0x0ffff),
               (int)(lgreen&0x0ffff), (int)(lblue&0x0ffff)) == EOF) {
            writeFileFailed = TRUE;
         }
      }
      if (j == gnCubeEntries-1) {
         fprintf(fp, "\n};\n");
      } else {
         fprintf(fp, ",\n");
      }
      /*
       * use the gpHistogram[*].pixel as the reverse color index
       */
      for (i=min_index; i <= max_index; i++) {
         HISTOGRAMCOUNT(gpnSortedIndex[i]) = (long)j;
      }
   }
   if (fprintf(fp, "static char *conv_pixels[] = {\n") == EOF) {
      writeFileFailed = TRUE;
   }
   target_percent = 5;
   for (i=0; i < gnImageH; i++) {
      int percent=(i*10000/gnImageH)/100;

      if (percent >= target_percent) {
         sprintf(gszMsgBox, "Generating final pixels: %1d%%", percent);
         SetStringStatus(gszMsgBox);
         XSync(mainDisplay, False);
         while (target_percent <= percent) {
            target_percent += 5;
         }
      }
      fprintf(fp, "\"");
      for (j=0; j < gnImageW; j++) {
         int orig_index=gnFinalImageIndex[i][j];
         int index=HISTOGRAMCOUNT(orig_index);

         if (chars_per_pixel == 1) {
            if (fprintf(fp, "%c", c0[index]) == EOF) {
               writeFileFailed = TRUE;
            }
         } else {
            if (fprintf(fp, "%c%c",
                  c0[(int)(index/10)], c1[index % 10]) == EOF) {
               writeFileFailed = TRUE;
            }
         }
      }
      if (i == gnImageH-1) {
         if (fprintf(fp, "\"\n};\n") == EOF) writeFileFailed = TRUE;
      } else {
         if (fprintf(fp, "\",\n") == EOF) writeFileFailed = TRUE;
      }
   }
   return TRUE;
}

static
int AlreadySorted(nMinIndex, nMaxIndex, nLongAxis)
   int nMinIndex, nMaxIndex, nLongAxis;
{
   register int i;

   switch (nLongAxis) {
   case LONG_AXIS_IS_RED:
      for (i=nMinIndex; i < nMaxIndex; i++) {
         if (HISTOGRAMRED(gpnSortedIndex[i]) <
               HISTOGRAMRED(gpnSortedIndex[i+1])) {
            return FALSE;
         }
      }
      break;
   case LONG_AXIS_IS_GREEN:
      for (i=nMinIndex; i < nMaxIndex; i++) {
         if (HISTOGRAMGREEN(gpnSortedIndex[i]) <
               HISTOGRAMGREEN(gpnSortedIndex[i+1])) {
            return FALSE;
         }
      }
      break;
   case LONG_AXIS_IS_BLUE:
      for (i=nMinIndex; i < nMaxIndex; i++) {
         if (HISTOGRAMBLUE(gpnSortedIndex[i]) <
               HISTOGRAMBLUE(gpnSortedIndex[i+1])) {
            return FALSE;
         }
      }
      break;
   }
   return TRUE;
}

static
void DebugSortACube(nMinIndex, nMaxIndex, nLevel, nLongAxis)
   int nMinIndex, nMaxIndex, nLevel, nLongAxis;
{
   register int i;
   int sorted=TRUE;

   fprintf(stderr, "Level %1d done (long axis is '%s'):\n", nLevel,
         (nLongAxis==LONG_AXIS_IS_RED ? "red" :
         (nLongAxis==LONG_AXIS_IS_GREEN ? "green" : "blue")));
   for (i=nMinIndex; i <= nMaxIndex; i++) {
      fprintf(stderr, "\t%6ld: %6d %6d %6d\n",
            (long)HISTOGRAMCOUNT(gpnSortedIndex[i]),
            (int)HISTOGRAMRED(gpnSortedIndex[i]),
            (int)HISTOGRAMGREEN(gpnSortedIndex[i]),
            (int)HISTOGRAMBLUE(gpnSortedIndex[i]));
      switch (nLongAxis) {
      case LONG_AXIS_IS_RED:
         if (sorted && i != nMinIndex && HISTOGRAMRED(gpnSortedIndex[i-1]) <
               HISTOGRAMRED(gpnSortedIndex[i])) {
            sorted = FALSE;
         }
         break;
      case LONG_AXIS_IS_GREEN:
         if (sorted && i != nMinIndex && HISTOGRAMGREEN(gpnSortedIndex[i-1]) <
               HISTOGRAMGREEN(gpnSortedIndex[i])) {
            sorted = FALSE;
         }
         break;
      case LONG_AXIS_IS_BLUE:
         if (sorted && i != nMinIndex && HISTOGRAMBLUE(gpnSortedIndex[i-1]) <
               HISTOGRAMBLUE(gpnSortedIndex[i])) {
            sorted = FALSE;
         }
         break;
      }
   }
   if (!sorted) fprintf(stderr, "Not sorted!\n");
}

static
void DisplaySortACube(nMinIndex, nMaxIndex)
   int nMinIndex, nMaxIndex;
{
   register int i;
   int sorted=TRUE;

   for (i=nMinIndex; i <= nMaxIndex; i++) {
      fprintf(stderr, "\t%6ld %6d %6d %6d %6d %3d\n",
            (long)HISTOGRAMCOUNT(gpnSortedIndex[i]),
            (int)HISTOGRAMRED(gpnSortedIndex[i]),
            (int)HISTOGRAMGREEN(gpnSortedIndex[i]),
            (int)HISTOGRAMBLUE(gpnSortedIndex[i]),
            gpnSortedIndex[i], i);
   }
   if (!sorted) fprintf(stderr, "Not sorted!\n");
}

static int dbg_sort=FALSE;

static
void QuickSortACube(nMinIndex, nMaxIndex, nLevel, nLongAxis)
   int nMinIndex, nMaxIndex, nLevel, nLongAxis;
{
   register int i, j;
   int pivot_index, tmp, something_swapped;
   unsigned long pivot_value;

   if (nMinIndex > nMaxIndex) return;
   if (AlreadySorted(nMinIndex, nMaxIndex, nLongAxis)) return;

   pivot_index = nMaxIndex;
   switch (nLongAxis) {
   case LONG_AXIS_IS_RED:
      pivot_value = HISTOGRAMRED(gpnSortedIndex[pivot_index]);
      break;
   case LONG_AXIS_IS_GREEN:
      pivot_value = HISTOGRAMGREEN(gpnSortedIndex[pivot_index]);
      break;
   case LONG_AXIS_IS_BLUE:
      pivot_value = HISTOGRAMBLUE(gpnSortedIndex[pivot_index]);
      break;
   }
   i = nMinIndex;
   j = nMaxIndex-1;
   something_swapped = FALSE;
   do {
      switch (nLongAxis) {
      case LONG_AXIS_IS_RED:
         while (HISTOGRAMRED(gpnSortedIndex[i]) > pivot_value) i++;
         while (j > i && HISTOGRAMRED(gpnSortedIndex[j]) < pivot_value) j--;
         break;
      case LONG_AXIS_IS_GREEN:
         while (HISTOGRAMGREEN(gpnSortedIndex[i]) > pivot_value) i++;
         while (j > i && HISTOGRAMGREEN(gpnSortedIndex[j]) < pivot_value) j--;
         break;
      case LONG_AXIS_IS_BLUE:
         while (HISTOGRAMBLUE(gpnSortedIndex[i]) > pivot_value) i++;
         while (j > i && HISTOGRAMBLUE(gpnSortedIndex[j]) < pivot_value) j--;
         break;
      }
      if (j > i) {
         tmp = gpnSortedIndex[j];
         gpnSortedIndex[j] = gpnSortedIndex[i];
         gpnSortedIndex[i] = tmp;
         if (something_swapped == FALSE) {
            something_swapped = TRUE;
         }
         if (j == i+1) break;
         i++; j--;
      } else {
         break;
      }
   } while (TRUE);
   if (i == nMaxIndex) {
      /* pivot_value is the smallest */
      if (something_swapped) {
         fprintf(stderr, "Huh? nMinIndex=%1d, nMaxIndex=%1d, nLevel=%1d\n",
               nMinIndex, nMaxIndex, nLevel);
      } else {
         QuickSortACube(nMinIndex, j, nLevel+1, nLongAxis);
      }
   } else if (j > i) {
      tmp = gpnSortedIndex[nMaxIndex];
      gpnSortedIndex[nMaxIndex] = gpnSortedIndex[j];
      gpnSortedIndex[j] = tmp;
      QuickSortACube(nMinIndex, j-1, nLevel+1, nLongAxis);
      QuickSortACube(j+1, nMaxIndex, nLevel+1, nLongAxis);
   } else {
      tmp = gpnSortedIndex[nMaxIndex];
      gpnSortedIndex[nMaxIndex] = gpnSortedIndex[i];
      gpnSortedIndex[i] = tmp;
      QuickSortACube(nMinIndex, i-1, nLevel+1, nLongAxis);
      QuickSortACube(i+1, nMaxIndex, nLevel+1, nLongAxis);
   }
   if (dbg_sort) {
      DebugSortACube(nMinIndex, nMaxIndex, nLevel, nLongAxis);
   }
}

static
void SweepACube(cube_index)
   int cube_index;
{
   register int i;
   int min_index=gpCube[cube_index].min_index;
   int max_index=gpCube[cube_index].max_index;
   unsigned short min_r, max_r, min_g, max_g, min_b, max_b;

   min_r = max_r = gpHistogram[gpnSortedIndex[min_index]].red;
   min_g = max_g = gpHistogram[gpnSortedIndex[min_index]].green;
   min_b = max_b = gpHistogram[gpnSortedIndex[min_index]].blue;
   gpCube[cube_index].num_points = HISTOGRAMCOUNT(gpnSortedIndex[min_index]);
   for (i=min_index+1; i <= max_index; i++) {
      unsigned short red=gpHistogram[gpnSortedIndex[i]].red;
      unsigned short green=gpHistogram[gpnSortedIndex[i]].green;
      unsigned short blue=gpHistogram[gpnSortedIndex[i]].blue;

      gpCube[cube_index].num_points += HISTOGRAMCOUNT(gpnSortedIndex[i]);
      if (red < min_r) min_r = red;
      if (red > max_r) max_r = red;
      if (green < min_g) min_g = green;
      if (green > max_g) max_g = green;
      if (blue < min_b) min_b = blue;
      if (blue > max_b) max_b = blue;
   }
   gpCube[cube_index].red_length = max_r-min_r;
   gpCube[cube_index].green_length = max_g-min_g;
   gpCube[cube_index].blue_length = max_b-min_b;
   if (gpCube[cube_index].red_length >= gpCube[cube_index].green_length) {
      if (gpCube[cube_index].red_length >= gpCube[cube_index].blue_length) {
         gpCube[cube_index].long_axis = LONG_AXIS_IS_RED;
      } else {
         gpCube[cube_index].long_axis = LONG_AXIS_IS_BLUE;
      }
   } else {
      if (gpCube[cube_index].green_length >= gpCube[cube_index].blue_length) {
         gpCube[cube_index].long_axis = LONG_AXIS_IS_GREEN;
      } else {
         gpCube[cube_index].long_axis = LONG_AXIS_IS_BLUE;
      }
   }
}

static
int SplitACube(cube_index, pul_before_count, pul_after_count)
   int cube_index;
   unsigned long *pul_before_count, *pul_after_count;
   /*
    * cube to be split into (min_index,return_index)
    * and (return_index+1,max_index)
    */
{
   register int i;
   int min_index=gpCube[cube_index].min_index;
   int max_index=gpCube[cube_index].max_index;
   unsigned long count;
   unsigned long half_num_points;

   if (max_index == min_index+1) {
      *pul_before_count = HISTOGRAMCOUNT(gpnSortedIndex[min_index]);
      *pul_after_count = HISTOGRAMCOUNT(gpnSortedIndex[max_index]);
      return min_index;
   }
   count = 0;
   half_num_points = (gpCube[cube_index].num_points>>1);
   for (i=min_index; i <= max_index; i++) {
      unsigned long inc=HISTOGRAMCOUNT(gpnSortedIndex[i]);

      if (count+inc >= half_num_points) {
         if (i == min_index) {
            *pul_before_count = inc;
            *pul_after_count = gpCube[cube_index].num_points-inc;
            return i;
         } else if (i == max_index) {
            *pul_before_count = count;
            *pul_after_count = gpCube[cube_index].num_points-count;
            return i-1;
         } else if (count+inc == half_num_points) {
            *pul_before_count = count+inc;
            *pul_after_count = gpCube[cube_index].num_points-count-inc;
            return i;
         } else if (half_num_points-count >= count+inc-half_num_points) {
            if (i+1 == max_index) {
               *pul_before_count = count;
               *pul_after_count = gpCube[cube_index].num_points-count;
               return i;
            } else {
               *pul_before_count = count+inc;
               *pul_after_count = gpCube[cube_index].num_points-count-inc;
               return i+1;
            }
         } else {
            *pul_before_count = count;
            *pul_after_count = gpCube[cube_index].num_points-count;
            return i;
         }
      }
      count += inc;
   }
   count = HISTOGRAMCOUNT(gpnSortedIndex[max_index-1]);
   *pul_before_count = gpCube[cube_index].num_points-count;
   *pul_after_count = count;
   return max_index-1;
}

static
int Quantize()
   /* median-cut quantization */
{
   int smallest_level=0, max_level=0, cube_index;

   gpCube = (struct CubeRec *)malloc(gnQuantizingLevels*sizeof(struct CubeRec));
   if (gpCube == NULL) {
      FailAllocMessage();
      return FALSE;
   }
   memset(gpCube, 0, gnQuantizingLevels*sizeof(struct CubeRec));
   gnCubeEntries = 1;
   gpCube[0].min_index = 0;
   gpCube[0].max_index = gnHistogramEntries-1;
   gpCube[0].level = 0;
   cube_index = 0;
   SweepACube(0);
   if (gnDebugQuantization) {
      int i;

      fprintf(stderr, "Original histogram in Quantize():\n");
      for (i=0; i < gnHistogramEntries; i++) {
         unsigned long count=(int)HISTOGRAMCOUNT(gpnSortedIndex[i]);
         int red=(int)((HISTOGRAMRED(gpnSortedIndex[i])>>8) & 0xff);
         int green=(int)((HISTOGRAMGREEN(gpnSortedIndex[i])>>8) & 0xff);
         int blue=(int)((HISTOGRAMBLUE(gpnSortedIndex[i])>>8) & 0xff);

         fprintf(stderr, "\t#%02x%02x%02x %6ld\n",
               red&0x0ff, green&0x0ff, blue&0x0ff, count);
      }
   }
   while (gnCubeEntries < gnQuantizingLevels) {
      unsigned long before_count, after_count;
      int split_index, new_level;

      while (smallest_level <= max_level) {
         int saved_cube_index=cube_index, found=FALSE;

         for ( ; cube_index < gnCubeEntries; cube_index++) {
            if (gpCube[cube_index].min_index != gpCube[cube_index].max_index &&
                  gpCube[cube_index].level == smallest_level) {
               found = TRUE;
               break;
            }
         }
         if (found) break;
         for (cube_index=0; cube_index < saved_cube_index; cube_index++) {
            if (gpCube[cube_index].min_index != gpCube[cube_index].max_index &&
                  gpCube[cube_index].level == smallest_level) {
               found = TRUE;
               break;
            }
         }
         if (found) break;
         smallest_level++;
      }
      if (smallest_level > max_level) break;

      /*
       * determine which is the longest axis
       */
      QuickSortACube(gpCube[cube_index].min_index, gpCube[cube_index].max_index,
            0, gpCube[cube_index].long_axis);
      /*
       * cube to be split into (min_index,split_index)
       * and (split_index+1,max_index)
       */
      split_index = SplitACube(cube_index, &before_count, &after_count);
      new_level = gpCube[cube_index].level+1;
      if (gnDebugQuantization) {
         fprintf(stderr,
               "Level %2d (%2d): [%3d,%3d] -> %6ld/[%3d,%3d] %6ld/[%3d,%3d]\n",
               gpCube[cube_index].level, cube_index,
               gpCube[cube_index].min_index, gpCube[cube_index].max_index,
               before_count, gpCube[cube_index].min_index, split_index,
               after_count, split_index+1, gpCube[cube_index].max_index);
      }
      gpCube[gnCubeEntries].min_index = split_index+1;
      gpCube[gnCubeEntries].max_index = gpCube[cube_index].max_index;
      gpCube[gnCubeEntries].level = new_level;
      gpCube[gnCubeEntries].num_points = after_count;
      SweepACube(gnCubeEntries);
      gnCubeEntries++;

      gpCube[cube_index].min_index = gpCube[cube_index].min_index;
      gpCube[cube_index].max_index = split_index;
      gpCube[cube_index].level = new_level;
      gpCube[cube_index].num_points = before_count;
      SweepACube(cube_index);

      if (max_level < new_level) max_level = new_level;

      cube_index++;
   }
   if (gnDebugQuantization) {
      for (cube_index=0; cube_index < gnCubeEntries; cube_index++) {
         int i;

         fprintf(stderr, "cube %3d: (%3d) [%3d,%3d] %6ld\n",
               cube_index, gpCube[cube_index].level,
               gpCube[cube_index].min_index, gpCube[cube_index].max_index,
               (long)gpCube[cube_index].num_points);
         for (i=gpCube[cube_index].min_index; i <= gpCube[cube_index].max_index;
               i++) {
            unsigned long count=(int)HISTOGRAMCOUNT(gpnSortedIndex[i]);
            int red=(int)((HISTOGRAMRED(gpnSortedIndex[i])>>8) & 0xff);
            int green=(int)((HISTOGRAMGREEN(gpnSortedIndex[i])>>8) & 0xff);
            int blue=(int)((HISTOGRAMBLUE(gpnSortedIndex[i])>>8) & 0xff);

            fprintf(stderr, "\t#%02x%02x%02x %6ld\n",
                  red&0x0ff, green&0x0ff, blue&0x0ff, count);
         }
      }
   }
   return TRUE;
}

int DoConvolution(fp, image, bitmap_image, w, h)
   FILE *fp;
   XImage *image, *bitmap_image;
   int w, h;
{
   register int j, i;
   int target_percent, interrupted=FALSE, rc;

   SetStringStatus("Building histogram...");
   XSync(mainDisplay, False);
   if (gpConvolveFunc == NULL) {
      return FALSE;
   }
   if (!CreatePixelToIndexMapping()) {
      return FALSE;
   }
   memset(gaHGBucket, 0, sizeof(gaHGBucket));

   gnHistogramEntries = 0;
   gnHistogramSize = 256;
   gpHistogram = (XColor*)malloc(gnHistogramSize*sizeof(XColor));
   if (gpHistogram == NULL) {
      FailAllocMessage();
      CleanUpConvolution();
      return FALSE;
   }
   gnImageW = w;
   gnImageH = h;
   gnOrigImageIndex = (int**)malloc(h*sizeof(int*));
   if (gnOrigImageIndex == NULL) {
      FailAllocMessage();
      CleanUpConvolution();
      return FALSE;
   }
   memset(gnOrigImageIndex, 0, h*sizeof(int*));
   for (i=0; i < h; i++) {
      gnOrigImageIndex[i] = (int*)malloc(w*sizeof(int));
      if (gnOrigImageIndex[i] == NULL) {
         FailAllocMessage();
         CleanUpConvolution();
         return FALSE;
      }
   }
   if (image != NULL) {
      target_percent = 5;
      for (i=0; i < h; i++) {
         int percent=(i*10000/gnImageH)/100;

         if (percent >= target_percent) {
            sprintf(gszMsgBox, "Histogram: %1d%%", percent);
            SetStringStatus(gszMsgBox);
            XSync(mainDisplay, False);
            while (target_percent <= percent) {
               target_percent += 5;
            }
         }
         for (j=0; j < w; j++) {
            /* int pixel=XGetPixel(image,j,i); */
            /* int index=pnPixelToIndexMap[pixel]; */

            gnOrigImageIndex[i][j] = gpnPixelToIndexMap[XGetPixel(image,j,i)];
         }
      }
   }
   gnFinalImageIndex = (int**)malloc(h*sizeof(int*));
   if (gnFinalImageIndex == NULL) {
      FailAllocMessage();
      CleanUpConvolution();
      return FALSE;
   }
   memset(gnFinalImageIndex, 0, h*sizeof(int*));
   for (i=0; i < h; i++) {
      gnFinalImageIndex[i] = (int*)malloc(w*sizeof(int));
      if (gnFinalImageIndex[i] == NULL) {
         FailAllocMessage();
         CleanUpConvolution();
         return FALSE;
      }
   }
   ShowInterrupt(1);
   target_percent = 5;
   for (i=0; i < h; i++) {
      int percent=(i*10000/gnImageH)/100;

      if (percent >= target_percent) {
         sprintf(gszMsgBox, "Computing new pixels: %1d%%", percent);
         SetStringStatus(gszMsgBox);
         XSync(mainDisplay, False);
         while (target_percent <= percent) {
            target_percent += 5;
         }
      }
      if (ESCPressed() || CheckInterrupt()) {
         sprintf(gszMsgBox, "User interrupt.");
         Msg(gszMsgBox);
         interrupted = TRUE;
         break;
      }
      for (j=0; j < w; j++) {
         gnFinalImageIndex[i][j] = ((ConvolveFunc*)gpConvolveFunc)(j, i);
      }
   }
   HideInterrupt();
   if (interrupted) {
      CleanUpConvolution();
      return FALSE;
   }
   gpnSortedIndex = (int*)malloc(gnHistogramEntries*sizeof(int));
   if (gpnSortedIndex == NULL) {
      FailAllocMessage();
      CleanUpConvolution();
      return FALSE;
   }
   for (i=0; i < gnHistogramEntries; i++) gpnSortedIndex[i] = i;
   if (gnUserSpecifiedLevels != (-1) ||
         gnHistogramEntries > gnQuantizingLevels) {
      int saved_levels=gnQuantizingLevels;

      if (gnUserSpecifiedLevels != (-1)) {
         gnQuantizingLevels = gnUserSpecifiedLevels;
      }
      sprintf(gszMsgBox, "Quantizing from %1d to %1d colors...",
            gnHistogramEntries, gnQuantizingLevels);
      Msg(gszMsgBox);
      SetStringStatus(gszMsgBox);
      XSync(mainDisplay, False);
      if (Quantize()) {
         rc = DumpQuantizedConvolution(fp);
         if (gnUserSpecifiedLevels != (-1)) {
            gnQuantizingLevels = saved_levels;
         }
         CleanUpConvolution();
         return rc;
      }
      CleanUpConvolution();
      gnQuantizingLevels = saved_levels;;
      return FALSE;
   }
   rc = DumpConvolution(fp);
   CleanUpConvolution();
   return rc;
}

static
int DoColorMapping(fp, image, bitmap_image, w, h, xpm_ptr)
   FILE *fp;
   XImage *image, *bitmap_image;
   int w, h;
   struct XPmRec *xpm_ptr;
{
   register int j, i;
   int target_percent, interrupted=FALSE, rc;

   if (gpImageMapColorFunc == NULL) {
      return FALSE;
   }
   SetStringStatus("Remapping Colors...");
   XSync(mainDisplay, False);

   memset(gaHGBucket, 0, sizeof(gaHGBucket));

   gnHistogramEntries = 0;
   gnHistogramSize = 256;
   gpHistogram = (XColor*)malloc(gnHistogramSize*sizeof(XColor));
   if (gpHistogram == NULL) {
      FailAllocMessage();
      CleanUpConvolution();
      return FALSE;
   }
   if (!CreateObjPixelToIndexMapping(xpm_ptr)) {
      CleanUpConvolution();
      return FALSE;
   }
   gnImageW = w;
   gnImageH = h;
   gnFinalImageIndex = (int**)malloc(h*sizeof(int*));
   if (gnFinalImageIndex == NULL) {
      FailAllocMessage();
      CleanUpConvolution();
      return FALSE;
   }
   memset(gnFinalImageIndex, 0, h*sizeof(int*));
   for (i=0; i < h; i++) {
      gnFinalImageIndex[i] = (int*)malloc(w*sizeof(int));
      if (gnFinalImageIndex[i] == NULL) {
         FailAllocMessage();
         CleanUpConvolution();
         return FALSE;
      }
   }
   ShowInterrupt(1);
   target_percent = 5;
   for (i=0; i < h; i++) {
      int percent=(i*10000/gnImageH)/100;

      if (percent >= target_percent) {
         sprintf(gszMsgBox, "Computing new pixels: %1d%%", percent);
         SetStringStatus(gszMsgBox);
         XSync(mainDisplay, False);
         while (target_percent <= percent) {
            target_percent += 5;
         }
      }
      if (ESCPressed() || CheckInterrupt()) {
         sprintf(gszMsgBox, "User interrupt.");
         Msg(gszMsgBox);
         interrupted = TRUE;
         break;
      }
      for (j=0; j < w; j++) {
         /* int pixel=XGetPixel(image,j,i); */
         /* int index=pnPixelToIndexMap[pixel]; */

         if (bitmap_image != NULL && XGetPixel(bitmap_image,j,i) == 0) {
            gnFinalImageIndex[i][j] = gnTransparentIndex;
         } else {
            gnFinalImageIndex[i][j] = gpnPixelToIndexMap[XGetPixel(image,j,i)];
         }
      }
   }
   HideInterrupt();
   if (interrupted) {
      CleanUpConvolution();
      return FALSE;
   }
   rc = DumpConvolution(fp);
   CleanUpConvolution();
   return rc;
}

/* ----------------------- ProcessImage ----------------------- */

static
FILE *GetImageProcOutputFileName(pszPath, pnShortName, ppszRest)
   char *pszPath, **ppszRest;
   int *pnShortName;
{
   FILE *fp=NULL;

   sprintf(pszPath, "%sTgifXXXXXX", TMP_DIR);
   mktemp(pszPath);
   unlink(pszPath);
   if ((*pnShortName=IsPrefix(bootDir, pszPath, ppszRest))) {
      *ppszRest = (&(*ppszRest)[1]);
   }
   if ((fp=fopen(pszPath, "w")) == NULL) {
      if (*pnShortName) {
         sprintf(gszMsgBox, "Can not open '%s', print aborted.", *ppszRest);
      } else {
         sprintf(gszMsgBox, "Can not open '%s', print aborted.",
               pszPath);
      }
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return NULL;
   }
   return fp;
}

static
int CleanUpProcessImage(fp, image, bitmap_image)
   FILE *fp;
   XImage *image, *bitmap_image;
{
   if (fp != NULL) fclose(fp);
   if (image != NULL) XDestroyImage(image);
   if (bitmap_image != NULL) XDestroyImage(bitmap_image);
   return FALSE;
}

static
int ProcessImage()
{
   int short_name=FALSE, ok=TRUE;
   char path[MAXPATHLENGTH+1], *rest=NULL;
   Pixmap pixmap=None, bitmap=None;
   XImage *image=NULL, *bitmap_image=NULL;
   FILE *fp=NULL;
   struct ObjRec *obj_ptr=NULL;
   int image_w=0, image_h=0;
   struct XPmRec *xpm_ptr=NULL;

   if ((fp=GetImageProcOutputFileName(path, &short_name, &rest)) == NULL) {
      return FALSE;
   }
   if (gnCombining) {
      obj_ptr = NULL;
   } else {
      obj_ptr = topSel->obj;
   }
   if (obj_ptr == NULL) {
      pixmap = None;
      bitmap = None;
      image_w = gnCombineW;
      image_h = gnCombineH;
      image = bitmap_image = NULL;
   } else if (obj_ptr->type == OBJ_XPM) {
      xpm_ptr = obj_ptr->detail.xpm;

      pixmap = xpm_ptr->pixmap;
      bitmap = xpm_ptr->bitmap;
      image_w = xpm_ptr->image_w;
      image_h = xpm_ptr->image_h;

      image = XGetImage(mainDisplay, pixmap, 0, 0, image_w, image_h, AllPlanes,
            ZPixmap);
      if (bitmap != None) {
         bitmap_image = XGetImage(mainDisplay, bitmap, 0, 0, image_w, image_h,
            1, ZPixmap);
      }
      if (image == NULL || (bitmap != None && bitmap_image == NULL)) {
         sprintf(gszMsgBox, "%s.\n\n%s.",
               "Can not generate an image", "Image processing aborted");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return CleanUpProcessImage(fp, image, bitmap_image);
      }
   } else {
      return CleanUpProcessImage(fp, image, bitmap_image);
   }
   SaveStatusStrings();
   if (gnConvolving) {
      ok = DoConvolution(fp, image, bitmap_image, image_w, image_h);
   } else {
      ok = DoColorMapping(fp, image, bitmap_image, image_w, image_h, xpm_ptr);
   }
   RestoreStatusStrings();
   CleanUpProcessImage(fp, image, bitmap_image);
   if (!ok) return FALSE;
   strcpy(gszImageProcXPmFile, path);
   if (gnConvolving) {
      CleanUpConvolution();
   }
   return TRUE;
}

static
int DoImageProc(pvImageMapColorFunc)
   void *pvImageMapColorFunc;
{
   int saved_colordump=colorDump, saved_left=leftExportPixelTrim;
   int saved_top=topExportPixelTrim, saved_right=rightExportPixelTrim;
   int saved_bottom=bottomExportPixelTrim, saved_where_to_print=whereToPrint;
   int saved_ltx, saved_lty, saved_cur_file_defined=curFileDefined;
   int ltx, lty, rbx, rby, saved_w, saved_h, saved_x, saved_y, ctm_saved=FALSE;
   int w, h, image_w, image_h, ncolors, hcars_per_pixel, first_pixel_is_bg;
   int rc, chars_per_pixel, *pixels=NULL, retry_count=0;
   struct XfrmMtrxRec saved_ctm;
   struct BBRec saved_orig_obbox;
   XPoint saved_rotated_obbox[5];
   char *color_char=NULL, **color_str=NULL, *xpm_data=NULL;
   Pixmap pixmap=None, bitmap=None;
   XImage *image=NULL, *bitmap_image=NULL;
   struct ObjRec *obj_ptr;
   struct AttrRec *saved_fattr=NULL, *saved_lattr=NULL;

   if (gnCombining) {
      saved_ltx = selObjLtX;
      saved_lty = selObjLtY;
      saved_w = selObjRbX - saved_ltx;
      saved_h = selObjRbY - saved_lty;
      ltx = selLtX;
      lty = selLtY;
      rbx = selRbX;
      rby = selRbY;
   } else {
      if (topSel->obj->ctm != NULL) {
         ctm_saved = TRUE;
         saved_x = topSel->obj->x;
         saved_y = topSel->obj->y;
         memcpy(&saved_ctm, topSel->obj->ctm,
               sizeof(struct XfrmMtrxRec));

         memcpy(&saved_orig_obbox, &topSel->obj->orig_obbox,
               sizeof(struct BBRec));
         memcpy(saved_rotated_obbox, topSel->obj->rotated_obbox,
               5*sizeof(XPoint));
      }
      saved_ltx = topSel->obj->obbox.ltx;
      saved_lty = topSel->obj->obbox.lty;
      saved_w = topSel->obj->obbox.rbx - saved_ltx;
      saved_h = topSel->obj->obbox.rby - saved_lty;
      ltx = topSel->obj->bbox.ltx;
      lty = topSel->obj->bbox.lty;
      rbx = topSel->obj->bbox.rbx;
      rby = topSel->obj->bbox.rby;
   }
   leftExportPixelTrim = topExportPixelTrim = rightExportPixelTrim =
         bottomExportPixelTrim = 0;
   *gszImageProcXPmFile = '\0';

   curFileDefined = TRUE;
   whereToPrint = XBM_FILE;
   colorDump = TRUE;
   gnInImageProc = TRUE;
   gpImageMapColorFunc = pvImageMapColorFunc;

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   ProcessImage();
   SetDefaultCursor(mainWindow);
   ShowCursor();

   gpImageMapColorFunc = NULL;
   colorDump = saved_colordump;
   whereToPrint = saved_where_to_print;
   curFileDefined = saved_cur_file_defined;

   if (*gszImageProcXPmFile == '\0') {
      gnInImageProc = FALSE;
      return FALSE;
   }
   leftExportPixelTrim = saved_left;
   topExportPixelTrim = saved_top;
   rightExportPixelTrim = saved_right;
   bottomExportPixelTrim = saved_bottom;

   if (gnCombining) {
      struct SelRec *sel_ptr;

      HighLightReverse();
      PrepareToRecord(CMD_REPLACE, topSel, botSel, numObjSelected);

      obj_ptr = NULL;
      for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
         UnlinkObj(sel_ptr->obj);
         FreeObj(sel_ptr->obj);
      }
      RemoveAllSel();
   } else {
      HighLightReverse();
      PrepareToRecord(CMD_REPLACE, topSel, botSel, numObjSelected);

      obj_ptr = topSel->obj;
      saved_fattr = obj_ptr->fattr;
      saved_lattr = obj_ptr->lattr;
      obj_ptr->fattr = obj_ptr->lattr = NULL;
      RemoveAllSel();
   }
   gnInImageProc = FALSE;
   do {
      int new_colormap_used=newColormapUsed;

      if (obj_ptr != NULL) UnlinkObj(obj_ptr);
      if (obj_ptr != NULL) FreeObj(obj_ptr);

      gnInImageProc = TRUE;
      if (FlushColormap()) {
         Msg("Colormap flushed.");
      }
      gnInImageProc = FALSE;

      allocColorFailed = FALSE;
      SetWatchCursor(drawWindow);
      SetWatchCursor(mainWindow);
      rc = MyReadPixmapFile(gszImageProcXPmFile, &image_w, &image_h, &w, &h,
            &pixmap, &image, &bitmap, &bitmap_image, &ncolors, &chars_per_pixel,
            &first_pixel_is_bg, &color_char, &color_str, &pixels, &xpm_data);
      SetDefaultCursor(mainWindow);
      ShowCursor();

      if (rc != BitmapSuccess) {
         sprintf(gszMsgBox, "Can not import X Pixmap file '%s'.",
               gszImageProcXPmFile);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         unlink(gszImageProcXPmFile);
         if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
            RedrawColorWindow();
         }
         AbortPrepareCmd(CMD_REPLACE);

         return FALSE;
      }
      obj_ptr = CreateXPmObj(image_w, image_h, w, h, pixmap, image, bitmap,
            bitmap_image, ncolors, chars_per_pixel, FALSE, color_char,
            color_str, pixels, xpm_data);
      obj_ptr->obbox.rbx = obj_ptr->obbox.ltx+saved_w;
      obj_ptr->obbox.rby = obj_ptr->obbox.lty+saved_h;
      AdjObjBBox(obj_ptr);
      AddObj(NULL, topObj, obj_ptr);
      MoveObj(obj_ptr, saved_ltx-obj_ptr->obbox.ltx,
            saved_lty-obj_ptr->obbox.lty);
      if (ctm_saved) {
         obj_ptr->x = saved_x;
         obj_ptr->y = saved_y;
         obj_ptr->ctm = (struct XfrmMtrxRec*)malloc(sizeof(struct XfrmMtrxRec));
         if (obj_ptr->ctm == NULL) FailAllocMessage();
         memcpy(obj_ptr->ctm, &saved_ctm, sizeof(struct XfrmMtrxRec));

         memcpy(&obj_ptr->orig_obbox, &saved_orig_obbox, sizeof(struct BBRec));
         memcpy(obj_ptr->rotated_obbox, saved_rotated_obbox, 5*sizeof(XPoint));
      }
      if (saved_fattr != NULL) {
         obj_ptr->fattr = saved_fattr;
         obj_ptr->lattr = saved_lattr;
      }
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
            obj_ptr->bbox.ltx-GRID_ABS_SIZE(1),
            obj_ptr->bbox.lty-GRID_ABS_SIZE(1),
            obj_ptr->bbox.rbx+GRID_ABS_SIZE(1),
            obj_ptr->bbox.rby+GRID_ABS_SIZE(1));
      if (!new_colormap_used && newColormapUsed && allocColorFailed) {
         if (retry_count > 1) {
            break;
         }
         sprintf(gszMsgBox, "%s.\n\n%s?",
               "May have used up all the colors",
               "Would you like to retry it once more");
         if (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB) != MB_ID_YES) {
            break;
         }
         retry_count++;
         if (saved_fattr != NULL) {
            obj_ptr->fattr = obj_ptr->lattr = NULL;
         }
      } else {
         break;
      }
   } while (retry_count > 0);
   unlink(gszImageProcXPmFile);
   if (saved_fattr != NULL && topObj->fattr == NULL) {
      topObj->fattr = saved_fattr;
      topObj->lattr = saved_lattr;
   }
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   if (gnCombining) {
      SelectTopObj();

      recordCmdUsesNewColormap = TRUE;
      RecordCmd(CMD_MANY_TO_ONE, NULL, topSel, botSel, 1);
      recordCmdUsesNewColormap = FALSE;
   } else {
      SelectTopObj();

      recordCmdUsesNewColormap = TRUE;
      RecordCmd(CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
      recordCmdUsesNewColormap = FALSE;
   }
   SetFileModified(TRUE);
   justDupped = FALSE;
   return TRUE;
}

/* ----------------------- MakeGray ----------------------- */

static
void ChangeToGray(nColorIndex, pColor)
   int nColorIndex;
   XColor *pColor;
{
   int red=(int)tgifColors[nColorIndex].red;
   int green=(int)tgifColors[nColorIndex].green;
   int blue=(int)tgifColors[nColorIndex].blue;
   float gray=0.299*((float)red)+0.587*((float)green)+0.114*((float)blue);
   int val=(int)gray;
   int real_gray=((val>0x0ffff) ? 0x0ffff : ((val<0) ? 0 : val));

   pColor->red = pColor->green = pColor->blue = real_gray;
}

void MakeGray()
{
   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_MAKEGRAY])) {
      return;
   }
   DoImageProc((void*)ChangeToGray);
}

/* ----------------------- InvertColor ----------------------- */

static
void ChangeToInvertColor(nColorIndex, pColor)
   int nColorIndex;
   XColor *pColor;
{
   int red=(int)tgifColors[nColorIndex].red;
   int green=(int)tgifColors[nColorIndex].green;
   int blue=(int)tgifColors[nColorIndex].blue;

   pColor->red = 0x0ffff-((unsigned int)red);
   pColor->green = 0x0ffff-((unsigned int)green);
   pColor->blue = 0x0ffff-((unsigned int)blue);
}

void InvertColor()
{
   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_INVERTCOLOR])) {
      return;
   }
   DoImageProc((void*)ChangeToInvertColor);
}

/* ----------------------- InterpolateColor ----------------------- */

static XColor gInterpFromColor, gInterpToColor;

static
void ChangeToInterpolateColor(nColorIndex, pColor)
   int nColorIndex;
   XColor *pColor;
{
   int red=(int)tgifColors[nColorIndex].red;
   int green=(int)tgifColors[nColorIndex].green;
   int blue=(int)tgifColors[nColorIndex].blue;
   float gray=(0.299*((float)red)+0.587*((float)green)+0.114*((float)blue)) /
         (float)(0x0000ffff), tmp_fval;
   int val, real_red, real_green, real_blue;

   tmp_fval = ((float)gInterpFromColor.red) +
         gray*(((float)gInterpToColor.red)-((float)gInterpFromColor.red));
   val = round(tmp_fval);
   real_red = ((val>0x0ffff) ? 0x0ffff : ((val<0) ? 0 : val));

   tmp_fval = ((float)gInterpFromColor.green) +
         gray*(((float)gInterpToColor.green)-((float)gInterpFromColor.green));
   val = round(tmp_fval);
   real_green = ((val>0x0ffff) ? 0x0ffff : ((val<0) ? 0 : val));

   tmp_fval = ((float)gInterpFromColor.blue) +
         gray*(((float)gInterpToColor.blue)-((float)gInterpFromColor.blue));
   val = round(tmp_fval);
   real_blue = ((val>0x0ffff) ? 0x0ffff : ((val<0) ? 0 : val));

   pColor->red = (unsigned int)real_red;
   pColor->green = (unsigned int)real_green;
   pColor->blue = (unsigned int)real_blue;
}

void InterpolateColor()
{
   char *c_ptr, szFrom[MAXSTRING+1], szTo[MAXSTRING+1];
   char szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   XColor color_def;

   if (!CheckSelectionForImageProc(
         imageProcMenuStr[IMAGEPROC_INTERPOLATECOLOR])) {
      return;
   }
   sprintf(gszMsgBox, "%s %s (e.g., Black Yellow):",
         "Please enter a pair of colors",
         "(from dark to bright) for interpolation");
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') return;

   strcpy(szSpecCopy, szSpec);
   *szFrom = *szTo = '\0';
   if ((c_ptr=strtok(szSpec, " ,-\t\n\r")) != NULL) {
      strcpy(szFrom, c_ptr);
      if ((c_ptr=strtok(NULL, " ,-\t\n\r")) != NULL) {
         strcpy(szTo, c_ptr);
      }
   }
   if (*szFrom == '\0' || *szTo == '\0') {
      sprintf(gszMsgBox, "Invalid spec entered: '%s'.", szSpecCopy);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (!TgifParseColor(szFrom, &gInterpFromColor)) {
      sprintf(gszMsgBox, "'%s' is not a valid color.", szFrom);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   } else if (!TgifParseColor(szTo, &gInterpToColor)) {
      sprintf(gszMsgBox, "'%s' is not a valid color.", szTo);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   DoImageProc((void*)ChangeToInterpolateColor);
}

/* ----------------------- Brighten/Darken ----------------------- */

static int gnBrighten=0;

static
void ChangeToBrightenDarken(nColorIndex, pColor)
   int nColorIndex;
   XColor *pColor;
{
   int red=((int)tgifColors[nColorIndex].red)+gnBrighten;
   int green=((int)tgifColors[nColorIndex].green)+gnBrighten;
   int blue=((int)tgifColors[nColorIndex].blue)+gnBrighten;
   int real_red, real_green, real_blue;

   real_red = ((red>0x0ffff) ? 0x0ffff : ((red<0) ? 0 : red));
   real_green = ((green>0x0ffff) ? 0x0ffff : ((green<0) ? 0 : green));
   real_blue = ((blue>0x0ffff) ? 0x0ffff : ((blue<0) ? 0 : blue));

   pColor->red = (unsigned int)real_red;
   pColor->green = (unsigned int)real_green;
   pColor->blue = (unsigned int)real_blue;
}

void BrightenDarken()
{
   char *c_ptr, szValue[MAXSTRING+1];
   char szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   float fVal;

   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_BRIGHTDARKEN])) {
      return;
   }
   sprintf(gszMsgBox, "%s:",
         "Please enter a value between -1.0 (all black) and +1.0 (all white)");
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') return;

   strcpy(szSpecCopy, szSpec);
   if ((c_ptr=strtok(szSpec, " ,\t\n\r")) == NULL) return;
   strcpy(szValue, c_ptr);
   if (sscanf(szValue, "%f", &fVal) != 1) {
      sprintf(gszMsgBox, "Fail to parse '%s' for a value.", szSpecCopy);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   fVal *= ((float)0x0000ffff);
   gnBrighten = (int)round(fVal);

   DoImageProc((void*)ChangeToBrightenDarken);
}

/* ----------------------- ChangeSaturation ----------------------- */

/*
 * 0                  65535          spread
 * |--------------------r-----| s = -------- * 65535
 * |---g----------------------|        v
 * |-----------b--------------|
 * |<---------v-------->|              x
 *     |<----spread---->|       h = -------- * 60
 *     |<--x-->|                     spread
 *
 * 0     60   120   180   240   300   360
 * |--r--|--g--|--g--|--b--|--b--|--r--|
 */

#define SAT_TOL (1.0e-5)

static float gfSaturation=(float)0.0;

static
void RGBtoHSV(r, g, b, h, s, v)
   int r, g, b, *h, *v;
   float *s;
   /* 0 <= r,g,b <= 0x0ffff */
   /* 0 <= *h < 360 */
   /* 0 <= *s,*v <= 0x0ffff */
{
   int max_val=max(r,max(g,b)), min_val=min(r,min(g,b));
   float spread=(float)(max_val-min_val);

   *v = max_val;
   if (max_val == 0) {
      *s = (float)0.0;
   } else {
      *s = (float)((spread*((float)0x0ffff))/((float)max_val));
   }
   if (*s < (float)0.0) *s = (float)0.0;
   if (*s > SAT_TOL) {
      int hue;

      if (r == max_val) {
         hue = (int)(((float)(g-b))/spread*((float)60.0));
         if (hue < -60) hue = -60;
         if (hue < 0) {
            hue += 360;
         } else if (hue > 60) {
            hue = 60;
         }
      } else if (g == max_val) {
         hue = (int)(((float)120.0) + (((float)(b-r))/spread*((float)60.0)));
         if (hue < 60) hue = 60;
         if (hue > 180) hue = 180;
      } else if (b == max_val) {
         hue = (int)(((float)240.0) + (((float)(r-g))/spread*((float)60.0)));
         if (hue < 180) hue = 180;
         if (hue > 300) hue = 300;
      }
      *h = hue;
   } else {
      *h = 0;
   }
}

#define R_IS_MAX 0
#define G_IS_MAX 1
#define B_IS_MAX 2

static
void HSVtoRGB(h, s, v, r, g, b)
   float s;
   int h, v, *r, *g, *b;
   /* 0 <= *r,*g,*b <= 0x0ffff */
   /* 0 <= h < 360 */
   /* 0 <= s,v <= 0x0ffff */
{
   if (s <= SAT_TOL) {
      *r = *g = *b = v;
   } else {
      float frac, spread;
      int which, min_val, mid_val;

      if (h >= 300) {
         frac = (((float)(360-h))/((float)60.0));
         which = R_IS_MAX;
      } else if (h >= 240) {
         frac = (((float)(h-240))/((float)60.0));
         which = B_IS_MAX;
      } else if (h >= 180) {
         frac = (((float)(240-h))/((float)60.0));
         which = B_IS_MAX;
      } else if (h >= 120) {
         frac = (((float)(h-120))/((float)60.0));
         which = G_IS_MAX;
      } else if (h >= 60) {
         frac = (((float)(120-h))/((float)60.0));
         which = G_IS_MAX;
      } else {
         frac = (((float)h)/((float)60.0));
         which = R_IS_MAX;
      }
      spread = (((float)v)*s/((float)0x0ffff));
      min_val = (int)(v-spread);
      mid_val = min_val+((int)(frac*spread));

      switch (which) {
      case R_IS_MAX:
         *r = v;
         if (h >= 300) {
            /* g < b */  *g = min_val; *b = mid_val;
         } else {
            /* g >= b */ *g = mid_val; *b = min_val;
         }
         break;
      case G_IS_MAX:
         *g = v;
         if (h >= 120) {
            /* b >= r */ *b = mid_val; *r = min_val;
         } else {
            /* b < r */  *b = min_val; *r = mid_val;
         }
         break;
      case B_IS_MAX:
         *b = v;
         if (h >= 240) {
            /* r >= g */ *r = mid_val; *g = min_val;
         } else {
            /* r < g */  *r = min_val; *g = mid_val;
         }
         break;
      }
   }
}

static
void ChangeToChangeSaturation(nColorIndex, pColor)
   int nColorIndex;
   XColor *pColor;
{
   int red=((int)tgifColors[nColorIndex].red);
   int green=((int)tgifColors[nColorIndex].green);
   int blue=((int)tgifColors[nColorIndex].blue);
   int h, v, real_red, real_green, real_blue;
   float s;

   RGBtoHSV(red, green, blue, &h, &s, &v);
   s = s * (((float)1.0)+gfSaturation);
   if (s > (float)0x0ffff) s = (float)0x0ffff;
   if (s < (float)0.0) s = (float)0.0;
   HSVtoRGB(h, s, v, &red, &green, &blue);

   real_red = ((red>0x0ffff) ? 0x0ffff : ((red<0) ? 0 : red));
   real_green = ((green>0x0ffff) ? 0x0ffff : ((green<0) ? 0 : green));
   real_blue = ((blue>0x0ffff) ? 0x0ffff : ((blue<0) ? 0 : blue));

   pColor->red = (unsigned int)real_red;
   pColor->green = (unsigned int)real_green;
   pColor->blue = (unsigned int)real_blue;
}

void ChangeSaturation()
{
   char *c_ptr, szValue[MAXSTRING+1];
   char szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   float fVal;

   if (!CheckSelectionForImageProc(
         imageProcMenuStr[IMAGEPROC_CHANGESATURATION])) {
      return;
   }
   sprintf(gszMsgBox, "%s:",
         "Please enter a value between -1.0 (gray) and +1.0 (saturated)");
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') return;

   strcpy(szSpecCopy, szSpec);
   if ((c_ptr=strtok(szSpec, " ,\t\n\r")) == NULL) return;
   strcpy(szValue, c_ptr);
   if (sscanf(szValue, "%f", &fVal) != 1) {
      sprintf(gszMsgBox, "Fail to parse '%s' for a value.", szSpecCopy);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (fVal > (float)1.0) fVal = (float)1.0;
   if (fVal < (float)(-1.0)) fVal = (float)(-1.0);
   gfSaturation = fVal;

   DoImageProc((void*)ChangeToChangeSaturation);
}

/* ----------------------- ChangeHue ----------------------- */

static int gnFromHue=0, gnToHue=0, gnZeroWithinFromAngle=FALSE;
static float gfFromAngle=(float)0.0, gfToAngle=(float)0.0;
static float gfFromStart=(float)0.0, gfToStart=(float)0.0;
static float gfFromEnd=(float)0.0, gfToEnd=(float)0.0;

static
int HueInFromRange(fHue)
   float fHue;
{
   if (gfFromStart >= gfFromEnd) {
      if (gfFromEnd <= fHue && fHue <= gfFromStart) {
         return TRUE;
      }
   } else {
      if (gfFromStart <= fHue && fHue <= gfFromEnd) {
         return TRUE;
      }
   }
   return FALSE;
}

static
void ChangeToChangeHue(nColorIndex, pColor)
   int nColorIndex;
   XColor *pColor;
{
   int red=((int)tgifColors[nColorIndex].red);
   int green=((int)tgifColors[nColorIndex].green);
   int blue=((int)tgifColors[nColorIndex].blue);
   int h, v, real_red, real_green, real_blue;
   float s;

   RGBtoHSV(red, green, blue, &h, &s, &v);
   if (s > SAT_TOL && h > 300) h -= 360;
   if (s > SAT_TOL && HueInFromRange((float)h)) {
      float fFraction=(float)0.0;

      if (fabs(gfFromAngle) > ZERO_TOL) {
         fFraction = (((float)h)-gfFromStart)/(gfFromAngle*((float)2.0));
      }
      h = (int)(fFraction*gfToAngle*((float)2.0) + gfToStart);
      while (h >= 360) h -= 360;
      while (h < 0) h += 360;
      HSVtoRGB(h, s, v, &red, &green, &blue);

      real_red = ((red>0x0ffff) ? 0x0ffff : ((red<0) ? 0 : red));
      real_green = ((green>0x0ffff) ? 0x0ffff : ((green<0) ? 0 : green));
      real_blue = ((blue>0x0ffff) ? 0x0ffff : ((blue<0) ? 0 : blue));

      pColor->red = (unsigned int)real_red;
      pColor->green = (unsigned int)real_green;
      pColor->blue = (unsigned int)real_blue;
   } else {
      pColor->red = (unsigned int)red;
      pColor->green = (unsigned int)green;
      pColor->blue = (unsigned int)blue;
   }
}

void ChangeHue()
{
   char *szFrom=NULL, *szFromAngle=NULL, *szTo=NULL, *szToAngle=NULL;
   char szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1], szValue[MAXSTRING+1];
   float sVal, fVal, fFrom=(float)0.0, fTo=(float)0.0;
   int vVal;
   XColor xcolor;

   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_CHANGEHUE])) {
      return;
   }
   sprintf(gszMsgBox, "%s: (%s)",
         "Please enter two pairs of (color,angle) values",
         "e.g., \"red 60 green 60\" maps all reddish to greenish colors");
   Dialog(gszMsgBox, NULL, szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') return;

   strcpy(szSpecCopy, szSpec);
   if ((szFrom=strtok(szSpec, " ,\t\n\r")) == NULL ||
         (szFromAngle=strtok(NULL, " ,\t\n\r")) == NULL ||
         (szTo=strtok(NULL, " ,\t\n\r")) == NULL ||
         (szToAngle=strtok(NULL, " ,\t\n\r")) == NULL) {
      sprintf(gszMsgBox, "Fail to parse '%s' for 4 values.", szSpecCopy);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (!TgifParseColor(szFrom, &xcolor)) {
      sprintf(gszMsgBox, "'%s' is not a valid color.", szFrom);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   RGBtoHSV(((int)xcolor.red), ((int)xcolor.green), ((int)xcolor.blue),
         &gnFromHue, &sVal, &vVal);
   if (!TgifParseColor(szTo, &xcolor)) {
      sprintf(gszMsgBox, "'%s' is not a valid color.", szTo);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   RGBtoHSV(((int)xcolor.red), ((int)xcolor.green), ((int)xcolor.blue),
         &gnToHue, &sVal, &vVal);

   strcpy(szValue, szFromAngle);
   if (sscanf(szValue, "%f", &fVal) != 1) {
      sprintf(gszMsgBox, "Fail to parse '%s' for a value.", szFromAngle);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (fVal > (float)180.0) fVal = (float)180.0;
   if (fVal < (float)(-180.0)) fVal = (float)(-180.0);
   gfFromAngle = fVal;
   gfFromStart = ((float)gnFromHue)-gfFromAngle;
   gfFromEnd = ((float)gnFromHue)+gfFromAngle;

   strcpy(szValue, szToAngle);
   if (sscanf(szValue, "%f", &fVal) != 1) {
      sprintf(gszMsgBox, "Fail to parse '%s' for a value.", szToAngle);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (fVal > (float)180.0) fVal = (float)180.0;
   if (fVal < (float)(-180.0)) fVal = (float)(-180.0);
   gfToAngle = fVal;
   gfToStart = ((float)gnToHue)-gfToAngle;
   gfToEnd = ((float)gnToHue)+gfToAngle;

   DoImageProc((void*)ChangeToChangeHue);
}

/* ----------------------- ContrastEnhance ----------------------- */

static float gfContrastFactor=1.0;

static
void ChangeToContrastEnhance(nColorIndex, pColor)
   int nColorIndex;
   XColor *pColor;
{
   int red=(int)tgifColors[nColorIndex].red;
   int green=(int)tgifColors[nColorIndex].green;
   int blue=(int)tgifColors[nColorIndex].blue;
   int ival, real_red, real_green, real_blue;
   float fval;

   fval = ((float)(red-0x8000))*gfContrastFactor + ((float)0x8000);
   ival = round(fval);
   real_red = ((ival>0x0ffff) ? 0x0ffff : ((ival<0) ? 0 : ival));
   fval = ((float)(green-0x8000))*gfContrastFactor + ((float)0x8000);
   ival = round(fval);
   real_green = ((ival>0x0ffff) ? 0x0ffff : ((ival<0) ? 0 : ival));
   fval = ((float)(blue-0x8000))*gfContrastFactor + ((float)0x8000);
   ival = round(fval);
   real_blue = ((ival>0x0ffff) ? 0x0ffff : ((ival<0) ? 0 : ival));

   pColor->red = (unsigned int)real_red;
   pColor->green = (unsigned int)real_green;
   pColor->blue = (unsigned int)real_blue;
}

void ContrastEnhance()
{
   char *c_ptr, szValue[MAXSTRING+1];
   char szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   float fVal;

   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_CONTRAST])) {
      return;
   }
   sprintf(gszMsgBox, "%s:",
         "Please enter a non-negative value (1.0 means no adjustment)");
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') return;

   strcpy(szSpecCopy, szSpec);
   if ((c_ptr=strtok(szSpec, " ,\t\n\r")) == NULL) return;
   strcpy(szValue, c_ptr);
   if (strcmp(szValue, "1.0") == 0 || strcmp(szValue, "1") == 0 ||
         strcmp(szValue, "1.") == 0) {
      return;
   } else if (sscanf(szValue, "%f", &fVal) != 1) {
      sprintf(gszMsgBox, "Fail to parse '%s' for a value.", szSpecCopy);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   } else if (fVal < (float)0.0) {
      sprintf(gszMsgBox, "Nevative value: '%s' is not allowed.", szSpecCopy);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   gfContrastFactor = fVal;

   DoImageProc((void*)ChangeToContrastEnhance);
}

/* ----------------------- ColorBalance ----------------------- */

static float gfRedBalanceFactor=1.0;
static float gfGreenBalanceFactor=1.0;
static float gfBlueBalanceFactor=1.0;

static
void ChangeToColorBalance(nColorIndex, pColor)
   int nColorIndex;
   XColor *pColor;
{
   int red=(int)tgifColors[nColorIndex].red;
   int green=(int)tgifColors[nColorIndex].green;
   int blue=(int)tgifColors[nColorIndex].blue;
   int ival, real_red, real_green, real_blue;
   float fval;

   fval = ((float)red) * gfRedBalanceFactor;
   ival = round(fval);
   real_red = ((ival>0x0ffff) ? 0x0ffff : ((ival<0) ? 0 : ival));
   fval = ((float)green) * gfGreenBalanceFactor;
   ival = round(fval);
   real_green = ((ival>0x0ffff) ? 0x0ffff : ((ival<0) ? 0 : ival));
   fval = ((float)blue) * gfBlueBalanceFactor;
   ival = round(fval);
   real_blue = ((ival>0x0ffff) ? 0x0ffff : ((ival<0) ? 0 : ival));

   pColor->red = (unsigned int)real_red;
   pColor->green = (unsigned int)real_green;
   pColor->blue = (unsigned int)real_blue;
}

void ColorBalance()
{
   char *c_ptr, szRed[MAXSTRING+1], szGreen[MAXSTRING+1], szBlue[MAXSTRING+1];
   char szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   float fVal;

   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_COLORBALANCE])) {
      return;
   }
   sprintf(gszMsgBox, "%s %s:",
         "Please enter non-negative R G B factors",
         "(1.0 1.0 1.0 means no change)");
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') return;

   strcpy(szSpecCopy, szSpec);
   *szRed = *szGreen = *szBlue = '\0';
   if ((c_ptr=strtok(szSpec, " ,\t\n\r")) != NULL) {
      strcpy(szRed, c_ptr);
      if ((c_ptr=strtok(NULL, " ,\t\n\r")) != NULL) {
         strcpy(szGreen, c_ptr);
         if ((c_ptr=strtok(NULL, " ,\t\n\r")) != NULL) {
            strcpy(szBlue, c_ptr);
         }
      }
   }
   if (*szRed == '\0' || *szGreen == '\0' || *szBlue == '\0' ||
         sscanf(szRed, "%f", &gfRedBalanceFactor) != 1 ||
         sscanf(szGreen, "%f", &gfGreenBalanceFactor) != 1 ||
         sscanf(szBlue, "%f", &gfBlueBalanceFactor) != 1) {
      sprintf(gszMsgBox, "Fail to parse '%s' for 3 values.", szSpecCopy);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   } else if (gfRedBalanceFactor < (float)0.0 ||
         gfGreenBalanceFactor < (float)0.0 ||
         gfBlueBalanceFactor < (float)0.0) {
      sprintf(gszMsgBox, "Nevative values are not allowed: '%s'.", szSpecCopy);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }

   DoImageProc((void*)ChangeToColorBalance);
}

/* ----------------------- Gamma ----------------------- */

static float gfOneOverGamma=1.0;

static
void ChangeToGamma(nColorIndex, pColor)
   int nColorIndex;
   XColor *pColor;
{
   double red=((double)tgifColors[nColorIndex].red)/((double)0x0ffff);
   double green=((double)tgifColors[nColorIndex].green)/((double)0x0ffff);
   double blue=((double)tgifColors[nColorIndex].blue)/((double)0x0ffff);
   int ival, real_red, real_green, real_blue;
   double dval;

   dval = pow(red, (double)gfOneOverGamma) * ((double)0x010000);
   ival = round(dval);
   real_red = ((ival>0x0ffff) ? 0x0ffff : ((ival<0) ? 0 : ival));
   dval = pow(green, (double)gfOneOverGamma) * ((double)0x010000);
   ival = round(dval);
   real_green = ((ival>0x0ffff) ? 0x0ffff : ((ival<0) ? 0 : ival));
   dval = pow(blue, (double)gfOneOverGamma) * ((double)0x010000);
   ival = round(dval);
   real_blue = ((ival>0x0ffff) ? 0x0ffff : ((ival<0) ? 0 : ival));

   pColor->red = (unsigned int)real_red;
   pColor->green = (unsigned int)real_green;
   pColor->blue = (unsigned int)real_blue;
}

void Gamma()
{
   char *c_ptr, szValue[MAXSTRING+1];
   char szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   float gamma=0.0;

   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_GAMMA])) return;
   sprintf(gszMsgBox, "%s %s:",
         "Please enter a gamma value",
         "(1.0 means no change, 2.2 is brighter)");
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') return;

   strcpy(szSpecCopy, szSpec);
   *szValue = '\0';
   if ((c_ptr=strtok(szSpec, " ,\t\n\r")) != NULL) {
      strcpy(szValue, c_ptr);
   }
   if (*szValue == '\0' || sscanf(szValue, "%f", &gamma) != 1) {
      sprintf(gszMsgBox, "Fail to parse '%s' for a value.", szSpecCopy);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   } else if (gamma < (float)ZERO_TOL) {
      sprintf(gszMsgBox, "Nevative values are not allowed: '%s'.", szSpecCopy);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   gfOneOverGamma = (float)1.0 / gamma;

   DoImageProc((void*)ChangeToGamma);
}

/* ----------------------- Convolution Required ----------------------- */

/* ----------------------- EdgeDetect ----------------------- */

static
int ConvolveToEdgeDetect(x, y)
   int x, y;
{
   XColor xcolor;
   long red, green, blue;

   if (x == 0 || x == gnImageW-1 || y == 0 || y == gnImageH-1) {
      return GetOrAllocHistogramIndex(&tgifColors[gnOrigImageIndex[y][x]]);
   }
   memset(&xcolor, 0, sizeof(XColor));
   red = (((((long)tgifColors[gnOrigImageIndex[y][x]].red)<<2) -
         ((long)tgifColors[gnOrigImageIndex[y][x-1]].red) -
         ((long)tgifColors[gnOrigImageIndex[y][x+1]].red) -
         ((long)tgifColors[gnOrigImageIndex[y-1][x]].red) -
         ((long)tgifColors[gnOrigImageIndex[y+1][x]].red))>>2);
   green = (((((long)tgifColors[gnOrigImageIndex[y][x]].green)<<2) -
         ((long)tgifColors[gnOrigImageIndex[y][x-1]].green) -
         ((long)tgifColors[gnOrigImageIndex[y][x+1]].green) -
         ((long)tgifColors[gnOrigImageIndex[y-1][x]].green) -
         ((long)tgifColors[gnOrigImageIndex[y+1][x]].green))>>2);
   blue = (((((long)tgifColors[gnOrigImageIndex[y][x]].blue)<<2) -
         ((long)tgifColors[gnOrigImageIndex[y][x-1]].blue) -
         ((long)tgifColors[gnOrigImageIndex[y][x+1]].blue) -
         ((long)tgifColors[gnOrigImageIndex[y-1][x]].blue) -
         ((long)tgifColors[gnOrigImageIndex[y+1][x]].blue))>>2);
   xcolor.red = (red > 0L ?
         (red > 0x0000ffff ? 0x0ffff : (unsigned int)(red & 0xffff)) : 0);
   xcolor.green = (green > 0L ?
         (green > 0x0000ffff ? 0x0ffff : (unsigned int)(green & 0xffff)) : 0);
   xcolor.blue = (blue > 0L ?
         (blue > 0x0000ffff ? 0x0ffff : (unsigned int)(blue & 0xffff)) : 0);
   return GetOrAllocHistogramIndex(&xcolor);
}

void EdgeDetect()
{
   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_EDGEDETECT])) {
      return;
   }
   if (topSel->obj->detail.xpm->image_w < 2 ||
         topSel->obj->detail.xpm->image_h < 2) {
      sprintf(gszMsgBox,
            "Selected object is too thin or flat for edge detection.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   gpConvolveFunc = (void*)ConvolveToEdgeDetect;
   gnConvolving = TRUE;
   DoImageProc(NULL);
   gnConvolving = FALSE;
   gpConvolveFunc = NULL;
}

/* ----------------------- Emboss ----------------------- */

static
int ConvolveToEmboss(x, y)
   int x, y;
{
   XColor xcolor;

   memset(&xcolor, 0, sizeof(XColor));
   if (x == 0 || x == gnImageW-1 || y == 0 || y == gnImageH-1) {
      float gray=0.299*((float)tgifColors[gnOrigImageIndex[y][x]].red) +
            0.587*((float)tgifColors[gnOrigImageIndex[y][x]].green) +
            0.144*((float)tgifColors[gnOrigImageIndex[y][x]].blue);

      xcolor.red = xcolor.green = xcolor.blue = (unsigned short)gray;
      return GetOrAllocHistogramIndex(&xcolor);
   } else {
      float lt_gray, rb_gray;
      int val;

      lt_gray=0.299*((float)tgifColors[gnOrigImageIndex[y-1][x-1]].red) +
            0.587*((float)tgifColors[gnOrigImageIndex[y-1][x-1]].green) +
            0.144*((float)tgifColors[gnOrigImageIndex[y-1][x-1]].blue);
      rb_gray=0.299*((float)tgifColors[gnOrigImageIndex[y+1][x+1]].red) +
            0.587*((float)tgifColors[gnOrigImageIndex[y+1][x+1]].green) +
            0.144*((float)tgifColors[gnOrigImageIndex[y+1][x+1]].blue);
      val = 0x7fff +
            (int)((rb_gray - lt_gray) / 2.0);
      val = ((val>0x0ffff) ? 0x0ffff : ((val<0) ? 0 : val));
      xcolor.red = xcolor.green = xcolor.blue = (unsigned short)val;
      return GetOrAllocHistogramIndex(&xcolor);
   }
}

void Emboss()
{
   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_EMBOSS])) return;

   if (topSel->obj->detail.xpm->image_w < 2 ||
         topSel->obj->detail.xpm->image_h < 2) {
      sprintf(gszMsgBox, "Selected object is too thin or flat for embossing.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   gpConvolveFunc = (void*)ConvolveToEmboss;
   gnConvolving = TRUE;
   DoImageProc(NULL);
   gnConvolving = FALSE;
   gpConvolveFunc = NULL;
}

/* ----------------------- ReduceColors ----------------------- */

static
int ConvolveToReduceColors(x, y)
   int x, y;
{
   return GetOrAllocHistogramIndex(&tgifColors[gnOrigImageIndex[y][x]]);
}

void ReduceColors()
{
   char *c_ptr, szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   int colors_to_reduce_to=0;

   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_REDUCECOLORS])) {
      return;
   }
   sprintf(gszMsgBox, "%s (from %1d colors):",
         "Please enter the number of colors to reduce to",
         topSel->obj->detail.xpm->ncolors);
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') return;

   strcpy(szSpecCopy, szSpec);
   if ((c_ptr=strtok(szSpec, " ,\t\n\r")) == NULL) return;
   colors_to_reduce_to = atoi(c_ptr);
   if (colors_to_reduce_to < 2 ||
         colors_to_reduce_to > topSel->obj->detail.xpm->ncolors) {
      sprintf(gszMsgBox,
            "Number of colors: '%s' must be between 2 and %1d.",
            szSpecCopy, topSel->obj->detail.xpm->ncolors);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   gnUserSpecifiedLevels = colors_to_reduce_to;
   gpConvolveFunc = (void*)ConvolveToReduceColors;
   gnConvolving = TRUE;
   DoImageProc(NULL);
   gnConvolving = FALSE;
   gpConvolveFunc = NULL;
   gnUserSpecifiedLevels = (-1);
}

/* ----------------------- ReduceToPixmapColors ----------------------- */

static struct XPmRec gXPmTarget;
static XColor *gTargetColors=NULL;
static int *gnObjectColorsToTargetColorMapping=NULL;
static int *gnTgifColorsToObjectColorMapping=NULL;

static int gnFloyd=FALSE;
static int **gnImageTargetColor=NULL;
static XImage *gpTargetImage=NULL, *gpTargetBitmapImage=NULL;

static
int ConvolveToErrorDiffuse(x, y)
   int x, y;
{
   int targetcolor_index=gnImageTargetColor[y][x];

   return GetOrAllocHistogramIndex(&gTargetColors[targetcolor_index]);
}

static
int ConvolveToReduceToPixmapColors(x, y)
   int x, y;
{
   int tgifcolor_index=gnOrigImageIndex[y][x]; /* this index into tgifColors */
   int objcolor_index=gnTgifColorsToObjectColorMapping[tgifcolor_index];
   int targetcolor_index=gnObjectColorsToTargetColorMapping[objcolor_index];

   return GetOrAllocHistogramIndex(&gTargetColors[targetcolor_index]);
}

static
int GetIntensity(index, max_levels)
   int index, max_levels; /* 0 < max_levels <= 255 */
{
   int ival=(int)(((double)index)/((double)max_levels)*((double)256));

   return ((ival < 0 ? 0 : (ival > 0xff ? 0xff : ival))<<8);
}

static
int GetClosestColorIndex(red_bits, green_bits, blue_bits, red, green, blue,
      target_ncolors)
   int red_bits, green_bits, blue_bits, red, green, blue, target_ncolors;
{
   int min_index=0;
   unsigned long dr, dg, db, min_dist;

   if (gXPmTarget.color_str != NULL) {
      /* ReduceToPixmapColors() */
      int j;

      dr = (unsigned long)abs(red-(int)(gTargetColors[0].red));
      dg = (unsigned long)abs(green-(int)(gTargetColors[0].green));
      db = (unsigned long)abs(blue-(int)(gTargetColors[0].blue));
      dr >>= 4; dg >>= 4; db >>= 4;

      min_index = 0;
      min_dist = dr*dr + dg*dg + db*db;
      for (j=1; j < target_ncolors; j++) {
         unsigned long new_dist;

         dr = (unsigned long)abs(red-(int)(gTargetColors[j].red));
         dg = (unsigned long)abs(green-(int)(gTargetColors[j].green));
         db = (unsigned long)abs(blue-(int)(gTargetColors[j].blue));
         dr >>= 4; dg >>= 4; db >>= 4;
         new_dist = dr*dr + dg*dg + db*db;

         if (new_dist < min_dist) {
            min_index = j;
            min_dist = new_dist;
         }
      }
   } else {
      /* ReduceToDefaultColors() or DefaultErrorDiffuse() */
      int k, j, i;
      int red_max=(1<<red_bits);
      int green_max=(1<<green_bits);
      int blue_max=(1<<blue_bits);
      int red_index=(red >> (16-red_bits));
      int green_index=(green >> (16-green_bits));
      int blue_index=(blue >> (16-blue_bits));

      min_index = (((red_index << green_bits) + green_index) << blue_bits) +
            blue_index;
      dr = (unsigned long)abs(red-(int)(gTargetColors[min_index].red));
      dg = (unsigned long)abs(green-(int)(gTargetColors[min_index].green));
      db = (unsigned long)abs(blue-(int)(gTargetColors[min_index].blue));
      dr >>= 4; dg >>= 4; db >>= 4;

      min_dist = dr*dr + dg*dg + db*db;
      for (i=red_index-1; i <= red_index+1; i++) {
         if (i >= 0 && i < red_max) {
            for (j=green_index-1; j <= green_index+1; j++) {
               if (j >= 0 && j < green_max) {
                  for (k=blue_index-1; k <= blue_index+1; k++) {
                     if (k >= 0 && k < blue_max) {
                        if (i!=red_index || j!=green_index || k!=blue_index) {
                           int index=(((i << green_bits) + j) << blue_bits) + k;
                           unsigned long new_dist;

                           dr = (unsigned long)abs(red -
                                 (int)(gTargetColors[index].red));
                           dg = (unsigned long)abs(green -
                                 (int)(gTargetColors[index].green));
                           db = (unsigned long)abs(blue -
                                 (int)(gTargetColors[index].blue));
                           dr >>= 4; dg >>= 4; db >>= 4;
                           new_dist = dr*dr + dg*dg + db*db;
                           if (new_dist < min_dist) {
                              min_index = index;
                              min_dist = new_dist;
                           }
                        }
                     }
                  }
               }
            }
         }
      }
   }
   return min_index;
}
#define FS_SCALE 0x400
#define HALF_FS_SCALE 0x200

static
int DoPrepareForErrorDiffuse(xpm_ptr)
   struct XPmRec *xpm_ptr;
{
   int j, i, target_ncolors=gXPmTarget.ncolors, target_percent, fs_forward=TRUE;
   int image_w=xpm_ptr->image_w, image_h=xpm_ptr->image_h;
   int red_bits, green_bits, blue_bits;
   long *this_r_err, *this_g_err, *this_b_err, *tmp_err;
   long *next_r_err, *next_g_err, *next_b_err;

   if (!CreatePixelToIndexMapping()) return FALSE;
   if (gXPmTarget.color_str != NULL) {
      for (i=0; i < target_ncolors; i++) {
         if (!TgifParseColor(gXPmTarget.color_str[i], &gTargetColors[i])) {
            return FALSE;
         }
      }
   } else {
      red_bits = gDefErrorDiffuseLevel.red;
      green_bits = gDefErrorDiffuseLevel.green;
      blue_bits = gDefErrorDiffuseLevel.blue;
   }
   if (!GetXPmImages(xpm_ptr, &gpTargetImage, &gpTargetBitmapImage)) {
      return FALSE;
   }
   gnImageTargetColor = (int**)malloc(image_h*sizeof(int*));
   if (gnImageTargetColor == NULL) return FailAllocMessage();
   for (i=0; i < image_h; i++) {
      if ((gnImageTargetColor[i]=(int*)malloc(image_w*sizeof(int))) == NULL) {
         for (j=0; j < i; j++) free(gnImageTargetColor[j]);
         free(gnImageTargetColor);
         gnImageTargetColor = NULL;
         return FailAllocMessage();
      }
   }
   this_r_err = (long*)malloc((image_w+2)*sizeof(long));
   this_g_err = (long*)malloc((image_w+2)*sizeof(long));
   this_b_err = (long*)malloc((image_w+2)*sizeof(long));
   next_r_err = (long*)malloc((image_w+2)*sizeof(long));
   next_g_err = (long*)malloc((image_w+2)*sizeof(long));
   next_b_err = (long*)malloc((image_w+2)*sizeof(long));
   if (this_r_err==NULL || this_g_err==NULL || this_b_err==NULL ||
         next_r_err==NULL || next_g_err==NULL || next_b_err==NULL) {
      if (this_r_err != NULL) free(this_r_err);
      if (this_g_err != NULL) free(this_g_err);
      if (this_b_err != NULL) free(this_b_err);
      if (next_r_err != NULL) free(next_r_err);
      if (next_g_err != NULL) free(next_g_err);
      if (next_b_err != NULL) free(next_b_err);
      return FALSE;
   }
   srand(0);
   for (j=0; j < image_w+2; j++) {
      this_r_err[j] = (rand() % FS_SCALE - HALF_FS_SCALE) << 4;
      this_g_err[j] = (rand() % FS_SCALE - HALF_FS_SCALE) << 4;
      this_b_err[j] = (rand() % FS_SCALE - HALF_FS_SCALE) << 4;
   }

   target_percent = 5;
   for (i=0; i < image_h; i++) {
      int col, limitcol;
      int percent=(i*10000/image_h)/100;

      if (percent >= target_percent) {
         sprintf(gszMsgBox,
               "Calculating error diffused pixel values: %1d%%", percent);
         SetStringStatus(gszMsgBox);
         XSync(mainDisplay, False);
         while (target_percent <= percent) {
            target_percent += 5;
         }
      }
      /*--------------------------------------------------------*/
      /* the error diffusion code is adapted from 'ppmquant.c', */
      /*       which is part of the netpbm package.             */
      /*                                                        */
      /*       Copyright (C) 1989, 1991 by Jef Poskanzer        */
      /*--------------------------------------------------------*/
      col = (fs_forward ? 0 : image_w-1);
      limitcol = (fs_forward ? image_w : (-1));
      for (j=0; j < image_w+2; j++) {
         next_r_err[j] = next_g_err[j] = next_b_err[j] = 0L;
      }
      do {
         int red, green, blue, err, min_index=0;
         int pixel=XGetPixel(gpTargetImage, col, i);

         red = (int)(tgifColors[gpnPixelToIndexMap[pixel]].red) +
               this_r_err[col+1];
         green = (int)(tgifColors[gpnPixelToIndexMap[pixel]].green) +
               this_g_err[col+1];
         blue = (int)(tgifColors[gpnPixelToIndexMap[pixel]].blue) +
               this_b_err[col+1];
         if (red < 0) red = 0; if (red > 65535) red = 65535;
         if (green < 0) green = 0; if (green > 65535) green = 65535;
         if (blue < 0) blue = 0; if (blue > 65535) blue = 65535;

         min_index = GetClosestColorIndex(red_bits, green_bits, blue_bits,
               red, green, blue, target_ncolors);

         gnImageTargetColor[i][col] = min_index;
         if (fs_forward) {
            err = (red - gTargetColors[min_index].red); /* * FS_SCALE; */
            this_r_err[col+2] += (err*7) >> 4;
            next_r_err[col  ] += (err*3) >> 4;
            next_r_err[col+1] += (err*5) >> 4;
            next_r_err[col+2] += (err  ) >> 4;
            err = (green - gTargetColors[min_index].green); /* * FS_SCALE; */
            this_g_err[col+2] += (err*7) >> 4;
            next_g_err[col  ] += (err*3) >> 4;
            next_g_err[col+1] += (err*5) >> 4;
            next_g_err[col+2] += (err  ) >> 4;
            err = (blue - gTargetColors[min_index].blue); /* * FS_SCALE; */
            this_b_err[col+2] += (err*7) >> 4;
            next_b_err[col  ] += (err*3) >> 4;
            next_b_err[col+1] += (err*5) >> 4;
            next_b_err[col+2] += (err  ) >> 4;

            col++;
         } else {
            err = (red - gTargetColors[min_index].red); /* * FS_SCALE; */
            this_r_err[col  ] += (err*7) >> 4;
            next_r_err[col+2] += (err*3) >> 4;
            next_r_err[col+1] += (err*5) >> 4;
            next_r_err[col  ] += (err  ) >> 4;
            err = (green - gTargetColors[min_index].green); /* * FS_SCALE; */
            this_g_err[col  ] += (err*7) >> 4;
            next_g_err[col+2] += (err*3) >> 4;
            next_g_err[col+1] += (err*5) >> 4;
            next_g_err[col  ] += (err  ) >> 4;
            err = (blue - gTargetColors[min_index].blue); /* * FS_SCALE; */
            this_b_err[col  ] += (err*7) >> 4;
            next_b_err[col+2] += (err*3) >> 4;
            next_b_err[col+1] += (err*5) >> 4;
            next_b_err[col  ] += (err  ) >> 4;

            col--;
         }
      } while (col != limitcol);
      tmp_err = this_r_err; this_r_err = next_r_err; next_r_err = tmp_err;
      tmp_err = this_g_err; this_g_err = next_g_err; next_g_err = tmp_err;
      tmp_err = this_b_err; this_b_err = next_b_err; next_b_err = tmp_err;
      fs_forward = !fs_forward;
   }
   free(this_r_err); free(this_g_err); free(this_b_err);
   free(next_r_err); free(next_g_err); free(next_b_err);
   return TRUE;
}

static
int PrepareForErrorDiffuse(xpm_ptr)
   struct XPmRec *xpm_ptr;
{
   int rc;

   SaveStatusStrings();
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = DoPrepareForErrorDiffuse(xpm_ptr);
   SetDefaultCursor(mainWindow);
   ShowCursor();
   RestoreStatusStrings();
   return rc;
}

static
int MapTargetColors(xpm_ptr)
   struct XPmRec *xpm_ptr;
{
   int i, target_ncolors=gXPmTarget.ncolors, red_bits, green_bits, blue_bits;

   if (gXPmTarget.color_str != NULL) {
      for (i=0; i < target_ncolors; i++) {
         if (!TgifParseColor(gXPmTarget.color_str[i], &gTargetColors[i])) {
            return FALSE;
         }
      }
   } else {
      red_bits = gDefErrorDiffuseLevel.red;
      green_bits = gDefErrorDiffuseLevel.green;
      blue_bits = gDefErrorDiffuseLevel.blue;
   }
   gnTgifColorsToObjectColorMapping = (int*)malloc(maxColors*sizeof(int));
   if (gnTgifColorsToObjectColorMapping == NULL) {
      FailAllocMessage();
      return FALSE;
   }
   memset(gnTgifColorsToObjectColorMapping, (-1), maxColors*sizeof(int));

   for (i=0; i < maxColors; i++) {
      int cur_pixel=colorPixels[i], ncolors=xpm_ptr->ncolors;
      int found_index, *pixels=xpm_ptr->pixels;
      int red, green, blue, min_index;

      for (found_index=0; found_index < ncolors; found_index++) {
         if (pixels[found_index] == cur_pixel) {
            break;
         }
      }
      if (found_index >= ncolors) {
         continue;
      }
      gnTgifColorsToObjectColorMapping[i] = found_index;

      red = (int)(tgifColors[i].red);
      green = (int)(tgifColors[i].green);
      blue = (int)(tgifColors[i].blue);

      min_index = GetClosestColorIndex(red_bits, green_bits, blue_bits,
            red, green, blue, target_ncolors);

      gnObjectColorsToTargetColorMapping[found_index] = min_index;
   }
   return TRUE;
}

void ReduceToPixmapColors()
{
   char *c_ptr, szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   int i, ok=TRUE, rc, short_name;
   char file_name[MAXPATHLENGTH+1], *rest;
   XEvent ev;
   int ncolors=0, chars_per_pixel=0, first_pixel_is_bg=0;
   char *color_char=NULL, **color_str=NULL, *xpm_data=NULL;
   struct XPmRec *xpm_ptr;

   if (!CheckSelectionForImageProc(
         imageProcMenuStr[IMAGEPROC_REDUCETOPIXMAPCOLORS])) {
      return;
   }
   importingFile = TRUE;
   if (importFromLibrary) {
      char name[MAXSTRING+1], path[MAXSTRING+1];

      if (SelectFromLibrary("Please select an XPixmap file for colors...",
            XPM_FILE_EXT, name, path) == INVALID) {
         ok = FALSE;
      } else {
         sprintf(file_name, "%s/%s", path, name);
      }
   } else if (SelectFileNameToImport(
         "Please select an XPixmap file for colors...", XPM_FILE_EXT,
         file_name) == INVALID) {
      ok = FALSE;
   } else if (FileIsRemote(file_name)) {
      MsgBox("Importing remote XPixmap file not supported.",
            TOOL_NAME, INFO_MB);
      ok = FALSE;
   }
   importingFile = FALSE;
   if (!ok) {
      return;
   }
   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
   }
   xpm_ptr = topSel->obj->detail.xpm;
   gnObjectColorsToTargetColorMapping =
         (int*)malloc(xpm_ptr->ncolors*sizeof(int));
   if (gnObjectColorsToTargetColorMapping == NULL) {
      FailAllocMessage();
      return;
   }
   memset(gnObjectColorsToTargetColorMapping, 0, xpm_ptr->ncolors*sizeof(int));

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = MyReadPixmapFile(file_name, NULL, NULL, NULL, NULL, NULL,
         NULL, NULL, NULL, &ncolors, &chars_per_pixel,
         &first_pixel_is_bg, &color_char, &color_str, NULL, &xpm_data);
   SetDefaultCursor(mainWindow);
   ShowCursor();

   if ((short_name=IsPrefix(bootDir, file_name, &rest))) ++rest;
   if (rc != BitmapSuccess) {
      if (short_name) {
         sprintf(gszMsgBox, "Inavlid XPixmap file '%s'.", rest);
      } else {
         sprintf(gszMsgBox, "Inavlid XPixmap file '%s'.", file_name);
      }
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      free(gnObjectColorsToTargetColorMapping);
      gnObjectColorsToTargetColorMapping = NULL;
      return;
   }
   gXPmTarget.ncolors = ncolors;
   gXPmTarget.chars_per_pixel = chars_per_pixel;
   gXPmTarget.first_pixel_is_bg = first_pixel_is_bg;
   gXPmTarget.color_char = color_char;
   gXPmTarget.color_str = color_str;
   gXPmTarget.data = xpm_data;

   if (ncolors <= 0) {
      if (short_name) {
         sprintf(gszMsgBox, "No colors in XPixmap file '%s'.", rest);
      } else {
         sprintf(gszMsgBox, "No colors in XPixmap file '%s'.", file_name);
      }
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   } else {
      int image_h=xpm_ptr->image_h;

      sprintf(gszMsgBox, "%s?",
            "Would you like to apply Floyd-Steinberg error diffusion (slow)");
      if (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB) == MB_ID_YES) {
         gnFloyd = TRUE;
      }
      gTargetColors = (XColor*)malloc(ncolors*sizeof(XColor));
      if (gTargetColors == NULL) FailAllocMessage();
      memset(gTargetColors, 0, ncolors*sizeof(XColor));
      if (gnFloyd) {
         rc = PrepareForErrorDiffuse(xpm_ptr);
         if (gpnPixelToIndexMap != NULL) {
            free(gpnPixelToIndexMap);
            gpnPixelToIndexMap = NULL;
         }
         if (gpTargetImage != NULL) XDestroyImage(gpTargetImage);
         if (gpTargetBitmapImage != NULL) XDestroyImage(gpTargetBitmapImage);
         gpTargetImage = gpTargetBitmapImage = NULL;
      } else {
         rc = MapTargetColors(xpm_ptr);
      }
      if (rc) {
         gnUserSpecifiedLevels = ncolors;
         if (gnFloyd) {
            gpConvolveFunc = (void*)ConvolveToErrorDiffuse;
         } else {
            gpConvolveFunc = (void*)ConvolveToReduceToPixmapColors;
         }
         gnConvolving = TRUE;
         DoImageProc(NULL);
         gnConvolving = FALSE;
         gpConvolveFunc = NULL;
         gnUserSpecifiedLevels = (-1);
      }
      free(gTargetColors);
      gTargetColors = NULL;
      if (gnFloyd) {
         if (gnImageTargetColor != NULL) {
            for (i=0; i < image_h; i++) free(gnImageTargetColor[i]);
            free(gnImageTargetColor);
            gnImageTargetColor = NULL;
         }
      }
      gnFloyd = FALSE;
   }
   if (gXPmTarget.color_char != NULL) free(gXPmTarget.color_char);
   if (gXPmTarget.color_str != NULL) {
      for (i=0; i < gXPmTarget.ncolors; i++) free(gXPmTarget.color_str[i]);
      free(gXPmTarget.color_str);
   }
   if (gXPmTarget.data != NULL) free(gXPmTarget.data);
   memset(&gXPmTarget, 0, sizeof(struct XPmRec));

   if (gnObjectColorsToTargetColorMapping != NULL) {
      free(gnObjectColorsToTargetColorMapping);
      gnObjectColorsToTargetColorMapping = NULL;
   }
   if (gnTgifColorsToObjectColorMapping != NULL) {
      free(gnTgifColorsToObjectColorMapping);
      gnTgifColorsToObjectColorMapping = NULL;
   }
}

/* ----------------------- SetDefaultColorLevels ----------------------- */

#define PDCL_OK 0
#define PDCL_TOO_LARGE 1
#define PDCL_TOO_SMALL 2
#define PDCL_BAD_FORMAT 3

static
int ParseDefaultColorLevels(buf, p_xcolor)
   char *buf;
   XColor *p_xcolor;
{
   char *red_ptr, *green_ptr, *blue_ptr;

   if ((red_ptr=strtok(buf, " ,:\t\n\r")) != NULL &&
         (green_ptr=strtok(NULL, " ,:\t\n\r")) != NULL &&
         (blue_ptr=strtok(NULL, " ,:\t\n\r")) != NULL) {
      int red, green, blue;

      red = atoi(red_ptr);
      green = atoi(green_ptr);
      blue = atoi(blue_ptr);
      if (red+green+blue > 8) {
         return PDCL_TOO_LARGE;
      } else if (red <= 0 || green <= 0 || blue <= 0) {
         return PDCL_TOO_SMALL;
      } else {
         p_xcolor->red = red;
         p_xcolor->green = green;
         p_xcolor->blue = blue;
         return PDCL_OK;
      }
   }
   return PDCL_BAD_FORMAT;
}

void SetDefaultColorLevels()
{
   char *c_ptr, szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   XColor xcolor;

   sprintf(gszMsgBox, "%s (current [R G B] levels are [%1d %1d %1d]):",
         "Please enter number of bits to use for R, G, and B",
         (int)gDefErrorDiffuseLevel.red, (int)gDefErrorDiffuseLevel.green,
         (int)gDefErrorDiffuseLevel.blue);
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') return;

   strcpy(szSpecCopy, szSpec);
   switch (ParseDefaultColorLevels(szSpecCopy, &xcolor)) {
   case PDCL_OK:
      gDefErrorDiffuseLevel.red = xcolor.red;
      gDefErrorDiffuseLevel.green = xcolor.green;
      gDefErrorDiffuseLevel.blue = xcolor.blue;
      sprintf(gszMsgBox, "Levels changed to: [%1d %1d %1d].",
            (int)gDefErrorDiffuseLevel.red, (int)gDefErrorDiffuseLevel.green,
            (int)gDefErrorDiffuseLevel.blue);
      Msg(gszMsgBox);
      break;
   case PDCL_TOO_LARGE:
      sprintf(gszMsgBox,
            "Bad values: '%s'.  %s.\n\nLevels unchanged at [%1d %1d %1d].",
            szSpec, "R+G+B must be <= 8", (int)gDefErrorDiffuseLevel.red,
            (int)gDefErrorDiffuseLevel.green, (int)gDefErrorDiffuseLevel.blue);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      break;
   case PDCL_TOO_SMALL:
      sprintf(gszMsgBox,
            "Bad values: '%s'.  %s.\n\nLevels unchanged at [%1d %1d %1d].",
            szSpec, "Values must be > 0", (int)gDefErrorDiffuseLevel.red,
            (int)gDefErrorDiffuseLevel.green, (int)gDefErrorDiffuseLevel.blue);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      break;
   case PDCL_BAD_FORMAT:
      sprintf(gszMsgBox,
            "Fail to parse '%s' for 3 values.\n\n%s [%1d %1d %1d].",
            szSpec, "Levels unchanged at", (int)gDefErrorDiffuseLevel.red,
            (int)gDefErrorDiffuseLevel.green, (int)gDefErrorDiffuseLevel.blue);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      break;
   }
}

/* ----------------------- ReduceToDefaultColors ----------------------- */

void ReduceToDefaultColors()
{
   char *c_ptr, szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   int i, red, ok=TRUE, rc, ncolors, image_h;
   int red_bits, green_bits, blue_bits, red_levels, green_levels, blue_levels;
   XEvent ev;
   struct XPmRec *xpm_ptr;

   if (!CheckSelectionForImageProc(
         imageProcMenuStr[IMAGEPROC_DEFAULTERRORDIFFUSE])) {
      return;
   }
   xpm_ptr = topSel->obj->detail.xpm;
   image_h = xpm_ptr->image_h;

   gnObjectColorsToTargetColorMapping =
         (int*)malloc(xpm_ptr->ncolors*sizeof(int));
   if (gnObjectColorsToTargetColorMapping == NULL) {
      FailAllocMessage();
      return;
   }
   memset(gnObjectColorsToTargetColorMapping, 0, xpm_ptr->ncolors*sizeof(int));

   red_bits = gDefErrorDiffuseLevel.red;
   green_bits = gDefErrorDiffuseLevel.green;
   blue_bits = gDefErrorDiffuseLevel.blue;
   sprintf(gszMsgBox, "[R G B] levels are [%1d %1d %1d].",
         red_bits, green_bits, blue_bits);
   Msg(gszMsgBox);
   red_levels = (1 << red_bits);
   green_levels = (1 << green_bits);
   blue_levels = (1 << blue_bits);
   ncolors = (1 << (red_bits+green_bits+blue_bits));
   if (ncolors > 0x100) ncolors = 0x100;

   memset(&gXPmTarget, 0, sizeof(struct XPmRec));
   gXPmTarget.ncolors = ncolors;

   gTargetColors = (XColor*)malloc(ncolors*sizeof(XColor));
   if (gTargetColors == NULL) FailAllocMessage();
   i = 0;
   for (red=0; red < red_levels; red++) {
      int red_intensity=GetIntensity(red, red_levels-1), green;

      for (green=0; green < green_levels; green++) {
         int green_intensity=GetIntensity(green, green_levels-1), blue;

         for (blue=0; blue < blue_levels; blue++) {
            int blue_intensity=GetIntensity(blue, blue_levels-1);

            gTargetColors[i].red = red_intensity;
            gTargetColors[i].green = green_intensity;
            gTargetColors[i].blue = blue_intensity;
            i++;
         }
      }
   }
   rc = MapTargetColors(xpm_ptr);

   if (rc) {
      gnUserSpecifiedLevels = ncolors;
      gpConvolveFunc = (void*)ConvolveToReduceToPixmapColors;
      gnConvolving = TRUE;
      DoImageProc(NULL);
      gnConvolving = FALSE;
      gpConvolveFunc = NULL;
      gnUserSpecifiedLevels = (-1);
   }
   free(gTargetColors);
   gTargetColors = NULL;
   memset(&gXPmTarget, 0, sizeof(struct XPmRec));

   if (gnObjectColorsToTargetColorMapping != NULL) {
      free(gnObjectColorsToTargetColorMapping);
      gnObjectColorsToTargetColorMapping = NULL;
   }
   if (gnTgifColorsToObjectColorMapping != NULL) {
      free(gnTgifColorsToObjectColorMapping);
      gnTgifColorsToObjectColorMapping = NULL;
   }
}

/* ----------------------- DefaultErrorDiffuse ----------------------- */

void DefaultErrorDiffuse()
{
   char *c_ptr, szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   int i, red, ok=TRUE, rc, ncolors, image_h;
   int red_bits, green_bits, blue_bits, red_levels, green_levels, blue_levels;
   XEvent ev;
   struct XPmRec *xpm_ptr;

   if (!CheckSelectionForImageProc(
         imageProcMenuStr[IMAGEPROC_DEFAULTERRORDIFFUSE])) {
      return;
   }
   xpm_ptr = topSel->obj->detail.xpm;
   image_h = xpm_ptr->image_h;

   red_bits = gDefErrorDiffuseLevel.red;
   green_bits = gDefErrorDiffuseLevel.green;
   blue_bits = gDefErrorDiffuseLevel.blue;
   sprintf(gszMsgBox, "[R G B] levels are [%1d %1d %1d].",
         red_bits, green_bits, blue_bits);
   Msg(gszMsgBox);
   red_levels = (1 << red_bits);
   green_levels = (1 << green_bits);
   blue_levels = (1 << blue_bits);
   ncolors = (1 << (red_bits+green_bits+blue_bits));
   if (ncolors > 0x100) ncolors = 0x100;

   memset(&gXPmTarget, 0, sizeof(struct XPmRec));
   gXPmTarget.ncolors = ncolors;

   gnFloyd = TRUE;

   gTargetColors = (XColor*)malloc(ncolors*sizeof(XColor));
   if (gTargetColors == NULL) FailAllocMessage();
   i = 0;
   for (red=0; red < red_levels; red++) {
      int red_intensity=GetIntensity(red, red_levels-1), green;

      for (green=0; green < green_levels; green++) {
         int green_intensity=GetIntensity(green, green_levels-1), blue;

         for (blue=0; blue < blue_levels; blue++) {
            int blue_intensity=GetIntensity(blue, blue_levels-1);

            gTargetColors[i].red = red_intensity;
            gTargetColors[i].green = green_intensity;
            gTargetColors[i].blue = blue_intensity;
            i++;
         }
      }
   }
   rc = PrepareForErrorDiffuse(xpm_ptr);
   if (gpnPixelToIndexMap != NULL) {
      free(gpnPixelToIndexMap);
      gpnPixelToIndexMap = NULL;
   }
   if (gpTargetImage != NULL) XDestroyImage(gpTargetImage);
   if (gpTargetBitmapImage != NULL) XDestroyImage(gpTargetBitmapImage);
   gpTargetImage = gpTargetBitmapImage = NULL;

   if (rc) {
      gnUserSpecifiedLevels = ncolors;
      gpConvolveFunc = (void*)ConvolveToErrorDiffuse;
      gnConvolving = TRUE;
      DoImageProc(NULL);
      gnConvolving = FALSE;
      gpConvolveFunc = NULL;
      gnUserSpecifiedLevels = (-1);
   }
   free(gTargetColors);
   gTargetColors = NULL;
   if (gnImageTargetColor != NULL) {
      for (i=0; i < image_h; i++) free(gnImageTargetColor[i]);
      free(gnImageTargetColor);
      gnImageTargetColor = NULL;
   }
   gnFloyd = FALSE;
   memset(&gXPmTarget, 0, sizeof(struct XPmRec));
}

/* ----------------------- Spread ----------------------- */

static int gnAmountToSpread=0;
static int **gnSpreadImageIndex=NULL;

static
int ConvolveToSpread(x, y)
   int x, y;
{
   return GetOrAllocHistogramIndex(&tgifColors[gnSpreadImageIndex[y][x]]);
}

static
void CleanUpSpreadData()
{
   int i, image_h=topSel->obj->detail.xpm->image_h;

   for (i=0; i < image_h; i++) free(gnSpreadImageIndex[i]);
   free(gnSpreadImageIndex);
   gnSpreadImageIndex = NULL;
}

static
int ComputeSpreadData()
{
   struct XPmRec *xpm_ptr=topSel->obj->detail.xpm;
   int i, image_w=xpm_ptr->image_w, image_h=xpm_ptr->image_h, target_percent;
   Pixmap pixmap=xpm_ptr->pixmap;
   XImage *image=XGetImage(mainDisplay, pixmap, 0, 0, image_w, image_h,
         AllPlanes, ZPixmap);

   if (image == NULL) {
      FailAllocMessage();
      return FALSE;
   }
   if (!CreatePixelToIndexMapping()) {
      XDestroyImage(image);
      return FALSE;
   }
   gnSpreadImageIndex = (int**)malloc(image_h*sizeof(int*));
   if (gnSpreadImageIndex == NULL) {
      FailAllocMessage();
      XDestroyImage(image);
      free(gpnPixelToIndexMap);
      gpnPixelToIndexMap = NULL;
      return FALSE;
   }
   gnAmountToSpread++;
   target_percent = 5;
   for (i=0; i < image_h; i++) {
      int j;
      int percent=(i*10000/image_h)/100;

      if (percent >= target_percent) {
         sprintf(gszMsgBox,
               "Calculating new pixel values (pass 1): %1d%%", percent);
         SetStringStatus(gszMsgBox);
         XSync(mainDisplay, False);
         while (target_percent <= percent) {
            target_percent += 5;
         }
      }
      gnSpreadImageIndex[i] = (int*)malloc(image_w*sizeof(int));
      if (gnSpreadImageIndex[i] == NULL) {
         FailAllocMessage();
         for (j=0; j < i; j++) free(gnSpreadImageIndex[j]);
         free(gnSpreadImageIndex);
         gnSpreadImageIndex = NULL;
         XDestroyImage(image);
         free(gpnPixelToIndexMap);
         gpnPixelToIndexMap = NULL;
         return FALSE;
      }
      for (j=0; j < image_w; j++) {
         gnSpreadImageIndex[i][j] = gpnPixelToIndexMap[XGetPixel(image,j,i)];
      }
   }
   target_percent = 5;
   srand(0);
   for (i=0; i < image_h; i++) {
      int j;
      int percent=(i*10000/image_h)/100;

      if (percent >= target_percent) {
         sprintf(gszMsgBox,
               "Calculating new pixel values (pass 2): %1d%%", percent);
         SetStringStatus(gszMsgBox);
         XSync(mainDisplay, False);
         while (target_percent <= percent) {
            target_percent += 5;
         }
      }
      for (j=0; j < image_w; j++) {
         int dx=(rand() % gnAmountToSpread) - (gnAmountToSpread>>1);
         int dy=(rand() % gnAmountToSpread) - (gnAmountToSpread>>1);
         int y=i+dy, x=j+dx, tmp_index;

         if (x >= 0 && x < image_w && y >= 0 && y < image_h) {
            tmp_index = gnSpreadImageIndex[i][j];
            gnSpreadImageIndex[i][j] = gnSpreadImageIndex[y][x];
            gnSpreadImageIndex[y][x] = tmp_index;
         }
      }
   }
   XDestroyImage(image);
   free(gpnPixelToIndexMap);
   gpnPixelToIndexMap = NULL;
   return TRUE;
}

void Spread()
{
   char *c_ptr, szSpec[MAXSTRING+1], szSpecCopy[MAXSTRING+1];
   int j;

   if (!CheckSelectionForImageProc(
         imageProcMenuStr[IMAGEPROC_SPREAD])) {
      return;
   }
   sprintf(gszMsgBox, "Please enter an integer amount to spread:");
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') return;

   strcpy(szSpecCopy, szSpec);
   if ((c_ptr=strtok(szSpec, " ,\t\n\r")) == NULL) return;
   gnAmountToSpread = atoi(c_ptr);
   if (gnAmountToSpread <= 0) {
      sprintf(gszMsgBox, "Invalid amount '%s' specified.", szSpecCopy);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (!ComputeSpreadData()) {
      return;
   }
   gpConvolveFunc = (void*)ConvolveToSpread;
   gnConvolving = TRUE;
   DoImageProc(NULL);
   gnConvolving = FALSE;
   gpConvolveFunc = NULL;

   CleanUpSpreadData();
}

/* ----------------------- Sharpen ----------------------- */

static
int ConvolveToSharpen(x, y)
   int x, y;
   /*
    * [  0 -1  0 ]
    * [ -1  5 -1 ]
    * [  0 -1  0 ]
    */
{
   register long red, green, blue;
   XColor xcolor, *color_ptr;

   if (x == 0 || x == gnImageW-1 || y == 0 || y == gnImageH-1) {
      return GetOrAllocHistogramIndex(&tgifColors[gnOrigImageIndex[y][x]]);
   }
   color_ptr = (&tgifColors[gnOrigImageIndex[y][x]]);
   red = ((long)color_ptr->red) << 3;
   green = ((long)color_ptr->green) << 3;
   blue = ((long)color_ptr->blue) << 3;
   color_ptr = (&tgifColors[gnOrigImageIndex[y][x-1]]);
   red -= ((long)color_ptr->red);
   green -= ((long)color_ptr->green);
   blue -= ((long)color_ptr->blue);
   color_ptr = (&tgifColors[gnOrigImageIndex[y][x+1]]);
   red -= ((long)color_ptr->red);
   green -= ((long)color_ptr->green);
   blue -= ((long)color_ptr->blue);
   color_ptr = (&tgifColors[gnOrigImageIndex[y-1][x]]);
   red -= ((long)color_ptr->red);
   green -= ((long)color_ptr->green);
   blue -= ((long)color_ptr->blue);
   color_ptr = (&tgifColors[gnOrigImageIndex[y+1][x]]);
   red = (red - ((long)color_ptr->red)) >> 2;
   green = (green - ((long)color_ptr->green)) >> 2;
   blue = (blue - ((long)color_ptr->blue)) >> 2;

   memset(&xcolor, 0, sizeof(XColor));
   xcolor.red = (red > 0L ?
         (red > 0x0000ffff ? 0x0ffff : (unsigned int)(red & 0xffff)) : 0);
   xcolor.green = (green > 0L ?
         (green > 0x0000ffff ? 0x0ffff : (unsigned int)(green & 0xffff)) : 0);
   xcolor.blue = (blue > 0L ?
         (blue > 0x0000ffff ? 0x0ffff : (unsigned int)(blue & 0xffff)) : 0);
   return GetOrAllocHistogramIndex(&xcolor);
}

void Sharpen()
{
   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_SHARPEN])) return;
   if (topSel->obj->detail.xpm->image_w < 2 ||
         topSel->obj->detail.xpm->image_h < 2) {
      sprintf(gszMsgBox,
            "Selected object is too thin or flat for sharpening.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   gpConvolveFunc = (void*)ConvolveToSharpen;
   gnConvolving = TRUE;
   DoImageProc(NULL);
   gnConvolving = FALSE;
   gpConvolveFunc = NULL;
}

/* ----------------------- Blur3, Blur5, Blur7 ----------------------- */

static int gnBlurSize=1;

static
int ConvolveToBlur(x, y)
   int x, y;
   /*
    *                 [ 1/25 1/25 1/25 ]
    * [ 1/9 1/9 1/9 ] [ 1/25 1/25 1/25 ]
    * [ 1/9 1/9 1/9 ] [ 1/25 1/25 1/25 ] ...
    * [ 1/9 1/9 1/9 ] [ 1/25 1/25 1/25 ]
    *                 [ 1/25 1/25 1/25 ]
    */
{
   register long red=0L, green=0L, blue=0L;
   XColor xcolor;
   int x_index, array_size=((gnBlurSize<<1)+1);

   if (x < gnBlurSize || x >= gnImageW-gnBlurSize ||
         y < gnBlurSize || y >= gnImageH-gnBlurSize) {
      return GetOrAllocHistogramIndex(&tgifColors[gnOrigImageIndex[y][x]]);
   }
   for (x_index=x-gnBlurSize; x_index <= x+gnBlurSize; x_index++) {
      int y_index;

      for (y_index=y-gnBlurSize; y_index <= y+gnBlurSize; y_index++) {
         register XColor *color_ptr;

         color_ptr = (&tgifColors[gnOrigImageIndex[y_index][x_index]]);
         red += ((long)color_ptr->red);
         green += ((long)color_ptr->green);
         blue += ((long)color_ptr->blue);
      }
   }
   array_size *= array_size;
   red /= array_size;
   green /= array_size;
   blue /= array_size;

   memset(&xcolor, 0, sizeof(XColor));
   xcolor.red = (red > 0L ?
         (red > 0x0000ffff ? 0x0ffff : (unsigned int)(red & 0xffff)) : 0);
   xcolor.green = (green > 0L ?
         (green > 0x0000ffff ? 0x0ffff : (unsigned int)(green & 0xffff)) : 0);
   xcolor.blue = (blue > 0L ?
         (blue > 0x0000ffff ? 0x0ffff : (unsigned int)(blue & 0xffff)) : 0);
   return GetOrAllocHistogramIndex(&xcolor);
}

static
void Blur(nSize)
   int nSize;
{
   int index=IMAGEPROC_BLUR3;

   switch (nSize) {
   case 3: index=IMAGEPROC_BLUR3; break;
   case 5: index=IMAGEPROC_BLUR5; break;
   case 7: index=IMAGEPROC_BLUR7; break;
   }
   gnBlurSize = ((nSize-1)>>1);
   if (!CheckSelectionForImageProc(imageProcMenuStr[index])) return;
   if (topSel->obj->detail.xpm->image_w <= gnBlurSize ||
         topSel->obj->detail.xpm->image_h <= gnBlurSize) {
      sprintf(gszMsgBox, "%s %1d by %1d blurring.",
            "Selected object is too thin or flat for", nSize, nSize);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   gpConvolveFunc = (void*)ConvolveToBlur;
   gnConvolving = TRUE;
   DoImageProc(NULL);
   gnConvolving = FALSE;
   gpConvolveFunc = NULL;
}

void Blur3() { Blur(3); }
void Blur5() { Blur(5); }
void Blur7() { Blur(7); }

/* ----------------------- AlphaCombine ----------------------- */

static struct ObjRec *gpFgObj=NULL, *gpBgObj=NULL, *gpAlphaObj=NULL;
static struct BBRec gTotalCombineBBox;
static struct BBRec gFgCombineBBox, gBgCombineBBox, gAlphaCombineBBox;
static XImage *gpFgImage=NULL, *gpBgImage=NULL, *gpAlphaImage=NULL;
static XImage *gpFgBitmapImage=NULL, *gpBgBitmapImage=NULL,
      *gpAlphaBitmapImage=NULL;

static
int PointInRect(x, y, p_bbox)
   int  x, y;
   struct BBRec *p_bbox;
{
   return (x>=p_bbox->ltx && y>=p_bbox->lty && x<p_bbox->rbx && y<p_bbox->rby);
}

#define INSIDE_NONE  0x0
#define INSIDE_FG    0x1
#define INSIDE_BG    0x2
#define INSIDE_ALPHA 0x4

static
int ConvolveToAlphaCombine(x, y)
   int x, y;
{
   XColor fg_xcolor, bg_xcolor, alpha_xcolor, xcolor;
   int inside_flag=INSIDE_NONE, pixel;
   double alpha, beta;
   long red, green, blue;

   if (PointInRect(x, y, &gAlphaCombineBBox)) inside_flag |= INSIDE_ALPHA;
   if (PointInRect(x, y, &gFgCombineBBox)) inside_flag |= INSIDE_FG;
   if (PointInRect(x, y, &gBgCombineBBox)) inside_flag |= INSIDE_BG;
   switch (inside_flag) {
   case 0x0: return GetOrAllocHistogramIndex(&myBgColor);
   case 0x1:
      /* INSIDE_FG */
      pixel = XGetPixel(gpFgImage, x-gFgCombineBBox.ltx, y-gFgCombineBBox.lty);
      return GetOrAllocHistogramIndex(&tgifColors[gpnPixelToIndexMap[pixel]]);
   case 0x2:
      /* INSIDE_BG */
      pixel = XGetPixel(gpBgImage, x-gBgCombineBBox.ltx, y-gBgCombineBBox.lty);
      return GetOrAllocHistogramIndex(&tgifColors[gpnPixelToIndexMap[pixel]]);
   case 0x3:
      /* INSIDE_BG | INSIDE_FG */
      pixel = XGetPixel(gpFgImage, x-gFgCombineBBox.ltx, y-gFgCombineBBox.lty);
      return GetOrAllocHistogramIndex(&tgifColors[gpnPixelToIndexMap[pixel]]);
   case 0x4:
      /* INSIDE_ALPHA */
      pixel = XGetPixel(gpAlphaImage, x-gAlphaCombineBBox.ltx,
            y-gAlphaCombineBBox.lty);
      return GetOrAllocHistogramIndex(&tgifColors[gpnPixelToIndexMap[pixel]]);
   case 0x5:
      /* INSIDE_ALPHA | INSIDE_FG */
      pixel = XGetPixel(gpFgImage, x-gFgCombineBBox.ltx, y-gFgCombineBBox.lty);
      memcpy(&fg_xcolor, &tgifColors[gpnPixelToIndexMap[pixel]],
            sizeof(XColor));
      pixel = XGetPixel(gpAlphaImage, x-gAlphaCombineBBox.ltx,
            y-gAlphaCombineBBox.lty);
      memcpy(&alpha_xcolor, &tgifColors[gpnPixelToIndexMap[pixel]],
            sizeof(XColor));
      alpha = (0.299*((double)alpha_xcolor.red) +
            0.587*((double)alpha_xcolor.green) +
            0.114*((double)alpha_xcolor.blue)) / 65536.0;
      beta = 1.0 - alpha;
      red = (long)(((double)fg_xcolor.red)*alpha);
      green = (long)(((double)fg_xcolor.green)*alpha);
      blue = (long)(((double)fg_xcolor.blue)*alpha);
      break;
   case 0x6:
      /* INSIDE_ALPHA | INSIDE_BG */
      pixel = XGetPixel(gpBgImage, x-gBgCombineBBox.ltx, y-gBgCombineBBox.lty);
      memcpy(&bg_xcolor, &tgifColors[gpnPixelToIndexMap[pixel]],
            sizeof(XColor));
      pixel = XGetPixel(gpAlphaImage, x-gAlphaCombineBBox.ltx,
            y-gAlphaCombineBBox.lty);
      memcpy(&alpha_xcolor, &tgifColors[gpnPixelToIndexMap[pixel]],
            sizeof(XColor));
      alpha = (0.299*((double)alpha_xcolor.red) +
            0.587*((double)alpha_xcolor.green) +
            0.114*((double)alpha_xcolor.blue)) / 65536.0;
      beta = 1.0 - alpha;
      red = (long)(((double)bg_xcolor.red)*beta);
      green = (long)(((double)bg_xcolor.green)*beta);
      blue = (long)(((double)bg_xcolor.blue)*beta);
      break;
   case 0x7:
      /* INSIDE_ALPHA | INSIDE_BG | INSIDE_FG */
      pixel = XGetPixel(gpFgImage, x-gFgCombineBBox.ltx, y-gFgCombineBBox.lty);
      memcpy(&fg_xcolor, &tgifColors[gpnPixelToIndexMap[pixel]],
            sizeof(XColor));
      pixel = XGetPixel(gpBgImage, x-gBgCombineBBox.ltx, y-gBgCombineBBox.lty);
      memcpy(&bg_xcolor, &tgifColors[gpnPixelToIndexMap[pixel]],
            sizeof(XColor));
      pixel = XGetPixel(gpAlphaImage, x-gAlphaCombineBBox.ltx,
            y-gAlphaCombineBBox.lty);
      memcpy(&alpha_xcolor, &tgifColors[gpnPixelToIndexMap[pixel]],
            sizeof(XColor));
      alpha = ((double)(0.299*((double)alpha_xcolor.red) +
            0.587*((double)alpha_xcolor.green) +
            0.114*((double)alpha_xcolor.blue))) / 65536.0;
      beta = 1.0 - alpha;
      red = (long)(((double)fg_xcolor.red)*alpha +
            ((double)bg_xcolor.red)*beta);
      green = (long)(((double)fg_xcolor.green)*alpha +
            ((double)bg_xcolor.green)*beta);
      blue = (long)(((double)fg_xcolor.blue)*alpha +
            ((double)bg_xcolor.blue)*beta);
      break;
   }
   memset(&xcolor, 0, sizeof(XColor));
   xcolor.red = (red > 0L ?
         (red > 0x0000ffff ? 0x0ffff : (unsigned int)(red & 0xffff)) : 0);
   xcolor.green = (green > 0L ?
         (green > 0x0000ffff ? 0x0ffff : (unsigned int)(green & 0xffff)) : 0);
   xcolor.blue = (blue > 0L ?
         (blue > 0x0000ffff ? 0x0ffff : (unsigned int)(blue & 0xffff)) : 0);
   return GetOrAllocHistogramIndex(&xcolor);
}

static
void OffsetRect(p_bbox, dx, dy)
   struct BBRec *p_bbox;
   int dx, dy;
{
   p_bbox->ltx += dx; p_bbox->lty += dy;
   p_bbox->rbx += dx; p_bbox->rby += dy;
}

static
void UnionRect(p_bbox1, p_bbox2, p_bbox)
   struct BBRec *p_bbox1, *p_bbox2, *p_bbox;
{
   p_bbox->ltx = min(p_bbox1->ltx, p_bbox2->ltx);
   p_bbox->lty = min(p_bbox1->lty, p_bbox2->lty);
   p_bbox->rbx = max(p_bbox1->rbx, p_bbox2->rbx);
   p_bbox->rby = max(p_bbox1->rby, p_bbox2->rby);
}

static
void CleanUpAlphaCombine()
{
   if (gpFgImage != NULL) XDestroyImage(gpFgImage);
   if (gpBgImage != NULL) XDestroyImage(gpBgImage);
   if (gpAlphaImage != NULL) XDestroyImage(gpAlphaImage);
   if (gpFgBitmapImage != NULL) XDestroyImage(gpFgBitmapImage);
   if (gpBgBitmapImage != NULL) XDestroyImage(gpBgBitmapImage);
   if (gpAlphaBitmapImage != NULL) XDestroyImage(gpAlphaBitmapImage);
   gpFgImage = gpBgImage = gpAlphaImage =
         gpFgBitmapImage = gpBgBitmapImage = gpAlphaBitmapImage = NULL;
   gpFgObj = gpBgObj = gpAlphaObj = NULL;
}

static
int PrepareForAlphaCombine()
{
   memcpy(&gFgCombineBBox, &gpFgObj->obbox, sizeof(struct BBRec));
   memcpy(&gBgCombineBBox, &gpBgObj->obbox, sizeof(struct BBRec));
   if (gpAlphaObj != NULL) {
      memcpy(&gAlphaCombineBBox, &gpAlphaObj->obbox, sizeof(struct BBRec));
   }
   UnionRect(&gFgCombineBBox, &gBgCombineBBox, &gTotalCombineBBox);
   if (gpAlphaObj != NULL) {
      UnionRect(&gAlphaCombineBBox, &gTotalCombineBBox, &gTotalCombineBBox);
   }
   gnCombineW = gTotalCombineBBox.rbx - gTotalCombineBBox.ltx;
   gnCombineH = gTotalCombineBBox.rby - gTotalCombineBBox.lty;
   OffsetRect(&gFgCombineBBox, -gTotalCombineBBox.ltx, -gTotalCombineBBox.lty);
   OffsetRect(&gBgCombineBBox, -gTotalCombineBBox.ltx, -gTotalCombineBBox.lty);
   if (gpAlphaObj != NULL) {
      OffsetRect(&gAlphaCombineBBox,
            -gTotalCombineBBox.ltx, -gTotalCombineBBox.lty);
   }
   gpAlphaImage = gpAlphaBitmapImage = NULL;
   if (!GetXPmImages(gpFgObj->detail.xpm, &gpFgImage, &gpFgBitmapImage) ||
         !GetXPmImages(gpBgObj->detail.xpm, &gpBgImage, &gpBgBitmapImage) ||
         (gpAlphaObj != NULL && !GetXPmImages(gpAlphaObj->detail.xpm,
         &gpAlphaImage, &gpAlphaBitmapImage))) {
      return FALSE;
   }
   return TRUE;
}

void AlphaCombine()
{
   char szBuf[MAXSTRING+1];
   int num_objs=0;

   strcpy(szBuf, imageProcMenuStr[IMAGEPROC_ALPHACOMBINE]);
   UtilTrimBlanks(szBuf);
   gpFgObj = gpBgObj = gpAlphaObj = NULL;
   if (curChoice == NOTHING && numObjSelected == 3) {
      struct SelRec *sel_ptr;
      struct ObjRec *obj_ptr;
      int ok=TRUE;

      for (obj_ptr=topObj; ok && obj_ptr != NULL; obj_ptr=obj_ptr->next) {
         for (sel_ptr=topSel; sel_ptr != NULL; sel_ptr=sel_ptr->next) {
            if (obj_ptr == sel_ptr->obj) {
               if (obj_ptr->type == OBJ_XPM) {
                  struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;
                  int image_w=obj_ptr->obbox.rbx-obj_ptr->obbox.ltx;
                  int image_h=obj_ptr->obbox.rby-obj_ptr->obbox.lty;

                  if (obj_ptr->ctm != NULL || xpm_ptr->image_w != image_w ||
                        xpm_ptr->image_h != image_h) {
                     char szBuf1[MAXSTRING+1];

                     strcpy(szBuf1,
                           imageProcMenuStr[IMAGEPROC_REGENERATEIMAGE]);
                     UtilTrimBlanks(szBuf1);
                     sprintf(gszMsgBox, "%s() %s %s.\n\n%s %s() %s.",
                           szBuf, "cannot operate on",
                           "stretched/rotated/sheared pixmap objects",
                           "You can use", szBuf1,
                           "in the ImageProc Menu to regenerate these objects");
                     MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
                     return;
                  }
                  switch (num_objs++) {
                  case 0: gpFgObj = obj_ptr; break;
                  case 1: gpAlphaObj = obj_ptr; break;
                  case 2: gpBgObj = obj_ptr; break;
                  }
               } else {
                  ok = FALSE;
                  break;
               }
            }
         }
      }
      if (!ok) num_objs = 0;
   }
   if (num_objs != 3) {
      sprintf(gszMsgBox, "Please select 3 X Pixmap object for %s().", szBuf);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (PrepareForAlphaCombine()) {
      gnCombining = TRUE;
      gpConvolveFunc = (void*)ConvolveToAlphaCombine;
      gnConvolving = TRUE;
      DoImageProc(NULL);
      gnConvolving = FALSE;
      gpConvolveFunc = NULL;
      gnCombining = FALSE;
   }
   CleanUpAlphaCombine();
}

/* ----------------------- Subtract ----------------------- */

static
int ConvolveToSubtract(x, y)
   int x, y;
{
   XColor fg_xcolor, bg_xcolor, alpha_xcolor, xcolor;
   int inside_flag=INSIDE_NONE, pixel;
   double alpha, beta;
   long red, green, blue;

   if (PointInRect(x, y, &gFgCombineBBox)) inside_flag |= INSIDE_FG;
   if (PointInRect(x, y, &gBgCombineBBox)) inside_flag |= INSIDE_BG;
   switch (inside_flag) {
   case 0x0: return GetOrAllocHistogramIndex(&myBgColor);
   case 0x1:
      /* INSIDE_FG */
      pixel = XGetPixel(gpFgImage, x-gFgCombineBBox.ltx, y-gFgCombineBBox.lty);
      memcpy(&fg_xcolor, &tgifColors[gpnPixelToIndexMap[pixel]],
            sizeof(XColor));
      red = (long)fg_xcolor.red;
      green = (long)fg_xcolor.green;
      blue = (long)fg_xcolor.blue;
      break;
   case 0x2:
      /* INSIDE_BG */
      pixel = XGetPixel(gpBgImage, x-gBgCombineBBox.ltx, y-gBgCombineBBox.lty);
      memcpy(&bg_xcolor, &tgifColors[gpnPixelToIndexMap[pixel]],
            sizeof(XColor));
      red = (long)bg_xcolor.red;
      green = (long)bg_xcolor.green;
      blue = (long)bg_xcolor.blue;
      break;
   case 0x3:
      /* INSIDE_BG | INSIDE_FG */
      pixel = XGetPixel(gpFgImage, x-gFgCombineBBox.ltx, y-gFgCombineBBox.lty);
      memcpy(&fg_xcolor, &tgifColors[gpnPixelToIndexMap[pixel]],
            sizeof(XColor));
      pixel = XGetPixel(gpBgImage, x-gBgCombineBBox.ltx, y-gBgCombineBBox.lty);
      memcpy(&bg_xcolor, &tgifColors[gpnPixelToIndexMap[pixel]],
            sizeof(XColor));
      red = ((long)fg_xcolor.red) - ((long)bg_xcolor.red);
      green = ((long)fg_xcolor.green) - ((long)bg_xcolor.green);
      blue = ((long)fg_xcolor.blue) - ((long)bg_xcolor.blue);
      break;
   }
   memset(&xcolor, 0, sizeof(XColor));
   xcolor.red = (red > 0L ?
         (red > 0x0000ffff ? 0x0ffff : (unsigned int)(red & 0xffff)) : 0);
   xcolor.green = (green > 0L ?
         (green > 0x0000ffff ? 0x0ffff : (unsigned int)(green & 0xffff)) : 0);
   xcolor.blue = (blue > 0L ?
         (blue > 0x0000ffff ? 0x0ffff : (unsigned int)(blue & 0xffff)) : 0);
   return GetOrAllocHistogramIndex(&xcolor);
}

void Subtract()
{
   char szBuf[MAXSTRING+1];
   int num_objs=0;

   strcpy(szBuf, imageProcMenuStr[IMAGEPROC_SUBTRACT]);
   UtilTrimBlanks(szBuf);
   gpFgObj = gpBgObj = gpAlphaObj = NULL;
   if (curChoice == NOTHING && numObjSelected == 2) {
      struct SelRec *sel_ptr;
      struct ObjRec *obj_ptr;
      int ok=TRUE;

      for (obj_ptr=topObj; ok && obj_ptr != NULL; obj_ptr=obj_ptr->next) {
         for (sel_ptr=topSel; sel_ptr != NULL; sel_ptr=sel_ptr->next) {
            if (obj_ptr == sel_ptr->obj) {
               if (obj_ptr->type == OBJ_XPM) {
                  struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;
                  int image_w=obj_ptr->obbox.rbx-obj_ptr->obbox.ltx;
                  int image_h=obj_ptr->obbox.rby-obj_ptr->obbox.lty;

                  if (obj_ptr->ctm != NULL || xpm_ptr->image_w != image_w ||
                        xpm_ptr->image_h != image_h) {
                     char szBuf1[MAXSTRING+1];

                     strcpy(szBuf1,
                           imageProcMenuStr[IMAGEPROC_REGENERATEIMAGE]);
                     UtilTrimBlanks(szBuf1);
                     sprintf(gszMsgBox, "%s() %s %s.\n\n%s %s() %s.",
                           szBuf, "cannot operate on",
                           "stretched/rotated/sheared pixmap objects",
                           "You can use", szBuf1,
                           "in the ImageProc Menu to regenerate these objects");
                     MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
                     return;
                  }
                  switch (num_objs++) {
                  case 0: gpFgObj = obj_ptr; break;
                  case 1: gpBgObj = obj_ptr; break;
                  }
               } else {
                  ok = FALSE;
                  break;
               }
            }
         }
      }
      if (!ok) num_objs = 0;
   }
   if (num_objs != 2) {
      sprintf(gszMsgBox, "Please select 2 X Pixmap object for %s().", szBuf);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (PrepareForAlphaCombine()) {
      gnCombining = TRUE;
      gpConvolveFunc = (void*)ConvolveToSubtract;
      gnConvolving = TRUE;
      DoImageProc(NULL);
      gnConvolving = FALSE;
      gpConvolveFunc = NULL;
      gnCombining = FALSE;
   }
   CleanUpAlphaCombine();
}

/* ----------------------- Non-Image Processing ----------------------- */

/* ----------------------- RunBggen ----------------------- */

static
int GetBggenImageSize(p_image_w, p_image_h)
   int *p_image_w, *p_image_h;
{
   char spec[MAXSTRING+1], *dup_spec=NULL, *part1=NULL, *part2=NULL;
   int ok=TRUE;

   sprintf(gszMsgBox, "Please enter image size in pixels: [Col x Row]");
   if (Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", spec)==INVALID) {
      return FALSE;
   }
   UtilTrimBlanks(spec);
   if (*spec == '\0') return FALSE;
   if ((dup_spec=UtilStrDup(spec)) == NULL) {
      FailAllocMessage();
      return FALSE;
   }
   if ((part1=strtok(dup_spec, " ,xX[]")) != NULL &&
         (part2=strtok(NULL, " ,xX[]")) != NULL) {
      *p_image_w = atoi(part1);
      *p_image_h = atoi(part2);
      if ((*p_image_w) <= 0 || (*p_image_h) <= 0) {
         ok = FALSE;
      }
   } else {
      ok = FALSE;
   }
   if (!ok) {
      sprintf(gszMsgBox, "%s: '%s'.\n\n%s.",
            "Invalid specification", dup_spec, "Please enter [Cols x Rows]");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
   free(dup_spec);
   return ok;
}

static
int BggenGenerateXpm(image_w, image_h, sz_spec, sz_path)
   int image_w, image_h;
   char *sz_spec, *sz_path;
{
   char *psz_cmd, sz_geom[MAXSTRING+1];
   FILE *pFile, *pPipe;
   int bytes_read;

   sprintf(sz_path, "%sTgifXXXXXX", TMP_DIR);
   mktemp(sz_path);
   unlink(sz_path);
   sprintf(sz_geom, "%1dx%1d", image_w, image_h);
   sprintf(gszMsgBox, bggenToXpmCmd, sz_spec, sz_geom);
   if ((psz_cmd=UtilStrDup(gszMsgBox)) == NULL) {
      return FailAllocMessage();
   }
   if ((pFile=fopen(sz_path,"w")) == NULL) {
      char szBuf[MAXSTRING+1];

      strcpy(szBuf, imageProcMenuStr[IMAGEPROC_RUNBGGEN]);
      UtilTrimBlanks(szBuf);
      sprintf(gszMsgBox, "Cannot create temporary file '%s'.\n\n%s() aborted.",
            sz_path, szBuf);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      free(psz_cmd);
      return FALSE;
   }
   Msg("Executing:");
   sprintf(gszMsgBox, "    %s", psz_cmd);
   Msg(gszMsgBox);
   sprintf(gszMsgBox, "Executing '%s'...", psz_cmd);
   SetStringStatus(gszMsgBox);
   XSync(mainDisplay, False);
   if ((pPipe=(FILE*)popen(psz_cmd,"r")) == NULL) {
      char szBuf[MAXSTRING+1];

      strcpy(szBuf, imageProcMenuStr[IMAGEPROC_RUNBGGEN]);
      UtilTrimBlanks(szBuf);
      sprintf(gszMsgBox, "Fail to execute '%s'.\n\n%s() aborted.",
            psz_cmd, szBuf);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      free(psz_cmd);
      fclose(pFile);
      unlink(sz_path);
      return FALSE;
   }
   writeFileFailed = FALSE;
   while ((bytes_read=fread(gszMsgBox, sizeof(char), sizeof(gszMsgBox),
         pPipe)) > 0) {
      if ((int)fwrite(gszMsgBox, sizeof(char), bytes_read, pFile) <= 0) {
         writeFileFailed = TRUE;
         break;
      }
   }
   pclose(pPipe);
   SetStringStatus("...Done");
   free(psz_cmd);
   fclose(pFile);
   if (writeFileFailed) {
      sprintf(gszMsgBox, "Fail to write to '%s'.\n\nFile system may be full.",
            sz_path);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      unlink(sz_path);
      return FALSE;
   }
   return TRUE;
}

void RunBggen()
{
   int image_w=0, image_h=0, w, h, short_name, rc, use_obj_pos=FALSE;
   int ltx=0, lty=0;
   int ncolors=0, chars_per_pixel=0, first_pixel_is_bg=0, *pixels=NULL;
   char szSpec[MAXSTRING+1], szPath[MAXPATHLENGTH+1], *rest;
   char *color_char=NULL, **color_str=NULL, *xpm_data=NULL;
   struct ObjRec *obj_ptr;
   Pixmap pixmap=None, bitmap=None;
   XImage *image=NULL, *bitmap_image=NULL;

   if (curChoice != NOTHING || topSel == NULL) {
      MakeQuiescent();
      if (!GetBggenImageSize(&image_w, &image_h)) {
         return;
      }
   } else if (!CheckSelectionForImageProc(
         imageProcMenuStr[IMAGEPROC_RUNBGGEN])) {
      return;
   } else {
      obj_ptr = topSel->obj;
      ltx = obj_ptr->obbox.ltx;
      lty = obj_ptr->obbox.lty;
      image_w = obj_ptr->obbox.rbx - ltx;
      image_h = obj_ptr->obbox.rby - lty;
      use_obj_pos = TRUE;
      HighLightReverse();
   }
   sprintf(gszMsgBox, "%s (e.g., '%s'):",
         "Please enter command options for 'bggen'", "blue magenta");
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') {
      if (use_obj_pos) HighLightForward();
      return;
   }
   if (!BggenGenerateXpm(image_w, image_h, szSpec, szPath)) {
      if (use_obj_pos) HighLightForward();
      return;
   }
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = MyReadPixmapFile(szPath, &image_w, &image_h, &w, &h, &pixmap,
         &image, &bitmap, &bitmap_image, &ncolors, &chars_per_pixel,
         &first_pixel_is_bg, &color_char, &color_str, &pixels, &xpm_data);
   SetDefaultCursor(mainWindow);
   ShowCursor();

   if ((short_name=IsPrefix(bootDir, szPath, &rest))) ++rest;
   if (rc != BitmapSuccess) {
      char szBuf[MAXSTRING+1];

      strcpy(szBuf, imageProcMenuStr[IMAGEPROC_RUNBGGEN]);
      UtilTrimBlanks(szBuf);
      if (use_obj_pos) HighLightForward();
      sprintf(gszMsgBox, "%s Error: Cannot import generated XPM file '%s'.",
            szBuf, short_name ? rest : szPath);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      unlink(szPath);
      return;
   }
   unlink(szPath);
   obj_ptr = CreateXPmObj(image_w, image_h, w, h, pixmap, image, bitmap,
         bitmap_image, ncolors, chars_per_pixel, first_pixel_is_bg, color_char,
         color_str, pixels, xpm_data);
   AddObj(NULL, topObj, obj_ptr);
   if (use_obj_pos) {
      RemoveAllSel();
      MoveObj(obj_ptr, ltx-obj_ptr->obbox.ltx, lty-obj_ptr->obbox.lty);
      numRedrawBBox = 0;
      obj_ptr->tmp_parent = NULL;
      DrawObj(drawWindow, obj_ptr);
   } else {
      PlaceTopObj(obj_ptr);
   }
   SelectTopObj();
   RecordNewObjCmd();
   SetFileModified(TRUE);
   justDupped = FALSE;
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   sprintf(gszMsgBox, "New XPM object (%1dx%1d) generated.", image_w, image_h);
   Msg(gszMsgBox);
}

/* ----------------------- CircularBggen ----------------------- */

static
int CircularBggenGenerateXpm(image_w, image_h, ncolors, sz_path)
   int image_w, image_h, ncolors;
   char *sz_path;
{
   FILE *pFile;
   int i, cx, cy, target_percent;
   float fval, finc=65535.0/((float)(ncolors-1));
   double max_dist;

   sprintf(sz_path, "%sTgifXXXXXX", TMP_DIR);
   mktemp(sz_path);
   unlink(sz_path);
   gpHistogram = (XColor*)malloc(ncolors*sizeof(XColor));
   gnFinalImageIndex = (int**)malloc(image_h*sizeof(int*));
   if (gpHistogram == NULL || gnFinalImageIndex == NULL) {
      FailAllocMessage();
      if (gpHistogram != NULL) free(gpHistogram);
      if (gnFinalImageIndex != NULL) free(gnFinalImageIndex);
      return FALSE;
   }
   for (i=0, fval=0.0; i < ncolors; i++, fval+=finc) {
      int ival;

      ival = round(fval);
      HISTOGRAMRED(i) = HISTOGRAMGREEN(i) = HISTOGRAMBLUE(i) =
            (unsigned int)ival;
   }
   i--;
   HISTOGRAMRED(i) = HISTOGRAMGREEN(i) = HISTOGRAMBLUE(i) = 65535;

   cx = (image_w>>1);
   cy = (image_h>>1);
   max_dist=(double)sqrt((double)(cx*cx+cy*cy));

   target_percent = 5;
   for (i=0; i < image_h; i++) {
      int j, dy2=(i-cy)*(i-cy);
      int percent=(i*10000/image_h)/100;

      if (percent >= target_percent) {
         sprintf(gszMsgBox, "Computing pixels: %1d%%", percent);
         SetStringStatus(gszMsgBox);
         XSync(mainDisplay, False);
         while (target_percent <= percent) {
            target_percent += 5;
         }
      }
      if ((gnFinalImageIndex[i]=(int*)malloc(image_w*sizeof(int))) == NULL) {
         for (j=0; j < i; j++) free(gnFinalImageIndex[j]);
         free(gnFinalImageIndex);
         free(gpHistogram);
         return FailAllocMessage();
      }
      for (j=0; j < image_w; j++) {
         int dx2=(j-cx)*(j-cx);
         double dist=(double)sqrt((double)(dx2+dy2));
         double gray=((double)ncolors)*dist/max_dist+0.5;
         int index=round(gray);

         if (index < 0) index = 0;
         if (index >= ncolors) index = ncolors-1;
         gnFinalImageIndex[i][j] = ncolors-1-index;
      }
   }
   gnTransparentIndex = (-1);
   if ((pFile=fopen(sz_path,"w")) == NULL) {
      char szBuf[MAXSTRING+1];

      strcpy(szBuf, imageProcMenuStr[IMAGEPROC_CIRCULARBGGEN]);
      UtilTrimBlanks(szBuf);
      sprintf(gszMsgBox, "Cannot create temporary file '%s'.\n\n%s() aborted.",
            sz_path, szBuf);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      for (i=0; i < image_h; i++) free(gnFinalImageIndex[i]);
      free(gnFinalImageIndex);
      free(gpHistogram);
      return FALSE;
   }
   gnImageW = image_w;
   gnImageH = image_h;
   gnHistogramEntries = ncolors;
   writeFileFailed = FALSE;
   DumpConvolution(pFile);
   fclose(pFile);
   for (i=0; i < image_h; i++) free(gnFinalImageIndex[i]);
   free(gnFinalImageIndex);
   free(gpHistogram);
   if (writeFileFailed) {
      sprintf(gszMsgBox, "Fail to write to '%s'.\n\nFile system may be full.",
            sz_path);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      unlink(sz_path);
      return FALSE;
   }
   return TRUE;
}

void CircularBggen()
{
   int image_w=0, image_h=0, w, h, short_name, rc, use_obj_pos=FALSE;
   int ltx=0, lty=0;
   int ncolors=0, chars_per_pixel=0, first_pixel_is_bg=0, *pixels=NULL;
   char szSpec[MAXSTRING+1], szPath[MAXPATHLENGTH+1], *rest;
   char *color_char=NULL, **color_str=NULL, *xpm_data=NULL;
   struct ObjRec *obj_ptr;
   Pixmap pixmap=None, bitmap=None;
   XImage *image=NULL, *bitmap_image=NULL;

   if (curChoice != NOTHING || topSel == NULL) {
      MakeQuiescent();
      if (!GetBggenImageSize(&image_w, &image_h)) {
         return;
      }
   } else if (!CheckSelectionForImageProc(
         imageProcMenuStr[IMAGEPROC_CIRCULARBGGEN])) {
      return;
   } else {
      obj_ptr = topSel->obj;
      ltx = obj_ptr->obbox.ltx;
      lty = obj_ptr->obbox.lty;
      image_w = obj_ptr->obbox.rbx - ltx;
      image_h = obj_ptr->obbox.rby - lty;
      use_obj_pos = TRUE;
      HighLightReverse();
   }
   sprintf(gszMsgBox,
         "Please enter number of gray levels (between 2 and 222):");
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", szSpec);
   UtilTrimBlanks(szSpec);
   if (*szSpec == '\0') {
      if (use_obj_pos) HighLightForward();
      return;
   }
   ncolors = atoi(szSpec);
   if (ncolors < 2 || ncolors > 222) {
      sprintf(gszMsgBox, "Invalid value '%s' entered.", szSpec);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      if (use_obj_pos) HighLightForward();
      return;
   }
   if (!CircularBggenGenerateXpm(image_w, image_h, ncolors, szPath)) {
      if (use_obj_pos) HighLightForward();
      return;
   }
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = MyReadPixmapFile(szPath, &image_w, &image_h, &w, &h, &pixmap,
         &image, &bitmap, &bitmap_image, &ncolors, &chars_per_pixel,
         &first_pixel_is_bg, &color_char, &color_str, &pixels, &xpm_data);
   SetDefaultCursor(mainWindow);
   ShowCursor();

   if ((short_name=IsPrefix(bootDir, szPath, &rest))) ++rest;
   if (rc != BitmapSuccess) {
      char szBuf[MAXSTRING+1];

      strcpy(szBuf, imageProcMenuStr[IMAGEPROC_CIRCULARBGGEN]);
      UtilTrimBlanks(szBuf);
      if (use_obj_pos) HighLightForward();
      sprintf(gszMsgBox, "%s Error: Cannot import generated XPM file '%s'.",
            szBuf, short_name ? rest : szPath);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      unlink(szPath);
      return;
   }
   unlink(szPath);
   obj_ptr = CreateXPmObj(image_w, image_h, w, h, pixmap, image, bitmap,
         bitmap_image, ncolors, chars_per_pixel, first_pixel_is_bg, color_char,
         color_str, pixels, xpm_data);
   AddObj(NULL, topObj, obj_ptr);
   if (use_obj_pos) {
      RemoveAllSel();
      MoveObj(obj_ptr, ltx-obj_ptr->obbox.ltx, lty-obj_ptr->obbox.lty);
      numRedrawBBox = 0;
      obj_ptr->tmp_parent = NULL;
      DrawObj(drawWindow, obj_ptr);
   } else {
      PlaceTopObj(obj_ptr);
   }
   SelectTopObj();
   RecordNewObjCmd();
   SetFileModified(TRUE);
   justDupped = FALSE;
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   sprintf(gszMsgBox, "New XPM object (%1dx%1d) generated.", image_w, image_h);
   Msg(gszMsgBox);
}

/* ----------------------- RegenerateImage ----------------------- */

static
int RegenerateImageFile(pszPath)
   char *pszPath;
{
   int saved_colordump=colorDump, saved_wheretoprint=whereToPrint;

   *gszImageProcXPmFile = '\0';
   gnConvolving = FALSE;

   colorDump = TRUE;
   whereToPrint = XBM_FILE;
   gnInImageProc = TRUE;
   gpImageMapColorFunc = NULL;

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   DumpXBitmapFile();
   SetDefaultCursor(mainWindow);
   ShowCursor();

   gnInImageProc = FALSE;
   whereToPrint = saved_wheretoprint;
   colorDump = saved_colordump;
   if (*gszImageProcXPmFile == '\0') return FALSE;
   strcpy(pszPath, gszImageProcXPmFile);
   return TRUE;
}

void RegenerateImage()
{
   int image_w=0, image_h=0, w, h, short_name, rc;
   int ltx=0, lty=0;
   int ncolors=0, chars_per_pixel=0, first_pixel_is_bg=0, *pixels=NULL;
   char *color_char=NULL, **color_str=NULL, *xpm_data=NULL, *rest;
   char szPath[MAXPATHLENGTH+1];
   struct ObjRec *obj_ptr, *saved_top_obj, *saved_bot_obj;
   struct SelRec *top_sel_ptr=NULL, *bot_sel_ptr=NULL;
   Pixmap pixmap=None, bitmap=None;
   XImage *image=NULL, *bitmap_image=NULL;

   if (!CheckSelectionForImageProc(
         imageProcMenuStr[IMAGEPROC_REGENERATEIMAGE])) {
      return;
   }
   obj_ptr = topSel->obj;
   if (obj_ptr->ctm == NULL) {
      sprintf(gszMsgBox, "%s.\n\n%s?",
            "Selected object is not stretched/rotated/sheared",
            "Would you like to regenerate it anyway");
      if (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB) != MB_ID_YES) {
         return;
      }
   }
   ltx = obj_ptr->obbox.ltx;
   lty = obj_ptr->obbox.lty;

   HighLightReverse();
   PrepareToReplaceAnObj(obj_ptr);
   PushPageInfo();
   saved_top_obj = topObj;
   saved_bot_obj = botObj;

   JustDupSelObj(&top_sel_ptr, &bot_sel_ptr);
   curPage->top = topObj = top_sel_ptr->obj;
   curPage->bot = botObj = bot_sel_ptr->obj;
   CopyObjId(topSel->obj, topObj);
   CopyObjLocks(topSel->obj, topObj);

   rc = RegenerateImageFile(szPath);

   DelAllObj();
   free(top_sel_ptr);
   PopPageInfo();
   curPage->top = topObj = saved_top_obj;
   curPage->bot = botObj = saved_bot_obj;
   RedrawAnArea(botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   if (!rc) {
      HighLightForward();
      AbortPrepareCmd(CMD_REPLACE);
      return;
   }
   UnlinkObj(obj_ptr);
   FreeObj(obj_ptr);
   RemoveAllSel();

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = MyReadPixmapFile(szPath, &image_w, &image_h, &w, &h, &pixmap,
         &image, &bitmap, &bitmap_image, &ncolors, &chars_per_pixel,
         &first_pixel_is_bg, &color_char, &color_str, &pixels, &xpm_data);
   SetDefaultCursor(mainWindow);
   ShowCursor();

   if ((short_name=IsPrefix(bootDir, szPath, &rest))) ++rest;
   if (rc != BitmapSuccess) {
      char szBuf[MAXSTRING+1];

      strcpy(szBuf, imageProcMenuStr[IMAGEPROC_REGENERATEIMAGE]);
      UtilTrimBlanks(szBuf);
      AbortPrepareCmd(CMD_REPLACE);
      sprintf(gszMsgBox, "%s Error: Cannot import generated XPM file '%s'.",
            szBuf, short_name ? rest : szPath);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      unlink(szPath);
      return;
   }
   unlink(szPath);
   obj_ptr = CreateXPmObj(image_w, image_h, w, h, pixmap, image, bitmap,
         bitmap_image, ncolors, chars_per_pixel, first_pixel_is_bg, color_char,
         color_str, pixels, xpm_data);
   AddObj(NULL, topObj, obj_ptr);
   MoveObj(obj_ptr, ltx-obj_ptr->obbox.ltx, lty-obj_ptr->obbox.lty);
   numRedrawBBox = 0;
   obj_ptr->tmp_parent = NULL;
   DrawObj(drawWindow, obj_ptr);
   SelectTopObj();
   RecordReplaceAnObj(topObj);
   SetFileModified(TRUE);
   justDupped = FALSE;
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   sprintf(gszMsgBox, "New XPM object (%1dx%1d) generated.", image_w, image_h);
   Msg(gszMsgBox);
}

/* ----------------------- CropImage ----------------------- */

static
void ContinueCrop(nStartXOff, nStartYOff, pnEndXOff, pnEndYOff)
   int nStartXOff, nStartYOff, *pnEndXOff, *pnEndYOff;
{
   int end_x, end_y, cropping=TRUE;
   char buf[80], w_buf[80], h_buf[80], x_buf[80], y_buf[80];

   end_x = nStartXOff;
   end_y = nStartYOff;
   SelBox(drawWindow, revDefaultGC, nStartXOff, nStartYOff, end_x, end_y);
   PixelToMeasurementUnit(w_buf, 0);
   PixelToMeasurementUnit(h_buf, 0);
   PixelToMeasurementUnit(x_buf, ABS_X(end_x));
   PixelToMeasurementUnit(y_buf, ABS_Y(end_y));
   sprintf(buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
   StartShowMeasureCursor(end_x, end_y, buf, TRUE);
   XGrabPointer(mainDisplay, drawWindow, False,
         PointerMotionMask | ButtonReleaseMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   
   while (cropping) {
      XEvent input;

      XNextEvent(mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.type == ButtonRelease) {
         XUngrabPointer(mainDisplay, CurrentTime);
         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(end_x-nStartXOff)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(end_y-nStartYOff)));
         PixelToMeasurementUnit(x_buf, ABS_X(end_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(end_y));
         sprintf(buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         EndShowMeasureCursor(end_x, end_y, buf, TRUE);
         SelBox(drawWindow, revDefaultGC, nStartXOff, nStartYOff, end_x, end_y);
         cropping = FALSE;
      } else if (input.type == MotionNotify) {
         int new_end_x, new_end_y;
         XMotionEvent *motion_ev;
         XEvent ev;

         motion_ev = &(input.xmotion);
         new_end_x = motion_ev->x;
         new_end_y = motion_ev->y;

         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(end_x-nStartXOff)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(end_y-nStartYOff)));
         PixelToMeasurementUnit(x_buf, ABS_X(end_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(end_y));
         sprintf(buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         ShowMeasureCursor(end_x, end_y, buf, TRUE);
         SelBox(drawWindow, revDefaultGC, nStartXOff, nStartYOff, end_x, end_y);
         end_x = new_end_x; end_y = new_end_y;
         SelBox(drawWindow, revDefaultGC, nStartXOff, nStartYOff, end_x, end_y);
         PixelToMeasurementUnit(w_buf, ABS_SIZE(abs(end_x-nStartXOff)));
         PixelToMeasurementUnit(h_buf, ABS_SIZE(abs(end_y-nStartYOff)));
         PixelToMeasurementUnit(x_buf, ABS_X(end_x));
         PixelToMeasurementUnit(y_buf, ABS_Y(end_y));
         sprintf(buf, "%sx%s%s%s", w_buf, h_buf, x_buf, y_buf);
         ShowMeasureCursor(end_x, end_y, buf, TRUE);

         MarkRulers(end_x, end_y);
         while (XCheckMaskEvent(mainDisplay, PointerMotionMask, &ev)) ;
      }
   }
   *pnEndXOff = end_x;
   *pnEndYOff = end_y;
}

static
int GetCropArea(pbbox)
   struct BBRec *pbbox;
{
   unsigned int button;
   int mouse_x=0, mouse_y=0, rc=TRUE;

   SaveStatusStrings();
   Msg("Please drag out a rectangular area to crop...");
   SetMouseStatus("Start crop image", "Abort crop image", "Abort crop image");
   button = DrawWindowLoop(&mouse_x, &mouse_y, cornerCursor, FALSE);
   if (button == Button1) {
      int end_x=mouse_x, end_y=mouse_y;

      ContinueCrop(mouse_x, mouse_y, &end_x, &end_y);
      if (pbbox != NULL) {
         CalcBBox(mouse_x, mouse_y, end_x, end_y,
               &pbbox->ltx, &pbbox->lty, &pbbox->rbx, &pbbox->rby);
      }
   } else {
      rc = FALSE;
   }
   Msg("");
   RestoreStatusStrings();
   return rc;
}

void CropImage()
{
   int image_w=0, image_h=0, w, h, short_name, rc;
   int ltx=0, lty=0, crop_x=0, crop_y=0, crop_w=0, crop_h=0;
   int ncolors=0, chars_per_pixel=0, first_pixel_is_bg=0, *pixels=NULL;
   int saved_ltx=selLtX, saved_lty=selLtY, saved_rbx=selRbX, saved_rby=selRbY;
   char *color_char=NULL, **color_str=NULL, *xpm_data=NULL, *rest;
   char szPath[MAXPATHLENGTH+1];
   struct ObjRec *obj_ptr, *saved_top_obj, *saved_bot_obj;
   struct SelRec *top_sel_ptr=NULL, *bot_sel_ptr=NULL;
   Pixmap pixmap=None, bitmap=None;
   XImage *image=NULL, *bitmap_image=NULL;
   struct AttrRec *saved_fattr=NULL, *saved_lattr=NULL;
   struct BBRec crop_bbox;

   if (!CheckSelectionForImageProc(
         imageProcMenuStr[IMAGEPROC_CROPIMAGE])) {
      return;
   }
   obj_ptr = topSel->obj;
   ltx = obj_ptr->obbox.ltx;
   lty = obj_ptr->obbox.lty;

   HighLightReverse();
   XSync(mainDisplay, False);
   if (!GetCropArea(&crop_bbox)) {
      HighLightForward();
      return;
   }
   XSync(mainDisplay, False);
   crop_bbox.ltx = ABS_X(crop_bbox.ltx); crop_bbox.lty = ABS_Y(crop_bbox.lty);
   crop_bbox.rbx = ABS_X(crop_bbox.rbx); crop_bbox.rby = ABS_Y(crop_bbox.rby);
   if (!BBoxIntersect(crop_bbox, obj_ptr->obbox)) {
      HighLightForward();
      sprintf(gszMsgBox, "Selected area does not intersect selected image.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   } else {
      crop_bbox.ltx = max(crop_bbox.ltx, obj_ptr->obbox.ltx);
      crop_bbox.lty = max(crop_bbox.lty, obj_ptr->obbox.lty);
      crop_bbox.rbx = min(crop_bbox.rbx, obj_ptr->obbox.rbx);
      crop_bbox.rby = min(crop_bbox.rby, obj_ptr->obbox.rby);
      crop_x = crop_bbox.ltx-obj_ptr->obbox.ltx;
      crop_y = crop_bbox.lty-obj_ptr->obbox.lty;
      crop_w = crop_bbox.rbx-crop_bbox.ltx;
      crop_h = crop_bbox.rby-crop_bbox.lty;
   }
   if (obj_ptr->ctm == NULL) {
      HighLightForward();
      CutXPixmap(&crop_x, &crop_y, &crop_w, &crop_h);
      return;
   }
   PrepareToReplaceAnObj(obj_ptr);
   PushPageInfo();
   saved_top_obj = topObj;
   saved_bot_obj = botObj;

   JustDupSelObj(&top_sel_ptr, &bot_sel_ptr);
   curPage->top = topObj = top_sel_ptr->obj;
   curPage->bot = botObj = bot_sel_ptr->obj;
   CopyObjId(topSel->obj, topObj);
   CopyObjLocks(topSel->obj, topObj);

   rc = RegenerateImageFile(szPath);

   DelAllObj();
   free(top_sel_ptr);
   PopPageInfo();
   curPage->top = topObj = saved_top_obj;
   curPage->bot = botObj = saved_bot_obj;
   RedrawAnArea(botObj, saved_ltx-GRID_ABS_SIZE(1),
         saved_lty-GRID_ABS_SIZE(1), saved_rbx+GRID_ABS_SIZE(1),
         saved_rby+GRID_ABS_SIZE(1));
   if (!rc) {
      HighLightForward();
      AbortPrepareCmd(CMD_REPLACE);
      return;
   }
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = MyReadPixmapFile(szPath, &image_w, &image_h, &w, &h, &pixmap,
         &image, &bitmap, &bitmap_image, &ncolors, &chars_per_pixel,
         &first_pixel_is_bg, &color_char, &color_str, &pixels, &xpm_data);
   SetDefaultCursor(mainWindow);
   ShowCursor();
   if (rc == BitmapSuccess) {
      Pixmap dest_pixmap=None, dest_bitmap=None;
      XImage *dest_image=NULL, *dest_bitmap_image=NULL;
      int ok;

      ok = ExtractPixmap(pixmap, image, bitmap,
            bitmap_image, crop_x, crop_y, crop_w, crop_h,
            &dest_pixmap, &dest_image, &dest_bitmap, &dest_bitmap_image);
      if (ok) {
         XFreePixmap(mainDisplay, pixmap);
         XFreePixmap(mainDisplay, bitmap);
         XDestroyImage(image);
         XDestroyImage(bitmap_image);
         pixmap = dest_pixmap;
         bitmap = dest_bitmap;
         image = dest_image;
         bitmap_image = dest_bitmap_image;
      } else {
         rc = BitmapFileInvalid;
      }
   }
   if (rc == BitmapSuccess) {
      saved_fattr = obj_ptr->fattr;
      saved_lattr = obj_ptr->lattr;
      obj_ptr->fattr = obj_ptr->lattr = NULL;
      UnlinkObj(obj_ptr);
      FreeObj(obj_ptr);
      RemoveAllSel();
   }
   if ((short_name=IsPrefix(bootDir, szPath, &rest))) ++rest;
   if (rc != BitmapSuccess) {
      char szBuf[MAXSTRING+1];

      strcpy(szBuf, imageProcMenuStr[IMAGEPROC_CROPIMAGE]);
      UtilTrimBlanks(szBuf);
      AbortPrepareCmd(CMD_REPLACE);
      sprintf(gszMsgBox, "%s Error: Cannot import generated XPM file '%s'.",
            szBuf, short_name ? rest : szPath);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      unlink(szPath);
      return;
   }
   unlink(szPath);
   obj_ptr = CreateXPmObj(crop_w, crop_h, crop_w, crop_h, pixmap, image,
         bitmap, bitmap_image, ncolors, chars_per_pixel, first_pixel_is_bg,
         color_char, color_str, pixels, xpm_data);

   AddObj(NULL, topObj, obj_ptr);
   MoveObj(obj_ptr, ltx-obj_ptr->obbox.ltx+crop_x,
         lty-obj_ptr->obbox.lty+crop_y);
   if (saved_fattr != NULL) {
      obj_ptr->fattr = saved_fattr;
      obj_ptr->lattr = saved_lattr;
   }
   RecordReplaceAnObj(topObj);
   RedrawAnArea(botObj, saved_ltx-GRID_ABS_SIZE(1), saved_lty-GRID_ABS_SIZE(1),
            saved_rbx+GRID_ABS_SIZE(1), saved_rby+GRID_ABS_SIZE(1));
   SelectTopObj();
   SetFileModified(TRUE);
   justDupped = FALSE;
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   sprintf(gszMsgBox, "New XPM object (%1dx%1d) generated.", image_w, image_h);
   Msg(gszMsgBox);
}

/* ----------------------- GetColor ----------------------- */

static
void SetCurrentColor(image, bitmap_image, image_x, image_y)
   XImage *image, *bitmap_image;
   int image_x, image_y;
{
   int pixel=(-1);

   if (bitmap_image == NULL) {
      pixel = XGetPixel(image, image_x, image_y);
   } else {
      if (XGetPixel(bitmap_image, image_x, image_y) == 0) {
         /* transparent */
      } else {
         pixel = XGetPixel(image, image_x, image_y);
      }
   }
   if (pixel == (-1)) {
   } else {
      int i;

      for (i=0; i < maxColors; i++) {
         if (colorPixels[i] == pixel) {
            struct SelRec *saved_top_sel=topSel, *saved_bot_sel=botSel;

            topSel = botSel = NULL;
            ChangeAllSelColor(i, FALSE);
            topSel = saved_top_sel;
            botSel = saved_bot_sel;
            break;
         }
      }
   }
}

static
void DoGetColor(obj_ptr)
   struct ObjRec *obj_ptr;
{
   int image_w, image_h, done=FALSE;
   struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;
   XImage *image, *bitmap_image;

   image_w = xpm_ptr->image_w;
   image_h = xpm_ptr->image_h;
   image = xpm_ptr->image;
   bitmap_image = xpm_ptr->bitmap_image;
   if (image == NULL) {
      image = xpm_ptr->image = XGetImage(mainDisplay, xpm_ptr->pixmap, 0, 0,
            image_w, image_h, AllPlanes, ZPixmap);
      if (image == NULL) FailAllocMessage();
   }
   if (xpm_ptr->bitmap != None && bitmap_image == NULL) {
      bitmap_image = xpm_ptr->bitmap_image = XGetImage(mainDisplay,
            xpm_ptr->bitmap, 0, 0, image_w, image_h, AllPlanes, ZPixmap);
      if (bitmap_image == NULL) FailAllocMessage();
   }
   SaveStatusStrings();
   Msg("Please select a color to be used as the current color...");
   SetMouseStatus("Select a color", "Finish", "Finish");
   while (!done) {
      int mouse_x=0, mouse_y=0;
      unsigned int button=PickAPoint(&mouse_x, &mouse_y, dripCursor);

      if (button == Button1) {
         int abs_x=ABS_X(mouse_x), abs_y=ABS_Y(mouse_y), found=FALSE;
         int image_x=0, image_y=0;

         if (obj_ptr->ctm == NULL) {
            if (abs_x >= obj_ptr->obbox.ltx && abs_y >= obj_ptr->obbox.lty &&
                  abs_x < obj_ptr->obbox.rbx && abs_y < obj_ptr->obbox.rby) {
               image_x = abs_x-obj_ptr->obbox.ltx;
               image_y = abs_y-obj_ptr->obbox.lty;
               found = TRUE;
            } else {
               sprintf(gszMsgBox,
                     "Selected point is not on the selected image.");
               SetStringStatus(gszMsgBox);
            }
         } else {
            struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;
            int tmp_x, tmp_y;

            ReverseTransformPointThroughCTM(abs_x-obj_ptr->x, abs_y-obj_ptr->y,
                  obj_ptr->ctm, &tmp_x, &tmp_y);
            tmp_x += obj_ptr->x-obj_ptr->orig_obbox.ltx;
            tmp_y += obj_ptr->y-obj_ptr->orig_obbox.lty;
            if (tmp_x >= 0 && tmp_y >= 0 &&
                  tmp_x < xpm_ptr->image_w && tmp_y < xpm_ptr->image_h) {
               image_x = tmp_x;
               image_y = tmp_y;
               found = TRUE;
            } else {
               sprintf(gszMsgBox,
                     "Selected point is not on the selected image.");
               SetStringStatus(gszMsgBox);
            }
         }
         if (found) {
            SetCurrentColor(image, bitmap_image, image_x, image_y);
         }
      } else {
         done = TRUE;
      }
   }
   RestoreStatusStrings();
}

void GetColor()
{
   int i, pixel;
   char szBuf[MAXSTRING+1];
   struct ObjRec *obj_ptr;

   strcpy(szBuf, imageProcMenuStr[IMAGEPROC_GETCOLOR]);
   UtilTrimBlanks(szBuf);
   if (curChoice != NOTHING || topSel == NULL || topSel != botSel) {
      sprintf(gszMsgBox, "Please select only one primitive object for %s().",
            szBuf);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   obj_ptr = topSel->obj;
   switch (obj_ptr->type) {
   case OBJ_GROUP:
   case OBJ_ICON:
   case OBJ_SYM:
      sprintf(gszMsgBox, "Please select only one primitive object for %s().",
            szBuf);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      break;
   case OBJ_XPM:
      HighLightReverse();
      XSync(mainDisplay, False);
      DoGetColor(obj_ptr);
      HighLightForward();
      break;
   default:
      pixel = colorPixels[obj_ptr->color];
      for (i=0; i < maxColors; i++) {
         if (colorPixels[i] == pixel) {
            struct SelRec *saved_top_sel=topSel, *saved_bot_sel=botSel;

            topSel = botSel = NULL;
            ChangeAllSelColor(i, FALSE);
            topSel = saved_top_sel;
            botSel = saved_bot_sel;
            break;
         }
      }
      break;
   }
}

/* ----------------------- ReplaceColor ----------------------- */

static XComposeStatus c_stat;

static
unsigned int FillReplacePickAPoint(OrigX, OrigY, cursor)
   int *OrigX, *OrigY;
   Cursor cursor;
{
   XEvent input;

   XGrabPointer(mainDisplay, drawWindow, False, ButtonPressMask,
         GrabModeAsync, GrabModeAsync, None, cursor, CurrentTime);

   for (;;) {
      XNextEvent(mainDisplay, &input);

      if (WindowIsSubMenu(input.xany.window, MENU_COLOR)) {
         HandleSubMenuEvent(input.xany.window, MENU_COLOR, &input);
      } else if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.type == ButtonPress) {
         XUngrabPointer(mainDisplay, CurrentTime);
         XSync(mainDisplay, False);
         *OrigX = input.xbutton.x;
         *OrigY = input.xbutton.y;
         return input.xbutton.button;
      } else if (input.type == KeyPress) {
         XKeyEvent *key_ev=(&(input.xkey));
         KeySym key_sym;
         char s[80];
         int has_ch=XLookupString(key_ev, s, sizeof(s)-1, &key_sym, &c_stat);

         TranslateKeys(s, &key_sym);
         if (key_sym== XK_Escape || (has_ch && s[0]=='\033')) {
            XUngrabPointer(mainDisplay, CurrentTime);
            XSync(mainDisplay, False);
            return (unsigned int)(-1);
         }
      }
   }
}

static
void DoReplaceAColor(obj_ptr, image, bitmap_image, image_x, image_y,
      image_w, image_h)
   struct ObjRec *obj_ptr;
   XImage *image, *bitmap_image;
   int image_x, image_y, image_w, image_h;
{
   struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;
   int pixel=(-1);

   if (bitmap_image == NULL) {
      pixel = XGetPixel(image, image_x, image_y);
   } else {
      if (XGetPixel(bitmap_image, image_x, image_y) == 0) {
         /* transparent */
      } else {
         pixel = XGetPixel(image, image_x, image_y);
      }
   }
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   if (pixel == (-1)) {
   } else {
      int i, r, *pixels=xpm_ptr->pixels, ncolors=xpm_ptr->ncolors;
      char **color_str=xpm_ptr->color_str;

      for (r=0; r < image_h; r++) {
         int c;

         for (c=0; c < image_w; c++) {
            if (XGetPixel(image, c, r) == pixel) {
               XPutPixel(image, c, r, colorPixels[colorIndex]);
            }
         }
      }
      for (i=0; i < ncolors; i++) {
         if (pixels[i] == pixel) {
            pixels[i] = colorPixels[colorIndex];
            if (color_str[i] != NULL) free(color_str[i]);
            color_str[i] = UtilStrDup(colorMenuItems[colorIndex]);
            if (color_str[i] == NULL) FailAllocMessage();
         }
      }
      if (xpm_ptr->data != NULL) {
         fprintf(stderr,
               "In ReplaceAColor(), unexpected xpm_ptr->data != NULL.\n");
      }
   }
   SetDefaultCursor(mainWindow);
   ShowCursor();

   XPutImage(mainDisplay, xpm_ptr->pixmap, xpmGC, image, 0, 0, 0, 0,
      image_w, image_h);
   if (bitmap_image != NULL) {
      XPutImage(mainDisplay, xpm_ptr->bitmap, xbmGC, bitmap_image, 0, 0, 0, 0,
         image_w, image_h);
   }
   if (xpm_ptr->cached_pixmap != None) {
      XFreePixmap(mainDisplay, xpm_ptr->cached_pixmap);
      xpm_ptr->cached_pixmap = None;
   }
   if (xpm_ptr->cached_bitmap != None) {
      XFreePixmap(mainDisplay, xpm_ptr->cached_bitmap);
      xpm_ptr->cached_bitmap = None;
   }
   AdjObjCache(obj_ptr);
   RedrawAnArea(botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);
   justDupped = FALSE;
}

static
int ContinueReplaceColor(obj_ptr)
   struct ObjRec *obj_ptr;
{
   int done=FALSE, image_w, image_h, changed=FALSE;
   struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;
   XImage *image, *bitmap_image;

   xpm_ptr = obj_ptr->detail.xpm;
   image_w = xpm_ptr->image_w;
   image_h = xpm_ptr->image_h;
   image = xpm_ptr->image;
   bitmap_image = xpm_ptr->bitmap_image;
   if (image == NULL) {
      image = xpm_ptr->image = XGetImage(mainDisplay, xpm_ptr->pixmap, 0, 0,
            image_w, image_h, AllPlanes, ZPixmap);
      if (image == NULL) FailAllocMessage();
   }
   if (xpm_ptr->bitmap != None && bitmap_image == NULL) {
      bitmap_image = xpm_ptr->bitmap_image = XGetImage(mainDisplay,
            xpm_ptr->bitmap, 0, 0, image_w, image_h, AllPlanes, ZPixmap);
      if (bitmap_image == NULL) FailAllocMessage();
   }
   SaveStatusStrings();
   Msg("Please select a color to be replaced by the current color...");
   SetMouseStatus("Select a color to replace", "Finish", "Finish");
   while (!done) {
      int mouse_x=0, mouse_y=0;
      unsigned int button=FillReplacePickAPoint(&mouse_x, &mouse_y,
            floodCursor);

      if (button == Button1) {
         int abs_x=ABS_X(mouse_x), abs_y=ABS_Y(mouse_y), found=FALSE;
         int image_x=0, image_y=0;

         if (obj_ptr->ctm == NULL) {
            if (abs_x >= obj_ptr->obbox.ltx && abs_y >= obj_ptr->obbox.lty &&
                  abs_x < obj_ptr->obbox.rbx && abs_y < obj_ptr->obbox.rby) {
               image_x = abs_x-obj_ptr->obbox.ltx;
               image_y = abs_y-obj_ptr->obbox.lty;
               if (image_x >= 0 && image_y >= 0 &&
                     image_x < image_w && image_y < image_h) {
                  found = TRUE;
               }
            }
         } else {
            ReverseTransformPointThroughCTM(abs_x-obj_ptr->x, abs_y-obj_ptr->y,
                  obj_ptr->ctm, &image_x, &image_y);
            image_x += obj_ptr->x-obj_ptr->orig_obbox.ltx;
            image_y += obj_ptr->y-obj_ptr->orig_obbox.lty;
            if (image_x >= 0 && image_y >= 0 &&
                  image_x < image_w && xpm_ptr->image_h) {
               found = TRUE;
            }
         }
         if (found) {
            changed = TRUE;
            DoReplaceAColor(obj_ptr, image, bitmap_image, image_x, image_y,
                  image_w, image_h);
         } else {
            sprintf(gszMsgBox, "Selected point is not on the selected image.");
            SetStringStatus(gszMsgBox);
         }
      } else {
         done = TRUE;
      }
   }
   RestoreStatusStrings();
   return changed;
}

void ReplaceColor()
{
   struct ObjRec *obj_ptr;

   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_REPLACECOLOR])) {
      return;
   }
   obj_ptr = topSel->obj;

   HighLightReverse();
   XSync(mainDisplay, False);
   PrepareToReplaceAnObj(obj_ptr);
   if (!ContinueReplaceColor(obj_ptr)) {
      AbortPrepareCmd(CMD_REPLACE);
   } else {
      RecordReplaceAnObj(obj_ptr);
   }
   HighLightForward();
}

/* ----------------------- FloodFill ----------------------- */

#define DIR_NONE 0
#define DIR_UP 1
#define DIR_RIGHT 2
#define DIR_DOWN 3
#define DIR_LEFT 4

/*
 *      1
 *      ^
 *      |
 * 4 <- 0 -> 2
 *      |
 *      v
 *      3
 */

static int gnPixelToFill=(-1);

static
void DoFloodFill(c, r, pixel, image, image_w, image_h, dir)
   int c, r, pixel, image_w, image_h, dir;
   XImage *image;
{
   if (XGetPixel(image, c, r) != pixel) {
      return;
   }
   XPutPixel(image, c, r, gnPixelToFill);
   if (dir != DIR_DOWN && r-1 >= 0) {
      DoFloodFill(c, r-1, pixel, image, image_w, image_h, DIR_UP);
   }
   if (dir != DIR_LEFT && c+1 < image_w) {
      DoFloodFill(c+1, r, pixel, image, image_w, image_h, DIR_RIGHT);
   }
   if (dir != DIR_UP && r+1 < image_h) {
      DoFloodFill(c, r+1, pixel, image, image_w, image_h, DIR_DOWN);
   }
   if (dir != DIR_RIGHT && c-1 >= 0) {
      DoFloodFill(c-1, r, pixel, image, image_w, image_h, DIR_LEFT);
   }
}

static
void StartFloodFill(obj_ptr, image, bitmap_image, image_x, image_y,
      image_w, image_h, do_flood_fill)
   struct ObjRec *obj_ptr;
   XImage *image, *bitmap_image;
   int image_x, image_y, image_w, image_h, do_flood_fill;
{
   struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;
   int pixel=(-1);

   if (bitmap_image == NULL) {
      pixel = XGetPixel(image, image_x, image_y);
   } else {
      if (XGetPixel(bitmap_image, image_x, image_y) == 0) {
         /* transparent */
      } else {
         pixel = XGetPixel(image, image_x, image_y);
      }
   }
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   gnPixelToFill = colorPixels[colorIndex];
   if (pixel == (-1)) {
   } else if (gnPixelToFill != pixel) {
      int i, *pixels=xpm_ptr->pixels, ncolors=xpm_ptr->ncolors, found=FALSE;

      if (do_flood_fill) {
         DoFloodFill(image_x, image_y, pixel, image, image_w, image_h, 0);
      } else {
         XPutPixel(image, image_x, image_y, gnPixelToFill);
      }
      for (i=0; i < ncolors; i++) {
         if (pixels[i] == gnPixelToFill) {
            found = TRUE;
         }
      }
      if (!found) {
         int chars_per_pixel=xpm_ptr->chars_per_pixel;

         xpm_ptr->pixels = (int*)realloc(xpm_ptr->pixels,
               sizeof(int)*(ncolors+1));
         if (xpm_ptr->pixels == NULL) FailAllocMessage();
         xpm_ptr->pixels[ncolors] = colorPixels[colorIndex];

         xpm_ptr->color_str = (char**)realloc(xpm_ptr->color_str,
               sizeof(char*)*(ncolors+1));
         if (xpm_ptr->color_str == NULL) FailAllocMessage();
         xpm_ptr->color_str[ncolors] = UtilStrDup(colorMenuItems[colorIndex]);
         if (xpm_ptr->color_str[ncolors] == NULL) FailAllocMessage();

         if (chars_per_pixel == 1 && ncolors >= 20) {
            /* needs to go from 1 chars_per_pixel to 2 chars_per_pixel */
            char *color_char=(char*)malloc(((ncolors+1)<<1)*sizeof(char));

            if (color_char == NULL) FailAllocMessage();
            for (i=0; i < ncolors+1; i++) {
               if (i == 0 && xpm_ptr->color_char[0] == '`') {
                  color_char[i<<1] = color_char[(i<<1)+1] = '`';
               } else {
                  color_char[i<<1] = (char)(((int)('a'))+(int)(i/10));
                  color_char[(i<<1)+1] = (char)(((int)('0'))+(int)(i%10));
               }
            }
            free(xpm_ptr->color_char);
            xpm_ptr->color_char = color_char;
            xpm_ptr->chars_per_pixel = 2;
         } else {
            char *color_char;

            xpm_ptr->color_char = color_char =
                  (char*)realloc(xpm_ptr->color_char,
                  sizeof(char)*chars_per_pixel*(ncolors+1));
            if (color_char == NULL) FailAllocMessage();
            if (chars_per_pixel == 1) {
               for (i=0; i < ncolors+1; i++) {
                  if (i == 0 && xpm_ptr->color_char[0] == '`') {
                     color_char[i] = '`';
                  } else {
                     color_char[i] = (char)(((int)('a'))+i-1);
                  }
               }
            } else {
               for (i=0; i < ncolors+1; i++) {
                  if (i == 0 && xpm_ptr->color_char[0] == '`' &&
                        xpm_ptr->color_char[1] == '`') {
                     color_char[i<<1] = color_char[(i<<1)+1] = '`';
                  } else {
                     color_char[i<<1] = (char)(((int)('a'))+(int)(i/10));
                     color_char[(i<<1)+1] = (char)(((int)('0'))+(int)(i%10));
                  }
               }
            }
         }
         xpm_ptr->ncolors++;
      }
      if (xpm_ptr->data != NULL) {
         fprintf(stderr,
               "In FloodFill(), unexpected xpm_ptr->data != NULL.\n");
      }
   }
   SetDefaultCursor(mainWindow);
   ShowCursor();

   XPutImage(mainDisplay, xpm_ptr->pixmap, xpmGC, image, 0, 0, 0, 0,
      image_w, image_h);
   if (bitmap_image != NULL) {
      XPutImage(mainDisplay, xpm_ptr->bitmap, xbmGC, bitmap_image, 0, 0, 0, 0,
         image_w, image_h);
   }
   if (xpm_ptr->cached_pixmap != None) {
      XFreePixmap(mainDisplay, xpm_ptr->cached_pixmap);
      xpm_ptr->cached_pixmap = None;
   }
   if (xpm_ptr->cached_bitmap != None) {
      XFreePixmap(mainDisplay, xpm_ptr->cached_bitmap);
      xpm_ptr->cached_bitmap = None;
   }
   AdjObjCache(obj_ptr);
   RedrawAnArea(botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);
   justDupped = FALSE;
}

static
int ContinueFloodFill(obj_ptr)
   struct ObjRec *obj_ptr;
{
   int done=FALSE, image_w, image_h, changed=FALSE;
   struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;
   XImage *image, *bitmap_image;

   xpm_ptr = obj_ptr->detail.xpm;
   image_w = xpm_ptr->image_w;
   image_h = xpm_ptr->image_h;
   image = xpm_ptr->image;
   bitmap_image = xpm_ptr->bitmap_image;
   if (image == NULL) {
      image = xpm_ptr->image = XGetImage(mainDisplay, xpm_ptr->pixmap, 0, 0,
            image_w, image_h, AllPlanes, ZPixmap);
      if (image == NULL) FailAllocMessage();
   }
   if (xpm_ptr->bitmap != None && bitmap_image == NULL) {
      bitmap_image = xpm_ptr->bitmap_image = XGetImage(mainDisplay,
            xpm_ptr->bitmap, 0, 0, image_w, image_h, AllPlanes, ZPixmap);
      if (bitmap_image == NULL) FailAllocMessage();
   }
   SaveStatusStrings();
   TwoLineMsg("<Btn1>: flood-fill, <Btn3>: set a pixel to current color.",
         "    <ESC> to finish.");
   SetMouseStatus("Flood-fill", "Finish", "Set a pixel");
   while (!done) {
      int mouse_x=0, mouse_y=0;
      unsigned int button=FillReplacePickAPoint(&mouse_x, &mouse_y,
            floodCursor);

      if (button == Button1 || button == Button3) {
         int abs_x=ABS_X(mouse_x), abs_y=ABS_Y(mouse_y), found=FALSE;
         int image_x=0, image_y=0;

         if (obj_ptr->ctm == NULL) {
            if (abs_x >= obj_ptr->obbox.ltx && abs_y >= obj_ptr->obbox.lty &&
                  abs_x < obj_ptr->obbox.rbx && abs_y < obj_ptr->obbox.rby) {
               image_x = abs_x-obj_ptr->obbox.ltx;
               image_y = abs_y-obj_ptr->obbox.lty;
               if (image_x >= 0 && image_y >= 0 &&
                     image_x < image_w && image_y < image_h) {
                  found = TRUE;
               }
            }
         } else {
            ReverseTransformPointThroughCTM(abs_x-obj_ptr->x, abs_y-obj_ptr->y,
                  obj_ptr->ctm, &image_x, &image_y);
            image_x += obj_ptr->x-obj_ptr->orig_obbox.ltx;
            image_y += obj_ptr->y-obj_ptr->orig_obbox.lty;
            if (image_x >= 0 && image_y >= 0 &&
                  image_x < image_w && image_y < image_h) {
               found = TRUE;
            }
         }
         if (found) {
            changed = TRUE;
            StartFloodFill(obj_ptr, image, bitmap_image, image_x, image_y,
                  image_w, image_h, button==Button1);
         } else {
            sprintf(gszMsgBox, "Selected point is not on the selected image.");
            SetStringStatus(gszMsgBox);
         }
      } else {
         done = TRUE;
      }
   }
   RestoreStatusStrings();
   return changed;
}

void FloodFill()
{
   struct ObjRec *obj_ptr;

   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_FLOODFILL])) {
      return;
   }
   obj_ptr = topSel->obj;

   HighLightReverse();
   XSync(mainDisplay, False);
   PrepareToReplaceAnObj(obj_ptr);
   if (ContinueFloodFill(obj_ptr)) {
      RecordReplaceAnObj(obj_ptr);
   } else {
      AbortPrepareCmd(CMD_REPLACE);
   }
   HighLightForward();
}

/* ----------------------- CreateContour ----------------------- */

/*------------------------------------------------*/
/* the contour code is adapted from Tim Feldman's */
/*        contour code in Graphics Gem III        */
/*------------------------------------------------*/

typedef struct CDirInfo {
   /* 5 6 7 */
   /* 4   0 */
   /* 3 2 1 */
   short dir;
   struct CDirInfo *next;
} CDirInfoPtr;

static struct CDirInfo *topOfChain=NULL, *botOfChain=NULL;

static int gnContourX=0, gnContourY=0;
static int gnContourW=0, gnContourH=0;
static unsigned short gnContourRed=0, gnContourGreen=0, gnContourBlue=0;
static int **gnaContourPixels=NULL;
static XImage *gContourImage=NULL, *gContourBitmapImage=NULL;
static struct ObjRec *gpContourObj=NULL;

static
void CleanUpContour()
{
   if (gnaContourPixels != NULL) {
      int i;

      for (i=0; i < gnContourH; i++) {
         if (gnaContourPixels[i] != NULL) {
            free(gnaContourPixels[i]);
         } else {
            break;
         }
      }
      free(gnaContourPixels);
   }
   gnaContourPixels = NULL;

   for ( ; topOfChain != NULL; topOfChain=botOfChain) {
      botOfChain = topOfChain->next;
      free(topOfChain);
   }
   topOfChain = botOfChain = NULL;
}

static
int OnContour(x, y)
   int x, y;
{
   if (x >= 0 && x < gnContourW && y >= 0 && y < gnContourH) {
      int index;

      if (gnaContourPixels[y][x] == BAD) {
         gnaContourPixels[y][x] = XGetPixel(gContourImage, x, y);
      }
      index = gpnPixelToIndexMap[gnaContourPixels[y][x]];
      if (tgifColors[index].red == gnContourRed &&
            tgifColors[index].green == gnContourGreen &&
            tgifColors[index].blue == gnContourBlue) {
         return TRUE;
      }
   }
   return FALSE;
}

static
int ProbeContour(x, y, dir, pn_new_x, pn_new_y)
   int x, y, dir, *pn_new_x, *pn_new_y;
{
   switch (dir) {
   case 0: x++;      break;
   case 1: x++; y++; break;
   case 2:      y++; break;
   case 3: x--; y++; break;
   case 4: x--;      break;
   case 5: x--; y--; break;
   case 6:      y--; break;
   case 7: x++; y--; break;
   }
   *pn_new_x = x;
   *pn_new_y = y;
   return OnContour(x, y);
}

static
int ContourNeighbor(x, y, last_dir, pn_new_x, pn_new_y)
   int x, y, last_dir, *pn_new_x, *pn_new_y;
   /*
    * if last vector was 0, start looking at 1
    * if last vector was 1, start looking at 3
    * if last vector was 2, start looking at 3
    * if last vector was 3, start looking at 5
    * if last vector was 4, start looking at 5
    * if last vector was 5, start looking at 7
    * if last vector was 6, start looking at 7
    * if last vector was 7, start looking at 1
    */
{
   int i;

   if (last_dir & 0x1) {
      last_dir += 2;
   } else {
      last_dir++;
   }
   if (last_dir > 7) last_dir -= 8;
   for (i=0; i < 8; i++) {
      if (ProbeContour(x, y, last_dir, pn_new_x, pn_new_y)) {
         return last_dir;
      } else {
         if (--last_dir < 0) last_dir += 8;
      }
   }
   fprintf(stderr, "Should not have come here ContourNeighbor()!\n");
}

static
int CreatePolyFromContour(num_pts)
   int num_pts;
{
   int x=gnContourX, y=gnContourY, generate=(num_pts > 2);
   struct CDirInfo *pcdi=topOfChain;
   struct XfrmMtrxRec *ctm=NULL;

   if (generate) {
      ResetCreatePolygon();
      ctm = gpContourObj->ctm;
   } else {
      num_pts = 0;
   }
   while (pcdi != NULL) {
      int dir=pcdi->dir, n=1;
      struct CDirInfo *pcdi1=pcdi->next;

      for ( ; pcdi1 != NULL; pcdi1=pcdi1->next) {
         if (pcdi1->dir != dir) {
            break;
         } else {
            n++;
         }
      }
      if (generate) {
         if (ctm == NULL) {
            AddPtToCreatePolygon(gpContourObj->x+x, gpContourObj->y+y);
         } else {
            int tmp_x, tmp_y;

            TransformPointThroughCTM(x, y, ctm, &tmp_x, &tmp_y);
            AddPtToCreatePolygon(gpContourObj->x+tmp_x, gpContourObj->y+tmp_y);
         }
      } else {
         num_pts++;
      }
      switch (dir) {
      case 0: x += n;         break;
      case 1: x += n; y += n; break;
      case 2:         y += n; break;
      case 3: x -= n; y += n; break;
      case 4: x -= n;         break;
      case 5: x -= n; y -= n; break;
      case 6:         y -= n; break;
      case 7: x += n; y -= n; break;
      }
      pcdi = pcdi1;
   }
   if (generate) {
      if (ctm == NULL) {
         AddPtToCreatePolygon(gpContourObj->x+x, gpContourObj->y+y);
      } else {
         int tmp_x, tmp_y;

         TransformPointThroughCTM(x, y, ctm, &tmp_x, &tmp_y);
         AddPtToCreatePolygon(gpContourObj->x+tmp_x, gpContourObj->y+tmp_y);
      }
      CreatePolygonObj(num_pts, TRUE);
   } else {
      num_pts++;
   }
   if (generate) {
      return TRUE;
   }
   return num_pts;
}

static
int DoCreateContour()
{
   int x, y, dir, new_x, new_y, last_dir, num_pts=0;

   while (OnContour(gnContourX, gnContourY)) {
      gnContourX--;
   }
   gnContourX++;
   topOfChain = NULL;

   x = new_x = gnContourX;
   y = new_y = gnContourY;
   dir = 0;
   for (;;) {
      if (ProbeContour(x, y, dir, &new_x, &new_y)) {
         break;
      } else if (++dir >= 8) {
         sprintf(gszMsgBox, "No contour can be generated from here!");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return FALSE;
      }
   }
   last_dir = 1;
   for (;;) {
      struct CDirInfo *pcdi;

      dir = ContourNeighbor(x, y, last_dir, &new_x, &new_y);
      pcdi = (struct CDirInfo *)malloc(sizeof(struct CDirInfo));
      if (pcdi == NULL) FailAllocMessage();
      memset(pcdi, 0, sizeof(struct CDirInfo));
      pcdi->dir = dir;
      pcdi->next = NULL;
      if (botOfChain == NULL) {
         topOfChain = pcdi;
      } else {
         botOfChain->next = pcdi;
      }
      botOfChain = pcdi;
      if (new_x == gnContourX && new_y == gnContourY) {
         break;
      }
      x = new_x;
      y = new_y;
      last_dir = dir;
   }
   num_pts = CreatePolyFromContour(0);
   if (num_pts > 2) {
      return CreatePolyFromContour(num_pts);
   }
   sprintf(gszMsgBox, "No contour can be generated from here!");
   MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   return FALSE;
}

static
int StartCreateContour(obj_ptr, image, bitmap_image, image_x, image_y,
      image_w, image_h)
   struct ObjRec *obj_ptr;
   XImage *image, *bitmap_image;
   int image_x, image_y, image_w, image_h;
{
   int i, pixel=(-1), created=FALSE;

   gnContourX = image_x;
   gnContourY = image_y;
   gnContourW = image_w;
   gnContourH = image_h;
   gContourImage = image;
   gContourBitmapImage = bitmap_image;
   gpContourObj = obj_ptr;

   gnaContourPixels = (int**)malloc(image_h*sizeof(int*));
   if (gnaContourPixels == NULL) {
      return FailAllocMessage();
   }
   memset(gnaContourPixels, 0, image_h*sizeof(int*));
   for (i=0; i < image_h; i++) {
      int j;

      gnaContourPixels[i] = (int*)malloc(image_w*sizeof(int));
      if (gnaContourPixels[i] == NULL) {
         FailAllocMessage();
         CleanUpContour();
         return FALSE;
      }
      for (j=0; j < image_w; j++) gnaContourPixels[i][j] = BAD;
   }
   if (!CreatePixelToIndexMapping()) {
      CleanUpContour();
      return FALSE;
   }
   if (bitmap_image == NULL) {
      pixel = XGetPixel(image, image_x, image_y);
   } else {
      if (XGetPixel(bitmap_image, image_x, image_y) == 0) {
         /* transparent */
      } else {
         pixel = XGetPixel(image, image_x, image_y);
      }
   }
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   gnPixelToFill = colorPixels[colorIndex];
   if (pixel == (-1)) {
   } else {
      int index=gpnPixelToIndexMap[pixel];

      gnContourRed = tgifColors[index].red;
      gnContourGreen = tgifColors[index].green;
      gnContourBlue = tgifColors[index].blue;
      created = DoCreateContour(image_x, image_y, pixel, image,
            image_w, image_h, 0);
   }
   SetDefaultCursor(mainWindow);
   ShowCursor();

   free(gpnPixelToIndexMap);
   gpnPixelToIndexMap = NULL;
   CleanUpContour();
   return created;
}

static
int ContinueCreateContour(obj_ptr)
   struct ObjRec *obj_ptr;
{
   int image_w, image_h, changed=FALSE, mouse_x=0, mouse_y=0;
   unsigned int button;
   struct XPmRec *xpm_ptr=obj_ptr->detail.xpm;
   XImage *image, *bitmap_image;

   xpm_ptr = obj_ptr->detail.xpm;
   image_w = xpm_ptr->image_w;
   image_h = xpm_ptr->image_h;
   image = xpm_ptr->image;
   bitmap_image = xpm_ptr->bitmap_image;
   if (image == NULL) {
      image = xpm_ptr->image = XGetImage(mainDisplay, xpm_ptr->pixmap, 0, 0,
            image_w, image_h, AllPlanes, ZPixmap);
      if (image == NULL) FailAllocMessage();
   }
   if (xpm_ptr->bitmap != None && bitmap_image == NULL) {
      bitmap_image = xpm_ptr->bitmap_image = XGetImage(mainDisplay,
            xpm_ptr->bitmap, 0, 0, image_w, image_h, AllPlanes, ZPixmap);
      if (bitmap_image == NULL) FailAllocMessage();
   }
   SaveStatusStrings();
   Msg("Please select a color be traced...");
   SetMouseStatus("Start-contour", "Finish", "Finish");

   button = FillReplacePickAPoint(&mouse_x, &mouse_y, handCursor);

   if (button == Button1) {
      int abs_x=ABS_X(mouse_x), abs_y=ABS_Y(mouse_y), found=FALSE;
      int image_x=0, image_y=0;

      if (obj_ptr->ctm == NULL) {
         if (abs_x >= obj_ptr->obbox.ltx && abs_y >= obj_ptr->obbox.lty &&
               abs_x < obj_ptr->obbox.rbx && abs_y < obj_ptr->obbox.rby) {
            image_x = abs_x-obj_ptr->obbox.ltx;
            image_y = abs_y-obj_ptr->obbox.lty;
            if (image_x >= 0 && image_y >= 0 &&
                  image_x < image_w && image_y < image_h) {
               found = TRUE;
            }
         }
      } else {
         ReverseTransformPointThroughCTM(abs_x-obj_ptr->x, abs_y-obj_ptr->y,
               obj_ptr->ctm, &image_x, &image_y);
         image_x += obj_ptr->x-obj_ptr->orig_obbox.ltx;
         image_y += obj_ptr->y-obj_ptr->orig_obbox.lty;
         if (image_x >= 0 && image_y >= 0 &&
               image_x < image_w && image_y < image_h) {
            found = TRUE;
         }
      }
      if (found) {
         changed = StartCreateContour(obj_ptr, image, bitmap_image, image_x,
               image_y, image_w, image_h);
      } else {
         sprintf(gszMsgBox, "Selected point is not on the selected image.");
         SetStringStatus(gszMsgBox);
      }
   }
   RestoreStatusStrings();
   return changed;
}

void CreateContour()
{
   struct ObjRec *obj_ptr;

   if (!CheckSelectionForImageProc(imageProcMenuStr[IMAGEPROC_CREATECONTOUR])) {
      return;
   }
   obj_ptr = topSel->obj;

   HighLightReverse();
   XSync(mainDisplay, False);
   if (ContinueCreateContour(obj_ptr)) {
      RemoveAllSel();
      numRedrawBBox = 0;
      obj_ptr->tmp_parent = NULL;
      DrawObj(drawWindow, topObj);
      SelectTopObj();
      RecordNewObjCmd();
      SetFileModified(TRUE);
      justDupped = FALSE;
   } else {
      HighLightForward();
   }
}

/* ----------------------- Init and Clean Up ----------------------- */

void CleanUpImageProc()
{
   CleanUpConvolution();
}

static char gszDefBggen[]="bggen %s -g %s | ppmquant 64 | ppmtoxpm";

void InitImageProc()
{
   char *c_ptr;

   gnQuantizingLevels = 222;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"QuantizingLevels")) != NULL) {
      gnQuantizingLevels = atoi(c_ptr);
      if (gnQuantizingLevels < 2 || gnQuantizingLevels > 256) {
         fprintf(stderr, "Invalid %s*%s: '%s', %s, %1d is used.\n",
               TOOL_NAME, "QuantizingLevels", c_ptr,
               "(must be between 2 and 256)", 256-maxColors);
         gnQuantizingLevels = 256-maxColors;
      }
   }
   strcpy(bggenToXpmCmd, gszDefBggen);
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"BggenToXpm")) != NULL) {
      int count=0;

      strcpy(bggenToXpmCmd, c_ptr);
      for (c_ptr=strstr(bggenToXpmCmd,"%s"); c_ptr!=NULL;
            c_ptr=strstr(++c_ptr,"%s")) {
         count++;
      }
      if (count != 2) {
         sprintf(gszMsgBox, "Invalid %s*%s: %s resource.  '%s' used.",
               TOOL_NAME, "BggenToXpm", bggenToXpmCmd, gszDefBggen);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         strcpy(bggenToXpmCmd, gszDefBggen);
      }
   }
   gDefErrorDiffuseLevel.red = gDefErrorDiffuseLevel.green =
         gDefErrorDiffuseLevel.blue = 2;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"DefaultErrorDiffuseLevels")) !=
         NULL) {
      XColor xcolor;
      char *dup_buf=UtilStrDup(c_ptr);

      if (dup_buf == NULL) FailAllocMessage();
      switch (ParseDefaultColorLevels(dup_buf, &xcolor)) {
      case PDCL_OK:
         gDefErrorDiffuseLevel.red = xcolor.red;
         gDefErrorDiffuseLevel.green = xcolor.green;
         gDefErrorDiffuseLevel.blue = xcolor.blue;
         break;
      case PDCL_TOO_LARGE:
         fprintf(stderr, "Values too large in %s*%s: '%s', '%s' is used.\n",
               TOOL_NAME, "DefaultErrorDiffuseLevels", c_ptr, "2 3 2");
         break;
      case PDCL_TOO_SMALL:
         fprintf(stderr, "Values too small in %s*%s: '%s', '%s' is used.\n",
               TOOL_NAME, "DefaultErrorDiffuseLevels", c_ptr, "2 3 2");
         break;
      case PDCL_BAD_FORMAT:
         fprintf(stderr, "Invalid %s*%s: '%s', '%s' is used.\n",
               TOOL_NAME, "DefaultErrorDiffuseLevels", c_ptr, "2 3 2");
         break;
      }
      free(dup_buf);
   }
   memset(gaHGBucket, 0, sizeof(gaHGBucket));
}

/* ----------------------- Menu Functions ----------------------- */

void ImageProcSubMenu(nIndex)
   int nIndex;
{
   switch (nIndex) {
   case IMAGEPROC_MAKEGRAY: MakeGray(); break;
   case IMAGEPROC_INVERTCOLOR: InvertColor(); break;
   case IMAGEPROC_INTERPOLATECOLOR: InterpolateColor(); break;
   case IMAGEPROC_BRIGHTDARKEN: BrightenDarken(); break;
   case IMAGEPROC_CHANGESATURATION: ChangeSaturation(); break;
   case IMAGEPROC_CHANGEHUE: ChangeHue(); break;
   case IMAGEPROC_CONTRAST: ContrastEnhance(); break;
   case IMAGEPROC_COLORBALANCE: ColorBalance(); break;
   case IMAGEPROC_GAMMA: Gamma(); break;
   case IMAGEPROC_EDGEDETECT: EdgeDetect(); break;
   case IMAGEPROC_EMBOSS: Emboss(); break;
   case IMAGEPROC_REDUCECOLORS: ReduceColors(); break;
   case IMAGEPROC_REDUCETOPIXMAPCOLORS: ReduceToPixmapColors(); break;
   case IMAGEPROC_SETDEFAULTCOLORLEVELS: SetDefaultColorLevels(); break;
   case IMAGEPROC_REDUCETODEFAULTCOLORS: ReduceToDefaultColors(); break;
   case IMAGEPROC_DEFAULTERRORDIFFUSE: DefaultErrorDiffuse(); break;
   case IMAGEPROC_SPREAD: Spread(); break;
   case IMAGEPROC_SHARPEN: Sharpen(); break;
   case IMAGEPROC_BLUR3: Blur3(); break;
   case IMAGEPROC_BLUR5: Blur5(); break;
   case IMAGEPROC_BLUR7: Blur7(); break;
   case IMAGEPROC_RUNBGGEN: RunBggen(); break;
   case IMAGEPROC_CIRCULARBGGEN: CircularBggen(); break;
   case IMAGEPROC_REGENERATEIMAGE: RegenerateImage(); break;
   case IMAGEPROC_CROPIMAGE: CropImage(); break;
   case IMAGEPROC_GETCOLOR: GetColor(); break;
   case IMAGEPROC_REPLACECOLOR: ReplaceColor(); break;
   case IMAGEPROC_FLOODFILL: FloodFill(); break;
   case IMAGEPROC_CREATECONTOUR: CreateContour(); break;
   case IMAGEPROC_SUBTRACT: Subtract(); break;
   case IMAGEPROC_ALPHACOMBINE: AlphaCombine(); break;
   }
}

int ImageProcMenu(X, Y, TrackMenubar)
   int  X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   DefaultColorArrays(MAXIMAGEPROCS, &fore_colors, &valid, &init_rv, NULL);

   activeMenu = MENU_IMAGEPROC;
   index = TextMenuLoop(X, Y, imageProcMenuStr, MAXIMAGEPROCS, fore_colors,
         valid, init_rv, imageProcMenuDesc, SINGLECOLOR, TrackMenubar);

   if (index >= 0) ImageProcSubMenu(index);
   return index;
}

